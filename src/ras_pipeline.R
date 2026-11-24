# ==============================================================================
# RAS / NRAS mutation analysis pipeline - Acral (AM) vs Cutaneous (CM) melanoma
# Usage (from the project root): Rscript src/ras_pipeline.R [input] [output_dir]
# ==============================================================================

# ---- Packages ----------------------------------------------------------------
suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(metafor)
  library(patchwork)
})

# ==============================================================================
# 1. CONFIGURATION
# ==============================================================================
# ---- Paths -------------------------------------------------------------------
.args <- commandArgs(trailingOnly = TRUE)

config <- list(
  input_file = if (length(.args) >= 1) .args[[1]] else
    Sys.getenv("RAS_INPUT", "data/ras_acral_cutaneous_cohorts_review.tsv"),
  output_dir = if (length(.args) >= 2) .args[[2]] else
    Sys.getenv("RAS_OUTPUT", "results"),

# ---- Cohorts -----------------------------------------------------------------
  cohort_renaming = c("mel_tsam_liang_2017" = "Liang et al., 2017"),
  subtypes        = c("Acral", "Cutaneous"),

# ---- Mutation hierarchy ------------------------------------------------------
  ras_genes    = c("NRAS", "KRAS", "HRAS"),
  nras_codons  = c("G12", "G13", "Q61"),
  q61_variants = c("Q61R", "Q61K", "Q61L", "Q61H", "Q61P", "Q61 dual hit"),
  q61_pattern  = "Q61R|Q61K|Q61L|Q61H|Q61P",

# ---- Figure options ----------------------------------------------------------
  bar_width  = 8.5,
  bar_height = 5.0,
  forest_hidden_groups  = c("NRAS Q61P", "NRAS Q61 dual hit"),
  or_hidden_matched     = "NRAS G13-mutant",

# ---- Output file names (PDF = <name>.pdf, table = Table_<name>.csv) ----------
  figure_names = list(
    heterogeneity = "Figure_1",
    composition   = "Figure_2",
    overall_ras   = c(matched = "Figure_3a", all = "Additional_Figure_3a"),
    genes         = c(matched = "Figure_3b", all = "Additional_Figure_3b"),
    codons        = c(matched = "Figure_3c", all = "Additional_Figure_3c"),
    q61           = c(matched = "Figure_3d", all = "Additional_Figure_3d"),
    or_forest     = c(matched = "Figure_4",  all = "Additional_Figure_4")
  )
)

# ---- Palettes ----------------------------------------------------------------
pal <- list(
  subtype    = c("Acral" = "#a4243b", "Cutaneous" = "#003049"),
  ras_status = c("RAS-mutant" = "#370617", "RAS wild-type" = "#A4AC86"),
  gene       = c("NRAS" = "#CA6702", "KRAS" = "#0A9396", "HRAS" = "#9B2226"),
  codon      = c("Q61" = "#7F4F24", "G12" = "#656D4A", "G13" = "#BB3E03"),
  q61        = c("Q61R" = "#D00000", "Q61K" = "#414833", "Q61L" = "#F48C06",
                 "Q61H" = "#001219", "Q61P" = "#A68A64", "Q61 dual hit" = "#582F0E"),
  composition = c("NRAS" = "#cdedf6", "KRAS" = "#f9c74f", "HRAS" = "#511410")
)
pal$forest <- c(
  pal$ras_status["RAS-mutant"],
  setNames(pal$gene,  paste0(names(pal$gene), "-mutant")),
  setNames(pal$codon, paste0("NRAS ", names(pal$codon), "-mutant")),
  setNames(pal$q61,   paste0("NRAS ", names(pal$q61)))
)

stopifnot(!anyDuplicated(toupper(unlist(pal[c("subtype", "ras_status", "gene", "codon", "q61")]))))

# ==============================================================================
# 2. UTILITIES
# ==============================================================================
# ---- Input -------------------------------------------------------------------
read_csv_robust <- function(path) {
  if (!file.exists(path)) stop("File not found: ", path)
  header_line <- readLines(path, n = 1, warn = FALSE, encoding = "UTF-8")
  candidate_seps <- c(";", ",", "\t", "|")
  n_fields <- vapply(candidate_seps, function(s)
    length(scan(text = header_line, what = "", sep = s, quiet = TRUE, quote = "\"")), integer(1))
  if (max(n_fields) < 2) stop("Could not detect a field separator.\nHeader line: ", header_line)
  sep <- candidate_seps[which.max(n_fields)]
  message("    Detected separator: '", sep, "' (", max(n_fields), " columns)")
  df <- read.csv(path, sep = sep, stringsAsFactors = FALSE, check.names = FALSE, encoding = "UTF-8")
  names(df) <- str_trim(str_remove(names(df), "^(\ufeff|\u00ef\u00bb\u00bf)+"))
  df
}

detect_schema <- function(df) {
  patient_cols   <- c("Study_cohort", "Melanoma_Type", "Genetic_Classification", "RAS_specific")
  aggregate_cols <- c("Study_cohort", "Melanoma_Type", "Total_Group", "RAS_NRAS", "RAS_KRAS", "RAS_HRAS")
  if (all(patient_cols   %in% names(df))) return("patient")
  if (all(aggregate_cols %in% names(df))) return("aggregate")
  stop("The input table does not match either known schema.")
}

# ---- Output ------------------------------------------------------------------
out <- function(file_name) file.path(config$output_dir, file_name)

save_pdf <- function(p, file_name, width, height) {
  pdf(out(file_name), width = width, height = height)
  on.exit(dev.off())
  print(p)
  invisible(p)
}

skip_log <- new.env()
skip_log$rows <- list()

report_skip <- function(file_name, reason) {
  status <- "Figure NOT generated"
  message(sprintf("    [%s] %s: %s", status, file_name, reason))
  skip_log$rows[[length(skip_log$rows) + 1]] <- data.frame(
    Status = status, File = file_name, Reason = reason, stringsAsFactors = FALSE)
  invisible(NULL)
}

# ---- Statistics --------------------------------------------------------------
clopper_pearson_pct <- function(k, n) {
  ci <- vapply(seq_along(k), function(i) {
    if (is.na(n[i]) || n[i] == 0) return(c(0, 0))
    binom.test(k[i], n[i], conf.level = 0.95)$conf.int * 100
  }, numeric(2))
  list(low = ci[1, ], high = ci[2, ])
}

fmt_p <- function(p, eq = TRUE) {
  ifelse(is.na(p), "N/A", ifelse(p < 0.001, "< 0.001", sprintf(if (eq) "= %.3f" else "%.3f", p)))
}

# ==============================================================================
# 3. DATA
# ==============================================================================
# ---- Import, cleaning and per-cohort counts ----------------------------------
load_data <- function(cfg) {
  raw    <- read_csv_robust(cfg$input_file)
  schema <- detect_schema(raw)

  raw <- raw %>%
    mutate(Melanoma_Type = str_to_title(str_trim(Melanoma_Type)),
           Study_cohort  = recode(str_trim(Study_cohort), !!!as.list(cfg$cohort_renaming)))

  base <- raw %>% filter(Melanoma_Type %in% cfg$subtypes)

  if (schema == "patient") {
    base <- base %>%
      mutate(Genetic_Classification = str_trim(Genetic_Classification),
             RAS_specific = str_to_upper(str_trim(RAS_specific)),
             NRAS   = if ("NRAS" %in% names(.)) str_trim(NRAS) else NA_character_,
             is_RAS = !is.na(Genetic_Classification) & Genetic_Classification == "RAS")

    clean <- base %>%
      group_by(Study_cohort, Melanoma_Type) %>%
      summarise(Total_Group = n(),
                RAS_NRAS = sum(is_RAS & RAS_specific == "NRAS", na.rm = TRUE),
                RAS_KRAS = sum(is_RAS & RAS_specific == "KRAS", na.rm = TRUE),
                RAS_HRAS = sum(is_RAS & RAS_specific == "HRAS", na.rm = TRUE),
                .groups = "drop")
  } else {
    clean <- base %>%
      mutate(across(all_of(c("Total_Group", "RAS_NRAS", "RAS_KRAS", "RAS_HRAS")),
                    ~ suppressWarnings(as.numeric(str_trim(as.character(.))))),
             across(c(RAS_NRAS, RAS_KRAS, RAS_HRAS), ~ replace_na(., 0)))
  }
  clean <- clean %>%
    mutate(Total_RAS = RAS_NRAS + RAS_KRAS + RAS_HRAS,
           Pct_RAS   = (Total_RAS / Total_Group) * 100)

  paired <- clean %>%
    distinct(Study_cohort, Melanoma_Type) %>%
    group_by(Study_cohort) %>%
    filter(n_distinct(Melanoma_Type) == 2) %>%
    pull(Study_cohort) %>% unique()
  message(sprintf("    Paired cohorts (AM + CM) detected: %d -> %s", length(paired), paste(paired, collapse = "; ")))

  ci <- clopper_pearson_pct(clean$Total_RAS, clean$Total_Group)
  per_study <- clean %>% mutate(CI_low = ci$low, CI_high = ci$high)

  list(schema = schema, base = base, clean = clean, paired = paired, per_study = per_study)
}

# ==============================================================================
# 4. FIGURE 1 - BETWEEN-COHORT HETEROGENEITY
# ==============================================================================
# ---- Figure 1: between-cohort heterogeneity (binomial-normal GLMM) -----------
logit_summary <- function(b, se, tau2, k) {
  z <- qnorm(0.975)
  res <- list(pooled_pct  = plogis(b) * 100,
              ci_low_pct  = plogis(b - z * se) * 100,
              ci_high_pct = plogis(b + z * se) * 100,
              pi_low_pct  = NA_real_, pi_high_pct = NA_real_)
  if (k >= 3) {
    tcrit <- qt(0.975, df = k - 2)
    res$pi_low_pct  <- plogis(b - tcrit * sqrt(tau2 + se^2)) * 100
    res$pi_high_pct <- plogis(b + tcrit * sqrt(tau2 + se^2)) * 100
  }
  res
}

fit_heterogeneity <- function(subtype, study_data) {
  dat <- study_data %>% filter(Melanoma_Type == subtype) %>% mutate(xi = Total_RAS, ni = Total_Group)
  k <- nrow(dat)
  if (k < 2) return(list(res = NULL, reason = sprintf("GLMM: only %d %s cohort(s) (at least 2 needed)", k, subtype)))

  es  <- escalc(measure = "PLO", xi = xi, ni = ni, data = dat, add = 1/2, to = "only0")
  fit <- tryCatch(suppressWarnings(rma.glmm(measure = "PLO", xi = xi, ni = ni, data = dat)),
                  error = function(e) NULL)
  if (is.null(fit)) return(list(res = NULL, reason = sprintf("GLMM for %s failed to fit", subtype)))

  tau2 <- as.numeric(fit$tau2)
  res  <- c(list(subtype = subtype, k = k, tau2 = tau2),
            logit_summary(as.numeric(coef(fit)), as.numeric(fit$se), tau2, k))

  w   <- 1 / (es$vi + tau2)
  wfe <- 1 / es$vi
  s2  <- (k - 1) * sum(wfe) / (sum(wfe)^2 - sum(wfe^2))
  dat$Weight_Pct <- w / sum(w) * 100
  res$I2 <- if (!is.null(fit$I2) && is.finite(fit$I2)) as.numeric(fit$I2) else 100 * tau2 / (tau2 + s2)
  res$study_data <- dat
  list(res = res, reason = NULL)
}

het_spec <- list(
  pdf = paste0(config$figure_names$heterogeneity, ".pdf"),
  csv = paste0("Table_", config$figure_names$heterogeneity, ".csv"),
  title = "Between-Cohort Heterogeneity of RAS Mutation Rate",
  subtitle = "Binomial-normal GLMM (logit link) | all cohorts | dashed bar = 95% prediction interval",
  size_name = "Approx. RE weight (%)", weight_col = "Approx_RE_Weight_Pct",
  pooled_row = "Pooled (binomial-normal GLMM, logit)",
  panel_label = function(r) sprintf(
    "tau2 (logit) = %.3f   |   I2 = %s",
    r$tau2, ifelse(is.na(r$I2), "N/A", sprintf("%.1f%%", r$I2))),
  table_label = function(r) sprintf(
    "k = %d; tau2 (logit) = %.4f; I2 = %s",
    r$k, r$tau2, ifelse(is.na(r$I2), "N/A", sprintf("%.1f%%", r$I2)))
)

build_heterogeneity_panel <- function(r, spec, color_value, size_limits) {
  studies <- r$study_data %>% mutate(Study_Label = sprintf("%s (N=%d)", Study_cohort, Total_Group))
  lab_pooled <- "POOLED ESTIMATE"
  lab_pi     <- "95% PREDICTION INTERVAL"
  label_levels <- c(lab_pi, lab_pooled, rev(studies$Study_Label))
  studies$Study_Order <- factor(studies$Study_Label, levels = label_levels)
  pooled <- data.frame(Study_Order = factor(lab_pooled, levels = label_levels),
                       Pct = r$pooled_pct, lo = r$ci_low_pct, hi = r$ci_high_pct)
  pint   <- data.frame(Study_Order = factor(lab_pi, levels = label_levels),
                       lo = r$pi_low_pct, hi = r$pi_high_pct)
  x_max <- max(c(studies$CI_high, pooled$hi, pint$hi), na.rm = TRUE) + 5

  p <- ggplot() +
    geom_vline(xintercept = 0, color = "grey70") +
    geom_errorbar(data = studies, aes(x = Pct_RAS, y = Study_Order, xmin = CI_low, xmax = CI_high),
                  width = 0.25, linewidth = 0.6, color = color_value, orientation = "y") +
    geom_point(data = studies, aes(x = Pct_RAS, y = Study_Order, size = Weight_Pct),
               shape = 16, color = color_value) +
    geom_errorbar(data = pooled, aes(x = Pct, y = Study_Order, xmin = lo, xmax = hi),
                  width = 0.35, linewidth = 1.1, color = "black", orientation = "y") +
    geom_point(data = pooled, aes(x = Pct, y = Study_Order), shape = 18, size = 5.5, color = "black")

  if (all(is.finite(c(pint$lo, pint$hi)))) {
    p <- p + geom_errorbar(data = pint, aes(y = Study_Order, xmin = lo, xmax = hi),
                           width = 0.25, linewidth = 0.8, color = "grey35", linetype = "dashed",
                           orientation = "y")
  }

  p +
    scale_size_continuous(range = c(2.5, 8), name = spec$size_name, limits = size_limits,
                          guide = guide_legend(override.aes = list(colour = "grey40"))) +
    scale_x_continuous(limits = c(0, x_max)) +
    scale_y_discrete(drop = FALSE) +
    labs(title = r$subtype, subtitle = spec$panel_label(r), x = "RAS Mutation Rate (%)", y = NULL) +
    theme_bw(base_size = 11) +
    theme(plot.title = element_text(face = "bold", size = 12, hjust = 0.5, color = color_value),
          plot.subtitle = element_text(size = 8.5, face = "italic", hjust = 0.5),
          axis.text.y = element_text(size = 9, color = "black"),
          panel.grid.minor = element_blank())
}

build_heterogeneity_table <- function(r, spec) {
  per_study <- r$study_data %>%
    transmute(Melanoma_Type, Study_cohort, Total_Group, Total_RAS,
              Observed_Pct = round(Pct_RAS, 2), CI_low_Pct = round(CI_low, 2),
              CI_high_Pct = round(CI_high, 2), W = round(Weight_Pct, 2),
              Row_Type = "Individual study (Clopper-Pearson CI)")
  pooled <- data.frame(Melanoma_Type = r$subtype, Study_cohort = "POOLED ESTIMATE",
                       Total_Group = sum(r$study_data$Total_Group), Total_RAS = sum(r$study_data$Total_RAS),
                       Observed_Pct = round(r$pooled_pct, 2), CI_low_Pct = round(r$ci_low_pct, 2),
                       CI_high_Pct = round(r$ci_high_pct, 2), W = NA_real_, Row_Type = spec$pooled_row)
  pint <- data.frame(Melanoma_Type = r$subtype, Study_cohort = "95% PREDICTION INTERVAL",
                     Total_Group = NA, Total_RAS = NA, Observed_Pct = NA_real_,
                     CI_low_Pct = round(r$pi_low_pct, 2), CI_high_Pct = round(r$pi_high_pct, 2),
                     W = NA_real_, Row_Type = "Prediction interval (t, k-2 df)")
  het <- data.frame(Melanoma_Type = r$subtype, Study_cohort = "HETEROGENEITY",
                    Total_Group = NA, Total_RAS = NA, Observed_Pct = NA_real_,
                    CI_low_Pct = NA_real_, CI_high_Pct = NA_real_, W = NA_real_,
                    Row_Type = spec$table_label(r))
  tbl <- bind_rows(per_study, pooled, pint, het)
  names(tbl)[names(tbl) == "W"] <- spec$weight_col
  tbl
}

make_heterogeneity_figure <- function(study_data) {
  spec <- het_spec
  fits <- lapply(config$subtypes, fit_heterogeneity, study_data = study_data)
  if (any(vapply(fits, function(f) is.null(f$res), logical(1)))) {
    report_skip(spec$pdf, paste(unlist(lapply(fits, `[[`, "reason")), collapse = " | "))
    return(invisible(NULL))
  }
  res <- lapply(fits, `[[`, "res")
  w_limits <- range(unlist(lapply(res, function(r) r$study_data$Weight_Pct)), na.rm = TRUE)

  p <- (build_heterogeneity_panel(res[[1]], spec, pal$subtype[[res[[1]]$subtype]], w_limits) /
          build_heterogeneity_panel(res[[2]], spec, pal$subtype[[res[[2]]$subtype]], w_limits)) +
    plot_layout(guides = "collect") +
    plot_annotation(title = spec$title, subtitle = spec$subtitle,
                    theme = theme(plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
                                  plot.subtitle = element_text(face = "italic", size = 9, hjust = 0.5)))

  save_pdf(p, spec$pdf, width = 11, height = 8.5)
  write.csv(bind_rows(lapply(res, build_heterogeneity_table, spec = spec)), out(spec$csv), row.names = FALSE)
  invisible(p)
}

# ==============================================================================
# 5. FIGURE 2 - PER-COHORT GENE COMPOSITION
# ==============================================================================
# ---- Figure 2: per-cohort gene composition of RAS-mutant samples -------------
build_composition_data <- function(clean, genes) {
  clean %>%
    transmute(Study_cohort, Melanoma_Type, NRAS = RAS_NRAS, KRAS = RAS_KRAS, HRAS = RAS_HRAS) %>%
    mutate(N_RAS = NRAS + KRAS + HRAS) %>%
    filter(N_RAS > 0) %>%
    pivot_longer(c(NRAS, KRAS, HRAS), names_to = "Gene", values_to = "n") %>%
    mutate(Gene = factor(Gene, levels = genes), Pct = n / N_RAS * 100) %>%
    arrange(Study_cohort, Melanoma_Type, Gene) %>%
    group_by(Study_cohort, Melanoma_Type) %>%
    mutate(end = pmin(cumsum(Pct), 100), start = pmax(end - Pct, 0)) %>%
    ungroup()
}

build_composition_rows <- function(comp) {
  rows <- comp %>%
    distinct(Study_cohort, Melanoma_Type, N_RAS) %>%
    group_by(Study_cohort) %>%
    summarise(has_AM = any(Melanoma_Type == "Acral"), has_CM = any(Melanoma_Type == "Cutaneous"),
              tot = sum(N_RAS), .groups = "drop") %>%
    mutate(block = case_when(has_CM & !has_AM ~ 1L, has_AM & has_CM ~ 2L, TRUE ~ 3L)) %>%
    arrange(block, desc(tot), Study_cohort)
  rows$y <- rev(seq_len(nrow(rows)))
  rows
}

build_composition_panel <- function(comp, rows, subtype, side, genes) {
  left <- side == "left"
  dd <- comp %>%
    filter(Melanoma_Type == subtype) %>%
    left_join(select(rows, Study_cohort, y), by = "Study_cohort") %>%
    mutate(xmin = if (left) -end else start, xmax = if (left) -start else end, xmid = (xmin + xmax) / 2)
  labs_df <- dd %>% distinct(y, Study_cohort, N_RAS) %>%
    mutate(label = if (left) sprintf("%s (%.0f)", Study_cohort, N_RAS) else sprintf("(%.0f) %s", N_RAS, Study_cohort))
  col_sub <- pal$subtype[[subtype]]

  ggplot(dd) +
    geom_rect(aes(xmin = xmin, xmax = xmax, ymin = y - 0.38, ymax = y + 0.38, fill = Gene),
              color = "black", linewidth = 0.3) +
    scale_fill_manual(values = pal$composition, limits = genes, drop = FALSE, name = NULL) +
    scale_x_continuous(limits = if (left) c(-100, 0) else c(0, 100),
                       breaks = if (left) seq(-100, 0, 20) else seq(0, 100, 20), labels = abs,
                       expand = expansion(mult = 0), oob = scales::oob_keep) +
    scale_y_continuous(breaks = labs_df$y, labels = labs_df$label, limits = c(0.5, nrow(rows) + 0.5),
                       expand = expansion(mult = 0), position = if (left) "left" else "right") +
    labs(title = paste(subtype, "Melanoma"), x = "Proportion of RAS-mutant samples (%)", y = NULL) +
    theme_classic(base_size = 11) +
    theme(plot.title       = element_text(face = "bold", hjust = 0.5, color = col_sub, size = 13),
          axis.text.y      = element_text(face = "bold", color = col_sub, size = 9),
          axis.text.x      = element_text(color = "black"),
          axis.line.y      = element_blank(),
          axis.ticks.y     = element_blank(),
          panel.background = element_blank(),
          legend.position  = "bottom")
}

make_composition_figure <- function(clean, genes = config$ras_genes) {
  comp <- build_composition_data(clean, genes)
  rows <- build_composition_rows(comp)
  p <- (build_composition_panel(comp, rows, "Acral", "left", genes) |
          (build_composition_panel(comp, rows, "Cutaneous", "right", genes) + guides(fill = "none"))) +
    plot_layout(guides = "collect") +
    plot_annotation(
      title    = "Gene Composition of RAS-Mutant Samples per Cohort",
      subtitle = "Each bar = 100% of the RAS-mutant samples of the cohort (number in parentheses = n RAS-mutant)",
      theme = theme(plot.title    = element_text(face = "bold", size = 14, hjust = 0.5),
                    plot.subtitle = element_text(face = "italic", size = 9, hjust = 0.5))) &
    theme(legend.position = "bottom")

  fig_name <- config$figure_names$composition
  save_pdf(p, paste0(fig_name, ".pdf"), width = 12, height = max(5, 0.45 * nrow(rows) + 2.2))
  write.csv(select(comp, Study_cohort, Melanoma_Type, Gene, n, N_RAS, Pct),
            out(paste0("Table_", fig_name, ".csv")), row.names = FALSE)
  invisible(p)
}

# ==============================================================================
# 6. FIGURE 3 - POOLED PROPORTIONS
# ==============================================================================
# ---- Mutation hierarchy (gene -> NRAS codon -> Q61 variant) ------------------
count_within <- function(d, col, group_levels) {
  d %>%
    count(across(all_of(c("Study_cohort", "Melanoma_Type", col))), name = "n_event") %>%
    rename(Group = all_of(col)) %>%
    group_by(Study_cohort, Melanoma_Type) %>%
    complete(Group = group_levels, fill = list(n_event = 0)) %>%
    mutate(n_total = sum(n_event)) %>%
    ungroup() %>%
    filter(n_total > 0)
}

build_hierarchy <- function(base, cfg) {
  gene  <- base %>% filter(is_RAS, RAS_specific %in% cfg$ras_genes)
  codon <- base %>% filter(is_RAS, RAS_specific == "NRAS") %>%
    mutate(NRAS_Codon = str_extract(str_to_upper(NRAS), "Q61|G12|G13")) %>%
    filter(!is.na(NRAS_Codon))
  q61 <- codon %>% filter(NRAS_Codon == "Q61") %>%
    mutate(Q61_hit_count = str_count(str_to_upper(NRAS), cfg$q61_pattern),
           Q61_Variant   = ifelse(Q61_hit_count > 1, "Q61 dual hit", str_extract(str_to_upper(NRAS), cfg$q61_pattern))) %>%
    filter(!is.na(Q61_Variant))
  list(
    gene = gene, codon = codon, q61 = q61,
    per_study = list(
      genes  = count_within(gene,  "RAS_specific", cfg$ras_genes),
      codons = count_within(codon, "NRAS_Codon",   cfg$nras_codons),
      q61    = count_within(q61,   "Q61_Variant",  cfg$q61_variants)
    )
  )
}

# ---- Pooled proportions (binomial-normal GLMM) -------------------------------
fit_me_proportion_meta <- function(xi, ni, study_cohort) {
  dat <- data.frame(xi = xi, ni = ni, Study_cohort = study_cohort)
  fit <- tryCatch(rma.glmm(measure = "PLO", xi = xi, ni = ni, data = dat), error = function(e) NULL)
  if (!is.null(fit)) {
    pred <- tryCatch(predict(fit, transf = transf.ilogit), error = function(e) NULL)
    first_pct <- function(x) if (!is.null(pred) && length(x) > 0) as.numeric(x[1]) * 100 else NA_real_
    return(list(method = "rma.glmm (binomial-normal GLMM)",
                pooled_pct = first_pct(pred$pred), ci_low_pct = first_pct(pred$ci.lb), ci_high_pct = first_pct(pred$ci.ub),
                tau2 = as.numeric(fit$tau2), I2 = if (!is.null(fit$I2)) as.numeric(fit$I2) else NA_real_))
  }
  if (!requireNamespace("lme4", quietly = TRUE)) stop("Package 'lme4' required.")
  g <- lme4::glmer(cbind(xi, ni - xi) ~ 1 + (1 | Study_cohort), data = dat, family = binomial(link = "logit"))
  tau2 <- as.numeric(lme4::VarCorr(g)$Study_cohort[1])
  est  <- lme4::fixef(g)[["(Intercept)"]]
  se   <- sqrt(as.numeric(vcov(g)[1, 1]))
  list(method = "lme4::glmer (logistic GLMM, fallback)",
       pooled_pct = plogis(est) * 100, ci_low_pct = plogis(est - 1.96 * se) * 100,
       ci_high_pct = plogis(est + 1.96 * se) * 100,
       tau2 = tau2, I2 = 100 * tau2 / (tau2 + (pi^2 / 3)))
}

fit_overall_ras <- function(per_study, subtype) {
  d <- per_study[per_study$Melanoma_Type == subtype, ]
  fit_me_proportion_meta(d$Total_RAS, d$Total_Group, d$Study_cohort)
}

level1_table <- function(fa, fc) {
  data.frame(
    Melanoma_Type = c("Acral", "Acral", "Cutaneous", "Cutaneous"),
    Group       = c("RAS-mutant", "RAS wild-type", "RAS-mutant", "RAS wild-type"),
    Pct         = c(fa$pooled_pct, 100 - fa$pooled_pct, fc$pooled_pct, 100 - fc$pooled_pct),
    CI_low_Pct  = c(fa$ci_low_pct, 100 - fa$ci_high_pct, fc$ci_low_pct, 100 - fc$ci_high_pct),
    CI_high_Pct = c(fa$ci_high_pct, 100 - fa$ci_low_pct, fc$ci_high_pct, 100 - fc$ci_low_pct),
    stringsAsFactors = FALSE
  )
}

run_grouped_proportion_meta <- function(per_study_tbl, group_values) {
  combos <- expand.grid(Melanoma_Type = c("Acral", "Cutaneous"), Group = group_values, stringsAsFactors = FALSE)
  na_row <- function(subtype, grp, k, why)
    data.frame(Melanoma_Type = subtype, Group = grp, Pooled_Pct = NA_real_, CI_low_Pct = NA_real_, CI_high_Pct = NA_real_,
               PI_low_Pct = NA_real_, PI_high_Pct = NA_real_, I2 = NA_real_, tau2 = NA_real_, n_studies = k, Row_Type = why)
  bind_rows(lapply(seq_len(nrow(combos)), function(i) {
    subtype <- combos$Melanoma_Type[i]; grp <- combos$Group[i]
    sub <- per_study_tbl %>% filter(Melanoma_Type == subtype, Group == grp)
    k <- nrow(sub)
    if (k < 2) return(na_row(subtype, grp, k, sprintf("only %d cohort(s) report this subgroup", k)))
    res <- tryCatch(fit_me_proportion_meta(sub$n_event, sub$n_total, sub$Study_cohort), error = function(e) NULL)
    if (is.null(res) || length(res$pooled_pct) == 0 || is.na(res$pooled_pct))
      return(na_row(subtype, grp, k, "model failed or not estimable"))
    data.frame(Melanoma_Type = subtype, Group = grp, Pooled_Pct = res$pooled_pct[1],
               CI_low_Pct = res$ci_low_pct[1], CI_high_Pct = res$ci_high_pct[1],
               PI_low_Pct = NA_real_, PI_high_Pct = NA_real_, I2 = res$I2[1], tau2 = res$tau2[1],
               Method = res$method[1], n_studies = k, Row_Type = "Meta-analyzed")
  }))
}

# ---- Figure 3a-3d (dot plots) ------------------------------------------------
plot_pooled_dot <- function(meta_df, colors, group_levels, title, subtitle, compact) {
  dd <- meta_df %>%
    mutate(Melanoma_Type = factor(Melanoma_Type, levels = c("Acral", "Cutaneous")),
           Group = factor(Group, levels = group_levels)) %>%
    filter(!is.na(Pooled_Pct))
  if (nrow(dd) == 0) return(NULL)

  ggplot(dd, aes(x = Group, y = Pooled_Pct, color = Group)) +
    geom_point(size = 4.5) +
    geom_errorbar(aes(ymin = pmax(CI_low_Pct, 0), ymax = pmin(CI_high_Pct, 100)), width = 0.2, linewidth = 1) +
    geom_text(aes(label = sprintf("%.1f%%", Pooled_Pct), y = pmin(CI_high_Pct, 100) + 4),
              vjust = 0, size = 3.5, fontface = "bold", show.legend = FALSE) +
    facet_wrap(~ Melanoma_Type, ncol = 2) +
    scale_color_manual(values = colors, guide = "none") +
    scale_y_continuous(limits = c(0, 110), breaks = seq(0, 100, 20), labels = function(x) paste0(x, "%")) +
    theme_classic(base_size = 12) +
    labs(title = title, subtitle = subtitle, x = NULL, y = "Pooled Proportion (%)") +
    theme(plot.title = element_text(face = "bold", hjust = 0.5, size = if (compact) 11),
          plot.subtitle = element_text(face = "italic", hjust = 0.5, size = if (compact) 9),
          strip.background = element_blank(),
          strip.text = element_text(face = "bold", size = 12),
          axis.text.x = element_text(face = "bold", color = "black", size = 11,
                                     angle = if (compact) 30, hjust = if (compact) 1),
          axis.text.y = element_text(face = "bold", color = "black"),
          panel.grid.major.y = element_line(color = "grey90", linetype = "dashed"),
          panel.grid.minor = element_blank())
}

proportion_level_specs <- function(cfg) list(
  list(key = "overall_ras", title = "Overall RAS Mutation Status", dot_suffix = "",
       colors = pal$ras_status, levels = c("RAS-mutant", "RAS wild-type"), compact = FALSE),
  list(key = "genes", title = "Distribution of RAS Genes", dot_suffix = " (Corrected)",
       colors = pal$gene, levels = cfg$ras_genes, compact = TRUE),
  list(key = "codons", title = "NRAS Mutation Spectrum", dot_suffix = " (Corrected)",
       colors = pal$codon, levels = cfg$nras_codons, compact = TRUE),
  list(key = "q61", title = "NRAS Q61 Substitution Spectrum", dot_suffix = " (Corrected)",
       colors = pal$q61, levels = cfg$q61_variants, compact = TRUE)
)

draw_proportion_set <- function(s, plot_df, fig_name, method_name, scope_note) {
  sub_dot <- sprintf("%s pooled proportion (95%% CI; %s)", method_name, scope_note)
  p <- plot_pooled_dot(plot_df, s$colors, s$levels, paste0(s$title, s$dot_suffix), sub_dot, s$compact)
  if (!is.null(p))
    save_pdf(p, paste0(fig_name, ".pdf"), config$bar_width, config$bar_height)
}

make_proportion_figures <- function(d, hier, scope, method_name, scope_note, cfg) {
  cohorts <- if (scope == "matched") d$paired else unique(d$base$Study_cohort)
  per_study <- d$per_study %>% filter(Study_cohort %in% cohorts)
  tables <- list()

  for (s in proportion_level_specs(cfg)) {
    fig_name <- cfg$figure_names[[s$key]][[scope]]
    if (s$key == "overall_ras") {
      df_l1 <- level1_table(fit_overall_ras(per_study, "Acral"), fit_overall_ras(per_study, "Cutaneous"))
      tables[[fig_name]] <- df_l1
      plot_df <- rename(df_l1, Pooled_Pct = Pct)
    } else {
      plot_df <- run_grouped_proportion_meta(filter(hier$per_study[[s$key]], Study_cohort %in% cohorts), s$levels)
      tables[[fig_name]] <- plot_df
    }
    draw_proportion_set(s, plot_df, fig_name, method_name, scope_note)
  }
  tables
}

# ==============================================================================
# 7. FIGURE 4 - ODDS RATIOS (AM vs CM)
# ==============================================================================
# ---- Count tables ------------------------------------------------------------
build_subtype_counts <- function(patient_df) {
  patient_df %>% group_by(Study_cohort, Melanoma_Type) %>%
    summarise(n_event = sum(Event, na.rm = TRUE), n_total = n(), .groups = "drop")
}

build_matched_cohort_2x2 <- function(patient_df) {
  matched <- build_subtype_counts(patient_df) %>% group_by(Study_cohort) %>%
    filter(n_distinct(Melanoma_Type) == 2) %>% ungroup()
  if (nrow(matched) == 0) return(data.frame())
  matched %>%
    pivot_wider(id_cols = Study_cohort, names_from = Melanoma_Type, values_from = c(n_event, n_total)) %>%
    transmute(Study_cohort, ai = n_event_Acral, bi = n_total_Acral - n_event_Acral,
              ci = n_event_Cutaneous, di = n_total_Cutaneous - n_event_Cutaneous) %>%
    filter((ai + bi) > 0, (ci + di) > 0)
}

# ---- OR models: all cohorts --------------------------------------------------
fit_glmer_or <- function(counts) {
  if (nrow(counts) < 3 || n_distinct(counts$Melanoma_Type) < 2 || n_distinct(counts$Study_cohort) < 2) return(NULL)
  by_subtype <- counts %>% group_by(Melanoma_Type) %>%
    summarise(ev = sum(n_event), non = sum(n_total - n_event), .groups = "drop")
  if (any(by_subtype$ev == 0) || any(by_subtype$non == 0)) return(NULL)
  if (!requireNamespace("lme4", quietly = TRUE)) stop("Package 'lme4' required.")
  counts$Subtype <- factor(counts$Melanoma_Type, levels = c("Cutaneous", "Acral"))
  fit <- tryCatch(suppressWarnings(lme4::glmer(
    cbind(n_event, n_total - n_event) ~ Subtype + (1 | Study_cohort), data = counts,
    family = binomial(link = "logit"), control = lme4::glmerControl(optimizer = "bobyqa"))), error = function(e) NULL)
  if (is.null(fit)) return(NULL)
  cf   <- summary(fit)$coefficients
  est  <- cf["SubtypeAcral", "Estimate"]
  se   <- cf["SubtypeAcral", "Std. Error"]
  tau2 <- as.numeric(lme4::VarCorr(fit)$Study_cohort[1])
  list(or = exp(est), ci_low = exp(est - 1.96 * se), ci_high = exp(est + 1.96 * se), p = cf["SubtypeAcral", "Pr(>|z|)"],
       I2 = 100 * tau2 / (tau2 + pi^2 / 3), tau2 = tau2, Q = NA_real_, Q_df = NA_integer_, Q_p = NA_real_,
       n_studies = n_distinct(counts$Study_cohort), n_obs = nrow(counts), model = "lme4::glmer (GLMM)")
}

# ---- OR models: paired cohorts -----------------------------------------------
fit_me_or_meta <- function(table_2x2) {
  if (nrow(table_2x2) < 2) return(NULL)
  fit <- tryCatch(suppressWarnings(rma.glmm(measure = "OR", ai = ai, bi = bi, ci = ci, di = di,
                                            data = table_2x2, model = "UM.RS")), error = function(e) NULL)
  if (!is.null(fit))
    return(list(or = exp(as.numeric(fit$b)), ci_low = exp(fit$ci.lb), ci_high = exp(fit$ci.ub), p = as.numeric(fit$pval),
                I2 = if (!is.null(fit$I2)) fit$I2 else NA_real_, tau2 = fit$tau2,
                Q = if (!is.null(fit$QE.Wld)) fit$QE.Wld else NA_real_, Q_df = fit$k - 1,
                Q_p = if (!is.null(fit$QEp.Wld)) fit$QEp.Wld else NA_real_,
                n_studies = fit$k, n_obs = 2L * fit$k, model = "rma.glmm (GLMM)"))
  long <- bind_rows(transmute(table_2x2, Study_cohort, Melanoma_Type = "Acral",     n_event = ai, n_total = ai + bi),
                    transmute(table_2x2, Study_cohort, Melanoma_Type = "Cutaneous", n_event = ci, n_total = ci + di))
  fit_glmer_or(long)
}

# ---- OR tables for every subgroup --------------------------------------------
build_subgroup_specs <- function(d, hier, cfg) {
  specs <- list("RAS-mutant" = list(level = "Level 1: Overall RAS", data = mutate(d$base, Event = is_RAS)))
  for (g in cfg$ras_genes)
    specs[[paste0(g, "-mutant")]] <- list(level = "Level 2: RAS Genes", data = mutate(hier$gene, Event = RAS_specific == g))
  for (cd in cfg$nras_codons)
    specs[[paste0("NRAS ", cd, "-mutant")]] <- list(level = "Level 3: NRAS Codons", data = mutate(hier$codon, Event = NRAS_Codon == cd))
  for (v in cfg$q61_variants)
    specs[[paste0("NRAS ", v)]] <- list(level = "Level 4: NRAS Q61 Variants", data = mutate(hier$q61, Event = Q61_Variant == v))
  specs
}

fit_all_or <- function(specs, scope, paired) {
  rows <- lapply(names(specs), function(nm) {
    spec <- specs[[nm]]
    level_label <- sprintf("%s (%s)", spec$level, if (scope == "matched") "Paired Cohorts" else "All Cohorts")
    if (scope == "matched") {
      tbl <- build_matched_cohort_2x2(filter(spec$data, Study_cohort %in% paired))
      n_avail <- nrow(tbl)
      res <- fit_me_or_meta(tbl)
    } else {
      counts <- build_subtype_counts(spec$data)
      n_avail <- n_distinct(counts$Study_cohort)
      res <- fit_glmer_or(counts)
    }
    if (is.null(res))
      return(data.frame(`Analysis Level` = level_label, `Mutation / Group` = nm, Model = "Not estimable",
                        OR_num = NA_real_, CI_low_num = NA_real_, CI_high_num = NA_real_, p_value = NA_real_,
                        I2 = NA_real_, tau2 = NA_real_, Q = NA_real_, Q_df = NA_real_, Q_p = NA_real_,
                        n_studies = n_avail, n_obs = NA_integer_, Reason = "insufficient data or non-convergence",
                        check.names = FALSE))
    data.frame(`Analysis Level` = level_label, `Mutation / Group` = nm, Model = res$model,
               OR_num = res$or, CI_low_num = res$ci_low, CI_high_num = res$ci_high, p_value = res$p,
               I2 = res$I2, tau2 = res$tau2, Q = res$Q, Q_df = as.numeric(res$Q_df), Q_p = res$Q_p,
               n_studies = res$n_studies, n_obs = res$n_obs, Reason = NA_character_, check.names = FALSE)
  })
  bind_rows(rows) %>% group_by(`Analysis Level`) %>% mutate(p_BH = p.adjust(p_value, method = "BH")) %>%
    ungroup() %>% as.data.frame(check.names = FALSE)
}

# ---- Figure 4: OR forest plot (all levels) -----------------------------------
build_or_forest_plot <- function(or_df, title, subtitle) {
  or_df <- or_df %>% filter(!(`Mutation / Group` %in% config$forest_hidden_groups))
  if (nrow(or_df) == 0) return(NULL)
  plottable <- with(or_df, is.finite(OR_num) & is.finite(CI_low_num) & is.finite(CI_high_num) &
                      OR_num > 0 & CI_low_num > 0 & CI_high_num > 0)
  plottable[is.na(plottable)] <- FALSE
  dd <- or_df[plottable, , drop = FALSE]
  if (nrow(dd) == 0) return(NULL)

  present_order <- unique(as.character(dd$`Mutation / Group`))
  display_levels <- rev(present_order)
  dd <- dd %>% mutate(`Mutation / Group` = factor(`Mutation / Group`, levels = display_levels),
                      stat_label = sprintf("p = %s (BH: %s)\nk = %d",
                                           fmt_p(p_value, eq = FALSE), fmt_p(p_BH, eq = FALSE), n_studies))
  finite_vals <- c(dd$CI_low_num, dd$CI_high_num, dd$OR_num)
  finite_vals <- finite_vals[is.finite(finite_vals) & finite_vals > 0]
  x_min <- 10^floor(log10(min(finite_vals, 0.1)))
  x_max <- 10^ceiling(log10(max(finite_vals, 10)))
  log_breaks <- 10^seq(log10(x_min), log10(x_max))
  text_x_pos <- x_max * 2.2
  n_items <- nlevels(droplevels(dd$`Mutation / Group`))

  ggplot(dd, aes(y = `Mutation / Group`, x = OR_num, color = `Mutation / Group`)) +
    geom_vline(xintercept = 1, linetype = "dashed", color = "gray50", linewidth = 0.6) +
    geom_errorbar(aes(xmin = CI_low_num, xmax = CI_high_num), orientation = "y", width = 0.25, linewidth = 0.8, na.rm = TRUE) +
    geom_point(size = 3.5, na.rm = TRUE) +
    geom_text(aes(x = text_x_pos, label = stat_label), hjust = 0, size = 3.0, fontface = "bold", color = "black",
              show.legend = FALSE, na.rm = TRUE) +
    scale_x_log10(breaks = log_breaks, labels = function(x) formatC(x, format = "fg", drop0trailing = TRUE)) +
    scale_color_manual(values = pal$forest, guide = "none") +
    annotate("text", x = text_x_pos, y = n_items + 0.8, label = "p-value (BH); k", fontface = "bold", size = 3.2,
             hjust = 0, color = "black") +
    coord_cartesian(xlim = c(x_min, x_max), clip = "off") +
    theme_classic(base_size = 12) +
    labs(title = title, subtitle = subtitle, x = "Odds Ratio (CM = 1, log scale)", y = "Mutation / Group") +
    theme(plot.title = element_text(face = "bold", hjust = 0),
          plot.subtitle = element_text(face = "italic", hjust = 0, size = 9.5),
          axis.text.y = element_text(face = "bold", color = "black", size = 10),
          axis.text.x = element_text(face = "bold", color = "black"),
          axis.title = element_text(face = "bold"),
          plot.margin = margin(t = 15, r = 190, b = 15, l = 15))
}

or_subtitles <- c(
  all     = "SENSITIVITY: Logistic GLMM (random study intercept), all cohorts",
  matched = "PRIMARY INFERENCE: Logistic GLMM of within-cohort ORs, paired cohorts only")

make_or_figure <- function(or_df, scope, fig_name) {
  dd <- if (scope == "matched") filter(or_df, `Mutation / Group` != config$or_hidden_matched) else or_df
  p  <- build_or_forest_plot(dd, "Forest Plot of Odds Ratios (AM vs CM)", or_subtitles[[scope]])
  if (!is.null(p)) save_pdf(p, paste0(fig_name, ".pdf"), width = 10, height = 6.8)
}

# ==============================================================================
# 8. EXPORT
# ==============================================================================
# ---- CSV exports, skip log and session record --------------------------------
export_tables <- function(exports) {
  for (entry in exports) {
    for (fig_name in names(entry$bar_tables))
      write.csv(entry$bar_tables[[fig_name]], out(paste0("Table_", fig_name, ".csv")), row.names = FALSE)
    write.csv(entry$or_results, out(paste0("Table_", entry$or_name, ".csv")), row.names = FALSE)
  }
}

write_skip_log <- function() {
  if (length(skip_log$rows) > 0) {
    skipped <- bind_rows(skip_log$rows)
    write.csv(skipped, out("Figure_Generation_Log_Skipped.csv"), row.names = FALSE)
    message(sprintf("\n%d figure(s) skipped. Check Figure_Generation_Log_Skipped.csv.", nrow(skipped)))
  } else {
    message("\nAll figures were generated; nothing was skipped.")
  }
}

write_session_info <- function() {
  writeLines(c(sprintf("Run: %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
               sprintf("Input: %s (md5 %s)", normalizePath(config$input_file), tools::md5sum(config$input_file)),
               "", capture.output(sessionInfo())),
             out("sessionInfo.txt"))
}

# ==============================================================================
# 9. RUN
# ==============================================================================
# ---- Data --------------------------------------------------------------------
dir.create(config$output_dir, recursive = TRUE, showWarnings = FALSE)

message("--- Reading and cleaning the input file ---")
d <- load_data(config)

# ---- Figure 1 ----------------------------------------------------------------
message("--- Figure 1: GLMM heterogeneity ---")
make_heterogeneity_figure(d$per_study)

# ---- Figure 2 ----------------------------------------------------------------
message("--- Figure 2: Per-cohort RAS gene composition ---")
make_composition_figure(d$clean)

# ---- Figures 3 and 4 ---------------------------------------------------------
exports <- list()
if (d$schema == "patient") {
  message("--- Figures 3 and 4: Hierarchical proportions and odds ratios ---")
  hier  <- build_hierarchy(d$base, config)
  specs <- build_subgroup_specs(d, hier, config)

  scope_notes  <- c(all = "all cohorts (sensitivity analysis)",
                    matched = sprintf("%d paired cohorts", length(d$paired)))

  for (scope in names(scope_notes)) {
    message(sprintf("    Building figure set: %s", scope_notes[[scope]]))
    or_name <- config$figure_names$or_forest[[scope]]
    or_df <- fit_all_or(specs, scope, d$paired)
    bar_tables <- make_proportion_figures(d, hier, scope, "Mixed-Effects (GLMM)", scope_notes[[scope]], config)
    make_or_figure(or_df, scope, or_name)
    exports[[scope]] <- list(or_name = or_name, bar_tables = bar_tables, or_results = or_df)
  }
}

# ---- Export ------------------------------------------------------------------
message("--- Exporting CSV tables ---")
export_tables(exports)
write_skip_log()
write_session_info()
message("\nPipeline complete. PDFs and CSVs saved to: ", config$output_dir)
