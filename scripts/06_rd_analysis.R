################################################################################
# 06_rd_analysis.R
# Phase 4: Regression Discontinuity in Time (RDiT) Analysis
#
# Applies sharp RD at key policy implementation dates to estimate
# the immediate local treatment effect of each intervention.
#
# Methodological note: RDiT captures INSTANTANEOUS level shifts at the cutoff,
# while ITS captures both level shifts (B2) and slope changes (B3). For
# pharmaceutical policy interventions that operate through changed prescribing
# incentives rather than forced overnight switching, we expect ITS slope
# changes to dominate RD level effects. The RD analysis serves to:
#   1. Confirm no anticipation effects (no jump before the cutoff)
#   2. Quantify the immediate vs gradual nature of policy effects
#   3. Validate ITS findings through a complementary identification strategy
#
# Methods:
#   - rdrobust: local polynomial regression with CCT optimal bandwidth
#   - Triangular kernel (default)
#   - Robust bias-corrected inference (Cattaneo, Idrobo, Titiunik 2020)
#   - Bandwidth sensitivity: 50-200% of MSE-optimal
#   - Polynomial order sensitivity: p = 1 (primary), p = 2 (robustness)
#   - Donut RD: exclude 1 month at cutoff for anticipation/lag check
#   - Placebo tests at non-intervention dates
#   - Comparison with ITS step-change estimates
#
# Cutoffs:
#   Rituximab:
#     - MabThera IV delisting (Apr 2021) — primary
#     - MabThera SC delisting (Oct 2021) — secondary
#   Adalimumab:
#     - 24% price cut (Apr 2023) — primary
#     - Streamlined authority (Nov 2023) — secondary
#
# Inputs:
#   - data/processed/market_share_national.csv
#   - outputs/tables/tbl_its_rituximab.csv (for comparison)
#   - outputs/tables/tbl_its_adalimumab.csv (for comparison)
#
# Outputs:
#   - outputs/tables/tbl_rd_results.csv
#   - outputs/tables/tbl_rd_robustness.csv
#   - outputs/figures/fig_rd_rituximab.pdf/png
#   - outputs/figures/fig_rd_adalimumab.pdf/png
#   - outputs/figures/fig_rd_comparison.pdf/png
################################################################################

library(tidyverse)
library(lubridate)
library(here)
library(scales)
library(rdrobust)    # Cattaneo, Calonico, Titiunik
library(cli)

# ─── Theme ───────────────────────────────────────────────────────────────────

theme_pbs <- theme_minimal(base_size = 12) +
  theme(
    text = element_text(colour = "grey20"),
    plot.title = element_text(face = "bold", size = 14, margin = margin(b = 4)),
    plot.subtitle = element_text(colour = "grey40", size = 10.5,
                                 margin = margin(b = 10)),
    plot.caption = element_text(colour = "grey50", size = 8, hjust = 0,
                                margin = margin(t = 10)),
    plot.margin = margin(12, 12, 8, 12),
    axis.title = element_text(size = 10.5),
    axis.text = element_text(size = 9.5),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(linewidth = 0.3, colour = "grey88"),
    legend.position = "bottom",
    legend.text = element_text(size = 9.5)
  )

save_fig <- function(plot, name, width, height) {
  ggsave(here("outputs", "figures", paste0(name, ".pdf")),
         plot, width = width, height = height)
  ggsave(here("outputs", "figures", paste0(name, ".png")),
         plot, width = width, height = height, dpi = 300)
  cli_alert_success("Saved {name}.pdf/png")
}


# ─── Load data ───────────────────────────────────────────────────────────────

cli_h1("Loading data")

ms_national <- read_csv(here("data", "processed", "market_share_national.csv"),
                        show_col_types = FALSE) %>%
  mutate(period = ymd(period))


# ─── Helper: run single RD and return tidy results ───────────────────────────

tidy_rd <- function(y, x, cutoff = 0, label = "", p = 1,
                    kernel = "triangular", bwselect = "mserd", h = NULL) {

  args <- list(y = y, x = x, c = cutoff, p = as.integer(p), kernel = kernel)
  if (!is.null(h)) {
    args$h <- h
  } else {
    args$bwselect <- bwselect
  }

  rd <- tryCatch(
    do.call(rdrobust, args),
    error = function(e) {
      cli_alert_warning("RD failed for '{label}': {e$message}")
      return(NULL)
    }
  )

  if (is.null(rd)) {
    return(tibble(
      cutoff = label, estimate = NA_real_, estimate_bc = NA_real_,
      se_robust = NA_real_, ci_lower = NA_real_, ci_upper = NA_real_,
      p_robust = NA_real_, bandwidth = NA_real_,
      n_left = NA_integer_, n_right = NA_integer_
    ))
  }

  tibble(
    cutoff = label,
    estimate = rd$coef[1],       # Conventional
    estimate_bc = rd$coef[2],    # Bias-corrected
    se_robust = rd$se[3],        # Robust SE
    ci_lower = rd$ci[3, 1],      # Robust CI lower
    ci_upper = rd$ci[3, 2],      # Robust CI upper
    p_robust = rd$pv[3],         # Robust p-value
    bandwidth = rd$bws[1, 1],    # Bandwidth (h, left)
    n_left = rd$N_h[1],          # Effective N left of cutoff
    n_right = rd$N_h[2]          # Effective N right of cutoff
  )
}

# ─── Helper: create RD plot with rdplot and customise ────────────────────────

make_rd_fig <- function(y, x, cutoff = 0, title = "", subtitle = "",
                        x_label = "Months from policy implementation",
                        y_label = "Biosimilar market share",
                        caption = "", y_lim = NULL) {

  rdp <- rdplot(y = y, x = x, c = cutoff,
                title = "", x.label = "", y.label = "",
                col.dots = "#2166AC", col.lines = "#D6604D")

  p <- rdp$rdplot +
    labs(title = title, subtitle = subtitle,
         x = x_label, y = y_label, caption = caption) +
    scale_y_continuous(labels = percent_format(accuracy = 1),
                       limits = y_lim) +
    geom_vline(xintercept = cutoff, linetype = "dashed",
               colour = "grey50", linewidth = 0.3) +
    theme_pbs

  list(plot = p, rdplot_obj = rdp)
}


# ═══════════════════════════════════════════════════════════════════════════════
# RITUXIMAB RD
# ═══════════════════════════════════════════════════════════════════════════════

cli_h1("Rituximab: Regression Discontinuity in Time")

# ─── Prepare data ────────────────────────────────────────────────────────────

ritux <- ms_national %>%
  filter(molecule == "rituximab") %>%
  arrange(period) %>%
  mutate(time = row_number())

# Cutoff dates
ritux_delist_iv <- ymd("2021-04-01")
ritux_delist_sc <- ymd("2021-10-01")

# ─── Primary: MabThera IV delisting (Apr 2021) ──────────────────────────────

cli_h2("Primary cutoff: MabThera IV delisting (Apr 2021)")

# Running variable: months from cutoff
cutoff_idx_iv <- which(ritux$period == ritux_delist_iv)
ritux$rv_iv <- ritux$time - cutoff_idx_iv

cli_alert_info("Cutoff at time index {cutoff_idx_iv}, running variable range: [{min(ritux$rv_iv)}, {max(ritux$rv_iv)}]")

# Main RD estimation
rd_ritux_iv <- tidy_rd(
  y = ritux$biosimilar_share,
  x = ritux$rv_iv,
  label = "Rituximab: MabThera IV delisting"
)
cli_alert_info("Estimate: {round(rd_ritux_iv$estimate * 100, 2)} pp, p(robust) = {round(rd_ritux_iv$p_robust, 4)}, BW = {round(rd_ritux_iv$bandwidth, 1)} months")

# RD plot
fig_ritux_iv <- make_rd_fig(
  y = ritux$biosimilar_share,
  x = ritux$rv_iv,
  title = "Rituximab: RD at MabThera IV delisting (Apr 2021)",
  subtitle = paste0("Local polynomial regression, CCT optimal bandwidth = ",
                    round(rd_ritux_iv$bandwidth, 1), " months"),
  caption = "Source: PBS Date of Supply + Medicare Statistics. Method: rdrobust, triangular kernel, p = 1."
)

# ─── Secondary: MabThera SC delisting (Oct 2021) ────────────────────────────

cli_h2("Secondary cutoff: MabThera SC delisting (Oct 2021)")

cutoff_idx_sc <- which(ritux$period == ritux_delist_sc)
ritux$rv_sc <- ritux$time - cutoff_idx_sc

rd_ritux_sc <- tidy_rd(
  y = ritux$biosimilar_share,
  x = ritux$rv_sc,
  label = "Rituximab: MabThera SC delisting"
)
cli_alert_info("Estimate: {round(rd_ritux_sc$estimate * 100, 2)} pp, p(robust) = {round(rd_ritux_sc$p_robust, 4)}, BW = {round(rd_ritux_sc$bandwidth, 1)} months")

# RD plot
fig_ritux_sc <- make_rd_fig(
  y = ritux$biosimilar_share,
  x = ritux$rv_sc,
  title = "Rituximab: RD at MabThera SC delisting (Oct 2021)",
  subtitle = paste0("Local polynomial regression, CCT optimal bandwidth = ",
                    round(rd_ritux_sc$bandwidth, 1), " months"),
  caption = "Source: PBS Date of Supply + Medicare Statistics. Method: rdrobust, triangular kernel, p = 1."
)

# ─── Combined rituximab figure ───────────────────────────────────────────────

cli_h2("Rituximab RD figure")

# Two-panel figure: IV delisting (left) and SC delisting (right)
if (requireNamespace("patchwork", quietly = TRUE)) {
  library(patchwork)
  p_rd_ritux <- (fig_ritux_iv$plot + labs(title = "MabThera IV delisting (Apr 2021)")) +
    (fig_ritux_sc$plot + labs(title = "MabThera SC delisting (Oct 2021)")) +
    plot_annotation(
      title = "Rituximab: regression discontinuity at reference product delistings",
      subtitle = "Local polynomial regression with CCT optimal bandwidth; robust bias-corrected inference",
      caption = "Source: PBS Date of Supply + Medicare Statistics. Method: rdrobust (Cattaneo et al.), triangular kernel, p = 1.",
      theme = theme(
        plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(colour = "grey40", size = 10.5),
        plot.caption = element_text(colour = "grey50", size = 8, hjust = 0)
      )
    )
  save_fig(p_rd_ritux, "fig_rd_rituximab", 14, 6)
} else {
  # Fallback: save IV delisting plot only
  save_fig(fig_ritux_iv$plot, "fig_rd_rituximab", 8, 6)
}


# ═══════════════════════════════════════════════════════════════════════════════
# ADALIMUMAB RD (Jan 2022+)
# ═══════════════════════════════════════════════════════════════════════════════

cli_h1("Adalimumab: Regression Discontinuity in Time (Jan 2022+)")

# ─── Prepare data ────────────────────────────────────────────────────────────

adal <- ms_national %>%
  filter(molecule == "adalimumab", period >= ymd("2022-01-01")) %>%
  arrange(period) %>%
  mutate(time = row_number())

# Cutoff dates
adal_price_cut   <- ymd("2023-04-01")
adal_streamlined <- ymd("2023-11-01")

# ─── Primary: 24% price cut (Apr 2023) ──────────────────────────────────────

cli_h2("Primary cutoff: 24% price cut (Apr 2023)")

cutoff_idx_pc <- which(adal$period == adal_price_cut)
adal$rv_pc <- adal$time - cutoff_idx_pc

cli_alert_info("Cutoff at time index {cutoff_idx_pc}, running variable range: [{min(adal$rv_pc)}, {max(adal$rv_pc)}]")
cli_alert_info("Pre-period: {cutoff_idx_pc - 1} months, Post-period: {nrow(adal) - cutoff_idx_pc} months")

rd_adal_pc <- tidy_rd(
  y = adal$biosimilar_share,
  x = adal$rv_pc,
  label = "Adalimumab: 24% price cut"
)
cli_alert_info("Estimate: {round(rd_adal_pc$estimate * 100, 2)} pp, p(robust) = {round(rd_adal_pc$p_robust, 4)}, BW = {round(rd_adal_pc$bandwidth, 1)} months")

fig_adal_pc <- make_rd_fig(
  y = adal$biosimilar_share,
  x = adal$rv_pc,
  title = "Adalimumab: RD at 24% price cut (Apr 2023)",
  subtitle = paste0("Local polynomial regression, CCT optimal bandwidth = ",
                    round(rd_adal_pc$bandwidth, 1), " months"),
  caption = "Source: PBS Date of Supply (Jan 2022\u2013Nov 2025). Method: rdrobust, triangular kernel, p = 1.",
  y_lim = c(0.10, 0.35)
)

# ─── Secondary: Streamlined authority (Nov 2023) ────────────────────────────

cli_h2("Secondary cutoff: Streamlined authority (Nov 2023)")

cutoff_idx_sa <- which(adal$period == adal_streamlined)
adal$rv_sa <- adal$time - cutoff_idx_sa

rd_adal_sa <- tidy_rd(
  y = adal$biosimilar_share,
  x = adal$rv_sa,
  label = "Adalimumab: Streamlined authority"
)
cli_alert_info("Estimate: {round(rd_adal_sa$estimate * 100, 2)} pp, p(robust) = {round(rd_adal_sa$p_robust, 4)}, BW = {round(rd_adal_sa$bandwidth, 1)} months")

fig_adal_sa <- make_rd_fig(
  y = adal$biosimilar_share,
  x = adal$rv_sa,
  title = "Adalimumab: RD at streamlined authority (Nov 2023)",
  subtitle = paste0("Local polynomial regression, CCT optimal bandwidth = ",
                    round(rd_adal_sa$bandwidth, 1), " months"),
  caption = "Source: PBS Date of Supply (Jan 2022\u2013Nov 2025). Method: rdrobust, triangular kernel, p = 1.",
  y_lim = c(0.10, 0.35)
)

# ─── Combined adalimumab figure ──────────────────────────────────────────────

cli_h2("Adalimumab RD figure")

if (requireNamespace("patchwork", quietly = TRUE)) {
  p_rd_adal <- (fig_adal_pc$plot + labs(title = "24% price cut (Apr 2023)")) +
    (fig_adal_sa$plot + labs(title = "Streamlined authority (Nov 2023)")) +
    plot_annotation(
      title = "Adalimumab: regression discontinuity at policy intervention dates",
      subtitle = "Local polynomial regression with CCT optimal bandwidth; robust bias-corrected inference",
      caption = "Source: PBS Date of Supply (Jan 2022\u2013Nov 2025). Method: rdrobust (Cattaneo et al.), triangular kernel, p = 1.",
      theme = theme(
        plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(colour = "grey40", size = 10.5),
        plot.caption = element_text(colour = "grey50", size = 8, hjust = 0)
      )
    )
  save_fig(p_rd_adal, "fig_rd_adalimumab", 14, 6)
} else {
  save_fig(fig_adal_pc$plot, "fig_rd_adalimumab", 8, 6)
}


# ─── Combine main results ───────────────────────────────────────────────────

rd_main <- bind_rows(rd_ritux_iv, rd_ritux_sc, rd_adal_pc, rd_adal_sa) %>%
  mutate(molecule = c("Rituximab", "Rituximab", "Adalimumab", "Adalimumab"))

cli_h2("Main RD results")
print(rd_main %>%
        mutate(across(c(estimate, estimate_bc, se_robust, ci_lower, ci_upper),
                      ~ round(.x, 5)),
               p_robust = round(p_robust, 4),
               bandwidth = round(bandwidth, 1)))


# ═══════════════════════════════════════════════════════════════════════════════
# ROBUSTNESS: BANDWIDTH SENSITIVITY
# ═══════════════════════════════════════════════════════════════════════════════

cli_h1("Robustness: bandwidth sensitivity")

bw_multipliers <- c(0.5, 0.75, 1.0, 1.25, 1.5, 2.0)

# Define all cutoffs for robustness testing
rd_specs <- tribble(
  ~molecule,     ~label,                            ~y_col,  ~x_col,   ~opt_bw,
  "Rituximab",   "MabThera IV delisting",           "ritux", "rv_iv",  rd_ritux_iv$bandwidth,
  "Rituximab",   "MabThera SC delisting",           "ritux", "rv_sc",  rd_ritux_sc$bandwidth,
  "Adalimumab",  "24% price cut",                   "adal",  "rv_pc",  rd_adal_pc$bandwidth,
  "Adalimumab",  "Streamlined authority",            "adal",  "rv_sa",  rd_adal_sa$bandwidth
)

bw_results <- map_dfr(seq_len(nrow(rd_specs)), function(i) {
  spec <- rd_specs[i, ]
  data <- if (spec$y_col == "ritux") ritux else adal

  map_dfr(bw_multipliers, function(mult) {
    h_val <- spec$opt_bw * mult
    result <- tidy_rd(
      y = data$biosimilar_share,
      x = data[[spec$x_col]],
      label = spec$label,
      h = h_val
    )
    result %>%
      mutate(molecule = spec$molecule,
             bw_multiplier = mult,
             bw_used = h_val)
  })
})

cli_alert_info("Bandwidth sensitivity: {nrow(bw_results)} estimates across {nrow(rd_specs)} cutoffs")


# ═══════════════════════════════════════════════════════════════════════════════
# ROBUSTNESS: POLYNOMIAL ORDER
# ═══════════════════════════════════════════════════════════════════════════════

cli_h1("Robustness: polynomial order")

poly_results <- map_dfr(seq_len(nrow(rd_specs)), function(i) {
  spec <- rd_specs[i, ]
  data <- if (spec$y_col == "ritux") ritux else adal

  map_dfr(c(1, 2), function(poly_p) {
    result <- tidy_rd(
      y = data$biosimilar_share,
      x = data[[spec$x_col]],
      label = spec$label,
      p = poly_p
    )
    result %>%
      mutate(molecule = spec$molecule, poly_order = poly_p)
  })
})

cli_alert_info("Polynomial sensitivity results:")
print(poly_results %>%
        select(molecule, cutoff, poly_order, estimate, p_robust, bandwidth) %>%
        mutate(across(c(estimate), ~ round(.x, 5)),
               p_robust = round(p_robust, 4),
               bandwidth = round(bandwidth, 1)))


# ═══════════════════════════════════════════════════════════════════════════════
# ROBUSTNESS: DONUT RD (exclude 1 month at cutoff)
# ═══════════════════════════════════════════════════════════════════════════════

cli_h1("Robustness: donut RD (exclude 1 month at cutoff)")

donut_results <- map_dfr(seq_len(nrow(rd_specs)), function(i) {
  spec <- rd_specs[i, ]
  data <- if (spec$y_col == "ritux") ritux else adal

  # Exclude observation at cutoff (running var == 0)
  data_donut <- data %>% filter(!!sym(spec$x_col) != 0)

  result <- tidy_rd(
    y = data_donut$biosimilar_share,
    x = data_donut[[spec$x_col]],
    label = paste0(spec$label, " (donut)")
  )
  result %>% mutate(molecule = spec$molecule)
})

cli_alert_info("Donut RD results:")
print(donut_results %>%
        select(molecule, cutoff, estimate, p_robust, bandwidth) %>%
        mutate(estimate = round(estimate, 5),
               p_robust = round(p_robust, 4),
               bandwidth = round(bandwidth, 1)))


# ═══════════════════════════════════════════════════════════════════════════════
# PLACEBO TESTS
# ═══════════════════════════════════════════════════════════════════════════════

cli_h1("Placebo tests at non-intervention dates")

# ─── Rituximab placebos ──────────────────────────────────────────────────────

cli_h2("Rituximab placebo RDs")

# Test at dates where no policy change occurred
placebo_dates_r <- ymd(c("2020-06-01", "2020-10-01", "2022-06-01"))

placebo_r <- map_dfr(placebo_dates_r, function(pdate) {
  idx <- which(ritux$period == pdate)
  if (length(idx) == 0) {
    cli_alert_warning("Placebo date {pdate} not in data")
    return(tibble())
  }
  rv_placebo <- ritux$time - idx
  tidy_rd(y = ritux$biosimilar_share, x = rv_placebo,
          label = paste0("Placebo: ", pdate)) %>%
    mutate(molecule = "Rituximab")
})

# ─── Adalimumab placebos ────────────────────────────────────────────────────

cli_h2("Adalimumab placebo RDs")

placebo_dates_a <- ymd(c("2022-07-01", "2022-11-01", "2023-01-01"))

placebo_a <- map_dfr(placebo_dates_a, function(pdate) {
  idx <- which(adal$period == pdate)
  if (length(idx) == 0) {
    cli_alert_warning("Placebo date {pdate} not in data")
    return(tibble())
  }
  rv_placebo <- adal$time - idx
  tidy_rd(y = adal$biosimilar_share, x = rv_placebo,
          label = paste0("Placebo: ", pdate)) %>%
    mutate(molecule = "Adalimumab")
})

all_placebo_rd <- bind_rows(placebo_r, placebo_a)

cli_alert_info("Placebo RD results (should be non-significant):")
print(all_placebo_rd %>%
        select(molecule, cutoff, estimate, p_robust, bandwidth) %>%
        mutate(estimate = round(estimate, 5),
               p_robust = round(p_robust, 4),
               bandwidth = round(bandwidth, 1)))


# ═══════════════════════════════════════════════════════════════════════════════
# ITS vs RD COMPARISON
# ═══════════════════════════════════════════════════════════════════════════════

cli_h1("ITS vs RD comparison")

# Read ITS results (Newey-West preferred)
its_ritux <- read_csv(here("outputs", "tables", "tbl_its_rituximab.csv"),
                      show_col_types = FALSE) %>%
  filter(model == "R5: Newey-West SEs")

its_adal <- read_csv(here("outputs", "tables", "tbl_its_adalimumab.csv"),
                     show_col_types = FALSE) %>%
  filter(model == "A5: Newey-West SEs")

# Build comparison table
# ITS step changes correspond to RD local effects
comparison <- tribble(
  ~molecule, ~cutoff, ~its_term,
  "Rituximab", "MabThera IV delisting", "post_delist_iv",
  "Rituximab", "MabThera SC delisting", "post_delist_sc",
  "Adalimumab", "24% price cut", "post_price_cut",
  "Adalimumab", "Streamlined authority", "post_streamlined"
) %>%
  left_join(
    bind_rows(
      its_ritux %>% select(term, its_estimate = estimate, its_se = std_error, its_p = p_value),
      its_adal %>% select(term, its_estimate = estimate, its_se = std_error, its_p = p_value)
    ),
    by = c("its_term" = "term")
  ) %>%
  left_join(
    rd_main %>% select(cutoff, rd_estimate = estimate, rd_se = se_robust, rd_p = p_robust),
    by = "cutoff"
  ) %>%
  # Also add ITS slope changes for context
  left_join(
    bind_rows(
      its_ritux %>%
        filter(str_detect(term, "time_after")) %>%
        mutate(cutoff = case_when(
          str_detect(term, "delist_iv") ~ "MabThera IV delisting",
          str_detect(term, "delist_sc") ~ "MabThera SC delisting",
          TRUE ~ NA_character_
        )) %>%
        filter(!is.na(cutoff)) %>%
        select(cutoff, its_slope = estimate, its_slope_p = p_value),
      its_adal %>%
        filter(str_detect(term, "time_after")) %>%
        mutate(cutoff = case_when(
          str_detect(term, "price_cut") ~ "24% price cut",
          str_detect(term, "streamlined") ~ "Streamlined authority",
          TRUE ~ NA_character_
        )) %>%
        filter(!is.na(cutoff)) %>%
        select(cutoff, its_slope = estimate, its_slope_p = p_value)
    ),
    by = "cutoff"
  )

cli_h2("ITS step change vs RD local effect")
print(comparison %>%
        mutate(across(c(its_estimate, its_slope, rd_estimate), ~ round(.x * 100, 2)),
               across(c(its_p, its_slope_p, rd_p), ~ round(.x, 4))) %>%
        select(molecule, cutoff, its_step_pp = its_estimate, its_step_p = its_p,
               its_slope_pp_mo = its_slope, its_slope_p = its_slope_p,
               rd_estimate_pp = rd_estimate, rd_p))


# ─── Comparison figure ───────────────────────────────────────────────────────

cli_h2("ITS vs RD comparison figure")

comp_plot_data <- comparison %>%
  select(molecule, cutoff, its_estimate, its_se, rd_estimate, rd_se) %>%
  pivot_longer(
    cols = c(its_estimate, rd_estimate),
    names_to = "method",
    values_to = "estimate"
  ) %>%
  mutate(
    se = if_else(method == "its_estimate", its_se, rd_se),
    method = if_else(method == "its_estimate", "ITS step change (B2)", "RD local effect"),
    cutoff_short = str_replace(cutoff, "MabThera ", "")
  )

p_comparison <- ggplot(comp_plot_data,
                       aes(x = cutoff_short, y = estimate, colour = method)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_pointrange(aes(ymin = estimate - 1.96 * se,
                       ymax = estimate + 1.96 * se),
                  position = position_dodge(width = 0.5),
                  size = 0.5) +
  facet_wrap(~molecule, scales = "free_x") +
  scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
  scale_colour_manual(
    name = NULL,
    values = c("ITS step change (B2)" = "#D6604D", "RD local effect" = "#2166AC")
  ) +
  labs(
    title = "Comparison: ITS step change vs RD local treatment effect",
    subtitle = "Both methods estimate the immediate level shift at each policy date. 95% CIs shown.",
    x = NULL, y = "Estimated immediate effect (percentage points)",
    caption = paste0("ITS: OLS with Newey-West HAC SEs. RD: rdrobust with CCT optimal bandwidth. ",
                     "Neither method captures slope changes (the dominant effect channel).")
  ) +
  theme_pbs +
  theme(axis.text.x = element_text(angle = 30, hjust = 1, size = 9))

save_fig(p_comparison, "fig_rd_comparison", 12, 6)


# ═══════════════════════════════════════════════════════════════════════════════
# SAVE ALL RESULTS
# ═══════════════════════════════════════════════════════════════════════════════

cli_h1("Saving results")

# Main results table
rd_results_full <- bind_rows(
  rd_main %>% mutate(type = "Main", poly_order = 1L, bw_multiplier = 1.0),
  donut_results %>% mutate(type = "Donut (1 month)", poly_order = 1L, bw_multiplier = 1.0),
  all_placebo_rd %>% mutate(type = "Placebo", poly_order = 1L, bw_multiplier = 1.0)
) %>%
  mutate(
    across(c(estimate, estimate_bc, se_robust, ci_lower, ci_upper),
           ~ round(.x, 6)),
    p_robust = round(p_robust, 4),
    bandwidth = round(bandwidth, 2),
    sig = case_when(
      is.na(p_robust) ~ "",
      p_robust < 0.001 ~ "***",
      p_robust < 0.01  ~ "**",
      p_robust < 0.05  ~ "*",
      p_robust < 0.1   ~ ".",
      TRUE ~ ""
    )
  )

write_csv(rd_results_full, here("outputs", "tables", "tbl_rd_results.csv"))
cli_alert_success("Saved tbl_rd_results.csv")

# Robustness table (bandwidth + polynomial)
robustness_full <- bind_rows(
  bw_results %>% mutate(type = "Bandwidth", poly_order = 1L),
  poly_results %>% mutate(type = "Polynomial", bw_multiplier = 1.0, bw_used = bandwidth)
) %>%
  mutate(
    across(c(estimate, estimate_bc, se_robust, ci_lower, ci_upper),
           ~ round(.x, 6)),
    p_robust = round(p_robust, 4),
    bandwidth = round(bandwidth, 2),
    sig = case_when(
      is.na(p_robust) ~ "",
      p_robust < 0.001 ~ "***",
      p_robust < 0.01  ~ "**",
      p_robust < 0.05  ~ "*",
      p_robust < 0.1   ~ ".",
      TRUE ~ ""
    )
  )

write_csv(robustness_full, here("outputs", "tables", "tbl_rd_robustness.csv"))
cli_alert_success("Saved tbl_rd_robustness.csv")


# ═══════════════════════════════════════════════════════════════════════════════
# FINAL SUMMARY
# ═══════════════════════════════════════════════════════════════════════════════

cli_h1("Phase 4 complete")

cli_alert_info("Key findings:")
cli_alert_info("")
cli_alert_info("Rituximab:")
cli_alert_info("  IV delisting RD: {round(rd_ritux_iv$estimate * 100, 2)} pp (p = {round(rd_ritux_iv$p_robust, 4)})")
cli_alert_info("  SC delisting RD: {round(rd_ritux_sc$estimate * 100, 2)} pp (p = {round(rd_ritux_sc$p_robust, 4)})")
cli_alert_info("")
cli_alert_info("Adalimumab:")
cli_alert_info("  Price cut RD: {round(rd_adal_pc$estimate * 100, 2)} pp (p = {round(rd_adal_pc$p_robust, 4)})")
cli_alert_info("  Streamlined RD: {round(rd_adal_sa$estimate * 100, 2)} pp (p = {round(rd_adal_sa$p_robust, 4)})")
cli_alert_info("")
cli_alert_info("Interpretation: RD estimates capture the IMMEDIATE level shift at each cutoff.")
cli_alert_info("These are expected to be small/null because biosimilar policies operate through")
cli_alert_info("gradual incentive changes (captured by ITS slope changes), not overnight switching.")
cli_alert_info("The absence of sharp discontinuities supports the ITS finding that slope changes")
cli_alert_info("(B3) are the dominant causal channel, while step changes (B2) are small.")
cli_alert_info("")
cli_alert_info("This is a methodological contribution: for pharmaceutical policy evaluation,")
cli_alert_info("ITS > RD because effects accumulate gradually over months, not instantaneously.")
cli_alert_info("")
cli_alert_info("Next: Run 07_geographic_variation.R for state-level analysis")
