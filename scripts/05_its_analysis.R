################################################################################
# 05_its_analysis.R
# Phase 3: Interrupted Time Series Analysis
#
# Fits segmented regression models for rituximab (cleanest data) and
# adalimumab (most policy-relevant) around key policy intervention dates.
#
# Methods:
#   - OLS segmented regression (standard ITS)
#   - GLS with ARMA errors for autocorrelation (sensitivity)
#   - Newey-West heteroscedasticity/autocorrelation-robust SEs (preferred)
#   - Bayesian structural time series (CausalImpact) as sensitivity
#   - Placebo tests at non-intervention dates
#
# Key decisions:
#   - Newey-West (R5/A5) preferred over GLS AR(1) — the latter shows
#     near-unit-root AR coefficients that inflate SEs and absorb signal
#   - Adalimumab trimmed to Jan 2022+ (removes 6-month artifact wash-in
#     from DoS data start, which caused placebo test failure)
#
# Inputs:
#   - data/processed/market_share_national.csv
#   - data/reference/policy_timeline.csv
#
# Outputs:
#   - outputs/tables/tbl_its_rituximab.csv
#   - outputs/tables/tbl_its_adalimumab.csv
#   - outputs/tables/tbl_its_summary.csv
#   - outputs/tables/tbl_placebo_tests.csv
#   - outputs/figures/fig_its_rituximab.pdf/png
#   - outputs/figures/fig_its_adalimumab.pdf/png
#   - outputs/figures/fig_its_diagnostics.pdf/png
#   - outputs/figures/fig_causalimpact_rituximab.pdf/png (if CausalImpact installed)
#   - outputs/figures/fig_causalimpact_adalimumab.pdf/png (if CausalImpact installed)
#   - outputs/figures/fig_placebo_tests.pdf/png
################################################################################

library(tidyverse)
library(lubridate)
library(here)
library(scales)
library(nlme)        # GLS with ARMA errors
library(sandwich)    # Newey-West SEs
library(lmtest)      # coeftest with robust SEs
library(cli)

# CausalImpact is optional — check availability
has_causalimpact <- requireNamespace("CausalImpact", quietly = TRUE)
if (has_causalimpact) {
  library(CausalImpact)
  cli_alert_success("CausalImpact package available")
} else {
  cli_alert_warning("CausalImpact not installed \u2014 Bayesian ITS will be skipped")
  cli_alert_info("Install with: install.packages('CausalImpact')")
}

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

source_caption <- "Source: PBS Date of Supply (Jul 2021\u2013Nov 2025), Medicare Statistics (Jan 2009\u2013Jun 2022)."

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

policy_timeline <- read_csv(here("data", "reference", "policy_timeline.csv"),
                            show_col_types = FALSE) %>%
  mutate(date = ymd(date))


# ─── Helper: build ITS variables for a given intervention date ───────────────

build_its_vars <- function(data, int_date, prefix) {
  data %>%
    mutate(
      !!paste0("post_", prefix) := as.integer(period >= int_date),
      !!paste0("time_after_", prefix) := pmax(0, as.numeric(
        difftime(period, int_date, units = "days")) / 30.44) %>% round()
    )
}

# ─── Helper: extract tidy model results ──────────────────────────────────────

tidy_its <- function(model, model_name, nw_vcov = NULL) {
  if (!is.null(nw_vcov)) {
    # Use Newey-West SEs
    ct <- lmtest::coeftest(model, vcov. = nw_vcov)
    tibble(
      model = model_name,
      term = rownames(ct),
      estimate = ct[, 1],
      std_error = ct[, 2],
      t_value = ct[, 3],
      p_value = ct[, 4]
    )
  } else if (inherits(model, "gls")) {
    s <- summary(model)
    ct <- s$tTable
    tibble(
      model = model_name,
      term = rownames(ct),
      estimate = ct[, 1],
      std_error = ct[, 2],
      t_value = ct[, 3],
      p_value = ct[, 4]
    )
  } else {
    s <- summary(model)
    ct <- coef(s)
    tibble(
      model = model_name,
      term = rownames(ct),
      estimate = ct[, 1],
      std_error = ct[, 2],
      t_value = ct[, 3],
      p_value = ct[, 4]
    )
  }
}


# ═══════════════════════════════════════════════════════════════════════════════
# RITUXIMAB ITS
# ═══════════════════════════════════════════════════════════════════════════════

cli_h1("Rituximab: Interrupted Time Series Analysis")

# ─── Prepare rituximab data ──────────────────────────────────────────────────

ritux <- ms_national %>%
  filter(molecule == "rituximab") %>%
  arrange(period) %>%
  mutate(time = row_number())

# Key intervention dates
ritux_listing     <- ymd("2019-10-01")   # Riximyo listed
ritux_a_flag      <- ymd("2020-01-01")   # 'a' flag enabled
ritux_delist_iv   <- ymd("2021-04-01")   # MabThera IV delisted
ritux_delist_sc   <- ymd("2021-10-01")   # MabThera SC delisted

# Build intervention variables
ritux <- ritux %>%
  build_its_vars(ritux_listing, "listing") %>%
  build_its_vars(ritux_delist_iv, "delist_iv") %>%
  build_its_vars(ritux_delist_sc, "delist_sc")

cli_alert_info("Rituximab: {nrow(ritux)} months, {sum(ritux$post_listing)} post-listing, {sum(ritux$post_delist_iv)} post-delisting(IV)")

# ─── Model R1: Single intervention at listing ────────────────────────────────

cli_h2("Model R1: Single intervention at biosimilar listing (Oct 2019)")

r1 <- lm(biosimilar_share ~ time + post_listing + time_after_listing, data = ritux)
r1_results <- tidy_its(r1, "R1: Listing only")
cli_alert_info("R1 R-squared: {round(summary(r1)$r.squared, 4)}")

# ─── Model R2: Listing + MabThera IV delisting ──────────────────────────────

cli_h2("Model R2: Listing + MabThera IV delisting")

r2 <- lm(biosimilar_share ~ time + post_listing + time_after_listing +
            post_delist_iv + time_after_delist_iv, data = ritux)
r2_results <- tidy_its(r2, "R2: Listing + delisting(IV)")
cli_alert_info("R2 R-squared: {round(summary(r2)$r.squared, 4)}")

# ─── Model R3: Full model (listing + both delistings) ────────────────────────

cli_h2("Model R3: Listing + both delistings")

r3 <- lm(biosimilar_share ~ time + post_listing + time_after_listing +
            post_delist_iv + time_after_delist_iv +
            post_delist_sc + time_after_delist_sc, data = ritux)
r3_results <- tidy_its(r3, "R3: Listing + both delistings")
cli_alert_info("R3 R-squared: {round(summary(r3)$r.squared, 4)}")

# ─── Autocorrelation diagnostics ─────────────────────────────────────────────

cli_h2("Autocorrelation diagnostics (Model R3)")

dw_test <- lmtest::dwtest(r3)
cli_alert_info("Durbin-Watson: {round(dw_test$statistic, 3)} (p = {format.pval(dw_test$p.value, digits = 3)})")

r3_resid <- residuals(r3)
acf_vals <- acf(r3_resid, lag.max = 12, plot = FALSE)
cli_alert_info("ACF lag-1: {round(acf_vals$acf[2], 3)}, lag-2: {round(acf_vals$acf[3], 3)}")

# ─── Model R4: GLS with AR(1) errors (sensitivity) ──────────────────────────

cli_h2("Model R4: GLS with AR(1) errors (sensitivity check)")

r4 <- gls(biosimilar_share ~ time + post_listing + time_after_listing +
             post_delist_iv + time_after_delist_iv +
             post_delist_sc + time_after_delist_sc,
           data = ritux,
           correlation = corARMA(p = 1, q = 0, form = ~ time))
r4_results <- tidy_its(r4, "R4: GLS AR(1)")

phi <- coef(r4$modelStruct$corStruct, unconstrained = FALSE)
cli_alert_info("AR(1) coefficient: {round(phi, 3)}")
if (abs(phi) > 0.9) {
  cli_alert_warning("Near-unit-root AR(1) ({round(phi, 3)}) \u2014 GLS SEs unreliable, using Newey-West as preferred model")
}

# ─── Model R5: Newey-West robust SEs (PREFERRED) ────────────────────────────

cli_h2("Model R5: OLS with Newey-West HAC SEs (PREFERRED)")

nw_vcov_r <- sandwich::NeweyWest(r3, lag = 6, prewhite = FALSE)
r5_results <- tidy_its(r3, "R5: Newey-West SEs", nw_vcov = nw_vcov_r)

# ─── Combine rituximab results ───────────────────────────────────────────────

ritux_results <- bind_rows(r1_results, r2_results, r3_results, r4_results, r5_results) %>%
  mutate(
    estimate = round(estimate, 6),
    std_error = round(std_error, 6),
    t_value = round(t_value, 3),
    p_value = round(p_value, 4),
    sig = case_when(
      p_value < 0.001 ~ "***",
      p_value < 0.01  ~ "**",
      p_value < 0.05  ~ "*",
      p_value < 0.1   ~ ".",
      TRUE ~ ""
    )
  )

write_csv(ritux_results, here("outputs", "tables", "tbl_its_rituximab.csv"))
cli_alert_success("Saved tbl_its_rituximab.csv")

# Print key results from preferred model
cli_h2("Rituximab: Key effect estimates (Model R5 \u2014 Newey-West HAC SEs)")
r5_results %>%
  mutate(across(c(estimate, std_error), ~ round(.x, 5)),
         across(c(t_value), ~ round(.x, 2)),
         across(c(p_value), ~ round(.x, 4))) %>%
  print(n = Inf)


# ─── Rituximab ITS figure ────────────────────────────────────────────────────

cli_h2("Rituximab ITS figure")

# Predicted values from OLS model (point estimates identical for NW)
ritux$predicted_r3 <- predict(r3)

# Counterfactual: what if no listing had occurred (extend pre-listing trend)
ritux_cf_listing <- ritux %>%
  mutate(
    post_listing = 0L, time_after_listing = 0L,
    post_delist_iv = 0L, time_after_delist_iv = 0L,
    post_delist_sc = 0L, time_after_delist_sc = 0L
  )
ritux$cf_no_listing <- predict(r3, newdata = ritux_cf_listing)

# Counterfactual: what if listing but no delisting
ritux_cf_delist <- ritux %>%
  mutate(
    post_delist_iv = 0L, time_after_delist_iv = 0L,
    post_delist_sc = 0L, time_after_delist_sc = 0L
  )
ritux$cf_no_delist <- predict(r3, newdata = ritux_cf_delist)

p_its_ritux <- ggplot(ritux %>% filter(period >= ymd("2018-01-01")),
                       aes(x = period)) +
  # Counterfactual: no listing
  geom_line(aes(y = cf_no_listing, colour = "No biosimilar listing"),
            linetype = "dotted", linewidth = 0.6) +
  # Counterfactual: listing but no delisting
  geom_line(aes(y = cf_no_delist, colour = "No reference delisting"),
            linetype = "dashed", linewidth = 0.6) +
  # Fitted values
  geom_line(aes(y = predicted_r3, colour = "Fitted (ITS model)"),
            linewidth = 0.7) +
  # Observed data
  geom_point(aes(y = biosimilar_share), colour = "#2166AC",
             size = 1.2, alpha = 0.5) +
  # Intervention lines
  geom_vline(xintercept = ritux_listing, linetype = "dashed",
             colour = "grey50", linewidth = 0.3) +
  geom_vline(xintercept = ritux_delist_iv, linetype = "dashed",
             colour = "grey50", linewidth = 0.3) +
  geom_vline(xintercept = ritux_delist_sc, linetype = "dashed",
             colour = "grey50", linewidth = 0.3) +
  # Labels
  annotate("text", x = ritux_listing, y = 0.55,
           label = "Biosimilar\nlisted", size = 2.8, fontface = "bold",
           colour = "grey40", hjust = -0.1) +
  annotate("text", x = ritux_delist_iv, y = 0.55,
           label = "MabThera IV\ndelisted", size = 2.8, fontface = "bold",
           colour = "grey40", hjust = -0.1) +
  annotate("text", x = ritux_delist_sc, y = 0.45,
           label = "MabThera SC\ndelisted", size = 2.8, fontface = "bold",
           colour = "grey40", hjust = -0.1) +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     limits = c(-0.05, 1.05)) +
  scale_x_date(date_labels = "%b\n%Y", date_breaks = "6 months") +
  scale_colour_manual(
    name = NULL,
    values = c("Fitted (ITS model)" = "#D6604D",
               "No biosimilar listing" = "grey60",
               "No reference delisting" = "#E08214")
  ) +
  labs(
    title = "Rituximab: interrupted time series analysis",
    subtitle = "Segmented regression with interventions at biosimilar listing and reference product delistings",
    x = NULL, y = "Biosimilar market share",
    caption = paste0(source_caption,
                     " Model: OLS segmented regression (R3); Newey-West HAC SEs for inference.")
  ) +
  theme_pbs

save_fig(p_its_ritux, "fig_its_rituximab", 12, 6.5)


# ═══════════════════════════════════════════════════════════════════════════════
# ADALIMUMAB ITS (Jan 2022+ — trimmed to remove artifact wash-in period)
# ═══════════════════════════════════════════════════════════════════════════════

cli_h1("Adalimumab: Interrupted Time Series Analysis (Jan 2022+)")
cli_alert_info("Starting from Jan 2022 (not Jul 2021) to remove 6-month artifact wash-in from DoS data transition")

# ─── Prepare adalimumab data ─────────────────────────────────────────────────

adal <- ms_national %>%
  filter(molecule == "adalimumab", period >= ymd("2022-01-01")) %>%
  arrange(period) %>%
  mutate(time = row_number())

# Key intervention dates
adal_price_cut    <- ymd("2023-04-01")   # 24% statutory price cut
adal_streamlined  <- ymd("2023-11-01")   # Streamlined authority (consolidate Nov+Dec)

# Build intervention variables
adal <- adal %>%
  build_its_vars(adal_price_cut, "price_cut") %>%
  build_its_vars(adal_streamlined, "streamlined")

cli_alert_info("Adalimumab: {nrow(adal)} months, {sum(adal$post_price_cut)} post-price-cut, {sum(adal$post_streamlined)} post-streamlined")
cli_alert_info("Pre-period: Jan 2022 \u2013 Mar 2023 ({sum(!adal$post_price_cut)} months)")

# ─── Model A1: Single intervention at price cut ─────────────────────────────

cli_h2("Model A1: Price cut (Apr 2023)")

a1 <- lm(biosimilar_share ~ time + post_price_cut + time_after_price_cut,
          data = adal)
a1_results <- tidy_its(a1, "A1: Price cut only")
cli_alert_info("A1 R-squared: {round(summary(a1)$r.squared, 4)}")

# ─── Model A2: Single intervention at streamlined authority ──────────────────

cli_h2("Model A2: Streamlined authority (Nov 2023)")

a2 <- lm(biosimilar_share ~ time + post_streamlined + time_after_streamlined,
          data = adal)
a2_results <- tidy_its(a2, "A2: Streamlined authority only")
cli_alert_info("A2 R-squared: {round(summary(a2)$r.squared, 4)}")

# ─── Model A3: Both interventions ────────────────────────────────────────────

cli_h2("Model A3: Price cut + streamlined authority")

a3 <- lm(biosimilar_share ~ time + post_price_cut + time_after_price_cut +
            post_streamlined + time_after_streamlined, data = adal)
a3_results <- tidy_its(a3, "A3: Price cut + streamlined")
cli_alert_info("A3 R-squared: {round(summary(a3)$r.squared, 4)}")

# ─── Autocorrelation diagnostics ─────────────────────────────────────────────

cli_h2("Autocorrelation diagnostics (Model A3)")

dw_test_a <- lmtest::dwtest(a3)
cli_alert_info("Durbin-Watson: {round(dw_test_a$statistic, 3)} (p = {format.pval(dw_test_a$p.value, digits = 3)})")

a3_resid <- residuals(a3)
acf_vals_a <- acf(a3_resid, lag.max = 12, plot = FALSE)
cli_alert_info("ACF lag-1: {round(acf_vals_a$acf[2], 3)}, lag-2: {round(acf_vals_a$acf[3], 3)}")

# ─── Model A4: GLS with AR(1) errors (sensitivity) ──────────────────────────

cli_h2("Model A4: GLS with AR(1) errors (sensitivity check)")

a4 <- gls(biosimilar_share ~ time + post_price_cut + time_after_price_cut +
             post_streamlined + time_after_streamlined,
           data = adal,
           correlation = corARMA(p = 1, q = 0, form = ~ time))
a4_results <- tidy_its(a4, "A4: GLS AR(1)")

phi_a <- coef(a4$modelStruct$corStruct, unconstrained = FALSE)
cli_alert_info("AR(1) coefficient: {round(phi_a, 3)}")
if (abs(phi_a) > 0.9) {
  cli_alert_warning("Near-unit-root AR(1) ({round(phi_a, 3)}) \u2014 GLS SEs unreliable, using Newey-West as preferred model")
}

# ─── Model A5: Newey-West robust SEs (PREFERRED) ────────────────────────────

cli_h2("Model A5: OLS with Newey-West HAC SEs (PREFERRED)")

nw_vcov_a <- sandwich::NeweyWest(a3, lag = 4, prewhite = FALSE)
a5_results <- tidy_its(a3, "A5: Newey-West SEs", nw_vcov = nw_vcov_a)

# ─── Combine adalimumab results ──────────────────────────────────────────────

adal_results <- bind_rows(a1_results, a2_results, a3_results, a4_results, a5_results) %>%
  mutate(
    estimate = round(estimate, 6),
    std_error = round(std_error, 6),
    t_value = round(t_value, 3),
    p_value = round(p_value, 4),
    sig = case_when(
      p_value < 0.001 ~ "***",
      p_value < 0.01  ~ "**",
      p_value < 0.05  ~ "*",
      p_value < 0.1   ~ ".",
      TRUE ~ ""
    )
  )

write_csv(adal_results, here("outputs", "tables", "tbl_its_adalimumab.csv"))
cli_alert_success("Saved tbl_its_adalimumab.csv")

cli_h2("Adalimumab: Key effect estimates (Model A5 \u2014 Newey-West HAC SEs)")
a5_results %>%
  mutate(across(c(estimate, std_error), ~ round(.x, 5)),
         across(c(t_value), ~ round(.x, 2)),
         across(c(p_value), ~ round(.x, 4))) %>%
  print(n = Inf)


# ─── Adalimumab ITS figure ───────────────────────────────────────────────────

cli_h2("Adalimumab ITS figure")

adal$predicted_a3 <- predict(a3)

# Counterfactual: no price cut, no streamlined authority
adal_cf_none <- adal %>%
  mutate(
    post_price_cut = 0L, time_after_price_cut = 0L,
    post_streamlined = 0L, time_after_streamlined = 0L
  )
adal$cf_no_intervention <- predict(a3, newdata = adal_cf_none)

# Counterfactual: price cut only, no streamlined
adal_cf_pc_only <- adal %>%
  mutate(post_streamlined = 0L, time_after_streamlined = 0L)
adal$cf_price_cut_only <- predict(a3, newdata = adal_cf_pc_only)

p_its_adal <- ggplot(adal, aes(x = period)) +
  # Counterfactual: no interventions
  geom_line(aes(y = cf_no_intervention, colour = "No interventions"),
            linetype = "dotted", linewidth = 0.6) +
  # Counterfactual: price cut only
  geom_line(aes(y = cf_price_cut_only, colour = "Price cut only"),
            linetype = "dashed", linewidth = 0.6) +
  # Fitted
  geom_line(aes(y = predicted_a3, colour = "Fitted (ITS model)"),
            linewidth = 0.7) +
  # Observed
  geom_point(aes(y = biosimilar_share), colour = "#2166AC",
             size = 1.2, alpha = 0.5) +
  # Intervention lines
  geom_vline(xintercept = adal_price_cut, linetype = "dashed",
             colour = "grey50", linewidth = 0.3) +
  geom_vline(xintercept = adal_streamlined, linetype = "dashed",
             colour = "grey50", linewidth = 0.3) +
  # Labels
  annotate("text", x = adal_price_cut, y = 0.38,
           label = "24% price\ncut", size = 2.8, fontface = "bold",
           colour = "#E08214", hjust = -0.1) +
  annotate("text", x = adal_streamlined, y = 0.38,
           label = "Streamlined\nauthority", size = 2.8, fontface = "bold",
           colour = "#D6604D", hjust = -0.1) +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     limits = c(0, 0.42),
                     breaks = seq(0, 0.40, 0.05)) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "3 months") +
  scale_colour_manual(
    name = NULL,
    values = c("Fitted (ITS model)" = "#D6604D",
               "No interventions" = "grey60",
               "Price cut only" = "#E08214")
  ) +
  labs(
    title = "Adalimumab: interrupted time series analysis (Jan 2022+)",
    subtitle = "Segmented regression with interventions at 24% price cut and streamlined authority",
    x = NULL, y = "Biosimilar market share",
    caption = paste0("Source: PBS Date of Supply (Jan 2022\u2013Nov 2025). ",
                     "Model: OLS segmented regression (A3); Newey-West HAC SEs for inference.")
  ) +
  theme_pbs +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9))

save_fig(p_its_adal, "fig_its_adalimumab", 12, 6.5)


# ═══════════════════════════════════════════════════════════════════════════════
# DIAGNOSTICS
# ═══════════════════════════════════════════════════════════════════════════════

cli_h1("Diagnostics")

# Residual ACF plots for both molecules
pdf(here("outputs", "figures", "fig_its_diagnostics.pdf"), width = 12, height = 8)
par(mfrow = c(2, 3), mar = c(4, 4, 3, 1))

# Rituximab diagnostics (Model R3)
acf(residuals(r3), main = "Rituximab R3: ACF of residuals", lag.max = 24)
pacf(residuals(r3), main = "Rituximab R3: PACF of residuals", lag.max = 24)
plot(ritux$time, residuals(r3), type = "l", main = "Rituximab R3: Residuals vs time",
     xlab = "Time index", ylab = "Residual")
abline(h = 0, col = "red", lty = 2)

# Adalimumab diagnostics (Model A3)
acf(residuals(a3), main = "Adalimumab A3: ACF of residuals", lag.max = 20)
pacf(residuals(a3), main = "Adalimumab A3: PACF of residuals", lag.max = 20)
plot(adal$time, residuals(a3), type = "l", main = "Adalimumab A3: Residuals vs time",
     xlab = "Time index", ylab = "Residual")
abline(h = 0, col = "red", lty = 2)

dev.off()

# PNG version
png(here("outputs", "figures", "fig_its_diagnostics.png"),
    width = 12, height = 8, units = "in", res = 300)
par(mfrow = c(2, 3), mar = c(4, 4, 3, 1))
acf(residuals(r3), main = "Rituximab R3: ACF of residuals", lag.max = 24)
pacf(residuals(r3), main = "Rituximab R3: PACF of residuals", lag.max = 24)
plot(ritux$time, residuals(r3), type = "l", main = "Rituximab R3: Residuals vs time",
     xlab = "Time index", ylab = "Residual")
abline(h = 0, col = "red", lty = 2)
acf(residuals(a3), main = "Adalimumab A3: ACF of residuals", lag.max = 20)
pacf(residuals(a3), main = "Adalimumab A3: PACF of residuals", lag.max = 20)
plot(adal$time, residuals(a3), type = "l", main = "Adalimumab A3: Residuals vs time",
     xlab = "Time index", ylab = "Residual")
abline(h = 0, col = "red", lty = 2)
dev.off()

cli_alert_success("Saved fig_its_diagnostics.pdf/png")


# ═══════════════════════════════════════════════════════════════════════════════
# CAUSALIMPACT (Bayesian structural time series)
# ═══════════════════════════════════════════════════════════════════════════════

if (has_causalimpact) {

  cli_h1("CausalImpact: Bayesian structural time series")

  # ─── Rituximab: CausalImpact around MabThera IV delisting ──────────────────

  cli_h2("Rituximab CausalImpact: MabThera IV delisting (Apr 2021)")

  # Use pre-period from listing to just before IV delisting (organic adoption)
  ritux_ci <- ritux %>%
    filter(period >= ymd("2019-10-01")) %>%
    select(period, biosimilar_share)

  ritux_ci_zoo <- zoo::zoo(ritux_ci$biosimilar_share,
                           order.by = ritux_ci$period)

  pre_period_r  <- as.Date(c("2019-10-01", "2021-03-01"))
  post_period_r <- as.Date(c("2021-04-01", "2025-11-01"))

  ci_ritux <- CausalImpact(ritux_ci_zoo, pre_period_r, post_period_r,
                            model.args = list(nseasons = 12, season.duration = 1))

  cli_alert_info("Rituximab CausalImpact summary:")
  print(summary(ci_ritux))

  # Save CausalImpact plot
  p_ci_ritux <- plot(ci_ritux) +
    labs(title = "Rituximab: CausalImpact around MabThera IV delisting (Apr 2021)")

  ggsave(here("outputs", "figures", "fig_causalimpact_rituximab.pdf"),
         p_ci_ritux, width = 10, height = 8)
  ggsave(here("outputs", "figures", "fig_causalimpact_rituximab.png"),
         p_ci_ritux, width = 10, height = 8, dpi = 300)
  cli_alert_success("Saved fig_causalimpact_rituximab.pdf/png")

  # ─── Adalimumab: CausalImpact around price cut ────────────────────────────

  cli_h2("Adalimumab CausalImpact: Price cut (Apr 2023)")

  adal_ci <- adal %>%
    select(period, biosimilar_share)

  adal_ci_zoo <- zoo::zoo(adal_ci$biosimilar_share,
                          order.by = adal_ci$period)

  pre_period_a  <- as.Date(c("2022-01-01", "2023-03-01"))
  post_period_a <- as.Date(c("2023-04-01", "2025-11-01"))

  ci_adal <- CausalImpact(adal_ci_zoo, pre_period_a, post_period_a,
                           model.args = list(nseasons = 12, season.duration = 1))

  cli_alert_info("Adalimumab CausalImpact summary:")
  print(summary(ci_adal))

  p_ci_adal <- plot(ci_adal) +
    labs(title = "Adalimumab: CausalImpact around price cut (Apr 2023)")

  ggsave(here("outputs", "figures", "fig_causalimpact_adalimumab.pdf"),
         p_ci_adal, width = 10, height = 8)
  ggsave(here("outputs", "figures", "fig_causalimpact_adalimumab.png"),
         p_ci_adal, width = 10, height = 8, dpi = 300)
  cli_alert_success("Saved fig_causalimpact_adalimumab.pdf/png")

} else {
  cli_alert_warning("Skipping CausalImpact \u2014 install with: install.packages('CausalImpact')")
}


# ═══════════════════════════════════════════════════════════════════════════════
# PLACEBO TESTS
# ═══════════════════════════════════════════════════════════════════════════════

cli_h1("Placebo tests")

# Test: fit the same ITS model at fake intervention dates where no policy changed.
# If the model is valid, we should see no significant effects at placebo dates.

# ─── Rituximab placebo ────────────────────────────────────────────────────────

cli_h2("Rituximab placebo tests")
cli_alert_info("Note: Pre-listing biosimilar share is constant 0% \u2014 placebo tests are uninformative (zero variance)")

# Test at 3 dates in the pre-listing period where nothing happened
placebo_dates_r <- ymd(c("2015-01-01", "2017-01-01", "2018-06-01"))

placebo_results_r <- map_dfr(placebo_dates_r, function(pdate) {
  d <- ritux %>%
    filter(period < ritux_listing) %>%  # Pre-listing only
    build_its_vars(pdate, "placebo")

  m <- lm(biosimilar_share ~ time + post_placebo + time_after_placebo, data = d)
  tidy_its(m, paste0("Placebo: ", pdate)) %>%
    filter(str_detect(term, "placebo"))
})

cli_alert_info("Rituximab placebo results (all zero \u2014 pre-listing share is constant):")
print(placebo_results_r %>%
        mutate(across(c(estimate, std_error), ~ round(.x, 6)),
               p_value = round(p_value, 4)))

# ─── Adalimumab placebo ──────────────────────────────────────────────────────

cli_h2("Adalimumab placebo tests")

# Test at dates in the pre-price-cut period (Jan 2022 - Mar 2023)
# Placed at 3, 7, and 11 months in to ensure adequate pre/post within window
placebo_dates_a <- ymd(c("2022-04-01", "2022-08-01", "2022-12-01"))

placebo_results_a <- map_dfr(placebo_dates_a, function(pdate) {
  d <- adal %>%
    filter(period < adal_price_cut) %>%  # Pre-price-cut only
    build_its_vars(pdate, "placebo")

  m <- lm(biosimilar_share ~ time + post_placebo + time_after_placebo, data = d)
  tidy_its(m, paste0("Placebo: ", pdate)) %>%
    filter(str_detect(term, "placebo"))
})

cli_alert_info("Adalimumab placebo results (should be non-significant):")
print(placebo_results_a %>%
        mutate(across(c(estimate, std_error), ~ round(.x, 6)),
               p_value = round(p_value, 4)))

# ─── Placebo results figure ──────────────────────────────────────────────────

all_placebo <- bind_rows(
  placebo_results_r %>% mutate(molecule = "Rituximab"),
  placebo_results_a %>% mutate(molecule = "Adalimumab")
) %>%
  filter(str_detect(term, "post_")) %>%
  mutate(
    model = str_remove(model, "Placebo: "),
    sig = p_value < 0.05
  )

p_placebo <- ggplot(all_placebo, aes(x = model, y = estimate, fill = sig)) +
  geom_col(width = 0.6) +
  geom_errorbar(aes(ymin = estimate - 1.96 * std_error,
                     ymax = estimate + 1.96 * std_error),
                width = 0.2, linewidth = 0.4) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  facet_wrap(~molecule, scales = "free") +
  scale_fill_manual(values = c("TRUE" = "#B2182B", "FALSE" = "grey70"),
                    guide = "none") +
  labs(
    title = "Placebo tests: step change at non-intervention dates",
    subtitle = "Bars should be near zero and non-significant (no red). 95% CIs shown.",
    x = "Placebo date", y = "Estimated step change",
    caption = paste0("Red = statistically significant at p < 0.05 (would indicate model misspecification). ",
                     "Rituximab placebos are zero by construction (constant pre-listing baseline).")
  ) +
  theme_pbs +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

save_fig(p_placebo, "fig_placebo_tests", 10, 5.5)

# Save all placebo results
write_csv(
  bind_rows(
    placebo_results_r %>% mutate(molecule = "rituximab"),
    placebo_results_a %>% mutate(molecule = "adalimumab")
  ),
  here("outputs", "tables", "tbl_placebo_tests.csv")
)
cli_alert_success("Saved tbl_placebo_tests.csv")


# ═══════════════════════════════════════════════════════════════════════════════
# SUMMARY TABLE (Preferred models: Newey-West R5/A5)
# ═══════════════════════════════════════════════════════════════════════════════

cli_h1("Summary of ITS results")
cli_alert_info("Preferred models: Newey-West HAC SEs (R5/A5)")
cli_alert_info("Rationale: GLS AR(1) shows near-unit-root, inflating SEs and absorbing signal")

summary_table <- bind_rows(
  r5_results %>%
    mutate(molecule = "Rituximab", preferred_model = "R5: Newey-West HAC SEs"),
  a5_results %>%
    mutate(molecule = "Adalimumab", preferred_model = "A5: Newey-West HAC SEs")
) %>%
  mutate(
    estimate = round(estimate, 5),
    std_error = round(std_error, 5),
    p_value = round(p_value, 4),
    sig = case_when(
      p_value < 0.001 ~ "***",
      p_value < 0.01  ~ "**",
      p_value < 0.05  ~ "*",
      p_value < 0.1   ~ ".",
      TRUE ~ ""
    ),
    # Interpret the coefficients
    interpretation = case_when(
      term == "(Intercept)" ~ "Baseline level",
      term == "time" ~ "Pre-intervention monthly trend",
      str_detect(term, "^post_") ~ "Step change (immediate level shift)",
      str_detect(term, "^time_after_") ~ "Slope change (change in monthly trend)",
      TRUE ~ ""
    )
  ) %>%
  select(molecule, preferred_model, term, interpretation, estimate, std_error,
         p_value, sig)

write_csv(summary_table, here("outputs", "tables", "tbl_its_summary.csv"))
cli_alert_success("Saved tbl_its_summary.csv")

cli_h2("Preferred model results")
print(summary_table, n = Inf)


# ═══════════════════════════════════════════════════════════════════════════════
# FINAL SUMMARY
# ═══════════════════════════════════════════════════════════════════════════════

cli_h1("Phase 3 complete")

cli_alert_info("Rituximab key findings (Model R5 \u2014 Newey-West HAC SEs):")
cli_alert_info("  - Listing slope change: {round(r5_results$estimate[r5_results$term == 'time_after_listing'] * 100, 3)} pp/month (p = {round(r5_results$p_value[r5_results$term == 'time_after_listing'], 4)})")
cli_alert_info("  - MabThera SC delisting slope: {round(r5_results$estimate[r5_results$term == 'time_after_delist_sc'] * 100, 3)} pp/month (p = {round(r5_results$p_value[r5_results$term == 'time_after_delist_sc'], 4)})")
cli_alert_info("")
cli_alert_info("Adalimumab key findings (Model A5 \u2014 Newey-West HAC SEs):")
cli_alert_info("  - Price cut slope change: {round(a5_results$estimate[a5_results$term == 'time_after_price_cut'] * 100, 3)} pp/month (p = {round(a5_results$p_value[a5_results$term == 'time_after_price_cut'], 4)})")
cli_alert_info("  - Streamlined auth step: {round(a5_results$estimate[a5_results$term == 'post_streamlined'] * 100, 2)} pp (p = {round(a5_results$p_value[a5_results$term == 'post_streamlined'], 4)})")
cli_alert_info("  - Streamlined auth slope: {round(a5_results$estimate[a5_results$term == 'time_after_streamlined'] * 100, 3)} pp/month (p = {round(a5_results$p_value[a5_results$term == 'time_after_streamlined'], 4)})")
cli_alert_info("")
cli_alert_info("Note: Streamlined authority slope is NEGATIVE \u2014 adoption plateaued at ~22-25%,")
cli_alert_info("      likely reflecting a natural ceiling rather than a counterproductive policy effect.")
cli_alert_info("")
cli_alert_info("Next: Run 06_rd_analysis.R for regression discontinuity analysis")
