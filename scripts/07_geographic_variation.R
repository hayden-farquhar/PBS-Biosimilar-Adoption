################################################################################
# 07_geographic_variation.R
# Phase 5: Geographic Variation Analysis
#
# Analyzes state/territory-level variation in biosimilar adoption
# using Medicare Statistics data (Jan 2009 – Jun 2022).
#
# Sections:
#   1. State-level adoption snapshots and variation metrics
#   2. Time-varying geographic variation (convergence/divergence)
#   3. Funnel plots (binomial control limits)
#   4. Multilevel ITS model for rituximab (state random effects)
#   5. State characteristic correlates (ABS/AIHW published data)
#   6. Summary
#
# Data limitation: State-level data comes from Medicare Statistics only,
# ending Jun 2022. DoS data (Jul 2021+) is national only.
# Item-sharing affects absolute levels but relative state rankings
# are informative for between-state variation analysis.
#
# Inputs:
#   - data/processed/market_share_state.csv
#   - data/processed/market_share_national.csv
#
# Outputs:
#   - outputs/tables/tbl_state_variation.csv
#   - outputs/tables/tbl_state_correlates.csv
#   - outputs/tables/tbl_multilevel_rituximab.csv
#   - outputs/figures/fig_funnel_adalimumab.pdf/png
#   - outputs/figures/fig_funnel_rituximab.pdf/png
#   - outputs/figures/fig_state_variation_time.pdf/png
#   - outputs/figures/fig_state_correlates.pdf/png
#   - outputs/figures/fig_state_map.pdf/png (if sf/ozmaps available)
################################################################################

library(tidyverse)
library(lubridate)
library(here)
library(scales)
library(lme4)       # Multilevel models
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

state_colours <- c(
  NSW = "#E41A1C", VIC = "#377EB8", QLD = "#FF7F00", WA = "#4DAF4A",
  SA = "#984EA3", TAS = "#A65628", ACT = "#F781BF", NT = "#999999"
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

ms_state <- read_csv(here("data", "processed", "market_share_state.csv"),
                     show_col_types = FALSE) %>%
  mutate(period = ymd(period))

ms_national <- read_csv(here("data", "processed", "market_share_national.csv"),
                        show_col_types = FALSE) %>%
  mutate(period = ymd(period))

cli_alert_info("State data: {nrow(ms_state)} rows, {n_distinct(ms_state$state)} states, {n_distinct(ms_state$molecule)} molecules")
cli_alert_info("Date range: {min(ms_state$period)} to {max(ms_state$period)}")


# ─── State characteristics (ABS/AIHW published data) ─────────────────────────

state_chars <- tribble(
  ~state, ~state_name, ~population_2021, ~pct_65plus, ~pct_remote, ~seifa_irsd, ~specialist_per_100k,
  "NSW",  "New South Wales",      8072163, 17.3,  5.3, 1001, 125,
  "VIC",  "Victoria",             6503491, 16.5,  3.0, 1010, 130,
  "QLD",  "Queensland",           5156138, 16.5, 11.3,  993, 105,
  "WA",   "Western Australia",    2660026, 14.8,  9.0, 1019, 115,
  "SA",   "South Australia",      1781516, 19.0,  8.7,  978, 120,
  "TAS",  "Tasmania",              557571, 21.2, 16.8,  953,  90,
  "ACT",  "Australian Capital T.", 431826, 13.4,  0.0, 1079, 160,
  "NT",   "Northern Territory",    232605,  7.6, 46.1,  912,  70
)

cli_alert_info("State characteristics loaded (ABS Census 2021, AIHW Medical Workforce)")


# ═══════════════════════════════════════════════════════════════════════════════
# SECTION 1: STATE-LEVEL SNAPSHOTS AND VARIATION METRICS
# ═══════════════════════════════════════════════════════════════════════════════

cli_h1("Section 1: State-level variation metrics")

# Focus on adalimumab and rituximab
# Use last 6 months of data (Jan-Jun 2022) for stable estimates

snapshot_period <- ms_state %>%
  filter(period >= ymd("2022-01-01"),
         molecule %in% c("adalimumab", "rituximab")) %>%
  group_by(molecule, state) %>%
  summarise(
    biosimilar_rx = sum(biosimilar_rx),
    reference_rx = sum(reference_rx),
    total_rx = sum(total_rx),
    biosimilar_share = biosimilar_rx / total_rx,
    .groups = "drop"
  ) %>%
  left_join(state_chars %>% select(state, state_name, population_2021),
            by = "state")

# Calculate variation metrics per molecule
variation_metrics <- snapshot_period %>%
  group_by(molecule) %>%
  summarise(
    national_share = sum(biosimilar_rx) / sum(total_rx),
    mean_state = mean(biosimilar_share),
    median_state = median(biosimilar_share),
    sd_state = sd(biosimilar_share),
    cv = sd_state / mean_state,
    min_state = min(biosimilar_share),
    max_state = max(biosimilar_share),
    range = max_state - min_state,
    iqr = IQR(biosimilar_share),
    state_min = state[which.min(biosimilar_share)],
    state_max = state[which.max(biosimilar_share)],
    .groups = "drop"
  )

cli_h2("Variation metrics (Jan-Jun 2022 averaged)")
print(variation_metrics %>%
        mutate(across(c(national_share, mean_state, median_state, sd_state,
                        min_state, max_state, range, iqr),
                      ~ round(.x * 100, 1)),
               cv = round(cv, 3)))

# State-level snapshot table
state_snapshot <- snapshot_period %>%
  arrange(molecule, desc(biosimilar_share)) %>%
  mutate(biosimilar_share = round(biosimilar_share, 4))

write_csv(state_snapshot, here("outputs", "tables", "tbl_state_variation.csv"))
cli_alert_success("Saved tbl_state_variation.csv")


# ═══════════════════════════════════════════════════════════════════════════════
# SECTION 2: TIME-VARYING GEOGRAPHIC VARIATION
# ═══════════════════════════════════════════════════════════════════════════════

cli_h1("Section 2: Time-varying geographic variation")

# Calculate quarterly CV and IQR for adalimumab and rituximab
# (quarterly smooths out monthly noise for small states)

variation_time <- ms_state %>%
  filter(molecule %in% c("adalimumab", "rituximab")) %>%
  mutate(quarter = floor_date(period, "quarter")) %>%
  group_by(molecule, quarter, state) %>%
  summarise(
    biosimilar_rx = sum(biosimilar_rx),
    total_rx = sum(total_rx),
    biosimilar_share = if_else(total_rx > 0, biosimilar_rx / total_rx, 0),
    .groups = "drop"
  ) %>%
  # Only keep quarters where all states have data
  group_by(molecule, quarter) %>%
  filter(n() == 8) %>%
  summarise(
    cv = sd(biosimilar_share) / mean(biosimilar_share),
    iqr = IQR(biosimilar_share),
    range = max(biosimilar_share) - min(biosimilar_share),
    mean_share = mean(biosimilar_share),
    n_states = n(),
    .groups = "drop"
  ) %>%
  # CV is undefined when mean is 0 (pre-listing)
  mutate(cv = if_else(is.finite(cv), cv, NA_real_))

mol_labels <- c(adalimumab = "Adalimumab", rituximab = "Rituximab")

p_variation_time <- ggplot(variation_time %>%
                             filter(mean_share > 0.001),  # Only post-listing
                           aes(x = quarter)) +
  geom_line(aes(y = cv), colour = "#D6604D", linewidth = 0.7) +
  geom_point(aes(y = cv), colour = "#D6604D", size = 1) +
  facet_wrap(~molecule, scales = "free",
             labeller = labeller(molecule = mol_labels)) +
  scale_x_date(date_labels = "%b\n%Y", date_breaks = "6 months") +
  labs(
    title = "Geographic variation in biosimilar adoption over time",
    subtitle = "Coefficient of variation across 8 states/territories, quarterly",
    x = NULL, y = "Coefficient of variation (CV)",
    caption = "Source: Medicare Statistics (Jan 2009\u2013Jun 2022). CV = SD/mean of state-level biosimilar share."
  ) +
  theme_pbs

save_fig(p_variation_time, "fig_state_variation_time", 12, 5.5)


# ═══════════════════════════════════════════════════════════════════════════════
# SECTION 3: FUNNEL PLOTS
# ═══════════════════════════════════════════════════════════════════════════════

cli_h1("Section 3: Funnel plots")

# Funnel plot: state biosimilar share vs total prescriptions (volume)
# Control limits based on binomial distribution

make_funnel <- function(data, mol_name, title_extra = "") {
  national_avg <- sum(data$biosimilar_rx) / sum(data$total_rx)

  # Generate smooth control limits across range of volumes
  n_range <- seq(min(data$total_rx) * 0.5, max(data$total_rx) * 1.2, length.out = 200)
  se_range <- sqrt(national_avg * (1 - national_avg) / n_range)

  limits <- tibble(
    total_rx = n_range,
    upper_95 = national_avg + 1.96 * se_range,
    lower_95 = national_avg - 1.96 * se_range,
    upper_998 = national_avg + 3.09 * se_range,
    lower_998 = national_avg - 3.09 * se_range
  )

  # Flag outliers
  data <- data %>%
    mutate(
      se = sqrt(national_avg * (1 - national_avg) / total_rx),
      z_score = (biosimilar_share - national_avg) / se,
      outlier = abs(z_score) > 1.96,
      label_text = state
    )

  p <- ggplot() +
    # 99.8% limits
    geom_ribbon(data = limits, aes(x = total_rx, ymin = lower_998, ymax = upper_998),
                fill = "grey90", alpha = 0.5) +
    # 95% limits
    geom_ribbon(data = limits, aes(x = total_rx, ymin = lower_95, ymax = upper_95),
                fill = "grey80", alpha = 0.5) +
    # National average
    geom_hline(yintercept = national_avg, linetype = "dashed", colour = "#D6604D") +
    # State points
    geom_point(data = data, aes(x = total_rx, y = biosimilar_share, colour = outlier),
               size = 3) +
    geom_text(data = data, aes(x = total_rx, y = biosimilar_share, label = label_text),
              nudge_y = 0.008, size = 3, fontface = "bold") +
    scale_colour_manual(values = c("FALSE" = "#2166AC", "TRUE" = "#B2182B"),
                        guide = "none") +
    scale_x_continuous(labels = comma_format()) +
    scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
    annotate("text", x = max(data$total_rx) * 0.95, y = national_avg + 0.003,
             label = paste0("National: ", round(national_avg * 100, 1), "%"),
             colour = "#D6604D", size = 3, hjust = 1, fontface = "italic") +
    labs(
      title = paste0(mol_name, ": funnel plot of state biosimilar adoption", title_extra),
      subtitle = "95% (dark grey) and 99.8% (light grey) binomial control limits. Red = outlier (outside 95%).",
      x = "Total prescriptions (Jan\u2013Jun 2022)",
      y = "Biosimilar market share",
      caption = "Source: Medicare Statistics. National average shown as dashed red line."
    ) +
    theme_pbs

  list(plot = p, data = data, national_avg = national_avg)
}

# ─── Adalimumab funnel ───────────────────────────────────────────────────────

cli_h2("Adalimumab funnel plot")

adal_snapshot <- snapshot_period %>% filter(molecule == "adalimumab")
funnel_adal <- make_funnel(adal_snapshot, "Adalimumab")
save_fig(funnel_adal$plot, "fig_funnel_adalimumab", 10, 6.5)

cli_alert_info("Adalimumab national average: {round(funnel_adal$national_avg * 100, 1)}%")
cli_alert_info("Outlier states:")
print(funnel_adal$data %>% filter(outlier) %>% select(state, biosimilar_share, z_score, total_rx))

# ─── Rituximab funnel ────────────────────────────────────────────────────────

cli_h2("Rituximab funnel plot")

ritux_snapshot <- snapshot_period %>% filter(molecule == "rituximab")
funnel_ritux <- make_funnel(ritux_snapshot, "Rituximab")
save_fig(funnel_ritux$plot, "fig_funnel_rituximab", 10, 6.5)

cli_alert_info("Rituximab national average: {round(funnel_ritux$national_avg * 100, 1)}%")
cli_alert_info("Outlier states:")
print(funnel_ritux$data %>% filter(outlier) %>% select(state, biosimilar_share, z_score, total_rx))


# ═══════════════════════════════════════════════════════════════════════════════
# SECTION 4: MULTILEVEL ITS FOR RITUXIMAB
# ═══════════════════════════════════════════════════════════════════════════════

cli_h1("Section 4: Multilevel ITS model (rituximab)")

# Rituximab has clean brand-level data and interventions within the Medicare era
# Model: biosimilar_share ~ time + interventions + (1 + time_after_listing | state)

ritux_state <- ms_state %>%
  filter(molecule == "rituximab") %>%
  arrange(state, period) %>%
  group_by(state) %>%
  mutate(time = row_number()) %>%
  ungroup()

# Intervention dates
ritux_listing   <- ymd("2019-10-01")
ritux_delist_iv <- ymd("2021-04-01")

ritux_state <- ritux_state %>%
  mutate(
    post_listing = as.integer(period >= ritux_listing),
    time_after_listing = pmax(0, round(as.numeric(
      difftime(period, ritux_listing, units = "days")) / 30.44)),
    post_delist_iv = as.integer(period >= ritux_delist_iv),
    time_after_delist_iv = pmax(0, round(as.numeric(
      difftime(period, ritux_delist_iv, units = "days")) / 30.44))
  )

# ─── Model 1: Random intercept only ─────────────────────────────────────────

cli_h2("Multilevel model: random intercept")

ml_ri <- lmer(biosimilar_share ~ time + post_listing + time_after_listing +
                post_delist_iv + time_after_delist_iv +
                (1 | state),
              data = ritux_state)

cli_alert_info("Random intercept model:")
print(summary(ml_ri))

# ICC: proportion of variance between states
var_comp <- as.data.frame(VarCorr(ml_ri))
icc <- var_comp$vcov[1] / sum(var_comp$vcov)
cli_alert_info("ICC (intraclass correlation): {round(icc, 4)}")
cli_alert_info("  {round(icc * 100, 1)}% of variation in biosimilar share is between states")

# ─── Model 2: Random intercept + random slope ───────────────────────────────

cli_h2("Multilevel model: random intercept + slope")

ml_ris <- tryCatch(
  lmer(biosimilar_share ~ time + post_listing + time_after_listing +
         post_delist_iv + time_after_delist_iv +
         (1 + time_after_listing | state),
       data = ritux_state,
       control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 20000))),
  error = function(e) {
    cli_alert_warning("Random slope model failed: {e$message}")
    NULL
  }
)

if (!is.null(ml_ris)) {
  cli_alert_info("Random intercept + slope model:")
  print(summary(ml_ris))

  # Extract state-specific slopes
  state_effects <- ranef(ml_ris)$state %>%
    rownames_to_column("state") %>%
    as_tibble() %>%
    rename(intercept_re = `(Intercept)`, slope_re = time_after_listing) %>%
    mutate(
      total_slope = fixef(ml_ris)["time_after_listing"] + slope_re
    )

  cli_alert_info("State-specific adoption slopes (post-listing):")
  print(state_effects %>%
          arrange(desc(total_slope)) %>%
          mutate(across(c(intercept_re, slope_re, total_slope), ~ round(.x, 5))))

  # Model comparison
  lr_test <- anova(ml_ri, ml_ris)
  cli_alert_info("LR test (random slope vs intercept only): p = {round(lr_test$`Pr(>Chisq)`[2], 4)}")
}

# ─── Save multilevel results ────────────────────────────────────────────────

ml_results <- broom.mixed::tidy(ml_ri) %>%
  mutate(model = "Random intercept") %>%
  bind_rows(
    if (!is.null(ml_ris)) broom.mixed::tidy(ml_ris) %>% mutate(model = "Random intercept + slope")
  )

write_csv(ml_results, here("outputs", "tables", "tbl_multilevel_rituximab.csv"))
cli_alert_success("Saved tbl_multilevel_rituximab.csv")


# ═══════════════════════════════════════════════════════════════════════════════
# SECTION 5: STATE CHARACTERISTIC CORRELATES
# ═══════════════════════════════════════════════════════════════════════════════

cli_h1("Section 5: State characteristic correlates")

# Join state characteristics to snapshot data (only columns not already present)
correlate_data <- snapshot_period %>%
  left_join(state_chars %>% select(state, pct_65plus, pct_remote, seifa_irsd, specialist_per_100k),
            by = "state")

# ─── Correlation matrix ──────────────────────────────────────────────────────

cli_h2("Correlations: biosimilar share vs state characteristics")

correlate_vars <- c("pct_65plus", "pct_remote", "seifa_irsd", "specialist_per_100k")

for (mol in c("adalimumab", "rituximab")) {
  d <- correlate_data %>% filter(molecule == mol)
  cli_alert_info("{mol}:")
  for (var in correlate_vars) {
    r <- cor(d$biosimilar_share, d[[var]], use = "complete.obs")
    cli_alert_info("  r({var}) = {round(r, 3)}")
  }
}

# ─── Scatter plots of correlates ─────────────────────────────────────────────

cli_h2("Correlate scatter plots")

correlate_long <- correlate_data %>%
  filter(molecule == "adalimumab") %>%
  select(state, biosimilar_share, all_of(correlate_vars)) %>%
  pivot_longer(cols = all_of(correlate_vars),
               names_to = "characteristic",
               values_to = "value") %>%
  mutate(
    characteristic = case_when(
      characteristic == "pct_65plus" ~ "Population aged 65+ (%)",
      characteristic == "pct_remote" ~ "Population in remote areas (%)",
      characteristic == "seifa_irsd" ~ "SEIFA IRSD score",
      characteristic == "specialist_per_100k" ~ "Specialists per 100k"
    )
  )

p_correlates <- ggplot(correlate_long,
                       aes(x = value, y = biosimilar_share)) +
  geom_point(colour = "#2166AC", size = 3) +
  geom_text(aes(label = state), nudge_y = 0.006, size = 2.8) +
  geom_smooth(method = "lm", se = TRUE, colour = "#D6604D",
              fill = "#D6604D", alpha = 0.15, linewidth = 0.6) +
  facet_wrap(~characteristic, scales = "free_x") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Adalimumab biosimilar adoption vs state characteristics",
    subtitle = "State-level biosimilar share (Jan\u2013Jun 2022) vs published demographic and healthcare indicators",
    x = NULL, y = "Biosimilar market share",
    caption = "Source: PBS Medicare Statistics, ABS Census 2021, AIHW. N = 8 states; correlations are exploratory."
  ) +
  theme_pbs

save_fig(p_correlates, "fig_state_correlates", 12, 8)

# ─── Save correlate data ────────────────────────────────────────────────────

write_csv(correlate_data, here("outputs", "tables", "tbl_state_correlates.csv"))
cli_alert_success("Saved tbl_state_correlates.csv")


# ═══════════════════════════════════════════════════════════════════════════════
# SECTION 6: CHOROPLETH MAP (OPTIONAL)
# ═══════════════════════════════════════════════════════════════════════════════

has_sf <- requireNamespace("sf", quietly = TRUE)
has_ozmaps <- requireNamespace("ozmaps", quietly = TRUE)

if (has_sf && has_ozmaps) {
  cli_h1("Section 6: Choropleth map")

  library(sf)
  library(ozmaps)

  aus_states <- ozmap_states %>%
    mutate(state = case_when(
      NAME == "New South Wales" ~ "NSW",
      NAME == "Victoria" ~ "VIC",
      NAME == "Queensland" ~ "QLD",
      NAME == "Western Australia" ~ "WA",
      NAME == "South Australia" ~ "SA",
      NAME == "Tasmania" ~ "TAS",
      NAME == "Australian Capital Territory" ~ "ACT",
      NAME == "Northern Territory" ~ "NT",
      TRUE ~ NA_character_
    )) %>%
    filter(!is.na(state))

  # Join adalimumab adoption data
  map_data <- aus_states %>%
    left_join(snapshot_period %>% filter(molecule == "adalimumab") %>%
                select(state, biosimilar_share),
              by = "state")

  p_map <- ggplot(map_data) +
    geom_sf(aes(fill = biosimilar_share), colour = "white", linewidth = 0.3) +
    geom_sf_text(aes(label = paste0(state, "\n", round(biosimilar_share * 100, 1), "%")),
                 size = 2.5, fontface = "bold") +
    scale_fill_distiller(
      palette = "RdYlBu",
      direction = 1,
      labels = percent_format(accuracy = 1),
      name = "Biosimilar\nshare"
    ) +
    labs(
      title = "Adalimumab biosimilar market share by state",
      subtitle = "Average Jan\u2013Jun 2022, Medicare Statistics",
      caption = "Source: PBS Medicare Statistics. Item-sharing affects absolute levels but relative state rankings are informative."
    ) +
    theme_void(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 14, margin = margin(b = 4)),
      plot.subtitle = element_text(colour = "grey40", size = 10.5, margin = margin(b = 10)),
      plot.caption = element_text(colour = "grey50", size = 8, hjust = 0, margin = margin(t = 10)),
      plot.margin = margin(12, 12, 8, 12),
      legend.position = "right"
    )

  save_fig(p_map, "fig_state_map", 10, 8)

} else {
  cli_alert_info("Skipping choropleth map (sf and/or ozmaps not installed)")
  cli_alert_info("Install with: install.packages(c('sf', 'ozmaps'))")
}


# ═══════════════════════════════════════════════════════════════════════════════
# FINAL SUMMARY
# ═══════════════════════════════════════════════════════════════════════════════

cli_h1("Phase 5 complete")

cli_alert_info("Key findings:")
cli_alert_info("")

for (mol in c("adalimumab", "rituximab")) {
  vm <- variation_metrics %>% filter(molecule == mol)
  cli_alert_info("{str_to_title(mol)}:")
  cli_alert_info("  National average: {round(vm$national_share * 100, 1)}%")
  cli_alert_info("  Range: {round(vm$min_state * 100, 1)}% ({vm$state_min}) to {round(vm$max_state * 100, 1)}% ({vm$state_max})")
  cli_alert_info("  CV: {round(vm$cv, 3)}, IQR: {round(vm$iqr * 100, 1)} pp")
  cli_alert_info("")
}

cli_alert_info("ICC (rituximab multilevel model): {round(icc * 100, 1)}% of variation is between states")
cli_alert_info("")
cli_alert_info("Data limitation: State data ends Jun 2022 (Medicare Statistics).")
cli_alert_info("DoS data (Jul 2021+) provides national totals only.")
cli_alert_info("")
cli_alert_info("Next: Run 08_counterfactual.R for cross-national comparison and savings estimate")
