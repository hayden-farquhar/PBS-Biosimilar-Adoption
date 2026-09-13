################################################################################
# 09_figure2_observed.R
#
# Main-text Figure 2, rebuilt for the R1 revision.
#
# The submitted Figure 2 showed a fitted segmented regression over the
# adalimumab series. That figure can no longer appear in the main text: the two
# modelled policy dates do not fall within the confidence interval of any
# structural break detected in the series, so the intervention effects are not
# identified and drawing the fitted line would assert an inference the design
# does not support. The fitted version is retained in Additional file 1
# alongside the A5 coefficients, reported for completeness only.
#
# This script draws the series as observed, marks the two policy dates, and
# marks the four Bai-Perron breaks so the reader can see directly that they do
# not coincide.
#
# Inputs:  data/processed/market_share_national.csv
# Outputs: outputs/figures/fig1_rituximab_observed.{pdf,png}
#          outputs/figures/fig2_adalimumab_observed.{pdf,png}
#
# Figure 1 is rebuilt on the same principle. The submitted version drew the
# fitted segmented regression over rituximab, and that line rises above 100%
# market share by 2025 while missing the observed sigmoid entirely -- a linear
# segmented model on a bounded proportion. Having withdrawn the fitted line from
# Figure 2 because the coefficients are not precisely attributed, it would be
# incoherent to keep it in Figure 1. Both figures now show the series as
# observed, which is also the stronger evidence for the rituximab claim: a
# decade at exactly zero followed by a complete switch needs no fitted line.
################################################################################

library(tidyverse)
library(lubridate)
library(here)
library(scales)
library(strucchange)
library(cli)

make_fig <- function(mol, start, ylim, ybreak_lab, policy, title, subtitle, outname,
                     annotate_peak = TRUE, detect_from = start) {
  # Breaks are detected on the ANALYSIS window (detect_from), which for
  # rituximab is the full series, so that the breaks drawn match those reported
  # in the text. `start` only truncates what is displayed, for legibility.
  full <- read_csv(here("data", "processed", "market_share_national.csv"),
                   show_col_types = FALSE) %>%
    filter(molecule == mol, period >= ymd(detect_from)) %>%
    mutate(period = ymd(period)) %>% arrange(period) %>% mutate(t = row_number())

  bp <- breakpoints(biosimilar_share ~ t, data = full, h = 0.15)
  break_dates <- full$period[bp$breakpoints]

  d <- full %>% filter(period >= ymd(start))
  cli_alert_info("{mol}: breaks {paste(format(break_dates, '%b %Y'), collapse=', ')}")

  span <- ylim[2] - ylim[1]
  policy <- policy %>% mutate(ypos = ylim[2] - 0.02 * span - (row_number() - 1) * 0.11 * span)

  peak <- d %>% slice_max(biosimilar_share, n = 1) %>% slice(1)
  last_obs <- d %>% slice_max(period, n = 1)

  p <- ggplot(d, aes(period, biosimilar_share)) +
    geom_vline(xintercept = break_dates, linetype = "dotted", colour = "grey55", linewidth = 0.5) +
    geom_vline(data = policy, aes(xintercept = date), linetype = "dashed",
               colour = "#B2182B", linewidth = 0.55) +
    geom_line(colour = "#2166AC", linewidth = 0.75) +
    geom_point(colour = "#2166AC", size = 1.0) +
    geom_text(data = policy, aes(x = date, y = ypos, label = label),
              inherit.aes = FALSE, size = 2.8, colour = "#B2182B", hjust = -0.06,
              vjust = 1, lineheight = 0.9) +
    scale_y_continuous(labels = percent_format(accuracy = 1), limits = ylim, expand = c(0, 0)) +
    scale_x_date(date_breaks = ybreak_lab, date_labels = "%b\n%Y", expand = expansion(mult = 0.03)) +
    labs(x = NULL, y = "Biosimilar share of prescriptions", title = title, subtitle = subtitle,
         caption = "Red dashed lines: policy dates. Grey dotted lines: structural breaks located by Bai-Perron detection, without reference to the policy calendar.\nSource: PBS Date of Supply (Jul 2021-Nov 2025) and Medicare Statistics (Jan 2009-Jun 2022), national records only.") +
    theme_minimal(base_size = 11) +
    theme(plot.title = element_text(face = "bold", size = 12.5, margin = margin(b = 3)),
          plot.subtitle = element_text(colour = "grey35", size = 9.2, margin = margin(b = 11), lineheight = 1.1),
          plot.caption = element_text(colour = "grey50", size = 7.6, hjust = 0, margin = margin(t = 9)),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(linewidth = 0.3, colour = "grey90"),
          axis.text = element_text(size = 8.8), plot.margin = margin(12, 14, 8, 12))

  if (annotate_peak) {
    p <- p +
      geom_point(data = peak, colour = "#B2182B", size = 2.4) +
      annotate("text", x = peak$period, y = peak$biosimilar_share + 0.018,
               label = paste0("Maximum ", percent(peak$biosimilar_share, 0.1), "\n",
                              format(peak$period, "%b %Y")),
               size = 3.1, colour = "#B2182B", lineheight = 0.95) +
      annotate("text", x = last_obs$period, y = last_obs$biosimilar_share - 0.020,
               label = percent(last_obs$biosimilar_share, 0.1), size = 3.1,
               colour = "#2166AC", hjust = 1)
  }
  ggsave(here("outputs", "figures", paste0(outname, ".pdf")), p, width = 8.2, height = 5.0)
  ggsave(here("outputs", "figures", paste0(outname, ".png")), p, width = 8.2, height = 5.0, dpi = 300)
  cli_alert_success("Saved {outname}.pdf/png")
}

cli_h1("Figure 1: observed rituximab trajectory")
make_fig("rituximab", "2017-01-01", c(0, 1.05), "12 months",
  tibble(date = ymd(c("2019-10-01","2021-04-01","2021-10-01")),
         label = c("Biosimilar\nlisted","MabThera IV\ndelisted","MabThera SC\ndelisted")),
  "Rituximab: zero to complete substitution after reference delisting",
  "Observed series. Share was identically zero for 127 months to October 2019 and reached 100% by October 2023.\nNo fitted regression is shown; a linear segmented model on a bounded share exceeded 100% and did not track the observed curve.",
  "fig1_rituximab_observed", annotate_peak = FALSE, detect_from = "2009-01-01")

cli_h1("Figure 2: observed adalimumab trajectory")
make_fig("adalimumab", "2022-01-01", c(0.14, 0.30), "6 months",
  tibble(date = ymd(c("2023-04-01","2023-11-01")),
         label = c("24% statutory\nprice cut","Streamlined\nauthority")),
  "Adalimumab biosimilar uptake reached 26% in mid-2024 and has since reversed",
  "Observed series. No fitted regression is shown: neither policy date falls within the confidence\ninterval of any detected structural break, so the intervention effects are not identified.",
  "fig2_adalimumab_observed", annotate_peak = TRUE)
