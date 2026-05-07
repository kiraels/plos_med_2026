# ----- get delta for each outcome -----
delta_draws1.0 <- calculate_delta_draws(ce_draws1.0)
delta_draws2.0 <- calculate_delta_draws(ce_draws2.0)


# ----- ce planes - 1wk ART -----

set.seed(12)
a <- delta_draws1.0 %>%
    filter(country == "Mozambique") %>%
    slice_sample(n = 500) %>%
    ggplot(aes(dE, dC)) +
    geom_point(color = pal28[6], shape = 18, size = 1) +
    geom_hline(aes(yintercept = 0)) +
    geom_vline(aes(xintercept = 0)) +
    scale_x_continuous(limits = c(-1, 1), breaks = seq(0, 100, by = 0.25), labels = scales::percent_format(scale = 100)) +
    scale_y_continuous(limits = c(-4000, 4000), breaks = seq(-5000, 5000, by = 1000), labels = scales::dollar_format()) +
    labs(x = "Percent increase of HIV-positive\ninfants on ART within 1 week", 
         y = "Incremental cost per HIV-positive infant",
         title = "Mozambique: mPIMA\nCost-effectiveness plane") +
    NULL

set.seed(13)
b <- delta_draws1.0 %>%
    filter(country == "Tanzania") %>%
    slice_sample(n = 500) %>%
    ggplot(.) +
    geom_point(aes(dE, dC), color = pal28[23], shape = 18, size = 1) +
    geom_hline(aes(yintercept = 0)) +
    geom_vline(aes(xintercept = 0)) +
    scale_x_continuous(limits = c(-1, 1), breaks = seq(0, 100, by = 0.25), labels = scales::percent_format(scale = 100)) +
    scale_y_continuous(limits = c(-8000, 8000), breaks = seq(-10000, 10000, by = 2000), labels = scales::dollar_format()) +
    labs(x = "Percent increase of HIV-positive\ninfants on ART within 1 week", 
         y = "Incremental cost per HIV-positive infant",
         title = "Tanzania: Xpert\nCost-effectiveness plane") +
    NULL

# ----- ceac - 1wk ART -----

c <- ggplot(cea_results_moz1.0$ceac) +
    geom_line(aes(k, prob, color = studyarm), linewidth = 0.75) +
    scale_x_continuous(limits = c(0, 50000), breaks = seq(0, 50000, by = 10000), labels = scales::dollar_format()) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
    scale_color_manual(values = c(pal28[6])) +
    labs(x = "Willingness to pay (2020 US$; per life-year)", 
         y = "Probability VEID cost-\neffective versus SoC", 
         color = "",
         title = "Cost-effectiveness acceptability curve") +
    theme(legend.position = "none")

d <- ggplot(cea_results_tan1.0$ceac) +
    geom_line(aes(k, prob, color = studyarm), linewidth = 0.75) +
    scale_x_continuous(limits = c(0, 50000), breaks = seq(0, 50000, by = 10000), labels = scales::dollar_format()) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
    scale_color_manual(values = c(pal28[23])) +
    labs(x = "Willingness to pay (2020 US$; per life-year)", 
         y = "Probability VEID cost-\neffective versus SoC", 
         color = "",
         title = "Cost-effectiveness acceptability curve") +
    theme(legend.position = "none")

# ----- ce planes - 8wk EID -----

set.seed(12)
e <- delta_draws2.0 %>%
    filter(country == "Mozambique") %>%
    slice_sample(n = 500) %>%
    ggplot(.) +
    geom_point(aes(dE, dC), color = pal28[6], shape = 18, size = 1) +
    geom_hline(aes(yintercept = 0)) +
    geom_vline(aes(xintercept = 0)) +
    scale_x_continuous(limits = c(-0.3, 0.3), breaks = seq(0, 100, by = 0.1), labels = scales::percent_format(scale = 100)) +
    scale_y_continuous(limits = c(-80, 80), breaks = seq(-500, 500, by = 20), labels = scales::dollar_format()) +
    labs(x = "Percent increase of HIV-exposed infants\nwith a PoC EID test within 8 weeks", 
         y = "Incremental cost per HIV-exposed infant",
         title = "Cost-effectiveness plane") +
    NULL

set.seed(14)
f <- delta_draws2.0 %>%
    filter(country == "Tanzania") %>%
    slice_sample(n = 500) %>%
    ggplot(.) +
    geom_point(aes(dE, dC), color = pal28[23], shape = 18, size = 1) +
    geom_hline(aes(yintercept = 0)) +
    geom_vline(aes(xintercept = 0)) +
    scale_x_continuous(limits = c(-0.3, 0.3), breaks = seq(0, 100, by = 0.1), labels = scales::percent_format(scale = 100)) +
    scale_y_continuous(limits = c(-80, 80), breaks = seq(-500, 500, by = 20), labels = scales::dollar_format()) +
    labs(x = "Percent increase of HIV-exposed infants\nwith a PoC EID test within 8 weeks", 
         y = "Incremental cost per HIV-exposed infant",
         title = "Cost-effectiveness plane") +
    NULL

# ----- ceac - 8wk EID -----
# ceac

g <- ggplot(cea_results_moz2.0$ceac) +
    geom_line(aes(k, prob, color = studyarm), linewidth = 0.75) +
    scale_x_continuous(limits = c(0, 1000), breaks = seq(0, 1000, by = 200), labels = scales::dollar_format()) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
    scale_color_manual(values = c(pal28[6])) +
    labs(x = "Willingness to pay (2020 US$; per life-year)", 
         y = "Probability VEID cost-\neffective versus SoC", 
         color = "",
         title = "Cost-effectiveness acceptability curve") +
    theme(legend.position = "none")

h <- ggplot(cea_results_tan2.0$ceac) +
    geom_line(aes(k, prob, color = studyarm), linewidth = 0.75) +
    scale_x_continuous(limits = c(0, 1000), breaks = seq(0, 1000, by = 200), labels = scales::dollar_format()) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
    scale_color_manual(values = c(pal28[23])) +
    labs(x = "Willingness to pay (2020 US$; per life-year)", 
         y = "Probability VEID cost-\neffective versus SoC", 
         color = "",
         title = "Cost-effectiveness acceptability curve") +
    theme(legend.position = "none")

# ----- fig formatting -----
block1 <- patchwork::wrap_plots(a, b, c, d, ncol = 2, heights = c(2, 1))
block2 <- patchwork::wrap_plots(e, f, g, h, ncol = 2, heights = c(2, 1))

block1 / block2 + plot_annotation(tag_levels = "A") & theme(plot.tag = element_text(size = 9.5))

ggsave("/figures/fig4.png",
       path = path_sub_results,
       width = 173,
       height = 222.3,
       units = "mm",
       dpi = 300)
