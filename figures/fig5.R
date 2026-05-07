
# ly ribbon per country
ggplot(ly_threshold, aes(x = lambda, y = median_per_early, color = country, fill = country)) +
    #geom_errorbar(aes(ymin = ci_per_early_low, ymax = ci_per_early_high), width = 0.1) +
    geom_ribbon(aes(ymin = ci_per_early_low, ymax = ci_per_early_high), alpha = 0.2, color = NA) +
    geom_line(size = 0.5) +
    geom_point(size = 1) +
    scale_x_continuous(breaks = lambda_grid) +
    scale_y_continuous(breaks = seq(0, 140, by = 20)) +
    scale_color_manual(values = c(pal28[6], pal28[23])) +
    scale_fill_manual(values = c(pal28[6], pal28[23])) +
    labs(x = "Willingness to pay (per life-year; 2020 US$)",
         y = "Required life-years per early ART initiation",
         color = "",
         fill = "") +
    theme(legend.position = "bottom")

ggsave("/figures/fig5.png",
       path = path_sub_results,
       width = 1400,
       height = 1400 + 200,
       units = "px",
       dpi = 300)

ggsave("/figures/fig5.png",
       path = path_sub_results,
       width = 83,
       height = 90,
       units = "mm",
       dpi = 300)
