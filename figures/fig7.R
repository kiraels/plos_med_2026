# fig 7
## icer sensitivity to intrauterine transmission

plot_icer_sensitivity <- function(d, wtp) {
    
    label_data <- map_dfr(names(wtp), function(y) {
        tibble(
            country = y,
            lambda = wtp[[y]],
            label = paste0("WTP $", wtp[[y]]),
            x = 0.38
        )
    })
    
    ggplot(d, aes(x = pi, y = u_icer, color = country, fill = country)) +
        geom_ribbon(aes(ymin = ll_icer, ymax = ul_icer), alpha = 0.2, color = NA) +
        geom_line(linewidth = 0.5) +
        geom_hline(data = label_data, aes(yintercept = lambda, color = country), linetype = "dashed", linewidth = 0.35, inherit.aes = FALSE) +
        geom_text(data = label_data, aes(x = 0.38, y = lambda, label = label, color = country), vjust = -0.7, size = 3, inherit.aes = FALSE) +
        facet_wrap(~ country) +
        scale_x_continuous(labels = scales::percent_format(), limits = c(0, 0.405)) +
        scale_y_continuous(breaks = seq(0, 3000, by = 500)) +
        coord_cartesian(ylim = c(0, 3000)) +
        scale_color_manual(values = c(pal28[6], pal28[23])) +
        scale_fill_manual(values = c(pal28[6], pal28[23])) +
        labs(
            x = "Transmission rate at birth",
            y = "Incremental cost per HIV-positive infant\ninitiating ART within 1 week (2020 US$)",
            color = "",
            fill = ""
        ) +
        theme(legend.position = "none")
}

plot_icer_sensitivity(iut_sens, wtp)

ggsave("/figures/fig7.png",
       path = path_sub_results,
       width = 173,
       height = 90,
       units = "mm",
       dpi = 300)
