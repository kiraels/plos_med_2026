# figure 6
## one-way sensitivity analysis for cost per test 

plot_tornado <- function(d) {
    # compute max impact 
    param_order <- d %>%
        filter(country == "Mozambique") %>%
        group_by(parameter) %>%
        summarise(max_impact = min(delta, na.rm = TRUE), .groups = "drop") %>%
        arrange(-max_impact) %>%
        pull(parameter)
    
    # set factor levels 
    d <- d %>% mutate(parameter = factor(parameter, levels = param_order))
    
    param_labels <- c(
        t   = "Testing volume\n[-50-200%]",
        r   = "Discount rate\n[0-6%]",
        C_c = "Consumables price\n[-50-50%]",
        n   = "Equipment lifespan\n[4-15 years]"
    )
    
    ggplot(d, aes(x = parameter, y = delta, fill = which)) +
        geom_bar(stat = "identity", width = 0.7) +
        geom_hline(yintercept = 0, size = 1) +
        facet_wrap(~ country, nrow = 2) +
        scale_x_discrete(labels = param_labels) +
        scale_fill_manual(values = c(Low = pal28[11], High = pal28[7])) +
        coord_flip() +
        labs(x = "", y = "Δ Cost per test (2020 US$)", fill = "") +
        theme(legend.position = "bottom")
}

plot_tornado(tornado_data_agg) + 
    geom_text(aes(label = paste0("$", round(delta, 2)), hjust = ifelse(delta >= 0, -0.1, 1.1)), size = 3) +
    scale_y_continuous(limits = c(-17, 17), breaks = seq(-15, 15, by = 3)) 

ggsave("/figures/fig6.png",
       path = path_sub_results,
       width = 173,
       height = 130,
       units = "mm",
       dpi = 300
)
