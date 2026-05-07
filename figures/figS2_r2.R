#----- figure S2 panel A: absolute costs ($) -----
p_abs <- ggplot(groups, aes(reorder(cluster_num, -total_cost_per_test), cost_per_test, fill = category)) +
    geom_bar(stat = "identity", color = "black") +
    geom_text(aes(forcats::fct_rev(cluster_num), label = ifelse(cost_per_test > 3, paste0("$", round(cost_per_test, 2)), "")), 
              position = position_stack(vjust = 0.5), 
              size = 4, 
              color = "black") +
    geom_text(aes(forcats::fct_rev(cluster_num), 89, label = paste0("Total: $", round(total_cost_per_test, 2))), color = "black") +
    facet_wrap(~ country, ncol = 1, scales = "free_y") +
    scale_y_continuous(breaks = seq(0, 90, by = 10)) +
    scale_fill_manual(values = c(pal28[11], pal28[7], "grey70", "grey95")) +
    coord_flip() +
    labs(x = NULL, y = "Cost per test (2020 US$)", fill = "Cost category") +
    NULL

#----- figure S2 panel B: proportions (100% stacked) -----
p_pct <- ggplot(groups, aes(reorder(cluster_num, -total_cost_per_test), y = percent, fill = category)) +
    geom_bar(stat = "identity", color = "black") +
    geom_text(aes(forcats::fct_rev(cluster_num), label = ifelse(percent > 10, paste0(round(percent), "%"), "")), 
              position = position_stack(vjust = 0.5), 
              size = 4, 
              color = "black") +
    facet_wrap(~ country, ncol = 1, scales = "free") +
    scale_y_continuous(breaks = seq(0, 100, by = 10)) +
    scale_fill_manual(values = c(pal28[11], pal28[7], "grey70", "grey95")) +
    coord_flip() +
    labs(x = NULL, y = "Proportion of total cost per test (%)", fill = "Cost category") +
    NULL


#----- figure S2: combine panels and save -----
p_abs / p_pct + plot_layout(guides = "collect") + plot_annotation(tag_levels = "A") & theme(plot.tag = element_text(size = 24))

ggsave("/figures/figS2.png",
       path = path_sub_results,
       width = 4000 + 400,
       height = 6000,
       units = "px",
       dpi = 300)
