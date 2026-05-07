# figure 3 revision - unit cost per test components

#----- testing volume groups -----
groups <- cost_basis_fa$totals %>%
    mutate(group = ifelse(life_poc_total < 5*52, "low",
                          ifelse(life_poc_total >= 5 & life_poc_total <= 12*52, "med",
                                 "high"))) %>%
    mutate(group = factor(group, levels = c("low", "med", "high"), labels = c("Low (<5)", "Med. (5-12)", "High (>12)"))) %>%
    dplyr::select(cluster_num, group) %>%
    unique() %>%
    merge(., cost_basis_fa$components) %>%
    # compute totals and percent shares (keeps total on every row)
    group_by(country, cluster_num, group) %>%
    mutate(percent = 100 * cost_per_test / total_cost_per_test) %>%
    # formatting for plots
    mutate(country = factor(country, 
                            levels = c("Mozambique", "Tanzania"), 
                            labels = c("Mozambique: mPIMA", "Tanzania: Xpert")),
           category = factor(category, 
                             levels = c("equipment", "overhead", "labor", "consumables"), 
                             labels = c("Equipment", "Overhead", "Labor", "Consumables"))
    )
           
#----- figure 3 panel A: absolute costs ($) -----
p_abs <- groups %>%
    group_by(country, group, category) %>%
    summarise(
        cost_per_test = median(cost_per_test, na.rm = TRUE),
        total_cost_per_test = median(total_cost_per_test, na.rm = TRUE),
        percent = 100 * cost_per_test / total_cost_per_test,
        .groups = "drop") %>%
    ggplot(., aes(group, cost_per_test, fill = category)) +
    geom_bar(stat = "identity", color = "black") +
    geom_text(aes(group, label = ifelse(cost_per_test > 3, paste0("$", round(cost_per_test, 2)), "")), 
              position = position_stack(vjust = 0.5), 
              size = 2.5, 
              color = "black") +
    geom_text(aes(group, total_cost_per_test + 3, label = paste0("Total: $", round(total_cost_per_test, 2))), color = "black", size = 2.5) +
    facet_wrap(~ country, ncol = 1, scales = "free_y") +
    scale_y_continuous(breaks = seq(0, 60, by = 10)) +
    scale_fill_manual(values = c(pal28[11], pal28[7], "grey70", "grey95")) +
    #coord_flip() +
    labs(x = NULL, y = "Cost per test (2020 US$)", fill = "Cost category") +
    NULL

#----- figure 3 panel B: proportions (100% stacked) -----
p_pct <- groups %>%
    group_by(country, group, category) %>%
    summarise(
        cost_per_test = median(cost_per_test, na.rm = TRUE),
        total_cost_per_test = median(total_cost_per_test, na.rm = TRUE),
        percent = 100 * cost_per_test / total_cost_per_test,
        .groups = "drop") %>%
    ggplot(., aes(group, percent, fill = category)) +
    geom_bar(stat = "identity", color = "black") +
    geom_text(aes(group, label = ifelse(percent > 10, paste0(round(percent), "%"), "")), 
              position = position_stack(vjust = 0.5), 
              size = 2.5, 
              color = "black") +
    facet_wrap(~ country, ncol = 1, scales = "free_y") +
    scale_fill_manual(values = c(pal28[11], pal28[7], "grey70", "grey95")) +
    #coord_flip() +
    labs(x = NULL, y = "Proportion of total cost per test (%)", fill = "Cost category") +
    NULL

#----- figure 3: combine panels and save -----
p_abs + p_pct + plot_layout(guides = "collect") + plot_annotation(tag_levels = "A") & theme(plot.tag = element_text(size = 9.5))

ggsave("/figures/fig3_r2.png",
       path = path_sub_results,
       width = 173,
       height = 150,
       units = "mm",
       dpi = 300)




