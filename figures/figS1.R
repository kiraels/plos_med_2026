# fig S1
## poc eid within 8 weeks + art initiation within 1 week

#----- fig S1A -----
# by cluster
outcomes_eid_uptake_8wk %>%
    mutate(country_group = fct_relevel(country_group, "Mozambique: VEID", "Tanzania: VEID")) %>%
    ggplot(.) +
    geom_violin(aes(reorder(cluster_num, age_eid), age_eid, fill = studyarm), color = NA, width = 1.2) +
    geom_boxplot(aes(cluster_num, age_eid), width = 0.5) +
    #geom_hline(yintercept = 8, linetype = "dotted", size = 0.7) +
    scale_x_discrete(limits = rev) +
    scale_y_continuous(trans = "log1p", limits = c(0, 18), breaks = seq(2, 18, by = 2)) +
    scale_fill_manual(values = rep(c("#d800fd", "#0541fe"), 2)) +
    coord_flip() +
    facet_wrap(~ country_group, scales = "free_y") +
    theme(legend.position = "none", axis.text = element_text(size = 12, color = "black")) +
    labs(x = "", y = "Age (weeks) at first EID test") +
    NULL

ggsave("/figures/figS1a_timing_first_EID_cluster.png", 
       path = path_sub_results,
       width = 4000,
       height = 2000,
       units = "px",
       dpi = 300)


#----- fig S1B -----
## not available due to small sample sizes at cluster level