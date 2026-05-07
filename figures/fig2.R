# fig 2 
## poc eid within 8 weeks + art initiation within 1 week

#----- fig 2A: poc eid within 8 weeks -----
# by studyarm
outcomes_eid_uptake_8wk %>%
    mutate(studyarm = factor(studyarm, levels = c("SoC", "VEID"), labels = c("SoC\n(N=2791;\n84.4%)", "VEID\n(N=3293;\n100%)"))) %>%
    ggplot(.) +
    geom_violin(aes(studyarm, age_eid, fill = studyarm), color = NA, width = 2) +
    geom_boxplot(aes(studyarm, age_eid), width = 0.25) +
    #geom_hline(yintercept = 8, linetype = "dotted", size = 0.7) +
    scale_y_continuous(trans = "log1p", limits = c(0, 18), breaks = seq(0, 18, by = 2)) +
    scale_fill_manual(values = c("#0541fe", "#d800fd")) +
    coord_flip() +
    theme(legend.position = "none", axis.text = element_text(size = 12, color = "black")) +
    labs(x = "", y = "Age (weeks) at first EID test") +
    #facet_wrap(~ country, ncol = 1) +
    NULL

ggsave("/fig2a_timing_first_EID.png", 
       path = paste0(path_sub_results, "/figures"),
       width = 2000,
       height = 1200,
       units = "px",
       dpi = 300)


#----- fig 2B -----
# by studyarm
## exact timing of ART initiation not available due to confidentiality
#outcomes_art_init_1wk %>%
#    filter(hiv_pos == 1) %>%
#    ggplot(., aes(studyarm, age_art)) +
#    geom_violin(aes(fill = studyarm), color = NA, width = 0.5) +
#    geom_boxplot(width = 0.2) + 
#    scale_x_discrete(limits = rev(levels(outcomes$studyarm)), labels = c("SoC\n(N=50\n89.3%)", "VEID\n(N=68;\n98.5%)")) +
#    scale_y_continuous(limits = c(0, 18), breaks = seq(0, 18, by = 2)) +
#    scale_fill_manual(values = c("#d800fd", "#0541fe")) +
#    coord_flip() +
#    theme(legend.position = "none", axis.text = element_text(size = 12, color = "black")) +
#    labs(x = "", y = "Age (weeks) at ART initiation") +
#    NULL

#ggsave("timing_art.png",
#       path = paste0(path_sub_results, "/desc_outcomes"),
#       width = 2000,
#       height = 1200,
#       units = "px",
#      dpi = 300)

# NOTE: figure 2 formatted in biorender