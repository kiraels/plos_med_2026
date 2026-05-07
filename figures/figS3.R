# figS3
## one-way sensitivity analysis for cost per test by sitegi

plot_tornado(tornado_data) + 
    facet_wrap(~ cluster_num, ncol = 7) +
    scale_y_continuous(breaks = seq(-60, 60, by = 20))

ggsave("/figures/figS3.png",
       path = path_sub_results,
       height = 3400,
       width = 4000,
       units = "px",
       dpi = 300
)
