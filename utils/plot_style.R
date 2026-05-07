# define style for plots

# utils/plot_style.R
# ------------------------------------------------------------------------------

# theme
theme_set(theme_classic() +
              theme(plot.title = element_text(size = 8.5, hjust = 0.5),
                    strip.text = element_text(color = "black", face = "bold", size = 8.5),
                    axis.text = element_text(color = "black", size = 8),
                    axis.title = element_text(color = "black", size = 8.5, face = "bold"),
                    legend.title = element_text(color = "black", size = 8.5),
                    legend.text = element_text(color = "black", size = 8)))


# palette for plots
pal28 <- colorRampPalette(c("#D800FD", "#6600CC", "#0D0AFE", "#0068FE", "#66c6e1", "#00E379", "#A9E500",
                            "#FFD900", "#FFA200", "#FF3600", "#FF004D"))(28)
#scales::show_col(pal28)

