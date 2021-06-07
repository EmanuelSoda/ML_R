plot_Scatterplot <- function (pca_data){

plot <- pca_data %>% mutate(type = if_else(str_detect(class, regex(pattern = "t-")),
                                           true = "Trisomic",
                                           false = "Control")) %>%
  ggplot(., aes(x=PC1, y=PC2, shape=type, color=class)) +
  geom_point(size=3) +
  geom_rug() +
  scale_color_brewer(palette="Set3") +
  theme_minimal()

xdensity <- pca_data %>% mutate(type = if_else(str_detect(class, regex(pattern = "t-")),
                                               true = "Trisomic",
                                               false = "Control")) %>%
  ggplot(., aes(PC1, fill=class)) +
  geom_density(alpha=.6) +
  scale_color_brewer(palette="Set3") + theme_minimal() +
  theme(legend.position = "none")

# Marginal density plot of y (right panel)
ydensity <- pca_data %>% mutate(type = if_else(str_detect(class, regex(pattern = "t-")),
                                               true = "Trisomic",
                                               false = "Control")) %>%
  ggplot(., aes(PC2, fill=class)) +
  geom_density(alpha=.56) +
  scale_color_brewer(palette="Set3") + theme_minimal() +
  theme(legend.position = "none")  + coord_flip()

blankPlot <- ggplot()+geom_blank(aes(1,1))+
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank()
  )


grid.arrange(xdensity, blankPlot, plot, ydensity,
             ncol=2, nrow=2, widths=c(4, 1.4), heights=c(1.4, 4))

}
