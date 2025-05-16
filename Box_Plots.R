library(ggpmisc)
library(dplyr)
library(ggplot2)
library(patchwork)

gmin_clean_new <- gmin_clean %>% filter(tree < 2000)

## only preburn 
preburn.gmin <- gmin_clean_new %>% filter(timing == "preburn")
mean(preburn.gmin$gmin)

## only postburn
postburn.gmin <- gmin_clean_new %>% filter(timing == "postburn")
mean(postburn.gmin$gmin)

## ttests
t.test(gmin ~ treatment, data = preburn.gmin)
t.test(gmin ~ treatment, data = postburn.gmin)

## boxplots
pre_burn_gmin_graph <- ggplot(preburn.gmin, aes(x = treatment, y = gmin, fill = treatment)) +
  geom_boxplot() +
  scale_fill_manual(values = c("burned" = "cornsilk3", "unburned" = "darkolivegreen4"))+
  theme_minimal()

post_burn_gmin_graph <- ggplot(postburn.gmin, aes(x = treatment, y = gmin, fill = treatment)) +
  geom_boxplot() +
  scale_fill_manual(values = c("burned" = "cornsilk3", "unburned" = "darkolivegreen4")) +
  theme_minimal()

pre_burn_gmin_graph + post_burn_gmin_graph + plot_annotation(
  title = "Gmin Pre- and Post- Fire in Burned and Unburned Controls",
  tag_levels = "A")

pre_burn_gmin_graph <- boxplot(gmin ~ treatment, data = preburn.gmin,
        col = c("cornsilk3", "darkolivegreen4"),
        ylab = "Gmin (mol*m^-2*s^-1)",
        xlab = "Treatment", 
        main = "Pre-fire gmin values for burned and unburned plots")
summary(preburn.gmin$gmin)
post_burn_gmin_graph <- boxplot(gmin ~ treatment, data = postburn.gmin,
        col = c("cornsilk3", "darkolivegreen4"),
        ylab = "Gmin (mol*m^-2*s^-1)",
        xlab = "Treatment", 
        main = "Post-fire gmin values for burned and unburned plots")

pre_burn_gmin_graph + post_burn_gmin_graph + plot_annotation(tag_levels = "A")

(pre_burn_gmin_graph + post_burn_gmin_graph) + 
  plot_annotation(title = "Pre and Post-fire burned and unburned gmin")+
  plot_annotation(tag_levels = A)
  
