## gmin vs. temperature 
library(ggpmisc)
library(dplyr)
library(ggplot2)
library(plotly)
library(ggpmisc)

gmin.temp <- merge(max.temp.data, gmin_clean, by = c("tree", "branch"))

## unburned mean
unburned.gmin <- gmin.temp %>% filter(treatment == "unburned")
mean.unburned.gmin <- mean(unburned.gmin$gmin, na.rm = TRUE)
print(mean.unburned.gmin)

## R^2 
gmin.model <- lm(gmin ~ temperature, data = gmin.temp)
summary(gmin.model)$r.squared

## variable transformations
gmin.temp$tree_num <- as.numeric(as.character(gmin.temp$tree))
gmin.temp$Treatment <- ifelse(gmin.temp$tree_num < 100, "burned", "unburned")
gmin.temp$Treatment <- factor(gmin.temp$Treatment, levels = c("burned", "unburned"))

## plot 
gmin_plot <- ggplot(data = gmin.temp, aes(x = temperature, y = gmin, color = Treatment)) +
  geom_point(size = 2) +  # Scatter plot
  scale_color_manual(values = c("burned" = "coral4", "unburned" = "darkslategray4")) +
  geom_smooth(aes(group = 1), method = "lm", color = "darkgoldenrod", fill = "cornsilk3", level = 0.95) +  # Regression line with confidence interval
  stat_poly_eq(
    aes(label = after_stat(paste(eq.label, p.value.label, sep = "~~~")), group = 1),
    formula = y ~ x,
    parse = TRUE,
    color = "darkgoldenrod"
  ) +
  annotate("text", x = 29, y = 11.25, label = "Unburned Mean = 0.889", 
           size = 4, color = "darkslategrey") +
  geom_hline(yintercept = mean.unburned.gmin, linetype = "dashed", color = "darkslategrey")+
  labs(title = "Cuticular Conductance Across Temperatures", 
       x = "Temperature (°C)", 
       y = "Gmin (mol*m^-2*s^-1)") +
  theme_minimal()

print(gmin_plot)

## to identify where confidence interval diverges from mean
ggplotly(gmin_plot)
ggidentify(cambium_necrosis_plot)




## code for equation
## stat_poly_eq(aes(label = paste(..eq.label.., ..p.value.label.., sep = "~~~")), 
## formula = y ~ x, 
## parse = TRUE) +  # Display equation

##annotate("text", x = 26, y = 11.25, label = "Unburned Mean = 1.001", 
        ## size = 4, color = "darkslategrey") +
 ## annotate("text", x = 25, y = 12, label = "y = 0.355 + 0.051*x  p = 0.007", 
        ##   size = 4, color = "darkslategrey")+
