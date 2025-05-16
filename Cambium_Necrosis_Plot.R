## cambium necrosis vs. temperature 
install.packages("ggpmisc")
library(ggpmisc)
library(dplyr)
library(tidyr)
library(plotly)
library(ggpmisc)

total.necrosis <- merge(cambium_gmin_clean, cambium_hydraulics_clean, by = c("tree", "branch"))
necrosis.temp <- merge(total.necrosis, max_temperatures, by = c("tree", "branch"))
necrosis.regression <- pivot_longer(necrosis.temp, cols = c(cell_viability.x, cell_viability.y), names_to = "source", values_to = "value")
necrosis <- merge(necrosis.regression, max.temp.data, by = c("tree", "branch"))

## unburned mean
unburned.necrosis <- necrosis %>% filter(Group == "unburned")
mean.unburned.necrosis <- mean(unburned.necrosis$value, na.rm = TRUE)
print(mean.unburned.necrosis)

## r squared
cn.r2 <- lm(value ~ temperature.x, data = necrosis)
cn_r_squared <- summary(cn.r2)$r.squared

## data transformations
necrosis$tree_num <- as.numeric(as.character(necrosis$tree))
necrosis$Group <- ifelse(necrosis$tree_num < 100, "burned", "unburned")
necrosis$Group <- factor(necrosis$Group, levels = c("burned", "unburned"))


cambium_necrosis_plot <- ggplot(data = necrosis, aes(x = temperature.x, y = value, color = Group)) +
  geom_point(size = 2) + 
  scale_color_manual(values = c("burned" = "coral4", "unburned" = "darkslategray4")) +
  geom_smooth(aes(group = 1), method = "lm", color = "darkgoldenrod", fill = "cornsilk3", level = 0.95) +  # Regression line with confidence interval
  geom_hline(yintercept = mean.unburned.necrosis, linetype = "dashed", color = "darkslategrey")+
  stat_poly_eq(aes(label = after_stat(paste(eq.label, p.value.label, sep = "~~~")), group = 1), 
               formula = y ~ x, 
               parse = TRUE) +
  annotate("text", x = 30, y = 2.6, label = "Unburned Mean = 0.918", 
           size = 4, color = "darkslategrey") +
  labs(title = "Cell Viability Across Temperatures", 
       x = "Temperature (°C)", 
       y = "Cell Viability") +
  theme_minimal()
print(cambium_necrosis_plot)

## to identify where confidence interval diverges from mean
ggplotly(cambium_necrosis_plot)
ggidentify(cambium_necrosis_plot)

## code for equation
##   stat_poly_eq(aes(label = after_stat(paste(eq.label, p.value.label, sep = "~~~")), group = 1), 
## formula = y ~ x, 
## parse = TRUE) +
