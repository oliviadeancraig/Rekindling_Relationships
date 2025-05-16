install.packages("ggplot2")
install.packages("ggpmisc")
install.packages("dplyr")
library(ggplot2)
library(ggpmisc)
library(dplyr)

max_temperatures$tree_num <- as.numeric(as.character(max.temp.data$tree))

# Create a grouping variable based on tree number
max.temp.data$Group <- ifelse(max.temp.data$tree_num < 100, "burned", "unburned")
max.temp.data$Group <- factor(max.temp.data$Group, levels = c("burned", "unburned"))

# Plot
ggplot(max.temp.data, aes(x = tree_num, y = temperature, color = Group)) +
  geom_point(size = 2) +
  scale_color_manual(values = c("burned" = "coral4", "unburned" = "darkslategray4")) +
  labs(title = "Temperature Range of Samples", 
       x = "Sample Number", 
       y = "Maximum Temperature (°C)") +
  theme_minimal()
