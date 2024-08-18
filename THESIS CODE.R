library(ggplot2)
library(dplyr)
library(readxl)
library(scales)
library(tidyr)
library(RColorBrewer)
# load data
data <- read_excel("C:/Users/xiao/Desktop/data_R.xlsx")
data
data <- data %>%
  filter(Day >= 1 & Day <= 11)



# Convert data to long format for easier plotting
data_long <- data %>%
  pivot_longer(
    cols = -Day,
    names_to = "Variable",
    values_to = "Value"
  ) %>%
  mutate(
    Category = factor(case_when(
      grepl("Cr[1-4]$", Variable) ~ "Low Si",
      grepl("Cr[5-8]$", Variable) ~ "Medium Si",
      grepl("Cr(9|10|11|12)$", Variable) ~ "High Si"
    ), levels = c("Low Si", "Medium Si", "High Si")),
    Variable = factor(Variable, levels = c("Cr1", "Cr2", "Cr3", "Cr4", "Cr5", "Cr6", "Cr7", "Cr8", "Cr9", "Cr10", "Cr11", "Cr12"))
  )

# Calculate the daily average for each category
data_summary <- data_long %>%
  group_by(Day, Category, Variable) %>%
  summarise(
    Mean = mean(Value, na.rm = TRUE),
    .groups = 'drop'
  )

# choose the colors
colors <- c("red", "orange", "gold", "green", "blue", "purple", "turquoise", "magenta", "limegreen", "cyan", "pink", "black")

# draw the plots
p1 <- ggplot(data_summary, aes(x = Day, y = Mean, group = Variable, color = Variable)) +
  geom_point() +  
  geom_line(linetype = "dashed") + 
  facet_wrap(~Category, scales = "free_y", ncol = 1) +
  scale_color_manual(values = setNames(colors, levels(data_long$Variable))) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(breaks = 1:11, labels = 1:11) +
  labs(title = "(a) Growth condition of CR in Silicon Level", x = "Day", y = "Measurement") +
  theme_minimal() +
  theme(
    text = element_text(family = "Times New Roman"),  
    axis.text.x = element_text(angle = 90, hjust = 1),
    strip.background = element_blank(),
    strip.text.x = element_text(size = 12)
  )

# show the plots
print(p1)



data <- read_excel("C:/Users/xiao/Desktop/data_tw.xlsx")
data

# Convert data to long format for easier plotting
data_long <- data %>%
  pivot_longer(
    cols = -Day,
    names_to = "Variable",
    values_to = "Value"
  ) %>%
  mutate(
    Category = factor(case_when(
      grepl("Tw[1-4]$", Variable) ~ "Low Si",
      grepl("Tw[5-8]$", Variable) ~ "Medium Si",
      grepl("Tw(9|10|11|12)$", Variable) ~ "High Si"
    ), levels = c("Low Si", "Medium Si", "High Si")),
    Variable = factor(Variable, levels = c("Tw1", "Tw2", "Tw3", "Tw4", "Tw5", "Tw6", "Tw7", "Tw8", "Tw9", "Tw10", "Tw11", "Tw12"))
  )

# Calculate the daily average for each category
data_summary <- data_long %>%
  group_by(Day, Category, Variable) %>%
  summarise(
    Mean = mean(Value, na.rm = TRUE),
    .groups = 'drop'
  )

# choose the color
colors <- c("red", "orange", "gold", "green", "blue", "purple", "turquoise", "magenta", "limegreen", "cyan", "pink", "black")

# draw the plots
p2 <- ggplot(data_summary, aes(x = Day, y = Mean, group = Variable, color = Variable)) +
  geom_point() +  
  geom_line(linetype = "dashed") +  
  facet_wrap(~Category, scales = "free_y", ncol = 1) +
  scale_color_manual(values = setNames(colors, levels(data_long$Variable))) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(breaks = seq(min(data$Day), max(data$Day), by = 1)) +
  labs(title = "(b) Growth condition of TW in Silicon Level", x = "Day", y = "Measurement") +
  theme_minimal() +
  theme(text = element_text(family = "Times New Roman"), 
    axis.text.x = element_text(angle = 90, hjust = 1),
        strip.background = element_blank(),
        strip.text.x = element_text(size = 12))

# show the plots
print(p2)

library(cowplot)
# Use the `cowplot` package to arrange two plots vertically
combined_plot <- plot_grid(p1, p2, ncol = 1)

# Display the combined plots
print(combined_plot)

# Save the plot with a resolution of 300 DPI and size 
ggsave("combined_plot_A4.png", plot = combined_plot, width = 8.27, height = 11.69, dpi = 300)


library(readxl)
library(dplyr)
library(ggplot2)
data <- read_excel("C:/Users/xiao/Desktop/data_tw.xlsx")


# Convert data to long format for easier plotting
data_long <- data %>%
  pivot_longer(
    cols = -Day,  
    names_to = "Variable",
    values_to = "Biomass"
  )

# Calculate the maximum biomass for each treatment
max_biomass <- data_long %>%
  group_by(Variable) %>%
  summarise(
    MaxBiomass = max(Biomass, na.rm = TRUE)
  ) %>%
  ungroup()

# Ensure that the order is arranged from Tw1 to Tw12
max_biomass$Variable <- factor(max_biomass$Variable, levels = c("Tw1", "Tw2", "Tw3", "Tw4", "Tw5", "Tw6", "Tw7", "Tw8", "Tw9", "Tw10", "Tw11", "Tw12"))

# Create a bar plot
p <- ggplot(max_biomass, aes(x = Variable, y = MaxBiomass, fill = Variable)) +
  geom_bar(stat = "identity", color = "black") +
  labs(title = "Maximum Biomass of TW by Treatment", x = "Treatment", y = "Maximum Biomass") +
  scale_y_continuous(labels = scales::comma) +  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  

# Display the plot
print(p)


library(readxl)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
data <- read_excel("C:/Users/xiao/Desktop/data_R.xlsx")

# Convert data to long format 
data_long <- data %>%
  pivot_longer(
    cols = -Day,  
    names_to = "Variable",
    values_to = "Biomass"
  )

# Calculate the maximum biomass for each treatment
max_biomass <- data_long %>%
  group_by(Variable) %>%
  summarise(
    MaxBiomass = max(Biomass, na.rm = TRUE)
  ) %>%
  ungroup()

# Ensure the data is ordered sequentially from Tw1 to Tw12
max_biomass$Variable <- factor(max_biomass$Variable, levels = c("Cr1", "Cr2", "Cr3", "Cr4", "Cr5", "Cr6", "Cr7", "Cr8", "Cr9", "Cr10", "Cr11", "Cr12"))

# Create a bar plot
p <- ggplot(max_biomass, aes(x = Variable, y = MaxBiomass, fill = Variable)) +
  geom_bar(stat = "identity", color = "black") +
  labs(title = "Maximum Biomass of CR by Treatment", x = "Treatment", y = "Maximum Biomass") +
  scale_y_continuous(labels = scales::comma) +  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  

# display the plot
print(p)


##### analysis
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)

# load the data
data_merge <- read_excel("C:/Users/xiao/Desktop/merge_data.xlsx")
treatment_data <- read_excel("C:/Users/xiao/Desktop/treatment.xlsx") 
data_merge


# Convert data to long format
data_long <- data_merge %>%
  pivot_longer(cols = -Day, names_to = "Measurement", values_to = "Value")

# To match, directly copy the values from the Measurement column to the CODE column
data_long <- data_long %>%
  mutate(CODE = Measurement)

# Check the column names
print(names(data_long)) 
print(names(treatment_data)) 

# Rename the 'code' column in treatment_data to 'CODE'
treatment_data <- treatment_data %>%
  rename(CODE = code)

# Merge the data
data_combined <- merge(data_long, treatment_data, by = "CODE", all.x = TRUE)

# Inspect the merged data
print(head(data_combined))

# clean the NA value
data_clean <- data_combined %>%
  drop_na()

# View the cleaned data
print(head(data_clean))

# Take the logarithm of the numerical values
data_clean <- data_clean %>%
  mutate(Value = log(Value))

# Separate the CR and TW data
cr_data <- data_clean %>% filter(grepl("^CR", Measurement))
tw_data <- data_clean %>% filter(grepl("^TW", Measurement))

# Perform regression analysis on the CR data
cr_model <- lm(Value ~ Si + Fe + Li + Day, data = cr_data)
cr_summary <- summary(cr_model)

# Perform regression analysis on the TW data
tw_model <- lm(Value ~ Si + Fe + Li + Day, data = tw_data)
tw_summary <- summary(tw_model)

# Print the regression analysis results
print(cr_summary)
print(tw_summary)



####################The effect of Fe, Si and Li on growth rate.
library(dplyr)

# Calculate the growth rate by grouping by 'Measurement' and computing the difference between adjacent days
data_growth <- data_clean %>%
  group_by(Measurement) %>%
  arrange(Day) %>%
  mutate(GrowthRate = Value - lag(Value)) %>%
  filter(!is.na(GrowthRate))  

# Separate the CR and TW data
cr_growth <- data_growth %>% filter(grepl("^CR", Measurement))
tw_growth <- data_growth %>% filter(grepl("^TW", Measurement))

# Perform regression analysis on the CR data
cr_growth_model <- lm(GrowthRate ~ Si + Fe + Li+ Day, data = cr_growth)
cr_growth_summary <- summary(cr_growth_model)

# Perform regression analysis on the TW data
tw_growth_model <- lm(GrowthRate ~ Si + Fe + Li + Day, data = tw_growth)
tw_growth_summary <- summary(tw_growth_model)

# Print the result
print(cr_growth_summary)
print(tw_growth_summary)



# Load necessary packages
if (!require(ggplot2)) install.packages("ggplot2")

library(ggplot2)
library(readxl)
library(dplyr)
library(tidyr)

data <- read_excel("C:/Users/xiao/Desktop/merge_data.xlsx")

library(ggplot2)
library(dplyr)
library(gridExtra)

# "Convert data to long format
data_long <- data %>%
  pivot_longer(cols = -Day, names_to = "Measurement", values_to = "Value")

# To avoid issues in logarithmic calculations, replace zero values with a very small positive number
data_long <- data_long %>%
  mutate(Value = ifelse(Value == 0, 1e-8, Value))

# Take the logarithm
data_long <- data_long %>%
  mutate(LogValue = log(Value))

# Calculate the daily average
data_mean <- data_long %>%
  group_by(Day, Measurement) %>%
  summarise(MeanValue = mean(LogValue), .groups = 'drop')

# Remove the declining data points
data_filtered <- data_mean %>%
  group_by(Measurement) %>%
  filter(MeanValue >= lag(MeanValue, default = first(MeanValue)))

# Calculate the growth rate
data_growth <- data_filtered %>%
  group_by(Measurement) %>%
  arrange(Day) %>%
  mutate(GrowthRate = (MeanValue - first(MeanValue)) / (Day - first(Day)))

# Separate the CR and TW data
cr_growth <- data_growth %>% filter(grepl("^CR", Measurement))
tw_growth <- data_growth %>% filter(grepl("^TW", Measurement))

# Use a color scheme
new_colors <- c("Low Si" = "#F8766D",   
                "Medium Si" = "#00BA38",  
                "High Si" = "#619CFF")    

# Calculate the average growth rate and standard deviation for the CR data
cr_summary <- cr_growth %>%
  summarise(
    MeanGrowth = mean(GrowthRate, na.rm = TRUE),
    SD = sd(GrowthRate, na.rm = TRUE)
  ) %>%
  mutate(Measurement = factor(Measurement, levels = paste0("CR", 1:12)))

# Calculate the average growth rate and standard deviation for the TW data."
tw_summary <- tw_growth %>%
  summarise(
    MeanGrowth = mean(GrowthRate, na.rm = TRUE),
    SD = sd(GrowthRate, na.rm = TRUE)
  ) %>%
  mutate(Measurement = factor(Measurement, levels = paste0("TW", 1:12)))

new_labels <- c(
  "CR1" = "High Li, High Fe", "CR2" = "High Li, Low Fe", "CR3" = "Low Li, High Fe", "CR4" = "Low Li, Low Fe",
  "CR5" = "High Li, High Fe", "CR6" = "High Li, Low Fe", "CR7" = "Low Li, High Fe", "CR8" = "Low Li, Low Fe",
  "CR9" = "High Li, High Fe", "CR10" = "High Li, Low Fe", "CR11" = "Low Li, High Fe", "CR12" = "Low Li, Low Fe",
  "TW1" = "High Li, High Fe", "TW2" = "High Li, Low Fe", "TW3" = "Low Li, High Fe", "TW4" = "Low Li, Low Fe",
  "TW5" = "High Li, High Fe", "TW6" = "High Li, Low Fe", "TW7" = "Low Li, High Fe", "TW8" = "Low Li, Low Fe",
  "TW9" = "High Li, High Fe", "TW10" = "High Li, Low Fe", "TW11" = "Low Li, High Fe", "TW12" = "Low Li, Low Fe"
)

# Create a bar plot for the CR data
p_cr <- ggplot(cr_summary, aes(x = Measurement, y = MeanGrowth, fill = cut(as.numeric(Measurement), breaks = 3, labels = c("Low Si", "Medium Si", "High Si")))) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = MeanGrowth - SD, ymax = MeanGrowth + SD), width = 0.2, color = "gray40") +
  scale_fill_manual(values = new_colors) +
  scale_x_discrete(labels = new_labels) + 
  labs(title = "(a) CR Growth Rate Constant", x = "Condition", y = "Growth Rate Constant") +
  theme_minimal() +
  theme(text = element_text(family = "Times New Roman"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_blank())  

# Create a bar plot for the tw data
p_tw <- ggplot(tw_summary, aes(x = Measurement, y = MeanGrowth, fill = cut(as.numeric(Measurement), breaks = 3, labels = c("Low Si", "Medium Si", "High Si")))) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = MeanGrowth - SD , ymax = MeanGrowth + SD ), width = 0.2, color = "gray40") +
  scale_fill_manual(values = new_colors) +
  scale_x_discrete(labels = new_labels) +  
  labs(title = "(b) TW Growth Rate Constant", x = "Condition", y = "Growth Rate Constant") +
  theme_minimal() +
  theme(text = element_text(family = "Times New Roman"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_blank())  

# Display the two plots side by side horizontally
p_combined <- grid.arrange(p_cr, p_tw, ncol = 2)

# Save the image as a high-resolution file
ggsave("combined_growth_rate_with_conditions1.png", plot = p_combined, width = 14, height = 7, units = "in", dpi = 300)






###########
library(ggplot2)
library(gridExtra)
library(extrafont)
install.packages("extrafont")

# read the data
data <- read.csv("C:/Users/xiao/Desktop/percentage.csv")
loadfonts(device = "win")

# Examine the data structure
str(data)

# Load the necessary packages.
install.packages("tidyverse")
library(tidyverse)

# Convert the data from wide format to long format
data_long <- data %>%
  pivot_longer(cols = -Element, 
               names_to = c("Group", "Measurement"),
               names_pattern = "(.*)_(\\d+)",
               values_to = "Percentage") %>%
  mutate(Measurement = as.factor(Measurement))

head(data_long)

# Calculate descriptive statistics: mean and standard deviation
summary_stats <- data_long %>%
  group_by(Group, Element) %>%
  summarise(
    mean_value = mean(Percentage, na.rm = TRUE),
    sd_value = sd(Percentage, na.rm = TRUE),
    .groups = "drop"
  )

# Review the descriptive statistics results
print(summary_stats)

# Create a bar plot with error bars
ggplot(summary_stats, aes(x = Group, y = mean_value, fill = Element)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = mean_value - sd_value, ymax = mean_value + sd_value),
                width = 0.2, position = position_dodge(0.9)) +
  facet_wrap(~Element, scales = "free_y") +
  labs(title = "Elemental Percentage with Error Bars by Group",
       x = "Group",
       y = "Mean Percentage") +
  theme_minimal()

# Create a scatter plot with error bars
ggplot(summary_stats, aes(x = Group, y = mean_value, color = Element)) +
  geom_point(position = position_dodge(width = 0.75), size = 3) +
  geom_errorbar(aes(ymin = mean_value - sd_value, ymax = mean_value + sd_value),
                width = 0.2, position = position_dodge(0.75)) +
  facet_wrap(~Element, scales = "free_y") +
  labs(title = "Elemental Percentage Scatterplot with Error Bars by Group",
       x = "Group",
       y = "Mean Percentage") +
  theme_minimal()


# Create a grouped bar plot with error bars and overlaid dot plot
ggplot(summary_stats, aes(x = Group, y = mean_value, fill = Element)) +
  geom_col(position = position_dodge(width = 0.75)) +
  geom_errorbar(aes(ymin = mean_value - sd_value, ymax = mean_value + sd_value),
                width = 0.2, position = position_dodge(width = 0.75)) +
  geom_point(data = data_long, aes(x = Group, y = Percentage, color = Element), 
             position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.75), size = 2) +
  labs(title = "Elemental Composition by Group",
       x = "Group",
       y = "Percentage") +
  theme_minimal() +
  theme(legend.position = "top")

# Calculate the mean of the total amount of elements for each experimental group
summary_stats <- data_long %>%
  group_by(Group, Element) %>%
  summarise(
    mean_value = mean(Percentage, na.rm = TRUE),
    .groups = "drop"
  )

# Create a stacked bar plot
ggplot(summary_stats, aes(x = Group, y = mean_value, fill = Element)) +
  geom_bar(stat = "identity") +
  labs(title = "Elemental Composition by Group (Stacked Bar Chart)",
       x = "Group",
       y = "Mean Percentage") +
  theme_minimal() +
  theme(legend.position = "top")



# Calculate the mean total amount of elements for each experimental group
summary_stats <- data_long %>%
  group_by(Group, Element) %>%
  summarise(
    mean_value = mean(Percentage, na.rm = TRUE),
    .groups = "drop"
  )

# Define a new label mapping without adding annotations for Si
new_labels <- c(
  "CR1" = "High Li, High Fe", "CR2" = "High Li, Low Fe", 
  "CR3" = "Low Li, High Fe", "CR4" = "Low Li, Low Fe",
  "CR9" = "High Li, High Fe", "CR10" = "High Li, Low Fe", 
  "CR11" = "Low Li, High Fe", "CR12" = "Low Li, Low Fe",
  "TW1" = "High Li, High Fe", "TW2" = "High Li, Low Fe", 
  "TW3" = "Low Li, High Fe", "TW4" = "Low Li, Low Fe",
  "TW9" = "High Li, High Fe", "TW10" = "High Li, Low Fe", 
  "TW11" = "Low Li, High Fe", "TW12" = "Low Li, Low Fe"
)

# Create a stacked bar plot for the CR group and display percentages
p1 <- ggplot(filter(summary_stats, str_detect(Group, "CR")), 
             aes(x = factor(Group, levels = c("CR1", "CR2", "CR3", "CR4", "CR9", "CR10", "CR11", "CR12")), 
                 y = mean_value, fill = Element)) +
  geom_bar(stat = "identity", width = 0.5) +  
  geom_text(aes(label = scales::percent(mean_value, accuracy = 0.1)),
            position = position_stack(vjust = 0.5), size = 3, color = "white", family = "Times New Roman") +
  labs(title = "(a) Elemental Composition in CR Group ",
       x = "Group",
       y = "Percentage") +
  scale_x_discrete(labels = new_labels) +  
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +  
  theme_minimal() +
  theme(text = element_text(family = "Times New Roman"),
        axis.text.x = element_text(angle = 45, hjust = 1, family = "Times New Roman"),  
        legend.position = "bottom") +
  geom_vline(xintercept = 4.5, linetype = "dashed", color = "black") +  
  annotate("text", x = 2.5, y = 1.05, label = "Low Si", size = 4, fontface = "plain", family = "Times New Roman", hjust = 0.5) +  # 标注“低硅”
  annotate("text", x = 6.5, y = 1.05, label = "High Si", size = 4, fontface = "plain", family = "Times New Roman", hjust = 0.5)  # 标注“高硅”

# Create a stacked bar plot for the TW group and display percentages
p2 <- ggplot(filter(summary_stats, str_detect(Group, "TW")), 
             aes(x = factor(Group, levels = c("TW1", "TW2", "TW3", "TW4", "TW9", "TW10", "TW11", "TW12")), 
                 y = mean_value, fill = Element)) +
  geom_bar(stat = "identity", width = 0.5) +  
  geom_text(aes(label = scales::percent(mean_value, accuracy = 0.1)),
            position = position_stack(vjust = 0.5), size = 3, color = "white", family = "Times New Roman") +
  labs(title = "(b) Elemental Composition in TW Group",
       x = "Group",
       y = "Percentage") +
  scale_x_discrete(labels = new_labels) +  
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +  
  theme_minimal() +
  theme(text = element_text(family = "Times New Roman"),
        axis.text.x = element_text(angle = 45, hjust = 1, family = "Times New Roman"),  
        legend.position = "bottom") +
  geom_vline(xintercept = 4.5, linetype = "dashed", color = "black") +  
  annotate("text", x = 2.5, y = 1.05, label = "Low Si", size = 4, fontface = "plain", family = "Times New Roman", hjust = 0.5) +  # 标注“低硅”
  annotate("text", x = 6.5, y = 1.05, label = "High Si", size = 4, fontface = "plain", family = "Times New Roman", hjust = 0.5)  # 标注“高硅”

p1 <- p1 + scale_y_continuous(labels = function(x) paste0(x * 100, "%"))
p2 <- p2 + scale_y_continuous(labels = function(x) paste0(x * 100, "%"))
p_combined <- grid.arrange(p1, p2, ncol = 2)
p_combined
ggsave("1combined_elemental_composition.png", plot = p_combined, width = 14, height = 10, units = "in", dpi = 300)





library(tidyverse)
percentage_data <- read_csv("C:/Users/xiao/Desktop/percentage.csv")
treatment_data <- read_excel("C:/Users/xiao/Desktop/treatment2.xlsx")
library(dplyr)
library(tidyr)
library(readr)
library(readxl)


# Convert percentage_data to long format
percentage_long <- percentage_data %>%
  pivot_longer(cols = -Element, names_to = "Measurement", values_to = "Percentage")

# Separate the numerical part from the measurement column
percentage_long <- percentage_long %>%
  separate(col = Measurement, into = c("Measurement", "Time"), sep = "_")

# Convert treatment_data to long format for easier merging
treatment_long <- treatment_data %>%
  pivot_longer(cols = -code, names_to = "Measurement", values_to = "TreatmentValue")

# Merge the treatment information into percentage_long
data_combined <- percentage_long %>%
  left_join(treatment_long, by = "Measurement")

# Split the treatment information into separate columns (Si, Fe, Li)
data_combined_wide <- data_combined %>%
  pivot_wider(names_from = code, values_from = TreatmentValue, names_prefix = "")

# Recombine the Measurement and Time columns
data_combined_wide <- data_combined_wide %>%
  unite(col = "Measurement", c("Measurement", "Time"), sep = "_")

# Remove the column containing NA values
data_combined_wide <- data_combined_wide %>%
  select(-`NA`)


# Inspect the final data
print(data_combined_wide)


library(dplyr)
# Separate the CR and TW data
cr_data <- data_combined_wide %>%
  filter(grepl("^CR", Measurement))

tw_data <- data_combined_wide %>%
  filter(grepl("^TW", Measurement))

# Perform regression analysis on the CR data
# O% 
cr_o_model <- lm(Percentage ~ Si + Fe + Li, data = cr_data %>% filter(Element == "O%"))
cr_o_summary <- summary(cr_o_model)

# Fe% 
cr_fe_model <- lm(Percentage ~ Si + Fe + Li, data = cr_data %>% filter(Element == "Fe%"))
cr_fe_summary <- summary(cr_fe_model)

# Si% 
cr_si_model <- lm(Percentage ~ Si + Fe + Li, data = cr_data %>% filter(Element == "Si%"))
cr_si_summary <- summary(cr_si_model)

# Perform regression analysis on the TW data
# O% 
tw_o_model <- lm(Percentage ~ Si + Fe + Li, data = tw_data %>% filter(Element == "O%"))
tw_o_summary <- summary(tw_o_model)

# Fe% 
tw_fe_model <- lm(Percentage ~ Si + Fe + Li, data = tw_data %>% filter(Element == "Fe%"))
tw_fe_summary <- summary(tw_fe_model)

# Si% 
tw_si_model <- lm(Percentage ~ Si + Fe + Li, data = tw_data %>% filter(Element == "Si%"))
tw_si_summary <- summary(tw_si_model)

print(cr_o_summary)
print(cr_fe_summary)
print(cr_si_summary)

print(tw_o_summary)
print(tw_fe_summary)
print(tw_si_summary)

library(car)
library(lmtest)
library(ggplot2)

check_regression_assumptions <- function(model, model_name) {
par(mfrow = c(2, 2))  
  
# Check for normality of residuals
qqnorm(residuals(model), main = paste("Q-Q Plot for", model_name))
qqline(residuals(model), col = "red")
shapiro_test <- shapiro.test(residuals(model))
cat(paste("Shapiro-Wilk Test p-value for", model_name, ":", shapiro_test$p.value, "\n"))
  
# Check for homoscedasticity
plot(model$fitted.values, residuals(model), 
       main = paste("Residuals vs Fitted for", model_name), 
       xlab = "Fitted values", 
       ylab = "Residuals")
  abline(h = 0, col = "red")
  bp_test <- bptest(model)
  cat(paste("Breusch-Pagan Test p-value for", model_name, ":", bp_test$p.value, "\n"))
  
# Durbin-Watson 
dw_test <- dwtest(model)
cat(paste("Durbin-Watson Test p-value for", model_name, ":", dw_test$p.value, "\n"))
  
 # VIF
vif_values <- vif(model)
cat(paste("VIF values for", model_name, ":\n"))
print(vif_values)
  
par(mfrow = c(1, 1))  
}

cat("CR O% Model:\n")
check_regression_assumptions(cr_o_model, "CR O% Model")

cat("\nCR Fe% Model:\n")
check_regression_assumptions(cr_fe_model, "CR Fe% Model")

cat("\nCR Si% Model:\n")
check_regression_assumptions(cr_si_model, "CR Si% Model")

cat("\nTW O% Model:\n")
check_regression_assumptions(tw_o_model, "TW O% Model")

cat("\nTW Fe% Model:\n")
check_regression_assumptions(tw_fe_model, "TW Fe% Model")

cat("\nTW Si% Model:\n")
check_regression_assumptions(tw_si_model, "TW Si% Model")


