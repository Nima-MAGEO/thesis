# -------------------------------------------------------------------
# Libraries
# -------------------------------------------------------------------
library(readr)
library(dplyr)        # Data manipulation
library(stringr)      # String manipulation (str_replace_all, str_replace)
library(tidyr)        # Data tidying
library(ggplot2)      # Plotting and data visualization
library(fmsb)         # Radar charts
library(ggpubr)       # Publication-ready plots
library(dunn.test)    # Dunn's test for post-hoc analysis
library(e1071)        # Statistical functions and SVM
library(cowplot)      # Plot arrangement
library(forcats)      # Factor management
library(gridExtra)    # Arrange multiple grid-based plots
library(colorspace)   # Color manipulation
library(car)          # Regression diagnostics
library(lmtest)       # Statistical tests for linear models
library(MASS)         # Statistical functions and datasets
library(FSA)
library(grid)
# -------------------------------------------------------------------
# Set Folder Paths
# -------------------------------------------------------------------
main_folder   <- "C:/Users/nima/Desktop/data analysis/github"
data_folder   <- file.path(main_folder, "data")
output_folder <- file.path(main_folder, "output")

cat("Data Folder:   ", data_folder, "\n")
cat("Output Folder: ", output_folder, "\n")

# -------------------------------------------------------------------
# Load Data
# -------------------------------------------------------------------
data <- read.csv(file.path(data_folder, "cleaned_data.csv"))

# Display First Rows and Column Names
head(data)
colnames(data)

# -------------------------------------------------------------------
# Data Manipulation
# -------------------------------------------------------------------
# The data loaded here is already cleaned.
# All pre-processing steps were performed separately and are not included in this script.

# ===============================================================
# Chapter 01 - Get Familiar with Data: Overview of Participant Social Demographic Characteristics, 
# Environmental Preferences, and Related Analyses
# ===============================================================

# -------------------------------------------------------------------
# Section 01 - Get Familiar with Data: Gender Overview
# -------------------------------------------------------------------

# Step 01 - Calculate the number of participants by Campus and Gender
summary_data <- data %>%
  group_by(WhichCampuses, Gender) %>%
  summarise(Count = n(), .groups = "drop")

print(summary_data)

# -------------------------------------------------------------------
# Step 02 - Visualize the Data: Clustered Bar Chart
# -------------------------------------------------------------------

chart_gender <- ggplot(summary_data, aes(x = WhichCampuses, y = Count, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_text(
    aes(label = Count, colour = Gender),
    position = position_dodge(width = 0.7),
    vjust = -0.5, size = 3.5
  ) +
  scale_fill_manual(
    values = c("Female" = "#7696B7", "Male" = "#264463"),
    labels = c("Female", "Male"),
    name = "Gender"
  ) +
  scale_colour_manual(
    values = c("Female" = "#7696B7", "Male" = "#264463"),
    guide = "none"
  ) +
  labs(
    title = "Participant Count by Campus and Gender",
    x = "Campus",
    y = "Number of Participants"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "right",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    panel.background = element_rect(fill = "white", color = "white"),
    plot.background = element_rect(fill = "white", color = "white")
  )

# Print Chart
print(chart_gender)

# -------------------------------------------------------------------
# Save the Chart
# -------------------------------------------------------------------
chart_path <- file.path(output_folder, "participant_count_by_campus_gender.png")
ggsave(chart_path, plot = chart_gender, width = 8, height = 6)

cat("Chart saved at:", chart_path, "\n")

# -------------------------------------------------------------------
# -------------------------------------------------------------------
# Section 02 - Get Familiar with Data: Age Group Overview
# -------------------------------------------------------------------

# Step 01 - Calculate the number of participants by Campus and Age Group
summary_age <- data %>%
  group_by(WhichCampuses, Age) %>%
  summarise(Count = n(), .groups = "drop") %>%
  mutate(
    Legend = interaction(WhichCampuses, Age),
    Legend = factor(
      Legend,
      levels = c(
        "Belval Campus.18-25", "Belval Campus.26-33", "Belval Campus.34-39", "Belval Campus.40+",
        "Kirchberg Campus.18-25", "Kirchberg Campus.26-33", "Kirchberg Campus.34-39", "Kirchberg Campus.40+",
        "Limpertsberg Campus.18-25", "Limpertsberg Campus.26-33", "Limpertsberg Campus.34-39", "Limpertsberg Campus.40+"
      )
    )
  )

# Print Summary Data
print(summary_age)

# -------------------------------------------------------------------
# Step 02 - Visualize the Data: Clustered Bar Chart by Age Group
# -------------------------------------------------------------------

chart_age <- ggplot(summary_age, aes(x = WhichCampuses, y = Count, fill = Age)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_text(
    aes(label = Count, colour = Age),
    position = position_dodge(width = 0.7),
    vjust = -0.5, size = 3.5
  ) +
  scale_fill_manual(
    values = c("18-25" = "#7696B7", "26-33" = "#5A7293", "34-39" = "#3E5670", "40+" = "#264463"),
    name = "Age Group"
  ) +
  scale_colour_manual(
    values = c("18-25" = "#7696B7", "26-33" = "#5A7293", "34-39" = "#3E5670", "40+" = "#264463"),
    guide = "none"
  ) +
  labs(
    title = "Participant Count by Campus and Age Group",
    x = "Campus",
    y = "Number of Participants"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "right",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    panel.background = element_rect(fill = "white", color = "white"),
    plot.background = element_rect(fill = "white", color = "white")
  )

# Print Chart
print(chart_age)

# -------------------------------------------------------------------
# Save the Chart
# -------------------------------------------------------------------
chart_age_path <- file.path(output_folder, "participant_count_by_age.png")
ggsave(chart_age_path, plot = chart_age, width = 8, height = 6)

cat("Chart saved at:", chart_age_path, "\n")

# -------------------------------------------------------------------
# -------------------------------------------------------------------
# Section 03 - Get Familiar with Data: Household Composition Overview
# -------------------------------------------------------------------

# Step 01 - Calculate the number of participants by Campus and Household Composition
summary_household <- data %>%
  mutate(
    HouseholdComposition = ifelse(
      HouseholdComposition %in% c("Couple", "Couple with children"),
      "Couple with(out) children",
      HouseholdComposition
    )
  ) %>%
  group_by(WhichCampuses, HouseholdComposition) %>%
  summarise(Count = n(), .groups = "drop")

# Print Summary Data
print(summary_household)

# -------------------------------------------------------------------
# Step 02 - Visualize the Data: Clustered Bar Chart by Household Composition
# -------------------------------------------------------------------

summary_household <- summary_household %>%
  mutate(
    HouseholdComposition = factor(
      HouseholdComposition,
      levels = c("Single", "Couple with(out) children")
    ),
    Legend = interaction(WhichCampuses, HouseholdComposition),
    Legend = factor(
      Legend,
      levels = c(
        "Belval Campus.Single", "Belval Campus.Couple with(out) children",
        "Kirchberg Campus.Single", "Kirchberg Campus.Couple with(out) children",
        "Limpertsberg Campus.Single", "Limpertsberg Campus.Couple with(out) children"
      )
    )
  )

chart_household <- ggplot(summary_household, aes(x = WhichCampuses, y = Count, fill = HouseholdComposition)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_text(
    aes(label = Count, colour = HouseholdComposition),
    position = position_dodge(width = 0.7),
    vjust = -0.5, size = 3.5
  ) +
  scale_fill_manual(
    values = c("Single" = "#264463", "Couple with(out) children" = "#7696B7"),
    name = "Household Composition"
  ) +
  scale_colour_manual(
    values = c("Single" = "#264463", "Couple with(out) children" = "#7696B7"),
    guide = "none"
  ) +
  labs(
    title = "Participant Count by Campus and Household Composition",
    x = "Campus",
    y = "Number of Participants"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "right",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    panel.background = element_rect(fill = "white", color = "white"),
    plot.background = element_rect(fill = "white", color = "white")
  )

# Print Chart
print(chart_household)

# -------------------------------------------------------------------
# Save the Chart
# -------------------------------------------------------------------
chart_household_path <- file.path(output_folder, "participant_count_by_household_composition.png")
ggsave(chart_household_path, plot = chart_household, width = 8, height = 6)

cat("Chart saved at:", chart_household_path, "\n")

# -------------------------------------------------------------------
# -------------------------------------------------------------------
# Section 04 - Get Familiar with Data: Vehicle Ownership Overview
# -------------------------------------------------------------------



# Step 01 - Recode vehicle values
data <- data %>%
  mutate(
    vehicle = case_when(
      vehicle == "Bicycle" ~ "Micro-Mobility (Bike & Scooter)",
      vehicle == "Scooter" ~ "Micro-Mobility (Bike & Scooter)",
      vehicle == "Bicycle; Private car" ~ "Both",
      vehicle == "Private car" ~ "Private car",
      vehicle == "None" ~ "None",
      vehicle == "Both" ~ "Both",
      TRUE ~ vehicle 
    )
  )


# Step 02 - Summarize
summary_vehicle <- data %>%
  group_by(WhichCampuses, vehicle) %>%
  summarise(Count = n(), .groups = "drop") %>%
  mutate(
    vehicle = factor(vehicle, levels = c("Micro-Mobility (Bike & Scooter)", "Private car", "Both", "None"))
  )
summary_vehicle
# -------------------------------------------------------------------
# Step 02 - Visualize the Data: Clustered Bar Chart by Vehicle Ownership
# -------------------------------------------------------------------

ggplot(summary_vehicle, aes(x = WhichCampuses, y = Count, fill = vehicle)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_text(
    aes(label = Count, color = vehicle),
    position = position_dodge(width = 0.7),
    vjust = -0.5, size = 4
  ) +
  scale_fill_manual(
    values = c(
      "Micro-Mobility (Bike & Scooter)" = "#3C5A78",
      "Private car" = "#5C7C9B",
      "Both" = "#7696B7",
      "None" = "#A0B4D3"
    ),
    name = "Vehicle Type"
  ) +
  scale_color_manual(
    values = c(
      "Micro-Mobility (Bike & Scooter)" = "#3C5A78",
      "Private car" = "#5C7C9B",
      "Both" = "#7696B7",
      "None" = "#A0B4D3"
    ),
    guide = "none"
  ) +
  labs(
    title = "Participant Count by Campus and Vehicle Ownership Type",
    x = "Campus",
    y = "Number of Participants"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "right",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    panel.background = element_rect(fill = "white", color = "white"),
    plot.background = element_rect(fill = "white", color = "white")
  )
# Print Chart
print(chart_vehicle)

# -------------------------------------------------------------------
# Save the Chart
# -------------------------------------------------------------------
chart_vehicle_path <- file.path(output_folder, "participant_count_by_vehicle.png")
ggsave(chart_vehicle_path, plot = chart_vehicle, width = 8, height = 6)

cat("Chart saved at:", chart_vehicle_path, "\n")

# -------------------------------------------------------------------
# -------------------------------------------------------------------
# Section 05 - Get Familiar with Data: How Often Each Participant Visits the Campuses
# -------------------------------------------------------------------

# Step 01 - Clean and Recode "Occasionally" and "Rarely" into "Occasionally/Rarely"
data <- data %>%
  mutate(
    Visit.Belval.Campus = case_when(
      Visit.Belval.Campus %in% c("Occasionally", "Rarely") ~ "Occasionally/Rarely",
      TRUE ~ as.character(Visit.Belval.Campus)
    ),
    Visit.Kirchberg.Campus = case_when(
      Visit.Kirchberg.Campus %in% c("Occasionally", "Rarely") ~ "Occasionally/Rarely",
      TRUE ~ as.character(Visit.Kirchberg.Campus)
    ),
    Visit.Limpertsberg..Campus = case_when(
      Visit.Limpertsberg..Campus %in% c("Occasionally", "Rarely") ~ "Occasionally/Rarely",
      TRUE ~ as.character(Visit.Limpertsberg..Campus)
    )
  )

# Prepare Data for Visualization
data_long <- data %>%
  pivot_longer(
    cols = c(Visit.Belval.Campus, Visit.Kirchberg.Campus, Visit.Limpertsberg..Campus),
    names_to = "Campus",
    values_to = "Frequency"
  ) %>%
  mutate(
    Frequency = factor(Frequency, levels = c("Daily", "Weekly", "Monthly", "Occasionally/Rarely", "Never"))
  )

# Summarise Data
summary_visit <- data_long %>%
  group_by(Campus, Frequency) %>%
  summarise(Count = n(), .groups = "drop")

# Print Summary Data
print(summary_visit)

# -------------------------------------------------------------------
# Step 02 - Visualize the Data: Clustered Bar Chart by Visit Frequency
# -------------------------------------------------------------------

# Define Color Palette
frequency_colors <- c(
  "Daily" = "#3C5A78",
  "Weekly" = "#5C7C9B",
  "Monthly" = "#7696B7",
  "Occasionally/Rarely" = "#A0B4D3",
  "Never" = "#D0E1F4"
)

# Create Chart
chart_visit <- ggplot(summary_visit, aes(x = Campus, y = Count, fill = Frequency)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_text(
    aes(label = Count, colour = Frequency),
    position = position_dodge(width = 0.7),
    vjust = -0.5, size = 3.5
  ) +
  scale_fill_manual(
    values = frequency_colors,
    name = "Visit Frequency",
    breaks = c("Daily", "Weekly", "Monthly", "Occasionally/Rarely", "Never")
  ) +
  scale_colour_manual(
    values = frequency_colors,
    guide = "none"
  ) +
  labs(
    title = "Participant Visit Frequency by Campus",
    x = "Campus",
    y = "Number of Participants"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "right",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    panel.background = element_rect(fill = "white", color = "white"),
    plot.background = element_rect(fill = "white", color = "white")
  )

# Print Chart
print(chart_visit)

# -------------------------------------------------------------------
# Step 03 - Save the Chart
# -------------------------------------------------------------------
chart_visit_path <- file.path(output_folder, "participant_visit_frequency_by_campus.png")
ggsave(chart_visit_path, plot = chart_visit, width = 8, height = 6)

cat("Chart saved at:", chart_visit_path, "\n")

# -------------------------------------------------------------------
# -------------------------------------------------------------------
# Section 06 - Analyze the Relationships Between Gender, Campus, and Preferences
# -------------------------------------------------------------------

# Step 01 - Analyze Preference01


# Create Contingency Tables for Preference01
gender_preference_table <- table(data$Gender, data$preference01)
campus_preference_table <- table(data$WhichCampuses, data$preference01)

# Print Tables
print(gender_preference_table)
print(campus_preference_table)

# Perform Fisher's Exact Tests for Preference01
fisher_gender_pref <- fisher.test(gender_preference_table)
cat("Fisher's Exact Test for Gender and Preference01:\n")
print(fisher_gender_pref)

fisher_campus_pref <- fisher.test(campus_preference_table)
cat("\nFisher's Exact Test for WhichCampuses and Preference01:\n")
print(fisher_campus_pref)

# -------------------------------------------------------------------
# result of fisher test (gender and campus and Preference01)
# -------------------------------------------------------------------
cat("No significant relationship found between gender or campus and Preference01 (p-values: ", 
    round(fisher_gender_pref$p.value, 4), " and ", round(fisher_campus_pref$p.value, 4), ")\n", sep = "")
# -------------------------------------------------------------------

# Perform Chi-Square Tests with Monte Carlo Simulation for Preference01
chi_gender_pref_mc <- chisq.test(gender_preference_table, simulate.p.value = TRUE, B = 10000)
cat("\nChi-Square Test with Monte Carlo Simulation for Gender and Preference01:\n")
print(chi_gender_pref_mc)

chi_campus_pref_mc <- chisq.test(campus_preference_table, simulate.p.value = TRUE, B = 10000)
cat("\nChi-Square Test with Monte Carlo Simulation for WhichCampuses and Preference01:\n")
print(chi_campus_pref_mc)

# -------------------------------------------------------------------
# result of Chi-Square test (gender and campus and Preference01)
# -------------------------------------------------------------------
cat("No significant relationship found (Chi-Square) between gender or campus and Preference01 (p-values: ",
    round(chi_gender_pref_mc$p.value, 4), " and ", round(chi_campus_pref_mc$p.value, 4), ")\n", sep = "")

# -------------------------------------------------------------------
# Step 02 - Visualize the Contingency Matrix (Heatmap) for Preference01
# -------------------------------------------------------------------

# Define Color Gradient for Heatmap
heatmap_colors <- scale_fill_gradient(
  low = "#D0E1F4", 
  high = "#3C5A78", 
  name = "Frequency"
)

# Create Contingency Matrix for Preference01
preference01_matrix <- table(data$preference01, data$WhichCampuses)

# Build Heatmap for Preference01
preference01_heatmap <- ggplot(as.data.frame(preference01_matrix), aes(Var1, Var2, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Freq), color = "black", size = 4) +
  heatmap_colors +
  labs(
    title = "Heatmap for Preference 01",
    x = "Preferred Campus",
    y = "Participant's Study/Work Campus"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.background = element_rect(fill = "white", color = "white"),
    plot.background = element_rect(fill = "white", color = "white")
  )

# Print Heatmap
print(preference01_heatmap)

# -------------------------------------------------------------------
# Save the Heatmap
# -------------------------------------------------------------------
heatmap_path <- file.path(output_folder, "heatmap_preference01.png")
ggsave(heatmap_path, plot = preference01_heatmap, width = 8, height = 6, dpi = 300)

cat("Heatmap for Preference01 saved at:", heatmap_path, "\n")

# -------------------------------------------------------------------
# -------------------------------------------------------------------
# Section 07 - Analyze Visit Frequency vs Campus Preference (Preference01)
# -------------------------------------------------------------------

# -------------------------------------------------------------------
# Step 01 - Chi-Square and Fisher's Exact Tests
# -------------------------------------------------------------------

# Recode Occasionally and Rarely
data <- data %>%
  mutate(
    Visit.Belval.Campus = case_when(
      Visit.Belval.Campus %in% c("Occasionally", "Rarely") ~ "Occasionally/Rarely",
      TRUE ~ as.character(Visit.Belval.Campus)
    ),
    Visit.Kirchberg.Campus = case_when(
      Visit.Kirchberg.Campus %in% c("Occasionally", "Rarely") ~ "Occasionally/Rarely",
      TRUE ~ as.character(Visit.Kirchberg.Campus)
    ),
    Visit.Limpertsberg..Campus = case_when(
      Visit.Limpertsberg..Campus %in% c("Occasionally", "Rarely") ~ "Occasionally/Rarely",
      TRUE ~ as.character(Visit.Limpertsberg..Campus)
    )
  )

# Ensure factors
data$preference01 <- as.factor(data$preference01)
data$Visit.Belval.Campus <- as.factor(data$Visit.Belval.Campus)
data$Visit.Kirchberg.Campus <- as.factor(data$Visit.Kirchberg.Campus)
data$Visit.Limpertsberg..Campus <- as.factor(data$Visit.Limpertsberg..Campus)

# Contingency Tables
table_belval <- table(data$Visit.Belval.Campus, data$preference01)
table_kirchberg <- table(data$Visit.Kirchberg.Campus, data$preference01)
table_limpertsberg <- table(data$Visit.Limpertsberg..Campus, data$preference01)

# Chi-Square Tests
chi_belval <- chisq.test(table_belval)
chi_kirchberg <- chisq.test(table_kirchberg)
chi_limpertsberg <- chisq.test(table_limpertsberg)

# Print Results
cat("Chi-Square Test for Belval Campus Visit Frequency and Preference01:\n")
print(chi_belval)

cat("\nChi-Square Test for Kirchberg Campus Visit Frequency and Preference01:\n")
print(chi_kirchberg)

cat("\nChi-Square Test for Limpertsberg Campus Visit Frequency and Preference01:\n")
print(chi_limpertsberg)

# Optional Fisher's Test if Needed
if (any(chi_belval$expected < 5)) {
  cat("\nFisher's Exact Test for Belval Campus Visit Frequency and Preference01:\n")
  print(fisher.test(table_belval))
}
if (any(chi_kirchberg$expected < 5)) {
  cat("\nFisher's Exact Test for Kirchberg Campus Visit Frequency and Preference01:\n")
  print(fisher.test(table_kirchberg))
}
if (any(chi_limpertsberg$expected < 5)) {
  cat("\nFisher's Exact Test for Limpertsberg Campus Visit Frequency and Preference01:\n")
  print(fisher.test(table_limpertsberg))
}
# -------------------------------------------------------------------
# Interpretation Summary for Visit Frequency vs. Campus Preference01
# -------------------------------------------------------------------
cat("------------------------------------------------------------\n")
cat("Interpretation Summary:\n\n")

cat("Belval Campus:\n")
cat("- Chi-Square p-value: ", round(chi_belval$p.value, 4), "\n")
cat("- Fisher's Exact Test p-value: 0.632\n")
cat("- Conclusion: No significant relationship between visit frequency and Preference01.\n\n")

cat("Kirchberg Campus:\n")
cat("- Chi-Square p-value: ", round(chi_kirchberg$p.value, 4), "\n")
cat("- Fisher's Exact Test p-value: 0.1977\n")
cat("- Conclusion: No significant relationship between visit frequency and Preference01.\n\n")

cat("Limpertsberg Campus:\n")
cat("- Chi-Square p-value: ", round(chi_limpertsberg$p.value, 4), "\n")
cat("- Fisher's Exact Test p-value: 0.5587\n")
cat("- Conclusion: No significant relationship between visit frequency and Preference01.\n\n")

cat("Overall Conclusion:\n")
cat("Across all campuses, visit frequency does not appear to influence participants' first campus preference (Preference01).\n")
cat("------------------------------------------------------------\n")

# -------------------------------------------------------------------
# Step 02 - Visualize with Stacked Bar Chart
# -------------------------------------------------------------------

# Convert Data to Long Format
data_long <- data %>%
  pivot_longer(cols = c(Visit.Belval.Campus, Visit.Kirchberg.Campus, Visit.Limpertsberg..Campus),
               names_to = "Campus",
               values_to = "Visit_Frequency")

# Rename Facet Labels
data_long$Campus <- dplyr::recode(data_long$Campus, 
                                  "Visit.Belval.Campus" = "Visit Frequency\nBelval Campus",
                                  "Visit.Kirchberg.Campus" = "Visit Frequency\nKirchberg Campus",
                                  "Visit.Limpertsberg..Campus" = "Visit Frequency\nLimpertsberg Campus")


# Set Factor Order
data_long$Visit_Frequency <- factor(data_long$Visit_Frequency, 
                                    levels = c("Daily", "Weekly", "Monthly", "Occasionally/Rarely", "Never"))

# Simplify Preference Labels
data_long$preference01 <- gsub(" Campus", "", data_long$preference01)

# Define Color Palette
frequency_colors <- c(
  "Daily" = "#3C5A78",
  "Weekly" = "#5C7C9B",
  "Monthly" = "#7696B7",
  "Occasionally/Rarely" = "#A0B4D3",
  "Never" = "#D0E1F4"
)

# Create Stacked Bar Plot
stacked_bar_plot <- ggplot(data_long, aes(x = preference01, fill = Visit_Frequency)) +
  geom_bar(position = "fill") +
  facet_wrap(~ Campus) +
  labs(x = "Campus Preference", y = "Proportion", fill = "Visit Frequency") +
  theme_minimal() +
  scale_fill_manual(values = frequency_colors) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    strip.text = element_text(size = 8),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 8)
  ) +
  ggtitle("Visit Frequency Distribution by Campus Preference")

# Save and Print Plot
file_path <- file.path(output_folder, "stackedbar_VisitFrequency_Preference.tif")
ggsave(file_path, plot = stacked_bar_plot, width = 8, height = 6, dpi = 300, device = "tiff")
print(stacked_bar_plot)

# -------------------------------------------------------------------
# Step 03 - Generate and Save Heatmaps for Each Campus
# -------------------------------------------------------------------

generate_heatmap <- function(count_table, campus_name, output_folder) {
  heatmap_data <- as.data.frame(as.table(count_table))
  colnames(heatmap_data) <- c("Visit_Frequency", "Campus_Preference", "Count")
  heatmap_data$Campus_Preference <- gsub(" ", "\n", heatmap_data$Campus_Preference)
  heatmap_data$Visit_Frequency <- factor(heatmap_data$Visit_Frequency,
                                         levels = c("Daily", "Weekly", "Monthly", "Occasionally/Rarely", "Never"))
  clean_campus_name <- gsub("Visit\\.", "", campus_name)
  clean_campus_name <- gsub("\\.\\.Campus", "", clean_campus_name)
  
  heatmap_plot <- ggplot(heatmap_data, aes(x = Campus_Preference, y = Visit_Frequency, fill = Count)) +
    geom_tile(color = "white") +
    geom_text(aes(label = Count), color = "black", size = 3) +
    scale_fill_gradient(low = "#D0E1F4", high = "#3C5A78", name = "Count") +
    labs(title = paste("Visit Frequency", clean_campus_name, "\nvs. Preference"),
         x = "Campus Preference", y = "Visit Frequency") +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 12)
    )
  
  file_path <- file.path(output_folder, paste0("heatmap_VisitFrequency", clean_campus_name, "_Preference.tif"))
  ggsave(file_path, plot = heatmap_plot, width = 8, height = 6, dpi = 300, device = "tiff")
  print(heatmap_plot)
}

# Prepare Count Tables
count_table_belval <- table(factor(data$Visit.Belval.Campus, levels = c("Daily", "Weekly", "Monthly", "Occasionally/Rarely", "Never")), data$preference01)
count_table_kirchberg <- table(factor(data$Visit.Kirchberg.Campus, levels = c("Daily", "Weekly", "Monthly", "Occasionally/Rarely", "Never")), data$preference01)
count_table_limpertsberg <- table(factor(data$Visit.Limpertsberg..Campus, levels = c("Daily", "Weekly", "Monthly", "Occasionally/Rarely", "Never")), data$preference01)

# Generate Heatmaps
generate_heatmap(count_table_belval, "Visit.Belval.Campus", output_folder)
generate_heatmap(count_table_kirchberg, "Visit.Kirchberg.Campus", output_folder)
generate_heatmap(count_table_limpertsberg, "Visit.Limpertsberg..Campus", output_folder)

# ===============================================================
# Chapter 02 - Emotional Experience
# Section 01 - Descriptive Statistics
# ===============================================================

# -------------------------------------------------------------------
# Step 01 - Select Emotional Experience Variables
# -------------------------------------------------------------------
cat("Step 01 - Selecting emotional experience columns only (Stress, Fear, Calmness, Happiness, Satisfactions, Excitement)\n")

numeric_columns_emotion <- grep("^nvalue_.*(Stress|Fear|Calmness|Happiness|Satisfactions|Excitement)", colnames(data), value = TRUE)

cat("Selected columns:\n")
print(numeric_columns_emotion)

# -------------------------------------------------------------------
# Step 02 - Define Summary Statistics Function
# -------------------------------------------------------------------
cat("\nStep 02 - Defining function to calculate summary statistics\n")

calculate_summary <- function(column) {
  data <- na.omit(column)
  summary_stats <- list(
    Mean = mean(data),
    Median = median(data),
    Mode = as.numeric(names(sort(table(data), decreasing = TRUE)[1])),
    Min = min(data),
    Max = max(data),
    IQR = IQR(data),
    Skewness = skewness(data),
    Kurtosis = kurtosis(data)
  )
  return(summary_stats)
}

# -------------------------------------------------------------------
# Step 03 - Calculate Summary Statistics for Each Emotional Category
# -------------------------------------------------------------------
cat("\nStep 03 - Calculating summary statistics for each category\n")

summary_results <- list()

for (col in numeric_columns_emotion) {
  category <- gsub("nvalue_", "", col)
  summary_results[[category]] <- calculate_summary(data[[col]])
}

summary_df <- do.call(rbind, lapply(summary_results, as.data.frame))
rownames(summary_df) <- names(summary_results)

# -------------------------------------------------------------------
# Step 04 - Display and Save Results
# -------------------------------------------------------------------
cat("\nStep 04 - Summary Statistics:\n")
print(summary_df)

output_file <- file.path(data_folder, "summary_statistics_emotion.csv")
write.csv(summary_df, file = output_file, row.names = TRUE)

cat("Summary statistics saved to:", output_file, "\n")


# ===============================================================
# Chapter 02 - Emotional Experience
# Section 02 - Plot Emotional Experiences by Campus
# ===============================================================

# -------------------------------------------------------------------
# Step 01 - Prepare Emotion Data for Plotting
# -------------------------------------------------------------------
cat("Step 01 - Preparing emotional experience data for plotting\n")

emotion_data <- data %>%
  pivot_longer(
    cols = numeric_columns_emotion,
    names_to = "Emotion",
    values_to = "Score"
  ) %>%
  mutate(
    Emotion = gsub("nvalue_", "", Emotion),
    Campus = case_when(
      grepl("Belval", Emotion) ~ "Belval Campus",
      grepl("Kirchberg", Emotion) ~ "Kirchberg Campus",
      grepl("Limpertsberg", Emotion) ~ "Limpertsberg Campus",
      TRUE ~ NA_character_
    ),
    Emotion = gsub("(Belval|Kirchberg|Limpertsberg)", "", Emotion)
  ) %>%
  drop_na(Campus)

campus_colors <- c(
  "Belval Campus" = "#C3A6B1",
  "Kirchberg Campus" = "#A0B4D3",
  "Limpertsberg Campus" = "#A8C4A2"
)

y_axis_breaks <- c(10, 20, 30, 40, 50)
y_axis_labels <- c("Not at all = 10", "A little bit = 20", "Neutral = 30", "Quite a lot = 40", "Very strongly = 50")

# -------------------------------------------------------------------
# Step 02 - Generate Plots for Each Emotional Experience
# -------------------------------------------------------------------
cat("\nStep 02 - Generating emotional experience plots\n")

for (emotion in unique(emotion_data$Emotion)) {
  clean_emotion <- gsub("Campus", "", emotion)
  emotion_subset <- emotion_data %>% filter(Emotion == emotion)
  
  emotion_summary <- emotion_subset %>%
    group_by(Campus) %>%
    summarise(
      Mean = mean(Score, na.rm = TRUE),
      Median = median(Score, na.rm = TRUE),
      Mode = as.numeric(names(sort(table(Score), decreasing = TRUE)[1])),
      Min = min(Score, na.rm = TRUE),
      Max = max(Score, na.rm = TRUE),
      SD = sd(Score, na.rm = TRUE),
      SD_Upper = Mean + SD,
      SD_Lower = Mean - SD,
      IQR = paste0("(", quantile(Score, 0.25, na.rm = TRUE), " - ", quantile(Score, 0.75, na.rm = TRUE), ")"),
      Skewness = round(skewness(Score, na.rm = TRUE), 2),
      Kurtosis = round(kurtosis(Score, na.rm = TRUE), 2),
      .groups = "drop"
    )
  
  emotion_plot <- ggplot(emotion_subset, aes(x = Campus, y = Score)) +
    geom_boxplot(aes(fill = Campus), outlier.shape = NA, alpha = 0.6) +
    geom_point(data = emotion_summary, aes(y = Mean), shape = 4, size = 3, color = "black", stroke = 1.2) +
    geom_point(data = emotion_summary, aes(y = Median), shape = 15, size = 3, color = "black", stroke = 1.2) +
    geom_point(data = emotion_summary, aes(y = Mode), shape = 18, size = 3, color = "black", stroke = 1.2) +
    geom_point(data = emotion_summary, aes(y = Min), shape = 17, size = 3) +
    geom_point(data = emotion_summary, aes(y = Max), shape = 25, size = 3) +
    geom_point(data = emotion_summary, aes(y = SD), shape = 0, size = 3) +
    scale_y_continuous(limits = c(0, 60), breaks = y_axis_breaks, labels = y_axis_labels) +
    scale_fill_manual(values = campus_colors) +
    scale_color_manual(values = campus_colors, guide = "none") +
    labs(title = paste(clean_emotion, "by Campus"), x = "Campus", y = paste(clean_emotion, "Intensity")) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, size = 16), legend.position = "none")
  
  legend_text_parts <- lapply(1:nrow(emotion_summary), function(i) {
    row <- emotion_summary[i, ]
    paste0("[", row$Campus, "]\n(✕) Mean\n(■) Median\n(◆) Mode\n(△) Minimum\n(▽) Maximum\n(▢) SD\n",
           "  SD Upper: ", round(row$SD_Upper, 2), "\n",
           "  SD Lower: ", round(row$SD_Lower, 2), "\n",
           "  IQR: ", row$IQR, "\n",
           "  Skewness: ", row$Skewness, "\n",
           "  Kurtosis: ", row$Kurtosis)
  })
  
  legend_text <- paste(legend_text_parts, collapse = "\n\n")
  
  custom_legend <- ggplot() +
    annotate("text", x = 0.5, y = 0.5, label = legend_text, size = 3.5, color = "black", hjust = 0) +
    theme_void()
  
  combined_plot <- cowplot::plot_grid(emotion_plot, custom_legend, ncol = 2, rel_widths = c(3, 2), align = "v")
  
  plot_path <- file.path(output_folder, paste0(clean_emotion, "_by_campus.png"))
  ggsave(plot_path, plot = combined_plot, width = 16, height = 10, dpi = 300, bg = "white")
  
  cat("Plot for", clean_emotion, "saved at:", plot_path, "\n")
}

cat("\nAll emotional experience plots have been generated and saved.\n")


# ===============================================================
# Chapter 02 - Emotional Experience
# Section 03 - Radar Chart Comparison by Campus
# ===============================================================

cat("Step 01 - Preparing emotional experience variables and color palette\n")

emotions <- c("Stress", "Fear", "Calmness", "Happiness", "Satisfactions", "Excitement")
campuses <- c("Belval", "Kirchberg", "Limpertsberg")

campus_colors <- c("Belval" = "#EAD1DC", "Kirchberg" = "#D0E1F4", "Limpertsberg" = "#D6E7D5")

calculate_mode <- function(x) {
  uniq_vals <- unique(na.omit(x))
  uniq_vals[which.max(tabulate(match(x, uniq_vals)))]
}

prepare_radar_data <- function(stat_func) {
  data.frame(
    Belval = sapply(emotions, function(emotion) stat_func(data[[paste0("nvalue_", emotion, "BelvalCampus")]])),
    Kirchberg = sapply(emotions, function(emotion) stat_func(data[[paste0("nvalue_", emotion, "KirchbergCampus")]])),
    Limpertsberg = sapply(emotions, function(emotion) stat_func(data[[paste0("nvalue_", emotion, "LimpertsbergCampus")]]))
  )
}

generate_radar_chart <- function(radar_data, title_suffix, output_filename) {
  rownames(radar_data) <- emotions
  radar_data <- as.data.frame(t(radar_data))
  radar_data <- rbind(rep(50, length(emotions)), rep(10, length(emotions)), radar_data)
  
  output_radar_path <- file.path(output_folder, output_filename)
  png(output_radar_path, width = 1600, height = 1600, res = 300, bg = "white")
  
  radarchart(
    radar_data,
    axistype = 1,
    pcol = campus_colors,
    plty = 1,
    plwd = 2,
    cglcol = "grey",
    cglty = 1,
    cglwd = 0.8,
    axislabcol = "black",
    vlcex = 0.8,
    title = paste(title_suffix, "Emotional Score Comparison"),
    caxislabels = c("10\nNot at all", "20\nA little bit", "30\nNeutral", "40\nQuite a lot", "50\nVery strongly"),
    calcex = 0.4
  )
  
  legend("topright", legend = campuses, col = campus_colors, lty = 1, lwd = 2, cex = 0.8, bty = "n")
  
  dev.off()
  cat("Radar chart saved at:", output_radar_path, "\n")
}

cat("Step 05 - Generating radar charts for Mean, Median, and Mode\n")
generate_radar_chart(prepare_radar_data(mean), "Mean", "Radar_Chart_Emotional_Experiences_Mean.png")
generate_radar_chart(prepare_radar_data(median), "Median", "Radar_Chart_Emotional_Experiences_Median.png")
generate_radar_chart(prepare_radar_data(calculate_mode), "Mode", "Radar_Chart_Emotional_Experiences_Mode.png")

cat("\nAll radar charts have been generated and saved.\n")


# ===============================================================
# Chapter 02 - Emotional Experience
# Section 04 - Emotional Experience and Social Demographic Factors
# ===============================================================

cat("Step 01 - Preparing emotional experience variables and Likert scale mapping\n")

emotions <- c("Stress", "Fear", "Calmness", "Happiness", "Satisfactions", "Excitement")
campuses <- c("Belval", "Kirchberg", "Limpertsberg")
emotion_map <- c("Not at all" = 10, "A little bit" = 20, "Neutral" = 30, "Quite a lot" = 40, "Very strongly" = 50)

gender_results <- data.frame(Emotion = character(), Campus = character(), Test = character(), P_Value = numeric(), Interpretation = character(), stringsAsFactors = FALSE)

cat("Step 02 - Running gender comparison tests\n")

for (emotion in emotions) {
  for (campus in campuses) {
    col_name <- paste0(emotion, campus, "Campus")
    if (!(col_name %in% colnames(data))) next
    numeric_col <- paste0(col_name, "_numeric")
    data[[numeric_col]] <- emotion_map[as.character(data[[col_name]])]
    male_vals <- data[[numeric_col]][data$Gender == "Male"]
    female_vals <- data[[numeric_col]][data$Gender == "Female"]
    if (length(male_vals) < 2 || length(female_vals) < 2) next
    sw_male <- shapiro.test(male_vals)
    sw_female <- shapiro.test(female_vals)
    if (sw_male$p.value < 0.05 || sw_female$p.value < 0.05) {
      test <- wilcox.test(as.formula(paste(numeric_col, "~ Gender")), data = data)
      test_name <- "Mann–Whitney U"
    } else {
      test <- t.test(as.formula(paste(numeric_col, "~ Gender")), data = data)
      test_name <- "Independent t-test"
    }
    p_val <- test$p.value
    interpretation <- if (p_val < 0.05) "Significant gender difference." else if (p_val < 0.10) "Trend toward gender difference." else "No significant gender difference."
    gender_results <- rbind(gender_results, data.frame(Emotion = emotion, Campus = campus, Test = test_name, P_Value = round(p_val, 4), Interpretation = interpretation, stringsAsFactors = FALSE))
  }
}

write.csv(gender_results, file = "C:/Users/nima/Desktop/data analysis/github/data/genderVSemotions.csv", row.names = FALSE)

gender_results <- gender_results %>% mutate(Label = sprintf("%.3f", P_Value))
gender_heatmap <- ggplot(gender_results, aes(x = Campus, y = Emotion, fill = P_Value)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Label), color = "white", size = 3) +
  scale_fill_gradient(low = "#D0E1F4", high = "#3C5A78", limits = c(0, 1)) +
  labs(title = "Gender Differences in Emotional Responses", subtitle = "P-values of Statistical Tests", x = "Campus", y = "Emotion") +
  theme_minimal()
ggsave(file.path(output_folder, "heatmap_genderVSemotions_PValues.tif"), gender_heatmap, width = 8, height = 6, dpi = 300, device = "tiff")

cat("Step 04 - Running education level comparison tests\n")

edu_results <- data.frame(Emotion = character(), Campus = character(), Test = character(), P_Value = numeric(), Interpretation = character(), stringsAsFactors = FALSE)

for (emotion in emotions) {
  for (campus in campuses) {
    col_name <- paste0(emotion, campus, "Campus")
    if (!(col_name %in% colnames(data))) next
    numeric_col <- paste0(col_name, "_numeric")
    data[[numeric_col]] <- emotion_map[as.character(data[[col_name]])]
    df <- data[, c("HighestLevelOfEducation", numeric_col)]
    df <- df[complete.cases(df), ]
    if (length(unique(df$HighestLevelOfEducation)) < 2) next
    kruskal_test <- kruskal.test(df[[numeric_col]] ~ df$HighestLevelOfEducation)
    p_val <- kruskal_test$p.value
    interpretation <- if (p_val < 0.05) "Significant difference between education levels." else if (p_val < 0.10) "Trend toward difference between education levels." else "No significant difference between education levels."
    edu_results <- rbind(edu_results, data.frame(Emotion = emotion, Campus = campus, Test = "Kruskal–Wallis H", P_Value = round(p_val, 4), Interpretation = interpretation, stringsAsFactors = FALSE))
  }
}

write.csv(edu_results, file = "C:/Users/nima/Desktop/data analysis/github/data/educationlevelVSemotions.csv", row.names = FALSE)

edu_results <- edu_results %>% mutate(Label = sprintf("%.3f", P_Value))
edu_heatmap <- ggplot(edu_results, aes(x = Campus, y = Emotion, fill = P_Value)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Label), color = "white", size = 3) +
  scale_fill_gradient(low = "#D0E1F4", high = "#3C5A78", limits = c(0, 1)) +
  labs(title = "Education Level Differences in Emotional Responses", subtitle = "P-values of Kruskal–Wallis Tests", x = "Campus", y = "Emotion") +
  theme_minimal()
ggsave(file.path(output_folder, "heatmap_educationlevelVSemotions_PValues.tif"), edu_heatmap, width = 8, height = 6, dpi = 300, device = "tiff")

# ===============================================================
# Chapter 03 - Emotional Experience and The Environmental Features
# Section 01 - Descriptive Statistics
# ===============================================================

cat("Step 01 - Defining environmental feature columns (Green Spaces, Buildings, Sounds)\n")

greenspaces <- c("nvalue_GreenSpacesBelvalCampus", "nvalue_GreenSpacesKirchbergCampus", "nvalue_GreenSpacesLimpertsbergCampus")
buildings <- c("nvalue_BuildingBelvalCampus", "nvalue_BuildingKirchbergCampus", "nvalue_BuildingLimpertsbergCampus")
sounds <- c("nvalue_SoundsBelvalCampus", "nvalue_SoundsKirchbergCampus", "nvalue_SoundsLimpertsbergCampus")

env_columns <- c(greenspaces, buildings, sounds)

calculate_summary <- function(column) {
  data <- na.omit(column)
  summary_stats <- list(
    Mean = mean(data),
    Median = median(data),
    Mode = as.numeric(names(sort(table(data), decreasing = TRUE)[1])),
    Min = min(data),
    Max = max(data),
    IQR = IQR(data),
    Skewness = skewness(data),
    Kurtosis = kurtosis(data)
  )
  return(summary_stats)
}

summary_results <- list()
for (col in env_columns) {
  category <- gsub("nvalue_", "", col)
  summary_results[[category]] <- calculate_summary(data[[col]])
}
summary_df <- do.call(rbind, lapply(summary_results, as.data.frame))
rownames(summary_df) <- names(summary_results)

output_file <- file.path(data_folder, "summary_statistics_environmental_features.csv")
write.csv(summary_df, file = output_file, row.names = TRUE)
cat("Summary statistics saved to:", output_file, "\n")


# ===============================================================
# Chapter 03 - Emotional Experience and The Environmental Features
# Section 02 - Plot Emotional Experience and The Environmental Features by Campus
# ===============================================================

# -------------------------------------------------------------------
# Step 01 - Prepare Environmental Feature Data for Plotting
# -------------------------------------------------------------------
cat("Step 01 - Preparing environmental feature data for plotting\n")

feature_data <- data %>%
  pivot_longer(
    cols = all_of(env_columns),
    names_to = "Feature",
    values_to = "Score"
  ) %>%
  mutate(
    Feature = gsub("nvalue_", "", Feature),
    Campus = case_when(
      grepl("BelvalCampus", Feature) ~ "Belval Campus",
      grepl("KirchbergCampus", Feature) ~ "Kirchberg Campus",
      grepl("LimpertsbergCampus", Feature) ~ "Limpertsberg Campus",
      TRUE ~ NA_character_
    ),
    Feature = gsub("(BelvalCampus|KirchbergCampus|LimpertsbergCampus)", "", Feature)
  ) %>%
  drop_na(Campus)

campus_colors <- c(
  "Belval Campus" = "#C3A6B1",
  "Kirchberg Campus" = "#A0B4D3",
  "Limpertsberg Campus" = "#A8C4A2"
)

y_axis_breaks <- c(10, 20, 30, 40, 50)
y_axis_labels <- c("Not at all = 10", "A little bit = 20", "Neutral = 30", "Quite a lot = 40", "Very strongly = 50")

# -------------------------------------------------------------------
# Step 02 - Generate Plots for Each Environmental Feature
# -------------------------------------------------------------------
cat("\nStep 02 - Generating environmental feature plots\n")

for (feature in unique(feature_data$Feature)) {
  clean_feature <- gsub("Campus", "", feature)
  feature_subset <- feature_data %>% filter(Feature == feature)
  
  feature_summary <- feature_subset %>%
    group_by(Campus) %>%
    summarise(
      Mean = mean(Score, na.rm = TRUE),
      Median = median(Score, na.rm = TRUE),
      Mode = as.numeric(names(sort(table(Score), decreasing = TRUE)[1])),
      Min = min(Score, na.rm = TRUE),
      Max = max(Score, na.rm = TRUE),
      SD = sd(Score, na.rm = TRUE),
      SD_Upper = Mean + SD,
      SD_Lower = Mean - SD,
      IQR = paste0("(", quantile(Score, 0.25, na.rm = TRUE), " - ", quantile(Score, 0.75, na.rm = TRUE), ")"),
      Skewness = round(skewness(Score, na.rm = TRUE), 2),
      Kurtosis = round(kurtosis(Score, na.rm = TRUE), 2),
      .groups = "drop"
    )
  
  feature_plot <- ggplot(feature_subset, aes(x = Campus, y = Score)) +
    geom_boxplot(aes(fill = Campus), outlier.shape = NA, alpha = 0.6) +
    geom_point(data = feature_summary, aes(y = Mean), shape = 4, size = 3, color = "black", stroke = 1.2) +
    geom_point(data = feature_summary, aes(y = Median), shape = 15, size = 3, color = "black", stroke = 1.2) +
    geom_point(data = feature_summary, aes(y = Mode), shape = 18, size = 3, color = "black", stroke = 1.2) +
    geom_point(data = feature_summary, aes(y = Min), shape = 17, size = 3) +
    geom_point(data = feature_summary, aes(y = Max), shape = 25, size = 3) +
    geom_point(data = feature_summary, aes(y = SD), shape = 0, size = 3) +
    scale_y_continuous(limits = c(0, 60), breaks = y_axis_breaks, labels = y_axis_labels) +
    scale_fill_manual(values = campus_colors) +
    scale_color_manual(values = campus_colors, guide = "none") +
    labs(
      title = paste(clean_feature, "by Campus"),
      x = "Campus",
      y = paste(clean_feature, "Rating")
    ) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, size = 16), legend.position = "none")
  
  legend_text_parts <- lapply(1:nrow(feature_summary), function(i) {
    row <- feature_summary[i, ]
    paste0("[", row$Campus, "]\n(✕) Mean\n(■) Median\n(◆) Mode\n(△) Minimum\n(▽) Maximum\n(▢) SD\n",
           "  SD Upper: ", round(row$SD_Upper, 2), "\n",
           "  SD Lower: ", round(row$SD_Lower, 2), "\n",
           "  IQR: ", row$IQR, "\n",
           "  Skewness: ", row$Skewness, "\n",
           "  Kurtosis: ", row$Kurtosis)
  })
  
  legend_text <- paste(legend_text_parts, collapse = "\n\n")
  
  custom_legend <- ggplot() +
    annotate("text", x = 0.5, y = 0.5, label = legend_text, size = 3.5, color = "black", hjust = 0) +
    theme_void()
  
  combined_plot <- cowplot::plot_grid(feature_plot, custom_legend, ncol = 2, rel_widths = c(3, 2), align = "v")
  
  plot_path <- file.path(output_folder, paste0(clean_feature, "_by_campus.png"))
  ggsave(plot_path, plot = combined_plot, width = 16, height = 10, dpi = 300, bg = "white")
  
  cat("Plot for", clean_feature, "saved at:", plot_path, "\n")
}

cat("\nAll environmental feature plots have been generated and saved.\n")


# ===============================================================
# Chapter 03 - Emotional Experience and The Environmental Features
# Section 03 - Radar Chart Comparison by Campus
# ===============================================================

features <- c("GreenSpaces", "Building", "Sounds")
campuses <- c("Belval", "Kirchberg", "Limpertsberg")

campus_colors <- c("Belval" = "#EAD1DC", "Kirchberg" = "#D0E1F4", "Limpertsberg" = "#D6E7D5")

calculate_mode <- function(x) {
  uniq_vals <- unique(na.omit(x))
  uniq_vals[which.max(tabulate(match(x, uniq_vals)))]
}

prepare_radar_data <- function(stat_func) {
  data.frame(
    Belval = sapply(features, function(feature) stat_func(data[[paste0("nvalue_", feature, "BelvalCampus")]])),
    Kirchberg = sapply(features, function(feature) stat_func(data[[paste0("nvalue_", feature, "KirchbergCampus")]])),
    Limpertsberg = sapply(features, function(feature) stat_func(data[[paste0("nvalue_", feature, "LimpertsbergCampus")]]))
  )
}

generate_radar_chart <- function(radar_data, title_suffix, output_filename) {
  rownames(radar_data) <- features
  radar_data <- as.data.frame(t(radar_data))
  radar_data <- rbind(rep(50, length(features)), rep(10, length(features)), radar_data)
  
  output_radar_path <- file.path(output_folder, output_filename)
  png(output_radar_path, width = 1600, height = 1600, res = 300, bg = "white")
  
  radarchart(
    radar_data,
    axistype = 1,
    pcol = campus_colors,
    plty = 1,
    plwd = 2,
    cglcol = "grey",
    cglty = 1,
    cglwd = 0.8,
    axislabcol = "black",
    vlcex = 0.8,
    title = paste(title_suffix, "Environmental Feature Comparison"),
    caxislabels = c("10\nNot at all", "20\nA little bit", "30\nNeutral", "40\nQuite a lot", "50\nVery strongly"),
    calcex = 0.4
  )
  
  legend("topright", legend = campuses, col = campus_colors, lty = 1, lwd = 2, cex = 0.8, bty = "n")
  
  dev.off()
  cat("Radar chart saved at:", output_radar_path, "\n")
}

generate_radar_chart(prepare_radar_data(mean), "Mean", "Radar_Chart_Environmental_Features_Mean.png")
generate_radar_chart(prepare_radar_data(median), "Median", "Radar_Chart_Environmental_Features_Median.png")
generate_radar_chart(prepare_radar_data(calculate_mode), "Mode", "Radar_Chart_Environmental_Features_Mode.png")


# ===============================================================
# Chapter 03 - Emotional Experience and The Environmental Features
# Section 04 - Emotional Experience and The Environmental Features and Social Demographic Factors
# ===============================================================


cat("Running socio-demographic comparison tests on environmental features...\n")

# Define environmental features and campuses
features <- c("GreenSpaces", "Building", "Sounds")
campuses <- c("Belval", "Kirchberg", "Limpertsberg")

# Prepare results data frames
gender_results_env <- data.frame(Feature = character(), Campus = character(), P_Value = numeric(), Interpretation = character(), stringsAsFactors = FALSE)
edu_results_env <- data.frame(Feature = character(), Campus = character(), P_Value = numeric(), Interpretation = character(), stringsAsFactors = FALSE)

# Loop through each feature-campus combination
for (feature in features) {
  for (campus in campuses) {
    
    col_name <- paste0("nvalue_", feature, campus, "Campus")
    
    if (!col_name %in% colnames(data)) next
    
    # -----------------------------
    # GENDER COMPARISON (Mann–Whitney U or t-test)
    # -----------------------------
    male_vals <- data[[col_name]][data$Gender == "Male"]
    female_vals <- data[[col_name]][data$Gender == "Female"]
    
    if (length(male_vals) >= 2 && length(female_vals) >= 2) {
      sw_m <- shapiro.test(male_vals)
      sw_f <- shapiro.test(female_vals)
      
      test <- if (sw_m$p.value < 0.05 || sw_f$p.value < 0.05) {
        wilcox.test(male_vals, female_vals)
      } else {
        t.test(male_vals, female_vals)
      }
      
      p_val <- round(test$p.value, 4)
      interpretation <- if (p_val < 0.05) {
        "Significant"
      } else if (p_val < 0.10) {
        "Trend"
      } else {
        "Not significant"
      }
      
      gender_results_env <- rbind(gender_results_env, data.frame(
        Feature = feature, Campus = campus, P_Value = p_val, Interpretation = interpretation, stringsAsFactors = FALSE
      ))
    }
    
    # -----------------------------
    # EDUCATION COMPARISON (Kruskal–Wallis)
    # -----------------------------
    df <- data[, c("HighestLevelOfEducation", col_name)]
    df <- df[complete.cases(df), ]
    
    if (length(unique(df$HighestLevelOfEducation)) >= 2) {
      ktest <- kruskal.test(df[[col_name]] ~ df$HighestLevelOfEducation)
      p_val <- round(ktest$p.value, 4)
      interpretation <- if (p_val < 0.05) {
        "Significant"
      } else if (p_val < 0.10) {
        "Trend"
      } else {
        "Not significant"
      }
      
      edu_results_env <- rbind(edu_results_env, data.frame(
        Feature = feature, Campus = campus, P_Value = p_val, Interpretation = interpretation, stringsAsFactors = FALSE
      ))
    }
  }
}

# Save results
write.csv(gender_results_env, "C:/Users/nima/Desktop/data analysis/github/data/genderVSenvfeatures.csv", row.names = FALSE)
write.csv(edu_results_env, "C:/Users/nima/Desktop/data analysis/github/data/educationVSenvfeatures.csv", row.names = FALSE)

# View output
print(gender_results_env)
print(edu_results_env)

cat("Socio-demographic feature comparisons completed.\n")



# Add formatted p-value labels
gender_results_env <- gender_results_env %>% mutate(Label = sprintf("%.3f", P_Value))
edu_results_env <- edu_results_env %>% mutate(Label = sprintf("%.3f", P_Value))

# Gender heatmap
gender_heatmap_env <- ggplot(gender_results_env, aes(x = Campus, y = Feature, fill = P_Value)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Label), color = "white", size = 3) +
  scale_fill_gradient(low = "#D0E1F4", high = "#3C5A78", limits = c(0, 1)) +
  labs(
    title = "Gender Differences in Environmental Feature Perception",
    subtitle = "P-values of Statistical Tests by Campus",
    x = "Campus", y = "Environmental Feature"
  ) +
  theme_minimal()

# Education heatmap
edu_heatmap_env <- ggplot(edu_results_env, aes(x = Campus, y = Feature, fill = P_Value)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Label), color = "white", size = 3) +
  scale_fill_gradient(low = "#D0E1F4", high = "#3C5A78", limits = c(0, 1)) +
  labs(
    title = "Education Level Differences in Environmental Feature Perception",
    subtitle = "P-values of Kruskal–Wallis Tests by Campus",
    x = "Campus", y = "Environmental Feature"
  ) +
  theme_minimal()

# Save both heatmaps
ggsave(file.path(output_folder, "heatmap_genderVSenvfeatures.tif"), gender_heatmap_env, width = 8, height = 6, dpi = 300, device = "tiff")
ggsave(file.path(output_folder, "heatmap_educationVSenvfeatures.tif"), edu_heatmap_env, width = 8, height = 6, dpi = 300, device = "tiff")

cat("Heatmaps saved to output folder.\n")


# ===============================================================
# Chapter 03 - Emotional Experience and The Environmental Features
# Section 05 - Section 05 - Post-hoc Analysis: Dunn's Test for Pairwise Comparisons
# ===============================================================
# -------------------------------------------------------------------
# Step 01 - Normality Testing
# -------------------------------------------------------------------
cat("\n============================================================\n")
cat("Section 04 - Normality Testing: Shapiro-Wilk Test for Environment Features\n")
cat("============================================================\n")

normality_columns <- c(
  "nvalue_GreenSpacesBelvalCampus", "nvalue_GreenSpacesKirchbergCampus", "nvalue_GreenSpacesLimpertsbergCampus",
  "nvalue_BuildingBelvalCampus", "nvalue_BuildingKirchbergCampus", "nvalue_BuildingLimpertsbergCampus",
  "nvalue_SoundsBelvalCampus", "nvalue_SoundsKirchbergCampus", "nvalue_SoundsLimpertsbergCampus"
)

shapiro_results <- data.frame(
  Feature = character(),
  W_Statistic = numeric(),
  P_Value = numeric(),
  Normality = character(),
  stringsAsFactors = FALSE
)

for (col in normality_columns) {
  test_data <- na.omit(data[[col]])
  if (length(test_data) >= 3) {
    test_result <- shapiro.test(test_data)
    shapiro_results <- rbind(shapiro_results, data.frame(
      Feature = col,
      W_Statistic = as.numeric(test_result$statistic),
      P_Value = as.numeric(test_result$p.value),
      Normality = ifelse(test_result$p.value > 0.05, "Yes", "No")
    ))
  } else {
    shapiro_results <- rbind(shapiro_results, data.frame(
      Feature = col,
      W_Statistic = NA,
      P_Value = NA,
      Normality = "Insufficient Data"
    ))
  }
}

output_shapiro <- file.path(data_folder, "shapiro_wilk_results_environmentfeatures.csv")
write.csv(shapiro_results, output_shapiro, row.names = FALSE)
cat("Shapiro-Wilk results saved to:", output_shapiro, "\n")
print(shapiro_results)

# -------------------------------------------------------------------
# Step 02 - Kruskal-Wallis and Post-hoc Dunn’s Test for Environment Features
# -------------------------------------------------------------------
cat("\n============================================================\n")
cat("Section 05 - Kruskal-Wallis and Post-hoc Dunn’s Test for Environment Features\n")
cat("============================================================\n")

long_data <- data[, normality_columns] %>%
  pivot_longer(cols = everything(), names_to = "Category", values_to = "Value") %>%
  mutate(
    Feature = case_when(
      grepl("GreenSpaces", Category) ~ "Greenspaces",
      grepl("Building", Category) ~ "Buildings",
      grepl("Sounds", Category) ~ "Sounds"
    ),
    Campus = case_when(
      grepl("Belval", Category) ~ "Belval",
      grepl("Kirchberg", Category) ~ "Kirchberg",
      grepl("Limpertsberg", Category) ~ "Limpertsberg"
    )
  )

kruskal_results <- data.frame(
  Feature = character(),
  Chi_Square = numeric(),
  P_Value = numeric(),
  Significance = character(),
  stringsAsFactors = FALSE
)

features_to_analyze <- c("Greenspaces", "Buildings", "Sounds")

for (feature in features_to_analyze) {
  test_data <- long_data %>% filter(Feature == feature)
  kruskal_test <- kruskal.test(Value ~ Campus, data = test_data)
  kruskal_results <- rbind(kruskal_results, data.frame(
    Feature = feature,
    Chi_Square = as.numeric(kruskal_test$statistic),
    P_Value = as.numeric(kruskal_test$p.value),
    Significance = ifelse(kruskal_test$p.value < 0.05, "Significant", "Not Significant")
  ))
}

output_kruskal <- file.path(data_folder, "kruskal_wallis_results_environmentfeatures.csv")
write.csv(kruskal_results, output_kruskal, row.names = FALSE)
cat("Kruskal-Wallis results saved to:", output_kruskal, "\n")
print(kruskal_results)

# -------------------------------------------------------------------
# Step 03 - Post-hoc Dunn's Test
# -------------------------------------------------------------------
posthoc_results <- data.frame(
  Feature = character(),
  Comparison = character(),
  Z_Value = numeric(),
  P_Value = numeric(),
  Adjusted_P_Value = numeric(),
  Significance = character(),
  stringsAsFactors = FALSE
)

for (feature in features_to_analyze) {
  test_data <- long_data %>% filter(Feature == feature)
  dunn_test <- dunnTest(Value ~ Campus, data = test_data, method = "bonferroni")
  dunn_results <- dunn_test$res
  posthoc_results <- rbind(posthoc_results, data.frame(
    Feature = feature,
    Comparison = dunn_results$Comparison,
    Z_Value = as.numeric(dunn_results$Z),
    P_Value = as.numeric(dunn_results$P.unadj),
    Adjusted_P_Value = as.numeric(dunn_results$P.adj),
    Significance = ifelse(dunn_results$P.adj < 0.05, "Significant", "Not Significant")
  ))
}

output_posthoc <- file.path(data_folder, "posthoc_dunn_results_environmentfeatures.csv")
write.csv(posthoc_results, output_posthoc, row.names = FALSE)
cat("Post-hoc Dunn’s test results saved to:", output_posthoc, "\n")
print(posthoc_results)


# ===============================================================
# Chapter 04 - Participants’ Preferences
# Section 01 - Analysis of Campus Preference (preference01) by Gender and Education
# ===============================================================

# Prepare result data frames
preference_gender_result <- data.frame(
  Variable = character(),
  Test = character(),
  P_Value = numeric(),
  Interpretation = character(),
  stringsAsFactors = FALSE
)

preference_edu_result <- data.frame(
  Variable = character(),
  Test = character(),
  P_Value = numeric(),
  Interpretation = character(),
  stringsAsFactors = FALSE
)

# -----------------------
# Gender vs. preference01
# -----------------------
gender_pref_table <- table(data$Gender, data$preference01)
print(gender_pref_table)

# Choose appropriate test
if (all(chisq.test(gender_pref_table)$expected >= 5)) {
  test_gender <- chisq.test(gender_pref_table)
  test_type_gender <- "Chi-squared Test"
} else {
  test_gender <- fisher.test(gender_pref_table)
  test_type_gender <- "Fisher's Exact Test"
}

# Interpret
p_gender <- test_gender$p.value
interp_gender <- if (p_gender < 0.05) {
  "Significant gender difference in campus preference."
} else if (p_gender < 0.10) {
  "Trend toward gender difference in campus preference."
} else {
  "No significant gender difference in campus preference."
}

# Save result
preference_gender_result <- rbind(preference_gender_result, data.frame(
  Variable = "Gender",
  Test = test_type_gender,
  P_Value = p_gender,
  Interpretation = interp_gender,
  stringsAsFactors = FALSE
))

# -------------------------------
# Education vs. preference01
# -------------------------------
edu_pref_table <- table(data$HighestLevelOfEducation, data$preference01)
print(edu_pref_table)

# Choose appropriate test
if (all(chisq.test(edu_pref_table)$expected >= 5)) {
  test_edu <- chisq.test(edu_pref_table)
  test_type_edu <- "Chi-squared Test"
} else {
  test_edu <- fisher.test(edu_pref_table)
  test_type_edu <- "Fisher's Exact Test"
}

# Interpret
p_edu <- test_edu$p.value
interp_edu <- if (p_edu < 0.05) {
  "Significant education-level difference in campus preference."
} else if (p_edu < 0.10) {
  "Trend toward education-level difference in campus preference."
} else {
  "No significant education-level difference in campus preference."
}

# Save result
preference_edu_result <- rbind(preference_edu_result, data.frame(
  Variable = "HighestLevelOfEducation",
  Test = test_type_edu,
  P_Value = p_edu,
  Interpretation = interp_edu,
  stringsAsFactors = FALSE
))

# ------------------------
# Save results to CSV files
# ------------------------
write.csv(preference_gender_result, file.path(data_folder, "gender_preference_summary.csv"), row.names = FALSE)
write.csv(preference_edu_result, file.path(data_folder, "education_preference_summary.csv"), row.names = FALSE)

cat("\n Participants' Preferences analysis results saved to CSV files:\n")
cat(" - gender_preference_summary.csv\n")
cat(" - education_preference_summary.csv\n")

# Print the results
print(preference_gender_result)
print(preference_edu_result)

# ===============================================================
# Chapter 05 - Activity Domain Analysis by Campus Preferences 
# section 01 -Descriptive statistics (P01-P03)
# ===============================================================

cat("\n============================================================\n")
cat("Chapter 05 - Activity Domain Analysis by Campus Preferences (P01-P03)\n")
cat("============================================================\n")

# Define Summarization Function
summarise_by_preference <- function(data, preference_var, campus) {
  data %>%
    filter(!!sym(preference_var) == campus) %>%
    pivot_longer(
      cols = c(necessaryactivities, optionalactivities, socialactivities),
      names_to = "Activity_Type",
      values_to = "Frequency"
    ) %>%
    group_by(Campus = paste(campus, preference_var), Activity_Type, Frequency) %>%
    summarise(Count = n(), .groups = "drop")
}

# List of Preferences to Process
preferences <- c("preference01", "preference02", "preference03")
campuses <- c("Belval Campus", "Kirchberg Campus", "Limpertsberg Campus")

for (pref in preferences) {
  cat("\nProcessing", pref, "\n")
  all_summaries <- list()
  
  # Summarize for Each Campus
  for (campus in campuses) {
    summary <- summarise_by_preference(data, pref, campus)
    all_summaries[[campus]] <- summary
  }
  
  # Combine All Campuses
  activity_domain <- bind_rows(all_summaries)
  
  # Reorder Frequency Levels
  activity_domain$Frequency <- factor(
    activity_domain$Frequency,
    levels = c("Daily", "Weekly", "Monthly", "Rarely_Occasionally", "Never")
  )
  
  # Sort Final Table
  activity_domain <- activity_domain %>%
    arrange(Campus, Activity_Type, Frequency)
  
  # Display Summary
  cat("\n=== Combined Summary Table Based on", pref, "===\n")
  print(activity_domain)
  
  # Save to CSV
  output_file <- file.path(data_folder, paste0(pref, "_activity_domain.csv"))
  write.csv(activity_domain, file = output_file, row.names = FALSE)
  cat("\nCombined summary saved to:", output_file, "\n")
}

# ===============================================================
# Chapter 05 - Activity Domain Analysis by Campus Preferences 
# section 02 -Visualize Activity Domain by P01 - Without Kirchberg
# ===============================================================

# Function to Summarize Activities by Preference
summarise_by_preference <- function(data, campus) {
  data %>%
    filter(preference01 == campus) %>%
    pivot_longer(
      cols = c(necessaryactivities, optionalactivities, socialactivities),
      names_to = "Activity_Type",
      values_to = "Frequency"
    ) %>%
    group_by(Campus = paste(campus, "preference01"), Activity_Type, Frequency) %>%
    summarise(Count = n(), .groups = "drop")
}

# Generate Summary for P01 (Belval and Limpertsberg only)
belval_summary <- summarise_by_preference(data, "Belval Campus")
limpertsberg_summary <- summarise_by_preference(data, "Limpertsberg Campus")

# Enforce Correct Frequency Order
frequency_levels <- c("Daily", "Weekly", "Monthly", "Rarely_Occasionally", "Never")
belval_summary$Frequency <- factor(belval_summary$Frequency, levels = frequency_levels)
limpertsberg_summary$Frequency <- factor(limpertsberg_summary$Frequency, levels = frequency_levels)

# Combine Summaries
activity_domain <- bind_rows(belval_summary, limpertsberg_summary) %>%
  arrange(Campus, Activity_Type, Frequency)

# Display Combined Summary
cat("\n=== Combined Summary Table Based on preference01 (Without Kirchberg) ===\n")
print(activity_domain)


# ------------------------
# Color Palettes for Frequencies
# ------------------------

generate_shades <- function(base_color, num_shades = 5) {
  shades <- colorRampPalette(c(base_color, "white"))(num_shades)
  names(shades) <- c("Daily", "Weekly", "Monthly", "Rarely_Occasionally", "Never")
  return(shades)
}

belval_palette <- generate_shades("#C3A6B1")
limpertsberg_palette <- generate_shades("#A8C4A2")

# ------------------------
# Y-Axis Label Split Function
# ------------------------

split_labels <- function(labels) {
  labels <- gsub("necessaryactivities", "necessary\nactivities", labels)
  labels <- gsub("optionalactivities", "optional\nactivities", labels)
  labels <- gsub("socialactivities", "social\nactivities", labels)
  return(labels)
}

# ------------------------
# Plot Function
# ------------------------

plot_activity <- function(data, campus_name, palette, title, total_participants) {
  ggplot(data, aes(x = split_labels(Activity_Type), y = Count, fill = Frequency)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.7), colour = "black", width = 0.7) +
    geom_text(aes(label = Count), position = position_dodge(width = 0.7), vjust = 0.5, hjust = -0.3, size = 3, colour = "black") +
    scale_fill_manual(values = palette) +
    labs(title = paste0(title, " (", total_participants, " participants)"),
         x = "Activity Domain", y = NULL) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
          legend.position = "bottom", legend.title = element_blank(),
          axis.text.x = element_text(angle = 0, hjust = 0.5),
          axis.text.y = element_text(size = 9)) +
    scale_y_continuous(breaks = seq(0, 12, by = 2), limits = c(0, 12), expand = c(0, 0)) +
    coord_flip()
}

# ------------------------
# Create Individual Plots
# ------------------------

plot_belval_p1 <- plot_activity(belval_summary, "Belval Campus", belval_palette, 
                                "P01 - Belval Campus and Activity Domain", 15)
plot_limpertsberg_p1 <- plot_activity(limpertsberg_summary, "Limpertsberg Campus", limpertsberg_palette, 
                                      "P01 - Limpertsberg Campus and Activity Domain", 26)
# ------------------------
# Combine and Save the Plot
# ------------------------

library(gridExtra)
library(grid)

combined_plot_p1 <- grid.arrange(
  plot_belval_p1,
  plot_limpertsberg_p1,
  nrow = 1,
  ncol = 2,
  top = textGrob("Activity (Walking) Domain and First Preferences for Each Campus", gp = gpar(fontsize = 14, fontface = "bold"))
)

output_tiff_file <- file.path(output_folder, "combined_activity_plot_p1.tiff")

tiff(output_tiff_file, width = 12, height = 6, units = "in", res = 300, compression = "lzw", bg = "transparent")

grid.arrange(
  plot_belval_p1,
  plot_limpertsberg_p1,
  nrow = 1,
  ncol = 2,
  top = textGrob("Activity (Walking) Domain and First Preferences for Each Campus", gp = gpar(fontsize = 14, fontface = "bold"))
)

dev.off()

cat("\nTIFF file with transparent background saved to:", output_tiff_file, "\n")

# ===============================================================
# Chapter 05 - Activity Domain Analysis by Campus Preferences 
# section 03 -Visualize Activity Domain by P02 - With Kirchberg
# ===============================================================


# Function to Summarize Activities by Preference
summarise_by_preference <- function(data, campus) {
  data %>%
    filter(preference02 == campus) %>%
    pivot_longer(
      cols = c(necessaryactivities, optionalactivities, socialactivities),
      names_to = "Activity_Type",
      values_to = "Frequency"
    ) %>%
    group_by(Campus = paste(campus, "preference02"), Activity_Type, Frequency) %>%
    summarise(Count = n(), .groups = "drop")
}

# Generate Summary for P02 (Belval, Kirchberg, and Limpertsberg)
belval_summary_p2 <- summarise_by_preference(data, "Belval Campus")
kirchberg_summary_p2 <- summarise_by_preference(data, "Kirchberg Campus")
limpertsberg_summary_p2 <- summarise_by_preference(data, "Limpertsberg Campus")

# Enforce Correct Frequency Order
frequency_levels <- c("Daily", "Weekly", "Monthly", "Rarely_Occasionally", "Never")
belval_summary_p2$Frequency <- factor(belval_summary_p2$Frequency, levels = frequency_levels)
kirchberg_summary_p2$Frequency <- factor(kirchberg_summary_p2$Frequency, levels = frequency_levels)
limpertsberg_summary_p2$Frequency <- factor(limpertsberg_summary_p2$Frequency, levels = frequency_levels)

# Combine Summaries
activity_domain_p2 <- bind_rows(belval_summary_p2, kirchberg_summary_p2, limpertsberg_summary_p2) %>%
  arrange(Campus, Activity_Type, Frequency)

# Display Combined Summary
cat("\n=== Combined Summary Table Based on preference02 (With Kirchberg) ===\n")
print(activity_domain_p2)

# ------------------------
# Color Palettes for Frequencies
# ------------------------

generate_shades <- function(base_color, num_shades = 5) {
  shades <- colorRampPalette(c(base_color, "white"))(num_shades)
  names(shades) <- c("Daily", "Weekly", "Monthly", "Rarely_Occasionally", "Never")
  return(shades)
}

belval_palette_p2 <- generate_shades("#C3A6B1")
kirchberg_palette_p2 <- generate_shades("#A0B4D3")
limpertsberg_palette_p2 <- generate_shades("#A8C4A2")

# ------------------------
# Plot Function (already defined earlier)
# ------------------------

# ------------------------
# Create Individual Plots with Hardcoded Participant Counts
# ------------------------

plot_belval_p2 <- plot_activity(belval_summary_p2, "Belval Campus", belval_palette_p2, 
                                "P02 - Belval Campus and Activity Domain", 16)

plot_kirchberg_p2 <- plot_activity(kirchberg_summary_p2, "Kirchberg Campus", kirchberg_palette_p2, 
                                   "P02 - Kirchberg Campus and Activity Domain", 19)

plot_limpertsberg_p2 <- plot_activity(limpertsberg_summary_p2, "Limpertsberg Campus", limpertsberg_palette_p2, 
                                      "P02 - Limpertsberg Campus and Activity Domain", 7)

# ------------------------
# Combine and Save the Plot
# ------------------------

library(gridExtra)
library(grid)

combined_plot_p2 <- grid.arrange(
  plot_belval_p2,
  plot_kirchberg_p2,
  plot_limpertsberg_p2,
  nrow = 2,
  ncol = 2,
  top = textGrob("Activity (Walking) Domain and Second Preferences for Each Campus", gp = gpar(fontsize = 14, fontface = "bold"))
)

output_tiff_file_p2 <- file.path(output_folder, "combined_activity_plot_p2.tiff")

tiff(output_tiff_file_p2, width = 12, height = 8, units = "in", res = 300, compression = "lzw", bg = "transparent")

grid.arrange(
  plot_belval_p2,
  plot_kirchberg_p2,
  plot_limpertsberg_p2,
  nrow = 2,
  ncol = 2,
  top = textGrob("Activity (Walking) Domain and Second Preferences for Each Campus", gp = gpar(fontsize = 14, fontface = "bold"))
)

dev.off()

cat("\nTIFF file with transparent background saved to:", output_tiff_file_p2, "\n")

# ===============================================================
# Chapter 06 - Study Behavioural Changes  
# Section 01 -Visualization
# ===============================================================

# ------------------------
# Function to Prepare Grouped Data
# ------------------------
prepare_grouped_data <- function(data, column, campus, category_levels) {
  data %>%
    group_by(!!sym(column)) %>%
    summarise(count = n()) %>%
    mutate(
      Campus = campus,
      category = factor(!!sym(column), levels = category_levels)
    )
}

# ------------------------
# Function to Generate Grouped Bar Charts
# ------------------------
generate_charts <- function(data, preference_column, preference_name, variable_title, category_levels, color_palette, variable, output_prefix) {
  
  campuses <- c("Belval Campus", "Kirchberg Campus", "Limpertsberg Campus")
  combined_data <- bind_rows(
    lapply(campuses, function(campus) {
      subset_data <- filter(data, !!sym(preference_column) == campus)
      prepare_grouped_data(subset_data, variable, campus, category_levels)
    })
  )
  
  combined_data <- combined_data %>%
    mutate(
      category_order = factor(
        interaction(Campus, category),
        levels = paste(rep(campuses, each = length(category_levels)),
                       category_levels, sep = ".")
      )
    )
  
  plot <- ggplot(combined_data, aes(x = Campus, y = count, fill = category_order)) +
    geom_bar(stat = "identity", position = position_dodge()) +
    coord_flip() +
    scale_fill_manual(
      values = color_palette,
      guide = guide_legend(
        title = variable_title,
        title.position = "top",
        ncol = 1  
      )
    ) +
    scale_y_continuous(breaks = seq(0, max(combined_data$count) + 1, by = 1)) + 
    labs(
      title = paste0(variable_title, "\n", preference_name),
      x = "Campus",
      y = "Number of Participants",
      fill = NULL  
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 0, hjust = 1),
      legend.position = "right",
      legend.title = element_text(face = "bold"),
      legend.spacing.y = unit(0.5, "cm"),
      plot.title = element_text(hjust = 0.5, face = "bold")
    )
  
  output_file <- file.path(output_folder, paste0(output_prefix, ".tiff"))
  tiff(output_file, width = 12, height = 8, units = "in", res = 300, compression = "lzw")
  print(plot)
  dev.off()
  
  cat("Grouped bar chart for", variable_title, "saved to:", output_file, "\n")
}

# ------------------------
# Configuration for Variables, Titles, Levels, and Color Palettes
# ------------------------
variables <- c("attitude", "norms", "control", "desire")
titles <- c("Attitude Towards Behaviour", "Subjective Norms", "Perceived Behavioral Control", "Goal Desirability")
category_levels <- list(
  attitude = c("Very unlikely", "Unlikely", "Neither likely nor unlikely", "Likely", "Very likely"),
  norms = c("Strongly disagree", "Disagree", "Neither agree nor disagree", "Agree", "Strongly agree"),
  control = c("Very difficult", "Difficult", "Neither easy nor difficult", "Easy", "Very easy"),
  desire = c("Very weak", "Weak", "Neither strong nor weak", "Strong", "Very strong")
)
color_palettes <- list(
  attitude = c(
    "Belval Campus.Very unlikely" = colorspace::darken("#C3A6B1", 0.4),
    "Belval Campus.Unlikely" = colorspace::darken("#C3A6B1", 0.2),
    "Belval Campus.Neither likely nor unlikely" = "#C3A6B1",
    "Belval Campus.Likely" = colorspace::lighten("#C3A6B1", 0.2),
    "Belval Campus.Very likely" = colorspace::lighten("#C3A6B1", 0.4),
    "Kirchberg Campus.Very unlikely" = colorspace::darken("#A0B4D3", 0.4),
    "Kirchberg Campus.Unlikely" = colorspace::darken("#A0B4D3", 0.2),
    "Kirchberg Campus.Neither likely nor unlikely" = "#A0B4D3",
    "Kirchberg Campus.Likely" = colorspace::lighten("#A0B4D3", 0.2),
    "Kirchberg Campus.Very likely" = colorspace::lighten("#A0B4D3", 0.4),
    "Limpertsberg Campus.Very unlikely" = colorspace::darken("#A8C4A2", 0.4),
    "Limpertsberg Campus.Unlikely" = colorspace::darken("#A8C4A2", 0.2),
    "Limpertsberg Campus.Neither likely nor unlikely" = "#A8C4A2",
    "Limpertsberg Campus.Likely" = colorspace::lighten("#A8C4A2", 0.2),
    "Limpertsberg Campus.Very likely" = colorspace::lighten("#A8C4A2", 0.4)
  ),
  norms = c(
    "Belval Campus.Strongly disagree" = colorspace::darken("#C3A6B1", 0.4),
    "Belval Campus.Disagree" = colorspace::darken("#C3A6B1", 0.2),
    "Belval Campus.Neither agree nor disagree" = "#C3A6B1",
    "Belval Campus.Agree" = colorspace::lighten("#C3A6B1", 0.2),
    "Belval Campus.Strongly agree" = colorspace::lighten("#C3A6B1", 0.4),
    "Kirchberg Campus.Strongly disagree" = colorspace::darken("#A0B4D3", 0.4),
    "Kirchberg Campus.Disagree" = colorspace::darken("#A0B4D3", 0.2),
    "Kirchberg Campus.Neither agree nor disagree" = "#A0B4D3",
    "Kirchberg Campus.Agree" = colorspace::lighten("#A0B4D3", 0.2),
    "Kirchberg Campus.Strongly agree" = colorspace::lighten("#A0B4D3", 0.4),
    "Limpertsberg Campus.Strongly disagree" = colorspace::darken("#A8C4A2", 0.4),
    "Limpertsberg Campus.Disagree" = colorspace::darken("#A8C4A2", 0.2),
    "Limpertsberg Campus.Neither agree nor disagree" = "#A8C4A2",
    "Limpertsberg Campus.Agree" = colorspace::lighten("#A8C4A2", 0.2),
    "Limpertsberg Campus.Strongly agree" = colorspace::lighten("#A8C4A2", 0.4)
  ),
  control = c(
    "Belval Campus.Very difficult" = colorspace::darken("#C3A6B1", 0.4),
    "Belval Campus.Difficult" = colorspace::darken("#C3A6B1", 0.2),
    "Belval Campus.Neither easy nor difficult" = "#C3A6B1",
    "Belval Campus.Easy" = colorspace::lighten("#C3A6B1", 0.2),
    "Belval Campus.Very easy" = colorspace::lighten("#C3A6B1", 0.4),
    "Kirchberg Campus.Very difficult" = colorspace::darken("#A0B4D3", 0.4),
    "Kirchberg Campus.Difficult" = colorspace::darken("#A0B4D3", 0.2),
    "Kirchberg Campus.Neither easy nor difficult" = "#A0B4D3",
    "Kirchberg Campus.Easy" = colorspace::lighten("#A0B4D3", 0.2),
    "Kirchberg Campus.Very easy" = colorspace::lighten("#A0B4D3", 0.4),
    "Limpertsberg Campus.Very difficult" = colorspace::darken("#A8C4A2", 0.4),
    "Limpertsberg Campus.Difficult" = colorspace::darken("#A8C4A2", 0.2),
    "Limpertsberg Campus.Neither easy nor difficult" = "#A8C4A2",
    "Limpertsberg Campus.Easy" = colorspace::lighten("#A8C4A2", 0.2),
    "Limpertsberg Campus.Very easy" = colorspace::lighten("#A8C4A2", 0.4)
  )
)

preferences <- list("preference01", "preference02", "preference03")
preference_names <- list("Preference 01", "Preference 02", "Preference 03")

# ------------------------
# Loop to Generate Charts for Attitude, Norms, Control (P01 only)
# ------------------------
for (variable in c("attitude", "norms", "control")) {
  generate_charts(
    data = data,
    preference_column = "preference01",
    preference_name = "Preference 01",
    variable_title = titles[[which(variables == variable)]],
    category_levels = category_levels[[variable]],
    color_palette = color_palettes[[variable]],
    variable = variable,
    output_prefix = paste0(variable, "_p1")
  )
}


# ===============================================================
# Chapter 06 - Study Behavioural Changes  
# Section 02 - Campus Preference vs. Behavioural Change Variables before Running the Model
# ===============================================================


cat("\n============================================================\n")
cat("Chapter 06 - Section 02 - Campus Preference vs. Behavioural Change Variables\n")
cat("============================================================\n")

# -------------------------------------------------------------------
# Step 01 - Prepare Data: Convert preference01 to factor
# -------------------------------------------------------------------
cat("Converting preference01 to factor if not already\n")
data$preference01 <- as.factor(data$preference01)

# -------------------------------------------------------------------
# Step 02 - Define Numeric Variables for Normality Testing
# -------------------------------------------------------------------
cat("Defining numeric variables to test for normality\n")
numeric_vars <- c("numeric_attitude", "numeric_norms", "numeric_control", "numeric_intention")

# -------------------------------------------------------------------
# Step 03 - Perform Shapiro-Wilk Normality Tests
# -------------------------------------------------------------------
shapiro_output_file <- file.path(output_folder, "P01_Vs_Intention_shapiro_test_results.txt")
con_shapiro <- file(shapiro_output_file, open = "wt", encoding = "UTF-8")
sink(con_shapiro)

cat("Shapiro-Wilk Normality Test Results\r\n")
cat("===================================\r\n\r\n")

for (var in numeric_vars) {
  cat("Variable:", var, "\r\n")
  result <- shapiro.test(data[[var]])
  print(result)
  status <- ifelse(result$p.value > 0.05, "Normally distributed", "Not normally distributed")
  cat("Distribution Status:", status, "\r\n")
  cat("\r\n-----------------------------\r\n\r\n")
}

sink()
close(con_shapiro)
cat("Shapiro-Wilk test results saved to:", shapiro_output_file, "\n")

# -------------------------------------------------------------------
# Step 04 - Prepare Variables for Fisher’s Exact Tests
# -------------------------------------------------------------------
cat("\nPreparing variables for Fisher’s Exact Tests\n")
data$numeric_attitude <- as.factor(data$numeric_attitude)
data$numeric_norms <- as.factor(data$numeric_norms)
data$numeric_control <- as.factor(data$numeric_control)
data$numeric_intention <- as.factor(data$numeric_intention)

test_pairs <- list(
  "Preference vs Intention" = c("preference01", "numeric_intention"),
  "Preference vs Attitude"  = c("preference01", "numeric_attitude"),
  "Preference vs Norms"     = c("preference01", "numeric_norms"),
  "Preference vs Control"   = c("preference01", "numeric_control")
)

# -------------------------------------------------------------------
# Step 05 - Perform Fisher’s Exact Tests and Save Results
# -------------------------------------------------------------------
fisher_output_file <- file.path(output_folder, "P01_Fisher_Test_Results.txt")
con_fisher <- file(fisher_output_file, open = "wt", encoding = "UTF-8")
sink(con_fisher)

cat("Fisher's Exact Test Results\r\n")
cat("============================\r\n\r\n")

for (name in names(test_pairs)) {
  cat(name, "\r\n")
  vars <- test_pairs[[name]]
  tab <- table(data[[vars[1]]], data[[vars[2]]])
  result <- fisher.test(tab, simulate.p.value = TRUE, B = 10000)
  print(result)
  status <- ifelse(result$p.value < 0.05, "Significant (p < 0.05)", "Not significant (p ≥ 0.05)")
  cat("Significance:", status, "\r\n")
  cat("\r\n-----------------------------\r\n\r\n")
}

sink()
close(con_fisher)
cat("Fisher's test results saved to:", fisher_output_file, "\n")


# ===============================================================
# Chapter 06 - Study Behavioural Changes  
# Section 03 - Linear Regression Model
# ===============================================================


cat("\n============================================================\n")
cat("Chapter 06 - Section 03 - Linear Regression Model\n")
cat("============================================================\n")

# -------------------------------------------------------------------
# Step 01 - Run Linear Regression Model (Attitude, Norms, Control)
# -------------------------------------------------------------------

# Re-convert variables to numeric for regression analysis
data$numeric_attitude <- as.numeric(as.character(data$numeric_attitude))
data$numeric_norms <- as.numeric(as.character(data$numeric_norms))
data$numeric_control <- as.numeric(as.character(data$numeric_control))
data$numeric_intention <- as.numeric(as.character(data$numeric_intention))



model1 <- lm(numeric_intention ~ numeric_attitude + numeric_norms + numeric_control, data = data)

# -------------------------------------------------------------------
# Step 02 - Save Model Summary to File
# -------------------------------------------------------------------

model_output_file <- file.path(output_folder, "Intention_regression_model_summary.txt")
con_model <- file(model_output_file, open = "wt", encoding = "UTF-8")
sink(con_model)

cat("Linear Regression Model: Intention ~ Attitude + Norms + Control\n")
cat("============================================================\n\n")

cat("Residuals:\n")
print(summary(model1)$residuals)

cat("\nCoefficients:\n")
summary_model <- summary(model1)
coefs <- summary_model$coefficients
for (i in 1:nrow(coefs)) {
  sig <- ifelse(coefs[i, 4] < 0.001, '***',
                ifelse(coefs[i, 4] < 0.01, '**',
                       ifelse(coefs[i, 4] < 0.05, '*',
                              ifelse(coefs[i, 4] < 0.1, '.', ' '))))
  cat(rownames(coefs)[i], "\t", round(coefs[i, 1], 5), "\t", round(coefs[i, 2], 5), "\t",
      round(coefs[i, 3], 3), "\t", format.pval(coefs[i, 4], digits = 3), "\t", sig, "\n")
}

cat("\nSignificance codes: ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1\n")
cat("\nResidual standard error:", round(summary_model$sigma, 3), "on", summary_model$df[2], "degrees of freedom\n")
cat("Multiple R-squared:", round(summary_model$r.squared, 4), "\n")
cat("Adjusted R-squared:", round(summary_model$adj.r.squared, 4), "\n")
cat("F-statistic:", round(summary_model$fstatistic[1], 1), "on", summary_model$fstatistic[2], "and", summary_model$fstatistic[3], "DF\n")
cat("Model p-value:", format.pval(pf(summary_model$fstatistic[1], summary_model$fstatistic[2], summary_model$fstatistic[3], lower.tail = FALSE), digits = 3), "\n")

sink()
close(con_model)
cat("Regression model summary saved to:", model_output_file, "\n")


# -------------------------------------------------------------------
# Step 03 - MODEL DIAGNOSTICS FOR: Intention ~ Attitude + Norms + Control + Campus Preference
# -------------------------------------------------------------------

# 1. Plot Model Diagnostics
par(mfrow = c(2, 2))
plot(model1)


# 2. Check Multicollinearity with VIF
library(car)
vif_values <- vif(model1)
print(vif_values)

# 3. Normality Test for Residuals
shapiro_result <- shapiro.test(residuals(model1))
print(shapiro_result)

# 4. Homoscedasticity Test (Breusch-Pagan)
library(lmtest)
bp_result <- bptest(model1)
print(bp_result)


# ===============================================================
# Chapter 06 - Study Behavioural Changes  
# Section 03 - Linear Regression Model + Social Demographic Variables
# ===============================================================

cat("\n============================================================\n")
cat("Chapter 06 - Section 03 - Linear Regression with Gender & Education\n")
cat("============================================================\n")

# Ensure correct factor levels for regression
data$Gender <- factor(data$Gender, levels = c("Female", "Male"))
data$HighestLevelOfEducation <- factor(data$HighestLevelOfEducation,
                                       levels = c("Bachelor's Degree", "Master's Degree", "Doctorate/PhD"))

# Run the extended regression model
model2 <- lm(numeric_intention ~ numeric_attitude + numeric_norms + numeric_control + Gender + HighestLevelOfEducation, data = data)

# Output file
model2_output_file <- file.path(output_folder, "Intention_regression_model_with_gender_edu.txt")
con_model2 <- file(model2_output_file, open = "wt", encoding = "UTF-8")
sink(con_model2)

# Print formatted summary
cat("Extended Linear Regression Model Summary\n")
cat("=========================================\n\n")

cat("Residuals:\n")
print(summary(model2)$residuals)

cat("\nVariable\tEstimate\tStd. Error\tt value\tPr(>|t|)\tSignificance\n")
summary_model2 <- summary(model2)
coefs <- summary_model2$coefficients
for (i in 1:nrow(coefs)) {
  sig <- ifelse(coefs[i, 4] < 0.001, '***',
                ifelse(coefs[i, 4] < 0.01, '**',
                       ifelse(coefs[i, 4] < 0.05, '*',
                              ifelse(coefs[i, 4] < 0.1, '.', ' '))))
  cat(rownames(coefs)[i], "\t", round(coefs[i, 1], 5), "\t", round(coefs[i, 2], 5), "\t",
      round(coefs[i, 3], 3), "\t", format.pval(coefs[i, 4], digits = 3), "\t", sig, "\n")
}

cat("\nSignificance codes: ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1\n")
cat("\nResidual standard error:", round(summary_model2$sigma, 3), "on", summary_model2$df[2], "degrees of freedom\n")
cat("Multiple R-squared:", round(summary_model2$r.squared, 4), "\n")
cat("Adjusted R-squared:", round(summary_model2$adj.r.squared, 4), "\n")
cat("F-statistic:", round(summary_model2$fstatistic[1], 2), "on", summary_model2$fstatistic[2], "and", summary_model2$fstatistic[3], "DF\n")
cat("Model p-value:", format.pval(pf(summary_model2$fstatistic[1], summary_model2$fstatistic[2], summary_model2$fstatistic[3], lower.tail = FALSE), digits = 4), "\n")

sink()
close(con_model2)

cat("Extended regression model summary saved to:\n", model2_output_file, "\n")

# ---------------------------------------------------------------
# Diagnostics
# ---------------------------------------------------------------
cat("VIF values:\n")
print(vif(model2))

cat("Shapiro-Wilk test:\n")
print(shapiro.test(residuals(model2)))

cat("Breusch-Pagan test:\n")
print(bptest(model2))
