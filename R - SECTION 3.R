##########################
##########################
###### SECTION 3 #########
##########################
##########################


# Load libraries
library(survival)
library(ggplot2)
library(VennDiagram)
library(dplyr)
library(broom)
library(scales)
library(readr)


# Load the data
data <- read.csv("data.csv")


###########################################
# Figure 2 : Percentage of each diagnosis #
###########################################


# Filter for true MACE cases
true_mace_cases <- data %>% filter(MACE == "True")

# Calculate total number of true MACE cases
total_true_mace <- nrow(true_mace_cases)

# Define the conditions
conditions <- c('Cardiac.arrest', 'Cardiac.Arrhythmia', 'Stroke', 
                'Heart.Failure', 'Myocardial.Injury')

# Calculate the percentage of true MACE cases for each condition
condition_percentages <- sapply(conditions, function(condition) {
  condition_count <- sum(true_mace_cases[[condition]] == "True")
  percentage <- (condition_count / total_true_mace) * 100
  return(percentage)
})

# Create data frame for plotting
condition_df <- data.frame(
  Condition = conditions,
  Percentage = condition_percentages
)

# Print the results
for (i in seq_along(conditions)) {
  cat(sprintf("Percentage of MACE cases with %s: %.2f%%\n", 
              conditions[i], condition_percentages[i]))
}

# Rename the conditions in the data frame
condition_df$Condition <- c("Cardiac Arrest", "Cardiac Arrhythmia", 
                            "Heart Failure", "Myocardial Injury", "Stroke")

# Plotting the results
ggplot(condition_df, aes(x = Condition, y = Percentage)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(x = "Complication", y = "Percentage (%)") +
  theme_minimal() +
  theme(
    plot.title = element_blank(),
    axis.title.x = element_text(size = 14, margin = margin(t = 12)),  
    axis.title.y = element_text(size = 14, margin = margin(r = 15)),  
    axis.text = element_text(size = 12), 
  )

###########################
# Figure 3: Venn diagram #
###########################


# Filter for instances of MACE
mace_data <- data[data$MACE == 'True', ]

# Define conditions to be included in Venn diagram
condition1 <- mace_data$`Cardiac.arrest` == 'True'
condition2 <- mace_data$`Cardiac.Arrhythmia` == 'True'
condition3 <- mace_data$Stroke == 'True'
condition4 <- mace_data$`Heart.Failure` == 'True'
condition5 <- mace_data$`Myocardial.Injury` == 'True'

# Create Venn diagram for the 5 conditions
venn.plot <- draw.quintuple.venn(
  area1 = sum(condition1),
  area2 = sum(condition2),
  area3 = sum(condition3),
  area4 = sum(condition4),
  area5 = sum(condition5),
  n12 = sum(condition1 & condition2),
  n13 = sum(condition1 & condition3),
  n14 = sum(condition1 & condition4),
  n15 = sum(condition1 & condition5),
  n23 = sum(condition2 & condition3),
  n24 = sum(condition2 & condition4),
  n25 = sum(condition2 & condition5),
  n34 = sum(condition3 & condition4),
  n35 = sum(condition3 & condition5),
  n45 = sum(condition4 & condition5),
  n123 = sum(condition1 & condition2 & condition3),
  n124 = sum(condition1 & condition2 & condition4),
  n125 = sum(condition1 & condition2 & condition5),
  n134 = sum(condition1 & condition3 & condition4),
  n135 = sum(condition1 & condition3 & condition5),
  n145 = sum(condition1 & condition4 & condition5),
  n234 = sum(condition2 & condition3 & condition4),
  n235 = sum(condition2 & condition3 & condition5),
  n245 = sum(condition2 & condition4 & condition5),
  n345 = sum(condition3 & condition4 & condition5),
  n1234 = sum(condition1 & condition2 & condition3 & condition4),
  n1235 = sum(condition1 & condition2 & condition3 & condition5),
  n1245 = sum(condition1 & condition2 & condition4 & condition5),
  n1345 = sum(condition1 & condition3 & condition4 & condition5),
  n2345 = sum(condition2 & condition3 & condition4 & condition5),
  n12345 = sum(condition1 & condition2 & condition3 & condition4 & condition5),
  category = c("Cardiac Arrest", "Cardiac Arrhythmia", "Stroke",
               "Heart Failure", "Myocardial Injury"),
  fill = c("blue", "yellow", "green", "purple", "orange"),
  alpha = 0.5,
  lwd = 1.5,
  label.col = "black",
  cex = 1.1,
  cat.cex = 1.1,
  cat.dist = c(0.05, 0.05, 0.05, 0.05, 0.05),
  cat.pos = 0,
  cat.fontfamily = "sans"
)

grid.newpage()
# Display the Venn diagram
grid.draw(venn.plot)


#########################################
# Figure 4a: Age Distribution for MACE  #
#########################################


# Function to calculate confidence intervals
calculate_confidence_interval <- function(p, n) {
  if (n == 0) {
    return(c(0, 0))
  }
  z <- qnorm(0.975)
  se <- sqrt(p * (1 - p) / n)
  ci_lower <- p - z * se
  ci_upper <- p + z * se
  return(c(ci_lower * 100, ci_upper * 100))
}


data$MACE <- as.logical(data$MACE)

#Find age distribution of MACE cases and 95% confidence intervals
age_group_stats <- data %>%
  group_by(age_group) %>%
  summarise(
    percentage = mean(MACE, na.rm = TRUE) * 100,
    ci_lower = calculate_confidence_interval(mean(MACE, na.rm = TRUE), n())[[1]],
    ci_upper = calculate_confidence_interval(mean(MACE, na.rm = TRUE), n())[[2]]
  )

# Change order of age group labels
age_group_stats$age_group <- factor(age_group_stats$age_group, levels = age_labels)

# Remove rows with NA values
age_group_stats_clean <- na.omit(age_group_stats)

# Plot the bar chart
ggplot(age_group_stats_clean, aes(x = age_group, y = percentage)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
  labs(x = "Age Group", y = "Percentage (%)") +
  theme_minimal() +
  coord_cartesian(ylim = c(0, max(age_group_stats_clean$ci_upper, na.rm = TRUE))) +
  theme(
    plot.title = element_blank(),  # Remove the title
    axis.text.x = element_text(size = 15, angle = 45, hjust = 1), 
    axis.text.y = element_text(size = 16),  
    axis.title.x = element_text(size = 16, margin = margin(t = 12)),  
    axis.title.y = element_text(size = 16, margin = margin(r = 12)) 
  )


############################################################
# Figure 5: Patients with MACE categorised by age and sex  #
############################################################


# Filter those who develop MACE and group by age group and sex
grouped_data <- data %>%
  filter(MACE == TRUE) %>%
  group_by(age_group, slider_sex) %>%
  summarise(count = n()) %>%
  ungroup()

grouped_data <- grouped_data[!is.na(grouped_data$age_group), ]

# Adjust count for males
grouped_data <- grouped_data %>%
  mutate(count_adj = ifelse(slider_sex == "Male", -count, count))

# Create the pyramid plot
ggplot(grouped_data, aes(x = age_group, y = count_adj, fill = slider_sex)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_y_continuous(labels = abs, limits = c(-6000, 6000)) +
  scale_fill_manual(values = c("Male" = "steelblue", "Female" = "pink"), 
                    name = "Sex") +
  labs(x = "Age Group", y = "Number of MACE Cases") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_blank(),
    axis.text.x = element_text(size = 13), 
    axis.text.y = element_text(size = 13),  
    axis.title.x = element_text(size = 14, margin = margin(t = 12)),  
    axis.title.y = element_text(size = 14, margin = margin(r = 12)),  
    legend.title = element_text(size = 12),  
    legend.text = element_text(size = 12)   
  )


##################################################
# Figure 6: Sex distribution for each diagnosis  #
##################################################


# Define columns of interest
columns_of_interest <- c('Cardiac.arrest', 'Cardiac.Arrhythmia', 'Stroke',
                         'Heart.Failure', 'Myocardial.Injury')

# Initialise results data frames
results <- data.frame(Complication = rep(columns_of_interest, each = 2),
                      Sex = rep(c("Male", "Female"), length(columns_of_interest)),
                      Percentage = NA, CI_Lower = NA, CI_Upper = NA)


# Calculate percentages and confidence intervals for males and females
for (col in columns_of_interest) {
  
  male_data <- data %>% filter(slider_sex == "Male") %>% 
    select(all_of(col)) %>% na.omit()
  female_data <- data %>% filter(slider_sex == "Female") %>% 
    select(all_of(col)) %>% na.omit()
  
  # Calculate for males
  num_true_male <- sum(male_data == TRUE)
  total_male <- nrow(male_data)
  percentage_male <- (num_true_male / total_male) * 100
  ci_male <- calculate_confidence_interval(
    num_true_male / total_male, total_male)
  
  # Calculate for females
  num_true_female <- sum(female_data == TRUE)
  total_female <- nrow(female_data)
  percentage_female <- (num_true_female / total_female) * 100
  ci_female <- calculate_confidence_interval(
    num_true_female / total_female, total_female)
  
  # Store the results
  results[results$Complication == col & results$Sex == "Male",
          c("Percentage", "CI_Lower", "CI_Upper")] <- c(percentage_male, 
                                                        ci_male)
  results[results$Complication == col & results$Sex == "Female", 
          c("Percentage", "CI_Lower", "CI_Upper")] <- c(percentage_female, 
                                                        ci_female)
}


# Plot pyramid plot
ggplot(results, aes(x = Complication, y = Percentage, fill = Sex)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), 
                position = position_dodge(width = 0.8), width = 0.2) +
  labs(x = "Complication", y = "Percentage (%)") +
  scale_fill_manual(values = c("Male" = "steelblue", "Female" = "pink")) +
  theme_minimal() +
  theme(
    plot.title = element_blank(),
    legend.position = "bottom",  
    axis.text.x = element_text(size = 11),  
    axis.text.y = element_text(size = 15),  
    axis.title.x = element_text(size = 13, margin = margin(t = 15)),  
    axis.title.y = element_text(size = 13, margin = margin(r = 15))  
  ) +
  scale_x_discrete(labels = c("Cardiac Arrest", "Cardiac Arrhythmia",
                              "Stroke", "Heart Failure", "Myocardial Injury"))


#############################################
# Figure 7: Patients with MACE by country  #
#############################################


country_counts <- data %>%
  count(slider_country) %>%
  arrange(desc(n))

# Identify countries with fewer than 50 patients
countries_to_replace <- country_counts %>%
  filter(n < 50) %>%
  pull(slider_country)

# Replace these country names with 'Other'
data <- data %>%
  mutate(slider_country = ifelse(slider_country %in% 
                                   countries_to_replace, 'Other', slider_country))

updated_country_counts <- data %>%
  count(slider_country) %>%
  arrange(desc(n))
column_MACE <- 'MACE'

# Calculate the percentage of MACE cases and confidence intervals
country_stats <- data %>%
  group_by(slider_country) %>%
  summarise(
    count_true = sum(get(column_MACE), na.rm = TRUE),
    count_total = sum(!is.na(get(column_MACE)))
  ) %>%
  mutate(
    proportion = count_true / count_total,
    se = sqrt(proportion * (1 - proportion) / count_total),
    ci_lower = proportion - qnorm(0.975) * se,
    ci_upper = proportion + qnorm(0.975) * se,
    proportion = proportion * 100,
    ci_lower = ci_lower * 100,
    ci_upper = ci_upper * 100
  ) %>%
  arrange(desc(proportion))


# Create the bar plot
ggplot(country_stats, aes(x = reorder(slider_country, -proportion), y = proportion)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
  scale_y_continuous( limits = c(0, 60)) +
  labs(x = "Country", y = "Percentage (%) ") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, size = 10),  
    axis.text.y = element_text(size = 12), 
    axis.title.x = element_text(size = 14, margin = margin(t = 12)),  
    axis.title.y = element_text(size = 13, margin = margin(r = 12))  
  )


#################################################
# Figure 8: Patients with MACE by Income group  #
#################################################


# Calculate the percentage of MACE by income group and their confidence intervals
income_group_stats <- data %>%
  group_by(income) %>%
  summarize(
    count_true = sum(MACE == TRUE, na.rm = TRUE),
    count_total = sum(!is.na(MACE)),
    percentage = (count_true / count_total) * 100
  ) %>%
  rowwise() %>%
  mutate(
    ci_values = list(calculate_confidence_interval(percentage / 100, count_total)),
    ci_lower = ci_values[[1]][1],
    ci_upper = ci_values[[2]][1]
  ) %>%
  ungroup()


# Define order for income groups
income_order <- c('Low income', 'Lower middle income', 
                  'Upper middle income', 'High income')
income_group_stats$income <- factor(income_group_stats$income, 
                                    levels = income_order)

# Plot the bar chart
ggplot(income_group_stats, aes(x = income, y = percentage)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
  scale_x_discrete(labels = c("Low", "Lower middle", "Upper middle", "High")) +  
  labs(x = "Income", y = "Percentage (%)") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 12, angle = 0, hjust = 0.5), 
    axis.text.y = element_text(size = 14),  # Set y-axis text size
    axis.title.x = element_text(size = 14, margin = margin(t = 12)), 
    axis.title.y = element_text(size = 14, margin = margin(r = 12))  
  )



################################################
# Figure 10: Univariable Logistic Analysis Plot #
################################################


# Load the odds ratio data
odds_data <- read_csv("odds_ratio_data.csv")

# Replace labels for the comorbidities
odds_data$Comorbidities <- recode(odds_data$Comorbidities,
                                  comorbid_aids_hiv = "AIDS/HIV",
                                  comorbid_asthma = "Asthma",
                                  comorbid_chronic_cardiac_disease = "Chronic Cardiac Disease",
                                  comorbid_chronic_haematological_disease = "Chronic Haematological Disease",
                                  comorbid_chronic_kidney_disease = "Chronic Kidney Disease",
                                  comorbid_chronic_neurological_disorder = "Chronic Neurological Disorder",
                                  comorbid_chronic_pulmonary_disease = "Chronic Pulmonary Disease",
                                  comorbid_dementia = "Dementia",
                                  comorbid_diabetes = "Diabetes",
                                  comorbid_hypertension = "Hypertension", 
                                  comorbid_immunosuppression = "Immunosuppression",
                                  comorbid_liver_disease = "Liver Disease",
                                  comorbid_malignant_neoplasm = "Malignant Neoplasm",
                                  comorbid_malnutrition = "Malnutrition",
                                  comorbid_obesity = "Obesity",
                                  comorbid_other = "Other",
                                  comorbid_rare_diseases = "Rare Diseases",
                                  comorbid_rheumatologic_disorder = "Rheumatologic Disorder",
                                  comorbid_smoking = "Smoking",
                                  comorbid_transplantation = "Transplantation",
                                  comorbid_tuberculosis = "Tuberculosis",
                                  comorbid_pregnancy = "Pregnancy")


# Plot the odds ratios 
ggplot(data, aes(x = reorder(Comorbidities, Odds_Ratio), y = Odds_Ratio)) +
  geom_point(size = 2, color = "black") +
  geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI), width = 0.2, color = "red") +
  coord_flip() +  
  theme_minimal() +
  labs(
    x = "Comorbidities",
    y = "Odds Ratio"
  ) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "blue") +  
  theme(
    axis.text.x = element_text(size = 14),  
    axis.text.y = element_text(size = 10),  
    axis.title.x = element_text(size = 13, margin = margin(t = 12)),  
    axis.title.y = element_text(size = 13, margin = margin(r = 12))   
  )


################################################
# Figure 11: Deaths categorised by Age and Sex #
################################################


# Filter for patients who died and group by age group and sex
grouped_data <- data %>%
  filter(dsdecod == 'DEATH') %>%
  group_by(age_group, slider_sex) %>%
  summarise(count = n()) %>%
  ungroup()

grouped_data <- grouped_data[!is.na(grouped_data$age_group), ]

# Adjust the count for males
grouped_data <- grouped_data %>%
  mutate(count_adj = ifelse(slider_sex == "Male", -count, count))

# Create the pyramid plot
ggplot(grouped_data, aes(x = age_group, y = count_adj, fill = slider_sex)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_y_continuous(labels = abs, limits = c(-12000, 10000)) +
  scale_fill_manual(values = c("Male" = "steelblue", "Female" = "pink"), name = "Sex") +
  labs(x = "Age Group", y = "Number of Deaths") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_blank(),
    axis.text.x = element_text(size = 13),  
    axis.text.y = element_text(size = 13),  
    axis.title.x = element_text(size = 14, margin = margin(t = 12)), 
    axis.title.y = element_text(size = 14, margin = margin(r = 12)),  
    legend.title = element_text(size = 12), 
    legend.text = element_text(size = 12)   
  )


##################################################
# Figure 12: Proportion of Deaths by Age and Sex #
##################################################


# Filter data to include only deaths and discharges
filtered_data <- data %>%
  filter(dsdecod %in% c("DEATH", "DISCHARGED"))


# Calculate number of deaths by age group and sex, and confidence intervals
age_sex_summary <- filtered_data %>%
  group_by(age_group, slider_sex) %>%
  summarise(
    total = n(),
    deaths = sum(dsdecod == 'DEATH'),
    prop = deaths / total,
    se = sqrt(prop * (1 - prop) / total),
    ci_lower = prop - 1.96 * se,
    ci_upper = prop + 1.96 * se
  ) %>%
  mutate(
    ci_lower = pmax(0, ci_lower),  
    ci_upper = pmin(1, ci_upper)
  )

# Separate the data for males and females
male_data <- age_sex_summary %>%
  filter(slider_sex == "Male") %>%
  mutate(
    prop = -prop,
    ci_lower = -ci_lower,
    ci_upper = -ci_upper
  )

female_data <- age_sex_summary %>%
  filter(slider_sex == "Female")

# Combine the male and female data 
age_sex_summary <- bind_rows(male_data, female_data)


# Create the pyramid plot
ggplot(age_sex_summary, aes(x = age_group, y = prop, fill = slider_sex)) +
  geom_bar(stat = "identity", position = "identity", width = 0.7) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2, color = "black") +
  coord_flip() +  
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  scale_fill_manual(values = c("Male" = "steelblue", "Female" = "pink"), name = "Sex") +
  labs(x = "Age Group", y = "Proportion of Deaths") +
  theme_minimal() +
  theme(
    legend.position = "bottom",  
    plot.title = element_blank(), 
    axis.text.x = element_text(size = 13),  
    axis.text.y = element_text(size = 13),  
    axis.title.x = element_text(size = 14, margin = margin(t = 12)), 
    axis.title.y = element_text(size = 14, margin = margin(r = 12)), 
    legend.title = element_text(size = 12), 
    legend.text = element_text(size = 12)  
  )


###############################
# Case fatality data analysis #
###############################


# Calculate median age and interquartile range for death among males
age_stats_male <- filtered_data %>%
  filter(dsdecod == "DEATH", slider_sex == "Male") %>%
  summarise(
    median_age = median(age, na.rm = TRUE),
    IQR_age = IQR(age, na.rm = TRUE)
  )

# Calculate median age and interquartile range for death among females
age_stats_female <- filtered_data %>%
  filter(dsdecod == "DEATH", slider_sex == "Female") %>%
  summarise(
    median_age = median(age, na.rm = TRUE),
    IQR_age = IQR(age, na.rm = TRUE)
  )

# Print the results
print(age_stats_male)
print(age_stats_female)


####################################################
# Figure 13: Proportion of Deaths by Income Group  #
####################################################


filtered_data <- filtered_data %>%
  mutate(income = factor(income, levels = c('Low income', 'Lower middle income',
                                            'Upper middle income', 'High income')))

# Create dataframe with proportion of deaths by income, and confidence intervals
prop_deaths <- filtered_data %>%
  group_by(income) %>%
  summarise(
    total = n(),
    deaths = sum(dsdecod == "DEATH"),
    prop = deaths / total,
    se = sqrt(prop * (1 - prop) / total),
    ci_lower = prop - 1.96 * se,
    ci_upper = prop + 1.96 * se
  ) %>%
  mutate(
    ci_lower = pmax(0, ci_lower), 
    ci_upper = pmin(1, ci_upper)
  )

print(prop_deaths)

# Create the bar plot
ggplot(prop_deaths, aes(x = income, y = prop)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2, color = "black") +
  labs(
    x = "Income Group",
    y = "Proportion of Deaths"
  ) +
  scale_x_discrete(labels = c("Low", "Lower middle", "Upper middle", "High")) +
  theme_minimal() +
  theme(
    plot.title = element_blank(),
    axis.text.x = element_text(size = 13),  
    axis.text.y = element_text(size = 13), 
    axis.title.x = element_text(size = 14, margin = margin(t = 15)),  
    axis.title.y = element_text(size = 14, margin = margin(r = 15))   
  )



