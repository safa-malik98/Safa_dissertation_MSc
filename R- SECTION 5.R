##########################
##########################
###### SECTION 5 #########
##########################
##########################



# Load the libraries
library(survival)
library(ggplot2)
library(gridExtra)  
library(survminer)
library(readxl)
library(readr)
library(dplyr)
library(cmprsk)


# Load data
data <- read.csv("final_data2.csv")


# Ensure columns are correct type
data$age_group <- factor(data$age_group, levels = age_labels)
data$slider_sex <- factor(data$slider_sex, levels = c("Female", "Male"))
data$dsstdy <- as.numeric(as.character(data$dsstdy))
data$event <- ifelse(data$dsdecod == "DEATH", 1, 0)

#Remove rows with missing days until outcome 
data <- data[!is.na(data$dsstdy), ]

###############################
# Cumulative Incidence Curves #
###############################


# Count the number of rows where dsdecod is "DEATH"
death_count <- data %>%
  filter(dsdecod == "DEATH") %>%
  nrow()

print(death_count)

# Convert dsstdy to numeric
data$dsstdy <- as.numeric(as.character(data$dsstdy))

# Remove rows with missing values in dsstdy
data <- data %>%
  filter(!is.na(dsstdy))

# Filter only relevant events (DEATH, DISCHARGED)
data <- data %>%
  filter(dsdecod %in% c("DEATH", "DISCHARGED")) %>%
  mutate(event = factor(dsdecod, levels = c("DISCHARGED", "DEATH")))

data$slider_sex <- factor(data$slider_sex, levels = c("Female", "Male"))

# Create a status variable where 1 indicates death
data$status <- ifelse(data$event == "DEATH", 1, 2)

# Compute cumulative incidence functions 
cif <- cuminc(ftime = data$dsstdy, fstatus = data$status, group = data$slider_sex)


# Plot the cumulative incidence curves ]
plot(cif, col = c("blue", "red", "green", "purple"),
     xlab = "Days", ylab = "Cumulative Incidence",
     xlim = c(0, 100),  
     lwd = 1.5,           
     lty = 1,          
     curvlab = c("Female Death", "Male Death", "Female Discharge", "Male Discharge"),
     cex = 0.7)   


# Extract the cumulative incidence function for female deaths
female_death_cif <- cif$`Female 1`

# Identify the cumulative incidence at day 40
day_40_index <- which(female_death_cif$time == 40)

# Check if day 40 is directly in the data
if (length(day_40_index) > 0) {
  female_death_at_day_40 <- female_death_cif$est[day_40_index]
  print(paste("Cumulative incidence of female death at day 40:", female_death_at_day_40))
} else {
  # If day 40 is not exactly in the data, interpolate or use the closest day
  closest_day_index <- which.min(abs(female_death_cif$time - 40))
  female_death_at_closest_day <- female_death_cif$est[closest_day_index]
  closest_day <- female_death_cif$time[closest_day_index]
  print(paste("Cumulative incidence of female death
              closest to day 40 (day", closest_day, "):", 
              female_death_at_closest_day))
}

############################################
# Kaplan-Meier Curves for 90-100 Age Group #
############################################


# Subset data for age group 90-100
subset_data <- data[data$age_group == "90-100", ]

# Fit Kaplan-Meier curves for males and females
km_fit <- survfit(Surv(dsstdy, event) ~ slider_sex, data = subset_data)

# Print the p-value:
p_value <- surv_pvalue(km_fit)$pval
print(paste("The p-value for 90-100:", p_value))


# Create the plot
p <- ggsurvplot(
  km_fit,
  data = subset_data,
  pval = FALSE,
  ggtheme = theme_minimal(),
  conf.int = TRUE,
  title = "Age Group: 90-100",
  legend.labs = c("Female", "Male"),
  legend.title = "Sex", 
  risk.table = FALSE,
  xlab = "Time (Days)",
  ylab = "Survival Probability",
  xlim = c(0, 150)
)

# Modify the plot
p$plot <- p$plot + 
  theme(
    legend.text = element_text( size = 11),  
    legend.title = element_text(face = "bold", size = 11),
    plot.title = element_blank()  
  ) + 
  scale_x_continuous(breaks = seq(0, 150, by = 10))  

# Plot the curve
print(p)

# Summarize the Kaplan-Meier fit
km_summary <- summary(km_fit)


# Extract survival probabilities and confidence intervals

# Extract survival probabilities and confidence intervals for males
male_index <- which(km_summary$strata == "slider_sex=Male")
male_survival <- km_summary$surv[male_index]
male_lower <- km_summary$lower[male_index]
male_upper <- km_summary$upper[male_index]
male_times <- km_summary$time[male_index]

# Find the index for day 100
closest_index <- which.min(abs(male_times - 100))

# Get survival probability and confidence interval for day 100
survival_100 <- male_survival[closest_index]
lower_100 <- male_lower[closest_index]
upper_100 <- male_upper[closest_index]

# Print the results
cat("Survival probability for males at day 100:", survival_100, "\n")
cat("95% Confidence interval for males at day 100: [", lower_100, ", ", upper_100, "]\n")


#################################
# Cox Proportional Hazard Model #
#################################


# Ensure the necessary columns are in the correct format
data$age_group <- factor(data$age_group, levels = age_labels)
data$slider_sex <- factor(data$slider_sex, levels = c("Female", "Male"))
data$income <- factor(data$income)
data$MACE <- factor(data$MACE)

# Convert comorbidity columns and replace NAs with 'Missing'
comorbidity_columns <- grep("^comorbid_", colnames(data), value = TRUE)
data[comorbidity_columns] <- lapply(data[comorbidity_columns], function(x) {
  x <- as.character(x)
  x[is.na(x)] <- "Missing"
  factor(x)
})

# Change reference level to comorbidity not present
data[comorbidity_columns] <- lapply(data[comorbidity_columns], function(x) {
  relevel(x, ref = "False")
})

# Remove rows with missing outcome
data <- data[complete.cases(data$dsdecod), ]

# Convert dsstdy to numeric 
data$dsstdy <- as.numeric(as.character(data$dsstdy))

# Create the event indicator column based on dsdecod
data$event <- ifelse(data$dsdecod == "DEATH", 1, 0)

# Create surv object
surv_object <- Surv(time = data$dsstdy, event = data$event)

# Set reference category for age_group and slider_sex
data$age_group <- relevel(data$age_group, ref = "20-30")
data$slider_sex <- relevel(data$slider_sex, ref = "Female")


# Construct formula for Cox model, stratified by income
formula <- as.formula(paste("surv_object ~ age_group +",
                            paste(comorbidity_columns, collapse = " + "), 
                            "+ strata(income) + strata(slider_sex)"))

# Fit the model
cox_model <- coxph(formula, data = data)

# Get summary model and extract HRs and CIs
summary_model <- summary(cox_model)

hr <- exp(summary_model$coefficients[, "coef"])
ci_low <- summary_model$conf.int[, "lower .95"]
ci_high <- summary_model$conf.int[, "upper .95"]

# Create table for HRs and CIs
hr_table <- data.frame(
  Variable = rownames(summary_model$coefficients),
  Hazard_Ratio = hr,
  CI_Lower = ci_low,
  CI_Upper = ci_high
)

# Filter table to keep 'True' values 
hr_table_filtered <- hr_table[grep("True$", hr_table$Variable), ]


# Change labels
comorbidity_labels <- c(
  comorbid_aids_hivTrue = "AIDS/HIV",
  comorbid_asthmaTrue = "Asthma",
  comorbid_chronic_cardiac_diseaseTrue = "Chronic Cardiac Disease",
  comorbid_chronic_haematological_diseaseTrue = "Chronic Haematological Disease",
  comorbid_chronic_kidney_diseaseTrue = "Chronic Kidney Disease",
  comorbid_chronic_neurological_disorderTrue = "Chronic Neurological Disorder",
  comorbid_chronic_pulmonary_diseaseTrue = "Chronic Pulmonary Disease",
  comorbid_dementiaTrue = "Dementia",
  comorbid_diabetesTrue = "Diabetes",
  comorbid_hypertensionTrue = "Hypertension",
  comorbid_immunosuppressionTrue = "Immunosuppression",
  comorbid_liver_diseaseTrue = "Liver Disease",
  comorbid_malignant_neoplasmTrue = "Malignant Neoplasm",
  comorbid_malnutritionTrue = "Malnutrition",
  comorbid_obesityTrue = "Obesity",
  comorbid_otherTrue = "Other",
  comorbid_rare_diseases_and_inborn_errors_of_metabolismTrue = 
    "Rare Diseases/Inborn Errors of Metabolism",
  comorbid_rheumatologic_disorderTrue = "Rheumatologic Disorder",
  comorbid_smokingTrue = "Smoking",
  comorbid_transplantationTrue = "Transplantation",
  comorbid_tuberculosisTrue = "Tuberculosis",
  comorbid_pregnancyTrue = "Pregnancy"
)

hr_table_filtered$Variable <- comorbidity_labels[hr_table_filtered$Variable]


# Convert table to csv file
write.csv(hr_table_filtered, "hazard_ratio.csv", row.names = FALSE)

# Load the data from the Excel file
data_hazard <- read_csv("hazard_ratio.csv")

# Create the plot
ggplot(data_hazard, aes(x = reorder(Variable, Hazard_Ratio), y = Hazard_Ratio)) +
  geom_point(size = 2, color = "black") +
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), width = 0.2, color = "red") +
  coord_flip() +  
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 13, margin = margin(t = 10)),  
    axis.text.y = element_text(size = 10, margin = margin(r = 10)),  
    axis.title.x = element_text(size = 14, margin = margin(t = 15)),  
    axis.title.y = element_text(size = 14, margin = margin(r = 15)),  
    plot.title = element_blank()  
  ) +
  labs(
    x = "Comorbidities",
    y = "Hazard Ratio"
  ) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "blue") 



