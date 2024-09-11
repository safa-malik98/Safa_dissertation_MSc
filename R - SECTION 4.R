##########################
##########################
###### SECTION 4 ##########
##########################
##########################

# Load libraries
library(ggplot2)
library(dplyr)


###############################################
# Figure 13: Random Forest Feature Importance #
###############################################


# Feature importances for Random Forest
feature_importance_rf <- data.frame(
  Feature = c("Chronic Cardiac Disease: True", "Age Group", "Smoking: True",
              "Liver Disease: Unknown", "Sex: Male", "Income", "Dementia: Unknown", 
              "Chronic Haematological Disease: True"
  ),
  Importance = c(0.310542, 0.309462, 0.228502, 0.101153, 0.023871, 
                 0.021041, 0.003679, 0.001750)
)


# Create the plot
ggplot(feature_importance_rf, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(x = "Feature", y = "Importance") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 14, margin = margin(t = 10)),  
    axis.text.y = element_text(size = 12, margin = margin(r = 10)),  
    axis.title.x = element_text(size = 15, margin = margin(t = 15)),  
    axis.title.y = element_text(size = 15, margin = margin(r = 15)),  
    plot.title = element_blank()  
  )


##########################################
# Figure 14: AdaBoost Feature Importance #
##########################################


# Feature importances for Adaboost
feature_importance_ada <- data.frame(
  Feature = c("Age Group", "AIDS/HIV: Unknown", "Income", "Obesity: True", 
              "Chronic Cardiac Disease: Unknown", "Tuberculosis: Unknown", 
              "Sex: Male", "Other Comorbidity: True", "Malnutrition: Unknown",
              "Smoking: Unknown", "Chronic Kidney Disease: True", 
              "Hypertension: True", "Malignant Neoplasm: Unknown","Diabetes: True", 
              "Pregnancy: Unknown", "Asthma: Unknown", "Dementia: True", 
              "Chronic Neurological Disorder: Unknown", "Liver Disease: Unknown", 
              "Hypertension: Unknown", "Transplantation: Unknown", "Malnutrition: True",
              "Chronic Haematological Disease: Unknown"
  ),
  
  Importance = c(0.1575, 0.1325, 0.1200, 0.0725, 0.0700, 
                 0.0525, 0.0475, 0.0475, 0.0450, 0.0425,
                 0.0375, 0.0300, 0.0225, 0.0175, 0.0175,
                 0.0150, 0.0150, 0.0150, 0.0125, 0.0100,
                 0.0100, 0.0075, 0.0025)
)

# Create the plot
ggplot(feature_importance_ada, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(x = "Feature", y = "Importance") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 14, margin = margin(t = 10)), 
    axis.text.y = element_text(size = 11, margin = margin(r = 10)), 
    axis.title.x = element_text(size = 15, margin = margin(t = 15)),
    axis.title.y = element_text(size = 15, margin = margin(r = 15)),  
    plot.title = element_blank()
  )


#########################################
# Figure 15: XGBoost Feature Importance #
#########################################


# Feature importances for XGBoost
feature_importance_xgb <- data.frame(
  Feature = c("Chronic Cardiac Disease: True", "Age Group", "Obesity: True",
              "AIDS/HIV: Unknown", "Chronic Kidney Disease: True","Tuberculosis: Unknown", 
              "Smoking: Unknown", "Sex: Male","Hypertension: True", "Pregnancy: Unknown", 
              "Other Comorbidity: True", "Rare Diseases and Inborn Errors: Unknown", 
              "Transplantation: Unknown", "Dementia: True", "Malnutrition: Unknown", 
              "Chronic Haematological Disease: Unknown", "Malnutrition: True", 
              "Obesity: Unknown", "Dementia: Unknown", "Chronic Haematological Disease: True",
              "Income", "Other Comorbidity: Unknown", "Rheumatologic Disorder: Unknown", 
              "Rheumatologic Disorder: True", "Smoking: True", "Immunosuppression: True", 
              "Diabetes: True", "Malignant Neoplasm: True", "Chronic Kidney Disease: Unknown",
              "Diabetes: Unknown", "Chronic Neurological Disorder: True",
              "Hypertension: Unknown", "Liver Disease: True", "Chronic Pulmonary Disease: True",
              "Asthma: True", "Liver Disease: Unknown", "Immunosuppression: Unknown",
              "Chronic Neurological Disorder: Unknown","Chronic Cardiac Disease: Unknown",
              "Pregnancy: True", "Chronic Pulmonary Disease: Unknown",
              "Malignant Neoplasm: Unknown", "Transplantation: True", 
              "Asthma: Unknown", "AIDS/HIV: True", "Rare Diseases and Inborn Errors: True"
  ),
  Importance = c(0.305700, 0.085436, 0.076053, 0.073615, 0.044962, 
                 0.044231, 0.035702, 0.028659, 0.028032, 0.027330, 
                 0.025590, 0.018481, 0.018211, 0.016620, 0.013992, 
                 0.013592, 0.013331, 0.012825, 0.011928, 0.011301, 
                 0.010346, 0.009904, 0.006160, 0.005977, 0.005884, 
                 0.005606, 0.005556, 0.005398, 0.004903, 0.004142, 
                 0.003964, 0.003815, 0.003597, 0.003554, 0.002490, 
                 0.002281, 0.001751, 0.001346, 0.001336, 0.001314, 
                 0.001309, 0.001202, 0.001120, 0.000977, 0.000300, 
                 0.000177)
)


# Create the plot
ggplot(feature_importance_xgb, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(x = "Feature", y = "Importance") +
  theme_minimal()+
  theme(
    axis.text.x = element_text(size = 13, margin = margin(t = 10)), 
    axis.text.y = element_text(size = 7, margin = margin(r = 10)),  
    axis.title.x = element_text(size = 13, margin = margin(t = 15)),  
    axis.title.y = element_text(size = 13, margin = margin(r = 15)),  
    plot.title = element_blank()  
  )


#############################################################
# Figure 16: Histogram Gradient Boosting Feature Importance #
#############################################################


# Feature importances for Histogram Gradient Boosting
feature_importance_hist <- data.frame(
  Feature = c("Comorbid Chronic Pulmonary Disease: True", "Chronic Cardiac Disease: Unknown", 
              "Sex: Male", "Diabetes: True", "Age Group", "Liver Disease: True", 
              "Chronic Cardiac Disease: Unknown", "Malnutrition: Unknown", 
              "Malnutrition: True", "Smoking: True", "Chronic Neurological Disorder: Unknown",
              "Hypertension: True", "Diabetes: Unknown", "Liver Disease: Unknown",
              "Immunosuppression: True", "AIDS/HIV: True", "Tuberculosis: True", 
              "Malignant Neoplasm: Unknown", "Transplantation: True", 
              "Rare Diseases and Inborn Errors: True", "Pregnancy: Unknown", 
              "Dementia: Unknown", "Pregnancy: True", "Obesity: True", "Dementia: True",
              "Asthma: Unknown", "Asthma: True", "Malignant Neoplasm: True", 
              "Rheumatologic Disorder: Unknown", "Rare Diseases and Inborn Errors: Unknown",
              "Rheumatologic Disorder: True", "Other Comorbidity: True",
              "Chronic Kidney Disease: Unknown", "Chronic Pulmonary Disease: Unknown", 
              "Chronic Haematological Disease: Unknown", "Obesity: Unknown",
              "Immunosuppression: Unknown", "Transplantation: Unknown", 
              "Hypertension: Unknown", "Chronic Neurological Disorder: True", 
              "Smoking: Unknown", "Other Comorbidity: Unknown", "AIDS/HIV: Unknown", 
              "Tuberculosis: Unknown", "Chronic Kidney Disease: True", "Income",
              "Chronic Cardiac Disease: True"),
  Importance = c(-6.78409902e-05, -4.16567484e-05, -2.97548203e-05, -2.38038562e-05,
                 -1.30921209e-05, -1.19019281e-05, -9.52154249e-06, -5.95096406e-06,
                 -3.57057843e-06, -3.57057843e-06, -3.57057843e-06, -2.38038562e-06,
                 -1.19019281e-06, -1.19019281e-06, -2.22044605e-17, -1.11022302e-17,
                  0.00000000e+00,  0.00000000e+00,  1.19019281e-06,  1.19019281e-06, 
                  2.38038562e-06,  3.57057843e-06,  5.95096406e-06,  5.95096406e-06, 
                  7.14115687e-06,  8.33134968e-06,  1.19019281e-05,  1.19019281e-05, 
                  1.19019281e-05,  1.66626994e-05,  2.14234706e-05,  2.49940490e-05, 
                  2.49940490e-05,  3.21352059e-05,  3.68959771e-05,  3.68959771e-05, 
                  4.28469412e-05,  4.76077124e-05,  5.47488693e-05,  5.59390621e-05, 
                  6.30802190e-05,  8.68840752e-05,  1.20209474e-04,  1.55915258e-04, 
                  2.70173768e-04,  4.35610569e-04,  5.46298500e-04)
)

# Filter out features with very small importance values
feature_importance_hist <- feature_importance_hist %>%
  filter(Importance > 0)

# Create the plot
ggplot(feature_importance_hist, aes(x = reorder(Feature, Importance * 1e4), 
                                    y = Importance * 1e4)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(x = "Feature", y = "Importance (× 10⁴)") + 
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 13, margin = margin(t = 10)), 
    axis.text.y = element_text(size = 10, margin = margin(r = 10)),
    axis.title.x = element_text(size = 14, margin = margin(t = 15)),  
    axis.title.y = element_text(size = 14, margin = margin(r = 15)),  
    plot.title = element_blank()  
  )





