# Load Dataset
data("Theoph")

print(head(Theoph))
str(Theoph)

# Remove duplicate rows
# Keep only one row per subject
# Keep only the Subject, Wt, and Dose columns
theoph_subj <- Theoph[!duplicated(Theoph$Subject), c("Subject", "Wt", "Dose")]
print(theoph_subj)

# Linear model: oral dose (mg/kg) ~ weight (kg)
model_theoph <- lm(Dose ~ Wt, data = theoph_subj)

print(model_theoph)

model_summary <- summary(model_theoph)
print(model_summary)

# ========= Discussion of Linear Model Summary =========
# - Call / Residuals:
#   Residuals represent the difference between observed and predicted Dose values.
#   A small and balanced residual range suggests a reasonable fit, while large
#   residuals may indicate outliers or poor model fit for certain observations.
#
# - Coefficients:
#   * (Intercept): Predicted Dose when Wt = 0 kg. This is not clinically meaningful,
#     but serves as a mathematical baseline for the regression line.
#
#   * Wt: Represents the estimated change in Dose (mg/kg) for every 1 kg increase
#     in body weight. A positive value indicates Dose increases with Wt, while a
#     negative value indicates the opposite relationship.
#
#   * Std. Error / t value / Pr(>|t|):
#     These test the null hypothesis that the coefficient = 0.
#     A small p-value (typically < 0.05) for Wt suggests that body weight
#     significantly contributes to explaining variation in Dose.
#     However, interpretation should consider the small sample size (n = 12).
#
# - Residual Standard Error (RSE):
#   Indicates the typical prediction error in Dose (mg/kg). Lower values imply
#   better model accuracy.
#
# - Multiple R-squared:
#   Measures the proportion of variance in Dose explained by Wt (ranges from 0 to 1).
#   Higher values indicate a stronger relationship.
#
# - Adjusted R-squared:
#   Adjusted version of R-squared that accounts for the number of predictors,
#   providing a more reliable measure for model evaluation.
#
# - F-statistic:
#   Tests whether the model provides a better fit than an intercept-only model.
#   A small p-value indicates the model is statistically significant overall.
#
# - Limitation:
#   The observed Wt values are within a limited range in the dataset.
#   Predictions for 90–100 kg are extrapolations beyond the data range,
#   and therefore may be less reliable and should be interpreted cautiously.
# ======================================================

# Visualization 
plot(
  x = theoph_subj$Wt,
  y = theoph_subj$Dose,
  col = "blue",
  pch = 16,
  cex = 1.1,
  main = "Linear Regression: Dose vs Weight",
  xlab = "Weight (kg)",
  ylab = "Dose of theophylline (mg/kg, oral)"
)
abline(model_theoph, col = "red", lwd = 2)
grid()

# Smoother scatter plot
scatter.smooth(
  x = theoph_subj$Wt,
  y = theoph_subj$Dose,
  col = "blue",
  main = "Dose vs Weight (with smooth trend)",
  xlab = "Weight (kg)",
  ylab = "Dose (mg/kg)"
)

# Predictions: oral dose (mg/kg) for weights 90, 95, 100 kg
new_weights <- data.frame(Wt = c(90, 95, 100))
predicted_dose <- predict(model_theoph, newdata = new_weights)

prediction_table <- data.frame(
  Weight_kg = new_weights$Wt,
  Predicted_Dose_mg_per_kg = predicted_dose
)

print("Predicted oral dose (mg/kg) for given weights:")
print(prediction_table)

