
install.packages("caret")

library(caret)

# Load Dataset 
data(mtcars)
head(mtcars)
str(mtcars)

# vs and am include 0, so log() is undefined on those zeros.

pos_log <- vapply(mtcars, function(x) all(x > 0), logical(1))
mtcars_log <- mtcars
mtcars_log[, pos_log] <- lapply(mtcars[, pos_log, drop = FALSE], log)

# Standard scaling (z-scores)
mtcars_z <- as.data.frame(scale(mtcars))

# Min–max scaling to [0, 1]
pp_range <- preProcess(mtcars, method = c("range"))
mtcars_mm <- predict(pp_range, mtcars)

# Compare: summaries of raw vs each normalization
cat("\n--- summary: raw mtcars ---\n")
print(summary(mtcars))

cat("\n--- summary: log transform (positive columns only; vs, am unchanged) ---\n")
print(summary(mtcars_log))

cat("\n--- summary: standard scaling (scale) ---\n")
print(summary(mtcars_z))

cat("\n--- summary: min–max scaling (caret range) ---\n")
print(summary(mtcars_mm))

# Quick numeric check: means and SDs of mpg across methods
cat("\n--- mpg: mean and SD under each version ---\n")
print(data.frame(
  version = c("raw", "log_pos_cols", "z_score", "min_max"),
  mean = c(
    mean(mtcars$mpg),
    mean(mtcars_log$mpg),
    mean(mtcars_z$mpg),
    mean(mtcars_mm$mpg)
  ),
  sd = c(
    sd(mtcars$mpg),
    sd(mtcars_log$mpg),
    sd(mtcars_z$mpg),
    sd(mtcars_mm$mpg)
  )
))

# Observations / insights
cat(
  "\n========== Discussion (mtcars normalization) ==========\n",
  "1. Log transformation\n",
  "   log() was applied only to columns where every value is strictly positive,\n",
  "   because log(0) is undefined (vs and am contain 0). Those two columns were left\n",
  "   on the original scale so all entries stay valid.\n",
  "   Large-magnitude variables (e.g. disp, hp) are pulled in; units become log-scale,\n",
  "   so interpretation uses transformed magnitudes, not raw mpg or cubic inches.\n\n",
  "2. Standard scaling (scale())\n",
  "   Each column was centered and divided by its SD: mean about 0 and SD about 1 in\n",
  "   this sample. Variables become comparable for distance-based or penalized models.\n",
  "   Values are not bounded; extreme observations can still lie far from zero.\n\n",
  "3. Min–max scaling (caret, method = range)\n",
  "   Each column was linearly rescaled to lie between 0 and 1 using the observed\n",
  "   min and max. Order within a column is preserved, but one very large or small\n",
  "   value can compress the rest of the column into a narrow band.\n\n",
  "4. Compare and contrast\n",
  "   Log mainly changes shape and handles wide positive ranges and skewness; it is\n",
  "   not the same as centering or bounding. Standard scaling aligns spread across\n",
  "   columns without capping values. Min–max forces a shared numeric range [0,1]\n",
  "   and is simple, but it is sensitive to outliers in the min and max. The choice\n",
  "   depends on the next step: log for skewed positive measurements, z-scores for\n",
  "   many classical models and distances, min–max when inputs must stay in a fixed\n",
  "   bounded interval.\n",
  "\n===========================================================\n",
  sep = ""
)

