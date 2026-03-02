# Create data frame
student_df <- data.frame(
  name = c("Anastasia", "Dima", "Michael", "Matthew", "Laura", "Kevin", "Jonas"),
  score = c(12.5, 9.0, 16.5, 12.0, 9.0, 8.0, 19.0),
  attempts = c(1, 3, 2, 3, 2, 1, 2)
)

# Add new column
student_df$qualify <- c("yes", "no", "yes", "no", "no", "no", "yes")

# Create new row
new_row <- data.frame(
  name = "Emily",
  score = 14.5,
  attempts = 1,
  qualify = "yes"
)

# Add new row
student_df <- rbind(student_df, new_row)

# Display updated data frame
student_df

# Structure of data frame
str(student_df)

# Convert qualify to factor (IMPORTANT for better summary)
student_df$qualify <- as.factor(student_df$qualify)

# Summary of data frame
summary(student_df)

# Number of rows
nrow(student_df)

# Number of columns
ncol(student_df)

#Observation and Insights
cat("\nOBSERVATION / INSIGHT:\n")

cat("The dataset contains", nrow(student_df), "observations and", ncol(student_df), "variables.\n")

cat("The score and attempts variables are numeric, the name variable is character,",
    "and the qualify variable is a factor.\n")

cat("The highest score is", max(student_df$score),
    "and the lowest score is", min(student_df$score), ".\n")

cat("The average score of the students is", 
    round(mean(student_df$score), 2), ".\n")

cat("Based on the qualification status,",
    summary(student_df$qualify)["yes"], "students qualified and",
    summary(student_df$qualify)["no"], "students did not qualify.\n")

cat("This shows that the qualification outcome is evenly distributed among the students.\n")

