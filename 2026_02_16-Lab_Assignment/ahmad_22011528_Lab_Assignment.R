# List of required packages
required_packages <- c("readr", "dplyr", "stringr", "tidyr", "lubridate")

# Install any packages that are not already installed
for(pkg in required_packages) {
  if(!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
  }
}

library(readr)
library(dplyr)
library(stringr)
library(tidyr)
library(lubridate)

# 1. Load raw dataset and set column names
df <- read_csv(
  "Unclean Dataset.csv",
  col_names = c(
    "Student_ID", "First_Name", "Last_Name",
    "Age", "Gender", "Course",
    "Enrollment_Date", "Total_Payments"
  ),
  skip = 1,
  locale = locale(encoding = "ISO-8859-1"),
  show_col_types = FALSE
) %>%
  mutate(across(everything(), as.character))

# 2. Remove rows that are empty
df <- df %>%
  filter(
    !if_all(
      everything(),
      ~ is.na(.x) | str_trim(.x) == ""
    )
  )

# 3. Fix rows where all info is inside Student_ID using pipes
#    e.g. "101 | John | Smith | 22 | M | Data Science | 2022-05-15 | $1200"
pipe_rows <- str_detect(df$Student_ID, "\\|")
pipe_rows_logical <- !is.na(pipe_rows) & pipe_rows

if (any(pipe_rows_logical)) {
  df_pipe <- df %>%
    filter(pipe_rows_logical) %>%
    transmute(raw = Student_ID) %>%
    separate(
      raw,
      into = c(
        "Student_ID", "First_Name", "Last_Name",
        "Age", "Gender", "Course",
        "Enrollment_Date", "Total_Payments"
      ),
      sep = "\\|",
      fill = "right",
      extra = "drop"
    ) %>%
    mutate(across(everything(), ~ str_squish(.x)))
  
  df[pipe_rows_logical, c(
    "Student_ID", "First_Name", "Last_Name",
    "Age", "Gender", "Course",
    "Enrollment_Date", "Total_Payments"
  )] <- df_pipe
}

# 4. Clean Gender & Age:
#    - Any number from either column goes to Age
#    - Any 'F'/'M' from either column goes to Gender
df <- df %>%
  mutate(
    age_from_age = str_extract(Age, "\\d+"),
    age_from_gender = str_extract(Gender, "\\d+"),
    gender_from_age = str_extract(Age, "[MF]"),
    gender_from_gender = str_extract(Gender, "[MF]"),
    Age = coalesce(age_from_age, age_from_gender),
    Gender = coalesce(gender_from_gender, gender_from_age),
    Age = str_replace_all(Age, "[^0-9]", "")
  ) %>%
  select(-age_from_age, -age_from_gender, -gender_from_age, -gender_from_gender)

# 5. Clean Total_Payments: remove currency symbols, spaces, commas
df <- df %>%
  mutate(
    Total_Payments = str_squish(Total_Payments),
    Total_Payments = str_replace_all(Total_Payments, ",", ""),
    Total_Payments = str_replace_all(Total_Payments, "[^0-9.]", "")
  )

# 6. Standardise dates use DD/MM/YYYY
df <- df %>%
  mutate(
    Enrollment_Date = str_squish(Enrollment_Date),
    Enrollment_Date = na_if(Enrollment_Date, ""),
    Enrollment_Date = if_else(
      Enrollment_Date %in% c("NA", "NaN"),
      NA_character_,
      Enrollment_Date
    ),
    Enrollment_Date = parse_date_time(
      Enrollment_Date,
      orders = c("Y-m-d", "d-m-y", "d-m-Y", "m-d-y", "m-d-Y")
    ),
    Enrollment_Date = as_date(Enrollment_Date)
  )

# 7. Trim spaces from names and course
df <- df %>%
  mutate(
    First_Name = str_squish(First_Name),
    Last_Name = str_squish(Last_Name),
    Course = str_squish(Course)
  )

# 8. Convert to numeric where appropriate
df <- df %>%
  mutate(
    Student_ID = suppressWarnings(
      as.integer(str_replace_all(Student_ID, "[^0-9]", ""))
    ),
    Age = suppressWarnings(as.integer(Age)),
    Total_Payments = suppressWarnings(as.numeric(Total_Payments))
  ) %>%
  # 8b. Remove any rows that still have missing values in any column
  tidyr::drop_na()

cat("=== Initial Data Overview ===\n")
str(df)
cat("\nSummary:\n")
print(summary(df))

# 9. Remove duplicates
dup_before <- sum(duplicated(df))
cat("\nDuplicates before removal:", dup_before, "\n")

df_cleaned <- distinct(df)

dup_after <- sum(duplicated(df_cleaned))
cat("Duplicates after removal:", dup_after, "\n")
cat("Duplicate rows removed successfully.\n")

# 10. Save cleaned dataset (missing values as blank)
write_csv(df_cleaned, "Cleaned_Dataset.csv", na = "")

cat("\nCleaned dataset saved as 'Cleaned_Dataset.csv'.\n")
cat("Duplicates in final dataset:", sum(duplicated(df_cleaned)), "\n")

cat("\n=== Missing Values per Column ===\n")
print(colSums(is.na(df_cleaned)))

cat("\n=== Final Data Structure ===\n")
str(df_cleaned)

cat("\n=== Final Summary ===\n")
print(summary(df_cleaned))

cat("\n=== First 6 Rows ===\n")
print(head(df_cleaned))

