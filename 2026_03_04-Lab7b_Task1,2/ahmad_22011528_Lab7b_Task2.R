
required_packages <- c("readxl", "dplyr", "xlsx", "ggplot2")

for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
  }
}

library(readxl)
library(dplyr)
library(xlsx)
library(ggplot2)

# Read Data created in Task 1
titanic <- read.csv("C:/Users/Waffy/Documents/DS_TEB2043/2026_03_04-Lab7b_Task1,2/titanic_clean.csv", stringsAsFactors = FALSE)

titanic$Survived <- factor(titanic$Survived, levels = c("No", "Yes"))
titanic$Pclass   <- factor(titanic$Pclass, levels = sort(unique(titanic$Pclass)))
titanic$Sex      <- factor(titanic$Sex)
titanic$Embarked <- factor(titanic$Embarked)

# Create helper variables (for more information)

# Family size & age group
titanic <- titanic |>
  mutate(FamilySize = SibSp + Parch + 1,
         AgeGroup = cut(
           Age,
           breaks = c(0, 12, 18, 35, 50, Inf),
           labels = c("Child (0-12)", "Teen (13-18)", "Young Adult (19-35)",
                      "Adult (36-50)", "Senior (51+)")
         ))


# Calculate summary tables using dplyr

# Overall survival rate
overall_surv <- titanic |>
  summarise(
    Total = n(),
    Survived = sum(Survived == "Yes"),
    SurvivalRate = round(100 * Survived / Total, 1)
  )

# Survival by sex
by_sex <- titanic |>
  group_by(Sex) |>
  summarise(
    Total = n(),
    Survived = sum(Survived == "Yes"),
    SurvivalRate = round(100 * Survived / Total, 1)
  )

# Survival by passenger class
by_class <- titanic |>
  group_by(Pclass) |>
  summarise(
    Total = n(),
    Survived = sum(Survived == "Yes"),
    SurvivalRate = round(100 * Survived / Total, 1)
  )

# Survival by embarkation port
by_embarked <- titanic |>
  group_by(Embarked) |>
  summarise(
    Total = n(),
    Survived = sum(Survived == "Yes"),
    SurvivalRate = round(100 * Survived / Total, 1)
  )

# Survival by age group
by_agegroup <- titanic |>
  group_by(AgeGroup) |>
  summarise(
    Total = n(),
    Survived = sum(Survived == "Yes"),
    SurvivalRate = round(100 * Survived / Total, 1)
  )

# Survival by family size 
by_familysize <- titanic |>
  mutate(FamilyType = ifelse(FamilySize == 1, "Alone",
                             ifelse(FamilySize <= 4, "Small family (2-4)",
                                    "Large family (5+)"))) |>
  group_by(FamilyType) |>
  summarise(
    Total = n(),
    Survived = sum(Survived == "Yes"),
    SurvivalRate = round(100 * Survived / Total, 1)
  )

# Export summary tables to excel file using xlsx

report_file <- "titanic_report_summary.xlsx"

wb <- createWorkbook()
sheet <- createSheet(wb, sheetName = "Summary")

writeBlock <- function(sheet, data, title, startRow) {

  # Title row
  titleRow <- createRow(sheet, rowIndex = startRow)
  titleCell <- createCell(titleRow, colIndex = 1)
  setCellValue(titleCell[[1, 1]], title)
  
  addDataFrame(data,
               sheet,
               row.names   = FALSE,
               startRow    = startRow + 2,
               startColumn = 1)

  startRow + 2 + nrow(data) + 2
}

currentRow <- 1
currentRow <- writeBlock(sheet, overall_surv,  "Overall survival",             currentRow)
currentRow <- writeBlock(sheet, by_sex,        "Survival by sex",              currentRow)
currentRow <- writeBlock(sheet, by_class,      "Survival by passenger class",  currentRow)
currentRow <- writeBlock(sheet, by_embarked,   "Survival by embarkation port", currentRow)
currentRow <- writeBlock(sheet, by_agegroup,   "Survival by age group",        currentRow)
currentRow <- writeBlock(sheet, by_familysize, "Survival by family size",      currentRow)

# Adjust columns to be auto-fitted
maxCols <- max(
  ncol(overall_surv),
  ncol(by_sex),
  ncol(by_class),
  ncol(by_embarked),
  ncol(by_agegroup),
  ncol(by_familysize)
)

for (col in 1:maxCols) {
  autoSizeColumn(sheet, colIndex = col)
}

saveWorkbook(wb, report_file)

# Report summary 

cat("============================================================\n")
cat("                  TITANIC SURVIVAL REPORT                  \n")
cat("============================================================\n\n")

# Overall
cat("1. Overall survival:\n")
cat(paste0(
  "- Out of ", overall_surv$Total, " passengers, ",
  overall_surv$Survived, " survived (",
  overall_surv$SurvivalRate, "%).\n\n"
))

# By sex
cat("2. Survival by sex:\n")
for (i in 1:nrow(by_sex)) {
  row <- by_sex[i, ]
  cat(paste0(
    "- ", row$Sex, ": ",
    row$SurvivalRate, "% survived (",
    row$Survived, " out of ", row$Total, ").\n"
  ))
}
cat("\n")

# By class
cat("3. Survival by passenger class:\n")
for (i in 1:nrow(by_class)) {
  row <- by_class[i, ]
  cat(paste0(
    "- Class ", row$Pclass, ": ",
    row$SurvivalRate, "% survived (",
    row$Survived, " out of ", row$Total, ").\n"
  ))
}
cat("\n")

# By embarkation
cat("4. Survival by embarkation port:\n")
for (i in 1:nrow(by_embarked)) {
  row <- by_embarked[i, ]
  port <- as.character(row$Embarked)
  cat(paste0(
    "- Port ", port, ": ",
    row$SurvivalRate, "% survived (",
    row$Survived, " out of ", row$Total, ").\n"
  ))
}
cat("\n")

# By age group
cat("5. Survival by age group:\n")
for (i in 1:nrow(by_agegroup)) {
  row <- by_agegroup[i, ]
  cat(paste0(
    "- ", as.character(row$AgeGroup), ": ",
    row$SurvivalRate, "% survived (",
    row$Survived, " out of ", row$Total, ").\n"
  ))
}
cat("\n")

# By family size
cat("6. Survival by family size:\n")
for (i in 1:nrow(by_familysize)) {
  row <- by_familysize[i, ]
  cat(paste0(
    "- ", as.character(row$FamilyType), ": ",
    row$SurvivalRate, "% survived (",
    row$Survived, " out of ", row$Total, ").\n"
  ))
}
cat("\n")

cat("============================================================\n")
cat("   Detailed summary tables have also been saved to:\n")
cat(paste0("   ", report_file, "\n"))
cat("============================================================\n")

cat("\n\n================ SUMMARY TABLES ================\n\n")

cat("Overall survival table:\n")
print(overall_surv)
cat("\n\nSurvival by sex:\n")
print(by_sex)
cat("\n\nSurvival by passenger class:\n")
print(by_class)
cat("\n\nSurvival by embarkation port:\n")
print(by_embarked)
cat("\n\nSurvival by age group:\n")
print(by_agegroup)
cat("\n\nSurvival by family size:\n")
print(by_familysize)


# Visualisations for each summary

# Survival by sex
ggplot(by_sex, aes(x = Sex, y = SurvivalRate, fill = Sex)) +
  geom_col() +
  labs(title = "Titanic survival rate by sex",
       x = "Sex",
       y = "Survival rate (%)") +
  theme_minimal()

# Survival by passenger class
ggplot(by_class, aes(x = Pclass, y = SurvivalRate, fill = Pclass)) +
  geom_col() +
  labs(title = "Titanic survival rate by passenger class",
       x = "Passenger class",
       y = "Survival rate (%)") +
  theme_minimal()

# Survival by embarkation port
ggplot(by_embarked, aes(x = Embarked, y = SurvivalRate, fill = Embarked)) +
  geom_col() +
  labs(title = "Titanic survival rate by embarkation port",
       x = "Embarkation port",
       y = "Survival rate (%)") +
  theme_minimal()

# Survival by age group
ggplot(by_agegroup, aes(x = AgeGroup, y = SurvivalRate, fill = AgeGroup)) +
  geom_col() +
  labs(title = "Titanic survival rate by age group",
       x = "Age group",
       y = "Survival rate (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Survival by family size
ggplot(by_familysize, aes(x = FamilyType, y = SurvivalRate, fill = FamilyType)) +
  geom_col() +
  labs(title = "Titanic survival rate by family size",
       x = "Family type",
       y = "Survival rate (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

