
# Read data
titanic <- read.csv("C:/Users/Waffy/Documents/DS_TEB2043/2026_03_04-Lab7b_Task1,2/titanic.csv", stringsAsFactors = FALSE)

# Drop unwanted columns
titanic <- subset(titanic, select = -c(Name, Ticket, Fare, Cabin))

# Remove duplicate rows
titanic <- titanic[!duplicated(titanic), ]

# Convert variables to factors
titanic$Survived <- factor(titanic$Survived,
                           levels = c(0, 1),
                           labels = c("No", "Yes"))

titanic$Pclass   <- factor(titanic$Pclass,
                           levels = sort(unique(titanic$Pclass)))

titanic$Sex      <- factor(titanic$Sex)

# Set missing values in Embarked to NA
titanic$Embarked[titanic$Embarked == ""] <- NA
titanic$Embarked <- factor(titanic$Embarked)

# Handle missing values / NA

# Replace age NA with mean age
age_mean <- mean(titanic$Age, na.rm = TRUE)
titanic$Age[is.na(titanic$Age)] <- age_mean

# Make all Age values whole numbers by rounding up
titanic$Age <- ceiling(titanic$Age)

# Replace embarked NA with mode
if (any(is.na(titanic$Embarked))) {
  embarked_mode <- names(sort(table(titanic$Embarked), decreasing = TRUE))[1]
  titanic$Embarked[is.na(titanic$Embarked)] <- embarked_mode
  titanic$Embarked <- droplevels(titanic$Embarked)
}

# Save data
titanic_clean <- titanic

write.csv(titanic_clean,
          file = "titanic_clean.csv",
          row.names = FALSE)

# Check data
summary(titanic_clean)

