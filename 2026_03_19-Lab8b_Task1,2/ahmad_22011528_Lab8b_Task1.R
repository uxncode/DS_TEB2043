
install.packages("readxl")
library(readxl)

csv_path <- "titanic_clean.csv"
xlsx_path <- "titanic_report_summary.xlsx"

cat("Working directory:", getwd(), "\n")
cat("Loading dataset:", csv_path, "\n\n")

df <- read.csv(csv_path, stringsAsFactors = FALSE)

df$Survived <- factor(df$Survived, levels = c("No", "Yes"))
df$Sex <- factor(df$Sex)
df$Pclass <- factor(df$Pclass, levels = c("1", "2", "3"), labels = c("1st", "2nd", "3rd"))
df$Embarked <- factor(df$Embarked)

cat("Rows:", nrow(df), " | Cols:", ncol(df), "\n")
cat("Missing values per column:\n")
print(colSums(is.na(df)))
cat("\n")

cat("Survival rate (overall):\n")
print(round(prop.table(table(df$Survived)) * 100, 1))
cat("\n")

cat("Reading Excel summary:", xlsx_path, "\n")
sheets <- excel_sheets(xlsx_path)
cat("Excel sheets:", paste(sheets, collapse = ", "), "\n\n")
summary_tbl <- read_excel(xlsx_path, sheet = sheets[1])
cat("Preview of first Excel sheet:\n")
print(utils::head(summary_tbl, 10))
cat("\n")

# Visualization

# Bar chart - Survival count by Sex
surv_by_sex <- table(df$Sex, df$Survived)
barplot(
  surv_by_sex,
  beside = TRUE,
  col = c("tomato3", "steelblue3"),
  border = "white",
  main = "Titanic Survival by Sex",
  xlab = "Sex",
  ylab = "Number of Passengers",
  legend.text = colnames(surv_by_sex),
  args.legend = list(x = "topright", bty = "n")
)

cat("Observation: Females show a higher survival count/rate than males (consistent with 'women first').\n\n")

# Box plot - Age distribution by Survival
boxplot(
  Age ~ Survived,
  data = df,
  col = c("gray80", "palegreen3"),
  main = "Age Distribution by Survival",
  xlab = "Survived",
  ylab = "Age (years)",
  notch = TRUE
)

cat("Observation: Age distributions differ; compare medians/IQRs to see if survivors skew younger.\n\n")

# Histogram - Age distribution (overall)
hist(
  df$Age,
  breaks = 20,
  col = "khaki1",
  border = "gray30",
  main = "Age Distribution (All Passengers)",
  xlab = "Age (years)"
)
abline(v = mean(df$Age, na.rm = TRUE), col = "red3", lwd = 3)
legend("topright", legend = c("Mean age"), lwd = 3, col = "red3", bty = "n")

cat("Observation: Ages are concentrated in young adult range; mean is marked for reference.\n\n")

# Pie chart - Embarked distribution
emb_counts <- table(df$Embarked)
pie(
  emb_counts,
  labels = paste0(names(emb_counts), " (", emb_counts, ")"),
  main = "Passenger Count by Embarkation Port",
  col = rainbow(length(emb_counts))
)

cat("Observation: Most passengers embarked from the dominant port shown by the largest slice.\n\n")

# Insights

cat("Survival rate by Sex (%):\n")
surv_rate_by_sex <- round(prop.table(table(df$Sex, df$Survived), 1)[, "Yes"] * 100, 1)
print(surv_rate_by_sex)
cat("\n")

cat("Survival rate by Class (%):\n")
surv_rate_by_class <- round(prop.table(table(df$Pclass, df$Survived), 1)[, "Yes"] * 100, 1)
print(surv_rate_by_class)
cat("\n")

