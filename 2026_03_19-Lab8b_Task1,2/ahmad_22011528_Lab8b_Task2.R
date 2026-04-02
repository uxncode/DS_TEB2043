
data(BJsales)

# Quick facts for opening
cat("\n--- BJsales: quick summary ---\n")
str(BJsales)
cat("\n")
print(summary(BJsales))
cat("\n")

sales <- as.numeric(BJsales)
yr <- floor(time(BJsales))
annual_totals <- tapply(sales, yr, sum)

# Presentation script
presentation_script <- c(
  "",
  "OPENING",
  "Good morning. Today I am walking you through monthly sales using the classic BJsales dataset in R.",
  "I will show three views: the trend over time, how typical monthly sales are spread out, and how each full year compares to the next.",
  "",
  "TRANSITION — BEFORE THE LINE CHART",
  "First, here is the headline chart: monthly sales from the start of the series to the end.",
  "This is what you would use to answer: are we growing, holding steady, or seeing a pullback?",
  ""
)
writeLines(presentation_script)

# 1) Line graph — performance over time
plot(
  BJsales,
  type = "o",
  pch = 16,
  col = "steelblue",
  xlab = "Time (years)",
  ylab = "Sales (index units)",
  main = "Monthly Sales Trend (BJsales)"
)
grid()

writeLines(c(
  "OBSERVATION — AFTER THE LINE CHART",
  "What you see here is the month-to-month path of sales. When the line slopes up over long stretches, that tells us underlying demand or revenue is strengthening.",
  "When it flattens or dips, that is where we would dig into seasonality, competition, or operations — but the big picture for management is: direction and stability.",
  ""
))

# 2) Histogram — distribution of monthly sales levels
hist(
  sales,
  breaks = 15,
  col = "lightgoldenrod2",
  border = "gray30",
  xlab = "Monthly sales",
  ylab = "Frequency",
  main = "Distribution of Monthly Sales"
)
abline(v = mean(sales), col = "red3", lwd = 2)
legend("topright", legend = "Mean monthly sales", lwd = 2, col = "red3", bty = "n")

writeLines(c(
  "TRANSITION — BEFORE THE HISTOGRAM",
  "Next, this histogram answers a different question: not when sales happened, but how often we see high versus low monthly readings.",
  "",
  "OBSERVATION — AFTER THE HISTOGRAM",
  "Most months fall in the middle band — that is our normal operating range. The red line is the average month.",
  "If the bars were very wide or skewed, I would flag that we are living with more volatility than usual; if they are tight, I would say performance is predictable, which helps planning and targets.",
  ""
))

# 3) Bar chart — yearly totals
barplot(
  annual_totals,
  col = "darkseagreen3",
  border = "white",
  xlab = "Year",
  ylab = "Total sales (sum of months)",
  main = "Annual Total Sales by Year",
  las = 2,
  cex.names = 0.85
)

writeLines(c(
  "TRANSITION — BEFORE THE BAR CHART",
  "Finally, for budgeting and year-end reviews, managers usually care about full-year totals, not just individual months.",
  "",
  "OBSERVATION — AFTER THE BAR CHART",
  "Each bar is one calendar year’s total sales. Taller bars are stronger years; shorter bars are weaker years.",
  "This is the slide I would use to say which years beat the plan, which years recovered, and where we might set the next annual target.",
  "",
  "CLOSING",
  "In short: the line chart gives the story of momentum, the histogram gives the story of typical monthly performance, and the bar chart gives the story of year-over-year strength.",
  "Thank you — happy to take questions.",
  ""
))

