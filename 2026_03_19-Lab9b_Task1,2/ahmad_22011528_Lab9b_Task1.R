
install.packages(c("corrplot", "RColorBrewer", "ggplot2", "reshape2"))

library(corrplot)
library(RColorBrewer)
library(ggplot2)
library(reshape2)

# Load Dataset 
data(ToothGrowth)

head(ToothGrowth)
str(ToothGrowth)

# Encode supp as 1/2 for a single matrix for Pearson correlation
tg_num <- data.frame(
  len = ToothGrowth$len,
  dose = ToothGrowth$dose,
  supp_code = as.numeric(ToothGrowth$supp)
)
colnames(tg_num) <- c("len", "dose", "supp_numeric")

# Correlation analysis 
corr_mat <- round(cor(tg_num, method = "pearson"), 4)
print("Pearson correlation matrix:")
print(corr_mat)

# Formal test for the main relationship: length vs dose
test_len_dose <- cor.test(ToothGrowth$len, ToothGrowth$dose, method = "pearson")
print(test_len_dose)

# Reorder matrix (lab 2.1 style): distance from (1 - r) / 2, then hclust
dist_r <- as.dist((1 - corr_mat) / 2)
hc <- hclust(dist_r)
corr_mat_ordered <- corr_mat[hc$order, hc$order]

melted <- melt(as.matrix(corr_mat_ordered))
colnames(melted) <- c("Var1", "Var2", "value")

# Heatmap 1: corrplot 
corrplot(
  corr_mat,
  method = "color",
  type = "full",
  order = "hclust",
  tl.col = "black",
  col = colorRampPalette(brewer.pal(n = 8, name = "RdYlBu"))(200),
  addCoef.col = "black",
  number.cex = 0.7,
  title = "ToothGrowth: Pearson correlation (full matrix, hclust)",
  mar = c(0, 0, 2, 0)
)

# Heatmap 2: ggplot2 tile (reordered matrix) 
p <- ggplot(melted, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  geom_text(aes(label = value), color = "black", size = 4) +
  scale_fill_gradient2(
    low = "#2166AC", mid = "white", high = "#B2182B",
    midpoint = 0, limits = c(-1, 1), name = "r"
  ) +
  labs(
    title = "ToothGrowth: full correlation heatmap (hclust order)",
    x = NULL, y = NULL
  ) +
  coord_fixed() +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
print(p)

# Heatmap 3: ggplot2 full matrix
vars_natural <- colnames(corr_mat)
melted_full <- melt(as.matrix(corr_mat))
colnames(melted_full) <- c("Var1", "Var2", "value")
melted_full$Var1 <- factor(melted_full$Var1, levels = vars_natural)
melted_full$Var2 <- factor(melted_full$Var2, levels = vars_natural)

p_full <- ggplot(melted_full, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  geom_text(aes(label = value), color = "black", size = 4) +
  scale_fill_gradient2(
    low = "#2166AC", mid = "white", high = "#B2182B",
    midpoint = 0, limits = c(-1, 1), name = "r"
  ) +
  labs(
    title = "ToothGrowth: full correlation heatmap (original var order)",
    x = NULL, y = NULL
  ) +
  coord_fixed() +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
print(p_full)

# Observations / insights 
cat(
  "\n========== Discussion (ToothGrowth correlation) ==========\n",
  "1. len vs dose (main result)\n",
  "   Main relationship in the analysis. When dose went up, tooth length (len)\n",
  "   went up too, so the association was positive. Pearson r\n",
  "   measured how strong that linear pattern was: values near +1 mean a strong\n",
  "   positive link, values near 0 mean little or no linear link.\n",
  "   cor.test() is used for the p-value. A small p-value (below 0.05) suggested the\n",
  "   positive association was unlikely to be just random noise. The conclusion was\n",
  "   that higher dose was associated with longer teeth in this dataset.\n\n",
  "2. supp_numeric \n",
  "   The codes are only labels: they do not mean OJ is half of VC or that the gap between 1 and 2 is a real quantity.\n",
  "   Pearson correlation assumes numeric spacing matters, so it is not appropriate to judge supplement effects.\n",
  "   Better to use boxplots by supp, a t-test, or ANOVA instead.\n\n",
  "3. dose vs supp_numeric\n",
  "   The correlation between dose and the numeric supplement code was close to zero.\n",
  "   Each dose level (0.5, 1, 2) included both OJ and VC, so dose and supplement type were balanced and not tied together in the table.\n",
  "\n===========================================================\n",
  sep = ""
)
