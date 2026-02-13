scores <- c(33, 24, 54, 94, 16, 89, 60, 6, 77, 61, 13, 44, 26, 24, 73, 73, 90, 39, 90, 54)

grade_A <- sum(scores >= 90 & scores <= 100)
grade_B <- sum(scores >= 80 & scores <= 89)
grade_C <- sum(scores >= 70 & scores <= 79)
grade_D <- sum(scores >= 60 & scores <= 69)
grade_E <- sum(scores >= 50 & scores <= 59)
grade_F <- sum(scores <= 49)

print(paste("Grade A (90-100):", grade_A))
print(paste("Grade B (80-89):", grade_B))
print(paste("Grade C (70-79):", grade_C))
print(paste("Grade D (60-69):", grade_D))
print(paste("Grade E (50-59):", grade_E))
print(paste("Grade F (<=49):", grade_F))

pass_status <- scores > 49
print("Pass status (TRUE = pass, FALSE = fail):")
print(pass_status)
