students <- list(
  Name = c("Robert","Hemsworth","Scarlett","Evans","Pratt","Larson","Holland","Paul","Simu","Renner"),
  Chemistry = c(59, 71, 86, 65, 52, 60, 67, 40, 77, 90),
  Physics = c(59, 89, 83, 68, 65, 57, 62, 92, 92, 59)
)

# count fail
fail_chem <- sum(students$Chemistry <= 49)
fail_phys <- sum(students$Physics <= 49)

cat("Number of students failing Chemistry:", fail_chem, "\n")
cat("Number of students failing Physics:", fail_phys, "\n")

# check highest for Chemistry
highest_chem <- max(students$Chemistry)
student_highest_chem <- students$Name[students$Chemistry == highest_chem]

# check highest for Physics
highest_phys <- max(students$Physics)
student_highest_phys <- students$Name[students$Physics == highest_phys]

cat("Highest Chemistry Score:", highest_chem, "by",
    paste(student_highest_chem, collapse = " and "), "\n")
cat("Highest Physics Score:", highest_phys, "by",
    paste(student_highest_phys, collapse = " and "), "\n")