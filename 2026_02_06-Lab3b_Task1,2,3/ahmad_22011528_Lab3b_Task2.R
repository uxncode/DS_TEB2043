students <- list(Name = c("Robert","Hemsworth","Scarlett","Evans","Pratt","Larson","Holland","Paul","Simu","Renner"),
            Score = c(59,71,83,68,65,57,62,92,92,59))

highest <- max(students$Score)
lowest <- min(students$Score)
average <- mean(students$Score)
student_highest <- students$Name[students$Score == highest]
student_lowest <- students$Name[students$Score == lowest]

cat("Highest Score:", highest, "\n")
cat("Lowest Score:", lowest, "\n")
cat("Average Score:", average, "\n")
cat("Student with Highest Score:", student_highest, "\n")
cat("Student with Lowest Score:", student_lowest, "\n")


# $ access element 
