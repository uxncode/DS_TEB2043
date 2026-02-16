# Display cube of numbers up to a given integer

num <- as.numeric(readline(prompt = "Input an integer: "))

for (i in 1:num) {
  cube <- i^3
  cat("Number is:", i, "and cube of the", i, "is :", cube, "\n")
}
