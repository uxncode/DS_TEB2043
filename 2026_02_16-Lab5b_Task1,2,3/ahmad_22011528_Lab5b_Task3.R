# Check whether a digit is an Armstrong number

num <- as.numeric(readline(prompt = "Input an integer: "))
original_num <- num

# Count number of digits
num_digits <- nchar(as.character(num))

sum <- 0

# Count sum for each digits
while (num > 0) {
  digit <- num %% 10
  sum <- sum + (digit^num_digits)
  num <- num %/% 10
}

# Check if sum is same as original number
if (sum == original_num) {
  cat(original_num, "is an Armstrong number.")
} else {
  cat(original_num, "is not an Armstrong number.")
}

