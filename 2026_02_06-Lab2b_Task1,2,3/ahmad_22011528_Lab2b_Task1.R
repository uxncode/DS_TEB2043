weight <- as.numeric(readline("Enter your weight in kg: "))
height <- as.numeric(readline("Enter your height in meters: "))

bmi <- weight / (height^2)

print(paste("Your BMI is:", round(bmi, 1)))

if (bmi <= 18.4) {
  print("BMI Status: Underweight")
} else if (bmi >= 18.5 && bmi <= 24.9) {
  print("BMI Status: Normal")
} else if (bmi >= 25.0 && bmi <= 39.9) {
  print("BMI Status: Overweight")
} else {
  print("BMI Status: Obese")
}
