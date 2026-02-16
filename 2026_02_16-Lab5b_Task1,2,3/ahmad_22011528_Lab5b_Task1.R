# Check whether a year is leap year or not

year <- as.numeric(readline(prompt = "Input year: "))

if((year %% 4 == 0 && year %% 100 != 0) || (year %% 400 == 0)) {
  cat(year, "is a leap year.")
} else {
  cat(year, "is a not leap year.")
}

