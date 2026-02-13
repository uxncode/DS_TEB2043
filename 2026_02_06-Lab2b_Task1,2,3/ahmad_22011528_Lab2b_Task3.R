name <- readline("Enter your name: ")
phone <- readline("Enter your phone number: ")

name_upper <- toupper(name)

first3 <- substr(phone, 1, 3)
last4 <- substr(phone, nchar(phone)-3, nchar(phone))

masked_phone <- paste0(first3, "-xxxxx ", last4)

print(paste("Hi,", name_upper, ". A verification code has been sent to", masked_phone))
