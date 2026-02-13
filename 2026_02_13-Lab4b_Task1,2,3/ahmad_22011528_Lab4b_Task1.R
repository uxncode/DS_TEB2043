age = c(55,57,56,52,51,59,58,53,59,55,60,60,60,60,52,55,56,51,60,52,54,56,52,57,54,56,58,53,53,50,55,51,57,60,57,55,51,50,57,58)

age_factor <- factor(age)
levels(age_factor)
table(age)

age_group <- cut(age, breaks=c(50,52,54,56,58,60),include.lowest=TRUE, right=FALSE)
table(age_group)

# Retirement age range from 50-60.
# 60 is the highest amount in terms of retirement ge.
# In range : Mod staff is in range (58-60), min is in range (50-52)
