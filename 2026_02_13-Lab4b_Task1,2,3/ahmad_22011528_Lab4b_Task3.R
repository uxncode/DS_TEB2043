Array1 <- array(1:24, dim = c(2,4,3))
cat("Array1")
print(Array1)

Array2 <- array(25:54, dim = c(3,2,5))
cat("Array2")
print(Array2)

# print second row of second matrix of Array1
cat("The second row of the second matrix of the array:")
print(Array1[2, , 2])

# print element in 3rd row, 2nd column of first matrix of Array2
cat("The element in the 3rd row and 2nd column of the 1st matrix:")
print(Array2[3, 2, 1])
