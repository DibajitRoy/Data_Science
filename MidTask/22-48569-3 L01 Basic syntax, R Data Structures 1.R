# Creating Different Types of Vectors
# Numeric Vector
N_vec <- c(3, 4, 8, 9, 5)
print(N_vec)

# Character Vector
fruits_vec <- c("pinaple", "mango", "banana" , "coconut")
print(fruits_vec)

# Logical Vector
vector <- c(TRUE, TRUE, FALSE , FALSE)
print(vector)

# Vector Operations
# Arithmetic Operations
vector1 <- c(2, 2, 2)
vector2 <- c(5, 5, 5)

sum <- vector1 + vector2  # Element-wise addition
mul <- vector1 * vector2 # Element-wise multiplication

print(sum) 
print(mul)

# Accessing Elements in a Vector
# Create a vector
number <- c(15, 25, 3, 90, 80,7)

# Access elements using index (1-based index)
print(number[6])
print(number[c(1, 4, 2)])
print(number[number > 25])
number[3] <- 9
print(number) 
number <- c(number, 60, 70)
print(number)

# Vector Functions
v <- c(5, 5, 10, 10)

# Length of the vector
print(length(v)) 
print(sum(v))
print(mean(v)) 
sorted_v <- sort(v, decreasing = TRUE)
print(sorted_v)

# Sequence and Repetition in Vectors
# Sequence from 1 to 10
sequence <- seq(1, 20, by = 4)  # Steps of 2
print(sequence)
rep_vec <- rep(c(3, 5, 9), times = 4)  # Repeat entire vector
print(rep_vec)

# Creating a 3x3 matrix (filled column-wise by default)
matri <- matrix(1:12, nrow = 4, ncol = 4)
print(matri)
matri <- matrix(1:12, nrow = 4, byrow = TRUE)
print(matri)
# Naming Rows and Columns
# Creating a matrix
matri <- matrix(1:16, nrow = 4)

# Assigning row and column names
rownames(matri) <- c("Row1", "Row2", "Row3" ,"Row4")
colnames(matri) <- c("Col1", "Col2", "Col3" ,"Col4")

print(matri)
# Accessing Elements in a Matrix
# Create a 3x3 matrix
matri <- matrix(1:9, nrow = 3)

# Access element at row 2, column 3
print(matri[1, 2]) 
print(matri[2, ]) 
print(matri[, 3]) 

# Matrix Arithmetic
m1 <- matrix(1:3, nrow = 2)
m2 <- matrix(1:3, nrow = 2)

# Matrix addition
sum_m <- m1 + m2
print(sum_m)
# Matrix multiplication (element-wise)
prod_mat <- m1 * m2
print(prod_mat)
dot_prod_mat <- m1 %*% m2  # %*% for matrix multiplication
print(dot_prod_mat)
t_m <- t(m1)
print(t_m)

# Creating an array with dimensions (3x3x2)
arr <- array(1:12, dim = c(4, 2, 2))
print(arr)
# Accessing Elements in an Array
# Create a 3x3x2 array
arr <- array(1:15, dim = c(2, 2, 4))

# Access element at [2nd row, 3rd column, 1st layer]
print(arr[1, 2, 2])
print(arr[1, , 1])
print(arr[, 2, 2])

#Performing Operations on Arrays
# Creating two 3x3x2 arrays
a1 <- array(1:12, dim = c(2, 2, 3))
a2 <- array(20:40, dim = c(2, 2, 3))

# Element-wise addition
sum_arr <- a1 + a2
print(sum_arr)
prod_arr <- a1 * a2
print(prod_arr)
# Applying Functions to Arrays
# Creating an array
arr <- array(1:20, dim = c(4, 4, 2))

# Sum of all elements in the array
print(sum(arr))
print(mean(arr))
apply(arr, MARGIN = 3, FUN = sum)
# Apply function to each column (margin = 2)
apply(arr, MARGIN = 3, FUN = mean)

# Creating a simple data frame
df <- data.frame(
  ID = c(501, 502, 503, 504),
  Name = c("A", "B", "C", "D"),
  Age = c(20, 26, 25, 23),
  Score = c(90, 60, 96.2, 73),
  Passed = c(TRUE, FALSE, TRUE, TRUE)
)

# Print the data frame
print(df)
print(df$Name)
print(df[4, ])  
print(df[, c("Name", "Age")])  
print(df[1:3, ])
df$Grade <- c("A+", "D", "A+", "c+")
print(df)
passed_students <- df[df$Passed == FALSE, ]
print(passed_students)
high_scorers <- df[df$Score > 85, ]
print(high_scorers)
# Sorting by Age (Ascending)
df_sorted <- df[order(df$Score), ]
print(df_sorted)
# Sorting by Score (Descending)
df_sorted_desc <- df[order(-df$Score), ]
print(df_sorted_desc)

# Changing a value (Changing Bob’s Score to 80)
df$Score[df$Name == "A"] <- 80
print(df)
# Renaming column names
colnames(df) <- c("Stu_ID", "FULL_Name", "Student_Age", "MidExam_Score", "Passed_Exam", "Final_Grade")
print(df)
# Remove a column
df$Grade <- NULL
print(df)
df <- df[-1, ]
print(df)
# Get summary statistics
summary(df)
# Get structure of the data frame
str(df)

# Creating a list with different data types
my_list <- list(
  Name = "Dibbo",
  Age = 22,
  Scores = c(91, 75, 88),
  Passed = TRUE
)

# Print the list
print(my_list)
# Access by index
print(my_list[[1]]) 
# Access by name
print(my_list$Scores)
# Access specific elements within a list item
print(my_list$Scores[3]) 
# Change an element
my_list$Age <- 23
print(my_list$Age)
# Add a new element
my_list$Country <- "Canada"
print(my_list)
# Remove an element
my_list$Passed <- NULL
print(my_list)

# Creating a list with a matrix and a data frame
Number_list <- list(
  Numbers = c(9, 8, 5, 1),
  Matrix = matrix(1:12, nrow = 4),
  DataFrame = data.frame(ID = c(101, 102), Name = c("Dip", "Coco"))
)

# Print the list
print(Number_list)

# Access elements inside the matrix
print(Number_list$Matrix[1, 2])
L1 <- list(A = 1:8, B = "Hello")
L2 <- list(C = c(TRUE, FALSE), D = matrix(1:9, nrow = 3))

# Merge lists
merged_list <- c(L1, L2)
print(merged_list)

# Convert list to data frame
list_to_df <- data.frame(
  Name = c("FOP", "Cob"),
  Age = c(22, 20),
  Score = c(96, 82)
)

print(list_to_df)

#Exercise1(Vector)
scores <- c(72, 95, 81, 67, 89, 54, 100, 78)
print(scores)
print(min(scores))
print(max(scores))
print(mean(scores))
print(median(scores))

#Exercise2(list)

student_info <- list(
  name = "Dibbo",
  age = 22,
  grades = c(86, 90, 88)
)
print(student_info)
print(student_info$age)
print(student_info$grades[2])

#Exercise3(Matrix)
mat <- matrix(1:16, nrow = 4)
print(mat)
print(mat[3, ])
print(mat[, 2])
print(sum(diag(mat)))

#Exercise4(Data Frame)
stu <- data.frame(
  Name = c("alice", "Bob", "Charlie", "David"),
  Math = c(85, 78, 92, 88),
  English = c(90, 82, 95, 87)
)
print(stu)
str(stu)
print(stu$English)
stu$Average <- (stu$Math + stu$English) / 2
print(stu)

#Exercise5
blood_group <- factor(c("A", "B", "O", "AB", "A", "O"))
print(blood_group)
print(levels(blood_group))
print(table(blood_group))

#Exercise6
Detail_Info <- data.frame(
  Name = c("Dibbo", "pob", "jon", "rafid", "Eva"),
  Age = c(23, 22, 24, 23, 21),
  Gender = factor(c("Male", "Female", "Male", "Male", "Female")),
  Math = c(85, 78, 90, 88, 95),
  English = c(92, 80, 87, 91, 96),
  Science = c(88, 82, 85, 89, 94)
)
print(Detail_Info)
print(Detail_Info$Name[Detail_Info$Average > 80])
print(table(Detail_Info$Gender))