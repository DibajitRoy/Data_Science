
y <- 20
if (y > 10) {
  print("y is greater than 10")
}
y <- 10
if (y > 9) {
  print("y is greater than 9")
} else {
  print("y is 9 or less")
}
result <- 80
if (result >= 92) {
  print("Grade A")
} else if (result >= 80) {
  print("Grade B")
} else if (result >= 70) {
  print("Grade C")
} else {
  print("Grade F")
}
for (i in 1:3) {
  print(paste("Iteration", i))
}
#repeat Loop (with break)
i <- 1
repeat {
  print(i)
  i <- i + 1
  if (i > 5) break
}
#repeat Loop (with break)
i <- 2
repeat {
  print(i)
  i <- i + 2
  if (i > 10) break
}
#next Statement (skip to next iteration)
for (i in 1:10) {
  if (i == 3) next
  print(i)
}
  
 #break Statement (exit the loop)
  for (i in 1:8) {
    if (i == 7) break
    print(i)
  }
#mean()
numbers <- c(5, 2, 3, 4, 5)
mean(numbers)
sum(numbers) 
length(numbers)
#round()
pi_val <- 3.14159
round(pi_val, 3)
paste("Hello", "Dibbo")

#Simple function to add two numbers
add_numbers <- function(a, b) {
  return(a + b)
}

add_numbers(2, 4) 

#Function to check if a number is even
is_even <- function(x) {
  if (x %% 2 == 0) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

is_even(3) 
#Function with default parameter
greet <- function(name = "Dibbo") {
  paste("Hello", name)
}

greet() 
greet("Agnib") 
#Anonymous (Lambda) Function with sapply()
numbers <- 1:4
squared <- sapply(numbers, function(x) x^2)
print(squared) 


#Reading a CSV File
data <- read.csv("C:\\Users\\student\\Downloads\\sample_dataset.csv")
head(data)

#Reading a Text File (tab-delimited)
data <- read.table("C:\\Users\\student\\Desktop\\dataset.txt", header = TRUE, sep = "\t")
head(data)
#Reading Data from a URL
url <- "https://raw.githubusercontent.com/uiuc-cse/data-fa14/gh-pages/data/iris.csv"
data <- read.csv(url)
head(data)

#Exercise 1:
score <- 80
if (score >= 90) {
  print("Excellent")
} else if (score >= 75) {
  print("Good")
} else if (score >= 50) {
  print("Pass")
} else {
  print("Fail")
}

#Exercise 2:
num <- 1:10
for(N in num){
  print( N^2)
}
#Exercise 3:
num <- 1
while(num<20){
  if (num %% 2 == 0) {
    print(num)
  }
  num<- num+1
}
#Exercise 4:
multi <- function(x,y){
  return(x*y)
}
multi(5,5)
#Exercise 5:
calculate_stats  <- function(x){
  result <- list(
  mean <- mean(x),
  median  <- median(x),
  sd <- sd(x)
  )
  return(result)
}
t <- c(10,25,80,25,10) 
print(calculate_stats (t))
#Exercise 6: 
grade  <- function(score){
if (score >= 90) {
  print("A")
} else if (score >= 75) {
  print("B")
} else if (score >= 50) {
  print("C")
} else {
  print("Fail")
}
}
grade (50)
grade (90)
grade (80)
#Exercise 7: 
data <- read.csv("C:\\Users\\student\\Desktop\\datafile.csv")
head(data)
str(data)
  
  
