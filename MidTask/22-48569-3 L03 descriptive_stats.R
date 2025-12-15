head(iris)
str(iris)
summary(iris)
mean(iris$Petal.Width)
median(iris$Petal.Width)

freq_table <- table(iris$Petal.Width)
names(freq_table)[which.max(freq_table)]

range_val <- range(iris$Petal.Width)
max(range_val) - min(range_val)

var(iris$Petal.Width)

sd(iris$Petal.Width)

IQR(iris$Petal.Width)


quantile(iris$Petal.Width, probs = c(0.20, 0.85))
library(dplyr)

# Calculate mean, sd, and count for each species
iris %>%
  group_by(Species) %>%
  summarise(
    count = n(),
    mean_sepal_length = mean(Sepal.Length),
    sd_sepal_length = sd(Sepal.Length),
    mean_petal_length = mean(Petal.Length),
    sd_petal_length = sd(Petal.Length)
  )
pairs(iris[, 1:4], main = "Scatterplot Matrix of Iris Data", col = iris$Species)

#Exercise
library(readr)
url <- "https://drive.google.com/uc?id=1iHcAvkN5z7TbBhgB2hUA1ecV9bSRNfnn"
dataset <- read_csv(url)

head(dataset)

summary(dataset)

str(dataset)
dataset_clean <- dataset[!is.na(dataset$total_laid_off), ]
mean(dataset_clean$total_laid_off)
median(dataset_clean$total_laid_off)

freq_table <- table(dataset$total_laid_off)
names(freq_table)[which.max(freq_table)]

# Variance
var(dataset$total_laid_off, na.rm = TRUE)

# Standard Deviation
sd(dataset$total_laid_off, na.rm = TRUE)

# Interquartile Range (IQR)
IQR(dataset$total_laid_off, na.rm = TRUE)
quantile(dataset$total_laid_off, probs = c(0.20, 0.85), na.rm = TRUE)

# Load the necessary library
library(dplyr)

# Assuming your dataset has a 'species' column (adjust if needed)
dataset %>%
  group_by(species) %>%
  summarise(
    count = n(),
    mean_total_laid_off = mean(total_laid_off, na.rm = TRUE),
    sd_total_laid_off = sd(total_laid_off, na.rm = TRUE)
  )

