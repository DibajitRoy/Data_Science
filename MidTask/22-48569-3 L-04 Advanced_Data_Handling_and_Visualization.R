data("mtcars")
head(mtcars)

ggplot(mtcars, aes(x = cyl, y = drat)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "CYL vs DRAT", x = "Horsepower", y = "Miles per Gallon")

ggplot(mtcars, aes(y = cyl)) +
  geom_boxplot(fill = "yellow") +
  labs(title = "Boxplot of Miles per Gallon", y = "Cyl")

library(GGally)
ggcorr(mtcars, label = TRUE)


colSums(is.na(mtcars))


mtcars_clean <- na.omit(mtcars)
cat("Total NA values after cleaning:", sum(is.na(mtcars_clean)), "\n")

mtcars_filtered <- mtcars_clean %>% filter(mpg > 20)
head(mtcars_filtered)

mtcars_selected <- mtcars_filtered %>% select(mpg, hp, wt)
mtcars_mutated <- mtcars_selected %>%
  mutate(power_to_weight = hp / wt)
head(mtcars_selected)

mtcars_scaled <- mtcars_selected %>%
  mutate(across(c(mpg, hp, wt), ~ scale(.)[,1]))
head(mtcars_scaled)

dibbo <- read.csv("C://Users//User//Downloads//dataset.csv")
head(dibbo)

library(GGally)
ggcorr(dibbo, label = TRUE)

ggplot(dibbo, aes(x = NS1, y = IgG)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "green") +
  labs(title = "AGE vs OUTCOME", x = "Horsepower", y = "Miles per Gallon")


ggplot(dibbo, aes(y = Age)) +
  geom_boxplot(fill = "purple") +
  labs(title = "Boxplot of Miles per Gallon", y = "Age")

colSums(is.na(dibbo))

dibbo_clean <- na.omit(dibbo)

dibbo_clean <- na.omit(dibbo)
cat("Total NA values after cleaning:", sum(is.na(dibbo_clean)), "\n")

dibbo_filtered <- dibbo_clean %>% filter(Age > 30)
head(dibbo_filtered)       


dibbo_selected <- dibbo_filtered %>% select(Age, NS1, IgG, IgM, Outcome)
dibbo_mutated <- dibbo_selected %>%
mutate(Seropositivity = NS1 + IgG + IgM)
head(dibbo_mutated)

