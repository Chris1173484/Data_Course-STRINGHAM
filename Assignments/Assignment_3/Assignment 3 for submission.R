# 1.  Get a subset of the "iris" data frame where it's just even-numbered rows
library(tidyverse)
seq(2,150,2) # here's the code to get a list of the even numbers between 2 and 150
even_iris <- seq(2,150,2)
even_iris2 <- iris[even_iris,]
print(even_iris2)

# 2.  Create a new object called iris_chr which is a copy of iris, except where every column is a character class
iris_chr <- data.frame(iris)

for (col  in names(iris_chr)){
  iris_chr[,col] <- as.character(iris_chr[,col])
}
str(iris_chr)

# 3.  Create a new iris# 3.  Create a new numeric vector object named "Sepal.Area" which is the product of Sepal.Length and Sepal.Width
iris$sepal.area <- iris$Sepal.Length*iris$Sepal.Width


# 4.  Add Sepal.Area to the iris data frame as a new column
iris %>% 
  mutate(sepal.area=Sepal.Length*Sepal.Width)

# 5.  Create a new data frame that is a subset of iris using only rows where Sepal.Area is greater than 20 
# (name it big_area_iris)
big_area_iris <- iris[iris$sepal.area>20,]
