# 1- Vectors
# Create the following vectors:

# (4,6,3,4,6,3,. . . .,4,6,3,4) where there are 11 occurrences of 4

pattern <- c(4, 6, 3)
vector <- rep(pattern, times = 10)
vector <- c(vector, 4)

# (4,4,. . . .,4,6,6,. . . .,6,3,3,. . . .,3) where there are 10 occurrences
# of 4, 20 occurrences of 6, and 30 occurrences of 3.

vector_2 <- c(rep(4, 10), rep(6, 20), rep(3, 30))

# 2- Predict the output of each when executed in R. Please do not use R for this at the beginning. Try to
# predict the output and then, execute the code in R to see if your prediction was correct. This exercise
# aims to check if you undertood how the vectors are working.

# a-
nums = c(5, 8, 3, 2, 7, 4) + c(1, 2, 3)
# 6, 10, 6, 3, 9, 7
nums[c(2:4)]
# 10, 6, 3

# b
nums = c(5, 8, 3, 2, 7, 4) - c(3, 0, 1)
# 2, 8, 2, -1, 7, 3
nums[-c(2:4)]
# 2, 7, 3

# c
nums = c(3, 1, 5, 4) + c(2)
# 5, 3, 7, 6
nums[c(1,3)]
# 5, 7

# d
nums = c(9,7,6,2,8)
nums[length(nums)]
# 8

# 3- Matrices
# The following code creates in R a matrix.

m <- rbind(c(11,12,13,14,15),
           c(21,22,23,24,25),
           c(31,32,33,34,35),
           c(41,42,43,44,45),
           c(51,52,53,54,55))

# Using the above, write R code to generate the following sub-matrices of m: a. the sub-matrix of elements in
# rows 3, 4, or 5 and in columns 1 or 2 b. the sub-matrix formed by deleting columns 2 and 4 c. the sub-matrix
# of rows whose first column element has an even ten’s digit

sub_mat1 <- m[3:5, 1:2]

sub_mat2 <- m[, -c(2, 4)]

sub_mat3 <- m[as.integer(substr(as.character(m[, 1]), 1, 1)) %% 2 == 0, ]


# 4- Lists
# 1. Use R to express the following list in order by component name

my.list <- list(b=3,a="bob",c=c(1,2,3),f=TRUE,e=c("x","y","z"),d=37)

names_sorted <- sort(names(my.list))
my.list[names_sorted]

# 2. Create a list with the factor named grades made from the vector c(“A”,“C”,“B”,“B”,“D”,“A”); the
# vector named nums identical to c(3,2.1,3.3,4,1.5,4.9). Then do the following:
my_list <- list(grades = factor(c("A", "C", "B", "B", "D", "A")), nums = c(3, 2.1, 3.3, 4, 1.5, 4.9))


# a. Write R code to display the elements of grades that correspond to elements of nums that are greater
# than or equal to 3.

my_list$grades[my_list$nums >= 3]

# b. Add a new member to the list, name it days. Add the weekdays to the newly created member from
# Monday through Saturday.

my_list$days <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")

# 5- Data Frames

# 1. Create a data frame named dframe in accordance with the following table. Note, person should be a
# vector of strings, while sex and funny should be factors.

person <- c('Stan', 'Francine', 'Stee', 'Roger', 'Abigail', 'Klaus')
sex <- factor(c('M', 'F', 'M', 'M', 'F', 'M'), levels = c("F", "M"))
funny <- factor(c('High', 'Med', 'Low', 'High', 'High', 'Med'), levels = c("Low", "Med", "High"))

dframe <- data.frame(person, sex, funny)

# Now do or find the following: 
# a. Add an age column where the ages of Stan, Francine, Steve, Roger, Hayley, and Klaus are 41, 41, 15, 1600, 21, and
# 60, respectively. (Yes, Roger is very old.)

age <- c(41, 41, 15, 1600, 21, 60)
dframe$age <- age

# b. Reorder the columns of the data frame so that they occur in the following order: person, age, sex,
# funny.

dframe <- dframe[, c("person", "age", "sex", "funny")]

# c. Find the mean age with including and excluding Roger.

print(mean(dframe$age))
age_wo_roger <- dframe[dframe$person != "Roger", ]$age
print(mean(age_wo_roger))

# d. Find the standard deviation of the age including and excluding Roger.

print(sd(dframe$age))
print(sd(age_wo_roger))

# e.  Select these people who are “high” on funny variable and store it in a new data.frame

high_funny <- dframe[dframe$funny == "High", ]

# Loops

# 1. Loop through a vector (lets say: vec <-seq(1,10, by=.5)) and print only the odd numbers.

vec <- seq(1, 10, by = 0.5)

for (i in vec) {
  if (i %% 1 == 0 && i %% 2 != 0) {
    print(i)
  }
}


# 2. Using the same vector, loop through it and print only the odd numbers that the remainder is zero
# when divided by 3.

for (i in vec) {
  if (i %% 2 != 0 && i %% 3 == 0) {
    print(i)
  }
}

# 3. A coin is flipped and one of the results “heads”, “tails”, “on edge” is stored in the variable coin. Write
# code that prints “It will be sunny” if the coin landed “heads”, prints “It will be cloudy” if the coin
# landed “tails”, and prints “There will be a blizzard” if the coin landed “on edge”. I want you toss the
# coin 30 times.


coin <- c('Heads', 'Tails', 'Edge') # the coin and possible outcomes
set.seed(1257)
coin_flips <- sample(coin, size = 30, replace = TRUE) 

for (i in coin_flips) {
  if (i == "Heads") {
    print("It will be sunny")
  } else if (i == "Tails") {
    print("It will be cloudy")
  } else {
    print("There will be a blizzard")
  }
}

# Functions

# 1. Write a function U(n) that calculates the following:

U <- function(n) {
  if (n%%3 == 0) {
    7*n + 3
  } else if (n%%3 == 1) {
    (7*n + 2)/3
  } else {
    (n - 2)/3
  }
}

# 2. Suppose a jar contains r red and b blue marbles, and n marbles are selected without replacement.
# Write an R function, marbles.probability(r,b,n,x), that returns the probability of getting exactly x red
# marbles, similar to the function shown below:

# marbles.probability(8,9,5,2)
# 0.3800905


marbles.probability <- function(r, b, n, x) {

  total <- choose(r + b, n)
  red <- choose(r, x)
  blue <- choose(b, n - x)
  (red * blue) / total
}


