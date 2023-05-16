library(soscisurvey)
library(plotly)
library(ggplot2)
library(psych)
library(dplyr)

# This is an API and using this, one can address where the data is located.
path <- "https://www.soscisurvey.de/RCOURSESS23/?act=aicmMlFDWe9TWBfEwRscP8mG"
# this reads the data from the path we have specified above.
data <- read_sosci(path)
data <- as.data.frame(data)
data <- data[,-c(1:4, (ncol(data) - 16):ncol(data))]

old_con <- c("CS01", "CS02", "CS03", "AM01")
new_con <- c("AffPreference", "CogPreference", "PerAbilities", "AcademicMotivation")

for (i in 1:4){
  selected_cols <- startsWith(names(data), old_con[i])
  names(data)[selected_cols] <- paste(new_con[i], 1:sum(selected_cols), sep = '_')
}

colnames(data)[grep("SC01", colnames(data))] <- "InformedConsent"

affective <- select(data, starts_with('AffPreference'))
cognitive <- select(data, starts_with('CogPreference'))
abilities <- select(data, starts_with('PerAbilities'))
motivation <- select(data, starts_with('AcademicMotivation'))

describe(affective)
describe(cognitive)
# When we check cognitive preferences CogPreference_5, CogPreference_12 and CogPreference_13
# have relatively low mean values. They all corresponds to reverse coded items

data <- subset(data, select = -c(CogPreference_5, CogPreference_12, CogPreference_13))

describe(abilities)
# PerAbilities_3, PerAbilities_6, PerAbilities_7 are relatively low

data <- subset(data, select = -c(PerAbilities_3, PerAbilities_6, PerAbilities_7))

describe(motivation)

sapply(data, function(x) sum(is.na(x)))

data <- data[!is.na(data$CogPreference_1),]

affective <- select(data, starts_with('AffPreference'))
cognitive <- select(data, starts_with('CogPreference'))
abilities <- select(data, starts_with('PerAbilities'))
motivation <- select(data, starts_with('AcademicMotivation'))

data$AffPreference_Mean <- rowMeans(affective, na.rm = T)
data$CogPreference_Mean <- rowMeans(cognitive, na.rm = T)
data$PerAbilities_Mean <- rowMeans(abilities, na.rm = T)
data$AcademicMotivation_Mean <- rowMeans(motivation, na.rm = T)

df.summarise <- data %>% group_by(Gender) %>%
  summarise(Mean_AffPre = mean(AffPreference_Mean ),
            Mean_AcMot = mean(AcademicMotivation_Mean ),
            STD_AffPre = sd(AffPreference_Mean),
            STD_AcMot = sd(AcademicMotivation_Mean),
            Median_AffPre = median(AffPreference_Mean),
            Median_AcMot = median(AcademicMotivation_Mean),
            Max_AffPre = max(AffPreference_Mean),
            Max_AcMot = max(AcademicMotivation_Mean),
            Min_AffPre = min(AffPreference_Mean),
            Min_AcMot = min(AcademicMotivation_Mean))

sink('Descriptives.txt')
print('Descriptive statistics comparison of AffPreference and AcademicMotivation')
df.summarise
sink()

library(plotly)

# Assuming your data frame is named "df" and contains the necessary columns

data$Gender = as.factor(data$Gender)
# Create the boxplot with plotly
plot <- plot_ly(data = data, x = ~Gender, y = ~AcademicMotivation_Mean, type = "box",
                color = ~Gender, colors = c("blue", "pink"))

# Add axis labels and plot title
plot <- plot %>% layout(yaxis = list(title = "AcademicMotivation_Mean"),
                        xaxis = list(title = "Gender"),
                        title = "Boxplot of Academic Motivation by Gender")

# Display the plot
plot


# check the reliability of our data dimensions
reliability_result <- psych::alpha(motivation)

print(reliability_result$total$alpha)  # Total alpha coefficient
print(reliability_result$alpha)        # Alpha coefficient for each variable


# Assuming your data frame is named "df" and the gender column is named "gender"

# Convert 1s to "Female" and 2s to "Male"
data$Gender <- ifelse(data$Gender == 1, "Female", "Male")

# Create the boxplot with plotly
plot <- plot_ly(data = data, x = ~Gender, y = ~AcademicMotivation_Mean, type = "box",
                color = ~Gender, colors = c("blue", "pink"))

# Display the plot
plot

plot <- plot_ly(data = data, x = ~Gender, y = ~AcademicMotivation_Mean, type = "box",
                color = ~Gender, colors = c("black", "gray"))

# Add axis labels and plot title
plot <- plot %>% layout(yaxis = list(title = "Achievement Motive"),
                        xaxis = list(title = "Gender"),
                        title = "Boxplot of Academic Motivation by Gender")

# Display the plot
plot

# Define the shapes for males and females
gender_shapes <- c("M" = "circle", "F" = "square")

# Create the scatterplot
p <- plot_ly(data, x = ~AffPreference_Mean, y = ~AcademicMotivation_Mean, type = "scatter",
             mode = "markers",
             symbol = ~Gender, symbols = gender_shapes, color = ~Gender,
             marker = list(size = 10))

# Set the axis labels
p <- layout(p, xaxis = list(title = "AffPreference_Mean"), yaxis = list(title = "AcademicMotivation_Mean"))

# Display the plot
p

p <- ggplot(data, aes(x = AffPreference_Mean, y = AcademicMotivation_Mean, color = Gender,
                      shape = Gender)) +
  geom_point(size = 3) +
  xlab("AffPreference_Mean") +
  ylab("AcademicMotivation_Mean")

# Add separate lm() lines for males and females
p <- p + geom_smooth(method = "lm", se = FALSE)
p

p <- ggplot(data, aes(x = AffPreference_Mean, y = AcademicMotivation_Mean)) +
  geom_point(size = 3) +
  xlab("AffPreference_Mean") +
  ylab("AcademicMotivation_Mean") +
  facet_wrap(~ Gender, nrow = 1)

# Add lm() lines for each facet
p <- p + geom_smooth(method = "lm", se = FALSE)

# Display the plot
p
