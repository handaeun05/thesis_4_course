#Importing libraries
install.packages("skimr")
install.packages("gganimate")
library(tidyverse)
library(haven)
library(ggplot2)
library(gganimate)
library(foreign)
library(skimr)

library(dplyr)

df <- read_csv("C:/Users/Catherine/Documents/한다은/For thesis/KLoSA_STATA_v3_20210324/klosa.csv")
head(df)
tail(df)

df <- read_dta("C:/Users/Catherine/Documents/한다은/For thesis/KLoSA_STATA_v3_20210324/str01-08.dta")
head(df)

# Getting sample data.

#1 - KReil
df <- read.spss("dataset.sav", use.value.label=TRUE, to.data.frame=TRUE)

#2 - KLoSA
df <- read_dta("C:/Users/Catherine/Documents/한다은/For thesis/KLoSA_STATA_v3_20210324/data18-20.dta")
head(df)

df1 <- read.spss("C:/Users/Catherine/Documents/한다은/For thesis/[spss]kreisw/kreishw.sav", use.value.label=TRUE, to.data.frame=TRUE)
View(df1)
#View data
View(df)

my.data <- read_spss("C:/Users/Catherine/Documents/한다은/For thesis/[spss]kreisw/kreishw.sav")
View(my.data)

#dataset is too big, let's select necessary columns and make new dataset
mini <- df %>% select(pid, w08gender1, w08marital, w08A002_age, w08A032, w08region1, w08Ba003, w08G027, w08edu, w08dep1, w08sumcesd)
#data summary
summary(mini)

# Creating a dummy variable to indicate the time when the treatment started. 
df$time <- ifelse(df$w08mniw_y >= 2019, 1, 0)

# Creating a dummy variable to identify the group exposed to the treatment.
df$treated <- ifelse(df$w08dep1 == 0, 0, 1)

# Creating an interaction between time and treated. We will call this interaction ??did??.
df$did <- df$time * df$treated

# Estimating the DID estimator
didreg <- lm(w08sumcesd ~ treated + time + did, data = df)
summary(didreg)

# The coefficient for ??did?? is the differences-in-differences estimator. 
#The effect is significant at 5% with the treatment having a negative effect.

# Estimating the DID estimator (using the multiplication method, no need to generate the interaction)
didreg1 <- lm(w08sumcesd ~ treated*time, data = df)
summary(didreg1)

skim(df, w08gender1, w08marital, w08A002_age, w08A032, w08region1, w08Ba003, w08G027, w08edu) %>% 
  skimr::kable(digits = 0)


data <- select(df, w01gender1, w01marital, w01A002_age, w01A032, w01region3, w01Ba003, w01G027, w01edu)
summarize(data)
data$w01gender1 <- ifelse(df$w01gender1 == "여자", 0, 1)
data$w01marital <- ifelse(df$w01marital == "", 1, 0)
data$w01marital <- ifelse(df$w01marital == "ȥ????", 1, 0)

head(data)


#plots
mean_cesd <- w08sumcesd %>% filter(!is.na(w08sumcesd)) %>% group_by(treated) %>% sum(mean_cesd = mean(w08sumcesd))
ggplot(data =df, aes(x = w08mniw_y, y = w08sumcesd, group= treated, color = treated)) +
  stat_summary(geom = 'line') +
  geom_vline(xintercept = 2018) +
  theme_minimal() + 
  scale_x_continuous(breaks=unique(df$w08mniw_y)) +
  labs(title="Difference in Differences", x="Time", y="CESD10")

#+  transition_reveal(w08mniw_y)
#anim_save(filename = "ddanim.gif")
