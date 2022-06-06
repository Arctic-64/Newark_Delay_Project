library(tidyverse)
load("D:/UNIVERSITY/CodeClan/codeclan_homework_JackTiberiusPatton/week_05/day_2/Backpack.rda")

backpack

nrow(backpack)

summary(backpack)

colnames(backpack)

## 100 rows, 9 columbs



ggplot(backpack) +
  aes(x = backpack_weight, y = units) +
  geom_col() 

## the mode backpack weight was 10 and looks pretty simmilar to the mean
## the distribution of backpack weights show the students have a preferance for backpacks slightly under the median weight
## a small subset of students exist that have a preferance for heavier backpacks
## this is likely due to a minority of students who require backpacks with strenth over weight
## this explains why there are no instances of backpacks which occur in the 26-34 region of weight


ggplot(backpack) +
  aes(x = backpack_weight, y = units, fill = sex) +
  geom_col() 

## it is hard to determine from the graph if the number of backbacks owned by females is greater
## however, regaurdless of the total volume of backpacks per gender, it is clear that females have a preferance for backpacks slightly in the lower weight catagory.
## given the distribution of the data, i suspect the backpack weights are not the primary factor,
## i suspect that another factor which has in effect on the resultant weight of the backpack drives selection
## i base this on the vastly most popular female backpack being ajacent to the most popular male backpack.
## if weight were the defineing factor, i would expect more crossover between these two groups.

ggplot(backpack) +
  aes(x = body_weight, y = backpack_weight) +
  geom_point() 

## i can see no solid relationship between body weight and backpack weight in the scatter plot
## there is a very broad and general trend of backpack weight increasing as bodyweight increases
## however the datapoints are too randomly spread to tell.

ggplot(backpack) +
  aes(x = body_weight, y = backpack_weight, colour = sex) +
  geom_point() 

## when splitting the above data by sex, the pattern becomes more apparrent.
## while the trend is still very general, females bodyweight increase appearst to cause a more substantial increase in backpack weight than seen with the males
## males appear to have a larger increase in bodyweight for the same general trent increase for backpack weight when compaired to females

backpack %>% count(back_problems) -> back_problem_counts
pie(back_problem_counts$n, labels = c("no_problems","back_problems"))

## i thinks the pie chart speaks for itself.
## most people dont have back problems

ggplot(backpack) +
  aes(x = back_problems, y = units, fill = sex) +
  geom_col() 

## the numebr of males and females who did not report back problems was about the same,
## however the number of femals who did report back problems was significantly higher than the males

## this couls be due to data imbalence, the biology of men haveing an avarage 40% extra upper body strenth or the proclivity of mens personality to refuse to report pain.

