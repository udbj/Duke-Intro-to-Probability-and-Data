data(present)
library(dplyr)
library(ggplot2)
library(statsr)

present
dim(present) 
# 3 variables, 74 records

present$year
range(present$year)
# data from 1940 to 2013

# total number of births for each year
present <- present %>% mutate(total = boys + girls)
present$total

# proportion of boys
present <- present %>% mutate(prop_boys = boys/total)
present$prop_boys

#plotting proportion of boys
x11()
ggplot(data = present, aes(x = year,y = prop_boys)) + geom_line()
# proprtion of boys has declined over time

# checking if more boys were born compared to girls
present<- present %>% mutate(more_boys = boys > girls)
sum(present$more_boys == TRUE)
sum(present$more_boys == FALSE)
# more boys were born in every recorded year

# boy-to-girl ratios
present <- present %>% mutate(prop_boy_girl = boys/girls)
x11()
ggplot(data = present, aes(x = year,y = prop_boy_girl)) + geom_line()
"There is initially a decrease in the boy-to-girl ratio, and then an increase between 1960 and 1970, followed by a decrease."


max_births_row <- which.max(present$total)
present$year[max_births_row]
# maximum births were in year 2007

# arrange in descending order
present %>% arrange(desc(present$total))
