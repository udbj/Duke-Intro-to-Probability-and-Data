#install.packages("devtools")
library(devtools)
#install.packages("dplyr")
#install.packages("ggplot2")
#install.packages("shiny")
#install_github("StatsWithR/statsr")

library(dplyr)
library(ggplot2)
library(statsr)

data(arbuthnot) #load arbuthnot dataset
print(arbuthnot)
dim(arbuthnot) # 82x3
class(arbuthnot) #type: data frame

#Exercise: What years are included in this dataset? 
arbuthnot$year
range(arbuthnot$year)

#simple plot of the number of girls baptized per year 
ggplot(data=arbuthnot,aes(x = year,y = girls)) + geom_point()
#first arg = dataset, second arg = aesthetic elements i.e x and y axes
#Addition (+) symbol is used to specify another layer. geom_point() generates a scatterplot.

"There is initially an increase in the number of girls baptised, which peaks around 1640. 
After 1640 there is a decrease in the number of girls baptised, but the number begins to increase again in 1660. 
Overall the trend is an increase in the number of girls baptised."

#total baptisms
totl <- data.frame("Year" = arbuthnot$year, "Baptisms" = arbuthnot$boys + arbuthnot$girls)
totl


#add total to original dataset
arbuthnot <- arbuthnot %>% mutate(total = boys + girls)
arbuthnot

# %>% is the piping operator. 
# The function mutate() is in the dplyr package. It is used to do computations and add columns in data sets. Can be used with pipes.

# line plot of total baptisms
ggplot(data = arbuthnot, aes(x = year, y = total)) + geom_line()
#layers are simply added using +
ggplot(data = arbuthnot, aes(x = year, y = total)) + geom_line() + geom_point()

# Exercise: Now, generate a plot of the proportion of boys born over time.
ggplot(data = arbuthnot, aes(x = year, y = boys)) + geom_line()

# add a new column, which indicates if the number of boys was greater than girls for a particular year
arbuthnot <- arbuthnot %>% mutate(more_boys = boys > girls)


