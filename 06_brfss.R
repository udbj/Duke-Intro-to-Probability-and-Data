#install.packages("tidyr")
library(tidyr)
library(dplyr)
library(ggplot2)

load("brfss2013.RData")
" Since the people are selected using Random Digit Dialing (RDD) techniques,
  the data should be observational, random, and free from bias."


" 
Is there a correlation between hours of sleep and mental health? 

 sleptim1: How Much Time Do You Sleep

 menthlth: Number Of Days Mental Health Not Good

 addepev2: Ever Told You Had A Depressive Disorder

 misnervs: How Often Feel Nervous Past 30 Days
 mishopls: How Often Feel Hopeless Past 30 Days
 misrstls: How Often Feel Restless Past 30 Days
 misdeprd: How Often Feel Depressed Past 30 Days
 miseffrt: How Often Feel Everything Was An Effort Past 30 Days
 miswtles: How Often Feel Worthless Past 30 Days
 misnowrk: Emotional Problem Kept You From Doing Work Past 30 Days
 mistmnt: Receiving Medicine Or Treatment From Health Pro For Emotional Problem

"

str(brfss2013$sleptim1) # int vector
print.data.frame(
  brfss2013 %>% 
    group_by(sleptim1) %>% 
    summarise(count = n())
  )


# filter out entries where sleep time is over 24 hours, less than 1 or NA
# select sleep time column for analysis
sleepObs <- brfss2013 %>%
  filter(!is.na(sleptim1), !(sleptim1 > 24), !(sleptim1 < 1)) %>%
  select(sleptim1)


# view observations according to sleep time
print.data.frame(
  sleepObs %>% 
    group_by(sleptim1) %>%
    summarise(count = n(), freq = format(x = count/nrow(sleepObs),digits = 2))
)
# Majority of people sleep for 7, 8 or 9 hours. Combined proportion = 0.8/1


# summary statistics for average sleep times
sleepObs %>% summarise(med = median(sleptim1),
                        iqr = IQR(sleptim1),
                        mean = format(mean(sleptim1), digits = 3),
                        sd = format(sd(sleptim1), digits = 3)
                        
                        )


# visualise the distribution using histogram 
x11()
ggplot(data = sleepObs, aes(x = sleptim1)) + 
  geom_histogram(binwidth = 1) +
  xlab("Hours of sleep") +
  ylab("Number of people")
# right skewed distribution

# box plot for the data
x11()
ggplot(data = sleepObs, aes(y = sleptim1)) + 
  geom_boxplot(outlier.colour = "red") +
  ylab("Hours of sleep")
# observations below 3 hours and above 11 hours are outliers

rm(sleepObs) #cleanup

" Relation between sleep and number of days where mental health wasn't good "

str(brfss2013$menthlth)
# integer vector

# select mental health and sleep time columns
menhthObs <- brfss2013 %>% 
  filter(!is.na(sleptim1), (sleptim1 > 0), (sleptim1 < 25)) %>%
  select(sleptim1, menthlth)

# check the proportion of observations with missing mental health data
print.data.frame(
  menhthObs %>%
    group_by(sleptim1) %>%
    summarise(totl = n(), NAs = sum(is.na(menthlth)), NA_pc = NAs*100/totl)
  
)
# percentage of missing values is low, except for 21 and 23 hours

# filter out NA values in menthlth column
menhthObs <- menhthObs %>%
  filter(!is.na(menthlth))

range(menhthObs$menthlth) # data is for 1 month

# overall summary statistics for number of days with mental health problems
print.data.frame(
  menhthObs %>% summarise(mean = mean(menthlth), sd = sd(menthlth),
                          med = median(menthlth), iqr = IQR(menthlth))
)

# check statistics for days where mental health wasn't good, grouped by sleep time
menhthProp <- menhthObs %>%
  group_by(sleptim1) %>%
  summarise(count = n(), menhth_avg = mean(menthlth), menhth_sd = sd(menthlth),
            menhth_md = median(menthlth), iqr = IQR(menthlth))

print.data.frame(menhthProp)

# plot average number of days with mental problems vs sleep
x11()
ggplot(data = menhthProp, aes(x = sleptim1, y = menhth_avg)) + 
  geom_line() + geom_point() +
  geom_rect(mapping = aes(xmin = 3, xmax = 11, ymin = -Inf, ymax = Inf), fill = "green", alpha = 0.005) +
  xlab("Hours of sleep") +
  ylab("Avg. num. of days with mental problems")
  
" People who slept for 7,8 or 9 hours had the least number of days with mental problems 
  on average. Average number of problematic days rises as the hours of sleep rise above 
  9 hours or drop below 7.
"

rm(menhthObs, menhthProp) #cleanup

" Relation between depressive disorders and sleep "

str(brfss2013$addepev2)
# categorical variable with 2 levels "Yes" & "No"

# select sleep time and depressive disorder columns
depObs <- brfss2013 %>% 
  filter(!is.na(sleptim1), (sleptim1 > 0), (sleptim1 < 25)) %>%
  select(sleptim1, addepev2)

# check the proportion of observations with missing depression data
print.data.frame(
  depObs %>%
    group_by(sleptim1) %>%
    summarise(totl = n(), NAs = sum(is.na(addepev2)), NA_pc = NAs*100/totl)
  
)
# percentage of NAs in addepev2 column is very small, so analysis can be performed


# filter out observations with NAs in addepev2
depObs <- depObs %>%
  filter(!is.na(addepev2))

# calculate proportion of "Yes" for each sleep time
depProp <- depObs %>% 
  group_by(sleptim1) %>%
  summarise(count = n(), depr = sum(addepev2 == "Yes"), depr_pc = depr*100/count)

print.data.frame(depProp)

# plot the proportion of depressive disorders according to sleep time
x11()
ggplot(data = depProp, aes(x = sleptim1, y = depr_pc)) + 
  geom_line() + geom_point() +
  geom_rect(mapping = aes(xmin = 3, xmax = 11, ymin = -Inf, ymax = Inf), fill = "green", alpha = 0.005) +
  xlab("Hours of sleep") + ylab("% of people who've had depression") +
  scale_y_continuous(labels = function(x) paste0(x,"%"))

" People who slept for 7 or 8 hours had the lowest incidence of depression.
  There is a steep increase in the rate for people who slept below 7 or above 8 hours."

rm(depObs, depProp) #cleanup


" Relation between nervousness in last 30 days and sleep "

str(brfss2013$misnervs)
# vector of 5 ordinal variables, describing proportion of days where nervousness was felt

# select sleep time and nervousness columns
nervObs <- brfss2013 %>%
  filter(!is.na(sleptim1), (sleptim1 <25), sleptim1 > 0) %>%
  select(sleptim1, misnervs)

# check the proportion of observations with missing nervousness data

sum(is.na(nervObs$misnervs))*100/nrow(nervObs)
# 92% missing overall

print.data.frame(
  nervObs %>%
    group_by(sleptim1) %>%
    summarise(totl = n(), NAs = sum(is.na(misnervs)), NA_pc = NAs*100/totl)
  
)
# over 90% NAs for all sleep time categories.


rm(nervObs) #cleanup

# check missing values for all mental health parameters
mentObs <- brfss2013 %>%
  filter(!is.na(sleptim1), (sleptim1 <25), sleptim1 > 0) %>%
  select(sleptim1, misnervs, mishopls, misrstls, misdeprd, 
         miseffrt, miswtles, misnowrk, mistmnt)

print.data.frame(
  mentObs %>%
    group_by(sleptim1) %>%
    summarise(totl = n(), nerv_pc = sum(is.na(misnervs))*100/totl,
              hopls_pc = sum(is.na(mishopls))*100/totl,
              rstls_pc = sum(is.na(misrstls))*100/totl,
              depr_pc = sum(is.na(misdeprd))*100/totl,
              effrt_pc = sum(is.na(miseffrt))*100/totl,
              wtls_pc = sum(is.na(miswtles))*100/totl,
              nowrk_pc = sum(is.na(misnowrk))*100/totl,
              trt_pc = sum(is.na(mistmnt))*100/totl)
  
)

" percentages of missing values are very high for all mental health parameters
  optional modules of the survey may not be a good source of data"

rm(mentObs) #cleanup

" Overall, we can see that people who slept 7 or 8 hours had the lowest 
occurrence of depression, and the lowest number of days with mental health 
issues on average. "



"------------------------------------------------------------------------------------------"



" 
Is there a correlation between education level and tobacco use? 

 educa: Education Level

 smoke100: Smoked At Least 100 Cigarettes
 smokday2: Frequency Of Days Now Smoking
 stopsmk2: Stopped Smoking In Past 12 Months
 lastsmk2: Interval Since Last Smoked
 usenow3: Use Of Smokeless Tobacco Products

"

str(brfss2013$educa) # ordinal variable
sum(is.na(brfss2013$educa)) # small number of NAs
educa_labs = c("KGTN", "ELMTRY", "HISCL", "HISCL_GRD", "COL/TECSCL", "COL_GRD")


" Relation between education level and whether 100 cigarettes have been smoked"

str(brfss2013$smoke100) # categorical - "Yes" and "No"

# select education and 100 cigarettes columns
s100 <- brfss2013 %>%
  filter(!is.na(educa)) %>%
  select(educa, smoke100)

# check % of missing 100 cig observations
sum(is.na(s100$smoke100))*100 / nrow(s100)
# only 2.85% missing

# filter out NA observations
s100 <- s100 %>%
  filter(!is.na(smoke100))

# total percentage of people who have smoked 100 cigarettes
sum(s100$smoke100 == "Yes")*100/nrow(s100)
# 45.14% have smoked at least 100 cigarettes

# whether 100 cigarettes have been smoked, grouped by education level
s100prop <- s100 %>% group_by(educa) %>%
  summarise(count = n(), smoke100 = sum(smoke100 == "Yes"), smoke100_pc = smoke100*100/count)

print.data.frame(s100prop)

# visualise
x11()
ggplot(data = s100prop , aes(x = educa, y = smoke100_pc)) + 
  geom_col() +
  scale_x_discrete(labels = educa_labs) +
  ylab("% who've smoked 100 cigs") +
  xlab("Education Level") +
  scale_y_continuous(labels = function(x) paste0(x,"%"))

" People who've been to high school but not graduated smoke the most (~60%). 
  People who've only been to Kindergarten or graduated college smoke the least (~35%).
  There is an increase from kindergarten to high school, and then a drop till college graduate."


rm(s100, s100prop) #cleanup



" Relation between education level and frequency of days smoking at present "

# select education and cig frequency columns
sfrq <- brfss2013 %>%
  filter(!is.na(educa)) %>%
  select(educa, smokday2)

# check % of missing cig frequency observations
sum(is.na(sfrq$smokday2))*100 / nrow(sfrq)
# 56.22% missing

NAtest <- brfss2013 %>%
  filter(!is.na(smoke100)) %>% select(smoke100, smokday2)

NAtest %>% group_by(smoke100) %>% 
  summarise(count = n(), smokday_na = sum(is.na(smokday2)))
# Only one person who hasn't smoked 100 cigs has answered the smokday2 question
#  0.19 % of people who have smoked 100 cigs have left the smokday2 field blank

" Therefore, the relation will be checked only for people who have smoked at least 100 cigs "

rm(NAtest) #cleanup

# filter out NA observations
sfrq <- sfrq %>%
  filter(!is.na(smokday2))


# counts of smoking, grouped by education level
sfrqProp <- sfrq %>% group_by(educa) %>%
  summarise(evd_pc = sum(smokday2 == "Every day")*100/n(),
            smd_pc = sum(smokday2 == "Some days")*100/n(),
            noa_pc = sum(smokday2 == "Not at all")*100/n()) 

print.data.frame(sfrqProp)

# visualisation

long_sfrqProp <- sfrqProp %>% gather(key = freq, value = pc, evd_pc, smd_pc, noa_pc)
#convert to long format


x11()
ggplot(data = long_sfrqProp, aes(x = educa, y = pc,  fill = freq)) + 
  geom_col() + scale_x_discrete(labels = educa_labs) +
  scale_y_continuous(labels = function(x) paste0(x,"%")) +
  xlab("Education Level") + ylab("Percent of people") +
  scale_fill_discrete(name = "Frequency of smoking",
                      breaks = c("evd_pc","noa_pc","smd_pc"),
                      labels = c("Every day","Not at all","Some days"))
  
"
'High school but not graduated' has the highest proportion of people who smoke daily, 
 and the lowest proportion of people who never smoke. 'College graduate' is the opposite.

 Proportion of daily smokers rises from kindergarten to high school, and then decreases till
 college graduate. Opposite for proportion of people who do not smoke. 

"

rm(sfrq, sfrqProp, long_sfrqProp) #cleanup


" Relation between education level and whether the person stopped smoking in the last 12 months "

str(brfss2013$stopsmk2) # categorical - "Yes" and "No"

# select education level and stopped smoking columns
stopObs <- brfss2013 %>%
  filter(!is.na(educa)) %>%
  select(educa, stopsmk2)

# check proportion of NAs
sum(is.na(stopObs$stopsmk2))*100/nrow(stopObs)
# 84.42% missing observations

NAtest <- brfss2013 %>%
  filter(!is.na(smoke100)) %>% select(smoke100, stopsmk2)

print.data.frame(
  NAtest %>% group_by(smoke100) %>%
    summarise(NA_pc = sum(is.na(stopsmk2))*100/n())
  )
# 99.99% observations with smoke100 = "No" have this field as NA

NAtest <- brfss2013 %>%
  filter(!is.na(smokday2)) %>% select(smokday2, stopsmk2)

print.data.frame(
  NAtest %>% group_by(smokday2) %>%
    summarise(NA_pc = sum(is.na(stopsmk2))*100/n())
)

# 100% of the people who do not smoke at all have NA for this field
# less than 1% of the people who smoke every day or on some days have left this field blank

" Therefore, the analysis will be for people who have smoked at least 100 cigarettes
  and who still smoke nowadays, either daily or from time to time."

rm(NAtest) #cleanup

# filter observations with NA stopsmk2 values
stopObs <- stopObs %>% filter(!is.na(stopsmk2))

# check proportion of people who stopped smoking, grouped by education level
stopObs_prop <- stopObs %>%
  group_by(educa) %>%
  summarise(stopped_pc = sum(stopsmk2 == "Yes")*100/n())

print.data.frame(stopObs_prop)

# visualise
x11()
ggplot(data = stopObs_prop, aes(x = educa, y = stopped_pc)) +
  geom_col() + scale_x_discrete(labels = educa_labs) +
  scale_y_continuous(labels = function(x) paste0(x,"%")) +
  xlab("Education Level") + ylab("Quit smoking")

"Percentages are fairly equal, with a slight, continuous drop from kindergarten to 
 college graduate, and a spike at 1-3 year college education category. "

rm(stopObs, stopObs_prop) #cleanup 



" correlation between education and interval since last smoked "

str(brfss2013$lastsmk2) #ordinal variable

# check proportion of NAs
sum(is.na(brfss2013$lastsmk2))*100/nrow(brfss2013)
#72.08% observations have this missing

NAtest <- brfss2013 %>%
  filter(!is.na(smoke100)) %>% select(smoke100, lastsmk2)

print.data.frame(
  NAtest %>% group_by(smoke100) %>%
    summarise(NA_pc = sum(is.na(lastsmk2))*100/n())
)
# 100% of the people who haven't smoked 100 cigs have left this field empty

NAtest <- brfss2013 %>%
  filter(!is.na(smokday2)) %>% select(lastsmk2, smokday2)

print.data.frame(
  NAtest %>% group_by(smokday2) %>%
    summarise(NA_pc = sum(is.na(lastsmk2))*100/n())
)
# Only people who do not smoke at all nowadays have answered this question

NAtest <- brfss2013 %>%
  filter(!is.na(stopsmk2)) %>% select(stopsmk2, lastsmk2)

print.data.frame(
  NAtest %>% group_by(stopsmk2) %>%
    summarise(NA_pc = sum(is.na(lastsmk2))*100/n())
)
# No one who has answered the 'stopped smoking' question has answered this one

" Therefore, the analysis will be for people who have smoked 100 cigarettes 
  and do not smoke at all nowadays. "

rm(NAtest) #cleanup

# select education level and last smoke columns
lastsmk <- brfss2013 %>% 
  filter(!is.na(educa), !is.na(lastsmk2)) %>%
  select(educa, lastsmk2)

# check distribution of lastsmk2 grouped by education level
unique(lastsmk$lastsmk2)

lastsmk_prop <- lastsmk %>%
  group_by(educa) %>%
  summarise("10y+" = sum(lastsmk2 == "10 years or more")*100/n(),
            "-10y-" = sum(lastsmk2 == "Within the past 10 years")*100/n(),
            "-5y-" = sum(lastsmk2 == "Within the past 5 years")*100/n(),
            "-1y-" = sum(lastsmk2 == "Within the past year")*100/n(),
            "-6m-" = sum(lastsmk2 == "Within the past 6 months")*100/n(),
            "-3m-" = sum(lastsmk2 == "Within the past 3 months")*100/n(),
            "-1m-" = sum(lastsmk2 == "Within the past month")*100/n(),
            "nvr" = sum(lastsmk2 == "Never smoked regularly")*100/n())

print.data.frame(lastsmk_prop)

# visualisation
long_lstsmkprp <- lastsmk_prop %>% gather(key = last_smk, value = pc,
                                          "10y+", "-10y-", "-5y-", "-1y-",
                                          "-6m-", "-3m-", "-1m-", "nvr")


x11()
ggplot(data = long_lstsmkprp, aes(x = educa, y = pc,  fill = last_smk)) + 
  geom_col() + scale_x_discrete(labels = educa_labs) +
  scale_y_continuous(labels = function(x) paste0(x,"%")) +
  xlab("Education Level") + ylab("Percent of people") +
  scale_fill_discrete(name = "Last smoked",
                      breaks = c( "-10y-","-1m-", "-1y-", "-3m-", "-5y-", 
                                  "-6m-", "10y+", "nvr"),
                      labels = c("-10y-","-1m-", "-1y-", "-3m-", "-5y-", 
                                 "-6m-", "10y+", "never"))

"There doesn't seem to be any trend here. 'High school' category has the lowest % of people
 who last smoked 10+ years ago, and 'College graduate' has the highest. "

rm(lastsmk, lastsmk_prop, long_lstsmkprp) #cleanup


" Relation between education level and use of smokeless tobacco products"

str(brfss2013$usenow3) #ordinal variable

# check proportion of NAs
sum(is.na(brfss2013$usenow3))*100/nrow(brfss2013)
# 2.85% missing observations


# select education and smokeless tobacco usage columns
tobacObs <- brfss2013 %>%
  filter(!is.na(educa), !is.na(usenow3)) %>%
  select(educa, usenow3)

# check tobacco usage grouped by education level

unique(tobacObs$usenow3)

tobacoProp <- tobacObs %>%
  group_by(educa) %>%
  summarise(evry_pc = sum(usenow3 == "Every day")*100/n(),
            some_pc = sum(usenow3 == "Some days")*100/n(),
            noa_pc = sum(usenow3 == "Not at all")*100/n())

print.data.frame(tobacoProp)

# visualisation
long_tobacoProp <- tobacoProp %>% gather(key = freq, value = pc, evry_pc, some_pc, noa_pc)

x11()
ggplot(data = long_tobacoProp, aes(x = educa, y = pc, fill = freq)) +
  geom_col() + scale_x_discrete(labels = educa_labs) +
  scale_y_continuous(labels = function(x) paste0(x,"%")) +
  xlab("Education Level") + ylab("Percent of people consuming smokeless tobacco stuff") + 
  scale_fill_discrete(name = "Frequency",
                      breaks = c( "evry_pc","noa_pc","some_pc"),
                      labels = c("Every day","Not at all","Some days"))

"'High School' category has the highest proportion of people who consume smokeless 
tobacco every day or on some days, and the lowest proportion of people who do not 
consume them at all.

There's a continuous increase in daily and occasional consumption from 'kindergarten' 
to 'high school', and then a continuous decrease till 'college graduate' category. 
"

#cleanup
rm(long_tobacoProp, tobacObs, tobacoProp, educa_labs) 
rm(brfss2013)



" The incidence of people of had smoked at least 100 cigarettes was the highest in
 people who had not graduated high school, and the least for college graduates. 

 Among people who had smoked 100 cigarettes,  the ones who attended high school but did not
 graduate had the highest incidence of daily smoking, and the lowest for not smoking.
 It was the opposite for college graduates.

 For people who had smoked at least 100 cigarettes and still smoked daily or occasionally, 
 the proportion of people who'd stopped smoking in the last 12 months was fairly equal 
 across all education categories, with a slight, continuous drop from kindergarten to 
 college graduate.

 In people who had smoked 100 cigarettes but did not smoke at all nowadays, there was no
 trend noticed when the proportions of people were checked according to last time they 
 smoked. College graduates had the highest percentage of people who had last smoked over
 10 years ago.
 
 When the consumption data of smokeless tobacco products was examined, it was found that
 people who had been to high school but not graduated had the highest incidence of daily and
 occasional use. College graduates had the least for both.

 Overall, the general recurring pattern in the analyses was of tobacco consumption being
 fairly low among people who had never attended school or only kindergarten, more in people
 who had completed elementary schooling, and the highest for people who had attended high
 school but not graduated. From there onwards, with increasing level of education,
 there was a decrease in tobacco use, from high school graduates having lower consumption
 than droupouts, to college graduates that had the lowest incidence of tobacco use.

"


