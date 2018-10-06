library(dplyr)

load("selected_nzes2011.RData")

"
Exploring the relationship between the party the person voted for, the party that 
was their favourite, and if they believed that their vote makes a difference -

jpartyvote, _singlefav, and jdiffvoting

"
# check variable names
names(selected_nzes2011)

# find a particular name using regex
grep("singlefav", names(selected_nzes2011), value = TRUE) 
# value = TRUE to display the string, not the index number

# dplyr chain to select the variables of interest and investigate their structure
selected_nzes2011 %>% 
  select(jpartyvote, jdiffvoting, X_singlefav) %>% 
  str()

# summarise counts for jpartyvote
selected_nzes2011 %>%
  group_by(jpartyvote) %>%
  summarise(count = n())

# filter out "Don't Know" category as it is useless
selected_nzes2011 %>% 
  filter(jpartyvote != "Don't know") %>%
  group_by(jpartyvote) %>% 
  summarise(count = n())

# summarise counts for X_singlefav
selected_nzes2011 %>%
  group_by(X_singlefav) %>%
  summarise(count = n())

# filter out NA entries for X_singlefav and "Don't know" for jpartyvote
selected_nzes2011 %>% 
  filter(!is.na(X_singlefav), jpartyvote!= "Don't know") %>%
  group_by(X_singlefav) %>% 
  summarise(count = n())

# summarise jdiffvoting
selected_nzes2011 %>% 
  group_by(jdiffvoting) %>% 
  summarise(count = n())

# check if people voted for their favourite party
selected_nzes2011 <- selected_nzes2011 %>%
  mutate(sameparty = ifelse(jpartyvote == X_singlefav, "same", "different"))

selected_nzes2011 %>% 
  group_by(jpartyvote, X_singlefav, sameparty) %>%
  summarise(count = n())

# summarise "same" entries
selected_nzes2011 %>% 
  group_by(jpartyvote, X_singlefav, sameparty) %>%
  summarise(count = n()) %>% 
  filter(sameparty == "same")

# summarise "different" entries
selected_nzes2011 %>% 
  group_by(jpartyvote, X_singlefav, sameparty) %>%
  summarise(count = n()) %>% 
  filter(sameparty == "different")

# check number of NAs in 'sameparty'
selected_nzes2011 %>% 
  group_by(jpartyvote, X_singlefav, sameparty) %>%
  summarise(count = n()) %>% 
  filter(is.na(sameparty))
" NAs in sameparty will be removed when NAs from jpartyvote and X_singlefav are filtered out. "


"
exploring the relationship between age of voters and how much they like the NZ First party - 

jage and jnzflike 

"

# analyse the variables
str(selected_nzes2011$jnzflike)
str(selected_nzes2011$jage)
" jnzlike has factors, jage has integers. "

# summarise jnzflike
selected_nzes2011 %>% 
  group_by(jnzflike) %>% 
  summarise(count = n())

# calculate numerical summaries for jage
selected_nzes2011 %>% 
  summarise(agemean = mean(jage), agemedian = median(jage), agesd = sd(jage), 
          agemin = min(jage), agemax = max(jage))
" NAs must be removed first."

# filter out NAs and calculate numerical summaries
selected_nzes2011 %>% 
  filter(!(is.na(jage))) %>%
  summarise(agemean = mean(jage), agemedian = median(jage), agesd = sd(jage), 
            agemin = min(jage), agemax = max(jage))

"
Approach 1: Strongly liking and disliking NZ First and age

"

# selecting only two levels for like and dislike
selected_nzes2011 %>% 
  filter(jnzflike %in% c("0","10")) %>%
  group_by(jnzflike) %>% 
  summarise(count = n())
# When a variable can take multiple values, the %in% operator is used for filtering


"
Approach 2: Age and liking for NZ First

Asking if people above retirement age (65 in New Zealand) like NZ First more than younger people. 
"

# turn the numeric age variable into a categorical variable based on retirement age.
selected_nzes2011 <- selected_nzes2011 %>% 
  mutate(retiredage = ifelse(jage >= 65, "retired age", "working age"))

selected_nzes2011 %>% 
  group_by(retiredage) %>% 
  summarise(count = n())

# convert factors of jnzflike to numeric data
# as.numeric() function won't work because the factors are converted to numbers of their place in the ordering
# therefore, the factors are converted to characters first
selected_nzes2011 <- selected_nzes2011 %>% 
  mutate(numlikenzf = as.numeric(as.character(jnzflike)))
# A warning is thrown since "Don't know" cannot be converted to number, but it doesn't matter since they are to be ignored.

# check the new variable
selected_nzes2011 %>% 
  group_by(jnzflike, numlikenzf) %>% 
  summarise(count = n())