library(statsr)
library(dplyr)
library(ggplot2)

data(kobe_basket) # dataset of shots by Kobe Bryant
str(kobe_basket)

kobe_basket %>% filter(game == 1)

"A streak length of 1 means one hit followed by one miss."
"A streak length of 0 means one miss which must occur after a miss that ended the preceeding streak. "

kobe_streak <- calc_streak(kobe_basket$shot) # function in statsr to calculate streak lengths for this dataset
str(kobe_streak)

# plot streak lengths on histogram
x11()
ggplot(data = kobe_streak,aes(x = length)) + geom_histogram(binwidth = 1)

kobe_streak %>% summarise(median = median(kobe_streak$length), iqr = IQR(kobe_streak$length))
"FALSE: The shortest streak is of length 1. "


# simulations for random processes can be generated in R.
# coin toss simulation:

coin_outcomes <- c("heads", "tails")
sim_fair_coin <- sample(coin_outcomes, size = 100, replace = TRUE)
# toss it 100 times and replace the coin tossed.

sim_fair_coin # results of simulation
table(sim_fair_coin) # tabulate the results

# to set probabilities for outcomes, the argument 'prob' is supplied with a vector
sim_unfair_coin <- sample(coin_outcomes, size = 100, replace = TRUE, prob = c(0.2,0.8))
sim_unfair_coin
table(sim_unfair_coin)

# simulation of independent shooter with a shooting percentage of 45%
shoot_outcomes <- c("H","M")
sim_basket <- sample(x = shoot_outcomes, size = 133, replace = TRUE, prob = c(0.45,0.55))
table(sim_basket)

# calculate streaks for independent shooter
sim_streak <- calc_streak(sim_basket)

# plotting simulated streak lengths
x11()
ggplot(data = sim_streak, aes(x = length)) + geom_histogram(binwidth = 1)

"Typical streak length is 0. Longest streak is of six baskets."
"If the simulation of the independent shooter were to run a second time, its streak distribution would be somewhat similar. "
"The distributions look very similar. Therefore, there doesn’t appear to be evidence for Kobe Bryant’s hot hand. "
