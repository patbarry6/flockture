## Here are some fun things we can do during
## developemnt of this thing.
library(ggplot2)
library(stringr)

source("R/flockture.R")

# read in a small data set:
D <- read.table("data/loo_test1.txt", row.names = 1)

# this is the starting configuration we want
startc <- c(rep(1, 5), rep(2, 7))


# run flockture on it and grab the results out
catch <- run_flockture_bin(D, K = 2, iter = 2, reps = 1, start_config = startc)
out <- slurp_flockture_dumpola()


# summarize by plateaus and add that to our output
psum <- plateau_summarize(out)


# now, draw lines of log_prob against iterations for the 
# reps, and color them according to plateau length
ggplot(psum$log_probs, aes(x = iter, y = log.prob, group = rep, color = factor(plat.len))) + 
  geom_line() +
  scale_color_discrete(name="Plateau Length")


# here is a vector of the different plateau lengths in sorted order:
sapply(psum$plateau_list, length)
