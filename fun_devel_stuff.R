## Here are some fun things we can do during
## developemnt of this thing.
library(ggplot2)
library(stringr)

source("R/flockture.R")

# read in a small data set:
D <- read.table("data/small_data.txt", row.names = 1)

# here we could start it at the correct configuration:
small_start_correct <- as.numeric(str_sub(rownames(D), 5, 5))

# define some starting conditions if you want:
sc <- c(small_start_correct,
        rep(c(1,2), length.out = nrow(D)),
        rep(c(2,1), length.out = nrow(D)),
        rep(c(1,1,2,2), length.out = nrow(D)),
        rep(c(1,1,1,2,2,2), length.out = nrow(D)),
        rep(c(2,2,2,1,1,1), length.out = nrow(D))
)




# run flockture on it and grab the results out
catch <- run_flockture_bin(D, K = 2, iter = 20, reps = 60, start_config = sc)
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
