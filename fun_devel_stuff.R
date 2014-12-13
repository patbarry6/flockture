## Here are some fun things we can do during
## developemnt of this thing.
library(ggplot2)

source("R/flockture.R")

# read in a small data set:
D5 <- read.table("data/small_data.txt", row.names = 1)


# define some starting conditions if you want:
sc <- c(rep(c(1,2), length.out = nrow(D2)),
        rep(c(2,1), length.out = nrow(D2)),
        rep(c(1,1,2,2), length.out = nrow(D2)),
        rep(c(1,1,1,2,2,2), length.out = nrow(D2)),
        rep(c(2,2,2,1,1,1), length.out = nrow(D2))
)

# here we could start it at the correct configuration:
small_start_correct <- as.numeric(str_sub(rownames(D5), 5, 5))


# run flockture on it and grab the results out
d2_flokt <- run_flockture_bin(D5, K = 2, iter = 20, reps = 50, start_config = small_start_correct)
d2_outs <- slurp_flockture_dumpola()


# summarize by plateaus and add that to our output
d2_outs_p <- plateau_summarize(d2_outs)


# now, draw lines of log_prob against iterations for the 
# reps, and color them according to plateau length
xx <- d2_outs_p$log_probs[d2_outs_p$log_probs$plat.len > 1, ]
ggplot(d2_outs_p$log_probs, aes(x = iter, y = log.prob, group = rep, color = factor(plat.len))) + 
  geom_line() +
  scale_color_discrete(name="Plateau Length")


# here is a vector of the different plateau lengths in sorted order:
sapply(d2_outs_p$plateau_list, length)
