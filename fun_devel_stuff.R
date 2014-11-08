## Here are some fun things we can do during
## developemnt of this thing.
library(ggplot2)



# read in the 4000 individuals at Fst = 0.03
D5 <- read.table("data/struct_input_1.txt", row.names = 1)

# take just the first two pops
D2 <- D5[ grep("^Pop_[12]", rownames(D5)), ]

# run flockture on it and grab the results out
d2_flokt <- run_flockture_bin(D2, K = 2)
d2_outs <- slurp_flockture_dumpola()


# summarize by plateaus and add that to our output
d2_outs_p <- plateau_summarize(d2_outs)


# now, draw lines of log_prob against iterations for the 
# 100 reps, and color them according to plateau length
xx <- d2_outs_p$log_probs[d2_outs_p$log_probs$plat.len > 1, ]
ggplot(xx, aes(x = iter, y = log.prob, group = rep, color = factor(plat.len))) + 
  geom_line()


# here is a vector of the different plateau lengths in sorted order:
sapply(d2_outs_p$plateau_list, length)
