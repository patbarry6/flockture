library(ggplot2)

source("R/flockture.R")

x <- read.table("../flock-comment/inst/data_files/sib_yanked_001.txt", row.names = 1)

# change 0 to -9 for missing data.
x[ x == 0 ] <- -9

# this throws a malloc error.  Crap!
catch <- run_flockture_bin(x, K = 5, iter = 100, reps = 30)

out <- slurp_flockture_dumpola() 
psum <- plateau_summarize(out)

ggplot(psum$log_probs, aes(x = iter, y = log.prob, group = rep, color = factor(plat.len))) + 
  geom_line() +
  scale_color_discrete(name="Plateau Length")