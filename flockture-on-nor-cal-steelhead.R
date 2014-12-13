

source("R/flockture.R")

x <- read.table("../flock-comment/inst/data_files/sib_yanked_001.txt", row.names = 1)

# change 0 to -9 for missing data.
x[ x == 0 ] <- -9

# this throws a malloc error.  Crap!
catch <- run_flockture_bin(x, K = 2, iter = 10, reps = 10)



x <- read.table("data/struct_input_1.txt", row.names = 1)
catch <- run_flockture_bin(x, K = 2, iter = 20, reps = 5)
