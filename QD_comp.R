#### Flockture #### 
## Here are some fun things we can do during
source("R/flockture.R")
## developemnt of this thing.
library(ggplot2)


# read in the 4000 individuals at Fst = 0.03
D5 <- read.table("data/small_data.txt", row.names = 1)

# take just the first two pops
D2 <- D5[ grep("^Pop_[12]", rownames(D5)), ]

# define some starting conditions if you want:
sc <- c(c(rep(1,times=29),rep(2,times=21))# vector of all the starting condtions 
        #c(rep(2,times=14),rep(1,times=15),rep(2,times=10),rep(1,times=11))
        
)

# run flockture on it and grab the results out
d2_flokt <- run_flockture_bin(D2, K = 2, start_config = sc,iter=20,reps=1)
d2_outs <- slurp_flockture_dumpola()


# summarize by plateaus and add that to our output
d2_outs_p <- plateau_summarize(d2_outs)


# now, draw lines of log_prob against iterations for the 
# 100 reps, and color them according to plateau length
xx <- d2_outs_p$log_probs[d2_outs_p$log_probs$plat.len >= 1, ]
ggplot(xx, aes(x = iter, y = log.prob, group = rep, color = factor(plat.len))) + 
  geom_line() +
  scale_color_discrete(name="Plateau Length")


# here is a vector of the different plateau lengths in sorted order:
sapply(d2_outs_p$plateau_list, length)

######### lets compare some runs 
require("XLConnect")
options(java.parameters = "-Xmx4g" )
# import flock results
wd<-getwd()
filename<-'Pop1Ref1_Pop2Ref2.xls'
excel.file <- file.path(wd,'FlockResults_smalldata',filename)
assign(x="FLOCKres",value=(readWorksheetFromFile(excel.file, sheet="All specs alloc and likelihoods", header=TRUE)))
FLOCKres<-FLOCKres[-(1),]#get rid of that pesky header that is imported
###FLOCK gives ref1 or ref2 in a column, alternatively we could just round the likelihoods... let's do that,but will be tricky with k>2
Fk.like<-ifelse(FLOCKres[,10]>0.5,1,2)
Fk.like==d2_outs$indivs[,4]
