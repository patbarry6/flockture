

#' given a simple 2-column format data frame with rownames, run flockture on it
#' 
#' Boing
#' 
run_flockture_bin <- function(data, 
                  flockture_bin = "./src/flockture",
                  iter = 50,
                  reps = 100,
                  K = 2,
                  mptemp = "./inputs/mainparams_template",
                  eptemp = "./inputs/extraparams_template"
                  ) {
  
  # first write data to an input file
  data[is.na(data)] <- -9 
  write.table(data, file = "tmp_flock_input.txt", quote = F, sep = "  ", row.names = TRUE, col.names = FALSE)
  
  # make a mainparams and extra params file:
  tmp <- readLines(mptemp)
  tmp <- gsub("xxSET_BURN_IN_HERExx", replacement = iter, tmp)
  tmp <- gsub("xxSET_NUMREPS_HERExx", replacement = iter * (reps - 1), tmp)
  cat(tmp, file = "tmp_mainparams", sep = "\n")
  
  file.copy(eptemp, "tmp_extraparams", overwrite = TRUE)
  
  
  # make a command line to run flockture
  commline <- paste(flockture_bin,
                    "-m tmp_mainparams",
                    "-e tmp_extraparams",
                    "-K",  K,
                    "-L",  ncol(data)/2,
                    "-N", nrow(data),
                    "-i tmp_flock_input.txt > tmp_flockture_dumpola",
                    sep = " ")
  
  
  # do the call.  The output goes to a file called tmp_flockture_dumpola
  system(commline)
  
}


#' slurp flockture output from the dumpola file
#' 
#' just reads the dumpola file and puts things into a nice list
slurp_flockture_dumpola <- function(file = "tmp_flockture_dumpola") {
  
  # read all the initial lines
  x <- readLines(file)
  
  # get the individual Q values for each rep
  indlines <- x[grepl("^FLOCKTURE_INDIV:", x)]
  ncols <- length(strsplit(indlines[1], split = "  *")[[1]])
  tmp <- matrix(unlist(strsplit(indlines, split = "  *")), ncol = ncols, byrow = TRUE)[, -1]
  indivs <- as.data.frame(tmp, stringsAsFactors = FALSE)
  names(indivs) <- c("rep", "indiv", "label", "pickedpop", paste("q", 1:(ncols-5), sep="."))
  # now we want to coerce everything but the label colun to be numeric
  indivs[, names(indivs) != "label"] <- as.numeric(unlist(indivs[, names(indivs) != "label"]))
  
  # get the log probs for each of those
  lplines <- x[grepl("^FLOCK_LOG_PROB:", x)]
  tmp <- matrix(unlist(strsplit(lplines, split = "  *")), ncol = 4, byrow = TRUE)[, -1]
  log_probs <- as.data.frame(matrix(as.numeric(tmp), ncol = 3))
  names(log_probs) <- c("rep", "iter", "log.prob")
  
  
  # finally, get a list of the final partitions
  parts <- x[grepl("^FLOCKTURE_CLUSTER_STRING:", x)]
  tmp <- matrix(unlist(strsplit(parts, split = "  *")), ncol = 5, byrow = TRUE)[, -1]
  tmp <- as.data.frame(tmp, stringsAsFactors = FALSE)
  names(tmp) <- c("rep", "iter", "cluster", "members")
  tmp[ , names(tmp) != "members"] <- as.numeric(unlist(tmp[ , names(tmp) != "members"]))
  # now turn it all into a big list where the member strings are sorted,
  # which makes it easy to name certain identical partitions
  tmp <- split(tmp, tmp$rep)
  prtn_list <- lapply(tmp, function(x) x[,4])
  
  list(indivs = indivs, log_probs = log_probs, prtn_list = prtn_list)
  
}


#' given a list W like what comes out of slurp_flockture_dumpola
#' this function summarizes the output according to which solutions
#' were found multiple times
plateau_summarize <- function(W) {
  member_names <- unname(sapply(W$prtn_list, function(x) paste(sort(x), collapse = "-------")))
  plats <- plyr::count(member_names)
  plats <- plats[order(plats$freq, decreasing = TRUE), ]
  
  # get a list of which reps are in each of those
  reps_in_plats <- lapply(plats$x, function(y) which(member_names == y))
  
  # make a named vector to put plateau ID # and plateau length into the data frames.
  # there must be a cleaner way of doing this
  plat_ids <- do.call(what = c,
          args = lapply(1:length(reps_in_plats), function(x) {
            y <- rep(x, length(reps_in_plats[[x]])); 
            names(y) <- reps_in_plats[[x]]; 
            y}))
  plat_lens <- sapply(reps_in_plats, length)[plat_ids]
  names(plat_lens) <- names(plat_ids)
  
  # here we put those in as new columns in the data frame W, which we will return
  W$indivs$plat.id <- plat_ids[as.character(W$indivs$rep)]
  W$indivs$plat.len <- plat_lens[as.character(W$indivs$rep)]
  
  W$log_probs$plat.id <- plat_ids[as.character(W$log_probs$rep)]
  W$log_probs$plat.len <- plat_lens[as.character(W$log_probs$rep)]
  
  W$plateau_list <- reps_in_plats
  
  
  W
}


# prtn_list <- unlist(lapply(tmp, function(x) paste(sort(x[,4]), collapse = "--------")))