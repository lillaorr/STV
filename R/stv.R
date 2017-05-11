stv <- function(x, seats = 1, file = "", surplusMethod = "Cambridge", quotaMethod = "Droop") {

  # Trap wrong names Cambridge and Droop
  if(!surplusMethod == "Cambridge") stop("Please set surplusMethod = Cambridge. This is the only method currently supported.")
  if(!quotaMethod == "Droop") stop("Please set quotaMethod = Droop. This is the only method currently supported.")

  junk <- validateBallots(x)

  if(seats > ncol(x)) stop("Number of seats must be less than or equal to the number of candidates.") #Warning instead?

  # Initialize various things:
  elim <- c()                                  # Store names of eliminated candidates
  elect <- c()                                 # Store names of elected candidates
  included.ballots <- rep(TRUE, nrow(x))       # Ballots included with surplus redistribution
  unfilled <- seats                            # Vacant seats at a given round
  Nround <- 0                                  # Current number of round

  res <- data.frame(matrix(NA, ncol = 9 + ncol(x), nrow = 0))
  names(res) <- c("ballots", "seats.to.fill", "quota", "max.vote.count", "min.vote.count",
                           "elim.cand", "tied.for.elim", "elect.cand", "surplus", names(x))

  # Iteratively elect/eliminate candidates till all seats are filled:
  while (unfilled > 0) {
    Nround <- Nround + 1
    res[Nround, ] <- rep(NA, ncol(res))
    res$seats.to.fill[Nround] <- unfilled
    curr.candidates <- setdiff(names(x), c(elim, elect))

    # Check if #remaining candidates already equal #unfilled seats:
    #--- CODING CHECK ---: In validateBallots() add the case where after cleaning, valid ncols < seats (i.e. will have unfilled seats)
    if (unfilled == length(curr.candidates)) {
      elect <- c(elect, curr.candidates)
      res$elect.cand[Nround] <- paste(curr.candidates, collapse = "; ")
      res$seats.to.fill[Nround] <- 0
      res[Nround, elect] <- "Elected"
      res[Nround, elim] <- "Eliminated"
      res$quota[Nround] <- "Won by elim"

      if (file != "") {
        if (substr(file, nchar(file)-3, nchar(file)) != ".csv") warning("File name provided does not include .csv extention")
        write.table(res, file = file, sep = ",", row.names = FALSE)
      }

      return(list("elected" = elect, "detailed.info" = res))
    }

    # For remaining candidates, get valid ballots (1. not with all NA entries 2. excluded by surplus)
    curr.ballots <- (rowSums(!is.na(x[ ,curr.candidates])) > 0) & included.ballots
    ballot.size <- sum(curr.ballots)
    res$ballots[Nround] <- ballot.size

    # Calculate Quota: Manually add 1 instead of using "ceiling()" to address whole numbers
    if (quotaMethod == "Droop") quota <- floor(ballot.size/(unfilled + 1)) + 1
    res$quota[Nround] <- quota

    # Get top choice for each valid ballot then tabulate it (i.e. get vote count for each candidate):
    # ---NOTE--- which.min() function gives first position with minimum value
    top.choice <- apply(x[ ,curr.candidates], 1, function(i.row) names(x[ ,curr.candidates])[which.min(i.row)]) # try using curr.candidates instead of names(x[ ,curr.cand])
    top.choice[!curr.ballots] <- NA
    vote.counts <- table(factor(top.choice, levels = curr.candidates)) # try: table(top.choice)

    res[Nround, names(vote.counts)] <- vote.counts
    res$max.vote.count[Nround] <- max(vote.counts)
    res$min.vote.count[Nround] <- min(vote.counts)
    full.results[Nround, elect] <- "Elected"
    full.results[Nround, elim] <- "Eliminated"

    # Check for elected candidate(s): If quota crossed, surplus distribution by Cambridge method.
    #   else: eliminate a candidate and redistribute votes
    if (any(vote.counts >= quota)) {
      curr.elected <- vote.counts[vote.counts >= quota]
      elect <- c(elect, names(curr.elected))
      unfilled <- seats - length(elect)
      full.results$elect.cand[Nround] <- paste(names(curr.elected), collapse = "; ")
      full.results$surplus[Nround] <- paste(curr.elected - quota, collapse = "; ")

      # Redistribute surplus votes (Cambridge, MA method)
      for (i in names(curr.elected)) {
        cand.ballots <- which(top.choice == i)
        if (surplusMethod == "Cambridge") {
          included.ballots[sample(cand.ballots, quota)] <- FALSE
        }
      } # CLOSE surplus allocation
    } else {
      curr.elim <- names(which(vote.counts == min(vote.counts)))
      if (length(curr.elim) > 1) full.results$tied.for.elim[Nround] <- paste("Yes: ", length(curr.elim), sep = "")
      elim <- c(elim, sample(curr.elim, 1))
      full.results$elim.cand[Nround] <- tail(elim, 1)
    } # CLOSE elim/elect if-else function

  } # CLOSE while-loop

  if (file != "") {
    if (substr(file, nchar(file)-3, nchar(file)) != ".csv") warning("File name provided does not include .csv extention")
    write.table(full.results, file = file, sep = ",", row.names = FALSE)
  }

  return(list("elected" = elect, "detailed.info" = full.results))
}
