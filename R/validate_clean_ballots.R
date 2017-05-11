validateBallots <- function(x) {

  # 1. Allowed Classes: matrix or data.frame
  if (!(class(x) %in% c("data.frame", "matrix"))) stop("\nPlease enter object of class either data frame or matrix.")

  # 1a. Convert matrix input to data.frame and proceed with testing (should warn? too much detail?)
  if (class(x) == "matrix") x <- as.data.frame(x)

  # 2. Check sanity of column names:
  if (any(is.na(names(x)))) stop("\nPlease provide each candidate's name/identifier as column names.")

  if (length(unique(names(x))) != ncol(x)) stop("\nPlease provide unique column names.")

  # 3. Check if x is numeric:
  if (any(!sapply(x, is.numeric))) {
    print(paste("Column(s):",
                paste(which(!sapply(x, is.numeric)), collapse = ", "),
                "contain non-numeric data."))
    stop("\nPlease provide numeric input.")
  }

  # 4. Check for blank cols: (A candidate w/o any vote, including his own!)
  if (any(colSums(is.na(x)) == nrow(x))) {
    print(paste("Column(s):",
                paste(which(colSums(is.na(x)) == nrow(x)), collapse = ","),
                "do not contain any ranks."))
    stop("\nPlease remove column(s) for candidate(s) not ranked, or use cleanBallots()")
  }

  # 5. Check for blank rows and rows w/ non-sequencial ranks
  if (any(rowSums(is.na(x)) == ncol(x))) {
    print(paste("Row(s):",
                paste(which(rowSums(is.na(x)) == ncol(x)), collapse = ", "),
                "do not contain any ranks."))
    stop("\nPlease remove blank row(s), or use cleanBallots()")
  }

  invalid <- rep(FALSE, nrow(x))
  for (i in 1:nrow(x)) {
    if (!identical(as.numeric(sort(x[i,])), as.numeric(1:max(x[i,], na.rm = TRUE)))) {
      invalid[i] <- TRUE
    }
  }

  if (any(invalid)) {
    print(paste("Row(s):",
                paste(which(invalid), collapse = ", "),
                "contain non-sequencial (missing or duplicated) ranks."))
    stop("\nPlease remove row(s) with non-sequencial ranks, or use cleanBallots()")
  }

  return("All tests passed. Please feel free to run STV() function.")
}


cleanBallots <- function(x, cand.names = NA) {

  # 1. Check if input: matrix or data.frame, convert matrix into data.frame
  if (!(class(x) %in% c("data.frame", "matrix"))) stop("\nPlease enter object of class either data frame or matrix.")
  if (class(x) == "matrix") x <- as.data.frame(x, stringsAsFactors = FALSE)

  # 2. Check if x is numeric:
  cols.non.numeric <- !sapply(x, is.numeric)
  if (any(cols.non.numeric)) {
    for (i in which(cols.non.numeric)) {
      if (is.factor(x[,i])) x[,i] <- as.character(x[,i])
      temp <- x[!is.na(x[,i]), i]
      if (anyNA(suppressWarnings(as.numeric(temp)))) {
        stop(paste("\nPlease check data type in column", i))
      }
      x[,i] <- as.numeric(x[,i])
    }
  }

  # 3. Provide column names:
  if (!is.na(cand.names)) {
    if (length(cand.names) != ncol(x)) stop ("Please provide exactly one candidate name for each column.")
    names(x) <- cand.names
  }

  # 4. Remove blank cols:
  x <- x[, colSums(!is.na(x)) > 0]

  # 5. Remove blank and/or non-sequentially ranked rows:
  x <- x[rowSums(is.na(x)) != ncol(x), ]

  correct.ranking <- rep(FALSE, nrow(x))
  for(i in 1:nrow(x)) {
    correct.ranking[i] <- identical(as.numeric(sort(x[i,])), as.numeric(1:max(x[i,], na.rm = TRUE)))
  }
  x <- x[correct.ranking, ]

  if (class(try(validateBallots(x))) == "try-error") warning("Validation failed the validateBallots() check")
  return(x)
}
