# Code for Hungarian Algorithm Assignment problem

hungarianAlgorithm <- function(costMatrix) {
  rowMin <- apply(costMatrix, 1, min)
  costMatrix <- costMatrix - matrix(rowMin, nrow = nrow(costMatrix), ncol = ncol(costMatrix), byrow = TRUE)
  colMin <- apply(costMatrix, 2, min)
  costMatrix <- costMatrix - matrix(colMin, nrow = nrow(costMatrix), ncol = ncol(costMatrix), byrow = TRUE)
  lines <- coverZeros(costMatrix)
  while (length(lines) < nrow(costMatrix)) {
    minUncovered <- min(costMatrix[!rowSaturated(lines), !colSaturated(lines)])
    costMatrix[!rowSaturated(lines), !colSaturated(lines)] <- costMatrix[!rowSaturated(lines), !colSaturated(lines)] - minUncovered
    costMatrix[rowSaturated(lines), colSaturated(lines)] <- costMatrix[rowSaturated(lines), colSaturated(lines)] + minUncovered
    lines <- coverZeros(costMatrix)
  }
  assignments <- matrix(0, nrow = nrow(costMatrix), ncol = ncol(costMatrix))
  for (i in 1:nrow(costMatrix)) {
    assignments[i, which(costMatrix[i, ] == 0)] <- 1
  }
  return(assignments)
}
coverZeros <- function(costMatrix) {
  rowCovered <- rep(FALSE, nrow(costMatrix))
  colCovered <- rep(FALSE, ncol(costMatrix))
  while (TRUE) {
    zeroPositions <- which(costMatrix == 0 & !rowCovered, arr.ind = TRUE)
    if (length(zeroPositions) == 0) break
    rowCovered[zeroPositions[, 1]] <- TRUE
    colCovered[zeroPositions[, 2]] <- TRUE
  }
  lines <- c(rowCovered, !colCovered)
  return(lines)
}
rowSaturated <- function(lines) {
  return(lines[1:nrow(costMatrix)])
}
colSaturated <- function(lines) {
  return(lines[(nrow(costMatrix) + 1):length(lines)])
}
costMatrix <- matrix(c(9, 2, 7, 8,
                       6, 4, 3, 7,
                       5, 8, 1, 8,
                       7, 6, 9, 4), nrow = 4, ncol = 4, byrow = TRUE)

result <- hungarianAlgorithm(costMatrix)

cat("Optimal Assignments:\n")
print(result)

cat("\nTotal Cost HA:", sum(costMatrix * result), "\n")
