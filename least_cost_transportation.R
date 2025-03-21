# Code for Least Cost Transportation problem

leastCost <- function(costMatrix, supply, demand) {
  numSources <- nrow(costMatrix)
  numDestinations <- ncol(costMatrix)
  allocations <- matrix(0, nrow = numSources, ncol = numDestinations)
  while (sum(supply) > 0 && sum(demand) > 0) {
    minCostCell <- which(costMatrix == min(costMatrix), arr.ind = TRUE)
    row <- minCostCell[1, 1]
    col <- minCostCell[1, 2]
    quantity <- min(supply[row], demand[col])
    allocations[row, col] <- quantity
    supply[row] <- supply[row] - quantity
    demand[col] <- demand[col] - quantity
    costMatrix[row, col] <- Inf
  }
  return(allocations)
}
costMatrix <- matrix(c(3,1, 7, 4, 2, 6, 5, 9, 8,3,3,2), nrow = 3, ncol = 4, byrow = TRUE)
supply <- c(300, 400, 500)
demand <- c(250, 350, 400, 200)
result <- leastCost(costMatrix, supply, demand)

cat("Allocations matrix:\n")
print(result)

cat("\nTotal Cost LCT:", sum(result * costMatrix), "\n")
