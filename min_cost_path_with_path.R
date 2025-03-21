# Code for Dynamic Programming MinCostPath problem [with path]

MinCostPath <- function(A) {
  rows <- nrow(A)
  cols <- ncol(A)
  solution <- matrix(0, nrow = rows, ncol = cols)
  path <- matrix(0, nrow = rows, ncol = cols)

  solution[1, 1] <- A[1, 1]
  path[1, 1] <- 0  

  for (i in 2:cols) {
    solution[1, i] <- A[1, i] + solution[1, i - 1]
    path[1, i] <- 1 
  }

  for (i in 2:rows) {
    solution[i, 1] <- A[i, 1] + solution[i - 1, 1]
    path[i, 1] <- 2  
  }

  for (i in 2:rows) {
    for (j in 2:cols) {
      if (solution[i - 1, j] < solution[i, j - 1]) {
        solution[i, j] <- A[i, j] + solution[i - 1, j]
        path[i, j] <- 2  
      } else {
        solution[i, j] <- A[i, j] + solution[i, j - 1]
        path[i, j] <- 1  
      }
    }
  }

  i <- rows
  j <- cols
  path_values <- c(A[i, j])

  while (i > 1 || j > 1) {
    if (path[i, j] == 1) {
      j <- j - 1  
    } else {
      i <- i - 1  
    }
    path_values <- c(path_values, A[i, j])
  }

  path_values <- rev(path_values)

  return(list(cost = solution[rows, cols], path = path_values))
}

A <- matrix(c(9, 4, 9, 9,
              6, 7, 6, 4,
              8, 3, 3, 7,
              7, 4, 9, 10), nrow = 4, ncol = 4, byrow = TRUE)

result <- MinCostPath(A)
cat("Total Minimum Cost MCP:", result$cost, "\n")
cat("Path:", result$path, "\n")

