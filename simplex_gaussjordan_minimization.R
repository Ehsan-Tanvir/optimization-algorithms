# Code for Gauss Jordan Algorithm Simplex Minimization problem
# Create a matrix with specific values
M<-100
rows<-4
columns<-8
noofdec <-2
Matrix <- matrix (c(1,-4,-1, 0, -M,-M, 0, 0,
                    0, 3, 1, 0, 1,  0, 0, 3,
                    0, 4, 3,-1, 0,  1, 0, 6,
                    0, 1, 2, 0, 0,  0, 1, 4),
                  nrow = rows, ncol = columns, byrow = TRUE)

decvars <- abs(Matrix[1, 2:(noofdec + 1)])
z <- Matrix[1,]
z <- z +( M*Matrix[2,] +  M*Matrix[3,])


#########################################################################
Matrix[1,]<- z
print(Matrix)
iteration <- 0
cat("ITERATION:", iteration, "\n")
print(Matrix)
while(any(Matrix[1, 2:(ncol(Matrix)-1)] > 0))
{
  #1 For pivot column
  #minvalue <- max(Matrix[1,2:columns-1])
  maxvalue <- max(Matrix[1, 2:(ncol(Matrix)-1)])
  if (length(maxvalue)>1)
  {
      maxvalue <- maxvalue[1]
  }
  columnindex <- which(Matrix[1, ] == maxvalue)
  pivotcolumn <- Matrix[,columnindex]
  
  #2 For pivot row
  # Ratio Test Apply
  tempcolumn <- Matrix[-1,columns] / pivotcolumn[-1]
  RowIndex <- which(tempcolumn == min(tempcolumn[tempcolumn > 0])) + 1
  if (length(RowIndex)>1)
  {
      RowIndex <- RowIndex[1]
  }
  pivotrow <- Matrix[RowIndex,]
  pivotElement <- Matrix[RowIndex, columnindex]
  
  #Update
  newpivotrow <- pivotrow / pivotElement
  z <- Matrix[1, columns]
  NewMatrix <- matrix (0,nrow = rows, ncol = columns, byrow = TRUE)
  NewMatrix[RowIndex,] <- newpivotrow
  for (i in 1:rows) {
    if (i != RowIndex) {
      NewMatrix[i,] <- Matrix[i, ] - Matrix[i,columnindex] * newpivotrow
    }
  }
  z <- z + newpivotrow[columns] * abs(maxvalue)
  iteration=iteration + 1
  Matrix<-NewMatrix
  cat("ITERATION:", iteration, "\n")
  print(Matrix)
}
print("CONCLUSION:")
while (TRUE) {
  all <- Matrix[-1, columns]
  if (length(all) < noofdec) {
    print("Decision Variables > RHS")
    break  # or take other appropriate action
  }
  tempvalues <- sample(all, noofdec)
  result_sum <- 0

  for (i in 1:noofdec) {
    ans <- decvars[i] * tempvalues[i]
    result_sum <- result_sum +  ans
  }

  if (floor(Matrix[1, columns]) == floor(result_sum)) {
    for (i in 1:noofdec) {
      ans <- decvars[i] * tempvalues[i]
      print(paste(decvars[i],"*",tempvalues[i], "=", ans))
    }
    break
  }
}
cat("z =", Matrix[1, columns], "\n")
