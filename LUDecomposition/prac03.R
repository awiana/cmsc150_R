# LU DECOMPOSITION

LUDecomposition <- function(mat){
  get_n <- nrow(mat)
  
  matforL <- diag(x = 1, nrow = get_n, ncol = get_n) # matrix for lower triangular // diag(1, get_n)
  matforU <- mat # matrix for upper triangular
  
  
  for (i in 1:(get_n-1)){
    if (matforU[i,i] == 0){
      stop("Pivot element is equal to zero. Stops the algorithm.")
    }
    
    for (j in (i + 1):get_n){
      pivot_element <- matforU[i,i]
      multiplier <- matforU[i,j] / pivot_element
      matforL[j,i] <- multiplier
      
      normalized_row <- multiplier * matforU[i,]
      matforU[j,] <- matforU[j,] - normalized_row
    }
  }
  return (list(L = matforL, U = matforU))
}

# Example
A <- matrix(c(4,3,6,3,3,2,8,2,1), nrow = 3, byrow = TRUE)
result <- LUDecomposition(A)

L <- result$L
U <- result$U

print("L matrix: ")
print(L)

print("U matrix: ")
print(U)

# ===================================================
#for dot product
dot_product <- L%%U
print(dot_product)

#other algo for dot product
manual_dot_product <- function(L, U) {
  n <- nrow(L)
  result <- matrix(0, n, n) # Initialize an empty matrix
  
  for (i in 1:n) {
    for (j in 1:n) {
      sum <- 0
      for (k in 1:n) {
        sum <- sum + L[i, k] * U[k, j]
      }
      result[i, j] <- sum
    }
  }
  return(result)
}

# Example matrix dot product
dot_product_manual <- manual_dot_product(L, U)

print("Manual Dot product (L * U):")
print(dot_product_manual)
# ===================================================

#for determinant
determinant <- prod(diag(U))
print(determinant)

gauss_elim_det <- function(A) {
  get_n <- nrow(A)
  
  for (i in 1:(get_n-1)) {
    for (j in (i+1):get_n) {
      multiplier <- A[j,i] / A[i,i]
      A[j,] <- A[j,] - multiplier * A[i,]
    }
  }
  
  return(prod(diag(A))) # Determinant is the product of diagonal elements
}

determinant_gauss <- gauss_elim_det(A)
print(paste("Determinant of A using Gaussian Elimination:", determinant_gauss))
# ===================================================


#for forward elimination
for_elim <- function(matforU){
  get_n <- nrow(matforU)
  
  determinant <- 1
  for(i in 1:get_n){
    determinant <- determinant * matforU[i,i]
  }
  
 determinant
}

B <- matrix(c(4,3,6,3,3,2,8,2,1), nrow = 3, byrow = TRUE)
result <- for_elim(B)

print(result)

#====================================================
forward_elimination_with_pivot <- function(mat){
  get_n <- nrow(mat)
  matforU <- mat # Copy of the input matrix
  
  for (i in 1:(get_n-1)) {
    # Partial pivoting: Swap rows if necessary
    max_row <- which.max(abs(matforU[i:get_n, i])) + (i-1)
    if (i != max_row) {
      temp <- matforU[i,]
      matforU[i,] <- matforU[max_row,]
      matforU[max_row,] <- temp
    }
    
    if (matforU[i,i] == 0) {
      stop("Pivot element is zero after row swap. Cannot perform forward elimination.")
    }
    
    for (j in (i + 1):get_n) {
      pivot_element <- matforU[i,i]
      multiplier <- matforU[j,i] / pivot_element
      
      # Perform row operation to eliminate below-diagonal elements
      matforU[j,] <- matforU[j,] - multiplier * matforU[i,]
    }
  }
  return(matforU)
}
