# Tiamzon, Edgar Alan Emmanuel III B.
# Exercise 1 - Introduction to R 
# Description: Getting the frequency of a given vector in matrix format
# AB3L
# 08 - 28 - 24

# counts each element present in a given vector
CountElement <- function(elementPresent, vector){
  
  count <- 0L
  
  for (element in vector){
    if(element == elementPresent){
      count == count + 1L
    }
  }
  count # return the number of elements
}

# finds the number of element occurrences in a given vector
# then concatenates these elements in a list
SameElement <- function(addTovector, OrigVector){ # function(SearchElement, OrigVector)
  
  OccurElement <- c() # for listing of the same element
  
  for (element in OrigVector){ 
    OccurElement <- c(OccurElement, CountElement(element, addTovector)) # concatenation of elements 
    # OccurElement <- c(OccurElement(CountElement(elementPresent, vector)))
  }
  
  OccurElement # return the list
}

# this adds all the same element in a new set of vector
# call the SameElement function to adds its list to NewSetofVector
NewSetOfVector <- function(ForNewVector){
  
  SampleVector <- c() # list which accepts all the same element
  
  for (element in ForNewVector){
    SampleVector <- c(SampleVector, SameElement(element, ForNewVector))
  }
  
  SampleVector
}

createMatrix <- function(charVector, freqVector){
  
  # get the length of column based on a given element
  ColLength = length(freqVector)

  forMatrix <- matrix(
    c(charVector, freqVector),
    nrow = ColLength,
    ncol = 2,
    byrow = FALSE,
    dimnames <- list(1:ColLength, c("Characters", "Frequency"))
  )
  
  forMatrix
}

# calling all the functions to create frequency matrix
FreqMatrix <- function(sampleVector){

  # getting the occurrences of each elements for charVector
  charVector <- unique(sampleVector)
    
  # getting the vector for frequencies for freqVector
  freqVector <- NewSetOfVector(charVector)
  
  # creating and returning the frequency matrix
  createMatrix(charVector, freqVector)
}

sample <- c('a', 'b', 'a', 'c', 'b', 'a', 'd')

# execute and print the matrix
executeMatrix <- FreqMatrix(sample)
print(executeMatrix)

