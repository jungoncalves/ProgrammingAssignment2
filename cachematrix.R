############ Coursera - R Programming - week 3  ############ 
############ Programming Assignment 2: Lexical Scoping ############ 

## packages
#install.packages("matlib")
library(matlib)

## Function makEcacheMatrix() creates a special "matrix" object that can cache its inverse

##As explaneid in the example given, in 5 steps:

#1. check if the matrix is invertible
#2. set the value of the matrix
#3. get the value of the matrix
#4. set the value of the inverse matrix
#5. get the value of the inverse matrix


makeCacheMatrix <- function(x = matrix()) {
  if (det(x) != 0){
    m <- NULL
    set <- function(y)
    {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setInverse <- function(i) m <<- inv(x)
    getInverse <- function() m
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
  }
  
  else{
    print("Matrix not invertible and square")
  }
}


## Function cacheSolve() computes the inverse of the special "matrix" returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
    m <- x$getInverse()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m
}

