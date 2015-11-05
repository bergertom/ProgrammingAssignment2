## This R function is able to cache potentially time-consuming computations. For example, taking the mean of a 
## numeric vector is typically a fast operation. However, for a very long vector, it may take too long to compute 
## the mean, especially if it has to be computed repeatedly (e.g. in a loop). If the contents of a vector are 
## not changing, it may make sense to cache the value of the mean so that when we need it again, it can be looked 
## up in the cache rather than recomputed.

## This function creates a special "matrix" object (m) that can cache its inverse.
## Note: this function uses inv(); with the result that it only supports 2x2 matices
makeCacheMatrix <- function(x = matrix()) {
  inverseMatrix <- NULL
  # 1. set the value of the matrix
  set <- function(y) {
    x <<- y
    # use `<<-` to assign a value to an object in different environment
    inverseMatrix <<- NULL
  }
  # 2. get the value of the matrix
  get <- function()
    x
  # 3. set the value of inverse of the matrix
  setInverse <- function(inv_parameter)
    inverseMatrix <<- inv_parameter
  # 4. get the value of inverse of the matrix
  getInverse <- function()
    inverseMatrix
  list(
    set = set, get = get,
    setInverse = setInverse,
    getInverse = getInverse
  )
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve 
## should retrieve the inverse from the cache.
# return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  inverseMatrix <- x$getInverse()
  if (!is.null(inverseMatrix)) {
    # 1. if the cache exists, return it
    message("return cached reverse")
    return(inverseMatrix)
  }
  # 2. if the cache is emty; create a new reverse
  inverseMatrix <- solve(x$get())
  x$setInverse(inverseMatrix)
  message("set a new cached reverse")

  return(inverseMatrix);
}

#setwd("~/GitHub/ProgrammingAssignment2")
#source("cachematrix.R")
#y = rbind(c(1, 2), c(4 ,5))
#y
#[,1] [,2]
#[1,]    1    2
#[2,]    4    5
#> m = makeCacheMatrix(y)
#> cacheSolve(m)
#set a new cached reverse
#[,1]       [,2]
#[1,] -1.666667  0.6666667
#[2,]  1.333333 -0.3333333
#> cacheSolve(m)
#return cached reverse
#[,1]       [,2]
#[1,] -1.666667  0.6666667
#[2,]  1.333333 -0.3333333
 
