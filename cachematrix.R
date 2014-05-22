## R programming
## Programming Assignment 2

## A pair of functions that cache the inverse of a matrix.

## 1. makeCacheMatrix: Creates a special "matrix" object that can cache its inverse
##
## 2. cacheSolve:      Computes the inverse of the special "matrix" returned by makeCacheMatrix above. 

##                     If the inverse has already been calculated (and the matrix has not changed), 
##                     then the cachesolve should retrieve the inverse from the cache.


## -------------------------
## makeCacheMatrix function
## -------------------------

## Creates a special "matrix", which is really a list containing a function to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

# Input:  matrix 'x', ordinary matrix type 
# Output: special "matrix", list of 'set','get','setinv','getinv'

makeCacheMatrix <- function(x = matrix()) {
  # variable 'm' is firstly initialized as NULL:
  m <- NULL
  
  # function 'set' is defined as a function which takes an argument 'y'
  # it assigns 'x' this argument 'y' and assigns 'm' the value NULL
  set <- function(y) {
    # we use <<-  to assign a value to an object in an environment 
    # that is different from the current environment
    x <<- y       # 'x' is assigned the value 'y' 
    m <<- NULL    # 'm' is assigned NULL
  }
  
  # function 'get' returns 'x' (so we "get" the matrix 'x')
  get <- function() x
  
  # function 'setinv' assigns 'm' the argument of the function
  setinv <- function(solve) m <<- solve
  
  # function 'getinv' returns 'm' (so we "get" the inverse 'm')
  getinv <- function() m
  
  # list of the functions defined above: 'set', 'get', 'getinv', 'setinv'
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## --------------------
## cacheSolve function
## --------------------

## Computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve retrieves the inverse from the cache.

# Input:  matrix 'x'
#         of the type returned by makeCacheMatrix (the list of  'set','get','setinv','getinv')
# Output: inverse of the matrix 'x'
#         ordinary matrix type

cacheSolve <- function(x, ...) {
  # variable 'm' is assigned the value of 'getinv' part of 'x'
  # 'getinv' part of 'x' is
  #     either: NULL - in the case the inverse of 'x' has not been computed and cached yet
  #     or: the cached inverse matrix
  m <- x$getinv()
  
  # if 'm' is not NULL, it means that the inverse of the matrix 'x' is cached and contained in 'm'
  # in this case:  
  if(!is.null(m)) {
    message("getting cached data")   # the message is printed
    return(m)                        # the cached inverse 'm' is returned (ordinary matrix type)
  }                                       
  
  # if 'm' is NULL, it means that the inverse of the matrix 'x' has not been computed yet
  # in this case:
  data <- x$get()         # we assign the matrix 'x' to the variable data as a regular matrix type
  m <- solve(data, ...)   # we assign the inverse of 'data' to 'm' (also a ordinary matrix type)
  x$setinv(m)             # we assign this inverse as 'setinv' part of 'x', so that it gets cached
  m                       # the inverse 'm' is returned (ordinary matrix type)
}

## ----------------
## Example of use
## ----------------

## The steps are numbered and start with  ##  
## The output is simply commented with #

## 1. We define some test matrix
matrix1 <- matrix(c(1,2,3,4), nrow=2)
matrix1
# [1,]    1    3
# [2,]    2    4

## 2. We compute its inverse (so that we know what we are supposed to get)
solve(matrix1)
#      [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5

## 3. We try to use 'cacheSolve' for 'matrix1'
##    It doesn't work because 'matrix1' is not of the correct class
cacheSolve(matrix1)
# Error in x$getinv : $ operator is invalid for atomic vectors

## 4. What we need to do:
m1 <- makeCacheMatrix(matrix1)
cacheSolve(m1)
#      [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5

## 5. When running 'makeCacheMatrix' for 'm1' again,
##    the inverse is returned from cache
cacheSolve(m1)
# getting cached data
#       [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5

## 6. Note that 'matrix1' and 'm1' have different classes:
class(matrix1)
# [1] "matrix"
class(m1)
# [1] "list"

## 7. We check how much time is saved by caching the inverse
##    We generate 1000x1000 random matrix 'matrix2'
matrix2 <- matrix(rnorm(1000^2),nrow=1000)

## Each time we compute the inverse of 'matrix2'
## Test1: compute inverse using 'solve' function
## Test2: run 'cacheSolve' for the first time, so inverse gets computed as in 1.
##              and it gets cached
## Test3: run 'cacheSolve' when the inverse is cached 
##
## We measure the elapsed  time using system.time() (see Week 4 lectures)

## Test 1
system.time({
  solve(matrix2)
})
# user  system elapsed 
# 2.22    0.02    2.24 

## Test 2
system.time({
  m2 <- makeCacheMatrix(matrix2)
  cacheSolve(m2)
})
# user  system elapsed 
# 2.20    0.02    2.25

## Test3
system.time({
  cacheSolve(m2)
})
# getting cached data
# user  system elapsed 
# 0       0       0

## We see that Test3 is performed almost immediately
## while Test1 and Test2 need a couple of seconds