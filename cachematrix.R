## https://github.com/Whirligigchen/ProgrammingAssignment2-
## R programming assignment 2: lexical scoping- catching the inverse of a matrix

## The make CacheMatrix creates a special "matrix"
##which is really a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse of the matrix
## get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
   m <- NULL
   set <- function(y){
     x <<- y
     m <<- NULL
   }
   get <-function() x
   setinverse <- function(solve) m <<- solve
   getinverse <- function()  m
   list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
   
}

## The cacheSolve function calculates the inverse of the special "matrix"
## However, it first checks to see if the inverse has already been calculated.
## If so, it gets the inverse from the cach and skips the computation.
## Otherwise, it calculates the inverse of the data and sets the value of the inverse
## in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
  m<- x$getinverse()
  if(!is.null(m)){
          message("getting cached data")
          return(m)
  }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
