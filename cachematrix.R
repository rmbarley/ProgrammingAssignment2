## In order to more efficiently compute expensive matrix inversions,
## this module allows caching the result of solve(X).
##
## Calling:
## > X <- matrix(c(4, 7, 2, 6), nrow = 2, ncol = 2)
## > Y <- makeCacheMatrix(X)
## > cacheSolve(Y)
## [,1] [,2]
## [1,]  0.6 -0.2
## [2,] -0.7  0.4

## makeCacheMatrix manages cached matrices and memoized solutions
## args:
##   x: A a square invertible matrix. Defaults to an empty matrix
## Returns:
##   set (inverse: matrix) -> void: Sets new matrix, clears cached inverted matrix
##   get () -> matrix: Retrieves currently cached matrix
##   setinverse (inverse: matrix) -> void: Stores memoized inverted matrix
##   getinverse () -> matrix: Retrieves currently stored solution
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, 
       get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
}


## cacheSolve inverts a square matrix. If there is an available cached matrix/solution
## it returns that before attempting any computation.
## args:
##   x: A a square invertible matrix
## Returns:
##   m: matrix: The solution to solve(m)
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if (!is.null(m)) {
          message("getting cached solution...")
          return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
