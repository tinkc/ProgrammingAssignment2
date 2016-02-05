## The makeCacheMatrix first sets the matrix, then gets the matrix to be
## inversed using the solve function and gets the inversed matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y = matrix()) {
      x <<- y
      m <<- NULL
    }
  
    get <- function() x
    setInv <- function(solve) m <<- solve(x) %*% x
    getInv <- function() m
    list(set = set, get = get,
        setInv = setInv,
        getInv = getInv)
}


## The cacheSolve function first finds if the matrix has been inversed. 
## If the inversed matrix exists, the function returns the cached inversed matrix.
## Otherwise, the cacheSolve function inverses the new matrix and returns the 
## inversed matrix.

cacheSolve <- function(x, ...) {
    m <- x$getInv()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInv(m)
    m
    ## Return a matrix that is the inverse of 'x'
}
