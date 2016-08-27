## Dear Classmate:
## This function invokes the same principals as the example function in the readme, 
##but uses "solve" instead of "mean" as the preferred function
##


## you need a square matrix like these examples: 
##big_sq <- matrix(rnorm(1e6), nrow=1e3,ncol=1e3)
##sm_sq <- matrix(1:8, nrow=2, ncol=2)
##note if your matrix is not inversible, you will get a variation on this error
##when you run cache solve
##Error in solve.default(data, ...) : 
##  Lapack routine dgesv: system is exactly singular: U[5,5] = 0 

## you need to feed your square matrix into the function makeCacheMatrix, like this:
## mcm <- makeCacheMatrix(big_sq)
## feeding your matrix into makeCacheMatrix returns a list with the cached "solve"


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
  
}


## this function checks if you've got the inverse of a particular matrix cached.
## if you don't have your matrix cached it will call solve on your matrix to return the inverse.
## I have added a try catch to return a more helpful error message when you call
## this function on a non-square matrix. 

## you need to call cacheSolve on the list object you created above, like this
## cacheSolve(mcm)

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  out <- tryCatch({
    m <- solve(data, ...)
  }, warning = function(w){
    print(paste("Warning: ", w))
  }, error = function(e){
    print(paste("I'm sorry but your matrix is not square, and therefore is not inversible. Please call cacheSolve with a square matrix. ex. sm_sq <- matrix(1:8, nrow=2, ncol=2). The exact r error is: ", e))
    m <- NULL
  }, finally = {
    
  })
  
  x$setsolve(m)
  m
}
