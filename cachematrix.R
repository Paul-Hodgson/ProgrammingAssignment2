## the following function either sets or gets the value of the vector 
## and sets or gets the inverse
## the correct syntax for use is cacheSolve(makeCacheMatrix(x))

makeCacheMatrix <- function(x = matrix()) {
  f <- NULL  ##sets the initial value of the flag f 
  ## (which governs whether to use the cached value or calculate it)
  
  set <- function(A) {
    x <<- A
    f <<- NULL
  }
  get <- function() x
  setinv <- function(solve) f <<- solve ##create the inverse of a special vector 
  getinv <- function() f
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## the following function calculates the inverse of the matrix, 
## using a cached value if it exists

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  f <- x$getinv()   
  if(!is.null(f)) { ## check to see if the inverse has already been calculated
    message("getting cached data")
    return(f)
  }
  data <- x$get()
  f <- solve(data, ...)
  x$setinv(f) ## perform the inverse calculation
  f
}
