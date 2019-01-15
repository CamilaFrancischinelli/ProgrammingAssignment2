# This function sets the parameters of the list that will be created and
# the variables that will be called in a later stage to calculate the 
# inverse of the given marix

makeCacheMatrix <- function(x = matrix()) {
       inv <- NULL                            # sets the inverse to NULL before starting the calculations
       set <- function(y) {
              x <<- y
              inv <<- NULL                    # sets the inverse to NULL before starting the calculations
       }
       get <- function() x
       setsolve <- function() inv <<- solve(x) # calculates the inverse of matrix x
       getsolve <- function() inv
       list(set = set, get = get,
            setsolve = setsolve,
            getsolve = getsolve)               
}


# This is the function where the inverse of matrix x is calculated

cacheSolve <- function(x, ...) {
       inv <- x$getsolve()
       if(!is.null(inv)) {
              message("getting cached data")
              return(inv)
       }
       data <- x$get()
       inv <- solve(data, ...) # the inverse of matrix x is stored in inv 
       x$setsolve(inv)
       inv
}