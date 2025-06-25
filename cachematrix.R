## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL  # Initialize inverse as NULL

    # Set a new matrix and reset cached inverse
    set <- function(new_matrix) {
        x <<- new_matrix         # Use <<- to assign in parent environment
        inverse <<- NULL         # Reset cached inverse
    }

    # Get the current matrix
    get <- function() return(x)

    # Set the cached inverse
    setinv <- function(inv) inverse <<- inv

    # Get the cached inverse
    getinv <- function() return(inverse)

    # Return a list of the four functions
    return(list(set = set, 
                get = get,
                setinv = setinv, 
                getinv = getinv))
}




## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    inverse <- x$getinv()  # Check for a cached inverse

    if (!is.null(inverse)) {
        message("Getting cached data...")  # Inform user that cached data is used
        return(inverse)                    # Return cached inverse
    }

    data <- x$get()              # Retrieve the matrix
    inverse <- solve(data, ...) # Compute the inverse
    x$setinv(inverse)           # Cache the computed inverse

    return(inverse)             # Return the new inverse
}
