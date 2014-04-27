# Functions to create and manipulate an matrix object that can 
# cache the value of its inverse.
# Modified version of the example code informed by Fu Shen Wang's 
# forum post
# https://class.coursera.org/rprog-002/forum/thread?thread_id=696
 
# Create a list of functions that set and get a CacheMatrix object
# and its inverse
# Follows the same form as the example makeVector
# Arguments - matrix x

makeCacheMatrix <- function(x = matrix()) {
    # create a variable to hold the inverse
    cachedInverse <- NULL
    
    # Set the matrix x to the argument passed to the "set" 
    # function and clear old cached inverse
    set <- function(y) {
        x <<- y
        cachedInverse <<- NULL
    }
    
    # Return the matrix x
    get <- function() x
    
    # Search the parent environments for an existing variable 
    # "cachedInverse" and assign it the value that is passed 
    # as an argument
    setInverse <- function(inverse) cachedInverse <<- inverse
    
    # Return the value of cachedInverse
    getInverse <- function() cachedInverse
    
    # Create the CacheMatrix list object
    list(
        set = set, 
        get = get, 
        setInverse = setInverse, 
        getInverse = getInverse)
}

# Calculate and set the inverse of a CacheMatrix object
# Arguments - CacheMatrix list x
cacheSolve <- function(x, ...) {
        # Check the cache
        inverse <- x$getInverse()
        
        if(!is.null(inverse)){
            message("Retrieving cached inverse")
            return(inverse)
        }
        
        # Calculate and store the inverse if there is no value 
        # in the cache
        matrix <- x$get()
        inverse <- solve(matrix, ...)
        x$setInverse(inverse)
        
        # Return the inverse of the CacheMatrix
        inverse
}
