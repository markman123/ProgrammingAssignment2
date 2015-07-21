##  makeCacheMatrix is a function with functions, that accepts a matrix optionally
## if we assume m <- makeCacheMatrix():
## Initialisation of m creates a local variable inv, and sets it to null
##-----------------------------------
## m$set: sets a variable 'x' and sets the inverse to be NULL
## m$get: prints the matrix stored in 'x'
## m$setinverse: sets the inverse of 'x' (although not necessarily) equal to input to variable inv
## m$getinverse: simply returns inv

## makeChacheMatrix creates a special object to hold the information for a matrix
## It can then be inverted by cacheSolve
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse_matrix) inv <<- inverse_matrix
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse
         )
}


## The cacheSolve takes an instance of makeCacheMatrix and either
## 1) solves the inverse if the inverse variable inv is null or
## 2) returns the already calculated variable var
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data,...)
    x$setinverse(inv)
    inv
}
