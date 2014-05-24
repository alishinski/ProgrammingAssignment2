## This function takes a matrix as an input and returns a list of 4 functions defined in order to work
## with the matrix.  One function sets the matrix or changes it, the second returns it, the third assigns
## a value for the inverse of the matrix, and the fourth returns that value.

makeCacheMatrix <- function(x = matrix()) {
        inv = NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL                
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## This function takes the sort of list produced by makeCacheMatrix as an input and returns the inverse 
## of the matrix assigned to it, either by calculating it, or looking up the value associated with it
## through the makeCacheMatrix function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinv(inv)
        inv        
}
