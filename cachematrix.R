## makeCacheMatrix is a function that creates an object of type list. 
## This object stores a matrix and its inverse, along with the methods to set the matrix value

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y = matrix()) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    create_inverse <- function(inverse) i <<- inverse
    get_inverse <- function() i
    list(set = set, get = get, setinverse = create_inverse, getinverse = get_inverse)
}


## The function accesses the object matrix, created by the makeCacheMatrix function.
## It returns the inverse of the matrix by using its cached value 
## or calculating it (and caching it)
## .
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)){
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
