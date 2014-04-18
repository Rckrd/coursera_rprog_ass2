## these functions creates a matrix object with a cache to 
## store computed inverses, this will speed up subsequent uses
## of the particular matrix inverse


## create a matrix object with getters and setters
## a new object will not have a precalculated inverse,
## but will calculate it and store it once it is called
## subsequent calls will return the cached inverse instead of calculating it 
## if the matrix is changed with the set method, it will remove the precalculated inverse
makeCacheMatrix <- function(x = matrix()) {
    x_inv <- NULL
    set <- function(y) {
        x <<- y
        x_inv <<- NULL
    } 
    get <- function() x
    setinv <- function(inv) x_inv <<- inv
    getinv <- function() x_inv
    list(set = set, 
         get = get,
         setinv = setinv,
         getinv = getinv)
}



## return the matrix object's inverse
## first call will calculate it and store the result in 
## the objects cache, subsequent calls will return the cach
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached inverse")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinv(inv)
    inv
}
