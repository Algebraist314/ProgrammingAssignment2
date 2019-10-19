## Define two functions, one to cache a matrix inverse, another to either calculate a matrix inverse or retrieve
## a cached value

## Example usage:
## z <- makeCacheMatrix()
## z$set(matrix(c(1,2,-5,6), nrow = 2))
## cacheSolve(z)

## Function to define a set of functions to update and access cached values
## `get` and `set` will return and update the cached matrix
## `getinv` and `setin` will return and update the cached matrix inverse

makeCacheMatrix <- function(x = matrix()) {
    
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinv <- function(inv) i <<- inv
    getinv <- function() i
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
    
}


## Function to access cached value of matrix inverse or calculate if not previously cached, and return the inverse

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinv()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
    i
}
