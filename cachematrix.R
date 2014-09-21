## Cachematrix and its helper function make CacheMatrix calculate the inverse of a given
## matrix, either retrieving an already calculated results from cache, or making the 
## calculation for the first time and saving the result to cache

## makeCacheMatrix takes a matrix as an input and returns a list of 4 functions
## usage e.g.:
## m = matrix(c(2,4,3,4,4,9,2,1,3),nrow=3)
## m1 <- makeCacheMatrix(m)
## OBS.:  for some reason, m <- makeCacheMatrix(m) does not work

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

## cacheSolve takes a matrix as an input and returns its inverse, either from cache,
## if possible, or calculating the inverse for the first time and saving it to cache
## usage e.g.    cacheSolve(m1)

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
