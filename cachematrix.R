## These functions will cache matrix inversion

## Caches the matrix and returns on demand
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    
    setInv <- function(inver) {
        inv <<- inver
    }
    getInv <- function() inv
    
    list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## Inverses a matrix and adds to cache.
## Retrieves data from cache if it already exists
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getInv()
    if (!is.null(i)) {
        # Inverse available in cache
        message("Getting inverse from cache")
        return (i)
    } else {
        # Inverse needs to be calculated
        mat2Inv <- x$get()
        i <- solve(mat2Inv)
        x$setInv(i)
        i
    }
}
