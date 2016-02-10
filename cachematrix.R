## Create a special matrix that can cache its 'Inverse'and avoid calculation on
## every invocation. The special matrix is collection of set of functions.
## Set function makes sure that the value is set in the parent environment.
## Input parameter is assumed to be invertible.

## CacheMatrix that stores the 'invertible' matrix input and its inverse matrix.

makeCacheMatrix <- function(x = matrix()) {

    inv <- NULL
    ## If Matrix is set using 'set' instead of using function parameter
    ## make sure to reflect it in the enclosing 'special' matrix
    ## created using 'makeCachematrix'
    set <- function(n) {
        x <<- n
        inv <<- NULL
    }

    get <- function() x
    setInverse <- function(i) inv <<- i
    getInverse <- function() inv

    ## Magic: The special matrix is nothing but 'list' of functions
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Function to calculate inverse of the 'cacheMatrix'only if it is not already
## set

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'

    ## Input 'x' is special matrix.
    inv <- x$getInverse()

    ## Return cached inverse value set on the special matrix
    ## or else compute the inverse and set it on the special
    ## matrix
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }

    mdata <- x$get()
    inv <- solve(mdata)
    x$setInverse(inv)

    inv
}
