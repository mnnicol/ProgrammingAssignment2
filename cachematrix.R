## This function returns an object (a list of 4 functions) that
## operate on a matrix passed to the function. This can be 
## thought of as a "constructor" for a cacheMatrix object.
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## Return a matrix that is the inverse of 'x'
## The value passed in must have been created by
## calling makeCachedMatrix above. This function works
## by calling to get the solved matrix, and if it is not
## set, this function calculates the solved matrix and stores
## it by calling setsolve
cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}

## This one is better because it encapsulates the data
## and caching semantics in one object. All data manipulation
## is done locally, so other functions do not need to know what
## data is inside this object.
makeMatrix <- function(original = matrix()) {
	   inversed <- NULL
        set <- function(y) {
                original <<- y
                inversed <<- NULL
        }
        get <- function() original
        inverse <- function() {
           if(!is.null(inversed)) {
                message("getting cached data")
           } else {
                message("calculating inverse")
           	inversed <<- solve(original)
	   }
	   return(inversed)
        }
        list(set = set, get = get, inverse = inverse)
}
