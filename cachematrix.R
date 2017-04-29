## Calculates the inverse of a matrix, but first checks to
## see if the answer is saved in the global cache


## Sets up the variables to be used in cacheSolve
## so that they can be called in the global environment

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


## Checks to see if the inverse is already stored in the cache ('m')
##      If so, the function returns the cache
##      If not, the function calculates the inverse

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
