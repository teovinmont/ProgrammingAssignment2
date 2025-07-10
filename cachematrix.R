# These functions create a special object that stores a matrix
# and caches its inverse to avoid redundant calculations.

# This function creates a special object that stores a matrix and its inverse.
makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        getsolve <- function() s
        setsolve <- function(solve) s <<- solve
        list(set = set, get = get, getsolve = getsolve, setsolve = setsolve)
}

set.seed(123)
M <- makeCacheMatrix(matrix(sample(-10:10, 16), nrow = 4, ncol = 4))

# This function computes the inverse of the matrix stored in the special object.
# If the inverse has already been calculated, it retrieves it from the cache.
cacheSolve <- function(x, ...) {
        # Return the inverse of the matrix stored in 'x'
        s <- x$getsolve()
        if (!is.null(s)) {
                message('Getting cached data')
                return(s)
        } else {
                data <- x$get()
                s <- solve(data)
                x$setsolve(s)
                s
        }
}

cacheSolve(M)
