# create matrix that can have its inverse solved
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        cacheInverse <- function() {
        if (!is.null(inv))       {
               return(inv)
        }
                inv <- solve(x)
                return(inv) 
        }

        list(set = set, get = get,
            cacheInverse = cacheInverse)
}

# function to solve matrix inversion
cacheSolve <- function(x, ...)
{
        inv <- x$cacheInverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
