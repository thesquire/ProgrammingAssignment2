## Together these functions will receive a matrix, compute its inverse, and cache the result

## Create a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        a <- NULL
        set <- function(y) {
                x <<- y   
                a <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) a <<- solve
        getinverse <- function() a
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Computes the inverse of the special "matrix" returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        a <- x$getinverse()
        if(!is.null(a)){
                message("getting cached data")
                return(a)
        }
        b <- x$get()
        a <- solve(b)
        x$setinverse(a)
        a
}
