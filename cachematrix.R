## This file includes two functions to implement cached matrix inverse operations.
## The functions create a matrix, and solve the inverse of a matrix.

## Creates a matrix and cache by creating a special list that includes functions
## that set the the matrix, get the matrix, set the inverse of a matrix, and get the inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Calculates the inverse of a matrix.
## First, checks to see if the inverse has already been calculated and cached.
## If it has, returns it.
## If not, calculates the inverse and returns it.

cacheSolve <- function(x, ...) {
       ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data!")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
