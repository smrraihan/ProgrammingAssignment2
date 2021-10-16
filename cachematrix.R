## The functions defined below work in conjunction to define a matrix and 
## calculate its inverse. Then they store the matrix and its inverse in the 
## cache memory to retrieve it later and avoid recalculation again and again.
## They also define methods to re-assign the value of matrix and recalculate 
## the inverse of new matrix.

## This function creates a special object that creates a matrix and its inverse
## Then it caches both in the form of object called. It contains a list of 
## functions that 'set' the value of matrix and its inverse, and 'get' the value
## of stored matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function (mat) {
                x   <<- mat
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(matinverse) inv <<- matinverse 
        getinv <- function() inv
        list( set = set,
              get = get,
              setinv = setinv,
              getinv = getinv)
}


## This function complements the function defined above. It checks for the
## presence/existence of inverse of the matrix defined in makeCacheMatrix().
## If it is present, it returns the inverse from cache memory. Otherwise, it
## calculates the inverse and stores it in the object defined by the function
## defined above 'makeCacheMatrix()'

cacheSolve <- function(x, ...) {
        sol <- x$getinv()
        if (!is.null(sol)) {
                message("getting cached data")
                return(sol)
        }
        dat <- x$get()
        sol <- solve(dat, ...)
        x$setinv(sol)
        sol
}
