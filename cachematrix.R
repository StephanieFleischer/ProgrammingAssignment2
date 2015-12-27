## Put comments here that give an overall description of what your
## functions do

## This function will create a matrix and can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    set <- function(mat) {
        x <<- mat
        inv <<- NULL
    }
    get <- function() x
    setInv <- function(inverse) inv <<- inverse
    getInv <- function() inv
    
    list(set = set, get = get,
         setInv = setInv, getInv = getInv)
}


## This function computes the inverse of the “matrix” returned by makeCacheMatrix(). 
## If the inverse has already been calculated and the matrix has not changed,
## it’ll retrieves the inverse from the cache directly.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data) %*% data
        x$setinv(m)
        m
}
