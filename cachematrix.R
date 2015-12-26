## Put comments here that give an overall description of what your
## functions do

## This function will both set the value of a matrix as well as the inverse of the matrix
## It will also get the the value of the matrix and its inverse.

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


## cacheSolve is the "solve" equivalent for matrices in the environment setup by makeCacheMatrix;
## it will cache the result and retrieve already cached results from memory. 

cacheSolve <- function(m, ...) {
    sol <- m$getInv()
    if(!is.null(sol)) {
        message("Solved from cache")
        return(sol)
    }
    data <- m$get()
    sol <- solve(data, ...)
    m$setInv(sol)
    sol
}

        ## Return a matrix that is the inverse of 'x'
}
