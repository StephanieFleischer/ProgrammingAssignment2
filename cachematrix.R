## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

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
