## Homework 2 by ÑÖÕ×«‘£¬Zhaoxun Yan.
## These two functions will store a matrix, calculate its inverse
## as well as store it. Moreover if the inverse of the same matrix
## is asked again, the stored inverse matrix can be retrieved
## rather than being recalculated.

## This function creates a special "matrix" object 
## that can cache its inverse and itself;
## It also returns a list of four sub-functions,
## by which the matrix and its inverse can be cached and retrieved

makeCacheMatrix <- function(x = matrix()) {
    v <- NULL
    set <- function(y) {
        x <<- y         
        # "<<-" assigns y in set() to x in parent function makeCacheMatrix()
        v <<- NULL
    }
    get <- function() {x}
    setinv <- function(inv) {v <<- inv}
    getinv <- function() {v}
    return(
        list(set = set, get = get, setinv = setinv, getinv = getinv)
        )    
}


## This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already been
## calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache.

cacheSolve <- function(L) { # L is the result list of makeCacheMatrix(x)
    ## need to load the package "matrixcalc" in case of computing inverse
    require(matrixcalc)
    
    ## To check if the inverse is already stored in variable "v"
    v <- L$getinv()
    
    ## if "v" is nonempty, just retrieve it from cache
    if(!is.null(v)) {
        message("getting cached data")
        return(v)
    }
    
    ## "v" is empty, need to compute it by function "matrix.inverse" and store
    data <- L$get()
    v <- matrix.inverse(data)
    L$setinv(v)
    
    return(v)
}

## Usage Example:
# A = matrix(1:4, nrow=2)
# L = makeCacheMatrix(A)
# cacheSolve(L)