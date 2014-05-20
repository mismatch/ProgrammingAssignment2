## Matrix inversion is usually a costly computation and there may be some benefit 
## to caching the inverse of a matrix rather than computing it repeatedly. That's 
## the purpose of this script.


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    set <- function(y) {
        if (!(is.null(y) || is.matrix(y)))
            stop("only matrix could be set")
        x <<- y
        inv <<- NULL
    }
    
    get <- function() x
    
    setInverseMatrix <- function(invMtx) {
        if (!(is.null(invMtx) || is.matrix(invMtx)))
            stop("only matrix could be set as inverse matrix")
        inv <<- invMtx
    }
    
    getInverseMatrix <- function() inv
    
    list(set = set, get = get,
         setInverseMatrix = setInverseMatrix,
         getInverseMatrix = getInverseMatrix)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the matrix 
## has not changed), then cacheSolve would retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    inv <- x$getInverseMatrix()
    
    if (!is.null(inv)) {
        message("getting inverse matrix from cache")
        return(inv)
    }
    
    data <- x$get()
    inv <- solve(data, ...)
    x$setInverseMatrix(inv)
    
    inv
}
