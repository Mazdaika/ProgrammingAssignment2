## These two functions allow to calculate inverse matrices for squared invertible 
## matrices and store the inverse in cache in order to re-use it without 
## re-calculating


## function makeCacheMatrix creates a matrix-like object that stores the
## square matrix and its inverse and allows to change the matrix itself
## or its inverse
makeCacheMatrix <- function(m = matrix()) {
    ## this part checks that the input is a square matrix, otherwise the 
    ## inverse matrix cannot be defined
    if(class(m) != "matrix") {
        stop("object x must be a square matrix")
    } else {
        if(nrow(m)!=ncol(m)){
            stop("matrix must be square")
        }
    }
    
    ## variable 'solved' stores the cache for inverse of the matrix, which is NULL
    ## unless the inverse has been externally counted and passed to the object
    ## via "setSolve" function
    solved <- NULL
    
    ## re-initiates the object with a new matrix as an input
    set <- function(y) {
        m <<- y
        solved <<- NULL
    }
    
    ## returns the original matrix
    get <- function() return(m)
    
    ## sets invesre of the matrix
    setSolve <- function(s) solved <<-s
    
    ##returns the inverse of the matrix
    getSolve <- function() return(solved)
    
    ## returns a list of all available for this object functions
    return(list(set = set, get = get,
         setSolve = setSolve,
         getSolve = getSolve))
}


## function cacheSolve checks the presense of cached inverse matrix.
## if the cache exists it returns the cached matrix.
## otherwise it calculates the inverse and stores it in cache before returning it

cacheSolve <- function(x, ...) {
    ##checks whether the matrix is invertible
    if(det(x$get()) == 0) {stop("Matrix has to be invertible")}
    
    ## chechs for cached inverese and returns it if found
    if(!is.null(x$getSolve())) {
        message("getting pre-cached inverse")
        return(x$getSolve())
    }
    ## stores inverse of matrix in cache
    x$setSolve(solve(x$get()))
    
    ##returns inverse of matrix from cache
    return(x$getSolve())
}