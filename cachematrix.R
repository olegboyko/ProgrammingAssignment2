## The customer oriented function is cacheSolve() which return inversed matrix for initial matrix X.
## Function cacheSolve() internaly call makeCacheMatrix() to check if inverse matrix for matrix X
## already calculated and saved in cache and doing these steps if there is no cache for matrix X.

## Function makeCacheMatrix() create cache for matrix X and has 4 internal functions to get/set matrix itself and
## its inverse matrix getinverse/setinverse. It uses lazy getinverse initialization.

makeCacheMatrix <- function(x = numeric()) {
    ## arg. "x = matrix()" was changed on "x = numeric()" because
    ## next check is if x is square matrix().
    ## To make is_square_matrix() check package "tester" from Cran.R-project.org should be installed and used.
    
    ## There is no profit to collect matrixes in cache which could not be inverted.
    if (!is_square_matrix(x)) {
        message("you could try to inverse only square matrix (nrow == ncol)")
        return(NA)
    }
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## This function return inverse matrix of matrix X.
## On the first calculation value of inverse matrix will be saved in cache and
## on the second and any next call for same matrix its inverse value will be taken from cache.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        message("calculating data")
        data <- x$get()
        m <- solve(data)
        x$setinverse(m)
        m
}
