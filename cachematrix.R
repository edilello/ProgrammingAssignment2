## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

    # this is basically the constructor of a
    # matrix "object"
    inv_x <- NULL
    set <- function(y) {
        x <<-y
        inv_x <<-NULL
    }
    get <- function() x
    setinv <- function(inv) inv_x <<- inv
    getinv <- function() inv_x
    list(set = set, get = get, 
         setinv = setinv,
         getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    # check if inv_x is already computed
    inv_x <- x$getinv()
    if(!is.null(inv_x)) {
        message("getting cached data")
        return(inv_x)
    }
    
    # if not, compute and set inv_x
    # assuming X is a square, invertible matrix,
    # compute the inverse
    matrix = x$get()
    inv_x<-solve(matrix)
    # set and return
    x$setinv(inv_x)
    inv_x
}
