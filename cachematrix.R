## These functions save computing time by caching the results
## of an expensive calulation (solve(x)) and allowing it to be
## retrieved later

## The function makeCacheMatrix(x) creates a list of functions 
##relating to a particular matrix that allow a cached value to be
## retrieved if it is already there.

makeCacheMatrix <- function(x = matrix()) {
        #check that the matrix is square
        if(nrow(x) != ncol(x)) {
                message("Needs to be a square matrix to be inverted")
                return()
        }
        m <- NULL
        # define the four functions that will be in the list
        # first set the matrix
        mset <- function(y) {
                x <<- y
                m <<- NULL
        }
        # get the matrix
        mget <- function() x
        # if m is not there, set it by solving the matrix
        msetinv <- function(solve) m <<- solve
        # if m is there, retrieve the value
        mgetinv <- function() m
        list(mset = mset, mget = mget,
             msetinv = msetinv,
             mgetinv = mgetinv)
}


## the function cacheSolve uses the list of functions derived from
## the function makeCacheMatrix which either uses the cached 
##value, if available, or generates the inverse of the matrix and
## stores it to the cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
                # load the value of getinv from the created function
        m <- x$mgetinv()
        # if an answer is already there, retrieve it
        if(!is.null(m)) {
                message("getting cached inverse")
                return(m)
        }
        data <- x$mget()
        # otherwise, solve the matrix
        m <- solve(data, ...)
        x$msetinv(m)
        m
}
