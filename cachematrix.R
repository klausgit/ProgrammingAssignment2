## "cachematrix.R" contains two functions that create the invers of a matrix and 
## cache it.

## The first function, `makeCacheMatrix` creates a special "matix", which is
## really a list containing a function to

##  1.  set the value of the matrix
##  2.  get the value of the matrix
##  3.  set the value of the inverse matrix
##  4.  get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
            x <<- y
            m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This functions returns, the inverse of the matrix x. 
## If there is no matrix defined yet, the function will create a matrix first, 
## before it calculates its inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
