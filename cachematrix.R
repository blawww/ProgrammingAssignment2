## This pair of functions allows you to create a matrix an matrix
## and cache its inverse.  This saves processing time becasue after
## inverse of the matrix has been solved, it is stored.  Subsequent
## calls to find the inverse of the matrix returns the stored copy
## of the matrix instead of calling a function to solve for the inverse.

## makeCacheMatrix initializes the cached matrix.
## syntax is: 
## x <- makeCacheMatrix(m)
## where m is the starting matrix.
## and x is the cached matrix.
## 
## x contains a list of four functions:
## x[1]: function to set value of matrix
## x[2]: function to get value of matrix
## x[3]: function to solve for inverse
## x[4]: function to get inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL 
    ## m stores the inverse matrix, starts as NULL
    ## the 1st function, set assigns the starting matrix
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    ## the 2nd function get returns the starting matrix
    get <- function() x
    ## the 3rd function setinv takes in the inverse and sets m to it
    setinv <- function(solve) m <<- solve
    ## the 4th function returns the inverse
    getinv <- function() m
    ## makeCacheMatrix returns a list of the 4 functions
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## cacheSolve returns the inverse of the matrix.
## it first checks if the inverse is stored.
## if the inverse is stored, it returns the stored inverse
## Otherwise, it gets the starting matrix, solves for the inverse,
## stores the inverse, then returns the inverse.
## Then, subsequent calls to cacheSolve using the initialized
## cacheMatrix will return the stored inverse matrix.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinv()
    ## check if the inverse has been cached.
    ## returns the cached inverse if it has been cached.
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    ## if inverse has not been cached, the following lines are executed:
    ## gets the initialized matrix and stores it in "data"
    data <- x$get()
    ## solves the inverse of "data" and stores it in "m"
    m <- solve(data, ...)
    ## caches the inverse, "m" in the initialized matrix
    x$setinv(m)
    ## returns "m"
    m
}
