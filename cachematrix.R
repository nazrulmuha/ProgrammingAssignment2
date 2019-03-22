## Put comments here that give an overall description of what your
## functions do
## The makeCacheMatrix funtion calculates the inverse of the matrix and the cacheSolve function calls the cached inversed matrix

## Write a short comment describing this function
## This function creates a matrix and stores in the makeCacheMatrix. 
## It contains a function to 1.set and get the value pf the matrix and 2. set and get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
            m <- NULL
            set <- function(y) {
                    x <<- y
                    m <<- NULL
            }
            get <- function() x
            setinverse <- function(inverse) m <<- inverse
            getinverse <- function() m
            list(set = set, get = get,
                 setinverse = setinverse,
                 getinverse = getinverse)

}


## Write a short comment describing this function
## This function can be used to call the calculated inverse of the matrix from the cache created with the function above

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
