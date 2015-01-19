# This is the R Programming Assignment 2: Lexical Scoping (caching the inverse of a matrix)

# The makeCacheMatrix function creates a special "matrix", which is really a list
# containing a function to
# 1 - set the value of the matrix
# 2 - get the value of the matrix
# 3 - set the value of the inverse of the matrix
# 4 - get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

# The cacheSolve function calculates the inverse of the special "matrix" which was created
# with the makeCacheMatrix function in the previous step.
# Instead of computing directly, it first checks if the inverse has already been calculated.
# If that's true, it gets the inverse from the cached data and skips the computation.
# Otherwise, it calculates the inverse of the matrix using solve function in R
# and then sets the value of the inverse in the cache by the setinverse function.

cacheSolve <- function(x, ...) {
        # Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        matrix <- x$get()
        m <- solve(matrix, ...)
        x$setinverse(m)
        m
}