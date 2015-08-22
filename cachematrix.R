## These 2 functions cache the inverse of a matrix (which is always invertible, in this case)

## The first function creates a special "matrix" object that can catche its inverse. 
## This object is actually a list of 4 functions - (1) set and (2) get the matrix, 
## and (3) set and (4) get its inverse matrix
## The function is similar to the one presented in the example, only instead of a vector, 
## x is defined as a matrix

makeCacheMatrix <- function(x = matrix()) {
        mat <- NULL
        set <- function(y) {
                x <<- y
                mat <- NULL
        }
        get <- function() x
        setinverse <- function(solve) mat <<- solve
        getinverse <- function() mat
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function returns the inverse matrix of the matrix created with the first function
## In order to avoid inversing a matrix that was already inveresed, this function first checks
## to see if the inverse matrix has already been created, and then returns it. If not, it inverses
## the matrix and returns it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        mat <- x$getinverse()
        ## condition for checking whether the inverse matrix already exists
        if(!is.null(mat)) {
                message("getting cached data")
                return(mat)
        }
        ## getting the matrix and then applying the functions to get the inverse one
        data <- x$get()
        mat <- solve(data, ...)
        x$setinverse(mat)
        mat
}
