##These functions are used to create a special matrix that stores a matrix 
## and cache's its inverse

## makeCacheMatrix takes a matrix object and creates a special 2X2 matrix
## which contains 4 functions set the matrix, get the matrix, set 
## the inverse of the matrix and get the inverse of the matrix


## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(invmat) inv <<- invmat
        getinverse <- function() inv
        matrix(c(set, get, setinverse, getinverse), nrow = 2, ncol = 2)

}


## cacheSolve solves the inverse of the special matrix. It checks if the 
## inverse has already been solved. If it see this is true, the cached inverse
## is returned and skips a new inverse computation. Otherwise, it solves for 
## the inverse and caches the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        inv <- x[[2,2]]() ## get cached inverse matrix
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x[[2,1]]() ## get matrix data
        inv <- solve(data, ...)
        x[[1,2]](inv) ##set inverse matrix
        inv
}
