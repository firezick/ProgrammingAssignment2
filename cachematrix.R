## Put comments here that give an overall description of what your
## functions do


## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    #initialize inverse value
    i <- NULL
    
    #set matrix value
    set <- function(y) {
        x <<- y
        i <- NULL
    }
    #get matrix value
    get <- function() x
    #set inverse matrix
    setInverseMatrix <- function(solve) i <<- solve
    #get inverse matrix
    getInverseMatrix <- function() i
    
    #return a list
    list(set = set, 
         get = get,
         setInverseMatrix = setInverseMatrix,
         getInverseMatrix = getInverseMatrix)

}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
    #check if the inverse is already cached
    i <- x$getInverseMatrix()
    
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    
    data <- x$get()
    i <- solve(data, ...)
    
    x$setInverseMatrix(i)
    
    #return inverse
    i
}
