## makeCacheMatrix creates a list containing a function to
## 1. set a value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function() inv
        list(set=set, get=get, setinverse=setinverse,getinverse=getinverse)
}


## The following function returns the inverse of thematrix. It check if the inverse has 
## has already been computed. If so, it gets the result and skips the computation. If not,
## it computes the inverse, sets the value in the cache via setinverse function

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data.")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}

## Inverse of the cacheable matrix returned by makeCacheMatric()
## If the inverse has already been calculated and there is no charge in the matrix
## cacheSolve() returns the cached inverse
cacheSolve <- function(cacheable.matrix, ...){
	inverted.matrix <- cacheable.matrix$get.inverse()
	## We need to know if cached matrix is available
	if(!is.null(inverted.matrix)) {
		message("Getting cached inverse matrix")
		return(inverted.matrix)
		}
	## If there is not cached matrix available, 
	## we create inverted matrix in case
	matrix.to.inverse <- cacheable.matrix$get()
	inverted.matrix <- solve(matrix.to.inverse)
	cacheable.matrix$set.inverse(inverted.matrix)
	inverted.matrix
}
 