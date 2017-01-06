## As requested in the assignement, this function will take a matrix, creates a special matrix object, defining methods.
##space between lines is for better visualization. 

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
        
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        get <- function () x
        
        setinv <- function(solve) inv <<- solve
        
        getinv <- function () inv
        
        list( set = set, get = get,
              setinv = setinv, getinv = getinv)
}


## in the function below, inversion is  calculated and displayed, if it hasn't been calculated before.
## If it has, it will display a message telling that data is cached and display it. 

cacheSolve <- function(x, ...) {
        
		inv <- x$getinv()
        
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        data <- x$get()
		
        inv <- solve(data, ...)
		
        x$setinv(inv)
		
        inv
}
