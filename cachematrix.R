## These functions are used to create a special object that stores a
## matrix and caches its inverse

# This function creates a special "matrix" object that can cache 
# its inverse

makeCacheMatrix <- function(x = matrix()) {
                # Initialise inverse matrix to NULL        
                inverse <- NULL
                
                # Function that sets the value of the matrix
                set <- function(y) {
                        x <<- y
                        
                        # Define matrix inside function (apply R scoping rules)
                        inverse <<- NULL
                }
                
                # Function that gets the value of the matrix
                get <- function() x
                
                # Function that sets the inverse of the matrix
                setInverse <- function(solve) inverse <<- solve
                
                # Function that gets the inverse of the matrix
                getInverse <- function() inverse
                
                # List of all the functions defined above
                list(set = set, get = get,
                     setInverse = setInverse,
                     getInverse = getInverse)
        }


# This function computes the inverse of the special "matrix" returned 
# by makeCacheMatrix. If the inverse has already been calculated (and 
# the matrix has not changed), then the cacheSolve should retrieve the 
# inverse from the cache.

cacheSolve <- function(x, ...) {
        
        # Check if the inverse has been already computed
        inverse <- x$getInverse()
        
        # If so, return the cached data
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        
        # If not, get the matrix and compute its inverse
        data <- x$get()
        inverse <- solve(data, ...)
        x$setInverse(inverse)
        
        ## Return a matrix that is the inverse of 'x'
        inverse
        
}
