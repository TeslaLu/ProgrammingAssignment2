## Using super assignment operator to assign a value to an object in an environment that is different from the current environment.

## makeCacheMatrix will create a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

        ## Before computing, the inverse of matrix is NULL.
        Minverse <- NULL
        
        ## set the value of the matrix and the value of the inverse
        set <- function(y) {
            x <<- y
            Minverse <<- NULL
        }
        
        ## get the value of the matrix
        get <- function() x
        
        ## set the value of the inverse of matrix
        setinverse <- function(inverse) Minverse <<- inverse
        
        ## get the inverse of matrix
        getinverse <- function() Minverse
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
    }

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return the cached inverse of matrix, if it has been computed before. Otherwise, return 'NULL'.
        Minverse <- x$getinverse()
        if(!is.null(Minverse)) {
            message("getting cached data")
            return(Minverse)
        }
        ## Compute the inverse of matrix, if it hasn't been calculated yet.
        data <- x$get()
        Minverse <- solve(data, ...)
        
        ## Cache the data using the makeCacheMatrix function
        x$setinverse(Minverse)
        
        ## print the calculated inverse of a matrix
        Minverse
    }