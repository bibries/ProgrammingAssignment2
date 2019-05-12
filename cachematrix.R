## Matrix inversion is usually a costly computation and there may be some benefit 
## to caching the inverse of a matrix rather than compute it repeatedly.
## The pair of functions are used to cache the inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {
        ## This function creates a special "matrix" object that can cache its inverse.
        
        ## Input:
        ##      x: a square matrix
        ## Output:
        ##      Return a list containing 4 functions:
        ##        - set: set the matrix
        ##        - get: get the matrix
        ##        - setInverse: set the inverse matrix
        ##        - getInverse: get the inverse matrix
        
        ## Initialize the inverse matrix
        mInverse <- NULL
        
        ## Implement the function to set the matrix
        set <- function(y) {
                ## Using the <<- operator to assign a value to an object in an environment that is different from the current environment
                x <<- y
                mInverse <<- NULL
        }
        
        ## Implement the function to get the matrix
        get <- function() x
        
        
        ## Implement the function to set the inverse matrix
        setInverse <- function(inverse) mInverse <<- inverse

        ## Implement the function to set the inverse matrix
        getInverse <- function() mInverse
        
        ## return a list of the functions: set, get, setInverse and getInverse
        list(set = set, 
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
        
}


cacheSolve <- function(x, ...) {
        ## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
        ## If the inverse has already been calculated (and the matrix has not changed), 
        ## then the cachesolve return the inverse from the cache.
        ## If not, the function computing the inverse of a square matrix using the solve function in R. 
        
        ## Input:
        ##      x: a square matrix
        ## Output:
        ##      Return a matrix that is the inverse of 'x'

        ## get the inverse of x
        mInverse <- x$getInverse()
        
        ## If the inverse has already been calculated (and the matrix has not changed)
        if(!is.null(mInverse)) {
                message("getting cached data...")
                ## return the inverse matrix from the cache
                return(mInverse)
        }
        
        ## If not, computing the inverse of a square matrix
        ## get the matrix
        mData <- x$get()
        ## Computing the inverse using the solve function
        mInverse <- solve(mData, ...)
        ## set the result in x
        x$setInverse(mInverse)
        
        ## Return the inverse matrix of 'x'
        return(mInverse)
}
