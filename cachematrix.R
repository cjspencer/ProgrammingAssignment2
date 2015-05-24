# Matrix inversion can be a computationally expensive and time consuming computation.
# If the result of the matrix inversion comuptation must be recalled for future computations
# it can be more efficient to cache the result to be recalled when needed rather than 
# compute the result again.
# 
# ## makeCacheMatrix creates a list containing a funciton to:
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the inverse value of the matrix
# 4. get the inverse value of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function () x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


# The function cacheSolve returns the inverse of the matrix.  First, the function
# checkes if the the inverse has been computed and stored in the cache.  If the inverse 
# has been computed the computation is skipped and the inverse is retrieved from the cache.
# If the inverse has not been computed, the computation is executed and the inverse is 
# set in the cache by the setInverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data,...)
        x$setInverse(inv)
        inv
}

# Test
# > x = rbind(c(2, -1/2), c(-1/2, 2))
# > m = makeCacheMatrix(x)
# > m$get()
#      [,1] [,2]
# [1,]  2.0 -0.5
# [2,] -0.5  2.0
# 
# > cacheSolve(m)
#        [,1]      [,2]
# [1,] 0.5333333 0.1333333
# [2,] 0.1333333 0.5333333
# 
# Since the computation had been performed, when it is run again 
# the result is called from the cache and the message "getting cached data"
# is recieved.
# > cacheSolve(m)
# getting cached data
#         [,1]      [,2]
# [1,] 0.5333333 0.1333333
# [2,] 0.1333333 0.5333333
