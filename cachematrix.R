##These functions work together to first create and store a matrix object that caches its inverse, and then
##refer to the cached value if it already exists. Otherwise, the inverse of the matrix is calculated, 
##and then set as the cached value, which is then used in future calls.

##This function creates a matrix object that caches its inverse
makeCacheMatrix <- function(x = matrix()) {
        inv = NULL
        set = function(y) {
                x <<- y
                inv <<- NULL
        }
        get = function() x
        setinv = function(inverse) inv <<- inverse 
        getinv = function() inv
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}


##This function computes the inverse of a matrix, first checking for and using a cached value if it
##exists, otherwise calculates it then sets it as the cached value.

cacheSolve <- function(x, ...) {
        inv = x$getinv()
        if (!is.null(inv)){
                # get it from the cache and skips the computation. 
                message("getting cached data")
                return(inv)
        }
        mat.data = x$get()
        inv = solve(mat.data, ...)
        x$setinv(inv)
        inv
        
}
