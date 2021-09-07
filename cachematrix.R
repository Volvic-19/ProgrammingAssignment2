##These two functions work together to cache potentially time-consuming
##computations


## makeCacheMatrix: This function creates a special "matrix" object that 
## can cache its inverse.

makeCacheMatrix <- function(x=matrix()){
    m <- NULL
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    get <- function()x
    set_inv <- function(solve) m <<- solve
    get_inv <- function() m
    list(set=set,get=get,set_inv=set_inv,get_inv=get_inv)
}

##cacheSolve: This function computes the inverse of the special "matrix" 
##returned by makeCacheMatrix above. If the inverse has already been calculated,
##then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x,...){
    m <- x$get_inv()
    if(!is.null(m)){
        message('getting cached data')
        return(m)
    }
    matrix <- x$get()
    m <- solve(matrix)
    x$set_inv(m)
    m
}
