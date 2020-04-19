## x is iniialized as function argument
makeCacheMatrix <- function(x = matrix()) {
        s <- NULL  ## make variable to later used
        ## this function set the values to the parent enviourment
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) s <<- solve(x)
        getinverse <- function() s
        ## allows us to use $ form of extract operator
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## this function will check the value of s .If S value is cached before than it will retreive 
##from cache otherwise will show null nd compute the inverse of matrix nd cache it.

cacheSolve <- function(x, ...) {
        s <- x$getinverse()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setinverse(s)
        s}
