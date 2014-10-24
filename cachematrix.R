## makeCacheMatrix function is to create a special matrix object 
## that can cache its inverse


makeCacheMatrix <- function(x = matrix()) {

    m <- NULL                ##predefined value of NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x                           ##to get the matrix value
    setinverse <- function(inverse) m <<- inverse ##to set the inversed value of matrix to 'm'
    getinverse <- function() m                    ##to get back the inversed value of matrix
    list(set = set, get = get, 
         setinverse = setinverse,
         getinverse = getinverse)
    
}


## cacheSolve function is to compute the inverse of 
## matrix returned by makeCacheMatrix above
## if it has been calculated before, the cacheSolve would
## retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()                ##getting the inverse value from cached matrix
    if(!is.null(m)) {                  ##if m is not NULL, the inverse has been calculated before  
                                       ## if m is NULL, exit if-loop function
        message("getting cached data") ##to notified that the result is by getting cached data
        return(m)                      ## to return cached data 'm' and exit function cacheSolve
    }
    
    ##Continue if m is NULL
    data <- x$get()        ##to get matrix data  
    m <- solve(data, ...)  ##to solve and return inverse of matrix data
    x$setinverse(m)        ##to assign/set calculated inverse to 'm'
    m                      ##to return value of m
    
}
