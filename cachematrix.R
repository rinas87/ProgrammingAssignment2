## makeCacheMatrix function is to create a special matrix object 
## that can cache its inverse


makeCacheMatrix <- function(x = matrix()) {

    m <- NULL                ##predefined value of NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
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
    m <- x$getinverse()
    if(!is.null(m)) {     ##if m is not NULL, 
                          ##the inverse has been calculated before
                          ## in setinverse in function 
        message("getting cached data")  
        return(m)         ## to return cached data 'm' and exit function
    }
    data <- x$get()
    m <- solve(data, ...)  ##to return inverse of matrix
    x$setinverse(m)        ##to assign the calculated inverse back to m
    m
    
}
