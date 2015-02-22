makeCacheMatrix <- function(mx = matrix()) { #List of functions
    w <- NULL #Inverse of mx
    set <- function(y) {
        mx <<- y
        w <<- NULL
    }
    get <- function() mx  # get the setted matrix
    setinverse <- function(inverse) w <<- inverse
    getinverse <- function() w
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}
 #The next function computes the inverse of the special "matrix" returned by makeCacheMatrix above
cacheSolve <- function(mx, ...) {
    w <- mx$getinverse()
    if(!is.null(w)) {
        message("getting cached data.")
        return(w)
    }
    data <- mx$get()
    w <- solve(data)
    mx$setinverse(w)
    w
}
