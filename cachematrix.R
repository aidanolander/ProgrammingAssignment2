## These two functions set the values of a matrix and its inverse, creating a special
## matrix that caches the inverse. Then it checks if the inverse has already been
## cached, otherwise it solves it and caches the solution. 

## This first function will set the values of the matrix and its inverse, and get
## the values of each

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL ##inverse starts at NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL            #sets values using <<- operator, which assigns
                                        #value in a different environment
        }
        get <- function() x             #get value of matrix
        setinv <- function(inverse) inv <<- inverse     #set value of inverse matrix
        getinv <- function() inv        #get value of inverse
        
        list(set = set, get = get, setinv = setinv, getinv = getinv)

}


## first this function will see if we've gotten a previous result for the matrix
## that has been cached. If it finds one, it will return it and let you know it was cached
## otherwise it solves and sets the inverse, then returns it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {                             #check if it's null, which
                message("getting cached inverse!")      #would mean we haven't cached it
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)      #solves for inverse if not cached before
        x$setinv(inv)
        inv
        
}
