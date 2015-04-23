## There are two functions here 1. makeCacheMatrix, 2. cacheSolve
## Both these functions work together. 
## makeCacheMatrix creates a special"matrix" object that can cache its inverse
## cacheSolve; returns inverse of the special "matrix" returned by makeCacheMatrix
## if inverse was previously calculated, it will return the cached result instead of 
##calculating again.


## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        # set m as NULL for first time
        m<- NULL
        
        # This is the set function
        set <- function(y)  {
                x <<- y
                m <<- NULL
        }
        
        #This is the get function, just returning the value as is
        get <- function() {x}
        
        
        # saving the Inverse value to m for future use
        setInverse <- function(inverse) {m<<-inverse}
        
        
        # give the inverse value if it was previously calculated
        getInverse <- function() {m}
        
        list( set = set, get=get,
              setInverse = setInverse,
              getInverse = getInverse)

}


## Returns inverse value of the matrix, either by calulating or if previously calculated
## from Cache. It will let user know if the result was acquired from cache.

cacheSolve <- function(x, ...) {
        # get the Inverse value that is saved
        # if it is not NULL that means it was previously calculated
        # the previously calculted value is passed back from the cache,
        # if not it is calulated and and saved for future use
        
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        } else {
                mat_data<- x$get()
                m <- solve(mat_data)
                x$setInverse(m)
                return (m)
        }
        
}
