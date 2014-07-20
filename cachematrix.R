## Implementation of the Programming Assignment 2 for the R Programming course

## Given a matrix x, creates a special version of the matrix,
## capable of caching its inverse to avoid repeating costly calculations
makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL                 # Initialize the variable for the inverse cache
    
	#Setter for the matrix
	set <- function(y) {
            x <<- y             #Store the new value for the matrix
            inv <<- NULL        #Clear the cache
    }
    
    #Getter for the matrix
    get <- function() x
    
    #Setter for the cached inverse
    setinverse <- function(inverse) inv <<- inverse
    
    #Getter for the cached inverse
    getinverse <- function() inv
    
    #Return the special matrix object (as a list of functions)
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Given the special matrix x, created by the makeCacheMatrix function
## return the inverse of the matrix
## If the inverse had already been calculated, the cached version is returned,
## otherwise the calculation is performed
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        inv <- x$getinverse()   #Get the cached inverse
        if(!is.null(inv)) {     #If it already been calculated ...
                message("getting cached data")
                return(inv)     #...return the cached version
        }
        data <- x$get()         #Otherwise, get the matrix
        inv <- solve(data, ...) #Calculate the inverse
        x$setinverse(inv)       #Cache it
        inv                     #...and return the inverse
}
