## Calculating the inverse of a matrix is very expensive operation, the following functions 
## were created to solve the inverse of the matrix and to cache it in a way
## that is easy to be retreaved by any function. The inverse will be calculated only once 
## and saved inside the object function makeCacheMatrix. 
## Kindly note that we assume in the following code that the matrix assigned is always 
## invertible. 

## This function creates a matrix which is a list containing function that 
##sets the value of the matrix, get the value of the matrix, set the
##inverse of the matrix and gets the inverse of the matrix. 

makeCacheMatrix <- function(x = matrix()) {
    xinverse <- NULL
	set <- function(y){
	    x <<- y
		xinverse <- NULL
	}
	get <- function() x
	setinverse <- function(matinverse) xinverse <<- matinverse
	getinverse <- function() xinverse
	list (set = set, get=get, 
	      setinverse = setinverse, 
		  getinverse = getinverse)
}


## This function calculates the inverse of the matrix created  in "makeCacheMatrix", 
## it checks if the inverse of the matrix has been created, 
## if yes it will return the value, otherwise, it will create it and sets the inverse 
## in the cache via setinverse function. 

cacheSolve <- function(x, ...) {
       
        ## Return a matrix that is the inverse of 'x'
		xinverse <- x$getinverse()
		if (!is.null(xinverse)){
		    message("getting cached matrix inverse")
			return(xinverse)
		}
		data <- x$get()
		xinverse <- solve(data, ...)
		x$setinverse(xinverse)
		xinverse 
}
