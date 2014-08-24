#This function creates a special "matrix" object that can cache its inverse
#
#	The steps performe are following:
#	A-set the value of the vector
#	B-get the value of the vector
#	C-set the value of the mean
#	D-get the value of the mean
#	The matrix supplied should be invertible.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {			# (A)
                x <<- y
                m <<- NULL
        }
        get <- function() x			#(B)
        setinv <- function(inv) m <<- inv	#(C)
        getinv <- function() m			#(D)
        list(set = set, get = get,		#Setting a list of functions
             setinv = setinv,
             getinv = getinv)
}


#The following function calculates the inverse of the special "matrix" created with 
#the function "makeCacheMatrix". However, it first checks to see if the inverse has
#already been calculated. If so, it gets the inverse from the cache and skips the 
#computation. Otherwise, it calculates the inverse of the data and sets the value of 
#the inverse in the cache via the setinv function

cacheSolve <- function(x, ...) {
        m <- x$getinv()				#Getting the inverse function value
        if(!is.null(m)) {			#Checking if the value had beed already compute
                message("getting cached data")
                return(m)
        }
        data <- x$get()				#If the value is not in the cache, The program calculate the inverse
        m <- solve(data, ...)
        x$setinv(m)
        m
}
