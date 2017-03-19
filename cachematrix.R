## Assignment: Caching the Inverse of a Matrix
## Matrix inversion is usually a costly computation
## and there may be some benefit to caching the inverse of a matrix
## rather than compute it repeatedly
## (there are also alternatives to matrix inversion that we will not discuss here).
## Your assignment is to write a pair of functions that cache the inverse of a matrix.

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
			#set inv variable to null
			inv <- NULL
			
			#define set function
			set <- function(x){
				x <<- y
				inv <- NULL
			}
			
			#define get function
			get <- function(){
				x
			}

			#define setinversion function
			setinv <- function(inversion){
				inv <<- inversion
			}

			#define getinversion function
			getinv <- function(){
					inv
			}
			
			#display the list of functions
			list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve should retrieve
## the inverse from the cache.

cacheSolve <- function(x, ...) {
			## Return a matrix that is the inverse of 'x'

			#get inverse
			inv = x$getinv()

			#first checks to see if the inverse has already been calculated. 
        		if (!is.null(inv)){
                		#retrieve the inverse from the cache and skip the calculation
                		message("getting cached data")
                		return(inv)
        		}
        
        		#calculate the inverse of x
        		dataset = x$get()
        		inv = solve(dataset, ...)
        
        		#sets inv to cache
        		x$setinv(inv)

        		#return inverse dataset
		      return(inv)
}

