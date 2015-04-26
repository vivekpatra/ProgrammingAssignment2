################################################################################
## Two functions here, namely makeCacheMatrix() and cacheSolve(), demonstrate
## how we could cache a computationally expensive object for later use
## exploiting the scoping rules of R proramming language.
##
##
## Typical usage of these two functions is here:
## > a <- matrix( c( 5, 10, 66, 77, 23, 44, 43, 11, 22 ), nrow = 3, ncol = 3 )
## > mat_obj <- makeCacheMatrix( a )
## > mat_obj$getMatrix()
##      [,1] [,2] [,3]
## [1,]    5   77   43
## [2,]   10   23   11
## [3,]   66   44   22
## > cacheSolve( mat_obj )
## [1] "Generate inverse of the matrix..."
##              [,1]        [,2]        [,3]
## [1,] -0.003021148 -0.02719033  0.01950014
## [2,] -0.069486405  0.37462236 -0.05149684
## [3,]  0.148036254 -0.66767372  0.08994782
## > cacheSolve( mat_obj )
## [1] "Get the inverse from cache..."
##              [,1]        [,2]        [,3]
## [1,] -0.003021148 -0.02719033  0.01950014
## [2,] -0.069486405  0.37462236 -0.05149684
## [3,]  0.148036254 -0.66767372  0.08994782
################################################################################

################################################################################
## Purpose:
## Create special matrix objet having it's own inverse matrix. To manipulate this
## objet there are list of methords below:-
##                      setMatrix() - Update the matrix of the object; Also check
##                                    valid square matrix 
##                      getMatrix() - Get the matrix from the object
##                      setInverse() - Set the inverse matrix on the object 
##                      getInverse() - Get the inverse matrix from the object
## Input object(s):
##      x: input matrix
##
## Return object:
##      a list of all functions to manipulate on the matrix and inverse
##
################################################################################
makeCacheMatrix <- function(x = matrix()) {
	
	#Initialize inverse matrix
	invrsMatrx <- NULL

	# Set matrix methord 
	setMatrix <- function(y) {

		# Validate the matrix
		if( !is.matrix(y) ) {
		    print( "Set fail: not a matrix" )
		    return( NULL )
		}
		
		# Checking square matrix
		if( nrow(y) != ncol(y) ) {
		    print( "Set fail: not a square matrix" )
		    return( NULL )
		}
		
		# Update matrix object on own environment
		x <<- y
		
		# Reset inverse of matrix object on own environment
		invrsMatrx <<- NULL
	}

	# Get matrix methord
	getMatrix <- function() {
		x
	}

	# Set inverse matrix
	setInverse <- function(matrix) {
		invrsMatrx <<- matrix
	}

	# Get inverse matrix 
	getInverse <- function() {
		invrsMatrx
	}
	
	# List of all methord to the outer environment
	list(setMatrix = setMatrix, getMatrix = getMatrix,
		 setInverse = setInverse, getInverse = getInverse )
}

################################################################################
## Purpose:
## Returns the inverse of the special "matrix" object which is created by
## makeCacheMatrix() function. It first ask for the inverse from the cache. If
## the cache contains the inverse then returns the inverse from the cache. If
## the cache does not contain the inverse then calculates the same, set in the
## cache and retuns to the user.
##
##
## Input object(s):
##      x: special "matrix" object returned by makeCacheMatrix()
##
## Return object:
##      Inverse of input matrix x or NULL if objet does not have inverse 
################################################################################
cacheSolve <- function(x, ...) {

    # Get the inverse matrix
    inverse <- x$getInverse()
    
    # Inverse is not NULL: means we are getting inverse from cache
    if( !is.null( inverse ) ) {
        print( "Get the inverse from cache..." )

		# Return the cached inverse matrix 
        return( inverse )
    }
    
    # So, inverse is not there in cache, calculate, set in cache and return
    print( "Generate inverse of the matrix..." )

	# Checking determinant for matrix is inversible
	if( det( x$getMatrix() ) == 0 ) { 
    	print( "This matrix objet does not have inverse matrix" )
        return( NULL )
    }

	# Calculate inverse by using R solve() function 
    inverse <- solve( x$getMatrix() )

	# Update inverse matrix on object
    x$setInverse( inverse )
    
	## Return a matrix that is the inverse of 'x'
    inverse
}
