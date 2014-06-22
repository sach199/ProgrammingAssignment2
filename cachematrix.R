## functions to cache the inverse of a matrix

## Creating a special matrix

makeCacheMatrix <- function( m = matrix() ) {

    in <- NULL
    setter <- function( matrix ) {
            m <<- matrix
            in <<- NULL
    }

    
    getter <- function() {
    	## Return
    	m
    }

    ## Set the inverse of the matrix
    setInverse <- function(inverse) {
        in <<- inverse
    }

    ## get the inverse
    getInverse <- function() {
        ## Return
        in
    }

   
    list(setter = setter, getter = getter,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Find the inverse of the matrix returned by first method
## above. If the inverse has already been calculated
## thenretrieve the inverse from the cache.

cacheSolve <- function(x, ...) {

    ## Return inverse matrix
    im <- x$getInverse()

    
    if( !is.null(im) ) {
            message("cached !!!")
            return(im)
    }

    
    mat <- x$getter()

    ## Calculate the inverse 
    im <- solve(mat) %*% mat

    
    x$setInverse(im)

    ## Return
    im
}
