## Code to invert a matrix in a fast manner 


## function to create list of functions that can
## get and set the matrix as well as its inverse

#x1 - matrix
#x2 - inverse of matrix

makeCacheMatrix <- function(x1 = matrix()) {
    
    
    x2 <- NULL
    set <- function(matrx) {
 
        x1 <<- matrx
        x2 <<- NULL
    }

    
    get <- function() x1
    setinv <- function(matinv) x2 <<- matinv
    getinv <- function() x2
    
    
    
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
    
    

}


## solve to obtain inverse of a matrix if inverse has not been calculated. 
## If it has been calculated, load that value.


cacheSolve <- function(x,  ...) {
    
    matrx <- matrix(c(4,2:9),nrow=3,ncol=3)
    
    matrixold <- x$get()
    print(matrixold)
    if(!identical(matrx,matrixold)){
        x$set(matrx)
    }
    
    matinv <- x$getinv()
    if(!is.null(matinv)) {

        message("getting matrix inverse from cached data")
        return(matinv)
    }
    matrx <- x$get()
    matinv <- solve(matrx)
    x$setinv(matinv)
    
}
