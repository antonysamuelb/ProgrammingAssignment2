## Function to find inverse and access it 
# from Cache if it is readily available


rm(list = ls())
setwd("F:\\1 Studies\\ML\\Coursera\\2 R programming\\wk 3\\hub\\ProgrammingAssignment2")

## Function to get and set special matrix and its inverse
## from and to the Cache
makeCacheMatrix <- function(sp_mtrx = matrix()) {
    
    mtrx_inv <- NULL
    set <- function(y) {
        sp_mtrx <<- y
        mtrx_inv <<- NULL
    }
    get <- function() sp_mtrx
    setinv <- function(m_inv) mtrx_inv <<- m_inv
    getinv <- function() mtrx_inv
    
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

## Function to find matrix inverse if it has not already been calculated
cacheSolve <- function(x, ...) {
    MI <- x$getinv()
    
    if(!is.null(MI)) {
        message("getting cached Matrix Inverse")
        return(MI)
    }
    M <- x$get()
    MI <- solve(M)
    x$setinv(MI)
    MI
}

## For Console Use! 
set.seed(81)
A <- matrix(sample(100,100),nrow =10, ncol =10)
aCM <- makeCacheMatrix(A)
aCM$get()
aCM$getinv()
aCM$set(A)
cacheSolve(aCM)

