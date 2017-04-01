## Functions makeCacheMatrix and cacheSolve can be used to create a cached matrix objects that 
## are then used in calculation of the inverse matrix. The result of the calculation is stored
## in the cache object and if function cacheSolve is called again for the same object
## the cached result is returned.
##
## USAGE EXAMPLE:
##
## 1) Generate random matrix:
## > a <- replicate(5, rnorm(5))
## 2) Load the cachematrix.R code (assuming you are in your working directory):
## > source("cachematrix.R")
## 3) create a cache object to store the matrix object:
## >  cache<-makeCacheMatrix(a)
## 4) Calculate inverse matrix on the cached object
## >  cacheSolve(cache)
## >  calculating inverse matrix
## >              [,1]        [,2]       [,3]       [,4]        [,5]
## >  [1,]  0.13265071 -0.34317850 -0.7928498  0.3656069 -0.49149435
## >  [2,]  0.35482905 -0.41309832 -0.3764415  0.7557096 -0.48784028
## >  [3,] -0.45272635 -0.12520865  0.5231039 -0.6116363 -0.03043617
## >  [4,]  0.08301598 -0.02220867 -0.3742510  0.5671443 -0.47121107
## >  [5,] -0.07456763 -0.45104604 -0.4622572  0.6219293 -0.22350534
##
## 5) Attempt to calculate inverse matrix on the same cached object:
## >  cacheSolve(cache)
## >  getting cached matrix
## >              [,1]        [,2]       [,3]       [,4]        [,5]
## >  [1,]  0.13265071 -0.34317850 -0.7928498  0.3656069 -0.49149435
## >  [2,]  0.35482905 -0.41309832 -0.3764415  0.7557096 -0.48784028
## >  [3,] -0.45272635 -0.12520865  0.5231039 -0.6116363 -0.03043617
## >  [4,]  0.08301598 -0.02220867 -0.3742510  0.5671443 -0.47121107
## >  [5,] -0.07456763 -0.45104604 -0.4622572  0.6219293 -0.22350534

## makeCacheMatrix function takes a matrix as its argument and 
## constructs methods to manipulate cached objects:
## set the value of the matrix 
## get the value of the matrix
## set the value of the inverse matrix
## get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)

}


## cacheSolve function takes a cached objects as its arguments 
## before solving an inverse matrix it checks if inverse matrix 
## is set for the provided cached object
## if cached inverse matrix exists it returns its value
## otherwise it calculates the inverse matrix and stores it in cached object

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)){
                message ("getting cached matrix")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data,...)
        x$setinv(inv)
        message ("calculating inverse matrix")
        inv
}
