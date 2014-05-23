## This function sets and gets the value of a matrix
## the function also sets and gets the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {

inmat<- NULL
set<- function(y) {
x<<- y
inmat<<- NULL
}

get<- function() x
setinv<- function(inverse) inmat<<- inverse
getinv<- function() inmat
list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## function computes the inverse of the matrix x
##Function will check to determine if the inverse of the matrix was already calculated
## already computed inverses of the matrix are retrieved from the cache
## if the inverse of the matrix has not been calculated then the function will calculate it and set
## the value of the matrix in the cache using setinv
cacheSolve <- function(x) {
        inmat<- x$getinv()
        if(!is.null(inmat)) {
        message("retrieving cached data")
        return(inmat)
        }
        data<- x$get()
        inmat<- solve(data)
        x$setinv(inmat)
        inmat
}
