makeCacheMatrix <- function(x = matrix()) {
  invMat <- NULL
  setMat <- function(y) {
    x <<- y
    invMat <<- NULL
  }
  getMat <- function() x
  setInv <- function(invM) invMat <<- invM
  getInv <- function() invMat
  list(setMat = setMat, getMat = getMat,
       setInv = setInv,
       getInv = getInv)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  invMat<-x$getInv()
  if(!is.null(invMat)){
    message("Getting the inverse of the matix from the cache")
    return(invMat) }
  mat <-x$getMat()
  invMat<-solve(mat,...)
  x$setInv(invMat)
  ## invMatrix return a matrix that is the inverse of 'x'
  invMat
}

   


