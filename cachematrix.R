## This script contains 2 functions : makeCacheMatrix and cacheSolve. 
##makeCachematrix is used to create the matrix and set initial values used for caching the matrix, x must be in matrix form

## EXAMPLE CODE:
##Test=makeCacheMatrix(x=matrix(c(1,0,5,2,1,6,3,4,0),3,3))
##cacheSolve(Test)

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  #get the data stored in x
  get <- function() x
  #store the inverse matrix in m
  setinv <- function(inv) m <<- inv
  #get data stored in m
  getinv <- function() m
  
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}

## cacheSolve will return a matrix that is the inverse of 'x'
##the input for cache solve is a matrix

cacheSolve <- function(x, ...) {
  #returns what is stored in m
  m<-x$getinv()
  #if what is stored in m is not Na then m is returned as this is the already calculated inverse matrix
  if(!is.null(m)){
    message("getting cached data")
    return(m)
    #print(m)
  }
  #if m is NA then the inverse of the matrix inputed in makeCacheMatrix is calculated; get() retrieves this data from the input of makeCacheMatrix
  data<-x$get()
  #calculates inverse
  m<-solve(data,...)
  #chaches the inverse matrix which is now stored in inv
  x$setinv(m)
  m
  
}
