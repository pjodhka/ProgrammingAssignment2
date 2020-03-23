## Calculating the inverse of matrix and saving the inverted matrix in cache
## Avoid repeating same inverse calculations to save time
## Using <<- to assign variable within enclosed function (lexical scoping)



## input square invertible matrix x to the makeCacheMatrix function which stores the said values in cache
## example f<- makeCacheMatrix(x) will return list of functions as created
makeCacheMatrix <- function(x = matrix()) {

  
  ## initialising cache to NULL 
  ## which will then save the value of the inverted matrix with cacheSolve function using setinverse function
  cache <- NULL
  
  ##set matrix x in the working environment
  set <- function(y = matrix()){
    x <<- y
    }
  
  ## get the matrix x saved in the working environment
  get <- function() x
  
  ##setting value of the inverted matrix calculated in cacheSolve function to cache
  setinverse <- function(inverse = matrix()) {
    cache <<- inverse
    }
  
  ##get the inverted matrix from cache
  getinverse <- function() cache
  
  ## return list containing values from set,get,setinverse,getinverse functions 
  ## to be used as input to cacheSolve function
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)

}

## calculate and return a matrix that is inverse of the matrix x as created in makeCacheMatrix function above
## example cachesolve(f)
cacheSolve <- function(x, ...) {
  
  ## first check if the inverse matrix is already present in the cache
  cache <- x$getinverse()
  
  ## return inverted matrix already present in cache
  if(!is.null(cache)){
    message("Found in cache")
    return(cache)
  }
  
  ## else calculate inverse with solve and save in cache
  
  ## get data from matrix x saved in the working environment with makeCacheMatrix function
  matrix_data <- x$get()
  
  ##calculate inverse of the matrix and assign to cache
  cache <- solve(matrix_data,...)
  
  ##calling setinverse function to save the inverted matrix in cache and returning its value
  x$setinverse(cache)
  return(cache)
  
}
