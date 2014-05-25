## To accelerate the calculation speed, caching ptentially time-consuming computations 
## is a good way. This R script contains two main functions "makeCacheMatrix" and "cacheSolve".
## It aims at solving the inverse matrix of a given invertible matrix by adopting cache. This
## can avoid computing same calculation repeatedly, which can save time.

## The purpose of makeCacheMatrix is to create a list containing a function
## to (1) set the value of the matrix, (2) get the value of the matrix, (3) 
## set the inverse matrix, (4) get the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL ##set the null value for m
  set<-function(y){ ##Define the set function
    x<<-y           ##<<- operator is used to assign a value to an object in an
                    ##enviornment that is different from the current environment.
    m<<-NULL
  }
  get<-function() x ## Get the original matrix
  setinverse<-function(solve) m<<- solve ##using solve function to set the inverse matrix
  getinverse<-function() m ##get the inverse matrix
  list(set=set, get=get, ##create a list which contains four functions
       setinverse=setinverse,
       getinverse=getinverse) 
}


## cacheSolve function is used to calculate the inverse matrix. It has two functions:
## (1)check if the same calculation (get inverse matrix) has been performed. if the same 
## calculation has been done before and the results have been stored in cache, this function 
## will extract the results without calculating again which can save time in computing. (2)
## calculate the inverse matrix and set the inverse matrix iva the setinverse function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m<-x$getinverse() ##obtain and assign the inverse matrix to m
  if(!is.null(m)){ ##judge if the m is null
    message("getting cached data")
    return(m)
  }
  matrix<-x$get() 
  m<-solve(matrix, ...) ##calculate the inverse matrix and assign the value to m
  x$setinverse(m) ##call setinverse function as introduced in makeCacheMatrix
  m ##Return m, the inverse matrix
}