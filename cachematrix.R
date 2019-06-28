## This programming Assignment2 creates 2 functions, makeCacheMatrix() and cacheSolve() which 
## together creates an inverse of a given (invertible) matrix for the first time and stores it in 
## the cache. If the original matrix is not changed, subsequent calls to cacheSolve() will not
## recompute inverse but returns the cached inverse matrix itself.


## makeCacheMatrix() function creates an R object that stores a matrix and its inverse. Returns a 
## list with 4 functions (set&get matrix data and set&get its inverse) and 2 data variables for
## matrix (x) and its Inverse (x_Inv) available in the parent environment.

makeCacheMatrix <- function(x = matrix()) {
  
  ## make sure the inverse is initialized to NULL
  
  x_Inv <- NULL
  
  ## New matrix data can be initialized to the 'x' matrix variable in the parent environment and
  ## re-inilializes the Inverse matrix variable to NULL
  
  set_matrix <- function(newMat) {
    x <<- newMat
    x_Inv <<- NULL
  }
  
  
  get_matrix <- function() x                    ## returns the given matrix object 
  
  setInv <- function(inv_mat) x_Inv <<- inv_mat ## cache the 'Inverse' in parent env  
  
  getInv <- function() x_Inv                    ## returns the 'Inverse' (cached/NULL)
  
  list(set_matrix = set_matrix,                 ## returns list with named functions
       get_matrix = get_matrix,
       setInv= setInv,
       getInv = getInv)
  
}


## cacheSolve() function operates on an object of type 'makeCacheMatrix' and computes the 
## 'inverse' matrix for the first call, saves the 'inverse' in cache and if the original matrix 
## is not changed, returns the 'inverse' from cache.

cacheSolve <- function(cmat_Obj, ...) {
        
  ## Retrieve the 'inverse' matrix from parent env
  
  m_Inv <- cmat_Obj$getInv()
  
  ## If the 'inverse' is availabe in cache, return it with a message
  
  if(!is.null(m_Inv)) {
    message("getting inverse matrix from cache")
    return(m_Inv)
  }
  
  ## get original matrix data from the object and compute its inverse
  
  data <- cmat_Obj$get_matrix()
  m_Inv <- solve(data, ...)
  
  ## set the inverse matrix in cache and also return it
  
  cmat_Obj$setInv(m_Inv)
  m_Inv
  
}
