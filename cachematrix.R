## Put comments here that give an overall description of what your
## function do. 
## makeCacheMatrix creates a list that contains the matrix itself, 
## 2 elements to help the second function calculate the inverse (set and setInverse) and
## a final element (getInverse) that caches the Inverse of the Matrix in memory.
## The second Function checks the elements in the first list to see if 
## calculating the matrix is necessary. And if it is, checks if that Inverse exists.
## once calculated, it will store the matrix (getInverse) 

## Write a short comment describing this function

makeCacheMatrix <- function(X = matrix()) {
## This function starts setting the result of the Inverse as NULL.
  M <- NULL 

  ## Then set function allows you to pass a new Matrix to the special "Matrix" list 
  ## the superassign operator ensures X can be accessed after the call of this function,
  ## by the cacheSolve function.  
  set <- function(Y) {
    X <<- Y
    M <<- NULL
  }
  
  ## get is the element with the original Matrix 
  get <- function() X
  
  ## setInverse assigns the Inverse of the Matrix to M, when 
  ## the inverse is calculated, by the cacheSolve Function.
  setInverse <- function(inverse) M <<- inverse
  
  ## getInverse Stores this Inverse function into memory.
  ## so it can be recalled later.
  getInverse <- function() M
  
  ## A list is generated with all the functions 
  ## get and set, take and store the original matrix to be Inversed.
  ## getInverse and setInverse, grab and store the inverse of the original matrix. 
  list(set= set, get = get, setInverse = setInverse, getInverse = getInverse)
}



## Write a short comment describing this function

cacheSolve <- function(X, ...) {
## First this function stores the Inverse of the Matrix in Matrix
## if the Inverse was never calculated then this will pass a NULL to M
  M <- X$getInverse()
  
## Check if Matrix Inverse can be calculated
	## Checks if matrix is square, if not return message.
  if(ncol(X$get())!=nrow(X$get()))
    return(message(paste("Matrix must be squared, can't calculate Inverse of a matrix of ",nrow(X$get())," x ",ncol(X$get()),sep = "")))

	## Check if matrix has singular value and hence has inverse, if not return message.
  if(det(X$get())== 0)
    return(message("this matrix does not have Inverse because singular value is 0"))

## Check if Inverse was previously calculated. if so, return the calculated matrix. 
if(!is.null(M)) {
    message("getting cached data")
    return(M)
  }
  
## If not, calculate the inverse of the Matrix, using function solve,
##  and pass it to setInverse of the Special Matrix, list. 
## this will store the Inverse in getInverse element.      
  data <- X$get()
  M <- solve(data, ...)
  X$setInverse(M)
  ## Return a matrix that is the inverse of 'x'
  M

  }

## Run tests

## Store and calculate inverse of a matrix 
## makeCacheMatrix(matrix(c(1,2,1,3), nrow= 2, ncol= 2))->X
## cacheSolve(X)

## Try to calculate a non squared matrix
## makeCacheMatrix(matrix(c(1,2,2,3,4,9,4,3,2,1,2,3), nrow= 3, ncol= 4))->X
## cacheSolve(X)

## Try to calculate a matrix with no Inverse value
## makeCacheMatrix(matrix(c(0,0,1,0), nrow= 2, ncol= 2))->X
## cacheSolve(X)
