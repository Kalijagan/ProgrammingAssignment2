## 'makeCacheMatrix' function creates a special object.
## Within that object, we have a matrix object(matrixObj),its inverse object(inverse) and
## methods to get and set both matrix and its inverse.
## On execution it returns,a list containing the handles to the methods which 
## manipluate the matrix and its inverse.

## We can compare "makeCacheMatrix" to a concept called "class"
## where 1) 'makeCacheMatrix' is similar to constructor function(and class defination)
##       2)  matrix(matrixObj) and its inverse(inverse) can be compared to data members of the class
##       3)  set,get,setInverse,getInverse can be compared to member function of the class
##       4)  The return value of the 'makeCacheMatrix' can be compared to object of the
##          class(not literally).But they are the only way to make changes to the matrix object. 

## get and getInverse methods return the matrix(matrixObj) and invervseof matrix respectively.
## setInverse uptades the cacheedInverse value calculated inverse which we get using the solve function. 
## set function updates the matrix(matrixObj).After updating it makes inverse value to 'NULL' since the original matrix(matrixObj) is updated

makeCacheMatrix <- function(matrixObj = matrix()) {
	    inverse <- NULL
        set <- function(newMatrix) {
                matrixObj <<- newMatrix
                inverse <<- NULL
        }
        get <- function() matrixObj
        setInverse <- function(calculatedInverse) {
        	inverse <<- calculatedInverse
        }
        getInverse <- function() {
        	inverse
        }
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## 'cacheSolve' function tacken in the special matrix object(cached matrix object) and other relevant 
## arguments(right-hand-side vector/matrix,tolerance etc).It first checks if there is an inverse of matrix(using getInverse method) 
## availble in the object(cache). If there, it just returns that value, else it uses 'solve' function(with or without additional paramenters) 
## for calculating the inverse of the matrix.After calculating it stores the calculated inverse in the matrix object(cache) using
## setInverse method and returns the inverse. 

cacheSolve <- function(cachedMatrixObj, ...) {
        inverse <- cachedMatrixObj$getInverse()
        if(!is.null(inverse)) {
                message("fetched cached inverse of the matrix")
                return(inverse)
        }
        cachedMatrix <- cachedMatrixObj$get()
        inverse <- solve(cachedMatrix, ...)
        cachedMatrixObj$setInverse(inverse)
        inverse
}
