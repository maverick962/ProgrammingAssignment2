## functions do
## makeCacheMatrix stores a matrix X in memory
## cacheSolve shows the inverse of a matrix if is in memory or computes the inverse and then shows the inverse

## makeCacheMatrix uses scoping rules and stores matrices in memory
## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(X = matrix()) {
inverse <- NULL
set <- function(Y){
	X <<- Y
	inverse <<- NULL
	}
get <- function() X
setinverse <- function(Inverse) inverse <<- Inverse
getinverse <- function() inverse
list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}

## cacheSolve uses corpcor, a library that avoids determinants and uses orthogonal descomposition
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
## note: this function will attempt to load the corpcor library. If it's not installed will try to install the library

cacheSolve <- function(X, ...) 
## Return a matrix that is the inverse of 'x'
{
if(require("corpcor")){
	print("corpcor is loaded correctly")
	} else {
		print("trying to install corpcor")
		install.packages("corpcor")
		if(require(corpcor)){
			print("corpcor installed and loaded")
			} else {
			stop("could not install corpcor")
			}
		}
inverse <- X$getinverse()
if(!is.null(inverse)){
	message("matrix is in memory")
	return(inverse)
	}
message("inverse is not in memory!")
data <- X$get()
inverse <- pseudoinverse(data, ...)
X$setinverse(inverse)
inverse
}


## Test of the square matrix
X <- matrix(rpois(25,3), nrow = 5)
cX <- makeCacheMatrix(X)
cX$get()
cacheSolve(cX)
cacheSolve(cX)
invX <- cacheSolve(cX)

## Test of rectangular matrix rows > cols
Y <- matrix(rpois(20,2), nrow = 5, ncol = 4)
cY <- makeCacheMatrix(Y)
cY$get()
cacheSolve(cY)
cacheSolve(cY)
invY <- cacheSolve(cY)

## Test of rectangular matrix rows < cols
Z <- matrix(rpois(20,1), nrow = 4, ncol = 5)
cZ <- makeCacheMatrix(Z)
cZ$get()
cacheSolve(cZ)
cacheSolve(cZ)
invZ <- cacheSolve(cZ)

## Test of multiplication returns identity or closer
invX %*% X 
X %*% invX
invY %*% Y 
Z %*% invZ 