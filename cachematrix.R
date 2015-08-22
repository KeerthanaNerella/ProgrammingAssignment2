##The functions below are used to create a special object 
##that stores a numeric matrix and caches it's inverse


## The function makeCacheMatrix creates a list containing functions to
## set the value of the matrix - set
## get the value of the matrix - get
## set the value of the inverse of the matrix - setInverse
## get the value of the inverse of the matrix - getInverse

makeCacheMatrix <- function(x = matrix()) {
	
	inverseMatrix<-NULL

	set <-function(m)
		{
			##caching the matrix value using <<- operator	
			x<<-m

			## Initially value of inverseMatrix will be null
			inverseMatrix<<-NULL
		}

	get <-function()
		{
			##retrieving the matrix value
			 x
		}

	setInverse<-function(inverseM)
		{
			##caching the inverseMatrix value using <<- operator	
			 inverseMatrix<<-inverseM
		}

	getInverse <-function()
		{
			##retrieving the inverseMatrix value
			 inverseMatrix
		}

	##creating a list whose elemets are functions to set and get matrix and inverse matrix values
	list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}


## The function cahceSolve takes a matrix as a parameter and
## calculates the inverse of this matrix
## However, if the inverse of the matrix exists in the cache,then
## it retrieves this inverse from the cache and returns the same


##Note:1)Assuming matrix being passed is invertible
##         The matrix passed to cacheSolve function is the resulting list of makeCacheMatrix function

cacheSolve <- function(x, ...) 
{
	##retreiving the inverse of the Matrix from the Cache
	##if inverse exists in the cache,inverse value is returned,otherwise null value is returned        	
	
	inverseMatrix<-x$getInverse()

	##checking if the inverse of the given matrix is in the Cache
	if(!is.null(inverseMatrix))
	{
		message("getting Matrix Inverse from the Cache")
		return(inverseMatrix)
	}
	
	##retrieving matrix value to calculate inverse
	data<-x$get()
	
	
	##An invertible matrix can either be a square or rectangular matrix(called pseudo Inverse)
	##solve() function calculates the inverse of a square matrix and otherwise throws an error
	##Reference: http://www.statmethods.net/advstats/matrix.html

	##For pseudoInverse calculation, we can make use of ginv() function from MASS package
	##Reference: https://stat.ethz.ch/R-manual/R-devel/library/MASS/html/ginv.html

	##As we are using solve() to calcualte Matrix Inverse for this assignment,
	##To avoid the error we check if the given matrix is a Square Matrix
	if(nrow(data)==ncol(data))
	{
	        inverseMatrix<-solve(data,...)
	        x$setInverse(inverseMatrix)      
	}
	else
	{
	        stop("Please provide a Square Matrix to calculate the Inverse")
	}
	inverseMatrix
}

##Sample Test Cases to check the functionality:

## Sample Input: 1
## m<-matrix(c(1,2,3,4),2,2)
## cm<-makeCacheMatrix(m)
## cacheSolve(cm)


## Sample Output: 1
##      [,1] [,2]

## [1,]  -2  1.5
## [2,]   1  -0.5



## Sample Input: 2
## cacheSolve(cm)


## Sample Output: 2
## getting Matrix Inverse from the Cache
##      [,1] [,2]

## [1,]  -2  1.5
## [2,]   1  -0.5



## Sample Input: 3
## m<-matrix(c(7,0,-3,2,3,4,1,-1,-2),3,3)
## cm<-makeCacheMatrix(m)
## cacheSolve(cm)

## Sample Output: 3
##      [,1]  [,2]  [,3]

## [1,] -2     8    -5
## [2,]  3    -11    7
## [3,]  9    -34    21


## Sample Input: 4
## cacheSolve(cm)

## Sample Output: 4
## getting Matrix Inverse from the Cache
##      [,1]  [,2]  [,3]

## [1,] -2     8    -5
## [2,]  3    -11    7
## [3,]  9    -34    21

