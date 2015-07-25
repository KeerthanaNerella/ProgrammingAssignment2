##The functions below are used to create a special object 
##that stores a numeric matrix and caches it's inverse


## The function makeCacheMatrix creates a list containing functions to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse of the matrix
## get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
	
	invM<-NULL

	set <-function(m)
		{
			x<<-m
			invM<<-NULL
		}

	get <-function()
		{
			 x
		}

	setinv <-function(inv)
		{
			 invM<<-inv
		}

	getinv <-function()
		{
			 invM
		}

	list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## The function cahceSolve takes a matrix as a parameter and
## calculates the inverse of this matrix
## However, if the inverse of the matrix exists in the cache,then
## it retrieves this inverse from the cache and returns the same

##Note: Assuming matrix being passed is invertible

cacheSolve <- function(x, ...) 
{
        ##checking if the inverse of the given matrix is in the Cache
	invM<-x$getinv()
	if(!is.null(invM))
	{
		message("getting Matrix Inverse from the Cache")
		return(invM)
	}
	
	data<-x$get()
	
	##checking if the given matrix is a Square Matrix
	if(nrow(data)==ncol(data))
	{
	        invM<-solve(data,...)
	        x$setinv(invM)
	        
	}
	else
	{
	        stop("Please provide a Square Matrix to calculate the Inverse")
	}
	invM
}



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

