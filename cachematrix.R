## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	invM<-x$getinv()
	if(!is.null(invM))
	{
		message("getting Cached Inverse")
		return(invM)
	}
	data<-x$get()
	invM<-solve(data,...)
	x$setinv(invM)
	invM
}
