## This function creates a 'special' matrix list of functions for setting
## and getting the input matrix as well as setting and getting its inverse

makeCacheMatrix <- function(x = matrix()) {
	## First we must ensure the the testm matrix is cleared
	testm<-matrix()
	## this function sets the matrix variable x and its inverse testm
	set<-function(y){
		x<<-y
		testm<<-matrix()

	}
	## this function returns the matrix variable x
	get <- function() x
	## this function sets the inverse matrix variable testm
	setinv<-function(new_testm) testm<<-new_testm
	## this function returns the inverse matrix variable testm
	getinv<-function() testm
	## this is the list of functions returned by makeCacheMatrix
	list(set=set,get=get,setinv=setinv,getinv=getinv)
}

## This function takes in a 'special' matrix from makeCacheMatrix and returns
## its inverse. If the calculation was done already it will return the result 
## of the previous calculation otherwise it will return the result of the
## calculation done now

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	## this sets testma to be the inverse of the 'special' x matrix
	testma<-x$getinv()
	## if the first variable in the matrix is not NA then we know the calc 
	## was done before and its result will be returned exiting the function
	if(!is.na(testma[1.1])){
		message("getting cashed data")
		return(testma)
	}
	## sets data to the matrix in the 'special' matrix x
	data<-x$get()
	## calculates the inverse of the matrix
	testma<-solve(data,...)
	## sets the inverse just calculated to the inverse in the 'special' matrix x
	x$setinv(testma)
	## returns the inverse calculated
	testma
	
	
}