## Put comments here that give an overall description of what your
## functions do
#The two functions
#makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
#cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and it checks that the matrix has not changed), then the cachesolve  retrieves the inverse from the cache and prints it.
#In case the matrix has changed or the inverse wasnt calculated earlier, it is calculated and stored in the cache and printed.
#Messages are displayed (not necessary for running the code but mostly for understanding) as to which path the catchSolve function has taken.These are only indicative.




## Write a short comment describing this function

#The makeCacheMatrix takes in a matrix as a parameter and returns a special 
#matrix. The so -called "special matrix" is actually a list containing
#functions to:
# 1)set the value of the matrix  [set()]
# 2)get or fetch the value of the matrix  [get()]
# 3)set or assign the value of the inverse of the matrix [setinverse()]
# 4) get the value of the inverse [getinverse()].


makeCacheMatrix <- function(x = matrix()) {
 		m <- NULL
        	set <- function(y) {
                x <<- y
                m <<- NULL
                
       	 }
        	get <- function() x
       	 setinverse <- function(inv) m <<- inv
       	 getinverse <- function() m
        	list(set = set, get = get,
             	setinverse = setinverse,
             	getinverse = getinverse)

}


## Write a short comment describing this function
#The cacheSolve function takes a list of functions as its arguments.
#This list is the 'special matrix' returned by the function above(makeCacheMatrix)
#The function calculates the inverse of the above specified matrix.
#However, it first checks to see if the inverse has already been calculated(is not null). 
#If so, it again checks if the inverse is that of the matrix specified(nested if). If it indeed the inverse is that of the matrix, 
#it gets the inverse from the cache and skips the computation and prints it. If the matrix and the inverse do not match(as inverses) 
#then the function recalculates the inverse of the set matrix and prints it.And, in case no inverse has been calculated,
#it calculates the inverse of the matrix and sets the value of the inverse in the cache via the setinverse function.It also prints the inverse.
#The function also prints some messages that are only written to make the code comprehendible and these can be done without.


cacheSolve <- function(x, ...) 
{
     		 mat <- x$get()
             m <- x$getinverse()
		
       	 if(!is.null(m)) 	
			{
	              if(isTRUE(all.equal(m%*%mat,diag(nrow(mat)))))
      		          {
                			message("the matrix hasn't changed, getting cached data")
                			x$getinverse
      				return(m) 
				    }
      		  else 
				    {
   				      message ("matrix has changed, computing new inverse")
  				      new_inverse <-solve(x$get())
					x$setinverse(m)
                      		x$getinverse
   				      return(new_inverse)
				    }
			} 
   		 else { 
                      message ("no data in cache")
			    m <- solve(mat)
        		    x$setinverse(m)
                      x$getinverse
       		    return(m)
      		}
         

}
