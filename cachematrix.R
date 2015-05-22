## Example data for use with this method
## 3 x 3 invertible, square matrix: d <- matrix(c(1,1,4,0,3,1,4,4,0),3,3)
##
##      [,1] [,2] [,3]
## [1,]    1    0    4
## [2,]    1    3    4
## [3,]    4    1    0
## function to create an inverted matrix solve(d)
## Result example
##              [,1]        [,2]    [,3]
##  [1,]  0.08333333 -0.08333333  0.2500
##  [2,] -0.33333333  0.33333333  0.0000
##  [3,]  0.22916667  0.02083333 -0.0625
##
## makeCacheMatrix function returns a list of functions needed to cache
## an inverted matrix. This function is basically a list of get\set functions
## to store the base matrix and the inversion of that matrix
makeCacheMatrix <- function(baseMatrix = matrix()) {
        
        ## Every new call to the makeCacheMatrix function initializes the 
        ## inverted matrix to a null value.
        invertedeMatrix <- NULL
        
        ## This set function provides a means to set a new matrix without
        ## needing to call the makeCacheMatrix again. There are basically two 
        ## ways to set the base matrix value, one is to call the makeCacheMatrix
        ## function passing in a matrix as parameter, 
        ## k <- makeCacheMatrix(d)
        ## and the second is to call the set function from the returned list of 
        ## the k <- makeCacheMatrix()
        ## The default creates an empty matrix which is replaced by the 
        ## parameter passed in with the set
        ## k$set(d)
        set <- function(newMatrix) {
                
                ## set the lexical storage values, this preserves or caches
                ## the value in higher level environment, unlike local
                ## variables however, this will not be destroyed when the 
                ## function ends and the variable goes out of scope.
                ## The use of the superassignment operator (<<-) allows for
                ## the writing to variables in higher level environments.
                ## Here the new matrix is preserved in this manner.
                baseMatrix <<- newMatrix
                
                ## setting this to null indicates the base matrix has changed
                ## and that the inverted matrix needs to be re calculated.
                ## Again we store this lexically in a higher level environment.
                invertedMatrix <<- NULL
        }
        
        ## Simple get function to return the stored matrix value (not inverted)
        get <- function() baseMatrix
        
        ## Set function to update the matrix stored in higher level envrionment.
        ## The value passed in as parameter is expected to be an inverted
        ## matrix, for purpose of this program no error checking is done
        ## all values are assumed valid.
        setinverse <- function(inverseMatrix) inverseMatrix <<- inverseMatrix
        
        ## Get function to return the value in the inverted matrix storage
        getinverse <- function() invertedMatrix
        
        ## A list of functions and name identifiers by which to call them
        ## This is returned by default being the last item in the function.
        ## The list provides a means to access the internal functions within
        ## makeCacheMatrix its public interface if you will.
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)                              

}

## This function is the companion to the makeCacheMatrix and consumes the
## returned list of functions from it. This function then calls methods in the 
## makeCacheMatrix function to either retrieve a cached inverted version of the 
## matrix stored or create it using the solve() function.
cacheSolve <- function(makeCacheMatrixList, ...) {
  
        ## Start by trying to retrieve the cached matrix using the getinverse()
        ## function. Note this calls the getinverse() using the passed in list
        ## of functions.
        invertedMatrix <- makeCacheMatrixList$getinverse()
        
        ## If the invertedMatrix value retrieved is not null then post a message
        ## to the console that it was retrieved from the cached data
        if(!is.null(invertedMatrix)) {
                message("getting inverted matrix from cached data")
                
                ## Now return the cached inverted matrix and we are done.
                ## The return, explicitly returns the value and exits.
                return(invertedMatrix)
        }
        
        ## If the value of the invertedMatrix was NULL get the stored matrix
        data <- makeCacheMatrixList$get()
        
        ## Here we create the inverted matrix by passing in the matrix 
        ## retrieved using the get function and passing it to a solve()
        ## function, the result is then stored locally in the 
        ## cacheSolve function.
        invertedMatrix <- solve(data, ...)
        
        ## Next the local invertedMatrix is stored in the cache of the 
        ## makeCacheMatrix utilizing a call to the setinverse() function
        ## and passing in the locally stored value.
        ## This now caches the inverted matrix value and sets a relationship
        ## between the matrix and the inverted matrix that will track when
        ## the matrix has changed and needs to have the inverted value 
        ## recalculated.
        makeCacheMatrixList$setinverse(invertedMatrix)
        
        ## Lastly we implictly return the invertedMatrix 
        ## by listing it as the last thing in the function.
        invertedMatrix
  
}
