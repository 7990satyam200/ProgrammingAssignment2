##  The cachematrix.R file contains two functions, makeCacheMatrix()
##  and cacheSolve(). The first function in the file, makeCacheMatrix()
##  creates an R object that stores a matrix and its inverse 
##  The second function, cacheSolve() requires an argument that is 
##  returned by makeCacheMatrix() in order to retrieve the inverse from the 
##  cached value that is stored in the makeCacheMatrix() object's environment.



##  makeCacheMatrix: This function creates a special "matrix" 
##  object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    i<-NULL
    setMat<-function(y){
        x<<-y
        i<<-NULL
    }
    getMat<-function() x
    
    setMatInv<-function(inv) i<<-inv
    getMatInv<-function() i
    list(setMat=setMat, getMat=getMat, 
         setMatInv=setMatInv, getMatInv=getMatInv)
    
}


##  cacheSolve: This function computes the inverse of the special 
##  "matrix" returned by makeCacheMatrix above.If the inverse has
##  already been calculated,then the cachesolve retrieves the inverse
##  from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i<-x$getMatInv()
    if(!is.null(i)){
        message("getting cached data")
    }
    data<-x$getMat()
    if(det(data)!=0){
        i<-solve(data, ...)
        x$setMatInv(i)
        i
    }
    else{
        print("matrix is non-invertible")
    }
}
