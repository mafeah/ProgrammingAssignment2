#Overall Descrption: The function "makeCacheMatrix" is creating a matrix that the user inputs when they type the function makeCacheMatrix(x=matrix(c())).
#Please note that we need to assume that the matrix input by the User will be invertible.We will be using the function Solve to find the inverse matrix.

##first function:the variable "m" is created with a NULL value initially. Then, we create the "set" and "get" value of the matrix.We also create "setinverse1" and "getinverse1" to find the store values of the matrix.

makeCacheMatrix <- function(x = matrix()) {
            m<-NULL
            set<-function(y){
                x<<-y
                m<<-NULL
            }
            
            get<-function()x
            setinverse1<-function(inverse1) m<<-inverse1
            getinverse1<-function() m
            list(set = set, get = get,
                 setinverse1 = setinverse1,
                 getinverse1 = getinverse1)
            
}


##this function cacheSolve is used to compute the inverse of the matrix the user inputs using the function named makeCacheMatrix. If the inverse has already been calculated, it does not compute it. If not, it can calculate it using the setinverse1 function we created in the above function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse1()    
    if(!is.null(m)){
        message("value is cached data")
        return(m)
        }
    data <- x$get()
    m <- solve(data,...)
    x$setinverse1(m)
    m
}

#end

