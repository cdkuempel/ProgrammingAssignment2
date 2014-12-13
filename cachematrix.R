#function to create a special "matrix" object that can cache its inverse
makeCacheMatrix<- function(x=matrix()){ #input x will be a matrix
  m<<-NULL #m will be our "inverse" and is reset to NULL every time
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function(){x} #returns value of original matrix
  setmatrix<-function(inverse) {m<<-inverse}
  getmatrix<-function(){m} #return the cahced value to cachematrix()
  list(get=get, setmatrix=setmatrix, getmatrix=getmatrix) #list of internal functions so function knows how to access methods
}

cacheSolve<- function(x,...) { #input x ias an oject created by makeCacheMatrix
  m<-x$getmatrix() #accesses the object 'x' and gets the value of the inverse matrix
  if(!is.null(m)){ #if inverse was already cached (not NULL)
    message("getting cached data") #send this message to console
    return(m) #and return the inverse
  }
  data<-x$get() #uses this code only if x$getmean() returned NULL
  m<-solve(data) #if m was NULL then have to calculate the inverse
  x$setmatrix(m) #store the calculated inverse value in x 
  m #return the inverse to the code that called the function
}
