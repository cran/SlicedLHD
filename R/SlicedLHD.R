om16=function(v)
{
  H=matrix(c(v[1]  ,  -v[2]	,	-v[4]	,	-v[3]	,	-v[8]	,	v[5]	,
             v[2]	,	v[1]	,	-v[3]	,	v[4]	,	-v[7]	,	-v[6]	,
             v[3]	,	-v[4]	,	v[2]	,	v[1]	,	-v[6]	,	v[7]	,
             v[4]	,	v[3]	,	v[1]	,	-v[2]	,	-v[5]	,	-v[8]	,
             v[5]	,	-v[6]	,	-v[8]	,	v[7]	,	v[4]	,	-v[1]	,
             v[6]	,	v[5]	,	-v[7]	,	-v[8]	,	v[3]	,	v[2]	,
             v[7]	,	-v[8]	,	v[6]	,	-v[5]	,	v[2]	,	-v[3]	,
             v[8]	,	v[7]	,	v[5]	,	v[6]	,	v[1]	,	v[4]),ncol=6,byrow=TRUE)
  D=rbind(H,-H)
  return(D)
}

om16r=function(n,r,levels)
{
  if(r==n/16)
  {  
    design=NULL
    for(i in 1:r)
    {
      levels1=levels[(i-1)*8+1:(i*8)]
      design=rbind(design,om16(levels1))
    }       
  } else design="Does not exist"
  return(design)
}

olh1=function(n,m)
{
  if (n%%4 ==2 | n<4) return("OLH does not exist.")
  if (m<2)return ("OLH does not exist")
  if(m>6) return("OLH may exist but not generated using this facility")
  if (n<6 & m>2) return("OLH does not exist.")
  if (n<4 & m==2) return("OLH does not exist.")
  if (n<8 & m==4) return("OLH does not exist.")
  if (n<9 & m==5) return("OLH does not exist.")
  if(n<11 & m<6)
  {
    olh1=matrix(c(-7  ,	-5	,	-3	,	-1	,
                  -5	,	7	,	-1	,	3	,
                  -3	,	1	,	7	,	-5	,
                  -1	,	-3	,	5	,	7	,
                  7	,	5	,	3	,	1	,
                  5	,	-7	,	1	,	-3	,
                  3	,	-1	,	-7	,	5	,
                  1	,	3	,	-5	,	-7),ncol=4,byrow=TRUE)
    if(n==8 & m==4) design=olh1/2
    if(n==8 & m<4) design=(olh1[,1:m])/2
    olh2=matrix(c(-3  ,	3	,	2	,
                  -2	,	0	,	-3	,
                  -1	,	-2	,	-1	,
                  0	,	-3	,	1	,
                  1	,	-1	,	3	,
                  2	,	1	,	-2	,
                  3	,	2	,	0),ncol=3,byrow=TRUE)
    if(n==7 & m==3) design=olh2
    if(n==7 & m<3) design=olh2[,1:m]
    olh3=matrix(c(-4  ,	-2	,	0	,	-3	,	3	,
                  -3	,	4	,	2	,	1	,	-2	,
                  -2	,	-3	,	-4	,	-1	,	-3	,
                  -1	,	3	,	-2	,	3	,	4	,
                  0	,	-4	,	4	,	4	,	0	,
                  1	,	2	,	-1	,	0	,	-4	,
                  2	,	0	,	3	,	-2	,	-1	,
                  3	,	1	,	1	,	-4	,	2	,
                  4	,	-1	,	-3	,	2	,	1	),ncol=5,byrow=TRUE)
    if(n==9 & m==5) design=olh3
    if(n==9 & m<5) design=olh3[,1:m]    
    olh4=matrix(c(-2,-1,1,2,1,-2,2,-1),ncol=2)
    if(n==4 & m==2) design=matrix(c(0.5,-1.5,1.5,0.5,-.5,1.5,-1.5,-.5),byrow=T,ncol=2)
    if(n==5 & m==2) design=rbind(olh4,rep(0,m))
  }  else {  
    levels=seq(-(n - 1)/2,(n - 1)/2,1)
    olh11=matrix(c(-5  ,  -4	,	-5	,	-5	,	-3	,	0	,
                   -4	,	2	,	-1	,	3	,	4	,	5	,
                   -3	,	-2	,	4	,	5	,	-4	,	-2	,
                   -2	,	3	,	-3	,	4	,	1	,	-4	,
                   -1	,	4	,	2	,	-4	,	3	,	2	,
                   0	,	-5	,	5	,	-2	,	5	,	-3	,
                   1	,	5	,	3	,	-3	,	-5	,	-1	,
                   2	,	-1	,	1	,	1	,	-2	,	3	,
                   3	,	0	,	0	,	-1	,	0	,	1	,
                   4	,	1	,	-4	,	0	,	2	,	-5	,
                   5	,	-3	,	-2	,	2	,	-1	,	4	),ncol=6,byrow=TRUE)
    olh12=matrix(c(-11  ,  -11	,	-3	,	-11	,	-7	,	-7	,
                   -9	,	-5	,	-5	,	11	,	9	,	1	,
                   -7	,	9	,	11	,	-9	,	-1	,	3	,
                   -5	,	1	,	1	,	1	,	1	,	11	,
                   -3	,	5	,	-1	,	3	,	11	,	-9	,
                   -1	,	11	,	5	,	7	,	-5	,	-3	,
                   1	,	3	,	-11	,	5	,	-11	,	-5	,
                   3	,	-3	,	3	,	-3	,	3	,	5	,
                   5	,	-9	,	7	,	9	,	-9	,	7	,
                   7	,	-1	,	-9	,	-7	,	7	,	9	,
                   9	,	7	,	-7	,	-5	,	-3	,	-1	,
                   11	,	-7	,	9	,	-1	,	5	,	-11
    ),ncol=6,byrow=TRUE)/2
    olh13=matrix(c(-6  ,  -6	,	-6	,	0	,	-5	,	-1	,
                   -5	,	1	,	4	,	-1	,	6	,	5	,
                   -4	,	6	,	-4	,	5	,	5	,	-2	,
                   -3	,	2	,	6	,	-4	,	-6	,	2	,
                   -2	,	-2	,	2	,	2	,	-2	,	-4	,
                   -1	,	3	,	1	,	1	,	-3	,	3	,
                   0	,	4	,	-2	,	-6	,	1	,	-5	,
                   1	,	-4	,	-5	,	-2	,	3	,	4	,
                   2	,	-5	,	5	,	6	,	2	,	1	,
                   3	,	-3	,	3	,	-5	,	4	,	-6	,
                   4	,	-1	,	0	,	4	,	-1	,	-3	,
                   5	,	5	,	-1	,	3	,	-4	,	0	,
                   6	,	0	,	-3	,	-3	,	0	,	6	
    ),ncol=6,byrow=TRUE) 
    olh15=matrix(c(-7  ,  4	,	-7	,	-6	,	-2	,	-5	,
                   -6	,	3	,	5	,	3	,	5	,	2	,
                   -5	,	-6	,	-2	,	5	,	6	,	-3	,
                   -4	,	1	,	4	,	4	,	-5	,	-2	,
                   -3	,	0	,	-4	,	-7	,	0	,	3	,
                   -2	,	-2	,	3	,	0	,	-4	,	-7	,
                   -1	,	-7	,	6	,	-2	,	-3	,	5	,
                   0	,	7	,	-5	,	7	,	3	,	7	,
                   1	,	-4	,	1	,	-5	,	4	,	6	,
                   2	,	-1	,	0	,	6	,	-6	,	1	,
                   3	,	6	,	2	,	-3	,	-7	,	4	,
                   4	,	-3	,	-3	,	-1	,	2	,	-1	,
                   5	,	5	,	7	,	-4	,	7	,	-4	,
                   6	,	-5	,	-6	,	1	,	-1	,	0	,
                   7	,	2	,	-1	,	2	,	1	,	-6	                               
    ),ncol=6,byrow=TRUE)
    olh19=matrix(c(-9  ,  -9	,	-9	,	-8	,	2	,	7	,
                   -8	,	5	,	5	,	4	,	-8	,	9	,
                   -7	,	-6	,	-5	,	5	,	0	,	-5	,
                   -6	,	-3	,	0	,	-9	,	-4	,	-2	,
                   -5	,	9	,	6	,	1	,	7	,	4	,
                   -4	,	7	,	9	,	-5	,	-2	,	-6	,
                   -3	,	-2	,	3	,	3	,	-9	,	-7	,
                   -2	,	6	,	-8	,	6	,	6	,	-3	,
                   -1	,	-4	,	4	,	9	,	5	,	2	,
                   0	,	4	,	-6	,	-2	,	9	,	-8	,
                   1	,	0	,	-2	,	-4	,	-5	,	-1	,
                   2	,	8	,	-3	,	-1	,	3	,	1	,
                   3	,	-8	,	8	,	0	,	4	,	3	,
                   4	,	-7	,	2	,	8	,	-1	,	-9	,
                   5	,	-5	,	7	,	-3	,	8	,	5	,
                   6	,	1	,	-4	,	7	,	-7	,	6	,
                   7	,	2	,	-7	,	2	,	-3	,	8	,
                   8	,	3	,	-1	,	-6	,	-6	,	-4	,
                   9	,	-1	,	1	,	-7	,	1	,	0                           
    ),ncol=6,byrow=TRUE) 
    olh20=matrix(c(-19  ,  13	,	-19	,	-17	,	-19	,	7	,
                   -17	,	-13	,	9	,	17	,	-11	,	-3	,
                   -15	,	-9	,	11	,	5	,	11	,	9	,
                   -13	,	9	,	-3	,	-9	,	15	,	3	,
                   -11	,	-15	,	1	,	-15	,	-13	,	-15	,
                   -9	,	19	,	-1	,	11	,	13	,	-9	,
                   -7	,	15	,	-9	,	9	,	-7	,	5	,
                   -5	,	-3	,	7	,	-3	,	1	,	17	,
                   -3	,	-19	,	-7	,	7	,	7	,	15	,
                   -1	,	1	,	15	,	-11	,	-3	,	-13	,
                   1	,	-1	,	17	,	-1	,	-1	,	-5	,
                   3	,	-17	,	-11	,	13	,	9	,	-17	,
                   5	,	5	,	-5	,	-13	,	3	,	-7	,
                   7	,	11	,	3	,	3	,	19	,	-1	,
                   9	,	3	,	-15	,	1	,	17	,	1	,
                   11	,	17	,	19	,	15	,	-15	,	-11	,
                   13	,	-5	,	-13	,	19	,	-17	,	13	,
                   15	,	-11	,	13	,	-19	,	5	,	11	,
                   17	,	-7	,	-17	,	-7	,	-5	,	-19	,
                   19	,	7	,	5	,	-5	,	-9	,	19                     
    ),ncol=6,byrow=TRUE)/2  
    olh21=matrix(c(-10  ,  -2	,	-9	,	4	,	-9	,	-9	,
                   -9	,	-5	,	8	,	9	,	10	,	-7	,
                   -8	,	0	,	-3	,	-8	,	-4	,	-6	,
                   -7	,	10	,	4	,	7	,	6	,	6	,
                   -6	,	9	,	3	,	-5	,	4	,	-5	,
                   -5	,	5	,	-1	,	-1	,	-7	,	3	,
                   -4	,	1	,	-2	,	-9	,	-5	,	9	,
                   -3	,	-4	,	7	,	2	,	-1	,	10	,
                   -2	,	6	,	-5	,	-4	,	8	,	8	,
                   -1	,	-7	,	9	,	6	,	-10	,	-1	,
                   0	,	-3	,	0	,	-6	,	3	,	0	,
                   1	,	-1	,	-6	,	10	,	-3	,	5	,
                   2	,	-8	,	-4	,	-2	,	2	,	7	,
                   3	,	-10	,	5	,	-3	,	5	,	-8	,
                   4	,	-6	,	2	,	-10	,	7	,	1	,
                   5	,	-9	,	-7	,	0	,	-2	,	2	,
                   6	,	2	,	-10	,	5	,	9	,	-4	,
                   7	,	8	,	1	,	-7	,	-6	,	-10	,
                   8	,	4	,	-8	,	8	,	1	,	-3	,
                   9	,	3	,	6	,	3	,	-8	,	4	,
                   10	,	7	,	10	,	1	,	0	,	-2                        
    ),ncol=6,byrow=TRUE) 
    olh23=2*rbind(olh11,olh12)
    olh24=matrix(c(15  ,	-5	,	19	,	23	,	-21	,	17	,
                   19	,	15	,	-5	,	-21	,	17	,	23	,
                   -5	,	19	,	15	,	17	,	23	,	-21	,
                   -23	,	21	,	-17	,	15	,	-5	,	19	,
                   21	,	-17	,	-23	,	19	,	15	,	-5	,
                   -17	,	-23	,	21	,	-5	,	19	,	15	,
                   7	,	-3	,	-1	,	11	,	13	,	9	,
                   -3	,	-1	,	7	,	13	,	9	,	11	,
                   -1	,	7	,	-3	,	9	,	11	,	13	,
                   -13	,	-11	,	-9	,	-3	,	7	,	-1	,
                   -11	,	-9	,	-13	,	7	,	-1	,	-3	,
                   -9	,	-13	,	-11	,	-1	,	-3	,	7	,
                   -15	,	5	,	-19	,	-23	,	21	,	-17	,
                   -19	,	-15	,	5	,	21	,	-17	,	-23	,
                   5	,	-19	,	-15	,	-17	,	-23	,	21	,
                   23	,	-21	,	17	,	-15	,	5	,	-19	,
                   -21	,	17	,	23	,	-19	,	-15	,	5	,
                   17	,	23	,	-21	,	5	,	-19	,	-15	,
                   -7	,	3	,	1	,	-11	,	-13	,	-9	,
                   3	,	1	,	-7	,	-13	,	-9	,	-11	,
                   1	,	-7	,	3	,	-9	,	-11	,	-13	,
                   13	,	11	,	9	,	3	,	-7	,	1	,
                   11	,	9	,	13	,	-7	,	1	,	3	,
                   9	,	13	,	11	,	1	,	3	,	-7	
    ),ncol=6,byrow=TRUE)/2
    olh25=rbind(2*olh12,2*olh13)
    bd=list(olh11,olh12,olh13,olh15,olh19,olh20,olh21,olh23,olh24,olh25)  
    {
      if (n < 11 | n%%4 ==2) stop("OLH does not exist.")
      if(n>10 & n< 16)
      {
        if(n==11) design=bd[[1]]
        if(n==12) design=bd[[2]]
        if(n==13) design=bd[[3]]
        if(n==15) design=bd[[4]]
      }
      if (n>=16 & n < 32)
      {
        if (n==16) design=om16(levels)       
        if(n==17) design=rbind(om16(levels[c(1:8,(n-7):n)]),rep(0,6))       
        if(n==19) design=bd[[5]]        
        if(n==20) design=bd[[6]]       
        if(n==21)design=bd[[7]]       
        if(n==23) design=bd[[8]]
        if(n==24) design=bd[[9]]
        if(n==25) design=bd[[10]]
        if(n==27) design=rbind(om16(levels[c(1:8,(n-7):n)]),bd[[1]])            
        if(n==28) design=rbind(om16(levels[c(1:8,(n-7):n)]),bd[[2]])
        if(n==29) design=rbind(om16(levels[c(1:8,(n-7):n)]),bd[[3]])       
        if(n==31) design=rbind(om16(levels[c(1:8,(n-7):n)]),bd[[4]])       
      }
      if(n>31)
      {
        t=floor(n/16)
        if(n%%16==0)
        {
          design=om16r(n,t,levels)
        } 
        if(n%%16==1)
        {         
          design=rbind(om16r(n-1,t,levels[c(1:(8*t),(n-8*t+1):n)]),rep(0,6))         
        }
        if(n%%16==3)
        {                  
          design=rbind(om16r(16*(t-1),t-1,levels[c(1:(8*(t-1)),(n-8*(t-1)+1):n)]),bd[[5]])         
        }
        if(n%%16==4)
        {              
          design=rbind(om16r(16*(t-1),t-1,levels[c(1:(8*(t-1)),(n-8*(t-1)+1):n)]),bd[[6]])         
        }
        if(n%%16==5)
        {        
          design=rbind(om16r(16*(t-1),t-1,levels[c(1:(8*(t-1)),(n-8*(t-1)+1):n)]),bd[[7]])        
        }
        if(n%%16==7)
        {         
          design=rbind(om16r(16*(t-1),t-1,levels[c(1:(8*(t-1)),(n-8*(t-1)+1):n)]),bd[[8]])         
        }
        if(n%%16==8)
        {
          design=rbind(om16r(16*(t-1),t-1,levels[c(1:(8*(t-1)),(n-8*(t-1)+1):n)]),bd[[9]])         
        }
        if(n%%16==9)
        {
          design=rbind(om16r(16*(t-1),t-1,levels[c(1:(8*(t-1)),(n-8*(t-1)+1):n)]),bd[[10]])         
        }
        if(n%%16==11)
        {         
          design=rbind(om16r(16*t,t,levels[c(1:(8*t),(n-8*t+1):n)]),bd[[1]])         
        }
        if(n%%16==12)
        {         
          design=rbind(om16r(16*t,t,levels[c(1:(8*t),(n-8*t+1):n)]),bd[[2]])        
        }
        if(n%%16==13)
        {         
          design=rbind(om16r(16*t,t,levels[c(1:(8*t),(n-8*t+1):n)]),bd[[3]])         
        }
        if(n%%16==15)
        {         
          design=rbind(om16r(16*t,t,levels[c(1:(8*t),(n-8*t+1):n)]),bd[[4]])         
        }
      }     
    }  
    if(m<6)  design=design[,1:m]  
  }  
  if(is.matrix(design))
  {
    cp=t(design)%*%design
    temp=0
    for(i in 1:(m-1))
    {
      for (j in (i+1):m) 
      {
        if(cp[i,j]==0)  temp=temp+1        
      }  
    }
    if (temp==m*(m-1)/2) return(design) else return("OLH not found.")        
  }      
}

solh<-function(n1,t,q){
  if(t==4){
    L1<-olh1(n1,q)
    L2<-olh1(n1+1,q)
    L3<-olh1(2*n1,q)
    L4<-olh1(4*n1,q)
    D<-rbind(8*L1,"-",8*L2,"-",4*L3,"-",2*L4)
  }
  if(t==5){
    L1<-olh1(n1,q)
    L2<-olh1(n1+1,q)
    L3<-olh1(2*n1,q)
    L4<-olh1(4*n1,q)
    L5<-olh1(8*n1,q)
    D<-rbind(16*L1,"-",16*L2,"-",8*L3,"-",4*L4,"-",2*L5)
  }
  final_design<-D
  prmatrix(final_design, rowlab = rep("", nrow(final_design)), collab = rep("", ncol(final_design)),quote=FALSE, right = TRUE)
}

######################################################

slh<-function(n1,t,q){
  m = n1 - 1 
  T1<-c()
  for(i in 0:m){
    T1<-c(T1,c(1+(i*t)))
  }
  #######
  T_t<-c()
  for(i in 1:m){
    T_t<-c(T_t,(i*t))
  }
  ############
  if(t>2){
    fmatrix<-list()
    for(i in 2:(t-1)){
      blank<-c()
      for(j in 0:(m-1)){
        blank<-c(blank,(i+(j*t)))
      }
      fmatrix<-append(fmatrix,list(t(t(as.matrix((blank))))))
    }
    final_list<-c(list(t(t(as.matrix((T1))))),fmatrix,list(t(t(as.matrix((T_t))))))
  }else{
    final_list<-c(list(t(t(as.matrix((T1))))),list(t(t(as.matrix((T_t))))))
  }
  ##############
  
  selected_permutations<-function(vector,times){
    storage<-vector
    for(i in 1:100000){
      a<-sample(vector)
      storage<-rbind(storage,a)
      if(nrow(unique(storage))==times){
        return(unique(storage))
      }
    }
  }
  for(i in 1:length(final_list)){
    final_list[[i]]<-t(selected_permutations(c(final_list[[i]]),q))
  }
  ff<-lapply(final_list,function(mat)rbind(mat,"-"))
  final_design<-do.call(rbind,ff)
  final_design<-final_design[-nrow(final_design),]
  ######################################################################
  prmatrix(final_design, rowlab = rep("", nrow(final_design)), collab = rep("", ncol(final_design)),quote=FALSE)
} 
