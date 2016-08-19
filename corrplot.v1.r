#corrplot
require(RCurl)
urlfile<-"http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data"
x<-getURL(urlfile,ssl.verifypeer=FALSE)
adults<-read.csv(textConnection(x),header=F)
names(adults)<-c("Age","Workclass","FinalWeight","Education","EducationNumer",
                 "MaritalStatus","Occupation","Relationship","Race","Sex",
                 "CapitalGain","CapitalLoss","HoursWeek","NativeCountry","Income")
table(adults$Income)
adults$Income <- ifelse(adults$Income ==" <=50K",0,1)
str(adults)

# 对因子变量进行哑变量处理
library(caret)
dmy<-dummyVars("~.",data=adults)
adultsTrsf<-data.frame(predict(dmy,newdata=adults))
# 计算变量间相关系数的p值

cor.prob<-function(X,dfr=nrow(X)-2){
  
  R<-cor(X,use="pairwise.complete.obs")
  
  above<-row(R)<col(R)
  
  r2<-R[above]^2
  
  Fstat<-r2*dfr/(1-r2)
  
  R[above]<-1-pf(Fstat,1,dfr)
  
  R[row(R)==col(R)]<-NA
  
  R
  
}

# 将数据整理成data.frame形式

flattenSquareMatrix<-function(m){
  
  if((class(m) !="matrix") | (nrow(m) !=ncol(m))) stop("Must be asquare matrix.")
  
  if(!identical(rownames(m),colnames(m))) stop("Row and column names must be equal.")
  
  ut<-upper.tri(m)
  
  data.frame(i=rownames(m)[row(m)[ut]],
             
             j=rownames(m)[col(m)[ut]],
             
             cor=t(m)[ut],
             
             p=m[ut])
  
}

corMasterList<-flattenSquareMatrix(cor.prob(adultsTrsf))