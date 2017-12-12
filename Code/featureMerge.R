##Merging parsed features
library(data.table)
library(ff)

df=read.csv.ffdf(file="RC_2017-02_features.csv", header=TRUE, VERBOSE=TRUE, first.rows=10000, next.rows=50000, colClasses=NA)


##Merge everything in chunk by chunk
fname="RC_2017-02_combinedFeatures.csv"

for(i in 1:51){
  name= paste("phrase_features_batch_",i,".csv",sep="")
  df.tmp=fread(file=name)
  df.tmp[is.na(df.tmp)]= 0
  df.tmp=merge(df,df.tmp,key="id",all=FALSE)
  write.table(df.tmp,
              file=fname, 
              sep = ",",
              row.names = FALSE,
              append = TRUE,
              col.names=!file.exists(fname)) 
  print(i)
}
