##Random Forest code

##Loading libraries
library(data.table)
library(rpart)
library(randomForest)
library(fifer)


##Read in subset of 50 authors

df=fread("RC_50a-250p_subset.csv")
names="id"

df=subset(df, select=names)

names150=c("author","id","lego_2","i","x","lego_3","word_1","s","e","m","r","lego_5","w","v","l","word_7","word_8","p","word_6","phr_(SINV FRAG)","h","pos_DT","y","phr_(NP DT)","lego_4","word_2","q","g","pos_LS","phr_(S VP)","X..16","word_3","word_9","X2","k","pos_JJ","phr_(NP JJ)","X0","pos_EX","phr_(NP EX)","t","phr_(PP NNS)","pos_SYM","j","phr_(PP WHNP)","phr_(VP UCP)","word_10","lego_6","pos_MD","f","word_11","word_5","pos_VBP","phr_(VP VBP)","pos_POS","phr_(VP WHNP)","o","phr_(NP POS)","z","phr_(ADVP SYM)","phr_(NP NNS)","pos_WP$","phr_(WHADJP WP)","word_12","fw_very","X..14","X..10","pos_WRB","phr_(WHADJP WRB)","pos_FW","other_case","phr_(NP RRC)","pos_PRP$","phr_(NP PRP$)","phr_(VP NP)","pos_VBG","word_13","fw_haven.t","fw_mine","phr_(ADJP NNP)","pos_VBZ","pos_WDT","phr_(VP VBZ)","fw_whose","phr_(WHADJP JJ)","lego_7","fw_on","pos_RP","phr_(VP VBG)","phr_(NP RP)","fw_.re","pos_WP","word_14","phr_(VP INTJ)","pos_PDT","fw_together","phr_(VP POS)","phr_(WHADJP RB)","phr_(VP VB)","fw_in","fw_after","fw_yet","phr_(S NP)","pos_IN","phr_(NP IN)","n","lego_8","phr_(NP FW)","fw_them","phr_(S FRAG)","phr_(PP VBN)","phr_(ADJP SYM)","fw_by","X..23","phr_(VP LS)","word_15","fw_when","X1","fw_until","fw_the","fw_before","fw_ever","fw_inside","fw_was","fw_mayn.t","fw_here","fw_always","phr_(S ADVP)","fw_about","pos_VB","phr_(WHADJP ADJP)","phr_(NP INTJ)","phr_(X CC)","phr_(QP CD)","pos_NNS","phr_(S ADJP)","X..15","fw_itself","word_4","fw_perhaps","phr_(NP SYM)","X..4","X..5","phr_(X NN)","fw_wouldn.t","phr_(NX PP)","phr_(FRAG WHADJP)","fw_yourself","phr_(ADVP ADJP)","X..12","fw_though","fw_down")


##Dataframe at a time merge with the author list
dfMain=fread("RC_2017-02_combinedFeaturesSubset1.csv")

dfMain=subset(dfMain, select=names150)

dfMain=merge(df,dfMain,by = c("id"),all=FALSE)

##Now, merge the next one in 
##Dataframe at a time merge with the author list
tmp=fread("RC_2017-02_combinedFeaturesSubset2.csv")

tmp=subset(tmp, select=names150)

Hold=merge(df,tmp,by = c("id"),all=FALSE)

dfMain=rbind(dfMain,Hold)

##And the last one
##Dataframe at a time merge with the author list
tmp=fread("RC_2017-02_combinedFeaturesSubset3.csv")

tmp=subset(tmp, select=names150)

Hold=merge(df,tmp,by = c("id"),all=FALSE)

dfMain=rbind(dfMain,Hold)
rm(Hold,tmp)
gc()

##Missing a handful of observations due to slow parses

set.seed(1117)
ho_idx <- as.logical(rbinom(nrow(dfMain), 1, 0.2))
ho_set <- dfMain[ho_idx, ]
dfMain <- dfMain[!ho_idx, ]
n_posts <- nrow(dfMain)

# check set difference of authors
message("Users who are in the test set but not in the training set: ")
diff <- setdiff(ho_set$author, dfMain$author)
message(ifelse(length(diff) > 0, paste(diff, collapse = " "), "None"))
                            
message("Users who are in the training set but not in the test set: ")
diff <- setdiff(dfMain$author, ho_set$author)
message(ifelse(length(diff) > 0, paste(diff, collapse = " "), "None"))


##Now for the random forest
##Just on the training set
y=dfMain$author
y=as.factor(y)
dfMain=dfMain[,-c("id","author")]
dfOut=ho_set$author
dfOut=data.frame(dfOut)
names(dfOut)="author"
dfOut$id=ho_set$id

names150=c("lego_2","i","x","lego_3","word_1","s","e","m","r","lego_5","w","v","l","word_7","word_8","p","word_6","phr_(SINV FRAG)","h","pos_DT","y","phr_(NP DT)","lego_4","word_2","q","g","pos_LS","phr_(S VP)","X..16","word_3","word_9","X2","k","pos_JJ","phr_(NP JJ)","X0","pos_EX","phr_(NP EX)","t","phr_(PP NNS)","pos_SYM","j","phr_(PP WHNP)","phr_(VP UCP)","word_10","lego_6","pos_MD","f","word_11","word_5","pos_VBP","phr_(VP VBP)","pos_POS","phr_(VP WHNP)","o","phr_(NP POS)","z","phr_(ADVP SYM)","phr_(NP NNS)","pos_WP$","phr_(WHADJP WP)","word_12","fw_very","X..14","X..10","pos_WRB","phr_(WHADJP WRB)","pos_FW","other_case","phr_(NP RRC)","pos_PRP$","phr_(NP PRP$)","phr_(VP NP)","pos_VBG","word_13","fw_haven.t","fw_mine","phr_(ADJP NNP)","pos_VBZ","pos_WDT","phr_(VP VBZ)","fw_whose","phr_(WHADJP JJ)","lego_7","fw_on","pos_RP","phr_(VP VBG)","phr_(NP RP)","fw_.re","pos_WP","word_14","phr_(VP INTJ)","pos_PDT","fw_together","phr_(VP POS)","phr_(WHADJP RB)","phr_(VP VB)","fw_in","fw_after","fw_yet","phr_(S NP)","pos_IN","phr_(NP IN)","n","lego_8","phr_(NP FW)","fw_them","phr_(S FRAG)","phr_(PP VBN)","phr_(ADJP SYM)","fw_by","X..23","phr_(VP LS)","word_15","fw_when","X1","fw_until","fw_the","fw_before","fw_ever","fw_inside","fw_was","fw_mayn.t","fw_here","fw_always","phr_(S ADVP)","fw_about","pos_VB","phr_(WHADJP ADJP)","phr_(NP INTJ)","phr_(X CC)","phr_(QP CD)","pos_NNS","phr_(S ADJP)","X..15","fw_itself","word_4","fw_perhaps","phr_(NP SYM)","X..4","X..5","phr_(X NN)","fw_wouldn.t","phr_(NX PP)","phr_(FRAG WHADJP)","fw_yourself","phr_(ADVP ADJP)","X..12","fw_though","fw_down")

ho_set=subset(ho_set,select=names150)
preds=predict(randomForest(x=dfMain,y=y,ntree=500),newdata=ho_set,type="prob")

##Now to find the rank on the correctly predicted author

dfOut=data.frame(dfOut,stringsAsFactors=FALSE)
dfOut$rank=NA
for(i in 1:nrow(dfOut)){
  auth=dfOut$author[i]
  small=data.frame(preds[i,])
  small$auth=rownames(small)
  small=small[order(small$preds.i...,decreasing=TRUE),]
  dfOut$predicted[i]=small$auth[1]
  if(length(which(grepl(auth, small$auth)))==0){dfOut$rank[i]=NA} else {dfOut$rank[i]=which(grepl(auth, small$auth))}}


write.csv(dfOut,file="subset50-250p.csv")




##Read in subset of 600 authors

df=fread("RC_600w_subset.csv")
names="id"

df=subset(df, select=names)

names150=c("author","id","lego_2","i","x","lego_3","word_1","s","e","m","r","lego_5","w","v","l","word_7","word_8","p","word_6","phr_(SINV FRAG)","h","pos_DT","y","phr_(NP DT)","lego_4","word_2","q","g","pos_LS","phr_(S VP)","X..16","word_3","word_9","X2","k","pos_JJ","phr_(NP JJ)","X0","pos_EX","phr_(NP EX)","t","phr_(PP NNS)","pos_SYM","j","phr_(PP WHNP)","phr_(VP UCP)","word_10","lego_6","pos_MD","f","word_11","word_5","pos_VBP","phr_(VP VBP)","pos_POS","phr_(VP WHNP)","o","phr_(NP POS)","z","phr_(ADVP SYM)","phr_(NP NNS)","pos_WP$","phr_(WHADJP WP)","word_12","fw_very","X..14","X..10","pos_WRB","phr_(WHADJP WRB)","pos_FW","other_case","phr_(NP RRC)","pos_PRP$","phr_(NP PRP$)","phr_(VP NP)","pos_VBG","word_13","fw_haven.t","fw_mine","phr_(ADJP NNP)","pos_VBZ","pos_WDT","phr_(VP VBZ)","fw_whose","phr_(WHADJP JJ)","lego_7","fw_on","pos_RP","phr_(VP VBG)","phr_(NP RP)","fw_.re","pos_WP","word_14","phr_(VP INTJ)","pos_PDT","fw_together","phr_(VP POS)","phr_(WHADJP RB)","phr_(VP VB)","fw_in","fw_after","fw_yet","phr_(S NP)","pos_IN","phr_(NP IN)","n","lego_8","phr_(NP FW)","fw_them","phr_(S FRAG)","phr_(PP VBN)","phr_(ADJP SYM)","fw_by","X..23","phr_(VP LS)","word_15","fw_when","X1","fw_until","fw_the","fw_before","fw_ever","fw_inside","fw_was","fw_mayn.t","fw_here","fw_always","phr_(S ADVP)","fw_about","pos_VB","phr_(WHADJP ADJP)","phr_(NP INTJ)","phr_(X CC)","phr_(QP CD)","pos_NNS","phr_(S ADJP)","X..15","fw_itself","word_4","fw_perhaps","phr_(NP SYM)","X..4","X..5","phr_(X NN)","fw_wouldn.t","phr_(NX PP)","phr_(FRAG WHADJP)","fw_yourself","phr_(ADVP ADJP)","X..12","fw_though","fw_down")


##Dataframe at a time merge with the author list
dfMain=fread("RC_2017-02_combinedFeaturesSubset1.csv")

dfMain=subset(dfMain, select=names150)

dfMain=merge(dfMain,df,by = c("id"),all=FALSE)
##Now, merge the next one in 
##Dataframe at a time merge with the author list
tmp=fread("RC_2017-02_combinedFeaturesSubset2.csv")

tmp=subset(tmp, select=names150)

Hold=merge(df,tmp,by = c("id"),all=FALSE)

dfMain=rbind(dfMain,Hold)

##And the last one
##Dataframe at a time merge with the author list
tmp=fread("RC_2017-02_combinedFeaturesSubset3.csv")

tmp=subset(tmp, select=names150)

Hold=merge(df,tmp,by = c("id"),all=FALSE)

dfMain=rbind(dfMain,Hold)


##Missing a handful of observations due to slow parses

set.seed(1117)
ho_idx <- as.logical(rbinom(nrow(dfMain), 1, 0.2))
ho_set <- dfMain[ho_idx, ]
dfMain <- dfMain[!ho_idx, ]
n_posts <- nrow(dfMain)

# check set difference of authors
message("Users who are in the test set but not in the training set: ")
diff <- setdiff(ho_set$author, dfMain$author)
message(ifelse(length(diff) > 0, paste(diff, collapse = " "), "None"))

message("Users who are in the training set but not in the test set: ")
diff <- setdiff(dfMain$author, ho_set$author)
message(ifelse(length(diff) > 0, paste(diff, collapse = " "), "None"))


##Now for the random forest
##Just on the training set
y=dfMain$author
y=as.factor(y)
dfMain=dfMain[,-c("id","author")]
dfOut=ho_set$author
dfOut=data.frame(dfOut)
names(dfOut)="author"
dfOut$id=ho_set$id

names150=c("lego_2","i","x","lego_3","word_1","s","e","m","r","lego_5","w","v","l","word_7","word_8","p","word_6","phr_(SINV FRAG)","h","pos_DT","y","phr_(NP DT)","lego_4","word_2","q","g","pos_LS","phr_(S VP)","X..16","word_3","word_9","X2","k","pos_JJ","phr_(NP JJ)","X0","pos_EX","phr_(NP EX)","t","phr_(PP NNS)","pos_SYM","j","phr_(PP WHNP)","phr_(VP UCP)","word_10","lego_6","pos_MD","f","word_11","word_5","pos_VBP","phr_(VP VBP)","pos_POS","phr_(VP WHNP)","o","phr_(NP POS)","z","phr_(ADVP SYM)","phr_(NP NNS)","pos_WP$","phr_(WHADJP WP)","word_12","fw_very","X..14","X..10","pos_WRB","phr_(WHADJP WRB)","pos_FW","other_case","phr_(NP RRC)","pos_PRP$","phr_(NP PRP$)","phr_(VP NP)","pos_VBG","word_13","fw_haven.t","fw_mine","phr_(ADJP NNP)","pos_VBZ","pos_WDT","phr_(VP VBZ)","fw_whose","phr_(WHADJP JJ)","lego_7","fw_on","pos_RP","phr_(VP VBG)","phr_(NP RP)","fw_.re","pos_WP","word_14","phr_(VP INTJ)","pos_PDT","fw_together","phr_(VP POS)","phr_(WHADJP RB)","phr_(VP VB)","fw_in","fw_after","fw_yet","phr_(S NP)","pos_IN","phr_(NP IN)","n","lego_8","phr_(NP FW)","fw_them","phr_(S FRAG)","phr_(PP VBN)","phr_(ADJP SYM)","fw_by","X..23","phr_(VP LS)","word_15","fw_when","X1","fw_until","fw_the","fw_before","fw_ever","fw_inside","fw_was","fw_mayn.t","fw_here","fw_always","phr_(S ADVP)","fw_about","pos_VB","phr_(WHADJP ADJP)","phr_(NP INTJ)","phr_(X CC)","phr_(QP CD)","pos_NNS","phr_(S ADJP)","X..15","fw_itself","word_4","fw_perhaps","phr_(NP SYM)","X..4","X..5","phr_(X NN)","fw_wouldn.t","phr_(NX PP)","phr_(FRAG WHADJP)","fw_yourself","phr_(ADVP ADJP)","X..12","fw_though","fw_down")

ho_set=subset(ho_set,select=names150)
preds=predict(randomForest(x=dfMain,y=y,ntree=500),newdata=ho_set,type="prob")

##Now to find the rank on the correctly predicted author

dfOut$rank=NA
for(i in 1:nrow(dfOut)){
  auth=dfOut$author[i]
  small=data.frame(preds[i,])
  small$auth=rownames(small)
  small=small[order(small$preds.i...,decreasing=TRUE),]
  dfOut$rank[i]=if(length(which(grepl(auth, small$auth)))==0){dfOut$rank[i]=NA} else {dfOut$rank[i]=which(grepl(auth, small$auth))}}


write.csv(dfOut,file="RC_600w_rf.csv")




##Read in subset of 1500 authors

df=fread("RC_1500w_subset.csv")
names="id"

df=subset(df, select=names)

names150=c("author","id","lego_2","i","x","lego_3","word_1","s","e","m","r","lego_5","w","v","l","word_7","word_8","p","word_6","phr_(SINV FRAG)","h","pos_DT","y","phr_(NP DT)","lego_4","word_2","q","g","pos_LS","phr_(S VP)","X..16","word_3","word_9","X2","k","pos_JJ","phr_(NP JJ)","X0","pos_EX","phr_(NP EX)","t","phr_(PP NNS)","pos_SYM","j","phr_(PP WHNP)","phr_(VP UCP)","word_10","lego_6","pos_MD","f","word_11","word_5","pos_VBP","phr_(VP VBP)","pos_POS","phr_(VP WHNP)","o","phr_(NP POS)","z","phr_(ADVP SYM)","phr_(NP NNS)","pos_WP$","phr_(WHADJP WP)","word_12","fw_very","X..14","X..10","pos_WRB","phr_(WHADJP WRB)","pos_FW","other_case","phr_(NP RRC)","pos_PRP$","phr_(NP PRP$)","phr_(VP NP)","pos_VBG","word_13","fw_haven.t","fw_mine","phr_(ADJP NNP)","pos_VBZ","pos_WDT","phr_(VP VBZ)","fw_whose","phr_(WHADJP JJ)","lego_7","fw_on","pos_RP","phr_(VP VBG)","phr_(NP RP)","fw_.re","pos_WP","word_14","phr_(VP INTJ)","pos_PDT","fw_together","phr_(VP POS)","phr_(WHADJP RB)","phr_(VP VB)","fw_in","fw_after","fw_yet","phr_(S NP)","pos_IN","phr_(NP IN)","n","lego_8","phr_(NP FW)","fw_them","phr_(S FRAG)","phr_(PP VBN)","phr_(ADJP SYM)","fw_by","X..23","phr_(VP LS)","word_15","fw_when","X1","fw_until","fw_the","fw_before","fw_ever","fw_inside","fw_was","fw_mayn.t","fw_here","fw_always","phr_(S ADVP)","fw_about","pos_VB","phr_(WHADJP ADJP)","phr_(NP INTJ)","phr_(X CC)","phr_(QP CD)","pos_NNS","phr_(S ADJP)","X..15","fw_itself","word_4","fw_perhaps","phr_(NP SYM)","X..4","X..5","phr_(X NN)","fw_wouldn.t","phr_(NX PP)","phr_(FRAG WHADJP)","fw_yourself","phr_(ADVP ADJP)","X..12","fw_though","fw_down")


##Dataframe at a time merge with the author list
dfMain=fread("RC_2017-02_combinedFeaturesSubset1.csv")

dfMain=subset(dfMain, select=names150)

dfMain=merge(df,dfMain,by = c("id"),all=FALSE)

##Now, merge the next one in 
##Dataframe at a time merge with the author list
tmp=fread("RC_2017-02_combinedFeaturesSubset2.csv")

tmp=subset(tmp, select=names150)

Hold=merge(df,tmp,by = c("id"),all=FALSE)

dfMain=rbind(dfMain,Hold)

##And the last one
##Dataframe at a time merge with the author list
tmp=fread("RC_2017-02_combinedFeaturesSubset3.csv")

tmp=subset(tmp, select=names150)

Hold=merge(df,tmp,by = c("id"),all=FALSE)

dfMain=rbind(dfMain,Hold)


##Missing a handful of observations due to slow parses

set.seed(1117)
ho_idx <- as.logical(rbinom(nrow(dfMain), 1, 0.2))
ho_set <- dfMain[ho_idx, ]
dfMain <- dfMain[!ho_idx, ]
n_posts <- nrow(dfMain)

# check set difference of authors
message("Users who are in the test set but not in the training set: ")
diff <- setdiff(ho_set$author, dfMain$author)
message(ifelse(length(diff) > 0, paste(diff, collapse = " "), "None"))

message("Users who are in the training set but not in the test set: ")
diff <- setdiff(dfMain$author, ho_set$author)
message(ifelse(length(diff) > 0, paste(diff, collapse = " "), "None"))


##Now for the random forest
##Just on the training set
y=dfMain$author
dfMain=dfMain[,-c("id","author")]
dfOut=ho_set$author
dfOut=data.frame(dfOut)
names(dfOut)="author"

names150=c("lego_2","i","x","lego_3","word_1","s","e","m","r","lego_5","w","v","l","word_7","word_8","p","word_6","phr_(SINV FRAG)","h","pos_DT","y","phr_(NP DT)","lego_4","word_2","q","g","pos_LS","phr_(S VP)","X..16","word_3","word_9","X2","k","pos_JJ","phr_(NP JJ)","X0","pos_EX","phr_(NP EX)","t","phr_(PP NNS)","pos_SYM","j","phr_(PP WHNP)","phr_(VP UCP)","word_10","lego_6","pos_MD","f","word_11","word_5","pos_VBP","phr_(VP VBP)","pos_POS","phr_(VP WHNP)","o","phr_(NP POS)","z","phr_(ADVP SYM)","phr_(NP NNS)","pos_WP$","phr_(WHADJP WP)","word_12","fw_very","X..14","X..10","pos_WRB","phr_(WHADJP WRB)","pos_FW","other_case","phr_(NP RRC)","pos_PRP$","phr_(NP PRP$)","phr_(VP NP)","pos_VBG","word_13","fw_haven.t","fw_mine","phr_(ADJP NNP)","pos_VBZ","pos_WDT","phr_(VP VBZ)","fw_whose","phr_(WHADJP JJ)","lego_7","fw_on","pos_RP","phr_(VP VBG)","phr_(NP RP)","fw_.re","pos_WP","word_14","phr_(VP INTJ)","pos_PDT","fw_together","phr_(VP POS)","phr_(WHADJP RB)","phr_(VP VB)","fw_in","fw_after","fw_yet","phr_(S NP)","pos_IN","phr_(NP IN)","n","lego_8","phr_(NP FW)","fw_them","phr_(S FRAG)","phr_(PP VBN)","phr_(ADJP SYM)","fw_by","X..23","phr_(VP LS)","word_15","fw_when","X1","fw_until","fw_the","fw_before","fw_ever","fw_inside","fw_was","fw_mayn.t","fw_here","fw_always","phr_(S ADVP)","fw_about","pos_VB","phr_(WHADJP ADJP)","phr_(NP INTJ)","phr_(X CC)","phr_(QP CD)","pos_NNS","phr_(S ADJP)","X..15","fw_itself","word_4","fw_perhaps","phr_(NP SYM)","X..4","X..5","phr_(X NN)","fw_wouldn.t","phr_(NX PP)","phr_(FRAG WHADJP)","fw_yourself","phr_(ADVP ADJP)","X..12","fw_though","fw_down")

ho_set=subset(ho_set,select=names150)

y=data.frame(y)
names(y)="author"
dfMain=data.frame(cbind(y,dfMain))

df.samps=stratified(df=dfMain, group="author", 5, select = y)
y.samps=as.factor(df.samps$author)
df.samps=df.samps[,-1]
rf.tmp=randomForest(x=df.samps,y=y.samps,ntree=50,proximity=FALSE)
rf=rf.tmp

for(i in 1:9){
  df.samps=stratified(df=dfMain, group="author", 5, select = y)
  y.samps=as.factor(df.samps$author)
  df.samps=df.samps[,-1]
  rf.tmp=randomForest(x=df.samps,y=y.samps,ntree=50,proximity=FALSE)
  rf=combine(rf,rf.tmp)
  print(i)
}#end of for

names(ho_set)=names(df.samps)
ho_set1=ho_set[c(1:20000),]
preds=predict(rf,newdata=ho_set1,type="prob")

dfOut$rank=NA
for(i in 1:nrow(ho_set1)){
  auth=dfOut$author[i]
  small=data.frame(preds[i,])
  small$auth=rownames(small)
  small=small[order(small$preds.i...,decreasing=TRUE),]
  dfOut$rank[i]=if(length(which(grepl(auth, small$auth)))==0){dfOut$rank[i]=NA} else {dfOut$rank[i]=which(grepl(auth, small$auth))}
  print(i)}

ho_set1=ho_set[c(20001:nrow(ho_set)),]
preds=predict(rf,newdata=ho_set1,type="prob")
j=1
for(i in 20001:nrow(ho_set)){
  auth=dfOut$author[i]
  small=data.frame(preds[j,])
  small$auth=rownames(small)
  small=small[order(small$preds.j...,decreasing=TRUE),]
  dfOut$rank[i]=if(length(which(grepl(auth, small$auth)))==0){dfOut$rank[i]=NA} else {dfOut$rank[i]=which(grepl(auth, small$auth))}
  print(i)
  j=j+1}


write.csv(dfOut,file="RC_1500w_rf.csv")






##Read in subset of 5000 authors

df=fread("RC_5000a_subset.csv")
names="id"

df=subset(df, select=names)

names150=c("author","id","lego_2","i","x","lego_3","word_1","s","e","m","r","lego_5","w","v","l","word_7","word_8","p","word_6","phr_(SINV FRAG)","h","pos_DT","y","phr_(NP DT)","lego_4","word_2","q","g","pos_LS","phr_(S VP)","X..16","word_3","word_9","X2","k","pos_JJ","phr_(NP JJ)","X0","pos_EX","phr_(NP EX)","t","phr_(PP NNS)","pos_SYM","j","phr_(PP WHNP)","phr_(VP UCP)","word_10","lego_6","pos_MD","f","word_11","word_5","pos_VBP","phr_(VP VBP)","pos_POS","phr_(VP WHNP)","o","phr_(NP POS)","z","phr_(ADVP SYM)","phr_(NP NNS)","pos_WP$","phr_(WHADJP WP)","word_12","fw_very","X..14","X..10","pos_WRB","phr_(WHADJP WRB)","pos_FW","other_case","phr_(NP RRC)","pos_PRP$","phr_(NP PRP$)","phr_(VP NP)","pos_VBG","word_13","fw_haven.t","fw_mine","phr_(ADJP NNP)","pos_VBZ","pos_WDT","phr_(VP VBZ)","fw_whose","phr_(WHADJP JJ)","lego_7","fw_on","pos_RP","phr_(VP VBG)","phr_(NP RP)","fw_.re","pos_WP","word_14","phr_(VP INTJ)","pos_PDT","fw_together","phr_(VP POS)","phr_(WHADJP RB)","phr_(VP VB)","fw_in","fw_after","fw_yet","phr_(S NP)","pos_IN","phr_(NP IN)","n","lego_8","phr_(NP FW)","fw_them","phr_(S FRAG)","phr_(PP VBN)","phr_(ADJP SYM)","fw_by","X..23","phr_(VP LS)","word_15","fw_when","X1","fw_until","fw_the","fw_before","fw_ever","fw_inside","fw_was","fw_mayn.t","fw_here","fw_always","phr_(S ADVP)","fw_about","pos_VB","phr_(WHADJP ADJP)","phr_(NP INTJ)","phr_(X CC)","phr_(QP CD)","pos_NNS","phr_(S ADJP)","X..15","fw_itself","word_4","fw_perhaps","phr_(NP SYM)","X..4","X..5","phr_(X NN)","fw_wouldn.t","phr_(NX PP)","phr_(FRAG WHADJP)","fw_yourself","phr_(ADVP ADJP)","X..12","fw_though","fw_down")


##Dataframe at a time merge with the author list
dfMain=fread("RC_2017-02_combinedFeaturesSubset1.csv")

dfMain=subset(dfMain, select=names150)

dfMain=merge(df,dfMain,by = c("id"),all=FALSE)

##Now, merge the next one in 
##Dataframe at a time merge with the author list
tmp=fread("RC_2017-02_combinedFeaturesSubset2.csv")

tmp=subset(tmp, select=names150)

Hold=merge(df,tmp,by = c("id"),all=FALSE)

dfMain=rbind(dfMain,Hold)

##And the last one
##Dataframe at a time merge with the author list
tmp=fread("RC_2017-02_combinedFeaturesSubset3.csv")

tmp=subset(tmp, select=names150)

Hold=merge(df,tmp,by = c("id"),all=FALSE)

dfMain=rbind(dfMain,Hold)


##Missing a handful of observations due to slow parses

set.seed(1117)
ho_idx <- as.logical(rbinom(nrow(dfMain), 1, 0.2))
ho_set <- dfMain[ho_idx, ]
dfMain <- dfMain[!ho_idx, ]
n_posts <- nrow(dfMain)

# check set difference of authors
message("Users who are in the test set but not in the training set: ")
diff <- setdiff(ho_set$author, dfMain$author)
message(ifelse(length(diff) > 0, paste(diff, collapse = " "), "None"))

message("Users who are in the training set but not in the test set: ")
diff <- setdiff(dfMain$author, ho_set$author)
message(ifelse(length(diff) > 0, paste(diff, collapse = " "), "None"))


##Now for the random forest
##Just on the training set
y=dfMain$author
dfMain=dfMain[,-c("id","author")]
dfOut=ho_set$author
dfOut=data.frame(dfOut)
names(dfOut)="author"

names150=c("lego_2","i","x","lego_3","word_1","s","e","m","r","lego_5","w","v","l","word_7","word_8","p","word_6","phr_(SINV FRAG)","h","pos_DT","y","phr_(NP DT)","lego_4","word_2","q","g","pos_LS","phr_(S VP)","X..16","word_3","word_9","X2","k","pos_JJ","phr_(NP JJ)","X0","pos_EX","phr_(NP EX)","t","phr_(PP NNS)","pos_SYM","j","phr_(PP WHNP)","phr_(VP UCP)","word_10","lego_6","pos_MD","f","word_11","word_5","pos_VBP","phr_(VP VBP)","pos_POS","phr_(VP WHNP)","o","phr_(NP POS)","z","phr_(ADVP SYM)","phr_(NP NNS)","pos_WP$","phr_(WHADJP WP)","word_12","fw_very","X..14","X..10","pos_WRB","phr_(WHADJP WRB)","pos_FW","other_case","phr_(NP RRC)","pos_PRP$","phr_(NP PRP$)","phr_(VP NP)","pos_VBG","word_13","fw_haven.t","fw_mine","phr_(ADJP NNP)","pos_VBZ","pos_WDT","phr_(VP VBZ)","fw_whose","phr_(WHADJP JJ)","lego_7","fw_on","pos_RP","phr_(VP VBG)","phr_(NP RP)","fw_.re","pos_WP","word_14","phr_(VP INTJ)","pos_PDT","fw_together","phr_(VP POS)","phr_(WHADJP RB)","phr_(VP VB)","fw_in","fw_after","fw_yet","phr_(S NP)","pos_IN","phr_(NP IN)","n","lego_8","phr_(NP FW)","fw_them","phr_(S FRAG)","phr_(PP VBN)","phr_(ADJP SYM)","fw_by","X..23","phr_(VP LS)","word_15","fw_when","X1","fw_until","fw_the","fw_before","fw_ever","fw_inside","fw_was","fw_mayn.t","fw_here","fw_always","phr_(S ADVP)","fw_about","pos_VB","phr_(WHADJP ADJP)","phr_(NP INTJ)","phr_(X CC)","phr_(QP CD)","pos_NNS","phr_(S ADJP)","X..15","fw_itself","word_4","fw_perhaps","phr_(NP SYM)","X..4","X..5","phr_(X NN)","fw_wouldn.t","phr_(NX PP)","phr_(FRAG WHADJP)","fw_yourself","phr_(ADVP ADJP)","X..12","fw_though","fw_down")

ho_set=subset(ho_set,select=names150)

y=data.frame(y)
names(y)="author"
dfMain=data.frame(cbind(y,dfMain))

df.samps=stratified(df=dfMain, group="author", 5, select = y)
y.samps=as.factor(df.samps$author)
df.samps=df.samps[,-1]
rf.tmp=randomForest(x=df.samps,y=y.samps,ntree=50,proximity=FALSE)
rf=rf.tmp

for(i in 1:9){
  df.samps=stratified(df=dfMain, group="author", 5, select = y)
  y.samps=as.factor(df.samps$author)
  df.samps=df.samps[,-1]
  rf.tmp=randomForest(x=df.samps,y=y.samps,ntree=50,proximity=FALSE)
  rf=combine(rf,rf.tmp)
  print(i)
}#end of for

##Now, generate predictions
names(ho_set)=names(df.samps)
ho_set1=ho_set[c(1:20000),]
preds=predict(rf,newdata=ho_set1,type="prob")

dfOut$rank=NA
for(i in 1:nrow(ho_set1)){
  auth=dfOut$author[i]
  small=data.frame(preds[i,])
  small$auth=rownames(small)
  small=small[order(small$preds.i...,decreasing=TRUE),]
  dfOut$rank[i]=if(length(which(grepl(auth, small$auth)))==0){dfOut$rank[i]=NA} else {dfOut$rank[i]=which(grepl(auth, small$auth))}
  print(i)}

ho_set1=ho_set[c(20001:nrow(ho_set)),]
preds=predict(rf,newdata=ho_set1,type="prob")
j=1
for(i in 20001:nrow(ho_set)){
  auth=dfOut$author[i]
  small=data.frame(preds[j,])
  small$auth=rownames(small)
  small=small[order(small$preds.j...,decreasing=TRUE),]
  dfOut$rank[i]=if(length(which(grepl(auth, small$auth)))==0){dfOut$rank[i]=NA} else {dfOut$rank[i]=which(grepl(auth, small$auth))}
  print(i)
  j=j+1}


write.csv(dfOut,file="subset5000a.csv")





##Read in subset of 5000 words

df=fread("RC_5000w_subset.csv")
names="id"

df=subset(df, select=names)

names150=c("author","id","lego_2","i","x","lego_3","word_1","s","e","m","r","lego_5","w","v","l","word_7","word_8","p","word_6","phr_(SINV FRAG)","h","pos_DT","y","phr_(NP DT)","lego_4","word_2","q","g","pos_LS","phr_(S VP)","X..16","word_3","word_9","X2","k","pos_JJ","phr_(NP JJ)","X0","pos_EX","phr_(NP EX)","t","phr_(PP NNS)","pos_SYM","j","phr_(PP WHNP)","phr_(VP UCP)","word_10","lego_6","pos_MD","f","word_11","word_5","pos_VBP","phr_(VP VBP)","pos_POS","phr_(VP WHNP)","o","phr_(NP POS)","z","phr_(ADVP SYM)","phr_(NP NNS)","pos_WP$","phr_(WHADJP WP)","word_12","fw_very","X..14","X..10","pos_WRB","phr_(WHADJP WRB)","pos_FW","other_case","phr_(NP RRC)","pos_PRP$","phr_(NP PRP$)","phr_(VP NP)","pos_VBG","word_13","fw_haven.t","fw_mine","phr_(ADJP NNP)","pos_VBZ","pos_WDT","phr_(VP VBZ)","fw_whose","phr_(WHADJP JJ)","lego_7","fw_on","pos_RP","phr_(VP VBG)","phr_(NP RP)","fw_.re","pos_WP","word_14","phr_(VP INTJ)","pos_PDT","fw_together","phr_(VP POS)","phr_(WHADJP RB)","phr_(VP VB)","fw_in","fw_after","fw_yet","phr_(S NP)","pos_IN","phr_(NP IN)","n","lego_8","phr_(NP FW)","fw_them","phr_(S FRAG)","phr_(PP VBN)","phr_(ADJP SYM)","fw_by","X..23","phr_(VP LS)","word_15","fw_when","X1","fw_until","fw_the","fw_before","fw_ever","fw_inside","fw_was","fw_mayn.t","fw_here","fw_always","phr_(S ADVP)","fw_about","pos_VB","phr_(WHADJP ADJP)","phr_(NP INTJ)","phr_(X CC)","phr_(QP CD)","pos_NNS","phr_(S ADJP)","X..15","fw_itself","word_4","fw_perhaps","phr_(NP SYM)","X..4","X..5","phr_(X NN)","fw_wouldn.t","phr_(NX PP)","phr_(FRAG WHADJP)","fw_yourself","phr_(ADVP ADJP)","X..12","fw_though","fw_down")


##Dataframe at a time merge with the author list
dfMain=fread("RC_2017-02_combinedFeaturesSubset1.csv")

dfMain=subset(dfMain, select=names150)

dfMain=merge(df,dfMain,by = c("id"),all=FALSE)

##Now, merge the next one in 
##Dataframe at a time merge with the author list
tmp=fread("RC_2017-02_combinedFeaturesSubset2.csv")

tmp=subset(tmp, select=names150)

Hold=merge(df,tmp,by = c("id"),all=FALSE)

dfMain=rbind(dfMain,Hold)

##And the last one
##Dataframe at a time merge with the author list
tmp=fread("RC_2017-02_combinedFeaturesSubset3.csv")

tmp=subset(tmp, select=names150)

Hold=merge(df,tmp,by = c("id"),all=FALSE)

dfMain=rbind(dfMain,Hold)


##Missing a handful of observations due to slow parses

set.seed(1117)
ho_idx <- as.logical(rbinom(nrow(dfMain), 1, 0.2))
ho_set <- dfMain[ho_idx, ]
dfMain <- dfMain[!ho_idx, ]
n_posts <- nrow(dfMain)

# check set difference of authors
message("Users who are in the test set but not in the training set: ")
diff <- setdiff(ho_set$author, dfMain$author)
message(ifelse(length(diff) > 0, paste(diff, collapse = " "), "None"))

message("Users who are in the training set but not in the test set: ")
diff <- setdiff(dfMain$author, ho_set$author)
message(ifelse(length(diff) > 0, paste(diff, collapse = " "), "None"))


##Now for the random forest
##Just on the training set
y=dfMain$author
dfMain=dfMain[,-c("id","author")]
dfOut=ho_set$author
dfOut=data.frame(dfOut)
names(dfOut)="author"

names150=c("lego_2","i","x","lego_3","word_1","s","e","m","r","lego_5","w","v","l","word_7","word_8","p","word_6","phr_(SINV FRAG)","h","pos_DT","y","phr_(NP DT)","lego_4","word_2","q","g","pos_LS","phr_(S VP)","X..16","word_3","word_9","X2","k","pos_JJ","phr_(NP JJ)","X0","pos_EX","phr_(NP EX)","t","phr_(PP NNS)","pos_SYM","j","phr_(PP WHNP)","phr_(VP UCP)","word_10","lego_6","pos_MD","f","word_11","word_5","pos_VBP","phr_(VP VBP)","pos_POS","phr_(VP WHNP)","o","phr_(NP POS)","z","phr_(ADVP SYM)","phr_(NP NNS)","pos_WP$","phr_(WHADJP WP)","word_12","fw_very","X..14","X..10","pos_WRB","phr_(WHADJP WRB)","pos_FW","other_case","phr_(NP RRC)","pos_PRP$","phr_(NP PRP$)","phr_(VP NP)","pos_VBG","word_13","fw_haven.t","fw_mine","phr_(ADJP NNP)","pos_VBZ","pos_WDT","phr_(VP VBZ)","fw_whose","phr_(WHADJP JJ)","lego_7","fw_on","pos_RP","phr_(VP VBG)","phr_(NP RP)","fw_.re","pos_WP","word_14","phr_(VP INTJ)","pos_PDT","fw_together","phr_(VP POS)","phr_(WHADJP RB)","phr_(VP VB)","fw_in","fw_after","fw_yet","phr_(S NP)","pos_IN","phr_(NP IN)","n","lego_8","phr_(NP FW)","fw_them","phr_(S FRAG)","phr_(PP VBN)","phr_(ADJP SYM)","fw_by","X..23","phr_(VP LS)","word_15","fw_when","X1","fw_until","fw_the","fw_before","fw_ever","fw_inside","fw_was","fw_mayn.t","fw_here","fw_always","phr_(S ADVP)","fw_about","pos_VB","phr_(WHADJP ADJP)","phr_(NP INTJ)","phr_(X CC)","phr_(QP CD)","pos_NNS","phr_(S ADJP)","X..15","fw_itself","word_4","fw_perhaps","phr_(NP SYM)","X..4","X..5","phr_(X NN)","fw_wouldn.t","phr_(NX PP)","phr_(FRAG WHADJP)","fw_yourself","phr_(ADVP ADJP)","X..12","fw_though","fw_down")

ho_set=subset(ho_set,select=names150)

y=data.frame(y)
names(y)="author"
dfMain=data.frame(cbind(y,dfMain))

df.samps=stratified(df=dfMain, group="author", 5, select = y)
y.samps=as.factor(df.samps$author)
df.samps=df.samps[,-1]
rf.tmp=randomForest(x=df.samps,y=y.samps,ntree=50,proximity=FALSE)
rf=rf.tmp

for(i in 1:9){
  df.samps=stratified(df=dfMain, group="author", 5, select = y)
  y.samps=as.factor(df.samps$author)
  df.samps=df.samps[,-1]
  rf.tmp=randomForest(x=df.samps,y=y.samps,ntree=50,proximity=FALSE)
  rf=combine(rf,rf.tmp)
  print(i)
}#end of for

##Now, generate predictions
names(ho_set)=names(df.samps)
ho_set1=ho_set[c(1:25000),]
preds=predict(rf,newdata=ho_set1,type="prob")

dfOut$rank=NA
for(i in 1:nrow(ho_set1)){
  auth=dfOut$author[i]
  small=data.frame(preds[i,])
  small$auth=rownames(small)
  small=small[order(small$preds.i...,decreasing=TRUE),]
  dfOut$rank[i]=if(length(which(grepl(auth, small$auth)))==0){dfOut$rank[i]=NA} else {dfOut$rank[i]=which(grepl(auth, small$auth))}
  print(i)}

ho_set1=ho_set[c(25001:50000),]
preds=predict(rf,newdata=ho_set1,type="prob")
j=1
for(i in 25001:50000){
  auth=dfOut$author[i]
  small=data.frame(preds[j,])
  small$auth=rownames(small)
  small=small[order(small$preds.j...,decreasing=TRUE),]
  dfOut$rank[i]=if(length(which(grepl(auth, small$auth)))==0){dfOut$rank[i]=NA} else {dfOut$rank[i]=which(grepl(auth, small$auth))}
  print(i)
  j=j+1}

ho_set1=ho_set[c(50001:75000),]
preds=predict(rf,newdata=ho_set1,type="prob")
j=1
for(i in 50001:75000){
  auth=dfOut$author[i]
  small=data.frame(preds[j,])
  small$auth=rownames(small)
  small=small[order(small$preds.j...,decreasing=TRUE),]
  dfOut$rank[i]=if(length(which(grepl(auth, small$auth)))==0){dfOut$rank[i]=NA} else {dfOut$rank[i]=which(grepl(auth, small$auth))}
  print(i)
  j=j+1}

ho_set1=ho_set[c(75001:100000),]
preds=predict(rf,newdata=ho_set1,type="prob")
j=1
for(i in 75001:100000){
  auth=dfOut$author[i]
  small=data.frame(preds[j,])
  small$auth=rownames(small)
  small=small[order(small$preds.j...,decreasing=TRUE),]
  dfOut$rank[i]=if(length(which(grepl(auth, small$auth)))==0){dfOut$rank[i]=NA} else {dfOut$rank[i]=which(grepl(auth, small$auth))}
  print(i)
  j=j+1}

ho_set1=ho_set[c(100000:nrow(ho_set)),]
preds=predict(rf,newdata=ho_set1,type="prob")
j=1
for(i in 100000:nrow(ho_set)){
  auth=dfOut$author[i]
  small=data.frame(preds[j,])
  small$auth=rownames(small)
  small=small[order(small$preds.j...,decreasing=TRUE),]
  dfOut$rank[i]=if(length(which(grepl(auth, small$auth)))==0){dfOut$rank[i]=NA} else {dfOut$rank[i]=which(grepl(auth, small$auth))}
  print(i)
  j=j+1}


write.csv(dfOut,file="subset5000w.csv")








##Read in subset of 8000 words


df=fread("RC_8000w_subset.csv")
names="id"

df=subset(df, select=names)

names150=c("author","id","lego_2","i","x","lego_3","word_1","s","e","m","r","lego_5","w","v","l","word_7","word_8","p","word_6","phr_(SINV FRAG)","h","pos_DT","y","phr_(NP DT)","lego_4","word_2","q","g","pos_LS","phr_(S VP)","X..16","word_3","word_9","X2","k","pos_JJ","phr_(NP JJ)","X0","pos_EX","phr_(NP EX)","t","phr_(PP NNS)","pos_SYM","j","phr_(PP WHNP)","phr_(VP UCP)","word_10","lego_6","pos_MD","f","word_11","word_5","pos_VBP","phr_(VP VBP)","pos_POS","phr_(VP WHNP)","o","phr_(NP POS)","z","phr_(ADVP SYM)","phr_(NP NNS)","pos_WP$","phr_(WHADJP WP)","word_12","fw_very","X..14","X..10","pos_WRB","phr_(WHADJP WRB)","pos_FW","other_case","phr_(NP RRC)","pos_PRP$","phr_(NP PRP$)","phr_(VP NP)","pos_VBG","word_13","fw_haven.t","fw_mine","phr_(ADJP NNP)","pos_VBZ","pos_WDT","phr_(VP VBZ)","fw_whose","phr_(WHADJP JJ)","lego_7","fw_on","pos_RP","phr_(VP VBG)","phr_(NP RP)","fw_.re","pos_WP","word_14","phr_(VP INTJ)","pos_PDT","fw_together","phr_(VP POS)","phr_(WHADJP RB)","phr_(VP VB)","fw_in","fw_after","fw_yet","phr_(S NP)","pos_IN","phr_(NP IN)","n","lego_8","phr_(NP FW)","fw_them","phr_(S FRAG)","phr_(PP VBN)","phr_(ADJP SYM)","fw_by","X..23","phr_(VP LS)","word_15","fw_when","X1","fw_until","fw_the","fw_before","fw_ever","fw_inside","fw_was","fw_mayn.t","fw_here","fw_always","phr_(S ADVP)","fw_about","pos_VB","phr_(WHADJP ADJP)","phr_(NP INTJ)","phr_(X CC)","phr_(QP CD)","pos_NNS","phr_(S ADJP)","X..15","fw_itself","word_4","fw_perhaps","phr_(NP SYM)","X..4","X..5","phr_(X NN)","fw_wouldn.t","phr_(NX PP)","phr_(FRAG WHADJP)","fw_yourself","phr_(ADVP ADJP)","X..12","fw_though","fw_down")


##Dataframe at a time merge with the author list
dfMain=fread("RC_2017-02_combinedFeaturesSubset1.csv")

dfMain=subset(dfMain, select=names150)

dfMain=merge(df,dfMain,by = c("id"),all=FALSE)

##Now, merge the next one in 
##Dataframe at a time merge with the author list
tmp=fread("RC_2017-02_combinedFeaturesSubset2.csv")

tmp=subset(tmp, select=names150)

Hold=merge(df,tmp,by = c("id"),all=FALSE)

dfMain=rbind(dfMain,Hold)

##And the last one
##Dataframe at a time merge with the author list
tmp=fread("RC_2017-02_combinedFeaturesSubset3.csv")

tmp=subset(tmp, select=names150)

Hold=merge(df,tmp,by = c("id"),all=FALSE)

dfMain=rbind(dfMain,Hold)


##Missing a handful of observations due to slow parses

set.seed(1117)
ho_idx <- as.logical(rbinom(nrow(dfMain), 1, 0.2))
ho_set <- dfMain[ho_idx, ]
dfMain <- dfMain[!ho_idx, ]
n_posts <- nrow(dfMain)

# check set difference of authors
message("Users who are in the test set but not in the training set: ")
diff <- setdiff(ho_set$author, dfMain$author)
message(ifelse(length(diff) > 0, paste(diff, collapse = " "), "None"))

message("Users who are in the training set but not in the test set: ")
diff <- setdiff(dfMain$author, ho_set$author)
message(ifelse(length(diff) > 0, paste(diff, collapse = " "), "None"))


##Now for the random forest
##Just on the training set
y=dfMain$author
dfMain=dfMain[,-c("id","author")]
dfOut=ho_set$author
dfOut=data.frame(dfOut)
names(dfOut)="author"

names150=c("lego_2","i","x","lego_3","word_1","s","e","m","r","lego_5","w","v","l","word_7","word_8","p","word_6","phr_(SINV FRAG)","h","pos_DT","y","phr_(NP DT)","lego_4","word_2","q","g","pos_LS","phr_(S VP)","X..16","word_3","word_9","X2","k","pos_JJ","phr_(NP JJ)","X0","pos_EX","phr_(NP EX)","t","phr_(PP NNS)","pos_SYM","j","phr_(PP WHNP)","phr_(VP UCP)","word_10","lego_6","pos_MD","f","word_11","word_5","pos_VBP","phr_(VP VBP)","pos_POS","phr_(VP WHNP)","o","phr_(NP POS)","z","phr_(ADVP SYM)","phr_(NP NNS)","pos_WP$","phr_(WHADJP WP)","word_12","fw_very","X..14","X..10","pos_WRB","phr_(WHADJP WRB)","pos_FW","other_case","phr_(NP RRC)","pos_PRP$","phr_(NP PRP$)","phr_(VP NP)","pos_VBG","word_13","fw_haven.t","fw_mine","phr_(ADJP NNP)","pos_VBZ","pos_WDT","phr_(VP VBZ)","fw_whose","phr_(WHADJP JJ)","lego_7","fw_on","pos_RP","phr_(VP VBG)","phr_(NP RP)","fw_.re","pos_WP","word_14","phr_(VP INTJ)","pos_PDT","fw_together","phr_(VP POS)","phr_(WHADJP RB)","phr_(VP VB)","fw_in","fw_after","fw_yet","phr_(S NP)","pos_IN","phr_(NP IN)","n","lego_8","phr_(NP FW)","fw_them","phr_(S FRAG)","phr_(PP VBN)","phr_(ADJP SYM)","fw_by","X..23","phr_(VP LS)","word_15","fw_when","X1","fw_until","fw_the","fw_before","fw_ever","fw_inside","fw_was","fw_mayn.t","fw_here","fw_always","phr_(S ADVP)","fw_about","pos_VB","phr_(WHADJP ADJP)","phr_(NP INTJ)","phr_(X CC)","phr_(QP CD)","pos_NNS","phr_(S ADJP)","X..15","fw_itself","word_4","fw_perhaps","phr_(NP SYM)","X..4","X..5","phr_(X NN)","fw_wouldn.t","phr_(NX PP)","phr_(FRAG WHADJP)","fw_yourself","phr_(ADVP ADJP)","X..12","fw_though","fw_down")

ho_set=subset(ho_set,select=names150)

y=data.frame(y)
names(y)="author"
dfMain=data.frame(cbind(y,dfMain))

df.samps=stratified(df=dfMain, group="author", 5, select = y)
y.samps=as.factor(df.samps$author)
df.samps=df.samps[,-1]
rf.tmp=randomForest(x=df.samps,y=y.samps,ntree=50,proximity=FALSE)
rf=rf.tmp

for(i in 1:9){
  df.samps=stratified(df=dfMain, group="author", 5, select = y)
  y.samps=as.factor(df.samps$author)
  df.samps=df.samps[,-1]
  rf.tmp=randomForest(x=df.samps,y=y.samps,ntree=50,proximity=FALSE)
  rf=combine(rf,rf.tmp)
  print(i)
}#end of for

##Now, generate predictions
names(ho_set)=names(df.samps)
ho_set1=ho_set[c(1:25000),]
preds=predict(rf,newdata=ho_set1,type="prob")

dfOut$rank=NA
for(i in 1:nrow(ho_set1)){
  auth=dfOut$author[i]
  small=data.frame(preds[i,])
  small$auth=rownames(small)
  small=small[order(small$preds.i...,decreasing=TRUE),]
  dfOut$rank[i]=if(length(which(grepl(auth, small$auth)))==0){dfOut$rank[i]=NA} else {dfOut$rank[i]=which(grepl(auth, small$auth))}
  print(i)}

ho_set1=ho_set[c(25001:50000),]
preds=predict(rf,newdata=ho_set1,type="prob")
j=1
for(i in 25001:50000){
  auth=dfOut$author[i]
  small=data.frame(preds[j,])
  small$auth=rownames(small)
  small=small[order(small$preds.j...,decreasing=TRUE),]
  dfOut$rank[i]=if(length(which(grepl(auth, small$auth)))==0){dfOut$rank[i]=NA} else {dfOut$rank[i]=which(grepl(auth, small$auth))}
  print(i)
  j=j+1}

ho_set1=ho_set[c(50001:75000),]
preds=predict(rf,newdata=ho_set1,type="prob")
j=1
for(i in 50001:75000){
  auth=dfOut$author[i]
  small=data.frame(preds[j,])
  small$auth=rownames(small)
  small=small[order(small$preds.j...,decreasing=TRUE),]
  dfOut$rank[i]=if(length(which(grepl(auth, small$auth)))==0){dfOut$rank[i]=NA} else {dfOut$rank[i]=which(grepl(auth, small$auth))}
  print(i)
  j=j+1}

ho_set1=ho_set[c(75001:100000),]
preds=predict(rf,newdata=ho_set1,type="prob")
j=1
for(i in 75001:100000){
  auth=dfOut$author[i]
  small=data.frame(preds[j,])
  small$auth=rownames(small)
  small=small[order(small$preds.j...,decreasing=TRUE),]
  dfOut$rank[i]=if(length(which(grepl(auth, small$auth)))==0){dfOut$rank[i]=NA} else {dfOut$rank[i]=which(grepl(auth, small$auth))}
  print(i)
  j=j+1}

ho_set1=ho_set[c(100001:nrow(ho_set)),]
preds=predict(rf,newdata=ho_set1,type="prob")
j=1
for(i in 100001:nrow(ho_set)){
  auth=dfOut$author[i]
  small=data.frame(preds[j,])
  small$auth=rownames(small)
  small=small[order(small$preds.j...,decreasing=TRUE),]
  dfOut$rank[i]=if(length(which(grepl(auth, small$auth)))==0){dfOut$rank[i]=NA} else {dfOut$rank[i]=which(grepl(auth, small$auth))}
  print(i)
  j=j+1}


write.csv(dfOut,file="subset8000w.csv")
