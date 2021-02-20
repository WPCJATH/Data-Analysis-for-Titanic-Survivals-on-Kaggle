library(ggplot2,lib.loc="C:/Users/86155/Desktop/comp1433_proj/lib")#
library(e1071,lib.loc="C:/Users/86155/Desktop/comp1433_proj/lib")#
library(gridExtra,lib.loc="C:/Users/86155/Desktop/comp1433_proj/lib/gridExtra_2.3")
library(dplyr,lib.loc="C:/Users/86155/Desktop/comp1433_proj/lib/dplyr_0.8.5")
library(mice,lib.loc="C:/Users/86155/Desktop/comp1433_proj/lib/mice_3.9.0")#
library(knitr,lib.loc="C:/Users/86155/Desktop/comp1433_proj/lib/knitr_1.28")
library(stringr,lib.loc="C:/Users/86155/Desktop/comp1433_proj/lib/stringr_1.4.0")
library(readr,lib.loc="C:/Users/86155/Desktop/comp1433_proj/lib/readr_1.3.1")
library(ggthemes,lib.loc="C:/Users/86155/Desktop/comp1433_proj/lib/ggthemes_4.2.0")
library(Amelia,lib.loc="C:/Users/86155/Desktop/comp1433_proj/lib/Amelia_1.7.6")
library(party,lib.loc="C:/Users/86155/Desktop/comp1433_proj/lib/party_1.3-4")
library(gbm,lib.loc="C:/Users/86155/Desktop/comp1433_proj/lib/gbm_2.1.5")
library(plyr,lib.loc="C:/Users/86155/Desktop/comp1433_proj/lib/plyr_1.8.6")
library(corrplot,lib.loc="C:/Users/86155/Desktop/comp1433_proj/lib/corrplot_0.84")
library(randomForest,lib.loc="C:/Users/86155/Desktop/comp1433_proj/lib/randomForest_4.6-14")#
library(scales,lib.loc="C:/Users/86155/Desktop/comp1433_proj/lib")#

test <- read.csv("C:/Users/86155/Desktop/comp1433_proj/option2_attachment/option2_attachment/test.csv",header = T)
train <- read.csv("C:/Users/86155/Desktop/comp1433_proj/option2_attachment/option2_attachment/train.csv",header = T)
 

#We found that many of data in different types are missing when open these 2 .csv files, 
#so we'd like to combine the file and fill the missing data so that we can conveniently predict the data 
test$Survived <-NA
combination <-rbind(train,test)


print(sapply(combination,function(x) sum(is.na(x)))) # show the missing data
# We found it strange that some missing data are not detcted by R, so we checked the type of the data in R

print(str(combination))
# As expected, some missing data was considered as "" which is not totally equal to NA. R thinks "" is also valid 
#Besides, Survived and Pclass are considered as int by R, we change the types to Factor so that it is better for data anlysis
combination$Survived <- as.factor(combination$Survived)
combination$Pclass <- as.factor(combination$Pclass)
print(str(combination))#Check whether the above 2 statements have its effect

#We directly assign NA to the missing data which shows "" and it works
for (index in 1:1309){
  if (combination$Cabin[index]=="")
    combination$Cabin[index]=NA
  if (combination$Embarked[index]=="")
    combination$Embarked[index]=NA
}

print(sapply(combination,function(x) sum(is.na(x))))#All missing data was found by R 

#Fill the Fare*******************************************************************************
#Out of Fare is missing only one data, so we fill the Fare first
for (i in 1:1309){
  if (is.na(combination$Fare[i])){
    ID_noFare=combination$PassengerId[i] 
    print(ID_noFare)
  }
}
#The Id of the people is 1044

#Print the people who misses Fare
print(combination[combination$PassengerId==ID_noFare,])

#From the print operattion, we know that the people is from Pclass 3 and Embarked on S
#We picked all these people
Pclass3_EnbarS<-combination[which(combination$Pclass==3 & combination$Embarked=='S'),]
Pclass3_EnbarS<-data.frame(Fare=Pclass3_EnbarS$Fare)
Pclass3_EnbarS <- Pclass3_EnbarS[complete.cases(Pclass3_EnbarS),]#Delete the one who miss the Fare 
#we found Pclass3_EnbarS<-Pclass3_EnbarS[!(Pclass3_EnbarS$Fare=="NA" ),] is invalid, so we used the more compicated one

Pclass3_EnbarS<-data.frame(Fare=Pclass3_EnbarS)#change the type to data.frame to make preparation for ggplots2
#Therefore, we made a graph which covers all people at Pclass 3 and Enbarked from S
print(pic_Pclass3_EnbarS1<- ggplot(Pclass3_EnbarS,aes(x=Fare)) + geom_bar(stat="count",width=0.5, fill='gray'))
#This pic got a warning message beacasue too many x intervals, however, we get the most frequent number, and it is obivously bewteen 7 and 9.
#We choose modal number bacuase in probability, they are most possible paid the Fare in modal number
#We withdrew the range so that we can get the modal number
Pclass3_EnbarS<-Pclass3_EnbarS[which(Pclass3_EnbarS$Fare<9 & Pclass3_EnbarS$Fare>7),]
Pclass3_EnbarS<-data.frame(Fare=Pclass3_EnbarS)
print(pic_Pclass3_EnbarS2<- ggplot(Pclass3_EnbarS,aes(x=Fare)) + geom_bar(stat="count",width=0.1, fill='gray'))
#Even though we still get the warning message, we can ensure the modal number is very close to 8.0, so we assign the missing Fare as 8.0
combination[(is.na(combination$Fare)),"Fare"]<- 8.0
print(combination[combination$PassengerId==ID_noFare,])#test


#Fill the Embarked*******************************************************************************
#get the two people who don't have Embarked info.
for (i in 1:1309){
  if (is.na(combination$Embarked[i])){
    ID_noEmbark=combination$PassengerId[i] 
    print(ID_noEmbark)
  }
}
#The Id of the People are 62 and 830
#print they info.
print(combination[combination$PassengerId==62,])
print(combination[combination$PassengerId==830,])

#Interestingly, they are both class 1 people and they Fare are 80
#Get the people who are class 1 and paid 80 for the ticket
Pclass1_Fare80<-combination[which(combination$Pclass==1 & combination$Fare==80),]
#Oops, there only 2 people and the people are exactly them
#change the Fare to Cabin
Pclass1_CabinB28<-combination[which(combination$Pclass==1 & combination$Cabin=="B28"),]
#Still 2, they are must daughter and mother cause they have the same ticket
#Find all class 1 people
Pclass1<-combination[which(combination$Pclass==1),]#Enough data
Pclass1<-data.frame(Embarked=Pclass1$Embarked)
Pclass1 <- Pclass1[complete.cases(Pclass1),] #Delete the missing ones
Pclass1 <- data.frame(Embarked=Pclass1)#change the type to data.frame to make preparation for ggplots2

pic_Pclass1<- ggplot(Pclass1,aes(x=Embarked)) + geom_bar(stat="count",width=0.5, fill='gray')
#The graph shows that S is the most possible farry they embarked, assign it, however, C is too close to s to judge which farry they were from, so we add the fare to plots
Pclass1_Fare<-combination[which(combination$Pclass==1),]
Pclass1_Fare<-data.frame(Embarked=Pclass1_Fare$Embarked,Fare=Pclass1_Fare$Fare)
Pclass1_Fare <- Pclass1_Fare[complete.cases(Pclass1_Fare),] #Delete the missing ones

Pclass1_Fare<-Pclass1_Fare[which(Pclass1_Fare$Fare>60 & Pclass1_Fare$Fare<100),]
ggplot(data=Pclass1_Fare, mapping=aes(x=Fare,fill=Embarked))+geom_bar(stat="count",width=0.5,position='dodge')+
  scale_fill_manual(values=c('gray','blue','red'))+geom_text(stat='count',aes(label=..count..), color="black", size=3.5,position=position_dodge(0.5),vjust=-0.5)+theme_minimal()
#X crashes, arrow it

Pclass1_Fare<-Pclass1_Fare[which(Pclass1_Fare$Fare>75 & Pclass1_Fare$Fare<85),]
ggplot(data=Pclass1_Fare, mapping=aes(x=Fare,fill=Embarked))+geom_bar(stat="count",width=0.5,position='dodge')+
  scale_fill_manual(values=c('gray','blue','red'))+geom_text(stat='count',aes(label=..count..), color="black", size=3.5,position=position_dodge(0.5),vjust=-0.5)+theme_minimal()
#Still canot distinguish clearly, class should be added


Pclass_Fare<-data.frame(Embarked=combination$Embarked,Fare=combination$Fare,Pclass=combination$Pclass)
Pclass_Fare <- Pclass_Fare[complete.cases(Pclass_Fare),] #Delete the missing ones

#ggplot(data=Pclass_Fare, mapping=aes(x=integer(Pclass),y=Fare,fill=factor(Embarked)))+geom_bar(stat="identity",width=0.5,position='dodge')+
#  scale_fill_manual(values=c('gray','blue','red'))+geom_text(aes(label=..count..), color="black", size=3.5,position=position_dodge(0.5),vjust=-0.5)+theme_minimal()
#At present, we have not found a way to make the bar chart reasonably express these three parameters:Error in integer(Pclass) : 'length'参数不对 
#So we changed to a boxplot

pic_Pclass_Fare<-ggplot(Pclass_Fare, mapping=aes(x=Embarked, y=Fare, fill=Pclass)) +geom_boxplot() +
  geom_hline(aes(yintercept=80), colour='gray', lwd=2)+scale_fill_manual(values=c('gray','blue','red'))
#Finally, from this graph, we can get that most Pclass 1 people trend to embarked at C

combination[(is.na(combination$Embarked)),"Embarked"]<- 'C'
#test
print(combination[combination$PassengerId==62,])
print(combination[combination$PassengerId==830,])

#Fill the Ages*******************************************************************************
#There are 263 people missing the Age info.
#get and print them
print(No_age<-combination[which(is.na(combination$Age)),])
#Lack of age than the Fare and Embarked tend to be Missing Completely at Random, because not only from the lack of a lot of people, we almost can't find the rule.
#But also, in fact, Age can cause deficits for a variety of reasons.
#Therefore, we used random forest to make the age distribution of the missing person consistent with the age distribution of the non-missing person
#get people have age info
Have_age<-combination[which(!is.na(combination$Age)),]
#Print the original age composition. Sure enough, close to a normal distribution
pic_Have_age<-ggplot(Have_age,aes(x=Age)) + geom_bar(stat="count",width=0.5, fill='gray')

#use the mice ramdom forest
no_need <- c('PassengerId','Ticket','Cabin','Survived')
result<- mice(combination[, !names(combination) %in% no_need], method="rf")  
new_age <- complete(result)

#show the composition predict by ramdom forest
pic_new_age<-ggplot(new_age,aes(x=Age)) + geom_bar(stat="count",width=0.5, fill='gray')
#The distribution is similar, so it works
#replace
combination$Age<-new_age$Age

#Consider the Cobin*******************************************************************************
#There are 1014 out of 1309 people, that's so many, so the Cabin doesn't have much reference value and is ignored




#Anlysis part
#The following processing is mostly obtained by format 1, except for fare, since many attempts have failed

#Survival and Fare*******************************************************************************
#known_survival<-combination[!is.na(combination$Survived)]
#pic_survival_fare<-ggplot(known_survival,aes(x=Fare,y=Survived)) + geom_bar(width=0.5, fill='gray')
#We can get that the people who paied higher fare get the higher possibilty to survive

#Survival and Ebarked *******************************************************************************
#pic_survival_Embarked<-ggplot(known_survival,aes(x=Fare,y=Embarked)) + geom_bar(width=0.5, fill='gray')

#Name
combination$New_Name<-sapply(as.character(combination$Name),FUN=function(x){strsplit(x,split='[,.]')[[1]][2]})
combination$New_Name<-sub(" ","",combination$New_Name)
others<-c('Capt','Col','Don','Dona','Jonkheer','Lady','Major','Sir','the Countess')
combination$New_Name[combination$New_Name=='Mlle']<-'Miss'
combination$New_Name[combination$New_Name=='Mme']<-'Mrs'
combination$New_Name[combination$New_Name=='Ms']<-'Miss'
combination$New_Name[combination$New_Name %in% others]<-'others'

#FammilySize
combination$FamilySize <- combination$Parch + combination$SibSp + 1

combination$PossSize <- 0
for (i in 1:1309){
  for (j in 1:1309){
    if(combination$Ticket[i]==combination$Ticket[j])
      combination$PossSize[i]=combination$PossSize[i]+1
  }
}
print(data.frame(combination$PossSize,combination$FamilySize))
for (i in 1:1309){
  if (combination$FamilySize[i]<combination$PossSize[i])
    combination$FamilySize[i]=combination$PossSize[i]
}

#Age
combination$New_Age

#combination$New_Age <- as.numeric(combination$New_Age)
#for(i in 1309){
#  if (combination$Age[i]<=20.0)
#    combination$New_Age[i]=1
# if (combination$Age[i]>=50.0)
#    combination$New_Age[i]=3
# if (combination$Age[i]>20.0 & combination$Age[i]<50.0) 
#    combination$New_Age[i]=2
#}

#young<-combination$Age[which(combination$Age<20)]
#young<-1
#combination$New_Age[which(combination$Age<20)]<-young

#middle<-combination$Age[which(combination$Age<=20 & combination$Age>=50)]
#middle<-2
#combination$New_Age[which(combination$Age<=20 & combination$Age>=50)]<-middle

#old<-combination$Age[which(combination$Age>50)]
#old<-3
#combination$New_Age[which(combination$Age>50)]<-old
combination$New_Age<-combination$Age
combination$New_Age[which(combination$New_Age<15)]<-1
combination$New_Age[which(combination$New_Age>=15 & combination$New_Age<30)]<-2
combination$New_Age[which(combination$New_Age>=30 & combination$New_Age<45)]<-3
combination$New_Age[which(combination$New_Age>=45 & combination$New_Age<60)]<-4
combination$New_Age[which(combination$New_Age>=60)]<-5

print(combination$New_Age)
combination$New_Age<-as.factor(combination$New_Age)

#Fare distinguish to 2 group for better prediction
combination$Fare[which(combination$Fare<=80)]<-1
combination$Fare[!which(combination$Fare<=80)]<-2
combination$Fare<-as.factor(combination$Fare)

#prediction

training <- combination[1:891,]
testing <- combination[892:1309,]

result1 <- naiveBayes(Survived~Pclass+Sex+Fare+Embarked+New_Age+New_Name+FamilySize,data=training)

prediction <- predict(result1,testing)

final_data<-data.frame(PassengerId=testing$PassengerId,Survived=prediction)
write.csv(final_data,file = "C:/Users/86155/Desktop/comp1433_proj/option2_attachment/option2_attachment/proj.csv",row.names = F)


