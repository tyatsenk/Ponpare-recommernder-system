# This script calculates modified cosine similarites for Ponpare coupon competition

# Note: Weights in a weight matrix were found using ideas by Subhajit Mandal and leaderboard feedback

#"Load and transform data.R" must be run before this code to load and transform data

# Make extended train data set: merge train(coupon_list_train.csv) and
#coupon_detail_train by common column COUPON_ID_hash
train_detail<-merge(train, coupon_detail_train, by="COUPON_ID_hash")
train_detail<-train_detail[,c("COUPON_ID_hash", "USER_ID_hash", "GENRE_NAME_en",
                       "DISCOUNT_PRICE", "PRICE_RATE", "USABLE_DATE_MON",
                       "USABLE_DATE_TUE", "USABLE_DATE_WED", "USABLE_DATE_THU",
                       "USABLE_DATE_FRI", "USABLE_DATE_SAT", "USABLE_DATE_SUN",
                       "USABLE_DATE_HOLIDAY", "USABLE_DATE_BEFORE_HOLIDAY",
                       "large_area_name_en","ken_name_en", "small_area_name_en")]

                        
# Combine the test data with train_detail:
test$USER_ID_hash<-"dummyuser"
test<-test[,c("COUPON_ID_hash", "USER_ID_hash", "GENRE_NAME_en",
              "DISCOUNT_PRICE", "PRICE_RATE", "USABLE_DATE_MON",
              "USABLE_DATE_TUE", "USABLE_DATE_WED", "USABLE_DATE_THU",
              "USABLE_DATE_FRI", "USABLE_DATE_SAT", "USABLE_DATE_SUN",
              "USABLE_DATE_HOLIDAY", "USABLE_DATE_BEFORE_HOLIDAY",
              "large_area_name_en","ken_name_en", "small_area_name_en")]

train_detail<-rbind(train_detail, test)

# Replace all NA values with "1"
train_detail[is.na(train_detail)]<-1

# It is easier to interpret the weight parameters if variables of interest are within zero and one rande,
#therefore, transformation of DISCOUNT_PRICE and PRICE_RATE should be done:
#Transform DISCOUNT_PRICE variable:
train_detail$DISCOUNT_PRICE<-1/log10(train_detail$DISCOUNT_PRICE)

#Normalize PRICE_RATE variable:
train_detail$PRICE_RATE<-(train_detail$PRICE_RATE*train_detail$PRICE_RATE)/(100*100)

#Combine by columns the first two columns of detail_train and model matrix by 
#converting factors to columns of zeroes and ones:
train_detail<-cbind(train_detail[,c(1,2)], model.matrix(~-1+ .,train_detail[,-c(1,2)],
                contrasts.arg=lapply(train_detail[,names(which(sapply(train_detail[,-c(1,2)],
                is.factor)==TRUE))], contrasts, contrasts=FASLE)))

#Split test data from detail_train:
test<-train_detail[train_detail$USER_ID_hash=="dummyuser",]

#Remove USER_ID_hash from test data set:
test<-test[,-2]
train_detail<-train_detail[train_detail$USER_ID_hash!="dummyuser",]

#Create data frame of user features:
user_features<-aggregate(.~USER_ID_hash, data=train_detail[,-1], FUN=mean)

#Create dummies to make matrix multiplication work:
user_features$DISCOUNT_PRICE<-1
user_features$PRICE_RATE<-1

#Create Weight Matrix: GENRE_NAME, DISCOUNT_PRICE, PRICE_RATE, USABLE_DATE,
#large_area_name, ken_name, small_area_name:

require(Matrix)
W_Matrix<-as.matrix(Diagonal(x=c(rep(2.05,12), #GENRE_NAME
                                 rep(2.01,1), #DISCOUNT_PRICE
                                 rep(-0.13,1), #PRICE_RATE
                                 rep(0,9), #USABLE_DATE
                                 rep(0.51,8), #large_area_name
                                 rep(1.01,46), #ken_name
                                 rep(4.75,54)))) #small_area_name

#Calculate cosine silmilarities between users and coupons:
score<-as.matrix(user_features[,2:ncol(user_features)]) %*% W_Matrix %*% t(as.matrix(test[,2:ncol(test)]))

#Create ordered list of coupons according to theirs similarities. Display first 10 coupons:
user_features$PURCHASED_COUPONS<-do.call(rbind, lapply(1:nrow(user_features), 
                                 FUN=function(i){ 
                                 purchased_coupons<-paste(test$COUPON_ID_hash[order(score[i,],
                                 decreasing=TRUE)][1:10], collapse=" ") 
                                 return(purchased_coupons)
                                 }))
#Merge urer_features, user_list together                                                               
merged<-merge(user_list, user_features, all.x=TRUE)
merged<-merged[,c("USER_ID_hash", "PURCHASED_COUPONS")]
