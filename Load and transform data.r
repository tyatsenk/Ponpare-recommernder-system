#This code reads csv. files downloaded from 
#https://www.kaggle.com/c/coupon-purchase-prediction/data and performs data type conversion



###Set working directory to the directory which contains scv. files

setwd("/Users/tyatsenk/Documents/Ryerson/Data Analytics/Capstone/data")

#########################################################
#Read csv. files and create data frames from it
#########################################################



train<-read.csv("coupon_list_train.csv", as.is=TRUE) 
        #argument as.is=TRUE suppresses conversion of character vectors to factors
test<-read.csv("coupon_list_test.csv", as.is=TRUE)
user_list<-read.csv("user_list.csv", as.is=TRUE)
coupon_detail_train<-read.csv("coupon_detail_train.csv", as.is=TRUE)
coupon_area_test<-read.csv("coupon_area_test.csv", as.is=TRUE)
coupon_area_train<-read.csv("coupon_area_train.csv", as.is=TRUE)
coupon_visit_train<-read.csv("coupon_visit_train.csv", as.is=TRUE)
prefecture_locations<-read.csv("prefecture_locations.csv", as.is=TRUE)


#########################################################
#Translate Japaneese text into English
#########################################################

################### For train  #############################

translation<-data.frame(Japanese=unique(c(train$CAPSULE_TEXT, train$GENRE_NAME, 
                train$large_area_name, train$ken_name, train$small_area_name)), 
                English=c("Food", "Hair salon", "Spa", "Relaxation","Beauty", 
                        "Nail and eye salon","Delivery service","Lesson",
                        "Gift card","Other coupon","Leisure",
                        "Hotel and Japanese hotel","Health and medical","Other",
                        "Hotel","Japanese hotel","Vacation rental","Lodge",
                        "Resort inn","Guest house","Japanse guest house",
                        "Public hotel","Beauty","Event","Web service","Class",
                        "Correspondence course","Kanto","Kansai","East Sea",
                        "Hokkaido","Kyushu-Okinawa","Northeast","Shikoku",
                        "China","Hokushinetsu","Saitama Prefecture",
                        "Chiba Prefecture","Tokyo","Kyoto","Aichi Prefecture",
                        "Kanagawa Prefecture","Fukuoka Prefecture",
                        "Tochigi Prefecture","Osaka prefecture","Miyagi Prefecture",
                        "Fukushima Prefecture","Oita Prefecture","Kochi Prefecture",
                        "Hiroshima Prefecture","Niigata Prefecture",
                        "Okayama Prefecture","Ehime Prefecture","Kagawa Prefecture",
                        "Tokushima Prefecture","Hyogo Prefecture","Gifu Prefecture",
                        "Miyazaki Prefecture","Nagasaki Prefecture", 
                        "Ishikawa Prefecture","Yamagata Prefecture","Shizuoka Prefecture",
                        "Aomori Prefecture", "Okinawa","Akita","Nagano Prefecture",
                        "Iwate Prefecture","Kumamoto Prefecture",
                        "Yamaguchi Prefecture","Saga Prefecture","Nara Prefecture",
                        "Mie","Gunma Prefecture","Wakayama Prefecture",
                        "Yamanashi Prefecture","Tottori Prefecture","Kagoshima prefecture",
                        "Fukui Prefecture","Shiga Prefecture","Toyama Prefecture",
                        "Shimane Prefecture","Ibaraki Prefecture","Saitama","Chiba",
                        "Shinjuku, Takadanobaba Nakano - Kichijoji","Kyoto",
                        "Ebisu, Meguro Shinagawa","Ginza Shinbashi, Tokyo, Ueno",
                        "Aichi","Kawasaki, Shonan-Hakone other","Fukuoka","Tochigi",
                        "Minami other","Shibuya, Aoyama, Jiyugaoka",
                        "Ikebukuro Kagurazaka-Akabane","Akasaka, Roppongi, Azabu",
                        "Yokohama","Miyagi","Fukushima","Much","Kochi",
                        "Tachikawa Machida, Hachioji other","Hiroshima","Niigata",
                        "Okayama","Ehime","Kagawa","Northern","Tokushima","Hyogo",
                        "Gifu","Miyazaki","Nagasaki","Ishikawa","Yamagata",
                        "Shizuoka","Aomori","Okinawa","Akita","Nagano","Iwate",
                        "Kumamoto","Yamaguchi","Saga","Nara","Triple","Gunma",
                        "Wakayama","Yamanashi","Tottori","Kagoshima","Fukui",
                        "Shiga","Toyama","Shimane","Ibaraki"), 
         stringsAsFactors=FALSE) #stringsAsFactors=FALSE 
                                #because we have some text fields

# Merge translated data with original data colum by column:
names(translation)<-c("Japanese", "CAPSULE_TEXT_en")
train<-merge(train, translation, by.x="CAPSULE_TEXT", by.y="Japanese", all.x=TRUE)
names(translation)<-c("Japanese", "GENRE_NAME_en")
train<-merge(train, translation, by.x="GENRE_NAME", by.y="Japanese", all.x=TRUE)
names(translation)<-c("Japanese", "large_area_name_en")
train<-merge(train, translation, by.x="large_area_name", by.y="Japanese", all.x=TRUE)
names(translation)<-c("Japanese", "ken_name_en")
train<-merge(train, translation, by.x="ken_name", by.y="Japanese", all.x=TRUE)
names(translation)<-c("Japanese", "small_area_name_en")
train<-merge(train, translation, by.x="small_area_name", by.y="Japanese", all.x=TRUE)

################### For test #############################

names(translation)<-c("Japanese", "CAPSULE_TEXT_en")
test<-merge(test, translation, by.x="CAPSULE_TEXT", by.y="Japanese", all.x=TRUE)
names(translation)<-c("Japanese", "GENRE_NAME_en")
test<-merge(test, translation, by.x="GENRE_NAME", by.y="Japanese", all.x=TRUE)
names(translation)<-c("Japanese", "large_area_name_en")
test<-merge(test, translation, by.x="large_area_name", by.y="Japanese", all.x=TRUE)
names(translation)<-c("Japanese", "ken_name_en")
test<-merge(test, translation, by.x="ken_name", by.y="Japanese", all.x=TRUE)
names(translation)<-c("Japanese", "small_area_name_en")
test<-merge(test, translation, by.x="small_area_name", by.y="Japanese", all.x=TRUE)

################### For user_list #############################

user_list$PREF_NAME[(user_list$PREF_NAME=="")] <- NA #replace empty cells in PREF_NAME for NA
user_list_jp<-unique(user_list$PREF_NAME)

user_list_en<-c("NA", "Tokyo", "Aichi Prefecture", "Kanagawa Prefecture", 
                "Hiroshima Prefecture", "Saitama Prefecture", "Nara Prefecture",
                "Ishikawa Prefecture", "Osaka prefecture",
                "Kumamoto Prefecture", "Fukuoka Prefecture", "Hokkaido", "Kyoto", 
                "Akita", "Chiba Prefecture", "Nagasaki Prefecture", 
                "Hyogo Prefecture", "Okinawa","Mie", "Ibaraki Prefecture", 
                "Kagoshima prefecture", "Miyagi Prefecture", "Shizuoka Prefecture", 
                "Wakayama Prefecture", "Nagano Prefecture", "Okayama Prefecture", 
                "Tochigi Prefecture","Shiga Prefecture", "Toyama Prefecture", 
                "Saga Prefecture", "Miyazaki Prefecture", "Iwate Prefecture", 
                "Niigata Prefecture", "Oita Prefecture", "Yamaguchi Prefecture", 
                "Gifu Prefecture","Gunma Prefecture", "Fukushima Prefecture", 
                "Ehime Prefecture", "Kagawa Prefecture", "Yamanashi Prefecture", 
                "Kochi Prefecture", "Shimane Prefecture", "Tokushima Prefecture", 
                "Fukui Prefecture","Aomori Prefecture", "Yamagata Prefecture", 
                "Tottori Prefecture")

translation_user_list<-data.frame(user_list_jp, user_list_en, stringsAsFactors=FALSE)

names(translation_user_list)<-c("Japanese", "PREF_NAME_en")
user_list<-merge(user_list, translation_user_list, by.x="PREF_NAME", by.y="Japanese", 
                 all.x=TRUE)



################### For coupon_detail_train #############################

names(translation)<-c("Japanese", "SMALL_AREA_NAME_en")
coupon_detail_train<-merge(coupon_detail_train, translation, by.x="SMALL_AREA_NAME", 
                           by.y="Japanese", all.x=TRUE)



################### For coupon_area_test #############################

names(translation)<-c("Japanese", "SMALL_AREA_NAME_en")
coupon_area_test<-merge(coupon_area_test, translation, by.x="SMALL_AREA_NAME", 
                        by.y="Japanese", all.x=TRUE)
names(translation)<-c("Japanese", "PREF_NAME_en")
coupon_area_test<-merge(coupon_area_test, translation, by.x="PREF_NAME",
                        by.y="Japanese", all.x=TRUE)



################### For coupon_area_train #############################

names(translation)<-c("Japanese", "SMALL_AREA_NAME_en")
coupon_area_train<-merge(coupon_area_train, translation, by.x="SMALL_AREA_NAME", 
                         by.y="Japanese", all.x=TRUE)
names(translation)<-c("Japanese", "PREF_NAME_en")
coupon_area_train<-merge(coupon_area_train, translation, by.x="PREF_NAME", 
                         by.y="Japanese", all.x=TRUE)


################## For prefecture_locations ############################

names(translation)<-c("Japanese", "PREF_NAME_en")
prefecture_locations<-merge(prefecture_locations, translation, by.x="PREF_NAME", 
                            by.y="Japanese", all.x=TRUE)

prefecture_jp<-unique(prefecture_locations$PREFECTUAL_OFFICE)
prefecture_en<-c("Tsu", "Kyoto City", "Saga", "Kobe", "Sapporo", "Chiba", 
                 "Wakayama", "Saitama", "Oita", "Osaka", "Nara", "Sendai", 
                 "Miyazaki", "Toyama", "Yamaguchi", "Yamagata",
                 "Kofu", "Gifu", "Okayama", "Morioka", "Matsue", "Hiroshima", 
                 "Tokushima", "Matsuyama","Nagoya city", "Niigata", "Shinjuku",
                 "Utsunomiya", "Naha", "Otsu", "Kumamoto", "Kanazawa", 
                 "Yokohama", "Fukui", "Fukuoka", "Fukushima", "Akita", "Maebashi", 
                 "Mito", "Nagasaki","Nagano", "Aomori", "Shizuoka", "Takamatsu", 
                 "Kochi","Tottori", "Kagoshima")
translation_prefecture<-data.frame(prefecture_jp, prefecture_en, stringsAsFactors=FALSE)

names(translation_prefecture)<-c("Japanese", "PREFECTUAL_OFFICE_en")
prefecture_locations<-merge(prefecture_locations, translation_prefecture, 
                            by.x="PREFECTUAL_OFFICE", by.y="Japanese", all.x=TRUE)


#########################################################
#Data type conversion
#########################################################