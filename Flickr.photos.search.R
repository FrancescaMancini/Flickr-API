library(RCurl)
library(XML)
library(httr)

api_key<-"your_api_key_here"

secret<- "your_secret_here"

myapp<-oauth_app("your_app_name_here",key= api_key,secret= secret)                  #creates the app passing the key and secret




ep<-oauth_endpoint(request="https://www.flickr.com/services/oauth/request_token"    #get authentication credentials from the API
                    ,authorize="https://www.flickr.com/services/oauth/authorize",
                    access="https://www.flickr.com/services/oauth/access_token")


sig<-oauth1.0_token(ep,myapp,cache=F)                                             #creates variable with authentication credentials

fl_sig <- sign_oauth1.0(myapp,sig)

baseURL <- paste("https://api.flickr.com/services/rest/?method=flickr.photos.search&api_key=",api_key,sep="")   #set base URL


pics<-NULL                              #creates empty object
year<-seq(2005,2014,1)                  #creates variable "year" so that query returns pictures taken between 2005 and 2014
text<-"bird"                            #set keywords so that query returns pictures with the text "bird"
woeid<-"12578048"                       #only return pictures taken in Scotland
hasgeo<-"1"                             #only return pictures that are geotagged
extras<-"date_taken,geo,tags"           #extra information to download
perpage<-"250"                          #number of results to return per page
format<-"rest"                          #format of results


 for (y in 1:length(year)){                     #creates object dates
      for (m in 1:12){ daymin<-"01"

           ifelse ((m==4|m==6|m==9|m==11), daymax<-"30",daymax<-"31")
           if (m==2){
           ifelse (year[y]==2008|year[y]==2012, daymax<-29,daymax<-28)
                    }

           ifelse (m==10|m==11|m==12,
                    mindate<-as.character(paste(year[y],m,daymin,sep="-")),
                      mindate<-as.character(paste(year[y],paste("0",m,sep="")
                      ,daymin,sep="-")))

           ifelse (m==10|m==11|m==12,
                    maxdate<-as.character(paste(year[y],m,daymax,sep="-")),
                      maxdate<-as.character(paste(year[y],paste("0",m,sep="")
                      ,daymax,sep="-")))



           getPhotos <- paste(baseURL                                           #request URL
                         ,"&text=",text,"&min_taken_date=",mindate,
                         "&max_taken_date=",maxdate,"&woe_id=",woeid,
                         "&has_geo=",hasgeo,"&extras=",extras,
                         "&per_page=",perpage,"&format=",format,sep="")



           getPhotos_data <- xmlRoot(xmlTreeParse(getURL                                    #parse URL and extract root node
                             (getPhotos,ssl.verifypeer=FALSE, useragent = "flickr") ))


           #parse the total number of pages
           pages_data <- data.frame(xmlAttrs(getPhotos_data[["photos"]]))
           pages_data[] <- lapply(pages_data, as.character)
           pages_data[] <- lapply(pages_data, as.integer)
           colnames(pages_data)<- "value"
           total_pages <- pages_data["pages","value"]

           pics_tmp<-NULL


           # loop thru pages of photos and save the list in a DF
           for(i in c(1:total_pages)){
               getPhotos <- paste(baseURL
                           ,"&text=",text,"&min_taken_date=",mindate,
                         "&max_taken_date=",maxdate,"&woe_id=",woeid,
                         "&has_geo=",hasgeo,"&extras=",extras,
                         "&per_page=",perpage,"&format=",format,"&page="
                           ,i,sep="")

                getPhotos_data <- xmlRoot(xmlTreeParse(getURL
                  (getPhotos,ssl.verifypeer=FALSE, useragent = "flickr")
                  ,useInternalNodes = TRUE ))

                  id<-xpathSApply(getPhotos_data,"//photo",xmlGetAttr,"id")                 #extract photo id
                  owner<-xpathSApply(getPhotos_data,"//photo",xmlGetAttr,"owner")           #extract user id
                  datetaken<-xpathSApply(getPhotos_data,"//photo",xmlGetAttr,"datetaken")   #extract date picture was taken
                  tags<- xpathSApply(getPhotos_data,"//photo",xmlGetAttr,"tags")            #extract tags
                  latitude<- xpathSApply(getPhotos_data,"//photo",xmlGetAttr,"latitude")    #extract latitude
                  longitude<- xpathSApply(getPhotos_data,"//photo",xmlGetAttr,"longitude")  #extract longitude

                  tmp_df<-data.frame(cbind(id,owner,datetaken,tags,latitude,longitude),stringsAsFactors=FALSE)

                  tmp_df$page <- i
                  pics_tmp<-rbind(pics_tmp,tmp_df)
}


                  pics<-rbind(pics,pics_tmp)
}}

