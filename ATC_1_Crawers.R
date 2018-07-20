#
#install.packages("mederrRank","PhViD","tidyr",'plyr')
#install.packages("xml2"); install.packages("XML"); install.packages("dplyr")
#
# 20160830: Write for Updated ATC Code Data.

######Method 2#########

## 1. Function Define 

### Define the subfunction used in RetrieveFun
LongWord<-function(Words,Paragraph,S_Pos){
        trim <- function (x) gsub("^\\s+|\\s+$", "", x);
        kn<-S_Pos;
        while(kn<=length(Words)){
                if(kn==S_Pos) { word<-Words[kn]; }
                else {
                        #For ATC Name Use
                        if (S_Pos<=3){    
                                # working only the next term exsisted and it is not a number
                                if ( is.na( as.numeric(Words[kn]))==TRUE )     
                                { word<-paste(word,Words[kn], sep = " ") } 
                                else{ word<-trim(word); outs<-list(kn,word); return(outs)}
                        }
                        ## For Note, aggregate to the end
                        else {
                                word<-paste(word,Words[kn], sep = " ")  
                        }
                }
                if (kn==length(Words)) { word<-trim(word); outs<-list(kn,word) ; return(outs) } 
                kn<-kn+1        
        }
}
### Define the subfunction used in RetrieveFun#####END

###Define Retrive and Write to DataTable Core as a function
RetrieveFun<- function(F_htmlF,F_Obs,Lv){
        
        TB<-data.frame(ATC_C=character(),ATC_Name=character(), DDD=character(), U=character(),Adm.R=character(), Note=character(),
                           Lv=integer(),ATC_C1=character(),ATC_C2=character(),
                           ATC_C3=character(),ATC_C4=character(),ATC_C5=character(),YR=integer(),Obs=integer(),stringsAsFactors=FALSE);
        
        Num=0;
        
        if (Lv!=0){
                #Level 1
                if (Lv==1){
                  xpath<-"//*[@id='content']//b//a/text()"
                  L1Name<-xmlApply(getNodeSet(F_htmlF,xpath), xmlValue)[1];
                  xpath<-"//*[@id='content']/text()"
                  L1Code<-xmlApply(getNodeSet(F_htmlF,xpath), xmlValue)[2];
                  Num<-1+Num;
                  TB[Num,"ATC_C"]<-gsub("^*[[:space:]]","",gsub("[[:space:]]*$","",L1Code) )  ; 
                  TB[Num,"ATC_C1"]<-gsub("^*[[:space:]]","",gsub("[[:space:]]*$","",L1Code) ) ; 
                  TB[Num,"Lv"]<-1 ; 
                  TB[Num,"ATC_Name"]<-gsub("^*[[:space:]]","",gsub("[[:space:]]*$","",L1Name) );
                  TB[Num,"YR"]<-substr(Sys.Date(),1,4);
                  TB[Num,"Obs"]<-Num;
                }
            
                #Level 2-4
                xpath<-"//*[@id='content']//p/text()"
                Stri<-xmlApply(getNodeSet(F_htmlF,xpath), xmlValue); Stri<-Stri[-1];
            ## 20170306 Escape pot for no information error
            if (Stri[1]!="\n") { 
                xpath<-"//*[@id='content']//p//b//a";
                Nam<-xmlApply(getNodeSet(F_htmlF,xpath), xmlValue);
                Stri<-lapply(Stri,function(x){ trimws( gsub( pattern = "\n", replacement = "", x=x, fixed = TRUE)) });
                Stri<-gsub("^*[[:space:]]","",gsub("[[:space:]]*$","",Stri) );
                Nam<-lapply(Nam,function(x){ trimws( gsub( pattern = "\n", replacement = "", x=x, fixed = TRUE))  });
                Nam<-gsub("^*[[:space:]]","",gsub("[[:space:]]*$","",Nam) );
                #Put Data into DataFrame
                for(z in 1:length(Stri)) {
                  ##z<-Start
                        Num<-1+Num;
            
                         TB[Num,"ATC_C"]<-Stri[z] ; 
                         TB[Num,"ATC_Name"]<-Nam[z];
                        
                        TB[Num,"ATC_C1"]<-substr(TB[Num,"ATC_C"],1,1);
                        TB[Num,"ATC_C2"]<-substr(TB[Num,"ATC_C"],2,3);
                        TB[Num,"ATC_C3"]<-substr(TB[Num,"ATC_C"],4,4);
                        TB[Num,"ATC_C4"]<-substr(TB[Num,"ATC_C"],5,5);
                        TB[Num,"ATC_C5"]<-substr(TB[Num,"ATC_C"],6,7);
                        if(nchar(Stri[z])==3){ TB[Num,"Lv"]<-2  }
                        if(nchar(Stri[z])==4){ TB[Num,"Lv"]<-3  }
                        if(nchar(Stri[z])==5){ TB[Num,"Lv"]<-4  }
                        TB[Num,"YR"]<-substr(Sys.Date(),1,4);
                        TB[Num,"Obs"]<-Num;
                }
            } else {print(paste("Excape ATC",ATC) )}
        } #for Level 2-4
        else
        {
        #for Level 5 
                xpath<-"//*[@id='content']//ul//tr//td";
                ##if no Level 5 data, the return would be null;
                Box<-getNodeSet(t_HTML,xpath);
          if (length(Box)!=0) {
                Stri<-xmlApply(getNodeSet(t_HTML,xpath), xmlValue);
                Stri<-gsub( pattern = "   " , replacement = "", 
                            x= gsub(  pattern = "A<82>A<a0>", replacement =" " ,x =Stri, fixed = TRUE), fixed = TRUE );
                Stri<-gsub("^*[[:space:]]","",gsub("[[:space:]]*$","",Stri) ) ; zi<-6;
                #Writing
                while(zi<length(Stri)){
                  Num<-Num+1;
                  TB[Num,"Obs"]<-Num;
                  TB[Num,"Lv"]<-5;
                  TB[Num,"ATC_C"]<-Stri[zi+1];
                  TB[Num,"ATC_Name"]<-Stri[zi+2];
                  TB[Num,"DDD"]<-Stri[zi+3];
                  TB[Num,"U"]<-Stri[zi+4];
                  TB[Num,"Adm.R"]<-Stri[zi+5];
                  TB[Num,"Note"]<-Stri[zi+6];
                  zi<-zi+6;
                }
          } else{
                # no level 5 data
          }
        }          
      return(TB)
}
###Define Retrive and Write to DataTable Core as a function#####END#######
###########################################################################################
###  Define URL function
Func_ATC<-function(L1,L2,L3,L4,L5){ paste(L1,L2,L3,L4,L5,sep = "") }
Func_URL<-function(ATCs){ paste("http://www.whocc.no/atc_ddd_index/?code=",ATCs,"&showdescription=no",sep = "") }
###  Define URL function#######END########

##End Function Define

###############################################################################################

##2. Initiation

if( .Platform$OS.type=="windows") {Root<-"F:/"}
if( .Platform$OS.type=="unix"){Root<-"/Volumes/SanDisc/"}
MainRoute<-paste(Root,"Data/Reference/ATC/ATCCrawler/",sep="")
setwd(MainRoute)

library(xml2);library(dplyr);library(XML)
ATCCTb<-data.frame(ATC_C=character(),ATC_Name=character(), DDD=character(), U=character(),Adm.R=character(), Note=character(),
                   Lv=integer(),ATC_C1=character(),ATC_C2=character(),
                   ATC_C3=character(),ATC_C4=character(),ATC_C5=character(),YR=integer(),Obs=integer(),stringsAsFactors=FALSE)
ATCHead<-list("A","B","C","D","G","H","J","L","M","N","P","R","S","V")
options(warn=-1)
## Lv1<-1;
##3. Starting Rolling
for(Lv1 in 1:length(ATCHead) ) {
        ## Retrieve First and Second Level Data
        ATC<-Func_ATC( L1=ATCHead[Lv1],L2="",L3="",L4="",L5="");
        t_HTML<- htmlParse( read_html(Func_URL(ATCs =ATC) ,encoding ="UTF-8" ));
        ATCCTb<-rbind(ATCCTb,RetrieveFun(F_htmlF=t_HTML,F_Obs=nrow(ATCCTb), Lv=1));
        ATCCTb<-subset(ATCCTb,(is.na(ATC_Name)==FALSE | is.na(DDD)==FALSE) )
        
        ## Retrieve Third Level Data
       Thrd<- ATCCTb[ATCCTb$ATC_C1==ATCHead[Lv1] & ATCCTb$Lv==2 ,"ATC_C2"]
       ##Lv3<-1;
       for (Lv3 in 1:length(Thrd) ) {
                ## Retrieve Third Level Data
               ATC<-Func_ATC( L1=ATCHead[Lv1],L2=Thrd[Lv3],L3="",L4="",L5=""); 
               t_HTML<- htmlParse( read_html(Func_URL(ATCs =ATC) ,encoding ="UTF-8"));
               ATCCTb<-rbind(ATCCTb,RetrieveFun(F_htmlF=t_HTML,F_Obs=nrow(ATCCTb), Lv=3));
               ATCCTb<-subset(ATCCTb,(is.na(ATC_Name)==FALSE | is.na(DDD)==FALSE) )  
       }
       
       ## Build 4th Level Retrieve;
       Forth<-ATCCTb[ATCCTb$ATC_C3!='' & ATCCTb$ATC_C1==ATCHead[Lv1] , -c(1:8) ]; 
       Forth<-Forth[is.na(Forth$ATC_C2)==FALSE,1:2]
       ##Lv4<-17
       for(Lv4 in 1:length(Forth[,1])){
               ## Retrieve Forth Level Data
               ATC<-Func_ATC( L1=ATCHead[Lv1],L2=Forth[Lv4,1],L3=Forth[Lv4,2],L4="",L5="");
               t_HTML<- htmlParse( read_html(Func_URL(ATCs =ATC) ,encoding ="UTF-8" ));
               ATCCTb<-rbind(ATCCTb,RetrieveFun(F_htmlF=t_HTML,F_Obs=nrow(ATCCTb), Lv=4));
               ATCCTb<-subset(ATCCTb,(is.na(ATC_Name)==FALSE | is.na(DDD)==FALSE) )  
       }

}
saveRDS(ATCCTb,'ATCCTb.RDS')

## Clean Up
rm(Forth)
ATCCTb<-ATCCTb[,-14];
ATCCTb<-unique(x=ATCCTb, incomparables = FALSE)
ATCCTb$Obs<-nrow(ATCCTb)

## Retrieve final level 5; Lv5<-560;
Fifth<-ATCCTb[ATCCTb$Lv==4, -c(1:7,12:14) ]; rownames(Fifth)<-NULL;
for(Lv5 in 1:length(Fifth[,1]) ){
        ## Retrieve Forth Level Data
        print(paste("Now in",Lv5,"of",length(Fifth[,1]),"@",Sys.time()));
        rt<-list(Fifth[Lv5,1],Lv5,length(Fifth[,1]))
        ATC<-Func_ATC( L1=Fifth[Lv5,1],L2=Fifth[Lv5,2],L3=Fifth[Lv5,3],L4=Fifth[Lv5,4],L5="");
        t_HTML<- htmlParse( read_html(Func_URL(ATCs =ATC),encoding ="UTF-8",trim=FALSE)); 
        ATCCTb<-rbind(ATCCTb,RetrieveFun(F_htmlF=t_HTML,F_Obs=nrow(ATCCTb), Lv=0));
}
rm(Fifth)
saveRDS(ATCCTb,file = "ATC.RDS"); 


###########END####################

##DDD Alteration
DDDUrl<-"http://www.whocc.no/atc_ddd_alterations__cumulative/ddd_alterations/"
t_DDDAlter<- htmlParse( read_html(DDDUrl) )
xpath<-"//*[@class='listtable']//td"
DDDText<-xmlApply(getNodeSet(t_DDDAlter,xpath), xmlValue)
DDDText<-trimws(DDDText);

DDD<-data.frame(ATC_Name=character(),Pre_DDD=numeric(), Pre_DDD_U=character(), Pre_DDD_Route=character()
                , New_DDD=numeric(), New_DDD_U=character(), New_DDD_Route=character()
                , ATC_C=character(),Yr=integer(),Obs=integer(),stringsAsFactors=FALSE); 
i<-4;Num<-1;
while (i<=length(DDDText)) {
        DDD[Num,"ATC_Name"]<-DDDText[i] ;
        DDD[Num,"Pre_DDD"]<-as.numeric( gsub(pattern="[[:blank:]][[:digit:]]{1,2})",replacement = "",x=DDDText[i+1]) );
        DDD[Num,"Pre_DDD_U"]<-gsub(pattern="[[:blank:]][[:digit:]]{1,2})",replacement = "",x=DDDText[i+2]);
        DDD[Num,"Pre_DDD_Route"]<-gsub(pattern="[[:blank:]][[:digit:]]{1,2})",replacement = "",x=DDDText[i+3]);
        DDD[Num,"New_DDD"]<-as.numeric(  gsub(pattern="[[:blank:]][[:digit:]]{1,2})",replacement = "",x=DDDText[i+4]) );
        DDD[Num,"New_DDD_U"]<-gsub(pattern="[[:blank:]][[:digit:]]{1,2})",replacement = "",x=DDDText[i+5]);
        DDD[Num,"New_DDD_Route"]<-gsub(pattern="[[:blank:]][[:digit:]]{1,2})",replacement = "",x=DDDText[i+6]);
        DDD[Num,"ATC_C"]<-DDDText[i+7];
        DDD[Num,"Yr"]<-DDDText[i+8];
        DDD[Num,"Obs"]<-Num;
        Num<-Num+1; i<-i+9;
}


##ATC Alteration
ATCUrl<-"http://www.whocc.no/atc_ddd_alterations__cumulative/atc_alterations/"
t_ATCAlter<- htmlParse( read_html(ATCUrl) )
xpath<-"//*[@class='listtable']//td"
ATCText<-xmlApply(getNodeSet(t_ATCAlter,xpath), xmlValue)
ATCText<-trimws(ATCText);

ATC<-data.frame(Pre_ATC_C=character(),ATC_Name=character(),New_ATC_C=character()
                ,Yr=integer(),Obs=integer(),stringsAsFactors=FALSE); 
i<-2;Num<-1;
while (i<length(ATCText)) {
        ATC[Num,"Pre_ATC_C"]<-gsub(pattern="[[:blank:]][[:digit:]]{1,2})",replacement = "",x=ATCText[i]);
        ATC[Num,"ATC_Name"]<-gsub(pattern="[[:blank:]][[:digit:]]{1,2})",replacement = "",x=ATCText[i+1]);
        ATC[Num,"New_ATC_C"]<-gsub(pattern="[[:blank:]][[:digit:]]{1,2})",replacement = "",x=ATCText[i+2]);
        ATC[Num,"Yr"]<-ATCText[i+3];
        ATC[Num,"Obs"]<-Num;
        Num<-Num+1; i<-i+4;
}
for (i in 1:nrow(ATC)){
        for(k in 1:3){
                if (regexpr(text = ATC[i,k], pattern = "??[[:blank:]][[:digit:]]")[1]!=-1 )
                { ATC[i,k]<-trimws( substr(x=ATC[i,k],1,regexpr(text = ATC[i,k], pattern = "??[[:blank:]][[:digit:]]")[1]-1) )  }
                if (regexpr(text = ATC[i,k], pattern = "A<a0>", fixed = TRUE)[1]!=-1 )
                { ATC[i,k]<-trimws( substr(x=ATC[i,k],1,regexpr(text = ATC[i,k], pattern = "A<a0>", fixed=TRUE)[1]-1) )  }
          
                if (regexpr(text = ATC[i,k], pattern = "[[:punct:]]" )[1]!=-1& (k==1 | k==3 ))
                { ATC[i,k]<-trimws( substr(x=ATC[i,k],1,regexpr(text = ATC[i,k], pattern = "[[:punct:]]" )[1]-1) )   }
        }
}
###Write all DB into File
saveRDS(ATC,file = "ATCAlter.RDS");  
saveRDS(DDD,file = "DDDAlter.RDS");