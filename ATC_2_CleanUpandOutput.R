if( .Platform$OS.type=="windows") {Root<-"F:/"}
if( .Platform$OS.type=="unix"){Root<-"/Volumes/SanDisk/"}
RefPath<-paste(Root,"/R/Ref/",sep="");
MainRoute<-paste(Root,"Data/Reference/ATC/ATCCrawler/",sep="")
setwd(MainRoute)

ATC<-readRDS(file = "ATC.RDS")
DDDAlter<-readRDS(file = "DDDAlter.RDS") 
ATCAlter<-readRDS(file = "ATCAlter.RDS")

library(dplyr); library(magrittr); library(tidyr)

### Work 1. Built a format that is comaprable to WHO releasing file
ATC_csv<-mutate(ATC
            ,Col0 = ifelse(ATC$Lv!=5,ATC$ATC_C,"") #col0: L1-L4 atc_c
            ,Col1 = ATC_C #col1: L1-L5 atc_c
            ,Col2 = ifelse(ATC$Lv!=5, ATC$ATC_Name , "") #col2: L1-L4 atc_name
            ,Col3 = ifelse(ATC$Lv==5,substr(ATC$ATC_C,6,7),"") #col3: L5 numbering in character
            ,Col4 = ifelse(ATC$Lv==5,ATC$ATC_Name,"") #col4: L5 atc_name
            ,Col5 = "" #col5: blank
            ,Col6 = as.numeric( ATC$DDD) #col6: DDD
            ,Col7 = as.character(ATC$U)  #col7:unit
            ,Col8 = as.character(ATC$Adm.R) #col8: Route
            ,Col9 = as.character(ATC$Note)  #col8: Note
            )
ATC_csv<-ATC_csv[15:24]
a<-1;
#Replace Blank cell with meaningful ATC_C and ATC_Name for the sorting
while(a <= nrow(ATC_csv)) {
        if( ATC_csv[a,"Col1"]=="" &  ATC_csv[a,"Col4"]=="" ) {
                ATC_csv[a,"Col1"]<-ATC_csv[a-1,"Col1"]
                ATC_csv[a,"Col3"]<-ATC_csv[a-1,"Col3"]
                ATC_csv[a,"Col4"]<-ATC_csv[a-1,"Col4"]
                ATC_csv[a,"Mark"]<-1;
                
        }
        ATC_csv[a,"Ord"]<-a;
        a<-a+1;
}
ATC_csv<-arrange(ATC_csv,Ord)
ATC_csv<-arrange(ATC_csv,Col1)
ATC_F<-ATC_csv;
#Remove ATC_C and ATC_Name for Sorting
a<-1;
while(a <= nrow(ATC_csv)){
        if( is.na( ATC_csv[a,"Mark"])==FALSE ){
                ATC_csv[a,"Col1"]<-""
                ATC_csv[a,"Col3"]<-""
                ATC_csv[a,"Col4"]<-"" 
        }
        a<-a+1;
}
ATC_csv<-ATC_csv[1:10]
write.csv(x=ATC_csv, file="ATC.csv",na='',row.names = FALSE)
write.csv(ATCAlter[,1:4],"ATCAlter.csv",na='',row.names = FALSE)
write.csv(DDDAlter[,1:9],"DDDAlter.csv",na='',row.names = FALSE)
rm(ATC_csv,DDDAlter)

### Work 2. Built a format that is comaprable to working file that 
###         ATC_C/ATC_Name and DDD/route would be queried in the following section

ATC_F %>% subset(.,is.na(.$Col6)==FALSE) %>% .[,c(2,5,7:9)] %>% unique()->DDD
colnames(DDD)<-c('atc_c','atc_name','DDD','DDD_Unit','Route');
DDD %>% mutate( D=paste( .$DDD, .$DDD_Unit, .$Route, sep='$' ) ) %>% select(atc_c,atc_name,D) ->DDD
## IMPORTANT !! HORIZANTAL TRANSFORMATION
DDD %>% group_by(atc_c,atc_name) %>% mutate(D_index=row_number() ) -> DDD
DDD %>% spread(D_index,D) -> DDD
a<-character(); for(i in 1:(ncol(DDD)-2)){ a[i]<-paste('Col',as.character(i),sep=''); }; 
colnames(DDD)[3:ncol(DDD)]<-a; rm(a,i);
saveRDS(DDD,paste(RefPath,'DDD.RDS',sep = ""));
rm(DDD,ATC_F);

### Work 3. Built Table that could readly convert new/old atc_c/atc_name 
### 
ATCEd<-2018
colnames(ATC)[1:2]<-c('atc_c','atc_name'); ATC<-ATC[!ATC$atc_c=='',c(1,2,7)];row.names(ATC)<-NULL;
ATC$Yr<-ATCEd;
ATC<-ATC %>% arrange(atc_c,Lv);
ATCm<-as.data.frame(mapply(c,ATC[,c(1:2,4)],ATCAlter[,c(1:2,4) ]),stringsAsFactors = FALSE ) ;
ATCm %>% unique(.) %>% arrange(atc_c,Yr) %>% .[!duplicated(.$atc_c, fromLast = TRUE), ] ->ATCm 
rownames(ATCm)<-NULL;
ATCm$Lv<-nchar(ATCm$atc_c);
rm(ATC,ATCAlter);

ATCm %>% group_by(atc_name) %>% count(.,sort=TRUE)  %>% subset(n>20) -> tb
keyword<-unlist( tb[1,1], use.names = FALSE)
inner_join(ATCm,tb,by='atc_name') %>% mutate(lv5=substr(atc_c,1,5)) %>% select(atc_c,lv5) %>% 
        unique(.) %>%  left_join(.,ATCm[ATCm$Yr==ATCEd,c(1,2)],by=c('lv5'='atc_c')) %>%
        mutate(atc_name_m=paste(.$atc_name,keyword,sep='_')) %>% select(atc_c,atc_name_m) %>%
        left_join(ATCm,.,by='atc_c') -> ATCm; rm(tb,keyword);
ATCm$atc_name<-ifelse(is.na(ATCm$atc_name_m)==TRUE,ATCm$atc_name,ATCm$atc_name_m);
ATCm<-ATCm[,1:3];
saveRDS(ATCm,paste(RefPath,'ATCm.RDS',sep = ""));
rm(ATCEd,ATCm)


## Objective: identify the Drug Name from given libraries and the result would be used in sentence key word capture.

source(paste(Root,'R/Functions/F_TM.R',sep = ''),encoding = 'utf8')

# Generic Name: Library: WHO ATC;
list.files(RefPath, pattern = 'RDS', full.names = TRUE) %>% grep('ATCm',.,value = TRUE) %>% readRDS ->lb
lb %<>% select(atc_c,atc_name) %>% mutate(atc_name=tolower(atc_name));
  # Identify ATC that with multiple ingredinces
  multi_lv<- grep('combination|and|\\,',lb$atc_name) %>% lb[., ]
    ## there are some error included term should be excluded( with 'and' in the higher level)
    multi_lv %<>% .[!nchar(.$atc_c)==1, ]
    multi_lv %>% .[nchar(.$atc_c)!=7, ] %>% .[ grep('combination',.$atc_name), ] -> comb
      ## for higher level that is combination form, we take all the subsequence level as mlutiple ingredient ones
      comb %<>% mutate(lv2=substr(.$atc_c,1,3),lv3=substr(.$atc_c,1,4),lv4=substr(.$atc_c,1,5) ) %>% arrange(lv2,lv3,lv4,atc_c)
        comb[comb$lv3==comb$lv4,'lv3'] %>% unique->l
        comb %<>% .[ .$lv3 %in% l & nchar(.$atc_c)>4 , ] %>% setdiff(comb,.) %>% select(atc_c,atc_name) %>% mutate(multi=1); rm(l)
        nchar(comb$atc_c) %>% unique->lv; lb %<>% mutate(lv3=substr(.$atc_c,1,4), lv4=substr(.$atc_c,1,5)); lvs<-c(rep('',3),'lv3','lv4');
        multi_lv %<>% .[nchar(.$atc_c)==7, ]
        for (i in lv){  inner_join(comb[nchar(comb$atc_c)==i,],lb, by=c("atc_c"=lvs[i] ) ) %>% 
                        select(atc_c.y,atc_name.y) %>% rename(atc_c=atc_c.y, atc_name=atc_name.y) %>% 
                        rbind(multi_lv,.) ->multi_lv; }; rm(lvs,i,lv,comb); 
    lb %<>% select(atc_c,atc_name); multi_lv %>% mutate(multi=1) %>% select(atc_c,multi) %>% left_join(lb,.,by='atc_c')->lb
    ## take mulitple drug ingredients string into different terms.
    multi_lv %<>% unique.data.frame(.) %>% arrange(atc_c);
    multi_lv %>% filter(nchar(atc_c)==7) %>% select(atc_name) %>% unlist(.) %>% stringr::str_split(.,pattern='and\\s+|\\,|combinations with') %>%
                lapply(.,function(x)gsub('^\\s+|\\s+$','',x)) %>% lapply(.,function(x) x[x!='']) ->nmlist; 
    names(nmlist)<- multi_lv %>% filter(nchar(atc_c)==7) %>% select(atc_c) %>% unlist(.)
    nmlist_tfidf<-TFIDF(nmlist) 
    nmlist_tfidf %>% filter(df>=20) %>% select(term) %>% unlist(.) %>% unique(.) %>% 
                grep('combination|psycho',.,value = TRUE) ->kw;
    nmlist_tfidf[nmlist_tfidf$term %in% kw, 'label']<-1; rm(kw)
    nmlist_tfidf<- data.frame(grp=as.factor( seq(1,length(nmlist))) , atc_c=names(nmlist),stringsAsFactors = FALSE ) %>% left_join(nmlist_tfidf,.,by='grp') %>%
            select(atc_c,term,label) %>% unique.data.frame(.) %>% filter(is.na(label)==TRUE) %>% select(-label)
    rm(multi_lv,nmlist);
    
    lb %<>% left_join(.,nmlist_tfidf,by='atc_c') %>% unique.data.frame(.) %>%mutate(term= ifelse(is.na(.$multi)==TRUE,.$atc_name,.$term)  );
    rm(nmlist_tfidf);
    
ATCIn<- list.files(RefPath, pattern = 'RDS', full.names = TRUE) %>% grep('ATCm',.,value = TRUE) %>% readRDS %>% mutate(atc_names=tolower(atc_name) ) %>%
      left_join(.,lb,by=c('atc_c'='atc_c', 'atc_names'='atc_name')) %>% select(-atc_names); rm(lb)
ATCIn$lv<-lapply(nchar(ATCIn$atc_c), function(x) grep(x,c(1,3,4,5,7))) %>% unlist(.)
ATCIn %<>% mutate( len=nchar(term)) %>% filter(len>3) %>% select(-len);
saveRDS(ATCIn,paste( RefPath,'ATCIn.RDS',sep = ''))




