#######Get data######
CreateData <- function(study,aevar,pdtable)
{
  subj.site <- GetSubjSite(study,"subjects_and_orgs")
  all <- GetAll(study,"subjects_and_orgs")
  onstudy <- GetOnStudy(study,"subjects_and_orgs")
  timeonstudy <- GetStartDate(study,"subjects_and_orgs")
  nae <- GetAE(study,"ae",aevar,"nae",subj.site)
  npd <- GetEvents(study,pdtable,"npd",subj.site)
  query <- GetQuery(study,"query_status")
  querytime <- GetQueryTime(study,"query_status")
  
  #Combine all the data
  data <- NULL
  data <- merge(x=all,y=onstudy,by="orgidentifier",all.x=T)
  data <- merge(x=data,y=timeonstudy,by="orgidentifier",all.x=T)
  data <- merge(x=data,y=nae,by="orgidentifier",all.x=T)
  data <- merge(x=data,y=npd,by="orgidentifier",all.x=T)
  data <- merge(x=data,y=query,by="orgidentifier",all.x=T)
  data <- merge(x=data,y=querytime,by="orgidentifier",all.x=T)
  data[is.na(data)] <- 0
  
  #Remove site with no enrolled patients yet or invalid site
  not.en <- which(data$nonstudy==0)
  if(length(not.en)>0) 
    data <- data[-not.en,]
  inv <- which(data$orgidentifier == "000" | data$orgidentifier=="999" | data$orgidentifier=="998")
  if(length(inv)>0)
    data <- data[-inv,]
  return(data)
}

CreateSurvData <- function(study,table,col,col2)
{
  tmp <- dbReadTable(con,c(study,table))
  if(!is.null(col))
  {
    ae <- tmp[,c("subjectid",col,col2)]
    sae <- ae[(ae[,2]=="Yes" & !is.na(ae[,3])),]
    tmp <- sae
  }
  index <- which(colnames(tmp) == col2)
  tmp2 <- tmp[order(tmp$subjectid,tmp[,index]),]
  tmp3 <- aggregate(tmp2[,index],by=list(tmp2$subjectid),FUN=min)
  colnames(tmp3) <- c("id","eventdt")
  
  t1 <- dbReadTable(con,c(study,"subjects_and_orgs"))
  index <- c(which(t1$statename=="Enrolled"),
             which(t1$statename=="Randomized"),
             which(t1$statename=="Completed"),
             which(t1$statename=="Enrolled FastTrack"),
             which(t1$statename=="Enrolled EVAR"),
             which(t1$statename=="Completed FastTrack"),
             which(t1$statename=="Completed EVAR"))
  t1 <- data.frame(t1[index,c("id","datecreated","orgidentifier")])
  
  subj.evt <- merge(x=t1,y=tmp3,by="id",all.x=T)
  event <- as.numeric(!is.na(subj.evt$eventdt))
  for(i in 1:length(event))
  {
    if(is.na(subj.evt$eventdt[i]))
    {
      subj.evt$eventdt[i] <- Sys.Date() 
    }
  }
  timediff <- round(as.numeric(as.Date(subj.evt[,4]) - as.Date(subj.evt$datecreated))/7,2)
  subj.evt <- data.frame(subj.evt,timediff,event)
  return(subj.evt)
}



#Get number of sites
GetNumSite <- function(study, table)
{
  if(dbExistsTable(con,c(study,table)))
  {
    tmp <- dbReadTable(con,c(study,table))
    site <- tmp[,c("orgidentifier")]
    site <- data.frame(unique(site))
    colnames(site) <- c("orgidentifier")
    return(site)
  }
}

#Get all contacts
GetAll <- function(study,table)
{
  if(dbExistsTable(con,c(study,table)))
  {
    tmp <- dbReadTable(con,c(study,table))
    all <- tmp[,c("orgidentifier","id")]
    n.all <- aggregate(all,by=list(all$orgidentifier),FUN=length)
    n.all <- n.all[,1:2]
    colnames(n.all) <- c("orgidentifier","nall")
    return(n.all)
  }
}

#Get number of participants on study
GetOnStudy <- function(study,table)
{
  if(dbExistsTable(con,c(study,table)))
  {
    tmp <- dbReadTable(con,c(study,table))
    index <- c(which(tmp$statename=="Enrolled"),
               which(tmp$statename=="Randomized"),
               which(tmp$statename=="Completed"),
               which(tmp$statename=="Enrolled FastTrack"),
               which(tmp$statename=="Enrolled EVAR"),
               which(tmp$statename=="Completed FastTrack"),
               which(tmp$statename=="Completed EVAR"))
    onstudy <- tmp[index,c("orgidentifier","id")]
    n.onstudy <- aggregate(onstudy,by=list(onstudy$orgidentifier),FUN=length)
    n.onstudy <- n.onstudy[,1:2]
    colnames(n.onstudy) <- c("orgidentifier","nonstudy")
    return(n.onstudy)
  }
}

#Get number of screening failures on study
GetScrFail <- function(study,table)
{
  if(dbExistsTable(con,c(study,table)))
  {
    tmp <- dbReadTable(con,c(study,table))
    index <- c(which(tmp$statename=="Screen Failure"))
    if(length(index)>0){
    scrfail <- tmp[index,c("orgidentifier","id")]
    n.scrfail <- aggregate(scrfail,by=list(scrfail$orgidentifier),FUN=length)
    n.scrfail <- n.scrfail[,1:2]
    colnames(n.scrfail) <- c("orgidentifier","nscrfail")
    return(n.scrfail)
    }
  }
}

GetQuery <- function(study,table)
{
  if(dbExistsTable(con,c(study,table)))
  {
    tmp <- dbReadTable(con,c(study,table))
    tmp <- tmp[,c("orgidentifier","querytype","subjectid")]
    man <- tmp[(tmp$querytype=="Manual Query" | tmp$querytype=="DM Query" | tmp$querytype=="CDM Query" | tmp$querytype=="CRA Query"),]
    if(dim(man)[1]>0)
    {
      n.man <- aggregate(man,by=list(man$subjectid),FUN=min)
      n.man <- n.man[,-1]
      n.man <- n.man[order(n.man$orgidentifier,n.man$subjectid),]
      site.man <- aggregate(n.man,by=list(n.man$orgidentifier),FUN=length)
      site.man <- site.man[,1:2]
    }
    else
    {
      site <- unique(tmp$orgidentifier)
      site.man <- data.frame(site,rep(0,length(site)))
    }
    colnames(site.man) <- c("orgidentifier","numquery")
    return(site.man)
  }
}

GetQueryTime <- function(study,table)
{
  if(dbExistsTable(con,c(study,table)))
  {
    tmp <- dbReadTable(con,c(study,table))
    tmp <- tmp[,c("orgidentifier","querytype","querystate","subjectid","createddate","closeddate")]
    man <- tmp[(tmp$querytype=="Manual Query" | tmp$querytype=="DM Query" | tmp$querytype=="CDM Query" | tmp$querytype=="CRA Query") & (tmp$querystate=="Closed"|tmp$querystate=="Resolved"),]
    
    if(dim(man)[1]>0)
    {
    week <- cbind(man,round(as.numeric(as.Date(man$closeddate) - as.Date(man$createddate))/7,2))
    colnames(week) <- c("orgidentifier","querytype","querystate","subjectid","createddate","closeddate","reswk")
    
    subj.q <- aggregate(week,by=list(week$subjectid),FUN=max)
    subj.q <- subj.q[,c(-1,-3,-4,-5,-6,-7)]
    subj.q <- subj.q[order(subj.q$orgidentifier),]
    site.time <- aggregate(subj.q$reswk,by=list(subj.q$orgidentifier),FUN=mean)
    site.time <- site.time[,1:2]
    }
    else
    {
      site <- unique(tmp$orgidentifier)
      site.man <- data.frame(site,rep(0,length(site)))
    }
    colnames(site.time) <- c("orgidentifier","querytime")
    return(site.time)
  }
}

#Get number of AE events
GetAE <- function(study,table,col,colname,subj.site)
{
  if(dbExistsTable(con,c(study,table)))
  {
    tmp <- dbReadTable(con,c(study,table))
    if(!is.null(col))
    {
      ae <- tmp[,c("subjectid",col)]
      sae <- ae[ae[,2]=="Yes",]
      tmp <- sae
    }
    subj <- tmp[,c("subjectid")]
    subj <- t(t(unique(subj)))
    colnames(subj) <- c("id")
    subj.site <- merge(x=subj,y=subj.site,by="id",all.x=T)
    
    tmp <- dbReadTable(con,c(study,"subjects_and_orgs"))
    index <- c(which(tmp$statename=="Enrolled"),
               which(tmp$statename=="Randomized"),
               which(tmp$statename=="Completed"),
               which(tmp$statename=="Enrolled FastTrack"),
               which(tmp$statename=="Enrolled EVAR"),
               which(tmp$statename=="Completed FastTrack"),
               which(tmp$statename=="Completed EVAR"))
    tmp <- data.frame(tmp[index,c("id")])
    colnames(tmp) <- c("id")
    subj.site <- merge(x=tmp,y=subj.site,by="id",all.x=T)
    
    n.event <- aggregate(subj.site,by=list(subj.site$orgidentifier),FUN=length)
    n.event <- n.event[,1:2]
    colnames(n.event) <- c("orgidentifier",colname)
    return(n.event)
  }
}

#Get number of events
GetEvents <- function(study,table,colname,subj.site)
{
  if(dbExistsTable(con,c(study,table)))
  {
    tmp <- dbReadTable(con,c(study,table))
    subj <- tmp[,c("subjectid")]
    subj <- t(t(unique(subj)))
    colnames(subj) <- c("id")
    subj.site <- merge(x=subj,y=subj.site,by="id",all.x=T)
    
    tmp <- dbReadTable(con,c(study,"subjects_and_orgs"))
    index <- c(which(tmp$statename=="Enrolled"),
               which(tmp$statename=="Randomized"),
               which(tmp$statename=="Completed"),
               which(tmp$statename=="Enrolled FastTrack"),
               which(tmp$statename=="Enrolled EVAR"),
               which(tmp$statename=="Completed FastTrack"),
               which(tmp$statename=="Completed EVAR"))
    tmp <- data.frame(tmp[index,c("id")])
    colnames(tmp) <- c("id")
    subj.site <- merge(x=tmp,y=subj.site,by="id",all.x=T)
    
    n.event <- aggregate(subj.site,by=list(subj.site$orgidentifier),FUN=length)
    n.event <- n.event[,1:2]
    colnames(n.event) <- c("orgidentifier",colname)
    return(n.event)
  }
}

#Get mapping between subject and site
GetSubjSite <- function(study,table)
{
  if(dbExistsTable(con, c(study,table)))
  {
    subj.site <- dbReadTable(con, c(study,table))
    subj.site <- subj.site[,c("orgidentifier","id")]
    return(subj.site)
  }
}


#Get start date
GetStartDate <- function(study,table)
{
  if(dbExistsTable(con,c(study,table)))
  {
    tmp <- dbReadTable(con,c(study,table))
    
    date <- tmp[,c("orgidentifier","datecreated")]
    f.date <- aggregate(date,by=list(date$orgidentifier),FUN=min)
    f.date <- f.date[,-c(1)]
    timeonstudy <- round(as.numeric(Sys.Date() - as.Date(f.date[,2]))/7,2)
    f.date <- data.frame(f.date,timeonstudy)
    f.date <- f.date[,-2]
    colnames(f.date) <- c("orgidentifier","timeonstudy")
    return(f.date)
  }
}

#Get timing of events
GetTiming <- function(study,table,col,colname,subj.site)
{
  if(dbExistsTable(con,c(study,table)))
  {
    tmp <- dbReadTable(con,c(study,table))
    
    if(!is.null(col))
    {
      ae <- tmp[,c("subjectid",col,colname)]
      sae <- ae[ae[,2]=="Yes",]
      tmp <- sae
    }
    subj <- tmp[,c("subjectid",colname)]
    colnames(subj) <- c("id",colname)
    #subj <- aggregate(subj,by=list(subj$id),FUN=min)
    output <- merge(x=subj,y=subj.site,by="id",all.x=T)
    return(output)
  }
}

GetTimingE <- function(study,table,col,colname,subj.site)
{
  if(dbExistsTable(con,c(study,table)))
  {
    tmp <- dbReadTable(con,c(study,table))
    
    index <- c(which(tmp$statename=="Enrolled"),
               which(tmp$statename=="Randomized"),
               which(tmp$statename=="Completed"),
               which(tmp$statename=="Enrolled FastTrack"),
               which(tmp$statename=="Enrolled EVAR"),
               which(tmp$statename=="Completed FastTrack"),
               which(tmp$statename=="Completed EVAR"))
    tmp <- tmp[index,]
    
    subj <- tmp[,c("id",colname)]
    colnames(subj) <- c("id",colname)
    #subj <- aggregate(subj,by=list(subj$id),FUN=min)
    output <- merge(x=subj,y=subj.site,by="id",all.x=T)
    return(output)
  }
}