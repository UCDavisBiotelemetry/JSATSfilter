# Version MP_TeknoFilter2.5.3.0_20180710.R
###################################################################################################################
#
#                         Tag Filter for Teknologic Receiver Files converted from CBR description
#                           Written by: Gabe Singer, Damien Caillaud     On: 05/16/2017
#                                   Last Updated: 2018-07-10 by Matt Pagel
#                                  Version 2.5.2 (2.6 functions inactive)
#
#                             Special Note from http://www.twinsun.com/tz/tz-link.htm:
#        Numeric time zone abbreviations typically count hours east of UTC, e.g., +09 for Japan and -10 for Hawaii.
#                      However, the POSIX TZ environment variable uses the opposite convention.
#              For example, one might use TZ="JST-9" and TZ="HST10" for Japan and Hawaii, respectively.
###################################################################################################################

# TODO 20180313: File type cleaner consolidation...should just need a header row specified, everything else should be the same.
# TODO 20180313: re-verify non-preprocessed RT data
# TODO 20180313: check timestamps for SUM JST XLS TXT files
# TODO 20180313: do mode on a per-detection cluster basis, eliminate mode/getmode as outmoded, document max drift between clusters (known as version 2.6)
# TODO 20180313: process unknown tags too. Figure out if their PRI is near-integer seconds, less than 1hr, 1m (tester/beacon)
# TODO 20180313: for RT files, ignore incoming file name...just read them all in to a big array pre-clean.
# See also TODOs in-line

# daily temperature flux <= 4 Kelvin out of 300ish = 1.333% variance in clock rate within a day

setwd("Z:/LimitedAccess/tek_realtime_sqs/data/preprocess/")
memory.limit(44000)
# TAGFILENAME = "taglist/t2018TagInventory.csv" # superseeded by vTAGFILENAME, which has element for default PRI
# 2017
# vTAGFILENAME = cbind(TagFilename=c("taglist/2017/FrianttaglistUCDtags(withBeacon).csv","taglist/2017/Brandes.csv"),PRI=c(5,5))
# 2018
 vTAGFILENAME = cbind(TagFilename=c("taglist/t2018TagInventory.csv","taglist/qMultiAgencyTagList.csv","taglist/PckTags.csv"),PRI=c(5,5,3))
DoCleanJST = FALSE
DoCleanRT = FALSE
DoCleanPrePre = FALSE
DoCleanShoreRT = FALSE
DoCleanSUM = FALSE
DoCleanATS = TRUE
DoCleanLotek = FALSE
DoSaveIntermediate = TRUE # (DoCleanJST || DoCleanSUM || DoCleanATS || DoCleanLotek)
DoFilterFromSavedCleanedData = TRUE || !DoSaveIntermediate # if you're not saving the intermediate, you should do direct processing

# Algorithm constants.
FILTERTHRESH = 3 # PNNL spec: 4. Arnold: 2 for ATS&Tekno, 4 for Lotek
FLOPFACTOR = 0.155 # PNNL "spec": 0.006. Arnold: .04*5 = 0.2
FLOPFACTOR_2.6 = 0.006 
MULTIPATHWINDOW = 0.2 # PNNL spec: 0.156. Arnold: 0.2
COUNTERMAX = 12 # PNNL spec: 12
NON_RT_Dir = "raw/"
RT_Dir = "Z:/LimitedAccess/tek_realtime_sqs/data/preprocess/"
RT_File_PATTERN = "jsats_2016901[1389]_TEK_JSATS_*|jsats_2017900[34]_JSATS_*|jsats_20169020_TEK_JSATS_17607[12]*"
SSRT_Dir = "P:/Win8Usr/mpagel/Downloads/UC.Davis"

###Install Load Function
install.load <- function(package.name)
{
  if (!require(package.name, character.only=T)) install.packages(package.name)
  library(package.name, character.only=T)
}

install.load('tidyverse')
install.load('lubridate')
install.load('data.table')

# if using read.csv rather than fread (data.table), you'll want to 
#   1. read a few entries from each column first
#   2. set the data type accordingly and 
#   3. re-set to character if it's picky
# dathead <- read.csv(i, header=T, nrows=10)
# classes<-sapply(dathead, class)
# classes[names(unlist(list(classes[which(classes %in% c("factor","numeric"))],classes[names(classes) %in% c("time","date","dtf")])))] <- "character"
# dat <- read.csv(i, header=T, colClasses=classes)
# names(dat) # SQSQueue,SQSMessageID,ReceiverID,DLOrder,DetectionDate,TagID,TxAmplitude,TxOffset,TxNBW
# names(dat) <- c("SQSQueue","SQSMessageID","RecSN","DLOrder","dtf","Hex","TxAmplitude","TxOffset","TxNBW")

# Set up custom code for going back and forth with data table and data frame read/write files
data.table.parse<-function (file = "", n = NULL, text = NULL, prompt = "?", keep.source = getOption("keep.source"), 
                            srcfile = NULL, encoding = "unknown") { # needed for dput data.tables (rather than data.frames)
  keep.source <- isTRUE(keep.source)
  if (!is.null(text)) {
    if (length(text) == 0L) return(expression())
    if (missing(srcfile)) {
      srcfile <- "<text>"
      if (keep.source)srcfile <- srcfilecopy(srcfile, text)
    }
    file <- stdin()
  }
  else {
    if (is.character(file)) {
      if (file == "") {
        file <- stdin()
        if (missing(srcfile)) srcfile <- "<stdin>"
      }
      else {
        filename <- file
        file <- file(filename, "r")
        if (missing(srcfile)) srcfile <- filename
        if (keep.source) {
          text <- readLines(file, warn = FALSE)
          if (!length(text)) text <- ""
          close(file)
          file <- stdin()
          srcfile <- srcfilecopy(filename, text, file.mtime(filename), isFile = TRUE)
        }
        else {
          text <- readLines(file, warn = FALSE)
          if (!length(text)) text <- ""
          else text <- gsub("(, .internal.selfref = <pointer: 0x[0-9A-Fa-f]+>)","",text,perl=TRUE)
          on.exit(close(file))
        }
      }
    }
  }
  .Internal(parse(file, n, text, prompt, srcfile, encoding))
}
data.table.get <- function(file, keep.source = FALSE)
  eval(data.table.parse(file = file, keep.source = keep.source))
dtget <- data.table.get

# set up functions for calculating size of folders for progress bars
list.files.size <- function(path = getwd(), full.names=TRUE, nodotdot = TRUE, ignore.case = TRUE, include.dirs = FALSE, ...) { 
  filelist <- data.table(filename=list.files(path=path, full.names=full.names, no.. = nodotdot, ignore.case = ignore.case, include.dirs = include.dirs, ...))
  filelist[,size:=file.size(filename)][,tot:=sum.file.sizes(filelist)][,perc:=size/tot]
  return(filelist)
}
sum.file.sizes <- function(DT) {
  return(unlist(DT[,.(x=sum(size))],use.names=F)[1])
}

find_ePRI <- function(obj) {
#  browser()
  N<-nrows(obj)
  tmp<-data.table(merge.data.frame(x=1:COUNTERMAX,obj))
#  tmp<-tmp[twind>0]
  itr<-tmp[,ic:=round(twind/x,2)]
  ll <- itr[(ic >= nPRI*0.651 & ic<=nPRI*1.3)]
  setkey(ll,Hex,ic)
  rv<-ll[,dist:=abs(nPRI-ic)][,.(tot=.N),by=.(ic,dist)][order(-tot,dist,-ic)][1]
  retval<-data.table(rep(rv$ic,N))
  return(retval)
}

magicFilter2.6 <- function(dat, countermax, filterthresh){
  setkey(dat,Hex,dtf)
  dat[,temporary:=as.POSIXct(dtf, format = "%m/%d/%Y %H:%M:%OS", tz="Etc/GMT+8")] # dput file stores datestamp in this basic format
  if (is.na(dat[,.(temporary)][1])) dat[,dtf:=as.POSIXct(dtf, format = "%Y-%m-%dT%H:%M:%OSZ", tz="UTC")] # fwri file stores as UTC in this format ...was in this doc as %S.%OSZ
  else dat[,dtf:=temporary]
  dat[,temporary:=NULL]
  dat[,winmax:=dtf+((nPRI*1.3*countermax)+1)]
  wind_range <- dat[,.(Hex,dtf,winmax,nPRI)]
  setkey(wind_range,Hex,dtf,winmax)
  fit <- dat[,.(Hex,dup=dtf,hit=dtf)]
  setkey(fit,Hex,dup,hit)
  browser()
  fo_windows <- foverlaps(fit,wind_range,maxgap=0,type="within",nomatch=0,mult="all")[,twind:=(hit-dtf)*1000][,dup:=NULL][,winmax:=NULL][,c('hitsInWindow','WinID'):=list(.N,.GRP),by=c("Hex","dtf")][hitsInWindow>=filterthresh]
  if (fo_windows[,.N]>0) {
    setkey(fo_windows,WinID)
    fo_windows[,ePRI:=find_ePRI(.SD),by=WinID,.SDcols=c("twind","nPRI")]
    # itr <- as.data.table(merge(x=1:countermax,fo_windows))
    # itr[,icalc:=round(twind/x,2)]
    flopintervals <-as.data.table(0:countermax)
    setnames(flopintervals,c("x"))
    flopintervals[,flop:=(x+1)*FLOPFACTOR_2.6][,flopmin:=x*ePRI-flop][,flopmax:=x*ePRI+flop][,flop:=NULL]
    maxflop <- flopintervals[x==12,flopmax]
    windowz <- unique(abbrev[,.(dtf,ewinmax=dtf+maxflop)])
    dett <-res[,.(dup=dtf,dd=dtf)]
    setkey(dett,dup,dd)
    setkey(windowz,dtf,ewinmax)
    fomega <- foverlaps(dett,windowz,maxgap=0,type="within",nomatch=0)[,dd:=NULL][,dif:=(dup-dtf)*1000][,dif2:=(dup-dtf)*1000]
    flopintervals[,newmin:=flopmin*1000][,newmax:=flopmax*1000]
    setkey(flopintervals,newmin,newmax)
    if (fomega[,.N]>0) {
      setkey(fomega,dif,dif2)
      windHits<-foverlaps(fomega,flopintervals,maxgap=0,type="within",nomatch=0)[,.(firstHit=dtf,windowEnd=ewinmax,hit=dup,intervals=x,ePRI)]
      NAs<-windHits[is.na(intervals)]
      noNAs<-windHits[!is.na(intervals)][,c:=.N,keyby="firstHit"] # do I need to check for no lines before c code?
      noOnlyFirst<-noNAs[c>1]
      onlyFirst<-noNAs[c==1,]
      noOnlyFirst[,isAccepted:=TRUE]
      NAs[,isAccepted:=FALSE][,c:=NA]
      onlyFirst[,isAccepted:=FALSE][,c:=0]
      LT<-rbind(noOnlyFirst,NAs,onlyFirst)
      setkey(LT,firstHit,hit)
      logTable<-LT[,.(hit=hit, initialHit=firstHit, isAccepted, nbAcceptedHitsForThisInitialHit=c, ePRI)]
    } else {
      logTable<-data.table(hit=NA, initialHit=NA, isAccepted=FALSE, nbAcceptedHitsForThisInitialHit=0, ePRI=NA)
    }
  } else {
    logTable<-data.table(hit=NA, initialHit=NA, isAccepted=FALSE, nbAcceptedHitsForThisInitialHit=0, ePRI=NA)
  }
  return(logTable)
}


# filter windowed hits crudely
magicFunc <- function(dat, tagHex, countermax, filterthresh){
  setkey(dat,Hex)
  tagdet <- dat[Hex==tagHex]
  setkey(tagdet,dtf)
  tagdet[,temporary:=as.POSIXct(dtf, format = "%m/%d/%Y %H:%M:%OS", tz="Etc/GMT+8")] # dput file stores datestamp in this basic format
  if (is.na(tagdet[,.(temporary)][1])) tagdet[,dtf:=as.POSIXct(dtf, format = "%Y-%m-%dT%H:%M:%OSZ", tz="UTC")] # fwri file stores as UTC in this format ...was in this doc as %S.%OSZ
  else tagdet[,dtf:=temporary]
  tagdet[,temporary:=NULL]
  tagdet[,winmax:=dtf+((nPRI*1.3*countermax)+1)]
  setkey(tagdet,dtf)
  shiftz = 1:(filterthresh-1)
  shiftzcols = paste("l",shiftz,sep="")
  tagdet[,(shiftzcols):=shift(dtf,n=shiftz,fill=NA,type="lead")]
  aclist<-as.data.table(as.POSIXct(unique(unlist(tagdet[difftime(get(shiftzcols[max(shiftz)]),winmax)<=0,c("dtf",(shiftzcols[1:(filterthresh-1)])),with=F],use.names=FALSE)),origin="1970-01-01",tz="Etc/GMT+8"))
  if (aclist[,.N]>0) {
    setkey(aclist,x)
    res <- tagdet[aclist]
    res[,twind:=l1-dtf]
    itr <- as.data.table(merge(x=1:countermax,res))
    itr[,icalc:=round(twind/x,2)]
    ll <- itr[(icalc>=nPRI*0.651 & icalc<=nPRI*1.3)]
    setkey(ll,dtf)
    abbrev = ll[,.(dtf,twind,icalc,x,nPRI)]
    setkey(abbrev,icalc)
    cmod<-abbrev[,dist:=abs(nPRI-icalc)][,.(tot=.N),by=.(icalc,dist)][order(-tot,dist,-icalc)][1]
    ePRI<-cmod$icalc
    if (is.na(ePRI)) ePRI<-5
    flopintervals <-as.data.table(0:countermax)
    flopintervals[,x:=V1][,V1:=NULL][,flop:=(x+1)*FLOPFACTOR][,flopmin:=x*ePRI-flop][,flopmax:=x*ePRI+flop][,flop:=NULL]
    maxflop <- flopintervals[x==12,flopmax]
    windowz <- unique(abbrev[,.(dtf,ewinmax=dtf+maxflop)])
    dett <-res[,.(dup=dtf,dd=dtf)]
    setkey(dett,dup,dd)
    setkey(windowz,dtf,ewinmax)
    fomega <- foverlaps(dett,windowz,maxgap=0,type="within",nomatch=0)[,dd:=NULL][,dif:=(dup-dtf)*1000][,dif2:=(dup-dtf)*1000]
    flopintervals[,newmin:=flopmin*1000][,newmax:=flopmax*1000]
    setkey(flopintervals,newmin,newmax)
    if (fomega[,.N]>0) {
      setkey(fomega,dif,dif2)
      windHits<-foverlaps(fomega,flopintervals,maxgap=0,type="within",nomatch=0)[,.(firstHit=dtf,windowEnd=ewinmax,hit=dup,intervals=x,ePRI)]
      NAs<-windHits[is.na(intervals)]
      noNAs<-windHits[!is.na(intervals)][,c:=.N,keyby="firstHit"] # do I need to check for no lines before c code?
      noOnlyFirst<-noNAs[c>1]
      onlyFirst<-noNAs[c==1,]
      noOnlyFirst[,isAccepted:=TRUE]
      NAs[,isAccepted:=FALSE][,c:=NA]
      onlyFirst[,isAccepted:=FALSE][,c:=0]
      LT<-rbind(noOnlyFirst,NAs,onlyFirst)
      setkey(LT,firstHit,hit)
      logTable<-LT[,.(hit=hit, initialHit=firstHit, isAccepted, nbAcceptedHitsForThisInitialHit=c, ePRI)]
    } else {
      logTable<-data.table(hit=NA, initialHit=NA, isAccepted=FALSE, nbAcceptedHitsForThisInitialHit=0, ePRI=NA)
    }
  } else {
    logTable<-data.table(hit=NA, initialHit=NA, isAccepted=FALSE, nbAcceptedHitsForThisInitialHit=0, ePRI=NA)
  }
  return(logTable)
}

dataFilter2.6 <- function(dat, filterthresh, countermax){
  res <- dat[1==0] # copies structure
  # timer <- 0
  setkey(dat,Hex)
  titl<-dat[!is.na(RecSN)][1][,RecSN]
  ans <- magicFilter2.6(dat, countermax=countermax, filterthresh)
  setkey(ans,nbAcceptedHitsForThisInitialHit,isAccepted)
  ans2 <- ans[(nbAcceptedHitsForThisInitialHit >= filterthresh)&(isAccepted)]
  if (ans2[,.N]>0) {
    keep<-as.data.table(unique(ans2[,hit]))
    setkey(dat,dtf)
    setkey(keep,x)
    res <- rbind(res, dat[keep])
  }
  # timer <- timer+1
  return(res)
}


# filter windowed hits to verify which hits are in which windows
dataFilter <- function(dat, filterthresh, countermax){
  res <- dat[1==0] # copies structure
  timer <- 0
  setkey(dat,Hex)
  titl<-dat[!is.na(RecSN)][1][,RecSN]
  u<-as.list(unique(dat[,.N,by = Hex][N>=filterthresh])[,1])$Hex
  if (length(u)>0) {
    timerbar<-winProgressBar(title=titl, label="Tag", min=0, max=length(u), initial=0)
    for(i in u){
      setWinProgressBar(timerbar,timer,label=i)
      ans <- magicFunc(dat, tagHex=i, countermax=countermax, filterthresh)
      setkey(ans,nbAcceptedHitsForThisInitialHit,isAccepted)
      ans2 <- ans[(nbAcceptedHitsForThisInitialHit >= filterthresh)&(isAccepted)]
      if (ans2[,.N]>0) {
        keep<-as.data.table(unique(ans2[,hit]))
        setkey(dat,dtf)
        setkey(keep,x)
        res <- rbind(res, dat[keep,nomatch=0])
      }
      timer <- timer+1
    }
    close(timerbar)
  }
  return(res)
}

##filter Loop
filterData <- function(incomingData=NULL) {
  j <- 0
  loopFiles <- function() {
    for(i in list.files("./cleaned",full.names=TRUE)){
      if (as.integer(file.info(i)["isdir"])) next
      # if it's a dput file, read it back in, but make sure there's no funky memory addresses that were saved by data.table
      if (endsWith(i,'.dput') || endsWith(i,'.txt')) datos <-as.data.table(dtget(i))
      if (endsWith(i,'.fwri')) { # if it was written to disk with fwrite, use the faster fread, but make sure to set the datetime stamps
        datos <-as.data.table(fread(i))
        datos[,dtf:=as.POSIXct(dtf, format = "%Y-%m-%dT%H:%M:%OSZ", tz="UTC")] # %OS6Z doesn't seem to work correctly
      }
      proces(dat=datos)
    }
  }
  proces <- function(dat) {
    myResults <- dataFilter(dat=dat, filterthresh=FILTERTHRESH, countermax=COUNTERMAX) # dataFilter2.6(dat=dat, filterthresh=FILTERTHRESH, countermax=COUNTERMAX)
    setkey(dat,dtf) # TODO 20180313: we should probably put TagID_Hex and RecSN in the key also
    setkey(myResults,dtf)
    rejecteds <- dat[!myResults]
    myResults[,tt:=strftime(dtf,tz="Etc/GMT+8",format="%Y-%m-%d %H:%M:%S")][,FracSec:=round(as.double(difftime(dtf,tt,tz="Etc/GMT+8",units="secs")),6)][,dtf:=as.character(tt)][,tt:=NULL]
    rejecteds[,tt:=strftime(dtf,tz="Etc/GMT+8",format="%Y-%m-%d %H:%M:%S")][,FracSec:=round(as.double(difftime(dtf,tt,tz="Etc/GMT+8",units="secs")),6)][,dtf:=as.character(tt)][,tt:=NULL]
    j <<- j+1
    if (myResults[,.N]>0) {
      recsn <- myResults[!is.na(RecSN)][1][,RecSN]
    } else {
      recsn <- rejecteds[!is.na(RecSN)][1][,RecSN]
    }
    write.csv(rejecteds, paste0("./rejected/", j, "_", recsn, "_rejected.csv"), row.names=F)
    write.csv(myResults, paste0("./accepted/", j, "_", recsn, "_accepted.csv"), row.names=F)
  }
  if (is.null(incomingData)) loopFiles()
  else proces(dat=incomingData)
}

# TODO 20180313: directly in data.table
readTags <- function(vTagFileNames=vTAGFILENAME, priColName=c('PRI_nominal','Period_Nom','nPRI','PRI_estimate','ePRI','Period','PRI'), 
                     TagColName=c('TagID_Hex','TagIDHex','TagID','TagCode_Hex','TagCode','CodeID','CodeHex','CodeID_Hex','CodeIDHex','Tag','Code','TagSN','HexCode','Tag ID (hex)'),
                     grpColName=c("Rel_Group","RelGroup","Rel_group","Release","Group","Origin","StudyID","Owner")) {
  ret <- data.frame(TagID_Hex=character(),nPRI=numeric(),rel_group=character())
  for (i in 1:nrow(vTagFileNames)) {
    fn = vTagFileNames[i,"TagFilename"]
    if (!file.exists(fn)) { next }
    pv = vTagFileNames[i, "PRI"]
    tags<- fread(fn, header=T, stringsAsFactors=FALSE) #list of known Tag IDs #colClasses="character", 
    heads = names(tags)
    tcn = TagColName[which(TagColName %in% heads)[1]] # prioritize the first in priority list
    pcn = priColName[which(priColName %in% heads)[1]] # prioritize the first in priority list
    gcn = grpColName[which(grpColName %in% heads)[1]]
    if (is.null(pcn) || is.na(pcn) || length(pcn)<1 || pcn=="NA") pcn = NULL
    if (is.null(gcn) || is.na(gcn) || length(gcn)<1 || gcn=="NA") gcn = NULL
    tags <- tags[,c(tcn,pcn,gcn),with=F]
    if (is.null(pcn)) {
      tags[,nPRI:=as.numeric(pv)]
      pcn = "nPRI"
    }
    if (is.null(gcn)) {
      fn<-as.character(basename(fn))
      tags[,rel_group:=fn]
      gcn = "rel_group"
    }
    setnames(tags,c(tcn,pcn,gcn),c("TagID_Hex","nPRI","rel_group"))
    setkey(tags,TagID_Hex)
    
    tags[,nPRI:=as.numeric(nPRI)]
    tags[is.na(tags)] <- as.numeric(pv)
#    tags[,f_name:=fn]
    ret <- rbindlist(list(ret, tags),use.names=TRUE)
  }
  setDT(ret,key="TagID_Hex")
  ret[,TagID_Hex:=as.character(TagID_Hex)] # drop factors
  return(ret)
}

trimcomma <- function(x) { if (endsWith(trimws(x),",")) {return(substr(x,1,nchar(trimws(x))-1))} else {return(trimws(x))} }

isDataLine <- function(x) { # non-header
  a<-as.character(x)
  if (is.null(a) || length(a)==0) return(FALSE) # NULL check
  if (nchar(a)==0) return(FALSE) # empty string check
  if (str_count(a,",")<5) return(FALSE) # are there too few fields to actually be data?
  return (!grepl("SiteName",a,fixed = T)) # is it a header line
}

elimNPCandDS <- function(x) {# NPC = nonprinting characters; DS = double-space
  return(gsub("  ", "",gsub("[^[:alnum:] :.,|?&/\\\n-]", "",x)))
}

cleanLinesATS <- function(x) {
  #    a<-elimNPCandDS(x)
  #    if (nchar(a)>0) return(trimcomma(a)) else return(a) # commented out to consistently return 13 columns
  return(elimNPCandDS(x))
}

lotekDateconvert <- function(x) {
  return (as.POSIXct(round(x*60*60*24), # timestamps given in days since midnight first day of 1900
                     origin = "1899-12-30", tz = "GMT") # correction for erroneous regard of 1900 as a leapyear
  )
}

cleanWrapper <- function(functionCall, tags, precleanDir, filePattern, wpbTitle=NULL) { # for customized code (ATS)
  lfs<-list.files.size(precleanDir, pattern=filePattern, full.names=TRUE, include.dirs=FALSE)
  # lf<-list.files(precleanDir, pattern=filePattern, full.names=TRUE, include.dirs = FALSE)
  tf<-length(lfs[,filename]) # total files
  if (tf==0) return(F)
  tfs<-lfs[,tot][1] # total file sizes
  if (tfs==0) return(F)
  pb<-winProgressBar(title=wpbTitle, label="Setting up file cleaning...", min=0, max=tfs, initial=0)
  rt<-0 # running size total
  for(i in 1:tf){
    ts <- lfs[i,size]
    if (ts==0) next
    fn <- lfs[i,filename]
    rt <- rt+ts
    if (as.integer(file.info(fn)["isdir"])) next
    lab<-paste0(basename(fn),"\n",i,"/",tf," (",((10000*rt)%/%tfs)/100,"% by size)\n")
    setWinProgressBar(pb,rt,label=lab)
    functionCall(fn, tags)
  }
  close(pb)
  return(T)
}

cleanOuterWrapper <- function(functionCall, tags, precleanDir, filePattern, wpbTitle,
                              headerInFile, leadingBlanks, tz, dtFormat, nacols, foutPrefix,
                              inferredHeader, Rec_dtf_Hex_strings, mergeFrac) {
  lfs<-list.files.size(precleanDir, pattern=filePattern, full.names=TRUE, include.dirs=FALSE)
  # lf<-list.files(precleanDir, pattern=filePattern, full.names=TRUE, include.dirs = FALSE)
  tf<-length(lfs[,filename]) # total files
  if (tf==0) return(F)
  tfs<-lfs[,tot][1] # total file sizes
  if (tfs==0) return(F)
  pb<-winProgressBar(title=wpbTitle, label="Setting up file cleaning...", min=0, max=tfs, initial=0)
  rt<-0 # running size total
  for(i in 1:tf){
    ts <- lfs[i,size]
    if (ts==0) next
    fn <- lfs[i,filename]
    rt <- rt+ts
    if (as.integer(file.info(fn)["isdir"])) next
    lab<-paste0(basename(fn),"\n",i,"/",tf," (",((10000*rt)%/%tfs)/100,"% by size)\n")
    setWinProgressBar(pb,rt,label=lab)
    functionCall(i=fn, tags, headerInFile, leadingBlanks, tz, dtFormat, nacols, foutPrefix,
                 inferredHeader, Rec_dtf_Hex_strings, mergeFrac)
  }
  close(pb)
  return(T)
}

cleanInnerWrap <-function(...) {
  itercount <- 0
  function(i, tags, headerInFile=T, leadingBlanks=0, tz="GMT", dtFormat="%Y-%m-%dT%H:%M:%OS", nacols=NULL, foutPrefix=".txt", inferredHeader=NULL, Rec_dtf_Hex_strings=c("RecSN","DateTime","TagID_Hex"), mergeFrac=NULL) {
    if (!headerInFile) {
      dathead <- fread(i, header=F, nrows=10, stringsAsFactors=F, skip=leadingBlanks, fill=T, na.strings=c("NA","NULL","Null","null","nan","-nan","N/A",""))
#      browser()
      classes<-sapply(dathead, class)
      classes[names(unlist(list(classes[which(classes %in% c("factor","numeric"))],classes[names(classes) %in% c("time","date","dtf")])))] <- "character"
      dat <- fread(i, header=F, skip=leadingBlanks, fill=T, na.strings=c("NA","NULL","Null","null","nan","-nan","N/A","","-"), colClasses=classes)
      setnames(dat,inferredHeader)
    } else {
      dat <- fread(i, header=T, skip=leadingBlanks, fill=T, na.strings=c("NA","NULL","Null","null","nan","-nan","N/A","","-")) # fread (unlike read.csv) should read dates as character automatically
    }
    if (length(nacols)>0) {
      dat <- na.omit(dat,cols=nacols)
    }
    if (nrow(dat)==0) return(F)
    if (is.na(Rec_dtf_Hex_strings[1]))  {
      SN <- as.numeric(gsub("[a-zA-Z\\/]{0,255}([0-9]+).*$", "\\1", i)) # get it from the first number in the filename (please make sure not in raw file directory name)
      dat[,RecSN:=SN]
      Rec_dtf_Hex_strings[1]<-"RecSN"
    }
    setnames(dat,Rec_dtf_Hex_strings,c("RecSN","dtf","Hex")) 
    if (dtFormat=="EPOCH") {
      dat[,dtf:=as.character(lotekDateconvert(as.numeric(dtf)),format="%Y-%m-%d %H:%M:%S")] # dat[,epdtf:=dtf][,dtf:=as.character(...
#      browser()
      dtFormat="%Y-%m-%d %H:%M:%OS"
    }
    # combine the DT and FracSec columns into a single time column
    if (length(mergeFrac)>0) {
      dat[,iznumb:=ifelse(is.na(
             tryCatch(suppressWarnings(as.numeric(eval(as.name(mergeFrac)))))
                                ),FALSE,TRUE)
          ][iznumb==T         ,gt1:=(as.numeric(eval(as.name(mergeFrac)))>=1)
          ][gt1==T,fracLead0:=sprintf("%7.6f",as.numeric(eval(as.name(mergeFrac)))/1000000)
          ][iznumb==T & gt1==F,fracLead0:=sprintf("%7.6f",as.numeric(eval(as.name(mergeFrac))))
          ][iznumb==T         ,fracDot:=substring(fracLead0,2)
          ][!is.na(fracDot)   ,newDateTime:=paste0(as.character(dtf),fracDot)
          ][ is.na(fracDot)   ,newDateTime:=as.character(dtf)
        ]
      dat[,c("iznumb","gt1","fracLead0","dtf"):=NULL]
      setnames(dat, old="newDateTime", new="dtf")
      dat[,c(mergeFrac,"fracDot"):=NULL]
    }
    dat[nchar(Hex)==9,Hex:=substr(Hex,4,7)]
    setkey(dat, Hex)
    if (nrow(dat)==0) return(F)
    # convert time string into POSIXct timestamp
    dat[,dtf:=as.POSIXct(dtf, format = dtFormat, tz=tz)]
    setkey(tags,TagID_Hex)
    dat2<-dat[tags,nomatch=0] # bring in the nominal PRI (nPRI)
    if (nrow(dat2)==0) {
      print(unique(tags))
      print(unique(dat))
      return(F)
    }
    setkey(dat2, RecSN, Hex, dtf)
    dat2[,tlag:=shift(.SD,n=1L,fill=NA,type="lag"), by=.(Hex,RecSN),.SDcols="dtf"]
    # dat3<-na.omit(dat2,cols=c("tlag")) # TODO 20180313 need to verify this doesn't drop the first for a tag.  It must, as it is missing in cleaned (20180709)....
    if (nrow(dat2)==0) return(F) #was dat3...should this be <filterthresh?
    dat2[,c("SQSQueue","SQSMessageID","DLOrder","TxAmplitude","TxOffset","TxNBW"):=NULL] # will give warnings, not error if columns missing, was dat3
    setkey(dat2,RecSN,Hex,dtf) #was dat3
    # calculate tdiff, then remove multipath
    dat4 <- dat2[,tdiff:=difftime(dtf,tlag)][tdiff>MULTIPATHWINDOW | tdiff==0 | is.na(tdiff)]
    if (nrow(dat4)==0) return(F)
    # dat4[dtf==tlag,tlag:=NA] # if we want to set the first lag dif to NA rather than 0 for the first detection of a tag
    # rm(dat3)
    setkey(dat4,RecSN,Hex,dtf)
    setkey(dat ,RecSN,Hex,dtf)
    keepcols = unlist(list(colnames(dat),"nPRI")) # use initial datafile columns plus the nPRI column from the taglist file
    dat5 <- unique(dat[dat4,keepcols,with=FALSE]) # too slow?  try dat[dat4] then dat5[,(colnames(dat5)-keepcols):=NULL,with=FALSE]
    setkey(dat5,RecSN,Hex,dtf)
    rm(dat4)
    rm(dat2)
    # I think "crazy" in Damien's original was just a check to see if matches previous tag, if not discard result of subtraction
    # Shouldn't be needed for data.table.
    SNs<-unique(dat5[,RecSN])
    # browser()
    for(sn in SNs) { # don't trust the initial file to have only a single receiver in it
      itercount <<- itercount + 1
      # fwri format stores timestamps as UTC-based (yyyy-mm-ddTHH:MM:SS.microsZ)
      if (DoSaveIntermediate) fwrite(dat5[RecSN==sn], file = paste0("./cleaned/", foutPrefix, itercount, "_", sn,  "_cleaned.fwri"))
    }
    if (!DoFilterFromSavedCleanedData) filterData(dat5)
    rm(dat5)
  }
}

cleanATSxls <- function() { # just converts an excel file to a "csv" (with a bunch of header lines)
  function(i, tags) {
    install.load('readxl')
    dat <- read_excel(i)                    #read in each file
    newfn <- paste0(i,".xtmp")
    fwrite(dat,file=newfn)  # check for timezone shifts!
    cleanATScsv()(newfn, tags)
#    file.remove(newfn)
  }
}

cleanATScsv <- function() { # have to figure out how to dovetail this with the other, more sane, files.
  itercount <- 0 # TODO: Read in and immediately write out to CSV, then set params?
  function(i, tags) { # run the inner function of the enclosure
    # find serial number somewhere in the top 10 lines
    install.load('readr')
    install.load('stringr')
    SN<-NA
    headr <- read_lines(i,n_max=10)
    for (rw in 1:length(headr)) {
      if (startsWith(headr[rw],"Serial Number: ")) {
        SN<-as.numeric(gsub("Serial Number: ", "", headr[rw]))
        break
      }
    }
    if (is.na(SN) || SN==9000) # 9000 is a placeholder in some files.
    { SN<- as.numeric(gsub("[a-zA-Z_\\/]{0,20}([0-9]+).*$", "\\1", i))} # get it from the first number in the filename
    rl<-read_lines(i) # readr
    gs<-lapply(rl,cleanLinesATS)
    p<-paste(Filter(isDataLine,gs),sep="\n",collapse="\n") # Filter(isDataLine,gs) or possibly gs[lapply(gs,isDataLine)]
    dat<-fread(p,blank.lines.skip=TRUE,strip.white=TRUE,na.strings=c("NA","NULL","Null","null","nan","-nan","N/A","","-")) # ,skip="SiteName",
    headers <- c("Internal", "SiteName", "SiteName2", "SiteName3", "dtf", "Hex", "Tilt", "VBatt", "Temp", "Pres", "SigStr",
                 "BitPeriod", "Thresh","Detection")                        #make vector of new headers
    setnames(dat,headers)
    dat[,RecSN:=SN]   # add SN column 
    newfn <- paste0(i,".ctmp")
    fwrite(dat,newfn)
    headerInFile = T
    leadingBlanks = 0
    tz = "Etc/GMT+8"
    dtFormat = "%m/%d/%Y %H:%M:%OS"
    nacols = NULL # if this column is null, the row gets cut from the data set. c("Detection","Pres")
    foutPrefix = "ATS"
    inferredHeader = NULL
    Rec_dtf_Hex_strings = c("RecSN","dtf","Hex")
    mergeFrac = NULL
    functionCall<-cleanInnerWrap()
    functionCall(i=newfn, tags=tags, headerInFile=headerInFile, leadingBlanks=leadingBlanks, tz=tz, dtFormat=dtFormat, foutPrefix=foutPrefix, inferredHeader=inferredHeader, Rec_dtf_Hex_strings=Rec_dtf_Hex_strings, mergeFrac=mergeFrac)
    file.remove(newfn)
  }
}

###load taglist
# tags<- read.csv(TAGFILENAME, header=T, colClasses="character") # single tag list file. Superseeded.
tags<-readTags(vTAGFILENAME)

# Handle all the well-structured files
# Realtime file formats
if (DoCleanPrePre) {
  headerInFile = F
  leadingBlanks = 0
  tz = "GMT"
  dtFormat = "%Y-%m-%d %H:%M:%OS"
  nacols = NULL
  foutPrefix = "RT_npp"
  inferredHeader = c("SQSQueue","SQSMessageID","RecSN","DLOrder","DateTime","microsecs","Hex","TxAmplitude","TxOffset","TxNBW","TxCRC")
  Rec_dtf_Hex_strings = c("RecSN", "DateTime", "Hex")
  mergeFrac = "microsecs"
  functionCall<-cleanInnerWrap()
  cleanOuterWrapper(functionCall, tags=tags, precleanDir = RT_Dir, filePattern = "*.CSV$", wpbTitle = "Cleaning RT files before preprocessing",
                    headerInFile=headerInFile, leadingBlanks=leadingBlanks, tz=tz, dtFormat=dtFormat, 
                    nacols=nacols, foutPrefix=foutPrefix, inferredHeader=inferredHeader, 
                    Rec_dtf_Hex_strings=Rec_dtf_Hex_strings, mergeFrac=mergeFrac)
}

if (DoCleanRT) { # preprocessed (with DBCnx.py) DataCom detection files
  headerInFile = T
  leadingBlanks = 0
  tz = "GMT"
  dtFormat = "%Y-%m-%d %H:%M:%OS"
  nacols = NULL
  foutPrefix = "RT"
  inferredHeader = NULL
  Rec_dtf_Hex_strings = c("ReceiverID","DetectionDate","TagID")
  mergeFrac = NULL
  fn<-cleanInnerWrap()
  cleanOuterWrapper(fn, tags=tags, precleanDir = RT_Dir, filePattern = RT_File_PATTERN, wpbTitle = "Cleaning Preprocessed Realtime Data",
                    headerInFile=headerInFile, leadingBlanks=leadingBlanks, tz=tz, dtFormat=dtFormat, 
                    nacols=nacols, foutPrefix=foutPrefix, inferredHeader=inferredHeader, 
                    Rec_dtf_Hex_strings=Rec_dtf_Hex_strings, mergeFrac=mergeFrac)
}

if (DoCleanShoreRT){ # ShoreStation files created with
  # cat `ls -dR */*.txt` | awk -F , 'NF == 11' > DetectionsThru2018mmdd_hhnn.csv
  headerInFile = F
  leadingBlanks = 0
  tz = "GMT"
  dtFormat = "%Y-%m-%d %H:%M:%OS"
  nacols = NULL
  foutPrefix = "SSRT"
  inferredHeader = c("RecSN","DetOrder","DetectionDate","microsecs","TagID","Amp","FreqShift","NBW","Pressure","WaterTemp","CRC")
  Rec_dtf_Hex_strings = c("RecSN","DetectionDate","TagID")
  mergeFrac = "microsecs"
  fn<-cleanInnerWrap()
  cleanOuterWrapper(fn, tags=tags, precleanDir = SSRT_Dir, filePattern = "*.csv$", wpbTitle = "Cleaning Shore Station Data",
                    headerInFile=headerInFile, leadingBlanks=leadingBlanks, tz=tz, dtFormat=dtFormat, 
                    nacols=nacols, foutPrefix=foutPrefix, inferredHeader=inferredHeader, 
                    Rec_dtf_Hex_strings=Rec_dtf_Hex_strings, mergeFrac=mergeFrac)
}

# Tekno autonomous file formats
if (DoCleanJST) {
  headerInFile = F
  leadingBlanks = 0
  tz = "Etc/GMT+8"
  dtFormat = "%m/%d/%Y %H:%M:%OS"
  nacols = NULL
  foutPrefix = "JT"
  inferredHeader = c("Filename", "RecSN", "DT", "FracSec", "Hex", "CRC", "validFlag", "TagAmp", "NBW")
  Rec_dtf_Hex_strings = c("RecSN", "DT", "Hex")
  mergeFrac = "FracSec"
  fn<-cleanInnerWrap()
  cleanOuterWrapper(fn, tags=tags, precleanDir = NON_RT_Dir, filePattern = "*.JST$", wpbTitle = "Cleaning Tekno JST files",
                    headerInFile=headerInFile, leadingBlanks=leadingBlanks, tz=tz, dtFormat=dtFormat, 
                    nacols=nacols, foutPrefix=foutPrefix, inferredHeader=inferredHeader, 
                    Rec_dtf_Hex_strings=Rec_dtf_Hex_strings, mergeFrac=mergeFrac)
}

if (DoCleanSUM) {
  headerInFile = T
  leadingBlanks = 8
  tz = "Etc/GMT+8"
  dtFormat = "%m/%d/%Y %H:%M:%OS"
  nacols = c("Detection")
  foutPrefix = "SUM"
  inferredHeader = NULL
  Rec_dtf_Hex_strings = c("Serial Number","Date Time","TagCode")
  mergeFrac = NULL
  fn<-cleanInnerWrap()
  cleanOuterWrapper(fn, tags=tags, precleanDir = NON_RT_Dir, filePattern = "*.SUM$", wpbTitle = "Cleaning SUM Files",
                    headerInFile=headerInFile, leadingBlanks=leadingBlanks, tz=tz, dtFormat=dtFormat, 
                    nacols=nacols, foutPrefix=foutPrefix, inferredHeader=inferredHeader, 
                    Rec_dtf_Hex_strings=Rec_dtf_Hex_strings, mergeFrac=mergeFrac)
}

# Lotek Autonomous. Files are already pre-filtered for tags from their "JST" file format
if (DoCleanLotek) {
  headerInFile = F
  leadingBlanks = 0
  tz = "GMT"
  dtFormat = "EPOCH"
  nacols = NULL
  foutPrefix = "LT"
  inferredHeader = c("datetime", "FracSec", "Dec", "Hex", "SigStr")
  Rec_dtf_Hex_strings = c(NA, "datetime", "Hex")
  mergeFrac = "FracSec"
  fn<-cleanInnerWrap()
  cleanOuterWrapper(fn, tags=tags, precleanDir = NON_RT_Dir, filePattern = "(*.LO_CSV)|(*.TXT)$", wpbTitle = "Cleaning LoTek LO_CSV and TXT files",
                    headerInFile=headerInFile, leadingBlanks=leadingBlanks, tz=tz, dtFormat=dtFormat, 
                    nacols=nacols, foutPrefix=foutPrefix, inferredHeader=inferredHeader, 
                    Rec_dtf_Hex_strings=Rec_dtf_Hex_strings, mergeFrac=mergeFrac)
}

# Last, do the files with less structure (needing customized code)
# ATS autonomous
if (DoCleanATS) {
  fn<-cleanATScsv() # clean ATS CSVs
  cleanWrapper(fn, tags, precleanDir = NON_RT_Dir, filePattern = "*.CSV$", wpbTitle = "Cleaning ATS CSV files")
  fn<-cleanATSxls() # converts Excel files to a CSV-style file, then cleans
  cleanWrapper(fn, tags, precleanDir = NON_RT_Dir, filePattern = "*.XLS[X]?$", wpbTitle = "Cleaning ATS XLS(x) files")
}

###Do the filtering loop
if (DoFilterFromSavedCleanedData) {
  filterData()
}
