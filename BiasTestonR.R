#For the first step, write code to create bias scores based on the Incongruent minus congruent formula -- one for happy faces and one for disgust (negative) faces. We can then expand the formula to compute other bias scores reported in the paper.
# We first need to take the average/mean RT for ALL incongruent trials of a given emotion (e.g., DIT) across the whole task for a given participant, and then use that mean to subtract the congruent RT (e.g., DCT) on EACH trial. So RT[incongruentMEAN] - RT[congruentTRIAL1 ... TRIAL2 etc.] -
#see the Evans and Britton paper I sent you earlier, page 2. You can use the Trial.Type variable to determine whether it's a congruent or incongruent trial, and then the DCT, DIT, HCT etc. variables for the actual RT. 
#Clear variable list
rm(list=ls())

#load in data
df <- read.csv(file = "C:/Users/jimmy/Documents/UCSD/Senior/TBI/501_502_503.csv", header=TRUE, sep =",")
#dataframe <- subset(dataframe, (Valence == 'Disgust' | Valence == 'Positive'| Valence == 'Neutral' | Valence == 'Angry' ), select=c(ExperimentName, Subject, Session, Block, BorderType, Condition, CorrectResponse, Stimulus.Resp, Stimulus.ACC, Stimulus.RT, Valence))
#Clean up data
df <- subset(df, (Valence.Trial. == 'disgustneu' | Valence.Trial. == 'happyneu'| Valence.Trial. == 'neuneu' ), select=c(Subject, Session, Trial, Target.RT.Trial., Probe.Trial., Target.RESP.Trial., EmoPosition.Trial., ProbePosition.Trial.,  Valence.Trial.))
df<- transform(df, Subject=as.numeric(Subject), Session=as.numeric(Session), Trial=as.numeric(Trial), Target.RESP.Trial.=as.numeric(Target.RESP.Trial.), Target.RT.Trial.=as.numeric(Target.RT.Trial.))

#Change inaccurate data to R friendly NA
for (i in 1:(nrow(df)))
{
  if((df$Probe.Trial.[i]=="E" & df$Target.RESP.Trial.[i] == "2") | (df$Probe.Trial.[i] == "F" & df$Target.RESP.Trial.[i] == "1"))
  {
    df$Target.RT.Trial.[i] <- NA
  }
}

#Create incongruent and congruent trials
attach(df)
df$DCT = ifelse(((EmoPosition.Trial. == "top" & ProbePosition.Trial. == "top") | (EmoPosition.Trial. == "bottom" & ProbePosition.Trial. == "bottom")) & (Valence.Trial. == "disgustneu"), Target.RT.Trial., NA)
df$DIT = ifelse(((EmoPosition.Trial. == "top" & ProbePosition.Trial. == "bottom") | (EmoPosition.Trial. == "bottom" & ProbePosition.Trial. == "top")) & (Valence.Trial. == "disgustneu"), Target.RT.Trial., NA)
df$HCT = ifelse(((EmoPosition.Trial. == "top" & ProbePosition.Trial. == "top") | (EmoPosition.Trial. == "bottom" & ProbePosition.Trial. == "bottom")) & (Valence.Trial. == "happyneu"), Target.RT.Trial., NA)
df$HIT = ifelse(((EmoPosition.Trial. == "top" & ProbePosition.Trial. == "bottom") | (EmoPosition.Trial. == "bottom" & ProbePosition.Trial. == "top")) & (Valence.Trial. == "happyneu"), Target.RT.Trial., NA)
df$NTT = ifelse((ProbePosition.Trial. == "top" & Valence.Trial. == 'neuneu'), Target.RT.Trial., NA)
df$NBT = ifelse((ProbePosition.Trial. == "bottom" & Valence.Trial. == 'neuneu'), Target.RT.Trial., NA)

df$Trial.Type[((EmoPosition.Trial. == "top" & ProbePosition.Trial. == "top") | (EmoPosition.Trial. == "bottom" & ProbePosition.Trial. == "bottom")) & (Valence.Trial. == 'disgustneu')] <- "DCTrial"
df$Trial.Type[((EmoPosition.Trial. == "top" & ProbePosition.Trial. == "bottom") | (EmoPosition.Trial. == "bottom" & ProbePosition.Trial. == "top")) & (Valence.Trial. == 'disgustneu')] <- "DITrial"
df$Trial.Type[((EmoPosition.Trial. == "top" & ProbePosition.Trial. == "top") | (EmoPosition.Trial. == "bottom" & ProbePosition.Trial. == "bottom")) & (Valence.Trial. == 'happyneu')] <- "HCTrial"
df$Trial.Type[((EmoPosition.Trial. == "top" & ProbePosition.Trial. == "bottom") | (EmoPosition.Trial. == "bottom" & ProbePosition.Trial. == "top")) & (Valence.Trial. == 'happyneu')] <- "HITrial"
df$Trial.Type[(ProbePosition.Trial. == "top"  & Valence.Trial. == 'neuneu')] <- "NTTrial"
df$Trial.Type[(ProbePosition.Trial. == "bottom" & Valence.Trial. == 'neuneu')] <- "NBTrial"
detach(df)

df <- df
df$bias.score <- list(c(0))
df$bias.scorecN <- list(c(0))
df$bias.scoreiN <- list(c(0))
subjects = split(df, df$Subject) #Splits data by subjects

for (s in seq_along(subjects))
{
  sub1 <- subjects[[s]]
  tmp <- subjects[[s]]
  len = nrow(sub1)
  avgDIT <- mean(sub1$DIT,na.rm=TRUE)
  avgHIT <- mean(sub1$HIT,na.rm=TRUE)
  NTT <- mean(sub1$NTT,na.rm=TRUE)
  NBT <- mean(sub1$NBT, na.rm=TRUE)
  avgNeut <- (NTT+NBT)/2
  
  #Loop through Subject 501:503 and compute bias scores based on paper
  for (i in 1:len)
  { 
    if(!is.na(sub1$DCT[i]))
    {sub1$bias.score[[i]] <- avgDIT - sub1$DCT[i]
    sub1$bias.scorecN[[i]] <- avgNeut - sub1$DCT[i]
    }
    else if(!is.na(sub1$HCT[i]))
    {sub1$bias.score[[i]] <- avgHIT - sub1$HCT[i]
    sub1$bias.scorecN[[i]] <- avgNeut - sub1$HCT[i]
    }
    else if(!is.na(sub1$DIT[i]))
    {sub1$bias.scoreiN[[i]] <- sub1$DIT[i] - avgNeut}
    else if(!is.na(sub1$HIT[i]))
    {sub1$bias.scoreiN[[i]] <- sub1$HIT[i] - avgNeut}
    
    
  }
  
  
  #assign(paste('Bias(RT[incon]-RT[cong]: Subject', sep="", s), as.data.frame(sub1))
  tmp <- assign(paste('Bias(RT[incon]-RT[cong]: Subject', sep="", s), as.data.frame(sub1))
  tmp$bias.score[tmp$bias.score == "NA"]  <- 0
  tmp$bias.scoreN[tmp$bias.scoreiN == "NA"]  <- 0
  
  DT <- subset(tmp, select=c(Subject, Session,Trial.Type, bias.score, bias.scoreiN,bias.scorecN))
  HI <- subset(tmp, select=c(Subject, Session,Trial.Type, bias.score, bias.scoreiN,bias.scorecN))
  DTi <-tmp[which(tmp$Valence.Trial.=='disgustneu'),]
  HIi <- tmp[which(tmp$Valence.Trial.=='happyneu'),]
  #DTc <-tmp[which(tmp$Valence.Trial. =='disgustdisgust'),]
  #HTc <-tmp[which(tmp$Valence.Trial. =='happyhappy'),]
  
  x <- subset(HIi,bias.score>0)
  y <- subset(HIi,bias.score<0)
  z <- subset(DTi,bias.score>0)
  w <- subset(DTi,bias.score<0)
  
  hx <- subset(HIi, bias.scoreiN>0)
  hy <- subset(HIi, bias.scoreiN<0)
  hz <- subset(HIi, bias.scorecN>0)
  hw <- subset(HIi, bias.scorecN<0)
  
  dx <- subset(DTi, bias.scoreiN>0)
  dy <- subset(DTi, bias.scoreiN<0)
  dz <- subset(DTi, bias.scorecN>0)
  dw <- subset(DTi, bias.scorecN<0)
  
  
  datavalues <- data.frame("SID" = tmp$Subject[1], "Session" = tmp$Session[1], "happy_vig" = mean(as.numeric(x$bias.score)), "happy_avo" = mean(as.numeric(y$bias.score)), "disgust_vig" = mean(as.numeric(z$bias.score)), "disgust_avo" = mean(as.numeric(w$bias.score)), "disgust_vigO" = mean(as.numeric(dx$bias.scoreiN)), "disgust_avoO" = mean(as.numeric(dy$bias.scoreiN)), "disgust_slow" = mean(as.numeric(dz$bias.scorecN)), "disgust_fast" = mean(as.numeric(dw$bias.scorecN)),"happy_vigO" = mean(as.numeric(hx$bias.scoreiN)), "happy_avoO" = mean(as.numeric(hy$bias.scoreiN)), "happy_slow" = mean(as.numeric(hz$bias.scorecN)), "happy_fast" = mean(as.numeric(hw$bias.scorecN)))
  assign(paste('SubResults', sep="", s), as.data.frame(datavalues))
  
  
  #outfile <- paste(mydir, "SubResults",sep="",s,".csv")
  #write.csv(datavalues, outfile, row.names=F)
}


