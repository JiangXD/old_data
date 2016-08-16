#!/usr/bin/Rscript
library(ggplot2);

getstate = function(fileID){
                   mydat3=read.csv(paste0("m9c0r", fileID, "_Raw_Events.csv"), header=FALSE);
                   mymatrix3=matrix(mydat3$V1, nrow=5000);

                   ret=apply(mymatrix3, 2, function(x){
				                m_events_span=(x!=0);
				                m_events=x[m_events_span];
						m_events_pos=which(m_events_span);
						m_begin=1;
						for(i in 1:(length(m_events)-1)){
                                                   if(m_events[i]==25 && m_events[i+1]==24 ||
						      m_events[i]==15 && m_events[i+1]==1  ||
						      m_events[i]==13 && m_events[i+1]==1){
					                  m_begin=i;
					                  break;
						   }
				                }		
                                                curr=m_events[m_begin:length(m_events)];
                                                m_stat=-4;
						for(i in 1:(length(curr)-2)){
                                                   if(curr[i]==6 && curr[i+1]==7){
                                                      if(curr[i+2]==8) m_stat=1;
						      if(curr[i+2]==9) m_stat=2;
						      break;
						   }
						}
						if(10 %in% curr) m_stat=-2;
						m_stat
                                           });
ret
}



gen_logs = function(fileID){

             fulltrials=1;
             subtrial=1;
             saccseq=1;
	     id=1;

             myseq=getstate(fileID);
	     filename=paste0("newLogs/m9c0r",fileID,".log");
	     cat(paste0(rep("nextline\n",70)), file=filename);

             lapply(myseq, function(x){
		ret=data.frame(full=fulltrials, sub=subtrial, sacc=saccseq, error=0);
		if(saccseq<=2 && x==1 || saccseq>2 && x==2){
                    
		   if(saccseq==4){
		      saccseq <<- 1;
		      if(subtrial==2)fulltrials <<- fulltrials + 1;
		      subtrial <<- ifelse(subtrial==2, 1, 2);
		   } else saccseq <<- saccseq + 1;

		} else {
                   saccseq <<- 1;
		   subtrial <<- 1;
		   ret$error=-x;
		};

        	err_msg=paste0("OUTCOME ", ret$err);
                
		if(ret$error==0){
                   err_tail="(CORRECT)";
		}else if(ret$error==2){
                   err_tail="(ERR_BROKE_FIXATION)";
		}else if(ret$error==4){
                   err_tail="(ERR_MISSED_TARGETS)";
		}else err_tail="(OTHER*************)";
		err_msg=paste0(err_msg, err_tail);


                cat(paste0("TRIAL#    ",id,"\n",
			   err_msg, "\n",
			   "FIX_TO_STIMULUS_TIME 200\n",
			   "STIMULUS_TIME 700\n",
			   "FullTrial  ",ret$full,"\n",
			   "SubTrial  ",ret$sub,"\n",
			   "SaccSeq  ",ret$sacc,"\n",
			   "SEQUENCE_VAL  2211\n"), file=filename, append=TRUE);
		id <<- id+1;
                
             });

       fileID 
}




#main entry

log_ids=c(22:40,42:44,50:66,69:126);
sapply(log_ids, function(x){print(gen_logs(x))});


