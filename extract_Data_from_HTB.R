#!/usr/bin/Rscript

extractData = function(fileID){
                   mydat3=read.csv(paste0("m9c0r", fileID, "_Raw_Events.csv"), header=FALSE);
                   mymatrix3=matrix(mydat3$V1, nrow=5000);

                   mydat_traces=read.csv(paste0("m9c0r", fileID, "_Raw_Traces.csv"), header=FALSE);
		   mydat_traces=mydat_traces$V3;
		   mydat_traces=matrix(mydat_traces, nrow=1000);

		   m_trace_index=0;

                   ret=apply(mymatrix3, 2, function(x){
                                                m_trace_index <<- m_trace_index + 1;
						mytrace=mydat_traces[,m_trace_index];

				                m_events_span=(x!=0);
				                m_events=x[m_events_span];
						m_events_pos=which(m_events_span);
						m_begin=1;
                                                latency=-1; 
						duration=-1;
						for(i in 1:(length(m_events)-1)){
                                                   if(m_events[i]==25 && m_events[i+1]==24 ||
						      m_events[i]==15 && m_events[i+1]==1  ||
						      m_events[i]==13 && m_events[i+1]==1){
					                  m_begin=m_events_pos[i];
					                  break;
						   }
				                }		
						t=x[m_begin:length(x)];
						t_trace=mytrace[(m_begin/5):1000];
						k0=which(t==3);
						k10=which(t==10);
                                                k1=which(t==6); 
                                                k2=which(t==7); 
						k3=which(t==26);
						stimulus=-1;
						fixation=-1;
						direction=-1;

                                                if(length(k1)>0 && length(k2)>0){latency=k2[1]-k1[1];}; 
                                                if(length(k1)>0 && length(k3)>0){stimulus=k3[1]-k2[1];}; 
       						if(length(k2)>0){
      						   m_d=t[k2:length(t)];
      						   k3=which(m_d==3);
      						   if(length(k3)>0) duration=k3[1]-1;
      						}

                                                if(length(k0)>0){
						   m_fix_begin=0;
                                                   if(length(k2)>0){
                                                        m_fix_begin=k2[1]; 
						   } else if(length(k10)>0){
                                                        m_fix_begin=k10[1];
						   }
						   if(m_fix_begin>0){
                                                      fixation=m_fix_begin-k0[1];
						      trace_range=t_trace[m_fix_begin/5]-t_trace[k0[1]/5];
						      if(trace_range>50){
							      direction=1;
						      } else if(trace_range<-50){
							      direction=2;
						      } else  direction=0;
						   }
						}

						interval=-1;
						for(i in 1:(length(m_events)-1)){
                                                   if(m_events[i]==26 && m_events[i+1]==3){
						      m_begin=m_events_pos[i];
						      t=x[m_begin:length(x)];
                                                      k1=which(t==3);
						      k2=which(t==7);
                                                      if(length(k1)>0 && length(k2)>0){interval=k2[1]-k1[1];}; 
					              break;   
					           }
						}

                                                data.frame(latency, duration, interval, stimulus, fixation, direction)});

		   ret=do.call(rbind, ret);

                   mydat=read.table(pipe(paste0("sed -n '71,${H;n;H;n;H;n;H;n;H;n;H;n;H;n;g;s,\\r\\n, ,g;s,\\n,,g;s,[A-Za-z_#()],,g;p;s,^.*$,,g;h;}' newLogs/m9c0r",fileID,".log")),header=FALSE);
                   mydat$latency=ret$latency[mydat$V1];
                   mydat$duration=ret$duration[mydat$V1];
                   mydat$interval=ret$interval[mydat$V1];
                   mydat$stimulus=ret$stimulus[mydat$V1];
                   mydat$fixation=ret$fixation[mydat$V1];
                   mydat$direction=ret$direction[mydat$V1];

                   myindex=1:length(ret);
                   myindex=myindex[-mydat$V1];
                   ret=ret[myindex,];

                   tmp=with(mydat, which(V2==0 & V6==2 & V7==4));
                   myfull=as.vector(sapply(tmp, function(x){(x-7):x}));
                   mydat$full=0;  

                   mydat$errors=0;
                   mydat$rewards=0;
                   pre_rewards_num=16;
                   if(length(tmp)>0){
		       mydat$full[myfull]=1;
                       tmpdf=data.frame(x0=c(0,tmp[-length(tmp)])+1,x1=tmp);
                       apply(tmpdf, 1, function(x){
                                           tmp=mydat[x[1]:x[2],]; 
                                           errors_condition=(tmp$V2!=0);
                                           if(any(errors_condition)) tmp[errors_condition,]$errors=1;
                                           mydat[x[1]:x[2],]$errors <<- cumsum(tmp$errors);
                                           x1=x[2]-7-pre_rewards_num;
                                           rewards_span=ifelse(x1<1,1,x1):(x[2]-7);
                                           tmp=mydat[rewards_span,];
                                           tmp$rewards=0;
                                           if(any(tmp$V2==0 & tmp$V7==4)) tmp[tmp$V2==0 & tmp$V7==4,]$rewards=1;
                                           mydat[x[1]:x[2],]$rewards <<- sum(tmp$rewards);
                                           });
		   }
                   mydat$fileID=fileID;
		   print(fileID);
                   mydat  
                }


allData=do.call(rbind, lapply(c(22:40, 42:44, 50:66,  69:126), extractData));

allData$m_index=1:length(allData[,1]);
allData$window=with(allData, (V6-1)*4+V7);
#allData=subset(allData, latency>0);

write.csv(allData, "allData_22to126.csv");

