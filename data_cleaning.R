
library(survival)
library(survminer)

View(rotterdam)
names(rotterdam)

rot1 = rotterdam
#meno, grade, hormon, chemo
rot1[c(4,6,10,11)] = sapply(rot1[c(4,6,10,11)], as.factor)

#first use tmerge to set stopping time
rot2 <- tmerge(rot1, rot1, pid, tstop=dtime)

#set time to relapse
rot2 <- tmerge(rot2, rot1, pid, relapse = tdc(rtime))

#status at the end, either they had an infection or they were censored
status <- as.integer(with(rot2, (tstop == dtime & death)))

#put together
rot2 <- data.frame(rot2,status)

rot2$relapse = as.factor(rot2$relapse)

sur_ob = Surv(time=rot1$dtime, event=rot1$death)

#time dependent relapse 
sur_cov <- Surv(time=rot2$tstart,
                time2=rot2$tstop,
                event=rot2$status,
                type="counting")



