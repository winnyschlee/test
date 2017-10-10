################################################################
# read data
################################################################
rm(list=ls())
setwd("/Users/winny2/Dropbox/TRI/Projects/TrackYourTinnitus/Data/Rdata")
load("TYT.Rdata")
library('chron')


################################################################
# SD and mean
################################################################
# make new data frame 
q2 = as.numeric(as.character(longi$question2))
user_id = as.factor(longi$user_id)
myframe = data.frame(user_id, q2)

# functions
namean = function(x){mean(x, na.rm=T)}
nasd = function(x){sqrt(var(x, na.rm=T))}

# Standard Deviation and Mean
sd.agg = aggregate(myframe, by = list(user_id), nasd)
mean.agg = aggregate(myframe, by = list(user_id), namean)

# plots
quartz(w=6, h=6)
plot(sort(sd.agg$q2), pch=19, xlab = 'Users (sorted)', ylab = 'Standard Deviation [Tinnitus loudness]', xaxt = 'n')

quartz(w=6, h=6)
plot(c(0,max(sd.agg$q2)), c(0,1), type = 'n', xlab = 'Standard Deviation [Tinnitus loudness]', ylab = 'Mean [Tinnitus loudness]')
points(sd.agg$q2, mean.agg$q2, pch = 19)


################################################################
# within day - variability
################################################################
# ???? is save_date correct?????
# ???? die ersten paar einträge löschen????

# make new data frames
q2 = as.numeric(as.character(longi$question2))
user_id = as.factor(longi$user_id)
save_date = as.Date(longi$save_date)
save_time = hours(strptime(longi$save_date, format="%Y-%m-%d %H:%M:%S"))
myframe = data.frame(user_id, q2, save_date, save_time)


user_ids = counts$user_id[-1]
withindayvar = rep(NA, length(user_ids))
betweendayvar = rep(NA, length(user_ids))
overallvar = rep(NA, length(user_ids))
varframeq2 = data.frame(user_ids, withindayvar, betweendayvar, overallvar) 

# withinday variability
for (i in 1:length(varframeq2$user_ids)){
	rm(list=ls(pattern='tmp'))
	tmp = subset(myframe, user_id == as.character(varframeq2$user_ids[i]))

	# only days with number of entries >=2
	tmp_length = aggregate(tmp, list(date = tmp$save_date), length)
	tmp_indx = which(length$save_date>=2)
	tmp_var  = aggregate(tmp, list(tmp$save_date), var)
	tmp_var_vec = tmp_var$q2[tmp_indx]
	
	varframeq2$withindayvar[i] = mean(tmp_var_vec, na.rm = T)
}



################################################################
# betweenday variability
################################################################

for (i in 1:length(varframeq2$user_ids)){
	rm(list=ls(pattern='tmp'))
	tmp = subset(myframe, user_id == as.character(varframeq2$user_ids[i]))

	tmp_mean  = aggregate(tmp, list(tmp$save_date), mean)
	
	varframeq2$betweendayvar[i] = var(tmp_mean$q2, na.rm = T)
}


################################################################
# overall variability
################################################################

for (i in 1:length(varframeq2$user_ids)){
	rm(list=ls(pattern='tmp'))
	tmp = subset(myframe, user_id == as.character(varframeq2$user_ids[i]))
	
	varframeq2$overallvar[i] = var(tmp$q2, na.rm = T)
}

# variance in q2 better explained by withinday-var or betweenday-var?
# nested in 

myframe$save_date = as.factor(myframe$save_date)
myframe$save_time = as.factor(myframe$save_time)

library('nlme')
lm()
# .... später vielleicht ...

################################################################
# how about the first notificiations?
################################################################

# make new data frames
q1 = as.numeric(as.character(longi$question1))
q2 = as.numeric(as.character(longi$question2))
q3 = as.numeric(as.character(longi$question3))
q4 = as.numeric(as.character(longi$question4))
q5 = as.numeric(as.character(longi$question5))
q6 = as.numeric(as.character(longi$question6))
q7 = as.numeric(as.character(longi$question7))
q8 = as.numeric(as.character(longi$question8))
user_id = as.factor(longi$user_id)
save_date = longi$save_date
kickstart.df = data.frame(user_id, q1, q2, q3, q4, q5, q6, q7, q8, save_date)

user_ids = counts$user_id[-1]
user_id = rep(user_ids, each = 20)
ssq1 = rep(NA, 20*length(user_ids))
ssq2 = rep(NA, 20*length(user_ids))
ssq3 = rep(NA, 20*length(user_ids))
ssq4 = rep(NA, 20*length(user_ids))
ssq5 = rep(NA, 20*length(user_ids))
ssq6 = rep(NA, 20*length(user_ids))
ssq7 = rep(NA, 20*length(user_ids))
ssq8 = rep(NA, 20*length(user_ids))
entrynr = rep(c(1:20), length(user_ids))


for (i in 1:length(user_ids)){
	rm(list=ls(pattern='tmp'))
	tmp = subset(kickstart.df, user_id == as.character(user_ids[i]))
	indx = order(tmp$save_date)[c(1:20)] # make sure to select the first 20 entries
	ssq1[c(((i-1)*20+1):(i*20))] = (tmp$q1[indx]-mean(tmp$q1))^2
	ssq2[c(((i-1)*20+1):(i*20))] = (tmp$q2[indx]-mean(tmp$q2))^2
	ssq3[c(((i-1)*20+1):(i*20))] = (tmp$q3[indx]-mean(tmp$q3))^2
	ssq4[c(((i-1)*20+1):(i*20))] = (tmp$q4[indx]-mean(tmp$q4))^2
	ssq5[c(((i-1)*20+1):(i*20))] = (tmp$q5[indx]-mean(tmp$q5))^2
	ssq6[c(((i-1)*20+1):(i*20))] = (tmp$q6[indx]-mean(tmp$q6))^2
	ssq7[c(((i-1)*20+1):(i*20))] = (tmp$q7[indx]-mean(tmp$q7))^2
	ssq8[c(((i-1)*20+1):(i*20))] = (tmp$q8[indx]-mean(tmp$q8))^2
	print(user_ids[i])
}

ssqkickstart.df = data.frame(user_id, entrynr, ssq1, ssq2, ssq3, ssq4, ssq5, ssq6, ssq7, ssq8)

ssqs.df = aggregate(ssqkickstart.df, by = list(ssqkickstart.df$entrynr), namean)

plot(ssqs.df$ssq1, type = 'l', xlab = 'notification number', ylab = 'Sum of Squares', main = 'Question 1 (OnOff)')
abline(h = (mean(ssqs.df$ssq1)+ 2*sqrt(var(ssqs.df$ssq1))), col = 'red')
plot(ssqs.df$ssq2, type = 'l', xlab = 'notification number', ylab = 'Sum of Squares', main = 'Question 2 (loudness)')
abline(h = (mean(ssqs.df$ssq2)+ 2*sqrt(var(ssqs.df$ssq2))), col = 'red')
plot(ssqs.df$ssq3, type = 'l')
abline(h = (mean(ssqs.df$ssq3)+ 2*sqrt(var(ssqs.df$ssq3))), col = 'red')
plot(ssqs.df$ssq4, type = 'l')
abline(h = (mean(ssqs.df$ssq4)+ 2*sqrt(var(ssqs.df$ssq4))), col = 'red')
plot(ssqs.df$ssq5, type = 'l', xlab = 'notification number', ylab = 'Sum of Squares', main = 'Question 5 (arousal)')
abline(h = (mean(ssqs.df$ssq5)+ 2*sqrt(var(ssqs.df$ssq5))), col = 'red')
plot(ssqs.df$ssq6, type = 'l')
abline(h = (mean(ssqs.df$ssq6)+ 2*sqrt(var(ssqs.df$ssq6))), col = 'red')
plot(ssqs.df$ssq7, type = 'l', xlab = 'notification number', ylab = 'Sum of Squares', main = 'Question 7 (concentration)')
abline(h = (mean(ssqs.df$ssq7)+ 2*sqrt(var(ssqs.df$ssq7))), col = 'red')
plot(ssqs.df$ssq8, type = 'l',  xlab = 'notification number', ylab = 'Sum of Squares', main = 'Question 8 (user-specific)')
abline(h = (mean(ssqs.df$ssq8)+ 2*sqrt(var(ssqs.df$ssq8))), col = 'red')


plot(c(1,20),c(0,0.7), type = 'n', xlab = '', ylab = '' )
for(i in 1:length(user_ids)){
	lines(subset(ssqkickstart.df, user_id == user_ids[i])$ssq2)
	print(i)
}




# Relation to time of day
strptime('2014-04-28 18:36:41', format="%Y-%m-%d %H:%M:%S")
strptime(longi$save_date[1], format="%Y-%m-%d %H:%M:%S")
hours(strptime(longi$save_date[1], format="%Y-%m-%d %H:%M:%S"))

# Relation to weekday / weekend
weekdays(strptime(longi$save_date[1], format="%Y-%m-%d %H:%M:%S"))
is.weekend(strptime(longi$save_date[1], format="%Y-%m-%d %H:%M:%S"))
is.holiday(strptime(longi$save_date[1], format="%Y-%m-%d %H:%M:%S"))







