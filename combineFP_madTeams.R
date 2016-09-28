library(h2o)

combine1 = merge(Scrape2015,matchups[,c('AwayAbr','HomeAbr','Week','Year')], by.x=c('Team','Week','Year'), by.y=c('AwayAbr','Week','Year'))
names(combine1) = c('Team','Week','Year','Player','FP','Position','variable','NameRM','Key','Opponent')

combine2 = merge(Scrape2015,matchups[,c('AwayAbr','HomeAbr','Week','Year')], by.x=c('Team','Week','Year'), by.y=c('HomeAbr','Week','Year'))
names(combine2) = c('Team','Week','Year','Player','FP','Position','variable','NameRM','Key','Opponent')

combine = rbind(combine1,combine2)
combine[duplicated(combine),]
str(combine)
combine = unique(rbind(combine1,combine2))
str(combine)

combine$Team = ifelse(combine$Team == 'WSH', 'WAS', combine$Team)
combine$Team = ifelse(combine$Team == 'STL', 'LA', combine$Team)
combine$Opponent = ifelse(combine$Opponent == 'WSH', 'WAS', combine$Opponent)
combine$Opponent = ifelse(combine$Opponent == 'STL', 'LA', combine$Opponent)

combine = merge(combine,teams,by=c('Year','Week','Opponent'))
combine = combine[,!grepl('.Key',names(combine))]
names(combine)[grepl('.ovr',names(combine))] = paste0('Opponent.',names(combine)[grepl('.ovr',names(combine))])
dim(combine)

combine = merge(combine,teams,by.x=c('Year','Week','Team'), by.y=c('Year','Week','Opponent'),all.x=TRUE)
combine = combine[,!grepl('.Key',names(combine))]
names(combine)[grepl('.ovr',names(combine))] = paste0('Team.',names(combine)[grepl('.ovr',names(combine))])
dim(combine)

combine = merge(combine,mad[,c('Year','Week','Team', 'Key','Overall','file')], by=c('Year','Week','Team', 'Key'),all.x = TRUE)
dim(combine)

combine$Overall[combine$Position=='D' & is.na(combine$Overall)]   = rowMeans(combine[combine$Position=='D' & is.na(combine$Overall),
                                   c('Team.DL.1.ovr','Team.DL.2.ovr','Team.DL.3.ovr','Team.DL.4.ovr'
                                     ,'Team.LB.1.ovr','Team.LB.2.ovr','Team.LB.3.ovr','Team.CB.1.ovr'
                                     ,'Team.CB.2.ovr','Team.FS.1.ovr','Team.SS.1.ovr') ])

table(combine$Overall, useNA='ifany')
dim(combine)

#REMOVE NA OVERALLS
#save(combine,file='combineBest.Rda')
#load('combineBest.Rda')
combine = combine[!is.na(combine$Overall),]

write.csv(combine[is.na(combine$Overall),],'test.csv')
write.csv(Scrape2015,'Scrape.csv')
write.csv(mad,'test_mad.csv')
          
localH2O = h2o.init(nthreads=-1, max_mem_size="2G")
h2o.removeAll()

set.seed(1)
train.which = sample(length(combine$Key),floor(length(combine$Key)*.8))
train = as.h2o(combine[train.which,])
test = as.h2o(combine[-train.which,])

#ep <- c(1,25,50,100,250, 500)
#hidden = list(10, 25,100, c(10,10), c(25,25), c(100,100), c(10,10,10), c(25,25,25), c(100,100,100))
trials = expand.grid(epo=c(1,25,50,100,250, 500)
                     ,hid=list(10, 25,100, c(10,10), c(25,25), c(100,100), c(10,10,10), c(25,25,25), c(100,100,100))
                     ,drop_ratio = c(0,0.05)
                     ,rt=c(0.01,0.02)
                     ,rt_annealing=c(1e-8,1e-7,1e-6)
                     )

modelids = character()
m.RMSE = double()
for (i in 1:length(trials$epo)) {
    
  model = h2o.deeplearning(x=c(3,8,9,11:61), y=7
                           , train, validation_frame = test
                           , epochs=trials$epo[i]
                           , hidden=trials$hid[[i]]
                           , rate = trials$rt[i]
                           , rate_annealing = trials$rt_annealing[i]
                           , input_dropout_ratio = trials$drop_ratio[i]
                           , model_id = paste0('model_id_',i)
                           , l1=1e-5
                           , l2=1e-5
                           ,use_all_factor_levels=TRUE
                           ,variable_importances=TRUE
                           )
  modelids[i]=paste0('model_id_',i)
  m.RMSE[i]=model@model$validation_metrics@metrics$RMSE
}
m.RMSE[which.min(m.RMSE)]
model = h2o.getModel(modelids[which.min(m.RMSE)])
model@model$variable_importances

