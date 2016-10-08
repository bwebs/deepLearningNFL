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

combine = merge(combine,mad[,c('Year','Week','Team', 'Key','Overall','Rank','Depth','file')]
                , by=c('Year','Week','Team', 'Key'),all.x = TRUE)
dim(combine)

combine$Overall[combine$Position=='D' & is.na(combine$Overall)]   = rowMeans(combine[combine$Position=='D' & is.na(combine$Overall),
                                   c('Team.DL.1.ovr','Team.DL.2.ovr','Team.DL.3.ovr','Team.DL.4.ovr'
                                     ,'Team.LB.1.ovr','Team.LB.2.ovr','Team.LB.3.ovr','Team.CB.1.ovr'
                                     ,'Team.CB.2.ovr','Team.FS.1.ovr','Team.SS.1.ovr') ])
unique(combine[is.na(combine$Overall),'Key'])

table(combine$Overall, useNA='ifany')
dim(combine)

write.csv(Scrape2015,'Scrape.csv')
write.csv(mad,'test_mad.csv')

#REMOVE NA OVERALLS
#save(combine,file='combineBest.Rda')
#load('combineBest.Rda')
combine = combine[!is.na(combine$Overall),]


# New Variables -----------------------------------------------------------

combine$Defense.Team = combine$Team.DL.1.ovr + combine$Team.DL.2.ovr + combine$Team.DL.3.ovr + combine$Team.DL.4.ovr + combine$Team.LB.1.ovr + combine$Team.LB.2.ovr + combine$Team.LB.3.ovr + combine$Team.CB.1.ovr + combine$Team.CB.2.ovr + combine$Team.FS.1.ovr + combine$Team.SS.1.ovr
combine$Defense.Opponent = combine$Team.Opponent.DL.1.ovr + combine$Team.Opponent.DL.2.ovr + combine$Team.Opponent.DL.3.ovr + combine$Team.Opponent.DL.4.ovr + combine$Team.Opponent.LB.1.ovr + combine$Team.Opponent.LB.2.ovr + combine$Team.Opponent.LB.3.ovr + combine$Team.Opponent.CB.1.ovr + combine$Team.Opponent.CB.2.ovr + combine$Team.Opponent.FS.1.ovr + combine$Team.Opponent.SS.1.ovr
combine$Offense.Team = combine$Team.QB.1.ovr + combine$Team.WR.1.ovr + combine$Team.WR.2.ovr + combine$Team.WR.3.ovr + combine$Team.RB.1.ovr + combine$Team.RB.2.ovr + combine$Team.OL.1.ovr + combine$Team.OL.2.ovr + combine$Team.OL.3.ovr + combine$Team.OL.4.ovr + combine$Team.OL.5.ovr + combine$Team.TE.1.ovr
combine$Offense.Opponent = combine$Team.Opponent.QB.1.ovr + combine$Team.Opponent.WR.1.ovr + combine$Team.Opponent.WR.2.ovr + combine$Team.Opponent.WR.3.ovr + combine$Team.Opponent.RB.1.ovr + combine$Team.Opponent.RB.2.ovr + combine$Team.Opponent.OL.1.ovr + combine$Team.Opponent.OL.2.ovr + combine$Team.Opponent.OL.3.ovr + combine$Team.Opponent.OL.4.ovr + combine$Team.Opponent.OL.5.ovr + combine$Team.Opponent.TE.1.ovr
combine$Core.Offense.Team = combine$Team.QB.1.ovr + combine$Team.WR.1.ovr + combine$Team.RB.1.ovr + combine$Team.OL.1.ovr + combine$Team.OL.2.ovr + combine$Team.TE.1.ovr
combine$Core.Offense.Opponent = combine$Team.Opponent.QB.1.ovr + combine$Team.Opponent.WR.1.ovr + combine$Team.Opponent.RB.1.ovr + combine$Team.Opponent.OL.1.ovr + combine$Team.Opponent.OL.2.ovr + combine$Team.Opponent.TE.1.ovr
combine$Core.Defense.Team = combine$Team.DL.1.ovr + combine$Team.DL.2.ovr + combine$Team.LB.1.ovr + combine$Team.CB.1.ovr + combine$Team.FS.1.ovr + combine$Team.SS.1.ovr
combine$Core.Defense.Opponent = combine$Team.Opponent.DL.1.ovr + combine$Team.Opponent.DL.2.ovr + combine$Team.Opponent.LB.1.ovr + combine$Team.Opponent.CB.1.ovr + combine$Team.Opponent.FS.1.ovr + combine$Team.Opponent.SS.1.ovr
combine$WRs.Team = combine$Team.WR.1.ovr + combine$Team.WR.2.ovr + combine$Team.WR.3.ovr
combine$WRs.Opponent = combine$Team.Opponent.WR.1.ovr + combine$Team.Opponent.WR.2.ovr + combine$Team.Opponent.WR.3.ovr
combine$OL.Team = combine$Team.OL.1.ovr + combine$Team.OL.2.ovr + combine$Team.OL.3.ovr + combine$Team.OL.4.ovr
combine$OL.Opponent = combine$Team.Opponent.OL.1.ovr + combine$Team.Opponent.OL.2.ovr + combine$Team.Opponent.OL.3.ovr + combine$Team.Opponent.OL.4.ovr
combine$DL.Team = combine$Team.DL.1.ovr + combine$Team.DL.2.ovr + combine$Team.DL.3.ovr + combine$Team.DL.4.ovr
combine$DL.Opponent = combine$Team.Opponent.DL.1.ovr + combine$Team.Opponent.DL.2.ovr + combine$Team.Opponent.DL.3.ovr + combine$Team.Opponent.DL.4.ovr
combine$Secondary.Team = combine$Team.CB.1.ovr + combine$Team.CB.2.ovr + combine$Team.FS.1.ovr + combine$Team.SS.1.ovr
combine$Secondary.Opponent = combine$Team.Opponent.CB.1.ovr + combine$Team.Opponent.CB.2.ovr + combine$Team.Opponent.FS.1.ovr + combine$Team.Opponent.SS.1.ovr
combine$Box.Team = combine$Team.DL.1.ovr + combine$Team.DL.2.ovr + combine$Team.DL.3.ovr + combine$Team.DL.4.ovr + combine$Team.LB.1.ovr + combine$Team.LB.2.ovr + combine$Team.LB.3.ovr
combine$Box.Opponent = combine$Team.Opponent.DL.1.ovr + combine$Team.Opponent.DL.2.ovr + combine$Team.Opponent.DL.3.ovr + combine$Team.Opponent.DL.4.ovr + combine$Team.Opponent.LB.1.ovr + combine$Team.Opponent.LB.2.ovr + combine$Team.Opponent.LB.3.ovr
combine$LBs.Team = combine$Team.LB.1.ovr + combine$Team.LB.2.ovr + combine$Team.LB.3.ovr
combine$LBs.Opponent = combine$Team.Opponent.LB.1.ovr + combine$Team.Opponent.LB.2.ovr + combine$Team.Opponent.LB.3.ovr
combine$Ss.Team = combine$Team.FS.1.ovr + combine$Team.SS.1.ovr
combine$Ss.Opponent = combine$Team.Opponent.FS.1.ovr + combine$Team.Opponent.SS.1.ovr
combine$CBs.Team = combine$Team.CB.1.ovr + combine$Team.CB.2.ovr + combine$Team.CB.3.ovr
combine$CB.Opponent = combine$Team.Opponent.CB.1.ovr + combine$Team.Opponent.CB.2.ovr + combine$Team.Opponent.CB.3.ovr
combine$Matchup.All.Team = combine$Team.Opponent.DL.1.ovr - combine$Team.Opponent.DL.2.ovr - combine$Team.Opponent.DL.3.ovr - combine$Team.Opponent.DL.4.ovr - combine$Team.Opponent.LB.1.ovr - combine$Team.Opponent.LB.2.ovr - combine$Team.Opponent.LB.3.ovr - combine$Team.Opponent.CB.1.ovr - combine$Team.Opponent.CB.2.ovr - combine$Team.Opponent.FS.1.ovr - combine$Team.Opponent.SS.1.ovr + combine$Team.QB.1.ovr + combine$Team.WR.1.ovr + combine$Team.WR.2.ovr + combine$Team.WR.3.ovr + combine$Team.RB.1.ovr + combine$Team.RB.2.ovr + combine$Team.OL.1.ovr + combine$Team.OL.2.ovr + combine$Team.OL.3.ovr + combine$Team.OL.4.ovr + combine$Team.OL.5.ovr + combine$Team.TE.1.ovr
combine$Matchup.All.Opponent = combine$Team.Opponent.QB.1.ovr + combine$Team.Opponent.WR.1.ovr + combine$Team.Opponent.WR.2.ovr + combine$Team.Opponent.WR.3.ovr + combine$Team.Opponent.RB.1.ovr + combine$Team.Opponent.RB.2.ovr + combine$Team.Opponent.OL.1.ovr + combine$Team.Opponent.OL.2.ovr + combine$Team.Opponent.OL.3.ovr + combine$Team.Opponent.OL.4.ovr + combine$Team.Opponent.OL.5.ovr + combine$Team.Opponent.TE.1.ovr - combine$Team.DL.1.ovr - combine$Team.DL.2.ovr - combine$Team.DL.3.ovr - combine$Team.DL.4.ovr - combine$Team.LB.1.ovr - combine$Team.LB.2.ovr - combine$Team.LB.3.ovr - combine$Team.CB.1.ovr - combine$Team.CB.2.ovr - combine$Team.FS.1.ovr - combine$Team.SS.1.ovr
combine$Matchup.Cores.Team = combine$Team.Opponent.DL.1.ovr - combine$Team.Opponent.DL.2.ovr - combine$Team.Opponent.LB.1.ovr - combine$Team.Opponent.CB.1.ovr - combine$Team.Opponent.FS.1.ovr - combine$Team.Opponent.SS.1.ovr + combine$Team.QB.1.ovr + combine$Team.WR.1.ovr + combine$Team.RB.1.ovr + combine$Team.OL.1.ovr + combine$Team.OL.2.ovr + combine$Team.TE.1.ovr
combine$Matchup.Cores.Opponent = combine$Team.Opponent.QB.1.ovr + combine$Team.Opponent.WR.1.ovr + combine$Team.Opponent.RB.1.ovr + combine$Team.Opponent.OL.1.ovr + combine$Team.Opponent.OL.2.ovr + combine$Team.Opponent.TE.1.ovr - combine$Team.DL.1.ovr - combine$Team.DL.2.ovr - combine$Team.LB.1.ovr - combine$Team.CB.1.ovr - combine$Team.FS.1.ovr - combine$Team.SS.1.ovr
combine$Matchup.QBRBDL.Team = combine$Team.Opponent.DL.1.ovr - combine$Team.Opponent.DL.2.ovr - combine$Team.Opponent.DL.3.ovr - combine$Team.Opponent.DL.4.ovr + combine$Team.QB.1.ovr + combine$Team.RB.1.ovr
combine$Matchup.QBRBDL.Opponent = combine$Team.Opponent.QB.1.ovr + combine$Team.Opponent.RB.1.ovr - combine$Team.DL.1.ovr - combine$Team.DL.2.ovr - combine$Team.DL.3.ovr - combine$Team.DL.4.ovr
combine$Matchup.QBWR1RB1FS1CB1LB1.Team = combine$Team.Opponent.LB.1.ovr - combine$Team.Opponent.CB.1.ovr - combine$Team.Opponent.FS.1.ovr + combine$Team.QB.1.ovr + combine$Team.WR.1.ovr + combine$Team.RB.1.ovr
combine$Matchup.QBWR1RB1FS1CB1LB1.Opponent = combine$Team.Opponent.QB.1.ovr + combine$Team.Opponent.WR.1.ovr + combine$Team.Opponent.RB.1.ovr - combine$Team.LB.1.ovr - combine$Team.CB.1.ovr - combine$Team.FS.1.ovr
combine$Matchup.QBDL.Team = combine$Team.Opponent.DL.1.ovr - combine$Team.Opponent.DL.2.ovr - combine$Team.Opponent.DL.3.ovr - combine$Team.Opponent.DL.4.ovr + combine$Team.QB.1.ovr
combine$Matchup.QBDL.Opponent = combine$Team.Opponent.QB.1.ovr - combine$Team.DL.1.ovr - combine$Team.DL.2.ovr - combine$Team.DL.3.ovr - combine$Team.DL.4.ovr
combine$Matchup.RBDL.Team = combine$Team.Opponent.DL.1.ovr - combine$Team.Opponent.DL.2.ovr - combine$Team.Opponent.DL.3.ovr - combine$Team.Opponent.DL.4.ovr + combine$Team.RB.1.ovr
combine$Matchup.RB1DL.Opponent = combine$Team.Opponent.RB.1.ovr - combine$Team.DL.1.ovr - combine$Team.DL.2.ovr - combine$Team.DL.3.ovr - combine$Team.DL.4.ovr
combine$Matchup.RB12DL.Team = combine$Team.Opponent.DL.1.ovr - combine$Team.Opponent.DL.2.ovr - combine$Team.Opponent.DL.3.ovr - combine$Team.Opponent.DL.4.ovr + combine$Team.RB.2.ovr
combine$Matchup.RB2DL.Opponent = combine$Team.Opponent.RB.2.ovr - combine$Team.DL.1.ovr - combine$Team.DL.2.ovr - combine$Team.DL.3.ovr - combine$Team.DL.4.ovr
combine$Matchup.OLDL.Team = combine$Team.Opponent.DL.1.ovr - combine$Team.Opponent.DL.2.ovr - combine$Team.Opponent.DL.3.ovr - combine$Team.Opponent.DL.4.ovr + combine$Team.OL.1.ovr + combine$Team.OL.2.ovr + combine$Team.OL.3.ovr + combine$Team.OL.4.ovr + combine$Team.OL.5.ovr
combine$Matchup.OLDL.Opponent = combine$Team.Opponent.OL.1.ovr + combine$Team.Opponent.OL.2.ovr + combine$Team.Opponent.OL.3.ovr + combine$Team.Opponent.OL.4.ovr + combine$Team.Opponent.OL.5.ovr - combine$Team.DL.1.ovr - combine$Team.DL.2.ovr - combine$Team.DL.3.ovr - combine$Team.DL.4.ovr
combine$Matchup.WRCB3.Team = combine$Team.Opponent.CB.3.ovr + combine$Team.WR.3.ovr
combine$Matchup.WRCB3.Opponent = combine$Team.Opponent.WR.3.ovr - combine$Team.CB.3.ovr
combine$Matchup.WRCB2.Team = combine$Team.Opponent.CB.2.ovr + combine$Team.WR.2.ovr
combine$Matchup.WRCB2.Opponent = combine$Team.Opponent.WR.2.ovr - combine$Team.CB.2.ovr
combine$Matchup.WRCB1.Team = combine$Team.Opponent.CB.1.ovr + combine$Team.WR.1.ovr
combine$Matchup.WRCB1.Opponent = combine$Team.Opponent.WR.1.ovr - combine$Team.CB.1.ovr
          
# Try ALL ------------------------------------------------------------
names(combine)
variables = c(61,63,2,9,11:60,65:110)
response = 7
factors = c(2,9,63)
combine$Depth = as.factor(combine$Depth)
combine$variable = as.factor(combine$variable)
combine$Week = as.factor(combine$Week)
names(combine[,variables])

localH2O = h2o.init(nthreads=-1, max_mem_size="4G")
h2o.removeAll()

positions = c('D') #'QB','RB','WR','TE','K',
hyper_params <- list(
  activation=c("Rectifier","Tanh","RectifierWithDropout","TanhWithDropout"),
  epochs=c(10),
  hidden=list(16, 32, c(32,32)),
  input_dropout_ratio=c(0,0.05),
  rate=c(0.01,0.02),
  rate_annealing=c(1e-8,1e-6)
)

for (position in positions ) {
  print(position)
  set.seed(1)
  newCombine = combine[combine$Position==position,]
  if (position=='D') {
    variables = c(61,2,9,11:60,65:110)
    response = 7
    factors = c(2,9)
  }
  train.which = sample(length(newCombine$Key),floor(length(newCombine$Key)*.8))
  train = as.h2o(newCombine[train.which,])
  train = h2o.cbind(train,h2o.interaction(train,factors=factors,pairwise=TRUE,max_factors = 3,min_occurrence = 2))
  test = as.h2o(newCombine[-train.which,])
  test = h2o.cbind(test,h2o.interaction(test,factors=factors,pairwise=TRUE,max_factors = 3,min_occurrence = 2))
  sink(file=paste0('./modelOutputs/',paste0("dl_grid",position),'_model_text','.txt'))
  grid <- h2o.grid(
    algorithm="deeplearning",
    grid_id=paste0("dl_grid",position), 
    training_frame=train,
    validation_frame=test, 
    x=variables,             ## Removed Position
    y=response,
    stopping_tolerance=1e-2,        ## stop when misclassification does not improve by >=1% for 2 scoring events
    stopping_rounds=3,
    score_validation_samples=10000, ## downsample validation set for faster scoring
    score_duty_cycle=0.025,         ## don't score more than 2.5% of the wall time
    adaptive_rate=F,                ## manually tuned learning rate
    momentum_start=0.5,             ## manually tuned momentum
    momentum_stable=0.9, 
    momentum_ramp=1e7, 
    l1=1e-5,
    l2=1e-5,
    nfolds=5,
    #activation=c("Rectifier"),
    max_w2=10,                      ## can help improve stability for Rectifier
    hyper_params=hyper_params
    ,use_all_factor_levels=TRUE
    ,variable_importances=TRUE
  )
  grid <- h2o.getGrid(paste0("dl_grid",position),sort_by="rmse",decreasing=FALSE)
  summary(grid)
  best_model <- h2o.getModel(grid@model_ids[[1]])
  print(best_model@allparameters)
  print(h2o.performance(best_model, valid=T))
  sink()
  write.csv(as.data.frame(h2o.varimp(best_model)),file=paste0('./modelOutputs/',paste0("dl_grid",position),'_variableImp','.csv'))
  h2o.saveModel(best_model, 
                path=paste0('./modelOutputs/',paste0("dl_grid",position),'_bestModelSave'), force=TRUE) 
}


trials = expand.grid(epo=c(25,50,100,250)
                     ,hid=list(10, 25,100, c(10,10), c(25,25), c(10,10,10), c(25,25,25), c(100,100,100))
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

