e3
everest=as.data.frame(e3)
everest

#--Define some fresh variables--#

N.Teams=rep(0,nrow(everest))
for(i in 1:nrow(everest))
{
  N.Teams[i]=nrow(subset(everest,year==everest$year[i] & season==everest$season[i]))
}
N.Teams

Diversity=rep(0,nrow(everest))
for(i in 1:nrow(everest))
{
  if(is.na(unlist(strsplit(everest$countries[i], split=",")))==TRUE)
  {
    Diversity[i]=1
  }
  else
  {
    Diversity[i]=length(unlist(strsplit(everest$countries[i], split=",")))+1
  }
  
}
Diversity

N.Routes=rep(0,nrow(everest))
for(i in 1:nrow(everest))
{
  N.Routes[i]=length(which(c(is.na(everest$route1[i]),is.na(everest$route2[i]),is.na(everest$route3[i]),is.na(everest$route4[i]))==FALSE))
}
N.Routes

Ov.Success=rep(0,nrow(everest))
for(i in 1:nrow(everest))
{
  if(length(which(c(isFALSE(everest$success1[i]),isFALSE(everest$success2[i]),isFALSE(everest$success3[i]),isFALSE(everest$success4[i]))==FALSE))==0)
  {
    Ov.Success[i]="FALSE"
  }
  else{
    Ov.Success[i]="TRUE"
  }
}

Ov.Success

#---Enlarging the dataset---#
En.Everest=data.frame(everest$season,everest$smtdays,everest$totdays,everest$camps,everest$rope,everest$totmembers,everest$tothired,as.factor(everest$route1),as.factor(everest$o2climb),as.factor(everest$o2sleep),as.factor(everest$o2medical),N.Teams,Diversity,N.Routes,Ov.Success,as.numeric(everest$year)-1920)
En.Everest
colnames(En.Everest)<-c("Season","S.Days","T.Days","Camps","Rope","T.Mems","T.Hired","Route","O2Climb","O2Sleep","O2Medical","N.Teams","Diversity","N.Routes","Ov.Success","YearIndex")
En.Everest


Quantitatives=data.frame(everest$season,everest$smtdays,everest$totdays,everest$camps,everest$rope,everest$totmembers,everest$tothired,N.Teams,Diversity,N.Routes,as.numeric(everest$year)-1920)
corr = round(cor(Quantitatives), 2)
ggcorrplot(corr,title="Correlation heatmap among economic stress variables")
##################

#--A simple logistic---#

Succ.bin=ifelse(En.Everest$Ov.Success=="TRUE",1,0)
Succ.bin
#--good choices: S.Days,#
logistic.predictor=En.Everest$S.Days
reg.logistic=glm(Succ.bin~logistic.predictor,family=binomial(link='logit'))
summary(reg.logistic)
anova(reg.logistic)

logistic.predictor1=En.Everest$S.Days
logistic.predictor2=En.Everest$N.Teams
reg.logistic=glm(Succ.bin~logistic.predictor1+logistic.predictor2,family=binomial(link='logit'))
summary(reg.logistic)
anova(reg.logistic)

#---Some small-scale croos-validation---#
small.predictors=data.frame(En.Everest$S.Days,En.Everest$N.Teams)
small.response.df=data.frame(as.factor(En.Everest$Ov.Success))
tr=createDataPartition(En.Everest$Ov.Success,p=3/4,list = F) #---in a regression setting, we are preserving quartiles--#
#---in a classification setting, we are preserving proportions--# 

tr.pred=small.predictors[tr,]
test.pred=small.predictors[-tr,]
tr.response=small.response.df[tr,]
test.response=small.response.df[-tr,]

#---Notice how "createDataPartition" maintains balance:--#
prop.table(table(En.Everest$Ov.Success)) #---such a high "success" rate?--#
prop.table(table(tr.response))
prop.table(table(test.response))

#---Some data transformation (scaling, centering, etc.), notice how the categoricals are untouched---#
trans=preProcess(tr.pred, method = c('knnImpute','center','scale'))
trans.tr.pred=predict(trans,tr.pred)
trans.test.pred=predict(trans,test.pred)

##############
#--Setting up my 10-fold, 5-run CV--#
ctrl=trainControl(method = "repeatedcv", number = 10, repeats=5, savePredictions = TRUE)

cv.logistic=train(x=trans.tr.pred,y=tr.response,method='glm',trControl=ctrl,family='binomial')
cv.logistic
system.time(train(x=trans.tr.pred,y=tr.response,method='glm',trControl=ctrl,family='binomial'))
pred=predict(cv.logistic,newdata = trans.test.pred)
pred
pred.prob=predict(cv.logistic,newdata = trans.test.pred, type = "prob")
pred.prob
confusionMatrix(data=pred,test.response,positive = "TRUE")
summary(cv.logistic$finalModel)
cv.logistic$results

cv.knn=train(x=trans.tr.pred,y=tr.response,method="knn",trControl=ctrl)
cv.knn
plot(cv.knn)
pred.knn=predict(cv.knn,newdata = trans.test.pred)
pred.knn
pred.prob.knn=predict(cv.knn,newdata = trans.test.pred, type = "prob")
pred.prob.knn
confusionMatrix(data=pred.knn,test.response,positive = "TRUE")
cv.knn$resample
cv.knn$results
system.time(train(x=trans.tr.pred,y=tr.response,method="knn",trControl=ctrl))

#--Simple graphics (library(ggforce))---#

logistic.func <- function(x, y){
  1/(1+exp(-0.55276-0.77438*x-0.79691*y))
}

SummitDays <- NumberOfTeams <- seq(-3, 3, length= 40)
SuccessProbability <- outer(SummitDays, NumberOfTeams, logistic.func)

plot1=persp(SummitDays, NumberOfTeams, SuccessProbability,main="The logistic surface",theta = 230, phi = 25, d=0.5,
            col = "springgreen", shade = 0.1)

graph.data=data.frame(En.Everest$S.Days,En.Everest$N.Teams,En.Everest$Ov.Success)
colnames(graph.data)<-c("SummitDays","NumberOfTeams","Success")
plot2=ggplot(graph.data, aes(x=SummitDays, y=NumberOfTeams, shape=Success, color=Success)) +
  geom_point()+
  geom_circle(aes(x0 = 73, y0 = 18, r = 6),
              inherit.aes = FALSE)+
  geom_circle(aes(x0 = 73, y0 = 18, r = 8),
              inherit.aes = FALSE,colour="red")+
  annotate("segment", x = 79, xend = 88, y = 18, yend = 18,
           colour = "black", size = 1, arrow = arrow())+
  annotate("segment", x = 81, xend = 88, y = 20, yend = 23,
           colour = "red", size = 1, arrow = arrow())+
  annotate(geom="text", x=92, y=18, label="k = 3",
           color="black")+
  annotate(geom="text", x=92, y=23, label="k = 5",
           color="red")+
  ggtitle("Understanding k-nearest neighbours")
plot2+theme(legend.position = c(0.8, 0.8))

#grid.arrange(plot2,plot2,ncol=2,nrow=1)

#--library(patchwork,gridgraphics)#
plot2+~persp(SummitDays, NumberOfTeams, SuccessProbability,main="The logistic surface",theta = 230, phi = 25, d=0.5,
             col = "springgreen", shade = 0.1)

plot2+inset_element(plot1,0.6,0.6,1,1)

#logistic.data=data.frame(SummitDays, NumberOfTeams, SuccessProbability)

x.t=c(1.3076,-0.846)
y.t=c(2.0769,-1)
z.t=c(0.9616,0.2892)

d.t=data.frame(x.t,y.t,z.t)

plot3=plot_ly() %>% 
  #  add_markers(x = ~mtcars$hp, y=mtcars$wt, z=mtcars$qsec) %>% 
  add_surface(x = ~SummitDays, y = ~NumberOfTeams, z = t(SuccessProbability))%>%
  add_trace(d.t, x = ~x.t, y = ~y.t, z = ~z.t, type='scatter3d', mode = 'lines+markers', name = 'Name of Trace 1')%>%
  layout(
    scene = list(
      xaxis = list(title = "Summit Days (scaled and centered)"),
      yaxis = list(title = "Number of teams (scaled and centered)"),
      zaxis = list(title = "Success probability")
    )
  )%>% 
  layout(title= list(text = "Success probability surface from a logistic fit"))%>%
  layout(
    scene = list(
      annotations = list(
        #list(
        # showarrow = T,
        #  x = -0.846,
        # y = -1,
        #z = 0.3092,
        #text = "Expedition 1 (estimated success probability = 0.29), likely failure",
        #xanchor = "left",
        #xshift = 10,
        #opacity = 0.7
        # ), 
        list(
          x = 1.3076,
          y = 2.0769,
          z = 0.9816,
          text = "Expedition 2 (estimated success probability = 0.96), likely success",
          textangle = 0,
          ax = 0,
          ay = -75,
          font = list(
            color = "black",
            size = 12
          ),
          arrowcolor = "black",
          arrowsize = 3,
          arrowwidth = 1,
          arrowhead = 1
        ),
        list(
          x = -0.846,
          y = -1,
          z = 0.3092,
          text = "Expedition 1 (estimated success probability = 0.29), likely failure",
          textangle = 0,
          ax = 0,
          ay = -75,
          font = list(
            color = "black",
            size = 12
          ),
          arrowcolor = "black",
          arrowsize = 3,
          arrowwidth = 1,
          arrowhead = 1
        )
      )
    )
  )

grid.arrange(plot3,plot2,ncol=2,nrow=1)

subplot(plot3,plot2,nrows = 1,widths = c(0.6, 0.4))


#--Some treenets---#


############################################################################################
#--libary(caret)--#

#,everest$route1#
predictors=data.frame(En.Everest$Season,En.Everest$S.Days,En.Everest$T.Days,En.Everest$Camps,En.Everest$Rope,En.Everest$T.Mems,En.Everest$T.Hired,as.factor(En.Everest$Route),as.factor(En.Everest$O2Climb),as.factor(En.Everest$O2Sleep),as.factor(En.Everest$O2Medical),En.Everest$N.Teams,En.Everest$Diversity,En.Everest$N.Routes,En.Everest$YearIndex)
#predictors=data.frame(everest$season,everest$smtdays,everest$totdays,everest$camps,everest$rope,everest$totmembers,everest$tothired,as.factor(everest$o2climb),as.factor(everest$o2sleep),as.factor(everest$o2medical))
#response.df=data.frame(as.factor(everest$success1))
response.df=data.frame(as.factor(En.Everest$Ov.Success))
##########
tr=createDataPartition(En.Everest$Ov.Success,p=3/4,list = F) #---in a regression setting, we are preserving quartiles--#
#---in a classification setting, we are preserving proportions--# 

tr.pred=predictors[tr,]
test.pred=predictors[-tr,]
tr.response=response.df[tr,]
test.response=response.df[-tr,]
#r.hall=predictors[389,]
s.fisher.pr=predictors[387,]
my.expedition=data.frame(En.Everest.Season=1,En.Everest.S.Days=65,
                         En.Everest.T.Days=70, En.Everest.Camps=5,
                         En.Everest.Rope=0, En.Everest.T.Mems=15,
                         En.Everest.T.Hired=0, En.Everest.Route.=as.factor("S Col-SE Ridge"),
                         En.Everest.O2Climb.="FALSE",En.Everest.O2Sleep.="FALSE",En.Everest.O2Medical.="FALSE",
                         Everest.N.Teams=32, En.Everest.Diversity=4, En.Everest.N.Routes=2,
                         En.Everest.YearIndex=103)
my.ex2=data.frame(1,65,70,5,0,15,0,"S Col-SE Ridge","FALSE","FALSE","FALSE",32,4,2,103)
testPoint1=data.frame(Season=1,S.Days=65,T.Days=70,Camps=0,Rope=0,T.Mems=15,T.Hired=0,Route="S Col-SE Ridge",O2Climb="FALSE",O2Sleep="FALSE",O2Medical="FALSE",N.Teams=32,Diversity=4,N.Routes=2,YearIndex=103)

predictors.fresh=rbind(predictors,c(1,65,70,5,0,15,0,"S Col-SE Ridge","FALSE","FALSE","FALSE",32,4,2,103))

predictors.fresh=rbind(tr.pred,c(1,65,70,5,0,15,0,"S Col-SE Ridge","FALSE","FALSE","FALSE",32,4,2,103))
my.final.exp=predictors.fresh[nrow(predictors.fresh),]

#---Notice how "createDataPartition" maintains balance:--#
prop.table(table(En.Everest$Ov.Success)) #---such a high "success" rate?--#
prop.table(table(tr.response))
prop.table(table(test.response))

#---Some data transformation (scaling, centering, etc.), notice how the categoricals are untouched---#
trans=preProcess(tr.pred, method = c('knnImpute','center','scale'))
trans.tr.pred=predict(trans,tr.pred)
trans.test.pred=predict(trans,test.pred)
trans.s.fisher.pred=predict(trans,s.fisher.pr)

trans2=preProcess(my.ex2, method = c('knnImpute','center','scale'))
trans.my.expedition.pred=predict(trans,my.ex2)
trans.TestPoint1=predict(trans,testPoint1)

trans21=preProcess(predictors.fresh, method = c('knnImpute','center','scale'))
trans.my.final.exp=predict(trans21,my.final.exp)
##############
#--Setting up my 10-fold, 5-run CV--#
ctrl=trainControl(method = "repeatedcv", number = 10, repeats=5, savePredictions = TRUE)

###################
#--Next, a bagged tree---#
###################

cv.bagged.tree=train(x=trans.tr.pred,y=tr.response,method='treebag',trControl=ctrl)
cv.bagged.tree
pred.bagged.tree=predict(cv.bagged.tree,newdata = trans.test.pred)
pred.bagged.tree #---These would be your "hard" decisions---#
cv.bagged.tree$results #--So we have the volatilities too--#
#--How's our confusion matrix on the test set?---#
confusionMatrix(data=pred.bagged.tree,test.response,positive = "TRUE")
#--How long does it take? Longer than one tree. Expected---#
system.time(train(x=trans.tr.pred,y=tr.response,method='treebag',trControl=ctrl))
#--But now, its not one tree anymore, how to still retain some interpretability?--#
#--Through these variable importance scores--#
varImp(cv.bagged.tree)
plot(varImp(cv.bagged.tree))

pred.bagged.tree.s.fisher=predict(cv.bagged.tree,newdata = trans.s.fisher.pred,type = "prob")
pred.bagged.tree.s.fisher

pred.bagged.tree.my.ex=predict(cv.bagged.tree,newdata = trans.my.final.exp,type = "prob")
pred.bagged.tree.my.ex

extractProb(list(cv.bagged.tree),testX = c(1,65,70,5,0,15,0,"S Col-SE Ridge","FALSE","FALSE","FALSE",32,4,2,103))
###################
#--How about a random forest?---#
###################

cv.randomforest=train(x=trans.tr.pred,y=tr.response,method='cforest',trControl=ctrl)
cv.randomforest
plot(cv.randomforest)
pred.randomforest=predict(cv.randomforest,newdata = trans.test.pred)
pred.randomforest
cv.randomforest$results
confusionMatrix(data=pred.randomforest,test.response,positive = "TRUE")
system.time(train(x=trans.tr.pred,y=tr.response,method='cforest',trControl=ctrl))
varImp(cv.randomforest)
plot(varImp(cv.randomforest))

pred.bagged.tree.s.fisher=predict(cv.randomforest,newdata = trans.s.fisher.pred,type = "prob")
pred.bagged.tree.s.fisher

pred.bagged.tree.my.ex=predict(cv.randomforest,newdata = trans.my.final.exp,type = "prob")
pred.bagged.tree.my.ex
###################
#--Moving on to boosting---#
###################

cv.boosted=train(x=trans.tr.pred,y=tr.response,method='gbm',trControl=ctrl) #--The method changes to 'gbm'#
cv.boosted
plot(cv.boosted)
pred.boost=predict(cv.boosted,newdata = trans.test.pred)
pred.boost
cv.boosted$results
confusionMatrix(data=pred.boost,test.response,positive = "TRUE")
system.time(train(x=trans.tr.pred,y=tr.response,method='gbm',trControl=ctrl))
varImp(cv.boosted)
plot(varImp(cv.boosted))

pred.bagged.tree.s.fisher=predict(cv.boosted,newdata = trans.s.fisher.pred,type = "prob")
pred.bagged.tree.s.fisher

pred.bagged.tree.my.ex=predict(cv.boosted,newdata = trans.my.final.exp,type = "prob")
pred.bagged.tree.my.ex

###########
############
#--Let's "open up" our best boosted model, library(gbm)--#

names=c("Season","S.Days","T.Days","Camps","Rope","T.Mems","T.Hired","Route","O2Climb","O2Sleep","O2Medical","N.Teams","Diversity","N.Routes","Year.Index")
boost.data=cbind(trans.tr.pred,ifelse(tr.response=="TRUE",1,0))
colnames(boost.data)<-c(names,"Success")
boost.data

#150,3#
best.boost=gbm(Success~.,data=boost.data,distribution = "bernoulli",n.trees = 150,interaction.depth = 4,n.minobsinnode = 10,shrinkage = 0.1,cv.folds = 10)
gbm.perf(best.boost,plot.it = TRUE)

#--How about some isolated inferences?--#
#--i.e., measuring the impact on log-odds when we vary only one feature, holding the others fixed--#

plot(best.boost,i.var=1,main="Season")
plot(best.boost,i.var=2,main="S.Days")
plot(best.boost,i.var=3,main="T.Days")
plot(best.boost,i.var=4,main="Camps")
plot(best.boost,i.var=5,main="Rope")
plot(best.boost,i.var=6,main="T.Members")
plot(best.boost,i.var=7,main="T.Hired")
plot(best.boost,i.var=8,main="Route")
plot(best.boost,i.var=12,main="N.Teams")
plot(best.boost,i.var=13,main="Diversity")
plot(best.boost,i.var=14,main="N.Routes")
plot(best.boost,i.var=15,main="Year.Index")

plot(best.boost,i.var = c(3,12),contour = TRUE)
plot(best.boost,i.var = c(3,12),level.plot = FALSE)
plot(best.boost,i.var = c(2,15),contour = TRUE)
plot(best.boost,i.var = c(2,3),contour = TRUE) #--interesting: take time to climb the peak, but not on the full expedition--#

plot(best.boost,i.var = c(4,12),contour = TRUE)
plot(best.boost,i.var = c(12,15),contour = TRUE)
plot(best.boost,i.var = c(5,15),contour = TRUE)
plot(best.boost,i.var = c(12,13),contour = TRUE)
plot(best.boost,i.var = c(12,8),contour = TRUE) #--interesting arrangement---#
plot(best.boost,i.var = c(12,8),contour = TRUE)
plot(best.boost,i.var = c(12,9),contour = TRUE) #--use that---#

plot(best.boost,i.var=2,main="S.Days")
lines(best.boost,i.var=12,main="N.Teams")
pl.SDAYS=plot(best.boost,i.var=2,return.grid = TRUE,main="S.Days")
pl.T.DAYS=plot(best.boost,i.var=3,return.grid = TRUE,main="T.Days")
pl.T.Mem=plot(best.boost,i.var=6,return.grid = TRUE,main="T.Mem")
pl.NTEAMS=plot(best.boost,i.var=12,return.grid = TRUE,main="N.TEams")
pl.YIND=plot(best.boost,i.var=15,return.grid = TRUE,main="YInd")
str(pl.SDAYS)

xax=c(pl.SDAYS$S.Days,pl.T.DAYS$T.Days,pl.T.Mem$T.Mems,pl.NTEAMS$N.Teams,pl.YIND$Year.Index)
yax=c(pl.SDAYS$y,pl.T.DAYS$y,pl.T.Mem$y,pl.NTEAMS$y,pl.YIND$y)

#xlim = c(min(xax),max(xax)),ylim = c(min(yax),max(yax))
#par(mfrow=c(2,2))

#layout(mat = matrix(c(1,2,3,4), 
#                   nrow = 2, 
#                  ncol = 2),
#    heights = c(1, 2),    # Heights of the two rows
#   widths = c(2, 1))
par(mfrow=c(1,1))
plot(pl.SDAYS$S.Days,pl.SDAYS$y,"l",lwd=2,xlim = c(-2.8,2.2),ylim = c(-1,max(yax)),xlab = "Predictors (scaled and centered)",ylab="Estimated log-odds",main="Partial dependence plots (one variable at a time)")
lines(pl.T.DAYS$T.Days,pl.T.DAYS$y,col="green",lwd=2)
lines(pl.T.Mem$T.Mems,pl.T.Mem$y,col="blue",lwd=2)
lines(pl.NTEAMS$N.Teams,pl.NTEAMS$y,col="red",lwd=2)
lines(pl.YIND$Year.Index,pl.YIND$y,col="cyan",lwd=2)
text(-2.5,0.5,"Summit Days",col = "black")
text(-2.5,0.45,"Total Days",col = "green")
text(-2.5,0.40,"Total Members",col = "blue")
text(-2.5,0.35,"Number of teams",col = "red")
text(-2.5,0.3,"Year index",col = "cyan")
arrows(x0=1.1, y0=-0.7, x1 = 1.6, y1 = -0.4, length = 0.25, angle = 30,
       code = 2, col = "green", lty = 1,
       lwd = 2)
text(1.0,-0.73,"Everest can be a brutal place",col="green")
arrows(x0=-1, y0=0.8, x1 = -0.7, y1 = 0.75, length = 0.25, angle = 30,
       code = 2, col = "red", lty = 1,
       lwd = 2)
text(-1.8,0.82,"Decay beyond a threshold. Effect of crowding?",col="red")
arrows(x0=0, y0=-0.3, x1 = -0.31, y1 = -0.1, length = 0.25, angle = 30,
       code = 2, col = "cyan", lty = 1,
       lwd = 2)
text(0.45,-0.35,"Evidence of skill-evolution?",col="cyan")
arrows(x0=1.5, y0=0.17, x1 = 1.31, y1 = 0.3, length = 0.25, angle = 30,
       code = 2, col = "black", lty = 1,
       lwd = 2)
text(1.8,0.14,"Taking it slow helps",col="black")

#recordedplot <- recordPlot() 

pm.1=plot(best.boost,i.var=8,main="Route")
pm.2=plot(best.boost,i.var = c(2,15),contour = TRUE)
pm.3=plot(best.boost,i.var = c(2,3),contour = TRUE)

grid.arrange(plot(best.boost,i.var=8,main="Route"),plot(best.boost,i.var = c(12,9),contour = TRUE),
             plot(best.boost,i.var = c(2,3),contour = TRUE),
             ncol=2,nrow=2)




length(unique(En.Everest$Route))
as.data.frame(table(En.Everest$Route))
as.data.frame(table(En.Everest$O2Climb))
pl.ROUTE=plot(best.boost,i.var=8,return.grid = TRUE,main="Routes")
#######

#########
par(xpd = NA, # switch off clipping, necessary to always see axis labels
    bg = "transparent", # switch off background to avoid obscuring adjacent plots
    oma = c(2, 2, 0, 0), # move plot to the right and up
    mgp = c(2, 1, 0) # move axis labels closer to axis
) 

left_col <- plot_grid(pm.1,pm.2, labels = c('A', 'B'), ncol = 1)
right_col <- plot_grid(recordedplot,pm.3, labels = c('c', 'D'), ncol = 1)
plot_grid(left_col,recordedplot,pm.3, ncol = 2)




pm.1+~plot(pl.SDAYS$S.Days,pl.SDAYS$y,"l",lwd=2,xlim = c(-2.8,2.2),ylim = c(-1,max(yax)),xlab = "Predictors (scaled and centered)",ylab="Estimated log-odds",main="Partial dependence plots (one variable at a time)")
lines(pl.T.DAYS$T.Days,pl.T.DAYS$y,col="green",lwd=2)
lines(pl.T.Mem$T.Mems,pl.T.Mem$y,col="blue",lwd=2)
lines(pl.NTEAMS$N.Teams,pl.NTEAMS$y,col="red",lwd=2)
lines(pl.YIND$Year.Index,pl.YIND$y,col="cyan",lwd=2)
text(-2.5,-0.5,"Summit Days",col = "black")
text(-2.5,-0.55,"Total Days",col = "green")
text(-2.5,-0.60,"Total Members",col = "blue")
text(-2.5,-0.65,"Number of teams",col = "red")
text(-2.5,-0.7,"Year index",col = "cyan")
arrows(x0=1.1, y0=-0.7, x1 = 1.6, y1 = -0.6, length = 0.25, angle = 30,
       code = 2, col = "green", lty = 1,
       lwd = 2)
text(1.0,-0.73,"Everest can be a brutal place",col="green")
arrows(x0=-1, y0=0.8, x1 = -0.7, y1 = 0.7, length = 0.25, angle = 30,
       code = 2, col = "red", lty = 1,
       lwd = 2)
text(-2,0.8,"Decay beyond a threshold. Effect of crowding?",col="red")
arrows(x0=0, y0=-0.3, x1 = -0.26, y1 = -0.1, length = 0.25, angle = 30,
       code = 2, col = "cyan", lty = 1,
       lwd = 2)
text(0.5,-0.33,"Evidence of skill-evolution?",col="cyan")
arrows(x0=1.5, y0=0.25, x1 = 1.27, y1 = 0.3, length = 0.25, angle = 30,
       code = 2, col = "black", lty = 1,
       lwd = 2)
text(1.8,0.22,"Taking it slow helps",col="black")


plot2+~persp(SummitDays, NumberOfTeams, SuccessProbability,main="The logistic surface",theta = 230, phi = 25, d=0.5,
             col = "springgreen", shade = 0.1)