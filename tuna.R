tuna <- read_excel("C:\\Users\\Jay\\Documents\\Data\\tuna.xlsx", 
                   +     col_types = c("text", "numeric", "numeric", 
                                       +         "text", "numeric", "numeric", "numeric", 
                                       +         "numeric", "text", "numeric"))
tuna$tide = as.factor(tuna$tide)
tuna$species = as.factor(tuna$species)
tree.yellowfin=tree(species~.,tuna)
#classification tree
tree.yellowfin=tree(species~.,yellowfin)
summary(tree.yellowfin)
plot(tree.yellowfin)
text(tree.yellowfin, pretty=0)
#training set
attach(tuna)
set.seed(2)
train=sample(1:nrow(tuna), 200)
tuna.test=tuna[-train,]
tree.yellowfin=tree(species~.,tuna,subset=train)
tree.pred=predict(tree.yellowfin, tuna.test, type="class")
species.test=species[-train]
table(tree.pred, species.test)
#best number of nodes
cv.tuna=cv.tree(tree.yellowfin, FUN=prune.misclass)
names(cv.tuna)
cv.tuna
#plotting deviation against number of nodes
par(mfrow=c(1,2))
plot(cv.tuna$size,cv.tuna$dev,type="b")
plot(cv.tuna$k, cv.tuna$dev, type="b")
# 2 nodes best
prune.tuna=prune.misclass(tree.yellowfin, best=2)
plot(prune.tuna)
text(prune.tuna, pretty=0)
#predict error
table(tree.pred, species.test)

