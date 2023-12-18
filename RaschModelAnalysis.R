#Source examples: https://bookdown.org/dkatz/Rasch_Biome/
#Analysis done after: https://journals.sagepub.com/doi/full/10.1177/0265532220927487#bibr20-0265532220927487
library(TAM)
library(WrightMap)
library(psych)
library(ggplot2)
library("dplyr")


#Load the whole data of the first runs.
runData <- #insert Data here
  #needs form of a person item Matrix

#remove all 0 and 1 columns
#runData <- subset(runData, select = #some environment)


#Dimensions of the data
dim(runData)

#View data
View(runData)

#calculated Rasch Model
#Marginal Maximum Likelihood method
model <- tam(runData)
summary(model)

#item Difficulties
#-------------------------------------------------------------------------------
estimated_item_difficulties <- model$xsi
summary(estimated_item_difficulties)

sd(estimated_item_difficulties$xsi)
sd(estimated_item_difficulties$se.xsi)

#ordered from easiest to hardest item
print(estimated_item_difficulties[order(estimated_item_difficulties$xsi),])
#-------------------------------------------------------------------------------


#person abilities
#-------------------------------------------------------------------------------
abilities <- tam.wle(model)

PersonAbility <- abilities$theta

personHist(PersonAbility)
#-------------------------------------------------------------------------------



#ICCs
#-------------------------------------------------------------------------------
#plotting each theoretical ICC(blue) together with the empirical ICC(black with points)
plot(model, overlay=FALSE)
#-------------------------------------------------------------------------------


#PCAR - PCA analysis of rasch residuals
#-------------------------------------------------------------------------------
#residuals
resids <- TAM::IRT.residuals(model)

pca <- pca(resids$stand_residuals, nfactors=ncol(runData), rotate="none")

contrasts <- c(pca$values[1], pca$values[2], pca$values[3], pca$values[4], pca$values[5])
plot(contrasts, ylab = "Eigenvalues for Contrasts", xlab = "Contrast Number", main="Contrasts from PCA of Standardized Residuel Correlations")
#-------------------------------------------------------------------------------

#sufficient statistics?
#-------------------------------------------------------------------------------

#plot of standard error and their estimated item difficulties
ggplot(estimated_item_difficulties, aes(x = xsi, y=se.xsi)) + geom_point() +
  ggtitle("Item difficulties and their standard error") +
  xlab("Estimated Item Difficulties") +
  ylab("Estimated Item Standard Errors")

#mean and standard deviation of item difficulties
mean(estimated_item_difficulties$xsi)
sd(estimated_item_difficulties$xsi)
#-------------------------------------------------------------------------------


#Fit analysis, Outfit, Infit
#-------------------------------------------------------------------------------
fit <- tam.fit(model)

data.fit <- as.data.frame(fit$itemfit)
View(fit$itemfit)
summary(data.fit)

#plotting fit statistics
#itemfit -------------------------------------------------------------------------------------------------------------
#infit msq:
infit <- fit$itemfit$Infit

upper_bound <- rep(x = 1.33, times =42) # this repeats 1.33 fifteen times
lower_bound <- rep(x = .75, times = 42) 
head(runData)

fitgraph(fitEst = infit, fitLB = lower_bound, fitUB = upper_bound, itemLabels = names(runData))


#outfit msq:
outfit <- fit$itemfit$Outfit  
fitgraph(fitEst = outfit, fitLB = lower_bound, fitUB = upper_bound, fit.type = "U", itemLabels = names(runData))


#infit standardized
upper_bound <- rep(x = 1.96, times =42)
lower_bound <- rep(x = -1.96, times = 42) 

fit$itemfit %>%
  ggplot(aes(x=parameter, y = infit_t)) + 
  geom_point() + 
  geom_label(label=names(runData), label.size = 0.1) +
  geom_hline(yintercept = 1.96) +
  geom_hline(yintercept = -1.96) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 42)) + 
  ggtitle("Item Fit Statistics for standardized infit")


#outfit standardized
outfit_t<- fit$itemfit$Outfit_t
fit$itemfit %>%
  ggplot(aes(x=parameter, y = outfit_t)) + 
  geom_point() + 
  geom_label(label=names(runData), label.size = 0.1) +
  geom_hline(yintercept = 1.96) +
  geom_hline(yintercept = -1.96) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 42)) + 
  ggtitle("Item Fit Statistics for standardized outfit")

#---------------------------------------------------------------------------------------------------------------------
#personfit
#---------------------------------------------------------------------------------------------------------------------
person.fit <- tam.personfit(model)


#---------------------------------------------------------------------------------------------------------------------

#Q3 correlation 
#-------------------------------------------------------------------------------
res1 <- TAM::tam.modelfit(model)
summary(res1)
q3matrix <- as.data.frame(res1$Q3.matr)
View(q3matrix)
#-------------------------------------------------------------------------------


#reliability, person separation reliability
#-------------------------------------------------------------------------------
#person
abil <- tam.wle(model)
abil$WLE.rel

pmodel <- IRT.factor.scores(model, type="WLE")
#person reliability
personreliabilty <- WLErel(pmodel$theta, pmodel$error)

#person separation
person_separation <- sqrt(personreliabilty/(1-personreliabilty))

#item reliability coefficient
estimated_item_difficulties <- model$xsi
variance_of_item_diffs <- var(estimated_item_difficulties$xsi)

squared_standard_errors_of_items <- estimated_item_difficulties$se.xsi^2
mean_of_item_diffs_squared_error <- mean(squared_standard_errors_of_items)

sA <- variance_of_item_diffs - mean_of_item_diffs_squared_error

sE <- sqrt(mean_of_item_diffs_squared_error)

#item separation reliability
item_separation_index <- sA / sE

#item separation
item_separation <- sqrt(item_separation_index/(1-item_separation_index))
#-------------------------------------------------------------------------------



#item probabilities
#-------------------------------------------------------------------------------
item_prop <- model$item
item_prop

#plot for correlation between endorsement and item difficulty

item_prop <- mutate(item_prop, total_endorsed =N*M)
#correlation
cor(item_prop$xsi.item, item_prop$total_endorsed)

ggplot(item_prop, aes(x=total_endorsed, y=xsi.item)) + 
  geom_point() +
  ylab("Estimated Item Difficulties (logits)") +
  xlab("Total Number of Endorsements for an item") +
  ggtitle("Relationship between estimated item difficulty and total endorsements")
#-------------------------------------------------------------------------------


#Wright Map
#-------------------------------------------------------------------------------
wrightMap(PersonAbility)
#TAM Package Wright Map
IRT.WrightMap(model, show.thr.lab=FALSE)

#creating labels
#item_names_NEW <- c("I1", ...)

#more parameter settings for wright map
#thresh <- TAM::IRT.threshold(model)
#wrightMap(PersonAbility, thresh, show.thr.sym = FALSE,thr.lab.cex = 0.6,  thr.lab.text = item_names_NEW, label.items.srt = 45)
#IRT.WrightMap(model, show.thr.lab=FALSE, label.items=names(runData), label.items.srt = 45)


#test information curve plotted with standard error
#-------------------------------------------------------------------------------
#item information curves/ test information curves
inf_data <- IRT.informationCurves(model)


plot(inf_data, xlab = "Theta", 
     ylab = "Test information", 
     main = "Test Information Curve")

#plot together
#par(new=TRUE)
plot(inf_data, 
     curve_type = "se", 
     xlab = "Theta", 
     ylab = "Standard error", 
     main = "Test Standard Error Curve")
#-------------------------------------------------------------------------------
