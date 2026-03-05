#+###########################################
#### Pairwise comparisons analysis 2025 ####
####           Data analysis            ####
###########################################


rm(list=ls())
setwd("~/Desktop/Pairwise comparisons_study")

# Packages ####
library(lme4)
library(lattice)
library(lmerTest)
library(DHARMa)
library(performance)


# Loading datasets ####
analysis_total      <- read.csv("Data/analysis_total.csv")



# AUT analysis ----
### Final Rasch Model only ----
# Data from our collection
aut_RM_1 <- glmer(r~0+(V1+V2+V3+V4+V5+V6+V7)+(1|id), data=analysis_total[analysis_total$id_type==1 & analysis_total$collection == 1 & analysis_total$dataset_id == "AUT",], family = binomial)
summary(aut_RM_1)

# Outsourced data collection
aut_RM_2 <- glmer(r~0+(V1+V2+V3+V4+V5+V6+V7)+(1|id), data=analysis_total[analysis_total$id_type == 1 & analysis_total$dataset_id == "AUT" & analysis_total$collection == 2,], family = binomial)
summary(aut_RM_2)

# combined data collection
summary(aut_RM_3 <- glmer(r~0+(V1+V2+V3+V4+V5+V6+V7)+(1|id), data=analysis_total[analysis_total$id_type == 1 & analysis_total$dataset_id == "AUT",], family = binomial))
summary(aut_RM_4 <- glmer(r~0+(V1+V2+V3+V4+V5+V6+V7)+(1|id)+(1|collection), data=analysis_total[analysis_total$id_type == 1 & analysis_total$dataset_id == "AUT",], family = binomial))
dotplot(ranef(aut_RM_4))$collection # no difference between collections

# checking relation of the two item collections
cor.test(as.numeric(coef(aut_RM_2)$id[1,-1]), as.numeric(coef(aut_RM_1)$id[1,-1]))

# png("Plots/AUT/aut_item_difficulties_collections.png", width = 3500, height = 2200, res = 230)
plot(as.numeric(coef(aut_RM_2)$id[1,-1]), 
     as.numeric(coef(aut_RM_1)$id[1,-1]), 
     xlab = "Rasch model item difficulties of collection 2", ylab = "Rasch model item difficulties of collection 1",
     main = "Item difficulties across collections")

x <- lm(as.numeric(coef(aut_RM_1)$id[1,-1])~ 
     as.numeric(coef(aut_RM_2)$id[1,-1]))
abline(x)
# dev.off()


### Final BT model only ----
summary(aut_BT_1 <- glm(r~0 + (V2+V3+V4+V5+V6+V7), data=analysis_total[analysis_total$id_type==0 & analysis_total$dataset_id == "AUT",], family = binomial))

(1-mean(diag(vcov(aut_BT_1)))/var(coef(aut_BT_1)))

var(coef(aut_BT_1))/(var(coef(aut_BT_1))+mean(diag(vcov(aut_BT_1))))
var(fixef(aut_GLOB_1))/(var(fixef(aut_GLOB_1))+mean(diag(vcov(aut_GLOB_1))))

(var(coef(aut_BT_1))-mean(diag(vcov(aut_BT_1))))/var(coef(aut_BT_1))


qqnorm(statmod::qres.binom(aut_BT_1),main="Quantile residual normal plot",col=4,adj=0.1)
qqline(statmod::qres.binom(aut_BT_1))

c(Df = df.residual( aut_BT_1 ),
  Resid.Dev = deviance( aut_BT_1 ),
  Pearson.X2 = sum( resid(aut_BT_1, type="pearson")^2 )) # it could suggest underdispersion, but it has no meaning... binary data...


plot( factor(r, labels=c("No","Yes")) ~ factor(V1)+factor(V2)+factor(V3)+factor(V4)+factor(V5)+factor(V6)+factor(V7), las=1,
  ylab="Noisy miners present?", xlab="Eucalypts > 15", data=analysis_total[analysis_total$id_type==0 & analysis_total$dataset_id == "AUT",])

plot( r ~ V2, pch=ifelse(V1, 1, 19), data=analysis_total[analysis_total$id_type==0 & analysis_total$dataset_id == "AUT",], las=1)
abline(v=15.5, col="gray")


MuMIn::r.squaredGLMM(aut_BT_1)

#                R2m       R2c
# theoretical 0.8984962 0.8984962
# delta       0.8789978 0.8789978

fmsb::NagelkerkeR2(aut_BT_1) # 0.8402757

### Global model ----

aut_GLOB_1 <- glmer(r ~ 0 +  (V2+V3+V4+V5+V6+V7) + (0+id_type|id) + (0+item_type|item), 
                    data = analysis_total[analysis_total$dataset_id == "AUT",], 
                    family = binomial)

aut_GLOB_1s <- glmer(r ~ 0 + (V2+V3+V4+V5+V6+V7) + (0+item_type|item), 
                     data = analysis_total[analysis_total$dataset_id == "AUT",], 
                     family = binomial) #simpler model

aut_GLOB_1t <- glmer(r ~ 0 + (V2+V3+V4+V5+V6+V7) + (0+id_type|id), 
                     data = analysis_total[analysis_total$dataset_id == "AUT",], 
                     family = binomial) #simpler model

aut_GLOB_1e <- glm(r ~ 0 + (V2+V3+V4+V5+V6+V7), 
                     data = analysis_total[analysis_total$dataset_id == "AUT",], 
                     family = binomial) #simplest model



summary(aut_GLOB_1)

# checking signif# checking signif# checking significance
plogis(fixef(aut_GLOB_1))
car::Anova(aut_GLOB_1, type=3) 
car::Anova(aut_RM_1, type=3)
car::Anova(aut_RM_3, type=3)
car::Anova(aut_BT_1, type=3)

# profiling

pr_aut <- profile(aut_GLOB_1)
splom(pr_aut) # problem with V4 estimation # makes sense as it is item with the least construct in BTM and one of the most difficult in RM

# result is that the we can be somehow confident about confidence intervals of item parameters

# testing models
simp_mod_anova_aut <- anova(aut_GLOB_1, aut_GLOB_1s, aut_GLOB_1t, aut_GLOB_1e) # in favor of aut_GLOB_1


qqmath(ranef(aut_GLOB_1)) # there is a difference in people (they have different thetas) and also in items (different difficulties)
qqnorm(resid(aut_GLOB_1),main="Residual normal plot",col=4,adj=0.1)
qqline(resid(aut_GLOB_1)) # heavy tails

# model on our data collection
aut_GLOB_2 <- glmer(r ~ 0 + (V2+V3+V4+V5+V6+V7) + (0+id_type|id) + (0+item_type|item), 
                    data = analysis_total[analysis_total$dataset_id == "AUT" & analysis_total$collection == 1,], family = binomial) # only our data collection

qqmath(ranef(aut_GLOB_2)) # there is a difference in people (they have different thetas) and also in items (different difficulties) although not so obvious
qqnorm(resid(aut_GLOB_2),main="Residual normal plot",col=4,adj=0.1)
qqline(resid(aut_GLOB_2)) # heavy tails

performance::check_model(aut_GLOB_1) # high collinearity for reverse var, and some r estimates outside of simulated range together with influential observations, prolly due to BTM model
performance::model_performance(aut_GLOB_1) # R2 conditional 0.854, marginal 0.854, RMSE 0.296


# Reliability of BTM

var(fixef(aut_GLOB_1))/(var(fixef(aut_GLOB_1))+mean(diag(vcov(aut_GLOB_1)))) # 0.9723264


# manually calculating contribution of fixed effects to variance explained - 0.143574

var(fixef(aut_GLOB_1)) / (
  as.data.frame(VarCorr(aut_GLOB_1))[2, "vcov"] + var(fixef(aut_GLOB_1))
)

# calculation of contribution to explained variance via presudo R2 for GLMM (Nakagawa & Schielzeth, 2013)

MuMIn::r.squaredGLMM(aut_GLOB_1) 
#                   R2m       R2c
# theoretical 0.2624535 0.9552787
# delta       0.2599365 0.9461175

MuMIn::r.squaredGLMM(aut_GLOB_2) 

#                   R2m       R2c
# theoretical 0.5687096 0.9347449
# delta       0.5607584 0.9216761

# why is marginal pseudo R2 so much bigger on our data only? maybe, bcs outsourced data are so much bigger?

AIC(aut_GLOB_1, aut_GLOB_1e) # pointless as we have different number of observations

# what about residuals? 
simres <- simulateResiduals(aut_GLOB_1)
plot(simres)
testOutliers(simres, type = "bootstrap") # good, no signs of outlier related misfit

testDispersion(simres)

pred <- predict(aut_GLOB_1, type = "response")
range(pred) # good separation


## compare random effect of ID across RM and GLOBAL -> persons are estimated to be the same
plot(jitter(ranef(aut_GLOB_1)$id[-1, 1]), 
     jitter(ranef(aut_RM_3)$id[,1]), 
     xlab = "global model", ylab = "rasch model", 
     main = "Comparison of estimated latent traits (thetas)")
x <- lm(ranef(aut_RM_3)$id[,1]~ranef(aut_GLOB_1)$id[-1, 1])
abline(x)
cor(ranef(aut_GLOB_1)$id[-1, 1], ranef(aut_RM_3)$id[,1])

# great, so both models predict person parameters similarly

dotplot(ranef(aut_GLOB_1))$id
dotplot(ranef(aut_RM_3))$id

###### Compare item difficulties across BT and GLOBAL ----
plot(c(0, fixef(aut_GLOB_1)), 
     c(0, coef(aut_BT_1)), col = c("red", rep("black", 6)), 
     xlab = "Global model item difficulties of BT part", ylab = "Bradley-Terry model",
     main = "Item difficulties")

x <- lm(c(0, coef(aut_BT_1))~c(0, fixef(aut_GLOB_1)))
abline(x)

cor(c(0, fixef(aut_GLOB_1)), c(0, coef(aut_BT_1)))
# there is some noise, but I think it´s normal


###### Compare item difficulties across RM and GLOBAL ----

# png("Plots/AUT/aut_plot_RM.png", width = 3500, height = 2200, res = 230)
plot(c(0, fixef(aut_GLOB_1)) + as.vector(coef(aut_GLOB_1)$item[,2]), 
     fixef(aut_RM_3), 
     xlab = "Global model item difficulties of Rasch part", ylab = "Rasch model",
     main = "Item difficulties")

x <- lm(as.numeric(fixef(aut_RM_3)) ~ c(0, fixef(aut_GLOB_1)) + as.vector(coef(aut_GLOB_1)$item[,2]))
abline(x)

# dev.off()
cor(c(0, fixef(aut_GLOB_1)) + as.vector(coef(aut_GLOB_1)$item[,2]), fixef(aut_RM_3))
# finally, Item estimates are similar across both models, meaning GLOBAL model works well in estimating (possible problem is only heightened noise)



### Compare item difficulties across RM and BT ----

rasch_b <- c(0, fixef(aut_GLOB_1))+as.vector(coef(aut_GLOB_1)$item[,2])
rasch_se <- as.vector(arm::se.ranef(aut_GLOB_1)$item)   # SEs from RM
bt_b <- c(0, fixef(aut_GLOB_1))
bt_se <- c(0,sqrt(diag(vcov(aut_GLOB_1)))) # SEs for BT (0 has SE=0)


## Item names
items <- paste0("V", seq_along(bt_b))

mod <- lm(bt_b ~ rasch_b)
newx <- seq(min(rasch_b), max(rasch_b), length.out = 100)
conf <- predict(mod, newdata = data.frame(rasch_b = newx),
                interval = "confidence", level = 0.95)
preds <- predict(mod, newdata = data.frame(rasch_b = newx),
                 interval = "prediction", level = 0.95)

## Plot
r <- cor(rasch_b, bt_b)
# png("Plots/AUT/aut_plot_comparison.png", width = 3500, height = 2200, res = 230)


plot(rasch_b, bt_b,
     xlab = "RM part item easiness",
     ylab = "BTM part item parameters",
     main = paste0("Item difficulties for AUT (Global model)", "\n r = ", round(r, 3)),
     pch = 19)+
  arrows(rasch_b - 1.96*rasch_se, bt_b,
         rasch_b + 1.96*rasch_se, bt_b,
         angle = 90, code = 3, length = 0.05, col = "darkgray")+
  arrows(rasch_b, bt_b - 1.96*bt_se,
         rasch_b, bt_b + 1.96*bt_se,
         angle = 90, code = 3, length = 0.05, col = "darkgray")+
  text(rasch_b+0.01, bt_b, labels = items, pos = 2, cex = 0.7)+
  lines(newx, conf[, "fit"], col = "black", lwd = 2)+
  # lines(newx, preds[, "lwr"], col = "red", lty = 2)+
  # lines(newx, preds[, "upr"], col = "red", lty = 2)
  polygon(c(newx, rev(newx)),
          c(preds[, "lwr"], rev(preds[, "upr"])),
          col = rgb(0,0,1,0.05), border = NA)+
  polygon(c(newx, rev(newx)),
          c(conf[, "lwr"], rev(conf[, "upr"])),
          col = rgb(1,0,0,0.1), border = NA)

aut_comparisons <- recordPlot()

# dev.off()
# surprised that CIs for BT are only considerably high and not through the roof


## WARNING!!! comparison of RM and BT from their respective models

rasch_b <- fixef(aut_RM_3)
rasch_se <- sqrt(diag(vcov(aut_RM_3)))   # SEs from RM
bt_b <- c(0, coef(aut_BT_1))
bt_se <- c(0, sqrt(diag(vcov(aut_BT_1)))) # SEs for BT (0 has SE=0)

## Item names
items <- paste0("V", seq_along(bt_b))

mod <- lm(bt_b ~ rasch_b)
newx <- seq(min(rasch_b), max(rasch_b), length.out = 100)
conf <- predict(mod, newdata = data.frame(rasch_b = newx),
                interval = "confidence", level = 0.95)
preds <- predict(mod, newdata = data.frame(rasch_b = newx),
                 interval = "prediction", level = 0.95)

## Plot
# png("Plots/AUT/aut_plot_comparison_separate.png", width = 3500, height = 2200, res = 230)


plot(rasch_b, bt_b,
     xlab = "Rasch model item difficulties",
     ylab = "Bradley-Terry model",
     main = "Item difficulties for DERS",
     pch = 19)+
  arrows(rasch_b - 1.96*rasch_se, bt_b,
         rasch_b + 1.96*rasch_se, bt_b,
         angle = 90, code = 3, length = 0.05, col = "darkgray")+
  arrows(rasch_b, bt_b - 1.96*bt_se,
         rasch_b, bt_b + 1.96*bt_se,
         angle = 90, code = 3, length = 0.05, col = "darkgray")+
  text(rasch_b+0.01, bt_b, labels = items, pos = 2, cex = 0.7)+
  lines(newx, conf[, "fit"], col = "black", lwd = 2)+
  # lines(newx, preds[, "lwr"], col = "red", lty = 2)+
  # lines(newx, preds[, "upr"], col = "red", lty = 2)
  polygon(c(newx, rev(newx)),
          c(preds[, "lwr"], rev(preds[, "upr"])),
          col = rgb(0,0,1,0.05), border = NA)+
  polygon(c(newx, rev(newx)),
          c(conf[, "lwr"], rev(conf[, "upr"])),
          col = rgb(1,0,0,0.1), border = NA)

# dev.off()


# assumption of GLMM is that there are equal variances across categories on latent trait. 

x1 <- as.data.frame(confint(aut_GLOB_1, method = "Wald")[-c(1:2), ])
x2 <- as.data.frame(confint(aut_RM_3, method = "Wald") [-1, ])
x3 <- as.data.frame(confint(aut_BT_1, method = "Wald"))

x1$dif <- x1$`97.5 %` - x1$`2.5 %`
x2$dif <- x2$`97.5 %` - x2$`2.5 %`
x3$dif <- x3$`97.5 %` - x3$`2.5 %`


x1; x2; x3
# we could consider this assumption to be supported 


# FINAL STATEMENT FOR AUTONOMY IS THAT IT DOES NOT CORRESPOND WITH SUPPOSED DIRECTION OF THE RELATION BETWEEN BT AND RM ESTIMATES
# I checked the estimation of our collection separately to be in line with P. Jansen, who says that this estimation can match but only when the same population is concerned, but there is no difference in estimation
# POSSIBLE REASONS ARE:
# - response processes. People have answer items differently than their subjective portrayal of those items
# - actually items V1 and V3 scored really low on item difficulties, bcs they are not really representative of the high autonomy (I can decide, how I want to live my life, but literally everyone can do that, right? 
#     But if I can live my everyday life without need to shape my behavior acc to others´ expectations, I am more autonomous.)



# BMPN analysis ####

### Final Rasch Model only ----
# Data from our collection
bmpn_RM_1 <- glmer(r~0+(V1+V2+V3+V4+V5)+(1|id), data=analysis_total[analysis_total$id_type==1 & analysis_total$collection == 1 & analysis_total$dataset_id == "BMPN",], family = binomial)
summary(bmpn_RM_1)

# Outsourced data collection
bmpn_RM_2 <- glmer(r~0+(V1+V2+V3+V4+V5)+(1|id), data=analysis_total[analysis_total$id_type == 1 & analysis_total$dataset_id == "BMPN" & analysis_total$collection == 2,], family = binomial)

summary(bmpn_RM_2)

# combined data collection
summary(bmpn_RM_3 <- glmer(r~0+(V1+V2+V3+V4+V5)+(1|id), data=analysis_total[analysis_total$id_type == 1 & analysis_total$dataset_id == "BMPN",], family = binomial))
summary(bmpn_RM_4 <- glmer(r~0+(V1+V2+V3+V4+V5)+(1|id) + (1|collection), data=analysis_total[analysis_total$id_type == 1 & analysis_total$dataset_id == "BMPN",], family = binomial)) # didnt converge
dotplot(ranef(bmpn_RM_4))$collection


# checking relation of the two item collections
cor.test(as.numeric(coef(bmpn_RM_2)$id[1,-1]), as.numeric(coef(bmpn_RM_1)$id[1,-1])) # nice high correlation

# png("Plots/BMPN/bmpn_item_difficulties_collections.png", width = 3500, height = 2200, res = 230)
plot(as.numeric(coef(bmpn_RM_2)$id[1,-1]), 
     as.numeric(coef(bmpn_RM_1)$id[1,-1]), 
     xlab = "Rasch model item difficulties of collection 2", ylab = "Rasch model item difficulties of collection 1",
     main = "Item difficulties across collections")

x <- lm(as.numeric(coef(bmpn_RM_1)$id[1,-1])~ 
          as.numeric(coef(bmpn_RM_2)$id[1,-1]))
abline(x)
# dev.off()


### Final BT model only ----
summary(bmpn_BT_1 <- glm(r~0 + (V2+V3+V4+V5), data=analysis_total[analysis_total$id_type==0 & analysis_total$dataset_id == "BMPN",], family = binomial))


MuMIn::r.squaredGLMM(bmpn_BT_1)

#                R2m       R2c
# theoretical 0.8770122 0.8770122
# delta       0.8534625 0.8534625

# heh, really really impressive hmmm

fmsb::NagelkerkeR2(bmpn_BT_1) # 0.7904186

### Global model ----

bmpn_GLOB_1 <- glmer(r ~ 0 + (V2+V3+V4+V5) + (0+id_type|id) + (0+item_type|item), data = analysis_total[analysis_total$dataset_id == "BMPN",], family = binomial)
bmpn_GLOB_1s <- glmer(r ~ 0 + (V2+V3+V4+V5) + (0+item_type|item), data = analysis_total[analysis_total$dataset_id == "BMPN",], family = binomial) #simpler model
bmpn_GLOB_1t <- glmer(r ~ 0 + (V2+V3+V4+V5) + (0+id_type|id), data = analysis_total[analysis_total$dataset_id == "BMPN",], family = binomial) #simpler model
bmpn_GLOB_1e <- glm(r ~ 0 + (V2+V3+V4+V5), data = analysis_total[analysis_total$dataset_id == "BMPN",], family = binomial) #simpler model
summary(bmpn_GLOB_1)

# checking significance
plogis(fixef(bmpn_GLOB_1))
car::Anova(bmpn_GLOB_1, type=3) # V4 is bad
car::Anova(bmpn_RM_1, type=3)
car::Anova(bmpn_RM_3, type=3) # looks good
car::Anova(bmpn_BT_1, type=3) # looks good

# profiling

pr_bmpn <- profile(bmpn_GLOB_1) # took 12 minutes on M2
splom(pr_bmpn) # looks pretty good for id_type, id_type is considerably good, although reportedly longer CIs are to be expected mainly in higher easiness items, otherwise good to go

# it is nicely seen that high inter-correlations between items affect the healthiness of estimation

# testing two models
simp_mod_anova_bmpn <- anova(bmpn_GLOB_1, bmpn_GLOB_1s, bmpn_GLOB_1t, bmpn_GLOB_1e) # in favor of bmpn_GLOB_1

# what it looks like?
summary(bmpn_GLOB_1) # high correlations

qqmath(ranef(bmpn_GLOB_1)) # there is a difference in people (they have different thetas) and also in items (different difficulties)
qqnorm(resid(bmpn_GLOB_1),main="Residual normal plot",col=4,adj=0.1)
qqline(resid(bmpn_GLOB_1)) # separation of residuals

# model on our data collection
bmpn_GLOB_2 <- glmer(r ~ 0 + (V2+V3+V4+V5) + (0+id_type|id) + (0+item_type|item), 
                     data = analysis_total[analysis_total$dataset_id == "BMPN" & analysis_total$collection == 1,], family = binomial) # only our data collection


qqmath(ranef(bmpn_GLOB_2)) # there is a difference in items (different difficulties), but interestingly no difference in people...
qqnorm(resid(bmpn_GLOB_2),main="Residual normal plot",col=4,adj=0.1)
qqline(resid(bmpn_GLOB_2)) # looks good


# checking model GLOB_1
performance::check_model(bmpn_GLOB_1) # high collinearity and almost all r estimates outside of simulated range together with influential observations, prolly due to BTM model
performance::model_performance(bmpn_GLOB_1) # R2 conditional 0.727, marginal 0.727, RMSE 0.339

# Reliability of BTM

var(fixef(bmpn_GLOB_1))/(var(fixef(bmpn_GLOB_1))+mean(diag(vcov(bmpn_GLOB_1)))) # 0.9377402

# Reliability of RM

as.data.frame(VarCorr(bmpn_GLOB_1))[2, "vcov"]/(as.data.frame(VarCorr(bmpn_GLOB_1))[2, "vcov"]+rmse(bmpn_GLOB_1)) # 0.8732632

?rmse()
rmse(bmpn_GLOB_1) # 0.326

# manually calculating contribution of fixed effects to variance explained

var(fixef(bmpn_GLOB_1)) / (
  as.data.frame(VarCorr(bmpn_GLOB_1))[2, "vcov"] + var(fixef(bmpn_GLOB_1))
)

# calculation of contribution to explained variance via presudo R2 for GLMM (Nakagawa & Schielzeth, 2013)

MuMIn::r.squaredGLMM(bmpn_GLOB_1) 
#                 R2m       R2c
# theoretical 0.4510819 0.8038809
# delta       0.4324530 0.7706820

MuMIn::r.squaredGLMM(bmpn_GLOB_2) 

#                 R2m       R2c
# theoretical 0.7735485 0.8142901
# delta       0.7429747 0.7821059

# looks good and is comparable to manually calculated contribution, it is also seen, that the more cases there are in RM, the more information they source from them


AIC(bmpn_GLOB_1, bmpn_BT_1) # pointless as we have different number of observations

# what about residuals? 
simres <- simulateResiduals(bmpn_GLOB_1)
plot(simres)
testOutliers(simres, type = "bootstrap") # good, no signs of outlier related misfit


pred <- predict(bmpn_GLOB_1, type = "response")
range(pred) # good separation


## compare random effect of ID across RM and GLOBAL -> persons are estimated to be the same
plot(jitter(ranef(bmpn_GLOB_1)$id[-1, 1]), 
     jitter(ranef(bmpn_RM_3)$id[,1]), 
     xlab = "global model", ylab = "rasch model", 
     main = "Comparison of estimated latent traits (thetas)")
x <- lm(ranef(bmpn_RM_3)$id[,1]~ranef(bmpn_GLOB_1)$id[-1, 1])
abline(x)

cor(ranef(bmpn_GLOB_1)$id[-1, 1], ranef(bmpn_RM_3)$id[,1])
# great, so both models predict person parameters similarly

dotplot(ranef(bmpn_GLOB_1))$id
dotplot(ranef(bmpn_RM_3))$id

###### Compare item difficulties across BT and GLOBAL ----
plot(c(0,fixef(bmpn_GLOB_1)), 
     c(0, coef(bmpn_BT_1)), col = c("red", rep("black", 6)), 
     xlab = "Global model item difficulties of BT part", ylab = "Bradley-Terry model",
     main = "Item difficulties")

x <- lm(c(0, coef(bmpn_BT_1))~c(0,fixef(bmpn_GLOB_1)))
abline(x)
cor(c(0,fixef(bmpn_GLOB_1)), 
    c(0, coef(bmpn_BT_1)))
# looks perfect


###### Compare item difficulties across RM and GLOBAL ----

# png("Plots/BMPN/bmpn_plot_RM.png", width = 3500, height = 2200, res = 230)
plot(c(0,fixef(bmpn_GLOB_1)) + as.vector(coef(bmpn_GLOB_1)$item[,2]), 
     fixef(bmpn_RM_3), 
     xlab = "Global model item difficulties of Rasch part", ylab = "Rasch model",
     main = "Item difficulties")

x <- lm(as.numeric(fixef(bmpn_RM_3)) ~ as.vector(coef(bmpn_GLOB_1)$item[,2])+c(0,fixef(bmpn_GLOB_1)))
abline(x)
# dev.off()
cor(as.numeric(fixef(bmpn_RM_3)), as.vector(coef(bmpn_GLOB_1)$item[,2])+c(0,fixef(bmpn_GLOB_1)))


# finally, Item estimates are similar across both models, meaning GLOBAL model works well in estimating (possible problem is only heightened noise)



## Compare item difficulties across RM and BT ----

rasch_b <- c(0,fixef(bmpn_GLOB_1)) + as.vector(coef(bmpn_GLOB_1)$item[,2])
rasch_se <- as.vector(arm::se.ranef(bmpn_GLOB_1)$item)   # SEs from RM
bt_b <- c(0,fixef(bmpn_GLOB_1))
bt_se <- c(0, sqrt(diag(vcov(bmpn_GLOB_1)))) # SEs for BT (0 has SE=0)


## Item names
items <- paste0("V", seq_along(bt_b))

mod <- lm(bt_b ~ rasch_b)
newx <- seq(min(rasch_b), max(rasch_b), length.out = 100)
conf <- predict(mod, newdata = data.frame(rasch_b = newx),
                interval = "confidence", level = 0.95)
preds <- predict(mod, newdata = data.frame(rasch_b = newx),
                 interval = "prediction", level = 0.95)

## Plot
r <- cor(rasch_b, bt_b)
# png("Plots/BMPN/bmpn_plot_comparison.png", width = 3500, height = 2200, res = 230)


plot(rasch_b, bt_b,
     xlab = "RM part item easiness",
     ylab = "BTM part item parameters",
     main = paste0("Item difficulties for BMPN (Global model)", "\n r = ", round(r, 3)),
     pch = 19)+
  arrows(rasch_b - 1.96*rasch_se, bt_b,
         rasch_b + 1.96*rasch_se, bt_b,
         angle = 90, code = 3, length = 0.05, col = "darkgray")+
  arrows(rasch_b, bt_b - 1.96*bt_se,
         rasch_b, bt_b + 1.96*bt_se,
         angle = 90, code = 3, length = 0.05, col = "darkgray")+
  text(rasch_b+0.01, bt_b, labels = items, pos = 2, cex = 0.7)+
  lines(newx, conf[, "fit"], col = "black", lwd = 2)+
  # lines(newx, preds[, "lwr"], col = "red", lty = 2)+
  # lines(newx, preds[, "upr"], col = "red", lty = 2)
  polygon(c(newx, rev(newx)),
          c(preds[, "lwr"], rev(preds[, "upr"])),
          col = rgb(0,0,1,0.05), border = NA)+
  polygon(c(newx, rev(newx)),
          c(conf[, "lwr"], rev(conf[, "upr"])),
          col = rgb(1,0,0,0.1), border = NA)

# dev.off()
bmpn_comparison <- recordPlot()
cor.test(rasch_b, bt_b)

# CIs are good, but the direction of items is really in different direction


## WARNING!!! comparison of RM and BT from their respective models

rasch_b <- fixef(bmpn_RM_3)
rasch_se <- sqrt(diag(vcov(bmpn_RM_3)))   # SEs from RM
bt_b <- c(0, coef(bmpn_BT_1))
bt_se <- c(0, sqrt(diag(vcov(bmpn_BT_1)))) # SEs for BT (0 has SE=0)

## Item names
items <- paste0("V", seq_along(bt_b))

mod <- lm(bt_b ~ rasch_b)
newx <- seq(min(rasch_b), max(rasch_b), length.out = 100)
conf <- predict(mod, newdata = data.frame(rasch_b = newx),
                interval = "confidence", level = 0.95)
preds <- predict(mod, newdata = data.frame(rasch_b = newx),
                 interval = "prediction", level = 0.95)

## Plot
# png("Plots/BMPN/bmpn_plot_comparison_separate.png", width = 3500, height = 2200, res = 230)


plot(rasch_b, bt_b,
     xlab = "Rasch model item difficulties",
     ylab = "Bradley-Terry model",
     main = "Item difficulties for BMPN",
     pch = 19)+
  arrows(rasch_b - 1.96*rasch_se, bt_b,
         rasch_b + 1.96*rasch_se, bt_b,
         angle = 90, code = 3, length = 0.05, col = "darkgray")+
  arrows(rasch_b, bt_b - 1.96*bt_se,
         rasch_b, bt_b + 1.96*bt_se,
         angle = 90, code = 3, length = 0.05, col = "darkgray")+
  text(rasch_b+0.01, bt_b, labels = items, pos = 2, cex = 0.7)+
  lines(newx, conf[, "fit"], col = "black", lwd = 2)+
  # lines(newx, preds[, "lwr"], col = "red", lty = 2)+
  # lines(newx, preds[, "upr"], col = "red", lty = 2)
  polygon(c(newx, rev(newx)),
          c(preds[, "lwr"], rev(preds[, "upr"])),
          col = rgb(0,0,1,0.05), border = NA)+
  polygon(c(newx, rev(newx)),
          c(conf[, "lwr"], rev(conf[, "upr"])),
          col = rgb(1,0,0,0.1), border = NA)

# dev.off()

# interestingly, here is no change between GLOBAL model and separate models...



# assumption of GLMM is that there are equal variances across categories on latent trait. 

x1 <- as.data.frame(confint(bmpn_GLOB_1, method = "Wald")[-c(1:2), ])
x2 <- as.data.frame(confint(bmpn_RM_3, method = "Wald") [-1, ])
x3 <- as.data.frame(confint(bmpn_BT_1, method = "Wald"))

x1$dif <- x1$`97.5 %` - x1$`2.5 %`
x2$dif <- x2$`97.5 %` - x2$`2.5 %`
x3$dif <- x3$`97.5 %` - x3$`2.5 %`


x1; x2; x3
# we could consider this assumption to be supported 


# FINAL STATEMENT FOR BMPN IS THAT IT DOESN´T FOLLOW DESIRED DIRECTION, ESTIMATES TEND TO BE ON REGRESSION LINE, BUT IN DIFFERENT DIRECTION
# Interestingly, it is only in 2 scales that have negatively worded items that the directions are turned around. What could cause this? Maybe it influences perceived difficulties of other items in BT model?
#       - factor structure is completely off in this case, idk what to do about it...




# DASS analysis ----

### Final Rasch Model only ----
# Data from our collection
dass_RM_1 <- glmer(r~0+(V1+V2+V3+V4+V5+V6+V7)+(1|id), data=analysis_total[analysis_total$id_type==1 & analysis_total$collection == 1 & analysis_total$dataset_id == "DASS",], family = binomial)
summary(dass_RM_1)


# Outsourced data collection
dass_RM_2 <- glmer(r~0+(V1+V2+V3+V4+V5+V6+V7)+(1|id), data=analysis_total[analysis_total$id_type == 1 & analysis_total$dataset_id == "DASS" & analysis_total$collection == 2,], family = binomial) # model failed to converge, trying another optimizer
dass_RM_2 <- update(dass_RM_2, control = glmerControl(optimizer = "bobyqa"))

# combined data collection
dass_RM_3 <- glmer(r~0+(V1+V2+V3+V4+V5+V6+V7)+(1|id), data=analysis_total[analysis_total$id_type == 1 & analysis_total$dataset_id == "DASS",], family = binomial, control = glmerControl(optimizer = "bobyqa")) # model failed to converge, trying another optimizer
dass_RM_4 <- glmer(r~0+(V1+V2+V3+V4+V5+V6+V7)+(1|id) + (1|collection), data=analysis_total[analysis_total$id_type == 1 & analysis_total$dataset_id == "DASS",], family = binomial, control = glmerControl(optimizer = "bobyqa")) # model failed to converge, trying another optimizer
dotplot(ranef(dass_RM_4))$collection

anova(dass_RM_3, dass_RM_4, test = "Chisq")


# checking relation of the two item collections
cor(as.numeric(coef(dass_RM_2)$id[1,-1]), as.numeric(coef(dass_RM_1)$id[1,-1]))
plot(as.numeric(coef(dass_RM_2)$id[1,-1]), 
     as.numeric(coef(dass_RM_1)$id[1,-1]), 
     xlab = "Rasch model item difficulties of collection 2", ylab = "Rasch model item difficulties of collection 1",
     main = "Item difficulties across collections")

x <- lm(as.numeric(coef(dass_RM_1)$id[1,-1])~ 
     as.numeric(coef(dass_RM_2)$id[1,-1]))
abline(x)

# I don´t like this as the estimates are really off... and there were some convergence issues in outsourced data... let´s check it thoroughly
# I don´t really know what to do with this. I checked the outsourced data and they are as we got them, no problem in the dataset
# Let´s continue with the analysis, but we need to make sure to compare collection 2 against itself also
# I checked the FA for both datasets, I would say both reported 1 factor, althout fa.parallel for outsourced data suggested 3 factors and 1 PC
#     but when I tried 3 factors for both datasets, items 4 and 6 were loaded on different factors across the collections, while the rest remained on the same factors. IDK what to do with this...
#     although in outsourced data V6 created basically its own factor, while in ours it was V4



### Final BT model only ----
dass_BT_1 <- glm(r~0 + (V2+V3+V4+V5+V6+V7), data=analysis_total[analysis_total$id_type==0 & analysis_total$dataset_id == "DASS",], family = binomial)

(7/6)*(1-sum(diag(vcov(dass_BT_1)))/var(coef(dass_BT_1)))
1-mean(diag(vcov(dass_BT_1)))/var(coef(dass_BT_1))

summary(dass_BT_1)
performance::check_model(dass_BT_1)
model_performance(dass_BT_1)

MuMIn::r.squaredGLMM(dass_BT_1)

#                R2m       R2c
# theoretical 0.2575713 0.2575713
# delta       0.2219099 0.2219099

fmsb::NagelkerkeR2(dass_BT_1) # 0.2577254

# pretty small R2 actually


### Global model ----
dass_GLOB_1  <- glmer(r ~ 0 + (V2+V3+V4+V5+V6+V7) + (0+id_type|id) + (0+item_type|item), 
                      data = analysis_total[analysis_total$dataset_id == "DASS",], 
                      family = binomial)
dass_GLOB_1s <- glmer(r ~ 0 + (V2+V3+V4+V5+V6+V7) + (0+item_type|item), 
                      data = analysis_total[analysis_total$dataset_id == "DASS",], 
                      family = binomial)
dass_GLOB_1t <- glmer(r ~ 0 + (V2+V3+V4+V5+V6+V7) + (0+id_type|id), 
                      data = analysis_total[analysis_total$dataset_id == "DASS",], 
                      family = binomial)
dass_GLOB_1e <- glm(r ~ 0 + (V2+V3+V4+V5+V6+V7), 
                      data = analysis_total[analysis_total$dataset_id == "DASS",], 
                      family = binomial)
summary(dass_GLOB_1)

# checking significance
plogis(fixef(dass_GLOB_1))
car::Anova(dass_GLOB_1, type=3) # looks good actually
car::Anova(dass_RM_1, type=3)
car::Anova(dass_RM_3, type=3) # V3 is problematic
car::Anova(dass_BT_1, type=3)

# profiling

pr_dass <- profile(dass_GLOB_1)
splom(pr_dass) # nothing


# testing two models
simp_mod_anova_dass <- anova(dass_GLOB_1, dass_GLOB_1s, dass_GLOB_1t, dass_GLOB_1e)

plot(dass_GLOB_1, resid(., scaled=TRUE) ~ fitted(.), abline = 0,pch=16,xlab="Fitted values",ylab="Standardised residuals")
plot(dass_GLOB_1, resid(., scaled=TRUE) ~ fitted(.)| item, abline = 0,pch=16,xlab="Fitted values",ylab="Standardised residuals")
qqmath(ranef(dass_GLOB_1))

c <- DHARMa::simulateResiduals(dass_GLOB_1)
plot(c)




qqmath(ranef(dass_GLOB_1)) # there is a difference in people (they have different thetas) and also in items (different difficulties) although not so obvious
qqnorm(resid(dass_GLOB_1),main="Residual normal plot",col=4,adj=0.1)
qqline(resid(dass_GLOB_1)) # heavy tails, influential observations...

performance::check_model(dass_GLOB_1) # r estimates outside of simulated range, prolly due to BTM model, also a lot of influential observations
performance::model_performance(dass_GLOB_1) # R2 conditional 0.138, marginal 0.138, RMSE 0.353 # really bad model

# Reliability of BTM

var(fixef(dass_GLOB_1))/(var(fixef(dass_GLOB_1))+mean(diag(vcov(dass_GLOB_1)))) # 0.9662824

# only our data collection
dass_GLOB_2 <- glmer(r ~ 0 + (V2+V3+V4+V5+V6+V7) + (0+id_type|id) + (0+item_type|item), data = analysis_total[analysis_total$dataset_id == "DASS" & analysis_total$collection == 1,], family = binomial)

dotplot(ranef(dass_GLOB_2))

qqmath(ranef(dass_GLOB_2)) # there is a difference in people (they have different thetas) although not so big (not enough data for RM) and also in items (different difficulties) although not so obvious
qqnorm(resid(dass_GLOB_2),main="Residual normal plot",col=4,adj=0.1)
qqline(resid(dass_GLOB_2)) # heavy tails + both models show discontinuity in the middle


# manually calculating contribution of fixed effects to variance explained

var(fixef(dass_GLOB_1)) / (
  as.data.frame(VarCorr(dass_GLOB_1))[2, "vcov"] + var(fixef(dass_GLOB_1))
) # idk if this is a food equation

# calculation of contribution to explained variance via pseudo R2 for GLMM (Nakagawa & Schielzeth, 2013)

MuMIn::r.squaredGLMM(dass_GLOB_1) 
#                    R2m       R2c
# theoretical 0.06398580 0.6001151
# delta       0.05889295 0.5523499

MuMIn::r.squaredGLMM(dass_GLOB_2) 

#                   R2m       R2c
# theoretical 0.1918887 0.3431267
# delta       0.1680402 0.3004818

# why is marginal pseudo R2 so much bigger on our data only? maybe, bcs outsourced data are so much bigger?

AIC(dass_GLOB_1, dass_BT_1) # pointless as we have different number of observations

# what about residuals? 
simres <- simulateResiduals(dass_GLOB_1)
plot(simres)
testOutliers(simres, type = "bootstrap") # good, no signs of outlier related misfit


pred <- predict(aut_GLOB_1, type = "response")
range(pred) # good separation


## compare random effect of ID across RM and GLOBAL -> persons are estimated to be the same
plot(jitter(ranef(dass_GLOB_1)$id[-1, 1]), 
     jitter(ranef(dass_RM_3)$id[,1]), 
     xlab = "global model", ylab = "rasch model", 
     main = "Comparison of estimated latent traits (thetas)")
x <- lm(ranef(dass_RM_3)$id[,1]~ranef(dass_GLOB_1)$id[-1, 1])
abline(x)

cor.test(ranef(dass_GLOB_1)$id[-1, 1], ranef(dass_RM_3)$id[,1])
dotplot(ranef(dass_GLOB_1))$id
dotplot(ranef(dass_RM_3))$id
# great, so both models predict person parameters similarly


###### Compare item difficulties across BT and GLOBAL ----
plot(c(0,fixef(dass_GLOB_1)), 
     c(0, coef(dass_BT_1)), col = c("red", rep("black", 6)), 
     xlab = "Global model item difficulties of BT part", ylab = "Bradley-Terry model",
     main = "Item difficulties")

x <- lm(c(0, coef(dass_BT_1))~c(0,fixef(dass_GLOB_1)))
abline(x)

cor(c(0,fixef(dass_GLOB_1)), 
    c(0, coef(dass_BT_1)))

# good, so GLOBAL model estimates items similarly as BT model

###### Compare item difficulties across RM and GLOBAL ----

# png("Plots/DASS/aut_plot_RM.png", width = 3500, height = 2200, res = 230)
plot(c(0,fixef(dass_GLOB_1))+as.vector(coef(dass_GLOB_1)$item[,2]), 
     as.numeric(fixef(dass_RM_3)), 
     xlab = "Global model item difficulties of Rasch part", ylab = "Rasch model",
     main = "Item difficulties")

x <- lm(as.numeric(fixef(dass_RM_3)) ~ c(0,fixef(dass_GLOB_1))+as.vector(coef(dass_GLOB_1)$item[,2]))

abline(x)

# dev.off()


cor.test(c(0,fixef(dass_GLOB_1))+as.vector(coef(dass_GLOB_1)$item[,2]), 
         as.numeric(fixef(dass_RM_3)))

# dunno why there is one item off, but overall it seems fine

### Compare item difficulties across RM and BT ----

rasch_b <- c(0,fixef(dass_GLOB_1))+as.vector(coef(dass_GLOB_1)$item[,2])
rasch_se <- as.vector(arm::se.ranef(dass_GLOB_1)$item)   # SEs from RM
bt_b <- c(0,fixef(dass_GLOB_1))
bt_se <- c(0,sqrt(diag(vcov(dass_GLOB_1)))) # SEs for BT (0 has SE=0)

## Item names
items <- paste0("V", seq_along(bt_b))

mod <- lm(bt_b ~ rasch_b)

newx <- seq(min(rasch_b), max(rasch_b), length.out = 100)
conf <- predict(mod, newdata = data.frame(rasch_b = newx),
                interval = "confidence", level = 0.95)
preds <- predict(mod, newdata = data.frame(rasch_b = newx),
                 interval = "prediction", level = 0.95)


## Plot

r <- cor(rasch_b, bt_b)
# png("Plots/DASS/dass_plot_comparison.png", width = 3500, height = 2200, res = 230)
plot(rasch_b, bt_b,
     xlab = "RM part item easiness",
     ylab = "BTM part item parameters",
     main = paste0("Item difficulties for DASS (Global model)", "\n r = ", round(r, 3)),
     pch = 19)+
  arrows(rasch_b - 1.96*rasch_se, bt_b,
         rasch_b + 1.96*rasch_se, bt_b,
         angle = 90, code = 3, length = 0.05, col = "darkgray")+
  arrows(rasch_b, bt_b - 1.96*bt_se,
         rasch_b, bt_b + 1.96*bt_se,
         angle = 90, code = 3, length = 0.05, col = "darkgray")+
  text(rasch_b+0.01, bt_b+0.03, labels = items, pos = 2, cex = 1)+
  lines(newx, conf[, "fit"], col = "black", lwd = 2)+
  # lines(newx, preds[, "lwr"], col = "red", lty = 2)+
  # lines(newx, preds[, "upr"], col = "red", lty = 2)
  polygon(c(newx, rev(newx)),
          c(preds[, "lwr"], rev(preds[, "upr"])),
          col = rgb(0,0,1,0.05), border = NA)+
  polygon(c(newx, rev(newx)),
          c(conf[, "lwr"], rev(conf[, "upr"])),
          col = rgb(1,0,0,0.1), border = NA)

# dev.off()



dass_comparison <- recordPlot()
# again, respondents just don´t understand that some items might not be as representative of stress as they apear to be
# in this example, the least difficult RM V3 (I felt nervous) won in BT over the most difficult RM V1 (I found it hard to wind down)

## WARNING!!! comparison of RM and BT from their respective models

rasch_b <- fixef(dass_RM_3)
rasch_se <- sqrt(diag(vcov(dass_RM_3)))   # SEs from RM
bt_b <- c(0, coef(dass_BT_1))
bt_se <- c(0, sqrt(diag(vcov(dass_BT_1)))) # SEs for BT (0 has SE=0)

## Item names
items <- paste0("V", seq_along(bt_b))

mod <- lm(bt_b ~ rasch_b)
newx <- seq(min(rasch_b), max(rasch_b), length.out = 100)
conf <- predict(mod, newdata = data.frame(rasch_b = newx),
                interval = "confidence", level = 0.95)
preds <- predict(mod, newdata = data.frame(rasch_b = newx),
                 interval = "prediction", level = 0.95)

## Plot
# png("Plots/DASS/dass_plot_comparison_separate.png", width = 3500, height = 2200, res = 230)


plot(rasch_b, bt_b,
     xlab = "Rasch model item difficulties",
     ylab = "Bradley-Terry model",
     main = "Item difficulties for DASS",
     pch = 19)+
  arrows(rasch_b - 1.96*rasch_se, bt_b,
         rasch_b + 1.96*rasch_se, bt_b,
         angle = 90, code = 3, length = 0.05, col = "darkgray")+
  arrows(rasch_b, bt_b - 1.96*bt_se,
         rasch_b, bt_b + 1.96*bt_se,
         angle = 90, code = 3, length = 0.05, col = "darkgray")+
  text(rasch_b+0.01, bt_b, labels = items, pos = 2, cex = 0.7)+
  lines(newx, conf[, "fit"], col = "black", lwd = 2)+
  # lines(newx, preds[, "lwr"], col = "red", lty = 2)+
  # lines(newx, preds[, "upr"], col = "red", lty = 2)
  polygon(c(newx, rev(newx)),
          c(preds[, "lwr"], rev(preds[, "upr"])),
          col = rgb(0,0,1,0.05), border = NA)+
  polygon(c(newx, rev(newx)),
          c(conf[, "lwr"], rev(conf[, "upr"])),
          col = rgb(1,0,0,0.1), border = NA)

# dev.off()



x1 <- as.data.frame(confint(dass_GLOB_1, method = "Wald")[-c(1:2), ])
x2 <- as.data.frame(confint(dass_RM_3, method = "Wald") [-1, ])
x3 <- as.data.frame(confint(dass_BT_1, method = "Wald"))


x1$dif <- x1$`97.5 %` - x1$`2.5 %`
x2$dif <- x2$`97.5 %` - x2$`2.5 %`
x3$dif <- x3$`97.5 %` - x3$`2.5 %`


x1; x2; x3


# same business as AUT


# DEMOCR analysis ####



### Final Rasch Model only ----
# Data from our collection
dem_RM_1 <- glmer(r~0+(V1+V2+V3+V4+V5)+(1|id), data=analysis_total[analysis_total$id_type==1 & analysis_total$collection == 1 & analysis_total$dataset_id == "DEM",], family = binomial)
summary(dem_RM_1)

# Outsourced data collection
dem_RM_2 <- glmer(r~0+(V1+V2+V3+V4+V5)+(1|id), data=analysis_total[analysis_total$id_type == 1 & analysis_total$dataset_id == "DEM" & analysis_total$collection == 2,], family = binomial)

summary(dem_RM_2)

# combined data collection
summary(dem_RM_3 <- glmer(r~0+(V1+V2+V3+V4+V5)+(1|id), data=analysis_total[analysis_total$id_type == 1 & analysis_total$dataset_id == "DEM",], family = binomial))
summary(dem_RM_4 <- glmer(r~0+(V1+V2+V3+V4+V5)+(1|id)+(1|collection), data=analysis_total[analysis_total$id_type == 1 & analysis_total$dataset_id == "DEM",], family = binomial))
dotplot(ranef(dem_RM_4))$collection
anova(ders_RM_3,ders_RM_4)
# checking relation of the two item collections
cor.test(as.numeric(coef(dem_RM_2)$id[1,-1]), as.numeric(coef(dem_RM_1)$id[1,-1]))

# png("Plots/DEM/dem_item_difficulties_collections.png", width = 3500, height = 2200, res = 230)
plot(as.numeric(coef(dem_RM_2)$id[1,-1]), 
     as.numeric(coef(dem_RM_1)$id[1,-1]), 
     xlab = "Rasch model item difficulties of collection 2", ylab = "Rasch model item difficulties of collection 1",
     main = "Item difficulties across collections")

x <- lm(as.numeric(coef(dem_RM_1)$id[1,-1])~ 
          as.numeric(coef(dem_RM_2)$id[1,-1]))
abline(x)
# dev.off()



### Final BT model only ----
summary(dem_BT_1 <- glm(r~0 + (V2+V3+V4+V5), data=analysis_total[analysis_total$id_type==0 & analysis_total$dataset_id == "DEM",], family = binomial))

(5/4)*(1-sum(diag(vcov(dem_BT_1)))/var(coef(dem_BT_1)))
1-mean(diag(vcov(dem_BT_1)))/var(coef(dem_BT_1))

MuMIn::r.squaredGLMM(dem_BT_1)

#                R2m       R2c
# theoretical 0.08276059 0.08276059
# delta       0.06863606 0.06863606

fmsb::NagelkerkeR2(dem_BT_1) # 0.09056631

### Global model ----

dem_GLOB_1 <- glmer(r ~ 0 + (V2+V3+V4+V5) + (0+id_type|id) + (0+item_type|item), data = analysis_total[analysis_total$dataset_id == "DEM",], family = binomial, control=glmerControl(optimizer = "bobyqa"), verbose = 1)
dem_GLOB_1s <- glmer(r ~ 0 + (V2+V3+V4+V5) + (0+item_type|item), data = analysis_total[analysis_total$dataset_id == "DEM",], family = binomial) #simpler model
dem_GLOB_1t <- glmer(r ~ 0 + (V2+V3+V4+V5) + (0+id_type|id), data = analysis_total[analysis_total$dataset_id == "DEM",], family = binomial) #simpler model
dem_GLOB_1e <- glm(r ~ 0 + (V2+V3+V4+V5), data = analysis_total[analysis_total$dataset_id == "DEM",], family = binomial) #simpler model

pr_dem <- profile(dem_GLOB_1, devtol = 1e-3, signames = FALSE) # problem with estimation of V3, if dropped, behaves correctly
pr_dem <- profile(dem_GLOB_1)

# checking significance
plogis(fixef(dem_GLOB_1))
car::Anova(dem_GLOB_1, type=3) # nothing
car::Anova(dem_RM_1, type=3)
car::Anova(dem_RM_3, type=3) # V2 and V3 nonsig
car::Anova(dem_BT_1, type=3) # V2 nonsig


# testing two models
simp_mod_anova_dem <- anova(dem_GLOB_1, dem_GLOB_1s, dem_GLOB_1t, dem_GLOB_1e) # in favor of dem_GLOB_1

# what it looks like?
summary(dem_GLOB_1) # high correlations

qqmath(ranef(dem_GLOB_1)) # there is a difference in people (they have different thetas) and also in items (different difficulties)
qqnorm(resid(dem_GLOB_1),main="Residual normal plot",col=4,adj=0.1)
qqline(resid(dem_GLOB_1)) # separation of parameters

# model on our data collection
dem_GLOB_2 <- glmer(r ~ 0 + (V2+V3+V4+V5) + (0+id_type|id) + (0+item_type|item), 
                    data = analysis_total[analysis_total$dataset_id == "DEM" & analysis_total$collection == 1,], family = binomial) # only our data collection

qqmath(ranef(dem_GLOB_2)) # there is a difference in items (different difficulties), but interestingly no difference in people...
qqnorm(resid(dem_GLOB_2),main="Residual normal plot",col=4,adj=0.1)
qqline(resid(dem_GLOB_2)) # heavy tails

# profiling for model 2
pr_dem_2 <- profile(dem_GLOB_2)
splom(pr_dem_2) # some imperfections in profiles

# checking model GLOB_1
performance::check_model(dem_GLOB_1) # high collinearity and r estimates mildly outside of simulated range together with influential observations, prolly due to BTM model
performance::model_performance(dem_GLOB_1) # R2 conditional 0.049, marginal 0.049, RMSE 0.388

# Reliability of BTM

var(fixef(dem_GLOB_1))/(var(fixef(dem_GLOB_1))+mean(diag(vcov(dem_GLOB_1)))) # 0.8683482

# manually calculating contribution of fixed effects to variance explained

var(fixef(dem_GLOB_1)) / (
  as.data.frame(VarCorr(dem_GLOB_1))[2, "vcov"] + var(fixef(dem_GLOB_1))
)

# calculation of contribution to explained variance via presudo R2 for GLMM (Nakagawa & Schielzeth, 2013)

MuMIn::r.squaredGLMM(dem_GLOB_1) 
#                   R2m       R2c
# theoretical 0.0231076 0.5494904
# delta       0.0210254 0.4999765

MuMIn::r.squaredGLMM(dem_GLOB_2) 

#                    R2m       R2c
# theoretical 0.03871974 0.4769837
# delta       0.03473493 0.4278953

# looks good and is comparable to manually calculated contribution, but as in previous cases, these models really source information from RM part rather then from BT part
# - on one hand it might be due to amount of information, on the other hand it might simply be due to the fact, that BT part isn´t that certain in position of items

AIC(dem_GLOB_1, dem_BT_1) # pointless as we have different number of observations

# what about residuals? 
simres <- simulateResiduals(dem_GLOB_1) # nonsig
plot(simres)
testOutliers(simres, type = "bootstrap") # good, no signs of outlier related misfit

pred <- predict(dem_GLOB_1, type = "response")
range(pred) # good separation


## compare random effect of ID across RM and GLOBAL -> persons are estimated to be the same
plot(jitter(ranef(dem_GLOB_1)$id[-1, 1]), 
     jitter(ranef(dem_RM_3)$id[,1]), 
     xlab = "global model", ylab = "rasch model", 
     main = "Comparison of estimated latent traits (thetas)")
x <- lm(ranef(dem_RM_3)$id[,1]~ranef(dem_GLOB_1)$id[-1, 1])
abline(x)

cor(ranef(dem_GLOB_1)$id[-1, 1], ranef(dem_RM_3)$id[,1])
# great, so both models predict person parameters similarly

dotplot(ranef(dem_GLOB_1))$id
dotplot(ranef(dem_RM_3))$id

###### Compare item difficulties across BT and GLOBAL ----
plot(c(0, fixef(dem_GLOB_1)), 
     c(0, coef(dem_BT_1)), col = c("red", rep("black", 6)), 
     xlab = "Global model item difficulties of BT part", ylab = "Bradley-Terry model",
     main = "Item difficulties")

x <- lm(c(0, coef(dem_BT_1))~c(0, fixef(dem_GLOB_1)))
abline(x)
cor(c(0, fixef(dem_GLOB_1)), 
    c(0, coef(dem_BT_1)))
# looks perfect


###### Compare item difficulties across RM and GLOBAL ----

# png("Plots/DEM/dem_plot_RM.png", width = 3500, height = 2200, res = 230)
plot(c(0, fixef(dem_GLOB_1)) + as.vector(coef(dem_GLOB_1)$item[,2]), 
     fixef(dem_RM_3), 
     xlab = "Global model item difficulties of Rasch part", ylab = "Rasch model",
     main = "Item difficulties")

x <- lm(as.numeric(fixef(dem_RM_3)) ~  as.vector(coef(dem_GLOB_1)$item[,2])+c(0, fixef(dem_GLOB_1)) )
abline(x)
# dev.off()
cor(as.numeric(fixef(dem_RM_3)), as.vector(coef(dem_GLOB_1)$item[,2])+c(0, fixef(dem_GLOB_1)) )


# finally, Item estimates are similar across both models, meaning GLOBAL model works well in estimating (possible problem is only heightened noise)



### Compare item difficulties across RM and BT ----

rasch_b <- c(0, fixef(dem_GLOB_1)) + as.vector(coef(dem_GLOB_1)$item[,2])
rasch_se <- as.vector(arm::se.ranef(dem_GLOB_1)$item)   # SEs from RM
bt_b <- c(0, fixef(dem_GLOB_1))
bt_se <- c(0,sqrt(diag(vcov(dem_GLOB_1)))) # SEs for BT (0 has SE=0)


## Item names
items <- paste0("V", seq_along(bt_b))

mod <- lm(bt_b ~ rasch_b)
newx <- seq(min(rasch_b), max(rasch_b), length.out = 100)
conf <- predict(mod, newdata = data.frame(rasch_b = newx),
                interval = "confidence", level = 0.95)
preds <- predict(mod, newdata = data.frame(rasch_b = newx),
                 interval = "prediction", level = 0.95)

## Plot
r <- cor(rasch_b, bt_b)
# png("Plots/DEM/dem_plot_comparison.png", width = 3500, height = 2200, res = 230)


plot(rasch_b, bt_b,
     xlab = "RM part item easiness",
     ylab = "BTM part item parameters",
     main = paste0("Item difficulties for DEM (Global model)", "\n r = ", round(r, 3)),
     pch = 19)+
  arrows(rasch_b - 1.96*rasch_se, bt_b,
         rasch_b + 1.96*rasch_se, bt_b,
         angle = 90, code = 3, length = 0.05, col = "darkgray")+
  arrows(rasch_b, bt_b - 1.96*bt_se,
         rasch_b, bt_b + 1.96*bt_se,
         angle = 90, code = 3, length = 0.05, col = "darkgray")+
  text(rasch_b+0.01, bt_b, labels = items, pos = 2, cex = 0.7)+
  lines(newx, conf[, "fit"], col = "black", lwd = 2)+
  # lines(newx, preds[, "lwr"], col = "red", lty = 2)+
  # lines(newx, preds[, "upr"], col = "red", lty = 2)
  polygon(c(newx, rev(newx)),
          c(preds[, "lwr"], rev(preds[, "upr"])),
          col = rgb(0,0,1,0.05), border = NA)+
  polygon(c(newx, rev(newx)),
          c(conf[, "lwr"], rev(conf[, "upr"])),
          col = rgb(1,0,0,0.1), border = NA)

dem_comparison <- recordPlot()
# dev.off()
cor.test(rasch_b, bt_b)

# gosh, those CIs for BTM are enormous and we might never know, what they should really look like. I can try to compare BT_1 and RM_3 


## WARNING!!! comparison of RM and BT from their respective models

rasch_b <- fixef(dem_RM_3)
rasch_se <- sqrt(diag(vcov(dem_RM_3)))   # SEs from RM
bt_b <- c(0, coef(dem_BT_1))
bt_se <- c(0, sqrt(diag(vcov(dem_BT_1)))) # SEs for BT (0 has SE=0)

## Item names
items <- paste0("V", seq_along(bt_b))

mod <- lm(bt_b ~ rasch_b)
newx <- seq(min(rasch_b), max(rasch_b), length.out = 100)
conf <- predict(mod, newdata = data.frame(rasch_b = newx),
                interval = "confidence", level = 0.95)
preds <- predict(mod, newdata = data.frame(rasch_b = newx),
                 interval = "prediction", level = 0.95)

## Plot
# png("Plots/DEM/dem_plot_comparison_separate.png", width = 3500, height = 2200, res = 230)


plot(rasch_b, bt_b,
     xlab = "Rasch model item difficulties",
     ylab = "Bradley-Terry model",
     main = "Item difficulties for DEM",
     pch = 19)+
  arrows(rasch_b - 1.96*rasch_se, bt_b,
         rasch_b + 1.96*rasch_se, bt_b,
         angle = 90, code = 3, length = 0.05, col = "darkgray")+
  arrows(rasch_b, bt_b - 1.96*bt_se,
         rasch_b, bt_b + 1.96*bt_se,
         angle = 90, code = 3, length = 0.05, col = "darkgray")+
  text(rasch_b+0.01, bt_b, labels = items, pos = 2, cex = 0.7)+
  lines(newx, conf[, "fit"], col = "black", lwd = 2)+
  # lines(newx, preds[, "lwr"], col = "red", lty = 2)+
  # lines(newx, preds[, "upr"], col = "red", lty = 2)
  polygon(c(newx, rev(newx)),
          c(preds[, "lwr"], rev(preds[, "upr"])),
          col = rgb(0,0,1,0.05), border = NA)+
  polygon(c(newx, rev(newx)),
          c(conf[, "lwr"], rev(conf[, "upr"])),
          col = rgb(1,0,0,0.1), border = NA)

# dev.off()



# assumption of GLMM is that there are equal variances across categories on latent trait. 

x1 <- as.data.frame(confint(dem_GLOB_1, method = "Wald")[-c(1:2), ]) # I see, there are enormous CIs for fixed effects....
x2 <- as.data.frame(confint(dem_RM_3, method = "Wald") [-1, ])
x3 <- as.data.frame(confint(dem_BT_1, method = "Wald"))



x1$dif <- x1$`97.5 %` - x1$`2.5 %`
x2$dif <- x2$`97.5 %` - x2$`2.5 %`
x3$dif <- x3$`97.5 %` - x3$`2.5 %`


x1; x2; x3
# we could consider this assumption to be supported 


# FINAL STATEMENT FOR DEM IS THAT ALTHOUGH IT FOLLOWS DESIRED DIRECTION, AND ESTIMATES TEND TO BE ON REGRESSION LINE, GLOBAL MODEL PRODUCES REALLY WIDE CIs MAKING IT HARD TO INTERPRET ON ANY DIRECTION OF THE RELATIONSHIP.
# I checked this with separately estimated RM and BT models and they produce much smaller CIs, so prolly the issue is in combining those two models into one GLMM framework.
#   - however, items that are close to each other in terms of difficulty in RM cannot be discerned well in BT and are therefore creating a bundle of items with overlapping CIs
#   - we therefore cannot be sure, whether this relation is due to good estimation or pure noise...
#       - it is 1 factor scale, although with somewhat not so strong loadings between items




# DERS analysis ####

### Final Rasch Model only ----
# Data from our collection
ders_RM_1 <- glmer(r~0+(V1+V2+V3+V4+V5+V6+V7)+(1|id), data=analysis_total[analysis_total$id_type==1 & analysis_total$collection == 1 & analysis_total$dataset_id == "DERS",], family = binomial)
summary(ders_RM_1)

# Outsourced data collection
ders_RM_2 <- glmer(r~0+(V1+V2+V3+V4+V5+V6+V7)+(1|id), data=analysis_total[analysis_total$id_type == 1 & analysis_total$dataset_id == "DERS" & analysis_total$collection == 2,], family = binomial)

summary(ders_RM_2)

# combined data collection
summary(ders_RM_3 <- glmer(r~0+(V1+V2+V3+V4+V5+V6+V7)+(1|id), data=analysis_total[analysis_total$id_type == 1 & analysis_total$dataset_id == "DERS",], family = binomial))
summary(ders_RM_4 <- glmer(r~0+(V1+V2+V3+V4+V5+V6+V7)+(1|id)+(1|collection), data=analysis_total[analysis_total$id_type == 1 & analysis_total$dataset_id == "DERS",], family = binomial), control = glmerControl(optimizer = "bobyqa"))
summary(ders_RM_3)
dotplot(ranef(ders_RM_4))$collection
anova(ders_RM_3, ders_RM_4, test = "Chisq")
# checking relation of the two item collections
cor.test(as.numeric(coef(ders_RM_2)$id[1,-1]), as.numeric(coef(ders_RM_1)$id[1,-1]))

# png("Plots/DERS/ders_item_difficulties_collections.png", width = 3500, height = 2200, res = 230)
plot(as.numeric(coef(ders_RM_2)$id[1,-1]), 
     as.numeric(coef(ders_RM_1)$id[1,-1]), 
     xlab = "Rasch model item difficulties of collection 2", ylab = "Rasch model item difficulties of collection 1",
     main = "Item difficulties across collections")

x <- lm(as.numeric(coef(ders_RM_1)$id[1,-1])~ 
          as.numeric(coef(ders_RM_2)$id[1,-1]))
abline(x)
# dev.off()


### Final BT model only ----
summary(ders_BT_1 <- glm(r~0 + (V2+V3+V4+V5+V6+V7), data=analysis_total[analysis_total$id_type==0 & analysis_total$dataset_id == "DERS",], family = binomial))

(7/6)*(1-sum(diag(vcov(ders_BT_1)))/var(coef(ders_BT_1)))
1-mean(diag(vcov(ders_BT_1)))/var(coef(ders_BT_1))

MuMIn::r.squaredGLMM(ders_BT_1)

#                R2m       R2c
# theoretical 0.2823593 0.2823593
# delta       0.2437784 0.2437784

fmsb::NagelkerkeR2(ders_BT_1) # 0.2866717

### Global model ----

ders_GLOB_1  <- glmer(r ~ 0 + (V2+V3+V4+V5+V6+V7) + (0+id_type|id) + (0+item_type|item), data = analysis_total[analysis_total$dataset_id == "DERS",], family = binomial)
ders_GLOB_1s <- glmer(r ~ 0 + (V2+V3+V4+V5+V6+V7) + (0+item_type|item), data = analysis_total[analysis_total$dataset_id == "DERS",], family = binomial) #simpler model
ders_GLOB_1t <- glmer(r ~ 0 + (V2+V3+V4+V5+V6+V7) + (0+id_type|id), data = analysis_total[analysis_total$dataset_id == "DERS",], family = binomial) #simpler model
ders_GLOB_1e <- glm(r ~ 0 + (V2+V3+V4+V5+V6+V7), data = analysis_total[analysis_total$dataset_id == "DERS",], family = binomial) #simpler model



# checking significance
plogis(fixef(ders_GLOB_1))
car::Anova(ders_GLOB_1, type=3) # v2 V7 nonsig
car::Anova(ders_RM_1, type=3)
car::Anova(ders_RM_3, type=3) # looks ok
car::Anova(ders_BT_1, type=3) # V2 and V7 nonsig

# profiling

pr_ders <- profile(ders_RM_3)
splom(pr_dass) # nothing

# testing two models
simp_mod_anova_ders <- anova(ders_GLOB_1, ders_GLOB_1s, ders_GLOB_1t, ders_GLOB_1e) # in favor of ders_GLOB_1

# what it looks like?
summary(ders_GLOB_1) # high correlations

qqmath(ranef(ders_GLOB_1)) # there is a difference in people (they have different thetas) and also in items (different difficulties)
qqnorm(resid(ders_GLOB_1),main="Residual normal plot",col=4,adj=0.1)
qqline(resid(ders_GLOB_1)) # looks pretty good actually

# model on our data collection
ders_GLOB_2 <- glmer(r ~ 0 + (V2+V3+V4+V5+V6+V7) + (0+id_type|id) + (0+item_type|item), 
                    data = analysis_total[analysis_total$dataset_id == "DERS" & analysis_total$collection == 1,], family = binomial) # only our data collection

qqmath(ranef(ders_GLOB_2)) # there is a difference in people (they have different thetas) and also in items (different difficulties) although not so obvious
qqnorm(resid(ders_GLOB_2),main="Residual normal plot",col=4,adj=0.1)
qqline(resid(ders_GLOB_2)) # heavy tails


# checking model GLOB_1
performance::check_model(ders_GLOB_1) # high collinearity and r estimates outside of simulated range together with influential observations, prolly due to BTM model
performance::model_performance(ders_GLOB_1) # R2 conditional 0.157, marginal 0.157, RMSE 0.272

# Reliability of BTM

var(fixef(ders_GLOB_1))/(var(fixef(ders_GLOB_1))+mean(diag(vcov(ders_GLOB_1)))) # 0.9684075

# manually calculating contribution of fixed effects to variance explained

var(fixef(ders_GLOB_1)) / (
  as.data.frame(VarCorr(ders_GLOB_1))[2, "vcov"] + var(fixef(ders_GLOB_1))
)

# calculation of contribution to explained variance via presudo R2 for GLMM (Nakagawa & Schielzeth, 2013)

MuMIn::r.squaredGLMM(ders_GLOB_1) 
#                    R2m       R2c
# theoretical 0.02940872 0.8422860
# delta       0.02844035 0.8145512

MuMIn::r.squaredGLMM(ders_GLOB_2) 

#                   R2m       R2c
# theoretical 0.1656922 0.5129847
# delta       0.1497531 0.4636371

# looks good and is comparable to manually calculated contribution

AIC(ders_GLOB_1, ders_BT_1) # pointless as we have different number of observations

# what about residuals? 
simres <- simulateResiduals(ders_GLOB_1)
plot(simres)
testOutliers(simres, type = "bootstrap") # good, no signs of outlier related misfit

pred <- predict(ders_GLOB_1, type = "response")
range(pred) # good separation


## compare random effect of ID across RM and GLOBAL -> persons are estimated to be the same
plot(jitter(ranef(ders_GLOB_1)$id[-1, 1]), 
     jitter(ranef(ders_RM_3)$id[,1]), 
     xlab = "global model", ylab = "rasch model", 
     main = "Comparison of estimated latent traits (thetas)")
x <- lm(ranef(ders_RM_3)$id[,1]~ranef(ders_GLOB_1)$id[-1, 1])
abline(x)

cor(ranef(ders_GLOB_1)$id[-1, 1], ranef(ders_RM_3)$id[,1])

# great, so both models predict person parameters similarly

dotplot(ranef(ders_GLOB_1))$id
dotplot(ranef(ders_RM_3))$id

###### Compare item difficulties across BT and GLOBAL ----
plot(c(0,fixef(ders_GLOB_1)), 
     c(0, coef(ders_BT_1)), col = c("red", rep("black", 6)), 
     xlab = "Global model item difficulties of BT part", ylab = "Bradley-Terry model",
     main = "Item difficulties")

x <- lm(c(0, coef(ders_BT_1))~c(0,fixef(ders_GLOB_1)))
abline(x)

cor(c(0,fixef(ders_GLOB_1)), 
    c(0, coef(ders_BT_1)))
# looks perfect


###### Compare item difficulties across RM and GLOBAL ----

# png("Plots/DERS/ders_plot_RM.png", width = 3500, height = 2200, res = 230)
plot(c(0,fixef(ders_GLOB_1)) + as.vector(coef(ders_GLOB_1)$item[,2]), 
     as.numeric(fixef(ders_RM_3)), 
     xlab = "Global model item difficulties of Rasch part", ylab = "Rasch model",
     main = "Item difficulties")

x <- lm(as.numeric(fixef(ders_RM_3)) ~ ( as.vector(coef(ders_GLOB_1)$item[,2])+c(0,fixef(ders_GLOB_1))))
abline(x)
# dev.off()
cor.test(as.numeric(fixef(ders_RM_3)), ( as.vector(coef(ders_GLOB_1)$item[,2])+c(0,fixef(ders_GLOB_1))))
# finally, Item estimates are similar across both models, meaning GLOBAL model works well in estimating (possible problem is only heightened noise)



### Compare item difficulties across RM and BT ----

rasch_b <- c(0,fixef(ders_GLOB_1)) + as.vector(coef(ders_GLOB_1)$item[,2])
rasch_se <- as.vector(arm::se.ranef(ders_GLOB_1)$item)   # SEs from RM
bt_b <- c(0,fixef(ders_GLOB_1))
bt_se <- c(0,sqrt(diag(vcov(ders_GLOB_1)))) # SEs for BT (0 has SE=0)

## Item names
items <- paste0("V", seq_along(bt_b))

mod <- lm(bt_b ~ rasch_b)
newx <- seq(min(rasch_b), max(rasch_b), length.out = 100)
conf <- predict(mod, newdata = data.frame(rasch_b = newx),
                interval = "confidence", level = 0.95)
preds <- predict(mod, newdata = data.frame(rasch_b = newx),
                 interval = "prediction", level = 0.95)

## Plot
r <- cor(rasch_b, bt_b)
# png("Plots/DERS/ders_plot_comparison.png", width = 3500, height = 2200, res = 230)


plot(rasch_b, bt_b,
     xlab = "RM part item easiness",
     ylab = "BTM part item parameters",
     main = paste0("Item difficulties for DERS (Global model)", "\n r = ", round(r, 3)),
     pch = 19)+
  arrows(rasch_b - 1.96*rasch_se, bt_b,
         rasch_b + 1.96*rasch_se, bt_b,
         angle = 90, code = 3, length = 0.05, col = "darkgray")+
  arrows(rasch_b, bt_b - 1.96*bt_se,
         rasch_b, bt_b + 1.96*bt_se,
         angle = 90, code = 3, length = 0.05, col = "darkgray")+
  text(rasch_b+0.01, bt_b, labels = items, pos = 2, cex = 0.7)+
  lines(newx, conf[, "fit"], col = "black", lwd = 2)+
  # lines(newx, preds[, "lwr"], col = "red", lty = 2)+
  # lines(newx, preds[, "upr"], col = "red", lty = 2)
  polygon(c(newx, rev(newx)),
          c(preds[, "lwr"], rev(preds[, "upr"])),
          col = rgb(0,0,1,0.05), border = NA)+
  polygon(c(newx, rev(newx)),
          c(conf[, "lwr"], rev(conf[, "upr"])),
          col = rgb(1,0,0,0.1), border = NA)

# dev.off()
ders_comparison <- recordPlot()


cor.test(rasch_b, bt_b)
## WARNING!!! comparison of RM and BT from their respective models

rasch_b <- fixef(ders_RM_3)
rasch_se <- sqrt(diag(vcov(ders_RM_3)))   # SEs from RM
bt_b <- c(0, coef(ders_BT_1))
bt_se <- c(0, sqrt(diag(vcov(ders_BT_1)))) # SEs for BT (0 has SE=0)

## Item names
items <- paste0("V", seq_along(bt_b))

mod <- lm(bt_b ~ rasch_b)
newx <- seq(min(rasch_b), max(rasch_b), length.out = 100)
conf <- predict(mod, newdata = data.frame(rasch_b = newx),
                interval = "confidence", level = 0.95)
preds <- predict(mod, newdata = data.frame(rasch_b = newx),
                 interval = "prediction", level = 0.95)

## Plot
# png("Plots/DERS/ders_plot_comparison_separate.png", width = 3500, height = 2200, res = 230)


plot(rasch_b, bt_b,
     xlab = "Rasch model item difficulties",
     ylab = "Bradley-Terry model",
     main = "Item difficulties for DERS",
     pch = 19)+
  arrows(rasch_b - 1.96*rasch_se, bt_b,
         rasch_b + 1.96*rasch_se, bt_b,
         angle = 90, code = 3, length = 0.05, col = "darkgray")+
  arrows(rasch_b, bt_b - 1.96*bt_se,
         rasch_b, bt_b + 1.96*bt_se,
         angle = 90, code = 3, length = 0.05, col = "darkgray")+
  text(rasch_b+0.01, bt_b, labels = items, pos = 2, cex = 0.7)+
  lines(newx, conf[, "fit"], col = "black", lwd = 2)+
  # lines(newx, preds[, "lwr"], col = "red", lty = 2)+
  # lines(newx, preds[, "upr"], col = "red", lty = 2)
  polygon(c(newx, rev(newx)),
          c(preds[, "lwr"], rev(preds[, "upr"])),
          col = rgb(0,0,1,0.05), border = NA)+
  polygon(c(newx, rev(newx)),
          c(conf[, "lwr"], rev(conf[, "upr"])),
          col = rgb(1,0,0,0.1), border = NA)

# dev.off()



# assumption of GLMM is that there are equal variances across categories on latent trait. 

x1 <- as.data.frame(confint(ders_GLOB_1, method = "Wald")[-c(1:2), ]) # I see, there are enormous CIs for fixed effects....
x2 <- as.data.frame(confint(ders_RM_3, method = "Wald") [-1, ])
x3 <- as.data.frame(confint(ders_BT_1, method = "Wald"))

x1$dif <- x1$`97.5 %` - x1$`2.5 %`
x2$dif <- x2$`97.5 %` - x2$`2.5 %`
x3$dif <- x3$`97.5 %` - x3$`2.5 %`


x1; x2; x3
# we could consider this assumption to be supported 


# FINAL STATEMENT FOR DERS IS THAT ALTHOUGH IT FOLLOWS DESIRED DIRECTION, AND ESTIMATES TEND TO BE ON REGRESSION LINE, GLOBAL MODEL PRODUCES REALLY WIDE CIs MAKING IT HARD TO INTERPRET ON ANY DIRECTION OF THE RELATIONSHIP.
# I checked this with separately estimated RM and BT models and they produce much smaller CIs, so prolly the issue is in combining those two models into one GLMM framework.
#   - only problem from this perspective is item V6, which is not aligned with other items (When I’m upset, I have difficulty getting work done)
#   - fa.parallel suggests 3 factors, but inspecting scree plot, i would report only one
#       - doing 3 factor fa, item V6 creates its own factor, while having almost no loadings on other 2, others bind closely to 1st or 2nd factor with an exception of V7, binding across factor structure.
#       - hence I would say, there might by some noise in V6 influencing fa.parallel

# overall however I am kind of impressed with the estimation here





# HI analysis men ####


### Final Rasch Model only ----
# Data from our collection
hi_m_RM_1 <- glmer(r~0+(V1+V2+V3+V4+V5+V6+V7+V8)+(1|id), data=analysis_total[analysis_total$id_type==1 & analysis_total$collection == 1 & analysis_total$dataset_id == "HI" & analysis_total$sex == "male",], family = binomial)
summary(hi_m_RM_1)

# Outsourced data collection
hi_m_RM_2 <- glmer(r~0+(V1+V2+V3+V4+V5+V6+V7+V8)+(1|id), data=analysis_total[analysis_total$id_type == 1 & analysis_total$dataset_id == "HI" & analysis_total$sex == "male" & analysis_total$collection == 2,], family = binomial, control = glmerControl(optimizer = "bobyqa")) # model failed to converge, trying another optimizer
summary(hi_m_RM_2)
# combined data collection
hi_m_RM_3 <- glmer(r~0+(V1+V2+V3+V4+V5+V6+V7+V8)+(1|id), 
                   data=analysis_total[analysis_total$id_type == 1 & analysis_total$dataset_id == "HI" & analysis_total$sex == "male",],
                   family = binomial) # model failed to converge, trying another optimizer
hi_m_RM_4 <- glmer(r~0+(V1+V2+V3+V4+V5+V6+V7+V8)+(1|id)+(1|collection), data=analysis_total[analysis_total$id_type == 1 & analysis_total$dataset_id == "HI" & analysis_total$sex == "male",], family = binomial) # model failed to converge, trying another optimizer
dotplot(ranef(hi_m_RM_4))$collection
summary(hi_m_RM_3)

anova(hi_m_RM_3, hi_m_RM_4, test = "Chisq")
# checking relation of the two item collections
cor.test(as.numeric(coef(hi_m_RM_2)$id[1,-1]), as.numeric(coef(hi_m_RM_1)$id[1,-1]))
plot(as.numeric(coef(hi_m_RM_2)$id[1,-1]), 
     as.numeric(coef(hi_m_RM_1)$id[1,-1]), 
     xlab = "Rasch model item difficulties of collection 2", ylab = "Rasch model item difficulties of collection 1",
     main = "Item difficulties across collections")

x <- lm(as.numeric(coef(hi_m_RM_1)$id[1,-1])~ 
          as.numeric(coef(hi_m_RM_2)$id[1,-1]))
abline(x)

# I don´t like this as the estimates are really off... and there were some convergence issues in outsourced data... let´s check it thoroughly
# I checked the FA for and though fa.parallel reported 2 factors, I only see 1 (side note, V2 better works for women)



### Final BT model only ----
hi_m_BT_1 <- glm(r ~ 0 + (V2+V3+V4+V5+V6+V7+V8), data=analysis_total[analysis_total$id_type==0 & analysis_total$dataset_id == "HI",], family = binomial)

(8/7)*(1-sum(diag(vcov(hi_m_BT_1)))/var(coef(hi_m_BT_1)))
1-mean(diag(vcov(hi_m_BT_1)))/var(coef(hi_m_BT_1))

summary(hi_m_BT_1)


MuMIn::r.squaredGLMM(hi_m_BT_1)

#                R2m       R2c
# theoretical 0.3085222 0.3085222
# delta       0.2683864 0.2683864

fmsb::NagelkerkeR2(hi_m_BT_1) # 0.3046708

# pretty small R2 actually


### Global model ----
hi_m_GLOB_1 <- glmer(r ~ 0 + (V2+V3+V4+V5+V6+V7+V8) + (0+id_type|id) + (0+item_type|item), data = analysis_total[(analysis_total$sex == "male" | is.na(analysis_total$sex)) & analysis_total$dataset_id == "HI",], family = binomial)
hi_m_GLOB_1s <- glmer(r ~ 0 + (V2+V3+V4+V5+V6+V7+V8) + (0+item_type|item), data = analysis_total[(analysis_total$sex == "male" | is.na(analysis_total$sex)) & analysis_total$dataset_id == "HI",], family = binomial)
hi_m_GLOB_1t <- glmer(r ~ 0 + (V2+V3+V4+V5+V6+V7+V8) + (0+id_type|id), data = analysis_total[(analysis_total$sex == "male" | is.na(analysis_total$sex)) & analysis_total$dataset_id == "HI",], family = binomial, control = glmerControl(optimizer = "bobyqa"))
hi_m_GLOB_1e <- glm(r ~ 0 + (V2+V3+V4+V5+V6+V7+V8), data = analysis_total[(analysis_total$sex == "male" | is.na(analysis_total$sex)) & analysis_total$dataset_id == "HI",], family = binomial)
summary(hi_m_GLOB_1)
# checking significance
plogis(fixef(hi_m_GLOB_1))
car::Anova(hi_m_GLOB_1, type=3) # nothing
car::Anova(hi_m_RM_1, type=3)
car::Anova(hi_m_RM_3, type=3) # looks good
car::Anova(hi_m_BT_1, type=3) # except for V7

# profiling

pr_hi_m <- profile(hi_m_GLOB_1) 
splom(pr_bmpn) # doesn´t work

# testing two models
simp_mod_anova_hi_m <- anova(hi_m_GLOB_1, hi_m_GLOB_1s, hi_m_GLOB_1t, hi_m_GLOB_1e)


# what it looks like?
summary(hi_m_GLOB_1) #

qqmath(ranef(hi_m_GLOB_1)) # there is a difference in people (they have different thetas) and also in items (different difficulties)
qqnorm(resid(hi_m_GLOB_1),main="Residual normal plot",col=4,adj=0.1)
qqline(resid(hi_m_GLOB_1)) # looks great

# model on our data collection
hi_m_GLOB_2 <- glmer(r ~ 0 + (V2+V3+V4+V5+V6+V7+V8) + (0+id_type|id) + (0+item_type|item), 
                     data = analysis_total[(analysis_total$sex == "male" | is.na(analysis_total$sex)) & analysis_total$dataset_id == "HI" & analysis_total$collection == 1,], family = binomial) # only our data collection

    # pointless, really little observations

qqmath(ranef(hi_m_GLOB_2)) # there is a difference in people (they have different thetas) and also in items (different difficulties) although not so obvious
qqnorm(resid(hi_m_GLOB_2),main="Residual normal plot",col=4,adj=0.1)
qqline(resid(hi_m_GLOB_2)) # heavy tails


# checking model GLOB_1
performance::check_model(hi_m_GLOB_1) # high collinearity and almost all r estimates outside of simulated range together with influential observations, prolly due to BTM model
performance::model_performance(hi_m_GLOB_1) # R2 conditional 0.176, marginal 0.176, RMSE 0.314

# Reliability of BTM

var(fixef(hi_m_GLOB_1))/(var(fixef(hi_m_GLOB_1))+mean(diag(vcov(hi_m_GLOB_1)))) # 0.974044

# manually calculating contribution of fixed effects to variance explained

var(fixef(hi_m_GLOB_1)) / (
  as.data.frame(VarCorr(hi_m_GLOB_1))[2, "vcov"] + var(fixef(hi_m_GLOB_1))
)

# calculation of contribution to explained variance via presudo R2 for GLMM (Nakagawa & Schielzeth, 2013)

MuMIn::r.squaredGLMM(hi_m_GLOB_1) 
#                    R2m       R2c
# theoretical 0.04617891 0.7837500
# delta       0.04411920 0.7487925

MuMIn::r.squaredGLMM(hi_m_GLOB_2) 

#                   R2m       R2c
# theoretical 0.2429884 0.4270798
# delta       0.2162187 0.3800289

# looks good and is comparable to manually calculated contribution, but BTM only small amount of information

AIC(hi_m_GLOB_1, hi_m_BT_1) # pointless as we have different number of observations

# what about residuals? 
simres <- simulateResiduals(hi_m_GLOB_1)
plot(simres)
testOutliers(simres, type = "bootstrap") # good, no signs of outlier related misfit

pred <- predict(hi_m_GLOB_1, type = "response")
range(pred) # good separation


## compare random effect of ID across RM and GLOBAL -> persons are estimated to be the same
plot(jitter(ranef(hi_m_GLOB_1)$id[-1, 1]), 
     jitter(ranef(hi_m_RM_3)$id[,1]), 
     xlab = "global model", ylab = "rasch model", 
     main = "Comparison of estimated latent traits (thetas)")
x <- lm(ranef(hi_m_RM_3)$id[,1]~ranef(hi_m_GLOB_1)$id[-1, 1])
abline(x)

cor.test(ranef(hi_m_GLOB_1)$id[-1, 1], ranef(hi_m_RM_3)$id[,1])

# great, so both models predict person parameters similarly

dotplot(ranef(hi_m_GLOB_1))$id
dotplot(ranef(hi_m_RM_3))$id

###### Compare item difficulties across BT and GLOBAL ----
plot(c(0, fixef(hi_m_GLOB_1)), 
     c(0, coef(hi_m_BT_1)), col = c("red", rep("black", 6)), 
     xlab = "Global model item difficulties of BT part", ylab = "Bradley-Terry model",
     main = "Item difficulties")

x <- lm(c(0, coef(hi_m_BT_1))~c(0, fixef(hi_m_GLOB_1)))
abline(x)

cor(c(0, fixef(hi_m_GLOB_1)), 
    c(0, coef(hi_m_BT_1)))
# looks perfect


###### Compare item difficulties across RM and GLOBAL ----

# png("Plots/HI/hi_m_plot_RM.png", width = 3500, height = 2200, res = 230)
plot(c(0, fixef(hi_m_GLOB_1)) + as.vector(coef(hi_m_GLOB_1)$item[,2]), 
     fixef(hi_m_RM_3), 
     xlab = "Global model item difficulties of Rasch part", ylab = "Rasch model",
     main = "Item difficulties")

x <- lm(as.numeric(fixef(hi_m_RM_3)) ~ c(0, fixef(hi_m_GLOB_1)) + as.vector(coef(hi_m_GLOB_1)$item[,2]))
abline(x)
# dev.off()
cor(c(0, fixef(hi_m_GLOB_1)) + as.vector(coef(hi_m_GLOB_1)$item[,2]), 
    fixef(hi_m_RM_3))
# finally, Item estimates are similar across both models, meaning GLOBAL model works well in estimating (possible problem is only heightened noise)


x <- beta_global
y <- beta_bt

x_c <- scale(x, center = TRUE, scale = FALSE)

m_lin <- lm(y ~ x_c)

qqnorm(residuals(m_lin))
qqline(residuals(m_lin))

m_lin <- lm(y ~ x_c)
m_quad <- lm(y ~ x_c + I(x_c^2))
anova(m_lin, m_quad)
summary(m_quad)
I(x_c^2)

plot(x_c, y,
     xlab = "Centered global model estimates",
     ylab = "BT estimates")

abline(m_lin, lwd = 2)

x_seq <- seq(min(x_c), max(x_c), length = 100)
y_quad <- predict(m_quad, newdata = data.frame(x_c = x_seq))

lines(x_seq, y_quad, lty = 2, lwd = 2)





### Compare item difficulties across RM and BT ----

rasch_b <- c(0, fixef(hi_m_GLOB_1)) + as.vector(coef(hi_m_GLOB_1)$item[,2])
rasch_se <- as.vector(arm::se.ranef(hi_m_GLOB_1)$item)   # SEs from RM
bt_b <- c(0, fixef(hi_m_GLOB_1))
bt_se <- c(0, sqrt(diag(vcov(hi_m_GLOB_1)))) # SEs for BT (0 has SE=0)


## Item names
items <- paste0("V", seq_along(bt_b))

mod <- lm(bt_b ~ rasch_b)
newx <- seq(min(rasch_b), max(rasch_b), length.out = 100)
conf <- predict(mod, newdata = data.frame(rasch_b = newx),
                interval = "confidence", level = 0.95)
preds <- predict(mod, newdata = data.frame(rasch_b = newx),
                 interval = "prediction", level = 0.95)

## Plot

r <- cor(rasch_b, bt_b)
# png("Plots/HI/hi_m_plot_comparison.png", width = 3500, height = 2200, res = 230)


plot(rasch_b, bt_b,
     xlab = "RM part item easiness",
     ylab = "BTM part item parameters",
     main = paste0("Item difficulties for males on HI (Global model)", "\n r = ", round(r, 3)),
     pch = 19)+
  arrows(rasch_b - 1.96*rasch_se, bt_b,
         rasch_b + 1.96*rasch_se, bt_b,
         angle = 90, code = 3, length = 0.05, col = "darkgray")+
  arrows(rasch_b, bt_b - 1.96*bt_se,
         rasch_b, bt_b + 1.96*bt_se,
         angle = 90, code = 3, length = 0.05, col = "darkgray")+
  text(rasch_b+0.01, bt_b, labels = items, pos = 2, cex = 0.7)+
  lines(newx, conf[, "fit"], col = "black", lwd = 2)+
  # lines(newx, preds[, "lwr"], col = "red", lty = 2)+
  # lines(newx, preds[, "upr"], col = "red", lty = 2)
  polygon(c(newx, rev(newx)),
          c(preds[, "lwr"], rev(preds[, "upr"])),
          col = rgb(0,0,1,0.05), border = NA)+
  polygon(c(newx, rev(newx)),
          c(conf[, "lwr"], rev(conf[, "upr"])),
          col = rgb(1,0,0,0.1), border = NA)

hi_m_comparison <- recordPlot()
# dev.off()
cor.test(rasch_b, bt_b)
# gosh, those CIs for BTM are enormous and we might never know, what they should really look like. I can try to compare BT_1 and RM_3 


## WARNING!!! comparison of RM and BT from their respective models

rasch_b <- fixef(hi_m_RM_3)
rasch_se <- sqrt(diag(vcov(hi_m_RM_3)))   # SEs from RM
bt_b <- c(0, coef(hi_m_BT_1))
bt_se <- c(0, sqrt(diag(vcov(hi_m_BT_1)))) # SEs for BT (0 has SE=0)

## Item names
items <- paste0("V", seq_along(bt_b))

mod <- lm(bt_b ~ rasch_b)
newx <- seq(min(rasch_b), max(rasch_b), length.out = 100)
conf <- predict(mod, newdata = data.frame(rasch_b = newx),
                interval = "confidence", level = 0.95)
preds <- predict(mod, newdata = data.frame(rasch_b = newx),
                 interval = "prediction", level = 0.95)

## Plot
# png("Plots/HI/hi_m_plot_comparison_separate.png", width = 3500, height = 2200, res = 230)


plot(rasch_b, bt_b,
     xlab = "Rasch model item difficulties",
     ylab = "Bradley-Terry model",
     main = "Item difficulties for HI",
     pch = 19)+
  arrows(rasch_b - 1.96*rasch_se, bt_b,
         rasch_b + 1.96*rasch_se, bt_b,
         angle = 90, code = 3, length = 0.05, col = "darkgray")+
  arrows(rasch_b, bt_b - 1.96*bt_se,
         rasch_b, bt_b + 1.96*bt_se,
         angle = 90, code = 3, length = 0.05, col = "darkgray")+
  text(rasch_b+0.01, bt_b, labels = items, pos = 2, cex = 0.7)+
  lines(newx, conf[, "fit"], col = "black", lwd = 2)+
  # lines(newx, preds[, "lwr"], col = "red", lty = 2)+
  # lines(newx, preds[, "upr"], col = "red", lty = 2)
  polygon(c(newx, rev(newx)),
          c(preds[, "lwr"], rev(preds[, "upr"])),
          col = rgb(0,0,1,0.05), border = NA)+
  polygon(c(newx, rev(newx)),
          c(conf[, "lwr"], rev(conf[, "upr"])),
          col = rgb(1,0,0,0.1), border = NA)

# dev.off()



# assumption of GLMM is that there are equal variances across categories on latent trait. 

x1 <- as.data.frame(confint(hi_m_GLOB_1, method = "Wald")[-c(1:2), ]) # I see, there are enormous CIs for fixed effects....
x2 <- as.data.frame(confint(hi_m_RM_3, method = "Wald") [-1, ])
x3 <- as.data.frame(confint(hi_m_BT_1, method = "Wald"))

x1$dif <- x1$`97.5 %` - x1$`2.5 %`
x2$dif <- x2$`97.5 %` - x2$`2.5 %`
x3$dif <- x3$`97.5 %` - x3$`2.5 %`


x1; x2; x3
# we could consider this assumption to be supported 


# FINAL STATEMENT FOR HI IS THAT ALTHOUGH IT FOLLOWS DESIRED DIRECTION, AND ESTIMATES TEND TO BE ON REGRESSION LINE, GLOBAL MODEL PRODUCES REALLY WIDE CIs MAKING IT HARD TO INTERPRET ON ANY DIRECTION OF THE RELATIONSHIP.
# I checked this with separately estimated RM and BT models and they produce much smaller CIs, so prolly the issue is in combining those two models into one GLMM framework.
#   - altogether, in these composition they produce really good scale estimates, which we could really see in real life 
#   - fa.parallel suggests 2 factors, but inspecting scree plot, i would report only one
#       - doing 2 factor fa, no item really binds strongly to second factor


# overall however I am kind of impressed with the estimation here







# HI analysis women ####


### Final Rasch Model only ----
# Data from our collection
hi_f_RM_1 <- glmer(r~0+(V1+V2+V3+V4+V5+V6+V7+V8)+(1|id), data=analysis_total[analysis_total$id_type==1 & analysis_total$collection == 1 & analysis_total$dataset_id == "HI" & analysis_total$sex == "female",], family = binomial, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e7)))

isSingular(hi_f_RM_1, tol = 1e-4)

summary(hi_f_RM_1) # V8 looks scary, however, no female responded that they hit their head, so that´s probably it, we could drop the item in this model

# Outsourced data collection
hi_f_RM_2 <- glmer(r~0+(V1+V2+V3+V4+V5+V6+V7+V8)+(1|id), data=analysis_total[analysis_total$id_type == 1 & analysis_total$dataset_id == "HI" & analysis_total$sex == "female" & analysis_total$collection == 2,], family = binomial)
summary(hi_f_RM_2)

# combined data collection
hi_f_RM_3 <- glmer(r~0+(V1+V2+V3+V4+V5+V6+V7+V8)+(1|id), data=analysis_total[analysis_total$id_type == 1 & analysis_total$dataset_id == "HI" & analysis_total$sex == "female",], family = binomial, control = glmerControl(optimizer = "bobyqa")) # model failed to converge, trying another optimizer
hi_f_RM_4 <- glmer(r~0+(V1+V2+V3+V4+V5+V6+V7+V8)+(1|id)+(1|collection), data=analysis_total[analysis_total$id_type == 1 & analysis_total$dataset_id == "HI" & analysis_total$sex == "female",], family = binomial, control = glmerControl(optimizer = "Nelder_Mead")) # model failed to converge, big deviation

summary(hi_f_RM_3)
dotplot(ranef(hi_f_RM_4))


anova(hi_f_RM_3, hi_f_RM_4, test = "Chisq")
# checking relation of the two item collections
cor(fixef(hi_f_RM_2), fixef(hi_f_RM_1))

cor(as.numeric(coef(hi_f_RM_2)$id[1,-1]), as.numeric(coef(hi_f_RM_1)$id[1,-1]))
plot(as.numeric(coef(hi_f_RM_2)$id[1,-1]), 
     as.numeric(coef(hi_f_RM_1)$id[1,-1]), 
     xlab = "Rasch model item difficulties of collection 2", ylab = "Rasch model item difficulties of collection 1",
     main = "Item difficulties across collections")

x <- lm(as.numeric(coef(hi_f_RM_1)$id[1,-1])~ 
          as.numeric(coef(hi_f_RM_2)$id[1,-1]))
abline(x)

# estimates are really of as no one selected r = 1 in V8 in collection 1, so it seems like it is really hard to answer correctly
# I checked the FA for and though fa.parallel reported 2 factors, I only see 1 (side note, V2 better works for women)



### Final BT model only ----
hi_f_BT_1 <- glm(r ~ 0 + (V2+V3+V4+V5+V6+V7+V8), data=analysis_total[analysis_total$id_type==0 & analysis_total$dataset_id == "HI",], family = binomial)


(var(coef(hi_f_BT_1))-mean(diag(vcov(hi_f_BT_1))))/var(coef(hi_f_BT_1))

summary(hi_f_BT_1)


MuMIn::r.squaredGLMM(hi_f_BT_1)

#                R2m       R2c
# theoretical 0.3085222 0.3085222
# delta       0.2683864 0.2683864

fmsb::NagelkerkeR2(hi_f_BT_1) # 0.3046708

# pretty small R2 actually


### Global model ----
hi_f_GLOB_1 <- glmer(r ~ 0 +  (V2+V3+V4+V5+V6+V7+V8) + (0+id_type|id) + (0+item_type|item), data = analysis_total[(analysis_total$sex == "female" | is.na(analysis_total$sex)) & analysis_total$dataset_id == "HI",], family = binomial)
hi_f_GLOB_1s <- glmer(r ~ 0 + (V2+V3+V4+V5+V6+V7+V8) + (0+item_type|item), data = analysis_total[(analysis_total$sex == "female" | is.na(analysis_total$sex)) & analysis_total$dataset_id == "HI",], family = binomial)
hi_f_GLOB_1t <- glmer(r ~ 0 + (V2+V3+V4+V5+V6+V7+V8) + (0+id_type|id), data = analysis_total[(analysis_total$sex == "female" | is.na(analysis_total$sex)) & analysis_total$dataset_id == "HI",], family = binomial)
hi_f_GLOB_1e <- glm(r ~ 0 + (V2+V3+V4+V5+V6+V7+V8), data = analysis_total[(analysis_total$sex == "female" | is.na(analysis_total$sex)) & analysis_total$dataset_id == "HI",], family = binomial)


# checking significance
plogis(fixef(hi_f_GLOB_1))
car::Anova(hi_f_GLOB_1, type=3) # significant
car::Anova(hi_f_RM_1, type=3)
car::Anova(hi_f_RM_3, type=3) # sign
car::Anova(hi_f_BT_1, type=3) # except for V7

# profiling

pr_hi_f <- profile(hi_f_GLOB_1) # estimation got stuck
splom(pr_bmpn) 

# testing two models
simp_mod_anova_hi_f <- anova(hi_f_GLOB_1, hi_f_GLOB_1s, hi_f_GLOB_1t, hi_f_GLOB_1e)


# what it looks like?
summary(hi_f_GLOB_1) # high correlations

qqmath(ranef(hi_f_GLOB_1)) # there is a difference in people (they have different thetas) and also in items (different difficulties)
qqnorm(resid(hi_f_GLOB_1),main="Residual normal plot",col=4,adj=0.1)
qqline(resid(hi_f_GLOB_1)) # predicted qqline looks really off. Looks like people on low extreme get even lower estimates and people on higher extreme get higher estimates

# model on our data collection
hi_f_GLOB_2 <- glmer(r ~ 0 + (V2+V3+V4+V5+V6+V7+V8) + (0+id_type|id) + (0+item_type|item), 
                     data = analysis_total[(analysis_total$sex == "female" | is.na(analysis_total$sex)) & analysis_total$dataset_id == "HI" & analysis_total$collection == 1,], family = binomial) # only our data collection

# pointless, really little observations + V8 issue

qqmath(ranef(hi_f_GLOB_2)) # there is a difference in people (they have different thetas) and also in items (different difficulties) although not so obvious
qqnorm(resid(hi_f_GLOB_2),main="Residual normal plot",col=4,adj=0.1)
qqline(resid(hi_f_GLOB_2)) # heavy tails


# checking model GLOB_1
performance::check_model(hi_f_GLOB_1) # high collinearity and medium amount of r estimates outside of simulated range together with influential observations, prolly due to BTM model
performance::model_performance(hi_f_GLOB_1) # R2 conditional 0.175, marginal 0.175, RMSE 0.268

# Reliability of BTM

var(fixef(hi_f_GLOB_1))/(var(fixef(hi_f_GLOB_1))+mean(diag(vcov(hi_f_GLOB_1)))) # 0.974491

# manually calculating contribution of fixed effects to variance explained

var(fixef(hi_f_GLOB_1)) / (
  as.data.frame(VarCorr(hi_f_GLOB_1))[2, "vcov"] + var(fixef(hi_f_GLOB_1))
)

# calculation of contribution to explained variance via presudo R2 for GLMM (Nakagawa & Schielzeth, 2013)

MuMIn::r.squaredGLMM(hi_f_GLOB_1) 
#                    R2m       R2c
# theoretical 0.02761371 0.8695235
# delta       0.02685714 0.8457001

MuMIn::r.squaredGLMM(hi_f_GLOB_2) 

#                   R2m       R2c
# theoretical 0.1192121 0.7091606
# delta       0.1121673 0.6672534

# looks good and is comparable to manually calculated contribution, but BTM only small amount of information

AIC(hi_f_GLOB_1, hi_f_BT_1) # pointless as we have different number of observations

# what about residuals? 
simres <- simulateResiduals(hi_f_GLOB_1)
plot(simres) # lot of data, so significant KS on deviation, otherwise ok
testOutliers(simres, type = "bootstrap") # good, no signs of outlier related misfit


pred <- predict(hi_f_GLOB_1, type = "response")
range(pred) # good separation


## compare random effect of ID across RM and GLOBAL -> persons are estimated to be the same
plot(jitter(ranef(hi_f_GLOB_1)$id[-1, 1]), 
     jitter(ranef(hi_f_RM_3)$id[,1]), 
     xlab = "global model", ylab = "rasch model", 
     main = "Comparison of estimated latent traits (thetas)")
x <- lm(ranef(hi_f_RM_3)$id[,1]~ranef(hi_f_GLOB_1)$id[-1, 1])
abline(x)
cor.test(ranef(hi_f_GLOB_1)$id[-1, 1], ranef(hi_f_RM_3)$id[,1])
# great, so both models predict person parameters similarly

dotplot(ranef(hi_f_GLOB_1))$id
dotplot(ranef(hi_f_RM_3))$id

###### Compare item difficulties across BT and GLOBAL ----
plot(c(0,fixef(hi_f_GLOB_1)), 
     c(0, coef(hi_f_BT_1)), col = c("red", rep("black", 6)), 
     xlab = "Global model item difficulties of BT part", ylab = "Bradley-Terry model",
     main = "Item difficulties")

x <- lm(c(0, coef(hi_f_BT_1))~c(0,fixef(hi_f_GLOB_1)))
abline(x)
cor(c(0,fixef(hi_f_GLOB_1)), 
    c(0, coef(hi_f_BT_1)))
# looks perfect


###### Compare item difficulties across RM and GLOBAL ----

# png("Plots/HI/hi_f_plot_RM.png", width = 3500, height = 2200, res = 230)
plot(c(0,fixef(hi_f_GLOB_1)) + as.vector(coef(hi_f_GLOB_1)$item[,2]), 
     fixef(hi_f_RM_3), 
     xlab = "Global model item difficulties of Rasch part", ylab = "Rasch model",
     main = "Item difficulties")

x <- lm(as.numeric(fixef(hi_f_RM_3)) ~ c(0,fixef(hi_f_GLOB_1)) + as.vector(coef(hi_f_GLOB_1)$item[,2]))
abline(x)

# dev.off()
cor(as.numeric(fixef(hi_f_RM_3)), c(0,fixef(hi_f_GLOB_1)) + as.vector(coef(hi_f_GLOB_1)$item[,2]))
# finally, Item estimates are similar across both models, meaning GLOBAL model works well in estimating (possible problem is only heightened noise)



### Compare item difficulties across RM and BT ----

rasch_b <- c(0,fixef(hi_f_GLOB_1))+as.vector(coef(hi_f_GLOB_1)$item[,2])
rasch_se <- as.vector(arm::se.ranef(hi_f_GLOB_1)$item)   # SEs from RM
bt_b <- c(0,fixef(hi_f_GLOB_1))
bt_se <- c(0,sqrt(diag(vcov(hi_f_GLOB_1)))) # SEs for BT (0 has SE=0)


## Item names
items <- paste0("V", seq_along(bt_b))

mod <- lm(bt_b ~ rasch_b)
newx <- seq(min(rasch_b), max(rasch_b), length.out = 100)
conf <- predict(mod, newdata = data.frame(rasch_b = newx),
                interval = "confidence", level = 0.95)
preds <- predict(mod, newdata = data.frame(rasch_b = newx),
                 interval = "prediction", level = 0.95)

## Plot

r <- cor(rasch_b, bt_b)
# png("Plots/HI/hi_f_plot_comparison.png", width = 3500, height = 2200, res = 230)

plot(rasch_b, bt_b,
     xlab = "RM part item easiness",
     ylab = "BTM part item parameters",
     main = paste0("Item difficulties for females on HI (Global model)", "\n r = ", round(r, 3)),
     pch = 19)+
  arrows(rasch_b - 1.96*rasch_se, bt_b,
         rasch_b + 1.96*rasch_se, bt_b,
         angle = 90, code = 3, length = 0.05, col = "darkgray")+
  arrows(rasch_b, bt_b - 1.96*bt_se,
         rasch_b, bt_b + 1.96*bt_se,
         angle = 90, code = 3, length = 0.05, col = "darkgray")+
  text(rasch_b+0.01, bt_b, labels = items, pos = 2, cex = 0.7)+
  lines(newx, conf[, "fit"], col = "black", lwd = 2)+
  # lines(newx, preds[, "lwr"], col = "red", lty = 2)+
  # lines(newx, preds[, "upr"], col = "red", lty = 2)
  polygon(c(newx, rev(newx)),
          c(preds[, "lwr"], rev(preds[, "upr"])),
          col = rgb(0,0,1,0.05), border = NA)+
  polygon(c(newx, rev(newx)),
          c(conf[, "lwr"], rev(conf[, "upr"])),
          col = rgb(1,0,0,0.1), border = NA)


# dev.off()
hi_f_comparison <- recordPlot()
# gosh, those CIs for BTM are enormous (although not as huge as for males) and we might never know, what they should really look like. I can try to compare BT_1 and RM_3 
cor.test(rasch_b, bt_b)

grDevices::jpeg("lolo.jpeg") 
png("Plots/LOL.png", width = 3000, height = 4500, res = 380)
dev.off()
plot.new()

## WARNING!!! comparison of RM and BT from their respective models

rasch_b <- fixef(hi_f_RM_3)
rasch_se <- sqrt(diag(vcov(hi_f_RM_3)))   # SEs from RM
bt_b <- c(0, coef(hi_f_BT_1))
bt_se <- c(0, sqrt(diag(vcov(hi_f_BT_1)))) # SEs for BT (0 has SE=0)

## Item names
items <- paste0("V", seq_along(bt_b))

mod <- lm(bt_b ~ rasch_b)
newx <- seq(min(rasch_b), max(rasch_b), length.out = 100)
conf <- predict(mod, newdata = data.frame(rasch_b = newx),
                interval = "confidence", level = 0.95)
preds <- predict(mod, newdata = data.frame(rasch_b = newx),
                 interval = "prediction", level = 0.95)

## Plot
# png("Plots/HI/hi_f_plot_comparison_separate.png", width = 3500, height = 2200, res = 230)


plot(rasch_b, bt_b,
     xlab = "Rasch model item difficulties",
     ylab = "Bradley-Terry model",
     main = "Item difficulties for HI females",
     pch = 19)+
  arrows(rasch_b - 1.96*rasch_se, bt_b,
         rasch_b + 1.96*rasch_se, bt_b,
         angle = 90, code = 3, length = 0.05, col = "darkgray")+
  arrows(rasch_b, bt_b - 1.96*bt_se,
         rasch_b, bt_b + 1.96*bt_se,
         angle = 90, code = 3, length = 0.05, col = "darkgray")+
  text(rasch_b+0.01, bt_b, labels = items, pos = 2, cex = 0.7)+
  lines(newx, conf[, "fit"], col = "black", lwd = 2)+
  # lines(newx, preds[, "lwr"], col = "red", lty = 2)+
  # lines(newx, preds[, "upr"], col = "red", lty = 2)
  polygon(c(newx, rev(newx)),
          c(preds[, "lwr"], rev(preds[, "upr"])),
          col = rgb(0,0,1,0.05), border = NA)+
  polygon(c(newx, rev(newx)),
          c(conf[, "lwr"], rev(conf[, "upr"])),
          col = rgb(1,0,0,0.1), border = NA)

# dev.off()



# assumption of GLMM is that there are equal variances across categories on latent trait. 

x1 <- as.data.frame(confint(hi_f_GLOB_1, method = "Wald")[-c(1:2), ]) # I see, there are enormous CIs for fixed effects....
x2 <- as.data.frame(confint(hi_f_RM_3, method = "Wald") [-1, ])
x3 <- as.data.frame(confint(hi_f_BT_1, method = "Wald"))

x1$dif <- x1$`97.5 %` - x1$`2.5 %`
x2$dif <- x2$`97.5 %` - x2$`2.5 %`
x3$dif <- x3$`97.5 %` - x3$`2.5 %`


x1; x2; x3
# we could consider this assumption to be supported 


# FINAL STATEMENT FOR HI females IS THAT ALTHOUGH IT FOLLOWS DESIRED DIRECTION, ESTIMATES DON´T REALLY TEND TO BE ON REGRESSION LINE, GLOBAL MODEL PRODUCES REALLY WIDE CIs MAKING IT HARD TO INTERPRET ON ANY DIRECTION OF THE RELATIONSHIP.
# I checked this with separately estimated RM and BT models and they produce much smaller CIs, so prolly the issue is in combining those two models into one GLMM framework.
#   - altogether, these estimates don´t really work so good as we would like them to...
#   - fa.parallel suggests 2 factors, but inspecting scree plot, i would report only one
#       - doing 2 factor fa, no item really binds strongly to second factor

# FUNKY PLOT CODES ####
## Checking for linear and quadratic relationship between BT and GLOBAL estimates ####

library(car)

plot_scales <- list(c(aut_GLOB_1, "AUT"),
                    c(bmpn_GLOB_1, "BMPN"),
                    c(dass_GLOB_1, "DASS"),
                    c(dem_GLOB_1, "DEM"),
                    c(ders_GLOB_1, "DERS"),
                    c(hi_m_GLOB_1, "Males on HI"),
                    c(hi_f_GLOB_1, "Females on HI"))


quadratic_rel <- function(model, scale_name) {
  
  # Rasch part
  rasch_b <- c(0, fixef(model)) + as.vector(coef(model)$item[, 2])
  rm_c <- as.numeric(scale(rasch_b, center = TRUE, scale = FALSE))
  # Bradley–Terry part
  bt_b <- c(0, fixef(model))
  
  bt_c <- as.numeric(scale(bt_b, center = TRUE, scale = FALSE))
  
  m_lin  <- lm(bt_c ~ rm_c)
  m_quad <- lm(bt_c ~ rm_c + I(rm_c^2))
  
  anova_fin <- anova(m_lin, m_quad)
  # anova_fin$scale <- scale_name
  # anova_fin$model <- rownames(anova_fin)
  rownames(anova_fin) <- NULL
  
  plot(rm_c, bt_c,
       xlab = "Centered RM estimates",
       ylab = "Centered BT estimates",
       main = scale_name)
  
  abline(m_lin, lwd = 2)
  # Quadratic curve
  x_seq <- seq(min(rm_c), max(rm_c), length.out = 100)
  y_quad <- predict(m_quad, newdata = data.frame(rm_c = x_seq))
  
  lines(x_seq, y_quad, lwd = 2, lty = 2)
  
  
  # Plot
  qqnorm(residuals(m_lin))
  qqline(residuals(m_lin))
  
  wald_test <- linearHypothesis(
    m_lin, "rm_c = -1"
  )
  
  wald_tab <- as.data.frame(wald_test)
  wald_tab$scale <- scale_name
  
  return(list(
    anova_quad = anova_fin,
    chi_identity = wald_test
  ))
}

# pdf("Plots/linrel_testing.pdf", width = 8.3, height = 11.7)

layout(matrix(1:8, nrow = 4, ncol = 2, byrow = TRUE))

results <- vector("list", length(plot_scales))

for (i in seq_along(plot_scales)) {
  results[[i]] <- quadratic_rel(
    plot_scales[[i]][[1]],
    plot_scales[[i]][[2]]
  )
}


# dev.off()


# does regression with quadratic term fit better than linear one?
anova_all <- do.call(
  rbind,
  lapply(results, `[[`, "anova_quad")
)


# do we have perfect identity between BT and RM estimates?
chi_all <- do.call(
  rbind,
  lapply(results, `[[`, "chi_identity")
)

write.csv(anova_all, "Tables/quadratic_anova_results.csv", row.names = FALSE)
write.csv(chi_all, "Tables/chi_identity_wald_results.csv", row.names = FALSE)



## Comparing BT and GLOBAL estimations ####

plot_BT_est <- list(
  list(aut_BT_1,  aut_GLOB_1,  "AUT"),
  list(bmpn_BT_1, bmpn_GLOB_1, "BMPN"),
  list(dass_BT_1, dass_GLOB_1, "DASS"),
  list(dem_BT_1,  dem_GLOB_1,  "DEM"),
  list(ders_BT_1, ders_GLOB_1, "DERS"),
  list(hi_m_BT_1, hi_m_GLOB_1, "HI")
)

plot_BT_estimation <- function(BT, GLOBAL, scale_name) {
  
  # BTM
  bt_b  <- c(0, coef(BT))
  bt_se <- c(0, arm::se.coef(BT))
  
  # Rasch part of GM
  global_b  <- c(0, fixef(GLOBAL))
  global_se <- c(0, arm::se.fixef(GLOBAL))
  
  correlation <- cor(bt_b, 
                     global_b)
  
  plot(global_b, bt_b,
       xlim = range(c(0, global_b, bt_b)),
       ylim = range(c(0, global_b, bt_b)),
       col = c("red", rep("black", 7)), 
       xlab = "Fixed effects of Global Model", ylab = "Separate Bradley-Terry model",
       main = paste0("Comparison of BT part estimation of ", scale_name, "\n Correlation = ", round(correlation, 3)))
  
  
  arrows(global_b - 1.96*global_se, bt_b,
         global_b + 1.96*global_se, bt_b,
         angle = 90, code = 3, length = 0.05, col = "darkgray")
  
  arrows(global_b, bt_b - 1.96*bt_se,
         global_b, bt_b + 1.96*bt_se,
         angle = 90, code = 3, length = 0.05, col = "darkgray")
  
  line <- lm(bt_b ~ global_b)
  abline(line)
  
}

# png("Plots/BT_part_estimation.png", width = 2500, height = 3500, res = 300)
layout(matrix(1:6, nrow = 3, ncol = 2, byrow = TRUE))

for (p in plot_BT_est) {
  plot_BT_estimation(p[[1]], p[[2]], p[[3]]) }

# dev.off()

## Comparing RM and GLOBAL estimations ####

plot_RM_est <- list(
  list(aut_RM_3,  aut_GLOB_1,  "AUT"),
  list(bmpn_RM_3, bmpn_GLOB_1, "BMPN"),
  list(dass_RM_3, dass_GLOB_1, "DASS"),
  list(dem_RM_3,  dem_GLOB_1,  "DEM"),
  list(ders_RM_3, ders_GLOB_1, "DERS"),
  list(hi_m_RM_3, hi_m_GLOB_1, "males on HI"),
  list(hi_m_RM_3, hi_m_GLOB_1, "females on HI")
)


plot_RM_estimation <- function(RM, GLOBAL, scale_name) {
  
  # RM
  rasch_b  <- fixef(RM)
  rasch_se <- arm::se.fixef(RM)
  
  # Rasch part of GM
  global_b  <- c(0, fixef(GLOBAL)) + as.vector(coef(GLOBAL)$item[,2])
  global_se <- as.vector(arm::se.ranef(GLOBAL)$item)
  
  correlation <- cor(as.numeric(fixef(RM)), 
                     c(0,fixef(GLOBAL)) + as.vector(coef(GLOBAL)$item[,2]))
  
  plot(global_b, rasch_b,
       xlim = range(c(0, global_b, rasch_b)),
       ylim = range(c(0, global_b, rasch_b)),
       col = c("red", rep("black", 7)), 
       xlab = "Random effects of Global Model", ylab = "Separate Rasch model",
       main = paste0("Comparison of RM part estimation of ", scale_name, "\n Correlation = ", round(correlation, 3)))
  
  
  arrows(global_b - 1.96*global_se, rasch_b,
         global_b + 1.96*global_se, rasch_b,
         angle = 90, code = 3, length = 0.05, col = "darkgray")
  
  arrows(global_b, rasch_b - 1.96*rasch_se,
         global_b, rasch_b + 1.96*rasch_se,
         angle = 90, code = 3, length = 0.05, col = "darkgray")
  
  line <- lm(rasch_b ~ global_b)
  abline(line)
  
}

# png("Plots/RM_part_estimation.png", width = 2500, height = 3500, res = 300)
layout(matrix(1:8, nrow = 4, ncol = 2, byrow = TRUE))

for (p in plot_RM_est) {
  plot_RM_estimation(p[[1]], p[[2]], p[[3]]) }

# dev.off()


## Final comparisons between estimates from RM and BT parts of GLOBAL model ####

plot_scales <- list(c(aut_GLOB_1, "AUT"),
                    c(bmpn_GLOB_1, "BMPN"),
                    c(dass_GLOB_1, "DASS"),
                    c(dem_GLOB_1, "DEM"),
                    c(ders_GLOB_1, "DERS"),
                    c(hi_m_GLOB_1, "males on HI"),
                    c(hi_f_GLOB_1, "females on HI"))

plot_item_difficulties <- function(model, scale_name) {
  
  # Rasch part
  rasch_b  <- c(0, fixef(model)) + as.vector(coef(model)$item[,2])
  rasch_se <- as.vector(arm::se.ranef(model)$item)
  
  # Bradley–Terry part
  bt_b  <- c(0, fixef(model))
  bt_se <- c(0, sqrt(diag(vcov(model))))   # SE for intercept is 0
  
  # Item names
  items <- paste0("V", seq_along(bt_b))
  
  # Regression
  mod  <- lm(bt_b ~ rasch_b)
  newx <- seq(min(rasch_b), max(rasch_b), length.out = 100)
  conf <- predict(mod, newdata = data.frame(rasch_b = newx),
                  interval = "confidence", level = 0.95)
  preds <- predict(mod, newdata = data.frame(rasch_b = newx),
                   interval = "prediction", level = 0.95)
  
  # Correlation
  r <- cor.test(rasch_b, bt_b)
  r_coef <- r$estimate
  r_conf_low <- r$conf.int[1]
  r_conf_high <- r$conf.int[2]
  
  # Plot
  plot(rasch_b, bt_b,
       xlab = "RM part item easiness",
       ylab = "BTM part item parameters",
       main = paste0("Item difficulties for ", scale_name, " (Global model)",
                     "\n r = ", round(r_coef, 2), "[", round(r_conf_low, 2), ", ", round(r_conf_high, 2), "]"),
       pch = 19)
  
  arrows(rasch_b - 1.96*rasch_se, bt_b,
         rasch_b + 1.96*rasch_se, bt_b,
         angle = 90, code = 3, length = 0.05, col = "darkgray")
  
  arrows(rasch_b, bt_b - 1.96*bt_se,
         rasch_b, bt_b + 1.96*bt_se,
         angle = 90, code = 3, length = 0.05, col = "darkgray")
  
  text(rasch_b + 0.01, bt_b, labels = items, pos = 2, cex = 0.7)
  
  lines(newx, conf[, "fit"], col = "black", lwd = 2)
  
  polygon(c(newx, rev(newx)),
          c(preds[, "lwr"], rev(preds[, "upr"])),
          col = rgb(0,0,1,0.05), border = NA)
  
  polygon(c(newx, rev(newx)),
          c(conf[, "lwr"], rev(conf[, "upr"])),
          col = rgb(1,0,0,0.1), border = NA)
  
  lines(lowess(rasch_b, bt_b), col = "darkred", lwd = 0.5, lty = 2)
}

# png("Plots/all_comparisons.png", width = 2500, height = 3500, res = 300)
layout(matrix(1:8, nrow = 4, ncol = 2, byrow = TRUE))

for (p in plot_scales) {
  plot_item_difficulties(p[[1]], p[[2]]) }

   dev.off()

## Final comparisons between estimates from RM and BT separate models ####

plot_scales_separate <- list(list(aut_RM_3, aut_BT_1, "AUT"),
                    list(bmpn_RM_3, bmpn_BT_1, "BMPN"),
                    list(dass_RM_3, dass_BT_1, "DASS"),
                    list(dem_RM_3, dem_BT_1, "DEM"),
                    list(ders_RM_3, ders_BT_1, "DERS"),
                    list(hi_m_RM_3, hi_m_BT_1, "males on HI"),
                    list(hi_f_RM_3, hi_f_BT_1, "females on HI"))



plot_item_difficulties_separate <- function(RM, BT, scale_name) {
  
  # Rasch part
  rasch_b  <- fixef(RM)
  rasch_se <- arm::se.fixef(RM)
  
  # Bradley–Terry part
  bt_b  <- c(0, coef(BT))
  bt_se <- c(0, arm::se.coef(BT))   # SE for intercept is 0
  
  # Item names
  items <- paste0("V", seq_along(bt_b))
  
  # Regression
  mod  <- lm(bt_b ~ rasch_b)
  newx <- seq(min(rasch_b), max(rasch_b), length.out = 100)
  conf <- predict(mod, newdata = data.frame(rasch_b = newx),
                  interval = "confidence", level = 0.95)
  preds <- predict(mod, newdata = data.frame(rasch_b = newx),
                   interval = "prediction", level = 0.95)
  
  # Correlation
  r <- cor(rasch_b, bt_b)
  
  # Plot
  plot(rasch_b, bt_b,
       xlab = "RM item easiness",
       ylab = "BTM item parameters",
       main = paste0("Item difficulties for ", scale_name, " (Global model)",
                     "\n r = ", round(r, 3)),
       pch = 19)
  
  arrows(rasch_b - 1.96*rasch_se, bt_b,
         rasch_b + 1.96*rasch_se, bt_b,
         angle = 90, code = 3, length = 0.05, col = "darkgray")
  
  arrows(rasch_b, bt_b - 1.96*bt_se,
         rasch_b, bt_b + 1.96*bt_se,
         angle = 90, code = 3, length = 0.05, col = "darkgray")
  
  text(rasch_b + 0.01, bt_b, labels = items, pos = 2, cex = 0.7)
  
  lines(newx, conf[, "fit"], col = "black", lwd = 2)
  
  polygon(c(newx, rev(newx)),
          c(preds[, "lwr"], rev(preds[, "upr"])),
          col = rgb(0,0,1,0.05), border = NA)
  
  polygon(c(newx, rev(newx)),
          c(conf[, "lwr"], rev(conf[, "upr"])),
          col = rgb(1,0,0,0.1), border = NA)
  
  lines(lowess(rasch_b, bt_b), col = "darkred", lwd = 0.5, lty = 2)
}



# png("Plots/all_comparisons_separate.png", width = 2500, height = 3500, res = 300)
layout(matrix(1:8, nrow = 4, ncol = 2, byrow = TRUE))

for (p in plot_scales_separate) {
  plot_item_difficulties_separate(p[[1]], p[[2]], p[[3]]) }

# dev.off()


# TABLES ####

## Table for model fit of global models compared to simpler models ####
anova_list <- list(
  AUT  = simp_mod_anova_aut,
  BMPN = simp_mod_anova_bmpn,
  DASS = simp_mod_anova_dass,
  DEM  = simp_mod_anova_dem,
  DERS = simp_mod_anova_ders,
  HI_M = simp_mod_anova_hi_m,
  HI_F = simp_mod_anova_hi_f
)

anova_tables <- lapply(names(anova_list), function(nm) {
  df <- as.data.frame(anova_list[[nm]])
  df$model <- rownames(df)
  df$scale <- nm
  rownames(df) <- NULL
  df
})

anova_tables <- do.call(rbind, anova_tables)

anova_tables$`Pr(>Chisq)` <- ifelse(
  anova_tables$`Pr(>Chisq)` < .001,
  "< .001",
  round(anova_tables$`Pr(>Chisq)`, 3)
)

anova_tables <- anova_tables[, c(
  "scale", "model",
  "npar", "AIC", "BIC", "logLik",
  "-2*log(L)", "Chisq", "Df", "Pr(>Chisq)"
)]

write.csv(anova_tables, "Tables/anova_model_comparisons.csv", row.names = FALSE)

## Table 2 - Parameter estimates of global models ####

extract_rm_bt_table <- function(plot_scales, max_items = 8) {
  
  require(tidyr)
  require(arm)
  
  all_items <- paste0("V", seq_len(max_items))
  
  all_long <- lapply(plot_scales, function(p) {
    
    model <- p[[1]]
    scale_name <- p[[2]]
    
    
    ## Random effects (shared)
    
    vc <- VarCorr(model)
    
    var_item <- as.numeric(vc$item)
    sd_item  <- sqrt(var_item)
    
    var_id   <- as.numeric(vc$id)
    sd_id    <- sqrt(var_id)
    
    
    ## Rasch part (RM)
    
    rm_items <- rownames(coef(model)$item)
    rm_est   <- fixef(model)[1] + coef(model)$item[, 2]
    rm_se    <- as.vector(arm::se.ranef(model)$item)
    
    rm_tab <- data.frame(
      scale = scale_name,
      model = "RM",
      item  = rm_items,
      est   = rm_est,
      se    = rm_se,
      var_item = var_item,
      sd_item  = sd_item,
      var_id   = var_id,
      sd_id    = sd_id,
      stringsAsFactors = FALSE
    )
    
    
    ## Bradley–Terry part (BTM)
    
    bt_items <- c("V1", names(fixef(model)))
    bt_est   <- c(0, fixef(model))
    bt_se    <- c(0, sqrt(diag(vcov(model))))
    
    bt_tab <- data.frame(
      scale = scale_name,
      model = "BTM",
      item  = bt_items,
      est   = bt_est,
      se    = bt_se,
      var_item = var_item,
      sd_item  = sd_item,
      var_id   = var_id,
      sd_id    = sd_id,
      stringsAsFactors = FALSE
    )
    
    rbind(rm_tab, bt_tab)
  })
  
  long_tab <- do.call(rbind, all_long)
  
  ## ---- enforce full item grid
  long_tab$item <- factor(long_tab$item, levels = all_items)
  
  wide_tab <- pivot_wider(
    long_tab,
    id_cols = c(
      scale, model,
      var_item, sd_item,
      var_id, sd_id
    ),
    names_from = item,
    values_from = c(est, se),
    names_glue = "{item}_{.value}"
  )
  
  ## ---- order columns
  wide_tab <- wide_tab[, c(
    "scale", "model",
    "var_item", "sd_item",
    "var_id", "sd_id",
    as.vector(rbind(
      paste0(all_items, "_est"),
      paste0(all_items, "_se")
    ))
  )]
  
  return(wide_tab)
}


param_table <- extract_rm_bt_table(plot_scales)

param_table

write.csv(param_table, "Tables/parameter_estimates_global_models.csv", row.names = FALSE)


# NOTES ####
  # SELECTION OF ITEMS
    # Autonomy items from original dataset AS_1PL: all
    # BMPN items from original dataset CzG: bmpn05, bmpn10, bmpn11, bmpn16, bmpn18
    # DASS items from original dataset QZ: 1, 6, 8, 11, 12, 14, 18 (stress facet)
    # DEM items from original dataset 
    # DERS items from original dataset CzG: ders07, ders08, ders09, ders10, ders11, ders12, ders15
    # HI items from original dataset IGA.RDS: 1, 2, 3, 4, 5, 6, 7, 8

png("Plots/all_comparisons.png", width = 4000, height = 2000, res = 300)
layout(matrix(1:8, nrow = 4, ncol = 2, byrow = TRUE))
aut_comparisons
bmpn_comparison
dass_comparison
dem_comparison
ders_comparison
hi_m_comparison
hi_f_comparison

plot.new()

replayPlot(aut_comparisons)

plots <- list(aut_comparisons ,bmpn_comparison ,dass_comparison ,dem_comparison ,ders_comparison ,hi_m_comparison ,hi_f_comparison)
plots <- list(`aut_comparisons` ,`bmpn_comparison` ,`dass_comparison` ,`dem_comparison` ,`ders_comparison` ,`hi_m_comparison` ,`hi_f_comparison`)
plots
layout(matrix(c(1:7, NA), nrow = 2, byrow = TRUE))
for (p in plots) replayPlot(p)
for (p in plots) replayPlot(p)

quartz(width = 30, height = 15)  # macOS

grDevices::quartz.options(width = 8, height = 6,
                          pointsize = 10)



# Testing Code ####
#
chi_sq <- sum(diff^2 / var_diff)
df <- length(diff)
p_val <- pchisq(chi_sq, df = df, lower.tail = FALSE)

chi_t <- data.frame(
  Chi_square = chi_sq,
  df = df,
  p_value = p_val
)

chi_t



