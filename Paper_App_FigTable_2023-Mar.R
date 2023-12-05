# Codes for Figure and Tables in Appendix
#########################################
library(dplyr)
library(lfe)
library(clubSandwich)

rm(list = ls())
setwd("D:/water/Data")


# Table: Participants ----------------------------------------------------------
load("2Final/alldatacomb.relevant.rdata")

# Overall Participants
tmp_tbl1 <- data %>% group_by(treat, school.phase1) %>%
  summarise(participants = length(unique(Self.ID)))
tbl1 <- reshape(data.frame(tmp_tbl1), timevar = 'school.phase1', 
                    idvar = 'treat', direction = 'wide')
tbl1[nrow(tbl1)+1, ] <- c(100, colSums(tbl1[,2:ncol(tbl1)], na.rm = T))
tbl1$Total <- rowSums(tbl1[,2:ncol(tbl1)], na.rm = T)


# Baseline Participants
tmp_tbl1 <- subset(data, base.miss<=2) %>% group_by(treat, school.phase1) %>%
  summarise(participants = length(unique(Self.ID)))
tbl2 <- reshape(data.frame(tmp_tbl1), timevar = 'school.phase1', 
                idvar = 'treat', direction = 'wide')
tbl2[nrow(tbl2)+1, ] <- c(100, colSums(tbl2[,2:ncol(tbl2)], na.rm = T))
tbl2$Total <- rowSums(tbl2[,2:ncol(tbl2)], na.rm = T)


# Baseline & Treatment Participants
tmp_tbl1 <- subset(data, base.miss<=2 & treat.miss <= 2) %>% 
  group_by(treat, school.phase1) %>%
  summarise(participants = length(unique(Self.ID)))
tbl3 <- reshape(data.frame(tmp_tbl1), timevar = 'school.phase1', 
                idvar = 'treat', direction = 'wide')
tbl3[nrow(tbl3)+1, ] <- c(100, colSums(tbl3[,2:ncol(tbl3)], na.rm = T))
tbl3$Total <- rowSums(tbl3[,2:ncol(tbl3)], na.rm = T)



# Table: Discontinued in Treatment ---------------------------------------------
rm(list = ls())
load("2Final/alldatacomb.relevant.rdata")

# Process data
# Those continued in Baseline
tbldata <- within(subset(data, base.miss <= 2), {
  discont.treat <- treat.miss >2
})

tmp_tbl1 <- subset(tbldata, discont.treat==T) %>% group_by(treat) %>%
  summarise(Obs = length(unique(Self.ID)),
            discont.base.mean = round(mean(base.mean), 1))
tmp_tbl2 <- subset(tbldata, discont.treat==F) %>% group_by(treat) %>%
  summarise(Cont.base.mean = round(mean(base.mean), 1))
tbl <- merge(data.frame(tmp_tbl1), data.frame(tmp_tbl2), by='treat')
tbl[nrow(tbl)+1, ] <- c('Total', sum(tbl$Obs), 
                        round(mean(subset(tbldata, discont.treat==T)$base.mean, 
                             na.rm=T), 1),
                        round(mean(subset(tbldata, discont.treat==F)$base.mean, 
                             na.rm=T), 1))


# Test
for (i in c(1,2,3)){
  p.value <- t.test(subset(tbldata, discont.treat==T & treat==i)$base.mean,
                    subset(tbldata, discont.treat==F & treat==i)$base.mean,
                    alternative = 'two.sided', paired=F)$p.value
  print(paste0('Group ', i, ': ', p.value))
}



# Table: Correlation btw Self-reported data and Timer data ---------------------
rm(list = ls())
load("2Final/alldatacomb.relevant.rdata")

# Process data
tbldata <- subset(data, base.miss <= 2 & treat.miss <= 2)
tbldata$Reported.mean <- rowMeans(subset(tbldata, select = paste0('day', 1:14)),
                                  na.rm = T)
tbldata$Timer.mean <- rowMeans(subset(tbldata, 
                                      select = c(paste0('m', 1:30, '.phase1T'),
                                                 paste0('m', 1:30, '.phase1P'))),
                                  na.rm = T)

tbl <- tbldata %>% group_by(treat) %>%
  summarise(Reported = mean(Reported.mean, na.rm=T),
            Timer = mean(Timer.mean, na.rm=T),
            Corr = cor(Reported.mean, Timer.mean, 
                             use="pairwise.complete.obs"))
tbl <- data.frame(tbl)
tbl[nrow(tbl)+1, ] <- c(100,
                        mean(tbldata$Reported.mean, na.rm=T),
                        mean(tbldata$Timer.mean, na.rm=T),
                        cor(tbldata$Reported.mean,
                                  tbldata$Timer.mean, 
                                  use="pairwise.complete.obs"))

# Tests: Difference across group
reg1 <- lm(Reported.mean ~ Timer.mean * relevel(factor(treat), ref = 1), 
           data = subset(tbldata, is.na(Reported.mean)==F & 
                           is.na(Timer.mean)==F))
summary(reg1) # Difference vs control

# Difference btw treat 2 and 3
waldtest(reg1, ~`Timer.mean:relevel(factor(treat), ref = 1)2` - 
           `Timer.mean:relevel(factor(treat), ref = 1)3`, type = 'cluster')



# Regression: Reported Time (Different Dummies) --------------------------------
rm(list = ls())
load('2Final/sheet.reg.p1.mod.rdata')
load("2Final/alldatacomb.relevant.rdata")

# Process Data
# Only focus on those studied in the analysis (1121 students), and
# with non-missing values in gender
tbldata <- subset(data, base.miss<=2 & treat.miss<=2)
data.reg <- subset(regdata, Self.ID %in% unique(tbldata$Self.ID))

# Create new targets
tmp <- subset(data.reg, Treatment==F, select = c('Self.ID', 'sheet')) %>%
  group_by(Self.ID) %>%
  summarise(base.sheet.med = median(sheet, na.rm=T))
data.reg <- merge(data.reg, data.frame(tmp), by='Self.ID', all.x=T)

data.reg <- within(data.reg, {
  basegroup.shower.ave.greater5mins <- base.mean > 300
  basegroup.ach.less3days <- days.meet.base.norm < 3
  basegroup.shower.med.greater5mins <- base.sheet.med > 300
  schcls <- paste0(school.phase1, '_', class.phase1)
})



# Fixed effect of school, class and week
eq1 <- paste0("sheet ~ relevel(factor(treat), ref = 1) * Treatment * ",
              "basegroup.shower.ave.greater5mins", 
              "+ I(gender.phase1 == 'F') + meanC + weekend + base.mean ",
              "|schcls + week | 0 | Self.ID")
eq2 <- paste0("sheet ~ relevel(factor(treat), ref = 1) * Treatment * ",
              "basegroup.shower.med.greater5mins",
              "+ I(gender.phase1 == 'F') + meanC + weekend + base.mean ",
              "|schcls + week | 0 | Self.ID")
eq3 <- paste0("sheet ~ relevel(factor(treat), ref = 1) * Treatment * ",
              "basegroup.ach.less3days",
              "+ I(gender.phase1 == 'F') + meanC + weekend + base.mean ",
              "|schcls + week | 0 | Self.ID")

f1 <- felm(as.formula(eq1), data = data.reg)
f2 <- felm(as.formula(eq2), data = data.reg)
f3 <- felm(as.formula(eq3), data = data.reg)

fse1<- sqrt(diag(f1$clustervcv))
fse2<- sqrt(diag(f2$clustervcv))
fse3<- sqrt(diag(f3$clustervcv))

stargazer(f1, f2, f3,
          type = "html",
          se = list(fse1, fse2, fse3),
          star.char = c("*", "**", "***"),
          star.cutoffs = c(0.05, 0.01, 0.001),
          out = "result1.html")



# Regression: Reported Time (Baseline Ave Dummy)--------------------------------
rm(list = ls())
load('2Final/sheet.reg.p1.mod.rdata')
load("2Final/alldatacomb.relevant.rdata")

# Process Data
# Only focus on those studied in the analysis (1121 students), and
# with non-missing values in gender
tbldata <- subset(data, base.miss<=2 & treat.miss<=2)
data.reg <- subset(regdata, Self.ID %in% unique(tbldata$Self.ID))
data.reg <- within(data.reg, {
  schcls <- paste0(school.phase1, '_', class.phase1)
})

# Fixed effect of school, class and week
eq1 <- paste0("sheet ~ relevel(factor(treat), ref = 1) * Treatment ",
              "+ I(gender.phase1 == 'F') + meanC + weekend + base.mean ",
              "|school.phase1 + class.phase1 + week | 0 | Self.ID")
eq2 <- paste0("sheet ~ relevel(factor(treat), ref = 1) * Treatment ",
              "* I(base.mean > 300) ",
              "+ I(gender.phase1 == 'F') + meanC + weekend + base.mean ",
              "|school.phase1 + class.phase1 + week | 0 | Self.ID")
eq1 <- paste0("sheet ~ relevel(factor(treat), ref = 1) * Treatment ",
              "+ I(gender.phase1 == 'F') + meanC + weekend + base.mean ",
              "|schcls + week | 0 | Self.ID")
eq2 <- paste0("sheet ~ relevel(factor(treat), ref = 1) * Treatment ",
              "* I(base.mean > 300) ",
              "+ I(gender.phase1 == 'F') + meanC + weekend + base.mean ",
              "|schcls + week | 0 | Self.ID")

f1 <- felm(as.formula(eq1), data = data.reg)
f2 <- felm(as.formula(eq2), data = data.reg)

fse1<- sqrt(diag(f1$clustervcv))
fse2<- sqrt(diag(f2$clustervcv))

stargazer(f1, f2,
          type = "html",
          se = list(fse1, fse2),
          star.char = c("*", "**", "***"),
          star.cutoffs = c(0.05, 0.01, 0.001),
          out = "result1.html")



# Regression: Survey Willingness -----------------------------------------------
rm(list = ls())
load("2Final/alldatacomb.relevant.rdata")

# Process Data
# Subjects need to have >= 5 entries in baseline and treatment period
tbldata <- subset(data, base.miss<=2 & treat.miss<=2 & is.na(
  survey1_s2q4_tx)==F)
tbldata <- within(tbldata, {
  willing <- -3
  willing[survey1_s2q4_tx==1] <- 1
  willing[survey1_s2q4_tx==2] <- 0
  willing[survey1_s2q4_tx==3] <- -1
})

eq1 <- paste0("willing ~ relevel(factor(treat), ref = 1) + ",
              " I(gender.phase1 == 'F') + base.mean",
              "|school.phase1 + grade | 0 | Self.ID")
eq2 <- paste0("willing ~ relevel(factor(treat), ref = 1) * ",
              "I(base.mean > 300) + I(gender.phase1 == 'F') + base.mean",
              "|school.phase1 + grade | 0 | Self.ID")

f1 <- felm(as.formula(eq1), data = tbldata)
f2 <- felm(as.formula(eq2), data = tbldata)

fse1<- sqrt(diag(f1$clustervcv))
fse2<- sqrt(diag(f2$clustervcv))

stargazer(f1, f2,
          type = "html",
          se = list(fse1, fse2),
          star.char = c("*", "**", "***"),
          star.cutoffs = c(0.05, 0.01, 0.001),
          out = "result1.html")



# Table: Survey Show Knowledge -------------------------------------------------
rm(list = ls())
load("2Final/alldatacomb.relevant.rdata")

# Process Data
# Subjects need to have >= 5 entries in baseline and treatment period
tbldata <- subset(data, base.miss<=2 & treat.miss<=2 & (
  is.na(survey1_s1q1ii_tx)==F|is.na(survey1_s1q1iii_tx)==F))
tbldata <- within(tbldata, {
  show.water.ans <- survey1_s1q1ii_tx == 2
  show.save.ans <- survey1_s1q1iii_tx == '1'
})


tmp_tbl1 <- subset(tbldata, is.na(survey1_s1q1ii_tx)==F) %>% 
  group_by(treat) %>%
  summarise(
    water.ans = round(sum(show.water.ans==T, na.rm=T)/sum(
      is.na(show.water.ans)==F), 3),
    save.ans = round(sum(show.save.ans==T, na.rm=T)/sum(
      is.na(show.save.ans)==F), 3)
    )
tbl <- data.frame(tmp_tbl1)
tbl[nrow(tbl) + 1, ] <- c('Total', 
                          round(sum(tbldata$show.water.ans==T, na.rm=T)/sum(
                            is.na(tbldata$show.water.ans)==F), 3),
                          round(sum(tbldata$show.save.ans==T, na.rm=T)/sum(
                            is.na(tbldata$show.save.ans)==F), 3))







