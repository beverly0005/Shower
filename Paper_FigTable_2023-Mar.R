# Codes for Figure and Tables
####################################
library(dplyr)
library(lfe)

rm(list = ls())
setwd("D:/water/Data")



# Table 1: Demographics & Pledge Rate ----------------------------------
load("2Final/alldatacomb.relevant.rdata")
load("2Final/private.pledge.rdata")
load("2Final/public.pledge.rdata")


# Process Data
# Subjects need to have >= 5 entries in baseline and treatment period
tbldata <- subset(data, base.miss<=2 & treat.miss<=2)
tbldata <- within(tbldata, {
  schclass <- paste0(school.phase1, '_', class.phase1)
  base.fill.per <- 1-base.miss/7
  treat.fill.per <- 1-treat.miss/7
  female <- gender.phase1 == 'F'
  hdb <- house.type=='hdb'
})
private.pledge <- within(private.pledge, {
  pledge.rate <- `No. Completion`/`No. Analyzed in Phase 1`
})
public.pledge <- within(public.pledge, {
  pledge.rate <- `No. Completion`/`No. Analyzed in Phase 1`
})

# Generate demographics
tmp_tbl1 <- tbldata %>% group_by(treat) %>%
  summarise(Obs = length(unique(Self.ID)),
            Class_Num = length(unique(schclass)),
            Baseline_Entries = round(mean(base.fill.per, na.rm=T),3),
            Treatment_Entries = round(mean(treat.fill.per, na.rm=T),3),
            Female = round(sum(gender.phase1=='F', na.rm=T)/sum(
              is.na(gender.phase1)==F), 3),
            Age = round(mean(grade, na.rm=T) + 6, 1),
            HDB = round(sum(house.type=='hdb', na.rm=T)/sum(
              is.na(house.type)==F), 2),
            Occupants = round(mean(occupants, na.rm=T), 1))

tmp_tbl2 <- tbldata %>% 
  summarise(Obs = length(unique(Self.ID)),
            Class_Num = length(unique(schclass)),
            Baseline_Entries = round(mean(base.fill.per, na.rm=T),3),
            Treatment_Entries = round(mean(treat.fill.per, na.rm=T),3),
            Female = round(sum(gender.phase1=='F', na.rm=T)/sum(
              is.na(gender.phase1)==F), 3),
            Age = round(mean(grade, na.rm=T) + 6, 1),
            HDB = round(sum(house.type=='hdb', na.rm=T)/sum(is.na(house.type)==F), 2),
            Occupants = round(mean(occupants, na.rm=T), 1))
tmp_tbl2 <- cbind(data.frame(treat=c('Total')), tmp_tbl2)

# Pledge data is not accurate in alldatacomb.relevant.rdata,
# check separate pledge data instead
private.rate.ave <- round(sum(private.pledge$`No. Completion`)/sum(
  private.pledge$`No. Analyzed in Phase 1`), 2)
public.rate.ave <- round(sum(public.pledge$`No. Completion`)/sum(
  public.pledge$`No. Analyzed in Phase 1`), 2)
tmp_tbl3 <- data.frame(treat = c(1,2,3, 'Total'),
                       Pledge_Rate = c(
                         NA, private.rate.ave, public.rate.ave,
                         mean(cbind(private.rate.ave, public.rate.ave))))

# Combine
tbl <- rbind(tmp_tbl1, tmp_tbl2)
tbl <- merge(tbl, tmp_tbl3, by='treat')

# Tests (Proportion tests)-----------------------------
# Gender
for (i in c(1,2)){
  for (j in c(i+1, 3)){
    num = c(sum(subset(tbldata, treat==i)$female, na.rm = T), 
            sum(subset(tbldata, treat==j)$female, na.rm = T))
    pop = c(nrow(subset(tbldata, treat==i & is.na(gender.phase1)==F)),
            nrow(subset(tbldata, treat==j & is.na(gender.phase1)==F)))
    p.value = prop.test(num, pop, alternative = 'two.sided')$p.value
    print(paste0('Test:', i, ' vs ', j, ': ', p.value))
  }
} # min: 0.176709993283356


# House type - HDB
for (i in c(1,2)){
  for (j in c(i+1, 3)){
    num = c(sum(subset(tbldata, treat==i)$hdb, na.rm = T), 
            sum(subset(tbldata, treat==j)$hdb, na.rm = T))
    pop = c(nrow(subset(tbldata, treat==i & is.na(house.type)==F)),
            nrow(subset(tbldata, treat==j & is.na(house.type)==F)))
    p.value = prop.test(num, pop, alternative = 'two.sided')$p.value
    print(paste0('Test:', i, ' vs ', j, ': ', p.value))
  }
} # min: 0.0616047871072919


# Occupants
for (i in c(1,2)){
  for (j in c(i+1, 3)){
    p.value = t.test(subset(tbldata, treat==i)$occupants, 
                       subset(tbldata, treat==j)$occupants, 
                       alternative = 'two.sided', paired = F)$p.value
    print(paste0('Test:', i, ' vs ', j, ': ', p.value))
  }
} # min: 0.275551582617608


# Pledge rate
num <- c(sum(private.pledge$`No. Completion`), 
         sum(public.pledge$`No. Completion`))
pop <- c(sum(private.pledge$`No. Analyzed in Phase 1`),
         sum(public.pledge$`No. Analyzed in Phase 1`))
prop.test(num, pop, alternative = 'two.sided') #p=0.1772




# Table 2: Behavior Change ----------------------------------------------------
rm(list = ls())
load("2Final/alldatacomb.relevant.rdata")

# Process Data
# Subjects need to have >= 5 entries in baseline and treatment period
tbldata <- subset(data, base.miss<=2 & treat.miss<=2)
tbldata <- within(tbldata, {
  diff.days.ach <- days.meet.treat.norm - days.meet.base.norm
})

tmp_tbl1 <- tbldata %>% group_by(treat) %>%
  summarise(Base_Goal_Ach = round(sum(base.mean <= 300) / length(
              unique(Self.ID)), 2),
            Base_Day_Ach = round(mean(days.meet.base.norm, na.rm=T), 1),
            Base_Shower = round(mean(base.mean), 1),
            Treat_Goal_Ach = round(sum(treat.mean <= 300) / length(
              unique(Self.ID)), 2),
            Treat_Day_Ach = round(mean(days.meet.treat.norm, na.rm=T), 1),
            Treat_Shower = round(mean(treat.mean), 1))

tmp_tbl2 <- tbldata %>%
  summarise(Base_Goal_Ach = round(sum(base.mean <= 300) / length(
    unique(Self.ID)), 2),
    Base_Day_Ach = round(mean(days.meet.base.norm, na.rm=T), 1),
    Base_Shower = round(mean(base.mean), 1),
    Treat_Goal_Ach = round(sum(treat.mean <= 300) / length(
      unique(Self.ID)), 2),
    Treat_Day_Ach = round(mean(days.meet.treat.norm, na.rm=T), 1),
    Treat_Shower = round(mean(treat.mean), 1))
tmp_tbl2 <- cbind(data.frame(treat=c('Total')), tmp_tbl2)

tbl <- rbind(tmp_tbl1, tmp_tbl2)


# Tests -----------------------------
# Goal achievement: Difference across groups in baseline period
tmp <- tbldata %>% group_by(treat) %>%
  summarise(base.meet = sum(base.mean <= 300),
            base.pop = length(unique(Self.ID)))
num <- tmp$base.meet
pop <- tmp$base.pop
prop.test(num, pop, alternative = 'two.sided')


# Goal achievement: change in goal achievement across periods
tmp <- tbldata %>% group_by(treat) %>%
  summarise(base.meet = sum(base.mean <= 300),
            treat.meet = sum(treat.mean <= 300),
            base.pop = length(unique(Self.ID)))
for (i in c(1,2,3)){
  num <- c(subset(tmp, treat==i)$base.meet,
           subset(tmp, treat==i)$treat.meet)
  pop <- c(subset(tmp, treat==i)$base.pop,
           subset(tmp, treat==i)$base.pop)
  p.value <- prop.test(num, pop, alternative = 'two.sided')$p.value
  print(paste0('Group ', i, ': ', p.value))
}


# Goal achievement: change of goal achievement (vs. Control) across periods
tmp <- tbldata %>% group_by(treat) %>%
  summarise(base.meet = sum(base.mean <= 300),
            treat.meet = sum(treat.mean <= 300),
            base.pop = length(unique(Self.ID)))
tmp <- within(tmp, {
  diff <- treat.meet - base.meet
})
num <- tmp$diff
pop <- tmp$base.pop
for (i in c(2,3)){
  p.value <- prop.test(num[c(1,i)], pop[c(1,i)], 
                       alternative = 'two.sided')$p.value
  print(paste0('Group: ', i, 'vs. Control: ', p.value))
}


# Day achievement: Difference across groups in baseline period
for (i in c(1,2)){
  for (j in c(i+1, 3)){
    p.value <- t.test(subset(tbldata, treat == i)$days.meet.base.norm,
                      subset(tbldata, treat == j)$days.meet.base.norm,
                      alternative = 'two.sided')$p.value
    print(paste0('Group ', i, ' vs ', j, ': ', p.value))
  }
}


# Day achievement: Difference across periods
for (i in c(1,2,3)){
  p.value <- t.test(subset(tbldata, treat==i)$days.meet.base.norm,
                    subset(tbldata, treat==i)$days.meet.treat.norm,
                    alternative = 'two.sided', paired = T)$p.value
  print(paste0('Days achievement acorss periods: Group ', i, ': ', p.value))
}


# Day achievement: Different in change (vs. control) across periods
tmp <- within(tbldata, {
  diff.days.ach <- days.meet.treat.norm - days.meet.base.norm
})
for (i in c(2,3)){
  p.value <- t.test(subset(tmp, treat==1)$diff.days.ach,
                    subset(tmp, treat==i)$diff.days.ach,
                    alternative = 'two.sided', paired = F)$p.value
  print(paste0('Group: ', i, 'vs. Control: ', p.value))
}


# Average shower time: Difference in baseline period
for (i in c(1,2)){
  for (j in c(i+1, 3)){
    p.value <- t.test(subset(tbldata, treat==i)$base.mean,
                      subset(tbldata, treat==j)$base.mean,
                      alternative = 'two.sided', paired = F)$p.value
    print(paste0('Group ', i, ' vs ', j, ': ', p.value))
  }
}


# Average shower time: Difference across periods
for (i in c(1,2,3)){
  p.value <- t.test(subset(tbldata, treat==i)$base.mean,
                    subset(tbldata, treat==i)$treat.mean,
                    alternative = 'two.sided', paired = T)$p.value
  print(paste0('Shower time acorss periods: Group ', i, ': ', p.value))
}


# Average shower time: Different (vs. control) in treatment period
tmp <- within(tbldata, {
  diff <- base.mean - treat.mean
})
for (i in c(2,3)){
  p.value <- t.test(subset(tmp, treat==1)$diff,
                    subset(tmp, treat==i)$diff,
                    alternative = 'two.sided', paired = F)$p.value
  print(paste0('Group: ', i, ' vs. Control: ', p.value))
}



# Figure: Shower time trend ---------------------------------------------------
rm(list = ls())
load("2Final/alldatacomb.relevant.rdata")

# Process Data
# Subjects need to have >= 5 entries in baseline and treatment period
tbldata <- subset(data, base.miss<=2 & treat.miss<=2)

tmp <- within(tbldata, {
  day1.change <- day1 - base.mean
  day2.change <- day2 - base.mean
  day3.change <- day3 - base.mean
  day4.change <- day4 - base.mean
  day5.change <- day5 - base.mean
  day6.change <- day6 - base.mean
  day7.change <- day7 - base.mean
  day8.change <- day8 - base.mean
  day9.change <- day9 - base.mean
  day10.change <- day10 - base.mean
  day11.change <- day11 - base.mean
  day12.change <- day12 - base.mean
  day13.change <- day13 - base.mean
  day14.change <- day14 - base.mean
})

figdata1 <- tmp %>% group_by(treat) %>%
  summarise(day1.change = round(mean(day1.change, na.rm=T), 1),
            day2.change = round(mean(day2.change, na.rm=T), 1),
            day3.change = round(mean(day3.change, na.rm=T), 1),
            day4.change = round(mean(day4.change, na.rm=T), 1),
            day5.change = round(mean(day5.change, na.rm=T), 1),
            day6.change = round(mean(day6.change, na.rm=T), 1),
            day7.change = round(mean(day7.change, na.rm=T), 1),
            day8.change = round(mean(day8.change, na.rm=T), 1),
            day9.change = round(mean(day9.change, na.rm=T), 1),
            day10.change = round(mean(day10.change, na.rm=T), 1),
            day11.change = round(mean(day11.change, na.rm=T), 1),
            day12.change = round(mean(day12.change, na.rm=T), 1),
            day13.change = round(mean(day13.change, na.rm=T), 1),
            day14.change = round(mean(day14.change, na.rm=T), 1))

figdata2 <- subset(tmp, base.mean <= 300) %>% group_by(treat) %>%
  summarise(day1.change = round(mean(day1.change, na.rm=T), 1),
            day2.change = round(mean(day2.change, na.rm=T), 1),
            day3.change = round(mean(day3.change, na.rm=T), 1),
            day4.change = round(mean(day4.change, na.rm=T), 1),
            day5.change = round(mean(day5.change, na.rm=T), 1),
            day6.change = round(mean(day6.change, na.rm=T), 1),
            day7.change = round(mean(day7.change, na.rm=T), 1),
            day8.change = round(mean(day8.change, na.rm=T), 1),
            day9.change = round(mean(day9.change, na.rm=T), 1),
            day10.change = round(mean(day10.change, na.rm=T), 1),
            day11.change = round(mean(day11.change, na.rm=T), 1),
            day12.change = round(mean(day12.change, na.rm=T), 1),
            day13.change = round(mean(day13.change, na.rm=T), 1),
            day14.change = round(mean(day14.change, na.rm=T), 1))

figdata3 <- subset(tmp, base.mean > 300) %>% group_by(treat) %>%
  summarise(day1.change = round(mean(day1.change, na.rm=T), 1),
            day2.change = round(mean(day2.change, na.rm=T), 1),
            day3.change = round(mean(day3.change, na.rm=T), 1),
            day4.change = round(mean(day4.change, na.rm=T), 1),
            day5.change = round(mean(day5.change, na.rm=T), 1),
            day6.change = round(mean(day6.change, na.rm=T), 1),
            day7.change = round(mean(day7.change, na.rm=T), 1),
            day8.change = round(mean(day8.change, na.rm=T), 1),
            day9.change = round(mean(day9.change, na.rm=T), 1),
            day10.change = round(mean(day10.change, na.rm=T), 1),
            day11.change = round(mean(day11.change, na.rm=T), 1),
            day12.change = round(mean(day12.change, na.rm=T), 1),
            day13.change = round(mean(day13.change, na.rm=T), 1),
            day14.change = round(mean(day14.change, na.rm=T), 1))



# Regression ------------------------------------------------------------------
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


# Check p-value to rewrite star symbols
summary(f1)
summary(f2)


# Participants in different group
tmp_tbl1 <- aggregate(Self.ID ~ treat, data=data.reg, 
                      function(x) length(unique(x)))
tmp_tbl2 <- aggregate(Self.ID ~ treat, data=subset(data.reg, base.mean <= 300), 
                      function(x) length(unique(x)))
tmp_tbl3 <- aggregate(Self.ID ~ treat, data=subset(data.reg, base.mean > 300), 
                      function(x) length(unique(x)))
tbl <- merge(tmp_tbl1, tmp_tbl2, by='treat')
tbl <- merge(tbl, tmp_tbl3, by='treat')
names(tbl) <- c('Treatment', 'Overall', 'Base.meet5mins', 'Base.notmeet5mins')


# Coefficient test (p value): overall effect
waldtest(f1, ~`TreatmentTRUE` + `relevel(factor(treat), ref = 1)2:TreatmentTRUE`,
         type = 'cluster')
waldtest(f1, ~`TreatmentTRUE` + `relevel(factor(treat), ref = 1)3:TreatmentTRUE`,
         type = 'cluster')
waldtest(f2, ~`TreatmentTRUE` + `relevel(factor(treat), ref = 1)2:TreatmentTRUE`,
         type = 'cluster')
waldtest(f2, ~`TreatmentTRUE` + `relevel(factor(treat), ref = 1)3:TreatmentTRUE`,
         type = 'cluster')
waldtest(f2, ~`TreatmentTRUE` + `TreatmentTRUE:I(base.mean > 300)TRUE`,
         type = 'cluster')
waldtest(f2, ~`TreatmentTRUE` + `TreatmentTRUE:I(base.mean > 300)TRUE` + 
           `relevel(factor(treat), ref = 1)2:TreatmentTRUE` +
           `relevel(factor(treat), ref = 1)2:TreatmentTRUE:I(base.mean > 300)TRUE`,
         type = 'cluster')
waldtest(f2, ~`TreatmentTRUE` + `TreatmentTRUE:I(base.mean > 300)TRUE` + 
           `relevel(factor(treat), ref = 1)3:TreatmentTRUE` +
           `relevel(factor(treat), ref = 1)3:TreatmentTRUE:I(base.mean > 300)TRUE`,
         type = 'cluster')



# Table: Survey ----------------------------------------------------------------
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

# Overall
tmp_tbl1 <- tbldata %>% group_by(treat) %>%
  summarise(Obs = length(unique(Self.ID)),
            Willingness = round(mean(willing, na.rm=T), 2))
tmp_tbl2 <- tbldata %>% 
  summarise(Obs = length(unique(Self.ID)),
            Willingness = round(mean(willing, na.rm=T), 2))
tbl1 <- rbind(data.frame(tmp_tbl1), 
              cbind(data.frame(treat='Total'), data.frame(tmp_tbl2)))

# Base.mean <= 5mins
tmp_tbl1 <- subset(tbldata, base.mean <= 300) %>% group_by(treat) %>%
  summarise(Obs = length(unique(Self.ID)),
            Willingness = round(mean(willing, na.rm=T), 2))
tmp_tbl2 <- subset(tbldata, base.mean <= 300) %>%
  summarise(Obs = length(unique(Self.ID)),
            Willingness = round(mean(willing, na.rm=T), 2))
tbl2 <- rbind(data.frame(tmp_tbl1), 
              cbind(data.frame(treat='Total'), data.frame(tmp_tbl2)))

# Base.mean > 5mins
tmp_tbl1 <- subset(tbldata, base.mean > 300) %>% group_by(treat) %>%
  summarise(Obs = length(unique(Self.ID)),
            Willingness = round(mean(willing, na.rm=T), 2))
tmp_tbl2 <- subset(tbldata, base.mean > 300) %>% 
  summarise(Obs = length(unique(Self.ID)),
            Willingness = round(mean(willing, na.rm=T), 2))
tbl3 <- rbind(data.frame(tmp_tbl1), 
              cbind(data.frame(treat='Total'), data.frame(tmp_tbl2)))

  
# Tests -----------------------------------------------------------------------
# Overall
for (i in c(2,3)){
  p.value <- t.test(subset(tbldata, treat==1)$willing,
                    subset(tbldata, treat==i)$willing,
                    alternative='two.sided')$p.value
  print(paste0('Group ', i, 'vs. control: ', p.value))
}


# Baseline <= 5 mins
for (i in c(2,3)){
  p.value <- t.test(subset(tbldata, base.mean <= 300 & treat==1)$willing,
                    subset(tbldata, base.mean <= 300 & treat==i)$willing,
                    alternative='two.sided')$p.value
  print(paste0('Group ', i, 'vs. control: ', p.value))
}


# Baseline > 5 mins
for (i in c(2,3)){
  p.value <- t.test(subset(tbldata, base.mean > 300 & treat==1)$willing,
                    subset(tbldata, base.mean > 300 & treat==i)$willing,
                    alternative='two.sided')$p.value
  print(paste0('Group ', i, 'vs. control: ', p.value))
}



