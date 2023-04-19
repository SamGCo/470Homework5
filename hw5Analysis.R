#1
library(modelsummary)
library(fixest)
final.data<-subset(final.data,State != "District of Columbia" | State != "Puerto Rico")
q1Data<-final.data%>%
  mutate(insured=ins_employer+ins_direct+ins_medicaid+ins_medicare)%>%
  group_by(year)%>%
  summarize(q1Share=mean(ins_direct/adult_pop,na.rm=TRUE))
ans1<-ggplot(q1Data, aes(x = year, y = q1Share)) +
  geom_line()+
  labs(x = "Year", y = "Share of Direct Purchase Insured Individuals", title = "Direct Purchase Share of Insured Over Time")
ans1
#2

#3
q3Data<-final.data%>%
  mutate(insured=ins_employer+ins_direct+ins_medicaid+ins_medicare)%>%
  group_by(year)%>%
  summarize(q3Share=mean(ins_medicaid/adult_pop,na.rm=TRUE))
ans3<-ggplot(q3Data, aes(x = year, y = q3Share)) +
  geom_line()+
  labs(x = "Year", y = "Share of Medicaid Insured Individuals", title = "Medicaid Share of Insured Over Time")
ans3

#4
q4Data<-final.data%>%
  filter(is.na(expand_year) | expand_year==2014)%>%
  mutate(insured=ins_employer+ins_direct+ins_medicaid+ins_medicare)%>%
  group_by(year,expand)%>%
  summarize(q4Share=mean(uninsured/adult_pop))
ans4<-ggplot(q4Data, aes(x = year, y = q4Share,color=expand)) +
  geom_line()+
  labs(x = "Year", y = "Share of Uninsured Individuals", title = "Share of Uninsured Over Time")
ans4

#ATE qs 
#5
q5Data<-final.data%>%
  filter(!is.na(expand_year))%>%
  mutate(insured=ins_employer+ins_direct+ins_medicaid+ins_medicare)%>%
  group_by(year,expand_ever)%>%
  summarize(q5Share=mean(uninsured/adult_pop))%>%
  filter(year==2012 | year==2015)
q5Ans<-knitr::kable(xtabs(q5Share~year+expand_ever,q5Data))
q5Ans
#6


reg.dat <-final.data %>% filter(expand_year==2014 | is.na(expand_year), !is.na(expand_ever)) %>%
  mutate(perc_unins=uninsured/adult_pop,
         post = (year>=2014), 
         treat=post*expand_ever)
dd.ins.reg <- lm(perc_unins ~ post + expand_ever + post*expand_ever, data=reg.dat)
q6ans<-modelsummary::modelsummary(dd.ins.reg)
q6ans
#7

m.twfe <- fixest::feols(perc_unins ~ treat | State + year, data=reg.dat)
summary(m.twfe)
#8
reg8dat <-final.data %>% 
  mutate(perc_unins=uninsured/adult_pop,
         post = (year>=2014), 
         treat=post*expand_ever)
feAllYears <- fixest::feols(perc_unins ~ treat | State + year, data=reg8dat)
summary(feAllYears)
#The results here are not extremely different. The estimator is still negative and statistically significant, but it is a little smaller. 

#9

mod.twfe <- fixest::feols(perc_unins~i(year, expand_ever, ref=2013) | State + year,
                  cluster=~State,
                  data=reg.dat)
modelsummary(mod.twfe)
ans9<-iplot(mod.twfe)
library(broom)
# coefficients <- tidy(mod.twfe, conf.int = TRUE)[, c("term", "estimate")]
# 
# tidy_results <- tidy(mod.twfe, conf.int = TRUE) %>%
#   select(term, estimate, std.error, conf.low, conf.high)
# 
# years<-c(2012,2014,2015,2016,2017,2018,2019)
# tidy_results$term<-years
# ans9<-ggplot(tidy_results, aes(x = term, y = estimate)) +
#   geom_point() +
#   geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
#   labs(x = "Year", y = "Estimates and 95% CI", title = "Medicaid Expansion Effect on Uninsured Each Year")
#10
reg.dat10 <- final.data %>% 
  filter(!is.na(expand_ever)) %>%
  mutate(perc_unins=uninsured/adult_pop,
         post = (year>=2014), 
         treat=post*expand_ever,
         time_to_treat = ifelse(expand_ever==TRUE, year-expand_year,-1),
         time_to_treat = ifelse(time_to_treat < -4, -4, time_to_treat))
mod10 <- feols(perc_unins~i(time_to_treat, expand_ever, ref=-1) | State + year,
                  cluster=~State,
                  data=reg.dat10)
ans10<-iplot(mod10)
# tidy_results10 <- tidy(mod10, conf.int = TRUE) %>%
#   select(term, estimate, std.error, conf.low, conf.high)
# timeToTreat<-c(-3,-2,0,1,2,3,4,5)
# tidy_results10$term<-timeToTreat
# ans10<-ggplot(tidy_results10, aes(x = term, y = estimate)) +
#   geom_point() +
#   geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
#   labs(x = "Years to Treat", y = "Estimates and 95% CI", title = "Medicaid Expansion Effect on Uninsured Each Year")
# ans10
save.image("workspace.RData")