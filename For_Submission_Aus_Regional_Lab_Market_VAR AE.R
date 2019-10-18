#--------------------------------------------------------------------------------------------
# Projections of regional labour market adjustment Australia from a VAR model
#--------------------------------------------------------------------------------------------

# This code provides the charts and figures supporting our paper:
# "Projections of regional labour market adjustment Australia from a vector autogression model"
# Rebecca Colquhoun & Cedric Hodges & Adam Elderfield

#--------------------------------------------------------------------------------------------
# Contents
#--------------------------------------------------------------------------------------------

# 1. Set up and data import
# 2. Data cleaning/manipulation
# 3. Data visualisation
# 5. Statistical testing
# 6. VAR implementation

#--------------------------------------------------------------------------------------------
# 1. Set up and data import
#--------------------------------------------------------------------------------------------

rm(list = ls())
required_packages <-
  c(
    "utils",
    "dplyr",
    "lubridate",
    "tidyverse",
    "readxl",
    "ggplot2",
    "ggrepel",
    "tseries",
    "vars",
    "openxlsx"
  )
lapply(required_packages, require, character.only = TRUE)

rm(required_packages)

#  CHANGE WORKING DIRECTORY
setwd(
  "C:/Users/aelde/OneDrive/Documents/GitHub/Aus-labour-market-paper"
)

LabData     <- read_xlsx("Lab_Data.xlsx", col_types = "guess")
AgData      <- read_xlsx("Agri_Data.xlsx", col_types = "guess")


#--------------------------------------------------------------------------------------------
# 2. Data cleaning and manipluation
#--------------------------------------------------------------------------------------------

# Here we do two things: turn data into long format and aggregate some inner city SA4s to capital city regions.

LabData  <- LabData %>%
  gather(key = "date1",
         value = "emp",-`Labour Force Variable`,-`SA4 region`) %>%
  group_by(`SA4 region`,
           `Labour Force Variable`) %>%
  mutate(date1 = seq(ymd("1998/10/01"), ymd("2018/03/01"), by = "month")) %>%
  rename(reg1 = "SA4 region") %>%
  rename(var1 = "Labour Force Variable") %>%
  rename(val1 = "emp") 

AgData  <- AgData %>%
  rename(reg1 = "SA4 (UR)")


sydregs     <-
  c(
    "Sydney - Baulkham Hills and Hawkesbury",
    "Sydney - Blacktown",
    "Sydney - City and Inner South",
    "Sydney - Eastern Suburbs",
    "Sydney - Inner South West",
    "Sydney - Inner West",
    "Sydney - North Sydney and Hornsby",
    "Sydney - Northern Beaches",
    "Sydney - Outer South West",
    "Sydney - Outer West and Blue Mountains",
    "Sydney - Parramatta",
    "Sydney - Ryde",
    "Sydney - South West",
    "Sydney - Sutherland"
  )
melbregs    <-
  c(
    "Melbourne - Inner",
    "Melbourne - Inner East",
    "Melbourne - Inner South",
    "Melbourne - North East",
    "Melbourne - North West",
    "Melbourne - Outer East",
    "Melbourne - South East",
    "Melbourne - West"
  )
brisregs    <-
  c(
    "Brisbane - East",
    "Brisbane - North",
    "Brisbane - South",
    "Brisbane - West",
    "Brisbane Inner City"
  )
adelregs    <-
  c("Adelaide - Central and Hills",
    "Adelaide - North",
    "Adelaide - South",
    "Adelaide - West")
perthregs   <-
  c(
    "Perth - Inner",
    "Perth - North East",
    "Perth - North West",
    "Perth - South East",
    "Perth - South West"
  )
othregs     <-
  c(
    "No place of work SA4 (ACT)",
    "No place of work SA4 (NSW)",
    "No place of work SA4 (NTE)",
    "No place of work SA4 (QLD)",
    "No place of work SA4 (SAU)",
    "No place of work SA4 (TAS)",
    "No place of work SA4 (VIC)",
    "No place of work SA4 (WAU)"
  )


syddata   <-
  filter(LabData, reg1 %in% sydregs)    %>%
  group_by(date1, var1) %>%
  summarise(val1 = sum(val1)) %>% 
  mutate(reg1 = "Sydney")

melbdata  <-
  filter(LabData, reg1 %in% melbregs)   %>%
  group_by(date1, var1) %>%
  summarise(val1 = sum(val1)) %>%
  mutate(reg1 = "Melbourne")

brisdata  <-
  filter(LabData, reg1 %in% brisregs)   %>%
  group_by(date1, var1) %>%
  summarise(val1 = sum(val1)) %>%
  mutate(reg1 = "Brisbane")

adeldata  <-
  filter(LabData, reg1 %in% adelregs)   %>%
  group_by(date1, var1) %>%
  summarise(val1 = sum(val1)) %>%
  mutate(reg1 = "Adelaide")

perthdata <-
  filter(LabData, reg1 %in% perthregs)  %>%
  group_by(date1, var1) %>% 
  summarise(val1 = sum(val1)) %>%
  mutate(reg1 = "Perth")

otherdata <-
  filter(LabData, reg1 %in% othregs)    %>%
  group_by(date1, var1) %>%
  summarise(val1 = sum(val1)) %>%
  mutate(reg1 = "Other")

LabData  <-
  LabData %>% 
  ungroup() %>%
  bind_rows(syddata) %>%  
  bind_rows(melbdata) %>%
  bind_rows(brisdata) %>%
  bind_rows(adeldata) %>%
  bind_rows(perthdata) %>%
  bind_rows(otherdata) %>%
  arrange(reg1) %>%
  filter(!reg1 %in% sydregs) %>%
  filter(!reg1 %in% melbregs) %>%
  filter(!reg1 %in% brisregs) %>%
  filter(!reg1 %in% adelregs) %>%
  filter(!reg1 %in% perthregs) %>%
  filter(!reg1 %in% othregs)

rm(
  syddata,
  melbdata,
  brisdata,
  perthdata,
  otherdata,
  adeldata,
  adelregs,
  brisregs,
  melbregs,
  othregs,
  perthregs,
  sydregs
)

#--------------------------------------------------------------------------------------------
# 3. Data visualisation 
#--------------------------------------------------------------------------------------------

# Chart 3.a Emp growth rate 
LabData %>%
  group_by(reg1, year(date1)) %>%
  filter(var1 == "Employed Total ('000)") %>%
  summarise(val2 = mean(val1)) %>%
  mutate(val2 = val2 / lag(val2) * 100 - 100) %>%
  ungroup() %>% 
  mutate(decade = ifelse(`year(date1)` %in% 1998:2008, 1, 0)) %>%
  group_by(reg1, decade) %>%
  summarise(val3 = mean(val2, na.rm = T)) %>%  
  left_join(AgData, by = "reg1") %>%
  spread(decade, val3) %>% 
  ggplot(aes(x = `1`, y = `0`)) +
  geom_point(aes(col = Share), size = 5) +
  stat_smooth() + 
  geom_text_repel(aes(label = reg1, col = Share), size = 8) +
  theme(
    legend.position = "none",
    axis.text = element_text(size = 20),
    axis.title = element_blank()
  ) +
  ylim(-3, 7.5) +
  xlim(-3, 7.5) +
  geom_abline(intercept = 0, linetype = "dotted")

# Chart 3.b Unemp rate 
LabData %>%
  spread(var1, val1) %>%
  mutate(urate = 100 * (`Unemployed total ('000)` / `Labour force total ('000)`)) %>%
  gather(key = "var1",
         value = "val1",
         -`reg1`,-`date1`) %>%
  group_by(year(date1), reg1) %>%
  filter(var1 == "urate") %>%
  summarise(val2 = mean(val1)) %>% 
  ungroup() %>%
  mutate(decade = ifelse(`year(date1)` %in% 1998:2008, 1, 0)) %>%
  group_by(reg1, decade) %>%
  summarise(val3 = mean(val2)) %>%
  left_join(AgData, by = "reg1") %>%
  spread(decade, val3) %>%
  ggplot(aes(x = `1`, y = `0`)) +
  geom_point(aes(col = Share), size = 5) +
  stat_smooth() +
  geom_text_repel(aes(label = reg1, col = Share), size = 8) +
  theme(
    legend.position = "none",
    axis.text = element_text(size = 20),
    axis.title = element_blank()
  ) +
  ylim(3, 10) + 
  xlim(3, 10) +
  geom_abline(intercept = 0, linetype = "dotted")

# Chart 3.c Part rate 
LabData %>% spread(var1, val1) %>%
  mutate(prate = 100 * (
    `Labour force total ('000)` / `Civilian population aged 15 years and over`
  )) %>%
  gather(key = "var1",
         value = "val1",
         -`reg1`,-`date1`) %>%
  group_by(year(date1), reg1) %>%
  filter(var1 == "prate") %>%
  summarise(val2 = mean(val1)) %>%
  ungroup() %>%
  mutate(decade = ifelse(`year(date1)` %in% 1998:2008, 1, 0)) %>%
  group_by(reg1, decade) %>%
  summarise(val3 = mean(val2)) %>%
  left_join(AgData, by = "reg1") %>%
  spread(decade, val3) %>%
  ggplot(aes(x = `1`, y = `0`)) +
  geom_point(aes(col = Share), size = 5) +
  stat_smooth() + 
  geom_text_repel(aes(label = reg1, col = Share), size = 8) +
  theme(
    legend.position = "none",
    axis.text = element_text(size = 20),
    axis.title = element_blank()
  ) +
  ylim(50, 80) +
  xlim(50, 80) +
  geom_abline(intercept = 0, linetype = "dotted")

#--------------------------------------------------------------------------------------------
# 4. Statistical pre-testing 
#--------------------------------------------------------------------------------------------

# Here we calculate the three main variables for analysis and determine their order of integration
# a)    delta_e = the first difference of the log of employment in reg_i less that of Australia
# b)    log_e   = the log of the ratio EMP/LFORCE in reg_i less that of Australia
# c)    log_p   = the log of the ratio of LFORCE/WAP in reg_i less that of Australia

# 4.a delta_e 

LabData %>%
  group_by(reg1, quarter(date1, with_year = TRUE)) %>%
  filter(var1 == "Employed Total ('000)") %>%
  rename(quarter1 = "quarter(date1, with_year = TRUE)") %>%
  summarise(val2 = mean(val1)) %>%
  mutate(val2 = log(val2) - log(lag(val2))) %>%
  ungroup() %>%
  mutate(AU = rep(filter(., reg1 == "Australia")$val2, 57)) %>%
  mutate(delta_e = val2 - AU) %>% 
  dplyr::select(-val2,-AU) %>%
  filter(reg1 != "Australia" & !is.na(delta_e)) %>%
  filter(reg1 %in% c("Adelaide","Townsville" ,"Mandurah","West and North West" )) %>% 
  group_by(reg1) %>% 
  mutate(Date = seq(as.Date("1999-03-01"), as.Date("2018-03-01"), by = "quarter")) %>% 
  ggplot(aes(as.Date(Date), delta_e)) +
  geom_line() +
  facet_wrap( ~ reg1, scales = "free") +
  ggtitle("delta_e")


delta_e <- LabData %>% 
  group_by(reg1, quarter(date1, with_year = TRUE)) %>%
  filter(var1 == "Employed Total ('000)") %>%
  rename(quarter1 = "quarter(date1, with_year = TRUE)") %>%
  summarise(val2 = mean(val1)) %>%
  group_by(reg1) %>% 
  mutate(val2 = log(val2) - log(lag(val2))) %>%
  ungroup() %>%
  mutate(AU = rep(filter(., reg1 == "Australia")$val2, 57)) %>%
  mutate(delta_e = val2 - AU) %>% 
  dplyr::select(-val2,-AU) %>%
  filter(reg1 != "Australia" & !is.na(delta_e)) %>%
  group_by(reg1) %>% 
  mutate(Date = seq(as.Date("1999-03-01"), as.Date("2018-03-01"), by = "quarter"))


delta_e_ur <- lapply(delta_e %>% 
                       dplyr::select(-`quarter1`) %>% 
                       spread(reg1,delta_e) %>% 
                       dplyr::select(-Date), function(x){
                         
                         list(
                           
                           ADF =x %>% 
                             ts(start = c(1999,1), f =4) %>% 
                             ur.df(type = "drift", selectlags = "AIC"),
                           
                           DFGLS = x %>% 
                             ts(start = c(1999,1), f =4) %>% 
                             ur.ers(type ="DF-GLS",model = "const",lag.max = 4),
                           
                           KPSS = x %>% 
                             ts(start = c(1999,1), f =4) %>% 
                             ur.kpss(type ="mu",use.lag = 4)
                         )
                         
                         
                         
                         
                         
                       })


ur_test_delta_e <- delta_e_ur %>% 
  lapply(function(x){
    
    sapply(x, function(x){
      
      y <- tibble(`test stat` = NA,
                  `crit val-5%` =NA
      )
      
      
      y[["test stat"]] <-x@teststat[1]
      
      y[["crit val-5%"]] <- x@cval[1,2]
      
      
      
      return(y)
      
    } ) %>% 
      data.frame()
    
    
  }) %>%
  do.call("rbind",.) %>% 
  data.frame() %>% 
  mutate(reg1 = row.names(.)) %>% 
  mutate(stat_type = if_else(grepl("test stat",.$reg1),"test stat", "critical value 5%"),
         reg1 =gsub("\\..*","",.$reg1)) %>% 
  gather(test, value, -reg1, -stat_type) %>% 
  spread(stat_type, value) %>% 
  mutate(Hypothesis_test = if_else(abs(as.numeric(`test stat`)) > as.numeric(`critical value 5%`),
                                   "reject H0",
                                   "do not reject H0"))

delta_e_URregions <- ur_test_delta_e %>% 
  filter(reg1 %in% c("Adelaide","Townsville" ,"Mandurah","West and North West" ))

# Chart 4.b log_e 

LabData %>% 
  spread(var1, val1) %>%
  mutate(log_e = 100 * (`Employed Total ('000)` / `Labour force total ('000)`)) %>%
  gather(key = "var1", value = "val1",-`reg1`,-`date1`) %>%
  group_by(reg1, quarter(date1, with_year = TRUE)) %>%
  filter(var1 == "log_e") %>%
  rename(quarter1 = "quarter(date1, with_year = TRUE)") %>%
  summarise(val2 = mean(val1)) %>%
  group_by(reg1) %>% 
  ungroup() %>%
  mutate(AU = rep(filter(., reg1 == "Australia")$val2, 57)) %>%
  filter(reg1 %in% c("Adelaide","Townsville" ,"Mandurah","West and North West" )) %>% 
  mutate(log_e = log(val2 / AU)) %>%
  dplyr::select(-val2,-AU) %>%
  filter(reg1 != "Australia" & !is.na(log_e)) %>%
  group_by(reg1) %>% 
  mutate(Date = seq(as.Date("1998-12-01"), as.Date("2018-03-01"), by = "quarter")) %>% 
  filter(Date != "1998-12-01") %>%
  ggplot(aes(Date, log_e)) + 
  geom_line() + 
  facet_wrap( ~ reg1, scales = "free") + 
  ggtitle("log_e")

log_e <- 
  LabData %>% 
  spread(var1, val1) %>%
  mutate(log_e = 100 * (`Employed Total ('000)` / `Labour force total ('000)`)) %>%
  gather(key = "var1", value = "val1",-`reg1`,-`date1`) %>%
  group_by(reg1, quarter(date1, with_year = TRUE)) %>%
  filter(var1 == "log_e") %>%
  rename(quarter1 = "quarter(date1, with_year = TRUE)") %>%
  summarise(val2 = mean(val1)) %>%
  group_by(reg1) %>%  
  ungroup() %>%
  mutate(AU = rep(filter(., reg1 == "Australia")$val2, 57)) %>%
  mutate(log_e = log(val2 / AU)) %>%
  dplyr::select(-val2,-AU) %>%
  filter(reg1 != "Australia" & !is.na(log_e)) %>%
  group_by(reg1) %>% 
  mutate(Date = seq(as.Date("1998-12-01"), as.Date("2018-03-01"), by = "quarter")) %>% 
  filter(Date != "1998-12-01")


log_e_ur <- lapply(log_e %>% 
                     dplyr::select(-`quarter1`) %>% 
                     spread(reg1,log_e) %>% 
                     dplyr::select(-Date), function(x){
                       
                       list(
                         
                         ADF =x %>% 
                           ts(start = c(1999,1), f =4) %>% 
                           ur.df(type = "drift", selectlags = "AIC"),
                         
                         DFGLS = x %>% 
                           ts(start = c(1999,1), f =4) %>% 
                           ur.ers(type ="DF-GLS",model = "const",lag.max = 4),
                         
                         KPSS = x %>% 
                           ts(start = c(1999,1), f =4) %>% 
                           ur.kpss(type ="mu",use.lag = 4)
                       )
                       
                       
                       
                       
                       
                     })


ur_test_log_e <- log_e_ur %>% 
  lapply(function(x){
    
    sapply(x, function(x){
      
      y <- tibble(`test stat` = NA,
                  `crit val-5%` =NA
      )
      
      
      y[["test stat"]] <-x@teststat[1]
      
      y[["crit val-5%"]] <- x@cval[1,2]
      
      
      
      return(y)
      
    } ) %>% 
      data.frame()
    
    
  }) %>%
  do.call("rbind",.) %>% 
  data.frame() %>% 
  mutate(reg1 = row.names(.)) %>% 
  mutate(stat_type = if_else(grepl("test stat",.$reg1),"test stat", "critical value 5%"),
         reg1 =gsub("\\..*","",.$reg1)) %>% 
  gather(test, value, -reg1, -stat_type) %>% 
  spread(stat_type, value) %>% 
  mutate(Hypothesis_test = if_else(abs(as.numeric(`test stat`)) > as.numeric(`critical value 5%`),
                                   "reject H0",
                                   "do not reject H0"))


logeURregions <- ur_test_log_e %>% 
  filter(reg1 %in% c("Adelaide","Townsville" ,"Mandurah","West and North West" ))

# West and North West non-statioanry based on KPSS test

# Chart 4.c log_p 

LabData %>% 
  spread(var1, val1) %>%
  mutate(prate = 100 * (
    `Labour force total ('000)` / `Civilian population aged 15 years and over`
  )) %>%
  gather(key = "var1", value = "val1",-`reg1`,-`date1`) %>%
  group_by(reg1, quarter(date1, with_year = TRUE)) %>%
  filter(var1 == "prate") %>%
  rename(quarter1 = "quarter(date1, with_year = TRUE)") %>% 
  summarise(val2 = mean(val1))%>%
  ungroup() %>%
  mutate(AU = rep(filter(., reg1 == "Australia")$val2, 57)) %>%
  mutate(log_p = log(val2 / AU)) %>%
  dplyr::select(-val2,-AU) %>%
  filter(reg1 != "Australia" & !is.na(log_p)) %>%
  filter(reg1 %in% c("Adelaide","Townsville" ,"Mandurah","West and North West" )) %>% 
  
  group_by(reg1) %>% 
  mutate(Date = seq(as.Date("1998-12-01"), as.Date("2018-03-01"), by = "quarter")) %>% 
  filter(Date != "1998-12-01") %>%
  ggplot(aes(Date, log_p)) + 
  geom_line() + 
  facet_wrap( ~ reg1, scales = "free") + ggtitle("log_p")

log_p <-  LabData %>% 
  spread(var1, val1) %>%
  mutate(prate = 100 * (
    `Labour force total ('000)` / `Civilian population aged 15 years and over`
  )) %>%
  gather(key = "var1", value = "val1",-`reg1`,-`date1`) %>%
  group_by(reg1, quarter(date1, with_year = TRUE)) %>%
  filter(var1 == "prate") %>%
  rename(quarter1 = "quarter(date1, with_year = TRUE)") %>% 
  summarise(val2 = mean(val1))%>%
  group_by(reg1)  %>% 
  ungroup() %>%
  mutate(AU = rep(filter(., reg1 == "Australia")$val2, 57)) %>%
  mutate(log_p = log(val2 / AU)) %>%
  dplyr::select(-val2,-AU) %>%
  filter(reg1 != "Australia" & !is.na(log_p)) %>% 
  group_by(reg1) %>% 
  mutate(Date = seq(as.Date("1998-12-01"), as.Date("2018-03-01"), by = "quarter")) %>% 
  filter(Date != "1998-12-01")

log_p_ur <- lapply(log_p %>% 
                     dplyr::select(-`quarter1`) %>% 
                     spread(reg1,log_p) %>% 
                     dplyr::select(-Date), function(x){
                       
                       list(
                         
                         ADF =x %>% 
                           ts(start = c(1999,1), f =4) %>% 
                           ur.df(type = "drift", selectlags = "AIC"),
                         
                         DFGLS = x %>% 
                           ts(start = c(1999,1), f =4) %>% 
                           ur.ers(type ="DF-GLS",model = "const",lag.max = 4),
                         
                         KPSS = x %>% 
                           ts(start = c(1999,1), f =4) %>% 
                           ur.kpss(type ="mu", use.lag = 4)
                       )
                       
                       
                       
                       
                       
                     })


ur_test_log_p <- log_p_ur %>% 
  lapply(function(x){
    
    sapply(x, function(x){
      
      y <- tibble(`test stat` = NA,
                  `crit val-5%` =NA
      )
      
      
      y[["test stat"]] <-x@teststat[1]
      
      y[["crit val-5%"]] <- x@cval[1,2]
      
      
      
      return(y)
      
    } ) %>% 
      data.frame()
    
    
  }) %>%
  do.call("rbind",.) %>% 
  data.frame() %>% 
  mutate(reg1 = row.names(.)) %>% 
  mutate(stat_type = if_else(grepl("test stat",.$reg1),"test stat", "critical value 5%"),
         reg1 =gsub("\\..*","",.$reg1)) %>% 
  gather(test, value, -reg1, -stat_type) %>% 
  spread(stat_type, value) %>% 
  mutate(Hypothesis_test = if_else(abs(as.numeric(`test stat`)) > as.numeric(`critical value 5%`),
                                   "reject H0",
                                   "do not reject H0"))

logpURregions <- ur_test_log_p %>% 
  filter(reg1 %in% c("Adelaide","Townsville" ,"Mandurah","West and North West" ))


#--------------------------------------------------------------------------------------------
# 5. SVAR 
#--------------------------------------------------------------------------------------------

Var.data <- left_join(delta_e,
                      log_e) %>% 
  left_join(log_p) %>% 
  dplyr::select(-quarter1) %>% 
  dplyr::select(Date,
                reg1,
                everything())


Bmat <- matrix(c(NA,0,0,NA,NA,0,NA,0,NA),ncol = 3, byrow = TRUE)


Varlist <- list()
for(i in seq_along(unique(Var.data$reg1))){
  
  
  Varlist[[paste(unique(Var.data$reg1)[i])]][["plot"]] <- Var.data %>% 
    filter(reg1 == unique(Var.data$reg1)[i] ) %>%
    gather(Var, Val, -Date,-reg1) %>%
    rename(Variable = Var) %>% 
    ggplot(aes(x = Date, y= Val)) + 
    geom_line(aes(colour = Variable)) +
    scale_colour_manual(values = c("Black","Red","Blue"))+
    #  theme_classic()+
    theme(
      legend.title = element_blank(),
      legend.text = element_text(size = 20),
      legend.position = "bottom",
      axis.text = element_text(size = 20),
      axis.title = element_blank()
    )
  
  
  
  dat <- Var.data %>% 
    filter(reg1 == unique(Var.data$reg1)[i]) %>% 
    ungroup() %>% 
    dplyr::select(delta_e,log_e,log_p) %>% 
    ts(f =4)
  
  Varlist[[paste(unique(Var.data$reg1)[i])]][["VAR"]] <- VAR(dat,
                                                             type = "const", lag.max = 8, ic = "AIC"
  )
  
  Varlist[[paste(unique(Var.data$reg1)[i])]][["summary and test"]]$summary <- Varlist[[paste(unique(Var.data$reg1)[i])]][["VAR"]] %>% summary()  
  
  Varlist[[paste(unique(Var.data$reg1)[i])]][["summary and test"]]$PortACtest <- Varlist[[paste(unique(Var.data$reg1)[i])]][["VAR"]] %>% serial.test()
  
  Varlist[[paste(unique(Var.data$reg1)[i])]][["summary and test"]]$JBnormtest <- Varlist[[paste(unique(Var.data$reg1)[i])]][["VAR"]] %>% normality.test()
  
  Varlist[[paste(unique(Var.data$reg1)[i])]][["summary and test"]]$roots <- Varlist[[paste(unique(Var.data$reg1)[i])]][["VAR"]] %>% roots(modulus = TRUE)
  
  
  if(max(Varlist[[paste(unique(Var.data$reg1)[i])]][["VAR"]][["summary and test"]][["roots"]]$roots) >= 1){
    
    Varlist[[paste(unique(Var.data$reg1)[i])]][["summary and test"]]$DynamicStab <-  "Not stable - check"
    
  }else{
    
    Varlist[[paste(unique(Var.data$reg1)[i])]][["summary and test"]]$DynamicStab <-  "VAR is stable"
    
    
  }
  
  Varlist[[paste(unique(Var.data$reg1)[i])]][["irf chol"]]$data <- Varlist[[paste(unique(Var.data$reg1)[i])]][["VAR"]] %>% irf(n.ahead = 20)
  
  Varlist[[paste(unique(Var.data$reg1)[i])]][["svar"]]<- Varlist[[paste(unique(Var.data$reg1)[i])]][["VAR"]] %>% SVAR(Bmat = Bmat)
  
  Varlist[[paste(unique(Var.data$reg1)[i])]][["svar irf"]]$data <- Varlist[[paste(unique(Var.data$reg1)[i])]][["VAR"]] %>% SVAR(Bmat = Bmat) %>% irf(n.ahead = 20)
  
  
}

#--------------------------------------------------------------------------------------------
# 6. Deep dive into selected regions 
#--------------------------------------------------------------------------------------------

regions <- c("Adelaide","Townsville" ,"Mandurah","West and North West" )
RegionsVar <- Varlist[regions] 

#--------------------------------------------------------------------------------------------
# 5. Table for appendix 
#--------------------------------------------------------------------------------------------

URDiags <- ur_test_delta_e %>%
  mutate(Variable = "delta e") %>% 
  bind_rows(ur_test_log_e %>% 
              mutate(Variable = "log e")) %>% 
  bind_rows(ur_test_log_p %>% 
              mutate(Variable = "log p")) %>% 
  filter(reg1 %in% regions) %>% 
  rename(Region = reg1) %>% 
  dplyr::select(Region, Variable, test, `test stat`, `critical value 5%`)



## Adelaide

# Filter data from VARdata

Adelaide.data <- Var.data %>% 
  filter(reg1 == "Adelaide")

Adelaide.data %>% 
  dplyr::select(Date, log_p) %>%  
  ggplot(aes(Date, log_p)) + 
  geom_line()

# Removing mean from adelaide lop_p

Adelaide.data$log_p_dm <- residuals(lm(Adelaide.data$log_p~1))
Adelaide.data$log_e_dm <- residuals(lm(Adelaide.data$log_e~1))


Adelaideplot <- Adelaide.data %>% 
  ungroup() %>% 
  dplyr::select(-reg1,-log_p_dm, -log_e_dm) %>%
  gather(Variable, Val, -Date) %>% 
  ggplot(aes(Date)) + 
  geom_line(aes(y = Val,colour = Variable))+
  xlab("")+
  ylab("")+
  ggtitle("Adelaide")+
  theme_bw()+
  theme(legend.position = "none")


# Estiamte var

Adelaide.var <- VAR(Adelaide.data[,c("delta_e","log_e_dm","log_p_dm")],lag.max = 8,ic = "AIC",
                    type = "none")

# AIC favours 2 lags, dynamic responses don't change dramatically when including more.

# AC test, BG for small number of lags PT for large

Adelaide.var %>% 
  serial.test(lags.pt = 16, type = "PT.adjusted")

Adelaide.var %>% 
  serial.test(lags.bg = 4, type = "ES")

# Normality:
# Univariate residuals are fine
# Mulitvariate residuals are not normal, however small sample the test is oversized (Killian and Lutkepohl 2017) there are small sample corrections but are not implemented in the VARS package

Adelaide.var %>% 
  normality.test(multivariate.only = FALSE)

# ARCH

Adelaide.var %>% 
  arch.test(lags.multi = 4, multivariate.only = FALSE)

# stability

Adelaide.var %>% 
  stability(type = "OLS-CUSUM") %>% 
  plot()

# Recursive residuals for delta_e move outside the CI, but probably nothing to worry too much about. Possibly include a dummy variable?
Adelaide.var %>% 
  stability(type = "Rec-CUSUM") %>% 
  plot()

Adelaide.svar <-  SVAR(Adelaide.var, Bmat = Bmat)

Adelaide.svar$B/Adelaide.svar$Bse

RegionsVar$Adelaide$VAR <- Adelaide.var

RegionsVar$Adelaide$svar <- Adelaide.svar

RegionsVar$Adelaide$`svar irf`$data <- Adelaide.svar %>% irf(n.ahead = 20)

## Townsville

# Filter data from VARdata

Townsville.data <- Var.data %>% 
  filter(reg1 == "Townsville")

Townsville.data %>% 
  dplyr::select(Date, log_p) %>%  
  ggplot(aes(Date, log_p)) + 
  geom_line()

# Remove SB in log_p

Townsville.data$log_p_dm <- tsoutliers::tso(ts(Townsville.data$log_p, f = 4))$yadj


Townsplot <- Townsville.data %>% 
  ungroup() %>% 
  dplyr::select( -reg1,-log_p_dm) %>%
  gather(Variable, Val, -Date) %>% 
  ggplot(aes(Date)) + 
  geom_line(aes(y = Val,colour = Variable))+
  xlab("")+
  ylab("")+
  ggtitle("Townsville")+
  theme_bw()+
  theme(legend.position = "none")

# Estiamte var

Townsville.var <- VAR(Townsville.data[,c("delta_e","log_e","log_p_dm")], lag.max = 8, ic = "AIC", type ="const" )

Townsville.var <- VAR(Townsville.data[,c("delta_e","log_e","log_p_dm")], p =4, type ="const" )


# AIC favours 2 lags, dynamic responses don't change dramatically when including more.

# AC test, BG for small number of lags PT for large

Townsville.var %>% 
  serial.test(lags.pt = 24, type = "PT.adjusted")

Townsville.var %>% 
  serial.test(lags.bg = 10, type = "ES")

# Normality:
# Univariate residuals are fine
# Mulitvariate residuals are not normal, however small sample the test is oversized (Killian and Lutkepohl 2017) there are small sample corrections but are not implemented in the VARS package

Townsville.var %>% 
  normality.test(multivariate.only = FALSE)

# ARCH

Townsville.var %>% 
  arch.test(lags.multi = 10, multivariate.only = FALSE)

# stability

Townsville.var %>% 
  stability(type = "OLS-CUSUM") %>% 
  plot()

# Recursive residuals for delta_e move outside the CI, but probably nothing to worry too much about. Possibly include a dummy variable?
Townsville.var %>% 
  stability(type = "Rec-CUSUM") %>% 
  plot()

Townsville.svar <-  SVAR(Townsville.var, Bmat = Bmat)

Townsville.svar$B/Townsville.svar$Bse

RegionsVar$Townsville$VAR <- Townsville.var

RegionsVar$Townsville$svar <- Townsville.svar

RegionsVar$Townsville$`svar irf`$data <- Townsville.svar %>% irf(n.ahead = 20)


## Mandurah

# Filter data from VARdata

Mandurah.data <- Var.data %>% 
  filter(reg1 == "Mandurah")

Mandurah.data %>% 
  dplyr::select(Date, delta_e) %>%  
  ggplot(aes(Date, delta_e)) + 
  geom_line()

# Remove outlier in  in delta_e

Mandurah.data$delta_e_adj <- tsoutliers::tso(ts(Mandurah.data$delta_e, f = 4))$yadj

Mandurah.data$log_e_adj <- tsoutliers::tso(ts(Mandurah.data$log_e, f = 4))$yadj



Mandurah.data$log_p_dm <- residuals(lm(Mandurah.data$log_p~1))


Manplot <- Mandurah.data %>% 
  ungroup() %>% 
  dplyr::select( -reg1,-log_e_adj, -log_p_dm, -delta_e_adj) %>%
  gather(Variable, Val, -Date) %>% 
  ggplot(aes(Date)) + 
  geom_line(aes(y = Val,colour = Variable))+
  xlab("")+
  ylab("")+
  ggtitle("Mandurah")+
  theme_bw()+
  theme(legend.position = "none")



# Estiamte var

Mandurah.var <- VAR(Mandurah.data[,c("delta_e_adj","log_e_adj","log_p_dm")], lag.max = 8, ic = "AIC", type ="none" )

Mandurah.var <- VAR(Mandurah.data[,c("delta_e_adj","log_e_adj","log_p_dm")], p =4, type ="none")


# AIC favours 5 lags, dynamic responses don't change dramatically when including more.

# AC test, BG for small number of lags PT for large

Mandurah.var %>% 
  serial.test(lags.pt = 12, type = "PT.adjusted")

Mandurah.var %>% 
  serial.test(lags.bg = 8, type = "ES")

# Normality:
# Univariate - log_e could be an issue - likely an outlier
# Mulitvariate residuals are not normal, however small sample the test is oversized (Killian and Lutkepohl 2017) there are small sample corrections but are not implemented in the VARS package

Mandurah.var %>% 
  normality.test(multivariate.only = FALSE)

# ARCH

Mandurah.var %>% 
  arch.test(lags.multi = 10, multivariate.only = FALSE)

# stability

Mandurah.var %>% 
  stability(type = "OLS-CUSUM") %>% 
  plot()

# Recursive residuals for delta_e move outside the CI, but probably nothing to worry too much about. Possibly include a dummy variable?
Mandurah.var %>% 
  stability(type = "Rec-CUSUM") %>% 
  plot()

Mandurah.svar <-  SVAR(Mandurah.var, Bmat = Bmat)

Mandurah.svar$B/Mandurah.svar$Bse

RegionsVar$Mandurah$VAR <- Mandurah.var

RegionsVar$Mandurah$svar <- Mandurah.svar

RegionsVar$Mandurah$`svar irf`$data <- Mandurah.svar %>% irf(n.ahead = 20)

## West and  North West

# Filter data from VARdata

WNW.data <- Var.data %>% 
  filter(reg1 == "West and North West")

WNW.data %>% 
  dplyr::select(Date, delta_e) %>%  
  ggplot(aes(Date, delta_e)) + 
  geom_line()

# Remove trend in log_e

WNW.data$log_e_adj <- residuals(lm(WNW.data$log_e~c(1:length(WNW.data$log_e) ) ) )

WNW.data$log_p_dm <- residuals(lm(WNW.data$log_p~1))

WNW.data$delta_e_adj <- tsoutliers::tso(ts(WNW.data$delta_e, f =4))$yadj


WNW.plot <- WNW.data %>% 
  ungroup() %>% 
  dplyr::select( -reg1,-log_e_adj,-delta_e_adj,-log_p_dm) %>%
  gather(Variable, Val, -Date) %>% 
  ggplot(aes(Date)) + 
  geom_line(aes(y = Val,colour = Variable))+
  xlab("")+
  ylab("")+
  ggtitle("West North West")+
  theme_bw()+
  theme(legend.position = "none")


# Estiamte var

WNW.var <- VAR(WNW.data[,c("delta_e_adj","log_e_adj","log_p_dm")], lag.max = 8, ic = "AIC", type ="none" )

WNW.var <- VAR(WNW.data[,c("delta_e_adj","log_e_adj","log_p_dm")], p =4, type ="none")


# AIC favours 5 lags, dynamic responses don't change dramatically when including more.

# AC test, BG for small number of lags PT for large

WNW.var %>% 
  serial.test(lags.pt = 12, type = "PT.adjusted")

WNW.var %>% 
  serial.test(lags.bg = 8, type = "ES")

# Normality:
# Univariate - log_e could be an issue - likely an outlier
# Mulitvariate residuals are not normal, however small sample the test is oversized (Killian and Lutkepohl 2017) there are small sample corrections but are not implemented in the VARS package

WNW.var %>% 
  normality.test(multivariate.only = FALSE)

# ARCH

WNW.var %>% 
  arch.test(lags.multi = 10, multivariate.only = FALSE)

# stability

WNW.var %>% 
  stability(type = "OLS-CUSUM") %>% 
  plot()

# Recursive residuals for delta_e move outside the CI, but probably nothing to worry too much about. Possibly include a dummy variable?
WNW.var %>% 
  stability(type = "Rec-CUSUM") %>% 
  plot()

WNW.svar <-  SVAR(WNW.var, Bmat = Bmat)

WNW.svar$B/WNW.svar$Bse

RegionsVar$`West and North West`$VAR <- WNW.var

RegionsVar$`West and North West`$svar <- WNW.svar

RegionsVar$`West and North West`$`svar irf`$data <- WNW.svar %>% irf(n.ahead = 20)

#--------------------------------------------------------------------------------------------
# 5. Facet chart of raw data
#--------------------------------------------------------------------------------------------

gridExtra::grid.arrange(Adelaideplot,Townsplot,Manplot,WNW.plot)

#--------------------------------------------------------------------------------------------
# 5. IRFs for charts 
#--------------------------------------------------------------------------------------------


growby <- function(x, y, b = TRUE){
  
  for(i in seq(1:21)){  
    
    if(b == TRUE){
      
      
      x[i+1] <- x[i]*(1.02)
      
    }else{
      
      x[i+1] <- x[i]*(1.02+y[i])
      
    }
  }
  return(x)
}

index.fun <- function(y){
  
  x <- rep(100,21)
  
  for(i in seq(1:21)){
    
    x[i+1]= x[i]*y[i+1]
    
  }
  return(x)
}


(RegionsVar$Mandurah$`svar irf`$data$irf$delta_e_adj[,1]*shockval) %>% plot()

chartdata <- list()
for(i in regions){
  
  sigmas <- RegionsVar[[i]]$`svar irf`$data$irf[[1]][1,1]
  
  chartdata[[paste("Emp",i)]] <- index.fun(y =(growby(x = rep(100,21), y = 0.01*RegionsVar[[i]]$`svar irf`$data$irf$delta_e[,1]*(-1/sigmas) , b=FALSE)/growby(x = rep(100,21), b=TRUE)))-100
  
  chartdata[[paste("Emp H",i)]] <- index.fun(y =(growby(x = rep(100,21), y = 0.01*RegionsVar[[i]]$`svar irf`$data$Upper$delta_e[,1]*(-1/sigmas) , b=FALSE)/growby(x = rep(100,21), b=TRUE)))-100
  
  chartdata[[paste("Emp L",i)]] <- index.fun(y =(growby(x = rep(100,21), y = 0.01*RegionsVar[[i]]$`svar irf`$data$Lower$delta_e[,1]*(-1/sigmas) , b=FALSE)/growby(x = rep(100,21), b=TRUE)))-100
  
  
  chartdata[[paste("dEmp",i)]] <- c(0,(RegionsVar[[i]]$`svar irf`$data$irf$delta_e[,1]*(-1/sigmas)))
  
  chartdata[[paste("dEmp H",i)]] <-  c(0,(RegionsVar[[i]]$`svar irf`$data$Upper$delta_e[,1]*(-1/sigmas)))
  
  chartdata[[paste("dEmp L",i)]] <- c(0,(RegionsVar[[i]]$`svar irf`$data$Lower$delta_e[,1]*(-1/sigmas)))
  
  
  chartdata[[paste("Ur",i)]] <-  c(0,(0-RegionsVar[[i]]$`svar irf`$data$irf$delta_e[,2])*(-1/sigmas))
  
  chartdata[[paste("Ur H",i)]] <-  c(0,(0-RegionsVar[[i]]$`svar irf`$data$Upper$delta_e[,2])*(-1/sigmas))
  
  chartdata[[paste("Ur L",i)]] <-  c(0,0-(RegionsVar[[i]]$`svar irf`$data$Lower$delta_e[,2])*(-1/sigmas))
  
  
  chartdata[[paste("Part",i)]] <- c(0,(RegionsVar[[i]]$`svar irf`$data$irf$delta_e[,3])*(-1/sigmas))
  
  chartdata[[paste("Part H",i)]] <- c(0,(RegionsVar[[i]]$`svar irf`$data$Upper$delta_e[,3])*(-1/sigmas))
  
  chartdata[[paste("Part L",i)]] <- c(0,(RegionsVar[[i]]$`svar irf`$data$Lower$delta_e[,3])*(-1/sigmas))
  
}

chartdata %>% 
  bind_rows() %>%
  mutate(h = 0:21) %>% 
  gather(Var, Value, -h) %>% 
  filter(!grepl("^Emp*", .$Var)) %>% 
  filter(!grepl("dEmp H|dEmp L|Ur H|Ur L|Part H|Part L",.$Var)) %>%
  left_join(chartdata %>%
              bind_rows() %>%
              mutate(h = 0:21) %>%
              gather(Var, Value, -h) %>%
              filter(grepl("dEmp H|Ur H|Part H",.$Var)) %>%
              mutate(Var = gsub(" H","",.$Var)) %>%
              #mutate(Value = ifelse(grepl("dEmp ",.$Var), NA, Value)) %>%
              rename(Plus95 = Value )
  ) %>%
  left_join(chartdata %>%
              bind_rows() %>%
              mutate(h = 0:21) %>%
              gather(Var, Value, -h) %>%
              filter(grepl("dEmp L|Ur L|Part L",.$Var)) %>%
              mutate(Var = gsub(" L","",.$Var)) %>%
              #mutate(Value = ifelse(grepl("dEmp ",.$Var), NA, Value)) %>%
              rename(Less95 = Value)

  ) %>%
  
  ggplot(aes(x = h))+
  facet_wrap(~Var, scales = "free")+
  geom_line(aes(y = Value), colour = "red")+
  geom_ribbon(aes(ymin = Less95, ymax = Plus95, group = Var), alpha = 0.2)+
  theme_classic()

chartlist <- list()
for(i in regions){

chartlist[[i]] <- chartdata %>% 
  bind_rows() %>%
  mutate(h = 0:21) %>% 
  gather(Var, Value, -h) %>% 
  filter(!grepl("^Emp*", .$Var)) %>% 
  filter(!grepl("dEmp H|dEmp L|Ur H|Ur L|Part H|Part L",.$Var)) %>%
  mutate(Region = gsub("dEmp |Ur |Part ","",.$Var)) %>%
  mutate(Var = gsub("dEmp", "Employment", .$Var)) %>% 
  mutate(Var = gsub("Ur", "Unemployment", .$Var)) %>% 
  mutate(Var = gsub("Part", "Participation", .$Var)) %>%
  mutate(Var = gsub("Employment.*$","Employment", .$Var)) %>% 
  mutate(Var = gsub("Unemployment.*$","Unemployment", .$Var)) %>% 
  mutate(Var = gsub("Participation.*$","Participation", .$Var)) %>% 
  
  left_join(chartdata %>%
              bind_rows() %>%
              mutate(h = 0:21) %>%
              gather(Var, Value, -h) %>%
              filter(grepl("dEmp H|Ur H|Part H",.$Var)) %>%
              mutate(Var = gsub(" H","",.$Var)) %>%
              mutate(Region = gsub("dEmp |Ur |Part ","",.$Var)) %>%
              mutate(Var = gsub("dEmp", "Employment", .$Var)) %>% 
              mutate(Var = gsub("Ur", "Unemployment", .$Var)) %>% 
              mutate(Var = gsub("Part", "Participation", .$Var)) %>%
              mutate(Var = gsub("Employment.*$","Employment", .$Var)) %>% 
              mutate(Var = gsub("Unemployment.*$","Unemployment", .$Var)) %>% 
              mutate(Var = gsub("Participation.*$","Participation", .$Var)) %>% 
              
              #mutate(Value = ifelse(grepl("Emp ",.$Var), NA, Value)) %>%
              rename(Plus95 = Value )
  ) %>%
  left_join(chartdata %>%
              bind_rows() %>%
              mutate(h = 0:21) %>%
              gather(Var, Value, -h) %>%
              filter(grepl("dEmp L|Ur L|Part L",.$Var)) %>%
              mutate(Var = gsub(" L","",.$Var)) %>%
              mutate(Region = gsub("dEmp |Ur |Part ","",.$Var)) %>%
              mutate(Var = gsub("dEmp", "Employment", .$Var)) %>% 
              mutate(Var = gsub("Ur", "Unemployment", .$Var)) %>% 
              mutate(Var = gsub("Part", "Participation", .$Var)) %>%
              mutate(Var = gsub("Employment.*$","Employment", .$Var)) %>% 
              mutate(Var = gsub("Unemployment.*$","Unemployment", .$Var)) %>% 
              mutate(Var = gsub("Participation.*$","Participation", .$Var)) %>% 
              
              #mutate(Value = ifelse(grepl("Emp ",.$Var), NA, Value)) %>%
              rename(Less95 = Value)
            
  ) %>% 
  filter(grepl(paste0(i),.$Region)) %>% 
  ggplot(aes(x = h)) +
  geom_line(aes(y= Value, colour = Var))+
  geom_ribbon(aes(ymin = Less95, ymax = Plus95, group = Var), alpha = 0.2)+
  theme_bw()+
  theme(legend.title = element_blank())+
  ylab("")+
  xlab("")+
  ggtitle(i, subtitle = "Response to -1% shock to employment")

}

gridExtra::grid.arrange(chartlist[[1]],chartlist[[2]],chartlist[[3]],chartlist[[4]])


# Extra chart for total employment growth
LabData %>% 
  filter(var1 == "Employed Total ('000)") %>%
  filter(reg1 == "Australia") %>% 
  mutate(grth = val1/lag(val1,12)*100-100) %>%
  mutate(Mean1 = ifelse(date1<="2008-06-01",
         mean(grth[which(date1<="2008-06-01")],na.rm = T),
         NA)) %>% 
  mutate(Mean2 = ifelse(date1>"2008-06-01",
         mean(grth[which(date1>"2008-06-01")],na.rm = T),
         NA)) %>% 
  ggplot(aes(date1,grth))+
  geom_line()+
  geom_line(aes(date1,Mean1), col = "red")+
  geom_line(aes(date1,Mean2), col = "red")+
  ggtitle("Through the year growth in Australian employment", subtitle = "%")+
  annotate("text", x=ymd("2002-12-01") , y= 3.5, label = "1998:2008 average", colour = "red")+
  annotate("text", x=ymd("2013-04-01") , y= 2.2, label = "2009:2018 average", colour = "red")+
  xlab("")+
  ylab("")+
  theme_bw()
  


############ WE DID IT!!!!!!!!!!!!!!!!! ###########################