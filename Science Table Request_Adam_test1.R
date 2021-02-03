library(ggplot2)
library(dplyr)
library(reshape2)
library(tidyr)
library(stringr)
library(gridExtra)
library(anytime)
library("readxl")
library(DescTools)
library(zoo)

# episode_date_cut = as.Date("2020-12-31")
# report_date_cut = as.Date("2021-01-12")

GTA = c("City of Toronto Health Unit",
        "Durham Regional Health Unit",
        "Halton Regional Health Unit",
        "Peel Regional Health Unit",
        "York Regional Health Unit")


#################################################################################
#set working directory as appropriate
setwd("J:/MishraTeam/Research/Projects/COVID-IPAC+/Adam testing folder")

source("J:/MishraTeam/COVID-19_spatial_project/repo/covid-neighbourhoods/src/r/lorenz_functions_final.r")

#################################################################################
DA = read.csv("J:/MishraTeam/Research/Projects/COVID-IPAC+/Adam testing folder/data/DA_total_income_pop_DA_var_HRname_sub_Adam.csv", header = T)
colnames(DA)
nrow(DA)
ncol(DA)

#################################################################################
iphis_DA_Report_Date = read.csv("J:/MishraTeam/Research/Projects/COVID-IPAC+/Adam testing folder/data/iphis_DA_report_date_Adam.csv", header = T)
colnames(iphis_DA_Report_Date)
nrow(iphis_DA_Report_Date)
ncol(iphis_DA_Report_Date)

##################################################################################
###Epidemic curve by variable
##################################################################################
epidemic_curve_by_var <- function(PHU,
                                  byvar_raw, 
                                  date_type_episode, 
                                  rank_type, 
                                  labs,
                                  ylimit,
                                  intervention_lines,
                                  reserve_color){
  
  library(zoo)
  ################################################
  if(date_type_episode == T){
    date_cut = episode_date_cut
    date_type = "ACCURATE_EPISODE_DATE_UPDATE"
  }
  if(date_type_episode == F){
    date_cut = report_date_cut
    date_type = "CASE_REPORTED_DATE_UPDATE"
  }
  
  ################################################
  if(rank_type == "tertile"){
    byvar = paste0(byvar_raw, "_rank_tert")
  }
  
  if(rank_type == "quintile"){
    byvar = paste0(byvar_raw, "_rank_quin")
  }
  
  if(rank_type == "decile"){
    byvar = paste0(byvar_raw, "_rank_deci")
  }
  
  byvar <- as.name(byvar)                         # Create quosure
  byvar_raw <- as.name(byvar_raw)                 # Create quosure
  date_type  <- as.name(date_type)                # Create quosure
  
  # DA_level =
  #   DA %>%
  #   filter(PHU_name == !!PHU) %>%
  #   group_by(!!byvar) %>%
  #   summarize(total_pop = sum(DA_population),
  #             mean = mean(!!byvar_raw, na.rm = T),
  #             median = median(!!byvar_raw, na.rm = T),
  #             IQR = paste0(quantile(!!byvar_raw, 1/4, na.rm = T), " - ", quantile(!!byvar_raw, 3/4, na.rm = T)))
  # 
  # write.csv(DA_level, paste0("./fig/", PHU, "_DA_level_", byvar, ".csv"), row.names = F)
  
  ###################################################################################
  DA_population =
    DA %>%
    filter(PHU_name %in% !!PHU) %>%
    group_by(!!byvar) %>%
    summarize(Population = sum(DA_population))
  
  if(date_type_episode == T){
    iphis_DA_aggregated =
      iphis_DA %>%
      filter(!is.na(!!byvar) & PHU_name %in% !!PHU) %>%
      group_by(!!byvar, !!date_type) %>%
      summarize(total_new_case = sum(case_new_total_minus_LTCF_resident),
                total_cum_case = sum(case_cum_total_minus_LTCF_resident))
  }
  
  if(date_type_episode == F){
    iphis_DA_aggregated =
      iphis_DA_Report_Date %>%
      filter(!is.na(!!byvar) & PHU_name %in% !!PHU) %>%
      group_by(!!byvar, !!date_type) %>%
      summarize(total_new_case = sum(case_new_total_minus_LTCF_resident),
                total_cum_case = sum(case_cum_total_minus_LTCF_resident))
  }
  
  iphis_DA_aggregated_pop = merge(iphis_DA_aggregated, DA_population, all.x = T)
  iphis_DA_aggregated_pop$total_new_case100k = iphis_DA_aggregated_pop$total_new_case/iphis_DA_aggregated_pop$Population * 100000
  iphis_DA_aggregated_pop$total_cum_caseper100k = iphis_DA_aggregated_pop$total_cum_case/iphis_DA_aggregated_pop$Population * 100000
  
  iphis_DA_aggregated_pop_rolling =
    iphis_DA_aggregated_pop %>%
    # filter(!is.na(!!byvar)) %>%
    group_by(!!byvar) %>%
    mutate(rolling_newcases_100k = rollmean(total_new_case100k, k = 7, fill = NA))
  
  iphis_DA_aggregated_pop_rolling_sub =
    iphis_DA_aggregated_pop_rolling %>%
    filter(!!date_type <= date_cut)
  
  # write.csv(iphis_DA_aggregated_pop_rolling_sub,
  #           paste0("./fig/", PHU, "_", byvar, "_", date_type, ".csv"), row.names = F)
  
  
  if(intervention_lines == F){
    fig =
      ggplot(iphis_DA_aggregated_pop_rolling_sub,
             aes(x = !!date_type,
                 y = rolling_newcases_100k,
                 group = factor(!!byvar),
                 color= factor(!!byvar))) +
      geom_line(size= 1.5) +
      # xlab(label="Episode Date") +
      ylab(label="Daily Diagnosed COVID-19 Cases \n per 100k population \n (excluding LTCF residents) \n 7-day rolling average") +
      labs(color = labs)+
      # labs(color="Multi-generation tertile") +
      theme_bw(base_size = 24) +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            axis.line = element_line(colour = "black"),
            axis.text.x = element_text(angle=45,hjust=1, size = 20),
            axis.text.y = element_text(size = 20),
            legend.position = "right") +
      scale_x_date(breaks = x_break)+
      scale_y_continuous(limits = c(0, ylimit),
                         breaks = seq(0, ylimit, 6))+
      theme(legend.position="bottom") 
    
    if(length(PHU) == 1){
      fig = fig +ggtitle(PHU)
    }
    
    if(length(PHU) > 1){
      fig = fig +ggtitle("GTA")
    }
    
    if(reserve_color == T){
      fig = fig + scale_color_hue(direction = -1, h.start=90)
    }
    
    if(date_type_episode == T){
      fig = fig + xlab(label="Episode Date")
    }
    
    if(date_type_episode == F){
      fig = fig + xlab(label="Report Date")
    }
    
    if(length(PHU) > 1){
      ggsave(paste0("./fig/GTA_", byvar, "_", date_type, report_date_cut, "_without_lines.png"), width = 14, height =12)
      
    }
    if(length(PHU) == 1){
      ggsave(paste0("./fig/", PHU, "_", byvar, "_", date_type, report_date_cut, "_without_lines.png"), width = 14, height =12)
      
    }
    
  }
  
  if(intervention_lines == T){
    fig =
      ggplot(iphis_DA_aggregated_pop_rolling_sub,
             aes(x = !!date_type,
                 y = rolling_newcases_100k,
                 group = factor(!!byvar),
                 color= factor(!!byvar))) +
      geom_line(size= 1.5) +
      # xlab(label="Episode Date") +
      ylab(label="Daily Diagnosed COVID-19 Cases \n per 100k population \n (excluding LTCF residents) \n 7-day rolling average") +
      labs(color = labs)+
      # labs(color="Multi-generation tertile") +
      theme_bw(base_size = 24) +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            axis.line = element_line(colour = "black"),
            axis.text.x = element_text(angle=45,hjust=1, size = 20),
            axis.text.y = element_text(size = 20),
            legend.position = "right") +
      scale_x_date(breaks = x_break)+
      scale_y_continuous(limits = c(0, ylimit),
                         breaks = seq(0, ylimit, 6))+
      theme(legend.position="bottom") +
      geom_vline(xintercept = as.Date("2020-03-17"), size = 1, linetype = 2, color = "gray50")+
      annotate("text", x=as.Date("2020-03-11"), y= 36,
               label="2020-03-17: Lockdown",
               color="gray50", angle = 90, size = 7) +
      geom_vline(xintercept = as.Date("2020-05-14"), size = 1, linetype = 2, color = "gray50")+
      annotate("text", x=as.Date("2020-05-08"), y= 36,
               label="2020-05-14: Stage 1 relaxation",
               color="gray50", angle = 90, size = 7) +
      geom_vline(xintercept = as.Date("2020-06-24"), size = 1, linetype = 2, color = "gray50")+
      annotate("text", x=as.Date("2020-06-18"), y= 36,
               label="2020-06-24: Stage 2 re-opening",
               color="gray50", angle = 90, size = 7) +
      geom_vline(xintercept = as.Date("2020-07-31"), size = 1, linetype = 2, color = "gray50")+
      annotate("text", x=as.Date("2020-07-25"), y= 36,
               label="2020-07-31: Stage 3 re-opening",
               color="gray50", angle = 90, size = 7) +
      geom_vline(xintercept = as.Date("2020-10-10"), size = 1, linetype = 2, color = "gray50")+
      annotate("text", x=as.Date("2020-10-04"), y= 36,
               label="2020-10-10: Modified stage 2",
               color="gray50", angle = 90, size = 7)+
      geom_vline(xintercept = as.Date("2020-12-26"), size = 1, linetype = 2, color = "gray50")+
      annotate("text", x=as.Date("2020-12-20"), y= 36,
               label="2020-12-26: Lockdown",
               color="gray50", angle = 90, size = 7)
    
    if(reserve_color == T){
      fig = fig + scale_color_hue(direction = -1, h.start=90)
    }
    
    if(length(PHU) == 1){
      fig = fig +ggtitle(PHU)
    }
    
    if(length(PHU) > 1){
      fig = fig +ggtitle("GTA")
    }
    
    
    if(date_type_episode == T){
      fig = fig + xlab(label="Episode Date")
    }
    
    if(date_type_episode == F){
      fig = fig + xlab(label="Report Date")
    }
    
    if(length(PHU) > 1){
      ggsave(paste0("./fig/GTA_", byvar, "_", date_type, "_with_lines.png"), width = 14, height =12)
      
    }
    if(length(PHU) == 1){
      ggsave(paste0("./fig/", PHU, "_", byvar, "_", date_type, "_with_lines.png"), width = 14, height =12)
      
    }
    
  }
  
  return(fig)
}



################
##Report date
################
x_break = c(as.Date("2020-01-17"),
            as.Date("2020-02-17"),
            as.Date("2020-03-17"),
            as.Date("2020-04-17"),
            as.Date("2020-05-17"),
            as.Date("2020-06-17"),
            as.Date("2020-07-17"),
            as.Date("2020-08-17"),
            as.Date("2020-09-17"),
            as.Date("2020-10-17"),
            as.Date("2020-11-17"),
            as.Date("2020-12-17"),
            as.Date("2021-01-17"))

PHU_list = c("City of Toronto Health Unit",
             # "Durham Regional Health Unit",
             # "Halton Regional Health Unit",
             "Peel Regional Health Unit"#,
             # "York Regional Health Unit"
)

iphis_DA_Report_Date$CASE_REPORTED_DATE_UPDATE = anydate(iphis_DA_Report_Date$CASE_REPORTED_DATE_UPDATE)
report_date_cut = max(iphis_DA_Report_Date$CASE_REPORTED_DATE_UPDATE) - 7


for(i in 1: length(PHU_list)){
  epidemic_curve_by_var(PHU = PHU_list[i],
                        byvar_raw ="d_suitable_house",
                        date_type_episode = F,
                        rank_type = "tertile",
                        labs = "Suitable housing tertile",
                        ylimit = 66,
                        intervention_lines = F,
                        reserve_color = F)
}


for(i in 1: length(PHU_list)){
  epidemic_curve_by_var(PHU = PHU_list[i],
                        byvar_raw ="d_multi_generation",
                        date_type_episode = F,
                        rank_type = "tertile",
                        labs = "Multi-generation household tertile",
                        ylimit = 66,
                        intervention_lines = F,
                        reserve_color = T)
}


for(i in 1: length(PHU_list)){
  epidemic_curve_by_var(PHU = PHU_list[i],
                        byvar_raw ="d_sales_trades_manufacturing_agriculture",
                        date_type_episode = F,
                        rank_type = "tertile",
                        labs = "Essential services tertile",
                        ylimit = 66,
                        intervention_lines = F,
                        reserve_color = T)
}


report_date_cut = max(iphis_DA_Report_Date$CASE_REPORTED_DATE_UPDATE) - 3

x_break = c(as.Date("2020-01-21"),
            as.Date("2020-02-21"),
            as.Date("2020-03-21"),
            as.Date("2020-04-21"),
            as.Date("2020-05-21"),
            as.Date("2020-06-21"),
            as.Date("2020-07-21"),
            as.Date("2020-08-21"),
            as.Date("2020-09-21"),
            as.Date("2020-10-21"),
            as.Date("2020-11-21"),
            as.Date("2020-12-21"),
            as.Date("2021-01-21"))

for(i in 1: length(PHU_list)){
  epidemic_curve_by_var(PHU = PHU_list[i],
                        byvar_raw ="d_suitable_house",
                        date_type_episode = F,
                        rank_type = "tertile",
                        labs = "Suitable housing tertile",
                        ylimit = 66,
                        intervention_lines = F,
                        reserve_color = F)
}


for(i in 1: length(PHU_list)){
  epidemic_curve_by_var(PHU = PHU_list[i],
                        byvar_raw ="d_multi_generation",
                        date_type_episode = F,
                        rank_type = "tertile",
                        labs = "Multi-generation household tertile",
                        ylimit = 66,
                        intervention_lines = F,
                        reserve_color = T)
}


for(i in 1: length(PHU_list)){
  epidemic_curve_by_var(PHU = PHU_list[i],
                        byvar_raw ="d_sales_trades_manufacturing_agriculture",
                        date_type_episode = F,
                        rank_type = "tertile",
                        labs = "Essential services tertile",
                        ylimit = 66,
                        intervention_lines = F,
                        reserve_color = T)
}
