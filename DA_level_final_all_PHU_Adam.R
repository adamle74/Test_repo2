# clear workspace
rm(list=ls(all.names=TRUE)) 

library(ggplot2)
library(dplyr)
library(reshape2)
library(tidyr)
library(stringr)
library(gridExtra)
library(anytime)
library("readxl")
library(data.table)

#################################################################################
#set working directory as appropriate
# Adam please check working directory 
# setwd("J:/MishraTeam/COVID-19_spatial_project/repo/covid-neighbourhoods")
DA_list = read_excel("./data/private/raw/ConversionFiles/Conversion_DAs16_to_NHs396_04June2020_v12a.xlsx",
                     sheet = "DAs16_N396_v12_04June2020", range = "A1:W20006")
nrow(DA_list)
DA_list_sub = DA_list[, c("DA2016_num",
                          "OCHPPid",
                          "NHid",
                          "NHid_apNum",
                          "NHname",
                          "HRname")]
length(unique(DA_list_sub$DA2016_num))

table(DA_list_sub$HRname, exclude = NULL)
DA_full_list_ON = DA_list_sub[, "DA2016_num"]
DA_list_sub_HRname = unique(DA_list_sub$HRname)
length(DA_list_sub_HRname) #36

#########################################################################################################################
##multigeneration household
multi_household = read.csv("./data/private/raw/Census2016_Variables/EO3139_T17A_2016_DA_MultiGenHHLDS_02Dec2020_TRIM.csv", header = T)
colnames(multi_household)
table(multi_household$Age_Group, exclude = NULL)
multi_household_total = subset(multi_household, Age_Group == "Total - Age")
nrow(multi_household_total)

#########################################################################################################################
DA_variables = read_excel("./data/private/raw/Census2016_Variables/ModelVariables_IncDwellDensityOccTransp_ONT_DAsCSDs_07Dec2020c_ISSUED.xlsx",
                          sheet = "98-401-X2016044_SELVars_DA_noFN", range = "A3:FE20008")

colnames(DA_variables)

setnames(DA_variables,
        "Population, 2016", 
        "DA_population")

colnames(DA_variables)
nrow(DA_variables)
ncol(DA_variables)
summary(DA_variables$DA16UID)
table(DA_variables$'Government transfers (%)', exclude = NULL)

##################################################################################################################
# Limited educational attainment (percentage of adults in the area not having received any diploma)
table(DA_variables$"No certificate, diploma or degree", exclude = NULL)
table(DA_variables$"Total - Highest certificate, diploma or degree for the population aged 25 to 64 years in private households - 25% sample data", exclude = NULL)

sum(is.na(DA_variables$'No certificate, diploma or degree'))/nrow(DA_variables) * 100

DA_variables$d_low_educational_attainment = ifelse(DA_variables$'No certificate, diploma or degree' == "x", NA,
                                                           round(as.numeric(DA_variables$"No certificate, diploma or degree")/as.numeric(DA_variables$"Total - Highest certificate, diploma or degree for the population aged 25 to 64 years in private households - 25% sample data") * 100, 2))
summary(DA_variables$d_low_educational_attainment)


DA_variables[,c('No certificate, diploma or degree',
                'Total - Highest certificate, diploma or degree for the population aged 25 to 64 years in private households - 25% sample data',
                'd_low_educational_attainment')]

##################################################################################################################
# Social assistance (percentage of individuals in the area receiving governmental transfer payments)
table(DA_variables$'Government transfers (%)', exclude = NULL)
sum(DA_variables$'Government transfers (%)' %in% c("F", "x", ".."))/nrow(DA_variables) * 100

DA_variables$d_government_transfer = ifelse(DA_variables$'Government transfers (%)' %in% c("F", "x", ".."),
                                                    NA,
                                                    DA_variables$'Government transfers (%)')
table(DA_variables$d_government_transfer, exclude = NULL)
summary(as.numeric(DA_variables$d_government_transfer))



##################################################################################################################
# Recent immigration (percentage of individuals in the area who are recent immigrants; 2.1% represents the 60th percentile)
table(DA_variables$`2011 to 2016`, exclude = NULL)
table(DA_variables$`Total - Immigrant status and period of immigration for the population in private households - 25% sample data`, exclude = NULL)

sum(is.na(DA_variables$`2011 to 2016`))/nrow(DA_variables) * 100

DA_variables$d_recent_immigrantion = ifelse(DA_variables$`2011 to 2016` == "x", NA,
                                                           round(as.numeric(DA_variables$`2011 to 2016`)/as.numeric(DA_variables$`Total - Immigrant status and period of immigration for the population in private households - 25% sample data`) * 100, 2))
summary(DA_variables$d_recent_immigrantion)


##################################################################################################################
# Visible minority (percentage of individuals in the area self-identifying as a visible minority)
table(DA_variables$"Total visible minority population", exclude = NULL)
table(DA_variables$"Total - Visible minority for the population in private households - 25% sample data", exclude = NULL)

sum(DA_variables$`Total visible minority population` == "x")/nrow(DA_variables) * 100

DA_variables$d_visible_minority = ifelse(DA_variables$`Total visible minority population` == "x", NA,
                                                    round(as.numeric(DA_variables$`Total visible minority population`)/as.numeric(DA_variables$`Total - Visible minority for the population in private households - 25% sample data`) * 100, 2))
summary(DA_variables$d_visible_minority)

check_d_visible_minority = subset(DA_variables, d_visible_minority > 100)


##################################################################################################################
# Household density (number of persons per dwelling)
table(DA_variables$'Average household size', exclude = NULL)
sum(DA_variables$'Average household size' %in% c("F", "x"))/nrow(DA_variables) * 100

DA_variables$d_average_household_size = ifelse(DA_variables$'Average household size' %in% c("F", "x"),
                                                      NA,
                                                      DA_variables$'Average household size')
table(DA_variables$d_average_household_size, exclude = NULL)


##################################################################################################################
# % suitable housing 
table(DA_variables$"Suitable", exclude = NULL)
table(DA_variables$"Total - Private households by housing suitability - 25% sample data", exclude = NULL)

sum(DA_variables$`Suitable` == "x" | is.na(DA_variables$`Suitable`))/nrow(DA_variables) * 100

DA_variables$d_suitable_house = ifelse(DA_variables$`Suitable` == "x", NA,
                                                 round(as.numeric(DA_variables$`Suitable`)/as.numeric(DA_variables$`Total - Private households by housing suitability - 25% sample data`) * 100, 2))
summary(DA_variables$d_suitable_house)

check_d_suitable_house = subset(DA_variables, d_suitable_house > 100)

check_d_suitable_house[, c("Suitable",
                           "Total - Private households by housing suitability - 25% sample data")]


##################################################################################################################
# % apartments with >=5 storeys
table(DA_variables$`Apartment in a building that has five or more storeys`, exclude = NULL)
table(DA_variables$`Total - Occupied private dwellings by structural type of dwelling - 100% data`, exclude = NULL)

sum(is.na(DA_variables$`Apartment in a building that has five or more storeys`))/nrow(DA_variables) * 100
DA_variables$d_apartments5plus = ifelse(DA_variables$`Apartment in a building that has five or more storeys` == "x", NA,
                                               round(as.numeric(DA_variables$`Apartment in a building that has five or more storeys`)/as.numeric(DA_variables$`Total - Occupied private dwellings by structural type of dwelling - 100% data`) * 100, 2))
summary(DA_variables$d_apartments5plus)

check_d_apartments5plus = subset(DA_variables, d_apartments5plus > 100)

check_d_apartments5plus[, c("Apartment in a building that has five or more storeys",
                           "Total - Occupied private dwellings by structural type of dwelling - 100% data",
                           "d_apartments5plus")]

##################################################################################################################
# work from home
table(DA_variables$`Worked at home - BOTH`, exclude = NULL)
table(DA_variables$`Total - Place of work status for the employed labour force aged 15 years and over in private households - 25% sample data - BOTH`, exclude = NULL)

DA_variables$d_work_from_home = ifelse(DA_variables$`Worked at home - BOTH` == "x", NA,
                                               round(as.numeric(DA_variables$`Worked at home - BOTH`)/as.numeric(DA_variables$`Total - Place of work status for the employed labour force aged 15 years and over in private households - 25% sample data - BOTH`) * 100, 2))
summary(DA_variables$d_work_from_home)

##################################################################################################################
# noncensus-family households of two or more persons

##################################################################################################################
# occupation
table(DA_variables$`All occupations - BOTH`, exclude = NULL)
table(DA_variables$`0 Management occupations - BOTH`, exclude = NULL)
table(DA_variables$`6 Sales and service occupations - BOTH`, exclude = NULL)


##################################################################################################################
# Public transportation
table(DA_variables$`Public transit - BOTH`, exclude = NULL)
table(DA_variables$`Total - Main mode of commuting for the employed labour force aged 15 years and over in private households with a usual place of work or no fixed workplace address - 25% sample data - BOTH`, exclude = NULL)

sum(is.na(DA_variables$`Public transit - BOTH`))/nrow(DA_variables) * 100
DA_variables$d_public_transportation = ifelse(DA_variables$`Public transit - BOTH` == "x", NA,
                                                round(as.numeric(DA_variables$`Public transit - BOTH`)/as.numeric(DA_variables$`Total - Main mode of commuting for the employed labour force aged 15 years and over in private households with a usual place of work or no fixed workplace address - 25% sample data - BOTH`) * 100, 2))
summary(DA_variables$d_public_transportation)
head(DA_variables[, c("d_public_transportation",
                              "Public transit - BOTH",
                              "Total - Main mode of commuting for the employed labour force aged 15 years and over in private households with a usual place of work or no fixed workplace address - 25% sample data - BOTH")])

summary(DA_variables$d_public_transportation)

colnames(DA_variables)
DA_variables_sub = DA_variables[, c("DA16UID",
                                    "DA_population",
                                    "d_low_educational_attainment",                                                                                                 
                                    "d_government_transfer",                                                                                                        
                                    "d_recent_immigrantion",                                                                                                        
                                    "d_visible_minority",                                                                                                          
                                    "d_average_household_size",                                                                                                      
                                    "d_suitable_house",                                                                                                             
                                    "d_apartments5plus",
                                    "d_public_transportation",
                                    "d_work_from_home",
                                    "Number of persons in private households",
                                    "Total Labour Force population aged 15 years and over by Industry - North American Industry Classification System (NAICS) 2012 - 25% sample data - BOTH")]


                                               

#########################################################################################################################
# Material deprivation index
MargIndex = read_excel("./data/private/raw/ON-Marg2016/ON-Marg2016_DAsCTsPHUsCSDs_ONT_14OCT2020_ISSUED.xlsx",
                       sheet = "ON-Marg2016_DAs_ONT", range = "A4:E20164")
colnames(MargIndex)
table(MargIndex$'deprivation2016_DA', exclude = NULL)
summary(MargIndex$'deprivation2016_DA')

nrow(MargIndex)

###################
income_ON = read_excel("./data/private/raw/COVID19 Modeling - income-occ by DA - 2020-12-04.xlsx",
                       sheet = "da_income_occ", range = "A1:X20161")
colnames(income_ON)
ncol(income_ON)
nrow(income_ON)
income_ON$DAUID = income_ON$prcdda

table(income_ON$`After tax income quintile`, exclude = NULL)
table(income_ON$`After tax income decile`, exclude = NULL)
table(income_ON$CMA_name, exclude = NULL)

colnames(income_ON)
colnames(MargIndex)
income_pop_deprivation = merge(income_ON,
                               MargIndex, 
                               by.x = 'DAUID', by.y = "DA16UID_txt", all.x = T)
nrow(income_pop_deprivation)
colnames(income_pop_deprivation)

colnames(multi_household_total)
income_pop_deprivation_multi_gene = merge(income_pop_deprivation, 
                                          multi_household_total, 
                                          by.x = 'DAUID', by.y = "DA16UID", all.x = T)
nrow(income_pop_deprivation_multi_gene)
income_pop_DA_var = merge(income_pop_deprivation_multi_gene, 
                                  DA_variables_sub, 
                                  by.x = 'DAUID', by.y = "DA16UID", all = T)
nrow(income_pop_DA_var)
colnames(income_pop_DA_var)

colnames(DA_list_sub)
income_pop_DA_var_HRname = merge(income_pop_DA_var, DA_list_sub, by.x = 'DAUID', by.y = "DA2016_num", all.x = T)
colnames(income_pop_DA_var_HRname)

###########################################################################################
table(income_pop_DA_var_HRname$`Employed in sales/trades/manufacturing/agriculture (%)`, exclude = NULL)
sum(income_pop_DA_var_HRname$`Employed in sales/trades/manufacturing/agriculture (%)` == ".")
summary(as.numeric(income_pop_DA_var_HRname$`Employed in sales/trades/manufacturing/agriculture (%)`))


# "Employed in sales/services (%)"   
income_pop_DA_var_HRname$d_sales_services = as.numeric(income_pop_DA_var_HRname$`Employed in sales/services (%)`)
summary(income_pop_DA_var_HRname$d_sales_services)

# "Employed in trades/transport/equip (%)"  
income_pop_DA_var_HRname$d_trades_transport_equip = as.numeric(income_pop_DA_var_HRname$`Employed in trades/transport/equip (%)`)
summary(income_pop_DA_var_HRname$d_trades_transport_equip)

# "Employed in manufacturing/utilities (%)"      
income_pop_DA_var_HRname$d_manufacturing_utilities = as.numeric(income_pop_DA_var_HRname$`Employed in manufacturing/utilities (%)`)
summary(income_pop_DA_var_HRname$d_manufacturing_utilities)

# "Employed in agriculture/resources (%)"  
income_pop_DA_var_HRname$d_agriculture_resources = as.numeric(income_pop_DA_var_HRname$`Employed in agriculture/resources (%)`)
summary(income_pop_DA_var_HRname$d_agriculture_resources)

# "Employed in sales/trades/manufacturing/agriculture (%)"    
income_pop_DA_var_HRname$d_sales_trades_manufacturing_agriculture = as.numeric(income_pop_DA_var_HRname$`Employed in sales/trades/manufacturing/agriculture (%)`)
summary(income_pop_DA_var_HRname$d_sales_trades_manufacturing_agriculture)

# "Employed in executive/managerial/professional (%)"  
income_pop_DA_var_HRname$d_executive_managerial_professional = as.numeric(income_pop_DA_var_HRname$`Employed in executive/managerial/professional (%)`)
summary(income_pop_DA_var_HRname$d_executive_managerial_professional)

# "Employed in health (%)"    
income_pop_DA_var_HRname$d_health = as.numeric(income_pop_DA_var_HRname$`Employed in health (%)`)
summary(income_pop_DA_var_HRname$d_health)

# "Employed in business/admin (%)"     
income_pop_DA_var_HRname$d_business_admin = as.numeric(income_pop_DA_var_HRname$`Employed in business/admin (%)`)
summary(income_pop_DA_var_HRname$d_business_admin)

# "Employed in education/law/govt (%)"    
income_pop_DA_var_HRname$d_education_law_govt = as.numeric(income_pop_DA_var_HRname$`Employed in education/law/govt (%)`)
summary(income_pop_DA_var_HRname$d_education_law_govt)

# "Employed in non-essential (%)"    
income_pop_DA_var_HRname$d_other_non_essential = income_pop_DA_var_HRname$d_business_admin + income_pop_DA_var_HRname$d_education_law_govt + income_pop_DA_var_HRname$d_executive_managerial_professional
summary(income_pop_DA_var_HRname$d_other_non_essential)
subset(income_pop_DA_var_HRname, d_other_non_essential > 100)[, c("d_other_non_essential",
                                                                  "d_business_admin",
                                                                  "d_education_law_govt",
                                                                  "d_executive_managerial_professional")]




###########################################################################################################################################
##multigeneration
table(income_pop_DA_var_HRname$`Number of persons in private households`, exclude = NULL)
table(income_pop_DA_var_HRname$Total...Census.family.status.and.household.living.arrangements, exclude = NULL)

sum(is.na(income_pop_DA_var_HRname$`Total...Census.family.status.and.household.living.arrangements`))/nrow(income_pop_DA_var_HRname) * 100

income_pop_DA_var_HRname$d_multi_generation = ifelse(income_pop_DA_var_HRname$`Total...Census.family.status.and.household.living.arrangements` == "x", NA,
                                                 round(as.numeric(income_pop_DA_var_HRname$`Total...Census.family.status.and.household.living.arrangements`)/as.numeric(income_pop_DA_var_HRname$`Number of persons in private households`) * 100, 2))
summary(income_pop_DA_var_HRname$d_multi_generation)


colnames(income_pop_DA_var_HRname)
table(income_pop_DA_var_HRname$HRname, exclude = NULL)
income_pop_DA_var_HRname_sub = subset(income_pop_DA_var_HRname, HRname %in% c("City of Ottawa Health Unit",
                                                                              "City of Toronto Health Unit",
                                                                              "Durham Regional Health Unit",
                                                                              "Halton Regional Health Unit",
                                                                              "Peel Regional Health Unit",
                                                                              "York Regional Health Unit",
                                                                              "Niagara Regional Area Health Unit",
                                                                              "Windsor-Essex County Health Unit",
                                                                              "Wellington-Dufferin-Guelph Health Unit",
                                                                              "Waterloo Health Unit",
                                                                              "City of Hamilton Health Unit"))

nrow(income_pop_DA_var_HRname_sub)


###########################################################################################################################################
###Ranking
###########################################################################################################################################
#############################################################################################

quin <- function (input_var){

  data_ranked = 
    income_pop_DA_var_HRname_sub %>%
    arrange(PHU_name, DAUID) %>%
    group_by(PHU_name) %>%
    mutate(rank = ifelse({{input_var}}<=quantile(rep({{input_var}}, DA_population),c(.2),na.rm=TRUE), 1,
                         ifelse({{input_var}}>quantile(rep({{input_var}}, DA_population),c(.2),na.rm=TRUE)&{{input_var}}<=quantile(rep({{input_var}}, DA_population),c(.4),na.rm=TRUE), 2,
                                ifelse({{input_var}}>quantile(rep({{input_var}}, DA_population),c(.4),na.rm=TRUE)&{{input_var}}<=quantile(rep({{input_var}}, DA_population),c(.6),na.rm=TRUE), 3,
                                       ifelse({{input_var}}>quantile(rep({{input_var}}, DA_population),c(.6),na.rm=TRUE)&{{input_var}}<=quantile(rep({{input_var}}, DA_population),c(.8),na.rm=TRUE), 4,
                                              ifelse({{input_var}}>quantile(rep({{input_var}}, DA_population),c(.8),na.rm=TRUE), 5, NA))))))
  return(data_ranked$rank)
   
}


deci <- function(input_var){

  data_ranked = 
    income_pop_DA_var_HRname_sub %>%
    arrange(PHU_name, DAUID) %>%
    group_by(PHU_name) %>%
    mutate(rank = ifelse({{input_var}}<=quantile(rep({{input_var}}, DA_population),c(.1),na.rm=TRUE), 1,
                         ifelse({{input_var}}>quantile(rep({{input_var}}, DA_population),c(.1),na.rm=TRUE)&{{input_var}}<=quantile(rep({{input_var}}, DA_population),c(.2),na.rm=TRUE), 2,
                                ifelse({{input_var}}>quantile(rep({{input_var}}, DA_population),c(.2),na.rm=TRUE)&{{input_var}}<=quantile(rep({{input_var}}, DA_population),c(.3),na.rm=TRUE), 3,
                                       ifelse({{input_var}}>quantile(rep({{input_var}}, DA_population),c(.3),na.rm=TRUE)&{{input_var}}<=quantile(rep({{input_var}}, DA_population),c(.4),na.rm=TRUE), 4,
                                              ifelse({{input_var}}>quantile(rep({{input_var}}, DA_population),c(.4),na.rm=TRUE)&{{input_var}}<=quantile(rep({{input_var}}, DA_population),c(.5),na.rm=TRUE), 5,
                                                     ifelse({{input_var}}>quantile(rep({{input_var}}, DA_population),c(.5),na.rm=TRUE)&{{input_var}}<=quantile(rep({{input_var}}, DA_population),c(.6),na.rm=TRUE), 6,
                                                            ifelse({{input_var}}>quantile(rep({{input_var}}, DA_population),c(.6),na.rm=TRUE)&{{input_var}}<=quantile(rep({{input_var}}, DA_population),c(.7),na.rm=TRUE), 7,
                                                                   ifelse({{input_var}}>quantile(rep({{input_var}}, DA_population),c(.7),na.rm=TRUE)&{{input_var}}<=quantile(rep({{input_var}}, DA_population),c(.8),na.rm=TRUE), 8,
                                                                          ifelse({{input_var}}>quantile(rep({{input_var}}, DA_population),c(.8),na.rm=TRUE)&{{input_var}}<=quantile(rep({{input_var}}, DA_population),c(.9),na.rm=TRUE), 9,
                                                                                 ifelse({{input_var}}>quantile(rep({{input_var}}, DA_population),c(.9),na.rm=TRUE), 10, NA)))))))))))
  return(data_ranked$rank)

}

tertile <- function(input_var){

  data_ranked = 
    income_pop_DA_var_HRname_sub %>%
    arrange(PHU_name, DAUID) %>%
    group_by(PHU_name) %>%
    mutate(rank = ifelse({{input_var}}<= quantile(rep({{input_var}}, DA_population),c(.33),na.rm=TRUE), 1,
                         ifelse({{input_var}} >quantile(rep({{input_var}}, DA_population),c(.33),na.rm=TRUE)&
                                  {{input_var}} <=quantile(rep({{input_var}}, DA_population),c(.67),na.rm=TRUE), 2,
                                ifelse({{input_var}}>quantile(rep({{input_var}}, DA_population),c(.67),na.rm=TRUE), 3, NA))))
  return(data_ranked$rank)
}



income_pop_DA_var_HRname_sub = arrange(income_pop_DA_var_HRname_sub, PHU_name, DAUID)

###################################
# "Employed in sales/services (%)"  
summary(income_pop_DA_var_HRname_sub$d_sales_services)

income_pop_DA_var_HRname_sub$d_sales_services_rank_quin = quin(d_sales_services)
income_pop_DA_var_HRname_sub$d_sales_services_rank_deci = deci(d_sales_services)
income_pop_DA_var_HRname_sub$d_sales_services_rank_tert = tertile(d_sales_services)

table(income_pop_DA_var_HRname_sub$d_sales_services_rank_quin, exclude = NULL)
table(income_pop_DA_var_HRname_sub$d_sales_services_rank_deci, exclude = NULL)
table(income_pop_DA_var_HRname_sub$d_sales_services_rank_tert, exclude = NULL)

income_pop_DA_var_HRname_sub[, c("d_sales_services_rank_deci", "d_sales_services")]
balance_check = 
  income_pop_DA_var_HRname_sub %>%
  group_by(HRname, d_sales_services_rank_deci)%>% 
  summarize(N = sum(DA_population))



# "Employed in trades/transport/equip (%)"  
summary(income_pop_DA_var_HRname_sub$d_trades_transport_equip)

income_pop_DA_var_HRname_sub$d_trades_transport_equip_rank_quin = quin(d_trades_transport_equip)
income_pop_DA_var_HRname_sub$d_trades_transport_equip_rank_deci = deci(d_trades_transport_equip)
income_pop_DA_var_HRname_sub$d_trades_transport_equip_rank_tert = tertile(d_trades_transport_equip)

table(income_pop_DA_var_HRname_sub$d_trades_transport_equip_rank_quin, exclude = NULL)
table(income_pop_DA_var_HRname_sub$d_trades_transport_equip_rank_deci, exclude = NULL)
table(income_pop_DA_var_HRname_sub$d_trades_transport_equip_rank_tert, exclude = NULL)

# "Employed in manufacturing/utilities (%)"      
summary(income_pop_DA_var_HRname_sub$d_manufacturing_utilities)

income_pop_DA_var_HRname_sub$d_manufacturing_utilities_rank_quin = quin(d_manufacturing_utilities)
income_pop_DA_var_HRname_sub$d_manufacturing_utilities_rank_deci = deci(d_manufacturing_utilities)
income_pop_DA_var_HRname_sub$d_manufacturing_utilities_rank_tert = tertile(d_manufacturing_utilities)

table(income_pop_DA_var_HRname_sub$d_manufacturing_utilities_rank_quin, exclude = NULL)
table(income_pop_DA_var_HRname_sub$d_manufacturing_utilities_rank_deci, exclude = NULL)
table(income_pop_DA_var_HRname_sub$d_manufacturing_utilities_rank_tert, exclude = NULL)

# "Employed in agriculture/resources (%)"  
summary(income_pop_DA_var_HRname_sub$d_agriculture_resources)

income_pop_DA_var_HRname_sub$d_agriculture_resources_rank_quin = quin(d_agriculture_resources)
income_pop_DA_var_HRname_sub$d_agriculture_resources_rank_deci = deci(d_agriculture_resources)
income_pop_DA_var_HRname_sub$d_agriculture_resources_rank_tert = tertile(d_agriculture_resources)

table(income_pop_DA_var_HRname_sub$d_agriculture_resources_rank_quin, exclude = NULL)
table(income_pop_DA_var_HRname_sub$d_agriculture_resources_rank_deci, exclude = NULL)
table(income_pop_DA_var_HRname_sub$d_agriculture_resources_rank_tert, exclude = NULL)

# "Employed in sales/trades/manufacturing/agriculture (%)"    
summary(income_pop_DA_var_HRname_sub$d_sales_trades_manufacturing_agriculture)

income_pop_DA_var_HRname_sub$d_sales_trades_manufacturing_agriculture_rank_quin = quin(d_sales_trades_manufacturing_agriculture)
income_pop_DA_var_HRname_sub$d_sales_trades_manufacturing_agriculture_rank_deci = deci(d_sales_trades_manufacturing_agriculture)
income_pop_DA_var_HRname_sub$d_sales_trades_manufacturing_agriculture_rank_tert = tertile(d_sales_trades_manufacturing_agriculture)

table(income_pop_DA_var_HRname_sub$d_sales_trades_manufacturing_agriculture_rank_quin, exclude = NULL)
table(income_pop_DA_var_HRname_sub$d_sales_trades_manufacturing_agriculture_rank_deci, exclude = NULL)
table(income_pop_DA_var_HRname_sub$d_sales_trades_manufacturing_agriculture_rank_tert, exclude = NULL)




# "Employed in executive/managerial/professional (%)"  
summary(income_pop_DA_var_HRname_sub$d_executive_managerial_professional)

income_pop_DA_var_HRname_sub$d_executive_managerial_professional_rank_quin = quin(d_executive_managerial_professional)
income_pop_DA_var_HRname_sub$d_executive_managerial_professional_rank_deci = deci(d_executive_managerial_professional)
income_pop_DA_var_HRname_sub$d_executive_managerial_professional_rank_tert = tertile(d_executive_managerial_professional)

table(income_pop_DA_var_HRname_sub$d_executive_managerial_professional_rank_quin, exclude = NULL)
table(income_pop_DA_var_HRname_sub$d_executive_managerial_professional_rank_deci, exclude = NULL)
table(income_pop_DA_var_HRname_sub$d_executive_managerial_professional_rank_tert, exclude = NULL)

# "Employed in health (%)"    
summary(income_pop_DA_var_HRname_sub$d_health)

income_pop_DA_var_HRname_sub$d_health_rank_quin = quin(d_health)
income_pop_DA_var_HRname_sub$d_health_rank_deci = deci(d_health)
income_pop_DA_var_HRname_sub$d_health_rank_tert = tertile(d_health)

table(income_pop_DA_var_HRname_sub$d_health_rank_quin, exclude = NULL)
table(income_pop_DA_var_HRname_sub$d_health_rank_deci, exclude = NULL)
table(income_pop_DA_var_HRname_sub$d_health_rank_tert, exclude = NULL)

# "Employed in business/admin (%)"     
summary(income_pop_DA_var_HRname_sub$d_business_admin)

income_pop_DA_var_HRname_sub$d_business_admin_rank_quin = quin(d_business_admin)
income_pop_DA_var_HRname_sub$d_business_admin_rank_deci = deci(d_business_admin)
income_pop_DA_var_HRname_sub$d_business_admin_rank_tert = tertile(d_business_admin)

table(income_pop_DA_var_HRname_sub$d_business_admin_rank_quin, exclude = NULL)
table(income_pop_DA_var_HRname_sub$d_business_admin_rank_deci, exclude = NULL)
table(income_pop_DA_var_HRname_sub$d_business_admin_rank_tert, exclude = NULL)

# "Employed in education/law/govt (%)"    
summary(income_pop_DA_var_HRname_sub$d_education_law_govt)

income_pop_DA_var_HRname_sub$d_education_law_govt_rank_quin = quin(d_education_law_govt)
income_pop_DA_var_HRname_sub$d_education_law_govt_rank_deci = deci(d_education_law_govt)
income_pop_DA_var_HRname_sub$d_education_law_govt_rank_tert = tertile(d_education_law_govt)

table(income_pop_DA_var_HRname_sub$d_education_law_govt_rank_quin, exclude = NULL)
table(income_pop_DA_var_HRname_sub$d_education_law_govt_rank_deci, exclude = NULL)
table(income_pop_DA_var_HRname_sub$d_education_law_govt_rank_tert, exclude = NULL)


###################################
#d_other_non_essential
summary(income_pop_DA_var_HRname_sub$d_other_non_essential)

income_pop_DA_var_HRname_sub$d_other_non_essential_rank_quin = quin(d_other_non_essential)
income_pop_DA_var_HRname_sub$d_other_non_essential_rank_deci = deci(d_other_non_essential)
income_pop_DA_var_HRname_sub$d_other_non_essential_rank_tert = tertile(d_other_non_essential)

table(income_pop_DA_var_HRname_sub$d_other_non_essential_rank_quin, exclude = NULL)
table(income_pop_DA_var_HRname_sub$d_other_non_essential_rank_deci, exclude = NULL)
table(income_pop_DA_var_HRname_sub$d_other_non_essential_rank_tert, exclude = NULL)

###################################
summary(income_pop_DA_var_HRname_sub$d_multi_generation)
income_pop_DA_var_HRname_sub$d_multi_generation_rank_quin = quin(d_multi_generation)
income_pop_DA_var_HRname_sub$d_multi_generation_rank_deci = deci(d_multi_generation)
income_pop_DA_var_HRname_sub$d_multi_generation_rank_tert = tertile(d_multi_generation)

table(income_pop_DA_var_HRname_sub$d_multi_generation_rank_quin, exclude = NULL)
table(income_pop_DA_var_HRname_sub$d_multi_generation_rank_deci, exclude = NULL)
table(income_pop_DA_var_HRname_sub$d_multi_generation_rank_tert, exclude = NULL)

###################################
summary(income_pop_DA_var_HRname_sub$d_low_educational_attainment)
income_pop_DA_var_HRname_sub$d_low_educational_attainment_rank_quin = quin(d_low_educational_attainment)
income_pop_DA_var_HRname_sub$d_low_educational_attainment_rank_deci = deci(d_low_educational_attainment)
income_pop_DA_var_HRname_sub$d_low_educational_attainment_rank_tert = tertile(d_low_educational_attainment)

table(income_pop_DA_var_HRname_sub$d_low_educational_attainment_rank_quin, exclude = NULL)
table(income_pop_DA_var_HRname_sub$d_low_educational_attainment_rank_deci, exclude = NULL)
table(income_pop_DA_var_HRname_sub$d_low_educational_attainment_rank_tert, exclude = NULL)
table(income_pop_DA_var_HRname_sub$d_low_educational_attainment_rank_deci, income_pop_DA_var_HRname_sub$d_low_educational_attainment_rank_tert, exclude = NULL)
###################################
summary(income_pop_DA_var_HRname_sub$d_low_educational_attainment)
income_pop_DA_var_HRname_sub$d_low_educational_attainment_rank_quin = quin(d_low_educational_attainment)
income_pop_DA_var_HRname_sub$d_low_educational_attainment_rank_deci = deci(d_low_educational_attainment)
income_pop_DA_var_HRname_sub$d_low_educational_attainment_rank_tert = tertile(d_low_educational_attainment)

table(income_pop_DA_var_HRname_sub$d_low_educational_attainment_rank_quin, exclude = NULL)
table(income_pop_DA_var_HRname_sub$d_low_educational_attainment_rank_deci, exclude = NULL)
table(income_pop_DA_var_HRname_sub$d_low_educational_attainment_rank_tert, exclude = NULL)
table(income_pop_DA_var_HRname_sub$d_low_educational_attainment_rank_deci, income_pop_DA_var_HRname_sub$d_low_educational_attainment_rank_tert, exclude = NULL)


###################################
table(income_pop_DA_var_HRname_sub$d_government_transfer, exclude = NULL)
income_pop_DA_var_HRname_sub$d_government_transfer = as.numeric(income_pop_DA_var_HRname_sub$d_government_transfer)
summary(income_pop_DA_var_HRname_sub$d_government_transfer)

income_pop_DA_var_HRname_sub$d_government_transfer_rank_quin = quin(d_government_transfer)
income_pop_DA_var_HRname_sub$d_government_transfer_rank_deci = deci(d_government_transfer)
income_pop_DA_var_HRname_sub$d_government_transfer_rank_tert = tertile(d_government_transfer)

table(income_pop_DA_var_HRname_sub$d_government_transfer_rank_quin, exclude = NULL)
table(income_pop_DA_var_HRname_sub$d_government_transfer_rank_deci, exclude = NULL)
table(income_pop_DA_var_HRname_sub$d_government_transfer_rank_tert, exclude = NULL)

income_pop_DA_var_HRname_sub[, c("d_government_transfer", "d_government_transfer_rank_tert")]
###################################
summary(income_pop_DA_var_HRname_sub$d_recent_immigrantion)

income_pop_DA_var_HRname_sub$d_recent_immigrantion_rank_quin = quin(d_recent_immigrantion)
income_pop_DA_var_HRname_sub$d_recent_immigrantion_rank_deci = deci(d_recent_immigrantion)
income_pop_DA_var_HRname_sub$d_recent_immigrantion_rank_tert = tertile(d_recent_immigrantion)

table(income_pop_DA_var_HRname_sub$d_recent_immigrantion_rank_quin, exclude = NULL)
table(income_pop_DA_var_HRname_sub$d_recent_immigrantion_rank_deci, exclude = NULL)
table(income_pop_DA_var_HRname_sub$d_recent_immigrantion_rank_tert, exclude = NULL)

income_pop_DA_var_HRname_sub[, c("d_recent_immigrantion", "d_recent_immigrantion_rank_tert")]

###################################
summary(income_pop_DA_var_HRname_sub$d_visible_minority)

income_pop_DA_var_HRname_sub$d_visible_minority_rank_quin = quin(d_visible_minority)
income_pop_DA_var_HRname_sub$d_visible_minority_rank_deci = deci(d_visible_minority)
income_pop_DA_var_HRname_sub$d_visible_minority_rank_tert = tertile(d_visible_minority)

table(income_pop_DA_var_HRname_sub$d_visible_minority_rank_quin, exclude = NULL)
table(income_pop_DA_var_HRname_sub$d_visible_minority_rank_deci, exclude = NULL)
table(income_pop_DA_var_HRname_sub$d_visible_minority_rank_tert, exclude = NULL)


###################################
#d_average_househod_size
table(income_pop_DA_var_HRname_sub$d_average_household_size, exclude = NULL)

income_pop_DA_var_HRname_sub$d_average_household_size_rank_quin = quin(d_average_household_size)
income_pop_DA_var_HRname_sub$d_average_household_size_rank_deci = deci(d_average_household_size)
income_pop_DA_var_HRname_sub$d_average_household_size_rank_tert = tertile(d_average_household_size)

table(income_pop_DA_var_HRname_sub$d_average_household_size_rank_quin, exclude = NULL)
table(income_pop_DA_var_HRname_sub$d_average_household_size_rank_deci, exclude = NULL)
table(income_pop_DA_var_HRname_sub$d_average_household_size_rank_tert, exclude = NULL)


###################################
#d_suitable_house
summary(income_pop_DA_var_HRname_sub$d_suitable_house)

income_pop_DA_var_HRname_sub$d_suitable_house_rank_quin = quin(d_suitable_house)
income_pop_DA_var_HRname_sub$d_suitable_house_rank_deci = deci(d_suitable_house)
income_pop_DA_var_HRname_sub$d_suitable_house_rank_tert = tertile(d_suitable_house)

table(income_pop_DA_var_HRname_sub$d_suitable_house_rank_quin, exclude = NULL)
table(income_pop_DA_var_HRname_sub$d_suitable_house_rank_deci, exclude = NULL)
table(income_pop_DA_var_HRname_sub$d_suitable_house_rank_tert, exclude = NULL)


###################################
#d_apartments5plus
summary(income_pop_DA_var_HRname_sub$d_apartments5plus)

income_pop_DA_var_HRname_sub$d_apartments5plus_rank_quin = quin(d_apartments5plus)
income_pop_DA_var_HRname_sub$d_apartments5plus_rank_deci = deci(d_apartments5plus)
table(income_pop_DA_var_HRname_sub$d_apartments5plus_rank_quin, exclude = NULL)
table(income_pop_DA_var_HRname_sub$d_apartments5plus_rank_deci, exclude = NULL)

###################################
# Material deprivation index
income_pop_DA_var_HRname_sub$d_deprivation2016_DA_rank_quin = quin(deprivation2016_DA)
income_pop_DA_var_HRname_sub$d_deprivation2016_DA_rank_deci = deci(deprivation2016_DA)
table(income_pop_DA_var_HRname_sub$d_deprivation2016_DA_rank_quin, exclude = NULL)
table(income_pop_DA_var_HRname_sub$d_deprivation2016_DA_rank_deci, exclude = NULL)

###################################
# work from home
income_pop_DA_var_HRname_sub$d_work_from_home_rank_quin = quin(d_work_from_home)
income_pop_DA_var_HRname_sub$d_work_from_home_rank_deci = deci(d_work_from_home)

table(income_pop_DA_var_HRname_sub$d_work_from_home_rank_quin, exclude = NULL)
table(income_pop_DA_var_HRname_sub$d_work_from_home_rank_deci, exclude = NULL)



###################################
# income
table(income_pop_DA_var_HRname_sub$`After tax income PPE`, exclude = NULL)
income_pop_DA_var_HRname_sub$d_after_tax_income_PPE = as.numeric(income_pop_DA_var_HRname_sub$`After tax income PPE`)
summary(income_pop_DA_var_HRname_sub$d_after_tax_income_PPE)

income_pop_DA_var_HRname_sub$d_after_tax_income_PPE_rank_quin = quin(d_after_tax_income_PPE)
income_pop_DA_var_HRname_sub$d_after_tax_income_PPE_rank_deci = deci(d_after_tax_income_PPE)
income_pop_DA_var_HRname_sub$d_after_tax_income_PPE_rank_tert = tertile(d_after_tax_income_PPE)

table(income_pop_DA_var_HRname_sub$d_after_tax_income_PPE_rank_quin, exclude = NULL)
table(income_pop_DA_var_HRname_sub$d_after_tax_income_PPE_rank_deci, exclude = NULL)
table(income_pop_DA_var_HRname_sub$d_after_tax_income_PPE_rank_tert, exclude = NULL)
sum(income_pop_DA_var_HRname_sub$d_after_tax_income_PPE_rank_deci == income_pop_DA_var_HRname_sub$`After tax income decile (PHU-ranked)`)
nrow(income_pop_DA_var_HRname_sub)

sum(income_pop_DA_var_HRname_sub$d_after_tax_income_PPE_rank_deci == income_pop_DA_var_HRname_sub$`After tax income decile (PHU-ranked)`)
sum(income_pop_DA_var_HRname_sub$PHU_name == income_pop_DA_var_HRname_sub$HRname)

table(income_pop_DA_var_HRname_sub$HRname, exclude = NULL)
table(income_pop_DA_var_HRname_sub$PHU_name, exclude = NULL)

income_var_check_deci =
  subset(income_pop_DA_var_HRname_sub, d_after_tax_income_PPE_rank_deci != `After tax income decile (PHU-ranked)`)[, c("d_after_tax_income_PPE_rank_deci",
                                                                                                                       "After tax income decile (PHU-ranked)",
                                                                                                                       "HRname",
                                                                                                                       "d_after_tax_income_PPE")]
nrow(income_var_check_deci) #N = 3 not the same

income_var_check_quin =
  subset(income_pop_DA_var_HRname_sub, d_after_tax_income_PPE_rank_quin != `After tax income quintile (PHU-ranked)`)[, c("d_after_tax_income_PPE_rank_quin",
                                                                                                                       "After tax income quintile (PHU-ranked)",
                                                                                                                       "HRname",
                                                                                                                       "d_after_tax_income_PPE")]
nrow(income_var_check_quin) #N = 2 not the same

###################################
# d_public_transportation
income_pop_DA_var_HRname_sub$d_public_transportation_rank_quin = quin(d_public_transportation)
income_pop_DA_var_HRname_sub$d_public_transportation_rank_deci = deci(d_public_transportation)
income_pop_DA_var_HRname_sub$d_public_transportation_rank_tert = tertile(d_public_transportation)

table(income_pop_DA_var_HRname_sub$d_public_transportation_rank_quin, exclude = NULL)
table(income_pop_DA_var_HRname_sub$d_public_transportation_rank_deci, exclude = NULL)
table(income_pop_DA_var_HRname_sub$d_public_transportation_rank_tert, exclude = NULL)

#######################################################################################################################
write.csv(income_pop_DA_var_HRname_sub, "./data/private/r_processed/DA_total_income_pop_DA_var_HRname_sub.csv", row.names = F)
colnames(income_pop_DA_var_HRname_sub)

