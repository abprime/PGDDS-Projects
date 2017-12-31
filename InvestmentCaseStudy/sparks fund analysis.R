
#Loading tidyr and dplyr library
library(tidyr)
library(dplyr)
#Checkpoint 1----
#1. Loading data
companies <- read.delim("companies.txt", stringsAsFactors = T)
rounds2 <- read.csv("rounds2.csv", stringsAsFactors = T)

#changing cases to lower
companies$permalink <- tolower(companies$permalink)
rounds2$company_permalink <- tolower(rounds2$company_permalink)

#unique companies in both rounds2 & companies  - count of permalink
length(unique(tolower(rounds2$company_permalink)))
length(unique(tolower(companies$permalink)))

#Are there any companies in the rounds2 file which are not present in companies? Answer yes or no
#by using setdiff() function of dplyr package we can check.
setdiff(rounds2$company_permalink,companies$permalink)


#merging to master frame
master_frame<-merge(rounds2, companies, by.x = "company_permalink", by.y = "permalink")

#How many observationn present in master_frame
nrow(master_frame)



#Checkpoint 2----

#Average funding amount for venture,seed,angel,private_equity
average_funding_type_investments<-aggregate(raised_amount_usd~funding_round_type , data = master_frame, FUN=mean,na.rm=TRUE)
average_particlar_funding_type_investments<-subset(average_funding_type_investments,funding_round_type %in% c("venture","angel","seed","private_equity"))
average_particlar_funding_type_investments

#investment type which is most suitable


funding_type_investments <- subset(average_funding_type_investments, funding_round_type %in% c("venture", "angel", "seed", "private equity") 
                                   & raised_amount_usd>=5000000 & raised_amount_usd<=15000000)

ft <- funding_type_investments$funding_round_type[which.max(funding_type_investments$raised_amount_usd)]
ft <- as.character(ft)

ft


#english speaking countries.we are just maanually created a vector by refferring to the englishspeaking country PDF mentioned in the instruction.
english_countries<-c("BWA","CMR","ETH","ETF","ERI","GMB","GHA","KEN","LSO","LBR","MWI","MUS","NAM","NGA","RWA","SYC","SLE","ZAF","SDN","SWZ","TZA","UGA","ZMB","ZWE","ATG","BHS","BRB","BLZ","CAN","DMA","GRD","GUY","JAM","KN1","LCA","VCT","TTO","USA","IND","PAK","PHL","SGP","AUS","FJI","KIR","MHL","FSM","NRU","NZL","PLW","PNG","WSM","SLB","TON","TUV","VUT","IRL","MLT","GBR")

#subseting ft investment type data
ft_data<-subset(master_frame, funding_round_type==ft)

#country wise aggregation for finding the top invested 9 countries
country_aggregate <- aggregate(raised_amount_usd~country_code, data = ft_data, FUN=sum,na.rm=T)

#top9 countries
top9 <- head(country_aggregate$country_code[order(country_aggregate$raised_amount_usd, decreasing = TRUE)], n=9)



#top 3 countries from top9 countries
countries <- as.character(head(top9[top9 %in% english_countries], n = 3))
countries



#Checkpoint 4####
#load mapping data
mapping <- read.csv("mapping.csv")
#removing blanks from mapping as it is not one of the 8 main sectors
mapping <- mapping[!(mapping$Blanks==1),]

# replacing 0 with na in category_list column without affecting Enterprise2.0

mapping$category_list <- lapply(lapply(mapping$category_list, gsub, pattern = "0", replacement = "na", fixed = TRUE), gsub, pattern = ".na", replacement = ".0", fixed = TRUE)

#converting wide format to long format
mapping <- gather(mapping, main_sector, sector_val, Automotive...Sports:Social..Finance..Analytics..Advertising)
mapping <- mapping[!(mapping$sector_val==0),]
mapping <- mapping[,!(names(mapping) %in% "sector_val")]


#splitting category into primary and secondary
ft_category_sep<-separate(ft_data, category_list, into = c("primary_sector", "sec_sector"), sep = "\\|",fill = "right", extra = "merge")

#merging investment data with category mappings
ft_merged_category <- merge(ft_category_sep, mapping, by.x = "primary_sector", by.y = "category_list")




#Checkpoint 5
#subseting merged data
d1 <- subset(ft_merged_category, country_code==countries[1] & raised_amount_usd<=5000000 & raised_amount_usd>=15000000)
d2 <- subset(ft_merged_category, country_code==countries[2] & raised_amount_usd<=5000000 & raised_amount_usd>=15000000)
d3 <- subset(ft_merged_category, country_code==countries[3] & raised_amount_usd<=5000000 & raised_amount_usd>=15000000)

d1_sector_grp<-group_by(d1, main_sector)
d2_sector_grp<-group_by(d2, main_sector)
d3_sector_grp<-group_by(d3, main_sector)


#Adding columns
#The total number (or count) of investments for each main sector in a separate column
#The total amount invested in each main sector in a separate column
d1 <- merge(d1, summarize(d1_sector_grp, no_of_investment=length(company_permalink)), by = "main_sector")
d1 <- merge(d1, summarize(d1_sector_grp, total_investment=sum(raised_amount_usd)), by = "main_sector")

d2 <- merge(d2, summarize(d2_sector_grp, no_of_investment=length(company_permalink)), by = "main_sector")
d2 <- merge(d2, summarize(d2_sector_grp, total_investment=sum(raised_amount_usd)), by = "main_sector")

d3 <- merge(d3, summarize(d3_sector_grp, no_of_investment=length(company_permalink)), by = "main_sector")
d3 <- merge(d3, summarize(d3_sector_grp, total_investment=sum(raised_amount_usd)), by = "main_sector")

d1
d2
d3

#count of investments for each country
no_of_invstmnts_per_cntry <- lapply(list(d1,d2,d3),FUN=nrow)
no_of_invstmnts_per_cntry

#sum of investments
total_invstmnts_per_cntry <- lapply(list(d1,d2,d3), FUN = function(x){sum(x[,"raised_amount_usd"],na.rm=T);})
total_invstmnts_per_cntry

############################
#helper functions
#function to find nth max value for a column in a data frame
nmax  <- function(df, col, n){
  return(df[order(df[,col],decreasing = T),][n,]);
}

#function to get the top nth sector and the number of investments from the data frame
get_topn_sector_name_count <- function(df, n){
  df1<-as.data.frame(summarise(df, no_of_investments=length(main_sector)));
  return(nmax(df1, "no_of_investments", n));
}
############################



#top sector no of investment wise, and number of investments
top_sector_cntry_wise <- lapply(list(d1_sector_grp,d2_sector_grp,d3_sector_grp), FUN = function(x){
  get_topn_sector_name_count(x, 1);
})
top_sector_cntry_wise


#2nd top sector based on investment, and number of investments
second_top_sector_cntry_wise <- lapply(list(d1_sector_grp,d2_sector_grp,d3_sector_grp), FUN = function(x){
  get_topn_sector_name_count(x, 2);
})
second_top_sector_cntry_wise

#3rd top sector based on investment, and number of investments
third_top_sector_cntry_wise <- lapply(list(d1_sector_grp,d2_sector_grp,d3_sector_grp), FUN = function(x){
  get_topn_sector_name_count(x, 3);
})
third_top_sector_cntry_wise


#######################
#function to get the company with highest investment in top invested sector
get_topnsector_company <- function(df,n){
  topn_sector<-as.character(get_topn_sector_name_count(df, n)[,"main_sector"]);
  df_top_sector<-as.data.frame(subset(df, main_sector==topn_sector));
  company_investment <- aggregate(raised_amount_usd~company_permalink+name, df_top_sector, sum)
  name <- company_investment$name[which.max(company_investment$raised_amount_usd)]
  return(as.character(name));
}
#######################



#For point 3 (top sector count-wise), which company received the highest investment?
company_name_top_sectr <- lapply(list(d1_sector_grp,d2_sector_grp,d3_sector_grp), FUN=function(x){
  get_topnsector_company(x, 1);
})
company_name_top_sectr


#For second-best sector count-wise (point 4), which company received the highest investment?
company_name_2nd_top_sectr <- lapply(list(d1_sector_grp,d2_sector_grp,d3_sector_grp), FUN=function(x){
  get_topnsector_company(x, 2);
})
company_name_2nd_top_sectr
