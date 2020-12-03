library(data.table)
library(dplyr)
library(tidyr)
library(ggplot2)
library(cowplot)
library(stargazer)
library(stringr)
library(zoo)
library(RColorBrewer)

#--------- DIRECTORIES

direc_input  =  'C:/Users/julia/OneDrive/Documentos/4intelligence/Challenge_4i'
direc_output =  'C:/Users/julia/OneDrive/Documentos/4intelligence/Challenge_4i/output'

#--------- FILES

comexstat   = 'data_comexstat.csv'

#--------- SIMPLE FUNCTIONS

tlag = function(x, n = 1L, time) { 
  index <- match(time - n, time, incomparables = NA)
  x[index]
}
#-------- SETTING DIRECTORY

setwd(direc_output)

#--------- EXERCISE


#--------- PART I : Evolution of total exports

data_cx = fread(paste(direc_input,comexstat,sep = "/"),sep=",",keepLeadingZeros = T)

data_cx = data_cx %>% mutate(date = as.Date(date,"%Y-%m-%d"))

unique(data_cx$product)
unique(data_cx$route)

#Agreggating comex data to national level
BR_cx = data_cx %>% select(-state)%>%
         group_by(product,date,route,country,type)%>%
          mutate(tons=sum(tons),usd=sum(usd)) %>%
           distinct%>%
            ungroup

#---- MONTHLY DATA

#-  Filtering and aggregating data
dt_graph1 = BR_cx %>% 
             filter(type == 'Export', product %in% sort(unique(data_cx$product))[2:4]) %>%
              group_by(date,product) %>%
               transmute(tons=sum(tons)/10^9,usd=sum(usd)/10^9)%>%
                distinct%>%
                 ungroup
  
s_usd = dt_graph1 %>% select(-tons) %>%
             spread(product,usd) %>%
               arrange(date)

#- Calculating moving average
s_usd = s_usd %>% 
  mutate(s_meal_ma = rollmean(soybean_meal, k = 11, fill = NA, align ='left'),
         s_oil_ma  = rollmean(soybean_oil,  k = 11, fill = NA, align ='left'),
         s_ma      = rollmean(soybeans,     k = 11, fill = NA, align ='left'))

s_usd = s_usd %>%
  gather(key = "variable", value = "value", -date)

#- Plots
var = unique(s_usd$variable)

for (i in 1:3){
  A=s_usd %>% filter(variable %in% c(var[i],var[i+3]),substr(date,1,4)>=2000)%>%
    ggplot(aes(x = date, y = value)) + 
    geom_line(aes(color = variable))+
    theme_minimal()+
    theme(legend.position = 'none', plot.title = element_text(size=10,face='bold'),
          axis.text.y = element_text(size=8),axis.text.x = element_text(size=8))+
    ggtitle(str_to_title(gsub(var[i],replacement = " ",pattern = "\\_")))+
    xlab("")+
    ylab("")
  
  assign(paste("g",i,sep="_"),A)
}

plot_grid(g_1,g_2,g_3,ncol=1)
ggsave("case1_1_month.png")

rm(list=ls(pattern="g\\_"))

#---- ANNUAL DATA

#- Filtering and aggregating data
dt_graph2 = BR_cx %>% 
  filter(type == 'Export', product %in% sort(unique(data_cx$product))[2:4]) %>%
  mutate(year = as.numeric(substr(date,1,4)))%>%
  select(-date)%>%
  group_by(year,product) %>%  
  transmute(tons=sum(tons)/10^9,usd=sum(usd)/10^9)%>%
  distinct%>%
  ungroup

var = unique(dt_graph2$product)

#-Plots
for(i in 1:3){
  A=dt_graph2 %>% filter(product == var[i])%>%
    ggplot(aes(x = year, y = usd, group = product)) + 
    geom_line(color='red')+
    theme_minimal()+  xlab("")+ ylab("")+
    ggtitle(str_to_title(gsub(var[i],replacement = " ",pattern = "\\_")))+
    theme(legend.position = 'none', plot.title = element_text(size=10,face='bold'),
          axis.text.y = element_text(size=8),axis.text.x = element_text(size=8))
  
  assign(paste("g",i,sep = "_"),A)
}

plot_grid(g_1,g_2,g_3,ncol=1)
ggsave("case1_1_anual.png")

rm(list=ls(pattern="dt_graph"))
rm(list=ls(pattern="\\_usd"))
rm(list=ls(pattern="g\\_"))
rm(A)


#-------------------------------------------------------------------------------
# We don't need monthly data anymore
data_cx = data_cx %>% mutate(year = as.numeric(substr(date,1,4))) %>% select(-date)

#--------- PART II: Top 3 products

#- Filtering and aggregating data
dt_graph1 = BR_cx %>% ungroup %>% mutate(year = as.numeric(substr(date,1,4))) %>%
            filter(year>=max(year)-5, type == 'Export')%>%
            group_by(product,year)%>%
            transmute(usd=sum(usd)/10^9,tons=sum(tons)/10^9)%>%
            ungroup%>%
            distinct

dt_graph1 = dt_graph1 %>%
  mutate(product = str_to_title(gsub(pattern="\\_",replacement=" ",product)))

#-Plot
ggplot(dt_graph1, aes(fill=product, y=usd, x=year)) + 
  geom_bar(position="fill", stat="identity")+
  theme_minimal()+
  ylab("Share in total value")+
  xlab("Year")+
  scale_fill_brewer(palette='Set2')+
  theme(legend.title = element_blank())+
  scale_y_continuous(labels = scales::percent)
ggsave('case1_2_usd.png')

#-Table
dt_graph1 %>% group_by(year)%>%
  mutate(shr_usd = (usd/sum(usd))*100)%>%
  ungroup%>%
  group_by(product) %>%
  mutate(growth_tons = paste(round(((tons/tlag(tons,5,year))-1)*100,2),"%",sep=""),
         growth_usd  = paste(round(((usd/tlag(usd ,5,year))-1)*100,2),"%",sep = ""),
         delta_shr   = paste(round(shr_usd-tlag(shr_usd ,5,year),2),"p.p.",sep = ""))%>%
  ungroup %>%
  filter(year==max(year))%>%
  select(product,growth_tons,growth_usd,delta_shr,shr_usd)%>%
  mutate(shr_usd = paste(round(shr_usd,2),"%",sep=""))%>%
  rename(Product = product, `Change in Share`=delta_shr, `Change in 2019`=shr_usd,
         `Gr. of Volume` =growth_tons,`Gr. of Value`=growth_usd) %>%
  stargazer(summary = F, rownames = F, out="Table1.tex")

rm(list=ls(pattern="dt_graph"))

#--------- PART III: Top 3 routes

#-Filtering and aggregating data: Product-year-type-route level
dt_graph1 = BR_cx %>% ungroup%>% mutate(year = as.numeric(substr(date,1,4))) %>%
  filter(year>=max(year)-3, type == 'Export')%>%
  group_by(product,year,type,route)%>%
  transmute(tons=sum(tons)/10^9,usd=sum(usd)/10^9)%>%
  ungroup%>%
  distinct

dt_graph1 = dt_graph1 %>%
  mutate(product = str_to_title(gsub(pattern="\\_",replacement=" ",product)))

#-Plots
dt_graph1%>%filter(product=="Corn")%>%
ggplot(aes(fill=route, y=usd, x=year)) + 
  geom_bar(position="fill", stat="identity")+
  theme_minimal()+
  ylab("Share in total volume")+
  xlab("Year")+
  scale_fill_brewer(palette='Set2')+
  theme(legend.title = element_blank())+
  ggtitle("Volume of Exports of Corn by Routes")
ggsave('case1_3_cornxroutes.png')

#-Filtering and aggregating data: Product-state-route level in 2019
dt_graph2 = data_cx %>% 
  filter(type == 'Export',year == max(year))%>%
  group_by(product,route,state)%>%
  mutate(tons=sum(tons),usd=sum(usd)) %>%
  distinct%>%
  ungroup

dt_graph2 %>% filter(product=="corn")%>%
ggplot(aes(fill=route, y=usd, x=state)) + 
  geom_bar(position="fill", stat="identity")+
  theme_minimal()+
  ylab("Share in total value")+
  xlab("Year")+
  ggtitle("Volume of Exports of Corn in 2019 \n
          by Routes and Estates")+
  scale_fill_brewer(palette='Set2')+
  theme(legend.title = element_blank())+
  theme(axis.text.x = element_text(angle = 90,size=8))
ggsave('case1_3_corn2019xroutesxstates.png')


#-Filtering and aggregating data: Product-country-route level in 2019
dt_graph3 = data_cx %>% 
  filter(type == 'Export',year == max(year), product == "corn")%>%
  group_by(product,route,country)%>%
  mutate(tons=sum(tons),usd=sum(usd)) %>%
  distinct%>%
  ungroup

#-Plots
threshold = quantile(dt_graph3$tons,probs=c(0.80))

dt_graph3 %>% filter(tons>=threshold)%>%
  ggplot(aes(fill=route, y=tons, x=country)) + 
  geom_bar(position="fill", stat="identity")+
  theme_minimal()+
  ylab("Share in total value")+
  xlab("Year")+
  ggtitle("Volume of Exports of Corn in 2019 \n
          by Routes and Destinations")+
scale_fill_brewer(palette='Set2')+
  theme(legend.title = element_blank())+
  theme(axis.text.x = element_text(angle = 90,size=8))
ggsave('case1_3_corn2019xroutesxcountries_high.png')

threshold = quantile(dt_graph3$tons,probs=c(0.10))

dt_graph3 %>% filter(tons<=threshold)%>%
  ggplot(aes(fill=route, y=tons, x=country)) + 
  geom_bar(position="fill", stat="identity")+
  theme_minimal()+
  ylab("Share in total value")+
  xlab("Year")+
  ggtitle("Volume of Exports of Corn in 2019 \n
          by Routes and Destinations")+
  scale_fill_brewer(palette='Set2')+
  theme(legend.title = element_blank())+
  theme(axis.text.x = element_text(angle = 90,size=8))
ggsave('case1_3_corn2019xroutesxcountries_low.png')

rm(list=ls(pattern="dt_graph"))

#--------- PART IV: TOP PARTNERS

#-Filtering and aggregating data: Product-product-country level in 2016-2019
dt_tab1 = data_cx %>%
  filter(year>=max(2019)-3,product %in% sort(unique(data_cx$product))[c(1,5)],type=='Export')%>%
  group_by(year,product,country)%>%
  transmute(tons = sum(tons)/10^6,usd=sum(usd)/10^9)%>%
  ungroup %>%
  distinct

#- Ranking destinations by value
dt_tab1 =dt_tab1 %>% group_by(product,year) %>%
  mutate(rank = dense_rank(-usd))%>%
  ungroup %>%
  filter(rank<=5)%>% 
  arrange(product,rank) %>%
  mutate(product = str_to_title(gsub(pattern="\\_",replacement=" ",product)))

dt_tab1 =dt_tab1 %>% 
  mutate(country = ifelse(country == "United Arab Emirates","UAE",country))

products = str_to_title(gsub(unique(data_cx$product),pattern="\\_",replacement=" "))[c(1,5)]

#Plots
for (i in 1:length(products)){
  for (j in 2017:2019){
  
  if (i==1){  
  clr='darkgoldenrod1'
  }else{
  clr='darkcyan'
  }
  a=ggplot(filter(dt_tab1,product==products[i],year==j), aes(y = usd, x = reorder(country,-rank)))+
    geom_col(color=clr, fill=clr) +
    coord_flip()+xlab("")+ylab("")+
    ggtitle(j)+
    theme_minimal()+
    theme(legend.position = 'none', plot.title = element_text(size=10,face='bold'),
          axis.text.y = element_text(size=8),axis.text.x = element_text(size=8))
  assign(paste("g",i,j,sep="_"),a)  
  }
}

plot_grid(g_1_2017,g_1_2018,g_1_2019,nrow = 1)
ggsave("top_imp_corn.png")

plot_grid(g_2_2017,g_2_2018,g_2_2019,nrow = 1)
ggsave("top_imp_sugar.png")

rm(list=ls(pattern="g\\_"))

#--------- PART V: TOP EXPORTERS STATES
#-Filtering and aggregating data: Product-product-state level in 2016-2019
dt_tab1 = data_cx %>%
  filter(year>=max(2019)-3)%>%
  group_by(year,product,state)%>%
  transmute(tons = sum(tons)/10^6,usd=sum(usd))%>%
  ungroup %>%
  distinct

dt_tab1 = dt_tab1 %>% 
  group_by(product,state)%>%
  summarise(tons=mean(tons),usd=mean(usd))%>%
  ungroup

#- Ranking states by volume
dt_tab1 =dt_tab1 %>% group_by(product) %>%
  mutate(rank = dense_rank(-tons))%>%
  ungroup %>%
  filter(rank<=5)%>% 
  arrange(product,rank) %>%
  mutate(product = str_to_title(gsub(pattern="\\_",replacement=" ",product)))

products = str_to_title(gsub(unique(data_cx$product),pattern="\\_",replacement=" "))

#-Plots
for (i in 1:length(products)){
  a=ggplot(filter(dt_tab1,product==products[i]), aes(y = tons, x = reorder(state,-rank)))+
    geom_col(aes(fill = product), width = 0.7, color='red') +
    coord_flip()+xlab("")+ylab("")+
    ggtitle(products[i])+
    theme_minimal()+
    theme(legend.position = 'none', plot.title = element_text(size=10,face='bold'),
          axis.text.y = element_text(size=8),axis.text.x = element_text(size=8))
assign(paste("g",i,sep="_"),a)  
}

plot_grid(g_1,g_2,g_3,g_4,g_5,g_6)
ggsave("top_exp.png")

rm(list=ls(pattern="a\\_"))
rm(a)
rm(dt_tab1)