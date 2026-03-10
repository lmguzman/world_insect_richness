###Colwell et al - species estimate 2025 - R codes###

library(tidyverse)
library(VennDiagram)
library(dplyr)

Data=read.delim(file = "Data/raw/CR_all_rec_report_output.tsv", sep = '\t')


###count full ACG
allACG_1=Data%>%
  filter((str_detect(fieldid, "SRNP")))

allACG_3=Data%>%
  filter(str_detect(subfamily, "Microgastrinae"))%>%
  filter((str_detect(extrainfo,"ALTM|BARB|Ceibo|Hitoy|PE-SINAC|ICOCO|Quetzales|Sirena|TORTUG|Baru|Boconera|DOLE|Kasiiya")))%>%
  filter(!uri=="")%>%
  distinct(uri, .keep_all = TRUE)

allACG_2=Data%>%
  filter(str_detect(subfamily, "Microgastrinae"))%>%
  filter((str_detect(extrainfo,"ESG|BSE|PL12|Derrumbe|SSM|SMNR|SMNPL|Pedregal|Harold|Circular|Gongora|Cima|Arenales|CJAN|NAR|SGF|SGC|BT0|LDR|MBT|Sombra|LuzSol|Pitilla|malaise-trapped")))%>%
  filter(!uri=="")%>%
distinct(uri, .keep_all = TRUE)
                                                      
allACG_M=allACG_2%>%
  #filter(str_detect(subfamily, "Microgastrinae"))%>%
  #filter((str_detect(extrainfo,"ESG|BSE|PL12|Derrumbe")))%>%
  #filter((str_detect(class, "Insecta")))%>%
  filter(!uri=="")%>%
  group_by(uri) %>%
  dplyr::summarize(count = n()) %>%
  ungroup()


allACG=rbind(allACG_1,allACG_2)
allACGInsects=allACG%>%
  #filter((str_detect(extrainfo,"ESG|BSE|PL12|Derrumbe")))%>%
  filter((str_detect(class, "Insecta")))%>%
  filter(!uri=="")%>%
  group_by(uri) %>%
  dplyr::summarize(count = n()) %>%
  ungroup()

###count reared BINs in ACG
query_reared = Data%>%
  filter(str_detect(subfamily, "Microgastrinae"))%>%
  filter((str_detect(fieldid, "SRNP")))%>%
  filter((nchar(fieldid)<=13))%>%
  filter(!str_detect(extrainfo,"malaise|Malaise"))%>%
  filter(!extrainfo=="")%>%
  filter(!str_detect(extrainfo,"net|light"))%>%
  filter(!uri=="")%>%
  arrange(lat) %>%
  filter(duplicated(lat) == FALSE)

reared_tax=query_reared%>%
  filter((str_detect(family, "Braconidae|Ichneumonidae")))%>%
  group_by(uri) %>%
  summarize(count = n()) %>%
  ungroup()

reared_tax_host=query_reared%>%
  filter((str_detect(family, "Braconidae|Ichneumonidae")))%>%
  group_by(extrainfo) %>%
  summarize(count = n()) %>%
  ungroup()

reared_count = query_reared %>%
  group_by(uri) %>%
  summarize(count = n()) %>%
  ungroup()

write.csv(allACG_M,"Data/Microgastrines_forTree.csv")

###count Malaise BINs in ACG

##Filter All ACG Malaise
query_allACG = Data%>%
  #filter(str_detect(subfamily, "Microgastrinae"))%>%
  filter((str_detect(extrainfo,"ESG|BSE|PL12|Derrumbe|SSM|SMNR|SMNPL|Pedregal|Harold|Circular|Gongora|Cima|Arenales|CJAN|NAR|SGF|SGC|BT0|LDR|MBT|Sombra|LuzSol|Pitilla|malaise-trapped")))%>%
  filter(!uri=="")

allMalaise_count = query_allACG %>%
  filter((str_detect(class, "Insecta")))%>%
  group_by(uri) %>%
  dplyr::summarize(count = n()) %>%
  ungroup()

allMalaise_count_richness = allMalaise_count %>%
  count(count)%>%
  complete(count=1:748,fill = list(n= 0))
allMalaise_count_richness =as.data.frame(allMalaise_count_richness)

write.csv(reared_count,"Data/Micrograstrinae_AGCAllNov2024.csv")
  
##Filter Core ACG (Traps that have been completely processed)
query_coreACG = Data%>%
  #filter(str_detect(subfamily, "Microgastrinae"))%>%  
  filter((str_detect(extrainfo,"ESG|BSE|PL12|Derrumbe")))%>%
  filter(!uri=="")
  
seqACG_count = query_coreACG %>%
  group_by(nucraw_length) %>%
  summarize(count = n()) %>%
  ungroup()

seqACG_na=query_coreACG %>%
  filter(is.na(nucraw_length))%>%
  group_by(order) %>%
  summarize(count = n()) %>%
  ungroup()

coreACG_count = query_coreACG %>%
  filter (str_detect(class,"Insecta"))%>%
  filter(str_detect(subfamily, "Microgastrinae"))%>%
  group_by(uri) %>%
  dplyr::summarize(count = n()) %>%
  ungroup()

orderACG_count = query_coreACG %>%
  group_by(order) %>%
  summarize(count = n()) %>%
  ungroup()

coreACG_count_richness = coreACG_count %>%
  count(count)%>%
  complete(count=1:403,fill = list(n= 0))
coreACG_count_richness =as.data.frame(coreACG_count_richness)

  
##Filter peripheral ACG (traps that have only been partially processed)
query_peripherieACG = Data%>%
    filter(str_detect(subfamily, "Microgastrinae"))%>%
    filter((str_detect(extrainfo,"SSM|SMNR|SMNPL|Pedregal|Harold|Circular|Gongora|Cima|Arenales|CJAN|NAR|SGF|SGC|BT0|LDR|MBT|Sombra|LuzSol|Pitilla|malaise-trapped")))%>%  
    filter(!uri=="")
    
peripherie_count = query_peripherieACG %>%
  group_by(uri) %>%
  dplyr::summarize(count = n()) %>%
  ungroup()

peripherie_trap = query_peripherieACG %>%
  group_by(lat) %>%
  summarize(count = n()) %>%
  ungroup()

peripherie_count_richness = peripherie_count %>%
  count(count)%>%
  complete(count=1:692,fill = list(n= 0))
peripherie_count_richness =as.data.frame(peripherie_count_richness)


##reared vs malaise
overlap_reared=peripherie_count%>%
  inner_join(coreACG_count, by="uri")

##both malaise
overlap_malaise=reared_count%>%
  inner_join(allMalaise_count, by="uri")

write.csv(peripherie_count,"Data/Micrograstrinae_AGCPeripherieNov2024.csv")

  
##Bar chart displaying BINs per taxon  
coreACGBIN_count = query_coreACG %>%
  group_by(order) %>%
  summarise(distinctBINs= n_distinct(uri))
  
ggplot(coreACGBIN_count,aes(x=reorder(order, -distinctBINs), y=distinctBINs, color=order, fill=order))+
  geom_bar(stat="identity")+
  theme(legend.position = "none")+
  theme(axis.text.x = element_text(angle=75, hjust=1, size =15),
        axis.text.y=element_text(size=15),
        axis.title=element_text(size=20,face="bold"))+
  coord_cartesian(ylim = c(0, 10000))+
  scale_y_continuous(expand = c(0,0))+
  geom_text(aes(label=distinctBINs), vjust=-0.5, color="black",
            position = position_dodge(0.5), size=5)+
  labs(y= "BIN count", x = "Order")+
  theme(
    # Hide panel borders and remove grid lines
    axis.line=element_line(size=0.25),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank())
  

##overlap reared ACG and malaise ACG
# query_reared_single = query_reared%>%
#   distinct(uri, .keep_all = TRUE)
# 
# ovl_2=
#   query_reared_single%>%
#   inner_join(AllACG3, by="uri")
# 
# perct_all=nrow(ovl_2)/nrow(query_reared_single)
# estimate_all=nrow(query_reared_single)/perct_all
# 
# 
# #empty plot cache (repeat every time before plotting a diagram)
# while (dev.cur()>1) dev.off()
# 
# #draw pairwise Venn diagram (for category attribute, any label can be used)
# draw.pairwise.venn(
#   area1=round(av_tes1),
#   area2=round(av_tes2),
#   cross.area = round(av_over),
#   category=c("sample 1","sample 2"),
#   fill=c("red","green"),
#   cex=c(3,3,3))
