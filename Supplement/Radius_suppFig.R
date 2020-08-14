###### supplemental fig radius of specialization  
library(ggplot2)
library(tidyr)
library(dplyr)

df <- read.csv('SI.dat.3.31.20.csv', header=T)
df <- df %>% select(locality, ENM_avz, ENM_avm, ENM_avs, radius) %>% unique() %>% group_by(locality, radius) %>% pivot_longer(c(ENM_avz, ENM_avm, ENM_avs))
df <- as.data.frame(df)
colnames(df)[3] <- "ENM"

enm <- c(
  "ENM_avm" = "Millet",
  "ENM_avs" = "Sorghum",
  "ENM_avz" = "Maize"
)

pdf("Radius_suppFig.pdf", height=2.5, width=6.8)
ggplot(df, aes(x=(radius/1000), y=value, group=locality)) + geom_line(lwd=0.3) + facet_grid(.~ENM, labeller=labeller(ENM = enm)) + theme_classic() + xlab("Radius (km)") + ylab("Predicted specialization")
dev.off()


#######which pops are scale variant? (cut-off for difference is HS >0.2)
tmp <- (subset(df, radius==10000 | radius==170000) %>%filter(ENM=="ENM_avs")%>% select(-ENM)) %>% group_by(locality) %>%pivot_wider(id_cols=c(locality), names_from=radius, values_from=value)  

tmp$diff <- as.data.frame(tmp)[,2]-as.data.frame(tmp)[,3] 

subset(tmp, abs(diff)>0.2 )  #bambey for sorghum; busia/gumi for maize