## This script is used to recreate Figure 2
## please contact ebellis@astate.edu with any questions!

library(ggdendro)
library(dplyr)
library(ggplot2)
library(tidyr)
library(cowplot)

setwd('/Users/emilywork/Documents/GitHub/StrigaMacroecologyMS')

################  load gps points for experimental studies
meta <- read.csv('DataFiles/SI.dat.1.30.20.csv')
meta <- subset(meta, emergence != "NA") # 27 localities

############## fig for relative emergence. 
tmp <- meta %>% dplyr::select(locality, emergence, host) %>% group_by(locality, host) %>% summarize(emg = mean(emergence))
tmp2 <- tmp %>% group_by(locality) %>% summarize(total=sum(emg))
tmp3 <- inner_join(tmp, tmp2)

tmp3$locality <- factor(tmp3$locality, levels = c("Bambey", "Maradi", "Mintimbougou","FarakoBa","SinthionMaleme","Samaru","AbuNaama","Kamboinse","Kobo","Yendi","Tamale","WadMedani","Galadima","Bakura","Damaturu","Jibiya","Andre","Kankia","Bida","Gombe","Gumi","Busia","Gwoza","Mokwa","Ngezima","Kazgail","Obeid"))

p <- ggplot(tmp3, aes(x=locality, y=emg, col=host, fill=host)) + geom_bar(position="stack", stat="identity", alpha=0.4) +theme_minimal()+ scale_colour_manual(values=c('gold2','plum','sienna3'), labels=c("maize", "pearl millet","sorghum"), name="Host") + scale_fill_manual(values=c('gold2','plum','sienna3'), labels=c("maize", "pearl millet","sorghum"), name="Host")+ theme(axis.text.x=element_text(angle=90, vjust=0.4, hjust=1), legend.position="bottom",panel.grid.major = element_blank(),panel.grid.minor = element_blank()) + ylab("Relative emergence") + xlab("")
p <- p + geom_vline(xintercept=2.5, lwd=0.3, lty=2) + geom_vline(xintercept=5.5, lwd=0.3, lty=2) + geom_vline(xintercept=12.5, lwd=0.3, lty=2) +  geom_vline(xintercept=15.5, lwd=0.3, lty=2) + annotate('text', x=26, y = 1.7, label="*", size=7)+ annotate('text', x=27, y = 1.7, label="*", size=7) + annotate('text', x=1.5, y=2.1, label="1", size=6) + annotate('text', x=4, y=2.1, label="2", size=6)+ annotate('text', x=9, y=2.1, label="3", size=6)+ annotate('text', x=20, y=2.1, label="5", size=6)+ annotate('text', x=14, y=2.1, label="4", size=6)

host.df <- as.data.frame(as.data.frame(tmp) %>% pivot_wider(id_cols=c(locality,host), names_from= host,values_from=emg))
host.df <- subset(host.df, maize!="NA")
row.names(host.df) <- host.df$locality
dd <- as.dist(1-cor(t(host.df[,2:4])))
hc <- hclust(dd, method="complete")

q <- ggdendrogram(hc,  labels=F) + geom_hline(yintercept=0.5, lty=2) + ylim(c(0,2.2)) + xlim(c(0.5,27)) + theme_dendro()

pdf(file="Fig2_Clustering.pdf", width=6, height=3.2)
plot_grid(q, p,labels=c('A','B'), nrow = 2, rel_heights = c(1,2))
dev.off()
