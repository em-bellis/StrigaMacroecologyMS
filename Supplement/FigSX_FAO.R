################### Supplemental figure for FAO data
library(tidyverse)
library(patchwork)

### manipulate df to have fraction of total area harvested for each crop
fao <- read.csv('FAOSTAT_data_1-11-2021.csv', header = T, stringsAsFactors = T)
fao <- dplyr::select(fao, c("Area","Item", "Year", "Value"))
fao_wide <- tidyr::pivot_wider(fao, names_from = Item, values_from = Value)
fao_wide[is.na(fao_wide)] <- 0 #replace NA with 0
fao_wide$Total <- fao_wide$Maize + fao_wide$Millet + fao_wide$`Rice, paddy` + fao_wide$Sorghum + fao_wide$`Sugar cane`
fao_wide$sor_p <- fao_wide$Sorghum/fao_wide$Total
fao_wide$sug_p <- fao_wide$'Sugar cane'/fao_wide$Total
fao_wide$mze_p <- fao_wide$Maize/fao_wide$Total
fao_wide$mil_p <- fao_wide$Millet/fao_wide$Total
fao_wide$ric_p <- fao_wide$'Rice, paddy'/fao_wide$Total
colnames(fao_wide)[3:7] <- c("Sorghum","Sugarcane","Maize","Millet","Rice")
fao_long <- dplyr::select(fao_wide, c(Area, Year, sor_p, sug_p, mze_p, mil_p, ric_p)) %>%
  dplyr::rename("Sorghum" = sor_p, "Sugarcane" = sug_p, "Maize" = mze_p,"Millet"= mil_p, "Rice" = ric_p) %>%
  tidyr::pivot_longer(cols = c(Sorghum, Sugarcane, Maize, Millet, Rice))

### a figure of Sudan + South Sudan
sud <- dplyr::filter(fao_long, Area == c("Sudan","South Sudan","Sudan (former)"))
sud$id <- paste0(sud$name,sud$Area)
sud_p <- ggplot(sud, aes(x=Year, y=value*100, group=id, col=name, lty=Area)) + geom_line(size=1.25) +
  theme_classic() + scale_linetype_manual(values = c(9, 1, 1), name="Country") + 
  ylab("") +
  xlab("") +
  scale_color_manual(values = c('gold2','plum', "plum4",'sienna3', "grey80"), name="Host") +
  geom_vline(xintercept = 1983, lty=2, size=0.25, col="grey40") +
  geom_vline(xintercept = 1979, lty=2, size=0.25, col="grey40") +
  geom_vline(xintercept = 2000, size=0.25, col="grey40") +
  # annotate("text", x=1969, y=80, label="Ram83", size=3.5, col="grey40") +
  # annotate("text", x=1992, y=60, label="Beb86", size=3.5, col="grey40") +
  ylim(c(0,100)) +
  ggtitle("Sudan, \nSouth Sudan") +
  theme(legend.position = "none")

### a figure of Nigeria 
nig <- dplyr::filter(fao_long, Area == "Nigeria")
nig_p <- ggplot(nig, aes(x=Year, y=value*100, col=name)) + geom_line(size=1.25) +
  theme_classic() + 
  ylim(0,100) + 
  ylab("") +
  xlab("") +
  scale_color_manual(values = c('gold2','plum', "plum4",'sienna3', "grey80"), name="Host") +
  geom_vline(xintercept = 1975, lty=2, size=0.25, col="grey40") +
  geom_vline(xintercept = 1988, lty=2, size=0.25, col="grey40") +
  geom_vline(xintercept = 2000, size=0.25, col="grey40") +
  # annotate("text", x=1968, y=80, label="King77", size=3.5, col="grey40") +
  # annotate("text", x=1995, y=60, label="Kim94", size=3.5, col="grey40") +
  ggtitle("Nigeria")+
  theme(legend.position = "none")

### Senegal
sen <- dplyr::filter(fao_long, Area == "Senegal")
sen_p <- ggplot(sen, aes(x=Year, y=value*100, col=name)) + geom_line(size=1.25) +
  theme_classic() + 
  ylim(0,100) + 
  ylab("") +
  xlab("") +
  scale_color_manual(values = c('gold2','plum', "plum4",'sienna3', "grey80"), name="Host") +
  geom_vline(xintercept = 1975, lty=2, size=0.25, col="grey40") +
  geom_vline(xintercept = 2000, size=0.25, col="grey40") +
  # annotate("text", x=1968, y=90, label="King77", size=3.5, col="grey40") +
  ggtitle("Senegal")+
  theme(legend.position = "none")

### Cameroon
cam <- dplyr::filter(fao_long, Area == "Cameroon")
cam_p <- ggplot(cam, aes(x=Year, y=value*100, col=name)) + geom_line(size=1.25) +
  theme_classic() + 
  ylim(0,100) + 
  ylab("") +
  xlab("") +
  scale_color_manual(values = c('gold2','plum', "plum4",'sienna3', "grey80"), name="Host") +
  geom_vline(xintercept = 1975, lty=2, size=0.25, col="grey40") +
  geom_vline(xintercept = 2000, size=0.25, col="grey40") +
  # annotate("text", x=1968, y=90, label="King77", size=3.5, col="grey40") +
  ggtitle("Cameroon")+
  theme(legend.position = "none")

### Ghana
gha <- dplyr::filter(fao_long, Area == "Ghana")
gha_p <- ggplot(gha, aes(x=Year, y=value*100, col=name)) + geom_line(size=1.25) +
  theme_classic() + 
  ylim(0,100) + 
  ylab("") +
  xlab("") +
  scale_color_manual(values = c('gold2','plum', "plum4",'sienna3', "grey80"), name="Host") +
  geom_vline(xintercept = 1975, lty=2, size=0.25, col="grey40") +
  geom_vline(xintercept = 2000, size=0.25, col="grey40") +
  # annotate("text", x=1968, y=90, label="King77", size=3.5, col="grey40") +
  ggtitle("Ghana")+
  theme(legend.position = "none")

### Burkina Faso
bf <- dplyr::filter(fao_long, Area == c("Burkina Faso"))
bf_p <- ggplot(bf, aes(x=Year, y=value*100, col=name)) + geom_line(size=1.25) +
  theme_classic() + 
  ylab("") +
  xlab("") +
  scale_color_manual(values = c('gold2','plum', "plum4",'sienna3', "grey80"), name="Host") +
  geom_vline(xintercept = 1979, lty=2, size=0.25, col="grey40") +
  geom_vline(xintercept = 2000, size=0.25, col="grey40") +
  # annotate("text", x=1969, y=80, label="Ram83", size=3.5, col="grey40") +
  ylim(c(0,100)) +
  ggtitle("Burkina Faso")+
  theme(legend.position = "none")

### Mali
mal <- dplyr::filter(fao_long, Area == c("Mali"))
mal_p <- ggplot(mal, aes(x=Year, y=value*100, col=name)) + geom_line(size=1.25) +
  theme_classic() + 
  ylab("") +
  xlab("") +
  scale_color_manual(values = c('gold2','plum', "plum4",'sienna3', "grey80"), name="Host") +
  geom_vline(xintercept = 1979, lty=2, size=0.25, col="grey40") +
  geom_vline(xintercept = 2000, size=0.25, col="grey40") +
  # annotate("text", x=1969, y=80, label="Ram83", size=3.5, col="grey40") +
  ylim(c(0,100)) +
  ggtitle("Mali")+
  theme(legend.position = "none")

### Niger
ngr <- dplyr::filter(fao_long, Area == c("Niger"))
ngr_p <- ggplot(ngr, aes(x=Year, y=value*100, col=name)) + geom_line(size=1.25) +
  theme_classic() + 
  ylab("") +
  xlab("") +
  scale_color_manual(values = c('gold2','plum', "plum4",'sienna3', "grey80"), name="Host") +
  geom_vline(xintercept = 1979, lty=2, size=0.25, col="grey40") +
  geom_vline(xintercept = 2000, size=0.25, col="grey40") +
  # annotate("text", x=1969, y=90, label="Ram83", size=3.5, col="grey40") +
  ylim(c(0,100)) +
  ggtitle("Niger")+
  theme(legend.position = "none")

### Ethiopia
eth <- dplyr::filter(fao_long, Area == c("Ethiopia","Ethiopia PDR"))
eth_p <- ggplot(eth, aes(x=Year, y=value*100, col=name)) + geom_line(size=1.25) +
  theme_classic() + 
  ylab("") +
  xlab("") +
  scale_color_manual(values = c('gold2','plum', "plum4",'sienna3', "grey80"), name="Host") +
  geom_vline(xintercept = 1979, lty=2, size=0.25, col="grey40") +
  geom_vline(xintercept = 2000, size=0.25, col="grey40") +
  # annotate("text", x=1969, y=80, label="Ram83", size=3.5, col="grey40") +
  ylim(c(0,100)) +
  ggtitle("Ethiopia")+
  theme(legend.position = "none")

### Kenya
ken <- dplyr::filter(fao_long, Area == "Kenya")
ken_p <- ggplot(ken, aes(x=Year, y=value*100, col=name)) + geom_line(size=1.25) +
  theme_classic() + 
  ylab("") +
  xlab("") +
  scale_color_manual(values = c('gold2','plum', "plum4",'sienna3', "grey80"), name="Host") +
  geom_vline(xintercept = 1979, lty=2, size=0.25, col="grey40") +
  geom_vline(xintercept = 2000, size=0.25, col="grey40") +
  # annotate("text", x=1969, y=95, label="Ram83", size=3.5, col="grey40") +
  ylim(c(0,100)) +
  ggtitle("Kenya")+
  theme(legend.position = "none")

pdf(file="FigS1_FAO.pdf", width=7.5, height=7.5)
  wrap_plots(sen_p, mal_p, bf_p, gha_p, ngr_p, nig_p, cam_p, sud_p, eth_p, ken_p)
dev.off()
