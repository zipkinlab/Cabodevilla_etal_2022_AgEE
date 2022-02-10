##############################################################/############
###### The implementation of irrigation leads to declines in farmland birds ######
###############################  Graphics  ################################
###########################################################################

# Load the reshape library
library(reshape)
# Load the ggplot2 library
library(ggplot2)
# Load the cowplot library
library(cowplot)
# Load the ggthemes library
library(ggthemes)

Output.model<- output.Model
################################
########### Figure 2 ###########
###### Effect of irrigation ####

alpha2.val <- cbind(c(Output.model$q2.5$effect.a0.sp[c(45,1,15,7,22,29,18,33,12,9,47,30,40,5,20,23,3,27,26,44,42,43,38,24,37,13,21,34,2,32,17,4,8,39,10,19,35,25,28,46,41,16,36,14,11,31,6)]),
                    c(Output.model$q25$effect.a0.sp[c(45,1,15,7,22,29,18,33,12,9,47,30,40,5,20,23,3,27,26,44,42,43,38,24,37,13,21,34,2,32,17,4,8,39,10,19,35,25,28,46,41,16,36,14,11,31,6)]),
                    c(Output.model$mean$effect.a0.sp[c(45,1,15,7,22,29,18,33,12,9,47,30,40,5,20,23,3,27,26,44,42,43,38,24,37,13,21,34,2,32,17,4,8,39,10,19,35,25,28,46,41,16,36,14,11,31,6)]),
                    c(Output.model$q75$effect.a0.sp[c(45,1,15,7,22,29,18,33,12,9,47,30,40,5,20,23,3,27,26,44,42,43,38,24,37,13,21,34,2,32,17,4,8,39,10,19,35,25,28,46,41,16,36,14,11,31,6)]),
                    c(Output.model$q97.5$effect.a0.sp[c(45,1,15,7,22,29,18,33,12,9,47,30,40,5,20,23,3,27,26,44,42,43,38,24,37,13,21,34,2,32,17,4,8,39,10,19,35,25,28,46,41,16,36,14,11,31,6)]),
                    c(Output.model$overlap0$effect.a0.sp[c(45,1,15,7,22,29,18,33,12,9,47,30,40,5,20,23,3,27,26,44,42,43,38,24,37,13,21,34,2,32,17,4,8,39,10,19,35,25,28,46,41,16,36,14,11,31,6)]))


sppnames2 <- c("56", "55", "54", "53", "52",
               "51", "50", "49", "48", "47",
               "46", "45", "44", "43", "42",
               "41", "40", "39", "38", "37",
               "36", "35", "34", "33", "32", 
               "31", "30", "29", "28", "27",
               "26", "25", "24", "23", "22",
               "21", "20", "19", "18", "17",
               "16", "15", "14", "13", "12",
               "11", "10")

values2 <- data.frame(sppnames2, alpha2.val)
colnames(values2) <- c("species", "lower.alpha", "l25.alpha", "mean.alpha", "u75.alpha", "upper.alpha", "alpha.sig")

values2$species <- factor(values2$species, levels = values2$species)

values2$nodiff <- values2$l25.alpha < 0 & values2$u75.alpha > 0
values2$H95 <- values2$lower.alpha > 0
values2$H50 <- values2$lower.alpha < 0 & values2$l25.alpha > 0
values2$L95 <- values2$upper.alpha < 0
values2$L50 <- values2$upper.alpha > 0 & values2$u75.alpha < 0

# Community mean
alpha2.1.val <- cbind(c(mean(output.Model$q2.5$effect.a0.sp)),
                      c(mean(output.Model$q25$effect.a0.sp)),
                      c(mean(output.Model$mean$effect.a0.sp)),
                      c(mean(output.Model$q75$effect.a0.sp)),
                      c(mean(output.Model$q97.5$effect.a0.sp)),
                      c(0))

sppnames2.1 <- c("57")

values2.1 <- data.frame(sppnames2.1, alpha2.1.val)
colnames(values2.1) <- c("species", "lower.alpha", "l25.alpha", "mean.alpha", "u75.alpha", "upper.alpha", "alpha.sig")

values2.1$species <- factor(values2.1$species, levels = values2.1$species)

values2.1$nodiff <- values2.1$l25.alpha < 0 & values2.1$u75.alpha > 0
values2.1$H95 <- values2.1$lower.alpha > 0
values2.1$H50 <- values2.1$lower.alpha < 0 & values2.1$l25.alpha > 0
values2.1$L95 <- values2.1$upper.alpha < 0
values2.1$L50 <- values2.1$upper.alpha > 0 & values2.1$u75.alpha < 0

#Figure
Figure2_C <- ggplot() +
  geom_vline(xintercept = 0, alpha = 0.75, linetype = "dotdash") +
  geom_hline(yintercept = 47.5, size = 0.35) +
  geom_hline(yintercept = 32.5, linetype = "dotted", size = 0.6) +
  geom_hline(yintercept = 23.5, linetype = "dotted", size = 0.6) +
  geom_hline(yintercept = 22.5, linetype = "dotted", size = 0.6) +
  geom_hline(yintercept = 19.5, linetype = "dotted", size = 0.6) +
  geom_hline(yintercept = 17.5, linetype = "dotted", size = 0.6) +
  geom_hline(yintercept = 15.5, linetype = "dotted", size = 0.6) +
  geom_errorbarh(data = subset(values2.1, L50 == TRUE), aes(y = species, xmin = lower.alpha, xmax = upper.alpha, color = "red2"), 
                 size = 0.75, height = 0) +
  geom_errorbarh(data = subset(values2.1, L50 == TRUE), aes(y = species, xmin = l25.alpha, xmax = u75.alpha, color = "red2"), 
                 size = 2, height = 0) +
  geom_errorbarh(data = subset(values2.1, L50 == TRUE), aes(y = species, xmin = mean.alpha, xmax = mean.alpha, color = "black"), 
                 height = 0.5) +
  geom_errorbarh(data = subset(values2, nodiff == TRUE), aes(y = species, xmin = lower.alpha, xmax = upper.alpha, color = "gray80"), 
                 height = 0, size = 0.75) +
  geom_errorbarh(data = subset(values2, nodiff == TRUE), aes(y = species, xmin = l25.alpha, xmax = u75.alpha, color = "gray80"), 
                 height = 0, size = 2) +
  geom_errorbarh(data = subset(values2, nodiff == TRUE), aes(y = species, xmin = mean.alpha, xmax = mean.alpha, color = "black"), 
                 height = 0.5) +
  geom_errorbarh(data = subset(values2, H50 == TRUE), aes(y = species, xmin = lower.alpha, xmax = upper.alpha, color = "blue2"), 
                 size = 0.75, height = 0) +
  geom_errorbarh(data = subset(values2, H50 == TRUE), aes(y = species, xmin = l25.alpha, xmax = u75.alpha, color = "blue2"), 
                 size = 2, height = 0) +
  geom_errorbarh(data = subset(values2, H50 == TRUE), aes(y = species, xmin = mean.alpha, xmax = mean.alpha, color = "black"), 
                 height = 0.5) +
  geom_errorbarh(data = subset(values2, L50 == TRUE), aes(y = species, xmin = lower.alpha, xmax = upper.alpha, color = "red2"), 
                 size = 0.75, height = 0) +
  geom_errorbarh(data = subset(values2, L50 == TRUE), aes(y = species, xmin = l25.alpha, xmax = u75.alpha, color = "red2"), 
                 size = 2, height = 0) +
  geom_errorbarh(data = subset(values2, L50 == TRUE), aes(y = species, xmin = mean.alpha, xmax = mean.alpha, color = "black"), 
                 height = 0.5) +
  geom_errorbarh(data = subset(values2, L95 == TRUE), aes(y = species, xmin = lower.alpha, xmax = upper.alpha, color = "red"),
                 size = 0.75, height = 0) +
  geom_errorbarh(data = subset(values2, L95 == TRUE), aes(y = species, xmin = l25.alpha, xmax = u75.alpha, color = "red"),
                 size = 2, height = 0) +
  geom_errorbarh(data = subset(values2, L95 == TRUE), aes(y = species, xmin = mean.alpha, xmax = mean.alpha, color = "black"),
                 height = 0.5) +
  coord_cartesian(xlim = c(-7.3, 7.3)) +
  annotate("text", y = 40, x = 5.65, label = "Farmland", size=3.4, color="black", fontface = "bold") +
  annotate("text", y = 28, x = 5.65, label = "Shrubland", size=3.4, color="black", fontface = "bold") +
  annotate("text", y = 23, x = 5.65, label = "Rocky", size=3.4, color="black", fontface = "bold") +
  annotate("text", y = 21, x = 5.65, label = "Forest", size=3.4, color="black", fontface = "bold") +
  annotate("text", y = 18.5, x = 5.65, label = "Wetland", size=3.4, color="black", fontface = "bold") +
  annotate("text", y = 16.5, x = 5.65, label = "Urban", size=3.4, color="black", fontface = "bold") +
  annotate("text", y = 7.5, x = 5.4, label = "Non-specialist", size=3.4, color="black", fontface = "bold") +
  scale_color_manual(name = "", values = c("red" = "red3", "red2" = "chocolate2", "blue2" = "steelblue3", "black" = "black", "gray80" = "gray80")) +
  scale_y_discrete(labels = c("Common buzzard",	"Black kite",	"Western marsh harrier",	"Carrion crow",	"Magpie",	
                              "Common cuckoo",	"Spotless starling",	"Common blackbird",	"Common nightingale",	
                              "Barn swallow",	"House sparrow",	"Cirl bunting",	"European greenfinch",	"European serin",	
                              "Common linnet",	"Common swift",	"Common house martin",	"White wagtail",	"Mallard",	
                              "Great tit",	"Common chaffinch",	"Common wood pigeon",	"Red-billed chough",	"Melodious warbler",	
                              "European stonechat",	"Sardinian warbler",	"Subalpine warbler",	"Dartford warbler",	
                              "Iberian grey shrike",	"Woodchat shrike",	"Tawny pipit",	"Thekla's lark",	"Common kestrel",
                              "Little owl",	"European turtle-dove",	"European bee-eater",	"Eurasian hoopoe",	
                              "European goldfinch",	"Zitting cisticola",	"Black-eared wheatear",	"Corn bunting",
                              "Calandra lark",	"Crested lark",	"Greater short-toed lark",	"Common quail",	
                              "Red-legged partridge",	"Little bustard",	"Community mean"))+
  scale_x_continuous(position = 'top')+
  theme_few()+
  xlab("Change in occurrence") +
  ylab("") +
  theme(plot.margin = unit(c(0.25, 0.25, 0.15, -0.3), "cm"),
        text = element_text(size = 11),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        axis.text.y = element_text(size = 10, vjust = 0.4, color = "black"),
        axis.title.x = element_text(size=11.5, face="bold"),
        axis.ticks.y = element_line(size= 1.2, color = c("gray80",	"chocolate2",	"gray80",	"red3",	"red3",	"gray80",	
                                                         "chocolate2",	"gray80",	"red3",	"steelblue3",	"chocolate2",	
                                                         "gray80",	"gray80",	"chocolate2",	"gray80",	"gray80",	"steelblue3",	
                                                         "steelblue3",	"gray80",	"chocolate2",	"red3",	"gray80",	"gray80",	
                                                         "chocolate2",	"gray80",	"steelblue3",	"chocolate2",	"gray80",	
                                                         "chocolate2",	"chocolate2",	"chocolate2",	"chocolate2",	
                                                         "chocolate2",	"red3",	"gray80",	"red3",	"red3",	"steelblue3",	
                                                         "gray80",	"red3",	"chocolate2",	"gray80",	"chocolate2",	"red3",	
                                                         "chocolate2",	"chocolate2",	"red3",	"chocolate2")),
        legend.position = "none")

Figure2_C # size = 4.9 x 7.2

#####################################################
###################### Figure 3 ######################
## Species richness by different contest and years ##

# Control sampling locations
alpha3.val.control <- cbind(c(mean(Output.model$q2.5$Nsite[c(1,5),1]),
                              mean(Output.model$q2.5$Nsite[c(1,5),2]),
                              mean(Output.model$q2.5$Nsite[c(1,5),3]),
                              mean(Output.model$q2.5$Nsite[c(1,5),4]),
                              mean(Output.model$q2.5$Nsite[c(1,5),5]),
                              mean(Output.model$q2.5$Nsite[c(1,5),6]),
                              mean(Output.model$q2.5$Nsite[c(1,5),7]),
                              mean(Output.model$q2.5$Nsite[c(1,5),8]),
                              mean(Output.model$q2.5$Nsite[c(1,5),9]),
                              mean(Output.model$q2.5$Nsite[c(1,5),10]),
                              mean(Output.model$q2.5$Nsite[c(1,5),11]),
                              mean(Output.model$q2.5$Nsite[c(1,5),12])),
                            c(mean(Output.model$q25$Nsite[c(1,5),1]),
                              mean(Output.model$q25$Nsite[c(1,5),2]),
                              mean(Output.model$q25$Nsite[c(1,5),3]),
                              mean(Output.model$q25$Nsite[c(1,5),4]),
                              mean(Output.model$q25$Nsite[c(1,5),5]),
                              mean(Output.model$q25$Nsite[c(1,5),6]),
                              mean(Output.model$q25$Nsite[c(1,5),7]),
                              mean(Output.model$q25$Nsite[c(1,5),8]),
                              mean(Output.model$q25$Nsite[c(1,5),9]),
                              mean(Output.model$q25$Nsite[c(1,5),10]),
                              mean(Output.model$q25$Nsite[c(1,5),11]),
                              mean(Output.model$q25$Nsite[c(1,5),12])),
                            c(mean(Output.model$mean$Nsite[c(1,5),1]),
                              mean(Output.model$mean$Nsite[c(1,5),2]),
                              mean(Output.model$mean$Nsite[c(1,5),3]),
                              mean(Output.model$mean$Nsite[c(1,5),4]),
                              mean(Output.model$mean$Nsite[c(1,5),5]),
                              mean(Output.model$mean$Nsite[c(1,5),6]),
                              mean(Output.model$mean$Nsite[c(1,5),7]),
                              mean(Output.model$mean$Nsite[c(1,5),8]),
                              mean(Output.model$mean$Nsite[c(1,5),9]),
                              mean(Output.model$mean$Nsite[c(1,5),10]),
                              mean(Output.model$mean$Nsite[c(1,5),11]),
                              mean(Output.model$mean$Nsite[c(1,5),12])),
                            c(mean(Output.model$q75$Nsite[c(1,5),1]),
                              mean(Output.model$q75$Nsite[c(1,5),2]),
                              mean(Output.model$q75$Nsite[c(1,5),3]),
                              mean(Output.model$q75$Nsite[c(1,5),4]),
                              mean(Output.model$q75$Nsite[c(1,5),5]),
                              mean(Output.model$q75$Nsite[c(1,5),6]),
                              mean(Output.model$q75$Nsite[c(1,5),7]),
                              mean(Output.model$q75$Nsite[c(1,5),8]),
                              mean(Output.model$q75$Nsite[c(1,5),9]),
                              mean(Output.model$q75$Nsite[c(1,5),10]),
                              mean(Output.model$q75$Nsite[c(1,5),11]),
                              mean(Output.model$q75$Nsite[c(1,5),12])),
                            c(mean(Output.model$q97.5$Nsite[c(1,5),1]),
                              mean(Output.model$q97.5$Nsite[c(1,5),2]),
                              mean(Output.model$q97.5$Nsite[c(1,5),3]),
                              mean(Output.model$q97.5$Nsite[c(1,5),4]),
                              mean(Output.model$q97.5$Nsite[c(1,5),5]),
                              mean(Output.model$q97.5$Nsite[c(1,5),6]),
                              mean(Output.model$q97.5$Nsite[c(1,5),7]),
                              mean(Output.model$q97.5$Nsite[c(1,5),8]),
                              mean(Output.model$q97.5$Nsite[c(1,5),9]),
                              mean(Output.model$q97.5$Nsite[c(1,5),10]),
                              mean(Output.model$q97.5$Nsite[c(1,5),11]),
                              mean(Output.model$q97.5$Nsite[c(1,5),12])))

# Western Sampling locations
alpha3.val.west <- cbind(c(mean(Output.model$q2.5$Nsite[c(2,3,4,6,7,8,9,10),1]), 
                           mean(Output.model$q2.5$Nsite[c(2,3,4,6,7,8,9,10),2]),
                           mean(Output.model$q2.5$Nsite[c(2,3,4,6,7,8,9,10),3]),
                           mean(Output.model$q2.5$Nsite[c(2,3,4,6,7,8,9,10),4]),
                           mean(Output.model$q2.5$Nsite[c(2,3,4,6,7,8,9,10),5]),
                           mean(Output.model$q2.5$Nsite[c(2,3,4,6,7,8,9,10),6]),
                           mean(Output.model$q2.5$Nsite[c(2,3,4,6,7,8,9,10),7]),
                           mean(Output.model$q2.5$Nsite[c(2,3,4,6,7,8,9,10),8]),
                           mean(Output.model$q2.5$Nsite[c(2,3,4,6,7,8,9,10),9]),
                           mean(Output.model$q2.5$Nsite[c(2,3,4,6,7,8,9,10),10]),
                           mean(Output.model$q2.5$Nsite[c(2,3,4,6,7,8,9,10),11]),
                           mean(Output.model$q2.5$Nsite[c(2,3,4,6,7,8,9,10),12])),
                         c(mean(Output.model$q25$Nsite[c(2,3,4,6,7,8,9,10),1]),
                           mean(Output.model$q25$Nsite[c(2,3,4,6,7,8,9,10),2]),
                           mean(Output.model$q25$Nsite[c(2,3,4,6,7,8,9,10),3]),
                           mean(Output.model$q25$Nsite[c(2,3,4,6,7,8,9,10),4]),
                           mean(Output.model$q25$Nsite[c(2,3,4,6,7,8,9,10),5]),
                           mean(Output.model$q25$Nsite[c(2,3,4,6,7,8,9,10),6]),
                           mean(Output.model$q25$Nsite[c(2,3,4,6,7,8,9,10),7]),
                           mean(Output.model$q25$Nsite[c(2,3,4,6,7,8,9,10),8]),
                           mean(Output.model$q25$Nsite[c(2,3,4,6,7,8,9,10),9]),
                           mean(Output.model$q25$Nsite[c(2,3,4,6,7,8,9,10),10]),
                           mean(Output.model$q25$Nsite[c(2,3,4,6,7,8,9,10),11]),
                           mean(Output.model$q25$Nsite[c(2,3,4,6,7,8,9,10),12])),
                         c(mean(Output.model$mean$Nsite[c(2,3,4,6,7,8,9,10),1]),
                           mean(Output.model$mean$Nsite[c(2,3,4,6,7,8,9,10),2]),
                           mean(Output.model$mean$Nsite[c(2,3,4,6,7,8,9,10),3]),
                           mean(Output.model$mean$Nsite[c(2,3,4,6,7,8,9,10),4]),
                           mean(Output.model$mean$Nsite[c(2,3,4,6,7,8,9,10),5]),
                           mean(Output.model$mean$Nsite[c(2,3,4,6,7,8,9,10),6]),
                           mean(Output.model$mean$Nsite[c(2,3,4,6,7,8,9,10),7]),
                           mean(Output.model$mean$Nsite[c(2,3,4,6,7,8,9,10),8]),
                           mean(Output.model$mean$Nsite[c(2,3,4,6,7,8,9,10),9]),
                           mean(Output.model$mean$Nsite[c(2,3,4,6,7,8,9,10),10]),
                           mean(Output.model$mean$Nsite[c(2,3,4,6,7,8,9,10),11]),
                           mean(Output.model$mean$Nsite[c(2,3,4,6,7,8,9,10),12])),
                         c(mean(Output.model$q75$Nsite[c(2,3,4,6,7,8,9,10),1]),
                           mean(Output.model$q75$Nsite[c(2,3,4,6,7,8,9,10),2]),
                           mean(Output.model$q75$Nsite[c(2,3,4,6,7,8,9,10),3]),
                           mean(Output.model$q75$Nsite[c(2,3,4,6,7,8,9,10),4]),
                           mean(Output.model$q75$Nsite[c(2,3,4,6,7,8,9,10),5]),
                           mean(Output.model$q75$Nsite[c(2,3,4,6,7,8,9,10),6]),
                           mean(Output.model$q75$Nsite[c(2,3,4,6,7,8,9,10),7]),
                           mean(Output.model$q75$Nsite[c(2,3,4,6,7,8,9,10),8]),
                           mean(Output.model$q75$Nsite[c(2,3,4,6,7,8,9,10),9]),
                           mean(Output.model$q75$Nsite[c(2,3,4,6,7,8,9,10),10]),
                           mean(Output.model$q75$Nsite[c(2,3,4,6,7,8,9,10),11]),
                           mean(Output.model$q75$Nsite[c(2,3,4,6,7,8,9,10),12])),
                         c(mean(Output.model$q97.5$Nsite[c(2,3,4,6,7,8,9,10),1]),
                           mean(Output.model$q97.5$Nsite[c(2,3,4,6,7,8,9,10),2]),
                           mean(Output.model$q97.5$Nsite[c(2,3,4,6,7,8,9,10),3]),
                           mean(Output.model$q97.5$Nsite[c(2,3,4,6,7,8,9,10),4]),
                           mean(Output.model$q97.5$Nsite[c(2,3,4,6,7,8,9,10),5]),
                           mean(Output.model$q97.5$Nsite[c(2,3,4,6,7,8,9,10),6]),
                           mean(Output.model$q97.5$Nsite[c(2,3,4,6,7,8,9,10),7]),
                           mean(Output.model$q97.5$Nsite[c(2,3,4,6,7,8,9,10),8]),
                           mean(Output.model$q97.5$Nsite[c(2,3,4,6,7,8,9,10),9]),
                           mean(Output.model$q97.5$Nsite[c(2,3,4,6,7,8,9,10),10]),
                           mean(Output.model$q97.5$Nsite[c(2,3,4,6,7,8,9,10),11]),
                           mean(Output.model$q97.5$Nsite[c(2,3,4,6,7,8,9,10),12])))

# Eastern Sampling locations                      
alpha3.val.east <- cbind(c(mean(Output.model$q2.5$Nsite[c(11,12,13,14,15,16,17,18,19),1]),
                           mean(Output.model$q2.5$Nsite[c(11,12,13,14,15,16,17,18,19),2]),
                           mean(Output.model$q2.5$Nsite[c(11,12,13,14,15,16,17,18,19),3]),
                           mean(Output.model$q2.5$Nsite[c(11,12,13,14,15,16,17,18,19),4]),
                           mean(Output.model$q2.5$Nsite[c(11,12,13,14,15,16,17,18,19),5]),
                           mean(Output.model$q2.5$Nsite[c(11,12,13,14,15,16,17,18,19),6]),
                           mean(Output.model$q2.5$Nsite[c(11,12,13,14,15,16,17,18,19),7]),
                           mean(Output.model$q2.5$Nsite[c(11,12,13,14,15,16,17,18,19),8]),
                           mean(Output.model$q2.5$Nsite[c(11,12,13,14,15,16,17,18,19),9]),
                           mean(Output.model$q2.5$Nsite[c(11,12,13,14,15,16,17,18,19),10]),
                           mean(Output.model$q2.5$Nsite[c(11,12,13,14,15,16,17,18,19),11]),
                           mean(Output.model$q2.5$Nsite[c(11,12,13,14,15,16,17,18,19),12])),
                         c(mean(Output.model$q25$Nsite[c(11,12,13,14,15,16,17,18,19),1]),
                           mean(Output.model$q25$Nsite[c(11,12,13,14,15,16,17,18,19),2]),
                           mean(Output.model$q25$Nsite[c(11,12,13,14,15,16,17,18,19),3]),
                           mean(Output.model$q25$Nsite[c(11,12,13,14,15,16,17,18,19),4]),
                           mean(Output.model$q25$Nsite[c(11,12,13,14,15,16,17,18,19),5]),
                           mean(Output.model$q25$Nsite[c(11,12,13,14,15,16,17,18,19),6]),
                           mean(Output.model$q25$Nsite[c(11,12,13,14,15,16,17,18,19),7]),
                           mean(Output.model$q25$Nsite[c(11,12,13,14,15,16,17,18,19),8]),
                           mean(Output.model$q25$Nsite[c(11,12,13,14,15,16,17,18,19),9]),
                           mean(Output.model$q25$Nsite[c(11,12,13,14,15,16,17,18,19),10]),
                           mean(Output.model$q25$Nsite[c(11,12,13,14,15,16,17,18,19),11]),
                           mean(Output.model$q25$Nsite[c(11,12,13,14,15,16,17,18,19),12])),
                         c(mean(Output.model$mean$Nsite[c(11,12,13,14,15,16,17,18,19),1]),
                           mean(Output.model$mean$Nsite[c(11,12,13,14,15,16,17,18,19),2]),
                           mean(Output.model$mean$Nsite[c(11,12,13,14,15,16,17,18,19),3]),
                           mean(Output.model$mean$Nsite[c(11,12,13,14,15,16,17,18,19),4]),
                           mean(Output.model$mean$Nsite[c(11,12,13,14,15,16,17,18,19),5]),
                           mean(Output.model$mean$Nsite[c(11,12,13,14,15,16,17,18,19),6]),
                           mean(Output.model$mean$Nsite[c(11,12,13,14,15,16,17,18,19),7]),
                           mean(Output.model$mean$Nsite[c(11,12,13,14,15,16,17,18,19),8]),
                           mean(Output.model$mean$Nsite[c(11,12,13,14,15,16,17,18,19),9]),
                           mean(Output.model$mean$Nsite[c(11,12,13,14,15,16,17,18,19),10]),
                           mean(Output.model$mean$Nsite[c(11,12,13,14,15,16,17,18,19),11]),
                           mean(Output.model$mean$Nsite[c(11,12,13,14,15,16,17,18,19),12])),
                         c(mean(Output.model$q75$Nsite[c(11,12,13,14,15,16,17,18,19),1]),
                           mean(Output.model$q75$Nsite[c(11,12,13,14,15,16,17,18,19),2]),
                           mean(Output.model$q75$Nsite[c(11,12,13,14,15,16,17,18,19),3]),
                           mean(Output.model$q75$Nsite[c(11,12,13,14,15,16,17,18,19),4]),
                           mean(Output.model$q75$Nsite[c(11,12,13,14,15,16,17,18,19),5]),
                           mean(Output.model$q75$Nsite[c(11,12,13,14,15,16,17,18,19),6]),
                           mean(Output.model$q75$Nsite[c(11,12,13,14,15,16,17,18,19),7]),
                           mean(Output.model$q75$Nsite[c(11,12,13,14,15,16,17,18,19),8]),
                           mean(Output.model$q75$Nsite[c(11,12,13,14,15,16,17,18,19),9]),
                           mean(Output.model$q75$Nsite[c(11,12,13,14,15,16,17,18,19),10]),
                           mean(Output.model$q75$Nsite[c(11,12,13,14,15,16,17,18,19),11]),
                           mean(Output.model$q75$Nsite[c(11,12,13,14,15,16,17,18,19),12])),
                         c(mean(Output.model$q97.5$Nsite[c(11,12,13,14,15,16,17,18,19),1]),
                           mean(Output.model$q97.5$Nsite[c(11,12,13,14,15,16,17,18,19),2]),
                           mean(Output.model$q97.5$Nsite[c(11,12,13,14,15,16,17,18,19),3]),
                           mean(Output.model$q97.5$Nsite[c(11,12,13,14,15,16,17,18,19),4]),
                           mean(Output.model$q97.5$Nsite[c(11,12,13,14,15,16,17,18,19),5]),
                           mean(Output.model$q97.5$Nsite[c(11,12,13,14,15,16,17,18,19),6]),
                           mean(Output.model$q97.5$Nsite[c(11,12,13,14,15,16,17,18,19),7]),
                           mean(Output.model$q97.5$Nsite[c(11,12,13,14,15,16,17,18,19),8]),
                           mean(Output.model$q97.5$Nsite[c(11,12,13,14,15,16,17,18,19),9]),
                           mean(Output.model$q97.5$Nsite[c(11,12,13,14,15,16,17,18,19),10]),
                           mean(Output.model$q97.5$Nsite[c(11,12,13,14,15,16,17,18,19),11]),
                           mean(Output.model$q97.5$Nsite[c(11,12,13,14,15,16,17,18,19),12])))

alpha3.val.control
alpha3.val.west
alpha3.val.east

# sampling years
years <- c(1,2,3,4,5,6,7,8,9,10,11,12)

## Figure3.1
values3.control <- data.frame(years, alpha3.val.control)
colnames(values3.control) <- c("years", "lower.alpha", "l25.alpha", "mean.alpha", "u75.alpha", "upper.alpha")

#Figure
Figure3.1 <- ggplot() +
  scale_y_continuous(limits = c(11, 43), breaks = c(15,20,25,30,35,40))+
  geom_errorbar(data = subset(values3.control), aes(x = years, ymin = lower.alpha, ymax = upper.alpha, color = "yellow"), 
                size = 0.75, width = 0) +
  geom_errorbar(data = subset(values3.control), aes(x = years, ymin = l25.alpha, ymax = u75.alpha, color = "yellow"), 
                size = 2, width = 0) +
  geom_errorbar(data = subset(values3.control), aes(x = years, ymin = mean.alpha, ymax = mean.alpha, color = "black"), 
                width = 0.3) +
  scale_color_manual(name = "", values = c("yellow" = "#FFF685","black" = "black", "blue" = "#00BFFF")) +
  scale_x_continuous(limits=c(0.8, 12.2), breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12))+
  theme_few() +
  ylab("Species richness") +
  xlab("Year") +
  theme(plot.margin = unit(c(0.5, -0.1, 0.5, 0.3), "cm"),
        text = element_text(size = 11),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.5, color = "black"),
        axis.title.y = element_text(size=11, vjust = 2, face="bold"),
        axis.title.x = element_text(size=11, vjust = -2),
        legend.position = "none")

## Figure3.2
values3.west <- data.frame(years, alpha3.val.west)
colnames(values3.west) <- c("years", "lower.alpha", "l25.alpha", "mean.alpha", "u75.alpha", "upper.alpha")

#Identify irrigated and non-irrigated years 
values3.west$sec <- values3.west$years < 10
values3.west$irri <- values3.west$years > 9

#Figure
Figure3.2 <- ggplot() +
  scale_y_continuous(limits = c(11, 43), breaks = c(15,20,25,30,35,40))+
  geom_errorbar(data = subset(values3.west, irri == TRUE), aes(x = years, ymin = lower.alpha, ymax = upper.alpha, color = "blue"), 
                size = 0.75, width = 0) +
  geom_errorbar(data = subset(values3.west, irri == TRUE), aes(x = years, ymin = l25.alpha, ymax = u75.alpha, color = "blue"), 
                size = 2, width = 0) +
  geom_errorbar(data = subset(values3.west, irri == TRUE), aes(x = years, ymin = mean.alpha, ymax = mean.alpha, color = "black"), 
                width = 0.3) +
  geom_errorbar(data = subset(values3.west, sec == TRUE), aes(x = years, ymin = lower.alpha, ymax = upper.alpha, color = "yellow"), 
                width = 0, size = 0.75) +
  geom_errorbar(data = subset(values3.west, sec == TRUE), aes(x = years, ymin = l25.alpha, ymax = u75.alpha, color = "yellow"), 
                width = 0, size = 2) +
  geom_errorbar(data = subset(values3.west, sec == TRUE), aes(x = years, ymin = mean.alpha, ymax = mean.alpha, color = "black"), 
                width = 0.3) +
  scale_color_manual(name = "", values = c("yellow" = "#FFF685","black" = "black", "blue" = "#00BFFF")) +
  scale_x_continuous(limits=c(0.8, 12.2), breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12))+
  theme_few() +
  ylab("") +
  xlab("Year") +
  theme(plot.margin = unit(c(0.5, 0.1, 0.5, 0.1), "cm"),
        text = element_text(size = 11),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.5, color = "black"),
        axis.title.y = element_text(size=11, vjust = 2, face="bold"),
        axis.title.x = element_text(size=11, vjust = -2),
        legend.position = "none")

Figure3.2

# Figure3.3
values3.east <- data.frame(years, alpha3.val.east)
colnames(values3.east) <- c("years", "lower.alpha", "l25.alpha", "mean.alpha", "u75.alpha", "upper.alpha")

#Identify irrigated and non-irrigated years 
values3.east$sec <- values3.east$years < 3
values3.east$irri <- values3.east$years > 2

#Figure 
Figure3.3 <- ggplot() +
  scale_y_continuous(limits = c(11, 43), breaks = c(15,20,25,30,35,40))+
  geom_errorbar(data = subset(values3.east, irri == TRUE), aes(x = years, ymin = lower.alpha, ymax = upper.alpha, color = "blue"), 
                size = 0.75, width = 0) +
  geom_errorbar(data = subset(values3.east, irri == TRUE), aes(x = years, ymin = l25.alpha, ymax = u75.alpha, color = "blue"), 
                size = 2, width = 0) +
  geom_errorbar(data = subset(values3.east, irri == TRUE), aes(x = years, ymin = mean.alpha, ymax = mean.alpha, color = "black"), 
                width = 0.3) +
  geom_errorbar(data = subset(values3.east, sec == TRUE), aes(x = years, ymin = lower.alpha, ymax = upper.alpha, color = "yellow"), 
                width = 0, size = 0.75) +
  geom_errorbar(data = subset(values3.east, sec == TRUE), aes(x = years, ymin = l25.alpha, ymax = u75.alpha, color = "yellow"), 
                width = 0, size = 2) +
  geom_errorbar(data = subset(values3.east, sec == TRUE), aes(x = years, ymin = mean.alpha, ymax = mean.alpha, color = "black"), 
                width = 0.3) +
  scale_color_manual(name = "", values = c("yellow" = "#FFF685","black" = "black", "blue" = "#00BFFF")) +
  scale_x_continuous(limits=c(0.8, 12.2), breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12))+
  theme_few() +
  ylab("") +
  xlab("Year") +
  theme(plot.margin = unit(c(0.5, 0.3, 0.5, -0.1), "cm"),
        text = element_text(size = 11),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.5, color = "black"),
        axis.title.y = element_text(size=11, vjust = 2, face="bold"),
        axis.title.x = element_text(size=11, vjust = -2),
        legend.position = "none")

Figure3.3 

#Figure 3 # 6.8 x 2.25
plot_grid(Figure3.1 , Figure3.2, Figure3.3, labels = c("a)", "b)", "c)"), hjust = -0.5, 
          label_size = 11, ncol = 3, nrow = 1,rel_widths = c(1, 1, 1))


#######################################
############## Figure A.1 ##############
### Effect of occurrence probability ##

alphaS1.val <- cbind(c(mean(c(mean(Output.model$q2.5$psi[c(2),c(1,2,3,4,5,6,7,8,9),45]),mean(Output.model$q2.5$psi[c(3),c(1,2,3,4,5,6,7,8,9),45]),mean(Output.model$q2.5$psi[c(4),c(1,2,3,4,5,6,7,8,9),45]),
                             mean(Output.model$q2.5$psi[c(6),c(1,2,3,4,5,6,7,8,9),45]),mean(Output.model$q2.5$psi[c(7),c(1,2,3,4,5,6,7,8,9),45]),mean(Output.model$q2.5$psi[c(8),c(1,2,3,4,5,6,7,8,9),45]),
                             mean(Output.model$q2.5$psi[c(9),c(1,2,3,4,5,6,7,8,9),45]),mean(Output.model$q2.5$psi[c(10),c(1,2,3,4,5,6,7,8,9),45]),mean(Output.model$q2.5$psi[c(11),c(1,2),45]),
                             mean(Output.model$q2.5$psi[c(12),c(1,2),45]),mean(Output.model$q2.5$psi[c(13),c(1,2),45]),mean(Output.model$q2.5$psi[c(14),c(1,2),45]),
                             mean(Output.model$q2.5$psi[c(15),c(1,2),45]),mean(Output.model$q2.5$psi[c(16),c(1,2),45]),mean(Output.model$q2.5$psi[c(17),c(1,2),45]),
                             mean(Output.model$q2.5$psi[c(18),c(1,2),45]),mean(Output.model$q2.5$psi[c(19),c(1,2),45]))),
                      mean(c(mean(Output.model$q2.5$psi[c(2),c(1,2,3,4,5,6,7,8,9),1]),mean(Output.model$q2.5$psi[c(3),c(1,2,3,4,5,6,7,8,9),1]),mean(Output.model$q2.5$psi[c(4),c(1,2,3,4,5,6,7,8,9),1]),
                             mean(Output.model$q2.5$psi[c(6),c(1,2,3,4,5,6,7,8,9),1]),mean(Output.model$q2.5$psi[c(7),c(1,2,3,4,5,6,7,8,9),1]),mean(Output.model$q2.5$psi[c(8),c(1,2,3,4,5,6,7,8,9),1]),
                             mean(Output.model$q2.5$psi[c(9),c(1,2,3,4,5,6,7,8,9),1]),mean(Output.model$q2.5$psi[c(10),c(1,2,3,4,5,6,7,8,9),1]),mean(Output.model$q2.5$psi[c(11),c(1,2),1]),
                             mean(Output.model$q2.5$psi[c(12),c(1,2),1]),mean(Output.model$q2.5$psi[c(13),c(1,2),1]),mean(Output.model$q2.5$psi[c(14),c(1,2),1]),
                             mean(Output.model$q2.5$psi[c(15),c(1,2),1]),mean(Output.model$q2.5$psi[c(16),c(1,2),1]),mean(Output.model$q2.5$psi[c(17),c(1,2),1]),
                             mean(Output.model$q2.5$psi[c(18),c(1,2),1]),mean(Output.model$q2.5$psi[c(19),c(1,2),1]))),
                      mean(c(mean(Output.model$q2.5$psi[c(2),c(1,2,3,4,5,6,7,8,9),15]),mean(Output.model$q2.5$psi[c(3),c(1,2,3,4,5,6,7,8,9),15]),mean(Output.model$q2.5$psi[c(4),c(1,2,3,4,5,6,7,8,9),15]),
                             mean(Output.model$q2.5$psi[c(6),c(1,2,3,4,5,6,7,8,9),15]),mean(Output.model$q2.5$psi[c(7),c(1,2,3,4,5,6,7,8,9),15]),mean(Output.model$q2.5$psi[c(8),c(1,2,3,4,5,6,7,8,9),15]),
                             mean(Output.model$q2.5$psi[c(9),c(1,2,3,4,5,6,7,8,9),15]),mean(Output.model$q2.5$psi[c(10),c(1,2,3,4,5,6,7,8,9),15]),mean(Output.model$q2.5$psi[c(11),c(1,2),15]),
                             mean(Output.model$q2.5$psi[c(12),c(1,2),15]),mean(Output.model$q2.5$psi[c(13),c(1,2),15]),mean(Output.model$q2.5$psi[c(14),c(1,2),15]),
                             mean(Output.model$q2.5$psi[c(15),c(1,2),15]),mean(Output.model$q2.5$psi[c(16),c(1,2),15]),mean(Output.model$q2.5$psi[c(17),c(1,2),15]),
                             mean(Output.model$q2.5$psi[c(18),c(1,2),15]),mean(Output.model$q2.5$psi[c(19),c(1,2),15]))),
                      mean(c(mean(Output.model$q2.5$psi[c(2),c(1,2,3,4,5,6,7,8,9),7]),mean(Output.model$q2.5$psi[c(3),c(1,2,3,4,5,6,7,8,9),7]),mean(Output.model$q2.5$psi[c(4),c(1,2,3,4,5,6,7,8,9),7]),
                             mean(Output.model$q2.5$psi[c(6),c(1,2,3,4,5,6,7,8,9),7]),mean(Output.model$q2.5$psi[c(7),c(1,2,3,4,5,6,7,8,9),7]),mean(Output.model$q2.5$psi[c(8),c(1,2,3,4,5,6,7,8,9),7]),
                             mean(Output.model$q2.5$psi[c(9),c(1,2,3,4,5,6,7,8,9),7]),mean(Output.model$q2.5$psi[c(10),c(1,2,3,4,5,6,7,8,9),7]),mean(Output.model$q2.5$psi[c(11),c(1,2),7]),
                             mean(Output.model$q2.5$psi[c(12),c(1,2),7]),mean(Output.model$q2.5$psi[c(13),c(1,2),7]),mean(Output.model$q2.5$psi[c(14),c(1,2),7]),
                             mean(Output.model$q2.5$psi[c(15),c(1,2),7]),mean(Output.model$q2.5$psi[c(16),c(1,2),7]),mean(Output.model$q2.5$psi[c(17),c(1,2),7]),
                             mean(Output.model$q2.5$psi[c(18),c(1,2),7]),mean(Output.model$q2.5$psi[c(19),c(1,2),7]))),
                      mean(c(mean(Output.model$q2.5$psi[c(2),c(1,2,3,4,5,6,7,8,9),22]),mean(Output.model$q2.5$psi[c(3),c(1,2,3,4,5,6,7,8,9),22]),mean(Output.model$q2.5$psi[c(4),c(1,2,3,4,5,6,7,8,9),22]),
                             mean(Output.model$q2.5$psi[c(6),c(1,2,3,4,5,6,7,8,9),22]),mean(Output.model$q2.5$psi[c(7),c(1,2,3,4,5,6,7,8,9),22]),mean(Output.model$q2.5$psi[c(8),c(1,2,3,4,5,6,7,8,9),22]),
                             mean(Output.model$q2.5$psi[c(9),c(1,2,3,4,5,6,7,8,9),22]),mean(Output.model$q2.5$psi[c(10),c(1,2,3,4,5,6,7,8,9),22]),mean(Output.model$q2.5$psi[c(11),c(1,2),22]),
                             mean(Output.model$q2.5$psi[c(12),c(1,2),22]),mean(Output.model$q2.5$psi[c(13),c(1,2),22]),mean(Output.model$q2.5$psi[c(14),c(1,2),22]),
                             mean(Output.model$q2.5$psi[c(15),c(1,2),22]),mean(Output.model$q2.5$psi[c(16),c(1,2),22]),mean(Output.model$q2.5$psi[c(17),c(1,2),22]),
                             mean(Output.model$q2.5$psi[c(18),c(1,2),22]),mean(Output.model$q2.5$psi[c(19),c(1,2),22]))),
                      mean(c(mean(Output.model$q2.5$psi[c(2),c(1,2,3,4,5,6,7,8,9),29]),mean(Output.model$q2.5$psi[c(3),c(1,2,3,4,5,6,7,8,9),29]),mean(Output.model$q2.5$psi[c(4),c(1,2,3,4,5,6,7,8,9),29]),
                             mean(Output.model$q2.5$psi[c(6),c(1,2,3,4,5,6,7,8,9),29]),mean(Output.model$q2.5$psi[c(7),c(1,2,3,4,5,6,7,8,9),29]),mean(Output.model$q2.5$psi[c(8),c(1,2,3,4,5,6,7,8,9),29]),
                             mean(Output.model$q2.5$psi[c(9),c(1,2,3,4,5,6,7,8,9),29]),mean(Output.model$q2.5$psi[c(10),c(1,2,3,4,5,6,7,8,9),29]),mean(Output.model$q2.5$psi[c(11),c(1,2),29]),
                             mean(Output.model$q2.5$psi[c(12),c(1,2),29]),mean(Output.model$q2.5$psi[c(13),c(1,2),29]),mean(Output.model$q2.5$psi[c(14),c(1,2),29]),
                             mean(Output.model$q2.5$psi[c(15),c(1,2),29]),mean(Output.model$q2.5$psi[c(16),c(1,2),29]),mean(Output.model$q2.5$psi[c(17),c(1,2),29]),
                             mean(Output.model$q2.5$psi[c(18),c(1,2),29]),mean(Output.model$q2.5$psi[c(19),c(1,2),29]))),
                      mean(c(mean(Output.model$q2.5$psi[c(2),c(1,2,3,4,5,6,7,8,9),18]),mean(Output.model$q2.5$psi[c(3),c(1,2,3,4,5,6,7,8,9),18]),mean(Output.model$q2.5$psi[c(4),c(1,2,3,4,5,6,7,8,9),18]),
                             mean(Output.model$q2.5$psi[c(6),c(1,2,3,4,5,6,7,8,9),18]),mean(Output.model$q2.5$psi[c(7),c(1,2,3,4,5,6,7,8,9),18]),mean(Output.model$q2.5$psi[c(8),c(1,2,3,4,5,6,7,8,9),18]),
                             mean(Output.model$q2.5$psi[c(9),c(1,2,3,4,5,6,7,8,9),18]),mean(Output.model$q2.5$psi[c(10),c(1,2,3,4,5,6,7,8,9),18]),mean(Output.model$q2.5$psi[c(11),c(1,2),18]),
                             mean(Output.model$q2.5$psi[c(12),c(1,2),18]),mean(Output.model$q2.5$psi[c(13),c(1,2),18]),mean(Output.model$q2.5$psi[c(14),c(1,2),18]),
                             mean(Output.model$q2.5$psi[c(15),c(1,2),18]),mean(Output.model$q2.5$psi[c(16),c(1,2),18]),mean(Output.model$q2.5$psi[c(17),c(1,2),18]),
                             mean(Output.model$q2.5$psi[c(18),c(1,2),18]),mean(Output.model$q2.5$psi[c(19),c(1,2),18]))),
                      mean(c(mean(Output.model$q2.5$psi[c(2),c(1,2,3,4,5,6,7,8,9),33]),mean(Output.model$q2.5$psi[c(3),c(1,2,3,4,5,6,7,8,9),33]),mean(Output.model$q2.5$psi[c(4),c(1,2,3,4,5,6,7,8,9),33]),
                             mean(Output.model$q2.5$psi[c(6),c(1,2,3,4,5,6,7,8,9),33]),mean(Output.model$q2.5$psi[c(7),c(1,2,3,4,5,6,7,8,9),33]),mean(Output.model$q2.5$psi[c(8),c(1,2,3,4,5,6,7,8,9),33]),
                             mean(Output.model$q2.5$psi[c(9),c(1,2,3,4,5,6,7,8,9),33]),mean(Output.model$q2.5$psi[c(10),c(1,2,3,4,5,6,7,8,9),33]),mean(Output.model$q2.5$psi[c(11),c(1,2),33]),
                             mean(Output.model$q2.5$psi[c(12),c(1,2),33]),mean(Output.model$q2.5$psi[c(13),c(1,2),33]),mean(Output.model$q2.5$psi[c(14),c(1,2),33]),
                             mean(Output.model$q2.5$psi[c(15),c(1,2),33]),mean(Output.model$q2.5$psi[c(16),c(1,2),33]),mean(Output.model$q2.5$psi[c(17),c(1,2),33]),
                             mean(Output.model$q2.5$psi[c(18),c(1,2),33]),mean(Output.model$q2.5$psi[c(19),c(1,2),33]))),
                      mean(c(mean(Output.model$q2.5$psi[c(2),c(1,2,3,4,5,6,7,8,9),12]),mean(Output.model$q2.5$psi[c(3),c(1,2,3,4,5,6,7,8,9),12]),mean(Output.model$q2.5$psi[c(4),c(1,2,3,4,5,6,7,8,9),12]),
                             mean(Output.model$q2.5$psi[c(6),c(1,2,3,4,5,6,7,8,9),12]),mean(Output.model$q2.5$psi[c(7),c(1,2,3,4,5,6,7,8,9),12]),mean(Output.model$q2.5$psi[c(8),c(1,2,3,4,5,6,7,8,9),12]),
                             mean(Output.model$q2.5$psi[c(9),c(1,2,3,4,5,6,7,8,9),12]),mean(Output.model$q2.5$psi[c(10),c(1,2,3,4,5,6,7,8,9),12]),mean(Output.model$q2.5$psi[c(11),c(1,2),12]),
                             mean(Output.model$q2.5$psi[c(12),c(1,2),12]),mean(Output.model$q2.5$psi[c(13),c(1,2),12]),mean(Output.model$q2.5$psi[c(14),c(1,2),12]),
                             mean(Output.model$q2.5$psi[c(15),c(1,2),12]),mean(Output.model$q2.5$psi[c(16),c(1,2),12]),mean(Output.model$q2.5$psi[c(17),c(1,2),12]),
                             mean(Output.model$q2.5$psi[c(18),c(1,2),12]),mean(Output.model$q2.5$psi[c(19),c(1,2),12]))),
                      mean(c(mean(Output.model$q2.5$psi[c(2),c(1,2,3,4,5,6,7,8,9),9]),mean(Output.model$q2.5$psi[c(3),c(1,2,3,4,5,6,7,8,9),9]),mean(Output.model$q2.5$psi[c(4),c(1,2,3,4,5,6,7,8,9),9]),
                             mean(Output.model$q2.5$psi[c(6),c(1,2,3,4,5,6,7,8,9),9]),mean(Output.model$q2.5$psi[c(7),c(1,2,3,4,5,6,7,8,9),9]),mean(Output.model$q2.5$psi[c(8),c(1,2,3,4,5,6,7,8,9),9]),
                             mean(Output.model$q2.5$psi[c(9),c(1,2,3,4,5,6,7,8,9),9]),mean(Output.model$q2.5$psi[c(10),c(1,2,3,4,5,6,7,8,9),9]),mean(Output.model$q2.5$psi[c(11),c(1,2),9]),
                             mean(Output.model$q2.5$psi[c(12),c(1,2),9]),mean(Output.model$q2.5$psi[c(13),c(1,2),9]),mean(Output.model$q2.5$psi[c(14),c(1,2),9]),
                             mean(Output.model$q2.5$psi[c(15),c(1,2),9]),mean(Output.model$q2.5$psi[c(16),c(1,2),9]),mean(Output.model$q2.5$psi[c(17),c(1,2),9]),
                             mean(Output.model$q2.5$psi[c(18),c(1,2),9]),mean(Output.model$q2.5$psi[c(19),c(1,2),9]))),
                      mean(c(mean(Output.model$q2.5$psi[c(2),c(1,2,3,4,5,6,7,8,9),47]),mean(Output.model$q2.5$psi[c(3),c(1,2,3,4,5,6,7,8,9),47]),mean(Output.model$q2.5$psi[c(4),c(1,2,3,4,5,6,7,8,9),47]),
                             mean(Output.model$q2.5$psi[c(6),c(1,2,3,4,5,6,7,8,9),47]),mean(Output.model$q2.5$psi[c(7),c(1,2,3,4,5,6,7,8,9),47]),mean(Output.model$q2.5$psi[c(8),c(1,2,3,4,5,6,7,8,9),47]),
                             mean(Output.model$q2.5$psi[c(9),c(1,2,3,4,5,6,7,8,9),47]),mean(Output.model$q2.5$psi[c(10),c(1,2,3,4,5,6,7,8,9),47]),mean(Output.model$q2.5$psi[c(11),c(1,2),47]),
                             mean(Output.model$q2.5$psi[c(12),c(1,2),47]),mean(Output.model$q2.5$psi[c(13),c(1,2),47]),mean(Output.model$q2.5$psi[c(14),c(1,2),47]),
                             mean(Output.model$q2.5$psi[c(15),c(1,2),47]),mean(Output.model$q2.5$psi[c(16),c(1,2),47]),mean(Output.model$q2.5$psi[c(17),c(1,2),47]),
                             mean(Output.model$q2.5$psi[c(18),c(1,2),47]),mean(Output.model$q2.5$psi[c(19),c(1,2),47]))),
                      mean(c(mean(Output.model$q2.5$psi[c(2),c(1,2,3,4,5,6,7,8,9),30]),mean(Output.model$q2.5$psi[c(3),c(1,2,3,4,5,6,7,8,9),30]),mean(Output.model$q2.5$psi[c(4),c(1,2,3,4,5,6,7,8,9),30]),
                             mean(Output.model$q2.5$psi[c(6),c(1,2,3,4,5,6,7,8,9),30]),mean(Output.model$q2.5$psi[c(7),c(1,2,3,4,5,6,7,8,9),30]),mean(Output.model$q2.5$psi[c(8),c(1,2,3,4,5,6,7,8,9),30]),
                             mean(Output.model$q2.5$psi[c(9),c(1,2,3,4,5,6,7,8,9),30]),mean(Output.model$q2.5$psi[c(10),c(1,2,3,4,5,6,7,8,9),30]),mean(Output.model$q2.5$psi[c(11),c(1,2),30]),
                             mean(Output.model$q2.5$psi[c(12),c(1,2),30]),mean(Output.model$q2.5$psi[c(13),c(1,2),30]),mean(Output.model$q2.5$psi[c(14),c(1,2),30]),
                             mean(Output.model$q2.5$psi[c(15),c(1,2),30]),mean(Output.model$q2.5$psi[c(16),c(1,2),30]),mean(Output.model$q2.5$psi[c(17),c(1,2),30]),
                             mean(Output.model$q2.5$psi[c(18),c(1,2),30]),mean(Output.model$q2.5$psi[c(19),c(1,2),30]))),
                      mean(c(mean(Output.model$q2.5$psi[c(2),c(1,2,3,4,5,6,7,8,9),40]),mean(Output.model$q2.5$psi[c(3),c(1,2,3,4,5,6,7,8,9),40]),mean(Output.model$q2.5$psi[c(4),c(1,2,3,4,5,6,7,8,9),40]),
                             mean(Output.model$q2.5$psi[c(6),c(1,2,3,4,5,6,7,8,9),40]),mean(Output.model$q2.5$psi[c(7),c(1,2,3,4,5,6,7,8,9),40]),mean(Output.model$q2.5$psi[c(8),c(1,2,3,4,5,6,7,8,9),40]),
                             mean(Output.model$q2.5$psi[c(9),c(1,2,3,4,5,6,7,8,9),40]),mean(Output.model$q2.5$psi[c(10),c(1,2,3,4,5,6,7,8,9),40]),mean(Output.model$q2.5$psi[c(11),c(1,2),40]),
                             mean(Output.model$q2.5$psi[c(12),c(1,2),40]),mean(Output.model$q2.5$psi[c(13),c(1,2),40]),mean(Output.model$q2.5$psi[c(14),c(1,2),40]),
                             mean(Output.model$q2.5$psi[c(15),c(1,2),40]),mean(Output.model$q2.5$psi[c(16),c(1,2),40]),mean(Output.model$q2.5$psi[c(17),c(1,2),40]),
                             mean(Output.model$q2.5$psi[c(18),c(1,2),40]),mean(Output.model$q2.5$psi[c(19),c(1,2),40]))),
                      mean(c(mean(Output.model$q2.5$psi[c(2),c(1,2,3,4,5,6,7,8,9),5]),mean(Output.model$q2.5$psi[c(3),c(1,2,3,4,5,6,7,8,9),5]),mean(Output.model$q2.5$psi[c(4),c(1,2,3,4,5,6,7,8,9),5]),
                             mean(Output.model$q2.5$psi[c(6),c(1,2,3,4,5,6,7,8,9),5]),mean(Output.model$q2.5$psi[c(7),c(1,2,3,4,5,6,7,8,9),5]),mean(Output.model$q2.5$psi[c(8),c(1,2,3,4,5,6,7,8,9),5]),
                             mean(Output.model$q2.5$psi[c(9),c(1,2,3,4,5,6,7,8,9),5]),mean(Output.model$q2.5$psi[c(10),c(1,2,3,4,5,6,7,8,9),5]),mean(Output.model$q2.5$psi[c(11),c(1,2),5]),
                             mean(Output.model$q2.5$psi[c(12),c(1,2),5]),mean(Output.model$q2.5$psi[c(13),c(1,2),5]),mean(Output.model$q2.5$psi[c(14),c(1,2),5]),
                             mean(Output.model$q2.5$psi[c(15),c(1,2),5]),mean(Output.model$q2.5$psi[c(16),c(1,2),5]),mean(Output.model$q2.5$psi[c(17),c(1,2),5]),
                             mean(Output.model$q2.5$psi[c(18),c(1,2),5]),mean(Output.model$q2.5$psi[c(19),c(1,2),5]))),
                      mean(c(mean(Output.model$q2.5$psi[c(2),c(1,2,3,4,5,6,7,8,9),20]),mean(Output.model$q2.5$psi[c(3),c(1,2,3,4,5,6,7,8,9),20]),mean(Output.model$q2.5$psi[c(4),c(1,2,3,4,5,6,7,8,9),20]),
                             mean(Output.model$q2.5$psi[c(6),c(1,2,3,4,5,6,7,8,9),20]),mean(Output.model$q2.5$psi[c(7),c(1,2,3,4,5,6,7,8,9),20]),mean(Output.model$q2.5$psi[c(8),c(1,2,3,4,5,6,7,8,9),20]),
                             mean(Output.model$q2.5$psi[c(9),c(1,2,3,4,5,6,7,8,9),20]),mean(Output.model$q2.5$psi[c(10),c(1,2,3,4,5,6,7,8,9),20]),mean(Output.model$q2.5$psi[c(11),c(1,2),20]),
                             mean(Output.model$q2.5$psi[c(12),c(1,2),20]),mean(Output.model$q2.5$psi[c(13),c(1,2),20]),mean(Output.model$q2.5$psi[c(14),c(1,2),20]),
                             mean(Output.model$q2.5$psi[c(15),c(1,2),20]),mean(Output.model$q2.5$psi[c(16),c(1,2),20]),mean(Output.model$q2.5$psi[c(17),c(1,2),20]),
                             mean(Output.model$q2.5$psi[c(18),c(1,2),20]),mean(Output.model$q2.5$psi[c(19),c(1,2),20]))),
                      mean(c(mean(Output.model$q2.5$psi[c(2),c(1,2,3,4,5,6,7,8,9),23]),mean(Output.model$q2.5$psi[c(3),c(1,2,3,4,5,6,7,8,9),23]),mean(Output.model$q2.5$psi[c(4),c(1,2,3,4,5,6,7,8,9),23]),
                             mean(Output.model$q2.5$psi[c(6),c(1,2,3,4,5,6,7,8,9),23]),mean(Output.model$q2.5$psi[c(7),c(1,2,3,4,5,6,7,8,9),23]),mean(Output.model$q2.5$psi[c(8),c(1,2,3,4,5,6,7,8,9),23]),
                             mean(Output.model$q2.5$psi[c(9),c(1,2,3,4,5,6,7,8,9),23]),mean(Output.model$q2.5$psi[c(10),c(1,2,3,4,5,6,7,8,9),23]),mean(Output.model$q2.5$psi[c(11),c(1,2),23]),
                             mean(Output.model$q2.5$psi[c(12),c(1,2),23]),mean(Output.model$q2.5$psi[c(13),c(1,2),23]),mean(Output.model$q2.5$psi[c(14),c(1,2),23]),
                             mean(Output.model$q2.5$psi[c(15),c(1,2),23]),mean(Output.model$q2.5$psi[c(16),c(1,2),23]),mean(Output.model$q2.5$psi[c(17),c(1,2),23]),
                             mean(Output.model$q2.5$psi[c(18),c(1,2),23]),mean(Output.model$q2.5$psi[c(19),c(1,2),23]))),
                      mean(c(mean(Output.model$q2.5$psi[c(2),c(1,2,3,4,5,6,7,8,9),3]),mean(Output.model$q2.5$psi[c(3),c(1,2,3,4,5,6,7,8,9),3]),mean(Output.model$q2.5$psi[c(4),c(1,2,3,4,5,6,7,8,9),3]),
                             mean(Output.model$q2.5$psi[c(6),c(1,2,3,4,5,6,7,8,9),3]),mean(Output.model$q2.5$psi[c(7),c(1,2,3,4,5,6,7,8,9),3]),mean(Output.model$q2.5$psi[c(8),c(1,2,3,4,5,6,7,8,9),3]),
                             mean(Output.model$q2.5$psi[c(9),c(1,2,3,4,5,6,7,8,9),3]),mean(Output.model$q2.5$psi[c(10),c(1,2,3,4,5,6,7,8,9),3]),mean(Output.model$q2.5$psi[c(11),c(1,2),3]),
                             mean(Output.model$q2.5$psi[c(12),c(1,2),3]),mean(Output.model$q2.5$psi[c(13),c(1,2),3]),mean(Output.model$q2.5$psi[c(14),c(1,2),3]),
                             mean(Output.model$q2.5$psi[c(15),c(1,2),3]),mean(Output.model$q2.5$psi[c(16),c(1,2),3]),mean(Output.model$q2.5$psi[c(17),c(1,2),3]),
                             mean(Output.model$q2.5$psi[c(18),c(1,2),3]),mean(Output.model$q2.5$psi[c(19),c(1,2),3]))),
                      mean(c(mean(Output.model$q2.5$psi[c(2),c(1,2,3,4,5,6,7,8,9),27]),mean(Output.model$q2.5$psi[c(3),c(1,2,3,4,5,6,7,8,9),27]),mean(Output.model$q2.5$psi[c(4),c(1,2,3,4,5,6,7,8,9),27]),
                             mean(Output.model$q2.5$psi[c(6),c(1,2,3,4,5,6,7,8,9),27]),mean(Output.model$q2.5$psi[c(7),c(1,2,3,4,5,6,7,8,9),27]),mean(Output.model$q2.5$psi[c(8),c(1,2,3,4,5,6,7,8,9),27]),
                             mean(Output.model$q2.5$psi[c(9),c(1,2,3,4,5,6,7,8,9),27]),mean(Output.model$q2.5$psi[c(10),c(1,2,3,4,5,6,7,8,9),27]),mean(Output.model$q2.5$psi[c(11),c(1,2),27]),
                             mean(Output.model$q2.5$psi[c(12),c(1,2),27]),mean(Output.model$q2.5$psi[c(13),c(1,2),27]),mean(Output.model$q2.5$psi[c(14),c(1,2),27]),
                             mean(Output.model$q2.5$psi[c(15),c(1,2),27]),mean(Output.model$q2.5$psi[c(16),c(1,2),27]),mean(Output.model$q2.5$psi[c(17),c(1,2),27]),
                             mean(Output.model$q2.5$psi[c(18),c(1,2),27]),mean(Output.model$q2.5$psi[c(19),c(1,2),27]))),
                      mean(c(mean(Output.model$q2.5$psi[c(2),c(1,2,3,4,5,6,7,8,9),26]),mean(Output.model$q2.5$psi[c(3),c(1,2,3,4,5,6,7,8,9),26]),mean(Output.model$q2.5$psi[c(4),c(1,2,3,4,5,6,7,8,9),26]),
                             mean(Output.model$q2.5$psi[c(6),c(1,2,3,4,5,6,7,8,9),26]),mean(Output.model$q2.5$psi[c(7),c(1,2,3,4,5,6,7,8,9),26]),mean(Output.model$q2.5$psi[c(8),c(1,2,3,4,5,6,7,8,9),26]),
                             mean(Output.model$q2.5$psi[c(9),c(1,2,3,4,5,6,7,8,9),26]),mean(Output.model$q2.5$psi[c(10),c(1,2,3,4,5,6,7,8,9),26]),mean(Output.model$q2.5$psi[c(11),c(1,2),26]),
                             mean(Output.model$q2.5$psi[c(12),c(1,2),26]),mean(Output.model$q2.5$psi[c(13),c(1,2),26]),mean(Output.model$q2.5$psi[c(14),c(1,2),26]),
                             mean(Output.model$q2.5$psi[c(15),c(1,2),26]),mean(Output.model$q2.5$psi[c(16),c(1,2),26]),mean(Output.model$q2.5$psi[c(17),c(1,2),26]),
                             mean(Output.model$q2.5$psi[c(18),c(1,2),26]),mean(Output.model$q2.5$psi[c(19),c(1,2),26]))),
                      mean(c(mean(Output.model$q2.5$psi[c(2),c(1,2,3,4,5,6,7,8,9),44]),mean(Output.model$q2.5$psi[c(3),c(1,2,3,4,5,6,7,8,9),44]),mean(Output.model$q2.5$psi[c(4),c(1,2,3,4,5,6,7,8,9),44]),
                             mean(Output.model$q2.5$psi[c(6),c(1,2,3,4,5,6,7,8,9),44]),mean(Output.model$q2.5$psi[c(7),c(1,2,3,4,5,6,7,8,9),44]),mean(Output.model$q2.5$psi[c(8),c(1,2,3,4,5,6,7,8,9),44]),
                             mean(Output.model$q2.5$psi[c(9),c(1,2,3,4,5,6,7,8,9),44]),mean(Output.model$q2.5$psi[c(10),c(1,2,3,4,5,6,7,8,9),44]),mean(Output.model$q2.5$psi[c(11),c(1,2),44]),
                             mean(Output.model$q2.5$psi[c(12),c(1,2),44]),mean(Output.model$q2.5$psi[c(13),c(1,2),44]),mean(Output.model$q2.5$psi[c(14),c(1,2),44]),
                             mean(Output.model$q2.5$psi[c(15),c(1,2),44]),mean(Output.model$q2.5$psi[c(16),c(1,2),44]),mean(Output.model$q2.5$psi[c(17),c(1,2),44]),
                             mean(Output.model$q2.5$psi[c(18),c(1,2),44]),mean(Output.model$q2.5$psi[c(19),c(1,2),44]))),
                      mean(c(mean(Output.model$q2.5$psi[c(2),c(1,2,3,4,5,6,7,8,9),42]),mean(Output.model$q2.5$psi[c(3),c(1,2,3,4,5,6,7,8,9),42]),mean(Output.model$q2.5$psi[c(4),c(1,2,3,4,5,6,7,8,9),42]),
                             mean(Output.model$q2.5$psi[c(6),c(1,2,3,4,5,6,7,8,9),42]),mean(Output.model$q2.5$psi[c(7),c(1,2,3,4,5,6,7,8,9),42]),mean(Output.model$q2.5$psi[c(8),c(1,2,3,4,5,6,7,8,9),42]),
                             mean(Output.model$q2.5$psi[c(9),c(1,2,3,4,5,6,7,8,9),42]),mean(Output.model$q2.5$psi[c(10),c(1,2,3,4,5,6,7,8,9),42]),mean(Output.model$q2.5$psi[c(11),c(1,2),42]),
                             mean(Output.model$q2.5$psi[c(12),c(1,2),42]),mean(Output.model$q2.5$psi[c(13),c(1,2),42]),mean(Output.model$q2.5$psi[c(14),c(1,2),42]),
                             mean(Output.model$q2.5$psi[c(15),c(1,2),42]),mean(Output.model$q2.5$psi[c(16),c(1,2),42]),mean(Output.model$q2.5$psi[c(17),c(1,2),42]),
                             mean(Output.model$q2.5$psi[c(18),c(1,2),42]),mean(Output.model$q2.5$psi[c(19),c(1,2),42]))),
                      mean(c(mean(Output.model$q2.5$psi[c(2),c(1,2,3,4,5,6,7,8,9),43]),mean(Output.model$q2.5$psi[c(3),c(1,2,3,4,5,6,7,8,9),43]),mean(Output.model$q2.5$psi[c(4),c(1,2,3,4,5,6,7,8,9),43]),
                             mean(Output.model$q2.5$psi[c(6),c(1,2,3,4,5,6,7,8,9),43]),mean(Output.model$q2.5$psi[c(7),c(1,2,3,4,5,6,7,8,9),43]),mean(Output.model$q2.5$psi[c(8),c(1,2,3,4,5,6,7,8,9),43]),
                             mean(Output.model$q2.5$psi[c(9),c(1,2,3,4,5,6,7,8,9),43]),mean(Output.model$q2.5$psi[c(10),c(1,2,3,4,5,6,7,8,9),43]),mean(Output.model$q2.5$psi[c(11),c(1,2),43]),
                             mean(Output.model$q2.5$psi[c(12),c(1,2),43]),mean(Output.model$q2.5$psi[c(13),c(1,2),43]),mean(Output.model$q2.5$psi[c(14),c(1,2),43]),
                             mean(Output.model$q2.5$psi[c(15),c(1,2),43]),mean(Output.model$q2.5$psi[c(16),c(1,2),43]),mean(Output.model$q2.5$psi[c(17),c(1,2),43]),
                             mean(Output.model$q2.5$psi[c(18),c(1,2),43]),mean(Output.model$q2.5$psi[c(19),c(1,2),43]))),
                      mean(c(mean(Output.model$q2.5$psi[c(2),c(1,2,3,4,5,6,7,8,9),38]),mean(Output.model$q2.5$psi[c(3),c(1,2,3,4,5,6,7,8,9),38]),mean(Output.model$q2.5$psi[c(4),c(1,2,3,4,5,6,7,8,9),38]),
                             mean(Output.model$q2.5$psi[c(6),c(1,2,3,4,5,6,7,8,9),38]),mean(Output.model$q2.5$psi[c(7),c(1,2,3,4,5,6,7,8,9),38]),mean(Output.model$q2.5$psi[c(8),c(1,2,3,4,5,6,7,8,9),38]),
                             mean(Output.model$q2.5$psi[c(9),c(1,2,3,4,5,6,7,8,9),38]),mean(Output.model$q2.5$psi[c(10),c(1,2,3,4,5,6,7,8,9),38]),mean(Output.model$q2.5$psi[c(11),c(1,2),38]),
                             mean(Output.model$q2.5$psi[c(12),c(1,2),38]),mean(Output.model$q2.5$psi[c(13),c(1,2),38]),mean(Output.model$q2.5$psi[c(14),c(1,2),38]),
                             mean(Output.model$q2.5$psi[c(15),c(1,2),38]),mean(Output.model$q2.5$psi[c(16),c(1,2),38]),mean(Output.model$q2.5$psi[c(17),c(1,2),38]),
                             mean(Output.model$q2.5$psi[c(18),c(1,2),38]),mean(Output.model$q2.5$psi[c(19),c(1,2),38]))),
                      mean(c(mean(Output.model$q2.5$psi[c(2),c(1,2,3,4,5,6,7,8,9),24]),mean(Output.model$q2.5$psi[c(3),c(1,2,3,4,5,6,7,8,9),24]),mean(Output.model$q2.5$psi[c(4),c(1,2,3,4,5,6,7,8,9),24]),
                             mean(Output.model$q2.5$psi[c(6),c(1,2,3,4,5,6,7,8,9),24]),mean(Output.model$q2.5$psi[c(7),c(1,2,3,4,5,6,7,8,9),24]),mean(Output.model$q2.5$psi[c(8),c(1,2,3,4,5,6,7,8,9),24]),
                             mean(Output.model$q2.5$psi[c(9),c(1,2,3,4,5,6,7,8,9),24]),mean(Output.model$q2.5$psi[c(10),c(1,2,3,4,5,6,7,8,9),24]),mean(Output.model$q2.5$psi[c(11),c(1,2),24]),
                             mean(Output.model$q2.5$psi[c(12),c(1,2),24]),mean(Output.model$q2.5$psi[c(13),c(1,2),24]),mean(Output.model$q2.5$psi[c(14),c(1,2),24]),
                             mean(Output.model$q2.5$psi[c(15),c(1,2),24]),mean(Output.model$q2.5$psi[c(16),c(1,2),24]),mean(Output.model$q2.5$psi[c(17),c(1,2),24]),
                             mean(Output.model$q2.5$psi[c(18),c(1,2),24]),mean(Output.model$q2.5$psi[c(19),c(1,2),24]))),
                      mean(c(mean(Output.model$q2.5$psi[c(2),c(1,2,3,4,5,6,7,8,9),37]),mean(Output.model$q2.5$psi[c(3),c(1,2,3,4,5,6,7,8,9),37]),mean(Output.model$q2.5$psi[c(4),c(1,2,3,4,5,6,7,8,9),37]),
                             mean(Output.model$q2.5$psi[c(6),c(1,2,3,4,5,6,7,8,9),37]),mean(Output.model$q2.5$psi[c(7),c(1,2,3,4,5,6,7,8,9),37]),mean(Output.model$q2.5$psi[c(8),c(1,2,3,4,5,6,7,8,9),37]),
                             mean(Output.model$q2.5$psi[c(9),c(1,2,3,4,5,6,7,8,9),37]),mean(Output.model$q2.5$psi[c(10),c(1,2,3,4,5,6,7,8,9),37]),mean(Output.model$q2.5$psi[c(11),c(1,2),37]),
                             mean(Output.model$q2.5$psi[c(12),c(1,2),37]),mean(Output.model$q2.5$psi[c(13),c(1,2),37]),mean(Output.model$q2.5$psi[c(14),c(1,2),37]),
                             mean(Output.model$q2.5$psi[c(15),c(1,2),37]),mean(Output.model$q2.5$psi[c(16),c(1,2),37]),mean(Output.model$q2.5$psi[c(17),c(1,2),37]),
                             mean(Output.model$q2.5$psi[c(18),c(1,2),37]),mean(Output.model$q2.5$psi[c(19),c(1,2),37]))),
                      mean(c(mean(Output.model$q2.5$psi[c(2),c(1,2,3,4,5,6,7,8,9),13]),mean(Output.model$q2.5$psi[c(3),c(1,2,3,4,5,6,7,8,9),13]),mean(Output.model$q2.5$psi[c(4),c(1,2,3,4,5,6,7,8,9),13]),
                             mean(Output.model$q2.5$psi[c(6),c(1,2,3,4,5,6,7,8,9),13]),mean(Output.model$q2.5$psi[c(7),c(1,2,3,4,5,6,7,8,9),13]),mean(Output.model$q2.5$psi[c(8),c(1,2,3,4,5,6,7,8,9),13]),
                             mean(Output.model$q2.5$psi[c(9),c(1,2,3,4,5,6,7,8,9),13]),mean(Output.model$q2.5$psi[c(10),c(1,2,3,4,5,6,7,8,9),13]),mean(Output.model$q2.5$psi[c(11),c(1,2),13]),
                             mean(Output.model$q2.5$psi[c(12),c(1,2),13]),mean(Output.model$q2.5$psi[c(13),c(1,2),13]),mean(Output.model$q2.5$psi[c(14),c(1,2),13]),
                             mean(Output.model$q2.5$psi[c(15),c(1,2),13]),mean(Output.model$q2.5$psi[c(16),c(1,2),13]),mean(Output.model$q2.5$psi[c(17),c(1,2),13]),
                             mean(Output.model$q2.5$psi[c(18),c(1,2),13]),mean(Output.model$q2.5$psi[c(19),c(1,2),13]))),
                      mean(c(mean(Output.model$q2.5$psi[c(2),c(1,2,3,4,5,6,7,8,9),21]),mean(Output.model$q2.5$psi[c(3),c(1,2,3,4,5,6,7,8,9),21]),mean(Output.model$q2.5$psi[c(4),c(1,2,3,4,5,6,7,8,9),21]),
                             mean(Output.model$q2.5$psi[c(6),c(1,2,3,4,5,6,7,8,9),21]),mean(Output.model$q2.5$psi[c(7),c(1,2,3,4,5,6,7,8,9),21]),mean(Output.model$q2.5$psi[c(8),c(1,2,3,4,5,6,7,8,9),21]),
                             mean(Output.model$q2.5$psi[c(9),c(1,2,3,4,5,6,7,8,9),21]),mean(Output.model$q2.5$psi[c(10),c(1,2,3,4,5,6,7,8,9),21]),mean(Output.model$q2.5$psi[c(11),c(1,2),21]),
                             mean(Output.model$q2.5$psi[c(12),c(1,2),21]),mean(Output.model$q2.5$psi[c(13),c(1,2),21]),mean(Output.model$q2.5$psi[c(14),c(1,2),21]),
                             mean(Output.model$q2.5$psi[c(15),c(1,2),21]),mean(Output.model$q2.5$psi[c(16),c(1,2),21]),mean(Output.model$q2.5$psi[c(17),c(1,2),21]),
                             mean(Output.model$q2.5$psi[c(18),c(1,2),21]),mean(Output.model$q2.5$psi[c(19),c(1,2),21]))),
                      mean(c(mean(Output.model$q2.5$psi[c(2),c(1,2,3,4,5,6,7,8,9),34]),mean(Output.model$q2.5$psi[c(3),c(1,2,3,4,5,6,7,8,9),34]),mean(Output.model$q2.5$psi[c(4),c(1,2,3,4,5,6,7,8,9),34]),
                             mean(Output.model$q2.5$psi[c(6),c(1,2,3,4,5,6,7,8,9),34]),mean(Output.model$q2.5$psi[c(7),c(1,2,3,4,5,6,7,8,9),34]),mean(Output.model$q2.5$psi[c(8),c(1,2,3,4,5,6,7,8,9),34]),
                             mean(Output.model$q2.5$psi[c(9),c(1,2,3,4,5,6,7,8,9),34]),mean(Output.model$q2.5$psi[c(10),c(1,2,3,4,5,6,7,8,9),34]),mean(Output.model$q2.5$psi[c(11),c(1,2),34]),
                             mean(Output.model$q2.5$psi[c(12),c(1,2),34]),mean(Output.model$q2.5$psi[c(13),c(1,2),34]),mean(Output.model$q2.5$psi[c(14),c(1,2),34]),
                             mean(Output.model$q2.5$psi[c(15),c(1,2),34]),mean(Output.model$q2.5$psi[c(16),c(1,2),34]),mean(Output.model$q2.5$psi[c(17),c(1,2),34]),
                             mean(Output.model$q2.5$psi[c(18),c(1,2),34]),mean(Output.model$q2.5$psi[c(19),c(1,2),34]))),
                      mean(c(mean(Output.model$q2.5$psi[c(2),c(1,2,3,4,5,6,7,8,9),2]),mean(Output.model$q2.5$psi[c(3),c(1,2,3,4,5,6,7,8,9),2]),mean(Output.model$q2.5$psi[c(4),c(1,2,3,4,5,6,7,8,9),2]),
                             mean(Output.model$q2.5$psi[c(6),c(1,2,3,4,5,6,7,8,9),2]),mean(Output.model$q2.5$psi[c(7),c(1,2,3,4,5,6,7,8,9),2]),mean(Output.model$q2.5$psi[c(8),c(1,2,3,4,5,6,7,8,9),2]),
                             mean(Output.model$q2.5$psi[c(9),c(1,2,3,4,5,6,7,8,9),2]),mean(Output.model$q2.5$psi[c(10),c(1,2,3,4,5,6,7,8,9),2]),mean(Output.model$q2.5$psi[c(11),c(1,2),2]),
                             mean(Output.model$q2.5$psi[c(12),c(1,2),2]),mean(Output.model$q2.5$psi[c(13),c(1,2),2]),mean(Output.model$q2.5$psi[c(14),c(1,2),2]),
                             mean(Output.model$q2.5$psi[c(15),c(1,2),2]),mean(Output.model$q2.5$psi[c(16),c(1,2),2]),mean(Output.model$q2.5$psi[c(17),c(1,2),2]),
                             mean(Output.model$q2.5$psi[c(18),c(1,2),2]),mean(Output.model$q2.5$psi[c(19),c(1,2),2]))),
                      mean(c(mean(Output.model$q2.5$psi[c(2),c(1,2,3,4,5,6,7,8,9),32]),mean(Output.model$q2.5$psi[c(3),c(1,2,3,4,5,6,7,8,9),32]),mean(Output.model$q2.5$psi[c(4),c(1,2,3,4,5,6,7,8,9),32]),
                             mean(Output.model$q2.5$psi[c(6),c(1,2,3,4,5,6,7,8,9),32]),mean(Output.model$q2.5$psi[c(7),c(1,2,3,4,5,6,7,8,9),32]),mean(Output.model$q2.5$psi[c(8),c(1,2,3,4,5,6,7,8,9),32]),
                             mean(Output.model$q2.5$psi[c(9),c(1,2,3,4,5,6,7,8,9),32]),mean(Output.model$q2.5$psi[c(10),c(1,2,3,4,5,6,7,8,9),32]),mean(Output.model$q2.5$psi[c(11),c(1,2),32]),
                             mean(Output.model$q2.5$psi[c(12),c(1,2),32]),mean(Output.model$q2.5$psi[c(13),c(1,2),32]),mean(Output.model$q2.5$psi[c(14),c(1,2),32]),
                             mean(Output.model$q2.5$psi[c(15),c(1,2),32]),mean(Output.model$q2.5$psi[c(16),c(1,2),32]),mean(Output.model$q2.5$psi[c(17),c(1,2),32]),
                             mean(Output.model$q2.5$psi[c(18),c(1,2),32]),mean(Output.model$q2.5$psi[c(19),c(1,2),32]))),
                      mean(c(mean(Output.model$q2.5$psi[c(2),c(1,2,3,4,5,6,7,8,9),17]),mean(Output.model$q2.5$psi[c(3),c(1,2,3,4,5,6,7,8,9),17]),mean(Output.model$q2.5$psi[c(4),c(1,2,3,4,5,6,7,8,9),17]),
                             mean(Output.model$q2.5$psi[c(6),c(1,2,3,4,5,6,7,8,9),17]),mean(Output.model$q2.5$psi[c(7),c(1,2,3,4,5,6,7,8,9),17]),mean(Output.model$q2.5$psi[c(8),c(1,2,3,4,5,6,7,8,9),17]),
                             mean(Output.model$q2.5$psi[c(9),c(1,2,3,4,5,6,7,8,9),17]),mean(Output.model$q2.5$psi[c(10),c(1,2,3,4,5,6,7,8,9),17]),mean(Output.model$q2.5$psi[c(11),c(1,2),17]),
                             mean(Output.model$q2.5$psi[c(12),c(1,2),17]),mean(Output.model$q2.5$psi[c(13),c(1,2),17]),mean(Output.model$q2.5$psi[c(14),c(1,2),17]),
                             mean(Output.model$q2.5$psi[c(15),c(1,2),17]),mean(Output.model$q2.5$psi[c(16),c(1,2),17]),mean(Output.model$q2.5$psi[c(17),c(1,2),17]),
                             mean(Output.model$q2.5$psi[c(18),c(1,2),17]),mean(Output.model$q2.5$psi[c(19),c(1,2),17]))),
                      mean(c(mean(Output.model$q2.5$psi[c(2),c(1,2,3,4,5,6,7,8,9),4]),mean(Output.model$q2.5$psi[c(3),c(1,2,3,4,5,6,7,8,9),4]),mean(Output.model$q2.5$psi[c(4),c(1,2,3,4,5,6,7,8,9),4]),
                             mean(Output.model$q2.5$psi[c(6),c(1,2,3,4,5,6,7,8,9),4]),mean(Output.model$q2.5$psi[c(7),c(1,2,3,4,5,6,7,8,9),4]),mean(Output.model$q2.5$psi[c(8),c(1,2,3,4,5,6,7,8,9),4]),
                             mean(Output.model$q2.5$psi[c(9),c(1,2,3,4,5,6,7,8,9),4]),mean(Output.model$q2.5$psi[c(10),c(1,2,3,4,5,6,7,8,9),4]),mean(Output.model$q2.5$psi[c(11),c(1,2),4]),
                             mean(Output.model$q2.5$psi[c(12),c(1,2),4]),mean(Output.model$q2.5$psi[c(13),c(1,2),4]),mean(Output.model$q2.5$psi[c(14),c(1,2),4]),
                             mean(Output.model$q2.5$psi[c(15),c(1,2),4]),mean(Output.model$q2.5$psi[c(16),c(1,2),4]),mean(Output.model$q2.5$psi[c(17),c(1,2),4]),
                             mean(Output.model$q2.5$psi[c(18),c(1,2),4]),mean(Output.model$q2.5$psi[c(19),c(1,2),4]))),
                      mean(c(mean(Output.model$q2.5$psi[c(2),c(1,2,3,4,5,6,7,8,9),8]),mean(Output.model$q2.5$psi[c(3),c(1,2,3,4,5,6,7,8,9),8]),mean(Output.model$q2.5$psi[c(4),c(1,2,3,4,5,6,7,8,9),8]),
                             mean(Output.model$q2.5$psi[c(6),c(1,2,3,4,5,6,7,8,9),8]),mean(Output.model$q2.5$psi[c(7),c(1,2,3,4,5,6,7,8,9),8]),mean(Output.model$q2.5$psi[c(8),c(1,2,3,4,5,6,7,8,9),8]),
                             mean(Output.model$q2.5$psi[c(9),c(1,2,3,4,5,6,7,8,9),8]),mean(Output.model$q2.5$psi[c(10),c(1,2,3,4,5,6,7,8,9),8]),mean(Output.model$q2.5$psi[c(11),c(1,2),8]),
                             mean(Output.model$q2.5$psi[c(12),c(1,2),8]),mean(Output.model$q2.5$psi[c(13),c(1,2),8]),mean(Output.model$q2.5$psi[c(14),c(1,2),8]),
                             mean(Output.model$q2.5$psi[c(15),c(1,2),8]),mean(Output.model$q2.5$psi[c(16),c(1,2),8]),mean(Output.model$q2.5$psi[c(17),c(1,2),8]),
                             mean(Output.model$q2.5$psi[c(18),c(1,2),8]),mean(Output.model$q2.5$psi[c(19),c(1,2),8]))),
                      mean(c(mean(Output.model$q2.5$psi[c(2),c(1,2,3,4,5,6,7,8,9),39]),mean(Output.model$q2.5$psi[c(3),c(1,2,3,4,5,6,7,8,9),39]),mean(Output.model$q2.5$psi[c(4),c(1,2,3,4,5,6,7,8,9),39]),
                             mean(Output.model$q2.5$psi[c(6),c(1,2,3,4,5,6,7,8,9),39]),mean(Output.model$q2.5$psi[c(7),c(1,2,3,4,5,6,7,8,9),39]),mean(Output.model$q2.5$psi[c(8),c(1,2,3,4,5,6,7,8,9),39]),
                             mean(Output.model$q2.5$psi[c(9),c(1,2,3,4,5,6,7,8,9),39]),mean(Output.model$q2.5$psi[c(10),c(1,2,3,4,5,6,7,8,9),39]),mean(Output.model$q2.5$psi[c(11),c(1,2),39]),
                             mean(Output.model$q2.5$psi[c(12),c(1,2),39]),mean(Output.model$q2.5$psi[c(13),c(1,2),39]),mean(Output.model$q2.5$psi[c(14),c(1,2),39]),
                             mean(Output.model$q2.5$psi[c(15),c(1,2),39]),mean(Output.model$q2.5$psi[c(16),c(1,2),39]),mean(Output.model$q2.5$psi[c(17),c(1,2),39]),
                             mean(Output.model$q2.5$psi[c(18),c(1,2),39]),mean(Output.model$q2.5$psi[c(19),c(1,2),39]))),
                      mean(c(mean(Output.model$q2.5$psi[c(2),c(1,2,3,4,5,6,7,8,9),10]),mean(Output.model$q2.5$psi[c(3),c(1,2,3,4,5,6,7,8,9),10]),mean(Output.model$q2.5$psi[c(4),c(1,2,3,4,5,6,7,8,9),10]),
                             mean(Output.model$q2.5$psi[c(6),c(1,2,3,4,5,6,7,8,9),10]),mean(Output.model$q2.5$psi[c(7),c(1,2,3,4,5,6,7,8,9),10]),mean(Output.model$q2.5$psi[c(8),c(1,2,3,4,5,6,7,8,9),10]),
                             mean(Output.model$q2.5$psi[c(9),c(1,2,3,4,5,6,7,8,9),10]),mean(Output.model$q2.5$psi[c(10),c(1,2,3,4,5,6,7,8,9),10]),mean(Output.model$q2.5$psi[c(11),c(1,2),10]),
                             mean(Output.model$q2.5$psi[c(12),c(1,2),10]),mean(Output.model$q2.5$psi[c(13),c(1,2),10]),mean(Output.model$q2.5$psi[c(14),c(1,2),10]),
                             mean(Output.model$q2.5$psi[c(15),c(1,2),10]),mean(Output.model$q2.5$psi[c(16),c(1,2),10]),mean(Output.model$q2.5$psi[c(17),c(1,2),10]),
                             mean(Output.model$q2.5$psi[c(18),c(1,2),10]),mean(Output.model$q2.5$psi[c(19),c(1,2),10]))),
                      mean(c(mean(Output.model$q2.5$psi[c(2),c(1,2,3,4,5,6,7,8,9),19]),mean(Output.model$q2.5$psi[c(3),c(1,2,3,4,5,6,7,8,9),19]),mean(Output.model$q2.5$psi[c(4),c(1,2,3,4,5,6,7,8,9),19]),
                             mean(Output.model$q2.5$psi[c(6),c(1,2,3,4,5,6,7,8,9),19]),mean(Output.model$q2.5$psi[c(7),c(1,2,3,4,5,6,7,8,9),19]),mean(Output.model$q2.5$psi[c(8),c(1,2,3,4,5,6,7,8,9),19]),
                             mean(Output.model$q2.5$psi[c(9),c(1,2,3,4,5,6,7,8,9),19]),mean(Output.model$q2.5$psi[c(10),c(1,2,3,4,5,6,7,8,9),19]),mean(Output.model$q2.5$psi[c(11),c(1,2),19]),
                             mean(Output.model$q2.5$psi[c(12),c(1,2),19]),mean(Output.model$q2.5$psi[c(13),c(1,2),19]),mean(Output.model$q2.5$psi[c(14),c(1,2),19]),
                             mean(Output.model$q2.5$psi[c(15),c(1,2),19]),mean(Output.model$q2.5$psi[c(16),c(1,2),19]),mean(Output.model$q2.5$psi[c(17),c(1,2),19]),
                             mean(Output.model$q2.5$psi[c(18),c(1,2),19]),mean(Output.model$q2.5$psi[c(19),c(1,2),19]))),
                      mean(c(mean(Output.model$q2.5$psi[c(2),c(1,2,3,4,5,6,7,8,9),35]),mean(Output.model$q2.5$psi[c(3),c(1,2,3,4,5,6,7,8,9),35]),mean(Output.model$q2.5$psi[c(4),c(1,2,3,4,5,6,7,8,9),35]),
                             mean(Output.model$q2.5$psi[c(6),c(1,2,3,4,5,6,7,8,9),35]),mean(Output.model$q2.5$psi[c(7),c(1,2,3,4,5,6,7,8,9),35]),mean(Output.model$q2.5$psi[c(8),c(1,2,3,4,5,6,7,8,9),35]),
                             mean(Output.model$q2.5$psi[c(9),c(1,2,3,4,5,6,7,8,9),35]),mean(Output.model$q2.5$psi[c(10),c(1,2,3,4,5,6,7,8,9),35]),mean(Output.model$q2.5$psi[c(11),c(1,2),35]),
                             mean(Output.model$q2.5$psi[c(12),c(1,2),35]),mean(Output.model$q2.5$psi[c(13),c(1,2),35]),mean(Output.model$q2.5$psi[c(14),c(1,2),35]),
                             mean(Output.model$q2.5$psi[c(15),c(1,2),35]),mean(Output.model$q2.5$psi[c(16),c(1,2),35]),mean(Output.model$q2.5$psi[c(17),c(1,2),35]),
                             mean(Output.model$q2.5$psi[c(18),c(1,2),35]),mean(Output.model$q2.5$psi[c(19),c(1,2),35]))),
                      mean(c(mean(Output.model$q2.5$psi[c(2),c(1,2,3,4,5,6,7,8,9),25]),mean(Output.model$q2.5$psi[c(3),c(1,2,3,4,5,6,7,8,9),25]),mean(Output.model$q2.5$psi[c(4),c(1,2,3,4,5,6,7,8,9),25]),
                             mean(Output.model$q2.5$psi[c(6),c(1,2,3,4,5,6,7,8,9),25]),mean(Output.model$q2.5$psi[c(7),c(1,2,3,4,5,6,7,8,9),25]),mean(Output.model$q2.5$psi[c(8),c(1,2,3,4,5,6,7,8,9),25]),
                             mean(Output.model$q2.5$psi[c(9),c(1,2,3,4,5,6,7,8,9),25]),mean(Output.model$q2.5$psi[c(10),c(1,2,3,4,5,6,7,8,9),25]),mean(Output.model$q2.5$psi[c(11),c(1,2),25]),
                             mean(Output.model$q2.5$psi[c(12),c(1,2),25]),mean(Output.model$q2.5$psi[c(13),c(1,2),25]),mean(Output.model$q2.5$psi[c(14),c(1,2),25]),
                             mean(Output.model$q2.5$psi[c(15),c(1,2),25]),mean(Output.model$q2.5$psi[c(16),c(1,2),25]),mean(Output.model$q2.5$psi[c(17),c(1,2),25]),
                             mean(Output.model$q2.5$psi[c(18),c(1,2),25]),mean(Output.model$q2.5$psi[c(19),c(1,2),25]))),
                      mean(c(mean(Output.model$q2.5$psi[c(2),c(1,2,3,4,5,6,7,8,9),28]),mean(Output.model$q2.5$psi[c(3),c(1,2,3,4,5,6,7,8,9),28]),mean(Output.model$q2.5$psi[c(4),c(1,2,3,4,5,6,7,8,9),28]),
                             mean(Output.model$q2.5$psi[c(6),c(1,2,3,4,5,6,7,8,9),28]),mean(Output.model$q2.5$psi[c(7),c(1,2,3,4,5,6,7,8,9),28]),mean(Output.model$q2.5$psi[c(8),c(1,2,3,4,5,6,7,8,9),28]),
                             mean(Output.model$q2.5$psi[c(9),c(1,2,3,4,5,6,7,8,9),28]),mean(Output.model$q2.5$psi[c(10),c(1,2,3,4,5,6,7,8,9),28]),mean(Output.model$q2.5$psi[c(11),c(1,2),28]),
                             mean(Output.model$q2.5$psi[c(12),c(1,2),28]),mean(Output.model$q2.5$psi[c(13),c(1,2),28]),mean(Output.model$q2.5$psi[c(14),c(1,2),28]),
                             mean(Output.model$q2.5$psi[c(15),c(1,2),28]),mean(Output.model$q2.5$psi[c(16),c(1,2),28]),mean(Output.model$q2.5$psi[c(17),c(1,2),28]),
                             mean(Output.model$q2.5$psi[c(18),c(1,2),28]),mean(Output.model$q2.5$psi[c(19),c(1,2),28]))),
                      mean(c(mean(Output.model$q2.5$psi[c(2),c(1,2,3,4,5,6,7,8,9),46]),mean(Output.model$q2.5$psi[c(3),c(1,2,3,4,5,6,7,8,9),46]),mean(Output.model$q2.5$psi[c(4),c(1,2,3,4,5,6,7,8,9),46]),
                             mean(Output.model$q2.5$psi[c(6),c(1,2,3,4,5,6,7,8,9),46]),mean(Output.model$q2.5$psi[c(7),c(1,2,3,4,5,6,7,8,9),46]),mean(Output.model$q2.5$psi[c(8),c(1,2,3,4,5,6,7,8,9),46]),
                             mean(Output.model$q2.5$psi[c(9),c(1,2,3,4,5,6,7,8,9),46]),mean(Output.model$q2.5$psi[c(10),c(1,2,3,4,5,6,7,8,9),46]),mean(Output.model$q2.5$psi[c(11),c(1,2),46]),
                             mean(Output.model$q2.5$psi[c(12),c(1,2),46]),mean(Output.model$q2.5$psi[c(13),c(1,2),46]),mean(Output.model$q2.5$psi[c(14),c(1,2),46]),
                             mean(Output.model$q2.5$psi[c(15),c(1,2),46]),mean(Output.model$q2.5$psi[c(16),c(1,2),46]),mean(Output.model$q2.5$psi[c(17),c(1,2),46]),
                             mean(Output.model$q2.5$psi[c(18),c(1,2),46]),mean(Output.model$q2.5$psi[c(19),c(1,2),46]))),
                      mean(c(mean(Output.model$q2.5$psi[c(2),c(1,2,3,4,5,6,7,8,9),41]),mean(Output.model$q2.5$psi[c(3),c(1,2,3,4,5,6,7,8,9),41]),mean(Output.model$q2.5$psi[c(4),c(1,2,3,4,5,6,7,8,9),41]),
                             mean(Output.model$q2.5$psi[c(6),c(1,2,3,4,5,6,7,8,9),41]),mean(Output.model$q2.5$psi[c(7),c(1,2,3,4,5,6,7,8,9),41]),mean(Output.model$q2.5$psi[c(8),c(1,2,3,4,5,6,7,8,9),41]),
                             mean(Output.model$q2.5$psi[c(9),c(1,2,3,4,5,6,7,8,9),41]),mean(Output.model$q2.5$psi[c(10),c(1,2,3,4,5,6,7,8,9),41]),mean(Output.model$q2.5$psi[c(11),c(1,2),41]),
                             mean(Output.model$q2.5$psi[c(12),c(1,2),41]),mean(Output.model$q2.5$psi[c(13),c(1,2),41]),mean(Output.model$q2.5$psi[c(14),c(1,2),41]),
                             mean(Output.model$q2.5$psi[c(15),c(1,2),41]),mean(Output.model$q2.5$psi[c(16),c(1,2),41]),mean(Output.model$q2.5$psi[c(17),c(1,2),41]),
                             mean(Output.model$q2.5$psi[c(18),c(1,2),41]),mean(Output.model$q2.5$psi[c(19),c(1,2),41]))),
                      mean(c(mean(Output.model$q2.5$psi[c(2),c(1,2,3,4,5,6,7,8,9),16]),mean(Output.model$q2.5$psi[c(3),c(1,2,3,4,5,6,7,8,9),16]),mean(Output.model$q2.5$psi[c(4),c(1,2,3,4,5,6,7,8,9),16]),
                             mean(Output.model$q2.5$psi[c(6),c(1,2,3,4,5,6,7,8,9),16]),mean(Output.model$q2.5$psi[c(7),c(1,2,3,4,5,6,7,8,9),16]),mean(Output.model$q2.5$psi[c(8),c(1,2,3,4,5,6,7,8,9),16]),
                             mean(Output.model$q2.5$psi[c(9),c(1,2,3,4,5,6,7,8,9),16]),mean(Output.model$q2.5$psi[c(10),c(1,2,3,4,5,6,7,8,9),16]),mean(Output.model$q2.5$psi[c(11),c(1,2),16]),
                             mean(Output.model$q2.5$psi[c(12),c(1,2),16]),mean(Output.model$q2.5$psi[c(13),c(1,2),16]),mean(Output.model$q2.5$psi[c(14),c(1,2),16]),
                             mean(Output.model$q2.5$psi[c(15),c(1,2),16]),mean(Output.model$q2.5$psi[c(16),c(1,2),16]),mean(Output.model$q2.5$psi[c(17),c(1,2),16]),
                             mean(Output.model$q2.5$psi[c(18),c(1,2),16]),mean(Output.model$q2.5$psi[c(19),c(1,2),16]))),
                      mean(c(mean(Output.model$q2.5$psi[c(2),c(1,2,3,4,5,6,7,8,9),36]),mean(Output.model$q2.5$psi[c(3),c(1,2,3,4,5,6,7,8,9),36]),mean(Output.model$q2.5$psi[c(4),c(1,2,3,4,5,6,7,8,9),36]),
                             mean(Output.model$q2.5$psi[c(6),c(1,2,3,4,5,6,7,8,9),36]),mean(Output.model$q2.5$psi[c(7),c(1,2,3,4,5,6,7,8,9),36]),mean(Output.model$q2.5$psi[c(8),c(1,2,3,4,5,6,7,8,9),36]),
                             mean(Output.model$q2.5$psi[c(9),c(1,2,3,4,5,6,7,8,9),36]),mean(Output.model$q2.5$psi[c(10),c(1,2,3,4,5,6,7,8,9),36]),mean(Output.model$q2.5$psi[c(11),c(1,2),36]),
                             mean(Output.model$q2.5$psi[c(12),c(1,2),36]),mean(Output.model$q2.5$psi[c(13),c(1,2),36]),mean(Output.model$q2.5$psi[c(14),c(1,2),36]),
                             mean(Output.model$q2.5$psi[c(15),c(1,2),36]),mean(Output.model$q2.5$psi[c(16),c(1,2),36]),mean(Output.model$q2.5$psi[c(17),c(1,2),36]),
                             mean(Output.model$q2.5$psi[c(18),c(1,2),36]),mean(Output.model$q2.5$psi[c(19),c(1,2),36]))),
                      mean(c(mean(Output.model$q2.5$psi[c(2),c(1,2,3,4,5,6,7,8,9),14]),mean(Output.model$q2.5$psi[c(3),c(1,2,3,4,5,6,7,8,9),14]),mean(Output.model$q2.5$psi[c(4),c(1,2,3,4,5,6,7,8,9),14]),
                             mean(Output.model$q2.5$psi[c(6),c(1,2,3,4,5,6,7,8,9),14]),mean(Output.model$q2.5$psi[c(7),c(1,2,3,4,5,6,7,8,9),14]),mean(Output.model$q2.5$psi[c(8),c(1,2,3,4,5,6,7,8,9),14]),
                             mean(Output.model$q2.5$psi[c(9),c(1,2,3,4,5,6,7,8,9),14]),mean(Output.model$q2.5$psi[c(10),c(1,2,3,4,5,6,7,8,9),14]),mean(Output.model$q2.5$psi[c(11),c(1,2),14]),
                             mean(Output.model$q2.5$psi[c(12),c(1,2),14]),mean(Output.model$q2.5$psi[c(13),c(1,2),14]),mean(Output.model$q2.5$psi[c(14),c(1,2),14]),
                             mean(Output.model$q2.5$psi[c(15),c(1,2),14]),mean(Output.model$q2.5$psi[c(16),c(1,2),14]),mean(Output.model$q2.5$psi[c(17),c(1,2),14]),
                             mean(Output.model$q2.5$psi[c(18),c(1,2),14]),mean(Output.model$q2.5$psi[c(19),c(1,2),14]))),
                      mean(c(mean(Output.model$q2.5$psi[c(2),c(1,2,3,4,5,6,7,8,9),11]),mean(Output.model$q2.5$psi[c(3),c(1,2,3,4,5,6,7,8,9),11]),mean(Output.model$q2.5$psi[c(4),c(1,2,3,4,5,6,7,8,9),11]),
                             mean(Output.model$q2.5$psi[c(6),c(1,2,3,4,5,6,7,8,9),11]),mean(Output.model$q2.5$psi[c(7),c(1,2,3,4,5,6,7,8,9),11]),mean(Output.model$q2.5$psi[c(8),c(1,2,3,4,5,6,7,8,9),11]),
                             mean(Output.model$q2.5$psi[c(9),c(1,2,3,4,5,6,7,8,9),11]),mean(Output.model$q2.5$psi[c(10),c(1,2,3,4,5,6,7,8,9),11]),mean(Output.model$q2.5$psi[c(11),c(1,2),11]),
                             mean(Output.model$q2.5$psi[c(12),c(1,2),11]),mean(Output.model$q2.5$psi[c(13),c(1,2),11]),mean(Output.model$q2.5$psi[c(14),c(1,2),11]),
                             mean(Output.model$q2.5$psi[c(15),c(1,2),11]),mean(Output.model$q2.5$psi[c(16),c(1,2),11]),mean(Output.model$q2.5$psi[c(17),c(1,2),11]),
                             mean(Output.model$q2.5$psi[c(18),c(1,2),11]),mean(Output.model$q2.5$psi[c(19),c(1,2),11]))),
                      mean(c(mean(Output.model$q2.5$psi[c(2),c(1,2,3,4,5,6,7,8,9),31]),mean(Output.model$q2.5$psi[c(3),c(1,2,3,4,5,6,7,8,9),31]),mean(Output.model$q2.5$psi[c(4),c(1,2,3,4,5,6,7,8,9),31]),
                             mean(Output.model$q2.5$psi[c(6),c(1,2,3,4,5,6,7,8,9),31]),mean(Output.model$q2.5$psi[c(7),c(1,2,3,4,5,6,7,8,9),31]),mean(Output.model$q2.5$psi[c(8),c(1,2,3,4,5,6,7,8,9),31]),
                             mean(Output.model$q2.5$psi[c(9),c(1,2,3,4,5,6,7,8,9),31]),mean(Output.model$q2.5$psi[c(10),c(1,2,3,4,5,6,7,8,9),31]),mean(Output.model$q2.5$psi[c(11),c(1,2),31]),
                             mean(Output.model$q2.5$psi[c(12),c(1,2),31]),mean(Output.model$q2.5$psi[c(13),c(1,2),31]),mean(Output.model$q2.5$psi[c(14),c(1,2),31]),
                             mean(Output.model$q2.5$psi[c(15),c(1,2),31]),mean(Output.model$q2.5$psi[c(16),c(1,2),31]),mean(Output.model$q2.5$psi[c(17),c(1,2),31]),
                             mean(Output.model$q2.5$psi[c(18),c(1,2),31]),mean(Output.model$q2.5$psi[c(19),c(1,2),31]))),
                      mean(c(mean(Output.model$q2.5$psi[c(2),c(1,2,3,4,5,6,7,8,9),6]),mean(Output.model$q2.5$psi[c(3),c(1,2,3,4,5,6,7,8,9),6]),mean(Output.model$q2.5$psi[c(4),c(1,2,3,4,5,6,7,8,9),6]),
                             mean(Output.model$q2.5$psi[c(6),c(1,2,3,4,5,6,7,8,9),6]),mean(Output.model$q2.5$psi[c(7),c(1,2,3,4,5,6,7,8,9),6]),mean(Output.model$q2.5$psi[c(8),c(1,2,3,4,5,6,7,8,9),6]),
                             mean(Output.model$q2.5$psi[c(9),c(1,2,3,4,5,6,7,8,9),6]),mean(Output.model$q2.5$psi[c(10),c(1,2,3,4,5,6,7,8,9),6]),mean(Output.model$q2.5$psi[c(11),c(1,2),6]),
                             mean(Output.model$q2.5$psi[c(12),c(1,2),6]),mean(Output.model$q2.5$psi[c(13),c(1,2),6]),mean(Output.model$q2.5$psi[c(14),c(1,2),6]),
                             mean(Output.model$q2.5$psi[c(15),c(1,2),6]),mean(Output.model$q2.5$psi[c(16),c(1,2),6]),mean(Output.model$q2.5$psi[c(17),c(1,2),6]),
                             mean(Output.model$q2.5$psi[c(18),c(1,2),6]),mean(Output.model$q2.5$psi[c(19),c(1,2),6])))),
                    c(mean(c(mean(Output.model$q25$psi[c(2),c(1,2,3,4,5,6,7,8,9),45]),mean(Output.model$q25$psi[c(3),c(1,2,3,4,5,6,7,8,9),45]),mean(Output.model$q25$psi[c(4),c(1,2,3,4,5,6,7,8,9),45]),
                             mean(Output.model$q25$psi[c(6),c(1,2,3,4,5,6,7,8,9),45]),mean(Output.model$q25$psi[c(7),c(1,2,3,4,5,6,7,8,9),45]),mean(Output.model$q25$psi[c(8),c(1,2,3,4,5,6,7,8,9),45]),
                             mean(Output.model$q25$psi[c(9),c(1,2,3,4,5,6,7,8,9),45]),mean(Output.model$q25$psi[c(10),c(1,2,3,4,5,6,7,8,9),45]),mean(Output.model$q25$psi[c(11),c(1,2),45]),
                             mean(Output.model$q25$psi[c(12),c(1,2),45]),mean(Output.model$q25$psi[c(13),c(1,2),45]),mean(Output.model$q25$psi[c(14),c(1,2),45]),
                             mean(Output.model$q25$psi[c(15),c(1,2),45]),mean(Output.model$q25$psi[c(16),c(1,2),45]),mean(Output.model$q25$psi[c(17),c(1,2),45]),
                             mean(Output.model$q25$psi[c(18),c(1,2),45]),mean(Output.model$q25$psi[c(19),c(1,2),45]))),
                      mean(c(mean(Output.model$q25$psi[c(2),c(1,2,3,4,5,6,7,8,9),1]),mean(Output.model$q25$psi[c(3),c(1,2,3,4,5,6,7,8,9),1]),mean(Output.model$q25$psi[c(4),c(1,2,3,4,5,6,7,8,9),1]),
                             mean(Output.model$q25$psi[c(6),c(1,2,3,4,5,6,7,8,9),1]),mean(Output.model$q25$psi[c(7),c(1,2,3,4,5,6,7,8,9),1]),mean(Output.model$q25$psi[c(8),c(1,2,3,4,5,6,7,8,9),1]),
                             mean(Output.model$q25$psi[c(9),c(1,2,3,4,5,6,7,8,9),1]),mean(Output.model$q25$psi[c(10),c(1,2,3,4,5,6,7,8,9),1]),mean(Output.model$q25$psi[c(11),c(1,2),1]),
                             mean(Output.model$q25$psi[c(12),c(1,2),1]),mean(Output.model$q25$psi[c(13),c(1,2),1]),mean(Output.model$q25$psi[c(14),c(1,2),1]),
                             mean(Output.model$q25$psi[c(15),c(1,2),1]),mean(Output.model$q25$psi[c(16),c(1,2),1]),mean(Output.model$q25$psi[c(17),c(1,2),1]),
                             mean(Output.model$q25$psi[c(18),c(1,2),1]),mean(Output.model$q25$psi[c(19),c(1,2),1]))),
                      mean(c(mean(Output.model$q25$psi[c(2),c(1,2,3,4,5,6,7,8,9),15]),mean(Output.model$q25$psi[c(3),c(1,2,3,4,5,6,7,8,9),15]),mean(Output.model$q25$psi[c(4),c(1,2,3,4,5,6,7,8,9),15]),
                             mean(Output.model$q25$psi[c(6),c(1,2,3,4,5,6,7,8,9),15]),mean(Output.model$q25$psi[c(7),c(1,2,3,4,5,6,7,8,9),15]),mean(Output.model$q25$psi[c(8),c(1,2,3,4,5,6,7,8,9),15]),
                             mean(Output.model$q25$psi[c(9),c(1,2,3,4,5,6,7,8,9),15]),mean(Output.model$q25$psi[c(10),c(1,2,3,4,5,6,7,8,9),15]),mean(Output.model$q25$psi[c(11),c(1,2),15]),
                             mean(Output.model$q25$psi[c(12),c(1,2),15]),mean(Output.model$q25$psi[c(13),c(1,2),15]),mean(Output.model$q25$psi[c(14),c(1,2),15]),
                             mean(Output.model$q25$psi[c(15),c(1,2),15]),mean(Output.model$q25$psi[c(16),c(1,2),15]),mean(Output.model$q25$psi[c(17),c(1,2),15]),
                             mean(Output.model$q25$psi[c(18),c(1,2),15]),mean(Output.model$q25$psi[c(19),c(1,2),15]))),
                      mean(c(mean(Output.model$q25$psi[c(2),c(1,2,3,4,5,6,7,8,9),7]),mean(Output.model$q25$psi[c(3),c(1,2,3,4,5,6,7,8,9),7]),mean(Output.model$q25$psi[c(4),c(1,2,3,4,5,6,7,8,9),7]),
                             mean(Output.model$q25$psi[c(6),c(1,2,3,4,5,6,7,8,9),7]),mean(Output.model$q25$psi[c(7),c(1,2,3,4,5,6,7,8,9),7]),mean(Output.model$q25$psi[c(8),c(1,2,3,4,5,6,7,8,9),7]),
                             mean(Output.model$q25$psi[c(9),c(1,2,3,4,5,6,7,8,9),7]),mean(Output.model$q25$psi[c(10),c(1,2,3,4,5,6,7,8,9),7]),mean(Output.model$q25$psi[c(11),c(1,2),7]),
                             mean(Output.model$q25$psi[c(12),c(1,2),7]),mean(Output.model$q25$psi[c(13),c(1,2),7]),mean(Output.model$q25$psi[c(14),c(1,2),7]),
                             mean(Output.model$q25$psi[c(15),c(1,2),7]),mean(Output.model$q25$psi[c(16),c(1,2),7]),mean(Output.model$q25$psi[c(17),c(1,2),7]),
                             mean(Output.model$q25$psi[c(18),c(1,2),7]),mean(Output.model$q25$psi[c(19),c(1,2),7]))),
                      mean(c(mean(Output.model$q25$psi[c(2),c(1,2,3,4,5,6,7,8,9),22]),mean(Output.model$q25$psi[c(3),c(1,2,3,4,5,6,7,8,9),22]),mean(Output.model$q25$psi[c(4),c(1,2,3,4,5,6,7,8,9),22]),
                             mean(Output.model$q25$psi[c(6),c(1,2,3,4,5,6,7,8,9),22]),mean(Output.model$q25$psi[c(7),c(1,2,3,4,5,6,7,8,9),22]),mean(Output.model$q25$psi[c(8),c(1,2,3,4,5,6,7,8,9),22]),
                             mean(Output.model$q25$psi[c(9),c(1,2,3,4,5,6,7,8,9),22]),mean(Output.model$q25$psi[c(10),c(1,2,3,4,5,6,7,8,9),22]),mean(Output.model$q25$psi[c(11),c(1,2),22]),
                             mean(Output.model$q25$psi[c(12),c(1,2),22]),mean(Output.model$q25$psi[c(13),c(1,2),22]),mean(Output.model$q25$psi[c(14),c(1,2),22]),
                             mean(Output.model$q25$psi[c(15),c(1,2),22]),mean(Output.model$q25$psi[c(16),c(1,2),22]),mean(Output.model$q25$psi[c(17),c(1,2),22]),
                             mean(Output.model$q25$psi[c(18),c(1,2),22]),mean(Output.model$q25$psi[c(19),c(1,2),22]))),
                      mean(c(mean(Output.model$q25$psi[c(2),c(1,2,3,4,5,6,7,8,9),29]),mean(Output.model$q25$psi[c(3),c(1,2,3,4,5,6,7,8,9),29]),mean(Output.model$q25$psi[c(4),c(1,2,3,4,5,6,7,8,9),29]),
                             mean(Output.model$q25$psi[c(6),c(1,2,3,4,5,6,7,8,9),29]),mean(Output.model$q25$psi[c(7),c(1,2,3,4,5,6,7,8,9),29]),mean(Output.model$q25$psi[c(8),c(1,2,3,4,5,6,7,8,9),29]),
                             mean(Output.model$q25$psi[c(9),c(1,2,3,4,5,6,7,8,9),29]),mean(Output.model$q25$psi[c(10),c(1,2,3,4,5,6,7,8,9),29]),mean(Output.model$q25$psi[c(11),c(1,2),29]),
                             mean(Output.model$q25$psi[c(12),c(1,2),29]),mean(Output.model$q25$psi[c(13),c(1,2),29]),mean(Output.model$q25$psi[c(14),c(1,2),29]),
                             mean(Output.model$q25$psi[c(15),c(1,2),29]),mean(Output.model$q25$psi[c(16),c(1,2),29]),mean(Output.model$q25$psi[c(17),c(1,2),29]),
                             mean(Output.model$q25$psi[c(18),c(1,2),29]),mean(Output.model$q25$psi[c(19),c(1,2),29]))),
                      mean(c(mean(Output.model$q25$psi[c(2),c(1,2,3,4,5,6,7,8,9),18]),mean(Output.model$q25$psi[c(3),c(1,2,3,4,5,6,7,8,9),18]),mean(Output.model$q25$psi[c(4),c(1,2,3,4,5,6,7,8,9),18]),
                             mean(Output.model$q25$psi[c(6),c(1,2,3,4,5,6,7,8,9),18]),mean(Output.model$q25$psi[c(7),c(1,2,3,4,5,6,7,8,9),18]),mean(Output.model$q25$psi[c(8),c(1,2,3,4,5,6,7,8,9),18]),
                             mean(Output.model$q25$psi[c(9),c(1,2,3,4,5,6,7,8,9),18]),mean(Output.model$q25$psi[c(10),c(1,2,3,4,5,6,7,8,9),18]),mean(Output.model$q25$psi[c(11),c(1,2),18]),
                             mean(Output.model$q25$psi[c(12),c(1,2),18]),mean(Output.model$q25$psi[c(13),c(1,2),18]),mean(Output.model$q25$psi[c(14),c(1,2),18]),
                             mean(Output.model$q25$psi[c(15),c(1,2),18]),mean(Output.model$q25$psi[c(16),c(1,2),18]),mean(Output.model$q25$psi[c(17),c(1,2),18]),
                             mean(Output.model$q25$psi[c(18),c(1,2),18]),mean(Output.model$q25$psi[c(19),c(1,2),18]))),
                      mean(c(mean(Output.model$q25$psi[c(2),c(1,2,3,4,5,6,7,8,9),33]),mean(Output.model$q25$psi[c(3),c(1,2,3,4,5,6,7,8,9),33]),mean(Output.model$q25$psi[c(4),c(1,2,3,4,5,6,7,8,9),33]),
                             mean(Output.model$q25$psi[c(6),c(1,2,3,4,5,6,7,8,9),33]),mean(Output.model$q25$psi[c(7),c(1,2,3,4,5,6,7,8,9),33]),mean(Output.model$q25$psi[c(8),c(1,2,3,4,5,6,7,8,9),33]),
                             mean(Output.model$q25$psi[c(9),c(1,2,3,4,5,6,7,8,9),33]),mean(Output.model$q25$psi[c(10),c(1,2,3,4,5,6,7,8,9),33]),mean(Output.model$q25$psi[c(11),c(1,2),33]),
                             mean(Output.model$q25$psi[c(12),c(1,2),33]),mean(Output.model$q25$psi[c(13),c(1,2),33]),mean(Output.model$q25$psi[c(14),c(1,2),33]),
                             mean(Output.model$q25$psi[c(15),c(1,2),33]),mean(Output.model$q25$psi[c(16),c(1,2),33]),mean(Output.model$q25$psi[c(17),c(1,2),33]),
                             mean(Output.model$q25$psi[c(18),c(1,2),33]),mean(Output.model$q25$psi[c(19),c(1,2),33]))),
                      mean(c(mean(Output.model$q25$psi[c(2),c(1,2,3,4,5,6,7,8,9),12]),mean(Output.model$q25$psi[c(3),c(1,2,3,4,5,6,7,8,9),12]),mean(Output.model$q25$psi[c(4),c(1,2,3,4,5,6,7,8,9),12]),
                             mean(Output.model$q25$psi[c(6),c(1,2,3,4,5,6,7,8,9),12]),mean(Output.model$q25$psi[c(7),c(1,2,3,4,5,6,7,8,9),12]),mean(Output.model$q25$psi[c(8),c(1,2,3,4,5,6,7,8,9),12]),
                             mean(Output.model$q25$psi[c(9),c(1,2,3,4,5,6,7,8,9),12]),mean(Output.model$q25$psi[c(10),c(1,2,3,4,5,6,7,8,9),12]),mean(Output.model$q25$psi[c(11),c(1,2),12]),
                             mean(Output.model$q25$psi[c(12),c(1,2),12]),mean(Output.model$q25$psi[c(13),c(1,2),12]),mean(Output.model$q25$psi[c(14),c(1,2),12]),
                             mean(Output.model$q25$psi[c(15),c(1,2),12]),mean(Output.model$q25$psi[c(16),c(1,2),12]),mean(Output.model$q25$psi[c(17),c(1,2),12]),
                             mean(Output.model$q25$psi[c(18),c(1,2),12]),mean(Output.model$q25$psi[c(19),c(1,2),12]))),
                      mean(c(mean(Output.model$q25$psi[c(2),c(1,2,3,4,5,6,7,8,9),9]),mean(Output.model$q25$psi[c(3),c(1,2,3,4,5,6,7,8,9),9]),mean(Output.model$q25$psi[c(4),c(1,2,3,4,5,6,7,8,9),9]),
                             mean(Output.model$q25$psi[c(6),c(1,2,3,4,5,6,7,8,9),9]),mean(Output.model$q25$psi[c(7),c(1,2,3,4,5,6,7,8,9),9]),mean(Output.model$q25$psi[c(8),c(1,2,3,4,5,6,7,8,9),9]),
                             mean(Output.model$q25$psi[c(9),c(1,2,3,4,5,6,7,8,9),9]),mean(Output.model$q25$psi[c(10),c(1,2,3,4,5,6,7,8,9),9]),mean(Output.model$q25$psi[c(11),c(1,2),9]),
                             mean(Output.model$q25$psi[c(12),c(1,2),9]),mean(Output.model$q25$psi[c(13),c(1,2),9]),mean(Output.model$q25$psi[c(14),c(1,2),9]),
                             mean(Output.model$q25$psi[c(15),c(1,2),9]),mean(Output.model$q25$psi[c(16),c(1,2),9]),mean(Output.model$q25$psi[c(17),c(1,2),9]),
                             mean(Output.model$q25$psi[c(18),c(1,2),9]),mean(Output.model$q25$psi[c(19),c(1,2),9]))),
                      mean(c(mean(Output.model$q25$psi[c(2),c(1,2,3,4,5,6,7,8,9),47]),mean(Output.model$q25$psi[c(3),c(1,2,3,4,5,6,7,8,9),47]),mean(Output.model$q25$psi[c(4),c(1,2,3,4,5,6,7,8,9),47]),
                             mean(Output.model$q25$psi[c(6),c(1,2,3,4,5,6,7,8,9),47]),mean(Output.model$q25$psi[c(7),c(1,2,3,4,5,6,7,8,9),47]),mean(Output.model$q25$psi[c(8),c(1,2,3,4,5,6,7,8,9),47]),
                             mean(Output.model$q25$psi[c(9),c(1,2,3,4,5,6,7,8,9),47]),mean(Output.model$q25$psi[c(10),c(1,2,3,4,5,6,7,8,9),47]),mean(Output.model$q25$psi[c(11),c(1,2),47]),
                             mean(Output.model$q25$psi[c(12),c(1,2),47]),mean(Output.model$q25$psi[c(13),c(1,2),47]),mean(Output.model$q25$psi[c(14),c(1,2),47]),
                             mean(Output.model$q25$psi[c(15),c(1,2),47]),mean(Output.model$q25$psi[c(16),c(1,2),47]),mean(Output.model$q25$psi[c(17),c(1,2),47]),
                             mean(Output.model$q25$psi[c(18),c(1,2),47]),mean(Output.model$q25$psi[c(19),c(1,2),47]))),
                      mean(c(mean(Output.model$q25$psi[c(2),c(1,2,3,4,5,6,7,8,9),30]),mean(Output.model$q25$psi[c(3),c(1,2,3,4,5,6,7,8,9),30]),mean(Output.model$q25$psi[c(4),c(1,2,3,4,5,6,7,8,9),30]),
                             mean(Output.model$q25$psi[c(6),c(1,2,3,4,5,6,7,8,9),30]),mean(Output.model$q25$psi[c(7),c(1,2,3,4,5,6,7,8,9),30]),mean(Output.model$q25$psi[c(8),c(1,2,3,4,5,6,7,8,9),30]),
                             mean(Output.model$q25$psi[c(9),c(1,2,3,4,5,6,7,8,9),30]),mean(Output.model$q25$psi[c(10),c(1,2,3,4,5,6,7,8,9),30]),mean(Output.model$q25$psi[c(11),c(1,2),30]),
                             mean(Output.model$q25$psi[c(12),c(1,2),30]),mean(Output.model$q25$psi[c(13),c(1,2),30]),mean(Output.model$q25$psi[c(14),c(1,2),30]),
                             mean(Output.model$q25$psi[c(15),c(1,2),30]),mean(Output.model$q25$psi[c(16),c(1,2),30]),mean(Output.model$q25$psi[c(17),c(1,2),30]),
                             mean(Output.model$q25$psi[c(18),c(1,2),30]),mean(Output.model$q25$psi[c(19),c(1,2),30]))),
                      mean(c(mean(Output.model$q25$psi[c(2),c(1,2,3,4,5,6,7,8,9),40]),mean(Output.model$q25$psi[c(3),c(1,2,3,4,5,6,7,8,9),40]),mean(Output.model$q25$psi[c(4),c(1,2,3,4,5,6,7,8,9),40]),
                             mean(Output.model$q25$psi[c(6),c(1,2,3,4,5,6,7,8,9),40]),mean(Output.model$q25$psi[c(7),c(1,2,3,4,5,6,7,8,9),40]),mean(Output.model$q25$psi[c(8),c(1,2,3,4,5,6,7,8,9),40]),
                             mean(Output.model$q25$psi[c(9),c(1,2,3,4,5,6,7,8,9),40]),mean(Output.model$q25$psi[c(10),c(1,2,3,4,5,6,7,8,9),40]),mean(Output.model$q25$psi[c(11),c(1,2),40]),
                             mean(Output.model$q25$psi[c(12),c(1,2),40]),mean(Output.model$q25$psi[c(13),c(1,2),40]),mean(Output.model$q25$psi[c(14),c(1,2),40]),
                             mean(Output.model$q25$psi[c(15),c(1,2),40]),mean(Output.model$q25$psi[c(16),c(1,2),40]),mean(Output.model$q25$psi[c(17),c(1,2),40]),
                             mean(Output.model$q25$psi[c(18),c(1,2),40]),mean(Output.model$q25$psi[c(19),c(1,2),40]))),
                      mean(c(mean(Output.model$q25$psi[c(2),c(1,2,3,4,5,6,7,8,9),5]),mean(Output.model$q25$psi[c(3),c(1,2,3,4,5,6,7,8,9),5]),mean(Output.model$q25$psi[c(4),c(1,2,3,4,5,6,7,8,9),5]),
                             mean(Output.model$q25$psi[c(6),c(1,2,3,4,5,6,7,8,9),5]),mean(Output.model$q25$psi[c(7),c(1,2,3,4,5,6,7,8,9),5]),mean(Output.model$q25$psi[c(8),c(1,2,3,4,5,6,7,8,9),5]),
                             mean(Output.model$q25$psi[c(9),c(1,2,3,4,5,6,7,8,9),5]),mean(Output.model$q25$psi[c(10),c(1,2,3,4,5,6,7,8,9),5]),mean(Output.model$q25$psi[c(11),c(1,2),5]),
                             mean(Output.model$q25$psi[c(12),c(1,2),5]),mean(Output.model$q25$psi[c(13),c(1,2),5]),mean(Output.model$q25$psi[c(14),c(1,2),5]),
                             mean(Output.model$q25$psi[c(15),c(1,2),5]),mean(Output.model$q25$psi[c(16),c(1,2),5]),mean(Output.model$q25$psi[c(17),c(1,2),5]),
                             mean(Output.model$q25$psi[c(18),c(1,2),5]),mean(Output.model$q25$psi[c(19),c(1,2),5]))),
                      mean(c(mean(Output.model$q25$psi[c(2),c(1,2,3,4,5,6,7,8,9),20]),mean(Output.model$q25$psi[c(3),c(1,2,3,4,5,6,7,8,9),20]),mean(Output.model$q25$psi[c(4),c(1,2,3,4,5,6,7,8,9),20]),
                             mean(Output.model$q25$psi[c(6),c(1,2,3,4,5,6,7,8,9),20]),mean(Output.model$q25$psi[c(7),c(1,2,3,4,5,6,7,8,9),20]),mean(Output.model$q25$psi[c(8),c(1,2,3,4,5,6,7,8,9),20]),
                             mean(Output.model$q25$psi[c(9),c(1,2,3,4,5,6,7,8,9),20]),mean(Output.model$q25$psi[c(10),c(1,2,3,4,5,6,7,8,9),20]),mean(Output.model$q25$psi[c(11),c(1,2),20]),
                             mean(Output.model$q25$psi[c(12),c(1,2),20]),mean(Output.model$q25$psi[c(13),c(1,2),20]),mean(Output.model$q25$psi[c(14),c(1,2),20]),
                             mean(Output.model$q25$psi[c(15),c(1,2),20]),mean(Output.model$q25$psi[c(16),c(1,2),20]),mean(Output.model$q25$psi[c(17),c(1,2),20]),
                             mean(Output.model$q25$psi[c(18),c(1,2),20]),mean(Output.model$q25$psi[c(19),c(1,2),20]))),
                      mean(c(mean(Output.model$q25$psi[c(2),c(1,2,3,4,5,6,7,8,9),23]),mean(Output.model$q25$psi[c(3),c(1,2,3,4,5,6,7,8,9),23]),mean(Output.model$q25$psi[c(4),c(1,2,3,4,5,6,7,8,9),23]),
                             mean(Output.model$q25$psi[c(6),c(1,2,3,4,5,6,7,8,9),23]),mean(Output.model$q25$psi[c(7),c(1,2,3,4,5,6,7,8,9),23]),mean(Output.model$q25$psi[c(8),c(1,2,3,4,5,6,7,8,9),23]),
                             mean(Output.model$q25$psi[c(9),c(1,2,3,4,5,6,7,8,9),23]),mean(Output.model$q25$psi[c(10),c(1,2,3,4,5,6,7,8,9),23]),mean(Output.model$q25$psi[c(11),c(1,2),23]),
                             mean(Output.model$q25$psi[c(12),c(1,2),23]),mean(Output.model$q25$psi[c(13),c(1,2),23]),mean(Output.model$q25$psi[c(14),c(1,2),23]),
                             mean(Output.model$q25$psi[c(15),c(1,2),23]),mean(Output.model$q25$psi[c(16),c(1,2),23]),mean(Output.model$q25$psi[c(17),c(1,2),23]),
                             mean(Output.model$q25$psi[c(18),c(1,2),23]),mean(Output.model$q25$psi[c(19),c(1,2),23]))),
                      mean(c(mean(Output.model$q25$psi[c(2),c(1,2,3,4,5,6,7,8,9),3]),mean(Output.model$q25$psi[c(3),c(1,2,3,4,5,6,7,8,9),3]),mean(Output.model$q25$psi[c(4),c(1,2,3,4,5,6,7,8,9),3]),
                             mean(Output.model$q25$psi[c(6),c(1,2,3,4,5,6,7,8,9),3]),mean(Output.model$q25$psi[c(7),c(1,2,3,4,5,6,7,8,9),3]),mean(Output.model$q25$psi[c(8),c(1,2,3,4,5,6,7,8,9),3]),
                             mean(Output.model$q25$psi[c(9),c(1,2,3,4,5,6,7,8,9),3]),mean(Output.model$q25$psi[c(10),c(1,2,3,4,5,6,7,8,9),3]),mean(Output.model$q25$psi[c(11),c(1,2),3]),
                             mean(Output.model$q25$psi[c(12),c(1,2),3]),mean(Output.model$q25$psi[c(13),c(1,2),3]),mean(Output.model$q25$psi[c(14),c(1,2),3]),
                             mean(Output.model$q25$psi[c(15),c(1,2),3]),mean(Output.model$q25$psi[c(16),c(1,2),3]),mean(Output.model$q25$psi[c(17),c(1,2),3]),
                             mean(Output.model$q25$psi[c(18),c(1,2),3]),mean(Output.model$q25$psi[c(19),c(1,2),3]))),
                      mean(c(mean(Output.model$q25$psi[c(2),c(1,2,3,4,5,6,7,8,9),27]),mean(Output.model$q25$psi[c(3),c(1,2,3,4,5,6,7,8,9),27]),mean(Output.model$q25$psi[c(4),c(1,2,3,4,5,6,7,8,9),27]),
                             mean(Output.model$q25$psi[c(6),c(1,2,3,4,5,6,7,8,9),27]),mean(Output.model$q25$psi[c(7),c(1,2,3,4,5,6,7,8,9),27]),mean(Output.model$q25$psi[c(8),c(1,2,3,4,5,6,7,8,9),27]),
                             mean(Output.model$q25$psi[c(9),c(1,2,3,4,5,6,7,8,9),27]),mean(Output.model$q25$psi[c(10),c(1,2,3,4,5,6,7,8,9),27]),mean(Output.model$q25$psi[c(11),c(1,2),27]),
                             mean(Output.model$q25$psi[c(12),c(1,2),27]),mean(Output.model$q25$psi[c(13),c(1,2),27]),mean(Output.model$q25$psi[c(14),c(1,2),27]),
                             mean(Output.model$q25$psi[c(15),c(1,2),27]),mean(Output.model$q25$psi[c(16),c(1,2),27]),mean(Output.model$q25$psi[c(17),c(1,2),27]),
                             mean(Output.model$q25$psi[c(18),c(1,2),27]),mean(Output.model$q25$psi[c(19),c(1,2),27]))),
                      mean(c(mean(Output.model$q25$psi[c(2),c(1,2,3,4,5,6,7,8,9),26]),mean(Output.model$q25$psi[c(3),c(1,2,3,4,5,6,7,8,9),26]),mean(Output.model$q25$psi[c(4),c(1,2,3,4,5,6,7,8,9),26]),
                             mean(Output.model$q25$psi[c(6),c(1,2,3,4,5,6,7,8,9),26]),mean(Output.model$q25$psi[c(7),c(1,2,3,4,5,6,7,8,9),26]),mean(Output.model$q25$psi[c(8),c(1,2,3,4,5,6,7,8,9),26]),
                             mean(Output.model$q25$psi[c(9),c(1,2,3,4,5,6,7,8,9),26]),mean(Output.model$q25$psi[c(10),c(1,2,3,4,5,6,7,8,9),26]),mean(Output.model$q25$psi[c(11),c(1,2),26]),
                             mean(Output.model$q25$psi[c(12),c(1,2),26]),mean(Output.model$q25$psi[c(13),c(1,2),26]),mean(Output.model$q25$psi[c(14),c(1,2),26]),
                             mean(Output.model$q25$psi[c(15),c(1,2),26]),mean(Output.model$q25$psi[c(16),c(1,2),26]),mean(Output.model$q25$psi[c(17),c(1,2),26]),
                             mean(Output.model$q25$psi[c(18),c(1,2),26]),mean(Output.model$q25$psi[c(19),c(1,2),26]))),
                      mean(c(mean(Output.model$q25$psi[c(2),c(1,2,3,4,5,6,7,8,9),44]),mean(Output.model$q25$psi[c(3),c(1,2,3,4,5,6,7,8,9),44]),mean(Output.model$q25$psi[c(4),c(1,2,3,4,5,6,7,8,9),44]),
                             mean(Output.model$q25$psi[c(6),c(1,2,3,4,5,6,7,8,9),44]),mean(Output.model$q25$psi[c(7),c(1,2,3,4,5,6,7,8,9),44]),mean(Output.model$q25$psi[c(8),c(1,2,3,4,5,6,7,8,9),44]),
                             mean(Output.model$q25$psi[c(9),c(1,2,3,4,5,6,7,8,9),44]),mean(Output.model$q25$psi[c(10),c(1,2,3,4,5,6,7,8,9),44]),mean(Output.model$q25$psi[c(11),c(1,2),44]),
                             mean(Output.model$q25$psi[c(12),c(1,2),44]),mean(Output.model$q25$psi[c(13),c(1,2),44]),mean(Output.model$q25$psi[c(14),c(1,2),44]),
                             mean(Output.model$q25$psi[c(15),c(1,2),44]),mean(Output.model$q25$psi[c(16),c(1,2),44]),mean(Output.model$q25$psi[c(17),c(1,2),44]),
                             mean(Output.model$q25$psi[c(18),c(1,2),44]),mean(Output.model$q25$psi[c(19),c(1,2),44]))),
                      mean(c(mean(Output.model$q25$psi[c(2),c(1,2,3,4,5,6,7,8,9),42]),mean(Output.model$q25$psi[c(3),c(1,2,3,4,5,6,7,8,9),42]),mean(Output.model$q25$psi[c(4),c(1,2,3,4,5,6,7,8,9),42]),
                             mean(Output.model$q25$psi[c(6),c(1,2,3,4,5,6,7,8,9),42]),mean(Output.model$q25$psi[c(7),c(1,2,3,4,5,6,7,8,9),42]),mean(Output.model$q25$psi[c(8),c(1,2,3,4,5,6,7,8,9),42]),
                             mean(Output.model$q25$psi[c(9),c(1,2,3,4,5,6,7,8,9),42]),mean(Output.model$q25$psi[c(10),c(1,2,3,4,5,6,7,8,9),42]),mean(Output.model$q25$psi[c(11),c(1,2),42]),
                             mean(Output.model$q25$psi[c(12),c(1,2),42]),mean(Output.model$q25$psi[c(13),c(1,2),42]),mean(Output.model$q25$psi[c(14),c(1,2),42]),
                             mean(Output.model$q25$psi[c(15),c(1,2),42]),mean(Output.model$q25$psi[c(16),c(1,2),42]),mean(Output.model$q25$psi[c(17),c(1,2),42]),
                             mean(Output.model$q25$psi[c(18),c(1,2),42]),mean(Output.model$q25$psi[c(19),c(1,2),42]))),
                      mean(c(mean(Output.model$q25$psi[c(2),c(1,2,3,4,5,6,7,8,9),43]),mean(Output.model$q25$psi[c(3),c(1,2,3,4,5,6,7,8,9),43]),mean(Output.model$q25$psi[c(4),c(1,2,3,4,5,6,7,8,9),43]),
                             mean(Output.model$q25$psi[c(6),c(1,2,3,4,5,6,7,8,9),43]),mean(Output.model$q25$psi[c(7),c(1,2,3,4,5,6,7,8,9),43]),mean(Output.model$q25$psi[c(8),c(1,2,3,4,5,6,7,8,9),43]),
                             mean(Output.model$q25$psi[c(9),c(1,2,3,4,5,6,7,8,9),43]),mean(Output.model$q25$psi[c(10),c(1,2,3,4,5,6,7,8,9),43]),mean(Output.model$q25$psi[c(11),c(1,2),43]),
                             mean(Output.model$q25$psi[c(12),c(1,2),43]),mean(Output.model$q25$psi[c(13),c(1,2),43]),mean(Output.model$q25$psi[c(14),c(1,2),43]),
                             mean(Output.model$q25$psi[c(15),c(1,2),43]),mean(Output.model$q25$psi[c(16),c(1,2),43]),mean(Output.model$q25$psi[c(17),c(1,2),43]),
                             mean(Output.model$q25$psi[c(18),c(1,2),43]),mean(Output.model$q25$psi[c(19),c(1,2),43]))),
                      mean(c(mean(Output.model$q25$psi[c(2),c(1,2,3,4,5,6,7,8,9),38]),mean(Output.model$q25$psi[c(3),c(1,2,3,4,5,6,7,8,9),38]),mean(Output.model$q25$psi[c(4),c(1,2,3,4,5,6,7,8,9),38]),
                             mean(Output.model$q25$psi[c(6),c(1,2,3,4,5,6,7,8,9),38]),mean(Output.model$q25$psi[c(7),c(1,2,3,4,5,6,7,8,9),38]),mean(Output.model$q25$psi[c(8),c(1,2,3,4,5,6,7,8,9),38]),
                             mean(Output.model$q25$psi[c(9),c(1,2,3,4,5,6,7,8,9),38]),mean(Output.model$q25$psi[c(10),c(1,2,3,4,5,6,7,8,9),38]),mean(Output.model$q25$psi[c(11),c(1,2),38]),
                             mean(Output.model$q25$psi[c(12),c(1,2),38]),mean(Output.model$q25$psi[c(13),c(1,2),38]),mean(Output.model$q25$psi[c(14),c(1,2),38]),
                             mean(Output.model$q25$psi[c(15),c(1,2),38]),mean(Output.model$q25$psi[c(16),c(1,2),38]),mean(Output.model$q25$psi[c(17),c(1,2),38]),
                             mean(Output.model$q25$psi[c(18),c(1,2),38]),mean(Output.model$q25$psi[c(19),c(1,2),38]))),
                      mean(c(mean(Output.model$q25$psi[c(2),c(1,2,3,4,5,6,7,8,9),24]),mean(Output.model$q25$psi[c(3),c(1,2,3,4,5,6,7,8,9),24]),mean(Output.model$q25$psi[c(4),c(1,2,3,4,5,6,7,8,9),24]),
                             mean(Output.model$q25$psi[c(6),c(1,2,3,4,5,6,7,8,9),24]),mean(Output.model$q25$psi[c(7),c(1,2,3,4,5,6,7,8,9),24]),mean(Output.model$q25$psi[c(8),c(1,2,3,4,5,6,7,8,9),24]),
                             mean(Output.model$q25$psi[c(9),c(1,2,3,4,5,6,7,8,9),24]),mean(Output.model$q25$psi[c(10),c(1,2,3,4,5,6,7,8,9),24]),mean(Output.model$q25$psi[c(11),c(1,2),24]),
                             mean(Output.model$q25$psi[c(12),c(1,2),24]),mean(Output.model$q25$psi[c(13),c(1,2),24]),mean(Output.model$q25$psi[c(14),c(1,2),24]),
                             mean(Output.model$q25$psi[c(15),c(1,2),24]),mean(Output.model$q25$psi[c(16),c(1,2),24]),mean(Output.model$q25$psi[c(17),c(1,2),24]),
                             mean(Output.model$q25$psi[c(18),c(1,2),24]),mean(Output.model$q25$psi[c(19),c(1,2),24]))),
                      mean(c(mean(Output.model$q25$psi[c(2),c(1,2,3,4,5,6,7,8,9),37]),mean(Output.model$q25$psi[c(3),c(1,2,3,4,5,6,7,8,9),37]),mean(Output.model$q25$psi[c(4),c(1,2,3,4,5,6,7,8,9),37]),
                             mean(Output.model$q25$psi[c(6),c(1,2,3,4,5,6,7,8,9),37]),mean(Output.model$q25$psi[c(7),c(1,2,3,4,5,6,7,8,9),37]),mean(Output.model$q25$psi[c(8),c(1,2,3,4,5,6,7,8,9),37]),
                             mean(Output.model$q25$psi[c(9),c(1,2,3,4,5,6,7,8,9),37]),mean(Output.model$q25$psi[c(10),c(1,2,3,4,5,6,7,8,9),37]),mean(Output.model$q25$psi[c(11),c(1,2),37]),
                             mean(Output.model$q25$psi[c(12),c(1,2),37]),mean(Output.model$q25$psi[c(13),c(1,2),37]),mean(Output.model$q25$psi[c(14),c(1,2),37]),
                             mean(Output.model$q25$psi[c(15),c(1,2),37]),mean(Output.model$q25$psi[c(16),c(1,2),37]),mean(Output.model$q25$psi[c(17),c(1,2),37]),
                             mean(Output.model$q25$psi[c(18),c(1,2),37]),mean(Output.model$q25$psi[c(19),c(1,2),37]))),
                      mean(c(mean(Output.model$q25$psi[c(2),c(1,2,3,4,5,6,7,8,9),13]),mean(Output.model$q25$psi[c(3),c(1,2,3,4,5,6,7,8,9),13]),mean(Output.model$q25$psi[c(4),c(1,2,3,4,5,6,7,8,9),13]),
                             mean(Output.model$q25$psi[c(6),c(1,2,3,4,5,6,7,8,9),13]),mean(Output.model$q25$psi[c(7),c(1,2,3,4,5,6,7,8,9),13]),mean(Output.model$q25$psi[c(8),c(1,2,3,4,5,6,7,8,9),13]),
                             mean(Output.model$q25$psi[c(9),c(1,2,3,4,5,6,7,8,9),13]),mean(Output.model$q25$psi[c(10),c(1,2,3,4,5,6,7,8,9),13]),mean(Output.model$q25$psi[c(11),c(1,2),13]),
                             mean(Output.model$q25$psi[c(12),c(1,2),13]),mean(Output.model$q25$psi[c(13),c(1,2),13]),mean(Output.model$q25$psi[c(14),c(1,2),13]),
                             mean(Output.model$q25$psi[c(15),c(1,2),13]),mean(Output.model$q25$psi[c(16),c(1,2),13]),mean(Output.model$q25$psi[c(17),c(1,2),13]),
                             mean(Output.model$q25$psi[c(18),c(1,2),13]),mean(Output.model$q25$psi[c(19),c(1,2),13]))),
                      mean(c(mean(Output.model$q25$psi[c(2),c(1,2,3,4,5,6,7,8,9),21]),mean(Output.model$q25$psi[c(3),c(1,2,3,4,5,6,7,8,9),21]),mean(Output.model$q25$psi[c(4),c(1,2,3,4,5,6,7,8,9),21]),
                             mean(Output.model$q25$psi[c(6),c(1,2,3,4,5,6,7,8,9),21]),mean(Output.model$q25$psi[c(7),c(1,2,3,4,5,6,7,8,9),21]),mean(Output.model$q25$psi[c(8),c(1,2,3,4,5,6,7,8,9),21]),
                             mean(Output.model$q25$psi[c(9),c(1,2,3,4,5,6,7,8,9),21]),mean(Output.model$q25$psi[c(10),c(1,2,3,4,5,6,7,8,9),21]),mean(Output.model$q25$psi[c(11),c(1,2),21]),
                             mean(Output.model$q25$psi[c(12),c(1,2),21]),mean(Output.model$q25$psi[c(13),c(1,2),21]),mean(Output.model$q25$psi[c(14),c(1,2),21]),
                             mean(Output.model$q25$psi[c(15),c(1,2),21]),mean(Output.model$q25$psi[c(16),c(1,2),21]),mean(Output.model$q25$psi[c(17),c(1,2),21]),
                             mean(Output.model$q25$psi[c(18),c(1,2),21]),mean(Output.model$q25$psi[c(19),c(1,2),21]))),
                      mean(c(mean(Output.model$q25$psi[c(2),c(1,2,3,4,5,6,7,8,9),34]),mean(Output.model$q25$psi[c(3),c(1,2,3,4,5,6,7,8,9),34]),mean(Output.model$q25$psi[c(4),c(1,2,3,4,5,6,7,8,9),34]),
                             mean(Output.model$q25$psi[c(6),c(1,2,3,4,5,6,7,8,9),34]),mean(Output.model$q25$psi[c(7),c(1,2,3,4,5,6,7,8,9),34]),mean(Output.model$q25$psi[c(8),c(1,2,3,4,5,6,7,8,9),34]),
                             mean(Output.model$q25$psi[c(9),c(1,2,3,4,5,6,7,8,9),34]),mean(Output.model$q25$psi[c(10),c(1,2,3,4,5,6,7,8,9),34]),mean(Output.model$q25$psi[c(11),c(1,2),34]),
                             mean(Output.model$q25$psi[c(12),c(1,2),34]),mean(Output.model$q25$psi[c(13),c(1,2),34]),mean(Output.model$q25$psi[c(14),c(1,2),34]),
                             mean(Output.model$q25$psi[c(15),c(1,2),34]),mean(Output.model$q25$psi[c(16),c(1,2),34]),mean(Output.model$q25$psi[c(17),c(1,2),34]),
                             mean(Output.model$q25$psi[c(18),c(1,2),34]),mean(Output.model$q25$psi[c(19),c(1,2),34]))),
                      mean(c(mean(Output.model$q25$psi[c(2),c(1,2,3,4,5,6,7,8,9),2]),mean(Output.model$q25$psi[c(3),c(1,2,3,4,5,6,7,8,9),2]),mean(Output.model$q25$psi[c(4),c(1,2,3,4,5,6,7,8,9),2]),
                             mean(Output.model$q25$psi[c(6),c(1,2,3,4,5,6,7,8,9),2]),mean(Output.model$q25$psi[c(7),c(1,2,3,4,5,6,7,8,9),2]),mean(Output.model$q25$psi[c(8),c(1,2,3,4,5,6,7,8,9),2]),
                             mean(Output.model$q25$psi[c(9),c(1,2,3,4,5,6,7,8,9),2]),mean(Output.model$q25$psi[c(10),c(1,2,3,4,5,6,7,8,9),2]),mean(Output.model$q25$psi[c(11),c(1,2),2]),
                             mean(Output.model$q25$psi[c(12),c(1,2),2]),mean(Output.model$q25$psi[c(13),c(1,2),2]),mean(Output.model$q25$psi[c(14),c(1,2),2]),
                             mean(Output.model$q25$psi[c(15),c(1,2),2]),mean(Output.model$q25$psi[c(16),c(1,2),2]),mean(Output.model$q25$psi[c(17),c(1,2),2]),
                             mean(Output.model$q25$psi[c(18),c(1,2),2]),mean(Output.model$q25$psi[c(19),c(1,2),2]))),
                      mean(c(mean(Output.model$q25$psi[c(2),c(1,2,3,4,5,6,7,8,9),32]),mean(Output.model$q25$psi[c(3),c(1,2,3,4,5,6,7,8,9),32]),mean(Output.model$q25$psi[c(4),c(1,2,3,4,5,6,7,8,9),32]),
                             mean(Output.model$q25$psi[c(6),c(1,2,3,4,5,6,7,8,9),32]),mean(Output.model$q25$psi[c(7),c(1,2,3,4,5,6,7,8,9),32]),mean(Output.model$q25$psi[c(8),c(1,2,3,4,5,6,7,8,9),32]),
                             mean(Output.model$q25$psi[c(9),c(1,2,3,4,5,6,7,8,9),32]),mean(Output.model$q25$psi[c(10),c(1,2,3,4,5,6,7,8,9),32]),mean(Output.model$q25$psi[c(11),c(1,2),32]),
                             mean(Output.model$q25$psi[c(12),c(1,2),32]),mean(Output.model$q25$psi[c(13),c(1,2),32]),mean(Output.model$q25$psi[c(14),c(1,2),32]),
                             mean(Output.model$q25$psi[c(15),c(1,2),32]),mean(Output.model$q25$psi[c(16),c(1,2),32]),mean(Output.model$q25$psi[c(17),c(1,2),32]),
                             mean(Output.model$q25$psi[c(18),c(1,2),32]),mean(Output.model$q25$psi[c(19),c(1,2),32]))),
                      mean(c(mean(Output.model$q25$psi[c(2),c(1,2,3,4,5,6,7,8,9),17]),mean(Output.model$q25$psi[c(3),c(1,2,3,4,5,6,7,8,9),17]),mean(Output.model$q25$psi[c(4),c(1,2,3,4,5,6,7,8,9),17]),
                             mean(Output.model$q25$psi[c(6),c(1,2,3,4,5,6,7,8,9),17]),mean(Output.model$q25$psi[c(7),c(1,2,3,4,5,6,7,8,9),17]),mean(Output.model$q25$psi[c(8),c(1,2,3,4,5,6,7,8,9),17]),
                             mean(Output.model$q25$psi[c(9),c(1,2,3,4,5,6,7,8,9),17]),mean(Output.model$q25$psi[c(10),c(1,2,3,4,5,6,7,8,9),17]),mean(Output.model$q25$psi[c(11),c(1,2),17]),
                             mean(Output.model$q25$psi[c(12),c(1,2),17]),mean(Output.model$q25$psi[c(13),c(1,2),17]),mean(Output.model$q25$psi[c(14),c(1,2),17]),
                             mean(Output.model$q25$psi[c(15),c(1,2),17]),mean(Output.model$q25$psi[c(16),c(1,2),17]),mean(Output.model$q25$psi[c(17),c(1,2),17]),
                             mean(Output.model$q25$psi[c(18),c(1,2),17]),mean(Output.model$q25$psi[c(19),c(1,2),17]))),
                      mean(c(mean(Output.model$q25$psi[c(2),c(1,2,3,4,5,6,7,8,9),4]),mean(Output.model$q25$psi[c(3),c(1,2,3,4,5,6,7,8,9),4]),mean(Output.model$q25$psi[c(4),c(1,2,3,4,5,6,7,8,9),4]),
                             mean(Output.model$q25$psi[c(6),c(1,2,3,4,5,6,7,8,9),4]),mean(Output.model$q25$psi[c(7),c(1,2,3,4,5,6,7,8,9),4]),mean(Output.model$q25$psi[c(8),c(1,2,3,4,5,6,7,8,9),4]),
                             mean(Output.model$q25$psi[c(9),c(1,2,3,4,5,6,7,8,9),4]),mean(Output.model$q25$psi[c(10),c(1,2,3,4,5,6,7,8,9),4]),mean(Output.model$q25$psi[c(11),c(1,2),4]),
                             mean(Output.model$q25$psi[c(12),c(1,2),4]),mean(Output.model$q25$psi[c(13),c(1,2),4]),mean(Output.model$q25$psi[c(14),c(1,2),4]),
                             mean(Output.model$q25$psi[c(15),c(1,2),4]),mean(Output.model$q25$psi[c(16),c(1,2),4]),mean(Output.model$q25$psi[c(17),c(1,2),4]),
                             mean(Output.model$q25$psi[c(18),c(1,2),4]),mean(Output.model$q25$psi[c(19),c(1,2),4]))),
                      mean(c(mean(Output.model$q25$psi[c(2),c(1,2,3,4,5,6,7,8,9),8]),mean(Output.model$q25$psi[c(3),c(1,2,3,4,5,6,7,8,9),8]),mean(Output.model$q25$psi[c(4),c(1,2,3,4,5,6,7,8,9),8]),
                             mean(Output.model$q25$psi[c(6),c(1,2,3,4,5,6,7,8,9),8]),mean(Output.model$q25$psi[c(7),c(1,2,3,4,5,6,7,8,9),8]),mean(Output.model$q25$psi[c(8),c(1,2,3,4,5,6,7,8,9),8]),
                             mean(Output.model$q25$psi[c(9),c(1,2,3,4,5,6,7,8,9),8]),mean(Output.model$q25$psi[c(10),c(1,2,3,4,5,6,7,8,9),8]),mean(Output.model$q25$psi[c(11),c(1,2),8]),
                             mean(Output.model$q25$psi[c(12),c(1,2),8]),mean(Output.model$q25$psi[c(13),c(1,2),8]),mean(Output.model$q25$psi[c(14),c(1,2),8]),
                             mean(Output.model$q25$psi[c(15),c(1,2),8]),mean(Output.model$q25$psi[c(16),c(1,2),8]),mean(Output.model$q25$psi[c(17),c(1,2),8]),
                             mean(Output.model$q25$psi[c(18),c(1,2),8]),mean(Output.model$q25$psi[c(19),c(1,2),8]))),
                      mean(c(mean(Output.model$q25$psi[c(2),c(1,2,3,4,5,6,7,8,9),39]),mean(Output.model$q25$psi[c(3),c(1,2,3,4,5,6,7,8,9),39]),mean(Output.model$q25$psi[c(4),c(1,2,3,4,5,6,7,8,9),39]),
                             mean(Output.model$q25$psi[c(6),c(1,2,3,4,5,6,7,8,9),39]),mean(Output.model$q25$psi[c(7),c(1,2,3,4,5,6,7,8,9),39]),mean(Output.model$q25$psi[c(8),c(1,2,3,4,5,6,7,8,9),39]),
                             mean(Output.model$q25$psi[c(9),c(1,2,3,4,5,6,7,8,9),39]),mean(Output.model$q25$psi[c(10),c(1,2,3,4,5,6,7,8,9),39]),mean(Output.model$q25$psi[c(11),c(1,2),39]),
                             mean(Output.model$q25$psi[c(12),c(1,2),39]),mean(Output.model$q25$psi[c(13),c(1,2),39]),mean(Output.model$q25$psi[c(14),c(1,2),39]),
                             mean(Output.model$q25$psi[c(15),c(1,2),39]),mean(Output.model$q25$psi[c(16),c(1,2),39]),mean(Output.model$q25$psi[c(17),c(1,2),39]),
                             mean(Output.model$q25$psi[c(18),c(1,2),39]),mean(Output.model$q25$psi[c(19),c(1,2),39]))),
                      mean(c(mean(Output.model$q25$psi[c(2),c(1,2,3,4,5,6,7,8,9),10]),mean(Output.model$q25$psi[c(3),c(1,2,3,4,5,6,7,8,9),10]),mean(Output.model$q25$psi[c(4),c(1,2,3,4,5,6,7,8,9),10]),
                             mean(Output.model$q25$psi[c(6),c(1,2,3,4,5,6,7,8,9),10]),mean(Output.model$q25$psi[c(7),c(1,2,3,4,5,6,7,8,9),10]),mean(Output.model$q25$psi[c(8),c(1,2,3,4,5,6,7,8,9),10]),
                             mean(Output.model$q25$psi[c(9),c(1,2,3,4,5,6,7,8,9),10]),mean(Output.model$q25$psi[c(10),c(1,2,3,4,5,6,7,8,9),10]),mean(Output.model$q25$psi[c(11),c(1,2),10]),
                             mean(Output.model$q25$psi[c(12),c(1,2),10]),mean(Output.model$q25$psi[c(13),c(1,2),10]),mean(Output.model$q25$psi[c(14),c(1,2),10]),
                             mean(Output.model$q25$psi[c(15),c(1,2),10]),mean(Output.model$q25$psi[c(16),c(1,2),10]),mean(Output.model$q25$psi[c(17),c(1,2),10]),
                             mean(Output.model$q25$psi[c(18),c(1,2),10]),mean(Output.model$q25$psi[c(19),c(1,2),10]))),
                      mean(c(mean(Output.model$q25$psi[c(2),c(1,2,3,4,5,6,7,8,9),19]),mean(Output.model$q25$psi[c(3),c(1,2,3,4,5,6,7,8,9),19]),mean(Output.model$q25$psi[c(4),c(1,2,3,4,5,6,7,8,9),19]),
                             mean(Output.model$q25$psi[c(6),c(1,2,3,4,5,6,7,8,9),19]),mean(Output.model$q25$psi[c(7),c(1,2,3,4,5,6,7,8,9),19]),mean(Output.model$q25$psi[c(8),c(1,2,3,4,5,6,7,8,9),19]),
                             mean(Output.model$q25$psi[c(9),c(1,2,3,4,5,6,7,8,9),19]),mean(Output.model$q25$psi[c(10),c(1,2,3,4,5,6,7,8,9),19]),mean(Output.model$q25$psi[c(11),c(1,2),19]),
                             mean(Output.model$q25$psi[c(12),c(1,2),19]),mean(Output.model$q25$psi[c(13),c(1,2),19]),mean(Output.model$q25$psi[c(14),c(1,2),19]),
                             mean(Output.model$q25$psi[c(15),c(1,2),19]),mean(Output.model$q25$psi[c(16),c(1,2),19]),mean(Output.model$q25$psi[c(17),c(1,2),19]),
                             mean(Output.model$q25$psi[c(18),c(1,2),19]),mean(Output.model$q25$psi[c(19),c(1,2),19]))),
                      mean(c(mean(Output.model$q25$psi[c(2),c(1,2,3,4,5,6,7,8,9),35]),mean(Output.model$q25$psi[c(3),c(1,2,3,4,5,6,7,8,9),35]),mean(Output.model$q25$psi[c(4),c(1,2,3,4,5,6,7,8,9),35]),
                             mean(Output.model$q25$psi[c(6),c(1,2,3,4,5,6,7,8,9),35]),mean(Output.model$q25$psi[c(7),c(1,2,3,4,5,6,7,8,9),35]),mean(Output.model$q25$psi[c(8),c(1,2,3,4,5,6,7,8,9),35]),
                             mean(Output.model$q25$psi[c(9),c(1,2,3,4,5,6,7,8,9),35]),mean(Output.model$q25$psi[c(10),c(1,2,3,4,5,6,7,8,9),35]),mean(Output.model$q25$psi[c(11),c(1,2),35]),
                             mean(Output.model$q25$psi[c(12),c(1,2),35]),mean(Output.model$q25$psi[c(13),c(1,2),35]),mean(Output.model$q25$psi[c(14),c(1,2),35]),
                             mean(Output.model$q25$psi[c(15),c(1,2),35]),mean(Output.model$q25$psi[c(16),c(1,2),35]),mean(Output.model$q25$psi[c(17),c(1,2),35]),
                             mean(Output.model$q25$psi[c(18),c(1,2),35]),mean(Output.model$q25$psi[c(19),c(1,2),35]))),
                      mean(c(mean(Output.model$q25$psi[c(2),c(1,2,3,4,5,6,7,8,9),25]),mean(Output.model$q25$psi[c(3),c(1,2,3,4,5,6,7,8,9),25]),mean(Output.model$q25$psi[c(4),c(1,2,3,4,5,6,7,8,9),25]),
                             mean(Output.model$q25$psi[c(6),c(1,2,3,4,5,6,7,8,9),25]),mean(Output.model$q25$psi[c(7),c(1,2,3,4,5,6,7,8,9),25]),mean(Output.model$q25$psi[c(8),c(1,2,3,4,5,6,7,8,9),25]),
                             mean(Output.model$q25$psi[c(9),c(1,2,3,4,5,6,7,8,9),25]),mean(Output.model$q25$psi[c(10),c(1,2,3,4,5,6,7,8,9),25]),mean(Output.model$q25$psi[c(11),c(1,2),25]),
                             mean(Output.model$q25$psi[c(12),c(1,2),25]),mean(Output.model$q25$psi[c(13),c(1,2),25]),mean(Output.model$q25$psi[c(14),c(1,2),25]),
                             mean(Output.model$q25$psi[c(15),c(1,2),25]),mean(Output.model$q25$psi[c(16),c(1,2),25]),mean(Output.model$q25$psi[c(17),c(1,2),25]),
                             mean(Output.model$q25$psi[c(18),c(1,2),25]),mean(Output.model$q25$psi[c(19),c(1,2),25]))),
                      mean(c(mean(Output.model$q25$psi[c(2),c(1,2,3,4,5,6,7,8,9),28]),mean(Output.model$q25$psi[c(3),c(1,2,3,4,5,6,7,8,9),28]),mean(Output.model$q25$psi[c(4),c(1,2,3,4,5,6,7,8,9),28]),
                             mean(Output.model$q25$psi[c(6),c(1,2,3,4,5,6,7,8,9),28]),mean(Output.model$q25$psi[c(7),c(1,2,3,4,5,6,7,8,9),28]),mean(Output.model$q25$psi[c(8),c(1,2,3,4,5,6,7,8,9),28]),
                             mean(Output.model$q25$psi[c(9),c(1,2,3,4,5,6,7,8,9),28]),mean(Output.model$q25$psi[c(10),c(1,2,3,4,5,6,7,8,9),28]),mean(Output.model$q25$psi[c(11),c(1,2),28]),
                             mean(Output.model$q25$psi[c(12),c(1,2),28]),mean(Output.model$q25$psi[c(13),c(1,2),28]),mean(Output.model$q25$psi[c(14),c(1,2),28]),
                             mean(Output.model$q25$psi[c(15),c(1,2),28]),mean(Output.model$q25$psi[c(16),c(1,2),28]),mean(Output.model$q25$psi[c(17),c(1,2),28]),
                             mean(Output.model$q25$psi[c(18),c(1,2),28]),mean(Output.model$q25$psi[c(19),c(1,2),28]))),
                      mean(c(mean(Output.model$q25$psi[c(2),c(1,2,3,4,5,6,7,8,9),46]),mean(Output.model$q25$psi[c(3),c(1,2,3,4,5,6,7,8,9),46]),mean(Output.model$q25$psi[c(4),c(1,2,3,4,5,6,7,8,9),46]),
                             mean(Output.model$q25$psi[c(6),c(1,2,3,4,5,6,7,8,9),46]),mean(Output.model$q25$psi[c(7),c(1,2,3,4,5,6,7,8,9),46]),mean(Output.model$q25$psi[c(8),c(1,2,3,4,5,6,7,8,9),46]),
                             mean(Output.model$q25$psi[c(9),c(1,2,3,4,5,6,7,8,9),46]),mean(Output.model$q25$psi[c(10),c(1,2,3,4,5,6,7,8,9),46]),mean(Output.model$q25$psi[c(11),c(1,2),46]),
                             mean(Output.model$q25$psi[c(12),c(1,2),46]),mean(Output.model$q25$psi[c(13),c(1,2),46]),mean(Output.model$q25$psi[c(14),c(1,2),46]),
                             mean(Output.model$q25$psi[c(15),c(1,2),46]),mean(Output.model$q25$psi[c(16),c(1,2),46]),mean(Output.model$q25$psi[c(17),c(1,2),46]),
                             mean(Output.model$q25$psi[c(18),c(1,2),46]),mean(Output.model$q25$psi[c(19),c(1,2),46]))),
                      mean(c(mean(Output.model$q25$psi[c(2),c(1,2,3,4,5,6,7,8,9),41]),mean(Output.model$q25$psi[c(3),c(1,2,3,4,5,6,7,8,9),41]),mean(Output.model$q25$psi[c(4),c(1,2,3,4,5,6,7,8,9),41]),
                             mean(Output.model$q25$psi[c(6),c(1,2,3,4,5,6,7,8,9),41]),mean(Output.model$q25$psi[c(7),c(1,2,3,4,5,6,7,8,9),41]),mean(Output.model$q25$psi[c(8),c(1,2,3,4,5,6,7,8,9),41]),
                             mean(Output.model$q25$psi[c(9),c(1,2,3,4,5,6,7,8,9),41]),mean(Output.model$q25$psi[c(10),c(1,2,3,4,5,6,7,8,9),41]),mean(Output.model$q25$psi[c(11),c(1,2),41]),
                             mean(Output.model$q25$psi[c(12),c(1,2),41]),mean(Output.model$q25$psi[c(13),c(1,2),41]),mean(Output.model$q25$psi[c(14),c(1,2),41]),
                             mean(Output.model$q25$psi[c(15),c(1,2),41]),mean(Output.model$q25$psi[c(16),c(1,2),41]),mean(Output.model$q25$psi[c(17),c(1,2),41]),
                             mean(Output.model$q25$psi[c(18),c(1,2),41]),mean(Output.model$q25$psi[c(19),c(1,2),41]))),
                      mean(c(mean(Output.model$q25$psi[c(2),c(1,2,3,4,5,6,7,8,9),16]),mean(Output.model$q25$psi[c(3),c(1,2,3,4,5,6,7,8,9),16]),mean(Output.model$q25$psi[c(4),c(1,2,3,4,5,6,7,8,9),16]),
                             mean(Output.model$q25$psi[c(6),c(1,2,3,4,5,6,7,8,9),16]),mean(Output.model$q25$psi[c(7),c(1,2,3,4,5,6,7,8,9),16]),mean(Output.model$q25$psi[c(8),c(1,2,3,4,5,6,7,8,9),16]),
                             mean(Output.model$q25$psi[c(9),c(1,2,3,4,5,6,7,8,9),16]),mean(Output.model$q25$psi[c(10),c(1,2,3,4,5,6,7,8,9),16]),mean(Output.model$q25$psi[c(11),c(1,2),16]),
                             mean(Output.model$q25$psi[c(12),c(1,2),16]),mean(Output.model$q25$psi[c(13),c(1,2),16]),mean(Output.model$q25$psi[c(14),c(1,2),16]),
                             mean(Output.model$q25$psi[c(15),c(1,2),16]),mean(Output.model$q25$psi[c(16),c(1,2),16]),mean(Output.model$q25$psi[c(17),c(1,2),16]),
                             mean(Output.model$q25$psi[c(18),c(1,2),16]),mean(Output.model$q25$psi[c(19),c(1,2),16]))),
                      mean(c(mean(Output.model$q25$psi[c(2),c(1,2,3,4,5,6,7,8,9),36]),mean(Output.model$q25$psi[c(3),c(1,2,3,4,5,6,7,8,9),36]),mean(Output.model$q25$psi[c(4),c(1,2,3,4,5,6,7,8,9),36]),
                             mean(Output.model$q25$psi[c(6),c(1,2,3,4,5,6,7,8,9),36]),mean(Output.model$q25$psi[c(7),c(1,2,3,4,5,6,7,8,9),36]),mean(Output.model$q25$psi[c(8),c(1,2,3,4,5,6,7,8,9),36]),
                             mean(Output.model$q25$psi[c(9),c(1,2,3,4,5,6,7,8,9),36]),mean(Output.model$q25$psi[c(10),c(1,2,3,4,5,6,7,8,9),36]),mean(Output.model$q25$psi[c(11),c(1,2),36]),
                             mean(Output.model$q25$psi[c(12),c(1,2),36]),mean(Output.model$q25$psi[c(13),c(1,2),36]),mean(Output.model$q25$psi[c(14),c(1,2),36]),
                             mean(Output.model$q25$psi[c(15),c(1,2),36]),mean(Output.model$q25$psi[c(16),c(1,2),36]),mean(Output.model$q25$psi[c(17),c(1,2),36]),
                             mean(Output.model$q25$psi[c(18),c(1,2),36]),mean(Output.model$q25$psi[c(19),c(1,2),36]))),
                      mean(c(mean(Output.model$q25$psi[c(2),c(1,2,3,4,5,6,7,8,9),14]),mean(Output.model$q25$psi[c(3),c(1,2,3,4,5,6,7,8,9),14]),mean(Output.model$q25$psi[c(4),c(1,2,3,4,5,6,7,8,9),14]),
                             mean(Output.model$q25$psi[c(6),c(1,2,3,4,5,6,7,8,9),14]),mean(Output.model$q25$psi[c(7),c(1,2,3,4,5,6,7,8,9),14]),mean(Output.model$q25$psi[c(8),c(1,2,3,4,5,6,7,8,9),14]),
                             mean(Output.model$q25$psi[c(9),c(1,2,3,4,5,6,7,8,9),14]),mean(Output.model$q25$psi[c(10),c(1,2,3,4,5,6,7,8,9),14]),mean(Output.model$q25$psi[c(11),c(1,2),14]),
                             mean(Output.model$q25$psi[c(12),c(1,2),14]),mean(Output.model$q25$psi[c(13),c(1,2),14]),mean(Output.model$q25$psi[c(14),c(1,2),14]),
                             mean(Output.model$q25$psi[c(15),c(1,2),14]),mean(Output.model$q25$psi[c(16),c(1,2),14]),mean(Output.model$q25$psi[c(17),c(1,2),14]),
                             mean(Output.model$q25$psi[c(18),c(1,2),14]),mean(Output.model$q25$psi[c(19),c(1,2),14]))),
                      mean(c(mean(Output.model$q25$psi[c(2),c(1,2,3,4,5,6,7,8,9),11]),mean(Output.model$q25$psi[c(3),c(1,2,3,4,5,6,7,8,9),11]),mean(Output.model$q25$psi[c(4),c(1,2,3,4,5,6,7,8,9),11]),
                             mean(Output.model$q25$psi[c(6),c(1,2,3,4,5,6,7,8,9),11]),mean(Output.model$q25$psi[c(7),c(1,2,3,4,5,6,7,8,9),11]),mean(Output.model$q25$psi[c(8),c(1,2,3,4,5,6,7,8,9),11]),
                             mean(Output.model$q25$psi[c(9),c(1,2,3,4,5,6,7,8,9),11]),mean(Output.model$q25$psi[c(10),c(1,2,3,4,5,6,7,8,9),11]),mean(Output.model$q25$psi[c(11),c(1,2),11]),
                             mean(Output.model$q25$psi[c(12),c(1,2),11]),mean(Output.model$q25$psi[c(13),c(1,2),11]),mean(Output.model$q25$psi[c(14),c(1,2),11]),
                             mean(Output.model$q25$psi[c(15),c(1,2),11]),mean(Output.model$q25$psi[c(16),c(1,2),11]),mean(Output.model$q25$psi[c(17),c(1,2),11]),
                             mean(Output.model$q25$psi[c(18),c(1,2),11]),mean(Output.model$q25$psi[c(19),c(1,2),11]))),
                      mean(c(mean(Output.model$q25$psi[c(2),c(1,2,3,4,5,6,7,8,9),31]),mean(Output.model$q25$psi[c(3),c(1,2,3,4,5,6,7,8,9),31]),mean(Output.model$q25$psi[c(4),c(1,2,3,4,5,6,7,8,9),31]),
                             mean(Output.model$q25$psi[c(6),c(1,2,3,4,5,6,7,8,9),31]),mean(Output.model$q25$psi[c(7),c(1,2,3,4,5,6,7,8,9),31]),mean(Output.model$q25$psi[c(8),c(1,2,3,4,5,6,7,8,9),31]),
                             mean(Output.model$q25$psi[c(9),c(1,2,3,4,5,6,7,8,9),31]),mean(Output.model$q25$psi[c(10),c(1,2,3,4,5,6,7,8,9),31]),mean(Output.model$q25$psi[c(11),c(1,2),31]),
                             mean(Output.model$q25$psi[c(12),c(1,2),31]),mean(Output.model$q25$psi[c(13),c(1,2),31]),mean(Output.model$q25$psi[c(14),c(1,2),31]),
                             mean(Output.model$q25$psi[c(15),c(1,2),31]),mean(Output.model$q25$psi[c(16),c(1,2),31]),mean(Output.model$q25$psi[c(17),c(1,2),31]),
                             mean(Output.model$q25$psi[c(18),c(1,2),31]),mean(Output.model$q25$psi[c(19),c(1,2),31]))),
                      mean(c(mean(Output.model$q25$psi[c(2),c(1,2,3,4,5,6,7,8,9),6]),mean(Output.model$q25$psi[c(3),c(1,2,3,4,5,6,7,8,9),6]),mean(Output.model$q25$psi[c(4),c(1,2,3,4,5,6,7,8,9),6]),
                             mean(Output.model$q25$psi[c(6),c(1,2,3,4,5,6,7,8,9),6]),mean(Output.model$q25$psi[c(7),c(1,2,3,4,5,6,7,8,9),6]),mean(Output.model$q25$psi[c(8),c(1,2,3,4,5,6,7,8,9),6]),
                             mean(Output.model$q25$psi[c(9),c(1,2,3,4,5,6,7,8,9),6]),mean(Output.model$q25$psi[c(10),c(1,2,3,4,5,6,7,8,9),6]),mean(Output.model$q25$psi[c(11),c(1,2),6]),
                             mean(Output.model$q25$psi[c(12),c(1,2),6]),mean(Output.model$q25$psi[c(13),c(1,2),6]),mean(Output.model$q25$psi[c(14),c(1,2),6]),
                             mean(Output.model$q25$psi[c(15),c(1,2),6]),mean(Output.model$q25$psi[c(16),c(1,2),6]),mean(Output.model$q25$psi[c(17),c(1,2),6]),
                             mean(Output.model$q25$psi[c(18),c(1,2),6]),mean(Output.model$q25$psi[c(19),c(1,2),6])))),
                    c(mean(c(mean(Output.model$mean$psi[c(2),c(1,2,3,4,5,6,7,8,9),45]),mean(Output.model$mean$psi[c(3),c(1,2,3,4,5,6,7,8,9),45]),mean(Output.model$mean$psi[c(4),c(1,2,3,4,5,6,7,8,9),45]),
                             mean(Output.model$mean$psi[c(6),c(1,2,3,4,5,6,7,8,9),45]),mean(Output.model$mean$psi[c(7),c(1,2,3,4,5,6,7,8,9),45]),mean(Output.model$mean$psi[c(8),c(1,2,3,4,5,6,7,8,9),45]),
                             mean(Output.model$mean$psi[c(9),c(1,2,3,4,5,6,7,8,9),45]),mean(Output.model$mean$psi[c(10),c(1,2,3,4,5,6,7,8,9),45]),mean(Output.model$mean$psi[c(11),c(1,2),45]),
                             mean(Output.model$mean$psi[c(12),c(1,2),45]),mean(Output.model$mean$psi[c(13),c(1,2),45]),mean(Output.model$mean$psi[c(14),c(1,2),45]),
                             mean(Output.model$mean$psi[c(15),c(1,2),45]),mean(Output.model$mean$psi[c(16),c(1,2),45]),mean(Output.model$mean$psi[c(17),c(1,2),45]),
                             mean(Output.model$mean$psi[c(18),c(1,2),45]),mean(Output.model$mean$psi[c(19),c(1,2),45]))),
                      mean(c(mean(Output.model$mean$psi[c(2),c(1,2,3,4,5,6,7,8,9),1]),mean(Output.model$mean$psi[c(3),c(1,2,3,4,5,6,7,8,9),1]),mean(Output.model$mean$psi[c(4),c(1,2,3,4,5,6,7,8,9),1]),
                             mean(Output.model$mean$psi[c(6),c(1,2,3,4,5,6,7,8,9),1]),mean(Output.model$mean$psi[c(7),c(1,2,3,4,5,6,7,8,9),1]),mean(Output.model$mean$psi[c(8),c(1,2,3,4,5,6,7,8,9),1]),
                             mean(Output.model$mean$psi[c(9),c(1,2,3,4,5,6,7,8,9),1]),mean(Output.model$mean$psi[c(10),c(1,2,3,4,5,6,7,8,9),1]),mean(Output.model$mean$psi[c(11),c(1,2),1]),
                             mean(Output.model$mean$psi[c(12),c(1,2),1]),mean(Output.model$mean$psi[c(13),c(1,2),1]),mean(Output.model$mean$psi[c(14),c(1,2),1]),
                             mean(Output.model$mean$psi[c(15),c(1,2),1]),mean(Output.model$mean$psi[c(16),c(1,2),1]),mean(Output.model$mean$psi[c(17),c(1,2),1]),
                             mean(Output.model$mean$psi[c(18),c(1,2),1]),mean(Output.model$mean$psi[c(19),c(1,2),1]))),
                      mean(c(mean(Output.model$mean$psi[c(2),c(1,2,3,4,5,6,7,8,9),15]),mean(Output.model$mean$psi[c(3),c(1,2,3,4,5,6,7,8,9),15]),mean(Output.model$mean$psi[c(4),c(1,2,3,4,5,6,7,8,9),15]),
                             mean(Output.model$mean$psi[c(6),c(1,2,3,4,5,6,7,8,9),15]),mean(Output.model$mean$psi[c(7),c(1,2,3,4,5,6,7,8,9),15]),mean(Output.model$mean$psi[c(8),c(1,2,3,4,5,6,7,8,9),15]),
                             mean(Output.model$mean$psi[c(9),c(1,2,3,4,5,6,7,8,9),15]),mean(Output.model$mean$psi[c(10),c(1,2,3,4,5,6,7,8,9),15]),mean(Output.model$mean$psi[c(11),c(1,2),15]),
                             mean(Output.model$mean$psi[c(12),c(1,2),15]),mean(Output.model$mean$psi[c(13),c(1,2),15]),mean(Output.model$mean$psi[c(14),c(1,2),15]),
                             mean(Output.model$mean$psi[c(15),c(1,2),15]),mean(Output.model$mean$psi[c(16),c(1,2),15]),mean(Output.model$mean$psi[c(17),c(1,2),15]),
                             mean(Output.model$mean$psi[c(18),c(1,2),15]),mean(Output.model$mean$psi[c(19),c(1,2),15]))),
                      mean(c(mean(Output.model$mean$psi[c(2),c(1,2,3,4,5,6,7,8,9),7]),mean(Output.model$mean$psi[c(3),c(1,2,3,4,5,6,7,8,9),7]),mean(Output.model$mean$psi[c(4),c(1,2,3,4,5,6,7,8,9),7]),
                             mean(Output.model$mean$psi[c(6),c(1,2,3,4,5,6,7,8,9),7]),mean(Output.model$mean$psi[c(7),c(1,2,3,4,5,6,7,8,9),7]),mean(Output.model$mean$psi[c(8),c(1,2,3,4,5,6,7,8,9),7]),
                             mean(Output.model$mean$psi[c(9),c(1,2,3,4,5,6,7,8,9),7]),mean(Output.model$mean$psi[c(10),c(1,2,3,4,5,6,7,8,9),7]),mean(Output.model$mean$psi[c(11),c(1,2),7]),
                             mean(Output.model$mean$psi[c(12),c(1,2),7]),mean(Output.model$mean$psi[c(13),c(1,2),7]),mean(Output.model$mean$psi[c(14),c(1,2),7]),
                             mean(Output.model$mean$psi[c(15),c(1,2),7]),mean(Output.model$mean$psi[c(16),c(1,2),7]),mean(Output.model$mean$psi[c(17),c(1,2),7]),
                             mean(Output.model$mean$psi[c(18),c(1,2),7]),mean(Output.model$mean$psi[c(19),c(1,2),7]))),
                      mean(c(mean(Output.model$mean$psi[c(2),c(1,2,3,4,5,6,7,8,9),22]),mean(Output.model$mean$psi[c(3),c(1,2,3,4,5,6,7,8,9),22]),mean(Output.model$mean$psi[c(4),c(1,2,3,4,5,6,7,8,9),22]),
                             mean(Output.model$mean$psi[c(6),c(1,2,3,4,5,6,7,8,9),22]),mean(Output.model$mean$psi[c(7),c(1,2,3,4,5,6,7,8,9),22]),mean(Output.model$mean$psi[c(8),c(1,2,3,4,5,6,7,8,9),22]),
                             mean(Output.model$mean$psi[c(9),c(1,2,3,4,5,6,7,8,9),22]),mean(Output.model$mean$psi[c(10),c(1,2,3,4,5,6,7,8,9),22]),mean(Output.model$mean$psi[c(11),c(1,2),22]),
                             mean(Output.model$mean$psi[c(12),c(1,2),22]),mean(Output.model$mean$psi[c(13),c(1,2),22]),mean(Output.model$mean$psi[c(14),c(1,2),22]),
                             mean(Output.model$mean$psi[c(15),c(1,2),22]),mean(Output.model$mean$psi[c(16),c(1,2),22]),mean(Output.model$mean$psi[c(17),c(1,2),22]),
                             mean(Output.model$mean$psi[c(18),c(1,2),22]),mean(Output.model$mean$psi[c(19),c(1,2),22]))),
                      mean(c(mean(Output.model$mean$psi[c(2),c(1,2,3,4,5,6,7,8,9),29]),mean(Output.model$mean$psi[c(3),c(1,2,3,4,5,6,7,8,9),29]),mean(Output.model$mean$psi[c(4),c(1,2,3,4,5,6,7,8,9),29]),
                             mean(Output.model$mean$psi[c(6),c(1,2,3,4,5,6,7,8,9),29]),mean(Output.model$mean$psi[c(7),c(1,2,3,4,5,6,7,8,9),29]),mean(Output.model$mean$psi[c(8),c(1,2,3,4,5,6,7,8,9),29]),
                             mean(Output.model$mean$psi[c(9),c(1,2,3,4,5,6,7,8,9),29]),mean(Output.model$mean$psi[c(10),c(1,2,3,4,5,6,7,8,9),29]),mean(Output.model$mean$psi[c(11),c(1,2),29]),
                             mean(Output.model$mean$psi[c(12),c(1,2),29]),mean(Output.model$mean$psi[c(13),c(1,2),29]),mean(Output.model$mean$psi[c(14),c(1,2),29]),
                             mean(Output.model$mean$psi[c(15),c(1,2),29]),mean(Output.model$mean$psi[c(16),c(1,2),29]),mean(Output.model$mean$psi[c(17),c(1,2),29]),
                             mean(Output.model$mean$psi[c(18),c(1,2),29]),mean(Output.model$mean$psi[c(19),c(1,2),29]))),
                      mean(c(mean(Output.model$mean$psi[c(2),c(1,2,3,4,5,6,7,8,9),18]),mean(Output.model$mean$psi[c(3),c(1,2,3,4,5,6,7,8,9),18]),mean(Output.model$mean$psi[c(4),c(1,2,3,4,5,6,7,8,9),18]),
                             mean(Output.model$mean$psi[c(6),c(1,2,3,4,5,6,7,8,9),18]),mean(Output.model$mean$psi[c(7),c(1,2,3,4,5,6,7,8,9),18]),mean(Output.model$mean$psi[c(8),c(1,2,3,4,5,6,7,8,9),18]),
                             mean(Output.model$mean$psi[c(9),c(1,2,3,4,5,6,7,8,9),18]),mean(Output.model$mean$psi[c(10),c(1,2,3,4,5,6,7,8,9),18]),mean(Output.model$mean$psi[c(11),c(1,2),18]),
                             mean(Output.model$mean$psi[c(12),c(1,2),18]),mean(Output.model$mean$psi[c(13),c(1,2),18]),mean(Output.model$mean$psi[c(14),c(1,2),18]),
                             mean(Output.model$mean$psi[c(15),c(1,2),18]),mean(Output.model$mean$psi[c(16),c(1,2),18]),mean(Output.model$mean$psi[c(17),c(1,2),18]),
                             mean(Output.model$mean$psi[c(18),c(1,2),18]),mean(Output.model$mean$psi[c(19),c(1,2),18]))),
                      mean(c(mean(Output.model$mean$psi[c(2),c(1,2,3,4,5,6,7,8,9),33]),mean(Output.model$mean$psi[c(3),c(1,2,3,4,5,6,7,8,9),33]),mean(Output.model$mean$psi[c(4),c(1,2,3,4,5,6,7,8,9),33]),
                             mean(Output.model$mean$psi[c(6),c(1,2,3,4,5,6,7,8,9),33]),mean(Output.model$mean$psi[c(7),c(1,2,3,4,5,6,7,8,9),33]),mean(Output.model$mean$psi[c(8),c(1,2,3,4,5,6,7,8,9),33]),
                             mean(Output.model$mean$psi[c(9),c(1,2,3,4,5,6,7,8,9),33]),mean(Output.model$mean$psi[c(10),c(1,2,3,4,5,6,7,8,9),33]),mean(Output.model$mean$psi[c(11),c(1,2),33]),
                             mean(Output.model$mean$psi[c(12),c(1,2),33]),mean(Output.model$mean$psi[c(13),c(1,2),33]),mean(Output.model$mean$psi[c(14),c(1,2),33]),
                             mean(Output.model$mean$psi[c(15),c(1,2),33]),mean(Output.model$mean$psi[c(16),c(1,2),33]),mean(Output.model$mean$psi[c(17),c(1,2),33]),
                             mean(Output.model$mean$psi[c(18),c(1,2),33]),mean(Output.model$mean$psi[c(19),c(1,2),33]))),
                      mean(c(mean(Output.model$mean$psi[c(2),c(1,2,3,4,5,6,7,8,9),12]),mean(Output.model$mean$psi[c(3),c(1,2,3,4,5,6,7,8,9),12]),mean(Output.model$mean$psi[c(4),c(1,2,3,4,5,6,7,8,9),12]),
                             mean(Output.model$mean$psi[c(6),c(1,2,3,4,5,6,7,8,9),12]),mean(Output.model$mean$psi[c(7),c(1,2,3,4,5,6,7,8,9),12]),mean(Output.model$mean$psi[c(8),c(1,2,3,4,5,6,7,8,9),12]),
                             mean(Output.model$mean$psi[c(9),c(1,2,3,4,5,6,7,8,9),12]),mean(Output.model$mean$psi[c(10),c(1,2,3,4,5,6,7,8,9),12]),mean(Output.model$mean$psi[c(11),c(1,2),12]),
                             mean(Output.model$mean$psi[c(12),c(1,2),12]),mean(Output.model$mean$psi[c(13),c(1,2),12]),mean(Output.model$mean$psi[c(14),c(1,2),12]),
                             mean(Output.model$mean$psi[c(15),c(1,2),12]),mean(Output.model$mean$psi[c(16),c(1,2),12]),mean(Output.model$mean$psi[c(17),c(1,2),12]),
                             mean(Output.model$mean$psi[c(18),c(1,2),12]),mean(Output.model$mean$psi[c(19),c(1,2),12]))),
                      mean(c(mean(Output.model$mean$psi[c(2),c(1,2,3,4,5,6,7,8,9),9]),mean(Output.model$mean$psi[c(3),c(1,2,3,4,5,6,7,8,9),9]),mean(Output.model$mean$psi[c(4),c(1,2,3,4,5,6,7,8,9),9]),
                             mean(Output.model$mean$psi[c(6),c(1,2,3,4,5,6,7,8,9),9]),mean(Output.model$mean$psi[c(7),c(1,2,3,4,5,6,7,8,9),9]),mean(Output.model$mean$psi[c(8),c(1,2,3,4,5,6,7,8,9),9]),
                             mean(Output.model$mean$psi[c(9),c(1,2,3,4,5,6,7,8,9),9]),mean(Output.model$mean$psi[c(10),c(1,2,3,4,5,6,7,8,9),9]),mean(Output.model$mean$psi[c(11),c(1,2),9]),
                             mean(Output.model$mean$psi[c(12),c(1,2),9]),mean(Output.model$mean$psi[c(13),c(1,2),9]),mean(Output.model$mean$psi[c(14),c(1,2),9]),
                             mean(Output.model$mean$psi[c(15),c(1,2),9]),mean(Output.model$mean$psi[c(16),c(1,2),9]),mean(Output.model$mean$psi[c(17),c(1,2),9]),
                             mean(Output.model$mean$psi[c(18),c(1,2),9]),mean(Output.model$mean$psi[c(19),c(1,2),9]))),
                      mean(c(mean(Output.model$mean$psi[c(2),c(1,2,3,4,5,6,7,8,9),47]),mean(Output.model$mean$psi[c(3),c(1,2,3,4,5,6,7,8,9),47]),mean(Output.model$mean$psi[c(4),c(1,2,3,4,5,6,7,8,9),47]),
                             mean(Output.model$mean$psi[c(6),c(1,2,3,4,5,6,7,8,9),47]),mean(Output.model$mean$psi[c(7),c(1,2,3,4,5,6,7,8,9),47]),mean(Output.model$mean$psi[c(8),c(1,2,3,4,5,6,7,8,9),47]),
                             mean(Output.model$mean$psi[c(9),c(1,2,3,4,5,6,7,8,9),47]),mean(Output.model$mean$psi[c(10),c(1,2,3,4,5,6,7,8,9),47]),mean(Output.model$mean$psi[c(11),c(1,2),47]),
                             mean(Output.model$mean$psi[c(12),c(1,2),47]),mean(Output.model$mean$psi[c(13),c(1,2),47]),mean(Output.model$mean$psi[c(14),c(1,2),47]),
                             mean(Output.model$mean$psi[c(15),c(1,2),47]),mean(Output.model$mean$psi[c(16),c(1,2),47]),mean(Output.model$mean$psi[c(17),c(1,2),47]),
                             mean(Output.model$mean$psi[c(18),c(1,2),47]),mean(Output.model$mean$psi[c(19),c(1,2),47]))),
                      mean(c(mean(Output.model$mean$psi[c(2),c(1,2,3,4,5,6,7,8,9),30]),mean(Output.model$mean$psi[c(3),c(1,2,3,4,5,6,7,8,9),30]),mean(Output.model$mean$psi[c(4),c(1,2,3,4,5,6,7,8,9),30]),
                             mean(Output.model$mean$psi[c(6),c(1,2,3,4,5,6,7,8,9),30]),mean(Output.model$mean$psi[c(7),c(1,2,3,4,5,6,7,8,9),30]),mean(Output.model$mean$psi[c(8),c(1,2,3,4,5,6,7,8,9),30]),
                             mean(Output.model$mean$psi[c(9),c(1,2,3,4,5,6,7,8,9),30]),mean(Output.model$mean$psi[c(10),c(1,2,3,4,5,6,7,8,9),30]),mean(Output.model$mean$psi[c(11),c(1,2),30]),
                             mean(Output.model$mean$psi[c(12),c(1,2),30]),mean(Output.model$mean$psi[c(13),c(1,2),30]),mean(Output.model$mean$psi[c(14),c(1,2),30]),
                             mean(Output.model$mean$psi[c(15),c(1,2),30]),mean(Output.model$mean$psi[c(16),c(1,2),30]),mean(Output.model$mean$psi[c(17),c(1,2),30]),
                             mean(Output.model$mean$psi[c(18),c(1,2),30]),mean(Output.model$mean$psi[c(19),c(1,2),30]))),
                      mean(c(mean(Output.model$mean$psi[c(2),c(1,2,3,4,5,6,7,8,9),40]),mean(Output.model$mean$psi[c(3),c(1,2,3,4,5,6,7,8,9),40]),mean(Output.model$mean$psi[c(4),c(1,2,3,4,5,6,7,8,9),40]),
                             mean(Output.model$mean$psi[c(6),c(1,2,3,4,5,6,7,8,9),40]),mean(Output.model$mean$psi[c(7),c(1,2,3,4,5,6,7,8,9),40]),mean(Output.model$mean$psi[c(8),c(1,2,3,4,5,6,7,8,9),40]),
                             mean(Output.model$mean$psi[c(9),c(1,2,3,4,5,6,7,8,9),40]),mean(Output.model$mean$psi[c(10),c(1,2,3,4,5,6,7,8,9),40]),mean(Output.model$mean$psi[c(11),c(1,2),40]),
                             mean(Output.model$mean$psi[c(12),c(1,2),40]),mean(Output.model$mean$psi[c(13),c(1,2),40]),mean(Output.model$mean$psi[c(14),c(1,2),40]),
                             mean(Output.model$mean$psi[c(15),c(1,2),40]),mean(Output.model$mean$psi[c(16),c(1,2),40]),mean(Output.model$mean$psi[c(17),c(1,2),40]),
                             mean(Output.model$mean$psi[c(18),c(1,2),40]),mean(Output.model$mean$psi[c(19),c(1,2),40]))),
                      mean(c(mean(Output.model$mean$psi[c(2),c(1,2,3,4,5,6,7,8,9),5]),mean(Output.model$mean$psi[c(3),c(1,2,3,4,5,6,7,8,9),5]),mean(Output.model$mean$psi[c(4),c(1,2,3,4,5,6,7,8,9),5]),
                             mean(Output.model$mean$psi[c(6),c(1,2,3,4,5,6,7,8,9),5]),mean(Output.model$mean$psi[c(7),c(1,2,3,4,5,6,7,8,9),5]),mean(Output.model$mean$psi[c(8),c(1,2,3,4,5,6,7,8,9),5]),
                             mean(Output.model$mean$psi[c(9),c(1,2,3,4,5,6,7,8,9),5]),mean(Output.model$mean$psi[c(10),c(1,2,3,4,5,6,7,8,9),5]),mean(Output.model$mean$psi[c(11),c(1,2),5]),
                             mean(Output.model$mean$psi[c(12),c(1,2),5]),mean(Output.model$mean$psi[c(13),c(1,2),5]),mean(Output.model$mean$psi[c(14),c(1,2),5]),
                             mean(Output.model$mean$psi[c(15),c(1,2),5]),mean(Output.model$mean$psi[c(16),c(1,2),5]),mean(Output.model$mean$psi[c(17),c(1,2),5]),
                             mean(Output.model$mean$psi[c(18),c(1,2),5]),mean(Output.model$mean$psi[c(19),c(1,2),5]))),
                      mean(c(mean(Output.model$mean$psi[c(2),c(1,2,3,4,5,6,7,8,9),20]),mean(Output.model$mean$psi[c(3),c(1,2,3,4,5,6,7,8,9),20]),mean(Output.model$mean$psi[c(4),c(1,2,3,4,5,6,7,8,9),20]),
                             mean(Output.model$mean$psi[c(6),c(1,2,3,4,5,6,7,8,9),20]),mean(Output.model$mean$psi[c(7),c(1,2,3,4,5,6,7,8,9),20]),mean(Output.model$mean$psi[c(8),c(1,2,3,4,5,6,7,8,9),20]),
                             mean(Output.model$mean$psi[c(9),c(1,2,3,4,5,6,7,8,9),20]),mean(Output.model$mean$psi[c(10),c(1,2,3,4,5,6,7,8,9),20]),mean(Output.model$mean$psi[c(11),c(1,2),20]),
                             mean(Output.model$mean$psi[c(12),c(1,2),20]),mean(Output.model$mean$psi[c(13),c(1,2),20]),mean(Output.model$mean$psi[c(14),c(1,2),20]),
                             mean(Output.model$mean$psi[c(15),c(1,2),20]),mean(Output.model$mean$psi[c(16),c(1,2),20]),mean(Output.model$mean$psi[c(17),c(1,2),20]),
                             mean(Output.model$mean$psi[c(18),c(1,2),20]),mean(Output.model$mean$psi[c(19),c(1,2),20]))),
                      mean(c(mean(Output.model$mean$psi[c(2),c(1,2,3,4,5,6,7,8,9),23]),mean(Output.model$mean$psi[c(3),c(1,2,3,4,5,6,7,8,9),23]),mean(Output.model$mean$psi[c(4),c(1,2,3,4,5,6,7,8,9),23]),
                             mean(Output.model$mean$psi[c(6),c(1,2,3,4,5,6,7,8,9),23]),mean(Output.model$mean$psi[c(7),c(1,2,3,4,5,6,7,8,9),23]),mean(Output.model$mean$psi[c(8),c(1,2,3,4,5,6,7,8,9),23]),
                             mean(Output.model$mean$psi[c(9),c(1,2,3,4,5,6,7,8,9),23]),mean(Output.model$mean$psi[c(10),c(1,2,3,4,5,6,7,8,9),23]),mean(Output.model$mean$psi[c(11),c(1,2),23]),
                             mean(Output.model$mean$psi[c(12),c(1,2),23]),mean(Output.model$mean$psi[c(13),c(1,2),23]),mean(Output.model$mean$psi[c(14),c(1,2),23]),
                             mean(Output.model$mean$psi[c(15),c(1,2),23]),mean(Output.model$mean$psi[c(16),c(1,2),23]),mean(Output.model$mean$psi[c(17),c(1,2),23]),
                             mean(Output.model$mean$psi[c(18),c(1,2),23]),mean(Output.model$mean$psi[c(19),c(1,2),23]))),
                      mean(c(mean(Output.model$mean$psi[c(2),c(1,2,3,4,5,6,7,8,9),3]),mean(Output.model$mean$psi[c(3),c(1,2,3,4,5,6,7,8,9),3]),mean(Output.model$mean$psi[c(4),c(1,2,3,4,5,6,7,8,9),3]),
                             mean(Output.model$mean$psi[c(6),c(1,2,3,4,5,6,7,8,9),3]),mean(Output.model$mean$psi[c(7),c(1,2,3,4,5,6,7,8,9),3]),mean(Output.model$mean$psi[c(8),c(1,2,3,4,5,6,7,8,9),3]),
                             mean(Output.model$mean$psi[c(9),c(1,2,3,4,5,6,7,8,9),3]),mean(Output.model$mean$psi[c(10),c(1,2,3,4,5,6,7,8,9),3]),mean(Output.model$mean$psi[c(11),c(1,2),3]),
                             mean(Output.model$mean$psi[c(12),c(1,2),3]),mean(Output.model$mean$psi[c(13),c(1,2),3]),mean(Output.model$mean$psi[c(14),c(1,2),3]),
                             mean(Output.model$mean$psi[c(15),c(1,2),3]),mean(Output.model$mean$psi[c(16),c(1,2),3]),mean(Output.model$mean$psi[c(17),c(1,2),3]),
                             mean(Output.model$mean$psi[c(18),c(1,2),3]),mean(Output.model$mean$psi[c(19),c(1,2),3]))),
                      mean(c(mean(Output.model$mean$psi[c(2),c(1,2,3,4,5,6,7,8,9),27]),mean(Output.model$mean$psi[c(3),c(1,2,3,4,5,6,7,8,9),27]),mean(Output.model$mean$psi[c(4),c(1,2,3,4,5,6,7,8,9),27]),
                             mean(Output.model$mean$psi[c(6),c(1,2,3,4,5,6,7,8,9),27]),mean(Output.model$mean$psi[c(7),c(1,2,3,4,5,6,7,8,9),27]),mean(Output.model$mean$psi[c(8),c(1,2,3,4,5,6,7,8,9),27]),
                             mean(Output.model$mean$psi[c(9),c(1,2,3,4,5,6,7,8,9),27]),mean(Output.model$mean$psi[c(10),c(1,2,3,4,5,6,7,8,9),27]),mean(Output.model$mean$psi[c(11),c(1,2),27]),
                             mean(Output.model$mean$psi[c(12),c(1,2),27]),mean(Output.model$mean$psi[c(13),c(1,2),27]),mean(Output.model$mean$psi[c(14),c(1,2),27]),
                             mean(Output.model$mean$psi[c(15),c(1,2),27]),mean(Output.model$mean$psi[c(16),c(1,2),27]),mean(Output.model$mean$psi[c(17),c(1,2),27]),
                             mean(Output.model$mean$psi[c(18),c(1,2),27]),mean(Output.model$mean$psi[c(19),c(1,2),27]))),
                      mean(c(mean(Output.model$mean$psi[c(2),c(1,2,3,4,5,6,7,8,9),26]),mean(Output.model$mean$psi[c(3),c(1,2,3,4,5,6,7,8,9),26]),mean(Output.model$mean$psi[c(4),c(1,2,3,4,5,6,7,8,9),26]),
                             mean(Output.model$mean$psi[c(6),c(1,2,3,4,5,6,7,8,9),26]),mean(Output.model$mean$psi[c(7),c(1,2,3,4,5,6,7,8,9),26]),mean(Output.model$mean$psi[c(8),c(1,2,3,4,5,6,7,8,9),26]),
                             mean(Output.model$mean$psi[c(9),c(1,2,3,4,5,6,7,8,9),26]),mean(Output.model$mean$psi[c(10),c(1,2,3,4,5,6,7,8,9),26]),mean(Output.model$mean$psi[c(11),c(1,2),26]),
                             mean(Output.model$mean$psi[c(12),c(1,2),26]),mean(Output.model$mean$psi[c(13),c(1,2),26]),mean(Output.model$mean$psi[c(14),c(1,2),26]),
                             mean(Output.model$mean$psi[c(15),c(1,2),26]),mean(Output.model$mean$psi[c(16),c(1,2),26]),mean(Output.model$mean$psi[c(17),c(1,2),26]),
                             mean(Output.model$mean$psi[c(18),c(1,2),26]),mean(Output.model$mean$psi[c(19),c(1,2),26]))),
                      mean(c(mean(Output.model$mean$psi[c(2),c(1,2,3,4,5,6,7,8,9),44]),mean(Output.model$mean$psi[c(3),c(1,2,3,4,5,6,7,8,9),44]),mean(Output.model$mean$psi[c(4),c(1,2,3,4,5,6,7,8,9),44]),
                             mean(Output.model$mean$psi[c(6),c(1,2,3,4,5,6,7,8,9),44]),mean(Output.model$mean$psi[c(7),c(1,2,3,4,5,6,7,8,9),44]),mean(Output.model$mean$psi[c(8),c(1,2,3,4,5,6,7,8,9),44]),
                             mean(Output.model$mean$psi[c(9),c(1,2,3,4,5,6,7,8,9),44]),mean(Output.model$mean$psi[c(10),c(1,2,3,4,5,6,7,8,9),44]),mean(Output.model$mean$psi[c(11),c(1,2),44]),
                             mean(Output.model$mean$psi[c(12),c(1,2),44]),mean(Output.model$mean$psi[c(13),c(1,2),44]),mean(Output.model$mean$psi[c(14),c(1,2),44]),
                             mean(Output.model$mean$psi[c(15),c(1,2),44]),mean(Output.model$mean$psi[c(16),c(1,2),44]),mean(Output.model$mean$psi[c(17),c(1,2),44]),
                             mean(Output.model$mean$psi[c(18),c(1,2),44]),mean(Output.model$mean$psi[c(19),c(1,2),44]))),
                      mean(c(mean(Output.model$mean$psi[c(2),c(1,2,3,4,5,6,7,8,9),42]),mean(Output.model$mean$psi[c(3),c(1,2,3,4,5,6,7,8,9),42]),mean(Output.model$mean$psi[c(4),c(1,2,3,4,5,6,7,8,9),42]),
                             mean(Output.model$mean$psi[c(6),c(1,2,3,4,5,6,7,8,9),42]),mean(Output.model$mean$psi[c(7),c(1,2,3,4,5,6,7,8,9),42]),mean(Output.model$mean$psi[c(8),c(1,2,3,4,5,6,7,8,9),42]),
                             mean(Output.model$mean$psi[c(9),c(1,2,3,4,5,6,7,8,9),42]),mean(Output.model$mean$psi[c(10),c(1,2,3,4,5,6,7,8,9),42]),mean(Output.model$mean$psi[c(11),c(1,2),42]),
                             mean(Output.model$mean$psi[c(12),c(1,2),42]),mean(Output.model$mean$psi[c(13),c(1,2),42]),mean(Output.model$mean$psi[c(14),c(1,2),42]),
                             mean(Output.model$mean$psi[c(15),c(1,2),42]),mean(Output.model$mean$psi[c(16),c(1,2),42]),mean(Output.model$mean$psi[c(17),c(1,2),42]),
                             mean(Output.model$mean$psi[c(18),c(1,2),42]),mean(Output.model$mean$psi[c(19),c(1,2),42]))),
                      mean(c(mean(Output.model$mean$psi[c(2),c(1,2,3,4,5,6,7,8,9),43]),mean(Output.model$mean$psi[c(3),c(1,2,3,4,5,6,7,8,9),43]),mean(Output.model$mean$psi[c(4),c(1,2,3,4,5,6,7,8,9),43]),
                             mean(Output.model$mean$psi[c(6),c(1,2,3,4,5,6,7,8,9),43]),mean(Output.model$mean$psi[c(7),c(1,2,3,4,5,6,7,8,9),43]),mean(Output.model$mean$psi[c(8),c(1,2,3,4,5,6,7,8,9),43]),
                             mean(Output.model$mean$psi[c(9),c(1,2,3,4,5,6,7,8,9),43]),mean(Output.model$mean$psi[c(10),c(1,2,3,4,5,6,7,8,9),43]),mean(Output.model$mean$psi[c(11),c(1,2),43]),
                             mean(Output.model$mean$psi[c(12),c(1,2),43]),mean(Output.model$mean$psi[c(13),c(1,2),43]),mean(Output.model$mean$psi[c(14),c(1,2),43]),
                             mean(Output.model$mean$psi[c(15),c(1,2),43]),mean(Output.model$mean$psi[c(16),c(1,2),43]),mean(Output.model$mean$psi[c(17),c(1,2),43]),
                             mean(Output.model$mean$psi[c(18),c(1,2),43]),mean(Output.model$mean$psi[c(19),c(1,2),43]))),
                      mean(c(mean(Output.model$mean$psi[c(2),c(1,2,3,4,5,6,7,8,9),38]),mean(Output.model$mean$psi[c(3),c(1,2,3,4,5,6,7,8,9),38]),mean(Output.model$mean$psi[c(4),c(1,2,3,4,5,6,7,8,9),38]),
                             mean(Output.model$mean$psi[c(6),c(1,2,3,4,5,6,7,8,9),38]),mean(Output.model$mean$psi[c(7),c(1,2,3,4,5,6,7,8,9),38]),mean(Output.model$mean$psi[c(8),c(1,2,3,4,5,6,7,8,9),38]),
                             mean(Output.model$mean$psi[c(9),c(1,2,3,4,5,6,7,8,9),38]),mean(Output.model$mean$psi[c(10),c(1,2,3,4,5,6,7,8,9),38]),mean(Output.model$mean$psi[c(11),c(1,2),38]),
                             mean(Output.model$mean$psi[c(12),c(1,2),38]),mean(Output.model$mean$psi[c(13),c(1,2),38]),mean(Output.model$mean$psi[c(14),c(1,2),38]),
                             mean(Output.model$mean$psi[c(15),c(1,2),38]),mean(Output.model$mean$psi[c(16),c(1,2),38]),mean(Output.model$mean$psi[c(17),c(1,2),38]),
                             mean(Output.model$mean$psi[c(18),c(1,2),38]),mean(Output.model$mean$psi[c(19),c(1,2),38]))),
                      mean(c(mean(Output.model$mean$psi[c(2),c(1,2,3,4,5,6,7,8,9),24]),mean(Output.model$mean$psi[c(3),c(1,2,3,4,5,6,7,8,9),24]),mean(Output.model$mean$psi[c(4),c(1,2,3,4,5,6,7,8,9),24]),
                             mean(Output.model$mean$psi[c(6),c(1,2,3,4,5,6,7,8,9),24]),mean(Output.model$mean$psi[c(7),c(1,2,3,4,5,6,7,8,9),24]),mean(Output.model$mean$psi[c(8),c(1,2,3,4,5,6,7,8,9),24]),
                             mean(Output.model$mean$psi[c(9),c(1,2,3,4,5,6,7,8,9),24]),mean(Output.model$mean$psi[c(10),c(1,2,3,4,5,6,7,8,9),24]),mean(Output.model$mean$psi[c(11),c(1,2),24]),
                             mean(Output.model$mean$psi[c(12),c(1,2),24]),mean(Output.model$mean$psi[c(13),c(1,2),24]),mean(Output.model$mean$psi[c(14),c(1,2),24]),
                             mean(Output.model$mean$psi[c(15),c(1,2),24]),mean(Output.model$mean$psi[c(16),c(1,2),24]),mean(Output.model$mean$psi[c(17),c(1,2),24]),
                             mean(Output.model$mean$psi[c(18),c(1,2),24]),mean(Output.model$mean$psi[c(19),c(1,2),24]))),
                      mean(c(mean(Output.model$mean$psi[c(2),c(1,2,3,4,5,6,7,8,9),37]),mean(Output.model$mean$psi[c(3),c(1,2,3,4,5,6,7,8,9),37]),mean(Output.model$mean$psi[c(4),c(1,2,3,4,5,6,7,8,9),37]),
                             mean(Output.model$mean$psi[c(6),c(1,2,3,4,5,6,7,8,9),37]),mean(Output.model$mean$psi[c(7),c(1,2,3,4,5,6,7,8,9),37]),mean(Output.model$mean$psi[c(8),c(1,2,3,4,5,6,7,8,9),37]),
                             mean(Output.model$mean$psi[c(9),c(1,2,3,4,5,6,7,8,9),37]),mean(Output.model$mean$psi[c(10),c(1,2,3,4,5,6,7,8,9),37]),mean(Output.model$mean$psi[c(11),c(1,2),37]),
                             mean(Output.model$mean$psi[c(12),c(1,2),37]),mean(Output.model$mean$psi[c(13),c(1,2),37]),mean(Output.model$mean$psi[c(14),c(1,2),37]),
                             mean(Output.model$mean$psi[c(15),c(1,2),37]),mean(Output.model$mean$psi[c(16),c(1,2),37]),mean(Output.model$mean$psi[c(17),c(1,2),37]),
                             mean(Output.model$mean$psi[c(18),c(1,2),37]),mean(Output.model$mean$psi[c(19),c(1,2),37]))),
                      mean(c(mean(Output.model$mean$psi[c(2),c(1,2,3,4,5,6,7,8,9),13]),mean(Output.model$mean$psi[c(3),c(1,2,3,4,5,6,7,8,9),13]),mean(Output.model$mean$psi[c(4),c(1,2,3,4,5,6,7,8,9),13]),
                             mean(Output.model$mean$psi[c(6),c(1,2,3,4,5,6,7,8,9),13]),mean(Output.model$mean$psi[c(7),c(1,2,3,4,5,6,7,8,9),13]),mean(Output.model$mean$psi[c(8),c(1,2,3,4,5,6,7,8,9),13]),
                             mean(Output.model$mean$psi[c(9),c(1,2,3,4,5,6,7,8,9),13]),mean(Output.model$mean$psi[c(10),c(1,2,3,4,5,6,7,8,9),13]),mean(Output.model$mean$psi[c(11),c(1,2),13]),
                             mean(Output.model$mean$psi[c(12),c(1,2),13]),mean(Output.model$mean$psi[c(13),c(1,2),13]),mean(Output.model$mean$psi[c(14),c(1,2),13]),
                             mean(Output.model$mean$psi[c(15),c(1,2),13]),mean(Output.model$mean$psi[c(16),c(1,2),13]),mean(Output.model$mean$psi[c(17),c(1,2),13]),
                             mean(Output.model$mean$psi[c(18),c(1,2),13]),mean(Output.model$mean$psi[c(19),c(1,2),13]))),
                      mean(c(mean(Output.model$mean$psi[c(2),c(1,2,3,4,5,6,7,8,9),21]),mean(Output.model$mean$psi[c(3),c(1,2,3,4,5,6,7,8,9),21]),mean(Output.model$mean$psi[c(4),c(1,2,3,4,5,6,7,8,9),21]),
                             mean(Output.model$mean$psi[c(6),c(1,2,3,4,5,6,7,8,9),21]),mean(Output.model$mean$psi[c(7),c(1,2,3,4,5,6,7,8,9),21]),mean(Output.model$mean$psi[c(8),c(1,2,3,4,5,6,7,8,9),21]),
                             mean(Output.model$mean$psi[c(9),c(1,2,3,4,5,6,7,8,9),21]),mean(Output.model$mean$psi[c(10),c(1,2,3,4,5,6,7,8,9),21]),mean(Output.model$mean$psi[c(11),c(1,2),21]),
                             mean(Output.model$mean$psi[c(12),c(1,2),21]),mean(Output.model$mean$psi[c(13),c(1,2),21]),mean(Output.model$mean$psi[c(14),c(1,2),21]),
                             mean(Output.model$mean$psi[c(15),c(1,2),21]),mean(Output.model$mean$psi[c(16),c(1,2),21]),mean(Output.model$mean$psi[c(17),c(1,2),21]),
                             mean(Output.model$mean$psi[c(18),c(1,2),21]),mean(Output.model$mean$psi[c(19),c(1,2),21]))),
                      mean(c(mean(Output.model$mean$psi[c(2),c(1,2,3,4,5,6,7,8,9),34]),mean(Output.model$mean$psi[c(3),c(1,2,3,4,5,6,7,8,9),34]),mean(Output.model$mean$psi[c(4),c(1,2,3,4,5,6,7,8,9),34]),
                             mean(Output.model$mean$psi[c(6),c(1,2,3,4,5,6,7,8,9),34]),mean(Output.model$mean$psi[c(7),c(1,2,3,4,5,6,7,8,9),34]),mean(Output.model$mean$psi[c(8),c(1,2,3,4,5,6,7,8,9),34]),
                             mean(Output.model$mean$psi[c(9),c(1,2,3,4,5,6,7,8,9),34]),mean(Output.model$mean$psi[c(10),c(1,2,3,4,5,6,7,8,9),34]),mean(Output.model$mean$psi[c(11),c(1,2),34]),
                             mean(Output.model$mean$psi[c(12),c(1,2),34]),mean(Output.model$mean$psi[c(13),c(1,2),34]),mean(Output.model$mean$psi[c(14),c(1,2),34]),
                             mean(Output.model$mean$psi[c(15),c(1,2),34]),mean(Output.model$mean$psi[c(16),c(1,2),34]),mean(Output.model$mean$psi[c(17),c(1,2),34]),
                             mean(Output.model$mean$psi[c(18),c(1,2),34]),mean(Output.model$mean$psi[c(19),c(1,2),34]))),
                      mean(c(mean(Output.model$mean$psi[c(2),c(1,2,3,4,5,6,7,8,9),2]),mean(Output.model$mean$psi[c(3),c(1,2,3,4,5,6,7,8,9),2]),mean(Output.model$mean$psi[c(4),c(1,2,3,4,5,6,7,8,9),2]),
                             mean(Output.model$mean$psi[c(6),c(1,2,3,4,5,6,7,8,9),2]),mean(Output.model$mean$psi[c(7),c(1,2,3,4,5,6,7,8,9),2]),mean(Output.model$mean$psi[c(8),c(1,2,3,4,5,6,7,8,9),2]),
                             mean(Output.model$mean$psi[c(9),c(1,2,3,4,5,6,7,8,9),2]),mean(Output.model$mean$psi[c(10),c(1,2,3,4,5,6,7,8,9),2]),mean(Output.model$mean$psi[c(11),c(1,2),2]),
                             mean(Output.model$mean$psi[c(12),c(1,2),2]),mean(Output.model$mean$psi[c(13),c(1,2),2]),mean(Output.model$mean$psi[c(14),c(1,2),2]),
                             mean(Output.model$mean$psi[c(15),c(1,2),2]),mean(Output.model$mean$psi[c(16),c(1,2),2]),mean(Output.model$mean$psi[c(17),c(1,2),2]),
                             mean(Output.model$mean$psi[c(18),c(1,2),2]),mean(Output.model$mean$psi[c(19),c(1,2),2]))),
                      mean(c(mean(Output.model$mean$psi[c(2),c(1,2,3,4,5,6,7,8,9),32]),mean(Output.model$mean$psi[c(3),c(1,2,3,4,5,6,7,8,9),32]),mean(Output.model$mean$psi[c(4),c(1,2,3,4,5,6,7,8,9),32]),
                             mean(Output.model$mean$psi[c(6),c(1,2,3,4,5,6,7,8,9),32]),mean(Output.model$mean$psi[c(7),c(1,2,3,4,5,6,7,8,9),32]),mean(Output.model$mean$psi[c(8),c(1,2,3,4,5,6,7,8,9),32]),
                             mean(Output.model$mean$psi[c(9),c(1,2,3,4,5,6,7,8,9),32]),mean(Output.model$mean$psi[c(10),c(1,2,3,4,5,6,7,8,9),32]),mean(Output.model$mean$psi[c(11),c(1,2),32]),
                             mean(Output.model$mean$psi[c(12),c(1,2),32]),mean(Output.model$mean$psi[c(13),c(1,2),32]),mean(Output.model$mean$psi[c(14),c(1,2),32]),
                             mean(Output.model$mean$psi[c(15),c(1,2),32]),mean(Output.model$mean$psi[c(16),c(1,2),32]),mean(Output.model$mean$psi[c(17),c(1,2),32]),
                             mean(Output.model$mean$psi[c(18),c(1,2),32]),mean(Output.model$mean$psi[c(19),c(1,2),32]))),
                      mean(c(mean(Output.model$mean$psi[c(2),c(1,2,3,4,5,6,7,8,9),17]),mean(Output.model$mean$psi[c(3),c(1,2,3,4,5,6,7,8,9),17]),mean(Output.model$mean$psi[c(4),c(1,2,3,4,5,6,7,8,9),17]),
                             mean(Output.model$mean$psi[c(6),c(1,2,3,4,5,6,7,8,9),17]),mean(Output.model$mean$psi[c(7),c(1,2,3,4,5,6,7,8,9),17]),mean(Output.model$mean$psi[c(8),c(1,2,3,4,5,6,7,8,9),17]),
                             mean(Output.model$mean$psi[c(9),c(1,2,3,4,5,6,7,8,9),17]),mean(Output.model$mean$psi[c(10),c(1,2,3,4,5,6,7,8,9),17]),mean(Output.model$mean$psi[c(11),c(1,2),17]),
                             mean(Output.model$mean$psi[c(12),c(1,2),17]),mean(Output.model$mean$psi[c(13),c(1,2),17]),mean(Output.model$mean$psi[c(14),c(1,2),17]),
                             mean(Output.model$mean$psi[c(15),c(1,2),17]),mean(Output.model$mean$psi[c(16),c(1,2),17]),mean(Output.model$mean$psi[c(17),c(1,2),17]),
                             mean(Output.model$mean$psi[c(18),c(1,2),17]),mean(Output.model$mean$psi[c(19),c(1,2),17]))),
                      mean(c(mean(Output.model$mean$psi[c(2),c(1,2,3,4,5,6,7,8,9),4]),mean(Output.model$mean$psi[c(3),c(1,2,3,4,5,6,7,8,9),4]),mean(Output.model$mean$psi[c(4),c(1,2,3,4,5,6,7,8,9),4]),
                             mean(Output.model$mean$psi[c(6),c(1,2,3,4,5,6,7,8,9),4]),mean(Output.model$mean$psi[c(7),c(1,2,3,4,5,6,7,8,9),4]),mean(Output.model$mean$psi[c(8),c(1,2,3,4,5,6,7,8,9),4]),
                             mean(Output.model$mean$psi[c(9),c(1,2,3,4,5,6,7,8,9),4]),mean(Output.model$mean$psi[c(10),c(1,2,3,4,5,6,7,8,9),4]),mean(Output.model$mean$psi[c(11),c(1,2),4]),
                             mean(Output.model$mean$psi[c(12),c(1,2),4]),mean(Output.model$mean$psi[c(13),c(1,2),4]),mean(Output.model$mean$psi[c(14),c(1,2),4]),
                             mean(Output.model$mean$psi[c(15),c(1,2),4]),mean(Output.model$mean$psi[c(16),c(1,2),4]),mean(Output.model$mean$psi[c(17),c(1,2),4]),
                             mean(Output.model$mean$psi[c(18),c(1,2),4]),mean(Output.model$mean$psi[c(19),c(1,2),4]))),
                      mean(c(mean(Output.model$mean$psi[c(2),c(1,2,3,4,5,6,7,8,9),8]),mean(Output.model$mean$psi[c(3),c(1,2,3,4,5,6,7,8,9),8]),mean(Output.model$mean$psi[c(4),c(1,2,3,4,5,6,7,8,9),8]),
                             mean(Output.model$mean$psi[c(6),c(1,2,3,4,5,6,7,8,9),8]),mean(Output.model$mean$psi[c(7),c(1,2,3,4,5,6,7,8,9),8]),mean(Output.model$mean$psi[c(8),c(1,2,3,4,5,6,7,8,9),8]),
                             mean(Output.model$mean$psi[c(9),c(1,2,3,4,5,6,7,8,9),8]),mean(Output.model$mean$psi[c(10),c(1,2,3,4,5,6,7,8,9),8]),mean(Output.model$mean$psi[c(11),c(1,2),8]),
                             mean(Output.model$mean$psi[c(12),c(1,2),8]),mean(Output.model$mean$psi[c(13),c(1,2),8]),mean(Output.model$mean$psi[c(14),c(1,2),8]),
                             mean(Output.model$mean$psi[c(15),c(1,2),8]),mean(Output.model$mean$psi[c(16),c(1,2),8]),mean(Output.model$mean$psi[c(17),c(1,2),8]),
                             mean(Output.model$mean$psi[c(18),c(1,2),8]),mean(Output.model$mean$psi[c(19),c(1,2),8]))),
                      mean(c(mean(Output.model$mean$psi[c(2),c(1,2,3,4,5,6,7,8,9),39]),mean(Output.model$mean$psi[c(3),c(1,2,3,4,5,6,7,8,9),39]),mean(Output.model$mean$psi[c(4),c(1,2,3,4,5,6,7,8,9),39]),
                             mean(Output.model$mean$psi[c(6),c(1,2,3,4,5,6,7,8,9),39]),mean(Output.model$mean$psi[c(7),c(1,2,3,4,5,6,7,8,9),39]),mean(Output.model$mean$psi[c(8),c(1,2,3,4,5,6,7,8,9),39]),
                             mean(Output.model$mean$psi[c(9),c(1,2,3,4,5,6,7,8,9),39]),mean(Output.model$mean$psi[c(10),c(1,2,3,4,5,6,7,8,9),39]),mean(Output.model$mean$psi[c(11),c(1,2),39]),
                             mean(Output.model$mean$psi[c(12),c(1,2),39]),mean(Output.model$mean$psi[c(13),c(1,2),39]),mean(Output.model$mean$psi[c(14),c(1,2),39]),
                             mean(Output.model$mean$psi[c(15),c(1,2),39]),mean(Output.model$mean$psi[c(16),c(1,2),39]),mean(Output.model$mean$psi[c(17),c(1,2),39]),
                             mean(Output.model$mean$psi[c(18),c(1,2),39]),mean(Output.model$mean$psi[c(19),c(1,2),39]))),
                      mean(c(mean(Output.model$mean$psi[c(2),c(1,2,3,4,5,6,7,8,9),10]),mean(Output.model$mean$psi[c(3),c(1,2,3,4,5,6,7,8,9),10]),mean(Output.model$mean$psi[c(4),c(1,2,3,4,5,6,7,8,9),10]),
                             mean(Output.model$mean$psi[c(6),c(1,2,3,4,5,6,7,8,9),10]),mean(Output.model$mean$psi[c(7),c(1,2,3,4,5,6,7,8,9),10]),mean(Output.model$mean$psi[c(8),c(1,2,3,4,5,6,7,8,9),10]),
                             mean(Output.model$mean$psi[c(9),c(1,2,3,4,5,6,7,8,9),10]),mean(Output.model$mean$psi[c(10),c(1,2,3,4,5,6,7,8,9),10]),mean(Output.model$mean$psi[c(11),c(1,2),10]),
                             mean(Output.model$mean$psi[c(12),c(1,2),10]),mean(Output.model$mean$psi[c(13),c(1,2),10]),mean(Output.model$mean$psi[c(14),c(1,2),10]),
                             mean(Output.model$mean$psi[c(15),c(1,2),10]),mean(Output.model$mean$psi[c(16),c(1,2),10]),mean(Output.model$mean$psi[c(17),c(1,2),10]),
                             mean(Output.model$mean$psi[c(18),c(1,2),10]),mean(Output.model$mean$psi[c(19),c(1,2),10]))),
                      mean(c(mean(Output.model$mean$psi[c(2),c(1,2,3,4,5,6,7,8,9),19]),mean(Output.model$mean$psi[c(3),c(1,2,3,4,5,6,7,8,9),19]),mean(Output.model$mean$psi[c(4),c(1,2,3,4,5,6,7,8,9),19]),
                             mean(Output.model$mean$psi[c(6),c(1,2,3,4,5,6,7,8,9),19]),mean(Output.model$mean$psi[c(7),c(1,2,3,4,5,6,7,8,9),19]),mean(Output.model$mean$psi[c(8),c(1,2,3,4,5,6,7,8,9),19]),
                             mean(Output.model$mean$psi[c(9),c(1,2,3,4,5,6,7,8,9),19]),mean(Output.model$mean$psi[c(10),c(1,2,3,4,5,6,7,8,9),19]),mean(Output.model$mean$psi[c(11),c(1,2),19]),
                             mean(Output.model$mean$psi[c(12),c(1,2),19]),mean(Output.model$mean$psi[c(13),c(1,2),19]),mean(Output.model$mean$psi[c(14),c(1,2),19]),
                             mean(Output.model$mean$psi[c(15),c(1,2),19]),mean(Output.model$mean$psi[c(16),c(1,2),19]),mean(Output.model$mean$psi[c(17),c(1,2),19]),
                             mean(Output.model$mean$psi[c(18),c(1,2),19]),mean(Output.model$mean$psi[c(19),c(1,2),19]))),
                      mean(c(mean(Output.model$mean$psi[c(2),c(1,2,3,4,5,6,7,8,9),35]),mean(Output.model$mean$psi[c(3),c(1,2,3,4,5,6,7,8,9),35]),mean(Output.model$mean$psi[c(4),c(1,2,3,4,5,6,7,8,9),35]),
                             mean(Output.model$mean$psi[c(6),c(1,2,3,4,5,6,7,8,9),35]),mean(Output.model$mean$psi[c(7),c(1,2,3,4,5,6,7,8,9),35]),mean(Output.model$mean$psi[c(8),c(1,2,3,4,5,6,7,8,9),35]),
                             mean(Output.model$mean$psi[c(9),c(1,2,3,4,5,6,7,8,9),35]),mean(Output.model$mean$psi[c(10),c(1,2,3,4,5,6,7,8,9),35]),mean(Output.model$mean$psi[c(11),c(1,2),35]),
                             mean(Output.model$mean$psi[c(12),c(1,2),35]),mean(Output.model$mean$psi[c(13),c(1,2),35]),mean(Output.model$mean$psi[c(14),c(1,2),35]),
                             mean(Output.model$mean$psi[c(15),c(1,2),35]),mean(Output.model$mean$psi[c(16),c(1,2),35]),mean(Output.model$mean$psi[c(17),c(1,2),35]),
                             mean(Output.model$mean$psi[c(18),c(1,2),35]),mean(Output.model$mean$psi[c(19),c(1,2),35]))),
                      mean(c(mean(Output.model$mean$psi[c(2),c(1,2,3,4,5,6,7,8,9),25]),mean(Output.model$mean$psi[c(3),c(1,2,3,4,5,6,7,8,9),25]),mean(Output.model$mean$psi[c(4),c(1,2,3,4,5,6,7,8,9),25]),
                             mean(Output.model$mean$psi[c(6),c(1,2,3,4,5,6,7,8,9),25]),mean(Output.model$mean$psi[c(7),c(1,2,3,4,5,6,7,8,9),25]),mean(Output.model$mean$psi[c(8),c(1,2,3,4,5,6,7,8,9),25]),
                             mean(Output.model$mean$psi[c(9),c(1,2,3,4,5,6,7,8,9),25]),mean(Output.model$mean$psi[c(10),c(1,2,3,4,5,6,7,8,9),25]),mean(Output.model$mean$psi[c(11),c(1,2),25]),
                             mean(Output.model$mean$psi[c(12),c(1,2),25]),mean(Output.model$mean$psi[c(13),c(1,2),25]),mean(Output.model$mean$psi[c(14),c(1,2),25]),
                             mean(Output.model$mean$psi[c(15),c(1,2),25]),mean(Output.model$mean$psi[c(16),c(1,2),25]),mean(Output.model$mean$psi[c(17),c(1,2),25]),
                             mean(Output.model$mean$psi[c(18),c(1,2),25]),mean(Output.model$mean$psi[c(19),c(1,2),25]))),
                      mean(c(mean(Output.model$mean$psi[c(2),c(1,2,3,4,5,6,7,8,9),28]),mean(Output.model$mean$psi[c(3),c(1,2,3,4,5,6,7,8,9),28]),mean(Output.model$mean$psi[c(4),c(1,2,3,4,5,6,7,8,9),28]),
                             mean(Output.model$mean$psi[c(6),c(1,2,3,4,5,6,7,8,9),28]),mean(Output.model$mean$psi[c(7),c(1,2,3,4,5,6,7,8,9),28]),mean(Output.model$mean$psi[c(8),c(1,2,3,4,5,6,7,8,9),28]),
                             mean(Output.model$mean$psi[c(9),c(1,2,3,4,5,6,7,8,9),28]),mean(Output.model$mean$psi[c(10),c(1,2,3,4,5,6,7,8,9),28]),mean(Output.model$mean$psi[c(11),c(1,2),28]),
                             mean(Output.model$mean$psi[c(12),c(1,2),28]),mean(Output.model$mean$psi[c(13),c(1,2),28]),mean(Output.model$mean$psi[c(14),c(1,2),28]),
                             mean(Output.model$mean$psi[c(15),c(1,2),28]),mean(Output.model$mean$psi[c(16),c(1,2),28]),mean(Output.model$mean$psi[c(17),c(1,2),28]),
                             mean(Output.model$mean$psi[c(18),c(1,2),28]),mean(Output.model$mean$psi[c(19),c(1,2),28]))),
                      mean(c(mean(Output.model$mean$psi[c(2),c(1,2,3,4,5,6,7,8,9),46]),mean(Output.model$mean$psi[c(3),c(1,2,3,4,5,6,7,8,9),46]),mean(Output.model$mean$psi[c(4),c(1,2,3,4,5,6,7,8,9),46]),
                             mean(Output.model$mean$psi[c(6),c(1,2,3,4,5,6,7,8,9),46]),mean(Output.model$mean$psi[c(7),c(1,2,3,4,5,6,7,8,9),46]),mean(Output.model$mean$psi[c(8),c(1,2,3,4,5,6,7,8,9),46]),
                             mean(Output.model$mean$psi[c(9),c(1,2,3,4,5,6,7,8,9),46]),mean(Output.model$mean$psi[c(10),c(1,2,3,4,5,6,7,8,9),46]),mean(Output.model$mean$psi[c(11),c(1,2),46]),
                             mean(Output.model$mean$psi[c(12),c(1,2),46]),mean(Output.model$mean$psi[c(13),c(1,2),46]),mean(Output.model$mean$psi[c(14),c(1,2),46]),
                             mean(Output.model$mean$psi[c(15),c(1,2),46]),mean(Output.model$mean$psi[c(16),c(1,2),46]),mean(Output.model$mean$psi[c(17),c(1,2),46]),
                             mean(Output.model$mean$psi[c(18),c(1,2),46]),mean(Output.model$mean$psi[c(19),c(1,2),46]))),
                      mean(c(mean(Output.model$mean$psi[c(2),c(1,2,3,4,5,6,7,8,9),41]),mean(Output.model$mean$psi[c(3),c(1,2,3,4,5,6,7,8,9),41]),mean(Output.model$mean$psi[c(4),c(1,2,3,4,5,6,7,8,9),41]),
                             mean(Output.model$mean$psi[c(6),c(1,2,3,4,5,6,7,8,9),41]),mean(Output.model$mean$psi[c(7),c(1,2,3,4,5,6,7,8,9),41]),mean(Output.model$mean$psi[c(8),c(1,2,3,4,5,6,7,8,9),41]),
                             mean(Output.model$mean$psi[c(9),c(1,2,3,4,5,6,7,8,9),41]),mean(Output.model$mean$psi[c(10),c(1,2,3,4,5,6,7,8,9),41]),mean(Output.model$mean$psi[c(11),c(1,2),41]),
                             mean(Output.model$mean$psi[c(12),c(1,2),41]),mean(Output.model$mean$psi[c(13),c(1,2),41]),mean(Output.model$mean$psi[c(14),c(1,2),41]),
                             mean(Output.model$mean$psi[c(15),c(1,2),41]),mean(Output.model$mean$psi[c(16),c(1,2),41]),mean(Output.model$mean$psi[c(17),c(1,2),41]),
                             mean(Output.model$mean$psi[c(18),c(1,2),41]),mean(Output.model$mean$psi[c(19),c(1,2),41]))),
                      mean(c(mean(Output.model$mean$psi[c(2),c(1,2,3,4,5,6,7,8,9),16]),mean(Output.model$mean$psi[c(3),c(1,2,3,4,5,6,7,8,9),16]),mean(Output.model$mean$psi[c(4),c(1,2,3,4,5,6,7,8,9),16]),
                             mean(Output.model$mean$psi[c(6),c(1,2,3,4,5,6,7,8,9),16]),mean(Output.model$mean$psi[c(7),c(1,2,3,4,5,6,7,8,9),16]),mean(Output.model$mean$psi[c(8),c(1,2,3,4,5,6,7,8,9),16]),
                             mean(Output.model$mean$psi[c(9),c(1,2,3,4,5,6,7,8,9),16]),mean(Output.model$mean$psi[c(10),c(1,2,3,4,5,6,7,8,9),16]),mean(Output.model$mean$psi[c(11),c(1,2),16]),
                             mean(Output.model$mean$psi[c(12),c(1,2),16]),mean(Output.model$mean$psi[c(13),c(1,2),16]),mean(Output.model$mean$psi[c(14),c(1,2),16]),
                             mean(Output.model$mean$psi[c(15),c(1,2),16]),mean(Output.model$mean$psi[c(16),c(1,2),16]),mean(Output.model$mean$psi[c(17),c(1,2),16]),
                             mean(Output.model$mean$psi[c(18),c(1,2),16]),mean(Output.model$mean$psi[c(19),c(1,2),16]))),
                      mean(c(mean(Output.model$mean$psi[c(2),c(1,2,3,4,5,6,7,8,9),36]),mean(Output.model$mean$psi[c(3),c(1,2,3,4,5,6,7,8,9),36]),mean(Output.model$mean$psi[c(4),c(1,2,3,4,5,6,7,8,9),36]),
                             mean(Output.model$mean$psi[c(6),c(1,2,3,4,5,6,7,8,9),36]),mean(Output.model$mean$psi[c(7),c(1,2,3,4,5,6,7,8,9),36]),mean(Output.model$mean$psi[c(8),c(1,2,3,4,5,6,7,8,9),36]),
                             mean(Output.model$mean$psi[c(9),c(1,2,3,4,5,6,7,8,9),36]),mean(Output.model$mean$psi[c(10),c(1,2,3,4,5,6,7,8,9),36]),mean(Output.model$mean$psi[c(11),c(1,2),36]),
                             mean(Output.model$mean$psi[c(12),c(1,2),36]),mean(Output.model$mean$psi[c(13),c(1,2),36]),mean(Output.model$mean$psi[c(14),c(1,2),36]),
                             mean(Output.model$mean$psi[c(15),c(1,2),36]),mean(Output.model$mean$psi[c(16),c(1,2),36]),mean(Output.model$mean$psi[c(17),c(1,2),36]),
                             mean(Output.model$mean$psi[c(18),c(1,2),36]),mean(Output.model$mean$psi[c(19),c(1,2),36]))),
                      mean(c(mean(Output.model$mean$psi[c(2),c(1,2,3,4,5,6,7,8,9),14]),mean(Output.model$mean$psi[c(3),c(1,2,3,4,5,6,7,8,9),14]),mean(Output.model$mean$psi[c(4),c(1,2,3,4,5,6,7,8,9),14]),
                             mean(Output.model$mean$psi[c(6),c(1,2,3,4,5,6,7,8,9),14]),mean(Output.model$mean$psi[c(7),c(1,2,3,4,5,6,7,8,9),14]),mean(Output.model$mean$psi[c(8),c(1,2,3,4,5,6,7,8,9),14]),
                             mean(Output.model$mean$psi[c(9),c(1,2,3,4,5,6,7,8,9),14]),mean(Output.model$mean$psi[c(10),c(1,2,3,4,5,6,7,8,9),14]),mean(Output.model$mean$psi[c(11),c(1,2),14]),
                             mean(Output.model$mean$psi[c(12),c(1,2),14]),mean(Output.model$mean$psi[c(13),c(1,2),14]),mean(Output.model$mean$psi[c(14),c(1,2),14]),
                             mean(Output.model$mean$psi[c(15),c(1,2),14]),mean(Output.model$mean$psi[c(16),c(1,2),14]),mean(Output.model$mean$psi[c(17),c(1,2),14]),
                             mean(Output.model$mean$psi[c(18),c(1,2),14]),mean(Output.model$mean$psi[c(19),c(1,2),14]))),
                      mean(c(mean(Output.model$mean$psi[c(2),c(1,2,3,4,5,6,7,8,9),11]),mean(Output.model$mean$psi[c(3),c(1,2,3,4,5,6,7,8,9),11]),mean(Output.model$mean$psi[c(4),c(1,2,3,4,5,6,7,8,9),11]),
                             mean(Output.model$mean$psi[c(6),c(1,2,3,4,5,6,7,8,9),11]),mean(Output.model$mean$psi[c(7),c(1,2,3,4,5,6,7,8,9),11]),mean(Output.model$mean$psi[c(8),c(1,2,3,4,5,6,7,8,9),11]),
                             mean(Output.model$mean$psi[c(9),c(1,2,3,4,5,6,7,8,9),11]),mean(Output.model$mean$psi[c(10),c(1,2,3,4,5,6,7,8,9),11]),mean(Output.model$mean$psi[c(11),c(1,2),11]),
                             mean(Output.model$mean$psi[c(12),c(1,2),11]),mean(Output.model$mean$psi[c(13),c(1,2),11]),mean(Output.model$mean$psi[c(14),c(1,2),11]),
                             mean(Output.model$mean$psi[c(15),c(1,2),11]),mean(Output.model$mean$psi[c(16),c(1,2),11]),mean(Output.model$mean$psi[c(17),c(1,2),11]),
                             mean(Output.model$mean$psi[c(18),c(1,2),11]),mean(Output.model$mean$psi[c(19),c(1,2),11]))),
                      mean(c(mean(Output.model$mean$psi[c(2),c(1,2,3,4,5,6,7,8,9),31]),mean(Output.model$mean$psi[c(3),c(1,2,3,4,5,6,7,8,9),31]),mean(Output.model$mean$psi[c(4),c(1,2,3,4,5,6,7,8,9),31]),
                             mean(Output.model$mean$psi[c(6),c(1,2,3,4,5,6,7,8,9),31]),mean(Output.model$mean$psi[c(7),c(1,2,3,4,5,6,7,8,9),31]),mean(Output.model$mean$psi[c(8),c(1,2,3,4,5,6,7,8,9),31]),
                             mean(Output.model$mean$psi[c(9),c(1,2,3,4,5,6,7,8,9),31]),mean(Output.model$mean$psi[c(10),c(1,2,3,4,5,6,7,8,9),31]),mean(Output.model$mean$psi[c(11),c(1,2),31]),
                             mean(Output.model$mean$psi[c(12),c(1,2),31]),mean(Output.model$mean$psi[c(13),c(1,2),31]),mean(Output.model$mean$psi[c(14),c(1,2),31]),
                             mean(Output.model$mean$psi[c(15),c(1,2),31]),mean(Output.model$mean$psi[c(16),c(1,2),31]),mean(Output.model$mean$psi[c(17),c(1,2),31]),
                             mean(Output.model$mean$psi[c(18),c(1,2),31]),mean(Output.model$mean$psi[c(19),c(1,2),31]))),
                      mean(c(mean(Output.model$mean$psi[c(2),c(1,2,3,4,5,6,7,8,9),6]),mean(Output.model$mean$psi[c(3),c(1,2,3,4,5,6,7,8,9),6]),mean(Output.model$mean$psi[c(4),c(1,2,3,4,5,6,7,8,9),6]),
                             mean(Output.model$mean$psi[c(6),c(1,2,3,4,5,6,7,8,9),6]),mean(Output.model$mean$psi[c(7),c(1,2,3,4,5,6,7,8,9),6]),mean(Output.model$mean$psi[c(8),c(1,2,3,4,5,6,7,8,9),6]),
                             mean(Output.model$mean$psi[c(9),c(1,2,3,4,5,6,7,8,9),6]),mean(Output.model$mean$psi[c(10),c(1,2,3,4,5,6,7,8,9),6]),mean(Output.model$mean$psi[c(11),c(1,2),6]),
                             mean(Output.model$mean$psi[c(12),c(1,2),6]),mean(Output.model$mean$psi[c(13),c(1,2),6]),mean(Output.model$mean$psi[c(14),c(1,2),6]),
                             mean(Output.model$mean$psi[c(15),c(1,2),6]),mean(Output.model$mean$psi[c(16),c(1,2),6]),mean(Output.model$mean$psi[c(17),c(1,2),6]),
                             mean(Output.model$mean$psi[c(18),c(1,2),6]),mean(Output.model$mean$psi[c(19),c(1,2),6])))),
                    c(mean(c(mean(Output.model$q75$psi[c(2),c(1,2,3,4,5,6,7,8,9),45]),mean(Output.model$q75$psi[c(3),c(1,2,3,4,5,6,7,8,9),45]),mean(Output.model$q75$psi[c(4),c(1,2,3,4,5,6,7,8,9),45]),
                             mean(Output.model$q75$psi[c(6),c(1,2,3,4,5,6,7,8,9),45]),mean(Output.model$q75$psi[c(7),c(1,2,3,4,5,6,7,8,9),45]),mean(Output.model$q75$psi[c(8),c(1,2,3,4,5,6,7,8,9),45]),
                             mean(Output.model$q75$psi[c(9),c(1,2,3,4,5,6,7,8,9),45]),mean(Output.model$q75$psi[c(10),c(1,2,3,4,5,6,7,8,9),45]),mean(Output.model$q75$psi[c(11),c(1,2),45]),
                             mean(Output.model$q75$psi[c(12),c(1,2),45]),mean(Output.model$q75$psi[c(13),c(1,2),45]),mean(Output.model$q75$psi[c(14),c(1,2),45]),
                             mean(Output.model$q75$psi[c(15),c(1,2),45]),mean(Output.model$q75$psi[c(16),c(1,2),45]),mean(Output.model$q75$psi[c(17),c(1,2),45]),
                             mean(Output.model$q75$psi[c(18),c(1,2),45]),mean(Output.model$q75$psi[c(19),c(1,2),45]))),
                      mean(c(mean(Output.model$q75$psi[c(2),c(1,2,3,4,5,6,7,8,9),1]),mean(Output.model$q75$psi[c(3),c(1,2,3,4,5,6,7,8,9),1]),mean(Output.model$q75$psi[c(4),c(1,2,3,4,5,6,7,8,9),1]),
                             mean(Output.model$q75$psi[c(6),c(1,2,3,4,5,6,7,8,9),1]),mean(Output.model$q75$psi[c(7),c(1,2,3,4,5,6,7,8,9),1]),mean(Output.model$q75$psi[c(8),c(1,2,3,4,5,6,7,8,9),1]),
                             mean(Output.model$q75$psi[c(9),c(1,2,3,4,5,6,7,8,9),1]),mean(Output.model$q75$psi[c(10),c(1,2,3,4,5,6,7,8,9),1]),mean(Output.model$q75$psi[c(11),c(1,2),1]),
                             mean(Output.model$q75$psi[c(12),c(1,2),1]),mean(Output.model$q75$psi[c(13),c(1,2),1]),mean(Output.model$q75$psi[c(14),c(1,2),1]),
                             mean(Output.model$q75$psi[c(15),c(1,2),1]),mean(Output.model$q75$psi[c(16),c(1,2),1]),mean(Output.model$q75$psi[c(17),c(1,2),1]),
                             mean(Output.model$q75$psi[c(18),c(1,2),1]),mean(Output.model$q75$psi[c(19),c(1,2),1]))),
                      mean(c(mean(Output.model$q75$psi[c(2),c(1,2,3,4,5,6,7,8,9),15]),mean(Output.model$q75$psi[c(3),c(1,2,3,4,5,6,7,8,9),15]),mean(Output.model$q75$psi[c(4),c(1,2,3,4,5,6,7,8,9),15]),
                             mean(Output.model$q75$psi[c(6),c(1,2,3,4,5,6,7,8,9),15]),mean(Output.model$q75$psi[c(7),c(1,2,3,4,5,6,7,8,9),15]),mean(Output.model$q75$psi[c(8),c(1,2,3,4,5,6,7,8,9),15]),
                             mean(Output.model$q75$psi[c(9),c(1,2,3,4,5,6,7,8,9),15]),mean(Output.model$q75$psi[c(10),c(1,2,3,4,5,6,7,8,9),15]),mean(Output.model$q75$psi[c(11),c(1,2),15]),
                             mean(Output.model$q75$psi[c(12),c(1,2),15]),mean(Output.model$q75$psi[c(13),c(1,2),15]),mean(Output.model$q75$psi[c(14),c(1,2),15]),
                             mean(Output.model$q75$psi[c(15),c(1,2),15]),mean(Output.model$q75$psi[c(16),c(1,2),15]),mean(Output.model$q75$psi[c(17),c(1,2),15]),
                             mean(Output.model$q75$psi[c(18),c(1,2),15]),mean(Output.model$q75$psi[c(19),c(1,2),15]))),
                      mean(c(mean(Output.model$q75$psi[c(2),c(1,2,3,4,5,6,7,8,9),7]),mean(Output.model$q75$psi[c(3),c(1,2,3,4,5,6,7,8,9),7]),mean(Output.model$q75$psi[c(4),c(1,2,3,4,5,6,7,8,9),7]),
                             mean(Output.model$q75$psi[c(6),c(1,2,3,4,5,6,7,8,9),7]),mean(Output.model$q75$psi[c(7),c(1,2,3,4,5,6,7,8,9),7]),mean(Output.model$q75$psi[c(8),c(1,2,3,4,5,6,7,8,9),7]),
                             mean(Output.model$q75$psi[c(9),c(1,2,3,4,5,6,7,8,9),7]),mean(Output.model$q75$psi[c(10),c(1,2,3,4,5,6,7,8,9),7]),mean(Output.model$q75$psi[c(11),c(1,2),7]),
                             mean(Output.model$q75$psi[c(12),c(1,2),7]),mean(Output.model$q75$psi[c(13),c(1,2),7]),mean(Output.model$q75$psi[c(14),c(1,2),7]),
                             mean(Output.model$q75$psi[c(15),c(1,2),7]),mean(Output.model$q75$psi[c(16),c(1,2),7]),mean(Output.model$q75$psi[c(17),c(1,2),7]),
                             mean(Output.model$q75$psi[c(18),c(1,2),7]),mean(Output.model$q75$psi[c(19),c(1,2),7]))),
                      mean(c(mean(Output.model$q75$psi[c(2),c(1,2,3,4,5,6,7,8,9),22]),mean(Output.model$q75$psi[c(3),c(1,2,3,4,5,6,7,8,9),22]),mean(Output.model$q75$psi[c(4),c(1,2,3,4,5,6,7,8,9),22]),
                             mean(Output.model$q75$psi[c(6),c(1,2,3,4,5,6,7,8,9),22]),mean(Output.model$q75$psi[c(7),c(1,2,3,4,5,6,7,8,9),22]),mean(Output.model$q75$psi[c(8),c(1,2,3,4,5,6,7,8,9),22]),
                             mean(Output.model$q75$psi[c(9),c(1,2,3,4,5,6,7,8,9),22]),mean(Output.model$q75$psi[c(10),c(1,2,3,4,5,6,7,8,9),22]),mean(Output.model$q75$psi[c(11),c(1,2),22]),
                             mean(Output.model$q75$psi[c(12),c(1,2),22]),mean(Output.model$q75$psi[c(13),c(1,2),22]),mean(Output.model$q75$psi[c(14),c(1,2),22]),
                             mean(Output.model$q75$psi[c(15),c(1,2),22]),mean(Output.model$q75$psi[c(16),c(1,2),22]),mean(Output.model$q75$psi[c(17),c(1,2),22]),
                             mean(Output.model$q75$psi[c(18),c(1,2),22]),mean(Output.model$q75$psi[c(19),c(1,2),22]))),
                      mean(c(mean(Output.model$q75$psi[c(2),c(1,2,3,4,5,6,7,8,9),29]),mean(Output.model$q75$psi[c(3),c(1,2,3,4,5,6,7,8,9),29]),mean(Output.model$q75$psi[c(4),c(1,2,3,4,5,6,7,8,9),29]),
                             mean(Output.model$q75$psi[c(6),c(1,2,3,4,5,6,7,8,9),29]),mean(Output.model$q75$psi[c(7),c(1,2,3,4,5,6,7,8,9),29]),mean(Output.model$q75$psi[c(8),c(1,2,3,4,5,6,7,8,9),29]),
                             mean(Output.model$q75$psi[c(9),c(1,2,3,4,5,6,7,8,9),29]),mean(Output.model$q75$psi[c(10),c(1,2,3,4,5,6,7,8,9),29]),mean(Output.model$q75$psi[c(11),c(1,2),29]),
                             mean(Output.model$q75$psi[c(12),c(1,2),29]),mean(Output.model$q75$psi[c(13),c(1,2),29]),mean(Output.model$q75$psi[c(14),c(1,2),29]),
                             mean(Output.model$q75$psi[c(15),c(1,2),29]),mean(Output.model$q75$psi[c(16),c(1,2),29]),mean(Output.model$q75$psi[c(17),c(1,2),29]),
                             mean(Output.model$q75$psi[c(18),c(1,2),29]),mean(Output.model$q75$psi[c(19),c(1,2),29]))),
                      mean(c(mean(Output.model$q75$psi[c(2),c(1,2,3,4,5,6,7,8,9),18]),mean(Output.model$q75$psi[c(3),c(1,2,3,4,5,6,7,8,9),18]),mean(Output.model$q75$psi[c(4),c(1,2,3,4,5,6,7,8,9),18]),
                             mean(Output.model$q75$psi[c(6),c(1,2,3,4,5,6,7,8,9),18]),mean(Output.model$q75$psi[c(7),c(1,2,3,4,5,6,7,8,9),18]),mean(Output.model$q75$psi[c(8),c(1,2,3,4,5,6,7,8,9),18]),
                             mean(Output.model$q75$psi[c(9),c(1,2,3,4,5,6,7,8,9),18]),mean(Output.model$q75$psi[c(10),c(1,2,3,4,5,6,7,8,9),18]),mean(Output.model$q75$psi[c(11),c(1,2),18]),
                             mean(Output.model$q75$psi[c(12),c(1,2),18]),mean(Output.model$q75$psi[c(13),c(1,2),18]),mean(Output.model$q75$psi[c(14),c(1,2),18]),
                             mean(Output.model$q75$psi[c(15),c(1,2),18]),mean(Output.model$q75$psi[c(16),c(1,2),18]),mean(Output.model$q75$psi[c(17),c(1,2),18]),
                             mean(Output.model$q75$psi[c(18),c(1,2),18]),mean(Output.model$q75$psi[c(19),c(1,2),18]))),
                      mean(c(mean(Output.model$q75$psi[c(2),c(1,2,3,4,5,6,7,8,9),33]),mean(Output.model$q75$psi[c(3),c(1,2,3,4,5,6,7,8,9),33]),mean(Output.model$q75$psi[c(4),c(1,2,3,4,5,6,7,8,9),33]),
                             mean(Output.model$q75$psi[c(6),c(1,2,3,4,5,6,7,8,9),33]),mean(Output.model$q75$psi[c(7),c(1,2,3,4,5,6,7,8,9),33]),mean(Output.model$q75$psi[c(8),c(1,2,3,4,5,6,7,8,9),33]),
                             mean(Output.model$q75$psi[c(9),c(1,2,3,4,5,6,7,8,9),33]),mean(Output.model$q75$psi[c(10),c(1,2,3,4,5,6,7,8,9),33]),mean(Output.model$q75$psi[c(11),c(1,2),33]),
                             mean(Output.model$q75$psi[c(12),c(1,2),33]),mean(Output.model$q75$psi[c(13),c(1,2),33]),mean(Output.model$q75$psi[c(14),c(1,2),33]),
                             mean(Output.model$q75$psi[c(15),c(1,2),33]),mean(Output.model$q75$psi[c(16),c(1,2),33]),mean(Output.model$q75$psi[c(17),c(1,2),33]),
                             mean(Output.model$q75$psi[c(18),c(1,2),33]),mean(Output.model$q75$psi[c(19),c(1,2),33]))),
                      mean(c(mean(Output.model$q75$psi[c(2),c(1,2,3,4,5,6,7,8,9),12]),mean(Output.model$q75$psi[c(3),c(1,2,3,4,5,6,7,8,9),12]),mean(Output.model$q75$psi[c(4),c(1,2,3,4,5,6,7,8,9),12]),
                             mean(Output.model$q75$psi[c(6),c(1,2,3,4,5,6,7,8,9),12]),mean(Output.model$q75$psi[c(7),c(1,2,3,4,5,6,7,8,9),12]),mean(Output.model$q75$psi[c(8),c(1,2,3,4,5,6,7,8,9),12]),
                             mean(Output.model$q75$psi[c(9),c(1,2,3,4,5,6,7,8,9),12]),mean(Output.model$q75$psi[c(10),c(1,2,3,4,5,6,7,8,9),12]),mean(Output.model$q75$psi[c(11),c(1,2),12]),
                             mean(Output.model$q75$psi[c(12),c(1,2),12]),mean(Output.model$q75$psi[c(13),c(1,2),12]),mean(Output.model$q75$psi[c(14),c(1,2),12]),
                             mean(Output.model$q75$psi[c(15),c(1,2),12]),mean(Output.model$q75$psi[c(16),c(1,2),12]),mean(Output.model$q75$psi[c(17),c(1,2),12]),
                             mean(Output.model$q75$psi[c(18),c(1,2),12]),mean(Output.model$q75$psi[c(19),c(1,2),12]))),
                      mean(c(mean(Output.model$q75$psi[c(2),c(1,2,3,4,5,6,7,8,9),9]),mean(Output.model$q75$psi[c(3),c(1,2,3,4,5,6,7,8,9),9]),mean(Output.model$q75$psi[c(4),c(1,2,3,4,5,6,7,8,9),9]),
                             mean(Output.model$q75$psi[c(6),c(1,2,3,4,5,6,7,8,9),9]),mean(Output.model$q75$psi[c(7),c(1,2,3,4,5,6,7,8,9),9]),mean(Output.model$q75$psi[c(8),c(1,2,3,4,5,6,7,8,9),9]),
                             mean(Output.model$q75$psi[c(9),c(1,2,3,4,5,6,7,8,9),9]),mean(Output.model$q75$psi[c(10),c(1,2,3,4,5,6,7,8,9),9]),mean(Output.model$q75$psi[c(11),c(1,2),9]),
                             mean(Output.model$q75$psi[c(12),c(1,2),9]),mean(Output.model$q75$psi[c(13),c(1,2),9]),mean(Output.model$q75$psi[c(14),c(1,2),9]),
                             mean(Output.model$q75$psi[c(15),c(1,2),9]),mean(Output.model$q75$psi[c(16),c(1,2),9]),mean(Output.model$q75$psi[c(17),c(1,2),9]),
                             mean(Output.model$q75$psi[c(18),c(1,2),9]),mean(Output.model$q75$psi[c(19),c(1,2),9]))),
                      mean(c(mean(Output.model$q75$psi[c(2),c(1,2,3,4,5,6,7,8,9),47]),mean(Output.model$q75$psi[c(3),c(1,2,3,4,5,6,7,8,9),47]),mean(Output.model$q75$psi[c(4),c(1,2,3,4,5,6,7,8,9),47]),
                             mean(Output.model$q75$psi[c(6),c(1,2,3,4,5,6,7,8,9),47]),mean(Output.model$q75$psi[c(7),c(1,2,3,4,5,6,7,8,9),47]),mean(Output.model$q75$psi[c(8),c(1,2,3,4,5,6,7,8,9),47]),
                             mean(Output.model$q75$psi[c(9),c(1,2,3,4,5,6,7,8,9),47]),mean(Output.model$q75$psi[c(10),c(1,2,3,4,5,6,7,8,9),47]),mean(Output.model$q75$psi[c(11),c(1,2),47]),
                             mean(Output.model$q75$psi[c(12),c(1,2),47]),mean(Output.model$q75$psi[c(13),c(1,2),47]),mean(Output.model$q75$psi[c(14),c(1,2),47]),
                             mean(Output.model$q75$psi[c(15),c(1,2),47]),mean(Output.model$q75$psi[c(16),c(1,2),47]),mean(Output.model$q75$psi[c(17),c(1,2),47]),
                             mean(Output.model$q75$psi[c(18),c(1,2),47]),mean(Output.model$q75$psi[c(19),c(1,2),47]))),
                      mean(c(mean(Output.model$q75$psi[c(2),c(1,2,3,4,5,6,7,8,9),30]),mean(Output.model$q75$psi[c(3),c(1,2,3,4,5,6,7,8,9),30]),mean(Output.model$q75$psi[c(4),c(1,2,3,4,5,6,7,8,9),30]),
                             mean(Output.model$q75$psi[c(6),c(1,2,3,4,5,6,7,8,9),30]),mean(Output.model$q75$psi[c(7),c(1,2,3,4,5,6,7,8,9),30]),mean(Output.model$q75$psi[c(8),c(1,2,3,4,5,6,7,8,9),30]),
                             mean(Output.model$q75$psi[c(9),c(1,2,3,4,5,6,7,8,9),30]),mean(Output.model$q75$psi[c(10),c(1,2,3,4,5,6,7,8,9),30]),mean(Output.model$q75$psi[c(11),c(1,2),30]),
                             mean(Output.model$q75$psi[c(12),c(1,2),30]),mean(Output.model$q75$psi[c(13),c(1,2),30]),mean(Output.model$q75$psi[c(14),c(1,2),30]),
                             mean(Output.model$q75$psi[c(15),c(1,2),30]),mean(Output.model$q75$psi[c(16),c(1,2),30]),mean(Output.model$q75$psi[c(17),c(1,2),30]),
                             mean(Output.model$q75$psi[c(18),c(1,2),30]),mean(Output.model$q75$psi[c(19),c(1,2),30]))),
                      mean(c(mean(Output.model$q75$psi[c(2),c(1,2,3,4,5,6,7,8,9),40]),mean(Output.model$q75$psi[c(3),c(1,2,3,4,5,6,7,8,9),40]),mean(Output.model$q75$psi[c(4),c(1,2,3,4,5,6,7,8,9),40]),
                             mean(Output.model$q75$psi[c(6),c(1,2,3,4,5,6,7,8,9),40]),mean(Output.model$q75$psi[c(7),c(1,2,3,4,5,6,7,8,9),40]),mean(Output.model$q75$psi[c(8),c(1,2,3,4,5,6,7,8,9),40]),
                             mean(Output.model$q75$psi[c(9),c(1,2,3,4,5,6,7,8,9),40]),mean(Output.model$q75$psi[c(10),c(1,2,3,4,5,6,7,8,9),40]),mean(Output.model$q75$psi[c(11),c(1,2),40]),
                             mean(Output.model$q75$psi[c(12),c(1,2),40]),mean(Output.model$q75$psi[c(13),c(1,2),40]),mean(Output.model$q75$psi[c(14),c(1,2),40]),
                             mean(Output.model$q75$psi[c(15),c(1,2),40]),mean(Output.model$q75$psi[c(16),c(1,2),40]),mean(Output.model$q75$psi[c(17),c(1,2),40]),
                             mean(Output.model$q75$psi[c(18),c(1,2),40]),mean(Output.model$q75$psi[c(19),c(1,2),40]))),
                      mean(c(mean(Output.model$q75$psi[c(2),c(1,2,3,4,5,6,7,8,9),5]),mean(Output.model$q75$psi[c(3),c(1,2,3,4,5,6,7,8,9),5]),mean(Output.model$q75$psi[c(4),c(1,2,3,4,5,6,7,8,9),5]),
                             mean(Output.model$q75$psi[c(6),c(1,2,3,4,5,6,7,8,9),5]),mean(Output.model$q75$psi[c(7),c(1,2,3,4,5,6,7,8,9),5]),mean(Output.model$q75$psi[c(8),c(1,2,3,4,5,6,7,8,9),5]),
                             mean(Output.model$q75$psi[c(9),c(1,2,3,4,5,6,7,8,9),5]),mean(Output.model$q75$psi[c(10),c(1,2,3,4,5,6,7,8,9),5]),mean(Output.model$q75$psi[c(11),c(1,2),5]),
                             mean(Output.model$q75$psi[c(12),c(1,2),5]),mean(Output.model$q75$psi[c(13),c(1,2),5]),mean(Output.model$q75$psi[c(14),c(1,2),5]),
                             mean(Output.model$q75$psi[c(15),c(1,2),5]),mean(Output.model$q75$psi[c(16),c(1,2),5]),mean(Output.model$q75$psi[c(17),c(1,2),5]),
                             mean(Output.model$q75$psi[c(18),c(1,2),5]),mean(Output.model$q75$psi[c(19),c(1,2),5]))),
                      mean(c(mean(Output.model$q75$psi[c(2),c(1,2,3,4,5,6,7,8,9),20]),mean(Output.model$q75$psi[c(3),c(1,2,3,4,5,6,7,8,9),20]),mean(Output.model$q75$psi[c(4),c(1,2,3,4,5,6,7,8,9),20]),
                             mean(Output.model$q75$psi[c(6),c(1,2,3,4,5,6,7,8,9),20]),mean(Output.model$q75$psi[c(7),c(1,2,3,4,5,6,7,8,9),20]),mean(Output.model$q75$psi[c(8),c(1,2,3,4,5,6,7,8,9),20]),
                             mean(Output.model$q75$psi[c(9),c(1,2,3,4,5,6,7,8,9),20]),mean(Output.model$q75$psi[c(10),c(1,2,3,4,5,6,7,8,9),20]),mean(Output.model$q75$psi[c(11),c(1,2),20]),
                             mean(Output.model$q75$psi[c(12),c(1,2),20]),mean(Output.model$q75$psi[c(13),c(1,2),20]),mean(Output.model$q75$psi[c(14),c(1,2),20]),
                             mean(Output.model$q75$psi[c(15),c(1,2),20]),mean(Output.model$q75$psi[c(16),c(1,2),20]),mean(Output.model$q75$psi[c(17),c(1,2),20]),
                             mean(Output.model$q75$psi[c(18),c(1,2),20]),mean(Output.model$q75$psi[c(19),c(1,2),20]))),
                      mean(c(mean(Output.model$q75$psi[c(2),c(1,2,3,4,5,6,7,8,9),23]),mean(Output.model$q75$psi[c(3),c(1,2,3,4,5,6,7,8,9),23]),mean(Output.model$q75$psi[c(4),c(1,2,3,4,5,6,7,8,9),23]),
                             mean(Output.model$q75$psi[c(6),c(1,2,3,4,5,6,7,8,9),23]),mean(Output.model$q75$psi[c(7),c(1,2,3,4,5,6,7,8,9),23]),mean(Output.model$q75$psi[c(8),c(1,2,3,4,5,6,7,8,9),23]),
                             mean(Output.model$q75$psi[c(9),c(1,2,3,4,5,6,7,8,9),23]),mean(Output.model$q75$psi[c(10),c(1,2,3,4,5,6,7,8,9),23]),mean(Output.model$q75$psi[c(11),c(1,2),23]),
                             mean(Output.model$q75$psi[c(12),c(1,2),23]),mean(Output.model$q75$psi[c(13),c(1,2),23]),mean(Output.model$q75$psi[c(14),c(1,2),23]),
                             mean(Output.model$q75$psi[c(15),c(1,2),23]),mean(Output.model$q75$psi[c(16),c(1,2),23]),mean(Output.model$q75$psi[c(17),c(1,2),23]),
                             mean(Output.model$q75$psi[c(18),c(1,2),23]),mean(Output.model$q75$psi[c(19),c(1,2),23]))),
                      mean(c(mean(Output.model$q75$psi[c(2),c(1,2,3,4,5,6,7,8,9),3]),mean(Output.model$q75$psi[c(3),c(1,2,3,4,5,6,7,8,9),3]),mean(Output.model$q75$psi[c(4),c(1,2,3,4,5,6,7,8,9),3]),
                             mean(Output.model$q75$psi[c(6),c(1,2,3,4,5,6,7,8,9),3]),mean(Output.model$q75$psi[c(7),c(1,2,3,4,5,6,7,8,9),3]),mean(Output.model$q75$psi[c(8),c(1,2,3,4,5,6,7,8,9),3]),
                             mean(Output.model$q75$psi[c(9),c(1,2,3,4,5,6,7,8,9),3]),mean(Output.model$q75$psi[c(10),c(1,2,3,4,5,6,7,8,9),3]),mean(Output.model$q75$psi[c(11),c(1,2),3]),
                             mean(Output.model$q75$psi[c(12),c(1,2),3]),mean(Output.model$q75$psi[c(13),c(1,2),3]),mean(Output.model$q75$psi[c(14),c(1,2),3]),
                             mean(Output.model$q75$psi[c(15),c(1,2),3]),mean(Output.model$q75$psi[c(16),c(1,2),3]),mean(Output.model$q75$psi[c(17),c(1,2),3]),
                             mean(Output.model$q75$psi[c(18),c(1,2),3]),mean(Output.model$q75$psi[c(19),c(1,2),3]))),
                      mean(c(mean(Output.model$q75$psi[c(2),c(1,2,3,4,5,6,7,8,9),27]),mean(Output.model$q75$psi[c(3),c(1,2,3,4,5,6,7,8,9),27]),mean(Output.model$q75$psi[c(4),c(1,2,3,4,5,6,7,8,9),27]),
                             mean(Output.model$q75$psi[c(6),c(1,2,3,4,5,6,7,8,9),27]),mean(Output.model$q75$psi[c(7),c(1,2,3,4,5,6,7,8,9),27]),mean(Output.model$q75$psi[c(8),c(1,2,3,4,5,6,7,8,9),27]),
                             mean(Output.model$q75$psi[c(9),c(1,2,3,4,5,6,7,8,9),27]),mean(Output.model$q75$psi[c(10),c(1,2,3,4,5,6,7,8,9),27]),mean(Output.model$q75$psi[c(11),c(1,2),27]),
                             mean(Output.model$q75$psi[c(12),c(1,2),27]),mean(Output.model$q75$psi[c(13),c(1,2),27]),mean(Output.model$q75$psi[c(14),c(1,2),27]),
                             mean(Output.model$q75$psi[c(15),c(1,2),27]),mean(Output.model$q75$psi[c(16),c(1,2),27]),mean(Output.model$q75$psi[c(17),c(1,2),27]),
                             mean(Output.model$q75$psi[c(18),c(1,2),27]),mean(Output.model$q75$psi[c(19),c(1,2),27]))),
                      mean(c(mean(Output.model$q75$psi[c(2),c(1,2,3,4,5,6,7,8,9),26]),mean(Output.model$q75$psi[c(3),c(1,2,3,4,5,6,7,8,9),26]),mean(Output.model$q75$psi[c(4),c(1,2,3,4,5,6,7,8,9),26]),
                             mean(Output.model$q75$psi[c(6),c(1,2,3,4,5,6,7,8,9),26]),mean(Output.model$q75$psi[c(7),c(1,2,3,4,5,6,7,8,9),26]),mean(Output.model$q75$psi[c(8),c(1,2,3,4,5,6,7,8,9),26]),
                             mean(Output.model$q75$psi[c(9),c(1,2,3,4,5,6,7,8,9),26]),mean(Output.model$q75$psi[c(10),c(1,2,3,4,5,6,7,8,9),26]),mean(Output.model$q75$psi[c(11),c(1,2),26]),
                             mean(Output.model$q75$psi[c(12),c(1,2),26]),mean(Output.model$q75$psi[c(13),c(1,2),26]),mean(Output.model$q75$psi[c(14),c(1,2),26]),
                             mean(Output.model$q75$psi[c(15),c(1,2),26]),mean(Output.model$q75$psi[c(16),c(1,2),26]),mean(Output.model$q75$psi[c(17),c(1,2),26]),
                             mean(Output.model$q75$psi[c(18),c(1,2),26]),mean(Output.model$q75$psi[c(19),c(1,2),26]))),
                      mean(c(mean(Output.model$q75$psi[c(2),c(1,2,3,4,5,6,7,8,9),44]),mean(Output.model$q75$psi[c(3),c(1,2,3,4,5,6,7,8,9),44]),mean(Output.model$q75$psi[c(4),c(1,2,3,4,5,6,7,8,9),44]),
                             mean(Output.model$q75$psi[c(6),c(1,2,3,4,5,6,7,8,9),44]),mean(Output.model$q75$psi[c(7),c(1,2,3,4,5,6,7,8,9),44]),mean(Output.model$q75$psi[c(8),c(1,2,3,4,5,6,7,8,9),44]),
                             mean(Output.model$q75$psi[c(9),c(1,2,3,4,5,6,7,8,9),44]),mean(Output.model$q75$psi[c(10),c(1,2,3,4,5,6,7,8,9),44]),mean(Output.model$q75$psi[c(11),c(1,2),44]),
                             mean(Output.model$q75$psi[c(12),c(1,2),44]),mean(Output.model$q75$psi[c(13),c(1,2),44]),mean(Output.model$q75$psi[c(14),c(1,2),44]),
                             mean(Output.model$q75$psi[c(15),c(1,2),44]),mean(Output.model$q75$psi[c(16),c(1,2),44]),mean(Output.model$q75$psi[c(17),c(1,2),44]),
                             mean(Output.model$q75$psi[c(18),c(1,2),44]),mean(Output.model$q75$psi[c(19),c(1,2),44]))),
                      mean(c(mean(Output.model$q75$psi[c(2),c(1,2,3,4,5,6,7,8,9),42]),mean(Output.model$q75$psi[c(3),c(1,2,3,4,5,6,7,8,9),42]),mean(Output.model$q75$psi[c(4),c(1,2,3,4,5,6,7,8,9),42]),
                             mean(Output.model$q75$psi[c(6),c(1,2,3,4,5,6,7,8,9),42]),mean(Output.model$q75$psi[c(7),c(1,2,3,4,5,6,7,8,9),42]),mean(Output.model$q75$psi[c(8),c(1,2,3,4,5,6,7,8,9),42]),
                             mean(Output.model$q75$psi[c(9),c(1,2,3,4,5,6,7,8,9),42]),mean(Output.model$q75$psi[c(10),c(1,2,3,4,5,6,7,8,9),42]),mean(Output.model$q75$psi[c(11),c(1,2),42]),
                             mean(Output.model$q75$psi[c(12),c(1,2),42]),mean(Output.model$q75$psi[c(13),c(1,2),42]),mean(Output.model$q75$psi[c(14),c(1,2),42]),
                             mean(Output.model$q75$psi[c(15),c(1,2),42]),mean(Output.model$q75$psi[c(16),c(1,2),42]),mean(Output.model$q75$psi[c(17),c(1,2),42]),
                             mean(Output.model$q75$psi[c(18),c(1,2),42]),mean(Output.model$q75$psi[c(19),c(1,2),42]))),
                      mean(c(mean(Output.model$q75$psi[c(2),c(1,2,3,4,5,6,7,8,9),43]),mean(Output.model$q75$psi[c(3),c(1,2,3,4,5,6,7,8,9),43]),mean(Output.model$q75$psi[c(4),c(1,2,3,4,5,6,7,8,9),43]),
                             mean(Output.model$q75$psi[c(6),c(1,2,3,4,5,6,7,8,9),43]),mean(Output.model$q75$psi[c(7),c(1,2,3,4,5,6,7,8,9),43]),mean(Output.model$q75$psi[c(8),c(1,2,3,4,5,6,7,8,9),43]),
                             mean(Output.model$q75$psi[c(9),c(1,2,3,4,5,6,7,8,9),43]),mean(Output.model$q75$psi[c(10),c(1,2,3,4,5,6,7,8,9),43]),mean(Output.model$q75$psi[c(11),c(1,2),43]),
                             mean(Output.model$q75$psi[c(12),c(1,2),43]),mean(Output.model$q75$psi[c(13),c(1,2),43]),mean(Output.model$q75$psi[c(14),c(1,2),43]),
                             mean(Output.model$q75$psi[c(15),c(1,2),43]),mean(Output.model$q75$psi[c(16),c(1,2),43]),mean(Output.model$q75$psi[c(17),c(1,2),43]),
                             mean(Output.model$q75$psi[c(18),c(1,2),43]),mean(Output.model$q75$psi[c(19),c(1,2),43]))),
                      mean(c(mean(Output.model$q75$psi[c(2),c(1,2,3,4,5,6,7,8,9),38]),mean(Output.model$q75$psi[c(3),c(1,2,3,4,5,6,7,8,9),38]),mean(Output.model$q75$psi[c(4),c(1,2,3,4,5,6,7,8,9),38]),
                             mean(Output.model$q75$psi[c(6),c(1,2,3,4,5,6,7,8,9),38]),mean(Output.model$q75$psi[c(7),c(1,2,3,4,5,6,7,8,9),38]),mean(Output.model$q75$psi[c(8),c(1,2,3,4,5,6,7,8,9),38]),
                             mean(Output.model$q75$psi[c(9),c(1,2,3,4,5,6,7,8,9),38]),mean(Output.model$q75$psi[c(10),c(1,2,3,4,5,6,7,8,9),38]),mean(Output.model$q75$psi[c(11),c(1,2),38]),
                             mean(Output.model$q75$psi[c(12),c(1,2),38]),mean(Output.model$q75$psi[c(13),c(1,2),38]),mean(Output.model$q75$psi[c(14),c(1,2),38]),
                             mean(Output.model$q75$psi[c(15),c(1,2),38]),mean(Output.model$q75$psi[c(16),c(1,2),38]),mean(Output.model$q75$psi[c(17),c(1,2),38]),
                             mean(Output.model$q75$psi[c(18),c(1,2),38]),mean(Output.model$q75$psi[c(19),c(1,2),38]))),
                      mean(c(mean(Output.model$q75$psi[c(2),c(1,2,3,4,5,6,7,8,9),24]),mean(Output.model$q75$psi[c(3),c(1,2,3,4,5,6,7,8,9),24]),mean(Output.model$q75$psi[c(4),c(1,2,3,4,5,6,7,8,9),24]),
                             mean(Output.model$q75$psi[c(6),c(1,2,3,4,5,6,7,8,9),24]),mean(Output.model$q75$psi[c(7),c(1,2,3,4,5,6,7,8,9),24]),mean(Output.model$q75$psi[c(8),c(1,2,3,4,5,6,7,8,9),24]),
                             mean(Output.model$q75$psi[c(9),c(1,2,3,4,5,6,7,8,9),24]),mean(Output.model$q75$psi[c(10),c(1,2,3,4,5,6,7,8,9),24]),mean(Output.model$q75$psi[c(11),c(1,2),24]),
                             mean(Output.model$q75$psi[c(12),c(1,2),24]),mean(Output.model$q75$psi[c(13),c(1,2),24]),mean(Output.model$q75$psi[c(14),c(1,2),24]),
                             mean(Output.model$q75$psi[c(15),c(1,2),24]),mean(Output.model$q75$psi[c(16),c(1,2),24]),mean(Output.model$q75$psi[c(17),c(1,2),24]),
                             mean(Output.model$q75$psi[c(18),c(1,2),24]),mean(Output.model$q75$psi[c(19),c(1,2),24]))),
                      mean(c(mean(Output.model$q75$psi[c(2),c(1,2,3,4,5,6,7,8,9),37]),mean(Output.model$q75$psi[c(3),c(1,2,3,4,5,6,7,8,9),37]),mean(Output.model$q75$psi[c(4),c(1,2,3,4,5,6,7,8,9),37]),
                             mean(Output.model$q75$psi[c(6),c(1,2,3,4,5,6,7,8,9),37]),mean(Output.model$q75$psi[c(7),c(1,2,3,4,5,6,7,8,9),37]),mean(Output.model$q75$psi[c(8),c(1,2,3,4,5,6,7,8,9),37]),
                             mean(Output.model$q75$psi[c(9),c(1,2,3,4,5,6,7,8,9),37]),mean(Output.model$q75$psi[c(10),c(1,2,3,4,5,6,7,8,9),37]),mean(Output.model$q75$psi[c(11),c(1,2),37]),
                             mean(Output.model$q75$psi[c(12),c(1,2),37]),mean(Output.model$q75$psi[c(13),c(1,2),37]),mean(Output.model$q75$psi[c(14),c(1,2),37]),
                             mean(Output.model$q75$psi[c(15),c(1,2),37]),mean(Output.model$q75$psi[c(16),c(1,2),37]),mean(Output.model$q75$psi[c(17),c(1,2),37]),
                             mean(Output.model$q75$psi[c(18),c(1,2),37]),mean(Output.model$q75$psi[c(19),c(1,2),37]))),
                      mean(c(mean(Output.model$q75$psi[c(2),c(1,2,3,4,5,6,7,8,9),13]),mean(Output.model$q75$psi[c(3),c(1,2,3,4,5,6,7,8,9),13]),mean(Output.model$q75$psi[c(4),c(1,2,3,4,5,6,7,8,9),13]),
                             mean(Output.model$q75$psi[c(6),c(1,2,3,4,5,6,7,8,9),13]),mean(Output.model$q75$psi[c(7),c(1,2,3,4,5,6,7,8,9),13]),mean(Output.model$q75$psi[c(8),c(1,2,3,4,5,6,7,8,9),13]),
                             mean(Output.model$q75$psi[c(9),c(1,2,3,4,5,6,7,8,9),13]),mean(Output.model$q75$psi[c(10),c(1,2,3,4,5,6,7,8,9),13]),mean(Output.model$q75$psi[c(11),c(1,2),13]),
                             mean(Output.model$q75$psi[c(12),c(1,2),13]),mean(Output.model$q75$psi[c(13),c(1,2),13]),mean(Output.model$q75$psi[c(14),c(1,2),13]),
                             mean(Output.model$q75$psi[c(15),c(1,2),13]),mean(Output.model$q75$psi[c(16),c(1,2),13]),mean(Output.model$q75$psi[c(17),c(1,2),13]),
                             mean(Output.model$q75$psi[c(18),c(1,2),13]),mean(Output.model$q75$psi[c(19),c(1,2),13]))),
                      mean(c(mean(Output.model$q75$psi[c(2),c(1,2,3,4,5,6,7,8,9),21]),mean(Output.model$q75$psi[c(3),c(1,2,3,4,5,6,7,8,9),21]),mean(Output.model$q75$psi[c(4),c(1,2,3,4,5,6,7,8,9),21]),
                             mean(Output.model$q75$psi[c(6),c(1,2,3,4,5,6,7,8,9),21]),mean(Output.model$q75$psi[c(7),c(1,2,3,4,5,6,7,8,9),21]),mean(Output.model$q75$psi[c(8),c(1,2,3,4,5,6,7,8,9),21]),
                             mean(Output.model$q75$psi[c(9),c(1,2,3,4,5,6,7,8,9),21]),mean(Output.model$q75$psi[c(10),c(1,2,3,4,5,6,7,8,9),21]),mean(Output.model$q75$psi[c(11),c(1,2),21]),
                             mean(Output.model$q75$psi[c(12),c(1,2),21]),mean(Output.model$q75$psi[c(13),c(1,2),21]),mean(Output.model$q75$psi[c(14),c(1,2),21]),
                             mean(Output.model$q75$psi[c(15),c(1,2),21]),mean(Output.model$q75$psi[c(16),c(1,2),21]),mean(Output.model$q75$psi[c(17),c(1,2),21]),
                             mean(Output.model$q75$psi[c(18),c(1,2),21]),mean(Output.model$q75$psi[c(19),c(1,2),21]))),
                      mean(c(mean(Output.model$q75$psi[c(2),c(1,2,3,4,5,6,7,8,9),34]),mean(Output.model$q75$psi[c(3),c(1,2,3,4,5,6,7,8,9),34]),mean(Output.model$q75$psi[c(4),c(1,2,3,4,5,6,7,8,9),34]),
                             mean(Output.model$q75$psi[c(6),c(1,2,3,4,5,6,7,8,9),34]),mean(Output.model$q75$psi[c(7),c(1,2,3,4,5,6,7,8,9),34]),mean(Output.model$q75$psi[c(8),c(1,2,3,4,5,6,7,8,9),34]),
                             mean(Output.model$q75$psi[c(9),c(1,2,3,4,5,6,7,8,9),34]),mean(Output.model$q75$psi[c(10),c(1,2,3,4,5,6,7,8,9),34]),mean(Output.model$q75$psi[c(11),c(1,2),34]),
                             mean(Output.model$q75$psi[c(12),c(1,2),34]),mean(Output.model$q75$psi[c(13),c(1,2),34]),mean(Output.model$q75$psi[c(14),c(1,2),34]),
                             mean(Output.model$q75$psi[c(15),c(1,2),34]),mean(Output.model$q75$psi[c(16),c(1,2),34]),mean(Output.model$q75$psi[c(17),c(1,2),34]),
                             mean(Output.model$q75$psi[c(18),c(1,2),34]),mean(Output.model$q75$psi[c(19),c(1,2),34]))),
                      mean(c(mean(Output.model$q75$psi[c(2),c(1,2,3,4,5,6,7,8,9),2]),mean(Output.model$q75$psi[c(3),c(1,2,3,4,5,6,7,8,9),2]),mean(Output.model$q75$psi[c(4),c(1,2,3,4,5,6,7,8,9),2]),
                             mean(Output.model$q75$psi[c(6),c(1,2,3,4,5,6,7,8,9),2]),mean(Output.model$q75$psi[c(7),c(1,2,3,4,5,6,7,8,9),2]),mean(Output.model$q75$psi[c(8),c(1,2,3,4,5,6,7,8,9),2]),
                             mean(Output.model$q75$psi[c(9),c(1,2,3,4,5,6,7,8,9),2]),mean(Output.model$q75$psi[c(10),c(1,2,3,4,5,6,7,8,9),2]),mean(Output.model$q75$psi[c(11),c(1,2),2]),
                             mean(Output.model$q75$psi[c(12),c(1,2),2]),mean(Output.model$q75$psi[c(13),c(1,2),2]),mean(Output.model$q75$psi[c(14),c(1,2),2]),
                             mean(Output.model$q75$psi[c(15),c(1,2),2]),mean(Output.model$q75$psi[c(16),c(1,2),2]),mean(Output.model$q75$psi[c(17),c(1,2),2]),
                             mean(Output.model$q75$psi[c(18),c(1,2),2]),mean(Output.model$q75$psi[c(19),c(1,2),2]))),
                      mean(c(mean(Output.model$q75$psi[c(2),c(1,2,3,4,5,6,7,8,9),32]),mean(Output.model$q75$psi[c(3),c(1,2,3,4,5,6,7,8,9),32]),mean(Output.model$q75$psi[c(4),c(1,2,3,4,5,6,7,8,9),32]),
                             mean(Output.model$q75$psi[c(6),c(1,2,3,4,5,6,7,8,9),32]),mean(Output.model$q75$psi[c(7),c(1,2,3,4,5,6,7,8,9),32]),mean(Output.model$q75$psi[c(8),c(1,2,3,4,5,6,7,8,9),32]),
                             mean(Output.model$q75$psi[c(9),c(1,2,3,4,5,6,7,8,9),32]),mean(Output.model$q75$psi[c(10),c(1,2,3,4,5,6,7,8,9),32]),mean(Output.model$q75$psi[c(11),c(1,2),32]),
                             mean(Output.model$q75$psi[c(12),c(1,2),32]),mean(Output.model$q75$psi[c(13),c(1,2),32]),mean(Output.model$q75$psi[c(14),c(1,2),32]),
                             mean(Output.model$q75$psi[c(15),c(1,2),32]),mean(Output.model$q75$psi[c(16),c(1,2),32]),mean(Output.model$q75$psi[c(17),c(1,2),32]),
                             mean(Output.model$q75$psi[c(18),c(1,2),32]),mean(Output.model$q75$psi[c(19),c(1,2),32]))),
                      mean(c(mean(Output.model$q75$psi[c(2),c(1,2,3,4,5,6,7,8,9),17]),mean(Output.model$q75$psi[c(3),c(1,2,3,4,5,6,7,8,9),17]),mean(Output.model$q75$psi[c(4),c(1,2,3,4,5,6,7,8,9),17]),
                             mean(Output.model$q75$psi[c(6),c(1,2,3,4,5,6,7,8,9),17]),mean(Output.model$q75$psi[c(7),c(1,2,3,4,5,6,7,8,9),17]),mean(Output.model$q75$psi[c(8),c(1,2,3,4,5,6,7,8,9),17]),
                             mean(Output.model$q75$psi[c(9),c(1,2,3,4,5,6,7,8,9),17]),mean(Output.model$q75$psi[c(10),c(1,2,3,4,5,6,7,8,9),17]),mean(Output.model$q75$psi[c(11),c(1,2),17]),
                             mean(Output.model$q75$psi[c(12),c(1,2),17]),mean(Output.model$q75$psi[c(13),c(1,2),17]),mean(Output.model$q75$psi[c(14),c(1,2),17]),
                             mean(Output.model$q75$psi[c(15),c(1,2),17]),mean(Output.model$q75$psi[c(16),c(1,2),17]),mean(Output.model$q75$psi[c(17),c(1,2),17]),
                             mean(Output.model$q75$psi[c(18),c(1,2),17]),mean(Output.model$q75$psi[c(19),c(1,2),17]))),
                      mean(c(mean(Output.model$q75$psi[c(2),c(1,2,3,4,5,6,7,8,9),4]),mean(Output.model$q75$psi[c(3),c(1,2,3,4,5,6,7,8,9),4]),mean(Output.model$q75$psi[c(4),c(1,2,3,4,5,6,7,8,9),4]),
                             mean(Output.model$q75$psi[c(6),c(1,2,3,4,5,6,7,8,9),4]),mean(Output.model$q75$psi[c(7),c(1,2,3,4,5,6,7,8,9),4]),mean(Output.model$q75$psi[c(8),c(1,2,3,4,5,6,7,8,9),4]),
                             mean(Output.model$q75$psi[c(9),c(1,2,3,4,5,6,7,8,9),4]),mean(Output.model$q75$psi[c(10),c(1,2,3,4,5,6,7,8,9),4]),mean(Output.model$q75$psi[c(11),c(1,2),4]),
                             mean(Output.model$q75$psi[c(12),c(1,2),4]),mean(Output.model$q75$psi[c(13),c(1,2),4]),mean(Output.model$q75$psi[c(14),c(1,2),4]),
                             mean(Output.model$q75$psi[c(15),c(1,2),4]),mean(Output.model$q75$psi[c(16),c(1,2),4]),mean(Output.model$q75$psi[c(17),c(1,2),4]),
                             mean(Output.model$q75$psi[c(18),c(1,2),4]),mean(Output.model$q75$psi[c(19),c(1,2),4]))),
                      mean(c(mean(Output.model$q75$psi[c(2),c(1,2,3,4,5,6,7,8,9),8]),mean(Output.model$q75$psi[c(3),c(1,2,3,4,5,6,7,8,9),8]),mean(Output.model$q75$psi[c(4),c(1,2,3,4,5,6,7,8,9),8]),
                             mean(Output.model$q75$psi[c(6),c(1,2,3,4,5,6,7,8,9),8]),mean(Output.model$q75$psi[c(7),c(1,2,3,4,5,6,7,8,9),8]),mean(Output.model$q75$psi[c(8),c(1,2,3,4,5,6,7,8,9),8]),
                             mean(Output.model$q75$psi[c(9),c(1,2,3,4,5,6,7,8,9),8]),mean(Output.model$q75$psi[c(10),c(1,2,3,4,5,6,7,8,9),8]),mean(Output.model$q75$psi[c(11),c(1,2),8]),
                             mean(Output.model$q75$psi[c(12),c(1,2),8]),mean(Output.model$q75$psi[c(13),c(1,2),8]),mean(Output.model$q75$psi[c(14),c(1,2),8]),
                             mean(Output.model$q75$psi[c(15),c(1,2),8]),mean(Output.model$q75$psi[c(16),c(1,2),8]),mean(Output.model$q75$psi[c(17),c(1,2),8]),
                             mean(Output.model$q75$psi[c(18),c(1,2),8]),mean(Output.model$q75$psi[c(19),c(1,2),8]))),
                      mean(c(mean(Output.model$q75$psi[c(2),c(1,2,3,4,5,6,7,8,9),39]),mean(Output.model$q75$psi[c(3),c(1,2,3,4,5,6,7,8,9),39]),mean(Output.model$q75$psi[c(4),c(1,2,3,4,5,6,7,8,9),39]),
                             mean(Output.model$q75$psi[c(6),c(1,2,3,4,5,6,7,8,9),39]),mean(Output.model$q75$psi[c(7),c(1,2,3,4,5,6,7,8,9),39]),mean(Output.model$q75$psi[c(8),c(1,2,3,4,5,6,7,8,9),39]),
                             mean(Output.model$q75$psi[c(9),c(1,2,3,4,5,6,7,8,9),39]),mean(Output.model$q75$psi[c(10),c(1,2,3,4,5,6,7,8,9),39]),mean(Output.model$q75$psi[c(11),c(1,2),39]),
                             mean(Output.model$q75$psi[c(12),c(1,2),39]),mean(Output.model$q75$psi[c(13),c(1,2),39]),mean(Output.model$q75$psi[c(14),c(1,2),39]),
                             mean(Output.model$q75$psi[c(15),c(1,2),39]),mean(Output.model$q75$psi[c(16),c(1,2),39]),mean(Output.model$q75$psi[c(17),c(1,2),39]),
                             mean(Output.model$q75$psi[c(18),c(1,2),39]),mean(Output.model$q75$psi[c(19),c(1,2),39]))),
                      mean(c(mean(Output.model$q75$psi[c(2),c(1,2,3,4,5,6,7,8,9),10]),mean(Output.model$q75$psi[c(3),c(1,2,3,4,5,6,7,8,9),10]),mean(Output.model$q75$psi[c(4),c(1,2,3,4,5,6,7,8,9),10]),
                             mean(Output.model$q75$psi[c(6),c(1,2,3,4,5,6,7,8,9),10]),mean(Output.model$q75$psi[c(7),c(1,2,3,4,5,6,7,8,9),10]),mean(Output.model$q75$psi[c(8),c(1,2,3,4,5,6,7,8,9),10]),
                             mean(Output.model$q75$psi[c(9),c(1,2,3,4,5,6,7,8,9),10]),mean(Output.model$q75$psi[c(10),c(1,2,3,4,5,6,7,8,9),10]),mean(Output.model$q75$psi[c(11),c(1,2),10]),
                             mean(Output.model$q75$psi[c(12),c(1,2),10]),mean(Output.model$q75$psi[c(13),c(1,2),10]),mean(Output.model$q75$psi[c(14),c(1,2),10]),
                             mean(Output.model$q75$psi[c(15),c(1,2),10]),mean(Output.model$q75$psi[c(16),c(1,2),10]),mean(Output.model$q75$psi[c(17),c(1,2),10]),
                             mean(Output.model$q75$psi[c(18),c(1,2),10]),mean(Output.model$q75$psi[c(19),c(1,2),10]))),
                      mean(c(mean(Output.model$q75$psi[c(2),c(1,2,3,4,5,6,7,8,9),19]),mean(Output.model$q75$psi[c(3),c(1,2,3,4,5,6,7,8,9),19]),mean(Output.model$q75$psi[c(4),c(1,2,3,4,5,6,7,8,9),19]),
                             mean(Output.model$q75$psi[c(6),c(1,2,3,4,5,6,7,8,9),19]),mean(Output.model$q75$psi[c(7),c(1,2,3,4,5,6,7,8,9),19]),mean(Output.model$q75$psi[c(8),c(1,2,3,4,5,6,7,8,9),19]),
                             mean(Output.model$q75$psi[c(9),c(1,2,3,4,5,6,7,8,9),19]),mean(Output.model$q75$psi[c(10),c(1,2,3,4,5,6,7,8,9),19]),mean(Output.model$q75$psi[c(11),c(1,2),19]),
                             mean(Output.model$q75$psi[c(12),c(1,2),19]),mean(Output.model$q75$psi[c(13),c(1,2),19]),mean(Output.model$q75$psi[c(14),c(1,2),19]),
                             mean(Output.model$q75$psi[c(15),c(1,2),19]),mean(Output.model$q75$psi[c(16),c(1,2),19]),mean(Output.model$q75$psi[c(17),c(1,2),19]),
                             mean(Output.model$q75$psi[c(18),c(1,2),19]),mean(Output.model$q75$psi[c(19),c(1,2),19]))),
                      mean(c(mean(Output.model$q75$psi[c(2),c(1,2,3,4,5,6,7,8,9),35]),mean(Output.model$q75$psi[c(3),c(1,2,3,4,5,6,7,8,9),35]),mean(Output.model$q75$psi[c(4),c(1,2,3,4,5,6,7,8,9),35]),
                             mean(Output.model$q75$psi[c(6),c(1,2,3,4,5,6,7,8,9),35]),mean(Output.model$q75$psi[c(7),c(1,2,3,4,5,6,7,8,9),35]),mean(Output.model$q75$psi[c(8),c(1,2,3,4,5,6,7,8,9),35]),
                             mean(Output.model$q75$psi[c(9),c(1,2,3,4,5,6,7,8,9),35]),mean(Output.model$q75$psi[c(10),c(1,2,3,4,5,6,7,8,9),35]),mean(Output.model$q75$psi[c(11),c(1,2),35]),
                             mean(Output.model$q75$psi[c(12),c(1,2),35]),mean(Output.model$q75$psi[c(13),c(1,2),35]),mean(Output.model$q75$psi[c(14),c(1,2),35]),
                             mean(Output.model$q75$psi[c(15),c(1,2),35]),mean(Output.model$q75$psi[c(16),c(1,2),35]),mean(Output.model$q75$psi[c(17),c(1,2),35]),
                             mean(Output.model$q75$psi[c(18),c(1,2),35]),mean(Output.model$q75$psi[c(19),c(1,2),35]))),
                      mean(c(mean(Output.model$q75$psi[c(2),c(1,2,3,4,5,6,7,8,9),25]),mean(Output.model$q75$psi[c(3),c(1,2,3,4,5,6,7,8,9),25]),mean(Output.model$q75$psi[c(4),c(1,2,3,4,5,6,7,8,9),25]),
                             mean(Output.model$q75$psi[c(6),c(1,2,3,4,5,6,7,8,9),25]),mean(Output.model$q75$psi[c(7),c(1,2,3,4,5,6,7,8,9),25]),mean(Output.model$q75$psi[c(8),c(1,2,3,4,5,6,7,8,9),25]),
                             mean(Output.model$q75$psi[c(9),c(1,2,3,4,5,6,7,8,9),25]),mean(Output.model$q75$psi[c(10),c(1,2,3,4,5,6,7,8,9),25]),mean(Output.model$q75$psi[c(11),c(1,2),25]),
                             mean(Output.model$q75$psi[c(12),c(1,2),25]),mean(Output.model$q75$psi[c(13),c(1,2),25]),mean(Output.model$q75$psi[c(14),c(1,2),25]),
                             mean(Output.model$q75$psi[c(15),c(1,2),25]),mean(Output.model$q75$psi[c(16),c(1,2),25]),mean(Output.model$q75$psi[c(17),c(1,2),25]),
                             mean(Output.model$q75$psi[c(18),c(1,2),25]),mean(Output.model$q75$psi[c(19),c(1,2),25]))),
                      mean(c(mean(Output.model$q75$psi[c(2),c(1,2,3,4,5,6,7,8,9),28]),mean(Output.model$q75$psi[c(3),c(1,2,3,4,5,6,7,8,9),28]),mean(Output.model$q75$psi[c(4),c(1,2,3,4,5,6,7,8,9),28]),
                             mean(Output.model$q75$psi[c(6),c(1,2,3,4,5,6,7,8,9),28]),mean(Output.model$q75$psi[c(7),c(1,2,3,4,5,6,7,8,9),28]),mean(Output.model$q75$psi[c(8),c(1,2,3,4,5,6,7,8,9),28]),
                             mean(Output.model$q75$psi[c(9),c(1,2,3,4,5,6,7,8,9),28]),mean(Output.model$q75$psi[c(10),c(1,2,3,4,5,6,7,8,9),28]),mean(Output.model$q75$psi[c(11),c(1,2),28]),
                             mean(Output.model$q75$psi[c(12),c(1,2),28]),mean(Output.model$q75$psi[c(13),c(1,2),28]),mean(Output.model$q75$psi[c(14),c(1,2),28]),
                             mean(Output.model$q75$psi[c(15),c(1,2),28]),mean(Output.model$q75$psi[c(16),c(1,2),28]),mean(Output.model$q75$psi[c(17),c(1,2),28]),
                             mean(Output.model$q75$psi[c(18),c(1,2),28]),mean(Output.model$q75$psi[c(19),c(1,2),28]))),
                      mean(c(mean(Output.model$q75$psi[c(2),c(1,2,3,4,5,6,7,8,9),46]),mean(Output.model$q75$psi[c(3),c(1,2,3,4,5,6,7,8,9),46]),mean(Output.model$q75$psi[c(4),c(1,2,3,4,5,6,7,8,9),46]),
                             mean(Output.model$q75$psi[c(6),c(1,2,3,4,5,6,7,8,9),46]),mean(Output.model$q75$psi[c(7),c(1,2,3,4,5,6,7,8,9),46]),mean(Output.model$q75$psi[c(8),c(1,2,3,4,5,6,7,8,9),46]),
                             mean(Output.model$q75$psi[c(9),c(1,2,3,4,5,6,7,8,9),46]),mean(Output.model$q75$psi[c(10),c(1,2,3,4,5,6,7,8,9),46]),mean(Output.model$q75$psi[c(11),c(1,2),46]),
                             mean(Output.model$q75$psi[c(12),c(1,2),46]),mean(Output.model$q75$psi[c(13),c(1,2),46]),mean(Output.model$q75$psi[c(14),c(1,2),46]),
                             mean(Output.model$q75$psi[c(15),c(1,2),46]),mean(Output.model$q75$psi[c(16),c(1,2),46]),mean(Output.model$q75$psi[c(17),c(1,2),46]),
                             mean(Output.model$q75$psi[c(18),c(1,2),46]),mean(Output.model$q75$psi[c(19),c(1,2),46]))),
                      mean(c(mean(Output.model$q75$psi[c(2),c(1,2,3,4,5,6,7,8,9),41]),mean(Output.model$q75$psi[c(3),c(1,2,3,4,5,6,7,8,9),41]),mean(Output.model$q75$psi[c(4),c(1,2,3,4,5,6,7,8,9),41]),
                             mean(Output.model$q75$psi[c(6),c(1,2,3,4,5,6,7,8,9),41]),mean(Output.model$q75$psi[c(7),c(1,2,3,4,5,6,7,8,9),41]),mean(Output.model$q75$psi[c(8),c(1,2,3,4,5,6,7,8,9),41]),
                             mean(Output.model$q75$psi[c(9),c(1,2,3,4,5,6,7,8,9),41]),mean(Output.model$q75$psi[c(10),c(1,2,3,4,5,6,7,8,9),41]),mean(Output.model$q75$psi[c(11),c(1,2),41]),
                             mean(Output.model$q75$psi[c(12),c(1,2),41]),mean(Output.model$q75$psi[c(13),c(1,2),41]),mean(Output.model$q75$psi[c(14),c(1,2),41]),
                             mean(Output.model$q75$psi[c(15),c(1,2),41]),mean(Output.model$q75$psi[c(16),c(1,2),41]),mean(Output.model$q75$psi[c(17),c(1,2),41]),
                             mean(Output.model$q75$psi[c(18),c(1,2),41]),mean(Output.model$q75$psi[c(19),c(1,2),41]))),
                      mean(c(mean(Output.model$q75$psi[c(2),c(1,2,3,4,5,6,7,8,9),16]),mean(Output.model$q75$psi[c(3),c(1,2,3,4,5,6,7,8,9),16]),mean(Output.model$q75$psi[c(4),c(1,2,3,4,5,6,7,8,9),16]),
                             mean(Output.model$q75$psi[c(6),c(1,2,3,4,5,6,7,8,9),16]),mean(Output.model$q75$psi[c(7),c(1,2,3,4,5,6,7,8,9),16]),mean(Output.model$q75$psi[c(8),c(1,2,3,4,5,6,7,8,9),16]),
                             mean(Output.model$q75$psi[c(9),c(1,2,3,4,5,6,7,8,9),16]),mean(Output.model$q75$psi[c(10),c(1,2,3,4,5,6,7,8,9),16]),mean(Output.model$q75$psi[c(11),c(1,2),16]),
                             mean(Output.model$q75$psi[c(12),c(1,2),16]),mean(Output.model$q75$psi[c(13),c(1,2),16]),mean(Output.model$q75$psi[c(14),c(1,2),16]),
                             mean(Output.model$q75$psi[c(15),c(1,2),16]),mean(Output.model$q75$psi[c(16),c(1,2),16]),mean(Output.model$q75$psi[c(17),c(1,2),16]),
                             mean(Output.model$q75$psi[c(18),c(1,2),16]),mean(Output.model$q75$psi[c(19),c(1,2),16]))),
                      mean(c(mean(Output.model$q75$psi[c(2),c(1,2,3,4,5,6,7,8,9),36]),mean(Output.model$q75$psi[c(3),c(1,2,3,4,5,6,7,8,9),36]),mean(Output.model$q75$psi[c(4),c(1,2,3,4,5,6,7,8,9),36]),
                             mean(Output.model$q75$psi[c(6),c(1,2,3,4,5,6,7,8,9),36]),mean(Output.model$q75$psi[c(7),c(1,2,3,4,5,6,7,8,9),36]),mean(Output.model$q75$psi[c(8),c(1,2,3,4,5,6,7,8,9),36]),
                             mean(Output.model$q75$psi[c(9),c(1,2,3,4,5,6,7,8,9),36]),mean(Output.model$q75$psi[c(10),c(1,2,3,4,5,6,7,8,9),36]),mean(Output.model$q75$psi[c(11),c(1,2),36]),
                             mean(Output.model$q75$psi[c(12),c(1,2),36]),mean(Output.model$q75$psi[c(13),c(1,2),36]),mean(Output.model$q75$psi[c(14),c(1,2),36]),
                             mean(Output.model$q75$psi[c(15),c(1,2),36]),mean(Output.model$q75$psi[c(16),c(1,2),36]),mean(Output.model$q75$psi[c(17),c(1,2),36]),
                             mean(Output.model$q75$psi[c(18),c(1,2),36]),mean(Output.model$q75$psi[c(19),c(1,2),36]))),
                      mean(c(mean(Output.model$q75$psi[c(2),c(1,2,3,4,5,6,7,8,9),14]),mean(Output.model$q75$psi[c(3),c(1,2,3,4,5,6,7,8,9),14]),mean(Output.model$q75$psi[c(4),c(1,2,3,4,5,6,7,8,9),14]),
                             mean(Output.model$q75$psi[c(6),c(1,2,3,4,5,6,7,8,9),14]),mean(Output.model$q75$psi[c(7),c(1,2,3,4,5,6,7,8,9),14]),mean(Output.model$q75$psi[c(8),c(1,2,3,4,5,6,7,8,9),14]),
                             mean(Output.model$q75$psi[c(9),c(1,2,3,4,5,6,7,8,9),14]),mean(Output.model$q75$psi[c(10),c(1,2,3,4,5,6,7,8,9),14]),mean(Output.model$q75$psi[c(11),c(1,2),14]),
                             mean(Output.model$q75$psi[c(12),c(1,2),14]),mean(Output.model$q75$psi[c(13),c(1,2),14]),mean(Output.model$q75$psi[c(14),c(1,2),14]),
                             mean(Output.model$q75$psi[c(15),c(1,2),14]),mean(Output.model$q75$psi[c(16),c(1,2),14]),mean(Output.model$q75$psi[c(17),c(1,2),14]),
                             mean(Output.model$q75$psi[c(18),c(1,2),14]),mean(Output.model$q75$psi[c(19),c(1,2),14]))),
                      mean(c(mean(Output.model$q75$psi[c(2),c(1,2,3,4,5,6,7,8,9),11]),mean(Output.model$q75$psi[c(3),c(1,2,3,4,5,6,7,8,9),11]),mean(Output.model$q75$psi[c(4),c(1,2,3,4,5,6,7,8,9),11]),
                             mean(Output.model$q75$psi[c(6),c(1,2,3,4,5,6,7,8,9),11]),mean(Output.model$q75$psi[c(7),c(1,2,3,4,5,6,7,8,9),11]),mean(Output.model$q75$psi[c(8),c(1,2,3,4,5,6,7,8,9),11]),
                             mean(Output.model$q75$psi[c(9),c(1,2,3,4,5,6,7,8,9),11]),mean(Output.model$q75$psi[c(10),c(1,2,3,4,5,6,7,8,9),11]),mean(Output.model$q75$psi[c(11),c(1,2),11]),
                             mean(Output.model$q75$psi[c(12),c(1,2),11]),mean(Output.model$q75$psi[c(13),c(1,2),11]),mean(Output.model$q75$psi[c(14),c(1,2),11]),
                             mean(Output.model$q75$psi[c(15),c(1,2),11]),mean(Output.model$q75$psi[c(16),c(1,2),11]),mean(Output.model$q75$psi[c(17),c(1,2),11]),
                             mean(Output.model$q75$psi[c(18),c(1,2),11]),mean(Output.model$q75$psi[c(19),c(1,2),11]))),
                      mean(c(mean(Output.model$q75$psi[c(2),c(1,2,3,4,5,6,7,8,9),31]),mean(Output.model$q75$psi[c(3),c(1,2,3,4,5,6,7,8,9),31]),mean(Output.model$q75$psi[c(4),c(1,2,3,4,5,6,7,8,9),31]),
                             mean(Output.model$q75$psi[c(6),c(1,2,3,4,5,6,7,8,9),31]),mean(Output.model$q75$psi[c(7),c(1,2,3,4,5,6,7,8,9),31]),mean(Output.model$q75$psi[c(8),c(1,2,3,4,5,6,7,8,9),31]),
                             mean(Output.model$q75$psi[c(9),c(1,2,3,4,5,6,7,8,9),31]),mean(Output.model$q75$psi[c(10),c(1,2,3,4,5,6,7,8,9),31]),mean(Output.model$q75$psi[c(11),c(1,2),31]),
                             mean(Output.model$q75$psi[c(12),c(1,2),31]),mean(Output.model$q75$psi[c(13),c(1,2),31]),mean(Output.model$q75$psi[c(14),c(1,2),31]),
                             mean(Output.model$q75$psi[c(15),c(1,2),31]),mean(Output.model$q75$psi[c(16),c(1,2),31]),mean(Output.model$q75$psi[c(17),c(1,2),31]),
                             mean(Output.model$q75$psi[c(18),c(1,2),31]),mean(Output.model$q75$psi[c(19),c(1,2),31]))),
                      mean(c(mean(Output.model$q75$psi[c(2),c(1,2,3,4,5,6,7,8,9),6]),mean(Output.model$q75$psi[c(3),c(1,2,3,4,5,6,7,8,9),6]),mean(Output.model$q75$psi[c(4),c(1,2,3,4,5,6,7,8,9),6]),
                             mean(Output.model$q75$psi[c(6),c(1,2,3,4,5,6,7,8,9),6]),mean(Output.model$q75$psi[c(7),c(1,2,3,4,5,6,7,8,9),6]),mean(Output.model$q75$psi[c(8),c(1,2,3,4,5,6,7,8,9),6]),
                             mean(Output.model$q75$psi[c(9),c(1,2,3,4,5,6,7,8,9),6]),mean(Output.model$q75$psi[c(10),c(1,2,3,4,5,6,7,8,9),6]),mean(Output.model$q75$psi[c(11),c(1,2),6]),
                             mean(Output.model$q75$psi[c(12),c(1,2),6]),mean(Output.model$q75$psi[c(13),c(1,2),6]),mean(Output.model$q75$psi[c(14),c(1,2),6]),
                             mean(Output.model$q75$psi[c(15),c(1,2),6]),mean(Output.model$q75$psi[c(16),c(1,2),6]),mean(Output.model$q75$psi[c(17),c(1,2),6]),
                             mean(Output.model$q75$psi[c(18),c(1,2),6]),mean(Output.model$q75$psi[c(19),c(1,2),6])))),
                    c(mean(c(mean(Output.model$q97.5$psi[c(2),c(1,2,3,4,5,6,7,8,9),45]),mean(Output.model$q97.5$psi[c(3),c(1,2,3,4,5,6,7,8,9),45]),mean(Output.model$q97.5$psi[c(4),c(1,2,3,4,5,6,7,8,9),45]),
                             mean(Output.model$q97.5$psi[c(6),c(1,2,3,4,5,6,7,8,9),45]),mean(Output.model$q97.5$psi[c(7),c(1,2,3,4,5,6,7,8,9),45]),mean(Output.model$q97.5$psi[c(8),c(1,2,3,4,5,6,7,8,9),45]),
                             mean(Output.model$q97.5$psi[c(9),c(1,2,3,4,5,6,7,8,9),45]),mean(Output.model$q97.5$psi[c(10),c(1,2,3,4,5,6,7,8,9),45]),mean(Output.model$q97.5$psi[c(11),c(1,2),45]),
                             mean(Output.model$q97.5$psi[c(12),c(1,2),45]),mean(Output.model$q97.5$psi[c(13),c(1,2),45]),mean(Output.model$q97.5$psi[c(14),c(1,2),45]),
                             mean(Output.model$q97.5$psi[c(15),c(1,2),45]),mean(Output.model$q97.5$psi[c(16),c(1,2),45]),mean(Output.model$q97.5$psi[c(17),c(1,2),45]),
                             mean(Output.model$q97.5$psi[c(18),c(1,2),45]),mean(Output.model$q97.5$psi[c(19),c(1,2),45]))),
                      mean(c(mean(Output.model$q97.5$psi[c(2),c(1,2,3,4,5,6,7,8,9),1]),mean(Output.model$q97.5$psi[c(3),c(1,2,3,4,5,6,7,8,9),1]),mean(Output.model$q97.5$psi[c(4),c(1,2,3,4,5,6,7,8,9),1]),
                             mean(Output.model$q97.5$psi[c(6),c(1,2,3,4,5,6,7,8,9),1]),mean(Output.model$q97.5$psi[c(7),c(1,2,3,4,5,6,7,8,9),1]),mean(Output.model$q97.5$psi[c(8),c(1,2,3,4,5,6,7,8,9),1]),
                             mean(Output.model$q97.5$psi[c(9),c(1,2,3,4,5,6,7,8,9),1]),mean(Output.model$q97.5$psi[c(10),c(1,2,3,4,5,6,7,8,9),1]),mean(Output.model$q97.5$psi[c(11),c(1,2),1]),
                             mean(Output.model$q97.5$psi[c(12),c(1,2),1]),mean(Output.model$q97.5$psi[c(13),c(1,2),1]),mean(Output.model$q97.5$psi[c(14),c(1,2),1]),
                             mean(Output.model$q97.5$psi[c(15),c(1,2),1]),mean(Output.model$q97.5$psi[c(16),c(1,2),1]),mean(Output.model$q97.5$psi[c(17),c(1,2),1]),
                             mean(Output.model$q97.5$psi[c(18),c(1,2),1]),mean(Output.model$q97.5$psi[c(19),c(1,2),1]))),
                      mean(c(mean(Output.model$q97.5$psi[c(2),c(1,2,3,4,5,6,7,8,9),15]),mean(Output.model$q97.5$psi[c(3),c(1,2,3,4,5,6,7,8,9),15]),mean(Output.model$q97.5$psi[c(4),c(1,2,3,4,5,6,7,8,9),15]),
                             mean(Output.model$q97.5$psi[c(6),c(1,2,3,4,5,6,7,8,9),15]),mean(Output.model$q97.5$psi[c(7),c(1,2,3,4,5,6,7,8,9),15]),mean(Output.model$q97.5$psi[c(8),c(1,2,3,4,5,6,7,8,9),15]),
                             mean(Output.model$q97.5$psi[c(9),c(1,2,3,4,5,6,7,8,9),15]),mean(Output.model$q97.5$psi[c(10),c(1,2,3,4,5,6,7,8,9),15]),mean(Output.model$q97.5$psi[c(11),c(1,2),15]),
                             mean(Output.model$q97.5$psi[c(12),c(1,2),15]),mean(Output.model$q97.5$psi[c(13),c(1,2),15]),mean(Output.model$q97.5$psi[c(14),c(1,2),15]),
                             mean(Output.model$q97.5$psi[c(15),c(1,2),15]),mean(Output.model$q97.5$psi[c(16),c(1,2),15]),mean(Output.model$q97.5$psi[c(17),c(1,2),15]),
                             mean(Output.model$q97.5$psi[c(18),c(1,2),15]),mean(Output.model$q97.5$psi[c(19),c(1,2),15]))),
                      mean(c(mean(Output.model$q97.5$psi[c(2),c(1,2,3,4,5,6,7,8,9),7]),mean(Output.model$q97.5$psi[c(3),c(1,2,3,4,5,6,7,8,9),7]),mean(Output.model$q97.5$psi[c(4),c(1,2,3,4,5,6,7,8,9),7]),
                             mean(Output.model$q97.5$psi[c(6),c(1,2,3,4,5,6,7,8,9),7]),mean(Output.model$q97.5$psi[c(7),c(1,2,3,4,5,6,7,8,9),7]),mean(Output.model$q97.5$psi[c(8),c(1,2,3,4,5,6,7,8,9),7]),
                             mean(Output.model$q97.5$psi[c(9),c(1,2,3,4,5,6,7,8,9),7]),mean(Output.model$q97.5$psi[c(10),c(1,2,3,4,5,6,7,8,9),7]),mean(Output.model$q97.5$psi[c(11),c(1,2),7]),
                             mean(Output.model$q97.5$psi[c(12),c(1,2),7]),mean(Output.model$q97.5$psi[c(13),c(1,2),7]),mean(Output.model$q97.5$psi[c(14),c(1,2),7]),
                             mean(Output.model$q97.5$psi[c(15),c(1,2),7]),mean(Output.model$q97.5$psi[c(16),c(1,2),7]),mean(Output.model$q97.5$psi[c(17),c(1,2),7]),
                             mean(Output.model$q97.5$psi[c(18),c(1,2),7]),mean(Output.model$q97.5$psi[c(19),c(1,2),7]))),
                      mean(c(mean(Output.model$q97.5$psi[c(2),c(1,2,3,4,5,6,7,8,9),22]),mean(Output.model$q97.5$psi[c(3),c(1,2,3,4,5,6,7,8,9),22]),mean(Output.model$q97.5$psi[c(4),c(1,2,3,4,5,6,7,8,9),22]),
                             mean(Output.model$q97.5$psi[c(6),c(1,2,3,4,5,6,7,8,9),22]),mean(Output.model$q97.5$psi[c(7),c(1,2,3,4,5,6,7,8,9),22]),mean(Output.model$q97.5$psi[c(8),c(1,2,3,4,5,6,7,8,9),22]),
                             mean(Output.model$q97.5$psi[c(9),c(1,2,3,4,5,6,7,8,9),22]),mean(Output.model$q97.5$psi[c(10),c(1,2,3,4,5,6,7,8,9),22]),mean(Output.model$q97.5$psi[c(11),c(1,2),22]),
                             mean(Output.model$q97.5$psi[c(12),c(1,2),22]),mean(Output.model$q97.5$psi[c(13),c(1,2),22]),mean(Output.model$q97.5$psi[c(14),c(1,2),22]),
                             mean(Output.model$q97.5$psi[c(15),c(1,2),22]),mean(Output.model$q97.5$psi[c(16),c(1,2),22]),mean(Output.model$q97.5$psi[c(17),c(1,2),22]),
                             mean(Output.model$q97.5$psi[c(18),c(1,2),22]),mean(Output.model$q97.5$psi[c(19),c(1,2),22]))),
                      mean(c(mean(Output.model$q97.5$psi[c(2),c(1,2,3,4,5,6,7,8,9),29]),mean(Output.model$q97.5$psi[c(3),c(1,2,3,4,5,6,7,8,9),29]),mean(Output.model$q97.5$psi[c(4),c(1,2,3,4,5,6,7,8,9),29]),
                             mean(Output.model$q97.5$psi[c(6),c(1,2,3,4,5,6,7,8,9),29]),mean(Output.model$q97.5$psi[c(7),c(1,2,3,4,5,6,7,8,9),29]),mean(Output.model$q97.5$psi[c(8),c(1,2,3,4,5,6,7,8,9),29]),
                             mean(Output.model$q97.5$psi[c(9),c(1,2,3,4,5,6,7,8,9),29]),mean(Output.model$q97.5$psi[c(10),c(1,2,3,4,5,6,7,8,9),29]),mean(Output.model$q97.5$psi[c(11),c(1,2),29]),
                             mean(Output.model$q97.5$psi[c(12),c(1,2),29]),mean(Output.model$q97.5$psi[c(13),c(1,2),29]),mean(Output.model$q97.5$psi[c(14),c(1,2),29]),
                             mean(Output.model$q97.5$psi[c(15),c(1,2),29]),mean(Output.model$q97.5$psi[c(16),c(1,2),29]),mean(Output.model$q97.5$psi[c(17),c(1,2),29]),
                             mean(Output.model$q97.5$psi[c(18),c(1,2),29]),mean(Output.model$q97.5$psi[c(19),c(1,2),29]))),
                      mean(c(mean(Output.model$q97.5$psi[c(2),c(1,2,3,4,5,6,7,8,9),18]),mean(Output.model$q97.5$psi[c(3),c(1,2,3,4,5,6,7,8,9),18]),mean(Output.model$q97.5$psi[c(4),c(1,2,3,4,5,6,7,8,9),18]),
                             mean(Output.model$q97.5$psi[c(6),c(1,2,3,4,5,6,7,8,9),18]),mean(Output.model$q97.5$psi[c(7),c(1,2,3,4,5,6,7,8,9),18]),mean(Output.model$q97.5$psi[c(8),c(1,2,3,4,5,6,7,8,9),18]),
                             mean(Output.model$q97.5$psi[c(9),c(1,2,3,4,5,6,7,8,9),18]),mean(Output.model$q97.5$psi[c(10),c(1,2,3,4,5,6,7,8,9),18]),mean(Output.model$q97.5$psi[c(11),c(1,2),18]),
                             mean(Output.model$q97.5$psi[c(12),c(1,2),18]),mean(Output.model$q97.5$psi[c(13),c(1,2),18]),mean(Output.model$q97.5$psi[c(14),c(1,2),18]),
                             mean(Output.model$q97.5$psi[c(15),c(1,2),18]),mean(Output.model$q97.5$psi[c(16),c(1,2),18]),mean(Output.model$q97.5$psi[c(17),c(1,2),18]),
                             mean(Output.model$q97.5$psi[c(18),c(1,2),18]),mean(Output.model$q97.5$psi[c(19),c(1,2),18]))),
                      mean(c(mean(Output.model$q97.5$psi[c(2),c(1,2,3,4,5,6,7,8,9),33]),mean(Output.model$q97.5$psi[c(3),c(1,2,3,4,5,6,7,8,9),33]),mean(Output.model$q97.5$psi[c(4),c(1,2,3,4,5,6,7,8,9),33]),
                             mean(Output.model$q97.5$psi[c(6),c(1,2,3,4,5,6,7,8,9),33]),mean(Output.model$q97.5$psi[c(7),c(1,2,3,4,5,6,7,8,9),33]),mean(Output.model$q97.5$psi[c(8),c(1,2,3,4,5,6,7,8,9),33]),
                             mean(Output.model$q97.5$psi[c(9),c(1,2,3,4,5,6,7,8,9),33]),mean(Output.model$q97.5$psi[c(10),c(1,2,3,4,5,6,7,8,9),33]),mean(Output.model$q97.5$psi[c(11),c(1,2),33]),
                             mean(Output.model$q97.5$psi[c(12),c(1,2),33]),mean(Output.model$q97.5$psi[c(13),c(1,2),33]),mean(Output.model$q97.5$psi[c(14),c(1,2),33]),
                             mean(Output.model$q97.5$psi[c(15),c(1,2),33]),mean(Output.model$q97.5$psi[c(16),c(1,2),33]),mean(Output.model$q97.5$psi[c(17),c(1,2),33]),
                             mean(Output.model$q97.5$psi[c(18),c(1,2),33]),mean(Output.model$q97.5$psi[c(19),c(1,2),33]))),
                      mean(c(mean(Output.model$q97.5$psi[c(2),c(1,2,3,4,5,6,7,8,9),12]),mean(Output.model$q97.5$psi[c(3),c(1,2,3,4,5,6,7,8,9),12]),mean(Output.model$q97.5$psi[c(4),c(1,2,3,4,5,6,7,8,9),12]),
                             mean(Output.model$q97.5$psi[c(6),c(1,2,3,4,5,6,7,8,9),12]),mean(Output.model$q97.5$psi[c(7),c(1,2,3,4,5,6,7,8,9),12]),mean(Output.model$q97.5$psi[c(8),c(1,2,3,4,5,6,7,8,9),12]),
                             mean(Output.model$q97.5$psi[c(9),c(1,2,3,4,5,6,7,8,9),12]),mean(Output.model$q97.5$psi[c(10),c(1,2,3,4,5,6,7,8,9),12]),mean(Output.model$q97.5$psi[c(11),c(1,2),12]),
                             mean(Output.model$q97.5$psi[c(12),c(1,2),12]),mean(Output.model$q97.5$psi[c(13),c(1,2),12]),mean(Output.model$q97.5$psi[c(14),c(1,2),12]),
                             mean(Output.model$q97.5$psi[c(15),c(1,2),12]),mean(Output.model$q97.5$psi[c(16),c(1,2),12]),mean(Output.model$q97.5$psi[c(17),c(1,2),12]),
                             mean(Output.model$q97.5$psi[c(18),c(1,2),12]),mean(Output.model$q97.5$psi[c(19),c(1,2),12]))),
                      mean(c(mean(Output.model$q97.5$psi[c(2),c(1,2,3,4,5,6,7,8,9),9]),mean(Output.model$q97.5$psi[c(3),c(1,2,3,4,5,6,7,8,9),9]),mean(Output.model$q97.5$psi[c(4),c(1,2,3,4,5,6,7,8,9),9]),
                             mean(Output.model$q97.5$psi[c(6),c(1,2,3,4,5,6,7,8,9),9]),mean(Output.model$q97.5$psi[c(7),c(1,2,3,4,5,6,7,8,9),9]),mean(Output.model$q97.5$psi[c(8),c(1,2,3,4,5,6,7,8,9),9]),
                             mean(Output.model$q97.5$psi[c(9),c(1,2,3,4,5,6,7,8,9),9]),mean(Output.model$q97.5$psi[c(10),c(1,2,3,4,5,6,7,8,9),9]),mean(Output.model$q97.5$psi[c(11),c(1,2),9]),
                             mean(Output.model$q97.5$psi[c(12),c(1,2),9]),mean(Output.model$q97.5$psi[c(13),c(1,2),9]),mean(Output.model$q97.5$psi[c(14),c(1,2),9]),
                             mean(Output.model$q97.5$psi[c(15),c(1,2),9]),mean(Output.model$q97.5$psi[c(16),c(1,2),9]),mean(Output.model$q97.5$psi[c(17),c(1,2),9]),
                             mean(Output.model$q97.5$psi[c(18),c(1,2),9]),mean(Output.model$q97.5$psi[c(19),c(1,2),9]))),
                      mean(c(mean(Output.model$q97.5$psi[c(2),c(1,2,3,4,5,6,7,8,9),47]),mean(Output.model$q97.5$psi[c(3),c(1,2,3,4,5,6,7,8,9),47]),mean(Output.model$q97.5$psi[c(4),c(1,2,3,4,5,6,7,8,9),47]),
                             mean(Output.model$q97.5$psi[c(6),c(1,2,3,4,5,6,7,8,9),47]),mean(Output.model$q97.5$psi[c(7),c(1,2,3,4,5,6,7,8,9),47]),mean(Output.model$q97.5$psi[c(8),c(1,2,3,4,5,6,7,8,9),47]),
                             mean(Output.model$q97.5$psi[c(9),c(1,2,3,4,5,6,7,8,9),47]),mean(Output.model$q97.5$psi[c(10),c(1,2,3,4,5,6,7,8,9),47]),mean(Output.model$q97.5$psi[c(11),c(1,2),47]),
                             mean(Output.model$q97.5$psi[c(12),c(1,2),47]),mean(Output.model$q97.5$psi[c(13),c(1,2),47]),mean(Output.model$q97.5$psi[c(14),c(1,2),47]),
                             mean(Output.model$q97.5$psi[c(15),c(1,2),47]),mean(Output.model$q97.5$psi[c(16),c(1,2),47]),mean(Output.model$q97.5$psi[c(17),c(1,2),47]),
                             mean(Output.model$q97.5$psi[c(18),c(1,2),47]),mean(Output.model$q97.5$psi[c(19),c(1,2),47]))),
                      mean(c(mean(Output.model$q97.5$psi[c(2),c(1,2,3,4,5,6,7,8,9),30]),mean(Output.model$q97.5$psi[c(3),c(1,2,3,4,5,6,7,8,9),30]),mean(Output.model$q97.5$psi[c(4),c(1,2,3,4,5,6,7,8,9),30]),
                             mean(Output.model$q97.5$psi[c(6),c(1,2,3,4,5,6,7,8,9),30]),mean(Output.model$q97.5$psi[c(7),c(1,2,3,4,5,6,7,8,9),30]),mean(Output.model$q97.5$psi[c(8),c(1,2,3,4,5,6,7,8,9),30]),
                             mean(Output.model$q97.5$psi[c(9),c(1,2,3,4,5,6,7,8,9),30]),mean(Output.model$q97.5$psi[c(10),c(1,2,3,4,5,6,7,8,9),30]),mean(Output.model$q97.5$psi[c(11),c(1,2),30]),
                             mean(Output.model$q97.5$psi[c(12),c(1,2),30]),mean(Output.model$q97.5$psi[c(13),c(1,2),30]),mean(Output.model$q97.5$psi[c(14),c(1,2),30]),
                             mean(Output.model$q97.5$psi[c(15),c(1,2),30]),mean(Output.model$q97.5$psi[c(16),c(1,2),30]),mean(Output.model$q97.5$psi[c(17),c(1,2),30]),
                             mean(Output.model$q97.5$psi[c(18),c(1,2),30]),mean(Output.model$q97.5$psi[c(19),c(1,2),30]))),
                      mean(c(mean(Output.model$q97.5$psi[c(2),c(1,2,3,4,5,6,7,8,9),40]),mean(Output.model$q97.5$psi[c(3),c(1,2,3,4,5,6,7,8,9),40]),mean(Output.model$q97.5$psi[c(4),c(1,2,3,4,5,6,7,8,9),40]),
                             mean(Output.model$q97.5$psi[c(6),c(1,2,3,4,5,6,7,8,9),40]),mean(Output.model$q97.5$psi[c(7),c(1,2,3,4,5,6,7,8,9),40]),mean(Output.model$q97.5$psi[c(8),c(1,2,3,4,5,6,7,8,9),40]),
                             mean(Output.model$q97.5$psi[c(9),c(1,2,3,4,5,6,7,8,9),40]),mean(Output.model$q97.5$psi[c(10),c(1,2,3,4,5,6,7,8,9),40]),mean(Output.model$q97.5$psi[c(11),c(1,2),40]),
                             mean(Output.model$q97.5$psi[c(12),c(1,2),40]),mean(Output.model$q97.5$psi[c(13),c(1,2),40]),mean(Output.model$q97.5$psi[c(14),c(1,2),40]),
                             mean(Output.model$q97.5$psi[c(15),c(1,2),40]),mean(Output.model$q97.5$psi[c(16),c(1,2),40]),mean(Output.model$q97.5$psi[c(17),c(1,2),40]),
                             mean(Output.model$q97.5$psi[c(18),c(1,2),40]),mean(Output.model$q97.5$psi[c(19),c(1,2),40]))),
                      mean(c(mean(Output.model$q97.5$psi[c(2),c(1,2,3,4,5,6,7,8,9),5]),mean(Output.model$q97.5$psi[c(3),c(1,2,3,4,5,6,7,8,9),5]),mean(Output.model$q97.5$psi[c(4),c(1,2,3,4,5,6,7,8,9),5]),
                             mean(Output.model$q97.5$psi[c(6),c(1,2,3,4,5,6,7,8,9),5]),mean(Output.model$q97.5$psi[c(7),c(1,2,3,4,5,6,7,8,9),5]),mean(Output.model$q97.5$psi[c(8),c(1,2,3,4,5,6,7,8,9),5]),
                             mean(Output.model$q97.5$psi[c(9),c(1,2,3,4,5,6,7,8,9),5]),mean(Output.model$q97.5$psi[c(10),c(1,2,3,4,5,6,7,8,9),5]),mean(Output.model$q97.5$psi[c(11),c(1,2),5]),
                             mean(Output.model$q97.5$psi[c(12),c(1,2),5]),mean(Output.model$q97.5$psi[c(13),c(1,2),5]),mean(Output.model$q97.5$psi[c(14),c(1,2),5]),
                             mean(Output.model$q97.5$psi[c(15),c(1,2),5]),mean(Output.model$q97.5$psi[c(16),c(1,2),5]),mean(Output.model$q97.5$psi[c(17),c(1,2),5]),
                             mean(Output.model$q97.5$psi[c(18),c(1,2),5]),mean(Output.model$q97.5$psi[c(19),c(1,2),5]))),
                      mean(c(mean(Output.model$q97.5$psi[c(2),c(1,2,3,4,5,6,7,8,9),20]),mean(Output.model$q97.5$psi[c(3),c(1,2,3,4,5,6,7,8,9),20]),mean(Output.model$q97.5$psi[c(4),c(1,2,3,4,5,6,7,8,9),20]),
                             mean(Output.model$q97.5$psi[c(6),c(1,2,3,4,5,6,7,8,9),20]),mean(Output.model$q97.5$psi[c(7),c(1,2,3,4,5,6,7,8,9),20]),mean(Output.model$q97.5$psi[c(8),c(1,2,3,4,5,6,7,8,9),20]),
                             mean(Output.model$q97.5$psi[c(9),c(1,2,3,4,5,6,7,8,9),20]),mean(Output.model$q97.5$psi[c(10),c(1,2,3,4,5,6,7,8,9),20]),mean(Output.model$q97.5$psi[c(11),c(1,2),20]),
                             mean(Output.model$q97.5$psi[c(12),c(1,2),20]),mean(Output.model$q97.5$psi[c(13),c(1,2),20]),mean(Output.model$q97.5$psi[c(14),c(1,2),20]),
                             mean(Output.model$q97.5$psi[c(15),c(1,2),20]),mean(Output.model$q97.5$psi[c(16),c(1,2),20]),mean(Output.model$q97.5$psi[c(17),c(1,2),20]),
                             mean(Output.model$q97.5$psi[c(18),c(1,2),20]),mean(Output.model$q97.5$psi[c(19),c(1,2),20]))),
                      mean(c(mean(Output.model$q97.5$psi[c(2),c(1,2,3,4,5,6,7,8,9),23]),mean(Output.model$q97.5$psi[c(3),c(1,2,3,4,5,6,7,8,9),23]),mean(Output.model$q97.5$psi[c(4),c(1,2,3,4,5,6,7,8,9),23]),
                             mean(Output.model$q97.5$psi[c(6),c(1,2,3,4,5,6,7,8,9),23]),mean(Output.model$q97.5$psi[c(7),c(1,2,3,4,5,6,7,8,9),23]),mean(Output.model$q97.5$psi[c(8),c(1,2,3,4,5,6,7,8,9),23]),
                             mean(Output.model$q97.5$psi[c(9),c(1,2,3,4,5,6,7,8,9),23]),mean(Output.model$q97.5$psi[c(10),c(1,2,3,4,5,6,7,8,9),23]),mean(Output.model$q97.5$psi[c(11),c(1,2),23]),
                             mean(Output.model$q97.5$psi[c(12),c(1,2),23]),mean(Output.model$q97.5$psi[c(13),c(1,2),23]),mean(Output.model$q97.5$psi[c(14),c(1,2),23]),
                             mean(Output.model$q97.5$psi[c(15),c(1,2),23]),mean(Output.model$q97.5$psi[c(16),c(1,2),23]),mean(Output.model$q97.5$psi[c(17),c(1,2),23]),
                             mean(Output.model$q97.5$psi[c(18),c(1,2),23]),mean(Output.model$q97.5$psi[c(19),c(1,2),23]))),
                      mean(c(mean(Output.model$q97.5$psi[c(2),c(1,2,3,4,5,6,7,8,9),3]),mean(Output.model$q97.5$psi[c(3),c(1,2,3,4,5,6,7,8,9),3]),mean(Output.model$q97.5$psi[c(4),c(1,2,3,4,5,6,7,8,9),3]),
                             mean(Output.model$q97.5$psi[c(6),c(1,2,3,4,5,6,7,8,9),3]),mean(Output.model$q97.5$psi[c(7),c(1,2,3,4,5,6,7,8,9),3]),mean(Output.model$q97.5$psi[c(8),c(1,2,3,4,5,6,7,8,9),3]),
                             mean(Output.model$q97.5$psi[c(9),c(1,2,3,4,5,6,7,8,9),3]),mean(Output.model$q97.5$psi[c(10),c(1,2,3,4,5,6,7,8,9),3]),mean(Output.model$q97.5$psi[c(11),c(1,2),3]),
                             mean(Output.model$q97.5$psi[c(12),c(1,2),3]),mean(Output.model$q97.5$psi[c(13),c(1,2),3]),mean(Output.model$q97.5$psi[c(14),c(1,2),3]),
                             mean(Output.model$q97.5$psi[c(15),c(1,2),3]),mean(Output.model$q97.5$psi[c(16),c(1,2),3]),mean(Output.model$q97.5$psi[c(17),c(1,2),3]),
                             mean(Output.model$q97.5$psi[c(18),c(1,2),3]),mean(Output.model$q97.5$psi[c(19),c(1,2),3]))),
                      mean(c(mean(Output.model$q97.5$psi[c(2),c(1,2,3,4,5,6,7,8,9),27]),mean(Output.model$q97.5$psi[c(3),c(1,2,3,4,5,6,7,8,9),27]),mean(Output.model$q97.5$psi[c(4),c(1,2,3,4,5,6,7,8,9),27]),
                             mean(Output.model$q97.5$psi[c(6),c(1,2,3,4,5,6,7,8,9),27]),mean(Output.model$q97.5$psi[c(7),c(1,2,3,4,5,6,7,8,9),27]),mean(Output.model$q97.5$psi[c(8),c(1,2,3,4,5,6,7,8,9),27]),
                             mean(Output.model$q97.5$psi[c(9),c(1,2,3,4,5,6,7,8,9),27]),mean(Output.model$q97.5$psi[c(10),c(1,2,3,4,5,6,7,8,9),27]),mean(Output.model$q97.5$psi[c(11),c(1,2),27]),
                             mean(Output.model$q97.5$psi[c(12),c(1,2),27]),mean(Output.model$q97.5$psi[c(13),c(1,2),27]),mean(Output.model$q97.5$psi[c(14),c(1,2),27]),
                             mean(Output.model$q97.5$psi[c(15),c(1,2),27]),mean(Output.model$q97.5$psi[c(16),c(1,2),27]),mean(Output.model$q97.5$psi[c(17),c(1,2),27]),
                             mean(Output.model$q97.5$psi[c(18),c(1,2),27]),mean(Output.model$q97.5$psi[c(19),c(1,2),27]))),
                      mean(c(mean(Output.model$q97.5$psi[c(2),c(1,2,3,4,5,6,7,8,9),26]),mean(Output.model$q97.5$psi[c(3),c(1,2,3,4,5,6,7,8,9),26]),mean(Output.model$q97.5$psi[c(4),c(1,2,3,4,5,6,7,8,9),26]),
                             mean(Output.model$q97.5$psi[c(6),c(1,2,3,4,5,6,7,8,9),26]),mean(Output.model$q97.5$psi[c(7),c(1,2,3,4,5,6,7,8,9),26]),mean(Output.model$q97.5$psi[c(8),c(1,2,3,4,5,6,7,8,9),26]),
                             mean(Output.model$q97.5$psi[c(9),c(1,2,3,4,5,6,7,8,9),26]),mean(Output.model$q97.5$psi[c(10),c(1,2,3,4,5,6,7,8,9),26]),mean(Output.model$q97.5$psi[c(11),c(1,2),26]),
                             mean(Output.model$q97.5$psi[c(12),c(1,2),26]),mean(Output.model$q97.5$psi[c(13),c(1,2),26]),mean(Output.model$q97.5$psi[c(14),c(1,2),26]),
                             mean(Output.model$q97.5$psi[c(15),c(1,2),26]),mean(Output.model$q97.5$psi[c(16),c(1,2),26]),mean(Output.model$q97.5$psi[c(17),c(1,2),26]),
                             mean(Output.model$q97.5$psi[c(18),c(1,2),26]),mean(Output.model$q97.5$psi[c(19),c(1,2),26]))),
                      mean(c(mean(Output.model$q97.5$psi[c(2),c(1,2,3,4,5,6,7,8,9),44]),mean(Output.model$q97.5$psi[c(3),c(1,2,3,4,5,6,7,8,9),44]),mean(Output.model$q97.5$psi[c(4),c(1,2,3,4,5,6,7,8,9),44]),
                             mean(Output.model$q97.5$psi[c(6),c(1,2,3,4,5,6,7,8,9),44]),mean(Output.model$q97.5$psi[c(7),c(1,2,3,4,5,6,7,8,9),44]),mean(Output.model$q97.5$psi[c(8),c(1,2,3,4,5,6,7,8,9),44]),
                             mean(Output.model$q97.5$psi[c(9),c(1,2,3,4,5,6,7,8,9),44]),mean(Output.model$q97.5$psi[c(10),c(1,2,3,4,5,6,7,8,9),44]),mean(Output.model$q97.5$psi[c(11),c(1,2),44]),
                             mean(Output.model$q97.5$psi[c(12),c(1,2),44]),mean(Output.model$q97.5$psi[c(13),c(1,2),44]),mean(Output.model$q97.5$psi[c(14),c(1,2),44]),
                             mean(Output.model$q97.5$psi[c(15),c(1,2),44]),mean(Output.model$q97.5$psi[c(16),c(1,2),44]),mean(Output.model$q97.5$psi[c(17),c(1,2),44]),
                             mean(Output.model$q97.5$psi[c(18),c(1,2),44]),mean(Output.model$q97.5$psi[c(19),c(1,2),44]))),
                      mean(c(mean(Output.model$q97.5$psi[c(2),c(1,2,3,4,5,6,7,8,9),42]),mean(Output.model$q97.5$psi[c(3),c(1,2,3,4,5,6,7,8,9),42]),mean(Output.model$q97.5$psi[c(4),c(1,2,3,4,5,6,7,8,9),42]),
                             mean(Output.model$q97.5$psi[c(6),c(1,2,3,4,5,6,7,8,9),42]),mean(Output.model$q97.5$psi[c(7),c(1,2,3,4,5,6,7,8,9),42]),mean(Output.model$q97.5$psi[c(8),c(1,2,3,4,5,6,7,8,9),42]),
                             mean(Output.model$q97.5$psi[c(9),c(1,2,3,4,5,6,7,8,9),42]),mean(Output.model$q97.5$psi[c(10),c(1,2,3,4,5,6,7,8,9),42]),mean(Output.model$q97.5$psi[c(11),c(1,2),42]),
                             mean(Output.model$q97.5$psi[c(12),c(1,2),42]),mean(Output.model$q97.5$psi[c(13),c(1,2),42]),mean(Output.model$q97.5$psi[c(14),c(1,2),42]),
                             mean(Output.model$q97.5$psi[c(15),c(1,2),42]),mean(Output.model$q97.5$psi[c(16),c(1,2),42]),mean(Output.model$q97.5$psi[c(17),c(1,2),42]),
                             mean(Output.model$q97.5$psi[c(18),c(1,2),42]),mean(Output.model$q97.5$psi[c(19),c(1,2),42]))),
                      mean(c(mean(Output.model$q97.5$psi[c(2),c(1,2,3,4,5,6,7,8,9),43]),mean(Output.model$q97.5$psi[c(3),c(1,2,3,4,5,6,7,8,9),43]),mean(Output.model$q97.5$psi[c(4),c(1,2,3,4,5,6,7,8,9),43]),
                             mean(Output.model$q97.5$psi[c(6),c(1,2,3,4,5,6,7,8,9),43]),mean(Output.model$q97.5$psi[c(7),c(1,2,3,4,5,6,7,8,9),43]),mean(Output.model$q97.5$psi[c(8),c(1,2,3,4,5,6,7,8,9),43]),
                             mean(Output.model$q97.5$psi[c(9),c(1,2,3,4,5,6,7,8,9),43]),mean(Output.model$q97.5$psi[c(10),c(1,2,3,4,5,6,7,8,9),43]),mean(Output.model$q97.5$psi[c(11),c(1,2),43]),
                             mean(Output.model$q97.5$psi[c(12),c(1,2),43]),mean(Output.model$q97.5$psi[c(13),c(1,2),43]),mean(Output.model$q97.5$psi[c(14),c(1,2),43]),
                             mean(Output.model$q97.5$psi[c(15),c(1,2),43]),mean(Output.model$q97.5$psi[c(16),c(1,2),43]),mean(Output.model$q97.5$psi[c(17),c(1,2),43]),
                             mean(Output.model$q97.5$psi[c(18),c(1,2),43]),mean(Output.model$q97.5$psi[c(19),c(1,2),43]))),
                      mean(c(mean(Output.model$q97.5$psi[c(2),c(1,2,3,4,5,6,7,8,9),38]),mean(Output.model$q97.5$psi[c(3),c(1,2,3,4,5,6,7,8,9),38]),mean(Output.model$q97.5$psi[c(4),c(1,2,3,4,5,6,7,8,9),38]),
                             mean(Output.model$q97.5$psi[c(6),c(1,2,3,4,5,6,7,8,9),38]),mean(Output.model$q97.5$psi[c(7),c(1,2,3,4,5,6,7,8,9),38]),mean(Output.model$q97.5$psi[c(8),c(1,2,3,4,5,6,7,8,9),38]),
                             mean(Output.model$q97.5$psi[c(9),c(1,2,3,4,5,6,7,8,9),38]),mean(Output.model$q97.5$psi[c(10),c(1,2,3,4,5,6,7,8,9),38]),mean(Output.model$q97.5$psi[c(11),c(1,2),38]),
                             mean(Output.model$q97.5$psi[c(12),c(1,2),38]),mean(Output.model$q97.5$psi[c(13),c(1,2),38]),mean(Output.model$q97.5$psi[c(14),c(1,2),38]),
                             mean(Output.model$q97.5$psi[c(15),c(1,2),38]),mean(Output.model$q97.5$psi[c(16),c(1,2),38]),mean(Output.model$q97.5$psi[c(17),c(1,2),38]),
                             mean(Output.model$q97.5$psi[c(18),c(1,2),38]),mean(Output.model$q97.5$psi[c(19),c(1,2),38]))),
                      mean(c(mean(Output.model$q97.5$psi[c(2),c(1,2,3,4,5,6,7,8,9),24]),mean(Output.model$q97.5$psi[c(3),c(1,2,3,4,5,6,7,8,9),24]),mean(Output.model$q97.5$psi[c(4),c(1,2,3,4,5,6,7,8,9),24]),
                             mean(Output.model$q97.5$psi[c(6),c(1,2,3,4,5,6,7,8,9),24]),mean(Output.model$q97.5$psi[c(7),c(1,2,3,4,5,6,7,8,9),24]),mean(Output.model$q97.5$psi[c(8),c(1,2,3,4,5,6,7,8,9),24]),
                             mean(Output.model$q97.5$psi[c(9),c(1,2,3,4,5,6,7,8,9),24]),mean(Output.model$q97.5$psi[c(10),c(1,2,3,4,5,6,7,8,9),24]),mean(Output.model$q97.5$psi[c(11),c(1,2),24]),
                             mean(Output.model$q97.5$psi[c(12),c(1,2),24]),mean(Output.model$q97.5$psi[c(13),c(1,2),24]),mean(Output.model$q97.5$psi[c(14),c(1,2),24]),
                             mean(Output.model$q97.5$psi[c(15),c(1,2),24]),mean(Output.model$q97.5$psi[c(16),c(1,2),24]),mean(Output.model$q97.5$psi[c(17),c(1,2),24]),
                             mean(Output.model$q97.5$psi[c(18),c(1,2),24]),mean(Output.model$q97.5$psi[c(19),c(1,2),24]))),
                      mean(c(mean(Output.model$q97.5$psi[c(2),c(1,2,3,4,5,6,7,8,9),37]),mean(Output.model$q97.5$psi[c(3),c(1,2,3,4,5,6,7,8,9),37]),mean(Output.model$q97.5$psi[c(4),c(1,2,3,4,5,6,7,8,9),37]),
                             mean(Output.model$q97.5$psi[c(6),c(1,2,3,4,5,6,7,8,9),37]),mean(Output.model$q97.5$psi[c(7),c(1,2,3,4,5,6,7,8,9),37]),mean(Output.model$q97.5$psi[c(8),c(1,2,3,4,5,6,7,8,9),37]),
                             mean(Output.model$q97.5$psi[c(9),c(1,2,3,4,5,6,7,8,9),37]),mean(Output.model$q97.5$psi[c(10),c(1,2,3,4,5,6,7,8,9),37]),mean(Output.model$q97.5$psi[c(11),c(1,2),37]),
                             mean(Output.model$q97.5$psi[c(12),c(1,2),37]),mean(Output.model$q97.5$psi[c(13),c(1,2),37]),mean(Output.model$q97.5$psi[c(14),c(1,2),37]),
                             mean(Output.model$q97.5$psi[c(15),c(1,2),37]),mean(Output.model$q97.5$psi[c(16),c(1,2),37]),mean(Output.model$q97.5$psi[c(17),c(1,2),37]),
                             mean(Output.model$q97.5$psi[c(18),c(1,2),37]),mean(Output.model$q97.5$psi[c(19),c(1,2),37]))),
                      mean(c(mean(Output.model$q97.5$psi[c(2),c(1,2,3,4,5,6,7,8,9),13]),mean(Output.model$q97.5$psi[c(3),c(1,2,3,4,5,6,7,8,9),13]),mean(Output.model$q97.5$psi[c(4),c(1,2,3,4,5,6,7,8,9),13]),
                             mean(Output.model$q97.5$psi[c(6),c(1,2,3,4,5,6,7,8,9),13]),mean(Output.model$q97.5$psi[c(7),c(1,2,3,4,5,6,7,8,9),13]),mean(Output.model$q97.5$psi[c(8),c(1,2,3,4,5,6,7,8,9),13]),
                             mean(Output.model$q97.5$psi[c(9),c(1,2,3,4,5,6,7,8,9),13]),mean(Output.model$q97.5$psi[c(10),c(1,2,3,4,5,6,7,8,9),13]),mean(Output.model$q97.5$psi[c(11),c(1,2),13]),
                             mean(Output.model$q97.5$psi[c(12),c(1,2),13]),mean(Output.model$q97.5$psi[c(13),c(1,2),13]),mean(Output.model$q97.5$psi[c(14),c(1,2),13]),
                             mean(Output.model$q97.5$psi[c(15),c(1,2),13]),mean(Output.model$q97.5$psi[c(16),c(1,2),13]),mean(Output.model$q97.5$psi[c(17),c(1,2),13]),
                             mean(Output.model$q97.5$psi[c(18),c(1,2),13]),mean(Output.model$q97.5$psi[c(19),c(1,2),13]))),
                      mean(c(mean(Output.model$q97.5$psi[c(2),c(1,2,3,4,5,6,7,8,9),21]),mean(Output.model$q97.5$psi[c(3),c(1,2,3,4,5,6,7,8,9),21]),mean(Output.model$q97.5$psi[c(4),c(1,2,3,4,5,6,7,8,9),21]),
                             mean(Output.model$q97.5$psi[c(6),c(1,2,3,4,5,6,7,8,9),21]),mean(Output.model$q97.5$psi[c(7),c(1,2,3,4,5,6,7,8,9),21]),mean(Output.model$q97.5$psi[c(8),c(1,2,3,4,5,6,7,8,9),21]),
                             mean(Output.model$q97.5$psi[c(9),c(1,2,3,4,5,6,7,8,9),21]),mean(Output.model$q97.5$psi[c(10),c(1,2,3,4,5,6,7,8,9),21]),mean(Output.model$q97.5$psi[c(11),c(1,2),21]),
                             mean(Output.model$q97.5$psi[c(12),c(1,2),21]),mean(Output.model$q97.5$psi[c(13),c(1,2),21]),mean(Output.model$q97.5$psi[c(14),c(1,2),21]),
                             mean(Output.model$q97.5$psi[c(15),c(1,2),21]),mean(Output.model$q97.5$psi[c(16),c(1,2),21]),mean(Output.model$q97.5$psi[c(17),c(1,2),21]),
                             mean(Output.model$q97.5$psi[c(18),c(1,2),21]),mean(Output.model$q97.5$psi[c(19),c(1,2),21]))),
                      mean(c(mean(Output.model$q97.5$psi[c(2),c(1,2,3,4,5,6,7,8,9),34]),mean(Output.model$q97.5$psi[c(3),c(1,2,3,4,5,6,7,8,9),34]),mean(Output.model$q97.5$psi[c(4),c(1,2,3,4,5,6,7,8,9),34]),
                             mean(Output.model$q97.5$psi[c(6),c(1,2,3,4,5,6,7,8,9),34]),mean(Output.model$q97.5$psi[c(7),c(1,2,3,4,5,6,7,8,9),34]),mean(Output.model$q97.5$psi[c(8),c(1,2,3,4,5,6,7,8,9),34]),
                             mean(Output.model$q97.5$psi[c(9),c(1,2,3,4,5,6,7,8,9),34]),mean(Output.model$q97.5$psi[c(10),c(1,2,3,4,5,6,7,8,9),34]),mean(Output.model$q97.5$psi[c(11),c(1,2),34]),
                             mean(Output.model$q97.5$psi[c(12),c(1,2),34]),mean(Output.model$q97.5$psi[c(13),c(1,2),34]),mean(Output.model$q97.5$psi[c(14),c(1,2),34]),
                             mean(Output.model$q97.5$psi[c(15),c(1,2),34]),mean(Output.model$q97.5$psi[c(16),c(1,2),34]),mean(Output.model$q97.5$psi[c(17),c(1,2),34]),
                             mean(Output.model$q97.5$psi[c(18),c(1,2),34]),mean(Output.model$q97.5$psi[c(19),c(1,2),34]))),
                      mean(c(mean(Output.model$q97.5$psi[c(2),c(1,2,3,4,5,6,7,8,9),2]),mean(Output.model$q97.5$psi[c(3),c(1,2,3,4,5,6,7,8,9),2]),mean(Output.model$q97.5$psi[c(4),c(1,2,3,4,5,6,7,8,9),2]),
                             mean(Output.model$q97.5$psi[c(6),c(1,2,3,4,5,6,7,8,9),2]),mean(Output.model$q97.5$psi[c(7),c(1,2,3,4,5,6,7,8,9),2]),mean(Output.model$q97.5$psi[c(8),c(1,2,3,4,5,6,7,8,9),2]),
                             mean(Output.model$q97.5$psi[c(9),c(1,2,3,4,5,6,7,8,9),2]),mean(Output.model$q97.5$psi[c(10),c(1,2,3,4,5,6,7,8,9),2]),mean(Output.model$q97.5$psi[c(11),c(1,2),2]),
                             mean(Output.model$q97.5$psi[c(12),c(1,2),2]),mean(Output.model$q97.5$psi[c(13),c(1,2),2]),mean(Output.model$q97.5$psi[c(14),c(1,2),2]),
                             mean(Output.model$q97.5$psi[c(15),c(1,2),2]),mean(Output.model$q97.5$psi[c(16),c(1,2),2]),mean(Output.model$q97.5$psi[c(17),c(1,2),2]),
                             mean(Output.model$q97.5$psi[c(18),c(1,2),2]),mean(Output.model$q97.5$psi[c(19),c(1,2),2]))),
                      mean(c(mean(Output.model$q97.5$psi[c(2),c(1,2,3,4,5,6,7,8,9),32]),mean(Output.model$q97.5$psi[c(3),c(1,2,3,4,5,6,7,8,9),32]),mean(Output.model$q97.5$psi[c(4),c(1,2,3,4,5,6,7,8,9),32]),
                             mean(Output.model$q97.5$psi[c(6),c(1,2,3,4,5,6,7,8,9),32]),mean(Output.model$q97.5$psi[c(7),c(1,2,3,4,5,6,7,8,9),32]),mean(Output.model$q97.5$psi[c(8),c(1,2,3,4,5,6,7,8,9),32]),
                             mean(Output.model$q97.5$psi[c(9),c(1,2,3,4,5,6,7,8,9),32]),mean(Output.model$q97.5$psi[c(10),c(1,2,3,4,5,6,7,8,9),32]),mean(Output.model$q97.5$psi[c(11),c(1,2),32]),
                             mean(Output.model$q97.5$psi[c(12),c(1,2),32]),mean(Output.model$q97.5$psi[c(13),c(1,2),32]),mean(Output.model$q97.5$psi[c(14),c(1,2),32]),
                             mean(Output.model$q97.5$psi[c(15),c(1,2),32]),mean(Output.model$q97.5$psi[c(16),c(1,2),32]),mean(Output.model$q97.5$psi[c(17),c(1,2),32]),
                             mean(Output.model$q97.5$psi[c(18),c(1,2),32]),mean(Output.model$q97.5$psi[c(19),c(1,2),32]))),
                      mean(c(mean(Output.model$q97.5$psi[c(2),c(1,2,3,4,5,6,7,8,9),17]),mean(Output.model$q97.5$psi[c(3),c(1,2,3,4,5,6,7,8,9),17]),mean(Output.model$q97.5$psi[c(4),c(1,2,3,4,5,6,7,8,9),17]),
                             mean(Output.model$q97.5$psi[c(6),c(1,2,3,4,5,6,7,8,9),17]),mean(Output.model$q97.5$psi[c(7),c(1,2,3,4,5,6,7,8,9),17]),mean(Output.model$q97.5$psi[c(8),c(1,2,3,4,5,6,7,8,9),17]),
                             mean(Output.model$q97.5$psi[c(9),c(1,2,3,4,5,6,7,8,9),17]),mean(Output.model$q97.5$psi[c(10),c(1,2,3,4,5,6,7,8,9),17]),mean(Output.model$q97.5$psi[c(11),c(1,2),17]),
                             mean(Output.model$q97.5$psi[c(12),c(1,2),17]),mean(Output.model$q97.5$psi[c(13),c(1,2),17]),mean(Output.model$q97.5$psi[c(14),c(1,2),17]),
                             mean(Output.model$q97.5$psi[c(15),c(1,2),17]),mean(Output.model$q97.5$psi[c(16),c(1,2),17]),mean(Output.model$q97.5$psi[c(17),c(1,2),17]),
                             mean(Output.model$q97.5$psi[c(18),c(1,2),17]),mean(Output.model$q97.5$psi[c(19),c(1,2),17]))),
                      mean(c(mean(Output.model$q97.5$psi[c(2),c(1,2,3,4,5,6,7,8,9),4]),mean(Output.model$q97.5$psi[c(3),c(1,2,3,4,5,6,7,8,9),4]),mean(Output.model$q97.5$psi[c(4),c(1,2,3,4,5,6,7,8,9),4]),
                             mean(Output.model$q97.5$psi[c(6),c(1,2,3,4,5,6,7,8,9),4]),mean(Output.model$q97.5$psi[c(7),c(1,2,3,4,5,6,7,8,9),4]),mean(Output.model$q97.5$psi[c(8),c(1,2,3,4,5,6,7,8,9),4]),
                             mean(Output.model$q97.5$psi[c(9),c(1,2,3,4,5,6,7,8,9),4]),mean(Output.model$q97.5$psi[c(10),c(1,2,3,4,5,6,7,8,9),4]),mean(Output.model$q97.5$psi[c(11),c(1,2),4]),
                             mean(Output.model$q97.5$psi[c(12),c(1,2),4]),mean(Output.model$q97.5$psi[c(13),c(1,2),4]),mean(Output.model$q97.5$psi[c(14),c(1,2),4]),
                             mean(Output.model$q97.5$psi[c(15),c(1,2),4]),mean(Output.model$q97.5$psi[c(16),c(1,2),4]),mean(Output.model$q97.5$psi[c(17),c(1,2),4]),
                             mean(Output.model$q97.5$psi[c(18),c(1,2),4]),mean(Output.model$q97.5$psi[c(19),c(1,2),4]))),
                      mean(c(mean(Output.model$q97.5$psi[c(2),c(1,2,3,4,5,6,7,8,9),8]),mean(Output.model$q97.5$psi[c(3),c(1,2,3,4,5,6,7,8,9),8]),mean(Output.model$q97.5$psi[c(4),c(1,2,3,4,5,6,7,8,9),8]),
                             mean(Output.model$q97.5$psi[c(6),c(1,2,3,4,5,6,7,8,9),8]),mean(Output.model$q97.5$psi[c(7),c(1,2,3,4,5,6,7,8,9),8]),mean(Output.model$q97.5$psi[c(8),c(1,2,3,4,5,6,7,8,9),8]),
                             mean(Output.model$q97.5$psi[c(9),c(1,2,3,4,5,6,7,8,9),8]),mean(Output.model$q97.5$psi[c(10),c(1,2,3,4,5,6,7,8,9),8]),mean(Output.model$q97.5$psi[c(11),c(1,2),8]),
                             mean(Output.model$q97.5$psi[c(12),c(1,2),8]),mean(Output.model$q97.5$psi[c(13),c(1,2),8]),mean(Output.model$q97.5$psi[c(14),c(1,2),8]),
                             mean(Output.model$q97.5$psi[c(15),c(1,2),8]),mean(Output.model$q97.5$psi[c(16),c(1,2),8]),mean(Output.model$q97.5$psi[c(17),c(1,2),8]),
                             mean(Output.model$q97.5$psi[c(18),c(1,2),8]),mean(Output.model$q97.5$psi[c(19),c(1,2),8]))),
                      mean(c(mean(Output.model$q97.5$psi[c(2),c(1,2,3,4,5,6,7,8,9),39]),mean(Output.model$q97.5$psi[c(3),c(1,2,3,4,5,6,7,8,9),39]),mean(Output.model$q97.5$psi[c(4),c(1,2,3,4,5,6,7,8,9),39]),
                             mean(Output.model$q97.5$psi[c(6),c(1,2,3,4,5,6,7,8,9),39]),mean(Output.model$q97.5$psi[c(7),c(1,2,3,4,5,6,7,8,9),39]),mean(Output.model$q97.5$psi[c(8),c(1,2,3,4,5,6,7,8,9),39]),
                             mean(Output.model$q97.5$psi[c(9),c(1,2,3,4,5,6,7,8,9),39]),mean(Output.model$q97.5$psi[c(10),c(1,2,3,4,5,6,7,8,9),39]),mean(Output.model$q97.5$psi[c(11),c(1,2),39]),
                             mean(Output.model$q97.5$psi[c(12),c(1,2),39]),mean(Output.model$q97.5$psi[c(13),c(1,2),39]),mean(Output.model$q97.5$psi[c(14),c(1,2),39]),
                             mean(Output.model$q97.5$psi[c(15),c(1,2),39]),mean(Output.model$q97.5$psi[c(16),c(1,2),39]),mean(Output.model$q97.5$psi[c(17),c(1,2),39]),
                             mean(Output.model$q97.5$psi[c(18),c(1,2),39]),mean(Output.model$q97.5$psi[c(19),c(1,2),39]))),
                      mean(c(mean(Output.model$q97.5$psi[c(2),c(1,2,3,4,5,6,7,8,9),10]),mean(Output.model$q97.5$psi[c(3),c(1,2,3,4,5,6,7,8,9),10]),mean(Output.model$q97.5$psi[c(4),c(1,2,3,4,5,6,7,8,9),10]),
                             mean(Output.model$q97.5$psi[c(6),c(1,2,3,4,5,6,7,8,9),10]),mean(Output.model$q97.5$psi[c(7),c(1,2,3,4,5,6,7,8,9),10]),mean(Output.model$q97.5$psi[c(8),c(1,2,3,4,5,6,7,8,9),10]),
                             mean(Output.model$q97.5$psi[c(9),c(1,2,3,4,5,6,7,8,9),10]),mean(Output.model$q97.5$psi[c(10),c(1,2,3,4,5,6,7,8,9),10]),mean(Output.model$q97.5$psi[c(11),c(1,2),10]),
                             mean(Output.model$q97.5$psi[c(12),c(1,2),10]),mean(Output.model$q97.5$psi[c(13),c(1,2),10]),mean(Output.model$q97.5$psi[c(14),c(1,2),10]),
                             mean(Output.model$q97.5$psi[c(15),c(1,2),10]),mean(Output.model$q97.5$psi[c(16),c(1,2),10]),mean(Output.model$q97.5$psi[c(17),c(1,2),10]),
                             mean(Output.model$q97.5$psi[c(18),c(1,2),10]),mean(Output.model$q97.5$psi[c(19),c(1,2),10]))),
                      mean(c(mean(Output.model$q97.5$psi[c(2),c(1,2,3,4,5,6,7,8,9),19]),mean(Output.model$q97.5$psi[c(3),c(1,2,3,4,5,6,7,8,9),19]),mean(Output.model$q97.5$psi[c(4),c(1,2,3,4,5,6,7,8,9),19]),
                             mean(Output.model$q97.5$psi[c(6),c(1,2,3,4,5,6,7,8,9),19]),mean(Output.model$q97.5$psi[c(7),c(1,2,3,4,5,6,7,8,9),19]),mean(Output.model$q97.5$psi[c(8),c(1,2,3,4,5,6,7,8,9),19]),
                             mean(Output.model$q97.5$psi[c(9),c(1,2,3,4,5,6,7,8,9),19]),mean(Output.model$q97.5$psi[c(10),c(1,2,3,4,5,6,7,8,9),19]),mean(Output.model$q97.5$psi[c(11),c(1,2),19]),
                             mean(Output.model$q97.5$psi[c(12),c(1,2),19]),mean(Output.model$q97.5$psi[c(13),c(1,2),19]),mean(Output.model$q97.5$psi[c(14),c(1,2),19]),
                             mean(Output.model$q97.5$psi[c(15),c(1,2),19]),mean(Output.model$q97.5$psi[c(16),c(1,2),19]),mean(Output.model$q97.5$psi[c(17),c(1,2),19]),
                             mean(Output.model$q97.5$psi[c(18),c(1,2),19]),mean(Output.model$q97.5$psi[c(19),c(1,2),19]))),
                      mean(c(mean(Output.model$q97.5$psi[c(2),c(1,2,3,4,5,6,7,8,9),35]),mean(Output.model$q97.5$psi[c(3),c(1,2,3,4,5,6,7,8,9),35]),mean(Output.model$q97.5$psi[c(4),c(1,2,3,4,5,6,7,8,9),35]),
                             mean(Output.model$q97.5$psi[c(6),c(1,2,3,4,5,6,7,8,9),35]),mean(Output.model$q97.5$psi[c(7),c(1,2,3,4,5,6,7,8,9),35]),mean(Output.model$q97.5$psi[c(8),c(1,2,3,4,5,6,7,8,9),35]),
                             mean(Output.model$q97.5$psi[c(9),c(1,2,3,4,5,6,7,8,9),35]),mean(Output.model$q97.5$psi[c(10),c(1,2,3,4,5,6,7,8,9),35]),mean(Output.model$q97.5$psi[c(11),c(1,2),35]),
                             mean(Output.model$q97.5$psi[c(12),c(1,2),35]),mean(Output.model$q97.5$psi[c(13),c(1,2),35]),mean(Output.model$q97.5$psi[c(14),c(1,2),35]),
                             mean(Output.model$q97.5$psi[c(15),c(1,2),35]),mean(Output.model$q97.5$psi[c(16),c(1,2),35]),mean(Output.model$q97.5$psi[c(17),c(1,2),35]),
                             mean(Output.model$q97.5$psi[c(18),c(1,2),35]),mean(Output.model$q97.5$psi[c(19),c(1,2),35]))),
                      mean(c(mean(Output.model$q97.5$psi[c(2),c(1,2,3,4,5,6,7,8,9),25]),mean(Output.model$q97.5$psi[c(3),c(1,2,3,4,5,6,7,8,9),25]),mean(Output.model$q97.5$psi[c(4),c(1,2,3,4,5,6,7,8,9),25]),
                             mean(Output.model$q97.5$psi[c(6),c(1,2,3,4,5,6,7,8,9),25]),mean(Output.model$q97.5$psi[c(7),c(1,2,3,4,5,6,7,8,9),25]),mean(Output.model$q97.5$psi[c(8),c(1,2,3,4,5,6,7,8,9),25]),
                             mean(Output.model$q97.5$psi[c(9),c(1,2,3,4,5,6,7,8,9),25]),mean(Output.model$q97.5$psi[c(10),c(1,2,3,4,5,6,7,8,9),25]),mean(Output.model$q97.5$psi[c(11),c(1,2),25]),
                             mean(Output.model$q97.5$psi[c(12),c(1,2),25]),mean(Output.model$q97.5$psi[c(13),c(1,2),25]),mean(Output.model$q97.5$psi[c(14),c(1,2),25]),
                             mean(Output.model$q97.5$psi[c(15),c(1,2),25]),mean(Output.model$q97.5$psi[c(16),c(1,2),25]),mean(Output.model$q97.5$psi[c(17),c(1,2),25]),
                             mean(Output.model$q97.5$psi[c(18),c(1,2),25]),mean(Output.model$q97.5$psi[c(19),c(1,2),25]))),
                      mean(c(mean(Output.model$q97.5$psi[c(2),c(1,2,3,4,5,6,7,8,9),28]),mean(Output.model$q97.5$psi[c(3),c(1,2,3,4,5,6,7,8,9),28]),mean(Output.model$q97.5$psi[c(4),c(1,2,3,4,5,6,7,8,9),28]),
                             mean(Output.model$q97.5$psi[c(6),c(1,2,3,4,5,6,7,8,9),28]),mean(Output.model$q97.5$psi[c(7),c(1,2,3,4,5,6,7,8,9),28]),mean(Output.model$q97.5$psi[c(8),c(1,2,3,4,5,6,7,8,9),28]),
                             mean(Output.model$q97.5$psi[c(9),c(1,2,3,4,5,6,7,8,9),28]),mean(Output.model$q97.5$psi[c(10),c(1,2,3,4,5,6,7,8,9),28]),mean(Output.model$q97.5$psi[c(11),c(1,2),28]),
                             mean(Output.model$q97.5$psi[c(12),c(1,2),28]),mean(Output.model$q97.5$psi[c(13),c(1,2),28]),mean(Output.model$q97.5$psi[c(14),c(1,2),28]),
                             mean(Output.model$q97.5$psi[c(15),c(1,2),28]),mean(Output.model$q97.5$psi[c(16),c(1,2),28]),mean(Output.model$q97.5$psi[c(17),c(1,2),28]),
                             mean(Output.model$q97.5$psi[c(18),c(1,2),28]),mean(Output.model$q97.5$psi[c(19),c(1,2),28]))),
                      mean(c(mean(Output.model$q97.5$psi[c(2),c(1,2,3,4,5,6,7,8,9),46]),mean(Output.model$q97.5$psi[c(3),c(1,2,3,4,5,6,7,8,9),46]),mean(Output.model$q97.5$psi[c(4),c(1,2,3,4,5,6,7,8,9),46]),
                             mean(Output.model$q97.5$psi[c(6),c(1,2,3,4,5,6,7,8,9),46]),mean(Output.model$q97.5$psi[c(7),c(1,2,3,4,5,6,7,8,9),46]),mean(Output.model$q97.5$psi[c(8),c(1,2,3,4,5,6,7,8,9),46]),
                             mean(Output.model$q97.5$psi[c(9),c(1,2,3,4,5,6,7,8,9),46]),mean(Output.model$q97.5$psi[c(10),c(1,2,3,4,5,6,7,8,9),46]),mean(Output.model$q97.5$psi[c(11),c(1,2),46]),
                             mean(Output.model$q97.5$psi[c(12),c(1,2),46]),mean(Output.model$q97.5$psi[c(13),c(1,2),46]),mean(Output.model$q97.5$psi[c(14),c(1,2),46]),
                             mean(Output.model$q97.5$psi[c(15),c(1,2),46]),mean(Output.model$q97.5$psi[c(16),c(1,2),46]),mean(Output.model$q97.5$psi[c(17),c(1,2),46]),
                             mean(Output.model$q97.5$psi[c(18),c(1,2),46]),mean(Output.model$q97.5$psi[c(19),c(1,2),46]))),
                      mean(c(mean(Output.model$q97.5$psi[c(2),c(1,2,3,4,5,6,7,8,9),41]),mean(Output.model$q97.5$psi[c(3),c(1,2,3,4,5,6,7,8,9),41]),mean(Output.model$q97.5$psi[c(4),c(1,2,3,4,5,6,7,8,9),41]),
                             mean(Output.model$q97.5$psi[c(6),c(1,2,3,4,5,6,7,8,9),41]),mean(Output.model$q97.5$psi[c(7),c(1,2,3,4,5,6,7,8,9),41]),mean(Output.model$q97.5$psi[c(8),c(1,2,3,4,5,6,7,8,9),41]),
                             mean(Output.model$q97.5$psi[c(9),c(1,2,3,4,5,6,7,8,9),41]),mean(Output.model$q97.5$psi[c(10),c(1,2,3,4,5,6,7,8,9),41]),mean(Output.model$q97.5$psi[c(11),c(1,2),41]),
                             mean(Output.model$q97.5$psi[c(12),c(1,2),41]),mean(Output.model$q97.5$psi[c(13),c(1,2),41]),mean(Output.model$q97.5$psi[c(14),c(1,2),41]),
                             mean(Output.model$q97.5$psi[c(15),c(1,2),41]),mean(Output.model$q97.5$psi[c(16),c(1,2),41]),mean(Output.model$q97.5$psi[c(17),c(1,2),41]),
                             mean(Output.model$q97.5$psi[c(18),c(1,2),41]),mean(Output.model$q97.5$psi[c(19),c(1,2),41]))),
                      mean(c(mean(Output.model$q97.5$psi[c(2),c(1,2,3,4,5,6,7,8,9),16]),mean(Output.model$q97.5$psi[c(3),c(1,2,3,4,5,6,7,8,9),16]),mean(Output.model$q97.5$psi[c(4),c(1,2,3,4,5,6,7,8,9),16]),
                             mean(Output.model$q97.5$psi[c(6),c(1,2,3,4,5,6,7,8,9),16]),mean(Output.model$q97.5$psi[c(7),c(1,2,3,4,5,6,7,8,9),16]),mean(Output.model$q97.5$psi[c(8),c(1,2,3,4,5,6,7,8,9),16]),
                             mean(Output.model$q97.5$psi[c(9),c(1,2,3,4,5,6,7,8,9),16]),mean(Output.model$q97.5$psi[c(10),c(1,2,3,4,5,6,7,8,9),16]),mean(Output.model$q97.5$psi[c(11),c(1,2),16]),
                             mean(Output.model$q97.5$psi[c(12),c(1,2),16]),mean(Output.model$q97.5$psi[c(13),c(1,2),16]),mean(Output.model$q97.5$psi[c(14),c(1,2),16]),
                             mean(Output.model$q97.5$psi[c(15),c(1,2),16]),mean(Output.model$q97.5$psi[c(16),c(1,2),16]),mean(Output.model$q97.5$psi[c(17),c(1,2),16]),
                             mean(Output.model$q97.5$psi[c(18),c(1,2),16]),mean(Output.model$q97.5$psi[c(19),c(1,2),16]))),
                      mean(c(mean(Output.model$q97.5$psi[c(2),c(1,2,3,4,5,6,7,8,9),36]),mean(Output.model$q97.5$psi[c(3),c(1,2,3,4,5,6,7,8,9),36]),mean(Output.model$q97.5$psi[c(4),c(1,2,3,4,5,6,7,8,9),36]),
                             mean(Output.model$q97.5$psi[c(6),c(1,2,3,4,5,6,7,8,9),36]),mean(Output.model$q97.5$psi[c(7),c(1,2,3,4,5,6,7,8,9),36]),mean(Output.model$q97.5$psi[c(8),c(1,2,3,4,5,6,7,8,9),36]),
                             mean(Output.model$q97.5$psi[c(9),c(1,2,3,4,5,6,7,8,9),36]),mean(Output.model$q97.5$psi[c(10),c(1,2,3,4,5,6,7,8,9),36]),mean(Output.model$q97.5$psi[c(11),c(1,2),36]),
                             mean(Output.model$q97.5$psi[c(12),c(1,2),36]),mean(Output.model$q97.5$psi[c(13),c(1,2),36]),mean(Output.model$q97.5$psi[c(14),c(1,2),36]),
                             mean(Output.model$q97.5$psi[c(15),c(1,2),36]),mean(Output.model$q97.5$psi[c(16),c(1,2),36]),mean(Output.model$q97.5$psi[c(17),c(1,2),36]),
                             mean(Output.model$q97.5$psi[c(18),c(1,2),36]),mean(Output.model$q97.5$psi[c(19),c(1,2),36]))),
                      mean(c(mean(Output.model$q97.5$psi[c(2),c(1,2,3,4,5,6,7,8,9),14]),mean(Output.model$q97.5$psi[c(3),c(1,2,3,4,5,6,7,8,9),14]),mean(Output.model$q97.5$psi[c(4),c(1,2,3,4,5,6,7,8,9),14]),
                             mean(Output.model$q97.5$psi[c(6),c(1,2,3,4,5,6,7,8,9),14]),mean(Output.model$q97.5$psi[c(7),c(1,2,3,4,5,6,7,8,9),14]),mean(Output.model$q97.5$psi[c(8),c(1,2,3,4,5,6,7,8,9),14]),
                             mean(Output.model$q97.5$psi[c(9),c(1,2,3,4,5,6,7,8,9),14]),mean(Output.model$q97.5$psi[c(10),c(1,2,3,4,5,6,7,8,9),14]),mean(Output.model$q97.5$psi[c(11),c(1,2),14]),
                             mean(Output.model$q97.5$psi[c(12),c(1,2),14]),mean(Output.model$q97.5$psi[c(13),c(1,2),14]),mean(Output.model$q97.5$psi[c(14),c(1,2),14]),
                             mean(Output.model$q97.5$psi[c(15),c(1,2),14]),mean(Output.model$q97.5$psi[c(16),c(1,2),14]),mean(Output.model$q97.5$psi[c(17),c(1,2),14]),
                             mean(Output.model$q97.5$psi[c(18),c(1,2),14]),mean(Output.model$q97.5$psi[c(19),c(1,2),14]))),
                      mean(c(mean(Output.model$q97.5$psi[c(2),c(1,2,3,4,5,6,7,8,9),11]),mean(Output.model$q97.5$psi[c(3),c(1,2,3,4,5,6,7,8,9),11]),mean(Output.model$q97.5$psi[c(4),c(1,2,3,4,5,6,7,8,9),11]),
                             mean(Output.model$q97.5$psi[c(6),c(1,2,3,4,5,6,7,8,9),11]),mean(Output.model$q97.5$psi[c(7),c(1,2,3,4,5,6,7,8,9),11]),mean(Output.model$q97.5$psi[c(8),c(1,2,3,4,5,6,7,8,9),11]),
                             mean(Output.model$q97.5$psi[c(9),c(1,2,3,4,5,6,7,8,9),11]),mean(Output.model$q97.5$psi[c(10),c(1,2,3,4,5,6,7,8,9),11]),mean(Output.model$q97.5$psi[c(11),c(1,2),11]),
                             mean(Output.model$q97.5$psi[c(12),c(1,2),11]),mean(Output.model$q97.5$psi[c(13),c(1,2),11]),mean(Output.model$q97.5$psi[c(14),c(1,2),11]),
                             mean(Output.model$q97.5$psi[c(15),c(1,2),11]),mean(Output.model$q97.5$psi[c(16),c(1,2),11]),mean(Output.model$q97.5$psi[c(17),c(1,2),11]),
                             mean(Output.model$q97.5$psi[c(18),c(1,2),11]),mean(Output.model$q97.5$psi[c(19),c(1,2),11]))),
                      mean(c(mean(Output.model$q97.5$psi[c(2),c(1,2,3,4,5,6,7,8,9),31]),mean(Output.model$q97.5$psi[c(3),c(1,2,3,4,5,6,7,8,9),31]),mean(Output.model$q97.5$psi[c(4),c(1,2,3,4,5,6,7,8,9),31]),
                             mean(Output.model$q97.5$psi[c(6),c(1,2,3,4,5,6,7,8,9),31]),mean(Output.model$q97.5$psi[c(7),c(1,2,3,4,5,6,7,8,9),31]),mean(Output.model$q97.5$psi[c(8),c(1,2,3,4,5,6,7,8,9),31]),
                             mean(Output.model$q97.5$psi[c(9),c(1,2,3,4,5,6,7,8,9),31]),mean(Output.model$q97.5$psi[c(10),c(1,2,3,4,5,6,7,8,9),31]),mean(Output.model$q97.5$psi[c(11),c(1,2),31]),
                             mean(Output.model$q97.5$psi[c(12),c(1,2),31]),mean(Output.model$q97.5$psi[c(13),c(1,2),31]),mean(Output.model$q97.5$psi[c(14),c(1,2),31]),
                             mean(Output.model$q97.5$psi[c(15),c(1,2),31]),mean(Output.model$q97.5$psi[c(16),c(1,2),31]),mean(Output.model$q97.5$psi[c(17),c(1,2),31]),
                             mean(Output.model$q97.5$psi[c(18),c(1,2),31]),mean(Output.model$q97.5$psi[c(19),c(1,2),31]))),
                      mean(c(mean(Output.model$q97.5$psi[c(2),c(1,2,3,4,5,6,7,8,9),6]),mean(Output.model$q97.5$psi[c(3),c(1,2,3,4,5,6,7,8,9),6]),mean(Output.model$q97.5$psi[c(4),c(1,2,3,4,5,6,7,8,9),6]),
                             mean(Output.model$q97.5$psi[c(6),c(1,2,3,4,5,6,7,8,9),6]),mean(Output.model$q97.5$psi[c(7),c(1,2,3,4,5,6,7,8,9),6]),mean(Output.model$q97.5$psi[c(8),c(1,2,3,4,5,6,7,8,9),6]),
                             mean(Output.model$q97.5$psi[c(9),c(1,2,3,4,5,6,7,8,9),6]),mean(Output.model$q97.5$psi[c(10),c(1,2,3,4,5,6,7,8,9),6]),mean(Output.model$q97.5$psi[c(11),c(1,2),6]),
                             mean(Output.model$q97.5$psi[c(12),c(1,2),6]),mean(Output.model$q97.5$psi[c(13),c(1,2),6]),mean(Output.model$q97.5$psi[c(14),c(1,2),6]),
                             mean(Output.model$q97.5$psi[c(15),c(1,2),6]),mean(Output.model$q97.5$psi[c(16),c(1,2),6]),mean(Output.model$q97.5$psi[c(17),c(1,2),6]),
                             mean(Output.model$q97.5$psi[c(18),c(1,2),6]),mean(Output.model$q97.5$psi[c(19),c(1,2),6])))))


sppnamesS1 <- c("10", "11", "12", "13", "14",
               "15", "16", "17", "18", "19",
               "20", "21", "22", "23", "24",
               "25", "26", "27", "28", "29",
               "30", "31", "32", "33", "34", 
               "35", "36", "37", "38", "39",
               "40", "41", "42", "43", "44",
               "45", "46", "47", "48", "49",
               "50", "51", "52", "53", "54",
               "55", "56")

valuesS1 <- data.frame(sppnamesS1, alphaS1.val)
colnames(valuesS1) <- c("species", "lower.alpha", "l25.alpha", "mean.alpha", "u75.alpha", "upper.alpha")

valuesS1$species <- factor(valuesS1$species, levels = valuesS1$species)

valuesS1$nodiff <- valuesS1$l25.alpha < 0 & valuesS1$u75.alpha > 0
valuesS1$H95 <- valuesS1$lower.alpha > 0
valuesS1$H50 <- valuesS1$lower.alpha < 0 & valuesS1$l25.alpha > 0
valuesS1$L95 <- valuesS1$upper.alpha < 0
valuesS1$L50 <- valuesS1$upper.alpha > 0 & valuesS1$u75.alpha < 0

# Black and white
#Figure
FigureA.1_before_BW <- ggplot() +
  geom_hline(yintercept = 0.5, alpha = 0.75, linetype = "dotdash", color = "grey") +
  geom_vline(xintercept = 15.5, linetype = "dotted", size = 0.6) +
  geom_vline(xintercept = 24.5, linetype = "dotted", size = 0.6) +
  geom_vline(xintercept = 25.5, linetype = "dotted", size = 0.6) +
  geom_vline(xintercept = 28.5, linetype = "dotted", size = 0.6) +
  geom_vline(xintercept = 30.5, linetype = "dotted", size = 0.6) +
  geom_vline(xintercept = 32.5, linetype = "dotted", size = 0.6) +
  geom_errorbar(data = subset(valuesS1, H95 == TRUE), aes(x = species, ymin = lower.alpha, ymax = upper.alpha, color = "gray55"),
                size = 0.75, width = 0) +
  geom_errorbar(data = subset(valuesS1, H95 == TRUE), aes(x = species, ymin = l25.alpha, ymax = u75.alpha, color = "gray55"),
                size = 2, width = 0) +
  geom_errorbar(data = subset(valuesS1, H95 == TRUE), aes(x = species, ymin = mean.alpha, ymax = mean.alpha, color = "black"),
                width = 0.5) +
  coord_cartesian(ylim = c(-0.03, 1.03)) +
  annotate("text", y = 1, x = 8, label = "(Farmland)", size=3, color="black", fontface = "bold") +
  annotate("text", y = 1, x = 20, label = "(Shrubland)", size=3, color="black", fontface = "bold") +
  annotate("text", y = 1, x = 25, label = "(R)", size=3, color="black", fontface = "bold") +
  annotate("text", y = 1, x = 27, label = "(Fo)", size=3, color="black", fontface = "bold") +
  annotate("text", y = 1, x = 29.5, label = "(W)", size=3, color="black", fontface = "bold") +
  annotate("text", y = 1, x = 31.5, label = "(U)", size=3, color="black", fontface = "bold") +
  #annotate("text", y = 1.7, x = 29, label = "(Other habitats)", size=3, color="black", fontface = "bold") +
  annotate("text", y = 1, x = 40.5, label = "(Non-specialist)", size=3, color="black", fontface = "bold") +
  scale_color_manual(name = "", values = c("gray55" = "gray55", "black" = "black")) +
  scale_x_discrete(labels = c("Little bustard","Red-legged partridge","Common quail","Greater short-toed lark",
                              "Crested lark","Calandra lark","Corn bunting","Black-eared wheatear","Zitting cisticola",
                              "European goldfinch","Eurasian hoopoe","European bee-eater","European turtle-dove","Little owl",
                              "Common kestrel","Thekla's lark","Tawny pipit","Woodchat shrike","Iberian grey shrike",
                              "Dartford warbler","Subalpine warbler","Sardinian warbler","European stonechat","Melodious warbler",
                              "Red-billed chough","Common wood pigeon","Common chaffinch","Great tit","Mallard",
                              "White wagtail","Common house martin","Common swift","Common linnet","European serin",
                              "European greenfinch","Cirl bunting","House sparrow","Barn swallow","Common nightingale",
                              "Common blackbird","Spotless starling","Common cuckoo","Magpie","Carrion crow",
                              "Western marsh harrier","Black kite","Common buzzard")) +
  theme_few() +
  ylab("Occurrence probability") +
  xlab("") +
  theme(plot.margin = unit(c(0.5, 0.5, -0.25, 0.4), "cm"),
        text = element_text(size = 11),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, color = "black"),
        axis.title.y = element_text(size=11, vjust = 2, face="bold"),
        legend.position = "none")

FigureA.1_before_BW # 6.8 x 3.45

########

alphaS1.2.val <- cbind(c(mean(c(mean(Output.model$q2.5$psi[c(2),c(10,11,12),45]),mean(Output.model$q2.5$psi[c(3),c(10,11,12),45]),mean(Output.model$q2.5$psi[c(4),c(10,11,12),45]),
                               mean(Output.model$q2.5$psi[c(6),c(10,11,12),45]),mean(Output.model$q2.5$psi[c(7),c(10,11,12),45]),mean(Output.model$q2.5$psi[c(8),c(10,11,12),45]),
                               mean(Output.model$q2.5$psi[c(9),c(10,11,12),45]),mean(Output.model$q2.5$psi[c(10),c(10,11,12),45]),mean(Output.model$q2.5$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),45]),
                               mean(Output.model$q2.5$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),45]),mean(Output.model$q2.5$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),45]),mean(Output.model$q2.5$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),45]),
                               mean(Output.model$q2.5$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),45]),mean(Output.model$q2.5$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),45]),mean(Output.model$q2.5$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),45]),
                               mean(Output.model$q2.5$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),45]),mean(Output.model$q2.5$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),45]))),
                        mean(c(mean(Output.model$q2.5$psi[c(2),c(10,11,12),1]),mean(Output.model$q2.5$psi[c(3),c(10,11,12),1]),mean(Output.model$q2.5$psi[c(4),c(10,11,12),1]),
                               mean(Output.model$q2.5$psi[c(6),c(10,11,12),1]),mean(Output.model$q2.5$psi[c(7),c(10,11,12),1]),mean(Output.model$q2.5$psi[c(8),c(10,11,12),1]),
                               mean(Output.model$q2.5$psi[c(9),c(10,11,12),1]),mean(Output.model$q2.5$psi[c(10),c(10,11,12),1]),mean(Output.model$q2.5$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),1]),
                               mean(Output.model$q2.5$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),1]),mean(Output.model$q2.5$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),1]),mean(Output.model$q2.5$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),1]),
                               mean(Output.model$q2.5$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),1]),mean(Output.model$q2.5$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),1]),mean(Output.model$q2.5$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),1]),
                               mean(Output.model$q2.5$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),1]),mean(Output.model$q2.5$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),1]))),
                        mean(c(mean(Output.model$q2.5$psi[c(2),c(10,11,12),15]),mean(Output.model$q2.5$psi[c(3),c(10,11,12),15]),mean(Output.model$q2.5$psi[c(4),c(10,11,12),15]),
                               mean(Output.model$q2.5$psi[c(6),c(10,11,12),15]),mean(Output.model$q2.5$psi[c(7),c(10,11,12),15]),mean(Output.model$q2.5$psi[c(8),c(10,11,12),15]),
                               mean(Output.model$q2.5$psi[c(9),c(10,11,12),15]),mean(Output.model$q2.5$psi[c(10),c(10,11,12),15]),mean(Output.model$q2.5$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),15]),
                               mean(Output.model$q2.5$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),15]),mean(Output.model$q2.5$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),15]),mean(Output.model$q2.5$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),15]),
                               mean(Output.model$q2.5$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),15]),mean(Output.model$q2.5$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),15]),mean(Output.model$q2.5$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),15]),
                               mean(Output.model$q2.5$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),15]),mean(Output.model$q2.5$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),15]))),
                        mean(c(mean(Output.model$q2.5$psi[c(2),c(10,11,12),7]),mean(Output.model$q2.5$psi[c(3),c(10,11,12),7]),mean(Output.model$q2.5$psi[c(4),c(10,11,12),7]),
                               mean(Output.model$q2.5$psi[c(6),c(10,11,12),7]),mean(Output.model$q2.5$psi[c(7),c(10,11,12),7]),mean(Output.model$q2.5$psi[c(8),c(10,11,12),7]),
                               mean(Output.model$q2.5$psi[c(9),c(10,11,12),7]),mean(Output.model$q2.5$psi[c(10),c(10,11,12),7]),mean(Output.model$q2.5$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),7]),
                               mean(Output.model$q2.5$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),7]),mean(Output.model$q2.5$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),7]),mean(Output.model$q2.5$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),7]),
                               mean(Output.model$q2.5$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),7]),mean(Output.model$q2.5$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),7]),mean(Output.model$q2.5$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),7]),
                               mean(Output.model$q2.5$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),7]),mean(Output.model$q2.5$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),7]))),
                        mean(c(mean(Output.model$q2.5$psi[c(2),c(10,11,12),22]),mean(Output.model$q2.5$psi[c(3),c(10,11,12),22]),mean(Output.model$q2.5$psi[c(4),c(10,11,12),22]),
                               mean(Output.model$q2.5$psi[c(6),c(10,11,12),22]),mean(Output.model$q2.5$psi[c(7),c(10,11,12),22]),mean(Output.model$q2.5$psi[c(8),c(10,11,12),22]),
                               mean(Output.model$q2.5$psi[c(9),c(10,11,12),22]),mean(Output.model$q2.5$psi[c(10),c(10,11,12),22]),mean(Output.model$q2.5$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),22]),
                               mean(Output.model$q2.5$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),22]),mean(Output.model$q2.5$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),22]),mean(Output.model$q2.5$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),22]),
                               mean(Output.model$q2.5$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),22]),mean(Output.model$q2.5$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),22]),mean(Output.model$q2.5$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),22]),
                               mean(Output.model$q2.5$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),22]),mean(Output.model$q2.5$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),22]))),
                        mean(c(mean(Output.model$q2.5$psi[c(2),c(10,11,12),29]),mean(Output.model$q2.5$psi[c(3),c(10,11,12),29]),mean(Output.model$q2.5$psi[c(4),c(10,11,12),29]),
                               mean(Output.model$q2.5$psi[c(6),c(10,11,12),29]),mean(Output.model$q2.5$psi[c(7),c(10,11,12),29]),mean(Output.model$q2.5$psi[c(8),c(10,11,12),29]),
                               mean(Output.model$q2.5$psi[c(9),c(10,11,12),29]),mean(Output.model$q2.5$psi[c(10),c(10,11,12),29]),mean(Output.model$q2.5$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),29]),
                               mean(Output.model$q2.5$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),29]),mean(Output.model$q2.5$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),29]),mean(Output.model$q2.5$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),29]),
                               mean(Output.model$q2.5$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),29]),mean(Output.model$q2.5$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),29]),mean(Output.model$q2.5$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),29]),
                               mean(Output.model$q2.5$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),29]),mean(Output.model$q2.5$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),29]))),
                        mean(c(mean(Output.model$q2.5$psi[c(2),c(10,11,12),18]),mean(Output.model$q2.5$psi[c(3),c(10,11,12),18]),mean(Output.model$q2.5$psi[c(4),c(10,11,12),18]),
                               mean(Output.model$q2.5$psi[c(6),c(10,11,12),18]),mean(Output.model$q2.5$psi[c(7),c(10,11,12),18]),mean(Output.model$q2.5$psi[c(8),c(10,11,12),18]),
                               mean(Output.model$q2.5$psi[c(9),c(10,11,12),18]),mean(Output.model$q2.5$psi[c(10),c(10,11,12),18]),mean(Output.model$q2.5$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),18]),
                               mean(Output.model$q2.5$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),18]),mean(Output.model$q2.5$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),18]),mean(Output.model$q2.5$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),18]),
                               mean(Output.model$q2.5$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),18]),mean(Output.model$q2.5$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),18]),mean(Output.model$q2.5$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),18]),
                               mean(Output.model$q2.5$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),18]),mean(Output.model$q2.5$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),18]))),
                        mean(c(mean(Output.model$q2.5$psi[c(2),c(10,11,12),33]),mean(Output.model$q2.5$psi[c(3),c(10,11,12),33]),mean(Output.model$q2.5$psi[c(4),c(10,11,12),33]),
                               mean(Output.model$q2.5$psi[c(6),c(10,11,12),33]),mean(Output.model$q2.5$psi[c(7),c(10,11,12),33]),mean(Output.model$q2.5$psi[c(8),c(10,11,12),33]),
                               mean(Output.model$q2.5$psi[c(9),c(10,11,12),33]),mean(Output.model$q2.5$psi[c(10),c(10,11,12),33]),mean(Output.model$q2.5$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),33]),
                               mean(Output.model$q2.5$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),33]),mean(Output.model$q2.5$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),33]),mean(Output.model$q2.5$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),33]),
                               mean(Output.model$q2.5$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),33]),mean(Output.model$q2.5$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),33]),mean(Output.model$q2.5$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),33]),
                               mean(Output.model$q2.5$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),33]),mean(Output.model$q2.5$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),33]))),
                        mean(c(mean(Output.model$q2.5$psi[c(2),c(10,11,12),12]),mean(Output.model$q2.5$psi[c(3),c(10,11,12),12]),mean(Output.model$q2.5$psi[c(4),c(10,11,12),12]),
                               mean(Output.model$q2.5$psi[c(6),c(10,11,12),12]),mean(Output.model$q2.5$psi[c(7),c(10,11,12),12]),mean(Output.model$q2.5$psi[c(8),c(10,11,12),12]),
                               mean(Output.model$q2.5$psi[c(9),c(10,11,12),12]),mean(Output.model$q2.5$psi[c(10),c(10,11,12),12]),mean(Output.model$q2.5$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),12]),
                               mean(Output.model$q2.5$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),12]),mean(Output.model$q2.5$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),12]),mean(Output.model$q2.5$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),12]),
                               mean(Output.model$q2.5$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),12]),mean(Output.model$q2.5$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),12]),mean(Output.model$q2.5$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),12]),
                               mean(Output.model$q2.5$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),12]),mean(Output.model$q2.5$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),12]))),
                        mean(c(mean(Output.model$q2.5$psi[c(2),c(10,11,12),9]),mean(Output.model$q2.5$psi[c(3),c(10,11,12),9]),mean(Output.model$q2.5$psi[c(4),c(10,11,12),9]),
                               mean(Output.model$q2.5$psi[c(6),c(10,11,12),9]),mean(Output.model$q2.5$psi[c(7),c(10,11,12),9]),mean(Output.model$q2.5$psi[c(8),c(10,11,12),9]),
                               mean(Output.model$q2.5$psi[c(9),c(10,11,12),9]),mean(Output.model$q2.5$psi[c(10),c(10,11,12),9]),mean(Output.model$q2.5$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),9]),
                               mean(Output.model$q2.5$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),9]),mean(Output.model$q2.5$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),9]),mean(Output.model$q2.5$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),9]),
                               mean(Output.model$q2.5$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),9]),mean(Output.model$q2.5$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),9]),mean(Output.model$q2.5$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),9]),
                               mean(Output.model$q2.5$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),9]),mean(Output.model$q2.5$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),9]))),
                        mean(c(mean(Output.model$q2.5$psi[c(2),c(10,11,12),47]),mean(Output.model$q2.5$psi[c(3),c(10,11,12),47]),mean(Output.model$q2.5$psi[c(4),c(10,11,12),47]),
                               mean(Output.model$q2.5$psi[c(6),c(10,11,12),47]),mean(Output.model$q2.5$psi[c(7),c(10,11,12),47]),mean(Output.model$q2.5$psi[c(8),c(10,11,12),47]),
                               mean(Output.model$q2.5$psi[c(9),c(10,11,12),47]),mean(Output.model$q2.5$psi[c(10),c(10,11,12),47]),mean(Output.model$q2.5$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),47]),
                               mean(Output.model$q2.5$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),47]),mean(Output.model$q2.5$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),47]),mean(Output.model$q2.5$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),47]),
                               mean(Output.model$q2.5$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),47]),mean(Output.model$q2.5$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),47]),mean(Output.model$q2.5$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),47]),
                               mean(Output.model$q2.5$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),47]),mean(Output.model$q2.5$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),47]))),
                        mean(c(mean(Output.model$q2.5$psi[c(2),c(10,11,12),30]),mean(Output.model$q2.5$psi[c(3),c(10,11,12),30]),mean(Output.model$q2.5$psi[c(4),c(10,11,12),30]),
                               mean(Output.model$q2.5$psi[c(6),c(10,11,12),30]),mean(Output.model$q2.5$psi[c(7),c(10,11,12),30]),mean(Output.model$q2.5$psi[c(8),c(10,11,12),30]),
                               mean(Output.model$q2.5$psi[c(9),c(10,11,12),30]),mean(Output.model$q2.5$psi[c(10),c(10,11,12),30]),mean(Output.model$q2.5$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),30]),
                               mean(Output.model$q2.5$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),30]),mean(Output.model$q2.5$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),30]),mean(Output.model$q2.5$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),30]),
                               mean(Output.model$q2.5$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),30]),mean(Output.model$q2.5$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),30]),mean(Output.model$q2.5$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),30]),
                               mean(Output.model$q2.5$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),30]),mean(Output.model$q2.5$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),30]))),
                        mean(c(mean(Output.model$q2.5$psi[c(2),c(10,11,12),40]),mean(Output.model$q2.5$psi[c(3),c(10,11,12),40]),mean(Output.model$q2.5$psi[c(4),c(10,11,12),40]),
                               mean(Output.model$q2.5$psi[c(6),c(10,11,12),40]),mean(Output.model$q2.5$psi[c(7),c(10,11,12),40]),mean(Output.model$q2.5$psi[c(8),c(10,11,12),40]),
                               mean(Output.model$q2.5$psi[c(9),c(10,11,12),40]),mean(Output.model$q2.5$psi[c(10),c(10,11,12),40]),mean(Output.model$q2.5$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),40]),
                               mean(Output.model$q2.5$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),40]),mean(Output.model$q2.5$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),40]),mean(Output.model$q2.5$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),40]),
                               mean(Output.model$q2.5$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),40]),mean(Output.model$q2.5$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),40]),mean(Output.model$q2.5$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),40]),
                               mean(Output.model$q2.5$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),40]),mean(Output.model$q2.5$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),40]))),
                        mean(c(mean(Output.model$q2.5$psi[c(2),c(10,11,12),5]),mean(Output.model$q2.5$psi[c(3),c(10,11,12),5]),mean(Output.model$q2.5$psi[c(4),c(10,11,12),5]),
                               mean(Output.model$q2.5$psi[c(6),c(10,11,12),5]),mean(Output.model$q2.5$psi[c(7),c(10,11,12),5]),mean(Output.model$q2.5$psi[c(8),c(10,11,12),5]),
                               mean(Output.model$q2.5$psi[c(9),c(10,11,12),5]),mean(Output.model$q2.5$psi[c(10),c(10,11,12),5]),mean(Output.model$q2.5$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),5]),
                               mean(Output.model$q2.5$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),5]),mean(Output.model$q2.5$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),5]),mean(Output.model$q2.5$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),5]),
                               mean(Output.model$q2.5$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),5]),mean(Output.model$q2.5$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),5]),mean(Output.model$q2.5$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),5]),
                               mean(Output.model$q2.5$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),5]),mean(Output.model$q2.5$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),5]))),
                        mean(c(mean(Output.model$q2.5$psi[c(2),c(10,11,12),20]),mean(Output.model$q2.5$psi[c(3),c(10,11,12),20]),mean(Output.model$q2.5$psi[c(4),c(10,11,12),20]),
                               mean(Output.model$q2.5$psi[c(6),c(10,11,12),20]),mean(Output.model$q2.5$psi[c(7),c(10,11,12),20]),mean(Output.model$q2.5$psi[c(8),c(10,11,12),20]),
                               mean(Output.model$q2.5$psi[c(9),c(10,11,12),20]),mean(Output.model$q2.5$psi[c(10),c(10,11,12),20]),mean(Output.model$q2.5$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),20]),
                               mean(Output.model$q2.5$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),20]),mean(Output.model$q2.5$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),20]),mean(Output.model$q2.5$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),20]),
                               mean(Output.model$q2.5$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),20]),mean(Output.model$q2.5$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),20]),mean(Output.model$q2.5$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),20]),
                               mean(Output.model$q2.5$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),20]),mean(Output.model$q2.5$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),20]))),
                        mean(c(mean(Output.model$q2.5$psi[c(2),c(10,11,12),23]),mean(Output.model$q2.5$psi[c(3),c(10,11,12),23]),mean(Output.model$q2.5$psi[c(4),c(10,11,12),23]),
                               mean(Output.model$q2.5$psi[c(6),c(10,11,12),23]),mean(Output.model$q2.5$psi[c(7),c(10,11,12),23]),mean(Output.model$q2.5$psi[c(8),c(10,11,12),23]),
                               mean(Output.model$q2.5$psi[c(9),c(10,11,12),23]),mean(Output.model$q2.5$psi[c(10),c(10,11,12),23]),mean(Output.model$q2.5$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),23]),
                               mean(Output.model$q2.5$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),23]),mean(Output.model$q2.5$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),23]),mean(Output.model$q2.5$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),23]),
                               mean(Output.model$q2.5$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),23]),mean(Output.model$q2.5$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),23]),mean(Output.model$q2.5$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),23]),
                               mean(Output.model$q2.5$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),23]),mean(Output.model$q2.5$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),23]))),
                        mean(c(mean(Output.model$q2.5$psi[c(2),c(10,11,12),3]),mean(Output.model$q2.5$psi[c(3),c(10,11,12),3]),mean(Output.model$q2.5$psi[c(4),c(10,11,12),3]),
                               mean(Output.model$q2.5$psi[c(6),c(10,11,12),3]),mean(Output.model$q2.5$psi[c(7),c(10,11,12),3]),mean(Output.model$q2.5$psi[c(8),c(10,11,12),3]),
                               mean(Output.model$q2.5$psi[c(9),c(10,11,12),3]),mean(Output.model$q2.5$psi[c(10),c(10,11,12),3]),mean(Output.model$q2.5$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),3]),
                               mean(Output.model$q2.5$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),3]),mean(Output.model$q2.5$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),3]),mean(Output.model$q2.5$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),3]),
                               mean(Output.model$q2.5$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),3]),mean(Output.model$q2.5$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),3]),mean(Output.model$q2.5$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),3]),
                               mean(Output.model$q2.5$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),3]),mean(Output.model$q2.5$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),3]))),
                        mean(c(mean(Output.model$q2.5$psi[c(2),c(10,11,12),27]),mean(Output.model$q2.5$psi[c(3),c(10,11,12),27]),mean(Output.model$q2.5$psi[c(4),c(10,11,12),27]),
                               mean(Output.model$q2.5$psi[c(6),c(10,11,12),27]),mean(Output.model$q2.5$psi[c(7),c(10,11,12),27]),mean(Output.model$q2.5$psi[c(8),c(10,11,12),27]),
                               mean(Output.model$q2.5$psi[c(9),c(10,11,12),27]),mean(Output.model$q2.5$psi[c(10),c(10,11,12),27]),mean(Output.model$q2.5$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),27]),
                               mean(Output.model$q2.5$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),27]),mean(Output.model$q2.5$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),27]),mean(Output.model$q2.5$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),27]),
                               mean(Output.model$q2.5$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),27]),mean(Output.model$q2.5$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),27]),mean(Output.model$q2.5$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),27]),
                               mean(Output.model$q2.5$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),27]),mean(Output.model$q2.5$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),27]))),
                        mean(c(mean(Output.model$q2.5$psi[c(2),c(10,11,12),26]),mean(Output.model$q2.5$psi[c(3),c(10,11,12),26]),mean(Output.model$q2.5$psi[c(4),c(10,11,12),26]),
                               mean(Output.model$q2.5$psi[c(6),c(10,11,12),26]),mean(Output.model$q2.5$psi[c(7),c(10,11,12),26]),mean(Output.model$q2.5$psi[c(8),c(10,11,12),26]),
                               mean(Output.model$q2.5$psi[c(9),c(10,11,12),26]),mean(Output.model$q2.5$psi[c(10),c(10,11,12),26]),mean(Output.model$q2.5$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),26]),
                               mean(Output.model$q2.5$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),26]),mean(Output.model$q2.5$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),26]),mean(Output.model$q2.5$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),26]),
                               mean(Output.model$q2.5$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),26]),mean(Output.model$q2.5$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),26]),mean(Output.model$q2.5$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),26]),
                               mean(Output.model$q2.5$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),26]),mean(Output.model$q2.5$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),26]))),
                        mean(c(mean(Output.model$q2.5$psi[c(2),c(10,11,12),44]),mean(Output.model$q2.5$psi[c(3),c(10,11,12),44]),mean(Output.model$q2.5$psi[c(4),c(10,11,12),44]),
                               mean(Output.model$q2.5$psi[c(6),c(10,11,12),44]),mean(Output.model$q2.5$psi[c(7),c(10,11,12),44]),mean(Output.model$q2.5$psi[c(8),c(10,11,12),44]),
                               mean(Output.model$q2.5$psi[c(9),c(10,11,12),44]),mean(Output.model$q2.5$psi[c(10),c(10,11,12),44]),mean(Output.model$q2.5$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),44]),
                               mean(Output.model$q2.5$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),44]),mean(Output.model$q2.5$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),44]),mean(Output.model$q2.5$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),44]),
                               mean(Output.model$q2.5$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),44]),mean(Output.model$q2.5$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),44]),mean(Output.model$q2.5$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),44]),
                               mean(Output.model$q2.5$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),44]),mean(Output.model$q2.5$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),44]))),
                        mean(c(mean(Output.model$q2.5$psi[c(2),c(10,11,12),42]),mean(Output.model$q2.5$psi[c(3),c(10,11,12),42]),mean(Output.model$q2.5$psi[c(4),c(10,11,12),42]),
                               mean(Output.model$q2.5$psi[c(6),c(10,11,12),42]),mean(Output.model$q2.5$psi[c(7),c(10,11,12),42]),mean(Output.model$q2.5$psi[c(8),c(10,11,12),42]),
                               mean(Output.model$q2.5$psi[c(9),c(10,11,12),42]),mean(Output.model$q2.5$psi[c(10),c(10,11,12),42]),mean(Output.model$q2.5$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),42]),
                               mean(Output.model$q2.5$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),42]),mean(Output.model$q2.5$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),42]),mean(Output.model$q2.5$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),42]),
                               mean(Output.model$q2.5$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),42]),mean(Output.model$q2.5$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),42]),mean(Output.model$q2.5$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),42]),
                               mean(Output.model$q2.5$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),42]),mean(Output.model$q2.5$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),42]))),
                        mean(c(mean(Output.model$q2.5$psi[c(2),c(10,11,12),43]),mean(Output.model$q2.5$psi[c(3),c(10,11,12),43]),mean(Output.model$q2.5$psi[c(4),c(10,11,12),43]),
                               mean(Output.model$q2.5$psi[c(6),c(10,11,12),43]),mean(Output.model$q2.5$psi[c(7),c(10,11,12),43]),mean(Output.model$q2.5$psi[c(8),c(10,11,12),43]),
                               mean(Output.model$q2.5$psi[c(9),c(10,11,12),43]),mean(Output.model$q2.5$psi[c(10),c(10,11,12),43]),mean(Output.model$q2.5$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),43]),
                               mean(Output.model$q2.5$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),43]),mean(Output.model$q2.5$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),43]),mean(Output.model$q2.5$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),43]),
                               mean(Output.model$q2.5$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),43]),mean(Output.model$q2.5$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),43]),mean(Output.model$q2.5$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),43]),
                               mean(Output.model$q2.5$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),43]),mean(Output.model$q2.5$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),43]))),
                        mean(c(mean(Output.model$q2.5$psi[c(2),c(10,11,12),38]),mean(Output.model$q2.5$psi[c(3),c(10,11,12),38]),mean(Output.model$q2.5$psi[c(4),c(10,11,12),38]),
                               mean(Output.model$q2.5$psi[c(6),c(10,11,12),38]),mean(Output.model$q2.5$psi[c(7),c(10,11,12),38]),mean(Output.model$q2.5$psi[c(8),c(10,11,12),38]),
                               mean(Output.model$q2.5$psi[c(9),c(10,11,12),38]),mean(Output.model$q2.5$psi[c(10),c(10,11,12),38]),mean(Output.model$q2.5$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),38]),
                               mean(Output.model$q2.5$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),38]),mean(Output.model$q2.5$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),38]),mean(Output.model$q2.5$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),38]),
                               mean(Output.model$q2.5$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),38]),mean(Output.model$q2.5$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),38]),mean(Output.model$q2.5$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),38]),
                               mean(Output.model$q2.5$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),38]),mean(Output.model$q2.5$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),38]))),
                        mean(c(mean(Output.model$q2.5$psi[c(2),c(10,11,12),24]),mean(Output.model$q2.5$psi[c(3),c(10,11,12),24]),mean(Output.model$q2.5$psi[c(4),c(10,11,12),24]),
                               mean(Output.model$q2.5$psi[c(6),c(10,11,12),24]),mean(Output.model$q2.5$psi[c(7),c(10,11,12),24]),mean(Output.model$q2.5$psi[c(8),c(10,11,12),24]),
                               mean(Output.model$q2.5$psi[c(9),c(10,11,12),24]),mean(Output.model$q2.5$psi[c(10),c(10,11,12),24]),mean(Output.model$q2.5$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),24]),
                               mean(Output.model$q2.5$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),24]),mean(Output.model$q2.5$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),24]),mean(Output.model$q2.5$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),24]),
                               mean(Output.model$q2.5$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),24]),mean(Output.model$q2.5$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),24]),mean(Output.model$q2.5$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),24]),
                               mean(Output.model$q2.5$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),24]),mean(Output.model$q2.5$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),24]))),
                        mean(c(mean(Output.model$q2.5$psi[c(2),c(10,11,12),37]),mean(Output.model$q2.5$psi[c(3),c(10,11,12),37]),mean(Output.model$q2.5$psi[c(4),c(10,11,12),37]),
                               mean(Output.model$q2.5$psi[c(6),c(10,11,12),37]),mean(Output.model$q2.5$psi[c(7),c(10,11,12),37]),mean(Output.model$q2.5$psi[c(8),c(10,11,12),37]),
                               mean(Output.model$q2.5$psi[c(9),c(10,11,12),37]),mean(Output.model$q2.5$psi[c(10),c(10,11,12),37]),mean(Output.model$q2.5$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),37]),
                               mean(Output.model$q2.5$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),37]),mean(Output.model$q2.5$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),37]),mean(Output.model$q2.5$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),37]),
                               mean(Output.model$q2.5$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),37]),mean(Output.model$q2.5$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),37]),mean(Output.model$q2.5$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),37]),
                               mean(Output.model$q2.5$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),37]),mean(Output.model$q2.5$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),37]))),
                        mean(c(mean(Output.model$q2.5$psi[c(2),c(10,11,12),13]),mean(Output.model$q2.5$psi[c(3),c(10,11,12),13]),mean(Output.model$q2.5$psi[c(4),c(10,11,12),13]),
                               mean(Output.model$q2.5$psi[c(6),c(10,11,12),13]),mean(Output.model$q2.5$psi[c(7),c(10,11,12),13]),mean(Output.model$q2.5$psi[c(8),c(10,11,12),13]),
                               mean(Output.model$q2.5$psi[c(9),c(10,11,12),13]),mean(Output.model$q2.5$psi[c(10),c(10,11,12),13]),mean(Output.model$q2.5$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),13]),
                               mean(Output.model$q2.5$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),13]),mean(Output.model$q2.5$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),13]),mean(Output.model$q2.5$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),13]),
                               mean(Output.model$q2.5$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),13]),mean(Output.model$q2.5$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),13]),mean(Output.model$q2.5$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),13]),
                               mean(Output.model$q2.5$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),13]),mean(Output.model$q2.5$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),13]))),
                        mean(c(mean(Output.model$q2.5$psi[c(2),c(10,11,12),21]),mean(Output.model$q2.5$psi[c(3),c(10,11,12),21]),mean(Output.model$q2.5$psi[c(4),c(10,11,12),21]),
                               mean(Output.model$q2.5$psi[c(6),c(10,11,12),21]),mean(Output.model$q2.5$psi[c(7),c(10,11,12),21]),mean(Output.model$q2.5$psi[c(8),c(10,11,12),21]),
                               mean(Output.model$q2.5$psi[c(9),c(10,11,12),21]),mean(Output.model$q2.5$psi[c(10),c(10,11,12),21]),mean(Output.model$q2.5$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),21]),
                               mean(Output.model$q2.5$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),21]),mean(Output.model$q2.5$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),21]),mean(Output.model$q2.5$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),21]),
                               mean(Output.model$q2.5$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),21]),mean(Output.model$q2.5$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),21]),mean(Output.model$q2.5$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),21]),
                               mean(Output.model$q2.5$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),21]),mean(Output.model$q2.5$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),21]))),
                        mean(c(mean(Output.model$q2.5$psi[c(2),c(10,11,12),34]),mean(Output.model$q2.5$psi[c(3),c(10,11,12),34]),mean(Output.model$q2.5$psi[c(4),c(10,11,12),34]),
                               mean(Output.model$q2.5$psi[c(6),c(10,11,12),34]),mean(Output.model$q2.5$psi[c(7),c(10,11,12),34]),mean(Output.model$q2.5$psi[c(8),c(10,11,12),34]),
                               mean(Output.model$q2.5$psi[c(9),c(10,11,12),34]),mean(Output.model$q2.5$psi[c(10),c(10,11,12),34]),mean(Output.model$q2.5$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),34]),
                               mean(Output.model$q2.5$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),34]),mean(Output.model$q2.5$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),34]),mean(Output.model$q2.5$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),34]),
                               mean(Output.model$q2.5$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),34]),mean(Output.model$q2.5$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),34]),mean(Output.model$q2.5$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),34]),
                               mean(Output.model$q2.5$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),34]),mean(Output.model$q2.5$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),34]))),
                        mean(c(mean(Output.model$q2.5$psi[c(2),c(10,11,12),2]),mean(Output.model$q2.5$psi[c(3),c(10,11,12),2]),mean(Output.model$q2.5$psi[c(4),c(10,11,12),2]),
                               mean(Output.model$q2.5$psi[c(6),c(10,11,12),2]),mean(Output.model$q2.5$psi[c(7),c(10,11,12),2]),mean(Output.model$q2.5$psi[c(8),c(10,11,12),2]),
                               mean(Output.model$q2.5$psi[c(9),c(10,11,12),2]),mean(Output.model$q2.5$psi[c(10),c(10,11,12),2]),mean(Output.model$q2.5$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),2]),
                               mean(Output.model$q2.5$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),2]),mean(Output.model$q2.5$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),2]),mean(Output.model$q2.5$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),2]),
                               mean(Output.model$q2.5$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),2]),mean(Output.model$q2.5$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),2]),mean(Output.model$q2.5$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),2]),
                               mean(Output.model$q2.5$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),2]),mean(Output.model$q2.5$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),2]))),
                        mean(c(mean(Output.model$q2.5$psi[c(2),c(10,11,12),32]),mean(Output.model$q2.5$psi[c(3),c(10,11,12),32]),mean(Output.model$q2.5$psi[c(4),c(10,11,12),32]),
                               mean(Output.model$q2.5$psi[c(6),c(10,11,12),32]),mean(Output.model$q2.5$psi[c(7),c(10,11,12),32]),mean(Output.model$q2.5$psi[c(8),c(10,11,12),32]),
                               mean(Output.model$q2.5$psi[c(9),c(10,11,12),32]),mean(Output.model$q2.5$psi[c(10),c(10,11,12),32]),mean(Output.model$q2.5$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),32]),
                               mean(Output.model$q2.5$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),32]),mean(Output.model$q2.5$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),32]),mean(Output.model$q2.5$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),32]),
                               mean(Output.model$q2.5$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),32]),mean(Output.model$q2.5$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),32]),mean(Output.model$q2.5$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),32]),
                               mean(Output.model$q2.5$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),32]),mean(Output.model$q2.5$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),32]))),
                        mean(c(mean(Output.model$q2.5$psi[c(2),c(10,11,12),17]),mean(Output.model$q2.5$psi[c(3),c(10,11,12),17]),mean(Output.model$q2.5$psi[c(4),c(10,11,12),17]),
                               mean(Output.model$q2.5$psi[c(6),c(10,11,12),17]),mean(Output.model$q2.5$psi[c(7),c(10,11,12),17]),mean(Output.model$q2.5$psi[c(8),c(10,11,12),17]),
                               mean(Output.model$q2.5$psi[c(9),c(10,11,12),17]),mean(Output.model$q2.5$psi[c(10),c(10,11,12),17]),mean(Output.model$q2.5$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),17]),
                               mean(Output.model$q2.5$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),17]),mean(Output.model$q2.5$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),17]),mean(Output.model$q2.5$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),17]),
                               mean(Output.model$q2.5$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),17]),mean(Output.model$q2.5$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),17]),mean(Output.model$q2.5$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),17]),
                               mean(Output.model$q2.5$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),17]),mean(Output.model$q2.5$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),17]))),
                        mean(c(mean(Output.model$q2.5$psi[c(2),c(10,11,12),4]),mean(Output.model$q2.5$psi[c(3),c(10,11,12),4]),mean(Output.model$q2.5$psi[c(4),c(10,11,12),4]),
                               mean(Output.model$q2.5$psi[c(6),c(10,11,12),4]),mean(Output.model$q2.5$psi[c(7),c(10,11,12),4]),mean(Output.model$q2.5$psi[c(8),c(10,11,12),4]),
                               mean(Output.model$q2.5$psi[c(9),c(10,11,12),4]),mean(Output.model$q2.5$psi[c(10),c(10,11,12),4]),mean(Output.model$q2.5$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),4]),
                               mean(Output.model$q2.5$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),4]),mean(Output.model$q2.5$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),4]),mean(Output.model$q2.5$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),4]),
                               mean(Output.model$q2.5$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),4]),mean(Output.model$q2.5$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),4]),mean(Output.model$q2.5$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),4]),
                               mean(Output.model$q2.5$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),4]),mean(Output.model$q2.5$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),4]))),
                        mean(c(mean(Output.model$q2.5$psi[c(2),c(10,11,12),8]),mean(Output.model$q2.5$psi[c(3),c(10,11,12),8]),mean(Output.model$q2.5$psi[c(4),c(10,11,12),8]),
                               mean(Output.model$q2.5$psi[c(6),c(10,11,12),8]),mean(Output.model$q2.5$psi[c(7),c(10,11,12),8]),mean(Output.model$q2.5$psi[c(8),c(10,11,12),8]),
                               mean(Output.model$q2.5$psi[c(9),c(10,11,12),8]),mean(Output.model$q2.5$psi[c(10),c(10,11,12),8]),mean(Output.model$q2.5$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),8]),
                               mean(Output.model$q2.5$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),8]),mean(Output.model$q2.5$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),8]),mean(Output.model$q2.5$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),8]),
                               mean(Output.model$q2.5$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),8]),mean(Output.model$q2.5$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),8]),mean(Output.model$q2.5$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),8]),
                               mean(Output.model$q2.5$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),8]),mean(Output.model$q2.5$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),8]))),
                        mean(c(mean(Output.model$q2.5$psi[c(2),c(10,11,12),39]),mean(Output.model$q2.5$psi[c(3),c(10,11,12),39]),mean(Output.model$q2.5$psi[c(4),c(10,11,12),39]),
                               mean(Output.model$q2.5$psi[c(6),c(10,11,12),39]),mean(Output.model$q2.5$psi[c(7),c(10,11,12),39]),mean(Output.model$q2.5$psi[c(8),c(10,11,12),39]),
                               mean(Output.model$q2.5$psi[c(9),c(10,11,12),39]),mean(Output.model$q2.5$psi[c(10),c(10,11,12),39]),mean(Output.model$q2.5$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),39]),
                               mean(Output.model$q2.5$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),39]),mean(Output.model$q2.5$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),39]),mean(Output.model$q2.5$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),39]),
                               mean(Output.model$q2.5$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),39]),mean(Output.model$q2.5$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),39]),mean(Output.model$q2.5$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),39]),
                               mean(Output.model$q2.5$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),39]),mean(Output.model$q2.5$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),39]))),
                        mean(c(mean(Output.model$q2.5$psi[c(2),c(10,11,12),10]),mean(Output.model$q2.5$psi[c(3),c(10,11,12),10]),mean(Output.model$q2.5$psi[c(4),c(10,11,12),10]),
                               mean(Output.model$q2.5$psi[c(6),c(10,11,12),10]),mean(Output.model$q2.5$psi[c(7),c(10,11,12),10]),mean(Output.model$q2.5$psi[c(8),c(10,11,12),10]),
                               mean(Output.model$q2.5$psi[c(9),c(10,11,12),10]),mean(Output.model$q2.5$psi[c(10),c(10,11,12),10]),mean(Output.model$q2.5$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),10]),
                               mean(Output.model$q2.5$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),10]),mean(Output.model$q2.5$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),10]),mean(Output.model$q2.5$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),10]),
                               mean(Output.model$q2.5$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),10]),mean(Output.model$q2.5$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),10]),mean(Output.model$q2.5$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),10]),
                               mean(Output.model$q2.5$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),10]),mean(Output.model$q2.5$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),10]))),
                        mean(c(mean(Output.model$q2.5$psi[c(2),c(10,11,12),19]),mean(Output.model$q2.5$psi[c(3),c(10,11,12),19]),mean(Output.model$q2.5$psi[c(4),c(10,11,12),19]),
                               mean(Output.model$q2.5$psi[c(6),c(10,11,12),19]),mean(Output.model$q2.5$psi[c(7),c(10,11,12),19]),mean(Output.model$q2.5$psi[c(8),c(10,11,12),19]),
                               mean(Output.model$q2.5$psi[c(9),c(10,11,12),19]),mean(Output.model$q2.5$psi[c(10),c(10,11,12),19]),mean(Output.model$q2.5$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),19]),
                               mean(Output.model$q2.5$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),19]),mean(Output.model$q2.5$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),19]),mean(Output.model$q2.5$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),19]),
                               mean(Output.model$q2.5$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),19]),mean(Output.model$q2.5$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),19]),mean(Output.model$q2.5$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),19]),
                               mean(Output.model$q2.5$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),19]),mean(Output.model$q2.5$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),19]))),
                        mean(c(mean(Output.model$q2.5$psi[c(2),c(10,11,12),35]),mean(Output.model$q2.5$psi[c(3),c(10,11,12),35]),mean(Output.model$q2.5$psi[c(4),c(10,11,12),35]),
                               mean(Output.model$q2.5$psi[c(6),c(10,11,12),35]),mean(Output.model$q2.5$psi[c(7),c(10,11,12),35]),mean(Output.model$q2.5$psi[c(8),c(10,11,12),35]),
                               mean(Output.model$q2.5$psi[c(9),c(10,11,12),35]),mean(Output.model$q2.5$psi[c(10),c(10,11,12),35]),mean(Output.model$q2.5$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),35]),
                               mean(Output.model$q2.5$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),35]),mean(Output.model$q2.5$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),35]),mean(Output.model$q2.5$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),35]),
                               mean(Output.model$q2.5$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),35]),mean(Output.model$q2.5$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),35]),mean(Output.model$q2.5$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),35]),
                               mean(Output.model$q2.5$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),35]),mean(Output.model$q2.5$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),35]))),
                        mean(c(mean(Output.model$q2.5$psi[c(2),c(10,11,12),25]),mean(Output.model$q2.5$psi[c(3),c(10,11,12),25]),mean(Output.model$q2.5$psi[c(4),c(10,11,12),25]),
                               mean(Output.model$q2.5$psi[c(6),c(10,11,12),25]),mean(Output.model$q2.5$psi[c(7),c(10,11,12),25]),mean(Output.model$q2.5$psi[c(8),c(10,11,12),25]),
                               mean(Output.model$q2.5$psi[c(9),c(10,11,12),25]),mean(Output.model$q2.5$psi[c(10),c(10,11,12),25]),mean(Output.model$q2.5$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),25]),
                               mean(Output.model$q2.5$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),25]),mean(Output.model$q2.5$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),25]),mean(Output.model$q2.5$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),25]),
                               mean(Output.model$q2.5$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),25]),mean(Output.model$q2.5$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),25]),mean(Output.model$q2.5$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),25]),
                               mean(Output.model$q2.5$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),25]),mean(Output.model$q2.5$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),25]))),
                        mean(c(mean(Output.model$q2.5$psi[c(2),c(10,11,12),28]),mean(Output.model$q2.5$psi[c(3),c(10,11,12),28]),mean(Output.model$q2.5$psi[c(4),c(10,11,12),28]),
                               mean(Output.model$q2.5$psi[c(6),c(10,11,12),28]),mean(Output.model$q2.5$psi[c(7),c(10,11,12),28]),mean(Output.model$q2.5$psi[c(8),c(10,11,12),28]),
                               mean(Output.model$q2.5$psi[c(9),c(10,11,12),28]),mean(Output.model$q2.5$psi[c(10),c(10,11,12),28]),mean(Output.model$q2.5$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),28]),
                               mean(Output.model$q2.5$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),28]),mean(Output.model$q2.5$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),28]),mean(Output.model$q2.5$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),28]),
                               mean(Output.model$q2.5$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),28]),mean(Output.model$q2.5$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),28]),mean(Output.model$q2.5$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),28]),
                               mean(Output.model$q2.5$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),28]),mean(Output.model$q2.5$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),28]))),
                        mean(c(mean(Output.model$q2.5$psi[c(2),c(10,11,12),46]),mean(Output.model$q2.5$psi[c(3),c(10,11,12),46]),mean(Output.model$q2.5$psi[c(4),c(10,11,12),46]),
                               mean(Output.model$q2.5$psi[c(6),c(10,11,12),46]),mean(Output.model$q2.5$psi[c(7),c(10,11,12),46]),mean(Output.model$q2.5$psi[c(8),c(10,11,12),46]),
                               mean(Output.model$q2.5$psi[c(9),c(10,11,12),46]),mean(Output.model$q2.5$psi[c(10),c(10,11,12),46]),mean(Output.model$q2.5$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),46]),
                               mean(Output.model$q2.5$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),46]),mean(Output.model$q2.5$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),46]),mean(Output.model$q2.5$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),46]),
                               mean(Output.model$q2.5$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),46]),mean(Output.model$q2.5$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),46]),mean(Output.model$q2.5$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),46]),
                               mean(Output.model$q2.5$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),46]),mean(Output.model$q2.5$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),46]))),
                        mean(c(mean(Output.model$q2.5$psi[c(2),c(10,11,12),41]),mean(Output.model$q2.5$psi[c(3),c(10,11,12),41]),mean(Output.model$q2.5$psi[c(4),c(10,11,12),41]),
                               mean(Output.model$q2.5$psi[c(6),c(10,11,12),41]),mean(Output.model$q2.5$psi[c(7),c(10,11,12),41]),mean(Output.model$q2.5$psi[c(8),c(10,11,12),41]),
                               mean(Output.model$q2.5$psi[c(9),c(10,11,12),41]),mean(Output.model$q2.5$psi[c(10),c(10,11,12),41]),mean(Output.model$q2.5$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),41]),
                               mean(Output.model$q2.5$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),41]),mean(Output.model$q2.5$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),41]),mean(Output.model$q2.5$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),41]),
                               mean(Output.model$q2.5$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),41]),mean(Output.model$q2.5$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),41]),mean(Output.model$q2.5$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),41]),
                               mean(Output.model$q2.5$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),41]),mean(Output.model$q2.5$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),41]))),
                        mean(c(mean(Output.model$q2.5$psi[c(2),c(10,11,12),16]),mean(Output.model$q2.5$psi[c(3),c(10,11,12),16]),mean(Output.model$q2.5$psi[c(4),c(10,11,12),16]),
                               mean(Output.model$q2.5$psi[c(6),c(10,11,12),16]),mean(Output.model$q2.5$psi[c(7),c(10,11,12),16]),mean(Output.model$q2.5$psi[c(8),c(10,11,12),16]),
                               mean(Output.model$q2.5$psi[c(9),c(10,11,12),16]),mean(Output.model$q2.5$psi[c(10),c(10,11,12),16]),mean(Output.model$q2.5$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),16]),
                               mean(Output.model$q2.5$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),16]),mean(Output.model$q2.5$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),16]),mean(Output.model$q2.5$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),16]),
                               mean(Output.model$q2.5$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),16]),mean(Output.model$q2.5$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),16]),mean(Output.model$q2.5$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),16]),
                               mean(Output.model$q2.5$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),16]),mean(Output.model$q2.5$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),16]))),
                        mean(c(mean(Output.model$q2.5$psi[c(2),c(10,11,12),36]),mean(Output.model$q2.5$psi[c(3),c(10,11,12),36]),mean(Output.model$q2.5$psi[c(4),c(10,11,12),36]),
                               mean(Output.model$q2.5$psi[c(6),c(10,11,12),36]),mean(Output.model$q2.5$psi[c(7),c(10,11,12),36]),mean(Output.model$q2.5$psi[c(8),c(10,11,12),36]),
                               mean(Output.model$q2.5$psi[c(9),c(10,11,12),36]),mean(Output.model$q2.5$psi[c(10),c(10,11,12),36]),mean(Output.model$q2.5$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),36]),
                               mean(Output.model$q2.5$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),36]),mean(Output.model$q2.5$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),36]),mean(Output.model$q2.5$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),36]),
                               mean(Output.model$q2.5$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),36]),mean(Output.model$q2.5$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),36]),mean(Output.model$q2.5$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),36]),
                               mean(Output.model$q2.5$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),36]),mean(Output.model$q2.5$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),36]))),
                        mean(c(mean(Output.model$q2.5$psi[c(2),c(10,11,12),14]),mean(Output.model$q2.5$psi[c(3),c(10,11,12),14]),mean(Output.model$q2.5$psi[c(4),c(10,11,12),14]),
                               mean(Output.model$q2.5$psi[c(6),c(10,11,12),14]),mean(Output.model$q2.5$psi[c(7),c(10,11,12),14]),mean(Output.model$q2.5$psi[c(8),c(10,11,12),14]),
                               mean(Output.model$q2.5$psi[c(9),c(10,11,12),14]),mean(Output.model$q2.5$psi[c(10),c(10,11,12),14]),mean(Output.model$q2.5$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),14]),
                               mean(Output.model$q2.5$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),14]),mean(Output.model$q2.5$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),14]),mean(Output.model$q2.5$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),14]),
                               mean(Output.model$q2.5$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),14]),mean(Output.model$q2.5$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),14]),mean(Output.model$q2.5$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),14]),
                               mean(Output.model$q2.5$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),14]),mean(Output.model$q2.5$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),14]))),
                        mean(c(mean(Output.model$q2.5$psi[c(2),c(10,11,12),11]),mean(Output.model$q2.5$psi[c(3),c(10,11,12),11]),mean(Output.model$q2.5$psi[c(4),c(10,11,12),11]),
                               mean(Output.model$q2.5$psi[c(6),c(10,11,12),11]),mean(Output.model$q2.5$psi[c(7),c(10,11,12),11]),mean(Output.model$q2.5$psi[c(8),c(10,11,12),11]),
                               mean(Output.model$q2.5$psi[c(9),c(10,11,12),11]),mean(Output.model$q2.5$psi[c(10),c(10,11,12),11]),mean(Output.model$q2.5$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),11]),
                               mean(Output.model$q2.5$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),11]),mean(Output.model$q2.5$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),11]),mean(Output.model$q2.5$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),11]),
                               mean(Output.model$q2.5$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),11]),mean(Output.model$q2.5$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),11]),mean(Output.model$q2.5$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),11]),
                               mean(Output.model$q2.5$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),11]),mean(Output.model$q2.5$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),11]))),
                        mean(c(mean(Output.model$q2.5$psi[c(2),c(10,11,12),31]),mean(Output.model$q2.5$psi[c(3),c(10,11,12),31]),mean(Output.model$q2.5$psi[c(4),c(10,11,12),31]),
                               mean(Output.model$q2.5$psi[c(6),c(10,11,12),31]),mean(Output.model$q2.5$psi[c(7),c(10,11,12),31]),mean(Output.model$q2.5$psi[c(8),c(10,11,12),31]),
                               mean(Output.model$q2.5$psi[c(9),c(10,11,12),31]),mean(Output.model$q2.5$psi[c(10),c(10,11,12),31]),mean(Output.model$q2.5$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),31]),
                               mean(Output.model$q2.5$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),31]),mean(Output.model$q2.5$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),31]),mean(Output.model$q2.5$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),31]),
                               mean(Output.model$q2.5$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),31]),mean(Output.model$q2.5$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),31]),mean(Output.model$q2.5$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),31]),
                               mean(Output.model$q2.5$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),31]),mean(Output.model$q2.5$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),31]))),
                        mean(c(mean(Output.model$q2.5$psi[c(2),c(10,11,12),6]),mean(Output.model$q2.5$psi[c(3),c(10,11,12),6]),mean(Output.model$q2.5$psi[c(4),c(10,11,12),6]),
                               mean(Output.model$q2.5$psi[c(6),c(10,11,12),6]),mean(Output.model$q2.5$psi[c(7),c(10,11,12),6]),mean(Output.model$q2.5$psi[c(8),c(10,11,12),6]),
                               mean(Output.model$q2.5$psi[c(9),c(10,11,12),6]),mean(Output.model$q2.5$psi[c(10),c(10,11,12),6]),mean(Output.model$q2.5$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),6]),
                               mean(Output.model$q2.5$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),6]),mean(Output.model$q2.5$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),6]),mean(Output.model$q2.5$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),6]),
                               mean(Output.model$q2.5$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),6]),mean(Output.model$q2.5$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),6]),mean(Output.model$q2.5$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),6]),
                               mean(Output.model$q2.5$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),6]),mean(Output.model$q2.5$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),6])))),
                      c(mean(c(mean(Output.model$q25$psi[c(2),c(10,11,12),45]),mean(Output.model$q25$psi[c(3),c(10,11,12),45]),mean(Output.model$q25$psi[c(4),c(10,11,12),45]),
                               mean(Output.model$q25$psi[c(6),c(10,11,12),45]),mean(Output.model$q25$psi[c(7),c(10,11,12),45]),mean(Output.model$q25$psi[c(8),c(10,11,12),45]),
                               mean(Output.model$q25$psi[c(9),c(10,11,12),45]),mean(Output.model$q25$psi[c(10),c(10,11,12),45]),mean(Output.model$q25$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),45]),
                               mean(Output.model$q25$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),45]),mean(Output.model$q25$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),45]),mean(Output.model$q25$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),45]),
                               mean(Output.model$q25$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),45]),mean(Output.model$q25$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),45]),mean(Output.model$q25$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),45]),
                               mean(Output.model$q25$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),45]),mean(Output.model$q25$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),45]))),
                        mean(c(mean(Output.model$q25$psi[c(2),c(10,11,12),1]),mean(Output.model$q25$psi[c(3),c(10,11,12),1]),mean(Output.model$q25$psi[c(4),c(10,11,12),1]),
                               mean(Output.model$q25$psi[c(6),c(10,11,12),1]),mean(Output.model$q25$psi[c(7),c(10,11,12),1]),mean(Output.model$q25$psi[c(8),c(10,11,12),1]),
                               mean(Output.model$q25$psi[c(9),c(10,11,12),1]),mean(Output.model$q25$psi[c(10),c(10,11,12),1]),mean(Output.model$q25$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),1]),
                               mean(Output.model$q25$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),1]),mean(Output.model$q25$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),1]),mean(Output.model$q25$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),1]),
                               mean(Output.model$q25$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),1]),mean(Output.model$q25$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),1]),mean(Output.model$q25$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),1]),
                               mean(Output.model$q25$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),1]),mean(Output.model$q25$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),1]))),
                        mean(c(mean(Output.model$q25$psi[c(2),c(10,11,12),15]),mean(Output.model$q25$psi[c(3),c(10,11,12),15]),mean(Output.model$q25$psi[c(4),c(10,11,12),15]),
                               mean(Output.model$q25$psi[c(6),c(10,11,12),15]),mean(Output.model$q25$psi[c(7),c(10,11,12),15]),mean(Output.model$q25$psi[c(8),c(10,11,12),15]),
                               mean(Output.model$q25$psi[c(9),c(10,11,12),15]),mean(Output.model$q25$psi[c(10),c(10,11,12),15]),mean(Output.model$q25$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),15]),
                               mean(Output.model$q25$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),15]),mean(Output.model$q25$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),15]),mean(Output.model$q25$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),15]),
                               mean(Output.model$q25$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),15]),mean(Output.model$q25$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),15]),mean(Output.model$q25$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),15]),
                               mean(Output.model$q25$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),15]),mean(Output.model$q25$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),15]))),
                        mean(c(mean(Output.model$q25$psi[c(2),c(10,11,12),7]),mean(Output.model$q25$psi[c(3),c(10,11,12),7]),mean(Output.model$q25$psi[c(4),c(10,11,12),7]),
                               mean(Output.model$q25$psi[c(6),c(10,11,12),7]),mean(Output.model$q25$psi[c(7),c(10,11,12),7]),mean(Output.model$q25$psi[c(8),c(10,11,12),7]),
                               mean(Output.model$q25$psi[c(9),c(10,11,12),7]),mean(Output.model$q25$psi[c(10),c(10,11,12),7]),mean(Output.model$q25$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),7]),
                               mean(Output.model$q25$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),7]),mean(Output.model$q25$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),7]),mean(Output.model$q25$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),7]),
                               mean(Output.model$q25$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),7]),mean(Output.model$q25$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),7]),mean(Output.model$q25$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),7]),
                               mean(Output.model$q25$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),7]),mean(Output.model$q25$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),7]))),
                        mean(c(mean(Output.model$q25$psi[c(2),c(10,11,12),22]),mean(Output.model$q25$psi[c(3),c(10,11,12),22]),mean(Output.model$q25$psi[c(4),c(10,11,12),22]),
                               mean(Output.model$q25$psi[c(6),c(10,11,12),22]),mean(Output.model$q25$psi[c(7),c(10,11,12),22]),mean(Output.model$q25$psi[c(8),c(10,11,12),22]),
                               mean(Output.model$q25$psi[c(9),c(10,11,12),22]),mean(Output.model$q25$psi[c(10),c(10,11,12),22]),mean(Output.model$q25$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),22]),
                               mean(Output.model$q25$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),22]),mean(Output.model$q25$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),22]),mean(Output.model$q25$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),22]),
                               mean(Output.model$q25$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),22]),mean(Output.model$q25$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),22]),mean(Output.model$q25$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),22]),
                               mean(Output.model$q25$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),22]),mean(Output.model$q25$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),22]))),
                        mean(c(mean(Output.model$q25$psi[c(2),c(10,11,12),29]),mean(Output.model$q25$psi[c(3),c(10,11,12),29]),mean(Output.model$q25$psi[c(4),c(10,11,12),29]),
                               mean(Output.model$q25$psi[c(6),c(10,11,12),29]),mean(Output.model$q25$psi[c(7),c(10,11,12),29]),mean(Output.model$q25$psi[c(8),c(10,11,12),29]),
                               mean(Output.model$q25$psi[c(9),c(10,11,12),29]),mean(Output.model$q25$psi[c(10),c(10,11,12),29]),mean(Output.model$q25$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),29]),
                               mean(Output.model$q25$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),29]),mean(Output.model$q25$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),29]),mean(Output.model$q25$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),29]),
                               mean(Output.model$q25$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),29]),mean(Output.model$q25$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),29]),mean(Output.model$q25$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),29]),
                               mean(Output.model$q25$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),29]),mean(Output.model$q25$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),29]))),
                        mean(c(mean(Output.model$q25$psi[c(2),c(10,11,12),18]),mean(Output.model$q25$psi[c(3),c(10,11,12),18]),mean(Output.model$q25$psi[c(4),c(10,11,12),18]),
                               mean(Output.model$q25$psi[c(6),c(10,11,12),18]),mean(Output.model$q25$psi[c(7),c(10,11,12),18]),mean(Output.model$q25$psi[c(8),c(10,11,12),18]),
                               mean(Output.model$q25$psi[c(9),c(10,11,12),18]),mean(Output.model$q25$psi[c(10),c(10,11,12),18]),mean(Output.model$q25$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),18]),
                               mean(Output.model$q25$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),18]),mean(Output.model$q25$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),18]),mean(Output.model$q25$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),18]),
                               mean(Output.model$q25$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),18]),mean(Output.model$q25$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),18]),mean(Output.model$q25$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),18]),
                               mean(Output.model$q25$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),18]),mean(Output.model$q25$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),18]))),
                        mean(c(mean(Output.model$q25$psi[c(2),c(10,11,12),33]),mean(Output.model$q25$psi[c(3),c(10,11,12),33]),mean(Output.model$q25$psi[c(4),c(10,11,12),33]),
                               mean(Output.model$q25$psi[c(6),c(10,11,12),33]),mean(Output.model$q25$psi[c(7),c(10,11,12),33]),mean(Output.model$q25$psi[c(8),c(10,11,12),33]),
                               mean(Output.model$q25$psi[c(9),c(10,11,12),33]),mean(Output.model$q25$psi[c(10),c(10,11,12),33]),mean(Output.model$q25$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),33]),
                               mean(Output.model$q25$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),33]),mean(Output.model$q25$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),33]),mean(Output.model$q25$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),33]),
                               mean(Output.model$q25$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),33]),mean(Output.model$q25$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),33]),mean(Output.model$q25$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),33]),
                               mean(Output.model$q25$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),33]),mean(Output.model$q25$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),33]))),
                        mean(c(mean(Output.model$q25$psi[c(2),c(10,11,12),12]),mean(Output.model$q25$psi[c(3),c(10,11,12),12]),mean(Output.model$q25$psi[c(4),c(10,11,12),12]),
                               mean(Output.model$q25$psi[c(6),c(10,11,12),12]),mean(Output.model$q25$psi[c(7),c(10,11,12),12]),mean(Output.model$q25$psi[c(8),c(10,11,12),12]),
                               mean(Output.model$q25$psi[c(9),c(10,11,12),12]),mean(Output.model$q25$psi[c(10),c(10,11,12),12]),mean(Output.model$q25$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),12]),
                               mean(Output.model$q25$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),12]),mean(Output.model$q25$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),12]),mean(Output.model$q25$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),12]),
                               mean(Output.model$q25$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),12]),mean(Output.model$q25$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),12]),mean(Output.model$q25$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),12]),
                               mean(Output.model$q25$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),12]),mean(Output.model$q25$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),12]))),
                        mean(c(mean(Output.model$q25$psi[c(2),c(10,11,12),9]),mean(Output.model$q25$psi[c(3),c(10,11,12),9]),mean(Output.model$q25$psi[c(4),c(10,11,12),9]),
                               mean(Output.model$q25$psi[c(6),c(10,11,12),9]),mean(Output.model$q25$psi[c(7),c(10,11,12),9]),mean(Output.model$q25$psi[c(8),c(10,11,12),9]),
                               mean(Output.model$q25$psi[c(9),c(10,11,12),9]),mean(Output.model$q25$psi[c(10),c(10,11,12),9]),mean(Output.model$q25$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),9]),
                               mean(Output.model$q25$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),9]),mean(Output.model$q25$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),9]),mean(Output.model$q25$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),9]),
                               mean(Output.model$q25$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),9]),mean(Output.model$q25$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),9]),mean(Output.model$q25$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),9]),
                               mean(Output.model$q25$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),9]),mean(Output.model$q25$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),9]))),
                        mean(c(mean(Output.model$q25$psi[c(2),c(10,11,12),47]),mean(Output.model$q25$psi[c(3),c(10,11,12),47]),mean(Output.model$q25$psi[c(4),c(10,11,12),47]),
                               mean(Output.model$q25$psi[c(6),c(10,11,12),47]),mean(Output.model$q25$psi[c(7),c(10,11,12),47]),mean(Output.model$q25$psi[c(8),c(10,11,12),47]),
                               mean(Output.model$q25$psi[c(9),c(10,11,12),47]),mean(Output.model$q25$psi[c(10),c(10,11,12),47]),mean(Output.model$q25$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),47]),
                               mean(Output.model$q25$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),47]),mean(Output.model$q25$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),47]),mean(Output.model$q25$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),47]),
                               mean(Output.model$q25$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),47]),mean(Output.model$q25$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),47]),mean(Output.model$q25$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),47]),
                               mean(Output.model$q25$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),47]),mean(Output.model$q25$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),47]))),
                        mean(c(mean(Output.model$q25$psi[c(2),c(10,11,12),30]),mean(Output.model$q25$psi[c(3),c(10,11,12),30]),mean(Output.model$q25$psi[c(4),c(10,11,12),30]),
                               mean(Output.model$q25$psi[c(6),c(10,11,12),30]),mean(Output.model$q25$psi[c(7),c(10,11,12),30]),mean(Output.model$q25$psi[c(8),c(10,11,12),30]),
                               mean(Output.model$q25$psi[c(9),c(10,11,12),30]),mean(Output.model$q25$psi[c(10),c(10,11,12),30]),mean(Output.model$q25$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),30]),
                               mean(Output.model$q25$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),30]),mean(Output.model$q25$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),30]),mean(Output.model$q25$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),30]),
                               mean(Output.model$q25$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),30]),mean(Output.model$q25$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),30]),mean(Output.model$q25$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),30]),
                               mean(Output.model$q25$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),30]),mean(Output.model$q25$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),30]))),
                        mean(c(mean(Output.model$q25$psi[c(2),c(10,11,12),40]),mean(Output.model$q25$psi[c(3),c(10,11,12),40]),mean(Output.model$q25$psi[c(4),c(10,11,12),40]),
                               mean(Output.model$q25$psi[c(6),c(10,11,12),40]),mean(Output.model$q25$psi[c(7),c(10,11,12),40]),mean(Output.model$q25$psi[c(8),c(10,11,12),40]),
                               mean(Output.model$q25$psi[c(9),c(10,11,12),40]),mean(Output.model$q25$psi[c(10),c(10,11,12),40]),mean(Output.model$q25$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),40]),
                               mean(Output.model$q25$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),40]),mean(Output.model$q25$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),40]),mean(Output.model$q25$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),40]),
                               mean(Output.model$q25$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),40]),mean(Output.model$q25$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),40]),mean(Output.model$q25$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),40]),
                               mean(Output.model$q25$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),40]),mean(Output.model$q25$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),40]))),
                        mean(c(mean(Output.model$q25$psi[c(2),c(10,11,12),5]),mean(Output.model$q25$psi[c(3),c(10,11,12),5]),mean(Output.model$q25$psi[c(4),c(10,11,12),5]),
                               mean(Output.model$q25$psi[c(6),c(10,11,12),5]),mean(Output.model$q25$psi[c(7),c(10,11,12),5]),mean(Output.model$q25$psi[c(8),c(10,11,12),5]),
                               mean(Output.model$q25$psi[c(9),c(10,11,12),5]),mean(Output.model$q25$psi[c(10),c(10,11,12),5]),mean(Output.model$q25$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),5]),
                               mean(Output.model$q25$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),5]),mean(Output.model$q25$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),5]),mean(Output.model$q25$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),5]),
                               mean(Output.model$q25$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),5]),mean(Output.model$q25$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),5]),mean(Output.model$q25$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),5]),
                               mean(Output.model$q25$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),5]),mean(Output.model$q25$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),5]))),
                        mean(c(mean(Output.model$q25$psi[c(2),c(10,11,12),20]),mean(Output.model$q25$psi[c(3),c(10,11,12),20]),mean(Output.model$q25$psi[c(4),c(10,11,12),20]),
                               mean(Output.model$q25$psi[c(6),c(10,11,12),20]),mean(Output.model$q25$psi[c(7),c(10,11,12),20]),mean(Output.model$q25$psi[c(8),c(10,11,12),20]),
                               mean(Output.model$q25$psi[c(9),c(10,11,12),20]),mean(Output.model$q25$psi[c(10),c(10,11,12),20]),mean(Output.model$q25$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),20]),
                               mean(Output.model$q25$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),20]),mean(Output.model$q25$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),20]),mean(Output.model$q25$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),20]),
                               mean(Output.model$q25$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),20]),mean(Output.model$q25$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),20]),mean(Output.model$q25$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),20]),
                               mean(Output.model$q25$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),20]),mean(Output.model$q25$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),20]))),
                        mean(c(mean(Output.model$q25$psi[c(2),c(10,11,12),23]),mean(Output.model$q25$psi[c(3),c(10,11,12),23]),mean(Output.model$q25$psi[c(4),c(10,11,12),23]),
                               mean(Output.model$q25$psi[c(6),c(10,11,12),23]),mean(Output.model$q25$psi[c(7),c(10,11,12),23]),mean(Output.model$q25$psi[c(8),c(10,11,12),23]),
                               mean(Output.model$q25$psi[c(9),c(10,11,12),23]),mean(Output.model$q25$psi[c(10),c(10,11,12),23]),mean(Output.model$q25$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),23]),
                               mean(Output.model$q25$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),23]),mean(Output.model$q25$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),23]),mean(Output.model$q25$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),23]),
                               mean(Output.model$q25$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),23]),mean(Output.model$q25$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),23]),mean(Output.model$q25$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),23]),
                               mean(Output.model$q25$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),23]),mean(Output.model$q25$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),23]))),
                        mean(c(mean(Output.model$q25$psi[c(2),c(10,11,12),3]),mean(Output.model$q25$psi[c(3),c(10,11,12),3]),mean(Output.model$q25$psi[c(4),c(10,11,12),3]),
                               mean(Output.model$q25$psi[c(6),c(10,11,12),3]),mean(Output.model$q25$psi[c(7),c(10,11,12),3]),mean(Output.model$q25$psi[c(8),c(10,11,12),3]),
                               mean(Output.model$q25$psi[c(9),c(10,11,12),3]),mean(Output.model$q25$psi[c(10),c(10,11,12),3]),mean(Output.model$q25$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),3]),
                               mean(Output.model$q25$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),3]),mean(Output.model$q25$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),3]),mean(Output.model$q25$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),3]),
                               mean(Output.model$q25$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),3]),mean(Output.model$q25$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),3]),mean(Output.model$q25$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),3]),
                               mean(Output.model$q25$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),3]),mean(Output.model$q25$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),3]))),
                        mean(c(mean(Output.model$q25$psi[c(2),c(10,11,12),27]),mean(Output.model$q25$psi[c(3),c(10,11,12),27]),mean(Output.model$q25$psi[c(4),c(10,11,12),27]),
                               mean(Output.model$q25$psi[c(6),c(10,11,12),27]),mean(Output.model$q25$psi[c(7),c(10,11,12),27]),mean(Output.model$q25$psi[c(8),c(10,11,12),27]),
                               mean(Output.model$q25$psi[c(9),c(10,11,12),27]),mean(Output.model$q25$psi[c(10),c(10,11,12),27]),mean(Output.model$q25$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),27]),
                               mean(Output.model$q25$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),27]),mean(Output.model$q25$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),27]),mean(Output.model$q25$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),27]),
                               mean(Output.model$q25$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),27]),mean(Output.model$q25$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),27]),mean(Output.model$q25$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),27]),
                               mean(Output.model$q25$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),27]),mean(Output.model$q25$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),27]))),
                        mean(c(mean(Output.model$q25$psi[c(2),c(10,11,12),26]),mean(Output.model$q25$psi[c(3),c(10,11,12),26]),mean(Output.model$q25$psi[c(4),c(10,11,12),26]),
                               mean(Output.model$q25$psi[c(6),c(10,11,12),26]),mean(Output.model$q25$psi[c(7),c(10,11,12),26]),mean(Output.model$q25$psi[c(8),c(10,11,12),26]),
                               mean(Output.model$q25$psi[c(9),c(10,11,12),26]),mean(Output.model$q25$psi[c(10),c(10,11,12),26]),mean(Output.model$q25$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),26]),
                               mean(Output.model$q25$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),26]),mean(Output.model$q25$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),26]),mean(Output.model$q25$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),26]),
                               mean(Output.model$q25$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),26]),mean(Output.model$q25$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),26]),mean(Output.model$q25$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),26]),
                               mean(Output.model$q25$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),26]),mean(Output.model$q25$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),26]))),
                        mean(c(mean(Output.model$q25$psi[c(2),c(10,11,12),44]),mean(Output.model$q25$psi[c(3),c(10,11,12),44]),mean(Output.model$q25$psi[c(4),c(10,11,12),44]),
                               mean(Output.model$q25$psi[c(6),c(10,11,12),44]),mean(Output.model$q25$psi[c(7),c(10,11,12),44]),mean(Output.model$q25$psi[c(8),c(10,11,12),44]),
                               mean(Output.model$q25$psi[c(9),c(10,11,12),44]),mean(Output.model$q25$psi[c(10),c(10,11,12),44]),mean(Output.model$q25$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),44]),
                               mean(Output.model$q25$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),44]),mean(Output.model$q25$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),44]),mean(Output.model$q25$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),44]),
                               mean(Output.model$q25$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),44]),mean(Output.model$q25$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),44]),mean(Output.model$q25$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),44]),
                               mean(Output.model$q25$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),44]),mean(Output.model$q25$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),44]))),
                        mean(c(mean(Output.model$q25$psi[c(2),c(10,11,12),42]),mean(Output.model$q25$psi[c(3),c(10,11,12),42]),mean(Output.model$q25$psi[c(4),c(10,11,12),42]),
                               mean(Output.model$q25$psi[c(6),c(10,11,12),42]),mean(Output.model$q25$psi[c(7),c(10,11,12),42]),mean(Output.model$q25$psi[c(8),c(10,11,12),42]),
                               mean(Output.model$q25$psi[c(9),c(10,11,12),42]),mean(Output.model$q25$psi[c(10),c(10,11,12),42]),mean(Output.model$q25$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),42]),
                               mean(Output.model$q25$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),42]),mean(Output.model$q25$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),42]),mean(Output.model$q25$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),42]),
                               mean(Output.model$q25$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),42]),mean(Output.model$q25$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),42]),mean(Output.model$q25$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),42]),
                               mean(Output.model$q25$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),42]),mean(Output.model$q25$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),42]))),
                        mean(c(mean(Output.model$q25$psi[c(2),c(10,11,12),43]),mean(Output.model$q25$psi[c(3),c(10,11,12),43]),mean(Output.model$q25$psi[c(4),c(10,11,12),43]),
                               mean(Output.model$q25$psi[c(6),c(10,11,12),43]),mean(Output.model$q25$psi[c(7),c(10,11,12),43]),mean(Output.model$q25$psi[c(8),c(10,11,12),43]),
                               mean(Output.model$q25$psi[c(9),c(10,11,12),43]),mean(Output.model$q25$psi[c(10),c(10,11,12),43]),mean(Output.model$q25$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),43]),
                               mean(Output.model$q25$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),43]),mean(Output.model$q25$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),43]),mean(Output.model$q25$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),43]),
                               mean(Output.model$q25$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),43]),mean(Output.model$q25$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),43]),mean(Output.model$q25$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),43]),
                               mean(Output.model$q25$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),43]),mean(Output.model$q25$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),43]))),
                        mean(c(mean(Output.model$q25$psi[c(2),c(10,11,12),38]),mean(Output.model$q25$psi[c(3),c(10,11,12),38]),mean(Output.model$q25$psi[c(4),c(10,11,12),38]),
                               mean(Output.model$q25$psi[c(6),c(10,11,12),38]),mean(Output.model$q25$psi[c(7),c(10,11,12),38]),mean(Output.model$q25$psi[c(8),c(10,11,12),38]),
                               mean(Output.model$q25$psi[c(9),c(10,11,12),38]),mean(Output.model$q25$psi[c(10),c(10,11,12),38]),mean(Output.model$q25$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),38]),
                               mean(Output.model$q25$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),38]),mean(Output.model$q25$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),38]),mean(Output.model$q25$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),38]),
                               mean(Output.model$q25$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),38]),mean(Output.model$q25$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),38]),mean(Output.model$q25$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),38]),
                               mean(Output.model$q25$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),38]),mean(Output.model$q25$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),38]))),
                        mean(c(mean(Output.model$q25$psi[c(2),c(10,11,12),24]),mean(Output.model$q25$psi[c(3),c(10,11,12),24]),mean(Output.model$q25$psi[c(4),c(10,11,12),24]),
                               mean(Output.model$q25$psi[c(6),c(10,11,12),24]),mean(Output.model$q25$psi[c(7),c(10,11,12),24]),mean(Output.model$q25$psi[c(8),c(10,11,12),24]),
                               mean(Output.model$q25$psi[c(9),c(10,11,12),24]),mean(Output.model$q25$psi[c(10),c(10,11,12),24]),mean(Output.model$q25$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),24]),
                               mean(Output.model$q25$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),24]),mean(Output.model$q25$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),24]),mean(Output.model$q25$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),24]),
                               mean(Output.model$q25$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),24]),mean(Output.model$q25$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),24]),mean(Output.model$q25$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),24]),
                               mean(Output.model$q25$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),24]),mean(Output.model$q25$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),24]))),
                        mean(c(mean(Output.model$q25$psi[c(2),c(10,11,12),37]),mean(Output.model$q25$psi[c(3),c(10,11,12),37]),mean(Output.model$q25$psi[c(4),c(10,11,12),37]),
                               mean(Output.model$q25$psi[c(6),c(10,11,12),37]),mean(Output.model$q25$psi[c(7),c(10,11,12),37]),mean(Output.model$q25$psi[c(8),c(10,11,12),37]),
                               mean(Output.model$q25$psi[c(9),c(10,11,12),37]),mean(Output.model$q25$psi[c(10),c(10,11,12),37]),mean(Output.model$q25$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),37]),
                               mean(Output.model$q25$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),37]),mean(Output.model$q25$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),37]),mean(Output.model$q25$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),37]),
                               mean(Output.model$q25$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),37]),mean(Output.model$q25$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),37]),mean(Output.model$q25$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),37]),
                               mean(Output.model$q25$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),37]),mean(Output.model$q25$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),37]))),
                        mean(c(mean(Output.model$q25$psi[c(2),c(10,11,12),13]),mean(Output.model$q25$psi[c(3),c(10,11,12),13]),mean(Output.model$q25$psi[c(4),c(10,11,12),13]),
                               mean(Output.model$q25$psi[c(6),c(10,11,12),13]),mean(Output.model$q25$psi[c(7),c(10,11,12),13]),mean(Output.model$q25$psi[c(8),c(10,11,12),13]),
                               mean(Output.model$q25$psi[c(9),c(10,11,12),13]),mean(Output.model$q25$psi[c(10),c(10,11,12),13]),mean(Output.model$q25$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),13]),
                               mean(Output.model$q25$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),13]),mean(Output.model$q25$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),13]),mean(Output.model$q25$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),13]),
                               mean(Output.model$q25$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),13]),mean(Output.model$q25$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),13]),mean(Output.model$q25$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),13]),
                               mean(Output.model$q25$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),13]),mean(Output.model$q25$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),13]))),
                        mean(c(mean(Output.model$q25$psi[c(2),c(10,11,12),21]),mean(Output.model$q25$psi[c(3),c(10,11,12),21]),mean(Output.model$q25$psi[c(4),c(10,11,12),21]),
                               mean(Output.model$q25$psi[c(6),c(10,11,12),21]),mean(Output.model$q25$psi[c(7),c(10,11,12),21]),mean(Output.model$q25$psi[c(8),c(10,11,12),21]),
                               mean(Output.model$q25$psi[c(9),c(10,11,12),21]),mean(Output.model$q25$psi[c(10),c(10,11,12),21]),mean(Output.model$q25$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),21]),
                               mean(Output.model$q25$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),21]),mean(Output.model$q25$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),21]),mean(Output.model$q25$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),21]),
                               mean(Output.model$q25$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),21]),mean(Output.model$q25$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),21]),mean(Output.model$q25$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),21]),
                               mean(Output.model$q25$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),21]),mean(Output.model$q25$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),21]))),
                        mean(c(mean(Output.model$q25$psi[c(2),c(10,11,12),34]),mean(Output.model$q25$psi[c(3),c(10,11,12),34]),mean(Output.model$q25$psi[c(4),c(10,11,12),34]),
                               mean(Output.model$q25$psi[c(6),c(10,11,12),34]),mean(Output.model$q25$psi[c(7),c(10,11,12),34]),mean(Output.model$q25$psi[c(8),c(10,11,12),34]),
                               mean(Output.model$q25$psi[c(9),c(10,11,12),34]),mean(Output.model$q25$psi[c(10),c(10,11,12),34]),mean(Output.model$q25$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),34]),
                               mean(Output.model$q25$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),34]),mean(Output.model$q25$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),34]),mean(Output.model$q25$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),34]),
                               mean(Output.model$q25$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),34]),mean(Output.model$q25$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),34]),mean(Output.model$q25$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),34]),
                               mean(Output.model$q25$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),34]),mean(Output.model$q25$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),34]))),
                        mean(c(mean(Output.model$q25$psi[c(2),c(10,11,12),2]),mean(Output.model$q25$psi[c(3),c(10,11,12),2]),mean(Output.model$q25$psi[c(4),c(10,11,12),2]),
                               mean(Output.model$q25$psi[c(6),c(10,11,12),2]),mean(Output.model$q25$psi[c(7),c(10,11,12),2]),mean(Output.model$q25$psi[c(8),c(10,11,12),2]),
                               mean(Output.model$q25$psi[c(9),c(10,11,12),2]),mean(Output.model$q25$psi[c(10),c(10,11,12),2]),mean(Output.model$q25$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),2]),
                               mean(Output.model$q25$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),2]),mean(Output.model$q25$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),2]),mean(Output.model$q25$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),2]),
                               mean(Output.model$q25$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),2]),mean(Output.model$q25$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),2]),mean(Output.model$q25$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),2]),
                               mean(Output.model$q25$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),2]),mean(Output.model$q25$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),2]))),
                        mean(c(mean(Output.model$q25$psi[c(2),c(10,11,12),32]),mean(Output.model$q25$psi[c(3),c(10,11,12),32]),mean(Output.model$q25$psi[c(4),c(10,11,12),32]),
                               mean(Output.model$q25$psi[c(6),c(10,11,12),32]),mean(Output.model$q25$psi[c(7),c(10,11,12),32]),mean(Output.model$q25$psi[c(8),c(10,11,12),32]),
                               mean(Output.model$q25$psi[c(9),c(10,11,12),32]),mean(Output.model$q25$psi[c(10),c(10,11,12),32]),mean(Output.model$q25$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),32]),
                               mean(Output.model$q25$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),32]),mean(Output.model$q25$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),32]),mean(Output.model$q25$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),32]),
                               mean(Output.model$q25$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),32]),mean(Output.model$q25$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),32]),mean(Output.model$q25$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),32]),
                               mean(Output.model$q25$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),32]),mean(Output.model$q25$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),32]))),
                        mean(c(mean(Output.model$q25$psi[c(2),c(10,11,12),17]),mean(Output.model$q25$psi[c(3),c(10,11,12),17]),mean(Output.model$q25$psi[c(4),c(10,11,12),17]),
                               mean(Output.model$q25$psi[c(6),c(10,11,12),17]),mean(Output.model$q25$psi[c(7),c(10,11,12),17]),mean(Output.model$q25$psi[c(8),c(10,11,12),17]),
                               mean(Output.model$q25$psi[c(9),c(10,11,12),17]),mean(Output.model$q25$psi[c(10),c(10,11,12),17]),mean(Output.model$q25$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),17]),
                               mean(Output.model$q25$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),17]),mean(Output.model$q25$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),17]),mean(Output.model$q25$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),17]),
                               mean(Output.model$q25$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),17]),mean(Output.model$q25$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),17]),mean(Output.model$q25$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),17]),
                               mean(Output.model$q25$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),17]),mean(Output.model$q25$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),17]))),
                        mean(c(mean(Output.model$q25$psi[c(2),c(10,11,12),4]),mean(Output.model$q25$psi[c(3),c(10,11,12),4]),mean(Output.model$q25$psi[c(4),c(10,11,12),4]),
                               mean(Output.model$q25$psi[c(6),c(10,11,12),4]),mean(Output.model$q25$psi[c(7),c(10,11,12),4]),mean(Output.model$q25$psi[c(8),c(10,11,12),4]),
                               mean(Output.model$q25$psi[c(9),c(10,11,12),4]),mean(Output.model$q25$psi[c(10),c(10,11,12),4]),mean(Output.model$q25$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),4]),
                               mean(Output.model$q25$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),4]),mean(Output.model$q25$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),4]),mean(Output.model$q25$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),4]),
                               mean(Output.model$q25$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),4]),mean(Output.model$q25$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),4]),mean(Output.model$q25$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),4]),
                               mean(Output.model$q25$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),4]),mean(Output.model$q25$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),4]))),
                        mean(c(mean(Output.model$q25$psi[c(2),c(10,11,12),8]),mean(Output.model$q25$psi[c(3),c(10,11,12),8]),mean(Output.model$q25$psi[c(4),c(10,11,12),8]),
                               mean(Output.model$q25$psi[c(6),c(10,11,12),8]),mean(Output.model$q25$psi[c(7),c(10,11,12),8]),mean(Output.model$q25$psi[c(8),c(10,11,12),8]),
                               mean(Output.model$q25$psi[c(9),c(10,11,12),8]),mean(Output.model$q25$psi[c(10),c(10,11,12),8]),mean(Output.model$q25$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),8]),
                               mean(Output.model$q25$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),8]),mean(Output.model$q25$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),8]),mean(Output.model$q25$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),8]),
                               mean(Output.model$q25$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),8]),mean(Output.model$q25$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),8]),mean(Output.model$q25$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),8]),
                               mean(Output.model$q25$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),8]),mean(Output.model$q25$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),8]))),
                        mean(c(mean(Output.model$q25$psi[c(2),c(10,11,12),39]),mean(Output.model$q25$psi[c(3),c(10,11,12),39]),mean(Output.model$q25$psi[c(4),c(10,11,12),39]),
                               mean(Output.model$q25$psi[c(6),c(10,11,12),39]),mean(Output.model$q25$psi[c(7),c(10,11,12),39]),mean(Output.model$q25$psi[c(8),c(10,11,12),39]),
                               mean(Output.model$q25$psi[c(9),c(10,11,12),39]),mean(Output.model$q25$psi[c(10),c(10,11,12),39]),mean(Output.model$q25$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),39]),
                               mean(Output.model$q25$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),39]),mean(Output.model$q25$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),39]),mean(Output.model$q25$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),39]),
                               mean(Output.model$q25$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),39]),mean(Output.model$q25$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),39]),mean(Output.model$q25$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),39]),
                               mean(Output.model$q25$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),39]),mean(Output.model$q25$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),39]))),
                        mean(c(mean(Output.model$q25$psi[c(2),c(10,11,12),10]),mean(Output.model$q25$psi[c(3),c(10,11,12),10]),mean(Output.model$q25$psi[c(4),c(10,11,12),10]),
                               mean(Output.model$q25$psi[c(6),c(10,11,12),10]),mean(Output.model$q25$psi[c(7),c(10,11,12),10]),mean(Output.model$q25$psi[c(8),c(10,11,12),10]),
                               mean(Output.model$q25$psi[c(9),c(10,11,12),10]),mean(Output.model$q25$psi[c(10),c(10,11,12),10]),mean(Output.model$q25$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),10]),
                               mean(Output.model$q25$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),10]),mean(Output.model$q25$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),10]),mean(Output.model$q25$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),10]),
                               mean(Output.model$q25$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),10]),mean(Output.model$q25$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),10]),mean(Output.model$q25$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),10]),
                               mean(Output.model$q25$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),10]),mean(Output.model$q25$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),10]))),
                        mean(c(mean(Output.model$q25$psi[c(2),c(10,11,12),19]),mean(Output.model$q25$psi[c(3),c(10,11,12),19]),mean(Output.model$q25$psi[c(4),c(10,11,12),19]),
                               mean(Output.model$q25$psi[c(6),c(10,11,12),19]),mean(Output.model$q25$psi[c(7),c(10,11,12),19]),mean(Output.model$q25$psi[c(8),c(10,11,12),19]),
                               mean(Output.model$q25$psi[c(9),c(10,11,12),19]),mean(Output.model$q25$psi[c(10),c(10,11,12),19]),mean(Output.model$q25$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),19]),
                               mean(Output.model$q25$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),19]),mean(Output.model$q25$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),19]),mean(Output.model$q25$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),19]),
                               mean(Output.model$q25$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),19]),mean(Output.model$q25$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),19]),mean(Output.model$q25$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),19]),
                               mean(Output.model$q25$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),19]),mean(Output.model$q25$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),19]))),
                        mean(c(mean(Output.model$q25$psi[c(2),c(10,11,12),35]),mean(Output.model$q25$psi[c(3),c(10,11,12),35]),mean(Output.model$q25$psi[c(4),c(10,11,12),35]),
                               mean(Output.model$q25$psi[c(6),c(10,11,12),35]),mean(Output.model$q25$psi[c(7),c(10,11,12),35]),mean(Output.model$q25$psi[c(8),c(10,11,12),35]),
                               mean(Output.model$q25$psi[c(9),c(10,11,12),35]),mean(Output.model$q25$psi[c(10),c(10,11,12),35]),mean(Output.model$q25$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),35]),
                               mean(Output.model$q25$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),35]),mean(Output.model$q25$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),35]),mean(Output.model$q25$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),35]),
                               mean(Output.model$q25$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),35]),mean(Output.model$q25$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),35]),mean(Output.model$q25$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),35]),
                               mean(Output.model$q25$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),35]),mean(Output.model$q25$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),35]))),
                        mean(c(mean(Output.model$q25$psi[c(2),c(10,11,12),25]),mean(Output.model$q25$psi[c(3),c(10,11,12),25]),mean(Output.model$q25$psi[c(4),c(10,11,12),25]),
                               mean(Output.model$q25$psi[c(6),c(10,11,12),25]),mean(Output.model$q25$psi[c(7),c(10,11,12),25]),mean(Output.model$q25$psi[c(8),c(10,11,12),25]),
                               mean(Output.model$q25$psi[c(9),c(10,11,12),25]),mean(Output.model$q25$psi[c(10),c(10,11,12),25]),mean(Output.model$q25$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),25]),
                               mean(Output.model$q25$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),25]),mean(Output.model$q25$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),25]),mean(Output.model$q25$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),25]),
                               mean(Output.model$q25$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),25]),mean(Output.model$q25$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),25]),mean(Output.model$q25$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),25]),
                               mean(Output.model$q25$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),25]),mean(Output.model$q25$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),25]))),
                        mean(c(mean(Output.model$q25$psi[c(2),c(10,11,12),28]),mean(Output.model$q25$psi[c(3),c(10,11,12),28]),mean(Output.model$q25$psi[c(4),c(10,11,12),28]),
                               mean(Output.model$q25$psi[c(6),c(10,11,12),28]),mean(Output.model$q25$psi[c(7),c(10,11,12),28]),mean(Output.model$q25$psi[c(8),c(10,11,12),28]),
                               mean(Output.model$q25$psi[c(9),c(10,11,12),28]),mean(Output.model$q25$psi[c(10),c(10,11,12),28]),mean(Output.model$q25$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),28]),
                               mean(Output.model$q25$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),28]),mean(Output.model$q25$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),28]),mean(Output.model$q25$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),28]),
                               mean(Output.model$q25$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),28]),mean(Output.model$q25$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),28]),mean(Output.model$q25$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),28]),
                               mean(Output.model$q25$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),28]),mean(Output.model$q25$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),28]))),
                        mean(c(mean(Output.model$q25$psi[c(2),c(10,11,12),46]),mean(Output.model$q25$psi[c(3),c(10,11,12),46]),mean(Output.model$q25$psi[c(4),c(10,11,12),46]),
                               mean(Output.model$q25$psi[c(6),c(10,11,12),46]),mean(Output.model$q25$psi[c(7),c(10,11,12),46]),mean(Output.model$q25$psi[c(8),c(10,11,12),46]),
                               mean(Output.model$q25$psi[c(9),c(10,11,12),46]),mean(Output.model$q25$psi[c(10),c(10,11,12),46]),mean(Output.model$q25$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),46]),
                               mean(Output.model$q25$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),46]),mean(Output.model$q25$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),46]),mean(Output.model$q25$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),46]),
                               mean(Output.model$q25$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),46]),mean(Output.model$q25$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),46]),mean(Output.model$q25$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),46]),
                               mean(Output.model$q25$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),46]),mean(Output.model$q25$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),46]))),
                        mean(c(mean(Output.model$q25$psi[c(2),c(10,11,12),41]),mean(Output.model$q25$psi[c(3),c(10,11,12),41]),mean(Output.model$q25$psi[c(4),c(10,11,12),41]),
                               mean(Output.model$q25$psi[c(6),c(10,11,12),41]),mean(Output.model$q25$psi[c(7),c(10,11,12),41]),mean(Output.model$q25$psi[c(8),c(10,11,12),41]),
                               mean(Output.model$q25$psi[c(9),c(10,11,12),41]),mean(Output.model$q25$psi[c(10),c(10,11,12),41]),mean(Output.model$q25$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),41]),
                               mean(Output.model$q25$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),41]),mean(Output.model$q25$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),41]),mean(Output.model$q25$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),41]),
                               mean(Output.model$q25$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),41]),mean(Output.model$q25$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),41]),mean(Output.model$q25$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),41]),
                               mean(Output.model$q25$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),41]),mean(Output.model$q25$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),41]))),
                        mean(c(mean(Output.model$q25$psi[c(2),c(10,11,12),16]),mean(Output.model$q25$psi[c(3),c(10,11,12),16]),mean(Output.model$q25$psi[c(4),c(10,11,12),16]),
                               mean(Output.model$q25$psi[c(6),c(10,11,12),16]),mean(Output.model$q25$psi[c(7),c(10,11,12),16]),mean(Output.model$q25$psi[c(8),c(10,11,12),16]),
                               mean(Output.model$q25$psi[c(9),c(10,11,12),16]),mean(Output.model$q25$psi[c(10),c(10,11,12),16]),mean(Output.model$q25$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),16]),
                               mean(Output.model$q25$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),16]),mean(Output.model$q25$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),16]),mean(Output.model$q25$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),16]),
                               mean(Output.model$q25$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),16]),mean(Output.model$q25$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),16]),mean(Output.model$q25$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),16]),
                               mean(Output.model$q25$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),16]),mean(Output.model$q25$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),16]))),
                        mean(c(mean(Output.model$q25$psi[c(2),c(10,11,12),36]),mean(Output.model$q25$psi[c(3),c(10,11,12),36]),mean(Output.model$q25$psi[c(4),c(10,11,12),36]),
                               mean(Output.model$q25$psi[c(6),c(10,11,12),36]),mean(Output.model$q25$psi[c(7),c(10,11,12),36]),mean(Output.model$q25$psi[c(8),c(10,11,12),36]),
                               mean(Output.model$q25$psi[c(9),c(10,11,12),36]),mean(Output.model$q25$psi[c(10),c(10,11,12),36]),mean(Output.model$q25$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),36]),
                               mean(Output.model$q25$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),36]),mean(Output.model$q25$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),36]),mean(Output.model$q25$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),36]),
                               mean(Output.model$q25$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),36]),mean(Output.model$q25$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),36]),mean(Output.model$q25$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),36]),
                               mean(Output.model$q25$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),36]),mean(Output.model$q25$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),36]))),
                        mean(c(mean(Output.model$q25$psi[c(2),c(10,11,12),14]),mean(Output.model$q25$psi[c(3),c(10,11,12),14]),mean(Output.model$q25$psi[c(4),c(10,11,12),14]),
                               mean(Output.model$q25$psi[c(6),c(10,11,12),14]),mean(Output.model$q25$psi[c(7),c(10,11,12),14]),mean(Output.model$q25$psi[c(8),c(10,11,12),14]),
                               mean(Output.model$q25$psi[c(9),c(10,11,12),14]),mean(Output.model$q25$psi[c(10),c(10,11,12),14]),mean(Output.model$q25$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),14]),
                               mean(Output.model$q25$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),14]),mean(Output.model$q25$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),14]),mean(Output.model$q25$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),14]),
                               mean(Output.model$q25$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),14]),mean(Output.model$q25$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),14]),mean(Output.model$q25$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),14]),
                               mean(Output.model$q25$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),14]),mean(Output.model$q25$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),14]))),
                        mean(c(mean(Output.model$q25$psi[c(2),c(10,11,12),11]),mean(Output.model$q25$psi[c(3),c(10,11,12),11]),mean(Output.model$q25$psi[c(4),c(10,11,12),11]),
                               mean(Output.model$q25$psi[c(6),c(10,11,12),11]),mean(Output.model$q25$psi[c(7),c(10,11,12),11]),mean(Output.model$q25$psi[c(8),c(10,11,12),11]),
                               mean(Output.model$q25$psi[c(9),c(10,11,12),11]),mean(Output.model$q25$psi[c(10),c(10,11,12),11]),mean(Output.model$q25$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),11]),
                               mean(Output.model$q25$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),11]),mean(Output.model$q25$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),11]),mean(Output.model$q25$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),11]),
                               mean(Output.model$q25$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),11]),mean(Output.model$q25$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),11]),mean(Output.model$q25$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),11]),
                               mean(Output.model$q25$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),11]),mean(Output.model$q25$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),11]))),
                        mean(c(mean(Output.model$q25$psi[c(2),c(10,11,12),31]),mean(Output.model$q25$psi[c(3),c(10,11,12),31]),mean(Output.model$q25$psi[c(4),c(10,11,12),31]),
                               mean(Output.model$q25$psi[c(6),c(10,11,12),31]),mean(Output.model$q25$psi[c(7),c(10,11,12),31]),mean(Output.model$q25$psi[c(8),c(10,11,12),31]),
                               mean(Output.model$q25$psi[c(9),c(10,11,12),31]),mean(Output.model$q25$psi[c(10),c(10,11,12),31]),mean(Output.model$q25$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),31]),
                               mean(Output.model$q25$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),31]),mean(Output.model$q25$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),31]),mean(Output.model$q25$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),31]),
                               mean(Output.model$q25$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),31]),mean(Output.model$q25$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),31]),mean(Output.model$q25$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),31]),
                               mean(Output.model$q25$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),31]),mean(Output.model$q25$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),31]))),
                        mean(c(mean(Output.model$q25$psi[c(2),c(10,11,12),6]),mean(Output.model$q25$psi[c(3),c(10,11,12),6]),mean(Output.model$q25$psi[c(4),c(10,11,12),6]),
                               mean(Output.model$q25$psi[c(6),c(10,11,12),6]),mean(Output.model$q25$psi[c(7),c(10,11,12),6]),mean(Output.model$q25$psi[c(8),c(10,11,12),6]),
                               mean(Output.model$q25$psi[c(9),c(10,11,12),6]),mean(Output.model$q25$psi[c(10),c(10,11,12),6]),mean(Output.model$q25$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),6]),
                               mean(Output.model$q25$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),6]),mean(Output.model$q25$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),6]),mean(Output.model$q25$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),6]),
                               mean(Output.model$q25$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),6]),mean(Output.model$q25$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),6]),mean(Output.model$q25$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),6]),
                               mean(Output.model$q25$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),6]),mean(Output.model$q25$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),6])))),
                      c(mean(c(mean(Output.model$mean$psi[c(2),c(10,11,12),45]),mean(Output.model$mean$psi[c(3),c(10,11,12),45]),mean(Output.model$mean$psi[c(4),c(10,11,12),45]),
                               mean(Output.model$mean$psi[c(6),c(10,11,12),45]),mean(Output.model$mean$psi[c(7),c(10,11,12),45]),mean(Output.model$mean$psi[c(8),c(10,11,12),45]),
                               mean(Output.model$mean$psi[c(9),c(10,11,12),45]),mean(Output.model$mean$psi[c(10),c(10,11,12),45]),mean(Output.model$mean$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),45]),
                               mean(Output.model$mean$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),45]),mean(Output.model$mean$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),45]),mean(Output.model$mean$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),45]),
                               mean(Output.model$mean$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),45]),mean(Output.model$mean$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),45]),mean(Output.model$mean$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),45]),
                               mean(Output.model$mean$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),45]),mean(Output.model$mean$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),45]))),
                        mean(c(mean(Output.model$mean$psi[c(2),c(10,11,12),1]),mean(Output.model$mean$psi[c(3),c(10,11,12),1]),mean(Output.model$mean$psi[c(4),c(10,11,12),1]),
                               mean(Output.model$mean$psi[c(6),c(10,11,12),1]),mean(Output.model$mean$psi[c(7),c(10,11,12),1]),mean(Output.model$mean$psi[c(8),c(10,11,12),1]),
                               mean(Output.model$mean$psi[c(9),c(10,11,12),1]),mean(Output.model$mean$psi[c(10),c(10,11,12),1]),mean(Output.model$mean$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),1]),
                               mean(Output.model$mean$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),1]),mean(Output.model$mean$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),1]),mean(Output.model$mean$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),1]),
                               mean(Output.model$mean$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),1]),mean(Output.model$mean$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),1]),mean(Output.model$mean$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),1]),
                               mean(Output.model$mean$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),1]),mean(Output.model$mean$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),1]))),
                        mean(c(mean(Output.model$mean$psi[c(2),c(10,11,12),15]),mean(Output.model$mean$psi[c(3),c(10,11,12),15]),mean(Output.model$mean$psi[c(4),c(10,11,12),15]),
                               mean(Output.model$mean$psi[c(6),c(10,11,12),15]),mean(Output.model$mean$psi[c(7),c(10,11,12),15]),mean(Output.model$mean$psi[c(8),c(10,11,12),15]),
                               mean(Output.model$mean$psi[c(9),c(10,11,12),15]),mean(Output.model$mean$psi[c(10),c(10,11,12),15]),mean(Output.model$mean$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),15]),
                               mean(Output.model$mean$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),15]),mean(Output.model$mean$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),15]),mean(Output.model$mean$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),15]),
                               mean(Output.model$mean$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),15]),mean(Output.model$mean$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),15]),mean(Output.model$mean$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),15]),
                               mean(Output.model$mean$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),15]),mean(Output.model$mean$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),15]))),
                        mean(c(mean(Output.model$mean$psi[c(2),c(10,11,12),7]),mean(Output.model$mean$psi[c(3),c(10,11,12),7]),mean(Output.model$mean$psi[c(4),c(10,11,12),7]),
                               mean(Output.model$mean$psi[c(6),c(10,11,12),7]),mean(Output.model$mean$psi[c(7),c(10,11,12),7]),mean(Output.model$mean$psi[c(8),c(10,11,12),7]),
                               mean(Output.model$mean$psi[c(9),c(10,11,12),7]),mean(Output.model$mean$psi[c(10),c(10,11,12),7]),mean(Output.model$mean$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),7]),
                               mean(Output.model$mean$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),7]),mean(Output.model$mean$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),7]),mean(Output.model$mean$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),7]),
                               mean(Output.model$mean$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),7]),mean(Output.model$mean$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),7]),mean(Output.model$mean$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),7]),
                               mean(Output.model$mean$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),7]),mean(Output.model$mean$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),7]))),
                        mean(c(mean(Output.model$mean$psi[c(2),c(10,11,12),22]),mean(Output.model$mean$psi[c(3),c(10,11,12),22]),mean(Output.model$mean$psi[c(4),c(10,11,12),22]),
                               mean(Output.model$mean$psi[c(6),c(10,11,12),22]),mean(Output.model$mean$psi[c(7),c(10,11,12),22]),mean(Output.model$mean$psi[c(8),c(10,11,12),22]),
                               mean(Output.model$mean$psi[c(9),c(10,11,12),22]),mean(Output.model$mean$psi[c(10),c(10,11,12),22]),mean(Output.model$mean$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),22]),
                               mean(Output.model$mean$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),22]),mean(Output.model$mean$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),22]),mean(Output.model$mean$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),22]),
                               mean(Output.model$mean$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),22]),mean(Output.model$mean$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),22]),mean(Output.model$mean$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),22]),
                               mean(Output.model$mean$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),22]),mean(Output.model$mean$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),22]))),
                        mean(c(mean(Output.model$mean$psi[c(2),c(10,11,12),29]),mean(Output.model$mean$psi[c(3),c(10,11,12),29]),mean(Output.model$mean$psi[c(4),c(10,11,12),29]),
                               mean(Output.model$mean$psi[c(6),c(10,11,12),29]),mean(Output.model$mean$psi[c(7),c(10,11,12),29]),mean(Output.model$mean$psi[c(8),c(10,11,12),29]),
                               mean(Output.model$mean$psi[c(9),c(10,11,12),29]),mean(Output.model$mean$psi[c(10),c(10,11,12),29]),mean(Output.model$mean$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),29]),
                               mean(Output.model$mean$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),29]),mean(Output.model$mean$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),29]),mean(Output.model$mean$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),29]),
                               mean(Output.model$mean$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),29]),mean(Output.model$mean$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),29]),mean(Output.model$mean$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),29]),
                               mean(Output.model$mean$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),29]),mean(Output.model$mean$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),29]))),
                        mean(c(mean(Output.model$mean$psi[c(2),c(10,11,12),18]),mean(Output.model$mean$psi[c(3),c(10,11,12),18]),mean(Output.model$mean$psi[c(4),c(10,11,12),18]),
                               mean(Output.model$mean$psi[c(6),c(10,11,12),18]),mean(Output.model$mean$psi[c(7),c(10,11,12),18]),mean(Output.model$mean$psi[c(8),c(10,11,12),18]),
                               mean(Output.model$mean$psi[c(9),c(10,11,12),18]),mean(Output.model$mean$psi[c(10),c(10,11,12),18]),mean(Output.model$mean$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),18]),
                               mean(Output.model$mean$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),18]),mean(Output.model$mean$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),18]),mean(Output.model$mean$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),18]),
                               mean(Output.model$mean$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),18]),mean(Output.model$mean$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),18]),mean(Output.model$mean$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),18]),
                               mean(Output.model$mean$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),18]),mean(Output.model$mean$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),18]))),
                        mean(c(mean(Output.model$mean$psi[c(2),c(10,11,12),33]),mean(Output.model$mean$psi[c(3),c(10,11,12),33]),mean(Output.model$mean$psi[c(4),c(10,11,12),33]),
                               mean(Output.model$mean$psi[c(6),c(10,11,12),33]),mean(Output.model$mean$psi[c(7),c(10,11,12),33]),mean(Output.model$mean$psi[c(8),c(10,11,12),33]),
                               mean(Output.model$mean$psi[c(9),c(10,11,12),33]),mean(Output.model$mean$psi[c(10),c(10,11,12),33]),mean(Output.model$mean$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),33]),
                               mean(Output.model$mean$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),33]),mean(Output.model$mean$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),33]),mean(Output.model$mean$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),33]),
                               mean(Output.model$mean$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),33]),mean(Output.model$mean$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),33]),mean(Output.model$mean$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),33]),
                               mean(Output.model$mean$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),33]),mean(Output.model$mean$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),33]))),
                        mean(c(mean(Output.model$mean$psi[c(2),c(10,11,12),12]),mean(Output.model$mean$psi[c(3),c(10,11,12),12]),mean(Output.model$mean$psi[c(4),c(10,11,12),12]),
                               mean(Output.model$mean$psi[c(6),c(10,11,12),12]),mean(Output.model$mean$psi[c(7),c(10,11,12),12]),mean(Output.model$mean$psi[c(8),c(10,11,12),12]),
                               mean(Output.model$mean$psi[c(9),c(10,11,12),12]),mean(Output.model$mean$psi[c(10),c(10,11,12),12]),mean(Output.model$mean$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),12]),
                               mean(Output.model$mean$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),12]),mean(Output.model$mean$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),12]),mean(Output.model$mean$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),12]),
                               mean(Output.model$mean$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),12]),mean(Output.model$mean$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),12]),mean(Output.model$mean$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),12]),
                               mean(Output.model$mean$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),12]),mean(Output.model$mean$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),12]))),
                        mean(c(mean(Output.model$mean$psi[c(2),c(10,11,12),9]),mean(Output.model$mean$psi[c(3),c(10,11,12),9]),mean(Output.model$mean$psi[c(4),c(10,11,12),9]),
                               mean(Output.model$mean$psi[c(6),c(10,11,12),9]),mean(Output.model$mean$psi[c(7),c(10,11,12),9]),mean(Output.model$mean$psi[c(8),c(10,11,12),9]),
                               mean(Output.model$mean$psi[c(9),c(10,11,12),9]),mean(Output.model$mean$psi[c(10),c(10,11,12),9]),mean(Output.model$mean$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),9]),
                               mean(Output.model$mean$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),9]),mean(Output.model$mean$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),9]),mean(Output.model$mean$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),9]),
                               mean(Output.model$mean$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),9]),mean(Output.model$mean$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),9]),mean(Output.model$mean$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),9]),
                               mean(Output.model$mean$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),9]),mean(Output.model$mean$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),9]))),
                        mean(c(mean(Output.model$mean$psi[c(2),c(10,11,12),47]),mean(Output.model$mean$psi[c(3),c(10,11,12),47]),mean(Output.model$mean$psi[c(4),c(10,11,12),47]),
                               mean(Output.model$mean$psi[c(6),c(10,11,12),47]),mean(Output.model$mean$psi[c(7),c(10,11,12),47]),mean(Output.model$mean$psi[c(8),c(10,11,12),47]),
                               mean(Output.model$mean$psi[c(9),c(10,11,12),47]),mean(Output.model$mean$psi[c(10),c(10,11,12),47]),mean(Output.model$mean$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),47]),
                               mean(Output.model$mean$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),47]),mean(Output.model$mean$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),47]),mean(Output.model$mean$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),47]),
                               mean(Output.model$mean$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),47]),mean(Output.model$mean$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),47]),mean(Output.model$mean$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),47]),
                               mean(Output.model$mean$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),47]),mean(Output.model$mean$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),47]))),
                        mean(c(mean(Output.model$mean$psi[c(2),c(10,11,12),30]),mean(Output.model$mean$psi[c(3),c(10,11,12),30]),mean(Output.model$mean$psi[c(4),c(10,11,12),30]),
                               mean(Output.model$mean$psi[c(6),c(10,11,12),30]),mean(Output.model$mean$psi[c(7),c(10,11,12),30]),mean(Output.model$mean$psi[c(8),c(10,11,12),30]),
                               mean(Output.model$mean$psi[c(9),c(10,11,12),30]),mean(Output.model$mean$psi[c(10),c(10,11,12),30]),mean(Output.model$mean$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),30]),
                               mean(Output.model$mean$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),30]),mean(Output.model$mean$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),30]),mean(Output.model$mean$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),30]),
                               mean(Output.model$mean$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),30]),mean(Output.model$mean$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),30]),mean(Output.model$mean$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),30]),
                               mean(Output.model$mean$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),30]),mean(Output.model$mean$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),30]))),
                        mean(c(mean(Output.model$mean$psi[c(2),c(10,11,12),40]),mean(Output.model$mean$psi[c(3),c(10,11,12),40]),mean(Output.model$mean$psi[c(4),c(10,11,12),40]),
                               mean(Output.model$mean$psi[c(6),c(10,11,12),40]),mean(Output.model$mean$psi[c(7),c(10,11,12),40]),mean(Output.model$mean$psi[c(8),c(10,11,12),40]),
                               mean(Output.model$mean$psi[c(9),c(10,11,12),40]),mean(Output.model$mean$psi[c(10),c(10,11,12),40]),mean(Output.model$mean$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),40]),
                               mean(Output.model$mean$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),40]),mean(Output.model$mean$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),40]),mean(Output.model$mean$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),40]),
                               mean(Output.model$mean$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),40]),mean(Output.model$mean$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),40]),mean(Output.model$mean$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),40]),
                               mean(Output.model$mean$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),40]),mean(Output.model$mean$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),40]))),
                        mean(c(mean(Output.model$mean$psi[c(2),c(10,11,12),5]),mean(Output.model$mean$psi[c(3),c(10,11,12),5]),mean(Output.model$mean$psi[c(4),c(10,11,12),5]),
                               mean(Output.model$mean$psi[c(6),c(10,11,12),5]),mean(Output.model$mean$psi[c(7),c(10,11,12),5]),mean(Output.model$mean$psi[c(8),c(10,11,12),5]),
                               mean(Output.model$mean$psi[c(9),c(10,11,12),5]),mean(Output.model$mean$psi[c(10),c(10,11,12),5]),mean(Output.model$mean$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),5]),
                               mean(Output.model$mean$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),5]),mean(Output.model$mean$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),5]),mean(Output.model$mean$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),5]),
                               mean(Output.model$mean$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),5]),mean(Output.model$mean$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),5]),mean(Output.model$mean$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),5]),
                               mean(Output.model$mean$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),5]),mean(Output.model$mean$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),5]))),
                        mean(c(mean(Output.model$mean$psi[c(2),c(10,11,12),20]),mean(Output.model$mean$psi[c(3),c(10,11,12),20]),mean(Output.model$mean$psi[c(4),c(10,11,12),20]),
                               mean(Output.model$mean$psi[c(6),c(10,11,12),20]),mean(Output.model$mean$psi[c(7),c(10,11,12),20]),mean(Output.model$mean$psi[c(8),c(10,11,12),20]),
                               mean(Output.model$mean$psi[c(9),c(10,11,12),20]),mean(Output.model$mean$psi[c(10),c(10,11,12),20]),mean(Output.model$mean$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),20]),
                               mean(Output.model$mean$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),20]),mean(Output.model$mean$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),20]),mean(Output.model$mean$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),20]),
                               mean(Output.model$mean$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),20]),mean(Output.model$mean$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),20]),mean(Output.model$mean$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),20]),
                               mean(Output.model$mean$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),20]),mean(Output.model$mean$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),20]))),
                        mean(c(mean(Output.model$mean$psi[c(2),c(10,11,12),23]),mean(Output.model$mean$psi[c(3),c(10,11,12),23]),mean(Output.model$mean$psi[c(4),c(10,11,12),23]),
                               mean(Output.model$mean$psi[c(6),c(10,11,12),23]),mean(Output.model$mean$psi[c(7),c(10,11,12),23]),mean(Output.model$mean$psi[c(8),c(10,11,12),23]),
                               mean(Output.model$mean$psi[c(9),c(10,11,12),23]),mean(Output.model$mean$psi[c(10),c(10,11,12),23]),mean(Output.model$mean$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),23]),
                               mean(Output.model$mean$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),23]),mean(Output.model$mean$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),23]),mean(Output.model$mean$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),23]),
                               mean(Output.model$mean$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),23]),mean(Output.model$mean$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),23]),mean(Output.model$mean$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),23]),
                               mean(Output.model$mean$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),23]),mean(Output.model$mean$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),23]))),
                        mean(c(mean(Output.model$mean$psi[c(2),c(10,11,12),3]),mean(Output.model$mean$psi[c(3),c(10,11,12),3]),mean(Output.model$mean$psi[c(4),c(10,11,12),3]),
                               mean(Output.model$mean$psi[c(6),c(10,11,12),3]),mean(Output.model$mean$psi[c(7),c(10,11,12),3]),mean(Output.model$mean$psi[c(8),c(10,11,12),3]),
                               mean(Output.model$mean$psi[c(9),c(10,11,12),3]),mean(Output.model$mean$psi[c(10),c(10,11,12),3]),mean(Output.model$mean$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),3]),
                               mean(Output.model$mean$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),3]),mean(Output.model$mean$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),3]),mean(Output.model$mean$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),3]),
                               mean(Output.model$mean$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),3]),mean(Output.model$mean$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),3]),mean(Output.model$mean$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),3]),
                               mean(Output.model$mean$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),3]),mean(Output.model$mean$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),3]))),
                        mean(c(mean(Output.model$mean$psi[c(2),c(10,11,12),27]),mean(Output.model$mean$psi[c(3),c(10,11,12),27]),mean(Output.model$mean$psi[c(4),c(10,11,12),27]),
                               mean(Output.model$mean$psi[c(6),c(10,11,12),27]),mean(Output.model$mean$psi[c(7),c(10,11,12),27]),mean(Output.model$mean$psi[c(8),c(10,11,12),27]),
                               mean(Output.model$mean$psi[c(9),c(10,11,12),27]),mean(Output.model$mean$psi[c(10),c(10,11,12),27]),mean(Output.model$mean$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),27]),
                               mean(Output.model$mean$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),27]),mean(Output.model$mean$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),27]),mean(Output.model$mean$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),27]),
                               mean(Output.model$mean$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),27]),mean(Output.model$mean$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),27]),mean(Output.model$mean$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),27]),
                               mean(Output.model$mean$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),27]),mean(Output.model$mean$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),27]))),
                        mean(c(mean(Output.model$mean$psi[c(2),c(10,11,12),26]),mean(Output.model$mean$psi[c(3),c(10,11,12),26]),mean(Output.model$mean$psi[c(4),c(10,11,12),26]),
                               mean(Output.model$mean$psi[c(6),c(10,11,12),26]),mean(Output.model$mean$psi[c(7),c(10,11,12),26]),mean(Output.model$mean$psi[c(8),c(10,11,12),26]),
                               mean(Output.model$mean$psi[c(9),c(10,11,12),26]),mean(Output.model$mean$psi[c(10),c(10,11,12),26]),mean(Output.model$mean$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),26]),
                               mean(Output.model$mean$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),26]),mean(Output.model$mean$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),26]),mean(Output.model$mean$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),26]),
                               mean(Output.model$mean$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),26]),mean(Output.model$mean$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),26]),mean(Output.model$mean$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),26]),
                               mean(Output.model$mean$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),26]),mean(Output.model$mean$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),26]))),
                        mean(c(mean(Output.model$mean$psi[c(2),c(10,11,12),44]),mean(Output.model$mean$psi[c(3),c(10,11,12),44]),mean(Output.model$mean$psi[c(4),c(10,11,12),44]),
                               mean(Output.model$mean$psi[c(6),c(10,11,12),44]),mean(Output.model$mean$psi[c(7),c(10,11,12),44]),mean(Output.model$mean$psi[c(8),c(10,11,12),44]),
                               mean(Output.model$mean$psi[c(9),c(10,11,12),44]),mean(Output.model$mean$psi[c(10),c(10,11,12),44]),mean(Output.model$mean$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),44]),
                               mean(Output.model$mean$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),44]),mean(Output.model$mean$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),44]),mean(Output.model$mean$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),44]),
                               mean(Output.model$mean$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),44]),mean(Output.model$mean$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),44]),mean(Output.model$mean$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),44]),
                               mean(Output.model$mean$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),44]),mean(Output.model$mean$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),44]))),
                        mean(c(mean(Output.model$mean$psi[c(2),c(10,11,12),42]),mean(Output.model$mean$psi[c(3),c(10,11,12),42]),mean(Output.model$mean$psi[c(4),c(10,11,12),42]),
                               mean(Output.model$mean$psi[c(6),c(10,11,12),42]),mean(Output.model$mean$psi[c(7),c(10,11,12),42]),mean(Output.model$mean$psi[c(8),c(10,11,12),42]),
                               mean(Output.model$mean$psi[c(9),c(10,11,12),42]),mean(Output.model$mean$psi[c(10),c(10,11,12),42]),mean(Output.model$mean$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),42]),
                               mean(Output.model$mean$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),42]),mean(Output.model$mean$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),42]),mean(Output.model$mean$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),42]),
                               mean(Output.model$mean$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),42]),mean(Output.model$mean$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),42]),mean(Output.model$mean$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),42]),
                               mean(Output.model$mean$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),42]),mean(Output.model$mean$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),42]))),
                        mean(c(mean(Output.model$mean$psi[c(2),c(10,11,12),43]),mean(Output.model$mean$psi[c(3),c(10,11,12),43]),mean(Output.model$mean$psi[c(4),c(10,11,12),43]),
                               mean(Output.model$mean$psi[c(6),c(10,11,12),43]),mean(Output.model$mean$psi[c(7),c(10,11,12),43]),mean(Output.model$mean$psi[c(8),c(10,11,12),43]),
                               mean(Output.model$mean$psi[c(9),c(10,11,12),43]),mean(Output.model$mean$psi[c(10),c(10,11,12),43]),mean(Output.model$mean$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),43]),
                               mean(Output.model$mean$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),43]),mean(Output.model$mean$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),43]),mean(Output.model$mean$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),43]),
                               mean(Output.model$mean$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),43]),mean(Output.model$mean$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),43]),mean(Output.model$mean$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),43]),
                               mean(Output.model$mean$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),43]),mean(Output.model$mean$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),43]))),
                        mean(c(mean(Output.model$mean$psi[c(2),c(10,11,12),38]),mean(Output.model$mean$psi[c(3),c(10,11,12),38]),mean(Output.model$mean$psi[c(4),c(10,11,12),38]),
                               mean(Output.model$mean$psi[c(6),c(10,11,12),38]),mean(Output.model$mean$psi[c(7),c(10,11,12),38]),mean(Output.model$mean$psi[c(8),c(10,11,12),38]),
                               mean(Output.model$mean$psi[c(9),c(10,11,12),38]),mean(Output.model$mean$psi[c(10),c(10,11,12),38]),mean(Output.model$mean$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),38]),
                               mean(Output.model$mean$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),38]),mean(Output.model$mean$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),38]),mean(Output.model$mean$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),38]),
                               mean(Output.model$mean$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),38]),mean(Output.model$mean$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),38]),mean(Output.model$mean$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),38]),
                               mean(Output.model$mean$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),38]),mean(Output.model$mean$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),38]))),
                        mean(c(mean(Output.model$mean$psi[c(2),c(10,11,12),24]),mean(Output.model$mean$psi[c(3),c(10,11,12),24]),mean(Output.model$mean$psi[c(4),c(10,11,12),24]),
                               mean(Output.model$mean$psi[c(6),c(10,11,12),24]),mean(Output.model$mean$psi[c(7),c(10,11,12),24]),mean(Output.model$mean$psi[c(8),c(10,11,12),24]),
                               mean(Output.model$mean$psi[c(9),c(10,11,12),24]),mean(Output.model$mean$psi[c(10),c(10,11,12),24]),mean(Output.model$mean$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),24]),
                               mean(Output.model$mean$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),24]),mean(Output.model$mean$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),24]),mean(Output.model$mean$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),24]),
                               mean(Output.model$mean$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),24]),mean(Output.model$mean$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),24]),mean(Output.model$mean$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),24]),
                               mean(Output.model$mean$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),24]),mean(Output.model$mean$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),24]))),
                        mean(c(mean(Output.model$mean$psi[c(2),c(10,11,12),37]),mean(Output.model$mean$psi[c(3),c(10,11,12),37]),mean(Output.model$mean$psi[c(4),c(10,11,12),37]),
                               mean(Output.model$mean$psi[c(6),c(10,11,12),37]),mean(Output.model$mean$psi[c(7),c(10,11,12),37]),mean(Output.model$mean$psi[c(8),c(10,11,12),37]),
                               mean(Output.model$mean$psi[c(9),c(10,11,12),37]),mean(Output.model$mean$psi[c(10),c(10,11,12),37]),mean(Output.model$mean$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),37]),
                               mean(Output.model$mean$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),37]),mean(Output.model$mean$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),37]),mean(Output.model$mean$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),37]),
                               mean(Output.model$mean$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),37]),mean(Output.model$mean$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),37]),mean(Output.model$mean$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),37]),
                               mean(Output.model$mean$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),37]),mean(Output.model$mean$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),37]))),
                        mean(c(mean(Output.model$mean$psi[c(2),c(10,11,12),13]),mean(Output.model$mean$psi[c(3),c(10,11,12),13]),mean(Output.model$mean$psi[c(4),c(10,11,12),13]),
                               mean(Output.model$mean$psi[c(6),c(10,11,12),13]),mean(Output.model$mean$psi[c(7),c(10,11,12),13]),mean(Output.model$mean$psi[c(8),c(10,11,12),13]),
                               mean(Output.model$mean$psi[c(9),c(10,11,12),13]),mean(Output.model$mean$psi[c(10),c(10,11,12),13]),mean(Output.model$mean$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),13]),
                               mean(Output.model$mean$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),13]),mean(Output.model$mean$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),13]),mean(Output.model$mean$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),13]),
                               mean(Output.model$mean$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),13]),mean(Output.model$mean$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),13]),mean(Output.model$mean$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),13]),
                               mean(Output.model$mean$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),13]),mean(Output.model$mean$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),13]))),
                        mean(c(mean(Output.model$mean$psi[c(2),c(10,11,12),21]),mean(Output.model$mean$psi[c(3),c(10,11,12),21]),mean(Output.model$mean$psi[c(4),c(10,11,12),21]),
                               mean(Output.model$mean$psi[c(6),c(10,11,12),21]),mean(Output.model$mean$psi[c(7),c(10,11,12),21]),mean(Output.model$mean$psi[c(8),c(10,11,12),21]),
                               mean(Output.model$mean$psi[c(9),c(10,11,12),21]),mean(Output.model$mean$psi[c(10),c(10,11,12),21]),mean(Output.model$mean$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),21]),
                               mean(Output.model$mean$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),21]),mean(Output.model$mean$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),21]),mean(Output.model$mean$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),21]),
                               mean(Output.model$mean$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),21]),mean(Output.model$mean$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),21]),mean(Output.model$mean$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),21]),
                               mean(Output.model$mean$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),21]),mean(Output.model$mean$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),21]))),
                        mean(c(mean(Output.model$mean$psi[c(2),c(10,11,12),34]),mean(Output.model$mean$psi[c(3),c(10,11,12),34]),mean(Output.model$mean$psi[c(4),c(10,11,12),34]),
                               mean(Output.model$mean$psi[c(6),c(10,11,12),34]),mean(Output.model$mean$psi[c(7),c(10,11,12),34]),mean(Output.model$mean$psi[c(8),c(10,11,12),34]),
                               mean(Output.model$mean$psi[c(9),c(10,11,12),34]),mean(Output.model$mean$psi[c(10),c(10,11,12),34]),mean(Output.model$mean$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),34]),
                               mean(Output.model$mean$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),34]),mean(Output.model$mean$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),34]),mean(Output.model$mean$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),34]),
                               mean(Output.model$mean$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),34]),mean(Output.model$mean$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),34]),mean(Output.model$mean$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),34]),
                               mean(Output.model$mean$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),34]),mean(Output.model$mean$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),34]))),
                        mean(c(mean(Output.model$mean$psi[c(2),c(10,11,12),2]),mean(Output.model$mean$psi[c(3),c(10,11,12),2]),mean(Output.model$mean$psi[c(4),c(10,11,12),2]),
                               mean(Output.model$mean$psi[c(6),c(10,11,12),2]),mean(Output.model$mean$psi[c(7),c(10,11,12),2]),mean(Output.model$mean$psi[c(8),c(10,11,12),2]),
                               mean(Output.model$mean$psi[c(9),c(10,11,12),2]),mean(Output.model$mean$psi[c(10),c(10,11,12),2]),mean(Output.model$mean$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),2]),
                               mean(Output.model$mean$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),2]),mean(Output.model$mean$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),2]),mean(Output.model$mean$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),2]),
                               mean(Output.model$mean$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),2]),mean(Output.model$mean$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),2]),mean(Output.model$mean$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),2]),
                               mean(Output.model$mean$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),2]),mean(Output.model$mean$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),2]))),
                        mean(c(mean(Output.model$mean$psi[c(2),c(10,11,12),32]),mean(Output.model$mean$psi[c(3),c(10,11,12),32]),mean(Output.model$mean$psi[c(4),c(10,11,12),32]),
                               mean(Output.model$mean$psi[c(6),c(10,11,12),32]),mean(Output.model$mean$psi[c(7),c(10,11,12),32]),mean(Output.model$mean$psi[c(8),c(10,11,12),32]),
                               mean(Output.model$mean$psi[c(9),c(10,11,12),32]),mean(Output.model$mean$psi[c(10),c(10,11,12),32]),mean(Output.model$mean$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),32]),
                               mean(Output.model$mean$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),32]),mean(Output.model$mean$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),32]),mean(Output.model$mean$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),32]),
                               mean(Output.model$mean$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),32]),mean(Output.model$mean$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),32]),mean(Output.model$mean$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),32]),
                               mean(Output.model$mean$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),32]),mean(Output.model$mean$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),32]))),
                        mean(c(mean(Output.model$mean$psi[c(2),c(10,11,12),17]),mean(Output.model$mean$psi[c(3),c(10,11,12),17]),mean(Output.model$mean$psi[c(4),c(10,11,12),17]),
                               mean(Output.model$mean$psi[c(6),c(10,11,12),17]),mean(Output.model$mean$psi[c(7),c(10,11,12),17]),mean(Output.model$mean$psi[c(8),c(10,11,12),17]),
                               mean(Output.model$mean$psi[c(9),c(10,11,12),17]),mean(Output.model$mean$psi[c(10),c(10,11,12),17]),mean(Output.model$mean$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),17]),
                               mean(Output.model$mean$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),17]),mean(Output.model$mean$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),17]),mean(Output.model$mean$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),17]),
                               mean(Output.model$mean$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),17]),mean(Output.model$mean$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),17]),mean(Output.model$mean$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),17]),
                               mean(Output.model$mean$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),17]),mean(Output.model$mean$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),17]))),
                        mean(c(mean(Output.model$mean$psi[c(2),c(10,11,12),4]),mean(Output.model$mean$psi[c(3),c(10,11,12),4]),mean(Output.model$mean$psi[c(4),c(10,11,12),4]),
                               mean(Output.model$mean$psi[c(6),c(10,11,12),4]),mean(Output.model$mean$psi[c(7),c(10,11,12),4]),mean(Output.model$mean$psi[c(8),c(10,11,12),4]),
                               mean(Output.model$mean$psi[c(9),c(10,11,12),4]),mean(Output.model$mean$psi[c(10),c(10,11,12),4]),mean(Output.model$mean$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),4]),
                               mean(Output.model$mean$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),4]),mean(Output.model$mean$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),4]),mean(Output.model$mean$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),4]),
                               mean(Output.model$mean$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),4]),mean(Output.model$mean$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),4]),mean(Output.model$mean$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),4]),
                               mean(Output.model$mean$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),4]),mean(Output.model$mean$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),4]))),
                        mean(c(mean(Output.model$mean$psi[c(2),c(10,11,12),8]),mean(Output.model$mean$psi[c(3),c(10,11,12),8]),mean(Output.model$mean$psi[c(4),c(10,11,12),8]),
                               mean(Output.model$mean$psi[c(6),c(10,11,12),8]),mean(Output.model$mean$psi[c(7),c(10,11,12),8]),mean(Output.model$mean$psi[c(8),c(10,11,12),8]),
                               mean(Output.model$mean$psi[c(9),c(10,11,12),8]),mean(Output.model$mean$psi[c(10),c(10,11,12),8]),mean(Output.model$mean$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),8]),
                               mean(Output.model$mean$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),8]),mean(Output.model$mean$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),8]),mean(Output.model$mean$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),8]),
                               mean(Output.model$mean$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),8]),mean(Output.model$mean$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),8]),mean(Output.model$mean$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),8]),
                               mean(Output.model$mean$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),8]),mean(Output.model$mean$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),8]))),
                        mean(c(mean(Output.model$mean$psi[c(2),c(10,11,12),39]),mean(Output.model$mean$psi[c(3),c(10,11,12),39]),mean(Output.model$mean$psi[c(4),c(10,11,12),39]),
                               mean(Output.model$mean$psi[c(6),c(10,11,12),39]),mean(Output.model$mean$psi[c(7),c(10,11,12),39]),mean(Output.model$mean$psi[c(8),c(10,11,12),39]),
                               mean(Output.model$mean$psi[c(9),c(10,11,12),39]),mean(Output.model$mean$psi[c(10),c(10,11,12),39]),mean(Output.model$mean$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),39]),
                               mean(Output.model$mean$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),39]),mean(Output.model$mean$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),39]),mean(Output.model$mean$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),39]),
                               mean(Output.model$mean$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),39]),mean(Output.model$mean$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),39]),mean(Output.model$mean$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),39]),
                               mean(Output.model$mean$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),39]),mean(Output.model$mean$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),39]))),
                        mean(c(mean(Output.model$mean$psi[c(2),c(10,11,12),10]),mean(Output.model$mean$psi[c(3),c(10,11,12),10]),mean(Output.model$mean$psi[c(4),c(10,11,12),10]),
                               mean(Output.model$mean$psi[c(6),c(10,11,12),10]),mean(Output.model$mean$psi[c(7),c(10,11,12),10]),mean(Output.model$mean$psi[c(8),c(10,11,12),10]),
                               mean(Output.model$mean$psi[c(9),c(10,11,12),10]),mean(Output.model$mean$psi[c(10),c(10,11,12),10]),mean(Output.model$mean$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),10]),
                               mean(Output.model$mean$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),10]),mean(Output.model$mean$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),10]),mean(Output.model$mean$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),10]),
                               mean(Output.model$mean$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),10]),mean(Output.model$mean$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),10]),mean(Output.model$mean$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),10]),
                               mean(Output.model$mean$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),10]),mean(Output.model$mean$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),10]))),
                        mean(c(mean(Output.model$mean$psi[c(2),c(10,11,12),19]),mean(Output.model$mean$psi[c(3),c(10,11,12),19]),mean(Output.model$mean$psi[c(4),c(10,11,12),19]),
                               mean(Output.model$mean$psi[c(6),c(10,11,12),19]),mean(Output.model$mean$psi[c(7),c(10,11,12),19]),mean(Output.model$mean$psi[c(8),c(10,11,12),19]),
                               mean(Output.model$mean$psi[c(9),c(10,11,12),19]),mean(Output.model$mean$psi[c(10),c(10,11,12),19]),mean(Output.model$mean$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),19]),
                               mean(Output.model$mean$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),19]),mean(Output.model$mean$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),19]),mean(Output.model$mean$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),19]),
                               mean(Output.model$mean$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),19]),mean(Output.model$mean$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),19]),mean(Output.model$mean$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),19]),
                               mean(Output.model$mean$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),19]),mean(Output.model$mean$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),19]))),
                        mean(c(mean(Output.model$mean$psi[c(2),c(10,11,12),35]),mean(Output.model$mean$psi[c(3),c(10,11,12),35]),mean(Output.model$mean$psi[c(4),c(10,11,12),35]),
                               mean(Output.model$mean$psi[c(6),c(10,11,12),35]),mean(Output.model$mean$psi[c(7),c(10,11,12),35]),mean(Output.model$mean$psi[c(8),c(10,11,12),35]),
                               mean(Output.model$mean$psi[c(9),c(10,11,12),35]),mean(Output.model$mean$psi[c(10),c(10,11,12),35]),mean(Output.model$mean$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),35]),
                               mean(Output.model$mean$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),35]),mean(Output.model$mean$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),35]),mean(Output.model$mean$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),35]),
                               mean(Output.model$mean$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),35]),mean(Output.model$mean$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),35]),mean(Output.model$mean$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),35]),
                               mean(Output.model$mean$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),35]),mean(Output.model$mean$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),35]))),
                        mean(c(mean(Output.model$mean$psi[c(2),c(10,11,12),25]),mean(Output.model$mean$psi[c(3),c(10,11,12),25]),mean(Output.model$mean$psi[c(4),c(10,11,12),25]),
                               mean(Output.model$mean$psi[c(6),c(10,11,12),25]),mean(Output.model$mean$psi[c(7),c(10,11,12),25]),mean(Output.model$mean$psi[c(8),c(10,11,12),25]),
                               mean(Output.model$mean$psi[c(9),c(10,11,12),25]),mean(Output.model$mean$psi[c(10),c(10,11,12),25]),mean(Output.model$mean$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),25]),
                               mean(Output.model$mean$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),25]),mean(Output.model$mean$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),25]),mean(Output.model$mean$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),25]),
                               mean(Output.model$mean$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),25]),mean(Output.model$mean$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),25]),mean(Output.model$mean$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),25]),
                               mean(Output.model$mean$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),25]),mean(Output.model$mean$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),25]))),
                        mean(c(mean(Output.model$mean$psi[c(2),c(10,11,12),28]),mean(Output.model$mean$psi[c(3),c(10,11,12),28]),mean(Output.model$mean$psi[c(4),c(10,11,12),28]),
                               mean(Output.model$mean$psi[c(6),c(10,11,12),28]),mean(Output.model$mean$psi[c(7),c(10,11,12),28]),mean(Output.model$mean$psi[c(8),c(10,11,12),28]),
                               mean(Output.model$mean$psi[c(9),c(10,11,12),28]),mean(Output.model$mean$psi[c(10),c(10,11,12),28]),mean(Output.model$mean$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),28]),
                               mean(Output.model$mean$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),28]),mean(Output.model$mean$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),28]),mean(Output.model$mean$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),28]),
                               mean(Output.model$mean$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),28]),mean(Output.model$mean$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),28]),mean(Output.model$mean$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),28]),
                               mean(Output.model$mean$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),28]),mean(Output.model$mean$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),28]))),
                        mean(c(mean(Output.model$mean$psi[c(2),c(10,11,12),46]),mean(Output.model$mean$psi[c(3),c(10,11,12),46]),mean(Output.model$mean$psi[c(4),c(10,11,12),46]),
                               mean(Output.model$mean$psi[c(6),c(10,11,12),46]),mean(Output.model$mean$psi[c(7),c(10,11,12),46]),mean(Output.model$mean$psi[c(8),c(10,11,12),46]),
                               mean(Output.model$mean$psi[c(9),c(10,11,12),46]),mean(Output.model$mean$psi[c(10),c(10,11,12),46]),mean(Output.model$mean$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),46]),
                               mean(Output.model$mean$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),46]),mean(Output.model$mean$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),46]),mean(Output.model$mean$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),46]),
                               mean(Output.model$mean$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),46]),mean(Output.model$mean$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),46]),mean(Output.model$mean$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),46]),
                               mean(Output.model$mean$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),46]),mean(Output.model$mean$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),46]))),
                        mean(c(mean(Output.model$mean$psi[c(2),c(10,11,12),41]),mean(Output.model$mean$psi[c(3),c(10,11,12),41]),mean(Output.model$mean$psi[c(4),c(10,11,12),41]),
                               mean(Output.model$mean$psi[c(6),c(10,11,12),41]),mean(Output.model$mean$psi[c(7),c(10,11,12),41]),mean(Output.model$mean$psi[c(8),c(10,11,12),41]),
                               mean(Output.model$mean$psi[c(9),c(10,11,12),41]),mean(Output.model$mean$psi[c(10),c(10,11,12),41]),mean(Output.model$mean$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),41]),
                               mean(Output.model$mean$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),41]),mean(Output.model$mean$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),41]),mean(Output.model$mean$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),41]),
                               mean(Output.model$mean$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),41]),mean(Output.model$mean$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),41]),mean(Output.model$mean$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),41]),
                               mean(Output.model$mean$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),41]),mean(Output.model$mean$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),41]))),
                        mean(c(mean(Output.model$mean$psi[c(2),c(10,11,12),16]),mean(Output.model$mean$psi[c(3),c(10,11,12),16]),mean(Output.model$mean$psi[c(4),c(10,11,12),16]),
                               mean(Output.model$mean$psi[c(6),c(10,11,12),16]),mean(Output.model$mean$psi[c(7),c(10,11,12),16]),mean(Output.model$mean$psi[c(8),c(10,11,12),16]),
                               mean(Output.model$mean$psi[c(9),c(10,11,12),16]),mean(Output.model$mean$psi[c(10),c(10,11,12),16]),mean(Output.model$mean$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),16]),
                               mean(Output.model$mean$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),16]),mean(Output.model$mean$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),16]),mean(Output.model$mean$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),16]),
                               mean(Output.model$mean$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),16]),mean(Output.model$mean$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),16]),mean(Output.model$mean$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),16]),
                               mean(Output.model$mean$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),16]),mean(Output.model$mean$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),16]))),
                        mean(c(mean(Output.model$mean$psi[c(2),c(10,11,12),36]),mean(Output.model$mean$psi[c(3),c(10,11,12),36]),mean(Output.model$mean$psi[c(4),c(10,11,12),36]),
                               mean(Output.model$mean$psi[c(6),c(10,11,12),36]),mean(Output.model$mean$psi[c(7),c(10,11,12),36]),mean(Output.model$mean$psi[c(8),c(10,11,12),36]),
                               mean(Output.model$mean$psi[c(9),c(10,11,12),36]),mean(Output.model$mean$psi[c(10),c(10,11,12),36]),mean(Output.model$mean$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),36]),
                               mean(Output.model$mean$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),36]),mean(Output.model$mean$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),36]),mean(Output.model$mean$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),36]),
                               mean(Output.model$mean$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),36]),mean(Output.model$mean$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),36]),mean(Output.model$mean$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),36]),
                               mean(Output.model$mean$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),36]),mean(Output.model$mean$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),36]))),
                        mean(c(mean(Output.model$mean$psi[c(2),c(10,11,12),14]),mean(Output.model$mean$psi[c(3),c(10,11,12),14]),mean(Output.model$mean$psi[c(4),c(10,11,12),14]),
                               mean(Output.model$mean$psi[c(6),c(10,11,12),14]),mean(Output.model$mean$psi[c(7),c(10,11,12),14]),mean(Output.model$mean$psi[c(8),c(10,11,12),14]),
                               mean(Output.model$mean$psi[c(9),c(10,11,12),14]),mean(Output.model$mean$psi[c(10),c(10,11,12),14]),mean(Output.model$mean$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),14]),
                               mean(Output.model$mean$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),14]),mean(Output.model$mean$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),14]),mean(Output.model$mean$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),14]),
                               mean(Output.model$mean$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),14]),mean(Output.model$mean$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),14]),mean(Output.model$mean$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),14]),
                               mean(Output.model$mean$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),14]),mean(Output.model$mean$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),14]))),
                        mean(c(mean(Output.model$mean$psi[c(2),c(10,11,12),11]),mean(Output.model$mean$psi[c(3),c(10,11,12),11]),mean(Output.model$mean$psi[c(4),c(10,11,12),11]),
                               mean(Output.model$mean$psi[c(6),c(10,11,12),11]),mean(Output.model$mean$psi[c(7),c(10,11,12),11]),mean(Output.model$mean$psi[c(8),c(10,11,12),11]),
                               mean(Output.model$mean$psi[c(9),c(10,11,12),11]),mean(Output.model$mean$psi[c(10),c(10,11,12),11]),mean(Output.model$mean$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),11]),
                               mean(Output.model$mean$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),11]),mean(Output.model$mean$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),11]),mean(Output.model$mean$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),11]),
                               mean(Output.model$mean$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),11]),mean(Output.model$mean$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),11]),mean(Output.model$mean$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),11]),
                               mean(Output.model$mean$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),11]),mean(Output.model$mean$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),11]))),
                        mean(c(mean(Output.model$mean$psi[c(2),c(10,11,12),31]),mean(Output.model$mean$psi[c(3),c(10,11,12),31]),mean(Output.model$mean$psi[c(4),c(10,11,12),31]),
                               mean(Output.model$mean$psi[c(6),c(10,11,12),31]),mean(Output.model$mean$psi[c(7),c(10,11,12),31]),mean(Output.model$mean$psi[c(8),c(10,11,12),31]),
                               mean(Output.model$mean$psi[c(9),c(10,11,12),31]),mean(Output.model$mean$psi[c(10),c(10,11,12),31]),mean(Output.model$mean$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),31]),
                               mean(Output.model$mean$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),31]),mean(Output.model$mean$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),31]),mean(Output.model$mean$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),31]),
                               mean(Output.model$mean$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),31]),mean(Output.model$mean$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),31]),mean(Output.model$mean$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),31]),
                               mean(Output.model$mean$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),31]),mean(Output.model$mean$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),31]))),
                        mean(c(mean(Output.model$mean$psi[c(2),c(10,11,12),6]),mean(Output.model$mean$psi[c(3),c(10,11,12),6]),mean(Output.model$mean$psi[c(4),c(10,11,12),6]),
                               mean(Output.model$mean$psi[c(6),c(10,11,12),6]),mean(Output.model$mean$psi[c(7),c(10,11,12),6]),mean(Output.model$mean$psi[c(8),c(10,11,12),6]),
                               mean(Output.model$mean$psi[c(9),c(10,11,12),6]),mean(Output.model$mean$psi[c(10),c(10,11,12),6]),mean(Output.model$mean$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),6]),
                               mean(Output.model$mean$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),6]),mean(Output.model$mean$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),6]),mean(Output.model$mean$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),6]),
                               mean(Output.model$mean$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),6]),mean(Output.model$mean$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),6]),mean(Output.model$mean$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),6]),
                               mean(Output.model$mean$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),6]),mean(Output.model$mean$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),6])))),
                      c(mean(c(mean(Output.model$q75$psi[c(2),c(10,11,12),45]),mean(Output.model$q75$psi[c(3),c(10,11,12),45]),mean(Output.model$q75$psi[c(4),c(10,11,12),45]),
                               mean(Output.model$q75$psi[c(6),c(10,11,12),45]),mean(Output.model$q75$psi[c(7),c(10,11,12),45]),mean(Output.model$q75$psi[c(8),c(10,11,12),45]),
                               mean(Output.model$q75$psi[c(9),c(10,11,12),45]),mean(Output.model$q75$psi[c(10),c(10,11,12),45]),mean(Output.model$q75$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),45]),
                               mean(Output.model$q75$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),45]),mean(Output.model$q75$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),45]),mean(Output.model$q75$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),45]),
                               mean(Output.model$q75$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),45]),mean(Output.model$q75$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),45]),mean(Output.model$q75$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),45]),
                               mean(Output.model$q75$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),45]),mean(Output.model$q75$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),45]))),
                        mean(c(mean(Output.model$q75$psi[c(2),c(10,11,12),1]),mean(Output.model$q75$psi[c(3),c(10,11,12),1]),mean(Output.model$q75$psi[c(4),c(10,11,12),1]),
                               mean(Output.model$q75$psi[c(6),c(10,11,12),1]),mean(Output.model$q75$psi[c(7),c(10,11,12),1]),mean(Output.model$q75$psi[c(8),c(10,11,12),1]),
                               mean(Output.model$q75$psi[c(9),c(10,11,12),1]),mean(Output.model$q75$psi[c(10),c(10,11,12),1]),mean(Output.model$q75$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),1]),
                               mean(Output.model$q75$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),1]),mean(Output.model$q75$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),1]),mean(Output.model$q75$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),1]),
                               mean(Output.model$q75$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),1]),mean(Output.model$q75$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),1]),mean(Output.model$q75$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),1]),
                               mean(Output.model$q75$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),1]),mean(Output.model$q75$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),1]))),
                        mean(c(mean(Output.model$q75$psi[c(2),c(10,11,12),15]),mean(Output.model$q75$psi[c(3),c(10,11,12),15]),mean(Output.model$q75$psi[c(4),c(10,11,12),15]),
                               mean(Output.model$q75$psi[c(6),c(10,11,12),15]),mean(Output.model$q75$psi[c(7),c(10,11,12),15]),mean(Output.model$q75$psi[c(8),c(10,11,12),15]),
                               mean(Output.model$q75$psi[c(9),c(10,11,12),15]),mean(Output.model$q75$psi[c(10),c(10,11,12),15]),mean(Output.model$q75$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),15]),
                               mean(Output.model$q75$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),15]),mean(Output.model$q75$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),15]),mean(Output.model$q75$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),15]),
                               mean(Output.model$q75$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),15]),mean(Output.model$q75$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),15]),mean(Output.model$q75$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),15]),
                               mean(Output.model$q75$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),15]),mean(Output.model$q75$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),15]))),
                        mean(c(mean(Output.model$q75$psi[c(2),c(10,11,12),7]),mean(Output.model$q75$psi[c(3),c(10,11,12),7]),mean(Output.model$q75$psi[c(4),c(10,11,12),7]),
                               mean(Output.model$q75$psi[c(6),c(10,11,12),7]),mean(Output.model$q75$psi[c(7),c(10,11,12),7]),mean(Output.model$q75$psi[c(8),c(10,11,12),7]),
                               mean(Output.model$q75$psi[c(9),c(10,11,12),7]),mean(Output.model$q75$psi[c(10),c(10,11,12),7]),mean(Output.model$q75$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),7]),
                               mean(Output.model$q75$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),7]),mean(Output.model$q75$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),7]),mean(Output.model$q75$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),7]),
                               mean(Output.model$q75$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),7]),mean(Output.model$q75$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),7]),mean(Output.model$q75$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),7]),
                               mean(Output.model$q75$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),7]),mean(Output.model$q75$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),7]))),
                        mean(c(mean(Output.model$q75$psi[c(2),c(10,11,12),22]),mean(Output.model$q75$psi[c(3),c(10,11,12),22]),mean(Output.model$q75$psi[c(4),c(10,11,12),22]),
                               mean(Output.model$q75$psi[c(6),c(10,11,12),22]),mean(Output.model$q75$psi[c(7),c(10,11,12),22]),mean(Output.model$q75$psi[c(8),c(10,11,12),22]),
                               mean(Output.model$q75$psi[c(9),c(10,11,12),22]),mean(Output.model$q75$psi[c(10),c(10,11,12),22]),mean(Output.model$q75$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),22]),
                               mean(Output.model$q75$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),22]),mean(Output.model$q75$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),22]),mean(Output.model$q75$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),22]),
                               mean(Output.model$q75$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),22]),mean(Output.model$q75$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),22]),mean(Output.model$q75$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),22]),
                               mean(Output.model$q75$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),22]),mean(Output.model$q75$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),22]))),
                        mean(c(mean(Output.model$q75$psi[c(2),c(10,11,12),29]),mean(Output.model$q75$psi[c(3),c(10,11,12),29]),mean(Output.model$q75$psi[c(4),c(10,11,12),29]),
                               mean(Output.model$q75$psi[c(6),c(10,11,12),29]),mean(Output.model$q75$psi[c(7),c(10,11,12),29]),mean(Output.model$q75$psi[c(8),c(10,11,12),29]),
                               mean(Output.model$q75$psi[c(9),c(10,11,12),29]),mean(Output.model$q75$psi[c(10),c(10,11,12),29]),mean(Output.model$q75$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),29]),
                               mean(Output.model$q75$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),29]),mean(Output.model$q75$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),29]),mean(Output.model$q75$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),29]),
                               mean(Output.model$q75$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),29]),mean(Output.model$q75$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),29]),mean(Output.model$q75$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),29]),
                               mean(Output.model$q75$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),29]),mean(Output.model$q75$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),29]))),
                        mean(c(mean(Output.model$q75$psi[c(2),c(10,11,12),18]),mean(Output.model$q75$psi[c(3),c(10,11,12),18]),mean(Output.model$q75$psi[c(4),c(10,11,12),18]),
                               mean(Output.model$q75$psi[c(6),c(10,11,12),18]),mean(Output.model$q75$psi[c(7),c(10,11,12),18]),mean(Output.model$q75$psi[c(8),c(10,11,12),18]),
                               mean(Output.model$q75$psi[c(9),c(10,11,12),18]),mean(Output.model$q75$psi[c(10),c(10,11,12),18]),mean(Output.model$q75$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),18]),
                               mean(Output.model$q75$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),18]),mean(Output.model$q75$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),18]),mean(Output.model$q75$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),18]),
                               mean(Output.model$q75$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),18]),mean(Output.model$q75$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),18]),mean(Output.model$q75$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),18]),
                               mean(Output.model$q75$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),18]),mean(Output.model$q75$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),18]))),
                        mean(c(mean(Output.model$q75$psi[c(2),c(10,11,12),33]),mean(Output.model$q75$psi[c(3),c(10,11,12),33]),mean(Output.model$q75$psi[c(4),c(10,11,12),33]),
                               mean(Output.model$q75$psi[c(6),c(10,11,12),33]),mean(Output.model$q75$psi[c(7),c(10,11,12),33]),mean(Output.model$q75$psi[c(8),c(10,11,12),33]),
                               mean(Output.model$q75$psi[c(9),c(10,11,12),33]),mean(Output.model$q75$psi[c(10),c(10,11,12),33]),mean(Output.model$q75$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),33]),
                               mean(Output.model$q75$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),33]),mean(Output.model$q75$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),33]),mean(Output.model$q75$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),33]),
                               mean(Output.model$q75$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),33]),mean(Output.model$q75$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),33]),mean(Output.model$q75$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),33]),
                               mean(Output.model$q75$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),33]),mean(Output.model$q75$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),33]))),
                        mean(c(mean(Output.model$q75$psi[c(2),c(10,11,12),12]),mean(Output.model$q75$psi[c(3),c(10,11,12),12]),mean(Output.model$q75$psi[c(4),c(10,11,12),12]),
                               mean(Output.model$q75$psi[c(6),c(10,11,12),12]),mean(Output.model$q75$psi[c(7),c(10,11,12),12]),mean(Output.model$q75$psi[c(8),c(10,11,12),12]),
                               mean(Output.model$q75$psi[c(9),c(10,11,12),12]),mean(Output.model$q75$psi[c(10),c(10,11,12),12]),mean(Output.model$q75$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),12]),
                               mean(Output.model$q75$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),12]),mean(Output.model$q75$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),12]),mean(Output.model$q75$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),12]),
                               mean(Output.model$q75$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),12]),mean(Output.model$q75$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),12]),mean(Output.model$q75$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),12]),
                               mean(Output.model$q75$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),12]),mean(Output.model$q75$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),12]))),
                        mean(c(mean(Output.model$q75$psi[c(2),c(10,11,12),9]),mean(Output.model$q75$psi[c(3),c(10,11,12),9]),mean(Output.model$q75$psi[c(4),c(10,11,12),9]),
                               mean(Output.model$q75$psi[c(6),c(10,11,12),9]),mean(Output.model$q75$psi[c(7),c(10,11,12),9]),mean(Output.model$q75$psi[c(8),c(10,11,12),9]),
                               mean(Output.model$q75$psi[c(9),c(10,11,12),9]),mean(Output.model$q75$psi[c(10),c(10,11,12),9]),mean(Output.model$q75$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),9]),
                               mean(Output.model$q75$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),9]),mean(Output.model$q75$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),9]),mean(Output.model$q75$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),9]),
                               mean(Output.model$q75$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),9]),mean(Output.model$q75$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),9]),mean(Output.model$q75$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),9]),
                               mean(Output.model$q75$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),9]),mean(Output.model$q75$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),9]))),
                        mean(c(mean(Output.model$q75$psi[c(2),c(10,11,12),47]),mean(Output.model$q75$psi[c(3),c(10,11,12),47]),mean(Output.model$q75$psi[c(4),c(10,11,12),47]),
                               mean(Output.model$q75$psi[c(6),c(10,11,12),47]),mean(Output.model$q75$psi[c(7),c(10,11,12),47]),mean(Output.model$q75$psi[c(8),c(10,11,12),47]),
                               mean(Output.model$q75$psi[c(9),c(10,11,12),47]),mean(Output.model$q75$psi[c(10),c(10,11,12),47]),mean(Output.model$q75$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),47]),
                               mean(Output.model$q75$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),47]),mean(Output.model$q75$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),47]),mean(Output.model$q75$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),47]),
                               mean(Output.model$q75$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),47]),mean(Output.model$q75$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),47]),mean(Output.model$q75$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),47]),
                               mean(Output.model$q75$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),47]),mean(Output.model$q75$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),47]))),
                        mean(c(mean(Output.model$q75$psi[c(2),c(10,11,12),30]),mean(Output.model$q75$psi[c(3),c(10,11,12),30]),mean(Output.model$q75$psi[c(4),c(10,11,12),30]),
                               mean(Output.model$q75$psi[c(6),c(10,11,12),30]),mean(Output.model$q75$psi[c(7),c(10,11,12),30]),mean(Output.model$q75$psi[c(8),c(10,11,12),30]),
                               mean(Output.model$q75$psi[c(9),c(10,11,12),30]),mean(Output.model$q75$psi[c(10),c(10,11,12),30]),mean(Output.model$q75$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),30]),
                               mean(Output.model$q75$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),30]),mean(Output.model$q75$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),30]),mean(Output.model$q75$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),30]),
                               mean(Output.model$q75$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),30]),mean(Output.model$q75$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),30]),mean(Output.model$q75$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),30]),
                               mean(Output.model$q75$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),30]),mean(Output.model$q75$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),30]))),
                        mean(c(mean(Output.model$q75$psi[c(2),c(10,11,12),40]),mean(Output.model$q75$psi[c(3),c(10,11,12),40]),mean(Output.model$q75$psi[c(4),c(10,11,12),40]),
                               mean(Output.model$q75$psi[c(6),c(10,11,12),40]),mean(Output.model$q75$psi[c(7),c(10,11,12),40]),mean(Output.model$q75$psi[c(8),c(10,11,12),40]),
                               mean(Output.model$q75$psi[c(9),c(10,11,12),40]),mean(Output.model$q75$psi[c(10),c(10,11,12),40]),mean(Output.model$q75$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),40]),
                               mean(Output.model$q75$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),40]),mean(Output.model$q75$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),40]),mean(Output.model$q75$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),40]),
                               mean(Output.model$q75$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),40]),mean(Output.model$q75$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),40]),mean(Output.model$q75$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),40]),
                               mean(Output.model$q75$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),40]),mean(Output.model$q75$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),40]))),
                        mean(c(mean(Output.model$q75$psi[c(2),c(10,11,12),5]),mean(Output.model$q75$psi[c(3),c(10,11,12),5]),mean(Output.model$q75$psi[c(4),c(10,11,12),5]),
                               mean(Output.model$q75$psi[c(6),c(10,11,12),5]),mean(Output.model$q75$psi[c(7),c(10,11,12),5]),mean(Output.model$q75$psi[c(8),c(10,11,12),5]),
                               mean(Output.model$q75$psi[c(9),c(10,11,12),5]),mean(Output.model$q75$psi[c(10),c(10,11,12),5]),mean(Output.model$q75$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),5]),
                               mean(Output.model$q75$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),5]),mean(Output.model$q75$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),5]),mean(Output.model$q75$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),5]),
                               mean(Output.model$q75$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),5]),mean(Output.model$q75$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),5]),mean(Output.model$q75$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),5]),
                               mean(Output.model$q75$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),5]),mean(Output.model$q75$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),5]))),
                        mean(c(mean(Output.model$q75$psi[c(2),c(10,11,12),20]),mean(Output.model$q75$psi[c(3),c(10,11,12),20]),mean(Output.model$q75$psi[c(4),c(10,11,12),20]),
                               mean(Output.model$q75$psi[c(6),c(10,11,12),20]),mean(Output.model$q75$psi[c(7),c(10,11,12),20]),mean(Output.model$q75$psi[c(8),c(10,11,12),20]),
                               mean(Output.model$q75$psi[c(9),c(10,11,12),20]),mean(Output.model$q75$psi[c(10),c(10,11,12),20]),mean(Output.model$q75$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),20]),
                               mean(Output.model$q75$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),20]),mean(Output.model$q75$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),20]),mean(Output.model$q75$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),20]),
                               mean(Output.model$q75$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),20]),mean(Output.model$q75$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),20]),mean(Output.model$q75$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),20]),
                               mean(Output.model$q75$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),20]),mean(Output.model$q75$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),20]))),
                        mean(c(mean(Output.model$q75$psi[c(2),c(10,11,12),23]),mean(Output.model$q75$psi[c(3),c(10,11,12),23]),mean(Output.model$q75$psi[c(4),c(10,11,12),23]),
                               mean(Output.model$q75$psi[c(6),c(10,11,12),23]),mean(Output.model$q75$psi[c(7),c(10,11,12),23]),mean(Output.model$q75$psi[c(8),c(10,11,12),23]),
                               mean(Output.model$q75$psi[c(9),c(10,11,12),23]),mean(Output.model$q75$psi[c(10),c(10,11,12),23]),mean(Output.model$q75$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),23]),
                               mean(Output.model$q75$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),23]),mean(Output.model$q75$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),23]),mean(Output.model$q75$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),23]),
                               mean(Output.model$q75$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),23]),mean(Output.model$q75$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),23]),mean(Output.model$q75$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),23]),
                               mean(Output.model$q75$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),23]),mean(Output.model$q75$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),23]))),
                        mean(c(mean(Output.model$q75$psi[c(2),c(10,11,12),3]),mean(Output.model$q75$psi[c(3),c(10,11,12),3]),mean(Output.model$q75$psi[c(4),c(10,11,12),3]),
                               mean(Output.model$q75$psi[c(6),c(10,11,12),3]),mean(Output.model$q75$psi[c(7),c(10,11,12),3]),mean(Output.model$q75$psi[c(8),c(10,11,12),3]),
                               mean(Output.model$q75$psi[c(9),c(10,11,12),3]),mean(Output.model$q75$psi[c(10),c(10,11,12),3]),mean(Output.model$q75$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),3]),
                               mean(Output.model$q75$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),3]),mean(Output.model$q75$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),3]),mean(Output.model$q75$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),3]),
                               mean(Output.model$q75$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),3]),mean(Output.model$q75$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),3]),mean(Output.model$q75$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),3]),
                               mean(Output.model$q75$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),3]),mean(Output.model$q75$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),3]))),
                        mean(c(mean(Output.model$q75$psi[c(2),c(10,11,12),27]),mean(Output.model$q75$psi[c(3),c(10,11,12),27]),mean(Output.model$q75$psi[c(4),c(10,11,12),27]),
                               mean(Output.model$q75$psi[c(6),c(10,11,12),27]),mean(Output.model$q75$psi[c(7),c(10,11,12),27]),mean(Output.model$q75$psi[c(8),c(10,11,12),27]),
                               mean(Output.model$q75$psi[c(9),c(10,11,12),27]),mean(Output.model$q75$psi[c(10),c(10,11,12),27]),mean(Output.model$q75$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),27]),
                               mean(Output.model$q75$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),27]),mean(Output.model$q75$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),27]),mean(Output.model$q75$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),27]),
                               mean(Output.model$q75$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),27]),mean(Output.model$q75$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),27]),mean(Output.model$q75$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),27]),
                               mean(Output.model$q75$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),27]),mean(Output.model$q75$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),27]))),
                        mean(c(mean(Output.model$q75$psi[c(2),c(10,11,12),26]),mean(Output.model$q75$psi[c(3),c(10,11,12),26]),mean(Output.model$q75$psi[c(4),c(10,11,12),26]),
                               mean(Output.model$q75$psi[c(6),c(10,11,12),26]),mean(Output.model$q75$psi[c(7),c(10,11,12),26]),mean(Output.model$q75$psi[c(8),c(10,11,12),26]),
                               mean(Output.model$q75$psi[c(9),c(10,11,12),26]),mean(Output.model$q75$psi[c(10),c(10,11,12),26]),mean(Output.model$q75$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),26]),
                               mean(Output.model$q75$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),26]),mean(Output.model$q75$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),26]),mean(Output.model$q75$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),26]),
                               mean(Output.model$q75$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),26]),mean(Output.model$q75$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),26]),mean(Output.model$q75$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),26]),
                               mean(Output.model$q75$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),26]),mean(Output.model$q75$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),26]))),
                        mean(c(mean(Output.model$q75$psi[c(2),c(10,11,12),44]),mean(Output.model$q75$psi[c(3),c(10,11,12),44]),mean(Output.model$q75$psi[c(4),c(10,11,12),44]),
                               mean(Output.model$q75$psi[c(6),c(10,11,12),44]),mean(Output.model$q75$psi[c(7),c(10,11,12),44]),mean(Output.model$q75$psi[c(8),c(10,11,12),44]),
                               mean(Output.model$q75$psi[c(9),c(10,11,12),44]),mean(Output.model$q75$psi[c(10),c(10,11,12),44]),mean(Output.model$q75$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),44]),
                               mean(Output.model$q75$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),44]),mean(Output.model$q75$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),44]),mean(Output.model$q75$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),44]),
                               mean(Output.model$q75$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),44]),mean(Output.model$q75$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),44]),mean(Output.model$q75$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),44]),
                               mean(Output.model$q75$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),44]),mean(Output.model$q75$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),44]))),
                        mean(c(mean(Output.model$q75$psi[c(2),c(10,11,12),42]),mean(Output.model$q75$psi[c(3),c(10,11,12),42]),mean(Output.model$q75$psi[c(4),c(10,11,12),42]),
                               mean(Output.model$q75$psi[c(6),c(10,11,12),42]),mean(Output.model$q75$psi[c(7),c(10,11,12),42]),mean(Output.model$q75$psi[c(8),c(10,11,12),42]),
                               mean(Output.model$q75$psi[c(9),c(10,11,12),42]),mean(Output.model$q75$psi[c(10),c(10,11,12),42]),mean(Output.model$q75$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),42]),
                               mean(Output.model$q75$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),42]),mean(Output.model$q75$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),42]),mean(Output.model$q75$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),42]),
                               mean(Output.model$q75$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),42]),mean(Output.model$q75$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),42]),mean(Output.model$q75$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),42]),
                               mean(Output.model$q75$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),42]),mean(Output.model$q75$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),42]))),
                        mean(c(mean(Output.model$q75$psi[c(2),c(10,11,12),43]),mean(Output.model$q75$psi[c(3),c(10,11,12),43]),mean(Output.model$q75$psi[c(4),c(10,11,12),43]),
                               mean(Output.model$q75$psi[c(6),c(10,11,12),43]),mean(Output.model$q75$psi[c(7),c(10,11,12),43]),mean(Output.model$q75$psi[c(8),c(10,11,12),43]),
                               mean(Output.model$q75$psi[c(9),c(10,11,12),43]),mean(Output.model$q75$psi[c(10),c(10,11,12),43]),mean(Output.model$q75$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),43]),
                               mean(Output.model$q75$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),43]),mean(Output.model$q75$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),43]),mean(Output.model$q75$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),43]),
                               mean(Output.model$q75$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),43]),mean(Output.model$q75$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),43]),mean(Output.model$q75$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),43]),
                               mean(Output.model$q75$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),43]),mean(Output.model$q75$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),43]))),
                        mean(c(mean(Output.model$q75$psi[c(2),c(10,11,12),38]),mean(Output.model$q75$psi[c(3),c(10,11,12),38]),mean(Output.model$q75$psi[c(4),c(10,11,12),38]),
                               mean(Output.model$q75$psi[c(6),c(10,11,12),38]),mean(Output.model$q75$psi[c(7),c(10,11,12),38]),mean(Output.model$q75$psi[c(8),c(10,11,12),38]),
                               mean(Output.model$q75$psi[c(9),c(10,11,12),38]),mean(Output.model$q75$psi[c(10),c(10,11,12),38]),mean(Output.model$q75$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),38]),
                               mean(Output.model$q75$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),38]),mean(Output.model$q75$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),38]),mean(Output.model$q75$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),38]),
                               mean(Output.model$q75$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),38]),mean(Output.model$q75$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),38]),mean(Output.model$q75$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),38]),
                               mean(Output.model$q75$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),38]),mean(Output.model$q75$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),38]))),
                        mean(c(mean(Output.model$q75$psi[c(2),c(10,11,12),24]),mean(Output.model$q75$psi[c(3),c(10,11,12),24]),mean(Output.model$q75$psi[c(4),c(10,11,12),24]),
                               mean(Output.model$q75$psi[c(6),c(10,11,12),24]),mean(Output.model$q75$psi[c(7),c(10,11,12),24]),mean(Output.model$q75$psi[c(8),c(10,11,12),24]),
                               mean(Output.model$q75$psi[c(9),c(10,11,12),24]),mean(Output.model$q75$psi[c(10),c(10,11,12),24]),mean(Output.model$q75$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),24]),
                               mean(Output.model$q75$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),24]),mean(Output.model$q75$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),24]),mean(Output.model$q75$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),24]),
                               mean(Output.model$q75$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),24]),mean(Output.model$q75$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),24]),mean(Output.model$q75$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),24]),
                               mean(Output.model$q75$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),24]),mean(Output.model$q75$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),24]))),
                        mean(c(mean(Output.model$q75$psi[c(2),c(10,11,12),37]),mean(Output.model$q75$psi[c(3),c(10,11,12),37]),mean(Output.model$q75$psi[c(4),c(10,11,12),37]),
                               mean(Output.model$q75$psi[c(6),c(10,11,12),37]),mean(Output.model$q75$psi[c(7),c(10,11,12),37]),mean(Output.model$q75$psi[c(8),c(10,11,12),37]),
                               mean(Output.model$q75$psi[c(9),c(10,11,12),37]),mean(Output.model$q75$psi[c(10),c(10,11,12),37]),mean(Output.model$q75$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),37]),
                               mean(Output.model$q75$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),37]),mean(Output.model$q75$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),37]),mean(Output.model$q75$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),37]),
                               mean(Output.model$q75$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),37]),mean(Output.model$q75$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),37]),mean(Output.model$q75$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),37]),
                               mean(Output.model$q75$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),37]),mean(Output.model$q75$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),37]))),
                        mean(c(mean(Output.model$q75$psi[c(2),c(10,11,12),13]),mean(Output.model$q75$psi[c(3),c(10,11,12),13]),mean(Output.model$q75$psi[c(4),c(10,11,12),13]),
                               mean(Output.model$q75$psi[c(6),c(10,11,12),13]),mean(Output.model$q75$psi[c(7),c(10,11,12),13]),mean(Output.model$q75$psi[c(8),c(10,11,12),13]),
                               mean(Output.model$q75$psi[c(9),c(10,11,12),13]),mean(Output.model$q75$psi[c(10),c(10,11,12),13]),mean(Output.model$q75$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),13]),
                               mean(Output.model$q75$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),13]),mean(Output.model$q75$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),13]),mean(Output.model$q75$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),13]),
                               mean(Output.model$q75$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),13]),mean(Output.model$q75$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),13]),mean(Output.model$q75$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),13]),
                               mean(Output.model$q75$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),13]),mean(Output.model$q75$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),13]))),
                        mean(c(mean(Output.model$q75$psi[c(2),c(10,11,12),21]),mean(Output.model$q75$psi[c(3),c(10,11,12),21]),mean(Output.model$q75$psi[c(4),c(10,11,12),21]),
                               mean(Output.model$q75$psi[c(6),c(10,11,12),21]),mean(Output.model$q75$psi[c(7),c(10,11,12),21]),mean(Output.model$q75$psi[c(8),c(10,11,12),21]),
                               mean(Output.model$q75$psi[c(9),c(10,11,12),21]),mean(Output.model$q75$psi[c(10),c(10,11,12),21]),mean(Output.model$q75$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),21]),
                               mean(Output.model$q75$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),21]),mean(Output.model$q75$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),21]),mean(Output.model$q75$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),21]),
                               mean(Output.model$q75$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),21]),mean(Output.model$q75$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),21]),mean(Output.model$q75$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),21]),
                               mean(Output.model$q75$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),21]),mean(Output.model$q75$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),21]))),
                        mean(c(mean(Output.model$q75$psi[c(2),c(10,11,12),34]),mean(Output.model$q75$psi[c(3),c(10,11,12),34]),mean(Output.model$q75$psi[c(4),c(10,11,12),34]),
                               mean(Output.model$q75$psi[c(6),c(10,11,12),34]),mean(Output.model$q75$psi[c(7),c(10,11,12),34]),mean(Output.model$q75$psi[c(8),c(10,11,12),34]),
                               mean(Output.model$q75$psi[c(9),c(10,11,12),34]),mean(Output.model$q75$psi[c(10),c(10,11,12),34]),mean(Output.model$q75$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),34]),
                               mean(Output.model$q75$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),34]),mean(Output.model$q75$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),34]),mean(Output.model$q75$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),34]),
                               mean(Output.model$q75$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),34]),mean(Output.model$q75$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),34]),mean(Output.model$q75$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),34]),
                               mean(Output.model$q75$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),34]),mean(Output.model$q75$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),34]))),
                        mean(c(mean(Output.model$q75$psi[c(2),c(10,11,12),2]),mean(Output.model$q75$psi[c(3),c(10,11,12),2]),mean(Output.model$q75$psi[c(4),c(10,11,12),2]),
                               mean(Output.model$q75$psi[c(6),c(10,11,12),2]),mean(Output.model$q75$psi[c(7),c(10,11,12),2]),mean(Output.model$q75$psi[c(8),c(10,11,12),2]),
                               mean(Output.model$q75$psi[c(9),c(10,11,12),2]),mean(Output.model$q75$psi[c(10),c(10,11,12),2]),mean(Output.model$q75$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),2]),
                               mean(Output.model$q75$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),2]),mean(Output.model$q75$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),2]),mean(Output.model$q75$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),2]),
                               mean(Output.model$q75$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),2]),mean(Output.model$q75$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),2]),mean(Output.model$q75$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),2]),
                               mean(Output.model$q75$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),2]),mean(Output.model$q75$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),2]))),
                        mean(c(mean(Output.model$q75$psi[c(2),c(10,11,12),32]),mean(Output.model$q75$psi[c(3),c(10,11,12),32]),mean(Output.model$q75$psi[c(4),c(10,11,12),32]),
                               mean(Output.model$q75$psi[c(6),c(10,11,12),32]),mean(Output.model$q75$psi[c(7),c(10,11,12),32]),mean(Output.model$q75$psi[c(8),c(10,11,12),32]),
                               mean(Output.model$q75$psi[c(9),c(10,11,12),32]),mean(Output.model$q75$psi[c(10),c(10,11,12),32]),mean(Output.model$q75$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),32]),
                               mean(Output.model$q75$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),32]),mean(Output.model$q75$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),32]),mean(Output.model$q75$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),32]),
                               mean(Output.model$q75$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),32]),mean(Output.model$q75$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),32]),mean(Output.model$q75$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),32]),
                               mean(Output.model$q75$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),32]),mean(Output.model$q75$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),32]))),
                        mean(c(mean(Output.model$q75$psi[c(2),c(10,11,12),17]),mean(Output.model$q75$psi[c(3),c(10,11,12),17]),mean(Output.model$q75$psi[c(4),c(10,11,12),17]),
                               mean(Output.model$q75$psi[c(6),c(10,11,12),17]),mean(Output.model$q75$psi[c(7),c(10,11,12),17]),mean(Output.model$q75$psi[c(8),c(10,11,12),17]),
                               mean(Output.model$q75$psi[c(9),c(10,11,12),17]),mean(Output.model$q75$psi[c(10),c(10,11,12),17]),mean(Output.model$q75$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),17]),
                               mean(Output.model$q75$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),17]),mean(Output.model$q75$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),17]),mean(Output.model$q75$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),17]),
                               mean(Output.model$q75$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),17]),mean(Output.model$q75$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),17]),mean(Output.model$q75$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),17]),
                               mean(Output.model$q75$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),17]),mean(Output.model$q75$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),17]))),
                        mean(c(mean(Output.model$q75$psi[c(2),c(10,11,12),4]),mean(Output.model$q75$psi[c(3),c(10,11,12),4]),mean(Output.model$q75$psi[c(4),c(10,11,12),4]),
                               mean(Output.model$q75$psi[c(6),c(10,11,12),4]),mean(Output.model$q75$psi[c(7),c(10,11,12),4]),mean(Output.model$q75$psi[c(8),c(10,11,12),4]),
                               mean(Output.model$q75$psi[c(9),c(10,11,12),4]),mean(Output.model$q75$psi[c(10),c(10,11,12),4]),mean(Output.model$q75$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),4]),
                               mean(Output.model$q75$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),4]),mean(Output.model$q75$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),4]),mean(Output.model$q75$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),4]),
                               mean(Output.model$q75$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),4]),mean(Output.model$q75$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),4]),mean(Output.model$q75$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),4]),
                               mean(Output.model$q75$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),4]),mean(Output.model$q75$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),4]))),
                        mean(c(mean(Output.model$q75$psi[c(2),c(10,11,12),8]),mean(Output.model$q75$psi[c(3),c(10,11,12),8]),mean(Output.model$q75$psi[c(4),c(10,11,12),8]),
                               mean(Output.model$q75$psi[c(6),c(10,11,12),8]),mean(Output.model$q75$psi[c(7),c(10,11,12),8]),mean(Output.model$q75$psi[c(8),c(10,11,12),8]),
                               mean(Output.model$q75$psi[c(9),c(10,11,12),8]),mean(Output.model$q75$psi[c(10),c(10,11,12),8]),mean(Output.model$q75$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),8]),
                               mean(Output.model$q75$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),8]),mean(Output.model$q75$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),8]),mean(Output.model$q75$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),8]),
                               mean(Output.model$q75$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),8]),mean(Output.model$q75$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),8]),mean(Output.model$q75$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),8]),
                               mean(Output.model$q75$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),8]),mean(Output.model$q75$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),8]))),
                        mean(c(mean(Output.model$q75$psi[c(2),c(10,11,12),39]),mean(Output.model$q75$psi[c(3),c(10,11,12),39]),mean(Output.model$q75$psi[c(4),c(10,11,12),39]),
                               mean(Output.model$q75$psi[c(6),c(10,11,12),39]),mean(Output.model$q75$psi[c(7),c(10,11,12),39]),mean(Output.model$q75$psi[c(8),c(10,11,12),39]),
                               mean(Output.model$q75$psi[c(9),c(10,11,12),39]),mean(Output.model$q75$psi[c(10),c(10,11,12),39]),mean(Output.model$q75$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),39]),
                               mean(Output.model$q75$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),39]),mean(Output.model$q75$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),39]),mean(Output.model$q75$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),39]),
                               mean(Output.model$q75$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),39]),mean(Output.model$q75$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),39]),mean(Output.model$q75$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),39]),
                               mean(Output.model$q75$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),39]),mean(Output.model$q75$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),39]))),
                        mean(c(mean(Output.model$q75$psi[c(2),c(10,11,12),10]),mean(Output.model$q75$psi[c(3),c(10,11,12),10]),mean(Output.model$q75$psi[c(4),c(10,11,12),10]),
                               mean(Output.model$q75$psi[c(6),c(10,11,12),10]),mean(Output.model$q75$psi[c(7),c(10,11,12),10]),mean(Output.model$q75$psi[c(8),c(10,11,12),10]),
                               mean(Output.model$q75$psi[c(9),c(10,11,12),10]),mean(Output.model$q75$psi[c(10),c(10,11,12),10]),mean(Output.model$q75$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),10]),
                               mean(Output.model$q75$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),10]),mean(Output.model$q75$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),10]),mean(Output.model$q75$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),10]),
                               mean(Output.model$q75$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),10]),mean(Output.model$q75$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),10]),mean(Output.model$q75$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),10]),
                               mean(Output.model$q75$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),10]),mean(Output.model$q75$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),10]))),
                        mean(c(mean(Output.model$q75$psi[c(2),c(10,11,12),19]),mean(Output.model$q75$psi[c(3),c(10,11,12),19]),mean(Output.model$q75$psi[c(4),c(10,11,12),19]),
                               mean(Output.model$q75$psi[c(6),c(10,11,12),19]),mean(Output.model$q75$psi[c(7),c(10,11,12),19]),mean(Output.model$q75$psi[c(8),c(10,11,12),19]),
                               mean(Output.model$q75$psi[c(9),c(10,11,12),19]),mean(Output.model$q75$psi[c(10),c(10,11,12),19]),mean(Output.model$q75$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),19]),
                               mean(Output.model$q75$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),19]),mean(Output.model$q75$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),19]),mean(Output.model$q75$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),19]),
                               mean(Output.model$q75$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),19]),mean(Output.model$q75$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),19]),mean(Output.model$q75$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),19]),
                               mean(Output.model$q75$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),19]),mean(Output.model$q75$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),19]))),
                        mean(c(mean(Output.model$q75$psi[c(2),c(10,11,12),35]),mean(Output.model$q75$psi[c(3),c(10,11,12),35]),mean(Output.model$q75$psi[c(4),c(10,11,12),35]),
                               mean(Output.model$q75$psi[c(6),c(10,11,12),35]),mean(Output.model$q75$psi[c(7),c(10,11,12),35]),mean(Output.model$q75$psi[c(8),c(10,11,12),35]),
                               mean(Output.model$q75$psi[c(9),c(10,11,12),35]),mean(Output.model$q75$psi[c(10),c(10,11,12),35]),mean(Output.model$q75$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),35]),
                               mean(Output.model$q75$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),35]),mean(Output.model$q75$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),35]),mean(Output.model$q75$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),35]),
                               mean(Output.model$q75$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),35]),mean(Output.model$q75$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),35]),mean(Output.model$q75$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),35]),
                               mean(Output.model$q75$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),35]),mean(Output.model$q75$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),35]))),
                        mean(c(mean(Output.model$q75$psi[c(2),c(10,11,12),25]),mean(Output.model$q75$psi[c(3),c(10,11,12),25]),mean(Output.model$q75$psi[c(4),c(10,11,12),25]),
                               mean(Output.model$q75$psi[c(6),c(10,11,12),25]),mean(Output.model$q75$psi[c(7),c(10,11,12),25]),mean(Output.model$q75$psi[c(8),c(10,11,12),25]),
                               mean(Output.model$q75$psi[c(9),c(10,11,12),25]),mean(Output.model$q75$psi[c(10),c(10,11,12),25]),mean(Output.model$q75$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),25]),
                               mean(Output.model$q75$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),25]),mean(Output.model$q75$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),25]),mean(Output.model$q75$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),25]),
                               mean(Output.model$q75$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),25]),mean(Output.model$q75$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),25]),mean(Output.model$q75$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),25]),
                               mean(Output.model$q75$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),25]),mean(Output.model$q75$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),25]))),
                        mean(c(mean(Output.model$q75$psi[c(2),c(10,11,12),28]),mean(Output.model$q75$psi[c(3),c(10,11,12),28]),mean(Output.model$q75$psi[c(4),c(10,11,12),28]),
                               mean(Output.model$q75$psi[c(6),c(10,11,12),28]),mean(Output.model$q75$psi[c(7),c(10,11,12),28]),mean(Output.model$q75$psi[c(8),c(10,11,12),28]),
                               mean(Output.model$q75$psi[c(9),c(10,11,12),28]),mean(Output.model$q75$psi[c(10),c(10,11,12),28]),mean(Output.model$q75$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),28]),
                               mean(Output.model$q75$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),28]),mean(Output.model$q75$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),28]),mean(Output.model$q75$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),28]),
                               mean(Output.model$q75$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),28]),mean(Output.model$q75$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),28]),mean(Output.model$q75$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),28]),
                               mean(Output.model$q75$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),28]),mean(Output.model$q75$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),28]))),
                        mean(c(mean(Output.model$q75$psi[c(2),c(10,11,12),46]),mean(Output.model$q75$psi[c(3),c(10,11,12),46]),mean(Output.model$q75$psi[c(4),c(10,11,12),46]),
                               mean(Output.model$q75$psi[c(6),c(10,11,12),46]),mean(Output.model$q75$psi[c(7),c(10,11,12),46]),mean(Output.model$q75$psi[c(8),c(10,11,12),46]),
                               mean(Output.model$q75$psi[c(9),c(10,11,12),46]),mean(Output.model$q75$psi[c(10),c(10,11,12),46]),mean(Output.model$q75$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),46]),
                               mean(Output.model$q75$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),46]),mean(Output.model$q75$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),46]),mean(Output.model$q75$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),46]),
                               mean(Output.model$q75$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),46]),mean(Output.model$q75$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),46]),mean(Output.model$q75$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),46]),
                               mean(Output.model$q75$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),46]),mean(Output.model$q75$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),46]))),
                        mean(c(mean(Output.model$q75$psi[c(2),c(10,11,12),41]),mean(Output.model$q75$psi[c(3),c(10,11,12),41]),mean(Output.model$q75$psi[c(4),c(10,11,12),41]),
                               mean(Output.model$q75$psi[c(6),c(10,11,12),41]),mean(Output.model$q75$psi[c(7),c(10,11,12),41]),mean(Output.model$q75$psi[c(8),c(10,11,12),41]),
                               mean(Output.model$q75$psi[c(9),c(10,11,12),41]),mean(Output.model$q75$psi[c(10),c(10,11,12),41]),mean(Output.model$q75$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),41]),
                               mean(Output.model$q75$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),41]),mean(Output.model$q75$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),41]),mean(Output.model$q75$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),41]),
                               mean(Output.model$q75$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),41]),mean(Output.model$q75$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),41]),mean(Output.model$q75$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),41]),
                               mean(Output.model$q75$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),41]),mean(Output.model$q75$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),41]))),
                        mean(c(mean(Output.model$q75$psi[c(2),c(10,11,12),16]),mean(Output.model$q75$psi[c(3),c(10,11,12),16]),mean(Output.model$q75$psi[c(4),c(10,11,12),16]),
                               mean(Output.model$q75$psi[c(6),c(10,11,12),16]),mean(Output.model$q75$psi[c(7),c(10,11,12),16]),mean(Output.model$q75$psi[c(8),c(10,11,12),16]),
                               mean(Output.model$q75$psi[c(9),c(10,11,12),16]),mean(Output.model$q75$psi[c(10),c(10,11,12),16]),mean(Output.model$q75$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),16]),
                               mean(Output.model$q75$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),16]),mean(Output.model$q75$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),16]),mean(Output.model$q75$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),16]),
                               mean(Output.model$q75$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),16]),mean(Output.model$q75$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),16]),mean(Output.model$q75$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),16]),
                               mean(Output.model$q75$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),16]),mean(Output.model$q75$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),16]))),
                        mean(c(mean(Output.model$q75$psi[c(2),c(10,11,12),36]),mean(Output.model$q75$psi[c(3),c(10,11,12),36]),mean(Output.model$q75$psi[c(4),c(10,11,12),36]),
                               mean(Output.model$q75$psi[c(6),c(10,11,12),36]),mean(Output.model$q75$psi[c(7),c(10,11,12),36]),mean(Output.model$q75$psi[c(8),c(10,11,12),36]),
                               mean(Output.model$q75$psi[c(9),c(10,11,12),36]),mean(Output.model$q75$psi[c(10),c(10,11,12),36]),mean(Output.model$q75$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),36]),
                               mean(Output.model$q75$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),36]),mean(Output.model$q75$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),36]),mean(Output.model$q75$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),36]),
                               mean(Output.model$q75$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),36]),mean(Output.model$q75$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),36]),mean(Output.model$q75$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),36]),
                               mean(Output.model$q75$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),36]),mean(Output.model$q75$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),36]))),
                        mean(c(mean(Output.model$q75$psi[c(2),c(10,11,12),14]),mean(Output.model$q75$psi[c(3),c(10,11,12),14]),mean(Output.model$q75$psi[c(4),c(10,11,12),14]),
                               mean(Output.model$q75$psi[c(6),c(10,11,12),14]),mean(Output.model$q75$psi[c(7),c(10,11,12),14]),mean(Output.model$q75$psi[c(8),c(10,11,12),14]),
                               mean(Output.model$q75$psi[c(9),c(10,11,12),14]),mean(Output.model$q75$psi[c(10),c(10,11,12),14]),mean(Output.model$q75$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),14]),
                               mean(Output.model$q75$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),14]),mean(Output.model$q75$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),14]),mean(Output.model$q75$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),14]),
                               mean(Output.model$q75$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),14]),mean(Output.model$q75$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),14]),mean(Output.model$q75$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),14]),
                               mean(Output.model$q75$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),14]),mean(Output.model$q75$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),14]))),
                        mean(c(mean(Output.model$q75$psi[c(2),c(10,11,12),11]),mean(Output.model$q75$psi[c(3),c(10,11,12),11]),mean(Output.model$q75$psi[c(4),c(10,11,12),11]),
                               mean(Output.model$q75$psi[c(6),c(10,11,12),11]),mean(Output.model$q75$psi[c(7),c(10,11,12),11]),mean(Output.model$q75$psi[c(8),c(10,11,12),11]),
                               mean(Output.model$q75$psi[c(9),c(10,11,12),11]),mean(Output.model$q75$psi[c(10),c(10,11,12),11]),mean(Output.model$q75$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),11]),
                               mean(Output.model$q75$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),11]),mean(Output.model$q75$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),11]),mean(Output.model$q75$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),11]),
                               mean(Output.model$q75$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),11]),mean(Output.model$q75$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),11]),mean(Output.model$q75$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),11]),
                               mean(Output.model$q75$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),11]),mean(Output.model$q75$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),11]))),
                        mean(c(mean(Output.model$q75$psi[c(2),c(10,11,12),31]),mean(Output.model$q75$psi[c(3),c(10,11,12),31]),mean(Output.model$q75$psi[c(4),c(10,11,12),31]),
                               mean(Output.model$q75$psi[c(6),c(10,11,12),31]),mean(Output.model$q75$psi[c(7),c(10,11,12),31]),mean(Output.model$q75$psi[c(8),c(10,11,12),31]),
                               mean(Output.model$q75$psi[c(9),c(10,11,12),31]),mean(Output.model$q75$psi[c(10),c(10,11,12),31]),mean(Output.model$q75$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),31]),
                               mean(Output.model$q75$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),31]),mean(Output.model$q75$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),31]),mean(Output.model$q75$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),31]),
                               mean(Output.model$q75$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),31]),mean(Output.model$q75$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),31]),mean(Output.model$q75$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),31]),
                               mean(Output.model$q75$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),31]),mean(Output.model$q75$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),31]))),
                        mean(c(mean(Output.model$q75$psi[c(2),c(10,11,12),6]),mean(Output.model$q75$psi[c(3),c(10,11,12),6]),mean(Output.model$q75$psi[c(4),c(10,11,12),6]),
                               mean(Output.model$q75$psi[c(6),c(10,11,12),6]),mean(Output.model$q75$psi[c(7),c(10,11,12),6]),mean(Output.model$q75$psi[c(8),c(10,11,12),6]),
                               mean(Output.model$q75$psi[c(9),c(10,11,12),6]),mean(Output.model$q75$psi[c(10),c(10,11,12),6]),mean(Output.model$q75$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),6]),
                               mean(Output.model$q75$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),6]),mean(Output.model$q75$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),6]),mean(Output.model$q75$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),6]),
                               mean(Output.model$q75$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),6]),mean(Output.model$q75$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),6]),mean(Output.model$q75$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),6]),
                               mean(Output.model$q75$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),6]),mean(Output.model$q75$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),6])))),
                      c(mean(c(mean(Output.model$q97.5$psi[c(2),c(10,11,12),45]),mean(Output.model$q97.5$psi[c(3),c(10,11,12),45]),mean(Output.model$q97.5$psi[c(4),c(10,11,12),45]),
                               mean(Output.model$q97.5$psi[c(6),c(10,11,12),45]),mean(Output.model$q97.5$psi[c(7),c(10,11,12),45]),mean(Output.model$q97.5$psi[c(8),c(10,11,12),45]),
                               mean(Output.model$q97.5$psi[c(9),c(10,11,12),45]),mean(Output.model$q97.5$psi[c(10),c(10,11,12),45]),mean(Output.model$q97.5$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),45]),
                               mean(Output.model$q97.5$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),45]),mean(Output.model$q97.5$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),45]),mean(Output.model$q97.5$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),45]),
                               mean(Output.model$q97.5$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),45]),mean(Output.model$q97.5$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),45]),mean(Output.model$q97.5$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),45]),
                               mean(Output.model$q97.5$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),45]),mean(Output.model$q97.5$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),45]))),
                        mean(c(mean(Output.model$q97.5$psi[c(2),c(10,11,12),1]),mean(Output.model$q97.5$psi[c(3),c(10,11,12),1]),mean(Output.model$q97.5$psi[c(4),c(10,11,12),1]),
                               mean(Output.model$q97.5$psi[c(6),c(10,11,12),1]),mean(Output.model$q97.5$psi[c(7),c(10,11,12),1]),mean(Output.model$q97.5$psi[c(8),c(10,11,12),1]),
                               mean(Output.model$q97.5$psi[c(9),c(10,11,12),1]),mean(Output.model$q97.5$psi[c(10),c(10,11,12),1]),mean(Output.model$q97.5$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),1]),
                               mean(Output.model$q97.5$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),1]),mean(Output.model$q97.5$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),1]),mean(Output.model$q97.5$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),1]),
                               mean(Output.model$q97.5$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),1]),mean(Output.model$q97.5$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),1]),mean(Output.model$q97.5$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),1]),
                               mean(Output.model$q97.5$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),1]),mean(Output.model$q97.5$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),1]))),
                        mean(c(mean(Output.model$q97.5$psi[c(2),c(10,11,12),15]),mean(Output.model$q97.5$psi[c(3),c(10,11,12),15]),mean(Output.model$q97.5$psi[c(4),c(10,11,12),15]),
                               mean(Output.model$q97.5$psi[c(6),c(10,11,12),15]),mean(Output.model$q97.5$psi[c(7),c(10,11,12),15]),mean(Output.model$q97.5$psi[c(8),c(10,11,12),15]),
                               mean(Output.model$q97.5$psi[c(9),c(10,11,12),15]),mean(Output.model$q97.5$psi[c(10),c(10,11,12),15]),mean(Output.model$q97.5$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),15]),
                               mean(Output.model$q97.5$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),15]),mean(Output.model$q97.5$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),15]),mean(Output.model$q97.5$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),15]),
                               mean(Output.model$q97.5$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),15]),mean(Output.model$q97.5$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),15]),mean(Output.model$q97.5$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),15]),
                               mean(Output.model$q97.5$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),15]),mean(Output.model$q97.5$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),15]))),
                        mean(c(mean(Output.model$q97.5$psi[c(2),c(10,11,12),7]),mean(Output.model$q97.5$psi[c(3),c(10,11,12),7]),mean(Output.model$q97.5$psi[c(4),c(10,11,12),7]),
                               mean(Output.model$q97.5$psi[c(6),c(10,11,12),7]),mean(Output.model$q97.5$psi[c(7),c(10,11,12),7]),mean(Output.model$q97.5$psi[c(8),c(10,11,12),7]),
                               mean(Output.model$q97.5$psi[c(9),c(10,11,12),7]),mean(Output.model$q97.5$psi[c(10),c(10,11,12),7]),mean(Output.model$q97.5$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),7]),
                               mean(Output.model$q97.5$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),7]),mean(Output.model$q97.5$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),7]),mean(Output.model$q97.5$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),7]),
                               mean(Output.model$q97.5$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),7]),mean(Output.model$q97.5$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),7]),mean(Output.model$q97.5$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),7]),
                               mean(Output.model$q97.5$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),7]),mean(Output.model$q97.5$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),7]))),
                        mean(c(mean(Output.model$q97.5$psi[c(2),c(10,11,12),22]),mean(Output.model$q97.5$psi[c(3),c(10,11,12),22]),mean(Output.model$q97.5$psi[c(4),c(10,11,12),22]),
                               mean(Output.model$q97.5$psi[c(6),c(10,11,12),22]),mean(Output.model$q97.5$psi[c(7),c(10,11,12),22]),mean(Output.model$q97.5$psi[c(8),c(10,11,12),22]),
                               mean(Output.model$q97.5$psi[c(9),c(10,11,12),22]),mean(Output.model$q97.5$psi[c(10),c(10,11,12),22]),mean(Output.model$q97.5$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),22]),
                               mean(Output.model$q97.5$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),22]),mean(Output.model$q97.5$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),22]),mean(Output.model$q97.5$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),22]),
                               mean(Output.model$q97.5$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),22]),mean(Output.model$q97.5$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),22]),mean(Output.model$q97.5$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),22]),
                               mean(Output.model$q97.5$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),22]),mean(Output.model$q97.5$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),22]))),
                        mean(c(mean(Output.model$q97.5$psi[c(2),c(10,11,12),29]),mean(Output.model$q97.5$psi[c(3),c(10,11,12),29]),mean(Output.model$q97.5$psi[c(4),c(10,11,12),29]),
                               mean(Output.model$q97.5$psi[c(6),c(10,11,12),29]),mean(Output.model$q97.5$psi[c(7),c(10,11,12),29]),mean(Output.model$q97.5$psi[c(8),c(10,11,12),29]),
                               mean(Output.model$q97.5$psi[c(9),c(10,11,12),29]),mean(Output.model$q97.5$psi[c(10),c(10,11,12),29]),mean(Output.model$q97.5$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),29]),
                               mean(Output.model$q97.5$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),29]),mean(Output.model$q97.5$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),29]),mean(Output.model$q97.5$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),29]),
                               mean(Output.model$q97.5$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),29]),mean(Output.model$q97.5$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),29]),mean(Output.model$q97.5$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),29]),
                               mean(Output.model$q97.5$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),29]),mean(Output.model$q97.5$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),29]))),
                        mean(c(mean(Output.model$q97.5$psi[c(2),c(10,11,12),18]),mean(Output.model$q97.5$psi[c(3),c(10,11,12),18]),mean(Output.model$q97.5$psi[c(4),c(10,11,12),18]),
                               mean(Output.model$q97.5$psi[c(6),c(10,11,12),18]),mean(Output.model$q97.5$psi[c(7),c(10,11,12),18]),mean(Output.model$q97.5$psi[c(8),c(10,11,12),18]),
                               mean(Output.model$q97.5$psi[c(9),c(10,11,12),18]),mean(Output.model$q97.5$psi[c(10),c(10,11,12),18]),mean(Output.model$q97.5$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),18]),
                               mean(Output.model$q97.5$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),18]),mean(Output.model$q97.5$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),18]),mean(Output.model$q97.5$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),18]),
                               mean(Output.model$q97.5$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),18]),mean(Output.model$q97.5$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),18]),mean(Output.model$q97.5$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),18]),
                               mean(Output.model$q97.5$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),18]),mean(Output.model$q97.5$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),18]))),
                        mean(c(mean(Output.model$q97.5$psi[c(2),c(10,11,12),33]),mean(Output.model$q97.5$psi[c(3),c(10,11,12),33]),mean(Output.model$q97.5$psi[c(4),c(10,11,12),33]),
                               mean(Output.model$q97.5$psi[c(6),c(10,11,12),33]),mean(Output.model$q97.5$psi[c(7),c(10,11,12),33]),mean(Output.model$q97.5$psi[c(8),c(10,11,12),33]),
                               mean(Output.model$q97.5$psi[c(9),c(10,11,12),33]),mean(Output.model$q97.5$psi[c(10),c(10,11,12),33]),mean(Output.model$q97.5$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),33]),
                               mean(Output.model$q97.5$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),33]),mean(Output.model$q97.5$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),33]),mean(Output.model$q97.5$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),33]),
                               mean(Output.model$q97.5$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),33]),mean(Output.model$q97.5$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),33]),mean(Output.model$q97.5$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),33]),
                               mean(Output.model$q97.5$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),33]),mean(Output.model$q97.5$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),33]))),
                        mean(c(mean(Output.model$q97.5$psi[c(2),c(10,11,12),12]),mean(Output.model$q97.5$psi[c(3),c(10,11,12),12]),mean(Output.model$q97.5$psi[c(4),c(10,11,12),12]),
                               mean(Output.model$q97.5$psi[c(6),c(10,11,12),12]),mean(Output.model$q97.5$psi[c(7),c(10,11,12),12]),mean(Output.model$q97.5$psi[c(8),c(10,11,12),12]),
                               mean(Output.model$q97.5$psi[c(9),c(10,11,12),12]),mean(Output.model$q97.5$psi[c(10),c(10,11,12),12]),mean(Output.model$q97.5$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),12]),
                               mean(Output.model$q97.5$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),12]),mean(Output.model$q97.5$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),12]),mean(Output.model$q97.5$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),12]),
                               mean(Output.model$q97.5$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),12]),mean(Output.model$q97.5$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),12]),mean(Output.model$q97.5$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),12]),
                               mean(Output.model$q97.5$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),12]),mean(Output.model$q97.5$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),12]))),
                        mean(c(mean(Output.model$q97.5$psi[c(2),c(10,11,12),9]),mean(Output.model$q97.5$psi[c(3),c(10,11,12),9]),mean(Output.model$q97.5$psi[c(4),c(10,11,12),9]),
                               mean(Output.model$q97.5$psi[c(6),c(10,11,12),9]),mean(Output.model$q97.5$psi[c(7),c(10,11,12),9]),mean(Output.model$q97.5$psi[c(8),c(10,11,12),9]),
                               mean(Output.model$q97.5$psi[c(9),c(10,11,12),9]),mean(Output.model$q97.5$psi[c(10),c(10,11,12),9]),mean(Output.model$q97.5$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),9]),
                               mean(Output.model$q97.5$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),9]),mean(Output.model$q97.5$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),9]),mean(Output.model$q97.5$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),9]),
                               mean(Output.model$q97.5$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),9]),mean(Output.model$q97.5$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),9]),mean(Output.model$q97.5$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),9]),
                               mean(Output.model$q97.5$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),9]),mean(Output.model$q97.5$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),9]))),
                        mean(c(mean(Output.model$q97.5$psi[c(2),c(10,11,12),47]),mean(Output.model$q97.5$psi[c(3),c(10,11,12),47]),mean(Output.model$q97.5$psi[c(4),c(10,11,12),47]),
                               mean(Output.model$q97.5$psi[c(6),c(10,11,12),47]),mean(Output.model$q97.5$psi[c(7),c(10,11,12),47]),mean(Output.model$q97.5$psi[c(8),c(10,11,12),47]),
                               mean(Output.model$q97.5$psi[c(9),c(10,11,12),47]),mean(Output.model$q97.5$psi[c(10),c(10,11,12),47]),mean(Output.model$q97.5$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),47]),
                               mean(Output.model$q97.5$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),47]),mean(Output.model$q97.5$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),47]),mean(Output.model$q97.5$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),47]),
                               mean(Output.model$q97.5$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),47]),mean(Output.model$q97.5$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),47]),mean(Output.model$q97.5$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),47]),
                               mean(Output.model$q97.5$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),47]),mean(Output.model$q97.5$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),47]))),
                        mean(c(mean(Output.model$q97.5$psi[c(2),c(10,11,12),30]),mean(Output.model$q97.5$psi[c(3),c(10,11,12),30]),mean(Output.model$q97.5$psi[c(4),c(10,11,12),30]),
                               mean(Output.model$q97.5$psi[c(6),c(10,11,12),30]),mean(Output.model$q97.5$psi[c(7),c(10,11,12),30]),mean(Output.model$q97.5$psi[c(8),c(10,11,12),30]),
                               mean(Output.model$q97.5$psi[c(9),c(10,11,12),30]),mean(Output.model$q97.5$psi[c(10),c(10,11,12),30]),mean(Output.model$q97.5$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),30]),
                               mean(Output.model$q97.5$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),30]),mean(Output.model$q97.5$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),30]),mean(Output.model$q97.5$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),30]),
                               mean(Output.model$q97.5$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),30]),mean(Output.model$q97.5$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),30]),mean(Output.model$q97.5$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),30]),
                               mean(Output.model$q97.5$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),30]),mean(Output.model$q97.5$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),30]))),
                        mean(c(mean(Output.model$q97.5$psi[c(2),c(10,11,12),40]),mean(Output.model$q97.5$psi[c(3),c(10,11,12),40]),mean(Output.model$q97.5$psi[c(4),c(10,11,12),40]),
                               mean(Output.model$q97.5$psi[c(6),c(10,11,12),40]),mean(Output.model$q97.5$psi[c(7),c(10,11,12),40]),mean(Output.model$q97.5$psi[c(8),c(10,11,12),40]),
                               mean(Output.model$q97.5$psi[c(9),c(10,11,12),40]),mean(Output.model$q97.5$psi[c(10),c(10,11,12),40]),mean(Output.model$q97.5$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),40]),
                               mean(Output.model$q97.5$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),40]),mean(Output.model$q97.5$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),40]),mean(Output.model$q97.5$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),40]),
                               mean(Output.model$q97.5$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),40]),mean(Output.model$q97.5$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),40]),mean(Output.model$q97.5$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),40]),
                               mean(Output.model$q97.5$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),40]),mean(Output.model$q97.5$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),40]))),
                        mean(c(mean(Output.model$q97.5$psi[c(2),c(10,11,12),5]),mean(Output.model$q97.5$psi[c(3),c(10,11,12),5]),mean(Output.model$q97.5$psi[c(4),c(10,11,12),5]),
                               mean(Output.model$q97.5$psi[c(6),c(10,11,12),5]),mean(Output.model$q97.5$psi[c(7),c(10,11,12),5]),mean(Output.model$q97.5$psi[c(8),c(10,11,12),5]),
                               mean(Output.model$q97.5$psi[c(9),c(10,11,12),5]),mean(Output.model$q97.5$psi[c(10),c(10,11,12),5]),mean(Output.model$q97.5$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),5]),
                               mean(Output.model$q97.5$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),5]),mean(Output.model$q97.5$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),5]),mean(Output.model$q97.5$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),5]),
                               mean(Output.model$q97.5$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),5]),mean(Output.model$q97.5$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),5]),mean(Output.model$q97.5$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),5]),
                               mean(Output.model$q97.5$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),5]),mean(Output.model$q97.5$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),5]))),
                        mean(c(mean(Output.model$q97.5$psi[c(2),c(10,11,12),20]),mean(Output.model$q97.5$psi[c(3),c(10,11,12),20]),mean(Output.model$q97.5$psi[c(4),c(10,11,12),20]),
                               mean(Output.model$q97.5$psi[c(6),c(10,11,12),20]),mean(Output.model$q97.5$psi[c(7),c(10,11,12),20]),mean(Output.model$q97.5$psi[c(8),c(10,11,12),20]),
                               mean(Output.model$q97.5$psi[c(9),c(10,11,12),20]),mean(Output.model$q97.5$psi[c(10),c(10,11,12),20]),mean(Output.model$q97.5$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),20]),
                               mean(Output.model$q97.5$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),20]),mean(Output.model$q97.5$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),20]),mean(Output.model$q97.5$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),20]),
                               mean(Output.model$q97.5$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),20]),mean(Output.model$q97.5$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),20]),mean(Output.model$q97.5$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),20]),
                               mean(Output.model$q97.5$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),20]),mean(Output.model$q97.5$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),20]))),
                        mean(c(mean(Output.model$q97.5$psi[c(2),c(10,11,12),23]),mean(Output.model$q97.5$psi[c(3),c(10,11,12),23]),mean(Output.model$q97.5$psi[c(4),c(10,11,12),23]),
                               mean(Output.model$q97.5$psi[c(6),c(10,11,12),23]),mean(Output.model$q97.5$psi[c(7),c(10,11,12),23]),mean(Output.model$q97.5$psi[c(8),c(10,11,12),23]),
                               mean(Output.model$q97.5$psi[c(9),c(10,11,12),23]),mean(Output.model$q97.5$psi[c(10),c(10,11,12),23]),mean(Output.model$q97.5$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),23]),
                               mean(Output.model$q97.5$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),23]),mean(Output.model$q97.5$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),23]),mean(Output.model$q97.5$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),23]),
                               mean(Output.model$q97.5$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),23]),mean(Output.model$q97.5$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),23]),mean(Output.model$q97.5$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),23]),
                               mean(Output.model$q97.5$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),23]),mean(Output.model$q97.5$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),23]))),
                        mean(c(mean(Output.model$q97.5$psi[c(2),c(10,11,12),3]),mean(Output.model$q97.5$psi[c(3),c(10,11,12),3]),mean(Output.model$q97.5$psi[c(4),c(10,11,12),3]),
                               mean(Output.model$q97.5$psi[c(6),c(10,11,12),3]),mean(Output.model$q97.5$psi[c(7),c(10,11,12),3]),mean(Output.model$q97.5$psi[c(8),c(10,11,12),3]),
                               mean(Output.model$q97.5$psi[c(9),c(10,11,12),3]),mean(Output.model$q97.5$psi[c(10),c(10,11,12),3]),mean(Output.model$q97.5$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),3]),
                               mean(Output.model$q97.5$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),3]),mean(Output.model$q97.5$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),3]),mean(Output.model$q97.5$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),3]),
                               mean(Output.model$q97.5$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),3]),mean(Output.model$q97.5$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),3]),mean(Output.model$q97.5$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),3]),
                               mean(Output.model$q97.5$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),3]),mean(Output.model$q97.5$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),3]))),
                        mean(c(mean(Output.model$q97.5$psi[c(2),c(10,11,12),27]),mean(Output.model$q97.5$psi[c(3),c(10,11,12),27]),mean(Output.model$q97.5$psi[c(4),c(10,11,12),27]),
                               mean(Output.model$q97.5$psi[c(6),c(10,11,12),27]),mean(Output.model$q97.5$psi[c(7),c(10,11,12),27]),mean(Output.model$q97.5$psi[c(8),c(10,11,12),27]),
                               mean(Output.model$q97.5$psi[c(9),c(10,11,12),27]),mean(Output.model$q97.5$psi[c(10),c(10,11,12),27]),mean(Output.model$q97.5$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),27]),
                               mean(Output.model$q97.5$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),27]),mean(Output.model$q97.5$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),27]),mean(Output.model$q97.5$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),27]),
                               mean(Output.model$q97.5$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),27]),mean(Output.model$q97.5$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),27]),mean(Output.model$q97.5$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),27]),
                               mean(Output.model$q97.5$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),27]),mean(Output.model$q97.5$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),27]))),
                        mean(c(mean(Output.model$q97.5$psi[c(2),c(10,11,12),26]),mean(Output.model$q97.5$psi[c(3),c(10,11,12),26]),mean(Output.model$q97.5$psi[c(4),c(10,11,12),26]),
                               mean(Output.model$q97.5$psi[c(6),c(10,11,12),26]),mean(Output.model$q97.5$psi[c(7),c(10,11,12),26]),mean(Output.model$q97.5$psi[c(8),c(10,11,12),26]),
                               mean(Output.model$q97.5$psi[c(9),c(10,11,12),26]),mean(Output.model$q97.5$psi[c(10),c(10,11,12),26]),mean(Output.model$q97.5$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),26]),
                               mean(Output.model$q97.5$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),26]),mean(Output.model$q97.5$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),26]),mean(Output.model$q97.5$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),26]),
                               mean(Output.model$q97.5$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),26]),mean(Output.model$q97.5$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),26]),mean(Output.model$q97.5$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),26]),
                               mean(Output.model$q97.5$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),26]),mean(Output.model$q97.5$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),26]))),
                        mean(c(mean(Output.model$q97.5$psi[c(2),c(10,11,12),44]),mean(Output.model$q97.5$psi[c(3),c(10,11,12),44]),mean(Output.model$q97.5$psi[c(4),c(10,11,12),44]),
                               mean(Output.model$q97.5$psi[c(6),c(10,11,12),44]),mean(Output.model$q97.5$psi[c(7),c(10,11,12),44]),mean(Output.model$q97.5$psi[c(8),c(10,11,12),44]),
                               mean(Output.model$q97.5$psi[c(9),c(10,11,12),44]),mean(Output.model$q97.5$psi[c(10),c(10,11,12),44]),mean(Output.model$q97.5$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),44]),
                               mean(Output.model$q97.5$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),44]),mean(Output.model$q97.5$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),44]),mean(Output.model$q97.5$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),44]),
                               mean(Output.model$q97.5$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),44]),mean(Output.model$q97.5$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),44]),mean(Output.model$q97.5$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),44]),
                               mean(Output.model$q97.5$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),44]),mean(Output.model$q97.5$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),44]))),
                        mean(c(mean(Output.model$q97.5$psi[c(2),c(10,11,12),42]),mean(Output.model$q97.5$psi[c(3),c(10,11,12),42]),mean(Output.model$q97.5$psi[c(4),c(10,11,12),42]),
                               mean(Output.model$q97.5$psi[c(6),c(10,11,12),42]),mean(Output.model$q97.5$psi[c(7),c(10,11,12),42]),mean(Output.model$q97.5$psi[c(8),c(10,11,12),42]),
                               mean(Output.model$q97.5$psi[c(9),c(10,11,12),42]),mean(Output.model$q97.5$psi[c(10),c(10,11,12),42]),mean(Output.model$q97.5$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),42]),
                               mean(Output.model$q97.5$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),42]),mean(Output.model$q97.5$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),42]),mean(Output.model$q97.5$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),42]),
                               mean(Output.model$q97.5$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),42]),mean(Output.model$q97.5$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),42]),mean(Output.model$q97.5$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),42]),
                               mean(Output.model$q97.5$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),42]),mean(Output.model$q97.5$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),42]))),
                        mean(c(mean(Output.model$q97.5$psi[c(2),c(10,11,12),43]),mean(Output.model$q97.5$psi[c(3),c(10,11,12),43]),mean(Output.model$q97.5$psi[c(4),c(10,11,12),43]),
                               mean(Output.model$q97.5$psi[c(6),c(10,11,12),43]),mean(Output.model$q97.5$psi[c(7),c(10,11,12),43]),mean(Output.model$q97.5$psi[c(8),c(10,11,12),43]),
                               mean(Output.model$q97.5$psi[c(9),c(10,11,12),43]),mean(Output.model$q97.5$psi[c(10),c(10,11,12),43]),mean(Output.model$q97.5$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),43]),
                               mean(Output.model$q97.5$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),43]),mean(Output.model$q97.5$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),43]),mean(Output.model$q97.5$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),43]),
                               mean(Output.model$q97.5$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),43]),mean(Output.model$q97.5$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),43]),mean(Output.model$q97.5$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),43]),
                               mean(Output.model$q97.5$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),43]),mean(Output.model$q97.5$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),43]))),
                        mean(c(mean(Output.model$q97.5$psi[c(2),c(10,11,12),38]),mean(Output.model$q97.5$psi[c(3),c(10,11,12),38]),mean(Output.model$q97.5$psi[c(4),c(10,11,12),38]),
                               mean(Output.model$q97.5$psi[c(6),c(10,11,12),38]),mean(Output.model$q97.5$psi[c(7),c(10,11,12),38]),mean(Output.model$q97.5$psi[c(8),c(10,11,12),38]),
                               mean(Output.model$q97.5$psi[c(9),c(10,11,12),38]),mean(Output.model$q97.5$psi[c(10),c(10,11,12),38]),mean(Output.model$q97.5$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),38]),
                               mean(Output.model$q97.5$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),38]),mean(Output.model$q97.5$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),38]),mean(Output.model$q97.5$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),38]),
                               mean(Output.model$q97.5$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),38]),mean(Output.model$q97.5$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),38]),mean(Output.model$q97.5$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),38]),
                               mean(Output.model$q97.5$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),38]),mean(Output.model$q97.5$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),38]))),
                        mean(c(mean(Output.model$q97.5$psi[c(2),c(10,11,12),24]),mean(Output.model$q97.5$psi[c(3),c(10,11,12),24]),mean(Output.model$q97.5$psi[c(4),c(10,11,12),24]),
                               mean(Output.model$q97.5$psi[c(6),c(10,11,12),24]),mean(Output.model$q97.5$psi[c(7),c(10,11,12),24]),mean(Output.model$q97.5$psi[c(8),c(10,11,12),24]),
                               mean(Output.model$q97.5$psi[c(9),c(10,11,12),24]),mean(Output.model$q97.5$psi[c(10),c(10,11,12),24]),mean(Output.model$q97.5$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),24]),
                               mean(Output.model$q97.5$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),24]),mean(Output.model$q97.5$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),24]),mean(Output.model$q97.5$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),24]),
                               mean(Output.model$q97.5$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),24]),mean(Output.model$q97.5$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),24]),mean(Output.model$q97.5$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),24]),
                               mean(Output.model$q97.5$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),24]),mean(Output.model$q97.5$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),24]))),
                        mean(c(mean(Output.model$q97.5$psi[c(2),c(10,11,12),37]),mean(Output.model$q97.5$psi[c(3),c(10,11,12),37]),mean(Output.model$q97.5$psi[c(4),c(10,11,12),37]),
                               mean(Output.model$q97.5$psi[c(6),c(10,11,12),37]),mean(Output.model$q97.5$psi[c(7),c(10,11,12),37]),mean(Output.model$q97.5$psi[c(8),c(10,11,12),37]),
                               mean(Output.model$q97.5$psi[c(9),c(10,11,12),37]),mean(Output.model$q97.5$psi[c(10),c(10,11,12),37]),mean(Output.model$q97.5$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),37]),
                               mean(Output.model$q97.5$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),37]),mean(Output.model$q97.5$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),37]),mean(Output.model$q97.5$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),37]),
                               mean(Output.model$q97.5$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),37]),mean(Output.model$q97.5$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),37]),mean(Output.model$q97.5$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),37]),
                               mean(Output.model$q97.5$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),37]),mean(Output.model$q97.5$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),37]))),
                        mean(c(mean(Output.model$q97.5$psi[c(2),c(10,11,12),13]),mean(Output.model$q97.5$psi[c(3),c(10,11,12),13]),mean(Output.model$q97.5$psi[c(4),c(10,11,12),13]),
                               mean(Output.model$q97.5$psi[c(6),c(10,11,12),13]),mean(Output.model$q97.5$psi[c(7),c(10,11,12),13]),mean(Output.model$q97.5$psi[c(8),c(10,11,12),13]),
                               mean(Output.model$q97.5$psi[c(9),c(10,11,12),13]),mean(Output.model$q97.5$psi[c(10),c(10,11,12),13]),mean(Output.model$q97.5$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),13]),
                               mean(Output.model$q97.5$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),13]),mean(Output.model$q97.5$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),13]),mean(Output.model$q97.5$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),13]),
                               mean(Output.model$q97.5$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),13]),mean(Output.model$q97.5$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),13]),mean(Output.model$q97.5$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),13]),
                               mean(Output.model$q97.5$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),13]),mean(Output.model$q97.5$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),13]))),
                        mean(c(mean(Output.model$q97.5$psi[c(2),c(10,11,12),21]),mean(Output.model$q97.5$psi[c(3),c(10,11,12),21]),mean(Output.model$q97.5$psi[c(4),c(10,11,12),21]),
                               mean(Output.model$q97.5$psi[c(6),c(10,11,12),21]),mean(Output.model$q97.5$psi[c(7),c(10,11,12),21]),mean(Output.model$q97.5$psi[c(8),c(10,11,12),21]),
                               mean(Output.model$q97.5$psi[c(9),c(10,11,12),21]),mean(Output.model$q97.5$psi[c(10),c(10,11,12),21]),mean(Output.model$q97.5$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),21]),
                               mean(Output.model$q97.5$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),21]),mean(Output.model$q97.5$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),21]),mean(Output.model$q97.5$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),21]),
                               mean(Output.model$q97.5$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),21]),mean(Output.model$q97.5$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),21]),mean(Output.model$q97.5$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),21]),
                               mean(Output.model$q97.5$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),21]),mean(Output.model$q97.5$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),21]))),
                        mean(c(mean(Output.model$q97.5$psi[c(2),c(10,11,12),34]),mean(Output.model$q97.5$psi[c(3),c(10,11,12),34]),mean(Output.model$q97.5$psi[c(4),c(10,11,12),34]),
                               mean(Output.model$q97.5$psi[c(6),c(10,11,12),34]),mean(Output.model$q97.5$psi[c(7),c(10,11,12),34]),mean(Output.model$q97.5$psi[c(8),c(10,11,12),34]),
                               mean(Output.model$q97.5$psi[c(9),c(10,11,12),34]),mean(Output.model$q97.5$psi[c(10),c(10,11,12),34]),mean(Output.model$q97.5$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),34]),
                               mean(Output.model$q97.5$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),34]),mean(Output.model$q97.5$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),34]),mean(Output.model$q97.5$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),34]),
                               mean(Output.model$q97.5$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),34]),mean(Output.model$q97.5$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),34]),mean(Output.model$q97.5$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),34]),
                               mean(Output.model$q97.5$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),34]),mean(Output.model$q97.5$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),34]))),
                        mean(c(mean(Output.model$q97.5$psi[c(2),c(10,11,12),2]),mean(Output.model$q97.5$psi[c(3),c(10,11,12),2]),mean(Output.model$q97.5$psi[c(4),c(10,11,12),2]),
                               mean(Output.model$q97.5$psi[c(6),c(10,11,12),2]),mean(Output.model$q97.5$psi[c(7),c(10,11,12),2]),mean(Output.model$q97.5$psi[c(8),c(10,11,12),2]),
                               mean(Output.model$q97.5$psi[c(9),c(10,11,12),2]),mean(Output.model$q97.5$psi[c(10),c(10,11,12),2]),mean(Output.model$q97.5$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),2]),
                               mean(Output.model$q97.5$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),2]),mean(Output.model$q97.5$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),2]),mean(Output.model$q97.5$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),2]),
                               mean(Output.model$q97.5$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),2]),mean(Output.model$q97.5$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),2]),mean(Output.model$q97.5$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),2]),
                               mean(Output.model$q97.5$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),2]),mean(Output.model$q97.5$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),2]))),
                        mean(c(mean(Output.model$q97.5$psi[c(2),c(10,11,12),32]),mean(Output.model$q97.5$psi[c(3),c(10,11,12),32]),mean(Output.model$q97.5$psi[c(4),c(10,11,12),32]),
                               mean(Output.model$q97.5$psi[c(6),c(10,11,12),32]),mean(Output.model$q97.5$psi[c(7),c(10,11,12),32]),mean(Output.model$q97.5$psi[c(8),c(10,11,12),32]),
                               mean(Output.model$q97.5$psi[c(9),c(10,11,12),32]),mean(Output.model$q97.5$psi[c(10),c(10,11,12),32]),mean(Output.model$q97.5$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),32]),
                               mean(Output.model$q97.5$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),32]),mean(Output.model$q97.5$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),32]),mean(Output.model$q97.5$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),32]),
                               mean(Output.model$q97.5$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),32]),mean(Output.model$q97.5$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),32]),mean(Output.model$q97.5$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),32]),
                               mean(Output.model$q97.5$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),32]),mean(Output.model$q97.5$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),32]))),
                        mean(c(mean(Output.model$q97.5$psi[c(2),c(10,11,12),17]),mean(Output.model$q97.5$psi[c(3),c(10,11,12),17]),mean(Output.model$q97.5$psi[c(4),c(10,11,12),17]),
                               mean(Output.model$q97.5$psi[c(6),c(10,11,12),17]),mean(Output.model$q97.5$psi[c(7),c(10,11,12),17]),mean(Output.model$q97.5$psi[c(8),c(10,11,12),17]),
                               mean(Output.model$q97.5$psi[c(9),c(10,11,12),17]),mean(Output.model$q97.5$psi[c(10),c(10,11,12),17]),mean(Output.model$q97.5$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),17]),
                               mean(Output.model$q97.5$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),17]),mean(Output.model$q97.5$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),17]),mean(Output.model$q97.5$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),17]),
                               mean(Output.model$q97.5$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),17]),mean(Output.model$q97.5$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),17]),mean(Output.model$q97.5$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),17]),
                               mean(Output.model$q97.5$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),17]),mean(Output.model$q97.5$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),17]))),
                        mean(c(mean(Output.model$q97.5$psi[c(2),c(10,11,12),4]),mean(Output.model$q97.5$psi[c(3),c(10,11,12),4]),mean(Output.model$q97.5$psi[c(4),c(10,11,12),4]),
                               mean(Output.model$q97.5$psi[c(6),c(10,11,12),4]),mean(Output.model$q97.5$psi[c(7),c(10,11,12),4]),mean(Output.model$q97.5$psi[c(8),c(10,11,12),4]),
                               mean(Output.model$q97.5$psi[c(9),c(10,11,12),4]),mean(Output.model$q97.5$psi[c(10),c(10,11,12),4]),mean(Output.model$q97.5$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),4]),
                               mean(Output.model$q97.5$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),4]),mean(Output.model$q97.5$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),4]),mean(Output.model$q97.5$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),4]),
                               mean(Output.model$q97.5$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),4]),mean(Output.model$q97.5$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),4]),mean(Output.model$q97.5$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),4]),
                               mean(Output.model$q97.5$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),4]),mean(Output.model$q97.5$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),4]))),
                        mean(c(mean(Output.model$q97.5$psi[c(2),c(10,11,12),8]),mean(Output.model$q97.5$psi[c(3),c(10,11,12),8]),mean(Output.model$q97.5$psi[c(4),c(10,11,12),8]),
                               mean(Output.model$q97.5$psi[c(6),c(10,11,12),8]),mean(Output.model$q97.5$psi[c(7),c(10,11,12),8]),mean(Output.model$q97.5$psi[c(8),c(10,11,12),8]),
                               mean(Output.model$q97.5$psi[c(9),c(10,11,12),8]),mean(Output.model$q97.5$psi[c(10),c(10,11,12),8]),mean(Output.model$q97.5$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),8]),
                               mean(Output.model$q97.5$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),8]),mean(Output.model$q97.5$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),8]),mean(Output.model$q97.5$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),8]),
                               mean(Output.model$q97.5$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),8]),mean(Output.model$q97.5$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),8]),mean(Output.model$q97.5$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),8]),
                               mean(Output.model$q97.5$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),8]),mean(Output.model$q97.5$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),8]))),
                        mean(c(mean(Output.model$q97.5$psi[c(2),c(10,11,12),39]),mean(Output.model$q97.5$psi[c(3),c(10,11,12),39]),mean(Output.model$q97.5$psi[c(4),c(10,11,12),39]),
                               mean(Output.model$q97.5$psi[c(6),c(10,11,12),39]),mean(Output.model$q97.5$psi[c(7),c(10,11,12),39]),mean(Output.model$q97.5$psi[c(8),c(10,11,12),39]),
                               mean(Output.model$q97.5$psi[c(9),c(10,11,12),39]),mean(Output.model$q97.5$psi[c(10),c(10,11,12),39]),mean(Output.model$q97.5$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),39]),
                               mean(Output.model$q97.5$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),39]),mean(Output.model$q97.5$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),39]),mean(Output.model$q97.5$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),39]),
                               mean(Output.model$q97.5$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),39]),mean(Output.model$q97.5$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),39]),mean(Output.model$q97.5$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),39]),
                               mean(Output.model$q97.5$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),39]),mean(Output.model$q97.5$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),39]))),
                        mean(c(mean(Output.model$q97.5$psi[c(2),c(10,11,12),10]),mean(Output.model$q97.5$psi[c(3),c(10,11,12),10]),mean(Output.model$q97.5$psi[c(4),c(10,11,12),10]),
                               mean(Output.model$q97.5$psi[c(6),c(10,11,12),10]),mean(Output.model$q97.5$psi[c(7),c(10,11,12),10]),mean(Output.model$q97.5$psi[c(8),c(10,11,12),10]),
                               mean(Output.model$q97.5$psi[c(9),c(10,11,12),10]),mean(Output.model$q97.5$psi[c(10),c(10,11,12),10]),mean(Output.model$q97.5$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),10]),
                               mean(Output.model$q97.5$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),10]),mean(Output.model$q97.5$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),10]),mean(Output.model$q97.5$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),10]),
                               mean(Output.model$q97.5$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),10]),mean(Output.model$q97.5$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),10]),mean(Output.model$q97.5$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),10]),
                               mean(Output.model$q97.5$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),10]),mean(Output.model$q97.5$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),10]))),
                        mean(c(mean(Output.model$q97.5$psi[c(2),c(10,11,12),19]),mean(Output.model$q97.5$psi[c(3),c(10,11,12),19]),mean(Output.model$q97.5$psi[c(4),c(10,11,12),19]),
                               mean(Output.model$q97.5$psi[c(6),c(10,11,12),19]),mean(Output.model$q97.5$psi[c(7),c(10,11,12),19]),mean(Output.model$q97.5$psi[c(8),c(10,11,12),19]),
                               mean(Output.model$q97.5$psi[c(9),c(10,11,12),19]),mean(Output.model$q97.5$psi[c(10),c(10,11,12),19]),mean(Output.model$q97.5$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),19]),
                               mean(Output.model$q97.5$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),19]),mean(Output.model$q97.5$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),19]),mean(Output.model$q97.5$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),19]),
                               mean(Output.model$q97.5$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),19]),mean(Output.model$q97.5$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),19]),mean(Output.model$q97.5$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),19]),
                               mean(Output.model$q97.5$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),19]),mean(Output.model$q97.5$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),19]))),
                        mean(c(mean(Output.model$q97.5$psi[c(2),c(10,11,12),35]),mean(Output.model$q97.5$psi[c(3),c(10,11,12),35]),mean(Output.model$q97.5$psi[c(4),c(10,11,12),35]),
                               mean(Output.model$q97.5$psi[c(6),c(10,11,12),35]),mean(Output.model$q97.5$psi[c(7),c(10,11,12),35]),mean(Output.model$q97.5$psi[c(8),c(10,11,12),35]),
                               mean(Output.model$q97.5$psi[c(9),c(10,11,12),35]),mean(Output.model$q97.5$psi[c(10),c(10,11,12),35]),mean(Output.model$q97.5$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),35]),
                               mean(Output.model$q97.5$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),35]),mean(Output.model$q97.5$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),35]),mean(Output.model$q97.5$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),35]),
                               mean(Output.model$q97.5$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),35]),mean(Output.model$q97.5$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),35]),mean(Output.model$q97.5$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),35]),
                               mean(Output.model$q97.5$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),35]),mean(Output.model$q97.5$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),35]))),
                        mean(c(mean(Output.model$q97.5$psi[c(2),c(10,11,12),25]),mean(Output.model$q97.5$psi[c(3),c(10,11,12),25]),mean(Output.model$q97.5$psi[c(4),c(10,11,12),25]),
                               mean(Output.model$q97.5$psi[c(6),c(10,11,12),25]),mean(Output.model$q97.5$psi[c(7),c(10,11,12),25]),mean(Output.model$q97.5$psi[c(8),c(10,11,12),25]),
                               mean(Output.model$q97.5$psi[c(9),c(10,11,12),25]),mean(Output.model$q97.5$psi[c(10),c(10,11,12),25]),mean(Output.model$q97.5$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),25]),
                               mean(Output.model$q97.5$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),25]),mean(Output.model$q97.5$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),25]),mean(Output.model$q97.5$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),25]),
                               mean(Output.model$q97.5$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),25]),mean(Output.model$q97.5$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),25]),mean(Output.model$q97.5$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),25]),
                               mean(Output.model$q97.5$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),25]),mean(Output.model$q97.5$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),25]))),
                        mean(c(mean(Output.model$q97.5$psi[c(2),c(10,11,12),28]),mean(Output.model$q97.5$psi[c(3),c(10,11,12),28]),mean(Output.model$q97.5$psi[c(4),c(10,11,12),28]),
                               mean(Output.model$q97.5$psi[c(6),c(10,11,12),28]),mean(Output.model$q97.5$psi[c(7),c(10,11,12),28]),mean(Output.model$q97.5$psi[c(8),c(10,11,12),28]),
                               mean(Output.model$q97.5$psi[c(9),c(10,11,12),28]),mean(Output.model$q97.5$psi[c(10),c(10,11,12),28]),mean(Output.model$q97.5$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),28]),
                               mean(Output.model$q97.5$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),28]),mean(Output.model$q97.5$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),28]),mean(Output.model$q97.5$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),28]),
                               mean(Output.model$q97.5$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),28]),mean(Output.model$q97.5$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),28]),mean(Output.model$q97.5$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),28]),
                               mean(Output.model$q97.5$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),28]),mean(Output.model$q97.5$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),28]))),
                        mean(c(mean(Output.model$q97.5$psi[c(2),c(10,11,12),46]),mean(Output.model$q97.5$psi[c(3),c(10,11,12),46]),mean(Output.model$q97.5$psi[c(4),c(10,11,12),46]),
                               mean(Output.model$q97.5$psi[c(6),c(10,11,12),46]),mean(Output.model$q97.5$psi[c(7),c(10,11,12),46]),mean(Output.model$q97.5$psi[c(8),c(10,11,12),46]),
                               mean(Output.model$q97.5$psi[c(9),c(10,11,12),46]),mean(Output.model$q97.5$psi[c(10),c(10,11,12),46]),mean(Output.model$q97.5$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),46]),
                               mean(Output.model$q97.5$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),46]),mean(Output.model$q97.5$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),46]),mean(Output.model$q97.5$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),46]),
                               mean(Output.model$q97.5$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),46]),mean(Output.model$q97.5$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),46]),mean(Output.model$q97.5$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),46]),
                               mean(Output.model$q97.5$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),46]),mean(Output.model$q97.5$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),46]))),
                        mean(c(mean(Output.model$q97.5$psi[c(2),c(10,11,12),41]),mean(Output.model$q97.5$psi[c(3),c(10,11,12),41]),mean(Output.model$q97.5$psi[c(4),c(10,11,12),41]),
                               mean(Output.model$q97.5$psi[c(6),c(10,11,12),41]),mean(Output.model$q97.5$psi[c(7),c(10,11,12),41]),mean(Output.model$q97.5$psi[c(8),c(10,11,12),41]),
                               mean(Output.model$q97.5$psi[c(9),c(10,11,12),41]),mean(Output.model$q97.5$psi[c(10),c(10,11,12),41]),mean(Output.model$q97.5$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),41]),
                               mean(Output.model$q97.5$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),41]),mean(Output.model$q97.5$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),41]),mean(Output.model$q97.5$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),41]),
                               mean(Output.model$q97.5$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),41]),mean(Output.model$q97.5$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),41]),mean(Output.model$q97.5$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),41]),
                               mean(Output.model$q97.5$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),41]),mean(Output.model$q97.5$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),41]))),
                        mean(c(mean(Output.model$q97.5$psi[c(2),c(10,11,12),16]),mean(Output.model$q97.5$psi[c(3),c(10,11,12),16]),mean(Output.model$q97.5$psi[c(4),c(10,11,12),16]),
                               mean(Output.model$q97.5$psi[c(6),c(10,11,12),16]),mean(Output.model$q97.5$psi[c(7),c(10,11,12),16]),mean(Output.model$q97.5$psi[c(8),c(10,11,12),16]),
                               mean(Output.model$q97.5$psi[c(9),c(10,11,12),16]),mean(Output.model$q97.5$psi[c(10),c(10,11,12),16]),mean(Output.model$q97.5$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),16]),
                               mean(Output.model$q97.5$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),16]),mean(Output.model$q97.5$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),16]),mean(Output.model$q97.5$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),16]),
                               mean(Output.model$q97.5$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),16]),mean(Output.model$q97.5$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),16]),mean(Output.model$q97.5$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),16]),
                               mean(Output.model$q97.5$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),16]),mean(Output.model$q97.5$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),16]))),
                        mean(c(mean(Output.model$q97.5$psi[c(2),c(10,11,12),36]),mean(Output.model$q97.5$psi[c(3),c(10,11,12),36]),mean(Output.model$q97.5$psi[c(4),c(10,11,12),36]),
                               mean(Output.model$q97.5$psi[c(6),c(10,11,12),36]),mean(Output.model$q97.5$psi[c(7),c(10,11,12),36]),mean(Output.model$q97.5$psi[c(8),c(10,11,12),36]),
                               mean(Output.model$q97.5$psi[c(9),c(10,11,12),36]),mean(Output.model$q97.5$psi[c(10),c(10,11,12),36]),mean(Output.model$q97.5$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),36]),
                               mean(Output.model$q97.5$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),36]),mean(Output.model$q97.5$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),36]),mean(Output.model$q97.5$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),36]),
                               mean(Output.model$q97.5$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),36]),mean(Output.model$q97.5$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),36]),mean(Output.model$q97.5$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),36]),
                               mean(Output.model$q97.5$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),36]),mean(Output.model$q97.5$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),36]))),
                        mean(c(mean(Output.model$q97.5$psi[c(2),c(10,11,12),14]),mean(Output.model$q97.5$psi[c(3),c(10,11,12),14]),mean(Output.model$q97.5$psi[c(4),c(10,11,12),14]),
                               mean(Output.model$q97.5$psi[c(6),c(10,11,12),14]),mean(Output.model$q97.5$psi[c(7),c(10,11,12),14]),mean(Output.model$q97.5$psi[c(8),c(10,11,12),14]),
                               mean(Output.model$q97.5$psi[c(9),c(10,11,12),14]),mean(Output.model$q97.5$psi[c(10),c(10,11,12),14]),mean(Output.model$q97.5$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),14]),
                               mean(Output.model$q97.5$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),14]),mean(Output.model$q97.5$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),14]),mean(Output.model$q97.5$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),14]),
                               mean(Output.model$q97.5$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),14]),mean(Output.model$q97.5$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),14]),mean(Output.model$q97.5$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),14]),
                               mean(Output.model$q97.5$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),14]),mean(Output.model$q97.5$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),14]))),
                        mean(c(mean(Output.model$q97.5$psi[c(2),c(10,11,12),11]),mean(Output.model$q97.5$psi[c(3),c(10,11,12),11]),mean(Output.model$q97.5$psi[c(4),c(10,11,12),11]),
                               mean(Output.model$q97.5$psi[c(6),c(10,11,12),11]),mean(Output.model$q97.5$psi[c(7),c(10,11,12),11]),mean(Output.model$q97.5$psi[c(8),c(10,11,12),11]),
                               mean(Output.model$q97.5$psi[c(9),c(10,11,12),11]),mean(Output.model$q97.5$psi[c(10),c(10,11,12),11]),mean(Output.model$q97.5$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),11]),
                               mean(Output.model$q97.5$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),11]),mean(Output.model$q97.5$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),11]),mean(Output.model$q97.5$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),11]),
                               mean(Output.model$q97.5$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),11]),mean(Output.model$q97.5$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),11]),mean(Output.model$q97.5$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),11]),
                               mean(Output.model$q97.5$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),11]),mean(Output.model$q97.5$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),11]))),
                        mean(c(mean(Output.model$q97.5$psi[c(2),c(10,11,12),31]),mean(Output.model$q97.5$psi[c(3),c(10,11,12),31]),mean(Output.model$q97.5$psi[c(4),c(10,11,12),31]),
                               mean(Output.model$q97.5$psi[c(6),c(10,11,12),31]),mean(Output.model$q97.5$psi[c(7),c(10,11,12),31]),mean(Output.model$q97.5$psi[c(8),c(10,11,12),31]),
                               mean(Output.model$q97.5$psi[c(9),c(10,11,12),31]),mean(Output.model$q97.5$psi[c(10),c(10,11,12),31]),mean(Output.model$q97.5$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),31]),
                               mean(Output.model$q97.5$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),31]),mean(Output.model$q97.5$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),31]),mean(Output.model$q97.5$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),31]),
                               mean(Output.model$q97.5$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),31]),mean(Output.model$q97.5$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),31]),mean(Output.model$q97.5$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),31]),
                               mean(Output.model$q97.5$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),31]),mean(Output.model$q97.5$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),31]))),
                        mean(c(mean(Output.model$q97.5$psi[c(2),c(10,11,12),6]),mean(Output.model$q97.5$psi[c(3),c(10,11,12),6]),mean(Output.model$q97.5$psi[c(4),c(10,11,12),6]),
                               mean(Output.model$q97.5$psi[c(6),c(10,11,12),6]),mean(Output.model$q97.5$psi[c(7),c(10,11,12),6]),mean(Output.model$q97.5$psi[c(8),c(10,11,12),6]),
                               mean(Output.model$q97.5$psi[c(9),c(10,11,12),6]),mean(Output.model$q97.5$psi[c(10),c(10,11,12),6]),mean(Output.model$q97.5$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),6]),
                               mean(Output.model$q97.5$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),6]),mean(Output.model$q97.5$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),6]),mean(Output.model$q97.5$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),6]),
                               mean(Output.model$q97.5$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),6]),mean(Output.model$q97.5$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),6]),mean(Output.model$q97.5$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),6]),
                               mean(Output.model$q97.5$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),6]),mean(Output.model$q97.5$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),6])))))

valuesS1.2 <- data.frame(sppnamesS1, alphaS1.2.val)
colnames(valuesS1.2) <- c("species", "lower.alpha", "l25.alpha", "mean.alpha", "u75.alpha", "upper.alpha")

valuesS1.2$species <- factor(valuesS1.2$species, levels = valuesS1.2$species)

valuesS1.2$nodiff <- valuesS1.2$l25.alpha < 0 & valuesS1.2$u75.alpha > 0
valuesS1.2$H95 <- valuesS1.2$lower.alpha > 0
valuesS1.2$H50 <- valuesS1.2$lower.alpha < 0 & valuesS1.2$l25.alpha > 0
valuesS1.2$L95 <- valuesS1.2$upper.alpha < 0
valuesS1.2$L50 <- valuesS1.2$upper.alpha > 0 & valuesS1.2$u75.alpha < 0

# Black and white
#Figure
FigureA.1_after_BW <- ggplot() +
  geom_hline(yintercept = 0.5, alpha = 0.75, linetype = "dotdash", color = "grey") +
  geom_vline(xintercept = 15.5, linetype = "dotted", size = 0.6) +
  geom_vline(xintercept = 24.5, linetype = "dotted", size = 0.6) +
  geom_vline(xintercept = 25.5, linetype = "dotted", size = 0.6) +
  geom_vline(xintercept = 28.5, linetype = "dotted", size = 0.6) +
  geom_vline(xintercept = 30.5, linetype = "dotted", size = 0.6) +
  geom_vline(xintercept = 32.5, linetype = "dotted", size = 0.6) +
  geom_errorbar(data = subset(valuesS1.2, H95 == TRUE), aes(x = species, ymin = lower.alpha, ymax = upper.alpha, color = "gray55"),
                size = 0.75, width = 0) +
  geom_errorbar(data = subset(valuesS1.2, H95 == TRUE), aes(x = species, ymin = l25.alpha, ymax = u75.alpha, color = "gray55"),
                size = 2, width = 0) +
  geom_errorbar(data = subset(valuesS1.2, H95 == TRUE), aes(x = species, ymin = mean.alpha, ymax = mean.alpha, color = "black"),
                width = 0.5) +
  coord_cartesian(ylim = c(-0.03, 1.03)) +
  annotate("text", y = 1, x = 8, label = "(Farmland)", size=3, color="black", fontface = "bold") +
  annotate("text", y = 1, x = 20, label = "(Shrubland)", size=3, color="black", fontface = "bold") +
  annotate("text", y = 1, x = 25, label = "(R)", size=3, color="black", fontface = "bold") +
  annotate("text", y = 1, x = 27, label = "(Fo)", size=3, color="black", fontface = "bold") +
  annotate("text", y = 1, x = 29.5, label = "(W)", size=3, color="black", fontface = "bold") +
  annotate("text", y = 1, x = 31.5, label = "(U)", size=3, color="black", fontface = "bold") +
  #annotate("text", y = 1.7, x = 29, label = "(Other habitats)", size=3, color="black", fontface = "bold") +
  annotate("text", y = 1, x = 40.5, label = "(Non-specialist)", size=3, color="black", fontface = "bold") +
  scale_color_manual(name = "", values = c("gray55" = "gray55", "black" = "black")) +
  scale_x_discrete(labels = c("Little bustard","Red-legged partridge","Common quail","Greater short-toed lark",
                              "Crested lark","Calandra lark","Corn bunting","Black-eared wheatear","Zitting cisticola",
                              "European goldfinch","Eurasian hoopoe","European bee-eater","European turtle-dove","Little owl",
                              "Common kestrel","Thekla's lark","Tawny pipit","Woodchat shrike","Iberian grey shrike",
                              "Dartford warbler","Subalpine warbler","Sardinian warbler","European stonechat","Melodious warbler",
                              "Red-billed chough","Common wood pigeon","Common chaffinch","Great tit","Mallard",
                              "White wagtail","Common house martin","Common swift","Common linnet","European serin",
                              "European greenfinch","Cirl bunting","House sparrow","Barn swallow","Common nightingale",
                              "Common blackbird","Spotless starling","Common cuckoo","Magpie","Carrion crow",
                              "Western marsh harrier","Black kite","Common buzzard")) +
  theme_few() +
  ylab("Occurrence probability") +
  xlab("") +
  theme(plot.margin = unit(c(0.5, 0.5, -0.25, 0.4), "cm"),
        text = element_text(size = 11),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, color = "black"),
        axis.title.y = element_text(size=11, vjust = 2, face="bold"),
        legend.position = "none")

FigureA.1_after_BW # 6.8 x 3.45

FigureA.1=plot_grid(FigureA.1_before_BW, FigureA.1_after_BW, ncol = 1, nrow = 2, 
                   labels = c('(a)', '(b)'),label_size = 10,
                   label_y = c(1, 1), label_x = c(0.05, 0.05))

FigureA.1 # 6.8 x 6.9

####################################
############ Figure A.2 #############
## Effect of arable surface graph ##

alphaS2.val <- cbind(c(Output.model$q2.5$a1[c(45,1,15,7,22,29,18,33,12,9,47,30,40,5,20,23,3,27,26,44,42,43,38,24,37,13,21,34,2,32,17,4,8,39,10,19,35,25,28,46,41,16,36,14,11,31,6)]),
                     c(Output.model$q25$a1[c(45,1,15,7,22,29,18,33,12,9,47,30,40,5,20,23,3,27,26,44,42,43,38,24,37,13,21,34,2,32,17,4,8,39,10,19,35,25,28,46,41,16,36,14,11,31,6)]),
                     c(Output.model$mean$a1[c(45,1,15,7,22,29,18,33,12,9,47,30,40,5,20,23,3,27,26,44,42,43,38,24,37,13,21,34,2,32,17,4,8,39,10,19,35,25,28,46,41,16,36,14,11,31,6)]),
                     c(Output.model$q75$a1[c(45,1,15,7,22,29,18,33,12,9,47,30,40,5,20,23,3,27,26,44,42,43,38,24,37,13,21,34,2,32,17,4,8,39,10,19,35,25,28,46,41,16,36,14,11,31,6)]),
                     c(Output.model$q97.5$a1[c(45,1,15,7,22,29,18,33,12,9,47,30,40,5,20,23,3,27,26,44,42,43,38,24,37,13,21,34,2,32,17,4,8,39,10,19,35,25,28,46,41,16,36,14,11,31,6)]),
                     c(Output.model$overlap0$a1[c(45,1,15,7,22,29,18,33,12,9,47,30,40,5,20,23,3,27,26,44,42,43,38,24,37,13,21,34,2,32,17,4,8,39,10,19,35,25,28,46,41,16,36,14,11,31,6)]))


sppnamesS2 <- c("10", "11", "12", "13", "14",
                "15", "16", "17", "18", "19",
                "20", "21", "22", "23", "24",
                "25", "26", "27", "28", "29",
                "30", "31", "32", "33", "34", 
                "35", "36", "37", "38", "39",
                "40", "41", "42", "43", "44",
                "45", "46", "47", "48", "49",
                "50", "51", "52", "53", "54",
                "55", "56")

valuesS2 <- data.frame(sppnamesS2, alphaS2.val)
colnames(valuesS2) <- c("species", "lower.alpha", "l25.alpha", "mean.alpha", "u75.alpha", "upper.alpha", "alpha.sig")

valuesS2$species <- factor(valuesS2$species, levels = valuesS2$species)

valuesS2$nodiff <- valuesS2$l25.alpha < 0 & valuesS2$u75.alpha > 0
valuesS2$H95 <- valuesS2$lower.alpha > 0
valuesS2$H50 <- valuesS2$lower.alpha < 0 & valuesS2$l25.alpha > 0
valuesS2$L95 <- valuesS2$upper.alpha < 0
valuesS2$L50 <- valuesS2$upper.alpha > 0 & valuesS2$u75.alpha < 0

#Figure
FigureA.2 <- ggplot() +
  geom_hline(yintercept = 0, alpha = 0.75, linetype = "dotdash") +
  geom_vline(xintercept = 15.5, linetype = "dotted", size = 0.6) +
  geom_vline(xintercept = 24.5, linetype = "dotted", size = 0.6) +
  geom_vline(xintercept = 25.5, linetype = "dotted", size = 0.6) +
  geom_vline(xintercept = 28.5, linetype = "dotted", size = 0.6) +
  geom_vline(xintercept = 30.5, linetype = "dotted", size = 0.6) +
  geom_vline(xintercept = 32.5, linetype = "dotted", size = 0.6) +
  geom_errorbar(data = subset(valuesS2, nodiff == TRUE), aes(x = species, ymin = lower.alpha, ymax = upper.alpha, color = "gray80"), 
                width = 0, size = 0.75) +
  geom_errorbar(data = subset(valuesS2, nodiff == TRUE), aes(x = species, ymin = l25.alpha, ymax = u75.alpha, color = "gray80"), 
                width = 0, size = 2) +
  geom_errorbar(data = subset(valuesS2, nodiff == TRUE), aes(x = species, ymin = mean.alpha, ymax = mean.alpha, color = "black"), 
                width = 0.5) +
  geom_errorbar(data = subset(valuesS2, H50 == TRUE), aes(x = species, ymin = lower.alpha, ymax = upper.alpha, color = "gray60"), 
                size = 0.75, width = 0) +
  geom_errorbar(data = subset(valuesS2, H50 == TRUE), aes(x = species, ymin = l25.alpha, ymax = u75.alpha, color = "gray60"), 
                size = 2, width = 0) +
  geom_errorbar(data = subset(valuesS2, H50 == TRUE), aes(x = species, ymin = mean.alpha, ymax = mean.alpha, color = "black"), 
                width = 0.5) +
  geom_errorbar(data = subset(valuesS2, L50 == TRUE), aes(x = species, ymin = lower.alpha, ymax = upper.alpha, color = "gray60"), 
                size = 0.75, width = 0) +
  geom_errorbar(data = subset(valuesS2, L50 == TRUE), aes(x = species, ymin = l25.alpha, ymax = u75.alpha, color = "gray60"), 
                size = 2, width = 0) +
  geom_errorbar(data = subset(valuesS2, L50 == TRUE), aes(x = species, ymin = mean.alpha, ymax = mean.alpha, color = "black"), 
                width = 0.5) +
  geom_errorbar(data = subset(valuesS2, H95 == TRUE), aes(x = species, ymin = lower.alpha, ymax = upper.alpha, color = "brown"),
                size = 0.75, width = 0) +
  geom_errorbar(data = subset(valuesS2, H95 == TRUE), aes(x = species, ymin = l25.alpha, ymax = u75.alpha, color = "brown"),
                size = 2, width = 0) +
  geom_errorbar(data = subset(valuesS2, H95 == TRUE), aes(x = species, ymin = mean.alpha, ymax = mean.alpha, color = "black"),
                width = 0.5) +
  geom_errorbar(data = subset(valuesS2, L95 == TRUE), aes(x = species, ymin = lower.alpha, ymax = upper.alpha, color = "brown"),
                size = 0.75, width = 0) +
  geom_errorbar(data = subset(valuesS2, L95 == TRUE), aes(x = species, ymin = l25.alpha, ymax = u75.alpha, color = "brown"),
                size = 2, width = 0) +
  geom_errorbar(data = subset(valuesS2, L95 == TRUE), aes(x = species, ymin = mean.alpha, ymax = mean.alpha, color = "black"),
                width = 0.5) +
  coord_cartesian(ylim = c(-4, 4)) +
  annotate("text", y = 3.7, x = 8, label = "(Farmland)", size=3, color="black", fontface = "bold") +
  annotate("text", y = 3.7, x = 20, label = "(Shrubland)", size=3, color="black", fontface = "bold") +
  annotate("text", y = 3.7, x = 25, label = "(R)", size=3, color="black", fontface = "bold") +
  annotate("text", y = 3.7, x = 27, label = "(Fo)", size=3, color="black", fontface = "bold") +
  annotate("text", y = 3.7, x = 29.5, label = "(W)", size=3, color="black", fontface = "bold") +
  annotate("text", y = 3.7, x = 31.5, label = "(U)", size=3, color="black", fontface = "bold") +
  annotate("text", y = 3.7, x = 40.5, label = "(Non-specialist)", size=3, color="black", fontface = "bold") +
  scale_color_manual(name = "", values = c("brown" = "chocolate3", "gray60" = "gray55", "black" = "black", "gray80" = "gray80")) +
  scale_x_discrete(labels = c("Little bustard","Red-legged partridge","Common quail","Greater short-toed lark",
                              "Crested lark","Calandra lark","Corn bunting","Black-eared wheatear","Zitting cisticola",
                              "European goldfinch","Eurasian hoopoe","European bee-eater","European turtle-dove","Little owl",
                              "Common kestrel","Thekla's lark","Tawny pipit","Woodchat shrike","Iberian grey shrike",
                              "Dartford warbler","Subalpine warbler","Sardinian warbler","European stonechat","Melodious warbler",
                              "Red-billed chough","Common wood pigeon","Common chaffinch","Great tit","Mallard",
                              "White wagtail","Common house martin","Common swift","Common linnet","European serin",
                              "European greenfinch","Cirl bunting","House sparrow","Barn swallow","Common nightingale",
                              "Common blackbird","Spotless starling","Common cuckoo","Magpie","Carrion crow",
                              "Western marsh harrier","Black kite","Common buzzard")) +
  theme_few() +
  ylab("Effect in occurrence") +
  xlab("") +
  theme(plot.margin = unit(c(0.5, 0.5, -0.25, 0.4), "cm"),
        text = element_text(size = 11),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, color = "black"),
        axis.title.y = element_text(size=11, vjust = 2, face="bold"),
        axis.ticks.x = element_line(size= 1.2, color = c("gray55","gray80","gray80","gray55","chocolate3","chocolate3","gray80","gray55","gray55",
                                                         "gray55","gray55","chocolate3","chocolate3","gray80","gray55","chocolate3","gray55","gray55","gray80",
                                                         "chocolate3","gray55","chocolate3","gray80","chocolate3","gray55","chocolate3","chocolate3","chocolate3","gray80",
                                                         "gray80","gray80","gray55","gray80","gray55","chocolate3","chocolate3","gray55","gray80","chocolate3","chocolate3",
                                                         "gray80","gray55","gray80","gray80","gray55","gray80","gray80")),
        legend.position = "none")

FigureA.2 # 6.8 x 3.45

####################################
############ Figure A.3 #############
## Effect of hour and date on detection probability ##
###
# Hour
bdat_hour <- read.table(file = "Hour.txt", header = TRUE) 
dd_hour <- melt(bdat_hour, id=c("site", "year", "rep", "specie"), na.rm=TRUE)
dd_hour <- melt(bdat_hour, id = 1:4, na.rm=TRUE)
yH2 <- cast(dd_hour, site ~ rep ~ year ~ specie)
dimnames(yH2)=NULL

mcmc.sample <- Output.model$mcmc.info$n.samples
mean.hour <- mean(yH2, na.rm=T)
sd.hour <- sd(yH2, na.rm=T)
unstd.hour.pred <- yH2[,,,1]
hour.pred <- (unstd.hour.pred - mean.hour)/sd.hour
p.pred.hour <- plogis(Output.model$mean$mu.b0 + Output.model$mean$mu.b1*hour.pred)

array.p.pred.hour <- array(NA, dim=c(length(hour.pred), mcmc.sample))
for(i in 1:mcmc.sample){
  array.p.pred.hour[,i] <- plogis(Output.model$sims.list$mu.b0[i] + Output.model$sims.list$mu.b1[i]*hour.pred)
}
sub.set.H <- sort(sample(1:mcmc.sample, size=2000))

LPB_H <- apply(array.p.pred.hour, 1, quantile, probs=0.025)
UPB_H <- apply(array.p.pred.hour, 1, quantile, probs=0.975)
LPB50_H <- apply(array.p.pred.hour, 1, quantile, probs=0.25)
UPB50_H <- apply(array.p.pred.hour, 1, quantile, probs=0.75)  

globalPsH <- data.frame(hour = rep(unstd.hour.pred, times = 47), 
                        pred = rep(p.pred.hour, times = 47), 
                        L = LPB_H, 
                        U = UPB_H,
                        L50 = LPB50_H,
                        U50 = UPB50_H)

#for species-level estimates
detH.int <- Output.model$mean$b0[1:47]
detH.sl1 <- Output.model$mean$b1[1:47]

array.spp.p.pred.hour <- array(NA, dim=c(length(hour.pred), 47))
for(i in 1:(47)){
  array.spp.p.pred.hour[,i] <- plogis(detH.int[i] + detH.sl1[i]*hour.pred)
}

Spp <- data.frame(spp=seq(1:47), Spp=c("Alectoris rufa", "Anas platyrhynchos", "Anthus campestris", "Apus apus", "Athene noctua",
                                       "Buteo buteo", "Calandrella brachydactyla", "Carduelis cannabina", "Carduelis carduelis", 
                                       "Chloris chloris", "Circus aeruginosus", "Cisticola juncidis", "Columba palumbus",
                                       "Corvus corone", "Coturnix coturnix", "Cuculus canorus", "Delichon urbicum", "Emberiza calandra",
                                       "Emberiza cirlus", "Falco tinnunculus", "Fringilla coelebs", "Galerida cristata", "Galerida theklae",
                                       "Hippolais polyglotta", "Hirundo rustica", "Lanius meridionalis", "Lanius senator",
                                       "Luscinia megarhynchos", "Melanocorypha calandra", "Merops apiaster", "Milvus migrans", "Motacilla alba",
                                       "Oenanthe hispanica", "Parus major", "Passer domesticus", "Pica pica", "Pyrrhocorax pyrrhocorax",
                                       "Saxicola rubicola", "Serinus serinus", "Streptopelia turtur", "Sturnus unicolor", "Sylvia cantillans",
                                       "Sylvia melanocephala", "Sylvia undata", "Tetrax tetrax", "Turdus merula", "Upupa epops"))

sppPsH <- data.frame(hour = rep(unstd.hour.pred, times = 47),
                     pred = as.vector(array.spp.p.pred.hour),
                     spp = rep(Spp$spp, each = length(unstd.hour.pred)),
                     Spp = rep(Spp$Spp, each = length(unstd.hour.pred))
)
dH1 <- sppPsH[sppPsH$spp == 1,]
dH2 <- sppPsH[sppPsH$spp == 2,]
dH3 <- sppPsH[sppPsH$spp == 3,]
dH4 <- sppPsH[sppPsH$spp == 4,]
dH5 <- sppPsH[sppPsH$spp == 5,]
dH6 <- sppPsH[sppPsH$spp == 6,]
dH7 <- sppPsH[sppPsH$spp == 7,]
dH8 <- sppPsH[sppPsH$spp == 8,]
dH9 <- sppPsH[sppPsH$spp == 9,]
dH10 <- sppPsH[sppPsH$spp == 10,]
dH11 <- sppPsH[sppPsH$spp == 11,]
dH12 <- sppPsH[sppPsH$spp == 12,]
dH13 <- sppPsH[sppPsH$spp == 13,]
dH14 <- sppPsH[sppPsH$spp == 14,]
dH15 <- sppPsH[sppPsH$spp == 15,]
dH16 <- sppPsH[sppPsH$spp == 16,]
dH17 <- sppPsH[sppPsH$spp == 17,]
dH18 <- sppPsH[sppPsH$spp == 18,]
dH19 <- sppPsH[sppPsH$spp == 19,]
dH20 <- sppPsH[sppPsH$spp == 20,]
dH21 <- sppPsH[sppPsH$spp == 21,]
dH22 <- sppPsH[sppPsH$spp == 22,]
dH23 <- sppPsH[sppPsH$spp == 23,]
dH24 <- sppPsH[sppPsH$spp == 24,]
dH25 <- sppPsH[sppPsH$spp == 25,]
dH26 <- sppPsH[sppPsH$spp == 26,]
dH27 <- sppPsH[sppPsH$spp == 27,]
dH28 <- sppPsH[sppPsH$spp == 28,]
dH29 <- sppPsH[sppPsH$spp == 29,]
dH30 <- sppPsH[sppPsH$spp == 30,]
dH31 <- sppPsH[sppPsH$spp == 31,]
dH32 <- sppPsH[sppPsH$spp == 32,]
dH33 <- sppPsH[sppPsH$spp == 33,]
dH34 <- sppPsH[sppPsH$spp == 34,]
dH35 <- sppPsH[sppPsH$spp == 35,]
dH36 <- sppPsH[sppPsH$spp == 36,]
dH37 <- sppPsH[sppPsH$spp == 37,]
dH38 <- sppPsH[sppPsH$spp == 38,]
dH39 <- sppPsH[sppPsH$spp == 39,]
dH40 <- sppPsH[sppPsH$spp == 40,]
dH41 <- sppPsH[sppPsH$spp == 41,]
dH42 <- sppPsH[sppPsH$spp == 42,]
dH43 <- sppPsH[sppPsH$spp == 43,]
dH44 <- sppPsH[sppPsH$spp == 44,]
dH45 <- sppPsH[sppPsH$spp == 45,]
dH46 <- sppPsH[sppPsH$spp == 46,]
dH47 <- sppPsH[sppPsH$spp == 47,]

plot_hour <- ggplot() +
  scale_x_continuous(breaks = c(0.2916667,0.3333333,0.3750000,0.4166667,0.4583333) ,labels=c('7:00','8:00','9:00','10:00','11:00')) +
  ylab('Detection Probability') + xlab('Hour') + ylim(c(0,1)) + 
  theme_bw() + theme_few()+
  geom_smooth(aes(x = hour, y = pred, color = '1'), se = F, dH1)+
  geom_smooth(aes(x = hour, y = pred, color = '1'), se = F, dH2)+
  geom_smooth(aes(x = hour, y = pred, color = '1'), se = F, dH3)+
  geom_smooth(aes(x = hour, y = pred, color = '1'), se = F, dH4)+
  geom_smooth(aes(x = hour, y = pred, color = '1'), se = F, dH5)+
  geom_smooth(aes(x = hour, y = pred, color = '1'), se = F, dH6)+
  geom_smooth(aes(x = hour, y = pred, color = '1'), se = F, dH7)+
  geom_smooth(aes(x = hour, y = pred, color = '1'), se = F, dH8)+
  geom_smooth(aes(x = hour, y = pred, color = '1'), se = F, dH9)+
  geom_smooth(aes(x = hour, y = pred, color = '1'), se = F, dH10)+
  geom_smooth(aes(x = hour, y = pred, color = '1'), se = F, dH11)+
  geom_smooth(aes(x = hour, y = pred, color = '1'), se = F, dH12)+
  geom_smooth(aes(x = hour, y = pred, color = '1'), se = F, dH13)+
  geom_smooth(aes(x = hour, y = pred, color = '1'), se = F, dH14)+
  geom_smooth(aes(x = hour, y = pred, color = '1'), se = F, dH15)+
  geom_smooth(aes(x = hour, y = pred, color = '1'), se = F, dH16)+
  geom_smooth(aes(x = hour, y = pred, color = '1'), se = F, dH17)+
  geom_smooth(aes(x = hour, y = pred, color = '1'), se = F, dH18)+
  geom_smooth(aes(x = hour, y = pred, color = '1'), se = F, dH19)+
  geom_smooth(aes(x = hour, y = pred, color = '1'), se = F, dH20)+
  geom_smooth(aes(x = hour, y = pred, color = '1'), se = F, dH21)+
  geom_smooth(aes(x = hour, y = pred, color = '1'), se = F, dH22)+
  geom_smooth(aes(x = hour, y = pred, color = '1'), se = F, dH23)+
  geom_smooth(aes(x = hour, y = pred, color = '1'), se = F, dH24)+
  geom_smooth(aes(x = hour, y = pred, color = '1'), se = F, dH25)+
  geom_smooth(aes(x = hour, y = pred, color = '1'), se = F, dH26)+
  geom_smooth(aes(x = hour, y = pred, color = '1'), se = F, dH27)+
  geom_smooth(aes(x = hour, y = pred, color = '1'), se = F, dH28)+
  geom_smooth(aes(x = hour, y = pred, color = '1'), se = F, dH29)+
  geom_smooth(aes(x = hour, y = pred, color = '1'), se = F, dH30)+
  geom_smooth(aes(x = hour, y = pred, color = '1'), se = F, dH31)+
  geom_smooth(aes(x = hour, y = pred, color = '1'), se = F, dH32)+
  geom_smooth(aes(x = hour, y = pred, color = '1'), se = F, dH33)+
  geom_smooth(aes(x = hour, y = pred, color = '1'), se = F, dH34)+
  geom_smooth(aes(x = hour, y = pred, color = '1'), se = F, dH35)+
  geom_smooth(aes(x = hour, y = pred, color = '1'), se = F, dH36)+
  geom_smooth(aes(x = hour, y = pred, color = '1'), se = F, dH37)+
  geom_smooth(aes(x = hour, y = pred, color = '1'), se = F, dH38)+
  geom_smooth(aes(x = hour, y = pred, color = '1'), se = F, dH39)+
  geom_smooth(aes(x = hour, y = pred, color = '1'), se = F, dH40)+
  geom_smooth(aes(x = hour, y = pred, color = '1'), se = F, dH41)+
  geom_smooth(aes(x = hour, y = pred, color = '1'), se = F, dH42)+
  geom_smooth(aes(x = hour, y = pred, color = '1'), se = F, dH43)+
  geom_smooth(aes(x = hour, y = pred, color = '1'), se = F, dH44)+
  geom_smooth(aes(x = hour, y = pred, color = '1'), se = F, dH45)+
  geom_smooth(aes(x = hour, y = pred, color = '1'), se = F, dH46)+
  geom_smooth(aes(x = hour, y = pred, color = '1'), se = F, dH47)+
  geom_ribbon(data = globalPsH, aes(x = hour, y = pred, ymin = L, ymax = U), fill = 'grey20', alpha=0.3) +
  geom_ribbon(data = globalPsH, aes(x = hour, y = pred, ymin = L50, ymax = U50), fill = 'grey30', alpha=0.5) +
  geom_smooth(data = globalPsH, aes(x = hour, y = pred, color = 'Mean'), se = F)+
  scale_colour_manual(name='',
                      values=c('1' = 'grey90','Mean' = 'black'))+
  theme(plot.margin = unit(c(0.5, 0, 0.5, 0.5), "cm"),
        text = element_text(size = 10),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.position = "none", axis.text.x=element_text(color='black', size=8), 
        axis.title.x=element_text(size=10, vjust = -4),
        axis.text.y=element_text(color='black', size=8), 
        axis.title.y = element_text(size=11, vjust = 2, face="bold"))


plot_hour

# Date
bdat_date <- read.table(file = "Date.txt", header = TRUE) 
dd_date <- melt(bdat_date, id=c("site", "year", "rep", "specie"), na.rm=TRUE)
dd_date <- melt(bdat_date, id = 1:4, na.rm=TRUE)
yD2 <- cast(dd_date, site ~ rep ~ year ~ specie)
dimnames(yD2)=NULL

mcmc.sample <- Output.model$mcmc.info$n.samples
mean.date <- mean(yD2, na.rm=T)
sd.date <- sd(yD2, na.rm=T)
min.date <- min(yD2, na.rm=T)
max.date <- max(yD2, na.rm=T)
unstd.date.pred <- min.date:max.date
date.pred <- (unstd.date.pred - mean.date)/sd.date
p.pred.date <- plogis(Output.model$mean$mu.b0 + Output.model$mean$mu.b2*date.pred + Output.model$mean$mu.b3*date.pred^2)

array.p.pred.date <- array(NA, dim=c(length(date.pred), mcmc.sample))
for(i in 1:mcmc.sample){
  array.p.pred.date[,i] <- plogis(Output.model$sims.list$mu.b0[i] + Output.model$sims.list$mu.b2[i]*date.pred + Output.model$sims.list$mu.b3[i]*(date.pred^2))
}
sub.set.D <- sort(sample(1:mcmc.sample, size=2000))

LPB_D <- apply(array.p.pred.date, 1, quantile, probs=0.025)
UPB_D <- apply(array.p.pred.date, 1, quantile, probs=0.975)
LPB50_D <- apply(array.p.pred.date, 1, quantile, probs=0.25)
UPB50_D <- apply(array.p.pred.date, 1, quantile, probs=0.75)  

globalPsD <- data.frame(day = unstd.date.pred, 
                        pred = p.pred.date, 
                        L = LPB_D, 
                        U = UPB_D,
                        L50 = LPB50_D,
                        U50 = UPB50_D)

#for species-level estimates
detD.int <- Output.model$mean$b0[1:47]
detD.sl1 <- Output.model$mean$b2[1:47]
detD.sl2 <- Output.model$mean$b3[1:47]

array.spp.p.pred.date <- array(NA, dim=c(length(date.pred), 47))
for(i in 1:(47)){
  array.spp.p.pred.date[,i] <- plogis(detD.int[i] + detD.sl1[i]*date.pred + detD.sl2[i]*date.pred^2)
}

sppPsD <- data.frame(day = rep(unstd.date.pred, times = 47),
                     pred = as.vector(array.spp.p.pred.date),
                     spp = rep(Spp$spp, each = length(unstd.date.pred)),
                     Spp = rep(Spp$Spp, each = length(unstd.date.pred))
)
dD1 <- sppPsD[sppPsD$spp == 1,]
dD2 <- sppPsD[sppPsD$spp == 2,]
dD3 <- sppPsD[sppPsD$spp == 3,]
dD4 <- sppPsD[sppPsD$spp == 4,]
dD5 <- sppPsD[sppPsD$spp == 5,]
dD6 <- sppPsD[sppPsD$spp == 6,]
dD7 <- sppPsD[sppPsD$spp == 7,]
dD8 <- sppPsD[sppPsD$spp == 8,]
dD9 <- sppPsD[sppPsD$spp == 9,]
dD10 <- sppPsD[sppPsD$spp == 10,]
dD11 <- sppPsD[sppPsD$spp == 11,]
dD12 <- sppPsD[sppPsD$spp == 12,]
dD13 <- sppPsD[sppPsD$spp == 13,]
dD14 <- sppPsD[sppPsD$spp == 14,]
dD15 <- sppPsD[sppPsD$spp == 15,]
dD16 <- sppPsD[sppPsD$spp == 16,]
dD17 <- sppPsD[sppPsD$spp == 17,]
dD18 <- sppPsD[sppPsD$spp == 18,]
dD19 <- sppPsD[sppPsD$spp == 19,]
dD20 <- sppPsD[sppPsD$spp == 20,]
dD21 <- sppPsD[sppPsD$spp == 21,]
dD22 <- sppPsD[sppPsD$spp == 22,]
dD23 <- sppPsD[sppPsD$spp == 23,]
dD24 <- sppPsD[sppPsD$spp == 24,]
dD25 <- sppPsD[sppPsD$spp == 25,]
dD26 <- sppPsD[sppPsD$spp == 26,]
dD27 <- sppPsD[sppPsD$spp == 27,]
dD28 <- sppPsD[sppPsD$spp == 28,]
dD29 <- sppPsD[sppPsD$spp == 29,]
dD30 <- sppPsD[sppPsD$spp == 30,]
dD31 <- sppPsD[sppPsD$spp == 31,]
dD32 <- sppPsD[sppPsD$spp == 32,]
dD33 <- sppPsD[sppPsD$spp == 33,]
dD34 <- sppPsD[sppPsD$spp == 34,]
dD35 <- sppPsD[sppPsD$spp == 35,]
dD36 <- sppPsD[sppPsD$spp == 36,]
dD37 <- sppPsD[sppPsD$spp == 37,]
dD38 <- sppPsD[sppPsD$spp == 38,]
dD39 <- sppPsD[sppPsD$spp == 39,]
dD40 <- sppPsD[sppPsD$spp == 40,]
dD41 <- sppPsD[sppPsD$spp == 41,]
dD42 <- sppPsD[sppPsD$spp == 42,]
dD43 <- sppPsD[sppPsD$spp == 43,]
dD44 <- sppPsD[sppPsD$spp == 44,]
dD45 <- sppPsD[sppPsD$spp == 45,]
dD46 <- sppPsD[sppPsD$spp == 46,]
dD47 <- sppPsD[sppPsD$spp == 47,]

plot_date <- ggplot() +
  scale_x_continuous(breaks = c(20, 30, 40, 50),labels=c('Apr 20','Apr 30','May 10','May 20')) +
  ylab('') + xlab('Date') + ylim(c(0,1)) + 
  theme_bw() + theme_few()+
  geom_smooth(aes(x = day, y = pred, color = '1'), se = F, dD1)+
  geom_smooth(aes(x = day, y = pred, color = '1'), se = F, dD2)+
  geom_smooth(aes(x = day, y = pred, color = '1'), se = F, dD3)+
  geom_smooth(aes(x = day, y = pred, color = '1'), se = F, dD4)+
  geom_smooth(aes(x = day, y = pred, color = '1'), se = F, dD5)+
  geom_smooth(aes(x = day, y = pred, color = '1'), se = F, dD6)+
  geom_smooth(aes(x = day, y = pred, color = '1'), se = F, dD7)+
  geom_smooth(aes(x = day, y = pred, color = '1'), se = F, dD8)+
  geom_smooth(aes(x = day, y = pred, color = '1'), se = F, dD9)+
  geom_smooth(aes(x = day, y = pred, color = '1'), se = F, dD10)+
  geom_smooth(aes(x = day, y = pred, color = '1'), se = F, dD11)+
  geom_smooth(aes(x = day, y = pred, color = '1'), se = F, dD12)+
  geom_smooth(aes(x = day, y = pred, color = '1'), se = F, dD13)+
  geom_smooth(aes(x = day, y = pred, color = '1'), se = F, dD14)+
  geom_smooth(aes(x = day, y = pred, color = '1'), se = F, dD15)+
  geom_smooth(aes(x = day, y = pred, color = '1'), se = F, dD16)+
  geom_smooth(aes(x = day, y = pred, color = '1'), se = F, dD17)+
  geom_smooth(aes(x = day, y = pred, color = '1'), se = F, dD18)+
  geom_smooth(aes(x = day, y = pred, color = '1'), se = F, dD19)+
  geom_smooth(aes(x = day, y = pred, color = '1'), se = F, dD20)+
  geom_smooth(aes(x = day, y = pred, color = '1'), se = F, dD21)+
  geom_smooth(aes(x = day, y = pred, color = '1'), se = F, dD22)+
  geom_smooth(aes(x = day, y = pred, color = '1'), se = F, dD23)+
  geom_smooth(aes(x = day, y = pred, color = '1'), se = F, dD24)+
  geom_smooth(aes(x = day, y = pred, color = '1'), se = F, dD25)+
  geom_smooth(aes(x = day, y = pred, color = '1'), se = F, dD26)+
  geom_smooth(aes(x = day, y = pred, color = '1'), se = F, dD27)+
  geom_smooth(aes(x = day, y = pred, color = '1'), se = F, dD28)+
  geom_smooth(aes(x = day, y = pred, color = '1'), se = F, dD29)+
  geom_smooth(aes(x = day, y = pred, color = '1'), se = F, dD30)+
  geom_smooth(aes(x = day, y = pred, color = '1'), se = F, dD31)+
  geom_smooth(aes(x = day, y = pred, color = '1'), se = F, dD32)+
  geom_smooth(aes(x = day, y = pred, color = '1'), se = F, dD33)+
  geom_smooth(aes(x = day, y = pred, color = '1'), se = F, dD34)+
  geom_smooth(aes(x = day, y = pred, color = '1'), se = F, dD35)+
  geom_smooth(aes(x = day, y = pred, color = '1'), se = F, dD36)+
  geom_smooth(aes(x = day, y = pred, color = '1'), se = F, dD37)+
  geom_smooth(aes(x = day, y = pred, color = '1'), se = F, dD38)+
  geom_smooth(aes(x = day, y = pred, color = '1'), se = F, dD39)+
  geom_smooth(aes(x = day, y = pred, color = '1'), se = F, dD40)+
  geom_smooth(aes(x = day, y = pred, color = '1'), se = F, dD41)+
  geom_smooth(aes(x = day, y = pred, color = '1'), se = F, dD42)+
  geom_smooth(aes(x = day, y = pred, color = '1'), se = F, dD43)+
  geom_smooth(aes(x = day, y = pred, color = '1'), se = F, dD44)+
  geom_smooth(aes(x = day, y = pred, color = '1'), se = F, dD45)+
  geom_smooth(aes(x = day, y = pred, color = '1'), se = F, dD46)+
  geom_smooth(aes(x = day, y = pred, color = '1'), se = F, dD47)+
  geom_ribbon(data = globalPsD, aes(x = day, y = pred, ymin = L, ymax = U), fill = 'grey20', alpha=0.3) +
  geom_ribbon(data = globalPsD, aes(x = day, y = pred, ymin = L50, ymax = U50), fill = 'grey30', alpha=0.5) +
  geom_smooth(data = globalPsD, aes(x = day, y = pred, color = 'Mean'), se = F)+
  scale_colour_manual(name='',
                      values=c('1' = 'grey90','Mean' = 'black'))+
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0), "cm"),
        text = element_text(size = 10),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.position = "none", axis.text.x=element_text(color='black', size=8), 
        axis.title.x=element_text(size=10, vjust = -4),
        axis.text.y=element_text(color='black', size=8),
        legend.text=element_text(color='black', size=8))


plot_date

#####

FigureA.3=plot_grid(plot_hour, plot_date, ncol = 2, nrow = 1, 
          labels = c('(a)', '(b)'),label_size = 10,
          label_y = c(1.005, 1.005), label_x = c(0.085, 0.017))


#############################################
#######################################################

################################
# Occurrence probability of little bustard, greater short-toed lark, 
# eurasian hoopoe and little owl before and after irrigation 

# Little bustard
BeforeLB <- mean(c(mean(Output.model$mean$psi[c(2),c(1,2,3,4,5,6,7,8,9),45]),
                   mean(Output.model$mean$psi[c(3),c(1,2,3,4,5,6,7,8,9),45]),
                   mean(Output.model$mean$psi[c(4),c(1,2,3,4,5,6,7,8,9),45]),
                   mean(Output.model$mean$psi[c(6),c(1,2,3,4,5,6,7,8,9),45]),
                   mean(Output.model$mean$psi[c(7),c(1,2,3,4,5,6,7,8,9),45]),
                   mean(Output.model$mean$psi[c(8),c(1,2,3,4,5,6,7,8,9),45]),
                   mean(Output.model$mean$psi[c(9),c(1,2,3,4,5,6,7,8,9),45]),
                   mean(Output.model$mean$psi[c(10),c(1,2,3,4,5,6,7,8,9),45]),
                   mean(Output.model$mean$psi[c(11),c(1,2),45]),
                   mean(Output.model$mean$psi[c(12),c(1,2),45]),
                   mean(Output.model$mean$psi[c(13),c(1,2),45]),
                   mean(Output.model$mean$psi[c(14),c(1,2),45]),
                   mean(Output.model$mean$psi[c(15),c(1,2),45]),
                   mean(Output.model$mean$psi[c(16),c(1,2),45]),
                   mean(Output.model$mean$psi[c(17),c(1,2),45]),
                   mean(Output.model$mean$psi[c(18),c(1,2),45]),
                   mean(Output.model$mean$psi[c(19),c(1,2),45])))
BeforeLB

AfterLB <- mean(c(mean(Output.model$mean$psi[c(2),c(10,11,12),45]),
                  mean(Output.model$mean$psi[c(3),c(10,11,12),45]),
                  mean(Output.model$mean$psi[c(4),c(10,11,12),45]),
                  mean(Output.model$mean$psi[c(6),c(10,11,12),45]),
                  mean(Output.model$mean$psi[c(7),c(10,11,12),45]),
                  mean(Output.model$mean$psi[c(8),c(10,11,12),45]),
                  mean(Output.model$mean$psi[c(9),c(10,11,12),45]),
                  mean(Output.model$mean$psi[c(10),c(10,11,12),45]),
                  mean(Output.model$mean$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),45]),
                  mean(Output.model$mean$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),45]),
                  mean(Output.model$mean$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),45]),
                  mean(Output.model$mean$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),45]),
                  mean(Output.model$mean$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),45]),
                  mean(Output.model$mean$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),45]),
                  mean(Output.model$mean$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),45]),
                  mean(Output.model$mean$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),45]),
                  mean(Output.model$mean$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),45])))

AfterLB


# Greater short-toed lark
BeforeGSTL <- mean(c(mean(Output.model$mean$psi[c(2),c(1,2,3,4,5,6,7,8,9),7]),
                     mean(Output.model$mean$psi[c(3),c(1,2,3,4,5,6,7,8,9),7]),
                     mean(Output.model$mean$psi[c(4),c(1,2,3,4,5,6,7,8,9),7]),
                     mean(Output.model$mean$psi[c(6),c(1,2,3,4,5,6,7,8,9),7]),
                     mean(Output.model$mean$psi[c(7),c(1,2,3,4,5,6,7,8,9),7]),
                     mean(Output.model$mean$psi[c(8),c(1,2,3,4,5,6,7,8,9),7]),
                     mean(Output.model$mean$psi[c(9),c(1,2,3,4,5,6,7,8,9),7]),
                     mean(Output.model$mean$psi[c(10),c(1,2,3,4,5,6,7,8,9),7]),
                     mean(Output.model$mean$psi[c(11),c(1,2),7]),
                     mean(Output.model$mean$psi[c(12),c(1,2),7]),
                     mean(Output.model$mean$psi[c(13),c(1,2),7]),
                     mean(Output.model$mean$psi[c(14),c(1,2),7]),
                     mean(Output.model$mean$psi[c(15),c(1,2),7]),
                     mean(Output.model$mean$psi[c(16),c(1,2),7]),
                     mean(Output.model$mean$psi[c(17),c(1,2),7]),
                     mean(Output.model$mean$psi[c(18),c(1,2),7]),
                     mean(Output.model$mean$psi[c(19),c(1,2),7])))
BeforeGSTL

AfterGSTL <- mean(c(mean(Output.model$mean$psi[c(2),c(10,11,12),7]),
                    mean(Output.model$mean$psi[c(3),c(10,11,12),7]),
                    mean(Output.model$mean$psi[c(4),c(10,11,12),7]),
                    mean(Output.model$mean$psi[c(6),c(10,11,12),7]),
                    mean(Output.model$mean$psi[c(7),c(10,11,12),7]),
                    mean(Output.model$mean$psi[c(8),c(10,11,12),7]),
                    mean(Output.model$mean$psi[c(9),c(10,11,12),7]),
                    mean(Output.model$mean$psi[c(10),c(10,11,12),7]),
                    mean(Output.model$mean$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),7]),
                    mean(Output.model$mean$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),7]),
                    mean(Output.model$mean$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),7]),
                    mean(Output.model$mean$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),7]),
                    mean(Output.model$mean$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),7]),
                    mean(Output.model$mean$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),7]),
                    mean(Output.model$mean$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),7]),
                    mean(Output.model$mean$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),7]),
                    mean(Output.model$mean$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),7])))
AfterGSTL


# Eurasian hoopoe
BeforeEH <- mean(c(mean(Output.model$mean$psi[c(2),c(1,2,3,4,5,6,7,8,9),47]),
                   mean(Output.model$mean$psi[c(3),c(1,2,3,4,5,6,7,8,9),47]),
                   mean(Output.model$mean$psi[c(4),c(1,2,3,4,5,6,7,8,9),47]),
                   mean(Output.model$mean$psi[c(6),c(1,2,3,4,5,6,7,8,9),47]),
                   mean(Output.model$mean$psi[c(7),c(1,2,3,4,5,6,7,8,9),47]),
                   mean(Output.model$mean$psi[c(8),c(1,2,3,4,5,6,7,8,9),47]),
                   mean(Output.model$mean$psi[c(9),c(1,2,3,4,5,6,7,8,9),47]),
                   mean(Output.model$mean$psi[c(10),c(1,2,3,4,5,6,7,8,9),47]),
                   mean(Output.model$mean$psi[c(11),c(1,2),47]),
                   mean(Output.model$mean$psi[c(12),c(1,2),47]),
                   mean(Output.model$mean$psi[c(13),c(1,2),47]),
                   mean(Output.model$mean$psi[c(14),c(1,2),47]),
                   mean(Output.model$mean$psi[c(15),c(1,2),47]),
                   mean(Output.model$mean$psi[c(16),c(1,2),47]),
                   mean(Output.model$mean$psi[c(17),c(1,2),47]),
                   mean(Output.model$mean$psi[c(18),c(1,2),47]),
                   mean(Output.model$mean$psi[c(19),c(1,2),47])))
BeforeEH

AfterEH <- mean(c(mean(Output.model$mean$psi[c(2),c(10,11,12),47]),
                  mean(Output.model$mean$psi[c(3),c(10,11,12),47]),
                  mean(Output.model$mean$psi[c(4),c(10,11,12),47]),
                  mean(Output.model$mean$psi[c(6),c(10,11,12),47]),
                  mean(Output.model$mean$psi[c(7),c(10,11,12),47]),
                  mean(Output.model$mean$psi[c(8),c(10,11,12),47]),
                  mean(Output.model$mean$psi[c(9),c(10,11,12),47]),
                  mean(Output.model$mean$psi[c(10),c(10,11,12),47]),
                  mean(Output.model$mean$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),47]),
                  mean(Output.model$mean$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),47]),
                  mean(Output.model$mean$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),47]),
                  mean(Output.model$mean$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),47]),
                  mean(Output.model$mean$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),47]),
                  mean(Output.model$mean$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),47]),
                  mean(Output.model$mean$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),47]),
                  mean(Output.model$mean$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),47]),
                  mean(Output.model$mean$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),47])))
AfterEH


# Black-eared wheatear
BeforeBEW <- mean(c(mean(Output.model$mean$psi[c(2),c(1,2,3,4,5,6,7,8,9),33]),
                    mean(Output.model$mean$psi[c(3),c(1,2,3,4,5,6,7,8,9),33]),
                    mean(Output.model$mean$psi[c(4),c(1,2,3,4,5,6,7,8,9),33]),
                    mean(Output.model$mean$psi[c(6),c(1,2,3,4,5,6,7,8,9),33]),
                    mean(Output.model$mean$psi[c(7),c(1,2,3,4,5,6,7,8,9),33]),
                    mean(Output.model$mean$psi[c(8),c(1,2,3,4,5,6,7,8,9),33]),
                    mean(Output.model$mean$psi[c(9),c(1,2,3,4,5,6,7,8,9),33]),
                    mean(Output.model$mean$psi[c(10),c(1,2,3,4,5,6,7,8,9),33]),
                    mean(Output.model$mean$psi[c(11),c(1,2),33]),
                    mean(Output.model$mean$psi[c(12),c(1,2),33]),
                    mean(Output.model$mean$psi[c(13),c(1,2),33]),
                    mean(Output.model$mean$psi[c(14),c(1,2),33]),
                    mean(Output.model$mean$psi[c(15),c(1,2),33]),
                    mean(Output.model$mean$psi[c(16),c(1,2),33]),
                    mean(Output.model$mean$psi[c(17),c(1,2),33]),
                    mean(Output.model$mean$psi[c(18),c(1,2),33]),
                    mean(Output.model$mean$psi[c(19),c(1,2),33])))
BeforeBEW

AfterBEW <- mean(c(mean(Output.model$mean$psi[c(2),c(10,11,12),33]),
                   mean(Output.model$mean$psi[c(3),c(10,11,12),33]),
                   mean(Output.model$mean$psi[c(4),c(10,11,12),33]),
                   mean(Output.model$mean$psi[c(6),c(10,11,12),33]),
                   mean(Output.model$mean$psi[c(7),c(10,11,12),33]),
                   mean(Output.model$mean$psi[c(8),c(10,11,12),33]),
                   mean(Output.model$mean$psi[c(9),c(10,11,12),33]),
                   mean(Output.model$mean$psi[c(10),c(10,11,12),33]),
                   mean(Output.model$mean$psi[c(11),c(3,4,5,6,7,8,9,10,11,12),33]),
                   mean(Output.model$mean$psi[c(12),c(3,4,5,6,7,8,9,10,11,12),33]),
                   mean(Output.model$mean$psi[c(13),c(3,4,5,6,7,8,9,10,11,12),33]),
                   mean(Output.model$mean$psi[c(14),c(3,4,5,6,7,8,9,10,11,12),33]),
                   mean(Output.model$mean$psi[c(15),c(3,4,5,6,7,8,9,10,11,12),33]),
                   mean(Output.model$mean$psi[c(16),c(3,4,5,6,7,8,9,10,11,12),33]),
                   mean(Output.model$mean$psi[c(17),c(3,4,5,6,7,8,9,10,11,12),33]),
                   mean(Output.model$mean$psi[c(18),c(3,4,5,6,7,8,9,10,11,12),33]),
                   mean(Output.model$mean$psi[c(19),c(3,4,5,6,7,8,9,10,11,12),33])))
AfterBEW



#####################################
###########################################
#### Change in richness at community level and on target species groups

# At community level
East_before <- c(mean(Output.model$sims.list$z[,11,c(1,2),]),
                 mean(Output.model$sims.list$z[,12,c(1,2),]),
                 mean(Output.model$sims.list$z[,13,c(1,2),]),
                 mean(Output.model$sims.list$z[,14,c(1,2),]),
                 mean(Output.model$sims.list$z[,15,c(1,2),]),
                 mean(Output.model$sims.list$z[,16,c(1,2),]),
                 mean(Output.model$sims.list$z[,17,c(1,2),]),
                 mean(Output.model$sims.list$z[,18,c(1,2),]),
                 mean(Output.model$sims.list$z[,19,c(1,2),]))
East_after <- c(mean(Output.model$sims.list$z[,11,c(3,4,5,6,7,8,9,10,11,12),]),
                mean(Output.model$sims.list$z[,12,c(3,4,5,6,7,8,9,10,11,12),]),
                mean(Output.model$sims.list$z[,13,c(3,4,5,6,7,8,9,10,11,12),]),
                mean(Output.model$sims.list$z[,14,c(3,4,5,6,7,8,9,10,11,12),]),
                mean(Output.model$sims.list$z[,15,c(3,4,5,6,7,8,9,10,11,12),]),
                mean(Output.model$sims.list$z[,16,c(3,4,5,6,7,8,9,10,11,12),]),
                mean(Output.model$sims.list$z[,17,c(3,4,5,6,7,8,9,10,11,12),]),
                mean(Output.model$sims.list$z[,18,c(3,4,5,6,7,8,9,10,11,12),]),
                mean(Output.model$sims.list$z[,19,c(3,4,5,6,7,8,9,10,11,12),]))
West_before <- c(mean(Output.model$sims.list$z[,2,c(1,2,3,4,5,6,7,8,9),]),
                 mean(Output.model$sims.list$z[,3,c(1,2,3,4,5,6,7,8,9),]),
                 mean(Output.model$sims.list$z[,4,c(1,2,3,4,5,6,7,8,9),]),
                 mean(Output.model$sims.list$z[,6,c(1,2,3,4,5,6,7,8,9),]),
                 mean(Output.model$sims.list$z[,7,c(1,2,3,4,5,6,7,8,9),]),
                 mean(Output.model$sims.list$z[,8,c(1,2,3,4,5,6,7,8,9),]),
                 mean(Output.model$sims.list$z[,9,c(1,2,3,4,5,6,7,8,9),]),
                 mean(Output.model$sims.list$z[,10,c(1,2,3,4,5,6,7,8,9),]))
West_after <- c(mean(Output.model$sims.list$z[,2,c(10,11,12),]),
                mean(Output.model$sims.list$z[,3,c(10,11,12),]),
                mean(Output.model$sims.list$z[,4,c(10,11,12),]),
                mean(Output.model$sims.list$z[,6,c(10,11,12),]),
                mean(Output.model$sims.list$z[,7,c(10,11,12),]),
                mean(Output.model$sims.list$z[,8,c(10,11,12),]),
                mean(Output.model$sims.list$z[,9,c(10,11,12),]),
                mean(Output.model$sims.list$z[,10,c(10,11,12),]))

East_before1 <- 47*East_before
East_after1 <- 47*East_after
West_before1 <- 47*West_before
West_after1 <- 47*West_after

mean(East_before1)
mean(East_after1)
mean(West_before1)
mean(West_after1)

# Farmland 
Farmland_sec <- c(mean(Output.model$sims.list$z[,2,c(1,2,3,4,5,6,7,8,9),c(45,1,15,7,22,29,18,33,12,9,47,30,40,5,20)]),
                  mean(Output.model$sims.list$z[,3,c(1,2,3,4,5,6,7,8,9),c(45,1,15,7,22,29,18,33,12,9,47,30,40,5,20)]),
                  mean(Output.model$sims.list$z[,4,c(1,2,3,4,5,6,7,8,9),c(45,1,15,7,22,29,18,33,12,9,47,30,40,5,20)]),
                  mean(Output.model$sims.list$z[,6,c(1,2,3,4,5,6,7,8,9),c(45,1,15,7,22,29,18,33,12,9,47,30,40,5,20)]),
                  mean(Output.model$sims.list$z[,7,c(1,2,3,4,5,6,7,8,9),c(45,1,15,7,22,29,18,33,12,9,47,30,40,5,20)]),
                  mean(Output.model$sims.list$z[,8,c(1,2,3,4,5,6,7,8,9),c(45,1,15,7,22,29,18,33,12,9,47,30,40,5,20)]),
                  mean(Output.model$sims.list$z[,9,c(1,2,3,4,5,6,7,8,9),c(45,1,15,7,22,29,18,33,12,9,47,30,40,5,20)]),
                  mean(Output.model$sims.list$z[,10,c(1,2,3,4,5,6,7,8,9),c(45,1,15,7,22,29,18,33,12,9,47,30,40,5,20)]),
                  mean(Output.model$sims.list$z[,11,c(1,2),c(45,1,15,7,22,29,18,33,12,9,47,30,40,5,20)]),
                  mean(Output.model$sims.list$z[,12,c(1,2),c(45,1,15,7,22,29,18,33,12,9,47,30,40,5,20)]),
                  mean(Output.model$sims.list$z[,13,c(1,2),c(45,1,15,7,22,29,18,33,12,9,47,30,40,5,20)]),
                  mean(Output.model$sims.list$z[,14,c(1,2),c(45,1,15,7,22,29,18,33,12,9,47,30,40,5,20)]),
                  mean(Output.model$sims.list$z[,15,c(1,2),c(45,1,15,7,22,29,18,33,12,9,47,30,40,5,20)]),
                  mean(Output.model$sims.list$z[,16,c(1,2),c(45,1,15,7,22,29,18,33,12,9,47,30,40,5,20)]),
                  mean(Output.model$sims.list$z[,17,c(1,2),c(45,1,15,7,22,29,18,33,12,9,47,30,40,5,20)]),
                  mean(Output.model$sims.list$z[,18,c(1,2),c(45,1,15,7,22,29,18,33,12,9,47,30,40,5,20)]),
                  mean(Output.model$sims.list$z[,19,c(1,2),c(45,1,15,7,22,29,18,33,12,9,47,30,40,5,20)]))

Farmland_irri <- c(mean(Output.model$sims.list$z[,2,c(10,11,12),c(45,1,15,7,22,29,18,33,12,9,47,30,40,5,20)]),
                   mean(Output.model$sims.list$z[,3,c(10,11,12),c(45,1,15,7,22,29,18,33,12,9,47,30,40,5,20)]),
                   mean(Output.model$sims.list$z[,4,c(10,11,12),c(45,1,15,7,22,29,18,33,12,9,47,30,40,5,20)]),
                   mean(Output.model$sims.list$z[,6,c(10,11,12),c(45,1,15,7,22,29,18,33,12,9,47,30,40,5,20)]),
                   mean(Output.model$sims.list$z[,7,c(10,11,12),c(45,1,15,7,22,29,18,33,12,9,47,30,40,5,20)]),
                   mean(Output.model$sims.list$z[,8,c(10,11,12),c(45,1,15,7,22,29,18,33,12,9,47,30,40,5,20)]),
                   mean(Output.model$sims.list$z[,9,c(10,11,12),c(45,1,15,7,22,29,18,33,12,9,47,30,40,5,20)]),
                   mean(Output.model$sims.list$z[,10,c(10,11,12),c(45,1,15,7,22,29,18,33,12,9,47,30,40,5,20)]),
                   mean(Output.model$sims.list$z[,11,c(3,4,5,6,7,8,9,10,11,12),c(45,1,15,7,22,29,18,33,12,9,47,30,40,5,20)]),
                   mean(Output.model$sims.list$z[,12,c(3,4,5,6,7,8,9,10,11,12),c(45,1,15,7,22,29,18,33,12,9,47,30,40,5,20)]),
                   mean(Output.model$sims.list$z[,13,c(3,4,5,6,7,8,9,10,11,12),c(45,1,15,7,22,29,18,33,12,9,47,30,40,5,20)]),
                   mean(Output.model$sims.list$z[,14,c(3,4,5,6,7,8,9,10,11,12),c(45,1,15,7,22,29,18,33,12,9,47,30,40,5,20)]),
                   mean(Output.model$sims.list$z[,15,c(3,4,5,6,7,8,9,10,11,12),c(45,1,15,7,22,29,18,33,12,9,47,30,40,5,20)]),
                   mean(Output.model$sims.list$z[,16,c(3,4,5,6,7,8,9,10,11,12),c(45,1,15,7,22,29,18,33,12,9,47,30,40,5,20)]),
                   mean(Output.model$sims.list$z[,17,c(3,4,5,6,7,8,9,10,11,12),c(45,1,15,7,22,29,18,33,12,9,47,30,40,5,20)]),
                   mean(Output.model$sims.list$z[,18,c(3,4,5,6,7,8,9,10,11,12),c(45,1,15,7,22,29,18,33,12,9,47,30,40,5,20)]),
                   mean(Output.model$sims.list$z[,19,c(3,4,5,6,7,8,9,10,11,12),c(45,1,15,7,22,29,18,33,12,9,47,30,40,5,20)]))

Farmland_sec             
Farmland1_sec <- 15*Farmland_sec

Farmland_irri             
Farmland1_irri <- 15*Farmland_irri

mean(Farmland1_sec)
mean(Farmland1_irri)

# Shrubland
Shrub_sec <- c(mean(Output.model$sims.list$z[,2,c(1,2,3,4,5,6,7,8,9),c(23,3,27,26,44,42,43,38,24)]),
               mean(Output.model$sims.list$z[,3,c(1,2,3,4,5,6,7,8,9),c(23,3,27,26,44,42,43,38,24)]),
               mean(Output.model$sims.list$z[,4,c(1,2,3,4,5,6,7,8,9),c(23,3,27,26,44,42,43,38,24)]),
               mean(Output.model$sims.list$z[,6,c(1,2,3,4,5,6,7,8,9),c(23,3,27,26,44,42,43,38,24)]),
               mean(Output.model$sims.list$z[,7,c(1,2,3,4,5,6,7,8,9),c(23,3,27,26,44,42,43,38,24)]),
               mean(Output.model$sims.list$z[,8,c(1,2,3,4,5,6,7,8,9),c(23,3,27,26,44,42,43,38,24)]),
               mean(Output.model$sims.list$z[,9,c(1,2,3,4,5,6,7,8,9),c(23,3,27,26,44,42,43,38,24)]),
               mean(Output.model$sims.list$z[,10,c(1,2,3,4,5,6,7,8,9),c(23,3,27,26,44,42,43,38,24)]),
               mean(Output.model$sims.list$z[,11,c(1,2),c(23,3,27,26,44,42,43,38,24)]),
               mean(Output.model$sims.list$z[,12,c(1,2),c(23,3,27,26,44,42,43,38,24)]),
               mean(Output.model$sims.list$z[,13,c(1,2),c(23,3,27,26,44,42,43,38,24)]),
               mean(Output.model$sims.list$z[,14,c(1,2),c(23,3,27,26,44,42,43,38,24)]),
               mean(Output.model$sims.list$z[,15,c(1,2),c(23,3,27,26,44,42,43,38,24)]),
               mean(Output.model$sims.list$z[,16,c(1,2),c(23,3,27,26,44,42,43,38,24)]),
               mean(Output.model$sims.list$z[,17,c(1,2),c(23,3,27,26,44,42,43,38,24)]),
               mean(Output.model$sims.list$z[,18,c(1,2),c(23,3,27,26,44,42,43,38,24)]),
               mean(Output.model$sims.list$z[,19,c(1,2),c(23,3,27,26,44,42,43,38,24)]))

Shrub_irri <- c(mean(Output.model$sims.list$z[,2,c(10,11,12),c(23,3,27,26,44,42,43,38,24)]),
                mean(Output.model$sims.list$z[,3,c(10,11,12),c(23,3,27,26,44,42,43,38,24)]),
                mean(Output.model$sims.list$z[,4,c(10,11,12),c(23,3,27,26,44,42,43,38,24)]),
                mean(Output.model$sims.list$z[,6,c(10,11,12),c(23,3,27,26,44,42,43,38,24)]),
                mean(Output.model$sims.list$z[,7,c(10,11,12),c(23,3,27,26,44,42,43,38,24)]),
                mean(Output.model$sims.list$z[,8,c(10,11,12),c(23,3,27,26,44,42,43,38,24)]),
                mean(Output.model$sims.list$z[,9,c(10,11,12),c(23,3,27,26,44,42,43,38,24)]),
                mean(Output.model$sims.list$z[,10,c(10,11,12),c(23,3,27,26,44,42,43,38,24)]),
                mean(Output.model$sims.list$z[,11,c(3,4,5,6,7,8,9,10,11,12),c(23,3,27,26,44,42,43,38,24)]),
                mean(Output.model$sims.list$z[,12,c(3,4,5,6,7,8,9,10,11,12),c(23,3,27,26,44,42,43,38,24)]),
                mean(Output.model$sims.list$z[,13,c(3,4,5,6,7,8,9,10,11,12),c(23,3,27,26,44,42,43,38,24)]),
                mean(Output.model$sims.list$z[,14,c(3,4,5,6,7,8,9,10,11,12),c(23,3,27,26,44,42,43,38,24)]),
                mean(Output.model$sims.list$z[,15,c(3,4,5,6,7,8,9,10,11,12),c(23,3,27,26,44,42,43,38,24)]),
                mean(Output.model$sims.list$z[,16,c(3,4,5,6,7,8,9,10,11,12),c(23,3,27,26,44,42,43,38,24)]),
                mean(Output.model$sims.list$z[,17,c(3,4,5,6,7,8,9,10,11,12),c(23,3,27,26,44,42,43,38,24)]),
                mean(Output.model$sims.list$z[,18,c(3,4,5,6,7,8,9,10,11,12),c(23,3,27,26,44,42,43,38,24)]),
                mean(Output.model$sims.list$z[,19,c(3,4,5,6,7,8,9,10,11,12),c(23,3,27,26,44,42,43,38,24)]))

Shrub_sec             
Shrub1_sec <- 9*Shrub_sec

Shrub_irri             
Shrub1_irri <- 9*Shrub_irri

mean(Shrub1_sec)
mean(Shrub1_irri)

# Forest
Forest_sec <- c(mean(Output.model$sims.list$z[,2,c(1,2,3,4,5,6,7,8,9),c(13,21,34)]),
                mean(Output.model$sims.list$z[,3,c(1,2,3,4,5,6,7,8,9),c(13,21,34)]),
                mean(Output.model$sims.list$z[,4,c(1,2,3,4,5,6,7,8,9),c(13,21,34)]),
                mean(Output.model$sims.list$z[,6,c(1,2,3,4,5,6,7,8,9),c(13,21,34)]),
                mean(Output.model$sims.list$z[,7,c(1,2,3,4,5,6,7,8,9),c(13,21,34)]),
                mean(Output.model$sims.list$z[,8,c(1,2,3,4,5,6,7,8,9),c(13,21,34)]),
                mean(Output.model$sims.list$z[,9,c(1,2,3,4,5,6,7,8,9),c(13,21,34)]),
                mean(Output.model$sims.list$z[,10,c(1,2,3,4,5,6,7,8,9),c(13,21,34)]),
                mean(Output.model$sims.list$z[,11,c(1,2),c(13,21,34)]),
                mean(Output.model$sims.list$z[,12,c(1,2),c(13,21,34)]),
                mean(Output.model$sims.list$z[,13,c(1,2),c(13,21,34)]),
                mean(Output.model$sims.list$z[,14,c(1,2),c(13,21,34)]),
                mean(Output.model$sims.list$z[,15,c(1,2),c(13,21,34)]),
                mean(Output.model$sims.list$z[,16,c(1,2),c(13,21,34)]),
                mean(Output.model$sims.list$z[,17,c(1,2),c(13,21,34)]),
                mean(Output.model$sims.list$z[,18,c(1,2),c(13,21,34)]),
                mean(Output.model$sims.list$z[,19,c(1,2),c(13,21,34)]))

Forest_irri <- c(mean(Output.model$sims.list$z[,2,c(10,11,12),c(13,21,34)]),
                 mean(Output.model$sims.list$z[,3,c(10,11,12),c(13,21,34)]),
                 mean(Output.model$sims.list$z[,4,c(10,11,12),c(13,21,34)]),
                 mean(Output.model$sims.list$z[,6,c(10,11,12),c(13,21,34)]),
                 mean(Output.model$sims.list$z[,7,c(10,11,12),c(13,21,34)]),
                 mean(Output.model$sims.list$z[,8,c(10,11,12),c(13,21,34)]),
                 mean(Output.model$sims.list$z[,9,c(10,11,12),c(13,21,34)]),
                 mean(Output.model$sims.list$z[,10,c(10,11,12),c(13,21,34)]),
                 mean(Output.model$sims.list$z[,11,c(3,4,5,6,7,8,9,10,11,12),c(13,21,34)]),
                 mean(Output.model$sims.list$z[,12,c(3,4,5,6,7,8,9,10,11,12),c(13,21,34)]),
                 mean(Output.model$sims.list$z[,13,c(3,4,5,6,7,8,9,10,11,12),c(13,21,34)]),
                 mean(Output.model$sims.list$z[,14,c(3,4,5,6,7,8,9,10,11,12),c(13,21,34)]),
                 mean(Output.model$sims.list$z[,15,c(3,4,5,6,7,8,9,10,11,12),c(13,21,34)]),
                 mean(Output.model$sims.list$z[,16,c(3,4,5,6,7,8,9,10,11,12),c(13,21,34)]),
                 mean(Output.model$sims.list$z[,17,c(3,4,5,6,7,8,9,10,11,12),c(13,21,34)]),
                 mean(Output.model$sims.list$z[,18,c(3,4,5,6,7,8,9,10,11,12),c(13,21,34)]),
                 mean(Output.model$sims.list$z[,19,c(3,4,5,6,7,8,9,10,11,12),c(13,21,34)]))

Forest_sec             
Forest1_sec <- 3*Forest_sec

Forest_irri             
Forest1_irri <- 3*Forest_irri

mean(Forest1_sec)
mean(Forest1_irri)

# Nonspecialist
Nonsp_sec <- c(mean(Output.model$sims.list$z[,2,c(1,2,3,4,5,6,7,8,9),c(8,39,10,19,35,25,28,46,41,16,36,14,11,31,6)]),
               mean(Output.model$sims.list$z[,3,c(1,2,3,4,5,6,7,8,9),c(8,39,10,19,35,25,28,46,41,16,36,14,11,31,6)]),
               mean(Output.model$sims.list$z[,4,c(1,2,3,4,5,6,7,8,9),c(8,39,10,19,35,25,28,46,41,16,36,14,11,31,6)]),
               mean(Output.model$sims.list$z[,6,c(1,2,3,4,5,6,7,8,9),c(8,39,10,19,35,25,28,46,41,16,36,14,11,31,6)]),
               mean(Output.model$sims.list$z[,7,c(1,2,3,4,5,6,7,8,9),c(8,39,10,19,35,25,28,46,41,16,36,14,11,31,6)]),
               mean(Output.model$sims.list$z[,8,c(1,2,3,4,5,6,7,8,9),c(8,39,10,19,35,25,28,46,41,16,36,14,11,31,6)]),
               mean(Output.model$sims.list$z[,9,c(1,2,3,4,5,6,7,8,9),c(8,39,10,19,35,25,28,46,41,16,36,14,11,31,6)]),
               mean(Output.model$sims.list$z[,10,c(1,2,3,4,5,6,7,8,9),c(8,39,10,19,35,25,28,46,41,16,36,14,11,31,6)]),
               mean(Output.model$sims.list$z[,11,c(1,2),c(8,39,10,19,35,25,28,46,41,16,36,14,11,31,6)]),
               mean(Output.model$sims.list$z[,12,c(1,2),c(8,39,10,19,35,25,28,46,41,16,36,14,11,31,6)]),
               mean(Output.model$sims.list$z[,13,c(1,2),c(8,39,10,19,35,25,28,46,41,16,36,14,11,31,6)]),
               mean(Output.model$sims.list$z[,14,c(1,2),c(8,39,10,19,35,25,28,46,41,16,36,14,11,31,6)]),
               mean(Output.model$sims.list$z[,15,c(1,2),c(8,39,10,19,35,25,28,46,41,16,36,14,11,31,6)]),
               mean(Output.model$sims.list$z[,16,c(1,2),c(8,39,10,19,35,25,28,46,41,16,36,14,11,31,6)]),
               mean(Output.model$sims.list$z[,17,c(1,2),c(8,39,10,19,35,25,28,46,41,16,36,14,11,31,6)]),
               mean(Output.model$sims.list$z[,18,c(1,2),c(8,39,10,19,35,25,28,46,41,16,36,14,11,31,6)]),
               mean(Output.model$sims.list$z[,19,c(1,2),c(8,39,10,19,35,25,28,46,41,16,36,14,11,31,6)]))

Nonsp_irri <- c(mean(Output.model$sims.list$z[,2,c(10,11,12),c(8,39,10,19,35,25,28,46,41,16,36,14,11,31,6)]),
                mean(Output.model$sims.list$z[,3,c(10,11,12),c(8,39,10,19,35,25,28,46,41,16,36,14,11,31,6)]),
                mean(Output.model$sims.list$z[,4,c(10,11,12),c(8,39,10,19,35,25,28,46,41,16,36,14,11,31,6)]),
                mean(Output.model$sims.list$z[,6,c(10,11,12),c(8,39,10,19,35,25,28,46,41,16,36,14,11,31,6)]),
                mean(Output.model$sims.list$z[,7,c(10,11,12),c(8,39,10,19,35,25,28,46,41,16,36,14,11,31,6)]),
                mean(Output.model$sims.list$z[,8,c(10,11,12),c(8,39,10,19,35,25,28,46,41,16,36,14,11,31,6)]),
                mean(Output.model$sims.list$z[,9,c(10,11,12),c(8,39,10,19,35,25,28,46,41,16,36,14,11,31,6)]),
                mean(Output.model$sims.list$z[,10,c(10,11,12),c(8,39,10,19,35,25,28,46,41,16,36,14,11,31,6)]),
                mean(Output.model$sims.list$z[,11,c(3,4,5,6,7,8,9,10,11,12),c(8,39,10,19,35,25,28,46,41,16,36,14,11,31,6)]),
                mean(Output.model$sims.list$z[,12,c(3,4,5,6,7,8,9,10,11,12),c(8,39,10,19,35,25,28,46,41,16,36,14,11,31,6)]),
                mean(Output.model$sims.list$z[,13,c(3,4,5,6,7,8,9,10,11,12),c(8,39,10,19,35,25,28,46,41,16,36,14,11,31,6)]),
                mean(Output.model$sims.list$z[,14,c(3,4,5,6,7,8,9,10,11,12),c(8,39,10,19,35,25,28,46,41,16,36,14,11,31,6)]),
                mean(Output.model$sims.list$z[,15,c(3,4,5,6,7,8,9,10,11,12),c(8,39,10,19,35,25,28,46,41,16,36,14,11,31,6)]),
                mean(Output.model$sims.list$z[,16,c(3,4,5,6,7,8,9,10,11,12),c(8,39,10,19,35,25,28,46,41,16,36,14,11,31,6)]),
                mean(Output.model$sims.list$z[,17,c(3,4,5,6,7,8,9,10,11,12),c(8,39,10,19,35,25,28,46,41,16,36,14,11,31,6)]),
                mean(Output.model$sims.list$z[,18,c(3,4,5,6,7,8,9,10,11,12),c(8,39,10,19,35,25,28,46,41,16,36,14,11,31,6)]),
                mean(Output.model$sims.list$z[,19,c(3,4,5,6,7,8,9,10,11,12),c(8,39,10,19,35,25,28,46,41,16,36,14,11,31,6)]))

Nonsp_sec             
Nonsp1_sec <- 15*Nonsp_sec

Nonsp_irri             
Nonsp1_irri <- 15*Nonsp_irri

mean(Nonsp1_sec)
mean(Nonsp1_irri)

#############################
#############################
######### Effect of irrigation per group of species

### At community level
mean(Output.model$mean$effect.a0.sp)
mean(Output.model$q2.5$effect.a0.sp)
mean(Output.model$q97.5$effect.a0.sp)
# Negative proportion of distribution
mean(Output.model$sims.list$effect.a0.sp < 0)

### Species of agricultural habitats
mean(Output.model$mean$effect.a0.sp[c(45,1,15,7,22,29,18,33,12,9,47,30,40,5,20)])
mean(Output.model$q2.5$effect.a0.sp[c(45,1,15,7,22,29,18,33,12,9,47,30,40,5,20)])
mean(Output.model$q97.5$effect.a0.sp[c(45,1,15,7,22,29,18,33,12,9,47,30,40,5,20)])
# Negative proportion of distribution
mean((Output.model$sims.list$effect.a0.sp[,c(45,1,15,7,22,29,18,33,12,9,47,30,40,5,20)])<0)

### Species of shrublands
# Negative proportion of distribution
mean((Output.model$sims.list$effect.a0.sp[,c(23,3,27,26,44,42,43,38,24)])<0)

### Species of forest habitats
# Negative proportion of distribution
mean((Output.model$sims.list$effect.a0.sp[,c(13,21,34)])<0)

### Non-specialist species
# Negative proportion of distribution
mean((Output.model$sims.list$effect.a0.sp[,c(8,39,10,19,35,25,28,46,41,16,36,14,11,31,6)])<0)

#############################
#############################
######### probability to be negatively affected by irrigation
#For each species (Table 1)
mean((Output.model$sims.list$effect.a0.sp[,1])<0)
mean((Output.model$sims.list$effect.a0.sp[,2])<0)
mean((Output.model$sims.list$effect.a0.sp[,3])<0)
mean((Output.model$sims.list$effect.a0.sp[,4])<0)
mean((Output.model$sims.list$effect.a0.sp[,5])<0)
mean((Output.model$sims.list$effect.a0.sp[,6])<0)
mean((Output.model$sims.list$effect.a0.sp[,7])<0)
mean((Output.model$sims.list$effect.a0.sp[,8])<0)
mean((Output.model$sims.list$effect.a0.sp[,9])<0)
mean((Output.model$sims.list$effect.a0.sp[,10])<0)
mean((Output.model$sims.list$effect.a0.sp[,11])<0)
mean((Output.model$sims.list$effect.a0.sp[,12])<0)
mean((Output.model$sims.list$effect.a0.sp[,13])<0)
mean((Output.model$sims.list$effect.a0.sp[,14])<0)
mean((Output.model$sims.list$effect.a0.sp[,15])<0)
mean((Output.model$sims.list$effect.a0.sp[,16])<0)
mean((Output.model$sims.list$effect.a0.sp[,17])<0)
mean((Output.model$sims.list$effect.a0.sp[,18])<0)
mean((Output.model$sims.list$effect.a0.sp[,19])<0)
mean((Output.model$sims.list$effect.a0.sp[,20])<0)
mean((Output.model$sims.list$effect.a0.sp[,21])<0)
mean((Output.model$sims.list$effect.a0.sp[,22])<0)
mean((Output.model$sims.list$effect.a0.sp[,23])<0)
mean((Output.model$sims.list$effect.a0.sp[,24])<0)
mean((Output.model$sims.list$effect.a0.sp[,25])<0)
mean((Output.model$sims.list$effect.a0.sp[,26])<0)
mean((Output.model$sims.list$effect.a0.sp[,27])<0)
mean((Output.model$sims.list$effect.a0.sp[,28])<0)
mean((Output.model$sims.list$effect.a0.sp[,29])<0)
mean((Output.model$sims.list$effect.a0.sp[,30])<0)
mean((Output.model$sims.list$effect.a0.sp[,31])<0)
mean((Output.model$sims.list$effect.a0.sp[,32])<0)
mean((Output.model$sims.list$effect.a0.sp[,33])<0)
mean((Output.model$sims.list$effect.a0.sp[,34])<0)
mean((Output.model$sims.list$effect.a0.sp[,35])<0)
mean((Output.model$sims.list$effect.a0.sp[,36])<0)
mean((Output.model$sims.list$effect.a0.sp[,37])<0)
mean((Output.model$sims.list$effect.a0.sp[,38])<0)
mean((Output.model$sims.list$effect.a0.sp[,39])<0)
mean((Output.model$sims.list$effect.a0.sp[,40])<0)
mean((Output.model$sims.list$effect.a0.sp[,41])<0)
mean((Output.model$sims.list$effect.a0.sp[,42])<0)
mean((Output.model$sims.list$effect.a0.sp[,43])<0)
mean((Output.model$sims.list$effect.a0.sp[,44])<0)
mean((Output.model$sims.list$effect.a0.sp[,45])<0)
mean((Output.model$sims.list$effect.a0.sp[,46])<0)
mean((Output.model$sims.list$effect.a0.sp[,47])<0)


######################################################
##################### Detection ######################
## Mean detection probability
mean(Output.model$sims.list$p[,,,,])

## Average detection probability per species
mean(Output.model$sims.list$p[,,,,1])
mean(Output.model$sims.list$p[,,,,2])
mean(Output.model$sims.list$p[,,,,3])
mean(Output.model$sims.list$p[,,,,4])
mean(Output.model$sims.list$p[,,,,5])
mean(Output.model$sims.list$p[,,,,6])
mean(Output.model$sims.list$p[,,,,7])
mean(Output.model$sims.list$p[,,,,8])
mean(Output.model$sims.list$p[,,,,9])
mean(Output.model$sims.list$p[,,,,10])
mean(Output.model$sims.list$p[,,,,11])
mean(Output.model$sims.list$p[,,,,12])
mean(Output.model$sims.list$p[,,,,13])
mean(Output.model$sims.list$p[,,,,14])
mean(Output.model$sims.list$p[,,,,15])
mean(Output.model$sims.list$p[,,,,16])
mean(Output.model$sims.list$p[,,,,17])
mean(Output.model$sims.list$p[,,,,18])
mean(Output.model$sims.list$p[,,,,19])
mean(Output.model$sims.list$p[,,,,20])
mean(Output.model$sims.list$p[,,,,21])
mean(Output.model$sims.list$p[,,,,22])
mean(Output.model$sims.list$p[,,,,23])
mean(Output.model$sims.list$p[,,,,24])
mean(Output.model$sims.list$p[,,,,25])
mean(Output.model$sims.list$p[,,,,26])
mean(Output.model$sims.list$p[,,,,27])
mean(Output.model$sims.list$p[,,,,28])
mean(Output.model$sims.list$p[,,,,29])
mean(Output.model$sims.list$p[,,,,30])
mean(Output.model$sims.list$p[,,,,31])
mean(Output.model$sims.list$p[,,,,32])
mean(Output.model$sims.list$p[,,,,33])
mean(Output.model$sims.list$p[,,,,34])
mean(Output.model$sims.list$p[,,,,35])
mean(Output.model$sims.list$p[,,,,36])
mean(Output.model$sims.list$p[,,,,37])
mean(Output.model$sims.list$p[,,,,38])
mean(Output.model$sims.list$p[,,,,39])
mean(Output.model$sims.list$p[,,,,40])
mean(Output.model$sims.list$p[,,,,41])
mean(Output.model$sims.list$p[,,,,42])
mean(Output.model$sims.list$p[,,,,43])
mean(Output.model$sims.list$p[,,,,44])
mean(Output.model$sims.list$p[,,,,45])
mean(Output.model$sims.list$p[,,,,46])
mean(Output.model$sims.list$p[,,,,47])

###########
### Estimation of the overall reduction in site-level species richness

SiteReduction <- cbind(c(100-(100/mean(Output.model$mean$Nsite[2,c(1,2,3,4,5,6,7,8,9)])*mean(Output.model$mean$Nsite[2,c(10,11,12)])),
                 100-(100/mean(Output.model$mean$Nsite[3,c(1,2,3,4,5,6,7,8,9)])*mean(Output.model$mean$Nsite[3,c(10,11,12)])),
                 100-(100/mean(Output.model$mean$Nsite[4,c(1,2,3,4,5,6,7,8,9)])*mean(Output.model$mean$Nsite[4,c(10,11,12)])),
                 100-(100/mean(Output.model$mean$Nsite[6,c(1,2,3,4,5,6,7,8,9)])*mean(Output.model$mean$Nsite[6,c(10,11,12)])),
                 100-(100/mean(Output.model$mean$Nsite[7,c(1,2,3,4,5,6,7,8,9)])*mean(Output.model$mean$Nsite[7,c(10,11,12)])),
                 100-(100/mean(Output.model$mean$Nsite[8,c(1,2,3,4,5,6,7,8,9)])*mean(Output.model$mean$Nsite[8,c(10,11,12)])),
                 100-(100/mean(Output.model$mean$Nsite[9,c(1,2,3,4,5,6,7,8,9)])*mean(Output.model$mean$Nsite[9,c(10,11,12)])),
                 100-(100/mean(Output.model$mean$Nsite[10,c(1,2,3,4,5,6,7,8,9)])*mean(Output.model$mean$Nsite[10,c(10,11,12)])),
                 100-(100/mean(Output.model$mean$Nsite[11,c(1,2)])*mean(Output.model$mean$Nsite[11,c(3,4,5,6,7,8,9,10,11,12)])),
                 100-(100/mean(Output.model$mean$Nsite[12,c(1,2)])*mean(Output.model$mean$Nsite[12,c(3,4,5,6,7,8,9,10,11,12)])),
                 100-(100/mean(Output.model$mean$Nsite[13,c(1,2)])*mean(Output.model$mean$Nsite[13,c(3,4,5,6,7,8,9,10,11,12)])),
                 100-(100/mean(Output.model$mean$Nsite[14,c(1,2)])*mean(Output.model$mean$Nsite[14,c(3,4,5,6,7,8,9,10,11,12)])),
                 100-(100/mean(Output.model$mean$Nsite[15,c(1,2)])*mean(Output.model$mean$Nsite[15,c(3,4,5,6,7,8,9,10,11,12)])),
                 100-(100/mean(Output.model$mean$Nsite[16,c(1,2)])*mean(Output.model$mean$Nsite[16,c(3,4,5,6,7,8,9,10,11,12)])),
                 100-(100/mean(Output.model$mean$Nsite[17,c(1,2)])*mean(Output.model$mean$Nsite[17,c(3,4,5,6,7,8,9,10,11,12)])),
                 100-(100/mean(Output.model$mean$Nsite[18,c(1,2)])*mean(Output.model$mean$Nsite[18,c(3,4,5,6,7,8,9,10,11,12)])),
                 100-(100/mean(Output.model$mean$Nsite[19,c(1,2)])*mean(Output.model$mean$Nsite[19,c(3,4,5,6,7,8,9,10,11,12)]))))

mean(SiteReduction)
