##############################################################################
## Script to create pie shop from the results .xlsx file
## Source for figures in Appendix II.
##############################################################################

# Libraries

library(ggplot2)
library(hrbrthemes)
#library(viridis) # Only if you want this colorscheme
#install.packages("xlsx") # If you don't have it already
library("xlsx")
library("data.table")

##############################################################################

# Change this to the directory on your computer. 
setwd("D:\\MyDocus\\Simulation\\NetLogo\\Games\\HawkDove\\Evolving-Game-Players\\Data_Processing")

##############################################################################
# Import data

D <- as.data.table(read.xlsx(file="rerun-june2021-results_PlusRandom.xlsx", sheetIndex=1, header=TRUE))
dim(D)
head(D)
names(D)

##############################################################################
# Focus on the fields we want, and rename them.

D2 <- rbind(
	D[, .(Stage2Mode=Stage2Mode, AgentCount=AgentCount, MSNE=as.factor(HawkPortion), Perc_Red=as.factor(RedAgentPercentage), Z=FirstSeparationOfColors_DominatedByRed_Count, Group="Reds")],
	D[, .(Stage2Mode=Stage2Mode, AgentCount=AgentCount, MSNE=as.factor(HawkPortion), Perc_Red=as.factor(RedAgentPercentage), Z=FirstSeparationOfColors_DominatedByBlue_Count, Group="Blues")],
	D[, .(Stage2Mode=Stage2Mode, AgentCount=AgentCount, MSNE=as.factor(HawkPortion), Perc_Red=as.factor(RedAgentPercentage), Z=FirstSeparationOfColors_DominatedByNone_Count, Group="None")]
)

dim(D2)
head(D2)
tail(D2)
str(D2)

##############################################################################
# Set up pie shop function

# Draw the Pie Shop for Dominance
mycols <- c("darkblue", "green", "red")

piesh <- function(Data, Mode, Agents, Caption, Legend_Caption, Low_Col, High_Col) {
	ggplot(Data[Stage2Mode==Mode & AgentCount==Agents], aes(x = "", y = Z, fill = Group)) +
		geom_bar(width = 1, stat = "identity", color = "white") +
#		geom_bar(width = 1, stat = "identity", color = "black") +
  		coord_polar("y", start = 0)+
#		geom_text(aes(label = Z), color="white", size=3) +
		scale_fill_manual(values = mycols, name=Legend_Caption) +
		facet_grid(Perc_Red ~ MSNE, switch="both", as.table=FALSE) +
		theme_void() +
		theme(
			plot.title = element_text(size=16),
			axis.title.x = element_text(size=14), 
			axis.title.y = element_text(size=14, angle=90),
			strip.text.x = element_text(size=12),
			strip.text.y = element_text(size=12),
			legend.text = element_text(size=12),
			legend.title = element_text(size=12)
		) +
		labs(title = paste(Caption, "\n"),
#       		subtitle = "(margins= TRUE)",
       		x = "Reds (% of Pop)\n", y = "\nV / C")
}

##############################################################################
# Dominance Pie Shop FIGURES for Appendix II

piesh(D2, Mode="stage2_nmse", Agents=200, Caption="Play MSNE", Legend_Caption="Dominance By ", Low_Col="blue", High_Col="red") # Play MSNE as first move
ggsave("FigA2.1_PieShop_PlayMSNE.jpg", width=6, height=6, units=c("in"), dpi=300)

piesh(D2, Mode="stage2_dove", Agents=200, Caption="Expect Dove", Legend_Caption="Dominance By ", Low_Col="blue", High_Col="red") # Play best response to opponents playing Dove (i.e. play Hawk)
ggsave("FigA2.2_PieShop_ExpectDove.jpg", width=6, height=6, units=c("in"), dpi=300)

piesh(D2, Mode="stage2_hawks", Agents=200, Caption="Expect Hawk", Legend_Caption="Dominance By ", Low_Col="blue", High_Col="red") # Play best response to opponents playing Hawk (i.e. play Dove)
ggsave("FigA2.3_PieShop_ExpectHawk.jpg", width=6, height=6, units=c("in"), dpi=300)

piesh(D2, Mode="stage2_random", Agents=200, "Play Hawk with p=0.5", Legend_Caption="Dominance By ", Low_Col="blue", High_Col="red") # Play Hawk with probability = 0.5
ggsave("FigA2.4_PieShop_PlayHawk_p05.jpg", width=6, height=6, units=c("in"), dpi=300)

piesh(D2, Mode="stage2_half", Agents=200, Caption="Expect Hawk with p=0.5", Legend_Caption="Dominance By ", Low_Col="blue", High_Col="red") # Play best response to opponents playing Hawk with probability = 0.5
ggsave("FigA2.5_PieShop_ExpectHawk_p05.jpg", width=6, height=6, units=c("in"), dpi=300)

##############################################################################

# Done!
