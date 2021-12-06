##############################################################################
## Script to create heatmaps from the results .xlsx file
## Source for figures 1, 2, 3, 4, and 5. Search for "FIGURES".
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
#D <- as.data.table(read.xlsx(file="FSharpResults.xlsx", sheetIndex=1, header=TRUE))
#D <- as.data.table(read.xlsx(file="FSharpResults_RN2000.xlsx", sheetIndex=1, header=TRUE))
dim(D)
head(D)
names(D)

##############################################################################

# Focus on the fields we want, and rename them.

D2 <- D[, .(Stage2Mode=Stage2Mode, AgentCount=AgentCount, MSNE=HawkPortion, Perc_Red=RedAgentPercentage, Z=FirstSeparationOfColors_DominatedByRed_Count)]
dim(D2)
head(D2)
tail(D2)

##############################################################################

# Set up heatmap function

# Draw the heatmaps for Reds Dominance

heatm <- function(Data, Mode, Agents, Caption, Legend_Caption, Low_Col, High_Col) {
	ggplot(Data[Stage2Mode==Mode & AgentCount==Agents], aes(MSNE, Perc_Red, fill= Z)) + 
  		geom_tile() +
		scale_fill_gradient(Legend_Caption, low=Low_Col, high=High_Col) +
		#theme_ipsum() +
  		labs(x="MSNE = V / C", y="Reds (% of Pop)", caption=Caption) +
  		theme(
			plot.caption = element_text(size=16, hjust=0),
			axis.text.x= element_text(size=12),
			axis.text.y= element_text(size=12),
			axis.title.x= element_text(size=12),
			axis.title.y= element_text(size=12),
			legend.title=element_text(size=12),
			legend.text=element_text(size=12)
		)
}

save_heatm <- function(Filename, Data, Mode, Agents, Caption, Legend_Caption, Low_Col, High_Col) {
	heatm(Data, Mode, Agents, Caption, Legend_Caption, Low_Col, High_Col)
	ggsave(Filename, width=6, height=6, units=c("in"), dpi=300)
}

##############################################################################

# Dominance

heatm(D2, Mode="stage2_nmse", Agents=200, Caption="Play MSNE", Legend_Caption="Reds Dominance\n  (% of Runs)", Low_Col="darkblue", High_Col="red") # Play MSNE as first move
heatm(D2, Mode="stage2_hawks", Agents=200, Caption="Expect Hawk", Legend_Caption="Reds Dominance\n  (% of Runs)", Low_Col="darkblue", High_Col="red") # Play best response to opponents playing Hawk (i.e. play Dove)
heatm(D2, Mode="stage2_dove", Agents=200, Caption="Expect Dove", Legend_Caption="Reds Dominance\n  (% of Runs)", Low_Col="darkblue", High_Col="red") # Play best response to opponents playing Dove (i.e. play Hawk)
heatm(D2, Mode="stage2_half", Agents=200, Caption="Expect Hawk with p=0.5", Legend_Caption="Reds Dominance\n  (% of Runs)", Low_Col="darkblue", High_Col="red") # Play best response to opponents playing Hawk with probability = 0.5
heatm(D2, Mode="stage2_random", Agents=200, "Play Hawk with p=0.5", Legend_Caption="Reds Dominance\n  (% of Runs)", Low_Col="darkblue", High_Col="red") # Play Hawk with probability = 0.5

##############################################################################

# FIGURES

save_heatm(Filename="Fig1_HDB1_PlayMSNE.jpg", Data=D2, Mode="stage2_nmse", Agents=200, Caption="Play MSNE", Legend_Caption="Reds Dominance\n  (% of Runs)", Low_Col="white", High_Col="red") # Play MSNE as first move
save_heatm(Filename="Fig2_HDB2_ExpectDove_PlayHawk.jpg", Data=D2, Mode="stage2_dove", Agents=200, Caption="Expect Dove", Legend_Caption="Reds Dominance\n  (% of Runs)", Low_Col="white", High_Col="red") # Play best response to opponents playing Dove (i.e. play Hawk)
save_heatm(Filename="Fig3_HDB3_ExpectHawk_PlayDove.jpg", Data=D2, Mode="stage2_hawks", Agents=200, Caption="Expect Hawk", Legend_Caption="Reds Dominance\n  (% of Runs)", Low_Col="white", High_Col="red") # Play best response to opponents playing Hawk (i.e. play Dove)
save_heatm(Filename="Fig4_HDB4_Play_0.5_0.5.jpg", Data=D2, Mode="stage2_random", Agents=200, "Play Hawk with p=0.5", Legend_Caption="Reds Dominance\n  (% of Runs)", Low_Col="white", High_Col="red") # Play Hawk with probability = 0.5
save_heatm(Filename="Fig5_HDB5_Expect_0.5_0.5.jpg", Data=D2, Mode="stage2_half", Agents=200, Caption="Expect Hawk with p=0.5", Legend_Caption="Reds Dominance\n  (% of Runs)", Low_Col="white", High_Col="red") # Play best response to opponents playing Hawk with probability = 0.5




##############################################################################
# Not used in paper.
##############################################################################

# Round for Convergence

# Avg
D3 <- D[, .(Stage2Mode=Stage2Mode, AgentCount=AgentCount, MSNE=HawkPortion, Perc_Red=RedAgentPercentage, Z=FirstSeparationOfColors_Avg)]
# Min
D3 <- D[, .(Stage2Mode=Stage2Mode, AgentCount=AgentCount, MSNE=HawkPortion, Perc_Red=RedAgentPercentage, Z=FirstSeparationOfColors_Min)]
# Max
D3 <- D[, .(Stage2Mode=Stage2Mode, AgentCount=AgentCount, MSNE=HawkPortion, Perc_Red=RedAgentPercentage, Z=FirstSeparationOfColors_Max)]

heatm(D3, Mode="stage2_nmse", Agents=200, Caption="Play MSNE", Legend_Caption="Convergence\n  Round", Low_Col="green", High_Col="red") # Play MSNE as first move
heatm(D3, Mode="stage2_hawks", Agents=200, Caption="Expect Hawk", Legend_Caption="Convergence\n  Round", Low_Col="green", High_Col="red") # Play best response to opponents playing Hawk (i.e. play Dove)
heatm(D3, Mode="stage2_dove", Agents=200, Caption="Expect Dove", Legend_Caption="Convergence\n  Round", Low_Col="green", High_Col="red") # Play best response to opponents playing Dove (i.e. play Hawk)
heatm(D3, Mode="stage2_half", Agents=200, Caption="Expect Hawk with p=0.5", Legend_Caption="Convergence\n  Round", Low_Col="green", High_Col="red") # Play best response to opponents playing Hawk with probability = 0.5
heatm(D3, Mode="stage2_random", Agents=200, "Play Hawk with p=0.5", Legend_Caption="Convergence\n  Round", Low_Col="green", High_Col="red") # Play Hawk with probability = 0.5

##############################################################################

# Did the run converge?

# Count
D4 <- D[, .(Stage2Mode=Stage2Mode, AgentCount=AgentCount, MSNE=HawkPortion, Perc_Red=RedAgentPercentage, Z=FirstSeparationOfColors_Count)]

heatm(D4, Mode="stage2_nmse", Agents=200, Caption="Play MSNE", Legend_Caption="Converged?\n  (# Runs)", Low_Col="red", High_Col="green") # Play MSNE as first move
ggsave("RoundConv_nmse.png")
heatm(D4, Mode="stage2_hawks", Agents=200, Caption="Expect Hawk", Legend_Caption="Converged?\n  (# Runs)", Low_Col="red", High_Col="green") # Play best response to opponents playing Hawk (i.e. play Dove)
ggsave("RoundConv_hawks.png")
heatm(D4, Mode="stage2_dove", Agents=200, Caption="Expect Dove", Legend_Caption="Converged?\n  (# Runs)", Low_Col="red", High_Col="green") # Play best response to opponents playing Dove (i.e. play Hawk)
ggsave("RoundConv_dove.png")
heatm(D4, Mode="stage2_half", Agents=200, Caption="Expect Hawk with p=0.5", Legend_Caption="Converged?\n  (# Runs)", Low_Col="red", High_Col="green") # Play best response to opponents playing Hawk with probability = 0.5
ggsave("RoundConv_half.png")
heatm(D4, Mode="stage2_random", Agents=200, "Play Hawk with p=0.5", Legend_Caption="Converged?\n  (# Runs)", Low_Col="red", High_Col="green") # Play Hawk with probability = 0.5
ggsave("RoundConv_random.png")

##############################################################################




# Done!
