##############################################################################
## Script to compute errors in identification of Dominance relations.
## Source for Appendix 1 chart. Search for "FIGURE" in text.
##############################################################################

library(data.table)

##############################################################################

# Change this to the directory on your computer. 
setwd("D:\\MyDocus\\Simulation\\NetLogo\\Games\\HawkDove\\Evolving-Game-Players\\Data_Processing")

##############################################################################
# Import Data
# (If you have run this once already and saved D.rds,
# you can skip it and just load D.rds)

##############################################################################

# Import the attributes for each batch run number

A <- as.data.table(read.csv("EvolvingGamePlayers experiment-DomErrors-table.csv", skip=6))
dim(A)
names(A)
str(A)

##############################################################################
# Import one file to get structure

D <- as.data.table(read.csv("Dom_Errors_BS1.csv"))
D <- cbind(run_number=1,D)
dim(D)
D <- D[0]

# Import all the corresponding files

for (i in sort(A[,X.run.number.])) {
	d_tmp <- as.data.table(
		read.csv(
			paste0("Dom_Errors_BS", i, ".csv")
			)
		)
	# Fix problem from NetLogo. No row for Dominant=0.
	d_tmp <- rbind(d_tmp, 
			d_tmp[,.(ticks=10+max(ticks),Expectant=min(Expectant),Hawkish=min(Hawkish),Dominant=0),by=.(Pop,PRed,VOverC,G1Dom,G2Dom,NoneDom,Strategy)]
			)
	D <- rbind(D, 
		cbind(run_number=i, d_tmp)
		)
}

dim(D)
head(D)
tail(D)

# Save data set as a single file for quicker access in future.

saveRDS(D, file="D.rds")

##############################################################################
# Import previously compiled data set D.rds

D <- readRDS("D.rds")

##############################################################################
# Focus on the fields we want, and rename them.

D2 <- D[,.(reps=.N, Expectant=sum(Expectant), Hawkish=sum(Hawkish), Dominant=sum(Dominant), G1Dom=sum(G1Dom), G2Dom=sum(G2Dom), NoneDom=sum(NoneDom)),by=.(ticks, Pop, PRed, VOverC, Strategy)]
dim(D2)
head(D2)

##############################################################################
# Transform down

D3 <- rbind(
		cbind(Convergence="Out-Play", D2[,.(Value=Hawkish, reps, ticks, Pop, PRed, VOverC, Strategy)]),
		cbind(Convergence="Expectations", D2[,.(Value=Expectant, reps, ticks, Pop, PRed, VOverC, Strategy)]),
		cbind(Convergence="Not Converged", D2[,.(Value=Dominant, reps, ticks, Pop, PRed, VOverC, Strategy)])
	)
dim(D3)

##############################################################################

# ggplot code

library(ggplot2)

line_plot <- function(Data) {
	max_reps <- max(Data[,reps])
	ggplot(data=Data, aes(x=ticks, y=100*Value/(reps * Pop), group=Convergence)) +
  		geom_line(aes(color=Convergence), size=1)+
		xlab("Round")+
		ylab("% of Runs")+
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

##############################################################################

# Draw plots

# Appendix 1 FIGURE:
line_plot(D3[PRed==80 & VOverC==0.9 & Strategy=="BL-Play-MSNE"])
ggsave("FigA1.1_DominanceConvergence.jpg", width=6, height=6, units=c("in"), dpi=300)

line_plot(D3[PRed==80 & VOverC==0.9 & Strategy=="BL-Expect-H-0.5"])
line_plot(D3[PRed==50 & VOverC==0.9 & Strategy=="BL-Expect-D-D"])
line_plot(D3[PRed==50 & VOverC==0.9 & Strategy=="BL-Expect-H-H"])
line_plot(D3[PRed==90 & VOverC==0.9 & Strategy=="BL-Play-MSNE"])



##############################################################################
# (Not used in paper.)
##############################################################################

heatm <- function(Data, Caption, Legend_Caption, Low_Col, High_Col) {
	ggplot(Data, aes(VOverC, PRed, fill= Z)) + 
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

##############################################################################

save_heatm <- function(Filename, Data, Caption, Legend_Caption, Low_Col, High_Col) {
	heatm(Data, Strategy, Caption, Legend_Caption, Low_Col, High_Col)
	ggsave(Filename)
}

##############################################################################

D4 <- D2[ticks==10,.(Z=mean(100*G2Dom/(reps*Pop))), by=.(Strategy=Strategy, PRed=PRed, VOverC=VOverC)]
dim(D4)
head(D4)

##############################################################################

unique(D4[,Strategy])

heatm(D4[Strategy=="BL-Play-MSNE"], Caption="Play MSNE", Legend_Caption="Reds Dominance\n  (% of Runs)", Low_Col="darkblue", High_Col="red") # Play MSNE as first move
heatm(D4[Strategy=="BL-Expect-H-H"], Caption="Expect Hawk", Legend_Caption="Reds Dominance\n  (% of Runs)", Low_Col="darkblue", High_Col="red") # Play best response to opponents playing Hawk (i.e. play Dove)
heatm(D4[Strategy=="BL-Expect-D-D"], Caption="Expect Dove", Legend_Caption="Reds Dominance\n  (% of Runs)", Low_Col="darkblue", High_Col="red") # Play best response to opponents playing Dove (i.e. play Hawk)
heatm(D4[Strategy=="BL-Expect-H-0.5"], Caption="Expect Hawk with p=0.5", Legend_Caption="Reds Dominance\n  (% of Runs)", Low_Col="darkblue", High_Col="red") # Play best response to opponents playing Hawk with probability = 0.5
heatm(D4[Strategy=="BL-Play-Random"], "Play Hawk with p=0.5", Legend_Caption="Reds Dominance\n  (% of Runs)", Low_Col="darkblue", High_Col="red") # Play Hawk with probability = 0.5

##############################################################################

D5 <- D2[Dominant==0,.(Z=min(ticks)), by=.(Strategy=Strategy, PRed=PRed, VOverC=VOverC)]

heatm(D5[Strategy=="BL-Play-MSNE"], Caption="Play MSNE", Legend_Caption="Convergence\n  Round", Low_Col="green", High_Col="red") # Play MSNE as first move
heatm(D5[Strategy=="BL-Expect-H-H"], Caption="Expect Hawk", Legend_Caption="Convergence\n  Round", Low_Col="green", High_Col="red") # Play best response to opponents playing Hawk (i.e. play Dove)
heatm(D5[Strategy=="BL-Expect-D-D"], Caption="Expect Dove", Legend_Caption="Convergence\n  Round", Low_Col="green", High_Col="red") # Play best response to opponents playing Dove (i.e. play Hawk)
heatm(D5[Strategy=="BL-Expect-H-0.5"], Caption="Expect Hawk with p=0.5", Legend_Caption="Convergence\n  Round", Low_Col="green", High_Col="red") # Play best response to opponents playing Hawk with probability = 0.5
heatm(D5[Strategy=="BL-Play-Random"], "Play Hawk with p=0.5", Legend_Caption="Convergence\n  Round", Low_Col="green", High_Col="red") # Play Hawk with probability = 0.5

##############################################################################


D6 <- D2[Expectant==0,.(Z=min(ticks)), by=.(Strategy=Strategy, PRed=PRed, VOverC=VOverC)]
#D6 <- D2[Hawkish==0,.(Z=min(ticks)), by=.(Strategy=Strategy, PRed=PRed, VOverC=VOverC)]


heatm(D6[Strategy=="BL-Play-MSNE"], Caption="Play MSNE", Legend_Caption="Convergence\n  Round", Low_Col="green", High_Col="red") # Play MSNE as first move
heatm(D6[Strategy=="BL-Expect-H-H"], Caption="Expect Hawk", Legend_Caption="Convergence\n  Round", Low_Col="green", High_Col="red") # Play best response to opponents playing Hawk (i.e. play Dove)
heatm(D6[Strategy=="BL-Expect-D-D"], Caption="Expect Dove", Legend_Caption="Convergence\n  Round", Low_Col="green", High_Col="red") # Play best response to opponents playing Dove (i.e. play Hawk)
heatm(D6[Strategy=="BL-Expect-H-0.5"], Caption="Expect Hawk with p=0.5", Legend_Caption="Convergence\n  Round", Low_Col="green", High_Col="red") # Play best response to opponents playing Hawk with probability = 0.5
heatm(D6[Strategy=="BL-Play-Random"], "Play Hawk with p=0.5", Legend_Caption="Convergence\n  Round", Low_Col="green", High_Col="red") # Play Hawk with probability = 0.5

##############################################################################
# If we use the Expectant method, where are there errors after round 200?

D7 <- D2[ticks==250,.(Z=sum(100*Expectant/(reps*Pop))), by=.(Strategy=Strategy, PRed=PRed, VOverC=VOverC)]
#D7 <- D2[ticks==250,.(Z=sum(100*Hawkish/(reps*Pop))), by=.(Strategy=Strategy, PRed=PRed, VOverC=VOverC)]
#D7 <- D2[ticks==200,.(Z=sum(Expectant)), by=.(Strategy=Strategy, PRed=PRed, VOverC=VOverC)]
D7 <- D2[ticks==250,.(Z=sum(100*Dominant/(reps*Pop))), by=.(Strategy=Strategy, PRed=PRed, VOverC=VOverC)]

heatm(D7[Strategy=="BL-Play-MSNE"], Caption="Play MSNE", Legend_Caption="Errors\n (% of Runs)", Low_Col="white", High_Col="red") # Play MSNE as first move
heatm(D7[Strategy=="BL-Expect-H-H"], Caption="Expect Hawk", Legend_Caption="Errors\n (% of Runs)", Low_Col="white", High_Col="red") # Play best response to opponents playing Hawk (i.e. play Dove)
heatm(D7[Strategy=="BL-Expect-D-D"], Caption="Expect Dove", Legend_Caption="Errors\n (% of Runs)", Low_Col="white", High_Col="red") # Play best response to opponents playing Dove (i.e. play Hawk)
heatm(D7[Strategy=="BL-Expect-H-0.5"], Caption="Expect Hawk with p=0.5", Legend_Caption="Errors\n (% of Runs)", Low_Col="white", High_Col="red") # Play best response to opponents playing Hawk with probability = 0.5
heatm(D7[Strategy=="BL-Play-Random"], "Play Hawk with p=0.5", Legend_Caption="Errors\n (% of Runs)", Low_Col="white", High_Col="red") # Play Hawk with probability = 0.5

##############################################################################

