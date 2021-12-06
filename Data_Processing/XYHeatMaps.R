##############################################################################
## Script to create a grid of heatmaps from the NetLogo results .csv file
## Source for figures 6, 7, and 8. Search for "FIGURE".
##############################################################################

# Libraries

library(ggplot2)
library(hrbrthemes)
#library(viridis) # Only if you want this colorscheme
library("data.table")

##############################################################################

# Change this to the directory on your computer. 
setwd("D:\\MyDocus\\Simulation\\NetLogo\\Games\\HawkDove\\Evolving-Game-Players\\Data_Processing")

##############################################################################
# Import data

D <- as.data.table(read.csv(file="EvolvingGamePlayers experiment-Amadae-EmptyMemory-Play-XY-table.csv", header=TRUE, stringsAsFactors=TRUE, sep = ",", skip=6))
#D <- as.data.table(read.csv(file="EvolvingGamePlayers experiment-Amadae-EmptyMemory-Expect-XY-table.csv", header=TRUE, stringsAsFactors=TRUE, sep = ",", skip=6))
#D <- as.data.table(read.csv(file="EvolvingGamePlayers experiment-Amadae-EmptyMemory-Play-Neut-XY-table.csv", header=TRUE, stringsAsFactors=TRUE, sep = ",", skip=6))
#D <- as.data.table(read.csv(file="EvolvingGamePlayers experiment-Amadae-EmptyMemory-Expect-Neut-XY-table.csv", header=TRUE, stringsAsFactors=TRUE, sep = ",", skip=6))

#D <- as.data.table(read.csv(file="EvolvingGamePlayers experiment-MND-EmptyMemory-Play-XY-table.csv", header=TRUE, stringsAsFactors=TRUE, sep = ",", skip=6))
#D <- as.data.table(read.csv(file="EvolvingGamePlayers experiment-Mut-EmptyMemory-Play-XY-table.csv", header=TRUE, stringsAsFactors=TRUE, sep = ",", skip=6))


dim(D)
head(D)
names(D)

##############################################################################

# Focus on the fields we want, and rename them.

num_reps <- 100

D2 <- D[, .(run_number=X.run.number., strategy=Playing.Strategy, pop_size=Population.Size, msne=round(msne, 1), cost=Cost, perc_red=Perc.Group2, x=(Initial.X / 100), y=(Initial.Y / 100), z=(100 * num.pops.g2.dominant / num_reps), zh=(100 * num.pops.g2.hawkish / num_reps), zd=(100 * num.pops.with.group2.dom / num_reps), z_check=(0 == num.pops.none.dominant), unlimited_memory=(Unlimited.Memory. == "true"), finish_time=X.step.)]
dim(D2)
head(D2)
tail(D2)


##############################################################################

# Set up heatmap function

heatm <- function(Data, Caption, Legend_Caption, Low_Col, High_Col) {
	ggplot(Data, aes(msne, perc_red, fill= z)) + 
  		geom_tile() +
		scale_fill_gradient(Legend_Caption, low=Low_Col, high=High_Col, limits=c(0, 100)) +
		#theme_ipsum() +
  		labs(x="MSNE", y="Reds (% of Pop)", caption=Caption) +
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

save_heatm <- function(Filename, Data, Caption, Legend_Caption, Low_Col, High_Col) {
	heatm(Data, Caption, Legend_Caption, Low_Col, High_Col)
	ggsave(Filename, width=6, height=6, units=c("in"), dpi=300)
}

##############################################################################

# Draw the heatmaps for Reds Dominance

# Dominance

# Check initialization results we had before:
heatm(D2[10*x==round(10*msne) & 10*y==round(10*msne)], Caption="Initial Play = MSNE", Legend_Caption="Reds Dominance\n  (% of Runs)", Low_Col="darkblue", High_Col="red") # Play MSNE as first move
heatm(D2[x==0.5 & y==0.5], Caption="Initial Play = MS(0.5, 0.5)", Legend_Caption="Reds Dominance\n  (% of Runs)", Low_Col="darkblue", High_Col="red") # Play Random as first move
heatm(D2[x==0 & y==0], Caption="Initial Play = MS(0, 0)", Legend_Caption="Reds Dominance\n  (% of Runs)", Low_Col="darkblue", High_Col="red") # Play Random as first move
heatm(D2[x==1 & y==1], Caption="Initial Play = MS(1, 1)", Legend_Caption="Reds Dominance\n  (% of Runs)", Low_Col="darkblue", High_Col="red") # Play Random as first move
# Trivial cases
heatm(D2[x==1 & y==0], Caption="Initial Play = MS(1, 0)", Legend_Caption="Reds Dominance\n  (% of Runs)", Low_Col="darkblue", High_Col="red") # Play Random as first move
heatm(D2[x==0 & y==1], Caption="Initial Play = MS(0, 1)", Legend_Caption="Reds Dominance\n  (% of Runs)", Low_Col="darkblue", High_Col="red") # Play Random as first move

# Aggregation over x and y
heatm(D2[,.(z=mean(z)),by=.(msne, perc_red, pop_size, strategy)], Caption="Initial Play All MS", Legend_Caption="Reds Dominance\n  (% of Runs)", Low_Col="darkblue", High_Col="red") # Play Random as first move
heatm(D2[,.(z=mean(z)),by=.(msne, perc_red, pop_size, strategy)], Caption="Initial Expect All MS", Legend_Caption="Reds Dominance\n  (% of Runs)", Low_Col="darkblue", High_Col="red") # Play Random as first move

##############################################################################

# FIGURE 6
save_heatm("Fig6_HDB6_HeatMap_InitialPlayXY_AllMS.jpg", D2[,.(z=mean(z)),by=.(msne, perc_red, pop_size, strategy)], Caption="Initial Play All MS", Legend_Caption="Reds Dominance\n  (% of Runs)", Low_Col="white", High_Col="red") # Play Random as first move

#save_heatm("HeatMap_InitialPlayXY_MSNE.png", D2[x==msne & y==msne], Caption="Initial Play MSNE", Legend_Caption="Reds Dominance\n  (% of Runs)", Low_Col="white", High_Col="red") # Play Random as first move
#save_heatm("HeatMap_InitialPlayXY_MS_50_50.png", D2[x==0.5 & y==0.5], Caption="Initial Play MS(0.5, 0.5)", Legend_Caption="Reds Dominance\n  (% of Runs)", Low_Col="white", High_Col="red") # Play Random as first move
#save_heatm("HeatMap_InitialPlayXY_MS_100_100.png", D2[x==1 & y==1], Caption="Initial Play MS(1, 1)", Legend_Caption="Reds Dominance\n  (% of Runs)", Low_Col="white", High_Col="red") # Play Random as first move

#save_heatm("HeatMap_InitialExpectXY_AllMS.png", D2[,.(z=mean(z)),by=.(msne, perc_red, pop_size, strategy)], Caption="Initial Expect All MS", Legend_Caption="Reds Dominance\n  (% of Runs)", Low_Col="white", High_Col="red") # Play Random as first move
#save_heatm("HeatMap_InitialExpectXY_MSNE.png", D2[x==msne & y==msne], Caption="Initial Expect MSNE", Legend_Caption="Reds Dominance\n  (% of Runs)", Low_Col="white", High_Col="red") # Play Random as first move
#save_heatm("HeatMap_InitialExpectXY_MS_50_50.png", D2[x==0.5 & y==0.5], Caption="Initial Expect MS(0.5, 0.5)", Legend_Caption="Reds Dominance\n  (% of Runs)", Low_Col="white", High_Col="red") # Play Random as first move
#save_heatm("HeatMap_InitialExpectXY_MS_100_100.png", D2[x==1 & y==1], Caption="Initial Expect MS(1, 1)", Legend_Caption="Reds Dominance\n  (% of Runs)", Low_Col="white", High_Col="red") # Play Random as first move

##############################################################################

# Set up grid of heatmaps function

grid_heatm <- function(Data, Caption, Legend_Caption, Low_Col, High_Col) {
	ggplot(Data, aes(msne, perc_red, fill= z)) + 
  		geom_tile() +
		scale_fill_gradient(Legend_Caption, low=Low_Col, high=High_Col) +
		#theme_ipsum() +
  		labs(x="MSNE", y="Reds (% of Pop)", caption=Caption) +
  		facet_grid(paste0("r = ",y) ~ paste0("b = ",x), switch="both", as.table=FALSE) +
		theme(
			plot.caption = element_text(size=16, hjust=0),
			axis.text.x= element_text(size=11, angle = 45, vjust = 1, hjust=1),
			axis.text.y= element_text(size=11),
			axis.title.x= element_text(size=12),
			axis.title.y= element_text(size=12),
			legend.title=element_text(size=12),
			legend.text=element_text(size=12)
		) +
		scale_x_continuous(breaks=seq(0.25, 0.75, 0.25))

}

save_grid_heatm <- function(Filename, Data, Caption, Legend_Caption, Low_Col, High_Col) {
	grid_heatm(Data, Caption, Legend_Caption, Low_Col, High_Col)
	ggsave(Filename, width=6, height=6, units=c("in"), dpi=300)
}

##############################################################################

# Grid(x, y):
#xx <- c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)
xx <- c(0, 0.3, 0.5, 0.7, 1)
#xx <- c(0.5, 0.6, 0.7, 0.8, 0.9, 1)
yy <- c(0, 0.3, 0.5, 0.7, 1)
#yy <- c(0.5, 0.6, 0.7, 0.8, 0.9, 1)
grid_heatm(D2[x %in% xx & y %in% yy], Caption="By Initial Play MS(b, r)", Legend_Caption="Reds Dominance\n  (% of Runs)", Low_Col="white", High_Col="red") # Play mixed strategy as first move
grid_heatm(D2, Caption="By Initial Expect MS(b, r)", Legend_Caption="Reds Dominance\n  (% of Runs)", Low_Col="darkblue", High_Col="red") # Play mixed strategy as first move

##############################################################################

# FIGURE 7
save_grid_heatm("Fig7_HDB7_HeatMapGrid_InitialPlayXY.jpg", D2[x %in% xx & y %in% yy], Caption="By Initial Play MS(b, r)", Legend_Caption="Reds Dominance\n  (% of Runs)", Low_Col="white", High_Col="red") # Play mixed strategy as first move

#save_grid_heatm("HeatMapGrid_InitialExpectXY.png", D2, Caption="By Initial Expect MS(b, r)", Legend_Caption="Reds Dominance\n  (% of Runs)", Low_Col="white", High_Col="red") # Play mixed strategy as first move
#save_grid_heatm("HeatMapGrid_InitialPlayNeutXY.png", D2, Caption="By Initial Play MS(b, r)", Legend_Caption="Reds Dominance\n  (% of Runs)", Low_Col="white", High_Col="red") # Play mixed strategy as first move
#save_grid_heatm("HeatMapGrid_InitialExpectNeutXY.png", D2, Caption="By Initial Expect MS(b, r)", Legend_Caption="Reds Dominance\n  (% of Runs)", Low_Col="white", High_Col="red") # Play mixed strategy as first move

##############################################################################

# Set up alternative grid of heatmaps function

alt_grid_heatm <- function(Data, Caption, Legend_Caption, Low_Col, High_Col) {
	ggplot(Data, aes(x, y, fill= z)) + 
  		geom_tile() +
		scale_fill_gradient(Legend_Caption, low=Low_Col, high=High_Col) +
		#theme_ipsum() +
  		labs(x="P(Blues play Hawk)", y="P(Reds play Hawk)", caption=Caption) +
  		facet_grid(paste0(perc_red, "% R") ~ paste0("MSNE=",msne), switch="both", as.table=FALSE) +
		theme(
			plot.caption = element_text(size=16, hjust=0),
			axis.text.x= element_text(size=11, angle = 45, vjust = 1, hjust=1),
			axis.text.y= element_text(size=11),
			axis.title.x= element_text(size=12),
			axis.title.y= element_text(size=12),
			legend.title=element_text(size=12),
			legend.text=element_text(size=12)
		) +
		scale_x_continuous(breaks=seq(0, 1, 0.5))
}

alt_save_grid_heatm <- function(Filename, Data, Caption, Legend_Caption, Low_Col, High_Col) {
	alt_grid_heatm(Data, Caption, Legend_Caption, Low_Col, High_Col)
	ggsave(Filename, width=6, height=6, units=c("in"), dpi=300)
}

##############################################################################
# Grid(MSNE, % Red):

mm <- c(0.1, 0.3, 0.5, 0.7, 0.9)
gg <- c(10, 30, 50, 70, 90)

alt_grid_heatm(D2[msne %in% mm & perc_red %in% gg], Caption="By MSNE and % in Red Group", Legend_Caption="Reds Dominance\n  (% of Runs)", Low_Col="white", High_Col="red") # Expect mixed strategy as first move
alt_grid_heatm(D2, Caption="By Initial Expect MS(b, r)", Legend_Caption="Reds Dominance\n  (% of Runs)", Low_Col="darkblue", High_Col="red") # Expect mixed strategy as first move

# FIGURE 8
alt_save_grid_heatm("Fig8_HDB8_AltHeatMapGrid_InitialPlayXY.jpg", D2[msne %in% mm & perc_red %in% gg], Caption="By MSNE and % in Red Group", Legend_Caption="Reds Dominance\n  (% of Runs)", Low_Col="white", High_Col="red") # Play mixed strategy as first move

#alt_save_grid_heatm("AltHeatMapGrid_InitialExpectXY.png", D2, Caption="By Initial Expect MS(b, r)", Legend_Caption="Reds Dominance\n  (% of Runs)", Low_Col="white", High_Col="red") # Expect mixed strategy as first move
#alt_save_grid_heatm("AltHeatMapGrid_InitialPlayNeutXY.png", D2, Caption="By Initial Play MS(b, r)", Legend_Caption="Reds Dominance\n  (% of Runs)", Low_Col="white", High_Col="red") # Play mixed strategy as first move
#alt_save_grid_heatm("AltHeatMapGrid_InitialExpectNeutXY.png", D2, Caption="By Initial Expect MS(b, r)", Legend_Caption="Reds Dominance\n  (% of Runs)", Low_Col="white", High_Col="red") # Expect mixed strategy as first move




###############################################################################
# Time to Dominance
# (Not used in paper.)

# Set up heatmap function

num_rounds <- 1000

time_heatm <- function(Data, Caption, Legend_Caption, Low_Col, High_Col) {
	ggplot(Data, aes(msne, perc_red, fill=(finish_time))) + 
  		geom_tile() +
		scale_fill_gradient(Legend_Caption, low=Low_Col, high=High_Col, limits=c(0, num_rounds)) +
		#theme_ipsum() +
  		labs(x="MSNE", y="Reds (% of Pop)", caption=Caption) +
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

save_time_heatm <- function(Filename, Data, Caption, Legend_Caption, Low_Col, High_Col) {
	time_heatm(Data, Caption, Legend_Caption, Low_Col, High_Col)
	ggsave(Filename)
}

##############################################################################

# Draw the heatmaps for Time to Dominance

# Check initialization results we had before:
time_heatm(D2[10*x==round(10*msne) & 10*y==round(10*msne)], Caption="Initial Play = MSNE", Legend_Caption="Time to Dominance", Low_Col="white", High_Col="green") # Play MSNE as first move
time_heatm(D2[x==0.5 & y==0.5], Caption="Initial Play = MS(0.5, 0.5)", Legend_Caption="Time to Dominance", Low_Col="white", High_Col="green") # Play Random as first move
time_heatm(D2[x==1 & y==1], Caption="Initial Play = MS(0.5, 0.5)", Legend_Caption="Time to Dominance", Low_Col="white", High_Col="green") # Play Random as first move
time_heatm(D2[x==0 & y==0], Caption="Initial Play = MS(0.5, 0.5)", Legend_Caption="Time to Dominance", Low_Col="white", High_Col="green") # Play Random as first move
# Trivial cases
time_heatm(D2[x==1 & y==0], Caption="Initial Play = MS(0.5, 0.5)", Legend_Caption="Time to Dominance", Low_Col="white", High_Col="green") # Play Random as first move

# Aggregation over x and y
time_heatm(D2[,.(finish_time=mean(finish_time)),by=.(msne, perc_red, pop_size, strategy)], Caption="Initial Play All MS", Legend_Caption="Time to Dominance", Low_Col="white", High_Col="green") # Play Random as first move

#save_time_heatm("HeatMap_InitialPlayXY_AllMS.png", D2[,.(z=mean(z)),by=.(msne, perc_red, pop_size, strategy)], Caption="Initial Play All MS", Legend_Caption="Reds Dominance\n  (% of Runs)", Low_Col="white", High_Col="red") # Play Random as first move
#save_time_heatm("HeatMap_InitialPlayXY_MSNE.png", D2[x==msne & y==msne], Caption="Initial Play MSNE", Legend_Caption="Reds Dominance\n  (% of Runs)", Low_Col="white", High_Col="red") # Play Random as first move
#save_time_heatm("HeatMap_InitialPlayXY_MS_50_50.png", D2[x==0.5 & y==0.5], Caption="Initial Play MS(0.5, 0.5)", Legend_Caption="Reds Dominance\n  (% of Runs)", Low_Col="white", High_Col="red") # Play Random as first move
#save_time_heatm("HeatMap_InitialPlayXY_MS_100_100.png", D2[x==1 & y==1], Caption="Initial Play MS(1, 1)", Legend_Caption="Reds Dominance\n  (% of Runs)", Low_Col="white", High_Col="red") # Play Random as first move


##############################################################################

 