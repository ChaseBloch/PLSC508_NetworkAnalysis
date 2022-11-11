# Replication script for Figure 5 in Kinne, Brandon J. 2014. "Dependent Diplomacy:
# Signaling, Strategy, and Prestige in the Diplomatic Network." International Studies
# Quarterly 58(2): 247-259.
#
# This script estimates each of the five models in Figure 5 and saves the results
# as separate TeX files. Alternatively, the results can be displayed in a rope ladder
# plot. (If you need a script to build the figure itself, contact me.)
#
# The estimations are very computationally intensive and will take anywhere from
# a few hours to a few days to complete, depending on the system used. The current 
# script is set to use 3 clusters ("pp.nodes <- 3"); change as needed. Computation
# time can also be reduced by decreasing the number of subphases in phase 2 and/or by
# decreasing the number of iterations in phase 3 (though estimates will then be
# less precise).
#
# Note that because SAOMs estimate parameters using simulated method of moments, the
# estimates will likely differ *slightly* from the published results. However, any
# differences should be miniscule and negligible.
#
# These models were most recently estimated in R 3.0 using a Linux platform (Ubuntu
# 12.04), with version 1.1-212 of the RSiena package. Other platforms should yield
# comparable results.
#
# Email: brandon.kinne@utdallas.edu OR brandon.kinne@gmail.com

library(RSiena)
library(snow)
library(rlecuyer)
library(gdata)

pp.nodes <- 3

setwd("C:/Users/Chase/Google Drive/Spring 2020/PLSC 508 Networks/Data/Kinne_2014/ISQ-Replication")
load("Data/data.regions")
data.regions <- data; rm(data)

setwd("Output")

# Create a vector of region names and make some covariate indices
reg <- c("Europe", "LAmerica", "SSAfrica", "NAfricaME", "Asia")
dyad.effects <- c("igos", "allies", "trade", "swt")
monad.effects <- c("polity","power","gdppc")
dyad.covs <- c(paste(dyad.effects, ".net", sep=""),"dist.net","cont.net")
monad.covs <- paste(monad.effects, ".net", sep="")

# Now loop over regions to estimate models
for (i in 1:5) {

	if (reg[i]=="SSAfrica") {
		yrhi <- c(seq(1965,2000,by=5))
	}
	if (reg[i]=="Europe") {
		yrhi <- c(seq(1950,1990,by=5))
	}
	if (reg[i]!="Europe" && reg[i]!="SSAfrica") {
		yrhi <- c(seq(1950,2000,by=5))
	}

	# Set the time parameters
	yrlo <- yrhi[1:length(yrhi)-1]
	nlo <- (max(yrlo) - min(yrlo) + 5)/5
	nhi <- (max(yrhi) - min(yrhi) + 5)/5
	
	# Grab the data for region i
	data <- data.regions[[reg[i]]]

	# Number of actors in this network
	N <- length(data$change)

	# Assign dependent network data
	assign("diplomat.net", sienaNet(array(do.call("c", data$diplomat), dim=c(N,N,nhi))))

	# Assign exogenous networks as changing dyadic covariates
	for (x in 1:length(dyad.effects)) {
		assign(dyad.covs[x], varDyadCovar(array(do.call("c", data[[dyad.effects[x]]]), dim=c(N,N,nlo))))
	}; rm(x)

	# Assign monadic covariates
	for (x in 1:length(monad.effects)) {
		assign(monad.covs[x], varCovar(data[[monad.effects[x]]]))
	}; rm(x)

	# Assign constant dyadic covariates
	dist.net <- coDyadCovar(data$distance)
	cont.net <- coDyadCovar(data$contiguity)

	# Composition change
	change <- data$change

	# Create an RSiena object
	netdata <- sienaDataCreate(
		diplomat.net,
		igos.net,
		allies.net,
		trade.net,
		swt.net,
		polity.net,
		power.net,
		gdppc.net,
		dist.net,
		cont.net,
		change
	)

	# Specify model and prepare model effects
	if (reg[i]!="Europe") {
		model <- model.create(useStdInits = T, projname=paste(reg[i], ".log", sep=""), nsub=5, n3=3000, maxlike=F, modelType=1, cond=T)
	}
	if (reg[i]=="Europe") { # Use unconditional estimation for Europe
		model <- model.create(useStdInits = T, projname=paste(reg[i], ".log", sep=""), nsub=5, n3=3000, maxlike=F, modelType=1, cond=F)
	}
	eff <- getEffects(netdata)

	# Estimate the model with all network effects and exogenous covariates
	eff <- includeEffects(eff, density, recip, inPop, transTrip, include=T, type="eval")
	for (x in 1:length(dyad.covs)) { # Include exogenous dyadic covariates
		eff <- includeEffects(eff, X, interaction1=dyad.covs[x], include=T, type="eval")
	}; rm(x)
	for (x in 1:length(monad.covs)) { # Include monadic covariates and their interactions
		eff <- includeEffects(eff, egoXaltX, interaction1=monad.covs[x], include=T, type="eval")
		eff <- includeEffects(eff, altX, interaction1=monad.covs[x], include=T, type="eval")
		eff <- includeEffects(eff, egoX, interaction1=monad.covs[x], include=T, type="eval")
	}; rm(x)
	# Estimate and save results
	out <- siena07(model, data=netdata, effects=eff, batch=T, verbose=F, useCluster=T, nbrNodes=pp.nodes, initC=T)
	save(out,file=paste(reg[i], ".estimates", sep=""))
	print(xtable(out, type="latex", file=paste(reg[i], ".table.tex", sep="")))

	keep(pp.nodes, data.regions, reg, i, dyad.effects, dyad.covs,  monad.effects, monad.covs, sure=T)

}

print("El Fin!")

