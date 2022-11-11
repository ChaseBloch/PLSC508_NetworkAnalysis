# Replication script for Table 1 in Kinne, Brandon J. 2014. "Dependent Diplomacy:
# Signaling, Strategy, and Prestige in the Diplomatic Network." International Studies
# Quarterly 58(2): 247-259.
#
# This script estimates each of the four models in Table 1 and saves the results
# as separate TeX files.
#
# The estimations are very computationally intensive and will take anywhere from
# a few hours to a few days to complete, depending on the system used. The current 
# script is set to use 3 clusters ("pp.nodes <- 3"); change as needed. Computation
# time can also be reduced by decreasing the number of subphases in phase 2 and/or by
# decreasing the number of iterations in phase 3 (though estimates will then be
# less precise).
#
# Note that because SAOMs estimate parameters using simulated method of moments, the
# replicated estimates will likely differ *slightly* from the published results. However,
# any differences should be miniscule and negligible.
#
# These models were most recently estimated in R 3.0 using a Linux platform (Ubuntu
# 12.04), with version 1.1-212 of the RSiena package. Other platforms should yield
# comparable results.
#
# Email: brandon.kinne@utdallas.edu OR brandon.kinne@gmail.com
require(devtools)
#install_version('RSiena',version = "1.1-212",repos = "https://cran.r-project.org")

library(RSiena)
library(snow)
library(rlecuyer)

pp.nodes <- 3

setwd("C:/Users/Chase/Google Drive/Spring 2020/PLSC 508 Networks/Data/Kinne_2014/ISQ-Replication")
load("Data/data.world")

setwd("Output")

# Set the time parameters and make covariate indices
yrlo <- c(seq(1950,1995,by=5))
yrhi <- c(seq(1950,2000,by=5))
nlo <- (max(yrlo) - min(yrlo) + 5)/5
nhi <- (max(yrhi) - min(yrhi) + 5)/5
N <- length(data$change.diplomat)
dyad.effects <- c("igos", "allies", "trade", "Swt")
monad.effects <- c("polity","power","gdppc")
dyad.covs <- c(paste(dyad.effects, ".net", sep=""),"dist.net","cont.net")
monad.covs <- paste(monad.effects, ".net", sep="")

# Assign dependent network data and covariates
assign("diplomat.net", sienaNet(array(do.call("c", data$diplomat), dim=c(N,N,nhi))))

for (x in 1:length(dyad.effects)) { # Assign exogenous networks as changing dyadic covariates
	assign(paste(dyad.effects[x], ".net", sep=""), varDyadCovar(array(do.call("c", data[[dyad.effects[x]]]), dim=c(N,N,nlo))))
}; rm(x)

for (x in 1:length(monad.effects)) { # Also assign monadic covariates
	assign(paste(monad.effects[x], ".net", sep=""), varCovar(data[[monad.effects[x]]]))
}; rm(x)

# Constant dyadic covariates
dist.net <- coDyadCovar(data$distance)
cont.net <- coDyadCovar(data$contiguity)

# Network composition change
change <- data$change.diplomat

# Create an RSiena object
netdata <- sienaDataCreate(
	diplomat.net,
	igos.net,
	allies.net,
	trade.net,
	Swt.net,
	polity.net,
	power.net,
	gdppc.net,
	dist.net,
	cont.net,
	change
)

# Specify model and make effects object
model <- model.create(useStdInits = T, projname="table1.log", nsub=5, n3=5000, maxlike=F, modelType=1)
eff <- getEffects(netdata)

# Model 1 - No network effects (except density)
eff <- includeEffects(eff, density, include=T, type="eval")
eff <- includeEffects(eff, recip, inPop, transTrip, include=F, type="eval")
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
save(out,file="model1.estimates")
print(xtable(out, type="latex", file="model1.table.tex"))
rm(out)

# Model 2 - Full model
eff <- includeEffects(eff, density, recip, inPop, transTrip, include=T, type="eval")
out <- siena07(model, data=netdata, effects=eff, batch=T, verbose=F, useCluster=T, nbrNodes=pp.nodes, initC=T)
save(out,file="model2.estimates")
print(xtable(out, type="latex", file="model2.table.tex"))
rm(out)

# Model 3 - ART model, network effects only
for (x in 1:length(dyad.covs)) {
	eff <- includeEffects(eff, X, interaction1=dyad.covs[x], include=F, type="eval")
}; rm(x)
for (x in 1:length(monad.covs)) {
	eff <- includeEffects(eff, egoXaltX, interaction1=monad.covs[x], include=F, type="eval")
	eff <- includeEffects(eff, altX, interaction1=monad.covs[x], include=F, type="eval")
	eff <- includeEffects(eff, egoX, interaction1=monad.covs[x], include=F, type="eval")
}; rm(x)
out <- siena07(model, data=netdata, effects=eff, batch=T, verbose=F, useCluster=T, nbrNodes=pp.nodes, initC=T)
save(out,file="model3.estimates")
print(xtable(out, type="latex", file="model3.table.tex"))
rm(out)

# Model 4 - Ambassador-only model. This requires redoing the data object for a shorter time period.
nlo <- nlo - 4
nhi <- nhi - 4

# Grab the ambassador network
assign("ambassador.net", sienaNet(array(do.call("c", data$ambassador), dim=c(N,N,nhi))))

# Grab only observation moments 5-10 for covariates
for (x in 1:length(dyad.effects)) { # Dyadic effects
	assign(paste(dyad.effects[x], ".net", sep=""), varDyadCovar(array(do.call("c", data[[dyad.effects[x]]][5:10]), dim=c(N,N,nlo))))
}; rm(x)
for (x in 1:length(monad.effects)) { # Monadic effects
	assign(paste(monad.effects[x], ".net", sep=""), varCovar(data[[monad.effects[x]]][,5:10]))
}; rm(x)

# Composition change
change <- data$change.ambassador

# Create a new RSiena object
netdata <- sienaDataCreate(
	ambassador.net,
	igos.net,
	allies.net,
	trade.net,
	Swt.net,
	polity.net,
	power.net,
	gdppc.net,
	dist.net,
	cont.net,
	change
)

# Reset effects object and specify all effects
rm(eff); eff <- getEffects(netdata)
eff <- includeEffects(eff, density, recip, inPop, transTrip, include=T, type="eval")
for (x in 1:length(dyad.covs)) {
	eff <- includeEffects(eff, X, interaction1=dyad.covs[x], include=T, type="eval")
}; rm(x)
for (x in 1:length(monad.covs)) {
	eff <- includeEffects(eff, egoXaltX, interaction1=monad.covs[x], include=T, type="eval")
	eff <- includeEffects(eff, altX, interaction1=monad.covs[x], include=T, type="eval")
	eff <- includeEffects(eff, egoX, interaction1=monad.covs[x], include=T, type="eval")
}; rm(x)
out <- siena07(model, data=netdata, effects=eff, batch=T, verbose=F, useCluster=T, nbrNodes=pp.nodes, initC=T)
save(out,file="model4.estimates")
print(xtable(out, type="latex", file="model4.table.tex"))
rm(out)

print("El Fin!")

