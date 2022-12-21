#######################################################################################
# Calibration weighting call on ReGenesees for the Scottish Government
# Peter Broad
# Methodology Advisory Service
#######################################################################################


# This R program calls on the ReGenesees package to carry out calibration weighting
# This has been initially created for the Scottish Government for the Centralised Weighting Project
# It takes the following inputs and then uses these to carry out population based weighting

STEPD<- function(sampleData='C:\\Weighting\\dummy_data.csv',		# Sample data path file name
                 populationData='C:\\Weighting\\SCJShhtotals.csv',	# Population totals path file name
		 ids=~SERIAL,							# Unique identifier for the sample data
		 strata=NULL,							# Stratum used in the survey design
		 model=~pfa_cjaa:HHtype+pfa_cjaa:whrpagewb+laa2:urbrur_w-1,	# Model formula for the calibration
		 preweight = ~preweight,					# Preweight used in the calibration
		 calfun='raking',						# Calibration distance function used - either "raking", "linear" or "logit"
		 bounds=c(-Inf,Inf),						# Bounds that can be used in the calibration.
		 aggregate.stage=NULL,						# Used to aggregate and force the same weight for unique identifiers
		 sigma2=NULL							# Used to control the q weights within a cluster, for example households.
		 ){

library(ReGenesees)						# Call the package ReGenesees - needs to be installed already on the machine

rf.data <- read.csv(sampleData)					# Read in the sample data

pop <- pop.template(rf.data, 					# Create a population template from the sample data. Essentially this creates a
		    calmodel=model)				# file with the calibration variable names with missing spaces for the actual totals.
						
df.population <- read.csv(populationData)				# Read in the population totals file

pop2 <- t(pop)							# Transpose the population template file and adjust so that the calibration variable
colnames(pop2) <- "NA"						# names are easily read.
pop.names <- data.frame(rownames(pop2))
colnames(pop.names) <- "name"

merge1 <- merge(pop.names, df.population, 			# Merge on the population totals from the imported data file.
		by="name", all.x=TRUE,sort=FALSE)		# Note the calibration variable names will need to be exactly the same.
merge1[!complete.cases(merge1), "total"] <- 0

merge2 <- data.frame(merge1[,2])				# Further editing of the file to get the merged dataset in the correct format
rownames(merge2) <- merge1[,"name"]
merge3 <- t(merge2)

final_pop <- data.frame(merge3)
colnames(final_pop) <- colnames(merge3)

des<-e.svydesign(data=rf.data, ids=ids, 			# Now create an analytical object which stores the sample data as well as the unique
		 strata=strata, weights=preweight)		# identifier and the preweights

calr <- e.calibrate(design=des, df.population=final_pop, 	# This carries out the calibration using the analytical object created above, the
		    calmodel=model,calfun=calfun, 		# population totals also created above and the other options specified previously.
		    bounds=bounds, 
		    aggregate.stage=aggregate.stage, 
		    sigma2=sigma2)

calrdata <- calr$variables					# This then creates a separate data file with the output weights added
list(data=calrdata, poptemp=pop, poptot=final_pop)		# As well as the population template and the final population file.
}
