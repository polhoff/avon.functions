
AggregateStageToDayNight <- function(indata, l_keepdate = FALSE)
	{
    library(plyr)
    library(zoo)

    indata <- as.data.frame(indata)

	data_sub <- indata[c("daynight1", "date1", "WaterDepth", "stage")]


	outdata <- ddply(data_sub, .(daynight1), summarize, max.depth = max(get("WaterDepth")), mean.depth = mean(get("WaterDepth")), median.depth = median(get("WaterDepth")), min.depth = min(get("WaterDepth")), max.stage = max(get("stage")), mean.stage = mean(get("stage")), median.stage = median(get("stage")), min.stage = min(get("stage")))


	if(l_keepdate)
		{
		data_sub01 <- data_sub[c('daynight1','date1')]
		data_sub01 <- unique(data_sub01)
		
		outdata <- merge(data_sub01, outdata)
		}

	return(outdata)
	}


#x1 <- AggregateStageToDayNight(x, l_keepdate = TRUE)
#x1 <- AggregateStageToDayNight(x, l_keepdate = FALSE)

























































AggregateHOBOToDayNight <- function(indata, l_keepdate = FALSE)
	{
    library(plyr)
    library(zoo)

    indata <- as.data.frame(indata)

	data_sub <- indata[c("daynight1", "date1", "WaterDepth")]


	outdata <- ddply(data_sub, .(daynight1), summarize, max.depth = max(get("WaterDepth")), mean.depth = mean(get("WaterDepth")), median.depth = median(get("WaterDepth")), min.depth = min(get("WaterDepth")))


	if(l_keepdate)
		{
		data_sub01 <- data_sub[c('daynight1','date1')]
		data_sub01 <- unique(data_sub01)
		
		outdata <- merge(data_sub01, outdata)
		}

	return(outdata)
	}


#x1 <- AggregateHOBOToDayNight(merged_depth, l_keepdate = TRUE)
#x1 <- AggregateHOBOToDayNight(x, l_keepdate = FALSE)















































































































MergeHOBOminiDOT <- function(indata, c_site)
	{

	c_HOBO <- paste ('WaterDepth', c_site, sep = '')
	
	in_HOBO <- do.call(data, list(c_HOBO))
	in_HOBO <- get(in_HOBO)
	
	#it must be this way round so that result can easily merge with Manta data
	banana <- with(in_HOBO, approxfun(date, WaterDepth))
	
	indata$WaterDepth <- banana(indata$date)
	
	indata <- indata[c('date','date1','WaterDepth','daynight', 'daynight1')]
	
	return(indata)
	}

#merged_depth <- MergeHOBOminiDOT(indata, c_site)




























































MergeDepthStage <- function(c_site)
	{

	c_HOBO <- paste ('WaterDepth', c_site, sep = '')
	c_Manta <- paste (c_site, '_Manta_Aug2015', sep = '')
	
	in_HOBO <- do.call(data, list(c_HOBO))
	in_HOBO <- get(in_HOBO)
	
	in_Manta <- do.call(data, list(c_Manta))
	in_Manta <- get(in_Manta)
	
	#it must be this way round so that result can easily merge with Manta data
	banana <- with(in_HOBO, approxfun(date, WaterDepth))
	
	in_Manta$WaterDepth <- banana(in_Manta$date)
	
	in_Manta <- in_Manta[c('date','date1','stage','WaterDepth','daynight')]
	
	return(in_Manta)
	}

#merged_depth <- MergeDepthStage('AS2')














































MergeStageWithMiniDOT <- function(indata, merged_depth)
	{
	banana <- with(merged_depth, approxfun(date, WaterDepth))
	apple <- with(merged_depth, approxfun(date, stage))
	
	indata$WaterDepth <- banana(indata$date)
	indata$stage <- apple(indata$date)
	
	return(indata)
	}


#x <- MergeStageWithMiniDOT(indata, merged_depth)
#x <- MergeStageWithMiniDOT('AS2')

