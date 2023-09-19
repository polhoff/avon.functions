

CompareAirTempWaterTemp <- function ( c_indata, c_library = 'avon' )
	{
	library (BADC)
	library (parker)
	data (BADC_hourly_Temp_842)
	
	banana <- with ( BADC_hourly_Temp_842, approxfun ( date, air_temperature ))
	
	#c_indata <- DO_Ebble


	do.call ( library, list ( c_library))
	do.call ( data, list ( c_indata))
	indata <- get (c_indata)
	print (head (indata, 3))

	AirTemperature <- banana ( indata$date )
	WaterTemperature <- indata$Temp
	new_data <- data.frame ( WaterTemperature, AirTemperature )

	NewW()
	par ( mar = c (7,7,4,4))
	with (new_data, plot ( AirTemperature, WaterTemperature ))

	return (head (new_data))

	#with (new_data, plot ( AirTemperature, WaterTemperature ))
	}



#CompareAirTempWaterTemp ( 'DO_Ebble' )










































































FitWaterTemperature <- function ( c_indata, c_library = 'avon' )
	{

	do.call ( library, list ( c_library))
	do.call ( data, list ( c_indata))
	indata <- get (c_indata)
	print (head (indata, 2))

	indata$jday <- JulianDay ( indata$date )
	
	#beginning of day
	indata$daysecs <- as.POSIXct ( as.Date (indata$date))
	indata$time_elapse <- with (indata, (date - daysecs ))
	
	x_fit <- with (indata, lm ( Temp ~ jday + time_elapse ))
	#return (x_fit)

	indata$TempEst <- indata$Temp


	missing_ndx <- is.na (indata$Temp)
	data_sub <- indata[missing_ndx,]
	TempEst <- indata$Temp
	
	TempEst <- predict(x_fit, data_sub[c('jday', 'time_elapse')])
	#TempEst[missing_ndx] <- with (data_sub, x_fit ( jday, time_elapse))
	
	indata$TempEst[missing_ndx] <- TempEst
	TempEst <- indata[c ('date','Temp','TempEst')]
	return ( TempEst )
	}

#x_fit <- FitWaterTemperature ( 'DO_Ebble' )
#head(x_fit)
































































WaterTemperatureStats <- function ( c_indata, c_library = 'avon' )
	{

	do.call ( library, list ( c_library))
	do.call ( data, list ( c_indata))
	indata <- get (c_indata)
	print (head (indata, 2))

	indata$jday <- JulianDay ( indata$date )
	
	
	TempMax <- aggregate ( indata$Temp, by = list ( indata$jday), FUN = max, na.rm = TRUE )
	TempMin <- aggregate ( indata$Temp, by = list ( indata$jday), FUN = min, na.rm = TRUE )
	TempMean <- aggregate ( indata$Temp, by = list ( indata$jday), FUN = mean, na.rm = TRUE )
	
	TempMax[TempMax == -Inf] <- NA
	TempMin[TempMin == Inf] <- NA
	TempMean[TempMean == NaN] <- NA

	
	WT_stats <- data.frame ( TempMax, TempMin, TempMean ) 
	names (WT_stats)[c(2,4,6)] <- c ( 'TempMax', 'TempMin', 'TempMean' ) 
	names (WT_stats)[1] <- 'JulianDay'
	WT_stats <- WT_stats[c (1,2,4,6)]
	
	
	return ( WT_stats )
	
	}

#x_fit1 <- WaterTemperatureStats ( 'DO_Ebble' )
#head(x_fit1)
#with (x_fit1, plot (JulianDay, TempMax, col = 'red', ylim = c ( 5,20) ))
#with (x_fit1, points (JulianDay, TempMean, col = 'black' ))
#with (x_fit1, points (JulianDay, TempMin,  col = 'green' ))
