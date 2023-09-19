


ProcessMantaDataOrigSep2015 <- function ( c_filename, c_library = 'avon', outname, lon, lat)
	{

	library(avon)
	library (parker)
	library (sun)
	library (BADC)
	library (rovelli)
	library (O2)
	library(st)
	library (TTR)
	library(PenmanMonteith)

	data(st)
	
	
	#15 minute time stamp. Therefore only average over hour
	n_moveaverage = 4
	n_minutes = 15
	

	data  (BADC_hourly_Atm)
	print (table (BADC_hourly_Atm$src_id))
	data  (O2_sol)


	#c_site <- 'CE1'; c_outname <- paste(c_site,'_Manta_Aug2015', sep = ''); c_infile <- paste(c_site,'_Manta_orig_Aug2015', sep = '')
	#lon_lat = LonLatSite (c_site); lon = lon_lat['lon'] ; lat = lon_lat['lat']
	#c_filename <- c_infile
	#outname <- c_outname


	#lon = -1.861853; lat = 51.319424
	#c_filename <- 'GA2_Manta_orig_Aug2015'
	#outname <- 'GA2_Manta_Aug2015'

	#c_filename <- c_in
	#outname <- c_out

	#load original data set from library
	#do.call ( data, list (c_filename))
	do.call ( data, list (c_filename))
	indata00 <- get (c_filename)


	start_UNIX <- indata00$date[!is.na(indata00$date)][1]
	stop_UNIX <- tail(indata00$date[!is.na(indata00$date)])[1]
	
	#
	seq_UNIX <- data.frame(seq(start_UNIX, stop_UNIX, by = 60*n_minutes))
	names(seq_UNIX) <- 'date'


	#make base data.frame
	#make base data.frame
	#make base data.frame
	#make base data.frame
	#make base data.frame
	#make base data.frame
	#make base data.frame
	#make base data.frame
	#make base data.frame
	#make base data.frame
	#make base data.frame
	#make base data.frame
	#make base data.frame
	#make base data.frame
	#make base data.frame
	#make base data.frame
	#make base data.frame
	#make base data.frame
	#make base data.frame
	#make base data.frame
	#make base data.frame
	#make base data.frame
	#make base data.frame
	#make base data.frame
	
	indata <- merge(indata00, seq_UNIX)
	indata$date1 <- as.Date (indata$date)


	names(indata)
	str(indata)

	
	#this is redundant
	if(FALSE)
		{
		class_Temp <- with(indata, class(Temp))
		if(class_Temp == 'factor')
			{
			indata$Temp <- try(as.numeric(as.character(indata$Temp)))
			}



		class_DO <- with(indata, class(DO))
		if(class_DO == 'factor')
			{
			indata$DO <- try(as.numeric(as.character(indata$DO)))
			}



		class_DO_persat <- with(indata, class(DO_persat))
		if(class_DO_persat == 'factor')
			{
			indata$DO_persat <- try(as.numeric(as.character(indata$DO_persat)))
			}
		print('Hello')
		}




	summary ( indata$Temp)
	#make positive temperature, otherwise calculation of DOsat does not work
	indata$Temp[indata$Temp<=0] <- NA



	#END make base data.frame
	#END make base data.frame
	#END make base data.frame
	#END make base data.frame
	#END make base data.frame
	#END make base data.frame
	#END make base data.frame
	#END make base data.frame
	#END make base data.frame
	#END make base data.frame
	#END make base data.frame
	#END make base data.frame
	#END make base data.frame
	#END make base data.frame
	#END make base data.frame
	#END make base data.frame
	#END make base data.frame
	#END make base data.frame
	#END make base data.frame
	#END make base data.frame
	#END make base data.frame
	#END make base data.frame
	#END make base data.frame
	#END make base data.frame
	#END make base data.frame
	#END make base data.frame
	#END make base data.frame
	#END make base data.frame
	#END make base data.frame
	#END make base data.frame
	#END make base data.frame
	#END make base data.frame
	#END make base data.frame
	#END make base data.frame
	#END make base data.frame
	#END make base data.frame
	#END make base data.frame
	
	#data (Avon_GA2_mdot_long_orig); indata <- Avon_GA2_mdot_long_orig; outname <- 'Avon_GA2_mdot_long'
	#lon <- LonLatSite ('GA2')['lon']; lat <- LonLatSite ('GA2')['lat']


	#names(indata)
	#str(indata)


	if(FALSE)
		{
		if(class(indata$DO) == 'character')
			{
			indata$DO = as.numeric(indata$DO)
			}
		
		if(class(indata$DO_persat) == 'character')
			{
			indata$DO_persat = as.numeric(indata$DO_persat)
			}
		
		if(class(indata$Temp) == 'character')
			{
			indata$Temp = as.numeric(indata$Temp)
			indata$Temp[indata$Temp < 0] = NA
			indata$Temp[indata$Temp > 39] = NA
			}
		
		if(class(indata$Q) == 'character')
			{
			indata$Q = as.numeric(indata$Q)
			}
		
		if(class(indata$UnixTimestamp) == 'character')
			{
			indata$UnixTimestamp = as.numeric(indata$UnixTimestamp)
			indata$date = as.numeric(indata$UnixTimestamp)
			}
		}


	#indata$date[x_ndx] <- as.POSIXct(as.character(indata$date_UTC[x_ndx]))
	#indata$date <- as.POSIXct (as.character(indata$date))
	
	#get rid of records without date because easier than all this
	#infill NA dates
	#date_na_ndx <- is.na(indata$date)
	#date_na_ndx_which <- which(is.na(indata$date))
	#date_na_ndx_NOT <- !date_na_ndx
		
	#how_many_days <- dim(indata)[1]
	#peach <- approxfun ((1:how_many_days)[date_na_ndx_NOT], indata$date[date_na_ndx_NOT])
	
	#indata$date[date_na_ndx]
	#peach (how_many_days[date_na_ndx])
	


	dates_range <- unique (indata$date1)
	dates_range <- c (head (dates_range) - 1, dates_range, tail (dates_range) + 1)

	#indata$DO <- indata$DO_mol / (10^6) * 32 * 1000
	#.......which is:
	indata$DO_mol <- (indata$DO / 32) * (10^3)


	

	TimeDiff <- embed(as.numeric (indata$date),2)
	TimeDiff <- TimeDiff[,1] - TimeDiff[,2]

	indata$TimeDiff <- c (NA,TimeDiff)


	#merge BADC data
	#merge BADC data
	#merge BADC data
	#merge BADC data
	#merge BADC data
	#merge BADC data
	#merge BADC data
	#merge BADC data
	#merge BADC data
	#merge BADC data
	#merge BADC data
	#merge BADC data
	#merge BADC data
	#merge BADC data
	#merge BADC data
	#merge BADC data
	#merge BADC data
	#merge BADC data
	#merge BADC data

	BADC_hourly_Atm_sub <- BADC_hourly_Atm [BADC_hourly_Atm$date1 %in% dates_range, ]
	with (BADC_hourly_Atm_sub, plot ( date, AbsPres_kPa))
	summary ( BADC_hourly_Atm_sub$AbsPres_kPa)


	banana <- approxfun ( BADC_hourly_Atm_sub$date, BADC_hourly_Atm_sub$AbsPres_kPa)
	squashed <- banana ( indata$date )
	
	apple <- approxfun ( BADC_hourly_Atm_sub$date, BADC_hourly_Atm_sub$air_temperature)
	puree <- apple ( indata$date )



	indata$AbsPres_kPa <- squashed
	indata$air_temperature <- puree

	summary ( indata$AbsPres_kPa)



	#END merge BADC data
	#END merge BADC data
	#END merge BADC data
	#END merge BADC data
	#END merge BADC data
	#END merge BADC data
	#END merge BADC data
	#END merge BADC data
	#END merge BADC data
	#END merge BADC data
	#END merge BADC data
	#END merge BADC data
	#END merge BADC data
	#END merge BADC data
	#END merge BADC data
	#END merge BADC data
	#END merge BADC data
	#END merge BADC data
	#END merge BADC data
	#END merge BADC data
	#END merge BADC data

	input <- indata[, c ('date', 'Temp', 'AbsPres_kPa')]
	input_test <- input[1:100,]
	
	
	test1 <- try(CalcCsat ( input_test ))
	test2 <- try(CalcCsat1 ( input_test[,2:3] ))
	test3 <- try(CalcCsat03 ( input_test ))
	
	
	
	indata$CSat <- CalcCsat ( input )
	indata$DO15 <- MoveAv ( indata$DO, n_moveaverage )
	indata$Temp15 <- MoveAv ( indata$Temp, n_moveaverage )
	indata$AbsPres_kPa15 <- MoveAv ( indata$AbsPres_kPa, n_moveaverage )


	input <- indata[, c ('date', 'Temp15', 'AbsPres_kPa15')]
	#change names because required by function
	names(input) <- c ('date', 'Temp', 'AbsPres_kPa')
	indata$CSat15 <- CalcCsat ( input )

	rm(input)




















































	#differences for regression analysis
	indata$DO_deficit = with ( indata, CSat - DO )
	indata$DO_diff = c (NA, diff (indata$DO))
	indata$DO_diff_lag = c ( diff (indata$DO), NA)
	indata$DO_diff_mean =  ( indata$DO_diff + indata$DO_diff_lag ) / 2
	indata$CSat_diff =  c (NA, diff (indata$CSat))
	indata$Temp_diff =  c (NA, diff (indata$Temp))
	#DO_diff = c (diff (DO_change$DO), NA)


	indata$DO_diff_diff = c(NA, diff(indata$DO_diff))
	indata$DO_diff_diff_lag = c ( diff (indata$DO), NA)




	#differences for regression analysis
	indata$DO15_deficit = with ( indata, CSat15 - DO15 )
	indata$DO15_diff = c (NA, diff (indata$DO15))
	indata$DO15_diff_lag = c ( diff (indata$DO15), NA)
	indata$DO15_diff_mean =  ( indata$DO15_diff + indata$DO15_diff_lag ) / 2
	indata$CSat15_diff =  c (NA, diff (indata$CSat15))
	#DO15_diff = c (diff (DO15_change$DO15), NA)


	indata$DO15_diff_diff = c(NA, diff(indata$DO15_diff))
	indata$DO15_diff_diff_lag = c ( diff (indata$DO15), NA)

	indata$mv_avg_diff <- with ( indata, filter (DO_diff, rep(1/4,4), sides=2))


	#calc Ks
	#indata$Ks_2 <- with ( indata, (DO_diff_diff / (CSat_diff - DO_diff)))
	indata$Ks_2 <- with ( indata, (DO15_diff_diff / (CSat15_diff - DO15_diff)))

	
	#nice
	#nice
	#nice
	DO_vectored <- embed(indata$DO, 5)
	mv_SD <- c(NA,NA,apply ( DO_vectored, 1, sd ),NA,NA)
	indata$st.dev <- mv_SD
	
	#indata$st.dev <- runSD(indata$DO,5) 
	#indata$mv_avg_diff




















































	#sunrise and sunset
	#sunrise and sunset
	#sunrise and sunset
	#sunrise and sunset
	#sunrise and sunset
	#sunrise and sunset
	#sunrise and sunset
	#sunrise and sunset
	#sunrise and sunset
	#sunrise and sunset
	#sunrise and sunset
	#sunrise and sunset
	#sunrise and sunset
	#sunrise and sunset
	#sunrise and sunset
	#sunrise and sunset
	#sunrise and sunset
	#sunrise and sunset
	#sunrise and sunset
	#sunrise and sunset
	#sunrise and sunset
	#sunrise and sunset
	#sunrise and sunset
	#sunrise and sunset
	#sunrise and sunset
	#sunrise and sunset
	#sunrise and sunset
	#sunrise and sunset
	#sunrise and sunset
	#sunrise and sunset


	indata$Sunrise <- Sunrise ( indata$date, lon, lat, uniq = FALSE )
	indata$Sunset <- Sunset ( indata$date, lon, lat, uniq = FALSE )
	indata$SolarNoon <- with (indata, (Sunset - Sunrise)/2 + Sunrise )

	indata$Midnight <- with (indata, (SolarNoon - 60*60*12))
	indata$TimeAfterMidnight <- with (indata, difftime(date,Midnight, units = 'hours'))
	indata$TimeAfterMidnight <- round(as.numeric(indata$TimeAfterMidnight) %% 24,1)


	indata$TimeAfterSunrise <- with (indata, difftime(date,Sunrise, units = 'hours'))
	indata$TimeAfterSunrise <- round(as.numeric(indata$TimeAfterSunrise) %% 24,1)

	indata$daynight <- 'night'
	indata$daynight[indata$date > indata$Sunrise & indata$date < indata$Sunset] <- 'day'



	#END sunrise and sunset
	#END sunrise and sunset
	#END sunrise and sunset
	#END sunrise and sunset
	#END sunrise and sunset
	#END sunrise and sunset
	#END sunrise and sunset
	#END sunrise and sunset
	#END sunrise and sunset
	#END sunrise and sunset
	#END sunrise and sunset
	#END sunrise and sunset
	#END sunrise and sunset
	#END sunrise and sunset
	#END sunrise and sunset
	#END sunrise and sunset
	#END sunrise and sunset
	#END sunrise and sunset
	#END sunrise and sunset
	#END sunrise and sunset
	#END sunrise and sunset
	#END sunrise and sunset
	#END sunrise and sunset
	#END sunrise and sunset
	#END sunrise and sunset
	#END sunrise and sunset
	#END sunrise and sunset
	#END sunrise and sunset
	#END sunrise and sunset
	#END sunrise and sunset
	#END sunrise and sunset
	#END sunrise and sunset
	#END sunrise and sunset
	#END sunrise and sunset
	#END sunrise and sunset
	#END sunrise and sunset
	#END sunrise and sunset
	#END sunrise and sunset
	#END sunrise and sunset
	#END sunrise and sunset


	dates <- unique ( indata$date1 )
	dates <- c ( dates[1] - 1, dates, tail (dates,1) + 1 )
	dates_days <- dates


	dates <- as.POSIXct (paste (as.character (dates), '12:00:00'), tz = 'UTC')
	c_labels <- Labels1()[1:length(dates)]


	dates_days <- as.POSIXct (paste (as.character (dates_days), '02:00:00'), tz = 'UTC')
	#c_labels1 <- Labels1()[1:length(dates_days)]



	indata$daynight1 <- indata$daynight
	for ( i in 1:length(dates))
		{
		#nights
		x_ndx_night <- indata$date > dates[i] & indata$daynight == 'night'
		indata$daynight1[x_ndx_night] <- paste ('night', c_labels[i], sep = '')
		
		#days
		x_ndx_day <- indata$date > dates_days[i] & indata$daynight == 'day'
		indata$daynight1[x_ndx_day] <- paste ('day', c_labels[i], sep = '')
		}











































































	#other days
	#other days
	#other days
	#other days
	#other days
	#other days
	#other days
	#other days
	#other days
	#other days
	#other days
	#other days
	#other days
	#other days
	#other days
	#other days
	#other days
	#other days
	#other days
	#other days
	#other days
	#other days
	#other days
	#other days
	#other days
	#other days
	#other days
	#other days
	#other days
	#other days
	#other days
	#other days
	#other days
	#other days
	#other days
	#other days
	#other days
	#other days
	#other days
	#other days
	#other days
	#other days
	#other days
	#other days
	#other days
	#other days
	#other days
	#other days
	#other days
	#other days


	indata$YearMonth <- format(indata$date, '%Y %m')
	indata$Year <- as.factor(as.integer(format(indata$date, '%Y')))
	indata$Month01 <- as.factor(mnths[as.numeric(as.character(format(indata$date, '%m')))])
	indata$Month02 <- format(indata$date, '%m')
	indata$Julian <- as.integer(JulianDay(indata$date))
	indata$WeekNum <- as.integer(floor(indata$Julian / 7) + 1)


	#END other days
	#END other days
	#END other days
	#END other days
	#END other days
	#END other days
	#END other days
	#END other days
	#END other days
	#END other days
	#END other days
	#END other days
	#END other days
	#END other days
	#END other days
	#END other days
	#END other days
	#END other days
	#END other days
	#END other days
	#END other days
	#END other days
	#END other days
	#END other days
	#END other days
	#END other days
	#END other days
	#END other days
	#END other days
	#END other days
	#END other days
	#END other days
	























































	###################################################
	###################################################
	###################################################
	###################################################
	###################################################
	###################################################
	###################################################
	###################################################
	#quality filters
	#quality filters
	#quality filters
	#quality filters
	#quality filters
	#quality filters
	#quality filters
	#quality filters
	#quality filters

	#with ( indata, plot(date, DO_diff, ylim = c (-0.1,0.1)))
	#with ( indata, plot(date, DO_diff_diff))
	#with ( indata, plot(DO_diff, DO_diff_diff))

	indata$qualityFilter <- 0


	indata$qualityFilter <- 0
	qc1 <- indata$qualityFilter
	qc2 <- indata$qualityFilter
	qc3 <- indata$qualityFilter
	qc4 <- indata$qualityFilter


	qc1[abs(indata$DO_diff) < 0.05 * 15] <- 1
	qc2[abs(indata$DO_diff_diff) < 0.05 * 15] <- 1
	qc3[abs(indata$mv_avg_diff) < 0.01 * 15] <- 1
	qc4[abs(indata$st.dev) < 0.012 * 15] <- 1


	indata$qualityFilter <- qc1 * 1 + qc2 * 2 + qc3 * 4 + qc4 * 8
	print ( table (indata$qualityFilter))

	#.....so that qualityFilter01 > 0 would select all data points
	#avoids having to filter by > -1, which is conceptually clumsy
	indata$qualityFilter01 <- indata$qualityFilter + 1
	print ( table (indata$qualityFilter01))

	#quality filters
	########################################################
	########################################################
	########################################################
	########################################################
	########################################################
	########################################################
	########################################################
	########################################################
	########################################################
	########################################################
	########################################################
	########################################################
	########################################################
	########################################################
	########################################################
	########################################################
	########################################################
	########################################################
	########################################################
	########################################################
	########################################################
	





































	





	n_coord <- c ( 'x' = lon, 'y' = lat, 'z' = 65 )
	
	SolarRadiation <- CalcETSWSubDaily (indata$date, lon, lat)
	SolarInsolation <- CalcInsol (indata$date, lon, lat)

	summary (index(SolarRadiation) == indata$date)

	indata1 <- data.frame (indata, SolarInsolation)


	assign ( outname, indata1 )
	save ( list = outname, file =  paste ( dirdmp, outname, '.rda', sep = ''))
	}



#library(avon)


#c_site <- 'GA2'; c_outname <- paste(c_site,'_Manta_Aug2015', sep = ''); c_infile <- paste(c_site,'_Manta_orig_Aug2015', sep = '')
#lon_lat = LonLatSite (c_site); lon = lon_lat['lon'] ; lat = lon_lat['lat']
#ProcessMantaDataOrigSep2015 ( c_filename = c_infile, c_library = 'avon.data', outname = c_outname, lon, lat )


#c_site <- 'AS2'; c_outname <- paste(c_site,'_Manta_Aug2015', sep = ''); c_infile <- paste(c_site,'_Manta_orig_Aug2015', sep = '')
#lon_lat = LonLatSite (c_site); lon = lon_lat['lon'] ; lat = lon_lat['lat']
#ProcessMantaDataOrigSep2015 ( c_filename = c_infile, c_library = 'avon.data', outname = c_outname, lon, lat )


#c_site <- 'CE1'; c_outname <- paste(c_site,'_Manta_Aug2015', sep = ''); c_infile <- paste(c_site,'_Manta_orig_Aug2015', sep = '')
#lon_lat = LonLatSite (c_site); lon = lon_lat['lon'] ; lat = lon_lat['lat']
#ProcessMantaDataOrigSep2015 ( c_filename = c_infile, c_library = 'avon.data', outname = c_outname, lon, lat )


#c_site <- 'GN1'; c_outname <- paste(c_site,'_Manta_Aug2015', sep = ''); c_infile <- paste(c_site,'_Manta_orig_Aug2015', sep = '')
#lon_lat = LonLatSite (c_site); lon = lon_lat['lon'] ; lat = lon_lat['lat']
#ProcessMantaDataOrigSep2015 ( c_filename = c_infile, c_library = 'avon.data', outname = c_outname, lon, lat )

