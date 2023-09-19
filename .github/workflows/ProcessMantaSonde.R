
ProcessMantaSonde <- function ( infile, c_library = 'avon', outname, lon, lat)
	{

	library (parker)
	library (sun)
	library (BADC)
	#library (rovelli)
	library (O2)
	library (TTR)

	
	#lon <- LonLatSite ('CE1')['lon']
	#lat <- LonLatSite ('CE1')['lat']

	
	data (BADC_hourly_Atm)
	print (table (BADC_hourly_Atm$src_id))
	data (O2_sol)
	
	#load original data set from library
	#do.call ( data, list (infile))
	do.call ( data, list (infile))
	indata <- get (infile)


	#data (Minidot268810888); indata <- Minidot268810888; outname <- 'Minidot_02'
	#data (Minidot368810875); indata <- Minidot368810875; outname <- 'Minidot_03'
	#data (Minidot468810883); indata <- Minidot468810883; outname <- 'Minidot_04'
	#data (Minidot568810889); indata <- Minidot568810889; outname <- 'Minidot_05'
	#data (Minidot668810892); indata <- Minidot668810892; outname <- 'Minidot_06'
	#data (Minidot768810881); indata <- Minidot768810881; outname <- 'Minidot_07'
	
	
	indata$date <- as.character (indata$date)
	indata$date <- paste (indata$date, indata$tz, sep = ' ')
	
	indata$date <- try (as.POSIXct (indata$date))
	indata$date1 <- try (as.Date (indata$date))


	dates_range <- unique (indata$date1)
	dates_range <- c (head (dates_range) - 1, dates_range, tail (dates_range) + 1)

	#indata$DO <- indata$DO_mol / (10^6) * 32 * 1000
	#.......which is:
	indata$DO_mol <- (indata$DO / 32) * (10^3)
	


	x22 <- as.numeric (indata$date)
	x23 <- c (x22[-1], NA)
	print ( table (x23-x22))


	indata$TimeDiff <- c (NA, (x23 - x22)[-1])


	BADC_hourly_Atm_sub <- BADC_hourly_Atm [BADC_hourly_Atm$date1 %in% dates_range, ]
	plot ( BADC_hourly_Atm_sub$AbsPres_kPa)


	banana <- approxfun ( BADC_hourly_Atm_sub$date, BADC_hourly_Atm_sub$AbsPres_kPa)
	squashed <- banana ( indata$date )


	indata$AbsPres_kPa <- squashed


	input <- indata[, c ('Temp', 'AbsPres_kPa')]
	indata$CSat <- CalcCsat1 ( input )
	indata$DO_4 <- MoveAv ( indata$DO, 4 )
















































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
	indata$DO_4_deficit = with ( indata, CSat - DO_4 )
	indata$DO_4_diff = c (NA, diff (indata$DO_4))
	indata$DO_4_diff_lag = c ( diff (indata$DO_4), NA)
	indata$DO_4_diff_mean =  ( indata$DO_4_diff + indata$DO_4_diff_lag ) / 2
	indata$CSat_diff =  c (NA, diff (indata$CSat))
	indata$Temp_diff =  c (NA, diff (indata$Temp))
	#DO_4_diff = c (diff (DO_4_change$DO_4), NA)


	indata$DO_4_diff_diff = c(NA, diff(indata$DO_4_diff))
	indata$DO_4_diff_diff_lag = c ( diff (indata$DO_4), NA)


	indata$mv_avg_diff <- with ( indata, filter (DO_diff, rep(1/10,10), sides=2))

	#calc Ks
	#indata$Ks_2 <- with ( indata, (DO_diff_diff / (CSat_diff - DO_diff)))
	indata$Ks_2 <- with ( indata, (DO_4_diff_diff / (CSat_diff - DO_4_diff)))

	
	#nice
	#nice
	#nice
	DO_vectored <- embed(indata$DO, 10)
	mv_SD <- apply ( DO_vectored, 1, sd )
	indata$st.dev <- runSD(indata$DO,10) 



























	indata$Sunrise <- Sunrise ( indata$date, lon, lat, uniq = FALSE )
	indata$Sunset <- Sunset ( indata$date, lon, lat, uniq = FALSE )
	indata$SolarNoon <- with (indata, (Sunset - Sunrise)/2 + Sunrise )

	indata$daynight <- 'night'
	indata$daynight[indata$date > indata$Sunrise & indata$date < indata$Sunset] <- 'day'

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

	with ( indata, plot(date, DO_diff, ylim = c (-0.1,0.1)))
	with ( indata, plot(date, DO_diff_diff))
	with ( indata, plot(DO_diff, DO_diff_diff))

	indata$qualityFilter <- 0


	indata$qualityFilter <- 0
	qc1 <- indata$qualityFilter
	qc2 <- indata$qualityFilter
	qc3 <- indata$qualityFilter
	qc4 <- indata$qualityFilter


	qc1[abs(indata$DO_diff) < 0.05] <- 1
	qc2[abs(indata$DO_diff_diff) < 0.05] <- 1
	qc3[abs(indata$mv_avg_diff) < 0.01] <- 1
	qc4[abs(indata$st.dev) < 0.012] <- 1


	indata$qualityFilter <- qc1 * 1 + qc2 * 2 + qc3 * 4 + qc4 * 8
	print ( table (indata$qualityFilter))

	#.....so that qualityFilter01 > 0 would select all data points
	#avoids having to filter by > -1, which is conceptually clumsy
	indata$qualityFilter01 <- indata$qualityFilter + 1

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
	

















































	assign ( outname, indata )

	setwd (dirdmp)
	save ( list = outname, file =  paste ( outname, '.rda', sep = ''))
	}



#library (rovelli)

#lon_lat = LonLatSite ('GA2'); lon = lon_lat['lon'] ; lat = lon_lat['lat']
#ProcessMiniDotDataOrig ( infile = 'Minidot268810888', c_library = 'mdot', outname = 'Minidot_02', lon, lat )

#lon_lat = LonLatSite ('CW2'); lon = lon_lat['lon'] ; lat = lon_lat['lat']
#ProcessMiniDotDataOrig ( infile = 'Minidot368810875', c_library = 'mdot', outname = 'Minidot_03', lon, lat )

#lon_lat = LonLatSite ('AS2'); lon = lon_lat['lon'] ; lat = lon_lat['lat']
#ProcessMiniDotDataOrig ( infile = 'Minidot468810883', c_library = 'mdot', outname = 'Minidot_04', lon, lat )

#lon_lat = LonLatSite ('GN1'); lon = lon_lat['lon'] ; lat = lon_lat['lat']
#ProcessMiniDotDataOrig ( infile = 'Minidot568810889', c_library = 'mdot', outname = 'Minidot_05', lon, lat )

#lon_lat = LonLatSite ('CE1'); lon = lon_lat['lon'] ; lat = lon_lat['lat']
#ProcessMiniDotDataOrig ( infile = 'Minidot668810892', c_library = 'mdot', outname = 'Minidot_06', lon, lat )

#lon_lat = LonLatSite ('AS1'); lon = lon_lat['lon'] ; lat = lon_lat['lat']
#ProcessMiniDotDataOrig ( infile = 'Minidot768810881', c_library = 'mdot', outname = 'Minidot_07', lon, lat )

#lon_lat = LonLatSite ('GA2'); lon = lon_lat['lon'] ; lat = lon_lat['lat']
#ProcessMiniDotDataOrig ( infile = 'Minidot8', c_library = 'mdot', outname = 'Minidot_08', lon, lat )



#ListData('mdot.data' )
#library(O2)
#FitRegressionParametersUseLib (c_indata = 'Ebble_CE1_2014_08_21_md', c_library = 'mdot.data' )
#FitRegressionParametersUseLib (c_indata = 'Sem_AS1_2014_08_21_md', c_library = 'mdot.data' )
#FitRegressionParametersUseLib (c_indata = 'Sem_AS2_2014_08_20_md', c_library = 'mdot.data' )
#FitRegressionParametersUseLib (c_indata = 'Nadder_GN1_2014_08_20_md', c_library = 'mdot.data' )
#FitRegressionParametersUseLib (c_indata = 'Wylye_CW2_2014_08_20_md', c_library = 'mdot.data' )
#FitRegressionParametersUseLib (c_indata = 'Avon_GA2_2014_08_20_deep_md', c_library = 'mdot.data' )

