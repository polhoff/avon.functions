
CalcAngle <- function(c_site)
	{
	
	library(rovelli)
	
	#c_site = 'GA2'
	
	depth_data <- get(do.call(data, list('DepthAveragesBySite', package = 'rovelli.data')))
	depth_data <- depth_data[depth_data$SiteCode == c_site,]
	
	stage_shared <- banana(depth_data$date1)
	depth_exceeds_level <- depth_data$depth.mean - stage_shared
	
	cross_section <- with(depth_data, discharge.mean/velocity.mean)
	
	
	A2 <- max(cross_section,na.rm = TRUE)
	A1 <- min(cross_section,na.rm = TRUE)
	
	h2 <- max(depth_data$depth.mean,na.rm = TRUE)
	h1 <- min(depth_data$depth.mean,na.rm = TRUE)
 	
 	x1 <- (A2 - (h2/h1)*A1) / (h2^2/h1 - h2)
 	
 	
	depth_diff = with(depth_data, depth.mean[1] - depth.mean[2])
	n_quot <- (cross_section[1] - cross_section[2])/depth_diff

	n_quot1 <- 

	
	bank_angle_tan <- 1/(1 - n_quot)
	bank_angle <- atan(bank_angle_tan)
	
	return(bank_angle)
	}
	

#CalcAngle('GA2')



























































CalcDepth <- function(c_site, bank_angle = pi/2/3)
	{
	library(avon)
	library(st)
	
	data(st)
	
	#c_site = 'GA2'

	c_avonfile <- paste (c_site, '_Manta_Aug2015', sep = '')
	
	Manta_data <- get(do.call(data, list(c_avonfile, package = 'avon.data')))
	Manta_stage <- Manta_data[c('date1', 'stage')]
	
	Manta_stage_daily <- AggregateToDaily(Manta_stage, 'stage', c_fun = 'median')
	#names(Manta_stage_daily) <- c('date1','level')
	
	
	
	depth_data <- get(do.call(data, list('DepthAveragesBySite', package = 'rovelli.data')))
	depth_data <- depth_data[depth_data$SiteCode == c_site,]

	
	banana <- with(Manta_stage_daily, approxfun(date1, stage))
	
	stage_shared <- banana(depth_data$date1)
	depth_exceeds_level <- depth_data$depth.mean - stage_shared
	depth_exceeds_level <- depth_exceeds_level[!is.na(depth_exceeds_level)]



	c_namesdepth <- paste('depth', Labels1()[1:length(depth_exceeds_level)], sep = '')
	c_nameswidth <- paste('width', Labels1()[1:length(depth_exceeds_level)], sep = '')
	c_namesArea <- paste('CrossArea', Labels1()[1:length(depth_exceeds_level)], sep = '')
	#c_namesSurfaceArea <- paste('SurfaceArea', Labels1()[1:length(depth_exceeds_level)], sep = '')
	
	
	
	for (i in 1:length(depth_exceeds_level))
		{
		n_depth_adjust <- depth_exceeds_level[i]
		
		n_depth <- Manta_stage_daily$stage + n_depth_adjust
		
		#assume 1m by 1m at surface
		UnitVolume <- Manta_stage_daily$stage + n_depth_adjust
		
		#base width of 1 metre
		stream_width_base <- 1
		width_add <- n_depth/tan(bank_angle) * 2
		
		stream_width_surface <- stream_width_base + (2 * width_add)
		cross_section_area <- (stream_width_surface + stream_width_base) / 2 * n_depth


		Manta_stage_daily$n_depth <- n_depth
		names(Manta_stage_daily)[names(Manta_stage_daily) == 'n_depth'] <- c_namesdepth[i]
		
		
		Manta_stage_daily$CrossArea <- cross_section_area
		names(Manta_stage_daily)[names(Manta_stage_daily) == 'CrossArea'] <- c_namesArea[i]
		
		Manta_stage_daily$Width <- stream_width_surface
		names(Manta_stage_daily)[names(Manta_stage_daily) == 'Width'] <- c_nameswidth[i]
		
		
		n_depth_adjust <- depth_exceeds_level[i]
		}


	new_name <- paste(c_site, 'Manta_stage_daily', sep = '')

	assign(new_name, Manta_stage_daily)
	save(list  =  new_name, file = paste(dirdmp, new_name, '.rda',sep = ''))
	
	return(Manta_stage_daily)
	}


#Manta_stage_daily <- CalcDepth('GA2')
#CalcDepth('GA2')
#CalcDepth('CE1')
#CalcDepth('AS2')
#CalcDepth('GN1')
#head(Manta_stage_daily)






















































































CalcDepthHOBO <- function(c_site, bank_angle = pi/2/3)
	{
	library(avon)
	library(st)
	
	data(st)
	
	#c_site = 'GA2'

	c_avonfile <- paste ('WaterDepth', c_site, sep = '')
	
	HOBO_data <- get(do.call(data, list(c_avonfile, package = 'avon.data')))
	HOBO_data$date1 <- as.Date(HOBO_data$date)
	
	HOBO_data_daily <- AggregateToDaily(HOBO_data, 'WaterDepth', c_fun = 'median')
	#names(Manta_stage_daily) <- c('date1','level')
	

	n_depth <- HOBO_data_daily$WaterDepth

	#assume 1m by 1m at surface
	UnitVolume <- HOBO_data_daily$WaterDepth
	
	#base width of 1 metre
	stream_width_base <- 1
	width_add <- n_depth/tan(bank_angle) * 2
	
	stream_width_surface <- stream_width_base + (2 * width_add)
	cross_section_area <- (stream_width_surface + stream_width_base) / 2 * n_depth

	HOBO_data_daily$CrossArea <- cross_section_area
	HOBO_data_daily$width <- stream_width_surface
	HOBO_data_daily$depth <- HOBO_data_daily$WaterDepth

	HOBO_data_daily <- HOBO_data_daily[c('date1', 'depth', 'width', 'CrossArea')]

	new_name <- paste(c_site, 'HOBO_data_daily', sep = '')

	assign(new_name, HOBO_data_daily)
	save(list  =  new_name, file = paste(dirdmp, new_name, '.rda',sep = ''))
	
	return(HOBO_data_daily)
	}


#HOBO_data_daily <- CalcDepthHOBO('GA2')
#CalcDepthHOBO('GN1')
#CalcDepthHOBO('CE1')
#CalcDepthHOBO('CW2')
#CalcDepthHOBO('AS1')
#CalcDepthHOBO('AS2')






















































































CalcDepthDiscrepancy <- function(c_site)
	{
	library(rovelli)
	#c_site = 'GA2'

	c_avonfile <- paste (c_site, '_Manta_Aug2015', sep = '')
	
	Manta_data <- get(do.call(data, list(c_avonfile, package = 'avon.data')))
	Manta_stage <- Manta_data[c('date1', 'stage')]
	
	Manta_stage_daily <- AggregateToDaily(Manta_stage, 'stage', c_fun = 'median')
	
	
	depth_data <- get(do.call(data, list('DepthAveragesBySite', package = 'rovelli.data')))
	depth_data <- depth_data[depth_data$SiteCode == c_site,]

	
	banana <- with(Manta_stage_daily, approxfun(date1, stage))
	
	stage_shared <- banana(depth_data$date1)
	depth_exceeds_level <- depth_data$depth.mean - stage_shared
	
	return(depth_exceeds_level)
	}

#DepthDifference <- CalcDepthDiscrepancy('GA2')
#CalcDepthDiscrepancy('GA2')
#CalcDepthDiscrepancy('CE1')
#CalcDepthDiscrepancy('GN1')
#CalcDepthDiscrepancy('AS2')
#CalcDepthDiscrepancy('CW2')























































































CalcDepthRovelli <- function(c_site, bank_angle = pi/2/3)
	{
	library(avon)
	
	#c_site = 'GA2'

	
	depth_data <- get(do.call(data, list('DepthAveragesBySite', package = 'rovelli.data')))
	depth_data <- depth_data[depth_data$SiteCode == c_site,]

	
	cross_section <- round(with(depth_data, discharge.mean/velocity.mean),3)
	depth_diff = with(depth_data, depth.mean[1] - depth.mean[2])

	x <- list(depth_data$date1, depth_data$depth, cross_section, depth_diff)
	names(x) <- c('date1','depth','CrossSectionArea','DifferenceInDepths')
	
	return(x)
	
	}

#DepthEstimate <- CalcDepthRovelli('GA2')
#DepthEstimate <- CalcDepthRovelli('CW2')
