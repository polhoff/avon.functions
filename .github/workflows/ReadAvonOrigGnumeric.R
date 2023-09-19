

ReadAvonOrigGnumeric <- function ( infile, outname, sheetname, indir = paste ( dirtop, "avon/orig/", sep = "" ))
	{

	library (gnumeric)
	library (st)
	data(st)
	
	setwd (indir)
	
	#field.format = 'preserve' prevents writing '2013-12-31 00:00:00' as '2013-12-31', which makes as.POSIXct work correctly.
	a1 <- read.gnumeric.sheet (file = infile, head = TRUE, sheet =  sheetname, stringsAsFactors = FALSE, field.format = 'preserve')
	
	a1$date[a1$date == ''] = NA

	x1 <- names (a1)
	x1[x1 == 'Csat_mol'] <- try ('CSat_mol')
	names (a1) <- x1

	assign (outname, a1, env = .GlobalEnv)
	#names(get(outname)) <- x1

	setwd (dirdmp)
	save ( list = outname, file = paste ( outname, '.rda', sep = ''))
	
	x = get(outname)
	}



#x1 <- ReadAvonOrigGnumeric ( infile = 'AS2_130609_to_150208.gnumeric', sheetname = 'AS2', outname = 'Sem_AS2_2013_06_09_Manta_orig' )
#x1$date[x1$date == ''] = NA
#length_date <- nchar(x1$date)
#table (length_date)
#x1$date[length_date == 10]
#which(length_date == 10)


#x2 <- ReadAvonOrigGnumeric ( infile = 'GA2untilFeb2015.gnumeric', sheetname = 'GA2', outname = 'Avon_GA2_2013_06_17_Manta_orig' )
#x2$date[x2$date == '']
#x2$date[x2$date == ''] = NA


#x1[x1$date == '', ]
#x1_dates <- try(as.POSIXct(x1$date[1:2]))
#x1_dates <- try(as.POSIXct(x1$date))
#x2_dates <- try(as.POSIXct(x2$date))

























































ReadAvonOrigcsv <- function ( infile, outname, indir = paste ( dirtop, "avon/orig/", sep = "" ))
	{

	library (st)
	data(st)
	
	setwd (indir)
	
	a1 <- read.csv (file = infile, header = TRUE, stringsAsFactors = FALSE)
	x1 <- names (a1)
	x1[x1 == 'Csat_mol'] <- try ('CSat_mol')
	names (a1) <- x1

	assign (outname, a1, env = .GlobalEnv)
	#names(get(outname)) <- x1

	setwd (dirdmp)
	save ( list = outname, file = paste ( outname, '.rda', sep = ''))
	
	x = get(outname)
	}


#x1 <- ReadAvonOrigcsv ( infile = 'AS2_130609_to_150208.csv', outname = 'Sem_AS2_2013_06_09_Manta_orig' )





























































ReadAvonOrigExcel <- function ( infile, outname, sheetname, indir = paste ( dirtop, "avon/orig/", sep = "" ))
	{

	library (XLConnect)
	library (st)
	data(st)
	
	setwd (indir)
	demoExcelFile <- system.file("demoFiles/mtcars.xlsx", package = "XLConnect")
 
	# Load workbook
	wb <- loadWorkbook(infile)
	 
	# Read worksheet 'mtcars' (providing no specific area bounds;
	# with default header = TRUE)
	a1 <- readWorksheet(wb, sheet = sheetname)
	
	assign (outname, a1)
	#names(get(outname)) <- x1

	setwd (dirdmp)
	save ( list = outname, file = paste ( outname, '.rda', sep = ''))
	
	x = get(outname)
	}



#x1 <- ReadAvonOrigExcel ( infile = 'GA2 until Feb 2015.xls', sheetname = 'GA2', outname = 'Avon_GA2_2013_04_28_orig' )
#x1 <- ReadAvonOrigExcel ( infile = 'GA2 until Feb 2015.xls', sheetname = 'GA2', outname = 'Avon_GA2_2013_04_28_orig' )
#x1 <- ReadAvonOrigExcel ( infile = 'GA2untilFeb2015.xls', sheetname = 'GA2', outname = 'Avon_GA2_2013_04_28_orig' )

