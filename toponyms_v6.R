### Authors: Søren Wichmann and Lennart Chevalier 
# This script contains the following functions
# top(): displays maps of toponyms given one or more strings that are part
#   of the name and one or more countries
#   It reads information on different countries using read.files(),
#   extracts coordinates using get.coordinates, and produces a 
#   a map using simple_map()
# stg(): stands for Slavic toponyms in Germany; outputs candidates based on the length of a string
#   and a ratio of east-to-west occurrences
# Hello World
# see country abbreviations at the end of the script

# install packages if missing
packages = c("tidyverse", "maptools", "rgeos", "sf", "rnaturalearth", "rgdal")
install.packages(setdiff(packages, rownames(installed.packages())))

# load all packages
lapply(packages, require, character.only = TRUE)

## calling functions
# function definition: top()
# this mapps place names containing a certain
# string or strings in one or more countries
# run like this for several countries and strings:
# top(c("ice$", "itz$"), c("DE","PL"), "blue")
# or like this for one country and one string
# top("^Vlad", "RU", "red")
# note that ^ indicates beginning of string and $ end of string
# the default country is Germany and default color
# red, so if you just want Germany you can run it like this
# top("by$")

top <- function(strings, countries="DE", color=rainbow(length(countries)), df = TRUE, csv = FALSE, plot = FALSE) {
  gn <- read.files(countries)  # stands for geonames
  coors <- get.coordinates(gn, strings, df, csv)
	simple_map(coors[[1]], coors[[2]], coors[[3]], color, strings, plot)
}

# function definiton stg.maps()
# stands for create Slavic toponyms in Germany maps
# extracts 
stg.maps <- function(countries="DE", count = 10, lon = 10, lat = 50, len = 3, df = FALSE, csv = TRUE){
	dat <- stg(countries, count, lon, lat, len)
	for(i in 1:length(dat)) {
		top(dat[i], countries, color=rainbow(length(countries)), df, csv, plot = TRUE)
	}
}

## called functions
# function definition: read.files()
# this following function reads the geonames files for the relevant countries
# but only if they have not already been read earlier in the session
read.files <- function(countries) {
	L <- list()
	for (i in 1:length(countries)) {
		if ( tolower(countries[i]) %in% ls(envir = .GlobalEnv) == FALSE ) {
			filename <- paste(countries[i], ".txt", sep="")
			geonames_content <- read.table(file=filename, 
			   head=FALSE, sep="\t", quote="", na.strings="", 
			   comment.char="", encoding="utf8")
			L[[i]] <- assign(tolower(countries[i]), geonames_content, envir = .GlobalEnv)
		} else {
			L[[i]] <- get(tolower(countries[i]))
		}
	}
	if ( length(L) > 1 ) {
		gn <- L[[1]]
		for (j in 2:length(L)) {
			gn <- rbind(gn, L[[j]])
		}
	} else {
		gn <- L[[1]]
	}
	# add column names, which were gotten from the geonames readme file
	colnames(gn) <- c("geonameid", "name", "asciiname", "alternatenames", 
	   "rlatitude", "rlongitude", "rfeature class", "rfeature_code", 
	   "rcountry_code", "rcc2", "radmin1 code", "radmin2 code", "radmin3 code", 
	   "radmin4_code", "rpopulation", "relevation", "rdem", "rtimezone", 
	   "rmodification date")

	# select only the populated places
	gn <- gn[which(gn$"rfeature class"=="P"),]
	
	return(gn)
}

# function definition: get.coordinates()
get.coordinates <- function(gn, strings, df, csv) {
	w_strings <- unique(grep(paste(strings,collapse="|"), gn$name))
	lat_strings <- gn$rlatitude[w_strings]
	lon_strings <- gn$rlongitude[w_strings]
	country <- gn$rcountry_code[w_strings]	
	
	# saves data as df and/or csv 
	if(df == TRUE || csv == TRUE) {
		dat_name <- paste0("data_", paste(regmatches(strings, regexpr("[a-zA-Z]+", strings)), collapse = "_"), collapse="_")
		if(df == TRUE) {
			dat <- assign(dat_name, gn[w_strings,], envir = .GlobalEnv)
			cat(paste("\nDataframe",dat_name ,"saved in global environment.\n"))
		}
		if(csv == TRUE) {
			csv_dir = file.path(getwd(),"dataframes")
			csv_name = paste(file.path(csv_dir, dat_name), ".csv", sep ="")
			if (!dir.exists(csv_dir)) dir.create(csv_dir)
			write.csv(gn[w_strings,], csv_name)
			cat(paste("\nDataframe",dat_name ,"saved as csv in dataframes folder of the working directory.\n"))
		}
	}	
	
	return(list(lat_strings, lon_strings, country))
}

# function definition: simple_map()
# A function for creating maps
# x and y are vectors of latitudes and longitudes
# you need the packages maptools and rgeos,
# but the function will install them for you if you
# don't have them
# can be run as simple_map(lats, lons)
simple_map <- function(x, y, cc, color, strings, plot) {
	suppressMessages(library(maptools))
	nas <- unique(which(is.na(x)), which(is.na(y)))
	if ( length(nas) > 0 ) {
		x <- x[-nas]; y <- y[-nas]
	}
	if ( length(x)==0 | length(y)==0 ) {
		cat("\nThere are no coordinates to plot\n")
		return(invisible(NULL))
	}
	md <- cbind(as.numeric(x), as.numeric(y), cc) %>%
	  as.data.frame() %>% 
	  mutate_at(c("V1", "V2"), as.numeric)
	###### AUSGABE ORTSNAMEN, cat / print ... which vorher. cat("XXX", file = "XXXX")
      #### for (i in l:nrow(df)) { 
	#cat(df(i,1)), "n". sep = "\t", file = "XXX", append=TRUE)}
	## write.table(df...)
	
	# get max min long and lat and add a frame of 10% around the points
	lat_range <- range(md[,1])
	lng_range <- range(md[,2])
	lat_extend <- 0.1 * diff(lat_range)
	lng_extend <- 0.1 * diff(lng_range)
	lat_range <- c(lat_range - lat_extend, lat_range + lat_extend)
	lng_range <- c(lng_range - lng_extend, lng_range + lng_extend)
#	for (i in 1:4) {
#		if (lng_range[i] > 180) {lng_range[i] <- 180}
#		if (lng_range[i] < -180) {lng_range[i] <- -180}
#	}
	map <- ne_countries(scale = 50, returnclass = "sf")

	p <- ggplot() +
	  geom_sf(data = map) +
	  geom_point(data = md, mapping = aes(x = md[,2], y = md[,1], col = md[,3])) + 
	  coord_sf(xlim=c(min(lng_range), max(lng_range)),
	           ylim=c(min(lat_range), max(lat_range))) +
	  scale_color_manual(values = color) +
	  labs(x = "longitude", y = "latitude", color = "country", title = paste(strings, collapse = " "))
	
	# saves or prints plot
	if (plot == TRUE) {
    plot_name <- paste0("plot_", paste(regmatches(strings, regexpr("[a-zA-Z]+", strings)), collapse = "_"),".png", collapse="_")
    ggsave(plot_name, path = file.path(getwd(), "plots"))
    cat(paste("\nPlot",plot_name ,"saved in plots folder of the working directory.\n"))
	}
	else {
	  print(p)
	}
}

# function definition: stg(), stands for (candidates for) Slavic toponyms in Germany
stg <- function(countries="DE", count = 10, lon = 10, lat = 50, len = 3, rat = .5) {
	gn <- read.files(countries)
  
	# query all endings from the dataset
	endings <- paste(
		# creates a reg expr looking for endings of length "len" 
		regmatches(gn$name,regexpr(paste0(paste(replicate(len,"."), collapse = ""), "$"),gn$name)), "$", sep = "")
	# order them by frequency
	endings_o <- names(table(endings)[order(table(endings), decreasing = TRUE)])

	endings_ID_o <- list()
	lat_strings <- list()
	lon_strings <- list()
	# country <- list()
	loc_log <- list()
	ratio <- list()
	dat <- list()
  
	for (i in 1:count) {
		# stores indices of all ordered endings  
		endings_ID_o[[i]] <- unique(grep(endings_o[i], gn$name))
    
		lat_strings[[i]] <- gn$rlatitude[endings_ID_o[[i]]]
		lon_strings[[i]] <- gn$rlongitude[endings_ID_o[[i]]]
		# country[[i]] <- gn$rcountry_code[endings_ID_o[[i]]]
    
		# logical vectors storing if each place is within the given area
		loc_log[[i]] <- lon_strings[[i]] >= lon & lat_strings[[i]] >= lat
		# percentage of places which are in the area
		ratio[[i]] <- sum(loc_log[[i]])/length(loc_log[[i]])
    
		# select only endings with over 50%
		if (ratio[[i]]>rat) { 
			dat[[i]] <- endings_o[i]
		}
	}
  
	dat <- unlist(dat)
	dat_name <- paste0("data_top_", count)
	assign(dat_name, dat, envir = .GlobalEnv)
	cat(paste("\nDataframe",dat_name ,"saved in global environment.\n"))
  
	invisible(return(dat))
}

## abbreviations:
# AL Albania
# AT Austria
# BA Bosnia and Herzegovina
# BG Bulgaria
# BY Belarus
# CZ Czechia
# DE Germany
# EE Estonia
# FI Finland
# GR Greece
# HR Croatia
# HU Hungary
# KZ Kazakhstan
# LT Lithuania
# LV Latvia
# MD Moldavia
# ME Montenegro
# MK Macedonia
# NL Netherlands
# PL Poland
# RO Romania
# RU Russia
# RS Serbia
# SI Switzerland
# SK Slovakia
# UA Ukraine


# files were downloaded from https://download.geonames.org/export/dump/



