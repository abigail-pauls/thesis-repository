getwd()
setwd("C:/Users/pauls/OneDrive/Documents/Fall 2025/THESIS/THESIS")
library(sp)
library(raster)
library(terra)
library(sf)
library(fuzzyjoin)
require(RSQLite)
require(DBI)
library(readxl)
library(mgsub)
library(purrr)
library(tidyverse)
library(data.table)

wosis <- read.csv("~/Fall 2025/THESIS/THESIS/WoSIS_Data/wosis_og.csv")
grid_SMU <- read.csv("~/Fall 2025/THESIS/THESIS/grid_SMU.csv")
smu_latlon <- read.csv("~/Fall 2025/THESIS/THESIS/smu_latlon.csv")
HWSD2_LAYERS <- read_excel("~/Fall 2025/THESIS/THESIS/HWSD2_LAYERS.xlsx")
lp.all<-read.csv("~/Fall 2025/THESIS/THESIS/wosis.lp.date.prof.csv")

#BATCH UPLOAD WOSIS DATA
#create a list containing the names of each csv file
CSVfiles <- list.files(pattern="\\.csv$", full.names=TRUE)
print(CSVfiles)
dataframe_names <- mgsub(CSVfiles, c("./",".csv"), c("",""))
print(dataframe_names)

dataframe_names <- mgsub(CSVfiles, c("./",".csv"), c("",""))

#import data from each csv into a list
dat <- list()
for(x in unique(CSVfiles)){
  dat[[x]] <- read.csv(x)
}

#split the list into individual dataframes
dat <- dat %>% set_names(dataframe_names)
invisible(list2env(dat ,.GlobalEnv))
ls()

#CLEAN UP DATAFRAMES
#rename 'value' and 'value_avg' column in each csv so the data can be merged and retain all information labeled correctly
bdfi33<-rename(bdfi33,bdfi33="value")
bdfi33<-rename(bdfi33,bdfi33_avg="value_avg")
bdfiad<-rename(bdfiad,bdfiad="value")
bdfiad<-rename(bdfiad,bdfiad_avg="value_avg")
bdfifm<-rename(bdfifm,bdfifm="value")
bdfifm<-rename(bdfifm,bdfifm_avg="value_avg")
bdwsod<-rename(bdwsod,bdwsod="value")
bdwsod<-rename(bdwsod,bdwsod_avg="value_avg")
cecph7<-rename(cecph7,cecph7="value")
cecph7<-rename(cecph7,cecph7_avg="value_avg")
cecph8<-rename(cecph8,cecph8="value")
cecph8<-rename(cecph8,cecph8_avg="value_avg")
cfgr<-rename(cfgr,cfgr="value")
cfgr<-rename(cfgr,cfgr_avg="value_avg")
cfvo<-rename(cfvo,cfvo="value")
cfvo<-rename(cfvo,cfvo_avg="value_avg")
clay<-rename(clay,clay="value")
clay<-rename(clay,clay_avg="value_avg")
ecec<-rename(ecec,ecec="value")
ecec<-rename(ecec,ecec_avg="value_avg")
elco20<-rename(elco20,elco20="value")
elco20<-rename(elco20,elco20_avg="value_avg")
elco25<-rename(elco25,elco25="value")
elco25<-rename(elco25,elco25_avg="value_avg")
elco50<-rename(elco50,elco50="value")
elco50<-rename(elco50,elco50_avg="value_avg")
orgc<-rename(orgc,orgc="value")
orgc<-rename(orgc,orgc_avg="value_avg")
elcosp<-rename(elcosp,elcosp="value")
elcosp<-rename(elcosp,elcosp_avg="value_avg")
nitkjd<-rename(nitkjd,nitkjd="value")
nitkjd<-rename(nitkjd,nitkjd_avg="value_avg")
orgm<-rename(orgm,orgm="value")
orgm<-rename(orgm,orgm_avg="value_avg")
phaq<-rename(phaq,phaq="value")
phaq<-rename(phaq,phaq_avg="value_avg")
phca<-rename(phca,phca="value")
phca<-rename(phca,phca_avg="value_avg")
phetb1<-rename(phetb1,phetb1="value")
phetb1<-rename(phetb1,phetb1_avg="value_avg")
phetm3<-rename(phetm3,phetm3="value")
phetm3<-rename(phetm3,phetm3_avg="value_avg")
phetol<-rename(phetol,phetol="value")
phetol<-rename(phetol,phetol_avg="value_avg")
phkc<-rename(phkc,phkc="value")
phkc<-rename(phkc,phkc_avg="value_avg")
phnf<-rename(phnf,phnf="value")
phnf<-rename(phnf,phnf_avg="value_avg")
phprtn<-rename( phprtn, phprtn="value")
phprtn<-rename( phprtn, phprtn_avg="value_avg")
phptot<-rename(phptot,phptot="value")
phptot<-rename(phptot,phptot_avg="value_avg")
phpwsl<-rename(phpwsl,phpwsl="value")
phpwsl<-rename(phpwsl,phpwsl_avg="value_avg")
sand<-rename(sand,sand="value")
sand<-rename(sand,sand_avg="value_avg")
silt<-rename(silt,silt="value")
silt<-rename(silt,silt_avg="value_avg")
tceq<-rename(tceq,tceq="value")
tceq<-rename(tceq,tceq_avg="value_avg")
totc<-rename(totc,totc="value")
totc<-rename(totc,totc_avg="value_avg")
wg0006<-rename(wg0006,wg0006="value")
wg0006<-rename(wg0006,wg0006_avg="value_avg")
wg0010<-rename(wg0010,wg0010="value")
wg0010<-rename(wg0010,wg0010_avg="value_avg")
wg0033<-rename(wg0033,wg0033="value")
wg0033<-rename(wg0033,wg0033_avg="value_avg")
wg0100<-rename(wg0100,wg0100="value")
wg0100<-rename(wg0100,wg0100_avg="value_avg")
wg0200<-rename(wg0200,wg0200="value")
wg0200<-rename(wg0200,wg0200_avg="value_avg")
wg0500<-rename(wg0500,wg0500="value")
wg0500<-rename(wg0500,wg0500_avg="value_avg")
wg1500<-rename(wg1500,wg1500="value")
wg1500<-rename(wg1500,wg1500_avg="value_avg")
wv0010<-rename(wv0010,wv0010="value")
wv0010<-rename(wv0010,wv0010_avg="value_avg")
wv0033<-rename(wv0033,wv0033="value")
wv0033<-rename(wv0033,wv0033_avg="value_avg")
wv0500<-rename(wv0500,wv0500="value")
wv0500<-rename(wv0500,wv0500_avg="value_avg")
wv1500<-rename(wv1500,wv1500="value")
wv1500<-rename(wv1500,wv1500_avg="value_avg")

#remove data columns from each csv to streamline merge process
for (i in dataframe_names) {
  x=get(i)
  x$layer_name<-NULL
  x$method_options<-NULL
  x$licence<-NULL
  x$positional_uncertainty<-NULL
  assign(i,x)
}

#MERGE ALL DATA INTO A SINGLE DATAFRAME
#make vector list of data sets I want to merge
data_merge<-list("tceq","orgm","orgc","totc","nitkjd","ecec","cfgr","cfvo","clay","silt","sand","phaq","phptot","wg1500")
data_merge<-as.vector(data_merge)
#create dataframe to merge all csv information into
wosis<-data.frame()

#add all data into the wosis dataframe
for (j in data_merge) {
  x=get(j)
  wosis<-merge(wosis,x,all=TRUE)
}

wosis<-merge(wosis,profiles)
write.csv(wosis,"wosis_og.csv", row.names = FALSE)
wosis <- read.csv("~/Fall 2025/THESIS/THESIS/WoSIS_Data/wosis_og.csv")

#save a copy of this dataframe to return to if any mistakes are made later
wosis_1<-wosis
wosis<-wosis_1

#a lot of different things I tried and they did not work
#simple croping to one smu
#test_crop<-crop(hwsd,79)
#bbox(test_crop)
#units <- as.vector(RG_SMU)
#coords<-data.frame()
#for (x in units) {
  #cells.x <- which(values(hwsd) == x)
  #coords.x<-xyFromCell(hwsd, cells.x)
  #coords.x<-as.data.frame(coords.x)
  #coords<-rbind(coords,coords.x)
#}

#smu units are not square/rectable to latitude and longitude bounds are not consistent

# Get cell numbers for the mapping unit
#cells <- which(values(hwsd) == 81)
# Get coordinates for those cells
#coords<-xyFromCell(hwsd, cells)
# Extract centroids
#centroids <- centroids(units)
# Extract coordinates
#coords <- crds(centroids)
# Combine with attributes if needed
#output <- cbind(as.data.frame(units), coords)
# Rename coordinate columns
#colnames(output)[(ncol(output)-1):ncol(output)] <- c("longitude", "latitude")
# View result
#head(output)

# Get cell numbers for the mapping unit
#cells <- which(values(hwsd) == 81)
# Get coordinates for those cells
#coords<-xyFromCell(hwsd, cells)
#bbox(coords)

#coords.df<-as.data.frame(coords)
#coords.df<-rename(coords.df,lat="x")
#coords.df<-rename(coords.df,long="y")

#coords<-rename(coords,lat="x")
#coords<-rename(coords,long="y")

#wosis$X<-as.numeric(wosis$X)
#wosis$Y<-as.numeric(wosis$Y)
#wosis$lat<-substr(wosis$X, 1, 8)
#wosis$long<-substr(wosis$Y, 1, 8)
#wosis$lat<-as.numeric(wosis$lat)
#wosis$long<-as.numeric(wosis$long)

#intersect(wosis$long,coords$long)

#regosol.81<-merge(wosis,coords,by=c("lat","long"))

#merged <- stringdist_join(wosis, coords,
                          #by = c("lat","long"),
                          #method = "lv",       # Jaro-Winkler distance
                          #max_dist = 0.15,     # Adjust threshold based on tolerance
                          #mode = "right")       # Keep all rows from df1

#print(merged)

#range(wosis$lat)
#range(coords$lat)
#range(wosis$long)
#range(coords$long)

#mask_raster <- hwsd == 79
#mask_raster
# Apply the mask to the raster
#cropped_raster <- mask(hwsd, mask_raster, maskvalues = FALSE)
#cropped_raster <- trim(cropped_raster)
#plot(cropped_raster)

#bbox(cells)

#m <- dbDriver("SQLite")
#con <- dbConnect(m, dbname = "HWSD.sqlite")
#dbListTables(con)

#m<- dbDriver("SQLite")
#con<- dbConnect(m,dbname = "HWSD.sqlite")
#dbListTables(con)

#dbGetQuery(con, "pragma table_info(HWSD_DATA)")$name
#con

#dbGetQuery(con, "select count(*) as grid_total from HWSD_DATA")


# 1. Load the raster
#r <- rast("your_raster.tif")

# 2. Create a mask of the specific mapping unit (e.g., value 42)
#mu_value <- 79
#mask_r <- hwsd == mu_value
#mask_r
# 3. Trim the masked raster to remove NA borders (optional but recommended)
#trimmed_r <- trim(mask_r)
#trimmed_r
# 4. Get the extent (bounding box)
#ext <- ext(trimmed_r)


# 5. If the raster is not already in lat/lon (WGS84), reproject it
#if (crs(hwsd) != "EPSG:4326") {
 # trimmed_r <- project(trimmed_r, "EPSG:4326")
  #ext <- ext(trimmed_r)
#}

# 6. Extract coordinates of the bounding box
#bounds <- list(
 # xmin = ext[1],
  #xmax = ext[2],
  #ymin = ext[3],
  #ymax = ext[4]
#)

# Print the bounds
#print(bounds)

#coords.df<-as.data.frame(coords)

#wosis.rows <- split(wosis,seq(nrow(wosis))) 
#wosis.rows

#for (i in wosis$X) {
 # for (j in coords.df$x) {
    
  #}
#}

#if(wosis$X==coords.df$x & wosis$Y==coords.df$Y){
 # print("True")  
#}

#r <- rast(xmin = -180, xmax = 180, ymin = -90, ymax = 90,
          #resolution = c(0.05,0.05), crs = "EPSG:4326")

# Convert the raster to polygons (grid cells)
#grid <- as.polygons(r)

# Optional: give each grid cell an ID
#grid$cell_id <- 1:nrow(grid)

# Plot to verify
#plot(grid)

#crs(hwsd)
#crs(r)
# Resample soil to match grid resolution (if needed)
#soil_resampled <- resample(hwsd.sp, r, method = "near")
#class(hwsd.sp)
#class(grid)

#hwsd.sp <- rast(hwsd)

# Get modal soil unit per grid cell
#zonal_stats <- zonal(hwsd.sp, grid, fun = "modal", na.rm = TRUE)

# Attach results
#grid$dominant_soil_unit <- zonal_stats$hwsd

#grid_raster <- rast(xmin = -180, xmax = 180, ymin = -90, ymax = 90,
                   # resolution = c(5, 5), crs = "EPSG:4326")

# Match CRS
#soil <- project(hwsd, grid_raster)

# Resample
#soil_resampled <- resample(soil, grid_raster, method = "near")
#print(grid)
#grid.df<-as.data.frame(grid)
#tests<-merge(grid.df,RG_SMU,by.x="dominant_soil_unit",by.y="HWSD2_SMU_ID")
#RG_SMU

#WHAT ACTUALLY WORKED
#make a raster with a grid of desired size
r <- rast(xmin = -180, xmax = 180, ymin = -90, ymax = 90,
          resolution = c(0.05,0.05), crs = "EPSG:4326")
#convert the raster to polygons (grid cells)
grid <- as.polygons(r)
#give each grid cell an ID
grid$cell_id <- 1:nrow(grid)
#make sure the hwsd and grid rasters are made in the same package (sp instead of terra)
hwsd.sp <- rast(hwsd)
#resample soil to match grid resolution
soil_resampled <- resample(hwsd.sp, r, method = "near")
#fet modal soil unit per grid cell
zonal_stats <- zonal(hwsd.sp, grid, fun = "modal", na.rm = TRUE)
#write csv of zonal_stats (previous line took 6 days to run I am not doing that again)
write.csv(zonal_stats,"zonalstats.csv", row.names = FALSE)
#attach results
grid$dominant_soil_unit <- zonal_stats$hwsd
#convert grid into a dataframe
grid.df<-as.data.frame(grid)
#write grid.df as a csv
write.csv(grid.df,"grid_SMU.csv", row.names = FALSE)
#merge grid.df with RG_SMU to isolate only cells where regosols are dominant
grided_RG_SMU<-merge(grid.df,RG_SMU,by.x="dominant_soil_unit",by.y="HWSD2_SMU_ID")

#upload saved grid dataframe (to skip the code above)
grid_SMU <- read.csv("~/Fall 2025/THESIS/THESIS/grid_SMU.csv")

#make a raster with a grid of desired size
r <- rast(xmin = -180, xmax = 180, ymin = -90, ymax = 90,
          resolution = c(0.05, 0.05), crs = "EPSG:4326")
#attach raster cells to a cell numbers
cells <- 1:ncell(r)
# Get cell centers
xy <- xyFromCell(r, cells)
# Compute half the resolution
halfres <- res(r) / 2
# Create bounding box columns
bounds <- data.frame(
  cell_id = cells,
  xmin = xy[, 1] - halfres[1],
  xmax = xy[, 1] + halfres[1],
  ymin = xy[, 2] - halfres[2],
  ymax = xy[, 2] + halfres[2]
)

#merge grid_SMU with bounds
smu_latlon<-merge(grid_SMU,bounds,by="cell_id")

write.csv(smu_latlon,"smu_latlon.csv", row.names = FALSE)

smu_latlon <- read.csv("~/Fall 2025/THESIS/THESIS/smu_latlon.csv")

#Now have to filter out soil types from this data set
#upload HWSD SMU categorized by WRB 
HWSD2_LAYERS <- read_excel("~/HWSD2_LAYERS.xlsx")
#make it smaller
hwsd_smaller<-with(HWSD2_LAYERS,subset(HWSD2_LAYERS,select=c(HWSD2_SMU_ID,WRB2)))
#subset out soil type of interest
hwsd_SMU_lp<-with(hwsd_smaller,subset(hwsd_smaller,WRB2=="LP"))
hwsd_SMU_rg<-with(hwsd_smaller,subset(hwsd_smaller,WRB2=="RG"))
#merge with smu_latlon data set
lp_smu_latlon<-merge(hwsd_SMU_lp,smu_latlon,by.x="HWSD2_SMU_ID",by.y="dominant_soil_unit",all=FALSE)
grid_rg_smu_latlon<-merge(hwsd_SMU_rg,grid_SMU_latlon,by.x="HWSD2_SMU_ID",by.y="dominant_soil_unit",all=FALSE)

#make wosis and lp_smu data smaller to run some tests 
wosis_tests<-wosis[1:100000,]
lp_smu_tests<-grid_lp_smu_latlon[1:10000,]

#
#create a new wosis dataframe
#lp_wosis<-data.frame()

#move just rows within lp bounds into lp_wosis
#move_rows <- function(wosis_tests, lp_wosis, condition) {
#  condition_expr <- rlang::enquo(wosis$X<=grid_lp_smu_latlon$xmax&wosis$X>=grid_lp_smu_latlon$xmin)
#  rows_to_move <- df_from %>% filter(!!wosis$X<=grid_lp_smu_latlon$xmax&wosis$X>=grid_lp_smu_latlon$xmin)
#  df_to_new   <- bind_rows(df_to, rows_to_move)
#}
#this did not work, no data got put into the new dataframe

#testing to see if filtering works
#tests <- wosis %>% filter(wosis$X<=grid_lp_smu_latlon$xmax, wosis$X>=grid_lp_smu_latlon$xmin)
#tests.1 <- tests %>% filter(tests$Y<=grid_lp_smu_latlon$ymax,tests$Y>=grid_lp_smu_latlon$ymin)
#it does not 

#trying to write a loop
#make data sets smaller to test
#row_number<-list(1:860996)
#row_number<-as.vector(row_number)

#row_number_grid<-list(1:1000)
#row_number_grid<-as.vector(row_number_grid)

#for (i in row_number) {
#  lat=print(wosis[i,1])
#  lon=print(wosis[i,2])
#  for (j in row_number_grid) {
#    xmin=get(grid_lp_smu_latlon[j,4])
#    xmax=get(grid_lp_smu_latlon[j,5])
#    ymin=get(grid_lp_smu_latlon[j,6])
#    ymax=get(grid_lp_smu_latlon[j,7])
#  }
#  if (lat<=xmax & lat>= xmin & lon<=ymax&lon>=ymin){
#      rbind(wosis_lp,wosis[i,])
#  }
#}
#doesn't work, can't tell you why

#try using the data.table package
# Convert to data.tables
#points_dt <- as.data.table(wosis)
#bounds_dt <- as.data.table(lp_smu_latlon)

# For each point, check whether it falls inside any bounding box
# This version avoids looping over bounds using foverlaps() but requires bounds to be treated as intervals

# Create intervals
#setnames(bounds_dt, c("smu_id","wrb","cell_id","lat_min", "lat_max", "lon_min", "lon_max"))
#setkey(bounds_dt, lon_min, lon_max, lat_min, lat_max)

# Create interval-like columns for points (zero-width)
#points_dt[, lon_min := lon]
#points_dt[, lon_max := lon]
#points_dt[, lat_min := lat]
#points_dt[, lat_max := lat]

#ranme column names for simplicity
#lon<-"lon"
#lat<-"lat"
#points_dt[, paste0(X, "_min") := X]
#points_dt[, paste0(X, "_max") := X]
#points_dt[, paste0(Y, "_min") := Y]
#points_dt[, paste0(Y, "_max") := Y]

#set(points_dt, j = paste(lon, "_min"), value = points_dt[[X]])
#set(points_dt, j = paste(lon, "_max"), value = points_dt[[X]])
#set(points_dt, j = paste(lat, "_min"), value = points_dt[[Y]])
#set(points_dt, j = paste(lat, "_max"), value = points_dt[[Y]])

# Set same key for overlap join
#setkey(points_dt, lon_min, lon_max, lat_min, lat_max)

# Perform fast interval overlap join
#result <- foverlaps(points_dt, bounds_dt, nomatch = 0L)

# Keep only original point columns
#filtered_points <- unique(result[, .(lat, lon, value)])
#did not work

#tweeking this code
#wosis<-rename(wosis,x_col="lon")
#wosis<-rename(wosis,y_col="Y")
#lp_smu_latlon<-rename(lp_smu_latlon,x_min_col="xmin")
#lp_smu_latlon<-rename(lp_smu_latlon,x_max_col="xmax")
#lp_smu_latlon<-rename(lp_smu_latlon,y_min_col="ymin")
#lp_smu_latlon<-rename(lp_smu_latlon,y_max_col="ymax")
#lp_smu_latlon_save<-lp_smu_latlon
#lp_smu_latlon<-with(lp_smu_latlon,subset(lp_smu_latlon,select=c("x_min_col","x_max_col","y_min_col","y_max_col")))

#filter_points_in_bounds <- function(points_df, bounds_df,
#                                    x_col = "x_col", y_col = "y_col",
#                                x_min_col = "x_min_col", x_max_col = "x_max_col",
#                                y_min_col = "y_min_col", y_max_col = "y_max_col") {
  # Convert to data.table
#  points_dt <- as.data.table(points_df)
#  bounds_dt <- as.data.table(bounds_df)
  
  # --- Create zero-width intervals for the points safely ---
#  set(points_dt, j = sprintf("%s_min", x_col), value = points_dt[[x_col]])
#  set(points_dt, j = sprintf("%s_max", x_col), value = points_dt[[x_col]])
#  set(points_dt, j = sprintf("%s_min", y_col), value = points_dt[[y_col]])
#  set(points_dt, j = sprintf("%s_max", y_col), value = points_dt[[y_col]])
  
  # --- Rename bounding box columns temporarily to match point interval names ---
#  setnames(bounds_dt,
#         old = c(x_min_col, x_max_col, y_min_col, y_max_col),
#           new = c(sprintf("%s_min", x_col), sprintf("%s_max", x_col),
#                   sprintf("%s_min", y_col), sprintf("%s_max", y_col)))
  
  # --- Set keys for interval overlap ---
#  setkeyv(bounds_dt, c(sprintf("%s_min", x_col), sprintf("%s_max", x_col),
#                       sprintf("%s_min", y_col), sprintf("%s_max", y_col)))
#  setkeyv(points_dt, c(sprintf("%s_min", x_col), sprintf("%s_max", x_col),
#                       sprintf("%s_min", y_col), sprintf("%s_max", y_col)))
  
  # --- Perform overlap join ---
#  result <- foverlaps(points_dt, bounds_dt,nomatch=0L)
  
#  return(result)
#}

#filtered_points <- filter_points_in_bounds(wosis, lp_smu_latlon)
#does nto work 

# --- Keep only the original point columns ---
#filtered_points <- unique(result[, .(y_col, x_col, value)])

#check<-with(filtered_points,subset(filtered_points,y_col_min!="NA"))

#tweak this code again
#filter_points_in_bounds <- function(points_df, bounds_df,
#                                    x_col = "lon", y_col = "lat",
#                                    x_min_col = "lon_min", x_max_col = "lon_max",
#                                    y_min_col = "lat_min", y_max_col = "lat_max") {
  # Convert to data.table
#  points_dt <- as.data.table(points_df)
#  bounds_dt <- as.data.table(bounds_df)
  
  # --- Check columns exist ---
#  stopifnot(all(c(x_col, y_col) %in% names(points_dt)))
#  stopifnot(all(c(x_min_col, x_max_col, y_min_col, y_max_col) %in% names(bounds_dt)))
  
  # --- Create zero-width intervals for points safely ---
#  set(points_dt, j = paste0(x_col, "_min"), value = points_dt[[x_col]])
#  set(points_dt, j = paste0(x_col, "_max"), value = points_dt[[x_col]])
#  set(points_dt, j = paste0(y_col, "_min"), value = points_dt[[y_col]])
#  set(points_dt, j = paste0(y_col, "_max"), value = points_dt[[y_col]])
  
  # --- Temporarily rename bounds columns to match interval names ---
#  setnames(bounds_dt,
#           old = c(x_min_col, x_max_col, y_min_col, y_max_col),
#           new = c(paste0(x_col, "_min"), paste0(x_col, "_max"),
#                   paste0(y_col, "_min"), paste0(y_col, "_max")),
#           skip_absent = FALSE)
  
  # --- Set keys for fast overlap ---
#  setkeyv(bounds_dt, c(paste0(x_col, "_min"), paste0(x_col, "_max"),
#                       paste0(y_col, "_min"), paste0(y_col, "_max")))
#  setkeyv(points_dt, c(paste0(x_col, "_min"), paste0(x_col, "_max"),
#                       paste0(y_col, "_min"), paste0(y_col, "_max")))
  
  # --- Perform the overlap join ---
#  result <- foverlaps(points_dt, bounds_dt, nomatch = 0L)
  
  # --- Keep only original point columns ---
#  filtered_points <- unique(result[, names(points_df), with = FALSE])
  
#  return(filtered_points)
#}

#if loop
#Assessing by season: Read in seasons data and assign a season to each measurement date
#seasons <- read.csv("Season3 for R 20231116.csv", header=T)
#seasons$Date <- as.POSIXct(strptime(seasons$Date, format="%m/%d/%y"))
#str(seasons)

#mydata_withseasons <- mydata %>% #Add a blank season column
#  dplyr::mutate(season = NA)

#for(i in 1:nrow(mydata_withseasons)) { #Assigning a season to each site start date
#  date <- mydata_withseasons$Site.start.date[i]
  
#  for(j in 1:nrow(seasons)){
#    if(date >= seasons$Date[j] & date < seasons$Date[j+1] & !is.na(date) | seasons$Date[j] == "2023-06-01" & date >= seasons$Date[j] & !is.na(date)){
#      mydata_withseasons$season[i] <- seasons$Season3[j]
#    }
#  }
#}
#try this formt for my data
#tests<- data.frame()
#for (i in 1:nrow(wosis)) {
#  lon<-wosis$x_col[i]
#  lat<-wosis$y_col[i]
#  for (j in 1:nrow(lp_smu_latlon)) {
#    if(lon >= lp_smu_latlon$x_min_col[j] & lon <= lp_smu_latlon$x_max_col[j] & lat >= lp_smu_latlon$y_min_col[j] & lat <= lp_smu_latlon$y_max_col[j]){
#      tests<-rbind(tests,wosis[i,])
#    }
#  }
#}
#tried this, i believe that it worked, but it took too long for it to run on the whole data set

#lapply loop
#test<-do.call(rbind,lapply(1:nrow(lp_smu_latlon),function(i){
#  filter(wosis,
#         between(X, lp_smu_latlon$xmin[i], lp_smu_latlon$xmax[i]),
#         between(Y, lp_smu_latlon$ymin[i], lp_smu_latlon$ymax[i]))
#}))
#I believe this works but also takes too long

#using the package data.table
#format data sets for data.table
setDT(wosis)
setDT(lp_smu_latlon)

#run data.table function
test <- lp_smu_latlon[wosis, 
                      on = .(xmin <= X, xmax >= X, ymin <= Y, ymax >= Y),
                      nomatch = 0]
#this worked!

#a fair amount of duplicates in the data set, seeing if I can get it to a reasonable mount
test.1<-unique(test)
test.2<-unique(test.1)
#narrowed it down a little bit but still see duplicates 

#removed duplicates from lp_smu_latlon and then  ran it
hwsd_SMU_lp.1<-unique(hwsd_SMU_lp)
lp_smu_latlon.1<-merge(hwsd_SMU_lp.1,smu_latlon,by.x="HWSD2_SMU_ID",by.y="dominant_soil_unit",all=FALSE)
setDT(wosis)
setDT(lp_smu_latlon.1)
test.3 <- lp_smu_latlon.1[wosis, 
                      on = .(xmin <= X, xmax >= X, ymin <= Y, ymax >= Y),
                      nomatch = 0]
#got to the same point as the earlier unique functions
#try again for fun
test.4<-unique(test.3)
#didn't do anything

#clean up test.2 a bit
test.2<-test.3
test.2$HWSD2_SMU_ID<-NULL
test.2$WRB2<-NULL
test.2[,1:7]<-NULL
test.2[,7:8]<-NULL
test.2[,43:59]<-NULL
test.2$dataset_code<-NULL
test.2$site_id<-NULL

#realized that test.2 does not have any tceq values in it, running the same sorting on just that dataset to see if there randomly just is no data
setDT(tceq)
setDT(lp_smu_latlon.1)
test.5 <- lp_smu_latlon.1[tceq,on = .(xmin <= X, xmax >= X, ymin <= Y, ymax >= Y),nomatch = 0]
#nope there is data :/

#try sorting each individual dataset before merging
#trying to automate cleaning up these data sets 
for (i in data_merge) {
  x=get(i)
  x$layer_name<-NULL
  x$method_options<-NULL
  x$licence<-NULL
  x$positional_uncertainty<-NULL
  assign(i,x)
}
#this works

#for (j in data_merge) {
#  x=get(i)
#  setDT(x)
#  x.1<-lp_smu_latlon.1[x,on = .(xmin <= X, xmax >= X, ymin <= Y, ymax >= Y),nomatch = 0]
#  assign(i,x)
#}
#this does not work 

#its going to take less time to do it by hand :/
setDT(lp_smu_latlon.1)
cfgr<-rename(cfgr,cfgr="x")
cfgr<-rename(cfgr,cfgr_avg="x_avg")
setDT(cfgr)
cfgr.1 <- lp_smu_latlon.1[cfgr,on = .(xmin <= X, xmax >= X, ymin <= Y, ymax >= Y),nomatch = 0]

cfvo<-rename(cfvo,cfvo="x")
cfvo<-rename(cfvo,cfvo_avg="x_avg")
setDT(cfvo)
cfvo.1 <- lp_smu_latlon.1[cfvo,on = .(xmin <= X, xmax >= X, ymin <= Y, ymax >= Y),nomatch = 0]

clay<-rename(clay,clay="x")
clay<-rename(clay,clay_avg="x_avg")
setDT(clay)
clay.1 <- lp_smu_latlon.1[clay,on = .(xmin <= X, xmax >= X, ymin <= Y, ymax >= Y),nomatch = 0]

ecec<-rename(ecec,ecec="x")
ecec<-rename(ecec,ecec_avg="x_avg")
setDT(ecec)
ecec.1 <- lp_smu_latlon.1[ecec,on = .(xmin <= X, xmax >= X, ymin <= Y, ymax >= Y),nomatch = 0]

orgc<-rename(orgc,orgc="x")
orgc<-rename(orgc,orgc_avg="x_avg")
setDT(orgc)
orgc.1 <- lp_smu_latlon.1[orgc,on = .(xmin <= X, xmax >= X, ymin <= Y, ymax >= Y),nomatch = 0]

nitkjd<-rename(nitkjd,nitkjd="x")
nitkjd<-rename(nitkjd,nitkjd_avg="x_avg")
setDT(nitkjd)
nitkjd.1 <- lp_smu_latlon.1[nitkjd,on = .(xmin <= X, xmax >= X, ymin <= Y, ymax >= Y),nomatch = 0]

orgm<-rename(orgm,orgm="x")
orgm<-rename(orgm,orgm_avg="x_avg")
setDT(orgm)
orgm.1 <- lp_smu_latlon.1[orgm,on = .(xmin <= X, xmax >= X, ymin <= Y, ymax >= Y),nomatch = 0]

phaq<-rename(phaq,phaq="x")
phaq<-rename(phaq,phaq_avg="x_avg")
setDT(phaq)
phaq.1 <- lp_smu_latlon.1[phaq,on = .(xmin <= X, xmax >= X, ymin <= Y, ymax >= Y),nomatch = 0]

phetol<-rename(phetol,phetol="x")
phetol<-rename(phetol,phetol_avg="x_avg")
setDT(phetol)
phetol.1 <- lp_smu_latlon.1[phetol,on = .(xmin <= X, xmax >= X, ymin <= Y, ymax >= Y),nomatch = 0]

sand<-rename(sand,sand="value")
sand<-rename(sand,sand_avg="value_avg")
setDT(sand)
sand.1 <- lp_smu_latlon.1[sand,on = .(xmin <= X, xmax >= X, ymin <= Y, ymax >= Y),nomatch = 0]

silt<-rename(silt,silt="value")
silt<-rename(silt,silt_avg="value_avg")
setDT(silt)
silt.1 <- lp_smu_latlon.1[silt,on = .(xmin <= X, xmax >= X, ymin <= Y, ymax >= Y),nomatch = 0]

tceq<-rename(tceq,tceq="value")
tceq<-rename(tceq,tceq_avg="value_avg")
setDT(tceq)
tceq.1 <- lp_smu_latlon.1[tceq,on = .(xmin <= X, xmax >= X, ymin <= Y, ymax >= Y),nomatch = 0]

totc<-rename(totc,totc="value")
totc<-rename(totc,totc_avg="value_avg")
setDT(totc)
totc.1 <- lp_smu_latlon.1[tceq,on = .(xmin <= X, xmax >= X, ymin <= Y, ymax >= Y),nomatch = 0]

wg1500<-rename(wg1500,wg1500="value")
wg1500<-rename(wg1500,wg1500_avg="value_avg")
setDT(wg1500)
wg1500.1 <- lp_smu_latlon.1[wg1500,on = .(xmin <= X, xmax >= X, ymin <= Y, ymax >= Y),nomatch = 0]

cfgr.2<-unique(cfgr.1)
cfvo.2<-unique(cfvo.1)
clay.2<-unique(clay.1)
ecec.2<-unique(ecec.1)
nitkjd.2<-unique(nitkjd.1)
orgc.2<-unique(orgc.1)
orgm.2<-unique(orgm.1)
phaq.2<-unique(phaq.1)
phetol.2<-unique(phetol.1)
sand.2<-unique(sand.1)
silt.2<-unique(silt.1)
tceq.2<-unique(tceq.1)
totc.2<-unique(totc.1)
wg1500.2<-unique(wg1500.1)

#make vector list of data sets I want to merge
merge<-list("tceq.2","orgm.2","orgc.2","totc.2","nitkjd.2","ecec.2","cfgr.2","cfvo.2","clay.2","silt.2","sand.2","phaq.2","phetol.2","wg1500.2")
merge<-as.vector(merge)
#create dataframe to merge all csv information into
wosis.lp<-data.frame()

#add all data into the wosis dataframe
for (j in merge) {
  x=get(j)
  wosis.lp<-merge(wosis.lp,x,all=TRUE)
}

wosis.lp$country_name<-as.factor(wosis.lp$country_name)
levels(wosis.lp$country_name)

wosis.lp %>% count(country_name)
wosis.lp.date<-subset(wosis.lp,wosis.lp$date!="????-??-??")

library(ggplot2)
ggplot(lp.all,aes(x=longitude,y=latitude,color=country_name.x))+
  geom_point(show.legend = FALSE)

wosis.lp.date.prof<-merge.data.frame(wosis.lp.date,profiles,by.x = c("xmin","ymin"),by.y = c("X","Y"),all.x = TRUE, all.y = FALSE)

wosis.lp.date.prof[,57:73]<-NULL
wosis.lp.date.prof[,45:46]<-NULL
wosis.lp.date.prof[,1:8]<-NULL
wosis.lp.date.prof[,41:44]<-NULL

write.csv(wosis.lp.date.prof,"wosis.lp.date.prof.csv",row.names = FALSE)

setDT(lp.all)
test_1 <- lp_smu_latlon.1[lp.all,on = .(xmin <= X, xmax >= X, ymin <= Y, ymax >= Y),nomatch = 0]
