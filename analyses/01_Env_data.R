## Environmental data ##
########################

library(sp)
library(raster)
library(rgdal)
library(gsl)
library(palinsol)
library(envirem)
library(rgeos)
library(viridis)
library(maps)

# Bulk download
f <- read.table("data/CHELSA/envidatS3paths.txt") ; f <- as.character(f$V1)
destfile <- "data/CHELSA/global/"
for(i in 1:length(f)){
  download.file(f[i],destfile=paste0(destfile,strsplit(as.character(f[i]), "/")[[1]][10]), mode="wb")
}

# Import global data maps and crop/mask on sweden

for(country in c("sweden","france")){

  inputDir <- "./Data/CHELSA/global"
  outputDir <- paste0("./Data/CHELSA/",country)

  # dir <- list.files(outputDir, full.names = TRUE)
  # lapply(dir, function(x){dir.create(paste0(x,"/bioclim"), showWarnings = F)})

  files <- list.files(inputDir, pattern = '.tif$', full.names = TRUE)
  tifOptions <- c("COMPRESS=DEFLATE", "PREDICTOR=2", "ZLEVEL=6")

  # Crop and mask maps for Sweden
  for (i in 1:length(files)) {
    cat(i, ' ')
    r <- stack(files[i])
    r <- crop(r, get(paste0(country,".spdf")))

    # apply terrestrial mask
    # r <- mask(r, sweden.spdf)

    # if (grepl('temp|tmax|tmin', files[i])) {
    #   r <- r / 10 # for CHELSA timeseries
    # }

    if (grepl('tas', files[i])) {
      r <- r / 10 # for CHELSA [CMIP5]
      # r <- r - 273.15 # for CHELSA [CMIP5] timeseries
    }
    if (grepl('pr', files[i])) {
      r[r[]<0] <- NA
    }

    out <- gsub('-', '.',paste0(strsplit(strsplit(files[i], "global")[[1]][2],".tif")[[1]][1], ".",country,".tif"))
    fold <- paste(strsplit(strsplit(out,"_")[[1]][9],paste0(".",country))[[1]][1],
                  strsplit(out,"_")[[1]][4], strsplit(out,"_")[[1]][5],  sep="_")
    outfile <- paste0(outputDir,"/",fold, out)
    outfile <- gsub('_(\\d)_', '_0\\1_', outfile)
    writeRaster(r, filename = outfile, format = 'GTiff', options = tifOptions, overwrite = TRUE)
    print(paste0(i,"/",length(files)))
  }

}


# Present-day envirem variables 1979-2013

dir <- './Data/CHELSA/france/timeseries/'
dir.fig <- 'Figs/02_envirem/france_1979.2013/'
files <- list.files(dir, full.names = TRUE)
rasterTemplate <- raster(files[1])
refyear <- 1996
ETsolradRasters(rasterTemplate = rasterTemplate, year = refyear-1950, outputDir = dir, overwrite = TRUE)

assignNames(tmax = "CHELSA_tmax_##_1979.2013_V1.2_land.france",
            tmin = "CHELSA_tmin10_##_1979.2013_V1.2_land.france",
            tmean = "CHELSA_temp10_##_1979.2013_V1.2_land.france",
            precip = "CHELSA_prec_##_V1.2_land.france",
            solrad = "et_solrad_##")
varnames()
verifyFileStructure(dir, returnFileNames = FALSE)
outdir <- paste0(dir,"envirem")
dir.create(outdir)


# chelsaFiles <- list.files(dir, pattern = 'CHELSA', full.names = TRUE)
# chelsaStack <- stack(chelsaFiles)
# solarFiles <- list.files(dir, pattern = 'sol', full.names = TRUE)
# solarStack <- stack(solarFiles)
# verifyRasterNames(chelsaStack, solradstack = solarStack)


## GENERATE ENVIREM variables
generateRasters(var = 'all',
                maindir = dir,
                outputDir = outdir)

enviremRasters <- list.files(outdir, pattern = '\\.tif$', full.names = TRUE)
enviremRasters <- stack(enviremRasters)


## PLOT ENVIREM maps
dir.create(dir.fig)
for (l in 1:nlayers(enviremRasters)) {
  png(paste0(dir.fig, names(enviremRasters)[l],'_',strsplit(dir.fig,"/")[[1]][3],".png"),
      pointsize = 4, res = 300, width = 1000, height = 1000)
  plot(enviremRasters[[l]], col = inferno(100), box = FALSE, axes = FALSE)
  title(main = names(enviremRasters)[l])
  dev.off()
}

## PLOT bioclim maps
dir.create(paste0(dir.fig,'/bioclim'))
bioclimRasters <- list.files(dir, pattern = 'bio.*\\.tif$', full.names = TRUE)
bioclimRasters <- stack(bioclimRasters)
names(bioclimRasters) <- c('Annual Mean Temperature','Mean Diurnal Range','Isothermality','Temperature Seasonality','Max Temperature of Warmest Month','Min Temperature of Coldest Month','Temperature Annual Range',
                           'Mean Temperature of Wettest Quarter','Mean Temperature of Driest Quarter','Mean Temperature of Warmest Quarter','Mean Temperature of Coldest Quarter','Annual Precipitation','Precipitation of Wettest Month',
                           'Precipitation of Driest Month','Precipitation Seasonality','Precipitation of Wettest Quarter','Precipitation of Driest Quarter','Precipitation of Warmest Quarter','Precipitation of Coldest Quarter')
for (l in 1:nlayers(bioclimRasters)) {
  png(paste0(paste0(dir.fig,'/bioclim/'), names(bioclimRasters)[l],'_',strsplit(dir.fig,"/")[[1]][3],".png"),
      pointsize = 4, res = 300, width = 1000, height = 1000)
  plot(bioclimRasters[[l]], col = inferno(100), box = FALSE, axes = FALSE)
  title(main = names(bioclimRasters)[l])
  dev.off()
}


## Europe maps
inputDir <- "./Data/CHELSA/global"
outputDir <- "./Data/CHELSA/europe"

files <- list.files(inputDir, pattern = '.tif$', full.names = TRUE)
tifOptions <- c("COMPRESS=DEFLATE", "PREDICTOR=2", "ZLEVEL=6")

europe.spdf <- readRDS("Data/z_Map/Europe.rds")
europe.spdf@bbox[1] <- -11
mask <- stack(files[1])
mask <- crop(mask, europe.spdf)
mask[mask[]<0] <- NA
mask[!is.na(mask[])] <- 1

for (i in 1:length(files)) {
  cat(i, ' ')
  r <- stack(files[i])
  r <- crop(r, mask)

  # apply terrestrial mask
  r <- mask(r, mask)

  if (grepl('temp|tmax|tmin', files[i])) {
    r <- r / 10 # for CHELSA timeseries
  }

  if (grepl('pr', files[i])) {
    r[r[]<0] <- NA
  }

  outfile <- paste0(outputDir, strsplit(strsplit(files[i], "global")[[1]][2],".tif")[[1]][1], '.europe.tif')
  outfile <- gsub('_(\\d)_', '_0\\1_', outfile)
  outfile <- gsub('-', '.', outfile)
  writeRaster(r, filename = outfile, format = 'GTiff', options = tifOptions, overwrite = TRUE)
  print(paste0(i,"/48"))
}

# Lower spatial resolution

files <- list.files(outputDir, pattern = '.tif$', full.names = TRUE)
for (i in 1:length(files)) {
  cat(i, ' ')
  r <- stack(files[i])
  r <- raster::aggregate(r, fact=5, fun = mean)
  outfile <- paste0(outputDir,"/lower_res/", strsplit(files[i], "/")[[1]][5])
  writeRaster(r, filename = outfile, format = 'GTiff', options = tifOptions, overwrite = TRUE)
}

# Present-day Envirem
dir <- './Data/CHELSA/europe/lower_res'
dir.fig <- 'Figs/02_envirem/europe_1979.2013/lower_res/'
files <- list.files(dir, pattern = ".tif$", full.names = TRUE)
rasterTemplate <- raster(files[1])
refyear <- 1996
ETsolradRasters(rasterTemplate = rasterTemplate, year = refyear-1950, outputDir = dir, overwrite = TRUE)

assignNames(tmax = "CHELSA_tmax10_##_1979.2013_V1.2_land.europe",
            tmin = "CHELSA_tmin10_##_1979.2013_V1.2_land.europe",
            tmean = "CHELSA_temp10_##_1979.2013_V1.2_land.europe",
            precip = "CHELSA_prec_##_V1.2_land.europe",
            solrad = "et_solrad_##")
varnames()
verifyFileStructure(dir, returnFileNames = FALSE)
outdir <- paste0(dir,"envirem")
dir.create(outdir)


# chelsaFiles <- list.files(dir, pattern = 'CHELSA', full.names = TRUE)
# chelsaStack <- stack(chelsaFiles)
# solarFiles <- list.files(dir, pattern = 'sol', full.names = TRUE)
# solarStack <- stack(solarFiles)
# verifyRasterNames(chelsaStack, solradstack = solarStack)


## GENERATE ENVIREM variables
generateRasters(var = 'all',
                maindir = dir,
                outputDir = outdir)

enviremRasters <- list.files(outdir, pattern = '\\.tif$', full.names = TRUE)
enviremRasters <- stack(enviremRasters)

## PLOT ENVIREM maps
dir.create(dir.fig)
for (l in 1:nlayers(enviremRasters)) {
  png(paste0(dir.fig, "/", names(enviremRasters)[l],'_',strsplit(dir.fig,"/")[[1]][3],".png"),
      pointsize = 4, res = 300, width = 1000, height = 1000)
  plot(enviremRasters[[l]], col = inferno(100), box = FALSE, axes = FALSE)
  title(main = names(enviremRasters)[l])
  dev.off()
}


## Extra code --------------------------------------------------


# Converting precipitation units from mm/m2/s to mm/m2/month
fold <- list.files(inputDir, full.names = TRUE)
for (k in 1:length(fold)){
  cat(k, ' ')
  files <- list.files(fold[k], full.names=TRUE)
  for (i in 1:length(files)){
    cat(i, ' ')
    prec <- list.files(files[i], pattern = "pr", full.names=TRUE)
    for (m in 1:length(prec)){
      cat(m, ' ')
      r <- raster(prec[m])
      month <- strsplit(prec[m], '_')[[1]][8]
      if (month %in% c('01','03','05','07','08','10','12')) days <- 31
      if (month %in% c('04','06','09','11')) days <- 30
      if (month =='02'){
        leap <- strsplit(prec[m], '_')[[1]][7]
        if (as.numeric(leap) %% 4 == 0 ) days <- 29
        else days <- 28
      }
      r <- r*3600*24*days
      writeRaster(r, filename = prec[m], format = 'GTiff', options = tifOptions, overwrite = TRUE)
    }
  }
}


# correcting temperature sweden
fold <- list.files('./Data/CHELSA/sweden')
ptm <- Sys.time()
for (m in 1:length(fold)){
  p <- paste0("./Data/CHELSA/sweden/", fold[m])
  files <- list.files(p, pattern = 'tas.*.tif$', full.names = TRUE)
  for (k in 1:length(files)){
    r <- stack(files[k])
    r <- r*10-273.15
    print(paste0(m,"/7 ; ",k, "/2"))
    writeRaster(r, filename = files[k], format = 'GTiff', options = tifOptions, overwrite = TRUE)
  }
}
Sys.time()-ptm


# correcting temperature sweden montly files
fold <- list.files('./Data/CHELSA/sweden')
ptm <- Sys.time()
for (m in 1:length(fold)){
  year <- as.numeric(strsplit(fold[m],"_")[[1]][1]):as.numeric(strsplit(fold[m],"_")[[1]][2])
  p <- paste0("./Data/CHELSA/sweden/", fold[m])
  files <- list.files(p, full.names = TRUE)
  for (i in 1:length(year)){
    p <- paste0("./Data/CHELSA/sweden/", fold[m])
    temp <- list.files(files[i], pattern = 'tas.*.tif$', full.names = TRUE)
    for (k in 1:length(temp)){
      r <- raster(temp[k])
      r <- r*10-273.15
      writeRaster(r, filename = temp[k], format = 'GTiff', options = tifOptions, overwrite = TRUE)
    }
  print(paste0(m,"/7 ; ",i, "/",length(year)))
  }
}
Sys.time()-ptm


# Changing raster filenames in bulk
f <- list.files('./Data/CHELSA/sweden future/2041.2060_CMCC.CM_rcp85/', full.names = TRUE) ; f
for (i in 1:length(f)){
  r <- raster(f[i])
  outfile <- gsub('-','.',f[i])
  writeRaster(r, filename = outfile, format = 'GTiff', options = tifOptions, overwrite = TRUE)
}



# Create annual folders with files properly named and annual envirem variables
ptm <- Sys.time()
fold <- list.files('./Data/CHELSA/sweden/')
for (m in 1:length(fold)){

  # year <- as.numeric(strsplit(fold[m],"_")[[1]][1]):as.numeric(strsplit(fold[m],"_")[[1]][2])
  p <- paste0("./Data/CHELSA/sweden/", fold[m])
  files <- list.files(p,
                      # pattern = '.tif$',
                      full.names = TRUE)

  for (i in 1:length(year)){

    # path <- paste0(p,"/",year[i])
    # dir.create(path)
    # c <- 0
    #
    # for (k in c("pr","tasmax","tasmin")){
    #   c <- c+1
    #   K <- k
    #   K <- stack(files[c])
    #   temp <- K[[(12*(i-1)+1):(12*i)]]
    #   for(j in 1:12){
    #     # Not run
    #     if (k == "pr"){
    #       month <- as.character(j)
    #       if (month %in% c('1','3','5','7','8','10','12')) days <- 31
    #       if (month %in% c('4','6','9','11')) days <- 30
    #       if (month =='2'){
    #         if (year[i] %% 4 == 0) days <- 29
    #         else days <- 28
    #       }
    #       temp[[j]] <- temp[[j]]*3600*24*days
    #     }
    #
    #     type <- grep(pattern = "historical|rcp",strsplit(names(temp)[1],"_")[[1]], value=TRUE)
    #     outfile <- paste0(path,"/CHELSAcmip5ts_",k,"_CMCC.CM_",type,"_",year[i],"_",j,"_",year[1],".",year[length(year)],"_V1.1.sweden.tif")
    #     outfile <- gsub('_(\\d)_', '_0\\1_', outfile)
    #     writeRaster(temp[[j]], filename = outfile, format = 'GTiff', options = tifOptions, overwrite = TRUE)
    #   }
    # }
    type <- grep(pattern = "historical|rcp",strsplit(fold[m],"_")[[1]], value=TRUE)
    # outfile <- paste0(path,"/CHELSAcmip5ts_",k,"_CMCC.CM_",type,"_",year[i],"_",j,"_",year[1],".",year[length(year)],"_V1.1.sweden.tif")
    # outfile <- gsub('_(\\d)_', '_0\\1_', outfile)
    assignNames(tmax = paste0("CHELSAcmip5ts_tasmax_CMCC.CM_",type,"_",year[i],"_##_",year[1],".",year[length(year)],"_V1.1.sweden"),
                tmin = paste0("CHELSAcmip5ts_tasmin_CMCC.CM_",type,"_",year[i],"_##_",year[1],".",year[length(year)],"_V1.1.sweden"),
                precip = paste0("CHELSAcmip5ts_pr_CMCC.CM_",type,"_",year[i],"_##_",year[1],".",year[length(year)],"_V1.1.sweden"),
                solrad = paste0("solar_",year[i],"_##_sweden"))

    # rasterTemplate <- raster(outfile)
    # ETsolradRasters(rasterTemplate = rasterTemplate, year = year[i]-1950, outputDir = path, overwrite = TRUE)
    varnames()
    verifyFileStructure(paste0("./Data/CHELSA/sweden/",fold[m],"/",year[i]), returnFileNames = FALSE)

    # chelsaFiles <- list.files(paste0("./Data/CHELSA/sweden/",fold[m],"/",year[i]), pattern = 'CHELSA', full.names = TRUE)
    # chelsaStack <- stack(chelsaFiles)
    # solarFiles <- list.files(paste0("./Data/CHELSA/sweden/",fold[m],"/",year[i]), pattern = 'solar', full.names = TRUE)
    # solarStack <- stack(solarFiles)
    # verifyRasterNames(chelsaStack, solradstack = solarStack)

    outdir <- paste0("./Data/CHELSA/sweden/",fold[m],"/",year[i],"/envirem")
    # dir.create(outdir)

    generateRasters(var = 'all',
                    maindir = paste0("./Data/CHELSA/sweden/",fold[m],"/",year[i]),
                    outputDir = outdir)

    enviremRasters <- list.files(outdir, pattern = '\\.tif$', full.names = TRUE)
    enviremRasters <- stack(enviremRasters)

    # par(mfrow = c(4, 4), mar = c(0.5, 0.5, 2, 0))
    # dir.create(paste0("Figs/02_envirem/",year[i],"_",type))
    for (l in 1:nlayers(enviremRasters)) {
      png(paste0("Figs/02_envirem/",year[i],"_",type,"/",names(enviremRasters)[l],"_",year[i],"_",type,".png"),
          pointsize = 4, res = 300, width = 1000, height = 1000)
      plot(enviremRasters[[l]], col = inferno(100), box = FALSE, axes = FALSE)
      title(main = names(enviremRasters)[l])
      dev.off()
    }
  }
}

Sys.time()-ptm


# ---------- Topographic data ----------

setwd("C:/Users/Mathieu/Documents/Mathieu/ENS/04_4A-PLR/Uppsala")
# CRS conversion to WGS84 was made on QGIS
tifOptions <- c("COMPRESS=DEFLATE", "PREDICTOR=2", "ZLEVEL=6")
tiles <- list.files("Data/CHELSA/topo/", recursive = T, pattern="wgs84.TIF$", full.names = T)
sweden.spdf <- readRDS("Data/z_Map/gadm36_SWE_0_sp.rds")

r <- lapply(tiles, function(x){raster(x)})
temp <- raster("Data/CHELSA/sweden/1979.2013/envirem/annualPET.tif")
res <- res(temp)
for(i in 1:length(r)){
  r[[i]] <- crop(r[[i]],sweden.spdf) ; print(paste0("Cropping done: ",i))
  if(i==2){
    t <- splitRaster(r[[i]],nx=1,ny=5)
    for(j in 1:length(t)) {t[[j]][t[[j]]<0] <- NA ; print(j)}
    r[[i]] <- mergeRaster(lapply(t, function(x){aggregate(x,fact=res/res(x))}))
  } else{r[[i]][r[[i]]<0] <- NA}
  r[[i]] <- aggregate(r[[i]],fact=res/res(r[[i]])) ; print(paste0("Aggregation done: ",i))
}

writeRaster(r[[1]], filename = "Data/z_Map/r1.raster", format = 'GTiff', options = tifOptions, overwrite = TRUE)
writeRaster(r[[2]], filename = "Data/z_Map/r2.raster", format = 'GTiff', options = tifOptions, overwrite = TRUE)
writeRaster(r[[3]], filename = "Data/z_Map/r3.raster", format = 'GTiff', options = tifOptions, overwrite = TRUE)

template1<- projectRaster(from = r[[2]], to= r[[1]], alignOnly=TRUE)
r1_aligned<- projectRaster(from = r[[1]], to= template1)
r_merged1<- merge(r1_aligned,r[[2]])
template2<- projectRaster(from = r[[2]], to= r[[3]], alignOnly=TRUE)
r3_aligned<- projectRaster(from = r[[3]], to= template2)
r_merged<- merge(r_merged1,r3_aligned)
plot(r_merged)

r2 <- mask(r_merged, sweden.spdf)
elev <- resample(r2,temp)

writeRaster(elev, filename = "Data/z_Map/Elevation.raster", format = 'GTiff', options = tifOptions, overwrite = TRUE)
