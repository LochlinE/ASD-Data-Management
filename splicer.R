install.packages("prospectr")
suppressMessages(library(tidyverse))
suppressMessages(library(prospectr))

filesLocation <- "C:\\Users\\lochl\\OneDrive\\Desktop\\Sawfly Lab\\ASD\\2022\\Plant\\PA\\DC1.5.1.22\\Processed\\"
fileNames <- list.files(filesLocation)


readRaw <- function(name, location) {
  x = read.delim(paste0(location, name))
  names(x)[2] <- "Reflectance"
  x <- mutate(x, fileName = name)
  return(x)
}

testFiles <- lapply(fileNames, readRaw, location = filesLocation)
names(testFiles) <- c(1:length(testFiles))

splicer <- function(x){
  x <- mutate(x, splicedReflectance = spliceCorrection(X = x$Reflectance,
                                            wav = x$Wavelength,
                                            splice = c(965,1775),
                                            interpol.bands = 10))
}

testDataSpliced <- lapply(testFiles,splicer)

testDataSpliced %>%
  bind_rows %>%
  group_by(fileName) %>%
  filter(fileName == "t0PAC100001.asd.txt") %>%
  ggplot(mapping = aes(x = Wavelength,y = Reflectance, group = fileName, color = fileName)) + 
  geom_line(mapping = aes(x = Wavelength, y = splicedReflectance, color = "Spliced Reflectance")) +
  geom_line(size = 1) + xlim(350,2500) + ylim(0.0,1) 




























