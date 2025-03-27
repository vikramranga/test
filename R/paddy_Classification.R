#
##
### paddy classification using VH
##
#

#libraries
library(pacman)
p_load(terra, sf, tidyverse)


#<----------------- Year 2018 ------------------------------------------------->
paddy_ndvif <- list.files("//172.28.13.23/BSB_Rabi/Amnex RRSC Data/00_Raw Data/3_VH Data/dB/2018/FNWise",
                          pattern = "*.tif$",
                          full.names = TRUE)
# 
must_ndvi <- paddy_ndvif |>
  set_names(basename(paddy_ndvif)) |>
  lapply(function(x) {rast(x)})

must_ndvi <- must_ndvi[13:24]
a <- must_ndvi[[1]]

#<---- The code below to make extent of all raster same ----->
must_ndvi <- lapply(must_ndvi, function(x)
{
  if(ext(x) != ext(a)) {
    x <- resample(x, a)
    return(x)
  } else
  {x}
}
)

must_ndvi <- rast(must_ndvi)

#<-------------- ESA Crop Mask ------------------------------------------------>
cpmskBank<-rast("//172.28.13.23/BSB_Rabi/Amnex RRSC Data/00_Raw Data/1_Agrimask_esa/WB_Agrimask_2020_21_them.img")

#<----- Classification Rules -------------------------------------------------->
# re0me them
names(must_ndvi) <- c("J1", "J2", "A1", "A2", "S1", "S2",
                      "O1", "O2", "N1", "N2", "D1", "D2")
must_ndvi <- must_ndvi/1000

#<---------Calculate Minimum at pixel level ----------------------------------->
# bankMin <- which.min(must_ndvi[[1:6]])
bankImg <- must_ndvi

#Apply crop mask
cpmskBank <- ifel(cpmskBank == 1,1, NA)
cpmskBank <- resample(cpmskBank, bankImg)
bankImg1 <- mask(bankImg, cpmskBank)
bankImg <- bankImg1
bankMin <- which.min(bankImg[[1:6]])
################################################################################
#<------------------ Create rules -------------------------------------------->#
################################################################################
#
##
### July 1 FN
##
#


clsfd1_j1 <- ifel(bankMin == 1 & 
                    bankImg[['J2']] < bankImg[['A1']],
                  1, 0)
clsfd2_j1 <- ifel(bankMin == 1 & 
                    bankImg[['A1']] < bankImg[['A2']],
                  1, 0)
clsfd3_j1 <- ifel(bankMin == 1 &
                    bankImg[['A2']] < bankImg[['S1']],
                  1, 0)
clsfd4_j1 <- ifel(bankMin == 1 &
                    bankImg[['J1']] < bankImg[['J2']],
                  1, 0)
clsfd_j1 <- clsfd1_j1 + clsfd2_j1 + clsfd3_j1 + clsfd4_j1
clsfdF_j1 <- ifel(clsfd_j1 >= 3, 1, NA)


R0 <- clsfdF_j1

R0|> 
  writeRaster("//172.28.13.23/BSB_Rabi/WB_kharif_paddy/Final_kharif_paddy/paddy_acreage_5yrs/2018/july1_2018.tif",
              overwrite = TRUE)



#
##
### July 2 FN
##
#

clsfd1 <- ifel(bankMin == 2 & 
                 bankImg[['J2']] < bankImg[['A1']],
               1, 0)
clsfd2 <- ifel(bankMin == 2 & 
                 bankImg[['A1']] < bankImg[['A2']],
               1, 0)
clsfd3 <- ifel(bankMin == 2 &
                 bankImg[['A2']] < bankImg[['S1']],
               1, 0)
clsfd4 <- ifel(bankMin == 2 &  
                 bankImg[['S1']] < bankImg[['S2']],
               1, 0)
clsfd <- clsfd1 + clsfd2 + clsfd3 + clsfd4
clsfdF <- ifel(clsfd >= 3, 1, NA)

#### cumulate july 1 and 2 FN

R1 <- sum(R0,clsfdF,na.rm=TRUE)
plot(R1,main='R1')

R1|> 
  writeRaster("//172.28.13.23/BSB_Rabi/WB_kharif_paddy/Final_kharif_paddy/paddy_acreage_5yrs/2018/july12_2018.tif",
              overwrite = TRUE)

#
##
### Aug 1 FN
##
#

clsfd2_A1 <- ifel(bankMin == 3 & 
                    bankImg[['A1']] < bankImg[['A2']],
                  1, 0)
clsfd3_A1 <- ifel(bankMin == 3 &
                    bankImg[['A2']] < bankImg[['S1']],
                  1, 0)
clsfd4_A1 <- ifel(bankMin == 3 &
                    bankImg[['S1']] < bankImg[['S2']],
                  1, 0)
clsfd1_A1 <- ifel(bankMin == 3 & 
                    bankImg[['S2']] < bankImg[['O1']],
                  1, 0)
clsfd_A1 <- clsfd1_A1 + clsfd2_A1 + clsfd3_A1 + clsfd4_A1
clsfdF_A1 <- ifel(clsfd_A1 >= 3, 1, NA)


R2 <- sum(R1,clsfdF_A1,na.rm=TRUE)
plot(R2,main='R2')

R2|> 
  writeRaster("//172.28.13.23/BSB_Rabi/WB_kharif_paddy/Final_kharif_paddy/paddy_acreage_5yrs/2018/july12_aug1_2018.tif",
              overwrite = TRUE)


#
##
### Aug 2 FN
##
#

clsfd1_A2 <- ifel(bankMin == 4 & 
                    bankImg[['S2']] < bankImg[['O1']],
                  1, 0)

clsfd2_A2 <- ifel(bankMin == 4 & 
                    bankImg[['O1']] < bankImg[['O2']],
                  1, 0)
clsfd3_A2 <- ifel(bankMin == 4 &
                    bankImg[['A2']] < bankImg[['S1']],
                  1, 0)
clsfd4_A2 <- ifel(bankMin == 4 &
                    bankImg[['S1']] < bankImg[['S2']],
                  1, 0)

clsfd_A2 <- clsfd1_A2 + clsfd2_A2 + clsfd3_A2 + clsfd4_A2
clsfdF_A2 <- ifel(clsfd_A2 >= 3, 1, NA)

R3 <- sum(R2,clsfdF_A2,na.rm=TRUE)
plot(R3,main='R3')

R3|> 
  writeRaster("//172.28.13.23/BSB_Rabi/WB_kharif_paddy/Final_kharif_paddy/paddy_acreage_5yrs/2018/july12_aug12_2018.tif",
              overwrite = TRUE)
#
##
### Sept 1 FN
##
#
clsfd1_S1 <- ifel(bankMin == 5 & 
                    bankImg[['S2']] < bankImg[['O1']],
                  1, 0)

clsfd2_S1 <- ifel(bankMin == 5 & 
                    bankImg[['O1']] < bankImg[['O2']],
                  1, 0)
clsfd3_S1 <- ifel(bankMin == 5 &
                    bankImg[['O2']] < bankImg[['N1']],
                  1, 0)
clsfd4_S1 <- ifel(bankMin == 5 &
                    bankImg[['S1']] < bankImg[['S2']],
                  1, 0)

clsfd_S1 <- clsfd1_S1 + clsfd2_S1 + clsfd3_S1 + clsfd4_S1
clsfdF_S1 <- ifel(clsfd_S1 >= 3 , 1, NA)



R4 <- sum(R3,clsfdF_S1,na.rm=TRUE)
plot(R4,main='R4')

R4|> 
  writeRaster("//172.28.13.23/BSB_Rabi/WB_kharif_paddy/Final_kharif_paddy/paddy_acreage_5yrs/2018/july12_aug12_sept1_2018.tif",
              overwrite = TRUE)






#clsfdFF <- merge(clsfdF_j1,clsfdF, clsfdF_A1, clsfdF_A2, clsfdF_S1)

#------------------------------- agri mask ------------------------------------------------#

#clsfdFF|> 
# writeRaster("D:/Amnex/Kharif/R_result/PADDY_2018_23/New folder/WBpaddy_Final_2018.tif",
#            overwrite = TRUE)

rm(list=ls())


