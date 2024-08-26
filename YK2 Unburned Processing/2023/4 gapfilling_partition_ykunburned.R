rm(list = ls())
library(REddyProc)
setwd('C:/Users/karndt.WHRC/Desktop/sites/YKD/Ameriflux YK/unburned/') #use this to set your directory

#help("REddyProc-package")

  #+++ Load data with one header and one unit row from (tab-delimited) text file
    EddyData.F <- fLoadTXTIntoDataframe('./reddy_ykdub.txt')
  # note: use \code{fFilterAttr} to subset rows while keeping the units attributes
  
    #subset out problematic repeat rows
#    EddyData.F = EddyData.F[-35088,]
    
#+++ If not provided, calculate VPD from Tair and rH
  EddyData.F <- cbind(EddyData.F,VPD=fCalcVPDfromRHandTair(EddyData.F$RH, EddyData.F$Tair))
  
#+++ Add time stamp in POSIX time format
  EddyDataWithPosix.F <- fConvertTimeToPosix(EddyData.F, 'YDH', Year='Year', Day='DoY', Hour='Hour')

  
  #+++ Initialize R5 reference class sEddyProc for processing of eddy data
#+++ with all variables needed for processing later
  EddyProc.C <- sEddyProc$new('YKD Unburned', EddyDataWithPosix.F, c('NEE','CH4','H','LE','Ustar','Rg','Tair','Tsoil','RH','VPD'))
  EddyProc.C$sSetLocationInfo(LatDeg =  61.2548, LongDeg = -163.2590, TimeZoneHour = -9)
  
#+++ Generate plots of all data in directory \plots (of current R working dir)
#  EddyProc.C$sPlotDiurnalCycle('CH4')
#  EddyProc.C$sPlotDiurnalCycle('NEE')
#  EddyProc.C$sPlotDiurnalCycle('Rg')
#  EddyProc.C$sPlotDiurnalCycle('Tair')

  #+++ Plot individual months/years to screen (of current R graphics device)
  #EddyProc.C$sPlotHHFluxesY('NEE',Year = 2015)
  #EddyProc.C$sPlotFingerprintY('NEE', Year=2014)
  
#+++ Fill gaps in variables with MDS gap filling algorithm (without prior ustar filtering)
  EddyProc.C$sMDSGapFill('Rg',    FillAll=F) #Fill only the gaps 'Rg'
  EddyProc.C$sMDSGapFill('Tair',  FillAll=F) #Fill only the gaps 'Tair'
  EddyProc.C$sMDSGapFill('VPD',   FillAll=F) #Fill only the gaps 'VPD'
  EddyProc.C$sMDSGapFill('Tsoil', FillAll=F) #Fill only the gaps Tsoil'
  EddyProc.C$sMDSGapFill('RH',    FillAll=F) #Fill only the gaps for RH'
  EddyProc.C$sMDSGapFill('Ustar', FillAll=F) #Fill only the gaps Ustar'
  EddyProc.C$sMDSGapFill('H',     FillAll=F) #Fill only the gaps for H'
  EddyProc.C$sMDSGapFill('LE',    FillAll=F) #Fill only the gaps for 'LE'
  EddyProc.C$sMDSGapFill('NEE',   FillAll=T) #Fill all values to estimate flux uncertainties
  EddyProc.C$sMDSGapFill('CH4',   FillAll=T)  #Fill all values to estimate flux uncertainties
  
  #+++ Example plots of filled data to screen or to directory \plots
#  EddyProc.C$sPlotFingerprintY('NEE_f',Year.i = 2015)
 # EddyProc.C$sPlotDailySumsY('NEE_f','NEE_fsd', Year=2013) #Plot of sums with uncertainties
  #EddyProc.C$sPlotDailySums('NEE_f','NEE_fsd')

    #+++ Partition NEE into GPP and respiration
  EddyProc.C$sMRFluxPartition()	# night time partitioning -> Reco, GPP
  EddyProc.C$sGLFluxPartition()	# day time partitioning -> Reco_DT, GPP_DT Lasslop,
  #EddyProc.C$sGLFluxPartition(controlGLPart.l=partGLControl(isBoundLowerNEEUncertainty=FALSE))	# day time partitioning -> Reco_DT, GPP_DT
  
  #check out data
  # plot(EddyProc.C$sTEMP$sDateTime ,EddyProc.C$sTEMP$Rg_f,col='red')
  # points(EddyProc.C$sTEMP$sDateTime ,EddyProc.C$sTEMP$Rg_orig)
  # 
  # plot(EddyProc.C$sTEMP$sDateTime ,EddyProc.C$sTEMP$Tair_f,col='red')
  # points(EddyProc.C$sTEMP$sDateTime ,EddyProc.C$sTEMP$Tair_orig)
  # 
  # plot(EddyProc.C$sTEMP$sDateTime ,EddyProc.C$sTEMP$VPD_f,col='red')
  # points(EddyProc.C$sTEMP$sDateTime ,EddyProc.C$sTEMP$VPD_orig)
  # 
  # plot(EddyProc.C$sTEMP$sDateTime ,EddyProc.C$sTEMP$Tsoil_f,col='red')
  # points(EddyProc.C$sTEMP$sDateTime ,EddyProc.C$sTEMP$Tsoil_orig)
  # 
  # plot(EddyProc.C$sTEMP$sDateTime ,EddyProc.C$sTEMP$LE_f,col='red')
  # points(EddyProc.C$sTEMP$sDateTime ,EddyProc.C$sTEMP$LE_orig)
  # 
  # plot(EddyProc.C$sTEMP$sDateTime ,EddyProc.C$sTEMP$H_f,col='red')
  # points(EddyProc.C$sTEMP$sDateTime ,EddyProc.C$sTEMP$H_orig)
  
  # plot(EddyProc.C$sTEMP$sDateTime ,EddyProc.C$sTEMP$NEE_f,col='red')
  # points(EddyProc.C$sTEMP$sDateTime ,EddyProc.C$sTEMP$NEE_orig)
  # abline(h = 0,col='green')
  # 
  # plot(EddyProc.C$sTEMP$sDateTime ,EddyProc.C$sTEMP$CH4_f,col='red')
  # points(EddyProc.C$sTEMP$sDateTime ,EddyProc.C$sTEMP$CH4_orig)  
  # abline(h = 0,col='green')
  # 

  plot(EddyProc.C$sTEMP$sDateTime ,EddyProc.C$sTEMP$GPP_f,col='green')
  points(EddyProc.C$sTEMP$sDateTime ,EddyProc.C$sTEMP$Reco)
  
  plot(EddyProc.C$sTEMP$sDateTime ,EddyProc.C$sTEMP$GPP_DT,col='green')
  points(EddyProc.C$sTEMP$sDateTime ,EddyProc.C$sTEMP$Reco_DT)
  
  #test quality of partitioned data
  par(mar=c(5,5,5,5))
  plot( EddyProc.C$sTEMP$GPP_DT ~ EddyProc.C$sTEMP$GPP_f); abline(0,1,col='red')
  plot( EddyProc.C$sTEMP$Reco_DT ~ EddyProc.C$sTEMP$Reco); abline(0,1,col='red')
  plot(-EddyProc.C$sTEMP$GPP_DT + EddyProc.C$sTEMP$Reco_DT ~ EddyProc.C$sTEMP$NEE_f ); abline(0,1,col='red')
  
  # #test results of gapfilling
  # plot(EddyProc.C$sTEMP$NEE_orig,EddyProc.C$sTEMP$NEE_fall);abline(0,1,col='red')
  # summary(lm(EddyProc.C$sTEMP$NEE_fall ~ EddyProc.C$sTEMP$NEE_orig))
  # 
  # plot(EddyProc.C$sTEMP$CH4_orig,EddyProc.C$sTEMP$CH4_fall);abline(0,1,col='red')
  # summary(lm(EddyProc.C$sTEMP$CH4_fall ~ EddyProc.C$sTEMP$CH4_orig))
  
  
  #names(EddyProc.C$sTEMP)
  # there are some constraints, that might be too strict for some datasets
  # e.g. in the tropics the required temperature range might be too large.
  # Its possible to change these constraints
  #EddyProc.C$sMRFluxPartition(parsE0Regression=list(TempRange.n=2.0, optimAlgorithm="LM")	)  
  
  #+++ Example plots of calculated GPP and respiration 
#  EddyProc.C$sPlotFingerprintY('GPP_f', Year=2016)
 # EddyProc.C$sPlotHHFluxes('GPP_f')
  #EddyProc.C$sPlotHHFluxesY('GPP_f', Year=2017)
#  EddyProc.C$sPlotHHFluxes('Reco')
  
  
  #+++ Export gap filled and partitioned data to standard data frame
  FilledEddyData.F <- EddyProc.C$sExportResults()
  #+++ Save results into (tab-delimited) text file in directory \out
  CombinedData.F <- cbind(EddyData.F, FilledEddyData.F)
  #+++ May rename variables to correspond to Ameriflux 
  colnames(CombinedDataAmeriflux.F <- renameVariablesInDataframe(CombinedData.F, getBGC05ToAmerifluxVariableNameMapping() ))
  CombinedDataAmeriflux.F$TIMESTAMP_END <- POSIXctToBerkeleyJulianDate( EddyProc.C$sExportData()[[1]] )
  head(tmp <- BerkeleyJulianDateToPOSIXct( CombinedDataAmeriflux.F$TIMESTAMP_END ))
  #colnames(tmp <- renameVariablesInDataframe(CombinedData.F, getAmerifluxToBGC05VariableNameMapping() ))
  fWriteDataframeToFile(CombinedData.F, 'ykdub-Results_data.txt', 'data')
  