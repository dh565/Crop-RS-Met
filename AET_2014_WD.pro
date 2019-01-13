;=========================================================================================!
PRO AET_2014_WD, n_plots, no_obs, SWC_f, t_ET, swc, err_max, err_min, cum_wat $
          , cum_PET, cum_ET, sm_pot, fVC
;-----------------------------------------------------------------------------------------!
; Main program.
; The RS-Met model estimates daily ET based on NDVI and meteorological data.
; 
; Explanation in Helman et al. 2017 ('Daily estimations of evapotranspiration and 
; CO2 uptake in high-energy water-limited environments using vegetation index and 
; meteorological data') - hereafter Ref_Helman17.
; 
; Call procedures: 'Calculate_kc_ks.pro'
;
; David Helman
; 27th December, 2017.
;-----------------------------------------------------------------------------------------!
;
;-----------------------------------------------------------------------------------------!
; INPUT PARAMETERS (this may be changed):  @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;-----------------------------------------------------------------------------------------!
;-----------------------------------------------------------------------------------------!
; no. of days for the cumulated period (see Ref_Helman17).
;-----------------------------------------------------------------------------------------!
  t_accum = 15  ; (Maselli et al. 2014 used a one month period for grasslands).  
;-----------------------------------------------------------------------------------------!
; Crop and soil coefficients.
;-----------------------------------------------------------------------------------------!
  kc_max = 1.2  ; maximum potential kc (following FAO56 see also Ref_Helman17).           
  ks_max = 0.2  ; maximum potential ks (Maselli etal 2014 used ks = 0.2 for grasses).    
;-----------------------------------------------------------------------------------------!
; Effective water supply (in the root zone after reducing water loss through percolation).  
;-----------------------------------------------------------------------------------------!
  eff_wat   = 0.93; 0.95      (0.0 - 1.0 mean 0% to 100% effective water).
;-----------------------------------------------------------------------------------------!
; Maximum rate of interception loss (as %age from rain and as a function of LAI).  
;-----------------------------------------------------------------------------------------!
  inter_max = 0.25   ; (0.25 means 25% interception loss at a full canopy coverage).
;-----------------------------------------------------------------------------------------!
; For display... p_test is the plot #
;-----------------------------------------------------------------------------------------!
  p_test = 1; n_plots-1 ['Wet1','Dry1','Wet2','Dry2','Wet3']
;-----------------------------------------------------------------------------------------!
; end INPUT.   @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;-----------------------------------------------------------------------------------------!
;-----------------------------------------------------------------------------------------!
;
;-----------------------------------------------------------------------------------------!
; Open directory and csv file.
;-----------------------------------------------------------------------------------------!
  @directory_RS_Met
;-----------------------------------------------------------------------------------------!
; Model procedure:
;-----------------------------------------------------------------------------------------!
; Get year and directory path from @directory_RS_Met.
;-----------------------------------------------------------------------------------------!
  file        = 'Meteo_'+yr+'*.csv'
  dir         = strcompress(dir_parent + file)
;-----------------------------------------------------------------------------------------!
; Get data from csv and other general information.
;-----------------------------------------------------------------------------------------!
  T_header    = 3
  Result      = read_csv(dir, N_TABLE_HEADER=T_header)   ; read data into Result_NDVI.
  N           = n_elements(Result.FIELD01)          ; no. of rows in file.
;-----------------------------------------------------------------------------------------!
; Read data into arrays (see fields in 'Calculate_kc_ks_dry.pro').   
;-----------------------------------------------------------------------------------------!
  Gdate_met   = Result.field02      ; date.
  precip      = Result.field04      ; precipitation only (in mm d-1).
  WS          = Result.field12      ; water supply (precip + irrigation).
  PET_PM      = Result.field16      ; Penmman-Monteith PET.
  PET_buck    = Result.field17      ; PET estimated using a bucket.
;-----------------------------------------------------------------------------------------!
; Calculate PET, fws and fwa (use Calculate_kc_ks_dry.pro).
;-----------------------------------------------------------------------------------------!
  Calculate_kc_ks,Gdate_met,WS,PET_PM,PET_buck,t_accum,ks_max,kc_max,PET_JH_d,kc_JH_d $
    ,kc_PM_d,kc_buck_d,ks_JH_d,ks_PM_d,ks_buck_d,n_plots,fVC
;-----------------------------------------------------------------------------------------!
; Calculate no. of observations (days).
;-----------------------------------------------------------------------------------------!
  no_obs = n_elements(PET_JH_d) 
;-----------------------------------------------------------------------------------------!
; Calculate AET using Eq. (12) from Ref_Helman17.
;-----------------------------------------------------------------------------------------!
  AET_PM     = findgen(n_plots,no_obs) * 0.0
  AET_JH     = findgen(n_plots,no_obs) * 0.0
  AET_buck   = findgen(n_plots,no_obs) * 0.0
;-----------------------------------------------------------------------------------------!
; Arrays for daily ET (from all methods), water supply and soil water content.
;-----------------------------------------------------------------------------------------!
  wsupp      = findgen(n_plots,no_obs) * 0.0
  AET_t      = findgen(n_plots,no_obs) * 0.0
  AET_max    = findgen(n_plots,no_obs) * 0.0
  AET_min    = findgen(n_plots,no_obs) * 0.0
  swc        = findgen(n_plots,no_obs) * 0.0  
  swc_max    = findgen(n_plots,no_obs) * 0.0 
  swc_min    = findgen(n_plots,no_obs) * 0.0 
  err_max    = findgen(n_plots,no_obs) * 0.0
  err_min    = findgen(n_plots,no_obs) * 0.0
  cum_wat    = findgen(n_plots,no_obs) * 0.0
  cum_ET     = findgen(n_plots,no_obs) * 0.0
  sm_pot     = findgen(n_plots,no_obs) * 0.0
  cum_PET    = findgen(no_obs) * 0.0
;-----------------------------------------------------------------------------------------!
; Arrays for total annual ET (from all methods), water supply and soil water content.
;-----------------------------------------------------------------------------------------!
  t_ET       = findgen(n_plots) * 0.0  ; for total annual ET (from mean over 3 methods).
  t_water    = findgen(n_plots) * 0.0  ; for total annual water supply.
  SWC_f      = findgen(n_plots) * 0.0  ; for expected final soil moisture content.
;-----------------------------------------------------------------------------------------!
  
  ;---------------------------------------------------------------------------------------!
  ;  Loop over plots and days.
  ;---------------------------------------------------------------------------------------!
  ;
    ;--------------------------------------------------------------------------------!
      for p = 0, n_plots-1 do begin
        ;-----------------------------------------------------------------------!
        ; Initial quantities for diagnostics.
        ;-----------------------------------------------------------------------!
        cum_wat_i = 0.0                  ; for water supplied (rain+irr).
        cum_ET_i  = 0.0                  ; for water evaporated.
        swc_i     = eff_wat * WS(0)      ; for swc (remained water in soil).
        swc_imax  = 0.0
        swc_imin  = 0.0
        ;-----------------------------------------------------------------------!
        ; Loop over plots.
        ;-----------------------------------------------------------------------!
          for t = 0, no_obs-1 do begin
            ;-------------------------------------------------------------------!
            ; Calculate actual ET with 3 different methods for PET (dry plots).
            ;-------------------------------------------------------------------!
            AET_PM(p,t)   = PET_PM(t)   * (kc_JH_d(p,t)   + ks_JH_d(p,t))
            AET_JH(p,t)   = PET_JH_d(t) * (kc_PM_d(p,t)   + ks_PM_d(p,t))
            AET_buck(p,t) = PET_buck(t) * (kc_buck_d(p,t) + ks_buck_d(p,t))
            wsupp(p,t)    = WS(t)
            ;-------------------------------------------------------------------!
            ; Averaged actual et from three methods (and max and min).
            ;-------------------------------------------------------------------!
            AET_t(p,t) = (AET_PM(p,t) + AET_JH(p,t) + AET_buck(p,t)) / 3.0
            AET_max(p,t)  = max([AET_PM(p,t), AET_JH(p,t), AET_buck(p,t)])
            AET_min(p,t)  = min([AET_PM(p,t), AET_JH(p,t), AET_buck(p,t)])
            ;-------------------------------------------------------------------!
            ; Add interception loss as a function of LAI (fVC used for now).
            ;-------------------------------------------------------------------!
            act_ET = AET_t(p,t)
            max_ET = AET_max(p,t)
            min_ET = AET_min(p,t)
            
            AET_t(p,t)   = act_ET + fVC(t) * precip(t) * inter_max
            AET_max(p,t) = max_ET + fVC(t) * precip(t) * inter_max
            AET_min(p,t) = min_ET + fVC(t) * precip(t) * inter_max
            ;-------------------------------------------------------------------!
            ; Choose model for ET:
            ;-------------------------------------------------------------------!
            AET_m = AET_t
            ;-------------------------------------------------------------------!
            ; Cumulated quantities (dry plots).
            ;-------------------------------------------------------------------!
            if (t lt no_obs-1) then begin
              ;---------------------------------------------------------------!
              ; soil water content, water supply and water evaporated.
              ;---------------------------------------------------------------!
              swc_i        = swc_i    + (eff_wat * wsupp(p,t) - AET_m(p,t))
              swc(p,t)     = swc_i
              ;---------------------------------------------------------------!
              swc_imax     = swc_imax + (eff_wat * wsupp(p,t) - AET_max(p,t))
              swc_max(p,t) = swc_imax
              ;---------------------------------------------------------------!
              swc_imin     = swc_imin + (eff_wat * wsupp(p,t) - AET_min(p,t))
              swc_min(p,t) = swc_imin
              ;---------------------------------------------------------------!
              err_max(p,t) = AET_max(p,t) - AET_m(p,t)
              err_min(p,t) = AET_m(p,t)   - AET_min(p,t)
              ;---------------------------------------------------------------!
              cum_wat_i    = cum_wat_i + wsupp(p,t)
              cum_wat(p,t) = cum_wat_i
              cum_ET_i     = cum_ET_i + AET_m(p,t)
              cum_ET(p,t)  = cum_ET_i
              ;----------------------------------------------------------!
            endif
          endfor
          ;--------------------------------------------------------------!
          ; Cumulated quantities - last day (wet plots).
          ;--------------------------------------------------------------!
          swc(p,no_obs-1)     = swc(p,no_obs-2) $
                                + (eff_wat * wsupp(p,no_obs-1) $
                                - AET_m(p,no_obs-1))
          ;--------------------------------------------------------------!
          swc_max(p,no_obs-1) = swc_max(p,no_obs-2) $
                                + (eff_wat * wsupp(p,no_obs-1) $
                                - AET_max(p,no_obs-1))
          ;--------------------------------------------------------------!
          swc_min(p,no_obs-1) = swc_min(p,no_obs-2) $
                                + (eff_wat * wsupp(p,no_obs-1) $
                                - AET_min(p,no_obs-1))          
          ;--------------------------------------------------------------!
          err_max(p,no_obs-1) = AET_max(p,no_obs-1) - AET_m(p,no_obs-1)
          err_min(p,no_obs-1) = AET_m(p,no_obs-1)   - AET_min(p,no_obs-1)
          ;--------------------------------------------------------------!
          cum_wat(p,no_obs-1) = cum_wat(p,no_obs-2) $
                                + (eff_wat * wsupp(p,no_obs-1))
          cum_ET(p,no_obs-1)  = cum_ET(p,no_obs-2) $
                                + (AET_m(p,no_obs-1)) 
          ;--------------------------------------------------------------!
          ; total quantities.
          ;--------------------------------------------------------------!
          t_ET(p)     = total(AET_m(p,*))
          t_water(p)  = total(eff_wat * wsupp(p,*))
          SWC_f(p)    = eff_wat * t_water(p) - t_ET(p)
          ;--------------------------------------------------------------!
          ;
        ;-----------------------------------------------------------------------!
        ;
        ;-----------------------------------------------------------------------!
        ; Evaporated water and remaining water in soil (assuming no runoff).
        ;-----------------------------------------------------------------------!
      endfor
  ;---------------------------------------------------------------------------------------!
  ; End loop over plots and days.
  ;---------------------------------------------------------------------------------------!
  ; check cumulated PET since date of first water supply.
  ;---------------------------------------------------------------------------------------!
      cum_PET_i = 0.0                            ; initial PET for diagnostics.
      ;-------------------------------------------------------------------------!
      ; Loop over days (since day 60 - first rain).
      ;-------------------------------------------------------------------------!
      PET = (PET_JH_d + PET_PM + PET_buck) / 3.0  ; choose PET method.
      ;-------------------------------------------------------------------------!
      for t = 20, no_obs-2 do begin
        cum_PET_i    = cum_PET_i + PET(t)
        cum_PET(t)   = cum_PET_i
      endfor
      ;-------------------------------------------------------!
      ; Last date.
      ;-------------------------------------------------------!
      cum_PET(no_obs-1) = cum_PET(no_obs-2) + PET(no_obs-1)
      ;-------------------------------------------------------!
      ; swc calculated as water supply minus PET.
      ;-------------------------------------------------------!
      for p = 0, n_plots-1 do begin
        for t = 0, no_obs-1 do begin
          sm_pot(p,t)     = cum_wat(p,t) - cum_PET(t)
        endfor
      endfor
      NoValid = where(sm_pot lt 0.0)
      sm_pot(NoValid) = 0.0
      ;-------------------------------------------------------------------------!
      ; end loop.
      ;-------------------------------------------------------------------------!
      
  ;---------------------------------------------------------------------------------------!
  ; Convert to m3/m3.
  ;---------------------------------------------------------------------------------------!
      swc      = swc / 1000.00   
      err_max  = err_max ;/ 1000.0  
      err_min  = err_min ;/ 1000.0 
      cum_wat  = cum_wat / 1000.00
      cum_PET  = cum_PET / 1000.00
      cum_ET   = cum_ET  / 1000.00
      sm_pot   = sm_pot  / 1000.00
  ;---------------------------------------------------------------------------------------!
  ; calculate mean total swc (from all methods and plots).
  ;---------------------------------------------------------------------------------------!
      SWC_mean = mean(SWC_f)
      print, '========================================='
      print, ' Mean SWC = ', SWC_mean, ' mm'
      print, '-----------------------------------------'
;---------------------------------------------------------------------------------------!
; save swc for plots as csv file.
;---------------------------------------------------------------------------------------!
      file_name = 'swc_'+yr+'.csv'
      plt1 = reform(swc(0,*))
      plt2 = reform(swc(1,*))
      plt3 = reform(swc(2,*))
      plt4 = reform(swc(3,*))
      plt5 = reform(swc(4,*))
;      plt6 = reform(swc(5,*))
;      plt7 = reform(swc(6,*))
;      plt8 = reform(swc(7,*))
      
;      write_csv, path_out + file_name, plt1, plt2, plt3, plt4 $
;        , plt5, header=['Wet1','Dry1','Wet2','Dry2','Wet3']

;-----------------------------------------------------------------------------------------!
; End of model (following is display of results).
;-----------------------------------------------------------------------------------------!
; 
;-----------------------------------------------------------------------------------------!
; Read observed swc data.
;-----------------------------------------------------------------------------------------!
  Read_insitu_swc_2014, measured_swc, measured_dates, high_yerror, low_yerror
;-----------------------------------------------------------------------------------------!
;
      ;-----------------------------------------------------------------------!
      ; prepare data for in situ and modeled swc (for linear regression).
      ;-----------------------------------------------------------------------!
      ts_model    = indgen(no_obs)    
      ;-----------------------------------------------------------------------!
      ;-----------------------------------------------------------------------!
      ; Legend (all quantities are volumetric as m3/m3).
      ;-----------------------------------------------------------------------!
      ; red          : swc (volumetric)
      ; grey         : swc calculated as water supply - PET
      ; green        : cumulated PET since first day of water supply.
      ; forest green : cumulated ET.
      ; blue         : cumulated water (from rain+irr).
      ;-----------------------------------------------------------------------!
      ; Error on model (max and min from different PET methods).
      ;-----------------------------------------------------------------------!
      high_xerror = reform(err_max(p_test-1,*))
      low_xerror  = reform(err_min(p_test-1,*))
      high_error  = reform(AET_m(p_test-1,*)) + high_xerror  ; for error band.
      low_error   = reform(AET_m(p_test-1,*)) - low_xerror   ; for error band.
      range       = n_elements(measured_dates)
      meas_dates  = measured_dates - 33
  ;-----------------------------------------------------------------------------!
  ; Open window.
  ;-----------------------------------------------------------------------------!
  ; Characteristics of window.
  ;-----------------------------------------------------------------------------!
      zm = 15                                               ; zoom.
      zm1 = zm * 10                                         ; re-scale x-axis.
      zm2 = zm * 0.015                                       ; re-scale y-axis
  ;-----------------------------------------------------------------------------!
      IF (!D.Flags AND 256) NE 0 THEN cgWindow, 'cgPlot', wxsize=4*zm1 $
        , wysize=1776*zm2, wtitle='volumetric quantities (m3/m3)' $
        , wypos=210, wxpos=400  
  ;-----------------------------------------------------------------------------!
  ; display ts.
  ;-----------------------------------------------------------------------------!
      position1 = [0.125, 0.22, 0.925, 0.925]
      cgControl, Execute=0
      cgplot, AET_m(p_test-1,*), ytitle='Accumulated water' $
        +' (m m$\up-2$)', xrange=[0,220], yrange=[0,10] $
        , Position=position1, YStyle=8, /NoData $
        , xtitle='days from 1/10/2013', charsize=2.0, /AddCmd
      cgColorFill, [ts_model, Reverse(ts_model), ts_model[0]], $
        [high_error, Reverse(low_error), high_error[0]], $
        Color='pink', /WINDOW
      cgplot, PET,  color='orange', linestyle=0 $
        ,thick=1, /overplot, /AddCmd
      cgplot, 10*cum_wat(p_test-1,*), color='blue', thick=1.5, /overplot, /AddCmd
      cgplot, AET_m(p_test-1,*), color='red', thick=1.7, /overplot, /AddCmd 
;      cgplot, meas_dates, measured_swc(p_test-1,*), PSym='Filled Circle' $
;        , symsize=1.0, color='black', thick=0.5, ERR_YLow=low_yerror(p_test-1,*) $
;        , ERR_YHigh=high_yerror(p_test-1,*), ERR_Color='black', /overplot, /AddCmd
      ;-----------------------------------------------------------------------!
      cgAxis, YAxis=1, YStyle=8, YRange=[0, 1.05], title='f$\downLAI$' $
        , charsize=2.0, color='grn6', /Save, /WINDOW
      cgplot, fVC(p_test-1,*), color='grn6', linestyle=3 $
        , thick=1, /overplot, /AddCmd
      ;-----------------------------------------------------------------------!
      streight_line1 = indgen(no_obs) * 0.0
      streight_line2 = indgen(no_obs) * 0.0 + 0.25
      ;-----------------------------------------------------------------------!
      cgplot, streight_line1, color='black', linestyle=2, /overplot, /AddCmd
      ;-----------------------------------------------------------------------!
      ; display annotation.
      ;-----------------------------------------------------------------------!
;      cgLegend, Title=['swc (obs)'], PSym=[16], symsize=1.3 $
;        , Location=[0.225, 0.75], Color=['black'], Length=0.0, charsize=2.0, /AddCmd
      cgLegend, Title=['f$\downLAI$', 'water (rain+irrig)', 'PET', $
       'AET'], PSym=[0,0,0,0] $
        , Location=[0.15, 0.88], Color=['grn6', 'blue',$
        'orange','red'], LineStyle=[3,0,0,0], charsize=1.8, /AddCmd
      ;-----------------------------------------------------------------------!
      ;
      ;-----------------------------------------------------------------------!
      file_name = 'WD_'+yr
      cgControl, Execute=1, create_pdf = strcompress(path_out + file_name)
  ;-----------------------------------------------------------------------------!
  ; Print image.
  ;-----------------------------------------------------------------------------!
  ; Scatterplot of validation.
  ;-----------------------------------------------------------------------------!
;      window, 2, xsize=510, ysize=450 $
;        , title='Soil water content', ypos=500, xpos=800
  ;-----------------------------------------------------------------------------!
  IF (!D.Flags AND 256) NE 0 THEN cgWindow,'cgPlot' $
    , wxsize=4*zm1/2.0, wysize=1776*15*0.01 $
    , wtitle='swc (mm3/mm3)', wypos=210, wxpos=800
      ;-------------------------------------------------------!
      ; do linear regression.
      ;-------------------------------------------------------!
      x_data = reform(swc(p_test-1,[meas_dates])) ; obs. data
      y_data = reform(measured_swc(p_test-1,*))  ; mod. data
      ;-------------------------------------------------------!
      x_max_error = high_xerror(meas_dates) ; x-axis error bars.
      x_min_error = low_xerror(meas_dates)  ; x-axis error bars.
      ;-------------------------------------------------------!
      rlt       = linfit(x_data, y_data)
      lincorr   = rlt[1] * [0.01,0.26] + rlt[0]
      y_modeled = rlt[1] * x_data + rlt[0]
      ;-------------------------------------------------------!
      ; display scatterplot.
      ;-------------------------------------------------------!
      position2 = [0.275, 0.2, 0.925, 0.925]
      cgControl, Execute=0
      cgplot, x_data, y_data, XTitle='Modeled (mm$\up3$ mm$\up-3$)' $
        , YTitle='Observed (mm$\up3$ mm$\up-3$)', xrange=[0.0,0.25] $
        , yrange=[0.0,0.25], SymColor='black' $
        , PSym='Open Circle', symsize=2.5, Position=position2 $
        , charsize=3.0, ERR_YLow=low_yerror(p_test-1,*) $
        , ERR_YHigh=high_yerror(p_test-1,*), ERR_XLow=x_min_error $
        , ERR_XHigh=x_max_error, ERR_Color='BLK5', /AddCmd
      ;-------------------------------------------------------!
      ; display linear regression and 1:1 line.
      ;-------------------------------------------------------!
     ; cgplot, [0.01,0.26], lincorr, color='black', /overplot, /AddCmd
      cgplot, [0.0,0.5], [0.0,0.5], linestyle=2, color='black', /overplot, /AddCmd
      ;-------------------------------------------------------!
      ; For annotation of linear equation and Rsq on plot.
      ;-------------------------------------------------------!
      rsq_annot = string((correlate(y_data, y_modeled))^2.0 $
        , FORMAT='(F0.3)')
      slope     = string(rlt[1], FORMAT='(F0.3)')
      inter     = string(rlt[0], FORMAT='(F0.3)')
      N_data    = strtrim(range,1); , FORMAT='(F0.0)')
      ;-------------------------------------------------------!
      ; display annotation.
      ;-------------------------------------------------------!
      cgText, 0.315, 0.86, /NORMAL, 'y$\up$  = '+ inter $
        + ' + ' + slope +'x', color='black', charsize=2.8, /AddCmd
      cgText, 0.315, 0.80, /NORMAL, 'r$\up2$ = '+rsq_annot $
        , color='black', charsize=2.8, /AddCmd
      cgText, 0.315, 0.74, /NORMAL, 'N = '+N_data $
        , color='black', charsize=2.8, /AddCmd
      ;-------------------------------------------------------!
      ; end scatterplot.
      ;-------------------------------------------------------!
      file_name = 'scatterplot_'+yr
      cgControl, Execute=1, create_pdf = strcompress(path_out + file_name)
      ;-------------------------------------------------------!
      ; Calculate RMSE and bias.
      ;-------------------------------------------------------!
      diff    = findgen(N_data) * 0.0
      diff_sq = findgen(N_data) * 0.0
      
      for j = 0, N_data-1 do begin
        diff(j) = x_data(j) - y_data(j)
        diff_sq(j) = diff(j)^2.0
      endfor
      RMSE = sqrt(total(diff_sq, /NaN) / N_data)
      RMSE_relative = 100 * (RMSE / mean(y_data,/NaN))
      bias = total(diff, /NaN) / N_data
      bias_relative = 100 * (bias / mean(y_data,/NaN))
      print, 'RMSE'
      print, RMSE, RMSE_relative
      print
      print,'============================'
      print, 'bias'
      print, bias, bias_relative
      write_csv, path_out + 'data_m2/WD-D1.csv', x_data, y_data, header=['observed','modeled']

END
;=========================================================================================!
