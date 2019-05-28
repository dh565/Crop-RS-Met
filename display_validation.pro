;=========================================================================================!
PRO Display_validation_2017_SH
;-----------------------------------------------------------------------------------------!
; Displays RS-Met model validation with ins-situ swc (Bonfil's data).
; Call procedures: 'RS_Met.pro' and 'Read_insitu_swc.pro'
;
; David Helman
; 6th June, 2017.
;-----------------------------------------------------------------------------------------!
; Call directory path.
;-----------------------------------------------------------------------------------------!
  @directory_RS_Met
;-----------------------------------------------------------------------------------------!
; Year of data.
;-----------------------------------------------------------------------------------------!
  year = yr ; get year from @directory_RS_Met
;-----------------------------------------------------------------------------------------!
; Call RS-Met model.
;-----------------------------------------------------------------------------------------!
  RS_Met, n_plots, no_obs, SWC_f, t_ET, swc, err_max, err_min, cum_wat $
    , cum_PET, cum_ET, sm_pot, fVC
;-----------------------------------------------------------------------------------------!
; Call read_in-situ data for swc validation.
;-----------------------------------------------------------------------------------------!
  Read_insitu_swc, measured_swc, meas_dates, err_post, err_neg
;-----------------------------------------------------------------------------------------!
;
;-----------------------------------------------------------------------------------------!
; Final swc (end of season) from model and in-sityu data.
;-----------------------------------------------------------------------------------------!
  swc_final_mod = findgen(n_plots) * 0.0
  swc_final_obs = findgen(n_plots) * 0.0
  plot_num      = indgen(n_plots) + 1
  range         = n_elements(meas_dates)
  for p = 0, n_plots-1 do begin
;    swc_final_mod(p) = swc(p,no_obs-1) * 1000.0   ; converted to mm per m2 of soil area.
;    swc_final_obs(p) = measured_swc(p,range-2) * 1000.0
    swc_final_mod(p) = swc(p,147) * 1000.0   ; converted to mm per m2 of soil area.
    swc_final_obs(p) = (measured_swc(p,range-12)+measured_swc(p,range-11)) * 1000.0/2.0  
  endfor
  file_name ='mod_obs_swc_' + year + '.csv'
;-----------------------------------------------------------------------------------------!
; Write csv file and save.
;-----------------------------------------------------------------------------------------!
;  write_csv, strcompress(path_out + file_name), plot_num, swc_final_mod $
;    , swc_final_obs, header=['plot no.','swc mod (mm/m2)', 'swc obs (mm/m2)']
;-----------------------------------------------------------------------------------------!
; Read and display image of plots.
;-----------------------------------------------------------------------------------------!
  read_png, dir_parent + 'plots_Bonfilv2.png', image    ; read png image.
  imSize=size(image)                                    ; get the size.
  zm = 15                                               ; zoom.
  zm1 = zm * 10                                         ; re-scale x-axis.
  zm2 = zm * 0.01                                       ; re-scale y-axis
;-----------------------------------------------------------------------------!
; Open window.
;-----------------------------------------------------------------------------!
  IF (!D.Flags AND 256) NE 0 THEN Window,0, xsize=imSize[1]*zm1 $
    , ysize=imSize[2]*zm2 $
    , title='cultivar plots', xpos=1100, ypos=800
  IF (!D.Flags AND 256) NE 0 THEN Window,1, xsize=imSize[1]*zm1 $
    , ysize=imSize[2]*zm2 $
    ,title='volumetric quantities (m3/m3)', ypos=500, xpos=1010;1100
  IF (!D.Flags AND 256) NE 0 THEN Window,2, xsize=imSize[1]*zm1/2.0 $
    , ysize=imSize[2]*zm2 $
    , title='Soil water content', ypos=500, xpos=1610
;-----------------------------------------------------------------------------!
; Display image.
;-----------------------------------------------------------------------------!
  WSet,0
  cgImage, image
;-----------------------------------------------------------------------------!
; Interactive mode.
;-----------------------------------------------------------------------------!
  cursor,x,y, /NORMAL, /CHANGE       ;DOWN - for touched ;CHANGE - for moved on
  while (!MOUSE.BUTTON ne 4) do begin
    WSet,0
    cursor,x,y, /NORMAL, /CHANGE
   ;--------------------------------------------------------------------------!
   ; Convert coordinates to serial number.
   ;--------------------------------------------------------------------------!
    p_y = long(x*zm1/(12.5*(zm/15.0)))
    p_x = long(y*zm1/(25*(zm/15.0)))
   ;--------------------------------------------------------------------------!
   ; Algorithm to convert to Bonfil's plots serial number.
   ;--------------------------------------------------------------------------!
    p_inv = (long(y*zm1/25))+(long(x*zm1/12.5))*5
    alg = (p_y/2.0) - long(p_y/2.0)
   
    if (alg ne 0.0) then (p_str = 60 - p_inv) else $
     (p_str = 56 - p_inv + p_x*2)
   ;--------------------------------------------------------------------------!
   ; Display cultiver's name when moved on the name field.
   ;--------------------------------------------------------------------------!
   if (y gt 0.830860) then begin
     print
       if (p_y eq 0)  then print, 'Amit',     mean(SWC_f(55:59)), ' mm' else $
       if (p_y eq 1)  then print, 'Omer',     mean(SWC_f(50:54)), ' mm' else $
       if (p_y eq 2)  then print, 'Binyamin', mean(SWC_f(45:49)), ' mm' else $
       if (p_y eq 3)  then print, 'Negev',    mean(SWC_f(40:44)), ' mm' else $
       if (p_y eq 4)  then print, 'Kizilitan',mean(SWC_f(35:39)), ' mm' else $
       if (p_y eq 5)  then print, 'Yuval',    mean(SWC_f(30:34)), ' mm' else $
       if (p_y eq 6)  then print, 'Ruta',     mean(SWC_f(25:29)), ' mm' else $
       if (p_y eq 7)  then print, 'Bar-Nir',  mean(SWC_f(20:24)), ' mm' else $
       if (p_y eq 8)  then print, 'Galil',    mean(SWC_f(15:19)), ' mm' else $
       if (p_y eq 9)  then print, 'Gedera',   mean(SWC_f(10:14)), ' mm' else $
       if (p_y eq 10) then print, 'Zahir',    mean(SWC_f(5:9)),   ' mm' else $
                           print, 'Sababo',   mean(SWC_f(0:4)),   ' mm'
     print,'-----------------------------------------------------------------'
     print
   endif
   ;--------------------------------------------------------------------------!
   ; Print plot no. (Bonfil's no.) and ET and final SWC.
   ;--------------------------------------------------------------------------!
    if (y le 0.830860) then begin
      print
      print, p_str, '       ET =', t_ET(p_str-1), '     SWC =', SWC_f(p_str-1) $
        , ' mm', ',  SWC =', 1000*measured_swc(p_str-1,21),' mm'
      print,'-------------------------------------------------------------------------------'
      WSet, 1
      ;-----------------------------------------------------------------------!
      ; prepare data for in situ and modeled swc (for linear regression).
      ;-----------------------------------------------------------------------!
      arr_selec = meas_dates  
      ts_model  = indgen(no_obs)    
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
      ;-----------------------------------------------------------------------!
      ; calculate error on model and measurements.
      ;-----------------------------------------------------------------------!
      data        = reform(measured_swc(p_str-1,*))      ; in-situ data.
      ;-----------------------------------------------------------------------!
      ; Error on measurements (max and min meas. + 0.02 on calibration).
      ;-----------------------------------------------------------------------!
      high_yerror = reform(err_post(p_str-1,*))+ 0.02     
      low_yerror  = reform(err_neg(p_str-1,*)) + 0.02   
      ;-----------------------------------------------------------------------!
      ; Error on model (max and min from different PET methods).
      ;-----------------------------------------------------------------------!
      high_xerror = reform(err_max(p_str-1,*))
      low_xerror  = reform(err_min(p_str-1,*))
      high_error  = reform(swc(p_str-1,*)) + high_xerror  ; for error band.
      low_error   = reform(swc(p_str-1,*)) - low_xerror   ; for error band.
      ;-----------------------------------------------------------------------!
      ; display ts.
      ;-----------------------------------------------------------------------!
      position1 = [0.125, 0.2, 0.925, 0.925]
      cgplot, swc(p_str-1,*), ytitle='volumetric water' $
        +' / fVC', yrange=[-0.05,0.5], Position=position1, /NoData $
        , xtitle='days from 1/10/2013'
      cgColorFill, [ts_model, Reverse(ts_model), ts_model[0]], $
        [high_error, Reverse(low_error), high_error[0]], $
        Color='pink'
      cgplot, sm_pot(p_str-1,*),  color='orange', linestyle=0 $
        ,thick=1, /overplot
      cgplot, cum_wat(p_str-1,*), color='blue', thick=1.5, /overplot
      cgplot, arr_selec, measured_swc(p_str-1,*) $
        , PSym='Filled Circle', symsize=0.8, color='black' $
        , thick=0.5, ERR_YLow=low_yerror, ERR_YHigh=high_yerror $
        , ERR_Color='black', /overplot ;, /overplot;
      cgplot, swc(p_str-1,*), color='red', thick=1.7, /overplot   
      cgplot, fVC(p_str-1,*)/2.0, color='grn6', linestyle=3 $
        , thick=1, /overplot
      ;-----------------------------------------------------------------------!
      streight_line1 = indgen(no_obs) * 0.0
      streight_line2 = indgen(no_obs) * 0.0 + 0.25
      cgplot, streight_line1,   color='black', linestyle=2, /overplot
      ;-----------------------------------------------------------------------!
      ; display annotation.
      ;-----------------------------------------------------------------------!
      cgLegend, Title=['swc (obs)'], PSym=[16], symsize=1.3 $
        , Location=[0.225, 0.66], Color=['black'], Length=0.0, charsize=1.25
      cgLegend, Title=['water (rain+irrig)', 'fVC', 'swc (pot)', $
       'swc (mod)'], PSym=[0,0,0,0] $
        , Location=[0.15, 0.88], Color=['blue','grn6', $
        'orange','red'], LineStyle=[0,3,0,0], charsize=1.25
      ;-----------------------------------------------------------------------!
      ; Scatterplot of validation.
      ;-----------------------------------------------------------------------!
      WSet, 2
      ;-------------------------------------------------------!
      ; do linear regression.
      ;-------------------------------------------------------!
      x_data = reform(swc(p_str-1,[arr_selec])) ; obs. data
      y_data = reform(measured_swc(p_str-1,*))  ; mod. data
      ;-------------------------------------------------------!
      x_max_error = high_xerror(arr_selec) ; x-axis error bars.
      x_min_error = low_xerror(arr_selec)  ; x-axis error bars.
      ;-------------------------------------------------------!
      rlt    = linfit(x_data, y_data)
      lincorr   = rlt[1] * [0.01,0.26] + rlt[0]
      y_modeled = rlt[1] * x_data + rlt[0]
      ;-------------------------------------------------------!
      ; display scatterplot.
      ;-------------------------------------------------------!
      position2 = [0.275, 0.2, 0.925, 0.925]
      cgplot, x_data, y_data, XTitle='Modeled (m/m2)' $
        , YTitle='Observed (m/m2)', xrange=[0.0,0.3] $
        , yrange=[0.0,0.3], SymColor='black' $
        , PSym='Open Circle', symsize=1.0, Position=position2 $
        , charsize=1.3, ERR_YLow=low_yerror $
        , ERR_YHigh=high_yerror, ERR_XLow=x_min_error $
        , ERR_XHigh=x_max_error, ERR_Color='black'
      ;-------------------------------------------------------!
      ; display linear regression and 1:1 line.
      ;-------------------------------------------------------!
      cgplot, [0.01,0.26], lincorr, color='black', /overplot
      cgplot, [0.0,0.5], [0.0,0.5], color='blue', /overplot
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
        + ' + ' + slope +'x', color='black', charsize=1.25
      cgText, 0.315, 0.80, /NORMAL, 'r$\up2$ = '+rsq_annot $
        , color='black', charsize=1.25
      cgText, 0.315, 0.74, /NORMAL, 'N = '+N_data $
        , color='black', charsize=1.25
      ;-------------------------------------------------------!
      ; end scatterplot.
      ;-------------------------------------------------------!
      ;
    endif
   ;--------------------------------------------------------------------------!
   ; end display mode.
   ;--------------------------------------------------------------------------!
  endwhile
;-----------------------------------------------------------------------------!
; end interactive mode.
;-----------------------------------------------------------------------------!
; 
;-----------------------------------------------------------------------------------------!
; end of display.
;-----------------------------------------------------------------------------------------!

END
;=========================================================================================!
