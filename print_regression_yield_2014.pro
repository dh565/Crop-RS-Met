PRO print_regression_yield_2014
  ;-----------------------------------------------------------------------------------------!
  ; Displays scatterplot of swc vs. grain yield
  ; Call procedures: 'RS_Met.pro' and 'Read_yield.pro'
  ;
  ; David Helman
  ; 6th June, 2017.
  ;-----------------------------------------------------------------------------------------!
  ;
  ;-----------------------------------------------------------------------------------------!
  ; Call directory path and csv file.
  ;-----------------------------------------------------------------------------------------!
  @directory_RS_Met
  ;-----------------------------------------------------------------------------------------!
  ; Year of data.
  ;-----------------------------------------------------------------------------------------!
  year = yr 
  ;-----------------------------------------------------------------------------------------!
  ; Call RS-Met model.
  ;-----------------------------------------------------------------------------------------!
  RS_Met_2014, n_plots, no_obs, SWC_f, t_ET, swc, err_max, err_min, cum_wat $
    , cum_PET, cum_ET, sm_pot, fVC
  ;-----------------------------------------------------------------------------------------!
  ; Call read_yield for regression.
  ;-----------------------------------------------------------------------------------------!
  Read_yield_2014, plot_n, yield
  ;-----------------------------------------------------------------------------------------!
  ;
  ;-----------------------------------------------------------------------------------------!
  ; Characteristics of window.
  ;-----------------------------------------------------------------------------------------!
  zm = 15                                               ; zoom.
  zm1 = zm * 10                                         ; re-scale x-axis.
  zm2 = zm * 0.01                                       ; re-scale y-axis
  ;-----------------------------------------------------------------------------------------!
  ; Open window.
  ;-----------------------------------------------------------------------------------------!
;  IF (!D.Flags AND 256) NE 0 THEN Window,3, xsize=4*zm1/2.0, ysize=1776*zm2 $
;    , title='swc vs. grain yield', ypos=210, xpos=1450
  IF (!D.Flags AND 256) NE 0 THEN cgWindow,'cgPlot', wxsize=4*zm1/2.0, wysize=1776*zm2 $
    , wtitle='swc vs. grain yield', wypos=210, wxpos=1450
  ;-----------------------------------------------------------------------------------------!
  ;
  ;-----------------------------------------------------------------------!
  ; Plot : scatterplot of swc vs yield.
  ;-----------------------------------------------------------------------!
;  WSet, 3
  ;-----------------------------------------------------------------------!
  ; compare with swc over the period:
  ;-----------------------------------------------------------------------!
  start_day = 147 ; no_obs-1 
  end_day   = 147 ; no_obs-1 
  ;-----------------------------------------------------------------------!
  cult_swc   = findgen(5)  * 0.0
  cult_yield = findgen(5)  * 0.0
;  for i = 0, 11 do begin
;    cult_swc(i,*) = mean(swc[(5*i):(5*(i+1)-1),*] $
;                         , DIMENSION=1)
;    cult_yield(i) = mean(yield[(5*i):(5*(i+1)-1)])
;  endfor
  for i = 0, 4 do begin
  ;-----------------------------------------------------------------------!
  ; Arrays for mean over all cultivars - 5 sets: 3 W and 2 D.
  ;-----------------------------------------------------------------------!
    yie1  = yield(0+i, *)
    yie2  = yield(9-i,*)
    yie3  = yield(10+i,*)
    yie4  = yield(19-i,*)
    yie5  = yield(20+i,*)
    yie6  = yield(29-i,*)
    yie7  = yield(30+i,*)
    yie8  = yield(39-i,*)
    yie9  = yield(40+i,*)
    yie10 = yield(49-i,*)
    yie11 = yield(50+i,*)
    yie12 = yield(59-i,*)
  ;-----------------------------------------------------------------------!
  ; This is to delete invalid data (Plot #56 (Amit) has too low yield=2.0).
  ;-----------------------------------------------------------------------!
    all_yield = [yie1,yie2,yie3,yie4,yie5,yie6,yie7 $
      ,yie8,yie9,yie10,yie11,yie12]
    NoValid = where(all_yield le 0.00, count)
    if (count gt 0) then begin
      all_yield(NoValid) = 0.0
      cult_yield(i) = total(all_yield)/(12-count)
    endif
    if (count le 0) then begin
      cult_yield(i)   = mean(all_yield)
    endif
;    cult_yield(i)  = yie4
  ;-----------------------------------------------------------------------!
  ; This is the same but for swc.
  ;-----------------------------------------------------------------------!
    cul1  = swc(0+i, *)
    cul2  = swc(9-i,*)
    cul3  = swc(10+i,*)
    cul4  = swc(19-i,*)
    cul5  = swc(20+i,*)
    cul6  = swc(29-i,*)
    cul7  = swc(30+i,*)
    cul8  = swc(39-i,*)
    cul9  = swc(40+i,*)
    cul10 = swc(49-i,*)
    cul11 = swc(50+i,*)
    cul12 = swc(59-i,*)
  ;-----------------------------------------------------------------------!
  ; If mean over days do:
  ;-----------------------------------------------------------------------!
    if (end_day - start_day) gt 0 then begin
      all_cult = [mean(cul1(start_day:end_day)) $
        ,mean(cul2(start_day:end_day)),mean(cul3(start_day:end_day))   $
        ,mean(cul4(start_day:end_day)),mean(cul5(start_day:end_day))   $
        ,mean(cul6(start_day:end_day)),mean(cul7(start_day:end_day))   $
        ,mean(cul8(start_day:end_day)),mean(cul9(start_day:end_day))   $
        ,mean(cul10(start_day:end_day)),mean(cul11(start_day:end_day)) $
        ,mean(cul12(start_day:end_day))]
      ;-------------------------------------------------------------------!
      ; check for valid data.
      ;-------------------------------------------------------------------!
      if (count gt 0) then begin
        all_cult(NoValid) = 0.0
        cult_swc(i)       = total(all_cult) / (12-count)
      endif
      if (count le 0) then begin
        cult_swc(i)       = mean(all_cult)
      endif
      ;-------------------------------------------------------------------!
      ; 
      ;-------------------------------------------------------------------!
    endif
  ;-----------------------------------------------------------------------!
  ; If single date do:
  ;-----------------------------------------------------------------------!
    if (end_day - start_day) le 0 then begin
      all_cult = ([cul1(start_day),cul2(start_day),cul3(start_day)     $
        ,cul4(start_day),cul5(start_day),cul6(start_day)               $
        ,cul7(start_day),cul8(start_day),cul9(start_day)               $
        ,cul10(start_day),cul11(start_day),cul12(start_day)])
      ;-------------------------------------------------------------------!
      ; check for valid data.
      ;-------------------------------------------------------------------!
      if (count gt 0) then begin
        all_cult(NoValid) = 0.0
        cult_swc(i)       = total(all_cult) / (12-count)
      endif
      if (count le 0) then begin
        cult_swc(i)       = mean(all_cult)
      endif
      ;-------------------------------------------------------------------!
      ; 
      ;-------------------------------------------------------------------!
    endif
;    cult_swc(i,*)  = cul4
  endfor  
  ;-----------------------------------------------------------------------!
  ; get data for linear regression.
  ;-----------------------------------------------------------------------!
  x_swc  = cult_swc
  x_data = reform(x_swc) * 1000.0 ; swc from RS-Met.
  ;-----------------------------------------------------------------------!
  ; vs.
  ;-----------------------------------------------------------------------!
  y_data = reform(cult_yield(*))
  ;-----------------------------------------------------------------------!
  rlt    = linfit(x_data, y_data)
  x_linfit  = [min(x_data)-10.0,max(x_data)+10.0]
  lincorr   = rlt[1] * x_linfit + rlt[0]
  y_modeled = rlt[1] * x_data + rlt[0]
  ;-----------------------------------------------------------------------!
  ; display scatterplot.
  ;-----------------------------------------------------------------------!
  position2 = [0.275, 0.2, 0.925, 0.925]
  cgControl, Execute=0
  cgPlot, x_data, y_data, XTitle='swc at heading stage (mm)' $
    , YTitle='Grain yield (ton/ha)' $
    , SymColor='blue', PSym='Open Circle', symsize=2.5 $
    , Position=position2, charsize=2.5 $
    , yrange=[min(y_data)-0.5,  max(y_data) +  0.5] $
    , xrange=[min(x_data)-20.0, max(x_data) + 20.0], /AddCmd
  ;-----------------------------------------------------------------------!
  ; display linear regression and 1:1 line.
  ;-----------------------------------------------------------------------!
  cgPlot, x_linfit, lincorr $
    , color='blue', /overplot, / AddCmd
  ;-----------------------------------------------------------------------!
  ; For annotation of linear equation and Rsq on plot.
  ;-----------------------------------------------------------------------!
  rsq_annot = string((correlate(y_data, y_modeled))^2.0 $
    , FORMAT='(F0.3)')
  slope     = string(rlt[1], FORMAT='(F0.3)')
  inter     = string(rlt[0], FORMAT='(F0.3)')
  N_data    = strtrim(n_elements(y_data),1)
  ;-----------------------------------------------------------------------!
  ; display annotation.
  ;-----------------------------------------------------------------------!
  cgText, 0.315, 0.86, /NORMAL, 'y$\up$  = '+ inter $
    + ' + ' + slope +'x', color='blue', charsize=2.5, /AddCmd
  cgText, 0.315, 0.80, /NORMAL, 'R$\up2$ = '+rsq_annot $
    , color='blue', charsize=2.5, /AddCmd
  cgText, 0.315, 0.74, /NORMAL, 'N = '+N_data $
    , color='blue', charsize=2.5, /AddCmd
  file_name = '1153'
  cgControl, Execute=1, create_pdf = strcompress(path_out + file_name)
  ;-----------------------------------------------------------------------!
  ; end scatterplot.
  ;-----------------------------------------------------------------------!
  print, y_data
  ;-----------------------------------------------------------------------------------------!
  ; end program.
  ;-----------------------------------------------------------------------------------------!
END