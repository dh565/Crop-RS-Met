;===========================================================================================!
PRO Calculate_kc_ks,Gdate_met,WS,PET_PM,PET_buck,t_accum,ks_max,kc_max $
  ,PET_JH,kc_JH,kc_PM,kc_buck,ks_JH,ks_PM,ks_buck,n_plots,fVC
  ;-----------------------------------------------------------------------------------------!
  ; Calculates fwa, fws and kc, ks (and PET from J-H) for the different PET methods.
  ; 
  ; Reads meteo file (from csv file) and NDVI data (from sav file).
  ; Uses 'interpolated_NDVI_*'+yr+'.sav' file created with 'Read_insitu_NDVI_2017_SH'
  ; from --> directory '1_wheat_Bonfil/read_data/'
  ;
  ; David Helman
  ; 19th December, 2017 (Last modified on 28 May 2019).
  ;-----------------------------------------------------------------------------------------!
  ;
  ;-----------------------------------------------------------------------------------------!
  ; Call directory path and csv file.
  ;-----------------------------------------------------------------------------------------!
  @directory_RS_Met
  ;-----------------------------------------------------------------------------------------!
  ; Get data.
  ;-----------------------------------------------------------------------------------------!
  ; Crop and soil coefficients. @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  ; @@@ Activate when running this pro. not called by RS_Met. @@@@@@@@@@@@@@@@@@@@@@@@@@@@
  ;-----------------------------------------------------------------------------------------!
;  kc_max  = 1.2  ; maximum potential kc (following FAO56 see also Ref_Helman17).           
;  ks_max  = 0.2  ; maximum potential ks (Maselli etal 2014 used ks = 0.2 for grasses).    
;  t_accum = 15   ; (following Helman etal. 2019, AGWAT)
  ;-----------------------------------------------------------------------------------------!
  ; @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  ;-----------------------------------------------------------------------------------------!
  year        = yr 
  month       = 'Dec_'
  file        = 'Meteo_'+year+'.csv'
  dir         = dir_parent + file
  ;-----------------------------------------------------------------------------------------!
  ; Get data from csv and other general information.
  ;-----------------------------------------------------------------------------------------!
  T_header    = 3
  Result      = read_csv(dir, N_TABLE_HEADER=T_header)   ; read data into Result_NDVI.
  N           = n_elements(Result.field01)               ; no. of rows in file.
  ;-----------------------------------------------------------------------------------------!
  ; Fields in Meteo_YEAR.csv file.
  ;-----------------------------------------------------------------------------------------!
  ; Read data into arrays.
  ;-----------------------------------------------------------------------------------------!
  jday        = Result.field01      ; jday.
  Ta          = Result.field03      ; mean air temp.
  Rg          = Result.field05      ; global solar radiation.
  ;-----------------------------------------------------------------------------------------!
  ; These are being read from main prog. 'RS_Met_...'  
  ;-----------------------------------------------------------------------------------------!
;  WS          = Result.field13      ; water supply (precip + irrigation).
;  Gdate_met   = Result.field02      ; date.
;  PET_PM      = Result.field16      ; Penmman-Monteith PET.
;  PET_buck    = Result.field17      ; PET estimated using pot.
  ;-----------------------------------------------------------------------------------------!
  ; Calculate no. of observations (days).
  ;-----------------------------------------------------------------------------------------!
  no_obs      = n_elements(jday)
  ;-----------------------------------------------------------------------------------------!
  ; Calculate PET, fws and fwa as in Eqs. (1) and (11) in Ref_Helman17.
  ;-----------------------------------------------------------------------------------------!
  PET_JH      = findgen(no_obs) * 0.0  ; prepare arrays.
  ;-----------------------------------------------------------------------------------------!
  ; water stress and available water factors from different PET methods.
  ;-----------------------------------------------------------------------------------------!
  fws_JH      = findgen(no_obs) * 0.0
  fwa_JH      = findgen(no_obs) * 0.0
  ;------------------------------------------------;
  fws_PM      = findgen(no_obs) * 0.0
  fwa_PM      = findgen(no_obs) * 0.0
  ;------------------------------------------------;
  fws_buck    = findgen(no_obs) * 0.0
  fwa_buck    = findgen(no_obs) * 0.0
  ;-----------------------------------------------------------------------------------------!
  cf = 2470.0 / 1000.0   ; convertion factor from MJ d-1 to mm d-1.
  ;-----------------------------------------------------------------------------------------!
  ;  Loop over days to calculate daily ET0 from Rg and Ta.
  ;-------------------------------------------------------------------------!
  for i = 0, no_obs-1 do begin
    PET_JH(i) = (Rg(i)/cf) * (0.078 + 0.0252*Ta(i))
  endfor
  for i = 0, no_obs-1 do begin
    ;-----------------------------------------------------------------------!
    ; Run diagnostics for fwa and fws.
    ;-----------------------------------------------------------------------!
    ld = i-t_accum
    if (ld lt 0.0) then (ld = 0) else (ld = i-t_accum)
    ;-----------------------------------------------------------------------!
    fwa_JH(i)    = total(WS(ld:i)) / total(PET_JH(ld:i))
    a = fwa_JH(i)
    if (a gt 1.0) then (fwa_JH(i) = 1.0) else (a = fwa_JH(i))
    fws_JH(i)    = 0.5 + 0.5 * fwa_JH(i)
    ;-----------------------------------------------------------------------!
    fwa_PM(i)    = total(WS(ld:i)) / total(PET_PM(ld:i))
    b = fwa_PM(i)
    if (b gt 1.0) then (fwa_PM(i) = 1.0) else (b = fwa_PM(i))
    fws_PM(i)    = 0.5 + 0.5 * fwa_PM(i)
    ;-----------------------------------------------------------------------!
    fwa_buck(i)  = total(WS(ld:i)) / total(PET_buck(ld:i))
    c = fwa_buck(i)
    if (c gt 1.0) then (fwa_buck(i) = 1.0) else (c = fwa_buck(i))
    fws_buck(i)  = 0.5 + 0.5 * fwa_buck(i)
    ;-----------------------------------------------------------------------!
  endfor
  ;-----------------------------------------------------------------------------------------!
  ;
  ;-----------------------------------------------------------------------------------------!
  ; Display PET from different methods. @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  ; @@@ Activate when running this pro. not called from main 'RS_Met...' @@@@@@@@@@@@@@@
  ;-----------------------------------------------------------------------------------------!
;    cgPlot, fws_JH,   color='red', yrange=[0,1.1], xrange=[0,150] 
;    cgPlot, fws_PM,   color='blue',   /overplot
;    cgPlot, fws_buck, color='black',  /overplot
  ;-----------------------------------------------------------------------------------------!
  ; @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  ;-----------------------------------------------------------------------------------------!
  ; Read interpolated NDVI from the sav file (file created with 'read_insitu_NDVI.pro').
  ;    ++ Gdate           : the Gregorian date of original data {string(original dates)}.
  ;    ++ DAE             : original days after emeregence (i.e. after Gdate(0),
  ;                         {indgen(original dates)}).
  ;    ++ t_intpol        : interpolated days after emeregence (i.e. after Gdate(0),
  ;                         {indgen(total interpolated days)}).
  ;    ++ plots_ts_intpol : interpolated time series of NDVI
  ;                         {findgen(no. of plots,total interpolated days)}
  ;-----------------------------------------------------------------------------------------!
  dir_NDVI = dir_parent + '/interpolated_NDVI__KKL1_' + year+'.sav'
  restore, dir_NDVI, /verbose ; interpolated_NDVI_2014
  ;-----------------------------------------------------------------------------------------!
  ; Prepera array for NDVI with same length as meteo data.
  ;-----------------------------------------------------------------------------------------!
  no_size    = size(plots_ts_intpol)               ; reads size of plots_ts_intpol array.
  strt_d     = DAE(0) - 1
  n_plots    = no_size(1)                          ; reads no. of plots.
  last_DAE   = no_size(2)
  n_size     = size(DAE)
  n_DAE      = n_size(1)
  fVC        = findgen(n_plots, no_obs) * 0.0 ; prepare fVC array for each plot and date.
  ind        = where(Gdate(0) eq Gdate_met) ;+ strt_d ; check that Meteo and NDVI concord.
  ts_NDVI    = findgen(n_plots, no_obs) * 0.0
  ;-----------------------------------------------------------------------------------------!
  kc_JH    = findgen(n_plots,no_obs) * 0.0 ; for adjusted kc.
  ks_JH    = findgen(n_plots,no_obs) * 0.0 ; for adjusted ks.
  ;--------------------------------------------------------------------!
  kc_PM    = findgen(n_plots,no_obs) * 0.0 ; for adjusted kc.
  ks_PM    = findgen(n_plots,no_obs) * 0.0 ; for adjusted ks.
  ;--------------------------------------------------------------------!
  kc_buck  = findgen(n_plots,no_obs) * 0.0 ; for adjusted kc.
  ks_buck  = findgen(n_plots,no_obs) * 0.0 ; for adjusted ks.
  ;-----------------------------------------------------------------------------------------!
  ; Calculate Kc, ks and fVC as in Eqs. (5), (9-10) in Ref_Helman17.
  ;-----------------------------------------------------------------------------------------!
  ; Values for full vegetated and bare soil areas.
  ;-----------------------------------------------------------------------------------------!
  NDVI_soil = 0.10              ; NDVI value for bare soil.
  NDVI_veg  = 0.85              ; NDVI value for full vegetated area.
  ;-----------------------------------------------------------------------------------------!
  ;
    ;-----------------------------------------------------------------------!
    ; Loop over plots.
    ;-----------------------------------------------------------------------!
      for p = 0, n_plots-1 do begin
    ;-----------------------------------------------------------------------!
    ; Fill NDVI array with min data at start and end of ts (for each plot).
    ;-----------------------------------------------------------------------!
        ts_NDVI(p,0:ind) = plots_ts_intpol(p,strt_d) ; fill with min value.
        end_val = DAE(n_DAE-1)-1
        ts_NDVI(p,(last_DAE-1):*) = plots_ts_intpol(p,end_val)
        for time = 0, last_DAE-strt_d-1 do begin
          ts_NDVI(p,ind+time) = plots_ts_intpol(p,strt_d+time)
        endfor
    ;-----------------------------------------------------------------------!
    ; Convert NDVI to fVC (for each plot).
    ;-----------------------------------------------------------------------!
        fVC(p,*) = (ts_NDVI(p,*) - NDVI_soil) / (NDVI_veg - NDVI_soil)
;        cgPlot, ts_NDVI(0,*)
;        write_csv, path_out + 'test_NDVI_adj_NEW.csv', reform(ts_NDVI(0,*)) $
;          , header=['ndvi']
        ;-------------------------------------------------------------------!
        ; Loop over days.
        ;-------------------------------------------------------------------!
        for t = 0, no_obs-1 do begin
          ;--------------------------------------------------------------!
          ; check fVC not greater than 1.
          ;--------------------------------------------------------------!
          if (fVC(p,t) gt 1) then fVC(p,t) = 1.0 else fVC(p,t) = fVC(p,t)
          ;--------------------------------------------------------------!
          kc_JH(p,t)  = kc_max * fVC(p,t) * fws_JH(t) 
          ks_JH(p,t)  = ks_max * (1 - fVC(p,t)) * fwa_JH(t)
          ;--------------------------------------------------------------!
          kc_PM(p,t)  = kc_max * fVC(p,t) * fws_PM(t) 
          ks_PM(p,t)  = ks_max * (1 - fVC(p,t)) * fwa_PM(t)
          ;--------------------------------------------------------------!
          kc_buck(p,t)  = kc_max * fVC(p,t) * fws_buck(t) 
          ks_buck(p,t)  = ks_max * (1 - fVC(p,t)) * fwa_buck(t)
          ;--------------------------------------------------------------!
        endfor   ; end loop over days.
        ;-------------------------------------------------------------------!
      endfor     ; end loop over plots.
  ;-----------------------------------------------------------------------------------------!
  ; Display kc for plots. @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  ; @@@ Activate when running this pro. not called by RS_Met.@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  ;-----------------------------------------------------------------------------------------!
;  k = kc_JH
  ;-----------------------------------------------------------------------------------------!
;  cgPlot, k(0,*),   color='black', yrange=[0,1.25], xrange=[0,180]
  ;-----------------------------------------------------------------------------------------!
;  palet = ['red','blue','green','gold','purple','sky blue','GRN6']
;  for j = 1, N_plots-1 do begin
;    cgPlot, k(j,*), color=palet(j-1), /overplot
;  endfor
  ;-----------------------------------------------------------------------------------------!
  ; @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  ;-----------------------------------------------------------------------------------------!
END
;===========================================================================================!
