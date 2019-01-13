;===========================================================================================!
PRO Calculate_kc_ks_wet,t_accum,ks_max,kc_max $
  ,PET_JH,kc_JH,kc_PM,kc_buck,ks_JH,ks_PM,ks_buck,n_plots,fVC,fws_JH,fws_PM,fws_buck,f_VPD
  ;-----------------------------------------------------------------------------------------!
  ; Calculates fwa, fws and kc, ks (and PET from J-H) for the different PET methods.
  ; It reads meteo (from csv file) and NDVI data (from sav file).
  ;-----------------------------------------------------------------------------------------!
  ; Gets input: 
  ;-----------------------------------------------------------------------------------------!
  ;   +++ 'computer'   (computer = 'david' or 'davidhelman'         )
  ;   +++ 'yr'         (yr       = 2004 {integer for year of data  })
  ;   +++ 't_accum'    (t_accum  = 30   {integer for days in period})
  ;        from 'RS_Met_ET_model_v2.pro'
  ;   
  ;   Uses 'interpolated_NDVI_*'+yr+'.sav' file created with 'Make_interpoled_NDVI.pro'
  ;-----------------------------------------------------------------------------------------!
  ; 
  ; David Helman
  ; 9th May, 2017.
  ;-----------------------------------------------------------------------------------------!
  ;-----------------------------------------------------------------------------------------!
  ;
  ;-----------------------------------------------------------------------------------------!
  ; Call directory path and csv file.
  ;-----------------------------------------------------------------------------------------!
  @directory_RS_Met
  ;-----------------------------------------------------------------------------------------!
  ; Get data.
  ;-----------------------------------------------------------------------------------------!
  year        = yr 
  file        = 'Meteo_'+year+'_old.csv'
  dir         = dir_parent + file
  ;-----------------------------------------------------------------------------------------!
  ; Get data from csv and other general information.
  ;-----------------------------------------------------------------------------------------!
  T_header    = 3
  Result      = read_csv(dir, N_TABLE_HEADER=T_header)   ; read data into Result_NDVI.
  N           = n_elements(Result.FIELD01)               ; no. of rows in file.
  ;-----------------------------------------------------------------------------------------!
  ; Fields in Meteo_YEAR.csv file.
  ;-----------------------------------------------------------------------------------------!
  ; Read data into arrays.
  ;-----------------------------------------------------------------------------------------!
  jday        = Result.field01      ; jday.
  Gdate_met   = Result.field02      ; date.
  Ta          = Result.field03      ; mean air temp.
  RH          = Result.field07      ; relative humidity (mean daily as %)
  WS_W        = Result.field13      ; water supply (precip + irrigation).
  Rg          = Result.field05      ; global solar radiation.
  PET_PM      = Result.field16      ; Penmman-Monteith PET.
  PET_buck    = Result.field17      ; PET estimated using pot.
  ;-----------------------------------------------------------------------------------------!
  ; Calculate no. of observations (days).
  ;-----------------------------------------------------------------------------------------!
  no_obs      = n_elements(jday)
  ;-----------------------------------------------------------------------------------------!
  ; Calculate PET, fws and fwa as in Eqs. (1) and (11) in Ref_Helman17.
  ;-----------------------------------------------------------------------------------------!
  PET_JH      = findgen(no_obs) * 0.0  ; prepare arrays.
  SVP         = findgen(no_obs) * 0.0  ; prepare array for saturated vapor pressure.
  VPD         = findgen(no_obs) * 0.0  ; prepare array for vapor pressure deficit.
  for_f_VPD   = findgen(no_obs) * 0.0  ; prepare array for unscaled factor of VPD.
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
  ;  Loop over days.
  ;-------------------------------------------------------------------------!
  for i = 0, no_obs-1 do begin
    PET_JH(i) = (Rg(i)/cf) * (0.078 + 0.0252*Ta(i))
    SVP(i)    = 610.7 * 10^((7.5*Ta(i))/(237.3+Ta(i)))
    VPD(i)    = (1-(RH(i)/100))*SVP(i)/1000
    for_f_VPD(i)  = 1-(0.6*alog(VPD(i)))
  endfor
  scale_fVPD = max(for_f_VPD)
  f_VPD      = for_f_VPD / scale_fVPD
  for i = 0, no_obs-1 do begin
    ;-----------------------------------------------------------------------!
    ; Run diagnostics for fwa and fws.
    ;-----------------------------------------------------------------------!
    ld = i-t_accum
    if (ld lt 0.0) then (ld = 0) else (ld = i-t_accum)
    ;-----------------------------------------------------------------------!
    fwa_JH(i)    = total(WS_W(ld:i)) / total(PET_JH(ld:i))
    a = fwa_JH(i)
    if (a gt 1.0) then (fwa_JH(i) = 1.0) else (a = fwa_JH(i))
    fws_JH(i)    = 0.5 + 0.5 * fwa_JH(i)
    ;-----------------------------------------------------------------------!
    fwa_PM(i)    = total(WS_W(ld:i)) / total(PET_PM(ld:i))
    b = fwa_PM(i)
    if (b gt 1.0) then (fwa_PM(i) = 1.0) else (b = fwa_PM(i))
    fws_PM(i)    = 0.5 + 0.5 * fwa_PM(i)
    ;-----------------------------------------------------------------------!
    fwa_buck(i)  = total(WS_W(ld:i)) / total(PET_buck(ld:i))
    c = fwa_buck(i)
    if (c gt 1.0) then (fwa_buck(i) = 1.0) else (c = fwa_buck(i))
    fws_buck(i)  = 0.5 + 0.5 * fwa_buck(i)
    ;-----------------------------------------------------------------------!
  endfor
  ;-----------------------------------------------------------------------------------------!
  ;
  ;-----------------------------------------------------------------------------------------!
  ; Display PET from different methods.
  ;-----------------------------------------------------------------------------------------!
   ; cgPlot, fws_JH,   color='red', yrange=[0,1.1];, xrange=[70,190] 
   ; cgPlot, fws_PM,   color='blue',   /overplot
   ; cgPlot, fws_buck, color='black',  /overplot
  ;-----------------------------------------------------------------------------------------!
  ;
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
  dir_NDVI = dir_parent + '/interpolated_NDVI_'+year+'_60plots.sav'
  restore, dir_NDVI, /verbose
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
  ind        = where(Gdate(0) eq Gdate_met) + strt_d ; check that Meteo and NDVI concord.
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
     ;     NDVI_soil = min(reform(plots_ts_intpol(p,ind:*)))
     ;     NDVI_veg  = max(reform(plots_ts_intpol(p,ind:*)))
     ;     print, NDVI_soil
    ;-----------------------------------------------------------------------!
    ; Fill NDVI array with min data at start and end of ts (for each plot).
    ;-----------------------------------------------------------------------!
        ts_NDVI(p,0:ind-1) = plots_ts_intpol(p,strt_d) ; fill with min value.
        end_val = DAE(n_DAE-1)-1
        ts_NDVI(p,(ind+last_DAE-strt_d-2):*) = plots_ts_intpol(p,end_val)
        for time = 0, last_DAE-strt_d-1 do begin
          ts_NDVI(p,ind+time) = plots_ts_intpol(p,strt_d+time)
        endfor
    ;-----------------------------------------------------------------------!
    ; Convert NDVI to fVC (for each plot).
    ;-----------------------------------------------------------------------!
        fVC(p,*) = (ts_NDVI(p,*) - NDVI_soil) / (NDVI_veg - NDVI_soil)
        ;-------------------------------------------------------------------!
        ; Loop over days.
        ;-------------------------------------------------------------------!
        for t = 0, no_obs-1 do begin
          ;--------------------------------------------------------------!
          ; check fVC not greater than 1.
          ;--------------------------------------------------------------!
          if (fVC(p,t) gt 1) then fVC(p,t) = 1.0 else fVC(p,t) = fVC(p,t)
          ;--------------------------------------------------------------!
          kc_JH(p,t)  = kc_max * fVC(p,t) * max([fws_JH(t), f_VPD(t)])
          ks_JH(p,t)  = ks_max * (1 - fVC(p,t)) * fwa_JH(t)
          ;--------------------------------------------------------------!
          kc_PM(p,t)  = kc_max * fVC(p,t) * max([fws_PM(t), f_VPD(t)])
          ks_PM(p,t)  = ks_max * (1 - fVC(p,t)) * fwa_PM(t)
          ;--------------------------------------------------------------!
          kc_buck(p,t)  = kc_max * fVC(p,t) * max([fws_buck(t), f_VPD(t)])
          ks_buck(p,t)  = ks_max * (1 - fVC(p,t)) * fwa_buck(t)
          ;--------------------------------------------------------------!
        endfor   ; end loop over days.
        ;-------------------------------------------------------------------!
      endfor     ; end loop over plots.
  ;------------------------------------------------------------------------------------!
  ;
  ;------------------------------------------------------------------------------------!    
END
;===========================================================================================!
