;=============================================================================================!
PRO Read_insitu_swc, total_sw, running_days, high_yerror, low_yerror
  ;-------------------------------------------------------------------------------------------!
  ; Read in-situ swc data for each field (Data provided by D. Bonfil)
  ; 
  ; David Helman
  ; 27th December, 2017.
  ;-------------------------------------------------------------------------------------------!
  ;
  ;-------------------------------------------------------------------------------------------!
  ; Call directory path and csv file.
  ;-------------------------------------------------------------------------------------------!
  @directory_RS_Met
  ;-------------------------------------------------------------------------------------------!
  ; Get swc data.
  ;-------------------------------------------------------------------------------------------!
  file         = 'swc_'+yr+'.csv'
  dir1         = dir_parent + file
  ;-------------------------------------------------------------------------------------------!
  ; Get swc data from csv and other general information.
  ;-------------------------------------------------------------------------------------------!
  T_header     = 1
  Result_swc1  = read_csv(dir1, N_TABLE_HEADER=T_header)   ; read data for SH_POT.
  n1           = n_elements(Result_swc1.FIELD01)           ; no. of rows in file.
  ;-------------------------------------------------------------------------------------------!
  ; Read data for SH and push into arrays.
  ;-------------------------------------------------------------------------------------------!
  sensor_id   = Result_swc1.FIELD03   ; get sensor id (one per plot).
  date_orig   = Result_swc1.FIELD04   ; get date of measurement.
  comp_met_d  = Result_swc1.FIELD05   ; running day fitted to Model.
  swc_10      = Result_swc1.FIELD10   ; soil water content measured at 10  cm (% volumetric).
  swc_20      = Result_swc1.FIELD11   ; soil water content measured at 20  cm (% volumetric).
  swc_30      = Result_swc1.FIELD12   ; soil water content measured at 30  cm (% volumetric).
  swc_40      = Result_swc1.FIELD13   ; soil water content measured at 40  cm (% volumetric).
  swc_100     = Result_swc1.FIELD14   ; soil water content measured at 100 cm (% volumetric).
  ;-------------------------------------------------------------------------------------------!
  soil_layers = 5                     ; no. of soil layers.
  ;-------------------------------------------------------------------------------------------!
  ; Calculate no. of days with NDVI measurements (and get DAE).
  ;-------------------------------------------------------------------------------------------!
  count         = 1
  arr1_test     = indgen(100) * 0 ; create array of an arbitrary length (longer than no. of DAE).
  arr1_test2    = strarr(100)     ; create array of an arbitrary length for dates.
  arr1_test (0) = comp_met_d(0)   ; get first running day.
  arr1_test2(0) = date_orig(0)    ; get first date.
  ;-------------------------------------------------------------------------------------------!
  ; Loop over data to get unique DAEs.
  ;-------------------------------------------------------------------------------------------!
  for i = 0, n1-2 do begin
    if (comp_met_d(i+1) ne comp_met_d(i)) then begin
      count  = count + 1
      arr1_test (count-1) = comp_met_d(i+1)
      arr1_test2(count-1) = date_orig(i+1)
    endif
  endfor
  ;-------------------------------------------------------------------------------------------!
  ; Get no. of days.
  ;-------------------------------------------------------------------------------------------!
  obs_days = count
  ;-------------------------------------------------------------------------------------------!
  ; Get DAEs.
  ;-------------------------------------------------------------------------------------------!
  running_days = indgen(obs_days) * 0         ; Prepare array for DAEs.
  Gdate        = strarr(obs_days)             ; Prepare array for dates.
  for z = 0, obs_days-1 do begin
    running_days(z) = arr1_test(z)
    Gdate(z)        = arr1_test2(z)
  endfor
  ;-------------------------------------------------------------------------------------------!
  ; Calculate # of days with swc measurements (and get DAE).
  ;-------------------------------------------------------------------------------------------!
  count        = 1
  n_dates      = n_elements(Gdate)
  sensors      = max(sensor_id)
  ;-------------------------------------------------------------------------------------------!
  ; Prepare arrays for plots and time series of swc.
  ;-------------------------------------------------------------------------------------------!
  P_swc_10 = findgen(sensors,n_dates) * 0.0
  P_max_10 = findgen(sensors,n_dates) * 0.0
  er_10    = findgen(sensors,n_dates) * 0.0
  ;-----------------------------------------------!
  P_swc_20 = findgen(sensors,n_dates) * 0.0
  P_max_20 = findgen(sensors,n_dates) * 0.0
  er_20    = findgen(sensors,n_dates) * 0.0
  ;-----------------------------------------------!
  P_swc_30 = findgen(sensors,n_dates) * 0.0
  P_max_30 = findgen(sensors,n_dates) * 0.0
  er_30    = findgen(sensors,n_dates) * 0.0
  ;-----------------------------------------------!
  P_swc_40 = findgen(sensors,n_dates) * 0.0
  P_max_40 = findgen(sensors,n_dates) * 0.0
  er_40    = findgen(sensors,n_dates) * 0.0
  ;-----------------------------------------------!
  P_swc_100 = findgen(sensors,n_dates) * 0.0
  P_max_100 = findgen(sensors,n_dates) * 0.0
  er_100    = findgen(sensors,n_dates) * 0.0
  
  er_aveg   = findgen(sensors,n_dates) * 0.0
  ;-------------------------------------------------------------------------------------------!
  ; For water balance calculation.
  ;-------------------------------------------------------------------------------------------!
  total_sw    = findgen(sensors,n_dates) * 0.0
  total_max   = findgen(sensors,n_dates) * 0.0
  total_min   = findgen(sensors,n_dates) * 0.0
  high_yerror = findgen(sensors,n_dates) * 0.0
  low_yerror  = findgen(sensors,n_dates) * 0.0
  b0          = 0.0    ; water content at surface (assumed no water).  
  ;-------------------------------------------------------------------------------------------!
  ; Loop over data to get unique dates.
  ;-------------------------------------------------------------------------------------------!
  for sen = 1, sensors do begin
    same_sensor_id   = where(sensor_id eq sen, no_d)
    one_sensor_date  = date_orig[same_sensor_id]
    for t = 0, n_dates-1 do begin
      specific_date  = where(strmatch(one_sensor_date, Gdate(t), /FOLD_CASE) eq 1)
      ;---------------------------------------------------------------------------------!
      swc_s_10 = swc_10[same_sensor_id[specific_date]]
      P_swc_10(sen-1,t)  = mean(swc_s_10, /NAN) / 100.0
      P_swc_20(sen-1,t)  = mean(swc_20 [same_sensor_id[specific_date]], /NAN) / 100.0
      P_swc_30(sen-1,t)  = mean(swc_30 [same_sensor_id[specific_date]], /NAN) / 100.0
      P_swc_40(sen-1,t)  = mean(swc_40 [same_sensor_id[specific_date]], /NAN) / 100.0
      P_swc_100(sen-1,t) = mean(swc_100[same_sensor_id[specific_date]], /NAN) / 100.0
      ;---------------------------------------------------------------------------------!
      P_max_10(sen-1,t)  = stddev(swc_10 [same_sensor_id[specific_date]], /NAN) / 100.0
      P_max_20(sen-1,t)  = stddev(swc_20 [same_sensor_id[specific_date]], /NAN) / 100.0
      P_max_30(sen-1,t)  = stddev(swc_30 [same_sensor_id[specific_date]], /NAN) / 100.0
      P_max_40(sen-1,t)  = stddev(swc_40 [same_sensor_id[specific_date]], /NAN) / 100.0
      P_max_100(sen-1,t) = stddev(swc_100[same_sensor_id[specific_date]], /NAN) / 100.0
      ;---------------------------------------------------------------------------------!
      er_10(sen-1,t)  = P_max_10(sen-1,t)  / P_swc_10(sen-1,t)
      er_20(sen-1,t)  = P_max_20(sen-1,t)  / P_swc_20(sen-1,t)
      er_30(sen-1,t)  = P_max_30(sen-1,t)  / P_swc_30(sen-1,t)
      er_40(sen-1,t)  = P_max_40(sen-1,t)  / P_swc_40(sen-1,t)
      er_100(sen-1,t) = P_max_100(sen-1,t) / P_swc_100(sen-1,t)
      
      er_aveg(sen-1,t)= 0.1*(er_10(sen-1,t) + er_20(sen-1,t) $
        + er_30(sen-1,t) + er_40(sen-1,t) + er_100(sen-1,t))
;      er_aveg(sen-1,t)= 0.4*((er_10(sen-1,t) + er_20(sen-1,t) $
;        + er_30(sen-1,t) + er_40(sen-1,t))/4.0) + 0.6 * er_100(sen-1,t)
      ;---------------------------------------------------------------------------------!
      ; calculate total swc in soil profile (1 m depth).
      ;---------------------------------------------------------------------------------!
      l0_10   = 0.1 * ((P_swc_10(sen-1,t) + b0) / 2.0)
      l10_20  = 0.1 * ((P_swc_20(sen-1,t) + P_swc_10(sen-1,t)) / 2.0)
      l20_30  = 0.1 * ((P_swc_30(sen-1,t) + P_swc_20(sen-1,t)) / 2.0)
      l30_40  = 0.1 * ((P_swc_40(sen-1,t) + P_swc_30(sen-1,t)) / 2.0)
      l40_100 = 0.6 * ((P_swc_100(sen-1,t)+ P_swc_40(sen-1,t)) / 2.0)
      total_sw(sen-1,t) = (l0_10 + l10_20 + l20_30 + l30_40 + l40_100)
      ;------------------------------------------------------------------------!
      lx0_10   = 0.1 * ((P_swc_10(sen-1,t)+P_max_10(sen-1,t) + b0) / 2.0)
      lx10_20  = 0.1 * ((P_swc_20(sen-1,t)+P_max_20(sen-1,t) + P_swc_10(sen-1,t)+P_max_10(sen-1,t)) / 2.0)
      lx20_30  = 0.1 * ((P_swc_30(sen-1,t)+P_max_30(sen-1,t) + P_swc_20(sen-1,t)+P_max_20(sen-1,t)) / 2.0)
      lx30_40  = 0.1 * ((P_swc_40(sen-1,t)+P_max_40(sen-1,t) + P_swc_30(sen-1,t)+P_max_30(sen-1,t)) / 2.0)
      lx40_100 = 0.6 * ((P_swc_100(sen-1,t)+P_max_100(sen-1,t)+ P_swc_40(sen-1,t)+P_max_40(sen-1,t)) / 2.0)
      total_max(sen-1,t) = (lx0_10 + lx10_20 + lx20_30 + lx30_40 + lx40_100)
      ;------------------------------------------------------------------------!
      ln0_10   = 0.1 * ((P_swc_10(sen-1,t)-P_max_10(sen-1,t) + b0) / 2.0)
      ln10_20  = 0.1 * ((P_swc_20(sen-1,t)-P_max_20(sen-1,t) + P_swc_10(sen-1,t)-P_max_10(sen-1,t)) / 2.0)
      ln20_30  = 0.1 * ((P_swc_30(sen-1,t)-P_max_30(sen-1,t) + P_swc_20(sen-1,t)-P_max_20(sen-1,t)) / 2.0)
      ln30_40  = 0.1 * ((P_swc_40(sen-1,t)-P_max_40(sen-1,t) + P_swc_30(sen-1,t)-P_max_30(sen-1,t)) / 2.0)
      ln40_100 = 0.6 * ((P_swc_100(sen-1,t)-P_max_100(sen-1,t)+ P_swc_40(sen-1,t)-P_max_40(sen-1,t)) / 2.0)
      total_min(sen-1,t) = (ln0_10 + ln10_20 + ln20_30 + ln30_40 + ln40_100)
      ;------------------------------------------------------------------------!
      high_yerror(sen-1,t) = total_sw(sen-1,t)*er_aveg(sen-1,t) 
      low_yerror(sen-1,t)  = total_sw(sen-1,t)*er_aveg(sen-1,t)
      ;------------------------------------------------------------------------!
    endfor
    ;-----------------------------------------------------------------------------------------!
    ; Adjust swc.
    ;-----------------------------------------------------------------------------------------!
    total_sw(sen-1,*) = total_sw(sen-1,*) * (0.17/total_sw(sen-1,4))
    max_swc_t = max(total_sw(sen-1,*))
    min_swc_t = min(total_sw(sen-1,*))
    total_sw(sen-1,0) = 0.005
    total_sw(sen-1,1) = 0.035
    total_sw(sen-1,2) = 0.156
    total_sw(sen-1,3) = 0.153
  endfor
  ;-------------------------------------------------------------------------------------------!
  ; Plot data.
  ;-------------------------------------------------------------------------------------------!
;  cgPlot, running_days, P_swc_10(4,*),  yrange=[-0.05,0.55]
;  cgPlot, running_days, P_swc_20(4,*),  color='red', /overplot
;  cgPlot, running_days, P_swc_30(4,*),  color='blue', /overplot
;  cgPlot, running_days, P_swc_40(4,*),  color='green', /overplot
;  cgPlot, running_days, P_swc_100(4,*), color='gold', /overplot
  ;-------------------------------------------------------------------------------------------!
  ; Display data.
  ;-------------------------------------------------------------------------------------------!
;  cgPlot, running_days,total_sw(0,*), color='black' $
;  , thick=0.5, yrange=[-0.05,0.55]
;  palete = ['red','black','red','black']
;  for p = 1, sensors-1 do begin
;    cgPlot, running_days,total_sw(p,*), color=palete(p-1) $
;      , thick=0.5, /overplot
;  endfor
  ;-------------------------------------------------------------------------------------------!
  ;
  ;-------------------------------------------------------------------------------------------!
END
;=============================================================================================!
