;=============================================================================================!
PRO Read_insitu_NDVI_2014
  ;-------------------------------------------------------------------------------------------!
  ; Read in-situ NDVI data for each treatment (Data provided by D. Bonfil)
  ; 
  ; David Helman
  ; 27th december, 2017.
  ;-------------------------------------------------------------------------------------------!
  ;
  ;-------------------------------------------------------------------------------------------!
; Open directory and csv file.
  ;-------------------------------------------------------------------------------------------!
  @directory_RS_Met
  ;-------------------------------------------------------------------------------------------!
  ; get field name, etc.
  ;-------------------------------------------------------------------------------------------!
  file        = 'NDVI_' + yr + '.csv'
  dir         = dir_parent + file
  ;-------------------------------------------------------------------------------------------!
  ; Get data from csv and other general information.
  ;-------------------------------------------------------------------------------------------!
  T_header    = 1
  Result_NDVI = read_csv(dir, N_TABLE_HEADER=T_header)   ; read data into Result_NDVI.
  N           = n_elements(Result_NDVI.FIELD01)          ; no. of rows in file.
  ;-------------------------------------------------------------------------------------------!
  ; Read data into arrays.
  ;-------------------------------------------------------------------------------------------!
  ew          = Result_NDVI.FIELD02      ; East to west position (of field).
  ns          = Result_NDVI.FIELD03      ; North to south position (of field).
  DAE_orig    = Result_NDVI.FIELD05      ; Days after emergence.
  NDVI        = Result_NDVI.FIELD09      ; NDVI value.
  date_year   = Result_NDVI.FIELD10      ; Gregorian date.
  ;-------------------------------------------------------------------------------------------!
  ; get x and y of the field and last DAE.
  ;-------------------------------------------------------------------------------------------!
  x_row    = max(ew)
  y_col    = max(ns)
  last_DAE = max(DAE_orig)
  ;-------------------------------------------------------------------------------------------!
  ; Calculate no. of days with NDVI measurements (and get DAE).
  ;-------------------------------------------------------------------------------------------!
  count         = 1
  arr1_test     = indgen(100) * 0 ; create array of an arbitrary length (longer than no. of DAE).
  arr1_test2    = strarr(100)     ; create array of an arbitrary length for dates.
  arr1_test (0) = DAE_orig(0)    ; get first DAE.
  arr1_test2(0) = date_year(0)   ; get first date.
  ;-------------------------------------------------------------------------------------------!
  ; Loop over data to get unique DAEs.
  ;-------------------------------------------------------------------------------------------!
  for i = 0, N-2 do begin
    if (DAE_orig(i+1) ne DAE_orig(i)) then begin
      count  = count + 1
      arr1_test (count-1) = DAE_orig(i+1)
      arr1_test2(count-1) = date_year(i+1)
    endif
  endfor
  ;-------------------------------------------------------------------------------------------!
  ; Get no. of days.
  ;-------------------------------------------------------------------------------------------!
  obs_days = count
  ;-------------------------------------------------------------------------------------------!
  ; Get DAEs.
  ;-------------------------------------------------------------------------------------------!
  DAE   = indgen(obs_days) * 0         ; Prepare array for DAEs.
  Gdate = strarr(obs_days)             ; Prepare array for dates.
  for j = 0, obs_days-1 do begin
    DAE(j)   = arr1_test(j)
    Gdate(j) = arr1_test2(j)
  endfor
  ;-------------------------------------------------------------------------------------------!
  ; Prepare array for plots and time series of NDVIs.
  ;-------------------------------------------------------------------------------------------!
  N_plots    = y_col
  Plots_NDVI = findgen(y_col, obs_days) * 0.0    
  ;-------------------------------------------------------------------------------------------!
  ; Identify plots by location (ew and ns) and push data.
  ;-------------------------------------------------------------------------------------------!
  for i = 1, N_plots do begin
    for d = 0, obs_days-1 do begin
      index_x = where(ns eq i and DAE_orig eq DAE(d), count_eq_x)
      Plots_NDVI(i-1,d) = total(NDVI[index_x]) / count_eq_x
      if (Plots_NDVI(i-1,d) le 0.0 or Plots_NDVI(i-1,d) ge 1.0) then $
        Plots_NDVI(i-1,d) = 0.0
    endfor
  endfor
  ;-------------------------------------------------------------------------------------------!
  ; Fill missing data in ts with an average value of the same ts.
  ;-------------------------------------------------------------------------------------------!
  for i = 1, N_plots do begin
    if Plots_NDVI(i-1,0) eq 0.0 then Plots_NDVI(i-1,0) = Plots_NDVI(i-1,1)
    for d = 1, obs_days-1 do begin
      if Plots_NDVI(i-1,d) eq 0.0 then Plots_NDVI(i-1,d) = 0.5 * (Plots_NDVI(i-1,d-1) $
        + Plots_NDVI(i-1,d+1))
    endfor
  endfor
  ;-------------------------------------------------------------------------------------------!
  ; Fill missing data at early stages in the two dry treatments.
  ;-------------------------------------------------------------------------------------------!
;  Plots_NDVI(1,0:6) = 0.5 * (Plots_NDVI(0,0:6) + Plots_NDVI(2,0:6))
;  Plots_NDVI(3,0:6) = 0.5 * (Plots_NDVI(2,0:6) + Plots_NDVI(4,0:6))
  ;-------------------------------------------------------------------------------------------!
  ; Display plots of NDVI ts.
  ;-------------------------------------------------------------------------------------------!
  palet = ['red','black','red','black']
  cgPlot, Plots_NDVI(0,*), yrange=[0,1.0], thick=1
  for j = 1, N_plots-1 do begin
    cgPlot, Plots_NDVI(j,*), color=palet(j-1), /overplot
  endfor
  ;-------------------------------------------------------------------------------------------!
  ; Interpolate to daily values.
  ;-------------------------------------------------------------------------------------------!
  dir_save = dir_parent
  no_d     = DAE(obs_days-1)                     ; check last DAE for no. of timesteps (days).
  t_intpol = indgen(no_d) + 1                    ; prepare array for timesteps (daily).
  plots_ts_intpol = findgen(N_plots,no_d) * 0.0  ; prepare array for interpolated values.
  ;-------------------------------------------------------------------------------------------!
  ; Loop over plots.
  ;-------------------------------------------------------------------------------------------!
  for p = 0, N_plots-1 do begin
    ;----------------------------------------------------------------!
    ; Loopr over dates.
    ;----------------------------------------------------------------!
    if DAE(0) gt 1 then begin
      n_fill       = DAE(0) - 1
      for_arr_fill = findgen(n_fill) * 0.0 + 1.0
      arr_fill     = for_arr_fill * (Plots_NDVI(p,0))
      plots_ts_intpol(p,0:n_fill-1) = arr_fill
    endif
    for j = 0, obs_days-2 do begin
      span = DAE(j+1) - DAE(j)
      tsp  = indgen(span+1)
      var     = linfit(DAE(j:j+1), Plots_NDVI(p,j:(j+1)))
      ts      = var(1) * (tsp+DAE(j)) + var(0)
      plots_ts_intpol(p,DAE(j)-1:DAE(j+1)-1) = ts
    endfor  ; end loop over dates.
    ;----------------------------------------------------------------!
    ; end loop.
    ;----------------------------------------------------------------!
  endfor
  ;-------------------------------------------------------------------------------------------!
  ; Display plot of interpolated ts.
  ;-------------------------------------------------------------------------------------------!
  palet = ['red','black','red','black']
  window, 1
  cgPlot, plots_ts_intpol(0,*), yrange=[0,1.0];, xrange=[-20,230]
  for j = 1, N_plots-1 do begin
    cgPlot, plots_ts_intpol(j,*), color=palet(j-1), /overplot
  endfor
  ;-------------------------------------------------------------------------------------------!
  ;
  ;-------------------------------------------------------------------------------------------!
  ; Print DAE.
  ;-------------------------------------------------------------------------------------------!
  print, DAE
  print, Gdate
  ;-------------------------------------------------------------------------------------------!
  ; Save plots_ts_intpol as sav. file.
  ;-------------------------------------------------------------------------------------------!
  save, Gdate, DAE, t_intpol, plots_ts_intpol, filename = dir_save $
    +'interpolated_NDVI_'+yr+'.sav'
  print, '=================================================================================='
  print, '=================================================================================='
  print, '============================     FILE SAVED !!!    ==============================='
  print, '=================================================================================='
  print, '=================================================================================='
  ;-------------------------------------------------------------------------------------------!
  ;
  ;-------------------------------------------------------------------------------------------!
END
;=============================================================================================!
