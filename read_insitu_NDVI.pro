;=============================================================================================!
PRO Read_insitu_NDVI
  ;-------------------------------------------------------------------------------------------!
  ; Read in-situ NDVI data for each treatment (Data provided by D. Bonfil)
  ; 
  ; David Helman
  ; 31th december, 2017.
  ;-------------------------------------------------------------------------------------------!
  ;
  ;-------------------------------------------------------------------------------------------!
; Open directory and csv file.
  ;-------------------------------------------------------------------------------------------!
  @directory_RS_Met
  ;-------------------------------------------------------------------------------------------!
  ; get field name, etc.
  ;-------------------------------------------------------------------------------------------!
  fields      = '_KKL2_' ; Fields at KKL1
  file        = 'NDVI_' + yr + fields + '*.csv'
  dir         = dir_parent + file
  ;-------------------------------------------------------------------------------------------!
  ; Get data from csv and other general information.
  ;-------------------------------------------------------------------------------------------!
  T_header    = 1
  Result_NDVI = read_csv(dir, N_TABLE_HEADER=T_header)   ; read data into Result_NDVI.
  N           = n_elements(Result_NDVI.FIELD1)          ; no. of rows in file.
  ;-------------------------------------------------------------------------------------------!
  ; Read data into arrays.
  ;-------------------------------------------------------------------------------------------!
  ns        = Result_NDVI.FIELD2      ; North to south position (of field).
  ew        = Result_NDVI.FIELD3      ; East to west position (of field).
  LUM       = Result_NDVI.FIELD6      ; Treatment (land use management).
  NDVI      = Result_NDVI.FIELD7      ; NDVI value.
  date_year = Result_NDVI.FIELD8      ; Gregorian date.
  DAE_orig  = Result_NDVI.FIELD9      ; Days after emergence.
  ;-------------------------------------------------------------------------------------------!
  ; get x and y of the field and last DAE.
  ;-------------------------------------------------------------------------------------------!
  x_row    = max(ew)
  y_col    = max(ns)
  last_DAE = max(DAE_orig)
  ;-------------------------------------------------------------------------------------------!
  ; Calculate no. of days with NDVI measurements (and get DAE).
  ;-------------------------------------------------------------------------------------------!
  count       = 1
  arr_test    = indgen(300) * 0 ; create array of an arbitrary length (longer than no. of DAE).
  arr_test2   = strarr(300)     ; create array of an arbitrary length for dates.
  arr_test (0) = DAE_orig(0)    ; get first DAE.
  arr_test2(0) = date_year(0)   ; get first date.
  ;-------------------------------------------------------------------------------------------!
  ; Loop over data to get unique DAEs.
  ;-------------------------------------------------------------------------------------------!
  for i = 0, N-2 do begin
    if (DAE_orig(i+1) ne DAE_orig(i)) then begin
      count  = count + 1
      arr_test (count-1) = DAE_orig(i+1)
      arr_test2(count-1) = date_year(i+1)
    endif
  endfor
  ;-------------------------------------------------------------------------------------------!
  ; Get no. of days.
  ;-------------------------------------------------------------------------------------------!
  obs_days = count
  ;-------------------------------------------------------------------------------------------!
  ; Get DAEs.
  ;-------------------------------------------------------------------------------------------!
  DAE   = intarr(obs_days) * 0         ; Prepare array for DAEs.
  Gdate = strarr(obs_days)             ; Prepare array for dates.
  for j = 0, obs_days-1 do begin
    DAE(j)   = arr_test(j)
    Gdate(j) = arr_test2(j)
  endfor
  print, DAE
  print, Gdate
  ;-------------------------------------------------------------------------------------------!
  ; Prepare array for plots and time series of NDVIs.
  ;-------------------------------------------------------------------------------------------!
  N_plots    = x_row
  Plots_NDVI = findgen(x_row, obs_days) * 0.0    
  ;-------------------------------------------------------------------------------------------!
  ; Identify plots by location (ew and ns) and push data.
  ;-------------------------------------------------------------------------------------------!
  for i = 1, N_plots do begin
    for d = 0, obs_days-1 do begin
      index_x = where(ew eq i and DAE_orig eq DAE(d), count_eq_x)
      Plots_NDVI(i-1,d) = total(NDVI(index_x)) / count_eq_x
      if (Plots_NDVI(i-1,d) le 0.0 or Plots_NDVI(i-1,d) ge 1.0) then $
        Plots_NDVI(i-1,d) = 0.0
    endfor
    test_ts = Plots_NDVI(i-1,*)
    invalid = where(test_ts eq 0.0)
    if invalid gt 0 then begin
      Plots_NDVI(i-1,invalid) = 0.5 * (Plots_NDVI(i-1,invalid-1) + Plots_NDVI(i-1,invalid+1))
    endif
  endfor
  cgPlot, Plots_NDVI(0,*), yrange=[0,1.0]
  for j = 1, N_plots-1 do begin
    cgPlot, Plots_NDVI(j,*), color='red', /overplot
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
    plots_ts_intpol(p,0:DAE(0)-2) = Plots_NDVI(p,0)
    for j = 0, obs_days-2 do begin
      span = DAE(j+1) - DAE(j)
      tsp  = indgen(span+1)
      var     = linfit(DAE(j:j+1), Plots_NDVI(p,j:(j+1)))
      ts      = var(1) * (tsp+DAE(j)) + var(0)
      plots_ts_intpol(p,DAE(j)-1:DAE(j+1)-1) = ts
    endfor  ; end loop over dates.
    ;----------------------------------------------------------------!
    ; Write and save csv file for each plot.
    ;----------------------------------------------------------------!
;    start_date = Gdate(0)                    ; get date of emergence.
;    x    = strsplit(start_date,'/',/extract) ; split string.
;    day  = x(0)      ; get day.
;    mon  = x(1)      ; get month.
;    yr   = x(2)      ; get year.
;    file_name = 'plot_' +strtrim(p+1,1)+'_'+strtrim(DAE(0)) $
;      +'_days_from_' + day+'_'+mon+'_'+yr+ '.csv'
;    write_csv, dir_save + file_name, t_intpol, plots_ts_intpol(p,*) $
;      , header=['DAE','NDVI']
  endfor
  ;-------------------------------------------------------------------------------------------!
  ;
  ;-------------------------------------------------------------------------------------------!
  ; Display data.
  ;-------------------------------------------------------------------------------------------!
  palet = ['black','black','red','grey','black','black','grey']
  window, 1
  cgPlot, plots_ts_intpol(0,*), color='red',yrange=[0,1.0], xrange=[-20,230]
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
    +'interpolated_NDVI_'+fields+yr+'.sav'
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
