;==========================================================================================!
; Directory for all RS-Met programs.
;==========================================================================================!
; David Helman.
; 12th December, 2017.
;------------------------------------------------------------------------------------------!
;
;------------------------------------------------------------------------------------------!
; @@@@@ CHANGE FIELDS BELOW ACCORDINGLY @@@@@
;------------------------------------------------------------------------------------------!
; Computer.
;------------------------------------------------------------------------------------------!
  computer = 'macbook' ; 'imac' ; 'pc'
;------------------------------------------------------------------------------------------!
  case computer of
    'macbook' : computer = 'davidhelman'
    'imac'    : computer = 'david'
    'pc'      : computer = 'E:/'
    
    else: begin
      print, '============================================='
      print, ' Computer "' + computer + '" is not reconized...'
      print, '============================================='
      stop
    end
  endcase
;------------------------------------------------------------------------------------------!
; Year of data.
;------------------------------------------------------------------------------------------!
  yr   = '2017' ; '2014' ; 
;------------------------------------------------------------------------------------------!
; Main directory.
;------------------------------------------------------------------------------------------!
  dir_parent  = '/Users/'+computer+'/Dropbox (Personal)/Personal_backup/4_projects/' $
  + '1_wheat_Bonfil/1_Input_for_model/'+yr+'/'
;------------------------------------------------------------------------------------------!
; sub-directory for output files.
;------------------------------------------------------------------------------------------!
  path_out   = '/Users/'+computer+'/Desktop/'    ; directory in which output files will 
                                                ; be saved.
;==========================================================================================!
; End.
;==========================================================================================!