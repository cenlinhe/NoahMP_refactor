# define namelist for NoahMP
export namelist_input="
&timing ! and output
  dt              = 600.0   ! timestep [seconds]
  maxtime         = 30      ! total simulation time [hours]
  output_filename = 'output.nc'
  runsnow         = ${snowopt} ! true or false whether to run snow cases
/

&forcing
  rainrate      = 20.0    ! rain rate, when raining [mm/hr]
  rain_duration = 3       ! duration of rain events [hours]
  dry_duration  = 3     ! duration of no-rain events [hours]
  raining       = .true.  ! start with rain
  uwind         = 3.0   ! wind speed m/s
  vwind         = 3.0   ! wind speed m/s
  sfcpres       = 90000.0  ! surface pressure Pa
  fcev_e        = 5.0       ! constant canopy evaporation heat (w/m2) [+ to atm ]
  fctr_e        = 5.0       ! constant transpiration heat (w/m2) [+ to atm]
  fgev_e        = 15.0      ! evaporation/sublimation heat (w/m2) [+ to atm]
/

&structure
 isltyp           = ${soiltp}       ! soil texture class
 vegtype          = ${vegetp}       ! vegetation type modis
 soilcolor        = 4       ! color to decide soil albedo
 slopetype        = 1       ! slope factor for underground runoff
 croptype         = 0       ! no crop
 nsoil            = 4       ! number of soil levels
 nsnow            = 3       ! number of snow levels
 structure_option = 1       ! 1: use preset zsoil; 2: uniform levels
 soil_depth       = 2.0     ! total soil thickness [m] for structure_option > 1
 vegfra           = 50      ! vegetation fraction 50%
 vegmax           = 50      ! Vegetation fraction annual max [0-1]
 shdmax           = 50        !yearly max vegetation fraction
/

&fixed_initial
 zsoil     = -0.1, -0.4, -1.0, -2.0  ! depth to level interface [m]
 dzsnso    =  0.0, 0.0,   0.0, 0.1,  0.3,  0.6,  1.0  ! level thickness [m]
/

&uniform_initial
 initial_uniform    = .true.         ! initial all levels the same
 initial_sh2o_value = 0.3            ! constant soil liquid value [vol]
 initial_sice_value = 0.0            ! constant soil ice value [vol]
/

&options
idveg = 4
iopt_crs = 1
iopt_btr = 1
iopt_run = ${runoff}  ! runoff
iopt_sfc = 1
iopt_frz = 1
iopt_inf = 1
iopt_rad = 3
iopt_alb = 1
iopt_snf = 1
iopt_tbot = 2
iopt_stc = 1
iopt_rsf = 1
iopt_soil = 1
iopt_pedo = 1
iopt_crop = 0
iopt_irr = ${irri}  ! irrigation
iopt_irrm = ${irrim} ! irri method
iopt_infdv = ${infdv} ! only for runoff=8
iopt_tdrn = ${tdrn} ! drainage only for runoff=3

/

"