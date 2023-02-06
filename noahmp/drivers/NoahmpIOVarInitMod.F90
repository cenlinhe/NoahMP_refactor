module NoahmpIOVarInitMod

!!! Initialize Noah-MP input/output variables (2D forcing, namelist, table, static)
!!! Input/Output variables should be first defined in NoahmpIOVarType.F90

! ------------------------ Code history -----------------------------------
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (Jan 2023)
! -------------------------------------------------------------------------

  use Machine
  use NoahmpIOVarType

  implicit none

contains

!=== initialize with default values

  subroutine NoahmpIOVarInitDefault(NoahmpIO)

    implicit none

    type(NoahmpIO_type), intent(inout) :: NoahmpIO
    
    associate(XSTART  =>  NoahmpIO%XSTART   ,&
              XEND    =>  NoahmpIO%XEND     ,&
              YSTART  =>  NoahmpIO%YSTART   ,&
              YEND    =>  NoahmpIO%YEND     ,&
              KDS     =>  NoahmpIO%KDS      ,&
              KDE     =>  NoahmpIO%KDE      ,&
              NSOIL   =>  NoahmpIO%NSOIL    ,&
              NSNOW   =>  NoahmpIO%NSNOW     &
             )

    allocate ( NoahmpIO%COSZEN       (XSTART:XEND,YSTART:YEND) )            ! cosine zenith angle
    allocate ( NoahmpIO%XLAT         (XSTART:XEND,YSTART:YEND) )            ! latitude [radians] 
    allocate ( NoahmpIO%DZ8W         (XSTART:XEND,KDS:KDE,YSTART:YEND) )    ! thickness of atmo layers [m]
    allocate ( NoahmpIO%DZS          (1:NSOIL)                   )          ! thickness of soil layers [m]
    allocate ( NoahmpIO%ZSOIL        (1:NSOIL)                   )          ! depth to soil interfaces [m] 
    allocate ( NoahmpIO%IVGTYP       (XSTART:XEND,YSTART:YEND) )            ! vegetation type
    allocate ( NoahmpIO%ISLTYP       (XSTART:XEND,YSTART:YEND) )            ! soil type
    allocate ( NoahmpIO%VEGFRA       (XSTART:XEND,YSTART:YEND) )            ! vegetation fraction []
    allocate ( NoahmpIO%TMN          (XSTART:XEND,YSTART:YEND) )            ! deep soil temperature [K]
    allocate ( NoahmpIO%XLAND        (XSTART:XEND,YSTART:YEND) )            ! =2 ocean; =1 land/seaice
    allocate ( NoahmpIO%XICE         (XSTART:XEND,YSTART:YEND) )            ! fraction of grid that is seaice
    allocate ( NoahmpIO%T_PHY        (XSTART:XEND,KDS:KDE,YSTART:YEND) )    ! 3D atmospheric temperature valid at mid-levels [K]
    allocate ( NoahmpIO%QV_CURR      (XSTART:XEND,KDS:KDE,YSTART:YEND) )    ! 3D water vapor mixing ratio [kg/kg_dry]
    allocate ( NoahmpIO%U_PHY        (XSTART:XEND,KDS:KDE,YSTART:YEND) )    ! 3D U wind component [m/s]
    allocate ( NoahmpIO%V_PHY        (XSTART:XEND,KDS:KDE,YSTART:YEND) )    ! 3D V wind component [m/s]
    allocate ( NoahmpIO%SWDOWN       (XSTART:XEND,YSTART:YEND) )            ! solar down at surface [W m-2]
    allocate ( NoahmpIO%SWDDIR       (XSTART:XEND,YSTART:YEND) )            ! solar down at surface [W m-2] for new urban solar panel
    allocate ( NoahmpIO%SWDDIF       (XSTART:XEND,YSTART:YEND) )            ! solar down at surface [W m-2] for new urban solar panel
    allocate ( NoahmpIO%GLW          (XSTART:XEND,YSTART:YEND) )            ! longwave down at surface [W m-2]
    allocate ( NoahmpIO%P8W          (XSTART:XEND,KDS:KDE,YSTART:YEND) )    ! 3D pressure, valid at interface [Pa]
    allocate ( NoahmpIO%RAINBL       (XSTART:XEND,YSTART:YEND) )            ! total precipitation entering land model [mm] per time step
    allocate ( NoahmpIO%SNOWBL       (XSTART:XEND,YSTART:YEND) )            ! snow entering land model [mm] per time step
    allocate ( NoahmpIO%RAINBL_tmp   (XSTART:XEND,YSTART:YEND) )            ! precipitation entering land model [mm]
    allocate ( NoahmpIO%SR           (XSTART:XEND,YSTART:YEND) )            ! frozen precip ratio entering land model [-]
    allocate ( NoahmpIO%RAINCV       (XSTART:XEND,YSTART:YEND) )            ! convective precip forcing [mm]
    allocate ( NoahmpIO%RAINNCV      (XSTART:XEND,YSTART:YEND) )            ! non-convective precip forcing [mm]
    allocate ( NoahmpIO%RAINSHV      (XSTART:XEND,YSTART:YEND) )            ! shallow conv. precip forcing [mm]
    allocate ( NoahmpIO%SNOWNCV      (XSTART:XEND,YSTART:YEND) )            ! non-covective snow forcing (subset of rainncv) [mm]
    allocate ( NoahmpIO%GRAUPELNCV   (XSTART:XEND,YSTART:YEND) )            ! non-convective graupel forcing (subset of rainncv) [mm]
    allocate ( NoahmpIO%HAILNCV      (XSTART:XEND,YSTART:YEND) )            ! non-convective hail forcing (subset of rainncv) [mm]
    allocate ( NoahmpIO%MP_RAINC     (XSTART:XEND,YSTART:YEND) )            ! convective precip forcing [mm]
    allocate ( NoahmpIO%MP_RAINNC    (XSTART:XEND,YSTART:YEND) )            ! non-convective precip forcing [mm]
    allocate ( NoahmpIO%MP_SHCV      (XSTART:XEND,YSTART:YEND) )            ! shallow conv. precip forcing [mm]
    allocate ( NoahmpIO%MP_SNOW      (XSTART:XEND,YSTART:YEND) )            ! non-covective snow (subset of rainnc) [mm]
    allocate ( NoahmpIO%MP_GRAUP     (XSTART:XEND,YSTART:YEND) )            ! non-convective graupel (subset of rainnc) [mm]
    allocate ( NoahmpIO%MP_HAIL      (XSTART:XEND,YSTART:YEND) )            ! non-convective hail (subset of rainnc) [mm]

    allocate ( NoahmpIO%bexp_3d      (XSTART:XEND,1:NSOIL,YSTART:YEND) )    ! C-H B exponent
    allocate ( NoahmpIO%smcdry_3D    (XSTART:XEND,1:NSOIL,YSTART:YEND) )    ! Soil Moisture Limit: Dry
    allocate ( NoahmpIO%smcwlt_3D    (XSTART:XEND,1:NSOIL,YSTART:YEND) )    ! Soil Moisture Limit: Wilt
    allocate ( NoahmpIO%smcref_3D    (XSTART:XEND,1:NSOIL,YSTART:YEND) )    ! Soil Moisture Limit: Reference
    allocate ( NoahmpIO%smcmax_3D    (XSTART:XEND,1:NSOIL,YSTART:YEND) )    ! Soil Moisture Limit: Max
    allocate ( NoahmpIO%dksat_3D     (XSTART:XEND,1:NSOIL,YSTART:YEND) )    ! Saturated Soil Conductivity
    allocate ( NoahmpIO%dwsat_3D     (XSTART:XEND,1:NSOIL,YSTART:YEND) )    ! Saturated Soil Diffusivity
    allocate ( NoahmpIO%psisat_3D    (XSTART:XEND,1:NSOIL,YSTART:YEND) )    ! Saturated Matric Potential
    allocate ( NoahmpIO%quartz_3D    (XSTART:XEND,1:NSOIL,YSTART:YEND) )    ! Soil quartz content
    allocate ( NoahmpIO%refdk_2D     (XSTART:XEND,YSTART:YEND) )            ! Reference Soil Conductivity
    allocate ( NoahmpIO%refkdt_2D    (XSTART:XEND,YSTART:YEND) )            ! Soil Infiltration Parameter
    allocate ( NoahmpIO%soilcomp     (XSTART:XEND,1:2*NSOIL,YSTART:YEND) )  ! Soil sand and clay content [fraction]
    allocate ( NoahmpIO%soilcl1      (XSTART:XEND,YSTART:YEND) )            ! Soil texture class with depth
    allocate ( NoahmpIO%soilcl2      (XSTART:XEND,YSTART:YEND) )            ! Soil texture class with depth
    allocate ( NoahmpIO%soilcl3      (XSTART:XEND,YSTART:YEND) )            ! Soil texture class with depth
    allocate ( NoahmpIO%soilcl4      (XSTART:XEND,YSTART:YEND) )            ! Soil texture class with depth
    allocate ( NoahmpIO%irr_frac_2D  (XSTART:XEND,YSTART:YEND) )            ! irrigation Fraction
    allocate ( NoahmpIO%irr_har_2D   (XSTART:XEND,YSTART:YEND) )            ! number of days before harvest date to stop irrigation 
    allocate ( NoahmpIO%irr_lai_2D   (XSTART:XEND,YSTART:YEND) )            ! Minimum lai to trigger irrigation
    allocate ( NoahmpIO%irr_mad_2D   (XSTART:XEND,YSTART:YEND) )            ! management allowable deficit (0-1)
    allocate ( NoahmpIO%filoss_2D    (XSTART:XEND,YSTART:YEND) )            ! fraction of flood irrigation loss (0-1) 
    allocate ( NoahmpIO%sprir_rate_2D(XSTART:XEND,YSTART:YEND) )            ! mm/h, sprinkler irrigation rate
    allocate ( NoahmpIO%micir_rate_2D(XSTART:XEND,YSTART:YEND) )            ! mm/h, micro irrigation rate
    allocate ( NoahmpIO%firtfac_2D   (XSTART:XEND,YSTART:YEND) )            ! flood application rate factor
    allocate ( NoahmpIO%ir_rain_2D   (XSTART:XEND,YSTART:YEND) )            ! maximum precipitation to stop irrigation trigger
    allocate ( NoahmpIO%bvic_2D      (XSTART:XEND,YSTART:YEND) )            ! VIC model infiltration parameter [-]
    allocate ( NoahmpIO%axaj_2D      (XSTART:XEND,YSTART:YEND) )            ! Tension water distribution inflection parameter [-]
    allocate ( NoahmpIO%bxaj_2D      (XSTART:XEND,YSTART:YEND) )            ! Tension water distribution shape parameter [-]
    allocate ( NoahmpIO%xxaj_2D      (XSTART:XEND,YSTART:YEND) )            ! Free water distribution shape parameter [-]
    allocate ( NoahmpIO%bdvic_2D     (XSTART:XEND,YSTART:YEND) )            ! DVIC model infiltration parameter [-]
    allocate ( NoahmpIO%gdvic_2D     (XSTART:XEND,YSTART:YEND) )            ! Mean Capillary Drive (m) for infiltration models
    allocate ( NoahmpIO%bbvic_2D     (XSTART:XEND,YSTART:YEND) )            ! DVIC heterogeniety parameter for infiltration [-]
    allocate ( NoahmpIO%KLAT_FAC     (XSTART:XEND,YSTART:YEND) )            ! factor multiplier to hydraulic conductivity
    allocate ( NoahmpIO%TDSMC_FAC    (XSTART:XEND,YSTART:YEND) )            ! factor multiplier to field capacity
    allocate ( NoahmpIO%TD_DC        (XSTART:XEND,YSTART:YEND) )            ! drainage coefficient for simple
    allocate ( NoahmpIO%TD_DCOEF     (XSTART:XEND,YSTART:YEND) )            ! drainge coefficient for Hooghoudt 
    allocate ( NoahmpIO%TD_DDRAIN    (XSTART:XEND,YSTART:YEND) )            ! depth of drain
    allocate ( NoahmpIO%TD_RADI      (XSTART:XEND,YSTART:YEND) )            ! tile radius
    allocate ( NoahmpIO%TD_SPAC      (XSTART:XEND,YSTART:YEND) )            ! tile spacing

! INOUT (with generic LSM equivalent) (as defined in WRF)
    allocate ( NoahmpIO%TSK          (XSTART:XEND,YSTART:YEND) )            ! surface radiative temperature [K]
    allocate ( NoahmpIO%HFX          (XSTART:XEND,YSTART:YEND) )            ! sensible heat flux [W m-2]
    allocate ( NoahmpIO%QFX          (XSTART:XEND,YSTART:YEND) )            ! latent heat flux [kg s-1 m-2]
    allocate ( NoahmpIO%LH           (XSTART:XEND,YSTART:YEND) )            ! latent heat flux [W m-2]
    allocate ( NoahmpIO%GRDFLX       (XSTART:XEND,YSTART:YEND) )            ! ground/snow heat flux [W m-2]
    allocate ( NoahmpIO%SMSTAV       (XSTART:XEND,YSTART:YEND) )            ! soil moisture avail. [not used]
    allocate ( NoahmpIO%SMSTOT       (XSTART:XEND,YSTART:YEND) )            ! total soil water [mm][not used]
    allocate ( NoahmpIO%SFCRUNOFF    (XSTART:XEND,YSTART:YEND) )            ! accumulated surface runoff [m]
    allocate ( NoahmpIO%UDRUNOFF     (XSTART:XEND,YSTART:YEND) )            ! accumulated sub-surface runoff [m]
    allocate ( NoahmpIO%ALBEDO       (XSTART:XEND,YSTART:YEND) )            ! total grid albedo []
    allocate ( NoahmpIO%SNOWC        (XSTART:XEND,YSTART:YEND) )            ! snow cover fraction []
    allocate ( NoahmpIO%SMOISEQ      (XSTART:XEND,1:NSOIL,YSTART:YEND) )    ! eq volumetric soil moisture [m3/m3]
    allocate ( NoahmpIO%SMOIS        (XSTART:XEND,1:NSOIL,YSTART:YEND) )    ! volumetric soil moisture [m3/m3]
    allocate ( NoahmpIO%SH2O         (XSTART:XEND,1:NSOIL,YSTART:YEND) )    ! volumetric liquid soil moisture [m3/m3]
    allocate ( NoahmpIO%TSLB         (XSTART:XEND,1:NSOIL,YSTART:YEND) )    ! soil temperature [K]
    allocate ( NoahmpIO%SNOW         (XSTART:XEND,YSTART:YEND) )            ! snow water equivalent [mm]
    allocate ( NoahmpIO%SNOWH        (XSTART:XEND,YSTART:YEND) )            ! physical snow depth [m]
    allocate ( NoahmpIO%CANWAT       (XSTART:XEND,YSTART:YEND) )            ! total canopy water + ice [mm]
    allocate ( NoahmpIO%ACSNOM       (XSTART:XEND,YSTART:YEND) )            ! accumulated snow melt leaving pack
    allocate ( NoahmpIO%ACSNOW       (XSTART:XEND,YSTART:YEND) )            ! accumulated snow on grid
    allocate ( NoahmpIO%EMISS        (XSTART:XEND,YSTART:YEND) )            ! surface bulk emissivity
    allocate ( NoahmpIO%QSFC         (XSTART:XEND,YSTART:YEND) )            ! bulk surface specific humidity

! INOUT (with no Noah LSM equivalent) (as defined in WRF)
    allocate ( NoahmpIO%ISNOWXY      (XSTART:XEND,YSTART:YEND) )            ! actual no. of snow layers
    allocate ( NoahmpIO%TVXY         (XSTART:XEND,YSTART:YEND) )            ! vegetation leaf temperature
    allocate ( NoahmpIO%TGXY         (XSTART:XEND,YSTART:YEND) )            ! bulk ground surface temperature
    allocate ( NoahmpIO%CANICEXY     (XSTART:XEND,YSTART:YEND) )            ! canopy-intercepted ice (mm)
    allocate ( NoahmpIO%CANLIQXY     (XSTART:XEND,YSTART:YEND) )            ! canopy-intercepted liquid water (mm)
    allocate ( NoahmpIO%EAHXY        (XSTART:XEND,YSTART:YEND) )            ! canopy air vapor pressure (pa)
    allocate ( NoahmpIO%TAHXY        (XSTART:XEND,YSTART:YEND) )            ! canopy air temperature (k)
    allocate ( NoahmpIO%CMXY         (XSTART:XEND,YSTART:YEND) )            ! bulk momentum drag coefficient
    allocate ( NoahmpIO%CHXY         (XSTART:XEND,YSTART:YEND) )            ! bulk sensible heat exchange coefficient
    allocate ( NoahmpIO%FWETXY       (XSTART:XEND,YSTART:YEND) )            ! wetted or snowed fraction of the canopy (-)
    allocate ( NoahmpIO%SNEQVOXY     (XSTART:XEND,YSTART:YEND) )            ! snow mass at last time step(mm h2o)
    allocate ( NoahmpIO%ALBOLDXY     (XSTART:XEND,YSTART:YEND) )            ! snow albedo at last time step (-)
    allocate ( NoahmpIO%QSNOWXY      (XSTART:XEND,YSTART:YEND) )            ! snowfall on the ground [mm/s]
    allocate ( NoahmpIO%QRAINXY      (XSTART:XEND,YSTART:YEND) )            ! rainfall on the ground [mm/s]
    allocate ( NoahmpIO%WSLAKEXY     (XSTART:XEND,YSTART:YEND) )            ! lake water storage [mm]
    allocate ( NoahmpIO%ZWTXY        (XSTART:XEND,YSTART:YEND) )            ! water table depth [m]
    allocate ( NoahmpIO%WAXY         (XSTART:XEND,YSTART:YEND) )            ! water in the "aquifer" [mm]
    allocate ( NoahmpIO%WTXY         (XSTART:XEND,YSTART:YEND) )            ! groundwater storage [mm]
    allocate ( NoahmpIO%SMCWTDXY     (XSTART:XEND,YSTART:YEND) )            ! soil moisture below the bottom of the column (m3m-3)
    allocate ( NoahmpIO%DEEPRECHXY   (XSTART:XEND,YSTART:YEND) )            ! recharge to the water table when deep (m)
    allocate ( NoahmpIO%RECHXY       (XSTART:XEND,YSTART:YEND) )            ! recharge to the water table (diagnostic) (m)
    allocate ( NoahmpIO%TSNOXY       (XSTART:XEND,-NSNOW+1:0,    YSTART:YEND) )  ! snow temperature [K]
    allocate ( NoahmpIO%ZSNSOXY      (XSTART:XEND,-NSNOW+1:NSOIL,YSTART:YEND) )  ! snow layer depth [m]
    allocate ( NoahmpIO%SNICEXY      (XSTART:XEND,-NSNOW+1:0,    YSTART:YEND) )  ! snow layer ice [mm]
    allocate ( NoahmpIO%SNLIQXY      (XSTART:XEND,-NSNOW+1:0,    YSTART:YEND) )  ! snow layer liquid water [mm]
    allocate ( NoahmpIO%LFMASSXY     (XSTART:XEND,YSTART:YEND) )            ! leaf mass [g/m2]
    allocate ( NoahmpIO%RTMASSXY     (XSTART:XEND,YSTART:YEND) )            ! mass of fine roots [g/m2]
    allocate ( NoahmpIO%STMASSXY     (XSTART:XEND,YSTART:YEND) )            ! stem mass [g/m2]
    allocate ( NoahmpIO%WOODXY       (XSTART:XEND,YSTART:YEND) )            ! mass of wood (incl. woody roots) [g/m2]
    allocate ( NoahmpIO%GRAINXY      (XSTART:XEND,YSTART:YEND) )            ! mass of grain XING [g/m2]
    allocate ( NoahmpIO%GDDXY        (XSTART:XEND,YSTART:YEND) )            ! growing degree days XING FOUR
    allocate ( NoahmpIO%STBLCPXY     (XSTART:XEND,YSTART:YEND) )            ! stable carbon in deep soil [g/m2]
    allocate ( NoahmpIO%FASTCPXY     (XSTART:XEND,YSTART:YEND) )            ! short-lived carbon, shallow soil [g/m2]
    allocate ( NoahmpIO%LAI          (XSTART:XEND,YSTART:YEND) )            ! leaf area index
    allocate ( NoahmpIO%LAI_tmp      (XSTART:XEND,YSTART:YEND) )            ! leaf area index
    allocate ( NoahmpIO%XSAIXY       (XSTART:XEND,YSTART:YEND) )            ! stem area index
    allocate ( NoahmpIO%TAUSSXY      (XSTART:XEND,YSTART:YEND) )            ! snow age factor

! irrigation
    allocate ( NoahmpIO%IRFRACT      (XSTART:XEND,YSTART:YEND) )            ! irrigation fraction
    allocate ( NoahmpIO%SIFRACT      (XSTART:XEND,YSTART:YEND) )            ! sprinkler irrigation fraction
    allocate ( NoahmpIO%MIFRACT      (XSTART:XEND,YSTART:YEND) )            ! micro irrigation fraction
    allocate ( NoahmpIO%FIFRACT      (XSTART:XEND,YSTART:YEND) )            ! flood irrigation fraction   
    allocate ( NoahmpIO%IRNUMSI      (XSTART:XEND,YSTART:YEND) )            ! irrigation event number, Sprinkler
    allocate ( NoahmpIO%IRNUMMI      (XSTART:XEND,YSTART:YEND) )            ! irrigation event number, Micro
    allocate ( NoahmpIO%IRNUMFI      (XSTART:XEND,YSTART:YEND) )            ! irrigation event number, Flood 
    allocate ( NoahmpIO%IRWATSI      (XSTART:XEND,YSTART:YEND) )            ! irrigation water amount [m] to be applied, Sprinkler
    allocate ( NoahmpIO%IRWATMI      (XSTART:XEND,YSTART:YEND) )            ! irrigation water amount [m] to be applied, Micro
    allocate ( NoahmpIO%IRWATFI      (XSTART:XEND,YSTART:YEND) )            ! irrigation water amount [m] to be applied, Flood
    allocate ( NoahmpIO%IRELOSS      (XSTART:XEND,YSTART:YEND) )            ! loss of irrigation water to evaporation,sprinkler [mm]
    allocate ( NoahmpIO%IRSIVOL      (XSTART:XEND,YSTART:YEND) )            ! amount of irrigation by sprinkler (mm)
    allocate ( NoahmpIO%IRMIVOL      (XSTART:XEND,YSTART:YEND) )            ! amount of irrigation by micro (mm)
    allocate ( NoahmpIO%IRFIVOL      (XSTART:XEND,YSTART:YEND) )            ! amount of irrigation by micro (mm)
    allocate ( NoahmpIO%IRRSPLH      (XSTART:XEND,YSTART:YEND) )            ! latent heating from sprinkler evaporation (w/m2)
    allocate ( NoahmpIO%LOCTIM       (XSTART:XEND,YSTART:YEND) )            ! local time
  
! OUT (with no Noah LSM equivalent) (as defined in WRF)   
    allocate ( NoahmpIO%T2MVXY       (XSTART:XEND,YSTART:YEND) )            ! 2m temperature of vegetation part
    allocate ( NoahmpIO%T2MBXY       (XSTART:XEND,YSTART:YEND) )            ! 2m temperature of bare ground part
    allocate ( NoahmpIO%Q2MVXY       (XSTART:XEND,YSTART:YEND) )            ! 2m mixing ratio of vegetation part
    allocate ( NoahmpIO%Q2MBXY       (XSTART:XEND,YSTART:YEND) )            ! 2m mixing ratio of bare ground part
    allocate ( NoahmpIO%TRADXY       (XSTART:XEND,YSTART:YEND) )            ! surface radiative temperature (k)
    allocate ( NoahmpIO%NEEXY        (XSTART:XEND,YSTART:YEND) )            ! net ecosys exchange (g/m2/s CO2)
    allocate ( NoahmpIO%GPPXY        (XSTART:XEND,YSTART:YEND) )            ! gross primary assimilation [g/m2/s C]
    allocate ( NoahmpIO%NPPXY        (XSTART:XEND,YSTART:YEND) )            ! net primary productivity [g/m2/s C]
    allocate ( NoahmpIO%FVEGXY       (XSTART:XEND,YSTART:YEND) )            ! Noah-MP vegetation fraction [-]
    allocate ( NoahmpIO%RUNSFXY      (XSTART:XEND,YSTART:YEND) )            ! surface runoff [mm per soil timestep]
    allocate ( NoahmpIO%RUNSBXY      (XSTART:XEND,YSTART:YEND) )            ! subsurface runoff [mm per soil timestep]
    allocate ( NoahmpIO%ECANXY       (XSTART:XEND,YSTART:YEND) )            ! evaporation of intercepted water (mm/s)
    allocate ( NoahmpIO%EDIRXY       (XSTART:XEND,YSTART:YEND) )            ! soil surface evaporation rate (mm/s]
    allocate ( NoahmpIO%ETRANXY      (XSTART:XEND,YSTART:YEND) )            ! transpiration rate (mm/s)
    allocate ( NoahmpIO%FSAXY        (XSTART:XEND,YSTART:YEND) )            ! total absorbed solar radiation (w/m2)
    allocate ( NoahmpIO%FIRAXY       (XSTART:XEND,YSTART:YEND) )            ! total net longwave rad (w/m2) [+ to atm]
    allocate ( NoahmpIO%APARXY       (XSTART:XEND,YSTART:YEND) )            ! photosyn active energy by canopy (w/m2)
    allocate ( NoahmpIO%PSNXY        (XSTART:XEND,YSTART:YEND) )            ! total photosynthesis (umol co2/m2/s) [+]
    allocate ( NoahmpIO%SAVXY        (XSTART:XEND,YSTART:YEND) )            ! solar rad absorbed by veg. (w/m2)
    allocate ( NoahmpIO%SAGXY        (XSTART:XEND,YSTART:YEND) )            ! solar rad absorbed by ground (w/m2)
    allocate ( NoahmpIO%RSSUNXY      (XSTART:XEND,YSTART:YEND) )            ! sunlit leaf stomatal resistance (s/m)
    allocate ( NoahmpIO%RSSHAXY      (XSTART:XEND,YSTART:YEND) )            ! shaded leaf stomatal resistance (s/m)
    allocate ( NoahmpIO%BGAPXY       (XSTART:XEND,YSTART:YEND) )            ! between gap fraction
    allocate ( NoahmpIO%WGAPXY       (XSTART:XEND,YSTART:YEND) )            ! within gap fraction
    allocate ( NoahmpIO%TGVXY        (XSTART:XEND,YSTART:YEND) )            ! under canopy ground temperature[K]
    allocate ( NoahmpIO%TGBXY        (XSTART:XEND,YSTART:YEND) )            ! bare ground temperature [K]
    allocate ( NoahmpIO%CHVXY        (XSTART:XEND,YSTART:YEND) )            ! sensible heat exchange coefficient vegetated
    allocate ( NoahmpIO%CHBXY        (XSTART:XEND,YSTART:YEND) )            ! sensible heat exchange coefficient bare-ground
    allocate ( NoahmpIO%SHGXY        (XSTART:XEND,YSTART:YEND) )            ! veg ground sen. heat [w/m2]   [+ to atm]
    allocate ( NoahmpIO%SHCXY        (XSTART:XEND,YSTART:YEND) )            ! canopy sen. heat [w/m2]   [+ to atm]
    allocate ( NoahmpIO%SHBXY        (XSTART:XEND,YSTART:YEND) )            ! bare sensible heat [w/m2]  [+ to atm]
    allocate ( NoahmpIO%EVGXY        (XSTART:XEND,YSTART:YEND) )            ! veg ground evap. heat [w/m2]  [+ to atm]
    allocate ( NoahmpIO%EVBXY        (XSTART:XEND,YSTART:YEND) )            ! bare soil evaporation [w/m2]  [+ to atm]
    allocate ( NoahmpIO%GHVXY        (XSTART:XEND,YSTART:YEND) )            ! veg ground heat flux [w/m2]  [+ to soil]
    allocate ( NoahmpIO%GHBXY        (XSTART:XEND,YSTART:YEND) )            ! bare ground heat flux [w/m2] [+ to soil]
    allocate ( NoahmpIO%IRGXY        (XSTART:XEND,YSTART:YEND) )            ! veg ground net LW rad. [w/m2] [+ to atm]
    allocate ( NoahmpIO%IRCXY        (XSTART:XEND,YSTART:YEND) )            ! canopy net LW rad. [w/m2] [+ to atm]
    allocate ( NoahmpIO%IRBXY        (XSTART:XEND,YSTART:YEND) )            ! bare net longwave rad. [w/m2] [+ to atm]
    allocate ( NoahmpIO%TRXY         (XSTART:XEND,YSTART:YEND) )            ! transpiration [w/m2]  [+ to atm]
    allocate ( NoahmpIO%EVCXY        (XSTART:XEND,YSTART:YEND) )            ! canopy evaporation heat [w/m2]  [+ to atm]
    allocate ( NoahmpIO%CHLEAFXY     (XSTART:XEND,YSTART:YEND) )            ! leaf exchange coefficient 
    allocate ( NoahmpIO%CHUCXY       (XSTART:XEND,YSTART:YEND) )            ! under canopy exchange coefficient 
    allocate ( NoahmpIO%CHV2XY       (XSTART:XEND,YSTART:YEND) )            ! veg 2m exchange coefficient 
    allocate ( NoahmpIO%CHB2XY       (XSTART:XEND,YSTART:YEND) )            ! bare 2m exchange coefficient 
    allocate ( NoahmpIO%RS           (XSTART:XEND,YSTART:YEND) )            ! Total stomatal resistance (s/m)
    allocate ( NoahmpIO%Z0           (XSTART:XEND,YSTART:YEND) )            ! roughness length output to WRF 
    allocate ( NoahmpIO%ZNT          (XSTART:XEND,YSTART:YEND) )            ! roughness length output to WRF 
    allocate ( NoahmpIO%QTDRAIN      (XSTART:XEND,YSTART:YEND) )            ! tile drainage (mm)
    allocate ( NoahmpIO%TD_FRACTION  (XSTART:XEND,YSTART:YEND) )            ! tile drainage fraction
    allocate ( NoahmpIO%XLONG        (XSTART:XEND,YSTART:YEND) )            ! longitude
    allocate ( NoahmpIO%TERRAIN      (XSTART:XEND,YSTART:YEND) )            ! terrain height
    allocate ( NoahmpIO%GVFMIN       (XSTART:XEND,YSTART:YEND) )            ! annual minimum in vegetation fraction
    allocate ( NoahmpIO%GVFMAX       (XSTART:XEND,YSTART:YEND) )            ! annual maximum in vegetation fraction

! additional output variables
    allocate ( NoahmpIO%PAHXY        (XSTART:XEND,YSTART:YEND) )
    allocate ( NoahmpIO%PAHGXY       (XSTART:XEND,YSTART:YEND) )
    allocate ( NoahmpIO%PAHBXY       (XSTART:XEND,YSTART:YEND) )
    allocate ( NoahmpIO%PAHVXY       (XSTART:XEND,YSTART:YEND) )
    allocate ( NoahmpIO%QINTSXY      (XSTART:XEND,YSTART:YEND) )
    allocate ( NoahmpIO%QINTRXY      (XSTART:XEND,YSTART:YEND) )
    allocate ( NoahmpIO%QDRIPSXY     (XSTART:XEND,YSTART:YEND) )
    allocate ( NoahmpIO%QDRIPRXY     (XSTART:XEND,YSTART:YEND) )
    allocate ( NoahmpIO%QTHROSXY     (XSTART:XEND,YSTART:YEND) )
    allocate ( NoahmpIO%QTHRORXY     (XSTART:XEND,YSTART:YEND) )
    allocate ( NoahmpIO%QSNSUBXY     (XSTART:XEND,YSTART:YEND) )
    allocate ( NoahmpIO%QSNFROXY     (XSTART:XEND,YSTART:YEND) )
    allocate ( NoahmpIO%QSUBCXY      (XSTART:XEND,YSTART:YEND) )
    allocate ( NoahmpIO%QFROCXY      (XSTART:XEND,YSTART:YEND) )
    allocate ( NoahmpIO%QEVACXY      (XSTART:XEND,YSTART:YEND) )
    allocate ( NoahmpIO%QDEWCXY      (XSTART:XEND,YSTART:YEND) )
    allocate ( NoahmpIO%QFRZCXY      (XSTART:XEND,YSTART:YEND) )
    allocate ( NoahmpIO%QMELTCXY     (XSTART:XEND,YSTART:YEND) )
    allocate ( NoahmpIO%QSNBOTXY     (XSTART:XEND,YSTART:YEND) )
    allocate ( NoahmpIO%QMELTXY      (XSTART:XEND,YSTART:YEND) )
    allocate ( NoahmpIO%PONDINGXY    (XSTART:XEND,YSTART:YEND) )
    allocate ( NoahmpIO%FPICEXY      (XSTART:XEND,YSTART:YEND) )
    allocate ( NoahmpIO%RAINLSM      (XSTART:XEND,YSTART:YEND) )
    allocate ( NoahmpIO%SNOWLSM      (XSTART:XEND,YSTART:YEND) )
    allocate ( NoahmpIO%FORCTLSM     (XSTART:XEND,YSTART:YEND) )
    allocate ( NoahmpIO%FORCQLSM     (XSTART:XEND,YSTART:YEND) )
    allocate ( NoahmpIO%FORCPLSM     (XSTART:XEND,YSTART:YEND) )
    allocate ( NoahmpIO%FORCZLSM     (XSTART:XEND,YSTART:YEND) )
    allocate ( NoahmpIO%FORCWLSM     (XSTART:XEND,YSTART:YEND) )
    allocate ( NoahmpIO%ACC_SSOILXY  (XSTART:XEND,YSTART:YEND) )
    allocate ( NoahmpIO%ACC_QINSURXY (XSTART:XEND,YSTART:YEND) )
    allocate ( NoahmpIO%ACC_QSEVAXY  (XSTART:XEND,YSTART:YEND) )
    allocate ( NoahmpIO%ACC_ETRANIXY (XSTART:XEND,1:NSOIL,YSTART:YEND) )
    allocate ( NoahmpIO%EFLXBXY      (XSTART:XEND,YSTART:YEND) )
    allocate ( NoahmpIO%SOILENERGY   (XSTART:XEND,YSTART:YEND) )
    allocate ( NoahmpIO%SNOWENERGY   (XSTART:XEND,YSTART:YEND) )
    allocate ( NoahmpIO%CANHSXY      (XSTART:XEND,YSTART:YEND) )            ! canopy heat storage change [W/m2]
    allocate ( NoahmpIO%ACC_DWATERXY (XSTART:XEND,YSTART:YEND) )
    allocate ( NoahmpIO%ACC_PRCPXY   (XSTART:XEND,YSTART:YEND) )
    allocate ( NoahmpIO%ACC_ECANXY   (XSTART:XEND,YSTART:YEND) )
    allocate ( NoahmpIO%ACC_ETRANXY  (XSTART:XEND,YSTART:YEND) )
    allocate ( NoahmpIO%ACC_EDIRXY   (XSTART:XEND,YSTART:YEND) )

!------------------------------------------------------------------------
! Needed for MMF_RUNOFF (IOPT_RUN = 5); not part of MP driver in WRF
!------------------------------------------------------------------------

    allocate ( NoahmpIO%MSFTX        (XSTART:XEND,YSTART:YEND) )  ! 
    allocate ( NoahmpIO%MSFTY        (XSTART:XEND,YSTART:YEND) )  ! 
    allocate ( NoahmpIO%EQZWT        (XSTART:XEND,YSTART:YEND) )  ! 
    allocate ( NoahmpIO%RIVERBEDXY   (XSTART:XEND,YSTART:YEND) )  ! 
    allocate ( NoahmpIO%RIVERCONDXY  (XSTART:XEND,YSTART:YEND) )  ! 
    allocate ( NoahmpIO%PEXPXY       (XSTART:XEND,YSTART:YEND) )  ! 
    allocate ( NoahmpIO%FDEPTHXY     (XSTART:XEND,YSTART:YEND) )  ! 
    allocate ( NoahmpIO%AREAXY       (XSTART:XEND,YSTART:YEND) )  ! 
    allocate ( NoahmpIO%QRFSXY       (XSTART:XEND,YSTART:YEND) )  ! 
    allocate ( NoahmpIO%QSPRINGSXY   (XSTART:XEND,YSTART:YEND) )  ! 
    allocate ( NoahmpIO%QRFXY        (XSTART:XEND,YSTART:YEND) )  ! 
    allocate ( NoahmpIO%QSPRINGXY    (XSTART:XEND,YSTART:YEND) )  ! 
    allocate ( NoahmpIO%QSLATXY      (XSTART:XEND,YSTART:YEND) )  ! 
    allocate ( NoahmpIO%QLATXY       (XSTART:XEND,YSTART:YEND) )  !
    allocate ( NoahmpIO%RECHCLIM     (XSTART:XEND,YSTART:YEND) )  ! 
    allocate ( NoahmpIO%RIVERMASK    (XSTART:XEND,YSTART:YEND) )  ! 
    allocate ( NoahmpIO%NONRIVERXY   (XSTART:XEND,YSTART:YEND) )  ! 

!------------------------------------------------------------------------
! Needed for crop model (OPT_CROP=1)
!------------------------------------------------------------------------

    allocate ( NoahmpIO%PGSXY        (XSTART:XEND,  YSTART:YEND) )
    allocate ( NoahmpIO%CROPCAT      (XSTART:XEND,  YSTART:YEND) )
    allocate ( NoahmpIO%PLANTING     (XSTART:XEND,  YSTART:YEND) )
    allocate ( NoahmpIO%HARVEST      (XSTART:XEND,  YSTART:YEND) )
    allocate ( NoahmpIO%SEASON_GDD   (XSTART:XEND,  YSTART:YEND) )
    allocate ( NoahmpIO%CROPTYPE     (XSTART:XEND,5,YSTART:YEND) )

!------------------------------------------------------------------------
! Single- and Multi-layer Urban Models
!------------------------------------------------------------------------

    if(NoahmpIO%SF_URBAN_PHYSICS > 0 )  then  ! any urban model

       allocate ( NoahmpIO%sh_urb2d       (XSTART:XEND,                 YSTART:YEND) )  ! 
       allocate ( NoahmpIO%lh_urb2d       (XSTART:XEND,                 YSTART:YEND) )  ! 
       allocate ( NoahmpIO%g_urb2d        (XSTART:XEND,                 YSTART:YEND) )  ! 
       allocate ( NoahmpIO%rn_urb2d       (XSTART:XEND,                 YSTART:YEND) )  ! 
       allocate ( NoahmpIO%ts_urb2d       (XSTART:XEND,                 YSTART:YEND) )  ! 
       allocate ( NoahmpIO%HRANG          (XSTART:XEND,                 YSTART:YEND) )  !
       allocate ( NoahmpIO%frc_urb2d      (XSTART:XEND,                 YSTART:YEND) )  ! 
       allocate ( NoahmpIO%utype_urb2d    (XSTART:XEND,                 YSTART:YEND) )  ! 
       allocate ( NoahmpIO%lp_urb2d       (XSTART:XEND,                 YSTART:YEND) )  !
       allocate ( NoahmpIO%lb_urb2d       (XSTART:XEND,                 YSTART:YEND) )  !
       allocate ( NoahmpIO%hgt_urb2d      (XSTART:XEND,                 YSTART:YEND) )  !
       allocate ( NoahmpIO%ust            (XSTART:XEND,                 YSTART:YEND) )  !

       !ENDIF
         
       !IF(SF_URBAN_PHYSICS == 1 ) THEN  ! single layer urban model
         
       allocate ( NoahmpIO%cmr_sfcdif     (XSTART:XEND,                 YSTART:YEND) )  ! 
       allocate ( NoahmpIO%chr_sfcdif     (XSTART:XEND,                 YSTART:YEND) )  ! 
       allocate ( NoahmpIO%cmc_sfcdif     (XSTART:XEND,                 YSTART:YEND) )  ! 
       allocate ( NoahmpIO%chc_sfcdif     (XSTART:XEND,                 YSTART:YEND) )  ! 
       allocate ( NoahmpIO%cmgr_sfcdif    (XSTART:XEND,                 YSTART:YEND) )  ! 
       allocate ( NoahmpIO%chgr_sfcdif    (XSTART:XEND,                 YSTART:YEND) )  ! 
       allocate ( NoahmpIO%tr_urb2d       (XSTART:XEND,                 YSTART:YEND) )  ! 
       allocate ( NoahmpIO%tb_urb2d       (XSTART:XEND,                 YSTART:YEND) )  ! 
       allocate ( NoahmpIO%tg_urb2d       (XSTART:XEND,                 YSTART:YEND) )  ! 
       allocate ( NoahmpIO%tc_urb2d       (XSTART:XEND,                 YSTART:YEND) )  ! 
       allocate ( NoahmpIO%qc_urb2d       (XSTART:XEND,                 YSTART:YEND) )  ! 
       allocate ( NoahmpIO%uc_urb2d       (XSTART:XEND,                 YSTART:YEND) )  ! 
       allocate ( NoahmpIO%xxxr_urb2d     (XSTART:XEND,                 YSTART:YEND) )  ! 
       allocate ( NoahmpIO%xxxb_urb2d     (XSTART:XEND,                 YSTART:YEND) )  ! 
       allocate ( NoahmpIO%xxxg_urb2d     (XSTART:XEND,                 YSTART:YEND) )  ! 
       allocate ( NoahmpIO%xxxc_urb2d     (XSTART:XEND,                 YSTART:YEND) )  ! 
       allocate ( NoahmpIO%trl_urb3d      (XSTART:XEND, nsoil,          YSTART:YEND) )  ! 
       allocate ( NoahmpIO%tbl_urb3d      (XSTART:XEND, nsoil,          YSTART:YEND) )  ! 
       allocate ( NoahmpIO%tgl_urb3d      (XSTART:XEND, nsoil,          YSTART:YEND) )  ! 
       allocate ( NoahmpIO%psim_urb2d     (XSTART:XEND,                 YSTART:YEND) )  ! 
       allocate ( NoahmpIO%psih_urb2d     (XSTART:XEND,                 YSTART:YEND) )  ! 
       allocate ( NoahmpIO%u10_urb2d      (XSTART:XEND,                 YSTART:YEND) )  ! 
       allocate ( NoahmpIO%v10_urb2d      (XSTART:XEND,                 YSTART:YEND) )  ! 
       allocate ( NoahmpIO%GZ1OZ0_urb2d   (XSTART:XEND,                 YSTART:YEND) )  ! 
       allocate ( NoahmpIO%AKMS_URB2D     (XSTART:XEND,                 YSTART:YEND) )  ! 
       allocate ( NoahmpIO%th2_urb2d      (XSTART:XEND,                 YSTART:YEND) )  ! 
       allocate ( NoahmpIO%q2_urb2d       (XSTART:XEND,                 YSTART:YEND) )  ! 
       allocate ( NoahmpIO%ust_urb2d      (XSTART:XEND,                 YSTART:YEND) )  ! 
       allocate ( NoahmpIO%dzr            (             nsoil                      ) )  !
       allocate ( NoahmpIO%dzb            (             nsoil                      ) )  !
       allocate ( NoahmpIO%dzg            (             nsoil                      ) )  !
       allocate ( NoahmpIO%cmcr_urb2d     (XSTART:XEND,                 YSTART:YEND) )  ! 
       allocate ( NoahmpIO%tgr_urb2d      (XSTART:XEND,                 YSTART:YEND) )  ! 
       allocate ( NoahmpIO%tgrl_urb3d     (XSTART:XEND, nsoil,          YSTART:YEND) )  ! 
       allocate ( NoahmpIO%smr_urb3d      (XSTART:XEND, nsoil,          YSTART:YEND) )  ! 
       allocate ( NoahmpIO%drelr_urb2d    (XSTART:XEND,                 YSTART:YEND) )  ! 
       allocate ( NoahmpIO%drelb_urb2d    (XSTART:XEND,                 YSTART:YEND) )  ! 
       allocate ( NoahmpIO%drelg_urb2d    (XSTART:XEND,                 YSTART:YEND) )  ! 
       allocate ( NoahmpIO%flxhumr_urb2d  (XSTART:XEND,                 YSTART:YEND) )  ! 
       allocate ( NoahmpIO%flxhumb_urb2d  (XSTART:XEND,                 YSTART:YEND) )  ! 
       allocate ( NoahmpIO%flxhumg_urb2d  (XSTART:XEND,                 YSTART:YEND) )  ! 
       allocate ( NoahmpIO%chs            (XSTART:XEND,                 YSTART:YEND) )  ! 
       allocate ( NoahmpIO%chs2           (XSTART:XEND,                 YSTART:YEND) )  ! 
       allocate ( NoahmpIO%cqs2           (XSTART:XEND,                 YSTART:YEND) )  ! 
       allocate ( NoahmpIO%mh_urb2d       (XSTART:XEND,                 YSTART:YEND) )  !
       allocate ( NoahmpIO%stdh_urb2d     (XSTART:XEND,                 YSTART:YEND) )  !
       allocate ( NoahmpIO%lf_urb2d       (XSTART:XEND, 4,              YSTART:YEND) )  !

       !ENDIF

       !IF(SF_URBAN_PHYSICS == 2 .or. SF_URBAN_PHYSICS == 3) THEN  ! BEP or BEM urban models
         
       allocate ( NoahmpIO%trb_urb4d      (XSTART:XEND,NoahmpIO%urban_map_zrd,YSTART:YEND) )  !
       allocate ( NoahmpIO%tw1_urb4d      (XSTART:XEND,NoahmpIO%urban_map_zwd,YSTART:YEND) )  !
       allocate ( NoahmpIO%tw2_urb4d      (XSTART:XEND,NoahmpIO%urban_map_zwd,YSTART:YEND) )  !
       allocate ( NoahmpIO%tgb_urb4d      (XSTART:XEND,NoahmpIO%urban_map_gd ,YSTART:YEND) )  !
       allocate ( NoahmpIO%sfw1_urb3d     (XSTART:XEND,NoahmpIO%urban_map_zd ,YSTART:YEND) )  !
       allocate ( NoahmpIO%sfw2_urb3d     (XSTART:XEND,NoahmpIO%urban_map_zd ,YSTART:YEND) )  !
       allocate ( NoahmpIO%sfr_urb3d      (XSTART:XEND,NoahmpIO%urban_map_zdf,YSTART:YEND) )  !
       allocate ( NoahmpIO%sfg_urb3d      (XSTART:XEND,NoahmpIO%num_urban_ndm,YSTART:YEND) )  !
       allocate ( NoahmpIO%hi_urb2d       (XSTART:XEND,NoahmpIO%num_urban_hi, YSTART:YEND) )  !

       allocate ( NoahmpIO%theta_urban    (XSTART:XEND,KDS:KDE,         YSTART:YEND) )  !
       allocate ( NoahmpIO%    u_urban    (XSTART:XEND,KDS:KDE,         YSTART:YEND) )  !
       allocate ( NoahmpIO%    v_urban    (XSTART:XEND,KDS:KDE,         YSTART:YEND) )  !
       allocate ( NoahmpIO%   dz_urban    (XSTART:XEND,KDS:KDE,         YSTART:YEND) )  !
       allocate ( NoahmpIO%  rho_urban    (XSTART:XEND,KDS:KDE,         YSTART:YEND) )  !
       allocate ( NoahmpIO%    p_urban    (XSTART:XEND,KDS:KDE,         YSTART:YEND) )  !

       allocate ( NoahmpIO%a_u_bep        (XSTART:XEND,KDS:KDE,         YSTART:YEND) )  !
       allocate ( NoahmpIO%a_v_bep        (XSTART:XEND,KDS:KDE,         YSTART:YEND) )  !
       allocate ( NoahmpIO%a_t_bep        (XSTART:XEND,KDS:KDE,         YSTART:YEND) )  !
       allocate ( NoahmpIO%a_q_bep        (XSTART:XEND,KDS:KDE,         YSTART:YEND) )  !
       allocate ( NoahmpIO%a_e_bep        (XSTART:XEND,KDS:KDE,         YSTART:YEND) )  !
       allocate ( NoahmpIO%b_u_bep        (XSTART:XEND,KDS:KDE,         YSTART:YEND) )  !
       allocate ( NoahmpIO%b_v_bep        (XSTART:XEND,KDS:KDE,         YSTART:YEND) )  !
       allocate ( NoahmpIO%b_t_bep        (XSTART:XEND,KDS:KDE,         YSTART:YEND) )  !
       allocate ( NoahmpIO%b_q_bep        (XSTART:XEND,KDS:KDE,         YSTART:YEND) )  !
       allocate ( NoahmpIO%b_e_bep        (XSTART:XEND,KDS:KDE,         YSTART:YEND) )  !
       allocate ( NoahmpIO%dlg_bep        (XSTART:XEND,KDS:KDE,         YSTART:YEND) )  !
       allocate ( NoahmpIO%dl_u_bep       (XSTART:XEND,KDS:KDE,         YSTART:YEND) )  !
       allocate ( NoahmpIO%sf_bep         (XSTART:XEND,KDS:KDE,         YSTART:YEND) )  !
       allocate ( NoahmpIO%vl_bep         (XSTART:XEND,KDS:KDE,         YSTART:YEND) )  !

       !ENDIF

        !IF(SF_URBAN_PHYSICS == 3) THEN  ! BEM urban model
         
       allocate ( NoahmpIO%tlev_urb3d     (XSTART:XEND,NoahmpIO%urban_map_bd  ,YSTART:YEND) )  !
       allocate ( NoahmpIO%qlev_urb3d     (XSTART:XEND,NoahmpIO%urban_map_bd  ,YSTART:YEND) )  !
       allocate ( NoahmpIO%tw1lev_urb3d   (XSTART:XEND,NoahmpIO%urban_map_wd  ,YSTART:YEND) )  !
       allocate ( NoahmpIO%tw2lev_urb3d   (XSTART:XEND,NoahmpIO%urban_map_wd  ,YSTART:YEND) )  !
       allocate ( NoahmpIO%tglev_urb3d    (XSTART:XEND,NoahmpIO%urban_map_gbd ,YSTART:YEND) )  !
       allocate ( NoahmpIO%tflev_urb3d    (XSTART:XEND,NoahmpIO%urban_map_fbd ,YSTART:YEND) )  !
       allocate ( NoahmpIO%sf_ac_urb3d    (XSTART:XEND,                        YSTART:YEND) )  !
       allocate ( NoahmpIO%lf_ac_urb3d    (XSTART:XEND,                        YSTART:YEND) )  !
       allocate ( NoahmpIO%cm_ac_urb3d    (XSTART:XEND,                        YSTART:YEND) )  !
       allocate ( NoahmpIO%sfvent_urb3d   (XSTART:XEND,                        YSTART:YEND) )  !
       allocate ( NoahmpIO%lfvent_urb3d   (XSTART:XEND,                        YSTART:YEND) )  !
       allocate ( NoahmpIO%sfwin1_urb3d   (XSTART:XEND,NoahmpIO%urban_map_wd  ,YSTART:YEND) )  !
       allocate ( NoahmpIO%sfwin2_urb3d   (XSTART:XEND,NoahmpIO%urban_map_wd  ,YSTART:YEND) )  !
       ! new urban variables greenroof & solar panel for BEM
       allocate ( NoahmpIO%ep_pv_urb3d    (XSTART:XEND,                        YSTART:YEND) )  !
       allocate ( NoahmpIO%t_pv_urb3d     (XSTART:XEND,NoahmpIO%urban_map_zdf ,YSTART:YEND) )  !
       allocate ( NoahmpIO%trv_urb4d      (XSTART:XEND,NoahmpIO%urban_map_zgrd,YSTART:YEND) ) !
       allocate ( NoahmpIO%qr_urb4d       (XSTART:XEND,NoahmpIO%urban_map_zgrd,YSTART:YEND) ) !
       allocate ( NoahmpIO%qgr_urb3d      (XSTART:XEND,                        YSTART:YEND) )  !
       allocate ( NoahmpIO%tgr_urb3d      (XSTART:XEND,                        YSTART:YEND) )  !
       allocate ( NoahmpIO%drain_urb4d    (XSTART:XEND,NoahmpIO%urban_map_zdf ,YSTART:YEND) )  !
       allocate ( NoahmpIO%draingr_urb3d  (XSTART:XEND,                        YSTART:YEND) )  !
       allocate ( NoahmpIO%sfrv_urb3d     (XSTART:XEND,NoahmpIO%urban_map_zdf ,YSTART:YEND) )  !
       allocate ( NoahmpIO%lfrv_urb3d     (XSTART:XEND,NoahmpIO%urban_map_zdf ,YSTART:YEND) )  !
       allocate ( NoahmpIO%dgr_urb3d      (XSTART:XEND,NoahmpIO%urban_map_zdf ,YSTART:YEND) )  !
       allocate ( NoahmpIO%dg_urb3d       (XSTART:XEND,NoahmpIO%num_urban_ndm ,YSTART:YEND) )  !
       allocate ( NoahmpIO%lfr_urb3d      (XSTART:XEND,NoahmpIO%urban_map_zdf ,YSTART:YEND) )  !
       allocate ( NoahmpIO%lfg_urb3d      (XSTART:XEND,NoahmpIO%num_urban_ndm ,YSTART:YEND) )  !

    endif

!------------------------------------------------------------------------

    allocate ( NoahmpIO%CHSTARXY  (XSTART:XEND,YSTART:YEND) )  ! for consistency with MP_init; delete later
    allocate ( NoahmpIO%SEAICE    (XSTART:XEND,YSTART:YEND) )  ! seaice fraction
    
#ifdef WRF_HYDRO
    allocate (NoahmpIO%infxsrt    (XSTART:XEND,YSTART:YEND) )
    allocate (NoahmpIO%sfcheadrt  (XSTART:XEND,YSTART:YEND) )
    allocate (NoahmpIO%soldrain   (XSTART:XEND,YSTART:YEND) )
    allocate (NoahmpIO%qtiledrain (XSTART:XEND,YSTART:YEND) )
    allocate (NoahmpIO%ZWATBLE2D  (XSTART:XEND,YSTART:YEND) )
#endif    

    end associate  
    
    !-------------------------------------------------------------------
    ! Initialize variables with default values 
    !-------------------------------------------------------------------
    
    NoahmpIO%ICE             = 0
    NoahmpIO%COSZEN          = undefined_real
    NoahmpIO%XLAT            = undefined_real
    NoahmpIO%DZ8W            = undefined_real
    NoahmpIO%DZS             = undefined_real
    NoahmpIO%ZSOIL           = undefined_real
    NoahmpIO%IVGTYP          = undefined_int
    NoahmpIO%ISLTYP          = undefined_int
    NoahmpIO%SOILCL1         = undefined_real
    NoahmpIO%SOILCL2         = undefined_real
    NoahmpIO%SOILCL3         = undefined_real
    NoahmpIO%SOILCL4         = undefined_real
    NoahmpIO%SOILCOMP        = undefined_real
    NoahmpIO%VEGFRA          = undefined_real
    NoahmpIO%TMN             = undefined_real
    NoahmpIO%XLAND           = undefined_real
    NoahmpIO%XICE            = undefined_real
    NoahmpIO%T_PHY           = undefined_real
    NoahmpIO%QV_CURR         = undefined_real
    NoahmpIO%U_PHY           = undefined_real
    NoahmpIO%V_PHY           = undefined_real
    NoahmpIO%SWDOWN          = undefined_real
    NoahmpIO%SWDDIR          = undefined_real
    NoahmpIO%SWDDIF          = undefined_real
    NoahmpIO%GLW             = undefined_real
    NoahmpIO%P8W             = undefined_real
    NoahmpIO%RAINBL          = undefined_real
    NoahmpIO%SNOWBL          = undefined_real
    NoahmpIO%RAINBL_tmp      = undefined_real
    NoahmpIO%SR              = undefined_real
    NoahmpIO%RAINCV          = undefined_real
    NoahmpIO%RAINNCV         = undefined_real
    NoahmpIO%RAINSHV         = undefined_real
    NoahmpIO%SNOWNCV         = undefined_real
    NoahmpIO%GRAUPELNCV      = undefined_real
    NoahmpIO%HAILNCV         = undefined_real
    NoahmpIO%MP_RAINC        = 0.0
    NoahmpIO%MP_RAINNC       = 0.0
    NoahmpIO%MP_SHCV         = 0.0
    NoahmpIO%MP_SNOW         = 0.0
    NoahmpIO%MP_GRAUP        = 0.0
    NoahmpIO%MP_HAIL         = 0.0
    NoahmpIO%TSK             = undefined_real
    NoahmpIO%QFX             = undefined_real
    NoahmpIO%SMSTAV          = undefined_real
    NoahmpIO%SMSTOT          = undefined_real
    NoahmpIO%SMOIS           = undefined_real
    NoahmpIO%SH2O            = undefined_real
    NoahmpIO%TSLB            = undefined_real
    NoahmpIO%SNOW            = undefined_real
    NoahmpIO%SNOWH           = undefined_real
    NoahmpIO%CANWAT          = undefined_real
    NoahmpIO%ACSNOM          = 0.0
    NoahmpIO%ACSNOW          = 0.0
    NoahmpIO%QSFC            = undefined_real
    NoahmpIO%SFCRUNOFF       = 0.0
    NoahmpIO%UDRUNOFF        = 0.0
    NoahmpIO%SMOISEQ         = undefined_real
    NoahmpIO%ALBEDO          = undefined_real
    NoahmpIO%ISNOWXY         = undefined_int
    NoahmpIO%TVXY            = undefined_real
    NoahmpIO%TGXY            = undefined_real
    NoahmpIO%CANICEXY        = undefined_real
    NoahmpIO%CANLIQXY        = undefined_real
    NoahmpIO%EAHXY           = undefined_real
    NoahmpIO%TAHXY           = undefined_real
    NoahmpIO%CMXY            = undefined_real
    NoahmpIO%CHXY            = undefined_real
    NoahmpIO%FWETXY          = undefined_real
    NoahmpIO%SNEQVOXY        = undefined_real
    NoahmpIO%ALBOLDXY        = undefined_real
    NoahmpIO%QSNOWXY         = undefined_real
    NoahmpIO%QRAINXY         = undefined_real
    NoahmpIO%WSLAKEXY        = undefined_real
    NoahmpIO%ZWTXY           = undefined_real
    NoahmpIO%WAXY            = undefined_real
    NoahmpIO%WTXY            = undefined_real
    NoahmpIO%TSNOXY          = undefined_real
    NoahmpIO%SNICEXY         = undefined_real
    NoahmpIO%SNLIQXY         = undefined_real
    NoahmpIO%LFMASSXY        = undefined_real
    NoahmpIO%RTMASSXY        = undefined_real
    NoahmpIO%STMASSXY        = undefined_real
    NoahmpIO%WOODXY          = undefined_real
    NoahmpIO%STBLCPXY        = undefined_real
    NoahmpIO%FASTCPXY        = undefined_real
    NoahmpIO%LAI             = undefined_real
    NoahmpIO%LAI_tmp         = undefined_real
    NoahmpIO%XSAIXY          = undefined_real
    NoahmpIO%TAUSSXY         = 0.0
    NoahmpIO%XLONG           = undefined_real
    NoahmpIO%SEAICE          = undefined_real
    NoahmpIO%SMCWTDXY        = undefined_real
    NoahmpIO%DEEPRECHXY      = 0.0
    NoahmpIO%RECHXY          = 0.0
    NoahmpIO%ZSNSOXY         = undefined_real
    NoahmpIO%GRDFLX          = undefined_real
    NoahmpIO%HFX             = undefined_real
    NoahmpIO%LH              = undefined_real
    NoahmpIO%EMISS           = undefined_real
    NoahmpIO%SNOWC           = undefined_real
    NoahmpIO%T2MVXY          = undefined_real
    NoahmpIO%T2MBXY          = undefined_real
    NoahmpIO%Q2MVXY          = undefined_real
    NoahmpIO%Q2MBXY          = undefined_real
    NoahmpIO%TRADXY          = undefined_real
    NoahmpIO%NEEXY           = undefined_real
    NoahmpIO%GPPXY           = undefined_real
    NoahmpIO%NPPXY           = undefined_real
    NoahmpIO%FVEGXY          = undefined_real
    NoahmpIO%RUNSFXY         = undefined_real
    NoahmpIO%RUNSBXY         = undefined_real
    NoahmpIO%ECANXY          = undefined_real
    NoahmpIO%EDIRXY          = undefined_real
    NoahmpIO%ETRANXY         = undefined_real
    NoahmpIO%FSAXY           = undefined_real
    NoahmpIO%FIRAXY          = undefined_real
    NoahmpIO%APARXY          = undefined_real
    NoahmpIO%PSNXY           = undefined_real
    NoahmpIO%SAVXY           = undefined_real
    NoahmpIO%SAGXY           = undefined_real
    NoahmpIO%RSSUNXY         = undefined_real
    NoahmpIO%RSSHAXY         = undefined_real
    NoahmpIO%BGAPXY          = undefined_real
    NoahmpIO%WGAPXY          = undefined_real
    NoahmpIO%TGVXY           = undefined_real
    NoahmpIO%TGBXY           = undefined_real
    NoahmpIO%CHVXY           = undefined_real
    NoahmpIO%CHBXY           = undefined_real
    NoahmpIO%SHGXY           = undefined_real
    NoahmpIO%SHCXY           = undefined_real
    NoahmpIO%SHBXY           = undefined_real
    NoahmpIO%EVGXY           = undefined_real
    NoahmpIO%EVBXY           = undefined_real
    NoahmpIO%GHVXY           = undefined_real
    NoahmpIO%GHBXY           = undefined_real
    NoahmpIO%IRGXY           = undefined_real
    NoahmpIO%IRCXY           = undefined_real
    NoahmpIO%IRBXY           = undefined_real
    NoahmpIO%TRXY            = undefined_real
    NoahmpIO%EVCXY           = undefined_real
    NoahmpIO%CHLEAFXY        = undefined_real
    NoahmpIO%CHUCXY          = undefined_real
    NoahmpIO%CHV2XY          = undefined_real
    NoahmpIO%CHB2XY          = undefined_real
    NoahmpIO%RS              = undefined_real
    NoahmpIO%CANHSXY         = undefined_real
    ! additional output
    NoahmpIO%PAHXY           = undefined_real
    NoahmpIO%PAHGXY          = undefined_real
    NoahmpIO%PAHBXY          = undefined_real
    NoahmpIO%PAHVXY          = undefined_real
    NoahmpIO%QINTSXY         = undefined_real
    NoahmpIO%QINTRXY         = undefined_real
    NoahmpIO%QDRIPSXY        = undefined_real
    NoahmpIO%QDRIPRXY        = undefined_real
    NoahmpIO%QTHROSXY        = undefined_real
    NoahmpIO%QTHRORXY        = undefined_real
    NoahmpIO%QSNSUBXY        = undefined_real
    NoahmpIO%QSNFROXY        = undefined_real
    NoahmpIO%QSUBCXY         = undefined_real
    NoahmpIO%QFROCXY         = undefined_real
    NoahmpIO%QEVACXY         = undefined_real
    NoahmpIO%QDEWCXY         = undefined_real
    NoahmpIO%QFRZCXY         = undefined_real
    NoahmpIO%QMELTCXY        = undefined_real
    NoahmpIO%QSNBOTXY        = undefined_real
    NoahmpIO%QMELTXY         = undefined_real
    NoahmpIO%FPICEXY         = undefined_real
    NoahmpIO%RAINLSM         = undefined_real
    NoahmpIO%SNOWLSM         = undefined_real
    NoahmpIO%FORCTLSM        = undefined_real
    NoahmpIO%FORCQLSM        = undefined_real
    NoahmpIO%FORCPLSM        = undefined_real
    NoahmpIO%FORCZLSM        = undefined_real
    NoahmpIO%FORCWLSM        = undefined_real
    NoahmpIO%EFLXBXY         = undefined_real
    NoahmpIO%SOILENERGY      = undefined_real
    NoahmpIO%SNOWENERGY      = undefined_real
    NoahmpIO%PONDINGXY       = 0.0
    NoahmpIO%ACC_SSOILXY     = 0.0
    NoahmpIO%ACC_QINSURXY    = 0.0
    NoahmpIO%ACC_QSEVAXY     = 0.0
    NoahmpIO%ACC_ETRANIXY    = 0.0
    NoahmpIO%ACC_DWATERXY    = 0.0
    NoahmpIO%ACC_PRCPXY      = 0.0
    NoahmpIO%ACC_ECANXY      = 0.0
    NoahmpIO%ACC_ETRANXY     = 0.0
    NoahmpIO%ACC_EDIRXY      = 0.0

    NoahmpIO%TERRAIN         = undefined_real
    NoahmpIO%GVFMIN          = undefined_real
    NoahmpIO%GVFMAX          = undefined_real
    NoahmpIO%MSFTX           = undefined_real
    NoahmpIO%MSFTY           = undefined_real
    NoahmpIO%EQZWT           = undefined_real
    NoahmpIO%RIVERBEDXY      = undefined_real
    NoahmpIO%RIVERCONDXY     = undefined_real
    NoahmpIO%PEXPXY          = undefined_real
    NoahmpIO%FDEPTHXY        = undefined_real
    NoahmpIO%AREAXY          = undefined_real
    NoahmpIO%QRFSXY          = undefined_real
    NoahmpIO%QSPRINGSXY      = undefined_real
    NoahmpIO%QRFXY           = undefined_real
    NoahmpIO%QSPRINGXY       = undefined_real
    NoahmpIO%QSLATXY         = undefined_real
    NoahmpIO%QLATXY          = undefined_real
    NoahmpIO%CHSTARXY        = undefined_real
    NoahmpIO%Z0              = undefined_real
    NoahmpIO%ZNT             = undefined_real
    NoahmpIO%PGSXY           = undefined_int
    NoahmpIO%CROPCAT         = undefined_int
    NoahmpIO%PLANTING        = undefined_real
    NoahmpIO%HARVEST         = undefined_real
    NoahmpIO%SEASON_GDD      = undefined_real
    NoahmpIO%CROPTYPE        = undefined_real

    ! tile drainage
    NoahmpIO%QTDRAIN         = 0.0
    NoahmpIO%TD_FRACTION     = undefined_real

    ! irrigation
    NoahmpIO%IRFRACT         = 0.0
    NoahmpIO%SIFRACT         = 0.0
    NoahmpIO%MIFRACT         = 0.0
    NoahmpIO%FIFRACT         = 0.0
    NoahmpIO%IRNUMSI         = 0
    NoahmpIO%IRNUMMI         = 0
    NoahmpIO%IRNUMFI         = 0
    NoahmpIO%IRWATSI         = 0.0
    NoahmpIO%IRWATMI         = 0.0
    NoahmpIO%IRWATFI         = 0.0
    NoahmpIO%IRELOSS         = 0.0
    NoahmpIO%IRSIVOL         = 0.0
    NoahmpIO%IRMIVOL         = 0.0
    NoahmpIO%IRFIVOL         = 0.0
    NoahmpIO%IRRSPLH         = 0.0
    NoahmpIO%LOCTIM          = undefined_real
 
    if ( NoahmpIO%SF_URBAN_PHYSICS > 0 )then  ! any urban model
      NoahmpIO%HRANG         = undefined_real
      NoahmpIO%DECLIN        = undefined_real
      NoahmpIO%sh_urb2d      = undefined_real
      NoahmpIO%lh_urb2d      = undefined_real
      NoahmpIO%g_urb2d       = undefined_real
      NoahmpIO%rn_urb2d      = undefined_real
      NoahmpIO%ts_urb2d      = undefined_real
      NoahmpIO%GMT           = undefined_real
      NoahmpIO%JULDAY        = undefined_int
      NoahmpIO%IRI_URBAN     = undefined_int
      NoahmpIO%frc_urb2d     = undefined_real
      NoahmpIO%utype_urb2d   = undefined_int
      NoahmpIO%lp_urb2d      = undefined_real
      NoahmpIO%lb_urb2d      = undefined_real
      NoahmpIO%hgt_urb2d     = undefined_real
      NoahmpIO%ust           = undefined_real
      NoahmpIO%cmr_sfcdif    = undefined_real
      NoahmpIO%chr_sfcdif    = undefined_real
      NoahmpIO%cmc_sfcdif    = undefined_real
      NoahmpIO%chc_sfcdif    = undefined_real
      NoahmpIO%cmgr_sfcdif   = undefined_real
      NoahmpIO%chgr_sfcdif   = undefined_real
      NoahmpIO%tr_urb2d      = undefined_real
      NoahmpIO%tb_urb2d      = undefined_real
      NoahmpIO%tg_urb2d      = undefined_real
      NoahmpIO%tc_urb2d      = undefined_real
      NoahmpIO%qc_urb2d      = undefined_real
      NoahmpIO%uc_urb2d      = undefined_real
      NoahmpIO%xxxr_urb2d    = undefined_real
      NoahmpIO%xxxb_urb2d    = undefined_real
      NoahmpIO%xxxg_urb2d    = undefined_real
      NoahmpIO%xxxc_urb2d    = undefined_real
      NoahmpIO%trl_urb3d     = undefined_real
      NoahmpIO%tbl_urb3d     = undefined_real
      NoahmpIO%tgl_urb3d     = undefined_real
      NoahmpIO%psim_urb2d    = undefined_real
      NoahmpIO%psih_urb2d    = undefined_real
      NoahmpIO%u10_urb2d     = undefined_real
      NoahmpIO%v10_urb2d     = undefined_real
      NoahmpIO%GZ1OZ0_urb2d  = undefined_real
      NoahmpIO%AKMS_URB2D    = undefined_real
      NoahmpIO%th2_urb2d     = undefined_real
      NoahmpIO%q2_urb2d      = undefined_real
      NoahmpIO%ust_urb2d     = undefined_real
      NoahmpIO%dzr           = undefined_real
      NoahmpIO%dzb           = undefined_real
      NoahmpIO%dzg           = undefined_real
      NoahmpIO%cmcr_urb2d    = undefined_real
      NoahmpIO%tgr_urb2d     = undefined_real
      NoahmpIO%tgrl_urb3d    = undefined_real
      NoahmpIO%smr_urb3d     = undefined_real
      NoahmpIO%drelr_urb2d   = undefined_real
      NoahmpIO%drelb_urb2d   = undefined_real
      NoahmpIO%drelg_urb2d   = undefined_real
      NoahmpIO%flxhumr_urb2d = undefined_real
      NoahmpIO%flxhumb_urb2d = undefined_real
      NoahmpIO%flxhumg_urb2d = undefined_real
      NoahmpIO%chs           = undefined_real
      NoahmpIO%chs2          = undefined_real
      NoahmpIO%cqs2          = undefined_real
      NoahmpIO%mh_urb2d      = undefined_real
      NoahmpIO%stdh_urb2d    = undefined_real
      NoahmpIO%lf_urb2d      = undefined_real
      NoahmpIO%trb_urb4d     = undefined_real
      NoahmpIO%tw1_urb4d     = undefined_real
      NoahmpIO%tw2_urb4d     = undefined_real
      NoahmpIO%tgb_urb4d     = undefined_real
      NoahmpIO%sfw1_urb3d    = undefined_real
      NoahmpIO%sfw2_urb3d    = undefined_real
      NoahmpIO%sfr_urb3d     = undefined_real
      NoahmpIO%sfg_urb3d     = undefined_real
      NoahmpIO%hi_urb2d      = undefined_real
      NoahmpIO%theta_urban   = undefined_real
      NoahmpIO%u_urban       = undefined_real
      NoahmpIO%v_urban       = undefined_real
      NoahmpIO%dz_urban      = undefined_real
      NoahmpIO%rho_urban     = undefined_real
      NoahmpIO%p_urban       = undefined_real
      NoahmpIO%a_u_bep       = undefined_real
      NoahmpIO%a_v_bep       = undefined_real
      NoahmpIO%a_t_bep       = undefined_real
      NoahmpIO%a_q_bep       = undefined_real
      NoahmpIO%a_e_bep       = undefined_real
      NoahmpIO%b_u_bep       = undefined_real
      NoahmpIO%b_v_bep       = undefined_real
      NoahmpIO%b_t_bep       = undefined_real
      NoahmpIO%b_q_bep       = undefined_real
      NoahmpIO%b_e_bep       = undefined_real
      NoahmpIO%dlg_bep       = undefined_real
      NoahmpIO%dl_u_bep      = undefined_real
      NoahmpIO%sf_bep        = undefined_real
      NoahmpIO%vl_bep        = undefined_real
      NoahmpIO%tlev_urb3d    = undefined_real
      NoahmpIO%qlev_urb3d    = undefined_real
      NoahmpIO%tw1lev_urb3d  = undefined_real
      NoahmpIO%tw2lev_urb3d  = undefined_real
      NoahmpIO%tglev_urb3d   = undefined_real
      NoahmpIO%tflev_urb3d   = undefined_real
      NoahmpIO%sf_ac_urb3d   = undefined_real
      NoahmpIO%lf_ac_urb3d   = undefined_real
      NoahmpIO%cm_ac_urb3d   = undefined_real
      NoahmpIO%sfvent_urb3d  = undefined_real
      NoahmpIO%lfvent_urb3d  = undefined_real
      NoahmpIO%sfwin1_urb3d  = undefined_real
      NoahmpIO%sfwin2_urb3d  = undefined_real
      NoahmpIO%ep_pv_urb3d   = undefined_real
      NoahmpIO%t_pv_urb3d    = undefined_real
      NoahmpIO%trv_urb4d     = undefined_real
      NoahmpIO%qr_urb4d      = undefined_real
      NoahmpIO%qgr_urb3d     = undefined_real
      NoahmpIO%tgr_urb3d     = undefined_real
      NoahmpIO%drain_urb4d   = undefined_real
      NoahmpIO%draingr_urb3d = undefined_real
      NoahmpIO%sfrv_urb3d    = undefined_real
      NoahmpIO%lfrv_urb3d    = undefined_real
      NoahmpIO%dgr_urb3d     = undefined_real
      NoahmpIO%dg_urb3d      = undefined_real
      NoahmpIO%lfr_urb3d     = undefined_real
      NoahmpIO%lfg_urb3d     = undefined_real
    endif

    NoahmpIO%XLAND             = 1.0      ! water = 2.0, land = 1.0
    NoahmpIO%XICE              = 0.0      ! fraction of grid that is seaice
    NoahmpIO%XICE_THRESHOLD    = 0.5      ! fraction of grid determining seaice (from WRF)
    NoahmpIO%SLOPETYP          = 1        ! soil parameter slope type
    NoahmpIO%soil_update_steps = 1        ! number of model time step to update soil proces
    NoahmpIO%calculate_soil    = .false.  ! index for if do soil process
    NoahmpIO%ITIMESTEP         = 0        ! model time step count

#ifdef WRF_HYDRO
    NoahmpIO%infxsrt         = 0.0
    NoahmpIO%sfcheadrt       = 0.0 
    NoahmpIO%soldrain        = 0.0
    NoahmpIO%qtiledrain      = 0.0
    NoahmpIO%ZWATBLE2D       = 0.0
#endif 
    
  end subroutine NoahmpIOVarInitDefault

end module NoahmpIOVarInitMod
