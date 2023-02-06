module NoahmpReadTableMod

!!! Initialize Noah-MP look-up table variables
!!! Table variables should be first defined in NoahmpIOVarType.F90

! ------------------------ Code history -----------------------------------
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (Jan 2023)
! -------------------------------------------------------------------------

  use Machine
  use NoahmpIOVarType

  implicit none

contains

!=== read Noahmp Table values

  subroutine NoahmpReadTable(NoahmpIO)

    implicit none

    type(NoahmpIO_type), intent(inout)  :: NoahmpIO

    !-------------------------------------------------------
    !=== define key dimensional variables
    !-------------------------------------------------------
    integer, parameter :: MVT         = 27   ! number of vegetation types
    integer, parameter :: MBAND       = 2    ! number of radiation bands
    integer, parameter :: MSC         = 8    ! number of soil texture
    integer, parameter :: MAX_SOILTYP = 30   ! max number of soil types
    integer, parameter :: NCROP       = 5    ! number of crop types
    integer, parameter :: NSTAGE      = 8    ! number of crop growth stages
    integer, parameter :: NUM_SLOPE   = 9    ! number of slope

    !-------------------------------------------------------
    !=== define local variables to store NoahmpTable values
    !-------------------------------------------------------
    
    ! vegetation parameters
    character(len=256)                     :: DATASET_IDENTIFIER
    character(len=256)                     :: VEG_DATASET_DESCRIPTION
    logical                                :: file_named
    integer                                :: ierr, IK, IM
    integer                                :: NVEG, ISURBAN, ISWATER, ISBARREN, ISICE, ISCROP, EBLFOREST, NATURAL
    integer                                :: LCZ_1, LCZ_2, LCZ_3, LCZ_4, LCZ_5, LCZ_6, LCZ_7, LCZ_8, LCZ_9, LCZ_10, LCZ_11
    real(kind=kind_noahmp), dimension(MVT) :: SAI_JAN, SAI_FEB, SAI_MAR, SAI_APR, SAI_MAY, SAI_JUN, SAI_JUL, SAI_AUG,        &
                                              SAI_SEP, SAI_OCT, SAI_NOV, SAI_DEC, LAI_JAN, LAI_FEB, LAI_MAR, LAI_APR,        &
                                              LAI_MAY, LAI_JUN, LAI_JUL, LAI_AUG, LAI_SEP, LAI_OCT, LAI_NOV, LAI_DEC,        &
                                              RHOL_VIS, RHOL_NIR, RHOS_VIS, RHOS_NIR, TAUL_VIS, TAUL_NIR, TAUS_VIS, TAUS_NIR,&
                                              CH2OP, DLEAF, Z0MVT, HVT, HVB, DEN, RC, MFSNO, SCFFAC, XL, CWPVT, C3PSN, KC25, &
                                              AKC, KO25, AKO, AVCMX, AQE, LTOVRC, DILEFC, DILEFW, RMF25, SLA, FRAGR, TMIN,   &
                                              VCMX25, TDLEF, BP, MP, QE25, RMS25, RMR25, ARM, FOLNMX, WDPOOL, WRRAT, MRP,    &
                                              NROOT, RGL, RS, HS, TOPT, RSMAX, RTOVRC, RSWOODC, BF, WSTRC, LAIMIN, CBIOM,    &
                                              XSAMIN, EPS1, EPS2, EPS3, EPS4, EPS5
    namelist / noahmp_usgs_veg_categories /   VEG_DATASET_DESCRIPTION, NVEG
    namelist / noahmp_usgs_parameters     /   ISURBAN, ISWATER, ISBARREN, ISICE, ISCROP, EBLFOREST, NATURAL,                 &
                                              LCZ_1, LCZ_2, LCZ_3, LCZ_4, LCZ_5, LCZ_6, LCZ_7, LCZ_8, LCZ_9, LCZ_10, LCZ_11, &
                                              CH2OP, DLEAF, Z0MVT, HVT, HVB, DEN, RC, MFSNO, SCFFAC, XL, CWPVT, C3PSN, KC25, &
                                              AKC, KO25, AKO, AVCMX, AQE, LTOVRC, DILEFC, DILEFW, RMF25, SLA, FRAGR, TMIN,   &
                                              VCMX25, TDLEF, BP, MP, QE25, RMS25, RMR25, ARM, FOLNMX, WDPOOL, WRRAT, MRP,    &
                                              NROOT, RGL, RS, HS, TOPT, RSMAX, RTOVRC, RSWOODC, BF, WSTRC, LAIMIN, CBIOM,    &
                                              XSAMIN, SAI_JAN, SAI_FEB, SAI_MAR, SAI_APR, SAI_MAY,                           &
                                              SAI_JUN, SAI_JUL, SAI_AUG, SAI_SEP, SAI_OCT, SAI_NOV, SAI_DEC, LAI_JAN,        &
                                              LAI_FEB, LAI_MAR, LAI_APR, LAI_MAY, LAI_JUN, LAI_JUL, LAI_AUG, LAI_SEP,        &
                                              LAI_OCT, LAI_NOV, LAI_DEC, RHOL_VIS, RHOL_NIR, RHOS_VIS, RHOS_NIR, TAUL_VIS,   &
                                              TAUL_NIR, TAUS_VIS, TAUS_NIR, EPS1, EPS2, EPS3, EPS4, EPS5
    namelist / noahmp_modis_veg_categories /  VEG_DATASET_DESCRIPTION, NVEG
    namelist / noahmp_modis_parameters     /  ISURBAN, ISWATER, ISBARREN, ISICE, ISCROP, EBLFOREST, NATURAL,                 &
                                              LCZ_1, LCZ_2, LCZ_3, LCZ_4, LCZ_5, LCZ_6, LCZ_7, LCZ_8, LCZ_9, LCZ_10, LCZ_11, &
                                              CH2OP, DLEAF, Z0MVT, HVT, HVB, DEN, RC, MFSNO, SCFFAC, XL, CWPVT, C3PSN, KC25, &
                                              AKC, KO25, AKO, AVCMX, AQE, LTOVRC, DILEFC, DILEFW, RMF25, SLA, FRAGR, TMIN,   &
                                              VCMX25, TDLEF, BP, MP, QE25, RMS25, RMR25, ARM, FOLNMX, WDPOOL, WRRAT, MRP,    &
                                              NROOT, RGL, RS, HS, TOPT, RSMAX, RTOVRC, RSWOODC, BF, WSTRC, LAIMIN, CBIOM,    &
                                              XSAMIN, SAI_JAN, SAI_FEB, SAI_MAR, SAI_APR, SAI_MAY,                           &
                                              SAI_JUN, SAI_JUL, SAI_AUG, SAI_SEP, SAI_OCT, SAI_NOV, SAI_DEC, LAI_JAN,        &
                                              LAI_FEB, LAI_MAR, LAI_APR, LAI_MAY, LAI_JUN, LAI_JUL, LAI_AUG, LAI_SEP,        &
                                              LAI_OCT, LAI_NOV, LAI_DEC, RHOL_VIS, RHOL_NIR, RHOS_VIS, RHOS_NIR, TAUL_VIS,   &
                                              TAUL_NIR, TAUS_VIS, TAUS_NIR, EPS1, EPS2, EPS3, EPS4, EPS5

    ! soil parameters
    character(len=256)                             :: message
    character(len=10)                              :: SLTYPE
    integer                                        :: SLCATS
    real(kind=kind_noahmp), dimension(MAX_SOILTYP) :: BB, DRYSMC, MAXSMC, REFSMC, SATPSI, SATDK, SATDW, WLTSMC, QTZ,    &
                                                      BVIC, AXAJ, BXAJ, XXAJ, BDVIC, BBVIC, GDVIC, HC
    namelist / noahmp_stas_soil_categories /          SLTYPE, SLCATS
    namelist / noahmp_soil_stas_parameters /          BB, DRYSMC, MAXSMC, REFSMC, SATPSI, SATDK, SATDW, WLTSMC, QTZ,    &
                                                      BVIC, AXAJ, BXAJ, XXAJ, BDVIC, BBVIC, GDVIC
    namelist / noahmp_soil_stas_ruc_parameters /      BB, DRYSMC, HC, MAXSMC, REFSMC, SATPSI, SATDK, SATDW, WLTSMC, QTZ,    &
                                                      BVIC, AXAJ, BXAJ, XXAJ, BDVIC, BBVIC, GDVIC

    ! general parameters
    real(kind=kind_noahmp)                       :: CSOIL_DATA, REFDK_DATA, REFKDT_DATA, FRZK_DATA, ZBOT_DATA, CZIL_DATA
    real(kind=kind_noahmp), dimension(NUM_SLOPE) :: SLOPE_DATA
    namelist / noahmp_general_parameters /          SLOPE_DATA, CSOIL_DATA, REFDK_DATA, REFKDT_DATA, FRZK_DATA, ZBOT_DATA,   &
                                                    CZIL_DATA

    ! radiation parameters
    real(kind=kind_noahmp)                   :: BETADS, BETAIS, EICE
    real(kind=kind_noahmp), dimension(MBAND) :: ALBICE, ALBLAK, OMEGAS 
    real(kind=kind_noahmp), dimension(2)     :: EG
    real(kind=kind_noahmp), dimension(MSC)   :: ALBSAT_VIS, ALBSAT_NIR, ALBDRY_VIS, ALBDRY_NIR
    namelist / noahmp_rad_parameters /          ALBSAT_VIS, ALBSAT_NIR, ALBDRY_VIS, ALBDRY_NIR, ALBICE, ALBLAK, OMEGAS,      &
                                                BETADS, BETAIS, EG, EICE

    ! global parameters
    real(kind=kind_noahmp)                   :: CO2, O2, TIMEAN, FSATMX, Z0SNO, SSI, SNOW_RET_FAC ,SNOW_EMIS, SWEMX, TAU0,   &
                                                GRAIN_GROWTH, EXTRA_GROWTH, DIRT_SOOT, BATS_COSZ, BATS_VIS_NEW,              &
                                                BATS_NIR_NEW, BATS_VIS_AGE, BATS_NIR_AGE, BATS_VIS_DIR, BATS_NIR_DIR,        &
                                                RSURF_SNOW, RSURF_EXP, C2_SNOWCOMPACT, C3_SNOWCOMPACT, C4_SNOWCOMPACT,       &
                                                C5_SNOWCOMPACT, DM_SNOWCOMPACT, ETA0_SNOWCOMPACT, SNLIQMAXFRAC, SWEMAXGLA,   &
                                                WSLMAX, ROUS, CMIC, SNOWDEN_MAX, CLASS_ALB_REF, CLASS_SNO_AGE, CLASS_ALB_NEW,&
                                                PSIWLT, Z0SOIL, Z0LAKE
    namelist / noahmp_global_parameters /       CO2, O2, TIMEAN, FSATMX, Z0SNO, SSI, SNOW_RET_FAC ,SNOW_EMIS, SWEMX, TAU0,   &
                                                GRAIN_GROWTH, EXTRA_GROWTH, DIRT_SOOT, BATS_COSZ, BATS_VIS_NEW,              &
                                                BATS_NIR_NEW, BATS_VIS_AGE, BATS_NIR_AGE, BATS_VIS_DIR, BATS_NIR_DIR,        &
                                                RSURF_SNOW, RSURF_EXP, C2_SNOWCOMPACT, C3_SNOWCOMPACT, C4_SNOWCOMPACT,       &
                                                C5_SNOWCOMPACT, DM_SNOWCOMPACT, ETA0_SNOWCOMPACT, SNLIQMAXFRAC, SWEMAXGLA,   &
                                                WSLMAX, ROUS, CMIC, SNOWDEN_MAX, CLASS_ALB_REF, CLASS_SNO_AGE, CLASS_ALB_NEW,&
                                                PSIWLT, Z0SOIL, Z0LAKE

    ! irrigation parameters
    integer                                  :: IRR_HAR
    real(kind=kind_noahmp)                   :: IRR_FRAC, IRR_LAI, IRR_MAD, FILOSS, SPRIR_RATE, MICIR_RATE, FIRTFAC, IR_RAIN
    namelist / noahmp_irrigation_parameters /   IRR_FRAC, IRR_HAR, IRR_LAI, IRR_MAD, FILOSS, SPRIR_RATE, MICIR_RATE, FIRTFAC,&
                                                IR_RAIN

    ! crop parameters
    integer                                  :: DEFAULT_CROP
    integer               , dimension(NCROP) :: PLTDAY, HSDAY
    real(kind=kind_noahmp), dimension(NCROP) :: PLANTPOP, IRRI, GDDTBASE, GDDTCUT, GDDS1, GDDS2, GDDS3, GDDS4, GDDS5, C3PSNI,&
                                                KC25I, AKCI, KO25I, AKOI, AVCMXI, VCMX25I, BPI, MPI, FOLNMXI, QE25I, AREF,   &
                                                PSNRF, I2PAR, TASSIM0, TASSIM1, TASSIM2, K, EPSI, Q10MR, LEFREEZ,            &
                                                DILE_FC_S1, DILE_FC_S2, DILE_FC_S3, DILE_FC_S4, DILE_FC_S5, DILE_FC_S6,      &
                                                DILE_FC_S7, DILE_FC_S8, DILE_FW_S1, DILE_FW_S2, DILE_FW_S3, DILE_FW_S4,      &
                                                DILE_FW_S5, DILE_FW_S6, DILE_FW_S7, DILE_FW_S8, FRA_GR, LF_OVRC_S1,          &
                                                LF_OVRC_S2, LF_OVRC_S3, LF_OVRC_S4, LF_OVRC_S5, LF_OVRC_S6, LF_OVRC_S7,      &
                                                LF_OVRC_S8, ST_OVRC_S1, ST_OVRC_S2, ST_OVRC_S3, ST_OVRC_S4, ST_OVRC_S5,      &
                                                ST_OVRC_S6, ST_OVRC_S7, ST_OVRC_S8, RT_OVRC_S1, RT_OVRC_S2, RT_OVRC_S3,      &
                                                RT_OVRC_S4, RT_OVRC_S5, RT_OVRC_S6, RT_OVRC_S7, RT_OVRC_S8, LFMR25, STMR25,  &
                                                RTMR25, GRAINMR25, LFPT_S1, LFPT_S2, LFPT_S3, LFPT_S4, LFPT_S5, LFPT_S6,     &
                                                LFPT_S7, LFPT_S8, STPT_S1, STPT_S2, STPT_S3, STPT_S4, STPT_S5, STPT_S6,      &
                                                STPT_S7, STPT_S8, RTPT_S1, RTPT_S2, RTPT_S3, RTPT_S4, RTPT_S5, RTPT_S6,      &
                                                RTPT_S7, RTPT_S8, GRAINPT_S1, GRAINPT_S2, GRAINPT_S3, GRAINPT_S4, GRAINPT_S5,&
                                                GRAINPT_S6, GRAINPT_S7, GRAINPT_S8, LFCT_S1, LFCT_S2, LFCT_S3, LFCT_S4,      &
                                                LFCT_S5, LFCT_S6, LFCT_S7, LFCT_S8, STCT_S1, STCT_S2, STCT_S3, STCT_S4,      &
                                                STCT_S5, STCT_S6, STCT_S7, STCT_S8, RTCT_S1, RTCT_S2, RTCT_S3, RTCT_S4,      &
                                                RTCT_S5, RTCT_S6, RTCT_S7, RTCT_S8, BIO2LAI
    namelist / noahmp_crop_parameters /         DEFAULT_CROP, PLTDAY, HSDAY, PLANTPOP, IRRI, GDDTBASE, GDDTCUT, GDDS1, GDDS2,&
                                                GDDS3, GDDS4, GDDS5, C3PSNI, KC25I, AKCI, KO25I, AKOI, AVCMXI, VCMX25I, BPI, &
                                                MPI, FOLNMXI, QE25I, AREF, PSNRF, I2PAR, TASSIM0, TASSIM1, TASSIM2, K,       &
                                                EPSI,Q10MR, LEFREEZ, DILE_FC_S1, DILE_FC_S2, DILE_FC_S3, DILE_FC_S4,         &
                                                DILE_FC_S5, DILE_FC_S6, DILE_FC_S7, DILE_FC_S8, DILE_FW_S1, DILE_FW_S2,      &
                                                DILE_FW_S3, DILE_FW_S4, DILE_FW_S5, DILE_FW_S6, DILE_FW_S7, DILE_FW_S8,      &
                                                FRA_GR, LF_OVRC_S1, LF_OVRC_S2, LF_OVRC_S3, LF_OVRC_S4, LF_OVRC_S5,          &
                                                LF_OVRC_S6, LF_OVRC_S7, LF_OVRC_S8, ST_OVRC_S1, ST_OVRC_S2, ST_OVRC_S3,      &
                                                ST_OVRC_S4, ST_OVRC_S5, ST_OVRC_S6, ST_OVRC_S7, ST_OVRC_S8, RT_OVRC_S1,      &
                                                RT_OVRC_S2, RT_OVRC_S3, RT_OVRC_S4, RT_OVRC_S5, RT_OVRC_S6, RT_OVRC_S7,      &
                                                RT_OVRC_S8, LFMR25, STMR25, RTMR25, GRAINMR25, LFPT_S1, LFPT_S2, LFPT_S3,    &
                                                LFPT_S4, LFPT_S5, LFPT_S6, LFPT_S7, LFPT_S8, STPT_S1, STPT_S2, STPT_S3,      &
                                                STPT_S4, STPT_S5, STPT_S6, STPT_S7, STPT_S8, RTPT_S1, RTPT_S2, RTPT_S3,      &
                                                RTPT_S4, RTPT_S5, RTPT_S6, RTPT_S7, RTPT_S8, GRAINPT_S1, GRAINPT_S2,         &
                                                GRAINPT_S3, GRAINPT_S4, GRAINPT_S5, GRAINPT_S6, GRAINPT_S7, GRAINPT_S8,      &
                                                LFCT_S1, LFCT_S2, LFCT_S3, LFCT_S4, LFCT_S5, LFCT_S6, LFCT_S7, LFCT_S8,      &
                                                STCT_S1, STCT_S2, STCT_S3, STCT_S4, STCT_S5, STCT_S6, STCT_S7, STCT_S8,      &
                                                RTCT_S1, RTCT_S2, RTCT_S3, RTCT_S4, RTCT_S5, RTCT_S6, RTCT_S7, RTCT_S8,      &
                                                BIO2LAI

    ! tile drainage parameters
    integer                                        :: NSOILTYPE, DRAIN_LAYER_OPT
    integer               , dimension(MAX_SOILTYP) :: TD_DEPTH
    real(kind=kind_noahmp), dimension(MAX_SOILTYP) :: TDSMC_FAC, TD_DC, TD_DCOEF, TD_D, TD_ADEPTH, TD_RADI, TD_SPAC,         &
                                                      TD_DDRAIN, KLAT_FAC
    namelist / noahmp_tiledrain_parameters /          NSOILTYPE, DRAIN_LAYER_OPT, TDSMC_FAC, TD_DEPTH, TD_DC, TD_DCOEF, TD_D,&
                                                      TD_ADEPTH, TD_RADI, TD_SPAC, TD_DDRAIN, KLAT_FAC

    ! optional parameters
    real(kind=kind_noahmp)                         :: sr2006_theta_1500t_a, sr2006_theta_1500t_b, sr2006_theta_1500t_c,      &
                                                      sr2006_theta_1500t_d, sr2006_theta_1500t_e, sr2006_theta_1500t_f,      &
                                                      sr2006_theta_1500t_g, sr2006_theta_1500_a , sr2006_theta_1500_b,       &
                                                      sr2006_theta_33t_a, sr2006_theta_33t_b, sr2006_theta_33t_c,            &
                                                      sr2006_theta_33t_d, sr2006_theta_33t_e, sr2006_theta_33t_f,            &
                                                      sr2006_theta_33t_g, sr2006_theta_33_a, sr2006_theta_33_b,              &
                                                      sr2006_theta_33_c, sr2006_theta_s33t_a, sr2006_theta_s33t_b,           &
                                                      sr2006_theta_s33t_c, sr2006_theta_s33t_d, sr2006_theta_s33t_e,         &
                                                      sr2006_theta_s33t_f, sr2006_theta_s33t_g, sr2006_theta_s33_a,          &
                                                      sr2006_theta_s33_b, sr2006_psi_et_a, sr2006_psi_et_b, sr2006_psi_et_c, &
                                                      sr2006_psi_et_d, sr2006_psi_et_e, sr2006_psi_et_f, sr2006_psi_et_g,    &
                                                      sr2006_psi_e_a, sr2006_psi_e_b, sr2006_psi_e_c, sr2006_smcmax_a,       &
                                                      sr2006_smcmax_b
    namelist / noahmp_optional_parameters /           sr2006_theta_1500t_a, sr2006_theta_1500t_b, sr2006_theta_1500t_c,      &
                                                      sr2006_theta_1500t_d, sr2006_theta_1500t_e, sr2006_theta_1500t_f,      &
                                                      sr2006_theta_1500t_g, sr2006_theta_1500_a, sr2006_theta_1500_b,        &
                                                      sr2006_theta_33t_a, sr2006_theta_33t_b, sr2006_theta_33t_c,            &
                                                      sr2006_theta_33t_d, sr2006_theta_33t_e, sr2006_theta_33t_f,            &
                                                      sr2006_theta_33t_g, sr2006_theta_33_a, sr2006_theta_33_b,              &
                                                      sr2006_theta_33_c, sr2006_theta_s33t_a, sr2006_theta_s33t_b,           &
                                                      sr2006_theta_s33t_c, sr2006_theta_s33t_d, sr2006_theta_s33t_e,         &
                                                      sr2006_theta_s33t_f, sr2006_theta_s33t_g, sr2006_theta_s33_a,          &
                                                      sr2006_theta_s33_b, sr2006_psi_et_a, sr2006_psi_et_b, sr2006_psi_et_c, &
                                                      sr2006_psi_et_d, sr2006_psi_et_e, sr2006_psi_et_f, sr2006_psi_et_g,    &
                                                      sr2006_psi_e_a, sr2006_psi_e_b, sr2006_psi_e_c, sr2006_smcmax_a,       &
                                                      sr2006_smcmax_b

    !--------------------------------------------------
    !=== allocate multi-dim input table variables
    !--------------------------------------------------

    ! vegetation parameters
    allocate( NoahmpIO%CH2OP_TABLE (MVT) )
    allocate( NoahmpIO%DLEAF_TABLE (MVT) )
    allocate( NoahmpIO%Z0MVT_TABLE (MVT) )
    allocate( NoahmpIO%HVT_TABLE   (MVT) )
    allocate( NoahmpIO%HVB_TABLE   (MVT) )
    allocate( NoahmpIO%DEN_TABLE   (MVT) )
    allocate( NoahmpIO%RC_TABLE    (MVT) )
    allocate( NoahmpIO%MFSNO_TABLE (MVT) )
    allocate( NoahmpIO%SCFFAC_TABLE(MVT) )
    allocate( NoahmpIO%CBIOM_TABLE (MVT) )
    allocate( NoahmpIO%SAIM_TABLE  (MVT,12) )
    allocate( NoahmpIO%LAIM_TABLE  (MVT,12) )
    allocate( NoahmpIO%SLA_TABLE   (MVT) )
    allocate( NoahmpIO%DILEFC_TABLE(MVT) )
    allocate( NoahmpIO%DILEFW_TABLE(MVT) )
    allocate( NoahmpIO%FRAGR_TABLE (MVT) )
    allocate( NoahmpIO%LTOVRC_TABLE(MVT) )
    allocate( NoahmpIO%C3PSN_TABLE (MVT) )
    allocate( NoahmpIO%KC25_TABLE  (MVT) )
    allocate( NoahmpIO%AKC_TABLE   (MVT) )
    allocate( NoahmpIO%KO25_TABLE  (MVT) )
    allocate( NoahmpIO%AKO_TABLE   (MVT) )
    allocate( NoahmpIO%VCMX25_TABLE(MVT) )
    allocate( NoahmpIO%AVCMX_TABLE (MVT) )
    allocate( NoahmpIO%BP_TABLE    (MVT) )
    allocate( NoahmpIO%MP_TABLE    (MVT) )
    allocate( NoahmpIO%QE25_TABLE  (MVT) )
    allocate( NoahmpIO%AQE_TABLE   (MVT) )
    allocate( NoahmpIO%RMF25_TABLE (MVT) )
    allocate( NoahmpIO%RMS25_TABLE (MVT) )
    allocate( NoahmpIO%RMR25_TABLE (MVT) )
    allocate( NoahmpIO%ARM_TABLE   (MVT) )
    allocate( NoahmpIO%FOLNMX_TABLE(MVT) )
    allocate( NoahmpIO%TMIN_TABLE  (MVT) )
    allocate( NoahmpIO%XL_TABLE    (MVT) )
    allocate( NoahmpIO%RHOL_TABLE  (MVT,MBAND) )
    allocate( NoahmpIO%RHOS_TABLE  (MVT,MBAND) )
    allocate( NoahmpIO%TAUL_TABLE  (MVT,MBAND) )
    allocate( NoahmpIO%TAUS_TABLE  (MVT,MBAND) )
    allocate( NoahmpIO%MRP_TABLE   (MVT) )
    allocate( NoahmpIO%CWPVT_TABLE (MVT) )
    allocate( NoahmpIO%WRRAT_TABLE (MVT) )
    allocate( NoahmpIO%WDPOOL_TABLE(MVT) )
    allocate( NoahmpIO%TDLEF_TABLE (MVT) )
    allocate( NoahmpIO%NROOT_TABLE (MVT) )
    allocate( NoahmpIO%RGL_TABLE   (MVT) )
    allocate( NoahmpIO%RS_TABLE    (MVT) )
    allocate( NoahmpIO%HS_TABLE    (MVT) )
    allocate( NoahmpIO%TOPT_TABLE  (MVT) )
    allocate( NoahmpIO%RSMAX_TABLE (MVT) )
    allocate( NoahmpIO%RTOVRC_TABLE(MVT) )
    allocate( NoahmpIO%RSWOODC_TABLE(MVT) )
    allocate( NoahmpIO%BF_TABLE    (MVT) )
    allocate( NoahmpIO%WSTRC_TABLE (MVT) )
    allocate( NoahmpIO%LAIMIN_TABLE(MVT) )
    allocate( NoahmpIO%XSAMIN_TABLE(MVT) )

    ! soil parameters
    allocate( NoahmpIO%BEXP_TABLE  (MAX_SOILTYP) )
    allocate( NoahmpIO%SMCDRY_TABLE(MAX_SOILTYP) )
    allocate( NoahmpIO%SMCMAX_TABLE(MAX_SOILTYP) )
    allocate( NoahmpIO%SMCREF_TABLE(MAX_SOILTYP) )
    allocate( NoahmpIO%PSISAT_TABLE(MAX_SOILTYP) )
    allocate( NoahmpIO%DKSAT_TABLE (MAX_SOILTYP) )
    allocate( NoahmpIO%DWSAT_TABLE (MAX_SOILTYP) )
    allocate( NoahmpIO%SMCWLT_TABLE(MAX_SOILTYP) )
    allocate( NoahmpIO%QUARTZ_TABLE(MAX_SOILTYP) )
    allocate( NoahmpIO%BVIC_TABLE  (MAX_SOILTYP) )
    allocate( NoahmpIO%AXAJ_TABLE  (MAX_SOILTYP) )
    allocate( NoahmpIO%BXAJ_TABLE  (MAX_SOILTYP) )
    allocate( NoahmpIO%XXAJ_TABLE  (MAX_SOILTYP) )
    allocate( NoahmpIO%BDVIC_TABLE (MAX_SOILTYP) )
    allocate( NoahmpIO%GDVIC_TABLE (MAX_SOILTYP) )
    allocate( NoahmpIO%BBVIC_TABLE (MAX_SOILTYP) )

    ! general parameters
    allocate( NoahmpIO%SLOPE_TABLE(NUM_SLOPE) )

    ! radiation parameters
    allocate( NoahmpIO%ALBSAT_TABLE(MSC,MBAND) )
    allocate( NoahmpIO%ALBDRY_TABLE(MSC,MBAND) )
    allocate( NoahmpIO%ALBICE_TABLE(MBAND) )
    allocate( NoahmpIO%ALBLAK_TABLE(MBAND) )
    allocate( NoahmpIO%OMEGAS_TABLE(MBAND) )
    allocate( NoahmpIO%EG_TABLE(2) )

    ! tile drainage parameters
    allocate( NoahmpIO%TDSMC_FAC_TABLE(MAX_SOILTYP) )
    allocate( NoahmpIO%TD_DC_TABLE    (MAX_SOILTYP) )
    allocate( NoahmpIO%TD_DEPTH_TABLE (MAX_SOILTYP) )
    allocate( NoahmpIO%TD_DCOEF_TABLE (MAX_SOILTYP) )
    allocate( NoahmpIO%TD_D_TABLE     (MAX_SOILTYP) )
    allocate( NoahmpIO%TD_ADEPTH_TABLE(MAX_SOILTYP) )
    allocate( NoahmpIO%TD_RADI_TABLE  (MAX_SOILTYP) )
    allocate( NoahmpIO%TD_SPAC_TABLE  (MAX_SOILTYP) )
    allocate( NoahmpIO%TD_DDRAIN_TABLE(MAX_SOILTYP) )
    allocate( NoahmpIO%KLAT_FAC_TABLE (MAX_SOILTYP) )

    ! crop parameters
    allocate( NoahmpIO%PLTDAY_TABLE   (NCROP) )
    allocate( NoahmpIO%HSDAY_TABLE    (NCROP) )
    allocate( NoahmpIO%PLANTPOP_TABLE (NCROP) )
    allocate( NoahmpIO%IRRI_TABLE     (NCROP) )
    allocate( NoahmpIO%GDDTBASE_TABLE (NCROP) )
    allocate( NoahmpIO%GDDTCUT_TABLE  (NCROP) )
    allocate( NoahmpIO%GDDS1_TABLE    (NCROP) )
    allocate( NoahmpIO%GDDS2_TABLE    (NCROP) )
    allocate( NoahmpIO%GDDS3_TABLE    (NCROP) )
    allocate( NoahmpIO%GDDS4_TABLE    (NCROP) )
    allocate( NoahmpIO%GDDS5_TABLE    (NCROP) )
    allocate( NoahmpIO%C3PSNI_TABLE   (NCROP) )
    allocate( NoahmpIO%KC25I_TABLE    (NCROP) )
    allocate( NoahmpIO%AKCI_TABLE     (NCROP) )
    allocate( NoahmpIO%KO25I_TABLE    (NCROP) )
    allocate( NoahmpIO%AKOI_TABLE     (NCROP) )
    allocate( NoahmpIO%VCMX25I_TABLE  (NCROP) )
    allocate( NoahmpIO%AVCMXI_TABLE   (NCROP) )
    allocate( NoahmpIO%BPI_TABLE      (NCROP) )
    allocate( NoahmpIO%MPI_TABLE      (NCROP) )
    allocate( NoahmpIO%QE25I_TABLE    (NCROP) )
    allocate( NoahmpIO%FOLNMXI_TABLE  (NCROP) )
    allocate( NoahmpIO%AREF_TABLE     (NCROP) )
    allocate( NoahmpIO%PSNRF_TABLE    (NCROP) )
    allocate( NoahmpIO%I2PAR_TABLE    (NCROP) )
    allocate( NoahmpIO%TASSIM0_TABLE  (NCROP) )
    allocate( NoahmpIO%TASSIM1_TABLE  (NCROP) )
    allocate( NoahmpIO%TASSIM2_TABLE  (NCROP) )
    allocate( NoahmpIO%K_TABLE        (NCROP) )
    allocate( NoahmpIO%EPSI_TABLE     (NCROP) )
    allocate( NoahmpIO%Q10MR_TABLE    (NCROP) )
    allocate( NoahmpIO%LEFREEZ_TABLE  (NCROP) )
    allocate( NoahmpIO%DILE_FC_TABLE  (NCROP,NSTAGE) )
    allocate( NoahmpIO%DILE_FW_TABLE  (NCROP,NSTAGE) )
    allocate( NoahmpIO%FRA_GR_TABLE   (NCROP) )
    allocate( NoahmpIO%LF_OVRC_TABLE  (NCROP,NSTAGE) )
    allocate( NoahmpIO%ST_OVRC_TABLE  (NCROP,NSTAGE) )
    allocate( NoahmpIO%RT_OVRC_TABLE  (NCROP,NSTAGE) )
    allocate( NoahmpIO%LFMR25_TABLE   (NCROP) )
    allocate( NoahmpIO%STMR25_TABLE   (NCROP) )
    allocate( NoahmpIO%RTMR25_TABLE   (NCROP) )
    allocate( NoahmpIO%GRAINMR25_TABLE(NCROP) )
    allocate( NoahmpIO%LFPT_TABLE     (NCROP,NSTAGE) )
    allocate( NoahmpIO%STPT_TABLE     (NCROP,NSTAGE) )
    allocate( NoahmpIO%RTPT_TABLE     (NCROP,NSTAGE) )
    allocate( NoahmpIO%GRAINPT_TABLE  (NCROP,NSTAGE) )
    allocate( NoahmpIO%LFCT_TABLE     (NCROP,NSTAGE) )
    allocate( NoahmpIO%STCT_TABLE     (NCROP,NSTAGE) )
    allocate( NoahmpIO%RTCT_TABLE     (NCROP,NSTAGE) )
    allocate( NoahmpIO%BIO2LAI_TABLE  (NCROP) )

    !---------------------------------------------------------------
    ! intialization to bad value, so that if the namelist read fails,
    ! we come to a screeching halt as soon as we try to use anything
    !---------------------------------------------------------------

    ! vegetation parameters
    NoahmpIO%ISURBAN_TABLE      = -99999
    NoahmpIO%ISWATER_TABLE      = -99999
    NoahmpIO%ISBARREN_TABLE     = -99999
    NoahmpIO%ISICE_TABLE        = -99999
    NoahmpIO%ISCROP_TABLE       = -99999
    NoahmpIO%EBLFOREST_TABLE    = -99999
    NoahmpIO%NATURAL_TABLE      = -99999
    NoahmpIO%LCZ_1_TABLE        = -99999
    NoahmpIO%LCZ_2_TABLE        = -99999
    NoahmpIO%LCZ_3_TABLE        = -99999
    NoahmpIO%LCZ_4_TABLE        = -99999
    NoahmpIO%LCZ_5_TABLE        = -99999
    NoahmpIO%LCZ_6_TABLE        = -99999
    NoahmpIO%LCZ_7_TABLE        = -99999
    NoahmpIO%LCZ_8_TABLE        = -99999
    NoahmpIO%LCZ_9_TABLE        = -99999
    NoahmpIO%LCZ_10_TABLE       = -99999
    NoahmpIO%LCZ_11_TABLE       = -99999
    NoahmpIO%CH2OP_TABLE        = -1.0e36
    NoahmpIO%DLEAF_TABLE        = -1.0e36
    NoahmpIO%Z0MVT_TABLE        = -1.0e36
    NoahmpIO%HVT_TABLE          = -1.0e36
    NoahmpIO%HVB_TABLE          = -1.0e36
    NoahmpIO%DEN_TABLE          = -1.0e36
    NoahmpIO%RC_TABLE           = -1.0e36
    NoahmpIO%MFSNO_TABLE        = -1.0e36
    NoahmpIO%SCFFAC_TABLE       = -1.0e36
    NoahmpIO%CBIOM_TABLE        = -1.0e36
    NoahmpIO%RHOL_TABLE         = -1.0e36
    NoahmpIO%RHOS_TABLE         = -1.0e36
    NoahmpIO%TAUL_TABLE         = -1.0e36
    NoahmpIO%TAUS_TABLE         = -1.0e36
    NoahmpIO%XL_TABLE           = -1.0e36
    NoahmpIO%CWPVT_TABLE        = -1.0e36
    NoahmpIO%C3PSN_TABLE        = -1.0e36
    NoahmpIO%KC25_TABLE         = -1.0e36
    NoahmpIO%AKC_TABLE          = -1.0e36
    NoahmpIO%KO25_TABLE         = -1.0e36
    NoahmpIO%AKO_TABLE          = -1.0e36
    NoahmpIO%AVCMX_TABLE        = -1.0e36
    NoahmpIO%AQE_TABLE          = -1.0e36
    NoahmpIO%LTOVRC_TABLE       = -1.0e36
    NoahmpIO%DILEFC_TABLE       = -1.0e36
    NoahmpIO%DILEFW_TABLE       = -1.0e36
    NoahmpIO%RMF25_TABLE        = -1.0e36
    NoahmpIO%SLA_TABLE          = -1.0e36
    NoahmpIO%FRAGR_TABLE        = -1.0e36
    NoahmpIO%TMIN_TABLE         = -1.0e36
    NoahmpIO%VCMX25_TABLE       = -1.0e36
    NoahmpIO%TDLEF_TABLE        = -1.0e36
    NoahmpIO%BP_TABLE           = -1.0e36
    NoahmpIO%MP_TABLE           = -1.0e36
    NoahmpIO%QE25_TABLE         = -1.0e36
    NoahmpIO%RMS25_TABLE        = -1.0e36
    NoahmpIO%RMR25_TABLE        = -1.0e36
    NoahmpIO%ARM_TABLE          = -1.0e36
    NoahmpIO%FOLNMX_TABLE       = -1.0e36
    NoahmpIO%WDPOOL_TABLE       = -1.0e36
    NoahmpIO%WRRAT_TABLE        = -1.0e36
    NoahmpIO%MRP_TABLE          = -1.0e36
    NoahmpIO%SAIM_TABLE         = -1.0e36
    NoahmpIO%LAIM_TABLE         = -1.0e36
    NoahmpIO%NROOT_TABLE        = -1.0e36
    NoahmpIO%RGL_TABLE          = -1.0e36
    NoahmpIO%RS_TABLE           = -1.0e36
    NoahmpIO%HS_TABLE           = -1.0e36
    NoahmpIO%TOPT_TABLE         = -1.0e36
    NoahmpIO%RSMAX_TABLE        = -1.0e36
    NoahmpIO%RTOVRC_TABLE       = -1.0e36
    NoahmpIO%RSWOODC_TABLE      = -1.0e36
    NoahmpIO%BF_TABLE           = -1.0e36
    NoahmpIO%WSTRC_TABLE        = -1.0e36
    NoahmpIO%LAIMIN_TABLE       = -1.0e36
    NoahmpIO%XSAMIN_TABLE       = -1.0e36

    ! soil parameters
    NoahmpIO%SLCATS_TABLE       = -99999
    NoahmpIO%BEXP_TABLE         = -1.0e36
    NoahmpIO%SMCDRY_TABLE       = -1.0e36
    NoahmpIO%SMCMAX_TABLE       = -1.0e36
    NoahmpIO%SMCREF_TABLE       = -1.0e36
    NoahmpIO%PSISAT_TABLE       = -1.0e36
    NoahmpIO%DKSAT_TABLE        = -1.0e36
    NoahmpIO%DWSAT_TABLE        = -1.0e36
    NoahmpIO%SMCWLT_TABLE       = -1.0e36
    NoahmpIO%QUARTZ_TABLE       = -1.0e36
    NoahmpIO%BVIC_TABLE         = -1.0e36
    NoahmpIO%AXAJ_TABLE         = -1.0e36
    NoahmpIO%BXAJ_TABLE         = -1.0e36
    NoahmpIO%XXAJ_TABLE         = -1.0e36
    NoahmpIO%BDVIC_TABLE        = -1.0e36
    NoahmpIO%GDVIC_TABLE        = -1.0e36
    NoahmpIO%BBVIC_TABLE        = -1.0e36

    ! general parameters
    NoahmpIO%SLOPE_TABLE        = -1.0e36
    NoahmpIO%CSOIL_TABLE        = -1.0e36
    NoahmpIO%REFDK_TABLE        = -1.0e36
    NoahmpIO%REFKDT_TABLE       = -1.0e36
    NoahmpIO%FRZK_TABLE         = -1.0e36
    NoahmpIO%ZBOT_TABLE         = -1.0e36
    NoahmpIO%CZIL_TABLE         = -1.0e36

    ! radiation parameters
    NoahmpIO%ALBSAT_TABLE       = -1.0e36
    NoahmpIO%ALBDRY_TABLE       = -1.0e36
    NoahmpIO%ALBICE_TABLE       = -1.0e36
    NoahmpIO%ALBLAK_TABLE       = -1.0e36
    NoahmpIO%OMEGAS_TABLE       = -1.0e36
    NoahmpIO%BETADS_TABLE       = -1.0e36
    NoahmpIO%BETAIS_TABLE       = -1.0e36
    NoahmpIO%EG_TABLE           = -1.0e36
    NoahmpIO%EICE_TABLE         = -1.0e36

    ! global parameters
    NoahmpIO%CO2_TABLE              = -1.0e36
    NoahmpIO%O2_TABLE               = -1.0e36
    NoahmpIO%TIMEAN_TABLE           = -1.0e36
    NoahmpIO%FSATMX_TABLE           = -1.0e36
    NoahmpIO%Z0SNO_TABLE            = -1.0e36
    NoahmpIO%SSI_TABLE              = -1.0e36
    NoahmpIO%SNOW_RET_FAC_TABLE     = -1.0e36
    NoahmpIO%SNOW_EMIS_TABLE        = -1.0e36
    NoahmpIO%SWEMX_TABLE            = -1.0e36
    NoahmpIO%TAU0_TABLE             = -1.0e36
    NoahmpIO%GRAIN_GROWTH_TABLE     = -1.0e36
    NoahmpIO%EXTRA_GROWTH_TABLE     = -1.0e36
    NoahmpIO%DIRT_SOOT_TABLE        = -1.0e36
    NoahmpIO%BATS_COSZ_TABLE        = -1.0e36
    NoahmpIO%BATS_VIS_NEW_TABLE     = -1.0e36
    NoahmpIO%BATS_NIR_NEW_TABLE     = -1.0e36
    NoahmpIO%BATS_VIS_AGE_TABLE     = -1.0e36
    NoahmpIO%BATS_NIR_AGE_TABLE     = -1.0e36
    NoahmpIO%BATS_VIS_DIR_TABLE     = -1.0e36
    NoahmpIO%BATS_NIR_DIR_TABLE     = -1.0e36
    NoahmpIO%RSURF_SNOW_TABLE       = -1.0e36
    NoahmpIO%RSURF_EXP_TABLE        = -1.0e36
    NoahmpIO%C2_SNOWCOMPACT_TABLE   = -1.0e36
    NoahmpIO%C3_SNOWCOMPACT_TABLE   = -1.0e36
    NoahmpIO%C4_SNOWCOMPACT_TABLE   = -1.0e36
    NoahmpIO%C5_SNOWCOMPACT_TABLE   = -1.0e36
    NoahmpIO%DM_SNOWCOMPACT_TABLE   = -1.0e36
    NoahmpIO%ETA0_SNOWCOMPACT_TABLE = -1.0e36
    NoahmpIO%SNLIQMAXFRAC_TABLE     = -1.0e36
    NoahmpIO%SWEMAXGLA_TABLE        = -1.0e36
    NoahmpIO%WSLMAX_TABLE           = -1.0e36
    NoahmpIO%ROUS_TABLE             = -1.0e36
    NoahmpIO%CMIC_TABLE             = -1.0e36
    NoahmpIO%SNOWDEN_MAX_TABLE      = -1.0e36
    NoahmpIO%CLASS_ALB_REF_TABLE    = -1.0e36
    NoahmpIO%CLASS_SNO_AGE_TABLE    = -1.0e36
    NoahmpIO%CLASS_ALB_NEW_TABLE    = -1.0e36
    NoahmpIO%PSIWLT_TABLE           = -1.0e36
    NoahmpIO%Z0SOIL_TABLE           = -1.0e36
    NoahmpIO%Z0LAKE_TABLE           = -1.0e36

    ! irrigation parameters
    NoahmpIO%IRR_HAR_TABLE          = -99999
    NoahmpIO%IRR_FRAC_TABLE         = -1.0e36
    NoahmpIO%IRR_LAI_TABLE          = -1.0e36
    NoahmpIO%IRR_MAD_TABLE          = -1.0e36
    NoahmpIO%FILOSS_TABLE           = -1.0e36
    NoahmpIO%SPRIR_RATE_TABLE       = -1.0e36
    NoahmpIO%MICIR_RATE_TABLE       = -1.0e36
    NoahmpIO%FIRTFAC_TABLE          = -1.0e36
    NoahmpIO%IR_RAIN_TABLE          = -1.0e36

    ! crop parameters
    NoahmpIO%DEFAULT_CROP_TABLE     = -99999
    NoahmpIO%PLTDAY_TABLE           = -99999
    NoahmpIO%HSDAY_TABLE            = -99999
    NoahmpIO%PLANTPOP_TABLE         = -1.0e36
    NoahmpIO%IRRI_TABLE             = -1.0e36
    NoahmpIO%GDDTBASE_TABLE         = -1.0e36
    NoahmpIO%GDDTCUT_TABLE          = -1.0e36
    NoahmpIO%GDDS1_TABLE            = -1.0e36
    NoahmpIO%GDDS2_TABLE            = -1.0e36
    NoahmpIO%GDDS3_TABLE            = -1.0e36
    NoahmpIO%GDDS4_TABLE            = -1.0e36
    NoahmpIO%GDDS5_TABLE            = -1.0e36
    NoahmpIO%C3PSNI_TABLE           = -1.0e36
    NoahmpIO%KC25I_TABLE            = -1.0e36
    NoahmpIO%AKCI_TABLE             = -1.0e36
    NoahmpIO%KO25I_TABLE            = -1.0e36
    NoahmpIO%AKOI_TABLE             = -1.0e36
    NoahmpIO%AVCMXI_TABLE           = -1.0e36
    NoahmpIO%VCMX25I_TABLE          = -1.0e36
    NoahmpIO%BPI_TABLE              = -1.0e36
    NoahmpIO%MPI_TABLE              = -1.0e36
    NoahmpIO%FOLNMXI_TABLE          = -1.0e36
    NoahmpIO%QE25I_TABLE            = -1.0e36
    NoahmpIO%AREF_TABLE             = -1.0e36
    NoahmpIO%PSNRF_TABLE            = -1.0e36
    NoahmpIO%I2PAR_TABLE            = -1.0e36
    NoahmpIO%TASSIM0_TABLE          = -1.0e36
    NoahmpIO%TASSIM1_TABLE          = -1.0e36
    NoahmpIO%TASSIM2_TABLE          = -1.0e36
    NoahmpIO%K_TABLE                = -1.0e36
    NoahmpIO%EPSI_TABLE             = -1.0e36
    NoahmpIO%Q10MR_TABLE            = -1.0e36
    NoahmpIO%LEFREEZ_TABLE          = -1.0e36
    NoahmpIO%DILE_FC_TABLE          = -1.0e36
    NoahmpIO%DILE_FW_TABLE          = -1.0e36
    NoahmpIO%FRA_GR_TABLE           = -1.0e36
    NoahmpIO%LF_OVRC_TABLE          = -1.0e36
    NoahmpIO%ST_OVRC_TABLE          = -1.0e36
    NoahmpIO%RT_OVRC_TABLE          = -1.0e36
    NoahmpIO%LFMR25_TABLE           = -1.0e36
    NoahmpIO%STMR25_TABLE           = -1.0e36
    NoahmpIO%RTMR25_TABLE           = -1.0e36
    NoahmpIO%GRAINMR25_TABLE        = -1.0e36
    NoahmpIO%LFPT_TABLE             = -1.0e36
    NoahmpIO%STPT_TABLE             = -1.0e36
    NoahmpIO%RTPT_TABLE             = -1.0e36
    NoahmpIO%GRAINPT_TABLE          = -1.0e36
    NoahmpIO%LFCT_TABLE             = -1.0e36
    NoahmpIO%STCT_TABLE             = -1.0e36
    NoahmpIO%RTCT_TABLE             = -1.0e36
    NoahmpIO%BIO2LAI_TABLE          = -1.0e36

    ! tile drainage parameters
    NoahmpIO%DRAIN_LAYER_OPT_TABLE  = -99999
    NoahmpIO%TD_DEPTH_TABLE         = -99999
    NoahmpIO%TDSMC_FAC_TABLE        = -1.0e36
    NoahmpIO%TD_DC_TABLE            = -1.0e36
    NoahmpIO%TD_DCOEF_TABLE         = -1.0e36
    NoahmpIO%TD_D_TABLE             = -1.0e36
    NoahmpIO%TD_ADEPTH_TABLE        = -1.0e36
    NoahmpIO%TD_RADI_TABLE          = -1.0e36
    NoahmpIO%TD_SPAC_TABLE          = -1.0e36
    NoahmpIO%TD_DDRAIN_TABLE        = -1.0e36
    NoahmpIO%KLAT_FAC_TABLE         = -1.0e36

    ! optional parameters
    NoahmpIO%sr2006_theta_1500t_a_TABLE = -1.0e36
    NoahmpIO%sr2006_theta_1500t_b_TABLE = -1.0e36
    NoahmpIO%sr2006_theta_1500t_c_TABLE = -1.0e36
    NoahmpIO%sr2006_theta_1500t_d_TABLE = -1.0e36
    NoahmpIO%sr2006_theta_1500t_e_TABLE = -1.0e36
    NoahmpIO%sr2006_theta_1500t_f_TABLE = -1.0e36
    NoahmpIO%sr2006_theta_1500t_g_TABLE = -1.0e36
    NoahmpIO%sr2006_theta_1500_a_TABLE  = -1.0e36
    NoahmpIO%sr2006_theta_1500_b_TABLE  = -1.0e36
    NoahmpIO%sr2006_theta_33t_a_TABLE   = -1.0e36
    NoahmpIO%sr2006_theta_33t_b_TABLE   = -1.0e36
    NoahmpIO%sr2006_theta_33t_c_TABLE   = -1.0e36
    NoahmpIO%sr2006_theta_33t_d_TABLE   = -1.0e36
    NoahmpIO%sr2006_theta_33t_e_TABLE   = -1.0e36
    NoahmpIO%sr2006_theta_33t_f_TABLE   = -1.0e36
    NoahmpIO%sr2006_theta_33t_g_TABLE   = -1.0e36
    NoahmpIO%sr2006_theta_33_a_TABLE    = -1.0e36
    NoahmpIO%sr2006_theta_33_b_TABLE    = -1.0e36
    NoahmpIO%sr2006_theta_33_c_TABLE    = -1.0e36
    NoahmpIO%sr2006_theta_s33t_a_TABLE  = -1.0e36
    NoahmpIO%sr2006_theta_s33t_b_TABLE  = -1.0e36
    NoahmpIO%sr2006_theta_s33t_c_TABLE  = -1.0e36
    NoahmpIO%sr2006_theta_s33t_d_TABLE  = -1.0e36
    NoahmpIO%sr2006_theta_s33t_e_TABLE  = -1.0e36
    NoahmpIO%sr2006_theta_s33t_f_TABLE  = -1.0e36
    NoahmpIO%sr2006_theta_s33t_g_TABLE  = -1.0e36
    NoahmpIO%sr2006_theta_s33_a_TABLE   = -1.0e36
    NoahmpIO%sr2006_theta_s33_b_TABLE   = -1.0e36
    NoahmpIO%sr2006_psi_et_a_TABLE      = -1.0e36
    NoahmpIO%sr2006_psi_et_b_TABLE      = -1.0e36
    NoahmpIO%sr2006_psi_et_c_TABLE      = -1.0e36
    NoahmpIO%sr2006_psi_et_d_TABLE      = -1.0e36
    NoahmpIO%sr2006_psi_et_e_TABLE      = -1.0e36
    NoahmpIO%sr2006_psi_et_f_TABLE      = -1.0e36
    NoahmpIO%sr2006_psi_et_g_TABLE      = -1.0e36
    NoahmpIO%sr2006_psi_e_a_TABLE       = -1.0e36
    NoahmpIO%sr2006_psi_e_b_TABLE       = -1.0e36
    NoahmpIO%sr2006_psi_e_c_TABLE       = -1.0e36
    NoahmpIO%sr2006_smcmax_a_TABLE      = -1.0e36
    NoahmpIO%sr2006_smcmax_b_TABLE      = -1.0e36

    !---------------------------------------------------------------
    ! transfer values from table to input variables
    !---------------------------------------------------------------

    !---------------- NoahmpTable.TBL vegetation parameters

    DATASET_IDENTIFIER = NoahmpIO%LLANDUSE

    inquire( file='NoahmpTable.TBL', exist=file_named )
    if ( file_named ) then
       open(15, file="NoahmpTable.TBL", status='old', form='formatted', action='read', iostat=ierr)
    else
       open(15, status='old', form='formatted', action='read', iostat=ierr)
    end if
    if ( ierr /= 0 ) then
       write(*,'("WARNING: Cannot find file NoahmpTable.TBL")')
    endif

    if ( trim(DATASET_IDENTIFIER) == "USGS" ) then
       read(15, noahmp_usgs_veg_categories)
       read(15, noahmp_usgs_parameters)
    elseif ( trim(DATASET_IDENTIFIER) == "MODIFIED_IGBP_MODIS_NOAH" ) then
       read(15,noahmp_modis_veg_categories)
       read(15,noahmp_modis_parameters)
    else
       write(*,'("WARNING: Unrecognized DATASET_IDENTIFIER in subroutine ReadNoahmpTable")')
       write(*,'("WARNING: DATASET_IDENTIFIER = ''", A, "''")') trim(DATASET_IDENTIFIER)
    endif
    close(15)

    ! assign values
    NoahmpIO%ISURBAN_TABLE         = ISURBAN
    NoahmpIO%ISWATER_TABLE         = ISWATER
    NoahmpIO%ISBARREN_TABLE        = ISBARREN
    NoahmpIO%ISICE_TABLE           = ISICE
    NoahmpIO%ISCROP_TABLE          = ISCROP
    NoahmpIO%EBLFOREST_TABLE       = EBLFOREST
    NoahmpIO%NATURAL_TABLE         = NATURAL
    NoahmpIO%LCZ_1_TABLE           = LCZ_1
    NoahmpIO%LCZ_2_TABLE           = LCZ_2
    NoahmpIO%LCZ_3_TABLE           = LCZ_3
    NoahmpIO%LCZ_4_TABLE           = LCZ_4
    NoahmpIO%LCZ_5_TABLE           = LCZ_5
    NoahmpIO%LCZ_6_TABLE           = LCZ_6
    NoahmpIO%LCZ_7_TABLE           = LCZ_7
    NoahmpIO%LCZ_8_TABLE           = LCZ_8
    NoahmpIO%LCZ_9_TABLE           = LCZ_9
    NoahmpIO%LCZ_10_TABLE          = LCZ_10
    NoahmpIO%LCZ_11_TABLE          = LCZ_11
    NoahmpIO%CH2OP_TABLE  (1:NVEG) = CH2OP  (1:NVEG)
    NoahmpIO%DLEAF_TABLE  (1:NVEG) = DLEAF  (1:NVEG)
    NoahmpIO%Z0MVT_TABLE  (1:NVEG) = Z0MVT  (1:NVEG)
    NoahmpIO%HVT_TABLE    (1:NVEG) = HVT    (1:NVEG)
    NoahmpIO%HVB_TABLE    (1:NVEG) = HVB    (1:NVEG)
    NoahmpIO%DEN_TABLE    (1:NVEG) = DEN    (1:NVEG)
    NoahmpIO%RC_TABLE     (1:NVEG) = RC     (1:NVEG)
    NoahmpIO%MFSNO_TABLE  (1:NVEG) = MFSNO  (1:NVEG)
    NoahmpIO%SCFFAC_TABLE (1:NVEG) = SCFFAC (1:NVEG)
    NoahmpIO%CBIOM_TABLE  (1:NVEG) = CBIOM  (1:NVEG)
    NoahmpIO%XL_TABLE     (1:NVEG) = XL     (1:NVEG)
    NoahmpIO%CWPVT_TABLE  (1:NVEG) = CWPVT  (1:NVEG)
    NoahmpIO%C3PSN_TABLE  (1:NVEG) = C3PSN  (1:NVEG)
    NoahmpIO%KC25_TABLE   (1:NVEG) = KC25   (1:NVEG)
    NoahmpIO%AKC_TABLE    (1:NVEG) = AKC    (1:NVEG)
    NoahmpIO%KO25_TABLE   (1:NVEG) = KO25   (1:NVEG)
    NoahmpIO%AKO_TABLE    (1:NVEG) = AKO    (1:NVEG)
    NoahmpIO%AVCMX_TABLE  (1:NVEG) = AVCMX  (1:NVEG)
    NoahmpIO%AQE_TABLE    (1:NVEG) = AQE    (1:NVEG)
    NoahmpIO%LTOVRC_TABLE (1:NVEG) = LTOVRC (1:NVEG)
    NoahmpIO%DILEFC_TABLE (1:NVEG) = DILEFC (1:NVEG)
    NoahmpIO%DILEFW_TABLE (1:NVEG) = DILEFW (1:NVEG)
    NoahmpIO%RMF25_TABLE  (1:NVEG) = RMF25  (1:NVEG)
    NoahmpIO%SLA_TABLE    (1:NVEG) = SLA    (1:NVEG)
    NoahmpIO%FRAGR_TABLE  (1:NVEG) = FRAGR  (1:NVEG)
    NoahmpIO%TMIN_TABLE   (1:NVEG) = TMIN   (1:NVEG)
    NoahmpIO%VCMX25_TABLE (1:NVEG) = VCMX25 (1:NVEG)
    NoahmpIO%TDLEF_TABLE  (1:NVEG) = TDLEF  (1:NVEG)
    NoahmpIO%BP_TABLE     (1:NVEG) = BP     (1:NVEG)
    NoahmpIO%MP_TABLE     (1:NVEG) = MP     (1:NVEG)
    NoahmpIO%QE25_TABLE   (1:NVEG) = QE25   (1:NVEG)
    NoahmpIO%RMS25_TABLE  (1:NVEG) = RMS25  (1:NVEG)
    NoahmpIO%RMR25_TABLE  (1:NVEG) = RMR25  (1:NVEG)
    NoahmpIO%ARM_TABLE    (1:NVEG) = ARM    (1:NVEG)
    NoahmpIO%FOLNMX_TABLE (1:NVEG) = FOLNMX (1:NVEG)
    NoahmpIO%WDPOOL_TABLE (1:NVEG) = WDPOOL (1:NVEG)
    NoahmpIO%WRRAT_TABLE  (1:NVEG) = WRRAT  (1:NVEG)
    NoahmpIO%MRP_TABLE    (1:NVEG) = MRP    (1:NVEG)
    NoahmpIO%NROOT_TABLE  (1:NVEG) = NROOT  (1:NVEG)
    NoahmpIO%RGL_TABLE    (1:NVEG) = RGL    (1:NVEG)
    NoahmpIO%RS_TABLE     (1:NVEG) = RS     (1:NVEG)
    NoahmpIO%HS_TABLE     (1:NVEG) = HS     (1:NVEG)
    NoahmpIO%TOPT_TABLE   (1:NVEG) = TOPT   (1:NVEG)
    NoahmpIO%RSMAX_TABLE  (1:NVEG) = RSMAX  (1:NVEG)
    NoahmpIO%RTOVRC_TABLE (1:NVEG) = RTOVRC (1:NVEG)
    NoahmpIO%RSWOODC_TABLE(1:NVEG) = RSWOODC(1:NVEG)
    NoahmpIO%BF_TABLE     (1:NVEG) = BF     (1:NVEG)
    NoahmpIO%WSTRC_TABLE  (1:NVEG) = WSTRC  (1:NVEG)
    NoahmpIO%LAIMIN_TABLE (1:NVEG) = LAIMIN (1:NVEG)
    NoahmpIO%XSAMIN_TABLE (1:NVEG) = XSAMIN (1:NVEG)

    NoahmpIO%SAIM_TABLE(1:NVEG, 1) = SAI_JAN(1:NVEG)
    NoahmpIO%SAIM_TABLE(1:NVEG, 2) = SAI_FEB(1:NVEG)
    NoahmpIO%SAIM_TABLE(1:NVEG, 3) = SAI_MAR(1:NVEG)
    NoahmpIO%SAIM_TABLE(1:NVEG, 4) = SAI_APR(1:NVEG)
    NoahmpIO%SAIM_TABLE(1:NVEG, 5) = SAI_MAY(1:NVEG)
    NoahmpIO%SAIM_TABLE(1:NVEG, 6) = SAI_JUN(1:NVEG)
    NoahmpIO%SAIM_TABLE(1:NVEG, 7) = SAI_JUL(1:NVEG)
    NoahmpIO%SAIM_TABLE(1:NVEG, 8) = SAI_AUG(1:NVEG)
    NoahmpIO%SAIM_TABLE(1:NVEG, 9) = SAI_SEP(1:NVEG)
    NoahmpIO%SAIM_TABLE(1:NVEG,10) = SAI_OCT(1:NVEG)
    NoahmpIO%SAIM_TABLE(1:NVEG,11) = SAI_NOV(1:NVEG)
    NoahmpIO%SAIM_TABLE(1:NVEG,12) = SAI_DEC(1:NVEG)
    NoahmpIO%LAIM_TABLE(1:NVEG, 1) = LAI_JAN(1:NVEG)
    NoahmpIO%LAIM_TABLE(1:NVEG, 2) = LAI_FEB(1:NVEG)
    NoahmpIO%LAIM_TABLE(1:NVEG, 3) = LAI_MAR(1:NVEG)
    NoahmpIO%LAIM_TABLE(1:NVEG, 4) = LAI_APR(1:NVEG)
    NoahmpIO%LAIM_TABLE(1:NVEG, 5) = LAI_MAY(1:NVEG)
    NoahmpIO%LAIM_TABLE(1:NVEG, 6) = LAI_JUN(1:NVEG)
    NoahmpIO%LAIM_TABLE(1:NVEG, 7) = LAI_JUL(1:NVEG)
    NoahmpIO%LAIM_TABLE(1:NVEG, 8) = LAI_AUG(1:NVEG)
    NoahmpIO%LAIM_TABLE(1:NVEG, 9) = LAI_SEP(1:NVEG)
    NoahmpIO%LAIM_TABLE(1:NVEG,10) = LAI_OCT(1:NVEG)
    NoahmpIO%LAIM_TABLE(1:NVEG,11) = LAI_NOV(1:NVEG)
    NoahmpIO%LAIM_TABLE(1:NVEG,12) = LAI_DEC(1:NVEG)
    NoahmpIO%RHOL_TABLE(1:NVEG,1)  = RHOL_VIS(1:NVEG) !leaf reflectance: 1=vis, 2=nir
    NoahmpIO%RHOL_TABLE(1:NVEG,2)  = RHOL_NIR(1:NVEG) !leaf reflectance: 1=vis, 2=nir
    NoahmpIO%RHOS_TABLE(1:NVEG,1)  = RHOS_VIS(1:NVEG) !stem reflectance: 1=vis, 2=nir
    NoahmpIO%RHOS_TABLE(1:NVEG,2)  = RHOS_NIR(1:NVEG) !stem reflectance: 1=vis, 2=nir
    NoahmpIO%TAUL_TABLE(1:NVEG,1)  = TAUL_VIS(1:NVEG) !leaf transmittance: 1=vis, 2=nir
    NoahmpIO%TAUL_TABLE(1:NVEG,2)  = TAUL_NIR(1:NVEG) !leaf transmittance: 1=vis, 2=nir
    NoahmpIO%TAUS_TABLE(1:NVEG,1)  = TAUS_VIS(1:NVEG) !stem transmittance: 1=vis, 2=nir
    NoahmpIO%TAUS_TABLE(1:NVEG,2)  = TAUS_NIR(1:NVEG) !stem transmittance: 1=vis, 2=nir

    !---------------- NoahmpTable.TBL soil parameters
    inquire( file='NoahmpTable.TBL', exist=file_named )
    if ( file_named ) then
       open(15, file="NoahmpTable.TBL", status='old', form='formatted', action='read', iostat=ierr)
    else
       open(15, status='old', form='formatted', action='read', iostat=ierr)
    end if
    if ( ierr /= 0 ) then
       write(*,'("WARNING: Cannot find file NoahmpTable.TBL")')
    endif
    read(15, noahmp_stas_soil_categories)
    if ( trim(SLTYPE) == "STAS" ) then
       read(15, noahmp_soil_stas_parameters)
    elseif ( trim(SLTYPE) == "STAS_RUC" ) then
       read(15, noahmp_soil_stas_ruc_parameters)
    else
       write(*,'("WARNING: Unrecognized SOILTYPE in subroutine ReadNoahmpTable")')
       write(*,'("WARNING: DATASET_IDENTIFIER = ''", A, "''")') trim(SLTYPE)
    endif
    close(15)

    ! assign values
    NoahmpIO%SLCATS_TABLE           = SLCATS
    NoahmpIO%BEXP_TABLE  (1:SLCATS) = BB    (1:SLCATS)
    NoahmpIO%SMCDRY_TABLE(1:SLCATS) = DRYSMC(1:SLCATS)
    NoahmpIO%SMCMAX_TABLE(1:SLCATS) = MAXSMC(1:SLCATS)
    NoahmpIO%SMCREF_TABLE(1:SLCATS) = REFSMC(1:SLCATS)
    NoahmpIO%PSISAT_TABLE(1:SLCATS) = SATPSI(1:SLCATS)
    NoahmpIO%DKSAT_TABLE (1:SLCATS) = SATDK (1:SLCATS)
    NoahmpIO%DWSAT_TABLE (1:SLCATS) = SATDW (1:SLCATS)
    NoahmpIO%SMCWLT_TABLE(1:SLCATS) = WLTSMC(1:SLCATS)
    NoahmpIO%QUARTZ_TABLE(1:SLCATS) = QTZ   (1:SLCATS)
    NoahmpIO%BVIC_TABLE  (1:SLCATS) = BVIC  (1:SLCATS)
    NoahmpIO%AXAJ_TABLE  (1:SLCATS) = AXAJ  (1:SLCATS)
    NoahmpIO%BXAJ_TABLE  (1:SLCATS) = BXAJ  (1:SLCATS)
    NoahmpIO%XXAJ_TABLE  (1:SLCATS) = XXAJ  (1:SLCATS)
    NoahmpIO%BDVIC_TABLE (1:SLCATS) = BDVIC (1:SLCATS)
    NoahmpIO%GDVIC_TABLE (1:SLCATS) = GDVIC (1:SLCATS)
    NoahmpIO%BBVIC_TABLE (1:SLCATS) = BBVIC (1:SLCATS)

    !---------------- NoahmpTable.TBL general parameters
    inquire( file='NoahmpTable.TBL', exist=file_named )
    if ( file_named ) then
       open(15, file="NoahmpTable.TBL", status='old', form='formatted', action='read', iostat=ierr)
    else
       open(15, status='old', form='formatted', action='read', iostat=ierr)
    end if
    if ( ierr /= 0 ) then
       write(*,'("WARNING: Cannot find file NoahmpTable.TBL")')
    endif
    read(15, noahmp_general_parameters)
    close(15)

    ! assign values
    NoahmpIO%SLOPE_TABLE(1:NUM_SLOPE) = SLOPE_DATA(1:NUM_SLOPE)
    NoahmpIO%CSOIL_TABLE              = CSOIL_DATA
    NoahmpIO%REFDK_TABLE              = REFDK_DATA
    NoahmpIO%REFKDT_TABLE             = REFKDT_DATA
    NoahmpIO%FRZK_TABLE               = FRZK_DATA
    NoahmpIO%ZBOT_TABLE               = ZBOT_DATA
    NoahmpIO%CZIL_TABLE               = CZIL_DATA

    !---------------- NoahmpTable.TBL radiation parameters
    inquire( file='NoahmpTable.TBL', exist=file_named )
    if ( file_named ) then
      open(15, file="NoahmpTable.TBL", status='old', form='formatted', action='read', iostat=ierr)
    else
      open(15, status='old', form='formatted', action='read', iostat=ierr)
    end if
    if (ierr /= 0) then
       write(*,'("WARNING: Cannot find file NoahmpTable.TBL")')
    endif
    read(15,noahmp_rad_parameters)
    close(15)

    ! assign values
    NoahmpIO%ALBSAT_TABLE(:,1) = ALBSAT_VIS ! saturated soil albedos: 1=vis, 2=nir
    NoahmpIO%ALBSAT_TABLE(:,2) = ALBSAT_NIR ! saturated soil albedos: 1=vis, 2=nir
    NoahmpIO%ALBDRY_TABLE(:,1) = ALBDRY_VIS ! dry soil albedos: 1=vis, 2=nir
    NoahmpIO%ALBDRY_TABLE(:,2) = ALBDRY_NIR ! dry soil albedos: 1=vis, 2=nir
    NoahmpIO%ALBICE_TABLE      = ALBICE
    NoahmpIO%ALBLAK_TABLE      = ALBLAK
    NoahmpIO%OMEGAS_TABLE      = OMEGAS
    NoahmpIO%BETADS_TABLE      = BETADS
    NoahmpIO%BETAIS_TABLE      = BETAIS
    NoahmpIO%EG_TABLE          = EG
    NoahmpIO%EICE_TABLE        = EICE

    !---------------- NoahmpTable.TBL global parameters
    inquire( file='NoahmpTable.TBL', exist=file_named )
    if ( file_named ) then
      open(15, file="NoahmpTable.TBL", status='old', form='formatted', action='read', iostat=ierr)
    else
      open(15, status='old', form='formatted', action='read', iostat=ierr)
    end if
    if (ierr /= 0) then
       write(*,'("WARNING: Cannot find file NoahmpTable.TBL")')
    endif
    read(15,noahmp_global_parameters)
    close(15)

    ! assign values
    NoahmpIO%CO2_TABLE              = CO2
    NoahmpIO%O2_TABLE               = O2
    NoahmpIO%TIMEAN_TABLE           = TIMEAN
    NoahmpIO%FSATMX_TABLE           = FSATMX
    NoahmpIO%Z0SNO_TABLE            = Z0SNO
    NoahmpIO%SSI_TABLE              = SSI
    NoahmpIO%SNOW_RET_FAC_TABLE     = SNOW_RET_FAC
    NoahmpIO%SNOW_EMIS_TABLE        = SNOW_EMIS
    NoahmpIO%SWEMX_TABLE            = SWEMX
    NoahmpIO%TAU0_TABLE             = TAU0
    NoahmpIO%GRAIN_GROWTH_TABLE     = GRAIN_GROWTH
    NoahmpIO%EXTRA_GROWTH_TABLE     = EXTRA_GROWTH
    NoahmpIO%DIRT_SOOT_TABLE        = DIRT_SOOT
    NoahmpIO%BATS_COSZ_TABLE        = BATS_COSZ
    NoahmpIO%BATS_VIS_NEW_TABLE     = BATS_VIS_NEW
    NoahmpIO%BATS_NIR_NEW_TABLE     = BATS_NIR_NEW
    NoahmpIO%BATS_VIS_AGE_TABLE     = BATS_VIS_AGE
    NoahmpIO%BATS_NIR_AGE_TABLE     = BATS_NIR_AGE
    NoahmpIO%BATS_VIS_DIR_TABLE     = BATS_VIS_DIR
    NoahmpIO%BATS_NIR_DIR_TABLE     = BATS_NIR_DIR
    NoahmpIO%RSURF_SNOW_TABLE       = RSURF_SNOW
    NoahmpIO%RSURF_EXP_TABLE        = RSURF_EXP
    NoahmpIO%C2_SNOWCOMPACT_TABLE   = C2_SNOWCOMPACT
    NoahmpIO%C3_SNOWCOMPACT_TABLE   = C3_SNOWCOMPACT
    NoahmpIO%C4_SNOWCOMPACT_TABLE   = C4_SNOWCOMPACT
    NoahmpIO%C5_SNOWCOMPACT_TABLE   = C5_SNOWCOMPACT
    NoahmpIO%DM_SNOWCOMPACT_TABLE   = DM_SNOWCOMPACT
    NoahmpIO%ETA0_SNOWCOMPACT_TABLE = ETA0_SNOWCOMPACT
    NoahmpIO%SNLIQMAXFRAC_TABLE     = SNLIQMAXFRAC
    NoahmpIO%SWEMAXGLA_TABLE        = SWEMAXGLA
    NoahmpIO%WSLMAX_TABLE           = WSLMAX
    NoahmpIO%ROUS_TABLE             = ROUS
    NoahmpIO%CMIC_TABLE             = CMIC
    NoahmpIO%SNOWDEN_MAX_TABLE      = SNOWDEN_MAX
    NoahmpIO%CLASS_ALB_REF_TABLE    = CLASS_ALB_REF
    NoahmpIO%CLASS_SNO_AGE_TABLE    = CLASS_SNO_AGE
    NoahmpIO%CLASS_ALB_NEW_TABLE    = CLASS_ALB_NEW
    NoahmpIO%PSIWLT_TABLE           = PSIWLT
    NoahmpIO%Z0SOIL_TABLE           = Z0SOIL
    NoahmpIO%Z0LAKE_TABLE           = Z0LAKE

    !---------------- NoahmpTable.TBL irrigation parameters
    inquire( file='NoahmpTable.TBL', exist=file_named )
    if ( file_named ) then
      open(15, file="NoahmpTable.TBL", status='old', form='formatted', action='read', iostat=ierr)
    else
      open(15, status='old', form='formatted', action='read', iostat=ierr)
    end if
    if (ierr /= 0) then
       write(*,'("WARNING: Cannot find file NoahmpTable.TBL")')
    endif
    read(15,noahmp_irrigation_parameters)
    close(15)
    if ( (FILOSS < 0.0) .or. (FILOSS > 0.99) ) then
       write(*,'("WARNING: FILOSS should be >=0.0 and <=0.99")')
       stop "STOP in NoahMP_irrigation_parameters"
    endif

    ! assign values
    NoahmpIO%IRR_FRAC_TABLE   = IRR_FRAC
    NoahmpIO%IRR_HAR_TABLE    = IRR_HAR
    NoahmpIO%IRR_LAI_TABLE    = IRR_LAI
    NoahmpIO%IRR_MAD_TABLE    = IRR_MAD
    NoahmpIO%FILOSS_TABLE     = FILOSS  
    NoahmpIO%SPRIR_RATE_TABLE = SPRIR_RATE
    NoahmpIO%MICIR_RATE_TABLE = MICIR_RATE
    NoahmpIO%FIRTFAC_TABLE    = FIRTFAC
    NoahmpIO%IR_RAIN_TABLE    = IR_RAIN 

    !---------------- NoahmpTable.TBL crop parameters
    inquire( file='NoahmpTable.TBL', exist=file_named )
    if ( file_named ) then
      open(15, file="NoahmpTable.TBL", status='old', form='formatted', action='read', iostat=ierr)
    else
      open(15, status='old', form='formatted', action='read', iostat=ierr)
    end if
    if (ierr /= 0) then
       write(*,'("WARNING: Cannot find file NoahmpTable.TBL")')
    endif
    read(15,noahmp_crop_parameters)
    close(15)

    ! assign values
    NoahmpIO%DEFAULT_CROP_TABLE     = DEFAULT_CROP
    NoahmpIO%PLTDAY_TABLE           = PLTDAY
    NoahmpIO%HSDAY_TABLE            = HSDAY
    NoahmpIO%PLANTPOP_TABLE         = PLANTPOP
    NoahmpIO%IRRI_TABLE             = IRRI
    NoahmpIO%GDDTBASE_TABLE         = GDDTBASE
    NoahmpIO%GDDTCUT_TABLE          = GDDTCUT
    NoahmpIO%GDDS1_TABLE            = GDDS1
    NoahmpIO%GDDS2_TABLE            = GDDS2
    NoahmpIO%GDDS3_TABLE            = GDDS3
    NoahmpIO%GDDS4_TABLE            = GDDS4
    NoahmpIO%GDDS5_TABLE            = GDDS5
    NoahmpIO%C3PSNI_TABLE (1:5)     = C3PSNI (1:5)
    NoahmpIO%KC25I_TABLE  (1:5)     = KC25I  (1:5)
    NoahmpIO%AKCI_TABLE   (1:5)     = AKCI   (1:5)
    NoahmpIO%KO25I_TABLE  (1:5)     = KO25I  (1:5)
    NoahmpIO%AKOI_TABLE   (1:5)     = AKOI   (1:5)
    NoahmpIO%AVCMXI_TABLE (1:5)     = AVCMXI (1:5)
    NoahmpIO%VCMX25I_TABLE(1:5)     = VCMX25I(1:5)
    NoahmpIO%BPI_TABLE    (1:5)     = BPI    (1:5)
    NoahmpIO%MPI_TABLE    (1:5)     = MPI    (1:5)
    NoahmpIO%FOLNMXI_TABLE(1:5)     = FOLNMXI(1:5)
    NoahmpIO%QE25I_TABLE  (1:5)     = QE25I  (1:5)
    NoahmpIO%AREF_TABLE             = AREF
    NoahmpIO%PSNRF_TABLE            = PSNRF
    NoahmpIO%I2PAR_TABLE            = I2PAR
    NoahmpIO%TASSIM0_TABLE          = TASSIM0
    NoahmpIO%TASSIM1_TABLE          = TASSIM1
    NoahmpIO%TASSIM2_TABLE          = TASSIM2
    NoahmpIO%K_TABLE                = K
    NoahmpIO%EPSI_TABLE             = EPSI
    NoahmpIO%Q10MR_TABLE            = Q10MR
    NoahmpIO%LEFREEZ_TABLE          = LEFREEZ
    NoahmpIO%FRA_GR_TABLE           = FRA_GR
    NoahmpIO%LFMR25_TABLE           = LFMR25
    NoahmpIO%STMR25_TABLE           = STMR25
    NoahmpIO%RTMR25_TABLE           = RTMR25
    NoahmpIO%GRAINMR25_TABLE        = GRAINMR25
    NoahmpIO%BIO2LAI_TABLE          = BIO2LAI
    NoahmpIO%DILE_FC_TABLE(:,1)     = DILE_FC_S1
    NoahmpIO%DILE_FC_TABLE(:,2)     = DILE_FC_S2
    NoahmpIO%DILE_FC_TABLE(:,3)     = DILE_FC_S3
    NoahmpIO%DILE_FC_TABLE(:,4)     = DILE_FC_S4
    NoahmpIO%DILE_FC_TABLE(:,5)     = DILE_FC_S5
    NoahmpIO%DILE_FC_TABLE(:,6)     = DILE_FC_S6
    NoahmpIO%DILE_FC_TABLE(:,7)     = DILE_FC_S7
    NoahmpIO%DILE_FC_TABLE(:,8)     = DILE_FC_S8
    NoahmpIO%DILE_FW_TABLE(:,1)     = DILE_FW_S1
    NoahmpIO%DILE_FW_TABLE(:,2)     = DILE_FW_S2
    NoahmpIO%DILE_FW_TABLE(:,3)     = DILE_FW_S3
    NoahmpIO%DILE_FW_TABLE(:,4)     = DILE_FW_S4
    NoahmpIO%DILE_FW_TABLE(:,5)     = DILE_FW_S5
    NoahmpIO%DILE_FW_TABLE(:,6)     = DILE_FW_S6
    NoahmpIO%DILE_FW_TABLE(:,7)     = DILE_FW_S7
    NoahmpIO%DILE_FW_TABLE(:,8)     = DILE_FW_S8
    NoahmpIO%LF_OVRC_TABLE(:,1)     = LF_OVRC_S1
    NoahmpIO%LF_OVRC_TABLE(:,2)     = LF_OVRC_S2
    NoahmpIO%LF_OVRC_TABLE(:,3)     = LF_OVRC_S3
    NoahmpIO%LF_OVRC_TABLE(:,4)     = LF_OVRC_S4
    NoahmpIO%LF_OVRC_TABLE(:,5)     = LF_OVRC_S5
    NoahmpIO%LF_OVRC_TABLE(:,6)     = LF_OVRC_S6
    NoahmpIO%LF_OVRC_TABLE(:,7)     = LF_OVRC_S7
    NoahmpIO%LF_OVRC_TABLE(:,8)     = LF_OVRC_S8
    NoahmpIO%ST_OVRC_TABLE(:,1)     = ST_OVRC_S1
    NoahmpIO%ST_OVRC_TABLE(:,2)     = ST_OVRC_S2
    NoahmpIO%ST_OVRC_TABLE(:,3)     = ST_OVRC_S3
    NoahmpIO%ST_OVRC_TABLE(:,4)     = ST_OVRC_S4
    NoahmpIO%ST_OVRC_TABLE(:,5)     = ST_OVRC_S5
    NoahmpIO%ST_OVRC_TABLE(:,6)     = ST_OVRC_S6
    NoahmpIO%ST_OVRC_TABLE(:,7)     = ST_OVRC_S7
    NoahmpIO%ST_OVRC_TABLE(:,8)     = ST_OVRC_S8
    NoahmpIO%RT_OVRC_TABLE(:,1)     = RT_OVRC_S1
    NoahmpIO%RT_OVRC_TABLE(:,2)     = RT_OVRC_S2
    NoahmpIO%RT_OVRC_TABLE(:,3)     = RT_OVRC_S3
    NoahmpIO%RT_OVRC_TABLE(:,4)     = RT_OVRC_S4
    NoahmpIO%RT_OVRC_TABLE(:,5)     = RT_OVRC_S5
    NoahmpIO%RT_OVRC_TABLE(:,6)     = RT_OVRC_S6
    NoahmpIO%RT_OVRC_TABLE(:,7)     = RT_OVRC_S7
    NoahmpIO%RT_OVRC_TABLE(:,8)     = RT_OVRC_S8
    NoahmpIO%LFPT_TABLE   (:,1)     = LFPT_S1
    NoahmpIO%LFPT_TABLE   (:,2)     = LFPT_S2
    NoahmpIO%LFPT_TABLE   (:,3)     = LFPT_S3
    NoahmpIO%LFPT_TABLE   (:,4)     = LFPT_S4
    NoahmpIO%LFPT_TABLE   (:,5)     = LFPT_S5
    NoahmpIO%LFPT_TABLE   (:,6)     = LFPT_S6
    NoahmpIO%LFPT_TABLE   (:,7)     = LFPT_S7
    NoahmpIO%LFPT_TABLE   (:,8)     = LFPT_S8
    NoahmpIO%STPT_TABLE   (:,1)     = STPT_S1
    NoahmpIO%STPT_TABLE   (:,2)     = STPT_S2
    NoahmpIO%STPT_TABLE   (:,3)     = STPT_S3
    NoahmpIO%STPT_TABLE   (:,4)     = STPT_S4
    NoahmpIO%STPT_TABLE   (:,5)     = STPT_S5
    NoahmpIO%STPT_TABLE   (:,6)     = STPT_S6
    NoahmpIO%STPT_TABLE   (:,7)     = STPT_S7
    NoahmpIO%STPT_TABLE   (:,8)     = STPT_S8
    NoahmpIO%RTPT_TABLE   (:,1)     = RTPT_S1
    NoahmpIO%RTPT_TABLE   (:,2)     = RTPT_S2
    NoahmpIO%RTPT_TABLE   (:,3)     = RTPT_S3
    NoahmpIO%RTPT_TABLE   (:,4)     = RTPT_S4
    NoahmpIO%RTPT_TABLE   (:,5)     = RTPT_S5
    NoahmpIO%RTPT_TABLE   (:,6)     = RTPT_S6
    NoahmpIO%RTPT_TABLE   (:,7)     = RTPT_S7
    NoahmpIO%RTPT_TABLE   (:,8)     = RTPT_S8
    NoahmpIO%GRAINPT_TABLE(:,1)     = GRAINPT_S1
    NoahmpIO%GRAINPT_TABLE(:,2)     = GRAINPT_S2
    NoahmpIO%GRAINPT_TABLE(:,3)     = GRAINPT_S3
    NoahmpIO%GRAINPT_TABLE(:,4)     = GRAINPT_S4
    NoahmpIO%GRAINPT_TABLE(:,5)     = GRAINPT_S5
    NoahmpIO%GRAINPT_TABLE(:,6)     = GRAINPT_S6
    NoahmpIO%GRAINPT_TABLE(:,7)     = GRAINPT_S7
    NoahmpIO%GRAINPT_TABLE(:,8)     = GRAINPT_S8
    NoahmpIO%LFCT_TABLE   (:,1)     = LFCT_S1
    NoahmpIO%LFCT_TABLE   (:,2)     = LFCT_S2
    NoahmpIO%LFCT_TABLE   (:,3)     = LFCT_S3
    NoahmpIO%LFCT_TABLE   (:,4)     = LFCT_S4
    NoahmpIO%LFCT_TABLE   (:,5)     = LFCT_S5
    NoahmpIO%LFCT_TABLE   (:,6)     = LFCT_S6
    NoahmpIO%LFCT_TABLE   (:,7)     = LFCT_S7
    NoahmpIO%LFCT_TABLE   (:,8)     = LFCT_S8
    NoahmpIO%STCT_TABLE   (:,1)     = STCT_S1
    NoahmpIO%STCT_TABLE   (:,2)     = STCT_S2
    NoahmpIO%STCT_TABLE   (:,3)     = STCT_S3
    NoahmpIO%STCT_TABLE   (:,4)     = STCT_S4
    NoahmpIO%STCT_TABLE   (:,5)     = STCT_S5
    NoahmpIO%STCT_TABLE   (:,6)     = STCT_S6
    NoahmpIO%STCT_TABLE   (:,7)     = STCT_S7
    NoahmpIO%STCT_TABLE   (:,8)     = STCT_S8
    NoahmpIO%RTCT_TABLE   (:,1)     = RTCT_S1
    NoahmpIO%RTCT_TABLE   (:,2)     = RTCT_S2
    NoahmpIO%RTCT_TABLE   (:,3)     = RTCT_S3
    NoahmpIO%RTCT_TABLE   (:,4)     = RTCT_S4
    NoahmpIO%RTCT_TABLE   (:,5)     = RTCT_S5
    NoahmpIO%RTCT_TABLE   (:,6)     = RTCT_S6
    NoahmpIO%RTCT_TABLE   (:,7)     = RTCT_S7
    NoahmpIO%RTCT_TABLE   (:,8)     = RTCT_S8

    !---------------- NoahmpTable.TBL tile drainage parameters
    inquire( file='NoahmpTable.TBL', exist=file_named )
    if ( file_named ) then
      open(15, file="NoahmpTable.TBL", status='old', form='formatted', action='read', iostat=ierr)
    else
      open(15, status='old', form='formatted', action='read', iostat=ierr)
    end if
    if (ierr /= 0) then
       write(*,'("WARNING: Cannot find file NoahmpTable.TBL")')
    endif
    read(15,noahmp_tiledrain_parameters)
    close(15)

    ! assign values
    NoahmpIO%DRAIN_LAYER_OPT_TABLE        = DRAIN_LAYER_OPT
    NoahmpIO%TDSMC_FAC_TABLE(1:NSOILTYPE) = TDSMC_FAC(1:NSOILTYPE)
    NoahmpIO%TD_DEPTH_TABLE (1:NSOILTYPE) = TD_DEPTH (1:NSOILTYPE)
    NoahmpIO%TD_DC_TABLE    (1:NSOILTYPE) = TD_DC    (1:NSOILTYPE)
    NoahmpIO%TD_DCOEF_TABLE (1:NSOILTYPE) = TD_DCOEF (1:NSOILTYPE)
    NoahmpIO%TD_D_TABLE     (1:NSOILTYPE) = TD_D     (1:NSOILTYPE)
    NoahmpIO%TD_ADEPTH_TABLE(1:NSOILTYPE) = TD_ADEPTH(1:NSOILTYPE)
    NoahmpIO%TD_RADI_TABLE  (1:NSOILTYPE) = TD_RADI  (1:NSOILTYPE)
    NoahmpIO%TD_SPAC_TABLE  (1:NSOILTYPE) = TD_SPAC  (1:NSOILTYPE)
    NoahmpIO%TD_DDRAIN_TABLE(1:NSOILTYPE) = TD_DDRAIN(1:NSOILTYPE)
    NoahmpIO%KLAT_FAC_TABLE (1:NSOILTYPE) = KLAT_FAC (1:NSOILTYPE)

    !---------------- NoahmpTable.TBL optional parameters
    inquire( file='NoahmpTable.TBL', exist=file_named )
    if ( file_named ) then
      open(15, file="NoahmpTable.TBL", status='old', form='formatted', action='read', iostat=ierr)
    else
      open(15, status='old', form='formatted', action='read', iostat=ierr)
    end if
    if (ierr /= 0) then
       write(*,'("WARNING: Cannot find file NoahmpTable.TBL")')
    endif
    read(15,noahmp_optional_parameters)
    close(15)

    ! assign values
    NoahmpIO%sr2006_theta_1500t_a_TABLE = sr2006_theta_1500t_a
    NoahmpIO%sr2006_theta_1500t_b_TABLE = sr2006_theta_1500t_b
    NoahmpIO%sr2006_theta_1500t_c_TABLE = sr2006_theta_1500t_c
    NoahmpIO%sr2006_theta_1500t_d_TABLE = sr2006_theta_1500t_d
    NoahmpIO%sr2006_theta_1500t_e_TABLE = sr2006_theta_1500t_e
    NoahmpIO%sr2006_theta_1500t_f_TABLE = sr2006_theta_1500t_f
    NoahmpIO%sr2006_theta_1500t_g_TABLE = sr2006_theta_1500t_g
    NoahmpIO%sr2006_theta_1500_a_TABLE  = sr2006_theta_1500_a
    NoahmpIO%sr2006_theta_1500_b_TABLE  = sr2006_theta_1500_b
    NoahmpIO%sr2006_theta_33t_a_TABLE   = sr2006_theta_33t_a
    NoahmpIO%sr2006_theta_33t_b_TABLE   = sr2006_theta_33t_b
    NoahmpIO%sr2006_theta_33t_c_TABLE   = sr2006_theta_33t_c
    NoahmpIO%sr2006_theta_33t_d_TABLE   = sr2006_theta_33t_d
    NoahmpIO%sr2006_theta_33t_e_TABLE   = sr2006_theta_33t_e
    NoahmpIO%sr2006_theta_33t_f_TABLE   = sr2006_theta_33t_f
    NoahmpIO%sr2006_theta_33t_g_TABLE   = sr2006_theta_33t_g
    NoahmpIO%sr2006_theta_33_a_TABLE    = sr2006_theta_33_a
    NoahmpIO%sr2006_theta_33_b_TABLE    = sr2006_theta_33_b
    NoahmpIO%sr2006_theta_33_c_TABLE    = sr2006_theta_33_c
    NoahmpIO%sr2006_theta_s33t_a_TABLE  = sr2006_theta_s33t_a
    NoahmpIO%sr2006_theta_s33t_b_TABLE  = sr2006_theta_s33t_b
    NoahmpIO%sr2006_theta_s33t_c_TABLE  = sr2006_theta_s33t_c
    NoahmpIO%sr2006_theta_s33t_d_TABLE  = sr2006_theta_s33t_d
    NoahmpIO%sr2006_theta_s33t_e_TABLE  = sr2006_theta_s33t_e
    NoahmpIO%sr2006_theta_s33t_f_TABLE  = sr2006_theta_s33t_f
    NoahmpIO%sr2006_theta_s33t_g_TABLE  = sr2006_theta_s33t_g
    NoahmpIO%sr2006_theta_s33_a_TABLE   = sr2006_theta_s33_a
    NoahmpIO%sr2006_theta_s33_b_TABLE   = sr2006_theta_s33_b
    NoahmpIO%sr2006_psi_et_a_TABLE      = sr2006_psi_et_a
    NoahmpIO%sr2006_psi_et_b_TABLE      = sr2006_psi_et_b
    NoahmpIO%sr2006_psi_et_c_TABLE      = sr2006_psi_et_c
    NoahmpIO%sr2006_psi_et_d_TABLE      = sr2006_psi_et_d
    NoahmpIO%sr2006_psi_et_e_TABLE      = sr2006_psi_et_e
    NoahmpIO%sr2006_psi_et_f_TABLE      = sr2006_psi_et_f
    NoahmpIO%sr2006_psi_et_g_TABLE      = sr2006_psi_et_g
    NoahmpIO%sr2006_psi_e_a_TABLE       = sr2006_psi_e_a
    NoahmpIO%sr2006_psi_e_b_TABLE       = sr2006_psi_e_b
    NoahmpIO%sr2006_psi_e_c_TABLE       = sr2006_psi_e_c
    NoahmpIO%sr2006_smcmax_a_TABLE      = sr2006_smcmax_a
    NoahmpIO%sr2006_smcmax_b_TABLE      = sr2006_smcmax_b


  end subroutine NoahmpReadTable

end module NoahmpReadTableMod
