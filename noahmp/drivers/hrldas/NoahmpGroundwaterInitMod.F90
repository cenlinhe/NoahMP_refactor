module NoahmpGroundwaterInitMod

!!!  Module to initialize Noah-MP Groundwater (GW) variables for MMF GW scheme

  use Machine
  use NoahmpIOVarType
  
  implicit none
  
contains

  subroutine NoahmpGroundwaterInitMain(grid, NoahmpIO)

! ------------------------ Code history -------------------------------------
! Original Noah-MP subroutine: GROUNDWATER_INIT
! Original code: Miguez-Macho, Fan et al. (2007)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (He et al. 2023)
! ---------------------------------------------------------------------------

  use GroundWaterMmfMod, only : LATERALFLOW
  use module_domain,     only : domain
  
#if (EM_CORE == 1)
#ifdef DM_PARALLEL
  use module_dm     ,    only : ntasks_x,ntasks_y,local_communicator,mytask,ntasks
  use module_comm_dm,    only : halo_em_hydro_noahmp_sub
#endif
#endif

    implicit none 
    
    type(NoahmpIO_type), intent(inout) :: NoahmpIO
    type(domain), target               :: grid  
    
    ! local variables
    logical                                             :: urbanpt_flag ! added to identify urban pixels
    integer                                             :: I,J,K,ITER,itf,jtf, NITER, NCOUNT,NS
    real(kind=kind_noahmp)                              :: BEXP,SMCMAX,PSISAT,SMCWLT,DWSAT,DKSAT
    real(kind=kind_noahmp)                              :: FRLIQ,SMCEQDEEP
    real(kind=kind_noahmp)                              :: DELTAT,RCOND,TOTWATER
    real(kind=kind_noahmp)                              :: AA,BBB,CC,DD,DX,FUNC,DFUNC,DDZ,EXPON,SMC,FLUX
    real(kind=kind_noahmp), dimension(1:NoahmpIO%NSOIL) :: SMCEQ,ZSOIL
    real(kind=kind_noahmp), dimension(NoahmpIO%ims:NoahmpIO%ime, NoahmpIO%jms:NoahmpIO%jme) :: QLAT, QRF
    ! landmask: -1 for water (ice or no ice) and glacial areas, 1 for land where the LSM does its soil moisture calculations
    integer,                dimension(NoahmpIO%ims:NoahmpIO%ime, NoahmpIO%jms:NoahmpIO%jme) :: LANDMASK 

! --------------------------------------------------------------------------------    
    associate(                                       &
              NSOIL      => NoahmpIO%NSOIL          ,&
              DZS        => NoahmpIO%DZS            ,&
              ISLTYP     => NoahmpIO%ISLTYP         ,&
              IVGTYP     => NoahmpIO%IVGTYP         ,&
              WTDDT      => NoahmpIO%WTDDT          ,&  
              FDEPTH     => NoahmpIO%FDEPTHXY       ,&
              TOPO       => NoahmpIO%TERRAIN        ,&
              RIVERBED   => NoahmpIO%RIVERBEDXY     ,&
              EQWTD      => NoahmpIO%EQZWT          ,&
              RIVERCOND  => NoahmpIO%RIVERCONDXY    ,&
              PEXP       => NoahmpIO%PEXPXY         ,&  
              AREA       => NoahmpIO%AREAXY         ,& 
              WTD        => NoahmpIO%ZWTXY          ,& 
              SMOIS      => NoahmpIO%SMOIS          ,& 
              SH2O       => NoahmpIO%SH2O           ,&  
              SMOISEQ    => NoahmpIO%SMOISEQ        ,&  
              SMCWTDXY   => NoahmpIO%SMCWTDXY       ,&  
              DEEPRECHXY => NoahmpIO%DEEPRECHXY     ,&  
              RECHXY     => NoahmpIO%RECHXY         ,& 
              QLATXY     => NoahmpIO%QLATXY         ,&
              QSLATXY    => NoahmpIO%QSLATXY        ,&
              QRFXY      => NoahmpIO%QRFXY          ,&
              QRFSXY     => NoahmpIO%QRFSXY         ,&
              QSPRINGXY  => NoahmpIO%QSPRINGXY      ,&
              QSPRINGSXY => NoahmpIO%QSPRINGSXY     ,&
              RECHCLIM   => NoahmpIO%RECHCLIM       ,& 
              ids        => NoahmpIO%ids            ,&
              ide        => NoahmpIO%ide            ,&
              jds        => NoahmpIO%jds            ,&
              jde        => NoahmpIO%jde            ,&  
              kds        => NoahmpIO%kds            ,&
              kde        => NoahmpIO%kde            ,&
              ims        => NoahmpIO%ims            ,&
              ime        => NoahmpIO%ime            ,&  
              jms        => NoahmpIO%jms            ,&
              jme        => NoahmpIO%jme            ,&  
              kms        => NoahmpIO%kms            ,&
              kme        => NoahmpIO%kme            ,&
              ips        => NoahmpIO%ims            ,&
              ipe        => NoahmpIO%ime            ,&  
              jps        => NoahmpIO%jms            ,&
              jpe        => NoahmpIO%jme            ,&  
              kps        => NoahmpIO%kms            ,&
              kpe        => NoahmpIO%kme            ,&
              its        => NoahmpIO%its            ,&
              ite        => NoahmpIO%ite            ,&  
              jts        => NoahmpIO%jts            ,&
              jte        => NoahmpIO%jte            ,&  
              kts        => NoahmpIO%kts            ,&
              kte        => NoahmpIO%kte             & 
             )
! -------------------------------------------------------------------------------- 
    
    ! Given the soil layer thicknesses (in DZS), calculate the soil layer depths from the surface.
    ZSOIL(1) = -DZS(1)          ! negative
    do NS = 2, NSOIL
       ZSOIL(NS) = ZSOIL(NS-1) - DZS(NS)
    enddo

    ! initialize grid index
    itf = min0(ite,(ide+1)-1)
    jtf = min0(jte,(jde+1)-1)

    ! initialize land mask
    where ( (IVGTYP /= NoahmpIO%ISWATER_TABLE) .and. (IVGTYP /= NoahmpIO%ISICE_TABLE) )
         LANDMASK = 1
    elsewhere
         LANDMASK = -1
    endwhere
        
    PEXP   = 1.0
    DELTAT = 365.0*24*3600.0 ! 1 year
    
    ! read just the raw aggregated water table from hi-res map, so that it is better compatible with topography
    ! use WTD here, to use the lateral communication routine
    WTD    = EQWTD
    NCOUNT = 0

    do NITER = 1, 500
#if (EM_CORE == 1)
#ifdef DM_PARALLEL
#     include "HALO_EM_HYDRO_NOAHMP.inc"
#endif
#endif
      ! Calculate lateral flow
      if ( (NCOUNT > 0) .or. (NITER == 1) ) then
         QLAT = 0.0
         call LATERALFLOW(NoahmpIO,ISLTYP,WTD,QLAT,FDEPTH,TOPO,LANDMASK,DELTAT,AREA &
                                  ,ids,ide,jds,jde,kds,kde                          & 
                                  ,ims,ime,jms,jme,kms,kme                          &
                                  ,its,ite,jts,jte,kts,kte                          )
         NCOUNT = 0
         do J = jts, jtf
           do I = its, itf
             if ( LANDMASK(I,J) > 0 ) then
               if ( QLAT(i,j) > 1.0e-2 ) then
                  NCOUNT   = NCOUNT+1
                  WTD(I,J) = min(WTD(I,J)+0.25, 0.0)
               endif
             endif
           enddo
         enddo
      endif
    enddo !NITER

#if (EM_CORE == 1)
#ifdef DM_PARALLEL
#     include "HALO_EM_HYDRO_NOAHMP.inc"
#endif
#endif

    EQWTD=WTD

    ! after adjusting, where qlat > 1cm/year now wtd is at the surface.
    ! it may still happen that qlat + rech > 0 and eqwtd-rbed <0. There the wtd can
    ! rise to the surface (poor drainage) but the et will then increase.

    ! now, calculate river conductivity
    do J = jts, jtf
       do I = its, itf
          DDZ = EQWTD(I,J) - (RIVERBED(I,J) - TOPO(I,J))
          ! dont allow riverbed above water table
          if ( DDZ < 0.0 ) then
             RIVERBED(I,J) = TOPO(I,J) + EQWTD(I,J)
             DDZ = 0.0
          endif
          TOTWATER = AREA(I,J) * (QLAT(I,J) + RECHCLIM(I,J)*0.001) / DELTAT
          if ( TOTWATER > 0 ) then
             RIVERCOND(I,J) = TOTWATER / max(DDZ,0.05)
          else
             RIVERCOND(I,J) = 0.01
             ! make riverbed equal to eqwtd, otherwise qrf might be too big...
             RIVERBED(I,J)  = TOPO(I,J) + EQWTD(I,J)
          endif
       enddo
    enddo
    
    ! make riverbed to be height down from the surface instead of above sea level
    RIVERBED = min(RIVERBED-TOPO, 0.0)

    ! now recompute lateral flow and flow to rivers to initialize deep soil moisture
    DELTAT = WTDDT * 60.0 !timestep in seconds for this calculation
    QLAT   = 0.0
    call LATERALFLOW(NoahmpIO,ISLTYP,WTD,QLAT,FDEPTH,TOPO,LANDMASK,DELTAT,AREA &
                             ,ids,ide,jds,jde,kds,kde                          & 
                             ,ims,ime,jms,jme,kms,kme                          &
                             ,its,ite,jts,jte,kts,kte                          )
                        
    ! compute flux from grounwater to rivers in the cell
    do J = jts, jtf
       do I = its, itf
          if ( LANDMASK(I,J) > 0 ) then
             if ( (WTD(I,J) > RIVERBED(I,J)) .and. (EQWTD(I,J) > RIVERBED(I,J)) ) then
                RCOND = RIVERCOND(I,J) * exp(PEXP(I,J) * (WTD(I,J)-EQWTD(I,J)))
             else    
                RCOND = RIVERCOND(I,J)
             endif
             QRF(I,J) = RCOND * (WTD(I,J)-RIVERBED(I,J)) * DELTAT / AREA(I,J)
             ! for now, dont allow it to go from river to groundwater
             QRF(I,J) = max(QRF(I,J), 0.0) 
          else
             QRF(I,J) = 0.0
          endif
       enddo
    enddo
    
    ! now compute eq. soil moisture, change soil moisture to be compatible with the water table and compute deep soil moisture
    do J = jts, jtf
       do I = its, itf

          BEXP   = NoahmpIO%BEXP_TABLE(ISLTYP(I,J))
          SMCMAX = NoahmpIO%SMCMAX_TABLE(ISLTYP(I,J))
          SMCWLT = NoahmpIO%SMCWLT_TABLE(ISLTYP(I,J))                
          ! add urban flag
          urbanpt_flag = .false.
          if ( (IVGTYP(I,J) == NoahmpIO%ISURBAN_TABLE) .or. &
               (IVGTYP(I,J) > NoahmpIO%URBTYPE_beg) ) urbanpt_flag = .true.
          if ( urbanpt_flag .eqv. .true. ) then
             SMCMAX = 0.45         
             SMCWLT = 0.40         
          endif 
          DWSAT  = NoahmpIO%DWSAT_TABLE(ISLTYP(I,J))
          DKSAT  = NoahmpIO%DKSAT_TABLE(ISLTYP(I,J))
          PSISAT = -NoahmpIO%PSISAT_TABLE(ISLTYP(I,J))
          if ( (BEXP > 0.0) .and. (SMCMAX > 0.0) .and. (-PSISAT > 0.0) ) then
             ! initialize equilibrium soil moisture for water table diagnostic
             call EquilibriumSoilMoisture(NSOIL,  ZSOIL, SMCMAX, SMCWLT, DWSAT, DKSAT, BEXP, SMCEQ)  
             SMOISEQ(I,1:NSOIL,J) = SMCEQ(1:NSOIL)

             ! make sure that below the water table the layers are saturated and
             ! initialize the deep soil moisture
             if ( WTD(I,J) < (ZSOIL(NSOIL)-DZS(NSOIL)) ) then
                ! initialize deep soil moisture so that the flux compensates qlat+qrf
                ! use Newton-Raphson method to find soil moisture
                EXPON    = 2.0 * BEXP + 3.0
                DDZ      = ZSOIL(NSOIL) - WTD(I,J)
                CC       = PSISAT / DDZ
                FLUX     = (QLAT(I,J) - QRF(I,J)) / DELTAT
                SMC      = 0.5 * SMCMAX
                do ITER = 1, 100
                   DD    = (SMC + SMCMAX) / (2.0*SMCMAX)
                   AA    = -DKSAT * DD  ** EXPON
                   BBB   = CC * ((SMCMAX / SMC)**BEXP - 1.0) + 1.0 
                   FUNC  = AA * BBB - FLUX
                   DFUNC = -DKSAT * (EXPON / (2.0*SMCMAX)) * DD ** (EXPON - 1.0) * BBB &
                           + AA * CC * (-BEXP) * SMCMAX ** BEXP * SMC ** (-BEXP-1.0)
                   DX    = FUNC / DFUNC
                   SMC   = SMC - DX
                   if ( abs(DX) < 1.0e-6 ) exit
                enddo
                SMCWTDXY(I,J) = max(SMC, 1.0e-4)
             elseif ( WTD(I,J) < ZSOIL(NSOIL) ) then
                SMCEQDEEP     = SMCMAX * ( PSISAT / ( PSISAT - DZS(NSOIL) ) ) ** (1.0/BEXP)
               !SMCEQDEEP     = MAX(SMCEQDEEP,SMCWLT)
                SMCEQDEEP     = max(SMCEQDEEP, 1.0e-4)
                SMCWTDXY(I,J) = SMCMAX * (WTD(I,J) -  (ZSOIL(NSOIL)-DZS(NSOIL))) + &
                                SMCEQDEEP * (ZSOIL(NSOIL) - WTD(I,J))
             else !water table within the resolved layers
               SMCWTDXY(I,J) = SMCMAX
               do K = NSOIL, 2, -1
                  if ( WTD(I,J) >= ZSOIL(K-1) ) then
                     FRLIQ        = SH2O(I,K,J) / SMOIS(I,K,J)
                     SMOIS(I,K,J) = SMCMAX
                     SH2O(I,K,J)  = SMCMAX * FRLIQ
                  else
                     if ( SMOIS(I,K,J) < SMCEQ(K) ) then
                        WTD(I,J)  = ZSOIL(K)
                     else
                        WTD(I,J)  = (SMOIS(I,K,J)*DZS(K) - SMCEQ(K)*ZSOIL(K-1) + SMCMAX*ZSOIL(K)) / &
                                    (SMCMAX - SMCEQ(K)) 
                     endif
                     exit
                  endif
               enddo
             endif
          else
             SMOISEQ (I,1:NSOIL,J) = SMCMAX
             SMCWTDXY(I,J)         = SMCMAX
             WTD(I,J)              = 0.0
          endif
  
          ! zero out some arrays
          QLATXY(I,J)     = 0.0
          QSLATXY(I,J)    = 0.0
          QRFXY(I,J)      = 0.0
          QRFSXY(I,J)     = 0.0
          DEEPRECHXY(I,J) = 0.0
          RECHXY(I,J)     = 0.0
          QSPRINGXY(I,J)  = 0.0
          QSPRINGSXY(I,J) = 0.0

       enddo
    enddo

    end associate
    
  end subroutine NoahmpGroundwaterInitMain

  subroutine EquilibriumSoilMoisture(NSOIL, ZSOIL, SMCMAX, SMCWLT, DWSAT, DKSAT, BEXP, SMCEQ)

    implicit none 

    integer,                                     intent(in)  :: NSOIL !no. of soil layers
    real(kind=kind_noahmp),                      intent(in)  :: SMCMAX , SMCWLT, BEXP , DWSAT, DKSAT
    real(kind=kind_noahmp),  dimension(1:NSOIL), intent(in)  :: ZSOIL !depth of soil layer-bottom [m]
    real(kind=kind_noahmp),  dimension(1:NSOIL), intent(out) :: SMCEQ  !equilibrium soil water  content [m3/m3]

    ! local variables
    integer                                                  :: K, ITER
    real(kind=kind_noahmp)                                   :: DDZ, SMC, FUNC, DFUNC, AA, BB, EXPON, DX
    ! -------------------------------------------------------------------------------- 
                                                 
    ! gmm compute equilibrium soil moisture content for the layer when wtd=zsoil(k)
     do K = 1, NSOIL
        if ( K == 1 ) then
           DDZ = -ZSOIL(K+1) * 0.5
        elseif ( K < NSOIL ) then
           DDZ = ( ZSOIL(K-1) - ZSOIL(K+1) ) * 0.5
        else
           DDZ = ZSOIL(K-1) - ZSOIL(K)
        endif

        ! use Newton-Raphson method to find eq soil moisture
        EXPON   = BEXP + 1.0
        AA      = DWSAT / DDZ
        BB      = DKSAT / SMCMAX ** EXPON
        SMC     = 0.5 * SMCMAX
        do ITER = 1, 100
          FUNC  = (SMC - SMCMAX) * AA +  BB * SMC ** EXPON
          DFUNC = AA + BB * EXPON * SMC ** BEXP 
          DX    = FUNC / DFUNC
          SMC   = SMC - DX
          if ( abs(DX) < 1.0e-6 ) exit
        enddo

!       SMCEQ(K) = min(max(SMC,SMCWLT),SMCMAX*0.99)
        SMCEQ(K) = min(max(SMC,1.0e-4), SMCMAX*0.99)
     enddo

  end subroutine EquilibriumSoilMoisture

end module NoahmpGroundwaterInitMod
