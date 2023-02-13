module NoahmpInitMainMod

!!!  Module to initialize Noah-MP 2-D variables

  use Machine
  use NoahmpIOVarType
  use NoahmpSnowInitMod
 
  implicit none
  
contains

  subroutine NoahmpInitMain(NoahmpIO)

! ------------------------ Code history -------------------------------------
! Original Noah-MP subroutine: NOAHMP_INIT
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (He et al. 2023)
! ---------------------------------------------------------------------------

    implicit none 
   
    type(NoahmpIO_type), intent(inout) :: NoahmpIO

    ! local variables
    integer                                     :: ids, ide, jds, jde, kds, kde, &
                                                   ims, ime, jms, jme, kms, kme, &
                                                   its, ite, jts, jte, kts, kte
    integer                                     :: I,J,errflag,itf,jtf,ns
    logical                                     :: urbanpt_flag
    real(kind=kind_noahmp)                      :: BEXP, SMCMAX, PSISAT
    real(kind=kind_noahmp)                      :: FK, massLAI, masssai
    real(kind=kind_noahmp), parameter           :: BLIM  = 5.5
    real(kind=kind_noahmp), parameter           :: HLICE = 3.335E5
    real(kind=kind_noahmp), parameter           :: GRAV0 = 9.81
    real(kind=kind_noahmp), parameter           :: T0    = 273.15

! --------------------------------------------------------------------------- 
    associate(                                                   &
              SNOW                => NoahmpIO%SNOW              ,&
              SNOWH               => NoahmpIO%SNOWH             ,&
              CANWAT              => NoahmpIO%CANWAT            ,& 
              ISLTYP              => NoahmpIO%ISLTYP            ,&
              IVGTYP              => NoahmpIO%IVGTYP            ,&
              TSLB                => NoahmpIO%TSLB              ,&
              SMOIS               => NoahmpIO%SMOIS             ,&
              SH2O                => NoahmpIO%SH2O              ,&
              DZS                 => NoahmpIO%DZS               ,&
              TSK                 => NoahmpIO%TSK               ,&
              ISNOWXY             => NoahmpIO%ISNOWXY           ,& 
              TVXY                => NoahmpIO%TVXY              ,&
              TGXY                => NoahmpIO%TGXY              ,&
              CANICEXY            => NoahmpIO%CANICEXY          ,&        
              TMN                 => NoahmpIO%TMN               ,&
              XICE                => NoahmpIO%XICE              ,&
              CANLIQXY            => NoahmpIO%CANLIQXY          ,&
              EAHXY               => NoahmpIO%EAHXY             ,&
              TAHXY               => NoahmpIO%TAHXY             ,&
              CMXY                => NoahmpIO%CMXY              ,&
              CHXY                => NoahmpIO%CHXY              ,&
              FWETXY              => NoahmpIO%FWETXY            ,&
              SNEQVOXY            => NoahmpIO%SNEQVOXY          ,&
              ALBOLDXY            => NoahmpIO%ALBOLDXY          ,&
              QSNOWXY             => NoahmpIO%QSNOWXY           ,&
              QRAINXY             => NoahmpIO%QRAINXY           ,&
              WSLAKEXY            => NoahmpIO%WSLAKEXY          ,&
              ZWTXY               => NoahmpIO%ZWTXY             ,&
              WAXY                => NoahmpIO%WAXY              ,&
              WTXY                => NoahmpIO%WTXY              ,&  
              TSNOXY              => NoahmpIO%TSNOXY            ,&
              ZSNSOXY             => NoahmpIO%ZSNSOXY           ,&
              SNICEXY             => NoahmpIO%SNICEXY           ,&
              SNLIQXY             => NoahmpIO%SNLIQXY           ,&
              LFMASSXY            => NoahmpIO%LFMASSXY          ,&
              RTMASSXY            => NoahmpIO%RTMASSXY          ,&
              STMASSXY            => NoahmpIO%STMASSXY          ,&
              WOODXY              => NoahmpIO%WOODXY            ,&
              STBLCPXY            => NoahmpIO%STBLCPXY          ,&
              FASTCPXY            => NoahmpIO%FASTCPXY          ,&
              XSAIXY              => NoahmpIO%XSAIXY            ,&
              LAI                 => NoahmpIO%LAI               ,&
              GRAINXY             => NoahmpIO%GRAINXY           ,&
              GDDXY               => NoahmpIO%GDDXY             ,&
              CROPTYPE            => NoahmpIO%CROPTYPE          ,&
              CROPCAT             => NoahmpIO%CROPCAT           ,&
              IRNUMSI             => NoahmpIO%IRNUMSI           ,&
              IRNUMMI             => NoahmpIO%IRNUMMI           ,&
              IRNUMFI             => NoahmpIO%IRNUMFI           ,&
              IRWATSI             => NoahmpIO%IRWATSI           ,&
              IRWATMI             => NoahmpIO%IRWATMI           ,&
              IRWATFI             => NoahmpIO%IRWATFI           ,&
              IRELOSS             => NoahmpIO%IRELOSS           ,&
              IRSIVOL             => NoahmpIO%IRSIVOL           ,&
              IRMIVOL             => NoahmpIO%IRMIVOL           ,&
              IRFIVOL             => NoahmpIO%IRFIVOL           ,&
              IRRSPLH             => NoahmpIO%IRRSPLH           ,&       
              T2MVXY              => NoahmpIO%T2MVXY            ,&
              T2MBXY              => NoahmpIO%T2MBXY            ,&
              NSOIL               => NoahmpIO%NSOIL             ,&
              IOPT_RUNSRF         => NoahmpIO%IOPT_RUNSRF       ,& 
              IOPT_CROP           => NoahmpIO%IOPT_CROP         ,&
              IOPT_IRR            => NoahmpIO%IOPT_IRR          ,&
              IOPT_IRRM           => NoahmpIO%IOPT_IRRM         ,&
              SF_URBAN_PHYSICS    => NoahmpIO%SF_URBAN_PHYSICS  ,&
              SMOISEQ             => NoahmpIO%SMOISEQ           ,&
              SMCWTDXY            => NoahmpIO%SMCWTDXY          ,&
              RECHXY              => NoahmpIO%RECHXY            ,&
              DEEPRECHXY          => NoahmpIO%DEEPRECHXY        ,& 
              QTDRAIN             => NoahmpIO%QTDRAIN           ,&
              AREAXY              => NoahmpIO%AREAXY            ,&
              DX                  => NoahmpIO%DX                ,&
              DY                  => NoahmpIO%DY                ,&
              MSFTX               => NoahmpIO%MSFTX             ,& 
              MSFTY               => NoahmpIO%MSFTY             ,&
              WTDDT               => NoahmpIO%WTDDT             ,&
              STEPWTD             => NoahmpIO%STEPWTD           ,&
              DT                  => NoahmpIO%DTBL              ,&
              QRFSXY              => NoahmpIO%QRFSXY            ,&
              QSPRINGSXY          => NoahmpIO%QSPRINGSXY        ,&
              QSLATXY             => NoahmpIO%QSLATXY           ,&
              FDEPTHXY            => NoahmpIO%FDEPTHXY          ,&
              HT                  => NoahmpIO%TERRAIN           ,&
              RIVERBEDXY          => NoahmpIO%RIVERBEDXY        ,&
              EQZWT               => NoahmpIO%EQZWT             ,&
              RIVERCONDXY         => NoahmpIO%RIVERCONDXY       ,&
              PEXPXY              => NoahmpIO%PEXPXY            ,&
              RECHCLIM            => NoahmpIO%RECHCLIM           &
             ) 
!--------------------------------------------------------------------------------------- 

    ! initialize
    ids = NoahmpIO%ids
    ide = NoahmpIO%ide+1 
    jds = NoahmpIO%jds
    jde = NoahmpIO%jde+1 
    kds = NoahmpIO%kds
    kde = NoahmpIO%kde 
    ims = NoahmpIO%ims
    ime = NoahmpIO%ime 
    jms = NoahmpIO%jms
    jme = NoahmpIO%jme 
    kms = NoahmpIO%kms
    kme = NoahmpIO%kme 
    its = NoahmpIO%its
    ite = NoahmpIO%ite 
    jts = NoahmpIO%jts
    jte = NoahmpIO%jte 
    kts = NoahmpIO%kts
    kte = NoahmpIO%kte

    ! only initialize for non-restart case
    if ( .not. NoahmpIO%restart_flag ) then

       itf = min0(ite, ide-1)
       jtf = min0(jte, jde-1)

       ! initialize physical snow height SNOWH
       if ( .not. NoahmpIO%FNDSNOWH ) then
          ! If no SNOWH do the following
          print*, 'SNOW HEIGHT NOT FOUND - VALUE DEFINED IN LSMINIT'
          do J = jts, jtf
             do I = its, itf
                NoahmpIO%SNOWH(I,J) = NoahmpIO%SNOW(I,J) * 0.005  ! SNOW in mm and SNOWH in m
             enddo
          enddo
       endif
   
       ! Check if snow/snowh are consistent and cap SWE at 2000mm
       ! the Noah-MP code does it internally but if we don't do it here, problems ensue
       do J = jts, jtf
          do I = its, itf
             if ( NoahmpIO%SNOW(I,J)  < 0.0 ) NoahmpIO%SNOW(I,J)  = 0.0 
             if ( NoahmpIO%SNOWH(I,J) < 0.0 ) NoahmpIO%SNOWH(I,J) = 0.0
             if ( (NoahmpIO%SNOW(I,J) > 0.0) .and. (NoahmpIO%SNOWH(I,J) == 0.0) ) &
                NoahmpIO%SNOWH(I,J) = NoahmpIO%SNOW(I,J) * 0.005
             if ( (NoahmpIO%SNOWH(I,J) > 0.0) .and. (NoahmpIO%SNOW(I,J) == 0.0) ) &
                NoahmpIO%SNOW(I,J)  = NoahmpIO%SNOWH(I,J) / 0.005
             if ( NoahmpIO%SNOW(I,J) > 2000.0 ) then
                NoahmpIO%SNOWH(I,J) = NoahmpIO%SNOWH(I,J) * 2000.0 / NoahmpIO%SNOW(I,J)      ! SNOW in mm and SNOWH in m
                NoahmpIO%SNOW (I,J) = 2000.0                                                 ! cap SNOW at 2000, maintain density
             endif
          enddo
       enddo

       ! check soil type
       errflag = 0
       do J = jts, jtf
          do I = its, itf
             if ( NoahmpIO%ISLTYP(I,J) < 1 ) then
                errflag = 1
                write(*,*) "lsminit: out of range ISLTYP ",I,J,NoahmpIO%ISLTYP(I,J)
                stop
             endif
          enddo
       enddo

       ! initialize soil liquid water content SH2O
       do J = jts , jtf
          do I = its , itf
          if(IVGTYP(I,J)==NoahmpIO%ISICE_TABLE .AND. XICE(I,J) <= 0.0) then
             do NS=1, NSOIL
                SMOIS(I,NS,J) = 1.0                     ! glacier starts all frozen
                SH2O(I,NS,J) = 0.0
                TSLB(I,NS,J) = MIN(TSLB(I,NS,J),263.15) ! set glacier temp to at most -10C
             enddo
            !TMN(I,J) = MIN(TMN(I,J),263.15)           ! set deep temp to at most -10C
             SNOW(I,J) = MAX(SNOW(I,J), 10.0)           ! set SWE to at least 10mm
             SNOWH(I,J)=SNOW(I,J)*0.01                  ! SNOW in mm and SNOWH in m

          else

             BEXP   = NoahmpIO%BEXP_TABLE  (ISLTYP(I,J))
             SMCMAX = NoahmpIO%SMCMAX_TABLE(ISLTYP(I,J))
             PSISAT = NoahmpIO%PSISAT_TABLE(ISLTYP(I,J))

                do NS=1, NSOIL
                  if ( SMOIS(I,NS,J) > SMCMAX )  SMOIS(I,NS,J) = SMCMAX
                enddo

                if ( ( BEXP > 0.0 ) .AND. ( SMCMAX > 0.0 ) .AND. ( PSISAT > 0.0 ) ) then
                  do NS=1, NSOIL

                     if ( TSLB(I,NS,J) < 273.149 ) then    ! Use explicit as initial soil ice
                        FK = (( (HLICE/(GRAV0*(-PSISAT))) *                              &
                             ((TSLB(I,NS,J)-T0)/TSLB(I,NS,J)) )**(-1/BEXP) )*SMCMAX
                        FK = MAX(FK, 0.02)
                        SH2O(I,NS,J) = MIN( FK, SMOIS(I,NS,J) )
                     else
                        SH2O(I,NS,J)=SMOIS(I,NS,J)
                     endif

                  enddo

                else

                  do NS=1, NSOIL
                     SH2O(I,NS,J)=SMOIS(I,NS,J)
                  enddo

                endif
             endif
          enddo
       enddo

       do J = jts,jtf
          do I = its,itf
             QTDRAIN    (I,J) = 0.
             TVXY       (I,J) = TSK(I,J)
             if(snow(i,j) > 0.0 .and. tsk(i,j) > 273.15) TVXY(I,J) = 273.15
             TGXY       (I,J) = TSK(I,J)
             if(snow(i,j) > 0.0 .and. tsk(i,j) > 273.15) TGXY(I,J) = 273.15
             CANWAT     (I,J) = 0.0
             CANLIQXY   (I,J) = CANWAT(I,J)
             CANICEXY   (I,J) = 0.
             EAHXY      (I,J) = 2000. 
             TAHXY      (I,J) = TSK(I,J)
             if(snow(i,j) > 0.0 .and. tsk(i,j) > 273.15) TAHXY(I,J) = 273.15
!             TAHXY      (I,J) = 287.

             T2MVXY     (I,J) = TSK(I,J)
             if(snow(i,j) > 0.0 .and. tsk(i,j) > 273.15) T2MVXY(I,J) = 273.15
             T2MBXY     (I,J) = TSK(I,J)
             if(snow(i,j) > 0.0 .and. tsk(i,j) > 273.15) T2MBXY(I,J) = 273.15

           CMXY       (I,J) = 0.0
           CHXY       (I,J) = 0.0
           FWETXY     (I,J) = 0.0
           SNEQVOXY   (I,J) = 0.0
           ALBOLDXY   (I,J) = 0.65
           QSNOWXY    (I,J) = 0.0
           QRAINXY    (I,J) = 0.0
           WSLAKEXY   (I,J) = 0.0

           if(IOPT_RUNSRF.ne.5) then 
              WAXY       (I,J) = 4900.                                       !???
              WTXY       (I,J) = WAXY(i,j)                                   !???
              ZWTXY      (I,J) = (25. + 2.0) - WAXY(i,j)/1000/0.2            !???
           else
              WAXY       (I,J) = 0.
              WTXY       (I,J) = 0.
              AREAXY     (I,J) = (DX * DY) / ( MSFTX(I,J) * MSFTY(I,J) )
           endif

           ! add urban flag
           urbanpt_flag = .false.
           if ( IVGTYP(I,J) == NoahmpIO%ISURBAN_TABLE .or. IVGTYP(I,J) > NoahmpIO%URBTYPE_beg ) THEN
               urbanpt_flag = .true.
           endif

           if(IVGTYP(I,J) == NoahmpIO%ISBARREN_TABLE .OR. IVGTYP(I,J) == NoahmpIO%ISICE_TABLE .OR. &
             (NoahmpIO%SF_URBAN_PHYSICS == 0 .AND. urbanpt_flag) .OR. IVGTYP(I,J) == NoahmpIO%ISWATER_TABLE ) then
             LAI        (I,J) = 0.0
             XSAIXY     (I,J) = 0.0
             LFMASSXY   (I,J) = 0.0
             STMASSXY   (I,J) = 0.0
             RTMASSXY   (I,J) = 0.0
             WOODXY     (I,J) = 0.0
             STBLCPXY   (I,J) = 0.0
             FASTCPXY   (I,J) = 0.0
             GRAINXY    (I,J) = 1E-10
             GDDXY      (I,J) = 0
             CROPCAT    (I,J) = 0

           else
     
             LAI        (I,J) = max(LAI(i,j),0.05)                      ! at least start with 0.05 for arbitrary initialization (v3.7)
             XSAIXY     (I,J) = max(0.1*LAI(I,J),0.05)                  ! MB: arbitrarily initialize SAI using input LAI (v3.7)
             massLAI = 1000. / max(NoahmpIO%SLA_TABLE(IVGTYP(I,J)),1.0) ! conversion from LAI to mass  (v3.7)
             LFMASSXY   (I,J) = LAI(i,j)*massLAI                        ! use LAI to initialize (v3.7)
             masssai = 1000. / 3.0                                      ! conversion from LAI to mass (v3.7)
             STMASSXY   (I,J) = XSAIXY(i,j)*masssai                     ! use SAI to initialize (v3.7)
             RTMASSXY   (I,J) = 500.0                                   ! these are all arbitrary and probably should be
             WOODXY     (I,J) = 500.0                                   ! in the table or read from initialization
             STBLCPXY   (I,J) = 1000.0                                  !
             FASTCPXY   (I,J) = 1000.0                                  !
             GRAINXY    (I,J) = 1E-10
             GDDXY      (I,J) = 0    

! Initialize crop for Liu crop model

                if(IOPT_CROP == 1 ) then
                   CROPCAT    (i,j) = NoahmpIO%default_crop_table
                if(CROPTYPE(i,5,j) >= 0.5) then
                   RTMASSXY(i,j) = 0.0
                   WOODXY  (i,j) = 0.0                    

                if(CROPTYPE(i,1,j) > CROPTYPE(i,2,j) .and. &
                   CROPTYPE(i,1,j) > CROPTYPE(i,3,j) .and. &
                   CROPTYPE(i,1,j) > CROPTYPE(i,4,j) ) then        ! choose corn

                      CROPCAT (i,j) = 1
                      LFMASSXY(i,j) =    LAI(i,j)/0.015               ! Initialize lfmass Zhe Zhang 2020-07-13
                      STMASSXY(i,j) = XSAIXY(i,j)/0.003

                elseif(CROPTYPE(i,2,j) > CROPTYPE(i,1,j) .and. &
                       CROPTYPE(i,2,j) > CROPTYPE(i,3,j) .and. &
                       CROPTYPE(i,2,j) > CROPTYPE(i,4,j) ) then        ! choose soybean
                       CROPCAT (i,j) = 2
                       LFMASSXY(i,j) =    LAI(i,j)/0.030               ! Initialize lfmass Zhe Zhang 2020-07-13
                       STMASSXY(i,j) = XSAIXY(i,j)/0.003

               else
                      CROPCAT (i,j) = NoahmpIO%default_crop_table
                      LFMASSXY(i,j) =    LAI(i,j)/0.035
                      STMASSXY(i,j) = XSAIXY(i,j)/0.003
               end if
            end if
         end if

! Noah-MP irrigation scheme !pvk
             if ( (IOPT_IRR >= 1) .and. (IOPT_IRR <= 3) ) then
                if ( (IOPT_IRRM == 0) .or. (IOPT_IRRM ==1) ) then       ! sprinkler
                   IRNUMSI(i,j) = 0
                   IRWATSI(i,j) = 0.
                   IRELOSS(i,j) = 0.
                   IRRSPLH(i,j) = 0.     
                elseif ( (IOPT_IRRM == 0) .or. (IOPT_IRRM == 2) ) then ! micro or drip
                   IRNUMMI(i,j) = 0
                   IRWATMI(i,j) = 0.
                   IRMIVOL(i,j) = 0.
                elseif ( (IOPT_IRRM == 0) .or. (IOPT_IRRM == 3) ) then ! flood 
                   IRNUMFI(i,j) = 0
                   IRWATFI(i,j) = 0.
                   IRFIVOL(i,j) = 0.
                endif
             end if
            endif
          enddo
       enddo
       
       ! Given the soil layer thicknesses (in DZS), initialize the soil layer
       ! depths from the surface.
       NoahmpIO%ZSOIL(1)       = -DZS(1)          ! negative
       do NS=2, NSOIL
          NoahmpIO%ZSOIL(NS)   = NoahmpIO%ZSOIL(NS-1) - DZS(NS)
       enddo
       
       ! Initialize snow/soil layer arrays ZSNSOXY, TSNOXY, SNICEXY, SNLIQXY, 
       ! and ISNOWXY
       
       !---------------------------------------------------------------------
       ! Initialize Noah-MP Snow
       !--------------------------------------------------------------------- 
 
       call NoahmpSnowinitMain (NoahmpIO)
 
       !initialize arrays for groundwater dynamics iopt_run=5
 
       if(IOPT_RUNSRF.eq.5) then

             STEPWTD = nint(WTDDT*60./DT)
             STEPWTD = max(STEPWTD,1)

       endif
    endif
 
    end associate
    
  end subroutine NoahmpInitMain    

end module NoahmpInitMainMod
