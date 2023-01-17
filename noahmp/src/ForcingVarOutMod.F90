module ForcingVarOutMod

!!! Transfer column (1-D) Noah-MP forcing variables to 2D NoahmpIO for output
!!! Forcing variables should be first defined in ForcingVarType.F90

! ------------------------ Code history -----------------------------------
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: P. Valayamkunnath, C. He, & refactor team (Jan 2023)
! -------------------------------------------------------------------------

  use Machine
  use NoahmpIOVarType
  use NoahmpVarType

  implicit none

contains

!=== Transfer model states to output=====

  subroutine ForcingVarOutTransfer(noahmp, NoahmpIO)

    implicit none

    type(noahmp_type),   intent(inout) :: noahmp
    type(NoahmpIO_type), intent(inout) :: NoahmpIO

    associate(                                                         &
              I               => noahmp%config%domain%GridIndexI      ,&
              J               => noahmp%config%domain%GridIndexJ       &
             )

    NoahmpIO%FORCTLSM  (I,J) = noahmp%forcing%TemperatureAirRefHeight
    NoahmpIO%FORCQLSM  (I,J) = noahmp%forcing%SpecHumidityRefHeight
    NoahmpIO%FORCPLSM  (I,J) = noahmp%forcing%PressureAirRefHeight
    NoahmpIO%FORCWLSM  (I,J) = sqrt(noahmp%forcing%WindEastwardRefHeight**2 + noahmp%forcing%WindNorthwardRefHeight**2)

    end associate

  end subroutine ForcingVarOutTransfer

end module ForcingVarOutMod
