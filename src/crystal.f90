PROGRAM crystal
! University of Birmingham
! Ben Palmer
!
! This package has been designed to:
!   Fit functions to input data
!   Output
!
! Setup Modules
  Use kinds          ! from libBP
  Use env            ! from libBP
  Use msubs          ! from libBP
  Use loadData       ! load important data
  Use globals        ! declare all globals
  Use initialise     ! initialise program
  Use input          ! input data
  Use output         ! output data
  Use makeCrystal    ! output data
  Use finalise       ! finalise program
! Force declaration of all variables
  Implicit None
! Include MPI header
  Include 'mpif.h'
! Variables
  Integer(kind=StandardInteger) :: error
!======================================
! Init MPI
  Call MPI_Init(error)
!======================================
  Call initGlobals()
  Call runInitialise()
!--------------------------------------------------------------------------------------------------------
! Program Starts !
!----------------!
  Call readInput()
  Call runMakeCrystal()


!--------------------------------------------------------------------------------------------------------
! Finalise program
  Call runFinalise()
!======================================
! Finalise MPI
  Call MPI_Finalize(error)
!======================================
End Program crystal
