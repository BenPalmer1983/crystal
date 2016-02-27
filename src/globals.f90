Module globals

! --------------------------------------------------------------!
! Global Variables
! Ben Palmer, University of Birmingham
! --------------------------------------------------------------!

! Declare all global variables

! ----------------------------------------
! Updated: 21st February 2016
! ----------------------------------------

! Setup Modules
  Use kinds
  Use strings

! Force declaration of all variables
  Implicit None

!=========================================================================================
! Declare Variables
!=========================================================================================
! Standard Variables
!--------------------------------------------------------------
  Integer(kind=StandardInteger) :: mpiProcessCount, mpiProcessID
  Character(len=64) :: compileLine, inputFileName
  Character(len=255) :: currentWorkingDirectory, outputDirectory, tempDirectory
  Integer(kind=StandardInteger) :: inputFileRows
  Character(len=255), Dimension(1:4096) :: inputFileData
  Real(kind=DoubleReal) :: programStartTime, programEndTime, programRunTime
! Standard Parameters
!--------------------------------------------------------------
! Input Variables
!--------------------------------------------------------------
  Real(kind=DoubleReal), Dimension(1:3,1:3) :: crystalVectors, crystalVectorsOut
  Real(kind=DoubleReal) :: aLat, aLatOut
  Character(len=16), Dimension(1:100000) :: inputLabels
  Real(kind=DoubleReal), Dimension(1:100000,1:3) :: inputCoords
  Character(len=16), Dimension(1:100000) :: outputLabels, outputLabelsTemp
  Real(kind=DoubleReal), Dimension(1:100000,1:3) :: outputCoords, outputCoordsT
  Integer(kind=StandardInteger) :: inputCoordsCount, outputCoordsCount
  Integer(kind=StandardInteger) :: copyX, copyY, copyZ
  Real(kind=DoubleReal) :: displacement
  Integer(kind=StandardInteger) :: randomSeed
  Integer(kind=StandardInteger) :: speciesCount
  Character(len=255) :: subDir
  Character(len=16), Dimension(1:100) :: speciesList
  Real(kind=DoubleReal), Dimension(1:100) :: speciesRatio




!=========================================================================================
! Set scope: Public
!=========================================================================================
! Standard Variables
!--------------------------------------------------------------
  Public :: mpiProcessCount, mpiProcessID
  Public :: compileLine, inputFileName
  Public :: currentWorkingDirectory, outputDirectory, tempDirectory
  Public :: inputFileRows, inputFileData
  Public :: programStartTime, programEndTime, programRunTime
! Standard Parameters
!--------------------------------------------------------------
! Input Variables
!--------------------------------------------------------------
  Public :: crystalVectors, aLat
  Public :: inputLabels, inputCoords, inputCoordsCount
  Public :: outputLabels, outputCoords, outputCoordsCount, outputCoordsT, outputLabelsTemp
  Public :: copyX, copyY, copyZ
  Public :: crystalVectorsOut, aLatOut
  Public :: displacement
  Public :: randomSeed
  Public :: speciesCount
  Public :: subDir
  Public :: speciesList
  Public :: speciesRatio

!================================================================
! Subroutines
!================================================================

  Contains

! Init global variables
  Subroutine initGlobals()
    Implicit None
! Initialise Subroutine Variable
    Call cpu_time(programStartTime)
    compileLine = "01:00:08  22/12/2015"  !--compile-line-replace
    inputFileData = BlankStringArray(inputFileData)
  End Subroutine initGlobals

End Module globals
