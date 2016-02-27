Module output
! --------------------------------------------------------------!
! Ben Palmer, University of Birmingham
! Module: loadData
! Updated: 18th February 2016
! --------------------------------------------------------------!
! Description:
! Output data
! --------------------------------------------------------------!

! Setup Modules
  Use kinds          ! from libBP
  Use printMod       ! from libBP
  Use general       ! from libBP
  Use globals
! Force declaration of all variables
  Implicit None
! Privacy of variables/functions/subroutines
  Private
! Public Subroutines
  Public :: printInput
  Public :: printOutput
  Public :: saveOutput
  Contains
! ---------------------------------------------------------------------------------------------------
  Subroutine printInput()
  ! force declaration of all variables
    Implicit None
  ! Private variables
    Integer(kind=StandardInteger) :: i, inputCoord
    If(mpiProcessID.eq.0)Then
      print *,""
      print *,"Input:"
      print *,"----------------------------------------------------------------------"
      print *,""
      print *,"X copies: ",copyX
      print *,"Y copies: ",copyY
      print *,"Z copies: ",copyZ
      print *,""
      print *,"Alat: ",aLat
      print *,"Disp: ",displacement
      print *,"Seed: ",randomSeed
      print *,"Crystal Vectors:"
      Do i=1,3
        print "(F8.3,F8.3,F8.3)",crystalVectors(i,1),crystalVectors(i,2),crystalVectors(i,3)
      End Do
      Do inputCoord=1,inputCoordsCount
        print "(I8,A2,A16,F8.3,F8.3F8.3)",inputCoord,"  ",&
        inputLabels(inputCoord),inputCoords(inputCoord,1),&
        inputCoords(inputCoord,2),inputCoords(inputCoord,3)
      End Do
      print *,""
    End If
  End Subroutine printInput
  Subroutine printOutput()
  ! force declaration of all variables
    Implicit None
  ! Private variables
    Integer(kind=StandardInteger) :: i,outputCoord
  ! Loop through output coords
    print *,""
    print *,"Output:"
    print *,"----------------------------------------------------------------------"
    print *,""
    print *,"Alat: ",aLatOut
    print *,"Crystal Vectors:"
    Do i=1,3
      print "(F10.6,F10.6,F10.6)",crystalVectorsOut(i,1),crystalVectorsOut(i,2),crystalVectorsOut(i,3)
    End Do
    print *,""
    print *,"Numbered Coords:"
    Do outputCoord=1,outputCoordsCount
      print "(I8,A2,A16,F10.6,F10.6,F10.6)",outputCoord,"  ",outputLabels(outputCoord),&
      outputCoords(outputCoord,1),&
      outputCoords(outputCoord,2),outputCoords(outputCoord,3)
    End Do
    print *,""
    print *,"Unumbered Coords:"
    Do outputCoord=1,outputCoordsCount
      print "(A16,F10.6,F10.6,F10.6)",outputLabels(outputCoord),&
      outputCoords(outputCoord,1),&
      outputCoords(outputCoord,2),outputCoords(outputCoord,3)
    End Do
    print *,""
  End Subroutine printOutput
  Subroutine saveOutput()
  ! force declaration of all variables
    Implicit None
  ! Private variables
    Integer(kind=StandardInteger) :: i,outputCoord
    Character(len=255) :: fileRow, outDirPath, inFileCopy
    If(mpiProcessID.eq.0)Then
      outDirPath = trim(outputDirectory)//"/"//trim(subDir)
      Call makeDir(outDirPath)
  ! Save input file
      inFileCopy = trim(outDirPath)//"/"//trim(subDir)//".in"
      open(unit=999,file=trim(inFileCopy))
      i = 0
      Do While(i.le.inputFileRows)
        i = i + 1
        fileRow = inputFileData(i)
        write(999,"(A)") trim(fileRow)
      End Do
      close(999)
  ! Create fractional coords output file
      open(unit=999,file=trim(trim(outDirPath)//"/"//"fractionalCoords.dat"))
      write(999,"(A5,F10.5)") "Alat: ",aLatOut
      write(999,"(A16)") "Crystal Vectors:"
      Do i=1,3
        write(999,"(F10.6,F10.6,F10.6)") crystalVectorsOut(i,1),crystalVectorsOut(i,2),crystalVectorsOut(i,3)
      End Do
      Do outputCoord=1,outputCoordsCount
        write(999,"(A16,F10.6,F10.6,F10.6)") outputLabels(outputCoord),&
        outputCoords(outputCoord,1),&
        outputCoords(outputCoord,2),outputCoords(outputCoord,3)
      End Do
      Close(999)
      open(unit=999,file=trim(trim(outDirPath)//"/"//"transformedCoords.dat"))
      Do outputCoord=1,outputCoordsCount
        write(999,"(A16,F10.6,F10.6,F10.6)") outputLabels(outputCoord),&
        outputCoordsT(outputCoord,1),&
        outputCoordsT(outputCoord,2),outputCoordsT(outputCoord,3)
      End Do
      Close(999)
    End If
  End Subroutine saveOutput
End Module output
