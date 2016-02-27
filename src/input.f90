Module input
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
  Use strings        ! from libBP
  Use general        ! from libBP
  Use units          ! from libBP
  Use globals
  Use output
! Force declaration of all variables
  Implicit None
! Privacy of variables/functions/subroutines
  Private
! Public Subroutines
  Public :: readInput

  Contains

! ---------------------------------------------------------------------------------------------------
! Save to specific file
! ---------------------------------------------------------------------------------------------------

  Subroutine readInput()
! Saves the eam file to the output directory
    Implicit None   ! Force declaration of all variables
! Private variables
    Character(len=255) :: fileRow, fileRowU
    Integer(kind=StandardInteger) :: i, j, n
    Character(len=32) :: bufferA,bufferB,bufferC,bufferD
    Real(kind=DoubleReal) :: timeStartRI
    Real(kind=DoubleReal) :: tempDouble
    Integer(kind=StandardInteger) :: readingData, coordCounter
! Read through input file
    readingData = 0
    coordCounter = 0
    n = 0
    Do i=1,inputFileRows
      n = n + 1
      fileRow = inputFileData(n)
      fileRowU = StrToUpper(fileRow)
      If(fileRow(1:1).ne."!")Then
        If(fileRowU(1:4).eq."#NEW")Then
          readingData = 1
        End If
        If(fileRowU(1:4).eq."#END")Then
          readingData = 0
        End If
        If(readingData.eq.1)Then
          If(fileRow(1:1).eq."#")Then
            If(fileRowU(1:3).eq."#LP")Then
              Read(fileRow,*) bufferA, bufferB, bufferC
              Read(bufferB,*) aLat
              aLat = UnitConvert(aLat,bufferC,"A")
            End If
            If(fileRowU(1:2).eq."#X")Then
              Read(fileRow,*) bufferA, bufferB, bufferC, bufferD
              Read(bufferB,*) crystalVectors(1,1)
              Read(bufferC,*) crystalVectors(1,2)
              Read(bufferD,*) crystalVectors(1,3)
            End If
            If(fileRowU(1:2).eq."#Y")Then
              Read(fileRow,*) bufferA, bufferB, bufferC, bufferD
              Read(bufferB,*) crystalVectors(2,1)
              Read(bufferC,*) crystalVectors(2,2)
              Read(bufferD,*) crystalVectors(2,3)
            End If
            If(fileRowU(1:2).eq."#Z")Then
              Read(fileRow,*) bufferA, bufferB, bufferC, bufferD
              Read(bufferB,*) crystalVectors(3,1)
              Read(bufferC,*) crystalVectors(3,2)
              Read(bufferD,*) crystalVectors(3,3)
            End If
            If(fileRowU(1:3).eq."#CC")Then
              Read(fileRow,*) bufferA, bufferB, bufferC, bufferD
              Read(bufferB,"(I8)") copyX
              Read(bufferC,"(I8)") copyY
              Read(bufferD,"(I8)") copyZ
            End If
            If(fileRowU(1:2).eq."#D")Then
              Read(fileRow,*) bufferA, bufferB, bufferC
              Read(bufferB,*) displacement
              displacement = UnitConvert(displacement,bufferC,"A")
            End If
            If(fileRowU(1:2).eq."#S")Then    ! Seed
              Read(fileRow,*) bufferA, bufferB
              Read(bufferB,*) randomSeed
            End If
            If(fileRowU(1:3).eq."#AS")Then   ! Atom species
              Read(fileRow,*) bufferA, bufferB
              Read(bufferB,*) speciesCount
              Do j=1,speciesCount
                n = n + 1
                fileRow = inputFileData(n)
                fileRowU = StrToUpper(fileRow)
                Read(fileRow,*) bufferA, bufferB
                Read(bufferA,*) speciesList(j)
                Read(bufferB,*) speciesRatio(j)
              End Do
            End If
            If(fileRowU(1:7).eq."#OUTDIR")Then   ! Output
              Read(fileRow,*) bufferA, bufferB
              subDir = adjustl(trim(bufferB))
            End If
          Else
            coordCounter = coordCounter + 1
            Read(fileRow,*) bufferA, bufferB, bufferC, bufferD
            bufferA = adjustl(trim(bufferA))
            inputLabels(coordCounter) = bufferA(1:16)
            Read(bufferB,"(F16.8)") tempDouble
            inputCoords(coordCounter,1) = tempDouble
            Read(bufferC,"(F16.8)") tempDouble
            inputCoords(coordCounter,2) = tempDouble
            Read(bufferD,"(F16.8)") tempDouble
            inputCoords(coordCounter,3) = tempDouble
          End If
        End If
      End If
    End Do
! total coords input
    inputCoordsCount = coordCounter
! output summary
    Call printInput()
!
  End Subroutine readInput

End Module input
