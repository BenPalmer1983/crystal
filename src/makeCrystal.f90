Module makeCrystal
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
  Use rng            ! from libBP
  Use rngDist        ! from libBP
  Use coordFunctions ! from libBP
  Use globals
  Use output
! Force declaration of all variables
  Implicit None
! Privacy of variables/functions/subroutines
  Private
! Public Subroutines
  Public :: runMakeCrystal

  Contains

! ---------------------------------------------------------------------------------------------------
! Save to specific file
! ---------------------------------------------------------------------------------------------------

Subroutine runMakeCrystal()
! force declaration of all variables
  Implicit None
! Private variables
  Integer(kind=StandardInteger) :: i, j, k, inputCoord, outputCoord
  Real(kind=DoubleReal) :: maximumVal, resultVal
  Real(kind=DoubleReal), Dimension(1:3) :: xVect
! Set random seed
  resultVal = RandomLCG(randomSeed)
! Fractional coords
  outputCoord = 0
  Do i=1,copyX
    Do j=1,copyY
      Do k=1,copyZ
        Do inputCoord=1,inputCoordsCount
          outputCoord = outputCoord + 1
          outputLabels(outputCoord) = inputLabels(inputCoord)
          outputCoords(outputCoord,1) = (i-1+inputCoords(inputCoord,1))/copyX
          outputCoords(outputCoord,2) = (j-1+inputCoords(inputCoord,2))/copyY
          outputCoords(outputCoord,3) = (k-1+inputCoords(inputCoord,3))/copyZ
        End Do
      End Do
    End Do
  End Do
! If randomly make labels
  Call makeLabels()
! Apply random displacements
  outputCoordsCount = outputCoord
! Apply any random displacement
  If(displacement.gt.0.0D0)Then
    Do outputCoord=1,outputCoordsCount
      outputCoords(outputCoord,1) = outputCoords(outputCoord,1)+&
      2.0D0*(RandomDist("G")-0.5D0)*(displacement/(copyX*aLat))
      outputCoords(outputCoord,2) = outputCoords(outputCoord,2)+&
      2.0D0*(RandomDist("G")-0.5D0)*(displacement/(copyX*aLat))
      outputCoords(outputCoord,3) = outputCoords(outputCoord,3)+&
      2.0D0*(RandomDist("G")-0.5D0)*(displacement/(copyX*aLat))
    End Do
  End If
! crystal matrix and lattice parameter
  Do i=1,3
    crystalVectorsOut(1,i) = copyX*aLat*crystalVectors(1,i)
  End Do
  Do i=1,3
    crystalVectorsOut(2,i) = copyY*aLat*crystalVectors(2,i)
  End Do
  Do i=1,3
    crystalVectorsOut(3,i) = copyZ*aLat*crystalVectors(3,i)
  End Do
! max
  maximumVal = 0.0D0
  Do i=1,3
    Do j=1,3
      If(crystalVectorsOut(i,j).gt.maximumVal)Then
        maximumVal = crystalVectorsOut(i,j)
      End If
    End Do
  End Do
! normalise
  aLatOut = maximumVal
  Do i=1,3
    Do j=1,3
      crystalVectorsOut(i,j) = crystalVectorsOut(i,j) / maximumVal
    End Do
  End Do
! Make transformed coords
  Do outputCoord=1,outputCoordsCount
    Do i=1,3
      xVect(i) = aLatOut * outputCoords(outputCoord,i)
    End Do
    xVect = TransformCoords (xVect, crystalVectorsOut)
    Do i=1,3
      outputCoordsT(outputCoord,i) = xVect(i)
    End Do
  End Do
! Output to terminal and file
  Call printOutput()
  Call saveOutput()
End Subroutine runMakeCrystal
! ---------------------------------------------------------------------------------------------------
Subroutine makeLabels()
! force declaration of all variables
  Implicit None
! Private variables
  Integer(kind=StandardInteger) :: i, j, n, oCount, labelCount, atomLabels
  Real(kind=DoubleReal) ::  ratioTotal
  Character(len=16) :: tempLabel
! If set in input file (i.e. is #AS is set)
  If(speciesCount.gt.0)Then
! Ratio total
    ratioTotal = 0.0D0
    Do i=1,speciesCount
      ratioTotal = ratioTotal + speciesRatio(i)
    End Do
! total count
    oCount = copyX*copyY*copyZ*inputCoordsCount
! atom label count
    labelCount = 0
    Do i=1,speciesCount
      atomLabels = ceiling(oCount*speciesRatio(i)/ratioTotal)
      Do j=(labelCount+1),(labelCount+atomLabels)
        outputLabelsTemp(j) = speciesList(i)
      End Do
      labelCount = labelCount + atomLabels
    End Do
! Shuffle
    Do n=1,2000
      i = RandomInteger(1,labelCount)
      j = RandomInteger(1,labelCount)
      If(i.ne.j)Then
        tempLabel = BlankString(tempLabel)
        tempLabel = outputLabelsTemp(i)
        outputLabelsTemp(i) = outputLabelsTemp(j)
        outputLabelsTemp(j) = tempLabel
      End If
    End Do
! Store labels
    Do i=1,oCount
      outputLabels(i) = outputLabelsTemp(i)
    End Do
! Count label types
    Do i=1,speciesCount
      n = 0
      Do j=1,oCount
        If(trim(strtoupper(speciesList(i))).eq.&
        trim(strtoupper(outputLabels(j))))Then
          n = n + 1
        End If
      End Do
      print *,speciesList(i),n
    End Do
  End If
End Subroutine makeLabels

End Module makeCrystal
