module GlobalDataModule
    implicit none
    integer, save :: narealsp, issflg, nper
    integer, save :: mpbasUnit, disUnit, tdisUnit, gridMetaUnit, headUnit,       &
      headuUnit, budgetUnit, traceModeUnit, binPathlineUnit
    integer, save :: inUnit, pathlineUnit, endpointUnit, timeseriesUnit,        &
      mplistUnit, mpsimUnit, traceUnit, budchkUnit, aobsUnit, logUnit
    integer, save :: particleGroupCount
    integer, save :: gridFileType
    integer, parameter :: niunit = 100
    character*200 :: mpnamFile, mpsimFile, mplistFile, mpbasFile, disFile,      &
      tdisFile, gridFile, headFile, budgetFile, traceFile, gridMetaFile
end module