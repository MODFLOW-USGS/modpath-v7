module ParticleTrackingEngineModule
  use TrackPathResultModule,only : TrackPathResultType
  use ParticleLocationModule,only : ParticleLocationType
  use ParticleLocationListModule,only : ParticleLocationListType
  use ParticleCoordinateModule,only : ParticleCoordinateType
  use TrackCellModule,only : TrackCellType
  use TrackCellResultModule,only : TrackCellResultType
  use BudgetReaderModule,only : BudgetReaderType
  use HeadReaderModule,only : HeadReaderType
  use BudgetListItemModule,only : BudgetListItemType
  use ModflowRectangularGridModule,only : ModflowRectangularGridType
  use ModpathCellDataModule,only : ModpathCellDataType
  use ModpathSubCellDataModule,only : ModpathSubCellDataType
  use ParticleTrackingOptionsModule,only : ParticleTrackingOptionsType
  use BudgetRecordHeaderModule,only : BudgetRecordHeaderType
  use UtilMiscModule,only : TrimAll
!  use ModpathUnstructuredBasicDataModule,only : ModpathUnstructuredBasicDataType
  implicit none
  
! Set default access status to private
  private

  type,public :: ParticleTrackingEngineType
!    doubleprecision :: ReferenceTime = 0d0
!    doubleprecision :: StoppingTime = 0d0
    doubleprecision :: HDry = 0d0
    doubleprecision :: HNoFlow = 0d0
    type(ParticleTrackingOptionsType) :: TrackingOptions
    type(ModpathCellDataType) :: CellDataBuffer
    logical :: Initialized = .false.
    logical :: SteadyState = .true.
    integer :: DefaultIfaceCount
    character(len=16),dimension(20) :: DefaultIfaceLabels
    integer,dimension(20) :: DefaultIfaceValues
    integer,allocatable,dimension(:) :: IBoundTS
    doubleprecision,allocatable,dimension(:) :: Heads
    doubleprecision,allocatable,dimension(:) :: FlowsJA
    doubleprecision,allocatable,dimension(:) :: FlowsRightFace
    doubleprecision,allocatable,dimension(:) :: FlowsFrontFace
    doubleprecision,allocatable,dimension(:) :: FlowsLowerFace
    doubleprecision,allocatable,dimension(:) :: SourceFlows
    doubleprecision,allocatable,dimension(:) :: SinkFlows
    doubleprecision,allocatable,dimension(:) :: StorageFlows
    doubleprecision,allocatable,dimension(:) :: BoundaryFlows
    doubleprecision,allocatable,dimension(:) :: SubFaceFlows
    doubleprecision,allocatable,dimension(:) :: ArrayBufferDbl
    ! Externally assigned arrays
    !integer,dimension(:),pointer :: LayerTypes
    integer,dimension(:),pointer :: IBound
    integer,dimension(:),pointer :: Zones
    doubleprecision,dimension(:),pointer :: Porosity
    doubleprecision,dimension(:),pointer :: Retardation
    
    
    ! Private variables
    type(HeadReadertype),pointer :: HeadReader => null()
    type(BudgetReaderType),pointer :: BudgetReader => null()
    class(ModflowRectangularGridType),pointer :: Grid => null()
    type(TrackCellType),private :: TrackCell
    type(TrackCellResultType),private :: TrackCellResult
    integer,private :: CurrentStressPeriod = 0
    integer,private :: CurrentTimeStep = 0
    integer,private :: MaxReducedCellConnectionsCount = 17
    doubleprecision,dimension(17),private :: CellFlowsBuffer
    integer,dimension(17),private :: ReducedCellConnectionsBuffer
    type(BudgetListItemType),allocatable,dimension(:) :: ListItemBuffer
    logical,allocatable,dimension(:),private :: SubFaceFlowsComputed
    type(ParticleLocationListType) :: LocBuffP
    type(ParticleLocationListType) :: LocBuffTS
    
  contains
    procedure :: Initialize=>pr_Initialize
    procedure :: Reset=>pr_Reset
    procedure :: ClearTimeStepBudgetData=>pr_ClearTimeStepBudgetData
    procedure :: TrackPath=>pr_TrackPath
    procedure :: FillCellBuffer=>pr_FillCellBuffer
    procedure :: LoadTimeStep=>pr_LoadTimeStep
    procedure :: FillFaceFlowsBuffer=>pr_FillFaceFlowsBuffer
    procedure :: GetCurrentStressPeriod=>pr_GetCurrentStressPeriod
    procedure :: GetCurrentTimeStep=>pr_GetCurrentTimeStep
    procedure :: FillCellFlowsBuffer=>pr_FillCellFlowsBuffer
    procedure :: SetIBound=>pr_SetIBound
    procedure :: SetZones=>pr_SetZones
    procedure :: SetPorosity=>pr_SetPorosity
    procedure :: SetRetardation=>pr_SetRetardation
    !procedure :: SetLayerTypes=>pr_SetLayerTypes
    procedure :: SetDefaultIface=>pr_SetDefaultIface
    procedure :: CheckForDefaultIface=>pr_CheckForDefaultIface
    procedure :: GetVolumetricBalanceSummary=>pr_GetVolumetricBalanceSummary
    procedure :: WriteCellBuffer=>pr_WriteCellBuffer
    procedure :: GetTopMostActiveCell=>pr_GetTopMostActiveCell
  end type
  
contains

!***************************************************************************************************************
! Description goes here
!***************************************************************************************************************
!
! Specifications
!---------------------------------------------------------------------------------------------------------------
!  implicit none
!---------------------------------------------------------------------------------------------------------------
function pr_GetTopMostActiveCell(this, cellNumber) result(n)
!***************************************************************************************************************
! Description goes here
!***************************************************************************************************************
!
! Specifications
!---------------------------------------------------------------------------------------------------------------
  implicit none
  class(ParticleTrackingEngineType) :: this
  integer,intent(in) :: cellNumber
  integer :: n
  
  n = cellNumber
  do while(.true.)
      if(n .eq. 0) return
      if(this%IboundTS(n) .ne. 0) return
      n = this%Grid%GetFaceConnection(n, 5, 1)
  end do
      
end function pr_GetTopMostActiveCell

subroutine pr_WriteCellBuffer(this, unit, cellData, backwardTracking)
!***************************************************************************************************************
! Description goes here
!***************************************************************************************************************
!
! Specifications
!---------------------------------------------------------------------------------------------------------------
  implicit none
  class(ParticleTrackingEngineType) :: this
  integer,intent(in) :: unit
  logical,intent(in) :: backwardTracking
  type(ModpathCellDataType),intent(in) :: cellData
!---------------------------------------------------------------------------------------------------------------
  
  write(unit, '(1X,A)')     '-------------------------------------'
  write(unit, '(1X,A,I10)') '      Cell', cellData%CellNumber
  write(unit, '(1X,A)')     '-------------------------------------'
  call WriteCellData(unit, cellData, backwardTracking)
  
end subroutine pr_WriteCellBuffer

subroutine WriteCellData(unit, cellData, backwardTracking)
!***************************************************************************************************************
! Description goes here
!***************************************************************************************************************
!
! Specifications
!---------------------------------------------------------------------------------------------------------------
  implicit none
  integer,intent(in) :: unit
  logical,intent(in) :: backwardTracking
  type(ModpathCellDataType),intent(in) :: cellData
  type(ModpathSubCellDataType) :: subCellData
  integer :: m, n, count, row, column, subRowCount, subColumnCount
  doubleprecision :: balance, totalFaceInflow, totalFaceOutflow, sourceInflow,  &
    sinkOutflow, storageInflow, storageOutflow, netFaceInflow
  doubleprecision :: totalInflow, totalOutflow, netInflow
!---------------------------------------------------------------------------------------------------------------
  write(unit, '(1X,A,5I10)')   'Layer, Ibound, IboundTS, Zone, LayerType: ',    &
    cellData%Layer, cellData%Ibound, cellData%IboundTS, cellData%Zone,          &
    cellData%LayerType
  write(unit, '(1X,A,4E15.7)') 'DX, DY, MinX, MinY:    ', cellData%DX,          &
    cellData%DY, cellData%MinX, cellData%MinY
  write(unit, '(1X,A,3E15.7)') 'Bottom, Top, Head:     ', cellData%Bottom,      &
    cellData%Top, cellData%Head
  write(unit, '(1x,A,2E15.7)') 'Porosity, Retardation: ', cellData%Porosity,    &
    cellData%Retardation
  
  write(unit, *)
  write(unit, '(1X,A)') 'Volumetric Face Flows (L**3/T):'
  write(unit, '(17X,4(15X,A))') 'sub-face 1', 'sub-face 2', 'sub-face 3', 'sub-face 4'
  write(unit, '(1X,A,4E25.15)') 'Left   (face 1):',(cellData%GetFaceFlow(1,n), n = 1, cellData%GetSubFaceCount(1))
  write(unit, '(1X,A,4E25.15)') 'Right  (face 2):',(cellData%GetFaceFlow(2,n), n = 1, cellData%GetSubFaceCount(2))
  write(unit, '(1X,A,4E25.15)') 'Front  (face 3):',(cellData%GetFaceFlow(3,n), n = 1, cellData%GetSubFaceCount(3))
  write(unit, '(1X,A,4E25.15)') 'Back   (face 4):',(cellData%GetFaceFlow(4,n), n = 1, cellData%GetSubFaceCount(4))
  write(unit, '(1X,A,4E25.15)') 'Bottom (face 5):',(cellData%GetFaceFlow(5,n), n = 1, cellData%GetSubFaceCount(5))
  write(unit, '(1X,A,4E25.15)') 'Top    (face 6):',(cellData%GetFaceFlow(6,n), n = 1, cellData%GetSubFaceCount(6))
  
  call cellData%GetVolumetricBalanceComponents(totalFaceInflow,                 &
    totalFaceOutflow, sourceInflow, sinkOutflow, storageInflow, storageOutflow, balance)
  totalInflow = totalFaceInflow + sourceInflow + storageInflow
  totalOutflow = totalFaceOutflow + sinkOutflow + storageOutflow
  netInflow = totalInflow - totalOutflow
  write(unit, *)
  write(unit, '(1X,A)') 'Water balance components:'
  write(unit, '(1X,A)') 'Inflow (L**3/T)'
  write(unit, '(1X,A,E25.15)')    '     Total face inflow =', totalFaceInflow
  write(unit, '(1X,A,E25.15)')    '         Source inflow =', sourceInflow
  write(unit, '(1X,A,E25.15)')    '        Storage inflow =', storageInflow
  write(unit, '(27X,A)')          '-----------------------'
  write(unit, '(1X,A,E25.15)')    '               Inflow  =', totalInflow
  write(unit, *)
  write(unit, '(1X,A)') 'Outflow (L**3/T)'
  write(unit, '(1X,A,E25.15)')    '    Total face outflow =', totalFaceOutflow
  write(unit, '(1X,A,E25.15)')    '          Sink outflow =', sinkOutflow
  write(unit, '(1X,A,E25.15)')    '       Storage outflow =', storageOutflow
  write(unit, '(27X,A)')          '-----------------------'
  write(unit, '(1X,A,E25.15)')    '                Ouflow =', totalOutflow
  write(unit, *)
  write(unit, '(1X,A,E25.15)')    '      Inflow - Outflow =', netInflow
  write(unit, *)
  write(unit, '(1X,A,E25.15)')    '   Percent discrepancy =', balance
  
  write(unit, *)
  if(backwardTracking) then
      write(unit, '(1X,A)')                                                     &
        'Face velocities (Backward tracking. Velocity components has been reversed to represent backward tracking.)'
  else
      write(unit, '(1X,A)') 'Face velocities (Forward tracking)'
  end if
  subRowCount = cellData%GetSubCellRowCount()
  subColumnCount = cellData%GetSubCellColumnCount()
  do row = 1, subRowCount
      do column = 1, subColumnCount
          call cellData%FillSubCellDataBuffer(subCellData, row, column, backwardTracking)
          write(unit, '(1X,A,I2,A,I2,A)') 'SubCell (', row, ', ', column, ')'
          write(unit, '(1X,A,3E15.7)')    'DX, DY, DZ: ', subCellData%DX, subCellData%DY, subCellData%DZ
          write(unit, '(23X,A,5X,A)') 'Face Velocity (L/T)', 'Connection'  
          write(unit, '(1X,A,E25.15,I15)') 'Left   (face 1):', subCellData%VX1, subCellData%Connection(1)
          write(unit, '(1X,A,E25.15,I15)') 'Right  (face 2):', subCellData%VX2, subCellData%Connection(2)
          write(unit, '(1X,A,E25.15,I15)') 'Front  (face 3):', subCellData%VY1, subCellData%Connection(3)
          write(unit, '(1X,A,E25.15,I15)') 'Back   (face 4):', subCellData%VY2, subCellData%Connection(4)
          write(unit, '(1X,A,E25.15,I15)') 'Bottom (face 5):', subCellData%VZ1, subCellData%Connection(5)
          write(unit, '(1X,A,E25.15,I15)') 'Top    (face 6):', subCellData%VZ2, subCellData%Connection(6)
          write(unit, *)
      end do
  end do

end subroutine WriteCellData

subroutine WriteTraceData(unit, trackCell, tcResult, stressPeriod, timeStep)
!***************************************************************************************************************
! Description goes here
!***************************************************************************************************************
!
! Specifications
!---------------------------------------------------------------------------------------------------------------
  implicit none
  type(TrackCellType),intent(in),target :: trackCell
  type(TrackCellResultType),intent(in),target :: tcResult
  type(ModpathCellDataType),pointer :: cellData
  type(ModpathSubCellDataType) :: subCellData
  integer,intent(in) :: unit, stressPeriod, timeStep
  integer :: m, n, count, row, column, subRowCount, subColumnCount
  character(len=28) :: statusText
!---------------------------------------------------------------------------------------------------------------
  cellData => trackCell%CellData
  count = tcResult%TrackingPoints%GetItemCount()          
  write(unit, *)
  write(unit, '(1X,A,I10,A,I6,A,I6)') '----- Call TrackCell: Cell',             &
    cellData%CellNumber, ',  Stress period',stressPeriod,                       &
    ',  Time step', timeStep

  select case (tcResult%Status)
      case (1)
          statusText = '  (Reached stopping time)'
      case (2)
          statusText = '  (Exit at cell face)'
      case (3)
          statusText = '  (Stop at weak sink)'
      case (4)
          statusText = '  (Stop at weak source)'
      case (5)
          statusText = '  (No exit possible)'
      case (6)
          statusText = '  (Stop zone cell)'
      case (7)
          statusText = '  (Inactive cell)'
      case (8)
          statusText = '  (Inactive cell)'
      case default
          statusText = '  (Undefined)'
  end select
      
  write(unit, '(1X,A,I3,A)') 'Exit status =', tcResult%Status, statusText
  write(unit, '(1X,A)')         'Particle locations: Local X, Local Y, Local Z, Tracking time'
  do n = 1, count
      write(unit, '(1X,I10,4E25.15,I10)')                                       &
        tcResult%TrackingPoints%Items(n)%CellNumber,                            &
        tcResult%TrackingPoints%Items(n)%LocalX,                                &
        tcResult%TrackingPoints%Items(n)%LocalY,                                &
        tcResult%TrackingPoints%Items(n)%LocalZ,                                &
        tcResult%TrackingPoints%Items(n)%TrackingTime,                          &
        tcResult%TrackingPoints%Items(n)%Layer              
  end do
  write(unit, *)
  
  call WriteCellData(unit, cellData, trackCell%TrackingOptions%BackwardTracking)
  
end subroutine WriteTraceData

subroutine pr_GetVolumetricBalanceSummary(this, intervalCount, intervalBreaks,  &
  balanceCounts, maxError, maxErrorCell)
!***************************************************************************************************************
! Description goes here
!***************************************************************************************************************
!
! Specifications
!---------------------------------------------------------------------------------------------------------------
  implicit none
  class(ParticleTrackingEngineType) :: this
  integer, intent(in) :: intervalCount
  doubleprecision, dimension(intervalCount), intent(in) :: intervalBreaks
  integer, dimension(intervalCount + 1), intent(inout) :: balanceCounts
  integer, intent(inout) :: maxErrorCell
  doubleprecision, intent(inout) :: maxError
  type(ModpathCellDataType) :: cellBuffer
  integer :: cellCount, n, m
  doubleprecision :: balance, absBalance
  
  
  cellCount = this%Grid%CellCount
  
  maxErrorCell = 0
  maxError = 0.0d0
  do m = 1, intervalCount + 1
      balanceCounts(m) = 0
  end do
  
  do n = 1, cellCount
      call this%FillCellBuffer(n, cellBuffer)
      if(cellBuffer%IboundTS .gt. 0) then
          balance = cellBuffer%GetVolumetricBalance()
          absBalance = dabs(balance)
          if((maxErrorCell .eq. 0) .or. (absBalance .gt. maxError) ) then
              maxError = absBalance
              maxErrorCell = n
          end if
          do m = 1, intervalCount
              if(absBalance .le. intervalBreaks(m)) then
                  balanceCounts(m) = balanceCounts(m) + 1
                  exit
              end if
              if(m .eq. intervalCount) balanceCounts(intervalCount + 1) =       &
                balanceCounts(intervalCount + 1) + 1
          end do
      end if
  end do

end subroutine pr_GetVolumetricBalanceSummary

subroutine pr_SetDefaultIface(this, defaultIfaceLabels, defaultIfaceValues,     &
  arraySize)
!***************************************************************************************************************
! Description goes here
!***************************************************************************************************************
!
! Specifications
!---------------------------------------------------------------------------------------------------------------
  use UtilMiscModule,only : utrimall
  implicit none
  class(ParticleTrackingEngineType) :: this
  integer,intent(in) :: arraySize
  integer,dimension(arraySize),intent(in) :: defaultIfaceValues
  character(len=16),dimension(arraySize),intent(in) :: defaultIfaceLabels
  integer :: n, firstNonBlank, lastNonBlank, trimmedLength
  character(len=16) :: label
!---------------------------------------------------------------------------------------------------------------
  
  this%DefaultIfaceCount = 0
  do n = 1, 20
      this%DefaultIfaceValues(n) = 0
      this%DefaultIfaceLabels(n) = '                '
  end do
  
  do n = 1, arraySize
      this%DefaultIfaceValues(n) = defaultIfaceValues(n)
      label = defaultIfaceLabels(n)
      call utrimall(label)
      this%DefaultIfaceLabels(n) = label
  end do
  this%DefaultIfaceCount = arraySize

end subroutine pr_SetDefaultIface

!subroutine pr_SetLayerTypes(this, layerTypes, arraySize)
!!***************************************************************************************************************
!!
!!***************************************************************************************************************
!!
!! Specifications
!!---------------------------------------------------------------------------------------------------------------
!  implicit none
!  class(ParticleTrackingEngineType) :: this
!  integer,intent(in) :: arraySize
!  integer,dimension(arraySize),intent(in),target :: layerTypes
!  
!  if(arraySize .ne. this%Grid%LayerCount) then
!      write(*,*) "ParticleTrackingEngine: The LayerTypes array size does not match the layer count for the grid. stop"
!      stop
!  end if
!  
!  this%LayerTypes => layerTypes
!
!end subroutine pr_SetLayerTypes

subroutine pr_SetIbound(this, ibound, arraySize)
!***************************************************************************************************************
!
!***************************************************************************************************************
!
! Specifications
!---------------------------------------------------------------------------------------------------------------
  implicit none
  class(ParticleTrackingEngineType) :: this
  integer,intent(in) :: arraySize
  integer :: n
  integer,dimension(arraySize),intent(in),target :: ibound
  
  if(arraySize .ne. this%Grid%CellCount) then
      write(*,*) "ParticleTrackingEngine: The IBound array size does not match the cell count for the grid. stop"
      stop
  end if
  
  this%IBound => ibound
  ! Initialize the IBoundTS array to the same values as IBound whenever the IBound array is set.
  ! The values of IBoundTS will be updated for dry cells every time that data for a time step is loaded.
  do n = 1, arraySize
      this%IBoundTS(n) = this%IBound(n)
  end do

end subroutine pr_SetIbound

subroutine pr_SetZones(this, zones, arraySize)
!***************************************************************************************************************
!
!***************************************************************************************************************
!
! Specifications
!---------------------------------------------------------------------------------------------------------------
  implicit none
  class(ParticleTrackingEngineType) :: this
  integer,intent(in) :: arraySize
  integer,dimension(arraySize),intent(in),target :: zones
  
  if(arraySize .ne. this%Grid%CellCount) then
      write(*,*) "ParticleTrackingEngine: The Zones array size does not match the cell count for the grid. stop"
      stop
  end if
  
  this%Zones => zones

end subroutine pr_SetZones

subroutine pr_SetPorosity(this, porosity, arraySize)
!***************************************************************************************************************
!
!***************************************************************************************************************
!
! Specifications
!---------------------------------------------------------------------------------------------------------------
  implicit none
  class(ParticleTrackingEngineType) :: this
  integer,intent(in) :: arraySize
  doubleprecision,dimension(arraySize),intent(in),target :: porosity
  
  if(arraySize .ne. this%Grid%CellCount) then
      write(*,*) "ParticleTrackingEngine: The Porosity array size does not match the cell count for the grid. stop"
      stop
  end if
  
  this%Porosity => porosity

end subroutine pr_SetPorosity

subroutine pr_SetRetardation(this, retardation, arraySize)
!***************************************************************************************************************
!
!***************************************************************************************************************
!
! Specifications
!---------------------------------------------------------------------------------------------------------------
  implicit none
  class(ParticleTrackingEngineType) :: this
  integer,intent(in) :: arraySize
  doubleprecision,dimension(arraySize),intent(in),target :: retardation
  
  if(arraySize .ne. this%Grid%CellCount) then
      write(*,*) "ParticleTrackingEngine: The Retardation array size does not match the cell count for the grid. stop"
      stop
  end if
  
  this%Retardation => retardation

end subroutine pr_SetRetardation

function pr_FindTimeIndex(timeSeriesPoints, currentTime, maximumTime, timePointsCount) result(index)
!***************************************************************************************************************
! Find the index in the timeSeriesPoints array of the next stopping time after the currentTime value. 
! Return -1 if none is found or if maximumTime is less than the stopping time found in the timeSeriesPoints 
! array.
!***************************************************************************************************************
!
! Specifications
!---------------------------------------------------------------------------------------------------------------
  implicit none
  integer,intent(in) :: timePointsCount
  doubleprecision,dimension(timePointsCount),intent(in) :: timeSeriesPoints
  doubleprecision,intent(in) :: currentTime, maximumTime
  integer :: index, n
  doubleprecision :: t
!---------------------------------------------------------------------------------------------------------------
  index = -1
  
  if(timePointsCount .lt. 1) return
  do n = 1, timePointsCount
      if((timeSeriesPoints(n) .gt. currentTime) .and. (timeSeriesPoints(n) .le. maximumTime)) then
          index = n
          return
      end if
  end do

end function pr_FindTimeIndex

subroutine pr_Initialize(this,headReader, budgetReader, grid, hNoFlow, hDry, trackingOptions)
!***************************************************************************************************************
!
!***************************************************************************************************************
!
! Specifications
!---------------------------------------------------------------------------------------------------------------
  implicit none
  class(ParticleTrackingEngineType) :: this
  type(BudgetReaderType),intent(inout),target :: budgetReader
  type(HeadReaderType),intent(inout),target :: headReader
  class(ModflowRectangularGridType),intent(inout),pointer :: grid
  type(ParticleTrackingOptionsType),intent(in) :: trackingOptions
  integer :: cellCount,gridType
  integer :: n, flowArraySize
  doubleprecision :: hNoFlow, hDry
!---------------------------------------------------------------------------------------------------------------

  this%Initialized = .false.
  
  ! Call Reset to make sure that all arrays are initially unallocated
  call this%Reset()
  
  ! Return if the grid cell count equals 0
  cellCount = grid%CellCount
  if(cellCount .le. 0) return
  
  ! Check budget reader and grid data for compatibility and allocate appropriate cell-by-cell flow arrays
  gridType = grid%GridType
  select case (gridType)
      case (1)
          if((budgetReader%GetBudgetType() .ne. 1)) return
          if((headReader%GridStyle .ne. 1) .or. (headReader%CellCount .ne. cellCount)) return
          flowArraySize = budgetReader%GetFlowArraySize()
          if(flowArraySize .ne. cellCount) return
          allocate(this%FlowsRightFace(flowArraySize))
          allocate(this%FlowsFrontFace(flowArraySize))
          allocate(this%FlowsLowerFace(flowArraySize))
          allocate(this%FlowsJA(0))
      case (2)
          if((budgetReader%GetBudgetType() .ne. 2)) return
          if((headReader%GridStyle .ne. 2) .or. (headReader%CellCount .ne. cellCount)) return
          flowArraySize = budgetReader%GetFlowArraySize()
          if(flowArraySize .ne. grid%JaCount) return
          allocate(this%FlowsJA(flowArraySize))
          allocate(this%FlowsRightFace(0))
          allocate(this%FlowsFrontFace(0))
          allocate(this%FlowsLowerFace(0))
      case (3, 4)
          if((budgetReader%GetBudgetType() .ne. 2)) return
          if((headReader%GridStyle .ne. 1) .or. (headReader%CellCount .ne. cellCount)) return
          flowArraySize = budgetReader%GetFlowArraySize()
          if(flowArraySize .ne. grid%JaCount) return
          allocate(this%FlowsJA(flowArraySize))
          allocate(this%FlowsRightFace(0))
          allocate(this%FlowsFrontFace(0))
          allocate(this%FlowsLowerFace(0))          
      !case (4)
      !    ! Not implemented
      !    return
      case default
          return
  end select
  
  ! Set pointers to budgetReader and grid. Assign tracking options.
  this%HeadReader => headReader
  this%BudgetReader => budgetReader
  this%Grid => grid
  this%TrackingOptions = trackingOptions
  this%HNoFlow = hNoFlow
  this%HDry = hDry
  
  ! Allocate the rest of the arrays
  allocate(this%IBoundTS(cellCount))
  allocate(this%Heads(cellCount))
  allocate(this%SourceFlows(cellCount))
  allocate(this%SinkFlows(cellCount))
  allocate(this%StorageFlows(cellCount))
  allocate(this%SubFaceFlowsComputed(cellCount))
  allocate(this%BoundaryFlows(cellCount * 6))
  allocate(this%SubFaceFlows(cellCount * 4))
  
  ! Allocate buffers for reading array and list data
  allocate(this%ListItemBuffer(this%BudgetReader%GetMaximumListItemCount()))
  allocate(this%ArrayBufferDbl(this%BudgetReader%GetMaximumArrayItemCount()))  
  
  this%Initialized = .true.

end subroutine pr_Initialize

subroutine  pr_FillFaceFlowsBuffer(this,buffer,bufferSize,count)
!***************************************************************************************************************
!
!***************************************************************************************************************
!
! Specifications
!---------------------------------------------------------------------------------------------------------------
  implicit none
  class(ParticleTrackingEngineType) :: this
  integer,intent(in) :: bufferSize
  doubleprecision,intent(inout),dimension(bufferSize) :: buffer
  integer,intent(inout) :: count
  integer :: n,offset
!---------------------------------------------------------------------------------------------------------------
  
  do n = 1, bufferSize
      buffer(n) = 0.0d0
  end do
  
  count = size(this%FlowsJA)
  do n = 1, count
      buffer(n) = this%FlowsJA(n)
  end do
  
end subroutine pr_FillFaceFlowsBuffer
  
subroutine pr_FillCellFlowsBuffer(this,cellNumber,buffer,bufferSize,count)
!***************************************************************************************************************
!
!***************************************************************************************************************
!
! Specifications
!---------------------------------------------------------------------------------------------------------------
  implicit none
  class(ParticleTrackingEngineType) :: this
  integer,intent(in) :: cellNumber,bufferSize
  doubleprecision,intent(inout),dimension(bufferSize) :: buffer
  integer,intent(inout) :: count
  integer :: n,offset
!---------------------------------------------------------------------------------------------------------------
  
  do n = 1, bufferSize
      buffer(n) = 0.0d0
  end do
  
  !offset = this%Grid%GetOffsetJa(cellNumber)
  !count = this%Grid%GetOffsetJa(cellNumber + 1) - offset
  offset = this%Grid%JaOffsets(cellNumber)
  count = this%Grid%JaOffsets(cellNumber + 1) - offset
  do n = 1, count
      buffer(n) = this%FlowsJA(offset + n)
  end do
  
end subroutine pr_FillCellFlowsBuffer
  
function pr_GetCurrentStressPeriod(this) result(stressPeriod)
!***************************************************************************************************************
!
!***************************************************************************************************************
!
! Specifications
!---------------------------------------------------------------------------------------------------------------
  implicit none
  class(ParticleTrackingEngineType) :: this
  integer :: stressPeriod
!---------------------------------------------------------------------------------------------------------------
 
  stressPeriod = this%CurrentStressPeriod
  
end function pr_GetCurrentStressPeriod

function pr_GetCurrentTimeStep(this) result(timeStep)
 !***************************************************************************************************************
!
!***************************************************************************************************************
!
! Specifications
!---------------------------------------------------------------------------------------------------------------
 implicit none
  class(ParticleTrackingEngineType) :: this
  integer :: timeStep
!---------------------------------------------------------------------------------------------------------------
  
  timeStep = this%CurrentTimeStep
  
end function pr_GetCurrentTimeStep

subroutine pr_LoadTimeStep(this, stressPeriod, timeStep)
!***************************************************************************************************************
!
!***************************************************************************************************************
!
! Specifications
!---------------------------------------------------------------------------------------------------------------
  implicit none
  class(ParticleTrackingEngineType) :: this
  integer,intent(in) :: stressPeriod, timeStep
  integer :: firstRecord, lastRecord, n, m, firstNonBlank, lastNonBlank,        &
    trimmedLength
  integer :: spaceAssigned, status,cellCount, iface, index,                     &
    boundaryFlowsOffset, listItemBufferSize, cellNumber, layer
  type(BudgetRecordHeaderType) :: header
  character(len=16) :: textLabel
  doubleprecision :: top
!---------------------------------------------------------------------------------------------------------------
  
  call this%ClearTimeStepBudgetData()
  call this%BudgetReader%GetRecordHeaderRange(stressPeriod, timeStep, firstRecord, lastRecord)
  if(firstRecord .eq. 0) return

  cellCount = this%Grid%CellCount
  listItemBufferSize = size(this%ListItemBuffer)
  
  ! Set steady state = true, then change it if the budget file contains storage
  this%SteadyState = .true.
  
  ! Load heads for this time step
  call this%HeadReader%FillTimeStepHeadBuffer(stressPeriod, timeStep,           &
    this%Heads, cellCount, spaceAssigned)
  
  ! Fill IBoundTS array and set the SaturatedTop array for the Grid.
  ! The saturated top is set equal to the top for confined cells and water table cells 
  ! where the head is above the top or below the bottom.
  if(this%Grid%GridType .gt. 2) then
      do n = 1, cellCount
          this%Grid%SaturatedTop(n) = this%Grid%Top(n)
          this%StorageFlows(n) = 0.0
          this%IBoundTS(n) = this%IBound(n)
          layer = this%Grid%GetLayer(n)
          if(this%Grid%CellType(n) .eq. 1) then
              if(this%Heads(n) .eq. this%HDry) then
                  this%IBoundTS(n) = 0
              else
                  if(this%Heads(n) .lt. this%Grid%Bottom(n)) then
                      this%IBoundTS(n) = 0
                      this%Grid%SaturatedTop(n) = this%Grid%Bottom(n)
                  end if
              end if
              if(this%IBoundTS(n) .ne. 0) then
                  if((this%Heads(n) .le. this%Grid%Top(n)) .and.                                 &
                    (this%Heads(n) .ge. this%Grid%Bottom(n))) then
                      this%Grid%SaturatedTop(n) = this%Heads(n)
                  end if
              end if
          end if
      end do
      
  else
      do n = 1, cellCount
          this%Grid%SaturatedTop(n) = this%Grid%Top(n)
          this%StorageFlows(n) = 0.0
          this%IBoundTS(n) = this%IBound(n)
          layer = this%Grid%GetLayer(n)
          if(this%Grid%CellType(n) .eq. 1) then
              if((this%Heads(n) .eq. this%HDry) .or. (this%Heads(n) .gt. 1.0d+6)) then
                  this%IBoundTS(n) = 0
              end if
              if(this%IBoundTS(n) .ne. 0) then
                  if((this%Heads(n) .le. this%Grid%Top(n)) .and.                                 &
                    (this%Heads(n) .ge. this%Grid%Bottom(n))) then
                      this%Grid%SaturatedTop(n) = this%Heads(n)
                  end if
              end if
          end if
      end do
  end if
  
  ! Loop through record headers
  do n = firstRecord, lastRecord
       header = this%BudgetReader%GetRecordHeader(n)
       textLabel = header%TextLabel
       call TrimAll(textLabel, firstNonBlank, lastNonBlank, trimmedLength)
       
       select case(textLabel(firstNonBlank:lastNonBlank))
       case('CONSTANT HEAD', 'CHD')
            ! Read constant head flows into the sinkFlows and sourceFlows arrays.
            ! For a standard budget file, Method = 0. For a compact budget file,
            ! Method = 2.
            if(header%Method .eq. 0) then
                call this%BudgetReader%FillRecordDataBuffer(header,             &
                  this%ArrayBufferDbl, cellCount, spaceAssigned, status)
                if(cellCount .eq. spaceAssigned) then
                    do m = 1, spaceAssigned
                        if(this%ArrayBufferDbl(m) .gt. 0.0d0) then
                            this%SourceFlows(m) = this%SourceFlows(m) +         &
                              this%ArrayBufferDbl(m)
                        else if(this%ArrayBufferDbl(m) .lt. 0.0d0) then
                            this%SinkFlows(m) = this%SinkFlows(m) +             &
                              this%ArrayBufferDbl(m)
                        end if
                    end do
                end if
            else if(header%Method .eq. 2) then
                call this%BudgetReader%FillRecordDataBuffer(header,             &
                  this%ListItemBuffer, listItemBufferSize, spaceAssigned, status)
                if(spaceAssigned .gt. 0) then
                    do m = 1, spaceAssigned
                        cellNumber = this%ListItemBuffer(m)%CellNumber
                        if(this%ListItemBuffer(m)%BudgetValue .gt. 0.0d0) then
                            this%SourceFlows(cellNumber) =                      &
                              this%SourceFlows(cellNumber) + this%ListItemBuffer(m)%BudgetValue
                        else if(this%ListItemBuffer(m)%BudgetValue .lt. 0.0d0) then
                            this%SinkFlows(cellNumber) =                        &
                              this%SinkFlows(cellNumber) + this%ListItemBuffer(m)%BudgetValue
                        end if
                    end do
                end if
            else if((header%Method .eq. 5) .or. (header%Method .eq. 6)) then
                call this%BudgetReader%FillRecordDataBuffer(header,             &
                  this%ListItemBuffer, listItemBufferSize, spaceAssigned,       &
                  status)
                if(spaceAssigned .gt. 0) then
                    do m = 1, spaceAssigned
                        call this%CheckForDefaultIface(header%TextLabel, iface)
                        index = header%FindAuxiliaryNameIndex('IFACE')
                        if(index .gt. 0) then
                            iface = int(this%ListItemBuffer(m)%AuxiliaryValues(index))
                        end if
                        
                        cellNumber = this%ListItemBuffer(m)%CellNumber
                        if(iface .gt. 0) then
                            boundaryFlowsOffset = 6 * (cellNumber - 1)
                            this%BoundaryFlows(boundaryFlowsOffset + iface) =   &
                              this%BoundaryFlows(boundaryFlowsOffset + iface) + &
                              this%ListItemBuffer(m)%BudgetValue
                        else
                            if(this%ListItemBuffer(m)%BudgetValue .gt. 0.0d0) then
                                this%SourceFlows(cellNumber) =                  &
                                  this%SourceFlows(cellNumber) +                &
                                  this%ListItemBuffer(m)%BudgetValue
                            else if(this%ListItemBuffer(m)%BudgetValue .lt. 0.0d0) then
                                this%SinkFlows(cellNumber) =                    &
                                  this%SinkFlows(cellNumber) +                  &
                                  this%ListItemBuffer(m)%BudgetValue
                            end if
                        end if
                    end do
                end if
            end if
           
       case('STORAGE', 'STO-SS', 'STO-SY')
            ! Read storage for all cells into the StorageFlows array.
            ! Method should always be 0 or 1, but check anyway to be sure.
            if((header%Method .eq. 0) .or. (header%Method .eq. 1)) then
                if(header%ArrayItemCount .eq. cellCount) then
                    call this%BudgetReader%FillRecordDataBuffer(header,         &
                      this%ArrayBufferDbl, cellCount, spaceAssigned, status)
                    if(cellCount .eq. spaceAssigned) then
                        do m = 1, spaceAssigned
                            this%StorageFlows(m) = this%StorageFlows(m) + this%ArrayBufferDbl(m)
                            if(this%StorageFlows(m) .ne. 0.0) this%SteadyState = .false.
                        end do
                    end if
                end if
            end if
           
       case('FLOW JA FACE', 'FLOW-JA-FACE')
            ! Read connected face flows into the FlowsJA array for unstructured grids.
            if((header%Method .eq. 0) .or. (header%Method .eq. 1)) then
                ! Method should always be 0 or 1 for flow between grid cells. 
                if(header%ArrayItemCount .eq. this%BudgetReader%GetFlowArraySize()) then
                    call this%BudgetReader%FillRecordDataBuffer(header,         &
                      this%FlowsJA, header%ArrayItemCount, spaceAssigned,       &
                      status)
                end if
            else if(header%Method .eq. 6) then
                ! Method code 6 indicates flow to or from cells in the current model grid
                ! and another connected model grid in a multi-model MODFLOW-6 simulation. 
                ! Treat flows to or from connected model grids as distributed source/sink flows 
                ! for the current grid.
                call this%BudgetReader%FillRecordDataBuffer(header,             &
                  this%ListItemBuffer, listItemBufferSize, spaceAssigned,       &
                  status)
                if(spaceAssigned .gt. 0) then
                    do m = 1, spaceAssigned
                        cellNumber = this%ListItemBuffer(m)%CellNumber
                        if(this%ListItemBuffer(m)%BudgetValue .gt. 0.0d0) then
                            this%SourceFlows(cellNumber) =                  &
                                this%SourceFlows(cellNumber) +                &
                                this%ListItemBuffer(m)%BudgetValue
                        else if(this%ListItemBuffer(m)%BudgetValue .lt. 0.0d0) then
                            this%SinkFlows(cellNumber) =                    &
                                this%SinkFlows(cellNumber) +                  &
                                this%ListItemBuffer(m)%BudgetValue
                        end if
                    end do
                end if
            end if
           
       case('FLOW RIGHT FACE')
            ! Read flows across the right face for structured grids.
            ! Method should always be 0 or 1, but check anyway to be sure.
            if((header%Method .eq. 0) .or. (header%Method .eq. 1)) then
                if(header%ArrayItemCount .eq. this%BudgetReader%GetFlowArraySize()) then
                    call this%BudgetReader%FillRecordDataBuffer(header,         &
                      this%FlowsRightFace, header%ArrayItemCount, spaceAssigned,&
                      status)
                end if
            end if
           
       case('FLOW FRONT FACE')
            ! Read flows across the front face for structured grids.
            ! Method should always be 0 or 1, but check anyway to be sure.
            if((header%Method .eq. 0) .or. (header%Method .eq. 1)) then
                if(header%ArrayItemCount .eq. this%BudgetReader%GetFlowArraySize()) then
                    call this%BudgetReader%FillRecordDataBuffer(header,         &
                      this%FlowsFrontFace, header%ArrayItemCount, spaceAssigned,&
                      status)
                end if
            end if
           
       case('FLOW LOWER FACE')
            ! Read flows across the lower face for structured grids.
            ! Method should always be 0 or 1, but check anyway to be sure.
            if((header%Method .eq. 0) .or. (header%Method .eq. 1)) then
                if(header%ArrayItemCount .eq. this%BudgetReader%GetFlowArraySize()) then
                    call this%BudgetReader%FillRecordDataBuffer(header,         &
                      this%FlowsLowerFace, header%ArrayItemCount, spaceAssigned,&
                      status)
                end if
            end if
       
        case default
            ! Now handle any other records in the budget file.
             if((header%Method .eq. 0) .or. (header%Method .eq. 1)) then
                if(header%ArrayItemCount .eq. cellCount) then
                    call this%BudgetReader%FillRecordDataBuffer(header,         &
                      this%ArrayBufferDbl, cellCount, spaceAssigned, status)
                    if(cellCount .eq. spaceAssigned) then
                        call this%CheckForDefaultIface(header%TextLabel, iface)
                        if(iface .gt. 0) then
                            do m = 1, spaceAssigned
                                boundaryFlowsOffset = 6 * (m - 1)
                                this%BoundaryFlows(boundaryFlowsOffset + iface) =   &
                                  this%BoundaryFlows(boundaryFlowsOffset + iface) + &
                                  this%ArrayBufferDbl(m)
                            end do
                        else
                            do m = 1, spaceAssigned
                                if(this%ArrayBufferDbl(m) .gt. 0.0d0) then
                                    this%SourceFlows(m) = this%SourceFlows(m) +     &
                                      this%ArrayBufferDbl(m)
                                else if(this%ArrayBufferDbl(m) .lt. 0.0d0) then
                                    this%SinkFlows(m) = this%SinkFlows(m) +         &
                                      this%ArrayBufferDbl(m)
                                end if
                            end do
                        end if
                    end if
                end if
             else if(header%Method .eq. 3) then
                 ! Not yet supported
             else if(header%Method .eq. 4) then
                call this%BudgetReader%FillRecordDataBuffer(header,             &
                  this%ArrayBufferDbl, header%ArrayItemCount, spaceAssigned,    &
                  status)
                if(header%ArrayItemCount .eq. spaceAssigned) then
                    call this%CheckForDefaultIface(header%TextLabel, iface)
                    if(iface .gt. 0) then
                        do m = 1, spaceAssigned
                            boundaryFlowsOffset = 6 * (m - 1)
                            this%BoundaryFlows(boundaryFlowsOffset + iface) =   &
                              this%BoundaryFlows(boundaryFlowsOffset + iface) + &
                              this%ArrayBufferDbl(m)
                        end do
                    else            
                        do m = 1, spaceAssigned
                            if(this%ArrayBufferDbl(m) .gt. 0.0d0) then
                                this%SourceFlows(m) = this%SourceFlows(m) +     &
                                  this%ArrayBufferDbl(m)
                            else if(this%ArrayBufferDbl(m) .lt. 0.0d0) then
                                this%SinkFlows(m) = this%SinkFlows(m) +         &
                                  this%ArrayBufferDbl(m)
                            end if
                        end do
                    end if
                end if
            else if(header%Method .eq. 2) then
                call this%BudgetReader%FillRecordDataBuffer(header,             &
                  this%ListItemBuffer, listItemBufferSize, spaceAssigned,       &
                  status)
                if(spaceAssigned .gt. 0) then
                    call this%CheckForDefaultIface(header%TextLabel, iface)
                    if(iface .gt. 0) then
                        do m = 1, spaceAssigned
                            cellNumber = this%ListItemBuffer(m)%CellNumber
                            boundaryFlowsOffset = 6 * (cellNumber - 1)
                            this%BoundaryFlows(boundaryFlowsOffset + iface) =   &
                              this%BoundaryFlows(boundaryFlowsOffset + iface) + &
                              this%ListItemBuffer(m)%BudgetValue
                        end do
                    else            
                        do m = 1, spaceAssigned
                            cellNumber = this%ListItemBuffer(m)%CellNumber
                            if(this%ListItemBuffer(m)%BudgetValue .gt. 0.0d0) then
                                this%SourceFlows(cellNumber) =                  &
                                  this%SourceFlows(cellNumber) +                &
                                  this%ListItemBuffer(m)%BudgetValue
                            else if(this%ListItemBuffer(m)%BudgetValue .lt. 0.0d0) then
                                this%SinkFlows(cellNumber) =                    &
                                  this%SinkFlows(cellNumber) +                  &
                                  this%ListItemBuffer(m)%BudgetValue
                            end if
                        end do
                    end if
                end if
            else if((header%Method .eq. 5) .or. (header%Method .eq. 6)) then
                call this%BudgetReader%FillRecordDataBuffer(header,             &
                  this%ListItemBuffer, listItemBufferSize, spaceAssigned,       &
                  status)
                if(spaceAssigned .gt. 0) then
                    do m = 1, spaceAssigned
                        call this%CheckForDefaultIface(header%TextLabel, iface)
                        index = header%FindAuxiliaryNameIndex('IFACE')
                        if(index .gt. 0) then
                            iface = int(this%ListItemBuffer(m)%AuxiliaryValues(index))
                        end if
                        
                        cellNumber = this%ListItemBuffer(m)%CellNumber
                        if(iface .gt. 0) then
                            boundaryFlowsOffset = 6 * (cellNumber - 1)
                            this%BoundaryFlows(boundaryFlowsOffset + iface) =   &
                              this%BoundaryFlows(boundaryFlowsOffset + iface) + &
                              this%ListItemBuffer(m)%BudgetValue
                        else
                            if(this%ListItemBuffer(m)%BudgetValue .gt. 0.0d0) then
                                this%SourceFlows(cellNumber) =                  &
                                  this%SourceFlows(cellNumber) +                &
                                  this%ListItemBuffer(m)%BudgetValue
                            else if(this%ListItemBuffer(m)%BudgetValue .lt. 0.0d0) then
                                this%SinkFlows(cellNumber) =                    &
                                  this%SinkFlows(cellNumber) +                  &
                                  this%ListItemBuffer(m)%BudgetValue
                            end if
                        end if
                    end do
                end if
            end if
       
       end select
       
  end do

  this%CurrentStressPeriod = stressPeriod
  this%CurrentTimeStep = timeStep

end subroutine pr_LoadTimeStep

subroutine pr_CheckForDefaultIface(this, textLabel, iface)
!***************************************************************************************************************
!
!***************************************************************************************************************
!
! Specifications
!---------------------------------------------------------------------------------------------------------------
  use UtilMiscModule,only : utrimall
  implicit none
  class(ParticleTrackingEngineType) :: this
  character*(*), intent(in) :: textLabel
  integer,intent(inout) :: iface
  integer :: n
  character(len=16) :: label
!---------------------------------------------------------------------------------------------------------------
  
  iface = 0
  label = textLabel
  call utrimall(label)
  do n = 1, this%DefaultIfaceCount
      if(label .eq. this%DefaultIfaceLabels(n)) then
          iface = this%DefaultIfaceValues(n)
          return
      end if
  end do
  
end subroutine pr_CheckForDefaultIface

subroutine pr_ClearTimeStepBudgetData(this)
!***************************************************************************************************************
!
!***************************************************************************************************************
!
! Specifications
!---------------------------------------------------------------------------------------------------------------
  implicit none
  class(ParticleTrackingEngineType) :: this
  integer :: cellCount, n, arraySize
!---------------------------------------------------------------------------------------------------------------
  
  this%CurrentStressPeriod = 0
  this%CurrentTimeStep = 0
  
  if(allocated(this%SinkFlows)) then
      cellCount = this%Grid%CellCount
      do n = 1, cellCount
          this%IBoundTS(n) = this%IBound(n)
          this%Heads(n) = 0.0d0
          this%SourceFlows(n) = 0.0d0
          this%SinkFlows(n) = 0.0d0
          this%StorageFlows(n) = 0.0d0
          this%SubFaceFlowsComputed(n) = .false.
      end do
      
      arraySize = cellCount * 6
      do n = 1, arraySize
          this%BoundaryFlows(n) = 0.0d0
      end do
      
      arraySize = cellCount * 4
      do n = 1, arraySize
          this%SubFaceFlows(n) = 0.0d0
      end do
  
      arraySize = this%BudgetReader%GetFlowArraySize()
      if(this%Grid%GridType .eq. 1) then
          do n = 1, arraySize
          this%FlowsRightFace(n) = 0.0d0
          this%FlowsFrontFace(n) = 0.0d0
          this%FlowsLowerFace(n) = 0.0d0
          end do
      else if(this%Grid%GridType .eq. 2) then
          do n = 1, arraySize
              this%FlowsJA(n) = 0.0d0
          end do
      end if
     
  end if
  
end subroutine pr_ClearTimeStepBudgetData
  
subroutine pr_FillCellBuffer(this, cellNumber, cellBuffer)
!***************************************************************************************************************
!
!***************************************************************************************************************
!
! Specifications
!---------------------------------------------------------------------------------------------------------------
  implicit none
  class(ParticleTrackingEngineType) :: this
  integer,intent(in) :: cellNumber
  type(ModpathCellDataType),intent(inout) :: cellBuffer
  doubleprecision,dimension(6) :: boundaryFlows
  integer :: n, layer, boundaryFlowsOffset, gridType, cellType
!---------------------------------------------------------------------------------------------------------------
  
  boundaryFlowsOffset = 6 * (cellNumber - 1)
  do n = 1, 6
      boundaryFlows(n) = this%BoundaryFlows(boundaryFlowsOffset + n)
  end do
  
  layer = this%Grid%GetLayer(cellNumber)
  
  gridType = this%Grid%GridType
  cellType = this%Grid%CellType(cellNumber)
  select case (gridType)
      case (1)
          ! Set cell buffer data for a structured grid
          call cellBuffer%SetDataStructured(cellNumber,this%Grid%CellCount,     &
            this%Grid,this%IBound,this%IBoundTS(cellNumber),                    &
            this%Porosity(cellNumber),this%Retardation(cellNumber),             & 
            this%StorageFlows(cellNumber),this%SourceFlows(cellNumber),         &
            this%SinkFlows(cellNumber), this%FlowsRightFace,                    &
            this%FlowsFrontFace, this%FlowsLowerFace, boundaryFlows,            & 
            this%Heads(cellNumber), cellType,                                   &
            this%Zones(cellNumber))
      case (2)
          ! Set cell buffer data for a MODFLOW-USG unstructured grid
          call cellBuffer%SetDataUnstructured(cellNumber,this%Grid%CellCount,   &
            this%Grid%JaCount,this%Grid,                                        &
            this%IBound,this%IBoundTS(cellNumber),                              &
            this%Porosity(cellNumber),this%Retardation(cellNumber),             & 
            this%StorageFlows(cellNumber),this%SourceFlows(cellNumber),         &
            this%SinkFlows(cellNumber), this%FlowsJA, boundaryFlows,            & 
            this%Heads(cellNumber), cellType,                                   &
            this%Zones(cellNumber))
          ! Compute internal sub-cell face flows for cells with multiple sub-cells
          if(cellBuffer%GetSubCellCount() .gt. 1) then
              call cellBuffer%ComputeSubCellFlows()
          end if
      case (3)
          ! Set cell buffer data for a MODFLOW-6 structured grid (DIS)
          call cellBuffer%SetDataUnstructured(cellNumber,this%Grid%CellCount,   &
            this%Grid%JaCount,this%Grid,                                        &
            this%IBound,this%IBoundTS(cellNumber),                              &
            this%Porosity(cellNumber),this%Retardation(cellNumber),             & 
            this%StorageFlows(cellNumber),this%SourceFlows(cellNumber),         &
            this%SinkFlows(cellNumber), this%FlowsJA, boundaryFlows,            & 
            this%Heads(cellNumber), cellType,                                   &
            this%Zones(cellNumber))
      case (4)
          ! Set cell buffer data for a MODFLOW-6 unstructured grid (DISV)
          call cellBuffer%SetDataUnstructured(cellNumber,this%Grid%CellCount,   &
            this%Grid%JaCount,this%Grid,                                        &
            this%IBound,this%IBoundTS(cellNumber),                              &
            this%Porosity(cellNumber),this%Retardation(cellNumber),             & 
            this%StorageFlows(cellNumber),this%SourceFlows(cellNumber),         &
            this%SinkFlows(cellNumber), this%FlowsJA, boundaryFlows,            & 
            this%Heads(cellNumber), cellType,                                   &
            this%Zones(cellNumber))
           ! Compute internal sub-cell face flows for cells with multiple sub-cells
          if(cellBuffer%GetSubCellCount() .gt. 1) then
              call cellBuffer%ComputeSubCellFlows()
          end if
         
      case default
      ! Write error message and stop
  end select
  
end subroutine pr_FillCellBuffer

subroutine pr_Reset(this)
!***************************************************************************************************************
!
!***************************************************************************************************************
!
! Specifications
!---------------------------------------------------------------------------------------------------------------
  implicit none
!---------------------------------------------------------------------------------------------------------------
  class(ParticleTrackingEngineType) :: this
   
!   this%ReferenceTime = 0.0d0
!   this%StoppingTime = 0.0d0
   this%CurrentStressPeriod = 0
   this%CurrentTimeStep = 0
   this%BudgetReader => null()
   this%Grid => null()
   
   if(allocated(this%IBoundTS)) deallocate(this%IBoundTS)
   if(allocated(this%Heads)) deallocate(this%Heads)
   if(allocated(this%FlowsJA)) deallocate(this%FlowsJA)
   if(allocated(this%FlowsRightFace)) deallocate(this%FlowsRightFace)
   if(allocated(this%FlowsFrontFace)) deallocate(this%FlowsFrontFace)
   if(allocated(this%FlowsLowerFace)) deallocate(this%FlowsLowerFace)
   if(allocated(this%SourceFlows)) deallocate(this%SourceFlows)
   if(allocated(this%SinkFlows)) deallocate(this%SinkFlows)
   if(allocated(this%StorageFlows)) deallocate(this%StorageFlows)
   if(allocated(this%BoundaryFlows)) deallocate(this%BoundaryFlows)
   if(allocated(this%SubFaceFlows)) deallocate(this%SubFaceFlows)
   if(allocated(this%ArrayBufferDbl)) deallocate(this%ArrayBufferDbl)
   if(allocated(this%ListItemBuffer)) deallocate(this%ListItemBuffer)
   if(allocated(this%SubFaceFlowsComputed)) deallocate(this%SubFaceFlowsComputed)
   this%IBound => null()
   this%Porosity => null()
   this%Retardation => null()
   this%Zones => null()

end subroutine pr_Reset

subroutine pr_TrackPath(this, trackPathResult, traceModeOn, traceModeUnit,      &
  group, particleID, seqNumber, location, maximumTrackingTime, timeseriesPoints,&
  timeseriesPointCount)
!***************************************************************************************************************
!
!***************************************************************************************************************
!
! Specifications
!---------------------------------------------------------------------------------------------------------------
  implicit none
  class(ParticleTrackingEngineType),target :: this
  type(TrackPathResultType),target,intent(out) :: trackPathResult
  type(ParticleLocationType),intent(in) :: location
  integer,intent(in) :: group, particleID, seqNumber, timeseriesPointCount,     &
    traceModeUnit
  logical,intent(in) :: traceModeOn
  type(ParticleLocationType) :: loc
  type(ParticleCoordinateType) :: pCoord
  type(ModpathCellDataType),pointer :: cellData
  type(TrackCellResultType),pointer :: tcResult
  doubleprecision,intent(in) :: maximumTrackingTime
  doubleprecision,dimension(timeseriesPointCount),intent(in) :: timeseriesPoints
  doubleprecision :: stopTime, fromLocalX, fromLocalY, fromLocalZ, globalX,     &
    globalY, globalZ
  integer :: timeIndex, n, count, nextCell
  logical :: continueLoop, isTimeSeriesPoint, isMaximumTime
!---------------------------------------------------------------------------------------------------------------
  
  ! Reset trackPathResult and initialize particleID
  call trackPathResult%Reset()
  trackPathResult%ParticleID = particleID
  trackPathResult%Group = group
  trackPathResult%SequenceNumber = seqNumber
  
  ! Reset LocBuffP and LocBuffTS and initialize location data
  call this%LocBuffP%Clear()
  call this%LocBuffTS%Clear()
  call loc%SetData(location)
  call this%LocBuffP%AddItem(loc)
  
  ! Initialize loc
  call loc%SetData(location)
  
  ! Initialize TrackCell
  this%TrackCell%SteadyState = this%SteadyState
  this%TrackCell%TrackingOptions = this%TrackingOptions
  call this%FillCellBuffer(loc%CellNumber,  this%TrackCell%CellData)
  
  continueLoop = .true.
  isTimeSeriesPoint = .false.
  isMaximumTime = .false.
  
  do while(continueLoop)
      ! Check to see if the particle has moved to another cell. If so, load the new cell data
      if(loc%CellNumber .ne. this%TrackCell%CellData%CellNumber) then
          call this%FillCellBuffer(loc%CellNumber, this%TrackCell%CellData)
      end if
      
      ! Find the next stopping time value (tmax), then track the particle through the cell starting at location loc.
      timeIndex = -1
      if(timeseriesPointCount .gt. 0) then
          timeIndex = pr_FindTimeIndex(timeseriesPoints, loc%TrackingTime,      &
            maximumTrackingTime, timeseriesPointCount)
      end if
      stopTime = maximumTrackingTime
      isTimeSeriesPoint = .false.
      if(timeIndex .ne. -1) then
          stopTime = timeseriesPoints(timeIndex)
          isTimeSeriesPoint = .true.
      end if
      isMaximumTime = .false.
      if(stopTime .eq. maximumTrackingTime) isMaximumTime = .true.
      
      ! Start with the particle loacion loc and track it through the cell until it reaches
      ! an exit face or the tracking time reaches the value specified by stopTime
      call this%TrackCell%ExecuteTracking(loc, stopTime, this%TrackCellResult)
      
      ! Check the status flag of the result to find out what to do next
      if(this%TrackCellResult%Status .eq. this%TrackCellResult%Status_Undefined()) then
          continueLoop = .false.
          trackPathResult%Status = this%TrackCellResult%Status
      else if(this%TrackCellResult%Status .eq. this%TrackCellResult%Status_ExitAtCellFace()) then
          count = this%TrackCellResult%TrackingPoints%GetItemCount()
          if(count .gt. 1) then
              do n = 2, count
                  call this%LocBuffP%AddItem(this%TrackCellResult%TrackingPoints%Items(n))
              end do 
          end if
          
          ! If NextCellNumber is > 0, it means the particle has moved to another cell. 
          ! If so, convert loc from the current cell coordinates to the equivalent location in the new cell.
          nextCell = this%TrackCellResult%NextCellNumber
          if(nextCell .gt. 0) then
              if(this%IBoundTS(nextCell) .ne. 0) then
                  ! The next cell is active
                  fromLocalX = this%TrackCellResult%TrackingPoints%Items(count)%LocalX
                  fromLocalY = this%TrackCellResult%TrackingPoints%Items(count)%LocalY
                  fromLocalZ = this%TrackCellResult%TrackingPoints%Items(count)%LocalZ         
                  call this%Grid%ConvertFromNeighbor(                           &
                    this%TrackCellResult%NextCellNumber,                        &
                    this%TrackCellResult%CellNumber, fromLocalX, fromLocalY,    &
                    fromLocalZ, loc)
                  loc%TrackingTime = this%TrackCellResult%TrackingPoints%Items(count)%TrackingTime
              else
                  ! If next cell is inactive, it implies that a boundary face has been reached. 
                  ! Set status and return.
                  continueLoop = .false.
                  trackPathResult%Status = trackPathResult%Status_ReachedBoundaryFace()        
              end if
          else
              ! If next cell number = 0, the boundary of the grid has been reached. 
              ! Set status and return.
              continueLoop = .false.
              trackPathResult%Status = trackPathResult%Status_ReachedBoundaryFace()
          end if
          
      else if(this%TrackCellResult%Status .eq. this%TrackCellResult%Status_ReachedStoppingTime()) then
          count = this%TrackCellResult%TrackingPoints%GetItemCount()
          if(count .gt. 1) then
              do n = 2, count
                  call this%LocBuffP%AddItem(this%TrackCellResult%TrackingPoints%Items(n))
              end do 
              if(isTimeSeriesPoint) then
                  call this%LocBuffTS%AddItem(this%TrackCellResult%TrackingPoints%Items(count))
              end if
          end if
          
          call loc%SetData(this%TrackCellResult%TrackingPoints%Items(count))
          if(isMaximumTime) then
              continueLoop = .false.
              trackPathResult%Status = trackPathResult%Status_ReachedStoppingTime()
          end if
          
      else
          continueLoop = .false.
          if(this%TrackCellResult%Status .eq. this%TrackCellResult%Status_NoExitPossible()) then
              trackPathResult%Status = this%TrackCellResult%Status_NoExitPossible()         
          else if(this%TrackCellResult%Status .eq. this%TrackCellResult%Status_StopZoneCell()) then
              trackPathResult%Status = this%TrackCellResult%Status_StopZoneCell()                  
          else if(this%TrackCellResult%Status .eq. this%TrackCellResult%Status_StopAtWeakSink()) then
              trackPathResult%Status = this%TrackCellResult%Status_StopAtWeakSink()                   
          else if(this%TrackCellResult%Status .eq. this%TrackCellResult%Status_StopAtWeakSource()) then
              trackPathResult%Status = this%TrackCellResult%Status_StopAtWeakSource()
          else if(this%TrackCellResult%Status .eq. this%TrackCellResult%Status_InactiveCell()) then
              trackPathResult%Status = this%TrackCellResult%Status_InactiveCell()      
          else
              trackPathResult%Status = this%TrackCellResult%Status_Undefined()
          end if
          
          ! If the trackPathResult status is anything except Undefined, add the last tracking point to
          ! the trackPathResult tracking points
          if(trackPathResult%Status .ne. this%TrackCellResult%Status_Undefined()) then
              call this%LocBuffP%AddItem(this%TrackCellResult%TrackingPoints%Items(1))              
          end if
          
      end if
  
      ! Write trace mode data if the trace mode is on for this particle
      if(traceModeOn) then
         call WriteTraceData(traceModeUnit, this%TrackCell,                     &
           this%TrackCellResult, this%GetCurrentStressPeriod(),                 &
           this%GetCurrentTimeStep())
      end if
      
      ! If continueLoop is still set to true, go through the loop again. If set to false, exit the loop now.
  end do
  
  ! Generate global coordinates and finish initializing the result data
  count = this%LocBuffP%GetItemCount()
  do n = 1, count
      pCoord%CellNumber = this%LocBuffP%Items(n)%CellNumber
      pCoord%Layer = this%LocBuffP%Items(n)%Layer
      pCoord%LocalX = this%LocBuffP%Items(n)%LocalX
      pCoord%LocalY = this%LocBuffP%Items(n)%LocalY
      pCoord%LocalZ = this%LocBuffP%Items(n)%LocalZ
      pCoord%TrackingTime = this%LocBuffP%Items(n)%TrackingTime
      call this%Grid%ConvertToModelXYZ(pCoord%CellNumber, pCoord%LocalX,       &
        pCoord%LocalY, pCoord%LocalZ, pCoord%GlobalX, pCoord%GlobalY,           &
        pCoord%GlobalZ)
      call trackPathResult%ParticlePath%Pathline%AddItem(pCoord)
  end do
  
  do n = 1, this%LocBuffTS%GetItemCount()
      pCoord%CellNumber = this%LocBuffTS%Items(n)%CellNumber
      pCoord%Layer = this%LocBuffTS%Items(n)%Layer
      pCoord%LocalX = this%LocBuffTS%Items(n)%LocalX
      pCoord%LocalY = this%LocBuffTS%Items(n)%LocalY
      pCoord%LocalZ = this%LocBuffTS%Items(n)%LocalZ
      pCoord%TrackingTime = this%LocBuffTS%Items(n)%TrackingTime
      call this%Grid%ConvertToModelXYZ(pCoord%CellNumber, pCoord%LocalX,       &
        pCoord%LocalY, pCoord%LocalZ, pCoord%GlobalX, pCoord%GlobalY,           &
        pCoord%GlobalZ)
      call trackPathResult%ParticlePath%Timeseries%AddItem(pCoord)
  end do
  
end subroutine pr_TrackPath
  
end module ParticleTrackingEngineModule


!***************************************************************************************************************
!
!***************************************************************************************************************
!
! Specifications
!---------------------------------------------------------------------------------------------------------------
!  implicit none
!---------------------------------------------------------------------------------------------------------------
