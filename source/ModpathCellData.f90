module ModpathCellDataModule
  use ModflowRectangularGridModule,only : ModflowRectangularGridType
  !use RectangularUnstructuredGridModule, only : RectangularUnstructuredGridType
  use ParticleLocationModule,only : ParticleLocationType
  use ModpathSubCellDataModule,only : ModpathSubCellDataType
  implicit none
  
! Set default access status to private
  private

! Private data type declarations

  
! Public derived data type definitions
!--------------------------------------
! type: 
!--------------------------------------
  type,public :: ModpathCellDataType
    ! public data
    integer :: CellNumber, Layer, Ibound, IboundTS, Zone, LayerType
    doubleprecision :: DX, DY, MinX, MinY, Bottom, Top, Head, Porosity,Retardation,SourceFlow,SinkFlow,StorageFlow
    
    ! private data    
    integer,private :: SubCellRowCount,SubCellColumnCount,ReducedConnectionCount
    integer,private,dimension(6) :: SubFaceCounts,PotentialConnectionsCount,SubFaceBoundaryCounts
    integer,private,dimension(2) :: SubFaceConn1,SubFaceConn2,SubFaceConn3,SubFaceConn4
    integer,private,dimension(4) :: SubFaceConn5,SubFaceConn6
    doubleprecision,private,dimension(4) :: SubCellFlows,Q5,Q6
    doubleprecision,private,dimension(2) :: Q1,Q2,Q3,Q4
    integer,private :: ArraySizeMode = 1
  contains
    procedure :: GetDZ=>pr_GetDZ
    procedure :: GetArraySizeMode=>pr_GetArraySizeMode
    procedure :: SetArraySizeMode=>pr_SetArraySizeMode
    procedure :: SetFlowAndPropertyData=>pr_SetFlowAndPropertyData
    procedure :: Reset=>pr_Reset
    procedure :: HasExitFace=>pr_HasExitFace
    procedure :: GetSubCellRowCount=>pr_GetSubCellRowCount
    procedure :: GetSubCellColumnCount=>pr_GetSubCellColumnCount
    procedure :: GetSubCellCount=>pr_GetSubCellCount
    procedure :: GetReducedConnectionCount=>pr_GetReducedConnectionCount
    procedure :: GetSubFaceCount=>pr_GetSubFaceCount
    procedure :: GetFaceFlow=>pr_GetFaceFlow
    procedure :: GetFaceConnection=>pr_GetFaceConnection
    procedure :: SetSubCellFlows=>pr_SetSubCellFlows
    procedure :: GetSubCellFlow=>pr_GetSubCellFlow
    procedure :: GetAveragedFaceFlow=>pr_GetAveragedFaceFlow
    procedure :: FillSubCellDataBuffer=>pr_FillSubCellDataBuffer
    procedure :: GetSubCellData=>pr_GetSubCellData
    procedure :: FillSubCellFaceFlowsBuffer=>pr_FillSubCellFaceFlowsBuffer
    procedure :: AssignAveragedFaceFlowArray=>pr_AssignAveragedFaceFlowArray
    procedure :: GetVolumetricBalance=>pr_GetVolumetricBalance
    procedure :: GetVolumetricBalanceComponents=>pr_GetVolumetricBalanceComponents
    procedure :: ComputeSubCellFlows=>pr_ComputeSubCellFlows
    procedure,private :: GetSubFaceBoundaryCount=>pr_GetSubFaceBoundaryCount
    procedure,private :: GetSubCellBoundaryFlow=>pr_GetSubCellBoundaryFlow
    procedure,private :: pr_SetDataUnstructured
    procedure,private :: pr_SetDataStructured
    procedure :: SetDataUnstructured=>pr_SetDataUnstructured
    procedure :: SetDataStructured=>pr_SetDataStructured
    generic :: SetData=>pr_SetDataUnstructured
  end type

contains

  function pr_GetDZ(this) result(dz)
  implicit none
  class(ModpathCellDataType) :: this
  doubleprecision :: dz
  
  dz = this%Top - this%Bottom
  ! If the layer is convertible, set dz = Head - Bottom if Head < Top
  if(this%LayerType .eq. 1) then
      if(this%Head .lt. this%Top) dz = this%Head - this%Bottom
      ! If dz < 0, set dz to an arbitrary, small positive value
      if(dz .lt. 0.0d0) dz = 1.0d-4
  end if
  
  end function pr_GetDZ

  function pr_GetVolumetricBalance(this) result(balance)
  implicit none
  class(ModpathCellDataType) :: this
  doubleprecision :: balance,inflow,outflow
  integer :: n
  
  balance = 0.0d0
  inflow = 0.0d0
  outflow = 0.0d0
  
  inflow = this%SourceFlow
  outflow = -this%SinkFlow
  if(this%StorageFlow .ge. 0.0d0) then
      inflow = inflow + this%StorageFlow
  else
      outflow = outflow - this%StorageFlow
  end if
  
  ! Face 1
  do n = 1, this%SubFaceCounts(1)
      if(this%Q1(n) .ge. 0.0) then
          inflow = inflow + this%Q1(n)
      else
          outflow = outflow - this%Q1(n)
      end if
  end do
  
  ! Face 2
  do n = 1, this%SubFaceCounts(2)
      if(this%Q2(n) .le. 0.0) then
          inflow = inflow - this%Q2(n)
      else
          outflow = outflow + this%Q2(n)
      end if
  end do
  
  ! Face 3
  do n = 1, this%SubFaceCounts(3)
      if(this%Q3(n) .ge. 0.0) then
          inflow = inflow + this%Q3(n)
      else
          outflow = outflow - this%Q3(n)
      end if
  end do
  
  ! Face 4
  do n = 1, this%SubFaceCounts(4)
      if(this%Q4(n) .le. 0.0) then
          inflow = inflow - this%Q4(n)
      else
          outflow = outflow + this%Q4(n)
      end if
  end do
  
  ! Face 5
  do n = 1, this%SubFaceCounts(5)
      if(this%Q5(n) .ge. 0.0) then
          inflow = inflow + this%Q5(n)
      else
          outflow = outflow - this%Q5(n)
      end if
  end do
  
  ! Face 6
  do n = 1, this%SubFaceCounts(6)
      if(this%Q6(n) .le. 0.0) then
          inflow = inflow - this%Q6(n)
      else
          outflow = outflow + this%Q6(n)
      end if
  end do
  
  if((inflow .eq. 0.0d0) .and. (outflow .eq. 0.0d0)) then
      balance = 0.0d0
  else
      balance = 100.0d0 * (inflow - outflow) / ((inflow + outflow) / 2.0d0)
  end if
  
  end function pr_GetVolumetricBalance
  

  subroutine pr_GetVolumetricBalanceComponents(this, totalFaceInflow,           &
    totalFaceOutflow, sourceFlow, sinkFlow, storageInflow, storageOutflow,      &
    balance)
  implicit none
  class(ModpathCellDataType) :: this
  doubleprecision :: inflow,outflow
  doubleprecision,intent(inout) :: totalFaceInflow, totalFaceOutflow,           &
    sourceFlow, sinkFlow, storageInflow, storageOutflow, balance
  integer :: n
  
  balance = 0.0d0
  totalFaceInflow = 0.0d0
  totalFaceOutflow = 0.0d0
  sourceFlow = this%SourceFlow
  sinkFlow = -this%SinkFlow
  storageInflow = 0.0d0
  storageOutflow = 0.0d0
  if(this%StorageFlow .gt. 0.0d0) then
      storageInflow = this%StorageFlow
  else if(this%StorageFlow .lt. 0.0d0) then
      storageOutflow = -this%StorageFlow
  end if
  
  ! Face 1
  do n = 1, this%SubFaceCounts(1)
      if(this%Q1(n) .ge. 0.0) then
          totalFaceInflow = totalFaceInflow + this%Q1(n)
      else
          totalFaceOutflow = totalFaceOutflow - this%Q1(n)
      end if
  end do
  
  ! Face 2
  do n = 1, this%SubFaceCounts(2)
      if(this%Q2(n) .le. 0.0) then
          totalFaceInflow = totalFaceInflow - this%Q2(n)
      else
          totalFaceOutflow = totalFaceOutflow + this%Q2(n)
      end if
  end do
  
  ! Face 3
  do n = 1, this%SubFaceCounts(3)
      if(this%Q3(n) .ge. 0.0) then
          totalFaceInflow = totalFaceInflow + this%Q3(n)
      else
          totalFaceOutflow = totalFaceOutflow - this%Q3(n)
      end if
  end do
  
  ! Face 4
  do n = 1, this%SubFaceCounts(4)
      if(this%Q4(n) .le. 0.0) then
          totalFaceInflow = totalFaceInflow - this%Q4(n)
      else
          totalFaceOutflow = totalFaceOutflow + this%Q4(n)
      end if
  end do
  
  ! Face 5
  do n = 1, this%SubFaceCounts(5)
      if(this%Q5(n) .ge. 0.0) then
          totalFaceInflow = totalFaceInflow + this%Q5(n)
      else
          totalFaceOutflow = totalFaceOutflow - this%Q5(n)
      end if
  end do
  
  ! Face 6
  do n = 1, this%SubFaceCounts(6)
      if(this%Q6(n) .le. 0.0) then
          totalFaceInflow = totalFaceInflow - this%Q6(n)
      else
          totalFaceOutflow = totalFaceOutflow + this%Q6(n)
      end if
  end do
  
  ! Add in the internal sources and sinks to the appropriate inflow or outflow component
  inflow = totalFaceInflow + sourceFlow + storageInflow
  outflow = totalFaceOutflow + sinkFlow + storageOutflow
  
  if((inflow .eq. 0.0d0) .and. (outflow .eq. 0.0d0)) then
      balance = 0.0d0
  else
      balance = 100.0d0 * (inflow - outflow) / ((inflow + outflow) / 2.0d0)
  end if
  
  end subroutine pr_GetVolumetricBalanceComponents
  
!------------------------------------------
  function pr_GetArraySizeMode(this) result(mode)
  implicit none
  class(ModpathCellDataType) :: this
  integer :: mode
  
  mode = this%ArraySizeMode
  
  end function pr_GetArraySizeMode

!------------------------------------------
  subroutine pr_SetArraySizeMode(this,mode)
  implicit none
  class(ModpathCellDataType) :: this
  integer,intent(in) :: mode
  
  this%ArraySizeMode = mode
  call this%Reset()
  
  end subroutine pr_SetArraySizeMode
  
!------------------------------------------
  function pr_HasExitFace(this,backwardTracking) result(hasExit)
  implicit none
  class(ModpathCellDataType) :: this
  doubleprecision :: sign
  logical,intent(in) :: backwardTracking
  logical :: hasExit
  integer :: n
  doubleprecision :: q
  
  sign = 1.0d0
  if(backwardTracking) sign = - sign
  
  hasExit = .false.
  
  ! Check face 1
  do n = 1, this%subFaceCounts(1)
    q = sign*this%Q1(n)
    if(q .lt. 0.0d0) then
      hasExit = .true.
      return
    end if
  end do
  
  ! Check face 2
  do n = 1, this%subFaceCounts(2)
    q = sign*this%Q2(n)
    if(q .gt. 0.0d0) then
      hasExit = .true.
      return
    end if
  end do
  
  ! Check face 3
  do n = 1, this%subFaceCounts(3)
    q = sign*this%Q3(n)
    if(q .lt. 0.0d0) then
      hasExit = .true.
      return
    end if
  end do
  
  ! Check face 4
  do n = 1, this%subFaceCounts(4)
    q = sign*this%Q4(n)
    if(q .gt. 0.0d0) then
      hasExit = .true.
      return
    end if
  end do
  
  ! Check face 5
  do n = 1, this%subFaceCounts(5)
    q = sign*this%Q5(n)
    if(q .lt. 0.0d0) then
      hasExit = .true.
      return
    end if
  end do
  
  ! Check face 6
  do n = 1, this%subFaceCounts(6)
    q = sign*this%Q6(n)
    if(q .gt. 0.0d0) then
      hasExit = .true.
      return
    end if
  end do
  
  end function pr_HasExitFace
  
!------------------------------------------
  subroutine pr_Reset(this)
  implicit none
  class(ModpathCellDataType) :: this
  integer :: n
  
  this%CellNumber = 0
  this%DX = 0
  this%DY = 0
  this%MinX = 0.0d0
  this%MinY = 0.0d0
  this%Bottom = 0.0d0
  this%Top = 0.0d0
  this%Ibound = 0
  this%Zone = 0
  this%Porosity = 0d0
  this%Retardation = 0d0
  this%SourceFlow = 0d0
  this%SinkFlow =  0d0
  this%StorageFlow = 0d0
  this%ReducedConnectionCount = 0
  this%SubCellRowCount = 1
  this%SubCellColumnCount = 1
  
  do n = 1, 6
      this%SubFaceCounts(n) = 1
      this%SubFaceBoundaryCounts(n) = 1
  end do
  
  do n = 1, 2
      this%Q1(n) = 0.0d0
      this%Q2(n) = 0.0d0
      this%Q3(n) = 0.0d0
      this%Q4(n) = 0.0d0
      this%SubFaceConn1(n) = 0
      this%SubFaceConn2(n) = 0
      this%SubFaceConn3(n) = 0
      this%SubFaceConn4(n) = 0
  end do
  
  do n = 1, 4
      this%Q5(n) = 0.0d0
      this%Q6(n) = 0.0d0
      this%SubFaceConn5(n) = 0
      this%SubFaceConn6(n) = 0
      this%SubCellFlows(n) = 0.0d0
  end do

  end subroutine pr_Reset

!------------------------------------------
  subroutine pr_SetFlowAndPropertyData(this,ibound,porosity,retardation,        &
    arraySize,faceFlowsCount,faceFlows,connectionList,storageFlow,sourceFlow,   &
    sinkFlow,boundaryFlows)
  implicit none
  class(ModpathCellDataType) :: this
  integer :: n,index,cellNumber,count,i
  integer,intent(in) :: ibound,faceFlowsCount,arraySize
  integer,intent(in),dimension(arraySize) :: connectionList
  integer,dimension(6) :: subFaceBoundaryCounts
  doubleprecision :: flow
  doubleprecision,intent(in) :: porosity,retardation,storageFlow,sourceFlow,sinkFlow
  doubleprecision,intent(in),dimension(6) :: boundaryFlows
  doubleprecision,intent(in),dimension(arraySize) :: faceFlows
  
  ! Assign property data
  this%Ibound = ibound
  this%Porosity = porosity
  this%Retardation = retardation
  this%SourceFlow = sourceFlow
  this%SinkFlow = sinkFlow
  this%StorageFlow = storageFlow

  ! Process face data
  
  ! Face 1
  count = this%SubFaceCounts(1)
  do n = 1, count
    cellNumber = this%SubFaceConn1(n)
    if(cellNumber .gt. 0) then
      index = pr_FindConnectionNumberIndex(cellNumber,connectionList,arraySize,faceFlowsCount)
      this%Q1(n) = faceFlows(index)
    else
      this%Q1(n) = 0.0d0
    end if
  end do
  
  ! Face 2
  count = this%SubFaceCounts(2)
  do n = 1, count
    cellNumber = this%SubFaceConn2(n)
    if(cellNumber .gt. 0) then
      index = pr_FindConnectionNumberIndex(cellNumber,connectionList,arraySize,faceFlowsCount)
      this%Q2(n) = faceFlows(index)
    else
      this%Q2(n) = 0.0d0
    end if
  end do
  
  ! Face 3
  count = this%SubFaceCounts(3)
  do n = 1, count
    cellNumber = this%SubFaceConn3(n)
    if(cellNumber .gt. 0) then
      index = pr_FindConnectionNumberIndex(cellNumber,connectionList,arraySize,faceFlowsCount)
      this%Q3(n) = faceFlows(index)
    else
      this%Q3(n) = 0.0d0
    end if
  end do
  
  ! Face 4
  count = this%SubFaceCounts(4)
  do n = 1, count
    cellNumber = this%SubFaceConn4(n)
    if(cellNumber .gt. 0) then
      index = pr_FindConnectionNumberIndex(cellNumber,connectionList,arraySize,faceFlowsCount)
      this%Q4(n) = faceFlows(index)
    else
      this%Q4(n) = 0.0d0
    end if
  end do
  
  ! Face 5
  count = this%SubFaceCounts(5)
  do n = 1, count
    cellNumber = this%SubFaceConn5(n)
    if(cellNumber .gt. 0) then
      index = pr_FindConnectionNumberIndex(cellNumber,connectionList,arraySize,faceFlowsCount)
      this%Q5(n) = faceFlows(index)
    else
      this%Q5(n) = 0.0d0
    end if
  end do
  
  ! Face 6
  count = this%SubFaceCounts(6)
  do n = 1, count
    cellNumber = this%SubFaceConn1(n)
    if(cellNumber .gt. 0) then
      index = pr_FindConnectionNumberIndex(cellNumber,connectionList,arraySize,faceFlowsCount)
      this%Q6(n) = faceFlows(index)
    else
      this%Q6(n) = 0.0d0
    end if
  end do
  
  ! Process boundary flow data
  
  ! Face 1
  if(boundaryFlows(1) .ne. 0.0d0) then
      if(this%SubFaceBoundaryCounts(1) .gt. 0) then
        flow = boundaryFlows(1)/this%SubFaceBoundaryCounts(1)
        do n = 1, this%SubFaceCounts(1)
          if(this%SubFaceConn1(n) .eq. 0) then
            this%Q1(n) = flow
          end if
        end do
      else
        if(boundaryFlows(1) .gt. 0d0) then
          this%SourceFlow = this%SourceFlow + boundaryFlows(1)
        else
          this%SinkFlow = this%SinkFlow + boundaryFlows(1)
        end if
      end if
  end if

  ! Face 2
  if(boundaryFlows(2) .ne. 0.0d0) then
      if(this%SubFaceBoundaryCounts(2) .gt. 0) then
        flow = boundaryFlows(2)/this%SubFaceBoundaryCounts(2)
        do n = 1, this%SubFaceCounts(2)
          if(this%SubFaceConn2(n) .eq. 0) then
            this%Q2(n) = -flow
          end if
        end do
      else
        if(boundaryFlows(2) .gt. 0d0) then
          this%SourceFlow = this%SourceFlow + boundaryFlows(2)
        else
          this%SinkFlow = this%SinkFlow + boundaryFlows(2)
        end if
      end if
  end if
  
  ! Face 3
  if(boundaryFlows(3) .ne. 0.0d0) then
      if(this%SubFaceBoundaryCounts(3) .gt. 0) then
        flow = boundaryFlows(3)/this%SubFaceBoundaryCounts(3)
        do n = 1, this%SubFaceCounts(3)
          if(this%SubFaceConn3(n) .eq. 0) then
            this%Q3(n) = flow
          end if
        end do
      else
        if(boundaryFlows(3) .gt. 0d0) then
          this%SourceFlow = this%SourceFlow + boundaryFlows(3)
        else
          this%SinkFlow = this%SinkFlow + boundaryFlows(3)
        end if
      end if
  end if

  ! Face 4
  if(boundaryFlows(4) .ne. 0.0d0) then
      if(this%SubFaceBoundaryCounts(4) .gt. 0) then
        flow = boundaryFlows(4)/this%SubFaceBoundaryCounts(4)
        do n = 1, this%SubFaceCounts(4)
          if(this%SubFaceConn4(n) .eq. 0) then
            this%Q4(n) = -flow
          end if
        end do
      else
        if(boundaryFlows(4) .gt. 0d0) then
          this%SourceFlow = this%SourceFlow + boundaryFlows(4)
        else
          this%SinkFlow = this%SinkFlow + boundaryFlows(4)
        end if
      end if
  end if
  
  ! Face 5
  if(boundaryFlows(5) .ne. 0.0d0) then
      if(this%SubFaceBoundaryCounts(5) .gt. 0) then
        flow = boundaryFlows(5)/this%SubFaceBoundaryCounts(5)
        do n = 1, this%SubFaceCounts(5)
          if(this%SubFaceConn5(n) .eq. 0) then
            this%Q5(n) = flow
          end if
        end do
      else
        if(boundaryFlows(5) .gt. 0d0) then
          this%SourceFlow = this%SourceFlow + boundaryFlows(5)
        else
          this%SinkFlow = this%SinkFlow + boundaryFlows(5)
        end if
      end if
  end if

  ! Face 6
  if(boundaryFlows(6) .ne. 0.0d0) then
      if(this%SubFaceBoundaryCounts(6) .gt. 0) then
        flow = boundaryFlows(6)/this%SubFaceBoundaryCounts(6)
        do n = 1, this%SubFaceCounts(6)
          if(this%SubFaceConn6(n) .eq. 0) then
            this%Q6(n) = -flow
          end if
        end do
      else
        if(boundaryFlows(6) .gt. 0d0) then
          this%SourceFlow = this%SourceFlow + boundaryFlows(6)
        else
          this%SinkFlow = this%SinkFlow + boundaryFlows(6)
        end if
      end if
  end if
  
  end subroutine pr_SetFlowAndPropertyData

!------------------------------------------
  subroutine pr_SetDataUnstructured(this,cellNumber,cellCount,reducedConnectionCount,grid,&
    ibound,iboundTS,porosity,retardation,storageFlow,sourceFlow,sinkFlow,       &
    faceFlows,boundaryFlows, head, layerType, zone)
  implicit none
  class(ModpathCellDataType) :: this
  class(ModflowRectangularGridType),intent(in) :: grid
  integer,intent(in) :: cellNumber,cellCount
  integer,intent(in) :: reducedConnectionCount
  integer,intent(in) :: iboundTS, layerType, zone
  integer,intent(in),dimension(cellCount) :: ibound
  doubleprecision,intent(in) :: porosity, retardation, storageFlow, sourceFlow, sinkFlow
  doubleprecision,intent(in),dimension(6) :: boundaryFlows
  doubleprecision,intent(in),dimension(reducedConnectionCount) :: faceFlows
  doubleprecision,intent(in) :: head
  integer :: n,index,count,i,conn
  doubleprecision :: flow
  
  call this%Reset()

  this%CellNumber = cellNumber
  this%Layer = grid%GetLayer(cellNumber)
  this%DX = grid%DelX(cellNumber)
  this%DY = grid%DelY(cellNumber)
  this%MinX = grid%GetLeft(cellNumber)
  this%MinY = grid%GetFront(cellNumber)
  this%Bottom = grid%Bottom(cellNumber)
  this%Top = grid%Top(cellNumber)
  this%ReducedConnectionCount = grid%GetJaCellConnectionsCount(cellNumber)
  !this%ReducedConnectionCount = grid%GetReducedCellConnectionCount(cellNumber)
  
  ! Assign property data
  this%Zone = zone
  this%LayerType = layerType
  this%Head = head
  this%Ibound = ibound(cellNumber)
  this%IboundTS = iboundTS
  this%Porosity = porosity
  this%Retardation = retardation
  this%SourceFlow = sourceFlow
  this%SinkFlow = sinkFlow
  this%StorageFlow = storageFlow
  
  ! Process face flow data
  ! Face 1
  count = grid%GetPotentialFaceConnectionCount(cellNumber, 1)
  this%PotentialConnectionsCount(1) = count
  if(count .gt. 0) then
      this%SubFaceCounts(1) = count
      i = 0
      do n = 1, count
        conn = grid%GetFaceConnection(cellNumber,1,n)
        this%SubFaceConn1(n) = conn
        if(conn .eq. 0) then
            i = i + 1
        else
            if(ibound(conn) .eq. 0) i = i + 1
        end if
      end do
      this%SubFaceBoundaryCounts(1) = i
  end if
  
  
  ! Face 2
  count = grid%GetPotentialFaceConnectionCount(cellNumber, 2)
  this%PotentialConnectionsCount(2) = count
  if(count .gt. 0) then
      this%SubFaceCounts(2) = count
      i = 0
      do n = 1, count
        conn = grid%GetFaceConnection(cellNumber,2,n)
        this%SubFaceConn2(n) = conn
        if(conn .eq. 0) then
            i = i + 1
        else
            if(ibound(conn) .eq. 0) i = i + 1
        end if
      end do
      this%SubFaceBoundaryCounts(2) = i
  end if
  
  ! Face 3
  count = grid%GetPotentialFaceConnectionCount(cellNumber, 3)
  this%PotentialConnectionsCount(3) = count
  if(count .gt. 0) then
     this%SubFaceCounts(3) = count
      i = 0
      do n = 1, count
        conn = grid%GetFaceConnection(cellNumber,3,n)
        this%SubFaceConn3(n) = conn
        if(conn .eq. 0) then
            i = i + 1
        else
            if(ibound(conn) .eq. 0) i = i + 1
        end if
      end do
      this%SubFaceBoundaryCounts(3) = i
  end if
  
  ! Face 4
  count = grid%GetPotentialFaceConnectionCount(cellNumber, 4)
  this%PotentialConnectionsCount(4) = count
  if(count .gt. 0) then
      this%SubFaceCounts(4) = count
      i = 0
      do n = 1, count
        conn = grid%GetFaceConnection(cellNumber,4,n)
        this%SubFaceConn4(n) = conn
        if(conn .eq. 0) then
            i = i + 1
        else
            if(ibound(conn) .eq. 0) i = i + 1
        end if
      end do
      this%SubFaceBoundaryCounts(4) = i
  end if
  
  ! Face 5
  count = grid%GetPotentialFaceConnectionCount(cellNumber, 5)
  this%PotentialConnectionsCount(5) = count
  if(count .gt. 0) then
      this%SubFaceCounts(5) = count
      i = 0
      do n = 1, count
        conn = grid%GetFaceConnection(cellNumber,5,n)
        this%SubFaceConn5(n) = conn
        if(conn .eq. 0) then
            i = i + 1
        else
            if(ibound(conn) .eq. 0) i = i + 1
        end if
      end do
      this%SubFaceBoundaryCounts(5) = i
  end if
  
  ! Face 6
  count = grid%GetPotentialFaceConnectionCount(cellNumber, 6)
  this%PotentialConnectionsCount(6) = count
  if(count .gt. 0) then
      this%SubFaceCounts(6) = count
      i = 0
      do n = 1, count
        conn = grid%GetFaceConnection(cellNumber,6,n)
        this%SubFaceConn6(n) = conn
        if(conn .eq. 0) then
            i = i + 1
        else
            if(ibound(conn) .eq. 0) i = i + 1
        end if
      end do
      this%SubFaceBoundaryCounts(6) = i
  end if
  

  ! Process face data
  ! Face 1
  count = this%SubFaceCounts(1)
  do n = 1, count
    conn = this%SubFaceConn1(n)
    if(conn .gt. 0) then
      index = grid%FindConnectionIndex(cellNumber, conn)
      this%Q1(n) = faceFlows(index)
    end if
  end do
  
  ! Face 2
  count = this%SubFaceCounts(2)
  do n = 1, count
    conn = this%SubFaceConn2(n)
    if(conn .gt. 0) then
      index = grid%FindConnectionIndex(cellNumber, conn)
      this%Q2(n) = -faceFlows(index)
    end if
  end do
  
  ! Face 3
  count = this%SubFaceCounts(3)
  do n = 1, count
    conn = this%SubFaceConn3(n)
    if(conn .gt. 0) then
      index = grid%FindConnectionIndex(cellNumber, conn)
      this%Q3(n) = faceFlows(index)
    end if
  end do
  
  ! Face 4
  count = this%SubFaceCounts(4)
  do n = 1, count
    conn = this%SubFaceConn4(n)
    if(conn .gt. 0) then
      index = grid%FindConnectionIndex(cellNumber, conn)
      this%Q4(n) = -faceFlows(index)
    end if
  end do
  
  ! Face 5
  count = this%SubFaceCounts(5)
  do n = 1, count
    conn = this%SubFaceConn5(n)
    if(conn .gt. 0) then
      index = grid%FindConnectionIndex(cellNumber, conn)
      this%Q5(n) = faceFlows(index)
    end if
  end do
  
  ! Face 6
  count = this%SubFaceCounts(6)
  do n = 1, count
    conn = this%SubFaceConn6(n)
    if(conn .gt. 0) then
      index = grid%FindConnectionIndex(cellNumber, conn)
      this%Q6(n) = -faceFlows(index)
    end if
  end do
  
  ! Process boundary flow data
  ! Face 1
  if(boundaryFlows(1) .ne. 0.0d0) then
      if(this%SubFaceBoundaryCounts(1) .gt. 0) then
        flow = boundaryFlows(1)/this%SubFaceBoundaryCounts(1)
        do n = 1, this%SubFaceCounts(1)
          if(this%SubFaceConn1(n) .eq. 0) then
            this%Q1(n) = flow
          end if
        end do
      else
        if(boundaryFlows(1) .gt. 0d0) then
          this%SourceFlow = this%SourceFlow + boundaryFlows(1)
        else
          this%SinkFlow = this%SinkFlow + boundaryFlows(1)
        end if
      end if
  end if

  ! Face 2
  if(boundaryFlows(2) .ne. 0.0d0) then
      if(this%SubFaceBoundaryCounts(2) .gt. 0) then
        flow = boundaryFlows(2)/this%SubFaceBoundaryCounts(2)
        do n = 1, this%SubFaceCounts(2)
          if(this%SubFaceConn2(n) .eq. 0) then
            this%Q2(n) = -flow
          end if
        end do
      else
        if(boundaryFlows(2) .gt. 0d0) then
          this%SourceFlow = this%SourceFlow + boundaryFlows(2)
        else
          this%SinkFlow = this%SinkFlow + boundaryFlows(2)
        end if
      end if
  end if
  
  ! Face 3
  if(boundaryFlows(3) .ne. 0.0d0) then
      if(this%SubFaceBoundaryCounts(3) .gt. 0) then
        flow = boundaryFlows(3)/this%SubFaceBoundaryCounts(3)
        do n = 1, this%SubFaceCounts(3)
          if(this%SubFaceConn3(n) .eq. 0) then
            this%Q3(n) = flow
          end if
        end do
      else
        if(boundaryFlows(3) .gt. 0d0) then
          this%SourceFlow = this%SourceFlow + boundaryFlows(3)
        else
          this%SinkFlow = this%SinkFlow + boundaryFlows(3)
        end if
      end if
  end if

  ! Face 4
  if(boundaryFlows(4) .ne. 0.0d0) then
      if(this%SubFaceBoundaryCounts(4) .gt. 0) then
        flow = boundaryFlows(4)/this%SubFaceBoundaryCounts(4)
        do n = 1, this%SubFaceCounts(4)
          if(this%SubFaceConn4(n) .eq. 0) then
            this%Q4(n) = -flow
          end if
        end do
      else
        if(boundaryFlows(4) .gt. 0d0) then
          this%SourceFlow = this%SourceFlow + boundaryFlows(4)
        else
          this%SinkFlow = this%SinkFlow + boundaryFlows(4)
        end if
      end if
  end if
  
  ! Face 5
  if(boundaryFlows(5) .ne. 0.0d0) then
      if(this%SubFaceBoundaryCounts(5) .gt. 0) then
        flow = boundaryFlows(5)/this%SubFaceBoundaryCounts(5)
        do n = 1, this%SubFaceCounts(5)
          if(this%SubFaceConn5(n) .eq. 0) then
            this%Q5(n) = flow
          end if
        end do
      else
        if(boundaryFlows(5) .gt. 0d0) then
          this%SourceFlow = this%SourceFlow + boundaryFlows(5)
        else
          this%SinkFlow = this%SinkFlow + boundaryFlows(5)
        end if
      end if
  end if

  ! Face 6
  if(boundaryFlows(6) .ne. 0.0d0) then
      if(this%SubFaceBoundaryCounts(6) .gt. 0) then
        flow = boundaryFlows(6)/this%SubFaceBoundaryCounts(6)
        do n = 1, this%SubFaceCounts(6)
          if(this%SubFaceConn6(n) .eq. 0) then
            this%Q6(n) = -flow
          end if
        end do
      else
        if(boundaryFlows(6) .gt. 0d0) then
          this%SourceFlow = this%SourceFlow + boundaryFlows(6)
        else
          this%SinkFlow = this%SinkFlow + boundaryFlows(6)
        end if
      end if
  end if
  
  ! Set sub-cell row and column count
  this%SubCellRowCount = 1
  this%SubCellColumnCount = 1
  do n = 1, 6
      if(this%SubFaceCounts(n) .gt. 1) then
          this%SubCellRowCount = 2
          this%SubCellColumnCount = 2
      end if
  end do

  end subroutine pr_SetDataUnstructured

!------------------------------------------
  subroutine pr_SetDataStructured(this,cellNumber,cellCount,grid,ibound,        &
    iboundTS,porosity,retardation,storageFlow,sourceFlow,sinkFlow,              &
    flowsRightFace,flowsFrontFace,flowsLowerFace,boundaryFlows, head,           &
    layerType, zone)
  implicit none
  class(ModpathCellDataType) :: this
  class(ModflowRectangularGridType),intent(in) :: grid
  integer,intent(in) :: cellNumber
  integer,intent(in) :: cellCount
  integer,intent(in) :: iboundTS, layerType, zone
  integer,intent(in),dimension(cellCount) :: ibound
  doubleprecision,intent(in) :: porosity, retardation, storageFlow, sourceFlow, sinkFlow
  doubleprecision,intent(in),dimension(6) :: boundaryFlows
  doubleprecision,intent(in),dimension(cellCount) :: flowsRightFace
  doubleprecision,intent(in),dimension(cellCount) :: flowsFrontFace
  doubleprecision,intent(in),dimension(cellCount) :: flowsLowerFace
  doubleprecision,intent(in) :: head
  integer :: n,index,count,i,conn
  doubleprecision :: flow
  
  call this%Reset()

  this%CellNumber = cellNumber
  this%Layer = grid%GetLayer(cellNumber)
  this%DX = grid%DelX(cellNumber)
  this%DY = grid%DelY(cellNumber)
  this%MinX = grid%GetLeft(cellNumber)
  this%MinY = grid%GetFront(cellNumber)
  this%Bottom = grid%Bottom(cellNumber)
  this%Top = grid%Top(cellNumber)
  this%ReducedConnectionCount = grid%GetJaCellConnectionsCount(cellNumber)
  !this%ReducedConnectionCount = grid%GetReducedCellConnectionCount(cellNumber)
  
  ! Assign property data
  this%Zone = zone
  this%LayerType = layerType
  this%Head = head
  this%Ibound = ibound(cellNumber)
  this%IboundTS = iboundTS
  this%Porosity = porosity
  this%Retardation = retardation
  this%SourceFlow = sourceFlow
  this%SinkFlow = sinkFlow
  this%StorageFlow = storageFlow
  
  ! Process face flow data
  ! Face 1
  count = grid%GetPotentialFaceConnectionCount(cellNumber, 1)
  this%PotentialConnectionsCount(1) = count
  if(count .gt. 0) then
      this%SubFaceCounts(1) = count
      i = 0
      do n = 1, count
        conn = grid%GetFaceConnection(cellNumber,1,n)
        this%SubFaceConn1(n) = conn
        if(conn .eq. 0) then
            i = i + 1
        else
            if(ibound(conn) .eq. 0) i = i + 1
        end if
      end do
      this%SubFaceBoundaryCounts(1) = i
  end if
  
  
  ! Face 2
  count = grid%GetPotentialFaceConnectionCount(cellNumber, 2)
  this%PotentialConnectionsCount(2) = count
  if(count .gt. 0) then
      this%SubFaceCounts(2) = count
      i = 0
      do n = 1, count
        conn = grid%GetFaceConnection(cellNumber,2,n)
        this%SubFaceConn2(n) = conn
        if(conn .eq. 0) then
            i = i + 1
        else
            if(ibound(conn) .eq. 0) i = i + 1
        end if
      end do
      this%SubFaceBoundaryCounts(2) = i
  end if
  
  ! Face 3
  count = grid%GetPotentialFaceConnectionCount(cellNumber, 3)
  this%PotentialConnectionsCount(3) = count
  if(count .gt. 0) then
     this%SubFaceCounts(3) = count
      i = 0
      do n = 1, count
        conn = grid%GetFaceConnection(cellNumber,3,n)
        this%SubFaceConn3(n) = conn
        if(conn .eq. 0) then
            i = i + 1
        else
            if(ibound(conn) .eq. 0) i = i + 1
        end if
      end do
      this%SubFaceBoundaryCounts(3) = i
  end if
  
  ! Face 4
  count = grid%GetPotentialFaceConnectionCount(cellNumber, 4)
  this%PotentialConnectionsCount(4) = count
  if(count .gt. 0) then
      this%SubFaceCounts(4) = count
      i = 0
      do n = 1, count
        conn = grid%GetFaceConnection(cellNumber,4,n)
        this%SubFaceConn4(n) = conn
        if(conn .eq. 0) then
            i = i + 1
        else
            if(ibound(conn) .eq. 0) i = i + 1
        end if
      end do
      this%SubFaceBoundaryCounts(4) = i
  end if
  
  ! Face 5
  count = grid%GetPotentialFaceConnectionCount(cellNumber, 5)
  this%PotentialConnectionsCount(5) = count
  if(count .gt. 0) then
      this%SubFaceCounts(5) = count
      i = 0
      do n = 1, count
        conn = grid%GetFaceConnection(cellNumber,5,n)
        this%SubFaceConn5(n) = conn
        if(conn .eq. 0) then
            i = i + 1
        else
            if(ibound(conn) .eq. 0) i = i + 1
        end if
      end do
      this%SubFaceBoundaryCounts(5) = i
  end if
  
  ! Face 6
  count = grid%GetPotentialFaceConnectionCount(cellNumber, 6)
  this%PotentialConnectionsCount(6) = count
  if(count .gt. 0) then
      this%SubFaceCounts(6) = count
      i = 0
      do n = 1, count
        conn = grid%GetFaceConnection(cellNumber,6,n)
        this%SubFaceConn6(n) = conn
        if(conn .eq. 0) then
            i = i + 1
        else
            if(ibound(conn) .eq. 0) i = i + 1
        end if
      end do
      this%SubFaceBoundaryCounts(6) = i
  end if
  

  ! Process face data
  ! Face 1
  if(this%SubFaceCounts(1) .eq. 1) then
    conn = this%SubFaceConn1(1)
    if(conn .gt. 0) then
      this%Q1(1) = flowsRightFace(conn)
    end if
  end if
  
  ! Face 2
  if(this%SubFaceCounts(2) .eq. 1) then
    this%Q2(1) = flowsRightFace(cellNumber)
  end if
  
  ! Face 3
  if(this%SubFaceCounts(3) .eq. 1) then
    this%Q3(1) = -flowsFrontFace(cellNumber)
  end if
  
  ! Face 4
  if(this%SubFaceCounts(4) .eq. 1) then
    conn = this%SubFaceConn4(1)
    if(conn .gt. 0) then
      this%Q4(1) = -flowsFrontFace(conn)
    end if
  end if
  
  ! Face 5
  if(this%SubFaceCounts(5) .eq. 1) then
    this%Q5(1) = -flowsLowerFace(cellNumber)
  end if
  
  ! Face 6
  if(this%SubFaceCounts(6) .eq. 1) then
    conn = this%SubFaceConn6(1)
    if(conn .gt. 0) then
      this%Q6(1) = -flowsLowerFace(conn)
    end if
  end if
  
  ! Process boundary flow data
  ! Face 1
  if(boundaryFlows(1) .ne. 0.0d0) then
      if(this%SubFaceBoundaryCounts(1) .gt. 0) then
        flow = boundaryFlows(1)/this%SubFaceBoundaryCounts(1)
        do n = 1, this%SubFaceCounts(1)
          if(this%SubFaceConn1(n) .eq. 0) then
            this%Q1(n) = flow
          end if
        end do
      else
        if(boundaryFlows(1) .gt. 0d0) then
          this%SourceFlow = this%SourceFlow + boundaryFlows(1)
        else
          this%SinkFlow = this%SinkFlow + boundaryFlows(1)
        end if
      end if
  end if

  ! Face 2
  if(boundaryFlows(2) .ne. 0.0d0) then
      if(this%SubFaceBoundaryCounts(2) .gt. 0) then
        flow = boundaryFlows(2)/this%SubFaceBoundaryCounts(2)
        do n = 1, this%SubFaceCounts(2)
          if(this%SubFaceConn2(n) .eq. 0) then
            this%Q2(n) = -flow
          end if
        end do
      else
        if(boundaryFlows(2) .gt. 0d0) then
          this%SourceFlow = this%SourceFlow + boundaryFlows(2)
        else
          this%SinkFlow = this%SinkFlow + boundaryFlows(2)
        end if
      end if
  end if
  
  ! Face 3
  if(boundaryFlows(3) .ne. 0.0d0) then
      if(this%SubFaceBoundaryCounts(3) .gt. 0) then
        flow = boundaryFlows(3)/this%SubFaceBoundaryCounts(3)
        do n = 1, this%SubFaceCounts(3)
          if(this%SubFaceConn3(n) .eq. 0) then
            this%Q3(n) = flow
          end if
        end do
      else
        if(boundaryFlows(3) .gt. 0d0) then
          this%SourceFlow = this%SourceFlow + boundaryFlows(3)
        else
          this%SinkFlow = this%SinkFlow + boundaryFlows(3)
        end if
      end if
  end if

  ! Face 4
  if(boundaryFlows(4) .ne. 0.0d0) then
      if(this%SubFaceBoundaryCounts(4) .gt. 0) then
        flow = boundaryFlows(4)/this%SubFaceBoundaryCounts(4)
        do n = 1, this%SubFaceCounts(4)
          if(this%SubFaceConn4(n) .eq. 0) then
            this%Q4(n) = -flow
          end if
        end do
      else
        if(boundaryFlows(4) .gt. 0d0) then
          this%SourceFlow = this%SourceFlow + boundaryFlows(4)
        else
          this%SinkFlow = this%SinkFlow + boundaryFlows(4)
        end if
      end if
  end if
  
  ! Face 5
  if(boundaryFlows(5) .ne. 0.0d0) then
      if(this%SubFaceBoundaryCounts(5) .gt. 0) then
        flow = boundaryFlows(5)/this%SubFaceBoundaryCounts(5)
        do n = 1, this%SubFaceCounts(5)
          if(this%SubFaceConn5(n) .eq. 0) then
            this%Q5(n) = flow
          end if
        end do
      else
        if(boundaryFlows(5) .gt. 0d0) then
          this%SourceFlow = this%SourceFlow + boundaryFlows(5)
        else
          this%SinkFlow = this%SinkFlow + boundaryFlows(5)
        end if
      end if
  end if

  ! Face 6
  if(boundaryFlows(6) .ne. 0.0d0) then
      if(this%SubFaceBoundaryCounts(6) .gt. 0) then
        flow = boundaryFlows(6)/this%SubFaceBoundaryCounts(6)
        do n = 1, this%SubFaceCounts(6)
          if(this%SubFaceConn6(n) .eq. 0) then
            this%Q6(n) = -flow
          end if
        end do
      else
        if(boundaryFlows(6) .gt. 0d0) then
          this%SourceFlow = this%SourceFlow + boundaryFlows(6)
        else
          this%SinkFlow = this%SinkFlow + boundaryFlows(6)
        end if
      end if
  end if
  
  ! Set sub-cell row and column count. For structured grids, all cells have only 1 sub-cell.
  this%SubCellRowCount = 1
  this%SubCellColumnCount = 1

  end subroutine pr_SetDataStructured

!------------------------------------------
  function pr_GetSubFaceBoundaryCount(this,faceNumber) result(count)
  implicit none
  class(ModpathCellDataType) :: this
  integer,intent(in) :: faceNumber
  integer :: count
  
  end function

!------------------------------------------
  function pr_FindConnectionNumberIndex(cellNumber,connectionList,arraySize,connectionCount) result(index)
  implicit none
  integer,intent(in) :: cellNumber,connectionCount,arraySize
  integer,intent(in),dimension(connectionCount) :: connectionList
  integer :: index,n
  
  index = 0
  do n = 1,arraySize
    if(connectionList(n) .eq. cellNumber) then
      index = n
      exit
    end if
  end do
  
  end function pr_FindConnectionNumberIndex

!------------------------------------------
  function pr_GetReducedConnectionCount(this) result(fval)
  implicit none
  class(ModpathCellDataType) :: this
  integer :: fval
  
  fval = this%ReducedConnectionCount

  end function pr_GetReducedConnectionCount

!------------------------------------------
  function pr_GetSubFaceCount(this,faceNumber) result(count)
  implicit none
  class(ModpathCellDataType) :: this
  integer,intent(in) :: faceNumber
  integer :: count
  
  count = this%SubFaceCounts(faceNumber)
  
  end function pr_GetSubFaceCount

!------------------------------------------
  function pr_GetSubCellRowCount(this) result(fval)
  implicit none
  class(ModpathCellDataType) :: this
  integer :: fval
  
  fval = this%SubCellRowCount

  end function pr_GetSubCellRowCount

!------------------------------------------
  function pr_GetSubCellColumnCount(this) result(fval)
  implicit none
  class(ModpathCellDataType) :: this
  integer :: fval
  
  fval = this%SubCellColumnCount

  end function pr_GetSubCellColumnCount

!------------------------------------------
  function pr_GetSubCellCount(this) result(fval)
  implicit none
  class(ModpathCellDataType) :: this
  integer :: fval
  
  fval = this%SubCellColumnCount * this%SubCellRowCount

  end function pr_GetSubCellCount
  
!------------------------------------------
  function pr_GetFaceConnection(this,faceNumber,subFaceNumber) result(conn)
  implicit none
  class(ModpathCellDataType) :: this
  integer,intent(in) :: faceNumber,subFaceNumber
  integer :: conn
  
  conn = 0
  if(subFaceNumber .lt. 1) return
  
  select case (faceNumber)
    case (1)
      if(subFaceNumber .le. this%SubFaceCounts(1)) conn = this%SubFaceConn1(subFaceNumber)
    case (2)
      if(subFaceNumber .le. this%SubFaceCounts(2)) conn = this%SubFaceConn2(subFaceNumber)
    case (3)
      if(subFaceNumber .le. this%SubFaceCounts(3)) conn = this%SubFaceConn3(subFaceNumber)
    case (4)
      if(subFaceNumber .le. this%SubFaceCounts(4)) conn = this%SubFaceConn4(subFaceNumber)
    case (5)
      if(subFaceNumber .le. this%SubFaceCounts(5)) conn = this%SubFaceConn5(subFaceNumber)
    case (6)
      if(subFaceNumber .le. this%SubFaceCounts(6)) conn = this%SubFaceConn6(subFaceNumber)
    case default
      conn = 0
  end select
  
  end function pr_GetFaceConnection
  
!------------------------------------------
  function pr_GetFaceFlow(this,faceNumber,subFaceNumber) result(flow)
  implicit none
  class(ModpathCellDataType) :: this
  integer,intent(in) :: faceNumber,subFaceNumber
  doubleprecision :: flow 
  
  flow = 0.0d0
  if(subFaceNumber .lt. 1) return
  
  select case (faceNumber)
    case (1)
      if(subFaceNumber .le. this%SubFaceCounts(1)) flow = this%Q1(subFaceNumber)
    case (2)
      if(subFaceNumber .le. this%SubFaceCounts(2)) flow = this%Q2(subFaceNumber)
    case (3)
      if(subFaceNumber .le. this%SubFaceCounts(3)) flow = this%Q3(subFaceNumber)
    case (4)
      if(subFaceNumber .le. this%SubFaceCounts(4)) flow = this%Q4(subFaceNumber)
    case (5)
      if(subFaceNumber .le. this%SubFaceCounts(5)) flow = this%Q5(subFaceNumber)
    case (6)
      if(subFaceNumber .le. this%SubFaceCounts(6)) flow = this%Q6(subFaceNumber)
    case default
      flow = 0.0d0
  end select
  
  end function pr_GetFaceFlow
  
!------------------------------------------
  subroutine pr_SetSubCellFlows(this,subFlow1,subFlow2,subFlow3,subFlow4)
  implicit none
  class(ModpathCellDataType) :: this
  doubleprecision :: subFlow1,subFlow2,subFlow3,subFlow4
  
  this%SubCellFlows(1) = subFlow1
  this%SubCellFlows(2) = subFlow2
  this%SubCellFlows(3) = subFlow3
  this%SubCellFlows(4) = subFlow4
  
  end subroutine pr_SetSubCellFlows
  
!------------------------------------------
  function pr_GetSubCellFlow(this, n) result(flow)
  implicit none
  class(ModpathCellDataType) :: this
  doubleprecision :: flow
  integer :: n
  
  flow = this%SubCellFlows(n)
  
  end function pr_GetSubCellFlow

!------------------------------------------
  subroutine pr_SolveGauss(a,b,x,n)
    !============================================================
    ! Solutions to a system of linear equations A*x=b
    ! Method: the basic elimination (simple Gauss elimination)
    ! Alex G. November 2009
    !-----------------------------------------------------------
    ! input ...
    ! a(n,n) - array of coefficients for matrix A
    ! b(n)   - vector of the right hand coefficients b
    ! n      - number of equations
    ! output ...
    ! x(n)   - solutions
    ! comments ...
    ! the original arrays a(n,n) and b(n) will be destroyed 
    ! during the calculation
    !===========================================================
    implicit none 
    integer n
    double precision a(n,n), b(n), x(n)
    double precision c
    integer i, j, k

    !step 1: forward elimination
    do k=1, n-1
       do i=k+1,n
          c=a(i,k)/a(k,k)
          a(i,k) = 0.0
          b(i)=b(i)- c*b(k)
          do j=k+1,n
             a(i,j) = a(i,j)-c*a(k,j)
          end do
       end do
    end do

    !step 2: back substitution
    x(n) = b(n)/a(n,n)
    do i=n-1,1,-1
       c=0.0
       do j=i+1,n
         c= c + a(i,j)*x(j)
       end do 
       x(i) = (b(i)- c)/a(i,i)
    end do
end subroutine pr_SolveGauss

!------------------------------------------
  subroutine pr_ComputeSubCellFlows(this)
  implicit none 
  class(ModpathCellDataType) :: this
  doubleprecision,dimension(4,4) :: a
  doubleprecision,dimension(4) :: b,h,subFlows
  doubleprecision :: rhs1,rhs2,rhs3,qfaces,qsrc,qsink,qsto
  
  rhs1 = 0d0
  rhs2 = 0d0
  rhs3 = 0d0
  
  if(this%GetSubCellCount() .eq. 1) return
  
  ! Initialize matrix
  a(1,1) = 2
  a(1,2) = -1
  a(1,3) = -1
  a(1,4) = 0
  b(1) = 0
  
  a(2,1) = -1
  a(2,2) = 2
  a(2,3) = 0
  a(2,4) = -1
  b(2) = 0
  
  a(3,1) = -1
  a(3,2) = 0
  a(3,3) = 2
  a(3,4) = -1
  b(3) = 0
  
  a(4,1) = 0
  a(4,2) = 0
  a(4,3) = 0
  a(4,4) = 1
  b(4) = 0
  
  ! Compute internal source/sink values and set the right hand side
  qsrc = this%SourceFlow / 4d0
  qsink = this%SinkFlow / 4d0
  qsto = this%StorageFlow / 4d0
  
  ! Sub-cell 1
  qfaces = 0d0
  qfaces = qfaces + this%GetSubCellBoundaryFlow(1, 1)
  qfaces = qfaces - this%GetSubCellBoundaryFlow(4, 1)
  qfaces = qfaces + this%GetSubCellBoundaryFlow(5, 1)
  qfaces = qfaces - this%GetSubCellBoundaryFlow(6, 1)
  b(1) = qfaces + qsrc + qsink + qsto
  
  ! Sub-cell 2
  qfaces = 0d0
  qfaces = qfaces - this%GetSubCellBoundaryFlow(2, 1)
  qfaces = qfaces - this%GetSubCellBoundaryFlow(4, 2)
  qfaces = qfaces + this%GetSubCellBoundaryFlow(5, 2)
  qfaces = qfaces - this%GetSubCellBoundaryFlow(6, 2)
  b(2) = qfaces + qsrc + qsink + qsto
  
  ! Sub-cell 3
  qfaces = 0d0
  qfaces = qfaces + this%GetSubCellBoundaryFlow(1, 2)
  qfaces = qfaces + this%GetSubCellBoundaryFlow(3, 1)
  qfaces = qfaces + this%GetSubCellBoundaryFlow(5, 3)
  qfaces = qfaces - this%GetSubCellBoundaryFlow(6, 3)
  b(3) = qfaces + qsrc + qsink + qsto
  
  ! Solve equations using Gaussian elimination
  call pr_SolveGauss(a,b,h,4)
  
  ! Compute and assign sub-cell flows
  this%SubCellFlows(1) = h(1) - h(2)
  this%SubCellFlows(2) = h(3)
  this%SubCellFlows(3) = h(3) - h(1)
  this%SubCellFlows(4) = -h(2)
  
  end subroutine pr_ComputeSubCellFlows

!------------------------------------------
  function pr_GetSubCellBoundaryFlow(this, faceNumber, subFaceNumber) result(flow)
  implicit none 
  class(ModpathCellDataType) :: this
  integer,intent(in) :: faceNumber,subFaceNumber
  doubleprecision :: flow
  
  select case(faceNumber)
       case (1)
            if(this%SubFaceCounts(1) .eq. 1) then
                 flow = this%Q1(1) / 2.0d0
            else
                 flow = this%Q1(subFaceNumber)
            end if
       case (2)
            if(this%SubFaceCounts(2) .eq. 1) then
                 flow = this%Q2(1) / 2.0d0
            else
                 flow = this%Q2(subFaceNumber)
            end if
       case (3)
            if(this%SubFaceCounts(3) .eq. 1) then
                 flow = this%Q3(1) / 2.0d0
            else
                 flow = this%Q3(subFaceNumber)
            end if
       case (4)
            if(this%SubFaceCounts(4) .eq. 1) then
                 flow = this%Q4(1) / 2.0d0
            else
                 flow = this%Q4(subFaceNumber)
            end if
       case (5)
            if(this%SubFaceCounts(5) .eq. 1) then
                 flow = this%Q5(1) / 4.0d0
            else
                 flow = this%Q5(subFaceNumber)
            end if
       case (6)
            if(this%SubFaceCounts(6) .eq. 1) then
                 flow = this%Q6(1) / 4.0d0
            else
                 flow = this%Q6(subFaceNumber)
            end if
  end select
  
  end function pr_GetSubCellBoundaryFlow

!------------------------------------------
  function pr_GetAveragedFaceFlow(this, faceNumber) result(flow)
  implicit none 
  class(ModpathCellDataType) :: this
  integer,intent(in) :: faceNumber
  integer :: n, arraySize
  doubleprecision :: flow
  
  flow = 0d0
  select case (faceNumber)
    case (1)
      arraySize = size(this%Q1)
      do n = 1, arraySize
        flow = flow + this%Q1(n)
      end do
    case (2)
      arraySize = size(this%Q2)
      do n = 1, arraySize
        flow = flow + this%Q2(n)
      end do
    case (3)
      arraySize = size(this%Q3)
      do n = 1, arraySize
        flow = flow + this%Q3(n)
      end do
    case (4)
      arraySize = size(this%Q4)
      do n = 1, arraySize
        flow = flow + this%Q4(n)
      end do
    case (5)
      arraySize = size(this%Q5)
      do n = 1, arraySize
        flow = flow + this%Q5(n)
      end do
    case (6)
      arraySize = size(this%Q6)
      do n = 1, arraySize
        flow = flow + this%Q6(n)
      end do
    case default
      ! do nothing
  end select
  

  end function pr_GetAveragedFaceFlow

!------------------------------------------
  subroutine pr_AssignAveragedFaceFlowArray(this, flows)
  implicit none 
  class(ModpathCellDataType) :: this
  doubleprecision,dimension(6) :: flows
  integer :: n
  
  do n = 1, 6
    flows(n) = this%GetAveragedFaceFlow(n)
  end do
  
  end subroutine pr_AssignAveragedFaceFlowArray

!------------------------------------------
  subroutine pr_FillSubCellDataBuffer(this, subCellData, subRow, subColumn, backwardTracking)
  implicit none 
  class(ModpathCellDataType) :: this
  type(ModpathSubCellDataType),intent(inout) :: subCellData
  integer,intent(in) :: subRow,subColumn
  logical,intent(in) :: backwardTracking
  doubleprecision,dimension(6) :: flows
  doubleprecision :: sign,xinc,yinc
  integer :: n,rowcolumn,count
  
  ! Reset the data in subCellData (Not strictly necessary, but useful for debugging purposes)
  call subCellData%Reset()
  
  call this%FillSubCellFaceFlowsBuffer(subRow, subColumn, flows)
  
  subCellData%DX = this%DX / dble(this%SubCellColumnCount)
  subCellData%DY = this%DY / dble(this%SubCellRowCount)
  subCellData%DZ = this%GetDZ()
  
  sign = 1.0d0
  if(backwardTracking) sign = -sign
  subCellData%VX1 = sign * flows(1) / subCellData%DY /subCellData%DZ / this%Porosity / this%Retardation
  subCellData%VX2 = sign * flows(2) / subCellData%DY /subCellData%DZ / this%Porosity / this%Retardation
  subCellData%VY1 = sign * flows(3) / subCellData%DX /subCellData%DZ / this%Porosity / this%Retardation
  subCellData%VY2 = sign * flows(4) / subCellData%DX /subCellData%DZ / this%Porosity / this%Retardation
  subCellData%VZ1 = sign * flows(5) / subCellData%DX /subCellData%DY / this%Porosity / this%Retardation
  subCellData%VZ2 = sign * flows(6) / subCellData%DX /subCellData%DY / this%Porosity / this%Retardation
  
  subCellData%Row = subRow
  subCellData%Column = subColumn
  
  xinc = 1.0d0 / dble(this%SubCellColumnCount)
  subCellData%OffsetX(1) = (subColumn - 1) * xinc
  subCellData%OffsetX(2) = 1.0d0
  if(subColumn .lt. this%SubCellColumnCount) then
    subCellData%OffsetX(2) = subColumn * xinc
  end if
  
  yinc = 1.0d0 / dble(this%SubCellRowCount)
  subCellData%OffsetY(1) = (this%SubCellRowCount - subRow) * yinc
  subCellData%OffsetY(2) = 1.0d0
  if(subRow .gt. 1.0d0) then
    subCellData%OffsetY(2) = (this%SubCellRowCount - subRow + 1) * yinc
  end if
  
  subCellData%OffsetZ(1) = 0d0
  subCellData%OffsetZ(2) = 1.0d0
  
  ! Assign the connections for the 6 faces.
  ! All internal connections are set to -1.
  ! Boundary cells are set to the node number of the neighbor cell.
  ! Boundary faces that do not have adjacent neighbors are set to 0.
  
  ! Start by initializing all face connections to -1. 
  do n = 1, 6
    subCellData%Connection(n) = -1
  end do
  
  ! Assign the actual connection values to all of the faces that are not internal faces.
  if(subCellData%Row .eq. 1) then
    count = this%GetSubFaceCount(4)
    subCellData%Connection(4) = 0
    if(count .eq. 1) then
      subCellData%Connection(4) = this%SubFaceConn4(1)
    else if(count .gt. 1) then
      subCellData%Connection(4) = this%SubFaceConn4(subCellData%Column)
    end if
  end if
  
  if(subCellData%Row .eq. this%SubCellRowCount) then
    count = this%GetSubFaceCount(3)
    subCellData%Connection(3) = 0
    if(count .eq. 1) then
      subCellData%Connection(3) = this%SubFaceConn3(1)
    else if(count .gt. 1) then
      subCellData%Connection(3) = this%SubFaceConn3(subCellData%Column)
    end if
  end if
  
  if(subCellData%Column .eq. 1) then
    count = this%GetSubFaceCount(1)
    subCellData%Connection(1) = 0
    if(count .eq. 1) then
      subCellData%Connection(1) = this%SubFaceConn1(1)
    else if(count .gt. 1) then
      subCellData%Connection(1) = this%SubFaceConn1(subCellData%Row)
    end if
  end if
  
  if(subCellData%Column .eq. this%SubCellColumnCount) then
    count = this%GetSubFaceCount(2)
    subCellData%Connection(2) = 0
    if(count .eq. 1) then
      subCellData%Connection(2) = this%SubFaceConn2(1)
    else if(count .gt. 1) then
      subCellData%Connection(2) = this%SubFaceConn2(subCellData%Row)
    end if
  end if
  
  rowcolumn = this%SubCellRowCount * this%SubCellColumnCount
  if(this%GetSubFaceCount(5) .eq. 1) then
    subCellData%Connection(5) = this%SubFaceConn5(1)
  else if(this%GetSubFaceCount(5) .eq. rowcolumn) then
    n = ((subCellData%Row - 1) * this%SubCellColumnCount) + subCellData%Column
    subCellData%Connection(5) = this%SubFaceConn5(n)
  end if
  
  if(this%GetSubFaceCount(6) .eq. 1) then
    subCellData%Connection(6) = this%SubFaceConn6(1)
  else if(this%GetSubFaceCount(6) .eq. rowcolumn) then
    n = ((subCellData%Row - 1) * this%SubCellColumnCount) + subCellData%Column
    subCellData%Connection(6) = this%SubFaceConn6(n)
  end if
  
  end subroutine pr_FillSubCellDataBuffer

!------------------------------------------
  function pr_GetSubCellData(this, subRow, subColumn, backwardTracking) result(subCellData)
  implicit none 
  class(ModpathCellDataType) :: this
  integer,intent(in) :: subRow,subColumn
  logical,intent(in) :: backwardTracking
  type(ModpathSubCellDataType) :: subCellData
  
  call this%FillSubCellDataBuffer(subCellData, subRow, subColumn, backwardTracking)

  end function pr_GetSubCellData
  
!------------------------------------------
  subroutine pr_FillSubCellFaceFlowsBuffer(this, subRow, subColumn, faceFlows)
  implicit none 
  class(ModpathCellDataType) :: this
  integer,intent(in) :: subRow,subColumn
  doubleprecision,dimension(6) :: faceFlows
  integer :: subCellNumber
  
  if(this%GetSubCellCount() .eq. 1) then
      faceFlows(1) = this%Q1(1)
      faceFlows(2) = this%Q2(1)
      faceFlows(3) = this%Q3(1)
      faceFlows(4) = this%Q4(1)
      faceFlows(5) = this%Q5(1)
      faceFlows(6) = this%Q6(1)
  else
      subCellNumber = (subRow - 1) * this%GetSubCellColumnCount() + subColumn
      select case (subCellNumber)
        case (1)
          faceFlows(1) = this%GetSubCellBoundaryFlow(1, 1)
          faceFlows(2) = this%SubCellFlows(1)
          faceFlows(3) = this%SubCellFlows(3)
          faceFlows(4) = this%GetSubCellBoundaryFlow(4, 1)
          faceFlows(5) = this%GetSubCellBoundaryFlow(5, 1)
          faceFlows(6) = this%GetSubCellBoundaryFlow(6, 1)
        case (2)
          faceFlows(1) = this%SubCellFlows(1)
          faceFlows(2) = this%GetSubCellBoundaryFlow(2, 1)
          faceFlows(3) = this%SubCellFlows(4)
          faceFlows(4) = this%GetSubCellBoundaryFlow(4, 2)
          faceFlows(5) = this%GetSubCellBoundaryFlow(5, 2)
          faceFlows(6) = this%GetSubCellBoundaryFlow(6, 2)
        case (3)
          faceFlows(1) = this%GetSubCellBoundaryFlow(1, 2)
          faceFlows(2) = this%SubCellFlows(2)
          faceFlows(3) = this%GetSubCellBoundaryFlow(3, 1)
          faceFlows(4) = this%SubCellFlows(3)
          faceFlows(5) = this%GetSubCellBoundaryFlow(5, 3)
          faceFlows(6) = this%GetSubCellBoundaryFlow(6, 3)
        case (4)
          faceFlows(1) = this%SubCellFlows(2)
          faceFlows(2) = this%GetSubCellBoundaryFlow(2, 2)
          faceFlows(3) = this%GetSubCellBoundaryFlow(3, 2)
          faceFlows(4) = this%SubCellFlows(4)
          faceFlows(5) = this%GetSubCellBoundaryFlow(5, 4)
          faceFlows(6) = this%GetSubCellBoundaryFlow(6, 4)
        case default
          ! for now, do nothing.
      end select
  end if
  
  end subroutine pr_FillSubCellFaceFlowsBuffer

  
end module ModpathCellDataModule
