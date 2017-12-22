module RectangularGridDisModule
  use UTL8MODULE
  use ModflowRectangularGridModule
  use GridLocationModule,only : GridLocationType
  
  implicit none
  
! Set default access status to private
  private
    
! Public derived data type definitions
!--------------------------------------
! type: 
!--------------------------------------
  type, public, extends(ModflowRectangularGridType) :: RectangularGridDisType
    integer :: LayerCellCount
    logical :: HasClippedCells = .false.
  contains
    procedure :: GetLayerRowColumn
    procedure :: ConvertFromNeighbor
    procedure :: ComputeFaceAssignments
    procedure, private :: RectangularGridDisInit1
    procedure, private :: ComputeFaceConnection
    generic :: ReadData => RectangularGridDisInit1
  end type

    contains
!---------------------------------------------------------------------
!   Overriden procedures 
!-------------------------------------------------------------------------
  function GetGridType(this) result(gridType)
  implicit none
  class(RectangularGridDisType) :: this
  integer :: gridType
  
  gridType = 1
  
  end function GetGridType

  subroutine ConvertFromNeighbor(this, toCellNumber, fromCellNumber,         &
    fromLocalX, fromLocalY, fromLocalZ, newLocation)
  implicit none
  class(RectangularGridDisType) :: this
  integer,intent(in) :: toCellNumber,fromCellNumber
  doubleprecision,intent(in) :: fromLocalX,fromLocalY,fromLocalZ
  class(GridLocationType),intent(inout) :: newLocation
  integer :: status,faceNumber,subFaceNumber
  doubleprecision :: localX,localY,localZ,globalX,globalY,tol
  
  call newLocation%Reset()
  status = 0
  
  ! Check to make sure the cell connection specified by fromCellNumber is actually a connection for
  ! the cell specified by toCellNumber
  call this%FindConnectionPosition(toCellNumber, fromCellNumber, faceNumber, subFaceNumber)
  
  select case (faceNumber)
    case (0)
      status = 1
    case (1)
      if(fromLocalX .ne. 1.0d0) status = 2
    case (2)
      if(fromLocalX .ne. 0d0) status = 2
    case (3)
      if(fromLocalY .ne. 1.0d0) status = 2
    case (4)
      if(fromLocalY .ne. 0d0) status = 2
    case (5)
      if(fromLocalZ .ne. 1.0d0) status = 2
    case (6)
      if(fromLocalZ .ne. 0d0) status = 2
    case default
      status = 3
  end select
  
  if(status .gt. 0) return
  
  newLocation%CellNumber = toCellNumber
  newLocation%LocalX = fromLocalX
  newLocation%LocalY = fromLocalY
  newLocation%LocalZ = fromLocalZ
  select case (faceNumber)
      case (1)
        newLocation%LocalX = 0.0d0
      case (2)
        newLocation%LocalX = 1.0d0
      case (3)
        newLocation%LocalY = 0.0d0
      case (4)
        newLocation%LocalY = 1.0d0
      case (5)
        newLocation%LocalZ = 0.0d0
      case (6)
        newLocation%LocalZ = 1.0d0
  end select
  
  newLocation%Layer = this%GetLayer(newLocation%CellNumber)
  
  end subroutine ConvertFromNeighbor

  subroutine ComputeFaceAssignments(this)
  class(RectangularGridDisType) :: this
  integer :: n, i, m, offset, count, conn
  integer, dimension(6) :: faces, connCodes

! Initialize temporary arrays
  faces(1) = 6
  connCodes(1) = 60101
  faces(2) = 4
  connCodes(2) = 40101
  faces(3) = 1
  connCodes(3) = 10101
  faces(4) = 2
  connCodes(4) = 20101
  faces(5) = 3
  connCodes(5) = 30101
  faces(6) = 5
  connCodes(6) = 50101

! Allocate and compute JaOffsets array
  if(allocated(this%JaOffsets)) deallocate(this%JaOffsets)
  allocate(this%JaOffsets(this%CellCount + 1))
  this%JaOffsets(1) = 0
  do n = 1, this%CellCount
      count = 1
      do m = 1, 6
          conn = this%ComputeFaceConnection(n, faces(m))
          if(conn .gt. 0) then
              count = count + 1
          end if
      end do
      this%JaOffsets(n + 1) = this%JaOffsets(n) + count
  end do

! Allocate and compute Ja and JaFace arrays
  this%JaCount = this%JaOffsets(this%CellCount + 1)
  if(allocated(this%Ja)) deallocate(this%Ja)
  if(allocated(this%JaFace)) deallocate(this%JaFace)
  allocate(this%Ja(this%JaCount))
  allocate(this%JaFace(this%JaCount))
  do n = 1, this%CellCount
      count = 1
      offset = this%JaOffsets(n)
      this%Ja(offset + count) = n
      this%JaFace(offset + count) = 0
      do m = 1, 6
          conn = this%ComputeFaceConnection(n, faces(m))
          if(conn .gt. 0) then
              count = count + 1
              this%Ja(offset + count) = conn
              this%JaFace(offset + count) = connCodes(m)
          end if
      end do
  end do
  
  end subroutine ComputeFaceAssignments

!------------------------------------------------------------------------
!  New public procedures
!------------------------------------------------------------------------
  subroutine GetLayerRowColumn(this, cellNumber, layer, row, column)
    class(RectangularGridDisType) :: this
    integer, intent(in) :: cellNumber
    integer, intent(inout) :: layer, row, column
    integer :: layerCellNumber, n, rowOffset
    
    do n = 1, this%LayerCount
        if(cellNumber .le. this%LayerOffsets(n + 1)) then
            layer = n
            exit
        end if
    end do
  
    layerCellNumber = cellNumber - (layer - 1)*this%LayerCellCount
    rowOffset = 0
    do n = 1, this%RowCount
        rowOffset = rowOffset + this%ColumnCount
        if(layerCellNumber .le. rowOffset) then
            row = n
            exit
        end if
    end do
    
    column = layerCellNumber - (row - 1)*this%ColumnCount
    
  end subroutine GetLayerRowColumn

!----------------------------------------------------------------------
!  Private methods
!----------------------------------------------------------------------
  subroutine RectangularGridDisInit1(this, iin, gridMetaUnit, iout, stressPeriodCount)
    implicit none
    class(RectangularGridDisType) :: this
    integer, intent(in) :: iin, iout, gridMetaUnit
    integer, intent(inout) :: stressPeriodCount
    logical :: isOpen
    
! Read the MODFLOW-2005 DIS file
  call ReadDIS(this, iin, iout, stressPeriodCount)
  
! Read the GridMeta file
  inquire(unit=gridMetaUnit, opened=isOpen)
  if(isOpen) call ReadGridMeta(this, gridMetaUnit, iout)
    
  end subroutine RectangularGridDisInit1

  subroutine ReadDIS(grid, iin, iout, stressPeriodCount)
  implicit none
  class(RectangularGridDisType) :: grid
  integer,intent(in) :: iin, iout  
  integer,intent(inout) :: stressPeriodCount
    character*20 :: controlrecordflag
    character (len=24) :: aname
    character (len=200) :: line
    integer :: cellNumber,i,j,k,n,m,layer,row,column,level,maxlevel,conn,nc,ptr,count,offset,istart,istop,ierr,lloc
    integer :: itmuni, lenuni
    integer,dimension(:),allocatable :: laycbd
    doubleprecision :: r
    doubleprecision,dimension(:,:),allocatable :: bufdbl2d
    double precision,dimension(:),allocatable :: X
    double precision,dimension(:),allocatable :: Y
    double precision,dimension(:),allocatable :: DX
    double precision,dimension(:),allocatable :: DY
  
!   Deallocate arrays if they have been allocated previously
    call grid%Reset()    
    
!   Set grid type    
    grid%GridType = 1
    
!   Read and write comment lines. Return the first non-comment line in variable "line".
    call u8rdcom(iin,iout,line,ierr)
    lloc = 1
    call urword(line, lloc, istart, istop, 2, grid%LayerCount, r, iout, iin)
    call urword(line, lloc, istart, istop, 2, grid%RowCount, r, iout, iin)
    call urword(line, lloc, istart, istop, 2, grid%ColumnCount, r, iout, iin)
    call urword(line, lloc, istart, istop, 2, stressPeriodCount, r, iout, iin)
    call urword(line, lloc, istart, istop, 2, itmuni, r, iout, iin)
    call urword(line, lloc, istart, istop, 2, lenuni, r, iout, iin)
  
!   Compute the number of cells
    grid%CellCount = grid%LayerCount * grid%RowCount * grid%ColumnCount
    grid%LayerCellCount = grid%RowCount * grid%ColumnCount

!   Allocate and read confining bed layer flag array.
    allocate(laycbd(grid%LayerCount))
    read(iin, *) (laycbd(n), n = 1, grid%LayerCount)
    
!   Check to be sure there are no quasi-3d confining beds    
    do n = 1, grid%LayerCount
        if(laycbd(n) .ne. 0) then
            call ustop('Quasi-3d confining beds are not allowed. Stop.')
        end if
    end do
    
!   Allocate data pointers for arrays that depend only on grid dimensions
    allocate(DX(grid%ColumnCount))
    allocate(DY(grid%RowCount))
    allocate(X(grid%ColumnCount + 1))
    allocate(Y(grid%RowCount + 1))
    allocate(grid%Top(grid%CellCount))
    allocate(grid%CellX(grid%CellCount))
    allocate(grid%CellY(grid%CellCount))
    allocate(grid%DelX(grid%CellCount))
    allocate(grid%DelY(grid%CellCount))
    allocate(grid%CellType(grid%CellCount))
    allocate(grid%SaturatedTop(grid%CellCount))
    allocate(grid%Bottom(grid%CellCount))
    allocate(grid%LayerOffsets(grid%LayerCount + 1))
    
!   Compute and fill the layer and row offset arrays
    grid%LayerOffsets(1) = 0
    do n = 1, grid%LayerCount 
       grid%LayerOffsets(n + 1)  = grid%LayerOffsets(n) + grid%LayerCellCount
    end do
    
!   Set the default values of grid origin and rotation angle
    grid%OriginX = 0.0
    grid%OriginY = 0.0
    grid%RotationAngle = 0.0
    
!   Read X grid spacing (DELR, along a row) and compute X face coordinates
    aname = "DX"
    call u1ddbl(DX,aname,grid%ColumnCount,iin,iout)
    X(1) = 0.0
    do n = 1, grid%ColumnCount
        X(n + 1) = X(n) + DX(n)
    end do
    
!   Read Y grid spacing (DELC, along a column) and compute Y face coordinates
    aname = "DY"
    call u1ddbl(DY,aname,grid%RowCount,iin,iout) 
    
    Y(1) = 0.0
    do n = 1, grid%RowCount
        Y(1) = Y(1) + DY(n)
    end do
    do n = 1, grid%RowCount
        Y(n + 1) = Y(n) - DY(n)
    end do

!   Initialize CellType to 0 for all cells. This will be changed later when LAYTYP data is read
!   from the MPBAS file.
    do n = 1, grid%CellCount
        grid%CellType(n) = 0
    end do
    
!   Allocate double precision 2d buffer
    allocate(bufdbl2d(grid%ColumnCount, grid%RowCount))
    
!   Read top elevation for layer 1
    aname = "TOP ELEVATION"
    call u2drel(bufdbl2d, aname, grid%RowCount, grid%ColumnCount, 1, iin, iout)
    cellNumber = 0
    do row = 1, grid%RowCount
        do column = 1, grid%ColumnCount
            cellNumber = cellNumber + 1
            grid%Top(cellNumber) = bufdbl2d(column, row)
            grid%SaturatedTop(cellNumber) = grid%Top(cellNumber)
        end do
    end do
    
!   Read bottom elevation for all layers
    cellNumber = 0
    aname = "BOTTOM ELEVATION"
    do n = 1, grid%LayerCount
        call u2drel(bufdbl2d, aname, grid%RowCount, grid%ColumnCount, n, iin, iout)
        do row = 1, grid%RowCount
            do column = 1, grid%ColumnCount
                cellNumber = cellNumber + 1
                grid%Bottom(cellNumber) = bufdbl2d(column, row)
            end do
        end do
    end do
    
!   Set top elevation for the rest of the layers
    do n = 1, grid%CellCount - grid%LayerCellCount
        grid%Top(n + grid%LayerCellCount) = grid%Bottom(n)
        grid%SaturatedTop(n + grid%LayerCellCount) = grid%Top(n + grid%LayerCellCount)
    end do

!   Compute CellX, CellY, DelX, and DelY
    n = 0
    do layer = 1, grid%LayerCount
        do row = 1, grid%RowCount
            do column = 1, grid%ColumnCount
                n = n + 1
                grid%DelX(n) = DX(column)
                grid%DelY(n) = DY(row)
                grid%CellX(n) = (X(column) + X(column + 1)) / 2.0
                grid%CellY(n) = (Y(row) + Y(row + 1)) / 2.0
            end do
        end do
    end do

!   Create connection arrays and face assignment array
    call grid%ComputeFaceAssignments()
    
    ! Deallocate local arrays
    deallocate(X)
    deallocate(Y)
    deallocate(DX)
    deallocate(DY)
    
  
  end subroutine ReadDIS

  function ComputeFaceConnection(this, cellNumber, faceNumber) result(conn)
  implicit none
  class(RectangularGridDisType) :: this
  integer,intent(in) :: cellNumber, faceNumber
  integer :: conn, layer, row, column
  
  conn = 0
  call this%GetLayerRowColumn(cellNumber, layer, row, column)
  
  if(faceNumber .eq. 1) then
      if(column .gt. 1) conn = cellNumber - 1
  else if(faceNumber .eq. 2) then
      if(column .lt. this%ColumnCount) conn = cellNumber + 1
  else if(faceNumber .eq. 3) then
      if(row .lt. this%RowCount) conn = cellNumber + this%ColumnCount
  else if(faceNumber .eq. 4) then
      if(row .gt. 1) conn = cellNumber - this%ColumnCount
  else if(faceNumber .eq. 5) then
      if(layer .lt. this%LayerCount) conn = cellNumber + (this%RowCount * this%ColumnCount)
  else if(faceNumber .eq. 6) then
      if(layer .gt. 1) conn = cellNumber - (this%RowCount * this%ColumnCount)
  end if
  
  end function ComputeFaceConnection

  subroutine ReadGridMeta(grid, iin, iout)
  use utl7module,only : upcase
  use UTL8MODULE,only : u8rdcom, urword, ustop
  implicit none
  class(RectangularGridDisType),intent(inout) :: grid
  integer,intent(in) :: iin, iout
  integer :: n, ierr, lloc, nval, istart, istop, ncode
  integer,dimension(:),allocatable :: cellsPerLayer
  double precision :: rval
  logical :: isfound
  character (len=132) :: line
  
  call u8rdcom(iin, iout, line, ierr)
  lloc = 1
  call urword(line, lloc, istart, istop, 1, nval, grid%OriginX, 0, 0)
  call urword(line, lloc, istart, istop, 1, nval, grid%OriginY, 0, 0)
  call urword(line, lloc, istart, istop, 1, nval, grid%RotationAngle, 0, 0)

  end subroutine ReadGridMeta
  
  subroutine ReadGridMeta_block_form(grid, iin, iout)
  use utl7module,only : upcase
  use UTL8MODULE,only : uget_block, uterminate_block, u8rdcom, urword, ustop
  implicit none
  class(RectangularGridDisType),intent(inout) :: grid
  integer,intent(in) :: iin, iout
  integer :: n, ierr, lloc, nval, istart, istop, ncode
  double precision :: rval
  logical :: isfound
  character (len=132) :: line
  
  ! Read OPTIONS block
  call uget_block(iin, iout, 'OPTIONS', ierr, isfound, lloc, line)
  if(isfound) then
      grid%OriginX = 0.0
      grid%OriginY = 0.0
      grid%RotationAngle = 0.0
      do
          call u8rdcom(iin, iout, line, ierr)
          lloc = 1
          call urword(line, lloc, istart, istop, 1, nval, rval, 0, 0)
          select case(line(istart:istop))
              case('XORIGIN')
                  call urword(line, lloc, istart, istop, 3, nval, grid%OriginX, 0, 0)
              case('YORIGIN')
                   call urword(line, lloc, istart, istop, 3, nval, grid%OriginY, 0, 0)
              case('ANGROT')
                   call urword(line, lloc, istart, istop, 3, nval, grid%RotationAngle, 0, 0)
                  ! Ignore the time units parameter for now.
              case('END','BEGIN')
                  call uterminate_block(iin, iout, line(istart:istop), 'OPTIONS', lloc, line, ierr)
                  if(ierr .eq. 0) exit
              case default
                call ustop('OPTIONS block not found in GRIDMETA file for grid type DIS. Stop.')
          end select        
      end do
  else
      call ustop('OPTIONS block not found in GRIDMETA file for grid type DIS. Stop.')
  end if
  
  end subroutine ReadGridMeta_block_form
   
end module RectangularGridDisModule