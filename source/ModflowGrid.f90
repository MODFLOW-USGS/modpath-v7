module ModflowGridModule
  use UTL8MODULE
  
  implicit none
  
! Set default access status to private
  private
    
! Public derived data type definitions
!--------------------------------------
! type: 
!--------------------------------------
  type,public :: ModflowGridType
    integer :: GridType = 0  
    integer :: CellCount = 0
    integer :: LayerCount = 0
    integer :: JaCount = 0
    integer,dimension(:),allocatable :: JaOffsets, Ja
    double precision,dimension(:),allocatable :: Top
    double precision,dimension(:),allocatable :: SaturatedTop
    double precision,dimension(:),allocatable :: Bottom
    double precision,dimension(:),allocatable :: CellX, CellY
    integer,dimension(:),allocatable :: CellType
    integer,dimension(:),allocatable :: LayerOffsets
    double precision :: OriginX, OriginY, RotationAngle
 contains
    procedure :: GetLayerCellCount
    procedure :: GetLayer
    procedure :: GetLayerCellNumber
    procedure :: GetJaCellConnections
    procedure :: GetJaCellConnectionsCount
    procedure :: GetDZ
    procedure :: GetIDomain
  end type

    contains
!x------------------------------------------
  function GetLayerCellCount(this, layer) result(fval)
  class(ModflowGridType) :: this
  integer,intent(in) :: layer
  double precision :: fval
  
  fval = this%LayerOffsets(layer + 1) - this%LayerOffsets(layer)
  
  end function GetLayerCellCount
!------------------------------------------
  function GetLayer(this, cellNumber) result(layer)
    implicit none
    class(ModflowGridType) :: this
    integer, intent(in) :: cellNumber
    integer :: layer, count, n, n1, n2
  
    do n = 1, this%LayerCount
        n1 = this%LayerOffsets(n)
        n2 = this%LayerOffsets(n + 1)
        if( (cellNumber .gt. n1) .and. (cellNumber .le. n2) ) then
            layer = n
            exit
        end if
    end do
  
  end function GetLayer  
!x------------------------------------------
  function GetLayerCellNumber(this, cellNumber) result(fval)
  class(ModflowGridType) :: this
  integer, intent(in) :: cellNumber
  integer :: layer
  double precision :: fval
  
  layer = this%GetLayer(cellNumber)
  fval = cellNumber - this%LayerOffsets(layer)
  
  end function GetLayerCellNumber
    
  subroutine GetJaCellConnections(this, cellNumber, buffer, bufferSize, cellConnectionCount)
  class(ModflowGridType) :: this
  integer,intent(in) :: cellNumber,bufferSize
  integer,intent(inout) :: cellConnectionCount
  integer,intent(inout),dimension(bufferSize) :: buffer
  integer :: n, offset
  
  do n = 1, bufferSize
      buffer(n) = -1
  end do
  
  offset = this%JaOffsets(cellNumber)
  cellConnectionCount = this%JaOffsets(cellNumber + 1) - offset
  
  do n = 1, cellConnectionCount
      buffer(n) = this%Ja(offset + n)
  end do
  
  end subroutine GetJaCellConnections
  
  function GetJaCellConnectionsCount(this, cellNumber) result(count)
  class(ModflowGridType) :: this
  integer,intent(in) :: cellNumber
  integer :: count
  
  count = this%JaOffsets(cellNumber + 1) - this%JaOffsets(cellNumber)
  
  end function GetJaCellConnectionsCount
  
  !x------------------------------------------
  function GetDZ(this, cellNumber) result(fval)
  class(ModflowGridType) :: this
  integer,intent(in) :: cellNumber
  double precision :: fval
  
  ! Add override code to implement
  fval = this%Top(cellNumber) - this%Bottom(cellNumber)
  
  end function GetDZ

  !x------------------------------------------
  function GetIDomain(this, cellNumber) result(fval)
    class(ModflowGridType) :: this
    integer,intent(in) :: cellNumber
    integer :: fval
    
    ! Override this function if necessary
    fval = 1
    
  end function GetIDomain

end module ModflowGridModule