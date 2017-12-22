module HeadRecordHeaderModule
  implicit none
  
! Set default access status to private
  private
  
! Public derived data type definitions
!--------------------------------------
! type: 
!--------------------------------------
  type,public :: HeadRecordHeaderType
    integer :: TimeStep
    integer :: StressPeriod
    doubleprecision :: StressPeriodLength
    doubleprecision :: TotalTime
    character(len=16) :: TextLabel = '                '
    integer :: ColumnCount
    integer :: RowCount
    integer :: FirstCellNumber
    integer :: LastCellNumber
    integer :: Layer
    integer :: PrecisionType
    integer(kind=8) :: HeaderPosition = -1
    integer(kind=8) :: HeaderOffset = -1
    integer(kind=8) :: DataOffset = -1
  contains
    procedure :: Initialize=>pr_Initialize
    procedure :: GetGridType=>pr_GetGridType
    procedure :: GetNextHeaderPosition=>pr_GetNextHeaderPosition
    procedure :: GetDataPosition=>pr_GetDataPosition
    
  
  end type


contains
  subroutine pr_Initialize(this)
  implicit none
  class(HeadRecordHeaderType) :: this
  
  this%TimeStep = 0
  this%StressPeriod = 0
  this%HeaderPosition = -1
  this%HeaderOffset = -1
  this%DataOffset = -1
  this%StressPeriodLength = 0.0d0
  this%TotalTime = 0.0d0
  this%TextLabel = '                '
  this%ColumnCount = 0
  this%RowCount = 0
  this%FirstCellNumber = 0
  this%LastCellNumber = 0
  this%Layer = 0
  this%PrecisionType = 0
  
  end subroutine pr_Initialize

!------------------------------------------------------------------
  function pr_GetDataPosition(this) result(position)
  implicit none
  class(HeadRecordHeaderType) :: this
  integer(kind=8) :: position
  
  position = -1
  if(this%HeaderPosition .gt. 0) then
      position = this%HeaderPosition + this%HeaderOffset
  end if
  
  end function pr_GetDataPosition
  
!-------------------------------------------------------------------
  function pr_GetNextHeaderPosition(this) result(position)
  implicit none
  class(HeadRecordHeaderType) :: this
  integer(kind=8) :: position
  
  position = -1
  if(this%HeaderPosition .gt. 0) then
      position = this%HeaderPosition + this%HeaderOffset + this%DataOffset
  end if
  
  end function pr_GetNextHeaderPosition

!-------------------------------------------------------------------
  function pr_GetGridType(this) result(gridType)
  implicit none
  class(HeadRecordHeaderType) :: this
  integer :: gridType
  
  gridType = 1
  if((this%ColumnCount .eq. 0) .and. (this%RowCount .eq. 0)) gridType = 2
  end function pr_GetGridType



end module HeadRecordHeaderModule