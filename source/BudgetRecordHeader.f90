module BudgetRecordHeaderModule
  use UtilMiscModule,only : TrimAll
  implicit none
  
! Set default access status to private
  private

! Private data type declarations

  
! Public derived data type definitions
!--------------------------------------
! type: 
!--------------------------------------
  type,public :: BudgetRecordHeaderType
    integer :: PrecisionType = 0
    integer :: TimeStep = 0
    integer :: StressPeriod = 0
    integer :: Method = 0
    integer(kind=8) :: HeaderPosition = -1
    integer(kind=8) :: HeaderOffset = -1
    integer(kind=8) :: DataOffset = -1
    character(len=16) :: TextLabel = '                '
    character(len=16) :: TXT1ID1 = '                '
    character(len=16) :: TXT2ID1 = '                '
    character(len=16) :: TXT1ID2 = '                '
    character(len=16) :: TXT2ID2 = '                '
    integer :: ArrayBufferSize
    integer :: ColumnCount = 0
    integer :: RowCount = 0
    integer :: LayerCount = 0
    doubleprecision :: TimeStepLength = 0.0d0
    doubleprecision :: StressPeriodLength = 0.0d0
    doubleprecision :: TotalTime = 0.0d0
    integer :: ListItemValueCount = 0
    integer :: ListItemCount = 0
    integer :: ArrayItemCount = 0
    character(len=16),allocatable,dimension(:) :: AuxiliaryNames
  contains
    procedure :: Initialize=>pr_Initialize
    procedure :: GetAuxiliaryNamesCount=>pr_GetAuxiliaryNamesCount
    procedure :: FindAuxiliaryNameIndex=>pr_FindAuxiliaryNameIndex
    procedure :: GetNextHeaderPosition=>pr_GetNextHeaderPosition
  end type

contains

!-------------------------------------------------------------------
  function pr_FindAuxiliaryNameIndex(this, name) result(index)
  use utl7module,only : upcase
  implicit none
  class(BudgetRecordHeaderType) :: this
  integer :: index,n,auxFirst,auxLast,auxTrimmedLength,nameFirst,nameLast,nameTrimmedLength, labelLength, auxNameLength
  character*(*) name
  character(len=16) :: label, auxName
  
  index = -1
  if(.not. allocated(this%AuxiliaryNames)) return
  
  label = adjustl(name)
  labelLength = len_trim(label)
  call upcase(label)
  
  do n = 1, this%GetAuxiliaryNamesCount()
      auxName = adjustl(this%AuxiliaryNames(n))
      auxNameLength = len_trim(auxName)
      call upcase(auxName)
      if(label(1:labelLength) .eq. auxName(1:auxNameLength)) then
          index = n
          return
      end if
  end do
  
  end function pr_FindAuxiliaryNameIndex

!-------------------------------------------------------------------
  function pr_GetNextHeaderPosition(this) result(position)
  implicit none
  class(BudgetRecordHeaderType) :: this
  integer(kind=8) :: position
  
  position = -1
  if(this%HeaderPosition .gt. 0) then
      position = this%HeaderPosition + this%HeaderOffset + this%DataOffset
  end if
  
  end function pr_GetNextHeaderPosition
  
!-------------------------------------------------------------------
  subroutine pr_Initialize(this)
  implicit none
  class(BudgetRecordHeaderType) :: this
  integer :: n
  
  this%PrecisionType = 0
  this%TimeStep = 0
  this%StressPeriod = 0
  this%Method = 0
  this%HeaderPosition = -1
  this%HeaderOffset = -1
  this%DataOffset = -1
  this%ColumnCount = 0
  this%RowCount = 0
  this%LayerCount = 0
  this%TimeStepLength = 0.0d0
  this%StressPeriodLength = 0.0d0
  this%TotalTime = 0.0d0
  this%ListItemValueCount = 0
  this%ListItemCount = 0
  this%ArrayItemCount = 0
  this%TextLabel = '                '
  this%TXT1ID1 = '                '
  this%TXT1ID2 = '                '
  this%TXT2ID1 = '                '
  this%TXT2ID2 = '                '
  if(allocated(this%AuxiliaryNames)) deallocate(this%AuxiliaryNames)
  allocate(this%AuxiliaryNames(0))
  
  end subroutine pr_Initialize

!-------------------------------------------------------------------
  function pr_GetAuxiliaryNamesCount(this) result(count)
  implicit none
  class(BudgetRecordHeaderType) :: this
  integer :: count

  count = 0
  if(allocated(this%AuxiliaryNames)) then
      count = size(this%AuxiliaryNames)
  end if
  
  end function pr_GetAuxiliaryNamesCount
  
end module BudgetRecordHeaderModule