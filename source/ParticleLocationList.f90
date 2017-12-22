module ParticleLocationListModule
  use ParticleLocationModule,only : ParticleLocationType
  implicit none
  
! Set default access status to private
  private

! Private data type declarations

  
! Public derived data type definitions

!--------------------------------------
! type: 
!--------------------------------------
  type,public :: ParticleLocationListType
    ! private data
    type(ParticleLocationType),allocatable,dimension(:) :: Items
    integer,private :: Count = 0
    integer,private :: CurrentBufferSize = -1
    integer :: ExpansionIncrement = 500
    integer :: InitialBufferSize = 1000
  contains
    generic   :: Initialize=>pr_Initialize1,pr_Initialize2
    procedure :: AddItem=>pr_AddItem
    procedure :: AddItems=>pr_AddItems
    procedure :: GetItemCount=>pr_GetItemCount
    procedure :: GetItem=>pr_GetItem
    procedure :: Clear=>pr_Clear
    procedure :: ClearAndReset=>pr_ClearAndReset
    procedure :: AssociateItem=>pr_AssociateItem
    procedure :: GetItemArray=>pr_GetItemArray
    !private procedures
    procedure,private :: ReSizeArray=>pr_ReSizeArray
    procedure,private :: pr_Initialize1
    procedure,private :: pr_Initialize2
  end type
  
contains

!------------------------------------------
  subroutine pr_Initialize1(this)
  implicit none
  class(ParticleLocationListType) :: this
  integer :: initialBufferSize, expansionIncrement
  
  initialBufferSize = this%InitialBufferSize
  expansionIncrement = this%ExpansionIncrement
  call this%pr_Initialize2(initialBufferSize, expansionIncrement)
  
  end subroutine pr_Initialize1

!------------------------------------------
  subroutine pr_Initialize2(this, initialBufferSize, expansionIncrement)
  implicit none
  class(ParticleLocationListType) :: this
  integer :: initialBufferSize, expansionIncrement
  
  this%Count = 0
  this%InitialBufferSize = initialBufferSize
  this%ExpansionIncrement = expansionIncrement
  if(allocated(this%Items)) deallocate(this%Items)
  allocate(this%Items(this%InitialBufferSize))
  this%CurrentBufferSize = size(this%Items)
  
  end subroutine pr_Initialize2
  
!------------------------------------------
! Method: 
!------------------------------------------
  subroutine pr_Clear(this)
  implicit none
  class(ParticleLocationListType) :: this
  
  this%Count = 0
  
  end subroutine pr_Clear
  
!------------------------------------------
! Method: 
!------------------------------------------
  subroutine pr_AssociateItem(this,itemPointer,index)
  implicit none
  class(ParticleLocationListType),target :: this
  type(ParticleLocationType),pointer,intent(inout) :: itemPointer
  integer,intent(in) :: index
  
  itemPointer => this%Items(index)
  
  end subroutine pr_AssociateItem
  
!------------------------------------------
! Method: 
!------------------------------------------
  subroutine pr_ClearAndReset(this)
  implicit none
  class(ParticleLocationListType) :: this
  integer :: initialBufferSize,expansionIncrement
  
  initialBufferSize = this%InitialBufferSize
  expansionIncrement = this%ExpansionIncrement
  call this%Initialize(initialBufferSize, expansionIncrement)
  
  end subroutine pr_ClearAndReset
  
!------------------------------------------
! Method: 
!------------------------------------------
  function pr_GetItemCount(this) result(count)
  implicit none
  class(ParticleLocationListType) :: this
  integer :: count
  
  count = this%Count
  
  end function pr_GetItemCount
  
!------------------------------------------
! Method: 
!------------------------------------------
  subroutine pr_AddItem(this,item)
  implicit none
  class(ParticleLocationListType) :: this
  type(ParticleLocationType),intent(in) :: item
  integer :: count
  
  count = this%Count + 1
  if(count .gt. this%CurrentBufferSize) call this%ReSizeArray()
  this%Count = count
  this%Items(this%Count)%CellNumber = item%CellNumber
  this%Items(this%Count)%Layer = item%Layer
  this%Items(this%Count)%Row = item%Row
  this%Items(this%Count)%Column = item%Column
  this%Items(this%Count)%LocalX = item%LocalX
  this%Items(this%Count)%LocalY = item%LocalY
  this%Items(this%Count)%LocalZ = item%LocalZ
  this%Items(this%Count)%TrackingTime = item%TrackingTime
  
  end subroutine pr_AddItem
  
!------------------------------------------
! Method: 
!------------------------------------------
  subroutine pr_AddItems(this,items)
  implicit none
  class(ParticleLocationListType) :: this
  type(ParticleLocationType),dimension(:),intent(in) :: items
  integer :: count,sizeItems,n
  
  sizeItems = size(items)
  if(sizeItems .gt. 0) then
      do n = 1,sizeItems
          call this%AddItem(items(n))
      end do
  end if
  
  end subroutine pr_AddItems
  
!------------------------------------------
! Method: 
!------------------------------------------
  function pr_GetItem(this,index) result(item)
  implicit none
  class(ParticleLocationListType) :: this
  integer,intent(in) :: index
  type(ParticleLocationType) :: item
  
  item = this%Items(index)
  
  end function pr_GetItem
  
!------------------------------------------
! Method: 
!------------------------------------------
  function pr_GetItemArray(this) result(itemArray)
  implicit none
  class(ParticleLocationListType) :: this
  type(ParticleLocationType),allocatable,dimension(:) :: itemArray
  integer :: n
  
  allocate(itemArray(this%Count))
  do n = 1, this%Count
    itemArray(n) = this%Items(n)
  end do
  
  end function pr_GetItemArray
  
!------------------------------------------
! Method: 
!------------------------------------------
  subroutine pr_ReSizeArray(this)
  implicit none
  class(ParticleLocationListType) :: this
  type(ParticleLocationType),allocatable,dimension(:) :: temp
  integer :: n
  
  allocate(temp(this%CurrentBufferSize))
  
  do n = 1, this%CurrentBufferSize
    temp(n) = this%Items(n)
  end do
  
  if(allocated(this%Items)) deallocate(this%Items)
  allocate(this%Items(this%CurrentBufferSize + this%ExpansionIncrement))
  this%CurrentBufferSize = size(this%Items)
  
  do n = 1, this%Count
    this%Items(n)%CellNumber = temp(n)%CellNumber
    this%Items(n)%Layer = temp(n)%Layer
    this%Items(n)%Row = temp(n)%Row
    this%Items(n)%Column = temp(n)%Column
    this%Items(n)%LocalX = temp(n)%LocalX
    this%Items(n)%LocalY = temp(n)%LocalY
    this%Items(n)%LocalZ = temp(n)%LocalZ
    this%Items(n)%TrackingTime = temp(n)%TrackingTime
  end do
  
  deallocate(temp)
  
  
  end subroutine pr_ReSizeArray



end module ParticleLocationListModule