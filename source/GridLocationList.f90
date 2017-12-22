module  GridLocationListModule
  use GridLocationModule,only : GridLocationType
  implicit none
  
! Set default access status to private
  private

! Private data type declarations

  
! Public derived data type definitions

!--------------------------------------
! type: 
!--------------------------------------
  type,public ::  GridLocationListType
    ! private data
    type(GridLocationType),allocatable,dimension(:) :: Items
    integer,private :: Count = 0
    integer :: ExpansionIncrement = 500
  contains
    procedure :: AddItem=> GridLocationListType_AddItem
    procedure :: AddItems=> GridLocationListType_AddItems
    procedure :: GetItemCount=> GridLocationListType_GetItemCount
    procedure :: GetItem=> GridLocationListType_GetItem
    procedure :: Clear=> GridLocationListType_Clear
    procedure :: ClearAndReset=> GridLocationListType_ClearAndReset
    procedure :: AssociateItem=> GridLocationListType_AssociateItem
    procedure :: GetItemArray=> GridLocationListType_GetItemArray
    !private procedures
    procedure,private :: ReSizeArray=> GridLocationListType_ReSizeArray
  end type
  
contains

!------------------------------------------
! Method: 
!------------------------------------------
  subroutine  GridLocationListType_Clear(this)
  implicit none
  class( GridLocationListType) :: this
  
  this%Count = 0
  
  end subroutine  GridLocationListType_Clear
  
!------------------------------------------
! Method: 
!------------------------------------------
  subroutine  GridLocationListType_AssociateItem(this,itemPointer,index)
  implicit none
  class( GridLocationListType),target :: this
  type(GridLocationType),pointer,intent(inout) :: itemPointer
  integer,intent(in) :: index
  
  itemPointer => this%Items(index)
  
  end subroutine  GridLocationListType_AssociateItem
  
!------------------------------------------
! Method: 
!------------------------------------------
  subroutine  GridLocationListType_ClearAndReset(this)
  implicit none
  class( GridLocationListType) :: this
  
  this%Count = 0
  if(allocated(this%Items)) deallocate(this%Items)
  allocate(this%Items(this%ExpansionIncrement))  
  
  end subroutine  GridLocationListType_ClearAndReset
  
!------------------------------------------
! Method: 
!------------------------------------------
  function  GridLocationListType_GetItemCount(this) result(count)
  implicit none
  class( GridLocationListType) :: this
  integer :: count
  
  if(allocated(this%Items)) then
    count = this%Count
  else
    allocate(this%Items(this%ExpansionIncrement))
    count = 0
  end if
  
  end function  GridLocationListType_GetItemCount
  
!------------------------------------------
! Method: 
!------------------------------------------
  subroutine  GridLocationListType_AddItem(this,item)
  implicit none
  class( GridLocationListType) :: this
  type(GridLocationType),intent(in) :: item
  integer :: count
  
  if(.not. allocated(this%Items)) then
    allocate(this%Items(this%ExpansionIncrement))
    this%Count = 0
  end if
  
  count = this%Count + 1
  call this%ReSizeArray(count)
  this%Count = count
  this%Items(this%Count) = item
  
  end subroutine  GridLocationListType_AddItem
  
!------------------------------------------
! Method: 
!------------------------------------------
  subroutine  GridLocationListType_AddItems(this,items)
  implicit none
  class( GridLocationListType) :: this
  type(GridLocationType),dimension(:),intent(in) :: items
  integer :: count,sizeItems,n
  
  if(.not. allocated(this%Items)) then
    allocate(this%Items(this%ExpansionIncrement))
    this%Count = 0
  end if
  
  sizeItems = size(items)
  if(sizeItems .gt. 0) then
      count = this%Count + sizeItems
      call this%ReSizeArray(count)
      do n = 1,sizeItems
          this%Count = this%Count + 1
          this%Items(this%Count) = items(n)
      end do
  end if
  
  end subroutine  GridLocationListType_AddItems
  
!------------------------------------------
! Method: 
!------------------------------------------
  function  GridLocationListType_GetItem(this,index) result(item)
  implicit none
  class( GridLocationListType) :: this
  integer,intent(in) :: index
  type(GridLocationType) :: item
  
  if(allocated(this%Items)) then
    item = this%Items(index)
  else
    allocate(this%Items(this%ExpansionIncrement))
  end if
  
  end function  GridLocationListType_GetItem
  
!------------------------------------------
! Method: 
!------------------------------------------
  function  GridLocationListType_GetItemArray(this) result(itemArray)
  implicit none
  class( GridLocationListType) :: this
  type(GridLocationType),allocatable,dimension(:) :: itemArray
  integer :: n
  
  allocate(itemArray(this%Count))
  do n = 1, this%Count
    itemArray(n) = this%Items(n)
  end do
  
  end function  GridLocationListType_GetItemArray
  
!------------------------------------------
! Method: 
!------------------------------------------
  subroutine  GridLocationListType_ReSizeArray(this,count)
  implicit none
  class( GridLocationListType) :: this
  integer,intent(in) :: count
  type(GridLocationType),allocatable,dimension(:) :: temp
  integer :: arraySize,n
  
  if(.not. allocated(this%Items)) return
  arraySize = size(this%Items)
  if(count .lt. arraySize) return
  allocate(temp(arraySize))
  
  do n = 1, arraySize
    temp(n) = this%Items(n)
  end do
  
  deallocate(this%Items)
  allocate(this%Items(count + this%ExpansionIncrement))
  
  do n = 1, this%Count
    this%Items(n) = temp(n)
  end do
  
  deallocate(temp)
  
  end subroutine  GridLocationListType_ReSizeArray



end module  GridLocationListModule