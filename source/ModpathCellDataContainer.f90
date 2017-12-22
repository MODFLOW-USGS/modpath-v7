module ModpathCellDataContainerModule
  use ModpathCellDataModule,only : ModpathCellDataType
  implicit none
  
! Set default access status to private
  private

! Private data type declarations

! Public derived data type definitions
!--------------------------------------
! type: 
!--------------------------------------
  type,public :: ModpathCellDataContainerType
      type(ModpathCellDataType),pointer :: Item => null()
  contains
      procedure :: AllocateNew=>ModpathCellDataContainerType_AllocateNew
      procedure :: AssociateItem=>ModpathCellDataContainerType_AssociateItem
      procedure :: DeallocateAndNullify=>ModpathCellDataContainerType_DeallocateAndNullify
  end type

contains
!-----------------------------------------------------------------
  subroutine ModpathCellDataContainerType_AllocateNew(this)
  implicit none
  class(ModpathCellDataContainerType) :: this
 
  if(associated(this%Item)) deallocate(this%Item)
  allocate(this%Item)
  
  end subroutine ModpathCellDataContainerType_AllocateNew

!-----------------------------------------------------------------
  subroutine ModpathCellDataContainerType_AssociateItem(this,item)
  implicit none
  class(ModpathCellDataContainerType) :: this
  type(ModpathCellDataType),target :: item
 
  if(associated(this%item)) deallocate(this%Item)
  this%Item => item
  
  end subroutine ModpathCellDataContainerType_AssociateItem

!-----------------------------------------------------------------
  subroutine ModpathCellDataContainerType_DeallocateAndNullify(this)
  implicit none
  class(ModpathCellDataContainerType) :: this
 
  if(associated(this%Item)) deallocate(this%Item)
  this%Item => null()
  
  end subroutine ModpathCellDataContainerType_DeallocateAndNullify
  
end module ModpathCellDataContainerModule