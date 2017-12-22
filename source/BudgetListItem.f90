module BudgetListItemModule
  implicit none
  
! Set default access status to private
  private
  
! Public derived data type definitions
!--------------------------------------
! type: 
!--------------------------------------
  type,public :: BudgetListItemType
    integer :: CellNumber
    integer :: AuxiliaryValueCount
    integer :: ID2
    doubleprecision :: BudgetValue
    doubleprecision,dimension(20) :: AuxiliaryValues
  contains
    procedure :: Initialize=>pr_Initialize
    procedure :: GetAuxiliaryValue=>pr_GetAuxiliaryValue
    procedure :: SetAuxiliaryValue=>pr_SetAuxiliaryValue
  end type

contains

!--------------------------------------
  subroutine pr_Initialize(this)
  implicit none
  class(BudgetListItemType) :: this
  integer :: n
  
  this%CellNumber = 0
  this%BudgetValue = 0.0d0
  this%AuxiliaryValueCount = 0
  this%ID2 = 0
  do n = 1, 20
      this%AuxiliaryValues(n) = 0.0d0
  end do
  
  end subroutine pr_Initialize

!--------------------------------------
  function pr_GetAuxiliaryValue(this, index) result(auxValue)
  implicit none
  class(BudgetListItemType) :: this
  integer,intent(in) :: index
  doubleprecision :: auxValue
  
  auxValue = 0.0d0
  if(index .le. this%AuxiliaryValueCount) auxValue = this%AuxiliaryValues(index)
  
  end function pr_GetAuxiliaryValue

!--------------------------------------
  subroutine pr_SetAuxiliaryValue(this, index, auxiliaryValue)
  implicit none
  class(BudgetListItemType) :: this
  integer,intent(in) :: index
  doubleprecision,intent(in) :: auxiliaryValue
  
  if(index .le. this%AuxiliaryValueCount) then
       this%AuxiliaryValues(index) = auxiliaryValue
  end if
  
  end subroutine pr_SetAuxiliaryValue
  
end module BudgetListItemModule