module GridLocationModule
  implicit none
  
! Set default access status to private
  private

! Private data type declarations

  
! Public derived data type definitions
!--------------------------------------
! type: 
!--------------------------------------
  type,public :: GridLocationType
    integer :: CellNumber, Layer, Row, Column
    doubleprecision :: LocalX
    doubleprecision :: LocalY
    doubleprecision :: LocalZ
    contains
    procedure :: SetGridLocationData=>pr_SetData
    procedure :: Reset => pr_Reset
  end type
  
  
contains

subroutine pr_Reset(this)
!***************************************************************************************************************
! Description goes here
!***************************************************************************************************************
!
! Specifications
!---------------------------------------------------------------------------------------------------------------
  implicit none
  class(GridLocationType) :: this
!---------------------------------------------------------------------------------------------------------------

  this%CellNumber = 0
  this%Layer = 0
  this%Row = 0
  this%Column = 0
  this%LocalX = 0.0d0
  this%LocalY = 0.0d0
  this%LocalZ = 0.0d0
  
end subroutine pr_Reset

!------------------------------------------
! Method: 
!------------------------------------------
  subroutine pr_SetData(this,cellNumber,localX,localY,localZ,layer,row,column)
  implicit none
  class(GridLocationType) :: this
  integer,intent(in) :: cellNumber,layer,row,column
  doubleprecision,intent(in) :: localX,localY,localZ
  
  this%CellNumber = cellNumber
  this%Layer = layer
  this%Row = row
  this%Column = column
  this%LocalX = localX
  this%LocalY = localY
  this%LocalZ = localZ
  
  end subroutine pr_SetData


end module GridLocationModule