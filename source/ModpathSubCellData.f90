module ModpathSubCellDataModule
  use ParticleLocationModule,only : ParticleLocationType
  implicit none
  
! Set default access status to private
  private
  
  type,public :: ModpathSubCellDataType
    ! public data
    integer :: Row, Column
    integer,dimension(6) :: Connection
    doubleprecision :: VX1,VX2,VY1,VY2,VZ1,VZ2,DX,DY,DZ
    doubleprecision,dimension(2) :: OffsetX,OffsetY,OffsetZ
  contains
    procedure :: IsExitFace=>pr_IsExitFace
    procedure :: HasExitFace=>pr_HasExitFace
    procedure :: ConvertToLocalParentCoordinate=>pr_ConvertToLocalParentCoordinate
    procedure :: ConvertFromLocalParentCoordinate=>pr_ConvertFromLocalParentCoordinate
    procedure :: ContainsLocalParentCoordinate=>pr_ContainsLocalParentCoordinate
    procedure :: Reset=>pr_Reset
  end type


contains

!------------------------------------------
  subroutine pr_Reset(this)
  implicit none
  class(ModpathSubCellDataType) :: this
  integer :: n
  
  this%Row = 0
  this%Column = 0
  this%VX1 = 0.0d0
  this%VX2 = 0.0d0
  this%VY1 = 0.0d0
  this%VY2 = 0.0d0
  this%VZ1 = 0.0d0
  this%VZ2 = 0.0d0
  this%DX = 0.0d0
  this%DY = 0.0d0
  this%DZ = 0.0d0
  
  do n = 1, 6
    this%Connection(n) = 0
  end do
  
  do n = 1, 2
    this%OffsetX(n) = 0.0d0
    this%OffsetY(n) = 0.0d0
    this%OffsetZ(n) = 0.0d0
  end do
  
  end subroutine pr_Reset

!------------------------------------------
  function pr_IsExitFace(this,faceNumber) result(fval)
  implicit none
  class(ModpathSubCellDataType) :: this
  integer,intent(in) :: faceNumber
  logical :: fval
  
  fval = .false.
  select case (faceNumber)
    case (1)
      if(this%VX1 .lt. 0d0) fval = .true.
    case (2)
      if(this%VX2 .gt. 0d0) fval = .true.
    case (3)
      if(this%VY1 .lt. 0d0) fval = .true.
    case (4)
      if(this%VY2 .gt. 0d0) fval = .true.
    case (5)
      if(this%VZ1 .lt. 0d0) fval = .true.
    case (6)
      if(this%VZ2 .gt. 0d0) fval = .true.
    case default
    ! return false
  end select
  
  end function pr_IsExitFace
  
!------------------------------------------
  function pr_HasExitFace(this) result(fval)
  implicit none
  class(ModpathSubCellDataType) :: this
  logical :: fval
  integer :: n
  
  fval = .false.
  do n = 1, 6
    if(this%IsExitFace(n)) fval = .true.
    if(fval) exit
  end do
  
  end function pr_HasExitFace
  
!------------------------------------------
  function pr_ConvertToLocalParentCoordinate(this,location) result(fval)
  implicit none
  class(ModpathSubCellDataType) :: this
  type(ParticleLocationType),intent(in) :: location
  type(ParticleLocationType) :: fval
  
  fval%CellNumber = location%CellNumber
  fval%LocalX = ((1.0d0 - location%LocalX) * this%OffsetX(1)) + (location%LocalX * this%OffsetX(2))
  fval%LocalY = ((1.0d0 - location%LocalY) * this%OffsetY(1)) + (location%LocalY * this%OffsetY(2))
  fval%LocalZ = ((1.0d0 - location%LocalZ) * this%OffsetZ(1)) + (location%LocalZ * this%OffsetZ(2))
  fval%TracKingTime = location%TrackingTime
  
  end function pr_ConvertToLocalParentCoordinate
  
!------------------------------------------
  function pr_ConvertFromLocalParentCoordinate(this,location) result(fval)
  implicit none
  class(ModpathSubCellDataType) :: this
  type(ParticleLocationType),intent(in) :: location
  type(ParticleLocationType) :: fval
  
  fval%CellNumber = 0
  fval%LocalX = 0d0
  fval%LocalY = 0d0
  fval%LocalZ = 0d0
  fval%TrackingTime = 0d0  
  
  if(this%ContainsLocalParentCoordinate(location%LocalX, location%LocalY, location%LocalZ)) then
    fval%CellNumber = location%CellNumber
    fval%LocalX = (location%LocalX - this%OffsetX(1)) / (this%OffsetX(2) - this%OffsetX(1))
    fval%LocalY = (location%LocalY - this%OffsetY(1)) / (this%OffsetY(2) - this%OffsetY(1))
    fval%LocalZ = (location%LocalZ - this%OffsetZ(1)) / (this%OffsetZ(2) - this%OffsetZ(1))
    fval%TrackingTime = location%TrackingTime
  end if
  
  end function pr_ConvertFromLocalParentCoordinate
  
!------------------------------------------
  function pr_ContainsLocalParentCoordinate(this,localParentX,localParentY,localParentZ) result(fval)
  implicit none
  class(ModpathSubCellDataType) :: this
  doubleprecision,intent(in) :: localParentX,localParentY,localParentZ
  logical :: fval
  
  fval = .true.
  if( (localParentX .lt. this%OffsetX(1)) .or. (localParentX .gt. this%OffsetX(2))) fval = .false.
  if( (localParentY .lt. this%OffsetY(1)) .or. (localParentY .gt. this%OffsetY(2))) fval = .false.
  if( (localParentZ .lt. this%OffsetZ(1)) .or. (localParentZ .gt. this%OffsetZ(2))) fval = .false.
  
  end function pr_ContainsLocalParentCoordinate



end module ModpathSubCellDataModule