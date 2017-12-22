module ParticleLocationModule
  use GridLocationModule,only : GridLocationType
  implicit none
  
! Set default access status to private
  private

! Private data type declarations

  
! Public derived data type definitions
  
!--------------------------------------
! type: 
!--------------------------------------
  type,public,extends(GridLocationType) :: ParticleLocationType
    doubleprecision :: TrackingTime
    contains
    procedure :: SetParticleLocationData=>pr_SetData
    procedure :: Valid=>pr_Valid
    procedure :: GetCopy=>pr_GetCopy
    procedure :: Reset=>pr_Reset
    procedure,private :: pr_SetData
    procedure,private :: pr_SetData2
    generic   :: SetData=>pr_SetData, pr_SetData2
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
  class(ParticleLocationType) :: this
!---------------------------------------------------------------------------------------------------------------

  this%CellNumber = 0
  this%LocalX = 0.0d0
  this%LocalY = 0.0d0
  this%LocalZ = 0.0d0
  this%TrackingTime = 0.0d0
  
end subroutine pr_Reset

!------------------------------------------
  function pr_GetCopy(this) result(newCopy)
  implicit none
  class(ParticleLocationType) :: this
  type(ParticleLocationType),pointer :: newCopy
  
  allocate(newCopy)
  newCopy%CellNumber = this%CellNumber
  newCopy%LocalX = this%LocalX
  newCopy%LocalY = this%LocalY
  newCopy%LocalZ = this%LocalZ
  newCopy%TrackingTime = this%TrackingTime
  
  end function pr_GetCopy

subroutine  pr_SetData2(this, location)
!***************************************************************************************************************
! Description goes here
!***************************************************************************************************************
!
! Specifications
!---------------------------------------------------------------------------------------------------------------
  implicit none
  class(ParticleLocationType) :: this
  type(ParticleLocationType) :: location
!---------------------------------------------------------------------------------------------------------------

  call this%SetData(location%CellNumber, location%LocalX, location%LocalY, location%LocalZ, location%TrackingTime, location%Layer)

end subroutine  pr_SetData2

!------------------------------------------
  subroutine pr_SetData(this,cellNumber,localX,localY,localZ,trackingTime,layer)
  implicit none
  class(ParticleLocationType) :: this
  integer,intent(in) :: cellNumber,layer
  doubleprecision,intent(in) :: localX,localY,localZ,trackingTime
  
  this%CellNumber = cellNumber
  this%Layer = layer
  this%LocalX = localX
  this%LocalY = localY
  this%LocalZ = localZ
  this%TrackingTime = trackingTime
  
  end subroutine pr_SetData

!------------------------------------------
  function pr_Valid(this) result(valid)
  implicit none
  class(ParticleLocationType) :: this
  logical :: valid
  
  valid = .true.
  if((this%LocalX .lt. 0.0d0) .or. (this%LocalX .gt. 1.0d0)) valid = .false.
  if((this%LocalY .lt. 0.0d0) .or. (this%LocalY .gt. 1.0d0)) valid = .false.
  if((this%LocalZ .lt. 0.0d0) .or. (this%LocalZ .gt. 1.0d0)) valid = .false.
  
  end function pr_Valid

end module ParticleLocationModule