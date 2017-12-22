module ParticleCoordinateModule
  use ParticleLocationModule,only : ParticleLocationType
  implicit none
  
! Set default access status to private
  private

!--------------------------------------
! type: 
!--------------------------------------
  type,public,extends(ParticleLocationType) :: ParticleCoordinateType
    doubleprecision :: GlobalX
    doubleprecision :: GlobalY
    doubleprecision :: GlobalZ
    contains
    procedure,private :: ParticleCoordinateType_SetData1
    procedure,private :: ParticleCoordinateType_SetData2
    procedure :: Reset=>pr_Reset
    generic :: SetData=>ParticleCoordinateType_SetData1, ParticleCoordinateType_SetData2
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
  class(ParticleCoordinateType) :: this
!---------------------------------------------------------------------------------------------------------------

  this%CellNumber = 0
  this%LocalX = 0.0d0
  this%LocalY = 0.0d0
  this%LocalZ = 0.0d0
  this%GlobalX = 0.0d0
  this%GlobalY = 0.0d0
  this%GlobalZ = 0.0d0
  this%TrackingTime = 0.0d0
  
end subroutine pr_Reset

!------------------------------------------
! Method: 
!------------------------------------------
  subroutine ParticleCoordinateType_SetData1(this,cellNumber,localX,localY,localZ,globalX,globalY,globalZ,trackingTime)
  implicit none
  class(ParticleCoordinateType) :: this
  integer,intent(in) :: cellNumber
  doubleprecision,intent(in) :: localX,localY,localZ,globalX,globalY,globalZ,trackingTime
  
  this%CellNumber = cellNumber
  this%LocalX = localX
  this%LocalY = localY
  this%LocalZ = localZ
  this%GlobalX = globalX
  this%GlobalY = globalY
  this%GlobalZ = globalZ
  this%TrackingTime = trackingTime
  
  end subroutine ParticleCoordinateType_SetData1

!------------------------------------------
! Method: 
!------------------------------------------
  subroutine ParticleCoordinateType_SetData2(this,particleLocation,globalX,globalY,globalZ)
  implicit none
  class(ParticleCoordinateType) :: this
  doubleprecision,intent(in) :: globalX,globalY,globalZ
  type(ParticleLocationType),intent(in) :: particleLocation
  
  this%CellNumber = particleLocation%CellNumber
  this%LocalX = particleLocation%LocalX
  this%LocalY = particleLocation%LocalY
  this%LocalZ = particleLocation%LocalZ
  this%TrackingTime = particleLocation%TrackingTime
  this%GlobalX = globalX
  this%GlobalY = globalY
  this%GlobalZ = globalZ
  
  end subroutine ParticleCoordinateType_SetData2

end module ParticleCoordinateModule