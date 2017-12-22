module ParticlePathModule
  use ParticleCoordinateModule,only : ParticleCoordinateType
  use ParticleCoordinateListModule,only : ParticleCoordinateListType
  implicit none
  
  
! Set default access status to private
  private

  type,public :: ParticlePathType
    type(ParticleCoordinateListType) :: Pathline
    type(ParticleCoordinateListType) :: Timeseries
  contains
    procedure :: Clear=>pr_Clear
  end type  
  
contains

subroutine pr_Clear(this)
!***************************************************************************************************************
! Description goes here
!***************************************************************************************************************
!
! Specifications
!---------------------------------------------------------------------------------------------------------------
  implicit none
  class(ParticlePathType) :: this
!---------------------------------------------------------------------------------------------------------------
  call this%Pathline%Clear()
  call this%Timeseries%Clear()

end subroutine pr_Clear

end module ParticlePathModule