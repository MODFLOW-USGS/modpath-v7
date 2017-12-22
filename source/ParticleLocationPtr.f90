module ParticleLocationPtrModule
  use ParticleLocationModule,only : ParticleLocationType
  implicit none
  
! Set default access status to private
  private

! Private data type declarations

  
! Public derived data type definitions

!--------------------------------------
! type: 
!--------------------------------------
  type,public :: ParticleLocationPtrType
    type(ParticleLocationType),pointer :: ItemPtr => null()
  end type 



end module ParticleLocationPtrModule