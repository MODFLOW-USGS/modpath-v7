module TrackPathResultModule
  use ParticlePathModule,only : ParticlePathType
  use ParticleLocationModule,only : ParticleLocationType
  implicit none
  
  
! Set default access status to private
  private

  type,public :: TrackPathResultType 
    integer :: Group, ParticleID, SequenceNumber
    type(ParticlePathType) :: ParticlePath
    integer :: Status
    
  contains
    procedure :: Reset=>pr_Reset
    procedure :: Status_Undefined=>pr_Undefined
    procedure :: Status_ReachedStoppingTime=>pr_ReachedStoppingTime
    procedure :: Status_ReachedBoundaryFace=>pr_ReachedBoundaryFace
    procedure :: Status_StopAtWeakSink=>pr_StopAtWeakSink
    procedure :: Status_StopAtWeakSource=>pr_StopAtWeakSource
    procedure :: Status_NoExitPossible=>pr_NoExitPossible
    procedure :: Status_StopZoneCell=>pr_StopZoneCell
    procedure :: Status_InactiveCell=>pr_InactiveCell
    procedure :: Status_InvalidLocation=>pr_InvalidLocation
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
  class(TrackPathResultType) :: this
!---------------------------------------------------------------------------------------------------------------

  this%ParticleID = 0
  this%Status = 0
  call this%ParticlePath%Clear()

end subroutine pr_Reset

!-------------------------------------------------------------------
  function pr_Undefined(this) result(status)
  implicit none
  class(TrackPathResultType) :: this
  integer :: status
  
  status = 0
  
  end function pr_Undefined

!-------------------------------------------------------------------
  function pr_ReachedStoppingTime(this) result(status)
  implicit none
  class(TrackPathResultType) :: this
  integer :: status
  
  status = 1
  
  end function pr_ReachedStoppingTime

!-------------------------------------------------------------------
  function pr_ReachedBoundaryFace(this) result(status)
  implicit none
  class(TrackPathResultType) :: this
  integer :: status
  
  status = 2
  
  end function pr_ReachedBoundaryFace

!-------------------------------------------------------------------
  function pr_StopAtWeakSink(this) result(status)
  implicit none
  class(TrackPathResultType) :: this
  integer :: status
  
  status = 3
  
  end function pr_StopAtWeakSink

!-------------------------------------------------------------------
  function pr_StopAtWeakSource(this) result(status)
  implicit none
  class(TrackPathResultType) :: this
  integer :: status
  
  status = 4
  
  end function pr_StopAtWeakSource

!-------------------------------------------------------------------
  function pr_NoExitPossible(this) result(status)
  implicit none
  class(TrackPathResultType) :: this
  integer :: status
  
  status = 5
  
  end function pr_NoExitPossible

!-------------------------------------------------------------------
  function pr_StopZoneCell(this) result(status)
  implicit none
  class(TrackPathResultType) :: this
  integer :: status
  
  status = 6
  
  end function pr_StopZoneCell

!-------------------------------------------------------------------
  function pr_InactiveCell(this) result(status)
  implicit none
  class(TrackPathResultType) :: this
  integer :: status
  
  status = 7
  
  end function pr_InactiveCell

!-------------------------------------------------------------------
  function pr_InvalidLocation(this) result(status)
  implicit none
  class(TrackPathResultType) :: this
  integer :: status
  
  status = 8
  
  end function pr_InvalidLocation

end module TrackPathResultModule