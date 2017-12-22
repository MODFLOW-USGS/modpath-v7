module TrackCellResultModule
  use ParticleLocationListModule,only : ParticleLocationListType
  use ModpathCellDataModule,only : ModpathCellDataType
  implicit none
  
! Set default access status to private
  private

  type,public :: TrackCellResultType 
    integer :: CellNumber
    integer :: ExitFace
    doubleprecision :: MaximumTime
    type(ParticleLocationListType) :: TrackingPoints
    integer :: NextCellNumber
    integer :: Status
    character(len=132) :: LogMessage
  contains
    ! Status enumeration functions
    procedure :: Reset=>pr_Reset
    procedure :: Status_Undefined=>pr_Undefined
    procedure :: Status_ReachedStoppingTime=>pr_ReachedStoppingTime
    procedure :: Status_ExitAtCellFace=>pr_ExitAtCellFace
    procedure :: Status_StopAtWeakSink=>pr_StopAtWeakSink
    procedure :: Status_StopAtWeakSource=>pr_StopAtWeakSource
    procedure :: Status_NoExitPossible=>pr_NoExitPossible
    procedure :: Status_StopZoneCell=>pr_StopZoneCell
    procedure :: Status_InactiveCell=>pr_InactiveCell
    procedure :: Status_InvalidLocation=>pr_InvalidLocation
  end type
  
contains
! Add procedures here

subroutine pr_Reset(this)
!***************************************************************************************************************
! Description goes here
!***************************************************************************************************************
!
! Specifications
!---------------------------------------------------------------------------------------------------------------
  implicit none
  class(TrackCellResultType) :: this
!---------------------------------------------------------------------------------------------------------------
  this%CellNumber = 0
  this%ExitFace = 0
  this%MaximumTime = 0.0d0
  this%NextCellNumber = 0
  call this%TrackingPoints%Clear()

end subroutine

!-------------------------------------------------------------------
  function pr_Undefined(this) result(status)
  implicit none
  class(TrackCellResultType) :: this
  integer :: status
  
  status = 0
  
  end function pr_Undefined

!-------------------------------------------------------------------
  function pr_ReachedStoppingTime(this) result(status)
  implicit none
  class(TrackCellResultType) :: this
  integer :: status
  
  status = 1
  
  end function pr_ReachedStoppingTime

!-------------------------------------------------------------------
  function pr_ExitAtCellFace(this) result(status)
  implicit none
  class(TrackCellResultType) :: this
  integer :: status
  
  status = 2
  
  end function pr_ExitAtCellFace

!-------------------------------------------------------------------
  function pr_StopAtWeakSink(this) result(status)
  implicit none
  class(TrackCellResultType) :: this
  integer :: status
  
  status = 3
  
  end function pr_StopAtWeakSink

!-------------------------------------------------------------------
  function pr_StopAtWeakSource(this) result(status)
  implicit none
  class(TrackCellResultType) :: this
  integer :: status
  
  status = 4
  
  end function pr_StopAtWeakSource

!-------------------------------------------------------------------
  function pr_NoExitPossible(this) result(status)
  implicit none
  class(TrackCellResultType) :: this
  integer :: status
  
  status = 5
  
  end function pr_NoExitPossible

!-------------------------------------------------------------------
  function pr_StopZoneCell(this) result(status)
  implicit none
  class(TrackCellResultType) :: this
  integer :: status
  
  status = 6
  
  end function pr_StopZoneCell

!-------------------------------------------------------------------
  function pr_InactiveCell(this) result(status)
  implicit none
  class(TrackCellResultType) :: this
  integer :: status
  
  status = 7
  
  end function pr_InactiveCell

!-------------------------------------------------------------------
  function pr_InvalidLocation(this) result(status)
  implicit none
  class(TrackCellResultType) :: this
  integer :: status
  
  status = 8
  
  end function pr_InvalidLocation

end module TrackCellResultModule