module TrackCellOptionsModule
  implicit none
  
! Set default access status to private
  private

  type,public :: TrackCellOptionsType
    logical :: SteadyState
    logical :: BackwardTracking
    logical :: StopAtWeakSinks
    logical :: StopAtWeakSources
    logical :: CreateTrackingLog
  end type

contains

end module TrackCellOptionsModule