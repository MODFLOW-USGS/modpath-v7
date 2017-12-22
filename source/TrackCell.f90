module TrackCellModule
  use ParticleLocationModule,only : ParticleLocationType
  use TrackCellResultModule,only : TrackCellResultType
  use TrackSubCellResultModule,only : TrackSubCellResultType
  use ModpathSubCellDataModule,only : ModpathSubCellDataType
  use ModpathCellDataModule,only : ModpathCellDataType
  use ParticleTrackingOptionsModule,only : ParticleTrackingOptionsType
  use TrackSubCellModule,only : TrackSubCellType
  implicit none
  
! Set default access status to private
  private

  type,public :: TrackCellType
    type(ModpathCellDataType) :: CellData
    type(ParticleTrackingOptionsType) :: TrackingOptions
    type(TrackSubCellType),private :: TrackSubCell
    logical :: SteadyState
  contains
    procedure :: ExecuteTracking=>pr_ExecuteTracking
    ! Private methods
    procedure,private :: FillSubCellDataBuffer=>pr_FillSubCellDataBuffer
    procedure,private :: AtCellExitFace=>pr_AtCellExitFace
    procedure,private :: FindSubCell=>pr_FindSubCell
  end type

contains
  
!-------------------------------------------------------------------
  subroutine pr_FillSubCellDataBuffer(this,row,column)
  implicit none
  class(TrackCellType) :: this
  integer,intent(in) :: row,column
  
  call this%CellData%FillSubCellDataBuffer(this%TrackSubCell%SubCellData, row, column, this%TrackingOptions%BackwardTracking)
  
  end subroutine pr_FillSubCellDataBuffer
  
!-------------------------------------------------------------------
  subroutine pr_FindSubCell(this,location,row,column)
  implicit none
  class(TrackCellType) :: this
  type(ParticleLocationType),intent(in) :: location
  integer,intent(inout) :: row,column
  doubleprecision,dimension(3) :: offset
  doubleprecision :: xmin,xmax,ymin,ymax,x,y
  integer :: i,j,subCellCount
  
  row = 0
  column = 0
  
  subCellCount = this%CellData%GetSubCellCount()
  if(subCellCount .eq. 1) then
      row = 1
      column = 1
  else if(subCellCount .eq. 4) then
      offset(1) = 0.0d0
      offset(2) = 1.0d0 / 2.0d0
      offset(3) = 1.0d0
      
      x = location%LocalX
      y = location%LocalY
      do i = 1,2
          do j = 1,2
            xmin = offset(j)
            xmax = offset(j+1)
            ymin = offset(3-i)
            ymax = offset(3-i+1)
            if((x .ge. xmin) .and. (x .le. xmax) .and. (y .ge. ymin) .and. (y .le. ymax)) then
                row = i
                column = j
                return
            end if
          end do
      end do
  end if
  
  end subroutine pr_FindSubCell
    
!-------------------------------------------------------------------
  function pr_AtCellExitFace(this,trackingResult) result(atExitFace)
  implicit none
  class(TrackCellType) :: this
  type(TrackSubCellResultType),intent(in) :: trackingResult
  logical :: atExitFace
  
  atExitFace = .false.
  select case (trackingResult%ExitFace)
      case (1)
          if(trackingResult%Column .eq. 1) atExitFace = .true.
      case (2)
          if(trackingResult%Column .eq. this%CellData%GetSubCellColumnCount()) atExitFace = .true.
      case (3)
          if(trackingResult%Row .eq. this%CellData%GetSubCellRowCount()) atExitFace = .true.
      case (4)
          if(trackingResult%Row .eq. 1) atExitFace = .true.      
      case (5)
          atExitFace = .true.
      case (6)
          atExitFace = .true.
      case default
      ! do nothing
  end select
  
  end function pr_AtCellExitFace
  
!-------------------------------------------------------------------
  subroutine pr_ExecuteTracking(this, initialLocation, maximumTime, trackCellResult)
  implicit none
  class(TrackCellType) :: this
  type(ParticleLocationType),intent(in) :: initialLocation
  type(TrackCellResultType),intent(inout) :: trackCellResult
  doubleprecision,intent(in) :: maximumTime
  type(TrackSubCellResultType) :: subCellResult
  type(ParticleLocationType) :: subLoc,pLoc
  logical :: stopIfNoSubCellExit,hasExit
  integer :: subRow,subColumn,count, layer, stopZone
  
  stopIfNoSubCellExit = .true.
  
  ! Initialize cellResult
  call trackCellResult%Reset()
  trackCellResult%CellNumber = this%CellData%CellNumber
  trackCellResult%ExitFace = 0
  trackCellResult%Status = trackCellResult%Status_Undefined()
  trackCellResult%MaximumTime = maximumTime
  
  ! If the initial location cell number does not match this cell or any of the local coordinates
  ! are less than 0 or greater than 1, then set status to InvalidLocation and return.
  layer = initialLocation%Layer
  if((initialLocation%CellNumber .ne. trackCellResult%CellNumber) .or. (.not. initialLocation%Valid())) then
      trackCellResult%Status = trackCellResult%Status_InvalidLocation()
      return
  end if
  
  ! If the initial location cell is inactive for this time step, set status to InactiveCell
  if(this%CellData%IboundTS .eq. 0) then
      trackCellResult%Status = trackCellResult%Status_InactiveCell()
      return
  end if
  
  ! Add the initial location to the TrackingPoints list
  call trackCellResult%TrackingPoints%AddItem(initialLocation)
  
  ! Check for stopping conditions
  
  ! Check to see if this cell is an automatic stop zone cell
  stopZone = this%TrackingOptions%StopZone;
  if(stopZone .gt. 0) then
      if(this%CellData%Zone .eq. stopZone) then
        trackCellResult%Status = trackCellResult%Status_StopZoneCell()
        return;
      end if
  end if
  
  ! If the cell has no exit face then:
  !   1. If the system is steady state, set status NoExitPossible and return immediately
  !   2. If the system is transient, tracking is backward, and SourceFlow is not equal to 0, set status to 
  !      NoExitPossible and return immediately
  !   3. If the system is transient, tracking is forware, and SinkFlow is not equal to 0, set status to
  !      NoExitPossible and return immediately
  !
  ! First check to see if cell has at least one exit face.
  hasExit = this%CellData%HasExitFace(this%TrackingOptions%BackwardTracking)
  if(.not. hasExit) then
      if(this%SteadyState) then
          trackCellResult%Status = trackCellResult%Status_NoExitPossible()
          return;
      else
          if((this%TrackingOptions%BackwardTracking) .and. (this%CellData%SourceFlow .ne. 0.0d0)) then
               trackCellResult%Status = trackCellResult%Status_NoExitPossible()
               return;
          else if((.not. this%TrackingOptions%BackwardTracking) .and. (this%CellData%SinkFlow .ne. 0.0d0)) then
               trackCellResult%Status = trackCellResult%Status_NoExitPossible()
               return;
          end if
      end if
  end if
  
  ! Check weak sink/source stopping option
  if(this%TrackingOptions%BackwardTracking) then
      if(this%TrackingOptions%StopAtWeakSources) then
          if(this%CellData%SourceFlow .ne. 0.0d0) then 
              trackCellResult%Status = trackCellResult%Status_StopAtWeakSource()
              return;
          else
              if(.not. this%SteadyState) stopIfNoSubCellExit = .false.
          end if
      end if
  else
      if(this%TrackingOptions%StopAtWeakSinks) then
          if(this%CellData%SinkFlow .ne. 0.0d0) then
              trackCellResult%Status = trackCellResult%Status_StopAtWeakSink()
              return;
          else
              if(.not. this%SteadyState) stopIfNoSubCellExit = .false.
          end if
      end if
  end if
  
  ! No immediate stopping condition was found, so loop through sub-cells until a stopping condition is reached.
      
  ! Find the sub-cell that contains the initial location
  call this%FindSubCell(initialLocation,subRow,subColumn)
  
  ! If subRow or subColumn returns 0 then an error occurred. Set Status = Undefined and return
  if((subRow .eq. 0) .or. (subColumn .eq. 0)) then
      trackCellResult%Status = trackCellResult%Status_Undefined()
      return
  end if
  
  ! Initialize the sub-cell buffer and convert the initial location to the equivalent local sub-cell coordinates.
  call this%CellData%FillSubCellDataBuffer(this%TrackSubCell%SubCellData,       &
    subRow,subColumn,this%TrackingOptions%BackwardTracking)
  subLoc =                                                                      &
    this%TrackSubCell%SubCellData%ConvertFromLocalParentCoordinate(initialLocation)
  
  ! Loop through sub-cells until a stopping condition is met.
  ! The loop count is set to 100. If it goes through the loop 100 times then something is wrong. 
  ! In that case, trackCellResult%Status will be set to Undefined and the result will be returned.
  do count = 1, 100
      call this%TrackSubCell%ExecuteTracking(stopIfNoSubCellExit,subLoc,maximumTime, subCellResult)
  
      ! Check subCellResult status and process accordingly
      if(subCellResult%Status .eq. subCellResult%Status_Undefined()) then
          trackCellResult%Status = trackCellResult%Status_Undefined()
          return
      else if(subCellResult%Status .eq. subCellResult%Status_ExitAtInternalFace()) then
          pLoc = this%TrackSubCell%SubCellData%ConvertToLocalParentCoordinate(subCellResult%FinalLocation)
          pLoc%Layer = layer
          call trackCellResult%TrackingPoints%AddItem(pLoc)
          
          select case (subCellResult%ExitFace)
            case (1)
                if(subCellResult%Column .ne. 2) then
                    ! Face 1 cannot be an internal face unless the column index equals 2.
                    ! If so, set trackCellResult%Status to Undefined and return.
                    trackCellResult%Status = trackCellResult%Status_Undefined()
                    return
                end if
                subLoc%LocalX = 1.0d0
                subLoc%LocalY = subCellResult%FinalLocation%LocalY
                subLoc%LocalZ = subCellResult%FinalLocation%LocalZ
                subLoc%TrackingTime = subCellResult%FinalLocation%TrackingTime
                subColumn = 1
                call this%CellData%FillSubCellDataBuffer(                       &
                  this%TrackSubCell%SubCellData,subRow,subColumn,               &
                  this%TrackingOptions%BackwardTracking)
            case (2)
                if(subCellResult%Column .ne. 1) then
                    ! Face 2 cannot be an internal face unless the column index equals 1.
                    ! If so, set trackCellResult%Status to Undefined and return.
                    trackCellResult%Status = trackCellResult%Status_Undefined()
                    return
                end if
                subLoc%LocalX = 0.0d0
                subLoc%LocalY = subCellResult%FinalLocation%LocalY
                subLoc%LocalZ = subCellResult%FinalLocation%LocalZ
                subLoc%TrackingTime = subCellResult%FinalLocation%TrackingTime
                subColumn = 2
                call this%CellData%FillSubCellDataBuffer(                       &
                  this%TrackSubCell%SubCellData,subRow,subColumn,               &
                  this%TrackingOptions%BackwardTracking)
            case (3)
                if(subCellResult%Row .ne. 1) then
                    ! Face 3 cannot be an internal face unless the row index equals 1.
                    ! If so, set trackCellResult%Status to Undefined and return.
                    trackCellResult%Status = trackCellResult%Status_Undefined()
                    return
                end if
                subLoc%LocalX = subCellResult%FinalLocation%LocalX
                subLoc%LocalY = 1.0d0
                subLoc%LocalZ = subCellResult%FinalLocation%LocalZ
                subLoc%TrackingTime = subCellResult%FinalLocation%TrackingTime
                subRow = 2
                call this%CellData%FillSubCellDataBuffer(                       &
                  this%TrackSubCell%SubCellData, subRow, subColumn,             &
                  this%TrackingOptions%BackwardTracking)
            case (4)
                if(subCellResult%Row .ne. 2) then
                    ! Face 4 cannot be an internal face unless the row index equals 2.
                    ! If so, set trackCellResult%Status to Undefined and return.
                    trackCellResult%Status = trackCellResult%Status_Undefined()
                    return
                end if
                subLoc%LocalX = subCellResult%FinalLocation%LocalX
                subLoc%LocalY = 0.0d0
                subLoc%LocalZ = subCellResult%FinalLocation%LocalZ
                subLoc%TrackingTime = subCellResult%FinalLocation%TrackingTime
                subRow = 1
                call this%CellData%FillSubCellDataBuffer(                       &
                  this%TrackSubCell%SubCellData, subRow, subColumn,             &
                  this%TrackingOptions%BackwardTracking)
            case default
                ! Something went wrong. Set trackCellResult%Status equal to Undefined and return
                trackCellResult%Status = trackCellResult%Status_Undefined()
                return
          end select
      else if(subCellResult%Status .eq. subCellResult%Status_ExitAtCellFace()) then
          ! The particle has reached a cell boundary face, set trackCellResult status
          ! and exit face and then return. Status = 2 (ReachedBoundaryFace)
          pLoc = this%TrackSubCell%SubCellData%ConvertToLocalParentCoordinate(subCellResult%FinalLocation)
          pLoc%Layer = layer
          call trackCellResult%TrackingPoints%AddItem(pLoc)
          trackCellResult%Status = trackCellResult%Status_ExitAtCellFace()
          trackCellResult%ExitFace = subCellResult%ExitFace
          trackCellResult%NextCellNumber = subCellResult%ExitFaceConnection
          return
      else if(subCellResult%Status .eq. subCellResult%Status_ReachedMaximumTime()) then
          pLoc = this%TrackSubCell%SubCellData%ConvertToLocalParentCoordinate(subCellResult%FinalLocation)
          pLoc%Layer = layer
          call trackCellResult%TrackingPoints%AddItem(pLoc)
          trackCellResult%Status = trackCellResult%Status_ReachedStoppingTime()
          trackCellResult%ExitFace = subCellResult%ExitFace
          trackCellResult%NextCellNumber = pLoc%CellNumber
          return
      else if(subCellResult%Status .eq. subCellResult%Status_NoExitPossible()) then
          trackCellResult%Status = trackCellResult%Status_NoExitPossible()
          return
      end if
      
  end do

  ! If it gets this far something when wrong, so process the error condition and return
  trackCellResult%Status = trackCellResult%Status_Undefined()   
  
  end subroutine pr_ExecuteTracking
  
  
end module TrackCellModule