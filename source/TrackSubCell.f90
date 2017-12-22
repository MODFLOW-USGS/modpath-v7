module TrackSubCellModule
  use ParticleLocationModule,only : ParticleLocationType
  use TrackSubCellResultModule,only : TrackSubCellResultType
  use ModpathSubCellDataModule,only : ModpathSubCellDataType
  implicit none
  
! Set default access status to private
  private

  type,public :: TrackSubCellType
    type(ModpathSubCellDataType) :: SubCellData
  contains
    procedure,private :: CalculateDT=>pr_CalculateDT
    procedure,private :: NewXYZ=>pr_NewXYZ
    procedure :: ExecuteTracking=>pr_ExecuteTracking
  end type
  
contains
!-------------------------------------------------------------------
  subroutine pr_ExecuteTracking(this,stopIfNoExit,initialLocation,maximumTime, trackingResult)
  implicit none
  class(TrackSubCellType) :: this
  logical,intent(in) :: stopIfNoExit
  type(ParticleLocationType),intent(in) :: initialLocation
  doubleprecision,intent(in) :: maximumTime
  type(TrackSubCellResultType),intent(inout) :: trackingResult
  integer :: cellNumber
  doubleprecision :: initialX,initialY,initialZ,initialTime
  doubleprecision :: vx1,vx2,vy1,vy2,vz1,vz2
  doubleprecision :: vx,dvxdx,dtx,vy,dvydy,dty,vz,dvzdz,dtz,dt
  doubleprecision :: t,x,y,z
  integer :: exitFace,exitStatus
  integer :: statusVX,statusVY,statusVZ
  
  call trackingResult%Reset()
  
  cellNumber = initialLocation%CellNumber
  initialX = initialLocation%LocalX
  initialY = initialLocation%LocalY
  initialZ = initialLocation%LocalZ
  initialTime = initialLocation%TrackingTime
  
  trackingResult%CellNumber = cellNumber
  trackingResult%Row = this%SubCellData%Row
  trackingResult%Column = this%SubCellData%Column
  trackingResult%InitialLocation%CellNumber = cellNumber
  trackingResult%InitialLocation%LocalX = initialX
  trackingResult%InitialLocation%LocalY = initialY
  trackingResult%InitialLocation%LocalZ = initialZ
  trackingResult%InitialLocation%TrackingTime = initialTime
  trackingResult%FinalLocation%LocalX = initialX
  trackingResult%FinalLocation%LocalY = initialY
  trackingResult%FinalLocation%LocalZ = initialZ
  trackingResult%FinalLocation%TrackingTime = initialTime
  trackingResult%MaximumTime = maximumTime
  trackingResult%Status = trackingResult%Status_Undefined()
  
  if(stopIfNoExit) then
    if(.not. this%SubCellData%HasExitFace()) then
      trackingResult%Status = trackingResult%Status_NoExitPossible()
      return
    end if
  end if
  
  ! Make local copies of face velocities for convenience
  vx1 = this%SubCellData%VX1
  vx2 = this%SubCellData%VX2
  vy1 = this%SubCellData%VY1
  vy2 = this%SubCellData%VY2
  vz1 = this%SubCellData%VZ1
  vz2 = this%SubCellData%VZ2
  
  ! Compute time of travel to each possible exit face
  statusVX = this%CalculateDT(vx1, vx2, this%SubCellData%DX, initialX, vx, dvxdx, dtx)
  statusVY = this%CalculateDT(vy1, vy2, this%SubCellData%DY, initialY, vy, dvydy, dty)
  statusVZ = this%CalculateDT(vz1, vz2, this%SubCellData%DZ, initialZ, vz, dvzdz, dtz)
  
  exitFace = 0
  dt = 1.0d+30
  if((statusVX .lt. 2) .or. (statusVY .lt. 2) .or. (statusVZ .lt. 2)) then
    dt = dtx
    if(vx .lt. 0d0) then
      exitFace = 1
    else if(vx .gt. 0) then
      exitFace = 2
    end if
    
    if(dty .lt. dt) then
      dt = dty
      if(vy .lt. 0d0) then
        exitFace = 3
      else if(vy .gt. 0d0) then
        exitFace = 4
      end if
    end if
    
    if(dtz .lt. dt) then
      dt = dtz
      if(vz .lt. 0d0) then
        exitFace = 5
      else if(vz .gt. 0d0) then
        exitFace = 6
      end if
    end if
  else
  end if

  ! Increment t
  t = initialTime + dt
  
  ! If the maximum time is less than the computed exit time, then calculate the
  ! particle location at the maximum time and set the final time equal to
  ! the maximum time. Set status to 2 (MaximumTimeReached) and return.
  if(maximumTime .lt. t) then
    dt = maximumTime - initialTime
    t = maximumTime
    x = this%NewXYZ(vx, dvxdx, vx1, vx2, dt, initialX, this%SubCellData%DX, statusVX)
    y = this%NewXYZ(vy, dvydy, vy1, vy2, dt, initialY, this%SubCellData%DY, statusVY)
    z = this%NewXYZ(vz, dvzdz, vz1, vz2, dt, initialZ, this%SubCellData%DZ, statusVZ)
    trackingResult%ExitFace = 0
    trackingResult%FinalLocation%CellNumber = cellNumber
    trackingResult%FinalLocation%LocalX = x
    trackingResult%FinalLocation%LocalY = y
    trackingResult%FinalLocation%LocalZ = z
    trackingResult%FinalLocation%TrackingTime = t
    trackingResult%Status = trackingResult%Status_ReachedMaximumTime()
  else
      ! Otherwise, if the computed exit time is less than or equal to the maximum time,
      ! then calculate the exit location and set the final time equal to the computed
      ! exit time.
      if((exitFace .eq. 1) .or. (exitFace .eq.2)) then
          x = 0d0
          y = this%NewXYZ(vy, dvydy, vy1, vy2, dt, initialY, this%SubCellData%DY, statusVY)
          z = this%NewXYZ(vz, dvzdz, vz1, vz2, dt, initialZ, this%SubCellData%DZ, statusVZ)
          if(exitFace .eq. 2) x = 1.0d0
      else if((exitFace .eq. 3) .or. (exitFace .eq.4)) then
          x = this%NewXYZ(vx, dvxdx, vx1, vx2, dt, initialX, this%SubCellData%DX, statusVX)
          y = 0d0
          z = this%NewXYZ(vz, dvzdz, vz1, vz2, dt, initialZ, this%SubCellData%DZ, statusVZ)
          if(exitFace .eq. 4) y = 1.0d0
      else if((exitFace .eq. 5) .or. (exitFace .eq.6)) then
          x = this%NewXYZ(vx, dvxdx, vx1, vx2, dt, initialX, this%SubCellData%DX, statusVX)
          y = this%NewXYZ(vy, dvydy, vy1, vy2, dt, initialY, this%SubCellData%DY, statusVY)
          z = 0d0
          if(exitFace .eq. 6) z = 1.0d0
      else
          ! If it gets this far, something went wrong. Signal an error condition by
          ! setting Status = 0 (undefined) and then return.
          trackingResult%ExitFace = exitFace
          trackingResult%Status = trackingResult%Status_Undefined()
          return
      end if
      
      ! Assign tracking result data
      trackingResult%ExitFaceConnection = this%SubCellData%Connection(exitFace)
      if(this%SubCellData%Connection(exitFace) .lt. 0) then
          trackingResult%Status = trackingResult%Status_ExitAtInternalFace()
      else
          trackingResult%Status = trackingResult%Status_ExitAtCellFace()
      end if
      trackingResult%ExitFace = exitFace
      trackingResult%FinalLocation%CellNumber = cellNumber
      trackingResult%FinalLocation%LocalX = x
      trackingResult%FinalLocation%LocalY = y
      trackingResult%FinalLocation%LocalZ = z
      trackingResult%FinalLocation%TrackingTime = t
  end if
  
  end subroutine pr_ExecuteTracking
  
!-------------------------------------------------------------------
  function pr_CalculateDT(this,v1,v2,dx,xL,v,dvdx,dt) result(status)
  implicit none
  class(TrackSubCellType) :: this
  doubleprecision,intent(in) :: v1,v2,dx,xL
  doubleprecision,intent(inout) :: v,dvdx,dt
  doubleprecision :: v2a,v1a,dv,dva,vv,vvv,zro,zrom,x,tol
  doubleprecision :: vr1,vr2,vr,v1v2
  integer :: status
  logical :: noOutflow
  
  ! Initialize variables
  status = -1
  dt = 1.0d+20
  v2a = v2
  if(v2a .lt. 0d0) v2a = -v2a
  v1a = v1
  if(v1a .lt. 0d0) v1a = -v1a
  dv = v2 - v1
  dva = dv
  if(dva .lt. 0d0) dva = -dva
  
  ! Check for a uniform zero velocity in this direction.
  ! If so, set status = 2 and return (dt = 1.0d+20).
  tol = 1.0d-15
  if((v2a .lt. tol) .and. (v1a .lt. tol)) then
    v = 0d0
    dvdx = 0d0
    status = 2
    return
  end if
  
  ! Check for uniform non-zero velocity in this direction. 
  ! If so, set compute dt using the constant velocity, 
  ! set status = 1 and return.
  vv = v1a
  if(v2a .gt. vv) vv = v2a
  vvv = dva / vv
  if(vvv .lt. 1.0d-4) then
    zro = tol
    zrom = -zro
    v = v1
    x = xL * dx
    if(v1 .gt. zro) dt = (dx - x) / v1
    if(v1 .lt. zrom) dt = -x / v1
    dvdx = 0d0
    status = 1
    return
  end if
  
  ! Velocity has a linear variation.
  ! Compute velocity corresponding to particle position
  dvdx = dv / dx
  v = (1.0d0 - xL)*v1 + xL*v2
  
  ! If flow is into the cell from both sides there is no outflow.
  ! In that case, set status = 3 and return
  noOutflow = .true.
  if(v1 .lt. 0d0) noOutflow = .false.
  if(v2 .gt. 0d0) noOutflow = .false.
  if(noOutflow) then
    status = 3
    return
  end if
  
  ! If there is a divide in the cell for this flow direction, check to see if the
  ! particle is located exactly on the divide. If it is, move it very slightly to
  ! get it off the divide. This avoids possible numerical problems related to 
  ! stagnation points.
  if((v1 .le. 0d0) .and. (v2 .ge. 0d0)) then
    if(abs(v)  .le. 0d0) then
      v = 1.0d-20
      if(v2 .le. 0d0) v = -v
    end if
  end if

  ! If there is a flow divide, this check finds out what side of the divide the particle
  ! is on and sets the value of vr appropriately to reflect that location.
  vr1 = v1 / v
  vr2 = v2 / v
  vr = vr1
  if(vr .le. 0d0) vr = vr2
  
  ! Check to see if the velocity is in the same direction throughout the cell (i.e. no flow divide).
  ! Check to see if the product v1*v2 > 0 then the velocity is in the same direction throughout 
  ! the cell (i.e. no flow divide). If so, set the value of vr to reflect the appropriate direction.
  v1v2 = v1*v2
  if(v1v2 .gt. 0d0) then
    if(v .gt. 0d0) vr = vr2
    if(v .lt. 0d0) vr = vr1
  end if
  
  ! Compute travel time to exit face. Return with status = 0
  dt = log(vr) / dvdx
  status = 0
    
  end function pr_CalculateDT
  
!-------------------------------------------------------------------
  function pr_NewXYZ(this,v,dvdx,v1,v2,dt,x,dx,velocityProfileStatus) result(newX)
  implicit none
  class(TrackSubCellType) :: this
  integer,intent(in) :: velocityProfileStatus
  doubleprecision,intent(in) :: v,dvdx,v1,v2,dt,x,dx
  doubleprecision :: newX
  
  newX = x
  select case (velocityProfileStatus)
    case (1)
      newX = newX + (v1*dt/dx)
    case default
      if(v .ne. 0d0) then
        newX = newX + (v*(exp(dvdx*dt) - 1.0d0)/dvdx/dx)
      end if
  end select
  if(newX .lt. 0d0) newX = 0d0
  if(newX .gt. 1.0d0) newX = 1.0d0
  
  end function pr_NewXYZ
end module TrackSubCellModule