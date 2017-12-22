module ModflowRectangularGridModule
  use UTL8MODULE
  use ModflowGridModule
  use GridLocationModule,only : GridLocationType
  
  implicit none
  
! Set default access status to private
  private
    
! Public derived data type definitions
!--------------------------------------
! type: 
!--------------------------------------
  type, public, extends(ModflowGridType) :: ModflowRectangularGridType
    integer :: RowCount = 0 
    integer :: ColumnCount = 0
    integer,dimension(:),allocatable :: JaFace
    double precision,dimension(:),allocatable :: DelX, DelY
  contains
    procedure :: GetLeft
    procedure :: GetRight
    procedure :: GetFront
    procedure :: GetBack
    procedure :: GetCellBoundaries
    procedure :: GetDxDy
    procedure :: GetPotentialFaceConnectionCount
    procedure :: GetFaceConnection
    procedure :: ConvertFromNeighbor
    procedure :: FindConnectionPosition
    procedure :: FindConnectionIndex
    procedure :: GetCellConnectionFaces
    procedure :: ConvertToModelXY
    procedure :: ConvertToLocalXY
    procedure :: ConvertToModelXYZ
    procedure :: ConvertToModelZ
    procedure :: Reset
    procedure :: ComputeFaceAssignments
    procedure :: ExtractConnectionInfo
  end type

    contains
!----------------------------------------------------------------------
  subroutine ExtractConnectionInfo(this, connInfo, faceNumber, subFaceNumber, subFaceCount)
  implicit none
  class(ModflowRectangularGridType) :: this
  integer,intent(in) :: connInfo
  integer,intent(inout) :: faceNumber, subFaceNumber, subFaceCount
  integer :: n
  double precision :: r
  
  r = dble(connInfo)
  n = int(r / 100.0)
  n = 100 * n
  subFaceCount = connInfo - n
  
  if((n .gt. 10000) .and. (n .lt. 20000)) then
      faceNumber = 1
  else if((n .gt. 20000) .and. (n .lt. 30000)) then
      faceNumber = 2
  else if((n .gt. 30000) .and. (n .lt. 40000)) then
      faceNumber = 3
  else if((n .gt. 40000) .and. (n .lt. 50000)) then
      faceNumber = 4
  else if((n .gt. 50000) .and. (n .lt. 60000)) then
      faceNumber = 5
  else if((n .gt. 60000) .and. (n .lt. 70000)) then
      faceNumber = 6
  else
      ! Invalid value, stop with error message
      call ustop('Invalid entry in the JaFace array. stop.')
  end if     
  
  subFaceNumber = (n - (10000*faceNumber)) / 100
  
  end subroutine ExtractConnectionInfo
!----------------------------------------------------------------------
  subroutine ComputeFaceAssignments(this)
  implicit none
  class(ModflowRectangularGridType) :: this
  integer :: i, n, face, m, offset, count, conn, layer, connLayer
  double precision :: tol, diff, rx, ry
  double precision :: left, right, front, back
  double precision :: connLeft, connRight, connFront, connBack
  
  tol = 0.0001
  if(allocated(this%JaFace)) deallocate(this%JaFace)
  allocate(this%JaFace(this%JaCount))
  do n = 1, this%CellCount
      offset = this%JaOffsets(n)
      count = this%JaOffsets(n + 1) - offset
      if(count .eq. 0) cycle
      this%JaFace(offset + 1) = 0
      call this%GetCellBoundaries(n, left, right, front, back)
      layer = this%GetLayer(n)
      
      do i = 2, count
          conn = this%Ja(offset + i)
          call this%GetCellBoundaries(conn, connLeft, connRight, connFront, connBack)
          connLayer = this%GetLayer(conn)
          
          ! Compute grid spacing ratio and snap to 0.5, 1.0, or 2.0 so that equality
          ! can be checked without worrying about rounding errors making something
          ! mistakenly fail the check.
          rx = this%DelX(conn) / this%DelX(n)
          if(abs(rx - 0.5) .le. tol) then
              rx = 0.5
          else if(abs(rx - 1.0) .le. tol) then
              rx = 1.0
          else if(abs(rx - 2.0) .le. tol) then
              rx = 2.0
          else
              ! exit with error message that this grid is not consistent with a 
              ! smoothed quad grid.
              call ustop('Invalid grid spacing structure. stop.')
          end if
          ry = this%DelY(conn) / this%DelY(n)
          if(abs(ry - 0.5) .le. tol) then
              ry = 0.5
          else if(abs(ry - 1.0) .le. tol) then
              ry = 1.0
          else if(abs(ry - 2.0) .le. tol) then
              ry = 2.0
          else
              ! exit with error message that this grid is not consistent with a 
              ! smoothed quad grid.
              call ustop('Invalid grid spacing structure. stop.')
          end if
          
          ! Assign face connections
          m = 0
          if(connLayer .gt. layer) then
              m = 50000
              if((rx .eq. 0.5) .and. (ry .eq. 0.5)) then
                  if((this%CellY(conn) .gt. this%CellY(n)) .and. (this%CellX(conn) .lt. this%CellX(n))) then
                      m = m + 104
                  else if((this%CellY(conn) .gt. this%CellY(n)) .and. (this%CellX(conn) .gt. this%CellX(n))) then
                      m = m + 204
                  else if((this%CellY(conn) .lt. this%CellY(n)) .and. (this%CellX(conn) .lt. this%CellX(n))) then
                      m = m + 304
                  else if((this%CellY(conn) .lt. this%CellY(n)) .and. (this%CellX(conn) .gt. this%CellX(n))) then
                      m = m + 404
                  end if
              else if((rx .ge. 1.0) .and. (ry .ge. 1.0)) then
                  m = m + 101
              end if
          else if(connLayer .lt. layer) then
              m = 60000
              if((rx .eq. 0.5) .and. (ry .eq. 0.5)) then
                  if((this%CellY(conn) .gt. this%CellY(n)) .and. (this%CellX(conn) .lt. this%CellX(n))) then
                      m = m + 104
                  else if((this%CellY(conn) .gt. this%CellY(n)) .and. (this%CellX(conn) .gt. this%CellX(n))) then
                      m = m + 204
                  else if((this%CellY(conn) .lt. this%CellY(n)) .and. (this%CellX(conn) .lt. this%CellX(n))) then
                      m = m + 304
                  else if((this%CellY(conn) .lt. this%CellY(n)) .and. (this%CellX(conn) .gt. this%CellX(n))) then
                      m = m + 404
                  end if                  
              else if((rx .ge. 1.0) .and. (ry .ge. 1.0)) then
                  m = m + 101
              end if
          else
              if(abs(left - connRight) .le. tol) then
                  m = 10000
                  if(ry .eq. 0.5) then
                      if(this%CellY(conn) .gt. this%CellY(n)) then
                          m = m + 102
                      else if(this%CellY(conn) .lt. this%CellY(n)) then
                          m = m + 202
                      end if
                  else if(ry .ge. 1.0) then
                      m = m + 101
                  end if
              else if(abs(right - connLeft) .le. tol) then
                  m = 20000
                  if(ry .eq. 0.5) then
                      if(this%CellY(conn) .gt. this%CellY(n)) then
                          m = m + 102
                      else if(this%CellY(conn) .lt. this%CellY(n)) then
                          m = m + 202
                      end if
                   else if(ry .ge. 1.0) then
                      m = m + 101
                 end if
              else if(abs(front - connBack) .le. tol) then
                  m = 30000
                  if(rx .le. 0.5) then
                      if(this%CellX(conn) .lt. this%CellX(n)) then
                          m = m + 102
                      else if(this%CellX(conn) .gt. this%CellX(n)) then
                          m = m + 202
                      end if
                  else if(rx .ge. 1.0) then
                      m = m + 101
                  end if
              else if(abs(back - connFront) .le. tol) then
                  m = 40000
                  if(rx .le. 0.5) then
                      if(this%CellX(conn) .lt. this%CellX(n)) then
                          m = m + 102
                      else if(this%CellX(conn) .gt. this%CellX(n)) then
                          m = m + 202
                      end if                      
                  else if(rx .ge. 1.0) then
                      m = m + 101
                  end if
              end if
          end if
          ! Assign the computed value to the element of the JaFace array that corresponds to
          ! the connection.
          this%JaFace(offset + i) = m
      end do
      
  end do
  
  
  end subroutine ComputeFaceAssignments
!x------------------------------------------  
  subroutine Reset(this)
  implicit none
  class(ModflowRectangularGridType) :: this
  
  ! Add override code to implement
  
  end subroutine Reset
!x------------------------------------------
  subroutine GetCellBoundaries(this, cellNumber, left, right, front, back)
  class(ModflowRectangularGridType) :: this
  integer, intent(in) :: cellNumber
  doubleprecision, intent(inout) :: left, right, front, back
  
  left = this%GetLeft(cellNumber)
  right = this%GetRight(cellNumber)
  front = this%GetFront(cellNumber)
  back = this%GetBack(cellNumber)

  end subroutine GetCellBoundaries
!x------------------------------------------
  function GetLeft(this, cellNumber) result(fval)
  class(ModflowRectangularGridType) :: this
  integer :: cellNumber
  double precision :: fval
  
  fval = this%CellX(cellNumber) - (this%DelX(cellNumber) / 2.0)
  
  end function GetLeft
!x------------------------------------------
  function GetRight(this, cellNumber) result(fval)
  class(ModflowRectangularGridType) :: this
  integer :: cellNumber
  double precision :: fval
  
  fval = this%CellX(cellNumber) + (this%DelX(cellNumber) / 2.0)
  
  end function GetRight
!x------------------------------------------
  function GetFront(this, cellNumber) result(fval)
  class(ModflowRectangularGridType) :: this
  integer :: cellNumber
  double precision :: fval
  
  fval = this%CellY(cellNumber) - (this%DelY(cellNumber) / 2.0)
  
  end function GetFront
!x------------------------------------------
  function GetBack(this, cellNumber) result(fval)
  class(ModflowRectangularGridType) :: this
  integer :: cellNumber
  double precision :: fval
  
  fval = this%CellY(cellNumber) + (this%DelY(cellNumber) / 2.0)
  
  end function GetBack
!x------------------------------------------
  subroutine GetDxDy(this, cellNumber, dx, dy) 
  class(ModflowRectangularGridType) :: this
  integer,intent(in) :: cellNumber
  integer :: layer, row, column
  doubleprecision, intent(inout) :: dx, dy
  
  dx = this%DelX(cellNumber)
  dy = this%DelY(cellNumber)
  
  end subroutine GetDxDy
!x------------------------------------------
  function GetPotentialFaceConnectionCount(this, cellNumber, face) result(count)
  class(ModflowRectangularGridType) :: this
  integer,intent(in) :: cellNumber,face
  integer :: offset, i, count, connCount, f1, f2, n
  integer :: faceNumber, subFaceNumber
  
  count = 0
  offset = this%JaOffsets(cellNumber)
  connCount = this%JaOffsets(cellNumber + 1) - offset
  
  f1 = 10000 * face
  f2 = 10000 * (face + 1)
  
  do i = 2, connCount
      n = this%JaFace(offset + i)
      if((n .gt. f1) .and. (n .lt. f2)) then
          call this%ExtractConnectionInfo(n, faceNumber, subFaceNumber, count)
          return
      end if
  end do
    
  end function GetPotentialFaceConnectionCount
!x------------------------------------------
  function GetFaceConnection(this, cellNumber, face, subface) result(conn)
  class(ModflowRectangularGridType) :: this
  integer,intent(in) :: cellNumber,face,subface
  integer :: conn,offset,count,faceFlag,flag1,flag2,n
  
  conn = 0
  flag1 = (10000 * face) + (100 * subface)
  flag2 = (10000 * face) + (100 * (subface + 1))
  offset = this%JaOffsets(cellNumber)
  count = this%JaOffsets(cellNumber + 1) - offset
  
  do n = 1, count
      faceFlag = this%JaFace(offset + n)
      if((faceFlag .ge. flag1) .and. (faceFlag .lt. flag2)) then
          conn = this%Ja(offset + n)
          return
      end if
  end do

  end function GetFaceConnection
!x------------------------------------------
  subroutine ConvertFromNeighbor(this, toCellNumber, fromCellNumber,         &
    fromLocalX, fromLocalY, fromLocalZ, newLocation)
  implicit none
  class(ModflowRectangularGridType) :: this
  integer,intent(in) :: toCellNumber,fromCellNumber
  doubleprecision,intent(in) :: fromLocalX,fromLocalY,fromLocalZ
  class(GridLocationType),intent(inout) :: newLocation
  integer :: status,faceNumber,subFaceNumber,count1,count2
  doubleprecision :: localX,localY,localZ,globalX,globalY,tol
  logical :: simpleFullFaceConnection
  
  call newLocation%Reset()
  status = 0
  
  ! Check to make sure the cell connection specified by fromCellNumber is actually a connection for
  ! the cell specified by toCellNumber
  call this%FindConnectionPosition(toCellNumber, fromCellNumber, faceNumber, subFaceNumber)
  
  select case (faceNumber)
    case (0)
      status = 1
    case (1)
      if(fromLocalX .ne. 1.0d0) status = 2
    case (2)
      if(fromLocalX .ne. 0d0) status = 2
    case (3)
      if(fromLocalY .ne. 1.0d0) status = 2
    case (4)
      if(fromLocalY .ne. 0d0) status = 2
    case (5)
      if(fromLocalZ .ne. 1.0d0) status = 2
    case (6)
      if(fromLocalZ .ne. 0d0) status = 2
    case default
      status = 3
  end select
  
  if(status .gt. 0) return
  
  
  count2 = this%GetPotentialFaceConnectionCount(toCellNumber, faceNumber)
  select case (faceNumber)
      case (1)
        count1 = this%GetPotentialFaceConnectionCount(fromCellNumber, 2)
      case (2)
        count1 = this%GetPotentialFaceConnectionCount(fromCellNumber, 1)
      case (3)
        count1 = this%GetPotentialFaceConnectionCount(fromCellNumber, 4)
      case (4)
        count1 = this%GetPotentialFaceConnectionCount(fromCellNumber, 3)
      case (5)
        count1 = this%GetPotentialFaceConnectionCount(fromCellNumber, 6)
      case (6)
        count1 = this%GetPotentialFaceConnectionCount(fromCellNumber, 5)
  end select
      
  simpleFullFaceConnection = .false.
  if((count1 .eq. 1) .and. (count2 .eq. 1)) simpleFullFaceConnection = .true.
  
  if(simpleFullFaceConnection) then
      newLocation%CellNumber = toCellNumber
      newLocation%LocalX = fromLocalX
      newLocation%LocalY = fromLocalY
      newLocation%LocalZ = fromLocalZ
      select case (faceNumber)
          case (1)
            newLocation%LocalX = 0.0d0
          case (2)
            newLocation%LocalX = 1.0d0
          case (3)
            newLocation%LocalY = 0.0d0
          case (4)
            newLocation%LocalY = 1.0d0
          case (5)
            newLocation%LocalZ = 0.0d0
          case (6)
            newLocation%LocalZ = 1.0d0
      end select
  else
      status = this%ConvertToModelXY(fromCellNumber, fromLocalX, fromLocalY, globalX, globalY)
      if(status .gt. 0) return
      status = this%ConvertToLocalXY(toCellNumber, globalX, globalY, localX, localY)
      if(status .gt. 0) return
      
      tol = 1.0d-3
      select case (faceNumber)
        case (1)
            call SnapToValue(localX, 0d0, tol)
        case (2)
            call SnapToValue(localX, 1.0d0, tol)
        case (3)
            call SnapToValue(localY, 0d0, tol)
        case (4)
            call SnapToValue(localY, 1.0d0, tol)
      end select
      
      newLocation%CellNumber = toCellNumber
      newLocation%LocalX = localX
      newLocation%LocalY = localY
      localZ = fromLocalZ
      if(faceNumber .eq. 5) localZ = 0.0d0
      if(faceNumber .eq. 6) localZ = 1.0d0
      newLocation%LocalZ = localZ
      
  end if
  
  newLocation%Layer = this%GetLayer(newLocation%CellNumber)
  
  

  
  end subroutine ConvertFromNeighbor
!x------------------------------------------
  subroutine FindConnectionPosition(this, cellNumber, conn, faceNumber, subFaceNumber)
  class(ModflowRectangularGridType) :: this
  integer,intent(in) :: cellNumber,conn
  integer,intent(inout) :: faceNumber,subFaceNumber
  integer :: count,n,face,index,faceFlag
  
  faceNumber = 0
  subFaceNumber = 0
  if((cellNumber .lt. 1) .or. (cellNumber .gt. this%CellCount)) return
  if(conn .eq. cellNumber) return
  
  index = this%FindConnectionIndex(cellNumber, conn)
  if(index .eq. 0) return
  
  faceFlag = this%JaFace(index)
  call this%ExtractConnectionInfo(faceFlag, faceNumber, subFaceNumber, count)
      
  end subroutine FindConnectionPosition
!x------------------------------------------
  function FindConnectionIndex(this, cellNumber, conn) result(index)
  class(ModflowRectangularGridType) :: this
  integer,intent(in) :: cellNumber,conn
  integer :: count, n, cn, index, offset
  
  index = 0
  offset = this%JaOffsets(cellNumber)
  count = this%JaOffsets(cellNumber + 1) - offset
  do n = 2, count
      if(this%Ja(offset + n) .eq. conn) then
          index = offset + n
          return
      end if
  end do
  
  end function FindConnectionIndex
!x------------------------------------------
  subroutine ConvertToModelZ(this, cellNumber, localZ, globalZ, useSaturatedTop)
  class(ModflowRectangularGridType) :: this
  integer,intent(in) :: cellNumber
  doubleprecision,intent(in) :: localZ
  doubleprecision,intent(inout) :: globalZ
  logical,intent(in) :: useSaturatedTop
  doubleprecision :: bottom,top
  
  end subroutine ConvertToModelZ
!x------------------------------------------
  subroutine ConvertToModelXYZ(this, cellNumber, localX, localY, localZ,    &
    globalX, globalY, globalZ)
  class(ModflowRectangularGridType) :: this
  integer,intent(in) :: cellNumber
  doubleprecision,intent(in) :: localX,localY,localZ
  doubleprecision,intent(inout) :: globalX,globalY,globalZ
  doubleprecision :: left,right,front,back,bottom,top
  
  call this%GetCellBoundaries(cellNumber, left, right, front, back)
  bottom = this%Bottom(cellNumber)
  top = this%SaturatedTop(cellNumber)
  
  globalX = ((1.0d0 - localX) * left) + (localX * right)
  if(globalX .lt. left) globalX = left
  if(globalX .gt. right) globalX = right
  
  globalY = ((1.0d0 - localY) * front) + (localY * back)
  if(globalY .lt. front) globalY = front
  if(globalY .gt. back) globalY = back
  
  globalZ = ((1.0d0 - localZ) * bottom) + (localZ * top)
  if(globalZ .lt. bottom) globalZ = bottom
  if(globalZ .gt. top) globalZ = top
  
  
  end subroutine ConvertToModelXYZ
!x------------------------------------------
  function ConvertToModelXY(this, cellNumber, localX, localY,               &
    globalX, globalY) result(status)
  class(ModflowRectangularGridType) :: this
  integer,intent(in) :: cellNumber
  doubleprecision,intent(in) :: localX,localY
  doubleprecision,intent(inout) :: globalX,globalY
  doubleprecision :: left,right,front,back
  integer :: status
  
  call this%GetCellBoundaries(cellNumber, left, right, front, back)
  
  globalX = ((1.0d0 - localX) * left) + (localX * right)
  if(globalX .lt. left) globalX = left
  if(globalX .gt. right) globalX = right
  
  globalY = ((1.0d0 - localY) * front) + (localY * back)
  if(globalY .lt. front) globalY = front
  if(globalY .gt. back) globalY = back

  status = 0

  
  end function ConvertToModelXY
!x------------------------------------------
  function ConvertToLocalXY(this, cellNumber, globalX, globalY,              &
    localX, localY) result(status)
  class(ModflowRectangularGridType) :: this
  integer,intent(in) :: cellNumber
  doubleprecision,intent(in) :: globalX,globalY
  doubleprecision,intent(inout) :: localX,localY
  doubleprecision :: left,right,front,back
  integer :: status
  
  call this%GetCellBoundaries(cellNumber, left, right, front, back)
  
  localX = (globalX - left) / (right - left)
  if(localX .gt. 1.0d0) localX = 1.0d0
  if(localX .lt. 0d0) localX = 0d0
  
  localY = (globalY - front) / (back - front)
  if(localY .gt. 1.0d0) localY = 1.0d0
  if(localY .lt. 0d0) localY = 0d0

  status = 0
  
  
  end function ConvertToLocalXY
!x------------------------------------------    
  subroutine GetCellConnectionFaces(this, cellNumber, buffer, bufferSize, cellConnectionCount)
  class(ModflowRectangularGridType) :: this
  integer,intent(in) :: cellNumber,bufferSize
  integer,intent(inout) :: cellConnectionCount
  integer,intent(inout),dimension(bufferSize) :: buffer
  integer :: n, offset
  
  do n = 1, bufferSize
      buffer(n) = -1
  end do
  
  offset = this%JaOffsets(cellNumber)
  cellConnectionCount = this%JaOffsets(cellNumber + 1) - offset
  
  do n = 1, cellConnectionCount
      buffer(n) = this%JaFace(offset + n)
  end do
  
  end subroutine GetCellConnectionFaces
 
!------------------------------------------
  subroutine SnapToValue(dataValue, snapValue, tolerance)
  implicit none
  doubleprecision,intent(inout) :: dataValue
  doubleprecision,intent(in) :: tolerance,snapValue
  doubleprecision :: min, max
  
  min = snapValue - (tolerance / 2.0d0)
  max = snapValue + (tolerance / 2.0d0)
  if( (dataValue .ge. min) .and. (dataValue .le. max) ) dataValue = snapValue
  
  end subroutine SnapToValue
  
end module ModflowRectangularGridModule