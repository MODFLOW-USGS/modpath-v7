module RectangularGridDisuMf6Module
  use UTL8MODULE
  use ModflowRectangularGridModule
  use GridLocationModule,only : GridLocationType
  
  implicit none
  
! Set default access status to private
  private
    
! Public derived data type definitions
!--------------------------------------
! type: 
!--------------------------------------
  type, public, extends(ModflowRectangularGridType) :: RectangularGridDisuMf6Type
  ! Variables
  contains
  ! Procedures
  end type

  contains
!---------------------------------------------------------------------
!   Overriden procedures 
!----------------------------------------------------------------------
  function GetGridType(this) result(gridType)  
  class(RectangularGridDisuMf6Type) :: this
  integer :: gridType
  
  gridType = 5
  
  end function GetGridType
!x------------------------------------------
  function GetReducedCellConnectionOffset(this,cellNumber) result(offset)
  implicit none
  class(RectangularGridDisuMf6Type) :: this
  integer,intent(in) :: cellNumber
  integer :: offset
  
  ! Add override code to implement
  offset = 0
  
  end function
!x------------------------------------------
  subroutine ComputeReducedCellConnections(this,cellNumber,buffer,bufferSize,reducedConnectionCount)
  implicit none
  class(RectangularGridDisuMf6Type) :: this
  integer,intent(in) :: cellNumber,bufferSize
  integer,intent(inout),dimension(bufferSize) :: buffer
  integer,intent(inout) :: reducedConnectionCount
  integer :: n,face,conn,count,faceConnCount
  
  ! Add override code to implement
  
  end subroutine ComputeReducedCellConnections
!x------------------------------------------
  function   ComputeReducedCellConnCount(this,cellNumber) result(count)
  implicit none
  class(RectangularGridDisuMf6Type) :: this
  integer,intent(in) :: cellNumber
  integer :: count,face,conn,faceConnCount,n
  
  ! Add override code to implement
  count = 0
  
  end function ComputeReducedCellConnCount
!x------------------------------------------
  function   ComputePotentialCellConnCount(this,cellNumber) result(count)
  implicit none
  class(RectangularGridDisuMf6Type) :: this
  integer,intent(in) :: cellNumber
  integer :: count,face
  
  ! Add override code to implement
  count = 0
  
  end function ComputePotentialCellConnCount
!!x------------------------------------------
!  function GetReducedCellConnectionCount(this,cellNumber) result(count)
!  implicit none
!  class(RectangularGridDisuMf6Type) :: this
!  integer,intent(in) :: cellNumber
!  integer :: count
!  
!  ! Add override code to implement
!  count = 0
!  
!  end function GetReducedCellConnectionCount
!x------------------------------------------
  function GetPotentialCellConnectionCount(this,cellNumber) result(count)
  implicit none
  class(RectangularGridDisuMf6Type) :: this
  integer,intent(in) :: cellNumber
  integer :: count
  
  ! Add override code to implement
  count = 0
  
  end function GetPotentialCellConnectionCount
!x------------------------------------------  
  subroutine Reset(this)
  implicit none
  class(RectangularGridDisuMf6Type) :: this
  
  ! Add override code to implement
  
  end subroutine Reset
!x------------------------------------------
  function GetCellCount(this) result(fval)
    class(RectangularGridDisuMf6Type) :: this
    integer :: fval
    
  ! Add override code to implement
    fval = this%CellCount
    
  end function GetCellCount
!!x------------------------------------------
!  function GetLayerCount(this) result(fval)
!    class(RectangularGridDisuMf6Type) :: this
!    integer :: fval
!    
!  ! Add override code to implement
!    fval = 0
!    
!  end function GetLayerCount
!x------------------------------------------
  function GetIDomain(this, cellNumber) result(fval)
    class(RectangularGridDisuMf6Type) :: this
    integer,intent(in) :: cellNumber
    integer :: fval
    
  ! Add override code to implement
    fval = 0
    
  end function GetIDomain
!!x------------------------------------------
!  function GetHasClippedCells(this) result(fval)
!    class(RectangularGridDisuMf6Type) :: this
!    logical :: fval
!    
!  ! Add override code to implement
!    fval = 0
!    
!  end function GetHasClippedCells
!x------------------------------------------
  function GetLayer(this, cellNumber) result(fval)
  class(RectangularGridDisuMf6Type) :: this
  integer :: cellNumber, fval
  
  ! Add override code to implement
  fval = 0
  
  end function GetLayer
!x------------------------------------------
  function GetX(this, cellNumber) result(fval)
  class(RectangularGridDisuMf6Type) :: this
  integer :: cellNumber
  integer :: layerCellNumber
  double precision :: fval
  
  ! Add override code to implement
  fval = 0.0
  
  end function GetX
!x------------------------------------------
  function GetY(this, cellNumber) result(fval)
  class(RectangularGridDisuMf6Type) :: this
  integer :: cellNumber
  integer :: layerCellNumber
  double precision :: fval
  
  ! Add override code to implement
  fval = 0.0
  
  end function GetY
!x------------------------------------------
  function GetTop(this, cellNumber) result(fval)
  class(RectangularGridDisuMf6Type) :: this
  integer :: cellNumber
  double precision :: fval
  
  ! Add override code to implement
  fval = 0
  
  end function GetTop
!x------------------------------------------
  function GetSaturatedTop(this, cellNumber) result(fval)
  class(RectangularGridDisuMf6Type) :: this
  integer :: cellNumber
  double precision :: fval
  
  ! Add override code to implement
  fval = 0.0
  
  end function GetSaturatedTop
!x------------------------------------------
  subroutine SetSaturatedTop(this, cellNumber, saturatedTop)
  class(RectangularGridDisuMf6Type) :: this
  integer :: cellNumber
  double precision :: saturatedTop
  
  ! Add override code to implement
  
  end subroutine SetSaturatedTop
!x------------------------------------------
  function GetBottom(this, cellNumber) result(fval)
  class(RectangularGridDisuMf6Type) :: this
  integer :: cellNumber
  double precision :: fval
  
  ! Add override code to implement
  fval = 0.0
  
  end function GetBottom
!x------------------------------------------
  function GetLeft(this, cellNumber) result(fval)
  class(RectangularGridDisuMf6Type) :: this
  integer :: cellNumber
  double precision :: fval
  
  ! Add override code to implement
  fval = 0.0
  
  end function GetLeft
!x------------------------------------------
  function GetRight(this, cellNumber) result(fval)
  class(RectangularGridDisuMf6Type) :: this
  integer :: cellNumber
  double precision :: fval
  
  ! Add override code to implement
  fval = 0.0
  
  end function GetRight
!x------------------------------------------
  function GetFront(this, cellNumber) result(fval)
  class(RectangularGridDisuMf6Type) :: this
  integer :: cellNumber
  double precision :: fval
  
  ! Add override code to implement
  fval = 0.0
  
  end function GetFront
!x------------------------------------------
  function GetBack(this, cellNumber) result(fval)
  class(RectangularGridDisuMf6Type) :: this
  integer :: cellNumber
  double precision :: fval
  
  ! Add override code to implement
  fval = 0.0
  
  end function GetBack
!x------------------------------------------
  function GetDX(this, cellNumber) result(fval)
  class(RectangularGridDisuMf6Type) :: this
  integer,intent(in) :: cellNumber
  integer :: layerCellNumber
  double precision :: fval
  
  ! Add override code to implement
  fval = 0.0
  
  end function GetDX
!x------------------------------------------
  function GetDY(this, cellNumber) result(fval)
  class(RectangularGridDisuMf6Type) :: this
  integer,intent(in) :: cellNumber
  integer :: layerCellNumber
  double precision :: fval
  
  fval = 0.0
  
  end function GetDY
!!x------------------------------------------
!  function GetLayerCellCount(this, layer) result(fval)
!  class(RectangularGridDisuMf6Type) :: this
!  integer,intent(in) :: layer
!  double precision :: fval
!  
!  fval = this%LayerCellCounts(layer)
!  
!  end function GetLayerCellCount
!x------------------------------------------
  function GetLayerCellCountOffset(this, layer) result(fval)
  class(RectangularGridDisuMf6Type) :: this
  integer,intent(in) :: layer
  double precision :: fval
  
  ! Add override code to implement
  fval = 0
  
  end function GetLayerCellCountOffset
!x------------------------------------------
  function GetPotentialConnectionCount(this) result(count)
  class(RectangularGridDisuMf6Type) :: this
  integer :: count
  
  ! Add override code to implement
  count = 0
  
  end function GetPotentialConnectionCount
!!x------------------------------------------
!  function GetReducedConnectionCount(this) result(count)
!  class(RectangularGridDisuMf6Type) :: this
!  integer :: count,n
!  
!  ! Add override code to implement
!  count = 0
!  
!  end function GetReducedConnectionCount
!x------------------------------------------
  function GetPotentialFaceConnectionCount(this, cellNumber, face) result(fval)
  class(RectangularGridDisuMf6Type) :: this
  integer,intent(in) :: cellNumber,face
  integer :: fval

  ! Add override code to implement
  fval = 0
  
  end function GetPotentialFaceConnectionCount
!x------------------------------------------
  function GetFaceConnection(this, cellNumber, face, subface) result(conn)
  class(RectangularGridDisuMf6Type) :: this
  integer,intent(in) :: cellNumber,face,subface
  integer :: conn,offset

  ! Add override code to implement
  conn = -1
  
  end function GetFaceConnection
!x------------------------------------------
  subroutine ConvertFromNeighbor(this, toCellNumber, fromCellNumber,         &
    fromLocalX, fromLocalY, fromLocalZ, newLocation)
  implicit none
  class(RectangularGridDisuMf6Type) :: this
  integer,intent(in) :: toCellNumber,fromCellNumber
  doubleprecision,intent(in) :: fromLocalX,fromLocalY,fromLocalZ
  class(GridLocationType),intent(inout) :: newLocation
  integer :: status,faceNumber,subFaceNumber
  doubleprecision :: localX,localY,localZ,globalX,globalY,tol
  
  ! Add override code to implement
  
  end subroutine ConvertFromNeighbor
!x------------------------------------------
  subroutine FindConnectionPosition(this, cellNumber, conn, faceNumber, subFaceNumber)
  class(RectangularGridDisuMf6Type) :: this
  integer,intent(in) :: cellNumber,conn
  integer,intent(inout) :: faceNumber,subFaceNumber
  integer :: count,n,face
  
  faceNumber = 0
  subFaceNumber = 0
  
  do face = 1, 6
      count = this%GetPotentialFaceConnectionCount(cellNumber, face)
      if(count .gt. 0) then
        do n = 1, count
          if(this%GetFaceConnection(cellNumber, face, n) .eq. conn) then
            faceNumber = face
            subFaceNumber = n
            return
          end if
        end do
      end if
  end do
  
  end subroutine FindConnectionPosition
!x------------------------------------------
  function FindConnectionIndex(this, cellNumber, conn) result(index)
  class(RectangularGridDisuMf6Type) :: this
  integer,intent(in) :: cellNumber,conn
  integer :: count, n, cn, index, offset
  
  ! Add override code to implement
  index = 0
  
  end function FindConnectionIndex
!!x------------------------------------------
!  subroutine ConvertToModelZ(this, cellNumber, localZ, globalZ, useSaturatedTop)
!  class(RectangularGridDisuMf6Type) :: this
!  integer,intent(in) :: cellNumber
!  doubleprecision,intent(in) :: localZ
!  doubleprecision,intent(inout) :: globalZ
!  logical,intent(in) :: useSaturatedTop
!  doubleprecision :: bottom,top
!  
!  bottom = this%GetBottom(cellNumber)
!  if(useSaturatedTop) then
!      top = this%GetSaturatedTop(cellNumber)
!  else
!      top = this%GetTop(cellNumber)      
!  end if
!  
!  globalZ = ((1.0d0 - localZ) * bottom) + (localZ * top)
!  if(globalZ .lt. bottom) globalZ = bottom
!  if(globalZ .gt. top) globalZ = top
!  
!  end subroutine ConvertToModelZ
!!x------------------------------------------
!  subroutine ConvertToModelXYZ(this, cellNumber, localX, localY, localZ,    &
!    globalX, globalY, globalZ)
!  class(RectangularGridDisuMf6Type) :: this
!  integer,intent(in) :: cellNumber
!  doubleprecision,intent(in) :: localX,localY,localZ
!  doubleprecision,intent(inout) :: globalX,globalY,globalZ
!  doubleprecision :: left,right,front,back,bottom,top
!  
!  left = this%GetLeft(cellNumber)
!  right = this%GetRight(cellNumber)
!  front = this%GetFront(cellNumber)
!  back = this%GetBack(cellNumber)
!  bottom = this%GetBottom(cellNumber)
!  top = this%GetSaturatedTop(cellNumber)
!  
!  globalX = ((1.0d0 - localX) * left) + (localX * right)
!  if(globalX .lt. left) globalX = left
!  if(globalX .gt. right) globalX = right
!  
!  globalY = ((1.0d0 - localY) * front) + (localY * back)
!  if(globalY .lt. front) globalY = front
!  if(globalY .gt. back) globalY = back
!  
!  globalZ = ((1.0d0 - localZ) * bottom) + (localZ * top)
!  if(globalZ .lt. bottom) globalZ = bottom
!  if(globalZ .gt. top) globalZ = top
!  
!  end subroutine ConvertToModelXYZ
!!x------------------------------------------
!  function ConvertToModelXY(this, cellNumber, localX, localY,               &
!    globalX, globalY) result(status)
!  class(RectangularGridDisuMf6Type) :: this
!  integer,intent(in) :: cellNumber
!  doubleprecision,intent(in) :: localX,localY
!  doubleprecision,intent(inout) :: globalX,globalY
!  doubleprecision :: left,right,front,back
!  integer :: status
!  
!  left = this%GetLeft(cellNumber)
!  right = this%GetRight(cellNumber)
!  front = this%GetFront(cellNumber)
!  back = this%GetBack(cellNumber)
!  
!  globalX = ((1.0d0 - localX) * left) + (localX * right)
!  if(globalX .lt. left) globalX = left
!  if(globalX .gt. right) globalX = right
!  
!  globalY = ((1.0d0 - localY) * front) + (localY * back)
!  if(globalY .lt. front) globalY = front
!  if(globalY .gt. back) globalY = back
!
!  status = 0
!  
!  end function ConvertToModelXY
!!x------------------------------------------
!  function ConvertToLocalXY(this, cellNumber, globalX, globalY,              &
!    localX, localY) result(status)
!  class(RectangularGridDisuMf6Type) :: this
!  integer,intent(in) :: cellNumber
!  doubleprecision,intent(in) :: globalX,globalY
!  doubleprecision,intent(inout) :: localX,localY
!  doubleprecision :: left,right,front,back
!  integer :: status
!  
!  left = this%GetLeft(cellNumber)
!  right = this%GetRight(cellNumber)
!  front = this%GetFront(cellNumber)
!  back = this%GetBack(cellNumber)
!  
!  localX = (globalX - left) / (right - left)
!  if(localX .gt. 1.0d0) localX = 1.0d0
!  if(localX .lt. 0d0) localX = 0d0
!  
!  localY = (globalY - front) / (back - front)
!  if(localY .gt. 1.0d0) localY = 1.0d0
!  if(localY .lt. 0d0) localY = 0d0
!
!  status = 0
!  
!    end function ConvertToLocalXY

!------------------------------------------------------------------------
!  End of overridden procedures
!------------------------------------------------------------------------

    
    
end module RectangularGridDisuMf6Module