module RectangularGridDisvMf6Module
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
  type, public, extends(ModflowRectangularGridType) :: RectangularGridDisvMf6Type
    integer,private :: Nlay, Ncpl, Nvert, Njavert, Nja
    integer,private :: PotentialConnectionCount
    double precision :: RadAngle
    double precision,private :: DegToRad
    integer,private,dimension(:),allocatable :: IDomain
    integer,private,dimension(:),allocatable :: JaVertOffset, JaVert, JaVertRect
    logical,private :: HasClippedCells
    doubleprecision,private,dimension(:),allocatable :: VertX, VertY
  contains
    
    procedure :: CheckRectangular
    procedure :: GetIDomain
    procedure, private :: RectangularGridDisvMf6Init1
    generic :: ReadData => RectangularGridDisvMf6Init1
    
  end type

    contains
!---------------------------------------------------------------------
!   Initialization procedures 
!-------------------------------------------------------------------------    
  subroutine RectangularGridDisvMf6Init1(this, iin, gridMetaUnit, iout)
  implicit none
  class(RectangularGridDisvMf6Type) :: this
  integer,intent(in) :: iin, iout, gridMetaUnit

  ! Read binary grid file
  call ReadGRB(this, iin, iout)
  
  end subroutine RectangularGridDisvMf6Init1

  subroutine ReadGRB(grid, iin, iout)
  implicit none
  class(RectangularGridDisvMf6Type) :: grid
  integer,intent(in) :: iin, iout
  integer :: ncells, nlay, nrow, ncol, ncpl, nvert, njavert, idummy, offset
  integer :: version, ntxt, lentxt, ndat, ncom, lloc, istart, istop, n, i
  doubleprecision :: radAngle, xcell, ycell, r
  character*20 :: controlrecordflag
  character(len=50) :: headerLine
  character(len=100) :: line
  character(len=100),dimension(:),allocatable :: textLines
  logical ::  isSmoothedRectangular
  
!   Deallocate arrays if they have been allocated previously  
    call grid%Reset()

!   Read Header lines
    read(iin) headerLine
    lloc = 1
    call urword(headerLine, lloc, istart, istop, 0, n, r, iout, iin)
    if(headerLine(istart:istop) .eq. 'GRID') then
        call urword(headerLine, lloc, istart, istop, 0, n, r, iout, iin)
        if(headerLine(istart:istop) .ne. 'DISV') then 
            call grid%Reset()
            call ustop('Invalid binary grid type. Stop.')
        end if
    else        
        call grid%Reset()
        call ustop('Invalid binary grid file. Stop.')
    end if
  
    read(iin) headerLine
    lloc = 1 
    call urword(headerLine, lloc, istart, istop, 0, n, r, iout, iin)
    if(headerLine(istart:istop) .eq. 'VERSION') then
        call urword(headerLine, lloc, istart, istop, 2, version, r, iout, iin)
        if(version .ne. 1) then 
            call grid%Reset()
            call ustop('Unsupported binary grid file version. Stop.')
        end if
    else
        call grid%Reset()
        call ustop('Invalid binary grid file. Stop.')        
    end if
  
    read(iin) headerLine
    lloc = 1 
    call urword(headerLine, lloc, istart, istop, 0, n, r, iout, iin)
    if(headerLine(istart:istop) .eq. 'NTXT') then
        call urword(headerLine, lloc, istart, istop, 2, ntxt, r, iout, iin)
        !if(ntxt .ne. 20) then
        !    call grid%Reset()
        !    call ustop('Invalid binary grid file data (NTXT). Stop.')
        !end if
    else
        call grid%Reset()
        call ustop('Invalid binary grid file. Stop.')        
    end if
  
    read(iin) headerLine
    lloc = 1 
    call urword(headerLine, lloc, istart, istop, 0, n, r, iout, iin)
    if(headerLine(istart:istop) .eq. 'LENTXT') then
        call urword(headerLine, lloc, istart, istop, 2, lentxt, r, iout, iin)
        if(lentxt .ne. 100) then
            call grid%Reset()
            call ustop('Invalid binary grid file data (LENTXT). Stop.')
        end if
    else
        call grid%Reset()
        call ustop('Invalid binary grid file. Stop.')        
    end if
    
! Read data variable definition text lines
    allocate(textLines(ntxt))
    ncom = 0
    do n = 1, ntxt
        read(iin) line
        if(line(1:1) .eq. '#') then
            textLines(n) = '#'
            ncom = ncom + 1
        else
            lloc = 1
            call urword(line, lloc, istart, istop, 0, n, r, iout, iin)
            textlines(n) = line(istart:istop)
        end if
    end do
    
! Read data variables
    ndat = 0
    do n = 1, ntxt
        line = trim(textLines(n))
        select case(line)
            case ('#')
                ! skip over
            case ('NCELLS')
                ndat = ndat + 1
                read(iin) grid%CellCount
            case ('NLAY')
                ndat = ndat + 1
                read(iin) grid%Nlay
                grid%LayerCount = grid%Nlay
            case ('NCPL')
                ndat = ndat + 1
                read(iin) grid%Ncpl
            case ('ICELLTYPE')
                ndat = ndat + 1
                allocate(grid%CellType(grid%CellCount))
                do i = 1, grid%CellCount
                    read(iin) grid%CellType(i)
                end do
            case ('NVERT')
                ndat = ndat + 1
                read(iin) grid%Nvert
            case ('NJAVERT')
                ndat = ndat + 1
                read(iin) grid%Njavert
            case ('NJA')
                ndat = ndat + 1
                read(iin) grid%JaCount
            case ('TOP')
                ndat = ndat + 1
                allocate(grid%Top(grid%CellCount))
                read(iin) (grid%Top(i), i = 1, grid%Ncpl)
            case ('BOTM')
                ndat = ndat + 1
                allocate(grid%Bottom(grid%CellCount))
                read(iin) (grid%Bottom(i), i = 1, grid%CellCount)
            case ('VERTICES')
                ndat = ndat + 1
                allocate(grid%VertX(grid%Nvert))
                allocate(grid%VertY(grid%Nvert))
                do i = 1, grid%Nvert
                    read(iin) grid%VertX(i)
                    read(iin) grid%VertY(i)
                end do
            case ('CELLX')
                ndat = ndat + 1
                allocate(grid%CellX(grid%CellCount))
                do i = 1, grid%Ncpl
                    read(iin) grid%CellX(i)
                end do
            case ('CELLY')
                ndat = ndat + 1
                allocate(grid%CellY(grid%CellCount))
                do i = 1, grid%Ncpl
                    read(iin) grid%CellY(i)
                end do
            case ('IAVERT')
                ndat = ndat + 1
                allocate(grid%JavertOffset(grid%Ncpl + 1))
                !read(iin) (grid%JavertOffset(i), i = 1, grid%Ncpl + 1)
                do i = 1, grid%Ncpl + 1
                    read(iin) offset
                    grid%JavertOffset(i) = offset - 1
                end do
            case ('JAVERT')
                ndat = ndat + 1
                allocate(grid%Javert(grid%Njavert))
                read(iin) (grid%Javert(i), i = 1, grid%Njavert)
            case ('IA')
                ndat = ndat + 1
                allocate(grid%JaOffsets(grid%CellCount + 1))
                ! Subtract 1 from all array elements to convert the IA array pointers to 0-based offsets
                do i = 1, grid%CellCount + 1
                    read(iin) offset
                    grid%JaOffsets(i) = offset - 1
                end do
            case ('JA')
                ndat = ndat + 1
                allocate(grid%Ja(grid%JaCount))
                read(iin) (grid%Ja(i), i = 1, grid%JaCount)
            case ('IDOMAIN')
                ndat = ndat + 1
                allocate(grid%IDomain(grid%CellCount))
                read(iin) (grid%IDomain(i), i = 1, grid%CellCount)
            case ('XORIGIN')
                ndat = ndat + 1
                read(iin) grid%OriginX
            case ('YORIGIN')
                ndat = ndat + 1
                read(iin) grid%OriginY
            case ('ANGROT')
                ndat = ndat + 1
                read(iin) grid%RotationAngle
            case default
                ! do nothing
        end select
        
    end do
    
! Check to make sure all the required data variables were read    
    n = ndat + ncom
    if(n .ne. ntxt) then
        call grid%Reset()
        call ustop('Error reading binary grid file. Stop.')
    end if
    
!   Set grid type flag
    grid%GridType = 4

!   Allocate arrays
    allocate(grid%SaturatedTop(grid%CellCount))
    allocate(grid%LayerOffsets(grid%LayerCount + 1))
    
    grid%LayerOffsets(1) = 0
    do n = 1, grid%LayerCount
        grid%LayerOffsets(n + 1) = grid%LayerOffsets(n) + grid%Ncpl
    end do
    
    ! Compute the tops all layers other than layer 1
    do n = 2, grid%LayerCount
        offset = (n - 1) * grid%Ncpl
        do i = 1, grid%Ncpl
            grid%Top(offset + i) = grid%Bottom(offset + i - grid%Ncpl)
        end do
    end do
    
!   Check for rectangular cell structure
    call CheckRectangular(grid, isSmoothedRectangular)
    if( .not. isSmoothedRectangular) then
        call ustop('The binary grid contains data that is inconsistent with a rectangular DISV grid. Stop.')        
    end if
    
    ! Assign connections to cell faces
    call grid%ComputeFaceAssignments()
    
  
  end subroutine ReadGRB
  
!---------------------------------------------------------------------
!   Overriden procedures 
!----------------------------------------------------------------------
!----------------------------------------------------------------------
  function GetGridType(this) result(gridType)
  implicit none
  class(RectangularGridDisvMf6Type) :: this
  integer :: gridType
  
  gridType = 4
  
  end function GetGridType
!x------------------------------------------
  function GetIDomain(this, cellNumber) result(fval)
    class(RectangularGridDisvMf6Type) :: this
    integer,intent(in) :: cellNumber
    integer :: fval
    
    fval = this%Idomain(cellNumber)
    
  end function GetIDomain
!------------------------------------------------------------------------
!  End of overridden procedures
!------------------------------------------------------------------------

  function Distance(x1, y1, x2, y2) result(d)
  doubleprecision, intent(in) :: x1, x2, y1, y2
  doubleprecision :: d
  
  d = sqrt((x2 - x1)**2 + (y2 - y1)**2)
  
  end function Distance
  
  function IsColinear(x1, y1, x2, y2, x3, y3, tol) result(fval)
  doubleprecision, intent(in) :: x1, x2, x3, y1, y2, y3, tol
  doubleprecision :: d1, d2, d3
  doubleprecision :: delta
  logical :: fval
  
  d1 = Distance(x1, y1, x2, y2)
  d2 = Distance(x2, y2, x3, y3)
  d3 = Distance(x1, y1, x3, y3)
  
  fval = .false.
  delta = abs((d3 - d1 - d2) / d3)
  if(delta .le. tol) fval = .true.
  
  end function IsColinear
  

  subroutine CheckRectangular(this, isSmoothedRectangular)
  implicit none
  class(RectangularGridDisvMf6Type) :: this
  logical,intent(inout) :: isSmoothedRectangular
  integer :: m, n, i, k, cellPtCount, offset, rectOffset, ptCount, rectCount, v1, v2, v3, nJavert, midFaceCount
  integer :: failedCount
  integer,dimension(:),allocatable :: failedCellNumbers
  integer,dimension(4) :: tempVert, jx, jy
  integer,dimension(16) :: ix, iy
  doubleprecision :: d, x1, x2, x3, y1, y2, y3, tol, dx, dy
  logical :: isRectangular, isSmoothed, validAlignment
  data ix /1, 0, -1, 0, 0, -1, 0, 1, -1, 0, 1, 0, 0, 1, 0, -1/
  data iy /0, -1, 0, 1, -1, 0, 1, 0, 0, 1, 0, -1, 1, 0, -1, 0/
  
  isSmoothedRectangular = .false.
  isRectangular = .false.
  isSmoothed = .false.
  dx = 0.0
  dy = 0.0
  
  if( .not. allocated(this%JavertOffset)) return
  if( .not. allocated(this%Javert)) return
  if( .not. allocated(this%VertX)) return
  if( .not. allocated(this%VertY)) return
  if(allocated(this%JavertRect)) deallocate(this%JavertRect)
  allocate(this%JavertRect(4 * this%CellCount))
  allocate(failedCellNumbers(this%Ncpl))
  failedCount = 0
  do n = 1, this%Ncpl
      failedCellNumbers(n) = 0
  end do
  
  nJavert = 4 * this%CellCount
  do n = 1, nJavert
      this%JavertRect(n) = 0
  end do
  
  tol = 0.00001
  rectCount = 0
  isSmoothed = .true.
  do n = 1, this%Ncpl
      offset = this%JavertOffset(n)
      rectOffset = (n - 1)*4
      cellPtCount = this%JavertOffset(n + 1) - offset
      
      ! Step 1:
      !
      ! Loop through points to find all corner points. Count the mid-face points
      ! for all the cell sides defined by the corner points. Use the mid-face point
      ! counts to determine if the cell meets the definition of a smoothed grid.
      ! Smoothed grids have no more than one mid-point per cell face.
      ptCount = 1
      midFaceCount = 0
      this%JavertRect(rectOffset + 1) = this%Javert(offset + 1)
      do i = 2, cellPtCount -1
          v1 = this%Javert(offset + i -1)
          x1 = this%VertX(v1)
          y1 = this%VertY(v1)
          
          v2 = this%Javert(offset + i)
          x2 = this%VertX(v2)
          y2 = this%VertY(v2)
          
          v3 = this%Javert(offset + i + 1)
          x3 = this%VertX(v3)
          y3 = this%VertY(v3)
          
          if( .not. IsColinear(x1,y1, x2,y2, x3,y3, tol)) then
            ptCount = ptCount + 1
            if(ptCount .le. 4) this%JavertRect(rectOffset + ptCount) = v2
            ! If there is more than 1 mid-face point it means that a cell face is connected
            ! to more than two other cells. In that case, the grid is not smoothed.
            if(midFaceCount .gt. 1) isSmoothed = .false.
            ! Reset the mid-face count to 0
            midFaceCount = 0
          else
            ! This is a mid-face point, so increment the mid-face counter
            midFaceCount = midFaceCount + 1
          end if
      end do
      ptCount = ptCount + 1
      
      ! Step 2:
      !
      ! If the cell has 5 corner points it is a quadrilateral (points 1 and 5 are the same). 
      ! If so, continue checking to see if it is a rectangle aligned with the x-y axes.
      if(ptCount .eq. 5) then
          ! Check for rectangularity and alignment with the x-y axes
          ! Check side 1-2
          dy = this%VertY(this%JavertRect(rectOffset + 2)) - this%VertY(this%JavertRect(rectOffset + 1))
          dx = this%VertX(this%JavertRect(rectOffset + 2)) - this%VertX(this%JavertRect(rectOffset + 1))
          jx(1) = 0
          jy(1) = 0
          if(dy .gt. tol) jy(1) = 1
          if(dy .lt. -tol) jy(1) = -1
          if(dx .gt. tol) jx(1) = 1
          if(dx .lt. -tol) jx(1) = -1
          ! Check side 2-3
          dy = this%VertY(this%JavertRect(rectOffset + 3)) - this%VertY(this%JavertRect(rectOffset + 2))
          dx = this%VertX(this%JavertRect(rectOffset + 3)) - this%VertX(this%JavertRect(rectOffset + 2))
          jx(2) = 0
          jy(2) = 0
          if(dy .gt. tol) jy(2) = 1
          if(dy .lt. -tol) jy(2) = -1
          if(dx .gt. tol) jx(2) = 1
          if(dx .lt. -tol) jx(2) = -1
          ! Check side 3-4
          dy = this%VertY(this%JavertRect(rectOffset + 4)) - this%VertY(this%JavertRect(rectOffset + 3))
          dx = this%VertX(this%JavertRect(rectOffset + 4)) - this%VertX(this%JavertRect(rectOffset + 3))
          jx(3) = 0
          jy(3) = 0
          if(dy .gt. tol) jy(3) = 1
          if(dy .lt. -tol) jy(3) = -1
          if(dx .gt. tol) jx(3) = 1
          if(dx .lt. -tol) jx(3) = -1
          ! Check side 4-1
          dy = this%VertY(this%JavertRect(rectOffset + 1)) - this%VertY(this%JavertRect(rectOffset + 4))
          dx = this%VertX(this%JavertRect(rectOffset + 1)) - this%VertX(this%JavertRect(rectOffset + 4))
          jx(4) = 0
          jy(4) = 0
          if(dy .gt. tol) jy(4) = 1
          if(dy .lt. -tol) jy(4) = -1
          if(dx .gt. tol) jx(4) = 1
          if(dx .lt. -tol) jx(4) = -1
          
          do m = 1, 4
              k = 0
              validAlignment = .true.
              do i = 1, 4
                  if(jx(i) .ne. ix(4*(m - 1) + i)) validAlignment = .false.
                  if(jy(i) .ne. iy(4*(m - 1) + i)) validAlignment = .false.
                  if((i .eq. 4)) then
                      if(validAlignment) then
                          k = m
                          exit
                      end if
                  end if
              end do
              if(k .gt. 0) exit
          end do
          
          ! If k > 0 means that the cell is a rectangle aligned with the x-y axes. If so, increment the valid cell count
          ! and reorder the vertex points of the cell, if necessary, so that the first vertex point is the vertex
          ! corresponding to the upper left corner of the cell. 
          if(k .gt. 0) then
            
            ! The value of k is the element number of the vertex point that is the upper left corner of the cell. 
            ! The calculations in step 3 below require the upper left corner point to be the first point in the 
            ! list for the cell. If k > 1, construct a temporary array containing the new vertex order that can
            ! be used to reorder the JavertRect elements for the cell.
            select case (k)
            case (2)
                tempVert(1) = this%JavertRect(rectOffset + 2)
                tempVert(2) = this%JavertRect(rectOffset + 3)
                tempVert(3) = this%JavertRect(rectOffset + 4)
                tempVert(4) = this%JavertRect(rectOffset + 1)
            case (3)
                tempVert(1) = this%JavertRect(rectOffset + 3)
                tempVert(2) = this%JavertRect(rectOffset + 4)
                tempVert(3) = this%JavertRect(rectOffset + 1)
                tempVert(4) = this%JavertRect(rectOffset + 2)
            case (4)
                tempVert(1) = this%JavertRect(rectOffset + 4)
                tempVert(2) = this%JavertRect(rectOffset + 1)
                tempVert(3) = this%JavertRect(rectOffset + 2)
                tempVert(4) = this%JavertRect(rectOffset + 3)
            case default
            ! no reordering is required
            end select
            
            ! If k > 1, use the tempVert array to assign the new vertex order for the cell to the
            ! elements of the JavertRect array
            if(k .gt. 1) then
                do i = 1, 4
                    this%JavertRect(rectOffset + i) = tempVert(i)
                end do
            end if
            
            ! Increment rectCount to indicate that the cell is a rectangle aligned with the x-y axes.
            rectCount = rectCount + 1
            
          else
              failedCount = failedCount +1
              failedCellNumbers(failedCount) = n
          end if
      else
              failedCount = failedCount +1
              failedCellNumbers(failedCount) = n
      end if
      
  end do
  
  ! Step 3:
  !
  ! If all of the cells pass the rectangularity check, finish processing the grid data.
  if(rectCount .eq. this%Ncpl) then
      isRectangular = .true.
     
     ! Allocate DelX and DelY arrays. Also recompute CellX and CellY from the vertex coordinates to
     ! be sure the CellX and CellY are exactly consistent with the vertex coordinates (which may have
     ! been rotated. This also assures that the cell coordinates represents cell-centered node points.
     allocate(this%DelX(this%CellCount))
     allocate(this%DelY(this%CellCount))
     do i = 1, this%Nlay
         if(i .eq. 1) then
             do n = 1, this%Ncpl
                 x1 = this%VertX(this%JavertRect(4*(n - 1) + 1))
                 x2 = this%VertX(this%JavertRect(4*(n - 1) + 2))
                 this%DelX(n) = x2 - x1
                 this%CellX(n) = (x1 + x2) / 2.0
                 y1 = this%VertY(this%JavertRect(4*(n - 1) + 4))
                 y2 = this%VertY(this%JavertRect(4*(n - 1) + 1))
                 this%DelY(n) = y2 - y1
                 this%CellY(n) = (y1 + y2) / 2.0
             end do
         else
             offset = (i - 1) * this%Ncpl
             do n = 1, this%Ncpl
                 this%DelX(offset + n) = this%DelX(n)
                 this%DelY(offset + n) = this%DelY(n)
                 this%CellX(offset + n) = this%CellX(n)
                 this%CellY(offset + n) = this%CellY(n)
             end do
         end if
     end do
     
  end if
  
  ! Step 4:
  !
  ! Set the isSmoothedRectangular flag based on the results of the check
  if(isRectangular .and. isSmoothed) isSmoothedRectangular = .true.

  ! Step 5:
  !
  ! Deallocate grid arrays that are no longer needed
  deallocate(this%JaVert)
  deallocate(this%JaVertOffset)
  deallocate(this%JaVertRect)
  deallocate(this%VertX)
  deallocate(this%VertY)
  
  end subroutine CheckRectangular



end module RectangularGridDisvMf6Module