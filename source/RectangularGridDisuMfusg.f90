module RectangularGridDisuMfusgModule
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
  type, public, extends(ModflowRectangularGridType) :: RectangularGridDisuMfusgType
  ! Variables
  contains
  ! Procedures
    procedure, private :: RectangularGridDisuMfusgInit1
    generic :: ReadData => RectangularGridDisuMfusgInit1
  end type

    contains
!---------------------------------------------------------------------
!   Overriden procedures 
!----------------------------------------------------------------------
  function GetGridType(this) result(gridType)
  class(RectangularGridDisuMfusgType) :: this
  integer :: gridType
  
  gridType = 2
  
  end function GetGridType
!x------------------------------------------  
  subroutine Reset(this)
  implicit none
  class(RectangularGridDisuMfusgType) :: this
  
  ! Add override code to implement
  
  end subroutine Reset
!x------------------------------------------

!------------------------------------------------------------------------
!  End of overridden procedures
!------------------------------------------------------------------------

!------------------------------------------------------------------------
!  Begin initialization procedures
!------------------------------------------------------------------------
  subroutine RectangularGridDisuMfusgInit1(this, inUnit, gridMetaUnit, outUnit, stressPeriodCount)
  class(RectangularGridDisuMfusgType) :: this
  integer, intent(in) :: inUnit, outUnit, gridMetaUnit
  integer, intent(inout) :: stressPeriodCount
  
! Read the MODFLOW-USG DISU file
  call ReadDISU(this, stressPeriodCount, inUnit, outUnit)
  
! Read the GridMeta file
  if(gridMetaUnit .gt. 0) then
      call ReadGridMeta(this, gridMetaUnit, outUnit)
  else
      call ustop('The GRIDMETA file was not specified. Stop.')
  end if
    
! Assign connections to cell faces
  call this%ComputeFaceAssignments()
  
  end subroutine RectangularGridDisuMfusgInit1  
!------------------------------------------------------------------------
!  End initialization procedures
!------------------------------------------------------------------------

!------------------------------------------
  subroutine ReadDISU(grid, stressPeriodCount, iin, iout)
  implicit none
  class(RectangularGridDisuMfusgType),intent(inout) :: grid
  integer,intent(in) :: iin, iout
  integer,intent(inout) :: stressPeriodCount
  integer,dimension(:),allocatable :: iac, iBuffer, laycbd, nodelay
  integer :: njags, ivsd, itmuni, lenuni, idsymrd, ierr, lloc, istart, istop, n, offset
  doubleprecision :: r
  doubleprecision,dimension(:),allocatable :: rBuffer
  character*20 :: controlrecordflag
  character (len=24) :: aname
  character (len=200) :: line

!   Set grid type
    grid%GridType = 2
    
    write(iout, *)
    write(iout, '(1x,a)') 'Reading MODFLOW-USG unstructured discretization file (DISU):'
!   Read and write comment lines. Return the first non-comment line in variable "line".
    call u8rdcom(iin,iout,line,ierr)
    lloc = 1
    call urword(line, lloc, istart, istop, 2, grid%CellCount, r, iout, iin)
    call urword(line, lloc, istart, istop, 2, grid%LayerCount, r, iout, iin)
    call urword(line, lloc, istart, istop, 2, grid%JaCount, r, iout, iin)
    call urword(line, lloc, istart, istop, 2, ivsd, r, iout, iin)
    call urword(line, lloc, istart, istop, 2, stressPeriodCount, r, iout, iin)
    call urword(line, lloc, istart, istop, 2, itmuni, r, iout, iin)
    call urword(line, lloc, istart, istop, 2, lenuni, r, iout, iin)
    call urword(line, lloc, istart, istop, 2, idsymrd, r, iout, iin)
    
    if(idsymrd .ne. 0) then
        call ustop('The symmetric connection input format (IDSYMRD = 1) is not supported. Stop.')
    end if
    
    if(ivsd .eq. 1) then
        call ustop('The vertical layer subdivision option (IVSD = 1) is not supported. Stop,')
    end if
    
    ! Allocate arrays
    if(allocated(grid%Ja)) deallocate(grid%Ja)
    if(allocated(grid%JaOffsets)) deallocate(grid%JaOffsets)
    if(allocated(grid%Top)) deallocate(grid%Top)
    if(allocated(grid%Bottom)) deallocate(grid%Bottom)
    if(allocated(grid%CellX)) deallocate(grid%CellX)
    if(allocated(grid%CellY)) deallocate(grid%CellY)
    if(allocated(grid%DelX)) deallocate(grid%DelX)
    if(allocated(grid%DelY)) deallocate(grid%DelY)
    if(allocated(grid%LayerOffsets)) deallocate(grid%LayerOffsets)
    if(allocated(grid%CellType)) deallocate(grid%CellType)
    if(allocated(grid%SaturatedTop)) deallocate(grid%SaturatedTop)
    
    allocate(grid%Ja(grid%JaCount))
    allocate(grid%JaOffsets(grid%CellCount + 1))
    allocate(grid%Top(grid%CellCount))
    allocate(grid%SaturatedTop(grid%CellCount))
    allocate(grid%Bottom(grid%CellCount))
    allocate(grid%CellX(grid%CellCount))
    allocate(grid%CellY(grid%CellCount))
    allocate(grid%DelX(grid%CellCount))
    allocate(grid%DelY(grid%CellCount))
    allocate(grid%CellType(grid%CellCount))
    allocate(grid%LayerOffsets(grid%LayerCount + 1))
    allocate(iac(grid%CellCount))
    allocate(nodelay(grid%LayerCount))
    allocate(laycbd(grid%LayerCount))
    allocate(iBuffer(grid%JaCount))
    allocate(rBuffer(grid%JaCount))
    
    ! Initialize default georeference data
    grid%OriginX = 0.0
    grid%OriginY = 0.0
    grid%RotationAngle = 0.0
    
    ! Initialize node coordinates, cell spacing values, and CellType to 0
    do n = 1, grid%CellCount
        grid%CellX(n) = 0.0
        grid%CellY(n) = 0.0
        grid%DelX(n) = 0.0
        grid%DelY(n) = 0.0
        grid%CellType(n) = 0
    end do
    
    ! Read laycbd
    read(iin, *) (laycbd(n), n = 1, grid%LayerCount)
    do n = 1, grid%LayerCount
        if(laycbd(n) .ne. 0) then
            call ustop('This version of MODPATH does not support quasi-3d confining beds. Stop')
        end if
    end do
    
    ! Read nodelay and initialize LayerOffsets array
    aname = 'NODELAY'
    call u1dintmp(nodelay,aname,grid%LayerCount,0,iin,iout)
    grid%LayerOffsets(1) = 0
    do n = 1, grid%LayerCount
        grid%LayerOffsets(n + 1) = grid%LayerOffsets(n) + nodelay(n)
    end do
    
    ! Read top elevations
    offset = 0
    aname = 'TOP'
    do n = 1, grid%LayerCount
        call u1ddblmp(grid%Top(offset + 1),aname,nodelay(n),n,iin,iout)
        offset = offset + nodelay(n)
    end do
    
    ! Set initial value of SaturatedTop equal to Top for all cells
    do n = 1, grid%CellCount
        grid%SaturatedTop(n) = grid%Top(n)
    end do
    
    ! Read bottom elevations
    offset = 0
    aname = 'BOTTOM'
    do n = 1, grid%LayerCount
        call u1ddblmp(grid%Bottom(offset + 1),aname,nodelay(n),n,iin,iout)
        offset = offset + nodelay(n)
    end do
    
    ! Read area arrays
    aname = 'AREA'
    if(ivsd .eq. -1) then
        call u1ddblmp(rBuffer,aname,nodelay(1),0,iin,iout)
    else
        offset = 0
        do n = 1, grid%LayerCount
            call u1ddblmp(rBuffer(offset+1),aname,nodelay(n),n,iin,iout)
            offset = offset + nodelay(n)
        end do
    end if
 
    ! Read IA array
    aname = 'IAC'
    call u1dintmp(iac,aname,grid%CellCount,0,iin,iout)
    grid%JaOffsets(1) = 0
    do n = 1, grid%CellCount
        grid%JaOffsets(n + 1) = grid%JaOffsets(n) + iac(n)
    end do
 
    ! Read JA array
    aname = 'JA                      '
    call u1dintmp(grid%Ja,aname,grid%JaCount,0,iin,iout)
    
    ! Read nonsymmetric input arrays. Only non-symmetric input is supported (IDSYMRD = 0).
    aname = 'CL12                    '
    call u1ddblmp(rBuffer,aname,grid%JaCount,0,iin,iout)
    ! Read nonsymmetric FAHL array
    aname = 'FAHL                    '
    call u1ddblmp(rBuffer,aname,grid%JaCount,0,iin,iout)        
        
    ! Deallocate local arrays
    deallocate(iac)
    deallocate(laycbd)
    deallocate(nodelay)
    deallocate(iBuffer)
    deallocate(rBuffer)
    
  end subroutine ReadDISU

  subroutine ReadGridMeta(grid, iin, iout)
  use utl7module,only : upcase
  use UTL8MODULE,only : u8rdcom, urword, ustop
  implicit none
  class(RectangularGridDisuMfusgType),intent(inout) :: grid
  integer,intent(in) :: iin, iout
  integer :: n, ierr, lloc, nval, istart, istop, ncode
  integer,dimension(:),allocatable :: cellsPerLayer
  double precision :: rval
  logical :: isfound
  character (len=132) :: line
  
  call u8rdcom(iin, iout, line, ierr)
  lloc = 1
  call urword(line, lloc, istart, istop, 1, nval, grid%OriginX, 0, 0)
  call urword(line, lloc, istart, istop, 1, nval, grid%OriginY, 0, 0)
  call urword(line, lloc, istart, istop, 1, nval, grid%RotationAngle, 0, 0)

  call u8rdcom(iin, iout, line, ierr)
  lloc = 1
  call urword(line, lloc, istart, istop, 1, grid%CellCount,rval, 0, 0)
  call urword(line, lloc, istart, istop, 1, grid%LayerCount,rval, 0, 0)
  allocate(cellsPerLayer(grid%LayerCount))
  read(iin, *) (cellsPerLayer(n), n = 1, grid%LayerCount)
  
  do n = 1, grid%CellCount
    call u8rdcom(iin, iout, line, ierr)
    lloc = 1
    call urword(line, lloc, istart, istop, 2, nval, rval, 0, 0)
    if(nval .ne. n) then
      call ustop('A cell number is out of sequence in GRIDDATA block of GRIDMETA file for grid type DISU. Stop.')
    end if
    call URWORD(line, lloc, istart, istop, 3, nval, grid%CellX(n), 0, 0)
    call URWORD(line, lloc, istart, istop, 3, nval, grid%CellY(n), 0, 0)
    call URWORD(line, lloc, istart, istop, 3, nval, grid%DelX(n), 0, 0)
    call URWORD(line, lloc, istart, istop, 3, nval, grid%DelY(n), 0, 0)
  end do
  
  end subroutine ReadGridMeta
  
  subroutine ReadGridMeta_block_form(grid, iin, iout)
  use utl7module,only : upcase
  use UTL8MODULE,only : uget_block, uterminate_block, u8rdcom, urword, ustop
  implicit none
  class(RectangularGridDisuMfusgType),intent(inout) :: grid
  integer,intent(in) :: iin, iout
  integer :: n, ierr, lloc, nval, istart, istop, ncode
  double precision :: rval
  logical :: isfound
  character (len=132) :: line
  
  ! Read OPTIONS block
  call uget_block(iin, iout, 'OPTIONS', ierr, isfound, lloc, line)
  if(isfound) then
      grid%OriginX = 0.0
      grid%OriginY = 0.0
      grid%RotationAngle = 0.0
      do
          call u8rdcom(iin, iout, line, ierr)
          lloc = 1
          call urword(line, lloc, istart, istop, 1, nval, rval, 0, 0)
          select case(line(istart:istop))
              case('XORIGIN')
                  call urword(line, lloc, istart, istop, 3, nval, grid%OriginX, 0, 0)
              case('YORIGIN')
                   call urword(line, lloc, istart, istop, 3, nval, grid%OriginY, 0, 0)
              case('ANGROT')
                   call urword(line, lloc, istart, istop, 3, nval, grid%RotationAngle, 0, 0)
                  ! Ignore the time units parameter for now.
              case('END','BEGIN')
                  call uterminate_block(iin, iout, line(istart:istop), 'OPTIONS', lloc, line, ierr)
                  if(ierr .eq. 0) exit
              case default
                call ustop('OPTIONS block not found in GRIDMETA file for grid type DISU. Stop.')
          end select        
      end do
  else
      call ustop('OPTIONS block not found in GRIDMETA file for grid type DISU. Stop.')
  end if
  
  ! Read DIMENSIONS block
  call uget_block(iin, iout, 'DIMENSIONS', ierr, isfound, lloc, line)
  if(isfound) then
      do
          call u8rdcom(iin, iout, line, ierr)
          lloc = 1
          call urword(line, lloc, istart, istop, 1, nval, rval, 0, 0)
          select case(line(istart:istop))
            case('NCELLS')
                call urword(line, lloc, istart, istop, 2, nval, rval, 0, 0)
                if(nval .ne. grid%CellCount) then
                    call ustop('NCELLS value in DIMENSIONS block of GRIDMETA file does not match the value in the DISU file. Stop.')
                end if
            case('END','BEGIN')
                call uterminate_block(iin, iout, line(istart:istop), 'DIMENSIONS', lloc, line, ierr)
                if(ierr .eq. 0) exit
            case default
                call ustop('Unrecognized keyword in DIMENSIONS block of GRIDMETA file for grid type DISU. Stop.')
          end select        
      end do
  else
      call ustop('DIMENSIONS block not found in TDIS file. Stop.')
  end if
  
  ! Read GRIDDATA block
  call uget_block(iin, iout, 'GRIDDATA', ierr, isfound, lloc, line)
  if(isfound) then
      do n = 1, grid%CellCount
          call u8rdcom(iin, iout, line, ierr)
          lloc = 1
          call urword(line, lloc, istart, istop, 2, nval, rval, 0, 0)
          if(nval .ne. n) then
            call ustop('A cell number is out of sequence in GRIDDATA block of GRIDMETA file for grid type DISU. Stop.')
          end if
          call URWORD(line, lloc, istart, istop, 3, nval, grid%CellX(n), 0, 0)
          call URWORD(line, lloc, istart, istop, 3, nval, grid%CellY(n), 0, 0)
          call URWORD(line, lloc, istart, istop, 3, nval, grid%DelX(n), 0, 0)
          call URWORD(line, lloc, istart, istop, 3, nval, grid%DelY(n), 0, 0)
      end do
      
      do
          call u8rdcom(iin, iout, line, ierr)
          lloc = 1
          call urword(line, lloc, istart, istop, 1, nval, rval, 0, 0)
          select case(line(istart:istop))
            case('END','BEGIN')
                call uterminate_block(iin, iout, line(istart:istop), 'GRIDDATA', lloc, line, ierr)
                if(ierr .eq. 0) exit
            case default
                call ustop('Unrecognized keyword in GRIDDATA block of GRIDMETA file for grid type DISU. Stop.')
          end select        
      end do
  else
      call ustop('GRIDDATA block not found in GRIDMETA file for grid type DISU. Stop.')
  end if
  
  
  end subroutine ReadGridMeta_block_form
  
    
end module RectangularGridDisuMfusgModule