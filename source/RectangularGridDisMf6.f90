module RectangularGridDisMf6Module
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
  type, public, extends(ModflowRectangularGridType) :: RectangularGridDisMf6Type
    integer,private :: LayerCellCount
    integer,private,dimension(:),allocatable :: IDomain
  contains
  
    procedure :: GetLayerRowColumn
    procedure :: GetIDomain
    procedure :: ConvertFromNeighbor
    procedure, private ::RectangularGridDisMf6Init1
    generic :: ReadData => RectangularGridDisMf6Init1
    
  end type

    contains
!---------------------------------------------------------------------
!---------------------------------------------------------------------
!   Overriden procedures 
!-------------------------------------------------------------------------
  subroutine Reset(this)
  implicit none
  class(RectangularGridDisMf6Type) :: this
  
  ! Add override code to implement
  
  end subroutine Reset
!x------------------------------------------
  function GetIDomain(this, cellNumber) result(fval)
    class(RectangularGridDisMf6Type) :: this
    integer,intent(in) :: cellNumber
    integer :: fval
    
    fval = this%IDomain(cellNumber)
    
  end function GetIDomain
!x------------------------------------------
  subroutine ConvertFromNeighbor(this, toCellNumber, fromCellNumber,         &
    fromLocalX, fromLocalY, fromLocalZ, newLocation)
  implicit none
  class(RectangularGridDisMf6Type) :: this
  integer,intent(in) :: toCellNumber,fromCellNumber
  doubleprecision,intent(in) :: fromLocalX,fromLocalY,fromLocalZ
  class(GridLocationType),intent(inout) :: newLocation
  integer :: status,faceNumber,subFaceNumber
  doubleprecision :: localX,localY,localZ,globalX,globalY,tol
  
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
  
  newLocation%Layer = this%GetLayer(newLocation%CellNumber)
  end subroutine ConvertFromNeighbor

!------------------------------------------------------------------------
!  New procedures
!------------------------------------------------------------------------
  subroutine RectangularGridDisMf6Init1(this, iin, gridMetaUnit, iout)
    implicit none
    class(RectangularGridDisMf6Type) :: this
    integer, intent(in) :: iin, iout, gridMetaUnit
    
! Read the binary grid file for grid type DIS
  call ReadGRB(this, iin, iout)
    
  end subroutine RectangularGridDisMf6Init1

  subroutine ReadGRB(grid, iin, iout)
    implicit none
    class(RectangularGridDisMf6Type) :: grid
    integer, intent(in) :: iin, iout
    character(len=50) :: headerLine
    character(len=100) :: line
    character(len=100),dimension(:),allocatable :: textLines
    integer :: cellNumber,i,j,k,n,m,layer,row,column,level,maxlevel,conn,nc,ptr,count,offset,istart,istop,ierr,lloc
    integer :: itmuni, lenuni, version, ntxt, lentxt, ndat, ncom, idummy
    integer,dimension(:),allocatable :: laycbd
    doubleprecision :: r
    doubleprecision,dimension(:,:),allocatable :: bufdbl2d
    double precision,dimension(:),allocatable :: X
    double precision,dimension(:),allocatable :: Y
    double precision,dimension(:),allocatable :: DX
    double precision,dimension(:),allocatable :: DY

!   Set the grid type
    grid%GridType = 3
    
!   Deallocate arrays if they have been allocated previously
    call grid%Reset()    

!   Read Header lines
    read(iin) headerLine
    lloc = 1
    call urword(headerLine, lloc, istart, istop, 0, n, r, iout, iin)
    if(headerLine(istart:istop) .eq. 'GRID') then
        call urword(headerLine, lloc, istart, istop, 0, n, r, iout, iin)
        if(headerLine(istart:istop) .ne. 'DIS') then 
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
        if(ntxt .ne. 16) then
            call grid%Reset()
            call ustop('Invalid binary grid file data (NTXT). Stop.')
        end if
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
                read(iin) grid%LayerCount
            case ('NROW')
                ndat = ndat + 1
                read(iin) grid%RowCount
            case ('NCOL')
                ndat = ndat + 1
                read(iin) grid%ColumnCount
            case ('NJA')
                ndat = ndat + 1
                read(iin) grid%JaCount
            case ('ICELLTYPE')
                ndat = ndat + 1
                allocate(grid%CellType(grid%CellCount))
                do i = 1, grid%CellCount
                    read(iin) grid%CellType(i)
                end do
            case ('XORIGIN')
                ndat = ndat + 1
                read(iin) grid%OriginX
            case ('YORIGIN')
                ndat = ndat + 1
                read(iin) grid%OriginY
            case ('ANGROT')
                ndat = ndat + 1
                read(iin) grid%RotationAngle
            case ('DELR')
                ndat = ndat + 1
                allocate(DX(grid%ColumnCount))
                read(iin) (DX(i), i = 1, grid%ColumnCount)
            case ('DELC')
                ndat = ndat + 1
                allocate(DY(grid%RowCount))
                read(iin) (DY(i), i = 1, grid%RowCount)
            case ('TOP')
                ndat = ndat + 1
                allocate(grid%Top(grid%CellCount))
                read(iin) (grid%Top(i), i = 1, grid%RowCount * grid%ColumnCount)
            case ('BOTM')
                ndat = ndat + 1
                allocate(grid%Bottom(grid%CellCount))
                read(iin) (grid%Bottom(i), i = 1, grid%CellCount)
            case ('IA')
                ndat = ndat + 1
                allocate(grid%JaOffsets(grid%CellCount + 1))
                read(iin) (grid%JaOffsets(i), i = 1, grid%CellCount + 1)
                ! Subtract 1 from all array elements to convert the IA array pointers to 0-based offsets
                do i = 1, grid%CellCount + 1
                    grid%JaOffsets(i) = grid%JaOffsets(i) - 1
                end do
            case ('JA')
                ndat = ndat + 1
                allocate(grid%Ja(grid%JaCount))
                read(iin) (grid%Ja(i), i = 1, grid%JaCount)
            case ('IDOMAIN')
                ndat = ndat + 1
                allocate(grid%IDomain(grid%CellCount))
                read(iin) (grid%IDomain(i), i = 1, grid%CellCount)
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
    
!   Allocate additional arrays
    allocate(X(grid%ColumnCount + 1))
    allocate(Y(grid%RowCount + 1))
    allocate(grid%SaturatedTop(grid%CellCount))
    allocate(grid%LayerOffsets(grid%LayerCount + 1))
    allocate(grid%CellX(grid%CellCount))
    allocate(grid%CellY(grid%CellCount))
    allocate(grid%DelX(grid%CellCount))
    allocate(grid%DelY(grid%CellCount))
    allocate(grid%JaFace(grid%JaCount))
    
!   Compute and fill the layer and row offset arrays
    grid%LayerOffsets(1) = 0
    grid%LayerCellCount = grid%RowCount * grid%ColumnCount
    do n = 1, grid%LayerCount 
       grid%LayerOffsets(n + 1)  = grid%LayerOffsets(n) + grid%LayerCellCount
    end do
    
!   Compute X face coordinates
    X(1) = 0.0
    do n = 1, grid%ColumnCount
        X(n + 1) = X(n) + DX(n)
    end do
!   Compute Y face coordinates
    Y(1) = 0.0
    do n = 1, grid%RowCount
        Y(1) = Y(1) + DY(N)
    end do
    do n = 1, grid%RowCount
        Y(n + 1) = Y(n) - DY(n)
    end do
    
    do n = 1, grid%CellCount - grid%LayerCellCount
        grid%Top(n + grid%LayerCellCount) = grid%Bottom(n)
        grid%SaturatedTop(n + grid%LayerCellCount) = grid%Top(n + grid%LayerCellCount)
    end do

!   Compute CellX, CellY, DelX, and DelY
    n = 0
    do layer = 1, grid%LayerCount
        do row = 1, grid%RowCount
            do column = 1, grid%ColumnCount
                n = n + 1
                grid%DelX(n) = DX(column)
                grid%DelY(n) = DY(row)
                grid%CellX(n) = (X(column) + X(column + 1)) / 2.0
                grid%CellY(n) = (Y(row) + Y(row + 1)) / 2.0
            end do
        end do
    end do
        
!   Compute face assignments
    call grid%ComputeFaceAssignments()
    
    ! Deallocate local arrays
    deallocate(X)
    deallocate(Y)
    deallocate(DX)
    deallocate(DY)
  
  end subroutine ReadGRB
  
  !-------------------------------------------------------------------------
  subroutine GetLayerRowColumn(this, cellNumber, layer, row, column)
    class(RectangularGridDisMf6Type) :: this
    integer, intent(in) :: cellNumber
    integer, intent(inout) :: layer, row, column
    integer :: layerCellNumber, n, rowOffset
    
    do n = 1, this%LayerCount
        if(cellNumber .le. this%LayerOffsets(n + 1)) then
            layer = n
            exit
        end if
    end do
  
    layerCellNumber = cellNumber - (layer - 1)*this%LayerCellCount
    rowOffset = 0
    do n = 1, this%RowCount
        rowOffset = rowOffset + this%ColumnCount
        if(layerCellNumber .le. rowOffset) then
            row = n
            exit
        end if
    end do
    
    column = layerCellNumber - (row - 1)*this%ColumnCount
    
  end subroutine GetLayerRowColumn

  
  
end module RectangularGridDisMf6Module