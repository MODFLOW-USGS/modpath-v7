module ModpathBasicDataModule
  use UtilMiscModule,only : utrimall
  use ModflowRectangularGridModule,only : ModflowRectangularGridType
  implicit none
  
! Set default access status to private
  private
  
! Public derived data type definitions
!--------------------------------------
! type: 
!--------------------------------------
  type,public :: ModpathBasicDataType
      integer :: DefaultIfaceCount
      doubleprecision :: HNoFlow
      doubleprecision :: HDry
      integer,dimension(:),allocatable :: IBound
      integer,dimension(:),allocatable :: DefaultIfaceValues
      character(len=16),dimension(:),allocatable :: DefaultIFaceLabels
      doubleprecision,dimension(:),allocatable :: Porosity
  contains
    procedure :: ReadData=>pr_ReadData
  end type

contains

  subroutine pr_ReadData(this, inUnit, outUnit, grid)
  use utl7module,only : urdcom, upcase
  use UTL8MODULE,only : urword, ustop, u1dint, u1drel, u1ddbl, u8rdcom,         &
    u3dintmp, u3dintmpusg, u3ddblmp, u3ddblmpusg
  implicit none
  class(ModpathBasicDataType) :: this
  integer,intent(in) :: inUnit, outUnit
  integer,dimension(:),allocatable :: cellsPerLayer
  integer,dimension(:),allocatable :: layerTypes
  class(ModflowRectangularGridType),intent(inout) :: grid
  character(len=200) :: line
  character(len=16) :: txt
  integer :: n, m, nn, length, iface, errorCode, layer
  character(len=24),dimension(2) :: aname
  data aname(1) /'          BOUNDARY ARRAY'/
  data aname(2) /'                POROSITY'/
  
  if(allocated(this%IBound)) deallocate(this%IBound)
  if(allocated(this%Porosity)) deallocate(this%Porosity)
  allocate(this%IBound(grid%CellCount))
  allocate(this%Porosity(grid%CellCount))
  allocate(cellsPerLayer(grid%LayerCount))
  do n = 1, grid%LayerCount
      cellsPerLayer(n) = grid%GetLayerCellCount(n)
  end do
  
  ! Write header to the listing file
  write(outUnit, *)
  write(outUnit, '(1x,a)') 'MODPATH basic data file data'
  write(outUnit, '(1x,a)') '----------------------------'

  ! READ MPBAS DATA
  ! READ AND PRINT COMMENTS.
  call u8rdcom(inUnit,outUnit,line,errorCode)
 
  ! No flow and dry cell head flags
  if(grid%GridType .gt. 2) then
      this%HNoFlow = 1.0E+30
      this%HDry = -1.0E+30
      read(line, *) this%DefaultIfaceCount
      write(outUnit,'(1X,A,1PG12.5,A)') 'Aquifer head is set to ', this%HNoFlow,    &
        ' at cells with IDOMAIN = 0.'
      write(outUnit,'(1X,A,1PG12.5,A)') 'Aquifer head is set to ', this%Hdry,       &
        ' at all dry cells when Newton-Raphson rewetting is not active.'
      write(outUnit, *)
  else
      read(line,*) this%HNoFlow, this%HDry
      read(inUnit, *) this%DefaultIfaceCount
      write(outUnit,'(1X,A,1PG12.5,A)') 'Aquifer head is set to ', this%HNoFlow,    &
        ' at all no-flow cells (IBOUND=0).'
      write(outUnit,'(1X,A,1PG12.5,A)') 'Aquifer head is set to ', this%Hdry,       &
        ' at all dry cells.'
      write(outUnit, *)
  end if
  
      
      ! READ NUMBER OF STRESS PACKAGES THAT DO NOT HAVE AUXILIARY IFACE SUPPORT
      allocate (this%DefaultIfaceValues(this%DefaultIfaceCount))
      allocate (this%DefaultIfaceLabels(this%DefaultIfaceCount))
      
      if(this%DefaultIfaceCount .gt. 0) then
        ! READ BUDGET LABELS AND DEFAULT IFACE
        do n = 1, this%DefaultIfaceCount
          read(inUnit,'(A)') line
          call utrimall(line)
          length = len_trim(line)
          if(length .eq. 0) THEN
            call ustop('Stress package labels cannot be blank strings.')
          else
            if(length .gt. 16) length = 16
            this%DefaultIfaceLabels(n) = line
            call upcase(this%DefaultIfaceLabels(n))
          end if

          ! READ DEFAULT IFACE          
          read(inUnit,*) this%DefaultIfaceValues(n)
          
        end do

        ! Check for duplicate labels        
        do n = 1, this%DefaultIfaceCount
          txt = this%DefaultIfaceLabels(n)
          do m = 1, this%DefaultIfaceCount
            if(m .NE. n) then
              if(txt .eq. this%DefaultIfaceLabels(m)) THEN
               call ustop('Duplicate stress package labels are not allowed.')
              end if
            end if
          end do
        end do
        
        write(outUnit, *)
        write(outUnit,'(1X,A,A)') 'Default stress package boundary face options (IFACE):'
        if(this%DefaultIfaceCount .eq. 0) THEN
          write(outUnit,'(3X,A)') 'None were specified'
        else
          do n = 1, this%DefaultIfaceCount
            iface = this%DefaultIfaceValues(n)
            if(iface .eq. 0) THEN
              write(outUnit,'(3X,A,A)') trim(this%DefaultIfaceLabels(n)),       &
                ' will be treated as internal stresses (IFACE = 0)'
            else if((iface .lt. 0) .or. (iface .gt. 6)) THEN
              call ustop(' IFACE must have a value between 0 and 6.')
            else
              write(outUnit,'(3X,A,A,I2)') trim(this%DefaultIfaceLabels(n)),    &
                ' will be assigned to face ',IFACE
            end if
          end do
        end if
        
      end if
  
      ! LAYTYP
      ! Read LAYTYP only for MODFLOW-2005 and MODFLOW-USG datasets.
      ! Loop through layers and set the cell type for all cells in a 
      ! layer equal to the LAYTYP value for the layer.
      ! LAYTYP is not read for MODFLOW-6 datasets. Instead, cell types 
      ! for all cells are read directly for from the binary grid file.
      if((grid%GridType .eq. 1) .or. (grid%GridType .eq. 2)) then
          write(outUnit, *)
          write(outUnit,'(1X,A)') 'Layer type (LAYTYP)'
          allocate(layerTypes(grid%LayerCount))
          read(inUnit,*) (layerTypes(layer), layer = 1, grid%LayerCount)
          write(outUnit,'(1X,80I2)') (layerTypes(layer), layer = 1, grid%LayerCount)
          n = 0
          do layer = 1, grid%LayerCount
            if(layerTypes(layer) .lt. 0) layerTypes(layer) = 1
            do m = 1, cellsPerLayer(layer)
                n = n + 1
                grid%CellType(n) = layerTypes(layer)
            end do
          end do
      end if
      
      ! IBOUND
      if(grid%GridType .eq. 1) then
            call u3dintmp(inUnit, outUnit, grid%LayerCount, grid%RowCount,      &
              grid%ColumnCount, grid%CellCount, this%IBound, ANAME(1))                      
      else if(grid%GridType .eq. 2) then
          call u3dintmpusg(inUnit, outUnit, grid%CellCount, grid%LayerCount, this%IBound, &
            aname(1), cellsPerLayer)
      else if(grid%GridType .ge. 3) then
          do n = 1, grid%CellCount
              if(grid%GetIDomain(n) .lt. 1) then
                  this%IBound(n) = 0
              else
                  this%IBound(n) = 1
              end if
          end do
      else
            write(outUnit,*) 'Invalid grid type specified when reading IBOUND array data.'
            write(outUnit,*) 'Stopping.'
            call ustop(' ')          
      end if

      ! POROSITY
      if((grid%GridType .eq. 1) .or. (grid%GridType .eq. 3)) then
            call u3ddblmp(inUnit, outUnit, grid%LayerCount, grid%RowCount,      &
              grid%ColumnCount, grid%CellCount, this%Porosity, ANAME(2))                      
      else if((grid%GridType .eq. 2) .or. (grid%GridType .eq. 4)) then
          call u3ddblmpusg(inUnit, outUnit, grid%CellCount, grid%LayerCount,              &
            this%Porosity, aname(2), cellsPerLayer)
      else
            write(outUnit,*) 'Invalid grid type specified when reading POROSITY array data.'
            write(outUnit,*) 'Stopping.'
            call ustop(' ')          
      end if
  
  end subroutine pr_ReadData

end module  ModpathBasicDataModule     
