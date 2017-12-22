module StartingLocationReaderModule
use ParticleGroupModule,only : ParticleGroupType
!use RectangularUnstructuredGridModule,only : RectangularUnstructuredGridType
use ModflowRectangularGridModule,only : ModflowRectangularGridType
use ParticleModule,only : ParticleType
implicit none

! Set default access status to private
private

public ReadAndPrepareLocations
  
contains

subroutine ReadAndPrepareLocations(inUnit, outUnit, particleGroup, ibound,      &
  cellCount, grid, seqNumber)
!***************************************************************************************************************
! Description goes here
!***************************************************************************************************************
!
! Specifications
!---------------------------------------------------------------------------------------------------------------
use UTL8MODULE,only : ustop, u8rdcom
implicit none
type(ParticleGroupType),intent(inout) :: particleGroup
integer,intent(in) :: cellCount, inUnit, outUnit
integer,intent(inout) :: seqNumber
integer :: inu, inputStyle, templateCount, n, m, releaseTimeCount, offset, layerCount, rowCount, columnCount, idmax
integer :: singleReleaseCount, errorCode
logical :: closeFile
character (len=200) :: line
integer,dimension(cellCount),intent(in) :: ibound
class(ModflowRectangularGridType),intent(in) :: grid
!---------------------------------------------------------------------------------------------------------------
  
! Open starting locations file if inUnit is not positive. Otherwise, assume inUnit is open and read from it.
if(inUnit .le. 0) then
    inu = 99
    open(unit=inu, file=particleGroup%LocationFile, status='old',               &
      form='formatted', access='sequential')
    closeFile = .true.
else
    inu = inUnit
    closeFile = .false.
end if
  
! Read comment lines
call u8rdcom(inu, outUnit, line, errorCode)

! Read locations input style
read(line, *) inputStyle

select case (inputStyle)
    case (1)
        ! Read individual particle starting location data
        call pr_ReadLocations1(particleGroup, inu, grid)             
    case (2)
        ! Read a particle location template and a list of grid cell regions defined by base grid layer, row, and column.
        ! Particles are generated for each cell contained in the grid cell cell regions according to the template specifications.
        call pr_ReadLocations2(particleGroup, inu, grid, ibound, cellCount)             
    case (3)
        ! Read a particle location template and a list of grid cell numbers.
        ! Particles are generated for each grid cell according to the template specifications.
        call pr_ReadLocations3(particleGroup, inu, grid, ibound, cellCount)   
    case (4)
        ! Read particle template information and generate particles for cells defined by mask arrays for each layer.
        call pr_ReadLocations4(particleGroup, inu, outUnit, ibound, cellCount, grid) 
    case default
        call ustop('Unsupported input style was specified for starting locations. Stop.')
end select
      
if(closeFile) close(inu)
  
! Assign layer value to each particle
singleReleaseCount = particleGroup%GetSingleReleaseParticleCount()
idmax = 0
do m = 1, singleReleaseCount
    seqNumber = seqNumber + 1
    if(particleGroup%Particles(m)%Id .gt. idmax) idmax = particleGroup%Particles(m)%Id
    particleGroup%Particles(m)%Group = particleGroup%Group
    particleGroup%Particles(m)%SequenceNumber = seqNumber
    particleGroup%Particles(m)%InitialLayer =                                   &
      grid%GetLayer(particleGroup%Particles(m)%InitialCellNumber)
    particleGroup%Particles(m)%Layer =                                          &
      grid%GetLayer(particleGroup%Particles(m)%CellNumber)
end do
  
! Initialize particle data for all additional releases
if(particleGroup%GetReleaseTimeCount() .gt. 1) then
    do n = 2, particleGroup%GetReleaseTimeCount()
        offset = (n - 1) * singleReleaseCount
        do m = 1, singleReleaseCount
            idmax = idmax + 1
            seqNumber = seqNumber + 1
            particleGroup%Particles(offset + m)%Id = idmax
            particleGroup%Particles(offset + m)%SequenceNumber = seqNumber
            particleGroup%Particles(offset + m)%Group = particleGroup%Group
            particleGroup%Particles(offset + m)%Drape = particleGroup%Particles(m)%Drape
            particleGroup%Particles(offset + m)%Status = particleGroup%Particles(m)%Status
            particleGroup%Particles(offset + m)%InitialCellNumber = particleGroup%Particles(m)%InitialCellNumber
            particleGroup%Particles(offset + m)%InitialLayer = particleGroup%Particles(m)%InitialLayer
            particleGroup%Particles(offset + m)%InitialFace = particleGroup%Particles(m)%InitialFace
            particleGroup%Particles(offset + m)%InitialLocalX = particleGroup%Particles(m)%InitialLocalX
            particleGroup%Particles(offset + m)%InitialLocalY = particleGroup%Particles(m)%InitialLocalY
            particleGroup%Particles(offset + m)%InitialLocalZ = particleGroup%Particles(m)%InitialLocalZ
            particleGroup%Particles(offset + m)%InitialTrackingTime = particleGroup%GetReleaseTime(n)
            particleGroup%Particles(offset + m)%TrackingTime = particleGroup%Particles(offset + m)%InitialTrackingTime
            particleGroup%Particles(offset + m)%CellNumber = particleGroup%Particles(m)%CellNumber
            particleGroup%Particles(offset + m)%Layer = particleGroup%Particles(m)%Layer
            particleGroup%Particles(offset + m)%Face = particleGroup%Particles(m)%Face
            particleGroup%Particles(offset + m)%LocalX = particleGroup%Particles(m)%LocalX
            particleGroup%Particles(offset + m)%LocalY = particleGroup%Particles(m)%LocalY
            particleGroup%Particles(offset + m)%LocalZ = particleGroup%Particles(m)%LocalZ
        end do
    end do
end if
  
end subroutine ReadAndPrepareLocations

subroutine pr_ReadLocations1(pGroup, inUnit, grid)
!***************************************************************************************************************
! Description goes here
!***************************************************************************************************************
!
! Specifications
!---------------------------------------------------------------------------------------------------------------
use UTL8MODULE,only : ustop, ugetnode
implicit none
type(ParticleGroupType),intent(inout) :: pGroup
class(ModflowRectangularGridType),intent(in) :: grid
integer,intent(in) :: inUnit
integer :: count, n, drape, cellNumber, error, readId, particleId, locationStyle
integer :: totalParticleCount, layer, row, column, gridType, layerCount, rowCount, columnCount
doubleprecision :: x, y, z, t
!---------------------------------------------------------------------------------------------------------------

! Read Location style
read(inUnit, *) locationStyle
if((locationStyle .lt. 1) .or. (locationStyle .gt. 2))                          &
  call ustop('Unsupported locations style was specified in for starting locations. Stop.')

! Read particle count, drape option, particleId read option
read(inUnit, *) count, readId
if(locationStyle .eq. 1) then
    if( (grid%GridType .eq.2) .or. (grid%GridType .eq.4) ) then
         call ustop('Starting locations cannot be specified by layer,row, column for unstructured grids. Stop.')
    end if
end if
    
! Allocate the Particles array
totalParticleCount = count * pGroup%GetReleaseTimeCount()
if(allocated(pGroup%Particles)) deallocate(pGroup%Particles)
allocate(pGroup%Particles(totalParticleCount))
pGroup%TotalParticleCount = totalParticleCount
  
layerCount = grid%LayerCount
rowCount = grid%RowCount
columnCount = grid%ColumnCount
do n = 1, count
    particleId = n
    if(locationStyle .eq. 1) then
        if(readId .eq. 0) then
            read(inUnit, *) layer, row, column, x, y, z, t, drape
        else
            read(inUnit, *) particleId, layer, row, column, x, y, z, t, drape
        end if
        call ugetnode(layerCount, rowCount, columnCount, layer, row, column, cellNumber)
    else
        if(readId .eq. 0) then
            read(inUnit, *) cellNumber, x, y, z, t, drape
        else
            read(inUnit, *) particleId, cellNumber, x, y, z, t, drape
        end if
    end if
    error = 0
    if(t.lt.0.0d0) error = 1
    if(x.lt.0.0d0 .or. y.lt.0.0d0 .or. z.lt.0.0d0) error = 1
    if(x.gt.1.0d0 .or. y.gt.1.0d0 .or. z.gt.1.0d0) error = 1
    if(error .gt. 0) then
        call ustop('Invalid local coordinate or release time offset in particle input file. Stop.')
    end if
          
    pGroup%Particles(n)%Id = particleId
    pGroup%Particles(n)%Drape = drape
    pGroup%Particles(n)%Status = 0
    pGroup%Particles(n)%InitialCellNumber = cellNumber
    pGroup%Particles(n)%InitialFace = pr_FindFace(x, y, z) 
    pGroup%Particles(n)%InitialLocalX = x
    pGroup%Particles(n)%InitialLocalY = y
    pGroup%Particles(n)%InitialLocalZ = z
    pGroup%Particles(n)%InitialGlobalZ = 0.0d0
    pGroup%Particles(n)%InitialTrackingTime = pGroup%GetReleaseTime(1) + t
    pGroup%Particles(n)%CellNumber = pGroup%Particles(n)%InitialCellNumber
    pGroup%Particles(n)%Face = pGroup%Particles(n)%InitialFace
    pGroup%Particles(n)%LocalX = pGroup%Particles(n)%InitialLocalX
    pGroup%Particles(n)%LocalY = pGroup%Particles(n)%InitialLocalY
    pGroup%Particles(n)%LocalZ = pGroup%Particles(n)%InitialLocalZ
    pGroup%Particles(n)%GlobalZ = pGroup%Particles(n)%InitialGlobalZ
    pGroup%Particles(n)%TrackingTime = pGroup%Particles(n)%InitialTrackingTime
          
end do
  
100 continue  
  
end subroutine pr_ReadLocations1

subroutine pr_ReadLocations2(pGroup, inUnit, grid, ibound, gridCellCount)
!***************************************************************************************************************
! Description goes here
!***************************************************************************************************************
!
! Specifications
!---------------------------------------------------------------------------------------------------------------
use UTL8MODULE,only : ustop, urword, ugetnode
implicit none
type(ParticleGroupType),intent(inout) :: pGroup
class(ModflowRectangularGridType),intent(in) :: grid
integer,intent(in) :: inUnit, gridCellCount
integer,intent(in),dimension(gridCellCount) :: ibound
integer :: totalParticleCount, templateCount, cellRegionCount, intValue
integer :: count,np,n,m,face,i,j,k,layerCount,rowCount,columnCount,cell,npcell
doubleprecision :: dx,dy,dz,x,y,z,faceCoord,rowCoord,columnCoord,dr,dc
integer,dimension(:),allocatable :: templateSubDivisionTypes,minLayers,         &
  maxLayers,minRows,maxRows,minColumns,maxColumns,templateCellRegionCounts,     &
  drape
integer,dimension(:,:),allocatable :: subDiv
integer,dimension(12) :: sdiv
integer :: nc,nr,nl,layer,row,column,cr
integer :: icol,istart,istop
doubleprecision :: dblValue
character (len=200) :: line

! Read the number of cells
read(inUnit, *) templateCount, cellRegionCount
  
! Allocate temporary arrays
allocate(subDiv(templateCount,12))
allocate(templateSubDivisionTypes(templateCount))
allocate(templateCellRegionCounts(templateCount))
allocate(drape(templateCount))
allocate(minLayers(cellRegionCount))
allocate(maxLayers(cellRegionCount))
allocate(minRows(cellRegionCount))
allocate(maxRows(cellRegionCount))
allocate(minColumns(cellRegionCount))
allocate(maxColumns(cellRegionCount))
  
! Read data into temporary arrays and count the number of particles
rowCount = grid%RowCount
columnCount = grid%ColumnCount
layerCount = grid%LayerCount
do n = 1, templateCount
    do m = 1, 12
        subDiv(n,m) = 0
    end do
end do

np = 0
npcell = 0
do n = 1, templateCount
    read(inUnit, *) templateSubDivisionTypes(n), templateCellRegionCounts(n), drape(n)
    if(templateSubDivisionTypes(n) .eq. 1) then
        read(inUnit, *) (subDiv(n,i), i = 1, 12)
        npcell = subDiv(n,1)*subDiv(n,2) + subDiv(n,3)*subDiv(n,4) +            &
          subDiv(n,5)*subDiv(n,6) + subDiv(n,7)*subDiv(n,8) +                   &
          subDiv(n,9)*subDiv(n,10) + subDiv(n,11)*subDiv(n,12)
    else if(templateSubDivisionTypes(n) .eq. 2) then
        read(inUnit, *) (subDiv(n,i), i = 1, 3)
        npcell = subDiv(n,1)*subDiv(n,2)*subDiv(n,3)
    else
        call ustop('Invalid subdivision type was specified in starting locations data. Stop.')
    end if   
    
    do m = 1, templateCellRegionCounts(n)
        read(inUnit, *) minLayers(m), minRows(m), minColumns(m), maxLayers(m),  &
          maxRows(m), maxColumns(m)
        if(npcell .gt. 0) then
            do layer = minLayers(m), maxLayers(m)
                do row = minRows(m), maxRows(m)
                    do column = minColumns(m), maxColumns(m)
                        call ugetnode(layerCount, rowCount, columnCount, layer, &
                          row, column, cell)
                        if(ibound(cell) .ne. 0) then
                            np = np + npcell
                        end if
                    end do
                end do
            end do
        end if
    end do
end do
  
! Calculate the total number of particles for all release time points.
totalParticleCount = np*pGroup%GetReleaseTimeCount()
if(allocated(pGroup%Particles)) deallocate(pGroup%Particles)
allocate(pGroup%Particles(totalParticleCount))
pGroup%TotalParticleCount = totalParticleCount
 
! Set the data for particles at the first release time point
m = 0
if(templateCount .gt. 0) then
    do n = 1, templateCount
        do i = 1, 12
            sdiv(i) = subDiv(n, i)
        end do
        
        do cr = 1, templateCellRegionCounts(n)
            do layer = minLayers(cr), maxLayers(cr)
                do row = minRows(cr), maxRows(cr)
                    do column = minColumns(cr), maxColumns(cr)
                        call ugetnode(layerCount, rowCount, columnCount, layer, &
                          row, column, cell)
                        if(ibound(cell) .ne. 0) then
                            if(templateSubDivisionTypes(n) .eq. 1) then
                                call pr_CreateParticlesOnFaces(pGroup, cell, m, &
                                  sdiv, drape(n))
                            else if(templateSubDivisionTypes(n) .eq. 2) then
                                call pr_CreateParticlesAsInternalArray(pGroup,  &
                                  cell, m, sdiv(1), sdiv(2), sdiv(3), drape(n))
                            end if
                        end if
                    end do
                end do
            end do
        end do
    end do
end if
  
! Deallocate temporary arrays
deallocate(subDiv)
deallocate(templateSubDivisionTypes)
deallocate(drape)
deallocate(minLayers)
deallocate(maxLayers)
deallocate(minRows)
deallocate(maxRows)
deallocate(minColumns)
deallocate(maxColumns)
end subroutine pr_ReadLocations2

subroutine pr_ReadLocations3(pGroup, inUnit, grid, ibound, gridCellCount)
!***************************************************************************************************************
! Description goes here
!***************************************************************************************************************
!
! Specifications
!---------------------------------------------------------------------------------------------------------------
use UTL8MODULE,only : ustop
implicit none
type(ParticleGroupType),intent(inout) :: pGroup
class(ModflowRectangularGridType),intent(in) :: grid
integer,intent(in) :: inUnit, gridCellCount
integer,intent(in),dimension(gridCellCount) :: ibound
integer :: totalParticleCount,templateCount,templateCellCount,nc,nr,nl,layer,row,column
integer :: count,np,n,m,nn,face,i,j,k,layerCount,rowCount,columnCount,          &
  subCellCount,cell,offset,npcell
doubleprecision :: dx,dy,dz,x,y,z,faceCoord,rowCoord,columnCoord,dr,dc
integer,dimension(:),allocatable :: templateSubDivisionTypes,                   &
  templateCellNumbers,templateCellCounts, drape
integer,dimension(:,:),allocatable :: subDiv
integer,dimension(12) :: sdiv

! Read the number of cells
read(inUnit, *) templateCount, templateCellCount
  
! Allocate temporary arrays
allocate(subDiv(templateCount,12))
allocate(templateSubDivisionTypes(templateCount))
allocate(templateCellCounts(templateCount))
allocate(drape(templateCount))
allocate(templateCellNumbers(templateCellCount))
  
! Read data into temporary arrays and count the number of particles
rowCount = grid%RowCount
columnCount = grid%ColumnCount
layerCount = grid%LayerCount
do n = 1, templateCount
    do m = 1, 12
        subDiv(n,m) = 0
    end do
end do

np = 0
npcell = 0
offset = 0
do n = 1, templateCount
    read(inUnit, *) templateSubDivisionTypes(n), templateCellCounts(n), drape(n)
    if(templateSubDivisionTypes(n) .eq. 1) then
        read(inUnit, *) (subDiv(n,i), i = 1, 12)
        npcell = subDiv(n,1)*subDiv(n,2) + subDiv(n,3)*subDiv(n,4) +            &
          subDiv(n,5)*subDiv(n,6) + subDiv(n,7)*subDiv(n,8) +                   &
          subDiv(n,9)*subDiv(n,10) + subDiv(n,11)*subDiv(n,12)
    else if(templateSubDivisionTypes(n) .eq. 2) then
        read(inUnit, *) (subDiv(n,i), i = 1, 3)
        npcell = subDiv(n,1)*subDiv(n,2)*subDiv(n,3)
    else
        call ustop('Invalid subdivision type. Stop.')
    end if   
    
    read(inUnit, *) (templateCellNumbers(offset + m), m = 1, templateCellCounts(n))
    
    ! Loop through cells and count the number of particles 
    do nn = 1, templateCellCounts(n)
        cell = templateCellNumbers(offset + nn)
        if(ibound(cell) .ne. 0) then
            if(templateSubDivisionTypes(n) .eq. 1) then
                np = np + npcell
            else if(templateSubDivisionTypes(n) .eq. 2) then
                np = np + npcell
            else
                call ustop('Invalid subdivision type. Stop.')
            end if
        end if
    end do
    
    ! Increment the offset
    offset = offset + templateCellCounts(n)
end do
  
! Calculate the total number of particles for all release time points.
totalParticleCount = np*pGroup%GetReleaseTimeCount()
if(allocated(pGroup%Particles)) deallocate(pGroup%Particles)
allocate(pGroup%Particles(totalParticleCount))
pGroup%TotalParticleCount = totalParticleCount
 
! Set the data for particles at the first release time point
m = 0
offset = 0
if(templateCount .gt. 0) then
    do n = 1, templateCount
        do i = 1, 12
            sdiv(i) = subDiv(n, i)
        end do
        
        do nn = 1, templateCellCounts(n)
            cell = templateCellNumbers(offset + nn)
            if(ibound(cell) .ne. 0) then
                if(templateSubDivisionTypes(n) .eq. 1) then
                    call pr_CreateParticlesOnFaces(pGroup, cell, m, sdiv, drape(n))
                else if(templateSubDivisionTypes(n) .eq. 2) then
                    call pr_CreateParticlesAsInternalArray(pGroup, cell, m,     &
                      sdiv(1), sdiv(2), sdiv(3), drape(n))
                end if
            end if
        end do
        
        ! Increment the offset
        offset = offset + templateCellCounts(n)
    end do
end if
  
! Deallocate temporary arrays
deallocate(subDiv)
deallocate(templateSubDivisionTypes)
deallocate(drape)
deallocate(templateCellCounts)
deallocate(templateCellNumbers)
end subroutine pr_ReadLocations3

subroutine pr_ReadLocations4(pGroup, inUnit, outUnit, ibound, cellCount, grid)
!***************************************************************************************************************
! Description goes here
!***************************************************************************************************************
!
! Specifications
!---------------------------------------------------------------------------------------------------------------
use UTL8MODULE,only : ustop, u3dintmp, u3dintmpusg
implicit none
type(ParticleGroupType),intent(inout) :: pGroup
integer,intent(in) :: inUnit, outUnit, cellCount
integer :: totalParticleCount, firstElement, lastElement, layerCount,           &
  maskValueCount, drape
integer :: templateSubDivisionType, layer, np, nx, ny, nz, offset, n, m, row,   &
  column, face, layerCellCount, count, i, j, k, nf, npcell
integer,intent(in),dimension(cellCount) :: ibound
integer,dimension(12) :: subDiv
integer,dimension(:),allocatable :: cells, offsets, layerCellCounts
doubleprecision :: dx,dy,dz,x,y,z,faceCoord,rowCoord,columnCoord,dr,dc
integer,dimension(6) :: nc,nr
integer,dimension(:),allocatable :: mask
integer,dimension(:),allocatable :: maskValues
class(ModflowRectangularGridType),intent(in) :: grid
character(len=24) :: arrayName

read(inUnit, *) templateSubDivisionType, drape
  
layerCount = grid%LayerCount
allocate(offsets(layerCount))
allocate(layerCellCounts(layerCount))
allocate(mask(cellCount))
allocate(cells(cellCount))

do n = 1, layerCount
    layerCellCounts(n) = grid%GetLayerCellCount(n)
end do
  
! Read mask value count
read(inUnit, *) maskValueCount
  
! Read mask values
if(maskValueCount .gt. 0) then
    allocate(maskValues(maskValueCount))
    read(inUnit, *) (maskValues(m), m = 1, maskValueCount)
end if
  
! Read mask array
arrayName = 'LOCATION MASK'
if((grid%GridType .eq. 1) .or. (grid%GridType .eq. 3)) then
    call u3dintmp(inUnit, outUnit, layerCount, grid%RowCount,              &
      grid%ColumnCount, cellCount, mask, arrayName)                      
else if((grid%GridType .eq. 2) .or. (grid%GridType .eq. 4) ) then
    call u3dintmpusg(inUnit, outUnit, cellCount, layerCount, mask, arrayName,   &
      layerCellCounts)
else
    write(outUnit,*) 'Invalid grid type specified when reading IBOUND array data.'
    write(outUnit,*) 'Stopping.'
    call ustop(' ')          
end if

npcell = 0
if(templateSubDivisionType .eq. 1) then
    read(inUnit, *) (subDiv(i), i = 1, 12)
    npcell = subDiv(1)*subDiv(2) + subDiv(3)*subDiv(4) + subDiv(5)*subDiv(6) +  &
      subDiv(7)*subDiv(8) + subDiv(9)*subDiv(10) + subDiv(11)*subDiv(12)
else if(templateSubDivisionType .eq. 2) then
    read(inUnit, *) (subDiv(i), i = 1, 3)
    npcell = subDiv(1)*subDiv(2)*subDiv(3)
else
    call ustop('Invalid subdivision type was specified in starting location data. Stop.')
end if   
if(npcell .eq. 0) return
  
! Count the number of cells that will have particles
if(maskValueCount .gt. 0) then
    ! Count active cells that have a mask array value equal to one of the specified
    ! mask values.
    count = 0
    do m = 1, maskValueCount
        do n = 1, cellCount
            if((ibound(n) .ne. 0) .and. (mask(n) .eq. maskValues(m))) then
                count = count + 1
                cells(count) = n
            end if
        end do
    end do
    deallocate(maskValues)
else
    ! Count active cells that have a mask array value greater than 0.
    count = 0
    do n = 1, cellCount
        if((ibound(n) .ne. 0) .and. (mask(n) .gt. 0)) then
            count = count + 1
            cells(count) = n
        end if
    end do
end if
  
! Calculate the total number of particles for all release time points.
totalParticleCount = npcell * count * pGroup%GetReleaseTimeCount()
if(allocated(pGroup%Particles)) deallocate(pGroup%Particles)
allocate(pGroup%Particles(totalParticleCount))
pGroup%TotalParticleCount = totalParticleCount
 
! Set the data for particles at the first release time point
m = 0
if(count .gt. 0) then
    if(templateSubDivisionType .eq. 1) then
        do n = 1, count
            call pr_CreateParticlesOnFaces(pGroup, cells(n), m, subDiv, drape)
        end do
    else if(templateSubDivisionType .eq. 2) then
        do n = 1, count
            call pr_CreateParticlesAsInternalArray(pGroup, cells(n), m,         &
              subDiv(1), subDiv(2), subDiv(3), drape)
        end do
    end if
end if
  
end subroutine pr_ReadLocations4
  
function pr_FindFace(x, y, z) result(face)
implicit none
doubleprecision,intent(in) :: x, y, z
integer :: face
  
face = 0
if(x .eq. 0.0d0) then
    face = 1
else if(x .eq. 1.0d0) then
    face = 2
else if(y .eq. 0.0d0) then
    face = 3
else if(y .eq. 1.0d0) then
    face =4
else if(z .eq. 0.0d0) then
    face = 5
else if(z .eq. 1.0d0) then
    face = 6
end if
  
end function pr_FindFace

subroutine pr_CreateParticlesOnFaces(pGroup, cellNumber, currentParticleCount,  &
  subDiv, drape)
!***************************************************************************************************************
! Description goes here
!***************************************************************************************************************
!
! Specifications
!---------------------------------------------------------------------------------------------------------------
use UTL8MODULE,only : ustop
implicit none
type(ParticleGroupType),intent(inout) :: pGroup
integer,dimension(12),intent(in) :: subDiv
integer,intent(inout) :: currentParticleCount
integer,intent(in) :: cellNumber, drape
integer :: n,m,face,i,j,k,nr,nc
doubleprecision :: faceCoord,rowCoord,columnCoord,dr,dc
  
m = currentParticleCount
do face = 1, 6
    nr = subDiv(2*face -1)
    nc = subDiv(2*face)
    if((nc*nr) .gt. 0) then
        dc = 1.0d0 / dble(nc)
        dr = 1.0d0 / dble(nr)
        faceCoord = 0.0d0
        if(face.eq.2 .or. face.eq.4 .or. face.eq.6) faceCoord = 1.0d0
        do i = 1, nr
            rowCoord = (i - 1)*dr + (dr/2.0d0)
            do j = 1, nc
                columnCoord = (j - 1)*dc + (dc/2.0d0)
                m = m + 1
                pGroup%Particles(m)%InitialFace = face
                if(face.eq.1 .or. face.eq.2) then
                    pGroup%Particles(m)%InitialLocalX = faceCoord
                    pGroup%Particles(m)%InitialLocalY = columnCoord
                    pGroup%Particles(m)%InitialLocalZ = rowCoord
                else if(face.eq.3 .or. face.eq.4) then
                    pGroup%Particles(m)%InitialLocalX = columnCoord
                    pGroup%Particles(m)%InitialLocalY = faceCoord
                    pGroup%Particles(m)%InitialLocalZ = rowCoord
                else
                    pGroup%Particles(m)%InitialLocalX = columnCoord
                    pGroup%Particles(m)%InitialLocalY = rowCoord
                    pGroup%Particles(m)%InitialLocalZ = faceCoord
                end if
                pGroup%Particles(m)%Id = m
                pGroup%Particles(m)%InitialGlobalZ = 0.0d0
                pGroup%Particles(m)%Drape = drape
                pGroup%Particles(m)%Status = 0
                pGroup%Particles(m)%InitialCellNumber = cellNumber
                pGroup%Particles(m)%InitialTrackingTime = pGroup%GetReleaseTime(1)
                pGroup%Particles(m)%CellNumber = pGroup%Particles(m)%InitialCellNumber
                pGroup%Particles(m)%Face = pGroup%Particles(m)%InitialFace
                pGroup%Particles(m)%LocalX = pGroup%Particles(m)%InitialLocalX
                pGroup%Particles(m)%LocalY = pGroup%Particles(m)%InitialLocalY
                pGroup%Particles(m)%LocalZ = pGroup%Particles(m)%InitialLocalZ
                pGroup%Particles(m)%GlobalZ = pGroup%Particles(m)%InitialGlobalZ
                pGroup%Particles(m)%TrackingTime = pGroup%Particles(m)%InitialTrackingTime
            end do
        end do
    end if
end do
  
currentParticleCount = m
  
end subroutine pr_CreateParticlesOnFaces

subroutine pr_CreateParticlesAsInternalArray(pGroup, cellNumber,                &
  currentParticleCount, nx, ny, nz, drape)
!***************************************************************************************************************
! Description goes here
!***************************************************************************************************************
!
! Specifications
!---------------------------------------------------------------------------------------------------------------
use UTL8MODULE,only : ustop
implicit none
type(ParticleGroupType),intent(inout) :: pGroup
integer,intent(inout) :: currentParticleCount
integer,intent(in) :: nx, ny, nz, cellNumber, drape
integer :: n,m,face,i,j,k
doubleprecision :: dx,dy,dz,x,y,z,faceCoord,rowCoord,columnCoord
  
m = currentParticleCount
pGroup%Particles(n)%InitialFace = 0
dx = 1.0d0 / dble(nx)
dy = 1.0d0 / dble(ny)
dz = 1.0d0 / dble(nz)
do k = 1, nz
    z = (k - 1)*dz + (dz/2.0d0)
    do i = 1, ny
        y = (i - 1)*dy + (dy/2.0d0)
        do j = 1, nx
            m = m+ 1
            x = (j - 1)*dx + (dx/2.0d0)
            pGroup%Particles(m)%InitialLocalX = x
            pGroup%Particles(m)%InitialLocalY = y
            pGroup%Particles(m)%InitialLocalZ = z
            pGroup%Particles(m)%InitialGlobalZ = 0.0d0
            pGroup%Particles(m)%Id = m
            pGroup%Particles(m)%Drape = drape
            pGroup%Particles(m)%Status = 0
            pGroup%Particles(m)%InitialCellNumber = cellNumber
            pGroup%Particles(m)%InitialTrackingTime = pGroup%GetReleaseTime(1)
            pGroup%Particles(m)%CellNumber = pGroup%Particles(m)%InitialCellNumber
            pGroup%Particles(m)%InitialFace = 0
            pGroup%Particles(m)%Face = 0
            pGroup%Particles(m)%LocalX = pGroup%Particles(m)%InitialLocalX
            pGroup%Particles(m)%LocalY = pGroup%Particles(m)%InitialLocalY
            pGroup%Particles(m)%LocalZ = pGroup%Particles(m)%InitialLocalZ
            pGroup%Particles(m)%GlobalZ = pGroup%Particles(m)%InitialGlobalZ
            pGroup%Particles(m)%TrackingTime = pGroup%Particles(m)%InitialTrackingTime
        end do
    end do
end do
  
currentParticleCount = m
  
end subroutine pr_CreateParticlesAsInternalArray

end module StartingLocationReaderModule