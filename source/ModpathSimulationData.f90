module ModpathSimulationDataModule
  use ParticleTrackingOptionsModule,only : ParticleTrackingOptionsType
  use ParticleGroupModule,only : ParticleGroupType
  use ModflowRectangularGridModule,only : ModflowRectangularGridType
  use StartingLocationReaderModule,only : ReadAndPrepareLocations
  implicit none
  
! Set default access status to private
  private
  
! Public derived data type definitions
!--------------------------------------
! type: 
!--------------------------------------
  type,public :: ModpathSimulationDataType
    character(len=200) :: NameFile
    character(len=200) :: ListingFile
    integer :: TraceMode, TraceGroup, TraceID
    integer :: SimulationType
    integer :: TrackingDirection
    integer :: WeakSinkOption
    integer :: WeakSourceOption
    integer :: ReferenceTimeOption
    integer :: StoppingTimeOption
    integer :: BudgetOutputOption
    integer :: PathlineFormatOption
    integer :: ZoneDataOption
    integer :: RetardationFactorOption
    integer :: AdvectiveObservationsOption
    integer :: TimePointOption
    integer :: ParticleGroupCount
    integer :: TotalParticleCount
    integer :: TimePointCount
    integer :: StopZone
    integer :: BudgetCellsCount
    doubleprecision :: StopTime
    doubleprecision :: ReferenceTime
    doubleprecision :: TimePointInterval
    character(len=200) :: EndpointFile
    character(len=200) :: PathlineFile
    character(len=200) :: TimeseriesFile
    character(len=200) :: TraceFile
    character(len=200) :: AdvectiveObservationsFile
    integer,dimension(:),allocatable :: BudgetCells
    integer,dimension(:),allocatable :: Zones
    doubleprecision,dimension(:),allocatable :: Retardation
    doubleprecision,dimension(:),allocatable :: TimePoints
    type(ParticleGroupType),dimension(:),allocatable :: ParticleGroups
    type(ParticleTrackingOptionsType),allocatable :: TrackingOptions
  contains
    procedure :: ReadFileHeaders=>pr_ReadFileHeaders
    procedure :: ReadData=>pr_ReadData
  end type


contains

  subroutine pr_ReadFileHeaders(this, inUnit)
  use UTL8MODULE,only : u8rdcom
  implicit none
  class(ModpathSimulationDataType) :: this
  integer,intent(in) :: inUnit
  integer :: outUnit, errorCode
  character(len=200) line
  
  outUnit = 0
  call u8rdcom(inUnit, outUnit, line, errorCode)
  
  ! Assign the name file
  this%NameFile = line
  
  ! Read MODPATH listing file filename
  read(inUnit, '(a)') this%ListingFile
  
  end subroutine pr_ReadFileHeaders
  
  subroutine pr_ReadData(this, inUnit, outUnit, ibound, timeDiscretization, grid)
  use UTL8MODULE,only : urword, ustop, u1dint, u1drel, u1ddbl, u8rdcom, u3ddblmpusg, u3dintmp, u3dintmpusg, u3ddblmp
  use TimeDiscretizationDataModule,only : TimeDiscretizationDataType
  implicit none
  class(ModpathSimulationDataType) :: this
  class(ModflowRectangularGridType),intent(in) :: grid
  integer,intent(in) :: inUnit, outUnit
  integer,dimension(:),allocatable :: cellsPerLayer
  integer,dimension(grid%CellCount),intent(in) :: ibound
  type(TimeDiscretizationDataType),intent(in) :: timeDiscretization
  integer :: icol, istart, istop, n, nc, kper, kstp, seqNumber, particleCount, nn, slocUnit, errorCode
  integer :: releaseOption, releaseTimeCount
  doubleprecision :: initialReleaseTime, releaseInterval
  doubleprecision,dimension(:),allocatable :: releaseTimes
  doubleprecision :: frac, r, tinc
  character*24 aname(2)
  character(len=200) line
  DATA aname(1) /'              ZONE ARRAY'/
  DATA aname(2) /'                 RFACTOR'/
  
  ! Deallocate arrays
  if(allocated(this%Zones)) deallocate(this%Zones)
  if(allocated(this%Retardation)) deallocate(this%Retardation)
  if(allocated(this%TimePoints)) deallocate(this%TimePoints)
  if(allocated(this%ParticleGroups)) deallocate(this%ParticleGroups)
  if(allocated(this%TrackingOptions)) deallocate(this%TrackingOptions)
  allocate(this%Zones(grid%CellCount))
  allocate(this%Retardation(grid%CellCount))
  allocate(cellsPerLayer(grid%LayerCount))
  do n = 1, grid%LayerCount
      cellsPerLayer(n) = grid%GetLayerCellCount(n)
  end do
  
  ! Write header to the listing file
  write(outUnit, *)
  write(outUnit, '(1x,a)') 'MODPATH simulation file data'
  write(outUnit, '(1x,a)') '----------------------------'
  
  ! Rewind simulation file, then re-read comment lines and the first two non-comment lines containing the name file and listing file names
  ! that were read previously.
  rewind(inUnit)
  call u8rdcom(inUnit, outUnit, line, errorCode)
  read(inUnit, '(a)') line
  
  ! Read simulation options line, then parse line using subroutine urword
  read(inUnit, '(a)') line
  
  ! Simulation type
  icol = 1
  call urword(line, icol, istart, istop, 2, n, r, 0, 0)
  this%SimulationType = n
  
  ! Tracking direction
  call urword(line, icol, istart, istop, 2, n, r, 0, 0)
  this%TrackingDirection = n
  
  ! Weak sink option
  call urword(line, icol, istart, istop, 2, n, r, 0, 0)
  this%WeakSinkOption = n
  
  ! Weak source option
  call urword(line, icol, istart, istop, 2, n, r, 0, 0)
  this%WeakSourceOption = n
  
  ! Budget output option
  call urword(line, icol, istart, istop, 2, n, r, 0, 0)
  this%BudgetOutputOption = n
  
  ! Trace mode
  call urword(line, icol, istart, istop, 2, n, r, 0, 0)
  this%TraceMode = n
  
  ! Pathline format option (hardwire value 1 = consolidate)
  this%PathlineFormatOption = 1
  
  ! Advective observations option (hardwire value 1 = do not use advective observations)
  this%AdvectiveObservationsOption = 1
  
  ! Read coordinate output file names based on simulation type
  select case (this%SimulationType)
      case (1)
          write(outUnit,'(A,I2,A)') 'Endpoint Analysis (Simulation type =',this%SimulationType,')'
          read(inUnit,'(a)') this%EndpointFile
          icol=1
          call urword(this%EndpointFile, icol, istart, istop, 0, n, r, 0, 0)
          this%Endpointfile=this%EndpointFile(istart:istop)
      case (2)
          write(outUnit,'(A,I2,A)') 'Pathline Analysis (Simulation type =', this%SimulationType, ')'
          read(inUnit, '(a)') this%EndpointFile
          icol = 1
          call urword(this%EndpointFile,icol,istart,istop,0,n,r,0,0)
          this%EndpointFile = this%EndpointFile(istart:istop)
          read(inUnit, '(a)') this%PathlineFile
          icol = 1
          call urword(this%PathlineFile, icol, istart, istop, 0, n, r, 0, 0)
          this%PathlineFile = this%PathlineFile(istart:istop)
      case (3)
          write(outUnit,'(A,I2,A)') 'Timeseries Analysis (Simulation type =',this%SimulationType,')'
          read(inUnit, '(a)') this%EndpointFile
          icol=1
          call urword(this%EndpointFile, icol, istart, istop, 0, n, r, 0, 0)
          this%Endpointfile=this%EndpointFile(istart:istop)
          read(inUnit, '(a)') this%TimeseriesFile
          icol = 1
          call urword(this%TimeseriesFile, icol, istart, istop, 0, n, r, 0, 0)
          this%TimeseriesFile = this%TimeseriesFile(istart:istop)
          IF(this%AdvectiveObservationsOption.EQ.2) then
            read(inUnit, '(a)') this%AdvectiveObservationsFile
            icol = 1
            call urword(this%AdvectiveObservationsFile, icol, istart, istop, 0, n, r,0,0)
            this%AdvectiveObservationsFile = this%AdvectiveObservationsFile(istart:istop)
          end if
      case (4)
          write(outUnit,'(A,I2,A)') 'Combined Pathline and Timeseries Analysis (Simulation type =', this%SimulationType, ')'
          read(inUnit, '(a)') this%EndpointFile
          icol = 1
          call urword(this%EndpointFile,icol,istart,istop,0,n,r,0,0)
          this%EndpointFile = this%EndpointFile(istart:istop)
          read(inUnit, '(a)') this%PathlineFile
          icol = 1
          call urword(this%PathlineFile, icol, istart, istop, 0, n, r, 0, 0)
          this%PathlineFile = this%PathlineFile(istart:istop)
          read(inUnit, '(a)') this%TimeseriesFile
          icol = 1
          call urword(this%TimeseriesFile, icol, istart, istop, 0, n, r, 0, 0)
          this%TimeseriesFile = this%TimeseriesFile(istart:istop)
          IF(this%AdvectiveObservationsOption.EQ.2) then
            read(inUnit, '(a)') this%AdvectiveObservationsFile
            icol = 1
            call urword(this%AdvectiveObservationsFile, icol, istart, istop, 0, n, r,0,0)
            this%AdvectiveObservationsFile = this%AdvectiveObservationsFile(istart:istop)
          end if
      case default
          call ustop('Invalid simulation type. Stop.')
  end select
  
  ! Read trace mode filename if trace mode is on
  if(this%TraceMode .gt. 0) then
      read(inUnit,'(a)') this%TraceFile
      icol=1
      call urword(this%EndpointFile, icol, istart, istop, 0, n, r, 0, 0)
      this%TraceFile=this%TraceFile(istart:istop)
      read(inUnit,*) this%TraceGroup, this%TraceID
  end if
  
  ! Read budget cells
  read(inUnit, *) this%BudgetCellsCount
  if(allocated(this%BudgetCells)) then
      deallocate(this%BudgetCells)
  end if
  allocate(this%BudgetCells(this%BudgetCellsCount))
  if(this%BudgetCellsCount .gt. 0) then
      read(inUnit, *) (this%BudgetCells(n), n = 1, this%BudgetCellsCount)
  end if
  
  ! Tracking direction
  select case(this%TrackingDirection)
    case(1)
      write(outUnit,'(A,I2,A)') 'Forward tracking (Tracking direction = ', this%TrackingDirection,')'
    case(2)
      write(outUnit,'(A,I2,A)') 'Backward tracking (Tracking direction =', this%TrackingDirection,')'
    case default
      call ustop('Invalid tracking direction code. Stop.')
  end select
  
  ! Weak sink option
  select case(this%WeakSinkOption)
    case (1)
      write(outUnit, '(A)') 'Let particles pass through weak sink cells (Weak sink option = 1)'
    case (2)
      write(outUnit,'(A)') 'Stop particles when they enter weak sink cells. (Weak sink option = 2)'
    case default
      call ustop('Invalid weak sink option.')
  end select

  ! Weak source option   
  select case(this%WeakSourceOption)
    case(1)
      write(outUnit,'(A)') 'Let particles pass through weak source cells for backtracking simulations (Weak source option = 1)'
    case(2)
      write(outUnit,'(A)') 'Stop particles when they enter weak source cells for backtracking simulations (Weak source option = 2)'
    case default
      call ustop('Invalid weak source option.')
    end select
  
    ! Reference time option
    read(inUnit, '(a)') line
    icol = 1
    call urword(line, icol, istart, istop, 2, n, r, 0, 0)
    this%ReferenceTimeOption = n
  
    select case(this%ReferenceTimeOption)
      case(1)
        read(inUnit,*) this%ReferenceTime
        write(outUnit,'(A,E15.7)') 'Reference time = ', this%ReferenceTime
      case(2)
        read(inUnit, *) kper, kstp, frac
        this%ReferenceTime = timeDiscretization%GetTimeFromPeriodAndStep(kper, kstp, frac)
        write(outUnit,'(A,I6,A,I6)') 'Reference time will be calculated for: Period ', KPER,' Step ', KSTP
        write(outUnit,'(A,F10.7)') 'The relative time position within the time step is =',FRAC
        write(outUnit,'(A,E15.7)') 'Computed reference time = ', this%ReferenceTime
      case default
        call ustop('Invalid reference time option.')
    end select

    ! Read stopping option
    this%StopTime = 1.0E+30
    read(inUnit, '(a)') line
    icol = 1
    call urword(line, icol, istart, istop, 2, n, r, 0, 0)
    this%StoppingTimeOption = n
    select case(this%StoppingTimeOption)
        case(1)
            write(outUnit,'(A,I2,A)')                                           &
              'Stop tracking at the beginning or end of the MODFLOW simulation (Stopping time option = ',  &
              this%StoppingTimeOption,')'
        case(2)
            write(outUnit,'(A,I2,A)')                                           &
              'Extend initial or final steady-state time step and continue tracking (Stopping time option = ', &
              this%StoppingTimeOption,')'
        case(3)
            write(outUnit,'(A,I2,A)')                                           &
              'Specify a limit for tracking time (Stoping time option = ',this%StoppingTimeOption,')'
            read(inUnit, *) this%StopTime
            write(outUnit,'(A,E15.7)') 'Stop time = ', this%StopTime
        case default
            call ustop('Invalid stop time code. Stop.')
    end select
  
    ! Time point data
    if((this%SimulationType .eq. 3) .or. (this%SimulationType .eq. 4)) then
        read(inUnit, *) this%TimePointOption
        if(this%TimePointOption .eq. 1) then
            read(inUnit, *) this%TimePointCount, this%TimePointInterval
        
            allocate(this%TimePoints(this%TimePointCount))
            if(this%TimePointCount .gt. 0) then
              this%TimePoints(1) = this%TimePointInterval
              do n = 2, this%TimePointCount
                  this%TimePoints(n) = this%TimePoints(n-1) + this%TimePointInterval
              end do   
            end if
        else if(this%TimePointOption .eq. 2) then
            read(inUnit, *) this%TimePointCount
        
            allocate(this%TimePoints(this%TimePointCount))
            if(this%TimePointCount .gt. 0) then
                read(inUnit, *) (this%TimePoints(n), n = 1, this%TimePointCount)
            end if
        else
            ! write an error message and stop
        end if
    else
        this%TimePointOption = 0
        this%TimePointCount = 0
        this%TimePointInterval = 0.0d0
        allocate(this%TimePoints(0))      
    end if
  
    ! Zone array
    read(inUnit, '(a)') line
    icol = 1
    call urword(line, icol, istart, istop, 2, n, r, 0, 0)
    this%ZoneDataOption = n
    if(this%ZoneDataOption .gt. 1) then
        write(outUnit, '(/a)') 'A zone array will be read.'
        read(inUnit,*) this%StopZone
        if(this%StopZone .lt. 1) then
          write(outUnit,'(A,I5)')                                               &
            'Particles will be allowed to pass through all zones. StopZone = ', this%StopZone
        else
          write(outUnit,'(A,I5)')                                               &
            'Particles will be terminated when they enter cells with a zone numbers equal to ', this%StopZone
        end if
        if((grid%GridType .eq. 1) .or. (grid%GridType .eq. 3)) then
            call u3dintmp(inUnit, outUnit, grid%LayerCount, grid%RowCount,      &
              grid%ColumnCount, grid%CellCount, this%Zones, ANAME(1))            
        else if((grid%GridType .eq. 2) .or. (grid%GridType .eq. 4)) then
            call u3dintmpusg(inUnit, outUnit, grid%CellCount, grid%LayerCount, this%Zones,&
              ANAME(1), cellsPerLayer)
        else
            write(outUnit,*) 'Invalid grid type specified when reading zone array data.'
            write(outUnit,*) 'Stopping.'
            call ustop(' ')
        end if
    else
        write(outUnit,'(A)') 'The zone value for all cells = 1'
        this%StopZone = 0
        do n = 1, grid%CellCount
            this%Zones(n) = 1
        end do
    end if
      
    ! Retardation array
    read(inUnit, '(a)') line
    icol = 1
    call urword(line, icol, istart, istop, 2, n, r, 0, 0)
    this%RetardationFactorOption = n  
    if(this%RetardationFactorOption .gt. 1) then
        write(outUnit,'(/A)') 'The retardation factor array will be read.'
        if((grid%GridType .eq. 1) .or. (grid%GridType .eq. 3)) then
            call u3ddblmp(inUnit, outUnit, grid%LayerCount, grid%RowCount,      &
              grid%ColumnCount, grid%CellCount, this%Retardation, ANAME(2))            
        else if((grid%GridType .eq. 2) .or. (grid%GridType .eq. 4)) then
            call u3ddblmpusg(inUnit, outUnit, grid%CellCount, grid%LayerCount,            &
              this%Retardation, aname(2), cellsPerLayer)
        else
            write(outUnit,*) 'Invalid grid type specified when reading retardation array data.'
            write(outUnit,*) 'Stopping.'
            call ustop(' ')            
        end if
    else
        write(outUnit,'(/A)') 'The retardation factor for all cells = 1'
        do n = 1, grid%CellCount
            this%Retardation(n) = 1.0d0
        end do
    end if
      
    ! Particle data
    read(inUnit, *) this%ParticleGroupCount
    write(outUnit,'(/A,I5)') 'Number of particle groups = ', this%ParticleGroupCount
  
    seqNumber = 0
    this%TotalParticleCount = 0
    particleCount = 0
    if(this%ParticleGroupCount .gt. 0) then
    allocate(this%ParticleGroups(this%ParticleGroupCount))
    do n = 1, this%ParticleGroupCount
      this%ParticleGroups(n)%Group = n
      read(inUnit, '(a)') this%ParticleGroups(n)%Name
      read(inUnit, *) releaseOption
      
      select case (releaseOption)
          case (1)
              read(inUnit, *) initialReleaseTime
              call this%ParticleGroups(n)%SetReleaseOption1(initialReleaseTime)
          case (2)
              read(inUnit, *) releaseTimeCount, initialReleaseTime, releaseInterval
              call this%ParticleGroups(n)%SetReleaseOption2(initialReleaseTime, &
                releaseTimeCount, releaseInterval)
          case (3)
              read(inUnit, *) releaseTimeCount
              if(allocated(releaseTimes)) deallocate(releaseTimes)
              allocate(releaseTimes(releaseTimeCount))
              read(inUnit, *) (releaseTimes(nn), nn = 1, releaseTimeCount)
              call this%ParticleGroups(n)%SetReleaseOption3(releaseTimeCount,   &
                releaseTimes)
          case default
          ! write error message and stop
          end select
      
      read(inUnit, '(a)') line
      icol = 1
      call urword(line,icol,istart,istop,1,n,r,0,0)
      if(line(istart:istop) .eq. 'EXTERNAL') then
          call urword(line,icol,istart,istop,0,n,r,0,0)
          this%ParticleGroups(n)%LocationFile = line(istart:istop)
          slocUnit = 0
      else if(line(istart:istop) .eq. 'INTERNAL') then
          this%ParticleGroups(n)%LocationFile = ''
          slocUnit = inUnit
      else
          call ustop('Invalid starting locations file name. stop.')
      end if
      call ReadAndPrepareLocations(slocUnit, outUnit, this%ParticleGroups(n),   &
        ibound, grid%CellCount, grid, seqNumber)
      write(outUnit, '(a,i4,a,i10,a)') 'Particle group ', n, ' contains ',      &
        this%ParticleGroups(n)%TotalParticleCount, ' particles.'
      particleCount = particleCount + this%ParticleGroups(n)%TotalParticleCount
    end do
    this%TotalParticleCount = particleCount
    write(outUnit, '(a,i10)') 'Total number of particles = ', this%TotalParticleCount
    write(outUnit, *)
    end if
      
    ! TrackingOptions data
    allocate(this%TrackingOptions)
    ! Initialize defaults
    this%TrackingOptions%DebugMode = .false.
    this%TrackingOptions%BackwardTracking = .false.
    this%TrackingOptions%CreateTrackingLog = .false.
    this%TrackingOptions%StopAtWeakSinks = .false.
    this%TrackingOptions%StopAtWeakSources = .false.
    this%TrackingOptions%ExtendSteadyState = .true.
    this%TrackingOptions%SpecifyStoppingTime = .false.
    this%TrackingOptions%SpecifyStoppingZone = .false.
    this%TrackingOptions%StopTime = this%StopTime
    this%TrackingOptions%StopZone = this%StopZone
    ! Set specific option values
    if(this%TrackingDirection .eq. 2) this%TrackingOptions%BackwardTracking = .true.
    if(this%WeakSinkOption .eq. 2) this%TrackingOptions%StopAtWeakSinks = .true.
    if(this%WeakSourceOption .eq. 2) this%TrackingOptions%StopAtWeakSources = .true.
    if(this%StoppingTimeOption .ne. 2) this%TrackingOptions%ExtendSteadyState = .false.
    if(this%StoppingTimeOption .eq. 3) this%TrackingOptions%SpecifyStoppingTime = .true.
    if(this%ZoneDataOption .eq. 1) this%TrackingOptions%SpecifyStoppingZone = .true.
  
  end subroutine pr_ReadData



end module ModpathSimulationDataModule