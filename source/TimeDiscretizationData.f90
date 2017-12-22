module TimeDiscretizationDataModule
  implicit none
  
! Set default access status to private
  private
  
! Public derived data type definitions
!--------------------------------------
! type: 
!--------------------------------------
  type,public :: TimeDiscretizationDataType
    integer :: StressPeriodCount = 0
    integer :: CumulativeTimeStepCount = 0
    integer,dimension(:),allocatable :: TimeStepCounts
    doubleprecision,dimension(:),allocatable :: StressPeriodLengths, Multipliers
    logical,dimension(:),allocatable :: SteadyStateFlags, StressPeriodTypes
    doubleprecision,dimension(:),allocatable :: TotalTimes
    integer,dimension(:),allocatable,private :: StepOffsets
  contains
    procedure,private :: ReadFileType1
    procedure,private :: ReadFileType2
    generic :: ReadData => ReadFileType1, ReadFileType2
    procedure :: GetPeriodAndStep
    procedure :: GetCumulativeTimeStep
    procedure :: ComputeTotalTimes
    procedure :: FindContainingTimeStep
    procedure :: GetTimeFromPeriodAndStep
  end type


contains
  
  function GetTimeFromPeriodAndStep(this, stressPeriod, timeStep, relativeTime) result(time)
!***************************************************************************************************************
! Description goes here
!***************************************************************************************************************
!
! Specifications
!---------------------------------------------------------------------------------------------------------------
  implicit none
  class(TimeDiscretizationDataType) :: this
  integer,intent(in) :: stressPeriod, timeStep
  doubleprecision,intent(in) :: relativeTime
  doubleprecision :: time, time1, time2
  integer :: cStep
!---------------------------------------------------------------------------------------------------------------
  
  cStep = this%GetCumulativeTimeStep(stressPeriod, timeStep)
  time2 = this%TotalTimes(cStep)
  if(cStep .eq. 1) then
      time1 = 0.0d0
  else
      time1 = this%TotalTimes(cStep-1)
  end if
  
  time = (1.0d0 - relativeTime) * time1 + relativeTime * time2
  if(time .lt. time1) time = time1
  if(time .gt. time2) time = time2
  
  end function GetTimeFromPeriodAndStep
  
  function FindContainingTimeStep(this, time) result(step)
!***************************************************************************************************************
! Description goes here
!***************************************************************************************************************
!
! Specifications
!---------------------------------------------------------------------------------------------------------------
  implicit none
  class(TimeDiscretizationDataType) :: this
  doubleprecision,intent(in) :: time
  integer :: step, n
!---------------------------------------------------------------------------------------------------------------
  
  step = 0
  if(time .lt. 0.0d0) return
  if(time .gt. this%TotalTimes(this%CumulativeTimeStepCount)) return
  
  if(time .le. this%TotalTimes(1)) then
      step = 1
      return
  end if
  
  do n = 2, this%CumulativeTimeStepCount
      if((time .ge. this%TotalTimes(n-1)) .and. (time .le. this%TotalTimes(n))) then
          step = n
          return
      end if
  end do
  
  end function FindContainingTimeStep
  
  subroutine GetPeriodAndStep(this, cumulativeStep, stressPeriod, timeStep) 
!***************************************************************************************************************
! Description goes here
!***************************************************************************************************************
!
! Specifications
!---------------------------------------------------------------------------------------------------------------
  implicit none
  class(TimeDiscretizationDataType) :: this
  integer,intent(in) :: cumulativeStep
  integer,intent(inout) :: stressPeriod, timeStep
  integer :: step, n
!---------------------------------------------------------------------------------------------------------------
 
  stressPeriod = 0
  timeStep = 0
  if(cumulativeStep .lt. 1) return
  if(cumulativeStep .gt. this%CumulativeTimeStepCount) return
  
  do n = 1, this%StressPeriodCount
      step = cumulativeStep - this%StepOffsets(n)
      if(step .le. this%TimeStepCounts(n)) then
          stressPeriod = n
          timeStep = step
          return
      end if
  end do
  
  end subroutine GetPeriodAndStep
  
  function GetCumulativeTimeStep(this, stressPeriod, timeStep) result(step)
!***************************************************************************************************************
! Description goes here
!***************************************************************************************************************
!
! Specifications
!---------------------------------------------------------------------------------------------------------------
  implicit none
  class(TimeDiscretizationDataType) :: this
  integer,intent(in) :: stressPeriod, timeStep
  integer :: step
  
  step = 0
  if(timeStep .lt. 1) return
  if(timeStep .gt. this%TimeStepCounts(stressPeriod)) return
  
  step = this%StepOffsets(stressPeriod) + timeStep
  
  end function GetCumulativeTimeStep
  
  subroutine ReadFileType1(this, inUnit, outUnit, stressPeriodCount)
!***************************************************************************************************************
! Description goes here
!***************************************************************************************************************
!
! Specifications
!---------------------------------------------------------------------------------------------------------------
  use utl7module,only : upcase
  implicit none
  class(TimeDiscretizationDataType) :: this
  integer,intent(in) :: inUnit, outUnit, stressPeriodCount
  integer :: n, period, step, stepCount, cStep, cStepCount
  doubleprecision :: dt, mult, perlen
  character(len=2) :: ssFlag
!---------------------------------------------------------------------------------------------------------------
  
  if(allocated(this%StressPeriodLengths)) deallocate(this%StressPeriodLengths)
  if(allocated(this%TimeStepCounts)) deallocate(this%TimeStepCounts)
  if(allocated(this%Multipliers)) deallocate(this%Multipliers)
  if(allocated(this%SteadyStateFlags)) deallocate(this%SteadyStateFlags)
  if(allocated(this%StressPeriodTypes)) deallocate(this%StressPeriodTypes)
  if(allocated(this%StepOffsets)) deallocate(this%StepOffsets)
  if(allocated(this%TotalTimes)) deallocate(this%TotalTimes)
  
  this%StressPeriodCount = stressPeriodCount
  write(outUnit, '(1x,a,i10)') 'Stress period count = ', this%StressPeriodCount  
  
  allocate(this%TimeStepCounts(this%StressPeriodCount))
  allocate(this%StressPeriodLengths(this%StressPeriodCount))
  allocate(this%Multipliers(this%StressPeriodCount))
  allocate(this%StressPeriodTypes(this%StressPeriodCount))
  allocate(this%StepOffsets(this%StressPeriodCount))
  
  ! Write header to the listing file
  write(outUnit, *)
  write(outUnit, '(1x,a)') 'Time discretization file data'
  write(outUnit, '(1x,a)') '-----------------------------'

  cStepCount = 0
  do n = 1, this%StressPeriodCount
      read(inUnit, *) this%StressPeriodLengths(n), this%TimeStepCounts(n), this%Multipliers(n), ssFlag
      call upcase(ssFlag)
      this%StressPeriodTypes(n) = .true.
      if(ssFlag .eq. 'TR') this%StressPeriodTypes(n) = .false.
      cStepCount = cStepCount + this%TimeStepCounts(n)
      
      write(outUnit, '(1x,a,f15.5,a,i10,a,f15.5,a,a)')                          &
        'Period length:', this%StressPeriodLengths(n),                          &
        ' Time step count:', this%TimeStepCounts(n),                            &
        ' Time step multiplier:', this%Multipliers(n),                          &
        ' Stress period type:', ssFlag
  end do
  
  this%CumulativeTimeStepCount = cStepCount
  allocate(this%TotalTimes(this%CumulativeTimeStepCount))
  allocate(this%SteadyStateFlags(this%CumulativeTimeStepCount))
  
  this%StepOffsets(1) = 0
  cStep = 0
  do period = 1, this%StressPeriodCount
      if(period .gt. 1) then
          this%StepOffsets(period) = this%StepOffsets(period - 1) + this%TimeStepCounts(period - 1)
      end if
      do step = 1, this%TimeStepCounts(period)
          cStep = cStep + 1
          this%SteadyStateFlags(cStep) = this%StressPeriodTypes(period)
      end do
  end do
  
  call this%ComputeTotalTimes()
  
  end subroutine ReadFileType1
  
  subroutine ReadFileType2(this, inUnit, outUnit)
!***************************************************************************************************************
! Description goes here
!***************************************************************************************************************
!
! Specifications
!---------------------------------------------------------------------------------------------------------------
  use utl7module,only : upcase
  use UTL8MODULE,only : uget_block, uterminate_block, u8rdcom, urword, ustop
  implicit none
  class(TimeDiscretizationDataType) :: this
  integer,intent(in) :: inUnit, outUnit
  integer :: n, period, step, stepCount, cStep, cStepCount, ierr, lloc, nval
  integer :: istart, istop, ncode
  doubleprecision :: dt, mult, perlen, rval
  character(len=2) :: ssFlag
  character(len=15) :: ctag
  character(len=132) :: line
  logical :: isfound
!---------------------------------------------------------------------------------------------------------------
  
  if(allocated(this%StressPeriodLengths)) deallocate(this%StressPeriodLengths)
  if(allocated(this%TimeStepCounts)) deallocate(this%TimeStepCounts)
  if(allocated(this%Multipliers)) deallocate(this%Multipliers)
  if(allocated(this%SteadyStateFlags)) deallocate(this%SteadyStateFlags)
  if(allocated(this%StressPeriodTypes)) deallocate(this%StressPeriodTypes)
  if(allocated(this%StepOffsets)) deallocate(this%StepOffsets)
  if(allocated(this%TotalTimes)) deallocate(this%TotalTimes)
  
  ! Read OPTIONS block
  call uget_block(inUnit, outUnit, 'OPTIONS', ierr, isfound, lloc, line)
  if(isfound) then
      do
          call u8rdcom(inUnit, outUnit, line, ierr)
          lloc = 1
          call urword(line, lloc, istart, istop, 1, nval, rval, 0, 0)
          select case(line(istart:istop))
              case('TIME_UNITS')
                  call urword(line, lloc, istart, istop, 1, nval, rval, 0, 0)
                  ! Ignore the time units parameter for now.
              case('END','BEGIN')
                  call uterminate_block(inUnit, outUnit, line(istart:istop), 'OPTIONS', lloc, line, ierr)
                  if(ierr .eq. 0) exit
              case default
                call ustop('Unrecognized keyword in OPTIONS block of TDIS file. Stop.')
          end select        
      end do
  else
      call ustop('OPTIONS block not found in TDIS file. Stop.')
  end if
  
  ! Read DIMENSIONS block
  call uget_block(inUnit, outUnit, 'DIMENSIONS', ierr, isfound, lloc, line)
  if(isfound) then
      do
          call u8rdcom(inUnit, outUnit, line, ierr)
          lloc = 1
          call urword(line, lloc, istart, istop, 1, nval, rval, 0, 0)
          select case(line(istart:istop))
            case('NPER')
                call urword(line, lloc, istart, istop, 2, nval, rval, 0, 0)
                this%StressPeriodCount = nval
            case('END','BEGIN')
                call uterminate_block(inUnit, outUnit, line(istart:istop), 'DIMENSIONS', lloc, line, ierr)
                if(ierr .eq. 0) exit
            case default
                call ustop('Unrecognized keyword in DIMENSIONS block of TDIS file. Stop.')
          end select        
      end do
  else
      call ustop('DIMENSIONS block not found in TDIS file. Stop.')
  end if
  
  ! Allocate arrays
  allocate(this%TimeStepCounts(this%StressPeriodCount))
  allocate(this%StressPeriodLengths(this%StressPeriodCount))
  allocate(this%Multipliers(this%StressPeriodCount))
  allocate(this%StressPeriodTypes(this%StressPeriodCount))
  allocate(this%StepOffsets(this%StressPeriodCount))
  
  ! Write header to the listing file
  write(outUnit, *)
  write(outUnit, '(1x,a)') 'Time discretization file data'
  write(outUnit, '(1x,a)') '-----------------------------'
  write(outUnit, '(1x,a,i10)') 'Stress period count = ', this%StressPeriodCount  

  ! Read STRESS_PERIODS block (in newer versions this is now PERIODDATA)
  call uget_block(inUnit, outUnit, 'PERIODDATA', ierr, isfound, lloc, line)
  if(isfound) then
     cStepCount = 0
     do n = 1, this%StressPeriodCount
          lloc = 1
          call u8rdcom(inUnit, outUnit, line, ierr)
          call urword(line, lloc, istart, istop, 3, nval, rval, 0, 0)
          this%StressPeriodLengths(n) = rval
          call urword(line, lloc, istart, istop, 2, nval, rval, 0, 0)
          this%TimeStepCounts(n) = nval
          call urword(line, lloc, istart, istop, 3, nval, rval, 0, 0)
          this%Multipliers(n) = rval
          cStepCount = cStepCount + this%TimeStepCounts(n)
          ! Set the stress period type to TRUE to indicate steady state. 
          ! This will be modified as needed later when the storage package file is read.
          this%StressPeriodTypes(n) = .true.
          write(outUnit, '(1x,a,f15.5,a,i10,a,f15.5)')                              &
            'Period length:', this%StressPeriodLengths(n),                          &
            ' Time step count:', this%TimeStepCounts(n),                            &
            ' Time step multiplier:', this%Multipliers(n)                           
      end do
      lloc = 1
      call u8rdcom(inUnit, outUnit, line, ierr)
      call urword(line, lloc, istart, istop, 1, nval, rval, 0, 0)
      call uterminate_block(inUnit, outUnit, line(istart:istop), 'PERIODDATA', lloc, line, ierr)
      if(ierr .ne. 0) then
          call ustop('Improper termination of the STRESS_PERIODS block in the TDIS file. Stop.')
      end if
  else
      call ustop('STRESS_PERIODS block not found in TDIS file. Stop.')
  end if
  
  this%CumulativeTimeStepCount = cStepCount
  allocate(this%TotalTimes(this%CumulativeTimeStepCount))
  allocate(this%SteadyStateFlags(this%CumulativeTimeStepCount))
  
  this%StepOffsets(1) = 0
  cStep = 0
  do period = 1, this%StressPeriodCount
      if(period .gt. 1) then
          this%StepOffsets(period) = this%StepOffsets(period - 1) + this%TimeStepCounts(period - 1)
      end if
      do step = 1, this%TimeStepCounts(period)
          cStep = cStep + 1
          this%SteadyStateFlags(cStep) = this%StressPeriodTypes(period)
      end do
  end do
  
  call this%ComputeTotalTimes()
  
  end subroutine ReadFileType2
  
  subroutine ComputeTotalTimes(this)
!***************************************************************************************************************
! Description goes here
!***************************************************************************************************************
!
! Specifications
!---------------------------------------------------------------------------------------------------------------
  implicit none
  class(TimeDiscretizationDataType) :: this
  integer :: n, period, step, stepCount, cStep
  doubleprecision :: dt, mult, perlen
!---------------------------------------------------------------------------------------------------------------
  cStep = 0
  do period = 1, this%StressPeriodCount
      mult = this%Multipliers(period)
      perlen = this%StressPeriodLengths(period)
      stepCount = this%TimeStepCounts(period)
      do step = 1, this%TimeStepCounts(period)
          if(step .eq. 1) then
              if(mult .eq. 1.0d0) then
                  dt = perlen / dble(stepCount)
              else
                  dt = perlen * (mult - 1.0d0)/(mult**stepCount - 1.0d0)
              end if
          else
              dt = mult * dt
          end if
          
          cStep = cStep + 1
          if(cStep .eq. 1) then
              this%TotalTimes(cStep) = dt
          else
              this%TotalTimes(cStep) = this%TotalTimes(cStep-1) + dt
          end if
      end do
      
  end do

  end subroutine ComputeTotalTimes

end module TimeDiscretizationDataModule