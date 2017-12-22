module ParticleGroupModule
  use ParticleModule,only : ParticleType
  !use RectangularUnstructuredGridModule,only : RectangularUnstructuredGridType
  !use ModflowRectangularGridModule,only : ModflowRectangularGridType
  implicit none
  
! Set default access status to private
  private

  type,public :: ParticleGroupType
    integer :: Group
    character(len=16) :: Name
    character(len=200) :: LocationFile
    integer :: TotalParticleCount
    type(ParticleType),dimension(:),allocatable :: Particles
    integer,private :: Count
    integer,private :: ReleaseTimeCount = 1
    integer,private :: ReleaseOption = 1
    doubleprecision,private :: ReleaseInterval = 0.0d0
    doubleprecision,dimension(:),allocatable,private :: ReleaseTimes
  contains
    procedure :: GetSingleReleaseParticleCount=>pr_GetSingleReleaseParticleCount
    procedure :: GetParticleCount=>pr_GetParticleCount
    procedure :: GetReleaseOption=>pr_GetReleaseOption
    procedure :: GetReleaseTimeCount=>pr_GetReleaseTimeCount
    procedure :: GetReleaseTime=>pr_GetReleaseTime
    procedure :: GetReleaseInterval=>pr_GetReleaseInterval
    procedure :: SetReleaseOption1=>pr_SetReleaseOption1
    procedure :: SetReleaseOption2=>pr_SetReleaseOption2
    procedure :: SetReleaseOption3=>pr_SetReleaseOption3
  end type

contains
  
  function pr_GetSingleReleaseParticleCount(this) result(count)
  implicit none
  class(ParticleGroupType) :: this
  integer :: count
  
  count = this%TotalParticleCount / this%ReleaseTimeCount
  
  end function pr_GetSingleReleaseParticleCount
  
  function pr_GetParticleCount(this) result(totalCount)
  implicit none
  class(ParticleGroupType) :: this
  integer :: totalCount
  
  totalCount = this%TotalParticleCount
  
  end function pr_GetParticleCount
  
  function pr_GetReleaseOption(this) result(releaseOption)
  implicit none
  class(ParticleGroupType) :: this
  integer :: releaseOption
  
  releaseOption = this%ReleaseOption
  
  end function pr_GetReleaseOption
  
  function pr_GetReleaseTimeCount(this) result(count)
  implicit none
  class(ParticleGroupType) :: this
  integer :: count
  
  count = this%ReleaseTimeCount
  
  end function pr_GetReleaseTimeCount
  
  function pr_GetReleaseTime(this, releaseTimeIndex) result(releaseTime)
  implicit none
  class(ParticleGroupType) :: this
  integer,intent(in) :: releaseTimeIndex
  doubleprecision :: releaseTime
  
  if(.not. allocated(this%ReleaseTimes)) then
      this%ReleaseOption = 1
      this%ReleaseTimeCount = 1
      this%ReleaseInterval = 0.0d0
      allocate(this%ReleaseTimes(1))
  end if
  
  if(releaseTimeIndex.lt.1 .or. releaseTimeIndex.gt.this%ReleaseTimeCount) then
      stop
  end if
  releaseTime = this%ReleaseTimes(releaseTimeIndex)
  
  end function pr_GetReleaseTime
  
  function pr_GetReleaseInterval(this) result(releaseInterval)
  implicit none
  class(ParticleGroupType) :: this
  doubleprecision :: releaseInterval
  
  releaseInterval = this%ReleaseInterval
  
  end function pr_GetReleaseInterval
  
  subroutine pr_SetReleaseOption1(this, releaseTime)
  implicit none
  class(ParticleGroupType) :: this
  doubleprecision,intent(in) :: releaseTime
  
  if(allocated(this%ReleaseTimes)) deallocate(this%ReleaseTimes)
  allocate(this%ReleaseTimes(1))
  this%ReleaseOption = 1
  this%ReleaseTimeCount = 1
  this%ReleaseInterval = 0.0d0
  this%ReleaseTimes(1) = releaseTime
  
  end subroutine pr_SetReleaseOption1

  subroutine pr_SetReleaseOption2(this, initialReleaseTime, releaseTimeCount, releaseInterval)
  implicit none
  class(ParticleGroupType) :: this
  doubleprecision,intent(in) :: initialReleaseTime, releaseInterval
  integer,intent(in) :: releaseTimeCount
  integer :: n
  
  if(allocated(this%ReleaseTimes)) deallocate(this%ReleaseTimes)
  allocate(this%ReleaseTimes(releaseTimeCount))
  this%ReleaseOption = 2
  this%ReleaseTimeCount = releaseTimeCount
  this%ReleaseInterval = releaseInterval
  
  this%ReleaseTimes(1) = initialReleaseTime
  do n = 2, releaseTimeCount
      this%ReleaseTimes(n) = this%ReleaseTimes(n - 1) + releaseInterval
  end do
  
  end subroutine pr_SetReleaseOption2

  subroutine pr_SetReleaseOption3(this, releaseTimeCount, releaseTimes)
  implicit none
  class(ParticleGroupType) :: this
  integer,intent(in) :: releaseTimeCount
  doubleprecision,dimension(releaseTimeCount),intent(in) :: releaseTimes
  integer :: n
  
  if(allocated(this%ReleaseTimes)) deallocate(this%ReleaseTimes)
  allocate(this%ReleaseTimes(releaseTimeCount))
  this%ReleaseOption = 3
  this%ReleaseTimeCount = releaseTimeCount
  this%ReleaseInterval = 0.0d0
  
  do n = 1, releaseTimeCount
      this%ReleaseTimes(n) = releaseTimes(n)
  end do
  
  end subroutine pr_SetReleaseOption3

end module ParticleGroupModule