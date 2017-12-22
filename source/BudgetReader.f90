module BudgetReaderModule
  use UTL8MODULE,only : freeunitnumber
  use BudgetRecordHeaderModule,only : BudgetRecordHeaderType
  use BudgetListItemModule,only : BudgetListItemType
  use UtilMiscModule,only : TrimAll
  implicit none
  
! Set default access status to private
  private
  
! Public derived data type definitions
!--------------------------------------
! type: 
!--------------------------------------
  type,public :: BudgetReaderType
    ! Public variables -- none
    
    ! Private variables
    integer,private :: FileUnit = -1
    character(len=:),private,allocatable :: Filename
    integer,private :: OutputUnit = 0
    integer,private :: PrecisionType = 0
    integer,private :: BudgetType = 0
    integer,private :: FileFormat = 0
    integer,private :: MaximumArrayItemCount = 0
    integer,private :: MaximumListItemCount = 0
    integer,private :: FlowArraySize = 0
    type(BudgetRecordHeaderType),private,allocatable,dimension(:) :: RecordHeaders
    
  contains
    ! Public procedures
    procedure :: OpenBudgetFile=>pr_OpenBudgetFile
    procedure :: CloseBudgetFile=>pr_CloseBudgetFile
    procedure :: GetFileOpenStatus=>pr_GetFileOpenStatus
    procedure :: GetFileUnit=>pr_GetFileUnit
    procedure :: GetFilename=>pr_GetFilename
    procedure :: GetOutputUnit=>pr_GetOutputUnit
    procedure :: GetPrecisionType=>pr_GetPrecisionType
    procedure :: GetBudgetType=>pr_GetBudgetType
    procedure :: GetBudgetFileFormat=>pr_GetBudgetFileFormat
    procedure :: GetFlowArraySize=>pr_GetFlowArraySize
    procedure :: GetMaximumArrayItemCount=>pr_GetMaximumArrayItemCount
    procedure :: GetMaximumListItemCount=>pr_GetMaximumListItemCount
    procedure :: GetRecordHeaderRange=>pr_GetRecordHeaderRange
    procedure :: GetRecordHeaderCount=>pr_GetRecordHeaderCount
    procedure :: GetRecordHeader=>pr_GetRecordHeader
    procedure :: GetRecordHeaderIndex=>pr_GetRecordHeaderIndex
    generic   :: FillRecordDataBuffer=>pr_FillRecordDataBufferMethod_0_1_4,     &
      pr_FillRecordDataBufferMethod_3, pr_FillRecordDataBufferMethod_2_5_6
    
    ! Private procedures
    procedure,private :: ReadRecordHeader=>pr_ReadRecordHeader
    procedure,private :: ProcessRecordHeaders=>pr_ProcessRecordHeaders
    procedure,private :: pr_FillRecordDataBufferMethod_0_1_4
    procedure,private :: pr_FillRecordDataBufferMethod_3
    procedure,private :: pr_FillRecordDataBufferMethod_2_5_6
  end type

    contains

  function pr_GetRecordHeaderIndex(this, stressPeriod, timeStep, label) result(index)
  implicit none
  class(BudgetReaderType) :: this
  integer,intent(in) :: stressPeriod, timeStep
  character*(*),intent(in) :: label
  integer :: index, n, labelFirst, labelLast, headerLabelFirst, headerLabelLast, trimmedLength
  
  index = -1
  call TrimAll(label, labelFirst, labelLast, trimmedLength)
  do n = 1, this%GetRecordHeaderCount()
      call TrimAll(this%RecordHeaders(n)%TextLabel, headerLabelFirst, headerLabelLast, trimmedLength)
      if(labelFirst.gt.0 .and. labelLast.gt.0) then
          if(label(labelFirst:labelLast) .eq. this%RecordHeaders(n)%TextLabel(headerLabelFirst:headerLabelLast)) then
              index = n
              return
          end if
      end if
  end do
  
  end function pr_GetRecordHeaderIndex
  
!---------------------------------------------------------
  function pr_GetRecordHeader(this, recordIndex) result(recordHeader)
  implicit none
  class(BudgetReaderType) :: this
  integer,intent(in) :: recordIndex
  type(BudgetRecordHeaderType) :: recordHeader

  recordHeader = this%RecordHeaders(recordIndex)

  end function pr_GetRecordHeader
  
!---------------------------------------------------------
  function pr_GetRecordHeaderCount(this) result(count)
  implicit none
  class(BudgetReaderType) :: this
  integer :: count
  
  count = 0
  if(allocated(this%RecordHeaders)) then
      count = size(this%RecordHeaders)
  end if  
  
  end function pr_GetRecordHeaderCount
  
!---------------------------------------------------------
  subroutine pr_GetRecordHeaderRange(this, stressPeriod, timeStep, firstRecord, lastRecord)
  implicit none
  class(BudgetReaderType) :: this
  integer,intent(in) :: stressPeriod,timeStep
  integer,intent(inout) :: firstRecord,lastRecord
  integer :: count,n,period,step,recCount
  
  ! Find the range of header records
  count = 0
  firstRecord = 0
  lastRecord = 0
  recCount = this%GetRecordHeaderCount()
  do n = 1, recCount
      period = this%RecordHeaders(n)%StressPeriod
      step = this%RecordHeaders(n)%TimeStep
      
      if(firstRecord .gt. 0) then
          if((period .eq. stressPeriod) .and. (step .eq. timeStep)) then
              if(n .eq. recCount) then
                  lastRecord = n
              end if
          else
              lastRecord = n - 1
          end if
      else
          if((period .eq. stressPeriod) .and. (step .eq. timeStep)) then
              firstRecord = n
          end if
      end if
      if(lastRecord .gt. 0) exit
      
  end do
  
  end subroutine pr_GetRecordHeaderRange
  
!---------------------------------------------------------
  function pr_GetPrecisionType(this) result(precisionType)
  implicit none
  class(BudgetReaderType) :: this
  integer :: precisionType
  
  precisionType = this%PrecisionType
  
  end function pr_GetPrecisionType

!---------------------------------------------------------
  function pr_GetBudgetType(this) result(budgetType)
  implicit none
  class(BudgetReaderType) :: this
  integer :: budgetType
  
  budgetType = this%BudgetType
  
  end function pr_GetBudgetType

!---------------------------------------------------------
  function pr_GetBudgetFileFormat(this) result(fileFormat)
  implicit none
  class(BudgetReaderType) :: this
  integer :: fileFormat
  
  fileFormat = this%FileFormat
  
  end function pr_GetBudgetFileFormat

!---------------------------------------------------------
  function pr_GetFlowArraySize(this) result(bufferSize)
  implicit none
  class(BudgetReaderType) :: this
  integer :: bufferSize
  
  bufferSize = this%FlowArraySize
  
  end function pr_GetFlowArraySize

!---------------------------------------------------------
  function pr_GetMaximumArrayItemCount(this) result(bufferSize)
  implicit none
  class(BudgetReaderType) :: this
  integer :: bufferSize
  
  bufferSize = this%MaximumArrayItemCount
  
  end function pr_GetMaximumArrayItemCount

!---------------------------------------------------------
  function pr_GetMaximumListItemCount(this) result(bufferSize)
  implicit none
  class(BudgetReaderType) :: this
  integer :: bufferSize
  
  bufferSize = this%MaximumListItemCount
  
  end function pr_GetMaximumListItemCount

!---------------------------------------------------------
  function pr_GetOutputUnit(this) result(unit)
  implicit none
  class(BudgetReaderType) :: this
  integer :: unit
  
  unit = this%OutputUnit
  
  end function pr_GetOutputUnit
  
!---------------------------------------------------------
  subroutine pr_FillRecordDataBufferMethod_0_1_4(this,header,buffer,bufferSize,spaceAssigned,status)
  implicit none
  class(BudgetReaderType) :: this
  class(BudgetRecordHeaderType),intent(in) :: header
  integer,intent(in) :: bufferSize
  integer,intent(inout) :: spaceAssigned,status
  doubleprecision,intent(inout),dimension(bufferSize) :: buffer
  integer :: n, requiredSize
  integer(kind=8) :: position
  real(kind=4) :: valueSingle
  
  status = 0
  spaceAssigned = 0
  
  ! Clear buffer 
  do n = 1, bufferSize
      buffer(n) = 0.0d0
  end do
  
  ! If the budget type is undefined, set status = 1 and return
  if(this%BudgetType .eq. 0) then
      status = 1
      return
  end if
  
  ! If the method specified in the header is not 0, 1, or 4 set status = 2 and return.
  if((header%Method .ne. 0) .and. (header%Method .ne. 1) .and. (header%Method .ne. 4)) then
      status = 2
      return
  end if
  
  ! Find the buffer size that is needed to hold the data
  if(header%Method .eq. 4) then
      requiredSize = header%RowCount * header%ColumnCount
  else
      requiredSize = header%RowCount * header%ColumnCount * header%LayerCount
  end if
  
  ! Check to see if the buffer is large enough to hold the data. If not, set status = 3 and return
  if(requiredSize .gt. bufferSize) then
      status = 3
      return
  end if
  
  ! Read data and fill buffers
  ! Calculate the starting position for the data to be read
  position = header%HeaderPosition + header%HeaderOffset
  
  ! Select the precision type and read the data
  select case (this%PrecisionType)
      case (1)
          read(this%FileUnit,pos=position,err=100) 
          do n = 1, requiredSize
              read(this%FileUnit,err=100) valueSingle
              buffer(n) = dble(valueSingle)
          end do
          spaceAssigned = requiredSize
      case (2)
          read(this%FileUnit,pos=position,err=100) 
          do n = 1, requiredSize
              read(this%FileUnit,err=100) buffer(n)
          end do
          spaceAssigned = requiredSize
      case default
          ! The budget precision is undefined. Set status = 4 and return
          status = 4
          return
  end select
  
  return
! Process read error
100 continue
  status = 5
  
  end subroutine pr_FillRecordDataBufferMethod_0_1_4

!---------------------------------------------------------
  subroutine pr_FillRecordDataBufferMethod_3(this,header,buffer,indexBuffer,bufferSize,spaceAssigned,status)
  implicit none
  class(BudgetReaderType) :: this
  class(BudgetRecordHeaderType),intent(in) :: header
  integer,intent(in) :: bufferSize
  integer,intent(inout) :: spaceAssigned,status
  doubleprecision,intent(inout),dimension(bufferSize) :: buffer
  integer,intent(inout),dimension(bufferSize) :: indexBuffer
  integer :: n, requiredSize
  integer(kind=8) :: position
  real(kind=4) :: valueSingle
  
  status = 0
  spaceAssigned = 0
  
  ! Clear buffer 
  do n = 1, bufferSize
      buffer(n) = 0.0d0
      indexBuffer(n) = 0
  end do
  
  ! If the budget type is undefined, set status = 1 and return
  if(this%BudgetType .eq. 0) then
      status = 1
      return
  end if
  
  ! If the method specified in the header is not equal to 3 set status = 2 and return.
  if(header%Method .ne. 3) then
      status = 2
      return
  end if
  
  ! Find the buffer size that is needed to hold the data
  requiredSize = header%RowCount * header%ColumnCount
  
  ! Check to see if the buffer is large enough to hold the data. If not, set status = 3 and return
  if(requiredSize .gt. bufferSize) then
      status = 3
      return
  end if
  
  ! Read data and fill buffers
  ! Calculate the starting position for the data to be read
  position = header%HeaderPosition + header%HeaderOffset
  
  ! Select the precision type and read the data
  select case (this%PrecisionType)
      case (1)
          read(this%FileUnit,pos=position,err=100) 
          
          do n = 1, requiredSize
              read(this%FileUnit,err=100) indexBuffer(n)
          end do
          
          do n = 1, requiredSize
              read(this%FileUnit,err=100) valueSingle
              buffer(n) = dble(valueSingle)
          end do
          
      case (2)
          read(this%FileUnit,pos=position,err=100) 
          
          do n = 1, requiredSize
              read(this%FileUnit,err=100) indexBuffer(n)
          end do
          
          do n = 1, requiredSize
              read(this%FileUnit,err=100) buffer(n)
          end do
          
      case default
          ! The budget precision is undefined. Set status = 4 and return
          status = 4
          return
  end select
  
  return
! Process read error
100 continue
  status = 5
  
  end subroutine pr_FillRecordDataBufferMethod_3

!---------------------------------------------------------
  subroutine pr_FillRecordDataBufferMethod_2_5_6(this, header, listBuffer, bufferSize, spaceAssigned, status)
  implicit none
  class(BudgetReaderType) :: this
  type(BudgetRecordHeaderType),intent(in) :: header
  integer,intent(in) :: bufferSize
  type(BudgetListItemType),intent(inout),dimension(bufferSize) :: listBuffer
  integer,intent(inout) :: spaceAssigned,status
  integer :: n, m, requiredSize, auxNamesCount, cellNumber, id2
  integer(kind=8) :: position
  real(kind=4) :: singleValue
  
  status = 0
  spaceAssigned = 0
  
  ! Calculate the starting position for the data to be read
  position = header%HeaderPosition + header%HeaderOffset
  
  ! Select the precision type and read the data
  requiredSize = header%ListItemCount
  auxNamesCount = header%GetAuxiliaryNamesCount()
  listBuffer%AuxiliaryValueCount = auxNamesCount
  
  if(header%ListItemValueCount .lt. 1) goto 100
  if((auxNamesCount + 1) .ne. header%ListItemValueCount) goto 100
  
  select case (this%PrecisionType)
      case (1)
          read(this%FileUnit,pos=position,err=100) 
          do n = 1, requiredSize
               read(this%FileUnit, err = 100) listBuffer(n)%CellNumber
               listBuffer(n)%ID2 = 0
               if(header%Method .eq. 6) read(this%FileUnit, err = 100) listBuffer(n)%ID2
               read(this%FileUnit, err = 100) singleValue
               listBuffer(n)%BudgetValue = dble(singleValue)
               if(auxNamesCount .gt. 0) then
                   do m = 1, auxNamesCount
                       read(this%FileUnit, err = 100) singleValue
                       listBuffer(n)%AuxiliaryValues(m) = dble(singleValue)
                   end do   
               end if            
          end do
          spaceAssigned = requiredSize
      case (2)
          read(this%FileUnit,pos=position,err=100) 
          do n = 1, requiredSize
               read(this%FileUnit, err = 100) listBuffer(n)%CellNumber
               listBuffer(n)%ID2 = 0
               if(header%Method .eq. 6) read(this%FileUnit, err = 100) listBuffer(n)%ID2
               read(this%FileUnit, err = 100) listBuffer(n)%BudgetValue
               if(auxNamesCount .gt. 0) then
                   do m = 1, auxNamesCount
                       read(this%FileUnit, err = 100) listBuffer(n)%AuxiliaryValues(m)
                   end do     
               end if          
          end do
          spaceAssigned = requiredSize
      case default
          ! The budget precision is undefined. Set status = 4 and return
          status = 4
          return
  end select
  
  return
! Process read error
100 continue
  status = 5
  spaceAssigned = 0
  
  end subroutine pr_FillRecordDataBufferMethod_2_5_6
  
!---------------------------------------------------------
  subroutine pr_OpenBudgetFile(this,filename, inUnit, outputUnit)
  implicit none
  class(BudgetReaderType) :: this
  character(len=*),intent(in) :: filename
  integer,intent(in) :: inUnit, outputUnit
  integer(kind=8) :: fileLength
  integer :: recordCount
  character(len=:),allocatable :: openFileMessage
  logical :: fileExists
  
  ! Close any open budget file first
  call this%CloseBudgetFile()
  this%PrecisionType = 0
  this%BudgetType = 0
  this%FileFormat = 0 
  
  this%Filename = trim(filename)
  inquire(file=this%Filename, exist=fileExists)
  if(.not. fileExists) then
      write(*, '(1x,a)') 'Budget file does not exist.'
      goto 100
  end if
  
  this%FileUnit = inUnit
  this%OutputUnit = outputUnit
  
  ! Find a free file unit number and open the file for read unformatted stream access
!  call freeunitnumber(this%FileUnit)
  openFileMessage = ''
  open(unit=this%FileUnit,file=this%Filename,form='unformatted',                &
       access='stream',status='old',action='read',iomsg=openFileMessage,        &
       err=100)
  
  ! Find file size
  inquire(unit=this%FileUnit,size=fileLength)
  
  ! Try to read as single precision
  call this%ProcessRecordHeaders(1)
  
  ! If the precision type is still undefined, try to read as double precision
  if(this%PrecisionType .eq. 0) then
      call this%ProcessRecordHeaders(2)
  end if
  
  ! If PrecisionType, BudgetType, or FileFormat are still undefined it means the file 
  ! is not a valid MODFLOW budget file.
  if((this%PrecisionType .eq. 0) .or. (this%BudgetType .eq. 0) .or. (this%FileFormat .eq. 0)) then
      ! Process the error and return.
      goto 100
  end if
  
  ! The budget file was opened successfully
  return
  
100 continue
  call this%CloseBudgetFile()
    
  end subroutine pr_OpenBudgetFile

!---------------------------------------------------------
  subroutine pr_CloseBudgetFile(this)
  implicit none
  class(BudgetReaderType) :: this
  
  if(this%FileUnit .ge. 0) then
      close(this%FileUnit)
  end if
  this%FileUnit = -1
  this%Filename = ''
  this%OutputUnit = 0
  this%MaximumArrayItemCount = 0
  this%MaximumListItemCount = 0
  if(allocated(this%RecordHeaders)) deallocate(this%RecordHeaders)
  allocate(this%RecordHeaders(0))
  
  end subroutine pr_CloseBudgetFile

!---------------------------------------------------------
  function pr_GetFileOpenStatus(this) result(fileOpened)
  implicit none
  class(BudgetReaderType) :: this
  logical :: fileOpened
  
  fileOpened = .false.
  if(this%FileUnit .ge. 0) then
      inquire(unit=this%FileUnit, opened=fileOpened)
  end if
  
  end function pr_GetFileOpenStatus

!---------------------------------------------------------
  function pr_GetFileUnit(this) result(unitNumber)
  implicit none
  class(BudgetReaderType) :: this
  integer :: unitNumber
  
  unitNumber = this%FileUnit
  
  end function pr_GetFileUnit

!---------------------------------------------------------
  function pr_GetFilename(this) result(name)
  implicit none
  class(BudgetReaderType) :: this
  character(len=:),allocatable :: name
  
  if(.not. allocated(this%Filename)) this%Filename = ''
  name = this%Filename
  
  end function pr_GetFilename

!---------------------------------------------------------
  subroutine pr_ReadRecordHeader(this, position, header, precisionType)
  implicit none
  class(BudgetReaderType) :: this
  type(BudgetRecordHeaderType),intent(inout) :: header
  integer(kind=8),intent(in) :: position
  integer,intent(in) :: precisionType
  integer(kind=8) :: headerOffset
  integer(kind=8) :: layerCountLong,rowCountLong,columnCountLong,listItemValueCountLong,listItemCountLong
  integer :: auxCount,n,realBytes,intBytes
  real(kind=4) :: timeStepLength,stressPeriodLength, totalTime
  real(kind=8) :: timeStepLengthDbl,stressPeriodLengthDbl, totalTimeDbl
  
  call header%Initialize()
  
  intBytes = 4
  if(precisionType .eq. 1) then
  realBytes = 4
  else if(precisionType .eq. 2) then
      realBytes = 8
  else
      return
  end if
  
  ! Set position
  headerOffset = 0
  read(this%FileUnit, pos=position, err=100)
  read(this%FileUnit, err=100) header%TimeStep, header%StressPeriod,            &
    header%TextLabel, header%ColumnCount, header%RowCount, header%LayerCount
  headerOffset = headerOffset + 16 + (5 * intBytes)
  
  if(header%LayerCount .lt. 0) then
      header%LayerCount = -header%LayerCount
      
      if(precisionType .eq. 1) then
          read(this%FileUnit, err=100) header%Method, timeStepLength,           &
            stressPeriodLength, totalTime
          header%TimeStepLength = dble(timeStepLength)
          header%StressPeriodLength = dble(stressPeriodLength)
          header%TotalTime = dble(totalTime)
      else
          read(this%FileUnit, err=100) header%Method, timeStepLengthDbl,        &
            stressPeriodLengthDbl, totalTimeDbl
          header%TimeStepLength = timeStepLengthDbl
          header%StressPeriodLength = stressPeriodLengthDbl
          header%TotalTime = totalTimeDbl
      end if
      
      if((header%Method .lt. 1) .or. (header%Method .gt. 6)) goto 100
      headerOffset = headerOffset + intBytes + (3 * realBytes)
      if(header%Method .eq. 2) then
          header%ListItemValueCount = 1
          read(this%FileUnit, err=100) header%ListItemCount
          headerOffset = headerOffset + intBytes
          if(header%ListItemCount .lt. 0) goto 100
      else if(header%Method .eq. 5) then
          read(this%FileUnit, err=100) header%ListItemValueCount
          headerOffset = headerOffset + intBytes
          
          if(header%ListItemValueCount .lt. 0) goto 100
          auxCount = header%ListItemValueCount - 1
          if(allocated(header%AuxiliaryNames)) deallocate(header%AuxiliaryNames)
          allocate(header%AuxiliaryNames(auxCount))
          if(auxCount .gt. 0) then
              do n = 1, auxCount
                  read(this%FileUnit,err=100) header%AuxiliaryNames(n)
                  headerOffset = headerOffset + 16
              end do
          end if
          
          read(this%FileUnit,err=100) header%ListItemCount
          headerOffset = headerOffset + intBytes
          
          if(header%ListItemCount .lt. 0) goto 100
      else if(header%Method .eq. 6) then
          read(this%FileUnit, err=100) header%TXT1ID1
          read(this%FileUnit, err=100) header%TXT1ID2
          read(this%FileUnit, err=100) header%TXT2ID1
          read(this%FileUnit, err=100) header%TXT2ID2
          headerOffset = headerOffset + 64
          
          read(this%FileUnit, err=100) header%ListItemValueCount
          headerOffset = headerOffset + intBytes
          
          if(header%ListItemValueCount .lt. 0) goto 100
          auxCount = header%ListItemValueCount - 1
          if(allocated(header%AuxiliaryNames)) deallocate(header%AuxiliaryNames)
          allocate(header%AuxiliaryNames(auxCount))
          if(auxCount .gt. 0) then
              do n = 1, auxCount
                  read(this%FileUnit,err=100) header%AuxiliaryNames(n)
                  headerOffset = headerOffset + 16
              end do
          end if
          
          read(this%FileUnit,err=100) header%ListItemCount
          headerOffset = headerOffset + intBytes
          
          if(header%ListItemCount .lt. 0) goto 100
          ! Add code
      end if
      
  else
      header%Method = 0
      header%TimeStepLength = 0.0d0
      header%StressPeriodLength = 0.0d0
      header%TotalTime = 0.0d0
  end if
  
  if((header%Method .eq. 2) .or. (header%Method .eq. 5) .or. (header%Method .eq. 6)) then
      header%ArrayItemCount = 0
  else if((header%Method .eq. 3) .or. (header%Method .eq. 4)) then
      header%ArrayItemCount = header%ColumnCount * header%RowCount
  else
      header%ArrayItemCount = header%ColumnCount * header%RowCount * header%LayerCount
  end if
  
  ! Set position properties
  header%HeaderPosition = position
  header%HeaderOffset = headerOffset
  
  ! Calculate and set the data offset size
  layerCountLong = header%LayerCount
  rowCountLong = header%RowCount
  columnCountLong = header%ColumnCount
  listItemValueCountLong = header%ListItemValueCount
  listItemCountLong = header%ListItemCount
  
  if((header%Method .eq. 0) .or. (header%Method .eq. 1)) then
      header%DataOffset = realBytes * (layerCountLong * rowCountLong * columnCountLong)
  else if((header%Method .eq. 2) .or. (header%Method .eq. 5)) then
      header%DataOffset = listItemCountLong * ((realBytes * listItemValueCountLong) + intBytes)
  else if(header%Method .eq. 6) then
      header%DataOffset = listItemCountLong * ((realBytes * listItemValueCountLong) + (2 * intBytes))
  else if(header%Method .eq. 3) then
      header%DataOffset = (realBytes + intBytes) * (rowCountLong * columnCountLong)
  else if(header%Method .eq. 4) then
      header%DataOffset = realBytes * rowCountLong * columnCountLong
  end if
  
  ! Header data is valid, so set the precision type property and return
  header%PrecisionType = precisionType
  return
  
  ! Handle error. Reinitialize header to signal the error, then return.
100 continue
  call header%Initialize()
  
  end subroutine pr_ReadRecordHeader

!---------------------------------------------------------
  subroutine pr_ProcessRecordHeaders(this, targetPrecisionType)
  implicit none
  class(BudgetReaderType) :: this
  integer,intent(in) :: targetPrecisionType
  integer :: n,recordCount,budgetType,precisionType,budgetFileFormat,eq0Count,  &
    gt0Count
  logical :: hasFlowJA,hasFlowFrontFace
  integer :: nlay,nrow,ncol,firstChar,lastChar,trimmedLength,                   &
    maxArrayBufferSize,maxListItemCount,bufferSize
  integer(kind=8) :: position,fileSize,nextPosition,temp
  character(len=16) :: text
  type(BudgetRecordHeaderType) :: header
  
  ! Deallocate RecordHeaders array
  if(allocated(this%RecordHeaders)) deallocate(this%RecordHeaders)
  
  ! Find file size
  inquire(unit=this%FileUnit,size=fileSize)
  
  ! Set initial values for the logical flags used to check budget type (structured or unstructured)
  hasFlowJA = .false.
  hasFlowFrontFace = .false.
  
  ! Read through header records and count them. Update the logical budget type flags for each record header.
  precisionType = targetPrecisionType
  budgetType = 0
  budgetFileFormat = 0
  recordCount = 0
  eq0Count = 0
  gt0Count = 0
  position = 1
  do while (position .lt. fileSize)
      call this%ReadRecordHeader(position, header, targetPrecisionType)
      if(header%PrecisionType .ne. targetPrecisionType) then
          precisionType = 0
          budgetType = 0
          budgetFileFormat = 0
          goto 100
      end if
      recordCount = recordCount + 1
      
      ! Keep track of the header method value in order to determine whether
      ! the file is a standard budget file, compact budget file.
      if(header%Method .eq. 0) then
          eq0Count = eq0Count + 1
      else if(header%Method .gt. 0) then
          gt0Count = gt0Count + 1
      end if
      
      if(recordCount .eq. 1) then
          maxArrayBufferSize = header%RowCount * header%ColumnCount * header%LayerCount
          maxListItemCount = header%ListItemCount
      end if
      
      call TrimAll(header%TextLabel,firstChar,lastChar,trimmedLength)
      if(header%TextLabel(firstChar:lastChar) .eq. 'FLOW JA FACE') then
           hasFlowJA = .true.
           this%FlowArraySize = header%ColumnCount
      end if
      if(header%TextLabel(firstChar:lastChar) .eq. 'FLOW-JA-FACE') then
           hasFlowJA = .true.
           this%FlowArraySize = header%ColumnCount
      end if
      if(header%TextLabel(firstChar:lastChar) .eq. 'FLOW FRONT FACE') then
           hasFlowFrontFace = .true.
           this%FlowArraySize = header%ColumnCount * header%RowCount * header%LayerCount
      end if
      
      !if((header%RowCount .ne. 1) .or. (header%LayerCount .ne. 1)) layerAndRowCountEqual1 = .false.
      
      bufferSize = header%RowCount * header%ColumnCount * header%LayerCount
      if(bufferSize .gt. maxArrayBufferSize) maxArrayBufferSize = bufferSize
      if(header%ListItemCount .gt. maxListItemCount) maxListItemCount = header%ListItemCount
      
      nextPosition = header%GetNextHeaderPosition()
      
      ! Check for error condition. If nextPosition is not larger than position then there is 
      ! a problem with the file. 
      if(nextPosition .le. position) then
          precisionType = 0
          goto 100
      end if
      
      ! Check for error condition. If nextPosition is larger than position by more than 1 then
      ! there is a problem with the file.
      if(nextPosition .gt. fileSize) then
          temp = nextPosition - fileSize
          if(temp .gt. 1) then
              precisionType = 0
              goto 100
          end if
      end if
      
      ! No error condition was detected. Set the position equal to the value of tempPosition.
      position = nextPosition
      
  end do
      
  ! Check the values of eq0Count and gt0Count to determine whether the file is a standard or compact
  ! budget file. For a standard budget file the method number for all record headers will equal 0. 
  ! For a compact budget file the method number for all record headers will be greater than 0. 
  ! 
  ! The budgetFileFormat flag is defined as:
  !    0 = undefined (this value means an error of somekind)
  !    1 = standard budget file
  !    2 = compact budget file
  if((eq0Count .eq. recordCount) .and. (gt0Count .eq. 0)) then
      budgetFileFormat = 1
  else if((eq0Count .eq. 0) .and. (gt0Count .eq. recordCount)) then
      budgetFileFormat = 2
  else
      ! Leave budgetFileFormat set to 0 to indicate the budget file does not conform to either a
      ! standard or compact structure.
  end if
  
  ! Check budget type flags to determine whether it is a structured or unstructured budget file
  if(hasFlowJA) then
      budgetType = 2
  else if(hasFlowFrontFace) then
      budgetType = 1
  end if
  
  ! If the budget type is still set to undefined, process an error condition and return
  if(budgetType .eq. 0) goto 100
  
  ! If the budget file format is set to undefined, process an error condition and return
  if(budgetFileFormat .eq. 0) goto 100
  
  ! Allocate header records array
  if(allocated(this%RecordHeaders)) deallocate(this%RecordHeaders)
  allocate(this%RecordHeaders(recordCount))
  
  ! Read through file again to initialize and save the header records in the header records array
  position = 1
  do n = 1, recordCount
      call this%ReadRecordHeader(position, this%RecordHeaders(n), precisionType)
      position = this%RecordHeaders(n)%GetNextHeaderPosition()
  end do
  
  ! Set maximum array and list buffer sizes
  this%MaximumArrayItemCount = maxArrayBufferSize
  this%MaximumListItemCount = maxListItemCount
  
100 continue
  ! Set BudgetType, PrecisionType, and FileFormat flags. If all three values are greater than 0 it indicate success.
  this%BudgetType = budgetType
  this%PrecisionType = precisionType
  this%FileFormat = budgetFileFormat
  
  ! Process errors
  if((budgetType .eq. 0) .or. (precisionType .eq. 0) .or. (budgetFileFormat .eq. 0)) then
      ! Reallocate the RecordHeaders array to size 0 if an error was encountered.
      if(allocated(this%RecordHeaders)) deallocate(this%RecordHeaders)
      allocate(this%RecordHeaders(0))
  end if
  
  return
  
  end subroutine pr_ProcessRecordHeaders
  
end module BudgetReaderModule