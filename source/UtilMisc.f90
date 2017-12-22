module UtilMiscModule
contains

SUBROUTINE UTRIMALL(LINE)
CHARACTER*(*) LINE
INTEGER IFIRST,ILAST,NFIRST,NLAST,N,M,LENGTH,OFFSET

NLAST=LEN_TRIM(LINE)
IF(NLAST.EQ.0) RETURN

DO N=1,NLAST
IF(LINE(N:N).NE.' ') THEN
  IFIRST = N
  LENGTH = NLAST - IFIRST + 1
  EXIT
END IF
END DO

OFFSET=IFIRST-1
IF(OFFSET.GT.0) THEN
DO N=1,LENGTH
  M = N+OFFSET
  LINE(N:N) = LINE(M:M)
  LINE(M:M) = ' '
END DO 
END IF

LINE = TRIM(LINE)

RETURN
END subroutine UTRIMALL

subroutine ustop(stopMessage, fileUnit)
!     ******************************************************************
!     STOP PROGRAM, WITH OPTION TO PRINT MESSAGE BEFORE STOPPING
!     ******************************************************************
!        SPECIFICATIONS:
!     ------------------------------------------------------------------
    character stopMessage*(*)
    integer :: fileUnit
!     ------------------------------------------------------------------

    if (stopMessage .ne. ' ') then
    write(*,'(a)') stopMessage
    write(fileUnit,'(a)') stopMessage
    end if
    stop

end subroutine ustop

subroutine ulog(logMessage, fileUnit)
!     ******************************************************************
!     WRITE MESSAGE TO LOG FILE
!     ******************************************************************
!        SPECIFICATIONS:
!     ------------------------------------------------------------------
    character logMessage*(*)
    integer :: fileUnit
!     ------------------------------------------------------------------

    if (logMessage .ne. ' ') then
    write(fileUnit,'(a)') logMessage
    end if
    return

end subroutine ulog

!------------------------------------------------------
  subroutine TrimAll(string,firstNonBlank,lastNonBlank,trimmedLength)
  implicit none
  character*(*) string
  character(len=:),allocatable :: newString
  integer,intent(inout) :: firstNonBlank,lastNonBlank,trimmedLength
  integer :: n
  
  firstNonBlank = 0
  trimmedLength = 0
  lastNonBlank = len_trim(string)
  if(lastNonBlank .gt. 0) then
      do n = 1, lastNonBlank
          if(string(n:n) .ne. ' ') then 
              firstNonBlank = n
              trimmedLength = lastNonBlank - firstNonBlank + 1
              return
          end if
      end do
  end if
  
  end subroutine TrimAll

!------------------------------------------------------
  function FirstNonBlank(string) result(firstChar)
  implicit none
  character*(*) string
  integer :: firstChar,length,n
  
  firstChar = 0
  length = len_trim(string)
  if(length .gt. 0) then
      do n = 1, length
          if(string(n:n) .ne. ' ') then
              firstChar = n
              return
          end if
      end do
  end if
  
  end function FirstNonBlank

!------------------------------------------------------
  subroutine GetLayerRowColumn(cellNumber, layer, row, column, nlay, nrow, ncol)
  implicit none
  integer,intent(in) :: cellNumber,nlay,nrow,ncol
  integer,intent(inout) :: layer,row,column
  integer :: nrnc,r
  
  nrnc = nrow*ncol
  row = nrow
  column = ncol
  layer = cellNumber / nrnc
  r = mod(cellNumber, nrnc)
  if(r .gt. 0) then
      layer = layer + 1
      row = r / ncol
      r = mod(r, ncol)
      if(r .gt. 0) then
          row = row + 1
          column = r
      end if
  end if
  
  end subroutine GetLayerRowColumn

!------------------------------------------------------
  subroutine ModExt(a, p, r, n)
  implicit none
  integer,intent(in) :: a, p
  integer,intent(inout) :: r, n
  
  r = 0
  n = 0
  if(a .eq. 0) return
  
  n = a / p
  r = mod(a, p)
  if(r .eq. 0) n = n + 1
  
  end subroutine ModExt

! --------------------------------------------------------------------
! INTEGER FUNCTION  FindMinimum():
!    This function returns the location of the minimum in the section
! between StartValue and EndValue.
! --------------------------------------------------------------------

   INTEGER FUNCTION  FindMinimum(x, StartValue, EndValue)
      IMPLICIT  NONE
      INTEGER, DIMENSION(1:), INTENT(IN) :: x
      INTEGER, INTENT(IN)                :: StartValue, EndValue
      INTEGER                            :: Minimum
      INTEGER                            :: Location
      INTEGER                            :: i

      Minimum  = x(StartValue)		! assume the first is the min
      Location = StartValue			! record its position
      DO i = StartValue+1, EndValue		! start with next elements
         IF (x(i) < Minimum) THEN	!   if x(i) less than the min?
            Minimum  = x(i)		!      Yes, a new minimum found
            Location = i                !      record its position
         END IF
      END DO
      FindMinimum = Location        	! return the position
   END FUNCTION  FindMinimum

! --------------------------------------------------------------------
! SUBROUTINE  Swap():
!    This subroutine swaps the values of its two formal arguments.
! --------------------------------------------------------------------

   SUBROUTINE  Swap(a, b)
      IMPLICIT  NONE
      INTEGER, INTENT(INOUT) :: a, b
      INTEGER                :: Temp

      Temp = a
      a    = b
      b    = Temp
   END SUBROUTINE  Swap

! --------------------------------------------------------------------
! SUBROUTINE  Sort():
!    This subroutine receives an array x() and sorts it into ascending
! order.
! --------------------------------------------------------------------

   SUBROUTINE  Sort(x, Size)
      IMPLICIT  NONE
      INTEGER, INTENT(IN)                   :: Size
      INTEGER, DIMENSION(1:), INTENT(INOUT) :: x
      INTEGER                               :: i
      INTEGER                               :: Location

      DO i = 1, Size-1			! except for the last
         Location = FindMinimum(x, i, Size)	! find min from this to last
         CALL  Swap(x(i), x(Location))	! swap this and the minimum
      END DO
   END SUBROUTINE  Sort

end module UtilMiscModule