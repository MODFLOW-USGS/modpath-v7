! -- MODFLOW-2015 utility routines.  Note that these are now written in f90
!    fortran style.
!
module UTL8MODULE
  use UTL7MODULE,only:ustop,ulaprw,ucolno,ulstlb
  contains
  
  subroutine freeunitnumber(iu)
! ******************************************************************************
! Assign a free unopened unit number to the iu dummy argument.
! ******************************************************************************
! 
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    implicit none
    integer :: lastunitnumber
    parameter(lastunitnumber=10000)
    integer,intent(inout) :: iu
    integer, save :: nextunitnumber=1000
    integer i
    logical :: opened
! ------------------------------------------------------------------------------
  !
    do i=nextunitnumber,lastunitnumber
      inquire(unit=i,opened=opened)
      if(.not. opened) exit
    enddo
    nextunitnumber=i
    iu=nextunitnumber
  end subroutine freeunitnumber
  
  subroutine u8rdcom(iin,iout,line,ierr)
! ******************************************************************************
! Read until non-comment line found and then return line
! ******************************************************************************
! 
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    implicit none
    !dummy arguments
    integer, intent(in) :: iin
    integer, intent(in) :: iout
    character (len=*), intent(inout) :: line
    integer, intent(inout) :: ierr
    !local definitions
    character (len=2), parameter :: comment = '//'
    logical :: iscomment
    integer :: i, l
! ------------------------------------------------------------------------------
    !code
    line = comment
    pcomments: do
      read (iin,'(a)',iostat=ierr) line
      if (ierr < 0) then
        line = ' '
        exit pcomments
      else if (ierr > 0) then
        call ustop('could not read from unit iin')
      end if
      if (len_trim(line).lt.1) then
        line = comment
        cycle
      end if
      line = trim(adjustl(line))
      iscomment = .false.
      select case (line(1:1))
        case ('#')
          iscomment = .true.
        case ('!')
          iscomment = .true.
        case default
          if (line(1:2).eq.comment) iscomment = .true.
      end select
      if (.not.iscomment) then
        exit pcomments
      else
        if (iout > 0) then
          !find the last non-blank character.
          l=len(line)
          do i = l, 1, -1
            if(line(i:i).ne.' ') then
              exit
            end if
          end do
          !print the line up to the last non-blank character.
          write(iout,'(1x,a)') line(1:i)            
        end if
      end if
    end do pcomments
    return
  end subroutine u8rdcom

  subroutine uget_block(iin,iout,ctag,ierr,isfound,lloc,line)
! ******************************************************************************
! Read until the ctag block is found.  Return isfound with true, if found.
! ******************************************************************************
! 
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    implicit none
    !dummy
    integer, intent(in) :: iin
    integer, intent(in) :: iout
    character (len=*), intent(in) :: ctag
    integer, intent(inout) :: ierr
    logical, intent(inout) :: isfound
    integer, intent(inout) :: lloc
    character (len=*), intent(inout) :: line
    !local
    integer :: istart
    integer :: istop
    integer :: ival
    double precision :: rval
! ------------------------------------------------------------------------------
    !code
    isfound = .false.
    do
      lloc = 1
      call u8rdcom(iin,iout,line,ierr)
      if (ierr.lt.0) exit
      call urword(line, lloc, istart, istop, 1, ival, rval, iin, iout)
      if (line(istart:istop).eq.'BEGIN') then
        call urword(line, lloc, istart, istop, 1, ival, rval, iin, iout)
        if (line(istart:istop).eq.ctag) then
          isfound = .true.
        else
          backspace(iin)
        end if
        exit
      end if
    end do
    return
  end subroutine uget_block

  subroutine uget_block_labeled(iin,iout,ctag,clabel,ierr,isfound,lloc,line)
! ******************************************************************************
! Read until the ctag block is found.  Return isfound with true, if found.
! ******************************************************************************
! 
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    implicit none
    !dummy
    integer, intent(in) :: iin
    integer, intent(in) :: iout
    character (len=*), intent(in) :: ctag,clabel
    integer, intent(inout) :: ierr
    logical, intent(inout) :: isfound
    integer, intent(inout) :: lloc
    character (len=*), intent(inout) :: line
    !local
    integer :: istart
    integer :: istop
    integer :: ival
    double precision :: rval
! ------------------------------------------------------------------------------
    !code
    isfound = .false.
    do
      lloc = 1
      call u8rdcom(iin,iout,line,ierr)
      if (ierr.lt.0) exit
      call urword(line, lloc, istart, istop, 1, ival, rval, iin, iout)
      if (line(istart:istop).eq.'BEGIN') then
        call urword(line, lloc, istart, istop, 1, ival, rval, iin, iout)
        if (line(istart:istop).eq.ctag) then
            if(len_trim(clabel) .gt. 0) then
                call URWORD(line, lloc, istart, istop, 1, ival, rval, iin, iout)
                if(line(istart:istop).eq.clabel) then
                    isfound = .true.
                end if
            else
                isfound = .true.
            end if
        else
          backspace(iin)
        end if
        exit
      end if
    end do
    return
  end subroutine uget_block_labeled
  
  subroutine uterminate_block(iin,iout,key,ctag,lloc,line,ierr)
! ******************************************************************************
! Possible abnormal block termination.  Terminate if 'begin' found or if 
! 'end' encountered with incorrect tag.
! ******************************************************************************
! 
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    implicit none
    !dummy
    integer, intent(in) :: iin
    integer, intent(in) :: iout
    character (len=*), intent(in) :: key
    character (len=*), intent(in) :: ctag
    integer, intent(inout) :: lloc
    character (len=*), intent(inout) :: line
    integer, intent(inout) :: ierr
    !local
    integer :: istart
    integer :: istop
    integer :: ival
    double precision :: rval
    !format
1   format(//,'ERROR. "',A,'" DETECTED WITHOUT "',A,'"',/,'"END',1X,A, &
      '" MUST BE USED TO END ',A,'.',/,'STOPPING...')      
2   format(//,'ERROR. "',A,'" DETECTED BEFORE "END',1X,A,'"',/,'"END',1X,A, &
        '" MUST BE USED TO END ',A,'.',/,'STOPPING...')      
! ------------------------------------------------------------------------------
    !code
    ierr = 1
    select case(key)
      case ('END')
        call urword(line, lloc, istart, istop, 1, ival, rval, iout, iin)
        if (line(istart:istop).ne.ctag) then
          write(iout,1) key, ctag, ctag, ctag
          call ustop('')
        else
          ierr = 0
        end if
      case ('BEGIN')
          write(iout,2) key, ctag, ctag, ctag
        call ustop('')
    end select
    return
  end subroutine uterminate_block
 
  subroutine ugetnode(nlay,nrow,ncol,klay,irow,jcol,node)
! ******************************************************************************
! Convert k,i,j to nodenumber
! ******************************************************************************
! 
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    implicit none
    !dummy
    integer, intent(in) :: nlay
    integer, intent(in) :: nrow
    integer, intent(in) :: ncol
    integer, intent(in) :: klay
    integer, intent(in) :: irow
    integer, intent(in) :: jcol
    integer, intent(inout) :: node
    !local
! ------------------------------------------------------------------------------
    !code
    node = (klay-1)*nrow*ncol + (irow-1)*ncol + jcol
    return
  end subroutine ugetnode
  
  subroutine u3dint(iin,iout,nlay,nrow,ncol,neq,ival,cval)
! ******************************************************************************
! Read three-dimensional integer array for a structured grid, consisting of 
! multiple 2d arrays with array headers.
! ******************************************************************************
! 
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    implicit none
    integer, intent(in) :: iin
    integer, intent(in) :: iout
    integer, intent(in) :: nlay
    integer, intent(in) :: nrow
    integer, intent(in) :: ncol
    integer, intent(in) :: neq
    integer, dimension(neq), intent(inout) :: ival
    character (len=24), intent(in) :: cval
    !local
    integer, dimension(:,:), allocatable :: itemp
    integer :: node
    integer :: i, j, k
    !functions
! ------------------------------------------------------------------------------
    !code
    allocate(itemp(ncol,nrow))
    do k = 1, nlay
      call u2dint(itemp,cval,nrow,ncol,k,iin,iout)
      do i = 1, nrow
        do j = 1, ncol
          call ugetnode(nlay,nrow,ncol,k,i,j,node)
          ival(node) = itemp(j,i)
        end do
      end do
    end do
    deallocate(itemp)
    return    
  end subroutine u3dint

  subroutine u3ddbl(iin,iout,nlay,nrow,ncol,neq,rval,cval)
! ******************************************************************************
! Read three-dimensional integer array, consisting of multiple 2d arrays with
! array headers.
! ******************************************************************************
! 
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    implicit none
    integer, intent(in) :: iin
    integer, intent(in) :: iout
    integer, intent(in) :: nlay
    integer, intent(in) :: nrow
    integer, intent(in) :: ncol
    integer, intent(in) :: neq
    double precision, dimension(neq), intent(inout) :: rval
    character (len=24), intent(in) :: cval
    !local
    double precision, dimension(:,:), allocatable :: rtemp
    integer :: node
    integer :: i, j, k
    !functions
! ------------------------------------------------------------------------------
    !code
    allocate(rtemp(ncol,nrow))
    do k = 1, nlay
      call u2ddbl(rtemp,cval,nrow,ncol,k,iin,iout)
      do i = 1, nrow
        do j = 1, ncol
          call ugetnode(nlay,nrow,ncol,k,i,j,node)
          rval(node) = rtemp(j,i)
        end do
      end do
    end do
    deallocate(rtemp)
    return    
  end subroutine u3ddbl
  
      SUBROUTINE URWORD(LINE,ICOL,ISTART,ISTOP,NCODE,N,R,IOUT,IN)
!C     ******************************************************************
!C     ROUTINE TO EXTRACT A WORD FROM A LINE OF TEXT, AND OPTIONALLY
!C     CONVERT THE WORD TO A NUMBER.
!C        ISTART AND ISTOP WILL BE RETURNED WITH THE STARTING AND
!C          ENDING CHARACTER POSITIONS OF THE WORD.
!C        THE LAST CHARACTER IN THE LINE IS SET TO BLANK SO THAT IF ANY
!C          PROBLEMS OCCUR WITH FINDING A WORD, ISTART AND ISTOP WILL
!C          POINT TO THIS BLANK CHARACTER.  THUS, A WORD WILL ALWAYS BE
!C          RETURNED UNLESS THERE IS A NUMERIC CONVERSION ERROR.  BE SURE
!C          THAT THE LAST CHARACTER IN LINE IS NOT AN IMPORTANT CHARACTER
!C          BECAUSE IT WILL ALWAYS BE SET TO BLANK.
!C        A WORD STARTS WITH THE FIRST CHARACTER THAT IS NOT A SPACE OR
!C          COMMA, AND ENDS WHEN A SUBSEQUENT CHARACTER THAT IS A SPACE
!C          OR COMMA.  NOTE THAT THESE PARSING RULES DO NOT TREAT TWO
!C          COMMAS SEPARATED BY ONE OR MORE SPACES AS A NULL WORD.
!C        FOR A WORD THAT BEGINS WITH "'", THE WORD STARTS WITH THE
!C          CHARACTER AFTER THE QUOTE AND ENDS WITH THE CHARACTER
!C          PRECEDING A SUBSEQUENT QUOTE.  THUS, A QUOTED WORD CAN
!C          INCLUDE SPACES AND COMMAS.  THE QUOTED WORD CANNOT CONTAIN
!C          A QUOTE CHARACTER.
!C        IF NCODE IS 1, THE WORD IS CONVERTED TO UPPER CASE.
!C        IF NCODE IS 2, THE WORD IS CONVERTED TO AN INTEGER.
!C        IF NCODE IS 3, THE WORD IS CONVERTED TO A REAL NUMBER.
!C        NUMBER CONVERSION ERROR IS WRITTEN TO UNIT IOUT IF IOUT IS
!C          POSITIVE; ERROR IS WRITTEN TO DEFAULT OUTPUT IF IOUT IS 0;
!C          NO ERROR MESSAGE IS WRITTEN IF IOUT IS NEGATIVE.
!C     ******************************************************************
!C
!C        SPECIFICATIONS:
!C     ------------------------------------------------------------------
      double precision,intent(inout) :: r
      CHARACTER*(*) LINE
      CHARACTER*20 STRING
      CHARACTER*30 RW
      CHARACTER*1 TAB
!C     ------------------------------------------------------------------
      TAB=CHAR(9)
!C
!C1------Set last char in LINE to blank and set ISTART and ISTOP to point
!C1------to this blank as a default situation when no word is found.  If
!C1------starting location in LINE is out of bounds, do not look for a
!C1------word.
      LINLEN=LEN(LINE)
      LINE(LINLEN:LINLEN)=' '
      ISTART=LINLEN
      ISTOP=LINLEN
      LINLEN=LINLEN-1
      IF(ICOL.LT.1 .OR. ICOL.GT.LINLEN) GO TO 100
!C
!C2------Find start of word, which is indicated by first character that
!C2------is not a blank, a comma, or a tab.
      DO 10 I=ICOL,LINLEN
      IF(LINE(I:I).NE.' ' .AND. LINE(I:I).NE.',' &
     &    .AND. LINE(I:I).NE.TAB) GO TO 20
10    CONTINUE
      ICOL=LINLEN+1
      GO TO 100
!C
!C3------Found start of word.  Look for end.
!C3A-----When word is quoted, only a quote can terminate it.
20    IF(LINE(I:I).EQ.'''') THEN
         I=I+1
         IF(I.LE.LINLEN) THEN
            DO 25 J=I,LINLEN
            IF(LINE(J:J).EQ.'''') GO TO 40
25          CONTINUE
         END IF
!C
!C3B-----When word is not quoted, space, comma, or tab will terminate.
      ELSE
         DO 30 J=I,LINLEN
         IF(LINE(J:J).EQ.' ' .OR. LINE(J:J).EQ.',' &
     &    .OR. LINE(J:J).EQ.TAB) GO TO 40
30       CONTINUE
      END IF
!C
!C3C-----End of line without finding end of word; set end of word to
!C3C-----end of line.
      J=LINLEN+1
!C
!C4------Found end of word; set J to point to last character in WORD and
!C-------set ICOL to point to location for scanning for another word.
40    ICOL=J+1
      J=J-1
      IF(J.LT.I) GO TO 100
      ISTART=I
      ISTOP=J
!C
!C5------Convert word to upper case and RETURN if NCODE is 1.
      IF(NCODE.EQ.1) THEN
         IDIFF=ICHAR('a')-ICHAR('A')
         DO 50 K=ISTART,ISTOP
            IF(LINE(K:K).GE.'a' .AND. LINE(K:K).LE.'z') &
     &             LINE(K:K)=CHAR(ICHAR(LINE(K:K))-IDIFF)
50       CONTINUE
         RETURN
      END IF
!C
!C6------Convert word to a number if requested.
100   IF(NCODE.EQ.2 .OR. NCODE.EQ.3) THEN
         RW=' '
         L=30-ISTOP+ISTART
         IF(L.LT.1) GO TO 200
         RW(L:30)=LINE(ISTART:ISTOP)
         IF(NCODE.EQ.2) READ(RW,'(I30)',ERR=200) N
         IF(NCODE.EQ.3) READ(RW,'(F30.0)',ERR=200) R
      END IF
      RETURN
!C
!C7------Number conversion error.
200   IF(NCODE.EQ.3) THEN
         STRING= 'A REAL NUMBER'
         L=13
      ELSE
         STRING= 'AN INTEGER'
         L=10
      END IF
!C
!C7A-----If output unit is negative, set last character of string to 'E'.
      IF(IOUT.LT.0) THEN
         N=0
         R=0.
         LINE(LINLEN+1:LINLEN+1)='E'
         RETURN
!C
!C7B-----If output unit is positive; write a message to output unit.
      ELSE IF(IOUT.GT.0) THEN
         IF(IN.GT.0) THEN
            WRITE(IOUT,201) IN,LINE(ISTART:ISTOP),STRING(1:L),LINE
         ELSE
            WRITE(IOUT,202) LINE(ISTART:ISTOP),STRING(1:L),LINE
         END IF
201      FORMAT(1X,/1X,'FILE UNIT ',I4,' : ERROR CONVERTING "',A, &
     &       '" TO ',A,' IN LINE:',/1X,A)
202      FORMAT(1X,/1X,'KEYBOARD INPUT : ERROR CONVERTING "',A, &
     &       '" TO ',A,' IN LINE:',/1X,A)
!C
!C7C-----If output unit is 0; write a message to default output.
      ELSE
         IF(IN.GT.0) THEN
            WRITE(*,201) IN,LINE(ISTART:ISTOP),STRING(1:L),LINE
         ELSE
            WRITE(*,202) LINE(ISTART:ISTOP),STRING(1:L),LINE
         END IF
      END IF
!C
!C7D-----STOP after writing message.
      CALL USTOP(' ')
      END SUBROUTINE
      SUBROUTINE U1DREL(A,ANAME,JJ,IN,IOUT)
!C     ******************************************************************
!C     ROUTINE TO INPUT 1-D REAL DATA MATRICES
!C       A IS ARRAY TO INPUT
!C       ANAME IS 24 CHARACTER DESCRIPTION OF A
!C       JJ IS NO. OF ELEMENTS
!C       IN IS INPUT UNIT
!C       IOUT IS OUTPUT UNIT
!C     ******************************************************************
!C
!C        SPECIFICATIONS:
!C     ------------------------------------------------------------------
      USE OpenSpecModule
      double precision :: r,cnstnt
      CHARACTER*24 ANAME
      DIMENSION A(JJ)
      CHARACTER*20 FMTIN
      CHARACTER*200 CNTRL
      CHARACTER*200 FNAME
      DATA NUNOPN/99/
!      INCLUDE 'openspec.inc'
!C     ------------------------------------------------------------------
!C
!C1------READ ARRAY CONTROL RECORD AS CHARACTER DATA.
      READ(IN,'(A)') CNTRL
!C
!C2------LOOK FOR ALPHABETIC WORD THAT INDICATES THAT THE RECORD IS FREE
!C2------FORMAT.  SET A FLAG SPECIFYING IF FREE FORMAT OR FIXED FORMAT.
      ICLOSE=0
      IFREE=1
      ICOL=1
      CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,1,N,R,IOUT,IN)
      IF (CNTRL(ISTART:ISTOP).EQ.'CONSTANT') THEN
         LOCAT=0
      ELSE IF(CNTRL(ISTART:ISTOP).EQ.'INTERNAL') THEN
         LOCAT=IN
      ELSE IF(CNTRL(ISTART:ISTOP).EQ.'EXTERNAL') THEN
        CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,0,N,R,IOUT,IN)
        inquire(file=cntrl(istart:istop),number=locat)
        if(locat<0) then
          write(iout,*)'Error.  External file not opened: ', &
                       cntrl(ISTART:ISTOP)
          write(iout,*)'Specify file using DATA or DATA(BINARY)'
          write(iout,*)'in the name file.'
          write(iout,*)'STOPPING...'
          call ustop(' ')
        endif
      ELSE IF(CNTRL(ISTART:ISTOP).EQ.'OPEN/CLOSE') THEN
         CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,0,N,R,IOUT,IN)
         FNAME=CNTRL(ISTART:ISTOP)
         LOCAT=NUNOPN
         WRITE(IOUT,15) LOCAT,FNAME
   15    FORMAT(1X,/1X,'OPENING FILE ON UNIT ',I4,':',/1X,A)
         OPEN(UNIT=LOCAT,FILE=FNAME,ACTION=ACTION(1))
         ICLOSE=1
      ELSE
!C
!C2A-----DID NOT FIND A RECOGNIZED WORD, SO NOT USING FREE FORMAT.
!C2A-----READ THE CONTROL RECORD THE ORIGINAL WAY.
         IFREE=0
         READ(CNTRL,1,ERR=500) LOCAT,CNSTNT,FMTIN,IPRN
    1    FORMAT(I10,F10.0,A20,I10)
      END IF
!C
!C3------FOR FREE FORMAT CONTROL RECORD, READ REMAINING FIELDS.
      IF(IFREE.NE.0) THEN
         CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,3,N,CNSTNT,IOUT,IN)
         IF(LOCAT.GT.0) THEN
            CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,1,N,R,IOUT,IN)
            FMTIN=CNTRL(ISTART:ISTOP)
            CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,2,IPRN,R,IOUT,IN)
         END IF
      END IF
!C
!C4------TEST LOCAT TO SEE HOW TO DEFINE ARRAY VALUES.
      IF(LOCAT.GT.0) GO TO 90
!C
!C4A-----LOCAT <0 OR =0; SET ALL ARRAY VALUES EQUAL TO CNSTNT. RETURN.
      DO 80 J=1,JJ
   80 A(J)=CNSTNT
      WRITE(IOUT,3) ANAME,CNSTNT
    3 FORMAT(1X,/1X,A,' =',1P,G14.6)
      RETURN
!C
!C4B-----LOCAT>0; READ FORMATTED RECORDS USING FORMAT FMTIN.
   90 CONTINUE
      WRITE(IOUT,5) ANAME,LOCAT,FMTIN
    5 FORMAT(1X,///11X,A,/ &
     &       1X,'READING ON UNIT ',I4,' WITH FORMAT: ',A20)
      IF(FMTIN.EQ.'(FREE)') THEN
      READ(LOCAT,*) (A(J),J=1,JJ)
      ELSE
         READ(LOCAT,FMTIN) (A(J),J=1,JJ)
      END IF
      IF(ICLOSE.NE.0) CLOSE(UNIT=LOCAT)
!C
!C5------IF CNSTNT NOT ZERO THEN MULTIPLY ARRAY VALUES BY CNSTNT.
      ZERO=0.
      IF(CNSTNT.EQ.ZERO) GO TO 120
      DO 100 J=1,JJ
  100 A(J)=A(J)*CNSTNT
!C
!C6------IF PRINT CODE (IPRN) =0 OR >0 THEN PRINT ARRAY VALUES.
120   CONTINUE
      IF(IPRN.EQ.0) THEN
         WRITE(IOUT,1001) (A(J),J=1,JJ)
1001     FORMAT((1X,1PG12.5,9(1X,G12.5)))
      ELSE IF(IPRN.GT.0) THEN
         WRITE(IOUT,1002) (A(J),J=1,JJ)
1002     FORMAT((1X,1PG12.5,4(1X,G12.5)))
      END IF
!C
!C7------RETURN
      RETURN
!C
!C8------CONTROL RECORD ERROR.
500   WRITE(IOUT,502) ANAME
502   FORMAT(1X,/1X,'ERROR READING ARRAY CONTROL RECORD FOR ',A,':')
      WRITE(IOUT,'(1X,A)') CNTRL
      CALL USTOP(' ')
      END SUBROUTINE
      SUBROUTINE U1DDBL(A,ANAME,JJ,IN,IOUT)
!C     ******************************************************************
!C     ROUTINE TO INPUT 1-D REAL DATA MATRICES
!C       A IS ARRAY TO INPUT
!C       ANAME IS 24 CHARACTER DESCRIPTION OF A
!C       JJ IS NO. OF ELEMENTS
!C       IN IS INPUT UNIT
!C       IOUT IS OUTPUT UNIT
!C     ******************************************************************
!C
!C        SPECIFICATIONS:
!C     ------------------------------------------------------------------
      USE OpenSpecModule
      CHARACTER*24 ANAME
      DOUBLE PRECISION,DIMENSION(jj),intent(inout) :: A
      double precision :: r,cnstnt
      CHARACTER*20 FMTIN
      CHARACTER*200 CNTRL
      CHARACTER*200 FNAME
      DATA NUNOPN/99/
!      INCLUDE 'openspec.inc'
!C     ------------------------------------------------------------------
!C
!C1------READ ARRAY CONTROL RECORD AS CHARACTER DATA.
      READ(IN,'(A)') CNTRL
!C
!C2------LOOK FOR ALPHABETIC WORD THAT INDICATES THAT THE RECORD IS FREE
!C2------FORMAT.  SET A FLAG SPECIFYING IF FREE FORMAT OR FIXED FORMAT.
      ICLOSE=0
      IFREE=1
      ICOL=1
      CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,1,N,R,IOUT,IN)
      IF (CNTRL(ISTART:ISTOP).EQ.'CONSTANT') THEN
         LOCAT=0
      ELSE IF(CNTRL(ISTART:ISTOP).EQ.'INTERNAL') THEN
         LOCAT=IN
      ELSE IF(CNTRL(ISTART:ISTOP).EQ.'EXTERNAL') THEN
        CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,0,N,R,IOUT,IN)
        inquire(file=cntrl(istart:istop),number=locat)
        if(locat<0) then
          write(iout,*)'Error.  External file not opened: ', &
                       cntrl(ISTART:ISTOP)
          write(iout,*)'Specify file using DATA or DATA(BINARY)'
          write(iout,*)'in the name file.'
          write(iout,*)'STOPPING...'
          call ustop(' ')
        endif
      ELSE IF(CNTRL(ISTART:ISTOP).EQ.'OPEN/CLOSE') THEN
         CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,0,N,R,IOUT,IN)
         FNAME=CNTRL(ISTART:ISTOP)
         LOCAT=NUNOPN
         WRITE(IOUT,15) LOCAT,FNAME
   15    FORMAT(1X,/1X,'OPENING FILE ON UNIT ',I4,':',/1X,A)
         OPEN(UNIT=LOCAT,FILE=FNAME,ACTION=ACTION(1))
         ICLOSE=1
      ELSE
!C
!C2A-----DID NOT FIND A RECOGNIZED WORD, SO NOT USING FREE FORMAT.
!C2A-----READ THE CONTROL RECORD THE ORIGINAL WAY.
         IFREE=0
         READ(CNTRL,1,ERR=500) LOCAT,CNSTNT,FMTIN,IPRN
    1    FORMAT(I10,F10.0,A20,I10)
      END IF
!C
!C3------FOR FREE FORMAT CONTROL RECORD, READ REMAINING FIELDS.
      IF(IFREE.NE.0) THEN
         CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,3,N,CNSTNT,IOUT,IN)
         IF(LOCAT.GT.0) THEN
            CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,1,N,R,IOUT,IN)
            FMTIN=CNTRL(ISTART:ISTOP)
            CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,2,IPRN,R,IOUT,IN)
         END IF
      END IF
!C
!C4------TEST LOCAT TO SEE HOW TO DEFINE ARRAY VALUES.
      IF(LOCAT.GT.0) GO TO 90
!C
!C4A-----LOCAT <0 OR =0; SET ALL ARRAY VALUES EQUAL TO CNSTNT. RETURN.
      DO 80 J=1,JJ
   80 A(J)=CNSTNT
      WRITE(IOUT,3) ANAME,CNSTNT
    3 FORMAT(1X,/1X,A,' =',1P,G14.6)
      RETURN
!C
!C4B-----LOCAT>0; READ FORMATTED RECORDS USING FORMAT FMTIN.
   90 CONTINUE
      WRITE(IOUT,5) ANAME,LOCAT,FMTIN
    5 FORMAT(1X,///11X,A,/ &
     &       1X,'READING ON UNIT ',I4,' WITH FORMAT: ',A20)
      IF(FMTIN.EQ.'(FREE)') THEN
      READ(LOCAT,*) (A(J),J=1,JJ)
      ELSE
         READ(LOCAT,FMTIN) (A(J),J=1,JJ)
      END IF
      IF(ICLOSE.NE.0) CLOSE(UNIT=LOCAT)
!C
!C5------IF CNSTNT NOT ZERO THEN MULTIPLY ARRAY VALUES BY CNSTNT.
      ZERO=0.
      IF(CNSTNT.EQ.ZERO) GO TO 120
      DO 100 J=1,JJ
  100 A(J)=A(J)*CNSTNT
!C
!C6------IF PRINT CODE (IPRN) =0 OR >0 THEN PRINT ARRAY VALUES.
120   CONTINUE
      IF(IPRN.EQ.0) THEN
         WRITE(IOUT,1001) (A(J),J=1,JJ)
1001     FORMAT((1X,1PG12.5,9(1X,G12.5)))
      ELSE IF(IPRN.GT.0) THEN
         WRITE(IOUT,1002) (A(J),J=1,JJ)
1002     FORMAT((1X,1PG12.5,4(1X,G12.5)))
      END IF
!C
!C7------RETURN
      RETURN
!C
!C8------CONTROL RECORD ERROR.
500   WRITE(IOUT,502) ANAME
502   FORMAT(1X,/1X,'ERROR READING ARRAY CONTROL RECORD FOR ',A,':')
      WRITE(IOUT,'(1X,A)') CNTRL
      CALL USTOP(' ')
      END SUBROUTINE
      SUBROUTINE U1DINT(IA,ANAME,JJ,IN,IOUT)
!C     ******************************************************************
!C     ROUTINE TO INPUT 1-D REAL DATA MATRICES
!C       A IS ARRAY TO INPUT
!C       ANAME IS 24 CHARACTER DESCRIPTION OF A
!C       JJ IS NO. OF ELEMENTS
!C       IN IS INPUT UNIT
!C       IOUT IS OUTPUT UNIT
!C     ******************************************************************
!C
!C        SPECIFICATIONS:
!C     ------------------------------------------------------------------
      USE OpenSpecModule
      double precision :: r,cnstnt
      CHARACTER*24 ANAME
      DIMENSION IA(JJ)
      CHARACTER*20 FMTIN
      CHARACTER*200 CNTRL
      CHARACTER*200 FNAME
      DATA NUNOPN/99/
      !INCLUDE 'openspec.inc'
!C     ------------------------------------------------------------------
!C
!C1------READ ARRAY CONTROL RECORD AS CHARACTER DATA.
      READ(IN,'(A)') CNTRL
!C
!C2------LOOK FOR ALPHABETIC WORD THAT INDICATES THAT THE RECORD IS FREE
!C2------FORMAT.  SET A FLAG SPECIFYING IF FREE FORMAT OR FIXED FORMAT.
      ICLOSE=0
      IFREE=1
      ICOL=1
      CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,1,N,R,IOUT,IN)
      IF (CNTRL(ISTART:ISTOP).EQ.'CONSTANT') THEN
         LOCAT=0
      ELSE IF(CNTRL(ISTART:ISTOP).EQ.'INTERNAL') THEN
         LOCAT=IN
      ELSE IF(CNTRL(ISTART:ISTOP).EQ.'EXTERNAL') THEN
        CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,0,N,R,IOUT,IN)
        inquire(file=cntrl(istart:istop),number=locat)
        if(locat<0) then
          write(iout,*)'Error.  External file not opened: ', &
                       cntrl(ISTART:ISTOP)
          write(iout,*)'Specify file using DATA or DATA(BINARY)'
          write(iout,*)'in the name file.'
          write(iout,*)'STOPPING...'
          call ustop(' ')
        endif
      ELSE IF(CNTRL(ISTART:ISTOP).EQ.'OPEN/CLOSE') THEN
         CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,0,N,R,IOUT,IN)
         FNAME=CNTRL(ISTART:ISTOP)
         LOCAT=NUNOPN
         WRITE(IOUT,15) LOCAT,FNAME
   15    FORMAT(1X,/1X,'OPENING FILE ON UNIT ',I4,':',/1X,A)
         OPEN(UNIT=LOCAT,FILE=FNAME,ACTION=ACTION(1))
         ICLOSE=1
      ELSE
!C
!C2A-----DID NOT FIND A RECOGNIZED WORD, SO NOT USING FREE FORMAT.
!C2A-----READ THE CONTROL RECORD THE ORIGINAL WAY.
         IFREE=0
         READ(CNTRL,1,ERR=500) LOCAT,ICNSTNT,FMTIN,IPRN
    1    FORMAT(I10,I10.0,A20,I10)
      END IF
!C
!C3------FOR FREE FORMAT CONTROL RECORD, READ REMAINING FIELDS.
      IF(IFREE.NE.0) THEN
         CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,2,ICNSTNT,R,IOUT,IN)
         IF(LOCAT.GT.0) THEN
            CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,1,N,R,IOUT,IN)
            FMTIN=CNTRL(ISTART:ISTOP)
            CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,2,IPRN,R,IOUT,IN)
         END IF
      END IF
!C
!C4------TEST LOCAT TO SEE HOW TO DEFINE ARRAY VALUES.
      IF(LOCAT.GT.0) GO TO 90
!C
!C4A-----LOCAT <0 OR =0; SET ALL ARRAY VALUES EQUAL TO ICNSTNT. RETURN.
      DO 80 J=1,JJ
   80 IA(J)=ICNSTNT
      WRITE(IOUT,3) ANAME,ICNSTNT
    3 FORMAT(1X,/1X,A,' =',1P,I10)
      RETURN
!C
!C4B-----LOCAT>0; READ FORMATTED RECORDS USING FORMAT FMTIN.
   90 CONTINUE
      WRITE(IOUT,5) ANAME,LOCAT,FMTIN
    5 FORMAT(1X,///11X,A,/ &
     &       1X,'READING ON UNIT ',I4,' WITH FORMAT: ',A20)
      IF(FMTIN.EQ.'(FREE)') THEN
      READ(LOCAT,*) (IA(J),J=1,JJ)
      ELSE
         READ(LOCAT,FMTIN) (IA(J),J=1,JJ)
      END IF
      IF(ICLOSE.NE.0) CLOSE(UNIT=LOCAT)
!C
!C5------IF ICNSTNT NOT ZERO THEN MULTIPLY ARRAY VALUES BY ICNSTNT.
      ZERO=0.
      IF(ICNSTNT.EQ.ZERO) GO TO 120
      DO 100 J=1,JJ
  100 IA(J)=IA(J)*ICNSTNT
!C
!C6------IF PRINT CODE (IPRN) =0 OR >0 THEN PRINT ARRAY VALUES.
120   CONTINUE
      IF(IPRN.EQ.0) THEN
         WRITE(IOUT,1001) (IA(J),J=1,JJ)
1001     FORMAT(20(1X,I9))
      ELSE IF(IPRN.GT.0) THEN
         WRITE(IOUT,1002) (IA(J),J=1,JJ)
1002     FORMAT(8(1X,I9))
      END IF
!C
!C7------RETURN
      RETURN
!C
!C8------CONTROL RECORD ERROR.
500   WRITE(IOUT,502) ANAME
502   FORMAT(1X,/1X,'ERROR READING ARRAY CONTROL RECORD FOR ',A,':')
      WRITE(IOUT,'(1X,A)') CNTRL
      CALL USTOP(' ')
      END SUBROUTINE 
      SUBROUTINE U2DREL(A,ANAME,II,JJ,K,IN,IOUT)
!C     ******************************************************************
!C     ROUTINE TO INPUT 2-D REAL DATA MATRICES
!C       A IS ARRAY TO INPUT
!C       ANAME IS 24 CHARACTER DESCRIPTION OF A
!C       II IS NO. OF ROWS
!C       JJ IS NO. OF COLS
!C       K IS LAYER NO. (USED WITH NAME TO TITLE PRINTOUT --)
!C              IF K=0, NO LAYER IS PRINTED
!C              IF K<0, CROSS SECTION IS PRINTED)
!C       IN IS INPUT UNIT
!C       IOUT IS OUTPUT UNIT
!C     ******************************************************************
!C
!C        SPECIFICATIONS:
!C     ------------------------------------------------------------------
      USE OpenSpecModule
      double precision :: r,cnstnt
      CHARACTER*24 ANAME
      DOUBLEPRECISION A(JJ,II)
      CHARACTER*20 FMTIN
      CHARACTER*200 CNTRL
      CHARACTER*16 TEXT
      CHARACTER*200 FNAME
      DATA NUNOPN/99/
!      INCLUDE 'openspec.inc'
!C     ------------------------------------------------------------------
!C
!C1------READ ARRAY CONTROL RECORD AS CHARACTER DATA.
      READ(IN,'(A)') CNTRL
!C
!C2------LOOK FOR ALPHABETIC WORD THAT INDICATES THAT THE RECORD IS FREE
!C2------FORMAT.  SET A FLAG SPECIFYING IF FREE FORMAT OR FIXED FORMAT.
      ICLOSE=0
      IFREE=1
      ICOL=1
      CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,1,N,R,IOUT,IN)
      IF (CNTRL(ISTART:ISTOP).EQ.'CONSTANT') THEN
         LOCAT=0
      ELSE IF(CNTRL(ISTART:ISTOP).EQ.'INTERNAL') THEN
         LOCAT=IN
      ELSE IF(CNTRL(ISTART:ISTOP).EQ.'EXTERNAL') THEN
!mf2015         CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,2,LOCAT,R,IOUT,IN)
        CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,0,N,R,IOUT,IN)
        inquire(file=cntrl(istart:istop),number=locat)
        if(locat<0) then
          write(iout,*)'Error.  External file not opened: ', &
                       cntrl(ISTART:ISTOP)
          write(iout,*)'Specify file using DATA or DATA(BINARY)'
          write(iout,*)'in the name file.'
          write(iout,*)'STOPPING...'
          call ustop(' ')
        endif
      ELSE IF(CNTRL(ISTART:ISTOP).EQ.'OPEN/CLOSE') THEN
         CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,0,N,R,IOUT,IN)
         FNAME=CNTRL(ISTART:ISTOP)
         LOCAT=NUNOPN
         WRITE(IOUT,15) LOCAT,FNAME
   15    FORMAT(1X,/1X,'OPENING FILE ON UNIT ',I4,':',/1X,A)
         ICLOSE=1
      ELSE
!C
!C2A-----DID NOT FIND A RECOGNIZED WORD, SO NOT USING FREE FORMAT.
!C2A-----READ THE CONTROL RECORD THE ORIGINAL WAY.
         IFREE=0
         READ(CNTRL,1,ERR=500) LOCAT,CNSTNT,FMTIN,IPRN
    1    FORMAT(I10,F10.0,A20,I10)
      END IF
!C
!C3------FOR FREE FORMAT CONTROL RECORD, READ REMAINING FIELDS.
      IF(IFREE.NE.0) THEN
         CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,3,N,CNSTNT,IOUT,IN)
         IF(LOCAT.NE.0) THEN
            CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,1,N,R,IOUT,IN)
            FMTIN=CNTRL(ISTART:ISTOP)
            IF(ICLOSE.NE.0) THEN
               IF(FMTIN.EQ.'(BINARY)') THEN
                  OPEN(UNIT=LOCAT,FILE=FNAME,FORM=FORM,ACCESS=ACCESS, &
     &                 ACTION=ACTION(1))
               ELSE
                  OPEN(UNIT=LOCAT,FILE=FNAME,ACTION=ACTION(1))
               END IF
            END IF
            IF(LOCAT.GT.0 .AND. FMTIN.EQ.'(BINARY)') LOCAT=-LOCAT
            CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,2,IPRN,R,IOUT,IN)
         END IF
      END IF
!C
!C4------TEST LOCAT TO SEE HOW TO DEFINE ARRAY VALUES.
      IF(LOCAT.EQ.0) THEN
!C
!C4A-----LOCAT=0; SET ALL ARRAY VALUES EQUAL TO CNSTNT. RETURN.
        DO 80 I=1,II
        DO 80 J=1,JJ
   80   A(J,I)=CNSTNT
        IF(K.GT.0) WRITE(IOUT,2) ANAME,CNSTNT,K
    2   FORMAT(1X,/1X,A,' =',1P,G14.6,' FOR LAYER',I4)
        IF(K.LE.0) WRITE(IOUT,3) ANAME,CNSTNT
    3   FORMAT(1X,/1X,A,' =',1P,G14.6)
        RETURN
      ELSE IF(LOCAT.GT.0) THEN
!C
!C4B-----LOCAT>0; READ FORMATTED RECORDS USING FORMAT FMTIN.
        IF(K.GT.0) THEN
           WRITE(IOUT,94) ANAME,K,LOCAT,FMTIN
   94      FORMAT(1X,///11X,A,' FOR LAYER',I4,/ &
     &      1X,'READING ON UNIT ',I4,' WITH FORMAT: ',A)
        ELSE IF(K.EQ.0) THEN
           WRITE(IOUT,95) ANAME,LOCAT,FMTIN
   95      FORMAT(1X,///11X,A,/ &
     &      1X,'READING ON UNIT ',I4,' WITH FORMAT: ',A)
        ELSE
           WRITE(IOUT,96) ANAME,LOCAT,FMTIN
   96      FORMAT(1X,///11X,A,' FOR CROSS SECTION',/ &
     &      1X,'READING ON UNIT ',I4,' WITH FORMAT: ',A)
        END IF
        DO 100 I=1,II
        IF(FMTIN.EQ.'(FREE)') THEN
           READ(LOCAT,*) (A(J,I),J=1,JJ)
        ELSE
           READ(LOCAT,FMTIN) (A(J,I),J=1,JJ)
        END IF
  100   CONTINUE
      ELSE
!C
!C4C-----LOCAT<0; READ UNFORMATTED ARRAY VALUES.
        LOCAT=-LOCAT
        IF(K.GT.0) THEN
           WRITE(IOUT,201) ANAME,K,LOCAT
  201      FORMAT(1X,///11X,A,' FOR LAYER',I4,/ &
     &      1X,'READING BINARY ON UNIT ',I4)
        ELSE IF(K.EQ.0) THEN
           WRITE(IOUT,202) ANAME,LOCAT
  202      FORMAT(1X,///1X,A,/ &
     &      1X,'READING BINARY ON UNIT ',I4)
        ELSE
           WRITE(IOUT,203) ANAME,LOCAT
  203      FORMAT(1X,///1X,A,' FOR CROSS SECTION',/ &
     &      1X,'READING BINARY ON UNIT ',I4)
        END IF
        READ(LOCAT) KSTP,KPER,PERTIM,TOTIM,TEXT,NCOL,NROW,ILAY
        READ(LOCAT) A
      END IF
!C
!C5------IF CNSTNT NOT ZERO THEN MULTIPLY ARRAY VALUES BY CNSTNT.
      IF(ICLOSE.NE.0) CLOSE(UNIT=LOCAT)
      ZERO=0.
      IF(CNSTNT.EQ.ZERO) GO TO 320
      DO 310 I=1,II
      DO 310 J=1,JJ
      A(J,I)=A(J,I)*CNSTNT
  310 CONTINUE
!C
!C6------IF PRINT CODE (IPRN) >0 OR =0 THEN PRINT ARRAY VALUES.
  320 IF(IPRN.GE.0) CALL ULAPRW(A,ANAME,0,0,JJ,II,0,IPRN,IOUT)
!C
!C7------RETURN
      RETURN
!C
!C8------CONTROL RECORD ERROR.
  500 IF(K.GT.0) THEN
         WRITE(IOUT,501) ANAME,K
  501    FORMAT(1X,/1X,'ERROR READING ARRAY CONTROL RECORD FOR ',A, &
     &     ' FOR LAYER',I4,':')
      ELSE
         WRITE(IOUT,502) ANAME
  502    FORMAT(1X,/1X,'ERROR READING ARRAY CONTROL RECORD FOR ',A,':')
      END IF
      WRITE(IOUT,'(1X,A)') CNTRL
      CALL USTOP(' ')
      END SUBROUTINE
      SUBROUTINE U2DDBL(A,ANAME,II,JJ,K,IN,IOUT)
!C     ******************************************************************
!C     ROUTINE TO INPUT 2-D DOUBLE PRECISION DATA MATRICES
!C       A IS ARRAY TO INPUT
!C       ANAME IS 24 CHARACTER DESCRIPTION OF A
!C       II IS NO. OF ROWS
!C       JJ IS NO. OF COLS
!C       K IS LAYER NO. (USED WITH NAME TO TITLE PRINTOUT --)
!C              IF K=0, NO LAYER IS PRINTED
!C              IF K<0, CROSS SECTION IS PRINTED)
!C       IN IS INPUT UNIT
!C       IOUT IS OUTPUT UNIT
!C     ******************************************************************
!C
!C        SPECIFICATIONS:
!C     ------------------------------------------------------------------
      USE OpenSpecModule
      double precision :: r,cnstnt
      CHARACTER*24 ANAME
      DOUBLE PRECISION,DIMENSION(JJ,II) :: A
      CHARACTER*20 FMTIN
      CHARACTER*200 CNTRL
      CHARACTER*16 TEXT
      CHARACTER*200 FNAME
      DATA NUNOPN/99/
!      INCLUDE 'openspec.inc'
!C     ------------------------------------------------------------------
!C
!C1------READ ARRAY CONTROL RECORD AS CHARACTER DATA.
      READ(IN,'(A)') CNTRL
!C
!C2------LOOK FOR ALPHABETIC WORD THAT INDICATES THAT THE RECORD IS FREE
!C2------FORMAT.  SET A FLAG SPECIFYING IF FREE FORMAT OR FIXED FORMAT.
      ICLOSE=0
      IFREE=1
      ICOL=1
      CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,1,N,R,IOUT,IN)
      IF (CNTRL(ISTART:ISTOP).EQ.'CONSTANT') THEN
         LOCAT=0
      ELSE IF(CNTRL(ISTART:ISTOP).EQ.'INTERNAL') THEN
         LOCAT=IN
      ELSE IF(CNTRL(ISTART:ISTOP).EQ.'EXTERNAL') THEN
!mf2015         CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,2,LOCAT,R,IOUT,IN)
        CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,0,N,R,IOUT,IN)
        inquire(file=cntrl(istart:istop),number=locat)
        if(locat<0) then
          write(iout,*)'Error.  External file not opened: ', &
                       cntrl(ISTART:ISTOP)
          write(iout,*)'Specify file using DATA or DATA(BINARY)'
          write(iout,*)'in the name file.'
          write(iout,*)'STOPPING...'
          call ustop(' ')
        endif
      ELSE IF(CNTRL(ISTART:ISTOP).EQ.'OPEN/CLOSE') THEN
         CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,0,N,R,IOUT,IN)
         FNAME=CNTRL(ISTART:ISTOP)
         LOCAT=NUNOPN
         WRITE(IOUT,15) LOCAT,FNAME
   15    FORMAT(1X,/1X,'OPENING FILE ON UNIT ',I4,':',/1X,A)
         ICLOSE=1
      ELSE
!C
!C2A-----DID NOT FIND A RECOGNIZED WORD, SO NOT USING FREE FORMAT.
!C2A-----READ THE CONTROL RECORD THE ORIGINAL WAY.
         IFREE=0
         READ(CNTRL,1,ERR=500) LOCAT,CNSTNT,FMTIN,IPRN
    1    FORMAT(I10,F10.0,A20,I10)
      END IF
!C
!C3------FOR FREE FORMAT CONTROL RECORD, READ REMAINING FIELDS.
      IF(IFREE.NE.0) THEN
         CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,3,N,CNSTNT,IOUT,IN)
         IF(LOCAT.NE.0) THEN
            CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,1,N,R,IOUT,IN)
            FMTIN=CNTRL(ISTART:ISTOP)
            IF(ICLOSE.NE.0) THEN
               IF(FMTIN.EQ.'(BINARY)') THEN
                  OPEN(UNIT=LOCAT,FILE=FNAME,FORM=FORM,ACCESS=ACCESS, &
     &                 ACTION=ACTION(1))
               ELSE
                  OPEN(UNIT=LOCAT,FILE=FNAME,ACTION=ACTION(1))
               END IF
            END IF
            IF(LOCAT.GT.0 .AND. FMTIN.EQ.'(BINARY)') LOCAT=-LOCAT
            CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,2,IPRN,R,IOUT,IN)
         END IF
      END IF
!C
!C4------TEST LOCAT TO SEE HOW TO DEFINE ARRAY VALUES.
      IF(LOCAT.EQ.0) THEN
!C
!C4A-----LOCAT=0; SET ALL ARRAY VALUES EQUAL TO CNSTNT. RETURN.
        DO 80 I=1,II
        DO 80 J=1,JJ
   80   A(J,I)=CNSTNT
        IF(K.GT.0) WRITE(IOUT,2) ANAME,CNSTNT,K
    2   FORMAT(1X,/1X,A,' =',1P,G14.6,' FOR LAYER',I4)
        IF(K.LE.0) WRITE(IOUT,3) ANAME,CNSTNT
    3   FORMAT(1X,/1X,A,' =',1P,G14.6)
        RETURN
      ELSE IF(LOCAT.GT.0) THEN
!C
!C4B-----LOCAT>0; READ FORMATTED RECORDS USING FORMAT FMTIN.
        IF(K.GT.0) THEN
           WRITE(IOUT,94) ANAME,K,LOCAT,FMTIN
   94      FORMAT(1X,///11X,A,' FOR LAYER',I4,/ &
     &      1X,'READING ON UNIT ',I4,' WITH FORMAT: ',A)
        ELSE IF(K.EQ.0) THEN
           WRITE(IOUT,95) ANAME,LOCAT,FMTIN
   95      FORMAT(1X,///11X,A,/ &
     &      1X,'READING ON UNIT ',I4,' WITH FORMAT: ',A)
        ELSE
           WRITE(IOUT,96) ANAME,LOCAT,FMTIN
   96      FORMAT(1X,///11X,A,' FOR CROSS SECTION',/ &
     &      1X,'READING ON UNIT ',I4,' WITH FORMAT: ',A)
        END IF
        DO 100 I=1,II
        IF(FMTIN.EQ.'(FREE)') THEN
           READ(LOCAT,*) (A(J,I),J=1,JJ)
        ELSE
           READ(LOCAT,FMTIN) (A(J,I),J=1,JJ)
        END IF
  100   CONTINUE
      ELSE
!C
!C4C-----LOCAT<0; READ UNFORMATTED ARRAY VALUES.
        LOCAT=-LOCAT
        IF(K.GT.0) THEN
           WRITE(IOUT,201) ANAME,K,LOCAT
  201      FORMAT(1X,///11X,A,' FOR LAYER',I4,/ &
     &      1X,'READING BINARY ON UNIT ',I4)
        ELSE IF(K.EQ.0) THEN
           WRITE(IOUT,202) ANAME,LOCAT
  202      FORMAT(1X,///1X,A,/ &
     &      1X,'READING BINARY ON UNIT ',I4)
        ELSE
           WRITE(IOUT,203) ANAME,LOCAT
  203      FORMAT(1X,///1X,A,' FOR CROSS SECTION',/ &
     &      1X,'READING BINARY ON UNIT ',I4)
        END IF
        READ(LOCAT) KSTP,KPER,PERTIM,TOTIM,TEXT,NCOL,NROW,ILAY
        READ(LOCAT) A
      END IF
!C
!C5------IF CNSTNT NOT ZERO THEN MULTIPLY ARRAY VALUES BY CNSTNT.
      IF(ICLOSE.NE.0) CLOSE(UNIT=LOCAT)
      ZERO=0.
      IF(CNSTNT.EQ.ZERO) GO TO 320
      DO 310 I=1,II
      DO 310 J=1,JJ
      A(J,I)=A(J,I)*CNSTNT
  310 CONTINUE
!C
!C6------IF PRINT CODE (IPRN) >0 OR =0 THEN PRINT ARRAY VALUES.
  320 IF(IPRN.GE.0) CALL ULAPRW(A,ANAME,0,0,JJ,II,0,IPRN,IOUT)
!C
!C7------RETURN
      RETURN
!C
!C8------CONTROL RECORD ERROR.
  500 IF(K.GT.0) THEN
         WRITE(IOUT,501) ANAME,K
  501    FORMAT(1X,/1X,'ERROR READING ARRAY CONTROL RECORD FOR ',A, &
     &     ' FOR LAYER',I4,':')
      ELSE
         WRITE(IOUT,502) ANAME
  502    FORMAT(1X,/1X,'ERROR READING ARRAY CONTROL RECORD FOR ',A,':')
      END IF
      WRITE(IOUT,'(1X,A)') CNTRL
      CALL USTOP(' ')
      END SUBROUTINE
      
      SUBROUTINE U2DINT(IA,ANAME,II,JJ,K,IN,IOUT)
!C     ******************************************************************
!C     ROUTINE TO INPUT 2-D INTEGER DATA MATRICES
!C       IA IS ARRAY TO INPUT
!C       ANAME IS 24 CHARACTER DESCRIPTION OF IA
!C       II IS NO. OF ROWS
!C       JJ IS NO. OF COLS
!C       K IS LAYER NO. (USED WITH NAME TO TITLE PRINTOUT --
!C              IF K=0, NO LAYER IS PRINTED
!C              IF K<0, CROSS SECTION IS PRINTED)
!C       IN IS INPUT UNIT
!C       IOUT IS OUTPUT UNIT
!C     ******************************************************************
!C
!C        SPECIFICATIONS:
!C     ------------------------------------------------------------------
      use OpenSpecModule
      double precision :: r
      CHARACTER*24 ANAME
      DIMENSION IA(JJ,II)
      CHARACTER*20 FMTIN
      CHARACTER*200 CNTRL
      CHARACTER*200 FNAME
      DATA NUNOPN/99/
!C     ------------------------------------------------------------------
!C
!C1------READ ARRAY CONTROL RECORD AS CHARACTER DATA.
      READ(IN,'(A)') CNTRL
!C
!C2------LOOK FOR ALPHABETIC WORD THAT INDICATES THAT THE RECORD IS FREE
!C2------FORMAT.  SET A FLAG SPECIFYING IF FREE FORMAT OR FIXED FORMAT.
      ICLOSE=0
      IFREE=1
      ICOL=1
      CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,1,N,R,IOUT,IN)
      IF (CNTRL(ISTART:ISTOP).EQ.'CONSTANT') THEN
         LOCAT=0
      ELSE IF(CNTRL(ISTART:ISTOP).EQ.'INTERNAL') THEN
         LOCAT=IN
      ELSE IF(CNTRL(ISTART:ISTOP).EQ.'EXTERNAL') THEN
!mf2015         CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,2,LOCAT,R,IOUT,IN)
        CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,0,N,R,IOUT,IN)
        inquire(file=cntrl(istart:istop),number=locat)
        if(locat<0) then
          write(iout,*)'Error.  External file not opened: ', &
                       cntrl(ISTART:ISTOP)
          write(iout,*)'Specify file using DATA or DATA(BINARY)'
          write(iout,*)'in the name file.'
          write(iout,*)'STOPPING...'
          call ustop(' ')
        endif
      ELSE IF(CNTRL(ISTART:ISTOP).EQ.'OPEN/CLOSE') THEN
         CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,0,N,R,IOUT,IN)
         FNAME=CNTRL(ISTART:ISTOP)
         LOCAT=NUNOPN
         WRITE(IOUT,15) LOCAT,FNAME
   15    FORMAT(1X,/1X,'OPENING FILE ON UNIT ',I4,':',/1X,A)
         ICLOSE=1
      ELSE
!C
!C2A-----DID NOT FIND A RECOGNIZED WORD, SO NOT USING FREE FORMAT.
!C2A-----READ THE CONTROL RECORD THE ORIGINAL WAY.
         IFREE=0
         READ(CNTRL,1,ERR=600) LOCAT,ICONST,FMTIN,IPRN
    1    FORMAT(I10,I10,A20,I10)
      END IF
!C
!C3------FOR FREE FORMAT CONTROL RECORD, READ REMAINING FIELDS.
      IF(IFREE.NE.0) THEN
         CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,2,ICONST,R,IOUT,IN)
         IF(LOCAT.NE.0) THEN
            CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,1,N,R,IOUT,IN)
            FMTIN=CNTRL(ISTART:ISTOP)
            IF(ICLOSE.NE.0) THEN
               IF(FMTIN.EQ.'(BINARY)') THEN
                  OPEN(UNIT=LOCAT,FILE=FNAME,FORM=FORM,ACCESS=ACCESS, &
     &                 ACTION=ACTION(1))
               ELSE
                  OPEN(UNIT=LOCAT,FILE=FNAME,ACTION=ACTION(1))
               END IF
            END IF
            IF(LOCAT.GT.0 .AND. FMTIN.EQ.'(BINARY)') LOCAT=-LOCAT
            CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,2,IPRN,R,IOUT,IN)
         END IF
      END IF
!C
!C4------TEST LOCAT TO SEE HOW TO DEFINE ARRAY VALUES.
      IF(LOCAT.EQ.0) THEN
!C
!C4A-----LOCAT=0; SET ALL ARRAY VALUES EQUAL TO ICONST. RETURN.
        DO 80 I=1,II
        DO 80 J=1,JJ
   80   IA(J,I)=ICONST
        IF(K.GT.0) WRITE(IOUT,82) ANAME,ICONST,K
   82   FORMAT(1X,/1X,A,' =',I15,' FOR LAYER',I4)
        IF(K.LE.0) WRITE(IOUT,83) ANAME,ICONST
   83   FORMAT(1X,/1X,A,' =',I15)
        RETURN
      ELSE IF(LOCAT.GT.0) THEN
!C
!C4B-----LOCAT>0; READ FORMATTED RECORDS USING FORMAT FMTIN.
        IF(K.GT.0) THEN
           WRITE(IOUT,94) ANAME,K,LOCAT,FMTIN
   94      FORMAT(1X,///11X,A,' FOR LAYER',I4,/ &
     &      1X,'READING ON UNIT ',I4,' WITH FORMAT: ',A)
        ELSE IF(K.EQ.0) THEN
           WRITE(IOUT,95) ANAME,LOCAT,FMTIN
   95      FORMAT(1X,///11X,A,/ &
     &      1X,'READING ON UNIT ',I4,' WITH FORMAT: ',A)
        ELSE
           WRITE(IOUT,96) ANAME,LOCAT,FMTIN
   96      FORMAT(1X,///11X,A,' FOR CROSS SECTION',/ &
     &      1X,'READING ON UNIT ',I4,' WITH FORMAT: ',A)
        END IF
        DO 100 I=1,II
        IF(FMTIN.EQ.'(FREE)') THEN
           READ(LOCAT,*) (IA(J,I),J=1,JJ)
        ELSE
           READ(LOCAT,FMTIN) (IA(J,I),J=1,JJ)
        END IF
  100   CONTINUE
      ELSE
!C
!C4C-----LOCAT<0; READ UNFORMATTED RECORD CONTAINING ARRAY VALUES.
        LOCAT=-LOCAT
        IF(K.GT.0) THEN
           WRITE(IOUT,201) ANAME,K,LOCAT
  201      FORMAT(1X,///11X,A,' FOR LAYER',I4,/ &
     &      1X,'READING BINARY ON UNIT ',I4)
        ELSE IF(K.EQ.0) THEN
           WRITE(IOUT,202) ANAME,LOCAT
  202      FORMAT(1X,///11X,A,/ &
     &      1X,'READING BINARY ON UNIT ',I4)
        ELSE
           WRITE(IOUT,203) ANAME,LOCAT
  203      FORMAT(1X,///11X,A,' FOR CROSS SECTION',/ &
     &      1X,'READING BINARY ON UNIT ',I4)
        END IF
        READ(LOCAT)
        READ(LOCAT) IA
      END IF
!C
!C5------IF ICONST NOT ZERO THEN MULTIPLY ARRAY VALUES BY ICONST.
      IF(ICLOSE.NE.0) CLOSE(UNIT=LOCAT)
      IF(ICONST.EQ.0) GO TO 320
      DO 310 I=1,II
      DO 310 J=1,JJ
      IA(J,I)=IA(J,I)*ICONST
  310 CONTINUE
!C
!C6------IF PRINT CODE (IPRN) <0 THEN RETURN.
  320 IF(IPRN.LT.0) RETURN
!C
!C7------PRINT COLUMN NUMBERS AT TOP OF PAGE.
      IF(IPRN.GT.9 .OR. IPRN.EQ.0) IPRN=6
      GO TO(401,402,403,404,405,406,407,408,409), IPRN
401   CALL UCOLNO(1,JJ,4,60,2,IOUT)
      GO TO 500
402   CALL UCOLNO(1,JJ,4,40,3,IOUT)
      GO TO 500
403   CALL UCOLNO(1,JJ,4,30,4,IOUT)
      GO TO 500
404   CALL UCOLNO(1,JJ,4,25,5,IOUT)
      GO TO 500
405   CALL UCOLNO(1,JJ,4,20,6,IOUT)
      GO TO 500
406   CALL UCOLNO(1,JJ,4,10,12,IOUT)
      GO TO 500
407   CALL UCOLNO(1,JJ,4,25,3,IOUT)
      GO TO 500
408   CALL UCOLNO(1,JJ,4,15,5,IOUT)
      GO TO 500
409   CALL UCOLNO(1,JJ,4,10,7,IOUT)
!C
!C8------PRINT EACH ROW IN THE ARRAY.
500   DO 510 I=1,II
      GO TO(501,502,503,504,505,506,507,508,509), IPRN
!C
!C----------------FORMAT 60I1
  501 WRITE(IOUT,551) I,(IA(J,I),J=1,JJ)
  551 FORMAT(1X,I3,1X,60(1X,I1):/(5X,60(1X,I1)))
      GO TO 510
!C
!C----------------FORMAT 40I2
  502 WRITE(IOUT,552) I,(IA(J,I),J=1,JJ)
  552 FORMAT(1X,I3,1X,40(1X,I2):/(5X,40(1X,I2)))
      GO TO 510
!C
!C----------------FORMAT 30I3
  503 WRITE(IOUT,553) I,(IA(J,I),J=1,JJ)
  553 FORMAT(1X,I3,1X,30(1X,I3):/(5X,30(1X,I3)))
      GO TO 510
!C
!C----------------FORMAT 25I4
  504 WRITE(IOUT,554) I,(IA(J,I),J=1,JJ)
  554 FORMAT(1X,I3,1X,25(1X,I4):/(5X,25(1X,I4)))
      GO TO 510
!C
!C----------------FORMAT 20I5
  505 WRITE(IOUT,555) I,(IA(J,I),J=1,JJ)
  555 FORMAT(1X,I3,1X,20(1X,I5):/(5X,20(1X,I5)))
      GO TO 510
!C
!C----------------FORMAT 10I11
  506 WRITE(IOUT,556) I,(IA(J,I),J=1,JJ)
  556 FORMAT(1X,I3,1X,10(1X,I11):/(5X,10(1X,I11)))
      GO TO 510
!C
!C----------------FORMAT 25I2
  507 WRITE(IOUT,557) I,(IA(J,I),J=1,JJ)
  557 FORMAT(1X,I3,1X,25(1X,I2):/(5X,25(1X,I2)))
      GO TO 510
!C
!C----------------FORMAT 15I4
  508 WRITE(IOUT,558) I,(IA(J,I),J=1,JJ)
  558 FORMAT(1X,I3,1X,15(1X,I4):/(5X,15(1X,I4)))
      GO TO 510
!C
!C----------------FORMAT 10I6
  509 WRITE(IOUT,559) I,(IA(J,I),J=1,JJ)
  559 FORMAT(1X,I3,1X,10(1X,I6):/(5X,10(1X,I6)))
!C
  510 CONTINUE
!C
!C9------RETURN
      RETURN
!C
!C10-----CONTROL RECORD ERROR.
  600 IF(K.GT.0) THEN
         WRITE(IOUT,601) ANAME,K
  601    FORMAT(1X,/1X,'ERROR READING ARRAY CONTROL RECORD FOR ',A, &
     &     ' FOR LAYER',I4,':')
      ELSE
         WRITE(IOUT,602) ANAME
  602    FORMAT(1X,/1X,'ERROR READING ARRAY CONTROL RECORD FOR ',A,':')
      END IF
      WRITE(IOUT,'(1X,A)') CNTRL
      CALL USTOP(' ')
      END SUBROUTINE

      SUBROUTINE UBDSV4(KSTP,KPER,TEXT,NAUX,AUXTXT,IBDCHN, &
     &          NCOL,NROW,NLAY,NLIST,IOUT,DELT,PERTIM,TOTIM)
!C     ******************************************************************
!C     WRITE HEADER RECORDS FOR CELL-BY-CELL FLOW TERMS FOR ONE COMPONENT
!C     OF FLOW PLUS AUXILIARY DATA USING A LIST STRUCTURE.  EACH ITEM IN
!C     THE LIST IS WRITTEN BY MODULE UBDSVB
!C     ******************************************************************
!C
!C        SPECIFICATIONS:
!C     ------------------------------------------------------------------
      CHARACTER*16 TEXT,AUXTXT(*)
      double precision,intent(in) :: delt,pertim,totim
!C     ------------------------------------------------------------------
!C
!C1------WRITE UNFORMATTED RECORDS IDENTIFYING DATA.
      IF(IOUT.GT.0) WRITE(IOUT,1) TEXT,IBDCHN,KSTP,KPER
    1 FORMAT(1X,'UBDSV4 SAVING "',A16,'" ON UNIT',I4, &
     &     ' AT TIME STEP',I3,', STRESS PERIOD',I4)
      WRITE(IBDCHN) KSTP,KPER,TEXT,NCOL,NROW,-NLAY
      WRITE(IBDCHN) 5,DELT,PERTIM,TOTIM
      WRITE(IBDCHN) NAUX+1
      IF(NAUX.GT.0) WRITE(IBDCHN) (AUXTXT(N),N=1,NAUX)
      WRITE(IBDCHN) NLIST
!C
!C2------RETURN
      RETURN
      END SUBROUTINE
      SUBROUTINE UBDSVB(IBDCHN,ICRL,Q,VAL,NVL,NAUX,LAUX)
!C     ******************************************************************
!C     WRITE ONE VALUE OF CELL-BY-CELL FLOW PLUS AUXILIARY DATA USING
!C     A LIST STRUCTURE.
!C     ******************************************************************
!C
!C        SPECIFICATIONS:
!C     ------------------------------------------------------------------
      DIMENSION VAL(NVL)
!C     ------------------------------------------------------------------
!C
!C1------WRITE CELL NUMBER AND FLOW RATE
      IF(NAUX.GT.0) THEN
         N2=LAUX+NAUX-1
         WRITE(IBDCHN) ICRL,Q,(VAL(N),N=LAUX,N2)
      ELSE
         WRITE(IBDCHN) ICRL,Q
      END IF
!C
!C2------RETURN
      RETURN
      END SUBROUTINE
      SUBROUTINE ULSTRD(NLIST,NODELIST,RLIST,LSTBEG,LDIM,MXLIST,IAL, &
     &     INPACK,IOUT,LABEL,CAUX,NCAUX,NAUX,AUXVAR,IFREFM,ISCLOC1, &
     &     ISCLOC2,IPRFLG,IUNSTR,MODELNDIM,MODELSHAPE)
!C     ******************************************************************
!C     Read and print a list.  NAUX of the values in the list are
!C     optional -- auxiliary data.
!C     ******************************************************************
      USE OpenSpecModule
      double precision :: r,sfac
      CHARACTER*(*) LABEL
      CHARACTER*16 CAUX(NCAUX)
      DIMENSION NODELIST(MXLIST)
      !DIMENSION RLIST(LDIM,MXLIST)
      DOUBLE PRECISION, DIMENSION(LDIM,MXLIST) :: RLIST
      DOUBLE PRECISION, DIMENSION(NAUX,MXLIST) :: AUXVAR
      INTEGER,DIMENSION(MODELNDIM) :: MODELSHAPE
      CHARACTER*200 LINE,FNAME
      CHARACTER*20 FMTARG, ACCARG
      CHARACTER*20 FILEFMT
      CHARACTER*30 CERR
      LOGICAL LVAL
      DATA NUNOPN/99/
      !INCLUDE 'openspec.inc'
!C     ------------------------------------------------------------------
!C0------Set NCOL,NROW,NLAY,NODES from MODELSHAPE
      NCOL=-1
      NROW=-1
      NLAY=-1
      NODES=-1
      IF(IUNSTR.EQ.0) THEN
        NCOL=MODELSHAPE(1)
        NROW=MODELSHAPE(2)
        NLAY=MODELSHAPE(3)
      ELSE
        NODES=MODELSHAPE(1)
      ENDIF
!C
!C1------If the list is empty, return.
      IF (NLIST.EQ.0) RETURN
!C
!C2------Check for and decode EXTERNAL and OPEN/CLOSE records.
      IN=INPACK
      ICLOSE=0
      IBINARY=0
      READ(IN,'(A)') LINE
      SFAC=1.
      LLOC=1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,I,R,IOUT,IN)
      IF(LINE(ISTART:ISTOP).EQ.'EXTERNAL') THEN
!mf2015         CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,I,R,IOUT,IN)
!mf2015         IN=I
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,0,N,R,IOUT,IN)
        inquire(file=line(istart:istop),number=in)
        if(in<0) then
          write(iout,*)'Error.  External file not opened: ', &
                       line(ISTART:ISTOP)
          write(iout,*)'Specify file using DATA or DATA(BINARY)'
          write(iout,*)'in the name file.'
          write(iout,*)'STOPPING...'
          call ustop(' ')
        endif
!C          TEST IF EXTERNAL FILE IS OPEN
!mf2015         INQUIRE( UNIT=IN, OPENED=LVAL )
!mf2015         IF ( LVAL.EQV. .FALSE. ) THEN
!mf2015           WRITE ( CERR,110 ) IN
!mf2015  110    FORMAT('External unit ', I4,' is not open')     
!mf2015           WRITE ( IOUT,'(1X,A)' ) CERR
!mf2015           CALL USTOP(CERR)
!mf2015         END IF
!C          TEST IF OPEN EXTERNAL FILE IS FORMATTED OR UNFORMATTED/BINARY
!C          SEE FORM VARIABLE IN openspec.inc
         FMTARG=FORM
         INQUIRE( UNIT=IN, FORM=FILEFMT )
         IF ( FILEFMT.EQ.FMTARG ) THEN
           IBINARY=1
         END IF
         IF(IPRFLG.EQ.1) THEN
           IF ( IBINARY.NE.1 ) THEN
             WRITE(IOUT,111) IN
  111        FORMAT(1X,'Reading list on unit ',I4)
           ELSE
             WRITE(IOUT,1111) IN
 1111        FORMAT(1X,'Reading list on binary unit ',I4)
           END IF
         END IF
         IF (IBINARY.NE.1) READ(IN,'(A)') LINE
      ELSE IF(LINE(ISTART:ISTOP).EQ.'OPEN/CLOSE') THEN
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,0,N,R,IOUT,IN)
         FNAME=LINE(ISTART:ISTOP)
         IN=NUNOPN
!C          TEST IF OPEN\CLOSE FILE EXISTS
         INQUIRE( FILE=FNAME, EXIST=LVAL )
         IF ( LVAL.EQV. .FALSE. ) THEN
           WRITE ( IOUT,112 ) LINE(ISTART:ISTOP)
  112      FORMAT('Specified OPEN/CLOSE file ',(A),' does not exit')
           CALL USTOP('Specified OPEN/CLOSE file does not exit')
         END IF
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,0,N,R,IOUT,IN)
         IF (LINE(ISTART:ISTOP).EQ.'(BINARY)') THEN
           IBINARY=1
           IF(IPRFLG.EQ.1)WRITE(IOUT,1115) IN,FNAME
 1115      FORMAT(1X,/1X,'OPENING BINARY FILE ON UNIT ',I4,':',/1X,A)
           FMTARG=FORM
           ACCARG=ACCESS
           OPEN(UNIT=IN,FILE=FNAME,ACTION=ACTION(1),FORM=FMTARG, &
     &          ACCESS=ACCARG,STATUS='OLD')
         ELSE
           IF(IPRFLG.EQ.1)WRITE(IOUT,115) IN,FNAME
  115      FORMAT(1X,/1X,'OPENING FILE ON UNIT ',I4,':',/1X,A)
           OPEN(UNIT=IN,FILE=FNAME,ACTION=ACTION(1))
         END IF
         ICLOSE=1
         IF (IBINARY.NE.1) READ(IN,'(A)') LINE
      END IF
!C
!C3------Check for SFAC record in ascii records.
      IF (IBINARY.NE.1) THEN
        LLOC=1
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,I,R,IOUT,IN)
        IF(LINE(ISTART:ISTOP).EQ.'SFAC') THEN
           CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,SFAC,IOUT,IN)
           IF(IPRFLG.EQ.1) THEN
             WRITE(IOUT,116) SFAC
  116        FORMAT(1X,'LIST SCALING FACTOR=',1PG12.5)
             IF(ISCLOC1.EQ.ISCLOC2) THEN
                WRITE(IOUT,113) ISCLOC1
  113         FORMAT(1X,'(THE SCALE FACTOR WAS APPLIED TO FIELD',I2,')')
             ELSE
                WRITE(IOUT,114) ISCLOC1,ISCLOC2
  114           FORMAT(1X,'(THE SCALE FACTOR WAS APPLIED TO FIELDS', &
     &             I2,'-',I2,')')
             END IF
           ENDIF
           READ(IN,'(A)') LINE
        END IF
      END IF
!C
!C3------Write a label for the list if the list will be printed.
      IF(IPRFLG.EQ.1) THEN
         WRITE(IOUT,'(1X)')
         CALL ULSTLB(IOUT,LABEL,CAUX,NCAUX,NAUX)
      END IF
!C
!C4------Setup indices for reading the list
      NREAD2=LDIM-IAL
      NREAD1=NREAD2-NAUX
      N=NLIST+LSTBEG-1
!C
!C4A-----READ THE LIST -- BINARY OR ASCII
      IF (IBINARY.NE.0) THEN
          !READ(IN) ((RLIST(JJ,II),JJ=1,NREAD2),II=LSTBEG,N)
          IF(IUNSTR.EQ.0) THEN
            DO II=LSTBEG,N
              READ(IN) K,I,J,(RLIST(JJ,II),JJ=1,LDIM), &
     &                       (AUXVAR(II,JJ),JJ=1,NAUX)
              NODELIST(II)=J+NCOL*(I-1)+(K-1)*NROW*NCOL
            ENDDO
          ELSE
            DO II=LSTBEG,N
              READ(IN) NODELIST(II),(RLIST(JJ,II),JJ=1,LDIM), &
     &                       (AUXVAR(II,JJ),JJ=1,NAUX)
            ENDDO
          ENDIF
      ELSE
!C
!C5------READ AN ASCII LIST
        DO 240 II=LSTBEG,N
!C
!C5A-----Read a line into the buffer.  (The first line has already been
!C5A-----read to scan for EXTERNAL and SFAC records.)
        IF(II.NE.LSTBEG) READ(IN,'(A)') LINE
!C
!C5B-----Get the non-optional values from the line.
        IF(IFREFM.EQ.0) THEN
          IF(IUNSTR.EQ.0) THEN
            READ(LINE,'(3I10,9F10.0)') K,I,J,(RLIST(JJ,II),JJ=1,LDIM)
            LLOC=10*(LDIM+3)+1
            NODELIST(II)=J+NCOL*(I-1)+(K-1)*NROW*NCOL
          ELSE
            READ(LINE,'(I10,9F10.0)') NODELIST(II), &
     &                                (RLIST(JJ,II),JJ=1,LDIM)
            LLOC=10*(LDIM+1)+1            
          ENDIF
        ELSE
          LLOC=1
          IF(IUNSTR.EQ.0) THEN
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,K,R,IOUT,IN)
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,I,R,IOUT,IN)
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,J,R,IOUT,IN)
            NODELIST(II)=J+NCOL*(I-1)+(K-1)*NROW*NCOL
          ELSE
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NODELIST(II),R,IOUT,IN)
          ENDIF
          DO 200 JJ=1,LDIM
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,IDUM,R,IOUT,IN)
            RLIST(JJ,II)=R
200       CONTINUE
        END IF
!C
!C5E-----Get the optional values from the line
        IF(NAUX.GT.0) THEN
           DO 210 JJ=1,NAUX
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,IDUM,R,IOUT,IN)
            AUXVAR(JJ,II)=R
210        CONTINUE
        END IF
!C
240     CONTINUE
      ENDIF 
!C
!C6----SCALE THE DATA AND CHECK 
      DO 250 II=LSTBEG,N
!C
!C6A------Scale fields ISCLOC1-ISCLOC2 by SFAC
      DO 204 ILOC=ISCLOC1,ISCLOC2
        RLIST(ILOC,II)=RLIST(ILOC,II)*SFAC
204   CONTINUE
!C
!C6B-----Write the values that were read if IPRFLG is 1.
      NN=II-LSTBEG+1
      IF(IUNSTR.EQ.0) THEN
        N=NODELIST(II)
        K=(N-1)/(NCOL*NROW)+1
        IJ=N-(K-1)*NCOL*NROW
        I=(IJ-1)/NCOL+1
        J=IJ-(I-1)*NCOL
        IF(IPRFLG.EQ.1) &
     &    WRITE(IOUT,205) NN,K,I,J,(RLIST(JJ,II),JJ=1,LDIM), &
     &                             (AUXVAR(JJ,II),JJ=1,NAUX)
205   FORMAT(1X,I6,I7,I7,I7,26G16.4)
!C
!C6C-----Check for illegal grid location
        IF(K.LT.1 .OR. K.GT.NLAY) THEN
           WRITE(IOUT,*) ' Layer number in list is outside of the grid'
           CALL USTOP(' ')
        END IF
        IF(I.LT.1 .OR. I.GT.NROW) THEN
           WRITE(IOUT,*) ' Row number in list is outside of the grid'
           CALL USTOP(' ')
        END IF
        IF(J.LT.1 .OR. J.GT.NCOL) THEN
           WRITE(IOUT,*) ' Column number in list is outside of the grid'
           CALL USTOP(' ')
        END IF
      ELSE
        NOD=NODELIST(II)
        IF(IPRFLG.EQ.1) &
     &    WRITE(IOUT,206) NN,NOD,(RLIST(JJ,II),JJ=1,LDIM), &
     &                           (AUXVAR(II,JJ),JJ=1,NAUX)
206   FORMAT(1X,I6,I7,26G16.4)
!C
!C6C-----Check for illegal grid location
        IF(NOD.LT.1 .OR. NOD.GT.NODES) THEN
           WRITE(IOUT,*) ' Node number in list is outside of the grid'
           CALL USTOP(' ')
        END IF
      ENDIF
  250 CONTINUE
!C
!C7------Done reading the list.  If file is open/close, close it.
      IF(ICLOSE.NE.0) CLOSE(UNIT=IN)
!C
      RETURN
     END SUBROUTINE

!!!!!!!!!!!!!!!!!!!!!!!!
! MODPATH Additions     
!!!!!!!!!!!!!!!!!!!!!!!!
    
!**************************************
! Double precision layer array routines
!**************************************
  subroutine u3ddblmpusg(iin,iout,neq,nlay,rval,cval,layerNodeCounts)
! ******************************************************************************
! Read three-dimensional double precision array for unstructured grid, consisting of 
! multiple 1d layer arrays with array headers. EXTERNAL key word is not 
! supported. Fixed format is not supported. All arrays are read using free format
! regardless of the value of FMTIN specified on the array control records.
! ******************************************************************************
! 
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    implicit none
    integer, intent(in) :: iin
    integer, intent(in) :: iout
    integer, intent(in) :: nlay
    integer, intent(in) :: neq
    doubleprecision, dimension(neq), intent(inout) :: rval
    integer, dimension(nlay), intent(in) :: layerNodeCounts
    character (len=24), intent(in) :: cval
    !local
    integer :: n, k, offset, firstElement, lastElement, layerNodeCount
    !functions
! ------------------------------------------------------------------------------
    !code
    offset = 0
    do k = 1, nlay
        firstElement = offset + 1
        layerNodeCount = layerNodeCounts(k)
        lastElement = offset + layerNodeCount
        call u1ddblmp(rval(firstElement:lastElement),cval,layerNodeCount,k,iin,iout)
        offset = lastElement
    end do
    
    return    
  end subroutine u3ddblmpusg
  subroutine u3ddblmp(iin,iout,nlay,nrow,ncol,neq,rval,cval)
! ******************************************************************************
! Read three-dimensional integer array, consisting of multiple 2d arrays with
! array headers.
! ******************************************************************************
! 
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    implicit none
    integer, intent(in) :: iin
    integer, intent(in) :: iout
    integer, intent(in) :: nlay
    integer, intent(in) :: nrow
    integer, intent(in) :: ncol
    integer, intent(in) :: neq
    double precision, dimension(neq), intent(inout) :: rval
    character (len=24), intent(in) :: cval
    !local
    double precision, dimension(:,:), allocatable :: rtemp
    integer :: node
    integer :: i, j, k
    !functions
! ------------------------------------------------------------------------------
    !code
    allocate(rtemp(ncol,nrow))
    do k = 1, nlay
      call u2ddblmp(rtemp,cval,nrow,ncol,k,iin,iout)
      do i = 1, nrow
        do j = 1, ncol
          call ugetnode(nlay,nrow,ncol,k,i,j,node)
          rval(node) = rtemp(j,i)
        end do
      end do
    end do
    deallocate(rtemp)
    return    
  end subroutine u3ddblmp     
      SUBROUTINE U2DDBLMP(A,ANAME,II,JJ,K,IN,IOUT)
!C     ******************************************************************
!C     ROUTINE TO INPUT 2-D DOUBLE PRECISION DATA MATRICES
!C       A IS ARRAY TO INPUT
!C       ANAME IS 24 CHARACTER DESCRIPTION OF A
!C       II IS NO. OF ROWS
!C       JJ IS NO. OF COLS
!C       K IS LAYER NO. (USED WITH NAME TO TITLE PRINTOUT --)
!C              IF K=0, NO LAYER IS PRINTED
!C!C       IN IS INPUT UNIT
!C       IOUT IS OUTPUT UNIT
!C     ******************************************************************
!C
!C        SPECIFICATIONS:
!C     ------------------------------------------------------------------
      USE OpenSpecModule
      double precision :: r,cnstnt
      CHARACTER*24 ANAME
      DOUBLE PRECISION,DIMENSION(JJ,II) :: A
      CHARACTER*20 FMTIN
      CHARACTER*200 CNTRL
      CHARACTER*16 TEXT
      CHARACTER*200 FNAME
      DATA NUNOPN/99/
!      INCLUDE 'openspec.inc'
!C     ------------------------------------------------------------------
!C
!C1------READ ARRAY CONTROL RECORD AS CHARACTER DATA.
      READ(IN,'(A)') CNTRL
!C
!C2------LOOK FOR ALPHABETIC WORD THAT INDICATES THAT THE RECORD IS FREE
!C2------FORMAT.  SET A FLAG SPECIFYING IF FREE FORMAT OR FIXED FORMAT.
      ICLOSE=0
      IFREE=1
      ICOL=1
      CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,1,N,R,IOUT,IN)
      IF (CNTRL(ISTART:ISTOP).EQ.'CONSTANT') THEN
         LOCAT=0
      ELSE IF(CNTRL(ISTART:ISTOP).EQ.'INTERNAL') THEN
         LOCAT=IN
      ELSE IF(CNTRL(ISTART:ISTOP).EQ.'OPEN/CLOSE') THEN
         CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,0,N,R,IOUT,IN)
         FNAME=CNTRL(ISTART:ISTOP)
         LOCAT=NUNOPN
         WRITE(IOUT,15) LOCAT,FNAME
   15    FORMAT(1X,/1X,'OPENING FILE ON UNIT ',I4,':',/1X,A)
         ICLOSE=1
      ELSE
!C
!C2A-----DID NOT FIND A RECOGNIZED WORD. Print error message and stop.
         write(iout,*) 'Did not find a recognized keyword on the array control record.'
         write(iout,*) 'MODPATH supports: CONSTANT, INTERNAL, and OPEN/CLOSE.'
         write(iout,*) 'Stopping.'
         call ustop(' ')
      END IF
!C
!C3------FOR FREE FORMAT CONTROL RECORD, READ REMAINING FIELDS.
      IF(IFREE.NE.0) THEN
         CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,3,N,CNSTNT,IOUT,IN)
         IF(LOCAT.NE.0) THEN
            CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,1,N,R,IOUT,IN)
            FMTIN=CNTRL(ISTART:ISTOP)
            IF(ICLOSE.NE.0) THEN
               IF(FMTIN.EQ.'(BINARY)') THEN
                  OPEN(UNIT=LOCAT,FILE=FNAME,FORM=FORM,ACCESS=ACCESS, &
     &                 ACTION=ACTION(1))
               ELSE
                  OPEN(UNIT=LOCAT,FILE=FNAME,ACTION=ACTION(1))
               END IF
            END IF
            IF(LOCAT.GT.0 .AND. FMTIN.EQ.'(BINARY)') LOCAT=-LOCAT
            CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,2,IPRN,R,IOUT,IN)
         END IF
      END IF
!C
!C4------TEST LOCAT TO SEE HOW TO DEFINE ARRAY VALUES.
      IF(LOCAT.EQ.0) THEN
!C
!C4A-----LOCAT=0; SET ALL ARRAY VALUES EQUAL TO CNSTNT. RETURN.
        DO 80 I=1,II
        DO 80 J=1,JJ
   80   A(J,I)=CNSTNT
        IF(K.GT.0) WRITE(IOUT,2) ANAME,CNSTNT,K
    2   FORMAT(1X,/1X,A,' =',1P,G14.6,' FOR LAYER',I4)
        IF(K.LE.0) WRITE(IOUT,3) ANAME,CNSTNT
    3   FORMAT(1X,/1X,A,' =',1P,G14.6)
        RETURN
      ELSE IF(LOCAT.GT.0) THEN
!C
!C4B-----LOCAT>0; READ FORMATTED RECORDS USING FORMAT FMTIN.
        IF(K.GT.0) THEN
           WRITE(IOUT,94) ANAME,K,LOCAT,FMTIN
   94      FORMAT(1X,///11X,A,' FOR LAYER',I4,/ &
     &      1X,'READING ON UNIT ',I4,' WITH FORMAT: ',A)
        ELSE IF(K.EQ.0) THEN
           WRITE(IOUT,95) ANAME,LOCAT,FMTIN
   95      FORMAT(1X,///11X,A,/ &
     &      1X,'READING ON UNIT ',I4,' WITH FORMAT: ',A)
        ELSE
           WRITE(IOUT,*) 'Invalid layer number specified while reading layer array data.'
           write(iout,*) 'Stopping.'
           call ustop(' ')
        END IF
        DO 100 I=1,II
        IF(FMTIN.EQ.'(FREE)') THEN
           READ(LOCAT,*) (A(J,I),J=1,JJ)
        ELSE
           READ(LOCAT,FMTIN) (A(J,I),J=1,JJ)
        END IF
  100   CONTINUE
      ELSE
!C
!C4C-----LOCAT<0; READ UNFORMATTED ARRAY VALUES.
        LOCAT=-LOCAT
        IF(K.GT.0) THEN
           WRITE(IOUT,201) ANAME,K,LOCAT
  201      FORMAT(1X,///11X,A,' FOR LAYER',I4,/ &
     &      1X,'READING BINARY ON UNIT ',I4)
        ELSE IF(K.EQ.0) THEN
           WRITE(IOUT,202) ANAME,LOCAT
  202      FORMAT(1X,///1X,A,/ &
     &      1X,'READING BINARY ON UNIT ',I4)
        ELSE
           WRITE(IOUT,203) ANAME,LOCAT
  203      FORMAT(1X,///1X,A,' FOR CROSS SECTION',/ &
     &      1X,'READING BINARY ON UNIT ',I4)
        END IF
        READ(LOCAT) KSTP,KPER,PERTIM,TOTIM,TEXT,NCOL,NROW,ILAY
        READ(LOCAT) A
      END IF
!C
!C5------IF CNSTNT NOT ZERO THEN MULTIPLY ARRAY VALUES BY CNSTNT.
      IF(ICLOSE.NE.0) CLOSE(UNIT=LOCAT)
      ZERO=0.
      IF(CNSTNT.EQ.ZERO) GO TO 320
      DO 310 I=1,II
      DO 310 J=1,JJ
      A(J,I)=A(J,I)*CNSTNT
  310 CONTINUE
!C
!C6------IF PRINT CODE (IPRN) >0 OR =0 THEN PRINT ARRAY VALUES.
  320 IF(IPRN.GE.0) CALL ULAPRW(A,ANAME,0,0,JJ,II,0,IPRN,IOUT)
!C
!C7------RETURN
      RETURN
!C
!C8------CONTROL RECORD ERROR.
  500 IF(K.GT.0) THEN
         WRITE(IOUT,501) ANAME,K
  501    FORMAT(1X,/1X,'ERROR READING ARRAY CONTROL RECORD FOR ',A, &
     &     ' FOR LAYER',I4,':')
      ELSE
         WRITE(IOUT,502) ANAME
  502    FORMAT(1X,/1X,'ERROR READING ARRAY CONTROL RECORD FOR ',A,':')
      END IF
      WRITE(IOUT,'(1X,A)') CNTRL
      CALL USTOP(' ')
      END SUBROUTINE
      SUBROUTINE u1ddblmp(A,ANAME,JJ,LAYER,IN,IOUT)
!C     ******************************************************************
!C     ROUTINE TO INPUT 1-D REAL DATA MATRICES
!C       A IS ARRAY TO INPUT
!C       ANAME IS 24 CHARACTER DESCRIPTION OF A
!C       JJ IS NO. OF ELEMENTS
!C       IN IS INPUT UNIT
!C       IOUT IS OUTPUT UNIT
!C     ******************************************************************
!C
!C        SPECIFICATIONS:
!C     ------------------------------------------------------------------
      USE OpenSpecModule
      CHARACTER*24 ANAME
      DOUBLE PRECISION,DIMENSION(jj),intent(inout) :: A
      double precision :: r,cnstnt
      integer :: m, i, j, k, nr, npr, jfirst, jlast
      CHARACTER*20 FMTIN
      CHARACTER*200 CNTRL
      CHARACTER*200 FNAME
      DATA NUNOPN/99/
!      INCLUDE 'openspec.inc'
!C     ------------------------------------------------------------------
!C
!C1------READ ARRAY CONTROL RECORD AS CHARACTER DATA.
      READ(IN,'(A)') CNTRL
!C
!C2------LOOK FOR ALPHABETIC WORD THAT INDICATES THAT THE RECORD IS FREE
!C2------FORMAT.  SET A FLAG SPECIFYING IF FREE FORMAT OR FIXED FORMAT.
      ICLOSE=0
      IFREE=1
      ICOL=1
      CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,1,N,R,IOUT,IN)
      IF (CNTRL(ISTART:ISTOP).EQ.'CONSTANT') THEN
         LOCAT=0
      ELSE IF(CNTRL(ISTART:ISTOP).EQ.'INTERNAL') THEN
         LOCAT=IN
      ELSE IF(CNTRL(ISTART:ISTOP).EQ.'OPEN/CLOSE') THEN
         CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,0,N,R,IOUT,IN)
         FNAME=CNTRL(ISTART:ISTOP)
         LOCAT=NUNOPN
         WRITE(IOUT,15) LOCAT,FNAME
   15    FORMAT(1X,/1X,'OPENING FILE ON UNIT ',I4,':',/1X,A)
         OPEN(UNIT=LOCAT,FILE=FNAME,ACTION=ACTION(1))
         ICLOSE=1
      ELSE
!C
!C2A-----DID NOT FIND A RECOGNIZED WORD. Print error message and stop.
         write(iout,*) 'Did not find a recognized keyword on the array control record.'
         write(iout,*) 'MODPATH supports: CONSTANT, INTERNAL, and OPEN/CLOSE.'
         write(iout,*) 'Stopping.'
         call ustop(' ')
      END IF
!C
!C3------FOR FREE FORMAT CONTROL RECORD, READ REMAINING FIELDS.
      IF(IFREE.NE.0) THEN
         CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,3,N,CNSTNT,IOUT,IN)
         IF(LOCAT.GT.0) THEN
            CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,1,N,R,IOUT,IN)
            FMTIN = cntrl(istart:istop)
            CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,2,IPRN,R,IOUT,IN)
         END IF
      END IF
!C
!C4------TEST LOCAT TO SEE HOW TO DEFINE ARRAY VALUES.
      IF(LOCAT.GT.0) GO TO 90
!C
!C4A-----LOCAT <0 OR =0; SET ALL ARRAY VALUES EQUAL TO CNSTNT. RETURN.
      DO 80 J=1,JJ
   80 A(J)=CNSTNT
      if(layer .eq. 0) WRITE(IOUT,3) ANAME,CNSTNT
    3 FORMAT(1X,/1X,A,' =',1P,G14.6)
      if(layer .ne. 0) write(iout,33) aname,cnstnt,layer
33    format(1x,/1x,a,' =',1p,G14.6,' FOR LAYER ',i4)       
      RETURN
!C
!C4B-----LOCAT>0; READ FORMATTED RECORDS USING FORMAT FMTIN.
   90 CONTINUE
      if(layer .eq. 0) WRITE(IOUT,5) ANAME,LOCAT,FMTIN
5     FORMAT(1X,///11X,A,/ &
     &       1X,'READING ON UNIT ',I4,' WITH FORMAT: ',A20)
      if(layer .ne. 0) write(iout,55) aname,layer,locat,fmtin
55    format(1x,///11x,a,' FOR LAYER ',i5/ &      
     &       1X,'READING ON UNIT ',I4,' WITH FORMAT: ',A20)
      if(fmtin .eq. '(FREE)') then
          READ(LOCAT,*) (A(J),J=1,JJ)
      else
          read(locat,fmtin) (a(j),j=1,jj)
      end if
      IF(ICLOSE.NE.0) CLOSE(UNIT=LOCAT)
!C
!C5------IF CNSTNT NOT ZERO THEN MULTIPLY ARRAY VALUES BY CNSTNT.
      ZERO=0.
      IF(CNSTNT.EQ.ZERO) GO TO 120
      DO 100 J=1,JJ
  100 A(J)=A(J)*CNSTNT
!C
!C6------IF PRINT CODE (IPRN) =0 OR >0 THEN PRINT ARRAY VALUES.
120 CONTINUE
    
      if(iprn .ge. 0) then
          npr = 10
          if(iprn .gt. 0) npr = 5
          m = mod(jj, npr)
          nr = (jj - m)/npr
          if(m .gt. 0) nr = nr + 1
          jlast = 0
          do i = 1, nr
              jfirst = jlast + 1
              jlast = jfirst + npr -1
              if(jlast .gt. jj) jlast = jj
              WRITE(IOUT,'(1X,I11,A,5X,1PG12.5,9(1X,G12.5))') jfirst, ':', (A(J),J=jfirst,jlast)
          end do
      end if

!C
!C7------RETURN
      RETURN
!C
!C8------CONTROL RECORD ERROR.
500   WRITE(IOUT,502) ANAME
502   FORMAT(1X,/1X,'ERROR READING ARRAY CONTROL RECORD FOR ',A,':')
      WRITE(IOUT,'(1X,A)') CNTRL
      CALL USTOP(' ')
      END SUBROUTINE

!*****************************
! Integer layer array routines
!*****************************
  subroutine u3dintmp(iin,iout,nlay,nrow,ncol,neq,ival,cval)
! ******************************************************************************
! Read three-dimensional integer array for a structured grid, consisting of 
! multiple 2d arrays with array headers.
! ******************************************************************************
! 
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    implicit none
    integer, intent(in) :: iin
    integer, intent(in) :: iout
    integer, intent(in) :: nlay
    integer, intent(in) :: nrow
    integer, intent(in) :: ncol
    integer, intent(in) :: neq
    integer, dimension(neq), intent(inout) :: ival
    character (len=24), intent(in) :: cval
    !local
    integer, dimension(:,:), allocatable :: itemp
    integer :: node
    integer :: i, j, k
    !functions
! ------------------------------------------------------------------------------
    !code
    allocate(itemp(ncol,nrow))
    do k = 1, nlay
      call u2dintmp(itemp,cval,nrow,ncol,k,iin,iout)
      do i = 1, nrow
        do j = 1, ncol
          call ugetnode(nlay,nrow,ncol,k,i,j,node)
          ival(node) = itemp(j,i)
        end do
      end do
    end do
    deallocate(itemp)
    return    
  end subroutine u3dintmp
  subroutine u3dintmpusg(iin,iout,neq,nlay,ival,cval,layerNodeCounts)
! ******************************************************************************
! Read three-dimensional integer array for unstructured grid, consisting of 
! multiple 1d layer arrays with array headers. EXTERNAL key word is not 
! supported. Fixed format is not supported. All arrays are read using free format
! regardless of the value of FMTIN specified on the array control records.
! ******************************************************************************
! 
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    implicit none
    integer, intent(in) :: iin
    integer, intent(in) :: iout
    integer, intent(in) :: nlay
    integer, intent(in) :: neq
    integer, dimension(neq), intent(inout) :: ival
    integer, dimension(nlay), intent(in) :: layerNodeCounts
    character (len=24), intent(in) :: cval
    !local
    integer :: n, k, offset, firstElement, lastElement, layerNodeCount
    !functions
! ------------------------------------------------------------------------------
    !code
    offset = 0
    do k = 1, nlay
        firstElement = offset + 1
        layerNodeCount = layerNodeCounts(k)
        lastElement = offset + layerNodeCount
        call u1dintmp(ival(firstElement:lastElement),cval,layerNodeCount,k,iin,iout)
        offset = lastElement
    end do
    
    return    
  end subroutine u3dintmpusg
      SUBROUTINE U1DINTMP(IA,ANAME,JJ,LAYER,IN,IOUT)
!C     ******************************************************************
!C     ROUTINE TO INPUT 1-D INTEGER DATA MATRICES
!C       IA IS ARRAY TO INPUT
!C       ANAME IS 24 CHARACTER DESCRIPTION OF A
!C       JJ IS NO. OF ELEMENTS
!C       IN IS INPUT UNIT
!C       IOUT IS OUTPUT UNIT
!C     ******************************************************************
!C
!C        SPECIFICATIONS:
!C     ------------------------------------------------------------------
      USE OpenSpecModule
      double precision :: r,cnstnt
      integer :: m, i, j, k, nr, npr, jfirst, jlast
      integer, dimension(9) :: nprvals
      CHARACTER*24 ANAME
      DIMENSION IA(JJ)
      CHARACTER*20 FMTIN
      CHARACTER*200 CNTRL
      CHARACTER*200 FNAME
      DATA NUNOPN/99/
      data nprvals/60,40,30,25,20,10,25,15,10/
      !INCLUDE 'openspec.inc'
!C     ------------------------------------------------------------------
!C
!C1------READ ARRAY CONTROL RECORD AS CHARACTER DATA.
      READ(IN,'(A)') CNTRL
!C
!C2------LOOK FOR ALPHABETIC WORD THAT INDICATES THAT THE RECORD IS FREE
!C2------FORMAT.  SET A FLAG SPECIFYING IF FREE FORMAT OR FIXED FORMAT.
      ICLOSE=0
      IFREE=1
      ICOL=1
      CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,1,N,R,IOUT,IN)
      IF (CNTRL(ISTART:ISTOP).EQ.'CONSTANT') THEN
         LOCAT=0
      ELSE IF(CNTRL(ISTART:ISTOP).EQ.'INTERNAL') THEN
         LOCAT=IN
      ELSE IF(CNTRL(ISTART:ISTOP).EQ.'OPEN/CLOSE') THEN
         CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,0,N,R,IOUT,IN)
         FNAME=CNTRL(ISTART:ISTOP)
         LOCAT=NUNOPN
         WRITE(IOUT,15) LOCAT,FNAME
   15    FORMAT(1X,/1X,'OPENING FILE ON UNIT ',I4,':',/1X,A)
         OPEN(UNIT=LOCAT,FILE=FNAME,ACTION=ACTION(1))
         ICLOSE=1
      ELSE
!C
!C2A-----Did not find a recognized key word, so stop with an error message.
         WRITE(IOUT,502) ANAME
503      FORMAT(1X,/1X,'ERROR READING ARRAY CONTROL RECORD FOR ',A,':')
         WRITE(IOUT,'(1X,A)') CNTRL
         write(iout,'(1x,a)') 'Control records must specify data access as CONSTANT, INTERNAL, or OPEN/CLOSE.'
         CALL USTOP(' ')
          
      END IF
!C
!C3------FOR FREE FORMAT CONTROL RECORD, READ REMAINING FIELDS.
      IF(IFREE.NE.0) THEN
         CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,2,ICNSTNT,R,IOUT,IN)
         IF(LOCAT.GT.0) THEN
            CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,1,N,R,IOUT,IN)
            fmtin = cntrl(istart:istop)
            CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,2,IPRN,R,IOUT,IN)
         END IF
      END IF
!C
!C4------TEST LOCAT TO SEE HOW TO DEFINE ARRAY VALUES.
      IF(LOCAT.GT.0) GO TO 90
!C
!C4A-----LOCAT <0 OR =0; SET ALL ARRAY VALUES EQUAL TO ICNSTNT. RETURN.
      DO 80 J=1,JJ
80    IA(J)=ICNSTNT
      if(layer .eq. 0) WRITE(IOUT,3) ANAME,ICNSTNT
    3 FORMAT(1X,/1X,A,' =',1P,I10)
      if(layer .ne. 0) write(iout,33) aname,icnstnt,layer
33    format(1x,/1x,a,' =',1p,i10,' FOR LAYER ',i4)       
      RETURN
!C
!C4B-----LOCAT>0; READ FORMATTED RECORDS USING FORMAT FMTIN.
   90 CONTINUE
      if(layer .eq. 0) WRITE(IOUT,5) ANAME,LOCAT,FMTIN
5     FORMAT(1X,///11X,A,/ &
     &       1X,'READING ON UNIT ',I4,' WITH FORMAT: ',A20)
      if(layer .ne. 0) write(iout,55) aname,layer,locat,fmtin
55    format(1x,///11x,a,' FOR LAYER ',i5/ &      
     &       1X,'READING ON UNIT ',I4,' WITH FORMAT: ',A20)
      
      if(fmtin .eq. '(FREE)') then
          READ(LOCAT,*) (IA(J),J=1,JJ)
      else
          read(locat,fmtin) (ia(j),j=1,jj)
      end if
      
      IF(ICLOSE.NE.0) CLOSE(UNIT=LOCAT)
!C
!C5------IF ICNSTNT NOT ZERO THEN MULTIPLY ARRAY VALUES BY ICNSTNT.
      ZERO=0.
      IF(ICNSTNT.EQ.ZERO) GO TO 120
      DO 100 J=1,JJ
  100 IA(J)=IA(J)*ICNSTNT
!C
!C6------IF PRINT CODE (IPRN) >= 0 and <=9 THEN PRINT ARRAY VALUES.
120 CONTINUE
    if(iprn .ge. 0) then
      if(iprn .eq. 0) iprn = 6
      npr = nprvals(iprn)
      m = mod(jj, npr)
      nr = (jj - m)/npr
      if(m .gt. 0) nr = nr + 1
      
      jlast = 0
      do i = 1, nr
          jfirst = jlast + 1
          jlast = jfirst + npr -1
          if(jlast .gt. jj) jlast = jj
          select case(iprn)
            case(1)
!-------------FORMAT 60I1
              WRITE(IOUT,'(1X,I11,A,5X,60(1X,I1))') jfirst, ':', (IA(J),J=jfirst,jlast)
            case(2)
              WRITE(IOUT,'(1X,I11,A,5X,40(1X,I2))') jfirst, ':', (IA(J),J=jfirst,jlast)
            case(3)
              WRITE(IOUT,'(1X,I11,A,5X,60(1X,I3))') jfirst, ':', (IA(J),J=jfirst,jlast)
            case(4)
              WRITE(IOUT,'(1X,I11,A,5X,60(1X,I4))') jfirst, ':', (IA(J),J=jfirst,jlast)
            case(5)
              WRITE(IOUT,'(1X,I11,A,5X,60(1X,I5))') jfirst, ':', (IA(J),J=jfirst,jlast)
            case(6)
              WRITE(IOUT,'(1X,I11,A,5X,60(1X,I11))') jfirst, ':', (IA(J),J=jfirst,jlast)
            case(7)
              WRITE(IOUT,'(1X,I11,A,5X,60(1X,I2))') jfirst, ':', (IA(J),J=jfirst,jlast)
            case(8)
              WRITE(IOUT,'(1X,I11,A,5X,60(1X,I4))') jfirst, ':', (IA(J),J=jfirst,jlast)
            case(9)
              WRITE(IOUT,'(1X,I11,A,5X,60(1X,I6))') jfirst, ':', (IA(J),J=jfirst,jlast)
            case default
              ! do not print
          end select
      end do
      
    end if
!C
!C7------RETURN
      RETURN
!C
!C8------CONTROL RECORD ERROR.
500   WRITE(IOUT,502) ANAME
502   FORMAT(1X,/1X,'ERROR READING ARRAY CONTROL RECORD FOR ',A,':')
      WRITE(IOUT,'(1X,A)') CNTRL
      CALL USTOP(' ')
      END SUBROUTINE
      SUBROUTINE U2DINTMP(IA,ANAME,II,JJ,K,IN,IOUT)
!C     ******************************************************************
!C     ROUTINE TO INPUT 2-D INTEGER DATA MATRICES
!C       IA IS ARRAY TO INPUT
!C       ANAME IS 24 CHARACTER DESCRIPTION OF IA
!C       II IS NO. OF ROWS
!C       JJ IS NO. OF COLS
!C       K IS LAYER NO. (USED WITH NAME TO TITLE PRINTOUT --
!C              IF K=0, NO LAYER IS PRINTED
!C              IF K<0, CROSS SECTION IS PRINTED)
!C       IN IS INPUT UNIT
!C       IOUT IS OUTPUT UNIT
!C     ******************************************************************
!C
!C        SPECIFICATIONS:
!C     ------------------------------------------------------------------
      use OpenSpecModule
      double precision :: r
      CHARACTER*24 ANAME
      DIMENSION IA(JJ,II)
      CHARACTER*20 FMTIN
      CHARACTER*200 CNTRL
      CHARACTER*200 FNAME
      DATA NUNOPN/99/
!C     ------------------------------------------------------------------
!C
!C1------READ ARRAY CONTROL RECORD AS CHARACTER DATA.
      READ(IN,'(A)') CNTRL
!C
!C2------LOOK FOR ALPHABETIC WORD THAT INDICATES THAT THE RECORD IS FREE
!C2------FORMAT.  SET A FLAG SPECIFYING IF FREE FORMAT OR FIXED FORMAT.
      ICLOSE=0
      IFREE=1
      ICOL=1
      CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,1,N,R,IOUT,IN)
      IF (CNTRL(ISTART:ISTOP).EQ.'CONSTANT') THEN
         LOCAT=0
      ELSE IF(CNTRL(ISTART:ISTOP).EQ.'INTERNAL') THEN
         LOCAT=IN
      ELSE IF(CNTRL(ISTART:ISTOP).EQ.'OPEN/CLOSE') THEN
         CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,0,N,R,IOUT,IN)
         FNAME=CNTRL(ISTART:ISTOP)
         LOCAT=NUNOPN
         WRITE(IOUT,15) LOCAT,FNAME
   15    FORMAT(1X,/1X,'OPENING FILE ON UNIT ',I4,':',/1X,A)
         ICLOSE=1
      ELSE
!C
!C2A-----DID NOT FIND A RECOGNIZED WORD. Print error message and stop.
         write(iout,*) 'Did not find a recognized keyword on the array control record.'
         write(iout,*) 'MODPATH supports: CONSTANT, INTERNAL, and OPEN/CLOSE.'
         write(iout,*) 'Stopping.'
         call ustop(' ')          
      END IF
!C
!C3------FOR FREE FORMAT CONTROL RECORD, READ REMAINING FIELDS.
      IF(IFREE.NE.0) THEN
         CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,2,ICONST,R,IOUT,IN)
         IF(LOCAT.NE.0) THEN
            CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,1,N,R,IOUT,IN)
            FMTIN=CNTRL(ISTART:ISTOP)
            IF(ICLOSE.NE.0) THEN
               IF(FMTIN.EQ.'(BINARY)') THEN
                  OPEN(UNIT=LOCAT,FILE=FNAME,FORM=FORM,ACCESS=ACCESS, &
     &                 ACTION=ACTION(1))
               ELSE
                  OPEN(UNIT=LOCAT,FILE=FNAME,ACTION=ACTION(1))
               END IF
            END IF
            IF(LOCAT.GT.0 .AND. FMTIN.EQ.'(BINARY)') LOCAT=-LOCAT
            CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,2,IPRN,R,IOUT,IN)
         END IF
      END IF
!C
!C4------TEST LOCAT TO SEE HOW TO DEFINE ARRAY VALUES.
      IF(LOCAT.EQ.0) THEN
!C
!C4A-----LOCAT=0; SET ALL ARRAY VALUES EQUAL TO ICONST. RETURN.
        DO 80 I=1,II
        DO 80 J=1,JJ
   80   IA(J,I)=ICONST
        IF(K.GT.0) WRITE(IOUT,82) ANAME,ICONST,K
   82   FORMAT(1X,/1X,A,' =',I15,' FOR LAYER',I4)
        IF(K.LE.0) WRITE(IOUT,83) ANAME,ICONST
   83   FORMAT(1X,/1X,A,' =',I15)
        RETURN
      ELSE IF(LOCAT.GT.0) THEN
!C
!C4B-----LOCAT>0; READ FORMATTED RECORDS USING FORMAT FMTIN.
        IF(K.GT.0) THEN
           WRITE(IOUT,94) ANAME,K,LOCAT,FMTIN
   94      FORMAT(1X,///11X,A,' FOR LAYER',I4,/ &
     &      1X,'READING ON UNIT ',I4,' WITH FORMAT: ',A)
        ELSE IF(K.EQ.0) THEN
           WRITE(IOUT,95) ANAME,LOCAT,FMTIN
   95      FORMAT(1X,///11X,A,/ &
     &      1X,'READING ON UNIT ',I4,' WITH FORMAT: ',A)
        ELSE
           WRITE(IOUT,96) ANAME,LOCAT,FMTIN
   96      FORMAT(1X,///11X,A,' FOR CROSS SECTION',/ &
     &      1X,'READING ON UNIT ',I4,' WITH FORMAT: ',A)
        END IF
        DO 100 I=1,II
        IF(FMTIN.EQ.'(FREE)') THEN
           READ(LOCAT,*) (IA(J,I),J=1,JJ)
        ELSE
           READ(LOCAT,FMTIN) (IA(J,I),J=1,JJ)
        END IF
  100   CONTINUE
      ELSE
!C
!C2A-----DID NOT FIND A RECOGNIZED WORD. Print error message and stop.
         write(iout,*) 'Did not find a recognized keyword on the array control record.'
         write(iout,*) 'MODPATH supports: CONSTANT, INTERNAL, and OPEN/CLOSE.'
         write(iout,*) 'Stopping.'
         call ustop(' ')
          
!C
!C4C-----LOCAT<0; READ UNFORMATTED RECORD CONTAINING ARRAY VALUES.
        LOCAT=-LOCAT
        IF(K.GT.0) THEN
           WRITE(IOUT,201) ANAME,K,LOCAT
  201      FORMAT(1X,///11X,A,' FOR LAYER',I4,/ &
     &      1X,'READING BINARY ON UNIT ',I4)
        ELSE IF(K.EQ.0) THEN
           WRITE(IOUT,202) ANAME,LOCAT
  202      FORMAT(1X,///11X,A,/ &
     &      1X,'READING BINARY ON UNIT ',I4)
        ELSE
           WRITE(IOUT,*) 'Invalid layer number specified while reading layer array data.'
           write(iout,*) 'Stopping.'
           call ustop(' ')
        END IF
        READ(LOCAT)
        READ(LOCAT) IA
      END IF
!C
!C5------IF ICONST NOT ZERO THEN MULTIPLY ARRAY VALUES BY ICONST.
      IF(ICLOSE.NE.0) CLOSE(UNIT=LOCAT)
      IF(ICONST.EQ.0) GO TO 320
      DO 310 I=1,II
      DO 310 J=1,JJ
      IA(J,I)=IA(J,I)*ICONST
  310 CONTINUE
!C
!C6------IF PRINT CODE (IPRN) <0 THEN RETURN.
  320 IF(IPRN.LT.0) RETURN
!C
!C7------PRINT COLUMN NUMBERS AT TOP OF PAGE.
      IF(IPRN.GT.9 .OR. IPRN.EQ.0) IPRN=6
      GO TO(401,402,403,404,405,406,407,408,409), IPRN
401   CALL UCOLNO(1,JJ,4,60,2,IOUT)
      GO TO 500
402   CALL UCOLNO(1,JJ,4,40,3,IOUT)
      GO TO 500
403   CALL UCOLNO(1,JJ,4,30,4,IOUT)
      GO TO 500
404   CALL UCOLNO(1,JJ,4,25,5,IOUT)
      GO TO 500
405   CALL UCOLNO(1,JJ,4,20,6,IOUT)
      GO TO 500
406   CALL UCOLNO(1,JJ,4,10,12,IOUT)
      GO TO 500
407   CALL UCOLNO(1,JJ,4,25,3,IOUT)
      GO TO 500
408   CALL UCOLNO(1,JJ,4,15,5,IOUT)
      GO TO 500
409   CALL UCOLNO(1,JJ,4,10,7,IOUT)
!C
!C8------PRINT EACH ROW IN THE ARRAY.
500   DO 510 I=1,II
      GO TO(501,502,503,504,505,506,507,508,509), IPRN
!C
!C----------------FORMAT 60I1
  501 WRITE(IOUT,551) I,(IA(J,I),J=1,JJ)
  551 FORMAT(1X,I3,1X,60(1X,I1):/(5X,60(1X,I1)))
      GO TO 510
!C
!C----------------FORMAT 40I2
  502 WRITE(IOUT,552) I,(IA(J,I),J=1,JJ)
  552 FORMAT(1X,I3,1X,40(1X,I2):/(5X,40(1X,I2)))
      GO TO 510
!C
!C----------------FORMAT 30I3
  503 WRITE(IOUT,553) I,(IA(J,I),J=1,JJ)
  553 FORMAT(1X,I3,1X,30(1X,I3):/(5X,30(1X,I3)))
      GO TO 510
!C
!C----------------FORMAT 25I4
  504 WRITE(IOUT,554) I,(IA(J,I),J=1,JJ)
  554 FORMAT(1X,I3,1X,25(1X,I4):/(5X,25(1X,I4)))
      GO TO 510
!C
!C----------------FORMAT 20I5
  505 WRITE(IOUT,555) I,(IA(J,I),J=1,JJ)
  555 FORMAT(1X,I3,1X,20(1X,I5):/(5X,20(1X,I5)))
      GO TO 510
!C
!C----------------FORMAT 10I11
  506 WRITE(IOUT,556) I,(IA(J,I),J=1,JJ)
  556 FORMAT(1X,I3,1X,10(1X,I11):/(5X,10(1X,I11)))
      GO TO 510
!C
!C----------------FORMAT 25I2
  507 WRITE(IOUT,557) I,(IA(J,I),J=1,JJ)
  557 FORMAT(1X,I3,1X,25(1X,I2):/(5X,25(1X,I2)))
      GO TO 510
!C
!C----------------FORMAT 15I4
  508 WRITE(IOUT,558) I,(IA(J,I),J=1,JJ)
  558 FORMAT(1X,I3,1X,15(1X,I4):/(5X,15(1X,I4)))
      GO TO 510
!C
!C----------------FORMAT 10I6
  509 WRITE(IOUT,559) I,(IA(J,I),J=1,JJ)
  559 FORMAT(1X,I3,1X,10(1X,I6):/(5X,10(1X,I6)))
!C
  510 CONTINUE
!C
!C9------RETURN
      RETURN
!C
!C10-----CONTROL RECORD ERROR.
  600 IF(K.GT.0) THEN
         WRITE(IOUT,601) ANAME,K
  601    FORMAT(1X,/1X,'ERROR READING ARRAY CONTROL RECORD FOR ',A, &
     &     ' FOR LAYER',I4,':')
      ELSE
         WRITE(IOUT,602) ANAME
  602    FORMAT(1X,/1X,'ERROR READING ARRAY CONTROL RECORD FOR ',A,':')
      END IF
      WRITE(IOUT,'(1X,A)') CNTRL
      CALL USTOP(' ')
      END SUBROUTINE
     
      END MODULE UTL8MODULE