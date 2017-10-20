# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/lnd/clm2/src/main/fileutils.F90"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/lnd/clm2/src/main/fileutils.F90"

# 1 "./misc.h" 1
# 2 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/lnd/clm2/src/main/fileutils.F90" 2

# 1 "./preproc.h" 1






 
# 3 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/lnd/clm2/src/main/fileutils.F90" 2

module fileutils

    private

!public methods

    public :: get_filename  
    public :: set_filename
    public :: opnfil
    public :: getfil
    public :: putfil
    public :: getavu
    public :: relavu
    private:: shell_cmd

    logical, public :: lsmiou(99)  !I/O file unit numbers (1 to 99)

!=======================================================================
CONTAINS
!=======================================================================

  character(len=256) function get_filename (fulpath)

!----------------------------------------------------------------------- 
! 
! Purpose: 
! returns filename given full pathname
!
! Method: 
! 
! Author: Mariana Vertenstein
! 
!-----------------------------------------------------------------------

! ------------------------ arguments --------------------------------
    character(len=*), intent(in)  :: fulpath !full pathname
! -------------------------------------------------------------------

! ------------------------ local variables --------------------------
    integer i               !loop index
    integer klen            !length of fulpath character string
! -------------------------------------------------------------------

    klen = len_trim(fulpath)
    do i = klen, 1, -1
       if (fulpath(i:i) == '/') go to 10
    end do
    i = 0
10  get_filename = fulpath(i+1:klen)
    
    return
  end function get_filename
  
!=======================================================================

  character(len=256) function set_filename (rem_dir, loc_fn)
  
!----------------------------------------------------------------------- 
! 
! Purpose: 
! Set remote full path filename
! 
! Method: 
! 
! Author: Mariana Vertenstein
! 
!-----------------------------------------------------------------------

! ------------------------ arguments ------------------------------
    character(len=*), intent(in)  :: rem_dir !remote directory
    character(len=*), intent(in)  :: loc_fn  !local full path filename
! -----------------------------------------------------------------
    
! ------------------------ local variables ------------------------
    integer :: i   !integer
! -----------------------------------------------------------------

    set_filename = ' '
    do i = len_trim(loc_fn), 1, -1
       if (loc_fn(i:i)=='/') go to 10
    end do
    i = 0
10  set_filename = trim(rem_dir) // loc_fn(i+1:len_trim(loc_fn))
    
  end function set_filename

!=======================================================================

   subroutine getfil (fulpath, locfn, iflag)
 
!----------------------------------------------------------------------- 
! 
! Purpose: 
! Obtain local copy of file
! First check current working directory
! Next check full pathname[fulpath] on disk
! Finally check full pathname[fulpath] on mass store
! 
! Method: 
! 
! Author: Mariana Vertenstein
! 
!-----------------------------------------------------------------------
 
! ------------------------ arguments -----------------------------------
   character(len=*), intent(in)  :: fulpath !MSS or permanent disk full pathname
   character(len=*), intent(out) :: locfn   !output local file name
   integer, optional, intent(in) :: iflag   !0=>abort if file not found 1=>do not abort
! --------------------------------------------------------------------
 
! ------------------------ local variables ---------------------------
   integer i               !loop index
   integer klen            !length of fulpath character string
   integer ierr            !error status
   logical lexist          !true if local file exists
   character(len=256) text !mswrite command
! --------------------------------------------------------------------
 
 
! get local file name from full name: start at end. look for first "/"
 
   klen = len_trim(fulpath)
   do i = klen, 1, -1
      if (fulpath(i:i).eq.'/') go to 100
   end do
   i = 0
  100 locfn = fulpath(i+1:klen)
   if (len_trim(locfn) == 0) then
      write(6,*)'(GETFIL): local filename has zero length'
      call endrun
   else
      write(6,*)'(GETFIL): attempting to find local file ',          &
     &     trim(locfn)
   endif
 
! first check if file is in current working directory.
 
   inquire (file=locfn,exist=lexist)
   if (lexist) then
      write (6,*) '(GETFIL): using ',trim(locfn),                    &
     &     ' in current working directory'
      RETURN
   endif
 
! second check for full pathname on disk
 
   inquire(file=fulpath,exist=lexist)
   if (lexist) then
      locfn = trim(fulpath)
      write(6,*)'(GETFIL): using ',trim(fulpath)
      return
   endif
 
! finally check on mass store
 
   text='msread '//trim(locfn)//' '//trim(fulpath)
   call shell_cmd(text, ierr)
   if (ierr==0) then
      write(6,*)'(GETFIL): File ',trim(locfn),' read from MSS'
   else  ! all tries to get file have been unsuccessful
      write(6,*)'(GETFIL): failed cmd=',trim(text)
      if (present(iflag) .and. iflag==0) then
         call endrun
      else
         RETURN
      endif
   end if
 
   return
   end subroutine getfil
 
!=======================================================================
 
   subroutine putfil(locfn, mssfpn, pass, irt, lremov)
 
!----------------------------------------------------------------------- 
! 
! Purpose: 
! Dispose to Mass Store only if nonzero retention period.
! 
! Method: 
! Put mswrite command in background for asynchronous behavior.
! The string put into 'cmd' below needs to be changed to 
! the appropriate archival command for the users system 
! if a shell command 'mswrite' does not exist.
! 
! Author: Mariana Vertenstein
! 
!-----------------------------------------------------------------------
 
!------------------------------Arguments--------------------------------
   character(len=*), intent(in) :: locfn   ! Local filename
   character(len=*), intent(in) :: mssfpn  ! Mass Store full pathname
   character(len=*), intent(in) :: pass    ! write password
   integer, intent(in) :: irt              ! Mass Store retention time
   logical, intent(in) :: lremov           ! true=>remove local file
!-----------------------------------------------------------------------
 
!---------------------------Local workspace-----------------------------
   character(len=256) cmd     ! Command string
   character(len=256) cmdtem  ! Temporary for command string
   character(len=  4) crt     ! Retention time as characters
   character(len= 16) wpass   ! Write password
   integer ier                ! error number
!-----------------------------------------------------------------------
 
 
   if (irt/=0) then
      wpass = ' '
      if (pass(1:1) /= ' ') wpass = ' -w ' // trim(pass)
      write (crt,'(i4)') irt
!!    (wh 03.04)
!      write (cmd,'(100a)') 'mswrite ',' -t ',crt,trim(wpass),' ',&
!           trim(locfn),' ',trim(mssfpn)
      write(cmd,'(100a)') 'cp ',trim(locfn),' ',trim(mssfpn)
!!     
      if (lremov) then
         cmdtem = '('//trim(cmd)//'; /bin/rm '//trim(locfn)//' )&'
      else
         cmdtem = '('//trim(cmd)//' )&'
      end if
      write(6,*)'(PUTFIL): Issuing shell cmd:',trim(cmdtem)
      call shell_cmd(cmdtem, ier)
      if (ier /= 0) then
         write(6,*)'(PUTFIL): Error from shell cmd'
!!         call endrun
      end if
   endif

   return
   end subroutine putfil
 
!=======================================================================
 
   subroutine opnfil (locfn, iun, form)
 
!----------------------------------------------------------------------- 
! 
! Purpose: 
! open file locfn in unformatted or formatted form on unit iun
! 
! Method: 
! 
! Author: Mariana Vertenstein
! 
!-----------------------------------------------------------------------
 
! ------------------------ input variables ---------------------------
   character(len=*), intent(in):: locfn  !file name
   integer, intent(in):: iun             !fortran unit number
   character(len=1), intent(in):: form   !file format: u = unformatted. f = formatted
! --------------------------------------------------------------------
 
! ------------------------ local variables ---------------------------
   integer ioe             !error return from fortran open
   character(len=11) ft    !format type: formatted. unformatted
! --------------------------------------------------------------------
 
   if (len_trim(locfn) == 0) then
      write(6,*)'(OPNFIL): local filename has zero length'
      call endrun
   endif
   if (form=='u' .or. form=='U') then
      ft = 'unformatted'
   else
      ft = 'formatted  '
   end if
   open (unit=iun,file=locfn,status='unknown',form=ft,iostat=ioe)
   if (ioe /= 0) then
      write(6,*)'(OPNFIL): failed to open file ',trim(locfn),        &
     &     ' on unit ',iun,' ierr=',ioe
      call endrun
   else
      write(6,*)'(OPNFIL): Successfully opened file ',trim(locfn),   &
     &     ' on unit= ',iun
   end if
 
   return
   end subroutine opnfil
 
!=======================================================================

  integer function getavu()

!----------------------------------------------------------------------- 
! 
! Purpose: 
! get next available Fortran unit number
!
! Method: 
! Get next available Fortran unit number itst. Set lsmiou(itst), in 
! lsmio common block, true. If coupled to CAM, use CAM function navu
! to get available unit number, in which case lsmiou is not needed.
! 
! Author: Gordon Bonan
! 
!-----------------------------------------------------------------------


    use units     !CAM units module


! ------------------------ local variables ------------------------
    integer itst  !Fortran unit number
! -----------------------------------------------------------------


    getavu = getunit()
    RETURN
# 324 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/lnd/clm2/src/main/fileutils.F90"
  end function getavu

!=======================================================================

  subroutine relavu (iunit)

!----------------------------------------------------------------------- 
! 
! Purpose: 
! close and release Fortran unit no longer in use
!
! Method: 
! Close and release Fortran unit number iunit. Set lsmiou(iunit) to 
! false. If coupled to cam, use cam function relunit to close/release 
! unit number.
! 
! Author: Gordon Bonan
! 
!-----------------------------------------------------------------------


    use units     !CAM units module


! ------------------------ arguments ------------------------------
    integer, intent(in) :: iunit    !Fortran unit number
! -----------------------------------------------------------------


    close(iunit)
    call freeunit(iunit)
# 367 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/lnd/clm2/src/main/fileutils.F90"
    return
  end subroutine relavu

!=======================================================================

   subroutine shell_cmd(text, ier)
 
! ------------------------ arguments -----------------------------------
   character(len=*), intent(in) :: text
   integer         , intent(out):: ier
! ----------------------------------------------------------------------
! ------------------------ local variables -----------------------------





   integer, external :: system ! System routine, execute shell command

! ----------------------------------------------------------------------
 







   ier = system(trim(text))


   return
   end subroutine shell_cmd

!=======================================================================

end module fileutils
