Module reportError
  ! Used for output of read/write errors
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: IOSTAT_END
  implicit none
  character(len=20) :: routineName
  character(len=255) :: err_message, sys_message
  integer :: ioErr

contains

  subroutine reportErr
    print *, 'Fatal Error in routine ', trim(routineName)
    if (ioErr == IOSTAT_END) print *, 'Tried to read past end of file'
    ! print message if not empty
    if (len_trim(err_message) > 1) print *, trim(err_message)
    print *, 'Sys Err Code: ', ioErr
    print *, 'Sys Message: ', trim(sys_message)
    pause 'Press Enter to terminate.'
    stop
  end subroutine reportErr

end module reportError
