!**********************************************
!
!  Subroutine loadnrtl - loads NRTL parameters
!
!**********************************************

SUBROUTINE LOADNRTL(iErr, physFileName)

  use GPA_CONSTANTS, Only: GPAdir, outfile, physFile, REAL64
  USE GPA_SITENSPECIES, ONLY: nc, comp
  USE GPA_PHYS_PARMS, ONLY: aparam, bparam, alpha
  USE reportError

  implicit none
  character(len=40), intent(in) :: physFileName

  integer :: start_index
  ! integer :: index1, index2, compid1, compid2

! local variables
  integer :: i, j, k, l, temp_id, iErr, n_spairs, n_db, lastrow
  character*255 :: data_line
  character*100 :: data_field
  integer, allocatable, dimension(:) :: id1_db, id2_db, ts_db, indx_sorted
  integer, allocatable, dimension(:, :) :: specPairs
  real(real64), allocatable, dimension(:) :: aparam12_db, aparam21_db, bparam12_db, bparam21_db, alpha_db
  logical :: FOUND

  routineName = 'loadNRTL'
  iErr = 0
  n_spairs = nc*(nc - 1)/2
  if (.not. allocated(specPairs)) allocate (specPairs(n_spairs, 3))
  specPairs = 0

! Prepare for input
  data_line = TRIM(GPAdir)//"\Input\GAMMAPA\"//physFileName
  OPEN (physfile, ioStat=ioErr, iomsg=sys_message, file=data_line, status='OLD')
  if (ioErr .ne. 0) then
    err_message = 'Could not open phys file '//trim(data_line)
    call reportErr
  else
    print *, 'Phys File opened successfully: '//trim(data_line)
  end if

  WRITE (outfile, '(A)') 'NRTL file: '//trim(physFileName)

  start_index = 1
  READ (physFile, '(A)', ioStat=ioErr, iomsg=sys_message) data_line
  if (ioErr .ne. 0) then
    err_message = 'Could not read phys file.'
    call reportErr
  end if
  if (ioErr .ne. 0) then
    err_message = 'Could not read phys file.'
    call reportErr
  end if
  call parse_line(start_index, data_line, data_field)
  read (data_field, *) n_db
  READ (physFile, '(A)') data_line ! Header
  if (ioErr .ne. 0) then
    err_message = 'Could not read phys file.'
    call reportErr
  end if
  READ (physFile, '(A)', ioStat=ioErr, iomsg=sys_message) data_line
  if (ioErr .ne. 0) then
    err_message = 'Could not read phys file.'
    call reportErr
  end if
  WRITE (outFile, '(A)') trim(data_line)

  if (.not. allocated(id1_db)) allocate (id1_db(n_db))
  if (.not. allocated(id2_db)) allocate (id2_db(n_db))
  if (.not. allocated(aparam12_db)) allocate (aparam12_db(n_db))
  if (.not. allocated(aparam21_db)) allocate (aparam21_db(n_db))
  if (.not. allocated(bparam12_db)) allocate (bparam12_db(n_db))
  if (.not. allocated(bparam21_db)) allocate (bparam21_db(n_db))
  if (.not. allocated(alpha_db)) allocate (alpha_db(n_db))
  if (.not. allocated(ts_db)) allocate (ts_db(n_db))

  DO i = 1, n_db
    start_index = 1
    READ (physFile, '(A)', ioStat=ioErr, iomsg=sys_message) data_line
    if (ioErr .ne. 0) then
      err_message = 'Could not read phys file.'
      call reportErr
    end if
    call parse_line(start_index, data_line, data_field) !id1
    read (data_field, *) id1_db(i)
    call parse_line(start_index, data_line, data_field) !id2
    read (data_field, *) id2_db(i)
    if (id1_db(i) > id2_db(i)) then
      err_message = 'Id1 must be less than id2 in physical paramter file.'
      sys_message = ''
      ioErr = 0
      call reportErr
    end if
    if (i > 2) then
      if (id1_db(i) < id1_db(i - 1)) then
        err_message = 'Id1 entries should be sorted in ascending order.'
        sys_message = ''
        ioErr = 0
        call reportErr
      end if
    end if
    call parse_line(start_index, data_line, data_field) !a12
    read (data_field, *) aparam12_db(i)
    call parse_line(start_index, data_line, data_field) !a21
    read (data_field, *) aparam21_db(i)
    call parse_line(start_index, data_line, data_field) !b12
    read (data_field, *) bparam12_db(i)
    call parse_line(start_index, data_line, data_field) !b21
    read (data_field, *) bparam21_db(i)
    call parse_line(start_index, data_line, data_field) !alpha
    read (data_field, *) alpha_db(i)
    call parse_line(start_index, data_line, data_field) !alpha
    read (data_field, *) ts_db(i)
  END DO

  ! Build list of species pairs to be found
  ! This is generic and should be moved to MainGammaPA for any physical model
  ! Load vector with indices for species in ascending order
  if (.not. allocated(indx_sorted)) allocate (indx_sorted(nc))
  ! initialize as 1,2,3,...
  do i = 1, nc
    indx_sorted(i) = i
  end do
  ! sort
  do i = 2, nc
    temp_id = indx_sorted(i)
    j = i - 1
    do while (j >= 1)
      if (comp(indx_sorted(j))%id > comp(temp_id)%id) then
        indx_sorted(j + 1) = indx_sorted(j)
        j = j - 1
      else
        exit
      end if
    end do
    indx_sorted(j + 1) = temp_id
  end do

  ! Sort pair combinations so the smallest id is first.
  k = 1
  do i = 1, nc - 1
    do j = i + 1, nc
      specPairs(k, 1:2) = (/comp(indx_sorted(i))%id, comp(indx_sorted(j))%id/)
      k = k + 1
    end do
  end do

  lastrow = 1
  do i = 1, nc - 1
    do j = 1, n_db
      if (comp(indx_sorted(i))%id < id1_db(j)) exit ! exit if all sorted ea sites have been checked
      if (comp(indx_sorted(i))%id == id1_db(j)) then ! check ed sites that mate with ea site i
        do k = i + 1, nc
          if (comp(indx_sorted(k))%id == id2_db(j)) then ! a site pair is found
            aparam(indx_sorted(i), indx_sorted(k)) = aparam12_db(j)
            aparam(indx_sorted(k), indx_sorted(i)) = aparam21_db(j)
            bparam(indx_sorted(i), indx_sorted(k)) = bparam12_db(j)
            bparam(indx_sorted(k), indx_sorted(i)) = bparam21_db(j)
            alpha(indx_sorted(i), indx_sorted(k)) = alpha_db(j)
            alpha(indx_sorted(k), indx_sorted(i)) = alpha_db(j)
            ! mark as found in sitepairs
            do l = 1, n_spairs
              if (all(specpairs(l, 1:2) == (/comp(indx_sorted(i))%id, comp(indx_sorted(k))%id/))) specpairs(l, 3) = 1 ! set to 1 when found
            end do
            write (outfile, '(2(I5,A),4(G15.6,A),F6.4,A,I9)') id1_db(j), achar(9), id2_db(j), achar(9), &
              aparam12_db(j), achar(9), aparam21_db(j), achar(9), bparam12_db(j), achar(9), &
              bparam21_db(j), achar(9), alpha_db(j), achar(9), ts_db(j)
          end if
        end do
      end if
    end do
  end do

  ! check for any pairs that were not found
  FOUND = .TRUE.
  do i = 1, n_spairs
    if (specpairs(i, 3) == 0) then
      write (data_line, '(A, 2(I6))') 'Physical species pair not found for ', specpairs(i, 1:2)
      print *, trim(data_line)
      write (outfile, '(A)', ioStat=ioErr, iomsg=sys_message) data_line
      FOUND = .FALSE.
    end if
  end do

  if (.not. FOUND) then
    err_message = 'Some species physical pairs not found. Terminating.'
    sys_message = ''
    ioErr = 0
    call reportErr
  end if

  CLOSE (physFile)

  if (allocated(id1_db)) deallocate (id1_db)
  if (allocated(id2_db)) deallocate (id2_db)
  if (allocated(aparam12_db)) deallocate (aparam12_db)
  if (allocated(aparam21_db)) deallocate (aparam21_db)
  if (allocated(bparam12_db)) deallocate (bparam12_db)
  if (allocated(bparam21_db)) deallocate (bparam21_db)
  if (allocated(alpha_db)) deallocate (alpha_db)
  if (allocated(ts_db)) deallocate (ts_db)

END SUBROUTINE LOADNRTL
