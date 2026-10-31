Subroutine loadSpecies(iErr)
  ! todo - implement iErr properly
  ! This subroutine reads the species information from the input file and populates the speciesList array.
  ! It also returns the number of species read and an error code if any issues occur during reading.
  USE GPA_CONSTANTS
  USE GPA_SITENSPECIES
  use GPA_VOLUMES
  USE GPA_PHYS_PARMS
  USE reportError
  IMPLICIT NONE
  ! iErr tracks multiple error codes, ioErr is the I/O error code for file operations
  integer, intent(out) :: iErr
  integer :: i, j, start_index
  character(len=255) :: data_line, data_field
  logical :: LOUDER = .TRUE., FOUND
  ! variables for species file
  integer :: n_File
  character(len=20), allocatable, dimension(:) :: name_db
  integer, allocatable, dimension(:) :: idTRC, idDIPPR, ver, veq_db, ts_db
  real(real64), allocatable, dimension(:) :: bVDW, bESD, bCPA, m, sigma, epsok, r_uq_db, q_uq_db, vstd_db
  real(real64), allocatable, dimension(:, :) :: vparms_db

  ! Initialize error code
  iErr = 0
  routineName = 'loadSpecies'
  err_message = ''
  sys_message = ''

  ! Read Species Header line
  read (inputFile, '(A)', iomsg=sys_message, ioStat=ioErr) data_line
  if (ioErr /= 0) then
    err_message = 'Error reading Species header from input file.'
    call reportErr
  end if
  WRITE (outfile, '(A)') trim(data_line)
  do i = 1, nc
    ! Read species information and number/type of groups for each species
    read (inputFile, '(A)', iomsg=sys_message, ioStat=ioErr) data_line
    if (ioErr /= 0) then
      err_message = 'Error reading Species information from input file.'
      call reportErr
    end if
    WRITE (outfile, '(A)') trim(data_line)
    start_index = 1
    call parse_line(start_index, data_line, data_field)
    read (data_field, *) comp(i)%id
    call parse_line(start_index, data_line, data_field)
    read (data_field, *) comp(i)%ver
    call parse_line(start_index, data_line, data_field)
    read (data_field, *) comp(i)%ngroupTypes
    do j = 1, comp(i)%ngroupTypes
      call parse_line(start_index, data_line, data_field)
      read (data_field, *) comp(i)%groupid(j)
      call parse_line(start_index, data_line, data_field)
      read (data_field, *) comp(i)%ngroup(j)
    end do
  end do

  ! temporary code to check the species info loaded so far
  ! call species_dump(comp)

  ! Open species file and load species paramters for specified version
  ! specFile is set in module GPA_CONSTANTS
  data_field = trim(GPAdir)//"\input\GAMMAPA\GPA-libSpecies.txt"
  open (specFile, FILE=trim(data_field), iomsg=sys_message, iostat=ioErr, STATUS='OLD')
  if (ioErr .ne. 0) then
    err_message = 'Bad Species file. file = '//TRIM(data_field)
    call reportErr
  else
    print *, 'Species File opened successfully:', TRIM(data_field)
  end if
  read (specFile, '(A)', iomsg=sys_message, ioStat=ioErr) data_line
  if (ioErr /= 0) then
    err_message = 'Error reading Species information from input file.'
    call reportErr
  end if
  start_index = 1
  call parse_line(start_index, data_line, data_field)
  read (data_field, *) n_File

  ! two header lines
  read (specFile, '(A)', ioStat=ioErr) data_line
  if (ioErr /= 0) then
    print *, 'Error reading Species information from input file, Error code: ', ioErr
    pause 'gpa_getSpecies: errors at program startup.'
    stop
  end if
  read (specFile, '(A)', ioStat=ioErr) data_line
  if (ioErr == IOSTAT_END) print *, 'Tried to read past end of file'
  if (ioErr /= 0) then
    print *, 'Error reading Species information from input file, Error code: ', ioErr
    pause 'gpa_getSpecies: errors at program startup.'
    stop
  end if

  ! allocate storage
  if (.not. allocated(idTRC)) allocate (idTRC(n_File))
  if (.not. allocated(idDIPPR)) allocate (idDIPPR(n_File))
  if (.not. allocated(ver)) allocate (ver(n_File))
  if (.not. allocated(name_db)) allocate (name_db(n_File))
  if (.not. allocated(bVDW)) allocate (bVDW(n_File))
  if (.not. allocated(bESD)) allocate (bESD(n_File))
  if (.not. allocated(bCPA)) allocate (bCPA(n_File))
  if (.not. allocated(m)) allocate (m(n_File))
  if (.not. allocated(sigma)) allocate (sigma(n_File))
  if (.not. allocated(epsok)) allocate (epsok(n_File))
  if (.not. allocated(ts_db)) allocate (ts_db(n_File))

  do i = 1, n_File
    read (specFile, '(A)', iomsg=sys_message, ioStat=ioErr) data_line
    if (ioErr /= 0) then
      write (err_message, "(A, I4)") 'Error reading Species information from input file, species ', i
      call reportErr
    end if
    start_index = 1
    call parse_line(start_index, data_line, data_field)
    read (data_field, *) idTRC(i)
    if (i .gt. 1) then
      if (idTRC(i) .lt. idTRC(i - 1)) then
        print *, 'Error. Species database must be sorted with idTRC increasing.'
        pause 'gpa_getSpecies: error at program startup'
        stop
      end if
    end if
    call parse_line(start_index, data_line, data_field)
    read (data_field, *) idDIPPR(i)
    call parse_line(start_index, data_line, data_field)
    read (data_field, *) ver(i)
    call parse_line(start_index, data_line, data_field)
    name_db(i) = trim(data_field)
    call parse_line(start_index, data_line, data_field)
    read (data_field, *) bVDW(i)
    call parse_line(start_index, data_line, data_field)
    read (data_field, *) bESD(i)
    call parse_line(start_index, data_line, data_field)
    read (data_field, *) bCPA(i)
    call parse_line(start_index, data_line, data_field)
    read (data_field, *) m(i)
    call parse_line(start_index, data_line, data_field)
    read (data_field, *) sigma(i)
    call parse_line(start_index, data_line, data_field)
    read (data_field, *) epsok(i)
    call parse_line(start_index, data_line, data_field)
    read (data_field, *) ts_db(i)
  end do
  close (specFile)

! database is now loaded, assign to species in case
  write (outfile, '(*(G0))') 'idTRC', achar(9), 'name', achar(9), 'ver', achar(9), 'bvol', achar(9), &
    'm', achar(9), 'sigma', achar(9), 'eps/k', achar(9), 'ts'
  do i = 1, nc
    FOUND = .FALSE.
    do j = 1, n_File
      if (idTRC(j) > comp(i)%id) exit ! Species is not found in properly sorted database.
      if ((idTRC(j) == comp(i)%id) .AND. (comp(i)%ver == ver(j))) then
        FOUND = .TRUE.
        comp(i)%name = name_db(j)
        comp(i)%ts = ts_db(j)
        PCSAFT_m(i) = m(j)
        PCSAFT_sigma(i) = sigma(j)
        PCSAFT_epsok(i) = epsok(j)
        SELECT CASE (KOP(2))
          ! note bBondi is set later
        CASE (0)
          bvol(i) = bVDW(j)
        CASE (1)
          bvol(i) = bESD(j)
        CASE (2)
          bvol(i) = bCPA(j)
        CASE (3) ! use placeholder since PCSAFT will be used
          bvol(i) = 0
        CASE (4) ! g(sigma) = 1 can be calculated uisng bvol = 0
          bvol(i) = 0
        CASE DEFAULT
          write (err_message, '(A,I3)') 'Error, kop(2) value out of range: ', kop(2)
          sys_message = ''
          ioErr = 0
          call reportErr
        END SELECT
      end if
    end do ! 1-> n_File
    if (.not. FOUND) then
      print *, 'Unable to find component', comp(i)%id, ' in species database.'
      print *, 'Database must be sorted by increasing idTRC.'
      pause 'gpa_getSpecies: error at program startup'
      stop
    else
      write (outfile, '(*(G0))') comp(i)%id, achar(9), trim(comp(i)%name), achar(9), comp(i)%ver, achar(9), bvol(i), &
        achar(9), PCSAFT_m(i), achar(9), PCSAFT_sigma(i), achar(9), PCSAFT_epsok(i), achar(9), comp(i)%ts
    end if
  end do ! 1-> nc

  ! deallocate species database storage
  if (allocated(idTRC)) deallocate (idTRC)
  if (allocated(idDIPPR)) deallocate (idDIPPR)
  if (allocated(ver)) deallocate (ver)
  if (allocated(name_db)) deallocate (name_db)
  if (allocated(bVDW)) deallocate (bVDW)
  if (allocated(bESD)) deallocate (bESD)
  if (allocated(bCPA)) deallocate (bCPA)
  if (allocated(m)) deallocate (m)
  if (allocated(sigma)) deallocate (sigma)
  if (allocated(epsok)) deallocate (epsok)
  if (allocated(ts_db)) deallocate (ts_db)

  ! open volume database file
  ! volFile is set in module GPA_CONSTANTS
  data_field = trim(GPAdir)//"\input\GAMMAPA\GPA-libVuq.txt"
  open (volFile, FILE=trim(data_field), iomsg=sys_message, iostat=ioErr, STATUS='OLD')
  if (ioErr .ne. 0) then
    write (err_message, '(A)') 'Bad Volume file '//TRIM(data_field)
    call reportErr
  else
    print *, 'Volume File opened successfully:', TRIM(data_field)
  end if
  read (volFile, '(A)', iomsg=sys_message, ioStat=ioErr) data_line
  if (ioErr /= 0) then
    err_message = 'Error reading Volume information from input file.'
    call reportErr
  end if
  start_index = 1
  call parse_line(start_index, data_line, data_field)
  read (data_field, *) n_File

  ! one header lines
  read (volFile, '(A)', iomsg=sys_message, ioStat=ioErr) data_line
  if (ioErr /= 0) then
    err_message = 'Error reading Volume information from input file.'
    call reportErr
  end if

  ! load volume paramters
  if (.not. allocated(idTRC)) allocate (idTRC(n_File))
  if (.not. allocated(idDIPPR)) allocate (idDIPPR(n_File))
  if (.not. allocated(vstd_db)) allocate (vstd_db(n_File))
  if (.not. allocated(r_uq_db)) allocate (r_uq_db(n_File))
  if (.not. allocated(q_uq_db)) allocate (q_uq_db(n_File))
  if (.not. allocated(veq_db)) allocate (veq_db(n_File))
  if (.not. allocated(vparms_db)) allocate (vparms_db(7, n_File))

  do i = 1, n_File
    read (volFile, '(A)', iomsg=sys_message, ioStat=ioErr) data_line
    if (ioErr /= 0) then
      err_message = 'Error reading Volume information from input file.'
      call reportErr
    end if
    start_index = 1
    call parse_line(start_index, data_line, data_field)
    read (data_field, *) idTRC(i)
    call parse_line(start_index, data_line, data_field)
    read (data_field, *) idDIPPR(i)
    ! skip name. In the future, it may be good to compare it to confirm it matches id
    call parse_line(start_index, data_line, data_field)
    call parse_line(start_index, data_line, data_field)
    read (data_field, *) vstd_db(i)
    call parse_line(start_index, data_line, data_field)
    read (data_field, *) r_uq_db(i)
    call parse_line(start_index, data_line, data_field)
    read (data_field, *) q_uq_db(i)
    call parse_line(start_index, data_line, data_field)
    read (data_field, *) veq_db(i)
    do j = 1, 7
      call parse_line(start_index, data_line, data_field)
      read (data_field, *) vparms_db(j, i)
    end do

  end do

! database is now loaded, assign to volumes in case
  write (outfile, '(*(G0))') 'idTRC', achar(9), 'vSTD', &
    achar(9), 'R', achar(9), 'Q', achar(9), 'Veq', achar(9), 'vparms 1-7'
  do i = 1, nc
    FOUND = .FALSE.
    do j = 1, n_File
      if (idTRC(j) > comp(i)%id) exit ! Species is not found in properly sorted database.
      if (idTRC(j) == comp(i)%id) then
        FOUND = .TRUE.
        vstd(i) = vstd_db(j)
        r_uniquac(i) = r_uq_db(j)
        q_uniquac(i) = q_uq_db(j)
        veq(i) = veq_db(j)
        vparms(:, i) = vparms_db(:, j)
      end if
    end do ! 1-> n_File
    if (.not. FOUND) then
      print *, 'Unable to find component', comp(i)%id, ' in volume database.'
      print *, 'Database must be sorted by increasing idTRC.'
      pause 'gpa_getSpecies: error at program startup'
      stop
    else
      write (outfile, '(*(G0))') comp(i)%id, achar(9), vstd(i), &
        achar(9), r_uniquac(i), achar(9), q_uniquac(i), achar(9), veq(i), &
        achar(9), vparms(1, i), achar(9), vparms(2, i), achar(9), vparms(3, i), &
        achar(9), vparms(4, i), achar(9), vparms(5, i), achar(9), vparms(6, i), achar(9), vparms(7, i)
    end if
  end do ! 1-> nc

  close (volFile)

  if (allocated(idTRC)) deallocate (idTRC)
  if (allocated(idDIPPR)) deallocate (idDIPPR)
  if (allocated(vstd_db)) deallocate (vstd_db)
  if (allocated(r_uq_db)) deallocate (r_uq_db)
  if (allocated(q_uq_db)) deallocate (q_uq_db)
  if (allocated(veq_db)) deallocate (veq_db)
  if (allocated(vparms_db)) deallocate (vparms_db)

  ! temporary code to check the species info loaded so far
  ! call species_dump(comp)

end subroutine loadSpecies
