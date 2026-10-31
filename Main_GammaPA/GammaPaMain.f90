! *******************************************************

! gammapa_main.f90
!
! main entry point for GAMMA_PA application
!
!*********************************************************
!
!*********************************************************
! Option codes kop() set by the association parameter input file
!
! Debug options for printing to debug file.
! kop(1) == 0 no debug printing
! kop(1) == 1 for debug printing of only gammas and code version
! kop(1) == 2 adds printing of X, rho, deltas, etc.
! kop(1) == 3 adds printing of delta matrix indexes, and gamma, Hxs, volume output to csv files.
!
! rdf equation to use. The calculation uses packing factor based on bvol for CPA, ESD, VDW
! kop(2) == 0 VdW
! kop(2) == 1 ESD
! kop(2) == 2 CPA
! kop(2) == 3 PCSAFT
! kop(2) == 4 (g(sigma) = 1)
!
! Liquid volume
! kop(3) == 0 use standard liquid volume
! kop(3) == 1 use temperature-dependent volume
!
! attractive model
! kop(4) == 0 att term: NRTL. Parameters should be entered as GMNRTL.
! kop(4) == 1 att term: Wilson. Parameters should be entered as GMUW.
! kop(4) == 2 att term: Scatchard Hildebrand. Parameters should be entered as GMUSH.
! kop(4) == 3 att term: Nagata1. Parameters should be entered as GMUNAG.
!
! combinatorial correction (ignored when kop(4) = 1 (Wilson), kop(6) == 2 (no comb term))
! kop(5) == 0 combinatorial correction term: vdW with bBondi (calculated by uniquac r*15.17)
! kop(5) == 1 combinatorial correction term: None
! kop(5) == 2 combinatorial correction term: Staverman-Guggenheim
! kop(5) == 3 combinatorial correction term: vdW with the value of bvol
!
! combinatorial term (ignored for kop(4) == 1 (Wilson))
! kop(6) == 0 combinatorial term: Flory with normal volume
! kop(6) == 1 combinatorial term: Flory term with modified volume (v^2/3)
! kop(6) == 2 combinatorial term: None
!
! In addition, the variable Kcalc determines if temperature derivative is calculated.
!         1 - calculate only gammas
!         2 - calculate only gamma derivative, d(ln gamma)/dT
!        3 - calculate both gamma and gamma derivative
!         The temperature derivative is determined by finite differences in SUBROUTINE GAMMA_PA.
!

! **************************************************

PROGRAM GAMMAPA_MAIN

  USE IFPORT
  use GPA_CONSTANTS
! GPA_CONSTANTS includes the key file unit numbers
  use GPA_SITENSPECIES
  use GPA_VOLUMES
  USE GPA_PHYS_PARMS
  USE reportError
  IMPLICIT NONE

! Please update the version when making more than minor fixes
! Append RTPT or TPT1 so that debug code shows the option used for compiled code.
  character(len=10) :: ver = '0.3TPT1'

! todo - track sub and Henry's law components
! INTEGER NSUB,  NSUP

  INTEGER iErr
  real(real64) Tkelvin, Pbar ! used within a loop
  real(real64), dimension(:), allocatable :: gamma, dgamma

  integer i, j, start_index, ncases
  character(len=40) inputFileName, assocFileName, physFileName
  character(len=255) :: data_line, data_field, dumString

  ! DEBUG OUTPUT IS CONTROLLED BY KOP(1), NOT THE LOGICAL VARIABLE 'LOUD' OR 'LOUDER'
  ! SEE THE DESCRIPTION OF KOP(1) ABOVE

  ! used for read/write errors
  iErr = 0
  routineName = 'GammaPA_Main'
  sys_message = ''
  err_message = ''

  inputFileName = ''
  assocFileName = ''
  physFileName = ''

! for physical and combinatorial models
! aparam, bparam for NRTL, Nagata, Wilson, tau for NRTL, Aij for SH

  print *, 'Welcome to the GAMMAPA program.'
  print *, ' '
  GPAdir = '.' ! Set to MDNAproject folder within Cmake. Full path is built using GETDRIVEDIRQQ
  ! Expand to full path
  ioErr = GETDRIVEDIRQQ(GPAdir) ! returns the length of the path, but 0 if the path is too long
  if (ioErr .lt. 1) then
    print *, 'Path is too long. Error, file = ', 'I/O Error', iErr, ' ', TRIM(GPAdir)
    pause 'GammaPaMain: errors at program startup. Press Enter to terminate.'
    stop
  else
    print *, 'Project path set successfully:', TRIM(GPAdir)
  end if

  ! Prepare for output
  print *, "Preparing output and debug files."
  dumString = TRIM(GPAdir)//"\Output\GAMMAPAout.txt"
  OPEN (outfile, iostat=ioErr, iomsg=sys_message, file=TRIM(dumString), RECL=1500, STATUS='UNKNOWN')
  if (ioErr .ne. 0) then
    err_message = 'Bad output file: '//TRIM(dumString)
    call reportErr
  else
    print *, 'Output File opened successfully:', TRIM(dumString)
  end if

  dumString = TRIM(GPAdir)//"\Output\GAMMAPAdebug.txt"
  OPEN (debugfile, iostat=ioErr, iomsg=sys_message, file=TRIM(dumString), RECL=1500, STATUS='UNKNOWN')
  if (ioErr .ne. 0) then
    err_message = 'Bad debug file: '//TRIM(dumString)
    call reportErr
  else
    print *, 'Debug File opened successfully:', TRIM(dumString)
  end if

  ! Open the main input file for reading
  print *, 'Enter the main input file name. File should be tab delimited.'
  read (*, '(A)') inputFileName
  dumString = TRIM(GPAdir)//"\Input\GammaPa\"//trim(inputFileName)

  WRITE (outfile, '(A)', iostat=ioErr, iomsg=sys_message) 'InputFileName: '//TRIM(dumString)
  if (ioErr /= 0) then
    err_message = 'Unable to write to output file. File = '//TRIM(GPAdir)//'Output/GAMMAPAout.txt'
    call reportErr
  end if
  OPEN (inputFile, iostat=ioErr, file=TRIM(dumString), iomsg=sys_message, STATUS='OLD')
  if (ioErr .ne. 0) then
    print *, 'Bad Main Input File. Check for typos or placement in wrong folder. File= '//TRIM(dumString)
    call reportErr
  else
    print *, 'Main Input File opened successfully:', TRIM(dumString)
  end if

  WRITE (outfile, '(A,A10)', iostat=ioErr, iomsg=sys_message) 'GAMMAPA version ', ver
  if (ioErr /= 0) then
    err_message = 'Unable to write to output file. File = '//TRIM(GPAdir)//'Output/GAMMAPAout.txt'
    call reportErr
  end if

  ! Read number of species
  start_index = 1
  read (UNIT=inputFile, FMT='(A)', iomsg=sys_message, ioStat=ioErr) data_line
  if (ioErr /= 0) then
    err_message = 'Error reading number of species line from input file.'
    call reportErr
  end if
  WRITE (outfile, '(A)') trim(data_line)
  call parse_line(start_index, data_line, data_field)
  read (data_field, *, iomsg=sys_message, ioStat=ioErr) nc

  ! allocate storage for components
  if (.not. allocated(comp)) allocate (comp(nc))
  if (.not. allocated(bvol)) allocate (bvol(nc))
  if (.not. allocated(r_uniquac)) allocate (r_uniquac(nc))
  if (.not. allocated(q_uniquac)) allocate (q_uniquac(nc))
  if (.not. allocated(vstd)) allocate (vstd(nc))
  if (.not. allocated(PCSAFT_m)) allocate (PCSAFT_m(nc))
  if (.not. allocated(PCSAFT_sigma)) allocate (PCSAFT_sigma(nc))
  if (.not. allocated(PCSAFT_epsok)) allocate (PCSAFT_epsok(nc))
  if (.not. allocated(veq)) allocate (veq(nc))
  if (.not. allocated(vparms)) allocate (vparms(7, nc))
  IF (.NOT. ALLOCATED(x)) allocate (x(nc))
  IF (.NOT. ALLOCATED(gamma)) allocate (gamma(nc))
  IF (.NOT. ALLOCATED(dgamma)) allocate (dgamma(nc))
  IF (.NOT. ALLOCATED(aparam)) allocate (aparam(nc, nc))
  IF (.NOT. ALLOCATED(bparam)) allocate (bparam(nc, nc))
  IF (.NOT. ALLOCATED(alpha)) allocate (alpha(nc, nc))
  IF (.NOT. ALLOCATED(Aij)) allocate (Aij(nc, nc))
  x = 0D0
  aparam = 0D0
  bparam = 0D0
  alpha = 0D0
  Aij = 0D0

  ! Read association filename
  read (inputFile, '(A)', iomsg=sys_message, ioStat=ioErr) data_line
  if (ioErr /= 0) then
    err_message = 'Error reading number of association filename line from input file.'
    call reportErr
  end if
  WRITE (outfile, '(A)') trim(data_line)
  start_index = 1
  call parse_line(start_index, data_line, data_field)
  assocFileName = TRIM(data_field)
  print *, 'Full Association Parameter file name:', TRIM(GPAdir)//"\Input\GammaPa\"//TRIM(assocFileName)

  ! Read Phys parameter filename
  read (inputFile, '(A)', iomsg=sys_message, ioStat=ioErr) data_line
  if (ioErr /= 0) then
    err_message = 'Error reading NRTL filename line from input file.'
    call reportErr
  end if
  WRITE (outfile, '(A)') trim(data_line)
  start_index = 1
  call parse_line(start_index, data_line, data_field)
  physFileName = data_field
  print *, 'Full Phys file name:', TRIM(GPAdir)//"\Input\GammaPa\"//TRIM(physFileName)

  ! Read KOP header lines
  read (inputFile, '(A)', iomsg=sys_message, ioStat=ioErr) data_line
  if (ioErr /= 0) then
    err_message = 'Error KOP header from input file.'
    call reportErr
  end if
  WRITE (outfile, '(A)') trim(data_line)
  read (inputFile, '(A)', iomsg=sys_message, ioStat=ioErr) data_line
  if (ioErr /= 0) then
    err_message = 'Error KOP header from input file.'
    call reportErr
  end if
  WRITE (outfile, '(A)') trim(data_line)

  ! Read KOP
  read (inputFile, '(A)', iomsg=sys_message, ioStat=ioErr) data_line
  if (ioErr /= 0) then
    err_message = 'Error reading KOP from input file.'
    call reportErr
  end if
  WRITE (outfile, '(A)') trim(data_line)

  start_index = 1
  do i = 1, 10
    call parse_line(start_index, data_line, data_field)
    read (data_field, *) kop(i)
  end do

  Call loadSpecies(iErr)

  ! Match sites with species and load site information into species
  ! Load association strength values.
  call loadAssoc(iErr, assocFileName)

! for physical and combinatorial models
! aparam, bparam for NRTL, Nagata, Wilson, tau for NRTL, Aij for SH

  select case (kop(4))
  case (0)
    call loadnrtl(iErr, physFileName)
    if (kop(1) .gt. 1) then
      write (debugfile, *) ' *** GMNRTL Physical aij'
      write (debugfile, '(6G12.5)') aparam
      write (debugfile, *) ' *** GMNRTL Physical bij'
      write (debugfile, '(6G12.5)') bparam
      write (debugfile, *) ' *** GMNRTL Physical alpha'
      write (debugfile, '(6G12.5)') alpha
    end if

  case (1) ! wilson
    err_message = 'Wilson is not yet adapted to the stand-alone version. Sorry.'
    sys_message = ''
    ioErr = 0
    call reportErr
    call loadwilson(iErr, physFileName)
    if (kop(1) .gt. 1) then
      write (debugfile, *) ' *** GMUwilson Physical aij'
      write (debugfile, '(6G12.5)') aparam
      write (debugfile, *) ' *** GMUwilson Physical bij'
      write (debugfile, '(6G12.5)') bparam
    end if

  case (2) !scatchard hildebrand (eqn Elliott/Lira 12.24,12.25)
    err_message = 'Scatchard-Hildebrand is not yet adapted to the stand-alone version. Sorry.'
    sys_message = ''
    ioErr = 0
    call reportErr
    call loadsh(iErr, physFileName)
    if (kop(1) .gt. 1) then
      write (debugfile, *) ' *** GMUscathild Physical aij'
      write (debugfile, '(6G12.5)') aparam
      write (debugfile, *) ' *** GMUscathild Physical bij'
      write (debugfile, '(6G12.5)') bparam
    end if

  case (3) ! Nagata1
    err_message = 'Nagata is not yet adapted to the stand-alone version. Sorry.'
    sys_message = ''
    ioErr = 0
    call reportErr
    call loadnagata(iErr, physFileName)
    if (kop(1) .gt. 1) then
      write (debugfile, *) ' *** GMUnagata Physical aij'
      write (debugfile, '(6G12.5)') aparam
      write (debugfile, *) ' *** GMUnagata Physical bij'
      write (debugfile, '(6G12.5)') bparam
    end if

  end select

  if (kop(2) .EQ. 3) then
    if ((kop(1) .gt. 1) .and. (mod(kcalc, 2) .gt. 0)) then
    do i = 1, nc
      write (debugfile, '(I4, 3G12.5)') (i), PCSAFT_m(i), PCSAFT_sigma(i), PCSAFT_epsok(i)
    end do
    end if
  END IF

  ! Read number of States to evaluate
  read (inputFile, '(A)', ioStat=ioErr, iomsg=sys_message) data_line
  if (ioErr /= 0) then
    err_message = 'Error reading number of states header from input file.'
    call reportErr
  end if
  WRITE (outfile, '(A)') trim(data_line)
  start_index = 1
  call parse_line(start_index, data_line, data_field)
  read (data_field, *) ncases

  read (inputFile, '(A)', ioStat=ioErr, iomsg=sys_message) data_line
  if (ioErr /= 0) then
    err_message = 'Error reading header from input file.'
    call reportErr
  end if
  write (outfile, '(A)') 'T'//achar(9)//'P'//achar(9)//'x'//achar(9)//'lngamma'//achar(9)//'gamma'//achar(9)//'Hex'
  do i = 1, ncases
    read (inputFile, '(A)', ioStat=ioErr, iomsg=sys_message) data_line
    if (ioErr /= 0) then
      err_message = 'Error reading header from input file.'
      call reportErr
    end if
    start_index = 1
    call parse_line(start_index, data_line, data_field)
    read (data_field, *) KCALC
    call parse_line(start_index, data_line, data_field)
    read (data_field, *) Tkelvin
    call parse_line(start_index, data_line, data_field)
    read (data_field, *) Pbar
    do j = 1, nc
      call parse_line(start_index, data_line, data_field)
      read (data_field, *) x(j)
    end do
    x = x/sum(x)

    call gamma_pa(TKelvin, Pbar, gamma, dgamma, iErr)
    if (nc < 33) then
      ! build format specifier for a single line
      j = 3 + 3*nc
      write (data_field, '(A, I0, A)') '(', j, 'G15.6)'
    else
      data_field = '(100G15.5)'
    end if
    write (outfile, trim(data_field)) TKelvin, Pbar, X(:), gamma(:), dexp(gamma(:)), -Rgas*TKelvin**2*(dot_product(x, dgamma))
  end do

  close (outfile)
  close (debugfile)
  close (inputFile)

  print *, 'Program has ended normally. The output is in Output/GAMMAPAout.txt.'
  print *, 'Press any key to close this window.'
  pause
  if (allocated(gamma)) deallocate (gamma)
  if (allocated(dgamma)) deallocate (dgamma)
  if (allocated(aparam)) deallocate (aparam)
  if (allocated(bparam)) deallocate (bparam)
  if (allocated(alpha)) deallocate (alpha)
  if (allocated(Aij)) deallocate (Aij)
  if (allocated(site)) deallocate (site)
  if (allocated(comp)) deallocate (comp)
  if (allocated(x)) deallocate (x)
  if (allocated(KAD)) deallocate (KAD)
  if (allocated(eps)) deallocate (eps)
  if (allocated(PCSAFT_sigma)) deallocate (PCSAFT_sigma)
  if (allocated(PCSAFT_m)) deallocate (PCSAFT_m)
  if (allocated(PCSAFT_epsok)) deallocate (PCSAFT_epsok)
  if (allocated(veq)) deallocate (veq)
  if (allocated(bvol)) deallocate (bvol)
  if (allocated(r_uniquac)) deallocate (r_uniquac)
  if (allocated(q_uniquac)) deallocate (q_uniquac)
  if (allocated(vstd)) deallocate (vstd)
  if (allocated(vparms)) deallocate (vparms)

END PROGRAM GAMMAPA_MAIN
