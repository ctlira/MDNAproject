!****************************************************************************
!
!  Subroutine LOADASSOC
!    read component and site parameters
!     -'site' for each site holds the index to the site name and id, the species host
!          index and host mole fraction, the number of occurences on the host, the 'packed id'
!          holds the ids for the species present, which is the same for this application.
!     - 'comp' for each species holds the mole fraction, the number of sites on the species,
!          the site ids for the sites, a flag to indicate if the species self associates.
!     - 'aspmx' - 0 if a mixture has no self- or cross-association, 1 if association is present.
!
!****************************************************************************

SUBROUTINE LOADASSOC(iErr, assocFileName)

  use GPA_CONSTANTS, only: GPAdir, missing, outfile, debugfile, sitesFile, assocFile, KOP
  use GPA_SITENSPECIES
  use GPA_VOLUMES
  USE reportError

  implicit none

! local variables
  integer ::  i, j, k, l, iErr, nsitepair, start_index
  integer :: n_db = 0, n_ea = 0, n_ed = 0, ts_db
  integer, dimension(10) :: kop_assoc ! kop from association file
  ! for site database
  integer, allocatable, dimension(:) :: grpID_db, n_ea_db, n_ed_db, n_eda_db, ea_siteID_db, ed_siteID_db, eda_siteID_db
  integer, allocatable, dimension(:) :: ea_siteID, ed_siteID
  integer, allocatable, dimension(:, :) :: sitepairs
  character(len=50), allocatable, dimension(:) :: grp_desc_db
  character(len=255) :: data_line
  character(len=100) :: data_field
  character(len=40) :: assocFileName
  real(real64), allocatable, dimension(:) :: KAD_db, epsok_db
  LOGICAL LOUDER, FOUND
  LOUDER = .TRUE.                                                ! JRE
  ! For read/write errors
  routineName = 'LoadAssoc'
  iErr = 0
  sys_message = ''
  err_message = ''

! Prepare for input
  data_line = TRIM(GPAdir)//"\input\GAMMAPA\GPA-libSites.txt"
  OPEN (sitesFile, ioStat=ioErr, iomsg=sys_message, file=trim(data_line), STATUS="OLD")
  if (ioErr .ne. 0) then
    err_message = 'Could not open site library file. File = '//trim(data_line)
    call reportErr
  else
    print *, 'Site library file opened successfully: ', trim(data_line)
  end if
  WRITE (outfile, '(A)') 'Site library file: '//trim(data_line)

  READ (sitesFile, '(A)', ioStat=ioErr, iomsg=sys_message) data_line
  if (ioErr /= 0) then
    err_message = 'Error reading site library.'
    call reportErr
  end if
  start_index = 1
  call parse_line(start_index, data_line, data_field)
  read (data_field, *, iomsg=sys_message, ioStat=ioErr) n_db

  if (.not. allocated(grpID_db)) allocate (grpID_db(n_db))
  if (.not. allocated(n_ea_db)) allocate (n_ea_db(n_db))
  if (.not. allocated(n_ed_db)) allocate (n_ed_db(n_db))
  if (.not. allocated(n_eda_db)) allocate (n_eda_db(n_db))
  if (.not. allocated(ea_siteID_db)) allocate (ea_siteID_db(n_db))
  if (.not. allocated(ed_siteID_db)) allocate (ed_siteID_db(n_db))
  if (.not. allocated(eda_siteID_db)) allocate (eda_siteID_db(n_db))
  if (.not. allocated(grp_desc_db)) allocate (grp_desc_db(n_db))
  n_ea_db = 0
  n_ed_db = 0
  n_eda_db = 0
  ea_siteID_db = 0
  ed_siteID_db = 0
  eda_siteID_db = 0
  grp_desc_db = ''

  ! Read Site Library
  ! Read site library header lines
  do i = 1, 10
    READ (sitesFile, '(A)', ioStat=ioErr, iomsg=sys_message) data_line
    if (ioErr /= 0) then
      err_message = 'Error reading site library.'
      call reportErr
    end if
  end do

  ! Read site library contents
  do i = 1, n_db
    READ (sitesFile, '(A)', ioStat=ioErr, iomsg=sys_message) data_line
    if (ioErr /= 0) then
      err_message = 'Error reading site library.'
      call reportErr
    end if
    start_index = 1
    call parse_line(start_index, data_line, data_field)
    read (data_field, *) grpID_db(i)
    call parse_line(start_index, data_line, data_field)
    read (data_field, *) n_ea_db(i)
    call parse_line(start_index, data_line, data_field)
    read (data_field, *) n_ed_db(i)
    call parse_line(start_index, data_line, data_field)
    read (data_field, *) n_eda_db(i)
    call parse_line(start_index, data_line, data_field)
    if (n_ea_db(i) > 0) read (data_field, *) ea_siteID_db(i)
    call parse_line(start_index, data_line, data_field)
    if (n_ed_db(i) > 0) read (data_field, *) ed_siteID_db(i)
    call parse_line(start_index, data_line, data_field)
    if (n_eda_db(i) > 0) read (data_field, *) eda_siteID_db(i)
    call parse_line(start_index, data_line, data_field)
    grp_desc_db(i) = data_field
  end do
  ! check if not end of file
  READ (sitesFile, '(A)', ioStat=ioErr, iomsg=sys_message) data_line
  if (ioErr /= iostat_end) then
    call parse_line(start_index, data_line, data_field)
    ! check for blank line, which is OK
    if (data_field /= missing) then
      err_message = 'Unread lines remain in sites database. Check input field with number of sites.'
      sys_message = ''
      call reportErr
    end if
  end if

  ! count sites before allocating storage of structure sites
  ! nsites is number os site types, incremented if the same site type is on a different species.
  ! n_ea, n_ed, ea_siteID, ed_siteID are used for determining how many and which
  ! association pairs are needed.
  ! n_ea, n_ed are incremented only if site is unique, both are incremented for eda
  ! ea_siteID, ed_siteID are stored, eda_siteID stored as both and acceptor and donor for counting purposes.
  ! 5*nc should be plenty large for sites. If it is increased, search for 5*nc to increase in loops below.
  if (.not. allocated(ea_siteID)) allocate (ea_siteID(5*nc))
  if (.not. allocated(ed_siteID)) allocate (ed_siteID(5*nc))
  ea_siteID = 0
  ed_siteID = 0

  do i = 1, nc
    FOUND = .FALSE.
    ! for each group type on species
    do j = 1, comp(i)%ngroupTypes
      ! find group in database at index k
      do k = 1, n_db
        if (grpID_db(k) == comp(i)%groupid(j)) then
          FOUND = .TRUE.
          exit
        end if
      end do
      if (FOUND == .FALSE.) THEN
        write (err_message, '(A,I7)') 'Group is not found in database: ', comp(i)%groupid(j)
        sys_message = ''
        call reportErr
      end if

      ! database group index is found, increment n_ea sites
      if (n_ea_db(k) > 0) then
        nsites = nsites + 1
        ! store site if unique for case
        l = 1
        FOUND = .FALSE.
        do while ((ea_siteID(l) > 0) .and. (l <= 5*nc))  ! check stored sites for this case
          if (ea_siteID(l) == ea_siteID_db(k)) then
            FOUND = .TRUE.  ! site has been found previously
            exit
          end if
          l = l + 1
        end do
        if (l == 5*nc) then
          err_message = 'At limit for storing ea sites for counting. Size should be increased'
          sys_message = ''
          call reportErr
        end if
        if (.not. FOUND) then
          ea_siteID(l) = ea_siteID_db(k)   ! store unique site
          n_ea = n_ea + 1 ! increment uniques site
        end if
      end if

      ! database group index is found, increment n_ea sites
      if (n_ed_db(k) > 0) then
        nsites = nsites + 1
        ! store site if unique for case
        l = 1
        FOUND = .FALSE.
        do while ((ed_siteID(l) > 0) .and. (l <= 5*nc))  ! check stored sites for this case
          if (ed_siteID(l) == ed_siteID_db(k)) then
            FOUND = .TRUE.  ! site has been found previously
            exit
          end if
          l = l + 1
        end do
        if (l == 5*nc) then
          err_message = 'At limit for storing ed sites for counting. Size should be increased'
          sys_message = ''
          call reportErr
        end if
        if (.not. FOUND) then
          ed_siteID(l) = ed_siteID_db(k)   ! store unique site
          n_ed = n_ed + 1 ! increment uniques site
        end if
      end if

      ! database group index is found, increment n_ea and n_ed sites for eda site
      if (n_eda_db(k) > 0) then
        nsites = nsites + 1
        ! store site if unique for case
        l = 1
        FOUND = .FALSE.
        do while ((ea_siteID(l) > 0) .and. (l <= 5*nc))  ! check stored sites for this case
          if (ea_siteID(l) == eda_siteID_db(k)) then
            FOUND = .TRUE.  ! site has been found previously
            exit
          end if
          l = l + 1
        end do
        if (l == 5*nc) then
          err_message = 'At limit for storing ea sites for counting. Size should be increased'
          sys_message = ''
          call reportErr
        end if
        if (.not. FOUND) then
          ea_siteID(l) = eda_siteID_db(k)   ! store unique site
          n_ea = n_ea + 1 ! increment uniques site
        end if
        ! store site if unique for case
        l = 1
        FOUND = .FALSE.
        do while ((ed_siteID(l) > 0) .and. (l <= 5*nc))  ! check stored sites for this case
          if (ed_siteID(l) == eda_siteID_db(k)) then
            FOUND = .TRUE.  ! site has been found previously
            exit
          end if
          l = l + 1
        end do
        if (l == 5*nc) then
          err_message = 'At limit for storing ed sites for counting. Size should be increased'
          sys_message = ''
          call reportErr
        end if
        if (.not. FOUND) then
          ed_siteID(l) = eda_siteID_db(k)   ! store unique site
          n_ed = n_ed + 1 ! increment uniques site
        end if
      end if
    end do
  end do

  ! the number of association strength pairs should be n_ea*n_ed
  ! build list of pairs to find
  if (.not. allocated(sitepairs)) allocate (sitepairs(n_ea*n_ed, 3)) ! last column to indicate if loaded
  sitepairs = 0
  nsitepair = n_ea*n_ed ! needed number of pairs

  ! table with pairs to find
  do i = 1, n_ea
    do j = 1, n_ed
      sitepairs((i - 1)*n_ea + j, 1:2) = (/ea_siteID(i), ed_siteID(j)/)
    end do
  end do

  ! done with ea_SiteID and ed_SiteID
  if (allocated(ea_siteID)) deallocate (ea_siteID)
  if (allocated(ed_siteID)) deallocate (ed_siteID)

  if (.not. allocated(site)) allocate (site(nsites))

  ! Repeat loop to assign sites to comp
  nsites = 0  ! number os site types, incremented if the same site type is on a different species.
  do i = 1, nc
    ! for each group type on species
    do j = 1, comp(i)%ngroupTypes
      ! find group in database
      do k = 1, n_db
        if (grpID_db(k) == comp(i)%groupid(j)) exit
      end do
      ! database group index is found, assign groups to comp
      ! assign site(i)%packed index
      ! assign site(i)%id
      ! assign site(i)%name
      ! assign site(i)%host index
      ! assign site(i)%noccur
      ! increment comp(j)%nsite (number of site types)
      ! add site to comp(j)%sid(k) pointer
      ! set comp(j)%acpt, comp(j)%dnr
      if (n_ea_db(k) > 0) then
        nsites = nsites + 1
        ! The %packed index provides a method to read in more sites that are present in
        ! a function call for the code implemented in Aspen. In that case the index is
        ! be set for the function call. For this stand-alone implementation, all sites are
        ! always present, so the packed id = site index
        site(nsites)%packed = nsites
        site(nsites)%host = i
        site(nsites)%id = ea_siteID_db(k)
        site(nsites)%noccur = n_ea_db(k)*comp(i)%ngroup(j)
        site(nsites)%name = trim(grp_desc_db(k))  ! will be truncated if len(grp_desc_db)>len(%name)
        comp(i)%nsite = comp(i)%nsite + 1
        comp(i)%acpt = .TRUE.
        comp(i)%sid(comp(i)%nsite) = nsites
      end if
      if (n_ed_db(k) > 0) then
        nsites = nsites + 1
        site(nsites)%packed = nsites
        site(nsites)%host = i
        site(nsites)%id = ed_siteID_db(k)
        site(nsites)%noccur = n_ed_db(k)*comp(i)%ngroup(j)
        site(nsites)%name = trim(grp_desc_db(k))  ! will be truncated if len(grp_desc_db)>len(%name)
        comp(i)%nsite = comp(i)%nsite + 1
        comp(i)%dnr = .TRUE.
        comp(i)%sid(comp(i)%nsite) = nsites
      end if
      if (n_eda_db(k) > 0) then
        nsites = nsites + 1
        site(nsites)%packed = nsites
        site(nsites)%host = i
        site(nsites)%id = eda_siteID_db(k)
        site(nsites)%noccur = n_eda_db(k)*comp(i)%ngroup(j)
        site(nsites)%name = trim(grp_desc_db(k))  ! will be truncated if len(grp_desc_db)>len(%name)
        comp(i)%nsite = comp(i)%nsite + 1
        comp(i)%acpt = .TRUE.
        comp(i)%dnr = .TRUE.
        comp(i)%sid(comp(i)%nsite) = nsites
      end if
    end do
  end do

  ! Done with site database
  if (allocated(grpID_db)) deallocate (grpID_db)
  if (allocated(n_ea_db)) deallocate (n_ea_db)
  if (allocated(n_ed_db)) deallocate (n_ed_db)
  if (allocated(n_eda_db)) deallocate (n_eda_db)
  if (allocated(ea_siteID_db)) deallocate (ea_siteID_db)
  if (allocated(ed_siteID_db)) deallocate (ed_siteID_db)
  if (allocated(eda_siteID_db)) deallocate (eda_siteID_db)
  if (allocated(grp_desc_db)) deallocate (grp_desc_db)
  close (sitesFile)

  ! Determine if association is present in mixture
  if ((n_ea > 0) .and. (n_ed > 0)) aspmx = 1 ! =1 if Association or Solvation is Present in mixture

  ! Determine if components self-associate
  do i = 1, nc
    if ((comp(i)%dnr .AND. comp(i)%acpt)) comp(i)%selfassoc = 1
  end do

  ! Open Association Paramteer File
  data_line = TRIM(GPAdir)//"\Input\GammaPa\"//TRIM(assocFileName)
  OPEN (assocFile, iostat=ioErr, iomsg=sys_message, file=TRIM(data_line), STATUS='OLD')
  if (ioErr .ne. 0) then
    err_message = 'Bad association parameter file: '//TRIM(data_line)
    call reportErr
  else
    print *, 'Association Parameter File opened successfully:', TRIM(data_line)
  end if

! Read number of association parameters
  READ (assocFile, '(A)', ioStat=ioErr, iomsg=sys_message) data_line
  if (ioErr /= 0) then
    err_message = 'Error reading association parameter library.'
    call reportErr
  end if

  ! reuse variable n_db for this database
  start_index = 1
  call parse_line(start_index, data_line, data_field)
  read (data_field, *) n_db

  READ (assocFile, '(A)', ioStat=ioErr, iomsg=sys_message) data_line ! header for site params
  if (ioErr /= 0) then
    err_message = 'Error reading association parameter library.'
    call reportErr
  end if
  start_index = 1
  call parse_line(start_index, data_line, data_field)
  read (data_field, *) ts_db

  write (outfile, '(A,I)') 'Timestamp from association parameter file: ', ts_db
  if (ioErr /= 0) then
    err_message = 'Error writing timestamp to output file.'
    call reportErr
  end if

  ! compare timestamp of association parameters with ts of species
  do i = 1, nc
    if (comp(i)%ts > ts_db) then
      write (outfile, '(A)') 'Timestamp of species newer than timestamp of association strengths.'
write (outfile, '(A,I9,A,I9)', ioStat=ioErr, iomsg=sys_message) trim(comp(i)%name)//' ts : ', comp(i)%ts, ' Association ts: ', ts_db
      if (ioErr /= 0) then
        err_message = 'Error writing timestamps to output file.'
        call reportErr
      end if
      print *, ' '
      print *, 'Species timestamp is newer than association timestamp.'
      print *, 'Association parameters may have been fitted with older species parameters.'
      print *, trim(comp(i)%name), ' ts : ', comp(i)%ts, ' Association ts: ', ts_db
      pause 'Press enter to proceed. Cntl-C to terminate.'
    end if
  end do

  do i = 1, 5
    READ (assocFile, '(A)', ioStat=ioErr, iomsg=sys_message) data_line ! header for site params
    if (ioErr /= 0) then
      err_message = 'Error reading association parameter library.'
      call reportErr
    end if
  end do
  WRITE (outfile, '(A)') 'KOP from association file.'
  WRITE (outfile, '(A)') trim(data_line)

  start_index = 1
  do i = 1, 10
    call parse_line(start_index, data_line, data_field)
    read (data_field, *) kop_assoc(i)
  end do

  if (.not. all(KOP(2:10) == kop_assoc(2:10))) then
    print *, ' '
    print *, 'KOP for this case differs from KOP used with association constants.'
    print 420, 'KOP(2:10) this case:  ', (KOP(I), i=2, 10)
    print 420, 'KOP(2:10) assoc file: ', (kop_assoc(I), i=2, 10)
    write (outfile, '(A)', ioStat=ioErr, iomsg=sys_message) '**KOP for assoc file differs from KOP for this case.**'
    pause 'Press enter to proceed. Cntl-C to terminate.'
  end if
420 FORMAT(A, 9(I3))

  ! allocate memory for association database
  ! reuse variables ea_siteID_db and ed_siteID_db
  if (.not. allocated(ea_siteID_db)) allocate (ea_siteID_db(n_db))
  if (.not. allocated(ed_siteID_db)) allocate (ed_siteID_db(n_db))
  if (.not. allocated(KAD_db)) allocate (KAD_db(n_db))
  if (.not. allocated(epsok_db)) allocate (epsok_db(n_db))

  READ (assocFile, '(A)', ioStat=ioErr, iomsg=sys_message) data_line ! header for site params
  if (ioErr /= 0) then
    err_message = 'Error reading association parameter library.'
    call reportErr
  end if
  write (outfile, '(A)') data_line

  do i = 1, n_db
    READ (assocFile, '(A)', ioStat=ioErr, iomsg=sys_message) data_line
    if (ioErr /= 0) then
      err_message = 'Error reading association parameter library.'
      call reportErr
    end if
    start_index = 1
    call parse_line(start_index, data_line, data_field)
    read (data_field, *) ea_siteID_db(i)
    if (i > 1) then
      if (ea_siteID_db(i) < ea_siteID_db(i - 1)) then
      write (err_message, '(A, I4, I9, A, I9)') 'EA not sorted in ascending order at line ', i, ea_siteID_db(i), ea_siteID_db(i - 1)
        call reportErr
      end if
    end if
    call parse_line(start_index, data_line, data_field)
    read (data_field, *) ed_siteID_db(i)
    call parse_line(start_index, data_line, data_field)
    read (data_field, *) KAD_db(i)
    call parse_line(start_index, data_line, data_field)
    read (data_field, *) epsok_db(i)
    if ((ed_siteID_db(i) < ea_siteID_db(i)) .and. (ea_siteID_db(i) < 90000)) then
      write (err_message, '(A,I2,A,I9,I9)') 'EA should appear before ED unless type EDA at line ', i, ' ', ea_siteID_db(i), ed_siteID_db(i)
      call reportErr
    end if
  end do
  ! check if not end of file
  READ (assocFile, '(A)', ioStat=ioErr, iomsg=sys_message) data_line
  if (ioErr /= iostat_end) then
    call parse_line(start_index, data_line, data_field)
    ! check for blank line, which is OK
    if (data_field /= missing) then
      err_message = 'Unread lines remain in association database. Check input field with number of sites.'
      sys_message = ''
      call reportErr
    end if
  end if

  if (.not. allocated(KAD)) allocate (KAD(nsites, nsites))
  if (.not. allocated(eps)) allocate (eps(nsites, nsites))
  KAD = 0
  eps = 0

  ! load association parameters
  ! first find ea row

  do i = 1, nsites
    do j = 1, n_db
      if (site(i)%id < ea_siteID_db(j)) exit ! exit if all sorted ea sites have been checked
      if (site(i)%id == ea_siteID_db(j)) then ! check ed sites that mate with ea site i
        do k = 1, nsites
          if ((site(k)%id - 30000) < 0) cycle ! skip ea sites
          if (site(k)%id == ed_siteID_db(j)) then ! a site pair is found
            KAD(i, k) = KAD_db(j)
            KAD(k, i) = KAD(i, k)
            eps(i, k) = epsok_db(j)
            eps(k, i) = eps(i, k)
            ! mark as found in sitepairs
            do l = 1, nsitepair
              if (all(sitepairs(l, 1:2) == (/site(i)%id, site(k)%id/))) sitepairs(l, 3) = 1 ! set to 1 when found
            end do
            write (outfile, '(2(I6,A),G15.6,A,G15.6)') site(i)%id, achar(9), site(k)%id, achar(9), &
              KAD_db(j), achar(9), epsok_db(j)
          end if
        end do
      end if
    end do
  end do

! check for any pairs that were not found
  FOUND = .TRUE.
  do i = 1, nsitepair
    if (sitepairs(i, 3) == 0) then
      write (data_line, '(A, 2(I6))') 'Association Site pair not found for ', sitepairs(i, 1:2)
      print *, trim(data_line)
      write (outfile, '(A)', ioStat=ioErr, iomsg=sys_message) data_line
      FOUND = .FALSE.
    end if
  end do

  if (.not. FOUND) then
    err_message = 'Some association pairs not found. Terminating.'
    sys_message = ''
    call reportErr
  end if

  ! deallocate
  if (allocated(ea_siteID_db)) deallocate (ea_siteID_db)
  if (allocated(ed_siteID_db)) deallocate (ed_siteID_db)
  if (allocated(sitepairs)) deallocate (sitepairs)
  if (allocated(KAD_db)) deallocate (KAD_db)
  if (allocated(epsok_db)) deallocate (epsok_db)

END SUBROUTINE LOADASSOC
