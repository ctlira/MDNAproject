MODULE GPA_SITENSPECIES
  USE, INTRINSIC :: ISO_FORTRAN_ENV, only: REAL64
  ! See loadsites for descriptions
  TYPE siteinfo
    INTEGER :: host = 0 ! component index for site host. Currently each site on only one host
    CHARACTER(20) :: name = '' !site name
    INTEGER :: id = 0 ! site id value (e.g. 2 or 502)
    INTEGER :: noccur = 0 ! for use when each site on only one host
    INTEGER :: packed = 0 ! packed id
    REAL(REAL64) :: xhost = 0D0 ! mole fraction of host
  END TYPE siteinfo

  TYPE species
    CHARACTER(50):: name
    INTEGER :: id, ver, ts, ngroupTypes ! id = species id, ver = version of species, ngroups = number of groups on species, max 3
    LOGICAL :: selfassoc = .FALSE. ! False by default, True if self associating
    LOGICAL :: dnr = .FALSE.  ! False by default, True if electron donors on species
    LOGICAL :: acpt = .FALSE. ! False by default, True if electron acceptors on species
    INTEGER :: groupid(3) = (/0, 0, 0/) ! group id values for species, max 3
    INTEGER :: ngroup(3) = (/0, 0, 0/) ! number of groups on species, max 3
    INTEGER :: nsite = 0 ! number of sites on species, max 5
    INTEGER :: sid(5) = (/0, 0, 0, 0, 0/) ! site (defined above) indexes to point to the sites on the species, vector size limits the # sites on a species
    REAL(REAL64)  :: x = 0D0 ! mole fraction
  END TYPE species

  type(siteinfo), dimension(:), allocatable :: site ! dimensioned later to be the number of sites present
  type(species), dimension(:), allocatable :: comp ! components present
  REAL(REAL64), dimension(:), allocatable :: x
  REAL(REAL64), dimension(:, :), allocatable :: KAD, eps
  REAL(REAL64), dimension(:), allocatable :: PCSAFT_sigma, PCSAFT_m, PCSAFT_epsok ! PCSAFT
  integer :: nc = 0, nsites = 0, aspmx = 0
  ! aspmx =1 if association or solvation is present in mixture (or pure components)

CONTAINS

  SUBROUTINE species_dump(arr)
    ! call this routine for 'comp' and use breakpoints to inspect the 'Flat' variables.
    USE GPA_CONSTANTS, ONLY: debugfile
    IMPLICIT NONE

    TYPE(species), DIMENSION(:), INTENT(IN) :: arr

    ! Local "Flat" Variables for VS Code Debugger / Locals Window
    CHARACTER(50) :: dbg_name
    INTEGER :: dbg_id, dbg_ver, dbg_ngroupTypes, dbg_nsite
    LOGICAL :: dbg_selfassoc, dbg_dnr, dbg_acpt, dbg_open
    INTEGER :: i, dbg_groupid(3), dbg_ngroup(3), dbg_sid(5)
    REAL(REAL64) :: dbg_x

    INQUIRE (UNIT=debugfile, OPENED=dbg_open)
    IF (dbg_open) WRITE (debugfile, '(A)') "=========================================================="

    DO i = 1, size(arr)

      ! --- STEP A: UNPACK INTO FLAT LOCAL VARIABLES ---
      ! Place a breakpoint on the line AFTER this assignment block (e.g., the WRITE statement).
      ! Every time you hit "Continue" (F5), these flat variables will update in your
      ! Locals window for the current index 'i'.
      if (arr(i)%name(1:1) == char(0)) then
        dbg_name = 'Empty'
      else
        dbg_name = arr(i)%name
      end if
      dbg_id = arr(i)%id
      dbg_ver = arr(i)%ver
      dbg_ngroupTypes = arr(i)%ngroupTypes
      dbg_selfassoc = arr(i)%selfassoc
      dbg_dnr = arr(i)%dnr
      dbg_acpt = arr(i)%acpt
      dbg_groupid = arr(i)%groupid
      dbg_ngroup = arr(i)%ngroup
      dbg_nsite = arr(i)%nsite
      dbg_sid = arr(i)%sid
      dbg_x = arr(i)%x

      IF (dbg_open) THEN
        ! --- STEP B: WRITE LABELED DATA TO EXTERNAL LOG ---
        WRITE (debugfile, '(A,I4,A)') "--- Element [", i, "] ---"
        WRITE (debugfile, '(A,A)') "  Name:             ", TRIM(dbg_name)
        WRITE (debugfile, '(A,I8)') "  ID:               ", dbg_id
        WRITE (debugfile, '(A,I4)') "  Version:          ", dbg_ver
        WRITE (debugfile, '(A,I4)') "  nGroupTypes:      ", dbg_ngroupTypes
        WRITE (debugfile, '(A,L2)') "  Self Associating: ", dbg_selfassoc
        WRITE (debugfile, '(A,L2)') "  Electron Donor:   ", dbg_dnr
        WRITE (debugfile, '(A,L2)') "  Electron Acceptor:", dbg_acpt
        WRITE (debugfile, '(A,3I8)') "  Group IDs (3):    ", dbg_groupid
        WRITE (debugfile, '(A,3I4)') "  nGroup (3):       ", dbg_ngroup
        WRITE (debugfile, '(A,I4)') "  nSite:            ", dbg_nsite
        WRITE (debugfile, '(A,5I6)') "  Site IDs (5):     ", dbg_sid
        WRITE (debugfile, '(A,F12.6)') "  Mole Fraction x:  ", dbg_x
        WRITE (debugfile, '(A)') ""
      END IF
      ! <--- PLACE VS CODE BREAKPOINT AT END DO to step element-by-element
      ! You can hover over any 'dbg_*' variable to see its value natively.

    END DO

    IF (dbg_open) THEN
      WRITE (debugfile, '(A)') "=== END OF DUMP ==="
      WRITE (debugfile, '(A)') " "
    END IF

  END SUBROUTINE species_dump

  SUBROUTINE site_dump(arr)
    ! call this routine for 'site' and use breakpoints to inspect the 'Flat' variables.
    USE GPA_CONSTANTS, ONLY: debugfile
    IMPLICIT NONE

    TYPE(siteinfo), DIMENSION(:), INTENT(IN) :: arr

    ! Local "Flat" Variables for VS Code Debugger / Locals Window
    CHARACTER(20) :: dbg_name
    INTEGER :: i, dbg_id, dbg_host, dbg_noccur, dbg_packed
    REAL(8) :: dbg_xhost
    LOGICAL :: dbg_open

    INQUIRE (UNIT=debugfile, OPENED=dbg_open)
    IF (dbg_open) WRITE (debugfile, '(A)') "=========================================================="

    DO i = 1, size(arr)

      ! --- STEP A: UNPACK INTO FLAT LOCAL VARIABLES ---
      ! Place a breakpoint on the line AFTER this assignment block (e.g., the WRITE statement).
      ! Every time you hit "Continue" (F5), these flat variables will update in your
      ! Locals window for the current index 'i'.
      if (arr(i)%name(1:1) == char(0)) then
        dbg_name = 'Empty'
      else
        dbg_name = arr(i)%name
      end if
      dbg_id = arr(i)%id
      dbg_host = arr(i)%host
      dbg_noccur = arr(i)%noccur
      dbg_packed = arr(i)%packed
      dbg_xhost = arr(i)%xhost
      if (dbg_open) THEN
        ! --- STEP B: WRITE LABELED DATA TO EXTERNAL LOG ---
        WRITE (debugfile, '(A,I4,A)') "--- Element [", i, "] ---"
        WRITE (debugfile, '(A,A)') "  Name:             ", TRIM(dbg_name)
        WRITE (debugfile, '(A,I6)') "  ID:               ", dbg_id
        WRITE (debugfile, '(A,I8)') "  Host:             ", dbg_host
        WRITE (debugfile, '(A,I4)') "  nOccur:          ", dbg_noccur
        WRITE (debugfile, '(A,I4)') "  Packed:           ", dbg_packed
        WRITE (debugfile, '(A,F12.6)') "  Mol Frctn xhost: ", dbg_xhost
        WRITE (debugfile, '(A)') ""
      END IF
      ! <--- PLACE VS CODE BREAKPOINT AT END DO to step element-by-element
      ! You can hover over any 'dbg_*' variable to see its value natively.

    END DO
    IF (dbg_open) THEN
      WRITE (debugfile, '(A)') "=== END OF DUMP ==="
      WRITE (debugfile, '(A)') " "
    END IF

  END SUBROUTINE site_dump

END MODULE GPA_SITENSPECIES
