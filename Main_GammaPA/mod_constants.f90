MODULE GPA_CONSTANTS
  USE, INTRINSIC :: ISO_FORTRAN_ENV, only: REAL64
  REAL(REAL64), PARAMETER :: avoNum = 602.214076d0, kB = 0.01380649D0, Rgas = avoNum*kB
  INTEGER, PARAMETER :: inputFile=1006, specFile=1007, volFile=1008, sitesFile=1009, assocFile=1010, physFile=1011, outfile=2000, debugfile=2001
  INTEGER KOP(10), KCALC
  ! Units 10, 11, 12 are used for gamma.csv, hxs.csv, vol.csv respectively, within code
  CHARACTER(4), PARAMETER :: missing = '1D35'
  !When all is working, we may set GPAdir=PGLdir.
  CHARACTER(255) GPAdir
END MODULE GPA_CONSTANTS
