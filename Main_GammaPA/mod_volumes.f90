MODULE GPA_VOLUMES
  USE, INTRINSIC :: ISO_FORTRAN_ENV, only: REAL64
  integer, allocatable :: veq(:)
  real(REAL64), dimension(:), allocatable :: bvol, r_uniquac, q_uniquac, vstd
  real(REAL64), dimension(:, :), allocatable :: vparms
END MODULE GPA_VOLUMES
