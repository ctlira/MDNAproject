! *******************************************************

! gammapa_main.f90
!
! main entry point for GAMMA_PA application
! gammapa_main loads parameters and shows example calls to gamma_pa
! where the calculations are executed.
!
!*********************************************************
!

PROGRAM GAMMAPA_MAIN

use GPA_CONSTANTS
! constants includes the outfile id which should not be used for another file
use GPA_SITENSPECIES
use GPA_VOLUMES
USE GPA_PHYS_PARMS

IMPLICIT NONE

! Please update the version when making more than minor fixes
! Append RTPT or TPT1 so that debug code shows the option used for compiled code.
character(len=10) :: ver = '0.2TPT1'

! todo - track sub and Henry's law components
! INTEGER NSUB,  NSUP

INTEGER KOP(10), KCALC, ioErr, iErr
real*8 Tkelvin, Pbar
REAL*8, dimension(:), allocatable :: gamma, dgamma
! REAL*8 gammaPas(55), dgammaPas(55)
character(255) dumString
integer i
LOGICAL :: LOUDER = .TRUE.

iErr=0

! for physical and combinatorial models
! aparam, bparam for NRTL, Nagata, Wilson, tau for NRTL, Aij for SH

print *, 'Welcome to the GAMMAPA program.'
print *, ' '
GPAdir='c:\MDNAproject\'			! JRE
GPAdir=''			! CTL


! for physical and combinatorial models
! aparam, bparam for NRTL, Nagata, Wilson, tau for NRTL, Aij for SH

! Prepare for output
dumString=TRIM(GPAdir)//"Output\GAMMAPAout.txt"		! JRE
OPEN(outfile, ioStat=ioErr, file=dumString)
if (ioErr.ne.0) then
	if(LOUDER)print *, 'Bad output file. Error,file = ', ioErr,TRIM(dumString)
	iErr=ioErr
	!return
else
	if(LOUDER)print *, 'Output File opened successfully:',TRIM(dumString)
end if
dumString=TRIM(GPAdir)//"Output\GAMMAPAdebug.txt"	! JRE
OPEN(debugfile, ioStat=ioErr, file=dumString)
if (ioErr.ne.0) then
	if(LOUDER)print *, 'Bad debug file. Error,file = ', ioErr,TRIM(dumString)
	iErr=ioErr
	!return
else
	if(LOUDER)print *, 'Debug File opened successfully:',TRIM(dumString)
end if
if(iErr.NE.0)pause 'GammaPaMain: errors at program startup. Recommend you stop and fix.'
print *, 'Output will be written to:',TRIM(dumString)
print *, ' '
print *, ' '
print *, 'The input files ParmsGpaAssoc.txt and ParmsGpaNRTL.txt should reside'
print *, 'in the folder <project>/Input.'
print *, ' '

print *, 'Enter the Temperature(K) and Pressure (bar)'
print *, 'Use D0 on the end to read as double precision, e.g. 298.15D0'
read(*,*) TKelvin, Pbar

print *, 'Enter the Kcalc variable to indicate the calculations desired.'
print *, '1 - calculate only gammas'
print *, '2 - calculate only gamma derivative, d(ln gamma)/dT'
print *, '3 - calculate both gamma and gamma derivative'
print *, 'Note that Kcalc > 1 does a lot of calcs with T derivatives,'
print *, 'so some debugging output is supressed relative to Kcalc = 1.'
read(*,*) Kcalc
print *, 'You entered:', Kcalc

write(outfile,'(A, I3 )') 'Kcalc ', Kcalc
write(outfile, '(A, 2F10.3)') 'T(K) P(bar) ', Tkelvin,Pbar

! set composition of interest
! recall x is shared in gpa_sitenspecies
call loadsites(KOP,iErr)

! nc is now known
IF(.NOT.ALLOCATED(x)) allocate(x(nc))
IF(.NOT.ALLOCATED(gamma))allocate(gamma(nc))
IF(.NOT.ALLOCATED(dgamma))allocate(dgamma(nc))
IF(.NOT.ALLOCATED(aparam))allocate(aparam(nc,nc))
IF(.NOT.ALLOCATED(bparam))allocate(bparam(nc,nc))
IF(.NOT.ALLOCATED(alpha))allocate(alpha(nc,nc))
IF(.NOT.ALLOCATED(Aij))allocate(Aij(nc,nc))
x = 0D0
aparam = 0D0
bparam = 0D0
alpha = 0D0
Aij = 0D0

if(kop(1).gt.0) then
    WRITE(debugfile,"(120('-'))")
	WRITE(debugfile,'(A,A10)') 'Print debug info. GAMMAPA version ',ver
	WRITE(debugfile,'(A,10I3)') 'Option codes ', (kop(i),i=1,10)
    WRITE(debugfile,900) NC
    WRITE(debugfile,*) '*** Species Identifiers'
    do i=1,nc
        WRITE (debugfile, '(3X, I4, 3X, I4, 3X, A20)') i, comp(i)%id, comp(i)%name
    enddo
endif
900  FORMAT('n Species ', I3)

! load physical parameters
select case (kop(4))
    case (0)
        call loadnrtl(iErr)
        if(kop(1).gt.1) then
            write(debugfile,*) ' *** GMNRTL Physical aij'
            write(debugfile,'(6G12.5)') aparam
            write(debugfile,*) ' *** GMNRTL Physical bij'
            write(debugfile,'(6G12.5)') bparam
            write(debugfile,*) ' *** GMNRTL Physical alpha'
            write(debugfile,'(6G12.5)') alpha
        endif

    case (1) ! wilson
        call loadwilson(iErr)
        if(kop(1).gt.1) then
            write(debugfile,*) ' *** GMUwilson Physical aij'
            write(debugfile,'(6G12.5)') aparam
            write(debugfile,*) ' *** GMUwilson Physical bij'
            write(debugfile,'(6G12.5)') bparam
        endif

    case (2) !scatchard hildebrand (eqn Elliott/Lira 12.24,12.25)
        call loadsh(iErr)
        if(kop(1).gt.1) then
            write(debugfile,*) ' *** GMUscathild Physical aij'
            write(debugfile,'(6G12.5)') aparam
            write(debugfile,*) ' *** GMUscathild Physical bij'
            write(debugfile,'(6G12.5)') bparam
        endif

	case (3) ! Nagata1
        call loadnagata(iErr)
        if(kop(1).gt.1) then
            write(debugfile,*) ' *** GMUnagata Physical aij'
            write(debugfile,'(6G12.5)') aparam
            write(debugfile,*) ' *** GMUnagata Physical bij'
            write(debugfile,'(6G12.5)') bparam
            write(debugfile,*) ' *** GMUnagata Physical aij + bij/T'
            write(debugfile,'(6G12.5)') aparam + bparam/Tkelvin
        endif

end select

IF (kop(2) .EQ. 3) THEN
	if ((kop(1).gt.1).and.(mod(kcalc,2).gt.0)) then
        do i = 1, nc
            write(debugfile,'(I4, 3G12.5)') (i), PCSAFT_m(i), PCSAFT_sigma(i), PCSAFT_epsok(i)
        enddo
	endif
ENDIF

!******for meoh-cyclhex-assoc.txt and meoh-cyclhex-nrtl.txt ******
! example loop for a binary.
! set T = 298.15D0 and P = 1D0
write(outfile,'(A)') 'x, lngamma, gamma, hex'
do i = 1, 11
    x(1) = dble(i-1)/10D0
    x(2) = 1D0-x(1)
    call gamma_pa(kop, Kcalc, TKelvin, Pbar, gamma, dgamma, iErr)
    write(outfile, '(8F15.6)') X(:), gamma(:), dexp(gamma(:)), -Rgas*TKelvin**2*(dot_product(x,dgamma))
enddo
!****** end meoh-cyclhex ******************

!****** for casestudy1-assoc.txt and casestudy1-nrtl.txt *****
! set T = 298.15K for this composition
! x = (/ 0.33D0, 0.33D0, 0.34D0 /)
! set T = 347.125 (nrtl) T = 343.358 (nrtla) for this composition
!x = (/ 0.165D0, 0.165D0, 0.67D0 /)
! call gamma_pa(kop, Kcalc, T, P, gamma, dgamma, iErr)
! write(outfile, '(10F15.6)') X(:), gamma(:), dexp(gamma(:)), -Rgas*TKelvin**2*(dot_product(x,dgamma))
! set T = 298.15K for these loops
!write(outfile,'(A)') 'x, lngamma, gamma, hex'
!do i = 1, 11
! use one line or the other
!    x = (/ 0D0, dble(i-1)/10D0, 1D0-dble(i-1)/10D0 /)
!   x = (/ dble(i-1)/10D0, 0D0, 1D0-dble(i-1)/10D0 /)
!   call gamma_pa(kop, Kcalc, T, P, gamma, dgamma, iErr)
!   write(outfile, '(8F15.6)') X(:), gamma(:), dexp(gamma(:)), -Rgas*TKelvin**2*(dot_product(x,dgamma))
!enddo
!************ end casestudy1 ******************

!***********CaseStudy2i,v,ix***************
!*****CaseStudy2iCPA-assoc.txt, CaseStudy2iCPA-nrtl.txt*****
!*****CaseStudy2iPCS-assoc.txt, CaseStudy2iPCS-nrtl.txt*****
!x = (/ 0.05D0, 0.0D0, 0.25D0, 0.2D0, 0.05D0, 0.019D0, 0.431D0 /) ! Case 2v
!x = (/ 0.25D0, 0.1D0, 0.05D0, 0.1D0, 0.15D0, 0.2D0, 0.15D0 /) ! Case 2ix
!x = (/ 0.05D0, 0.075D0, 0.3D0, 0.2D0, 0.1D0, 0.01D0, 0.265D0 /) ! Case 2i
!call gamma_pa(kop, Kcalc, Tkelvin,Pbar, gamma, dgamma, iErr)
!write(outfile,'(A)') 'Gammas are not calculated when Kcalc = 2'
!write(outfile, '(A5,7F15.6,/,A5,7F15.6,/,A5,7F15.6,/)') 'x', X(:), 'ln(g)', gamma(:), 'g', dexp(gamma(:))
!write(outfile,'(A)') 'Hxs is not calculated if Kcalc = 1'
!write(outfile,'(A10, F15.6)') 'Hxs(J/mol)', -Rgas*TKelvin**2*(dot_product(x,dgamma))
!*********** end CaseStudy2i,v,ix***************

close(outfile)
close(debugfile)

print *, 'Program has ended normally. The output is in Output/GAMMAPAout.txt.'
print *, 'Press any key to close this window.'
pause
if(allocated(gamma)) deallocate(gamma)
if(allocated(dgamma)) deallocate(dgamma)
if(allocated(aparam)) deallocate(aparam)
if(allocated(bparam)) deallocate(bparam)
if(allocated(alpha)) deallocate(alpha)
if(allocated(Aij)) deallocate(Aij)
if(allocated(site)) deallocate(site)
if(allocated(comp)) deallocate(comp)
if(allocated(x)) deallocate(x)
if(allocated(KAD)) deallocate(KAD)
if(allocated(eps)) deallocate(eps)
if(allocated(PCSAFT_sigma)) deallocate(PCSAFT_sigma)
if(allocated(PCSAFT_m)) deallocate(PCSAFT_m)
if(allocated(PCSAFT_epsok)) deallocate(PCSAFT_epsok)
if(allocated(veq)) deallocate(veq)
if(allocated(bvol)) deallocate(bvol)
if(allocated(r_uniquac)) deallocate(r_uniquac)
if(allocated(q_uniquac)) deallocate(q_uniquac)
if(allocated(vstd)) deallocate(vstd)
if(allocated(vparms)) deallocate(vparms)
END PROGRAM GAMMAPA_MAIN
