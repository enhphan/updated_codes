program FreeEnergy
implicit none
!integer::hit1(24,24),hit2(24,24)
integer::n,m,j,r,hitnet(100,100)=0
real*8::fe,femat(100,100)=0
real*8::temp,k=0.0019872 !temperature is in kelvin, k is the boltzmann constant in kcal/mol*K
double precision::npdb
character(len=30)mfr,mfi
!
!open and reads in histogram produced by the func_test/pair_check code/program.
!disable if a single hit matrix exits; feed that in via i/o manipulation. see below
!open(unit=10, file='hist')
!open(unit=11, file='/home/hephan/wca12rna_u24_umbloop4_v104_mc2/0001/hist')
!read(10,*)hit1
!read(11,*)hit2
!
!the above open statements (unit 10 and 11) are really clunky and are specific to 
!the case of what i've done to process the data. Ideally there should be a single 
!hit matrix stored in the hist file for each collective set of simulations performed. 
!In that case, just feed the hist file into the program via standard input
! 
write(*,*)'How many nucleotides are in the strand?'
read(*,*)r
write(*,*) 'Enter desired temperature (in kelvin)'
read(*,*) temp
write(*,*) 'How many pdb files were analyzed in total for this hit matrix?'
read(*,*) npdb
write(mfr,'(a,i3,a)')'(',r,'f8.3)'
write(mfi,'(a,i3,a)')'(',r,'i7)'
!
open(unit=10,file='r_hist')
read(10,mfi) hitnet(1:r,1:r) !enable to read from standard input
!
!
!will be used to test the read in/write out process of the pro!gram, can be ignored/commented out afterwards.
open(unit=12, file='test')
100 format(35(i7))
!write(12,100)hit1
!
!the above open statements (unit 10 and 11) are really clunky and are specific to 
!the case of what i've done to process the data. Ideally there should be a single 
!hit matrix stored in the hist file for each collective set of simulations performed. 
!In that case, just fee the hist file into the program via standard input
!read(*,*) hitnet
!hitnet=real(hit1)+real(hit2)
!write(*,*)'How many nucleotides are in the strand?'
!read(*,*)r
!write(*,*) 'Enter desired temperature (in kelvin)'
!read(*,*) temp
!write(*,*) 'How many pdb files were analyzed in total for this hit matrix?'
!read(*,*) npdb
!write(mfr,'(a,i3,a)')'(',r,'f8.3)'
!fe=log(hitnet/npdb)*temp*k
101 format(i3,i3,f8.3)
open(13,file='r_fe.dat')
do m=1,r
 do n=1,r
  if (hitnet(m,n)==0) then
  femat(m,n)=0
  else
  fe=-log(hitnet(m,n)/npdb)*k*temp
  femat(m,n)=fe
  endif
 write(13,101)m,n,fe
 end do
end do
!102 format(35f8.3)
open(unit=14,file="r_FEmat")
write(14,mfr)femat(1:r,1:r)
stop
end
