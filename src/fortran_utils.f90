module fortran_utils
use iso_fortran_env, only: real64
use linalg_solver
implicit none
public
private :: rev_cmplx, rev_real, rev_int
private :: print_mat_bool, print_mat_int, print_mat_real, print_mat_cmplx
private :: lineval_r

! The rev(X) interface
!!! call rev(X)  ---- reverses a vector whether real, int, cmplx
interface rev
module procedure rev_cmplx, rev_real, rev_int
end interface rev

! The print_mat(X) interface
!!! integer dmp !!create a dummy variable
!! dmp = print_mat(X) !!works for X as int, bool, real, cmplx
interface print_mat
module procedure print_mat_bool, print_mat_int, print_mat_real, print_mat_cmplx
end interface print_mat

! The lineval(X) interface
interface lineval
module procedure lineval_r
end interface lineval
contains

!function produces a sequence
subroutine seqn(st, en, len, X)
real(real64), intent(in) :: st, en
integer, intent(in) :: len
real(real64) :: X(len)
integer :: i
real(real64) :: del
del = (en-st)/(len-1)
do i = 1, len
X(i) = st + (i-1)*del
end do
end subroutine seqn

!function calculates the difference of subsequent nos in a vector
subroutine diff(X, Y)
real(real64), dimension(:), intent(in)::X
real(real64) :: Y(size(X)-1)
integer :: i
do i = 1, size(Y)
Y(i) = X(i+1) - X(i)
end do
end subroutine diff

!function calculates the mean of a vector
subroutine mean(X, y)
real(real64), intent(in), dimension(:) :: X
real(real64) :: y
y = sum(X)/size(X)
end subroutine mean

!function calculates the var of a vector
subroutine var(X, res)
real(real64), intent(in), dimension(:) :: X
real(real64) :: res
real(real64) :: dmp = 0
integer :: i
real(real64) :: a
call mean(X, a)
do i = 1, size(X)
dmp = dmp + (X(i) - a)**2
end do
!print *, dmp
!print *, size(X)-1
res = dmp/(size(X)-1)
dmp = 0
end subroutine var

!function calculates the sdev of a vector
subroutine sdev(X, res)
real(real64), intent(in), dimension(:) :: X
real(real64) :: res, res1
call var(X, res1)
res = sqrt(res1)
end subroutine sdev

!function averages between successive elements of vector
subroutine mdpnt_vec(X, res)
real(real64), intent(in), dimension(:) :: X
real(real64) :: res(size(X)-1)
integer :: i, n
n = size(X)
do i = 1, n-1
res(i) = (X(i) + X(i+1))/2
end do
end subroutine mdpnt_vec

!subroutine for reversing a real vector
subroutine rev_real(X)
real(real64) :: X(:)
integer :: n, i
real(real64) :: tmp
n = size(X)
do i = 1, n/2
tmp = X(i)
X(i) = X(n+1-i)
X(n+1-i) = tmp
end do

end subroutine rev_real

!subroutine for reversing a complex vector
subroutine rev_cmplx(X)
complex(real64) :: X(:)
integer :: n, i
complex(real64) :: tmp
n = size(X)
do i = 1, n/2
tmp = X(i)
X(i) = X(n+1-i)
X(n+1-i) = tmp
end do

end subroutine rev_cmplx

!Subroutine for reversing an integer vector
subroutine rev_int(X)
integer :: X(:)
integer n, i
integer :: tmp
n = size(X)
do i = 1, n/2
tmp = X(i)
X(i) = X(n+1-i)
X(n+1-i) = tmp
end do
end subroutine rev_int

!function for print_mat_real(M)
subroutine print_mat_real(M)
real(real64), intent(in) :: M(:,:)
integer i, dmp
do i = 1, size(M, 1)
print *, M(i, :)
end do
end subroutine print_mat_real

subroutine print_mat_int(M)
integer, intent(in) :: M(:,:)
integer i, dmp
do i = 1, size(M, 1)
print *, M(i, :)
end do
end subroutine print_mat_int

subroutine print_mat_bool(M)
logical, intent(in) :: M(:,:)
integer i, dmp
do i = 1, size(M, 1)
print *, M(i, :)
end do
end subroutine print_mat_bool

subroutine print_mat_cmplx(M)
complex(real64), intent(in) :: M(:,:)
integer i, dmp
do i = 1, size(M, 1)
print *, M(i, :)
end do
end subroutine print_mat_cmplx

subroutine lineval_r(X, m, n, low, hi) bind(C, name="lineval")
integer, intent(in) :: m, n
real(real64) :: X(m, n)
real(real64), intent(in) :: low, hi
X = ((hi - low)/(maxval(X) - minval(X)))*(X - minval(X)) + low
end subroutine lineval_r


end module fortran_utils

