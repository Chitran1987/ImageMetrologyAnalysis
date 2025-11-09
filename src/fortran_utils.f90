module fortran_utils
use iso_fortran_env, only: real64
use linalg_solver
implicit none
public
private :: rev_cmplx, rev_real, rev_int
private :: print_mat_bool, print_mat_int, print_mat_real, print_mat_cmplx
private :: lineval_r

!Define the constants------------------------------------------------------
real(real64) :: pi = 4.0_real64*atan(1.0_real64)
!End definition of constants-----------------------------------------------


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

subroutine lin_reg(dat, p, q, Coeff) bind(C, name="lin_reg")
    integer :: p, q
    real(real64) :: dat(p, q)
    real(real64) :: Coeff(q)
    real(real64), allocatable :: A(:),M(:,:), dmp(:)
    integer :: i, j, info
    !p = size(dat, 1)
    !q = size(dat, 2)
    !allocate(Coeff(q))
    allocate(A(q))
    allocate(dmp(q))
    allocate(M(q,q)) !This is a symmetric matrix, barring the last row and column
    do concurrent (j=1:q-1, i=1:q-1, i>=j) !Fill the first half
        A(j) = dot_product(dat(:,j),dat(:,q))
        M(i,j) = dot_product(dat(:,i), dat(:,j))
    end do
    do concurrent (j=2:q-1, i=1:q-1, i<j) !Write to the 2nd half
        M(j,i) = M(i,j)
    end do
    A(q) = sum(dat(:,q))
    do i = 1, q-1
        dmp(i) = sum(dat(:,i))
    end do
    dmp(q) = real(p, kind=real64)
    M(:,q) = dmp
    M(q,:) = dmp
    call solve_linear_system(A = M, b = A, info = info)
    if ( info == 0 ) then
        Coeff = A
    else if ( info > 0 ) then
        error stop "Matrix M is singular — cannot invert."
    else
        error stop "Invalid arguments passed - please check arguments"
    end if
end subroutine lin_reg

function lin_bg_sub_1D(dat, win, min_zero) result(ret_mat)
    real(real64) :: dat(:,:), win(:,:) !Input declaration
    real(real64), allocatable :: ret_mat(:,:) !Output declaration
    real(real64), allocatable :: new_dat(:,:) !matrix fed in linreg
    real(real64) :: X(size(dat, 1)), Y(size(dat, 1)) !The X and Y vectors built out of the dataset dat
    real(real64) :: coeff(2) !The regression co-efficients
    logical :: mask_int(size(win,1), size(dat,1)) !The mask matrix declaration
    logical :: mask(size(dat, 1)) !The mask vector declaration
    logical, intent(in), optional :: min_zero !The optional input
    logical :: minz !Need it because min_zero is optional
    integer :: n_win !The no of rows in the win matrix
    integer :: i !Internally usable integers

    !default values amongst arguments
    if ( .not. present(min_zero) ) then
        minz = .true.
    else
        minz = min_zero
    end if
    !global scope for entire function
    X = dat(:,1)
    Y = dat(:,2)

    !Create the actual mask matrix.
    !See whether this matrix can be made cache friendly
    n_win = size(win, 1)
    do i = 1, n_win
        mask_int(i,:) = ( X >= win(i,1) ) .and. ( X <= win(i,2))
    end do

    !Create the mask vector
    mask = any(mask_int, dim=1)
    !Mask to get the new X and Y
    X = pack(X, mask)
    Y = pack(Y, mask)
    !Create the matrix which will be fed into linreg
    allocate(new_dat(size(X),2))
    new_dat(:,1) = X
    new_dat(:,2) = Y
    !get the regression co-efficients
    call lin_reg(dat=new_dat, p = size(new_dat, 1), q = 2, Coeff = coeff)
    !Input the original matrix to the result
    ret_mat = dat
    ret_mat(:,2) = ret_mat(:,2) - (coeff(1)*ret_mat(:,1) + coeff(2))
    if ( minz .eqv. .true. ) then
        ret_mat(:,2) = ret_mat(:,2) - minval(ret_mat(:,2))
    end if
end function lin_bg_sub_1D

pure elemental real(real64) function gauss_1D_core(x, A, x0, sig) result(y)
    real(real64), intent(in) :: x
    real(real64), intent(in) :: A, x0, sig
    y = A*exp((-1.0_real64*(x-x0)**2.0_real64/(2.0_real64*sig**2.0_real64)))
end function gauss_1D_core

function gauss_1D(x, A, x0, sig) result(y)
    real(real64), intent(in) :: x(:)
    real(real64), optional :: A, x0, sig
    real(real64) :: A_alt, x0_alt, sig_alt
    real(real64) :: y(size(x))
    if ( .not. present(x0) ) then
        x0_alt = 0.0_real64
    else
        x0_alt = x0
    end if
    if ( .not. present(sig) ) then
        sig_alt = 1.0_real64
    else
        sig_alt = sig
    end if
    if ( .not. present(A) ) then
        A_alt = 1.0_real64/(sqrt(2*pi)*sig_alt)
    else
        A_alt = A
    end if
    y = gauss_1D_core(x, A_alt, x0_alt, sig_alt)
end function gauss_1D

function gauss_2D_nocorr(X, Y, Ax, Ay, x0, y0, sig_x, sig_y) result(tens)
    real(real64), intent(in) :: X(:), Y(:) !The main inputs
    real(real64), optional :: Ax, Ay, x0, y0, sig_x, sig_y !The optional inputs
    real(real64) :: tens(size(Y), size(X), 3) !The output
    real(real64) :: Xsp(size(Y), size(X)), Ysp(size(Y), size(X)), Gsp(size(Y), size(X)) !The dummys for each tensor position tens(:,:,1), tens(:,:,2) and tens(:,:,3)
    real(real64) :: Gx(size(X)), Gy(size(Y)), Gx_sp(size(Y), size(X)), Gy_sp(size(Y), size(X)) !The dummys for Gx*Gy at each position
    real(real64) :: Ax_alt, Ay_alt, x0_alt, y0_alt, sig_x_alt, sig_y_alt
    !Error handling for Ax, Ay, x0, y0, sig_x, sig_y
    if ( .not. present(x0) ) then
        x0_alt = 0.0_real64
    else
        x0_alt = x0
    end if
    if ( .not. present(sig_x) ) then
        sig_x_alt = 1.0_real64
    else
        sig_x_alt = sig_x
    end if
    if ( .not. present(Ax) ) then
        Ax_alt = 1.0_real64/(sqrt(2*pi)*sig_x_alt)
    else
        Ax_alt = Ax
    end if
    if ( .not. present(y0) ) then
        y0_alt = 0.0_real64
    else
        y0_alt = y0
    end if
    if ( .not. present(sig_y) ) then
        sig_y_alt = 1.0_real64
    else
        sig_y_alt = sig_y
    end if
    if ( .not. present(Ay) ) then
        Ay_alt = 1.0_real64/(sqrt(2*pi)*sig_y_alt)
    else
        Ay_alt = Ay
    end if
    !
    !The core algorithm
    !First assume that everything is present
    Gx = gauss_1D_core(X, Ax_alt, x0_alt, sig_x_alt)
    Gy = gauss_1D_core(Y, Ay_alt, y0_alt, sig_y_alt)
    call rev(Gy) !Gy needs to be reversed
    Gx_sp = spread(Gx, dim=1, ncopies=size(Y))
    Gy_sp = spread(Gy, dim=2, ncopies=size(X))
    Gsp = Gx_sp*Gy_sp
    Xsp = spread(X, dim = 1, ncopies=size(Y))
    call rev(Y) !Y needs to be reversed for plotting as an X-Y plane
    Ysp = spread(Y, dim = 2, ncopies=size(X))
    tens(:,:,1) = Gsp
    tens(:,:,2) = Xsp
    tens(:,:,3) = Ysp
end function gauss_2D_nocorr

end module fortran_utils

