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

!function produces a sequence with start, length and increment
function seqn1(st, len, del) result(out)
    real(real64) :: st, del
    integer :: len
    real(real64) :: out(len)
    integer :: i
    do i = 1, len
        out(i) = st + (i-1)*del
    end do
end function seqn1

!function calculates the difference of subsequent nos in a vector
function diff(X) result(Y)
    real(real64), dimension(:), intent(in)::X
    real(real64) :: Y(size(X)-1)
    integer :: i
    do i = 1, size(Y)
    Y(i) = X(i+1) - X(i)
    end do
end function diff

!function calculates the mean of a vector
function mean(X) result(Y)
    real(real64), intent(in), dimension(:) :: X
    real(real64) :: y
    y = sum(X)/size(X)
end function mean

!function calculates the var of a vector
function var(X) result(res)
    real(real64), intent(in), dimension(:) :: X
    real(real64) :: res
    real(real64) :: dmp = 0
    integer :: i
    real(real64) :: a
    a =  mean(X)
    do i = 1, size(X)
    dmp = dmp + (X(i) - a)**2
    end do
    !print *, dmp
    !print *, size(X)-1
    res = dmp/(size(X)-1)
    dmp = 0
end function var

!function calculates the sdev of a vector
function sdev(X) result(res)
    real(real64), intent(in), dimension(:) :: X
    real(real64) :: res, res1
    res1 = var(X)
    res = sqrt(res1)
end function sdev

!function averages between successive elements of vector
function mdpnt_vec(X) result(res)
    real(real64), intent(in), dimension(:) :: X
    real(real64) :: res(size(X)-1)
    integer :: i, n
    n = size(X)
    do i = 1, n-1
    res(i) = (X(i) + X(i+1))/2
    end do
end function mdpnt_vec

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

pure elemental real(real64) function gauss_2D_nocorr_core(X, Y, A, x0, y0, sig_x, sig_y ) result(out)
    real(real64), intent(in) :: X, Y, A, x0, y0, sig_x, sig_y !Inputs
    !real(real64) :: A, x0, y0, sig_x, sig_y
    !real(real64) :: out
    out = A*exp(-1.0_real64*( (X - x0)**2/(2.0_real64*sig_x**2) + (Y- y0)**2/(2.0_real64*sig_y**2) ))
end function gauss_2D_nocorr_core

function gauss_2D_nocorr(X, Y, Ax, Ay, x0, y0, sig_x, sig_y) result(tens)
    real(real64), intent(in) :: X(:), Y(:) !The main inputs
    real(real64), optional :: Ax, Ay, x0, y0, sig_x, sig_y !The optional inputs
    real(real64) :: tens(size(Y), size(X), 3) !The output
    real(real64) :: Gsp(size(Y), size(X)) !The Gaussian Matrix
    real(real64) :: XYsp(size(Y), size(X), 2) !The XY spread called using grid2
    real(real64) :: Ax_alt, Ay_alt, x0_alt, y0_alt, sig_x_alt, sig_y_alt !The alternate inputs
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
    XYsp = grid_2(X, Y)
    Gsp = gauss_2D_nocorr_core(X = XYsp(:,:,1), Y = XYsp(:,:,2), A = Ax_alt*Ay_alt, x0 = x0_alt, y0 = y0_alt, sig_x = sig_x_alt, sig_y = sig_y_alt)
    tens(:,:,1) = Gsp
    tens(:,:,2) = XYsp(:,:,1)
    tens(:,:,3) = XYsp(:,:,2)

end function gauss_2D_nocorr

!Select a vertical line profile out of the Tm,n,p tensor
function lin_prof_v(M, v_val) result(res_mat)
    real(real64) :: M(:,:,:), v_val !inputs declaration
    real(real64) :: res_mat(size(M,1), 2) !output declaration
    real(real64) :: X_dumm(size(M,2)), dummy1(size(X_dumm)) !Dummy prior = 1
    integer :: imin(1) !Dummy prior = 2

    !core logic
    res_mat(:,1) = M(:,1,3) !The X-axis(distance vector) of the line profile is the Y-axis(3rd slice, all rows any single column) of the tensor
    X_dumm = M(1,:,2) !Subset the X-axis of the tensor, to choose which X-value will be selected to draw the vertical line through
    dummy1 = abs(X_dumm - v_val)
    !Get the index of the minimum value of dummy
    imin = minloc(dummy1)
    !Place the dataset from that index in the result matrix
    res_mat(:,2) = M(:,imin(1),1)

end function lin_prof_v

!Select a horizontal line profile out of the Tm,n,p tensor
function lin_prof_h(M, h_val) result(res_mat)
    real(real64) :: M(:,:,:), h_val !inputs declaration
    real(real64) :: res_mat(size(M,2), 2) !output declaration
    real(real64) :: Y_dumm(size(M,1)), dummy1(size(Y_dumm)) !Dummy prior = 1
    integer :: imin(1) !Dummy prior = 2

    !core logic
    res_mat(:,1) = M(1,:,2) !The X-axis(distance vector) of the line profile is the X-axis(2nd slice, all columns any single row) of the tensor
    Y_dumm = M(:,1,3) !Subset the Y-axis of the tensor, to choose which Y-value will be selected to draw a horizontal line through
    dummy1 = abs(Y_dumm - h_val)
    !Get the index of the minimum value of the dummy
    imin = minloc(dummy1)
    !Place the sataset from that index in the result matrix
    res_mat(:,2) = M(imin(1),:,1)
end function lin_prof_h

! Integrates a 1D function between X and Y within limits xmin and xmax
function integrate(X, Y, xmin, xmax, Riemann)
    real(real64), intent(in), dimension(:) :: X, Y
    real(real64) :: integrate
    real(real64), dimension(:), allocatable :: X_sub, Y_sub
    real(real64) :: xmin, xmax
    real(real64) :: sum
    logical, dimension(:), allocatable :: mask
    integer :: i
    logical, optional :: Riemann
    logical :: Riemann1
    mask = ( (X >= xmin) .and. (X <= xmax) )
    X_sub = pack(X, mask)
    Y_sub = pack(Y, mask)

    !Set Default Riemann argument to true
    if ( .not. present(Riemann) ) then
        Riemann1 = .true.
    else
        Riemann1 = Riemann
    end if

    !If Riemann Integratable
    if ( Riemann1 ) then
        sum = 0
        do i = 1, size(X_sub)-1
            sum = sum + Y_sub(i)*(X_sub(i+1)-X_sub(i))
        end do
        integrate = sum
        sum = 0
    else
        sum = 0
        do i = 1, size(X_sub)-1
            sum = sum + Y_sub(i)*(X_sub(i+1)-X_sub(i)) ! The Riemann rectangle
            sum = sum + 0.5*(Y_sub(i+1) - Y_sub(i))*(X_sub(i+1) - X_sub(i))
        end do
        integrate = sum
        sum = 0
    end if

end function integrate

!integrates a function to return a function
function integrate_function(X, Y, y0) result(dat)
    real(real64) :: X(:), Y(:) !Inputs
    real(real64) :: dat(size(X), 2) !Output
    real(real64) :: tri_area(size(X)), dmp, y0
    integer :: m, i
    m = size(X)
    !Calculate the area under each sample vector
    tri_area(1) = 0.0_real64
    dmp = 0.0_real64
    dat(1, 2) = 0.0_real64
    dat(:,1) = X
    do i = 2, m
        dmp = dmp + (X(i) - X(i-1))*(Y(i) + Y(i+1))/2.0_real64
        dat(i,2) = dmp
    end do
    !Add this to y0
    dat(:,2) = dat(:,2) + y0
end function integrate_function

!Creates a 2-slice X-Y tensor grid, given X and Y vectors
function grid_2(X, Y) result(tens)
    real(real64) :: X(:), Y(:) !Inputs
    real(real64) :: tens(size(Y), size(X), 2) !Outputs - Tensor size
    integer :: m, n
    m = size(X)
    n = size(Y)
    tens(:,:,1) = spread(X, 1, n)
    call rev(Y)
    tens(:,:,2) = spread(Y, 2, m)
end function grid_2

!Define a square lattice
function square_latt_sb(X, Y, R_latt, A, sig) result(tens)
  real(real64) :: X(:), Y(:), R_latt !Input
  real(real64) :: A, sig !Gaussian amplitude and sdev
  real(real64) :: tens(size(Y),size(X),3) !Output
  real(real64), allocatable :: tens_XY_00(:,:,:) !The same as tens with the X-Y grid in tens(:,:,:) set to X=0, Y=0, at the leftmost corner
  integer :: m_x, m_y
  integer :: i, j, k, cnt, dmp
  real(real64) :: xmin, xmax, ymin, ymax, gauss_dummy(size(Y),size(X)) ! gauss_dummy(size(Y),size(X),3)
  real(real64), allocatable  :: X_sh(:), Y_sh(:) !The shifted X and Y vectors
  real(real64) , allocatable :: pos(:,:)

  !error check
  if ( R_latt > maxval(X) - minval(X)  ) then
    error stop "R_latt too large for array"
  end if
  if ( R_latt > maxval(Y) - minval(Y) ) then
    error stop "R_latt too large for array"
  end if

  !allocate size of tens_XY_00 and build its X and Y and then shift the leftmost corner to (0,0)
  allocate(tens_XY_00(size(Y), size(X), 3))
  tens(:,:,2:3) = grid_2(X, Y)
  tens_XY_00(:,:,2:3) = tens(:,:,2:3)
  tens_XY_00(:,:,2) = tens_XY_00(:,:,2) - minval(X) !shift X to 0
  tens_XY_00(:,:,3) = tens_XY_00(:,:,3) - minval(Y) !shift y to 0
  tens_XY_00(:,:,1) = 0.0_real64

  !figure out the size of the loop and then the vector
  m_x = floor((maxval(X) - minval(X))/R_latt)
  m_y = floor((maxval(Y) - minval(Y))/R_latt)
  allocate(pos((m_x+1)*(m_y+1), 2)) !Debug vector. Remove after debug

  !Run the loop
  X_sh = X - minval(X)
  Y_sh = Y - minval(Y)
  cnt = 1 !New indexing integer
  do k = 0, m_x
    do j = 0, m_y
      gauss_dummy = gauss_2D_nocorr_core(X=tens_XY_00(:,:,2), Y=tens_XY_00(:,:,3), A = 1.0_real64, x0 = k*R_latt, y0 = j*R_latt, sig_x = sig, sig_y = sig)
      tens_XY_00(:,:,1) = tens_XY_00(:,:,1) + gauss_dummy
    end do
  end do

  tens(:,:,1) = tens_XY_00(:,:,1)
end function square_latt_sb

!Define a rectangular lattice
function rect_latt_sb(X, Y, R_latt_x, R_latt_y, A, sig) result(tens)
  real(real64) :: X(:), Y(:), R_latt_x, R_latt_y !Input
  real(real64) :: A, sig !Gaussian amplitude and sdev
  real(real64) :: tens(size(Y),size(X),3) !Output
  real(real64), allocatable :: tens_XY_00(:,:,:) !The same as tens with the X-Y grid in tens(:,:,:) set to X=0, Y=0, at the leftmost corner
  integer :: m_x, m_y
  integer :: i, j, k, cnt, dmp
  real(real64) :: xmin, xmax, ymin, ymax, gauss_dummy(size(Y),size(X)) ! gauss_dummy(size(Y),size(X),3)
  real(real64), allocatable  :: X_sh(:), Y_sh(:) !The shifted X and Y vectors
  real(real64) , allocatable :: pos(:,:)

  !error check
  if ( R_latt_x > maxval(X) - minval(X)  ) then
    error stop "R_latt too large for array"
  end if
  if ( R_latt_y > maxval(Y) - minval(Y) ) then
    error stop "R_latt too large for array"
  end if

  !allocate size of tens_XY_00 and build its X and Y and then shift the leftmost corner to (0,0)
  allocate(tens_XY_00(size(Y), size(X), 3))
  tens(:,:,2:3) = grid_2(X, Y)
  tens_XY_00(:,:,2:3) = tens(:,:,2:3)
  tens_XY_00(:,:,2) = tens_XY_00(:,:,2) - minval(X) !shift X to 0
  tens_XY_00(:,:,3) = tens_XY_00(:,:,3) - minval(Y) !shift y to 0
  tens_XY_00(:,:,1) = 0.0_real64

  !figure out the size of the loop and then the vector
  m_x = floor((maxval(X) - minval(X))/R_latt_x)
  m_y = floor((maxval(Y) - minval(Y))/R_latt_y)
  allocate(pos((m_x+1)*(m_y+1), 2)) !Debug vector. Remove after debug

  !Run the loop
  X_sh = X - minval(X)
  Y_sh = Y - minval(Y)
  cnt = 1 !New indexing integer
  do k = 0, m_x
    do j = 0, m_y
      gauss_dummy = gauss_2D_nocorr_core(X=tens_XY_00(:,:,2), Y=tens_XY_00(:,:,3), A = 1.0_real64, x0 = k*R_latt_x, y0 = j*R_latt_y, sig_x = sig, sig_y = sig)
      tens_XY_00(:,:,1) = tens_XY_00(:,:,1) + gauss_dummy
    end do
  end do

  tens(:,:,1) = tens_XY_00(:,:,1)
end function rect_latt_sb

!Define a zero pad function needed for fft_2D()
function zero_pad_tens(tens) result(res_tens)
    real(real64) :: tens(:,:,:)
    real(real64), allocatable :: res_tens(:,:,:)
    real(real64), allocatable :: X(:), Y(:), X_new(:), Y_new(:)
    real(real64), allocatable :: tens_dat(:,:), tens_X(:,:), tens_Y(:,:)
    real(real64) :: del_X, del_Y
    integer :: m, n, p, q

    X = tens(1,:,2)
    Y = tens(:,1,3)
    m = size(tens, 1)
    n = size(tens, 2)

    del_X = mean(diff(X))
    del_Y = mean(diff(Y))

    ! -----------------------------------
    ! Return the result if m = n else allocate variables and calculatr
    ! -----------------------------------
    if ( m > n ) then
        allocate(tens_dat(m,m))
        allocate(tens_X(m,m))
        allocate(tens_Y(m,m))
        allocate(res_tens(m,m,3))
        allocate(X_new(m))
        tens_Y = spread(Y, 2, m)
        X_new = seqn1(st = X(1), len = m, del = del_X)
        tens_X = spread(X_new, 1, m)
        tens_dat(:,1:n) = tens(:,:,1)
        tens_dat(:,(n+1):m) = 0.0_real64
        res_tens(:,:,1) = tens_dat
        res_tens(:,:,2) = tens_X
        res_tens(:,:,3) = tens_Y
        return
    else if (m == n) then
        res_tens = tens
        return
    else
        allocate(tens_dat(n,n))
        allocate(tens_X(n,n))
        allocate(tens_Y(n,n))
        allocate(res_tens(n,n,3))
        allocate(Y_new(n))
        tens_X = spread(X, 1, n)
        Y_new = seqn1(st = Y(1), len = n, del = del_Y) !Check this line !Maybe Y_new needs Y to be reversed
        tens_Y = spread(Y_new, 2, n)
        tens_dat(1:m,:) = tens(:,:,1)
        tens_dat((m+1):n,:) = 0.0_real64
        res_tens(:,:,1) = tens_dat
        res_tens(:,:,2) = tens_X
        res_tens(:,:,3) = tens_Y
        return
    end if
end function zero_pad_tens
!Define a hexagonal lattice

!Define a honeycomb lattice

!Define an arbitrary lattice

end module fortran_utils

