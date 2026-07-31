module fortran_utils
use iso_fortran_env, only: real64
use linalg_solver
implicit none
public
private :: rev_cmplx, rev_real, rev_int
private :: print_mat_bool, print_mat_int, print_mat_real, print_mat_cmplx
private :: lineval_r, int_search, latt_R_convert

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

!function produces a sequence with start, end and length
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

!Masks a tensor preserving the values under the mask and zeroing out the others.
!Needed for the fft_2D_map() function
function mask_tens(tens, xlim, ylim) result(res_tens)

    !_____Input and Output arguments_________________________________!
    real(real64) :: tens(:,:,:), xlim(2), ylim(2) !Inputs
    real(real64) :: res_tens(size(tens, 1), size(tens, 2), size(tens, 3)) !Outputs
    !_____Input and Output arguments_________________________________!

    !_____Internal declarations_________________________________!
    logical :: mask(size(tens, 1), size(tens, 2)) !The dummy mask
    !_____Internal declarations_________________________________!

    !_____core logic_____________________________________________!
    mask = ((tens(:,:,2) >= xlim(1)) .and. (tens(:,:,2) <= xlim(2))) .and. ( (tens(:,:,3) >= ylim(1)) .and. (tens(:,:,3) <= ylim(2)) )
    res_tens = tens
    where ( mask .eqv. .false. )
        res_tens(:,:,1) = 0.0_real64
    end where
    return
    !_____core logic_____________________________________________!

end function mask_tens


!Masks a tensor preserving the values under the mask and zeroing out the others.
!Needed for the fft_2D_map() function
function mask_tens_cent(tens, cent, Xspan, Yspan) result(res_tens)
    real(real64) :: tens(:,:,:), cent(2), Xspan, Yspan !Inputs
    real(real64) :: res_tens(size(tens, 1), size(tens, 2), size(tens, 3)) !Outputs
    real(real64) :: x_lim(2), y_lim(2) !Internals
    x_lim = [cent(1)-Xspan/2.0_real64, cent(1)+Xspan/2.0_real64]
    y_lim = [cent(2)-Yspan/2.0_real64, cent(2)+Yspan/2.0_real64]
    res_tens = mask_tens(tens=tens, xlim=x_lim, ylim=y_lim)
    return
end function mask_tens_cent

!The one sided sigmoid function
pure elemental real(real64) function sigmoid(x, k, cutoff) result(y)
    real(real64), intent(in) :: x, k, cutoff !Input variables vector X, sharpness k, cutoff/fermi_level
    y = 1/(1 + exp(-k*(x-cutoff)))
    return
end function sigmoid

!The two sided sigmoid function or the sigmoid plateau function sigmoid_plat()
pure elemental real(real64) function sigmoid_plat(x, k, left_cut, right_cut) result(y)
    real(real64), intent(in) :: x, k, left_cut, right_cut
    Y = sigmoid(x=x, k=k, cutoff=left_cut) - sigmoid(x=x, k=k, cutoff=right_cut)
    return
end function sigmoid_plat

!function sigmoid_2D
pure elemental real(real64) function sigmoid_2D(x, y, k, x_lo, x_hi, y_lo, y_hi) result(z)
    real(real64), intent(in) :: x, y, k, x_lo, x_hi, y_lo, y_hi
    z = ( sigmoid_plat(x=x, k=k, left_cut=x_lo, right_cut=x_hi) )*( sigmoid_plat(x=y, k=k, left_cut=y_lo, right_cut=y_hi) )
    return
end function sigmoid_2D

!window_sigmoid()
function window_sigmoid(tens, cent, k, Xspan, Yspan) result(res_tens)
    real(real64) :: tens(:,:,:), cent(2)
    real(real64) :: res_tens(size(tens, 1), size(tens, 2), 3)
    real(real64) :: k, Xspan, Yspan
    real(real64) :: offset_X, offset_Y !Internal variable
    offset_X = Xspan/10.0_real64
    res_tens(:,:,2:3) = tens(:,:,2:3)
    res_tens(:,:,1) = sigmoid_2D(x=res_tens(:,:,2), y= res_tens(:,:,3), k=k, x_lo = cent(1)-Xspan/2.0_real64+offset_X, x_hi = cent(1)+Xspan/2.0_real64-offset_X, y_lo = cent(2)-Yspan/2.0_real64+offset_Y, y_hi = cent(2)+Yspan/2.0_real64-offset_Y)
    res_tens(:,:,1) = res_tens(:,:,1)*tens(:,:,1)
    return
end function window_sigmoid

!plot box 1
function plot_box1(img_tens, box_vec, box_if, box_thick) result(res_tens)
    !img_tens ----> The img over which boxes should be drawn
    !box_vec -----> The vector caarying the co-ordinates (x0, y0, x1, y1)
    !box_if ------> The intensity factor by which the box color > maxval(img_tens(:,:,1)) in grayscale
    real(real64) :: img_tens(:,:,:), box_vec(4), box_if
    real(real64) :: res_tens(size(img_tens, 1), size(img_tens, 2), size(img_tens, 3))
    real(real64) :: A(2), B(2), C(2), D(2)
    logical :: mask(size(img_tens, 1), size(img_tens, 2)) !Internal
    real(real64) :: box_thick, delta !Internal
    !-----------------------Core Logic-------------------------------------------------------------------------------------!
    res_tens = img_tens !pass the values to res_tens
    delta = box_thick
    !Transfer the values
    A = [box_vec(1), box_vec(2)]
    B = [box_vec(3), box_vec(2)]
    C = [box_vec(3), box_vec(4)]
    D = [box_vec(1), box_vec(4)]
    delta = 2.0_real64*mean(diff(img_tens(1,:,2)))
    !Mask and draw the box
    !Mask for X
    !Line AB Horizontal !delta used in Y
    mask = (res_tens(:,:,2) >= A(1)) .and. (res_tens(:,:,2) <= B(1)) .and. (res_tens(:,:,3) >= A(2)-delta) .and. (res_tens(:,:,3) <= A(2)+delta)
    where ( mask )
        res_tens(:,:,1) = box_if*maxval(img_tens(:,:,1))
    end where
    !Line BC Vertical !delta used in X
    mask = (res_tens(:,:,2) >= B(1)-delta) .and. (res_tens(:,:,2) <= C(1)+delta) .and. (res_tens(:,:,3) >= B(2)) .and. (res_tens(:,:,3) <= C(2))
    where ( mask )
        res_tens(:,:,1) = box_if*maxval(img_tens(:,:,1))
    end where
    !Line DC Horizontal !delta used in Y
    mask = (res_tens(:,:,2) >= D(1)) .and. (res_tens(:,:,2) <= C(1)) .and. ( res_tens(:,:,3) >= C(2)-delta) .and. (res_tens(:,:,3) <= D(2)+delta)
    where ( mask )
        res_tens(:,:,1) = box_if*maxval(img_tens(:,:,1))
    end where
    !Line AD Vertical !delta used in X
    mask = (res_tens(:,:,2) >= A(1)-delta) .and. (res_tens(:,:,2) <= D(1)+delta) .and. (res_tens(:,:,3) >= A(2)) .and. (res_tens(:,:,3) <= D(2))
    where ( mask )
        res_tens(:,:,1) = box_if*maxval(img_tens(:,:,1))
    end where
    return
end function plot_box1

function plot_boxes(img_tens, box_mat, box_thick, box_if) result(res_tens)
    real(real64) :: img_tens(:,:,:), box_mat(:,:), box_thick, box_if !Input arguments
    real(real64) :: res_tens(size(img_tens, 1), size(img_tens, 2), 3) !Output declaration
    integer :: n_boxes !Internal ---> No of boxes
    integer :: i !Internal
    !-----------core logic---------------------------!
    res_tens = img_tens
    n_boxes = size(box_mat, 1)
    do i = 1, n_boxes
        res_tens = plot_box1(img_tens = res_tens, box_vec = box_mat(i,:), box_thick = box_thick, box_if = box_if)
    end do
    !-----------core logic---------------------------!
    return
end function plot_boxes

!Mask the (0,0) spot using boxes
function mask_box(tens, box_vec) result(res_tens)
    real(real64) :: tens(:,:,:), box_vec(4) !Inputs
    real(real64) :: res_tens(size(tens, 1), size(tens, 2), 3) !Output

    !----------Internal-------------------------------!
    logical :: mask(size(tens, 1), size(tens, 2))
    real(real64) :: x_LL, y_LL, x_UR, y_UR

    !----------Internal-------------------------------!
    !----------core logic-----------------------------!
    res_tens = tens !Transfer data to res_tens
    x_LL = box_vec(1)
    y_LL = box_vec(2)
    x_UR = box_vec(3)
    y_UR = box_vec(4)
    mask = (tens(:,:,2) >= x_LL) .and. (tens(:,:,2) <= x_UR) .and. (tens(:,:,3) >= y_LL) .and. (tens(:,:,3) <= y_UR)
    where(mask)
        res_tens(:,:,1) = minval(res_tens(:,:,1))
    end where
    !----------core logic-----------------------------!
    return
end function mask_box
!Define a hexagonal lattice

!Define a honeycomb lattice

!Private procedure to this module
!Creates a matrix of integers such that m,n,p,q = -int_range to +int_range
!Ensures that all combinations of m,n,p,q exist in a matrix where
!.....1st column are values of m, 2nd col->n, 3rd col->p, 4th col->q
!5th col -> mq-np
!Hence, returns a (2*int_range + 1) row, 5 column matrix
!Used in functions local to module ---> private
function int_search(int_range) result(list_mat)
    integer, intent(in) :: int_range
    integer :: list_mat1((2*int_range+1)**4, 5)
    integer, allocatable :: list_mat(:,:)
    integer :: m, n, p, q
    integer :: dumm, cnt, len
    logical :: mask((2*int_range+1)**4)
    !code
    m = int_range
    cnt = 1
    do m = -int_range, int_range
        do n = -int_range, int_range
            do p = -int_range, int_range
                do q = -int_range, int_range
                    dumm = m*q - n*p
                    list_mat1(cnt,1) = m
                    list_mat1(cnt, 2) = n
                    list_mat1(cnt, 3) = p
                    list_mat1(cnt, 4) = q
                    list_mat1(cnt, 5) = dumm
                    cnt = cnt+1
                end do
            end do
        end do
    end do
    mask = abs(list_mat1(:,5)) == 1
    len = count(mask)
    allocate(list_mat(len,4))
    list_mat(:,1) = pack(list_mat1(:,1), mask)
    list_mat(:,2) = pack(list_mat1(:,2), mask)
    list_mat(:,3) = pack(list_mat1(:,3), mask)
    list_mat(:,4) = pack(list_mat1(:,4), mask)
    return
end function int_search

!Private procedure to this module
!Converts provided lattices R1 and R2 to 1st quadrant R1 and R2
!Returns data in a matrix M
!R1 is the 1st column, R2 is the 2nd column
function latt_R_convert(R1, R2, search_rad) result(col_R1_R2)
        !Inputs
        real(real64), intent(in) :: R1(2), R2(2)
        integer, intent(in) :: search_rad
        !Outputs
        real(real64) :: col_R1_R2(2,2)
        !Internals
        real(real64) :: latt_mat(2,2), det_latt
        integer, allocatable :: M1(:,:) !M2(:,:)
        real(real64) :: S1(2), S2(2)
        integer :: m, n, p, q
        !logical, allocatable :: mask(:)
        integer :: i !mask_true
        !Error check
        latt_mat(:,1) = R1
        latt_mat(:,2) = R2
        det_latt = latt_mat(1,1)*latt_mat(2,2) - latt_mat(1,2)*latt_mat(2,1)
        if ( abs(det_latt) <= 1.0e-10_real64 ) then
            error stop "R1 and R2 are linearly dependent"
        end if
        M1 = int_search(int_range = search_rad)
        do i = 1, size(M1(:,1))
            !define m, n, p, q
            m = M1(i,1)
            n = M1(i,2)
            p = M1(i,3)
            q = M1(i,4)
            S1 = m*R1 + n*R2
            S2 = p*R1 + q*R2
            if ( S1(1) > 0 .and. S1(2) > 0 .and. S2(1) > 0 .and. S2(2) > 0 ) then
                col_R1_R2(:,1) = S1
                col_R1_R2(:,2) = S2
                return
            end if
        end do
        error stop "No first-quadrant primitive basis found. Increase search_rad or provide a manually reoriented unit cell."
end function latt_R_convert

!create a latt_pnt_lst() function
!The function should return a list of lattice points of m*R1+n*R2 type in the X-Y space
!shft = 0 means that once the lattice points are created
    !!There shouldat least be one lattice point present at the bottom left corner (X_min, Y_min)
    !!shft /= 0 means one latt point at (X_min+shft(1), Y_min+shft(2))
function latt_pnt_lst(X, Y, R1, R2, shft) result(lst_mat)
    real(real64) :: R1(2), R2(2), shft(2) !Inputs
    real(real64), intent(in) :: X(:), Y(:)
    real(real64), allocatable :: lst_mat(:,:) !Outputs
    real(real64) :: R1R2_mat(2,2), X1(size(X)), Y1(size(Y)) !Internals
    real(real64) :: R1_int(2), R2_int(2), lenX, lenY, t1, t2, p(2) !Internals
    real(real64) :: V_low(2), V_up(2) !Internals
    real(real64) :: distX, distY, dist_R1, dist_R2, vec(2), c1, c2, min_vec(2)  !Internals
    real(real64), allocatable :: M1(:,:), Mx(:,:), My(:,:)  !Internals
    real(real64), allocatable :: dis_small(:) !Internals
    integer :: r, s, cnt, i, j, k, shft_idx !Internals
    logical, allocatable :: mask_X(:), mask_Y(:)  !Internals

    !code----------------------------------------------------------------
    !!!swap R1 and R2 if necessary----------
    X1 = X - minval(X)
    Y1 = Y - minval(Y)
    !Get R1 and R2 into first quadrants
    R1R2_mat = latt_R_convert(R1, R2, search_rad=5)
    R1_int = R1R2_mat(:,1)
    R2_int = R1R2_mat(:,2)
    !swap R1_int and R2_int if necessary
    if ( atan(R1_int(2)/R1_int(1)) > atan(R2_int(2)/R2_int(1)) ) then
        R1R2_mat(:,1) = R2_int
        R1R2_mat(:,2) = R1_int
    end if
    R1 = R1R2_mat(:,1)
    R2 = R1R2_mat(:,2)
    !!!Calculate the skewed shape--------------
    lenX = maxval(X1) - minval(X1)
    lenY = maxval(Y1) - minval(Y1)
    t1 = R1(2)/R1(1)
    t2 = R2(2)/R2(1)
    c1 = -(1.5_real64*lenX*t1 + 0.5_real64*lenY)
    c2 =  0.5_real64*lenX*t2 + 1.5_real64*lenY
    p(1) = -(c2 - c1)/(t2 - t1)
    p(2) = (t2*c1 - t1*c2)/(t2 - t1)
    V_low = [1.5_real64*lenX, -0.5_real64*lenY]
    V_up = [-0.5_real64*lenX, 1.5_real64*lenY]
    distX = sqrt(sum((p - V_low)*(p - V_low)))
    distY = sqrt(sum((p - V_up)*(p - V_up)))
    dist_R1 = sqrt( sum(R1*R1) )
    dist_R2 = sqrt( sum(R2*R2) )
    r = ceiling(distX/dist_R1)
    s = ceiling(distY/dist_R2)
    allocate(M1((r+1)*(s+1),2))
    vec = p
    cnt = 1
    !!! Populate the skewed shape-------------------------
    do i = 0, r
        do j = 0, s
            vec = p + i*R1 + j*R2
            M1(cnt,:) = vec
            cnt = cnt + 1
        end do
    end do
    !!!Snip the skewed shape to a large rectangle -----------------------------
    mask_X = ( M1(:,1)>=-0.5_real64*lenX ) .and. ( M1(:,1) <= 1.5_real64*lenX )
    allocate(Mx(count(mask_X),2))
    Mx(:,1) = pack(M1(:,1), mask_X)
    Mx(:,2) = pack(M1(:,2), mask_X)
    !Now Mx is succesfully populated
    !redefine the mask
    mask_Y = (Mx(:,2)>=-0.5_real64*lenY) .and. (Mx(:,2) <= 1.5_real64*lenY)
    allocate(My(count(mask_Y),2))
    My(:,1) = pack(Mx(:,1), mask_Y)
    My(:,2) = pack(Mx(:,2), mask_Y)
    !Now that My is the final list first stage before shifting
    lst_mat = My
    !!!Shift the list of point accordingly
    !calculate the distance of each point
    !from the lower left corner of the image
    !Then take the pt corresponding to the least distance
    !shift the entire image by the shift of the corresponding point
    !to the co-ordinates of the lower-left image corner
    dis_small = sqrt(lst_mat(:,1)**2 + lst_mat(:,2)**2)
    shft_idx = minloc(dis_small, dim=1)
    min_vec = lst_mat(shft_idx,:)
    do k = 1, size(lst_mat(:,1))
        lst_mat(k,:) = lst_mat(k,:) - min_vec
    end do
    !Then shift the entire bunch by shft
    !Then shift it by the actual upper and lower corners of X and Y
    do k = 1, size(lst_mat(:,1))
        lst_mat(k,:) = lst_mat(k,:) + shft + [minval(X), minval(Y)] !shift by shft
    end do
    !Then shift it by
    !Then return
    return
end function latt_pnt_lst

!Populate a lattice given The X, Y and lattice point list
!Returns a 3 dimensional tensor for it
function populate_latt(X, Y, A, sig, list) result(tens)
    real(real64), intent(in) :: X(:), Y(:)
    real(real64), intent(in) :: list(:,:)
    real(real64), intent(in) :: A, sig

    real(real64) :: tens(size(Y), size(X), 3)

    real(real64) :: gauss_dumm(size(Y), size(X))
    real(real64) :: pop_sum(size(Y), size(X))
    real(real64) :: Y_local(size(Y))

    integer :: i

    ! grid_2 reverses its Y argument in place,
    ! so give it a local copy.
    Y_local = Y

    ! Generate coordinate grids before entering OpenMP.
    tens(:,:,2:3) = grid_2(X, Y_local)

    pop_sum = 0.0_real64

    !$omp parallel do default(none) schedule(static)             &
    !$omp& shared(tens, list, A, sig)                            &
    !$omp& private(gauss_dumm) reduction(+:pop_sum)
    do i = 1, size(list, 1)
        gauss_dumm = gauss_2D_nocorr_core(X=tens(:,:,2), Y=tens(:,:,3), A=A, x0=list(i,1), y0=list(i,2), sig_x=sig, sig_y=sig)
        pop_sum = pop_sum + gauss_dumm
    end do
    !$omp end parallel do
    tens(:,:,1) = pop_sum
    return
end function populate_latt



end module fortran_utils

