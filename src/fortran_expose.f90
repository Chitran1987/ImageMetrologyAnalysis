module fortran_expose
use iso_fortran_env, only: real64
use fortran_utils
use fft_utils
implicit none
public

contains
subroutine lin_bg_sub_1D_c(dat, dat_m, dat_n, win, win_m, win_n, res, min_zero) bind(C, name="lin_bg_sub_1D_c")
  integer :: dat_m, dat_n, win_m, win_n
  real(real64) :: dat(dat_m, dat_n), win(win_m, win_n), res(dat_m, dat_n)
  integer :: min_zero
  logical :: min_zero1
  !Typecast min_zero to min_zero1
    if ( min_zero == 1 ) then
        min_zero1 = .true.
    else
        min_zero1 = .false.
    end if
  res = lin_bg_sub_1D(dat, win, min_zero1)
end subroutine lin_bg_sub_1D_c

subroutine gauss_1D_c(x, x_len, y, A, x0, sig) bind(C, name="gauss_1D_c")
  integer :: x_len
  real(real64) :: x(x_len), y(x_len)
  real(real64) :: A, x0, sig
  y = gauss_1D(x, A, x0, sig)
end subroutine gauss_1D_c

subroutine gauss_2D_nocorr_c(X, Y, x_len, y_len, Ax, Ay, x0, y0, sig_x, sig_y, tens) bind(C, name='gauss_2D_nocorr_c')
  integer :: x_len, y_len
  real(real64), intent(in) :: X(x_len), Y(y_len), Ax, Ay, x0, y0, sig_x, sig_y !The main inputs
  !real(real64) :: Ax, Ay, x0, y0, sig_x, sig_y !The optional inputs
  real(real64) :: tens(y_len, x_len, 3) !The output
  tens = gauss_2D_nocorr(X, Y, Ax, Ay, x0, y0, sig_x, sig_y)
end subroutine gauss_2D_nocorr_c

subroutine lin_prof_h_c(Tens, m, n, p, h_val, res) bind(C, name="lin_prof_h_c")
  integer :: m, n, p !Input sizes
  real(real64) :: Tens(m, n, p), h_val !Input arguments
  real(real64) :: res(size(Tens, 2), 2) !The "output" arguments
  res = lin_prof_h(Tens, h_val)
end subroutine lin_prof_h_c

subroutine lin_prof_v_c(Tens, m, n, p, v_val, res) bind(C, name="lin_prof_v_c")
  integer :: m, n, p !Input sizes
  real(real64) :: Tens(m,n,p), v_val !Input arguments
  real(real64) :: res(size(Tens, 1), 2) !The output arguments
  res = lin_prof_v(M = Tens, v_val = v_val)
end subroutine lin_prof_v_c

subroutine integrate_c(X, Y, m, xmin, xmax, Riemann, int_val) bind(C, name="integrate_c")
  integer :: m !X, Y sizes
  real(real64) :: X(m), Y(m), xmin, xmax, int_val
  logical :: Riemann1
  integer :: Riemann
  if (Riemann == 0) then
    Riemann1 = .false.
  else
    Riemann1 = .true.
  end if
  int_val = integrate(X, Y, xmin, xmax, Riemann1 )
end subroutine integrate_c

subroutine integrate_function_c(X, Y, m, y0, dat) bind(C, name="integrate_function_c")
  integer :: m
  real(real64) :: X(m), Y(m), dat(m,2), y0
  dat = integrate_function(X, Y, y0)
end subroutine integrate_function_c

subroutine fft_1D_c(X, Y, m, res) bind(C, name="fft_1D_c")
  integer :: m
  real(real64) :: X(m), Y(m), res(m+1, 2, 2)
  res = fft_1D(X, Y)
end subroutine fft_1D_c

subroutine fft_2D_c(Tens, m, n, p, Transform, wm, wn, wp) bind(C, name='fft_2D_c')
  integer m, n, p, wm, wn, wp
  real(real64) :: Tens(m,n,p), Transform(wm, wn, wp)
  Transform = fft_2D(Tens)
end subroutine fft_2D_c

subroutine grid_2_c(X, Y, mx, my, tens) bind(C, name = 'grid_2_c')
  integer :: mx, my
  real(real64) :: X(mx), Y(my) !Inputs
  real(real64) :: tens(my, mx, 2) !Outputs - Tensor size
  tens = grid_2(X, Y)
end subroutine
end module fortran_expose
