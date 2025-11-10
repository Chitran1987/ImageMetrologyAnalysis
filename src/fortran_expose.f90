module fortran_expose
use iso_fortran_env, only: real64
use fortran_utils
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

subroutine lin_prof_h_c(T, m, n, p, h_val, res) bind(C, name="lin_prof_h_c")
  integer :: m, n, p !Input sizes
  real(real64) :: T(m, n, p), h_val !Input arguments
  real(real64) :: res(size(T, 2), 2) !The "output" arguments
  res = lin_prof_h(M = T, h_val = h_val)
end subroutine lin_prof_h_c

subroutine lin_prof_v_c(T, m, n, p, v_val, res) bind(C, name="lin_prof_v_c")
  integer :: m, n, p !Input sizes
  real(real64) :: T(m,n,p), v_val !Input arguments
  real(real64) :: res(size(T, 1), 2) !The output arguments
  res = lin_prof_v(M = T, v_val = v_val)
end subroutine lin_prof_v_c


end module fortran_expose
