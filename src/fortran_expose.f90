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


end module fortran_expose
