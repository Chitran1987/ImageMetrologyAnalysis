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
  min_zero = logical(min_zero)
  res = lin_bg_sub_1D(dat, win, min_zero)
end subroutine lin_bg_sub_1D_c



end module fortran_expose
