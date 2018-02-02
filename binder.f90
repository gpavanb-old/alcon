SUBROUTINE c_alcon1(c_f,len,x,scale,tau_start,tau_begin,tau_end,eps,info,rwork,lrwork,iwork,liwork) BIND(C, NAME='c_alcon1')
  USE, INTRINSIC :: iso_c_binding, ONLY : c_int, c_float
  INTERFACE
    SUBROUTINE c_f(x,tau,rhs) BIND(C)
      USE, INTRINSIC :: iso_c_binding, ONLY : c_int, c_float
      REAL(c_float) :: x(:)
      REAL(c_float) :: tau
      REAL(c_float) :: rhs(:)
    END SUBROUTINE c_f
  END INTERFACE
  INTEGER(c_int) :: len
  REAL(c_float) :: x(len)
  REAL(c_float) :: scale
  REAL(c_float) :: tau_start
  REAL(c_float) :: tau_begin
  REAL(c_float) :: tau_end
  REAL(c_float) :: eps
  INTEGER(c_int) :: info(9)
  INTEGER(c_int) :: lrwork
  REAL(c_float) :: rwork(lrwork)
  INTEGER(c_int) :: liwork
  REAL(c_float) :: iwork(liwork)
  CALL alcon1(c_f,len,x,scale,tau_start,tau_begin,tau_end,eps,info,rwork,lrwork,iwork,liwork)
END SUBROUTINE c_alcon1
