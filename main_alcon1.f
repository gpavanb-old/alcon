      PROGRAM MAINA1
C
C     ------------------------------------------------------------
C
C     Testexample for ALCON1
C
C*  Purpose           Testexample for code ALCON1
C*  Version           0.9
C*  Revision          September 1985
C*  Latest Change     January 1991
C*  Library           CodeLib
C*  Code              Fortran 77, Double Precision
C*  Environment       Standard Fortran 77 environment on PC's,
C                     workstations and hosts.
C
C     ------------------------------------------------------------
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION X(7),XW(7),RWORK(5000),IWORK(1000),INFO(9)
      EXTERNAL FTEST
      INTEGER UPR,UDIAG
      COMMON /UNIT/ UPR,UDIAG
      UPR=6
      UDIAG=2
      OPEN(2,FILE='alcon1.diag')
      LRW=5000
      LIW=1000
      N=6
      X(1)=0.D0
      X(2)=0.D0
      X(3)=0.D0
      X(4)=0.D0
      X(5)=0.D0
      X(6)=0.D0
      XW(1)=1.D0
      XW(2)=1.D0
      XW(3)=1.D0
      XW(4)=1.D0
      XW(5)=1.D0
      XW(6)=1.D0
      TAU=0.D0
      TAUMIN=0.D0
      TAUMAX=1.D0
      EPS=1.D-5
      INFO(1)=2
      INFO(2)=99
      INFO(3)=1
      INFO(4)=1
      CALL ALCON1(FTEST,N,X,XW,TAU,TAUMIN,TAUMAX,EPS,INFO,
     &   RWORK,LRW,IWORK,LIW)
      STOP
      END
      SUBROUTINE FTEST(X,TAU,F)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DOUBLE PRECISION X(6),F(6)
      DIODE(U)=5.6D-8*(DEXP(25.D0*U)-1.D0)
      AMPLIF(U)=7.65D0*DATAN(1962.D0*U)
      F(1)=(X(1)-X(3))/10000.D0+(X(1)-X(2))/39.D0+(X(1)+TAU)/51.D0
      F(2)=(X(2)-X(6))/10.D0+(X(2)-X(1))/39.D0+DIODE(X(2))
      F(3)=(X(3)-X(1))/10000.D0+(X(3)-X(4))/25.5D0
      F(4)=(X(4)-X(3))/25.5D0+(X(4)-X(5))+X(4)/0.62D0
      F(5)=(X(5)-X(6))/13.D0+(X(5)-X(4))+DIODE(X(5))
      F(6)=(X(6)-X(2))/10.D0+(X(6)-X(5))/13.D0+
     &       (X(6)-AMPLIF(X(3)-X(1)))/0.201D0
      RETURN
      END
