      PROGRAM PLOTHM
C
C     ------------------------------------------------------------
C
C     Sample driver program for plot routine PLTHM
C
C*  Version           0.9.5
C*  Revision          November 2005
C*  Latest Change     November 2005
C*  Library           CodeLib
C*  Code              Fortran 77, Double Precision
C*  Environment       Standard Fortran 77 environment on PC's,
C                     workstations and hosts.
C
C     ------------------------------------------------------------
C
      IMPLICIT NONE
      INTEGER N,I
      PARAMETER (N=6)
      INTEGER INCID(N),IOPT(20)
      REAL ROPT(10)
      CHARACTER PTEXT*20,DEVICE*20
C
      OPEN(2,FILE='alcon1.diag',STATUS='OLD',ERR=9990)
C
      DO I=1,N
        INCID(I) = 1
      ENDDO
      DO I=1,10
        ROPT(I) = 0.0
      ENDDO
      DO I=1,20
        IOPT(I) = 0
      ENDDO
      PTEXT = 'Sample plot'
      DEVICE = '?'
C   IOPT(1)  Marking of internal points (ityp=0-2)
      IOPT(1) = 2
C   IOPT(2)  Marking of turning points  (ityp=3)
      IOPT(2) = 1
C   IOPT(3)  Marking of external points (ityp=5)
      IOPT(3) = 1
C   IOPT(4)  Interpolation
      IOPT(4) = 1
C   IOPT(5)  Printing
      IOPT(5) = 1
C   IOPT(6)  Background/foreground color selection
      IOPT(6) = 1
C   IOPT(7)  Number of component plots in one row
      IOPT(7) = 3
C   ROPT(1)  The width of the plot window in cm (0.0 means device default)
      ROPT(1) = 0.0
C   ROPT(2)  The height of the plot window in cm (0.0 means same as ROPT(1))
      ROPT(2) = 0.0
C   ROPT(3)  Decreasing factor for the whole picture of one component
      ROPT(3) = 0.0
C   ROPT(4)  (TAUMIN) Minimum tau-value to be plotted (left margin value)
      ROPT(4) = 0.0
C   ROPT(5)  (TAUMAX) Maximum tau-value to be plotted (right margin value)
      ROPT(5) = 0.0
C
      CALL PLTHM(N,INCID,PTEXT,DEVICE,IOPT,ROPT)
      WRITE(6,*) 'Last clicked component is ',IOPT(20)
      STOP
9990  CONTINUE
      WRITE(6,*) '*** ERROR - could not open file alcon1.diag'
      WRITE(6,*) '*** Run alcon1 to generate this file'
      END
