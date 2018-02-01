      SUBROUTINE PLTHM(N,INCID,PTEXT,DEVICE,IOPT,ROPT)
      IMPLICIT NONE
C*    Begin Prologue PLTHM
      INTEGER N
      INTEGER INCID(N),IOPT(*)
      REAL TAUMIN,TAUMAX,ROPT(*)
      CHARACTER*(*) PTEXT,DEVICE
C
C---------------------------------------------------------------------
C
C*  Title
C
C         (PLO)T (Hom)otopy.
C          Routine to be used in connection with the continuation
C          routine ALCON1 or ALCON2.
C
C*  Written by        P. Kunkel, L. Weimann
C*  Purpose           Plot bifurcation diagrams computed by ALCON1 or
C                     ALCON2.
C*  Method            Uses PGPLOT graphics software
C*  Category          F4 - Parameter Dependent Nonlinear Equation 
C                          Systems
C*  Keywords          Numerical pathfollowing, Homotopy Method
C*  Version           0.9.5
C*  Revision          November 2005
C*  Latest Change     November 2005
C*  Library           CodeLib
C*  Code              Fortran 77, Double Precision
C*  Environment       Standard Fortran 77 environment on PC's,
C                     workstations and hosts.
C*  Copyright     (c) Konrad-Zuse-Zentrum fuer
C                     Informationstechnik Berlin (ZIB)
C                     Takustrasse 7, D-14195 Berlin-Dahlem
C                     phone : + 49/30/84185-0
C                     fax   : + 49/30/84185-125
C*  Contact           Bodo Erdmann
C                     ZIB, Division Scientific Computing, 
C                          Department Numerical Analysis and Modelling
C                     phone : + 49/30/84185-185
C                     fax   : + 49/30/84185-107
C                     e-mail: erdmann@zib.de
C
C  ---------------------------------------------------------------
C
C* Licence
C    You may use or modify this code for your own non-commercial
C    purposes for an unlimited time.
C    In any case you should not deliver this code without a special
C    permission of ZIB.
C    In case you intend to use the code commercially, we oblige you
C    to sign an according licence agreement with ZIB.
C
C* Warranty
C    This code has been tested up to a certain level. Defects and
C    weaknesses, which may be included in the code, do not establish
C    any warranties by ZIB. ZIB does not take over any liabilities
C    which may follow from aquisition or application of this code.
C
C* Software status
C    This code is under partial care of ZIB and belongs to ZIB
C    software class 2.
C
C     ------------------------------------------------------------
C
C  Parameters
C
C  N       Number of components  (maximum value allowed by dimension  99
C
C  INCID   Incidence vector monitoring which components have to be plott
C            0  No plotting of component
C            1  Component is to be plotted
C
C  PTEXT   Text to be plotted  (character variable - maximum number of
C          Characters  20)
C
C  DEVICE   The plot-output device to be passed to the pgopen routine
C           (a character variable)
C           Special values:
C           '-'   If any device is already open, this one is used,
C                 otherwise PGOPEN is called with the parameter value '?'
C           '?'   Causes PGOPEN to prompt the user for the device
C
C  NOTE: A preopened device is not automatically closed before leaving
C        PLTHM
C
C  IOPT(*) Integer options (Integer array, minimum size is 20)
C   IOPT(1)  Marking of internal points (ityp=0-2)
C            0  No marking
C            1  Marking of bifurcation points
C            2  Marking of all internal points
C   IOPT(2)  Marking of turning points  (ityp=3)
C            0  No marking
C            1  Marking of turning points
C   IOPT(3)  Marking of external points (ityp=5)
C            0  no marking
C            1  marking of external points
C   IOPT(4)  Interpolation
C            0  Linear
C            1  Cubic hermite
C   IOPT(5)  Printing of interpolation variables
C            0  No printing
C            1  Printing of some plot device and viewports information
C            2  Additional printing of interpolation related information
C   IOPT(6)  Background/foreground color selection
C            0      White lines are plotted on a black background
C            other  Black lines are plotted on a white background
C   IOPT(7)  Number of component plots in one row
C            0      The number is determined by ROPT(3) and the sizes
C                   of the maximum plottable area.
C            1..4   A valid number of component plots in one row
C                   (ROPT(3) is ignored in this case)
C            other  Invalid value
C   IOPT(8)  (LMODE)  Logarithmic scale for state variables
C            0        false, e.g. linear scaling
C            other    true, e.g. logarithmic scaling
C   IOPT(9)  (UPR)    Print unit
C            0        Use standard unit 6
C            1..99    Use IOPT(9) is print unit 
C   IOPT(10) (UDIAG)  Plot data unit ( IOPT(10).NE.IOPT(9) )
C            0        Use as standard unit 2
C            1..99    Use IOPT(10) is plot data unit 
C                     Must be opened prior calling PLTHM!
C   IOPT(11..19)  Reserved
C   IOPT(20) (output) Plotted component, which has been last clicked on
C                     the value is only set, if the graphics device has
C                     a cursor
C             
C  ROPT(*) Real options (real array, minimum size is 10)
C   ROPT(1)  The width of the plot window in cm
C              if ROPT(1).LE.0.0, the (device dependent) 
C              default value is used, and ROPT(2) is ignored
C   ROPT(2)  The height of the plot window in cm
C              if ROPT(2).LE.0.0 and ROPT(1).GT.0.0,
C              ROPT(2)=ROPT(1) is used
C   ROPT(3)  Decreasing factor for the whole picture of one component
C              (0.25 <= ROPT(3) <= 1.0)
C              If ROPT(3).EQ.0.0, ROPT(3)=1.0 is set
C   ROPT(4)  (TAUMIN) Minimum tau-value to be plotted (left margin value)
C   ROPT(5)  (TAUMAX) Maximum tau-value to be plotted (right margin value)
C   ROPT(6)  (SIGMA)  Plot accuracy (standard value 0.01)
C   ROPT(7)  (THR)    Threshold for switching to linear interpolation
C              in case of cubic hermite interpolation being not
C              suited in the corresponding interval
C              (Note that for each interpolation instruction
C              there is data such that that kind of interpolation
C              is not satisfactory)
C              (Standard value 0.5)
C
C  NOTE: If ROPT(4).GE.ROPT(5) is specified, TAUMIN and TAUMAX will
C        be determined from the extremal values of the data read
C        from unit UDIAG
C
C  Types of points distinguished by PLTHM
C  The types 0-3 are used by all ALCON routines
C  The types 0-4 require information as supported by the alcon routines
C  The types 5-6 only require values (x,tau)
C  The type  7   is used by ALCONB
C
C  ITYP=0  Standard point       symbol  3
C  ITYP=1  Bifurcation point    symbol  6
C  ITYP=2  Offset               symbol  3
C  ITYP=3  Turning point        symbol  5
C  ITYP=4  Interpolation point  no symbol
C  ITYP=5  External point       symbol  4
C  ITYP=6  Scaling point        no symbol
C  ITYP=7  B-point              symbol  5
C          Stable b-point   add symbol  7
C
C  Plot software needed: PGPLOT
C  PGPLOT is available from http://www.astro.caltech.edu/~tjp/pgplot/
C
C---------------------------------------------------------------------
C*    End Prologue
      INTEGER MAXN1
      PARAMETER (MAXN1=999)
      INTEGER MARK,TURN,EXT,INTER,KPRINT
      REAL X(MAXN1),XW(MAXN1),DX(MAXN1)
      INTEGER ICOMP(MAXN1)
      INTEGER I,IB,IC,ICLOSE,ICO,IDEV,IF,IH,IHOM,IHOMA,IJ,IP,IPOINT,IS,
     1        ITYP,IVCLCK,J,JQ,L,LENGTH,LH,MH,MODEVP,N1,NH,NH1,NVP,
     2        UPR,UDIAG
      REAL    A,AH,B,BH,C,CH,DELTAS,DELTAT,DELTAX,F0,F0H,F0P,F0PH,F1,
     1        F1H,F1P,F1PH,H,HMAX,HPOINT,SIG0,SIGMA,SIGMAX,SMALL,
     2        SMALIN,TAU,TAUMN2,TAUMX2,THR,X0,X1,XH,XIMIN,XIMAX,XP,
     3        XPQ,YH,YP,YPQ,Z
      REAL XSZ,XFR(5),YFR(5),XFR2(5),YFR2(5)
      REAL DHEIGH,DWIDTH,DSZ,FACT,FACTV,FACTX,FACTY,XL,XU,YL,YU
      LOGICAL LMODE,INIT,NOREST,INBOX,QCURS
      CHARACTER CHAR*1,CI*2,CVAL*3
      INTEGER PGOPEN
      EXTERNAL PGOPEN
C
      DATA XFR  /-1.2,11.0,11.0,-1.2,-1.2/,
     1     YFR  /-1.2,-1.2,11.0,11.0,-1.2/,
     2     XFR2 /-3.0,15.0,15.0,-3.0,-3.0/,
     3     YFR2 /-3.0,-3.0,15.0,15.0,-3.0/
C
      INBOX=.FALSE.
      SMALL = 1.0E-5
      SMALIN = 1.0E0 / SMALL
      N1=N+1
      IF (N1.GT.MAXN1) THEN
        WRITE(UPR,82050) N,N1
        RETURN
      ENDIF
C     Get and check plot options
      MARK   = IOPT(1)
      TURN   = IOPT(2)
      EXT    = IOPT(3)
      INTER  = IOPT(4)
      KPRINT = IOPT(5)
C     IOPT(6) see below
      NH = IOPT(7)
      IF (NH.LT.0 .OR. NH.GT.4) THEN
        WRITE(UPR,*) 'No. component plots IOPT(7) out of allowed range'
        RETURN
      ENDIF
      LMODE = IOPT(8).NE.0
      UPR = IOPT(9)
      IF (UPR.LE.0.OR.UPR.GT.99) UPR=6
      UDIAG=IOPT(10)
      IF (UDIAG.LE.0.OR.UDIAG.GT.99) UDIAG=2
      FACT = ROPT(3)
      IF (FACT.EQ.0.0) FACT = 1.0
      IF (FACT.LT.0.25 .OR. FACT.GT.1.0) THEN
        WRITE(UPR,*) 'Decreasing factor ROPT(3) out of allowed range'
        RETURN
      ENDIF
      TAUMIN = ROPT(4)
      TAUMAX = ROPT(5)
      SIGMA = ROPT(6)
      IF (SIGMA.LE.0.0) SIGMA=0.01
      THR = ROPT(7)
      IF (THR.LE.0.0) THR=0.5
C     Open and resize plot device - use current open device, if any is
      ICLOSE = 0
      IDEV   = 0
      IF (DEVICE(1:2).EQ.'- ') THEN
        CALL PGQID(IDEV)
        DEVICE = '?'
      ENDIF
      IF (IDEV.EQ.0) THEN
        IDEV = PGOPEN(DEVICE)
        IF (IDEV.LE.0) THEN
          WRITE(UPR,82051) DEVICE,IDEV
          RETURN
        ENDIF
        ICLOSE=1
      ENDIF
      CALL PGASK(.FALSE.)
      CALL PGQINF('CURSOR',CVAL,LH)
      QCURS = CVAL.EQ.'YES'
      DWIDTH = ROPT(1)
      IF (DWIDTH.GT.0.0) THEN
        DWIDTH = DWIDTH/2.54
        DHEIGH = ROPT(2)/2.54
        IF (DHEIGH.LE.0.0) DHEIGH=DWIDTH
        CALL PGPAP(DWIDTH,DHEIGH/DWIDTH)
      ELSE
        CALL PGQVSZ(1,XH,DWIDTH,YH,DHEIGH)
      ENDIF
C     Compute number and characteristics of viewports
      DSZ = MIN(DWIDTH,DHEIGH)
      IF (NH.EQ.0) THEN
        MODEVP = 0
        NH1 = INT(1.0/FACT)
        NH = MAX(NH1,INT(DWIDTH/DHEIGH))
        IF (NH.GT.NH1.AND.KPRINT.GE.1) WRITE(UPR,82002) NH
      ELSE
        MODEVP = 1
        FACT = 1.0/FLOAT(NH)
        IF (DWIDTH.GT.DHEIGH) FACT = MIN(DWIDTH/DHEIGH*FACT,1.0)
      ENDIF
      FACTV = FACT*DSZ
      IF (DHEIGH.EQ.DWIDTH) THEN
        MH = NH
      ELSE IF (DWIDTH.LE.DHEIGH .OR. MODEVP.EQ.1) THEN
        MH = INT(DHEIGH/FACTV)
      ELSE
        MH = NH
        NH = INT(DWIDTH/FACTV)
      ENDIF
      NVP = MH*NH
      FACTX = DWIDTH/FLOAT(NH)
      FACTY = DHEIGH/FLOAT(MH)
      IF (KPRINT.GE.1) 
     1   WRITE(UPR,82001) DWIDTH,DHEIGH,FACTX,FACTY,FACTV,NH,MH
C     Set background/foreground colors (black and white)
      IF (IOPT(6).EQ.0) THEN
        IC=0
      ELSE
        IC=1
      ENDIF
      CALL PGSCR(IC,0.0,0.0,0.0)
      CALL PGSCR(1-IC,1.0,1.0,1.0)
C
      SIGMAX=0.0
      NOREST=.FALSE.
      IF (TAUMAX.LE.TAUMIN) NOREST=.TRUE.
      IH = 0
C  Loop over all components
      DO I=1,N
      IF (INCID(I).EQ.1) THEN
        INIT=.FALSE.
C    Loop over all data
        DO J=1,9999
          READ(UDIAG,80001,END=1999) ITYP,IB,IF,IS,SIG0
          IF (ITYP.LE.4)
     &      READ(UDIAG,80002)(X(L),L=1,N1),(DX(L),L=1,N1),(XW(L),L=1,N1)
          IF (ITYP.GE.5) READ(UDIAG,80002) (X(L),L=1,N1)
          TAU=X(N1)
C    Compute scaling minimum and maximum
          IF (NOREST .OR. ( TAU.GE.TAUMIN .AND. TAU.LE.TAUMAX )) THEN
            JQ=J
            IF (LMODE) THEN
              DO L=1,N
                X(L)=ALOG10(X(L))
              ENDDO
            END IF
            IF (ITYP.NE.7) IHOM=IB
            IF (.NOT.INIT) THEN
              TAUMX2=TAU
              TAUMN2=TAU
              XIMAX=X(I)
              XIMIN=X(I)
              INIT=.TRUE.
            ENDIF
            XIMIN=AMIN1(XIMIN,X(I))
            XIMAX=AMAX1(XIMAX,X(I))
            TAUMN2=AMIN1(TAUMN2,TAU)
            TAUMX2=AMAX1(TAUMX2,TAU)
            SIGMAX=AMAX1(SIGMAX,SIG0)
          ENDIF
        ENDDO
1999    CONTINUE
        TAUMIN = TAUMN2
        TAUMAX = TAUMX2
        REWIND UDIAG
        DELTAX=0.1*(XIMAX-XIMIN)
        DELTAT=0.1*(TAUMX2-TAUMN2)
C    Skip nonvarying components
        IF (DELTAX.EQ.0.0) GOTO 5000
C    Setup viewport - new page, if page is full
        CALL PLTHNV(IH,NH,MH,DWIDTH,DHEIGH,FACTX,FACTY,FACTV,QCURS,
     1              .FALSE.,IVCLCK)
        IF (IVCLCK.NE.0) IOPT(20)=ICOMP(IVCLCK)
        ICOMP(IH)=I
        CALL PLTHVC(IH,NH,MH,FACTX,FACTY,FACTV,XL,XU,YL,YU)
        CALL PGVSIZ(XL,XU,YL,YU)
        CALL PGSWIN(-1.25,11.85,-1.25,11.85)
C    Plot box and axises
        CALL PGLINE(5,XFR,YFR)
        CALL PLTHAX(0.0,0.0,'TAU',-3,FACT,10.0,0.0,TAUMN2)
        CALL PLTHAX(0.0,0.0,' ',1,FACT,10.0,90.0,XIMIN)
        CALL PGSCH(0.7*FACT) 
        CALL PGPTXT(-0.75,5.0,90.0,0.0,'X')
        IF (LMODE) CALL PGPTXT(-2.0,3.5,90.0,0.0,'LOG')
        WRITE(CI,'(I2)') I
        CALL PGSCH(0.6*FACT) 
        CALL PGPTXT(-0.6,5.15,90.0,0.0,CI)
        LENGTH=MIN(20,LEN(PTEXT))
        DO J=20,1,-1
          IF (PTEXT(J:J).NE.' ') GOTO 2010
          LENGTH = J-1
        ENDDO
2010    CONTINUE
        CALL PGSCH(1.5*FACT)
        IF (LENGTH.GT.0) CALL PGPTXT(4.9,11.5,0.0,0.5,PTEXT(:LENGTH))
C    Loop over all data
        DO 3099 J=1,9999
          READ(UDIAG,80001,END=3999) ITYP,IB,IF,IS,SIG0
          IF (J.EQ.1) IHOMA=IHOM
          IF (ITYP.NE.7) IHOM=IB
C       Reentry in read
3001      CONTINUE
          IF (ITYP.LE.4)
     &      READ(UDIAG,80002)(X(L),L=1,N1),(DX(L),L=1,N1),(XW(L),L=1,N1)
          IF (ITYP.GE.5)
     &         READ(UDIAG,80002) (X(L),L=1,N1)
          TAU=X(N1)
          IF (LMODE) THEN
            DO 3002 L=1,N
              X(L)=ALOG10(X(L))
3002        CONTINUE
          END IF
C      No drawing to new offset
          IF (ITYP.EQ.2) GOTO 3040
C      Only marking for external point or B-point
          IF (ITYP.EQ.5 .OR. ITYP.EQ.7) GOTO 3051
C      Skip scaling values
          IF (ITYP.EQ.6) GOTO 3099
C      Set values for end of interval
          X1=X(IHOMA)
          IF (IHOMA.EQ.N1) X1=(X1-TAUMN2)/DELTAT
          IF (IHOMA.NE.N1) X1=(X1-XIMIN)/DELTAX
          F1=(X(I)-XIMIN)/DELTAX
          F1H=(TAU-TAUMN2)/DELTAT
          F1P=DX(I)/DX(IHOMA)*XW(I)/XW(IHOMA)/DELTAX
          IF (IHOMA.EQ.N1) F1P=F1P*DELTAT
          IF (IHOMA.NE.N1) F1P=F1P*DELTAX
          F1PH=DX(N1)/DX(IHOMA)*XW(N1)/XW(IHOMA)/DELTAT
          IF (IHOMA.EQ.N1) F1PH=F1PH*DELTAT
          IF (IHOMA.NE.N1) F1PH=F1PH*DELTAX
          IF (.NOT.INBOX .AND. (F1H.LT.0.0 .OR. F1H.GT.10.0) ) GOTO 3040
C      Set values for interpolating function
          H=X1-X0
          A=2.*(F0-F1)+H*(F0P+F1P)
          B=-3.*(F0-F1)-H*(2.*F0P+F1P)
          C=H*F0P
          AH=2.*(F0H-F1H)+H*(F0PH+F1PH)
          BH=-3.*(F0H-F1H)-H*(2.*F0PH+F1PH)
          CH=H*F0PH
C      Skip in case of zero step
          IF (H.EQ.0.D0) GOTO 3039
C      Estimate number of plot points
          HMAX=ABS(H)
          IF (A.EQ.0. .AND. B.EQ.0.) GOTO 3011
          HMAX=SQRT(8.*SIGMA/AMAX1(ABS(6.*A+2.*B),ABS(2.*B)))
3011      CONTINUE
          IF (AH.EQ.0. .AND. BH.EQ.0.) GOTO 3012
          HMAX=AMIN1(HMAX,
     1               SQRT(8.*SIGMA/AMAX1(ABS(6.*AH+2.*BH),ABS(2.*BH))))
3012      CONTINUE
C    Try another parametrization in case of high curvature
          IF (IHOM.NE.IHOMA .AND. AMAX1(ABS(A),ABS(AH)).GE.THR) THEN
            ITYP=2
            ICO=0
3021        BACKSPACE UDIAG
              READ(UDIAG,80003) CHAR
              IF (CHAR.NE.' ') ICO=ICO+1
              IF (ICO.EQ.2) GOTO 3001
              BACKSPACE UDIAG
            GOTO 3021
          ENDIF
          IPOINT=INT(ABS(H)/HMAX)+3
          IF (LMODE.OR.INTER.EQ.0.OR.ABS(A).GT.THR.OR.ABS(AH).GT.THR)
     &         IPOINT=1
          IF (KPRINT.EQ.2) WRITE(UPR,60001) IHOMA,X0,X1,F0,F1,F0P,F1P,
     &        F0H,F1H,F0PH,F1PH,A,B,C,AH,BH,CH,IPOINT
          HPOINT=H/FLOAT(IPOINT)
C      Plot interpolation function
          DO IP=1,IPOINT
			Z=IP*HPOINT/H
			XP=FLOAT(INT((F0H+Z*(CH+Z*(BH+Z*AH)))*SMALIN))*SMALL
			YP=F0+Z*(C+Z*(B+Z*A))
			IF (XP.GE.0.0 .AND. XP.LE.10.0) THEN
			   IF (INBOX) THEN
				  CALL PGDRAW(XP,YP)
			   ELSE
				  CALL PGMOVE(XP,YP)
				  INBOX=.TRUE.
			   ENDIF
			ELSE
			   INBOX=.FALSE.
			ENDIF
          ENDDO
3039      CONTINUE
C      Marking of internal points
          CALL PGSCH(0.84*FACT)
          IF (ITYP.EQ.0 .AND. MARK.EQ.2) CALL PGPT1(XP,YP,3)
          IF (ITYP.EQ.1 .AND. MARK.GE.1) CALL PGPT1(XP,YP,6)
          IF (ITYP.EQ.3 .AND. TURN.EQ.1) CALL PGPT1(XP,YP,5)
          CALL PGMOVE(XP,YP)
3040      CONTINUE
          XP=FLOAT(INT((TAU-TAUMN2)/DELTAT*SMALIN))*SMALL
          YP=(X(I)-XIMIN)/DELTAX
          IF (ITYP.EQ.2 .OR. IHOM.NE.IHOMA) THEN
C         Set values for beginning of interval
			X0=X(IHOM)
			IF (IHOM.EQ.N1) X0=(X0-TAUMN2)/DELTAT
			IF (IHOM.NE.N1) X0=(X0-XIMIN)/DELTAX
			F0=(X(I)-XIMIN)/DELTAX
			F0H=(TAU-TAUMN2)/DELTAT
			F0P=DX(I)/DX(IHOM)*XW(I)/XW(IHOM)/DELTAX
			IF (IHOM.EQ.N1) F0P=F0P*DELTAT
			IF (IHOM.NE.N1) F0P=F0P*DELTAX
			F0PH=DX(N1)/DX(IHOM)*XW(N1)/XW(IHOM)/DELTAT
			IF (IHOM.EQ.N1) F0PH=F0PH*DELTAT
			IF (IHOM.NE.N1) F0PH=F0PH*DELTAX
          ELSE
C         Begin new branch
			X0=X1
			F0=F1
			F0H=F1H
			F0P=F1P
			F0PH=F1PH
          ENDIF
          IHOMA=IHOM
          IF (XP.GE.0.0 .AND. XP.LE.10.0) THEN
             IF (ITYP.LE.2 .AND. MARK.EQ.2) THEN
               CALL PGSCH(0.84*FACT)
               CALL PGPT1(XP,YP,3)
             ENDIF
             CALL PGMOVE(XP,YP)
             INBOX=.TRUE.
          ELSE
             INBOX=.FALSE.
          ENDIF
          GOTO 3099
C      Marking of external points
3051      CONTINUE
          IF ( TAU.LT.TAUMIN .OR. TAU.GT.TAUMAX ) GOTO 3099
          XPQ=(TAU-TAUMN2)/DELTAT
          YPQ=(X(I)-XIMIN)/DELTAX
          IF (ITYP.EQ.5.AND.EXT.EQ.1) THEN
            CALL PGSCH(0.84*FACT)
            CALL PGPT1(XPQ,YPQ,4)
          ENDIF
          IF (ITYP.EQ.7) THEN
          XPQ=XPQ-0.21*FLOAT(IF)-0.03
          YPQ=YPQ-0.1
          CALL PGSCH(0.28*FACT)
          IF (IB.LT.0) CALL PGPT1(XPQ,YPQ,ICHAR('-'))
          IF (IB.EQ.0) CALL PGPT1(XPQ,YPQ,ICHAR('0'))
          IF (IB.GT.0) CALL PGPT1(XPQ,YPQ,ICHAR('+'))
          END IF
          CALL PGMOVE(XP,YP)
3099      CONTINUE
3999    CONTINUE
        REWIND UDIAG
5000    CONTINUE
      ENDIF
      ENDDO
      IF (SIGMAX.EQ.0.0) GOTO 9000
C    Setup viewport - new page, if page is full
      CALL PLTHNV(IH,NH,MH,DWIDTH,DHEIGH,FACTX,FACTY,FACTV,QCURS,
     1            .FALSE.,IVCLCK)
      IF (IVCLCK.NE.0) IOPT(20)=ICOMP(IVCLCK)
      ICOMP(IH)=N+1
      CALL PLTHVC(IH,NH,MH,FACTX,FACTY,FACTV,XL,XU,YL,YU)
      CALL PGVSIZ(XL,XU,YL,YU)
	  CALL PGSWIN(-3.05,15.05,-3.05,15.05)
C    Plot box and axises
	  CALL PGLINE(5,XFR2,YFR2)
      DELTAS=0.1*SIGMAX
      CALL PLTHAX(0.0,0.0,'TAU',-3,FACT,10.,0.,TAUMN2)
      CALL PLTHAX(0.0,0.0,'SIGMA',5,FACT,10.0,90.0,0.0)
      DO 5020 J=1,9999
      READ(UDIAG,80001,END=9000) ITYP,IB,IF,IS,SIG0
      IF (ITYP.LE.4)
     &   READ(UDIAG,80002) (X(L),L=1,N1),(DX(L),L=1,N1),(XW(L),L=1,N1)
      IF (ITYP.GE.5) READ(UDIAG,80002) (X(L),L=1,N1)
      TAU=X(N1)
      IF (TAU.LT.TAUMIN .OR. TAU.GT.TAUMAX) GOTO 5020
      IF (ITYP.NE.7) GOTO 5020
      IF (IB.EQ.0) GOTO 5020
      XP=FLOAT(INT((TAU-TAUMN2)/DELTAT*SMALIN))*SMALL
      YP=SIG0/DELTAS
      CALL PGMOVE(XP,0.0)
      CALL PGDRAW(XP,10.0)
      CALL PGMOVE(XP,YP)
      XPQ=XP+0.3*FLOAT(IF)
      CALL PGDRAW(XPQ,YP)
      CALL PGSCH(0.84*FACT)
      CALL PGPT1(XP,YP,5)
      IF (IS.EQ.1) CALL PGPT1(XP,YP,7)
      XPQ=XP-0.21*FLOAT(IF)-0.03
      YPQ=YP-0.1
      CALL PGSCH(0.28*FACT)
      IF (IB.LT.0) CALL PGPT1(XPQ,YPQ,ICHAR('-'))
      IF (IB.GT.0) CALL PGPT1(XPQ,YPQ,ICHAR('+'))
5020  CONTINUE
9000  CONTINUE
      CALL PLTHNV(IH,NH,MH,DWIDTH,DHEIGH,FACTX,FACTY,FACTV,QCURS,.TRUE.,
     1            IVCLCK)
      IF (IVCLCK.NE.0) IOPT(20)=ICOMP(IVCLCK)
      IF (ICLOSE.EQ.1) CALL PGCLOS
      RETURN
60001 FORMAT('0IHOM',I12/' X0  ',D12.3,'   X1  ',D12.3/
     1   ' F0  ',D12.3,'   F1  ',D12.3/
     2   ' F0P ',D12.3,'   F1P ',D12.3/
     3   ' F0H ',D12.3,'   F1H ',D12.3/
     4   ' F0HP',D12.3,'   F1HP',D12.3/
     5   ' A   ',D12.3,'   B   ',D12.3,'   C   ',D12.3/
     6   ' AH  ',D12.3,'   BH  ',D12.3,'   CH  ',D12.3/,' IP  ',I12)
80001 FORMAT(I1,3I4,E18.10)
80002 FORMAT(4E18.10)
80003 FORMAT(A1)
81002 FORMAT(A1)
82001 FORMAT(1X,'DWIDTH=',F5.2,'  DHEIGH',F5.2,'  FACTX=',F5.3,
     1          '  FACTY=',F5.3,'  FACTV=',F5.3,'  NH=',I2,'  MH=',I2)
82002 FORMAT(1X,'Option IOPT(7) changed due to view surface ratio to ',
     1           'value ',I2)
82050 FORMAT(1X,'N=',I5,' is too large - increase MAXN1 at least to',I5)
82051 FORMAT(1X,'Opening of plot device ',A,' failed, return-code=',I7)
      END
C
C   Plot an axis write an axis-name at the side of it 
C
      SUBROUTINE PLTHAX(XSTRT,YSTRT,TEXT,TXTLEN,FACT,AXLEN,ANGLE,AXMIN)
      IMPLICIT NONE
      REAL XSTRT,YSTRT,FACT,AXLEN,ANGLE,AXMIN
      INTEGER TXTLEN
      CHARACTER*(*) TEXT
C
      REAL AXLN,SCS,XFIN,YFIN
C
      AXLN = ABS(AXLEN)
      XFIN = XSTRT+COSD(ANGLE)*AXLN
      YFIN = YSTRT+SIND(ANGLE)*AXLN
      CALL PGQCH(SCS)
      CALL PGSCH(FACT)
      CALL PGAXIS('N',XSTRT,YSTRT,XFIN,YFIN,AXMIN,AXMIN+AXLN,
     1            0.0,2,0.5,0.5,0.5,0.7,0.0)
      IF (TXTLEN.LT.0) THEN
         CALL PGPTXT(0.5*(XSTRT+XFIN),0.5*(YSTRT+YFIN)-1.0,
     1              ANGLE,0.5,TEXT(:IABS(TXTLEN)))
      ELSE
         CALL PGPTXT(0.5*(XSTRT+XFIN)-1.0,0.5*(YSTRT+YFIN),
     1              ANGLE,0.5,TEXT(:IABS(TXTLEN)))
      ENDIF
      CALL PGSCH(SCS)
      RETURN
      END
C
C   Get viewport coordinates in inches from given viewport number 
C
      SUBROUTINE PLTHVC(IVP,NH,MH,FACTX,FACTY,FACTV,XL,XU,YL,YU)
      IMPLICIT NONE
      INTEGER IVP,NH,MH
      REAL FACTV,FACTX,FACTY,XL,XU,YL,YU
C
      INTEGER I,J,NMH
C
      NMH = NH*MH
      IF (IVP.GT.NMH) IVP = IVP-INT((IVP-1)/NMH)*NMH
      J = INT((IVP-1)/NH)+1
      I = IVP -(J-1)*NH
	  XL = FLOAT(I-1)*FACTX
	  XU = XL+FACTV
	  YL = FLOAT(MH-J)*FACTY
	  YU = YL+FACTV
C
      RETURN
      END
C
C   Switch to the next viewport, if needed to a new page
C
      SUBROUTINE PLTHNV(IVP,NH,MH,DWIDTH,DHEIGH,FACTX,FACTY,FACTV,
     1                  QCURS,QFIN,IVCLCK)
      IMPLICIT NONE
      INTEGER IVP,NH,MH,IVCLCK
      REAL DWIDTH,DHEIGH,FACTX,FACTY,FACTV
      LOGICAL QCURS,QFIN
C
      INTEGER I,J
      REAL XH,YH
      CHARACTER CHAR*1
C
      IVCLCK = 0
      IVP = IVP+1
	  IF (IVP.GT.NH*MH) THEN
		IF (QCURS) THEN
		  CALL PGSVP(0.0,1.0,0.0,1.0)
		  CALL PGSWIN (0.0,DWIDTH,0.0,DHEIGH)
		  CALL PGCURS(XH,YH,CHAR)
		  I = INT(XH/DWIDTH*FLOAT(NH))+1
		  J = INT(YH/DHEIGH*FLOAT(MH))+1
		  XH = XH-FLOAT(I-1)*FACTX
		  YH = YH-FLOAT(J-1)*FACTY
		  IF ( I.LE.NH .AND. J.LE.MH .AND. 
     1         XH.LE.FACTV .AND. YH.LE.FACTV ) IVCLCK=(MH-J)*NH+I
		ENDIF
		IF (.NOT.QFIN) CALL PGPAGE()
		IVP=1
	  ENDIF
C
      RETURN
      END
