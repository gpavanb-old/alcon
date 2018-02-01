      SUBROUTINE A1DECC (A,NROW,NCOL,MCON,M,N,IRANK,COND,D,
     1                                            PIVOT,KRED,AH,V)
C----------------------------------------------------------------------
C
C     CONSTRAINED QR-DECOMPOSITION OF (M,N)-MATRIX A
C     FIRST MCON ROWS BELONG TO EQUALITY CONSTRAINTS
C
C
C  REFERENCES:
C     1. P.DEUFLHARD, V.APOSTOLESCU:
C        AN UNDERRELAXED GAUSS-NEWTON METHOD FOR EQUALITY CONSTRAINED
C        NONLINEAR LEAST SQUARES PROBLEMS.
C        LECTURE NOTES CONTROL INFORM. SCI. VOL. 7, P. 22-32 (1978)
C
C     2. P.DEUFLHARD, W.SAUTTER:
C        ON RANK-DEFICIENT PSEUDOINVERSES.
C        J. LIN. ALG. APPL. VOL. 29, P. 91-111 (1980)
C
C*********************************************************************
C
C     TO BE USED IN CONNECTION WITH SUBROUTINE SOLCON (A1SOLC)
C
C     RESEARCH CODE FOR GENERAL (M,N)-MATRICES     V 03.04.1984
C
C     INPUT PARAMETERS (* MARKS INOUT PARAMETERS)
C     -----------------------------------------------
C
C
C      * A(NROW,NCOL)  INPUT MATRIX
C                      A(M,N) CONTAINS ACTUAL INPUT
C        NROW          DECLARED NUMBER OF ROWS OF A AND AH
C        NCOL          DECLARED NUMBER OF COLUMNS OF A AND AH
C     (*)MCON          NUMBER OF EQUALITY CONSTRAINTS (MCON<=N)
C                      INTERNALLY REDUCED IF EQUALITY CONSTRAINTS
C                      ARE LINEARLY DEPENDENT
C        M             TREATED NUMBER OF ROWS OF MATRIX A
C        N             TREATED NUMBER OF COLUMNS OF MATRIX A
C     (*)IRANK         PSEUDO-RANK OF MATRIX A
C      * COND          PERMITTED UPPER BOUND OF DABS(D(1)/D(IRANKC))
C                      AND OF DABS(D(IRANKC+1))/D(IRANK))
C                      (SUB-CONDITION NUMBERS OF A)
C        KRED          >=0    HOUSEHOLDER TRIANGULARIZATION
C                             (BUILD UP OF PSEUDO-INVERSE,IF IRANK<N )
C                      < 0    REDUCTION OF PSEUDO-RANK OF MATRIX A
C                             SKIPPING HOUSEHOLDER TRIANGULARIZATION
C                             BUILD-UP OF NEW PSEUDO-INVERSE
C        V(N)          REAL WORK ARRAY
C
C     OUTPUT PARAMETERS
C     -----------------
C
C        A(M,N)        OUTPUT MATRIX UPDATING PRODUCT OF HOUSEHOLDER
C                      TRANSFORMATIONS AND UPPER TRIANGULAR MATRIX
C        MCON          PSEUDO-RANK OF CONSTRAINED PART OF MATRIX A
C        IRANK         PSEUDO-RANK OF TOTAL MATRIX A
C        D(IRANK)      DIAGONAL ELEMENTS OF UPPER TRIANGULAR MATRIX
C        PIVOT(N)      INDEX VECTOR STORING PERMUTATION OF COLUMNS
C                      DUE TO PIVOTING
C        COND          SUB-CONDITION NUMBER OF A
C                      (IN CASE OF RANK REDUCTION: SUB-CONDITION NUMBER
C                      WHICH LED TO RANK REDUCTION)
C        AH(N,N)       UPDATING MATRIX FOR PART OF PSEUDO INVERSE
C
C----------------------------------------------------------------------
C
      INTEGER  IRANK, KRED, MCON, M, N, NROW, NCOL, PIVOT(N)
      INTEGER  I, II, IRK1, I1, J, JD, JJ, K, K1, MH, ISUB
      DOUBLE PRECISION    A(NROW,NCOL), AH(NCOL,NCOL), D(N), V(N)
      DOUBLE PRECISION    COND, ONE , DD, DABS, DSQRT
      DOUBLE PRECISION    H, HMAX, S, T, SMALL, ZERO, EPMACH
C
      DATA  ZERO/0.D0/ , ONE/1.D0/
C
C  RELATIVE MACHINE PRECISION
      CALL ZIBCONST(EPMACH,SMALL)
C
      IF(IRANK.GT.N) IRANK=N
      IF(IRANK.GT.M) IRANK=M
C
C  SPECIAL CASE M=1 AND N=1
      IF(M.GT.1 .OR. N.GT.1) GOTO 100
      PIVOT(1)=1
      D(1)=A(1,1)
      COND=1.D0
      RETURN
C
100   IF  (KRED.LT.0)  GO TO  3
C
C  CONSTRAINED HOUSEHOLDER TRIANGULARIZATION
C
      DO 1 J=1,N
      PIVOT(J) = J
 1    CONTINUE
C
      JD = 1
      ISUB = 1
      MH = MCON
      IF (MH.EQ.0) MH=M
      K1 = 1
201   K = K1
      IF (K.EQ.N)  GO TO 22
      K1 = K+1
      IF (JD.EQ.0)  GO TO 211
21    DO  210  J=K,N
      S = ZERO
      DO 2101  I=K,MH
2101  S = S+A(I,J)*A(I,J)
210   D(J) = S
C
C  COLUMN PIVOTING
211   H = D(K)
      JJ = K
      DO   212  J=K1,N
      IF (D(J).LE.H)  GO TO 212
      H = D(J)
      JJ = J
212   CONTINUE
      IF (JD.EQ.1)  HMAX = H * SMALL
      JD = 0
      IF (H.GE.HMAX)  GO TO 213
      JD = 1
      GO TO 21
 213  IF (JJ.EQ.K)  GO TO 22
C
C  COLUMN INTERCHANGE
      I = PIVOT(K)
      PIVOT(K) = PIVOT(JJ)
      PIVOT(JJ) = I
      D(JJ) = D(K)
      DO  215  I=1,M
      T = A(I,K)
      A(I,K) = A(I,JJ)
215   A(I,JJ) = T
C
22    H = ZERO
      DO  221  I=K,MH
221   H = H+A(I,K)*A(I,K)
      T = DSQRT(H)
C
C  A PRIORI TEST ON PSEUDO-RANK
      IF (ISUB.GT.0) DD = T/COND
      ISUB = 0
      IF (T.GT.DD) GOTO 23
C  RANK REDUCTION
      IF (K.GT.MCON) GOTO 222
C  CONSTRAINTS ARE LINEARLY DEPENDENT
      MCON = K-1
      K1 = K
      MH = M
      JD = 1
      ISUB = 1
      GO TO 201
C
222   IRANK = K - 1
      IF (IRANK.EQ.0)  GOTO 4
      GO TO 3
C
23    S = A(K,K)
      IF (S.GT.ZERO) T = -T
      D(K) = T
      A(K,K) = S-T
      IF (K.EQ.N)  GOTO 4
C
      T = ONE/(H-S*T)
      DO  24  J=K1,N
      S = ZERO
      DO  241  I=K,MH
241   S = S+A(I,K)*A(I,J)
      S = S*T
      DO  242  I=K,M
242   A(I,J) = A(I,J)-A(I,K)*S
24    D(J) = D(J)-A(K,J)*A(K,J)
C
      IF (K.EQ.IRANK) GOTO 3
      IF (K.NE.MCON) GOTO 201
      MH = M
      JD = 1
      ISUB = 1
      GOTO 201
C
C  RANK-DEFICIENT PSEUDO-INVERSE
C
3     IRK1 = IRANK+1
      DO  30  J=IRK1,N
      DO  31  II=1,IRANK
      I = IRK1-II
      S = A(I,J)
      IF (II.EQ.1)  GO TO 310
      DO  3111  JJ=I1,IRANK
3111  S = S-A(I,JJ)*V(JJ)
310   I1 = I
      V(I) = S/D(I)
31    AH(I,J) = V(I)
C     IF(M.LT.N) GOTO 30
      DO  32  I=IRK1,J
      S = ZERO
      I1 = I-1
      DO  321  JJ=1,I1
321   S = S+AH(JJ,I)*V(JJ)
      IF (I.EQ.J)  GO TO 32
      V(I) = -S/D(I)
      AH(I,J) = -V(I)
32    CONTINUE
30    D(J) = DSQRT(S+ONE)
C
C  EXIT
C
4     IF (K.EQ.IRANK) T=D(IRANK)
      IF (T.NE.0.D0) COND=DABS(D(1)/T)
      RETURN
C
C     **********  LAST CARD OF DECCON  **********
C
      END
      SUBROUTINE A1SOLC (A,NROW,NCOL,MCON,M,N,X,B,IRANK,D,
     @                   PIVOT,KRED,AH,V)
C
      INTEGER  IRANK, KRED, M, MCON, N, NROW, NCOL, PIVOT(N)
      DOUBLE PRECISION A(NROW,NCOL), AH(NCOL,NCOL)
      DOUBLE PRECISION B(M), D(N), V(N), X(N), S, ZERO
C
C
C     BEST CONSTRAINED LINEAR LEAST SQUARES SOLUTION OF (M,N)-SYSTEM
C     FIRST MCON ROWS COMPRISE MCON EQUALITY CONSTRAINTS
C
C *********************************************************************
C
C     TO BE USED IN CONNECTION WITH SUBROUTINE DECCON (A1DECC)
C
C     RESEARCH CODE FOR GENERAL (M,N)-MATRICES     V 19.01.1984
C
C     INPUT PARAMETERS (* MARKS INOUT PARAMETERS)
C     -----------------------------------------------
C
C        A(M,N)      SEE OUTPUT OF DECCON
C        NROW        SEE OUTPUT OF DECCON
C        NCOL        SEE OUTPUT OF DECCON
C        M           SEE OUTPUT OF DECCON
C        N           SEE OUTPUT OF DECCON
C        MCON        SEE OUTPUT OF DECCON
C        IRANK       SEE OUTPUT OF DECCON
C        D(N)        SEE OUTPUT OF DECCON
C        PIVOT(N)    SEE OUTPUT OF DECCON
C        AH(N,N)     SEE OUTPUT OF DECCON
C        KRED        SEE OUTPUT OF DECCON
C      * B(M)        RIGHT-HAND SIDE OF LINEAR SYSTEM, IF (KRED.GE.0)
C                    RIGHT-HAND SIDE OF UPPER LINEAR SYSTEM,
C                                                      IF (KRED.LT.0)
C        V(N)        REAL WORK ARRAY
C
C     OUTPUT PARAMETERS
C     -----------------
C
C        X(N)        BEST LSQ-SOLUTION OF LINEAR SYSTEM
C        B(M)        RIGHT-HAND OF UPPER TRIGULAR SYSTEM
C                    (TRANSFORMED RIGHT-HAND SIDE OF LINEAR SYSTEM)
C
C
      INTEGER  I, II, I1, IH, IRK1, J, JJ, J1, MH
C
C
      DATA  ZERO/0.D0/
C
C
      IF (IRANK.GT.0)  GO TO 110
C
C  SOLUTION FOR PSEUDO-RANK ZERO
C
      DO  1  I=1,N
1     X(I) = ZERO
      RETURN
C
110   IF (KRED.LT.0 .OR. (M.EQ.1 .AND. N.EQ.1)) GOTO 4
C
C  CONSTRAINED HOUSEHOLDER TRANSFORMATIONS OF RIGHT-HAND SIDE
C
3     MH = MCON
      IF (MH.EQ.0)  MH = M
      DO  31  J=1,IRANK
      S = ZERO
      DO  311  I=J,MH
311   S = S+A(I,J)*B(I)
      S = S/(D(J)*A(J,J))
      DO  312  I=J,M
312   B(I) = B(I)+A(I,J)*S
      IF (J.EQ.MCON)  MH = M
 31   CONTINUE
C
C  SOLUTION OF UPPER TRIANGULAR SYSTEM
C
4     IRK1 = IRANK+1
      DO  41  II=1,IRANK
      I = IRK1-II
      I1 = I + 1
      S = B(I)
      IF (I1.GT.IRANK)  GO TO 41
      DO  4111  JJ=I1,IRANK
4111  S = S-A(I,JJ)*V(JJ)
41    V(I) = S/D(I)
      IF (IRK1.GT.N) GOTO 5
C
C  COMPUTATION OF THE BEST CONSTRAINED LSQ-SOLUTION
C
      DO  421  J=IRK1,N
      S = ZERO
      J1 = J-1
      DO  4211  I=1,J1
4211  S = S+AH(I,J)*V(I)
421   V(J) = -S/D(J)
      DO  422  JJ=1,N
      J = N-JJ+1
      S = ZERO
      IF (JJ.EQ.1) GOTO 4222
      DO  4221  I=J1,N
4221  S = S+AH(J,I)*V(I)
      IF (J.LE.IRANK) GOTO 4223
4222  J1=J
      V(J)=-(V(J)+S)/D(J)
      GOTO 422
4223  V(J) = V(J)-S
422   CONTINUE
C
C BACK-PERMUTATION OF SOLUTION COMPONENTS
C
5     DO  50  J=1,N
      IH=PIVOT(J)
50    X(IH) = V(J)
      RETURN
C
C     **********  LAST CARD OF SOLCON  **********
C
      END
