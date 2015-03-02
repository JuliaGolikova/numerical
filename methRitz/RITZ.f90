PROGRAM RITZ
  IMPLICIT REAL*8(A-H, O-Z)
  DIMENSION X(6), Z(6)
  EXTERNAL FUNCT
  N = 6
  H = 0.05D0
  X(1) = -1.0D0
  X(2) = 1.0D0
  X(3) = 1.0D0
  X(4) = 1.0D0
  X(5) = 1.0D0
  X(6) = 1.0D0

  EPS = 1.D-12
20  MAXCAL = 100
  IFAIL = 0
  CALL SIMPLY (FUNCT, N, H, EPS, X, Z, FE, MAXCAL, IFAIL)
  WRITE(*,*) Z, FE, IFAIL
  IF (IFAIL.EQ.0) GOTO 10
  X(1) = Z(1)
  X(2) = Z(2)
  X(3) = Z(3)
  X(4) = Z(4)
  X(5) = Z(5)
  X(6) = Z(6)
  GOTO 20
10  CONTINUE
END

SUBROUTINE FUNCT (A, FC)
  IMPLICIT REAL*8(A-H, O-Z)
  DIMENSION A(6)
  INTEGER FACT
  MU = A(6)
  G = 1.0
  N = 5
  M = 5
  T = 0
  U = 0
  NORM = 0

DO K = 1, M
  DO J = 1, N

  T = T + 0.25*A(J)*A(K)*(MU**(-K-J-1))*FACT(J+K)*((J+K+2)*(J+K+1)-(J+1)*(K+1))

  U = U + (-G*A(J)*A(K)*FACT(K+J+1)*(MU+1)**(-J-K-2))

  NORM = NORM + A(J)*A(K)*FACT(2+K+J)*MU**(-K-J-3)

  FC =  (T + U) / NORM

  END DO
END DO

RETURN
END

  INTEGER FUNCTION FACT (L)
  INTEGER L
  FACT = 1
  DO I = 1, L
    FACT = FACT*I
  END DO
  RETURN
END
