PROGRAM MONCHER
  ! INTEGRATE FUNC USING MONTE-CARLO METHOD
  IMPLICIT REAL*8(A-H, O-Z)
  REAL*8 X(50), F(50)
  PI=4.*ATAN(1.)
  N = 50
  SOLINT = 0
  A = 0.0
  B = 2.0*pi
  H = (B-A)/N

  DO I = 1, N
    X(I) = A + (I-1)*H
    F(I) = FUN(X(I))
    G = ((B-A)/N)*F(I)
    SOLINT = SOLINT + G
  END DO

END

  FUNCTION FUN (X)
    FUN = SIN(X)
  RETURN
  END
