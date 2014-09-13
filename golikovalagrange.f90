program lagrange
real x(10), f(10), flagr(1000)

pi=4.*atan(1.)
N = 10
a = 0
b = 2*pi
h = (b-a)/N
DO i = 1, N
 x(i) = a + (i-1)*h
 f(i) = fun(x(i))
END DO

NN=100*N
hh=(b-a)/NN
do k=1, nn
  xx=a+(k-1)*hh
  prod = 0.
  do i = 1, N
    o=1.0
    DO j = 1, N
        if (j.ne.i) o = o*(xx-x(j))/(x(i)-x(j))
    end do
    prod = prod + f(i)*o
  end do
  flagr(k)=prod
end do

  open (10, file='sinl.txt')
DO k = 1, NN
  xx=a+(k-1)*hh
  write (10,*) xx, fun(xx)
end do
open (10, file='lagrintpol.txt')
 DO k = 1, NN
 xx=a+(k-1)*hh
write (10,*) xx, flagr(k)
end do
   close (10)
end


function fun(t)
  fun = sin(t)
  return
end
