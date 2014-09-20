program machinezero
write (*,*)'input e'
read (*,*) e
do while (1+e>1)
e = e/2
end do
print *, 'epsilon', e
end program
