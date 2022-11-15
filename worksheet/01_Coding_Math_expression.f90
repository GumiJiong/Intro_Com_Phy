program mathexp

    implicit none
    
    REAL :: chi, pi, x
    parameter (pi=acos(-1.0))

    read(*,*) x

    chi = (pi + sin(x)*cos(x)**3*sqrt(1+SQRT(x)) + exp(-x*sin(x)))*(1+x**2+x**3+x**4)**(-2) + pi**2

    write(*,*) chi

end program mathexp
