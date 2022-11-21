program compare
    implicit none
    real :: phi, tau, x
    integer :: xx

    do xx = 0, 1000
        x = xx*0.001
        phi = exp(-x**2*sin(x)**2)*x**1.5
        tau = 0.124523 + 0.739594*(-0.25+x)+0.65781*(-0.25+x)**2&
        -0.916955*(-0.25+x)**3-0.214698*(-0.25+x)**4-2.35154*(-0.25+x)**5

        open(1,file='02_phi.txt')
        open(2,file='02_tau.txt')
        write(1,*) phi
        write(2,*) tau

    end do
end program compare