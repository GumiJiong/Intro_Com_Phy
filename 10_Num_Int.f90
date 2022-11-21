program NumInt
    implicit none
    
    real (kind=16) :: a, b, trap, simp, h, x, f
    integer :: N, k

    print *, "input lower boundary a"
    read(*,*) a
    print *, "input higher boundary b"
    read(*,*) b

    do N = 10000, 100000, 10000
        trap = 0.0
        simp = 0.0
        h = (b - a)/real(N)
        do k = 1, N-1
            x = a + h*real(k)
            trap = trap + f(x)
            if ( ((-1)**k) > 0 ) then
                simp = simp + 2*f(x)
            else if ( ((-1)**k) < 0 ) then
                simp = simp + 4*f(x)
        end if
        end do
    
        trap = h*(f(a) + f(b) + 2.0*trap)/2
        simp = h*(f(a) + f(b) + simp)/3
    
        print *, "N = ", N, "a = ", a, "b = ", b
        print *, "Trapezoidal rule: ", trap
        print *, "Simpson's rule: ", simp
    end do
end program NumInt

function f(x) result(fx)
    real (kind=16), intent(in) :: x
    real (kind=16) :: fx

    fx = 3*exp(-x)*sin(x)**2+1
end function f