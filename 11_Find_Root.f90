program FindRoot
    implicit none
    real :: f, df, x, fx, dfx, tol, dx
    integer :: i
    logical :: flag

    x = 1.0
    tol = 1.0e-6
    flag = .false.
    do i = 1, 7
        dx = abs(f(x)/df(x))
        x = x-f(x)/df(x)
        if ( dx <= tol ) then
            flag = .true.
            print *, x
            call exit
        end if
    end do

    if ( flag .eqv. .false. ) print *, "Fails to detect the root"
end program FindRoot

function f(x) result(fx)
    real, intent(in) :: x
    real :: fx

    fx = exp(x)*log(x)-x**2
end function f

function df(x) result(dfx)
    real, intent(in) :: x
    real :: dfx

    dfx = exp(x)/x+exp(x)*log(x)-2*x
end function df