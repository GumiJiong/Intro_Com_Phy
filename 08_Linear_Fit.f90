program LinearFit
    implicit none
    
    real, dimension(:), allocatable :: x, y, est, res
    real, dimension(4) :: coeff = 0
    real :: a, b, mae
    integer :: N, i, io

    open(1, file='./08_data.txt', status='old', action='read')

    N = 0
    do
        read(1,*,iostat=io)
        if ( io/=0 ) exit
        N = N + 1
    end do
    if ( N<2 ) then
        print *, "Too few data to fit"
        call exit
    end if

    rewind(1)

    allocate(x(N))
    allocate(y(N))
    allocate(est(N))
    allocate(res(N))

    do i = 1, N
        read(1,*) x(i), y(i)
    end do

    do i = 1, N
        coeff(1) = coeff(1) + y(i)
        coeff(2) = coeff(2) + x(i)**2.0
        coeff(3) = coeff(3) + x(i)
        coeff(4) = coeff(4) + x(i)*y(i)
    end do

    a = (coeff(1)*coeff(2)-coeff(3)*coeff(4))/(N*coeff(2)-coeff(3)**2.0)
    b = (N*coeff(4)-coeff(3)*coeff(1))/(N*coeff(2)-coeff(3)**2.0)

    mae = 0.0
    do i = 1, N
        est(i) = a + b * x(i)
        res(i) = est(i) - y(i)
        mae = mae + res(i)**2.0
    end do
    mae = mae/N

    print "(1X, A, F5.3, A, F6.3)", "Linear model: y = ", b, " x + ", a
    print *, "original  original  estimated  residual"
    print *, "       x         y          y"
    do i = 1, N
        print "(3X, F6.3, 5X, F5.3, 6X, F5.3, 4X, F6.3)", x(i), y(i), est(i), res(i)
    end do
    print "(A, F5.3)", "Averaged squared error = ", mae

end program LinearFit