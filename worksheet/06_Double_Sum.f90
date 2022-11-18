program DoubleSum
    implicit none
    
    real, parameter :: pi=acos(-1.0)
    real :: A, Aa, B, Ba, C, Ca, s
    integer :: i, j, k, N

    print *, "input s"
    read(*,*) s

    do N = 10, 100, 10
        A = 0.0
        do i = 1, N
            do j = 1, i
                A = A + real(j*(i+1))**(-2)
            end do
        end do
        Aa = (pi**4)/120

        B = 1.0
        do k = 1, N-1
            B = B * sin(k*pi/N)
        end do
        Ba = N*(2.0**(1-N))

        C = 1.0
        do k = 1, N
            C = C *(1-((s**2.0)/(k**2.0)))
        end do
        C = C*pi*s
        Ca = sin(pi*s)


        print *, "N = ", N
        print *, "A = ", A, "A(analytic) = ", Aa
        print *, "B = ", B, "B(analytic) = ", Ba
        print *, "C = ", C, "C(analytic) = ", Ca
    end do
end program DoubleSum