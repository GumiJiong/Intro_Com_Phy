program madelung
    implicit none
    
    real :: alpha = 0
    integer :: i, j, k, idx
    integer, dimension(6) :: n = (/10, 20, 30, 40, 50, 100/)

    print *, "   N   | Madelung number alpha"
    print *, "---------------------------------"
    do idx = 1, 6
        do j = 1, n(idx)
            alpha = alpha - 6.0*(-1.0)**(j)/j
            do k = 1, n(idx)
                alpha = alpha - 12.0*(-1.0)**(j+k)/sqrt(j**2.0+k**2.0)
                do i = 1, n(idx)
                    alpha = alpha - 8.0*(-1.0)**(i+j+k)/sqrt(i**2.0+j**2.0+k**2.0)
                end do
            end do
        end do
        print 1, "  ", n(idx), "     Mad3D  =  ", alpha
    end do

    1 format(A, I3, A, F11.8)
end program madelung