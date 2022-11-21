program bessel
    implicit none
    integer, parameter :: dp = selected_real_kind(32)
    real(kind = dp) :: J, x
    integer :: m, xx, p, alpha

    read(*,*) m

    open(1,file='03_bessel.txt')

    do alpha = 0, 5
        write(1,*) alpha
        do xx = 0, 200
            x = xx*0.1
            J = 0
            do p = 0, m
                J = J+(-1)**p*(x/2)**(2*p+alpha)/(gamma(real(p)+1)*gamma(real(p)+alpha+1))
            end do
            write(1,*) J
        end do
    end do

end program bessel