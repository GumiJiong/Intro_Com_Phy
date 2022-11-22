program ode
    implicit none
    real (kind=16), dimension(:,:), allocatable :: y
    real (kind=16) :: dx, dy
    integer :: N

    print *, "input the step size Δx"
    read (*,*) dx
    N = int(20/dx)
    allocate(y(3,N+1))
    y(1,1) = 0
    y(2,1) = 6.75
    y(3,1) = -1

    open(1,file="12_ode_0001.txt",action="write")
    write (1,*) y(1,1), y(2,1)
    do N = 1, int(20/dx)
        y(1,N+1) = y(1,N) + dx
        y(2,N+1) = y(2,N) + dx*y(3,N)
        write (1,*) y(1,N+1), y(2,N+1)
        y(3,N+1) = dy(y(1,N+1),y(2,N+1))
    end do

end program ode

function dy(x, fx) result(dyx)
    real (kind=16), intent(in) :: x, fx
    real (kind=16) :: dyx

    dyx = (2*cos(x)**3*sin(x)-1-sin(x)*fx)/cos(x)
end function dy