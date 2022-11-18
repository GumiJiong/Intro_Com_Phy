program NumDiff
    implicit none
    
    real (kind=16) :: fx, diffx, ddiffx, x0, dx, f, diff, ddiff
    real (kind=16), dimension(3, 5) :: firstd, errfd
    real (kind=16), dimension(2, 5) :: secondd, errsd

    integer :: i, j
    
    x0 = 26.0
    dx = 0.01
    do i = 1, 5
        firstd(1, i) = (f(x0 + dx*i) - f(x0))/(dx*i)
        firstd(2, i) = (f(x0) - f(x0 - dx*i))/(dx*i)
        firstd(3, i) = (f(x0 + dx*i) - f(x0 - dx*i))/(2*dx*i)
        do j = 1, 3
            errfd(j, i) = firstd(j, i) - diff(x0)
        end do

        secondd(1,i) = (f(x0 + dx*i) - 2*f(x0) + f(x0 - dx*i))/(dx*i)**2
        secondd(2,i) = (-f(x0 + 2*dx*i) + 16*f(x0 + dx*i) - 30*f(x0) + 16*f(x0 - dx*i) - f(x0 - 2*dx*i))/(12*(dx*i)**2)
        do j = 1, 2
            errsd(j, i) = secondd(j, i) - ddiff(x0)
        end do
    end do
    
    open(1, file="09_err.txt")
    do j = 1, 3
        write(1,*) errfd(j, :)
    end do
    do j = 1, 2
        write(1,*) errsd(j, :)
    end do
end program NumDiff

function f(x) result(fx)
    real (kind=16) , intent(in) :: x
    real (kind=16)  :: fx

    fx = x*sin(x)
end function f

function diff(x) result(diffx)
    real (kind=16) , intent(in) :: x
    real (kind=16)  :: diffx

    diffx = sin(x) + x*cos(x)
end function diff

function ddiff(x) result(ddiffx)
    real (kind=16) , intent(in) :: x
    real (kind=16)  :: ddiffx

    ddiffx = 2*cos(x) - x*sin(x)
end function ddiff