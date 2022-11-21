program iflogic
    implicit none

    real :: phi, x, y, z
    real, parameter :: pi = (acos(-1.0))
    integer :: i
    real, dimension(3,3) :: para = reshape((/-1.5, 4.0, 9.0, 0.0, 12.0, -2.2, 3.5, 4.0, 0.5/),shape(para))

    open(1,file='./04_if.txt')
    do i = 1,3
        x = para(i,1)
        y = para(i,2)
        z = para(i,3)
        if ( x<0 ) then
            phi = sqrt(x**3+y**3+z**3)
        else if ( x==0 ) then
            phi = pi/4
        else if ( x>0 ) then
            phi = sin(x*y)+cos(x*z)
        endif
        write(1,*) '(x,y,z) =',x,y,z,'phi =',phi
        write(1,*) ''
    end do


end program iflogic