program projectile
    implicit none
    real (kind=16) :: Cd, b, A, Cdfun, Afun, bfun, Re, r
    real (kind=16) :: z_1, z_2, v_1, v_2, dt, time, m, g, v_15

    Re = 15000.0
    r = 5e-3
    b = bfun(Cdfun(Re), Afun(r))
    m = 50.0; g = 9.81
    print *, "input the time step Δt"
    read(*,*) dt

    z_1 = 0.0; v_1 = 5.0; time = 0.0;
    open(1,file="13_Euler_0001.txt")
    write(1,*) time, z_1, v_1

    do while (z_2 >= 0.0)
        z_2 = z_1 + v_1*dt
        v_2 = v_1 - dt*(m*g+b*v_1*abs(v_1))/m
        time = time + dt
        z_1 = z_2; v_1 = v_2
        write(1,*) time, z_2, v_2
    end do

    z_1 = 0.0; v_1 = 5.0; time = 0.0; z_2=0;
    open(1,file="13_mp_0001.txt")
    write(1,*) time, z_1, v_1

    do while (z_2 >= 0.0)
        v_15 = v_1 - dt*0.5*(m*g+b*v_1*abs(v_1))/m
        z_2 = z_1 + v_15*dt
        v_2 = v_1 - dt*(m*g+b*v_15*abs(v_15))/m
        time = time + dt
        z_1 = z_2; v_1 = v_2
        write(1,*) time, z_2, v_2
    end do
end program projectile

function Cdfun(Re) result(Cd)
    real (kind=16), intent(in) :: Re
    real (kind=16) :: Cd

    Cd = 24.0/Re+6.0/(1+sqrt(Re))+0.4
end function Cdfun

function Afun(r) result(A)
    real (kind=16), intent(in) :: r
    real (kind=16) :: A
    real (kind=16), parameter :: pi=acos(-1.0)

    A=pi*r*r
end function Afun

function bfun(Cd, A) result(b)
    real (kind=16), intent(in) :: Cd, A
    real (kind=16) :: b

    b = 0.5*1500000*Cd*A
end function bfun