program Test
    implicit none
    double precision :: k1theta,k2theta,k3theta,k4theta
    double precision :: k1omega,k2omega,k3omega,k4omega
    double precision :: omega,theta
    double precision :: der_theta,der_omega
    double precision :: g,l,t,tmax,dt
    
    !parameters
        l=9.8d0
        g=9.8d0
        dt=0.01d0
        tmax=60d0

    !initialization
        t=0d0
        theta=0.5d0
        omega=0d0

    open (1,file="Ali_Pendulum_RK4.txt")
    
    write(1,*) t, theta, omega !Prints initial condition

    RK4_loop : do while (t<=tmax)

        k1theta=omega
        k1omega=-g/l*sin(theta)

        k2theta=omega+k1omega*dt/2d0
        k2omega=-g/l*sin(theta+k1theta*dt/2d0)

        k3theta=omega+k2omega*dt/2d0
        k3omega=-g/l*sin(theta+k2theta*dt/2d0)

        k4theta=omega+k3omega*dt
        k4omega=-g/l*sin(theta+k3theta*dt)

        der_theta=(k1theta+2d0*k2theta+2d0*k3theta+k4theta)/6d0
        der_omega=(k1omega+2d0*k2omega+2d0*k3omega+k4omega)/6d0
    
        omega=omega+der_omega*dt
        theta=theta+der_theta*dt

        t=t+dt

        write(1,*) t,theta,omega

    end do RK4_loop

end program Test
