program SimplePendulum
implicit none 

!Declaration
double precision :: x,y,t,tmax,dt,theta,omega,domega,l,g,pi
double precision :: th_k1,th_k2,th_k3,th_k4,w_k1,w_k2,w_k3,w_k4
double precision :: der_theta,der_omega

!parameters:
pi=acos(-1.0d0)
g=9.8d0 !m/s^2
l=9.8d0 !m
dt=0.01d0
tmax=60.0d0 !s

!Initializing:
t=0.0d0 !s
x=0.0d0 !m
y=0.0d0 !m
theta=0.5d0 !rad
omega=0.0d0 !rad/s
domega = -g/l*sin(theta) 

!Storing results:
open(unit=11,file="Pendulum_RK4.txt")

!Loop over time
do while(t.le.tmax)
	!Omega:
	w_k1= -g/l*sin(theta)
	w_k2= -g/l*sin(theta) - 0.5d0*w_k1
	w_k3= -g/l*sin(theta) - 0.5d0*w_k2
	w_k4= -g/l*sin(theta) - w_k3
	omega = omega + 1/6.0d0*dt*(w_k1+2*(w_k2+w_k3)+w_k4)
	
	! omega = omega + der_omega(w_k1,w_k2,w_k3,w_k4)*dt
	
	!Theta:
	th_k1= omega 
	th_k2= omega + 0.5d0*th_k1
	th_k3= omega + 0.5d0*th_k2
	th_k4= omega + th_k3
	theta = theta + 1/6.0d0*dt*(th_k1+2*(th_k2+th_k3)+th_k4)
	
	! theta = theta + der_theta(th_k1,th_k2,th_k3,th_k4)*dt
	
	!x:
	x=l*sin(theta)
	
	!y:
	y=l*(1.0d0-cos(theta))
	
	t=t+dt
	write(11,*) t,theta,omega,x,y
enddo

end program SimplePendulum

!RK functions:
function der_theta(th_k1,th_k2,th_k3,th_k4)
	double precision :: th_k1,th_k2,th_k3,th_k4
	double precision :: der_theta
	th_k1= omega 
	th_k2= omega + 0.5d0*th_k1
	th_k3= omega + 0.5d0*th_k2
	th_k4= omega + th_k3
	der_theta = (th_k1+2.0d0*(th_k2+th_k3)+th_k4)/6.0d0
end function der_theta

function der_omega(w_k1,w_k2,w_k3,w_k4)
	double precision :: w_k1,w_k2,w_k3,w_k4
	double precision :: der_omega
	w_k1= -g/l*sin(theta)
	w_k2= -g/l*sin(theta) - 0.5d0*w_k1
	w_k3= -g/l*sin(theta) - 0.5d0*w_k2
	w_k4= -g/l*sin(theta) - w_k3
	der_omega = (w_k1+2.0d0*(w_k2+w_k3)+w_k4)/6.0d0
end function der_omega