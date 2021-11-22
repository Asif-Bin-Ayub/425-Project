program SimplePendulum
implicit none 

!Declaration:=================================
double precision :: x,y,t,tmax,dt,theta,omega,l,g,pi
double precision :: th_k1,th_k2,th_k3,th_k4,w_k1,w_k2,w_k3,w_k4
double precision :: der_theta,der_omega,K,AV,AA

!Parameters:=====================================
pi=acos(-1.0d0)
g=9.8d0 !m/s^2
l=9.8d0 !m
dt=0.01d0
tmax=60.0d0 !s

!Initializing:=================================
t=0.0d0 !s
x=l*sin(theta) !m
y=l*(1.0d0-cos(theta)) !m
theta=0.5d0 !rad
omega=0.0d0 !rad/s

!Storing results:==========================
open(unit=11,file="Pendulum_RK4.txt")

!Loop over time:===========================
do while(t.le.tmax)

	!K1
	th_k1= AV(theta,omega) 
	w_k1= AA(theta,omega)
	
	!K2
	th_k2= AV(theta + 0.5d0*th_k1*dt,omega + 0.5d0*w_k1*dt)
	w_k2= AA(theta + 0.5d0*th_k1*dt,omega + 0.5d0*w_k1*dt)
	
	!K3
	th_k3= AV(theta + 0.5d0*th_k2*dt,omega + 0.5d0*w_k2*dt)
	w_k3= AA(theta + 0.5d0*th_k2*dt,omega + 0.5d0*w_k2*dt)
	
	!K4
	th_k4= AV(theta + th_k3*dt,omega + w_k3*dt)
	w_k4= AA(theta + th_k3*dt,omega + w_k3*dt)
	
	!Rk4 Derivatives 
	der_theta=K(th_k1,th_k2,th_k3,th_k4)
	der_omega=K(w_k1,w_k2,w_k3,w_k4)
	
	theta = theta + der_theta*dt
	omega = omega + der_omega*dt
	
	!x&y:
	x=l*sin(theta)
	y=l*(1.0d0-cos(theta))
	
	t=t+dt
	write(11,*) t,theta,omega,x,y
enddo

end program SimplePendulum

!RK functions:===========================
!RK4 derivatives
function K(k1,k2,k3,k4)
	double precision :: k1,k2,k3,k4
	double precision :: K
	K = (k1+2.0d0*(k2+k3)+k4)/6.0d0	
end function K

!Angular velocity
function AV(theta,omega)
	double precision :: theta,omega
	double precision :: AV

	AV = omega
end function AV

!Angular acceleration
function AA(theta,omega)
	double precision :: g,l,theta,omega
	double precision :: AA
	g=9.8d0
	l=9.8d0
	AA = -g/l*sin(theta)
end function AA
