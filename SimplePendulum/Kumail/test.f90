program pendulum 

double precision :: t,theta,dtheta,dt,g,l,ddtheta,der_theta,der_dtheta,x,y

!Parameters:
pi=acos(-1.0d0)
g=9.8d0
l=9.8d0
dt=0.01d0
tmax=60.0d0

!Initializing:
t=0.0d0
theta=0.5d0
dtheta=0.0d0
ddtheta=-g/l*sin(theta)

!Results:
	!Position
	open(unit=11,file='test.txt')

do while (t.le.tmax)
	! Euler or Euler-Cromer method:
	der_theta=dtheta

	ddtheta=-g/l*sin(theta)
	der_dtheta=ddtheta

	dtheta = dtheta + der_dtheta*dt
	der_theta=dtheta				
	theta = theta + der_theta*dt
	
	x=l*sin(theta) 
	y=-l*cos(theta)

	t = t+dt
	
	write(11,*)t,theta,dtheta,x,y
enddo

end program pendulum