program Preject_Part1
implicit none 

!Declaration:==========================
double precision :: dt,g,l,pi
double precision :: t,tmax,x,z
double precision :: th_k1,th_k2,th_k3,th_k4,w_k1,w_k2,w_k3,w_k4
double precision :: theta,der_theta,omega,der_omega,K,AV,AA

!Parameters:===========================
pi=acos(-1.0d0)
l=9.8d0 !m
dt=0.01d0
tmax=60.0d0 !s

!Initializing:=========================
t=0.0d0 !s
theta=0.5d0 !rad
omega=0.0d0 !rad/s

!Path1:-------------
! x=l*sin(theta)/(1.0d0+cos(theta)) !m
! z=-l*cos(theta)/(1.0d0+cos(theta)) !m

!Path2:-------------
x=l*(theta+sin(theta)) !m 
z=-l*(1+cos(theta))+2.0d0*l !m 

!Storing results:======================
!Path1 (Parabola):---------------------
	!Trajectory:
	open(unit=10,file='Trajectory_Path1.txt')
	!Equilibrium Point(s):
	! open(unit=11,file='Equilibrium_Path1.txt')
	
!Path2 (Unknown):----------------------
	!Trajectory:
	! open(unit=12,file='Trajectory_Path2.txt')
	
!Loop:================================== 
do while(t.le.tmax)
	
	!Writing Results:
	!Path1:------------
		write(10,*) t,theta,omega,x,z
		! if(AA(theta,omega).le.3.0d-4 .and. AA(theta,omega).ge.-3.0d-4) write(11,*) t !To determine equlibrium poiunts
	!Path2:----------------
		! write(12,*) t,theta,omega,x,z
	
	!K1
	th_k1= AV(theta,omega) 
	w_k1= AA(theta,omega,t)
	
	!K2
	th_k2= AV(theta + 0.5d0*th_k1*dt,omega + 0.5d0*w_k1*dt)
	w_k2= AA(theta + 0.5d0*th_k1*dt,omega + 0.5d0*w_k1*dt,t)
	
	!K3
	th_k3= AV(theta + 0.5d0*th_k2*dt,omega + 0.5d0*w_k2*dt)
	w_k3= AA(theta + 0.5d0*th_k2*dt,omega + 0.5d0*w_k2*dt,t)
	
	!K4
	th_k4= AV(theta + th_k3*dt,omega + w_k3*dt)
	w_k4= AA(theta + th_k3*dt,omega + w_k3*dt,t)
	
	!Rk4 Derivatives 
	der_theta=K(th_k1,th_k2,th_k3,th_k4)
	der_omega=K(w_k1,w_k2,w_k3,w_k4)
	
	theta = theta + der_theta*dt
	omega = omega + der_omega*dt
	
	!Trajectory:
	!Path1:------------
		x=l*sin(theta)/(1.0d0+cos(theta))
		z=-l*cos(theta)/(1.0d0+cos(theta))
	!Path2:------------
		! x=l*(theta+sin(theta))
		! z=-l*(1+cos(theta))+2.0d0*l
	
	!Time Update:
	t=t+dt
	
enddo

	! Closing results files
	close(10)
	close(11)
	close(12)

end program Preject_Part1

!##########################################################

! Defining Functions:
	! RK4 Derivative (K):
	function K(k1,k2,k3,k4)
		double precision :: k1,k2,k3,k4
		double precision :: K
		K = (k1+2.0d0*(k2+k3)+k4)/6.0d0	
	end function K
	
	! Angular Acceleration (AA):
	! Take Fd=0 & q=0 UNLESS you want to simulate damping and driving forces 
	function AA(theta,omega,t)
		double precision :: g,l,theta,omega,t
		double precision :: q,Fd,omega_d
		double precision :: AA
		g=9.8d0
		l=9.8d0
	
		q=0.3d0 ! Damping coefficient
		Fd=12d0 ! Driving amplitude
		omega_d=2*acos(-1.0)/1d0 ! Driving frequency
		
		! Notes on damping coefficient q (with Fd=0)
		! Path 1:
			! 2 or more --> Overdamped (2 is not the critical damping coefficient, just roughly)
			! less than 2 --> Underdamped
			! less than 1 --> Sizeable oscillations
		! Path 2:
			! 2 or more --> Overdamped (2 is not the critical damping coefficient, just roughly)
			! less than 2 --> Underdamped
			! less than 0.5 --> Sizeable oscillations
			
		! Notes on Driving amplitude Fd & omega_d
			! For path 2, the solution blows up for Fd more than 1.2 (Need confirmation)
			! For path 1, the solution blows up for Fd more than 11 (Need confirmation)
			
		! Path1:-----------
			AA = -0.5d0*g/l*sin(theta)*(1.0d0+cos(theta))-1.5d0*omega**2*sin(theta)/(1.0d0+cos(theta))-q*omega+Fd*sin(omega_d*t)
		! Path2:----------
			! AA = 0.5d0*sin(theta)*(omega**2-g/l)/(1.0d0+cos(theta))-q*omega+Fd*sin(omega_d*t)
	end function AA
		
	! Angular Velocity (AV):
	function AV(theta,omega)
		double precision :: theta,omega
		double precision :: AV
		AV = omega
	end function AV