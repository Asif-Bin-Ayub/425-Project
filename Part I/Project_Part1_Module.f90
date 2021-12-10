module RK4_Part1
implicit none

!Declaration:==========================
double precision, parameter :: g=9.8d0,pi=acos(-1.0d0)
double precision :: t,dt,tmax,l,x(1:2),z(1:2)
double precision :: th_k(1:2,1:4), w_k(1:2,1:4)
double precision :: theta(1:2),der_theta(1:2),omega(1:2),der_omega(1:2)
double precision :: q=0d0,Fd=0d0,omega_d=0d0

contains

!Defining Functions:===================

	! RK4 Derivative (K):
	function K(k_i) result(res_K)
		double precision :: k_i(1:2,1:4)
		double precision :: res_K(1:2)
		res_K(1:2) = (k_i(1:2,1)+2.0d0*(k_i(1:2,2)+k_i(1:2,3))+k_i(1:2,4))/6.0d0	
		return
	end function K
	
	! Angular Acceleration (AA):
	! Take Fd=0 & q=0 UNLESS you want to simulate damping and driving forces 
	function AA(theta,omega,t) result(res_AA)
		double precision :: theta(1:2),omega(1:2),t
		double precision :: res_AA(1:2)

		! q=0.3d0 ! Damping coefficient
		! Fd=2d0 ! Driving amplitude
		! omega_d=2d0*acos(-1.0)/1d0 ! Driving frequency
		
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
			res_AA(1) = -0.5d0*g/l*sin(theta(1))*(1.0d0+cos(theta(1)))&
			-1.5d0*omega(1)**2*sin(theta(1))/(1.0d0+cos(theta(1)))-q*omega(1)+Fd*sin(omega_d*t)
		! Path2:----------
			res_AA(2) = 0.5d0*sin(theta(2))*(omega(2)**2-g/l)/(1.0d0+cos(theta(2)))-q*omega(2)+Fd*sin(omega_d*t)

		return
	end function AA

end module RK4_Part1

! #########################################################
! Main Program
program Preject_Part1
use RK4_Part1
implicit none 
	
	!Variables for determining period
	!theta_0, omega_0 --> Initial conditions
	!T_0 --> Period
	!check --> To stop period measurement after 
	double precision :: theta_0(1:2), omega_0(1:2), T_0(1:2)
	logical :: check(1:2)
	
	!Parameters:===========================
	dt=0.01d0
	tmax=60.0d0 !s
	l = 9.8d0
	
	! read(5,*) l
	
	!Storing results:======================
	!Path1 (Parabola):---------------------
		!Trajectory:
		open(unit=10,file='Trajectory_Path1.txt')
		!Equilibrium Point(s):
		open(unit=11,file='Equilibrium_Path1.txt')
		!Period vs l
		! open(unit=20,file='T_vs_L_Path1.txt')
	!Path2 (Cycloid):----------------------
		!Trajectory:
		open(unit=12,file='Trajectory_Path2.txt')
		!Equilibrium Point(s):
		open(unit=13,file='Equilibrium_Path2.txt')
		!Period vs l
		! open(unit=21,file='T_vs_L_Path2.txt')

	!Initializing:=========================
	t=0.0d0 !s
	theta(1:2)=0.5d0 !rad
	omega(1:2)=0.0d0 !rad/s
	
	!For period calculation
	check(1:2) = .FALSE.
	theta_0(1:2) = theta(1:2)
	omega_0(1:2) = omega(1:2)
	T_0(1:2) = 0d0

	!Path1:-------------
	x(1)=l*sin(theta(1))/(1.0d0+cos(theta(1))) !m
	z(1)=-l*cos(theta(1))/(1.0d0+cos(theta(1))) !m

	!Path2:-------------
	x(2)=l*(theta(2)+sin(theta(2))) !m 
	z(2)=-l*(1+cos(theta(2)))+2.0d0*l !m 

	!Loop:================================== 
	do while(t.le.tmax)
		!Writing Results:
		!Path1:------------
			write(10,*) t,theta(1),omega(1),x(1),z(1)
		!Path2:----------------
			write(12,*) t,theta(2),omega(2),x(2),z(2)
		
		!RK4 Step
		call simulation
		
		! Equilibrium Point(s)
		! if (abs(der_omega(1)) .le. 5d-4) then
			! write(11,*) x(1),z(1),t ! For path 1
		! else if (abs(der_omega(2)) .le. 3d-4) then
			! write(13,*) x(2),z(2),t ! For path 2
		! end if

		! Measuring period (ONLY for undamped, undriven) - UNRELIABLE
		Period: if(check(1) .eqv. .FALSE.) then
			!Path 1
			if(abs(theta_0(1)-theta(1)).le.3d-3 .and. abs(omega_0(1)-omega(1)).le.3d-3) then
				T_0(1)=t
				check(1) = .TRUE.
			end if
		else if(check(2) .eqv. .FALSE.) then
			!Path 2
			if(abs(theta_0(2)-theta(2)).le.3d-3 .and. abs(omega_0(2)-omega(2)).le.3d-3) then
				T_0(2)=t
				check(2) = .TRUE.
			end if		
		end if Period
		
		!Trajectory:
		!Path1:------------
			x(1)=l*sin(theta(1))/(1.0d0+cos(theta(1)))
			z(1)=-l*cos(theta(1))/(1.0d0+cos(theta(1)))
		!Path2:------------
			x(2)=l*(theta(2)+sin(theta(2)))
			z(2)=-l*(1+cos(theta(2)))+2.0d0*l
		
		!Time Update:
		t=t+dt
	end do
	
	! Printing periods on screen
	! print*,"Period of path 1:",T_0(1)
	! print*,"Period of path 2:",T_0(2)

	! Closing results files
	close(10)
	close(11)
	close(12)
	close(13)

end program Preject_Part1

! #########################################################
	
!Defining Subroutines:==================
subroutine simulation
use RK4_Part1
implicit none

	!K1
	th_k(1:2,1)= omega(1:2) 
	w_k(1:2,1)= AA(theta(1:2),omega(1:2),t)
	
	!K2
	th_k(1:2,2)= omega(1:2) + 0.5d0*w_k(1:2,1)*dt
	w_k(1:2,2)= AA(theta(1:2) + 0.5d0*th_k(1:2,1)*dt,omega(1:2) + 0.5d0*w_k(1:2,1)*dt,t + 0.5d0*dt)

	
	!K3
	th_k(1:2,3)= omega(1:2) + 0.5d0*w_k(1:2,2)*dt
	w_k(1:2,3)= AA(theta(1:2) + 0.5d0*th_k(1:2,2)*dt,omega(1:2) + 0.5d0*w_k(1:2,2)*dt,t + 0.5d0*dt)

	
	!K4
	th_k(1:2,4)= omega(1:2) + w_k(1:2,3)*dt
	w_k(1:2,4)= AA(theta(1:2) + th_k(1:2,3)*dt,omega(1:2) + w_k(1:2,3)*dt,t+dt)

	
	!Rk4 Derivatives 
	der_theta(1:2)=K(th_k)
	der_omega(1:2)=K(w_k)
	
	theta(1:2) = theta(1:2) + der_theta(1:2)*dt
	omega(1:2) = omega(1:2) + der_omega(1:2)*dt

end subroutine simulation
	