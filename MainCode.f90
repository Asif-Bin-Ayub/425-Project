!#########################################################################################
!#                    PHYCS425 - COMPUTATIONAL PHYSICS I                                 #
!#                                FALL 2021                                              #
!#                                                                                       #
!#					  CONSTRAINED MOTION ALONG SURFACES                                  #
!#				                                                                         #
!#		Project completed by:                                                            #
!#			Asif Bin Ayub                                                                #
!#			Ali Mirza Isa                                                                #
!#			Kumail Abdulaziz Radhi                                                       #
!#                                                                                       #
!#########################################################################################

!<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
!<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
!<><>									PART I CODE									       <><>
!<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
!<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

! KEY FOR VARIABLES USED IN PART I OF THE PROGRAM
! (This is NOT an exhaustive list)
!	g --> Acceleration due to gravity
!	pi --> Another name for 3
!	l --> Length parameter
!	x,z --> Cartesian coordinates
!	theta --> Angular variable
!	omega --> Time derivative of theta
!	q --> Angular damping coefficient
!	Fd --> Driving amplitude
!	omega_d --> Driving angular frequency
!	th_k --> RK-4 coefficients for theta
!	w_k --> RK-4 coefficients for omega

!  USE OF MODULE
!
! 	The following modules encapsulates all the RK-4 related functions in one place.
! 	Use of modules was quite helpful in doing this part, as we needed to vary quite
!	a few parameters to identify various properties of motion along the 2 surfaces.
!			The variables declared inside a module have global scope wherever the 
!	module is used (subroutines, functions, main programs etc.). This made looping 
!	over parameters such as length l, theta_0, Fd etc. a breeze. In addition, this
!	will allow us to easily add more features (like Fourier transforms) in the 
!	future (if we ever get back to this project ¯\_(ツ)_/¯ ).

!	NOTE: For a given set of parameters and initial conditions, both paths are 
!	simulated.

module RK4_Part1
implicit none

!Declaration:==========================
double precision, parameter :: g=9.8d0,pi=acos(-1.0d0)
double precision :: t,dt,tmax,l,x(1:2),z(1:2),E(1:2)
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
	function AA(theta,omega,t) result(res_AA)
		double precision :: theta(1:2),omega(1:2),t
		double precision :: res_AA(1:2)
	
		! Path1:-----------
			res_AA(1) = -0.5d0*g/l*sin(theta(1))*(1.0d0+cos(theta(1)))&
			-1.5d0*omega(1)**2*sin(theta(1))/(1.0d0+cos(theta(1)))-q*omega(1)+Fd*sin(omega_d*t)
		! Path2:----------
			res_AA(2) = 0.5d0*sin(theta(2))*(omega(2)**2-g/l)/(1.0d0+cos(theta(2)))-q*omega(2)+Fd*sin(omega_d*t)

		return
	end function AA

end module RK4_Part1

!#########################################################################################
! Main Program
program Preject_Part1
use RK4_Part1
implicit none 
	
	!Variables to determine period
	!theta_0, omega_0 --> Initial conditions
	!T_0 --> Period
	!check --> To check if period was measured 
	double precision :: theta_0(1:2), omega_0(1:2), T_0(1:2)
	logical :: check(1:2)
	
	!Oscillation amplitudes
	double precision :: A(1:2)
	
	!Parameters:===========================
	dt=0.01d0
	tmax=60.0d0 !s
	
	!Storing results:======================
	!Path1 (Parabola):---------------------
		!Trajectory:
		open(unit=10,file='Trajectory_Path1.txt')
		!Equilibrium Point(s):
		open(unit=11,file='Equilibrium_Path1.txt')
		! !Period data
		! open(unit=20,file='Period_Path1.txt')
		! !For resonance, amplitude
		! open(unit=31, file='Amplitude_Path1.txt')
		! !Conservation of energy
		open(unit=41,file='Energy_Path1.txt')
	!Path2 (Cycloid):----------------------
		!Trajectory:
		open(unit=12,file='Trajectory_Path2.txt')
		!Equilibrium Point(s):
		open(unit=13,file='Equilibrium_Path2.txt')
		! !Period data
		! open(unit=21,file='Period_Path2.txt')
		! !For resonance, amplitude
		! open(unit=32, file='Amplitude_Path2.txt')
		! !Conservation of energy
		open(unit=42,file='Energy_Path2.txt')
	
	!Initializing:=========================
	omega_0(1:2)=0.0d0 !rad/s
	theta_0(1:2)=0.9d0*pi !rad
	l=9.8d0 !metres
	
	
	! Take Fd=0 & q=0 UNLESS you want to simulate damping and driving forces
	
	! Notes on damping coefficient q (for Fd=0)
	! [This is just a qualitative estimate]
		! Path 1:
			! 2 or more --> Overdamped (2 is not the critical damping coefficient, just roughly)
			! less than 2 --> Underdamped
			! less than 1 --> Sizeable oscillations
		! Path 2:
			! 2 or more --> Overdamped (2 is not the critical damping coefficient, just roughly)
			! less than 2 --> Underdamped
			! less than 0.5 --> Sizeable oscillations
		
	
	Fd=0.0d0 !Driving amplitude
	omega_d=0d0*pi !Driving frequency
	q=0.0d0 !Damping coefficient
	
	! INSTRUCTIONS FOR THE OUTER LOOP:
	!
	! 	Only "activate" (or un-comment) one outer loop AT A TIME. Each loop is
	!	used for a specific task. Here's a brief summary:
	!		+	PeriodvsTheta0 --> Used to compare period with initial amplitude.	
	!		+	PeriodvsL --> Used to compare period with length parameter l.
	!		+ 	AmplitudevsFd --> Used to compare amplitude with Driving amplitude
	!		+	AmplitudevsWd --> Used to compare amplitude with Driving angular 
	!							  frequency
	!	In each case, activate the relavant block of code in the time loop, in 
	!	addition to activating the outer loops. The titles in each block are self-
	!	explanatory.
	
	!##################  OUTER LOOP  #########################
	
	! PeriodvsTheta0: do while (theta_0(1) .le. 0.99d0*pi)
	! PeriodvsL: do while (l .le. 15d0)
	! AmplitudevsFd: do while (Fd <= 10d0)
	! AmplitudevsWd: do while (omega_d <= 3*pi)
		
		!Initializing:=========================
		t=0.0d0 !s
		
		theta(1:2) = theta_0(1:2)
		omega(1:2) = omega_0(1:2)
		
		!For period calculation
		check(1:2) = .FALSE.
		T_0(1:2) = 0d0
		
		!Resetting amplitudes
		A(1:2) = 0d0
	
		!Path1:-------------
		x(1)=l*sin(theta(1))/(1.0d0+cos(theta(1))) !m
		z(1)=-l*cos(theta(1))/(1.0d0+cos(theta(1))) !m
	
		!Path2:-------------
		x(2)=l*(theta(2)+sin(theta(2)))
		z(2)=l*(1-cos(theta(2)))
				
		!Time Loop:============================ 
		do while(t.le.tmax)
			! Writing trajectories and paths:
			! DO NOT ACTIVATE THIS BLOCK WHEN OUTER LOOP IS ON
			! Path1:------------
				write(10,*) t,theta(1),omega(1),x(1),z(1)
			! Path2:----------------
				write(12,*) t,theta(2),omega(2),x(2),z(2)
			
			!RK4 Step
			call simulation
			
			! EQUILIBRIUM POINT(S)
			! DO NOT ACTIVATE THIS BLOCK WHEN OUTER LOOP IS ON
			if (abs(der_omega(1)) .le. 5d-4) then
				write(11,*) x(1),z(1),t ! For path 1
			else if (abs(der_omega(2)) .le. 3d-4) then
				write(13,*) x(2),z(2),t ! For path 2
			end if
	
			! MEASURING PERIOD (ONLY for undamped, undriven) - UNRELIABLE
			! 	This results in a lot of noise, which needed to be fixed manually.
			! 	Varying the tolerance for omega does reduce the noise, but this 
			! 	needs to be determined by trial and error.
			!			A better approach would be to do a discrete time Fourier
			!	transform, and see which frequencies dominate.
			
			! ACTIVATE THIS WITH PeriodvsL & PeriodvsTheta0 LOOPS
			! Period: if(check(1) .eqv. .FALSE.) then
				! !Path 1
				! if(theta(1).ge.0d0 .and. abs(omega_0(1)-omega(1)).le.3d-3) then
					! T_0(1)=t
					! check(1) = .TRUE.
				! end if
			! else if(check(2) .eqv. .FALSE.) then
				! !Path 2
				! if(theta(2).ge.0d0 .and. abs(omega_0(2)-omega(2)).le.7d-2) then
					! T_0(2)=t
					! check(2) = .TRUE.
				! end if		
			! end if Period
			
			
			! MEASURING TOTAL ENERGIES AT A GIVEN TIME
				!Path1
				E(1)=(((l*omega(1))**2d0)/(1+cos(theta(1)))**3d0)-(g*l*cos(theta(1))/(1+cos(theta(1))))
				!Path 2
				E(2)=((l*omega(2))**2d0 *(1+cos(theta(2))))+(g*l*(1-cos(theta(2)))) 
			
			! Writing total energies with time
				! Path 1
				write(41,*) t,E(1)
				! Path 2
				write(42,*) t,E(2)
			
			! MEASURING OSCILLATION AMPLITUDES
			! Amplitude: if (t .ge. 45d0) then
				! if (A(1).le.theta(1)) then
					! A(1) = theta(1)
				! end if
				
				! if (A(2).le.theta(2)) then
					! A(2) = theta(2)
				! end if	
			! end if Amplitude
			
			!Trajectory:
			!Path1:------------
				x(1)=l*sin(theta(1))/(1.0d0+cos(theta(1)))
				z(1)=-l*cos(theta(1))/(1.0d0+cos(theta(1)))
			!Path2:------------
				x(2)=l*(theta(2)+sin(theta(2)))
				z(2)=l*(1-cos(theta(2)))
			
			!Time Update:
			t=t+dt
		end do
	
	! Writing periods for different initial amplitudes & l's
		! ! Path 1
		! write(20,*)theta_0(1),l,T_0(1)
		! ! Path 2
		! write(21,*)theta_0(2),l,T_0(2)
	
	!Writing Amplitudes for different drivng frequencies & forces
		! ! Path 1
		! write(31,*)omega_d,Fd,A(1)
		! ! Path 2
		! write(32,*)omega_d,Fd,A(2)
	
		! omega_d = omega_d +0.05d0
	! end do AmplitudevsWd
	
		! Fd=Fd+0.05d0
	! end do AmplitudevsFd

		! l = l+0.05d0
	! end do PeriodvsL
		
		! theta_0(1:2) = theta_0(1:2)+0.05d0
	! end do PeriodvsTheta0

	! Closing results files
	close(10)
	close(11)
	close(12)
	close(13)
	close(20)
	close(21)
	close(31)
	close(32)
	close(41)
	close(42)
	
contains

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
	
end program Preject_Part1

!##############################################################################################

!<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
!<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
!<><>									PART II CODE								       <><>
!<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
!<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

! program Preject_Part2
! implicit none 

! !Declaration:==========================
! double precision :: dt,g,l,pi
! double precision :: t,tmax,x,y,z,vx,vy,vz
! double precision :: th_k(1:4),w_k(1:4),phi_k(1:4),a_k(1:4)
! double precision :: theta,der_theta,omega,der_omega,phi,der_phi,alpha,der_alpha
! double precision :: K,AV1,AA1,AV2,AA2
! double precision :: Lm,Em

! !Parameters:===========================
! pi=acos(-1.0d0)
! g=9.8d0 !m/s^2
! l=9.8d0 !m
! dt=0.01d0
! tmax= 60.0d0 !s

! !Initializing:=========================
! t=0.0d0 !s
! theta = 1.0d0 !rad
! phi   = 0.0d0 !rad
! omega = 0.5d0 !rad/s
! alpha = sqrt(1/cos(theta)) !rad/s

! x = l*sin(theta)*cos(phi) !m
! y = l*sin(theta)*sin(phi) !m
! z =-l*cos(theta) !m

! vx = l*(cos(theta)*cos(phi)*omega - sin(theta)*sin(phi)*alpha)
! vy = l*(cos(theta)*sin(phi)*omega + sin(theta)*cos(phi)*alpha)
! vz = l*sin(theta)*omega

! Lm = (l*sin(theta))**2*alpha                    	           
! Em = 0.5d0*l**2*(omega**2+(sin(theta)*alpha)**2)-g*l*cos(theta)	

! !Storing results:======================
	! !Trajectory:
	! open(unit=10,file='Trajectory.txt')
	! !Conservation:
	! open(unit=11,file='Conservation.txt')

! !Loop:================================== 
! loop: do while(t.le.tmax)
	
	! !Writing Results:
	! write(10,*) t,x,y,z,theta,omega
	! write(11,*) t,Lm,Em,-g*l*cos(theta),0.5d0*l**2*(omega**2+(sin(theta)*alpha)**2)
	
	! ! ## ACTIVATE THIS TO TEST THE OPTIONAL TASK ##
	! !Initializing only when the object coming from outside:
	! ! if(z.lt.0.0d0 .and. vz.lt.0.0d0) then
		! ! if(x.lt.0.0d0) then 
			! ! theta = -acos(-z/l)
		! ! else
			! ! theta = acos(-z/l)
		! ! endif
		! ! omega = vz/(l*sin(theta))
	! ! endif

	! !K1
	! th_k(1)  = AV1(theta,omega,phi,alpha)
	! w_k(1)   = AA1(theta,omega,phi,alpha)
	! phi_k(1) = AV2(theta,omega,phi,alpha)
	! a_k(1)   = AA2(theta,omega,phi,alpha)
	
	! !K2
	! th_k(2)  = AV1(theta+0.5d0*th_k(1)*dt,omega+0.5d0*w_k(1)*dt,phi+0.5d0*phi_k(1)*dt,alpha+0.5d0*a_k(1)*dt)
	! w_k(2)   = AA1(theta+0.5d0*th_k(1)*dt,omega+0.5d0*w_k(1)*dt,phi+0.5d0*phi_k(1)*dt,alpha+0.5d0*a_k(1)*dt)
	! phi_k(2) = AV2(theta+0.5d0*th_k(1)*dt,omega+0.5d0*w_k(1)*dt,phi+0.5d0*phi_k(1)*dt,alpha+0.5d0*a_k(1)*dt)
	! a_k(2)   = AA2(theta+0.5d0*th_k(1)*dt,omega+0.5d0*w_k(1)*dt,phi+0.5d0*phi_k(1)*dt,alpha+0.5d0*a_k(1)*dt)
	
	! !K3
	! th_k(3)  = AV1(theta+0.5d0*th_k(2)*dt,omega+0.5d0*w_k(2)*dt,phi+0.5d0*phi_k(2)*dt,alpha+0.5d0*a_k(2)*dt) 
	! w_k(3)   = AA1(theta+0.5d0*th_k(2)*dt,omega+0.5d0*w_k(2)*dt,phi+0.5d0*phi_k(2)*dt,alpha+0.5d0*a_k(2)*dt)
	! phi_k(3) = AV2(theta+0.5d0*th_k(2)*dt,omega+0.5d0*w_k(2)*dt,phi+0.5d0*phi_k(2)*dt,alpha+0.5d0*a_k(2)*dt)
	! a_k(3)   = AA2(theta+0.5d0*th_k(2)*dt,omega+0.5d0*w_k(2)*dt,phi+0.5d0*phi_k(2)*dt,alpha+0.5d0*a_k(2)*dt)
	
	! !K4
	! th_k(4)  = AV1(theta+th_k(3)*dt,omega+w_k(3)*dt,phi+phi_k(3)*dt,alpha+a_k(3)*dt) 
	! w_k(4)   = AA1(theta+th_k(3)*dt,omega+w_k(3)*dt,phi+phi_k(3)*dt,alpha+a_k(3)*dt)
	! phi_k(4) = AV2(theta+th_k(3)*dt,omega+w_k(3)*dt,phi+phi_k(3)*dt,alpha+a_k(3)*dt)
	! a_k(4)   = AA2(theta+th_k(3)*dt,omega+w_k(3)*dt,phi+phi_k(3)*dt,alpha+a_k(3)*dt)
	
	! !Rk4 Derivatives:
	! der_theta = K(th_k(1:4))
	! der_omega = K(w_k(1:4))
	! der_phi   = K(phi_k(1:4))
	! der_alpha = K(a_k(1:4))
	
	! theta = theta + der_theta*dt
	! omega = omega + der_omega*dt
	! phi   = phi   + der_phi*dt
	! alpha = alpha + der_alpha*dt
	
	! !Trajectory:
	! x = l*sin(theta)*cos(phi) !m
	! y = l*sin(theta)*sin(phi) !m
	! z =-l*cos(theta) !m
	
	! !Velocity:
	! vx = l*(cos(theta)*cos(phi)*omega - sin(theta)*sin(phi)*alpha)
	! vy = l*(cos(theta)*sin(phi)*omega + sin(theta)*cos(phi)*alpha)
	! vz = l*sin(theta)*omega

	! ! !Conservation:
	! Lm = (l*sin(theta))**2*alpha 
	! Em = 0.5d0*l**2*(omega**2+(sin(theta)*alpha)**2)-g*l*cos(theta)

	! ! ## ACTIVATE THIS TO TEST THE OPTIONAL TASK ##
	! !Freefall loop:
	! ! freefall: do while(z.ge.0.0d0)
		! ! !Euler method:
		! ! z=z+vz*dt
		! ! vz=vz-g*dt
		
		! ! !Writing results:
		! ! write(10,*) t,x,y,z
		
		! ! !Time update:
		! ! t=t+dt
		
	! ! enddo freefall
		
	! !Time Update:
	! t=t+dt

! enddo loop

	! ! Closing results files
	! close(10)
	! close(11)

! end program Preject_Part2

! !##########################################################

! ! Defining Functions:==============
	! ! RK4 Derivative (K):
	! function K(k_i)
		! double precision :: k_i(1:4)
		! double precision :: K
		! K = (k_i(1)+2.0d0*(k_i(2)+k_i(3))+k_i(4))/6.0d0	
	! end function K
	
	! ! Angular Acceleration (AA1): For "Theta"
	! function AA1(theta,omega,phi,alpha)
		! double precision :: q,g,l,theta,omega,phi,alpha
		! double precision :: AA1
		! g=9.8d0
		! l=9.8d0
		! q=0.0d0
		! AA1 = alpha**2*sin(theta)*cos(theta)-g/l*sin(theta)-q*omega
	! end function AA1
		
	! ! Angular Velocity (AV1): For "Theta"
	! function AV1(theta,omega,phi,alpha)
		! double precision :: theta,omega,phi,alpha
		! double precision :: AV1
		! AV1 = omega
	! end function AV1

	! ! Angular Acceleration (AA2): For "Phi"
	! function AA2(theta,omega,phi,alpha)
		! double precision :: q,theta,omega,phi,alpha
		! double precision :: AA2
		! q=0.0d0
		! AA2 = -2.0d0*omega*alpha*sin(theta)/cos(theta)-q*alpha
	! end function AA2
		
	! ! Angular Velocity (AV2): For "Phi"
	! function AV2(theta,omega,phi,alpha)
		! double precision :: theta,omega,phi,alpha
		! double precision :: AV2
		! AV2 = alpha
	! end function AV2
