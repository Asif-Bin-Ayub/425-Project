! Program to simulate a non-linear simple pendulum using RK-4
! Uses procedures to make the program modular

program SimplePendulum
implicit none
	
	! Variable Declarations
		
	! Constants
	! g = accleration due to gravity (m/s^2), l = length of pendulum
	double precision, parameter :: g = 9.8d0, l = 9.8d0, pi = acos(-1.00)
	
	! Parameters of motion
	! theta = angular displacement (rad), omega = angular velocity (rad/s) 
	! K = RK-4 derivative, ki = RK-4 coefficients
	double precision :: theta, omega, x, y, t, dt
	double precision :: k1_th, k2_th, k3_th, k4_th, K_th
	double precision :: k1_w, k2_w, k3_w, k4_w, K_w
	
	! Initializations
	dt = 1d-2 !sec
	t = 0d0 !sec

	! Initial conditions
	theta = 0.5d0 !rad
	omega = 0d0 !rad/s
	
	! Initial Positions
	x = l*sin(theta)
	y = l*(1-cos(theta))
	
	! Result files
	open(11, file = "Pendulum_RK4.txt")
		
	!RK-4 loop
	do while (t <= 60d0)
		
		!Writing results into file
		write(11,*) t, theta, omega, x, y
		
		
		! k1
		k1_th = omega
		k1_w = A(theta, 0d0)

		! k2
		k2_th = omega + k1_w*(dt/2d0)
		k2_w = A(theta, 0.5d0*k1_th) ! Factor of 1/2 for k2 & k3 coefficients

		! k3
		k3_th = omega + k2_w*(dt/2d0)
		k3_w = A(theta, 0.5d0*k2_th) ! Factor of 1/2 for k2 & k3 coefficients
		
		! k4
		k4_th = omega + k3_w*dt
		k4_w = A(theta, k3_th)

		! Derivatives
		K_th = K(k1_th, k2_th, k3_th, k4_th)
		K_w =  K(k1_w, k2_w, k3_w, k4_w)
		
		! Updating parameters
		theta = theta + K_th*dt
		omega = omega + K_w*dt
		
		! Updating positions
		x = l*sin(theta)
		y = l*(1-cos(theta))
		
		t = t + dt
		
		end do
	
	close(11)
	
	
	contains
	
	! Function to evaluate angular acceleration (alpha)
	function A(theta, k)
	implicit none
		! Dummy arguments
		double precision :: A, theta, k, dt
		double precision :: g, l
		
		g = 9.8d0 !m/s^2
		l = 9.8d0 !m
		
		dt = 1d-2 !sec
		
		A = -(g/l)*sin(theta + k*dt)

	end function A
	

	! Function to calculate derivatives for RK-4
	function K(k1, k2, k3, k4)
	implicit none
		! Dummy arguments
		double precision :: K, k1, k2, k3, k4
		
		K = (k1 + 2*k2 + 2*k3 + k4)/6d0

	end function K
	
	
end program SimplePendulum