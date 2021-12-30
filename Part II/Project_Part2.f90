program Preject_Part2
implicit none 

!Declaration:==========================
double precision :: dt,g,l,pi
double precision :: t,tmax,x,y,z
double precision :: th_k(1:4),w_k(1:4),phi_k(1:4),a_k(1:4)
double precision :: theta,der_theta,omega,der_omega,phi,der_phi,alpha,der_alpha
double precision :: K,AV1,AA1,AV2,AA2
double precision :: Lm,Em

!Parameters:===========================
pi=acos(-1.0d0)
g=9.8d0 !m/s^2
l=9.8d0 !m
dt=0.01d0
tmax= 60.0d0 !s

!Initializing:=========================
t=0.0d0 !s
theta = 1.5d0 !rad
phi   = 0.0d0 !rad
omega = 0.0d0 !rad/s
alpha = sqrt(1/cos(theta)) !rad/s

x = l*sin(theta)*cos(phi) !m
y = l*sin(theta)*sin(phi) !m
z =-l*cos(theta) !m

Lm = l**2*sqrt(omega**2+(sin(theta)*alpha)**2)
Em = 0.5d0*l**2*(omega**2+(sin(theta)*alpha)**2)-g*l*cos(theta)

!Storing results:======================
	!Trajectory:
	open(unit=10,file='Trajectory.txt')
	!Conservation:
	open(unit=11,file='Conservation.txt')

!Loop:================================== 
do while(t.le.tmax)
	
	!Writing Results:
	write(10,*) t,x,y,z!,theta,omega,phi,alpha
	write(11,*) t,Lm,Em
	
	!K1
	th_k(1)  = AV1(theta,omega,phi,alpha)
	w_k(1)   = AA1(theta,omega,phi,alpha)
	phi_k(1) = AV2(theta,omega,phi,alpha)
	a_k(1)   = AA2(theta,omega,phi,alpha)
	
	!K2
	th_k(2)  = AV1(theta+0.5d0*th_k(1)*dt,omega+0.5d0*w_k(1)*dt,phi+0.5d0*phi_k(1)*dt,alpha+0.5d0*a_k(1)*dt)
	w_k(2)   = AA1(theta+0.5d0*th_k(1)*dt,omega+0.5d0*w_k(1)*dt,phi+0.5d0*phi_k(1)*dt,alpha+0.5d0*a_k(1)*dt)
	phi_k(2) = AV2(theta+0.5d0*th_k(1)*dt,omega+0.5d0*w_k(1)*dt,phi+0.5d0*phi_k(1)*dt,alpha+0.5d0*a_k(1)*dt)
	a_k(2)   = AA2(theta+0.5d0*th_k(1)*dt,omega+0.5d0*w_k(1)*dt,phi+0.5d0*phi_k(1)*dt,alpha+0.5d0*a_k(1)*dt)
	
	!K3
	th_k(3)  = AV1(theta+0.5d0*th_k(2)*dt,omega+0.5d0*w_k(2)*dt,phi+0.5d0*phi_k(2)*dt,alpha+0.5d0*a_k(2)*dt) 
	w_k(3)   = AA1(theta+0.5d0*th_k(2)*dt,omega+0.5d0*w_k(2)*dt,phi+0.5d0*phi_k(2)*dt,alpha+0.5d0*a_k(2)*dt)
	phi_k(3) = AV2(theta+0.5d0*th_k(2)*dt,omega+0.5d0*w_k(2)*dt,phi+0.5d0*phi_k(2)*dt,alpha+0.5d0*a_k(2)*dt)
	a_k(3)   = AA2(theta+0.5d0*th_k(2)*dt,omega+0.5d0*w_k(2)*dt,phi+0.5d0*phi_k(2)*dt,alpha+0.5d0*a_k(2)*dt)
	
	!K4
	th_k(4)  = AV1(theta+th_k(3)*dt,omega+w_k(3)*dt,phi+phi_k(3)*dt,alpha+a_k(3)*dt) 
	w_k(4)   = AA1(theta+th_k(3)*dt,omega+w_k(3)*dt,phi+phi_k(3)*dt,alpha+a_k(3)*dt)
	phi_k(4) = AV2(theta+th_k(3)*dt,omega+w_k(3)*dt,phi+phi_k(3)*dt,alpha+a_k(3)*dt)
	a_k(4)   = AA2(theta+th_k(3)*dt,omega+w_k(3)*dt,phi+phi_k(3)*dt,alpha+a_k(3)*dt)
	
	!Rk4 Derivatives 
	der_theta = K(th_k(1:4))
	der_omega = K(w_k(1:4))
	der_phi   = K(phi_k(1:4))
	der_alpha = K(a_k(1:4))
	
	theta = theta + der_theta*dt
	omega = omega + der_omega*dt
	phi   = phi   + der_phi*dt
	alpha = alpha + der_alpha*dt
	
	!Trajectory:
	x = l*sin(theta)*cos(phi) !m
	y = l*sin(theta)*sin(phi) !m
	z =-l*cos(theta) !m

	!Conservation:
	Lm = l**2*sqrt(omega**2+(sin(theta)*alpha)**2)
	Em = 0.5d0*l**2*(omega**2+(sin(theta)*alpha)**2)-g*l*cos(theta)

	!Time Update:
	t=t+dt
	
enddo

	! Closing results files
	close(10)

end program Preject_Part2

!##########################################################

! Defining Functions:
	! RK4 Derivative (K):
	function K(k_i)
		double precision :: k_i(1:4)
		double precision :: K
		K = (k_i(1)+2.0d0*(k_i(2)+k_i(3))+k_i(4))/6.0d0	
	end function K
	
	! Angular Acceleration (AA1): For "Theta"
	function AA1(theta,omega,phi,alpha)
		double precision :: q,g,l,theta,omega,phi,alpha
		double precision :: AA1
		g=9.8d0
		l=9.8d0
		q=0.0d0
		AA1 = alpha**2*sin(theta)*cos(theta)-g/l*sin(theta)-q*omega
	end function AA1
		
	! Angular Velocity (AV1): For "Theta"
	function AV1(theta,omega,phi,alpha)
		double precision :: theta,omega,phi,alpha
		double precision :: AV1
		AV1 = omega
	end function AV1

	! Angular Acceleration (AA2): For "Phi"
	function AA2(theta,omega,phi,alpha)
		double precision :: q,theta,omega,phi,alpha
		double precision :: AA2
		q=0.0d0
		AA2 = -2.0d0*omega*alpha*sin(theta)/cos(theta)-q*alpha
	end function AA2
		
	! Angular Velocity (AV2): For "Phi"
	function AV2(theta,omega,phi,alpha)
		double precision :: theta,omega,phi,alpha
		double precision :: AV2
		AV2 = alpha
	end function AV2

! program Preject_Part2
! implicit none 

! !Declaration:==========================
! double precision :: dt,g,l,pi
! double precision :: t,tmax,x(1:2),y(1:2),z(1:2)
! double precision :: th_k(1:4,1:2),w_k(1:4,1:2),phi_k(1:4,1:2),a_k(1:4,1:2)
! double precision :: theta(1:2),der_theta(1:2),omega(1:2),der_omega(1:2),phi(1:2),der_phi(1:2),alpha(1:2),der_alpha(1:2)
! double precision :: K,AV1,AA1,AV2,AA2

! !Parameters:===========================
! pi=acos(-1.0d0)
! l=9.8d0 !m
! dt=0.01d0
! tmax= 300.0d0 !s

! !Initializing:=========================
! t=0.0d0 !s
! theta(:)=(/ 1.5d0,0.75d0 /) !rad
! phi(:)  =(/ 0.5d0,0.0d0/)  !rad
! omega(:)=(/ 0.0d0,0.0d0/) !rad/s
! alpha(:)= sqrt(1/cos(theta(:))) !rad/s

! x(:)=  l*sin(theta(:))*cos(phi(:)) !m
! y(:)=  l*sin(theta(:))*sin(phi(:)) !m
! z(:)= -l*cos(theta(:))             !m

! !Storing results:======================
	! !Trajectory:
	! open(unit=10,file='Trajectory.txt')
	
! !Loop:================================== 
! do while(t.le.tmax)
	
	! !Writing Results:
	! write(10,*) t,x,y,z!,theta,omega,phi,alpha
	
	! !K1
	! th_k(1,:)  = AV1(theta(:),omega(:),phi(:),alpha(:))
	! w_k(1,:)   = AA1(theta(:),omega(:),phi(:),alpha(:))
	! phi_k(1,:) = AV2(theta(:),omega(:),phi(:),alpha(:))
	! a_k(1,:)   = AA2(theta(:),omega(:),phi(:),alpha(:))
	
	! !K2
	! th_k(2,:)  = AV1(theta(:)+0.5d0*th_k(1,:)*dt,omega(:)+0.5d0*w_k(1,:)*dt,phi(:)+0.5d0*phi_k(1,:)*dt,alpha(:)+0.5d0*a_k(1,:)*dt) 
	! w_k(2,:)   = AA1(theta(:)+0.5d0*th_k(1,:)*dt,omega(:)+0.5d0*w_k(1,:)*dt,phi(:)+0.5d0*phi_k(1,:)*dt,alpha(:)+0.5d0*a_k(1,:)*dt)
	! phi_k(2,:) = AV2(theta(:)+0.5d0*th_k(1,:)*dt,omega(:)+0.5d0*w_k(1,:)*dt,phi(:)+0.5d0*phi_k(1,:)*dt,alpha(:)+0.5d0*a_k(1,:)*dt)
	! a_k(2,:)   = AA2(theta(:)+0.5d0*th_k(1,:)*dt,omega(:)+0.5d0*w_k(1,:)*dt,phi(:)+0.5d0*phi_k(1,:)*dt,alpha(:)+0.5d0*a_k(1,:)*dt)
	
	! !K3
	! th_k(3,:)  = AV1(theta(:)+0.5d0*th_k(2,:)*dt,omega(:)+0.5d0*w_k(2,:)*dt,phi(:)+0.5d0*phi_k(2,:)*dt,alpha(:)+0.5d0*a_k(2,:)*dt) 
	! w_k(3,:)   = AA1(theta(:)+0.5d0*th_k(2,:)*dt,omega(:)+0.5d0*w_k(2,:)*dt,phi(:)+0.5d0*phi_k(2,:)*dt,alpha(:)+0.5d0*a_k(2,:)*dt)
	! phi_k(3,:) = AV2(theta(:)+0.5d0*th_k(2,:)*dt,omega(:)+0.5d0*w_k(2,:)*dt,phi(:)+0.5d0*phi_k(2,:)*dt,alpha(:)+0.5d0*a_k(2,:)*dt)
	! a_k(3,:)   = AA2(theta(:)+0.5d0*th_k(2,:)*dt,omega(:)+0.5d0*w_k(2,:)*dt,phi(:)+0.5d0*phi_k(2,:)*dt,alpha(:)+0.5d0*a_k(2,:)*dt)
	
	! !K4
	! th_k(4,:)  = AV1(theta(:)+th_k(3,:)*dt,omega(:)+w_k(3,:)*dt,phi(:)+phi_k(3,:)*dt,alpha(:)+a_k(3,:)*dt) 
	! w_k(4,:)   = AA1(theta(:)+th_k(3,:)*dt,omega(:)+w_k(3,:)*dt,phi(:)+phi_k(3,:)*dt,alpha(:)+a_k(3,:)*dt)
	! phi_k(4,:) = AV2(theta(:)+th_k(3,:)*dt,omega(:)+w_k(3,:)*dt,phi(:)+phi_k(3,:)*dt,alpha(:)+a_k(3,:)*dt)
	! a_k(4,:)   = AA2(theta(:)+th_k(3,:)*dt,omega(:)+w_k(3,:)*dt,phi(:)+phi_k(3,:)*dt,alpha(:)+a_k(3,:)*dt)
	
	! !Rk4 Derivatives 
	! der_theta(:)=K(th_k(1:4,:))
	! der_omega(:)=K(w_k(1:4,:))
	! der_phi(:)  =K(phi_k(1:4,:))
	! der_alpha(:)=K(a_k(1:4,:))
	
	! theta(:) = theta(:) + der_theta(:)*dt
	! omega(:) = omega(:) + der_omega(:)*dt
	! phi(:)   = phi(:)   + der_phi(:)  *dt
	! alpha(:) = alpha(:) + der_alpha(:)*dt
	
	! !Trajectory:
	! x(:)=  l*sin(theta(:))*cos(phi(:)) !m
	! y(:)=  l*sin(theta(:))*sin(phi(:)) !m
	! z(:)= -l*cos(theta(:))             !m

	! !Time Update:
	! t=t+dt
	
! enddo

	! ! Closing results files
	! close(10)

! end program Preject_Part2

! !##########################################################

! ! Defining Functions:
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
		! q=0.05d0
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
		! q=0.05d0
		! AA2 = -2.0d0*omega*alpha*sin(theta)/cos(theta)-q*alpha
	! end function AA2
		
	! ! Angular Velocity (AV2): For "Phi"
	! function AV2(theta,omega,phi,alpha)
		! double precision :: theta,omega,phi,alpha
		! double precision :: AV2
		! AV2 = alpha
	! end function AV2