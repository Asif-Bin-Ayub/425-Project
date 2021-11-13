program projectile
implicit none 

!Declaration
double precision :: t,tmax,dt,x,y,z,vx,vy,vz
!do something here
double precision :: g,der_x,der_y,der_z

!parameters:

!Initializing:
!sdsj hehe
!Storing results:

!Kumail write something here.

!Loop over

print*, "Hello Asif"

t=0.0d0
tmax=20.0d0
do while(t.le.tmax)
 write(40,*) t
 t=t+1.0d0
enddo

print*, "I'm a projectile!"
end program projectile
