program twobodyproblem
        implicit none
        real, parameter ::G=6.6743 !declaring parameter, gravitational constant
        real::m1,m2,m,k,j,A,B,r,r0,vel_0,h,t,k1,k2,k3,k4,l1,l2,l3,l4,r1,vel_1 !declaring all are real numbers
        integer::s
        m1=10 !mass of first body
        m2=30 !mass of second body
        h=0.1
 
         !time at 0
        r0=3 ! at time 0 position is at 3, initial condition
        vel_0=0 !initial condition
        j=20
        
        m=(m1*m2)/(m1+m2) !reduced mass
        k=G*m1*m2 !declaring constant
        A=(j**2)/(m**2) !declaring constant
        B=k/m !declaring constant
        print*,"reduced mass of the system is", m
        print*,"each time step is",h !time interval between two consecutive position

        !print*,"value of A and B are", A,B
        !RK method
        do s = 1,100000
           k1=h*vel_0
           !print*,"value of k1=",k1
           l1=h*((A/(r0**3))-(B/(r0**2)))
           !print*,"value of l1=",l1

           k2=h*(vel_0+(l1/2))
           !print*,"value of k2=",k2

           l2=h*((A/((r0+(k1/2))**3))-(B/((r0+(k1/2))**2)))
           !print*,"value of l2=",l2

           k3=h*(vel_0+(l2/2))
           !print*,"value of k3=",k3

           l3=h*((A/((r0+(k2/2))**3))-(B/((r0+(k2/2))**2)))
           !print*,"value of l3=",l3

           k4=h*(vel_0+l3)
           !print*,"value of k4=",k4

           l4=h*((A/((r0+(k3/2))**3))-(B/((r0+(k3/2))**2)))
           !print*,"value of l4=",l4

           
        
           r1=r0+((k1+(2*k2)+(2*k3)+k4)/6)
           if(r1<=0) then
                   stop
           end if
           vel_1=vel_0+((1/6)*(l1+(2*l2)+(2*l3)+l4))
         
           print*,s,"new positions is", r1
           write(11,*)s,r1

          r0=r1   
          vel_0=vel_1
        
        
          end do

          

          end program twobodyproblem
