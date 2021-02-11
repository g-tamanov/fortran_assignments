program l1t2
    real x,y
    OPEN (1, FILE = 'l1t2.txt',ACCESS = 'SEQUENTIAL', STATUS = 'unknown')
    write(*,*)'write x'
    read(*,*)x
    write(1,*)'x=',x
    y=((2*x)**4)-((3*x)**3)+((4*x)**2)-(5*x)+6
    write(*,*)'y=((2*x)**4)-((3*x)**3)+((4*x)**2)-(5*x)+6=',y
    write(1,*)'y=((2*x)**4)-((3*x)**3)+((4*x)**2)-(5*x)+6=',y
    y=1-(2*x)+((3*x)**2)-((4*x)**3)
    write(*,*)'y=1-(2*x)+((3*x)**2)-((4*x)**3)=',y
    write(1,*)'y=1-(2*x)+((3*x)**2)-((4*x)**3)=',y
    y=1+(2*x)+((3*x)**2)+((4*x)**3)
    write(*,*)'y=1+(2*x)+((3*x)**2)+((4*x)**3)=',y
    write(1,*)'y=1+(2*x)+((3*x)**2)+((4*x)**3)=',y
    close(1)
end program l1t2