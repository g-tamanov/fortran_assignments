program L1T3
    real a,b,c,d,x
    OPEN (1, FILE = 'l1t3.txt',ACCESS = 'SEQUENTIAL', STATUS = 'unknown')
    write(*,*) 'write a,b,c,d from 1-4'
    write(*,*) 'write a'
    read(*,*) a
    write(1,*) 'a=',a
    write(*,*) 'write b'
    read(*,*) b
    write(1,*) 'b=',b
    write(*,*) 'write c'
    read(*,*) c
    write(1,*) 'c=',c
    write(*,*) 'write d'
    read(*,*) d
    write(1,*) 'd=',d

    x=((a+b)*(b+c)+d)*(a-b)
    write(*,*)'1) x=((a+b)*(b+c)+d)*(a-b)=',x
    write(1,*)'1) x=((a+b)*(b+c)+d)*(a-b)=',x

    x=(((a/b)+1)*((b/a)-1)-1)/(a+b)
    write(*,*)'2) x=(((a/b)+1)*((b/a)-1)-1)/(a+b)=',x
    write(1,*)'2) x=(((a/b)+1)*((b/a)-1)-1)/(a+b)=',x

    x=(sqrt(a)+sqrt(b))/a
    write(*,*)'8) x=(sqrt(a)+sqrt(b))/a=',x
    write(1,*)'8) x=(sqrt(a)+sqrt(b))/a=',x

    x=1+(((b**2)+(c**2)-(a**2))/2*b*c)
    write(*,*)'9) x=1+(((b**2)+(c**2)-(a**2))/2*b*c)=',x
    write(1,*)'9) x=1+(((b**2)+(c**2)-(a**2))/2*b*c)=',x

    x=((a**2)-1)/(sqrt(a)-sqrt(b))
    write(*,*)'10) x=((a**2)-1)/(sqrt(a)-sqrt(b))=',x
    write(1,*)'10) x=((a**2)-1)/(sqrt(a)-sqrt(b))=',x

    write(*,*) 'write a ,where a=pi/4 or pi/2 , pi'
    write(1,*) 'write a ,where a=pi/4 or pi/2 , pi'
    write(*,*) 'write a'
    read(*,*) a
    write(1,*) 'a=',a

    x=cos(a)+sin(a)+cos(3*a)+sin(3*a)
    write(*,*)'3) x=cos(a)+sin(a)+cos(3*a)+sin(3*a)=',x
    write(1,*)'3) x=cos(a)+sin(a)+cos(3*a)+sin(3*a)=',x

    x=(sin(2*a)+sin(5*a)-sin(3*a))/(cos(a)+1-(2*(sin(2*a)**2)))
    write(*,*)'4) x=(sin(2*a)+sin(5*a)-sin(3*a))/(cos(a)+1-(2*(sin(2*a)**2)))=',x
    write(1,*)'4) x=(sin(2*a)+sin(5*a)-sin(3*a))/(cos(a)+1-(2*(sin(2*a)**2)))=',x

    x=sin(a)**2+cos(a)**2
    write(*,*)'5) x=sin(a)**2+cos(a)**2=',x
    write(1,*)'5) x=sin(a)**2+cos(a)**2=',x

    !write(*,*)'6) x=cotan((3/4*pi)+(3/2*a))'
    !x=cotan((3/4*pi)+(3/2*a))
    !write(*,*)'x=',x

    x=(1-tan(a))/(1+tan(a))
    write(*,*)'7) x=(1-tg(a))/(1+tg(a))=',x
    write(1,*)'7) x=(1-tg(a))/(1+tg(a))=',x

    close(1)
end program L1T3