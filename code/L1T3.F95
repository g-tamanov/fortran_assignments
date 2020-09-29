program L1T3
    real a,b,c,d,x

    write(*,*) 'write a,b,c,d from 1-4'
    write(*,*) 'write a'
    read(*,*) a
    write(*,*) 'a=',a
    write(*,*) 'write b'
    read(*,*) b
    write(*,*) 'b=',b
    write(*,*) 'write c'
    read(*,*) c
    write(*,*) 'c=',c
    write(*,*) 'write d'
    read(*,*) d
    write(*,*) 'd=',d

    write(*,*)'1) x=((a+b)*(b+c)+d)*(a-b)'
    x=((a+b)*(b+c)+d)*(a-b)
    write(*,*)'x=',x

    write(*,*)'2) x=(((a/b)+1)*((b/a)-1)-1)/(a+b)'
    x=(((a/b)+1)*((b/a)-1)-1)/(a+b)
    write(*,*)'x=',x

    write(*,*)'8) x=(sqrt(a)+sqrt(b))/a'
    x=(sqrt(a)+sqrt(b))/a
    write(*,*)'x=',x

    write(*,*)'9) x=1+(((b**2)+(c**2)-(a**2))/2*b*c)'
    x=1+(((b**2)+(c**2)-(a**2))/2*b*c)
    write(*,*)'x=',x

    write(*,*)'10) x=((a**2)-1)/(sqrt(a)-sqrt(b))'
    x=((a**2)-1)/(sqrt(a)-sqrt(b))
    write(*,*)'x=',x

    write(*,*) 'write a ,where a=pi/4 or pi/2 , pi'
    write(*,*) 'write a'
    read(*,*) a

    write(*,*)'3) x=cos(a)+sin(a)+cos(3*a)+sin(3*a)'
    x=cos(a)+sin(a)+cos(3*a)+sin(3*a)
    write(*,*)'x=',x

    write(*,*)'4) x=(sin(2*a)+sin(5*a)-sin(3*a))/(cos(a)+1-(2*(sin(2*a)**2)))' !хз sin^2
    x=(sin(2*a)+sin(5*a)-sin(3*a))/(cos(a)+1-(2*(sin(2*a)**2)))
    write(*,*)'x=',x

    write(*,*)'5) x=sin(a)**2+cos(a)**2'
    x=sin(a)**2+cos(a)**2
    write(*,*)'x=',x

    !write(*,*)'6) x=cotan((3/4*pi)+(3/2*a))'
    !x=cotan((3/4*pi)+(3/2*a))
    !write(*,*)'x=',x

    write(*,*)'7) x=(1-tg(a))/(1+tg(a))'
    x=(1-tan(a))/(1+tan(a))
    write(*,*)'x=',x
end program L1T3