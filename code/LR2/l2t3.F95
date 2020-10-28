program L2T3
    real y, x
    OPEN (1, FILE = 'l2t3.txt', ACCESS = 'SEQUENTIAL', STATUS = 'unknown')

    write(*, *) '1) write x'
    read(*, *) x
    write(1, *) '1) x=', x

    if ((-9<x) .and. (x<-6)) then
        y = -3 + sqrt(9 - ((3 + x)**2))
        write(*, *) 'y=-3 + sqrt(9 - ((3 + x)**2))=', y
        write(1, *) 'y=-3 + sqrt(9 - ((3 + x)**2))=', y
    elseif ((-6<x) .and. (x<-3)) then
        y = 3 + x
        write(*, *) 'y=3 + x=', y
        write(1, *) 'y=3 + x=', y
    elseif ((-3<x) .and. (x<0)) then
        y = sqrt((3 + x)**2)
        write(*, *) 'y=sqrt((3 + x)**2)=', y
        write(1, *) 'y=sqrt((3 + x)**2)=', y
    elseif ((0<x) .and. (x<3)) then
        y = 3 - x
        write(*, *) 'y=3 - x=', y
        write(1, *) 'y=3 - x=', y
    end if

    write(*, *) '2) write x'
    read(*, *) x
    write(1, *) '2) x=', x

    if ((-2>x) .and. (x==-2)) then
        y = -2
        write(*, *) 'y=', y
        write(1, *) 'y=', y
    elseif ((-2<x) .and. (x<0)) then
        y = 2 + x
        write(*, *) 'y=2 + x=', y
        write(1, *) 'y=2 + x=', y
    elseif ((0==x) .and. (x>0)) then
        y = 2 - (x)
        write(*, *) 'y=2 - (x)=', y
        write(1, *) 'y=2 - (x)=', y
    end if

    write(*, *) '3) write x'
    read(*, *) x
    write(1, *) '3) x=', x

    if ((-2<=x) .and. (x<=2)) then
        y = sqrt(4 - (x**2))
        write(*, *) 'y=sqrt(4 - (x**2))=', y
        write(1, *) 'y=sqrt(4 - (x**2))=', y
    elseif ((2<x) .and. (x<=4)) then
        y = x - 2
        write(*, *) 'y=x - 2=', y
        write(1, *) 'y=x - 2=', y
    elseif ((x>4)) then
        y = 2
        write(*, *) 'y=', y
        write(1, *) 'y=', y
    end if

    write(*, *) '4) write x'
    read(*, *) x
    write(1, *) '4) x=', x

    if ((0<=x) .and. (x<=pi)) then
        y = sin(x)
        write(*, *) 'y=sin(x)=', y
        write(1, *) 'y=sin(x)=', y
    elseif ((pi<x) .and. (x<=2 * pi)) then
        y = sin(x - pi)
        write(*, *) 'y=sin(x - pi)=', y
        write(1, *) 'y=sin(x - pi)=', y
    elseif ((2 * pi<x) .and. (x<=3 * pi)) then
        y = -sin(x - (2 * pi))
        write(*, *) 'y=-sin(x - (2 * pi))=', y
        write(1, *) 'y=-sin(x - (2 * pi))=', y
    end if

    write(*, *) '5) write x'
    read(*, *) x
    write(1, *) '5) x=', x

    if ((0<=x) .and. (x<=pi / 2)) then
        y = sin(x)
        write(*, *) 'y=sin(x)=', y
        write(1, *) 'y=sin(x)=', y
    elseif ((pi / 2<x) .and. (x<=(3 * pi) / 2)) then
        y = 1
        write(*, *) 'y=', y
        write(1, *) 'y=', y
    elseif (((3 * pi) / 2<x) .and. (x<=2 * pi)) then
        y = -sin(x - (2 * pi))
        write(*, *) 'y=-sin(x - (2 * pi))=', y
        write(1, *) 'y=-sin(x - (2 * pi))=', y
    elseif (x<=2 * pi) then
        y = -sin(x - (2 * pi))
        write(*, *) 'y=-sin(x - (2 * pi))=', y
        write(1, *) 'y=-sin(x - (2 * pi))=', y
    end if
    close(1)
end program L2T3