program L2T3
    real y, x

    write(*, *) '1) write x'
    read(*, *) x

    if ((-9<x) .and. (x<-6)) then
        y = -3 + sqrt(9 - ((3 + x)**2))
        write(*, *) 'y=', y
    elseif ((-6<x) .and. (x<-3)) then
        y = 3 + x
        write(*, *) 'y=', y
    elseif ((-3<x) .and. (x<0)) then
        y = sqrt((3 + x)**2)
        write(*, *) 'y=', y
    elseif ((0<x) .and. (x<3)) then
        y = 3 - x
        write(*, *) 'y=', y
    end if

    write(*, *) '2) write x'
    read(*, *) x

    if ((-2>x) .and. (x==-2)) then
        y = -2
        write(*, *) 'y=', y
    elseif ((-2<x) .and. (x<0)) then
        y = 2 + x
        write(*, *) 'y=', y
    elseif ((0==x) .and. (x>0)) then
        y = 2 - (x)
        write(*, *) 'y=', y
    end if

    write(*, *) '3) write x'
    read(*, *) x

    if ((-2<=x) .and. (x<=2)) then
        y = sqrt(4 - (x**2))
        write(*, *) 'y=', y
    elseif ((2<x) .and. (x<=4)) then
        y = x - 2
        write(*, *) 'y=', y
    elseif ((x>4)) then
        y = 2
        write(*, *) 'y=', y
    end if

    write(*, *) '4) write x'
    read(*, *) x

    if ((0<=x) .and. (x<=pi)) then
        y = sin(x)
        write(*, *) 'y=', y
    elseif ((pi<x) .and. (x<=2 * pi)) then
        y = sin(x - pi)
        write(*, *) 'y=', y
    elseif ((2 * pi<x) .and. (x<=3 * pi)) then
        y = -sin(x - (2 * pi))
        write(*, *) 'y=', y
    end if

    write(*, *) '5) write x'
    read(*, *) x

    if ((0<=x) .and. (x<=pi / 2)) then
        y = sin(x)
        write(*, *) 'y=', y
    elseif ((pi / 2<x) .and. (x<=(3 * pi) / 2)) then
        y = 1
        write(*, *) 'y=', y
    elseif (((3 * pi) / 2<x) .and. (x<=2 * pi)) then
        y = -sin(x - (2 * pi))
        write(*, *) 'y=', y
    elseif (x<=2 * pi) then
        y = -sin(x - (2 * pi))
        write(*, *) 'y=', y
    end if

end program L2T3