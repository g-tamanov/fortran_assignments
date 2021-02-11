program L2T2
    real a, b, c, x
    OPEN (1, FILE = 'l2t2.txt', ACCESS = 'SEQUENTIAL', STATUS = 'unknown')

    write(*, *) 'write a,b,c '
    write(*, *) 'write a'
    read(*, *) a
    write(1, *) 'a=', a
    write(*, *) 'write b'
    read(*, *) b
    write(1, *) 'b=', b
    write(*, *) 'write c'
    read(*, *) c
    write(1, *) 'c=', c

    if ((a<b) .and. (a<c)) then
        write(*, *) 'min real a'
        write(1, *) 'min real a'
        if (b<c) then
            write(*, *) 'mid real b and max real c'
            write(1, *) 'mid real b and max real c'
        else
            write(*, *) 'mid real c and max real b'
            write(1, *) 'mid real c and max real b'
        end if
    elseif ((b>c)) then
        write(*, *) 'min real c '
        if (b>a) then
            write(*, *) 'mid real a and max real b'
            write(1, *) 'mid real a and max real b'
        else
            write(*, *) 'mid real b and max real a'
            write(1, *) 'mid real b and max real a'
        end if
    else
        write(*, *) 'min real b'
        if (c>a) then
            write(*, *) 'mid real a and max real c'
            write(1, *) 'mid real a and max real c'
        else
            write(*, *) 'mid real c and max real a'
            write(1, *) 'mid real c and max real a'
        end if
    end if

    close(1)
end program L2T2