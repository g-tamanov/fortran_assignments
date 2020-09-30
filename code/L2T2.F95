program L2T1
    real a, b, c, x

    write(*, *) 'write a,b,c '
    write(*, *) 'write a'
    read(*, *) a
    write(*, *) 'a=', a
    write(*, *) 'write b'
    read(*, *) b
    write(*, *) 'b=', b
    write(*, *) 'write c'
    read(*, *) c
    write(*, *) 'c=', c

    if ((a<b) .and. (a<c)) then
        write(*, *) 'min real a'
        if (b<c) then
            write(*, *) 'mid real b and max real c'
        else
            write(*, *) 'mid real c and max real b'
        end if
    elseif ((b>c)) then
        write(*, *) 'min real c '
        if (b>a) then
            write(*, *) 'mid real a and max real b'
        else
            write(*, *) 'mid real b and max real a'
        end if
    else
        write(*, *) 'min real b'
        if (c>a) then
            write(*, *) 'mid real a and max real c'
        else
            write(*, *) 'mid real c and max real a'
        end if
    end if

end program L2T1