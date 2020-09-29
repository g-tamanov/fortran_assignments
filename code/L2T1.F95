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

    if ((a>b) .and. (a>c)) then
        write(*, *) 'max real a'
    elseif ((b>c)) then
        write(*, *) 'max real b'
    else
        write(*, *) 'max real c'
    end if

end program L2T1