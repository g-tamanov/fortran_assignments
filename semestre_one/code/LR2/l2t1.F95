program L2T1
    real a, b, c
    OPEN (1, FILE = 'l2t1.txt', ACCESS = 'SEQUENTIAL', STATUS = 'unknown')

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
    elseif ((b>c)) then
        write(*, *) 'min real c '
        write(1, *) 'min real c '
    else
        write(*, *) 'min real b'
        write(1, *) 'min real b'
    end if
    close(1)
end program L2T1