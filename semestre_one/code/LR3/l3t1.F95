program L3T1
    integer x
    OPEN (1, FILE = 'l3t1.txt', ACCESS = 'SEQUENTIAL', STATUS = 'unknown')

    write(*, *) 'введите номер месяца'
    read(*, *) x
    write(1, *) 'номер месяца', x

    Select case (x)
    case (1:2, 12)
        write(*, *) "зима"
        write(1, *) "зима"
    case (3:5)
        write(*, *) "весна"
        write(1, *) "весна"
    case (6:8)
        write(*, *) "лето"
        write(1, *) "лето"
    case (9:11)
        write(*, *) "осень"
        write(1, *) "осень"
    End Select
    close(1)
end program L3T1