program L3T3
    integer x
    OPEN (1, FILE = 'l3t3.txt', ACCESS = 'SEQUENTIAL', STATUS = 'unknown')

    write(*, *) 'Введите возраст'
    read(*, *) x
    write(1, *) 'возраст',x

    Select case (x)
    case (0)
        write(*, *) "Мне меньше года"
        write(1, *) "Мне меньше года"
    case (1, 21, 31, 41, 51, 61, 71, 81, 91)
        write(*, *) "Мне ", x, " год"
        write(1, *) "Мне ", x, " год"
    case (2:4, 22:24, 32:34, 42:44, 52:54, 62:64, 72:74, 82:84, 92:94)
        write(*, *) "Мне ", x, " года"
        write(1, *) "Мне ", x, " года"
    case (5:20, 25:30, 35:40, 45:50, 55:60, 65:70, 75:80, 85:90, 95:100)
        write(*, *) "Мне ", x, " лет"
        write(1, *) "Мне ", x, " лет"
    End Select
    close(1)
end program L3T3