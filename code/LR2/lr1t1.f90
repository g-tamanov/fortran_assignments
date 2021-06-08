!Функция
real  function f(x)
    real, intent (in) :: x
    !    fx = log(1 + x**2)
    fx = sinx
end function f

program l2t1

    integer:: n, i
    real:: a, b, x(0:10), y(0:10), dy(0:9), ddy(0:8), h, a0(0:8), a1(0:8), a2(0:8), xk, P

    open(12,FILE='lab21_test.txt')

    write(12,*)'Задание: провести интерполяцию тестовой функции sinx'
    write(12,*)'         методом Ньютона'

    write(*,*)'Введите начальную координату x = a'
    read(*,*) a
    write(12,*)'Введите начальную координату x = a'
    write(12,*) a

    write(*,*)'Введите конечную координату x = b'
    read(*,*) b
    write(12,*)'Введите конечную координату x = b'
    write(12,*) b

    write(*,*)'Введите количество узлов n'
    read(*,*) n
    write(12,*)'Введите количество узлов n'
    write(12,*) n

    write(*,*)' '
    write(12,*)' '

    h = (b-a)/n
    x(0) = a
    y(0) = f(x(0))

    do i=1, n
        x(i)=x(i-1)+h
        y(i) = f(x(i))
    enddo

    do i = 0, n-1
        dy(i) = y(i+1) - y(i);
    enddo

    do i = 0, n-2
        ddy(i) = dy(i+1) - dy(i);
        a0(i) = y(i);
        a1(i) = dy(i) / h;
        a2(i) = ddy(i) / (2 * h**2);
    enddo
    write(*,*)' x         y         dy        ddy        a0        a1        a2'
    write(12,*)' x         y         dy        ddy        a0        a1        a2'

    do i=0, n
        if(i<9) then
            write(*, 10)(x(i)),(y(i)),(dy(i)),(ddy(i)),(a0(i)),(a1(i)),(a2(i))
            write(12, 10)(x(i)),(y(i)),(dy(i)),(ddy(i)),(a0(i)),(a1(i)),(a2(i))
        else
            if(i<10) then
                write(*, 11)(x(i)),(y(i)),(dy(i))
                write(12, 11)(x(i)),(y(i)),(dy(i))
            else
                write(*, 12)(x(i)),(y(i))
                write(12, 12)(x(i)),(y(i))
            endif
        endif

    enddo

    10 format(f6.3,'  ', f8.5,'  ', f8.5,'   ', f8.5,'   ', f8.5,'   ', f8.5,'   ', f8.5)
    11 format(f6.3,'  ', f8.5,'  ', f8.5)
    12 format(f6.3,'  ', f8.5)

    write(*,*)
    write(*,*)
    write(12,*)
    write(12,*)

    write(*,*)' x1        y1        p        dp'
    write(12,*)' x1        y1        p        dp'


    do i=0, n-2
        xk = (x(i) + x(i+1))/2;
        P = a0(i) + a1(i)*(xk - x(i)) + a2(i)*(xk - x(i))*(xk - x(i+1));
        write (*, 13)(xk),(f(xk)),(P),(F(xk)-P)
        write (12, 13)(xk),(f(xk)),(P),(F(xk)-P)
    enddo

    13 format(f6.3,'  ', f8.5,'  ', f8.5,'   ', f8.5)

end
