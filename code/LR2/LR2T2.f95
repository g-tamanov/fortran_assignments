!Функция
real  function f(x)
    real, intent (in) :: x
    !    fx = log(1 + x**2)
    fx = sin(x)
end function f
program lr2t2
    integer, parameter :: size = 100
    real x(0:size), y(0:size), x1(0:size), y1(0:size)
    real r(0:size, 0:size), p(0:size)

    OPEN (1, fILE = "lr2t2.txt ", ACCESS = "SEQUENTIAL ", STATUS = "unknown ")

    write(*, *) "Введите начальную,конечную координату и кол-во узлов "
    read(*, *) xn, xk, n
    write(*, 30) " ", "====INTRPOLATION LAGRANG=====", " ", "interval - [a,b],  number points - n"
    write(1, 30) " ", "====INTRPOLATION LAGRANG=====", " ", "interval - [a,b],  number points - n"
    write (*, *) xn, xk, n
    write (1, *) xn, xk, n

    if(xk<xn .or. xn==xk) then
        print *, "--------------------------"
        print *, "ОШИБКА: начальная координата должно быть больше конечной !!!"
        print *, "--------------------------"
        call EXIT(1)
    else if(n<=0) then
        print *, "--------------------------"
        print *, "ОШИБКА: n должно быть больше 0 !!!"
        print *, "--------------------------"
        call EXIT(1)
    else
        h = (xk - xn) / n
    end if

    do i = 0, n
        x(i) = h * i
        y(i) = f(x(i))
    end do

    do i = 0, n - 1
        x1(i) = h * i + h / 2
        y1(i) = f(x1(i))
    end do

    do k = 0, n - 1
        do i = 0, n
            u = 1
            d = 1
            do j = 0, n
                if (i/=j) then
                    u = u * (x1(k) - x(j))
                    d = d * (x(i) - x(j))
                end if
            end do
            r(k, i) = u / d
        end do
    end do

    do k = 0, n - 1
        sum = 0
        do i = 0, n
            sum = sum + y(i) * r(k, i)
        end do
        p(k) = sum
    end do

    write(*, 40)" ", "tables x,y and tables intermediate points x1,y1", "    x         y       x1        y1"
    write(1, 40)" ", "tables x,y and tables intermediate points x1,y1", "    x         y       x1        y1"

    do i = 0, n
        write(*, 10) x(i), y(i), x1(i), y1(i)
        write(1, 10) x(i), y(i), x1(i), y1(i)
    end do
    write(*, 40)" ", "tables polinom p and test function y1", "        p               y1"
    write(1, 40)" ", "tables polinom p and test function y1", "        p               y1"

    do j = 0, n - 1
        write(*, 20)  p(j), y1(j)
        write(1, 20)  p(j), y1(j)
    end do

    !formats
    10 format(f9.5, f9.5, f9.5, f9.5)
    20 format(e16.7, e15.7)
    30 format(A,/,A,/,A,/,A)
    40 format(A,/,A,/,A)
    !formats end

    close(1)

end program lr2t2

