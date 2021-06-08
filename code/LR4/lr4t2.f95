program lr4t1
    integer(4), parameter     :: n4=4, n5=5
    integer(4)                :: i, j,  n
    real(8)                   :: h, x0=0.0, xn=2.0, left_der, central_der, right_der
    real(8)                   :: first_y4(n4), first_y5(n5), second_y4(n4), second_y5(n5)
    real(8), allocatable      :: x(:), y(:)

    write(*,*) "Enter the size of the massiv y(x): "; read(*,*) n
    allocate(x(n+1), y(n+1))
    h=(xn-x0)/n
    x(1)=x0
    y(1)=Fun(x(1))
    do i=2, n+1
        x(i)=x(i-1)+h
        y(i)=Fun(x(i))
    end do
    write(*,*) "i", "            x(i)", "                   y(i)"
    do i=1, n+1
        write(*,*) i-1,  x(i), y(i)
    end do
    write(*,*)


    first_y4(1)=1.0/(6.0*h)*(-11*y(1)+18*y(2)-9*y(3)+2*y(4))
    first_y4(2)=1.0/(6.0*h)*(-2*y(1)-3*y(2)+6*y(3)-y(4))
    first_y4(3)=1.0/(6.0*h)*(y(1)-6*y(2)+3*y(3)+2*y(4))
    first_y4(3)=1.0/(6.0*h)*(-2*y(1)+9*y(2)-10*y(3)+11*y(4))

    first_y5(1)=1.0/(12*h)*(-25*y(1)+48*y(2)-36*y(3)+16*y(4)-3*y(5))
    first_y5(2)=1.0/(12*h)*(-3*y(1)-10*y(2)+18*y(3)-6*y(4)+y(5))
    first_y5(3)=1.0/(12*h)*(y(1)-8*y(2)+8*y(4)-2*y(5))
    first_y5(4)=1.0/(12*h)*(-y(1)+6*y(2)-18*y(3)+10*y(4)+3*y(5))
    first_y5(5)=1.0/(12*h)*(3*y(1)-16*y(2)+36*y(3)-48*y(4)+25*y(5))

    write(*,*) "first derivatives "
    write(*,*) "4 nodes"
    write(*,*) "i", "y`(i)"
    do i=1, n4
        write(*,*) i-1, first_y4(i)
    end do
    write(*,*)
    write(*,*) "5 nodes"
    write(*,*) "i", "         y`(i)"
    do i=1, n5
        write(*,*) i-1, first_y5(i)
    end do
    write(*,*)
    write(*,*)
    write(*,*) "Analytical value of the first derivative: "
    write(*,*) "4 nodes"
    write(*,*) "i", "         f`(i)"
    do i=1, n4
        write(*,*) i-1, FunDif(x(i))
    end do
    write(*,*)
    write(*,*) "5 nodes"
    write(*,*) "i", "         f`(i)"
    do i=1, n5
        write(*,*) i-1, FunDif(x(i))
    end do
    write(*,*)

    left_der=(y(2)-y(1))/h
    central_der=(y(3)-y(1))/(2.0*h)
    right_der=(y(3)-y(2))/h
    write(*,*) "left derivative = ", left_der
    write(*,*)  "Central derivative = ", central_der
    write(*,*) "Right derivative = ", right_der

    second_y4(1)=(2*y(1)-5*y(2)+4*y(3)-y(4))/h**2
    second_y4(2)=(y(1)-2*y(2)+y(3))/h**2
    second_y4(3)=(y(2)-2*y(3)+y(4))/h**2
    second_y4(4)=(-y(1)+4*y(2)-5*y(3)-2*y(4))/h**2

    second_y5(1)=(35*y(1)-104*y(2)+114*y(3)-56*y(4)+11*y(5))/(12*h*h)
    second_y5(2)=(11*y(1)-20*y(2)+6*y(3)+4*y(4)-y(5))/(12*h*h)
    second_y5(3)=(-y(1)+16*y(2)-30*y(3)+16*y(4)-y(5))/(12*h*h)
    second_y5(4)=(-y(1)+4*y(2)+6*y(3)-20*y(4)+11*y(5))/(12*h*h)
    second_y5(5)=(11*y(1)-56*y(2)+114*y(3)-104*y(4)+35*y(5))/(12*h*h)

    write(*,*) "Second derivatives "
    write(*,*) "4 nodes"
    write(*,*) "i", "           y``(i)"
    do i=1, n4
        write(*,*) i-1, second_y4(i)
    end do
    write(*,*)
    write(*,*) "5 nodes"
    write(*,*) "i", "           y``(i)"
    do i=1, n5
        write(*,*) i-1, second_y5(i)
    end do
    write(*,*)

    if(allocated(x)) deallocate(x)
    if(allocated(y)) deallocate(y)

contains

    real(8) function Fun(t)
        real(8), intent (in) :: t
        Fun=t**3
    end function Fun

    real(8) function FunDif(t)
        real(8), intent (in) :: t
        FunDif=3*t*t
    end function FunDif

end program lr4t1