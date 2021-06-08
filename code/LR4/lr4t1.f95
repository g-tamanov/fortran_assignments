program lr4t1
    integer(4), parameter     :: n4=4, n5=5
    integer(4)                :: i, j,  n
    real(8)                   :: h, x0=0.0, xn=2.0, left_der, central_der, right_der
    real(8)                   :: first_y4(n4), first_y5(n5), second_y4(n4), second_y5(n5)
    real(8), allocatable      :: x(:), y(:)

    OPEN (1, fILE = 'lr4t1.txt', aCCESS = 'SEQUENTIaL', STaTUS = 'unknown')

    write(*,*) "------дифференцирование----------"
    write(1,*) "------дифференцирование----------"
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

    write(*,*) "=0-й=                    =1-й=                      =2-й=                      =3-й=                      =4-й"
    write(1,*) "=0-й=                    =1-й=                      =2-й=                      =3-й=                      =4-й"
    write(*,*) "==== первые производные==="
    write(1,*) "==== первые производные==="
    write(*,10)
    write(1,10)
    write(*,*) "i", "y`(i)"
!    do i=1, n4
        write(*,*) first_y4(1) , first_y4(2), first_y4(3), first_y4(4)
        write(1,*) first_y4(1) , first_y4(2), first_y4(3), first_y4(4)
!    end do
    write(*,*)
    write(*,11)
    write(1,11)
    write(*,*) "i", "         y`(i)"
!    do i=1, n5
        write(*,*) first_y5(1), first_y5(2), first_y5(3), first_y5(4), first_y5(5)
        write(1,*) first_y5(1), first_y5(2), first_y5(3), first_y5(4), first_y5(5)
!    end do
    write(*,*)
    write(*,*)
    write(*,*) "Аналитическое значение первой производной: "
    write(1,*) "Аналитическое значение первой производной: "
    write(*,10)
    write(*,*) "i", "         f`(i)"
    write(1,*) "i", "         f`(i)"
!    do i=1, n4
        write(*,*) FunDif(x(1)), FunDif(x(2)), FunDif(x(3)), FunDif(x(4))
        write(1,*) FunDif(x(1)), FunDif(x(2)), FunDif(x(3)), FunDif(x(4))
!    end do
    write(*,*)
    write(*,11)
    write(1,11)
    write(*,*) "i", "         f`(i)"
    write(1,*) "i", "         f`(i)"
!    do i=1, n5
        write(*,*) FunDif(x(1)), FunDif(x(2)), FunDif(x(3)), FunDif(x(4)), FunDif(x(5))
        write(1,*) FunDif(x(1)), FunDif(x(2)), FunDif(x(3)), FunDif(x(4)), FunDif(x(5))
!    end do
    write(*,*)

    left_der=(y(2)-y(1))/h
    central_der=(y(3)-y(1))/(2.0*h)
    right_der=(y(3)-y(2))/h
    write(*,*) "==== 2_л =  = ", left_der
    write(1,*) "==== 2_л =  = ", left_der
    write(*,*) "==== 2_ц =  = ", central_der
    write(1,*) "==== 2_ц =  = ", central_der
    write(*,*) "==== 2_п =  = ", right_der
    write(1,*) "==== 2_п =  = ", right_der

    second_y4(1)=(2*y(1)-5*y(2)+4*y(3)-y(4))/h**2
    second_y4(2)=(y(1)-2*y(2)+y(3))/h**2
    second_y4(3)=(y(2)-2*y(3)+y(4))/h**2
    second_y4(4)=(-y(1)+4*y(2)-5*y(3)-2*y(4))/h**2

    second_y5(1)=(35*y(1)-104*y(2)+114*y(3)-56*y(4)+11*y(5))/(12*h*h)
    second_y5(2)=(11*y(1)-20*y(2)+6*y(3)+4*y(4)-y(5))/(12*h*h)
    second_y5(3)=(-y(1)+16*y(2)-30*y(3)+16*y(4)-y(5))/(12*h*h)
    second_y5(4)=(-y(1)+4*y(2)+6*y(3)-20*y(4)+11*y(5))/(12*h*h)
    second_y5(5)=(11*y(1)-56*y(2)+114*y(3)-104*y(4)+35*y(5))/(12*h*h)

    write(*,*) "====  вторые производные====="
    write(1,*) "====  вторые производные====="
    write(*,10)
    write(1,10)
    write(*,*) "i", "           y``(i)"
!    do i=1, n4
        write(*,*) second_y4(1), second_y4(2),second_y4(3),second_y4(4)
        write(1,*) second_y4(1), second_y4(2),second_y4(3),second_y4(4)
!    end do
    write(*,*)
    write(*,11)
    write(1,11)
    write(*,*) "i", "           y``(i)"
!    do i=1, n5
        write(*,*) second_y5(1),second_y5(2),second_y5(3),second_y5(4),second_y5(5)
        write(1,*) second_y5(1),second_y5(2),second_y5(3),second_y5(4),second_y5(5)
!    end do
    write(*,*)

    10 format("==== 4 узла")
    11 format("==== 5 узлов")

    if(allocated(x)) deallocate(x)
    if(allocated(y)) deallocate(y)

    close (1)
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