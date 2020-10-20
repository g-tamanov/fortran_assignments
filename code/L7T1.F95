module m
contains
    subroutine f(array,val)
        integer array(:)
        integer val,i
        do i = 1, size(array)
            if(array(i)==val) then
                print *, "индекс числа", val,"=",i
            end if
        end do
    end subroutine f

end module m


program L7T1
    use m

    implicit none
    external findLocations
    integer, allocatable :: matrix(:)
    integer s, i,num
    real hav
    s = 100
    allocate(matrix(s))

    print *,"введите число от 0 до 10"
    read (*,*)  num

    do i = 1, size(matrix, 1)
        call random_number(hav)
        matrix(i) = anint(hav * 10)
    end do

    call f(matrix,num)

end program L7T1