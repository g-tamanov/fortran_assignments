module find_sort_array
contains

    subroutine findElements (in_arr, out_arr, in_val)
        integer, dimension (:), intent (in) :: in_arr
        integer, dimension (:), allocatable, intent (out) :: out_arr
        integer :: i, ii, len, in_val
        len = size (in_arr)
        allocate (out_arr (1:len))
        ii = 1
        do i = ii, size(in_arr)
            if(in_arr(i)==in_val) then
                out_arr(ii) = i
                ii = ii + 1
                print *, "добавлен индекс числа", in_val, "=", i
                write(1, *) "добавлен индекс числа", in_val, "=", i
            end if
        end do
        return
    end subroutine findElements

end module find_sort_array


program L7T1
    use find_sort_array

    implicit none
    integer, allocatable :: matrix(:), result(:)
    integer s, i,num
    real hav
    OPEN (1, FILE = 'l7t1.txt', ACCESS = 'SEQUENTIAL', STATUS = 'unknown')

    s = 100
    allocate(matrix(s))

    write(*, *) "введите число от 0 до 10"
    read (*,*)  num
    write(1, *) "введите число от 0 до 10",num

    do i = 1, size(matrix, 1)
        call random_number(hav)
        matrix(i) = anint(hav * 10)
    end do

    write(*,'(I6)') matrix
    write(1,'(I6)') matrix

    call findElements(matrix,result, num)
    write(*, *) "индексы чисел",result
    write(1, *) "индексы чисел",result

    close(1)

end program L7T1