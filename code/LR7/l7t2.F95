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

    subroutine findElementsSort (in_arr, out_arr, in_val)
        integer, dimension (:), intent (in) :: in_arr
        integer, dimension (:), allocatable, intent (out) :: out_arr
        integer :: i, ii, len, in_val
        len = size (in_arr)
        allocate (out_arr (1:len))
        ii = 1
        do i = ii, size(in_arr)
            if(in_arr(i)==in_val ) then
                out_arr(ii) = i
                ii = ii + 1
                print *, "добавлен индекс числа", in_val, "=", i
                write(1, *) "добавлен индекс числа", in_val, "=", i
            else if (ii>1) then
                write(*, *) "найдены все элементы в сортированном массиве"
                write(1, *) "найдены все элементы в сортированном массиве"
                return
            end if
        end do
        return
    end subroutine findElementsSort

    subroutine sort(in_array)
        integer in_array(:)
        integer temp, i
        logical swap
        swap = .TRUE.
        do while(swap)
            swap = .FALSE.
            do i = 1, size(in_array)
                if(i + 1<=size(in_array) .and. in_array(i)>in_array(i + 1)) then
                    temp = in_array(i)
                    in_array(i) = in_array(i + 1)
                    in_array(i + 1) = temp
                    swap = .TRUE.
                end if
            end do
        end do
        write(*, *) "отсортированный массив ",in_array
        write(1, *) "отсортированный массив ",in_array
        return
    end subroutine sort

end module find_sort_array


program L7T2
    use find_sort_array

    implicit none
    integer, allocatable :: matrix(:), result(:)
    integer s, i, num
    real hav
    OPEN (1, FILE = 'l7t2.txt', ACCESS = 'SEQUENTIAL', STATUS = 'unknown')

    s = 100
    allocate(matrix(s))

    write(*, *) "введите число от 0 до 10"
    read (*, *)  num
    write(1, *) "введите число от 0 до 10", num

    do i = 1, size(matrix, 1)
        call random_number(hav)
        matrix(i) = anint(hav * 10)
    end do

    write(*, *) " массив",matrix
    write(1, *) " массив",matrix
    call sort(matrix)
    call findElementsSort(matrix,result, num)
    write(*, *) "индексы чисел",result
    write(1, *) "индексы чисел",result
    close(1)

end program L7T2