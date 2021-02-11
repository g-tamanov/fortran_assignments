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
                out_arr(i) = i
                ii = ii + 1
            else
                out_arr(i) = 0
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
    write(1, *) "введите число от 0 до 10 ",num

    do i = 1, size(matrix, 1)
        call random_number(hav)
        matrix(i) = anint(hav * 10)
    end do

    call findElements(matrix,result, num)
    write(*, *) "массив:"
    write(1, *) "массив:"
    write(*,2)(matrix(i),i=1,size(matrix))
    write(1,2)(matrix(i),i=1,size(matrix))
    write(*, 3) num
    write(1, 3) num
    write(*,2)(result(i),i=1,size(result))
    write(1,2)(result(i),i=1,size(result))

    2 format(10(2x, i2))
    3 format(" индексы числа: ", i2)

    close(1)

end program L7T1