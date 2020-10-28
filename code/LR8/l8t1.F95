module find_sort_array
contains

    subroutine FindIndexElementsArray (in_arr, out_arr, in_val)
        !возвращает индексы найденного значения из масс
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
    end subroutine FindIndexElementsArray

    subroutine FindIndexElementSortArray (in_arr, out_arr, in_val)
        !возвращает индексы найденного значения из сорт масс
        integer, dimension (:), intent (in) :: in_arr
        integer, dimension (:), allocatable, intent (out) :: out_arr
        integer :: i, ii, iii, len, in_val
        len = size (in_arr)
        allocate (out_arr (1:len))
        ii = 1
        do i = ii, size(in_arr)
            if(in_arr(i)==in_val) then
                out_arr(ii) = i
                ii = ii + 1
                print *, "добавлен индекс числа", in_val, "=", i
                write(1, *) "добавлен индекс числа", in_val, "=", i
            else if (ii>1) then
                write(*, *) "найдены все элементы в сортированном массе"
                write(1, *) "найдены все элементы в сортированном массе"
                return
            end if
        end do
        return
    end subroutine FindIndexElementSortArray

    subroutine RemoveSameElementsFromArray (in_arr, out_arr)
        !удаляет одинаковые элементы в массе
        integer, dimension (:), intent (in) :: in_arr
        integer, dimension (:), allocatable, intent (out) :: out_arr
        integer :: i, ii, len, in_val
        len = size (in_arr)
        allocate (out_arr (1:len))
        iii = 1
        do i = iii, size(in_arr)
            in_val = in_arr(i)
            do ii = 1, size(out_arr)
                if(in_arr(i)==out_arr(ii)) then
                    exit
                else if(size(out_arr)==ii) then
                    out_arr(iii) = in_arr(i)
                    iii = iii + 1
                end if
            end do
        end do
        return
    end subroutine RemoveSameElementsFromArray

    subroutine ArrayResize (in_arr, out_arr, out_arr_new_size)
        !меняет размер масс
        integer, dimension (:), intent (in) :: in_arr
        integer, dimension (:), allocatable, intent (out) :: out_arr
        integer :: i, out_arr_new_size
        allocate (out_arr (1:out_arr_new_size))
        do i = 1, out_arr_new_size
            out_arr(i) = in_arr(i)
        end do
        return
    end subroutine ArrayResize

    subroutine SortBubbleASC (in_array)
        !сортировка пузырьком от миньшего к большему
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
        write(*, *) "отсортированный масс ", in_array
        write(1, *) "отсортированный масс ", in_array
        return
    end subroutine SortBubbleASC

    subroutine SortBubbleDESC (in_array)
        !сортировка пузырьком от большего к меньшему
        integer in_array(:)
        integer temp, i
        logical swap
        swap = .TRUE.
        do while(swap)
            swap = .FALSE.
            do i = 1, size(in_array)
                if(i + 1<=size(in_array) .and. in_array(i)<in_array(i + 1)) then
                    temp = in_array(i)
                    in_array(i) = in_array(i + 1)
                    in_array(i + 1) = temp
                    swap = .TRUE.
                end if
            end do
        end do
        write(*, *) "отсортированный масс ", in_array
        write(1, *) "отсортированный масс ", in_array
        return
    end subroutine SortBubbleDESC

end module find_sort_array


program L8T1
    use find_sort_array

    implicit none
    integer, allocatable :: matrix(:), sortMatrix(:), sameMatrix(:), indexes(:)
    integer s, i, index_k
    real hav
    OPEN (1, FILE = 'l8t1.txt', ACCESS = 'SEQUENTIAL', STATUS = 'unknown')

    s = 100
    allocate(matrix(s))

    do i = 1, size(matrix, 1)
        call random_number(hav)
        matrix(i) = anint(hav * 10)
    end do

    write(*, *) "1) Неотсорт масс", matrix
    write(1, *) "1) Неотсорт масс", matrix

    write(*, *) "2) Задать k-ый номер яч масс"
    read (*, *)  index_k
    write(1, *) "2) k-ый номер яч масс", index_k

    sortMatrix = matrix
    call SortBubbleDESC(sortMatrix)
    write(*, *) "3) Отсортированный масс ", sortMatrix
    write(1, *) "3) Отсортированный масс ", sortMatrix

    call RemoveSameElementsFromArray(sortMatrix, sameMatrix)
    write(*, *) "4) Дополнительный массив из отсортированного", sameMatrix
    write(1, *) "4) Дополнительный массив из отсортированного", sameMatrix

    i = sameMatrix(index_k)
    write(*, *) "5) Элемент, находящийся на k-ой позиции доп масс", i
    write(1, *) "5) Элемент, находящийся на k-ой позиции доп масс", i

    call FindIndexElementsArray(matrix, indexes, i)
    write(*, *) "6) Неотсорт масс с индексами k-го", indexes
    write(1, *) "6) Неотсорт масс с индексами k-го", indexes

    call FindIndexElementSortArray(sortMatrix, indexes, i)
    write(*, *) "6) Отсорт масс с индексами k-го", indexes
    write(1, *) "6) Отсорт масс с индексами k-го", indexes

    close(1)

end program L8T1