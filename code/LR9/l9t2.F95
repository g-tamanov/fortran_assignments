module find_sort_array
contains

    subroutine OnlySortForL8T2 (in_arr, out_arr, in_val)
        !возвращает индексы найденного значения из сорт масс
        integer, dimension (:), intent (in) :: in_arr
        integer, dimension (:), allocatable, intent (out) :: out_arr
        integer :: i, ii, iii, len, in_val, index
        len = size (in_arr)
        allocate (out_arr (1:len))

        ii = 1
        iii = size(out_arr)
        !раскидываем элементы между p
        do i = ii, size(in_arr)
            if(in_arr(i)>in_val) then
                out_arr(iii) = in_arr(i)
                iii = iii - 1
                !                print *, "in_arr(i)",in_arr(i),">=","in_val",in_val
            else if(in_arr(i)<in_val) then
                !                print *, "in_arr(i)",in_arr(i),"<","in_val",in_val
                out_arr(ii) = in_arr(i)
                ii = ii + 1
            end if
        end do
        !добавляем p элемента
        do i = ii, iii
            out_arr(i) = in_val
        end do
        call FindLocArray(in_arr, in_val, index) !находим первый индекс p-ый
        return
    end subroutine OnlySortForL8T2

    subroutine SwapElementArray (in_arr, i, ii)
        !меняет элементы массива местами
        integer in_arr(:)
        integer i, ii, temp
        temp = in_arr(i)
        in_arr(i) = in_arr(ii)
        in_arr(ii) = temp
        return
    end subroutine SwapElementArray
    subroutine SwapElementArrayReal (in_arr, i, ii)
        !меняет элементы массива местами
        real in_arr(:), temp
        integer i, ii
        temp = in_arr(i)
        in_arr(i) = in_arr(ii)
        in_arr(ii) = temp
        return
    end subroutine SwapElementArrayReal

    subroutine FindLocArray (in_arr, in_val, out_i)
        !меняет элементы массива местами
        integer in_arr(:)
        integer in_val, out_i, temp
        do i = 1, size(in_arr)
            if(in_arr(i)==in_val) then
                out_i = i
                !                print *, "out_i", out_i, "==", "i", i
                return
            end if
        end do
    end subroutine FindLocArray
    subroutine MaxValArrayReal (in_arr, out_i)
        !max val in array real
        real in_arr(:)
        integer in_val, out_i
        in_val = in_arr(1)
        out_i = 1
        do i = 1, size(in_arr)
            if(in_arr(i)>in_val) then
                in_val = in_arr(i)
                out_i = i
            end if
        end do
    end subroutine MaxValArrayReal
    subroutine MinValArrayReal (in_arr, out_i)
        !min val in array real
        real in_arr(:)
        integer in_val, out_i
        in_val = in_arr(1)
        out_i = 1
        do i = 1, size(in_arr)
            if(in_arr(i)<in_val) then
                in_val = in_arr(i)
                out_i = i
            end if
        end do
    end subroutine MinValArrayReal

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
                    call SwapElementArray(in_array, i, i + 1)
                    swap = .TRUE.
                end if
            end do
        end do
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
                    call SwapElementArray(in_array, i, i + 1)
                    swap = .TRUE.
                end if
            end do
        end do
        return
    end subroutine SortBubbleDESC

    subroutine SortBubbleRealASC (in_array)
        !сортировка пузырьком от миньшего к большему
        real in_array(:)
        integer k,kk, i
        logical swap
        swap = .TRUE.
        do while(swap)
            swap = .FALSE.
            do i = 1, size(in_array)
                k=k+1
                if(i + 1<=size(in_array) .and. in_array(i)>in_array(i + 1)) then
                    kk=kk+1
                    call SwapElementArrayReal(in_array, i, i + 1)
                    swap = .TRUE.
                end if
            end do
        end do
        write (*, *) "сравнения k= ", k
        write (1, *) "сравнения k= ", k
        write (*, *) "перестановки kk= ", kk
        write (1, *) "перестановки kk= ", kk
        return
    end subroutine SortBubbleRealASC
    subroutine SortBubbleRealDESC (in_array)
        !сортировка пузырьком от большего к меньшему
        real in_array(:)
        integer k,kk, i
        logical swap
        k=0
        kk=k
        swap = .TRUE.
        do while(swap)
            swap = .FALSE.
            do i = 1, size(in_array)
                k=k+1
                if(i + 1<=size(in_array) .and. in_array(i)<in_array(i + 1)) then
                    kk=kk+1
                    call SwapElementArrayReal(in_array, i, i + 1)
                    swap = .TRUE.
                end if
            end do
        end do
        write (*, *) "сравнения k= ", k
        write (1, *) "сравнения k= ", k
        write (*, *) "перестановки kk= ", kk
        write (1, *) "перестановки kk= ", kk
        return
    end subroutine SortBubbleRealDESC

    subroutine SortInsertRealASC(in_arr)
        implicit none
        integer :: i, j
        real :: in_arr(:), x

        do i = 2, size(in_arr)
            x = in_arr(i)
            j = i - 1
            do while (j >= 1)
                if (in_arr(j) <= x) exit
                in_arr(j + 1) = in_arr(j)
                j = j - 1
            end do
            in_arr(j + 1) = x
        end do
        return
    end subroutine SortInsertRealASC
    subroutine SortInsertRealDESC(in_arr)
        implicit none
        integer :: i, j
        real :: in_arr(:), x

        do i = 2, size(in_arr)
            x = in_arr(i)
            j = i - 1
            do while (j >= 1)
                if (in_arr(j) >= x) exit
                in_arr(j + 1) = in_arr(j)!
                j = j - 1
            end do
            in_arr(j + 1) = x!
        end do
        return
    end subroutine SortInsertRealDESC


end module find_sort_array

program L9T2
    use find_sort_array

    implicit none
    REAL x, matrix (10), matrixTemp (10)
    integer i, iMin, iMax
    OPEN (1, FILE = 'l9t2.txt', ACCESS = 'SEQUENTIAL', STATUS = 'unknown')

    do i = 1, size(matrix)
        CALL RANDOM_NUMBER(x)
        matrix(i) = anint(100 * x) - 50
    end do

    write (*, *) "Неотсортированный массив ", matrix
    write (1, *) "Неотсортированный массив ", matrix

    call MinValArrayReal(matrix, iMin)
    write (*, *) "Минимальный индекс: ", iMin, "значение: ", matrix(iMin)
    write (1, *) "Минимальный индекс: ", iMin, "значение: ", matrix(iMin)

    call MaxValArrayReal(matrix, iMax)
    write (*, *) "Максимальный индекс: ", iMax, "значение: ", matrix(iMax)
    write (1, *) "Максимальный индекс: ", iMax, "значение: ", matrix(iMax)

    matrixTemp = matrix
    call SortInsertRealASC(matrixTemp)
    write (*, *) "Сортировка вставкой возврастание", matrixTemp
    write (1, *) "Сортировка вставкой возврастание", matrixTemp

    matrixTemp = matrix
    call SortInsertRealDESC(matrixTemp)
    write (*, *) "Сортировка вставкой убывание", matrixTemp
    write (1, *) "Сортировка вставкой убывание", matrixTemp

    matrixTemp = matrix
    call SortBubbleRealASC(matrixTemp)
    write (*, *) "Сортировка пузырьком возврастание", matrixTemp
    write (1, *) "Сортировка пузырьком возврастание", matrixTemp

    matrixTemp = matrix
    call SortBubbleRealDESC(matrixTemp)
    write (*, *) "Сортировка пузырьком убывание", matrixTemp
    write (1, *) "Сортировка пузырьком убывание", matrixTemp

    close(1)

end program L9T2