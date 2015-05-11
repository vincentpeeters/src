
    program test_lagrange
    use lagrange
    use handymanfunctions

    implicit none
    save
!    integer, parameter :: dp = kind(1.d0)
!    integer,parameter :: ip = selected_int_kind(15)

    real(dp) :: toll = 0.0001
    logical :: pass = .false.

    write(*,*) '*********LAGRANGE TESTS*********'

    pass =  test_initiateLagrange()
    pass = test_lagrangef()

    write(*,*) '**************END***************'

    contains


! *************** - TESTS - ************************

    logical function test_initiateLagrange() result(pass)
        integer :: n = 50
        real(dp), allocatable:: x(:), fx(:)
        integer:: i
        real(dp):: res = 0
        allocate(x(n))
        allocate(fx(n))
        pass = .false.
        do i = 1,n
            x(i)=real(i)/n
            fx(i) = square(x(i))
        enddo
        call initiateLagrange(x,fx)
        pass = .true.
        write(*,*) pass
    end function

    logical function test_lagrangef() result(pass)
        real(dp):: res = 0
        pass = .false.
        res = lagrangeValue(0.7071067812d0)
        pass = (abs(res - 0.5)<toll)
        write(*,*) pass
    end function


    end program