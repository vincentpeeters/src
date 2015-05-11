
    program test_lagrange

    use lagrange
    use handymanfunctions
    implicit none

    real :: toll = 0.01

    write(*,*) test_initiateLagrange()
    write(*,*) lagrangeValue(1.0)

    contains






! *************** - TESTS - ************************

    logical function test_initiateLagrange()
        real:: x(100), fx(100) = 0
        integer:: i
        real:: res = 0
        do i = 1,100
            x(i)=real(i)/100
            fx(i) = square(x(i))
        enddo
        call initiateLagrange(x,fx)
        test_initiateLagrange = .true.
    end function

!    logical function test_lagrangef()
!        real:: res = 0
!        call initiateLagrange(x,fx)
!        test_lagrangef = ((res - 1.0)<toll)
!    end function


    end program