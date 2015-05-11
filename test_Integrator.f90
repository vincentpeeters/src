
    program test_Integrator

    use integrator
    use handymanfunctions
    implicit none

    real :: toll = 0.0001

    write(*,*) test_samengesteldeTrapezium()
    write(*,*) test_samengesteldeTrapeziumVastePunten()
    write(*,*) test_gaussKwadratuur()

    contains






! *************** - TESTS - ************************

    logical function test_samengesteldeTrapeziumVastePunten()
        real:: x(100), fx(100) = 0
        integer:: i
        real:: res = 0
        do i = 1,100
            x(i)=real(i)/100
            fx(i) = square(x(i))
        enddo
        res =  samengesteldeTrapeziumVastePunten(x,fx)
        test_samengesteldeTrapeziumVastePunten = res - 1.0/3.0 < toll
    end function

    logical function test_samengesteldeTrapezium()
        integer:: i
        real:: res
        res = samengesteldeTrapezium(square,const_1,0.0,1.0,100)
        test_samengesteldeTrapezium = res - 1.0/3.0 < toll
    end function

    logical function test_gaussKwadratuur()
        real :: x=5
        integer:: i
        real:: res = 0
        res = gaussKwadratuur(square,0.0,1.0,1.0,1.0,100)
        test_gaussKwadratuur = res - 1.0/3.0 < toll
    end function




    end program