    program test_lagrange

    use polynomialModule
    use handymanfunctions

    implicit none
    save
!    integer, parameter :: dp = kind(1.d0)
!    integer,parameter :: ip = selected_int_kind(15)

    real :: toll = 0.001d0
    logical :: pass = .false.

    write(*,*) '*********POLYNOMIAL TESTS*********'

    pass =  test_createPolynomial()
    pass =  test_createPolynomial2()
    pass =  test_createPolynomial3()
    pass =  test_polVal()
    pass =  test_dotproduct()

    write(*,*) '**************END***************'

    contains


! *************** - TESTS - ************************

    logical function test_createPolynomial() result(pass)
        integer :: degree = 4
        real(dp), allocatable:: x(:)
        real(dp):: res = 0.0d0
        type (Polynomial) :: pol
        allocate(x(degree+1))
        pass = .false.
        x = 1.0d0
        allocate( pol%coefficients(degree+1))
        pol%coefficients = x
        res = getPolVal(pol,1.0d0)
        pass = abs(res - 5.0d0) < toll
!        call printPol(pol)
        write(*,*) pass
    end function

    logical function test_createPolynomial2() result(pass)
        integer :: degree = 4
        real(dp), allocatable:: x(:)
        real(dp):: res = 0.0d0
        type (Polynomial) :: pol
        allocate(x(degree+1))
        pass = .false.
        x = 0.0d0
        allocate( pol%coefficients(degree+1))
        pol%coefficients = x
        call setPolCo(pol,2,1.0d0)
        res = getPolVal(pol,2.0d0)
        pass = abs(res - 4.0d0) < toll
!        call printPol(pol)
        write(*,*) pass
    end function

    logical function test_createPolynomial3() result(pass)
        integer :: degree = 3
        real(dp), allocatable:: x(:)
        real(dp):: res = 0.0d0
        type (Polynomial) :: pol
        allocate(x(degree+1))
        pass = .false.
        allocate( pol%coefficients(degree+1))
        pol%coefficients = x
        call setPolCo(pol,3,5.35d0)
        call setPolCo(pol,2,874.0d0)
        call setPolCo(pol,1,97.324d0)
        call setPolCo(pol,0,23.0d0)
        res = getPolVal(pol,1.5d0)
        pass = abs(res - 2153.54d0) < 1.0d0
!        call printPol(pol)
        write(*,*) pass
    end function

    logical function test_polVal() result(pass)
        type (POLYNOMIAL) :: pol1, pol2
        real(dp) :: res = 0.0d0
        pass = .false.
        pol1 = createMonominialdegree(0)
        pol2 = createMonominialdegree(0)
!        call printPol(pol1)
!        call printPol(pol2)
!        res = polDot(const_1,0.0,1.0,pol1,pol2)
        res = getPolVal(pol1,0.355835665d0)
        call printPol(pol1)
        write(*,*) res
        pass = abs(res - 1.0d0 )<toll
        write(*,*) pass
    end function

    logical function test_dotproduct() result(pass)
        type (POLYNOMIAL) :: pol1, pol2
        real(dp) :: res = 0.0d0
        pass = .false.
        pol1 = createMonominialdegree(0)
        pol2 = createMonominialdegree(0)
!        call printPol(pol1)
!        call printPol(pol2)
        res = polDot(const_1,0.0d0,1.0d0,pol1,pol2)
        write(*,*) res
        pass = abs(res - 1.0d0 )<toll
        write(*,*) pass
    end function




    end program