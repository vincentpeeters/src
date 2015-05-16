    module polynomialModule
    use integrator
    use handymanfunctions

    implicit none
    save

!    polynonoal is presented in structre by its coeficients as follows
!   f(x) = coefficients(1) + coefficients(2)*x + .. + coefficients(size(coefficients))*x**(size(coefficients)-1)
    public Polynomial
    type Polynomial
        real(dp), allocatable :: coefficients(:)
    end type
    integer :: integrationSteps = 100
    type(Polynomial), private :: pol1, pol2

    contains

!***************** CREATION *********************

    type (Polynomial) function createMonominialdegree(n) result(pol)
        integer, intent(in):: n
        real(dp), allocatable :: coef(:)
        allocate(coef(n+1))
        coef = 0.0d0
        coef(n+1) = 1.0d0
        allocate(pol%coefficients(n+1))
        pol%coefficients = coef
    end function


!***************ACCESSORS******************

!    RETURNS THE COEFICIENT BEGLONING TO X^i 
    real(dp) function getPolCo(pol,i) result(res)
        type (Polynomial):: pol
        integer, intent(in):: i
        res = pol%coefficients(i+1)
    end function

!    SETS THE COEFICIENT BEGLONING TO X^i
    subroutine setPolCo(pol,i,coef)
        type (Polynomial):: pol
        integer, intent(in) :: i
        real(dp), intent(in) :: coef
        pol%coefficients(i+1) = coef
    end subroutine


    real(dp) function getPolVal(pol,x) result(res)
        type (Polynomial):: pol
        real(dp), intent(in) :: x
        integer :: i
        res = 0.0d0
        do i = 0,size(pol%coefficients-1)
            res = res + getPolCo(pol,i)*(x**i)
        enddo
    end function

    subroutine printPol(pol)
    type (Polynomial), intent(in):: pol
    integer:: i
    write(*,*) pol%coefficients
    end subroutine

!******************GEOMETRIC STUFF ***************
    real(dp) function polNorm(weightFunction, li, ri, pol) result(res)
        real(dp), external:: weightFunction
        real(dp) ::li,ri
        type (Polynomial) :: pol
        pol1 = pol
        res = samengesteldeTrapezium(tempPolFunc1,weightFunction,li,ri,integrationSteps)
    end function

    real(dp) function polDot(weightFunction, li, ri, pol_1,pol_2) result(res)
        real(dp), external:: weightFunction
        real(dp), intent(in) ::li,ri
        type (Polynomial) :: pol_1, pol_2
        pol1=pol_1
        pol2=pol_2
        res = samengesteldeTrapezium(tempPolFunc2,weightFunction,li,ri,integrationSteps)
    end function

!******************SUBSTRACTION, DEVISION*******************
    type (Polynomial) function devidePol(pol, quot) result(res)
        type (Polynomial) :: pol
        real(dp) :: quot
        allocate(res%coefficients(size(pol%coefficients)))
        res%coefficients = pol%coefficients/quot
    end function

    type (Polynomial) function multPol(pol, factor) result(res)
        type (Polynomial) :: pol
        real(dp) :: factor
        allocate(res%coefficients(size(pol%coefficients)))
        res%coefficients = pol%coefficients*factor
    end function

    type (Polynomial) function subPol(pol1, pol2) result(res)
        type (Polynomial) :: pol1, pol2
        allocate(res%coefficients(size(pol1%coefficients)))
        res%coefficients = pol1%coefficients - pol2%coefficients
    end function

    type (Polynomial) function addPol(pol1, pol2) result(res)
        type (Polynomial) :: pol1, pol2
        allocate(res%coefficients(size(pol1%coefficients)))
        res%coefficients = pol1%coefficients + pol2%coefficients
    end function

!******************private temp function stuff***************

    real(dp) function tempPolFunc1(x) result(res)
        real(dp) :: x
        res = getPolVal(pol1,x)
    end function

    real(dp) function tempPolFunc2(x) result(res)
        real(dp):: x
        res = getPolVal(pol2,x)*getPolVal(pol1,x)
!        write(*,*) res 
    end function


    end module polynomialModule

    