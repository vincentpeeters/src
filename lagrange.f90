    module lagrange
    use handymanfunctions
    use polynomialModule
!    use lapack

	implicit none
	save

    real(dp), private, allocatable :: x_(:),fx_(:),l(:)

    public :: lagrangeValue

	contains

!   Initialize the module with known points from a function to interpolate later
!   x   (real(:)): known points of function to interpolate
!   fx  (real(:)): known values of function to interpolate
!
!   post: module is ready to give interpolated vlues using the function lagrangeValue
    subroutine initiateLagrange(x,fx)
    real(dp), intent(in):: x(:),fx(:)
    integer :: n,i,j
    real(dp) :: quotient

    n = size(x)
    allocate(x_(n))
    allocate(fx_(n))
    allocate(l(n))
    x_ = x
    fx_ = fx
    do i = 1,n
        quotient= 1.0
        do j = 1,n
            if (i  .ne. j) then
                quotient=quotient*(x(i)-x(j))
            endif
        enddo
        l(i) = 1.0/quotient
    enddo
    end subroutine

!   Gives the interpolated value of the function given earlier at point x
!   x   (real): point at which we widh to interpolate
!
!   return: The interpolated value accoring to the points given earlier to the function initiateLagrange
    function lagrangeValue(x) result(res)
        real(dp), intent(in)::x
        real(dp) :: res,mi, resu, resl
        integer::i

        resu = 0.0d0
        resl = 0.0d0
        do i = 1,size(x_)
            mi = l(i)/(x-x_(i))

            resu = resu+(mi*fx_(i))
            resl = resl+mi
        enddo
        res = resu/resl
    end function




    type (polynomial) function getLagrangePol(x,fx) result(res)
        real(dp), intent(in):: x(:),fx(:)
        real(dp), allocatable:: A(:,:), pivot(:)
        logical :: ok = .false.
        integer :: N,i
        N = size(x)
        allocate(A(N,N))
        allocate(pivot(N))
        do i = 1,N
            A(:,i) = x**(i-1)
        enddo
!        TODO RETURN POLYNOMIAL
!        call SGESV(3, 1, A, 3, pivot, fx, 3, ok)
        allocate(res%coefficients(N))
        res%coefficients = fx
    end function

    end module lagrange

    