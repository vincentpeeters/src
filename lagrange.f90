    module lagrange
	implicit none
	save

    real, private, allocatable :: x_(:),fx_(:),l(:)

    public :: lagrangeValue
!    private ::

	contains

    subroutine initiateLagrange(x,fx)
    real, intent(in):: x(:),fx(:)
    integer :: n,i,j

    n = size(x)
    allocate(x_(n))
    allocate(fx_(n))
    allocate(l(n))
    x_ = x
    fx_ = fx
    do i = 1, n
        l(i)= 1.0
        do j = 1,n
            if (i  .ne. j) then
                l(i)=l(i)/(x(i)-x(j))
            endif
        enddo
    enddo
    end subroutine

    function lagrangeValue(x) result(res)
        real, intent(in)::x
        real :: res,mi, resu, resl
        integer::i

        resu = 0
        resl = 0
        do i = 1,size(x_)
            mi = l(i)/(x-x_(i))
            resu = resu+(mi*fx_(i))
            resl = resl+mi
        enddo
        res = resu/resl
    end function


!    function lagrange_interpolation(f,w,a,b,steps) result(res)
!		real, intent(in):: a,b
!		integer, intent(in):: steps
!        real, external:: f
!        real, external:: w
!        integer ::i
!        real:: res
!        real:: dx
!        dx = (b-a)/steps
!        res = 0
!        do i = 1,steps
!            res  = res + dx*(f((i-1)*dx)*w((i-1)*dx)+f(i*dx)*w(i*dx))/2
!        end do
!	end function

    end module lagrange

    