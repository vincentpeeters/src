module functie_mod

type functie
   contains
        procedure :: value
end type functie

type, extends(functie) :: veelterm
        integer :: graad
        real(dp), allocatable :: coefficients(:)
end type veelterm

!type, extends(rectangle) :: square
!end type square


contains
subroutine value(f,x)
class(functie) :: f
real(dp) :: x
integer :: i
select type (f)
!type is (shape)
!      ! no further initialization required
class is (veelterm)
        x = 0.0d0
        do i = 0,size(pol%coefficients-1)
            x = x + getPolCo(pol,i)*(x**i)
        enddo
class default
  ! give error for unexpected/unsupported type
     stop 'initialize: unexpected type for sh object!'
end select
end subroutine initialize
end module