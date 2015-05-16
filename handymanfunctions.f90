    module handymanfunctions
    implicit none
    save
    integer, parameter :: dp = kind(1.d0)
    integer,parameter :: ip = selected_int_kind(15)
	
    contains

    real(dp) function square(x)
        real(dp), intent(in):: x
        square = x*x
    end function

    real(dp) function const_1(x)
        real(dp), intent(in):: x
        const_1 = 1.0d0
    end function

    real(dp) function VIPsquare(x)
        real(dp), intent(in):: x
        VIPsquare = x*x
    end function

    end module handymanfunctions
