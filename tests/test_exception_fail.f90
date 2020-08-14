module test_exception_fail

  implicit none

contains

  subroutine test_throw(status)
    !! Test function that throws an exception and exits

    use exceptions, ONLY: throw, new_exception, error_status

    ! Arguments
    class(error_status),allocatable,optional::status
        !! Error status

    call throw(status,new_exception("An error occurred"))
    return

  end subroutine test_throw

  subroutine test_fail()
    !! Test function that calls an exception-throwing procedure and ignores the
    !! exception, triggering an fatal error.

    use exceptions, ONLY: exception,error_status

    class(error_status),allocatable::status
        !! Error status

    call test_throw(status)

    select type(status)
    class is(exception)
       print *,"Ignored error:",status%message
    end select

  end subroutine test_fail

end module test_exception_fail

program test_exception

  use test_exception_fail, ONLY: test_fail

  call test_fail()

end program test_exception
