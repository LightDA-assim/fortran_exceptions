module test_exception_fail

  implicit none

contains

  subroutine test_throw(status)
    use exceptions, ONLY: throw, new_exception, error_status

    class(error_status),allocatable,optional::status

    call throw(status,new_exception("An error occurred"))
    return

  end subroutine test_throw

  subroutine test_fail()
    use exceptions, ONLY: exception,error_status

    class(error_status),allocatable::status
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
