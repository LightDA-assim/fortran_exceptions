module test_exception_pass

  implicit none

contains

  subroutine test_throw(status)
    !! Test procedure that throws an exception and exits

    use exceptions, ONLY: throw, new_exception, error_status

    ! Arguments
    class(error_status),allocatable,optional::status
        !! Error status

    call throw(status,new_exception("An error occurred"))
    return

  end subroutine test_throw

  subroutine test_throw_with_procname(status)
    !! Test procedure that throws an exception with the procedure attribute set
    !! and exits

    use exceptions, ONLY: throw, new_exception, error_status

    ! Arguments
    class(error_status),allocatable,optional::status
        !! Error status

    call throw(status,new_exception("An error occurred","test_throw_with_procname"))
    return

  end subroutine test_throw_with_procname

  subroutine test_catch()
    !! Test procedure that calls exception-throwing procedures and handles the
    !! exceptions

    use exceptions, ONLY: exception,error_status

    class(error_status),allocatable::status
        !! Error status

    call test_throw(status)

    select type(status)
    class is(exception)
       print *,"Handled error:",status%as_string()
       status%handled=.true.
    end select

    call test_throw_with_procname(status)

    select type(status)
    class is(exception)
       print *,"Handled error:",status%as_string()
       status%handled=.true.
    end select
  end subroutine test_catch

end module test_exception_pass

program test_exception

  use test_exception_pass, ONLY: test_catch

  call test_catch()

end program test_exception
