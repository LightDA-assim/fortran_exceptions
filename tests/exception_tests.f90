module exception_tests

  implicit none

contains

  subroutine test_no_throw(status)
    !! Test procedure that can throw an error, but doesn't

    use exceptions, ONLY: error_status

    ! Arguments
    class(error_status),allocatable,optional::status
        !! Error status

  end subroutine test_no_throw

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

  subroutine test_fail()
    !! Test function that calls an exception-throwing procedure and ignores the
    !! exception explicitly, triggering a fatal error.

    use exceptions, ONLY: exception,error_status

    class(error_status),allocatable::status
        !! Error status

    call test_throw(status)

    select type(status)
    class is(exception)
       print *,"Ignored error:",status%message
    end select

  end subroutine test_fail

  subroutine test_fail_implicit()
    !! Test function that calls an exception-throwing procedure and
    !! ignores the exception implicitly, triggering a fatal error

    call test_throw()

  end subroutine test_fail_implicit

  subroutine test_catch()
    !! Test procedure that calls exception-throwing procedures and handles the
    !! exceptions

    use exceptions, ONLY: exception,error_status

    class(error_status),allocatable::status
        !! Error status

    call test_no_throw()

    call test_no_throw(status)

    select type(status)
    class is(exception)
       print *,"Saw error:",status%as_string()
    end select

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

end module exception_tests
