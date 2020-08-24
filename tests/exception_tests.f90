module exception_tests

  use exceptions, ONLY: throw, exception, new_exception, error_status

  implicit none

contains

  subroutine test_no_throw(status)
    !! Test procedure that can throw an error, but doesn't

    ! Arguments
    class(error_status), intent(out), allocatable, optional::status
        !! Error status

  end subroutine test_no_throw

  subroutine test_throw(status)
    !! Test procedure that throws an exception and exits

    ! Arguments
    class(error_status), intent(out), allocatable, optional::status
        !! Error status

    call throw(status, new_exception("An error occurred"))
    return

  end subroutine test_throw

  subroutine test_throw_with_procname(status)
    !! Test procedure that throws an exception with the procedure attribute set
    !! and exits

    ! Arguments
    class(error_status), intent(out), allocatable, optional::status
        !! Error status

    call throw(status, new_exception("An error occurred", &
                                     "test_throw_with_procname"))
    return

  end subroutine test_throw_with_procname

  subroutine test_fail()
    !! Test function that calls an exception-throwing procedure and ignores the
    !! exception explicitly, triggering a fatal error.

    class(error_status), allocatable::status
        !! Error status

    call test_throw(status)

    select type (status)
    class is (exception)
      print *, "Ignored error:", status%message
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

    class(error_status), allocatable::status
        !! Error status

    ! Call a routine that can throw an error, but doesn't
    call test_no_throw()

    ! Call the same routine again, but pass the optional status argument
    call test_no_throw(status)

    ! Check the status argument
    select type (status)
    class is (exception)
      print *, "Saw error:", status%as_string()
    end select

    ! Call a routine that throws an error
    call test_throw(status)

    ! Handle the error
    select type (status)
    class is (exception)
      print *, "Handled error:", status%as_string()
      status%handled = .true.
    end select

    ! Call a routine that throws an error with the procedure attribute set
    call test_throw_with_procname(status)

    ! Handle the error
    select type (status)
    class is (exception)
      print *, "Handled error:", status%as_string()
      status%handled = .true.
    end select
  end subroutine test_catch

end module exception_tests
