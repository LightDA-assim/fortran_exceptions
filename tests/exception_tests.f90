module exception_tests

  use exceptions, ONLY: throw, exception, new_exception, error_container

  implicit none

contains

  subroutine test_no_throw(status)
    !! Test procedure that can throw an error, but doesn't

    ! Arguments
    type(error_container), intent(out), optional::status
        !! Error status

  end subroutine test_no_throw

  subroutine test_throw(status)
    !! Test procedure that throws an exception and exits

    ! Arguments
    type(error_container), intent(out), optional::status
        !! Error status

    call throw(status, new_exception("An error occurred"))
    return

  end subroutine test_throw

  subroutine test_throw_with_procname(status)
    !! Test procedure that throws an exception with the procedure attribute set
    !! and exits

    ! Arguments
    type(error_container), intent(out), optional::status
        !! Error status

    call throw(status, new_exception("An error occurred", &
                                     "test_throw_with_procname"))
    return

  end subroutine test_throw_with_procname

  subroutine test_fail()
    !! Test function that calls an exception-throwing procedure and ignores the
    !! exception explicitly, triggering a fatal error.

    type(error_container)::status
        !! Error status

    call test_throw(status)

    select type (error => status%info)
    class is (exception)
      print *, "Ignored error:", error%message
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

    type(error_container)::status
        !! Error status

    ! Call a routine that can throw an error, but doesn't
    call test_no_throw()

    ! Call the same routine again, but pass the optional status argument
    call test_no_throw(status)

    ! Check the status argument
    select type (error => status%info)
    class is (exception)
      print *, "Saw error:", error%as_string()
    end select

    print *, 'Routine that throws error'
    ! Call a routine that throws an error
    call test_throw(status)

    ! Handle the error
    select type (error => status%info)
    class is (exception)
      print *, "Handled error:", error%as_string()
      error%handled = .true.
    end select

    ! Call a routine that throws an error with the procedure attribute set
    call test_throw_with_procname(status)

    ! Handle the error
    select type (error => status%info)
    class is (exception)
      print *, "Handled error:", error%as_string()
      error%handled = .true.
    end select
  end subroutine test_catch

end module exception_tests
