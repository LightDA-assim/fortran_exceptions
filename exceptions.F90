module exceptions

  type,abstract::error_status
     !! Base type for error statuses
     character(:),allocatable::message
     character(:),allocatable::procedure
     logical::handled = .false.
   contains
     procedure::as_string=>error_status_as_string
     procedure::print=>error_status_print
  end type error_status

  type, extends(error_status) :: no_error
     !! Non-error type, no error occurred
  end type no_error

  type, extends(error_status)::exception
     !! Generic exception
   contains
     final::exception_finalize
  end type exception

contains

  function new_exception(message,procedure) result(exc)

    !! Create a new exception

    ! Arguments
    character(*),intent(in)::message
        !! Error message
    character(*),intent(in),optional::procedure
        !! Procedure where error occured

    type(exception)::exc
        !! New exception object

    exc%message=message

    if(present(procedure)) exc%procedure=procedure

  end function new_exception

  subroutine throw(status,new_status)

    !! Throw an exception

    ! Arguments
    class(error_status),intent(out),allocatable,optional::status
        !! Status object from the caller
    class(error_status)::new_status
        !! New status object

    if(present(status)) then
       status=new_status
       new_status%handled=.true.
    end if
    
  end subroutine throw

  function error_status_as_string(this) result(string)
    !! Get a string representation of an error status

    ! Arguments
    class(error_status),intent(in)::this
        !! Status object

    character(:),allocatable::string

    string = trim(this%message)

    if(len_trim(this%procedure)>0) then
       string = string//new_line('A')//"in procedure "//trim(this%procedure)
    end if

  end function error_status_as_string

  subroutine error_status_print(this)
    !! Print exception to standard error

    use iso_fortran_env, ONLY: error_unit

    ! Arguments
    class(error_status),intent(in)::this
        !! Status object

    write(error_unit,*) this%as_string()
  end subroutine error_status_print

  subroutine exception_finalize(this)

    !! Finalize an exception. If this%handled is false, print error message
    !! and terminate. Otherwise do nothing.

    use iso_fortran_env, ONLY: error_unit

    ! Arguments
    type(exception),intent(inout)::this
    !! Exception object

    if(.not.this%handled) then
       write (error_unit,*) 'Unhandled exception:'
       call this%print()
       error stop
    end if

  end subroutine exception_finalize

end module exceptions
