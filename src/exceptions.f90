module exceptions

  type, abstract::error_status
    !! Base type for error statuses
    character(:), allocatable::message
      !! Status message
    character(:), allocatable::procedure
      !! Procedure where status was created
    logical::handled = .false.
      !! Whether the exception was handled
  contains
    procedure::as_string => error_status_as_string
    procedure::print => error_status_print
    procedure::default_handler => error_status_default_handler
  end type error_status

  type, extends(error_status) :: no_error
    !! Non-error type, no error occurred

    integer::gfortran_workaround = 0
      !! Workaround for gfortran bug #110987.
      !! (prevents a segmentation fault triggered when finalizing
      !! temporary objects that inherit from a parent type but don't
      !! have any data members aside from those in the parent)

  end type no_error

  type, extends(error_status)::exception
    !! Generic exception

    integer::gfortran_workaround = 0
      !! Workaround for gfortran bug #110987
      !! (prevents a segmentation fault triggered when finalizing
      !! temporary objects that inherit from a parent type but don't
      !! have any data members aside from those in the parent)

  contains
    procedure::default_handler => exception_default_handler
    final::exception_finalize
  end type exception

  type::error_container
    class(error_status), allocatable::info
  contains
    final::container_finalize
  end type error_container

contains

  subroutine transfer_error(old_container, new_container)

    !! Transfer error state to a new container

    ! Arguments
    type(error_container), intent(inout)::old_container
    type(error_container), intent(inout), optional::new_container

    if (present(new_container)) then

      ! Use allocate command to copy error to new container
      ! (A simple assignment works in Fortran 2008, but fails in older compilers
      ! such as gfortran 4-6 that don't support assigning to allocatable
      ! polymorphics
      allocate (new_container%info, source=old_container%info)

      ! Mark old_container%info as handled so that it doesn't trigger
      ! an error when finalized (which it shouldn't since the error has been
      ! transferred to another container)
      old_container%info%handled = .true.
    else
      call old_container%info%default_handler
    end if

  end subroutine transfer_error

  function new_exception(message, procedure) result(exc)

    !! Create a new exception

    ! Arguments
    character(*), intent(in)::message
        !! Error message
    character(*), intent(in), optional::procedure
        !! Procedure where error occured

    type(exception)::exc
        !! New exception object

    exc%message = message

    if (present(procedure)) exc%procedure = procedure

  end function new_exception

  subroutine throw(status, new_status)

    !! Throw an exception

    ! Arguments
    type(error_container), intent(out), optional::status
        !! Status object from the caller
    class(error_status)::new_status
        !! New status object

    if (present(status)) then

      ! Use allocate command to copy error to container
      ! (A simple assignment works in Fortran 2008, but fails in older compilers
      ! such as gfortran 4-6 that don't support assigning to allocatable
      ! polymorphics
      allocate (status%info, source=new_status)

      ! Mark error as handled
      new_status%handled = .true.
    else
      call new_status%default_handler()
    end if

  end subroutine throw

  function error_status_as_string(this) result(string)
    !! Get a string representation of an error status

    ! Arguments
    class(error_status), intent(in)::this
        !! Status object

    character(:), allocatable::string

    string = trim(this%message)

    if (allocated(this%procedure)) then
      if (len_trim(this%procedure) > 0) then
        string = string//new_line('A')//"in procedure "//trim(this%procedure)
      end if
    end if

  end function error_status_as_string

  subroutine error_status_print(this)
    !! Print exception to standard error

    use iso_fortran_env, ONLY: error_unit

    ! Arguments
    class(error_status), intent(in)::this
        !! Status object

    write (error_unit, *) this%as_string()
  end subroutine error_status_print

  subroutine error_status_default_handler(this)

    ! Arguments
    class(error_status), intent(in)::this
        !! Status object

  end subroutine error_status_default_handler

  subroutine exception_default_handler(this)

    !! Finalize an exception. If this%handled is false, print error message
    !! and terminate. Otherwise do nothing.

    use iso_fortran_env, ONLY: error_unit, output_unit

    ! Arguments
    class(exception), intent(in)::this
        !! Exception object

    if (.not. this%handled) then
      write (error_unit, *) 'Unhandled exception:'
      call this%print()
      flush (error_unit)
      flush (output_unit)
      error stop
    end if

  end subroutine exception_default_handler

  subroutine exception_finalize(this)

    !! Finalize an exception. If this%handled is false, print error message
    !! and terminate. Otherwise do nothing.

    use iso_fortran_env, ONLY: error_unit

    ! Arguments
    type(exception), intent(inout)::this
        !! Exception object

    call this%default_handler()

  end subroutine exception_finalize

  subroutine container_finalize(this)

    !! Finalize an exception. If this%handled is false, print error message
    !! and terminate. Otherwise do nothing.

    use iso_fortran_env, ONLY: error_unit

    ! Arguments
    type(error_container), intent(inout)::this
        !! Exception object

    if (allocated(this%info)) then
      if (.not. this%info%handled) then
        call this%info%default_handler
      end if
    end if

  end subroutine container_finalize

end module exceptions
