# Fortran exceptions library

This library provides exception handling for Fortran 2003 and later. It is based the methodology in Poppe et al. 2012[^1]. It enables errors in a Fortran procedure to be communicated to the caller and, optionally, handled. Errors are encapsulated an a derived type that contains an error message and other information.

This provides the following advantages over other error handling approaches:

- Compared with integer status codes, the derived type can provide more detailed information about what went wrong.
- Compared with directly invoking `stop` or `error stop`, this library provides an opportunity for the program to recover from the error and continue

## Installation

fortran_exceptions is built using CMake:

``` bash
mkdir fortran_exceptions-build
cd fortran_exceptions-build
cmake /path/to/fortran_exceptions
make
make install
```

## Usage

Another project built using CMake can load the library using `find_package(fortran_exceptions)` and link to the library with `target_link_libraries(<target> <PRIVATE|PUBLIC|INTERFACE> fortran_exceptions`.

### Throwing and handling exceptions

A procedure that may throw an exception should have an optional allocatable argument of type `error_status`. An exception can be thrown by passing this optional argument and a new exception to the `throw` subroutine. The `new_exception` function provides a convenient way to create the exception. For example:

``` fortran
subroutine throwing_procedure(status)
  !! Procedure that throws an exception and exits

  use exceptions, ONLY: throw, exception, new_exception, error_status

  implicit none

  ! Arguments
  class(error_status), intent(out), allocatable, optional::status
      !! Error status

  call throw(status, new_exception("An error occurred"))
  return

end subroutine throwing_procedure
```

A calling function can check the exception type using a `select type` statement and, if appropriate, handle the exception. For example:

``` fortran
subroutine exception_handling_procedure
  use exceptions, ONLY: exception, error_status

  implicit none

  !! Procedure that calls exception-throwing procedures and handles the
  !! exception

  class(error_status), allocatable::status
      !! Error status

  ! Call a routine that throws an error
  call throwing_procedure(status)

  ! Handle the error
  select type (status)
  class is (exception)
    print *, "Handled error:", status%as_string()
    status%handled = .true.
  end select

end subroutine exception_handling_procedure
```

After handling an exception, the caller must set the `handled` attribute to `.true.`. If the `handled` attribute is false when the exception goes out of scope, the exception finalizer will invoke `error stop`, causing the program to exit immediately in an error state. If no exception is thrown by a function, the status will have the generic type `error_status` and the program will continue running normally.

### Extending

Custom exception types can be created extending the generic `exception` type. Examples of this are provided in the documentation.

## License

[MIT](https://choosealicense.com/licenses/mit/)

[^1]: Poppe, K., Cools, R., & Vandewoestyne, B. (2012). Error handling in Fortran 2003. ACM SIGPLAN Fortran Forum, 31(2), 7–19. <https://doi.org/10.1145/2338786.2338787>
