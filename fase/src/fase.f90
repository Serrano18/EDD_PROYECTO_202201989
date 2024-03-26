module fase
  implicit none
  private

  public :: say_hello
contains
  subroutine say_hello
    print *, "Hello, fase!"
  end subroutine say_hello
end module fase
