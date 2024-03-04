module cliente
    implicit none
    type, public :: clientes
        integer :: id=-1, img_g, img_p
        character (len=100) :: nombre
        integer :: pasoInicio
        !logical :: init = .false.
    end type
end module cliente