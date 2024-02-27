module cliente
    implicit none
    type, public :: clientes
        integer :: id, img_g, img_p
        character (len=100) :: nombre
    end type
end module cliente