module  cola_clientes
    !El codigo de este archivo fue copiado del repositorio del aux y modificado por mí
  use cliente
  use json_module
  implicit none

  type, public :: node
      private
      type(clientes):: value
      type(node), pointer :: next => null()
  end type node
  type, public :: cola
  private 
  type(node), pointer :: head => null()
  type(node), pointer :: tail => null()
  contains
      procedure :: append
      procedure :: delete
      procedure :: print
      procedure :: guardar_json
      procedure :: clientesAleatorios
      procedure :: eliminarClienteMasAntiguo
  end type cola
  contains
  subroutine append(this, name, uid, img1, img2)

      class(cola), intent(inout) :: this
     
      character (len=*), intent(in) :: name
      integer, intent(in) :: uid,img1, img2
      type(node), pointer :: temp

      allocate(temp)
      temp%value%nombre = name
      temp%value%id = uid
      temp%value%img_g = img1
      temp%value%img_p = img2
      temp%next => null()
      

      if (.not. associated(this%head)) then
          this%head => temp
          this%tail => temp
      else
          this%tail%next => temp
          this%tail => temp
      end if

      print *, 'Cliente Agregado ', name
  end subroutine append

  subroutine delete(this)
      class(cola), intent(inout) :: this
      type(node), pointer :: temp

      if (.not. associated(this%head)) then
          print *, 'Cola esta vacia'
          return
      end if

      print *, 'Delete ', this%head%value
      temp => this%head
      this%head => this%head%next
      deallocate(temp)
  end subroutine delete

  subroutine print(this)
      class(cola), intent(in) :: this
      type(node), pointer :: current

      current => this%head

      print *, '|-------------------------------------------------------------------------|'
      print *, '|                          Listado de Clientes:                           |'
      print *, '|-------------------------------------------------------------------------|'
      do while (associated(current))
          write(*,*) current%value
          current => current%next
      end do 
  end subroutine print

  subroutine guardar_json(this,narchivo)
      class(cola), intent(inout) :: this
      type(json_file) :: json   ! Se declara una variable del tipo json_file
      type(json_value), pointer :: listPointer, personPointer, attributePointer  ! Se declaran punteros a variables del tipo json_value
      type(json_core) :: jsonc  ! Se declara una variable del tipo json_core para acceder a las funciones básicas de JSON
      !character(len=100):: nombre  
      character(:), allocatable :: nombre  
      integer :: i, size,u_id ,img_1, img_2
      logical :: found
      character(len=256), intent(in):: narchivo
      call json%initialize()
      call json%load(filename=narchivo)
      call json%info('',n_children=size)
      call json%get_core(jsonc)
      call json%get('',listPointer,found)
      do i = 1, size
        call jsonc%get_child(listPointer,i,personPointer,found=found)
        call jsonc%get_child(personPointer,'id',attributePointer,found=found)
        if (found) then
          call jsonc%get(attributePointer,u_id)
          !print *, u_id
        end if
        call jsonc%get_child(personPointer,'nombre',attributePointer,found=found)
        if (found) then
          call jsonc%get(attributePointer,nombre)
          !print *, trim(nombre)
        end if
        call jsonc%get_child(personPointer,'img_g',attributePointer,found=found)
        if (found) then
          call jsonc%get(attributePointer,img_1)
          !print *, img_1
        end if
        call jsonc%get_child(personPointer,'img_p',attributePointer,found=found)
        if (found) then
          call jsonc%get(attributePointer,img_2)
          !print *, img_2
        end if 
        call this%append(nombre, u_id, img_1, img_2)
      end do
      call this%print()
      call json%destroy()
    end subroutine guardar_json


    subroutine clientesAleatorios(this)
      class(cola), intent(inout) :: this
      integer :: num_imagenes, i, enClientes
      real :: num_clientes
      character(len=100) :: nombre
      integer :: rnombre, rapellido
      integer :: uid, img1, img2
      character(len=7), dimension(10) :: nombres = [ &
          "Alberto", "Martina", "Braulio", "Gabriel", "Ignacio", &
          "Gustavo", "Daniela", "Nicolas", "Alfredo", "Eduardo" &
      ]
      character(len=5), dimension(10) :: apellidos = [ &
      "Lopez", "Perez", "Gomez", "Rivas", "Pinto",  &
      "Rojas","Mendo", "Saenz", "Dumas", "Santo" &
      ]
      
      call random_seed()
      call random_number(num_clientes)
      enClientes = floor(num_clientes * 4) 
            call random_number(num_clientes)
            do i = 1, enClientes
              call random_number(num_clientes)
              rnombre = int(num_clientes * size(nombres)) + 1
              call random_number(num_clientes)
              rapellido = int(num_clientes * size(apellidos)) + 1
              nombre = trim(nombres(rnombre)) // " " // trim(apellidos(rapellido))
              call random_number(num_clientes)
              uid = 1000 + int(num_clientes * 9000)
              call random_number(num_clientes)
              img1 = int(num_clientes * 5.0)
              call random_number(num_clientes)
              img2 = int(num_clientes * 5.0)
              call this%append(nombre, uid, img1, img2)
            end do
      !call this%print()
  end subroutine clientesAleatorios

  function eliminarClienteMasAntiguo(this) result(client)
    class(cola), intent(inout) :: this
    type(clientes) :: client
    type(node), pointer :: temp

    if (.not. associated(this%head)) then
      print *, 'La cola está vacía, no hay clientes para eliminar.'
      return
    end if
    temp => this%head
    this%head => this%head%next
    client = temp%value
    deallocate(temp)
  end function eliminarClienteMasAntiguo

end module cola_clientes 