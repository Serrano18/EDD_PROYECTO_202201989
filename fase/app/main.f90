program main
  use usuario
  use arbolb
  use arbolavl
  use arbolbb
  use lecturajson
  implicit none
  type(User) :: nuevoCliente
  type(User),pointer :: usuarioActual
  character(len=1) :: opcu
  character(len=100) :: nombre
  character(len=20) :: password
  integer(kind=8) :: dpi
  character(len=1) :: opcion
  integer :: esValido
  type(BTreeNode) :: Usuarios
  type(avl) :: a
  type(abb) :: tree
  character (len=256 ) :: archivo

  do 
    print *, ""
    
    write(*, '(A)') "|-----------------------------------------|"
    write(*, '(A)') "|   Bienvenido A  Pixel Print Studio      |"
    write(*, '(A)') "|-----------------------------------------|"
    write(*, '(A)') "| 1. Registrarse                          |"              
    write(*, '(A)') "| 2. Iniciar Sesion                       |"  
    write(*, '(A)') "| 3. Salir                                |"  
    write(*, '(A)') "|-----------------------------------------|"
    write(*, '(A)') "Ingrese el numero de opcion"
      read *, opcion
      select case (opcion)
      case('1')
        ! Aquí iría el código para guardar los datos del usuario en alguna estructura de datos,
        call agregarUsuario()
      case('2')
        call Usuarios%graphTree()
        write(*, '(A)') "------------------------------------------"
        write(*,'(A)') "             INICIO DE USUARIO             "
        write(*, '(A)') "------------------------------------------"
        write(*, '(A)') "Ingrese su usuario:"
        read (*,'(A)') nombre
        write(*, '(A)') "Ingrese password:"
        read (*,'(A)') password
        write(*, '(A)') "------------------------------------------"
        ! Aquí iría el código para validar si es usuario o administrador y mandarlo a donde corresponde
        
        ! Redirigir al usuario según el resultado de la validación
        if (nombre == "admin" .and. password == "EDD2024") then
            write(*, '(A)') "Bienvenido a Pixel Print Studio Administrador"
            call menuAdmin()
        else if (esNumero(nombre)) then
          dpi = convert_to_integer(nombre)
          usuarioActual => Usuarios%searchUser(dpi)
          ! Verificar si se encontró el usuario
          if (associated(usuarioActual)) then
            if(usuarioActual%password == password) then
              write(*, '(A,A)') "Bienvenido a Pixel Print Studio ", trim(usuarioActual%nombre)
              call menuUsers()
            else
              print *, "Contrasena Invalida"
            end if
          end if
        else
          print *, "Usuario Invalido"
        end if
      case ('3')
        print *, "Has salido del programa. Hasta luego."  
        exit
      case default
          print *, "Opcion no valida. Por favor, intente de nuevo."
      end select   
  end do

  contains

  function esNumero(cadena) result(esEntero)
    character(len=*), intent(in) :: cadena
    logical :: esEntero
    integer(kind=8) :: valor,iostat

    ! Intentar convertir la cadena en un entero
    read(cadena, *, IOSTAT=iostat) valor
    if (iostat /= 0) then
        esEntero = .false. ! La conversión falló
    else
        esEntero = .true.  ! La conversión fue exitosa
    end if
  end function esNumero

    subroutine menuUsers ()
      character(len=1) :: opcu
      do
        print *, "" 
        write(*, '(A)') "|-----------------------------------------|"
        write(*, '(A)') "|     Bienvenido a Pixel Print Studio     |"
        write(*, '(A)') "|-----------------------------------------|"
        write(*, '(A)') "| 1. Reportes de Estructuras              |"              
        write(*, '(A)') "| 2. Navegación y gestión de imágenes     |"  
        write(*, '(A)') "| 3. Cargas Masivas                       |"
        write(*, '(A)') "| 4. Cerrar Sesion                        |"  
        write(*, '(A)') "|-----------------------------------------|"
        write(*, '(A)') "Ingrese el numero de opcion"
          read *, opcu
          select case (opcu)
          case ('1')
          case ('2')
          case ('3')
          case ('4')
            print *, "Gracias por tu visita"
            exit
          case default
            print *, "Opcion no valida. Por favor, intente de nuevo."
          end select  
      end do
    end subroutine menuUsers

    subroutine menuAdmin ()
     character(len=1) :: opca
        do 
            print *, "" 
            write(*, '(A)') "|-----------------------------------------|"
            write(*, '(A)') "|     Pixel Print Studio Administrador    |"
            write(*, '(A)') "|-----------------------------------------|"
            write(*, '(A)') "| 1. Arbol B de Usuarios                  |"              
            write(*, '(A)') "| 2. Insertar Usuario                     |"  
            write(*, '(A)') "| 3. Modificar Usuario                    |"
            write(*, '(A)') "| 4. Eliminar Usuario                     |"
            write(*, '(A)') "| 5. Carga Masiva Usuario                 |"
            write(*, '(A)') "| 6. Cerrar Sesion                        |"  
            write(*, '(A)') "|-----------------------------------------|"
            write(*, '(A)') "Ingrese el numero de opcion"
              read *, opca
              select case (opca)
              case ('1')
              case ('2')
                call agregarUsuario()
              case ('3')
              case ('4')
              case ('5')
                print *, ""
                print *, "Ingrese el nombre del Archivo: "
                read *, archivo
                call cargaMasivaUsuarios(archivo,Usuarios)
              case ('6')
                  print *, "Gracias por tu visita"
                  exit
              case default
                print *, "Opcion no valida. Por favor, intente de nuevo."
              end select   
        end do
    end subroutine menuAdmin

    subroutine agregarUsuario ()
      write(*, '(A)') "-----------------------------------------"
      write(*,'(A)') "             REGISTRO DE USUARIO          "
      write(*, '(A)') "-----------------------------------------"
      write(*, '(A)') "Ingrese el nombre:"
      read *, nombre
      write(*, '(A)')  "Ingrese el DPI:"
      read *, dpi
      write(*, '(A)') "Ingrese password:"
      read *, password
      write(*, '(A)') "-----------------------------------------"
      ! Aquí iría el código para guardar los datos del usuario en alguna estructura de datos,
      nuevoCliente = createUser(nombre, dpi, password)
      call Usuarios%insert(nuevoCliente )
    end subroutine agregarUsuario
end program main
