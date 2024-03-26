program check
  use usuario
  implicit none
  character(len=100) :: nombre
  character(len=15) :: dpi, password
  character(len=1) :: opcion
  type(listaUsuarios) :: listadoUsuarios
    do 
      print *, ""
      
        print *, "|-----------------------------------------|"
        print *, "|             Menu Principal              |"
        print *, "|-----------------------------------------|"
        print *, "| 1. Registrarse                          |"              
        print *, "| 2. Iniciar Sesion                       |"  
        print *, "| 3. Salir                                |"  
        print *, "|-----------------------------------------|"
        print *, "Ingrese el numero de opcion"
        read *, opcion
        select case (opcion)
        case('1')
  
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
          call listadoUsuarios%Agregar(nombre,dpi,password)
          call listadoUsuarios%Agregar("Juan_Perez", "1234567890123", "clave123")
          call listadoUsuarios%Agregar("Garcia_Maria", "9876543210987", "password")
  
          ! Imprimir la lista antes de hacer cambios
          print *, "Lista de usuarios inicial:"
          call listadoUsuarios%Imprimir()
  
          ! Eliminar un usuario
          call listadoUsuarios%Eliminar("1234567890123")
          call listadoUsuarios%Imprimir()
  
          ! Actualizar datos de un usuario
          call listadoUsuarios%ActualizarDatos("Carlos Lopez", "9876543210987", "nuevaclave")
          
          ! Imprimir la lista después de hacer cambios
          print *, "Lista de usuarios después de cambios:"
          call listadoUsuarios%Imprimir()
          ! Aquí iría el código para guardar los datos del usuario en alguna estructura de datos,
          ! como una cola o una lista, dependiendo de tus necesidades.
          ! Por ejemplo, podrías tener una cola de usuarios pendientes de registro,
          ! y luego al validar su información, moverlos a una lista de usuarios registrados.
      
          print *, "Usuario registrado exitosamente."
        case('2')
          write(*, '(A)') "-----------------------------------------"
          write(*,'(A)') "             REGISTRO DE USUARIO          "
          write(*, '(A)') "-----------------------------------------"
          write(*, '(A)') "Ingrese el nombre:"
          read *, nombre
          write(*, '(A)') "Ingrese password:"
          read *, password
          write(*, '(A)') "-----------------------------------------"
          ! Aquí iría el código para validar si es usuario o administrador y mandarlo a donde corresponde
  
                  ! Validar los datos de inicio de sesión
          esValido = call listadoUsuarios%IniciarSesion(nombre, password)

          ! Verificar si los datos de inicio de sesión son válidos
          if (esValido) then
              ! Aquí puedes redirigir al usuario al menú correspondiente
              write(*, '(A)') "Inicio de sesión exitoso."
          else
              write(*, '(A)') "Inicio de sesión fallido."
          end if
          print *, "Bienvenido a Pixel Print Studio "
        case ('3')
            exit
        case default
            print *, "Opcion no valida. Por favor, intente de nuevo."
        end select   
    end do
    print *, "Has salido del programa. Hasta luego."
  end program check