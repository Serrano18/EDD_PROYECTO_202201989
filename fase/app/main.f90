program main
  use usuario
  use arbolb
  implicit none
  character(len=1) :: opcu
  character(len=100) :: nombre
  character(len=15) :: dpi, password
  character(len=1) :: opcion
  integer :: esValido
  type(listaUsuarios) :: listadoUsuarios

  call insert(5)
  call insert(10)
  call insert(15)
  call insert(20)
  call insert(25)
  call insert(30)
  call insert(35)
  call insert(40)
  call insert(45)
  call traversal(root)
  !call graficar_arbolb(root,"Insertar.dot")
  !call graphTraversal(root, 'arbolb.dot')
  call generateDotFile( 'arbolb.dot',root)
  print * , ''
  call remove(20) ! Por ejemplo, eliminar el valor 20
  call traversal(root) ! Verificar que se haya eliminado
  !call graficar_arbolb(root,"Eliminar.dot")
  !call graphTraversal(root, 'arbolb1.dot')
  call generateDotFile( 'arbolb1.dot',root)

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
        call listadoUsuarios%Agregar(nombre,dpi,password)
      case('2')
        write(*, '(A)') "------------------------------------------"
        write(*,'(A)') "             INICIO DE USUARIO             "
        write(*, '(A)') "------------------------------------------"
        write(*, '(A)') "Ingrese el nombre:"
        read *, nombre
        write(*, '(A)') "Ingrese password:"
        read *, password
        write(*, '(A)') "------------------------------------------"
        ! Aquí iría el código para validar si es usuario o administrador y mandarlo a donde corresponde
        ! Validar los datos de inicio de sesión
        esValido = listadoUsuarios%IniciarSesion(nombre, password)
        ! Redirigir al usuario según el resultado de la validación
        if (esValido == 1) then
            write(*, '(A)') "Bienvenido a Pixel Print Studio Administrador"
            call menuAdmin()
        else if (esValido == -1) then
          
            write(*, '(A,A)') "Bienvenido a Pixel Print Studio ", trim(nombre)

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
        else
            
            write(*, '(A)') "Nombre de usuario o contraseña incorrectos."
        end if
      case ('3')
        print *, "Has salido del programa. Hasta luego."  
        exit
      case default
          print *, "Opcion no valida. Por favor, intente de nuevo."
      end select   
  end do

  contains


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
      call listadoUsuarios%Agregar(nombre,dpi,password)
    end subroutine agregarUsuario
end program main
