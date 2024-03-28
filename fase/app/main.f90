program main
  use usuario
  use arbolb

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
        
        write(*, '(A)') "------------------------------------------"
        write(*,'(A)') "             INICIO DE USUARIO             "
        write(*, '(A)') "------------------------------------------"
        write(*, '(A)') "Ingrese su usuario:"
        read (*,'(A)') nombre
        write(*, '(A)') "Ingrese password:"
        read (*,'(A)') password
        write(*, '(A)') "------------------------------------------"
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
        write(*, '(A)') "| 1. Estructuras                          |"                
        write(*, '(A)') "| 2. Navegación y gestión de imágenes     |"  
        write(*, '(A)') "| 3. Carga Masiva de Capas                |"
        write(*, '(A)') "| 4. Carga Masiva de Imagenes             |"
        write(*, '(A)') "| 5. Cargas Masiva de Álbumes             |"
        write(*, '(A)') "| 6. Cerrar Sesion                        |"  
        write(*, '(A)') "|-----------------------------------------|"
        write(*, '(A)') "Ingrese el numero de opcion"
          read *, opcu
          select case (opcu)
          case ('1')
            call menuestructuras()
          case ('2')
          case ('3')
            !Codigo para la carga de capas
            print *, "Ingrese el nombre del Archivo de Capas: "
            read *, archivo
            call cargaMasivaCapas(archivo,usuarioActual%arbolDeCapas)
          case ('4')
            print *, "Ingrese el nombre del Archivo de Imagenes: "
            read *, archivo
            call cargaMasivaImagenes(archivo,usuarioActual%arbolDeImagenes)
          case ('5')
          case ('6')
            print *, "Gracias por tu visita"
            exit
          case default
            print *, "Opcion no valida. Por favor, intente de nuevo."
          end select  
      end do
    end subroutine menuUsers

    subroutine menuAdmin ()
     character(len=1) :: opca
     character(len=1) :: op
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
                call Usuarios%graphTree()
              case ('2')
                write(*, '(A)') "|---------------------------------------------|"
                write(*, '(A)') "|                Crear Usuario                |"
                write(*, '(A)') "|---------------------------------------------|"
                call agregarUsuario()
              case ('3')
                write(*, '(A)') "|---------------------------------------------|"
                write(*, '(A)') "|              Modificar Usuario              |"
                write(*, '(A)') "|---------------------------------------------|"
                write(*, '(A)') "Ingrese el dpi del Usuario a Modificar"
                read *, dpi
                usuarioActual => Usuarios%searchUser(dpi)
                if (associated(usuarioActual)) then
                  write(*, '(A)') "Desea Modificar el Nombre del usuario [s/n]"
                  read *, op
                  if(op=='s'.or. op=='S') then
                    write(*, '(A)') "Ingrese Nombre Nuevo"
                    read (*,'(A)') nombre
                    usuarioActual%nombre = nombre
                  end if
                  write(*, '(A)') "Desea Modificar la contraseña del usuario [s/n]"
                  read *, op
                  if(op=='s'.or. op=='S') then
                    write(*, '(A)') "Ingrese Password Nuevo"
                    read (*,'(A)') password
                    usuarioActual%password = password
                  end if
                end if
              case ('4')
                write(*, '(A)') "|---------------------------------------------|"
                write(*, '(A)') "|              Eliminar Usuario              |"
                write(*, '(A)') "|---------------------------------------------|"
              case ('5')
                write(*, '(A)') "|---------------------------------------------|"
                write(*, '(A)') "|            Carga Masiva Usuario             |"
                write(*, '(A)') "|---------------------------------------------|"
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

    subroutine menuestructuras ()
      character(len=1) :: opce
      integer :: imgseleccionada
         do 
             print *, "" 
             write(*, '(A)') "|-----------------------------------------|"
             write(*, '(A)') "|     Pixel Print Studio Estructuras      |"
             write(*, '(A)') "|-----------------------------------------|"
             write(*, '(A)') "| 1. Arbol de Imagenes                    |" 
             write(*, '(A)') "| 2. Arbol de Capas                       |"     
             write(*, '(A)') "| 3. Listado de álbumes                   |"     
             write(*, '(A)') "| 4. Ver Capa                             |"     
             write(*, '(A)') "| 5. Imagen y Arbol de Capas              |"
             write(*, '(A)') "| 6. Regresar                             |"  
             write(*, '(A)') "|-----------------------------------------|"
             write(*, '(A)') "Ingrese el numero de opcion"
               read *, opce
               select case (opce)
               case ('1')
                 call usuarioActual%arbolDeImagenes%graficar()
               case ('2')
                 call usuarioActual%arbolDeCapas%graph("ArbolDeCapas")
               case ('3')
                write(*, '(A)') "Ingrese el id de la Imagen a observar"
                read *, imgseleccionada
                call usuarioActual%arbolDeImagenes%graficarArbolDeCapasDeImagen(imgseleccionada)
               case ('4')
 
               case ('5')
        
               case ('6')
                   exit
               case default
                 print *, "Opcion no valida. Por favor, intente de nuevo."
               end select   
         end do
    end subroutine menuestructuras
 
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
