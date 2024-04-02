program main
  use usuario
  use arbolb
  use arbolavl
  use lecturajson
  use linkedlistm, only: linkedlist,nodolistado=>node
  use listadoAlbums, nodoAlbum => nodoA
  use listadoPixeles, only: pixel, nodoPixel =>nodop
  use matriz, only:matrix
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
  type(matrix) :: trabajarMatriz
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
    !me falta la navegacion
    subroutine menuUsers ()
      character(len=1) :: opcu
      do
        print *, "" 
        write(*, '(A)') "|-----------------------------------------|"
        write(*, '(A)') "|     Bienvenido a Pixel Print Studio     |"
        write(*, '(A)') "|-----------------------------------------|"
        write(*, '(A)') "| 1. Estructuras                          |"                
        write(*, '(A)') "| 2. Navegacion y gestion de imagenes     |"  
        write(*, '(A)') "| 3. Carga Masiva de Capas                |"
        write(*, '(A)') "| 4. Carga Masiva de Imagenes             |"
        write(*, '(A)') "| 5. Cargas Masiva de Albums              |"
        write(*, '(A)') "| 6. Reportes                             |"
        write(*, '(A)') "| 7. Cerrar Sesion                        |"  
        write(*, '(A)') "|-----------------------------------------|"
        write(*, '(A)') "Ingrese el numero de opcion"
          read *, opcu
          select case (opcu)
          case ('1')
            call menuestructuras()
          case ('2')
            call navegacion()
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
            print *, "Ingrese el nombre del Archivo de Albums: "
            read *, archivo
            call cargaMasivaAlbums(archivo,usuarioActual%listadoDeAlbums)
          case ('6')
            call reportesUsuarios()
          case ('7')
            print *, "Gracias por tu visita"
            exit
          case default
            print *, "Opcion no valida. Por favor, intente de nuevo."
          end select  
      end do
    end subroutine menuUsers
    !me falta reportes
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
            write(*, '(A)') "| 6. Reportes Administrador               |"
            write(*, '(A)') "| 7. Cerrar Sesion                        |"  
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
                write(*, '(A)') "Ingrese el dpi del Usuario a Modificar"
                read *, dpi
                usuarioActual => Usuarios%searchUser(dpi)
                if ( associated(usuarioActual) ) then
                    call Usuarios%removeUser(dpi)
                else
                    print *, "Usuario no existente"
                end if
              case ('5')
                write(*, '(A)') "|---------------------------------------------|"
                write(*, '(A)') "|            Carga Masiva Usuario             |"
                write(*, '(A)') "|---------------------------------------------|"
                print *, ""
                print *, "Ingrese el nombre del Archivo: "
                read *, archivo
                call cargaMasivaUsuarios(archivo,Usuarios)
              case ('6')
                 call reportesAdministrador()
              case ('7')
                  print *, "Gracias por tu visita"
                  exit
              case default
                print *, "Opcion no valida. Por favor, intente de nuevo."
              end select   
        end do
    end subroutine menuAdmin

      !me falta graficar capas
    subroutine menuestructuras ()
      character(len=1) :: opce
      type(capas),pointer :: temp_capa
      type(nodoPixel), pointer :: pxI
      integer :: imgseleccionada
         do 
             print *, "" 
             write(*, '(A)') "|-----------------------------------------|"
             write(*, '(A)') "|     Pixel Print Studio Estructuras      |"
             write(*, '(A)') "|-----------------------------------------|"
             write(*, '(A)') "| 1. Arbol de Imagenes                    |" 
             write(*, '(A)') "| 2. Arbol de Capas                       |"     
             write(*, '(A)') "| 3. Listado de Albumes                   |"     
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
                  call usuarioActual%listadoDeAlbums%graficarA
               case ('4')
                write(*, '(A)') "Listado de Capas"
                call usuarioActual%arbolDeCapas%preorder()
                write(*, '(A)') "Ingrese el id de la Capa a observar"
                read *, i
                temp_capa => usuarioActual%arbolDeCapas%search(i)
                if ( associated(temp_capa) ) then
                  pxI => temp_capa%listadoDePixeles%head
                  !call temp_capa%listadoDePixeles%print()
                  usuarioActual%mimagenes = trabajarMatriz
                  do while ( associated(pxI) )
                      call usuarioActual%mimagenes%insert(pxI%value%fila, pxI%value%columna, pxI%value%color)
                      pxI => pxI%next
                  end do
                  call usuarioActual%mimagenes%graficarNLogico()
                end if
               case ('5')
                write(*, '(A)') "Listado de Imagenes"
                call usuarioActual%arbolDeImagenes%preorden()
                write(*,'(A)') " "
                write(*, '(A)') "Ingrese el id de la Imagen a observar"
                read *, imgseleccionada
                call usuarioActual%arbolDeImagenes%graficarArbolDeCapasDeImagen(imgseleccionada)

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

    subroutine navegacion()
      type(nodoAlbum), pointer :: albumActual
      type(Imagen), pointer :: imagenActual
      character(len=1) :: opce
      type(Imagen) :: temp_imag,new_imag
      type(linkedlist) :: temp_lista,new_lista
      integer :: imgseleccionada
      character(len=1)::b
     do 
        print *, "" 
        write(*, '(A)') "|-----------------------------------------|"
        write(*, '(A)') "|     Pixel Print Studio Navegacion       |"
        write(*, '(A)') "|-----------------------------------------|"
        write(*, '(A)') "| 1. Registrar Imagen                     |" 
        write(*, '(A)') "| 2. Eliminar Imagen                      |"     
        write(*, '(A)') "| 3. Generar Imagen por recorrido limitado|"     
        write(*, '(A)') "| 4. Generar Imagen por arbol de imagenes |"     
        write(*, '(A)') "| 5. Generar Imagen por Capas             |"
        write(*, '(A)') "| 6. Regresar                             |"  
        write(*, '(A)') "|-----------------------------------------|"
        write(*, '(A)') "Ingrese el numero de opcion"
        read *, opce
        temp_imag=new_imag
        temp_lista = new_lista
        select case (opce)
         case ('1')
            write(*, '(A)') "|-----------------------------------------|"
            write(*, '(A)') "|             REGISTRAR IMAGEN            |"
            write(*, '(A)') "|-----------------------------------------|"
            write(*, '(A)') "Ingrese el ID de la Imagen: "
            read *, temp_imag%id
            write(*, '(A)') "Registro de capas para Imagen !NUMEROS  (Para salir ingresa -1)"
            do while(.true.)
              write (*, '(A)') "Ingrese el ID de la capa: "
              read (*, *) i
              if (i >= 0) then
                call temp_imag%agregarcapa(i)
                call temp_imag%arbolIdCapas%add(i)
              else if (i == -1) then
                exit
              end if
            end do
            call usuarioActual%arbolDeImagenes%insertavl(temp_imag)
         case ('2')
            write(*, '(A)') "|-----------------------------------------|"
            write(*, '(A)') "|             ELIMINAR IMAGEN             |"
            write(*, '(A)') "|-----------------------------------------|"
            write(*, '(A)') "Listado de Imagenes"
            call usuarioActual%arbolDeImagenes%preorden()
            write(*,'(A)') " "
            write (*, '(A)') "Ingrese el ID de la Imagen: "
            read (*, *) i
            imagenActual=> usuarioActual%arbolDeImagenes%searchavl(i)
            if(associated(imagenActual))then
              call usuarioActual%arbolDeImagenes%deleteavl(imagenActual)
              call usuarioActual%listadoDeAlbums%eliminarImagenAlbum(i)
              print *, "Imagen eliminada con exito"
            else
              print *, "La imagen no existe"
            end if 
          case ('3')
            write(*, '(A)') "|-----------------------------------------|"
            write(*, '(A)') "|  Generar Imagen por recorrido limitado  |" 
            write(*, '(A)') "|-----------------------------------------|"
            write(*, '(A)') " Listado de Imagenes : "
            call usuarioActual%arbolDeImagenes%preorden()
            write(*,'(A)') "Ingresa el id de la Imagen: "
            read *, i
            imagenActual=>usuarioActual%arbolDeImagenes%searchavl(i)
            if(associated(imagenActual)) then 
              write(*, '(A)') " Listado de capas de la imagen: "
              call imagenActual%arbolIdCapas%inorder(imagenActual%arbolIdCapas%root)
              write(*,'(A,I0)') "Numero de Capas disponibles: ", imagenActual%arbolIdCapas%list%size
              write(*,'(A)') "Ingrese el numero de capas a utilizar: "
              read *, i
              if (i>0 .or. i<imagenActual%arbolIdCapas%list%size) then
                write(*,'(A)') "Recorridos"
                write(*,'(A)') "1. Preorden"
                write(*,'(A)') "2. Inorden"
                write(*,'(A)') "3. Postorden"
                write(*,'(A)') "Ingrese la opcion: "
                read *, b
                select case(b)
                  case("1")
                    print*, "Generando Imagen por preorden"
                    call imagenActual%arbolIdCapas%preorder(imagenActual%arbolIdCapas%root)
                    temp_lista = imagenActual%arbolIdCapas%list
                    call crearImagen(temp_lista,i)
                    call imagenActual%arbolIdCapas%generarTextAreaRecorridos()
                  case("2")
                    print*, "Generando imagen por Inorden"
                    call imagenActual%arbolIdCapas%inorder(imagenActual%arbolIdCapas%root)
                    temp_lista = imagenActual%arbolIdCapas%list
                    call crearImagen(temp_lista,i)
                    call imagenActual%arbolIdCapas%generarTextAreaRecorridos()
                  case("3")
                    print *, "Generando imagen por Postorden"
                    call imagenActual%arbolIdCapas%postorder(imagenActual%arbolIdCapas%root)
                    temp_lista = imagenActual%arbolIdCapas%list
                    call crearImagen(temp_lista,i)
                    call imagenActual%arbolIdCapas%generarTextAreaRecorridos()
                  case default
                    write(*,'(A)') "Recorrido no Valido"
                  end select
              end if
            else
              print *, "Id de imagen no encontrado"
            end if
          case ('4')    
            write(*, '(A)') "|-----------------------------------------|"
            write(*, '(A)') "|  Generar Imagen por Arbol de Imagenes   |" 
            write(*, '(A)') "|-----------------------------------------|"
            write(*, '(A)') " Listado de Imagenes : "
            call usuarioActual%arbolDeImagenes%preorden()
            write(*,'(A)') "Ingresa el id de la Imagen: "
            read *, i
            imagenActual=>usuarioActual%arbolDeImagenes%searchavl(i)
            if(associated(imagenActual)) then
              imagenActual%arbolIdCapas%list = new_lista
              call imagenActual%arbolIdCapas%amplitude()
              temp_lista = imagenActual%arbolIdCapas%list
              print *, "----------Amplitud----------"
              call crearImagen(temp_lista,temp_lista%size)
            else
              print *, "Imagen no encontrada"
            end if
          case ('5')
            write(*, '(A)') "|-----------------------------------------|"
            write(*, '(A)') "|       Generar Imagen por Capas          |" 
            write(*, '(A)') "|-----------------------------------------|"
            write(*, '(A)') " Listado de capas: "
            call usuarioActual%arbolDeCapas%preorder()
            do while (.true.)
              write (*, '(A)') "Ingresa los id de las capas: (Salir con -1) "
              read (*, *) i
              if (i >= 0) then
                if ( associated(usuarioActual%arbolDeCapas%search(i)) ) then
                  call temp_lista%addlist(i)
                end if
              else if (i == -1) then
                exit
              end if
            end do
            call crearImagen(temp_lista,temp_lista%size)
          case ('6')
                   exit
          case default
                 print *, "Opcion no valida. Por favor, intente de nuevo."
        end select   
      end do
    end subroutine navegacion
    
    subroutine crearImagen (listatemporal,num)
      type(linkedlist),intent(in)::listatemporal
      type(nodoPixel),pointer :: tempxl
      type(capas),pointer :: temporalcapas
      type(nodolistado),pointer :: temporalnlista
      integer :: num, a
      usuarioActual%mimagenes = trabajarMatriz
      call listatemporal%print
      if ( listatemporal%size > 0 ) then
        temporalnlista => listatemporal%head
        do a=1,num
          temporalcapas => usuarioActual%arbolDeCapas%search(temporalnlista%data)
          !print *,"no entro"
          if(associated(temporalcapas))then
           ! print *, "entro"
            write(*,'(I0)') temporalcapas%id
            if ( a < num ) then
              write(*, '(A)') " , "
            else
              write(*, *) ""
           end if
           tempxl => temporalcapas%listadoDePixeles%head
           
           do while ( associated(tempxl) )
            !  print *, "entro2"
          !    write(*,'(A,I0,A,I0,A,A)') "FILA: ",tempxl%value%fila, &
           !    " COLUMNA: ", tempxl%value%columna," COLOR: ",tempxl%value%color
              call usuarioActual%mimagenes%insert(tempxl%value%fila, tempxl%value%columna, tempxl%value%color)
            !  print *, "lo crea"
              tempxl => tempxl%next
             ! print *, "siguiente"
           end do
           !print *, "salio"
          end if
          temporalnlista => temporalnlista%next
          !print*, "asignolista"
        end do
      end if
      call usuarioActual%mimagenes%graphMatrix()
    end subroutine crearImagen

    subroutine reportesAdministrador
      character(len=1) :: opce
      type(linkedlist) :: new_lista
      integer :: totalimagenesalbum   
      do 
             print *, "" 
             write(*, '(A)') "|-----------------------------------------|"
             write(*, '(A)') "|     Pixel Print Studio Reportes AD      |"
             write(*, '(A)') "|-----------------------------------------|"
             write(*, '(A)') "| 1. Buscar Usuario                       |" 
             write(*, '(A)') "| 2. Listado de Usuarios                 |"     
             write(*, '(A)') "| 3. Regresar                             |"  
             write(*, '(A)') "|-----------------------------------------|"
             write(*, '(A)') "Ingrese el numero de opcion"
               read *, opce
               select case (opce)
               case ('1')
                write(*, '(A)') "Ingrese el dpi del Usuario"
                read *, dpi
                usuarioActual => Usuarios%searchUser(dpi)
                if (associated(usuarioActual)) then
                  write(*,'(A,I0,A)') "---------Datos Usuario ",dpi," ---------- "
                  print *, "---------Datos Principales---------- "
                 call usuarioActual%imprimirDatos()
                 print *, "---------Listado de Albums---------- "
                 write(*,'(A,I0)') "Cantidad Total de albums: ",usuarioActual%listadoDeAlbums%size
                 call usuarioActual%listadoDeAlbums%imprimeA()
                 write(*,'(A,I0)')  "Cantidad de Imagenes en el arbol: ", usuarioActual%arbolDeImagenes%num
                 totalimagenesalbum= usuarioActual%listadoDeAlbums%contarImagenes()
                 write(*,'(A,I0)')  "Cantidad de Imagenes en los Albums ", totalimagenesalbum
                 usuarioActual%arbolDeCapas%listado = new_lista
                 call usuarioActual%arbolDeCapas%recorrido(usuarioActual%arbolDeCapas%root)
                 write(*,'(A,I0)') "Capas por arbol de usuario: ",usuarioActual%arbolDeCapas%listado%size
                end if
               case ('2')
                call Usuarios%recorridopornivel()
               case ('3')
                   exit
               case default
                 print *, "Opcion no valida. Por favor, intente de nuevo."
               end select   
         end do
    end subroutine reportesAdministrador

    
    subroutine reportesUsuarios
      character(len=1) :: opce
      do 
             print *, "" 
             write(*, '(A)') "|-------------------------------------------------------|"
             write(*, '(A)') "|         Pixel Print Studio Reportes De Usuario        |"
             write(*, '(A)') "|-------------------------------------------------------|"
             write(*, '(A)') "| 1. Top 5 de imagenes con mas numero de capas          |" 
             write(*, '(A)') "| 2. Todas las capas que son hojas                      |"
             write(*, '(A)') "| 3. Profundidad de arbol de capas                      |"    
             write(*, '(A)') "| 4. Listar las capas en: preorden, inorden, postorden  |" 
             write(*, '(A)') "| 5. Regresar                                           |"  
             write(*, '(A)') "|-------------------------------------------------------|"
             write(*, '(A)') "Ingrese el numero de opcion"
               read *, opce
               select case (opce)
               case ('1')
                  call usuarioActual%arbolDeImagenes%top5()
               case ('2')
                  call usuarioActual%arbolDeCapas%print_leaf_layers()
               case ('3')
                  write(*,'(A,I0)') "La profundidad del arbol de capas es: ", usuarioActual%arbolDeCapas%depthf()
               case ('4')
                  call usuarioActual%arbolDeCapas%graph("ArbolDeCapas")
                  write(*,'(A)') "Listado de Capas en Preorden"
                  call usuarioActual%arbolDeCapas%preorder()
                  write(*,'(A)')"Listado de Capas en Inorden"
                  call usuarioActual%arbolDeCapas%inorder()
                  write(*,'(A)')"Listado de Capas en Posorden"
                  call usuarioActual%arbolDeCapas%posorder()
               case ('5')
                   exit
               case default
                 print *, "Opcion no valida. Por favor, intente de nuevo."
               end select   
         end do
    end subroutine reportesUsuarios
end program main
