program main
  use cargasmasivas
 
  implicit none
  type(sucursal),pointer :: sucursal_actual
  type(tecnicos),pointer :: tecnico_actual
  integer(kind=8)::dpi
  type(sucursalesavl) :: sucursales
  type(lista_rutas) ::rut
  character(len=1) :: opcion
  
  character (len=256 ) :: archivo
  do 
    write(*, '(A)') "|------------------------------------------|"
    write(*, '(A)') "|   Bienvenido A  Pixel Print Studio      |"
    write(*, '(A)') "|-----------------------------------------|"           
    write(*, '(A)') "| 1. Iniciar Sesion                       |"  
    write(*, '(A)') "| 2. Salir                                |"  
    write(*, '(A)') "|-----------------------------------------|"
    write(*, '(A)') "Ingrese el numero de opcion"
      read *, opcion
      select case (opcion)
        case ('1')
          call iniciar_sesion()
        case ('2')
          exit
        case default
          write(*, '(A)') "Opcion no valida"
      end select
  end do

  contains
    subroutine iniciar_sesion()
      implicit none
      character(len=20) :: usuario
      character(len=20) :: contrasena
      write(*, '(A)') "Ingrese su usuario"
      read *, usuario
      write(*, '(A)') "Ingrese su contrasena"
      read *, contrasena
      if (usuario == "EDD1S2024" .and. contrasena == "ProyectoFase3") then
        write(*, '(A)') "Bienvenido administrador"
        call menu_admin()
      else
        write(*, '(A)') "Usuario o contrasena incorrecta"
      end if
    end subroutine iniciar_sesion

    subroutine menu_admin()
      implicit none
      character(len=1) :: opc
      character(len=256) :: pass  ! Asegúrate de que 'pass' es declarado antes de ser utilizado
      integer :: sucursal_id
      do 
        write(*, '(A)') "|-----------------------------------------|"
        write(*, '(A)') "|   Bienvenido A  Pixel Print Studio      |"
        write(*, '(A)') "|-----------------------------------------|"           
        write(*, '(A)') "| 1. Carga de Sucursales                  |"  
        write(*, '(A)') "| 2. Carga de Rutas                       |" 
        write(*, '(A)') "| 3. Sucursales                           |"  
        write(*, '(A)') "| 4. Reportes                             |"
        write(*, '(A)') "| 5. Salir                                |"   
        write(*, '(A)') "|-----------------------------------------|"
        write(*, '(A)') "Ingrese el numero de opcion"
          read *, opc
          select case (opc)
            case ('1')
               !Codigo para la carga de capas
            print *, "Ingrese el nombre del Archivo de Sucursales: "
            read *, archivo
            call cargaMasivaSucursales(archivo,sucursales)
            
            case ('2')
              print *, "Ingrese el nombre del Archivo de Rutas: "
              read *, archivo
              call cargaMasivaRutas(archivo,rut)
            case ('3')
              call sucursales%inorder(sucursales%root)
              write(*, '(A)') "Ingrese el ID de la sucursal:"
              read *, sucursal_id
  
              write(*, '(A)') "Ingrese la contraseña:"
              read *, pass
              sucursal_actual => sucursales%buscar(sucursal_id)
              if(associated(sucursal_actual)) then
                if(sucursal_actual%contrasena == pass) then
                  call menu_sucursales()
                else
                  write(*, '(A)') "Contraseña incorrecta"
                end if
              else
                write(*, '(A)') "Sucursal no encontrada"
              end if

            case ('4')
              call menu_graficos()
            case ('5')
              exit
            case default
              write(*, '(A)') "Opcion no valida"
          end select
      end do
    end subroutine menu_admin

    subroutine menu_sucursales()
      character(len=1) :: opc3
      do
        write(*, '(A)') "|-----------------------------------------|"
        write(*, '(A)') "|   1. Carga de Tecnicos                  |"
        write(*, '(A)') "|   2. Generar recorrido más optimo       |"
        write(*, '(A)') "|   3. Informacion tecnico en Especifico  |"
        write(*, '(A)') "|   4. Listar Tecnicos                    |"
        write(*, '(A)') "|   5. Generar Reporte                    |"
        write(*, '(A)') "|   6. Regresar                           |"
        write(*, '(A)') "|-----------------------------------------|"
        read *, opc3
        select case (opc3)
          case ('1')
            print *, "Ingrese el nombre del Archivo de Tecnicos: "
            read (*,'(A)') archivo
            call cargaMasivaTecnicos(archivo,sucursal_actual%tecnicos)
          case ('2')
          case ('3')
            write(*, '(A)') "Ingrese el DPI del Tecnico:"
            read *, dpi
            call sucursal_actual%tecnicos%buscart(dpi)
          case ('4')
            call sucursal_actual%tecnicos%listadotecnicos()
          case ('5')
          case ('6')
            exit
          case default
            write(*, '(A)') "Opcion no valida"
        end select
      end do
    end subroutine menu_sucursales

    subroutine menu_graficos()
      character(len=1) :: opc4
      do
        write(*, '(A)') "|-----------------------------------------|"
        write(*, '(A)') "|   1. Grafo de sucursales                |"
        write(*, '(A)') "|   2. Grafico de Rutas                   |"
        write(*, '(A)') "|   3. Arbol de Merckle                   |"
        write(*, '(A)') "|   4. BlockChain                         |"
        write(*, '(A)') "|   5. Tabla Hash                         |"
        write(*, '(A)') "|   6. Regresar                           |"
        write(*, '(A)') "|-----------------------------------------|"
        read *, opc4
        select case (opc4)
        case ('1')
          call sucursales%dotgen()
        case ('2')
        case ('3')  
        case ('4')
        case ('5')  
        case ('6')
          exit
        end select
      end do

    end subroutine menu_graficos
    
end program main
