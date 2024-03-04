

program main
 
  use Cliente
  use cola_clientes
  use listaVentanilla
  use colaImpresion
  use ListaDeEspera
  use clienteAtendidos
  use conteopaso
  implicit none
  character(len=100):: nombreb
  type(clientes) :: clie
  type(cola) :: colaClientes
  type(listaVentanas) :: listaSVentana
  type(colaCI) :: colaImagenG, colaImagenP
  type(listaespera) :: le
  type(listaClientesA)::listaDeClientesAtendidos
  type(listaClientesA) :: topmayores
  type(listaClientesA) :: topmenores,toppasos
  character(len=1) :: opc
  character(len=1) :: opcion
  character (len=256 ) :: archivo
  integer :: nventana 
  integer :: idVentanilla
  pasos=1
  conteoid = 100

  do 
    print *, ""
      print *, "|-----------------------------------------|"
      print *, "|             Menu Principal              |"
      print *, "|-----------------------------------------|"
      print *, "| 1. Carga masiva de clientes             |"              
      print *, "| 2. Cantidad de Ventanillas              |"  
      print *, "| 3. Ejecutar Paso                        |"  
      print *, "| 4. Estado en memoria de las estructuras |"
      print *, "| 5. Reportes                             |"  
      print *, "| 6. Datos Estudiantiles                  |"  
      print *, "| 7. Salir                                |"  
      print *, "|-----------------------------------------|"
      print *, "Ingrese el numero de opcion"
      read *, opcion
      select case (opcion)
      case('1')
        print *, ""
        print *, "Ingrese el nombre del Archivo: "
        read *, archivo
        call colaClientes%guardar_json(archivo)
        print *, "Se cargaron todos los datos del archivo"        
      case('2')
        print *, ""
        print *, "Ingrese la cantidad de Ventanillas"
        read *, nventana
        call listaSVentana%Vinsertar(nventana)
        print *, "Se crearon todas las ventanillas"  
      case ('3')
        print *, ""
        call colaClientes%clientesAleatorios()
        print *, ""
        print *, '|------------------------------------------------------------|'
        write(*, '(A, I0, A)') '|------------------------- Paso ',pasos,' ----------------------------|'
        print *, '|------------------------------------------------------------|'
  
        call le%verificarAtendidos(listaDeClientesAtendidos,pasos)
                !Esto es para eliminar en la cola 
        call colaImagenG%eliminarNodoAntiguo(le)
        call colaImagenP%eliminarNodoAntiguo(le)
        !Esto es para enciar las imagenes
        call listaSVentana%ConteoImgagen(colaImagenG,colaImagenP,le)
        clie = colaClientes%eliminarClienteMasAntiguo()
        idVentanilla = listaSVentana%ObtenerVentanillaSinConfirmar()
        if (idVentanilla /= -1) then
        call listaSVentana%agregarClienteActual(idVentanilla, clie,pasos)
        end if
        pasos = pasos + 1
        print *, '|------------------------------------------------------------|'
        print *, ""
      case ('4')
          call colaClientes%graficarcolaclientes('colaclientes.dot')
          call colaImagenG%graficarcolaimpresion(colaImagenP,'colaImpresion.dot')
          call  listaDeClientesAtendidos%graficaratendidos('listaAtendidos.dot')
          call listaSVentana%graficarventanillas('listaventanas.dot')
      case ('5')
        do
          print *, ""
          print *, '|------------------------------------------------|'
          print *, '|-----------------Reportes ----------------------|'
          print *, '|------------------------------------------------|'
          print *, "| 1. Top clientes mayor IMG_G                    |"              
          print *, "| 2. Top clientes menor IMG_P                    |"  
          print *, "| 3. Persona con mas Pasos                       |"  
          print *, "| 4. Buscar cliente en el sistema                |"
          print *, "| 5. exit                                        |"
          print *, '|------------------------------------------------|'
          read *, opc
          select case (opc)
          case('1')
            print *, "Top clientes mayor IMG_G "
            call listaDeClientesAtendidos%copia(topmayores)
            call topmayores%ordenarPorImgG()
            call topmayores%graficarTop('topmayores.dot')
          case('2')
            print *, "Top clientes Menor IMG_P "
            call listaDeClientesAtendidos%copia(topmenores)
            call topmayores%ordenarPorImgP()
            call topmayores%graficarTop('topmenores.dot')
          case('3')
            print *, "Persona con mas Pasos"               
            call listaDeClientesAtendidos%copia(toppasos)
            call toppasos%graficarClienteMasPasos('topmaspasos.dot')
          case('4')
            print *, "Buscar cliente en el sistema"
            call listaDeClientesAtendidos%imprimirListaNombres()
            print *, "Ingrese el nombre del Cliente"
            read (*,'(A)'), nombreb
            nombreb = trim(nombreb)
            call listaDeClientesAtendidos%graficarCliente(nombreb,'cliente.dot')
          case('5')
            exit   
          case default
            print *, "Opcion no valida. Por favor, intente de nuevo."
          end select
        end do
      case ('6')
        print *, "------------Datos Estudiantiles------------"
        print *, "Universidad de San Carlos de Guatemala"
        print *, "Facultad de Ingenieria"
        print *, "Ingenieria en Ciencias y Sistemas"
        print *, "Estructura de Datos"
        print *, "Maria Patricia Serrano Ramires"
        print *, "202201989"
        print *, "--------------------------------------------"  
      case ('7')
          exit
      case default
          print *, "Opcion no valida. Por favor, intente de nuevo."
      end select   
  end do
  print *, "Has salido del programa. Hasta luego."
end program main