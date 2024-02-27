program main
 
  use cola_clientes
  use listaVentanilla
  use Cliente
  use colaImpresion
  use conteopaso
  implicit none
  type(clientes) :: clie
  type(cola) :: colaClientes
  type(listaVentanas) :: listaSVentana
  type(colaCI) :: colaImagenG, colaImagenP
  character(len=1) :: opcion
  character (len=256 ) :: archivo
  integer :: nventana 
  integer :: idVentanilla
  pasos=1
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
        !Esto es para eliminar en la cola 
        call colaImagenG%eliminarNodoAntiguo()
        call colaImagenP%eliminarNodoAntiguo()
        !Esto es para enciar las imagenes
        call listaSVentana%ConteoImgagen(colaImagenG,colaImagenP)
        !call colaImagenG%print("IMG_G")
        !call colaImagenP%print("IMG_P")
        clie = colaClientes%eliminarClienteMasAntiguo()
        idVentanilla = listaSVentana%ObtenerVentanillaSinConfirmar()
        if (idVentanilla /= -1) then
            call listaSVentana%agregarClienteActual(idVentanilla, clie)
        end if
        pasos = pasos + 1
      case ('4')
          
      case ('5')
          
      case ('6')
          
      case ('7')
          exit
      case default
          print *, "Opcion no valida. Por favor, intente de nuevo."
      end select   
  end do

  print *, "Has salido del programa. Hasta luego."
end program main
