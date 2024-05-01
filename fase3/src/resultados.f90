module resultados
   
    use rutas, only: ruta
    implicit none
    type :: resultado
       integer :: idr,weightr,distanciar,impresiones,padre
       contains
          procedure :: imprimir
    end type resultado

    type :: nodores
       type(resultado) :: res
       type(nodores), pointer :: next => null() 
    end type nodores

    type :: listares
       type(nodores), pointer :: head => null()
       type(nodores), pointer :: tail => null()
       integer :: size = 0
       integer :: total_weightr = 0
       contains
          procedure :: insertarResultado
          procedure :: imprimirResultado
    end type listares
    contains
    
       subroutine imprimir(this)
        class(resultado), intent(in) :: this
        print *, 'Resultado ', this%idr
          print *, 'Weight: ', this%weightr
          print *, 'Distancia: ', this%distanciar
          print *, 'Impresiones: ', this%impresiones
          print *, 'Padre: ', this%padre
       end subroutine imprimir

       subroutine imprimirResultado(this)
        class(listares), intent(in) :: this
        type(nodores), pointer :: current

        current => this%head

        print *, '-------------------------------------'
        print *, 'Total Weight: ', this%total_weightr
        print *, '--------------------------------------'

        do while ( associated(current) )
            call current%res%imprimir()
            current => current%next
        end do
    end subroutine imprimirResultado
    
    subroutine insertarResultado(this, oruta)
      class(listares), intent(inout):: this
      type(ruta), intent(in) :: oruta
      type(nodores), pointer :: new_nres

      allocate(new_nres)

      new_nres%res%idr = oruta%sucursal1
      new_nres%res%weightr = oruta%weight
      new_nres%res%distanciar = oruta%distancia
      new_nres%res%impresiones = oruta%impresoras
      new_nres%res%padre = oruta%sucursal2

      if ( .not. associated(this%head) ) then
          this%head => new_nres
          this%tail => new_nres
          this%size = this%size + 1
          return
      end if
      this%tail%next => new_nres
      this%tail => new_nres
      this%total_weightr = this%tail%res%weightr
      this%size = this%size + 1
  end subroutine insertarResultado
end module resultados