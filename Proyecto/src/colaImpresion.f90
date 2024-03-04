module colaImpresion
    
    implicit none
    integer :: ConteoIP = 0
    type, public :: imag
        integer :: id
        character (len=5) :: tipo
        character (len=100) :: nombreCl
        integer :: idClie
    end type imag

    type, public :: nodoCI
        type(imag):: value
        type(nodoCI), pointer :: next => null()
    end type nodoCI

    type, public :: colaCI
        integer :: llamadas = 0
        type(nodoCI), pointer :: head => null()
        type(nodoCI), pointer :: tail => null()
        contains
        procedure :: appendcl
        procedure :: deletecl
        procedure :: printcl
        procedure :: eliminarNodoAntiguo
        procedure :: graficarcolaimpresion
    end type colaCI

    contains

    subroutine appendcl(this,cantIG,nombrel,tip,idCle)
        class(colaCI), intent(inout) :: this
        integer, intent (in) :: cantIG,idCle
        character (len=5), intent (in) :: tip
        character (len=100), intent (in) :: nombrel
        type(nodoCI), pointer :: temp
        integer :: i

        do i = 1, cantIG
            
            allocate(temp)
            temp%value%tipo = tip
            temp%value%id = ConteoIP
            temp%value%nombreCl = nombrel
            temp%value%idClie = idCle
            ConteoIP = ConteoIP + 1
            temp%next => null()
            if (.not. associated(this%head)) then
                this%head => temp
                this%tail => temp
            else
                this%tail%next => temp
                this%tail => temp
            end if
        end do

        !call this%printcl(tip)
    end subroutine appendcl

    subroutine deletecl(this)
        class(colaCI), intent(inout) :: this
        type(nodoCI), pointer :: temp

        if (.not. associated(this%head)) then
            print *, 'Cola Imagenes Pequeñas Vacia'
            return
        end if

        print *, 'Delete ', this%head%value
        temp => this%head
        this%head => this%head%next
        deallocate(temp)
    end subroutine deletecl

    subroutine printcl(this,tipos)
        class(colaCI), intent(in) :: this
        type(nodoCI), pointer :: current
         character (len=5):: tipos
        current => this%head

        print *, '|--------------------------------------------------------|'
        print *, '|                   Listado de ',tipos,"                     |"
        print *, '|--------------------------------------------------------|'
        do while (associated(current))
            write(*,*) current%value%nombreCl
            current => current%next
        end do 
    end subroutine printcl

    subroutine eliminarNodoAntiguo(this,esperal)
        use ListaDeEspera
        class(colaCI), intent(inout) :: this
        type (listaespera), intent(inout) :: esperal
        type(nodoCI), pointer :: current, previous, temp
        integer :: pasos
        if (.not. associated(this%head)) then
            !print *, 'La cola está vacía.'
            return
        end if

        this%llamadas = this%llamadas + 1
        current => this%head
        previous => null()
        do while (associated(current))
            if (current%value%tipo ==  "IMG_G") then
                pasos = this%llamadas
                if (pasos == 2) then
                    write(*, '(A, A, A, A)') "Se entrego una imagen al cliente ", trim(current%value%nombreCl)  
                    call esperal%incrementarImagenRecibida(current%value%nombreCl, &
                    current%value%idClie,&
                    current%value%tipo)
                    ! Eliminar el nodo
                    if (associated(previous)) then
                        previous%next => current%next
                    else
                        this%head => current%next
                    end if
                    temp => current
                    current => current%next
                    deallocate(temp)
                    this%llamadas = 0
                    
                else
                    exit ! No es necesario seguir buscando
                end if
            elseif (current%value%tipo  == "IMG_P") then
                if (this%llamadas == 1) then
                    write(*, '(A, A, A, A)') "Se entrego una imagen al cliente ", trim(current%value%nombreCl) 
                    call esperal%incrementarImagenRecibida(current%value%nombreCl, &
                    current%value%idClie,&
                    current%value%tipo)
                    ! Eliminar el nodo
                    if (associated(previous)) then
                        previous%next => current%next
                    else
                        this%head => current%next
                    end if
                    temp => current
                    current => current%next
                    deallocate(temp)
                    this%llamadas = 0
                   
                else
                    exit ! No es necesario seguir buscando
                end if
            else
                previous => current
                current => current%next
            end if
        end do
        !call this%printcl("image")
    end subroutine eliminarNodoAntiguo
     
    subroutine graficarcolaimpresion(this, colapeque, filename)
        class(colaCI), intent(in) :: this
        class(colaCI), intent(in) :: colapeque
        character(len=*) :: filename
        integer :: unit
        type(nodoCI),pointer :: current
        integer :: count
        open(unit,file = filename,status='replace')
        write(unit, *) 'digraph colac {'
        write(unit, *) '    node [shape=box, style=filled];' ! Aplicar atributos a todos los nodos
        write(unit,*) 'rankdir = LR;'
        write(unit, *) '    "Impresora Grande" [label="Impresora Grande", shape=box, color=blue];'
    
        ! Escribir nodos y conexiones
        current => this%head
        count = 0
        write(unit, *) '    "Impresora Grande" -> "Node', count+1, '";' ! Conectar impresora grande con primer nodo de this
        do while (associated(current))
            count = count + 1
            write(unit, *) '    "Node', count, '" [label="', trim(current%value%tipo), '", shape=box];'
            if (associated(current%next)) then
                write(unit, *) '    "Node', count, '" -> "Node', count+1, '";'
           end if
            current => current%next
        end do 
        
          ! Escribir nodo de impresora pequeña
        write(unit, *) '    "Impresora Pequeña" [label="Impresora Pequeña", shape=box, color=red];'
        ! Escribir nodos y conexiones de la cola colapeque
        current => colapeque%head
        count = 0
        write(unit, *) '    "Impresora Pequeña" [label="Impresora Pequeña", shape=box, color=red];'

        ! Escribir nodos y conexiones de la cola colapeque
        current => colapeque%head
        count = 0
        write(unit, *) '    "Impresora Pequeña" ->  "NodeP', count+1, '";'! Conectar impresora pequeña con primer nodo de colapeque
        do while (associated(current))
            count = count + 1
            write(unit, *) '    "NodeP', count, '" [label="', trim(current%value%tipo),'", shape=box];'
            if (associated(current%next)) then
                write(unit, *) '    "NodeP', count, '" -> "NodeP', count+1, '";'
            end if
            current => current%next
        end do
        ! Cerrar el archivo DOT
        write(unit, *) '}'
        close(unit)
            ! Generar el archivo PNG utilizando Graphviz
        call system('dot -Tpng ' // trim(filename) // ' -o ' // trim(adjustl(filename)) // '.png')
        
        print *, 'Graphviz file generated: ', trim(adjustl(filename)) // '.png'

    end subroutine graficarcolaimpresion
end module colaImpresion
