module arbolavl
    use uuid_module
    use abbidm   
    implicit none

    type :: Imagen
    
        integer :: id
        type(abbid) :: arbolIdCapas
        type(linkedlist) :: capa
        contains
        procedure :: agregarcapa
        procedure :: eliminarCapa
        procedure :: setIdImg
        procedure :: getId
        procedure :: agregarNodoIdCapas
    end type Imagen

    type :: nodo
        type(Imagen) :: valor
        integer :: newId
        integer :: height
        integer :: altura = 1
        type(nodo), pointer :: derecha => null()
        type(nodo), pointer :: izquierda => null() 
    end type

    type, public :: avl
        integer :: uid = 1
        integer :: num = 0
        integer :: ncapas = 0
        type(nodo), pointer :: raiz => null()
    
      contains
        procedure :: insertavl
        procedure :: deleteavl
        procedure :: preorden
        procedure :: graficar
        procedure :: graficarArbolDeCapasDeImagen
        procedure :: getheight
        procedure :: getmax
        procedure :: searchavl
        procedure :: amplitudeavl
        procedure :: getTotalCapas
        procedure :: search_recavl
        procedure :: amplitude_recavl
        procedure :: insertRec
    end type avl

contains
    subroutine getTotalCapas(this, tmp)
        class(avl), intent(inout) :: this
        type(nodo), intent(in), pointer :: tmp
        if( .not. associated(tmp)) then
            return
        end if
        this%ncapas = this%ncapas + tmp%valor%capa%size
        call this%getTotalCapas(tmp%izquierda)
        call this%getTotalCapas(tmp%derecha)
    end subroutine getTotalCapas

    integer function getheight (this, tmp)
        class(avl), intent(in) :: this
        type(nodo), intent(in), pointer :: tmp
        if (.not. associated(tmp)) then
            getheight = -1
        else
            getheight = tmp%height
        end if
    end function getheight

    integer function getmax(this, val1, val2)
        class(avl), intent(in) :: this
        integer, intent(in) :: val1, val2
        getmax = merge(val1, val2, val1 > val2)
    end function getmax

    function searchavl(this, tid) result(res)
        class(avl), intent(in) :: this
        integer, intent(in) :: tid
        type(Imagen), pointer :: res
        type(Imagen) :: imag
        call imag%setIdImg(tid)
        res => this%search_recavl(imag, this%raiz)
    end function searchavl

    recursive function search_recavl(this, image, tmp) result(res)
        type(Imagen), target, intent(in) :: image
        class(avl), intent(in) :: this
        
        type(nodo), pointer, intent(in) :: tmp
        type(Imagen), pointer :: res
        if (.not. associated(tmp)) then
            res => null()
        else if (image%id < tmp%valor%id) then
            res => this%search_recavl(image, tmp%izquierda)
        else if (image%id > tmp%valor%id) then
            res => this%search_recavl(image, tmp%derecha)
        else
            res => tmp%valor
        end if
    end function search_recavl

    subroutine amplitudeavl(this)
        class(avl), intent(in) :: this
        integer :: i
        do i = 0, this%getheight(this%raiz)
            call this%amplitude_recavl(this%raiz, i)
        end do
    end subroutine amplitudeavl

    subroutine amplitude_recavl(this, tmp, level)
        class(avl), intent(in) :: this
        type(nodo), pointer, intent(in) :: tmp
        integer, intent(in) :: level
        if (.not. associated(tmp)) then
            return
        end if
        if (level == 0) then
            write (*, '(1I3)', advance='no') (tmp%valor%id)
        else
            call this%amplitude_recavl(tmp%izquierda, level-1)
            call this%amplitude_recavl(tmp%derecha, level-1)
        end if
    end subroutine amplitude_recavl


    subroutine agregarcapa(this, id)
        class(Imagen), intent(inout) :: this
        integer :: id
        call this%capa%addlist(id)
    end subroutine agregarcapa

    subroutine eliminarCapa(this, id)
        class(Imagen), intent(inout) :: this
        integer :: id
        call this%capa%remove(id)
    end subroutine eliminarCapa

    subroutine agregarNodoIdCapas(this, value)
        class(Imagen), intent(inout) :: this
        integer, intent(in) :: value
        call this%arbolIdCapas%add(value)
    end subroutine agregarNodoIdCapas

    subroutine setIdImg(this, newId)
        class(Imagen), intent(inout) :: this
        integer, intent(in) :: newId
        this%id = newId
    end subroutine setIdImg

    function getId(this) result(id)
        class(Imagen), intent(in) :: this
        integer :: id
        id = this%id
    end function getId
    
    subroutine insertavl(self, val)
        class(avl), intent(inout) :: self
        type(Imagen), intent(in) :: val
        if (associated(self%searchavl(val%id))) then
            print *, "La imagen con ID ", val%id, " ya existe en el Arbol."
        else
            call self%insertRec(self%raiz, val)
        end if
        
    end subroutine insertavl

    subroutine deleteavl(self, val)
        class(avl), intent(inout) :: self
        type(Imagen), intent(in) :: val

        self%raiz => deleteRec(self%raiz, val)
    end subroutine deleteavl

    subroutine preorden(self)
        class(avl), intent(in) :: self
        
        call preordenRec(self%raiz)
    end subroutine preorden

    recursive subroutine insertRec(this,raiz, val)
        class(avl), intent(inout) :: this
        type(nodo), pointer, intent(inout) :: raiz
        type(Imagen), intent(in) :: val
        integer :: r, l, m
        if(.not. associated(raiz)) then
            allocate(raiz)
            raiz%valor=val
            raiz%newId = this%uid
            raiz%height=0
            this%uid = this%uid + 1
            this%num = this%num + 1
        else if(val%id < raiz%valor%id) then 
            call this%insertRec(raiz%izquierda, val)

        else if(val%id > raiz%valor%id) then
            call this%insertRec(raiz%derecha, val)
        end if

        raiz%altura = maximo(obtenerAltura(raiz%izquierda), obtenerAltura(raiz%derecha)) + 1

        if(obtenerBalance(raiz) > 1) then
            if(obtenerBalance(raiz%derecha) < 0) then
                raiz%derecha => rotacionDerecha(raiz%derecha)
                raiz => rotacionIzquierda(raiz)
            else
                raiz => rotacionIzquierda(raiz)
            end if
        end if

        if(obtenerBalance(raiz) < -1) then
            if(obtenerBalance(raiz%izquierda) > 0) then
                raiz%izquierda => rotacionIzquierda(raiz%izquierda)
                raiz => rotacionDerecha(raiz)

            else
                raiz => rotacionDerecha(raiz)
            end if
        end if
        r = this%getheight(raiz%derecha)
        l = this%getheight(raiz%izquierda)
        m = this%getmax(r, l)
        raiz%height = m + 1
    end subroutine insertRec

    recursive function deleteRec(raiz, val) result(res)
        type(nodo), pointer :: raiz
        type(Imagen), intent(in) :: val

        type(nodo), pointer :: temp
        type(nodo), pointer :: res 
        
        if(.not. associated(raiz)) then
            res => raiz
            return
        end if

        if(val%id < raiz%valor%id) then
            raiz%izquierda => deleteRec(raiz%izquierda, val)
        
        else if(val%id > raiz%valor%id) then
            raiz%derecha => deleteRec(raiz%derecha, val)

        else
            if(.not. associated(raiz%izquierda)) then
                temp => raiz%derecha
                deallocate(raiz)
                res => temp

            else if (.not. associated(raiz%derecha)) then
                temp => raiz%izquierda
                deallocate(raiz)
                res => temp
            
            else
                call obtenerMayorDeMenores(raiz%izquierda, temp)
                raiz%valor = temp%valor
                raiz%izquierda => deleteRec(raiz%izquierda, temp%valor)
            end if
        end if

        res => raiz
        if(.not. associated(raiz)) return

        raiz%altura = maximo(obtenerAltura(raiz%izquierda), obtenerAltura(raiz%derecha))

        if(obtenerBalance(raiz) > 1) then
            if(obtenerBalance(raiz%derecha) < 0) then
                raiz%derecha => rotacionDerecha(raiz%derecha)
                raiz => rotacionIzquierda(raiz)
            else
                raiz => rotacionIzquierda(raiz)
            end if
        end if

        if(obtenerBalance(raiz) < -1) then
            if(obtenerBalance(raiz%izquierda) > 0) then
                raiz%izquierda => rotacionIzquierda(raiz%izquierda)
                raiz => rotacionDerecha(raiz)

            else
                raiz => rotacionDerecha(raiz)
            end if
        end if

        res => raiz
    end function deleteRec

    function rotacionIzquierda(raiz) result(raizDerecha)
        type(nodo), pointer, intent(in) :: raiz
        type(nodo), pointer :: raizDerecha
        type(nodo), pointer :: temp

        raizDerecha => raiz%derecha
        temp => raizDerecha%izquierda

        raizDerecha%izquierda => raiz
        raiz%derecha => temp

        raiz%altura = maximo(obtenerAltura(raiz%izquierda), obtenerAltura(raiz%derecha)) + 1
        raizDerecha%altura = maximo(obtenerAltura(raizDerecha%izquierda), obtenerAltura(raizDerecha%derecha)) + 1
    end function rotacionIzquierda

    function rotacionDerecha(raiz) result(raizIzquierda)
        type(nodo), pointer, intent(in) :: raiz
        type(nodo), pointer :: raizIzquierda
        type(nodo), pointer :: temp

        raizIzquierda => raiz%izquierda
        temp => raizIzquierda%derecha

        raizIzquierda%derecha => raiz
        raiz%izquierda => temp

        raiz%altura = maximo(obtenerAltura(raiz%izquierda), obtenerAltura(raiz%derecha)) + 1
        raizIzquierda%altura = maximo(obtenerAltura(raizIzquierda%izquierda), obtenerAltura(raizIzquierda%derecha)) + 1
    end function rotacionDerecha

    recursive subroutine obtenerMayorDeMenores(raiz, mayor)
        type(nodo), pointer :: raiz, mayor
        if(associated(raiz%derecha)) then
            call obtenerMayorDeMenores(raiz%derecha, mayor)
        else
            mayor => raiz
        end if
    end subroutine obtenerMayorDeMenores

    recursive subroutine preordenRec(raiz)
        type(nodo), pointer, intent(in) :: raiz

        if(associated(raiz)) then
            print *, raiz%valor%id
            call preordenRec(raiz%izquierda)
            call preordenRec(raiz%derecha)
        end if
    end subroutine preordenRec

    function maximo(izquierda, derecha) result(res)
        integer, intent(in) :: izquierda
        integer, intent(in) :: derecha

        integer :: res
        res = derecha

        if(izquierda >= derecha) then
            res = izquierda
            return
        end if
    end function maximo

    function obtenerBalance(raiz) result(res)
        type(nodo), pointer, intent(in) :: raiz
        integer :: res
        
        res = obtenerAltura(raiz%derecha) - obtenerAltura(raiz%izquierda)
    end function

    function obtenerAltura(n) result(res)
        type(nodo), pointer :: n
        integer :: res
        res = 0

        if(.not. associated(n)) return
        res = n%altura
    end function obtenerAltura

    recursive subroutine imprimirRec(raiz, nombre, io)
        type(nodo), pointer, intent(in) :: raiz
        character(len=36), intent(in) :: nombre
        integer :: io

        character(len=36) :: derecha
        character(len=36) :: izquierda

        derecha = generate_uuid()
        izquierda = generate_uuid()

        if(associated(raiz)) then
            !"Nodo_uuid"[Label="1"]
            write(io, *) '"Nodo'//nombre//'"[label= "', raiz%valor%id, '"]'

            if(associated(raiz%izquierda)) then
                !"Nodo_uuid"->"Nodo_uuidHijoIzquierdo"
                write(io, *) '"Nodo'//nombre//'"->"Nodo'//izquierda//'"'
            end if

            if(associated(raiz%derecha)) then
                !"Nodo_uuid"->"Nodo_uuidHijoDerecho"
                write(io, *) '"Nodo'//nombre//'"->"Nodo'//derecha//'"'
            end if
            call imprimirRec(raiz%izquierda, izquierda, io)
            call imprimirRec(raiz%derecha, derecha, io)
        end if
    end subroutine imprimirRec

    subroutine graficar(self)
        class(avl), intent(in) :: self
        integer :: io
        integer :: i
        character(len=100) :: comando

        io = 1
        open(newunit=io, file="./ArbolImagenes.dot")
        comando = "dot -Tpng ./ArbolImagenes.dot -o ./ArbolImagenes.png"

        write(io, *) "digraph G {"
            !Graficar
        if(associated(self%raiz)) then
            call imprimirRec(self%raiz, generate_uuid(), io)
        end if
        write(io, *) "}"
        close(io)

        call execute_command_line(comando, exitstat=i)
        call execute_command_line('start ArbolImagenes.png')
        if(i == 1) then
            print *, "Error al momento de crear la imagen"
        else
            print *, "La imagen fue generada exitosamente"
        end if
    end subroutine graficar

    subroutine graficarArbolDeCapasDeImagen(self,img)
        class(avl), intent(in) :: self
        integer,intent(in) :: img
        integer :: io
        integer :: i
        character(len=100) :: comando

        io = 1
        open(newunit=io, file="./ArbolImagenyCapas.dot")
        comando = "dot -Tpng ./ArbolImagenyCapas.dot -o ./ArbolImagenyCapas.png"

        write(io, *) "digraph G {"
            !Graficar
        if(associated(self%raiz)) then
            call imprimirRec2(self%raiz, generate_uuid(), io,img)
        end if
        write(io, *) "}"
        close(io)

        call execute_command_line(comando, exitstat=i)
       
        if(i == 1) then
            print *, "Error al momento de crear la imagen"
        else
            print *, "La imagen fue generada exitosamente"
            call execute_command_line('start ArbolImagenyCapas.png')
        end if
    end subroutine graficarArbolDeCapasDeImagen
    
    recursive subroutine imprimirRec2(raiz, nombre, io,img)
        type(nodo), pointer, intent(in) :: raiz
        character(len=36), intent(in) :: nombre
        integer,intent(in) :: img
        integer :: io

        character(len=36) :: derecha
        character(len=36) :: izquierda

        derecha = generate_uuid()
        izquierda = generate_uuid()

        if(associated(raiz)) then
            !"Nodo_uuid"[Label="1"]
            write(io, *) '"Nodo'//nombre//'"[label= "', raiz%valor%id, '"]'
            if(raiz%valor%id == img) then
                write(io,'(A,A,I0)') '"Nodo'//nombre, '"  ->  l',raiz%valor%arbolIdCapas%root%uid
                call raiz%valor%arbolIdCapas%dotgen_rec(raiz%valor%arbolIdCapas%root,io)
            end if

            if(associated(raiz%izquierda)) then
                !"Nodo_uuid"->"Nodo_uuidHijoIzquierdo"
                write(io, *) '"Nodo'//nombre//'"->"Nodo'//izquierda//'"'
            end if

            if(associated(raiz%derecha)) then
                !"Nodo_uuid"->"Nodo_uuidHijoDerecho"
                write(io, *) '"Nodo'//nombre//'"->"Nodo'//derecha//'"'
            end if
            call imprimirRec2(raiz%izquierda, izquierda, io,img)
            call imprimirRec2(raiz%derecha, derecha, io,img)
        end if
    end subroutine imprimirRec2
end module arbolavl