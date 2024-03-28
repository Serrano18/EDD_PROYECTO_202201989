module arbolavl
    use uuid_module
    use abbidm   
    implicit none

    type :: Imagen
    private
        integer :: id
        type(abbid) :: arbolIdCapas
        contains
        procedure :: setIdImg
        procedure :: getId
        procedure :: agregarNodoIdCapas
    end type Imagen

    type :: nodo
        type(Imagen) :: valor
        integer :: altura = 1
        type(nodo), pointer :: derecha => null()
        type(nodo), pointer :: izquierda => null() 
    end type

    type, public :: avl
        type(nodo), pointer :: raiz => null()
    
    contains
        procedure :: insertavl
        procedure :: deleteavl
        procedure :: preorden
        procedure :: graficar
        procedure :: graficarArbolDeCapasDeImagen
    end type avl

contains
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
        call insertRec(self%raiz, val)
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

    recursive subroutine insertRec(raiz, val)
        type(nodo), pointer, intent(inout) :: raiz
        type(Imagen), intent(in) :: val

        if(.not. associated(raiz)) then
            allocate(raiz)
            raiz = nodo(valor=val)
        
        else if(val%id < raiz%valor%id) then 
            call insertRec(raiz%izquierda, val)

        else if(val%id > raiz%valor%id) then
            call insertRec(raiz%derecha, val)
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