module TokenModule
    implicit none
    type :: Token
        character(len=100) :: descripcion
        character(len=100) :: tipo
        integer :: linea
        integer :: columna
    end type Token

    contains
    
    subroutine initToken(buffer_, tipo, linea, columna, t)
        implicit none
        character(len=*), intent(in) :: buffer_, tipo
        integer, intent(in) :: linea, columna
        type(Token), intent(inout) :: t

        ! Limpiar el token
        t%descripcion = ""
        t%tipo = ""
        t%linea = 0
        t%columna = 0

        ! Asignar valores al token
        t%descripcion = trim(buffer_)
        t%tipo = tipo
        t%linea = linea
        t%columna = columna
    end subroutine initToken

    subroutine addToken(tokens, buffer_, tipo, linea, columna)
        implicit none
        type(Token), allocatable, intent(inout) :: tokens(:)
        character(len=*), intent(in) :: buffer_, tipo
        integer, intent(in) :: linea, columna
        type(Token) :: t
        integer :: n
        ! Inicializar el token
        call initToken(buffer_, tipo, linea, columna, t)
        print *, "agregando token", buffer_
        ! Verificar si el arreglo de tokens está asignado
        if (.not. allocated(tokens)) then
            allocate(tokens(1))
            tokens(1) = t
        else
            ! Añadir el token al array
            n = size(tokens)
            call extendArrayTokens(tokens)
            tokens(n+1) = t
        end if
    end subroutine addToken
    
    subroutine extendArrayTokens(tokens)
        implicit none
        type(Token), allocatable, intent(inout) :: tokens(:)
        type(Token), allocatable :: temp(:)
        integer :: n

        ! extender el array
        n = size(tokens)
        allocate(temp(n+1))
        temp(1:n) = tokens
        deallocate(tokens)
        tokens = temp
        
    end subroutine extendArrayTokens

end module TokenModule
