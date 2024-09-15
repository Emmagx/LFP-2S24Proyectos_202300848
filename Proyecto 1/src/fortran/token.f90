module TokenModule
    implicit none
    type :: Token
        character(len=100) :: valor
        character(len=50) :: tipo
        integer :: linea, columna
    end type Token

contains
    subroutine addToken(tokens, buffer_, tipo, linea, columna)
        use utils
        implicit none
        type(Token), allocatable, intent(inout) :: tokens(:)
        character(len=100), intent(inout) :: buffer_
        character(len=*), intent(in) :: tipo
        integer, intent(in) :: linea, columna
        type(Token) :: nuevo_token  ! Eliminar 'allocatable' aquí

        ! Crear el nuevo token
        nuevo_token%valor = buffer_
        nuevo_token%tipo = tipo
        nuevo_token%linea = linea
        nuevo_token%columna = columna

        ! Imprimir el tipo del token leído
        print *, "Token leido: ", trim(buffer_), " | Tipo: ", trim(tipo), " | Linea: ", linea, " | Columna: ", columna

        ! Agregar el token al arreglo
        if (.not. allocated(tokens)) then
            allocate(tokens(1))
            tokens(1) = nuevo_token
        else
            call addToArray(tokens, nuevo_token)  ! Aquí se llama a 'addToArray'
        end if

        ! Limpiar el buffer
        call clearBuffer(buffer_)
    end subroutine addToken

    subroutine addToArray(tokens, nuevo_token)
        implicit none
        type(Token), allocatable, intent(inout) :: tokens(:)
        type(Token), intent(in) :: nuevo_token
        type(Token), allocatable :: temp(:)
        integer :: n
    
        n = size(tokens)
    
        allocate(temp(n + 1))
    
        temp(1:n) = tokens
    
        temp(n + 1) = nuevo_token
    
        call move_alloc(temp, tokens)
    end subroutine addToArray

end module TokenModule
