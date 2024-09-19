module lexer
    use TokenModule
    use ErrorModule
    use utils
    implicit none

contains

subroutine state0(current_char, buffer_, tokens, errors, linea, columna, estado, salto_linea)
    implicit none
    character(len=1), intent(in) :: current_char
    character(len=100), intent(inout) :: buffer_
    type(Token), allocatable, intent(inout) :: tokens(:)
    type(Error), allocatable, intent(inout) :: errors(:)
    integer, intent(inout) :: linea, columna
    integer, intent(inout) :: estado
    logical, intent(in) :: salto_linea

    ! Ignorar espacios en blanco y caracteres especiales
    if (isSpecialChar(current_char)) then
        if (len_trim(buffer_) > 0) then
            call processBuffer(buffer_, errors, tokens, linea, columna)
        end if
        call clearBuffer(buffer_)
        return
    end if

    ! Procesar otros caracteres como delimitadores
    if (current_char == '"') then
        estado = 2
        call stateCadena(current_char, buffer_, tokens, errors, linea, columna, estado)
    else if (current_char == '{') then
        if (len_trim(buffer_) > 0) then
           call processBuffer(buffer_, errors, tokens, linea, columna)
        end if
        call addToken(tokens, current_char, "LLAVE_APERTURA", linea, columna)
        call clearBuffer(buffer_)
        return
    else if (current_char == '}') then
        if (len_trim(buffer_) > 0) then
            call processBuffer(buffer_, errors, tokens, linea, columna)
        end if
        call addToken(tokens, current_char, "LLAVE_CERRADURA", linea, columna)
        call clearBuffer(buffer_)
        return
    else if (current_char == ':') then
        if (len_trim(buffer_) > 0) then
            call processBuffer(buffer_, errors, tokens, linea, columna)
        end if
        call addToken(tokens, current_char, "DOS_PUNTOS", linea, columna)
        call clearBuffer(buffer_)
        return
    else if (current_char == ';') then
        if (len_trim(buffer_) > 0) then
            call processBuffer(buffer_, errors, tokens, linea, columna)
        end if
        call addToken(tokens, current_char, "PUNTO_Y_COMA", linea, columna)
        call clearBuffer(buffer_)
        return
    end if

    call addtoBuffer(current_char, buffer_, columna)
end subroutine state0

    ! Estado 1: Identificadores y palabras reservadas
subroutine state1(current_char, buffer_, tokens, errors, linea, columna, estado, i)
    implicit none
    character(len=1), intent(in) :: current_char
    character(len=100), intent(inout) :: buffer_
    type(Token), allocatable, intent(inout) :: tokens(:)
    type(Error), allocatable, intent(inout) :: errors(:)
    integer, intent(inout) :: linea, columna, estado
    integer, intent(inout) :: i 
    print *, 'Estado 1 :', current_char, ' Buffer: ', buffer_, ' Linea: ', linea, ' Columna: ', columna

    if ((current_char >= 'a' .and. current_char <= 'z') .or. &
        (current_char >= 'A' .and. current_char <= 'Z')) then
        call addtoBuffer(current_char, buffer_, columna)
    else
        call processBuffer(buffer_, errors, tokens, linea, columna)
        if (.not. isSpecialChar(current_char)) then
            call goBack(i, columna, buffer_)
        end if
        call clearBuffer(buffer_)
        
        call iraState(estado, 0, buffer_)
    end if
end subroutine state1

subroutine stateNumero(current_char, buffer_, tokens, errors, linea, columna, estado, i)
    implicit none
    character(len=1), intent(in) :: current_char
    character(len=100), intent(inout) :: buffer_
    type(Token), allocatable, intent(inout) :: tokens(:)
    type(Error), allocatable, intent(inout) :: errors(:)
    integer, intent(inout) :: linea, columna, estado
    integer, intent(inout) :: i
    print *, 'Estado Numero: ', current_char, ' Buffer:', buffer_, ' Linea:', linea, ' Columna:', columna
    
    if (current_char == '%') then
        call addToken(tokens, buffer_, "PORCENTAJE", linea, columna)
        call clearBuffer(buffer_)
    else if (current_char >= '0' .and. current_char <= '9') then
        call addtoBuffer(current_char, buffer_, columna)
    else
        call addToken(tokens, buffer_, "NUMERO_ENTERO", linea, columna)
        if (current_char== ';') then
            buffer_=''
            estado = 0
            call state0(current_char, buffer_, tokens, errors, linea, columna, estado, .false.)
        endif
    end if
end subroutine stateNumero

subroutine stateCadena(current_char, buffer_, tokens, errors, linea, columna, estado)
    implicit none
    character(len=1), intent(in) :: current_char
    character(len=100), intent(inout) :: buffer_
    type(Token), allocatable, intent(inout) :: tokens(:)
    type(Error), allocatable, intent(inout) :: errors(:)
    integer, intent(inout) :: linea, columna, estado
    logical :: cadenaAbierta
    print *, 'Estado cadena: ', current_char, ' Buffer: ', buffer_, 'Linea:', linea, 'Columna:', columna
    if (current_char == '"') then
        if (len_trim(buffer_) == 0) then
            cadenaAbierta = .true.
            
        else

            call addtoBuffer(current_char, buffer_, columna)  
            call addToken(tokens, buffer_, "CADENA", linea, columna)  
            call clearBuffer(buffer_)  
            call iraState(estado, 0, buffer_) 
        end if
    else
        
        call addtoBuffer(current_char, buffer_, columna)
    end if
end subroutine stateCadena

subroutine processBuffer(buffer_, errors, tokens, linea, columna)
    implicit none
    character(len=100), intent(inout) :: buffer_
    type(Token), allocatable, intent(inout) :: tokens(:)
    type(Error), allocatable, intent(inout) :: errors(:)
    integer, intent(in) :: linea, columna
    character(len=100) :: tokenType
    tokenType = isReservedWord(buffer_)
    
    if (tokenType == "ERROR") then
        print *, "Nuevo Error"
        call addError(errors, "error Sintaxix", buffer_, linea, columna)
    else
        print *, "Nuevo Token"
        call addToken(tokens, buffer_, tokenType, linea, columna)
    end if

    call clearBuffer(buffer_)
end subroutine processBuffer


end module lexer
