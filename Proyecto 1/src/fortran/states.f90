module states
    use TokenModule
    use ErrorModule
    use utils
    implicit none

contains

    ! Estado 0: Inicial y de detección general
subroutine state0(current_char, buffer_, tokens, errors, linea, columna, estado, salto_linea)
    implicit none 
    character(len=1), intent(inout) :: current_char
    character(len=100), intent(inout) :: buffer_
    type(Token), allocatable, intent(inout) :: tokens(:)
    type(Error), allocatable, intent(inout) :: errors(:)
    integer, intent(inout) :: linea, columna, estado
    logical, intent(in) :: salto_linea


    ! Ignorar espacios y tabulaciones
    if (current_char == ' ' .or. current_char == '\t') then
        columna = columna + 1
        return
    end if

    ! Añadir el carácter al buffer si es relevante
    call addtoBuffer(current_char, buffer_, columna)

    ! Identificar tokens con delimitadores conocidos
    select case (current_char)
        case ('=')
            call addToken(tokens, buffer_, "IGUAL", linea, columna)
        case ('(')
            call addToken(tokens, buffer_, "PARENTESIS_IZQUIERDO", linea, columna)
        case (')')
            call addToken(tokens, buffer_, "PARENTESIS_DERECHO", linea, columna)
        case ('{')
            call addToken(tokens, buffer_, "LLAVE_IZQUIERDA", linea, columna)
        case ('}')
            call addToken(tokens, buffer_, "LLAVE_DERECHA", linea, columna)
        case (':')
            call addToken(tokens, buffer_, "DOS_PUNTOS", linea, columna)
        case (';')
            call addToken(tokens, buffer_, "PUNTO_Y_COMA", linea, columna)
        case ('"')
            call iraState(estado, 2) 
        case ('0':'9')
            call iraState(estado, 8)
        case ('a':'z', 'A':'Z')
            call iraState(estado, 1)
        case ('/')
            call iraState(estado, 5)
        case ('-')
            call iraState(estado, 3)
        case default
            print *, "Error: caracter no reconocido: ", current_char
            call addError(errors, "Caracter no reconocido", buffer_, "LEXICO", linea, columna)
    end select

    ! Si es un salto de línea, reiniciamos el buffer
    if (salto_linea) then 
        call newLine(linea, columna)
        call clearBuffer(buffer_)
    end if
end subroutine state0

    ! Estado 1: Maneja palabras reservadas y nombres
    subroutine state1(current_char, buffer_, tokens, errors, linea, columna, estado, i)
        implicit none 
        character(len=1), intent(inout) :: current_char
        character(len=100), intent(inout) :: buffer_
        type(Token), allocatable, intent(inout) :: tokens(:)
        type(Error), allocatable, intent(inout) :: errors(:)
        integer, intent(inout) :: linea, columna, estado, i
        character(len=100) :: tokenType

        ! Añadir el carácter al buffer si es alfabético
        if ((current_char >= 'a' .and. current_char <= 'z') .or. (current_char >= 'A' .and. current_char <= 'Z')) then
            call addtoBuffer(current_char, buffer_, columna)
        else
            ! Retrocedemos un carácter si no es alfanumérico
            call goBack(i, columna, buffer_)

            ! Verificamos si es una palabra reservada
            tokenType = isReservedWord(buffer_)
        if (len_trim(tokenType) == 0) then
            tokenType = "IDENTIFICADOR"
        end if

        call addToken(tokens, buffer_, tokenType, linea, columna)
        call clearBuffer(buffer_)  ! Limpia el buffer tras añadir el token

            ! Regresamos al estado 0
            call iraState(estado, 0)
        end if
    end subroutine state1

    ! Estado 2: Maneja cadenas de texto encerradas en comillas
    subroutine state2(current_char, buffer_, tokens, errors, linea, columna, estado, i)
        implicit none
        character(len=1), intent(in) :: current_char
        character(len=100), intent(inout) :: buffer_
        type(Token), allocatable, intent(inout) :: tokens(:)
        type(Error), allocatable, intent(inout) :: errors(:)
        integer, intent(inout) :: linea, columna, estado, i

        ! Si encontramos otra comilla, se termina la cadena
        if (current_char == '"') then
            call addToken(tokens, buffer_, "CADENA", linea, columna)
            call iraState(estado, 0)
        else
            ! Agregamos caracteres a la cadena
            call addtoBuffer(current_char, buffer_, columna)
        end if
    end subroutine state2

    ! Estado 3: Manejo de números negativos o guiones
    subroutine state3(current_char, buffer_, tokens, errors, linea, columna, estado, i)
        implicit none
        character(len=1), intent(in) :: current_char
        character(len=100), intent(inout) :: buffer_
        type(Token), allocatable, intent(inout) :: tokens(:)
        type(Error), allocatable, intent(inout) :: errors(:)
        integer, intent(inout) :: linea, columna, estado, i

        ! Verificar si después del guion viene un número
        if (current_char >= '0' .and. current_char <= '9') then
            ! Es un número negativo
            call addtoBuffer('-', buffer_, columna)
            call iraState(estado, 8)  ! Cambiamos al estado 8 para procesar el número
        else
            ! Si no es un número, puede ser un guion independiente o error léxico
            call addError(errors, "Caracter no esperado después del guion", buffer_, "LEXICO", linea, columna)
            call iraState(estado, 0)
        end if
    end subroutine state3

    ! Estado 8: Manejo de números y porcentajes
    subroutine state8(current_char, buffer_, tokens, errors, linea, columna, estado, i)
        implicit none
        character(len=1), intent(in) :: current_char
        character(len=100), intent(inout) :: buffer_
        type(Token), allocatable, intent(inout) :: tokens(:)
        type(Error), allocatable, intent(inout) :: errors(:)
        integer, intent(inout) :: linea, columna, estado, i
        character(len=100) :: tempBuffer

        ! Verificar si el carácter es un número
        if (current_char >= '0' .and. current_char <= '9') then
            call addtoBuffer(current_char, buffer_, columna)
        elseif (current_char == '%') then
            ! Si encontramos un porcentaje, lo añadimos
            call addToken(tokens, buffer_, "NUMERO_PORCENTAJE", linea, columna)
            tempBuffer = "%" ! Crear variable temporal
            call addToken(tokens, tempBuffer, "PORCENTAJE", linea, columna)
            call iraState(estado, 0)
        else
            ! Si no es un número ni un porcentaje, cerramos el número
            call addToken(tokens, buffer_, "NUMERO", linea, columna)
            call iraState(estado, 0)
            ! Retrocedemos un carácter para que el próximo estado lo procese
            call goBack(i, columna, buffer_)
        end if
    end subroutine state8

end module states
