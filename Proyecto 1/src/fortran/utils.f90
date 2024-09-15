module utils
    implicit none
    contains
    function isReservedWord(buffer) result(tokenType)
        implicit none
        character(len=*), intent(inout) :: buffer
        character(len=100) :: tokenType

        tokenType = ""

        select case (trim(buffer))
            case ("grafica")
                tokenType = "GRAFICA"
            case ("continente")
                tokenType = "CONTINENTE"
            case ("pais")
                tokenType = "PAIS"
            case ("Pais")
                tokenType = "PAIS"
            case default
                tokenType = "IDENTIFICADOR"
        end select

    end function isReservedWord
    
        subroutine iraState(estado, nuevo_estado)
            integer, intent(inout) :: estado
            integer, intent(in) :: nuevo_estado
            estado = nuevo_estado
        end subroutine iraState
    
        subroutine addtoBuffer(current_char, buffer_, columna)
            character(len=1), intent(in) :: current_char
            character(len=100), intent(inout) :: buffer_
            integer, intent(inout) :: columna
    
            buffer_ = trim(buffer_) // current_char
            columna = columna + 1
        end subroutine addtoBuffer
    
        subroutine clearBuffer(buffer_)
            character(len=100), intent(inout) :: buffer_
            buffer_ = ""
        end subroutine clearBuffer
    
        subroutine goBack(i, columna, buffer_)
            integer, intent(inout) :: i, columna
            character(len=100), intent(inout) :: buffer_
    
           
            i = i - 1
            columna = columna - 1
            buffer_ = adjustl(buffer_(:len(buffer_) - 1))
        end subroutine goBack
    
        subroutine newLine(linea, columna)
            integer, intent(inout) :: linea, columna
            linea = linea + 1
            columna = 1
        end subroutine newLine




end module utils
