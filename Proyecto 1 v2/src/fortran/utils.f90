module utils
    use ErrorModule
    implicit none
    
contains

    subroutine addtoBuffer(current_char, buffer_, columna)
        implicit none
        character(len=1), intent(in) :: current_char
        character(len=100), intent(inout) :: buffer_
        integer, intent(inout) :: columna

        buffer_ = trim(buffer_) // current_char
        columna = columna + 1
    end subroutine addtoBuffer

    subroutine clearBuffer(buffer_)
        implicit none
        character(len=100), intent(inout) :: buffer_

        buffer_ = ""
    end subroutine clearBuffer


    character(len=100) function isReservedWord(buffer_)
    implicit none
    character(len=100), intent(in) :: buffer_
    print *, "Buffer: ", buffer_
    select case (trim(buffer_))
    case ("pais", "Pais", "PAIS")
        !isReservedWord = "pais"
        isReservedWord = "PALABRA_RESERVADA"
    case ("continente", "Continente", "CONTINENTE")
        !isReservedWord = "continente"
        isReservedWord = "PALABRA_RESERVADA"
    case ("grafica", "Grafica", "GRAFICA")
        !isReservedWord = "grafica"
        isReservedWord = "PALABRA_RESERVADA"
    case ("nombre", "Nombre", "NOMBRE")
        isReservedWord = "PALABRA_RESERVADA"
        !isReservedWord = "nombre"
    case ("poblacion", "Poblacion", "POBLACION")
        isReservedWord = "PALABRA_RESERVADA"
        !isReservedWord = "poblacion"
    case ("saturacion", "Saturacion", "SATURACION")
        isReservedWord = "PALABRA_RESERVADA"
        !isReservedWord = "saturacion"
    case ("bandera", "Bandera", "BANDERA")
        isReservedWord = "PALABRA_RESERVADA"
        !isReservedWord = "bandera"
    case ("\t")
        isReservedWord = "TAB"    
    case ("\n")
        isReservedWord = "Salto de linea"
    case ("")
        isReservedWord = "ESPACIO"
    case default
        isReservedWord = "ERROR"
    end select
end function isReservedWord

    subroutine goBack(i, columna, buffer_)
        implicit none
        integer, intent(inout) :: i, columna
        character(len=100), intent(inout) :: buffer_

        if (i > 1) then
            i = i - 1
            buffer_ = adjustl(buffer_(1:i))
            columna = columna - 1
        end if
    end subroutine goBack

    subroutine iraState(estado, nuevo_estado, buffer_)
        implicit none
        integer, intent(inout) :: estado
        integer, intent(in) :: nuevo_estado
        character(len=100), intent(in) :: buffer_

        estado = nuevo_estado
    end subroutine iraState

    subroutine generarHTMLTokens(tokens)
        use TokenModule
        implicit none
        type(Token), allocatable, intent(in) :: tokens(:)
        integer :: i, unit_number
        integer :: max_len_desc, max_len_tipo
        character(len=100) :: linea_str, columna_str
        max_len_desc = 0
        max_len_tipo = 0
        unit_number = 15
        open(unit=unit_number, file='reporte_tokens.html', status='unknown', action='write', iostat=i)
        if (i /= 0) then
            print *, 'Error al abrir el archivo reporte_tokens.html'
            stop
        end if
        write(unit_number, '(A)') '<!DOCTYPE html>'
        write(unit_number, '(A)') '<html lang="es">'
        write(unit_number, '(A)') '<head><meta charset="UTF-8"><title>Reporte de Tokens</title></head>'
        write(unit_number, '(A)') '<body>'
        write(unit_number, '(A)') '<h1>Reporte de Tokens</h1>'
        write(unit_number, '(A)') '<table border="1">'
        write(unit_number, '(A)') '<tr><th>Lexema</th><th>Tipo</th><th>Linea</th><th>Columna</th></tr>'
        do i = 1, size(tokens)
            if (trim(tokens(i)%descripcion) /= '') then
                write(linea_str, '(I5)') tokens(i)%linea
                write(columna_str, '(I5)') tokens(i)%columna
                write(unit_number, '(A, A, A, A)') &
                    '<tr><td>', trim(tokens(i)%descripcion), '</td><td>', trim(tokens(i)%tipo), &
                    '</td><td>', trim(linea_str), '</td><td>', trim(columna_str), '</td></tr>'
            end if
        end do
        write(unit_number, '(A)') '</table>'
        write(unit_number, '(A)') '</body>'
        write(unit_number, '(A)') '</html>'
        close(unit_number)
        
        print *, 'Reporte de tokens generado en reporte_tokens.html'
    end subroutine generarHTMLTokens
    
    subroutine generarHTMLErrores(errors)
        use ErrorModule
        implicit none
        type(Error), allocatable, intent(in) :: errors(:)
        integer :: i, unit_number
        character(len=100) :: linea_str, columna_str
        unit_number = 13
        open(unit=unit_number, file='reporte_errores.html', status='unknown', action='write', iostat=i)
        if (i /= 0) then
            print *, 'Error al abrir el archivo reporte_errores.html'
            stop
        end if
    
        ! Escribir la estructura HTML
        write(unit_number, '(A)') '<!DOCTYPE html>'
        write(unit_number, '(A)') '<html lang="es">'
        write(unit_number, '(A)') '<head><meta charset="UTF-8"><title>Reporte de Errores Lexicos</title></head>'
        write(unit_number, '(A)') '<body>'
        write(unit_number, '(A)') '<h1>Reporte de Errores Lexicos</h1>'
        write(unit_number, '(A)') '<table border="1">'
        write(unit_number, '(A)') '<tr><th>Mensaje</th><th>Tipo</th><th>Linea</th><th>Columna</th></tr>'
        
        ! Recorrer los errores y escribir cada uno en una fila de la tabla
        do i = 1, size(errors)
            if (trim(errors(i)%mensaje) /= ' ' .or. trim(errors(i)%tipo) /=' ') then
                write(linea_str, '(I5)') errors(i)%linea
                write(columna_str, '(I5)') errors(i)%columna
                write(unit_number, '(A, A, A, A)') &
                    '<tr><td>', trim(errors(i)%mensaje), '</td><td>', trim(errors(i)%tipo), &
                    '</td><td>', trim(linea_str), '</td><td>', trim(columna_str), '</td></tr>'
            end if
        end do
    
        ! Cerrar la tabla y la estructura HTML
        write(unit_number, '(A)') '</table>'
        write(unit_number, '(A)') '</body>'
        write(unit_number, '(A)') '</html>'
    
        ! Cerrar el archivo
        close(unit_number)
    
        print *, 'Reporte de errores generado en reporte_errores.html'
    end subroutine generarHTMLErrores
    
    

end module utils
