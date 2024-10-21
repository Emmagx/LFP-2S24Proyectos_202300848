module mod_utilidades
    implicit none
    private
    public :: leerArchivo, generarHTMLTokens, generarHTMLErrores, getTipoTokenEnString

contains
    subroutine leerArchivo(nombreArchivo, cadena, ios)
        character(len=*), intent(in) :: nombreArchivo
        character(len=*), intent(out) :: cadena
        integer, intent(out) :: ios
        integer :: iunit
        character(len=100) :: temp_line

        iunit = 10
        cadena = ''
        
        print *, "Intentando abrir el archivo: ", trim(nombreArchivo)

        open(unit=iunit, file=trim(nombreArchivo), status='old', iostat=ios)
        if (ios /= 0) then
            print *, "Error al abrir el archivo con iostat = ", ios
            cadena = ''
            return
        else
            print *, "Archivo abierto correctamente."
        end if

        ! Leer el archivo línea por línea
        do
            read(iunit, '(A)', iostat=ios) temp_line
            if (ios /= 0) exit
            cadena = trim(cadena) // trim(temp_line) // char(10)
        end do
        
        ios = 0
        close(iunit)
    end subroutine leerArchivo

    subroutine generarHTMLTokens(tokens)
        use TokenModule
        implicit none
        type(Token), allocatable, intent(in) :: tokens(:)
        integer :: i, unit_number, ios
        character(len=100) :: linea_str

        unit_number = 15
        open(unit=unit_number, file='reporte_tokens.html', status='unknown', action='write', iostat=ios)
        if (ios /= 0) then
            print *, 'Error al abrir reporte_tokens.html'
            return
        end if

        ! Escribir la estructura HTML
        write(unit_number, '(A)') '<!DOCTYPE html>'
        write(unit_number, '(A)') '<html lang="es">'
        write(unit_number, '(A)') '<head><meta charset="UTF-8"><title>Reporte de Tokens</title></head>'
        write(unit_number, '(A)') '<body>'
        write(unit_number, '(A)') '<h1>Reporte de Tokens</h1>'
        write(unit_number, '(A)') '<table border="1">'
        write(unit_number, '(A)') '<tr><th>Lexema</th><th>Tipo</th><th>Linea</th></tr>'
        
        ! Escribir cada token en una fila de la tabla
        do i = 1, size(tokens)
            if (trim(tokens(i)%valor) /= '') then
                write(linea_str, '(I5)') tokens(i)%linea
                write(unit_number, '(A, A, A, A)') & 
                    '<tr><td>',trim(tokens(i)%valor), '</td><td>', trim(getTipoTokenEnString(tokens(i)%tipo)), &
                    '</td><td>', trim(linea_str), '</td></tr>'
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
        integer :: i, unit_number, ios
        character(len=100) :: linea_str

        unit_number = 13
        open(unit=unit_number, file='reporte_errores.html', status='unknown', action='write', iostat=ios)
        if (ios /= 0) then
            print *, 'Error al abrir reporte_errores.html'
            return
        end if

        ! Escribir la estructura HTML
        write(unit_number, '(A)') '<!DOCTYPE html>'
        write(unit_number, '(A)') '<html lang="es">'
        write(unit_number, '(A)') '<head><meta charset="UTF-8"><title>Reporte de Errores Lexicos</title></head>'
        write(unit_number, '(A)') '<body>'
        write(unit_number, '(A)') '<h1>Reporte de Errores Lexicos</h1>'
        write(unit_number, '(A)') '<table border="1">'
        write(unit_number, '(A)') '<tr><th>Descripcion</th><th>Tipo</th><th>Linea</th></tr>'

        ! Escribir cada error en una fila de la tabla
        do i = 1, size(errors)
            write(linea_str, '(I5)') errors(i)%linea
            write(unit_number, '(A, A, A, A)') & 
                '<tr><td>', trim(errors(i)%descripcion), '</td><td>', trim(errors(i)%tipo), &
                '</td><td>', trim(linea_str), '</td></tr>'
        end do

        write(unit_number, '(A)') '</table>'
        write(unit_number, '(A)') '</body>'
        write(unit_number, '(A)') '</html>'
        close(unit_number)
        
        print *, 'Reporte de errores generado en reporte_errores.html'
    end subroutine generarHTMLErrores

    function getTipoTokenEnString(p) result(res)
        use TokenModule
        integer, intent(in) :: p
        character(len=30) :: res
    
        select case (p)
        case (RESERVADA_CONTROLES)
            res = "RESERVADA_CONTROLES"
        case (RESERVADA_ETIQUETA)
            res = "RESERVADA_ETIQUETA"
        case (RESERVADA_BOTON)
            res = "RESERVADA_BOTON"
        case (RESERVADA_CHECK)
            res = "RESERVADA_CHECK"
        case (RESERVADA_RADIOBOTON)
            res = "RESERVADA_RADIOBOTON"
        case (RESERVADA_TEXTO)
            res = "RESERVADA_TEXTO"
        case (RESERVADA_AREATEXTO)
            res = "RESERVADA_AREATEXTO"
        case (RESERVADA_CLAVE)
            res = "RESERVADA_CLAVE"
        case (RESERVADA_CONTENEDOR)
            res = "RESERVADA_CONTENEDOR"
        case (IDENTIFICADOR)
            res = "IDENTIFICADOR"
        case (SIGNO_MENOR_QUE)
            res = "SIGNO_MENOR_QUE"
        case (SIGNO_MAYOR_QUE)
            res = "SIGNO_MAYOR_QUE"
        case (SIGNO_ADMIRACION_C)
            res = "SIGNO_ADMIRACION_C"
        case (SIGNO_PUNTO_Y_COMA)
            res = "SIGNO_PUNTO_Y_COMA"
        case (SIGNO_GUION)
            res = "SIGNO_GUION"
        case (COMENTARIO_LINEA)
            res = "COMENTARIO_LINEA"
        case (SIGNO_PUNTO)
            res = "SIGNO_PUNTO"
        case (SIGNO_PARENTESIS_APERTURA)
            res = "SIGNO_PARENTESIS_APERTURA"
        case (SIGNO_PARENTESIS_CERRADURA)
            res = "SIGNO_PARENTESIS_CERRADURA"
        case (VALOR_CADENA)
            res = "VALOR_CADENA"
        case (VALOR_NUMERICO)
            res = "VALOR_NUMERICO"
        case (COMENTARIO_MULTILINEA)
            res = "COMENTARIO_MULTILINEA"
        case (COMA)
            res = "COMA"
        case (setTexto)
            res = "setTexto"
        case (setAncho)
            res = "setAncho"
        case (setColorFondo)
            res = "setColorFondo"
        case (setPosicion)
            res = "setPosicion"
        case (add)
            res = "add"
        case (setMarcada)
            res = "setMarcada"
        case (setColorLetra)
            res = "setColorLetra"
        case (RESERVADA_PROPIEDADES)
            res = "RESERVADA_PROPIEDADES"
        case (RESERVADA_COLOCACION)
            res = 'RESERVADA_COLOCACION'
        case (setAlto)
            res = 'setAlto'
        case (THIS)
            res = 'THIS'
        case default
            res = "Desconocido"
        end select
    end function getTipoTokenEnString
end module mod_utilidades