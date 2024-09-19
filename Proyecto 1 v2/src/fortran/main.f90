program main
    use TokenModule
    use ErrorModule
    use lexer
    use utils
    implicit none

    character(len=1000) :: line  
    character(len=100) :: buffer_
    character(len=1) :: current_char
    type(Token), allocatable :: tokens(:)
    type(Error), allocatable :: errors(:)
    integer :: linea, columna, estado
    integer :: len_line, unit_number, iostat_code
    logical :: salto_linea
    integer :: j, temp_j 
    character(len=100) :: filename

    filename = "entradaEjemplo.org"

    linea = 1
    columna = 1
    estado = 0
    buffer_ = ""
    salto_linea = .false.
    unit_number = 20
    open(unit=unit_number, file=trim(filename), status='unknown', action='read', iostat=iostat_code)

    do
        read(unit_number, '(A)', iostat=iostat_code) line
        if (iostat_code /= 0) exit 
        
        len_line = len_trim(line)  

        do j = 1, len_line
            current_char = line(j:j) 
            temp_j = j 

            if (current_char == new_line('A')) then
                salto_linea = .true.
            else
                salto_linea = .false.
            end if
            if (current_char >= '0' .and. current_char <= '9' .or. index(buffer_,'%')>0 )then
                estado = 8

            endif
            select case (estado)
                case (0)
                    call state0(current_char, buffer_, tokens, errors, linea, columna, estado, salto_linea)
                    
                case (1)
                    call state1(current_char, buffer_, tokens, errors, linea, columna, estado, temp_j)

                case (8)
                    call stateNumero(current_char, buffer_, tokens, errors, linea, columna, estado, temp_j)

                case (2)
                    call stateCadena(current_char, buffer_, tokens, errors, linea, columna, estado)
                    
            end select
            
        end do

        linea = linea + 1
        columna = 1
    end do

    ! Cerrar el archivo
    close(unit_number)
        if (size(errors) > 0) then
            call generarHTMLErrores(errors)
            print *, 'Analisis lexico con errores. Generando reportes...'
        else
            call generarHTMLTokens(tokens)
        end if
        call generarHTMLTokens(tokens)
        print *, 'Analisis lexico completado. Generando reportes..'
end program main
