program main
    use TokenModule
    use ErrorModule
    use lexer
    use utils
    implicit none

    character(len=1000) :: line  ! Variable para almacenar una línea completa
    character(len=100) :: buffer_
    character(len=1) :: current_char
    type(Token), allocatable :: tokens(:)
    type(Error), allocatable :: errors(:)
    integer :: linea, columna, estado
    integer :: len_line, unit_number, iostat_code
    logical :: salto_linea
    integer :: j, temp_j  ! Nueva variable temporal para j
    character(len=100) :: filename

    ! Nombre del archivo de entrada
    filename = "entradaEjemplo.org"

    ! Inicializa variables
    linea = 1
    columna = 1
    estado = 0
    buffer_ = ""
    salto_linea = .false.

    ! Asignamos un número de unidad válido
    unit_number = 20

    ! Abrir el archivo .ORG
    open(unit=unit_number, file=trim(filename), status='unknown', action='read', iostat=iostat_code)
    ! if (iostat_code /= 0) then
    !     print *, 'Error al abrir el archivo:', filename
    !     stop
    ! end if

    ! Leer el archivo línea por línea
    do
        read(unit_number, '(A)', iostat=iostat_code) line
        if (iostat_code /= 0) exit  ! Salir del bucle si ya no hay más líneas
        
        len_line = len_trim(line)  ! Longitud de la línea actual

        ! Procesar cada carácter en la línea
        do j = 1, len_line
            current_char = line(j:j)  ! Extraer el caracter de la línea
            temp_j = j  ! Asignar j a una variable temporal

            ! Verificar si es un salto de línea (aunque aquí ya procesamos la línea completa)
            if (current_char == new_line('A')) then
                salto_linea = .true.
            else
                salto_linea = .false.
            end if
            if (current_char >= '0' .and. current_char <= '9') then
                estado = 8

            endif
            ! Llama al lexer con el estado actual
            select case (estado)
                case (0)
                    call state0(current_char, buffer_, tokens, errors, linea, columna, estado, salto_linea)
                    
                case (1)
                    call state1(current_char, buffer_, tokens, errors, linea, columna, estado, temp_j)
                    
                    !j = temp_j  ! Actualiza j con el valor de temp_j después de state1
                case (8)
                    call stateNumero(current_char, buffer_, tokens, errors, linea, columna, estado, temp_j)
                    
                    !j = temp_j  ! Actualiza j con el valor de temp_j después de stateNumero
                case (2)
                    call stateCadena(current_char, buffer_, tokens, errors, linea, columna, estado)
                    
            end select
            
        end do

        ! Incrementar la línea después de procesar todos los caracteres de la línea
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
