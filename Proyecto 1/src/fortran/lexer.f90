module lexer
    use TokenModule
    use ErrorModule
    use states
    implicit none

contains

    subroutine analizar_archivo(nombre_archivo, tokens, errors)
        character(len=*), intent(in) :: nombre_archivo
        type(Token), allocatable :: tokens(:)
        type(Error), allocatable :: errors(:)
        character(len=256) :: line
        integer :: i, ios, linea, columna, estado, line_length
        character(len=100) :: buffer_
        logical :: fin_de_linea
        ! Verificar si ya están asignados, si es así, desasignarlos
        if (allocated(tokens)) deallocate(tokens)
        if (allocated(errors)) deallocate(errors)

        ! Inicializar los arreglos
        allocate(tokens(0))
        allocate(errors(0))
        estado = 0
        buffer_ = ""
        linea = 1
        columna = 1
        ios = 0
        line_length = 0

        ! Abrir el archivo
        open(unit=10, file=nombre_archivo, status="old", action="read", iostat=ios)
        if (ios /= 0) then
            print *, "Error al abrir el archivo:", nombre_archivo
            return
        end if

        ! Leer el archivo línea por línea
        do
            read(10, '(A)', iostat=ios) line
            if (ios /= 0) exit
            line_length = len_trim(line)
            i = 1

            ! Procesar cada carácter de la línea
            do while (i <= line_length)
                fin_de_linea = (i == line_length)
                select case (estado)
                    case (0)
                        call state0(line(i:i), buffer_, tokens, errors, linea, columna, estado, fin_de_linea)
                    case (1)
                        call state1(line(i:i), buffer_, tokens, errors, linea, columna, estado, i)
                    case (2)
                        call state2(line(i:i), buffer_, tokens, errors, linea, columna, estado, i)
                    case (3)
                        call state3(line(i:i), buffer_, tokens, errors, linea, columna, estado, i) ! Estado 3: números negativos o guiones
                    case (8)
                        call state8(line(i:i), buffer_, tokens, errors, linea, columna, estado, i) ! Estado 8: números y porcentajes
                end select
                i = i + 1
            end do

            linea = linea
        end do

        ! Cerrar el archivo
        close(10)
    end subroutine analizar_archivo

    ! Subrutina para imprimir los tokens en formato de tabla
    subroutine imprimir_tokens(tokens)
        type(Token), allocatable, intent(in) :: tokens(:)
        integer :: i
    
        ! Imprimir encabezado de la tabla
        print *, "---------------------------------------------------------"
        print *, "|   Valor del Token   |       Tipo       | Linea | Columna |"
        print *, "---------------------------------------------------------"
    
        ! Imprimir cada token
        do i = 1, size(tokens)
            print '(A20, A20, I8, I8)', '| ', adjustl(tokens(i)%valor), ' | ', adjustl(tokens(i)%tipo), &
            ' | ', tokens(i)%linea, ' | ', tokens(i)%columna, ' |'
        end do
        print *, "---------------------------------------------------------"
    end subroutine imprimir_tokens

    ! Subrutina para imprimir los error en formato de tabla
    subroutine imprimir_errores(errors)
        type(Error), allocatable, intent(in) :: errors(:)
        integer :: i

        ! Imprimir encabezado de la tabla de errores
        print *, "----------------------------------------------"
        print *, "| Descripción del Error   | Tipo    | Linea | Columna |"
        print *, "----------------------------------------------"

        ! Imprimir cada error
        do i = 1, size(errors)
            print '(A20, A20, I8, I8)', '| ', adjustl(errors(i)%mensaje), & 
                   ' | ', adjustl(errors(i)%tipo), ' | ', errors(i)%linea, ' | ', errors(i)%columna, ' |'
        end do
        print *, "----------------------------------------------"
    end subroutine imprimir_errores

end module lexer
