program main_program
    use lexer
    use TokenModule
    use ErrorModule
    use utils
    implicit none
    type(Token), allocatable :: tokens(:)
    type(Error), allocatable :: errors(:)

    ! Inicialización de los arreglos de tokens y errores
    allocate(tokens(0))
    allocate(errors(0))
    
    ! Análisis léxico del archivo
    call analizar_archivo("Entrada3.ORG", tokens, errors)

    ! Imprimir los tokens encontrados
    call imprimir_tokens(tokens)

    ! Imprimir los errores léxicos encontrados, si los hay
    if (size(errors) > 0) then
        call imprimir_errores(errors)
    else
        print *, "Analisis lexico completado sin errores."
    end if

end program main_program
