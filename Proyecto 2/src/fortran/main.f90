program analizador
    use TokenModule
    use ErrorModule
    use mod_analizador_lexico
    use mod_analizador_sintactico
    use mod_utilidades
    use Generador
    use ControlModule
    implicit none
    integer :: ios
    type(token), dimension(:), allocatable :: listaTokens
    type(error), dimension(:), allocatable :: errores
    type(Control), dimension(:), allocatable :: controles
    character(len=1000) :: linea
    logical :: finArchivo, enComentario
    integer :: unidadArchivo
    integer :: i

    i = 0

    ! Abrir el archivo
    unidadArchivo = 10  ! Usar una unidad de archivo que no esté en uso
    open(unit=unidadArchivo, file="EntradaEjemplo.LFP", status="old", action="read", iostat=ios)
    if (ios /= 0) then
        print *, "Error al abrir el archivo, ios = ", ios
        stop
    end if

    ! Inicializar la lista de tokens y la bandera
    allocate(listaTokens(0))
    allocate(errores(0))
    enComentario = .false.
    finArchivo = .false.

    do while (.not. finArchivo)
        read(unit=unidadArchivo, fmt='(A)', iostat=ios) linea
        if (ios /= 0) then
            finArchivo = .true.
        else
            i = i + 1
            print *, "Analizando linea: ", trim(linea)
            call escanear(linea, listaTokens, enComentario, i, errores)
        end if
    end do

    ! Imprimir la lista de tokens
    print *, "Analisis lexico completado. Lista de tokens generada:"
    call imprimirListaTokens(listaTokens)

    ! Iniciar el análisis sintáctico
    print *, "Iniciando analisis sintactico"
    call parsear(listaTokens, errores)
    print *, "Analisis sintactico completado"
    
    ! Imprimir errores y generar reportes
    call printErrors(errores)
    if (size(errores) == 0) then
        call generarHTMLTokens(listaTokens)

        ! Procesar tokens a controles
        call procesarTokensToControles(listaTokens, controles)
    
        ! Llamar a la generación de la página HTML
        call generarPagina(controles, "pagina")
    else
        call generarHTMLErrores(errores)
        
    end if    

    close(unidadArchivo)

end program analizador