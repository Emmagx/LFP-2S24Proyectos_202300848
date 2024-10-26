program analizador
    use TokenModule
    use ErrorModule
    use mod_analizador_lexico
    use mod_analizador_sintactico
    use mod_utilidades
    use Generador
    use nary_tree
    implicit none
    integer :: ios, unidadArchivo, i
    type(token), dimension(:), allocatable :: listaTokens
    type(error), dimension(:), allocatable :: errores
    type(Node), allocatable :: arbol  ! Raíz del árbol

    character(len=1000) :: linea
    logical :: finArchivo, enComentario

    i = 0

    ! Abrir el archivo
    unidadArchivo = 10
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

    ! Leer y escanear línea por línea
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

        ! Procesar tokens y construir el árbol de controles
        call procesarTokensToArbol(listaTokens, arbol)
        print *, 'printenando html, antes de llamar generar pagina'
        ! Generar la página HTML con la estructura del árbol
        call generarPagina(arbol, "pagina")
    else
        call generarHTMLErrores(errores)
    end if    

    close(unidadArchivo)

end program analizador
