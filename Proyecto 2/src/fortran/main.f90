program analizador 
    use TokenModule
    use mod_analizador_lexico
    use mod_analizador_sintactico
    use mod_utilidades
    implicit none

    integer :: opcion
    integer :: ios
    type(token), dimension(:), allocatable :: listaTokens
    character(len=1000) :: linea
    logical :: finArchivo, enComentario
    integer :: unidadArchivo

    ! Abrir el archivo
    unidadArchivo = 10 ! Usar una unidad de archivo que no esté en uso
    open(unit=unidadArchivo, file="EntradaEjemplo.LFP", status="old", action="read", iostat=ios)
    if (ios /= 0) then
        print *, "Error al abrir el archivo, ios = ", ios
        stop
    end if

    ! Inicializar la lista de tokens y la bandera
    allocate(listaTokens(0))
    enComentario = .false.

    ! Ciclo del menú
    do
        print *, "Mostrando menu principal"
        call show_menu()

        print *, "Esperando entrada del usuario para seleccionar opcion..."
        read (*, *) opcion
        print *, "Opcion ingresada: ", opcion

        select case (opcion)
            case (1)
                print *, "Opcion 1 seleccionada: Analizar archivo"
                
                ! Leer el archivo línea por línea
                finArchivo = .false.
                do while (.not. finArchivo)
                    read(unit=unidadArchivo, fmt='(A)', iostat=ios) linea
                    if (ios /= 0) then
                        finArchivo = .true.
                    else
                        print *, "Analizando línea: ", trim(linea)
                        call escanear(linea, listaTokens, enComentario)
                    end if
                end do

                ! Imprimir la lista de tokens
                print *, "Análisis léxico completado. Lista de tokens generada:"
                call imprimirListaTokens(listaTokens)

                ! Iniciar el análisis sintáctico
                print *, "Iniciando análisis sintáctico"
                call parsear(listaTokens)
                print *, "Análisis sintáctico completado"
                
            case (2)
                print *, "Opcion 2 seleccionada: Saliendo del programa..."
                exit
            case default
                print *, "Opcion no válida. Por favor, Intente de nuevo."
        end select
    end do

    ! Cerrar el archivo
    close(unidadArchivo)
end program analizador

! Subrutina para mostrar el menú
subroutine show_menu()
    print *, "==========================="
    print *, "       Menu Principal       "
    print *, "==========================="
    print *, "1. Analizar"
    print *, "2. Salir"
    print *, "Seleccione una opcion:"
end subroutine show_menu
