program analizador 
    use TokenModule
    use mod_analizador_lexico
    !use mod_analizador_sintactico
    use mod_utilidades
    implicit none

    integer :: opcion
    character(len=350) :: cadenaEntrada
    integer :: ios
    type(token), dimension(:), allocatable :: listaTokens
    character(len=1000) :: linea
    logical :: finArchivo
    integer :: unidadArchivo

    ! Abrir el archivo
    unidadArchivo = 10 ! Usar una unidad de archivo que no esté en uso
    open(unit=unidadArchivo, file="EntradaEjemplo.LFP", status="old", action="read", iostat=ios)
    if (ios /= 0) then
        print *, "Error al abrir el archivo, ios = ", ios
    end if

    ! Inicializar la lista de tokens
    allocate(listaTokens(0))

    ! Ciclo del menú
    do
        print *, "Mostrando menu principal"
        ! Mostrar las opciones del menú
        call show_menu()

        ! Leer la opción ingresada por el usuario
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
                        print *, "Analizando linea: ", trim(linea)
                        call escanear(linea, listaTokens)
                    end if
                end do

                ! Imprimir la lista de tokens
                print *, "Analisis lexico completado. Lista de tokens generada:"
                call imprimirListaTokens(listaTokens)

                ! Iniciar el análisis sintáctico (comentado hasta que actives el analizador sintáctico)
                print *, "Iniciando analisis sintactico"
                !call parsear(listaTokens)
                print *, "Analisis sintactico completado"
                
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
