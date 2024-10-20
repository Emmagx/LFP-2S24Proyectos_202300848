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
                ! Abrir el archivo y leer su contenido completo
                call leerArchivo("EntradaEjemplo.LFP", cadenaEntrada, ios)
                if (ios /= 0) then
                    print *, "Error al leer el archivo, ios = ", ios
                else
                    print *, "Archivo leido correctamente. Cadena de entrada: ", trim(cadenaEntrada)
                    ! Creamos un analizador léxico, le pedimos que analice el texto y que nos dé 
                    ! la lista de tokens resultante del análisis
                    print *, "Iniciando analisis lexico"
                    call escanear(cadenaEntrada, listaTokens)
                    print *, "Analisis lexico completado. Lista de tokens generada."

                    ! Imprimir la lista de tokens
                    print *, "Lista de Tokens generada:"
                    call imprimirListaTokens(listaTokens)

                    ! Iniciar el análisis sintáctico (comentado hasta que actives el analizador sintáctico)
                    print *, "Iniciando analisis sintactico"
                    !call parsear(listaTokens)
                    print *, "Analisis sintactico completado"
                end if
            case (2)
                print *, "Opcion 2 seleccionada: Saliendo del programa..."
                exit
            case default
                print *, "Opcion no válida. Por favor, Intente de nuevo."
        end select
    end do
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

