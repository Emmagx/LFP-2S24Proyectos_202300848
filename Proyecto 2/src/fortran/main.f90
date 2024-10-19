program analizador
    use TokenModule
    use mod_analizador_lexico
    use mod_analizador_sintactico
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

                    ! Creamos un analizador sintáctico y le pedimos que analice la lista de tokens
                    ! que nos dio el analizador léxico como resultado
                    print *, "Iniciando analisis sintactico"
                    call parsear(listaTokens)
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

! NOTAS:
! Text = "(1+2+(5*(((7)))+2)+"
! En este ejemplo se muestra una expresión incompleta, léxicamente es correcta
! pero sintacticamente no, su estructura es incorrecta porque le hace falta un 
! numero y un paréntesis derecho para estar completa. 

! Cabe mencionar que en muchos casos este analizador sintáctico no funcionará como
! se espera porque no cuenta con un sistema de recuperación de errores, es trabajo 
! del estudiante desarrollar este sistema con alguna de las estrategias de recuperación 
! de errores sintácticos. 

! El fundamento teórico que sirvio de soporte para el desarrollo de este ejemplo es el 
! descrito en la sección 4.4.1 del libro del dragón, segunda edición, que se titula
! Análisis sintáctico de descenso recursivo.