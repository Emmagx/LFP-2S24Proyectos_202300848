module mod_analizador_sintactico
    use TokenModule
    implicit none
    private
    public :: parsear

    ! Variables para el analizador sintáctico
    type(token), dimension(:), allocatable :: tokens
    integer :: pos

contains

    subroutine parsear(listaTokens)
        type(token), dimension(:), intent(in) :: listaTokens
        tokens = listaTokens
        pos = 1

        print *, "Iniciando el analisis sintactico con ", size(tokens), " tokens."

        ! Iniciar la producción S
        call parse_S()

        if (pos <= size(tokens)) then
            print *, "Error sintactico: Tokens inesperados al final. Posicion actual: ", pos
        else
            print *, "Analisis sintactico completado con errores manejados (si los hubo)."
        end if
    end subroutine parsear

    ! Producción S ::= ControlesBlock
    subroutine parse_S()
        print *, "Llamando a parse_S"
        call parse_ControlesBlock()
    end subroutine parse_S

    ! Producción ControlesBlock ::= '<' '!' '-' '-' RESERVADA_CONTROLES ControlList RESERVADA_CONTROLES '-' '-' '>'
    subroutine parse_ControlesBlock()
        print *, "Iniciando parse_ControlesBlock"
        if (.not. consumirToken(SIGNO_MENOR_QUE)) call modoPanico(';')
        if (.not. consumirToken(SIGNO_ADMIRACION_C)) call modoPanico(';')
        if (.not. consumirToken(SIGNO_GUION)) call modoPanico(';')
        if (.not. consumirToken(SIGNO_GUION)) call modoPanico(';')
        if (.not. consumirToken(RESERVADA_CONTROLES)) call modoPanico(';')

        call parse_ControlList()

        if (.not. consumirToken(RESERVADA_CONTROLES)) call modoPanico(';')
        if (.not. consumirToken(SIGNO_GUION)) call modoPanico(';')
        if (.not. consumirToken(SIGNO_GUION)) call modoPanico(';')
        if (.not. consumirToken(SIGNO_MAYOR_QUE)) call modoPanico(';')
    end subroutine parse_ControlesBlock

    ! Producción ControlList ::= Control ControlList | ε
    recursive subroutine parse_ControlList()
        print *, "Iniciando parse_ControlList en la posicion ", pos
        if (esControlValido()) then
            print *, "Se encontro un control valido en la posicion ", pos
            call parse_Control()
            call parse_ControlList()
        else
            print *, "No hay mas controles validos, produccion vacia en la posicion ", pos
        end if
    end subroutine parse_ControlList

    ! Producción Control ::= ControlType IDENTIFICADOR ';' | COMENTARIO_LINEA
    subroutine parse_Control()
        print *, "Iniciando parse_Control en la posicion ", pos
        if (tokens(pos)%tipo == COMENTARIO_LINEA) then
            print *, "Comentario de línea encontrado en la posicion ", pos
            if (consumirToken(COMENTARIO_LINEA)) return
        else
            print *, "Llamando a parse_ControlType"
            call parse_ControlType()
            if (.not. consumirToken(IDENTIFICADOR)) call modoPanico(';')
            if (.not. consumirToken(SIGNO_PUNTO_Y_COMA)) call modoPanico(';')
        end if
    end subroutine parse_Control

    ! Producción ControlType ::= RESERVADA_ETIQUETA | RESERVADA_BOTON | ...
    subroutine parse_ControlType()
        print *, "Iniciando parse_ControlType en la posicion ", pos
        select case (tokens(pos)%tipo)
            case (RESERVADA_ETIQUETA)
                print *, "Reservada etiqueta encontrada"
                if (.not. consumirToken(RESERVADA_ETIQUETA)) call modoPanico(';')
            case (RESERVADA_BOTON)
                print *, "Reservada botón encontrada"
                if (.not. consumirToken(RESERVADA_BOTON)) call modoPanico(';')
            case (RESERVADA_CHECK)
                print *, "Reservada check encontrada"
                if (.not. consumirToken(RESERVADA_CHECK)) call modoPanico(';')
            case (RESERVADA_RADIOBOTON)
                print *, "Reservada radioboton encontrada"
                if (.not. consumirToken(RESERVADA_RADIOBOTON)) call modoPanico(';')
            case (RESERVADA_TEXTO)
                print *, "Reservada texto encontrada"
                if (.not. consumirToken(RESERVADA_TEXTO)) call modoPanico(';')
            case (RESERVADA_AREATEXTO)
                print *, "Reservada área texto encontrada"
                if (.not. consumirToken(RESERVADA_AREATEXTO)) call modoPanico(';')
            case (RESERVADA_CLAVE)
                print *, "Reservada clave encontrada"
                if (.not. consumirToken(RESERVADA_CLAVE)) call modoPanico(';')
            case (RESERVADA_CONTENEDOR)
                print *, "Reservada contenedor encontrada"
                if (.not. consumirToken(RESERVADA_CONTENEDOR)) call modoPanico(';')
            case default
                print *, "Error sintactico: Tipo de control no valido en la posicion ", pos
                call modoPanico(';')
        end select
    end subroutine parse_ControlType

    ! Función para consumir tokens
    logical function consumirToken(tipoEsperado)
        integer, intent(in) :: tipoEsperado
        if (pos > size(tokens)) then
            consumirToken = .false.
            print *, "Error sintactico: Fin de tokens inesperado en la posicion ", pos
            return
        end if
        if (tokens(pos)%tipo == tipoEsperado) then
            print *, "Token ", trim(getTipoTokenEnString(tipoEsperado)), " consumido correctamente en la posicion ", pos
            pos = pos + 1
            consumirToken = .true.
        else
            consumirToken = .false.
            print *, "Error sintactico: Se esperaba el token ", trim(getTipoTokenEnString(tipoEsperado)), &
                     ", pero se encontro ", trim(getTipoTokenEnString(tokens(pos)%tipo)), " en la posicion ", pos
        end if
    end function consumirToken

    ! Función para verificar si el siguiente token es un tipo válido de control
    logical function esControlValido()
        esControlValido = (pos <= size(tokens)) .and. &
                          ((tokens(pos)%tipo == RESERVADA_ETIQUETA) .or. &
                           (tokens(pos)%tipo == RESERVADA_BOTON) .or. &
                           (tokens(pos)%tipo == RESERVADA_CHECK) .or. &
                           (tokens(pos)%tipo == RESERVADA_RADIOBOTON) .or. &
                           (tokens(pos)%tipo == RESERVADA_TEXTO) .or. &
                           (tokens(pos)%tipo == RESERVADA_AREATEXTO) .or. &
                           (tokens(pos)%tipo == RESERVADA_CLAVE) .or. &
                           (tokens(pos)%tipo == RESERVADA_CONTENEDOR) .or. &
                           (tokens(pos)%tipo == COMENTARIO_LINEA))
        print *, "Es control valido: ", esControlValido, " en la posicion ", pos
    end function esControlValido

    ! Procedimiento Modo Pánico: busca el token de sincronización (';')
    subroutine modoPanico(tokenSincronizacion)
        character(len=*), intent(in) :: tokenSincronizacion
        print *, "Recuperacion de error en Modo Panico. Buscando '", tokenSincronizacion, "' en la posicion ", pos

        ! Saltar tokens hasta encontrar el token de sincronización (en este caso ';')
        do while (pos <= size(tokens) .and. trim(getTipoTokenEnString(tokens(pos)%tipo)) /= tokenSincronizacion)
            pos = pos + 1
        end do

        ! Una vez que se encuentra ';', avanzar para continuar el análisis
        if (pos <= size(tokens)) then
            print *, "Recuperacion completada, '", tokenSincronizacion, "' encontrado en la posicion ", pos
            pos = pos + 1
        else
            print *, "Fin de tokens alcanzado durante la recuperacion en Modo Panico."
        end if
    end subroutine modoPanico

    function getTipoTokenEnString(p) result(res)
        integer, intent(in) :: p
        character(len=20) :: res

        res = ""
        
        select case (p)
        case (RESERVADA_CONTROLES)
            res = "RESERVADA_CONTROLES"
        case (RESERVADA_ETIQUETA)
            res = "RESERVADA_ETIQUETA"
        case (RESERVADA_BOTON)
            res = "RESERVADA_BOTON"
        case (RESERVADA_CHECK)
            res = "RESERVADA_CHECK"
        case (RESERVADA_RADIOBOTON)
            res = "RESERVADA_RADIOBOTON"
        case (RESERVADA_TEXTO)
            res = "RESERVADA_TEXTO"
        case (RESERVADA_AREATEXTO)
            res = "RESERVADA_AREATEXTO"
        case (RESERVADA_CLAVE)
            res = "RESERVADA_CLAVE"
        case (RESERVADA_CONTENEDOR)
            res = "RESERVADA_CONTENEDOR"
        case (IDENTIFICADOR)
            res = "IDENTIFICADOR"
        case (SIGNO_MENOR_QUE)
            res = "SIGNO_MENOR_QUE"
        case (SIGNO_MAYOR_QUE)
            res = "SIGNO_MAYOR_QUE"
        case (SIGNO_ADMIRACION_C)
            res = "SIGNO_ADMIRACION_C"
        case (SIGNO_PUNTO_Y_COMA)
            res = ";"
        case (SIGNO_GUION)
            res = "SIGNO_GUION"
        case (COMENTARIO_LINEA)
            res = "COMENTARIO_LINEA"
        case default
            res = "Desconocido"
        end select
    end function getTipoTokenEnString

end module mod_analizador_sintactico
