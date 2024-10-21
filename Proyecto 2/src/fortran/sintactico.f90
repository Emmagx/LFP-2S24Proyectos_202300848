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

        call parse_S()

        if (pos <= size(tokens)) then
            print *, "Error sintactico: Tokens inesperados al final. Posicion actual: ", pos
        else
            print *, "Analisis sintactico completado."
        end if
    end subroutine parsear

! Producción S ::= ControlesBlock PropiedadesBlock ColocacionBlock
    subroutine parse_S()
        print *, "Llamando a parse_S"
        
        call parse_ControlesBlock()
        
        ! Parsear el bloque de propiedades solo si el siguiente token es correcto
        if (pos <= size(tokens) .and. tokens(pos)%tipo == SIGNO_MENOR_QUE) then
            call parse_PropiedadesBlock()
        end if
        
        ! Parsear el bloque de colocación solo si el siguiente token es correcto
        if (pos <= size(tokens) .and. tokens(pos)%tipo == SIGNO_MENOR_QUE) then
            call parse_ColocacionBlock()
        end if
    end subroutine parse_S
    

    subroutine parse_PropiedadesBlock()
        print *, "Iniciando parse_PropiedadesBlock"
        
        if (.not. consumirToken(SIGNO_MENOR_QUE)) call modoPanico(';')
        if (.not. consumirToken(SIGNO_ADMIRACION_C)) call modoPanico(';')
        if (.not. consumirToken(SIGNO_GUION)) call modoPanico(';')
        if (.not. consumirToken(SIGNO_GUION)) call modoPanico(';')
        if (.not. consumirToken(RESERVADA_PROPIEDADES)) call modoPanico(';')
        
        call parse_PropiedadesList()
        
        if (.not. consumirToken(RESERVADA_PROPIEDADES)) call modoPanico(';')
        if (.not. consumirToken(SIGNO_GUION)) call modoPanico(';')
        if (.not. consumirToken(SIGNO_GUION)) call modoPanico(';')
        if (.not. consumirToken(SIGNO_MAYOR_QUE)) call modoPanico(';')
    end subroutine parse_PropiedadesBlock
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
    ! Producción PropiedadesList ::= Propiedad PropiedadesList | ε
    recursive subroutine parse_PropiedadesList()
    print *, "Iniciando parse_PropiedadesList en la posicion ", pos

    if (tokens(pos)%tipo == IDENTIFICADOR) then
        call parse_Propiedades()
        call parse_PropiedadesList()
    else
        print *, "No hay más propiedades, produccion vacia en la posicion ", pos
    end if
end subroutine parse_PropiedadesList
    ! Producción Control ::= ControlType IDENTIFICADOR ';' | COMENTARIO_LINEA
    subroutine parse_Control()
        print *, "Iniciando parse_Control en la posicion ", pos
        if (tokens(pos)%tipo == COMENTARIO_LINEA) then
            print *, "Comentario de linea encontrado en la posicion ", pos
            if (consumirToken(COMENTARIO_LINEA)) return
        else
            print *, "Llamando a parse_ControlType"
            call parse_ControlType()
            if (.not. consumirToken(IDENTIFICADOR)) call modoPanico(';')
            if (.not. consumirToken(SIGNO_PUNTO_Y_COMA)) call modoPanico(';')
        end if
    end subroutine parse_Control

    subroutine parse_PropiedadesControl()
        if (tokens(pos)%tipo == PROPIEDAD_CONTROL) then
            print *, "Propiedad de control encontrada: ", tokens(pos)%valor
            if (.not. consumirToken(PROPIEDAD_CONTROL)) call modoPanico(';')
            ! Manejo de propiedades como setAncho(), setTexto(), setColorFondo(), etc.
        end if
    end subroutine parse_PropiedadesControl

    ! Producción ControlType ::= RESERVADA_ETIQUETA | RESERVADA_BOTON | ...
    subroutine parse_ControlType()
        print *, "Iniciando parse_ControlType en la posicion ", pos
        select case (tokens(pos)%tipo)
            case (RESERVADA_ETIQUETA)
                print *, "Reservada etiqueta encontrada"
                if (.not. consumirToken(RESERVADA_ETIQUETA)) call modoPanico(';')
            case (RESERVADA_BOTON)
                print *, "Reservada boton encontrada"
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
                print *, "Reservada area texto encontrada"
                if (.not. consumirToken(RESERVADA_AREATEXTO)) call modoPanico(';')
            case (RESERVADA_CLAVE)
                print *, "Reservada clave encontrada"
                if (.not. consumirToken(RESERVADA_CLAVE)) call modoPanico(';')
            case (RESERVADA_CONTENEDOR)
                print *, "Reservada contenedor encontrada"
                if (.not. consumirToken(RESERVADA_CONTENEDOR)) call modoPanico(';')
            case default
                print *, "Error sintatico: Tipo de control no valido en la posicion ", pos
                call modoPanico(';')
        end select
    end subroutine parse_ControlType

    ! Producción para parsear propiedades de control
subroutine parse_Propiedades()
    print *, "Iniciando parse_Propiedades en la posicion ", pos

    do while (pos <= size(tokens) .and. tokens(pos)%tipo /= RESERVADA_PROPIEDADES)
        ! Comprobación del formato de la propiedad: IDENTIFICADOR '.' método '(' valores ')'
        
        if (.not. consumirToken(IDENTIFICADOR)) then
            call modoPanico(';')
            return
        end if

        ! Consumir el signo de punto ('.')
        if (.not. consumirToken(SIGNO_PUNTO)) then
            call modoPanico(';')
            return
        end if
        
        ! Manejo de los diferentes métodos (como setAncho, setAlto, setColorFondo, etc.)
        select case (tokens(pos)%tipo)
        case (setAncho)
            if (.not. consumirToken(setAncho)) then
                call modoPanico(';')
                return
            end if
            if (.not. consumirToken(SIGNO_PARENTESIS_APERTURA)) call modoPanico(';')
            if (.not. consumirToken(VALOR_NUMERICO)) call modoPanico(';')
            if (.not. consumirToken(SIGNO_PARENTESIS_CERRADURA)) call modoPanico(';')
            if (.not. consumirToken(SIGNO_PUNTO_Y_COMA)) call modoPanico(';')

        case (setAlto)
            if (.not. consumirToken(setAlto)) then
                call modoPanico(';')
                return
            end if
            if (.not. consumirToken(SIGNO_PARENTESIS_APERTURA)) call modoPanico(';')
            if (.not. consumirToken(VALOR_NUMERICO)) call modoPanico(';')
            if (.not. consumirToken(SIGNO_PARENTESIS_CERRADURA)) call modoPanico(';')
            if (.not. consumirToken(SIGNO_PUNTO_Y_COMA)) call modoPanico(';')

        case (setColorFondo)
            if (.not. consumirToken(setColorFondo)) then
                call modoPanico(';')
                return
            end if
            if (.not. consumirToken(SIGNO_PARENTESIS_APERTURA)) call modoPanico(';')
            if (.not. consumirToken(VALOR_NUMERICO)) call modoPanico(';')
            if (.not. consumirToken(COMA)) call modoPanico(';')
            if (.not. consumirToken(VALOR_NUMERICO)) call modoPanico(';')
            if (.not. consumirToken(COMA)) call modoPanico(';')
            if (.not. consumirToken(VALOR_NUMERICO)) call modoPanico(';')
            if (.not. consumirToken(SIGNO_PARENTESIS_CERRADURA)) call modoPanico(';')
            if (.not. consumirToken(SIGNO_PUNTO_Y_COMA)) call modoPanico(';')

        case (setTexto)
            if (.not. consumirToken(setTexto)) then
                call modoPanico(';')
                return
            end if
            if (.not. consumirToken(SIGNO_PARENTESIS_APERTURA)) call modoPanico(';')
            if (.not. consumirToken(VALOR_CADENA)) call modoPanico(';')
            if (.not. consumirToken(SIGNO_PARENTESIS_CERRADURA)) call modoPanico(';')
            if (.not. consumirToken(SIGNO_PUNTO_Y_COMA)) call modoPanico(';')

        case (add)
            if (.not. consumirToken(add)) then
                call modoPanico(';')
                return
            end if
            if (.not. consumirToken(SIGNO_PARENTESIS_APERTURA)) call modoPanico(';')
            if (.not. consumirToken(IDENTIFICADOR)) call modoPanico(';')
            if (.not. consumirToken(SIGNO_PARENTESIS_CERRADURA)) call modoPanico(';')
            if (.not. consumirToken(SIGNO_PUNTO_Y_COMA)) call modoPanico(';')
        case (setColorLetra)
            if (.not. consumirToken(setColorLetra)) then
                call modoPanico(';')
                return
            end if
            if (.not. consumirToken(SIGNO_PARENTESIS_APERTURA)) call modoPanico(';')
            if (.not. consumirToken(VALOR_NUMERICO)) call modoPanico(';')
            if (.not. consumirToken(COMA)) call modoPanico(';')
            if (.not. consumirToken(VALOR_NUMERICO)) call modoPanico(';')
            if (.not. consumirToken(COMA)) call modoPanico(';')
            if (.not. consumirToken(VALOR_NUMERICO)) call modoPanico(';')
            if (.not. consumirToken(SIGNO_PARENTESIS_CERRADURA)) call modoPanico(';')
            if (.not. consumirToken(SIGNO_PUNTO_Y_COMA)) call modoPanico(';')

        case default
            print *, "Error sintáctico: Método desconocido en la posición ", pos
            call modoPanico(';')
            return
        end select
    end do
end subroutine parse_Propiedades



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
                           (tokens(pos)%tipo == COMENTARIO_LINEA) .or. &
                           (tokens(pos)%tipo == COMENTARIO_MULTILINEA))
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
        character(len=30) :: res
    
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
            res = "SIGNO_PUNTO_Y_COMA"
        case (SIGNO_GUION)
            res = "SIGNO_GUION"
        case (COMENTARIO_LINEA)
            res = "COMENTARIO_LINEA"
        case (SIGNO_PUNTO)
            res = "SIGNO_PUNTO"
        case (SIGNO_PARENTESIS_APERTURA)
            res = "SIGNO_PARENTESIS_APERTURA"
        case (SIGNO_PARENTESIS_CERRADURA)
            res = "SIGNO_PARENTESIS_CERRADURA"
        case (VALOR_CADENA)
            res = "VALOR_CADENA"
        case (VALOR_NUMERICO)
            res = "VALOR_NUMERICO"
        case (COMENTARIO_MULTILINEA)
            res = "COMENTARIO_MULTILINEA"
        case (COMA)
            res = "COMA"
        case (setTexto)
            res = "setTexto"
        case (setAncho)
            res = "setAncho"
        case (setColorFondo)
            res = "setColorFondo"
        case (setPosicion)
            res = "setPosicion"
        case (add)
            res = "add"
        case (setMarcada)
            res = "setMarcada"
        case (setColorLetra)
            res = "setColorLetra"
        case (RESERVADA_PROPIEDADES)
            res = "RESERVADA_PROPIEDADES"
        case (RESERVADA_COLOCACION)
            res = 'RESERVADA_COLOCACION'
        case (THIS)
            res = 'THIS'
        case (setAlto)
            res = 'setAlto'
        case default
            res = "Desconocido"
        end select
    end function getTipoTokenEnString
    ! Producción ColocacionBlock ::= '<' '!' '-' '-' RESERVADA_COLOCACION ColocacionList RESERVADA_COLOCACION '-' '-' '>'
subroutine parse_ColocacionBlock()
    print *, "Iniciando parse_ColocacionBlock"

    if (.not. consumirToken(SIGNO_MENOR_QUE)) call modoPanico(';')
    if (.not. consumirToken(SIGNO_ADMIRACION_C)) call modoPanico(';')
    if (.not. consumirToken(SIGNO_GUION)) call modoPanico(';')
    if (.not. consumirToken(SIGNO_GUION)) call modoPanico(';')
    if (.not. consumirToken(RESERVADA_COLOCACION)) call modoPanico(';')

    call parse_ColocacionList()

    if (.not. consumirToken(RESERVADA_COLOCACION)) call modoPanico(';')
    if (.not. consumirToken(SIGNO_GUION)) call modoPanico(';')
    if (.not. consumirToken(SIGNO_GUION)) call modoPanico(';')
    if (.not. consumirToken(SIGNO_MAYOR_QUE)) call modoPanico(';')
end subroutine parse_ColocacionBlock

! Producción ColocacionList ::= Colocacion ColocacionList | ε
recursive subroutine parse_ColocacionList()
    print *, "Iniciando parse_ColocacionList en la posicion ", pos
    
    if (tokens(pos)%tipo == IDENTIFICADOR .or. tokens(pos)%tipo == THIS) then
        call parse_Colocacion()
        call parse_ColocacionList()
    else
        print *, "No hay mas colocaciones, produccion vacia en la posicion ", pos
    end if
end subroutine parse_ColocacionList

! Parsear cada colocación
subroutine parse_Colocacion()
    print *, "Iniciando parse_Colocacion en la posicion ", pos

    do while (pos <= size(tokens) .and. tokens(pos)%tipo /= RESERVADA_COLOCACION)
        ! Comprobación del formato de la colocación: IDENTIFICADOR '.' setPosicion '(' valores ')' ';'
        
        if (.not. (consumirToken(IDENTIFICADOR) .or. consumirToken(THIS))) then
            call modoPanico(';')
            return
        end if

        ! Consumir el signo de punto ('.')
        if (.not. consumirToken(SIGNO_PUNTO)) then
            call modoPanico(';')
            return
        end if
        
        ! Manejo de los diferentes métodos de colocación
        select case (tokens(pos)%tipo)
        case (setPosicion)
            if (.not. consumirToken(setPosicion)) then
                call modoPanico(';')
                return
            end if
            if (.not. consumirToken(SIGNO_PARENTESIS_APERTURA)) call modoPanico(';')
            if (.not. consumirToken(VALOR_NUMERICO)) call modoPanico(';')  ! Valor x
            if (.not. consumirToken(COMA)) call modoPanico(';')
            if (.not. consumirToken(VALOR_NUMERICO)) call modoPanico(';')  ! Valor y
            if (.not. consumirToken(SIGNO_PARENTESIS_CERRADURA)) call modoPanico(';')
            if (.not. consumirToken(SIGNO_PUNTO_Y_COMA)) call modoPanico(';')

        case (add)
            if (.not. consumirToken(add)) then
                call modoPanico(';')
                return
            end if
            if (.not. consumirToken(SIGNO_PARENTESIS_APERTURA)) call modoPanico(';')
            if (.not. consumirToken(IDENTIFICADOR)) call modoPanico(';')
            if (.not. consumirToken(SIGNO_PARENTESIS_CERRADURA)) call modoPanico(';')
            if (.not. consumirToken(SIGNO_PUNTO_Y_COMA)) call modoPanico(';')

        case default
            print *, "Error sintáctico: Método desconocido en la posición ", pos
            call modoPanico(';')
            return
        end select
    end do
end subroutine parse_Colocacion


end module mod_analizador_sintactico