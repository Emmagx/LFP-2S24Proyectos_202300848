module mod_analizador_lexico
    use TokenModule  ! Aseguramos que todos los tokens estén disponibles
    use ErrorModule
    implicit none
    private
    public :: escanear
    
contains

subroutine escanear(entrada, listaTokens, enComentario, noLinea, errores)
    use TokenModule
    character(len=*), intent(inout) :: entrada
    type(token), dimension(:), allocatable, intent(inout) :: listaTokens
    
    integer :: i, estado, length
    character(len=1) :: c
    character(:), allocatable :: auxLex
    logical, intent(inout) :: enComentario
    integer, intent(inout) :: noLinea
    type(Error), dimension(:), allocatable, intent(inout):: errores
    length = len_trim(entrada)
    estado = 0
    auxLex = ""
    i = 0
    if (enComentario) then
        estado = 6
    end if
    ! Añadir un carácter de fin de cadena para facilitar el análisis
    entrada(length + 1: length + 1) = '#'
    length = length + 1

    ! Comenzar el ciclo de análisis
    do while (i <= length)
        i = i + 1
        c = entrada(i:i)

        select case (estado)
        case (0)
            if (c >= 'a' .and. c <= 'z' .or. c >= 'A' .and. c <= 'Z') then
                estado = 1
                auxLex = trim(auxLex) // c
            else if (c == '"') then
                estado = 5
                auxLex = ""
            else if (c >= '0' .and. c <= '9') then
                estado = 8
                auxLex = trim(auxLex) // c
            else if (c == '!' .or. c == '<' .or. c == '>' .or. c == ';' .or. c == '-' .or. c == '.' .or. c == "(" &
                .or. c ==")" .or. c == ',') then
                estado = 2
                auxLex = c
            else if (c == '/') then
                estado = 3
                auxLex = c
            else if (c == char(32) .or. c == char(9) .or. c == char(10)) then
                cycle
            else if (c == '#' .and. i == length) then
                exit
            else
                print *, "Error léxico con: ", trim(c)
                call addError(errores, "Error léxico", trim(c), noLinea) ! Añadir Error

                estado = 0
            end if
        case (1)
            if (c >= 'A' .and. c <= 'Z' .or. c >= 'a' .and. c <= 'z' .or. c >= '0' .and. c <= '9') then
                estado = 1
                auxLex = trim(auxLex) // c
            else
                select case (trim(auxLex))
                ! Propiedades del control
                case ('setTexto')
                    call addToken(setTexto, auxLex, listaTokens, noLinea)
                case ('setAncho')
                    call addToken(setAncho, auxLex, listaTokens, noLinea)
                case ('setColorFondo')
                    call addToken(setColorFondo, auxLex, listaTokens, noLinea)
                case ('setPosicion')
                    call addToken(setPosicion, auxLex, listaTokens, noLinea)
                case ('add')
                    call addToken(add, auxLex, listaTokens, noLinea)
                case ('setMarcada')
                    call addToken(setMarcada, auxLex, listaTokens, noLinea)
                case ('setColorLetra')
                    call addToken(setColorLetra, auxLex, listaTokens, noLinea)
                case ("setAlto")
                    call addToken(setAlto, auxLex, listaTokens, noLinea)
                
                ! Identificadores de controles
                case ('Contenedor')
                    call addToken(RESERVADA_CONTENEDOR, auxLex, listaTokens, noLinea)
                case ('Boton')
                    call addToken(RESERVADA_BOTON, auxLex, listaTokens, noLinea)
                case ('Clave')
                    call addToken(RESERVADA_CLAVE, auxLex, listaTokens, noLinea)
                case ('Etiqueta')
                    call addToken(RESERVADA_ETIQUETA, auxLex, listaTokens, noLinea)
                case ('Texto')
                    call addToken(RESERVADA_TEXTO, auxLex, listaTokens, noLinea)
                case ('AreaTexto')
                    call addToken(RESERVADA_AREATEXTO, auxLex, listaTokens, noLinea)
                case ('Check')
                    call addToken(RESERVADA_CHECK, auxLex, listaTokens, noLinea)
                case ('this')
                    call addToken(THIS, auxLex, listaTokens, noLinea)
                case ('true', 'false', 'False', 'True', 'TRUE', 'FALSE')
                    call addToken(LOGICO, auxLex, listaTokens, noLinea)
                
                ! Palabra reservada específica
                case ("Controles")
                    call addToken(RESERVADA_CONTROLES, auxLex, listaTokens, noLinea)
                case ("Colocacion")
                    call addToken(RESERVADA_COLOCACION, auxLex, listaTokens, noLinea)
                case ("Propiedades")
                    call addToken(RESERVADA_PROPIEDADES, auxLex, listaTokens, noLinea)
                ! Default: cualquier otro identificador
                case default
                    call addToken(IDENTIFICADOR, auxLex, listaTokens, noLinea)
                end select
                i = i - 1
                estado = 0
            end if
        
        case (2)
            select case (auxLex)
            case ('!')
                call addToken(SIGNO_ADMIRACION_C, auxLex, listaTokens, noLinea)
            case ('<')
                call addToken(SIGNO_MENOR_QUE, auxLex, listaTokens, noLinea)
            case ('>')
                call addToken(SIGNO_MAYOR_QUE, auxLex, listaTokens, noLinea)
            case (';')
                call addToken(SIGNO_PUNTO_Y_COMA, auxLex, listaTokens, noLinea)
            case ('-')
                call addToken(SIGNO_GUION, auxLex, listaTokens, noLinea)
            case ('.')
                call addToken(SIGNO_PUNTO, auxLex, listaTokens, noLinea)
            case ('(')
                call addToken(SIGNO_PARENTESIS_APERTURA, auxLex, listaTokens, noLinea)
            case (')')
                call addToken(SIGNO_PARENTESIS_CERRADURA, auxLex, listaTokens, noLinea)
            case (',')
                call addToken(COMA, auxLex, listaTokens, noLinea)
            end select
            estado = 0
            i = i - 1
        case (3)
            if (c == '/') then
                estado = 4
                auxLex = trim(auxLex) // c
            else if (c == '*') then
                estado = 6
                enComentario = .True.
                auxLex = trim(auxLex) // c
            else
                estado = 0
            end if
        case (4)
            if (c == char(10)) then
                call addToken(COMENTARIO_LINEA, auxLex, listaTokens, noLinea)
                estado = 0
                i = i - 1
            else
                auxLex = auxLex // c
            end if
        case (5)
            if (c == '"') then
                call addToken(VALOR_CADENA, auxLex, listaTokens, noLinea)
                estado = 0
            else
                auxLex = auxLex // c
            end if
        case (6)
            if (c == '*') then
                estado = 7
            else
            end if
        case (7)
            if (c == '/') then
                estado = 0
                enComentario = .false.
            else
                estado = 6
            end if
        case (8)
            if (c >= '0' .and. c <= '9') then
                auxLex = auxLex // c
            else
                call addToken(VALOR_NUMERICO, auxLex, listaTokens, noLinea)
                i = i - 1
                estado = 0
            end if
        end select
    end do
end subroutine escanear

subroutine addToken(tipo, valor, listaTokens, noLinea)
    integer, intent(in) :: tipo
    character(len=*), intent(inout) :: valor
    type(token), dimension(:), allocatable, intent(inout) :: listaTokens
    type(token) :: nuevoToken
    type(token), dimension(:), allocatable :: tempTokens
    integer :: tamanoActual
    integer, intent(inout) :: noLinea

    nuevoToken%tipo = tipo
    nuevoToken%valor = valor
    nuevoToken%linea = noLinea
    tamanoActual = size(listaTokens)

    print *, "Anadiendo token: ", trim(getTipoTokenEnString(tipo)), " con valor: ", trim(valor)

    ! Redimensionar la lista de tokens para agregar uno nuevo
    allocate(tempTokens(tamanoActual + 1))

    ! Copiar los tokens actuales a la nueva lista
    if (tamanoActual > 0) then
        tempTokens(1:tamanoActual) = listaTokens
    end if

    ! Añadir el nuevo token a la lista
    tempTokens(tamanoActual + 1) = nuevoToken

    ! Reemplazar la lista original por la lista expandida
    call move_alloc(tempTokens, listaTokens)

    valor = ""
end subroutine addToken

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
    case (setAlto)
        res = 'setAlto'
    case (THIS)
        res = 'THIS'
    case (LOGICO)
        res = 'LOGICO'
    case default
        res = "Desconocido"
    end select
end function getTipoTokenEnString

end module mod_analizador_lexico
