module mod_analizador_lexico
    use TokenModule  ! Aseguramos que todos los tokens estén disponibles
    implicit none
    private
    public :: escanear

contains

subroutine escanear(entrada, listaTokens)
    use TokenModule
    character(len=*), intent(inout) :: entrada
    type(token), dimension(:), allocatable, intent(out) :: listaTokens
    integer :: i, estado, length
    character(len=1) :: c
    character(:), allocatable :: auxLex

    length = len_trim(entrada)
    estado = 0
    auxLex = ""
    i = 0
    allocate(listaTokens(0))

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
            else if (c == '(') then
                call addToken(SIGNO_PARENTESIS_APERTURA, c, listaTokens)
            else if (c == ')') then
                call addToken(SIGNO_PARENTESIS_CERRADURA, c, listaTokens)
            else if (c == '"') then
                estado = 5
                auxLex = ""
            else if (c >= '0' .and. c <= '9') then
                estado = 8
                auxLex = trim(auxLex) // c
            else if (c == '!' .or. c == '<' .or. c == '>' .or. c == ';' .or. c == '-' .or. c == '.') then
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
                estado = 0
            end if
        case (1)
            if (c >= 'A' .and. c <= 'Z' .or. c >= 'a' .and. c <= 'z' .or. c >= '0' .and. c <= '9') then
                estado = 1
                auxLex = trim(auxLex) // c
            else
                select case (trim(auxLex))
                case ('setTexto', 'setAncho', 'setColorFondo', 'setPosicion', 'add', 'setMarcada', 'setColorLetra')
                    call addToken(PROPIEDAD_CONTROL, auxLex, listaTokens)
                case ('Contenedor', 'Boton', 'Clave', 'Etiqueta', 'Texto', 'AreaTexto', 'Check')
                    call addToken(IDENTIFICADOR, auxLex, listaTokens)
                case default
                    call addToken(IDENTIFICADOR, auxLex, listaTokens)
                end select
                i = i - 1
                estado = 0
            end if
        case (2)
            select case (auxLex)
            case ('!')
                call addToken(SIGNO_ADMIRACION_C, auxLex, listaTokens)
            case ('<')
                call addToken(SIGNO_MENOR_QUE, auxLex, listaTokens)
            case ('>')
                call addToken(SIGNO_MAYOR_QUE, auxLex, listaTokens)
            case (';')
                call addToken(SIGNO_PUNTO_Y_COMA, auxLex, listaTokens)
            case ('-')
                call addToken(SIGNO_GUION, auxLex, listaTokens)
            case ('.')
                call addToken(SIGNO_PUNTO, auxLex, listaTokens)
            end select
            estado = 0
            i = i - 1
        case (3)
            if (c == '/') then
                estado = 4
                auxLex = trim(auxLex) // c
            else if (c == '*') then
                estado = 6
                auxLex = trim(auxLex) // c
            else
                estado = 0
            end if
        case (4)
            if (c == char(10)) then
                call addToken(COMENTARIO_LINEA, auxLex, listaTokens)
                estado = 0
                i = i - 1
            else
                auxLex = auxLex // c
            end if
        case (5)
            if (c == '"') then
                call addToken(VALOR_CADENA, auxLex, listaTokens)
                estado = 0
            else
                auxLex = auxLex // c
            end if
        case (6)
            if (c == '*') then
                estado = 7
            else
                auxLex = auxLex // c
            end if
        case (7)
            if (c == '/') then
                call addToken(COMENTARIO_MULTILINEA, auxLex, listaTokens)
                estado = 0
            else
                auxLex = auxLex // '*' // c
                estado = 6
            end if
        case (8)
            if (c >= '0' .and. c <= '9') then
                auxLex = auxLex // c
            else
                call addToken(VALOR_NUMERICO, auxLex, listaTokens)
                i = i - 1
                estado = 0
            end if
        end select
    end do
end subroutine escanear

subroutine addToken(tipo, valor, listaTokens)
    integer, intent(in) :: tipo
    character(len=*), intent(inout) :: valor
    type(token), dimension(:), allocatable, intent(inout) :: listaTokens
    type(token) :: nuevoToken
    type(token), dimension(:), allocatable :: tempTokens
    integer :: tamanoActual

    nuevoToken%tipo = tipo
    nuevoToken%valor = valor

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
    character(len=20) :: res

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
    case default
        res = "Desconocido"
    end select
end function getTipoTokenEnString

end module mod_analizador_lexico
