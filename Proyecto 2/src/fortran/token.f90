module TokenModule
    implicit none
    private
    public :: token, RESERVADA_CONTROLES, IDENTIFICADOR, SIGNO_MENOR_QUE, SIGNO_MAYOR_QUE
    public :: RESERVADA_ETIQUETA, RESERVADA_BOTON, RESERVADA_CHECK, RESERVADA_RADIOBOTON, RESERVADA_TEXTO
    public :: SIGNO_ADMIRACION_C, SIGNO_PUNTO_Y_COMA, SIGNO_GUION, COMENTARIO_LINEA
    public :: RESERVADA_AREATEXTO, RESERVADA_CLAVE, RESERVADA_CONTENEDOR
    public :: COMENTARIO_MULTILINEA, PROPIEDAD_CONTROL, SIGNO_PUNTO, SIGNO_PARENTESIS_APERTURA
    public :: SIGNO_PARENTESIS_CERRADURA, VALOR_CADENA, VALOR_NUMERICO, imprimirListaTokens
    type :: token
        character(len=5000) :: valor
        integer :: tipo
    end type token

    integer, parameter :: RESERVADA_CONTROLES = 1
    integer, parameter :: IDENTIFICADOR = 2
    integer, parameter :: SIGNO_MENOR_QUE = 3
    integer, parameter :: SIGNO_MAYOR_QUE = 4
    integer, parameter :: SIGNO_ADMIRACION_C = 5
    integer, parameter :: SIGNO_PUNTO_Y_COMA = 6
    integer, parameter :: SIGNO_GUION = 7
    integer, parameter :: COMENTARIO_LINEA = 8
    integer, parameter :: RESERVADA_ETIQUETA = 9
    integer, parameter :: RESERVADA_BOTON = 10
    integer, parameter :: RESERVADA_CHECK = 11
    integer, parameter :: RESERVADA_RADIOBOTON = 12
    integer, parameter :: RESERVADA_TEXTO = 13
    integer, parameter :: RESERVADA_AREATEXTO = 14
    integer, parameter :: RESERVADA_CLAVE = 15
    integer, parameter :: RESERVADA_CONTENEDOR = 16
    integer, parameter :: COMENTARIO_MULTILINEA = 17
    integer, parameter :: PROPIEDAD_CONTROL = 18
    integer, parameter :: SIGNO_PUNTO = 19
    integer, parameter :: SIGNO_PARENTESIS_APERTURA = 20
    integer, parameter :: SIGNO_PARENTESIS_CERRADURA = 21
    integer, parameter :: VALOR_CADENA = 22
    integer, parameter :: VALOR_NUMERICO = 23

    contains
    ! Subrutina para imprimir la lista de tokens
    subroutine imprimirListaTokens(listaTokens)
        type(token), dimension(:), allocatable, intent(in) :: listaTokens
        integer :: i

        print *, "==========================="
        print *, "       Lista de Tokens      "
        print *, "==========================="
        
        do i = 1, size(listaTokens)
            print *, "Token ", i, ": Tipo = ", trim(listaTokens(i)%valor), &
                    ", Valor = '", trim(listaTokens(i)%valor), "'"
        end do

        print *, "==========================="
        print *, " Fin de la lista de tokens  "
        print *, "==========================="
    end subroutine imprimirListaTokens

end module TokenModule