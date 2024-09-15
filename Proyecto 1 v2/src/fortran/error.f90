module ErrorModule
    implicit none

    ! Definimos el tipo Error
    type :: Error
        character(len=20) :: mensaje
        character(len=20) :: tipo
        integer :: linea
        integer :: columna
    end type Error

    contains 

    subroutine initError(mensaje, tipo, linea, columna, e)
        implicit none
        character(len=*), intent(in) :: mensaje, tipo
        integer, intent(in) :: linea, columna
        type(Error), intent(inout) :: e

        ! limpiar el error
        e%mensaje = ""
        e%tipo = ""
        e%linea = 0
        e%columna = 0

        ! asignar valores al error
        e%mensaje = trim(tipo)
        e%tipo = trim(mensaje)
        e%linea = linea
        e%columna = columna
    end subroutine initError

    subroutine addError(errors, mensaje, buffer_, linea, columna)
        implicit none
        type(Error), allocatable, intent(inout) :: errors(:)
        character(len=*), intent(in) :: mensaje, buffer_
        integer, intent(in) :: linea, columna
        type(Error) :: e
        integer :: n
    
        ! Inicializar el error
        call initError(mensaje, buffer_, linea, columna, e)
        ! Verificar si el arreglo de errores está asignado
        if (.not. allocated(errors)) then
            allocate(errors(1))
            errors(1) = e
        else
            ! Añadir el error al array
            n = size(errors)
            call extendArrayErrores(errors)
            errors(n+1) = e
        end if
    end subroutine addError
    
    subroutine extendArrayErrores(errors)
        implicit none
        type(Error), allocatable, intent(inout) :: errors(:)
        type(Error), allocatable :: temp(:)
        integer :: n

        ! extender el array
        n = size(errors)
        allocate(temp(n+1))
        temp(1:n) = errors
        deallocate(errors)
        errors = temp
        
    end subroutine extendArrayErrores

end module ErrorModule
