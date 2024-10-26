module ErrorModule

    implicit none

    ! Definimos el tipo Error
    type :: Error
        character(len=20) :: descripcion
        character(len=20) :: tipo
        integer :: linea
    end type Error

    contains 

    subroutine initError(descripcion, tipo, linea, e)
        implicit none
        character(len=*), intent(in) :: descripcion, tipo
        integer, intent(in) :: linea
        type(Error), intent(inout) :: e

        ! limpiar el error
        e%descripcion = ""
        e%tipo = ""
        e%linea = 0

        ! asignar valores al error
        e%descripcion = trim(tipo)
        e%tipo = trim(descripcion)
        e%linea = linea
    end subroutine initError

    subroutine addError(errors, descripcion, buffer_, linea)
        implicit none
        type(Error), allocatable, intent(inout) :: errors(:)
        character(len=*), intent(in) :: descripcion, buffer_
        integer, intent(in) :: linea
        type(Error) :: e
        integer :: n
    
        ! Inicializar el error
        if (.not. isSpecialChar(descripcion)) then
            call initError(descripcion, buffer_, linea, e)
            if (.not. allocated(errors)) then
                allocate(errors(1))
                errors(1) = e
            else
                ! Añadir el error al array
                n = size(errors)
                call extendArrayErrores(errors)
                errors(n+1) = e
            end if
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

    logical function isSpecialChar(c)
        implicit none
        character(len=1), intent(in) :: c

        isSpecialChar = (c == ' ' .or. c == CHAR(9) .or. c == CHAR(10))
    end function isSpecialChar

    subroutine printErrors(errors)
        implicit none
        type(Error), allocatable, intent(in) :: errors(:)
        integer :: i

        ! Si no hay errores, no hacemos nada
        if (.not. allocated(errors)) then
            print *, "No hay errores."
            return
        end if

        ! Imprimir cada error
        do i = 1, size(errors)
            print *, "Error ", i, ":"
            print *, "  Descripción: ", trim(errors(i)%descripcion)
            print *, "  Tipo       : ", trim(errors(i)%tipo)
            print *, "  Línea      : ", errors(i)%linea
            print *, ""
        end do
    end subroutine printErrors

end module ErrorModule
