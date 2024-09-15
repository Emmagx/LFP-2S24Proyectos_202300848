module ErrorModule
    implicit none
    type :: Error
        character(len=200) :: mensaje
        character(len=50) :: tipo
        integer :: linea, columna
    end type Error


contains

subroutine addError(errors, descripcion, buffer_, tipo, linea, columna)
    type(Error), allocatable, intent(inout) :: errors(:)
    character(len=*), intent(in) :: descripcion, buffer_, tipo
    integer, intent(in) :: linea, columna
    type(Error), allocatable :: temp(:)
    integer :: n

    ! Obtener el tamaño actual del arreglo de errores
    n = size(errors)

    ! Aumentar el tamaño del arreglo en 1
    allocate(temp(n + 1))

    ! Copiar los errores antiguos al nuevo arreglo
    if (n > 0) then
        temp(1:n) = errors
    end if

    ! Agregar el nuevo error al final
    temp(n + 1)%mensaje = descripcion // ': ' // trim(buffer_)
    temp(n + 1)%tipo = tipo
    temp(n + 1)%linea = linea
    temp(n + 1)%columna = columna

    ! Mover el nuevo arreglo a errors (libera el antiguo)
    call move_alloc(temp, errors)
end subroutine addError

end module ErrorModule
