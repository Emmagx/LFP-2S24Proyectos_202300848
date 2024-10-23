module ControlModule
    implicit none
    type Control
    character(len=100) :: valor
    character(len=100) :: tipo
    type(Control), dimension(:), allocatable :: hijos
    logical :: consumido = .false.  ! Nuevo atributo para evitar duplicación
    logical :: agregado = .false.
    end type Control

end module ControlModule
