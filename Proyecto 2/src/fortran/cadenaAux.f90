module ControlModule
    implicit none
    type Control
        character(len=100) :: valor
        character(len=100) :: tipo
        type(Control), dimension(:), allocatable :: hijos
    end type Control
end module ControlModule
