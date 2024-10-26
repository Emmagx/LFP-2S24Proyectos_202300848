module nary_tree
    implicit none

    type :: Node
        character(len=35) :: id  ! Identificador del nodo
        integer :: value  ! Tipo de control (contenedor, etiqueta, botón, etc.)
        type(Node), allocatable, dimension(:) :: children  ! Lista de hijos
        integer :: num_children = 0
    end type Node

contains

    ! Crear un nuevo nodo con identificador
    function crear_nodo(id, value) result(new_node)
        implicit none
        character(len=*), intent(in) :: id
        integer, intent(in) :: value
        type(Node) :: new_node
        
        new_node%id = id
        new_node%value = value
        allocate(new_node%children(0))  ! Inicializamos sin hijos
        print *, "Nodo creado con id:", trim(id), ", tipo:", value
    end function crear_nodo

    ! Insertar un hijo en un nodo específico
    ! subroutine insertar_hijo(parent, child)
    !     implicit none
    !     type(Node), intent(inout) :: parent
    !     type(Node), intent(in) :: child
    !     integer :: num_children
    !     print *, "insertando hijo"
    !     ! Inicializar children si no está asignado
    !     if (.not. allocated(parent%children)) then
    !         allocate(parent%children(20))  ! Capacidad fija de 20 hijos
    !               ! Iniciar contador de hijos en 0
    !     end if
    !         parent%num_children = parent%num_children + 1
    !         parent%children(parent%num_children) = child  ! Añadir el hijo
    ! end subroutine insertar_hijo
    subroutine insertar_hijo(parent, child)
        implicit none
        type(Node), intent(inout) :: parent
        type(Node), intent(in) :: child
        integer :: MAX_CHILDREN = 9
        integer :: current_size
    
        ! Verificar el tamaño actual de `children`
        if (.not. allocated(parent%children)) then
            allocate(parent%children(MAX_CHILDREN))  ! Inicializa con tamaño MAX_CHILDREN
        else
            current_size = size(parent%children)
            
            ! Redimensionar solo si el número de hijos supera el tamaño actual
            if (parent%num_children >= current_size) then
                call resizeArray(parent%children, current_size + MAX_CHILDREN)
            end if
        end if
    
        ! Agregar el hijo
        if (parent%num_children < size(parent%children)) then
            parent%num_children = parent%num_children + 1
            parent%children(parent%num_children) = child
            print *, "Hijo agregado con id", trim(child%id), "al padre con id", trim(parent%id), &
                     " (Total hijos ahora:", parent%num_children, ")"
        else
            print *, "Error: No se puede agregar más hijos al nodo con id", trim(parent%id)
        end if
    end subroutine insertar_hijo
    
    ! Imprimir el árbol en preorden
    recursive subroutine print_tree(root, level)
        implicit none
        type(Node), intent(in) :: root
        integer, intent(in) :: level
        integer :: i

        ! Imprime la indentación y el valor del nodo actual
        print *, repeat(" ", level * 2), "Nodo id:", trim(root%id), ", tipo:", root%value

        ! Llama recursivamente para imprimir los hijos
        do i = 1, size(root%children)
            call print_tree(root%children(i), level + 1)
        end do
    end subroutine print_tree
    subroutine resizeArray(array, newSize)
        type(Node), allocatable, dimension(:), intent(inout) :: array
        integer, intent(in) :: newSize
        type(Node), allocatable :: tempArray(:)
        integer :: i, oldSize
    
        oldSize = size(array)
        
        if (newSize > oldSize) then
            allocate(tempArray(newSize))
            
            ! Copiar los elementos actuales al nuevo arreglo
            do i = 1, oldSize
                tempArray(i) = array(i)
            end do
            
            ! Asignar el nuevo arreglo a array
            deallocate(array)
            array = tempArray
            
        end if
    end subroutine resizeArray
end module nary_tree