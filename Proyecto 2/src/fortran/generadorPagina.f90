module Generador
    use nary_tree  ! Usamos el modulo del arbol N-ario
    use TokenModule  ! Modulo para gestionar tokens
    implicit none
    
    contains
    
    ! Subrutina para generar el archivo HTML basado en la estructura del arbol de controles
    subroutine generarPagina(arbol, nombreArchivo)
        implicit none
        type(Node), intent(in) :: arbol  ! Raiz del arbol de controles
        character(len=*), intent(in) :: nombreArchivo
        integer :: unidadHTML
        
        ! Abrir el archivo de salida HTML
        print *, "Abriendo archivo HTML para escribir:", trim(nombreArchivo)//".html"
        open(unit=unidadHTML, file=trim(nombreArchivo)//".html", status="unknown", action="write")
        print *, "Abriendo archivo HTML para escribir:", trim(nombreArchivo)//".html"
        ! Escribir la estructura basica del HTML
        write(unidadHTML, '(A)') "<!DOCTYPE html>"
        write(unidadHTML, '(A)') "<html>"
        write(unidadHTML, '(A)') "<head><link rel='stylesheet' type='text/css' href='"//trim(nombreArchivo)//".css'></head>"
        write(unidadHTML, '(A)') "<body>"
        
        ! Generar el HTML recursivamente a partir del arbol
        print *, "Generando HTML desde la raiz del arbol..."
        call generarHTMLControles(unidadHTML, arbol, 0)
        
        ! Cerrar el cuerpo y el HTML
        write(unidadHTML, '(A)') "</body>"
        write(unidadHTML, '(A)') "</html>"
        
        ! Cerrar el archivo HTML
        print *, "Cerrando archivo HTML:", trim(nombreArchivo)//".html"
        close(unidadHTML)
    end subroutine generarPagina
    
    ! Subrutina recursiva para generar el HTML a partir del arbol de controles
    recursive subroutine generarHTMLControles(unidadHTML, nodo, nivelIndentacion)
    implicit none
    integer, intent(in) :: unidadHTML
    type(Node), intent(in) :: nodo
    integer, intent(in) :: nivelIndentacion
    integer :: i
    character(len=10) :: indentacion
    
    ! Crear la indentacion para los elementos HTML
    indentacion = repeat("  ", nivelIndentacion)
    
    print *, "Generando HTML para nodo id:", trim(nodo%id), ", tipo:", nodo%value
    
    ! Generar el HTML segun el tipo de control en el nodo actual
    select case (nodo%value)
    case (RESERVADA_CONTENEDOR)
        write(unidadHTML, '(A)') trim(indentacion) // "<div class='contenedor' id='" //&
        trim(nodo%id) // "'>"
        do i = 1, size(nodo%children)
            call generarHTMLControles(unidadHTML, nodo%children(i), nivelIndentacion + 1)
        end do
        write(unidadHTML, '(A)') trim(indentacion) // "</div>"
        
    case (RESERVADA_ETIQUETA)
        write(unidadHTML, '(A)') trim(indentacion) // "<label id='" // trim(nodo%id) //& 
        "'>Etiqueta</label>"
        
    case (RESERVADA_BOTON)
        write(unidadHTML, '(A)') trim(indentacion) // "<input type='button' id='"// &
        trim(nodo%id) // "' value='Boton' />"
        
    case (RESERVADA_TEXTO)
        write(unidadHTML, '(A)') trim(indentacion) // "<input type='text' id='"// &
        trim(nodo%id) // "' value='Texto' />"
        
    case (RESERVADA_AREATEXTO)
        write(unidadHTML, '(A)') trim(indentacion) // "<textarea id='" // &
        trim(nodo%id) // "'>Texto de area</textarea>"
        
    case (RESERVADA_CLAVE)
        write(unidadHTML, '(A)') trim(indentacion) // "<input type='password' id='" //& 
        trim(nodo%id) // "' value='Clave' />"
        
    case (RESERVADA_CHECK)
        write(unidadHTML, '(A)') trim(indentacion) // "<input type='checkbox' id='" //&
        trim(nodo%id) // "' />"
        
    case (RESERVADA_RADIOBOTON)
        write(unidadHTML, '(A)') trim(indentacion) // "<input type='radio' id='" //&
        trim(nodo%id) // "' />"
    end select
end subroutine generarHTMLControles

subroutine procesarTokensToArbol(listaTokens, arbol)
    implicit none
    type(token), dimension(:), intent(in) :: listaTokens
    type(Node), allocatable, intent(inout) :: arbol
    integer :: i, j
    type(Node), allocatable, dimension(:) :: nodos
    integer :: numNodos
    character(len = 6)::nombreArchivo
    print *, "procesandoTokensToArbol..."
    
    ! Inicializar raiz
    numNodos = 0  ! Inicializar en 0
    arbol = crear_nodo("this", RESERVADA_CONTENEDOR)
    allocate(nodos(10))
    nodos(1) = arbol
    do j = 2, size(listaTokens), 1
        if (listaTokens(j)%tipo == RESERVADA_CLAVE .OR. &
            listaTokens(j)%tipo == RESERVADA_CONTENEDOR .OR. &
            listaTokens(j)%tipo == RESERVADA_AREATEXTO .OR. &
            listaTokens(j)%tipo == RESERVADA_BOTON .OR. &
            listaTokens(j)%tipo == RESERVADA_CHECK .OR. &
            listaTokens(j)%tipo == RESERVADA_ETIQUETA .OR. &
            listaTokens(j)%tipo == RESERVADA_RADIOBOTON .OR. &
            listaTokens(j)%tipo == RESERVADA_TEXTO) then
            numNodos = numNodos + 1
            nodos(numNodos) = crear_nodo(listaTokens(j + 1)%valor, listaTokens(j + 1)%tipo)
            print *, 'Nodo creado: ', nodos(numNodos)%id
        end if
    end do    

    ! Añadir hijos
    do i = 1, size(listaTokens), 1
        if (listaTokens(i)%tipo == add .and. listaTokens(i - 2)%tipo /= THIS) then
            call addChildByIdentifiers(nodos, listaTokens(i-2)%valor, listaTokens(i + 2)%valor)
        end if
    end do
        call insertar_hijo(arbol, nodos(i))

    ! Verificar si se construyó correctamente el árbol
    if (size(nodos) > 0) then
        print *, "Terminando ProcesarTokensArbol"
        do i=1, size(nodos)  
        print *, nodos(i)%id
        end do
        nombreArchivo = "pagina"  ! Cambia esto según tu necesidad
        print *, "Terminando ProcesarTokensArbol"
        call generarPagina(arbol, 'pagina')
    else
        print *, "Error: No se crearon nodos en el árbol"
    end if

end subroutine procesarTokensToArbol

subroutine addChildByIdentifiers(nodos, parent_id, child_id)
    implicit none
    type(Node), dimension(:), intent(inout) :: nodos
    character(len=35), intent(in) :: parent_id, child_id
    integer :: parentIndex, childIndex
    
    ! Buscar los indices de los nodos en `nodos` con `parent_id` y `child_id`
    
    parentIndex = findNodeById(nodos, parent_id)
    print *, "Nodo padre ", trim(parent_id), ' ', parentIndex
    childIndex = findNodeById(nodos, child_id)
    print *, "Nodo hijo ", trim(child_id), ' ', childIndex
    if (parentIndex >= 0) then
        print *, "Nodo padre encontrado: ", trim(parent_id), " con indice ", parentIndex
    else
        print *, "Nodo padre ", trim(parent_id), " no encontrado"
    endif
    
    if (childIndex >= 0) then
        print *, "Nodo hijo encontrado: ", trim(child_id), " con indice ", childIndex
    else
        print *, "Nodo hijo ", trim(child_id), " no encontrado"
    endif
    
    ! Añadir el nodo hijo al nodo padre si ambos se encuentran
    if (parentIndex > 0 .and. childIndex > 0) then
        call insertar_hijo(nodos(parentIndex), nodos(childIndex))
    else
        print *, "Error: No se pudo vincular ", trim(parent_id), " con ", trim(child_id)
    endif
end subroutine addChildByIdentifiers

! Funcion para encontrar el indice de un nodo por su id
integer function findNodeById(nodos, id)
    implicit none
    type(Node), dimension(:), intent(in) :: nodos
    character(len=35), intent(in) :: id
    integer :: i

    findNodeById = -1  ! Inicializar como no encontrado
    do i = 1, size(nodos, 1)  ! Inicia en 1
        print *, 'Buscando en nodo con indice:', i, ', id:', trim(nodos(i)%id)
        if (trim(nodos(i)%id) == trim(id)) then
            findNodeById = i
            print *, 'Nodo encontrado con id: ', trim(id), ' en indice: ', i
            exit
        end if
    end do

    if (findNodeById == -1) print *, " No se encontro el nodo con id: ", trim(id)
end function findNodeById


end module Generador
