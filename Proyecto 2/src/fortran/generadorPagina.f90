module Generador
    use ControlModule
    use TokenModule
    contains
    subroutine generarPagina(controles, nombreArchivo)
        implicit none
        type(Control), dimension(:), intent(in) :: controles
        character(len=*), intent(in) :: nombreArchivo
        integer :: unidadHTML

        ! Abrir el archivo de salida HTML
        print *, "Abriendo archivo HTML para escribir:", trim(nombreArchivo)//".html"
        open(unit=unidadHTML, file=trim(nombreArchivo)//".html", status="replace", action="write")

        ! Escribir la estructura básica del HTML
        write(unidadHTML, '(A)') "<!DOCTYPE html>"
        write(unidadHTML, '(A)') "<html>"
        write(unidadHTML, '(A)') "<head><link rel='stylesheet' type='text/css' href='"//trim(nombreArchivo)//".css'></head>"
        write(unidadHTML, '(A)') "<body>"

        ! Generar el HTML recursivamente
        print *, "Generando HTML recursivamente..."
        call generarHTMLControles(unidadHTML, controles)

        ! Cerrar el cuerpo y el HTML
        write(unidadHTML, '(A)') "</body>"
        write(unidadHTML, '(A)') "</html>"

        ! Cerrar el archivo HTML
        print *, "Cerrando archivo HTML:", trim(nombreArchivo)//".html"
        close(unidadHTML)
    end subroutine generarPagina

    recursive subroutine generarHTMLControles(unidadHTML, controles)
    use ControlModule
    implicit none
    integer, intent(in) :: unidadHTML
    type(Control), dimension(:), intent(in) :: controles
    integer :: i

    ! Recorrer los controles y generar el HTML apropiado
    do i = 1, size(controles)
        print *, "Generando control:", trim(controles(i)%valor), "de tipo:", trim(controles(i)%tipo)
        select case (controles(i)%tipo)
            case ("root")
                print *, "Procesando el root (this)"
                ! El root ('this') no genera etiquetas adicionales, simplemente escribe el contenido
                if (allocated(controles(i)%hijos)) then
                    print *, "Procesando hijos del root"
                    call generarHTMLControles(unidadHTML, controles(i)%hijos)
                end if

            case ("contenedor")
                write(unidadHTML, '(A)') "<div id='" // trim(controles(i)%valor) // "'>"
                print *, "Generando <div> con id:", trim(controles(i)%valor)
                if (allocated(controles(i)%hijos)) then
                    print *, "Procesando hijos del contenedor:", trim(controles(i)%valor)
                    call generarHTMLControles(unidadHTML, controles(i)%hijos)
                end if
                write(unidadHTML, '(A)') "</div>"

            case ("etiqueta")
                write(unidadHTML, '(A)') "<label id='" // trim(controles(i)%valor) // "'>" // &
                    trim(controles(i)%valor) // "</label>"
                print *, "Generando <label> con id:", trim(controles(i)%valor)

            case ("boton")
                write(unidadHTML, '(A)') "<input type='submit' id='" // trim(controles(i)%valor) // &
                    "' value='" // trim(controles(i)%valor) // "' />"
                print *, "Generando <button> con id:", trim(controles(i)%valor)

            case ("texto")
                write(unidadHTML, '(A)') "<input type='text' id='" // trim(controles(i)%valor) // &
                    "' value='" // trim(controles(i)%valor) // "' />"
                print *, "Generando <input type='text'> con id:", trim(controles(i)%valor)

            case ("areaTexto")
                write(unidadHTML, '(A)') "<textarea id='" // trim(controles(i)%valor) // "'></textarea>"
                print *, "Generando <textarea> con id:", trim(controles(i)%valor)

            case ("clave")
                write(unidadHTML, '(A)') "<input type='password' id='" // trim(controles(i)%valor) // "' />"
                print *, "Generando <input type='password'> con id:", trim(controles(i)%valor)

            case ("check")
                write(unidadHTML, '(A)') "<input type='checkbox' id='" // trim(controles(i)%valor) // "' />"
                print *, "Generando <input type='checkbox'> con id:", trim(controles(i)%valor)

            case ("radioBoton")
                write(unidadHTML, '(A)') "<input type='radio' id='" // trim(controles(i)%valor) // "' />"
                print *, "Generando <input type='radio'> con id:", trim(controles(i)%valor)

            ! Agregar más casos según sea necesario
        end select
    end do
end subroutine generarHTMLControles



subroutine procesarTokensToControles(listaTokens, controles)
    use TokenModule
    use ControlModule
    implicit none
    type(token), dimension(:), intent(in) :: listaTokens
    type(Control), dimension(:), allocatable, intent(out) :: controles
    character(len=100), dimension(:), allocatable :: controlIds
    integer :: numControles, i, j
    integer :: parentIdx, childIdx
    
    ! Primero, contamos el número de controles (más uno para 'this')
    numControles = 1  ! Incluimos 'this' como el control raíz
    print *, "Contando número de controles en los tokens..."
    do i = 1, size(listaTokens)-1
        if (listaTokens(i)%tipo == RESERVADA_CONTENEDOR .or. &
            listaTokens(i)%tipo == RESERVADA_BOTON .or. &
            listaTokens(i)%tipo == RESERVADA_ETIQUETA .or. &
            listaTokens(i)%tipo == RESERVADA_TEXTO .or. &
            listaTokens(i)%tipo == RESERVADA_AREATEXTO .or. &
            listaTokens(i)%tipo == RESERVADA_CLAVE .or. &
            listaTokens(i)%tipo == RESERVADA_CHECK .or. &
            listaTokens(i)%tipo == RESERVADA_RADIOBOTON) then
            print *, "Control encontrado en token", i, "de tipo:", listaTokens(i)%tipo
            numControles = numControles + 1
        end if
    end do
    print *, "Número total de controles encontrados:", numControles

    ! Asignamos memoria a los arreglos
    allocate(controles(numControles))
    allocate(controlIds(numControles))

    ! Inicializamos el control raíz 'this'
    print *, "Inicializando control raíz 'this'"
    controles(1)%tipo = "root"
    controles(1)%valor = "this"
    controlIds(1) = "this"

    ! Poblamos los controles y sus identificadores
    j = 1
    do i = 1, size(listaTokens)-1
        if (listaTokens(i)%tipo == RESERVADA_CONTENEDOR .or. &
            listaTokens(i)%tipo == RESERVADA_BOTON .or. &
            listaTokens(i)%tipo == RESERVADA_ETIQUETA .or. &
            listaTokens(i)%tipo == RESERVADA_TEXTO .or. &
            listaTokens(i)%tipo == RESERVADA_AREATEXTO .or. &
            listaTokens(i)%tipo == RESERVADA_CLAVE .or. &
            listaTokens(i)%tipo == RESERVADA_CHECK .or. &
            listaTokens(i)%tipo == RESERVADA_RADIOBOTON) then

            j = j + 1  ! Incrementamos el índice
            controles(j)%valor = listaTokens(i+1)%valor  ! Identificador
            controlIds(j) = listaTokens(i+1)%valor

            print *, "Procesando control:", trim(controlIds(j)), "de tipo:", listaTokens(i)%tipo

            select case (listaTokens(i)%tipo)
                case (RESERVADA_CONTENEDOR)
                    controles(j)%tipo = "contenedor"
                case (RESERVADA_BOTON)
                    controles(j)%tipo = "boton"
                case (RESERVADA_ETIQUETA)
                    controles(j)%tipo = "etiqueta"
                case (RESERVADA_TEXTO)
                    controles(j)%tipo = "texto"
                case (RESERVADA_AREATEXTO)
                    controles(j)%tipo = "areaTexto"
                case (RESERVADA_CLAVE)
                    controles(j)%tipo = "clave"
                case (RESERVADA_CHECK)
                    controles(j)%tipo = "check"
                case (RESERVADA_RADIOBOTON)
                    controles(j)%tipo = "radioBoton"
                case (THIS)
                    controles(j)%tipo = "this"
            end select
        end if
    end do

    ! Procesar 'add' para construir la jerarquía
    do i = 1, size(listaTokens)-6
        if (listaTokens(i)%tipo == IDENTIFICADOR .and. &
            listaTokens(i+1)%tipo == SIGNO_PUNTO .and. &
            listaTokens(i+2)%tipo == ADD .and. &
            listaTokens(i+3)%tipo == SIGNO_PARENTESIS_APERTURA .and. &
            listaTokens(i+4)%tipo == IDENTIFICADOR .and. &
            listaTokens(i+5)%tipo == SIGNO_PARENTESIS_CERRADURA .and. &
            listaTokens(i+6)%tipo == SIGNO_PUNTO_Y_COMA) then

            print *, "Procesando add:", trim(listaTokens(i)%valor), "->", trim(listaTokens(i+4)%valor)

            ! Obtenemos los identificadores del padre y del hijo
            parentIdx = buscarIndiceControl(controlIds, listaTokens(i)%valor)
            childIdx = buscarIndiceControl(controlIds, listaTokens(i+4)%valor)

            if (parentIdx > 0 .and. childIdx > 0) then
                print *, "Agregando hijo:", trim(controlIds(childIdx)), "a padre:", trim(controlIds(parentIdx))
                call agregarHijo(controles(parentIdx), controles(childIdx))
            else
                print *, "Error: Control no encontrado para 'add'"
            end if
        end if
    end do
end subroutine procesarTokensToControles


integer function buscarIndiceControl(controlIds, id)
    character(len=100), dimension(:), intent(in) :: controlIds
    character(len=*), intent(in) :: id
    integer :: i

    buscarIndiceControl = -1  ! Valor por defecto (no encontrado)
    do i = 1, size(controlIds)
        if (trim(controlIds(i)) == trim(id)) then
            buscarIndiceControl = i
            print *, "Control encontrado:", trim(id), "en índice:", i
            return
        end if
    end do
    print *, "Error: Control no encontrado para id:", trim(id)
end function buscarIndiceControl

! Subrutina para agregar un hijo a un control
subroutine agregarHijo(padre, hijo)
    use ControlModule
    implicit none
    type(Control), intent(inout) :: padre
    type(Control), intent(in) :: hijo
    type(Control), dimension(:), allocatable :: tempHijos
    integer :: numHijos

    if (.not. allocated(padre%hijos)) then
        ! Si no hay hijos asignados, asignamos el primero
        print *, "Asignando el primer hijo:", trim(hijo%valor), "a padre:", trim(padre%valor)
        allocate(padre%hijos(1))
        padre%hijos(1) = hijo
    else
        ! Si ya hay hijos, necesitamos realocar el arreglo
        numHijos = size(padre%hijos)
        print *, "Realocando hijos para agregar:", trim(hijo%valor), "a padre:", trim(padre%valor)
        allocate(tempHijos(numHijos + 1))
        tempHijos(1:numHijos) = padre%hijos  ! Copiar los hijos existentes
        tempHijos(numHijos + 1) = hijo       ! Añadir el nuevo hijo
        call move_alloc(tempHijos, padre%hijos)  ! Realocar el arreglo
    end if
end subroutine agregarHijo

end module Generador
