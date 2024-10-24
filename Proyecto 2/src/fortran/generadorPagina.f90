module Generador
    use ControlModule
    use TokenModule
    implicit none
    contains

    subroutine generarPagina(controles, nombreArchivo)
        implicit none
        type(Control), dimension(:), intent(inout) :: controles
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

        ! Generar los controles agregados al body (Controles que tienen 'this' como padre)
        call generarHTMLControles(unidadHTML, controles, 0)  ! Se agrega el nivel de indentación

        ! Cerrar el cuerpo y el HTML
        write(unidadHTML, '(A)') "</body>"
        write(unidadHTML, '(A)') "</html>"

        ! Cerrar el archivo HTML
        print *, "Cerrando archivo HTML:", trim(nombreArchivo)//".html"
        close(unidadHTML)
    end subroutine generarPagina

    recursive subroutine generarHTMLControles(unidadHTML, controles, nivelIndentacion)
    use ControlModule
    implicit none
    integer, intent(in) :: unidadHTML
    type(Control), dimension(:), intent(inout) :: controles
    integer, intent(in) :: nivelIndentacion
    integer :: i
    character(len=10) :: indentacion

    ! Crear la indentación para los elementos HTML
    indentacion = repeat("  ", nivelIndentacion)

    ! Recorrer los controles y generar el HTML respetando la jerarquía
    do i = 1, size(controles)
        if (.not. controles(i)%consumido .and. controles(i)%agregado) then
            select case (controles(i)%tipo)
                case ('this')
                    ! Procesar los hijos de 'this' (body)
                    if (allocated(controles(i)%hijos)) then
                        call generarHTMLControles(unidadHTML, controles(i)%hijos, nivelIndentacion)
                    end if

                case ("contenedor")
                    ! Escribir el inicio del contenedor (div) con indentación
                    write(unidadHTML, '(A)') trim(indentacion) // "<div id='" // trim(controles(i)%valor) // "'>"
                    ! Generar los hijos dentro del contenedor de forma recursiva
                    if (allocated(controles(i)%hijos)) then
                        call generarHTMLControles(unidadHTML, controles(i)%hijos, nivelIndentacion + 1)
                    end if
                    ! Cerrar el contenedor
                    write(unidadHTML, '(A)') trim(indentacion) // "</div>"

                case ("etiqueta")
                    ! Generar etiqueta (label)
                    write(unidadHTML, '(A)') trim(indentacion) // "<label id='" // trim(controles(i)%valor) // "'>" & 
                    // trim(controles(i)%valor) // "</label>"

                case ("boton")
                    ! Generar botón (input button)
                    write(unidadHTML, '(A)') trim(indentacion) // "<input type='button' id='" // trim(controles(i)%valor) & 
                    //"' value='" // trim(controles(i)%valor) // "' />"

                case ("texto")
                    ! Generar campo de texto (input text)
                    write(unidadHTML, '(A)') trim(indentacion) // "<input type='text' id='" // trim(controles(i)%valor) & 
                    // "' value='" // trim(controles(i)%valor) // "' />"

                case ("clave")
                    ! Generar campo de contraseña (input password)
                    write(unidadHTML, '(A)') trim(indentacion) // "<input type='password' id='" // trim(controles(i)%valor) & 
                    // "' />"

                case ("check")
                    ! Generar checkbox (input checkbox)
                    write(unidadHTML, '(A)') trim(indentacion) // "<input type='checkbox' id='" // trim(controles(i)%valor) & 
                    // "' />"
            end select

            ! Marcar el control como consumido una vez que se haya generado
            controles(i)%consumido = .true.
        end if
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

        ! Contar el número de controles
        numControles = 1  ! Incluimos 'this' como el control raíz
        do i = 1, size(listaTokens)-1
            if (listaTokens(i)%tipo == RESERVADA_CONTENEDOR .or. &
                listaTokens(i)%tipo == RESERVADA_BOTON .or. &
                listaTokens(i)%tipo == RESERVADA_ETIQUETA .or. &
                listaTokens(i)%tipo == RESERVADA_TEXTO .or. &
                listaTokens(i)%tipo == RESERVADA_AREATEXTO .or. &
                listaTokens(i)%tipo == RESERVADA_CLAVE .or. &
                listaTokens(i)%tipo == RESERVADA_CHECK) then
                numControles = numControles + 1
            end if
        end do

        allocate(controles(numControles))
        allocate(controlIds(numControles))

        ! Inicializar el control raíz "this"
        controles(1)%tipo = "this"
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
                listaTokens(i)%tipo == RESERVADA_CHECK) then

                j = j + 1  ! Incrementamos el índice
                controles(j)%valor = listaTokens(i+1)%valor  ! Identificador
                controlIds(j) = listaTokens(i+1)%valor

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
                end select
            end if
        end do

        ! Procesar las relaciones add para construir la jerarquía
        do i = 1, size(listaTokens)-6
            if (listaTokens(i)%tipo == IDENTIFICADOR .and. &
                listaTokens(i+1)%tipo == SIGNO_PUNTO .and. &
                listaTokens(i+2)%tipo == ADD .and. &
                listaTokens(i+3)%tipo == SIGNO_PARENTESIS_APERTURA .and. &
                listaTokens(i+4)%tipo == IDENTIFICADOR .and. &
                listaTokens(i+5)%tipo == SIGNO_PARENTESIS_CERRADURA .and. &
                listaTokens(i+6)%tipo == SIGNO_PUNTO_Y_COMA) then

                parentIdx = buscarIndiceControl(controlIds, listaTokens(i)%valor)
                childIdx = buscarIndiceControl(controlIds, listaTokens(i+4)%valor)

                if (parentIdx > 0 .and. childIdx > 0) then
                    call agregarHijo(controles(parentIdx), controles(childIdx))
                    controles(childIdx)%agregado = .true.  ! Marcar como agregado
                end if
            end if
        end do
    end subroutine procesarTokensToControles

    integer function buscarIndiceControl(controlIds, id)
        character(len=100), dimension(:), intent(in) :: controlIds
        character(len=*), intent(in) :: id
        integer :: i

        buscarIndiceControl = -1
        do i = 1, size(controlIds)
            if (trim(controlIds(i)) == trim(id)) then
                buscarIndiceControl = i
                return
            end if
        end do
    end function buscarIndiceControl

    subroutine agregarHijo(padre, hijo)
        use ControlModule
        implicit none
        type(Control), intent(inout) :: padre
        type(Control), intent(in) :: hijo
        integer :: numHijos
      
        ! Asignar el hijo al padre
        if (.not. allocated(padre%hijos)) then
          allocate(padre%hijos(1))
          padre%hijos(1) = hijo  ! Clonar el hijo
        else
          numHijos = size(padre%hijos)
          ! Liberar la memoria existente
          deallocate(padre%hijos)
          ! Realocar con el tamaño correcto
          allocate(padre%hijos(numHijos + 1))
          ! Copiar los hijos existentes y agregar el nuevo
          padre%hijos(1:numHijos) = padre%hijos(1:numHijos)  ! Auto-copia
          padre%hijos(numHijos + 1) = hijo
        end if
        print *, "Agregando hijo:", hijo%valor, "a padre:", padre%valor
      end subroutine agregarHijo
    
    

end module Generador
