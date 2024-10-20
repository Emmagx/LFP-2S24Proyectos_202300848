module mod_utilidades
    implicit none
    private
    public :: leerArchivo

contains
    ! Subrutina para leer un archivo y devolver su contenido
    subroutine leerArchivo(nombreArchivo, cadena, ios)
        character(len=*), intent(in) :: nombreArchivo
        character(len=*), intent(out) :: cadena
        integer, intent(out) :: ios
        integer :: iunit
        character(len=100) :: temp_line

        iunit = 10
        cadena = ''
        
        print *, "Intentando abrir el archivo: ", trim(nombreArchivo)

        ! Abrir el archivo
        open(unit=iunit, file=trim(nombreArchivo), status='old', iostat=ios)
        
        ! Si no se pudo abrir, mostrar un mensaje de error
        if (ios /= 0) then
            print *, "Error al abrir el archivo con iostat = ", ios
            cadena = ''
            return
        else
            print *, "Archivo abierto correctamente."
        end if

        ! Leer el contenido del archivo
        do
            read(iunit, '(A)', iostat=ios) temp_line
            if (ios /= 0) exit
            print *, "Línea leída: ", trim(temp_line)
            cadena = trim(cadena) // trim(temp_line) // char(10)
        end do
        
        if (ios == -1) then
            print *, "Fin de archivo alcanzado."
        else
            print *, "Error durante la lectura del archivo. iostat = ", ios
        end if
        
        ios = 0

        ! Cerrar el archivo
        close(iunit)
        print *, "Archivo cerrado."
    end subroutine leerArchivo
end module mod_utilidades
