DEFINE INPUT PARAMETER pPath AS CHARACTER.
/*DEFINE VAR pPath AS CHARACTER INITIAL "D:\SFG\Desarrollo\Obj\Reportes\FacturasCupos".*/

DEFINE VAR vNombreArchivo AS CHARACTER.

INPUT FROM OS-DIR(pPath).
REPEAT:
    IMPORT vNombreArchivo.
    OS-DELETE VALUE(pPath + '\' + vNombreArchivo).
END.
INPUT CLOSE.
