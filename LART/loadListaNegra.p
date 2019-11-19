
/*
   20 Codigo                           inte        m
   40 Nit                              char        i
   50 Nombre                           char        im
   60 Apellido1                        char        i
   70 Apellido2                        char        i
   80 Fec_Actualizacion                date
   90 Usuario                          char
  100 Estado                           inte
  110 Id_HaVenido                      logi
  120 Fec_HaVenido                     date
  130 Observacion                      char
  140 Fec_Exclusion                    date
*/


DEFINE TEMP-TABLE tL
    FIELDS nit          LIKE clientes.nit
    FIELDS fecha        AS DATE FORMAT "99/99/9999"
    INDEX idx nit.

INPUT FROM "c:\info_fodun\suspendidos.csv".
REPEAT:
    CREATE tl.
    IMPORT DELIMITER ";" tl.
END.
INPUT CLOSE.

FOR EACH tl WHERE nit NE "" NO-LOCK:
    FIND FIRST clientes WHERE clientes.nit EQ tl.nit NO-LOCK NO-ERROR.
    CREATE listanegra.
    UPDATE listanegra.codigo                = 4
            listanegra.nit                  = tl.nit 
            listanegra.nombre               = clientes.nombre
            listanegra.Apellido1            = clientes.apellido1
            listanegra.Apellido2            = clientes.apellido2
            listanegra.Fec_Actualizacion    = TODAY
            listanegra.Usuario              = "205"
            listanegra.Estado               = 1
            listanegra.Id_HaVenido          = NO
            listanegra.Fec_HaVenido         = ?
            listanegra.Observacion          = ""
            listanegra.Fec_Exclusion        = tl.fecha.
END.
