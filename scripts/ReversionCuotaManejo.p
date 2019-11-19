DEFINE TEMP-TABLE tt
    FIELD nit AS CHARACTER
    FIELD cue_ahorros AS CHARACTER.

DEFINE VAR vSec AS INTEGER.

INPUT FROM d:\Leonardo\cub.csv.
REPEAT :
    CREATE tt.
    IMPORT DELIMITER ";" tt.
END.
INPUT CLOSE.

FIND FIRST comprobantes WHERE comprobantes.comprobante = 999 NO-ERROR.
comprobantes.secuencia = comprobantes.secuencia + 1.
vSec = comprobantes.secuencia.

FOR EACH tt NO-LOCK:
    FIND FIRST ahorros WHERE ahorros.nit = tt.nit
                         AND ahorros.cue_ahorros = tt.cue_ahorros NO-ERROR.
    IF AVAILABLE ahorros THEN DO:
        ahorros.sdo_disponible = ahorros.sdo_disponible + 900.

        CREATE Mov_Ahorros.
        ASSIGN Mov_Ahorros.Agencia = Ahorros.Agencia
               Mov_Ahorros.Age_Destino = Ahorros.Agencia
               Mov_Ahorros.Age_Fuente = ahorros.agencia
               Mov_Ahorros.Cod_Ahorro = Ahorros.Cod_Ahorro
               Mov_Ahorros.Cue_Ahorros = Ahorros.Cue_Ahorro
               Mov_Ahorros.Fecha = TODAY
               Mov_Ahorros.Hora = TIME
               Mov_Ahorros.Nit = Ahorros.Nit
               Mov_Ahorros.Num_Documento = STRING(vSec)
               Mov_Ahorros.Sdo_Disponible = ahorros.sdo_disponible
               Mov_Ahorros.Usuario = "2305"
               Mov_Ahorros.Val_Efectivo = 900
               Mov_Ahorros.Cod_Operacion = 010101001
               Mov_Ahorros.Cpte = comprobantes.comprobante
               Mov_Ahorros.Descrip = "ReversiónCuotaManejo".


        CREATE mov_contable.
        ASSIGN mov_contable.agencia = ahorros.agencia
               mov_contable.cuenta = "21050501"
               mov_contable.comentario = "ReversiónCuotaManejo"
               mov_contable.nit = ahorros.nit
               mov_contable.cr = 900
               mov_contable.cen_costos = 999
               Mov_Contable.Destino = ahorros.agencia
               Mov_Contable.Comprobante = comprobantes.comprobante
               Mov_Contable.Num_Documento vSec
               Mov_contable.Doc_referencia = STRING(vSec)
               Mov_Contable.Fec_Contable = TODAY
               Mov_Contable.Fec_Grabacion = TODAY
               Mov_Contable.Usuario = "2305"
               Mov_Contable.Estacion = "005".

        CREATE mov_contable.
        ASSIGN mov_contable.agencia = ahorros.agencia
               mov_contable.cuenta = "41509807"
               mov_contable.comentario = "ReversiónCuotaManejo"
               mov_contable.nit = ahorros.nit
               mov_contable.db = 900
               mov_contable.cen_costos = 999
               Mov_Contable.Destino = ahorros.agencia
               Mov_Contable.Comprobante = comprobantes.comprobante
               Mov_Contable.Num_Documento vSec
               Mov_contable.Doc_referencia = STRING(vSec)
               Mov_Contable.Fec_Contable = TODAY
               Mov_Contable.Fec_Grabacion = TODAY
               Mov_Contable.Usuario = "2305"
               Mov_Contable.Estacion = "005".
    END.
END.
