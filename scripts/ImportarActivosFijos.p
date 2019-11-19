DEFINE TEMP-TABLE activos
    FIELD ccn AS INTEGER
    FIELD codigo AS CHARACTER
    FIELD nombre AS CHARACTER
    FIELD marca AS CHARACTER
    FIELD serial AS CHARACTER
    FIELD codigoVarios AS CHARACTER
    FIELD linea AS INTEGER
    FIELD tipoUnidadVida AS CHARACTER
    FIELD vidaUtilInicial AS INTEGER
    FIELD fechaCompra AS CHARACTER
    FIELD fechaFabricacion AS CHARACTER
    FIELD numUnidades AS INTEGER
    FIELD numFactura AS CHARACTER
    FIELD nitProveedor AS CHARACTER
    FIELD depreciable AS CHARACTER
    FIELD tipTasaDepreMes AS CHARACTER
    FIELD direccion AS CHARACTER
    FIELD subUbicacion AS CHARACTER
    FIELD ciudad AS CHARACTER
    FIELD ciudadSucursal AS CHARACTER
    FIELD saldoActivo AS DECIMAL
    FIELD saldoDepreciacion AS DECIMAL
    FIELD saldoNeto AS DECIMAL.

INPUT FROM D:\Leonardo\ActivosFijos\ActivosBogota.csv.
REPEAT :
    CREATE activos.
    IMPORT DELIMITER ";" activos.
END.

FOR EACH activos NO-LOCK:
    CREATE act_Fijo.
    act_fijo.agencia = 1.
    act_fijo.codigo = activos.codigo.
    act_fijo.nombre = activos.nombre + " " + activos.marca.
END.
