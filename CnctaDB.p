/*  Nombre      : CnctaDB.p
    Descripcion : Conecta Una Base De Datos
    Log         : Creado 14 de Jul de 2008, Ing. Edilberto Mari�o Moya
    Par�metros De Entrada: C�digo Nem�nico Para Leer La Tabla SuperCadCone
    Par�metros De Salida : Cadena Con La Palabra SI o Con La Palabra Error.
*/    

DEF INPUT-OUTPUT PARAMETER c AS CHAR NO-UNDO.

DEF VAR cCnxion AS CHAR NO-UNDO.
FOR FIRST SuperCadCone NO-LOCK
    WHERE
        SuperCadCone.Codigo = c:
    cCnxion = SuperCadCone.Cadena.
END.
CONNECT value(cCnxion) NO-ERROR.
IF CONNECTED ("repositorio")
THEN c = "SI".
ELSE c = "ERROR: Conectando La Base De Datos Con La Cadena '" + cCnxion + "'".

