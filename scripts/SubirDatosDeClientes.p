DEFINE TEMP-TABLE datos
    FIELD cedula AS CHARACTER
    FIELD estadoCivil AS CHARACTER
    FIELD fecAfiliacion AS CHARACTER
    FIELD fecIngreso AS CHARACTER
    FIELD sueldo AS DECIMAL FORMAT ">>>>>>>>>>"
    FIELD fecNacimiento AS CHARACTER.

INPUT FROM c:\INFO_Fodun\Leonardo\VariosDatos.csv.

REPEAT:
    CREATE datos.
    IMPORT DELIMITER ";" datos.
END.

FOR EACH datos NO-LOCK:
    FIND FIRST clientes WHERE clientes.nit = datos.cedula NO-ERROR.
    IF AVAILABLE clientes THEN DO:
        IF datos.estadoCivil = "C" THEN
            clientes.est_civil = "Casado".

        IF datos.estadoCivil = "S" THEN
            clientes.est_civil = "Soltero".

        IF datos.fecAfiliacion <> "0" AND datos.fecAfiliacion <> "" THEN
            clientes.fec_ingreso = DATE(INTEGER(SUBSTRING(datos.fecAfiliacion,5,2)), INTEGER(SUBSTRING(datos.fecAfiliacion,7,2)), INTEGER(SUBSTRING(datos.fecAfiliacion,1,4))).

        IF datos.fecIngreso <> "0" AND datos.fecIngreso <> "" THEN
            clientes.fec_ingEmpresa = DATE(INTEGER(SUBSTRING(datos.fecIngreso,5,2)), INTEGER(SUBSTRING(datos.fecIngreso,7,2)), INTEGER(SUBSTRING(datos.fecIngreso,1,4))).

        IF datos.sueldo > 0 THEN
            clientes.salario = datos.sueldo.

        IF datos.fecNacimiento <> "0" AND datos.fecNacimiento <> "" THEN
            clientes.fec_Nacimiento = DATE(INTEGER(SUBSTRING(datos.fecNacimiento,5,2)), INTEGER(SUBSTRING(datos.fecNacimiento,7,2)), INTEGER(SUBSTRING(datos.fecNacimiento,1,4))).
    END.
END.
