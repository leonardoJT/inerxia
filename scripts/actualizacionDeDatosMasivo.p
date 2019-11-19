DISABLE TRIGGERS FOR LOAD OF clientes.

DEFINE TEMP-TABLE tt
    FIELD agencia AS INTEGER
    FIELD cliente_id AS CHARACTER
    FIELD genero AS CHARACTER
    FIELD fec_nacimiento AS DATE FORMAT "99/99/9999"
    FIELD fec_ingreso AS DATE FORMAT "99/99/9999"
    FIELD tel_residencia AS CHARACTER
    FIELD celular AS CHARACTER
    FIELD DIR_residencia AS CHARACTER
    FIELD email AS CHARACTER
    FIELD fec_pagare AS DATE FORMAT "99/99/9999"
    FIELD facultad AS CHARACTER
    FIELD escuela AS CHARACTER.

DEFINE VAR cont AS INTEGER INITIAL 1.

DEFINE BUFFER bfrFacultades FOR facultades.

INPUT FROM c:\INFO_fodun\leonardo\datosAsociados.csv.
REPEAT:
    CREATE tt.
    IMPORT DELIMITER ";" tt NO-ERROR.

    IF cont = 1 THEN
        DELETE tt.

    cont = cont + 1.
END.
INPUT CLOSE.

FOR EACH tt NO-LOCK:
    FIND FIRST clientes WHERE clientes.nit = tt.cliente_id NO-ERROR.
    IF AVAILABLE clientes THEN DO:
        clientes.agencia = tt.agencia.

        CASE tt.genero:
            WHEN "HOMBRE" THEN clientes.sexo = 1.
            WHEN "MUJER" THEN clientes.sexo = 2.
        END.

        clientes.fec_nacimiento = tt.fec_nacimiento.
        clientes.fec_ingreso = tt.fec_ingreso.
        clientes.tel_residencia = tt.tel_residencia.
        clientes.celular = tt.celular.
        clientes.DIR_residencia = tt.DIR_residencia.
        clientes.email = tt.email.
        clientes.fecPagare = tt.fec_pagare.

        clientes.facultad = 0.
        clientes.departamento = 0.

        FIND FIRST facultades WHERE facultades.agencia = tt.agencia
                                AND facultades.nombre = tt.facultad
                                AND facultades.tipo = "F" NO-LOCK NO-ERROR.
        IF AVAILABLE facultades THEN DO:
            clientes.facultad = integer(facultades.codigo).

            FIND FIRST bfrFacultades WHERE bfrFacultades.agencia = tt.agencia
                                       AND bfrFacultades.nombre = tt.escuela
                                       AND bfrFacultades.tipo = "D"
                                       AND SUBSTRING(bfrFacultades.codigo,1,2) = facultades.codigo NO-LOCK NO-ERROR.
            IF AVAILABLE bfrFacultades THEN DO:
                clientes.facultad = INTEGER(facultades.codigo).
                clientes.departamento = INTEGER(SUBSTRING(bfrFacultades.codigo,3)).
            END.
        END.
    END.
END.

MESSAGE "Fin"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
