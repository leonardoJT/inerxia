OUTPUT TO C:\INFO_Fodun\Leonardo\Movs.csv.
EXPORT DELIMITER ";"
    "Agencia" "Fecha" "Comprobante" "Documento" "Nit" "Cuenta" "Descripción" "DB" "CR" "Usuario".

FOR EACH mov_contable WHERE nit = "1019116956" OR nit = "51940575" NO-LOCK BY fec_contable:
    EXPORT DELIMITER ";"
        agencia
        fec_contable
        comprobante
        num_documento
        nit
        cuenta
        comentario
        db
        cr
        usuario.
END.
OUTPUT CLOSE.
