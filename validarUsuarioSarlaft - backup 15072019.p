DEFINE INPUT PARAMETER DatoABuscar AS CHARACTER.
DEFINE OUTPUT PARAMETER W_Sarlaft AS LOG INIT FALSE.

{INCLUIDO\VARIABLE.I "SHARED"}

DEFINE VAR id AS CHARACTER.
DEFINE VAR linea AS CHARACTER.
DEFINE VAR inicio AS INTEGER.
DEFINE VAR finaldelinea AS INTEGER.
DEFINE VAR CaracterAnterior AS CHARACTER.
DEFINE VAR CaracterPosterior AS CHARACTER.

ASSIGN id = "*" + DatoABuscar + "*".
RUN BuscarListasSarlaft.

PROCEDURE BuscarListasSarlaft:
    FIND FIRST cfg_listasSarlaft WHERE cfg_listasSarlaft.contenido MATCHES(id) AND cfg_listasSarlaft.Estado NO-LOCK NO-ERROR.
    IF AVAILABLE cfg_listasSarlaft THEN DO:
        IF INDEX(cfg_listasSarlaft.contenido, DatoABuscar) > 1 THEN
            ASSIGN  CaracterAnterior = SUBSTRING(cfg_listasSarlaft.contenido, INDEX(cfg_listasSarlaft.contenido, DatoABuscar) - 1, 1).
        ELSE
            ASSIGN CaracterAnterior = " ".
        
        IF INDEX(cfg_listasSarlaft.contenido, DatoABuscar) <> LENGTH(cfg_listasSarlaft.contenido) - LENGTH(DatoABuscar) THEN
            ASSIGN  CaracterPosterior = SUBSTRING(cfg_listasSarlaft.contenido, INDEX(cfg_listasSarlaft.contenido, DatoABuscar) + LENGTH(DatoABuscar), 1).
        ELSE
            ASSIGN  CaracterPosterior = " ".

        DEFINE VARIABLE ceroascii    AS INTEGER   NO-UNDO.
        DEFINE VARIABLE nueveascii    AS INTEGER   NO-UNDO.
        DEFINE VARIABLE chChar1       AS CHARACTER NO-UNDO.
        DEFINE VARIABLE chChar2       AS CHARACTER NO-UNDO.
        
        ASSIGN ceroascii = ASC("0")
               nueveascii = ASC("9")
               chChar1    = SUBSTRING(CaracterAnterior,1,1)
               chChar2    = SUBSTRING(CaracterPosterior,1,1).                           
        
        IF NOT(ASC(chChar1) >= ceroascii AND ASC(chChar1) <= nueveascii) AND NOT(ASC(chChar2) >= ceroascii AND ASC(chChar2) <= nueveascii) AND chChar1 <> "/" THEN DO:
            ASSIGN inicio = R-INDEX(cfg_listasSarlaft.contenido, "~n", INDEX(cfg_listasSarlaft.contenido, DatoABuscar))
                finaldelinea = INDEX(cfg_listasSarlaft.contenido, "~n", INDEX(cfg_listasSarlaft.contenido, DatoABuscar)).
            IF finaldelinea = 0 THEN 
                ASSIGN finaldelinea = LENGTH(cfg_listasSarlaft.contenido) + 2.
            ASSIGN linea = SUBSTRING(cfg_listasSarlaft.contenido, inicio + 1, finaldelinea - inicio - 2).
            MESSAGE "OPERACIÓN RETRINGIDA. El número de identificación de esta persona fue encontrado" SKIP
                    "en lista SARLAFT '" + cfg_listasSarlaft.lista + "' en la siguiente línea:"SKIP
                    linea SKIP
                    "Informe al oficial de cumplimiento." SKIP
                    VIEW-AS ALERT-BOX INFORMATION TITLE "Identificación en listas SARLAFT".

            CREATE coincidencias_ListasSarlaft.

            ASSIGN coincidencias_ListasSarlaft.coincidencia = DatoABuscar
                   coincidencias_ListasSarlaft.idLista = string(ROWID(cfg_listasSarlaft))
                   coincidencias_ListasSarlaft.linea = STRING(linea)
                   coincidencias_ListasSarlaft.fecha = NOW.

            ASSIGN coincidencias_ListasSarlaft.usuario = W_Usuario.
            ASSIGN coincidencias_ListasSarlaft.agencia = W_Agencia.
            
            IF cfg_listasSarlaft.Tipo = "RES" THEN
                W_Sarlaft = TRUE.
            
        END.
    END.

END PROCEDURE.
