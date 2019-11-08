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
    FOR EACH cfg_contenidolistas, EACH cfg_listasSarlaft WHERE cfg_contenidolistas.IDLista = STRING(ROWID(cfg_listasSarlaft)) AND cfg_contenidolistas.linea MATCHES(id) AND cfg_listasSarlaft.Estado = 1:
        IF INDEX(cfg_contenidolistas.linea, DatoABuscar) > 1 THEN
            ASSIGN  CaracterAnterior = SUBSTRING(cfg_contenidolistas.linea, INDEX(cfg_contenidolistas.linea, DatoABuscar) - 1, 1).
        ELSE
            ASSIGN CaracterAnterior = " ".
        
        IF INDEX(cfg_contenidolistas.linea, DatoABuscar) <> LENGTH(cfg_contenidolistas.linea) - LENGTH(DatoABuscar) THEN
            ASSIGN  CaracterPosterior = SUBSTRING(cfg_contenidolistas.linea, INDEX(cfg_contenidolistas.linea, DatoABuscar) + LENGTH(DatoABuscar), 1).
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
            ASSIGN linea = cfg_contenidolistas.linea.
            MESSAGE "OPERACIÓN RESTRINGIDA" SKIP
                    "El número de identificación de esta persona fue encontrado en lista SARLAFT '"SKIP
                    cfg_listasSarlaft.lista + "' en la siguiente línea:"SKIP
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


