&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* oakley */

{Incluido\variable.i "shared"}
/* Local Variable Definitions ---                                       */

DEFI VAR w-hora    AS CHARACTER FORMAT "X(10)"  INITIAL "" NO-UNDO.
DEFI VAR w-salida  AS CHARACTER FORMAT "X(80)"  INITIAL "" NO-UNDO.
DEFI VAR w-salida2 AS CHARACTER FORMAT "X(80)"  INITIAL "" NO-UNDO.
DEFI VAR W-Cadena  AS CHARACTER FORMAT "X(132)" INITIAL "" NO-UNDO.
DEFI VAR W_Ok      AS LOGICAL INITIAL FALSE                NO-UNDO.
DEFI TEMP-TABLE clientesmov
    FIELD Estado            AS INTEGER       
    FIELD Cedula            AS CHARACTER FORMAT "X(14)"
    FIELD Nombre            AS CHARACTER FORMAT "X(40)"
    FIELD TarjetaDb         LIKE tarjetas.tarjetaDB  
    FIELD Num_CueAhorro     LIKE tarjetas.Cue_ahorro
    FIELD Sdo_Total         AS DECIMAL
    FIELD Sdo_Disponible    AS DECIMAL
    FIELD Cupo_MaxRetCaj    AS DECIMAL
    FIELD Trans_Caj         AS INTEGER
    FIELD Cupo_MaxRetPos    AS DECIMAL
    FIELD Trans_pos         AS INTEGER
    FIELD Novedad           AS DECIMAL
  INDEX IdxNom IS PRIMARY Nombre 
  INDEX IdxTar TarjetaDb.

DEFINE VARIABLE j AS INTEGER INITIAL 0 NO-UNDO.
DEFINE VARIABLE k AS INTEGER INITIAL 0 NO-UNDO.
DEFINE VARIABLE reem AS INTEGER INITIAL 0 NO-UNDO.
DEFINE VARIABLE ini AS CHARACTER EXTENT 18 NO-UNDO.
DEFINE VARIABLE fin AS CHARACTER EXTENT 18 NO-UNDO.
DEFINE VARIABLE viTotArray AS INTEGER INITIAL 18 NO-UNDO.
DEFINE VARIABLE W_Sarlaft AS LOG INIT FALSE.

ASSIGN ini[1]  = "Á"
       ini[2]  = "É"
       ini[3]  = "Í"
       ini[4]  = "Ó"
       ini[5]  = "Ú"
       ini[6]  = "À"
       ini[7]  = "È"
       ini[8]  = "Ì"
       ini[9]  = "Ò"
       ini[10] = "Ù"
       ini[11] = "'"
       ini[12] = "`"
       ini[13] = "´"
       ini[14] = "¥"
       ini[15] = "Ñ"
       ini[16] = "-"
       ini[17] = "Ü"
       ini[18] = ".".

ASSIGN fin[1]  = "A"
       fin[2]  = "E"
       fin[3]  = "I"
       fin[4]  = "O"
       fin[5]  = "U"
       fin[6]  = "A"
       fin[7]  = "E"
       fin[8]  = "I"
       fin[9]  = "O"
       fin[10] = "U"
       fin[11] = ""
       fin[12] = ""
       fin[13] = ""
       fin[14] = "N"
       fin[15] = "N"
       fin[16] = ""
       fin[17] = "U"
       fin[18] = "".

DEFINE BUFFER bfrCreditos FOR creditos.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Frm-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-345 B-Exportar Btn_Done fCedula 
&Scoped-Define DISPLAYED-OBJECTS w-totreg W-TotSdo W-TotDis fCedula ~
w-Mensaje 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-6 Btn-Visualizar 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-Exportar 
     LABEL "E&xportar" 
     SIZE 12 BY 1.5.

DEFINE BUTTON Btn-Visualizar 
     IMAGE-UP FILE "imagenes/impresora2.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/impresora2.bmp":U
     LABEL "Button 2" 
     SIZE 12 BY 1.5.

DEFINE BUTTON Btn_Done DEFAULT 
     IMAGE-UP FILE "imagenes/salir3.bmp":U
     LABEL "&Salir" 
     SIZE 12 BY 1.5 TOOLTIP "Sale de este proceso"
     BGCOLOR 8 .

DEFINE VARIABLE fCedula AS CHARACTER FORMAT "X(256)":U 
     LABEL "Cédula (Reporte individual)" 
     VIEW-AS FILL-IN 
     SIZE 18.57 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE w-Mensaje AS CHARACTER FORMAT "X(50)":U 
     VIEW-AS FILL-IN 
     SIZE 51.72 BY .54
     BGCOLOR 8 FGCOLOR 12  NO-UNDO.

DEFINE VARIABLE W-TotDis AS INT64 FORMAT "->,>>>,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE w-totreg AS DECIMAL FORMAT "->>,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W-TotSdo AS INT64 FORMAT "->,>>>,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-345
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 38 BY 3.19
     BGCOLOR 8 FGCOLOR 0 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Frm-Main
     Btn-Visualizar AT ROW 1.73 COL 42 WIDGET-ID 166
     B-Exportar AT ROW 3.23 COL 42 WIDGET-ID 184
     w-totreg AT ROW 3.42 COL 21.29 COLON-ALIGNED NO-LABEL WIDGET-ID 6
     W-TotSdo AT ROW 4.35 COL 21.29 COLON-ALIGNED NO-LABEL WIDGET-ID 168
     Btn_Done AT ROW 4.73 COL 41.86 WIDGET-ID 20
     W-TotDis AT ROW 5.27 COL 21.29 COLON-ALIGNED NO-LABEL WIDGET-ID 170
     fCedula AT ROW 6.92 COL 26.72 COLON-ALIGNED WIDGET-ID 188
     w-Mensaje AT ROW 8.27 COL 2.29 NO-LABEL WIDGET-ID 182
     "Total Disponible:" VIEW-AS TEXT
          SIZE 16 BY .81 AT ROW 5.23 COL 5.43 WIDGET-ID 174
          FGCOLOR 12 
     " Registros Leídos:" VIEW-AS TEXT
          SIZE 17.14 BY .81 AT ROW 3.42 COL 4.29 WIDGET-ID 8
          FGCOLOR 12 
     "EXPORTAR CLIENTES - SALDOS" VIEW-AS TEXT
          SIZE 30 BY .5 AT ROW 1.73 COL 6.86 WIDGET-ID 26
          BGCOLOR 8 FONT 1
     " Resumen:" VIEW-AS TEXT
          SIZE 11 BY .81 AT ROW 2.69 COL 3.14 WIDGET-ID 4
          FGCOLOR 0 
     "Total Montos:" VIEW-AS TEXT
          SIZE 13 BY .81 AT ROW 4.35 COL 8.29 WIDGET-ID 172
          FGCOLOR 12 
     RECT-345 AT ROW 3.19 COL 2.57 WIDGET-ID 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 54.14 BY 8.12
         BGCOLOR 17  WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Generación de saldos Tarjeta Débito"
         HEIGHT             = 8.12
         WIDTH              = 54.14
         MAX-HEIGHT         = 26.77
         MAX-WIDTH          = 194.86
         VIRTUAL-HEIGHT     = 26.77
         VIRTUAL-WIDTH      = 194.86
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME Frm-Main
   FRAME-NAME                                                           */
/* SETTINGS FOR BUTTON Btn-Visualizar IN FRAME Frm-Main
   NO-ENABLE 6                                                          */
ASSIGN 
       Btn-Visualizar:HIDDEN IN FRAME Frm-Main           = TRUE.

/* SETTINGS FOR FILL-IN w-Mensaje IN FRAME Frm-Main
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN W-TotDis IN FRAME Frm-Main
   NO-ENABLE                                                            */
ASSIGN 
       W-TotDis:READ-ONLY IN FRAME Frm-Main        = TRUE.

/* SETTINGS FOR FILL-IN w-totreg IN FRAME Frm-Main
   NO-ENABLE                                                            */
ASSIGN 
       w-totreg:READ-ONLY IN FRAME Frm-Main        = TRUE.

/* SETTINGS FOR FILL-IN W-TotSdo IN FRAME Frm-Main
   NO-ENABLE                                                            */
ASSIGN 
       W-TotSdo:READ-ONLY IN FRAME Frm-Main        = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Generación de saldos Tarjeta Débito */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Generación de saldos Tarjeta Débito */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Exportar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Exportar C-Win
ON CHOOSE OF B-Exportar IN FRAME Frm-Main /* Exportar */
DO:
    DEFINE VAR nomcontrol AS CHARACTER.
    DEFINE VAR nomcliente AS CHARACTER.
    DEFINE VAR nomcuentas AS CHARACTER.
    DEFINE VAR nomtarjeta AS CHARACTER.
    DEFINE VAR nomtarvcta AS CHARACTER.
    DEFINE VAR cFecha AS CHARACTER FORMAT "X(8)".
    DEFINE VAR cFechaHoy AS CHARACTER FORMAT "X(8)".
    DEFINE VAR cupo AS DECIMAL.
    DEFINE VARIABLE concli AS INTEGER INITIAL 0.
    DEFINE VARIABLE concta AS INTEGER INITIAL 0.
    DEFINE VARIABLE vcSexo AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE vi AS INTEGER NO-UNDO.
    DEFINE VARIABLE viCntCupo AS INT64 INITIAL 0 NO-UNDO.
    DEFINE VARIABLE viTotMonto AS INT64 NO-UNDO.
    DEFINE VARIABLE vcComando AS CHARACTER NO-UNDO.
    DEFINE VARIABLE vcNomFile7z AS CHARACTER NO-UNDO.
    DEFINE VARIABLE vcRaiz AS CHARACTER NO-UNDO.
    DEFINE VAR signo AS CHARACTER.
    DEFINE VAR tipoDoc AS CHARACTER.
    DEFINE VAR operacionesCajero AS INTEGER.
    DEFINE VAR operacionesPOS AS INTEGER.
    DEFINE VAR montoCajeros AS DECIMAL.
    DEFINE VAR montoPOS AS DECIMAL.
    DEFINE VAR archivoCuposPorCanal AS CHARACTER.
    DEFINE VAR contFCVA AS INTEGER.
    
    FIND FIRST cfg_tarjetaDB NO-LOCK NO-ERROR.

    FIND FIRST procDia WHERE procDia.cod_proceso = 13
                         AND procDia.fecha_proc = w_fecha - 1
                         AND procDia.estado = 2 NO-LOCK NO-ERROR.
    IF NOT AVAILABLE procDia THEN DO:
        MESSAGE "El archivo de movimientos aun no ha sido Aplicado. La generación del archivo de saldos no se permite hasta tanto no sean aplicados los movimientos el día de hoy." SKIP(2)
                "Descargue y aplique el archivo de movimientos para poder realizar luego la exportación de los saldos."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

        W-Mensaje:SCREEN-VALUE = "Proceso fallido...!!!".

        RETURN NO-APPLY.
    END.

    IF fCedula <> "" THEN DO:
        FIND FIRST clientes WHERE clientes.nit = fCedula NO-LOCK NO-ERROR.
        IF NOT AVAILABLE clientes THEN DO:
            MESSAGE "Esta cédula no se encuentra matriculada en la Base de Datos de Asociados/Clientes. Revise por favor."
                VIEW-AS ALERT-BOX INFO BUTTONS OK.

            W-Mensaje:SCREEN-VALUE = "Cliente no encontrado...!!!".

            RETURN NO-APPLY.
        END.
    END.

    FIND FIRST procDia WHERE procDia.cod_proceso = 14
                         AND procDia.fecha_proc = w_fecha
                         AND procDia.estado = 2 NO-LOCK NO-ERROR.
    IF AVAILABLE procDia THEN DO:
        IF fCedula = "" THEN DO:
            MESSAGE "Este proceso ya fue generado para el día de hoy. No se permite volver a generar el archivo."
                VIEW-AS ALERT-BOX INFO BUTTONS OK.

            W-Mensaje:SCREEN-VALUE = "Proceso fallido...!!!".

            RETURN NO-APPLY.
        END.
        ELSE DO:
            MESSAGE "Este proceso ya ha sido generado el día de hoy. Antes de hacer clic en el botón Aceptar se recomienda que se haga una copia de los archivos ya generados, pues estos
                    serán reemplazados"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
        END.
    END.

    /* Validación de Usuario */
    IF w_usuario <> cfg_tarjetaDB.usuarioAdministrador THEN DO:
        MESSAGE "Su usuario no está autorizado para realizar esta operación." SKIP
                "Consulte con el Administrador del Sistema..."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

        RETURN NO-APPLY.
    END.

    /* Se marca como reportado todo lo que esté pendiente por reportar a Visionamos */
    FOR EACH reportarVisionamos WHERE reportarVisionamos.estado = 1:
        reportarVisionamos.estado = 2.
    END.

    vcRaiz = "c:\visionamos\" + trim(string(YEAR(TODAY))) + trim(string(month(TODAY),"99")) + trim(string(day(TODAY),"99")) + ".00000018".

    ASSIGN  vcComando = "C:\7-Zip\7z"
            vcNomFile7z = vcRaiz + ".7z"
            vcRaiz = vcRaiz + "*".

    OS-COMMAND SILENT DEL VALUE(vcRaiz).

    nomcontrol = "c:\visionamos\" + trim(string(YEAR(TODAY))) + trim(string(month(TODAY),"99")) + trim(string(day(TODAY),"99")) + ".00000018.F".
    nomcliente = "c:\visionamos\" + trim(string(YEAR(TODAY))) + trim(string(month(TODAY),"99")) + trim(string(day(TODAY),"99")) + ".00000018.FCL".
    nomcuentas = "c:\visionamos\" + trim(string(YEAR(TODAY))) + trim(string(month(TODAY),"99")) + trim(string(day(TODAY),"99")) + ".00000018.FAC".
    nomtarjeta = "c:\visionamos\" + trim(string(YEAR(TODAY))) + trim(string(month(TODAY),"99")) + trim(string(day(TODAY),"99")) + ".00000018.FCA".
    nomtarvcta = "c:\visionamos\" + trim(string(YEAR(TODAY))) + trim(string(month(TODAY),"99")) + trim(string(day(TODAY),"99")) + ".00000018.FCVA".
    archivoCuposPorCanal = "c:\visionamos\" + trim(string(YEAR(TODAY))) + trim(string(month(TODAY),"99")) + trim(string(day(TODAY),"99")) + ".00000018.FACH".

    OUTPUT TO VALUE(NOMCLIENTE).
        FOR EACH clientes WHERE clientes.nit <> "":
            IF fCedula <> "" THEN DO:
                IF clientes.nit <> fCedula THEN
                    NEXT.
            END.

            CASE clientes.tipo_identificacion:
                WHEN "C.C" THEN tipoDoc = "0".
                WHEN "C.E" THEN tipoDoc = "1".
                WHEN "T.I" THEN tipoDoc = "2".
                WHEN "PAS" THEN tipoDoc = "3".
                WHEN "NIT" THEN tipoDoc = "9".
                OTHERWISE tipoDoc = "0".
            END CASE.

            FIND FIRST creditos WHERE creditos.nit = clientes.nit
                                  AND creditos.COD_CREDITO = 123
                                  AND creditos.monto GT 0
                                  AND creditos.estado EQ 2 NO-LOCK NO-ERROR.
            IF NOT AVAILABLE(creditos) THEN
                NEXT.

            IF TRIM(clientes.nombre) = "" THEN DO:
                FIND FIRST anexos_cliente WHERE anexos_clientes.nit = clientes.nit NO-LOCK NO-ERROR.
                IF AVAILABLE anexos_clientes THEN
                    clientes.nombre = anexos_clientes.NOMBRE1 + " " + anexos_clientes.NOMBRE2.
            END.

            /* Validamos la Dirección, ya que es obligatoria */
            IF clientes.DIR_residencia = "" THEN DO:
                MESSAGE "El Asociado" clientes.nit "-" clientes.nombre clientes.apellido1 clientes.apellido2 SKIP
                        "no tiene matriculada una dirección de residencia válida." SKIP
                        "Revise por favor. El proceso de exportación de saldos se cancela"
                    VIEW-AS ALERT-BOX INFO BUTTONS OK.

                W-Mensaje:SCREEN-VALUE = "Proceso fallido...!!!".

                RETURN NO-APPLY.
            END.

            CFECHA = trim(string(YEAR(FEC_NACIMIENTO))) + trim(string(month(FEC_NACIMIENTO), "99")) + trim(string(day(FEC_NACIMIENTO), "99")).
            cfechaHoy = trim(string(YEAR(TODAY))) + trim(string(month(TODAY), "99")) + trim(string(day(TODAY), "99")).

            IF CFECHA EQ ? THEN
                CFECHA = "".

            IF clientes.sexo = 1 THEN
                vcSexo = "M".
            ELSE
                vcSexo = "F".

            concli = concli + 1.

            IF concli GT 1 THEN
                PUT SKIP.

            IF clientes.estado = 1 THEN
                signo = "+".
            ELSE
                signo = "-".

            PUT UNFORMATTED signo                                           ","
                            TRIM(REPLACE(CLIENTES.NIT,","," "))             ","
                            tipoDoc                                         ","
                            TRIM(REPLACE(CLIENTES.NOMBRE,","," "))          ","
                            ""                                              ","
                            TRIM(REPLACE(CLIENTES.APELLIDO1,","," "))       ","
                            TRIM(REPLACE(CLIENTES.APELLIDO2,","," "))       ","
                            TRIM(REPLACE(CLIENTES.DIR_RESIDENCIA,","," "))  ","
                            TRIM(REPLACE(CLIENTES.DIR_COMERCIAL,","," "))   ","
                            TRIM(REPLACE(CLIENTES.TEL_RESIDENCIA,","," "))  ","
                            TRIM(REPLACE(CLIENTES.TEL_COMERCIAL,","," "))   ","
                            TRIM(REPLACE(CLIENTES.CELULAR,","," "))         ","
                            TRIM(CFECHA)                                    ","
                            TRIM(vcSexo)                                    ","
                            TRIM(REPLACE(CLIENTES.EMAIL,","," "))           ","
                            "" /* "COLOMBIA"     */  ","
                            "" /* "CUNDINAMARCA" */  ","
                            "" /* "BOGOTA"       */  ","
                            "" /* "COLOMBIA"     */  ","
                            "" /* "CUNDINAMARCA" */  ","
                            "" /* "BOGOTA"       */  ","
                            "0000"          ","
                            TRIM(cFechaHoy).
        END.
    OUTPUT CLOSE.

    OUTPUT TO VALUE(NOMCUENTAS).
        FOR EACH CLIENTES:
            IF fCedula <> "" THEN DO:
                IF clientes.nit <> fCedula THEN
                    NEXT.
            END.

            FIND FIRST creditos WHERE creditos.nit = clientes.nit
                                  AND creditos.COD_CREDITO = 123
                                  AND creditos.monto GT 0
                                  AND creditos.estado EQ 2 NO-LOCK NO-ERROR.
            IF AVAILABLE(creditos) THEN DO:
                cupo = (creditos.monto - creditos.sdo_capital) * 100.

                IF creditos.dias_atraso > 0 AND creditos.sdo_Capital > 0 THEN DO:
                    cupo = 0.

                    IF creditos.dias_atraso <= 3 THEN DO:
                        /* Para los 6 */
                        IF DAY(w_fecha) = 6 THEN DO:
                            FIND FIRST calendario WHERE calendario.dia = 5
                                                    AND calendario.mes = MONTH(w_fecha)
                                                    AND calendario.ano = YEAR(w_fecha)
                                                    AND calendario.habil = NO NO-LOCK NO-ERROR.
                            IF AVAILABLE calendario THEN
                                cupo = (creditos.monto - creditos.sdo_capital) * 100.
                        END.

                        /* Para los 7 */
                        IF DAY(w_fecha) = 7 THEN DO:
                            FIND FIRST calendario WHERE calendario.dia = 6
                                                    AND calendario.mes = MONTH(w_fecha)
                                                    AND calendario.ano = YEAR(w_fecha)
                                                    AND calendario.habil = NO NO-LOCK NO-ERROR.
                            IF AVAILABLE calendario THEN DO:
                                FIND FIRST calendario WHERE calendario.dia = 5
                                                        AND calendario.mes = MONTH(w_fecha)
                                                        AND calendario.ano = YEAR(w_fecha)
                                                        AND calendario.habil = NO NO-LOCK NO-ERROR.
                                IF AVAILABLE calendario THEN
                                    cupo = (creditos.monto - creditos.sdo_capital) * 100.
                            END.
                        END.

                        /* Para los 8 */
                        IF DAY(w_fecha) = 8 THEN DO:
                            FIND FIRST calendario WHERE calendario.dia = 7
                                                    AND calendario.mes = MONTH(w_fecha)
                                                    AND calendario.ano = YEAR(w_fecha)
                                                    AND calendario.habil = NO NO-LOCK NO-ERROR.
                            IF AVAILABLE calendario THEN DO:
                                FIND FIRST calendario WHERE calendario.dia = 6
                                                        AND calendario.mes = MONTH(w_fecha)
                                                        AND calendario.ano = YEAR(w_fecha)
                                                        AND calendario.habil = NO NO-LOCK NO-ERROR.
                                IF AVAILABLE calendario THEN DO:
                                    FIND FIRST calendario WHERE calendario.dia = 5
                                                            AND calendario.mes = MONTH(w_fecha)
                                                            AND calendario.ano = YEAR(w_fecha)
                                                            AND calendario.habil = NO NO-LOCK NO-ERROR.
                                    IF AVAILABLE calendario THEN
                                        cupo = (creditos.monto - creditos.sdo_capital) * 100.
                                END.
                            END.
                        END.
                    END.
                END.
                
                FIND FIRST bfrCreditos WHERE bfrCreditos.nit = clientes.nit
                                         AND bfrCreditos.sdo_capital > 0
                                         AND bfrcreditos.dias_atraso > 30 NO-LOCK NO-ERROR.
                IF AVAILABLE bfrCreditos THEN
                    cupo = 0.

                FIND FIRST listaNegra WHERE listaNegra.nit = clientes.nit
                                        AND listaNegra.codigo <> 5
                                        AND listaNegra.estado = 1 NO-LOCK NO-ERROR.
                IF AVAILABLE listaNegra THEN
                    cupo = 0.

                /* Validar crédito de solidaridad */
                /*FIND FIRST bfrCreditos WHERE bfrCreditos.nit = clientes.nit
                                         AND bfrCreditos.cod_credito = 158
                                         AND bfrCreditos.estado = 2 NO-LOCK NO-ERROR.
                IF AVAILABLE bfrCreditos THEN
                    cupo = 0.*/
                
                /*RUN validarUsuarioSarlaft.R (INPUT clientes.nit, OUTPUT W_Sarlaft) NO-ERROR.
                IF W_Sarlaft THEN
                     cupo = 0.*/

                IF cupo LT 0 THEN
                    cupo = 0.

                FIND FIRST tarjetas WHERE tarjetas.nit = creditos.nit
                                      AND tarjetas.num_credito = creditos.num_credito
                                      AND tarjetas.estado = '01' NO-LOCK NO-ERROR.
                IF AVAILABLE tarjetas THEN DO:
                    IF tarjetas.reportaTopesIndividual = YES THEN DO:
                        operacionesCajero = tarjetas.operMaxCaj.
                        operacionesPOS = tarjetas.operMaxPOS.
                        montoCajeros = tarjetas.montoMaxCaj.
                        montoPOS = tarjetas.montoMaxPOS.
                    END.
                    ELSE DO:
                        operacionesCajero = cfg_tarjetaDB.numeroRetirosCajeroDia.
                        operacionesPOS = cfg_tarjetaDB.numeroRetirosPOSDia.
                        montoCajeros = cfg_tarjetaDB.montoRetirosCajeroDia.
                        montoPOS = cfg_tarjetaDB.montoRetirosPOSDia.
                    END.

                    concta = concta + 1.

                    IF concta GT 1 THEN
                        PUT SKIP.
                
                    PUT UNFORMATTED "+"  ","
                                    "00000018" ","
                                    TRIM(creditos.nit) ","
                                    TRIM(STRING(creditos.num_credito)) ","
                                    "50" ","
                                    "1" ","
                                    "170" ","
                                    TRIM(STRING(creditos.monto * 100)) "," /*"1000000000" ","*/
                                    cupo /* FORMAT "->>>>>>>>>>>>" */ ","
                                    operacionesCajero ","
                                    operacionesPOS ","
                                    montoCajeros * 100  /* FORMAT "->>>>>>>>>>>>" */ ","
                                    montoPOS * 100  /* FORMAT "->>>>>>>>>>>>" */ ","
                                    TRIM(cFechaHoy).

                    FIND FIRST ahorros WHERE ahorros.nit = creditos.nit
                                         AND ahorros.cod_ahorro = 4
                                         AND ahorros.cue_ahorros = tarjetas.cue_ahorro
                                         AND ahorros.estado = 1 NO-LOCK NO-ERROR.
                    IF AVAILABLE ahorros THEN DO:
                        concta = concta + 1.

                        IF concta GT 1 THEN
                            PUT SKIP.

                        PUT UNFORMATTED "+"  ","
                                        "00000018" ","
                                        TRIM(creditos.nit) ","
                                        TRIM(ahorros.cue_ahorros) ","
                                        "10" ","
                                        "0" ","
                                        "170" ","
                                        TRIM(STRING((ahorros.sdo_disponible + ahorros.sdo_canje) * 100)) ","
                                        ahorros.sdo_disponible * 100 /* FORMAT "->>>>>>>>>>>>" */ ","
                                        operacionesCajero ","
                                        operacionesPOS ","
                                        montoCajeros * 100  /* FORMAT "->>>>>>>>>>>>" */ ","
                                        montoPOS * 100  /* FORMAT "->>>>>>>>>>>>" */ ","
                                        TRIM(cFechaHoy).
                    END.
                
                    ASSIGN viCntCupo = viCntCupo + (cupo / 100)
                           viTotMonto = viTotMonto + creditos.monto.
                END.

                FOR EACH creditos WHERE creditos.nit = clientes.nit
                                    AND creditos.COD_CREDITO = 123
                                    AND creditos.monto GT 0
                                    AND creditos.estado <> 2 NO-LOCK:
                    cupo = 0.

                    concta = concta + 1.

                    IF concta GT 1 THEN
                        PUT SKIP.

                    PUT UNFORMATTED "-"  ","
                                    "00000018" ","
                                    TRIM(creditos.nit) ","
                                    TRIM(STRING(creditos.num_credito)) ","
                                    "50" ","
                                    "0" ","
                                    "170" ","
                                    TRIM(STRING(creditos.monto * 100)) "," /*"1000000000" ","*/
                                    cupo /* FORMAT "->>>>>>>>>>>>" */ ","
                                    "0"  ","
                                    "0" ","
                                    0  /* FORMAT "->>>>>>>>>>>>" */ ","
                                    0  /* FORMAT "->>>>>>>>>>>>" */ ","
                                    TRIM(cFechaHoy).

                    FIND FIRST ahorros WHERE ahorros.nit = creditos.nit
                                         AND ahorros.cod_ahorro = 4
                                         AND ahorros.cue_ahorros = tarjetas.cue_ahorro
                                         AND ahorros.estado = 1 NO-LOCK NO-ERROR.
                    IF AVAILABLE ahorros THEN DO:
                        concta = concta + 1.

                        IF concta GT 1 THEN
                            PUT SKIP.

                        PUT UNFORMATTED "-" ","
                                        "00000018" ","
                                        TRIM(ahorros.nit) ","
                                        ahorros.cue_ahorros ","
                                        "10" ","
                                        "0" ","
                                        "170" ","
                                        TRIM(STRING((ahorros.sdo_disponible + ahorros.sdo_canje) * 100)) ","
                                        ahorros.sdo_disponible * 100 /* FORMAT "->>>>>>>>>>>>" */ ","
                                        "0"  ","
                                        "0" ","
                                        0  /* FORMAT "->>>>>>>>>>>>" */ ","
                                        0  /* FORMAT "->>>>>>>>>>>>" */ ","
                                        TRIM(cFechaHoy).
                    END.

                    ASSIGN viCntCupo = viCntCupo + (cupo / 100)
                           viTotMonto = viTotMonto + creditos.monto.
                END.
            END.
        END.
    OUTPUT CLOSE.

    OUTPUT TO VALUE(NOMTARJETA).
    OUTPUT CLOSE.

    OUTPUT TO VALUE(NOMTARVCTA).
    FOR EACH tarjetas NO-LOCK:
        IF tarjetas.estado = "01" THEN
            signo = "+".
        ELSE
            signo = "-".

        PUT UNFORMATTED signo                   ","
                        "00000018"              ","
                        TRIM(tarjetas.nit)      ","
                        tarjetas.tarjetaDB      ","
                        tarjetas.num_credito    ","
                        "50"                    ","
                        TRIM(cFechaHoy).

        contFCVA = contFCVA + 1.

        PUT SKIP.

        IF tarjetas.cue_ahorros <> "" AND tarjetas.cue_ahorros <> ? THEN DO:
            PUT UNFORMATTED signo                               ","
                            "00000018"                          ","
                            TRIM(tarjetas.nit)                  ","
                            SUBSTRING(tarjetas.tarjetaDB,1,16)  ","
                            tarjetas.cue_ahorros                ","
                            "10"                                ","
                            TRIM(cFechaHoy).

            contFCVA = contFCVA + 1.

            PUT SKIP.
        END.
    END.
    OUTPUT CLOSE.

    OUTPUT TO VALUE(NOMCONTROL).
        PUT UNFORMATTED substring(nomcliente,15) /* FORMAT "X(25)"  */ "," concli SKIP.
        PUT UNFORMATTED substring(nomcuentas,15) /* FORMAT "X(25)"  */ "," concta SKIP.
        PUT UNFORMATTED substring(nomtarjeta,15) /* FORMAT "X(25)"  */ "," 0 SKIP.
        PUT UNFORMATTED substring(nomtarvcta,15) /* FORMAT "X(25)"  */ "," contFCVA SKIP.
    OUTPUT CLOSE.

    OS-COMMAND SILENT VALUE(vcComando) a VALUE(vcNomFile7z) VALUE(vcRaiz) -r -pAF26BE74CD87AF23AC24AB65BA96DB12.

    ASSIGN W-TotSdo:SCREEN-VALUE = STRING(viTotMonto)
           W-TotDis:SCREEN-VALUE = STRING(viCntCupo)
           w-totreg:SCREEN-VALUE = STRING(concta)
           W-Mensaje:SCREEN-VALUE IN FRAME Frm-Main = '...Ha terminado el proceso, puede EXPORTAR'.

    IF fCedula = "" THEN DO:
        CREATE procDia.
        ASSIGN procDia.agencia = w_agencia
               procDia.cod_proceso = 14
               procDia.fecha_proc = w_fecha
               procDia.estado = 2
               procDia.usuario = w_usuario.
    END.
       
    MESSAGE "Proceso terminado" SKIP
            "Generado Archivo " SKIP
            vcNomFile7z
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-Visualizar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-Visualizar C-Win
ON CHOOSE OF Btn-Visualizar IN FRAME Frm-Main /* Button 2 */
DO:
    DEFINE VAR Listado AS CHARACTER INITIAL "".
     DEFINE VAR Tamano  AS INTEGER   INITIAL 2.
     listado = W_PathSpl + "L_Usuar.Lst".
     {Incluido\Imprimir.i "Listado" Tamano}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Done
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Done C-Win
ON CHOOSE OF Btn_Done IN FRAME Frm-Main /* Salir */
DO:
  &IF DEFINED (adm-panel) <> 0 &THEN
      RUN dispatch IN THIS-PROCEDURE ('exit').
  &ELSE
      APPLY "CLOSE":U TO THIS-PROCEDURE.
  &ENDIF
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fCedula
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fCedula C-Win
ON LEAVE OF fCedula IN FRAME Frm-Main /* Cédula (Reporte individual) */
DO:
    ASSIGN fCedula.

    FIND FIRST clientes WHERE clientes.nit = fCedula NO-LOCK NO-ERROR.
    IF AVAILABLE clientes THEN
        w-mensaje:SCREEN-VALUE = clientes.nombre + " " + clientes.apellido1 + " " + clientes.apellido2.
    ELSE DO:
        MESSAGE "Esta cédula no se encuentra matriculada en la Base de Datos de Asociados/Clientes. Revise por favor."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

        W-Mensaje:SCREEN-VALUE = "Proceso fallido...!!!".
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN _SetCurs.r ("Wait").
  RUN enable_UI.
  RUN inicializar_variables.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  DISPLAY w-totreg W-TotSdo W-TotDis fCedula w-Mensaje 
      WITH FRAME Frm-Main IN WINDOW C-Win.
  ENABLE RECT-345 B-Exportar Btn_Done fCedula 
      WITH FRAME Frm-Main IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-Frm-Main}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Inicializar_Variables C-Win 
PROCEDURE Inicializar_Variables :
DEFI VAR W-Sdomin  AS DECIMAL INITIAL 0.

/* Creacion de archivo Clientes.mov */
ASSIGN w-totreg:SCREEN-VALUE IN FRAME Frm-Main = "0"
       w-totreg
       w-totsdo:SCREEN-VALUE = "0"
       w-totsdo
       W-TotDis:SCREEN-VALUE = "0"
       w-totdis
       W-Mensaje:SCREEN-VALUE IN FRAME Frm-Main  = '...En proceso, espere un momento por Favor'.

RUN _SetCurs.r ("NoWait").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcesoImprimir C-Win 
PROCEDURE ProcesoImprimir :
/*------------------------------------------------------------------------------
  Autor: JOHN JAIRO MONCADA PUERTA    
  Cel  : 311 322 54 58 - Casa 253 49 71 
  Notes: diseñado bajo los parametros de Enpacto Administrativo      
------------------------------------------------------------------------------*/
 {INCLUIDO\RepEncabezado.I}.  
 DEFI VAR w-nomtipo  AS CHARACTER FORMAT "X(40)".
 FIND FIRST cfg_tarjetaDb NO-LOCK NO-ERROR.
 W_Reporte    = "REPORTE   : Exportación de Datos - " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am").
 W_EncColumna = "  Nro.Registros       Saldo Total  Saldo Disponible   Tipo de Exportación".
 VIEW FRAME F-Encabezado.
 VIEW FRAME F-Ftr.
    ASSIGN W-NomTipo = "Todos los Clientes".
 PUT "    " W-TotReg  FORMAT "->>,>>>,>>9"     "   "
            W-TotSdo  FORMAT "->>,>>>,>>>,>>9" "   "
            W-TotDis  FORMAT "->>,>>>,>>>,>>9" "   "
            W-NomTipo FORMAT "X(40)" SKIP(0).
 PAGE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

