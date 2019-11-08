&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME W-CentralesDeRiesgo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS W-CentralesDeRiesgo 
CREATE WIDGET-POOL.

ON RETURN TAB.

{Incluido/Variable.I "SHARED"}

DEFINE TEMP-TABLE cifin_txt
    FIELD registro AS CHARACTER.

DEFINE TEMP-TABLE cifin_csv
    FIELD tipoRegistro AS INTEGER
    FIELD tipoId AS INTEGER
    FIELD id AS CHARACTER
    FIELD apellido1 AS CHARACTER
    FIELD apellido2 AS CHARACTER
    FIELD nombre1 AS CHARACTER
    FIELD nombre2 AS CHARACTER
    FIELD num_credito AS INTEGER
    FIELD agencia AS INTEGER
    FIELD calidad AS CHARACTER
    FIELD calificacion AS CHARACTER
    FIELD situacion_titular AS INTEGER
    FIELD estado AS INTEGER
    FIELD edad_mora AS INTEGER
    FIELD years_mora AS INTEGER
    FIELD fec_corte AS DATE
    FIELD fec_inicial AS DATE
    FIELD fec_terminacion AS DATE
    FIELD fec_exigibilidad AS DATE
    FIELD fec_prescripcion AS DATE
    FIELD fec_pago AS DATE
    FIELD modo_extincion AS CHARACTER
    FIELD tipo_pago AS CHARACTER
    FIELD per_pago AS INTEGER
    FIELD probNoPago AS CHARACTER
    FIELD numCuotasPagas AS INTEGER
    FIELD numCuotasPactadas AS INTEGER
    FIELD cuotasEnMora AS INTEGER
    FIELD cupo AS INTEGER
    FIELD valorDeMora AS INTEGER
    FIELD saldo AS INTEGER
    FIELD valorCuota AS INTEGER
    FIELD valorCargoFijo AS CHARACTER INITIAL "            ".

DEFINE VAR deudor AS LOGICAL.

DEFINE BUFFER bfrRepcreditos FOR rep_creditos.

DEFINE VAR fechaAux AS DATE.
DEFINE VAR meses AS INTEGER.

DEFINE TEMP-TABLE datacredito
    FIELD registro AS CHARACTER.

DEFINE TEMP-TABLE datacredito_csv
    FIELD registro AS CHARACTER.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS cmbCentralDeRiesgo Btn_Imp wFecCorte 
&Scoped-Define DISPLAYED-OBJECTS cmbCentralDeRiesgo wFecCorte Msaje 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-2 Msaje 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-CentralesDeRiesgo AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Imp 
     IMAGE-UP FILE "imagenes/impresora2.bmp":U
     LABEL "Imprimir" 
     SIZE 10 BY 2.12.

DEFINE VARIABLE cmbCentralDeRiesgo AS CHARACTER FORMAT "X(25)":U 
     LABEL "Central de Riesgo" 
     VIEW-AS COMBO-BOX INNER-LINES 2
     LIST-ITEMS "CIFIN","DATACREDITO" 
     DROP-DOWN-LIST
     SIZE 22 BY 1 TOOLTIP "Centrales de Riesgo"
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE Msaje AS CHARACTER FORMAT "X(100)":U 
     VIEW-AS FILL-IN 
     SIZE 50 BY .77
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE wFecCorte AS DATE FORMAT "99/99/9999":U 
     LABEL "Fecha de corte" 
     VIEW-AS FILL-IN 
     SIZE 10.57 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     cmbCentralDeRiesgo AT ROW 1.54 COL 2.71
     Btn_Imp AT ROW 1.54 COL 42
     wFecCorte AT ROW 2.62 COL 28.29 COLON-ALIGNED WIDGET-ID 2
     Msaje AT ROW 3.96 COL 2 NO-LABEL
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 55.86 BY 5.62
         BGCOLOR 17 FGCOLOR 0 FONT 5.


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
  CREATE WINDOW W-CentralesDeRiesgo ASSIGN
         HIDDEN             = YES
         TITLE              = "Centrales de Riesgo"
         HEIGHT             = 3.96
         WIDTH              = 52
         MAX-HEIGHT         = 22.35
         MAX-WIDTH          = 114.29
         VIRTUAL-HEIGHT     = 22.35
         VIRTUAL-WIDTH      = 114.29
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = yes
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
/* SETTINGS FOR WINDOW W-CentralesDeRiesgo
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* SETTINGS FOR COMBO-BOX cmbCentralDeRiesgo IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN Msaje IN FRAME DEFAULT-FRAME
   NO-ENABLE ALIGN-L 2                                                  */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-CentralesDeRiesgo)
THEN W-CentralesDeRiesgo:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME DEFAULT-FRAME
/* Query rebuild information for FRAME DEFAULT-FRAME
     _Query            is NOT OPENED
*/  /* FRAME DEFAULT-FRAME */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-CentralesDeRiesgo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-CentralesDeRiesgo W-CentralesDeRiesgo
ON END-ERROR OF W-CentralesDeRiesgo /* Centrales de Riesgo */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-CentralesDeRiesgo W-CentralesDeRiesgo
ON WINDOW-CLOSE OF W-CentralesDeRiesgo /* Centrales de Riesgo */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Imp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Imp W-CentralesDeRiesgo
ON CHOOSE OF Btn_Imp IN FRAME DEFAULT-FRAME /* Imprimir */
DO:
    ASSIGN wFecCorte.

    IF cmbCentralDeRiesgo:SCREEN-VALUE IN FRAME DEFAULT-FRAME = "" OR cmbCentralDeRiesgo:SCREEN-VALUE IN FRAME DEFAULT-FRAME = ? THEN DO:
        MESSAGE "Debe escoger una Central de Riesgo para generar el archivo"
             VIEW-AS ALERT-BOX INFO BUTTONS OK.

        RETURN NO-APPLY.
    END.

    RUN VALUE(cmbCentralDeRiesgo:SCREEN-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME wFecCorte
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wFecCorte W-CentralesDeRiesgo
ON LEAVE OF wFecCorte IN FRAME DEFAULT-FRAME /* Fecha de corte */
DO:
    ASSIGN wFecCorte.

    IF DAY(wFecCorte + 1) <> 1 THEN DO:
        MESSAGE "Debe digitar una fecha de corte (Fin de mes)."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

        IF DAY(w_fecha + 1) = 1 THEN
            wFecCorte:SCREEN-VALUE = STRING(w_fecha,"99/99/9999").
        ELSE
            wFecCorte:SCREEN-VALUE = STRING(w_fecha - DAY(w_fecha),"99/99/9999").
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK W-CentralesDeRiesgo 


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
    RUN enable_UI.

    IF NOT THIS-PROCEDURE:PERSISTENT THEN
        WAIT-FOR CLOSE OF THIS-PROCEDURE.

    IF DAY(w_fecha + 1) = 1 THEN
        wFecCorte:SCREEN-VALUE = STRING(w_fecha,"99/99/9999").
    ELSE
        wFecCorte:SCREEN-VALUE = STRING(w_fecha - DAY(w_fecha),"99/99/9999").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CIFIN W-CentralesDeRiesgo 
PROCEDURE CIFIN :
/* 1 - Registro tipo 1 - Control */
CREATE cifin_txt.
/* Tipo de registro */  cifin_txt.registro = cifin_txt.registro + "1".
/* Tipo de paquete  */  cifin_txt.registro = cifin_txt.registro + "21". /* Sector Solidario - Cooperativo */
/* Tipo de entidad  */  cifin_txt.registro = cifin_txt.registro + "118". /* Fondos de empleados */
/* Código entidad   */  cifin_txt.registro = cifin_txt.registro + "153".
/* Reservado        */  cifin_txt.registro = cifin_txt.registro + "          ".
/* Tipo de reporte  */  cifin_txt.registro = cifin_txt.registro + "01". /* Reporte total */
/* Fecha de corte   */  cifin_txt.registro = cifin_txt.registro + STRING(YEAR(wFecCorte),"9999") + STRING(MONTH(wFecCorte),"99") + STRING(DAY(wFecCorte),"99").

/* 2 - Registros tipo 2 - Detalles */
FOR EACH rep_creditos WHERE rep_creditos.fecCorte = wFecCorte
                        AND (rep_creditos.estado = 2 OR (rep_creditos.estado = 3 AND rep_creditos.fec_canceTotal >= wFecCorte - DAY(wFecCorte) + 1)) NO-LOCK:
    FIND FIRST clientes WHERE clientes.nit = rep_creditos.nit NO-LOCK NO-ERROR.
    IF AVAILABLE clientes THEN DO:
        deudor = YES.

        RUN detalle.

        FOR EACH relaciones WHERE relaciones.Nit = rep_Creditos.Nit
                              AND INTEGER(relaciones.Cuenta) = rep_creditos.Num_credito
                              AND Relaciones.Clase_Producto = 2
                              AND Relaciones.Cod_Producto = rep_Creditos.Cod_Credito
                              AND Relaciones.Cod_Relacion = 11
                              AND Relaciones.Estado = 1
                              AND Relaciones.Aprobada = YES
                              AND relaciones.nit <> relaciones.nit_relacion
                              AND relaciones.nit_relacion <> "" NO-LOCK:
            FIND FIRST Clientes WHERE Clientes.Nit = Relaciones.Nit_Relacion NO-LOCK NO-ERROR.
            IF AVAILABLE(Clientes) THEN DO:
                deudor = NO.

                RUN detalle.
            END.
        END.
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE datacredito W-CentralesDeRiesgo 
PROCEDURE datacredito :
DEFINE VAR vNombreArchivo AS CHARACTER.
DEFINE BUFFER vRepCreditos FOR rep_creditos.

FIND FIRST cfg_creditos NO-LOCK NO-ERROR.

vNombreArchivo = cfg_creditos.codSuscriptorDatacredito + "." + STRING(YEAR(wFecCorte),"9999") + STRING(MONTH(wFecCorte),"99") + STRING(DAY(wFecCorte),"99")+ ".T".

/* MAESTRO DE CARTERA */

/* 1. Registro de control */
CREATE datacredito.
datacredito.registro = "HHHHHHHHHHHHHHHHHH" + /* Indicador del registro */
                       STRING(cfg_creditos.codSuscriptorDatacredito,"X(6)") + /* Código del suscriptor */
                       "08" + /* Tipo de cuenta */
                       STRING(YEAR(wFecCorte),"9999") + STRING(MONTH(wFecCorte),"99") + STRING(DAY(wFecCorte),"99") + /* Fecha de corte */
                       "M" + /* Ampliación milenio */
                       " " + /* Indicador de valores en miles */
                       "T" + /* Tipo de entrega */
                       "        " + /* Fecha de inicio reporte */
                       "        " + /* fecha de fin reporte */
                       "N" + /* Indicador de partir */
                       "00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000". /* Filler (746 0's) */

/* Créditos activos */
FOR EACH rep_creditos WHERE rep_creditos.fecCorte = wFecCorte NO-LOCK BREAK BY rep_creditos.nit:
    IF FIRST-OF(rep_creditos.nit) THEN
        FIND FIRST clientes WHERE clientes.nit = rep_creditos.nit NO-LOCK NO-ERROR.

    IF clientes.tipo_identificacion <> 'C.C' AND
       clientes.tipo_identificacion <> 'C.E' AND
       clientes.tipo_identificacion <> 'NIT' THEN
        NEXT.

    CREATE datacredito.

    /* Tipo de identificaciòn */
    CASE clientes.tipo_identificacion:
        WHEN 'C.C' THEN datacredito.registro = datacredito.registro + '1'.
        WHEN 'NIT' THEN datacredito.registro = datacredito.registro + '2'.
        WHEN 'C.E' THEN datacredito.registro = datacredito.registro + '3'.
    END CASE.

    /* Número de identificación */
    datacredito.registro = datacredito.registro + STRING(DECIMAL(rep_creditos.nit),"99999999999").

    /* Número de cuenta u obligación */
    datacredito.registro = datacredito.registro + STRING(rep_creditos.num_credito,"999999999999999999").

    /* Nombre completo del titular */
    datacredito.registro = datacredito.registro + STRING(clientes.nombre + clientes.apellido1 + clientes.apellido2, "X(45)").

    /* Situación del titular */
    datacredito.registro = datacredito.registro + "0". /* Normal */

    /* Fecha de apertura */
    datacredito.registro = datacredito.registro + STRING(YEAR(rep_creditos.fec_desembolso),"9999") + STRING(MONTH(rep_creditos.fec_desembolso),"99") + STRING(DAY(rep_creditos.fec_desembolso),"99").

    /* Fecha de vencimiento */
    FOR EACH CONTROL_pagos WHERE CONTROL_pagos.nit = rep_creditos.nit AND CONTROL_pagos.num_credito = rep_creditos.num_credito NO-LOCK BY fec_vcto DESC:
        datacredito.registro = datacredito.registro + STRING(YEAR(CONTROL_pagos.fec_vcto),"9999") + STRING(MONTH(CONTROL_pagos.fec_vcto),"99") + STRING(DAY(CONTROL_pagos.fec_vcto),"99").
        LEAVE.
    END.

    /* Responsable o calidad del deudor */
    datacredito.registro = datacredito.registro + "00". /* Principal */

    /* Tipo de obligación */
    CASE rep_creditos.tip_credito:
        WHEN 1 THEN datacredito.registro = datacredito.registro + "1". /* Consumo */
        WHEN 3 THEN datacredito.registro = datacredito.registro + "3". /* Hipotecario */
    END CASE.

    /* Subsidio hipotecario */
    datacredito.registro = datacredito.registro + "0". /* No */

    /* Fecha subsidio */
    datacredito.registro = datacredito.registro + "        ".

    /* Término del contrato que genera la obligación */
    IF rep_creditos.cod_credito = 123 THEN
        datacredito.registro = datacredito.registro + "2". /* Indefinido */
    ELSE
        datacredito.registro = datacredito.registro + "1". /* Definido */

    /* Forma de pago */
    IF rep_creditos.dias_atraso = 0 THEN
        datacredito.registro = datacredito.registro + "0". /* No pagada - Vigente */

    IF rep_creditos.abogado = TRUE THEN
        datacredito.registro = datacredito.registro + "2". /* Proceso ejecutivo */

    IF rep_creditos.estado = 5 THEN
        datacredito.registro = datacredito.registro + "9". /* Cuenta insoluta */

    /* Periodicidad de pago */
    CASE rep_creditos.per_pago:
        WHEN 1 THEN datacredito.registro = datacredito.registro + "9". /* Otro tipo */
        WHEN 2 THEN datacredito.registro = datacredito.registro + "9". /* Otro tipo */
        WHEN 3 THEN datacredito.registro = datacredito.registro + "9". /* Otro tipo */
        WHEN 4 THEN datacredito.registro = datacredito.registro + "1". /* Mensual */
        WHEN 5 THEN datacredito.registro = datacredito.registro + "2". /* Bimestral */
        WHEN 6 THEN datacredito.registro = datacredito.registro + "3". /* Trimestral */
        WHEN 7 THEN datacredito.registro = datacredito.registro + "9". /* Otro tipo */
        WHEN 8 THEN datacredito.registro = datacredito.registro + "4". /* Semestral */
        WHEN 9 THEN datacredito.registro = datacredito.registro + "5". /* Anual */
    END CASE.

    IF rep_creditos.plazo = 1 THEN
        datacredito.registro = datacredito.registro + "6". /* Al vencimiento */

    /* Novedad */
    IF rep_creditos.estado <> 5 THEN DO:
        IF rep_creditos.dias_atraso < 30 THEN
            datacredito.registro = datacredito.registro + "01". /* Al día */
        ELSE DO:
            IF rep_credito.dias_atraso < 60 THEN
                datacredito.registro = datacredito.registro + "06". /* Mora de 30 días */
            ELSE DO:
                IF rep_credito.dias_atraso < 90 THEN
                    datacredito.registro = datacredito.registro + "07". /* Mora de 60 días */
                ELSE DO:
                    IF rep_credito.dias_atraso < 120 THEN
                        datacredito.registro = datacredito.registro + "08". /* Mora de 90 días */
                    ELSE
                        datacredito.registro = datacredito.registro + "09". /* Mora de 120 días o más */
                END.
            END.
        END.
    END.
    ELSE
        datacredito.registro = datacredito.registro + "13". /* Cartera Castigada */

    /* Estado origen de la cuenta */
    datacredito.registro = datacredito.registro + "0". /* Normal - Creación por apertura */


    /* Fecha estado origen */
    datacredito.registro = datacredito.registro + STRING(YEAR(rep_creditos.fec_desembolso),"9999") + STRING(MONTH(rep_creditos.fec_desembolso),"99") + STRING(DAY(rep_creditos.fec_desembolso),"99").

    /* Estado de la cuenta */
    IF rep_creditos.estado <> 5 THEN DO:
        IF rep_creditos.dias_atraso = 0 THEN
            datacredito.registro = datacredito.registro + "01". /* Al día */
        ELSE
            datacredito.registro = datacredito.registro + "02". /* En mora */
    END.
    ELSE
        datacredito.registro = datacredito.registro + "06". /* Castigada */

    /* Fecha de Estado */
    IF rep_creditos.estado <> 5 THEN DO:
        IF rep_creditos.dias_atraso = 0 THEN
            datacredito.registro = datacredito.registro + STRING(YEAR(wFecCorte),"9999") + STRING(MONTH(wFecCorte),"99") + STRING(DAY(wFecCorte),"99"). /* Al día */
        ELSE
            datacredito.registro = datacredito.registro + STRING(YEAR(rep_creditos.fec_pago),"9999") + STRING(MONTH(rep_creditos.fec_pago),"99") + STRING(DAY(rep_creditos.fec_pago),"99"). /* En mora */
    END.
    ELSE
        datacredito.registro = datacredito.registro + STRING(YEAR(rep_creditos.fec_canceTotal),"9999") + STRING(MONTH(rep_creditos.fec_canceTotal),"99") + STRING(DAY(rep_creditos.fec_canceTotal),"99"). /* Castigada */

    /* Estado del plástico - Sólo aplica para Tarjetas de Crédito */
    datacredito.registro = datacredito.registro + " ".

    /* Fecha Estado del Plástico - Sólo aplica para Tarjetas de Crédito */
    datacredito.registro = datacredito.registro + "        ".

    /* Adjetivo */
    IF clientes.fec_fallecido <> ? THEN
        datacredito.registro = datacredito.registro + "1".  /* Fallecido */
    ELSE DO:
        IF rep_creditos.abogado = TRUE THEN
            datacredito.registro = datacredito.registro + "7".  /* Cobro jurídico */
        ELSE
            datacredito.registro = datacredito.registro + "0".  /* Sin adjetivo */
    END.

    /* Fecha Adjetivo */
    IF clientes.fec_fallecido <> ? THEN
        datacredito.registro = datacredito.registro + STRING(YEAR(clientes.fec_fallecido),"9999") + STRING(MONTH(clientes.fec_fallecido),"99") + STRING(DAY(clientes.fec_fallecido),"99").  /* Fallecido */
    ELSE DO:
        IF rep_creditos.abogado = TRUE THEN
            datacredito.registro = datacredito.registro + "7".  /* Cobro jurídico */
        ELSE
            datacredito.registro = datacredito.registro + "00000000".  /* Sin adjetivo */
    END.

    /* Clase de tarjeta */
    datacredito.registro = datacredito.registro + "0".

    /* Franquicia */
    datacredito.registro = datacredito.registro + "0".

    /* Nombre marca privada */
    datacredito.registro = datacredito.registro + "                              ".

    /* Tipo de moneda */
    datacredito.registro = datacredito.registro + "1".  /* Legal */

    /* Tipo de garantía */
    IF rep_creditos.tip_credito = 3 THEN
        datacredito.registro = datacredito.registro + "1".
    ELSE
        datacredito.registro = datacredito.registro + "2".

    /* Calificación */
    datacredito.registro = datacredito.registro + rep_creditos.categoriaMes + " ".

    /* Probabilidad de incumplimiento */
    datacredito.registro = datacredito.registro + "000".

    /* Edad de mora */
    IF rep_creditos.dias_atraso > 999 THEN
        datacredito.registro = datacredito.registro + "999".
    ELSE
        datacredito.registro = datacredito.registro + STRING(rep_creditos.dias_atraso,"999").

    /* Valor inicial */
    datacredito.registro = datacredito.registro + STRING(rep_creditos.monto,"99999999999").

    /* Saldo deuda */
    datacredito.registro = datacredito.registro + STRING(rep_creditos.sdo_capital,"99999999999").

    /* Valor disponible */
    IF rep_creditos.monto - rep_creditos.sdo_capital > 0 THEN
        datacredito.registro = datacredito.registro + STRING(rep_creditos.monto - rep_creditos.sdo_capital,"99999999999").
    ELSE
        datacredito.registro = datacredito.registro + "00000000000".

    /* Valor cuota mensual */
    CASE rep_credito.per_pago:
        WHEN 1 THEN datacredito.registro = datacredito.registro + STRING(rep_creditos.cuota * 4,"99999999999").
        WHEN 2 THEN datacredito.registro = datacredito.registro + STRING(rep_creditos.cuota * 3,"99999999999").
        WHEN 3 THEN datacredito.registro = datacredito.registro + STRING(rep_creditos.cuota * 2,"99999999999").
        WHEN 4 THEN datacredito.registro = datacredito.registro + STRING(rep_creditos.cuota,"99999999999").
        WHEN 5 THEN datacredito.registro = datacredito.registro + STRING(rep_creditos.cuota / 2,"99999999999").
        WHEN 6 THEN datacredito.registro = datacredito.registro + STRING(rep_creditos.cuota / 3,"99999999999").
        WHEN 7 THEN datacredito.registro = datacredito.registro + STRING(rep_creditos.cuota / 4,"99999999999").
        WHEN 8 THEN datacredito.registro = datacredito.registro + STRING(rep_creditos.cuota / 6,"99999999999").
        WHEN 9 THEN datacredito.registro = datacredito.registro + STRING(rep_creditos.cuota * 12,"99999999999").
    END CASE.
END.

/* Créditos cancelados */
FOR EACH creditos WHERE creditos.estado = 3
                    AND MONTH(creditos.fec_canceTotal) = MONTH(wFecCorte)
                    AND YEAR(creditos.fec_canceTotal) = YEAR(wFecCorte) NO-LOCK BREAK BY creditos.nit:

END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Detalle W-CentralesDeRiesgo 
PROCEDURE Detalle :
DEFINE BUFFER bfrCreditos FOR rep_creditos.

DEFINE VAR perPago AS CHARACTER.
DEFINE VAR vCuotasPagadas AS INTEGER.
DEFINE VAR vCuotasAtrasadas AS INTEGER.

CREATE cifin_txt.
CREATE cifin_csv.

cifin_txt.registro = cifin_txt.registro + "2". /* Tipo de registro */
cifin_csv.tipoRegistro = 2.

RUN tipoId. /* Tipo de identificación */

cifin_txt.registro = cifin_txt.registro + STRING(clientes.nit,"X(15)"). /* Número de identificación */
cifin_csv.id = clientes.nit.

RUN nombre. /* Nombre */

cifin_txt.registro = cifin_txt.registro + "          ". /* Reservado */

cifin_txt.registro = cifin_txt.registro + STRING(rep_creditos.num_credito,"X(20)"). /* Número de crédito */
cifin_csv.num_credito = rep_creditos.num_credito.

cifin_txt.registro = cifin_txt.registro + STRING(rep_creditos.agencia,"X(6)"). /* Agencia */
cifin_csv.agencia = rep_creditos.agencia.

IF deudor = YES THEN DO: /* Calidad */
    cifin_txt.registro = cifin_txt.registro + "P".
    cifin_csv.calidad = "P".
END.
ELSE DO:
    cifin_txt.registro = cifin_txt.registro + "C".
    cifin_csv.calidad = "C".
END.

IF rep_creditos.estado = 2 THEN DO: /* Calificación */
    cifin_txt.registro = cifin_txt.registro + rep_creditos.categoriaMes.
    cifin_csv.calificacion = rep_creditos.categoriaMes.
END.
ELSE DO:
    IF rep_creditos.estado = 3 THEN DO:
        cifin_txt.registro = cifin_txt.registro + "A".
        cifin_csv.calificacion = "A".
    END.
END.

cifin_txt.registro = cifin_txt.registro + "06". /* Situación o estado del titular */
cifin_csv.situacion_titular = 6.

/* Estado */
IF rep_creditos.estado = 2 THEN DO:
    cifin_txt.registro = cifin_txt.registro + "01". /* Vigente */
    cifin_csv.estado = 1.

    IF rep_creditos.categoriaMes = "D" THEN DO: /* Difícil Cobro */
        cifin_txt.registro = cifin_txt.registro + "04".
        cifin_csv.estado = 4.
    END.

    IF rep_creditos.categoriaMes = "E" THEN DO: /* Irrecuperable */
        cifin_txt.registro = cifin_txt.registro + "05".
        cifin_csv.estado = 5.
    END.
END.
ELSE DO:
    IF rep_creditos.estado = 1 THEN DO:
        cifin_txt.registro = cifin_txt.registro + "01". /* Contingencia */
        cifin_csv.estado = 1.
    END.

    IF rep_creditos.estado = 3 THEN DO:
        cifin_txt.registro = cifin_txt.registro + "07". /* Saldado */
        cifin_csv.estado = 7.

        FIND FIRST bfrRepCreditos WHERE bfrRepCreditos.fecCorte = wFecCorte - DAY(wFecCorte)
                                    AND bfrRepCreditos.nit = rep_creditos.nit
                                    AND bfrRepCreditos.num_credito = rep_creditos.num_credito
                                    AND bfrRepCreditos.categoriaMes >= "D" NO-LOCK NO-ERROR.
        IF AVAILABLE bfrRepCreditos THEN DO:
            cifin_txt.registro = cifin_txt.registro + "08". /* Recuperado */
            cifin_csv.estado = 8.
        END.
    END.
END.

/* Edad de mora */
IF rep_creditos.dias_atraso <= 29 THEN DO:
    cifin_txt.registro = cifin_txt.registro + "00".
    cifin_csv.edad_mora = 0.
END.
ELSE DO:
    IF rep_creditos.dias_atraso >= 30 AND rep_creditos.dias_atraso <= 59 THEN DO:
        cifin_txt.registro = cifin_txt.registro + "01".
        cifin_csv.edad_mora = 1.
    END.
    ELSE DO:
        IF rep_creditos.dias_atraso >= 60 AND rep_creditos.dias_atraso <= 89 THEN DO:
            cifin_txt.registro = cifin_txt.registro + "02".
            cifin_csv.edad_mora = 2.
        END.
        ELSE DO:
            IF rep_creditos.dias_atraso >= 90 AND rep_creditos.dias_atraso <= 119 THEN DO:
                cifin_txt.registro = cifin_txt.registro + "03".
                cifin_csv.edad_mora = 3.
            END.
            ELSE DO:
                IF rep_creditos.dias_atraso >= 120 AND rep_creditos.dias_atraso <= 149 THEN DO:
                    cifin_txt.registro = cifin_txt.registro + "04".
                    cifin_csv.edad_mora = 4.
                END.
                ELSE DO:
                    IF rep_creditos.dias_atraso >= 150 AND rep_creditos.dias_atraso <= 179 THEN DO:
                        cifin_txt.registro = cifin_txt.registro + "05".
                        cifin_csv.edad_mora = 5.
                    END.
                    ELSE DO:
                        IF rep_creditos.dias_atraso >= 180 AND rep_creditos.dias_atraso <= 209 THEN DO:
                            cifin_txt.registro = cifin_txt.registro + "06".
                            cifin_csv.edad_mora = 6.
                        END.
                        ELSE DO:
                            IF rep_creditos.dias_atraso >= 210 AND rep_creditos.dias_atraso <= 239 THEN DO:
                                cifin_txt.registro = cifin_txt.registro + "07".
                                cifin_csv.edad_mora = 7.
                            END.
                            ELSE DO:
                                IF rep_creditos.dias_atraso >= 240 AND rep_creditos.dias_atraso <= 269 THEN DO:
                                    cifin_txt.registro = cifin_txt.registro + "08".
                                    cifin_csv.edad_mora = 8.
                                END.
                                ELSE DO:
                                    IF rep_creditos.dias_atraso >= 270 AND rep_creditos.dias_atraso <= 299 THEN DO:
                                        cifin_txt.registro = cifin_txt.registro + "09".
                                        cifin_csv.edad_mora = 9.
                                    END.
                                    ELSE DO:
                                        IF rep_creditos.dias_atraso >= 300 AND rep_creditos.dias_atraso <= 329 THEN DO:
                                            cifin_txt.registro = cifin_txt.registro + "10".
                                            cifin_csv.edad_mora = 10.
                                        END.
                                        ELSE DO:
                                            IF rep_creditos.dias_atraso >= 330 AND rep_creditos.dias_atraso <= 359 THEN DO:
                                                cifin_txt.registro = cifin_txt.registro + "11".
                                                cifin_csv.edad_mora = 11.
                                            END.
                                            ELSE DO:
                                                IF rep_creditos.dias_atraso >= 360 AND rep_creditos.dias_atraso <= 539 THEN DO:
                                                    cifin_txt.registro = cifin_txt.registro + "12".
                                                    cifin_csv.edad_mora = 12.
                                                END.
                                                ELSE DO:
                                                    IF rep_creditos.dias_atraso >= 540 AND rep_creditos.dias_atraso <= 729 THEN DO:
                                                        cifin_txt.registro = cifin_txt.registro + "13".
                                                        cifin_csv.edad_mora = 13.
                                                    END.
                                                    ELSE DO:
                                                        IF rep_creditos.dias_atraso >= 730 THEN DO:
                                                            cifin_txt.registro = cifin_txt.registro + "14".
                                                            cifin_csv.edad_mora = 14.
                                                        END.
                                                    END.
                                                END.
                                            END.
                                        END.
                                    END.
                                END.
                            END.
                        END.
                    END.
                END.
            END.
        END.
    END.
END.

/* Años de mora */
cifin_txt.registro = cifin_txt.registro + STRING(TRUNCATE(rep_creditos.dias_atraso / 360,0),"99").
cifin_csv.years_mora = TRUNCATE(rep_creditos.dias_atraso / 360,0).

/* Fecha corte */
cifin_txt.registro = cifin_txt.registro + STRING(YEAR(wFecCorte),"9999") + STRING(MONTH(wFecCorte),"99") + STRING(DAY(wFecCorte),"99").
cifin_csv.fec_corte = wFecCorte.

/* Fecha inicial */
cifin_txt.registro = cifin_txt.registro + STRING(YEAR(rep_creditos.fec_desembolso),"9999") + STRING(MONTH(rep_creditos.fec_desembolso),"99") + STRING(DAY(rep_creditos.fec_desembolso),"99").
cifin_csv.fec_inicial = rep_creditos.fec_desembolso.

/* Fecha de terminación */
IF rep_creditos.per_pago = 1 OR rep_creditos.per_pago = 2 THEN DO:
    CASE rep_Creditos.Per_Pago:
        WHEN 1 THEN fechaAux = rep_Creditos.Fec_Desembolso + (rep_Creditos.Plazo * 7).
        WHEN 2 THEN fechaAux = rep_Creditos.Fec_Desembolso + (rep_Creditos.Plazo * 10).
    END.
END.
ELSE DO:
    CASE rep_creditos.per_pago:
        WHEN 3 THEN meses = ROUND(rep_Creditos.Plazo * 15 / 30,0).
        WHEN 4 THEN meses = ROUND(rep_Creditos.Plazo * 30 / 30,0).
        WHEN 5 THEN meses = ROUND(rep_Creditos.Plazo * 60 / 30,0).
        WHEN 6 THEN meses = ROUND(rep_Creditos.Plazo * 90 / 30,0).
        WHEN 7 THEN meses = ROUND(rep_Creditos.Plazo * 120 / 30,0).
        WHEN 8 THEN meses = ROUND(rep_Creditos.Plazo * 180 / 30,0).
        WHEN 9 THEN meses = ROUND(rep_Creditos.Plazo * 360 / 30,0).
    END CASE.

    fechaAux = ADD-INTERVAL(rep_creditos.fec_desembolso, meses, "months").
END.

cifin_txt.registro = cifin_txt.registro + STRING(YEAR(fechaAux),"9999") + STRING(MONTH(fechaAux),"99") + STRING(DAY(fechaAux),"99").
cifin_csv.fec_terminacion = fechaAux.

/* Fecha de ixigibilidad */
cifin_txt.registro = cifin_txt.registro + STRING(YEAR(fechaAux),"9999") + STRING(MONTH(fechaAux),"99") + STRING(DAY(fechaAux),"99").
cifin_csv.fec_exigibilidad = fechaAux.

/* Fecha de prescripción */
cifin_txt.registro = cifin_txt.registro + "        ".

/* Fecha de pago */
IF cifin_csv.estado = 8 THEN DO:
    cifin_txt.registro = cifin_txt.registro + STRING(YEAR(rep_Creditos.Fec_ultPago),"9999") + STRING(MONTH(rep_Creditos.Fec_ultPago),"99") + STRING(DAY(rep_Creditos.Fec_ultPago),"99").
    cifin_csv.fec_pago = rep_creditos.fec_ultPago.
END.
ELSE
    cifin_txt.registro = cifin_txt.registro + "        ".

/* Modo de extinción */
IF cifin_csv.estado = 8 THEN DO:
    cifin_txt.registro = cifin_txt.registro + "01".
    cifin_csv.modo_extincion = "1".
END.
ELSE
    cifin_txt.registro = cifin_txt.registro + "  ".

/* Tipo de pago */
IF cifin_csv.estado = 8 THEN DO:
    cifin_txt.registro = cifin_txt.registro + "01".
    cifin_csv.tipo_pago = "1".
END.
ELSE
    cifin_txt.registro = cifin_txt.registro + "  ".

/* Periodicidad de pago */
CASE rep_creditos.per_pago:
    WHEN 1 THEN perPago = "01".
    WHEN 2 THEN perPago = "23".
    WHEN 3 THEN perPago = "04".
    WHEN 4 THEN perPago = "07".
    WHEN 5 THEN perPago = "10".
    WHEN 6 THEN perPago = "13".
    WHEN 7 THEN perPago = "23".
    WHEN 8 THEN perPago = "16".
    WHEN 9 THEN perPago = "19".
END CASE.

cifin_txt.registro = cifin_txt.registro + perPago.
cifin_csv.per_pago = INTEGER(perPago).

/* Probabilidad de No Pago */
cifin_txt.registro = cifin_txt.registro + "   ".

/* Número de cuotas pagas */
IF rep_creditos.estado = 3 THEN
    vCuotasPagadas = 0.
ELSE
    vCuotasPagadas = creditos.plazo.

cifin_txt.registro = cifin_txt.registro + STRING(vCuotasPagadas,"999").
cifin_csv.numCuotasPagas = vCuotasPagadas.

/* Número de cuotas pactadas */
cifin_txt.registro = cifin_txt.registro + STRING(rep_creditos.plazo,"999").
cifin_csv.numCuotasPactadas = rep_creditos.plazo.

/* Cuotas en mora */
IF rep_creditos.estado = 3 THEN
    vCuotasAtrasadas = 0.
ELSE
    vCuotasAtrasadas = creditos.cuo_atraso.

cifin_txt.registro = cifin_txt.registro + STRING(vCuotasAtrasadas,"999").
cifin_csv.cuotasEnMora = vCuotasAtrasadas.

/* Valor o Cupo */
cifin_txt.registro = cifin_txt.registro + STRING(rep_Creditos.Monto / 1000,"999999999999").
cifin_csv.cupo = rep_creditos.monto.

/* Valor de mora */
IF rep_creditos.dias_atraso > 0 THEN DO:
    cifin_txt.registro = cifin_txt.registro + STRING(ROUND(rep_creditos.val_atraso / 1000,0),"999999999999").
    cifin_csv.valorDeMora = ROUND(creditos.val_atraso / 1000,0).
END.
ELSE
    cifin_txt.registro = cifin_txt.registro + "000000000000".

/* Valor de Saldo*/
cifin_txt.registro = cifin_txt.registro + STRING(ROUND(creditos.sdo_capital / 1000,0),"999999999999").
cifin_csv.saldo = ROUND(creditos.sdo_capital / 1000,0).

/* Valor de la cuota */
cifin_txt.registro = cifin_txt.registro + STRING(ROUND(creditos.cuota / 1000,0),"999999999999").
cifin_csv.valorCuota = ROUND(creditos.cuota / 1000,0).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI W-CentralesDeRiesgo  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-CentralesDeRiesgo)
  THEN DELETE WIDGET W-CentralesDeRiesgo.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI W-CentralesDeRiesgo  _DEFAULT-ENABLE
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
  DISPLAY cmbCentralDeRiesgo wFecCorte Msaje 
      WITH FRAME DEFAULT-FRAME IN WINDOW W-CentralesDeRiesgo.
  ENABLE cmbCentralDeRiesgo Btn_Imp wFecCorte 
      WITH FRAME DEFAULT-FRAME IN WINDOW W-CentralesDeRiesgo.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW W-CentralesDeRiesgo.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE nombre W-CentralesDeRiesgo 
PROCEDURE nombre :
IF clientes.tipo_identificacion = "C.C" OR
   clientes.tipo_identificacion = "C.E" OR
   clientes.tipo_identificacion = "C.I" OR
   clientes.tipo_identificacion = "PAS" THEN DO:
    cifin_txt.registro = cifin_txt.registro + string(clientes.apellido1,"X(15)") + string(clientes.apellido2,"X(15)").
    cifin_csv.apellido1 = clientes.apellido1.
    cifin_csv.apellido2 = clientes.apellido2.

    IF INDEX(clientes.nombre," ") > 0 THEN DO:
        cifin_txt.registro = cifin_txt.registro + string(SUBSTRING(clientes.nombre,1,INDEX(clientes.nombre," ") - 1),"X(15)") + string(SUBSTRING(clientes.nombre,INDEX(clientes.nombre," ") + 1),"X(15)").
        cifin_csv.nombre1 = SUBSTRING(clientes.nombre,1,INDEX(clientes.nombre," ") - 1).
        cifin_csv.nombre2 = SUBSTRING(clientes.nombre,INDEX(clientes.nombre," ") + 1).
    END.
END.
ELSE DO:
    cifin_txt.registro = cifin_txt.registro + clientes.nombre.
    cifin_csv.nombre1 = clientes.nombre.
    ASSIGN cifin_txt.registro = cifin_txt.registro + " " + clientes.apellido1 WHEN clientes.apellido1 <> "".
    ASSIGN cifin_csv.nombre1 = cifin_csv.nombre1 + " " + clientes.apellido1 WHEN clientes.apellido1 <> "".
    ASSIGN cifin_txt.registro = cifin_txt.registro + " " + clientes.apellido2 WHEN clientes.apellido2 <> "".
    ASSIGN cifin_csv.nombre1 = cifin_csv.nombre1 + " " + clientes.apellido2 WHEN clientes.apellido2 <> "".
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcesoImprimir W-CentralesDeRiesgo 
PROCEDURE ProcesoImprimir :
{Incluido\RepEncabezado.I}

DISPLAY STRING(W_Nom_Entidad,"X(40)") FORMAT "X(100)" SKIP
        "INFORME DE INTEGRIDAD - PRODUCTOS Vs. CONTABILIDAD" SKIP
        "Fecha:" STRING(W_fecha,"99/99/9999") FORM "X(80)" SKIP
        "Agencia:" string(w_agencia, "X(80)") SKIP
        "--------------------------------------------------------------------------------------------------------" SKIP(1)
    WITH WIDTH 150 NO-LABELS.

RUN Ahorros.
RUN Creditos.
RUN InteresCorriente.
RUN interesMora.
RUN InteresContingente.
RUN Provision.
RUN ProvisionInteres.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tipoId W-CentralesDeRiesgo 
PROCEDURE tipoId :
CASE clientes.tipo_id:
    WHEN "C.C" THEN
        ASSIGN cifin_txt.registro = cifin_txt.registro + "01"
               cifin_csv.tipoId = 1.

    WHEN "NIT" THEN
        ASSIGN cifin_txt.registro = cifin_txt.registro + "02"
               cifin_csv.tipoId = 2.

    WHEN "C.E" THEN
        ASSIGN cifin_txt.registro = cifin_txt.registro + "03"
               cifin_csv.tipoId = 3.

    WHEN "T.I" THEN
        ASSIGN cifin_txt.registro = cifin_txt.registro + "04"
               cifin_csv.tipoId = 4.

    WHEN "PAS" THEN
        ASSIGN cifin_txt.registro = cifin_txt.registro + "05"
               cifin_csv.tipoId = 5.

    WHEN "TSSE" THEN
        ASSIGN cifin_txt.registro = cifin_txt.registro + "06"
               cifin_csv.tipoId = 6.

    WHEN "SESN" THEN
        ASSIGN cifin_txt.registro = cifin_txt.registro + "07"
               cifin_csv.tipoId = 7.

    WHEN "FID" THEN
        ASSIGN cifin_txt.registro = cifin_txt.registro + "08"
               cifin_csv.tipoId = 8.

    WHEN "RC" THEN
        ASSIGN cifin_txt.registro = cifin_txt.registro + "09"
               cifin_csv.tipoId = 9.

    WHEN "CD" THEN
        ASSIGN cifin_txt.registro = cifin_txt.registro + "10"
               cifin_csv.tipoId = 10.
END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

