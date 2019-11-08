&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME Wwin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Wwin 
/*------------------------------------------------------------------------

  File: 

  Description:  INFORMES HABITUALES EN SUBGERENCIA (MENSUALMENTE)

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author:   JOHN JAIRO MONCADA PUERTA

  Created: 7 DE MAYO DE 2005

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
{Incluido\VARIABLE.I "SHARED"}
/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
DEFINE VAR Listado AS CHARACTER INITIAL "".
DEFINE VAR j       AS INTEGER.
DEFINE VAR k       AS INTEGER.    
DEFINE VAR w_ok     AS LOGICAL.

DEFINE VARIABLE WImagen  AS CHARACTER FORMAT "X(40)". 
DEFINE VARIABLE WAno     AS INTEGER FORMAT "9999".    
DEFINE VARIABLE WMes     AS INTEGER FORMAT "99".      
DEFINE VARIABLE WFec     AS DATE.   
DEFINE VARIABLE FecIni   AS DATE.
DEFINE VARIABLE FecFin   AS DATE.
DEFINE VARIABLE MesIni   AS INTEGER FORMAT "99".
DEFINE VARIABLE MesFin   AS INTEGER FORMAT "99".
DEFINE VARIABLE AgeIni   AS INTEGER FORMAT "999" INITIAL 0.
DEFINE VARIABLE AgeFin   AS INTEGER FORMAT "999" INITIAL 999.

DEFINE TEMP-TABLE tmpgral
    FIELD Tagencia  LIKE ahorros.agencia
    FIELD Tnit      LIKE ahorros.nit
    FIELD Tnombre   AS CHARACTER FORMAT "X(40)"
    FIELD TDep      AS DECIMAL   FORMAT "zzz,zzz,zz9" INITIAL 0.00
    FIELD TCte      AS DECIMAL   FORMAT "zzz,zzz,zz9" INITIAL 0.00
    FIELD TSem      AS DECIMAL   FORMAT "zzz,zzz,zz9" INITIAL 0.00
    FIELD Ttac      AS DECIMAL   FORMAT "zzz,zzz,zz9" INITIAL 0.00
    FIELD TCDAT     AS DECIMAL   FORMAT "zzz,zzz,zz9" INITIAL 0.00
    FIELD TICDAT    AS DECIMAL   FORMAT "zzz,zzz,zz9" INITIAL 0.00
    FIELD TAportes  AS DECIMAL   FORMAT "zzz,zzz,zz9" INITIAL 0.00
    FIELD TDDep     AS CHARACTER FORMAT "X(60)"
    FIELD TDCte     AS CHARACTER FORMAT "X(60)"
    FIELD TDSem     AS CHARACTER FORMAT "X(60)"
    FIELD TDtac     AS CHARACTER FORMAT "X(60)"
    FIELD TDCDAT    AS CHARACTER FORMAT "X(60)"
    FIELD TDICDAT   AS CHARACTER FORMAT "X(60)"
    FIELD TDAportes AS CHARACTER FORMAT "X(60)"
    INDEX idxtmp Tagencia Tnit ASCEND.

DEFINE TEMP-TABLE tmpCDAT
    FIELD Testado     LIKE ahorros.estado    
    FIELD Tcodage     LIKE ahorros.agencia
    FIELD TnomAge     LIKE agencias.nombre    
    FIELD Tnit        LIKE ahorros.nit    
    FIELD TNombre     AS CHARACTER FORMAT "X(40)"
    FIELD TCueaho     LIKE ahorros.cue_ahorros
    FIELD TFecApe     LIKE ahorros.Fec_Apertura
    FIELD TFecVen     LIKE ahorros.Fec_Vencimiento
    FIELD TFecCan     LIKE ahorros.Fec_Cancelacion
    FIELD TFecPro     LIKE ahorros.Fec_Prorroga
    FIELD Ttasa       LIKE ahorros.tasa    
    FIELD Tplazo      LIKE ahorros.plazo    
    FIELD Tperliq     LIKE ahorros.per_liquidacion
    FIELD Tmonto      LIKE ahorros.monto_apertura
    FIELD Tsaldo      LIKE ahorros.sdo_disponible
    INDEX IdxAgeESt Tcodage Testado TfecPro.

DECLARE InfCDAt CURSOR FOR
   SELECT a.estado          FORMAT "9",                                                            
          b.agencia         FORMAT "z9",
          b.nombre          FORMAT "X(15)",                                                        
          a.nit             FORMAT "X(12)",                                                        
          TRIM(c.apellido1) + " " + TRIM(c.apellido2) + " " + TRIM(c.nombre) FORMAT "X(40)",       
          a.cue_ahorros     FORMAT "X(8)",                                                         
          a.fec_apertura    FORMAT "99/99/9999",                                                   
          a.fec_vencimiento FORMAT "99/99/9999",                                                   
          a.fec_cancelacion FORMAT "99/99/9999",                                                   
          a.fec_prorroga    FORMAT "99/99/9999",                                                   
          a.tasa            FORMAT "z9.9999",                                                      
          a.plazo           FORMAT "z,zz9",                                                        
          a.per_liquidacion FORMAT "zz9",                                                          
          a.monto_apertura  FORMAT "zzz,zzz,zz9",                                                  
          (a.sdo_disponible + a.sdo_canje) FORMAT "zzz,zzz,zz9"                                    
       FROM  ahorros a, clientes c, agencias b                                                     
       WHERE a.agencia          = b.agencia                                AND 
             a.nit = c.nit AND cod_ahorro = 4                              AND
            (a.agencia         >= ageini AND a.agencia         <= agefin ) AND
            (a.fec_apertura    >= Fecini AND a.fec_apertura    <= Fecfin   OR
             a.fec_cancelacion >= Fecini AND a.fec_cancelacion <= Fecfin   OR
             a.fec_prorroga    >= FecIni AND a.Fec_Prorroga    <= Fecfin ) 
       ORDER BY a.agencia, a.estado, a.fec_prorroga, a.nit                                         
            WITH FRAME j WIDTH 210 NO-LABELS.

 DEFINE TEMP-TABLE tmpCartEdad
     FIELD agencia    LIKE creditos.agencia
     FIELD edad       AS   INTEGER INITIAL 0
     FIELD Cantidad   AS   INTEGER INITIAL 0
     FIELD sdocapital AS   DECIMAL INITIAL 0.00
     FIELD provision  AS   DECIMAL INITIAL 0.00
     INDEX idxedad Edad Agencia.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FrmGerencia

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Img_MesF Img_MesI RECT-282 RECT-283 RECT-291 ~
RECT-292 BUTTON-120 BUTTON-121 Cmb_Agencias R_InfGral BUTTON-3 BUTTON-2 ~
BtnDone 
&Scoped-Define DISPLAYED-OBJECTS Cmb_Agencias W_DiaIni AnoIni W_DiaFin ~
AnoFin R_InfGral 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR Wwin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BtnDone DEFAULT 
     LABEL "&Salir" 
     SIZE 13 BY 1.62
     BGCOLOR 8 .

DEFINE BUTTON BUTTON-120 
     IMAGE-UP FILE "imagenes/arwdwn.gif":U
     LABEL "Button 120" 
     SIZE 3 BY .54.

DEFINE BUTTON BUTTON-121 
     IMAGE-UP FILE "imagenes/arwdwn.gif":U
     LABEL "Button 121" 
     SIZE 4 BY .54.

DEFINE BUTTON BUTTON-2 
     IMAGE-UP FILE "imagenes/impresora2.bmp":U
     LABEL "Button 2" 
     SIZE 13 BY 1.62.

DEFINE BUTTON BUTTON-3 
     IMAGE-UP FILE "imagenes/informacion.bmp":U
     LABEL "Button 3" 
     SIZE 13 BY 1.62.

DEFINE BUTTON BUTTON-6 
     IMAGE-UP FILE "imagenes/lupa.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/lupa.bmp":U
     LABEL "Button 6" 
     SIZE 13 BY 1.62.

DEFINE VARIABLE Cmb_Agencias AS CHARACTER FORMAT "X(40)":U INITIAL "000 - Todas" 
     VIEW-AS COMBO-BOX INNER-LINES 15
     LIST-ITEMS "000 - Todas" 
     DROP-DOWN-LIST
     SIZE 27 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE AnoFin AS INTEGER FORMAT "9999":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 6 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE AnoIni AS INTEGER FORMAT "9999":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 6 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_DiaFin AS INTEGER FORMAT "99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 4 BY .81 NO-UNDO.

DEFINE VARIABLE W_DiaIni AS DECIMAL FORMAT "99":U INITIAL 1 
     VIEW-AS FILL-IN 
     SIZE 4 BY .81 NO-UNDO.

DEFINE IMAGE Img_MesF
     FILENAME "adeicon/blank":U
     SIZE 11 BY .81.

DEFINE IMAGE Img_MesI
     FILENAME "adeicon/blank":U
     SIZE 11 BY .81.

DEFINE VARIABLE R_InfGral AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Productos de Ahorros -Aperturas y Cancelaciones", 1,
"Créditos -Cancelados y Precancelados", 2,
"CDAT vigentes, Renovados y Cancelados", 3,
"Cartera - Vencimientos por Fechas", 4,
"Ingreso de Asociados", 5,
"Retiro de Asociados", 6,
"Cartera clasificada x dia mora (Provisión)", 7
     SIZE 49 BY 6.46 TOOLTIP "Informe Consolidados" NO-UNDO.

DEFINE RECTANGLE RECT-282
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 22 BY 1.88.

DEFINE RECTANGLE RECT-283
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 23 BY 1.88.

DEFINE RECTANGLE RECT-291
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 15 BY 7.

DEFINE RECTANGLE RECT-292
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 51 BY 7.04.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FrmGerencia
     BUTTON-120 AT ROW 1.54 COL 49.57
     BUTTON-121 AT ROW 1.54 COL 71
     Cmb_Agencias AT ROW 2.08 COL 1 COLON-ALIGNED NO-LABEL
     W_DiaIni AT ROW 2.19 COL 31.72 NO-LABEL
     AnoIni AT ROW 2.19 COL 44.72 COLON-ALIGNED NO-LABEL
     W_DiaFin AT ROW 2.19 COL 54.29 NO-LABEL
     AnoFin AT ROW 2.19 COL 67.29 COLON-ALIGNED NO-LABEL
     R_InfGral AT ROW 3.65 COL 3 NO-LABEL
     BUTTON-3 AT ROW 3.85 COL 62.57
     BUTTON-2 AT ROW 5.42 COL 62.57
     BUTTON-6 AT ROW 7.04 COL 62.57
     BtnDone AT ROW 8.62 COL 62.57
     "Agencias:" VIEW-AS TEXT
          SIZE 18 BY .81 AT ROW 1.27 COL 3
     "Fecha de Corte" VIEW-AS TEXT
          SIZE 15 BY .62 AT ROW 1.42 COL 54.72
          BGCOLOR 17 FGCOLOR 7 
     "Fecha Inicial" VIEW-AS TEXT
          SIZE 14.72 BY .69 AT ROW 1.38 COL 32
          BGCOLOR 17 FGCOLOR 7 
     Img_MesF AT ROW 2.19 COL 58.29
     Img_MesI AT ROW 2.19 COL 35.72
     RECT-282 AT ROW 1.27 COL 31
     RECT-283 AT ROW 1.27 COL 53.29
     RECT-291 AT ROW 3.46 COL 61.29
     RECT-292 AT ROW 3.42 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 79 BY 12.54
         BGCOLOR 17 .


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
  CREATE WINDOW Wwin ASSIGN
         HIDDEN             = YES
         TITLE              = "Informes Contraloria - Winformes_Contraloria.r"
         COLUMN             = 20
         ROW                = 6.85
         HEIGHT             = 9.58
         WIDTH              = 76.72
         MAX-HEIGHT         = 20.77
         MAX-WIDTH          = 114.29
         VIRTUAL-HEIGHT     = 20.77
         VIRTUAL-WIDTH      = 114.29
         MAX-BUTTON         = no
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = 17
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
/* SETTINGS FOR WINDOW Wwin
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME FrmGerencia
                                                                        */
/* SETTINGS FOR FILL-IN AnoFin IN FRAME FrmGerencia
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN AnoIni IN FRAME FrmGerencia
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON BUTTON-6 IN FRAME FrmGerencia
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_DiaFin IN FRAME FrmGerencia
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN W_DiaIni IN FRAME FrmGerencia
   NO-ENABLE ALIGN-L                                                    */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(Wwin)
THEN Wwin:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Wwin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Wwin Wwin
ON END-ERROR OF Wwin /* Informes Contraloria - Winformes_Contraloria.r */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Wwin Wwin
ON WINDOW-CLOSE OF Wwin /* Informes Contraloria - Winformes_Contraloria.r */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnDone
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnDone Wwin
ON CHOOSE OF BtnDone IN FRAME FrmGerencia /* Salir */
DO:
  &IF "{&PROCEDURE-TYPE}" EQ "SmartPanel" &THEN
    &IF "{&ADM-VERSION}" EQ "ADM1.1" &THEN
      RUN dispatch IN THIS-PROCEDURE ('exit').
    &ELSE
      RUN exitObject.
    &ENDIF
  &ELSE
      APPLY "CLOSE":U TO THIS-PROCEDURE.
  &ENDIF
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-120
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-120 Wwin
ON CHOOSE OF BUTTON-120 IN FRAME FrmGerencia /* Button 120 */
DO:
  RUN C-Fecha.r (OUTPUT WImagen, OUTPUT WAno, OUTPUT WMes, OUTPUT WFec).
  ASSIGN W_OK = Img_MesI:LOAD-IMAGE("imagenes\month" + STRING(WMes) + ".gif")
         AnoIni:SCREEN-VALUE = STRING(WAno)
         W_DiaIni:SCREEN-VALUE = STRING(DAY(WFec))
         AnoIni = WAno
         MesIni = WMes
         FecIni = WFec.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-121
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-121 Wwin
ON CHOOSE OF BUTTON-121 IN FRAME FrmGerencia /* Button 121 */
DO:
  RUN C-Fecha.r (OUTPUT WImagen, OUTPUT WAno, OUTPUT WMes, OUTPUT WFec).
  ASSIGN W_OK = Img_MesF:LOAD-IMAGE("imagenes\month" + STRING(WMes) + ".gif")
         W_DiaFin:SCREEN-VALUE = STRING(DAY(WFec))
         AnoFin:SCREEN-VALUE = STRING(WAno)
         AnoFin = WAno
         MesFin = WMes
         FecFin = WFec.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-2 Wwin
ON CHOOSE OF BUTTON-2 IN FRAME FrmGerencia /* Button 2 */
DO:
  VIEW FRAME f_progreso.
  IF ((fecini = ? ) OR (fecfin = ?)) AND INTEGER(R_InfGral:SCREEN-VALUE) LT 7  THEN DO: 
    MESSAGE " Ingrese Fecha inicial y fecha final para esta consulta" VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN.    
  END.
  CASE R_InfGral:SCREEN-VALUE:
      WHEN "1" THEN
          RUN InfAhorro_Apert_Canc.

      WHEN "2" THEN
          RUN InfCreditos_Canc_Precanc.

      WHEN "3" THEN
          RUN InfCDAT.

      WHEN "4" THEN
          RUN InfCreditos_FecVcto.

      WHEN "5" THEN
          RUN InfAsociados_Ingresos.
      
      WHEN "6" THEN
          RUN InfAsociados_Retiros.
          
      WHEN "7" THEN
          RUN InfCartera_Clasificada.

  END CASE.
  HIDE FRAME f_progreso.
  IF Listado EQ "" THEN 
     MESSAGE "No se ha ejecutado ningún informe" VIEW-AS ALERT-BOX.
  ELSE
  DO:
    DEFINE VAR W_Dispositivo AS CHARACTER INITIAL " ".
    DEFINE VAR W_sw          AS LOGICAL. 
    RUN P-DisPos IN W_Manija (INPUT-OUTPUT Listado,INPUT-OUTPUT W_Dispositivo).
    IF W_Dispositivo = "" THEN
      RETURN.
    IF W_Dispositivo = "P" THEN  
      RUN Pantalla IN W_Manija (INPUT Listado).
    ELSE                                                  
      IF W_Dispositivo = "I" THEN
          RUN adecomm/_osprint.r ( INPUT  ?, INPUT Listado,INPUT  ?,INPUT  1,INPUT  1,
                                          INPUT  99999,OUTPUT W_sw).
    IF W_Dispositivo = "E" THEN
      RUN Imprimir_Excel.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-3 Wwin
ON CHOOSE OF BUTTON-3 IN FRAME FrmGerencia /* Button 3 */
DO:
  RUN W-InfDia.r NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Cmb_Agencias
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_Agencias Wwin
ON VALUE-CHANGED OF Cmb_Agencias IN FRAME FrmGerencia
DO:
  ASSIGN FRAME FrmGerencia Cmb_Agencias.
  IF SUBSTRING(Cmb_Agencias,1,3) EQ "000" THEN
     ASSIGN AgeIni = 0 AgeFin = 999.
  ELSE
     ASSIGN AgeIni = INTEGER(SUBSTRING(Cmb_Agencias,1,3)) AgeFin = INTEGER(SUBSTRING(Cmb_Agencias,1,3)).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Wwin 


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

  /* Al inicio del  programa y una sola vez */
  DO WITH FRAME FrmGerencia:     
     FOR EACH Agencias WHERE Agencias.Estado EQ 1 NO-LOCK:
        W_Ok = Cmb_Agencias:ADD-LAST(STRING(Agencias.Agencia,"999") + " - " + Agencias.Nombre).
        IF Agencias.Agencia EQ W_Agencia THEN
           Cmb_Agencias:SCREEN-VALUE = STRING(Agencias.Agencia,"999") + " - " + Agencias.Nombre.
     END.
  END.



  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Wwin  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(Wwin)
  THEN DELETE WIDGET Wwin.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Wwin  _DEFAULT-ENABLE
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
  DISPLAY Cmb_Agencias W_DiaIni AnoIni W_DiaFin AnoFin R_InfGral 
      WITH FRAME FrmGerencia IN WINDOW Wwin.
  ENABLE Img_MesF Img_MesI RECT-282 RECT-283 RECT-291 RECT-292 BUTTON-120 
         BUTTON-121 Cmb_Agencias R_InfGral BUTTON-3 BUTTON-2 BtnDone 
      WITH FRAME FrmGerencia IN WINDOW Wwin.
  {&OPEN-BROWSERS-IN-QUERY-FrmGerencia}
  VIEW Wwin.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InfAhorro_Apert_Canc Wwin 
PROCEDURE InfAhorro_Apert_Canc :
/*------------------------------------------------------------------------------
   Autor: JOHN J. MONCADA PUERTA
   Actualizacion: 6 de septiembre de 2005
   Objetivo:  Reportar cuentas de ahorros aperturadas y Canceladas
   Solicitud: Conmtraloria .
------------------------------------------------------------------------------*/
RUN _SetCurs.r ("WAIT").
Listado = W_PathSpl + "InfAhoApeCanc-" + W_Usuario + ".Lst".
OS-DELETE VALUE(Listado).
FOR EACH tmpgral:     
  DELETE tmpgral.
END.
DEFINE VAR mens AS CHARACTER FORMAT "X(150)".
DEFINE VAR cont AS INTEGER INITIAL 0.
DEFINE VAR zsw  AS LOGICAL INITIAL TRUE.
DEFINE VAR zant LIKE ahorros.nit.
    
FOR EACH ahorros WHERE ahorros.agencia      GE ageini AND ahorros.agencia LE agefin AND
                      (ahorros.Fec_apertura GE FecIni AND Fec_Apertura LE FecFin    OR  
                       ahorros.Fec_Cancela  GE FecIni AND Fec_Cancela  LE FecIni)   NO-LOCK BREAK BY ahorros.agencia BY ahorros.nit:
   IF FIRST-OF(ahorros.nit) THEN DO:
      ASSIGN cont = cont + 1. 
      CREATE tmpgral.
      zsw = FALSE.
   END.
   FIND FIRST clientes WHERE Clientes.nit = ahorros.nit NO-LOCK NO-ERROR.
   ASSIGN tmpgral.Tagencia = Ahorros.agencia
          tmpgral.Tnit     = Ahorros.nit 
          tmpgral.Tnombre  = TRIM(Clientes.apellido1) + " " + TRIM(Clientes.apellido2) + " " + TRIM(Clientes.nombre).
   IF Clientes.estado = 1 THEN
      IF fec_apertura NE ? THEN
         mens = 'A-' + STRING(fec_apertura) + '-'.
      ELSE
         mens = 'A-' + "SinFec" + '-'.
   ELSE
      IF fec_retiro NE ? THEN
        mens = 'I-' + STRING(fec_retiro) + '-'.
      ELSE
        mens = 'I-' + "SinFec" + '-'.

   CASE cod_ahorro:
       WHEN  1 THEN                                    
             ASSIGN tmpgral.TDdep     = tmpgral.TDdep        + mens + "Dp.My:" + cue_ahorros + ', '
                    tmpgral.Tdep      = TDep + (sdo_disponible + sdo_canje).
       WHEN  2 THEN
             ASSIGN tmpgral.TDcte     = tmpgral.TDCte        + mens + "Cte:"   + cue_ahorros + ', '
                    tmpgral.TCte      = TCte + (sdo_disponible + sdo_canje).
       WHEN  3 THEN
             ASSIGN tmpgral.TDsem     = tmpgral.TDsem        + mens + "Sem:"   + cue_ahorros + ', '
                    tmpgral.TSem      = TSem + (sdo_disponible + sdo_canje).
       WHEN  4 THEN
             ASSIGN tmpgral.TDcdat    = tmpgral.TDcdat       + mens + "CDAT:"  + cue_ahorros + ', '
                    tmpgral.TCDAT     = TCDAT + (sdo_disponible + sdo_canje). 
       WHEN  5 THEN
             ASSIGN tmpgral.TDaportes = tmpgral.TDAportes    + mens + "ApoMy:" + cue_ahorros + ', '
                    tmpgral.TAportes  = TAportes + (sdo_disponible + sdo_canje). 
       WHEN  7 THEN
             ASSIGN tmpgral.TDaportes = tmpgral.TDaportes    + mens + "ApoDf:" + cue_ahorros + ', '
                    tmpgral.TAportes  = TAportes + (sdo_disponible + sdo_canje). 
       WHEN  9 THEN
             ASSIGN tmpgral.TDdep     = tmpgral.TDdep        + mens + "DepMe:" + cue_ahorros + ', '
                    tmpgral.Tdep      = TDep + (sdo_disponible + sdo_canje).
       WHEN 10 THEN
             ASSIGN tmpgral.TDaportes = tmpgral.TDaportes    + mens + "Apome:" + cue_ahorros + ', '
                    tmpgral.TAportes  = TAportes + (sdo_disponible + sdo_canje). 
       WHEN 11 THEN
             ASSIGN tmpgral.TDICDAT   = tmpgral.TDICDAT      + mens + "ICDAT:" + cue_ahorros + ', '
                    tmpgral.TICDAT    = TICDAT + (sdo_disponible + sdo_canje). 
       WHEN 12 THEN
             ASSIGN tmpgral.TDTac     = tmpgral.TDTac        + mens + "TAC:"   + cue_ahorros + ', '
                    tmpgral.TTAC      = TTAC + (sdo_disponible + sdo_canje). 
       END CASE.
END.
/*END.*/

cont = 0.
OUTPUT TO value(Listado) NO-ECHO PAGED PAGE-SIZE 65.
{Incluido\RepEncabezado.i}
W_Reporte    = "REPORTE   : Ahorros - Aperturas y Cancelaciones - Agencia: " + TRIM(SUBSTRING(Cmb_Agencias,6,15)) + " Del " + STRING(FecIni,"99/99/9999") + " Al " + STRING(FecFin,"99/99/9999").
W_EncColumna = " #Asoc. Nit          Nombres Completos                          Depositos   Creciente    Semillas      T A C         CDAT     IntCDAT     Aportes Apertura/Cancelación DEPOSITOS                               Apertura/Cancelación CRECIENTE                               Apertura/Cancelación SEMILLAS                                Apertura/Cancelación   T A C                                  Apertura/Cancelación  C D A T                               Apertura/Cancelación INTERES CDAT                            Apertura/Cancelación APORTES".
VIEW FRAME F-Encabezado.
DEFINE VAR Wsaldos  AS DECIMAL EXTENT 7 INITIAL 0.
DEFINE VAR WTsaldos AS DECIMAL EXTENT 7 INITIAL 0.
FOR EACH Tmpgral BREAK BY tmpgral.Tagencia:

    IF FIRST-OF(Tmpgral.Tagencia) THEN DO:
       FIND agencias WHERE agencias.agencia EQ tmpgral.Tagencia NO-LOCK NO-ERROR.
       PUT " Agencia: " CAPS(TRIM(Agencias.nombre)) SKIP(1).
       ASSIGN Wsaldos = 0  cont = 0.
    END.

    cont = cont + 1.
    PUT cont       FORMAT "zzz,zz9"      " "
        Tnit       FORMAT "X(12)"        " "       
        Tnombre    FORMAT "X(40)"        " "        
        TDep       FORMAT "zzz,zzz,zz9"  " "         
        TCte       FORMAT "zzz,zzz,zz9"  " " 
        TSem       FORMAT "zzz,zzz,zz9"  " " 
        Ttac       FORMAT "zzz,zzz,zz9"  " " 
        TCDAT      FORMAT "zzz,zzz,zz9"  " " 
        TICDAT     FORMAT "zzz,zzz,zz9"  " " 
        TAportes   FORMAT "zzz,zzz,zz9"  " "      
        TDDep      FORMAT "X(60)" " "      
        TDCte      FORMAT "X(60)" " "      
        TDSem      FORMAT "X(60)" " "      
        TDtac      FORMAT "X(60)" " "      
        TDCDAT     FORMAT "X(60)" " "      
        TDICDAT    FORMAT "X(60)" " "      
        TDAportes  FORMAT "X(60)" SKIP.

    ASSIGN  Wsaldos[1] =  Wsaldos[1] + TDep     
            Wsaldos[2] =  Wsaldos[2] + TCte     
            Wsaldos[3] =  Wsaldos[3] + TSem     
            Wsaldos[4] =  Wsaldos[4] + Ttac     
            Wsaldos[5] =  Wsaldos[5] + TCDAT    
            Wsaldos[6] =  Wsaldos[6] + TICDAT   
            Wsaldos[7] =  Wsaldos[7] + TAportes 
           WTsaldos[1] = WTsaldos[1] + TDep    
           WTsaldos[2] = WTsaldos[2] + TCte    
           WTsaldos[3] = WTsaldos[3] + TSem    
           WTsaldos[4] = WTsaldos[4] + Ttac    
           WTsaldos[5] = WTsaldos[5] + TCDAT   
           WTsaldos[6] = WTsaldos[6] + TICDAT  
           WTsaldos[7] = WTsaldos[7] + TAportes.

    IF LAST-OF(Tmpgral.Tagencia) THEN DO:
       PUT "                                                              -----------------------------------------------------------------------------------" SKIP(0).
       PUT "                             Totales Agencia:  " CAPS(TRIM(Agencias.nombre)) FORMAT "X(15)" Wsaldos[1] FORMAT "zzz,zzz,zz9"  " "
                          Wsaldos[2] FORMAT "zzz,zzz,zz9"  " "
                          Wsaldos[3] FORMAT "zzz,zzz,zz9"  " "
                          Wsaldos[4] FORMAT "zzz,zzz,zz9"  " "
                          Wsaldos[5] FORMAT "zzz,zzz,zz9"  " "
                          Wsaldos[6] FORMAT "zzz,zzz,zz9"  " "
                          Wsaldos[7] FORMAT "zzz,zzz,zz9" SKIP(3).
    END.
END.
PUT "                                                              -----------------------------------------------------------------------------------" SKIP(0).
PUT "                      T O T A L E S   G E N E R A L E S       " Wsaldos[1] FORMAT "zzz,zzz,zz9"  " "
                   WTsaldos[2] FORMAT "zzz,zzz,zz9"  " "
                   WTsaldos[3] FORMAT "zzz,zzz,zz9"  " "
                   WTsaldos[4] FORMAT "zzz,zzz,zz9"  " "
                   WTsaldos[5] FORMAT "zzz,zzz,zz9"  " "
                   WTsaldos[6] FORMAT "zzz,zzz,zz9"  " "
                   WTsaldos[7] FORMAT "zzz,zzz,zz9".
OUTPUT CLOSE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InfAsociados_Ingresos Wwin 
PROCEDURE InfAsociados_Ingresos :
/*------------------------------------------------------------------------------   
   PROPOSITO  : Reporte de ingreso de asociados apoyado en el manejo de los aportes
   AUTOR      :  JOHN  MONCADA PUERTA
   ACTAULIZADO: 9 septiembre de 2005      
------------------------------------------------------------------------------*/ 
 RUN _SetCurs.r ("WAIT").
 Listado = W_PathSpl + "InfAhoApeCanc-" + W_Usuario + ".Lst".
 OS-DELETE VALUE(Listado).
 OUTPUT TO value(Listado) NO-ECHO PAGED PAGE-SIZE 65.
 {Incluido\RepEncabezado.i}
 W_Reporte    = "REPORTE   : Ingreso de Asociados -  Agencia: " + TRIM(SUBSTRING(Cmb_Agencias,6,15)) + " Del " + STRING(FecIni,"99/99/9999") + " Al " + STRING(FecFin,"99/99/9999").
 W_EncColumna = "#Asoc. Nro.Identif  Nombres Completos                          Apertura     Aportes".
 VIEW FRAME F-Encabezado.
    
 DEFINE VAR wconteo AS INTEGER INITIAL 0.
 DEFINE VAR wsaldo  AS DECIMAL INITIAL 0.
 DEFINE VAR wTsaldo AS DECIMAL INITIAL 0.
 FOR EACH ahorros WHERE Ahorros.Tip_Ahorro       EQ 4 AND 
                        ahorros.estado           EQ 1       AND
                        ahorros.fec_cancelacion  EQ ? AND
                        ahorros.fec_apertura     GE fecini AND ahorros.fec_apertura LE fecfin AND
                        ahorros.agencia          GE ageini AND ahorros.agencia      LE agefin AND
                        (ahorros.sdo_disponible + ahorros.sdo_canje) GT 0 NO-LOCK BREAK BY ahorros.agencia BY ahorros.fec_apertura:
    IF FIRST-OF(Ahorros.agencia) THEN DO:
       FIND agencias WHERE agencias.agencia = ahorros.agencia NO-LOCK NO-ERROR.
       PUT " Agencia: " CAPS(TRIM(agencias.nombre)) FORMAT "X(15)" SKIP(1).
       ASSIGN wsaldo = 0 wconteo = 0.
    END.

    wconteo = wconteo + 1.
    wsaldo  = wsaldo + (sdo_disponible + sdo_canje).
    wtsaldo = wtsaldo + (sdo_disponible + sdo_canje).
    FIND clientes WHERE clientes.nit     = ahorros.nit      NO-LOCK NO-ERROR.
    PUT    wconteo           FORMAT "zzz,zz9" " "
           ahorros.nit FORMAT "X(12)" " "
           trim(clientes.apellido1) + " " + trim(clientes.apellido2) + " " + trim(clientes.nombre) FORMAT "X(40)" " "
           ahorros.fec_apertura FORMAT "99/99/9999" " "
           sdo_disponible + sdo_canje FORMAT "zz,zzz,zz9" SKIP(0).

    IF LAST-OF(ahorros.agencia) THEN DO:
       PUT "                                            =======================================" SKIP(0).
       PUT "   Total Agencia " CAPS(TRIM(agencias.nombre)) FORMAT "X(55)"  wsaldo FORMAT "zzz,zzz,zz9"  SKIP(2).
    END.
 END.
 PUT "                                            =======================================" SKIP(0).
 PUT " TOTALES GENERALES " FORMAT "X(72)"  wTsaldo FORMAT "zzz,zzz,zz9"  SKIP(2).
 OUTPUT CLOSE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InfAsociados_Retiros Wwin 
PROCEDURE InfAsociados_Retiros :
/*------------------------------------------------------------------------------   
   PROPOSITO  : Reporte de retiro de asociados apoyado en el manejo de los aportes
   AUTOR      :  JOHN  MONCADA PUERTA
   ACTAULIZADO: 9 septiembre de 2005     
------------------------------------------------------------------------------*/  
 RUN _SetCurs.r ("WAIT").
 Listado = W_PathSpl + "InfAhoApeCanc-" + W_Usuario + ".Lst".
 OS-DELETE VALUE(Listado).
 OUTPUT TO value(Listado) NO-ECHO PAGED PAGE-SIZE 65.
 {Incluido\RepEncabezado.i}
 W_Reporte    = "REPORTE   : Retiro de Asociados -  Agencia: " + TRIM(SUBSTRING(Cmb_Agencias,6,15)) + " Del " + STRING(FecIni,"99/99/9999") + " Al " + STRING(FecFin,"99/99/9999").
 W_EncColumna = "#Numero Nro.Identif  Nombres Completos                     Fec_Ret-Aportes  Saldo Apo".
 VIEW FRAME F-Encabezado.

 DEFINE VAR wconteo AS INTEGER INITIAL 0.
 FOR EACH ahorros WHERE Ahorros.Tip_Ahorro EQ 4 AND 
                        ahorros.fec_cancelacion  GE fecini AND ahorros.fec_cancelacion LE fecfin AND 
                        ahorros.agencia          GE ageini AND ahorros.agencia         LE agefin AND 
                        (sdo_disponible + sdo_canje) LE 0 NO-LOCK BREAK BY ahorros.agencia BY ahorros.fec_cancelacion:
  IF FIRST-OF(ahorros.agencia) THEN DO:
     FIND agencias WHERE agencias.agencia EQ ahorros.agencia  NO-LOCK NO-ERROR.
     PUT " Agencia: " CAPS(TRIM(agencias.nombre)) FORMAT "X(15)" SKIP(1).
  END.

  FIND clientes WHERE clientes.nit     EQ ahorros.nit     /* AND clientes.fec_retiro NE ? */ NO-LOCK NO-ERROR.
  IF AVAILABLE(clientes) THEN DO:
     wconteo = wconteo + 1.
     PUT wconteo FORMAT "zzz,zz9" " "
         ahorros.nit     FORMAT "X(12)"  " "
         trim(clientes.apellido1) +  " " + trim(clientes.apellido2) + " " + trim(clientes.nombre) FORMAT "X(40)" " "
         ahorros.fec_cancelacion    FORMAT "99/99/9999" " "
        (ahorros.sdo_disponible + ahorros.sdo_canje) FORMAT "zz,zzz,zz9" SKIP(0).
  END.
  ELSE
      MESSAGE "No se encontro la cedula " ahorros.nit
          VIEW-AS ALERT-BOX INFO BUTTONS OK.

  IF LAST-OF(ahorros.agencia) THEN
     PUT " " SKIP(2).
 END.
 OUTPUT CLOSE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InfCartera_Clasificada Wwin 
PROCEDURE InfCartera_Clasificada :
/*------------------------------------------------------------------------------   
   PROPOSITO  : Reporte de cartera clasificada por Dia Mora, mostrando valores 
                saldo y procision
   AUTOR      :  JOHN  MONCADA PUERTA
   ACTAULIZADO: 9 septiembre de 2005      
------------------------------------------------------------------------------*/ 
RUN _SetCurs.r ("WAIT").
Listado = W_PathSpl + "InfCart_clasif-" + W_Usuario + ".Lst".
OS-DELETE VALUE(Listado).
DEFINE VAR wcantidad AS INTEGER INITIAL 0 EXTENT 10.
DEFINE VAR wvalor    AS DECIMAL INITIAL 0 EXTENT 10.
DEFINE VAR wprovis   AS DECIMAL INITIAL 0 EXTENT 10.
DEFINE VAR wedad     AS DECIMAL INITIAL 0 EXTENT 10.
ASSIGN  wedad[1] =   30
        wedad[2] =   60
        wedad[3] =   90
        wedad[4] =  180
        wedad[5] =  360 
        wedad[6] =  540 
        wedad[7] =  720 
        wedad[8] =  900 
        wedad[9] =  1080
       wedad[10] = 99999.

DEFINE VAR I AS INTEGER INITIAL 0.
FOR EACH tmpcartEdad:
  DELETE tmpcartEdad.
END.
OUTPUT TO value(Listado) NO-ECHO PAGED PAGE-SIZE 65.
{Incluido\RepEncabezado.i}
W_Reporte    = "REPORTE   : Cartera - Clasificada por dias de mora - Agencia: " + TRIM(SUBSTRING(Cmb_Agencias,6,15)) + " Al " + STRING(TODAY,"99/99/9999").
W_EncColumna = " Edad Agencia      Cantidad         Saldos      Provision".
VIEW FRAME F-Encabezado.

FOR EACH creditos WHERE creditos.estado = 2 AND sdo_capital GT 0 AND 
                        creditos.agencia GE ageini AND creditos.agencia LE agefin 
                        NO-LOCK BREAK BY Creditos.Agencia:
    IF FIRST-OF(creditos.agencia) THEN
       ASSIGN wcantidad = 0    wvalor    = 0  wprovis   = 0.

    IF dias_atraso LE 30 THEN DO:
       ASSIGN wcantidad[1] = wcantidad[1] + 1
              wvalor[1]    = wvalor[1]    + creditos.sdo_capital
              wprovis[1]   = wprovis[1]   + creditos.provision.
    END.              
    ELSE
      IF dias_atraso LE 60 THEN DO:
          ASSIGN wcantidad[2] = wcantidad[2] + 1
                 wvalor[2]    = wvalor[2]    +  creditos.sdo_capital
                 wprovis[2]   = wprovis[2]   +  creditos.provision.
      END.
      ELSE
        IF dias_atraso LE 90 THEN DO:
            ASSIGN wcantidad[3] = wcantidad[3] + 1
                   wvalor[3]    = wvalor[3]    +  creditos.sdo_capital
                   wprovis[3]   = wprovis[3]   +  creditos.provision.
        END.
        ELSE
          IF dias_atraso LE 180 THEN DO:
              ASSIGN wcantidad[4] = wcantidad[4] + 1
                     wvalor[4]    = wvalor[4]    +  creditos.sdo_capital
                     wprovis[4]   = wprovis[4]   +  creditos.provision.
          END.
          ELSE
            IF dias_atraso LE 360 THEN DO:       
                ASSIGN wcantidad[5] = wcantidad[5] + 1
                       wvalor[5]    = wvalor[5]    +  creditos.sdo_capital
                       wprovis[5]   = wprovis[5]   +  creditos.provision.
            END.                                
            ELSE                                
              IF dias_atraso LE 540 THEN DO:     
                  ASSIGN wcantidad[6] = wcantidad[6] + 1
                         wvalor[6]    = wvalor[6]    +  creditos.sdo_capital
                         wprovis[6]   = wprovis[6]   +  creditos.provision.
              END.                              
              ELSE                              
                IF dias_atraso LE 720 THEN DO:   
                    ASSIGN wcantidad[7] = wcantidad[7] + 1
                           wvalor[7]    = wvalor[7]    +  creditos.sdo_capital
                           wprovis[7]   = wprovis[7]   +  creditos.provision.
                END.                            
                ELSE                            
                  IF dias_atraso LE 900 THEN DO:
                      ASSIGN wcantidad[8] = wcantidad[8] + 1
                             wvalor[8]    = wvalor[8]    +  creditos.sdo_capital
                             wprovis[8]   = wprovis[8]   +  creditos.provision.
                  END.                          
                  ELSE
                    IF dias_atraso LE 1080 THEN DO:       
                        ASSIGN wcantidad[9] = wcantidad[9] + 1
                               wvalor[9]    = wvalor[9]    +  creditos.sdo_capital
                               wprovis[9]   = wprovis[9]   +  creditos.provision.
                    END.                                
                    ELSE                                
                        ASSIGN wcantidad[10] = wcantidad[10] + 1
                               wvalor[10]    = wvalor[10]    +  creditos.sdo_capital
                               wprovis[10]   = wprovis[10]   +  creditos.provision.



    IF LAST-OF(creditos.agencia) THEN DO:
       REPEAT i = 1 TO 10 BY 1:
         CREATE tmpcartEdad.
         ASSIGN tmpcartEdad.agencia    = Creditos.agencia
               tmpcartEdad.edad        = wedad[i] 
               tmpcartEdad.Cantidad    = wcantidad[i]     
               tmpcartEdad.sdocapital  = wvalor[i]        
               tmpCartEdad.provision   = wprovis[i].       
       END.                                 
    END.                                    
END.

DEFINE VAR wcant  AS INTEGER INITIAL 0.
DEFINE VAR wsdo   AS DECIMAL INITIAL 0.
DEFINE VAR wpro   AS DECIMAL INITIAL 0.
DEFINE VAR wTcant AS INTEGER INITIAL 0.
DEFINE VAR wTsdo  AS DECIMAL INITIAL 0.
DEFINE VAR wTpro  AS DECIMAL INITIAL 0.

FOR EACH tmpcartEdad BREAK BY edad:
    IF FIRST-OF(tmpcartEdad.Edad) THEN
       ASSIGN wcant  = 0   wsdo  = 0   wpro  = 0.

    FIND agencias WHERE agencias.agencia = tmpcartEdad.agencia NO-LOCK NO-ERROR.
    PUT tmpcartEdad.Edad        FORMAT "zzzz9"          " "
        Agencias.nombre         FORMAT "X(10)"          " "
        tmpcartEdad.Cantidad    FORMAT "zz,zzz,zz9"     " "   
        tmpcartEdad.sdocapital  FORMAT "zz,zzz,zzz,zz9" " "
        tmpCartEdad.provision   FORMAT "zz,zzz,zzz,zz9" SKIP(0). 

    ASSIGN wcant  = wcant  + tmpcartEdad.Cantidad 
           wsdo   = wsdo   + tmpcartEdad.sdocapital
           wpro   = wpro   + tmpCartEdad.provision
           wtcant = wtcant + tmpcartEdad.Cantidad  
           wtsdo  = wtsdo  + tmpcartEdad.sdocapital
           wtpro  = wtpro  + tmpCartEdad.provision.

    IF LAST-OF(tmpcartEdad.Edad) THEN DO:
       PUT "                 ----------------------------------------" SKIP(0).
       PUT "     Totales     " 
           WCant FORMAT "zz,zzz,zz9"     " "   
           Wsdo  FORMAT "zz,zzz,zzz,zz9" " "
           Wpro  FORMAT "zz,zzz,zzz,zz9" SKIP(2). 
    END.
END.
PUT "                 ========================================" SKIP(0).
PUT "  Tot Generales: " WtCant FORMAT "zz,zzz,zz9"     " "      
                        Wtsdo  FORMAT "zz,zzz,zzz,zz9" " "      
                        Wtpro  FORMAT "zz,zzz,zzz,zz9" SKIP(2). 




OUTPUT CLOSE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InfCDAT Wwin 
PROCEDURE InfCDAT :
/*------------------------------------------------------------------------------
  Purpose:     Titulos de CDAT Vigentes, Renovados, Cancelados dentro del rango
               de fecha especificado y por la agencia requerida.
               Enseña: Agencia, documento de Identidad, nombres y apellidos, #Titulo,
               Fecha Constituci¢n, Fecha Renovacion, prorroga y la de cancelacion,
  Autor :     JOHN MONCADA PUERTA             
 Actualizacion:  9 septiembre de 2005
------------------------------------------------------------------------------*/
DEFINE VAR Westado    LIKE ahorros.estado.
DEFINE VAR Wcodage LIKE ahorros.agencia.
DEFINE VAR WnomAge LIKE agencias.nombre.    
DEFINE VAR Wnit    LIKE ahorros.nit.    
DEFINE VAR WNombre AS CHARACTER FORMAT "X(40)".
DEFINE VAR WCueaho LIKE ahorros.cue_ahorros.
DEFINE VAR WFecApe LIKE ahorros.Fec_Apertura.
DEFINE VAR WFecVen LIKE ahorros.Fec_Vencimiento.
DEFINE VAR WFecCan LIKE ahorros.Fec_Cancelacion.
DEFINE VAR WFecPro LIKE ahorros.Fec_Prorroga.
DEFINE VAR Wtasa   LIKE ahorros.tasa.    
DEFINE VAR Wplazo  LIKE ahorros.plazo.    
DEFINE VAR Wperliq LIKE ahorros.per_liquidacion.
DEFINE VAR Wmonto  LIKE ahorros.monto_apertura.
DEFINE VAR Wsaldo  LIKE ahorros.sdo_disponible.
DEFINE VAR WDesc   AS CHARACTER FORMAT "X(8)".

RUN _SetCurs.r ("WAIT").
Listado = W_PathSpl + "InfAhoApeCanc-" + W_Usuario + ".Lst".
OS-DELETE VALUE(Listado).
FOR EACH TmpCDAT:
    DELETE TmpCDAT.
END.

OPEN infCDAT.
REPEAT :
    FETCH infCDAT INTO Westado, Wcodage, WnomAge, Wnit, WNombre, WCueaho, WFecApe, WFecVen, WFecCan, WFecPro, Wtasa, Wplazo, Wperliq, Wmonto, Wsaldo.
    CREATE TmpCDAT.
    ASSIGN Testado   = Westado 
           Tcodage   = Wcodage 
           TnomAge   = WnomAge 
           Tnit      = Wnit    
           TNombre   = WNombre 
           TCueaho   = WCueaho 
           TFecApe   = WFecApe 
           TFecVen   = WFecVen 
           TFecCan   = WFecCan 
           TFecPro   = WFecPro 
           Ttasa     = Wtasa   
           Tplazo    = Wplazo  
           Tperliq   = Wperliq 
           Tmonto    = Wmonto  
           Tsaldo    = Wsaldo. 
END.
CLOSE infcdat.
OUTPUT TO value(Listado) NO-ECHO PAGED PAGE-SIZE 65.
{Incluido\RepEncabezado.i}
W_Reporte    = "REPORTE   : CDAT - Conmstituidos, Renovados, Cancelados - Agencia: " + TRIM(SUBSTRING(Cmb_Agencias,6,15)) + " Del " + STRING(FecIni,"99/99/9999") + " Al " + STRING(FecFin,"99/99/9999").
W_EncColumna = "Nit          Nombres Completos                       cuenta     Fec_Ape    Fec_Vcto  Fec_Canc    Fec_Pro      Tasa  Plazo Liq Monto Apert      Saldo".
VIEW FRAME F-Encabezado.
DEFINE VAR w-monto   AS DECIMAL INITIAL 0 EXTENT 3.
DEFINE VAR w-saldo   AS DECIMAL INITIAL 0 EXTENT 3.
DEFINE VAR w-cant    AS INTEGER INITIAL 0 EXTENT 3.

/* Reorganizo los estados */
FOR EACH TmpCDAT:
  IF Testado = 1 AND Tfecpro EQ ? THEN TmpCDAT.Testado = 0.
END.

FOR EACH TmpCDAT BREAK BY TmpCDAt.Tcodage BY Testado:
    IF FIRST-OF(TmpCDAT.TCodage) THEN DO:
       ASSIGN w-saldo[2] = 0  w-monto[2] = 0.
    END.
    IF FIRST-OF(TmpCDAt.Testado) THEN DO:
       IF Testado = 0 THEN Wdesc = "CONSTITUIDOS".
       ELSE IF Testado = 1 THEN Wdesc = "RENOVADOS ".
            ELSE wdesc = "CANCELADOS".
       PUT "Agencia: " TnomAge FORMAT "X(15)" " - CDAT " wdesc FORMAT "X(12)" SKIP(1).
       ASSIGN w-saldo[1] = 0  w-monto[1] = 0.
    END.

    ASSIGN w-saldo[1] = w-saldo[1] + Tsaldo 
           w-monto[1] = w-monto[1] + Tmonto
           w-saldo[2] = w-saldo[2] + Tsaldo
           w-monto[2] = w-monto[2] + Tmonto
           w-saldo[3] = w-saldo[3] + Tsaldo
           w-monto[3] = w-monto[3] + Tmonto.
    
    PUT   Tnit    FORMAT "X(12)" " "                                               
          TNombre FORMAT "X(40)" " "
          TCueaho FORMAT "X(8)"  " "                                                
          TFecApe FORMAT "99/99/9999"  " "                                          
          TFecVen FORMAT "99/99/9999"  " "                                          
          TFecCan FORMAT "99/99/9999"  " "                                          
          TFecPro FORMAT "99/99/9999"  " "                                          
          Ttasa   FORMAT "z9.9999"     " "                                             
          Tplazo  FORMAT "z,zz9"       " "                                               
          Tperliq FORMAT "zz9"         " "                                          
          Tmonto  FORMAT "zzz,zzz,zz9" " "                                         
          Tsaldo  FORMAT "zzz,zzz,zz9" SKIP(0).                           
    
    IF LAST-OF(TmpCDAt.Testado) THEN DO:
       PUT "                                                    ------------------------------------------------------------------------------------------------" SKIP(0).
       PUT " - Total CDAT " wdesc FORMAT "X(12)" " Agencia: " TnomAge FORMAT "X(81)"  w-monto[1] FORMAT "zzzz,zzz,zzz,zz9" " "
                                     w-saldo[1] FORMAT "zzz,zzz,zzz,zz9" SKIP(2). 
    END.

    IF LAST-OF(TmpCDAT.TCodage) THEN DO:
       PUT "                                                    ------------------------------------------------------------------------------------------------" SKIP(0).
       PUT "            Total Agencia "  TnomAge FORMAT "X(91)"  w-monto[2] FORMAT "zzz,zzz,zzz,zz9" " "
                                                        w-saldo[2] FORMAT "zzzz,zzz,zzz,zz9" SKIP(2).
    END.
END.
PUT SKIP(0) "                                       -------------------------------------------------" SKIP(0).
PUT "  TOTALES GENERALES        " FORMAT "X(117)"  w-monto[3] FORMAT "zzz,zzz,zzz,zz9" " "  w-saldo[3] FORMAT "zzz,zzz,zzz,zz9".                                             

OUTPUT CLOSE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InfCreditos_Canc_Precanc Wwin 
PROCEDURE InfCreditos_Canc_Precanc :
/*------------------------------------------------------------------------------
  Purpose      :     Informe de Creditos cancelados y precancelados
  autor        :     JOHN MONCADA PUERTA
  Actualizacion: 9 de septiembre de 2005
------------------------------------------------------------------------------*/
RUN _SetCurs.r ("WAIT").
Listado = W_PathSpl + "InfCreCancPrecanc-" + W_Usuario + ".Lst".
OS-DELETE VALUE(Listado).
DEFINE VAR cont       AS INTEGER INITIAL 0.
DEFINE VAR zFecVcto   AS DATE.
DEFINE VAR zFecCanc   AS DATE.
DEFINE VAR zper_pago  AS INTEGER INITIAL 0.
DEFINE VAR znombre    AS CHARACTER FORMAT "X(40)".
OUTPUT TO value(Listado) NO-ECHO PAGED PAGE-SIZE 65.
{Incluido\RepEncabezado.i}
W_Reporte    = "REPORTE   : Creditos - Cancelados y Precancelados - Agencia: " + TRIM(SUBSTRING(Cmb_Agencias,6,15)) + " Del " + STRING(FecIni,"99/99/9999") + " Al " + STRING(FecFin,"99/99/9999").
W_EncColumna = " #Asoc. Nit          Nombres Completos                          #Cred. #Pagare  Linea                 Periodo       Monto Plazo     Cuota  tasa Desembolso Vencimto Cancelacion".
VIEW FRAME F-Encabezado.

DEFINE VAR wmonto  AS DECIMAL INITIAL 0.
DEFINE VAR wtmonto AS DECIMAL INITIAL 0.
FOR EACH Creditos WHERE Creditos.estado = 3     AND
                        Creditos.agencia        GE ageini AND Creditos.agencia         LE agefin AND 
                        Creditos.Fec_CanceTotal GE FecIni AND Creditos.Fec_CanceTotal  LE FecFin NO-LOCK
                        BREAK BY creditos.agencia : 
    IF FIRST-OF(Creditos.agencia) THEN DO:
       FIND Agencias WHERE Agencias.Agencia = Creditos.agencia  NO-LOCK NO-ERROR.
       PUT " Agencia: " CAPS(TRIM(Agencias.nombre)) SKIP(1).
       ASSIGN cont = 0   wmonto = 0.
    END.

    cont = cont + 1.
    FIND FIRST planpagos WHERE planpagos.nit = creditos.nit AND 
               planpagos.nro_cuota   = creditos.plazo       AND 
               planpagos.num_credito = Creditos.num_credito NO-LOCK  NO-ERROR.
    IF AVAILABLE(planpagos) THEN
       zFecVcto = Planpagos.fec_vcto.
    ELSE
       MESSAGE "No se encontro fecha de vencimiento -credito Nro" Credito.Num_Credito SKIP
               "Nit " Creditos.nit
           VIEW-AS ALERT-BOX INFO BUTTONS OK.

    CASE Creditos.Per_Pago:                                                                        
     WHEN 1 THEN ASSIGN zper_pago = 7.                                                        
     WHEN 2 THEN ASSIGN zper_pago = 10.                                                        
     WHEN 3 THEN ASSIGN zper_pago = 15.                                                        
     WHEN 4 THEN ASSIGN zper_pago = 30.                                                        
     WHEN 5 THEN ASSIGN zper_pago = 60.                                                        
     WHEN 6 THEN ASSIGN zper_pago = 90.                                                        
     WHEN 7 THEN ASSIGN zper_pago = 120.                                                        
     WHEN 8 THEN ASSIGN zper_pago = 180.                                                        
     WHEN 9 THEN ASSIGN zper_pago = 360.                                                        
    END CASE. 

    IF Creditos.Fec_canceTot = ?  THEN
       MESSAGE "Sin  Fec_canceTot"
           VIEW-AS ALERT-BOX INFO BUTTONS OK.
    ELSE
       zFecCanc = Creditos.Fec_canceTot.

  FIND clientes     WHERE clientes.nit = creditos.nit NO-LOCK NO-ERROR.
  IF AVAILABLE(Clientes) THEN
     znombre = TRIM(Clientes.apellido1) + " " + TRIM(Clientes.apellido2) + " " + TRIM(Clientes.nombre).

  FIND pro_creditos WHERE pro_creditos.cod_credito = creditos.cod_credito NO-LOCK NO-ERROR.
  IF AVAILABLE(pro_creditos) THEN DO:
     ASSIGN wmonto  = wmonto + creditos.monto
            wTmonto = wmonto + creditos.monto.
     PUT cont                      FORMAT "zzz,zz9"     " "
         creditos.nit              FORMAT "X(12)"       " "
         znombre                   FORMAT "X(40)"       " "
         Creditos.num_credito      FORMAT "zzzzzzz9"    " "
         Creditos.pagare           FORMAT "X(8)"        " "
         Pro_creditos.Nom_Producto FORMAT "X(25)"       " "
         Zper_pago                 FORMAT "zz9"         " "
         Creditos.monto            FORMAT "zzz,zzz,zz9" " "
         Creditos.plazo            FORMAT "zz9"         " "
         Creditos.Cuota            FORMAT "zzz,zzz,zz9" " "
         Creditos.tasa             FORMAT "z9.99"       " "
         Creditos.Fec_Desembolso   FORMAT "99/99/9999"  " "
         zfecVcto                  FORMAT "99/99/9999"  " "
         zfecCanc                  FORMAT "99/99/9999"  SKIP.
  END.

  IF LAST-OF(creditos.agencia) THEN DO:
     PUT "                                                                               -----------------------------------------------" SKIP(0).
     PUT "                                                                               Total Agencia: " CAPS(TRIM(Agencias.nombre)) FORMAT "X(12)"
         wmonto FORMAT "zzz,zzz,zzz,zz9" SKIP(2).
  END.
END.
PUT "                                                                         ----------------------------------------------------------" SKIP(0).
PUT "                                                                         TOTAL GENERAL :                  " wTmonto FORMAT "zzz,zzz,zzz,zz9".

OUTPUT CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InfCreditos_FecVcto Wwin 
PROCEDURE InfCreditos_FecVcto :
/*------------------------------------------------------------------------------
   PROPOSITO  : Generar un archivo que contenga las fechas de vencimiento de las 
                obligaciones durante 
                el periodo seleccionado   
   AUTOR      :  JOHN  MONCADA PUERTA
   ACTAULIZADO: 9 septiembre de 2005      
   ------------------------------------------------------------------------------*/
 RUN _SetCurs.r ("WAIT").
 Listado = W_PathSpl + "InfCre-FecVcto" + W_Usuario + ".Lst".
 OS-DELETE VALUE(Listado).
 DEFINE VAR wconteo    AS INTEGER INITIAL 0.
 DEFINE VAR zper_pago  AS INTEGER INITIAL 0.
 DEFINE VAR znombre    AS CHARACTER FORMAT "X(40)".
 DEFINE VAR zagencia   AS CHARACTER FORMAT "X(15)" INITIAL "".
 
 OUTPUT TO value(Listado) NO-ECHO PAGED PAGE-SIZE 65.
 {Incluido\RepEncabezado.i}
 W_Reporte    = "REPORTE   : Ahorros - Aperturas y Cancelaciones - Agencia: " + TRIM(SUBSTRING(Cmb_Agencias,6,15)) + " Del " + STRING(FecIni,"99/99/9999") + " Al " + STRING(FecFin,"99/99/9999").
 W_EncColumna = "#Numero Cedula       Nombres Completos                            #Cre #Pagaré  Linea                     Per       Monto Pla       Cuota Tasa        Saldo Desembolso Vencimiento".
 VIEW FRAME F-Encabezado.

 DEFINE VAR wsaldo  AS DECIMAL INITIAL 0.
 DEFINE VAR wmonto  AS DECIMAL INITIAL 0.
 DEFINE VAR wtsaldo AS DECIMAL INITIAL 0.
 DEFINE VAR wtmonto AS DECIMAL INITIAL 0.
 FOR EACH Creditos WHERE Creditos.estado = 2        AND 
                         Creditos.agencia GE ageini AND Creditos.agencia LE agefin 
                   NO-LOCK BREAK By Creditos.agencia:
     IF FIRST-OF(Creditos.agencia) THEN DO:
        FIND agencias WHERE agencias.agencia = Creditos.agencia NO-LOCK.
        PUT " Agencia: "  CAPS(TRIM(agencias.nombre))  FORMAT "X(15)"  SKIP(1).
        ASSIGN wsaldo = 0   wmonto = 0   wconteo = 0.
     END.
     FIND FIRST planpagos WHERE planpagos.nit = creditos.nit           AND
                planpagos.nro_cuota   =  creditos.plazo                AND
                planpagos.num_credito =  Creditos.num_credito          AND
                Planpagos.fec_vcto    GE fecini AND Planpagos.fec_vcto LE  fecfin
                NO-LOCK  NO-ERROR.
     IF AVAILABLE(planpagos) THEN DO:
        CASE Creditos.Per_Pago:                                                                        
          WHEN 1 THEN ASSIGN zper_pago = 7.                                                        
          WHEN 2 THEN ASSIGN zper_pago = 10.                                                        
          WHEN 3 THEN ASSIGN zper_pago = 15.                                                        
          WHEN 4 THEN ASSIGN zper_pago = 30.                                                        
          WHEN 5 THEN ASSIGN zper_pago = 60.                                                        
          WHEN 6 THEN ASSIGN zper_pago = 90.                                                        
          WHEN 7 THEN ASSIGN zper_pago = 120.                                                        
          WHEN 8 THEN ASSIGN zper_pago = 180.                                                        
          WHEN 9 THEN ASSIGN zper_pago = 360.                                                        
        END CASE. 

        FIND clientes     WHERE clientes.nit = creditos.nit NO-LOCK NO-ERROR.
        IF AVAILABLE(Clientes) THEN
           znombre = TRIM(clientes.apellido1) + " " + TRIM(clientes.apellido2) + " " + TRIM(Clientes.nombre).

        FIND pro_creditos WHERE pro_creditos.cod_credito = creditos.cod_credito NO-LOCK.
        IF AVAILABLE(pro_creditos) THEN     
        DO:
           ASSIGN wsaldo  = wsaldo + Creditos.sdo_capital
                  wmonto  = wmonto + creditos.monto
                  wTsaldo = wTsaldo + Creditos.sdo_capital
                  wTmonto = wTmonto + creditos.monto.

           wconteo = wconteo + 1.
           PUT wconteo                   FORMAT "zzz,zz9"     " "
               creditos.nit              FORMAT "X(12)"       " "
               znombre                   FORMAT "X(40)"       " "
               Creditos.num_credito      FORMAT "zzzzzzz9"    " "
               Creditos.pagare           FORMAT "X(8)"        " "
               Pro_creditos.Nom_Producto FORMAT "X(25)"       " "
               Zper_pago                 FORMAT "zz9"         " "
               Creditos.monto            FORMAT "zzz,zzz,zz9" " "
               Creditos.plazo            FORMAT "zz9"         " "
               Creditos.Cuota            FORMAT "zzz,zzz,zz9" " "
               Creditos.tasa             FORMAT "z9.99"       " "
               Creditos.Sdo_capital      FORMAT "zzz,zzz,zz9" " "
               Creditos.Fec_Desembolso                        " "
               planpagos.fec_vcto                                       " " SKIP.
        END.
     END.
     IF LAST-OF(Creditos.agencia) THEN DO:
        PUT SKIP(1) "    Total Agencia: "  CAPS(TRIM(agencias.nombre))  FORMAT "X(86)" " "
                 wmonto FORMAT "zzz,zzz,zzz,zz9" " " FORMAT "X(19)"
                 wsaldo FORMAT "zzz,zzz,zzz,zz9" SKIP(2).
     END.
 END.
PUT SKIP(1) "    Total GENERAL: " FORMAT "X(104)" " "                                                    
         wTmonto FORMAT "zzzz,zzz,zzz,zz9" " " FORMAT "X(19)"                                                                   
         wTsaldo FORMAT "zzz,zzz,zzz,zz9" SKIP(2).                                                                              OUTPUT CLOSE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

