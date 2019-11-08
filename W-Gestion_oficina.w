&ANALYZE-SUSPEND _VERSION-NUMBER AB_v9r12 GUI ADM2
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS wWin 
/*------------------------------------------------------------------------

  File: 

  Description: from cntnrwin.w - ADM SmartWindow Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  History: New V9 Version - January 15, 1998
          
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AB.              */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
  {Incluido/Variable.I "SHARED"}
  {Incluido/VARCON.I "SHARED"}

  DEFINE VAR W_Ok AS LOGICAL.
  DEFINE VAR i AS INTEGER.
  
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
  DEFINE VARIABLE ProIni   AS INTEGER FORMAT "999" INITIAL 0.
  DEFINE VARIABLE ProFin   AS INTEGER FORMAT "999" INITIAL 999.
  DEFINE VARIABLE TpdIni   AS INTEGER FORMAT "999" INITIAL 0.
  DEFINE VARIABLE TpdFin   AS INTEGER FORMAT "999" INITIAL 9.
  
  DEFINE VARIABLE EstadoC  AS INTEGER FORMAT "9".
DEFINE VAR NomProduc AS CHARACTER FORMAT "X(15)".
  DEFINE VAR Listado AS CHARACTER INITIAL "".

 DEFINE VAR j AS DECIMAL.
 DEFINE VAR k AS INTEGER.

DEFINE VAR W_Nin AS CHARACTER FORMAT "X(61)".
DEFINE VAR W_Vig AS INTEGER FORMAT "9999".
DEFINE VAR W_Nus AS CHARACTER FORMAT "X(20)".

DEFINE VAR W_Compromiso AS CHARACTER FORMAT "X(2)".
DEFINE VAR W_Interes AS DECIMAL FORMAT "->>>,>>>,>>9".
DEFINE VAR TotReg    AS DECIMAL.
DEFINE VAR TotSdo    AS DECIMAL FORMAT ">,>>>,>>>,>>9".
DEFINE VAR TotRegAge AS DECIMAL.
DEFINE VAR TotSdoAge AS DECIMAL FORMAT ">,>>>,>>>,>>9".
DEFINE VAR W_TipPdt AS CHARACTER FORMAT "X(12)".
DEFINE VAR W_Vigen  AS INTEGER FORMAT "9999".
DEFINE VAR W_ForPag AS CHARACTER FORMAT "X(10)".

DEFINE VAR Tot_CtaNva AS DECIMAL FORMAT ">>>,>>9".
DEFINE VAR Tot_CtaRet AS DECIMAL FORMAT ">>>,>>9".
DEFINE VAR Tot_CtaTot AS DECIMAL FORMAT ">>>,>>9".
DEFINE VAR Tot_Saldos AS DECIMAL FORMAT ">>,>>>,>>>,>>9".
DEFINE VAR Tot_Consig AS DECIMAL FORMAT ">>,>>>,>>>,>>9".
DEFINE VAR Tot_Retiro AS DECIMAL FORMAT ">>,>>>,>>>,>>9".
DEFINE VAR Tot_Atraso AS DECIMAL FORMAT ">>,>>>,>>>,>>9".


DEFINE TEMP-TABLE TAge
FIELD TCla LIKE Total_agencia.Clase_Producto
FIELD TCod LIKE Total_agencia.Codigo
FIELD TCNv LIKE Total_agencia.Cta_nuevas
FIELD TCRt LIKE Total_agencia.Cta_Retiradas
FIELD TCTo LIKE Total_agencia.Cta_totales
FIELD TSdo LIKE Total_agencia.Sdo_dia
FIELD TTPo LIKE Total_agencia.Tasa_Ponderada
FIELD TTpr LIKE Total_agencia.Tasa_promedio
FIELD TTip LIKE Total_agencia.Tipo_Producto
FIELD TVAt LIKE Total_agencia.Val_Atraso 
FIELD TCon LIKE Total_agencia.Vr_Consignado
FIELD TRet LIKE Total_agencia.Vr_Retirado.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F_Buscar

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Buscar BUTTON-154 Btn_anterior Btn_siguiente 
&Scoped-Define DISPLAYED-OBJECTS Buscar 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_anterior 
     LABEL "Anterior" 
     SIZE 15 BY 1.12.

DEFINE BUTTON Btn_siguiente 
     LABEL "Siguiente" 
     SIZE 15 BY 1.12.

DEFINE BUTTON BUTTON-154 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Button 154" 
     SIZE 7 BY 1.62.

DEFINE VARIABLE Buscar AS CHARACTER FORMAT "X(20)":U 
     LABEL "Buscar" 
     VIEW-AS FILL-IN 
     SIZE 33 BY .81
     BGCOLOR 15 FONT 0 NO-UNDO.

DEFINE BUTTON BUTTON-143 
     IMAGE-UP FILE "imagenes/informacion.bmp":U
     LABEL "Button 143" 
     SIZE 10 BY 1.62.

DEFINE BUTTON BUTTON-148 
     LABEL "Ejecutar" 
     SIZE 10 BY 1.62.

DEFINE BUTTON BUTTON-149 
     IMAGE-UP FILE "imagenes/impresora2.bmp":U
     LABEL "Button 149" 
     SIZE 10 BY 1.62.

DEFINE BUTTON BUTTON-150 
     LABEL "Salir" 
     SIZE 10 BY 1.62.

DEFINE BUTTON BUTTON-152 
     IMAGE-UP FILE "imagenes/interrogacion.bmp":U
     LABEL "Button 152" 
     SIZE 4 BY 1.12.

DEFINE BUTTON BUTTON-153 
     IMAGE-UP FILE "imagenes/lupa.bmp":U
     LABEL "Button 153" 
     SIZE 7 BY 1.65.

DEFINE VARIABLE Cmb_Agencias AS CHARACTER FORMAT "X(256)":U 
     LABEL "Agencia" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "000 - Todas las Agencias" 
     DROP-DOWN-LIST
     SIZE 38 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE E1 AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL LARGE
     SIZE 96 BY 17.5
     BGCOLOR 15 FONT 2 NO-UNDO.

DEFINE VARIABLE Fec_Gestion AS DATE FORMAT "99/99/9999":U 
     LABEL "Fecha de Gestión" 
     VIEW-AS FILL-IN 
     SIZE 13.57 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE RTipo AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Informe Consolidado", 1,
"Comparativo Agencias", 2,
"De Gestion", 3
     SIZE 63 BY 1.08 NO-UNDO.

DEFINE IMAGE IMAGE-2
     FILENAME "imagenes/clock05.ico":U
     SIZE 5 BY 1.35.

DEFINE RECTANGLE R1
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 7 BY .54
     BGCOLOR 15 .

DEFINE RECTANGLE R2
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 7 BY .54
     BGCOLOR 15 .

DEFINE RECTANGLE R3
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 7 BY .54
     BGCOLOR 15 .

DEFINE RECTANGLE R4
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 7 BY .54
     BGCOLOR 15 .

DEFINE RECTANGLE R5
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 7 BY .54
     BGCOLOR 15 .

DEFINE RECTANGLE R6
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 7 BY .54
     BGCOLOR 15 .

DEFINE RECTANGLE R7
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 7 BY .54
     BGCOLOR 15 .

DEFINE RECTANGLE R8
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 7 BY .54
     BGCOLOR 15 .

DEFINE RECTANGLE R9
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 7 BY .54
     BGCOLOR 15 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F_Gestion
     Cmb_Agencias AT ROW 1.27 COL 14 COLON-ALIGNED
     Fec_Gestion AT ROW 1.54 COL 77 COLON-ALIGNED
     BUTTON-143 AT ROW 1.54 COL 102
     BUTTON-153 AT ROW 2.88 COL 3
     RTipo AT ROW 2.88 COL 14 NO-LABEL
     BUTTON-149 AT ROW 3.15 COL 102
     E1 AT ROW 4.5 COL 3 NO-LABEL
     BUTTON-148 AT ROW 4.77 COL 102
     BUTTON-150 AT ROW 19.04 COL 102
     BUTTON-152 AT ROW 20.92 COL 105
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 113.57 BY 21.38
         BGCOLOR 17 FONT 5.

DEFINE FRAME F_Buscar
     Buscar AT ROW 1.27 COL 7 COLON-ALIGNED
     BUTTON-154 AT ROW 1.54 COL 45
     Btn_anterior AT ROW 2.35 COL 9
     Btn_siguiente AT ROW 2.35 COL 27
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 3.96
         SIZE 53 BY 3.5
         BGCOLOR 17 FONT 4
         TITLE "Buscar".

DEFINE FRAME F_Progreso
     IMAGE-2 AT ROW 1.27 COL 4.57
     R1 AT ROW 6.92 COL 3.57
     R2 AT ROW 6.38 COL 3.57
     R3 AT ROW 5.85 COL 3.57
     R4 AT ROW 5.31 COL 3.57
     R5 AT ROW 4.77 COL 3.57
     R6 AT ROW 4.23 COL 3.57
     R7 AT ROW 3.69 COL 3.57
     R8 AT ROW 3.15 COL 3.57
     R9 AT ROW 2.62 COL 3.57
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 101 ROW 6.65
         SIZE 12 BY 6.73
         BGCOLOR 17 .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wWin ASSIGN
         HIDDEN             = YES
         TITLE              = "SFG - Modulo de Informes de Gestion Diaria de la Agencia"
         HEIGHT             = 21.38
         WIDTH              = 113.57
         MAX-HEIGHT         = 28.81
         MAX-WIDTH          = 146.14
         VIRTUAL-HEIGHT     = 28.81
         VIRTUAL-WIDTH      = 146.14
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.

&IF '{&WINDOW-SYSTEM}' NE 'TTY' &THEN
IF NOT wWin:LOAD-ICON("imagenes/cubos.ico":U) THEN
    MESSAGE "Unable to load icon: imagenes/cubos.ico"
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
&ENDIF
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB wWin 
/* ************************* Included-Libraries *********************** */

{src/adm2/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW wWin
  VISIBLE,,RUN-PERSISTENT                                               */
/* REPARENT FRAME */
ASSIGN FRAME F_Buscar:FRAME = FRAME F_Gestion:HANDLE
       FRAME F_Progreso:FRAME = FRAME F_Gestion:HANDLE.

/* SETTINGS FOR FRAME F_Buscar
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME F_Buscar:HIDDEN           = TRUE.

/* SETTINGS FOR FRAME F_Gestion
                                                                        */

DEFINE VARIABLE XXTABVALXX AS LOGICAL NO-UNDO.

ASSIGN XXTABVALXX = FRAME F_Buscar:MOVE-AFTER-TAB-ITEM (BUTTON-149:HANDLE IN FRAME F_Gestion)
       XXTABVALXX = FRAME F_Buscar:MOVE-BEFORE-TAB-ITEM (E1:HANDLE IN FRAME F_Gestion)
       XXTABVALXX = FRAME F_Progreso:MOVE-AFTER-TAB-ITEM (BUTTON-148:HANDLE IN FRAME F_Gestion)
       XXTABVALXX = FRAME F_Progreso:MOVE-BEFORE-TAB-ITEM (BUTTON-150:HANDLE IN FRAME F_Gestion)
/* END-ASSIGN-TABS */.

/* SETTINGS FOR FRAME F_Progreso
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME F_Progreso:HIDDEN           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* SFG - Modulo de Informes de Gestion Diaria de la Agencia */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* SFG - Modulo de Informes de Gestion Diaria de la Agencia */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_anterior
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_anterior wWin
ON CHOOSE OF Btn_anterior IN FRAME F_Buscar /* Anterior */
DO:
  ASSIGN FRAME F_Buscar Buscar.
  W_Ok = E1:SEARCH(Buscar,34) IN FRAME F_Gestion.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_siguiente
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_siguiente wWin
ON CHOOSE OF Btn_siguiente IN FRAME F_Buscar /* Siguiente */
DO:
  ASSIGN FRAME F_Buscar Buscar.
  W_Ok = E1:SEARCH(Buscar,33) IN FRAME F_Gestion.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Gestion
&Scoped-define SELF-NAME BUTTON-143
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-143 wWin
ON CHOOSE OF BUTTON-143 IN FRAME F_Gestion /* Button 143 */
DO:
  RUN W-InfDia.r NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-148
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-148 wWin
ON CHOOSE OF BUTTON-148 IN FRAME F_Gestion /* Ejecutar */
DO:
  
  ASSIGN FRAME F_Gestion Rtipo Cmb_Agencias Fec_Gestion.
  VIEW FRAME F_Progreso.
  IF SUBSTRING(Cmb_Agencias,1,3) EQ "000" THEN
     ASSIGN AgeIni = 0 AgeFin = 999.
  ELSE ASSIGN AgeIni = INTEGER(SUBSTRING(Cmb_Agencias,1,3)) AgeFin = AgeIni.
  CASE RTipo:
    WHEN 1 THEN RUN Consolidado.
    WHEN 2  THEN RUN Comparativo.
  END CASE.
  W_ok = e1:READ-FILE(Listado) IN FRAME F_Gestion.
  HIDE FRAME F_Progreso.
  ASSIGN R1:BGCOLOR = 15 R2:BGCOLOR = 15 R3:BGCOLOR = 15 R4:BGCOLOR = 15
         R5:BGCOLOR = 15 R6:BGCOLOR = 15 R7:BGCOLOR = 15 R8:BGCOLOR = 15
         R9:BGCOLOR = 15.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-149
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-149 wWin
ON CHOOSE OF BUTTON-149 IN FRAME F_Gestion /* Button 149 */
DO:
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


&Scoped-define SELF-NAME BUTTON-150
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-150 wWin
ON CHOOSE OF BUTTON-150 IN FRAME F_Gestion /* Salir */
DO:
  OS-DELETE VALUE(Listado).
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


&Scoped-define SELF-NAME BUTTON-153
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-153 wWin
ON CHOOSE OF BUTTON-153 IN FRAME F_Gestion /* Button 153 */
DO:
  IF E1:SCREEN-VALUE EQ "" THEN
     MESSAGE "No se ha ejecutado ningún Informe" SKIP
             "Escoja el informe y presione clic" SKIP
             "en el boton ejecutar!." VIEW-AS ALERT-BOX INFORMATION.
  ELSE DO:
    Buscar:SCREEN-VALUE IN FRAME F_Buscar = E1:SELECTION-TEXT.
    ASSIGN FRAME F_Buscar Buscar.
    W_Ok = E1:SEARCH(Buscar,32).
    VIEW FRAME F_Buscar.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Buscar
&Scoped-define SELF-NAME BUTTON-154
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-154 wWin
ON CHOOSE OF BUTTON-154 IN FRAME F_Buscar /* Button 154 */
DO:
  HIDE FRAME F_Buscar.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK wWin 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm2/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects wWin  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Comparativo wWin 
PROCEDURE Comparativo :
Listado = /*W_PathSpl +*/ "c:\info\Gestion.LST".  

DEFINE VAR TotReg AS DECIMAL.
DEFINE VAR NomTipPdt AS CHARACTER FORMAT "X(30)".
DEFINE VAR NomClaPdt AS CHARACTER FORMAT "X(15)".
OS-DELETE VALUE(Listado).
FOR EACH TAge: DELETE TAge. END.
ASSIGN FRAME F_Gestion Fec_Gestion.
OUTPUT TO value(Listado) PAGED PAGE-SIZE 65.

{Incluido\RepEncabezado.i}
  W_Reporte   = "REPORTE   : Gestion de Agencia Consolidado "
              + " - FECHA: " + STRING(Fec_Gestion) + " - " + STRING(TIME,"hh:mm am").
  W_EncColumna =  "Agencia            CtNuevas CtReti  Cta_Total    Saldo.Dia   TasaProm  TasaPond  Val.Atraso    Consignado     Retirado". 

  VIEW FRAME F-Encabezado.
  ASSIGN j = 0 k = 0.
  DISPLAY "COMPARATIVO DE PRODUCTOS DE AHORRO" WITH WIDTH 132 FRAME FTIAH USE-TEXT NO-BOX.
  FOR EACH Pro_Ahorros NO-LOCK:
    DISPLAY "------------------------------------------------------------------------------------------------------------------------" AT 1
            Pro_Ahorros.Cod_Ahorro   AT 1   
            Pro_Ahorros.Nom_Producto AT 5 WITH WIDTH 132 FRAME F_TitAho USE-TEXT NO-BOX NO-LABELS STREAM-IO.
    FOR EACH Total_Agencia WHERE 
             TOTAL_Agencia.Tipo_Producto  EQ 1 AND
             TOTAL_Agencia.Clase_Producto EQ Pro_Ahorro.Tip_Ahorro AND
             DECIMAL(TOTAL_Agencia.Codigo) EQ Pro_Ahorros.Cod_Ahorro AND
             Total_Agencia.Fecha          EQ Fec_Gestion NO-LOCK
             BREAK BY Total_Agencia.Agencia BY TOTAL_Agencia.Clase_Producto:  
      j = j + 1.
      RUN Progreso.
      FIND Agencias WHERE Agencias.Agencia EQ TOTAL_Agencia.Agencia NO-LOCK.
      DISPLAY Agencias.Nombre               AT 1  FORMAT "X(18)"
              Total_agencia.Cta_nuevas      AT 21 FORMAT ">>,>>9"
              Total_agencia.Cta_Retiradas   AT 28 FORMAT ">>,>>9"
              Total_agencia.Cta_totales     AT 35 FORMAT ">>,>>>,>>9"
              Total_agencia.Sdo_dia         AT 48 FORMAT "->>>,>>>,>>9"
              Total_agencia.Tasa_promedio   AT 62 FORMAT ">>>.9999"
              Total_agencia.Tasa_Ponderada  AT 71 FORMAT ">>>.9999"
              Total_agencia.Val_Atraso      AT 80 FORMAT "->>>,>>>,>>9"
              Total_agencia.Vr_Consignado   AT 94 FORMAT "->>>,>>>,>>9"
              Total_agencia.Vr_Retirado     AT 107 FORMAT "->>>,>>>,>>9"
              WITH FRAME F_gestionMov WIDTH 132 USE-TEXT NO-BOX NO-LABELS STREAM-IO.
    END.
  END.
  DISPLAY SKIP(1)
         "------------------------------------------------------------------------------------------------------------------------" AT 1 SKIP
         "COMPARATIVO DE PRODUCTOS DE CRÉDITO" AT 1 WITH WIDTH 132 FRAME FTICR USE-TEXT NO-BOX.
  FOR EACH Pro_Creditos NO-LOCK:
    DISPLAY "------------------------------------------------------------------------------------------------------------------------" AT 1
            Pro_Creditos.Cod_Credito   AT 1   
            Pro_Creditos.Nom_Producto AT 5 WITH WIDTH 132 FRAME F_TitCre USE-TEXT NO-BOX NO-LABELS STREAM-IO.
    FOR EACH Total_Agencia WHERE 
             TOTAL_Agencia.Tipo_Producto   EQ 2 AND
             TOTAL_Agencia.Clase_Producto  EQ Pro_Creditos.Tip_Credito AND
             DECIMAL(TOTAL_Agencia.Codigo) EQ Pro_Creditos.Cod_Credito AND
             Total_Agencia.Fecha           EQ Fec_Gestion NO-LOCK
             BREAK BY Total_Agencia.Agencia BY TOTAL_Agencia.Clase_Producto:  
      j = j + 1.
      RUN Progreso.
      FIND Agencias WHERE Agencias.Agencia EQ TOTAL_Agencia.Agencia NO-LOCK.
      DISPLAY Agencias.Nombre               AT 1  FORMAT "X(18)"
              Total_agencia.Cta_nuevas      AT 21 FORMAT ">>,>>9"
              Total_agencia.Cta_Retiradas   AT 28 FORMAT ">>,>>9"
              Total_agencia.Cta_totales     AT 35 FORMAT ">>,>>>,>>9"
              Total_agencia.Sdo_dia         AT 48 FORMAT "->>>,>>>,>>9"
              Total_agencia.Tasa_promedio   AT 62 FORMAT ">>>.9999"
              Total_agencia.Tasa_Ponderada  AT 71 FORMAT ">>>.9999"
              Total_agencia.Val_Atraso      AT 80 FORMAT "->>>,>>>,>>9"
              Total_agencia.Vr_Consignado   AT 94 FORMAT "->>>,>>>,>>9"
              Total_agencia.Vr_Retirado     AT 107 FORMAT "->>>,>>>,>>9"
              WITH FRAME F_ComCre WIDTH 132 USE-TEXT NO-BOX NO-LABELS STREAM-IO.
    END.
  END.
  DISPLAY SKIP(1)
         "------------------------------------------------------------------------------------------------------------------------" AT 1 SKIP
          "COMPARATIVO DE CUENTAS CONTABLES" AT 1 WITH WIDTH 132 FRAME FTICT USE-TEXT NO-BOX STREAM-IO.
  FOR EACH Cuentas NO-LOCK:
    FOR EACH Total_Agencia WHERE 
             TOTAL_Agencia.Tipo_Producto   EQ 3 AND
             TOTAL_Agencia.Clase_Producto  EQ Cuentas.Id_Cuenta AND
             TOTAL_Agencia.Codigo          EQ Cuentas.Cuenta AND
             Total_Agencia.Fecha           EQ Fec_Gestion NO-LOCK
             BREAK BY Total_Agencia.Agencia BY TOTAL_Agencia.Clase_Producto BY TOTAL_Agencia.Codigo:  
       DISPLAY "------------------------------------------------------------------------------------------------------------------------" AT 1
               Cuentas.Cuenta AT 1   
               Cuentas.Nombre AT 16 WITH WIDTH 132 FRAME F_TitCta USE-TEXT NO-BOX NO-LABELS STREAM-IO.
      j = j + 1.
      RUN Progreso.
      FIND Agencias WHERE Agencias.Agencia EQ TOTAL_Agencia.Agencia NO-LOCK.
      DISPLAY Agencias.Nombre               AT 1  FORMAT "X(18)"
              Total_agencia.Cta_nuevas      AT 21 FORMAT ">>,>>9"
              Total_agencia.Cta_Retiradas   AT 28 FORMAT ">>,>>9"
              Total_agencia.Cta_totales     AT 35 FORMAT ">>,>>>,>>9"
              Total_agencia.Sdo_dia         AT 48 FORMAT "->>>,>>>,>>9"
              Total_agencia.Tasa_promedio   AT 62 FORMAT ">>>.9999"
              Total_agencia.Tasa_Ponderada  AT 71 FORMAT ">>>.9999"
              Total_agencia.Val_Atraso      AT 80 FORMAT "->>>,>>>,>>9"
              Total_agencia.Vr_Consignado   AT 94 FORMAT "->>>,>>>,>>9"
              Total_agencia.Vr_Retirado     AT 107 FORMAT "->>>,>>>,>>9"
              WITH FRAME F_ComCTa WIDTH 132 USE-TEXT NO-BOX NO-LABELS STREAM-IO.
    END.
  END.
  
  VIEW FRAME F-Ftr.
  PAGE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Consolidado wWin 
PROCEDURE Consolidado :
Listado = /*W_PathSpl +*/ "c:\info\Gestion.LST".  

DEFINE VAR TotReg AS DECIMAL.
DEFINE VAR NomTipPdt AS CHARACTER FORMAT "X(30)".
DEFINE VAR NomClaPdt AS CHARACTER FORMAT "X(15)".


ASSIGN Tot_CtaNva = 0 Tot_CtaRet = 0 Tot_CtaTot = 0 Tot_Saldos = 0 Tot_Atraso = 0 Tot_Consig = 0 Tot_Retiro = 0.

OS-DELETE VALUE(Listado).
FOR EACH TAge: DELETE TAge. END.
ASSIGN FRAME F_Gestion Fec_Gestion.
OUTPUT TO value(Listado) PAGED PAGE-SIZE 65.

{Incluido\RepEncabezado.i}
  W_Reporte   = "REPORTE   : Gestion de Agencia Consolidado "
              + " - FECHA: " + STRING(Fec_Gestion) + " - " + STRING(TIME,"hh:mm am").
  W_EncColumna =  "Agencia            CtNuevas CtReti  Cta_Total    Saldo.Dia   TasaProm  TasaPond  Val.Atraso    Consignado     Retirado". 

  VIEW FRAME F-Encabezado.
  ASSIGN j = 0 k = 0.

  FOR EACH TOTAL_Agencia WHERE 
    TOTAL_Agencia.Agencia GE AgeIni AND
    TOTAL_Agencia.Agencia LE AgeFin AND
    TOTAL_Agencia.Fecha EQ Fec_Gestion 
    BREAK BY Total_Agencia.Tipo_Producto BY Total_Agencia.Clase_Producto BY TOTAL_Agencia.Codigo:  
    j = j + 1.
    RUN Progreso.
    IF AgeIni NE AgeFin THEN DO:
       FIND TAge WHERE TAge.TTip EQ TOTAL_Agencia.Tipo_Producto AND
                       TAge.TCla EQ TOTAL_Agencia.Clase_Producto AND
                       TAge.TCod EQ TOTAL_Agencia.Codigo NO-ERROR.
       IF NOT AVAILABLE TAge THEN DO:
          CREATE TAge.
          ASSIGN TAge.TTip = TOTAL_Agencia.Tipo_Producto
                 TAge.TCla = TOTAL_Agencia.Clase_Producto
                 TAge.TCod = TOTAL_Agencia.Codigo.
       END.
       ASSIGN TCNv = TCnv + Total_agencia.Cta_nuevas
              TCRt = TCrt + Total_agencia.Cta_Retiradas
              TCTo = TCto + Total_agencia.Cta_totales
              TSdo = TSdo + Total_agencia.Sdo_dia
              TTPo = TTpo + Total_agencia.Tasa_Ponderada
              TTpr = TTpr + Total_agencia.Tasa_promedio
              TVAt = Tvat + Total_agencia.Val_Atraso 
              TCon = TCon + Total_agencia.Vr_Consignado
              TRet = TRet + Total_agencia.Vr_Retirado. 
    END.
    ELSE DO:
      IF FIRST-OF(Total_Agencia.Tipo_Producto) THEN DO:
         CASE Total_Agencia.Tipo_Producto:
           WHEN 1 THEN NomTipPdt = "GESTION DE AHORROS".
           WHEN 2 THEN NomTipPdt = "GESTION DE CRÉDITOS".
           WHEN 3 THEN NomTipPdt = "GESTION CONTABLE".
         END CASE.
         DISPLAY  "------------------------------------------------------------------------------------------------------------------------" AT 1
                  NomTipPdt AT 1 
                  "------------------------------------------------------------------------------------------------------------------------" AT 1
         WITH FRAME FTipoPdt USE-TEXT NO-BOX WIDTH 132 NO-LABELS STREAM-IO.
      END.
      IF FIRST-OF(Total_Agencia.Clase_Producto) THEN DO:
         IF Total_Agencia.Tipo_Producto EQ 1 THEN DO:
            CASE Total_Agencia.Clase_Producto:
               WHEN 1 THEN NomClaPdt = "AHORRO A LA VISTA".
               WHEN 2 THEN NomClaPdt = "CONTRACTUAL".
               WHEN 3 THEN NomClaPdt = "A TÉRMINO".
               WHEN 4 THEN NomClaPdt = "APORTES".
            END CASE.
         END.
         IF Total_Agencia.Tipo_Producto EQ 2 THEN DO:
            CASE Total_Agencia.Clase_Producto:
               WHEN 1 THEN NomClaPdt = "CONSUMO".
               WHEN 2 THEN NomClaPdt = "COMERCIAL".
               WHEN 3 THEN NomClaPdt = "HIPOTECARIO".
               WHEN 4 THEN NomClaPdt = "MICROCRÉDITO".
            END CASE.
         END.
         IF Total_Agencia.Tipo_Producto EQ 3 THEN DO:
            CASE Total_Agencia.Clase_Producto:
               WHEN 1 THEN NomClaPdt = "ACTIVOS".
               WHEN 2 THEN NomClaPdt = "PASIVOS".
               WHEN 3 THEN NomClaPdt = "PATRIMONIO".
               WHEN 4 THEN NomClaPdt = "INGRESOS".
               WHEN 5 THEN NomClaPdt = "GASTOS".
            END CASE.
         END.
         DISPLAY SKIP(1) NomClaPdt WITH FRAME FClaPdt USE-TEXT NO-BOX NO-LABELS.
      END.
      CASE Total_Agencia.Tipo_Producto:
        WHEN 1 THEN DO:
           FIND Pro_Ahorros WHERE
                Pro_Ahorros.Tip_Ahorro EQ TOTAL_Agencia.Clase_Producto AND
                Pro_Ahorros.Cod_Ahorro EQ INTEGER(TOTAL_Agencia.Codigo) NO-LOCK NO-ERROR.
           IF AVAILABLE Pro_Ahorros THEN NomProduc = Pro_Ahorros.Nom_producto.
        END.
        WHEN 2 THEN DO:
           FIND Pro_Creditos WHERE
                Pro_Creditos.Tip_Credito EQ TOTAL_Agencia.Clase_Producto AND
                Pro_Creditos.Cod_Credito EQ INTEGER(TOTAL_Agencia.Codigo) NO-LOCK NO-ERROR.
           IF AVAILABLE Pro_Creditos THEN NomProduc = Pro_Creditos.Nom_producto.
        END.
        WHEN 3 THEN DO:
           FIND Cuentas WHERE
                Cuentas.Id_Cuenta EQ TOTAL_Agencia.Clase_Producto AND
                Cuentas.Cuenta    EQ TOTAL_Agencia.Codigo NO-LOCK NO-ERROR.
           IF AVAILABLE Cuentas THEN NomProduc = Cuentas.Nombre.
        END.

      END CASE.
      DISPLAY NomProduc                       AT 1  FORMAT "X(19)"
              Total_Agencia.Cta_Nuevas        AT 21 FORMAT ">>,>>9"
              Total_Agencia.Cta_Retiradas     AT 28 FORMAT ">>,>>9"
              Total_Agencia.Cta_Totales       AT 35 FORMAT ">>,>>>,>>9"
              Total_Agencia.Sdo_Dia           AT 48 FORMAT "->>>,>>>,>>9"
              Total_Agencia.Tasa_Promedio     AT 62 FORMAT ">>>.9999"
              Total_Agencia.Tasa_Ponderada    AT 71 FORMAT ">>>.9999"
              Total_Agencia.Val_Atraso        AT 80 FORMAT "->>>,>>>,>>9"
              Total_Agencia.Vr_Consignado     AT 94 FORMAT "->>>,>>>,>>9"
              Total_Agencia.Vr_Retirado       AT 107 FORMAT "->>>,>>>,>>9"
              WITH FRAME F_gestionMov WIDTH 132 USE-TEXT NO-BOX NO-LABELS STREAM-IO.
       ASSIGN Tot_CtaNva = Tot_CtaNva + TOTAL_Agencia.Cta_Nuevas
              Tot_CtaRet = Tot_CtaRet + TOTAL_Agencia.Cta_Retiradas
              Tot_CtaTot = Tot_CtaTot + TOTAL_Agencia.Cta_Totales
              Tot_Saldos = Tot_Saldos + TOTAL_Agencia.Sdo_Dia
              Tot_Atraso = Tot_Atraso + TOTAL_Agencia.Val_Atraso
              Tot_Consig = Tot_Consig + TOTAL_Agencia.Vr_Consignado
              Tot_Retiro = Tot_Retiro + TOTAL_Agencia.Vr_Retirado.
       IF LAST-OF(TOTAL_Agencia.Clase_Producto) THEN DO:
          DISPLAY SKIP(1)
                  "Total:"      AT 1 
                  NomClaPdt     AT 8 FORMAT "X(12)"
                  Tot_CtaNva    AT 21 FORMAT ">>,>>9"
                  Tot_CtaRet    AT 28 FORMAT ">>,>>9"
                  Tot_CtaTot    AT 35 FORMAT ">>,>>>,>>9"
                  Tot_Saldos    AT 48 FORMAT "->>>,>>>,>>9"
                  Tot_Atraso    AT 80 FORMAT "->>>,>>>,>>9"
                  Tot_Consig    AT 94 FORMAT "->>>,>>>,>>9"
                  Tot_Retiro    AT 107 FORMAT "->>>,>>>,>>9"
                  SKIP(1)
                  WITH FRAME F_TotCla WIDTH 132 USE-TEXT NO-BOX NO-LABELS STREAM-IO.
           ASSIGN Tot_CtaNva = 0 Tot_CtaRet = 0 Tot_CtaTot = 0 Tot_Saldos = 0 Tot_Atraso = 0 Tot_Consig = 0 Tot_Retiro = 0.
       END.
    END.
  END.
  IF AgeIni NE AgeFin THEN RUN Consolidado_Agencias.
  VIEW FRAME F-Ftr.
  PAGE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Consolidado_Agencias wWin 
PROCEDURE Consolidado_Agencias :
DEFINE VAR TotReg AS DECIMAL.
DEFINE VAR NomTipPdt AS CHARACTER FORMAT "X(30)".
DEFINE VAR NomClaPdt AS CHARACTER FORMAT "X(15)".
  
  ASSIGN j = 0 k = 0.
  FOR EACH TAge WHERE BREAK BY TAge.TTip BY TAge.TCla BY TAge.TCod:  
    j = j + 1.
    RUN Progreso.
      IF FIRST-OF(TAge.TTip) THEN DO:
         CASE TAge.TTip:
           WHEN 1 THEN NomTipPdt = "GESTION DE AHORROS".
           WHEN 2 THEN NomTipPdt = "GESTION DE CRÉDITOS".
           WHEN 3 THEN NomTipPdt = "GESTION CONTABLE".
         END CASE.
         DISPLAY  "------------------------------------------------------------------------------------------------------------------------" AT 1
                  NomTipPdt AT 1 
                  "------------------------------------------------------------------------------------------------------------------------" AT 1
         WITH FRAME FTipoPdt USE-TEXT NO-BOX WIDTH 132 NO-LABELS STREAM-IO.
      END.
      IF FIRST-OF(TAge.TCla) THEN DO:
         IF TAge.TTip EQ 1 THEN DO:
            CASE TAge.TCla:
               WHEN 1 THEN NomClaPdt = "AHORRO A LA VISTA".
               WHEN 2 THEN NomClaPdt = "CONTRACTUAL".
               WHEN 3 THEN NomClaPdt = "A TÉRMINO".
               WHEN 4 THEN NomClaPdt = "APORTES".
            END CASE.
         END.
         IF TAge.TTip EQ 2 THEN DO:
            CASE TAge.TCla:
               WHEN 1 THEN NomClaPdt = "CONSUMO".
               WHEN 2 THEN NomClaPdt = "COMERCIAL".
               WHEN 3 THEN NomClaPdt = "HIPOTECARIO".
               WHEN 4 THEN NomClaPdt = "MICROCRÉDITO".
            END CASE.
         END.
         IF TAge.TTip EQ 3 THEN DO:
            CASE TAge.TCla:
               WHEN 1 THEN NomClaPdt = "ACTIVOS".
               WHEN 2 THEN NomClaPdt = "PASIVOS".
               WHEN 3 THEN NomClaPdt = "PATRIMONIO".
               WHEN 4 THEN NomClaPdt = "INGRESOS".
               WHEN 5 THEN NomClaPdt = "GASTOS".
            END CASE.
         END.
         DISPLAY SKIP(1) NomClaPdt WITH FRAME FClaPdt USE-TEXT NO-BOX NO-LABELS.
      END.
      CASE TAge.TTip:
        WHEN 1 THEN DO:
           FIND Pro_Ahorros WHERE
                Pro_Ahorros.Tip_Ahorro EQ TAge.TCla AND
                Pro_Ahorros.Cod_Ahorro EQ INTEGER(TAge.TCod) NO-LOCK NO-ERROR.
           IF AVAILABLE Pro_Ahorros THEN NomProduc = Pro_Ahorros.Nom_producto.
        END.
        WHEN 2 THEN DO:
           FIND Pro_Creditos WHERE
                Pro_Creditos.Tip_Credito EQ TAge.TCla AND
                Pro_Creditos.Cod_Credito EQ INTEGER(TAge.TCod) NO-LOCK NO-ERROR.
           IF AVAILABLE Pro_Creditos THEN NomProduc = Pro_Creditos.Nom_producto.
        END.
        WHEN 3 THEN DO:
           FIND Cuentas WHERE
                Cuentas.Id_Cuenta EQ TAge.TCla AND
                Cuentas.Cuenta    EQ TAge.TCod NO-LOCK NO-ERROR.
           IF AVAILABLE Cuentas THEN NomProduc = Cuentas.Nombre.
        END.
      END CASE.
      DISPLAY NomProduc          AT 1  FORMAT "X(19)"
              TAge.TCNv          AT 21 FORMAT ">>,>>9"
              TAge.TCRt          AT 28 FORMAT ">>,>>9"
              TAge.TCto          AT 35 FORMAT ">>,>>>,>>9"
              TAge.TSdo          AT 48 FORMAT "->>>,>>>,>>9"
              TAge.TTPr          AT 62 FORMAT ">>>.9999"
              TAge.TTPo          AT 71 FORMAT ">>>.9999"
              TAge.TVat          AT 80 FORMAT "->>>,>>>,>>9"
              TAge.TCon          AT 94 FORMAT "->>>,>>>,>>9"
              TAge.TRet          AT 107 FORMAT "->>>,>>>,>>9"
              WITH FRAME F_gestionMov WIDTH 132 USE-TEXT NO-BOX NO-LABELS STREAM-IO.
       ASSIGN Tot_CtaNva = Tot_CtaNva + TAge.TCNv
              Tot_CtaRet = Tot_CtaRet + TAge.TCRt
              Tot_CtaTot = Tot_CtaTot + TAge.TCTo
              Tot_Saldos = Tot_Saldos + TAge.TSdo
              Tot_Atraso = Tot_Atraso + TAge.TVat
              Tot_Consig = Tot_Consig + TAge.TCon
              Tot_Retiro = Tot_Retiro + TAge.TRet.
       IF LAST-OF(TAge.TCla) THEN DO:
          DISPLAY SKIP(1)
                  "Total:"      AT 1 
                  NomClaPdt     AT 8 FORMAT "X(12)"
                  Tot_CtaNva    AT 21 FORMAT ">>,>>9"
                  Tot_CtaRet    AT 28 FORMAT ">>,>>9"
                  Tot_CtaTot    AT 35 FORMAT ">>,>>>,>>9"
                  Tot_Saldos    AT 48 FORMAT "->>>,>>>,>>9"
                  Tot_Atraso    AT 80 FORMAT "->>>,>>>,>>9"
                  Tot_Consig    AT 94 FORMAT "->>>,>>>,>>9"
                  Tot_Retiro    AT 107 FORMAT "->>>,>>>,>>9"
                  SKIP(1)
                  WITH FRAME F_TotCla WIDTH 132 USE-TEXT NO-BOX NO-LABELS STREAM-IO.
           ASSIGN Tot_CtaNva = 0 Tot_CtaRet = 0 Tot_CtaTot = 0 Tot_Saldos = 0 Tot_Atraso = 0 Tot_Consig = 0 Tot_Retiro = 0.
       END.
  END.
  /*DISPLAY SKIP "Total de Registros Reportados: " TotReg WITH FRAME FT WIDTH 132 NO-LABELS.*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE De_Gestion wWin 
PROCEDURE De_Gestion :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI wWin  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
  THEN DELETE WIDGET wWin.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI wWin  _DEFAULT-ENABLE
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
  DISPLAY Cmb_Agencias Fec_Gestion RTipo E1 
      WITH FRAME F_Gestion IN WINDOW wWin.
  ENABLE Cmb_Agencias Fec_Gestion BUTTON-143 BUTTON-153 RTipo BUTTON-149 E1 
         BUTTON-148 BUTTON-150 BUTTON-152 
      WITH FRAME F_Gestion IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Gestion}
  DISPLAY Buscar 
      WITH FRAME F_Buscar IN WINDOW wWin.
  ENABLE Buscar BUTTON-154 Btn_anterior Btn_siguiente 
      WITH FRAME F_Buscar IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Buscar}
  ENABLE IMAGE-2 R1 R2 R3 R4 R5 R6 R7 R8 R9 
      WITH FRAME F_Progreso IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Progreso}
  VIEW wWin.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE exitObject wWin 
PROCEDURE exitObject :
/*------------------------------------------------------------------------------
  Purpose:  Window-specific override of this procedure which destroys 
            its contents and itself.
    Notes:  
------------------------------------------------------------------------------*/

  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Imprimir_Excel wWin 
PROCEDURE Imprimir_Excel :
MESSAGE "Opción deshabilitada" VIEW-AS ALERT-BOX INFORMATION.
/*CASE Cmb_Tipo:
  WHEN "Clientes" THEN RUN ImpEx_Clientes.
  WHEN "Ahorros"  THEN RUN ImpEx_Ahorros.
END CASE.*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject wWin 
PROCEDURE initializeObject :
RUN SUPER.
  FOR EACH Agencias  NO-LOCK:
    W_Ok = Cmb_Agencias:ADD-LAST(STRING(Agencias.Agencia,"999") + " - " + Agencias.Nombre) IN FRAME F_Gestion.
    IF Agencias.Agencia EQ W_Agencia THEN
       Cmb_Agencias:SCREEN-VALUE IN FRAME F_Gestion = STRING(Agencias.Agencia,"999") + " - " + Agencias.Nombre.
  END.
  Fec_Gestion:SCREEN-VALUE IN FRAME F_Gestion = STRING(TODAY - 1).  
  HIDE FRAME F_Buscar.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Progreso wWin 
PROCEDURE Progreso :
DO WITH FRAME F_Progreso:
  IF j EQ 250 THEN DO:
        j = 0.
        k = k + 1.
        ASSIGN R1:BGCOLOR = 15 R2:BGCOLOR = 15 R3:BGCOLOR = 15 R4:BGCOLOR = 15
               R5:BGCOLOR = 15 R6:BGCOLOR = 15 R7:BGCOLOR = 15 R8:BGCOLOR = 15.
               R9:BGCOLOR = 15.
        CASE k:
          WHEN 1 THEN
             R1:BGCOL = 18.
          WHEN 2 THEN
             R2:BGCOL = 18.
          WHEN 3 THEN
             R3:BGCOL = 18.
          WHEN 4 THEN
             R4:BGCOL = 18.
          WHEN 5 THEN
             R5:BGCOL = 18.
          WHEN 6 THEN
             R6:BGCOL = 18.
          WHEN 7 THEN
             R7:BGCOL = 18.
          WHEN 8 THEN
             R8:BGCOL = 18.
          WHEN 9 THEN
             R9:BGCOL = 18.
        END CASE.
        IF k = 9 THEN k = 0.
   END.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

