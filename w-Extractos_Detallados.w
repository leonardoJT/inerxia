&ANALYZE-SUSPEND _VERSION-NUMBER AB_v9r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          bdcentral        PROGRESS
*/
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

/* Parameters Definitions ---                                           */
 {Incluido/Variable.I "SHARED"}
/* Local Variable Definitions ---                                       */

  DEFINE VAR W_Ok AS LOGICAL.
  DEFINE VAR W_EstAnterior AS CHARACTER FORMAT "X(30)".

  DEFINE VAR W_Age  LIKE Ahorros.Agencia.
  DEFINE VAR W_Pro  LIKE Ahorros.Cod_Ahorro.
  DEFINE VAR W_Nit  LIKE Ahorros.Nit.
  DEFINE VAR W_Cue  LIKE Ahorros.Cue_Ahorros.

  DEFINE VARIABLE WImagen  AS CHARACTER FORMAT "X(40)". 
  DEFINE VARIABLE WAno     AS INTEGER FORMAT "9999".    
  DEFINE VARIABLE WMes     AS INTEGER FORMAT "99".      
  DEFINE VARIABLE WFec     AS DATE.   

  DEFINE VARIABLE MesIni  AS INTEGER FORMAT "99".
  DEFINE VARIABLE MesFin  AS INTEGER FORMAT "99".
  DEFINE VARIABLE FecIni  AS DATE FORMAT "99/99/9999".
  DEFINE VARIABLE FecFin  AS DATE FORMAT "99/99/9999".

  DEFINE TEMP-TABLE Tmp
      FIELD Nit LIKE Clientes.Nit
      FIELD Nom AS CHARACTER FORMAT "X(30)"
      FIELD Fec AS DATE FORMAT "99/99/99"
      FIELD Tip AS CHARACTER FORMAT "X(3)"
      FIELD Doc LIKE Mov_Ahorros.Num_Documento
      FIELD Usu LIKE Mov_Ahorros.Usuario
      FIELD Con AS DECIMAL FORMAT "->>>,>>>,>>9"
      FIELD Ret AS DECIMAL FORMAT "->>>,>>>,>>9".

    DEFINE VAR Listado AS CHARACTER INITIAL "".

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F_Cambio

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES Ahorros

/* Definitions for FRAME F_Cambio                                       */
&Scoped-define QUERY-STRING-F_Cambio FOR EACH Ahorros SHARE-LOCK
&Scoped-define OPEN-QUERY-F_Cambio OPEN QUERY F_Cambio FOR EACH Ahorros SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-F_Cambio Ahorros
&Scoped-define FIRST-TABLE-IN-QUERY-F_Cambio Ahorros


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BUTTON-59 BUTTON-149 BUTTON-121 BUTTON-120 ~
BUTTON-66 E1 Btn_Extracto BUTTON-63 BUTTON-64 Img_MesF Img_MesI RECT-282 ~
RECT-283 
&Scoped-Define DISPLAYED-OBJECTS W_NomAgencia W_NomProducto WCuenta WNit ~
NomCliente DiaFin AnoFin DiaIni AnoIni E1 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Extracto 
     LABEL "Extracto" 
     SIZE 11 BY 1.62.

DEFINE BUTTON BUTTON-120 
     IMAGE-UP FILE "imagenes/arwdwn.gif":U
     LABEL "Button 120" 
     SIZE 3 BY .54.

DEFINE BUTTON BUTTON-121 
     IMAGE-UP FILE "imagenes/arwdwn.gif":U
     LABEL "Button 121" 
     SIZE 3 BY .54.

DEFINE BUTTON BUTTON-149 
     IMAGE-UP FILE "imagenes/impresora2.bmp":U
     LABEL "Button 149" 
     SIZE 11 BY 1.62.

DEFINE BUTTON BUTTON-59 
     IMAGE-UP FILE "imagenes/informacion.bmp":U
     LABEL "Button 59" 
     SIZE 11 BY 1.62.

DEFINE BUTTON BUTTON-63 
     LABEL "Salir" 
     SIZE 11 BY 1.38.

DEFINE BUTTON BUTTON-64 
     IMAGE-UP FILE "imagenes/interrogacion.bmp":U
     LABEL "Button 64" 
     SIZE 5 BY 1.12.

DEFINE BUTTON BUTTON-66 
     IMAGE-UP FILE "imagenes/lupa.bmp":U
     LABEL "Button 66" 
     SIZE 11 BY 1.62.

DEFINE VARIABLE E1 AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL LARGE
     SIZE 93 BY 12.92
     BGCOLOR 15 FONT 2 NO-UNDO.

DEFINE VARIABLE AnoFin AS INTEGER FORMAT "9999":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 6 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE AnoIni AS INTEGER FORMAT "9999":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 6 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE DiaFin AS INTEGER FORMAT "99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 3 BY .81 NO-UNDO.

DEFINE VARIABLE DiaIni AS INTEGER FORMAT "99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 3 BY .81 NO-UNDO.

DEFINE VARIABLE NomCliente AS CHARACTER FORMAT "X(50)":U 
     VIEW-AS FILL-IN 
     SIZE 29 BY .81
     BGCOLOR 18 FGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE WCuenta AS CHARACTER FORMAT "X(14)" INITIAL "?" 
     LABEL "Cuenta" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .81
     BGCOLOR 15 .

DEFINE VARIABLE WNit AS CHARACTER FORMAT "X(12)" 
     LABEL "Nit" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .81
     BGCOLOR 15 .

DEFINE VARIABLE W_NomAgencia AS CHARACTER FORMAT "X(30)":U 
     LABEL "Agencia" 
     VIEW-AS FILL-IN 
     SIZE 24 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_NomProducto AS CHARACTER FORMAT "X(256)":U 
     LABEL "Producto" 
     VIEW-AS FILL-IN 
     SIZE 42 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE IMAGE Img_MesF
     FILENAME "adeicon/blank":U
     SIZE 11 BY .81.

DEFINE IMAGE Img_MesI
     FILENAME "adeicon/blank":U
     SIZE 11 BY .81.

DEFINE RECTANGLE RECT-282
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 29 BY 2.15.

DEFINE RECTANGLE RECT-283
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 28 BY 2.15.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY F_Cambio FOR 
      Ahorros SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F_Cambio
     W_NomAgencia AT ROW 1.27 COL 14 COLON-ALIGNED
     W_NomProducto AT ROW 1.27 COL 50 COLON-ALIGNED
     BUTTON-59 AT ROW 1.27 COL 99
     WCuenta AT ROW 2.08 COL 14 COLON-ALIGNED
     WNit AT ROW 2.08 COL 50 COLON-ALIGNED
     NomCliente AT ROW 2.08 COL 63 COLON-ALIGNED NO-LABEL
     BUTTON-149 AT ROW 2.88 COL 99
     BUTTON-121 AT ROW 4.19 COL 73.86
     DiaFin AT ROW 4.23 COL 52 COLON-ALIGNED NO-LABEL
     AnoFin AT ROW 4.23 COL 66 COLON-ALIGNED NO-LABEL
     DiaIni AT ROW 4.31 COL 24 COLON-ALIGNED NO-LABEL
     AnoIni AT ROW 4.35 COL 38 COLON-ALIGNED NO-LABEL
     BUTTON-120 AT ROW 4.35 COL 46
     BUTTON-66 AT ROW 4.5 COL 99
     E1 AT ROW 5.85 COL 3 NO-LABEL
     Btn_Extracto AT ROW 6.12 COL 99
     BUTTON-63 AT ROW 10.15 COL 99
     BUTTON-64 AT ROW 12.04 COL 102
     Img_MesF AT ROW 4.23 COL 57
     Img_MesI AT ROW 4.35 COL 29
     RECT-282 AT ROW 3.15 COL 21
     RECT-283 AT ROW 3.15 COL 50
     "Fecha Inicial" VIEW-AS TEXT
          SIZE 11 BY .69 AT ROW 3.54 COL 29
          BGCOLOR 17 FGCOLOR 7 
     "Fecha de Corte" VIEW-AS TEXT
          SIZE 15 BY .62 AT ROW 3.46 COL 57
          BGCOLOR 17 FGCOLOR 7 
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 110.86 BY 18.12
         BGCOLOR 17 FONT 5.


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
         TITLE              = "Cambio de Estados a las Cuentas de Ahorro"
         HEIGHT             = 18.12
         WIDTH              = 110.86
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
/* SETTINGS FOR FRAME F_Cambio
                                                                        */
/* SETTINGS FOR FILL-IN AnoFin IN FRAME F_Cambio
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN AnoIni IN FRAME F_Cambio
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN DiaFin IN FRAME F_Cambio
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN DiaIni IN FRAME F_Cambio
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN NomCliente IN FRAME F_Cambio
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN WCuenta IN FRAME F_Cambio
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN WNit IN FRAME F_Cambio
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_NomAgencia IN FRAME F_Cambio
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_NomProducto IN FRAME F_Cambio
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Cambio
/* Query rebuild information for FRAME F_Cambio
     _TblList          = "bdCentral.Ahorros"
     _Query            is OPENED
*/  /* FRAME F_Cambio */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Cambio de Estados a las Cuentas de Ahorro */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Cambio de Estados a las Cuentas de Ahorro */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Extracto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Extracto wWin
ON CHOOSE OF Btn_Extracto IN FRAME F_Cambio /* Extracto */
DO:
  RUN Sacar_Extractos.
  W_ok = e1:READ-FILE(Listado) IN FRAME F_Cambio.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-120
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-120 wWin
ON CHOOSE OF BUTTON-120 IN FRAME F_Cambio /* Button 120 */
DO:
  RUN C-Fecha.r (OUTPUT WImagen, OUTPUT WAno, OUTPUT WMes, OUTPUT WFec).
  ASSIGN W_OK = Img_MesI:LOAD-IMAGE("imagenes\month" + STRING(WMes) + ".gif")
         AnoIni:SCREEN-VALUE = STRING(WAno)
         AnoIni = WAno
         MesIni = WMes
         FecIni = WFec
         DiaIni = INTEGER(DAY(WFec)).
  DISPLAY DiaIni WITH FRAME F_Cambio.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-121
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-121 wWin
ON CHOOSE OF BUTTON-121 IN FRAME F_Cambio /* Button 121 */
DO:
  RUN C-Fecha.r (OUTPUT WImagen, OUTPUT WAno, OUTPUT WMes, OUTPUT WFec).
  ASSIGN W_OK = Img_MesF:LOAD-IMAGE("imagenes\month" + STRING(WMes) + ".gif")
         AnoFin:SCREEN-VALUE = STRING(WAno)
         AnoFin = WAno
         MesFin = WMes
         FecFin = WFec
         DiaFin = INTEGER(DAY(WFec)).
  DISPLAY DiaFin WITH FRAME F_Cambio.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-149
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-149 wWin
ON CHOOSE OF BUTTON-149 IN FRAME F_Cambio /* Button 149 */
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


&Scoped-define SELF-NAME BUTTON-63
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-63 wWin
ON CHOOSE OF BUTTON-63 IN FRAME F_Cambio /* Salir */
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


&Scoped-define SELF-NAME BUTTON-66
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-66 wWin
ON CHOOSE OF BUTTON-66 IN FRAME F_Cambio /* Button 66 */
DO:
  RUN C-Ahorros.r (INPUT "", 
                   OUTPUT W_Age, OUTPUT W_Pro, OUTPUT W_Nit, OUTPUT W_Cue). 
  FIND Ahorros WHERE Ahorros.Agencia      EQ W_Age AND
                     Ahorros.Cod_Ahorro   EQ W_Pro AND
                     Ahorros.Cue_Ahorros  EQ W_Cue AND
                     Ahorros.Nit          EQ W_Nit NO-ERROR.
  IF AVAILABLE Ahorros THEN DO:
     FIND FIRST Pro_Ahorros WHERE Pro_Ahorros.Cod_ahorro EQ W_Pro NO-LOCK NO-ERROR.
     IF AVAILABLE Pro_Ahorros THEN DO:
        W_NomProducto:SCREEN-VALUE = STRING(Pro_Ahorros.Cod_Ahorro,"999") + " - " + Pro_Ahorros.Nom_Producto.
        FIND Agencias WHERE Agencias.Agencia EQ W_Age NO-LOCK NO-ERROR.
        IF AVAILABLE Agencias THEN
          W_NomAgencia:SCREEN-VALUE = STRING(Agencias.Agencia,"999") + " - " + Agencias.Nombre.
        FIND Clientes WHERE Clientes.Nit EQ W_Nit NO-LOCK NO-ERROR.
        IF AVAILABLE Clientes THEN
           NomCliente:SCREEN-VALUE = Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2.
        ELSE
           NomCliente:SCREEN-VALUE = "Cliente No Encontrado".
        ASSIGN WCuenta:SCREEN-VALUE = Ahorros.Cue_Ahorros
               WNit:SCREEN-VALUE = Ahorros.Nit.
        
     END.
  END.  
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

  {&OPEN-QUERY-F_Cambio}
  GET FIRST F_Cambio.
  DISPLAY W_NomAgencia W_NomProducto WCuenta WNit NomCliente DiaFin AnoFin 
          DiaIni AnoIni E1 
      WITH FRAME F_Cambio IN WINDOW wWin.
  ENABLE BUTTON-59 BUTTON-149 BUTTON-121 BUTTON-120 BUTTON-66 E1 Btn_Extracto 
         BUTTON-63 BUTTON-64 Img_MesF Img_MesI RECT-282 RECT-283 
      WITH FRAME F_Cambio IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Cambio}
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject wWin 
PROCEDURE initializeObject :
ASSIGN DiaIni = INTEGER(DAY(W_Fecha - DAY(W_Fecha) + 1))
         DiaFin = INTEGER(DAY(W_Fecha))
         W_OK = Img_MesI:LOAD-IMAGE("imagenes\month" + STRING(MONTH(W_Fecha)) + ".gif") IN FRAME F_Cambio
         W_OK = Img_MesF:LOAD-IMAGE("imagenes\month" + STRING(MONTH(W_Fecha)) + ".gif")
         FecIni = DATE(STRING(W_Fecha - DAY(W_Fecha) + 1))
         FecFin = W_Fecha.
  RUN SUPER.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Sacar_Extractos wWin 
PROCEDURE Sacar_Extractos :
DEFINE VAR WTipo AS CHARACTER FORMAT "X(8)".
ASSIGN FRAME F_Cambio W_NomAgencia W_NomProducto WCuenta WNit.
DEFINE VAR TotCon AS DECIMAL FORMAT ">>>>,>>>,>>9".
DEFINE VAR TotRet AS DECIMAL FORMAT ">>>>,>>>,>>9".

FOR EACH Mov_Ahorros WHERE
         Mov_Ahorros.Agencia    EQ INTEGER(SUBSTRING(W_NomAgencia,1,3))
     AND Mov_Ahorros.Cod_Ahorro EQ INTEGER(SUBSTRING(W_NomProducto,1,3))  
     AND Mov_Ahorros.Nit        NE ""
     AND Mov_Ahorros.Nit        EQ WNit
     AND Mov_Ahorros.Cue_Ahorro EQ WCuenta
     AND Mov_Ahorros.Fecha      GE FecIni
     AND Mov_Ahorros.Fecha      LE FecFin NO-LOCK:
     FIND Operacion WHERE Operacion.Cod_Operacion EQ Mov_Ahorros.Cod_Operacion NO-LOCK NO-ERROR.
     IF AVAILABLE(Operacion) THEN DO:
         CREATE Tmp.
         ASSIGN Tmp.Nit = Mov_Ahorros.Cedula_Transac
                Tmp.Nom = Mov_Ahorros.NomApell_Trans
                Tmp.Fec = Mov_Ahorros.Fecha
                Tmp.Doc = Mov_Ahorros.Num_Documento
                Tmp.Usu = Mov_Ahorros.Usuario.
         CASE Operacion.Ctrl_EfeChe:
             WHEN 1 THEN Tmp.Tip = "Efectivo".
             WHEN 2 THEN Tmp.Tip = "Cheques".
             WHEN 3 THEN Tmp.Tip = "Ninguno".
         END CASE.
         IF Operacion.Tipo_Operacion EQ 1 THEN 
            Tmp.Con = Mov_Ahorros.Val_Efectivo + Mov_Ahorros.Val_Cheque.
         ELSE
            Tmp.Ret = Mov_Ahorros.Val_Efectivo + Mov_Ahorros.Val_Cheque.
     END.
         
END.

Listado = W_PathSpl + "InfAho-" + W_Usuario + ".Lst".
OS-DELETE VALUE(Listado).
{Incluido\RepEncabezado.i}
ASSIGN W_Reporte   = "REPORTE   : EXTRACTO DETALLADO Cuenta: " + WCuenta + " Nit: " + WNit
              + " - FECHA: " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am").
       W_EncColumna = "Nit          Nombre                  Fecha Ope Docto     Usu   Consignacion      Retiros".

OUTPUT TO value(Listado) NO-ECHO PAGED PAGE-SIZE 65.
  
  VIEW FRAME F-Encabezado.
  VIEW FRAME F-Ftr.
  ASSIGN TotCon = 0 TotRet = 0.
  FOR EACH Tmp WHERE Tmp.Nit NE "" BREAK BY Tmp.Fec:
      DISPLAY Tmp.Nit Tmp.Nom FORMAT "X(20)" Tmp.Fec Tmp.Tip Tmp.Doc Tmp.Usu Tmp.Con Tmp.Ret
          WITH FRAME F_Ex WIDTH 132 NO-LABELS USE-TEXT NO-BOX STREAM-IO.
      ASSIGN TotCon = TotCon + Tmp.Con
             TotRet = TotRet + Tmp.Ret.
  END.
  DISPLAY SKIP(1) 
          "Total de transacciones : " AT 30
          TotCon AT 64
          Totret AT 77 WITH FRAME F_t WIDTH 180 NO-LABELS USE-TEXT NO-BOX STREAM-IO.
  PAGE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

