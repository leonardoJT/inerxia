&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          bdcentral        PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.
{Incluido\variable.i "shared"}
/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

 /*para buscar cuentas destino y cuentas debito automatico*/
  DEFINE VARIABLE Puntero AS ROWID.
  DEFINE VAR W_Age  LIKE Ahorros.Agencia.
  DEFINE VAR W_Pro  LIKE Ahorros.Cod_Ahorro.
  DEFINE VAR W_Nit  LIKE Ahorros.Nit.
  DEFINE VAR W_Cue  LIKE Ahorros.Cue_Ahorros.
  DEFINE VAR W_vlr  LIKE Ahorros.Sdo_Disponible INITIAL 0.
  DEFINE VARIABLE P_Nit      LIKE Clientes.Nit.
  DEFINE VARIABLE p_Nombre   LIKE Clientes.Nombre.
  DEFINE VARIABLE P_Apellido LIKE Clientes.Apellido1.
  DEFINE VARIABLE P_AgeCli   LIKE Clientes.Agencia.
  DEFINE VARIABLE W_Error       AS LOGICAL.
  DEFINE VARIABLE W_Autorizo  LIKE Usuarios.Usuario.
  DEFINE VAR W_Ok AS LOGICAL.
  DEFINE VARIABLE WAno     AS INTEGER FORMAT "9999".    
  DEFINE VARIABLE WMes     AS INTEGER FORMAT "99".      
  DEFINE VARIABLE WFec     AS DATE.   
  DEFINE VARIABLE WImagen  AS CHARACTER FORMAT "X(40)". 
  DEFINE VARIABLE FecIni   AS DATE.
  DEFINE VARIABLE FecFin   AS DATE.
  DEFINE VARIABLE MesIni   AS INTEGER FORMAT "99".
  DEFINE VARIABLE MesFin   AS INTEGER FORMAT "99".

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Frm_Creditos
&Scoped-define BROWSE-NAME Brw_Creditos

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES Creditos Ahorros Solicitud

/* Definitions for BROWSE Brw_Creditos                                  */
&Scoped-define FIELDS-IN-QUERY-Brw_Creditos Creditos.Agencia Creditos.Num_Credito Creditos.Pagare Creditos.Cuota Creditos.For_pago   
&Scoped-define ENABLED-FIELDS-IN-QUERY-Brw_Creditos   
&Scoped-define SELF-NAME Brw_Creditos
&Scoped-define QUERY-STRING-Brw_Creditos FOR EACH Creditos             WHERE Creditos.Nit = "Clientes.nit"             AND Creditos.estado EQ 2 NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-Brw_Creditos OPEN QUERY {&SELF-NAME} FOR EACH Creditos             WHERE Creditos.Nit = "Clientes.nit"             AND Creditos.estado EQ 2 NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-Brw_Creditos Creditos
&Scoped-define FIRST-TABLE-IN-QUERY-Brw_Creditos Creditos


/* Definitions for FRAME Frm_Creditos                                   */
&Scoped-define OPEN-BROWSERS-IN-QUERY-Frm_Creditos ~
    ~{&OPEN-QUERY-Brw_Creditos}
&Scoped-define SELF-NAME Frm_Creditos
&Scoped-define QUERY-STRING-Frm_Creditos FOR EACH Ahorros SHARE-LOCK
&Scoped-define OPEN-QUERY-Frm_Creditos OPEN QUERY {&SELF-NAME} FOR EACH Ahorros SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-Frm_Creditos Ahorros
&Scoped-define FIRST-TABLE-IN-QUERY-Frm_Creditos Ahorros


/* Definitions for FRAME F_ForPago                                      */
&Scoped-define FIELDS-IN-QUERY-F_ForPago Solicitud.Age_DebAutomatico ~
Solicitud.Cod_DebAutomatico 
&Scoped-define QUERY-STRING-F_ForPago FOR EACH Solicitud SHARE-LOCK
&Scoped-define OPEN-QUERY-F_ForPago OPEN QUERY F_ForPago FOR EACH Solicitud SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-F_ForPago Solicitud
&Scoped-define FIRST-TABLE-IN-QUERY-F_ForPago Solicitud


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS Creditos.Nit 
&Scoped-define ENABLED-TABLES Creditos
&Scoped-define FIRST-ENABLED-TABLE Creditos
&Scoped-Define ENABLED-OBJECTS Brw_Creditos btn_procesar Btn_Cancelar ~
BtnDone Btn_Consulta BUTTON-1 BUTTON-95 Btn_Aceptar RECT-274 RECT-309 
&Scoped-Define DISPLAYED-FIELDS Creditos.Nit 
&Scoped-define DISPLAYED-TABLES Creditos
&Scoped-define FIRST-DISPLAYED-TABLE Creditos
&Scoped-Define DISPLAYED-OBJECTS W_NomTitular 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-6 Btn_Cancelar BtnDone Btn_Consulta BUTTON-1 BUTTON-95 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BtnDone DEFAULT 
     LABEL "&Salir" 
     SIZE 10 BY 1.62
     BGCOLOR 8 FONT 4.

DEFINE BUTTON Btn_Aceptar 
     IMAGE-UP FILE "imagenes\impresora2":U
     LABEL "Ace&ptar" 
     SIZE 10 BY 1.58.

DEFINE BUTTON Btn_Cancelar 
     LABEL "&Cancelar" 
     SIZE 10 BY 1.65
     FONT 4.

DEFINE BUTTON Btn_Consulta 
     IMAGE-UP FILE "imagenes/lupa.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/lupa.bmp":U
     LABEL "Button 3" 
     SIZE 10 BY 1.62.

DEFINE BUTTON btn_procesar 
     LABEL "&Guardar" 
     SIZE 10 BY 1.65
     FONT 4.

DEFINE BUTTON BUTTON-1 
     IMAGE-UP FILE "imagenes/informacion.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/informacion.bmp":U
     LABEL "Button 1" 
     SIZE 10 BY 1.62.

DEFINE BUTTON BUTTON-95 
     IMAGE-UP FILE "imagenes/interrogacion.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/interrogacion.bmp":U
     LABEL "Button 95" 
     SIZE 4 BY 1.12.

DEFINE VARIABLE W_NomTitular AS CHARACTER FORMAT "X(50)":U 
     LABEL "Nombre" 
     VIEW-AS FILL-IN 
     SIZE 46 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-274
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 12 BY 5.42.

DEFINE RECTANGLE RECT-309
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 86 BY 7.

DEFINE VARIABLE Cue_DebAutomatico AS CHARACTER FORMAT "X(14)" 
     LABEL "Cuenta" 
     VIEW-AS FILL-IN 
     SIZE 29 BY .81
     BGCOLOR 18 FGCOLOR 15 .

DEFINE VARIABLE W_NomAgeDebAutomatico AS CHARACTER FORMAT "X(30)":U 
     VIEW-AS FILL-IN 
     SIZE 25 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_NomCodDebAutomatico AS CHARACTER FORMAT "X(30)":U 
     VIEW-AS FILL-IN 
     SIZE 25 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Brw_Creditos FOR 
      Creditos SCROLLING.

DEFINE QUERY Frm_Creditos FOR 
      Ahorros SCROLLING.

DEFINE QUERY F_ForPago FOR 
      Solicitud SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Brw_Creditos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Brw_Creditos C-Win _FREEFORM
  QUERY Brw_Creditos NO-LOCK DISPLAY
      Creditos.Agencia         FORMAT "999":U                    LABEL "Agencia"
      Creditos.Num_Credito     FORMAT "99999999":U                    LABEL "Nro. Titulo"
      Creditos.Pagare          FORMAT "99999999":U  LABEL "Pagare"
      Creditos.Cuota           FORMAT ">>>,>>>,>>9":U            LABEL "Cuota"
      Creditos.For_pago        FORMAT "999":U                  LABEL "Plazo"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 83.57 BY 5.12
         FONT 5 ROW-HEIGHT-CHARS .58 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Frm_Creditos
     Brw_Creditos AT ROW 2.88 COL 3
     Creditos.Nit AT ROW 1.81 COL 5 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 15 BY .81
          BGCOLOR 15 
     btn_procesar AT ROW 7.19 COL 90
     Btn_Cancelar AT ROW 8.85 COL 90
     BtnDone AT ROW 10.46 COL 90
     Btn_Consulta AT ROW 4.81 COL 90
     BUTTON-1 AT ROW 1.54 COL 90
     BUTTON-95 AT ROW 12.62 COL 93
     W_NomTitular AT ROW 1.81 COL 30 COLON-ALIGNED
     Btn_Aceptar AT ROW 3.23 COL 90 HELP
          "Permite seleccionar el dispositivo de salida del informe"
     RECT-274 AT ROW 1.27 COL 89
     RECT-309 AT ROW 1.27 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 100.43 BY 14.73
         BGCOLOR 17 FONT 5.

DEFINE FRAME F_ForPago
     Solicitud.Age_DebAutomatico AT ROW 1.81 COL 30 COLON-ALIGNED
          LABEL "Agencia"
          VIEW-AS FILL-IN 
          SIZE 4 BY .81
          BGCOLOR 18 FGCOLOR 15 
     W_NomAgeDebAutomatico AT ROW 1.81 COL 34 COLON-ALIGNED NO-LABEL
     Solicitud.Cod_DebAutomatico AT ROW 2.88 COL 30 COLON-ALIGNED
          LABEL "Producto"
          VIEW-AS FILL-IN 
          SIZE 4 BY .81
          BGCOLOR 18 FGCOLOR 15 
     W_NomCodDebAutomatico AT ROW 2.88 COL 34 COLON-ALIGNED NO-LABEL
     Cue_DebAutomatico AT ROW 3.96 COL 30 COLON-ALIGNED
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2 ROW 8.54
         SIZE 86 BY 5.65
         BGCOLOR 17 FONT 5
         TITLE "Forma de Pago de la Cuota".


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
         TITLE              = "Debito Automatico para creditos - W-Credito_DebAut.w"
         HEIGHT             = 13.27
         WIDTH              = 101
         MAX-HEIGHT         = 21.27
         MAX-WIDTH          = 114.29
         VIRTUAL-HEIGHT     = 21.27
         VIRTUAL-WIDTH      = 114.29
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
/* REPARENT FRAME */
ASSIGN FRAME F_ForPago:FRAME = FRAME Frm_Creditos:HANDLE.

/* SETTINGS FOR FRAME Frm_Creditos
   NOT-VISIBLE FRAME-NAME Custom                                        */
/* BROWSE-TAB Brw_Creditos 1 Frm_Creditos */
/* SETTINGS FOR BUTTON BtnDone IN FRAME Frm_Creditos
   6                                                                    */
/* SETTINGS FOR BUTTON Btn_Cancelar IN FRAME Frm_Creditos
   6                                                                    */
/* SETTINGS FOR BUTTON Btn_Consulta IN FRAME Frm_Creditos
   6                                                                    */
/* SETTINGS FOR BUTTON BUTTON-1 IN FRAME Frm_Creditos
   6                                                                    */
/* SETTINGS FOR BUTTON BUTTON-95 IN FRAME Frm_Creditos
   6                                                                    */
/* SETTINGS FOR FILL-IN W_NomTitular IN FRAME Frm_Creditos
   NO-ENABLE                                                            */
ASSIGN 
       W_NomTitular:READ-ONLY IN FRAME Frm_Creditos        = TRUE.

/* SETTINGS FOR FRAME F_ForPago
   NOT-VISIBLE                                                          */
/* SETTINGS FOR FILL-IN Solicitud.Age_DebAutomatico IN FRAME F_ForPago
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Solicitud.Cod_DebAutomatico IN FRAME F_ForPago
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Cue_DebAutomatico IN FRAME F_ForPago
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_NomAgeDebAutomatico IN FRAME F_ForPago
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_NomCodDebAutomatico IN FRAME F_ForPago
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Brw_Creditos
/* Query rebuild information for BROWSE Brw_Creditos
     _START_FREEFORM
 OPEN QUERY {&SELF-NAME} FOR EACH Creditos
            WHERE Creditos.Nit = "Clientes.nit"
            AND Creditos.estado EQ 2 NO-LOCK INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Where[1]         = "Clientes.Nit = ""ahorros.nit""
 AND ahorros.tip_ahorro = 3"
     _Query            is OPENED
*/  /* BROWSE Brw_Creditos */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME Frm_Creditos
/* Query rebuild information for FRAME Frm_Creditos
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH Ahorros SHARE-LOCK.
     _END_FREEFORM
     _Query            is OPENED
*/  /* FRAME Frm_Creditos */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_ForPago
/* Query rebuild information for FRAME F_ForPago
     _TblList          = "bdcentral.Solicitud"
     _Query            is NOT OPENED
*/  /* FRAME F_ForPago */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Debito Automatico para creditos - W-Credito_DebAut.w */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Debito Automatico para creditos - W-Credito_DebAut.w */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Frm_Creditos
&Scoped-define BROWSE-NAME Brw_Creditos
&Scoped-define SELF-NAME Brw_Creditos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Brw_Creditos C-Win
ON MOUSE-SELECT-DBLCLICK OF Brw_Creditos IN FRAME Frm_Creditos
DO:
     
  RUN C-Ahorros.w (INPUT Creditos.Nit:SCREEN-VALUE, OUTPUT W_Age, OUTPUT W_Pro, OUTPUT W_Nit, OUTPUT W_Cue).
     FIND Ahorros WHERE Ahorros.Agencia      EQ W_Age  AND
                        Ahorros.Cod_Ahorro   EQ W_Pro  AND
                        Ahorros.Nit          EQ W_Nit AND
                        Ahorros.Cue_Ahorros  EQ W_Cue NO-LOCK NO-ERROR.
     IF AVAILABLE Ahorros THEN DO:
        FIND Pro_Ahorros WHERE Pro_Ahorros.Cod_Ahorro EQ W_Pro NO-LOCK NO-ERROR.
        IF Pro_Ahorros.Tip_Ahorro    NE 1
        OR NOT Pro_Ahorros.Id_Debito   THEN DO:
           MESSAGE "La cuenta seleccionada no es de tipo" SKIP
                   "A la Vista, O no permite Deb/Automàtico. Escoja una cuenta de este" SKIP
                   "Tipo para hacer el débito automático!" VIEW-AS ALERT-BOX WARNING.
           SELF:SCREEN-VALUE = "1".
        END.
        ELSE DO:
           ASSIGN Cue_DebAutomatico:SCREEN-VALUE in FRAME F_ForPago = W_Cue .
           FIND Agencias WHERE Agencias.Agencia EQ W_Age NO-LOCK NO-ERROR.
           W_NomAgeDebAutomatico:SCREEN-VALUE = Agencias.Nombre.
           FIND Pro_Ahorros WHERE Pro_Ahorros.Cod_Ahorro EQ W_Pro NO-LOCK NO-ERROR.
           W_NomCodDebAutomatico:SCREEN-VALUE = Pro_Ahorros.Nom_producto.
           btn_procesar:SENSITIVE = TRUE.
        END.
     END.
     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Brw_Creditos C-Win
ON ROW-ENTRY OF Brw_Creditos IN FRAME Frm_Creditos
DO:
  
  RUN C-Ahorros.r (INPUT Creditos.Nit:SCREEN-VALUE, OUTPUT W_Age, OUTPUT W_Pro, OUTPUT W_Nit, OUTPUT W_Cue).
     FIND Ahorros WHERE Ahorros.Agencia      EQ W_Age  AND
                        Ahorros.Cod_Ahorro   EQ W_Pro  AND
                        Ahorros.Nit          EQ W_Nit AND
                        Ahorros.Cue_Ahorros  EQ W_Cue NO-LOCK NO-ERROR.
     IF AVAILABLE Ahorros THEN DO:
        FIND Pro_Ahorros WHERE Pro_Ahorros.Cod_Ahorro EQ W_Pro NO-LOCK NO-ERROR.
        IF Pro_Ahorros.Tip_Ahorro    NE 1
        OR NOT Pro_Ahorros.Id_Debito   THEN DO:
           MESSAGE "La cuenta seleccionada no es de tipo" SKIP
                   "A la Vista, O no permite Deb/Automàtico. Escoja una cuenta de este" SKIP
                   "Tipo para hacer el débito automático!" VIEW-AS ALERT-BOX WARNING.
           SELF:SCREEN-VALUE = "1".
        END.
        ELSE DO:
           ASSIGN Cue_DebAutomatico:SCREEN-VALUE in FRAME F_ForPago = W_Cue .
           FIND Agencias WHERE Agencias.Agencia EQ W_Age NO-LOCK NO-ERROR.
           W_NomAgeDebAutomatico:SCREEN-VALUE = Agencias.Nombre.
           FIND Pro_Ahorros WHERE Pro_Ahorros.Cod_Ahorro EQ W_Pro NO-LOCK NO-ERROR.
           W_NomCodDebAutomatico:SCREEN-VALUE = Pro_Ahorros.Nom_producto.
        END.
     END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Brw_Creditos C-Win
ON VALUE-CHANGED OF Brw_Creditos IN FRAME Frm_Creditos
DO :
/*    OPEN QUERY Brw_Ahorros FOR EACH Ahorros WHERE
          Ahorros.Nit EQ Clientes.nit:SCREEN-VALUE AND Ahorros.Estado EQ 1 AND tip_ahorro EQ 3 NO-LOCK.
*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnDone
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnDone C-Win
ON CHOOSE OF BtnDone IN FRAME Frm_Creditos /* Salir */
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


&Scoped-define SELF-NAME Btn_Aceptar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Aceptar C-Win
ON CHOOSE OF Btn_Aceptar IN FRAME Frm_Creditos /* Aceptar */
DO:
   DEFINE VAR Listado AS CHAR INITIAL "L_CfDebAut.Lst".
   
   Listado = W_Path + Listado.
  {Incluido\Imprimir.i "Listado"}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancelar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancelar C-Win
ON CHOOSE OF Btn_Cancelar IN FRAME Frm_Creditos /* Cancelar */
DO:
  RELEASE ahorros. 
  RELEASE Creditos. 
  RELEASE Clientes.
  RUN inicializar_variables.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Consulta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Consulta C-Win
ON CHOOSE OF Btn_Consulta IN FRAME Frm_Creditos /* Button 3 */
DO:
 
  RUN C-Ahorros.r (INPUT "", 
                   OUTPUT W_Age, OUTPUT W_Pro, OUTPUT W_Nit, OUTPUT W_Cue).
 
 FIND FIRST Ahorros WHERE Ahorros.Agencia EQ W_Age AND
                     Ahorros.Tip_Ahorro   EQ 3     AND
                     Ahorros.Estado       EQ 1     AND
                     Ahorros.Nit          EQ W_Nit NO-LOCK NO-ERROR.
  IF AVAILABLE Ahorros THEN DO: 
     puntero = ROWID(Ahorros).
 /* Llenar el browser*/
  END.
  ELSE
      MESSAGE "Cliente sin Ahorros a la Vista"
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_procesar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_procesar C-Win
ON CHOOSE OF btn_procesar IN FRAME Frm_Creditos /* Guardar */
DO:
 DO TRANSACTION ON ERROR UNDO:

     FIND CURRENT Creditos NO-ERROR.
     ASSIGN creditos.Age_DebAutomatico = w_age
            creditos.Cod_DebAutomatico = w_pro
            creditos.Cue_DebAutomatico = w_cue
            Creditos.FOR_pago          = 3.
 
    
    FIND CURRENT Creditos NO-LOCK NO-ERROR.
    MESSAGE "Credito Marcado Para Credito Deb-Automático!!!"
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
 END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-1 C-Win
ON CHOOSE OF BUTTON-1 IN FRAME Frm_Creditos /* Button 1 */
DO:
  RUN W-InfDia.r NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Creditos.Nit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Creditos.Nit C-Win
ON LEAVE OF Creditos.Nit IN FRAME Frm_Creditos /* Nit */
DO:

  DO WITH FRAME F_Creditos:
    IF Creditos.Nit:SCREEN-VALUE EQ "" THEN DO:
       APPLY "choose" TO BtnDone.
       RETURN NO-APPLY. 
    END.
     /* btn_procesar:SENSITIVE = FALSE. */
     FIND Clientes WHERE Clientes.Nit EQ Creditos.Nit:SCREEN-VALUE NO-LOCK NO-ERROR.

     IF AVAILABLE(Clientes) THEN
        W_NomTitular:SCREEN-VALUE = CAPS(Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2).
     ELSE DO:
        RUN C-Clientes.R(INPUT 2, INPUT W_Agencia, OUTPUT P_Nit,OUTPUT P_Nombre, OUTPUT P_Apellido, OUTPUT P_AgeCli).
        ASSIGN Creditos.Nit:SCREEN-VALUE = P_Nit
               W_Nomtitular:SCREEN-VALUE = CAPS(TRIM(P_Nombre) + " " + P_Apellido).
        FIND Clientes WHERE Clientes.Nit EQ P_Nit NO-LOCK NO-ERROR.
     END.
     
     FIND FIRST Creditos WHERE 
                Creditos.Nit           EQ SELF:SCREEN-VALUE AND
                Creditos.Estado        EQ 2 NO-LOCK NO-ERROR.
     IF NOT AVAILABLE Creditos THEN DO:
        MESSAGE "No existe Creditos Activos para este cliente " SKIP
                W_NomTitular:SCREEN-VALUE "no tiene ningùn Credito Activo" SKIP
                "Rectifique la cedula!!" VIEW-AS ALERT-BOX ERROR.
        APPLY "choose" TO Btn_Cancelar IN FRAME Frm_Creditos.
        RETURN NO-APPLY. 
      END.

      RELEASE Creditos.

   /*   ASSIGN W_Nuevo:SENSITIVE = TRUE.*/
      ASSIGN Creditos.nit:SENSITIVE = TRUE. 
      OPEN QUERY Brw_Creditos FOR EACH Creditos WHERE
                 Creditos.Nit EQ Clientes.nit AND Creditos.Estado EQ 2 NO-LOCK.
      APPLY "CHOOSE" TO Brw_Creditos.
     /*APPLY 'Value-Changed' TO Cmb_TipoProductos.*/
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

  {&OPEN-QUERY-Frm_Creditos}
  GET FIRST Frm_Creditos.
  DISPLAY W_NomTitular 
      WITH FRAME Frm_Creditos IN WINDOW C-Win.
  IF AVAILABLE Creditos THEN 
    DISPLAY Creditos.Nit 
      WITH FRAME Frm_Creditos IN WINDOW C-Win.
  ENABLE Brw_Creditos Creditos.Nit btn_procesar Btn_Cancelar BtnDone 
         Btn_Consulta BUTTON-1 BUTTON-95 Btn_Aceptar RECT-274 RECT-309 
      WITH FRAME Frm_Creditos IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-Frm_Creditos}
  DISPLAY W_NomAgeDebAutomatico W_NomCodDebAutomatico Cue_DebAutomatico 
      WITH FRAME F_ForPago IN WINDOW C-Win.
  IF AVAILABLE Solicitud THEN 
    DISPLAY Solicitud.Age_DebAutomatico Solicitud.Cod_DebAutomatico 
      WITH FRAME F_ForPago IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-F_ForPago}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE inicializar_variables C-Win 
PROCEDURE inicializar_variables :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  ASSIGN Creditos.Nit:SCREEN-VALUE IN FRAME FRM_Creditos   = "".
    btn_procesar:SENSITIVE = FALSE.
    
  OPEN QUERY Brw_Creditos FOR EACH Creditos WHERE
      Creditos.Nit EQ Clientes.nit AND Creditos.Estado EQ 2  NO-LOCK.
 

  APPLY "choose" TO Creditos.nit IN FRAME FRM_Creditos.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcesoImprimir C-Win 
PROCEDURE ProcesoImprimir :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DISPLAY "          Créditos con Matrícula de Cta-Ahorro para Deb/Automático"
      SKIP(1)
      WITH NO-LABELS.
  FOR EACH Creditos WHERE Creditos.Estado            EQ 2 AND
                          Creditos.Age_DebAutomatico GT 0 AND 
                          Creditos.Cod_DebAutomatico GT 0 AND 
                          Creditos.Cue_DebAutomatico GT " " NO-LOCK
                    BY Creditos.Nit:
      DISPLAY Creditos.Agencia       LABEL "Ag."
              Creditos.Nit           LABEL "Ced./Nit"
              Creditos.Cod_Credi     LABEL "Pdcto"
              Creditos.Num_Credito   LABEL "Num_Credito"
              Creditos.Sdo_Capital   LABEL "Sdo-Capital"
              Creditos.Cuota         LABEL "Vlr.Cuota"
              Creditos.Val_Atraso    LABEL "Vlr.Vencido Capital"
              Creditos.Age_DebAutomatico    LABEL "Ag.DB"
              Creditos.Cod_DebAutomatico    LABEL "PtoDb"
              Creditos.Cue_DebAutomatico    LABEL "Cta-a Debitar" SKIP(0)
          WITH DOWN WIDTH 150 FRAME F1 NO-BOX NO-LABELS USE-TEXT STREAM-IO.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

