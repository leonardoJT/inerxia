&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          bdcentral        PROGRESS
*/
&Scoped-define WINDOW-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS W-Win 
CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
   
   {Incluido/Variable.I "SHARED"}
   
   DEFINE VAR W_ConEfec AS   DECIMAL FORMAT "->>>>>,>>>,>>>,>>>,>>>,>>9.99".
   DEFINE VAR W_ConCheq AS   DECIMAL FORMAT "->>>>>,>>>,>>>,>>>,>>>,>>9.99".
   DEFINE VAR W_RetEfec AS   DECIMAL FORMAT "->>>>>,>>>,>>>,>>>,>>>,>>9.99".
   DEFINE VAR W_RetCheq AS   DECIMAL FORMAT "->>>>>,>>>,>>>,>>>,>>>,>>9.99".
   DEFINE VAR T_Usuario LIKE Usuarios.Usuario.
   
   DEFINE TEMP-TABLE Tempo
      FIELD T_Agencia LIKE Taquilla.Agencia
      FIELD T_Usuario LIKE Taquilla.Usuario
      FIELD T_Fecha   LIKE Taquilla.Fec_Transaccion
      FIELD T_TipPcto LIKE Taquilla.Tip_Producto
      FIELD T_CodPto  LIKE Taquilla.Cod_Producto
      FIELD T_Cuenta  LIKE Taquilla.Nro_Cuenta
      FIELD T_Nit     LIKE Terceros.Nit
      FIELD T_DocRef  LIKE Taquilla.Num_Documento
      FIELD T_UsuAut  LIKE Taquilla.Autorizo
      FIELD T_RetEfec LIKE W_RetEfec
      FIELD T_RetCheq LIKE W_RetCheq
      FIELD T_ConEfec LIKE W_ConEfec
      FIELD T_ConCheq LIKE W_ConCheq.

   DEFINE TEMP-TABLE IExcel
      FIELD INom_Ope         AS CHARACTER FORMAT "X(25)"       
      FIELD ITip_Producto    LIKE Taquilla.Tip_Producto  
      FIELD ICod_Producto    LIKE Taquilla.Cod_Producto  
      FIELD IIde             AS CHARACTER FORMAT "X(10)"
      FIELD INro_cuenta      LIKE Taquilla.Nro_cuenta    
      FIELD I_NIT            LIKE Taquilla.Nit           
      FIELD INum_Documento   LIKE Taquilla.Num_Documento 
      FIELD IAutorizo        LIKE Taquilla.Autorizo      
      FIELD IVal_Efectivo    LIKE Taquilla.Val_Efectivo  
      FIELD IVal_Cheque      LIKE Taquilla.Val_Cheque.

DEFINE VAR WInf AS INTEGER FORMAT 9.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES Cajero

/* Definitions for FRAME FCajero                                        */
&Scoped-define FIELDS-IN-QUERY-FCajero cajero.estado cajero.fecha ~
cajero.salini cajero.Sal_consignado cajero.Sal_retiro cajero.Sal_final 
&Scoped-define QUERY-STRING-FCajero FOR EACH Cajero SHARE-LOCK
&Scoped-define OPEN-QUERY-FCajero OPEN QUERY FCajero FOR EACH Cajero SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-FCajero Cajero
&Scoped-define FIRST-TABLE-IN-QUERY-FCajero Cajero


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS IMAGE-1 IMAGE-2 RECT-228 RECT-229 RECT-230 ~
RECT-231 BUTTON-59 BUTTON-65 Btn_Proceso Cmb_Usuario F_Fecha Btn_Imprimir ~
F_Base Btn_Done Btn_Ayuda 
&Scoped-Define DISPLAYED-OBJECTS Cmb_Usuario F_Fecha F_ConEfe F_RetEfe ~
F_ConChe F_RetChe F_TotRet F_TotCon F_Base F_Cuadre 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Ayuda 
     IMAGE-UP FILE "imagenes/interrogacion.bmp":U
     LABEL "Ayuda" 
     SIZE 4 BY 1.

DEFINE BUTTON Btn_Done DEFAULT 
     LABEL "&Salir" 
     SIZE 11 BY 1.62
     BGCOLOR 8 .

DEFINE BUTTON Btn_Imprimir 
     IMAGE-UP FILE "imagenes/impresora2.bmp":U
     LABEL "&Imprimir" 
     SIZE 11 BY 1.62.

DEFINE BUTTON Btn_Proceso 
     LABEL "Cuadre" 
     SIZE 11 BY 1.62.

DEFINE BUTTON BUTTON-59 
     IMAGE-UP FILE "imagenes/informacion.bmp":U
     LABEL "Button 59" 
     SIZE 11 BY 1.62.

DEFINE BUTTON BUTTON-65 
     LABEL "Información Base" 
     SIZE 23 BY 1.12.

DEFINE VARIABLE Cmb_Usuario AS CHARACTER FORMAT "X(50)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 59 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE F_Base AS DECIMAL FORMAT "->>>>>,>>>,>>>,>>>,>>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 26 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE F_ConChe AS DECIMAL FORMAT "->>>>>,>>>,>>>,>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Cheque" 
     VIEW-AS FILL-IN 
     SIZE 26 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE F_ConEfe AS DECIMAL FORMAT "->>>>>,>>>,>>>,>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Efectivo" 
     VIEW-AS FILL-IN 
     SIZE 26 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE F_Cuadre AS DECIMAL FORMAT "->>>>>,>>>,>>>,>>>,>>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 27 BY .81
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE F_Fecha AS DATE FORMAT "99/99/9999":U 
     VIEW-AS FILL-IN 
     SIZE 18 BY .81
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE F_RetChe AS DECIMAL FORMAT "->>>>>,>>>,>>>,>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Cheque" 
     VIEW-AS FILL-IN 
     SIZE 29 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE F_RetEfe AS DECIMAL FORMAT "->>>>>,>>>,>>>,>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Efectivo" 
     VIEW-AS FILL-IN 
     SIZE 29 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE F_TotCon AS DECIMAL FORMAT "->>>>>,>>>,>>>,>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Total" 
     VIEW-AS FILL-IN 
     SIZE 26 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE F_TotRet AS DECIMAL FORMAT "->>>>>,>>>,>>>,>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Total" 
     VIEW-AS FILL-IN 
     SIZE 29 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE IMAGE IMAGE-1
     FILENAME "imagenes/dinero.bmp":U
     SIZE 5 BY 1.35.

DEFINE IMAGE IMAGE-2
     FILENAME "imagenes/dinerorojo.bmp":U
     SIZE 6 BY 1.35.

DEFINE RECTANGLE RECT-228
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 89 BY 2.69
     BGCOLOR 18 FGCOLOR 15 .

DEFINE RECTANGLE RECT-229
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 42 BY 5.38.

DEFINE RECTANGLE RECT-230
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 45 BY 5.38.

DEFINE RECTANGLE RECT-231
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 89 BY 1.88
     BGCOLOR 1 .

DEFINE BUTTON BtnCerrarCaja 
     LABEL "Cerrar Caja" 
     SIZE 20 BY 1.12.

DEFINE BUTTON BUTTON-67 
     LABEL "Ocultar" 
     SIZE 20 BY 1.12.

DEFINE BUTTON BUTTON-68 
     IMAGE-UP FILE "imagenes/impresora2.bmp":U
     LABEL "Button 68" 
     SIZE 12 BY 2.42.

DEFINE RECTANGLE RECT-233
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 33 BY 1.35.

DEFINE BUTTON Btn_CanImp 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Cancelar" 
     SIZE 10 BY 1.88.

DEFINE BUTTON Btn_ImpFin 
     IMAGE-UP FILE "imagenes/impresora2.bmp":U
     LABEL "Imprimir" 
     SIZE 10 BY 1.81.

DEFINE VARIABLE W_F1 AS DATE FORMAT "99/99/99":U 
     LABEL "Fec-Inicial" 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .81 NO-UNDO.

DEFINE VARIABLE W_F2 AS DATE FORMAT "99/99/99":U 
     LABEL "Fec-Final" 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .81 NO-UNDO.

DEFINE VARIABLE Rad_SelImp AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Totales", 1,
"General", 2,
"Cheques Girados", 3,
"Cheques Consignados", 4,
"Cheques Cobrados por Ventanilla", 5,
"Resumido por Cuenta", 6,
"Resumido por Cuenta-Nit", 7
     SIZE 33.29 BY 6.46
     FONT 5 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY FCajero FOR 
      Cajero SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Frame_Imprimir
     W_F1 AT ROW 1.27 COL 42.72 COLON-ALIGNED
     Rad_SelImp AT ROW 1.31 COL 3.14 NO-LABEL
     W_F2 AT ROW 2.35 COL 42.86 COLON-ALIGNED
     Btn_ImpFin AT ROW 3.65 COL 44.43
     Btn_CanImp AT ROW 5.81 COL 44.43
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 45.29 ROW 6.27
         SIZE 55.29 BY 7.77
         BGCOLOR 17 FONT 4
         TITLE "Informe de Cajero".

DEFINE FRAME F-Main
     BUTTON-59 AT ROW 1.81 COL 101
     BUTTON-65 AT ROW 2.08 COL 8
     Btn_Proceso AT ROW 3.96 COL 101
     Cmb_Usuario AT ROW 4.88 COL 10 COLON-ALIGNED NO-LABEL
     F_Fecha AT ROW 4.92 COL 74 COLON-ALIGNED NO-LABEL
     Btn_Imprimir AT ROW 5.58 COL 101
     F_ConEfe AT ROW 9.08 COL 20 COLON-ALIGNED
     F_RetEfe AT ROW 9.08 COL 63 COLON-ALIGNED
     F_ConChe AT ROW 10.15 COL 20 COLON-ALIGNED
     F_RetChe AT ROW 10.15 COL 63 COLON-ALIGNED
     F_TotRet AT ROW 11.77 COL 63 COLON-ALIGNED
     F_TotCon AT ROW 11.92 COL 20 COLON-ALIGNED
     F_Base AT ROW 17.96 COL 19 COLON-ALIGNED NO-LABEL
     F_Cuadre AT ROW 17.96 COL 64 COLON-ALIGNED NO-LABEL
     Btn_Done AT ROW 18.5 COL 101
     Btn_Ayuda AT ROW 20.38 COL 104.72
     "Usuario del Sistema" VIEW-AS TEXT
          SIZE 19 BY .81 AT ROW 3.96 COL 12
          BGCOLOR 18 FGCOLOR 15 
     "Consignaciones" VIEW-AS TEXT
          SIZE 15 BY 1.35 AT ROW 6.92 COL 22
          BGCOLOR 17 FGCOLOR 7 
     "Base" VIEW-AS TEXT
          SIZE 6 BY .81 AT ROW 17.96 COL 15
          BGCOLOR 1 FGCOLOR 15 
     "Retiros" VIEW-AS TEXT
          SIZE 7 BY 1.35 AT ROW 6.92 COL 65
          BGCOLOR 17 FGCOLOR 7 
     "Fecha Proceso" VIEW-AS TEXT
          SIZE 14 BY .81 AT ROW 3.96 COL 76
          BGCOLOR 18 FGCOLOR 15 
     "Saldo Disponible" VIEW-AS TEXT
          SIZE 16 BY .81 AT ROW 17.96 COL 50
          BGCOLOR 1 FGCOLOR 15 
     IMAGE-1 AT ROW 6.92 COL 17
     IMAGE-2 AT ROW 6.92 COL 59
     RECT-228 AT ROW 3.69 COL 8
     RECT-229 AT ROW 8.27 COL 8
     RECT-230 AT ROW 8.27 COL 52
     RECT-231 AT ROW 17.42 COL 8
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 113.86 BY 21.19
         BGCOLOR 17 FONT 5
         DEFAULT-BUTTON Btn_Done.

DEFINE FRAME FCajero
     cajero.estado AT ROW 1.54 COL 7 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Caja Abierta", 1,
"Caja Cerrada", 2
          SIZE 28 BY .81
     cajero.fecha AT ROW 2.88 COL 19 COLON-ALIGNED
          LABEL "Fecha de la Base"
          VIEW-AS FILL-IN 
          SIZE 15 BY .81
          BGCOLOR 5 FGCOLOR 15 
     cajero.salini AT ROW 4.23 COL 19 COLON-ALIGNED
          LABEL "Base Inicial del Día"
          VIEW-AS FILL-IN 
          SIZE 15 BY .81
          BGCOLOR 18 FGCOLOR 15 
     cajero.Sal_consignado AT ROW 5.31 COL 19 COLON-ALIGNED
          LABEL "(+) Consignaciones" FORMAT "->>,>>>,>>>,>>9.99"
          VIEW-AS FILL-IN 
          SIZE 15 BY .81
     cajero.Sal_retiro AT ROW 6.38 COL 19 COLON-ALIGNED
          LABEL "(-) Retiros" FORMAT "->>,>>>,>>>,>>9.99"
          VIEW-AS FILL-IN 
          SIZE 15 BY .81
     cajero.Sal_final AT ROW 7.46 COL 19 COLON-ALIGNED
          LABEL "Saldo Final"
          VIEW-AS FILL-IN 
          SIZE 15 BY .81
          BGCOLOR 3 FGCOLOR 15 
     BUTTON-68 AT ROW 9.08 COL 3
     BtnCerrarCaja AT ROW 9.08 COL 16
     BUTTON-67 AT ROW 10.42 COL 16
     RECT-233 AT ROW 1.27 COL 4
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2.43 ROW 3.54
         SIZE 38 BY 11.85
         BGCOLOR 17 FONT 4
         TITLE "Información Base del Cajero".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW W-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Informe de Cuadre de Taquilla (Por Usuario)"
         HEIGHT             = 21.12
         WIDTH              = 114.29
         MAX-HEIGHT         = 21.19
         MAX-WIDTH          = 114.29
         VIRTUAL-HEIGHT     = 21.19
         VIRTUAL-WIDTH      = 114.29
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB W-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW W-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* REPARENT FRAME */
ASSIGN FRAME FCajero:FRAME = FRAME F-Main:HANDLE.

/* SETTINGS FOR FRAME F-Main
   FRAME-NAME                                                           */
/* SETTINGS FOR FILL-IN F_ConChe IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN F_ConEfe IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN F_Cuadre IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN F_RetChe IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN F_RetEfe IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN F_TotCon IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN F_TotRet IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME FCajero
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME FCajero:HIDDEN           = TRUE.

/* SETTINGS FOR RADIO-SET cajero.estado IN FRAME FCajero
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN cajero.fecha IN FRAME FCajero
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN cajero.salini IN FRAME FCajero
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN cajero.Sal_consignado IN FRAME FCajero
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */
/* SETTINGS FOR FILL-IN cajero.Sal_final IN FRAME FCajero
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN cajero.Sal_retiro IN FRAME FCajero
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */
/* SETTINGS FOR FRAME Frame_Imprimir
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME Frame_Imprimir:HIDDEN           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
THEN W-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FCajero
/* Query rebuild information for FRAME FCajero
     _TblList          = "bdcentral.Cajero"
     _Query            is OPENED
*/  /* FRAME FCajero */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON END-ERROR OF W-Win /* Informe de Cuadre de Taquilla (Por Usuario) */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* Informe de Cuadre de Taquilla (Por Usuario) */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FCajero
&Scoped-define SELF-NAME BtnCerrarCaja
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnCerrarCaja W-Win
ON CHOOSE OF BtnCerrarCaja IN FRAME FCajero /* Cerrar Caja */
DO:
  IF Cajero.Estado EQ 2 THEN DO:
     MESSAGE "Tabla cajero Ya aperece con Estado = 2, Revise por favor."
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
     RETURN.
  END.

  ASSIGN FRAME FCajero Cajero.Sal_consignado Cajero.Sal_final Cajero.Sal_retiro.
  Cajero.Estado = 2.
  DISABLE BtnCerrarCaja WITH FRAME FCajero.

  CREATE Cajero.
  ASSIGN Cajero.SalIni  = DEC(Cajero.Sal_Final:SCREEN-VALUE)
         Cajero.estado  = 1
         Cajero.fecha   = DATE(Cajero.fecha:SCREEN-VALUE) + 1
         Cajero.Usuario = SUBSTRING(Cmb_Usuario:SCREEN-VALUE IN FRAME F-Main,1,4).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F-Main
&Scoped-define SELF-NAME Btn_Ayuda
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Ayuda W-Win
ON CHOOSE OF Btn_Ayuda IN FRAME F-Main /* Ayuda */
OR HELP OF {&WINDOW-NAME}
DO:
   SYSTEM-HELP "AYUDAS\tesoreri" CONTEXT 25.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME Frame_Imprimir
&Scoped-define SELF-NAME Btn_CanImp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_CanImp W-Win
ON CHOOSE OF Btn_CanImp IN FRAME Frame_Imprimir /* Cancelar */
DO:
  HIDE FRAME Frame_Imprimir.
  FRAME F-Main:SENSITIVE = TRUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F-Main
&Scoped-define SELF-NAME Btn_Done
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Done W-Win
ON CHOOSE OF Btn_Done IN FRAME F-Main /* Salir */
DO:
  &IF DEFINED (adm-panel) <> 0 &THEN
      RUN dispatch IN THIS-PROCEDURE ('exit').
  &ELSE
      APPLY "CLOSE":U TO THIS-PROCEDURE.
  &ENDIF
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME Frame_Imprimir
&Scoped-define SELF-NAME Btn_ImpFin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_ImpFin W-Win
ON CHOOSE OF Btn_ImpFin IN FRAME Frame_Imprimir /* Imprimir */
DO:
  
  WInf = 1.
  RUN PImprimir.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F-Main
&Scoped-define SELF-NAME Btn_Imprimir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Imprimir W-Win
ON CHOOSE OF Btn_Imprimir IN FRAME F-Main /* Imprimir */
DO:
  VIEW FRAME Frame_Imprimir.
  FRAME F-Main:SENSITIVE = FALSE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Proceso
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Proceso W-Win
ON CHOOSE OF Btn_Proceso IN FRAME F-Main /* Cuadre */
DO:
  RUN CUADRE1.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-59
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-59 W-Win
ON CHOOSE OF BUTTON-59 IN FRAME F-Main /* Button 59 */
DO:
  RUN W-InfDia.r NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-65
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-65 W-Win
ON CHOOSE OF BUTTON-65 IN FRAME F-Main /* Información Base */
DO:
  DEFINE VAR Con LIKE Ahorros.Sdo_Disponible.
  DEFINE VAR Ret LIKE Ahorros.Sdo_Disponible.
  
  ASSIGN Con = 0
         Ret = 0.
  FIND Cajero WHERE Cajero.Usuario EQ SUBSTRING(Cmb_Usuario:SCREEN-VALUE,1,4) AND
       Cajero.Fecha EQ F_Fecha NO-ERROR.
  IF AVAILABLE Cajero THEN DO:
     DISPLAY Cajero.SalIni Cajero.Fecha Cajero.Sal_Consignado Cajero.Sal_Retiro Cajero.Sal_Final Cajero.Estado
             WITH FRAME FCajero.
     FOR EACH Mov_Contable WHERE 
         Mov_Contable.agencia = W_Agencia AND
         Mov_Contable.Fec_Contable EQ F_Fecha AND
         Mov_Contable.Usuario EQ Cajero.Usuario  AND
         (mov_contable.cuenta GE '11050501' AND mov_contable.cuenta LE "110510") AND 
         (Mov_Contable.Db GT 0 OR Mov_Contable.Cr GT 0)
         NO-LOCK
         BY Mov_Contable.agencia
         BY Mov_Contable.Fec_Contable 
         BY Mov_Contable.Usuario:

        ASSIGN Con = Con + Mov_Contable.Db
               Ret = Ret + Mov_Contable.Cr.
     END.
     ASSIGN Cajero.Sal_Final:SCREEN-VALUE = STRING(Cajero.SalIni + Con - Ret)
            Cajero.Sal_Consignado:SCREEN-VALUE = STRING(Con)
            Cajero.Sal_Retiro:SCREEN-VALUE = STRING(Ret).
     IF Cajero.Estado EQ 2 THEN DISABLE BtnCerrarCaja WITH FRAME FCajero.
     VIEW FRAME FCajero.
  END.
  ELSE DO:
     MESSAGE "No se ha entrado la base del día." SKIP
             "comuniquese con el administrador" VIEW-AS ALERT-BOX INFORMATION.
  END.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FCajero
&Scoped-define SELF-NAME BUTTON-67
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-67 W-Win
ON CHOOSE OF BUTTON-67 IN FRAME FCajero /* Ocultar */
DO:
  HIDE FRAME FCajero.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-68
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-68 W-Win
ON CHOOSE OF BUTTON-68 IN FRAME FCajero /* Button 68 */
DO:
  WInf = 2.
  RUN PImprimir.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F-Main
&Scoped-define SELF-NAME Cmb_Usuario
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_Usuario W-Win
ON VALUE-CHANGED OF Cmb_Usuario IN FRAME F-Main
DO:
  DO WITH FRAME {&FRAME-NAME}:
     ASSIGN T_Usuario = SUBSTRING(Cmb_Usuario:SCREEN-VALUE,1,4).
     RUN Saldo_Cero.
     APPLY "ENTRY" TO F_Base.
     RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME F_Fecha
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL F_Fecha W-Win
ON LEAVE OF F_Fecha IN FRAME F-Main
DO:
  ASSIGN F_Fecha.
  RUN Saldo_Cero.
  APPLY "ENTRY" TO F_Base.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME Frame_Imprimir
&Scoped-define SELF-NAME W_F1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_F1 W-Win
ON LEAVE OF W_F1 IN FRAME Frame_Imprimir /* Fec-Inicial */
DO:
  ASSIGN W_F1.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_F2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_F2 W-Win
ON LEAVE OF W_F2 IN FRAME Frame_Imprimir /* Fec-Final */
DO:
  ASSIGN W_F2.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F-Main
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK W-Win 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects W-Win  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available W-Win  _ADM-ROW-AVAILABLE
PROCEDURE adm-row-available :
/*------------------------------------------------------------------------------
  Purpose:     Dispatched to this procedure when the Record-
               Source has a new row available.  This procedure
               tries to get the new row (or foriegn keys) from
               the Record-Source and process it.
  Parameters:  <none>
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.             */
  {src/adm/template/row-head.i}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Che_Canje W-Win 
PROCEDURE Che_Canje :
/*------------------------------------------------------------------------------
  OBSERVACIONES : Imprimir el detalle.       
------------------------------------------------------------------------------*/
  
  DEFINE VAR Nom_Cajero AS   CHARACTER FORMAT "X(60)".
  DEFINE VAR Titulo     AS   CHARACTER FORMAT "X(60)".
  DEFINE VAR NomBco     LIKE Bancos.Nombre.
  DEFINE VAR Saldo      LIKE Taquilla.Val_Cheque.
  DEFINE VAR Cont       AS   INTEGER.
  
  DEFINE FRAME F-LineaEncabezado
    HEADER
      "Cod.Banco  Nombre Bco.                               Nro.Cheque                    Valor    " AT 2
      "____________________________________________________________________________________________" AT 2 SKIP(1)
  WITH DOWN WIDTH 180 USE-TEXT PAGE-TOP FRAME F-LineaEncabezado STREAM-IO NO-BOX.
  
  {Incluido\RepEncabezado.i}
  DO WITH FRAME {&FRAME-NAME}:
      ASSIGN Nom_Cajero = TRIM(SUBSTRING(Cmb_Usuario:SCREEN-VALUE,6,50))
             F_Fecha
             Saldo = 0 Cont = 0.

      ASSIGN F_ConChe F_ConEfe F_Cuadre F_RetChe F_RetEfe F_TotCon F_TotRet F_Base F_Cuadre F_Fecha
             Titulo = "Base : $ " + TRIM(STRING(F_Base,">>>,>>>,>>>,>>>,>>9.99")) + "   Fecha : " + STRING(F_Fecha)
             Nom_Cajero = TRIM(SUBSTRING(Cmb_Usuario:SCREEN-VALUE,6,50)).
      RUN Justificar IN W_Manija (INPUT-OUTPUT Titulo,INPUT " ",INPUT 60,INPUT "C").
      W_Reporte   =  "REPORTE   : LISTA DETALLADA DE CHEQUES EN CANJE - " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am").
      W_EncColumna = " CAJERO    : " + CAPS(STRING(Nom_Cajero,"X(40)")) + " - Base del Día: $" +  TRIM(STRING(F_Base,">>>,>>>,>>>,>>>,>>9.99")).
      VIEW FRAME F-Encabezado.
      VIEW FRAME F-LineaEncabezado.
      VIEW FRAME F-PiePagina. 
  END.     
  
  
  DO WITH FRAME {&FRAME-NAME}:
     ASSIGN Titulo = "LISTA DETALLADA DE LOS CHEQUES EN CONSIGNACION"
            Nom_Cajero = TRIM(SUBSTRING(Cmb_Usuario:SCREEN-VALUE,6,50))
            F_Fecha
            Saldo = 0 Cont = 0.
     RUN Justificar IN W_Manija (INPUT-OUTPUT Titulo,INPUT " ",INPUT 60,INPUT "C").
     RUN Justificar IN W_Manija (INPUT-OUTPUT Nom_Cajero,INPUT " ",INPUT 60,INPUT "C").
     FOR EACH Taquilla WHERE Taquilla.Agencia         EQ W_Agencia
                       AND   Taquilla.Usuario         EQ T_Usuario
                       AND   Taquilla.Fec_Transaccion EQ F_Fecha
                       AND   Taquilla.Estado          EQ 1
                       /*AND   Taquilla.Cod_Operacion   NE 010102005 */
                       AND   Taquilla.Naturaleza      EQ "CR" 
                       AND   Taquilla.Val_Cheque      GT 0 NO-LOCK:
         ASSIGN Saldo = Saldo + Taquilla.Val_Cheque
                Cont  = Cont  + 1.
         FIND Bancos WHERE Bancos.Cod_Compensa EQ Taquilla.Cod_Compensa NO-LOCK NO-ERROR.
         IF AVAILABLE(Bancos) THEN
            ASSIGN NomBco = Bancos.Nombre.
         ELSE
            ASSIGN NomBco = "".
         DISPLAY Taquilla.Cod_Compensa   AT 5   NO-LABEL
                 NomBco                  AT 13  NO-LABEL
                 Taquilla.Num_Retcheque  AT 55  NO-LABEL
                 Taquilla.Val_Cheque     AT 72  NO-LABEL
         WITH FRAME F-CAJA DOWN WIDTH 180 USE-TEXT STREAM-IO NO-BOX.
         DOWN WITH FRAME F-CAJA.
     END.
     DISPLAY "___________________"  AT 74
             "TOTAL CHEQUES :"      AT 55
             Saldo                  AT 72 NO-LABEL
             "NUM.CHEQUES   :"      AT 55
             Cont                   AT 72 NO-LABEL
     WITH FRAME F-TOTAL1 DOWN WIDTH 180 USE-TEXT STREAM-IO NO-BOX.
     DOWN WITH FRAME F-TOTAL1.
     
  END.
  PAGE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Che_Girados W-Win 
PROCEDURE Che_Girados :
/*------------------------------------------------------------------------------
  OBSERVACIONES : Imprimir el detalle.       
------------------------------------------------------------------------------*/
  
  DEFINE VAR Nom_Cajero AS   CHARACTER FORMAT "X(60)".
  DEFINE VAR Titulo     AS   CHARACTER FORMAT "X(60)".
  DEFINE VAR NomBco     LIKE Bancos.Nombre.
  DEFINE VAR Saldo      LIKE Taquilla.Val_Cheque.
  DEFINE VAR Cont       AS   INTEGER.

  DEFINE FRAME F-LineaEncabezado
    HEADER
      "Cod.Banco  Nombre Bco.               Nro.Cheque  Tip.Pto Cuenta                Valor        " AT 2
      "____________________________________________________________________________________________" AT 2 SKIP(1)
  WITH DOWN WIDTH 180 USE-TEXT PAGE-TOP FRAME F-LineaEncabezado STREAM-IO NO-BOX.

  {Incluido\RepEncabezado.i}
  DO WITH FRAME {&FRAME-NAME}:
      ASSIGN Nom_Cajero = TRIM(SUBSTRING(Cmb_Usuario:SCREEN-VALUE,6,50))
             F_Fecha
             Saldo = 0 Cont = 0.

      ASSIGN F_ConChe F_ConEfe F_Cuadre F_RetChe F_RetEfe F_TotCon F_TotRet F_Base F_Cuadre F_Fecha
             Titulo = "Base : $ " + TRIM(STRING(F_Base,">>>,>>>,>>>,>>>,>>9.99")) + "   Fecha : " + STRING(F_Fecha)
             Nom_Cajero = TRIM(SUBSTRING(Cmb_Usuario:SCREEN-VALUE,6,50)).
      RUN Justificar IN W_Manija (INPUT-OUTPUT Titulo,INPUT " ",INPUT 60,INPUT "C").
      W_Reporte   =  "REPORTE   : LISTA DETALLADA DE CHEQUES GIRADOS - " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am").
      W_EncColumna = " CAJERO    : " + CAPS(STRING(Nom_Cajero,"X(40)")) + " - Base del Día: $" +  TRIM(STRING(F_Base,">>>,>>>,>>>,>>>,>>9.99")).
      VIEW FRAME F-Encabezado.
      VIEW FRAME F-LineaEncabezado.
      VIEW FRAME F-PiePagina. 
  END.     
  
 
  DO WITH FRAME {&FRAME-NAME}:
     FOR EACH Taquilla WHERE Taquilla.Agencia         EQ W_Agencia
                       AND   Taquilla.Usuario         EQ T_Usuario
                       AND   Taquilla.Fec_Transaccion EQ F_Fecha
                       AND   Taquilla.Estado          EQ 1
                       /*AND   Taquilla.Cod_Operacion   NE 010102005 */
                       AND   Taquilla.Naturaleza      EQ "DB" 
                       AND   Taquilla.Val_Cheque      GT 0 NO-LOCK:
         ASSIGN Saldo = Saldo + Taquilla.Val_Cheque
                Cont  = Cont  + 1.
         FIND Bancos WHERE Bancos.Cod_Compensa EQ Taquilla.Cod_Compensa NO-LOCK NO-ERROR.
         IF AVAILABLE(Bancos) THEN
            ASSIGN NomBco = Bancos.Nombre.
         ELSE
            ASSIGN NomBco = "".
         DISPLAY Taquilla.Cod_Compensa   AT 5   NO-LABEL
                 NomBco                  AT 10  NO-LABEL FORMAT "X(30)"
                 Taquilla.Num_Retcheque  AT 41  NO-LABEL
                 Taquilla.Tip_Producto   AT 53  NO-LABEL
                 Taquilla.Cuenta         AT 58  NO-LABEL
                 Taquilla.Val_Cheque     AT 72  NO-LABEL
         WITH FRAME F-CAJA DOWN WIDTH 180 USE-TEXT STREAM-IO NO-BOX.
         DOWN WITH FRAME F-CAJA.
     END.
     DISPLAY "___________________"  AT 74
             "TOTAL CHEQUES :"      AT 55
             Saldo                  AT 72 NO-LABEL
             "NUM.CHEQUES   :"      AT 55
             Cont                   AT 72 NO-LABEL
     WITH FRAME F-TOTAL1 DOWN WIDTH 180 USE-TEXT STREAM-IO NO-BOX.
     DOWN WITH FRAME F-TOTAL1.
     
  END.
  PAGE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Che_Ventanilla W-Win 
PROCEDURE Che_Ventanilla :
/*------------------------------------------------------------------------------
  OBSERVACIONES : Imprimir el detalle.       
------------------------------------------------------------------------------*/
  
  DEFINE VAR Nom_Cajero AS   CHARACTER FORMAT "X(60)".
  DEFINE VAR Titulo     AS   CHARACTER FORMAT "X(60)".
  DEFINE VAR NomDno     AS   CHARACTER FORMAT "X(50)".
  DEFINE VAR Saldo      LIKE Taquilla.Val_Cheque.
  DEFINE VAR Valor      LIKE Taquilla.Val_Cheque.
  DEFINE VAR Cont       AS   INTEGER.
  
  DEFINE FRAME F-LineaEncabezado
    HEADER
      "Agencia Cuenta         Nombre Titular de la Cuenta      Num.Dto               Valor        " AT 1
      "___________________________________________________________________________________________" AT 1 SKIP(1)
  WITH DOWN WIDTH 180 USE-TEXT PAGE-TOP FRAME F-LineaEncabezado STREAM-IO NO-BOX.

  {Incluido\RepEncabezado.i}

  DO WITH FRAME {&FRAME-NAME}:
      ASSIGN Nom_Cajero = TRIM(SUBSTRING(Cmb_Usuario:SCREEN-VALUE,6,50))
             F_Fecha
             Saldo = 0 Cont = 0.

      ASSIGN F_ConChe F_ConEfe F_Cuadre F_RetChe F_RetEfe F_TotCon F_TotRet F_Base F_Cuadre F_Fecha
             Titulo = "Base : $ " + TRIM(STRING(F_Base,">>>,>>>,>>>,>>>,>>9.99")) + "   Fecha : " + STRING(F_Fecha)
             Nom_Cajero = TRIM(SUBSTRING(Cmb_Usuario:SCREEN-VALUE,6,50)).
      RUN Justificar IN W_Manija (INPUT-OUTPUT Titulo,INPUT " ",INPUT 60,INPUT "C").
      W_Reporte   =  "REPORTE   : LISTA DETALLADA DE CHEQUES COBRADOS POR VENTANILLA - " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am").
      W_EncColumna = " CAJERO    : " + CAPS(STRING(Nom_Cajero,"X(40)")) + " - Base del Día: $" +  TRIM(STRING(F_Base,">>>,>>>,>>>,>>>,>>9.99")).
      VIEW FRAME F-Encabezado.
      VIEW FRAME F-LineaEncabezado.
      VIEW FRAME F-PiePagina. 
  END.     
  
  
  DO WITH FRAME {&FRAME-NAME}:
     ASSIGN Titulo = "LISTA DETALLADA DE LOS CHEQUES PAGADOS POR VENTANILLA"
            Nom_Cajero = TRIM(SUBSTRING(Cmb_Usuario:SCREEN-VALUE,6,50))
            F_Fecha
            Saldo = 0 Cont = 0.
     RUN Justificar IN W_Manija (INPUT-OUTPUT Titulo,INPUT " ",INPUT 60,INPUT "C").
     RUN Justificar IN W_Manija (INPUT-OUTPUT Nom_Cajero,INPUT " ",INPUT 60,INPUT "C").
     FOR EACH Taquilla WHERE Taquilla.Agencia                      EQ W_Agencia
                       AND   Taquilla.Usuario                      EQ T_Usuario
                       AND   Taquilla.Estado                       EQ 1
                       AND   Taquilla.Fec_Transaccion              EQ F_Fecha 
                       AND   SUBSTRING(Taquilla.Num_Documento,1,1) EQ "C" NO-LOCK
                       BREAK BY Taquilla.Num_Documento:
         ASSIGN Valor = Valor + Taquilla.Val_Efectivo.
         IF LAST-OF(Taquilla.Num_Documento) THEN DO:
            ASSIGN Saldo = Saldo + Valor.
                   Cont  = Cont  + 1.
            FIND Terceros WHERE Terceros.Nit EQ Taquilla.Nit NO-LOCK NO-ERROR.
            IF AVAILABLE(Terceros) THEN DO:
               ASSIGN NomDno = TRIM(Terceros.Nombre) + " " + TRIM(REPLACE(Terceros.Apellido,'_', ' ')).
            END.
            ELSE DO:
               ASSIGN NomDno = "".
            END.
            DISPLAY Taquilla.Agencia                              AT 3  NO-LABEL
                    Taquilla.Nro_cuenta                           AT 8  NO-LABEL
                    NomDno                                        AT 23 NO-LABEL FORMAT "X(35)"
                    TRIM(REPLACE(Taquilla.Num_Documento,'C', '')) AT 58 NO-LABEL
                    Valor                                         AT 70 NO-LABEL
            WITH FRAME F-CAJA DOWN WIDTH 180 USE-TEXT STREAM-IO NO-BOX.
            DOWN WITH FRAME F-CAJA.
            ASSIGN Valor = 0.
         END.
     END.
     DISPLAY "_____________________"  AT 70
             "TOTAL CHEQUES :"        AT 23
             Saldo                    AT 70 NO-LABEL
             "NUM.CHEQUES   :"        AT 23
             Cont                     AT 74 NO-LABEL
     WITH FRAME F-TOTAL1 DOWN WIDTH 180 USE-TEXT STREAM-IO NO-BOX.
     DOWN WITH FRAME F-TOTAL1.
  END.
  PAGE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CierreCaja W-Win 
PROCEDURE CierreCaja :
DEFINE VAR Nom_Cajero AS CHARACTER FORMAT "X(40)".
  DEFINE VAR Titulo     AS CHARACTER FORMAT "X(60)".
  DEFINE VAR W_Linea    AS CHARACTER FORMAT "X(88)".
  DEFINE VAR W_Linea2   AS CHARACTER FORMAT "X(88)".
  DEFINE VAR WNomUsu    AS CHARACTER FORMAT "X(50)".
  WNomUsu = Cmb_Usuario:SCREEN-VALUE IN FRAME f-main.
  DEFINE FRAME F-Encabezado
    HEADER
      W_Nom_Entidad                           AT 20 FORMAT "X(60)"
      "Pagina: "                              AT 76 PAGE-NUMBER FORMAT ">>>9"
      "INFORME DE CIERRE DE CAJA"             AT 33 SKIP
      W_Linea          AT  1
  WITH DOWN WIDTH 132 USE-TEXT PAGE-TOP FRAME F-Encabezado STREAM-IO NO-BOX.
  
  DEFINE FRAME F-PiePagina
    HEADER 
      "Fecha:"         AT   1
      TODAY            AT  10 FORMAT "99/99/9999"
      W_Nom_Agencia    AT  37 FORMAT "X(30)"
      "Hora: "         AT  70 STRING(TIME,"HH:MM AM")
  WITH DOWN WIDTH 132 NO-BOX FRAME F-PiePagina PAGE-BOTTOM USE-TEXT STREAM-IO. 
      
  ASSIGN W_Linea  = FILL("-",88)
         W_Linea2 = FILL("_",88).

  VIEW FRAME F-Encabezado.
  VIEW FRAME F-PiePagina.
  DISPLAY "Cajero                 :    " WNomUsu SKIP(2)

          "Base Inicio de Dia     :    " DEC(Cajero.SalIni:SCREEN-VALUE IN FRAME FCajero) FORM "->>>>>,>>>,>>9.99" SKIP
          "Consignaciones         :    " DEC(Cajero.Sal_Consignado:SCREEN-VALUE) FORM "->>>>>,>>>,>>9.99" SKIP
          "Retiros                :    " DEC(Cajero.Sal_Retiro:SCREEN-VALUE)     FORM "->>>>>,>>>,>>9.99" SKIP
          "Saldo Final Cajero     :    " DEC(Cajero.Sal_Final:SCREEN-VALUE)      FORM "->>>>>,>>>,>>9.99"
      WITH FRAME Fmovcaj WIDTH 133 NO-LABELS USE-TEXT NO-BOX STREAM-IO.

  PAGE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Cuadre W-Win 
PROCEDURE Cuadre :
/*-----------------------------------------------------------------------------------
  OBSERVACIONES : Cuadrar la Caja.       
-------------------------------------------------------------------------------------*/
  DEFINE VAR T_Consigna AS   DECIMAL FORMAT "->>>>>,>>>,>>>,>>>,>>>,>>9.99" INITIAL 0.
  DEFINE VAR T_Retira   AS   DECIMAL FORMAT "->>>>>,>>>,>>>,>>>,>>>,>>9.99" INITIAL 0.
  
  DO WITH FRAME {&FRAME-NAME}:
     ASSIGN W_RetEfec = 0 
            W_RetCheq = 0 
            W_ConEfec = 0 
            W_ConCheq = 0
            F_Fecha F_Base.
  
     IF T_Usuario = "0000" THEN DO:
       RUN Cuadre_conso.
       RETURN.
     END.
      
     FOR EACH Taquilla WHERE Taquilla.Agencia         EQ W_Agencia
                       AND   Taquilla.Usuario         EQ T_Usuario
                       AND   Taquilla.Fec_Transaccion EQ F_Fecha
                       AND   Taquilla.Estado          EQ 1
                       AND   Taquilla.Cod_Operacion   NE 010102005 NO-LOCK:
         IF Taquilla.Naturaleza EQ "DB" THEN DO:
            IF Taquilla.Val_Efectivo GT 0 THEN DO:
               ASSIGN W_RetEfec = W_RetEfec + Taquilla.Val_Efectivo.
            END.
            ELSE DO:
               ASSIGN W_RetCheq = W_RetCheq + Taquilla.Val_Cheque.
            END.
         END.
         ELSE DO:
            IF Taquilla.Val_Efectivo GT 0 THEN DO:
               ASSIGN W_ConEfec = W_ConEfec + Taquilla.Val_Efectivo.
            END.
            ELSE DO:
               ASSIGN W_ConCheq = W_ConCheq + Taquilla.Val_Cheque.
            END.
         END.
     END.
     ASSIGN T_Consigna = W_ConCheq + W_ConEfec
            T_Retira   = W_RetCheq + W_RetEfec
            F_ConChe:SCREEN-VALUE = STRING(W_ConCheq)
            F_ConEfe:SCREEN-VALUE = STRING(W_ConEfec)
            F_RetChe:SCREEN-VALUE = STRING(W_RetCheq)
            F_RetEfe:SCREEN-VALUE = STRING(W_RetEfec)
            F_TotCon:SCREEN-VALUE = STRING(T_Consigna)
            F_TotRet:SCREEN-VALUE = STRING(T_Retira)
            F_Cuadre:SCREEN-VALUE = STRING((T_Consigna + F_Base) - (T_Retira)).
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Cuadre1 W-Win 
PROCEDURE Cuadre1 :
/*-----------------------------------------------------------------------------------
  OBSERVACIONES : Cuadrar la Caja.       
-------------------------------------------------------------------------------------*/
  DEFINE VAR T_Consigna AS   DECIMAL FORMAT "->>>>>,>>>,>>>,>>>,>>>,>>9.99" INITIAL 0.
  DEFINE VAR T_Retira   AS   DECIMAL FORMAT "->>>>>,>>>,>>>,>>>,>>>,>>9.99" INITIAL 0.
  
  DO WITH FRAME {&FRAME-NAME}:
     ASSIGN W_RetEfec = 0 
            W_RetCheq = 0 
            W_ConEfec = 0 
            W_ConCheq = 0
            F_Fecha F_Base.
  
     IF T_Usuario = "0000" THEN DO:
       RUN Cuadre_conso1.
       RETURN.
     END.
      
     FOR EACH mov_contable WHERE mov_contable.agencia      EQ  w_agencia AND
                                (mov_contable.cuenta GE '11050501' AND mov_contable.cuenta LE "11050502") AND 
                                 mov_contable.fec_contable EQ  F_Fecha  AND 
                                 mov_contable.usuario = T_Usuario NO-LOCK:
         CASE mov_contable.cuenta:
             WHEN "11050501" THEN DO:
                 IF mov_contable.db GT 0 THEN /* Consignación Efectivo */
                    ASSIGN W_ConEfec = W_ConEfec + mov_contable.db.
                 ELSE                         /* Retiro en Efectivo   */
                    ASSIGN W_RetEfec = W_RetEfec + mov_contable.cr.
             END.
             WHEN "11050502" THEN DO:
                 IF mov_contable.db GT 0 THEN /* Consignación Cheque */
                    ASSIGN W_ConCheq = W_ConCheq + mov_contable.db.
                 ELSE                         /* Retiro en cheque   */
                    ASSIGN W_RetCheq = W_RetCheq + mov_contable.cr.
             END.
         END.
     END.
     ASSIGN T_Consigna = W_ConCheq + W_ConEfec
            T_Retira   = W_RetCheq + W_RetEfec
            F_ConChe:SCREEN-VALUE = STRING(W_ConCheq)
            F_ConEfe:SCREEN-VALUE = STRING(W_ConEfec)
            F_RetChe:SCREEN-VALUE = STRING(W_RetCheq)
            F_RetEfe:SCREEN-VALUE = STRING(W_RetEfec)
            F_TotCon:SCREEN-VALUE = STRING(T_Consigna)
            F_TotRet:SCREEN-VALUE = STRING(T_Retira)
            F_Cuadre:SCREEN-VALUE = STRING((T_Consigna + F_Base) - (T_Retira)).
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Cuadre_Conso W-Win 
PROCEDURE Cuadre_Conso :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VAR T_Consigna AS   DECIMAL FORMAT "->>>>>,>>>,>>>,>>>,>>>,>>9.99" INITIAL 0.
  DEFINE VAR T_Retira   AS   DECIMAL FORMAT "->>>>>,>>>,>>>,>>>,>>>,>>9.99" INITIAL 0.
  DEFINE VAR T_ConCheq  AS   DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>9.99" INITIAL 0.
  DEFINE VAR T_ConEfec  AS   DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>9.99" INITIAL 0.
  DEFINE VAR T_RetCheq  AS   DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>9.99" INITIAL 0.
  DEFINE VAR T_RetEfec  AS   DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>9.99" INITIAL 0.

  
  DO WITH FRAME {&FRAME-NAME}:
     ASSIGN W_RetEfec = 0 
            W_RetCheq = 0 
            W_ConEfec = 0 
            W_ConCheq = 0
            F_Fecha F_Base.
        
     FOR EACH Taquilla WHERE Taquilla.Agencia         EQ W_Agencia
                       AND   Taquilla.Fec_Transaccion EQ F_Fecha
                       AND   Taquilla.Estado          EQ 1
                       /*AND   Taquilla.Cod_Operacion   NE 010102005*/ NO-LOCK
                    BREAK BY Taquilla.Usuario:
         IF Taquilla.Naturaleza EQ "DB" THEN DO:
            IF Taquilla.Val_Efectivo GT 0 THEN DO:
               ASSIGN W_RetEfec = W_RetEfec + Taquilla.Val_Efectivo.
            END.
            ELSE DO:
               ASSIGN W_RetCheq = W_RetCheq + Taquilla.Val_Cheque.
            END.
         END.
         ELSE DO:
            IF Taquilla.Val_Efectivo GT 0 THEN DO:
               ASSIGN W_ConEfec = W_ConEfec + Taquilla.Val_Efectivo.
            END.
            ELSE DO:
               ASSIGN W_ConCheq = W_ConCheq + Taquilla.Val_Cheque.
            END.
         END.
         IF LAST-OF(Taquilla.Usuario) THEN DO:
           Create Tempo.
           ASSIGN Tempo.T_Agencia = Taquilla.Agencia
                  Tempo.T_Usuario = Taquilla.Usuario
                  Tempo.T_Fecha   = Taquilla.Fec_Transaccion
                  Tempo.T_RetEfec = W_RetEfec
                  Tempo.T_RetCheq = W_RetCheq
                  Tempo.T_ConEfec = W_ConEfec
                  Tempo.T_ConCheq  = W_ConCheq
                  T_Consigna = T_Consigna + W_ConCheq + W_ConEfec
                  T_Retira   = T_Retira + W_RetCheq + W_RetEfec
                  T_ConCheq  = T_conCheq + W_conCheq
                  T_ConEfec  = T_ConEfec + W_ConEfec
                  T_RetCheq  = T_RetCheq + W_RetCheq
                  T_RetEfec  = T_RetEfec + W_RetEfec
                  W_ConEfec = 0 W_RetEfec = 0 W_conCheq = 0
                  W_ConEfec = 0 W_RetCheq = 0 W_RetEfec = 0.
         END.
     END.
     ASSIGN F_ConChe:SCREEN-VALUE = STRING(t_ConCheq)
            F_ConEfe:SCREEN-VALUE = STRING(t_ConEfec)
            F_RetChe:SCREEN-VALUE = STRING(t_RetCheq)
            F_RetEfe:SCREEN-VALUE = STRING(t_RetEfec)
            F_TotCon:SCREEN-VALUE = STRING(T_Consigna)
            F_TotRet:SCREEN-VALUE = STRING(T_Retira)
            F_Cuadre:SCREEN-VALUE = STRING((T_Consigna + F_Base) - (T_Retira)).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Cuadre_conso1 W-Win 
PROCEDURE Cuadre_conso1 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VAR T_Consigna AS   DECIMAL FORMAT "->>>>>,>>>,>>>,>>>,>>>,>>9.99" INITIAL 0.
  DEFINE VAR T_Retira   AS   DECIMAL FORMAT "->>>>>,>>>,>>>,>>>,>>>,>>9.99" INITIAL 0.
  DEFINE VAR T_ConCheq  AS   DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>9.99" INITIAL 0.
  DEFINE VAR T_ConEfec  AS   DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>9.99" INITIAL 0.
  DEFINE VAR T_RetCheq  AS   DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>9.99" INITIAL 0.
  DEFINE VAR T_RetEfec  AS   DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>9.99" INITIAL 0.

  
  DO WITH FRAME {&FRAME-NAME}:
     ASSIGN W_RetEfec = 0 
            W_RetCheq = 0 
            W_ConEfec = 0 
            W_ConCheq = 0
            F_Fecha F_Base.
     FOR EACH mov_contable WHERE mov_contable.agencia      EQ  w_agencia AND
                                (mov_contable.cuenta GE '11050501' AND mov_contable.cuenta LE "11050502") AND
                                 mov_contable.fec_contable EQ  F_Fecha NO-LOCK BREAK BY mov_contable.usuario:
        CASE mov_contable.cuenta:
            WHEN "11050501" THEN DO:
                IF mov_contable.db GT 0 THEN /* Consignación Efectivo */
                   ASSIGN W_ConEfec = W_ConEfec + mov_contable.db.
                ELSE                         /* Retiro en Efectivo   */
                   ASSIGN W_RetEfec = W_RetEfec + mov_contable.cr.
            END.
            WHEN "11050502" THEN DO:
                IF mov_contable.db GT 0 THEN /* Consignación Cheque */
                   ASSIGN W_ConCheq = W_ConCheq + mov_contable.db.
                ELSE                         /* Retiro en cheque   */
                   ASSIGN W_RetCheq = W_RetCheq + mov_contable.cr.
            END.
        END.
        IF LAST-OF(Mov_contable.Usuario) THEN DO:
          Create Tempo.
          ASSIGN Tempo.T_Agencia = Mov_contable.Agencia
                 Tempo.T_Usuario = Mov_contable.Usuario
                 Tempo.T_Fecha   = Mov_contable.Fec_contable
                 Tempo.T_RetEfec = W_RetEfec
                 Tempo.T_RetCheq = W_RetCheq
                 Tempo.T_ConEfec = W_ConEfec
                 Tempo.T_ConCheq  = W_ConCheq
                 T_Consigna = T_Consigna + W_ConCheq + W_ConEfec
                 T_Retira   = T_Retira + W_RetCheq + W_RetEfec
                 T_ConCheq  = T_conCheq + W_conCheq
                 T_ConEfec  = T_ConEfec + W_ConEfec
                 T_RetCheq  = T_RetCheq + W_RetCheq
                 T_RetEfec  = T_RetEfec + W_RetEfec
                 W_ConEfec = 0 W_RetEfec = 0 W_conCheq = 0
                 W_ConEfec = 0 W_RetCheq = 0 W_RetEfec = 0.
        END.
    END.
    ASSIGN F_ConChe:SCREEN-VALUE = STRING(t_ConCheq)
           F_ConEfe:SCREEN-VALUE = STRING(t_ConEfec)
           F_RetChe:SCREEN-VALUE = STRING(t_RetCheq)
           F_RetEfe:SCREEN-VALUE = STRING(t_RetEfec)
           F_TotCon:SCREEN-VALUE = STRING(T_Consigna)
           F_TotRet:SCREEN-VALUE = STRING(T_Retira)
           F_Cuadre:SCREEN-VALUE = STRING((T_Consigna + F_Base) - (T_Retira)).
 END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI W-Win  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
  THEN DELETE WIDGET W-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI W-Win  _DEFAULT-ENABLE
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
  DISPLAY Cmb_Usuario F_Fecha F_ConEfe F_RetEfe F_ConChe F_RetChe F_TotRet 
          F_TotCon F_Base F_Cuadre 
      WITH FRAME F-Main IN WINDOW W-Win.
  ENABLE IMAGE-1 IMAGE-2 RECT-228 RECT-229 RECT-230 RECT-231 BUTTON-59 
         BUTTON-65 Btn_Proceso Cmb_Usuario F_Fecha Btn_Imprimir F_Base Btn_Done 
         Btn_Ayuda 
      WITH FRAME F-Main IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}

  {&OPEN-QUERY-FCajero}
  GET FIRST FCajero.
  IF AVAILABLE cajero THEN 
    DISPLAY cajero.estado cajero.fecha cajero.salini cajero.Sal_consignado 
          cajero.Sal_retiro cajero.Sal_final 
      WITH FRAME FCajero IN WINDOW W-Win.
  ENABLE RECT-233 BUTTON-68 BtnCerrarCaja BUTTON-67 
      WITH FRAME FCajero IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-FCajero}
  DISPLAY W_F1 Rad_SelImp W_F2 
      WITH FRAME Frame_Imprimir IN WINDOW W-Win.
  ENABLE W_F1 Rad_SelImp W_F2 Btn_ImpFin Btn_CanImp 
      WITH FRAME Frame_Imprimir IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-Frame_Imprimir}
  VIEW W-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE General W-Win 
PROCEDURE General :
/*------------------------------------------------------------------------------
  OBSERVACIONES : Imprimir el detalle.       
------------------------------------------------------------------------------*/
  IF T_Usuario = "0000" THEN DO:
    RUN General_Conso.
    RETURN.
  END.
  
  DEFINE VAR Nom_Cajero AS   CHARACTER FORMAT "X(60)".
  DEFINE VAR Titulo     AS   CHARACTER FORMAT "X(60)".
  DEFINE VAR Nom_Ope    LIKE Operacion.Nom_Operacion.
  DEFINE VAR T_NatEfec  LIKE Taquilla.Val_Efectivo INITIAL 0.
  DEFINE VAR T_NatCheq  LIKE Taquilla.Val_Cheque   INITIAL 0.
  DEFINE VAR T_Total    LIKE Taquilla.Val_Cheque   INITIAL 0.
  
DEFINE FRAME F-LineaEncabezado
    HEADER
      "Operación           Tip Cod Nro.Cuenta     Nit          Documento          Usu         Vlr.Efectivo          Vlr.Cheque" AT 1
      "                    Pto Pto                             Referencia.        Aut                                         " AT 1
      "_______________________________________________________________________________________________________________________" AT 1 SKIP(1)
WITH DOWN WIDTH 180 USE-TEXT PAGE-TOP FRAME F-LineaEncabezado STREAM-IO NO-BOX.

{Incluido\RepEncabezado.i}
DO WITH FRAME {&FRAME-NAME}:
    ASSIGN F_ConChe F_ConEfe F_Cuadre F_RetChe F_RetEfe F_TotCon F_TotRet F_Base F_Cuadre F_Fecha
           Titulo = "Base : $ " + TRIM(STRING(F_Base,">>>,>>>,>>>,>>>,>>9.99")) + "   Fecha : " + STRING(F_Fecha)
           Nom_Cajero = TRIM(SUBSTRING(Cmb_Usuario:SCREEN-VALUE,6,50)).
    RUN Justificar IN W_Manija (INPUT-OUTPUT Titulo,INPUT " ",INPUT 60,INPUT "C").
    W_Reporte   =  "REPORTE   : CUADRE DE CAJA - " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am").
    W_EncColumna = " CAJERO    : " + CAPS(STRING(Nom_Cajero,"X(40)")) + " - Base del Día: $" +  TRIM(STRING(F_Base,">>>,>>>,>>>,>>>,>>9.99")).
    VIEW FRAME F-Encabezado.
    VIEW FRAME F-LineaEncabezado.
    VIEW FRAME F-PiePagina. 
END.  
  
  
  VIEW FRAME F-LineaEncabezado.
  
  DO WITH FRAME {&FRAME-NAME}:
     ASSIGN F_ConChe F_ConEfe F_Cuadre F_RetChe F_RetEfe F_TotCon F_TotRet F_Base F_Cuadre F_Fecha
            Titulo = "Base : $ " + TRIM(STRING(F_Base,">>>,>>>,>>>,>>>,>>9.99")) + "   Fecha : " + STRING(F_Fecha)
            Nom_Cajero = TRIM(SUBSTRING(Cmb_Usuario:SCREEN-VALUE,6,50)).
     RUN Justificar IN W_Manija (INPUT-OUTPUT Titulo,INPUT " ",INPUT 60,INPUT "C").
     RUN Justificar IN W_Manija (INPUT-OUTPUT Nom_Cajero,INPUT " ",INPUT 60,INPUT "C").
     FOR EACH Taquilla WHERE Taquilla.Agencia         EQ W_Agencia
                       AND   Taquilla.Usuario         EQ T_Usuario
                       AND   Taquilla.Estado          EQ 1
                       AND   Taquilla.Fec_Transaccion GE W_F1
                       AND   Taquilla.Fec_Transaccion LE W_F2
                       /*AND   Taquilla.Fec_Transaccion EQ F_Fecha */
                       /*AND   Taquilla.Cod_Operacion   NE 010102005*/ NO-LOCK
                       BREAK BY Taquilla.Naturaleza BY Taquilla.Nit:
         FIND Operacion WHERE Operacion.Cod_Operacion EQ Taquilla.Cod_Operacion NO-LOCK NO-ERROR.
         IF AVAILABLE(Operacion) THEN
            ASSIGN Nom_Ope = Operacion.Nom_Operacion.
         ELSE
            ASSIGN Nom_Ope = "".
         ASSIGN T_NatEfec = T_NatEfec + Taquilla.Val_Efectivo
                T_NatCheq = T_NatCheq + Taquilla.Val_Cheque.
         DISPLAY Nom_Ope                AT 1   NO-LABEL FORMAT "X(20)"
                 Taquilla.Tip_Producto  AT 22  NO-LABEL
                 Taquilla.Cod_Producto  AT 25  NO-LABEL
                 Taquilla.Nro_cuenta    AT 29  NO-LABEL
                 Taquilla.Nit           AT 44  NO-LABEL
                 Taquilla.Num_Documento AT 58  NO-LABEL
                 Taquilla.Autorizo      AT 76  NO-LABEL
                 Taquilla.Val_Efectivo  AT 80  NO-LABEL FORMAT "->>>,>>>,>>>,>>9.99"
                 Taquilla.Val_Cheque    AT 100 NO-LABEL FORMAT "->>>,>>>,>>>,>>9.99"
         WITH FRAME F-CAJA DOWN WIDTH 180 USE-TEXT STREAM-IO NO-BOX.
         DOWN WITH FRAME F-CAJA.
         IF LAST-OF(Taquilla.Naturaleza) THEN DO:
            IF Taquilla.Naturaleza EQ "CR" THEN DO:
               DISPLAY "___________________"   AT 80
                       "___________________"   AT 100
                       "TOTAL CONSIGNACIONES:" AT 58
                       T_NatEfec               AT 80  FORMAT "->>>,>>>,>>>,>>9.99" NO-LABEL
                       T_NatCheq               AT 100 FORMAT "->>>,>>>,>>>,>>9.99" NO-LABEL SKIP(1)
               WITH FRAME F-TOTAL1 DOWN WIDTH 180 USE-TEXT STREAM-IO NO-BOX.
               DOWN WITH FRAME F-TOTAL1.
               ASSIGN T_Total = T_Total + T_NatEfec + T_NatCheq.
            END.
            ELSE DO:
               DISPLAY "___________________"   AT 80
                       "___________________"   AT 100
                       "TOTAL RETIROS       :" AT 58
                       T_NatEfec               AT 80  FORMAT "->>>,>>>,>>>,>>9.99" NO-LABEL
                       T_NatCheq               AT 100 FORMAT "->>>,>>>,>>>,>>9.99" NO-LABEL SKIP(1)
               WITH FRAME F-TOTAL2 DOWN WIDTH 180 USE-TEXT STREAM-IO NO-BOX.
               DOWN WITH FRAME F-TOTAL2.
               ASSIGN T_Total = T_Total - T_NatEfec.
               /*ASSIGN T_Total = T_Total - T_NatEfec - T_NatCheq.*/
            END.
            ASSIGN T_NatEfec = 0 T_NatCheq = 0.
         END.
     END.
     ASSIGN T_Total = T_Total + F_Base.
     DISPLAY "DIFERENCIA DE CAJA" AT 58  NO-LABEL
             T_Total              AT 80  NO-LABEL FORMAT "->>>,>>>,>>>,>>9.99"
     WITH FRAME F-TOTAL3 DOWN WIDTH 180 USE-TEXT STREAM-IO NO-BOX.
     DOWN WITH FRAME F-TOTAL3.
  END.
  PAGE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE General_Conso W-Win 
PROCEDURE General_Conso :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VAR Nom_Cajero AS   CHARACTER FORMAT "X(60)".
  DEFINE VAR Titulo     AS   CHARACTER FORMAT "X(60)".
  DEFINE VAR Nom_Ope    LIKE Operacion.Nom_Operacion.
  DEFINE VAR T_NatEfec  LIKE Taquilla.Val_Efectivo INITIAL 0.
  DEFINE VAR T_NatCheq  LIKE Taquilla.Val_Cheque   INITIAL 0.
  DEFINE VAR T_Total    LIKE Taquilla.Val_Cheque   INITIAL 0.
  DEFINE VAR T_TotCon   LIKE Taquilla.Val_Cheque   INITIAL 0.
  DEFINE VAR T_TotRet   LIKE Taquilla.Val_Cheque   INITIAL 0.

  DEFINE FRAME F-Encabezado
    HEADER
      W_Nom_Entidad                               AT 20 FORMAT "X(60)"
      "Pagina: "                                  AT 94 PAGE-NUMBER FORMAT ">>>9"
      "INFORME DETALLADO DEL MOVIMIENTO DE CAJA - Consolidado"  AT 33
      Titulo                                      AT 20
      "_______________________________________________________________________________________________________________________" AT 1
      "Operación           Tip Cod Nro.Cuenta     Nit          Documento          Usu         Vlr.Efectivo          Vlr.Cheque" AT 1
      "                    Pto Pto                             Referencia.        Aut                                         " AT 1
      "_______________________________________________________________________________________________________________________" AT 1 SKIP(1)
  WITH DOWN WIDTH 180 USE-TEXT PAGE-TOP FRAME F-Encabezado STREAM-IO NO-BOX.
  
  DEFINE FRAME F-PiePagina
    HEADER 
      "Fecha:"         AT   1
      TODAY            AT  10 FORMAT "99/99/9999"
      W_Nom_Agencia    AT  37 FORMAT "X(30)"
      "Hora: "         AT  70 STRING(TIME,"HH:MM AM")
  WITH DOWN WIDTH 180 NO-BOX FRAME F-PiePagina PAGE-BOTTOM USE-TEXT STREAM-IO. 
  
  VIEW FRAME F-Encabezado.
  VIEW FRAME F-PiePagina.
  
  DO WITH FRAME {&FRAME-NAME}:
     ASSIGN F_ConChe F_ConEfe F_Cuadre F_RetChe F_RetEfe F_TotCon F_TotRet F_Base F_Cuadre F_Fecha
            Titulo = "Base : $ " + TRIM(STRING(F_Base,">>>,>>>,>>>,>>>,>>9.99")) + "   Fecha : " + STRING(F_Fecha)
            Nom_Cajero = TRIM(SUBSTRING(Cmb_Usuario:SCREEN-VALUE,6,50)).
     RUN Justificar IN W_Manija (INPUT-OUTPUT Titulo,INPUT " ",INPUT 60,INPUT "C").
     RUN Justificar IN W_Manija (INPUT-OUTPUT Nom_Cajero,INPUT " ",INPUT 60,INPUT "C").
     FOR EACH Usuarios WHERE Usuarios.Agencia         EQ W_Agencia
                         AND Usuarios.Estado          EQ 1
                     NO-LOCK:
        FOR EACH Taquilla WHERE Taquilla.Agencia         EQ W_Agencia
                          AND   Taquilla.Usuario         EQ Usuarios.Usuario
                          AND   Taquilla.Estado          EQ 1
                          AND   Taquilla.Fec_Transaccion GE W_F1 
                          AND   Taquilla.Fec_Transaccion LE W_F2  /* F_Fecha */
                          /*AND   Taquilla.Cod_Operacion   NE 010102005*/ NO-LOCK
                          BREAK BY  Taquilla.Usuario
                                BY  Taquilla.Naturaleza:
            IF FIRST-OF(Taquilla.Usuario) THEN DO:
              Nom_Cajero = usuarios.Nombre.
              DISPLAY 
                  "Cajero: "  AT  2
                   Nom_Cajero AT 10 FORMAT "X(40)" SKIP(1)
              WITH FRAME F-Cajero DOWN WIDTH 100 USE-TEXT NO-LABEL STREAM-IO NO-BOX.
            END.
            FIND Operacion WHERE Operacion.Cod_Operacion EQ Taquilla.Cod_Operacion NO-LOCK NO-ERROR.
            IF AVAILABLE(Operacion) THEN
               ASSIGN Nom_Ope = Operacion.Nom_Operacion.
            ELSE
               ASSIGN Nom_Ope = "".
            ASSIGN T_NatEfec = T_NatEfec + Taquilla.Val_Efectivo
                   T_NatCheq = T_NatCheq + Taquilla.Val_Cheque.
            DISPLAY Nom_Ope                AT 1   NO-LABEL FORMAT "X(20)"
                    Taquilla.Tip_Producto  AT 22  NO-LABEL
                    Taquilla.Cod_Producto  AT 25  NO-LABEL
                    Taquilla.Nro_cuenta    AT 29  NO-LABEL
                    Taquilla.Nit           AT 44  NO-LABEL
                    Taquilla.Num_Documento AT 58  NO-LABEL
                    Taquilla.Autorizo      AT 76  NO-LABEL
                    Taquilla.Val_Efectivo  AT 80  NO-LABEL FORMAT "->>>,>>>,>>>,>>9.99"
                    Taquilla.Val_Cheque    AT 100 NO-LABEL FORMAT "->>>,>>>,>>>,>>9.99"
            WITH FRAME F-CAJA DOWN WIDTH 180 USE-TEXT STREAM-IO NO-BOX.
            DOWN WITH FRAME F-CAJA.
            IF LAST-OF(Taquilla.Naturaleza) THEN DO:
               IF Taquilla.Naturaleza EQ "CR" THEN DO:
                  DISPLAY "___________________"   AT 80
                          "___________________"   AT 100
                          "TOTAL CONSIGNACIONES:" AT 58
                          T_NatEfec               AT 80  FORMAT "->>>,>>>,>>>,>>9.99" NO-LABEL
                          T_NatCheq               AT 100 FORMAT "->>>,>>>,>>>,>>9.99" NO-LABEL SKIP(1)
                  WITH FRAME F-TOTAL1 DOWN WIDTH 180 USE-TEXT STREAM-IO NO-BOX.
                  DOWN WITH FRAME F-TOTAL1.
                  ASSIGN T_Total = T_Total + T_NatEfec + T_NatCheq
                         T_TotCon = T_TotCon + T_NatEfec + T_NatCheq.
               END.
               ELSE DO:
                  DISPLAY "___________________"   AT 80
                          "___________________"   AT 100
                          "TOTAL RETIROS       :" AT 58
                          T_NatEfec               AT 80  FORMAT "->>>,>>>,>>>,>>9.99" NO-LABEL
                          T_NatCheq               AT 100 FORMAT "->>>,>>>,>>>,>>9.99" NO-LABEL SKIP(1)
                  WITH FRAME F-TOTAL2 DOWN WIDTH 180 USE-TEXT STREAM-IO NO-BOX.
                  DOWN WITH FRAME F-TOTAL2.
                  ASSIGN T_Total = T_Total - T_NatEfec - T_NatCheq
                         T_TotRet = T_TotRet + T_NatEfec + T_NatCheq.
               END.
               ASSIGN T_NatEfec = 0 T_NatCheq = 0.
            END.
        END. /* End del for each de Taquilla */
     END.    /* End del for each de usuarios */
     ASSIGN T_Total = T_TotCon - T_TotRet + F_Base.
     DISPLAY 
             SKIP(1)
             "Total Gral Consignaciones: "  AT 2
             T_TotCon                       AT 29 FORMAT "->>>,>>>,>>>,>>9.99"
             "Total Gral Retiros       : "  AT 2
             T_TotRet                       AT 29 FORMAT "->>>,>>>,>>>,>>9.99"
             "DIFERENCIA DE CAJA       :"   AT 2
             T_Total                        AT 29 FORMAT "->>>,>>>,>>>,>>9.99"
     WITH FRAME F-TOTAL3 DOWN WIDTH 180 USE-TEXT NO-LABEL STREAM-IO NO-BOX.
     DOWN WITH FRAME F-TOTAL3.
  END.
  PAGE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Imprimir_Excel W-Win 
PROCEDURE Imprimir_Excel :
/*------------------------------------------------------------------------------
  OBSERVACIONES : Imprimir el detalle.       
------------------------------------------------------------------------------*/
  FOR EACH IExcel: DELETE IExcel. END.
  DEFINE VAR Nom_Cajero AS   CHARACTER FORMAT "X(60)".
  DEFINE VAR Titulo     AS   CHARACTER FORMAT "X(60)".
  DEFINE VAR Nom_Ope    LIKE Operacion.Nom_Operacion.
  DEFINE VAR T_NatEfec  LIKE Taquilla.Val_Efectivo INITIAL 0.
  DEFINE VAR T_NatCheq  LIKE Taquilla.Val_Cheque   INITIAL 0.
  DEFINE VAR T_Total    LIKE Taquilla.Val_Cheque   INITIAL 0.
  DO WITH FRAME {&FRAME-NAME}:
     ASSIGN F_ConChe F_ConEfe F_Cuadre F_RetChe F_RetEfe F_TotCon F_TotRet F_Base F_Cuadre F_Fecha.
     FOR EACH Taquilla WHERE Taquilla.Agencia         EQ W_Agencia
                       AND   Taquilla.Usuario         EQ T_Usuario
                       AND   Taquilla.Estado          EQ 1
                       AND   Taquilla.Fec_Transaccion EQ F_Fecha 
                       AND   Taquilla.Cod_Operacion   NE 010102005 NO-LOCK
                       BREAK BY Taquilla.Naturaleza:
         FIND Operacion WHERE Operacion.Cod_Operacion EQ Taquilla.Cod_Operacion NO-LOCK NO-ERROR.
         IF AVAILABLE(Operacion) THEN
            ASSIGN Nom_Ope = Operacion.Nom_Operacion.
         ELSE
            ASSIGN Nom_Ope = "".
         CREATE IExcel.
         ASSIGN INom_Ope         = Nom_Ope               
                ITip_Producto    = Taquilla.Tip_Producto 
                ICod_Producto    = Taquilla.Cod_Producto 
                INro_cuenta      = Taquilla.Nro_cuenta   
                I_NIT            = Taquilla.Nit          
                INum_Documento   = Taquilla.Num_Documento
                IAutorizo        = Taquilla.Autorizo     
                IVal_Efectivo    = Taquilla.Val_Efectivo 
                IVal_Cheque      = Taquilla.Val_Cheque.
         IF Taquilla.Naturaleza EQ "CR" THEN IIde = "Consignac".
         ELSE IIde  = "Retiro".
     END.
  END.
  RUN Imp_Excel.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Imp_Excel W-Win 
PROCEDURE Imp_Excel :
{Incluido\Def_Excel.i}
 
    DEFINE VAR W_NomPcto   AS CHARACTER FORMAT "X(30)"  INITIAL "".
    DEFINE VAR W_Tipo      AS CHARACTER FORMAT "X(10)"  INITIAL "".
    DEFINE VAR W_Ret       AS DECIMAL   FORMAT "99.99%" INITIAL 0.
 /* MANDA ENCABEZADO DE HOJA DE CALCULO*/
 
 E_NumFila = 1.
 E_NumColumn = 10.
 E_Fila      =      "025" + "Nombre Operacion         "
                  + "001" + "T"
                  + "003" + "Pdt"
                  + "014" + "Numero Cuenta "
                  + "014" + "Nit           "
                  + "010" + "Num.Docto "
                  + "004" + "Autz"
                  + "021" + "Valor en Efectivo    "
                  + "021" + "Valor en Cheque      "
                  + "010" + "Ide       ".
 RUN W-GraExcel.w (INPUT E_Fila, INPUT E_NumColumn, OUTPUT E_CmpGrafic).

/* launch Excel so it is visible to the user */
chExcelApp:Visible = TRUE.

/* create a new Workbook */
chWorkbook = chExcelApp:Workbooks:Add().

/* get the active Worksheet */
chWorkSheet = chExcelApp:Sheets:Item(1).
FOR EACH IExcel BREAK BY IIde:
      E_Fila2     = "".
      E_Fila2     = "025" + STRING(INom_Ope,"X(25)")
                  + "001" + STRING(ITip_Producto,"9")
                  + "003" + STRING(ICod_Producto,"999")
                  + "014" + STRING(INro_cuenta,"X(14)")
                  + "014" + STRING(I_NIT ,"X(14)")
                  + "010" + STRING(INum_Documento,"X(10)")
                  + "004" + STRING(IAutorizo,"X(04)")
                  + "021" + STRING(IVal_Efectivo,"->>>>>,>>>,>>>,>>9.99")
                  + "021" + STRING(IVal_Cheque,"->>>>>,>>>,>>>,>>9.99")
                  + "010" + STRING(IIde,"X(10)").
                  
      {Incluido\imprimir_Excel.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit W-Win 
PROCEDURE local-exit :
/* -----------------------------------------------------------
  Purpose:  Starts an "exit" by APPLYing CLOSE event, which starts "destroy".
  Parameters:  <none>
  Notes:    If activated, should APPLY CLOSE, *not* dispatch adm-exit.   
-------------------------------------------------------------*/
   APPLY "CLOSE":U TO THIS-PROCEDURE.
   
   RETURN.
       
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize W-Win 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VAR W_Reg AS CHARACTER FORMAT "X(50)".
  DEFINE VAR W_Logica  AS   LOGICAL.
  DO WITH FRAME {&FRAME-NAME}:
     RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ).
     ASSIGN F_Fecha:SCREEN-VALUE   = STRING(W_Fecha)
            W_F1 = W_Fecha
            W_F2 = W_Fecha
            F_Fecha
            Cmb_Usuario:LIST-ITEMS = ""
            W_Logica = Cmb_Usuario:ADD-LAST("0000" + "-" + "Consolidado")
            T_Usuario              = W_Usuario.
     FOR EACH Usuarios WHERE Usuarios.Agencia EQ W_Agencia 
                       AND   Usuarios.Estado  EQ 1 
                        NO-LOCK:
         Cmb_Usuario:ADD-LAST(STRING(Usuarios.Usuario,"X(4)") + "-" + STRING(Usuarios.Nombre,"X(40)")).
         IF Usuarios.Usuario EQ W_Usuario THEN
            ASSIGN W_REG = STRING(Usuarios.Usuario,"X(4)") + "-" + STRING(Usuarios.Nombre,"X(40)").
     END.
     ASSIGN Cmb_Usuario:SCREEN-VALUE = W_REG.
  END.
  ASSIGN W_F1:SCREEN-VALUE IN FRAME Frame_Imprimir  = STRING(W_Fecha)
         W_F2:SCREEN-VALUE   = STRING(W_Fecha).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PImprimir W-Win 
PROCEDURE PImprimir :
DEFINE VAR Listado AS CHARACTER INITIAL "".
  Listado = W_PathSpl + "CAJERO.LST" + TRIM(w_usuario).
  {Incluido/Imprimir.I "Listado"}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcesoImprimir W-Win 
PROCEDURE ProcesoImprimir :
/*------------------------------------------------------------------------------
  OBSERVACION : Imprimir el Cuadre.       
------------------------------------------------------------------------------*/
IF WInf EQ 1 THEN DO:
    ASSIGN FRAME Frame_Imprimir Rad_SelImp.
    CASE Rad_SelImp:
      WHEN 1 THEN 
        RUN Total.
      WHEN 2 THEN
        RUN General.
      WHEN 3 THEN 
        RUN Che_Girados.
      WHEN 4 THEN
        RUN Che_Canje.
      WHEN 5 THEN
        RUN Che_Ventanilla.
      WHEN 6 THEN
        RUN Resumido_Cuenta.
      WHEN 7 THEN
        RUN Resumido_CuentaNit.
    END CASE.
    HIDE FRAME frame-Imprimir.
END.
IF WInf EQ 2 THEN DO:
   RUN CierreCaja.
END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Resumido_cuenta W-Win 
PROCEDURE Resumido_cuenta :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF T_Usuario = "0000" THEN DO:
    RUN Resumido_cuentaConso.
    RETURN.
  END.

  DEFINE VAR Nom_Cajero AS   CHARACTER FORMAT "X(60)".
  DEFINE VAR Titulo     AS   CHARACTER FORMAT "X(60)".
  DEFINE VAR Nom_Ope    LIKE Operacion.Nom_Operacion.
  DEFINE VAR T_ConEfec  LIKE Taquilla.Val_Efectivo INITIAL 0.
  DEFINE VAR T_ConCheq  LIKE Taquilla.Val_Cheque   INITIAL 0.
  DEFINE VAR T_RetEfec  LIKE Taquilla.Val_Efectivo INITIAL 0.
  DEFINE VAR T_RetCheq  LIKE Taquilla.Val_Cheque   INITIAL 0.
  DEFINE VAR T_Total    LIKE Taquilla.Val_Cheque   INITIAL 0.
  DEFINE VAR T_TotCon   LIKE Taquilla.Val_Cheque   INITIAL 0.
  DEFINE VAR T_TotRet   LIKE Taquilla.Val_Cheque   INITIAL 0.
  DEFINE VAR T_TotCch   LIKE Taquilla.Val_Cheque   INITIAL 0.
  DEFINE VAR T_TotCef   LIKE Taquilla.Val_Cheque   INITIAL 0.
  DEFINE VAR T_TotRch   LIKE Taquilla.Val_Cheque   INITIAL 0.
  DEFINE VAR T_TotRef   LIKE Taquilla.Val_Cheque   INITIAL 0.
  
  DEFINE FRAME F-LineaEncabezado
    HEADER
      "CONSIGNACIONES" AT 40
      "RETIROS"        AT 68
      "_______________________" AT 35
      "_______________________" AT 61
      "Cuenta "        AT 1
      "En Cheque"      AT 37
      "En Efectivo"    AT 49
      "En Cheque "     AT 62 
      "En Efectivo"    AT 74 
      "-----------------------------------------------------------------------------------" AT 1
  WITH DOWN WIDTH 180 USE-TEXT PAGE-TOP FRAME F-LineaEncabezado NO-LABEL STREAM-IO NO-BOX.
  
  {Incluido\RepEncabezado.i}
  DO WITH FRAME {&FRAME-NAME}:
      ASSIGN Nom_Cajero = TRIM(SUBSTRING(Cmb_Usuario:SCREEN-VALUE,6,50))
             F_Fecha. 

      ASSIGN F_ConChe F_ConEfe F_Cuadre F_RetChe F_RetEfe F_TotCon F_TotRet F_Base F_Cuadre F_Fecha
             Titulo = "Base : $ " + TRIM(STRING(F_Base,">>>,>>>,>>>,>>>,>>9.99")) + "   Fecha : " + STRING(F_Fecha)
             Nom_Cajero = TRIM(SUBSTRING(Cmb_Usuario:SCREEN-VALUE,6,50)).
      RUN Justificar IN W_Manija (INPUT-OUTPUT Titulo,INPUT " ",INPUT 60,INPUT "C").
      W_Reporte   =  "REPORTE   : RESUMIDO POR CUENTA - " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am").
      W_EncColumna = " CAJERO    : " + CAPS(STRING(Nom_Cajero,"X(40)")) + " - Base del Día: $" +  TRIM(STRING(F_Base,">>>,>>>,>>>,>>>,>>9.99")).
      VIEW FRAME F-Encabezado.
      VIEW FRAME F-LineaEncabezado.
      VIEW FRAME F-PiePagina. 
  END.     
  
  
  DO WITH FRAME {&FRAME-NAME}:
     ASSIGN F_ConChe F_ConEfe F_Cuadre F_RetChe F_RetEfe F_TotCon F_TotRet F_Base F_Cuadre F_Fecha
            Titulo = "Base : $ " + TRIM(STRING(F_Base,">>>,>>>,>>>,>>>,>>9.99")) + "   Fecha : " + STRING(F_Fecha)
            Nom_Cajero = TRIM(SUBSTRING(Cmb_Usuario:SCREEN-VALUE,6,50)).
     RUN Justificar IN W_Manija (INPUT-OUTPUT Titulo,INPUT " ",INPUT 60,INPUT "C").
     RUN Justificar IN W_Manija (INPUT-OUTPUT Nom_Cajero,INPUT " ",INPUT 60,INPUT "C").
     FOR EACH Taquilla WHERE Taquilla.Agencia         EQ W_Agencia
                       AND   Taquilla.Usuario         EQ T_Usuario
                       AND   Taquilla.Estado          EQ 1
                       AND   Taquilla.Fec_Transaccion EQ F_Fecha 
                       /*AND   Taquilla.Cod_Operacion   NE 010102005*/ NO-LOCK
                       BREAK BY Taquilla.Cuenta
                             BY  Taquilla.Naturaleza:
         IF Taquilla.Naturaleza EQ "CR" THEN
           ASSIGN T_ConEfec = T_ConEfec + Taquilla.Val_Efectivo
                  T_ConCheq = T_ConCheq + Taquilla.Val_Cheque.
         ELSE
           ASSIGN T_RetEfec = T_RetEfec + Taquilla.Val_Efectivo
                  T_RetCheq = T_RetCheq + Taquilla.Val_Cheque.

         IF LAST-OF(Taquilla.Cuenta) THEN DO:
           FIND Cuentas WHERE Cuentas.Cuenta EQ Taquilla.Cuenta NO-LOCK NO-ERROR.           
           DISPLAY Taquilla.Cuenta        AT 1  FORMAT "X(14)"
                   Cuentas.Nombre         AT 16 FORMAT "X(18)" WHEN AVAILABLE(Cuentas)
                   T_ConCheq              AT 35 FORMAT ">>>>,>>>,>>9"
                   T_ConEfec              AT 48 FORMAT ">>>>,>>>,>>9"
                   T_RetCheq              AT 61 FORMAT ">>>>,>>>,>>9"
                   T_RetEfec              AT 74 FORMAT ">>>>,>>>,>>9"
           WITH FRAME F-CAJA DOWN WIDTH 180 USE-TEXT NO-LABEL STREAM-IO NO-BOX.
           ASSIGN T_TotCon  = T_TotCon + T_ConEfec + T_ConCheq
                  T_TotRet  = T_TotRet + T_RetEfec + T_RetCheq
                  T_TotCch  = T_TotCch + T_ConCheq
                  T_TotCef  = T_TotCef + T_ConEfec
                  T_TotRch  = T_TotRch + T_RetCheq
                  T_TotRef  = T_TotRef + T_RetEfec
                  T_ConEfec = 0 T_ConCheq = 0 T_RetEfec = 0 T_RetCheq = 0.
         END.
     END. /* End del for each de Taquilla */
     ASSIGN T_Total = T_TotCon - T_TotRet + F_Base.
     DISPLAY 
             SKIP(1)
             "_________________________"   AT 35
             "________________________"    AT 62
             "Totales:"                    AT 2
             T_TotCch                      AT 35 FORMAT ">>>>,>>>,>>9"
             T_TotCef                      AT 48 FORMAT ">>>>,>>>,>>9"
             T_TotRch                      AT 61 FORMAT ">>>>,>>>,>>9"
             T_TotRef                      AT 74 FORMAT ">>>>,>>>,>>9"
             SKIP(2)
             "Total Gral Consignaciones: "  AT 2
             T_TotCon                       AT 29 FORMAT "->>>,>>>,>>>,>>9.99"
             "Total Gral Retiros       : "  AT 2
             T_TotRet                       AT 29 FORMAT "->>>,>>>,>>>,>>9.99"
             "DIFERENCIA DE CAJA       :"   AT 2
             T_Total                        AT 29 FORMAT "->>>,>>>,>>>,>>9.99"
     WITH FRAME F-TOTAL3 DOWN WIDTH 180 USE-TEXT NO-LABEL STREAM-IO NO-BOX.
     DOWN WITH FRAME F-TOTAL3.
  END.
  PAGE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Resumido_CuentaConso W-Win 
PROCEDURE Resumido_CuentaConso :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VAR Nom_Cajero    AS   CHARACTER FORMAT "X(60)".
  DEFINE VAR Titulo        AS   CHARACTER FORMAT "X(60)".
  DEFINE VAR Nom_Ope       LIKE Operacion.Nom_Operacion.
  DEFINE VAR T_ConEfec     LIKE Taquilla.Val_Efectivo INITIAL 0.
  DEFINE VAR T_ConCheq     LIKE Taquilla.Val_Cheque   INITIAL 0.
  DEFINE VAR T_RetEfec     LIKE Taquilla.Val_Efectivo INITIAL 0.
  DEFINE VAR T_RetCheq     LIKE Taquilla.Val_Cheque   INITIAL 0.
  DEFINE VAR T_Total       LIKE Taquilla.Val_Cheque   INITIAL 0.
  DEFINE VAR T_TotCon      LIKE Taquilla.Val_Cheque   INITIAL 0.
  DEFINE VAR T_TotRet      LIKE Taquilla.Val_Cheque   INITIAL 0.
  DEFINE VAR T_TotCch      LIKE Taquilla.Val_Cheque   INITIAL 0.
  DEFINE VAR T_TotCef      LIKE Taquilla.Val_Cheque   INITIAL 0.
  DEFINE VAR T_TotRch      LIKE Taquilla.Val_Cheque   INITIAL 0.
  DEFINE VAR T_TotRef      LIKE Taquilla.Val_Cheque   INITIAL 0.
  DEFINE VAR T_TotCchUsu   LIKE Taquilla.Val_Cheque   INITIAL 0.
  DEFINE VAR T_TotCefUsu   LIKE Taquilla.Val_Cheque   INITIAL 0.
  DEFINE VAR T_TotRchUsu   LIKE Taquilla.Val_Cheque   INITIAL 0.
  DEFINE VAR T_TotRefUsu   LIKE Taquilla.Val_Cheque   INITIAL 0.

  DEFINE VAR W_Linea    AS CHARACTER FORMAT "X(88)".
  DEFINE VAR W_Linea2   AS CHARACTER FORMAT "X(88)".

  DEFINE FRAME F-Encabezado
    HEADER
      W_Nom_Entidad                               AT 20 FORMAT "X(60)"
      "Pagina: "                                  AT 94 PAGE-NUMBER FORMAT ">>>9"
      "INFORME RESUMIDO POR CUENTA CONTABLE - Consolidado"  AT 33
      W_Linea2         AT 2 SKIP
      "CONSIGNACIONES" AT 40
      "RETIROS"        AT 68
      "_______________________" AT 35
      "_______________________" AT 61
      "Cuenta "        AT 1
      "En Cheque"      AT 36
      "En Efectivo"    AT 49
      "En Cheque"      AT 63
      "En Efectivo"    AT 76 
      W_Linea          AT  1
  WITH DOWN WIDTH 180 USE-TEXT PAGE-TOP FRAME F-Encabezado NO-LABEL STREAM-IO NO-BOX.
  
  DEFINE FRAME F-PiePagina
    HEADER 
      "Fecha:"         AT   1
      TODAY            AT  10 FORMAT "99/99/9999"
      W_Nom_Agencia    AT  37 FORMAT "X(30)"
      "Hora: "         AT  70 STRING(TIME,"HH:MM AM")
  WITH DOWN WIDTH 180 NO-BOX FRAME F-PiePagina PAGE-BOTTOM USE-TEXT STREAM-IO. 
  ASSIGN W_Linea  = FILL("-",88)
         W_Linea2 = FILL("_",88).  
  VIEW FRAME F-Encabezado.
  VIEW FRAME F-PiePagina.
  
  DO WITH FRAME {&FRAME-NAME}:
     ASSIGN F_ConChe F_ConEfe F_Cuadre F_RetChe F_RetEfe F_TotCon F_TotRet F_Base F_Cuadre F_Fecha
            Titulo = "Base : $ " + TRIM(STRING(F_Base,">>>,>>>,>>>,>>>,>>9.99")) + "   Fecha : " + STRING(F_Fecha)
            Nom_Cajero = TRIM(SUBSTRING(Cmb_Usuario:SCREEN-VALUE,6,50)).
     RUN Justificar IN W_Manija (INPUT-OUTPUT Titulo,INPUT " ",INPUT 60,INPUT "C").
     RUN Justificar IN W_Manija (INPUT-OUTPUT Nom_Cajero,INPUT " ",INPUT 60,INPUT "C").
     FOR EACH Usuarios WHERE Usuarios.Agencia         EQ W_Agencia
                         AND Usuarios.Estado          EQ 1
                     NO-LOCK:
        FOR EACH Taquilla WHERE Taquilla.Agencia         EQ W_Agencia
                          AND   Taquilla.Usuario         EQ Usuarios.Usuario
                          AND   Taquilla.Estado          EQ 1
                          AND   Taquilla.Fec_Transaccion EQ F_Fecha 
                          /*AND   Taquilla.Cod_Operacion   NE 010102005*/ NO-LOCK
                          BREAK BY Taquilla.Usuario
                                BY Taquilla.Cuenta
                                BY Taquilla.Naturaleza:
            
            IF Taquilla.Naturaleza EQ "CR" THEN
              ASSIGN T_ConEfec = T_ConEfec + Taquilla.Val_Efectivo
                     T_ConCheq = T_ConCheq + Taquilla.Val_Cheque.
            ELSE
              ASSIGN T_RetEfec = T_RetEfec + Taquilla.Val_Efectivo
                     T_RetCheq = T_RetCheq + Taquilla.Val_Cheque.

            IF FIRST-OF(Taquilla.usuario) THEN DO:
               DISPLAY 
                 "Usuario: "       AT 2
                 Usuario.Nombre    AT 11
               WITH FRAME F-Usuario DOWN WIDTH 180 USE-TEXT NO-LABEL STREAM-IO NO-BOX.
            END.
            IF LAST-OF(Taquilla.Cuenta) THEN DO:
              FIND Cuentas WHERE Cuentas.Cuenta EQ Taquilla.Cuenta NO-LOCK NO-ERROR.
              DISPLAY Taquilla.Cuenta        AT 1  FORMAT "X(14)"
                      Cuentas.Nombre         AT 16 FORMAT "X(18)" WHEN AVAILABLE(Cuentas)
                      T_ConCheq              AT 35 FORMAT ">>>>,>>>,>>9"
                      T_ConEfec              AT 48 FORMAT ">>>>,>>>,>>9"
                      T_RetCheq              AT 61 FORMAT ">>>>,>>>,>>9"
                      T_RetEfec              AT 74 FORMAT ">>>>,>>>,>>9"
              WITH FRAME F-CAJA DOWN WIDTH 180 USE-TEXT NO-LABEL STREAM-IO NO-BOX.
              ASSIGN T_TotCon  = T_TotCon + T_ConEfec + T_ConCheq
                     T_TotRet  = T_TotRet + T_RetEfec + T_RetCheq
                     T_TotCchUsu = T_TotCchUsu + T_ConCheq
                     T_TotCefUsu = T_TotCefUsu + T_ConEfec
                     T_TotRchUsu = T_TotRchUsu + T_RetCheq
                     T_TotRefUsu = T_TotRefUsu + T_RetEfec
                     T_ConEfec   = 0 T_ConCheq = 0 T_RetEfec = 0 T_RetCheq = 0.
            END.
            IF LAST-OF(Taquilla.Usuario) THEN DO:
              DISPLAY 
               "_________________________"    AT 35
               "________________________"     AT 62
                "Total Usuario"               AT 1
                T_TotCchUsu                   AT 35 FORMAT ">>>>,>>>,>>9"
                T_TotCefUsu                   AT 48 FORMAT ">>>>,>>>,>>9"
                T_TotRchUsu                   AT 61 FORMAT ">>>>,>>>,>>9"
                T_TotRefUsu                   AT 74 FORMAT ">>>>,>>>,>>9" skip(1)
              WITH FRAME F-Totusu DOWN WIDTH 180 USE-TEXT NO-LABEL STREAM-IO NO-BOX.
              ASSIGN T_TotCch  = T_TotCch + T_TotCchUsu
                     T_TotCef  = T_TotCef + T_TotCefUsu
                     T_TotRch  = T_TotRch + T_TotRchUsu
                     T_TotRef  = T_TotRef + T_TotRefusu
                     T_TotCchUsu = 0 T_TotCefUsu = 0 
                     T_TotRchUsu = 0 T_TotRefusu = 0.
            END.

        END. /* End del for each de Taquilla */
     END.    /* End del for each de usuarios */ 
     ASSIGN T_Total = T_TotCon - T_TotRet + F_Base.
     DISPLAY 
             SKIP(1)
             "_________________________" AT 35
             "________________________" AT 62
             "Totales:"                    AT 2
             T_TotCch                      AT 35 FORMAT ">>>>,>>>,>>9"
             T_TotCef                      AT 48 FORMAT ">>>>,>>>,>>9"
             T_TotRch                      AT 61 FORMAT ">>>>,>>>,>>9"
             T_TotRef                      AT 74 FORMAT ">>>>,>>>,>>9"
             SKIP(2)
             "Total Gral Consignaciones: "  AT 2
             T_TotCon                       AT 29 FORMAT "->>>,>>>,>>>,>>9.99"
             "Total Gral Retiros       : "  AT 2
             T_TotRet                       AT 29 FORMAT "->>>,>>>,>>>,>>9.99"
             "DIFERENCIA DE CAJA       :"   AT 2
             T_Total                        AT 29 FORMAT "->>>,>>>,>>>,>>9.99"
     WITH FRAME F-TOTAL3 DOWN WIDTH 180 USE-TEXT NO-LABEL STREAM-IO NO-BOX.
     DOWN WITH FRAME F-TOTAL3.
  END.
  PAGE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Resumido_CuentaNit W-Win 
PROCEDURE Resumido_CuentaNit :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF T_Usuario = "0000" THEN DO:
    RUN Resum_cuentanitConso.
    RETURN.
  END.
  DEFINE VAR Nom_Cajero    AS   CHARACTER FORMAT "X(60)".
  DEFINE VAR Titulo        AS   CHARACTER FORMAT "X(60)".
  DEFINE VAR Nom_Ope       LIKE Operacion.Nom_Operacion.
  DEFINE VAR Nom_Cuenta    LIKE Operacion.Nom_Operacion.
  DEFINE VAR T_ConEfec     LIKE Taquilla.Val_Efectivo INITIAL 0.
  DEFINE VAR T_ConCheq     LIKE Taquilla.Val_Cheque   INITIAL 0.
  DEFINE VAR T_RetEfec     LIKE Taquilla.Val_Efectivo INITIAL 0.
  DEFINE VAR T_RetCheq     LIKE Taquilla.Val_Cheque   INITIAL 0.
  DEFINE VAR T_Total       LIKE Taquilla.Val_Cheque   INITIAL 0.
  DEFINE VAR T_TotCon      LIKE Taquilla.Val_Cheque   INITIAL 0.
  DEFINE VAR T_TotRet      LIKE Taquilla.Val_Cheque   INITIAL 0.
  DEFINE VAR T_TotCch      LIKE Taquilla.Val_Cheque   INITIAL 0.
  DEFINE VAR T_TotCef      LIKE Taquilla.Val_Cheque   INITIAL 0.
  DEFINE VAR T_TotRch      LIKE Taquilla.Val_Cheque   INITIAL 0.
  DEFINE VAR T_TotRef      LIKE Taquilla.Val_Cheque   INITIAL 0.
  DEFINE VAR T_TotCchCta   LIKE Taquilla.Val_Cheque   INITIAL 0.
  DEFINE VAR T_TotCefCta   LIKE Taquilla.Val_Cheque   INITIAL 0.
  DEFINE VAR T_TotRchCta   LIKE Taquilla.Val_Cheque   INITIAL 0.
  DEFINE VAR T_TotRefCta   LIKE Taquilla.Val_Cheque   INITIAL 0.



  DEFINE FRAME F-LineaEncabezado
    HEADER
      "CONSIGNACIONES" AT 28
      "RETIROS"        AT 67 
      "______________________________" AT 23
      "______________________________" AT 55
      "Nit "           AT 1
      "En Cheque"      AT 23 
      "En Efectivo"    AT 39
      "En Cheque   :"  AT 55 
      "En Efectivo :"  AT 71 
      "----------------------------------------------------------------------------------------"          AT  1
  WITH DOWN WIDTH 180 USE-TEXT PAGE-TOP FRAME F-LineaEncabezado NO-LABEL STREAM-IO NO-BOX.
  
  {Incluido\RepEncabezado.i}
  DO WITH FRAME {&FRAME-NAME}:
      ASSIGN Nom_Cajero = TRIM(SUBSTRING(Cmb_Usuario:SCREEN-VALUE,6,50))
             F_Fecha. 

      ASSIGN F_ConChe F_ConEfe F_Cuadre F_RetChe F_RetEfe F_TotCon F_TotRet F_Base F_Cuadre F_Fecha
             Titulo = "Base : $ " + TRIM(STRING(F_Base,">>>,>>>,>>>,>>>,>>9.99")) + "   Fecha : " + STRING(F_Fecha)
             Nom_Cajero = TRIM(SUBSTRING(Cmb_Usuario:SCREEN-VALUE,6,50)).
      RUN Justificar IN W_Manija (INPUT-OUTPUT Titulo,INPUT " ",INPUT 60,INPUT "C").
      W_Reporte   =  "REPORTE   : RESUMIDO POR NIT y CUENTA - " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am").
      W_EncColumna = " CAJERO    : " + CAPS(STRING(Nom_Cajero,"X(40)")) + " - Base del Día: $" +  TRIM(STRING(F_Base,">>>,>>>,>>>,>>>,>>9.99")).
      VIEW FRAME F-Encabezado.
      VIEW FRAME F-LineaEncabezado.
      VIEW FRAME F-PiePagina. 
  END.     


  DO WITH FRAME {&FRAME-NAME}:
     ASSIGN F_ConChe F_ConEfe F_Cuadre F_RetChe F_RetEfe F_TotCon F_TotRet F_Base F_Cuadre F_Fecha
            Titulo = "Base : $ " + TRIM(STRING(F_Base,">>>,>>>,>>>,>>>,>>9.99")) + "   Fecha : " + STRING(F_Fecha)
            Nom_Cajero = TRIM(SUBSTRING(Cmb_Usuario:SCREEN-VALUE,6,50)).
     RUN Justificar IN W_Manija (INPUT-OUTPUT Titulo,INPUT " ",INPUT 60,INPUT "C").
     RUN Justificar IN W_Manija (INPUT-OUTPUT Nom_Cajero,INPUT " ",INPUT 60,INPUT "C").
     FOR EACH Taquilla WHERE Taquilla.Agencia         EQ W_Agencia
                       AND   Taquilla.Usuario         EQ T_Usuario
                       AND   Taquilla.Estado          EQ 1
                       AND   Taquilla.Fec_Transaccion EQ F_Fecha 
                       /*AND   Taquilla.Cod_Operacion   NE 010102005*/ NO-LOCK
                       BREAK BY Taquilla.Usuario
                             BY Taquilla.Cuenta
                             BY Taquilla.Nit
                             BY  Taquilla.Naturaleza:
         IF FIRST-OF(Taquilla.Usuario) THEN DO:
           FIND Usuarios WHERE Usuarios.Usuario EQ T_Usuario
                           AND Usuarios.Agencia EQ W_Agencia           
                       NO-LOCK NO-ERROR.
           DISPLAY
              "Usuario: "         AT 2
              Usuario.Nombre      AT 11  WHEN AVAILABLE(Usuarios)
              SKIP(1)
            WITH FRAME F-Usuario DOWN WIDTH 180 USE-TEXT NO-LABEL STREAM-IO NO-BOX.
         END.
         IF FIRST-OF(Taquilla.Cuenta) THEN DO:
           FIND Cuentas WHERE Cuentas.Cuenta EQ Taquilla.Cuenta
                      NO-LOCK NO-ERROR.
           Nom_cuenta = "".
           IF AVAILABLE(Cuentas) THEN 
             Nom_cuenta = Cuentas.Nombre.
           DISPLAY
              "Cuenta: "         AT 2
              Nom_Cuenta         AT 10 SKIP(1)
            WITH FRAME F-Cuenta DOWN WIDTH 180 USE-TEXT NO-LABEL STREAM-IO NO-BOX.
         END.
         IF Taquilla.Naturaleza EQ "CR" THEN
           ASSIGN T_ConEfec = T_ConEfec + Taquilla.Val_Efectivo
                  T_ConCheq = T_ConCheq + Taquilla.Val_Cheque.
         ELSE
           ASSIGN T_RetEfec = T_RetEfec + Taquilla.Val_Efectivo
                  T_RetCheq = T_RetCheq + Taquilla.Val_Cheque.

         IF LAST-OF(Taquilla.Nit) THEN DO:
           DISPLAY Taquilla.Nit           AT 1   FORMAT "X(14)"
                   T_ConCheq              AT 23 FORMAT ">>>>,>>>,>>9"
                   T_ConEfec              AT 39 FORMAT ">>>>,>>>,>>9"
                   T_RetCheq              AT 55 FORMAT ">>>>,>>>,>>9"
                   T_RetEfec              AT 71 FORMAT ">>>>,>>>,>>9"
           WITH FRAME F-CAJA DOWN WIDTH 180 USE-TEXT NO-LABEL STREAM-IO NO-BOX.
           ASSIGN T_TotCon  = T_TotCon + T_ConEfec + T_ConCheq
                  T_TotRet  = T_TotRet + T_RetEfec + T_RetCheq
                  T_TotCchCta = T_TotCchCta + T_ConCheq
                  T_TotCefCta = T_TotCefCta + T_ConEfec
                  T_TotRchCta = T_TotRchCta + T_RetCheq
                  T_TotRefCta = T_TotRefCta + T_RetEfec
                  T_ConEfec   = 0 T_ConCheq = 0 T_RetEfec = 0 T_RetCheq = 0.
         END.
         IF LAST-OF(Taquilla.Cuenta) THEN DO:
           DISPLAY 
             "_____________________________" AT 25
             "_____________________________" AT 57
             "Totales x Cuenta"            AT 2
             T_TotCchcta                   AT 23 FORMAT ">>>>,>>>,>>9"
             T_TotCefcta                   AT 39 FORMAT ">>>>,>>>,>>9"
             T_TotRchcta                   AT 55 FORMAT ">>>>,>>>,>>9"
             T_TotRefcta                   AT 71 FORMAT ">>>>,>>>,>>9" SKIP(1)
           WITH FRAME F-TotCta DOWN WIDTH 180 USE-TEXT NO-LABEL STREAM-IO NO-BOX.
           ASSIGN  T_TotCch = T_TotCch + T_TotCchCta
                   T_TotCef = T_TotCef + T_TotCefCta
                   T_TotRch = T_TotRch + T_TotRchCta
                   T_TotRef = T_TotRef + T_TotRefCta
                   T_TotCchCta = 0 T_TotCefCta = 0
                   T_TotRchCta = 0 T_TotRefCta = 0.
         END.
     END. /* End del for each de Taquilla */
     ASSIGN T_Total = T_TotCon - T_TotRet + F_Base.
     DISPLAY 
             SKIP(1)
             "_____________________________" AT 25
             "_____________________________" AT 57
             "Totales:"                    AT 2
             T_TotCch                      AT 23 FORMAT ">>>>,>>>,>>9.99"
             T_TotCef                      AT 39 FORMAT ">>>>,>>>,>>9.99"
             T_TotRch                      AT 55 FORMAT ">>>>,>>>,>>9.99"
             T_TotRef                      AT 71 FORMAT ">>>>,>>>,>>9.99"
             SKIP(2)
             "Total Gral Consignaciones: "  AT 2
             T_TotCon                       AT 29 FORMAT "->>>,>>>,>>>,>>9.99"
             "Total Gral Retiros       : "  AT 2
             T_TotRet                       AT 29 FORMAT "->>>,>>>,>>>,>>9.99"
             "DIFERENCIA DE CAJA       :"   AT 2
             T_Total                        AT 29 FORMAT "->>>,>>>,>>>,>>9.99"
     WITH FRAME F-TOTAL3 DOWN WIDTH 180 USE-TEXT NO-LABEL STREAM-IO NO-BOX.
     DOWN WITH FRAME F-TOTAL3.
  END.
  PAGE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Resum_cuentanitConso W-Win 
PROCEDURE Resum_cuentanitConso :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VAR Nom_Cajero    AS   CHARACTER FORMAT "X(60)".
  DEFINE VAR Titulo        AS   CHARACTER FORMAT "X(60)".
  DEFINE VAR Nom_Ope       LIKE Operacion.Nom_Operacion.
  DEFINE VAR Nom_Cuenta    LIKE Operacion.Nom_Operacion.
  DEFINE VAR T_ConEfec     LIKE Taquilla.Val_Efectivo INITIAL 0.
  DEFINE VAR T_ConCheq     LIKE Taquilla.Val_Cheque   INITIAL 0.
  DEFINE VAR T_RetEfec     LIKE Taquilla.Val_Efectivo INITIAL 0.
  DEFINE VAR T_RetCheq     LIKE Taquilla.Val_Cheque   INITIAL 0.
  DEFINE VAR T_Total       LIKE Taquilla.Val_Cheque   INITIAL 0.
  DEFINE VAR T_TotCon      LIKE Taquilla.Val_Cheque   INITIAL 0.
  DEFINE VAR T_TotRet      LIKE Taquilla.Val_Cheque   INITIAL 0.
  DEFINE VAR T_TotCch      LIKE Taquilla.Val_Cheque   INITIAL 0.
  DEFINE VAR T_TotCef      LIKE Taquilla.Val_Cheque   INITIAL 0.
  DEFINE VAR T_TotRch      LIKE Taquilla.Val_Cheque   INITIAL 0.
  DEFINE VAR T_TotRef      LIKE Taquilla.Val_Cheque   INITIAL 0.
  DEFINE VAR T_TotCchCta   LIKE Taquilla.Val_Cheque   INITIAL 0.
  DEFINE VAR T_TotCefCta   LIKE Taquilla.Val_Cheque   INITIAL 0.
  DEFINE VAR T_TotRchCta   LIKE Taquilla.Val_Cheque   INITIAL 0.
  DEFINE VAR T_TotRefCta   LIKE Taquilla.Val_Cheque   INITIAL 0.
  DEFINE VAR T_TotCchUsu   LIKE Taquilla.Val_Cheque   INITIAL 0.
  DEFINE VAR T_TotCefUsu   LIKE Taquilla.Val_Cheque   INITIAL 0.
  DEFINE VAR T_TotRchUsu   LIKE Taquilla.Val_Cheque   INITIAL 0.
  DEFINE VAR T_TotRefUsu   LIKE Taquilla.Val_Cheque   INITIAL 0.


  DEFINE VAR W_Linea       AS CHARACTER FORMAT "X(88)".
  DEFINE VAR W_Linea2      AS CHARACTER FORMAT "X(88)".

  DEFINE FRAME F-Encabezado
    HEADER
      W_Nom_Entidad                               AT 20 FORMAT "X(60)"
      "Pagina: "                                  AT 94 PAGE-NUMBER FORMAT ">>>9"
      "INFORME RESUMIDO POR CUENTA CONTABLE y NIT - Consolidado"  AT 33
      W_Linea2         AT 2 SKIP
      "CONSIGNACIONES" AT 28
      "RETIROS"        AT 67 
      "______________________________" AT 23
      "______________________________" AT 55
      "Nit "           AT 1
      "En Cheque"      AT 23 
      "En Efectivo"    AT 39
      "En Cheque   :"  AT 55 
      "En Efectivo :"  AT 71 
      W_Linea          AT  1
  WITH DOWN WIDTH 180 USE-TEXT PAGE-TOP FRAME F-Encabezado NO-LABEL STREAM-IO NO-BOX.
  
  DEFINE FRAME F-PiePagina
    HEADER 
      "Fecha:"         AT   1
      TODAY            AT  10 FORMAT "99/99/9999"
      W_Nom_Agencia    AT  37 FORMAT "X(30)"
      "Hora: "         AT  70 STRING(TIME,"HH:MM AM")
  WITH DOWN WIDTH 180 NO-BOX FRAME F-PiePagina PAGE-BOTTOM USE-TEXT STREAM-IO. 
  ASSIGN W_Linea  = FILL("-",88)
         W_Linea2 = FILL("_",88).  
  VIEW FRAME F-Encabezado.
  VIEW FRAME F-PiePagina.
  
  DO WITH FRAME {&FRAME-NAME}:
     ASSIGN F_ConChe F_ConEfe F_Cuadre F_RetChe F_RetEfe F_TotCon F_TotRet F_Base F_Cuadre F_Fecha
            Titulo = "Base : $ " + TRIM(STRING(F_Base,">>>,>>>,>>>,>>>,>>9.99")) + "   Fecha : " + STRING(F_Fecha)
            Nom_Cajero = TRIM(SUBSTRING(Cmb_Usuario:SCREEN-VALUE,6,50)).
     RUN Justificar IN W_Manija (INPUT-OUTPUT Titulo,INPUT " ",INPUT 60,INPUT "C").
     RUN Justificar IN W_Manija (INPUT-OUTPUT Nom_Cajero,INPUT " ",INPUT 60,INPUT "C").
     FOR EACH Usuarios WHERE Usuarios.Agencia         EQ W_Agencia
                         AND Usuarios.Estado          EQ 1
                     NO-LOCK:
        FOR EACH Taquilla WHERE Taquilla.Agencia         EQ W_Agencia
                          AND   Taquilla.Usuario         EQ Usuarios.Usuario
                          AND   Taquilla.Estado          EQ 1
                          AND   Taquilla.Fec_Transaccion EQ F_Fecha 
                          /*AND   Taquilla.Cod_Operacion   NE 010102005*/ NO-LOCK
                          BREAK BY Taquilla.Usuario
                                BY Taquilla.Cuenta
                                BY Taquilla.Nit
                                BY  Taquilla.Naturaleza:
            IF FIRST-OF(Taquilla.Usuario) THEN DO:
              DISPLAY
                 "Usuario: "         AT 2
                 Usuario.Nombre      AT 11 SKIP(1)
               WITH FRAME F-Usuario DOWN WIDTH 180 USE-TEXT NO-LABEL STREAM-IO NO-BOX.
            END.
            IF FIRST-OF(Taquilla.Cuenta) THEN DO:
              FIND Cuentas WHERE Cuentas.Cuenta EQ Taquilla.Cuenta
                         NO-LOCK NO-ERROR.
              Nom_cuenta = "".
              IF AVAILABLE(Cuentas) THEN 
                Nom_cuenta = Cuentas.Nombre.
              DISPLAY
                 "Cuenta: "         AT 2
                 Nom_Cuenta         AT 10
               WITH FRAME F-Cuenta DOWN WIDTH 180 USE-TEXT NO-LABEL STREAM-IO NO-BOX.
            END.
            IF Taquilla.Naturaleza EQ "CR" THEN
              ASSIGN T_ConEfec = T_ConEfec + Taquilla.Val_Efectivo
                     T_ConCheq = T_ConCheq + Taquilla.Val_Cheque.
            ELSE
              ASSIGN T_RetEfec = T_RetEfec + Taquilla.Val_Efectivo
                     T_RetCheq = T_RetCheq + Taquilla.Val_Cheque.

            IF LAST-OF(Taquilla.Nit) THEN DO:
              DISPLAY Taquilla.Nit           AT 1   FORMAT "X(14)"
                      T_ConCheq              AT 23 FORMAT ">>>>,>>>,>>9"
                      T_ConEfec              AT 39 FORMAT ">>>>,>>>,>>9"
                      T_RetCheq              AT 55 FORMAT ">>>>,>>>,>>9"
                      T_RetEfec              AT 71 FORMAT ">>>>,>>>,>>9"
              WITH FRAME F-CAJA DOWN WIDTH 180 USE-TEXT NO-LABEL STREAM-IO NO-BOX.
              ASSIGN T_TotCon  = T_TotCon + T_ConEfec + T_ConCheq
                     T_TotRet  = T_TotRet + T_RetEfec + T_RetCheq
                     T_TotCchCta = T_TotCchCta + T_ConCheq
                     T_TotCefCta = T_TotCefCta + T_ConEfec
                     T_TotRchCta = T_TotRchCta + T_RetCheq
                     T_TotRefCta = T_TotRefCta + T_RetEfec
                     T_ConEfec   = 0 T_ConCheq = 0 T_RetEfec = 0 T_RetCheq = 0.
            END.
            IF LAST-OF(Taquilla.Cuenta) THEN DO:
              DISPLAY 
                "_____________________________" AT 25
                "_____________________________" AT 57
                "Totales x Cuenta"            AT 2
                T_TotCchcta                   AT 23 FORMAT ">>>>,>>>,>>9"
                T_TotCefcta                   AT 39 FORMAT ">>>>,>>>,>>9"
                T_TotRchcta                   AT 55 FORMAT ">>>>,>>>,>>9"
                T_TotRefcta                   AT 71 FORMAT ">>>>,>>>,>>9" SKIP(1)
              WITH FRAME F-TotCta DOWN WIDTH 180 USE-TEXT NO-LABEL STREAM-IO NO-BOX.
              ASSIGN  T_TotCchUsu = T_TotCchUsu + T_TotCchCta
                      T_TotCefUsu = T_TotCefUsu + T_TotCefCta
                      T_TotRchUsu = T_TotRchUsu + T_TotRchCta
                      T_TotRefUsu = T_TotRefUsu + T_TotRefCta
                      T_TotCchCta = 0 T_TotCefCta = 0
                      T_TotRchCta = 0 T_TotRefCta = 0.
            END.

            IF LAST-OF(Taquilla.Usuario) THEN DO:
              DISPLAY 
                "_____________________________" AT 25
                "_____________________________" AT 57
                "Total X Usuario"               AT 1
                T_TotCchUsu                   AT 23 FORMAT ">>>>,>>>,>>9"
                T_TotCefUsu                   AT 39 FORMAT ">>>>,>>>,>>9"
                T_TotRchUsu                   AT 55 FORMAT ">>>>,>>>,>>9"
                T_TotRefUsu                   AT 71 FORMAT ">>>>,>>>,>>9" skip(1)
              WITH FRAME F-Totusu DOWN WIDTH 180 USE-TEXT NO-LABEL STREAM-IO NO-BOX.
              ASSIGN T_TotCch  = T_TotCch + T_TotCchUsu
                     T_TotCef  = T_TotCef + T_TotCefUsu
                     T_TotRch  = T_TotRch + T_TotRchUsu
                     T_TotRef  = T_TotRef + T_TotRefusu
                     T_TotCchUsu = 0 T_TotCefUsu = 0 
                     T_TotRchUsu = 0 T_TotRefusu = 0.
            END.
        END. /* End del for each de Taquilla */
     END.    /* End del for each de usuarios */ 
     ASSIGN T_Total = T_TotCon - T_TotRet + F_Base.
     DISPLAY 
             SKIP(1)
             "_____________________________" AT 25
             "_____________________________" AT 57
             "Totales:"                    AT 2
             T_TotCch                      AT 23 FORMAT ">>>>,>>>,>>9.99"
             T_TotCef                      AT 39 FORMAT ">>>>,>>>,>>9.99"
             T_TotRch                      AT 55 FORMAT ">>>>,>>>,>>9.99"
             T_TotRef                      AT 71 FORMAT ">>>>,>>>,>>9.99"
             SKIP(2)
             "Total Gral Consignaciones: "  AT 2
             T_TotCon                       AT 29 FORMAT "->>>,>>>,>>>,>>9.99"
             "Total Gral Retiros       : "  AT 2
             T_TotRet                       AT 29 FORMAT "->>>,>>>,>>>,>>9.99"
             "DIFERENCIA DE CAJA       :"   AT 2
             T_Total                        AT 29 FORMAT "->>>,>>>,>>>,>>9.99"
     WITH FRAME F-TOTAL3 DOWN WIDTH 180 USE-TEXT NO-LABEL STREAM-IO NO-BOX.
     DOWN WITH FRAME F-TOTAL3.
  END.
  PAGE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Saldo_Cero W-Win 
PROCEDURE Saldo_Cero :
/*------------------------------------------------------------------------------
  OBSERVACIONES : Borrar Saldo.       
------------------------------------------------------------------------------*/
  DO WITH FRAME {&FRAME-NAME}:
     ASSIGN F_ConChe:SCREEN-VALUE = "0"
            F_ConEfe:SCREEN-VALUE = "0"
            F_RetChe:SCREEN-VALUE = "0"
            F_RetEfe:SCREEN-VALUE = "0"
            F_TotCon:SCREEN-VALUE = "0"
            F_TotRet:SCREEN-VALUE = "0"
            F_Cuadre:SCREEN-VALUE = "0"
            F_Base:SCREEN-VALUE   = "0"
            W_RetEfec = 0 
            W_RetCheq = 0 
            W_ConEfec = 0 
            W_ConCheq = 0.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records W-Win  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "Cajero"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed W-Win 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Total W-Win 
PROCEDURE Total :
/*------------------------------------------------------------------------------
  OBSERVACION : IMP TOTALES.       
------------------------------------------------------------------------------*/

  IF T_Usuario = "0000" THEN DO:
    RUN Total_Conso.
    RETURN.
  END.
DEFINE VAR Nom_Cajero AS CHARACTER FORMAT "X(60)".
DEFINE VAR Titulo     AS CHARACTER FORMAT "X(60)".

{Incluido\RepEncabezado.i}
DO WITH FRAME {&FRAME-NAME}:
    ASSIGN F_ConChe F_ConEfe F_Cuadre F_RetChe F_RetEfe F_TotCon F_TotRet F_Base F_Cuadre F_Fecha
           Titulo = "Base : $ " + TRIM(STRING(F_Base,">>>,>>>,>>>,>>>,>>9.99")) + "   Fecha : " + STRING(F_Fecha)
           Nom_Cajero = TRIM(SUBSTRING(Cmb_Usuario:SCREEN-VALUE,6,50)).
    RUN Justificar IN W_Manija (INPUT-OUTPUT Titulo,INPUT " ",INPUT 60,INPUT "C").
    W_Reporte   =  "REPORTE   : CUADRE DE CAJA - " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am").
    W_EncColumna = " CAJERO    : " + CAPS(STRING(Nom_Cajero,"X(40)")) + " - Base del Día: $" +  TRIM(STRING(F_Base,">>>,>>>,>>>,>>>,>>9.99")).
    VIEW FRAME F-Encabezado.
    VIEW FRAME F-PiePagina. 
END.     
     DISPLAY "CONSIGNACIONES" AT 16 NO-LABEL
             "RETIROS"        AT 67 NO-LABEL
             "_____________________________________________" AT 2
             "_________________________________________" AT 51
             "Consignación Cheque   :" AT 2  NO-LABEL
             F_ConChe                  AT 25 NO-LABEL FORMAT "->>,>>>,>>>,>>>,>>9.99"
             "Retiro Cheque   :"       AT 51 NO-LABEL
             F_RetChe                  AT 69 NO-LABEL FORMAT "->>,>>>,>>>,>>>,>>9.99"
             "Consignacion Efectivo :" AT 2  NO-LABEL
             F_ConEfe                  AT 25 NO-LABEL FORMAT "->>,>>>,>>>,>>>,>>9.99"
             "Retiro Efectivo :"       AT 51 NO-LABEL
             F_RetEfe                  AT 69 NO-LABEL FORMAT "->>,>>>,>>>,>>>,>>9.99"
             "___________________"     AT 28 NO-LABEL
             "___________________"     AT 72 NO-LABEL
             "Total Consignación"      AT 2  NO-LABEL
             F_TotCon                  AT 25 NO-LABEL FORMAT "->>,>>>,>>>,>>>,>>9.99"
             "Total Retiro"            AT 51 NO-LABEL
             F_TotRet                  AT 69 NO-LABEL FORMAT "->>,>>>,>>>,>>>,>>9.99" SKIP(2)
             "DIFERENCIA DE CAJA"      AT 2  NO-LABEL
             F_Cuadre                  AT 25 NO-LABEL FORMAT "->>,>>>,>>>,>>>,>>9.99"
     WITH FRAME F-CAJA DOWN WIDTH 132 USE-TEXT STREAM-IO NO-BOX.
     DOWN WITH FRAME F-CAJA.
  
  PAGE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Total_conso W-Win 
PROCEDURE Total_conso :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VAR Nom_Cajero AS CHARACTER FORMAT "X(40)".
  DEFINE VAR Titulo     AS CHARACTER FORMAT "X(60)".
  DEFINE VAR W_Linea    AS CHARACTER FORMAT "X(88)".
  DEFINE VAR W_Linea2   AS CHARACTER FORMAT "X(88)".
  
  DEFINE FRAME F-Encabezado
    HEADER
      W_Nom_Entidad                           AT 20 FORMAT "X(60)"
      "Pagina: "                              AT 76 PAGE-NUMBER FORMAT ">>>9"
      "INFORME DE TOTALES DE CUADRE DE CAJA - Consolidado"  AT 33
      Titulo                                  AT 20
      W_Linea2         AT 2 SKIP
      "CONSIGNACIONES" AT 28
      "RETIROS"        AT 67 
      "______________________________" AT 23
      "______________________________" AT 55
      "Usuario"        AT 1
      "En Cheque"      AT 23 
      "En Efectivo"    AT 39
      "En Cheque   :"  AT 55 
      "En Efectivo :"  AT 71 
      W_Linea          AT  1
  WITH DOWN WIDTH 132 USE-TEXT PAGE-TOP FRAME F-Encabezado STREAM-IO NO-BOX.
  
  DEFINE FRAME F-PiePagina
    HEADER 
      "Fecha:"         AT   1
      TODAY            AT  10 FORMAT "99/99/9999"
      W_Nom_Agencia    AT  37 FORMAT "X(30)"
      "Hora: "         AT  70 STRING(TIME,"HH:MM AM")
  WITH DOWN WIDTH 132 NO-BOX FRAME F-PiePagina PAGE-BOTTOM USE-TEXT STREAM-IO. 
      
  ASSIGN W_Linea  = FILL("-",88)
         W_Linea2 = FILL("_",88).

  VIEW FRAME F-Encabezado.
  VIEW FRAME F-PiePagina.
  
  DO WITH FRAME {&FRAME-NAME}:
     ASSIGN F_ConChe F_ConEfe F_Cuadre F_RetChe F_RetEfe F_TotCon F_TotRet F_Base F_Cuadre F_Fecha
            Titulo = "Base : $ " + TRIM(STRING(F_Base,">>>,>>>,>>>,>>>,>>9.99")) + "   Fecha : " + STRING(F_Fecha)
            Nom_Cajero = TRIM(SUBSTRING(Cmb_Usuario:SCREEN-VALUE,6,50)).
     FOR EACH Tempo:     
       FIND Usuarios WHERE Usuarios.Agencia EQ Tempo.T_Agencia 
                       AND Usuarios.Usuario EQ Tempo.T_Usuario
                       AND Usuarios.Estado  EQ 1 NO-LOCK NO-ERROR.
       IF AVAILABLE(Usuarios) THEN
         Nom_Cajero = usuarios.Nombre.
       ELSE
         Nom_Cajero = "".
       DISPLAY Nom_Cajero                AT  1 FORMAT "X(21)"
               T_ConCheq                 AT 23 FORMAT ">>>>,>>>,>>9.99"
               T_ConEfe                  AT 39 FORMAT ">>>>,>>>,>>9.99"
               T_RetChe                  AT 55 FORMAT ">>>>,>>>,>>9.99"
               T_RetEfe                  AT 71 FORMAT ">>>>,>>>,>>9.99"
       WITH FRAME F-CAJA DOWN WIDTH 132 USE-TEXT NO-LABEL STREAM-IO NO-BOX.
     END.

     DISPLAY
             skip(1)
             "Totales           "      AT 1  NO-LABEL
             F_TotCon                  AT 35 NO-LABEL FORMAT ">>>>,>>>,>>>,>>9.99"
             F_TotRet                  AT 67 NO-LABEL FORMAT ">>>>,>>>,>>>,>>9.99" SKIP(2)
             "DIFERENCIA DE CAJA"      AT 1  NO-LABEL
             F_Cuadre                  AT 23 NO-LABEL FORMAT "->>,>>>,>>>,>>>,>>9.99"
     WITH FRAME F-CAJATot DOWN WIDTH 132 USE-TEXT STREAM-IO NO-BOX.
  END.
  PAGE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

