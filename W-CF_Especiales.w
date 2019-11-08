&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS W-Win 
CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

   {Incluido/Variable.I "SHARED"}

  DEFINE NEW SHARED VAR W_CodProE LIKE Pro_Especiales.Cod_Producto.

  DEFINE VARIABLE W_Rowid           AS ROWID.
  DEFINE VARIABLE W-Procedure       AS CHARACTER.
  DEFINE VARIABLE W_OfiDest       LIKE Agencias.Agencia.
  DEFINE VARIABLE W_Str             AS CHARACTER FORMAT "X(30)".
  DEFINE VARIABLE W_Rpta            AS LOGICAL.

  DEFINE BUFFER Tmp_ProEspecial    FOR Pro_Especiales.
  DEFINE TEMP-TABLE RegPEsp       LIKE Pro_Especiales.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BUTTON-4 Btn_Imprimir Btn_Consulta ~
Btn_Replica Btn_Ayuda RECT-265 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_p-updsav AS HANDLE NO-UNDO.
DEFINE VARIABLE h_q-cf_especiales AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-cf_especiales AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Ayuda 
     IMAGE-UP FILE "imagenes/interrogacion.bmp":U
     LABEL "A&yuda" 
     SIZE 4 BY 1.15.

DEFINE BUTTON Btn_Consulta 
     IMAGE-UP FILE "imagenes/lupa.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/lupa.bmp":U
     LABEL "&Consulta" 
     SIZE 10 BY 1.69 TOOLTIP "Busqueda de información de la pantalla en uso".

DEFINE BUTTON Btn_Imprimir 
     IMAGE-UP FILE "imagenes/impresora2.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/impresora2.bmp":U
     LABEL "I&mprimir" 
     SIZE 10 BY 1.69 TOOLTIP "Muestra interface de salida de información (Reportes)".

DEFINE BUTTON Btn_Replica 
     LABEL "&Replicar" 
     SIZE 10 BY 1.65 TOOLTIP "Duplica el producto en la oficina que se necesite"
     BGCOLOR 15 FGCOLOR 3 FONT 1.

DEFINE BUTTON BUTTON-4 
     IMAGE-UP FILE "imagenes/informacion.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/informacion.bmp":U
     LABEL "Button 4" 
     SIZE 10 BY 1.65 TOOLTIP "Muestra la pantalla de información de sesión y los mensajes del usuario activo".

DEFINE RECTANGLE RECT-265
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 12 BY 5.38.

DEFINE BUTTON Btn_Cancelar 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Salir In&forme" 
     SIZE 10 BY 1.62 TOOLTIP "Cierra esta ventana y vuelve a la pantalla anterior"
     FONT 4.

DEFINE BUTTON Btn_Imprimir-2 
     IMAGE-UP FILE "imagenes/impresora2.bmp":U
     LABEL "Im&primir" 
     SIZE 10 BY 1.62
     FONT 4.

DEFINE VARIABLE W_CodPdt AS INTEGER FORMAT "999":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 5 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_NomOfiTra AS CHARACTER FORMAT "X(40)":U 
     VIEW-AS FILL-IN 
     SIZE 28 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_NomPro AS CHARACTER FORMAT "X(40)":U 
     VIEW-AS FILL-IN 
     SIZE 28 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_OfiInf AS INTEGER FORMAT "999":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 5 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE RS_SeleOfi AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Todas", 1,
"Agencia", 2
     SIZE 16.43 BY .81 NO-UNDO.

DEFINE VARIABLE RS_SelePro AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Todos", 1,
"Producto", 2
     SIZE 16.43 BY .81
     FONT 4 NO-UNDO.

DEFINE VARIABLE R_Tipo AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "General", 1,
"Detallado", 2
     SIZE 23 BY .81
     BGCOLOR 17 FONT 5 NO-UNDO.

DEFINE BUTTON Btn_GenRep 
     IMAGE-UP FILE "imagenes/proceso_g.bmp":U
     LABEL "&Generar" 
     SIZE 10 BY 1.62 TOOLTIP "Ejecuta el proceso de replica"
     FONT 4.

DEFINE BUTTON Btn_SalirRep 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Sali&r" 
     SIZE 10 BY 1.62 TOOLTIP "Cierra esta ventana y vuelve a la pantalla anterior"
     FONT 4.

DEFINE VARIABLE W_CmbOfiDest AS CHARACTER FORMAT "X(30)":U 
     LABEL "Agencia Destino" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 22 BY 1
     BGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE W_Con AS INTEGER FORMAT ">,>>>,>>9":U INITIAL 0 
     LABEL "Generados..." 
     VIEW-AS FILL-IN 
     SIZE 21.86 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE RS_SelecOfi AS INTEGER INITIAL 2 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Solo para Agencia a Seleccionar", 1,
"Para todas las Agencias", 2
     SIZE 25 BY 2.15
     FONT 4 NO-UNDO.

DEFINE VARIABLE RS_SelecProd AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Solo el Producto Seleccionado", 1,
"Todos los Productos", 2
     SIZE 24 BY 2.42
     FONT 4 NO-UNDO.

DEFINE RECTANGLE RECT-117
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 27 BY 2.96.

DEFINE RECTANGLE RECT-118
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 27 BY 3.23.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     BUTTON-4 AT ROW 1.54 COL 102
     Btn_Imprimir AT ROW 3.15 COL 102
     Btn_Consulta AT ROW 4.77 COL 102
     Btn_Replica AT ROW 8 COL 102
     Btn_Ayuda AT ROW 20.65 COL 105
     RECT-265 AT ROW 1.27 COL 101
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 113.57 BY 21.38
         BGCOLOR 17 FONT 4.

DEFINE FRAME F_Infproesp
     R_Tipo AT ROW 1.27 COL 2 HELP
          "Seleccione el tipo de Informe" NO-LABEL
     Btn_Imprimir-2 AT ROW 1.27 COL 54 HELP
          "Permite generar la consulta de productos de Ahorro"
     RS_SeleOfi AT ROW 2.88 COL 2 NO-LABEL
     W_OfiInf AT ROW 2.88 COL 17 COLON-ALIGNED HELP
          "Ingrese el código de la Agencia" NO-LABEL
     W_NomOfiTra AT ROW 2.88 COL 22 COLON-ALIGNED NO-LABEL
     Btn_Cancelar AT ROW 3.15 COL 54 HELP
          "Permite salir de la pantalla de informes"
     RS_SelePro AT ROW 3.96 COL 2 NO-LABEL
     W_CodPdt AT ROW 3.96 COL 17 COLON-ALIGNED HELP
          "Ingrese el código del producto" NO-LABEL
     W_NomPro AT ROW 3.96 COL 22 COLON-ALIGNED NO-LABEL
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 48.14 ROW 6.69
         SIZE 63.72 BY 4.92
         BGCOLOR 17 FONT 4
         TITLE "Informe Productos Especiales de Captación".

DEFINE FRAME F_Replica
     W_Con AT ROW 1.27 COL 45 COLON-ALIGNED
     RS_SelecOfi AT ROW 2.35 COL 4 NO-LABEL
     W_CmbOfiDest AT ROW 2.35 COL 45 COLON-ALIGNED HELP
          "Seleccione la Agencia"
     Btn_GenRep AT ROW 4.5 COL 60
     RS_SelecProd AT ROW 6.12 COL 5 NO-LABEL
     Btn_SalirRep AT ROW 6.65 COL 60
     RECT-117 AT ROW 1.81 COL 3
     RECT-118 AT ROW 5.58 COL 3
     " Generar para" VIEW-AS TEXT
          SIZE 13 BY 1.04 AT ROW 1.27 COL 4
          BGCOLOR 17 FGCOLOR 7 FONT 5
     " Productos a Generar" VIEW-AS TEXT
          SIZE 19 BY 1.08 AT ROW 5.04 COL 4
          BGCOLOR 17 FGCOLOR 7 FONT 5
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 41 ROW 6.77
         SIZE 71.43 BY 8.81
         BGCOLOR 17 FONT 5
         TITLE "Genera Productos desde Agencia Central".


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
         TITLE              = "Configuración de Productos Especiales."
         HEIGHT             = 21.38
         WIDTH              = 114.14
         MAX-HEIGHT         = 21.38
         MAX-WIDTH          = 114.29
         VIRTUAL-HEIGHT     = 21.38
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

&IF '{&WINDOW-SYSTEM}' NE 'TTY' &THEN
IF NOT W-Win:LOAD-ICON("imagenes/cubos.ico":U) THEN
    MESSAGE "Unable to load icon: imagenes/cubos.ico"
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
&ENDIF
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
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE                                                          */
/* SETTINGS FOR FRAME F_Infproesp
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME F_Infproesp:HIDDEN           = TRUE
       FRAME F_Infproesp:MOVABLE          = TRUE.

/* SETTINGS FOR RADIO-SET RS_SelePro IN FRAME F_Infproesp
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_CodPdt IN FRAME F_Infproesp
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_NomOfiTra IN FRAME F_Infproesp
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_NomPro IN FRAME F_Infproesp
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_OfiInf IN FRAME F_Infproesp
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME F_Replica
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME F_Replica:HIDDEN           = TRUE
       FRAME F_Replica:MOVABLE          = TRUE.

/* SETTINGS FOR COMBO-BOX W_CmbOfiDest IN FRAME F_Replica
   NO-ENABLE                                                            */
ASSIGN 
       W_CmbOfiDest:HIDDEN IN FRAME F_Replica           = TRUE.

/* SETTINGS FOR FILL-IN W_Con IN FRAME F_Replica
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
THEN W-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON END-ERROR OF W-Win /* Configuración de Productos Especiales. */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* Configuración de Productos Especiales. */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Ayuda
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Ayuda W-Win
ON CHOOSE OF Btn_Ayuda IN FRAME F-Main /* Ayuda */
OR HELP OF {&WINDOW-NAME} DO:
   SYSTEM-HELP "\AYUDAS\AHORRO.HLP" CONTEXT 150.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Infproesp
&Scoped-define SELF-NAME Btn_Cancelar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancelar W-Win
ON CHOOSE OF Btn_Cancelar IN FRAME F_Infproesp /* Salir Informe */
DO:
     HIDE FRAME F_Infproesp.
     FRAME F-Main:SENSITIVE = TRUE.
     RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F-Main
&Scoped-define SELF-NAME Btn_Consulta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Consulta W-Win
ON CHOOSE OF Btn_Consulta IN FRAME F-Main /* Consulta */
DO:
  ASSIGN W_Rowid = ?.
  
  RUN C-Especiales.r (INPUT-OUTPUT W_Rowid).
  IF W_Rowid EQ ? THEN
     RETURN NO-APPLY.
 
  RUN get-link-handle IN adm-broker-hdl (INPUT THIS-PROCEDURE, INPUT "Query", OUTPUT W-Procedure).
  RUN Reposicion-query IN WIDGET-HANDLE(W-Procedure) (INPUT-OUTPUT W_Rowid). 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Replica
&Scoped-define SELF-NAME Btn_GenRep
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_GenRep W-Win
ON CHOOSE OF Btn_GenRep IN FRAME F_Replica /* Generar */
DO:
  ASSIGN RS_SelecProd
         RS_SelecOfi.
          
  IF RS_SelecProd EQ 1 AND RS_SelecOfi  EQ 1 THEN 
     RUN UnProUnaOfi.
  ELSE
     IF RS_SelecProd EQ 1 AND RS_SelecOfi  EQ 2 THEN 
        RUN UnProTodasOfi.
     ELSE
        IF RS_SelecProd EQ 2 AND RS_SelecOfi  EQ 1 THEN 
           RUN TodosProUnaOfi.
        ELSE
           IF RS_SelecProd EQ 2 AND RS_SelecOfi  EQ 2 THEN 
              RUN TodosProOfi.
  APPLY "ENTRY" TO Btn_SalirRep IN FRAME F_Replica.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F-Main
&Scoped-define SELF-NAME Btn_Imprimir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Imprimir W-Win
ON CHOOSE OF Btn_Imprimir IN FRAME F-Main /* Imprimir */
DO:
    FRAME F-Main:SENSITIVE = FALSE.
    VIEW FRAME F_Infproesp.
    APPLY "ENTRY":U TO R_Tipo.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Infproesp
&Scoped-define SELF-NAME Btn_Imprimir-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Imprimir-2 W-Win
ON CHOOSE OF Btn_Imprimir-2 IN FRAME F_Infproesp /* Imprimir */
DO:
    DEFINE VAR Listado AS CHAR INITIAL "L_ProEsp.LST".
    
    Listado = W_PathSpl + Listado.
    {INCLUIDO/Imprimir.I "Listado"}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F-Main
&Scoped-define SELF-NAME Btn_Replica
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Replica W-Win
ON CHOOSE OF Btn_Replica IN FRAME F-Main /* Replicar */
DO:     
  FIND Agencias WHERE Agencias.Agencia     EQ W_Agencia
                AND   Agencias.Tip_Agencia EQ "C" NO-LOCK NO-ERROR.
  IF AVAILABLE (Agencias) THEN 
     DO:
       FIND Usuarios WHERE Usuarios.Usuario     EQ W_Usuario
                     AND   Usuarios.Prioridad   GT 2
                     AND   Usuarios.Agencia     EQ W_Agencia
                     AND   Usuarios.Id_OpeOfi   EQ TRUE NO-LOCK NO-ERROR.
       IF NOT AVAILABLE (Usuarios) THEN DO:
          RUN MostrarMensaje IN W_Manija (INPUT 392, OUTPUT W_Rpta).
          RETURN NO-APPLY.
          END.
     END.
  ELSE 
     DO:    
       RUN MostrarMensaje IN W_Manija (INPUT 392, OUTPUT W_Rpta).
       RETURN NO-APPLY.
     END.

  FRAME F-Main:SENSITIVE = FALSE.
  VIEW FRAME F_Replica.
  ASSIGN W_CmbOfiDest:VISIBLE     = FALSE
         RS_SelecOfi:SCREEN-VALUE = "2".
         W_Con:SCREEN-VALUE IN FRAME F_Replica  = "0".
  APPLY "ENTRY" TO RS_SelecOfi IN FRAME F_Replica.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Replica
&Scoped-define SELF-NAME Btn_SalirRep
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_SalirRep W-Win
ON CHOOSE OF Btn_SalirRep IN FRAME F_Replica /* Salir */
DO:
  HIDE FRAME F_Replica.
  FRAME F-Main:SENSITIVE = TRUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F-Main
&Scoped-define SELF-NAME BUTTON-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-4 W-Win
ON CHOOSE OF BUTTON-4 IN FRAME F-Main /* Button 4 */
DO:
  RUN W-InfDia NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Replica
&Scoped-define SELF-NAME RS_SelecOfi
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RS_SelecOfi W-Win
ON VALUE-CHANGED OF RS_SelecOfi IN FRAME F_Replica
OR LEAVE OF RS_SelecOfi DO:
   ASSIGN RS_SelecOfi.
   IF RS_SelecOfi EQ 1 THEN DO:
      ASSIGN W_CmbOfiDest:HIDDEN IN FRAME F_Replica      = FALSE
             W_CmbOfiDest:LIST-ITEMS IN FRAME F_Replica  = "".
      ENABLE W_CmbOfiDest WITH FRAME F_Replica.
      FOR EACH Agencias WHERE Agencias.Agencia NE W_Agencia 
                        AND   Agencias.Estado  NE 3 NO-LOCK:
             ASSIGN W_Str      = STRING(Agencias.Agencia, "999") + "-" + STRING(Agencias.Nombre,"X(26)")
                    W_Eleccion = W_CmbOfiDest:ADD-LAST(W_Str) IN FRAME F_Replica
                    W_CmbOfiDest:SCREEN-VALUE IN FRAME F_Replica = W_Str.
       END.
       W_OfiDest = INTEGER(SUBSTRING(W_Str,1,3)).
   END.
   ELSE
   ASSIGN W_CmbOfiDest:HIDDEN IN FRAME F_Replica  = TRUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RS_SelecProd
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RS_SelecProd W-Win
ON VALUE-CHANGED OF RS_SelecProd IN FRAME F_Replica
DO:
  ASSIGN RS_SelecProd.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Infproesp
&Scoped-define SELF-NAME RS_SeleOfi
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RS_SeleOfi W-Win
ON VALUE-CHANGED OF RS_SeleOfi IN FRAME F_Infproesp
DO:
  ASSIGN RS_SeleOfi.
  IF RS_SeleOfi EQ 1 THEN 
            ASSIGN W_OfiInf:SCREEN-VALUE IN FRAME F_Infproesp    = "000"
                   W_NomOfiTra:SCREEN-VALUE IN FRAME F_Infproesp = ""
                   W_OfiInf:SENSITIVE                            = FALSE.    
  ELSE DO:
     W_OfiInf:SENSITIVE = TRUE.
     APPLY "ENTRY" TO W_OfiInf IN FRAME F_Infproesp.
     RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RS_SelePro
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RS_SelePro W-Win
ON VALUE-CHANGED OF RS_SelePro IN FRAME F_Infproesp
DO:
  ASSIGN RS_SelePro.
  IF RS_SelePro EQ 1 THEN
     ASSIGN W_CodPdt:SCREEN-VALUE IN FRAME F_Infproesp = STRING(0)
            W_NomPro:SCREEN-VALUE  = ""
            W_CodPdt:SENSITIVE     = FALSE.
  ELSE DO:
     ASSIGN W_CodPdt:SENSITIVE = TRUE.
     APPLY "ENTRY" TO W_OfiInf IN FRAME F_Infproesp.
     RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME R_Tipo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL R_Tipo W-Win
ON VALUE-CHANGED OF R_Tipo IN FRAME F_Infproesp
DO:
  ASSIGN R_Tipo.
  IF R_Tipo EQ 1 THEN
     ASSIGN W_CodPdt:SCREEN-VALUE IN FRAME F_Infproesp = "0000"
            W_NomPro:SCREEN-VALUE IN FRAME F_Infproesp = ""
            W_CodPdt:SENSITIVE                         = FALSE
            RS_SelePro:SENSITIVE                       = FALSE.
  ELSE DO:
     RS_SelePro:SENSITIVE = TRUE.
     IF RS_Selepro EQ 2 THEN
        W_CodPdt:SENSITIVE = TRUE.
     APPLY "ENTRY" TO RS_SeleOfi IN FRAME F_Infproesp.
     RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Replica
&Scoped-define SELF-NAME W_CmbOfiDest
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_CmbOfiDest W-Win
ON VALUE-CHANGED OF W_CmbOfiDest IN FRAME F_Replica /* Agencia Destino */
DO:
  ASSIGN W_OfiDest = INTEGER(SUBSTRING(W_CmbOfiDest:SCREEN-VALUE IN FRAME {&Frame-Name},1,3)).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Infproesp
&Scoped-define SELF-NAME W_CodPdt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_CodPdt W-Win
ON LEAVE OF W_CodPdt IN FRAME F_Infproesp
DO:
  ASSIGN W_CodPdt.

  IF W_CodPdt EQ 0
  OR W_CodPdt EQ ? THEN DO:
     ASSIGN W_CodPdt:SCREEN-VALUE = STRING(0)
            W_NomPro:SCREEN-VALUE = "".
     RETURN.
  END.
       
  RUN Busca_ProImp.
  IF NOT AVAILABLE Tmp_ProEspecial THEN DO:
     APPLY "MOUSE-SELECT-DBLCLICK":U TO SELF.
    END.
  
  IF AVAILABLE Tmp_ProEspecial THEN
     ASSIGN W_CodPdt:SCREEN-VALUE = STRING(Tmp_ProEspecial.Cod_Producto)
            W_NomPro:SCREEN-VALUE = Tmp_ProEspecial.Nom_Producto.
  ELSE
     ASSIGN W_CodPdt:SCREEN-VALUE = STRING(0)
            W_NomPro:SCREEN-VALUE = "".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_CodPdt W-Win
ON MOUSE-SELECT-DBLCLICK OF W_CodPdt IN FRAME F_Infproesp
DO:
  ASSIGN W_Rowid = ?.

  RUN b-proesp.w (INPUT-OUTPUT W_Rowid).
  IF W_Rowid EQ ? THEN
     RETURN NO-APPLY.
  
  FIND Tmp_ProEspecial WHERE ROWID(Tmp_ProEspecial) EQ W_Rowid
                       NO-LOCK NO-ERROR.

  IF AVAILABLE Tmp_ProEspecial THEN
     ASSIGN W_CodPdt:SCREEN-VALUE = STRING(Tmp_ProEspecial.Cod_Producto)
            W_NomPro:SCREEN-VALUE = Tmp_ProEspecial.Nom_Producto.
  ELSE          
     RETURN NO-APPLY.

  DISPLAY W_OfiInf W_NomPro.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_OfiInf
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_OfiInf W-Win
ON LEAVE OF W_OfiInf IN FRAME F_Infproesp
DO:
  ASSIGN W_OfiInf.
 
  IF W_OfiInf EQ 0
  OR W_OfiInf EQ ? THEN DO:
     ASSIGN W_CodPdt:SCREEN-VALUE = STRING(0)
            W_NomPro:SCREEN-VALUE = "".
     RETURN.
  END.
 
  FIND Agencias WHERE Agencias.Agencia = W_OfiInf NO-LOCK NO-ERROR.
  IF NOT AVAILABLE Agencias THEN
     APPLY "MOUSE-SELECT-DBLCLICK":U TO SELF.
  ELSE
     ASSIGN W_NomOfiTra:SCREEN-VALUE = Agencias.Nombre.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_OfiInf W-Win
ON MOUSE-SELECT-DBLCLICK OF W_OfiInf IN FRAME F_Infproesp
DO:     
   RUN P-BrwOfi.p (OUTPUT W_OfiInf, OUTPUT W_NomOfiTra).

   FIND Agencias WHERE Agencias.Agencia = W_OfiInf NO-LOCK NO-ERROR.
   IF NOT AVAILABLE(Agencias) THEN 
      ASSIGN W_OfiInf:SCREEN-VALUE    = STRING(0)
             W_NomOfiTra:SCREEN-VALUE = "".
   ELSE
      ASSIGN W_OfiInf:SCREEN-VALUE    = STRING(W_OfiInf)
             W_NomOfiTra:SCREEN-VALUE = Agencias.Nombre.
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
  DEFINE VARIABLE adm-current-page  AS INTEGER NO-UNDO.

  RUN get-attribute IN THIS-PROCEDURE ('Current-Page':U).
  ASSIGN adm-current-page = INTEGER(RETURN-VALUE).

  CASE adm-current-page: 

    WHEN 0 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'v-cf_especiales.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_v-cf_especiales ).
       /* Position in AB:  ( 3.42 , 6.00 ) */
       /* Size in UIB:  ( 18.58 , 90.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'p-updsav.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Save,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-updsav ).
       RUN set-position IN h_p-updsav ( 10.15 , 101.00 ) NO-ERROR.
       RUN set-size IN h_p-updsav ( 10.27 , 12.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'q-cf_especiales.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_q-cf_especiales ).
       RUN set-position IN h_q-cf_especiales ( 1.54 , 3.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.50 , 7.72 ) */

       /* Links to  h_v-cf_especiales. */
       RUN add-link IN adm-broker-hdl ( h_p-updsav , 'TableIO':U , h_v-cf_especiales ).
       RUN add-link IN adm-broker-hdl ( h_q-cf_especiales , 'Record':U , h_v-cf_especiales ).

       /* Links to SmartQuery h_q-cf_especiales. */
       RUN add-link IN adm-broker-hdl ( h_q-cf_especiales , 'Query':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-updsav ,
             Btn_Replica:HANDLE IN FRAME F-Main , 'AFTER':U ).
    END. /* Page 0 */

  END CASE.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Busca_ProImp W-Win 
PROCEDURE Busca_ProImp :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF Rs_Seleofi EQ 1 THEN
     FIND FIRST Tmp_ProEspecial WHERE Tmp_ProEspecial.Cod_Producto EQ W_CodPdt 
                                NO-LOCK NO-ERROR.
  ELSE
     FIND FIRST Tmp_ProEspecial WHERE Tmp_ProEspecial.Agencia      EQ W_OfiInf
                                  AND Tmp_ProEspecial.Cod_Producto EQ W_CodPdt 
                                NO-LOCK NO-ERROR.
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
  ENABLE BUTTON-4 Btn_Imprimir Btn_Consulta Btn_Replica Btn_Ayuda RECT-265 
      WITH FRAME F-Main IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  DISPLAY R_Tipo RS_SeleOfi W_OfiInf W_NomOfiTra RS_SelePro W_CodPdt W_NomPro 
      WITH FRAME F_Infproesp IN WINDOW W-Win.
  ENABLE R_Tipo Btn_Imprimir-2 RS_SeleOfi Btn_Cancelar 
      WITH FRAME F_Infproesp IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-F_Infproesp}
  DISPLAY W_Con RS_SelecOfi W_CmbOfiDest RS_SelecProd 
      WITH FRAME F_Replica IN WINDOW W-Win.
  ENABLE RS_SelecOfi Btn_GenRep RS_SelecProd Btn_SalirRep RECT-117 RECT-118 
      WITH FRAME F_Replica IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-F_Replica}
  VIEW W-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Estado_Btn W-Win 
PROCEDURE Estado_Btn :
/*------------------------------------------------------------------------------
  Purpose: Habilita o Deshabilita los Botones de Replicar, Consultar e Imprimir
           Si se esta Ingresando un Nuevo Registro.
  Parameters: Variable Tipo Lógico.
  Notes:       
------------------------------------------------------------------------------*/

  DEFINE INPUT PARAMETER P_Estado AS LOGICAL.

  ASSIGN Btn_Replica:SENSITIVE IN FRAME F-MAIN = P_Estado
         Btn_Consulta:SENSITIVE                = P_Estado
         Btn_Imprimir:SENSITIVE                = P_Estado.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ImpDetalle W-Win 
PROCEDURE ImpDetalle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VAR W_NomTipo         AS CHARACTER FORMAT "X(11)".
    DEFINE VAR W_Estado          AS CHARACTER FORMAT "X(10)".

    ASSIGN W_Estado = "Activo".
    IF Pro_Especiales.Estado EQ 2 THEN
        ASSIGN W_Estado = "Inactivo".
        DISPLAY 
            Pro_Especiales.Agencia              AT 1 
            Pro_Especiales.Cod_Producto         AT 15
            Pro_Especiales.Nom_Producto         AT 24 FORMAT "X(35)"
            W_Estado                            AT 63 SKIP(0)
            WITH WIDTH 135 FRAME F-reportes USE-TEXT NO-BOX NO-LABELS STREAM-IO.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Imprimir_Excel W-Win 
PROCEDURE Imprimir_Excel :
{Incluido\Def_Excel.i}
 /* MANDA ENCABEZADO DE HOJA DE CALCULO*/
 DEFINE VAR W_EstadoInf AS CHARACTER FORMAT "X(8)". 
 E_NumFila = 1.
 E_NumColumn = 5.
 E_Fila     = "003" + "Agencia"
            + "003" + "Cod"
            + "003" + "Nom_Producto                            " 
            + "008" + "Estado  "
            + "010" + "TipCliente".
    
 RUN W-GraExcel.w (INPUT E_Fila, INPUT E_NumColumn, OUTPUT E_CmpGrafic).

/* launch Excel so it is visible to the user */
chExcelApp:Visible = TRUE.

/* create a new Workbook */
chWorkbook = chExcelApp:Workbooks:Add().

/* get the active Worksheet */
chWorkSheet = chExcelApp:Sheets:Item(1).
DEFINE VAR TCli AS CHARACTER FORMAT "X(10)".
DEFINE VAR Est  AS CHARACTER FORMAT "X(8)".
    FOR EACH Pro_Especiales NO-LOCK BY Pro_Especiales.Agencia BY Pro_Especiales.Cod_Producto:
        CASE Pro_Especiales.Pro_Asociado:
           WHEN 1 THEN TCli = "Asociado".
           WHEN 2 THEN TCli = "NoAsociado".
           WHEN 3 THEN TCli = "Todos".
        END CASE. 
        CASE Pro_Especiales.Estado:
            WHEN 1 THEN
                    Est = "Activo".
            WHEN 2 THEN
                    Est = "Inactivo".
        END CASE.
      E_Fila2     = "".
      E_Fila2     = "003" + STRING(Pro_Especiales.Agencia,"999")
                  + "003" + STRING(Pro_Especiales.Cod_Producto,"999")
                  + "040" + STRING(Pro_Especiales.Nom_Producto,"X(40)")
                  + "008" + STRING(Est,"X(8)")
                  + "010" + STRING(TCli,"X(10)").
                  
      {Incluido\imprimir_Excel.i}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InfDetal1 W-Win 
PROCEDURE InfDetal1 :
/*------------------------------------------------------------------------------
    IMPRIMIR DETALLE DE PRODUCTOS ESPECIALES DE CAPTACION
------------------------------------------------------------------------------*/
    DEFINE VAR W_TxtCtaCble         AS CHARACTER FORMAT "X(14)".    
    DEFINE VAR W_Estado             AS CHARACTER FORMAT "X(6)".
    DEFINE VAR W_TxtCliente         AS CHARACTER FORMAT "X(5)". 
    DEFINE VAR W_TxtCuoFijaVar      AS CHARACTER FORMAT "X(4)".
    DEFINE VAR W_ContPag            AS INTEGER INITIAL 0.

    ASSIGN W_Estado = "Activo".
    IF Pro_Especiales.Estado EQ 2 THEN
        ASSIGN W_Estado = "Inact.".

    IF Pro_Especiales.Cta_Cargos NE "" THEN
        W_TxtCtaCble = Pro_Especiales.Cta_Cargos.
    ELSE
        W_TxtCtaCble = Pro_Especiales.Cta_Recaudos.         
        
    CASE Pro_Especiales.Pro_Asociado:
        WHEN 1 THEN W_TxtCliente = "Asoc.".
        WHEN 2 THEN W_TxtCliente = "No A.".
        WHEN 3 THEN W_TxtCliente = "Todos".
    END CASE.
    
    IF Pro_Especiales.Cuo_FijaVar THEN W_TxtCuoFijaVar  = "Fija".
    ELSE                               W_TxtCuoFijaVar  = "Var.".

    DISPLAY
        Pro_Especiales.Agencia                  AT 1 
        Pro_Especiales.Cod_Producto             AT 5 
        Pro_Especiales.Nom_Producto             AT 9 FORMAT "X(20)"
        W_Estado                                AT 31 
        W_TxtCliente                            AT 38
        Pro_Especiales.Fec_Creacion             AT 44 FORMAT "99/99/9999"
        Pro_Especiales.Prioridad                AT 55
        Pro_Especiales.Id_CtrSaldo              AT 59 FORMAT "SI/NO"
        Pro_Especiales.Id_Contabiliza           AT 62 FORMAT "SI/NO"
        W_TxtCuoFijaVar                         AT 68
        Pro_Especiales.Cta_Cargos               AT 73 FORMAT "X(14)"
        Pro_Especiales.Cta_Recaudos             AT 88 FORMAT "X(14)"
        Pro_Especiales.Plazo_Minimo             AT 103 FORMAT "99999"
        Pro_Especiales.Plazo_Maximo             AT 109 FORMAT "99999"
        Pro_Especiales.Ran_IniCuota             AT 115 FORMAT "99,999,999"
        Pro_Especiales.Ran_FinCuota             AT 126 FORMAT "99,999,999"
        WITH WIDTH 135 FRAME F-Reportes1 USE-TEXT STREAM-IO NO-BOX NO-LABELS.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InfGenerales W-Win 
PROCEDURE InfGenerales :
/*------------------------------------------------------------------------------
    PROCEDIMIENTO PARA INFORME GENERAL DE PRODUCTOS ESPECIALES DE CAPTACION  
    FEBRERO 11 DE 1999.
------------------------------------------------------------------------------*/
    IF RS_SeleOfi EQ 1 THEN DO:         /*General Solo Tipo de Producto Seleccionado*/
        FOR EACH Pro_Especiales FIELDS (Agencia Cod_Producto Nom_Producto Estado)
                        NO-LOCK BY Pro_Especiales.Agencia
                        BY Pro_Especiales.Cod_Producto:
                        RUN ImpDetalle.
        END.
    END.
    ELSE                               /*General de una Agencia todos los Productos*/
        FOR EACH Pro_Especiales FIELDS (Agencia Cod_Producto Nom_Producto Estado) 
                             WHERE Pro_Especiales.Agencia  EQ W_OfiInf
                             NO-LOCK BY Pro_Especiales.Agencia
                             BY Pro_Especiales.Cod_Producto:
                             RUN ImpDetalle.
    END.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcesoImprimir W-Win 
PROCEDURE ProcesoImprimir :
/*------------------------------------------------------------------------------
    PROCEDIMIENTO PARA IMPRIMIR PRODUCTOS ESPECIALES DE CAPTACION  
    FEBRERO 11 DE 1999.
------------------------------------------------------------------------------*/
DEFINE VAR W_Linea1          AS CHARACTER FORMAT "X(133)".
DEFINE VAR W_Linea           AS CHARACTER FORMAT "X(80)".
DEFINE VAR procname          AS CHARACTER FORMAT "X(59)" INITIAL "IProEsp.txt".
DEFINE VAR W_Raya            AS CHARACTER FORMAT "X" INITIAL "-".
DEFINE VAR Mensaje           AS CHARACTER INITIAL "".  
DEFINE VAR Op_Salida         AS CHARACTER INITIAL "".
    
ASSIGN FRAME F_Infproesp W_OfiInf W_CodPdt.

DEFINE FRAME F-EncabezadoGral
       HEADER
       W_Nom_Entidad                                   AT 18  
       "PAGINA:"                                       AT 69 PAGE-NUMBER FORMAT ">>>9" SKIP(1)
       "INFORME DE PRODUCTOS ESPECIALES DE CAPTACION"  AT 19  
       W_Linea                                         AT 1   
       "Agencia      Código    Nombre                                 Estado"  AT 1
       "=======      ======    ======                                 ======"  AT 1 SKIP(1)
       WITH WIDTH 135 USE-UNDERLINE USE-TEXT PAGE-TOP FRAME F-EncabezadoGral STREAM-IO.

DEFINE FRAME F-EncabezadoDeta
       HEADER
       W_Nom_Entidad                                   AT 49  
       "PAGINA:"                                       AT 120 PAGE-NUMBER FORMAT ">>>9" SKIP(1)
       "INFORME DE PRODUCTOS ESPECIALES DE CAPTACION"  AT 50 SKIP(1)  
       "Of. Cód.Nombre                Estado TipoC Fec.Creado Pr. Cs Cb    T.C. Cuenta Cargos  Cuenta Recaud. P.Mi. P.Mx. Cuota Mín. Cuota Máx." AT 1 SKIP(0)
       "=======================================================================================================================================" AT 1 SKIP(0)
       WITH WIDTH 135 USE-UNDERLINE USE-TEXT PAGE-TOP FRAME F-EncabezadoDeta STREAM-IO NO-BOX.

DEFINE FRAME F-PiePagina
       HEADER 
       "FECHA:"                                        AT 2 
       TODAY                                           AT 10 FORMAT "99/99/9999"
       W_Nom_Agencia                                   AT 25 FORMAT "X(40)"
       "HORA:"                                         AT 67 STRING(TIME,"HH:MM:SS")
       WITH DOWN WIDTH 135 USE-TEXT PAGE-BOTTOM FRAME F-PiePagina STREAM-IO NO-BOX.            

DEFINE FRAME F-PiePagina1
       HEADER 
       "FECHA:"                                        AT 2 
       TODAY                                           AT 10 FORMAT "99/99/9999"
       W_Nom_Agencia                                   AT 50 FORMAT "X(40)"
       "HORA:"                                         AT 120 STRING(TIME,"HH:MM:SS")
       WITH DOWN WIDTH 135 USE-TEXT PAGE-BOTTOM FRAME F-PiePagina1 STREAM-IO NO-BOX. 

ASSIGN FRAME F_Infproesp R_Tipo
       W_Linea  = FILL(W_Raya,80)
       W_Linea1 = FILL(W_Raya,135).
      
IF R_Tipo EQ 1 THEN DO:
   VIEW FRAME F-EncabezadoGral.
   VIEW FRAME F-PiePagina.        
   RUN InfGenerales.
   PAGE.
   RETURN.
END.

VIEW FRAME F-EncabezadoDeta.  
VIEW FRAME F-PiePagina1.

IF RS_SeleOfi EQ 1              /*Detallado todos los Productos,todas las Agencias*/
   AND RS_SelePro EQ 1 THEN 
   FOR EACH Pro_Especiales NO-LOCK 
            BY Pro_Especiales.Agencia
            BY Pro_Especiales.Cod_Producto:
       RUN InfDetal1.
   END.
ELSE
    IF RS_SeleOfi EQ 1          /*Detallado un Producto,todas las Agencias*/
       AND RS_SelePro EQ 2 THEN 
       FOR EACH Pro_Especiales WHERE Pro_Especiales.Cod_producto EQ W_CodPdt
                               NO-LOCK BY Pro_Especiales.Agencia
                                       BY Pro_Especiales.Cod_Producto:
           RUN InfDetal1.
       END.
    ELSE
        IF RS_SeleOfi EQ 2      /*Detallado todos los Productos,una Agencia*/
           AND RS_SelePro EQ 1 THEN  
           FOR EACH Pro_Especiales WHERE Pro_Especiales.Agencia EQ W_OfiInf
                                   NO-LOCK BY Pro_Especiales.Agencia
                                           BY Pro_Especiales.Cod_Producto:
               RUN InfDetal1.
           END.
        ELSE
            FOR EACH Pro_Especiales WHERE Pro_Especiales.Agencia EQ W_OfiInf 
                                    AND Pro_Especiales.Cod_Producto EQ W_CodPdt NO-LOCK
                                    BY Pro_Especiales.Agencia
                                    BY Pro_Especiales.Cod_Producto:
                RUN InfDetal1.
        END.

DISPLAY SKIP(2)
        W_Linea1                                            AT 1
        "CONVENCIONES: Of.:Agencia      TipoC:Tipo Cliente          Pr.:Prioridad                Cs:Controla Saldo  Cb:Contabiliza"   AT 1 SKIP(0)
        "              T.C.:Tipo Cuota  P.Ma.:Plazo Mínimo(En días) P.Mx.:Plazo Máximo(En días)" AT 1 SKIP(0)
        W_Linea1                                            AT 1 SKIP(0)
        WITH WIDTH 135 FRAME F-Resumen USE-TEXT STREAM-IO NO-BOX NO-LABELS.

PAGE.
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

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this SmartWindow, and there are no
     tables specified in any contained Browse, Query, or Frame. */

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE TodosProOfi W-Win 
PROCEDURE TodosProOfi :
/*------------------------------------------------------------------------------
  Purpose:  Genera desde Of.Central todos los Productos para las demás
            Agencias.
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE BUFFER Tmp_ProEsp  FOR Pro_Especiales.
  DEFINE BUFFER Tmp2_ProEsp FOR Pro_Especiales.
  ASSIGN W_Con   = 0
         W_Rowid = ROWID(Pro_Especiales).
  DISPLAY W_Con WITH FRAME F_Replica.

  FOR EACH RegPEsp: 
      DELETE RegPEsp. 
  END.
 
  FOR EACH Tmp2_ProEsp WHERE Tmp2_ProEsp.Agencia EQ W_Agencia
                         AND Tmp2_ProEsp.Estado  EQ 1 NO-LOCK:
      CREATE RegPEsp.
      BUFFER-COPY Tmp2_ProEsp TO RegPEsp.      
      ASSIGN RegPEsp.Fec_Creacion = TODAY.

      FOR EACH Agencias FIELDS(Agencia.Agencia)
                        WHERE Agencias.Agencia NE W_Agencia
                          AND Agencias.Estado  NE 3 NO-LOCK:
          ASSIGN RegPEsp.Agencia = Agencias.Agencia.
          FIND Tmp_ProEsp WHERE Tmp_ProEsp.Agencia      EQ RegPEsp.Agencia
                            AND Tmp_ProEsp.Cod_Producto EQ RegPEsp.Cod_Producto NO-LOCK NO-ERROR.
          IF NOT AVAILABLE(Tmp_ProEsp) THEN 
            DO:
              CREATE Tmp_ProEsp.
              BUFFER-COPY RegPEsp TO Tmp_ProEsp.
              ASSIGN W_Con = W_Con + 1.
              DISPLAY W_Con WITH FRAME F_Replica.
            END.
      END.
      DELETE RegPEsp. 
  END.
  FIND Pro_Especiales WHERE ROWID(Pro_Especiales) EQ W_Rowid NO-LOCK NO-ERROR.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE TodosProUnaOfi W-Win 
PROCEDURE TodosProUnaOfi :
/*------------------------------------------------------------------------------
  Purpose:  Genera desde Of.Central todos los Productos para la Agencia
            Seleccionada.
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE BUFFER Tmp_ProEsp FOR Pro_Especiales.
    
  ASSIGN W_Con   = 0
         W_Rowid = ROWID(Pro_Especiales).
  DISPLAY W_Con WITH FRAME F_Replica.

  FOR EACH RegPEsp: 
      DELETE RegPEsp. 
  END.
 
  FOR EACH Tmp_ProEsp WHERE Tmp_ProEsp.Agencia EQ W_Agencia
                       AND  Tmp_ProEsp.Estado  EQ 1 NO-LOCK:
      CREATE RegPEsp.
      BUFFER-COPY Tmp_ProEsp TO RegPEsp.      
      ASSIGN RegPEsp.Agencia      = W_OfiDest
             RegPEsp.Fec_Creacion = TODAY. 
      FIND Pro_Especiales WHERE Pro_Especiales.Agencia      EQ W_OfiDest
                            AND Pro_Especiales.Cod_Producto EQ RegPEsp.Cod_Producto NO-LOCK NO-ERROR.
      IF NOT AVAILABLE(Pro_Especiales) THEN 
        DO:
          CREATE Pro_Especiales.
          BUFFER-COPY RegPEsp TO Pro_Especiales.           
          W_Con = W_Con + 1.
          DISPLAY W_Con WITH FRAME F_Replica.
        END.  
      DELETE RegPEsp.
  END.
  FIND Pro_Especiales WHERE ROWID(Pro_Especiales) EQ W_Rowid NO-LOCK NO-ERROR. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE UnProTodasOfi W-Win 
PROCEDURE UnProTodasOfi :
/*------------------------------------------------------------------------------
  Purpose:  Genera desde Of.Central El Producto Seleccionado para las demás
            Agencias.
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  ASSIGN W_Con   = 0
         W_Rowid = ROWID(Pro_Especiales).
  DISPLAY W_Con WITH FRAME F_Replica.

  FOR EACH RegPEsp: 
      DELETE RegPEsp. 
  END.
   
  FIND Pro_Especiales WHERE Pro_Especiales.Cod_Producto EQ W_CodProE
                        AND Pro_Especiales.Estado       EQ 1
                        AND Pro_Especiales.Agencia      EQ W_Agencia NO-LOCK NO-ERROR.
  IF AVAILABLE (Pro_Especiales) THEN 
    DO:
      BUFFER-COPY Pro_Especiales TO RegPEsp.
      FOR EACH Agencias FIELDS (Agencia) WHERE Agencias.Agencia NE W_Agencia
                                           AND Agencias.Estado  NE 3 NO-LOCK:
          FIND Pro_Especiales WHERE Pro_Especiales.Agencia      EQ Agencias.Agencia
                                AND Pro_Especiales.Cod_Producto EQ W_CodProE NO-LOCK NO-ERROR.
          IF NOT AVAILABLE(Pro_Especiales) THEN 
            DO:
              ASSIGN RegPEsp.Agencia      = Agencias.Agencia
                     RegPEsp.Fec_Creacion = TODAY. 
              CREATE Pro_Especiales.
              BUFFER-COPY RegPEsp TO Pro_Especiales.           
              W_Con = W_Con + 1.
              DISPLAY W_Con WITH FRAME F_Replica.         
            END.
      END.
      DELETE RegPEsp.
    END.
  FIND Pro_Especiales WHERE ROWID(Pro_Especiales) EQ W_Rowid NO-LOCK NO-ERROR.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE UnProUnaOfi W-Win 
PROCEDURE UnProUnaOfi :
/*------------------------------------------------------------------------------
  Purpose: Genera desde Of.Central El Producto Seleccionado para la Agencia 
           Seleccionada.
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  ASSIGN W_Con   = 0
         W_Rowid = ROWID(Pro_Especiales).
  DISPLAY W_Con WITH FRAME F_Replica.

  FOR EACH RegPEsp: 
      DELETE RegPEsp. 
  END.

  FIND Pro_Especiales WHERE Pro_Especiales.Cod_Producto EQ W_CodProE
                      AND   Pro_Especiales.Estado       EQ 1
                      AND   Pro_Especiales.Agencia      EQ W_Agencia NO-LOCK NO-ERROR.
  IF AVAILABLE (Pro_Especiales) THEN 
    DO:
      BUFFER-COPY Pro_Especiales TO RegPEsp.
      FIND Pro_Especiales WHERE Pro_Especiales.Agencia      EQ W_OfiDest
                            AND Pro_Especiales.Cod_Producto EQ W_CodProE NO-LOCK NO-ERROR.
      IF NOT AVAILABLE(Pro_Especiales) THEN 
        DO:
          ASSIGN RegPEsp.Agencia         = W_OfiDest
                 RegPEsp.Fec_Creacion    = TODAY.
          CREATE Pro_Especiales.
          BUFFER-COPY RegPEsp TO Pro_Especiales.           
          DELETE RegPEsp.
          W_Con = W_Con + 1.
          DISPLAY W_Con WITH FRAME F_Replica.     
        END. 
    END.
  FIND Pro_Especiales WHERE ROWID(Pro_Especiales) EQ W_Rowid NO-LOCK NO-ERROR.  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

