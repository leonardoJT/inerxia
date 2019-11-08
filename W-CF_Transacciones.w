&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS W-Win 
CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

 {incluido/variable.i "SHARED"}

  DEFINE NEW SHARED VARIABLE W_CodOperacion LIKE Operacion.Cod_Operacion.
  DEFINE VAR CUsu AS INTEGER FORMAT ">>>9".
  DEFINE VAR T_Pto AS CHARACTER FORMAT "X(10)".
  DEFINE VAR C_Ope AS CHARACTER FORMAT "X(10)".
  DEFINE VAR T_Ope AS CHARACTER FORMAT "X(10)".
  DEFINE VAR T_Val AS CHARACTER FORMAT "X(10)".
  DEFINE VAR T_EC  AS CHARACTER FORMAT "X(10)".
  
  DEFINE VARIABLE W_Car AS CHAR FORMAT "X(15)".
  DEFINE VARIABLE W_Cla AS CHAR FORMAT "X(15)".
  DEFINE VAR Listado AS CHARACTER INITIAL "l_manop.lst".
  
  ASSIGN Listado = W_PathSpl + Listado.
  DEFINE VAR i AS DECIMAL.
  DEFINE VAR W_Ok AS LOGICAL.

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
&Scoped-Define ENABLED-OBJECTS BUTTON-52 Btn_Imprimir Btn_Consulta ~
Btn_Restricciones Btn_Ayuda RECT-225 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-2 F-Main 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_b-cf_transacciones AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-updsav AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-cf_transacciones AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Ayuda 
     IMAGE-UP FILE "imagenes/interrogacion.bmp":U
     LABEL "Button 5" 
     SIZE 4 BY 1.08 TOOLTIP "Ayuda".

DEFINE BUTTON Btn_Consulta 
     IMAGE-UP FILE "imagenes/lupa.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/lupa.bmp":U
     LABEL "C&onsulta" 
     SIZE 10 BY 1.62 TOOLTIP "Busqueda de información de la pantalla en uso".

DEFINE BUTTON Btn_Imprimir 
     IMAGE-UP FILE "imagenes/impresora2.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/impresora2.bmp":U
     LABEL "&Imprimir" 
     SIZE 10 BY 1.62 TOOLTIP "Muestra la interface de filtros a elegir para el resultado del reporte".

DEFINE BUTTON Btn_Restricciones 
     LABEL "Restricciones" 
     SIZE 12 BY 1.92.

DEFINE BUTTON BUTTON-52 
     IMAGE-UP FILE "imagenes/informacion.bmp":U
     LABEL "Button 52" 
     SIZE 10 BY 1.65 TOOLTIP "Muestra la pantalla de información de sesión y los mensajes del usuario activo".

DEFINE RECTANGLE RECT-225
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 12 BY 5.38.

DEFINE BUTTON F_SalirConsulta 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Salir Consulta" 
     SIZE 8 BY 1.65 TOOLTIP "Cierra esta ventana y vuelve a la pantalla en uso".

DEFINE BUTTON Btn_Aceptar 
     IMAGE-UP FILE "imagenes/impresora2.bmp":U
     LABEL "&Aceptar" 
     SIZE 8 BY 1.62 TOOLTIP "Muestra la interface de salida de información (Reportes)"
     FONT 4.

DEFINE BUTTON Btn_Cancelar 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "&Cancelar" 
     SIZE 8 BY 1.62 TOOLTIP "Cierra esta ventana y vuelve a la panatalla activa"
     FONT 4.

DEFINE VARIABLE W_TCampo AS CHARACTER FORMAT "X(20)":U INITIAL "Por Codigo" 
     LABEL "Selección" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Por Codigo","Por Producto","Por Cuenta","Activas","Inactivas" 
     DROP-DOWN-LIST
     SIZE 21 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE F_CtaFin AS CHARACTER FORMAT "X(14)":U 
     LABEL "Cta.Final" 
     VIEW-AS FILL-IN 
     SIZE 21 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE F_CtaIni AS CHARACTER FORMAT "X(14)":U 
     LABEL "Cta.Inicial" 
     VIEW-AS FILL-IN 
     SIZE 21 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_RanFin AS INTEGER FORMAT ">>>>>>>>9":U INITIAL 0 
     LABEL "Rango Final" 
     VIEW-AS FILL-IN 
     SIZE 21 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_RanIni AS INTEGER FORMAT ">>>>>>>>9":U INITIAL 0 
     LABEL "Rango Inicial" 
     VIEW-AS FILL-IN 
     SIZE 21 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_RTipo AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "General", 1,
"Detallado", 2
     SIZE 24 BY .81
     BGCOLOR 17  NO-UNDO.

DEFINE VARIABLE W_RTipPro AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Ahorro", 1,
"Crédito", 2,
"Cuentas", 3
     SIZE 32 BY .81
     BGCOLOR 17  NO-UNDO.

DEFINE BUTTON Btn_QuitarRes 
     LABEL "Quitar Restricción" 
     SIZE 22 BY .81
     FONT 5.

DEFINE BUTTON BUTTON-54 
     LABEL "Restringir" 
     SIZE 22 BY .81
     FONT 5.

DEFINE BUTTON BUTTON-57 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Button 57" 
     SIZE 7 BY 1.38.

DEFINE VARIABLE R_Eleccion AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Agencia", 1,
"Grupo", 2,
"Usuario", 3
     SIZE 46 BY .81
     FONT 5 NO-UNDO.

DEFINE VARIABLE ConRestriccion AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 43 BY 12.12
     BGCOLOR 15 FGCOLOR 12 FONT 5 NO-UNDO.

DEFINE VARIABLE SinRestriccion AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 43 BY 12.12
     BGCOLOR 15 FONT 5 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F_Imprimir
     W_RTipo AT ROW 1.27 COL 4 HELP
          "Seleccione el tipo de consulta" NO-LABEL
     W_TCampo AT ROW 2.62 COL 12 COLON-ALIGNED
     Btn_Aceptar AT ROW 3.69 COL 37 HELP
          "Genera la impresion de Operaciones"
     W_RTipPro AT ROW 3.96 COL 4 HELP
          "Seleccione el tipo de Producto" NO-LABEL
     F_CtaIni AT ROW 5.58 COL 12 COLON-ALIGNED
     W_RanIni AT ROW 5.58 COL 12 COLON-ALIGNED HELP
          "Ingrese el Rango Inicial para la Impresión"
     Btn_Cancelar AT ROW 5.85 COL 37 HELP
          "Regresa a la pantalla de operaciones"
     F_CtaFin AT ROW 6.65 COL 12 COLON-ALIGNED
     W_RanFin AT ROW 6.65 COL 12 COLON-ALIGNED HELP
          "Ingrese el Rango Final para la Impresión"
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 66.86 ROW 6.65
         SIZE 45.14 BY 7.73
         BGCOLOR 17 FONT 5
         TITLE "Imprimir Operaciones".

DEFINE FRAME Frame_Consulta
     F_SalirConsulta AT ROW 13.12 COL 74
     "Escoja el filtro por el cual desea realizar la busqueda," VIEW-AS TEXT
          SIZE 36 BY .81 AT ROW 13.38 COL 8
          FONT 4
     "digite la palabra clave a buscar, luego presione ENTER." VIEW-AS TEXT
          SIZE 38 BY .62 AT ROW 14.19 COL 9
          FONT 4
     "Nota:" VIEW-AS TEXT
          SIZE 6 BY .81 AT ROW 13.38 COL 3
          FGCOLOR 7 FONT 5
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 29.43 ROW 5.04
         SIZE 83.29 BY 14.88
         BGCOLOR 17 FONT 4
         TITLE "Consulta de Operaciones".

DEFINE FRAME F-Main
     BUTTON-52 AT ROW 1.81 COL 102
     Btn_Imprimir AT ROW 3.42 COL 102 HELP
          "Permite Generar la impresión de Operaciones"
     Btn_Consulta AT ROW 5.04 COL 102 HELP
          "Permite Generar la consulta de Operaciones"
     Btn_Restricciones AT ROW 7.46 COL 101
     Btn_Ayuda AT ROW 20.65 COL 105 HELP
          "Permite Obtener la ayuda de la pantalla"
     RECT-225 AT ROW 1.54 COL 101
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 114 BY 21.38
         BGCOLOR 17 FONT 4.

DEFINE FRAME F_Restricciones
     R_Eleccion AT ROW 1.27 COL 28 NO-LABEL
     BUTTON-54 AT ROW 2.88 COL 25
     Btn_QuitarRes AT ROW 2.88 COL 70
     SinRestriccion AT ROW 3.96 COL 4 NO-LABEL
     ConRestriccion AT ROW 3.96 COL 50 NO-LABEL
     BUTTON-57 AT ROW 16.35 COL 85
     "Restringidos" VIEW-AS TEXT
          SIZE 13 BY .77 AT ROW 2.88 COL 50
          FGCOLOR 7 FONT 5
     "Por Restringir" VIEW-AS TEXT
          SIZE 13 BY .81 AT ROW 2.88 COL 4
          FGCOLOR 7 FONT 5
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 4 ROW 4.5
         SIZE 95 BY 17.77
         BGCOLOR 17 FONT 4
         TITLE "Restricción de Operaciones".


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
         TITLE              = "Configuración de Transacciones"
         HEIGHT             = 21.38
         WIDTH              = 114.29
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
/* REPARENT FRAME */
ASSIGN FRAME F_Restricciones:FRAME = FRAME F-Main:HANDLE.

/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE 2                                                        */
/* SETTINGS FOR FRAME Frame_Consulta
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME Frame_Consulta:HIDDEN           = TRUE
       FRAME Frame_Consulta:MOVABLE          = TRUE.

/* SETTINGS FOR FRAME F_Imprimir
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME F_Imprimir:HIDDEN           = TRUE
       FRAME F_Imprimir:MOVABLE          = TRUE.

/* SETTINGS FOR FILL-IN F_CtaFin IN FRAME F_Imprimir
   NO-ENABLE                                                            */
ASSIGN 
       F_CtaFin:HIDDEN IN FRAME F_Imprimir           = TRUE.

/* SETTINGS FOR FILL-IN F_CtaIni IN FRAME F_Imprimir
   NO-ENABLE                                                            */
ASSIGN 
       F_CtaIni:HIDDEN IN FRAME F_Imprimir           = TRUE.

/* SETTINGS FOR FILL-IN W_RanFin IN FRAME F_Imprimir
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_RanIni IN FRAME F_Imprimir
   NO-ENABLE                                                            */
/* SETTINGS FOR RADIO-SET W_RTipPro IN FRAME F_Imprimir
   NO-ENABLE                                                            */
/* SETTINGS FOR COMBO-BOX W_TCampo IN FRAME F_Imprimir
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME F_Restricciones
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME F_Restricciones:HIDDEN           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
THEN W-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON END-ERROR OF W-Win /* Configuración de Transacciones */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* Configuración de Transacciones */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Imprimir
&Scoped-define SELF-NAME Btn_Aceptar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Aceptar W-Win
ON CHOOSE OF Btn_Aceptar IN FRAME F_Imprimir /* Aceptar */
DO:
  {incluido/Imprimir.I "Listado"}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F-Main
&Scoped-define SELF-NAME Btn_Ayuda
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Ayuda W-Win
ON CHOOSE OF Btn_Ayuda IN FRAME F-Main /* Button 5 */
OR HELP OF {&WINDOW-NAME} DO:
  SYSTEM-HELP "ayudas/tesoreri" CONTEXT 18.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Imprimir
&Scoped-define SELF-NAME Btn_Cancelar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancelar W-Win
ON CHOOSE OF Btn_Cancelar IN FRAME F_Imprimir /* Cancelar */
DO:
 DO WITH FRAME {&FRAME-NAME}:
  FRAME F-Main:SENSITIVE = TRUE.
  HIDE FRAME F_Imprimir.
 END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F-Main
&Scoped-define SELF-NAME Btn_Consulta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Consulta W-Win
ON CHOOSE OF Btn_Consulta IN FRAME F-Main /* Consulta */
DO:
  VIEW FRAME Frame_Consulta.
  FRAME F-Main:SENSITIVE = FALSE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Imprimir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Imprimir W-Win
ON CHOOSE OF Btn_Imprimir IN FRAME F-Main /* Imprimir */
DO:
 DO WITH FRAME {&FRAME-NAME}:
  VIEW FRAME F_Imprimir.
  FRAME F-Main:SENSITIVE = FALSE.
 END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Restricciones
&Scoped-define SELF-NAME Btn_QuitarRes
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_QuitarRes W-Win
ON CHOOSE OF Btn_QuitarRes IN FRAME F_Restricciones /* Quitar Restricción */
DO:
   IF ConRestriccion:SCREEN-VALUE NE "" THEN DO:
      CASE R_Eleccion:SCREEN-VALUE:
      WHEN "1" THEN DO:
           FIND Res_Operacion WHERE Res_Operacion.Cod_Operacion EQ W_CodOperacion AND
                                    Res_Operacion.Restriccion   EQ INTEGER(R_Eleccion:SCREEN-VALUE) AND
                                    Res_Operacion.Agencia       EQ INTEGER(SUBSTRING(ConRestriccion:SCREEN-VALUE,1,3)) NO-ERROR.
           IF AVAILABLE Res_Operacion THEN DELETE Res_Operacion.
           APPLY "value-changed" TO R_Eleccion.
      END.
      WHEN "2" THEN DO:
           FIND Res_Operacion WHERE Res_Operacion.Cod_Operacion EQ W_CodOperacion AND
                                    Res_Operacion.Restriccion   EQ INTEGER(R_Eleccion:SCREEN-VALUE) AND
                                    Res_Operacion.Grupo         EQ INTEGER(SUBSTRING(ConRestriccion:SCREEN-VALUE,1,2)) NO-ERROR.
           IF AVAILABLE Res_Operacion THEN DELETE Res_Operacion.
           APPLY "value-changed" TO R_Eleccion.
      END.
      WHEN "3" THEN DO:
           Cusu = INTEGER(SUBSTRING(ConRestriccion:SCREEN-VALUE,1,4)).
           FIND Res_Operacion WHERE Res_Operacion.Cod_Operacion EQ W_CodOperacion AND
                                    Res_Operacion.Restriccion   EQ INTEGER(R_Eleccion:SCREEN-VALUE) AND
                                    Res_Operacion.Usuario       EQ STRING(Cusu) NO-ERROR.
           IF AVAILABLE Res_Operacion THEN DELETE Res_Operacion.
           APPLY "value-changed" TO R_Eleccion.
      END.
      END CASE.
   END.
   ELSE DO:
       MESSAGE "Para Quitar una Restricción se debe seleccionar" SKIP
               "algún campo de la lista. Rectifique" VIEW-AS ALERT-BOX INFORMATION.
   END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F-Main
&Scoped-define SELF-NAME Btn_Restricciones
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Restricciones W-Win
ON CHOOSE OF Btn_Restricciones IN FRAME F-Main /* Restricciones */
DO:
  DO WITH FRAME F_Restricciones:
     APPLY "value-changed" TO R_Eleccion IN FRAME F_Restricciones.
  END.
  VIEW FRAME F_Restricciones.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-52
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-52 W-Win
ON CHOOSE OF BUTTON-52 IN FRAME F-Main /* Button 52 */
DO:
  RUN W-InfDia NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Restricciones
&Scoped-define SELF-NAME BUTTON-54
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-54 W-Win
ON CHOOSE OF BUTTON-54 IN FRAME F_Restricciones /* Restringir */
DO:
   

   IF SinRestriccion:SCREEN-VALUE NE "" THEN DO:
       IF R_Eleccion:SCREEN-VALUE EQ "3" THEN
          Cusu = INTEGER(SUBSTRING(SinRestriccion:SCREEN-VALUE,1,4)).
       CREATE Res_Operacion.
       ASSIGN Res_Operacion.Cod_Operacion = W_CodOperacion
              Res_Operacion.Restriccion   = INTEGER(R_Eleccion:SCREEN-VALUE).
       CASE R_Eleccion:SCREEN-VALUE:
           WHEN "1" THEN ASSIGN Res_Operacion.Agencia = INTEGER(SUBSTRING(SinRestriccion:SCREEN-VALUE,1,3))
                                Res_Operacion.Usuario = "".
           WHEN "2" THEN ASSIGN Res_Operacion.Grupo   = INTEGER(SUBSTRING(SinRestriccion:SCREEN-VALUE,1,2))
                                Res_Operacion.Usuario = "".
           WHEN "3" THEN ASSIGN Res_Operacion.Agencia = 0
                                Res_Operacion.Grupo   = 0
                                Res_Operacion.Usuario = STRING(Cusu).
       END CASE.
       APPLY "value-changed" to R_Eleccion.
   END.
   ELSE DO:
       MESSAGE "Para Restringir se debe seleccionar" SKIP
               "algún campo de la lista. Rectifique" VIEW-AS ALERT-BOX INFORMATION.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-57
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-57 W-Win
ON CHOOSE OF BUTTON-57 IN FRAME F_Restricciones /* Button 57 */
DO:
  HIDE FRAME F_Restricciones.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME Frame_Consulta
&Scoped-define SELF-NAME F_SalirConsulta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL F_SalirConsulta W-Win
ON CHOOSE OF F_SalirConsulta IN FRAME Frame_Consulta /* Salir Consulta */
DO:
  HIDE FRAME Frame_Consulta.
  FRAME F-Main:SENSITIVE = TRUE.
  APPLY "value-changed" TO R_Eleccion IN FRAME F_Restricciones.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Restricciones
&Scoped-define SELF-NAME R_Eleccion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL R_Eleccion W-Win
ON VALUE-CHANGED OF R_Eleccion IN FRAME F_Restricciones
DO:
DO WITH FRAME F_Restricciones:
    DO i = 1 TO 100 BY 1:
       SinRestriccion:DELETE(1).
       ConRestriccion:DELETE(1).
    END.
    CASE R_Eleccion:SCREEN-VALUE:
      WHEN "1" THEN DO:
         FOR EACH Agencias WHERE Agencias.Estado EQ 1 NO-LOCK:
             FIND Res_Operacion WHERE Res_Operacion.Cod_Operacion EQ W_CodOperacion AND 
                                      Res_Operacion.Restriccion   EQ 1 AND 
                                      Res_Operacion.Agencia       EQ Agencias.Agencia NO-ERROR.
             IF NOT AVAILABLE Res_Operacion THEN
                W_Ok = SinRestriccion:ADD-LAST(STRING(Agencias.Agencia,"999") + " - " + Agencias.Nombre).
             ELSE
                W_Ok = ConRestriccion:ADD-LAST(STRING(Agencias.Agencia,"999") + " - " + Agencias.Nombre).
         END.
      END.
      WHEN "2" THEN DO:
         FOR EACH Grupos WHERE Grupos.Estado EQ 1 NO-LOCK:
             FIND Res_Operacion WHERE Res_Operacion.Cod_Operacion EQ W_CodOperacion AND 
                                      Res_Operacion.Restriccion   EQ 2 AND 
                                      Res_Operacion.Grupo         EQ Grupos.Grupo NO-ERROR.
             IF NOT AVAILABLE Res_Operacion THEN
                W_Ok = SinRestriccion:ADD-LAST(STRING(Grupos.Grupo,"99") + " - " + Grupos.Nombre).
             ELSE
                W_Ok = ConRestriccion:ADD-LAST(STRING(Grupos.Grupo,"99") + " - " + Grupos.Nombre).
         END.
      END.
      WHEN "3" THEN DO:
         FOR EACH Usuarios WHERE Usuarios.Estado EQ 1 NO-LOCK:
             FIND Res_Operacion WHERE Res_Operacion.Cod_Operacion EQ W_CodOperacion AND 
                                      Res_Operacion.Restriccion   EQ 3 AND 
                                      Res_Operacion.Usuario       EQ Usuarios.Usuario NO-ERROR.
             IF NOT AVAILABLE Res_Operacion THEN
                W_Ok = SinRestriccion:ADD-LAST(STRING(INTEGER(Usuarios.Usuario),"9999") + " - " + Usuarios.Nombre).
             ELSE
                W_Ok = ConRestriccion:ADD-LAST(STRING(INTEGER(Usuarios.Usuario),"9999") + " - " + Usuarios.Nombre).
         END.
      END.
    END CASE.
END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Imprimir
&Scoped-define SELF-NAME W_RTipo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_RTipo W-Win
ON VALUE-CHANGED OF W_RTipo IN FRAME F_Imprimir
OR RETURN OF W_RTipo DO:
  DO WITH FRAME F_Imprimir:
     ASSIGN W_RTipo.
     CASE W_RTipo:
       WHEN 1 THEN DO:
          HIDE F_CtaFin F_CtaIni.
          VIEW W_RanFin W_RanIni.
          DISABLE F_CtaFin F_CtaIni W_RanFin W_RanIni W_TCampo W_RTipPro.
          ASSIGN W_RanIni:SCREEN-VALUE = ""
                 W_RanFin:SCREEN-VALUE = "".
       END.
       WHEN 2 THEN DO:
          ASSIGN W_TCampo:SCREEN-VALUE = W_TCampo:ENTRY(1).
          ENABLE W_TCampo W_RanFin W_RanIni.
          APPLY "ENTRY" TO W_TCampo.
          RETURN NO-APPLY.
       END.
     END CASE.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_RTipPro
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_RTipPro W-Win
ON VALUE-CHANGED OF W_RTipPro IN FRAME F_Imprimir
DO:
  ASSIGN W_RTipPro.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_TCampo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_TCampo W-Win
ON VALUE-CHANGED OF W_TCampo IN FRAME F_Imprimir /* Selección */
DO:
  DO WITH FRAME F_Imprimir:
     ASSIGN W_TCampo.
     CASE W_TCampo:
       WHEN "Por Codigo" THEN DO:
         VIEW    W_RanFin W_RanIni.
         ENABLE  W_RanFin W_RanIni.
         DISABLE F_CtaFin F_CtaIni W_RTipPro.
         HIDE    F_CtaFin F_CtaIni.
         APPLY "ENTRY" TO W_RanIni.
         RETURN NO-APPLY.
       END.
       WHEN "Por Producto" THEN DO:
         ENABLE  W_RTipPro.
         DISABLE F_CtaFin F_CtaIni W_RanFin W_RanIni.
         HIDE    F_CtaFin F_CtaIni.
         APPLY "ENTRY" TO W_RTipPro.
         RETURN NO-APPLY.
       END.
       WHEN "Por Cuenta" THEN DO:
         VIEW    F_CtaFin F_CtaIni.
         ENABLE  F_CtaFin F_CtaIni.
         DISABLE W_RanFin W_RanIni W_RTipPro.
         HIDE    W_RanFin W_RanIni.
         APPLY "ENTRY" TO F_CtaIni.
         RETURN NO-APPLY.
       END.
       WHEN "Activas" THEN DO:
         VIEW    W_RanFin W_RanIni.
         DISABLE W_RanFin W_RanIni W_RTipPro F_CtaFin F_CtaIni.
         HIDE    F_CtaFin F_CtaIni.
       END.
       WHEN "Inactivas" THEN DO:
         VIEW    W_RanFin W_RanIni.
         DISABLE W_RanFin W_RanIni W_RTipPro F_CtaFin F_CtaIni.
         HIDE    F_CtaFin F_CtaIni.
       END.
     END CASE.
  END.
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
             INPUT  'v-cf_transacciones.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v-cf_transacciones ).
       RUN set-position IN h_v-cf_transacciones ( 1.54 , 3.00 ) NO-ERROR.
       /* Size in UIB:  ( 18.04 , 96.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'b-cf_transacciones.w':U ,
             INPUT  FRAME Frame_Consulta:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b-cf_transacciones ).
       RUN set-position IN h_b-cf_transacciones ( 1.08 , 2.00 ) NO-ERROR.
       RUN set-size IN h_b-cf_transacciones ( 11.85 , 81.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'p-updsav.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Save,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-updsav ).
       RUN set-position IN h_p-updsav ( 9.88 , 101.00 ) NO-ERROR.
       RUN set-size IN h_p-updsav ( 10.54 , 11.86 ) NO-ERROR.

       /* Links to SmartViewer h_v-cf_transacciones. */
       RUN add-link IN adm-broker-hdl ( h_b-cf_transacciones , 'Record':U , h_v-cf_transacciones ).
       RUN add-link IN adm-broker-hdl ( h_p-updsav , 'TableIO':U , h_v-cf_transacciones ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_v-cf_transacciones ,
             BUTTON-52:HANDLE IN FRAME F-Main , 'BEFORE':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_b-cf_transacciones ,
             F_SalirConsulta:HANDLE IN FRAME Frame_Consulta , 'BEFORE':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-updsav ,
             Btn_Restricciones:HANDLE IN FRAME F-Main , 'AFTER':U ).
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
  ENABLE BUTTON-52 Btn_Imprimir Btn_Consulta Btn_Restricciones Btn_Ayuda 
         RECT-225 
      WITH FRAME F-Main IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  DISPLAY R_Eleccion SinRestriccion ConRestriccion 
      WITH FRAME F_Restricciones IN WINDOW W-Win.
  ENABLE R_Eleccion BUTTON-54 Btn_QuitarRes SinRestriccion ConRestriccion 
         BUTTON-57 
      WITH FRAME F_Restricciones IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-F_Restricciones}
  ENABLE F_SalirConsulta 
      WITH FRAME Frame_Consulta IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-Frame_Consulta}
  DISPLAY W_RTipo W_TCampo W_RTipPro F_CtaIni W_RanIni F_CtaFin W_RanFin 
      WITH FRAME F_Imprimir IN WINDOW W-Win.
  ENABLE W_RTipo Btn_Aceptar Btn_Cancelar 
      WITH FRAME F_Imprimir IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-F_Imprimir}
  VIEW W-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Imprimir_Excel W-Win 
PROCEDURE Imprimir_Excel :
{Incluido\Def_Excel.i}
 /* MANDA ENCABEZADO DE HOJA DE CALCULO*/
 DEFINE VAR W_EstadoInf AS CHARACTER FORMAT "X(8)". 
 E_NumFila = 1.
 E_NumColumn = 13.
 E_Fila      = "009" + "Cod_Ope  "
             + "040" + "Nom_Operacion                           "    
             + "006" + "TipPdt"
             + "006" + "ClaOpe"
             + "006" + "TipOpe"
             + "009" + "CtrEfeChe"
             + "006" + "TipVal"
             + "004" + "Comp"
             + "005" + "IdCla"
             + "003" + "Pri"
             + "003" + "Est"
             + "014" + "Cuenta        "
             + "011" + "CodCompensa".
 RUN W-GraExcel.w (INPUT E_Fila, INPUT E_NumColumn, OUTPUT E_CmpGrafic).

/* launch Excel so it is visible to the user */
chExcelApp:Visible = TRUE.

/* create a new Workbook */
chWorkbook = chExcelApp:Workbooks:Add().

/* get the active Worksheet */
chWorkSheet = chExcelApp:Sheets:Item(1).

    FOR EACH Operacion NO-LOCK BY Operacion.Cod_Operacion:
      E_Fila2     = "".
      E_Fila2     = "009" + STRING(Operacion.Cod_Operacion,"999999999")
                  + "040" + STRING(Operacion.Nom_Operacion,"X(40)")
                  + "001" + STRING(Operacion.Tipo_Producto,"9")
                  + "001" + STRING(Operacion.Clase_Operacion,"9")
                  + "001" + STRING(Operacion.Tipo_Operacion,"9")
                  + "010" + STRING(Operacion.Ctrl_EfeChe,"->,>>>,>>9")
                  + "001" + STRING(Operacion.Tipo_Validacion,"9")
                  + "002" + STRING(Operacion.Comprobante,"99")
                  + "003" + STRING(Operacion.Id_Clave,"X(3)")
                  + "003" + STRING(Operacion.Prioridad,"999")
                  + "001" + STRING(Operacion.Estado,"9")
                  + "014" + STRING(Operacion.Cuenta,"X(14)")
                  + "002" + STRING(Operacion.Cod_Compensa,"99").
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcesoImprimir W-Win 
PROCEDURE ProcesoImprimir :
/*------------------------------------------------------------------------------------
  OBSERVACIONES : Imprimir las Operaciones.       
-------------------------------------------------------------------------------------*/
  
  DEFINE VAR W_Coment AS CHARACTER FORMAT "X(60)".
  DEFINE VAR W_Rango  AS CHARACTER FORMAT "X(60)".
  
  DEFINE FRAME F_Encabezado1
    HEADER
    W_Nom_Entidad                    AT 20  FORMAT "X(60)"            
    "PAGINA:"                        AT 100 PAGE-NUMBER FORMAT ">>>9" SKIP(1)
    "LISTADO GENERAL DE OPERACIONES" AT 34  SKIP(1)
    "--------------------------------------------------------------------------------------------------------------------------------------------------------" AT 1 SKIP(1)
    "Número            Nombre             Tipo       Clase       Tipo        Control     Tipo       Cpbte       Req.   Prioridad Estado  Cuenta         Banco" AT 1 
    "                                     Pto        Opera.      Opera.      Efe/Che     Vald.                  Clave                                        " AT 1 
    "--------------------------------------------------------------------------------------------------------------------------------------------------------" AT 1
  WITH WIDTH 200 USE-UNDERLINE USE-TEXT NO-BOX PAGE-TOP FRAME F_Encabezado1.
     
  DEFINE FRAME F_Encabezado2
    HEADER
    W_Nom_Entidad                    AT 20  FORMAT "X(60)"
    "PAGINA:"                        AT 100 PAGE-NUMBER FORMAT ">>>9" SKIP(1)
    W_Coment                         AT 20 FORMAT "X(60)"
    W_Rango                          AT 20 FORMAT "X(60)" SKIP(1)
    "--------------------------------------------------------------------------------------------------------------------------------------------------------" AT 1 
    "Número            Nombre             Tipo       Clase       Tipo        Control     Tipo       Cpbte       Req.   Prioridad Estado  Cuenta         Banco" AT 1 
    "                                     Pto        Opera.      Opera.      Efe/Che     Vald.                  Clave                                        " AT 1 SKIP(1)
    "--------------------------------------------------------------------------------------------------------------------------------------------------------" AT 1
  WITH WIDTH 200 USE-UNDERLINE USE-TEXT NO-BOX PAGE-TOP FRAME F_Encabezado2.
  
  DEFINE FRAME F-PiePagina
    HEADER 
      "FECHA:"       AT 2 
       TODAY         AT 10 FORMAT "99/99/9999"
       W_Nom_Agencia AT 46
      "HORA:"        AT 100 STRING(TIME,"HH:MM:SS")
  WITH WIDTH 150 FRAME F-PiePagina PAGE-BOTTOM USE-TEXT STREAM-IO. 

  ASSIGN FRAME F_Imprimir F_CtaFin F_CtaIni W_RanFin 
               W_RanIni W_RTipo W_RTipPro W_TCampo.
  IF W_RTipo EQ 1 THEN DO:
     VIEW FRAME F_Encabezado1.
     VIEW FRAME F-Piepagina.
     FOR EACH Operacion NO-LOCK BY Operacion.Cod_Operacion:
         RUN Asignacion.
         RUN Imp_Campos.
     END.
  END.
  ELSE DO:
     VIEW FRAME F_Encabezado2.
     VIEW FRAME F-Piepagina.
     ASSIGN W_Coment = "Listado de Operaciones " + W_TCampo.
     RUN Justificar IN W_Manija (INPUT-OUTPUT W_Coment,INPUT " ",INPUT 60,INPUT "C").
     CASE W_TCampo:
       WHEN "Por Codigo" THEN DO:
         ASSIGN W_Rango = "Código Inicial " + STRING(W_RanIni) + " " + "Código Final " + STRING(W_RanFin).
         RUN Justificar IN W_Manija (INPUT-OUTPUT W_Rango,INPUT " ",INPUT 60,INPUT "C").
         FOR EACH Operacion WHERE Operacion.Cod_Operacion GE W_RanIni
                            AND   Operacion.Cod_Operacion LE W_RanFin 
                            NO-LOCK BY Operacion.Cod_Operacion:
             RUN Asignacion.
             RUN Imp_Campos.
         END.
       END.
       WHEN "Por Producto" THEN DO:
         IF W_RTipPro EQ 1 THEN DO:
            ASSIGN W_Rango = "Listado de Productos de Ahorro".
         END.
         ELSE
         IF W_RTipPro EQ 2 THEN DO:
            ASSIGN W_Rango = "Listado de Productos de Crédito".
         END.
         ELSE
         IF W_RTipPro EQ 3 THEN DO:
            ASSIGN W_Rango = "Listado de Cuentas Contables".
         END.
         RUN Justificar IN W_Manija (INPUT-OUTPUT W_Rango,INPUT " ",INPUT 60,INPUT "C").
         FOR EACH Operacion WHERE Operacion.Tipo_Producto EQ W_RTipPro 
                            NO-LOCK BY Operacion.Cod_Operacion:
             RUN Asignacion.
             RUN Imp_Campos.
         END.
       END.
       WHEN "Por Cuenta" THEN DO:
         ASSIGN W_Rango = "Cuenta Inicial " + F_CtaIni + " " + "Cuenta Final " + F_CtaFin.
         RUN Justificar IN W_Manija (INPUT-OUTPUT W_Rango,INPUT " ",INPUT 60,INPUT "C").
         FOR EACH Operacion WHERE Operacion.Cuenta GE F_CtaIni
                            AND   Operacion.Cuenta LE F_CtaFin 
                            NO-LOCK BY Operacion.Cuenta:
             RUN Asignacion.
             RUN Imp_Campos.
         END.
       END.
       WHEN "Activas" THEN DO:
         ASSIGN W_Rango = "".
         FOR EACH Operacion WHERE Operacion.Estado EQ 1 
                            NO-LOCK BY Operacion.Cod_Operacion:
             RUN Asignacion.
             RUN Imp_Campos.
         END.
       END.
       WHEN "Inactivas" THEN DO:
         ASSIGN W_Rango = "".
         FOR EACH Operacion WHERE Operacion.Estado EQ 2 
                            NO-LOCK BY Operacion.Cod_Operacion:
             RUN Asignacion.
             RUN Imp_Campos.
         END.
       END.
     END CASE.
  END.
  PAGE.
END PROCEDURE.

/*************************************************************************************/
/******************************* PROCEDIMIENTOS **************************************/
/*************************************************************************************/

PROCEDURE Imp_Campos:
  DISPLAY Operacion.Cod_Operacion    AT 1   NO-LABEL
          Operacion.Nom_Operacion    AT 11  NO-LABEL FORMAT "X(25)"
          T_Pto                      AT 37  NO-LABEL
          C_OPe                      AT 49  NO-LABEL
          T_Ope                      AT 61  NO-LABEL
          T_EC                       AT 73  NO-LABEL
          T_Val                      AT 85  NO-LABEL
          Operacion.Comprobante      AT 97  NO-LABEL
          Operacion.Id_Clave         AT 109 NO-LABEL FORMAT "SI/NO"
          Operacion.Prioridad        AT 117 NO-LABEL
          Operacion.Estado           AT 127 NO-LABEL
          Operacion.Cuenta           AT 133 NO-LABEL
          Operacion.Cod_Compensa     AT 149 NO-LABEL
  WITH FRAME F-Imp DOWN WIDTH 200 USE-TEXT STREAM-IO NO-BOX.
  DOWN WITH FRAME F-Imp.
END PROCEDURE.

PROCEDURE Asignacion:
  IF Operacion.Tipo_Producto EQ 1 THEN DO:
     ASSIGN T_Pto = "AHORROS".
  END.
  ELSE
  IF Operacion.Tipo_Producto EQ 2 THEN DO:
     ASSIGN T_Pto = "CREDITOS".
  END.
  ELSE
  IF Operacion.Tipo_Producto EQ 3 THEN DO:
     ASSIGN T_Pto = "CONTABLE".
  END.
  IF Operacion.Ctrl_EfeChe EQ 1 THEN DO:
     ASSIGN T_EC = "EFECTIVO".
  END.
  ELSE
  IF Operacion.Ctrl_EfeChe EQ 2 THEN DO:
     ASSIGN T_EC = "CHEQUE".
  END.
  IF Operacion.Clase_Operacion EQ 1 THEN DO:
     ASSIGN C_OPe = "TAQUILLA".
  END.
  ELSE
  IF Operacion.Clase_Operacion EQ 2 THEN DO:
     ASSIGN C_OPe = "NOMINA".
  END.
  ELSE
  IF Operacion.Clase_Operacion EQ 3 THEN DO:
     ASSIGN C_OPe = "PROCESOS".
  END.
  IF Operacion.Tipo_Operacion EQ 1 THEN DO:
     ASSIGN T_Ope = "INGRESOS".
  END.
  ELSE
  IF Operacion.Tipo_Operacion EQ 2 THEN DO:
     ASSIGN T_Ope = "EGRESOS".
  END.
  ELSE
  IF Operacion.Tipo_Operacion EQ 3 THEN DO:
     ASSIGN T_Ope = "TRASLADO".
  END.
  ELSE
  IF Operacion.Tipo_Operacion EQ 4 THEN DO:
     ASSIGN T_Ope = "CORRECCION".
  END.
  IF Operacion.Tipo_Validacion EQ 1 THEN DO:
     ASSIGN T_Val = "NINGUNO".
  END.
  ELSE
  IF Operacion.Tipo_Validacion EQ 2 THEN DO:
     ASSIGN T_Val = "VALIDADORA".
  END.
  ELSE
  IF Operacion.Tipo_Validacion EQ 3 THEN DO:
     ASSIGN T_Val = "IMPRESORA".
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

