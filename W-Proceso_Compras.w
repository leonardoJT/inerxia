&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
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
{INCLUIDO\VARIABLE.I "SHARED"}
  DEFINE VAR W_Re AS INTEGER.
  DEFINE VARIABLE W_Error       AS LOGICAL.
  DEFINE VARIABLE W_Autorizo  LIKE Usuarios.Usuario.
  DEFINE VARIABLE choice AS LOGICAL.
  DEFINE VARIABLE W_Grupo LIKE Grupos.Grupo.
  DEFINE VAR W_NvaAdm AS LOGICAL.
  DEFINE VAR W_NvaHV AS LOGICAL.
  DEFINE VAR W_NvoCD AS LOGICAL.
  DEFINE VAR W_Pdt AS CHARACTER FORMAT "X(80)".
  DEFINE VAR Puntero AS ROWID.
  DEFINE VAR PuntGar AS ROWID.
  DEFINE VAR Longitud AS DECIMAL.
  DEFINE VAR W_Ultima  LIKE Instancias.Instancia.
  DEFINE VAR W_Primera LIKE Instancias.Instancia.
  DEFINE VAR W_SucAgen LIKE Usuarios.Id_OpeOfi.
  DEFINE VAR Id_Agregar AS CHARACTER FORMAT "X(2)".
  DEFINE VAR W_Tippdt LIKE Creditos.Tip_Credito.
  DEFINE VAR W_TipoInforme AS CHARACTER FORMAT "X(10)".
  DEFINE VAR T_Operacion LIKE Operacion.Cod_Operacion.
  DEFINE VAR T_Deducible LIKE Operacion.Cod_Deducible.
  DEFINE VAR W_ProFor    LIKE Formatos.Nom_Proceso.
  DEFINE VAR W_CodChe    LIKE Cuentas.Cod_Formato.
  DEFINE VAR W_VigIns    LIKE Instancias.TMI.
  DEFINE VAR W_NumCbt    LIKE Comprobantes.Secuencia.
   

DEFINE VAR W_Age LIKE Creditos.Agencia.
DEFINE VAR W_Pro LIKE Creditos.Cod_Credito.
DEFINE VAR W_NitW LIKE Creditos.Nit.
DEFINE VAR W_Cue LIKE Creditos.Num_Credito.   
   
  DEFINE VAR W_CtaCorCre LIKE Cuentas.Cuenta.
  DEFINE VAR W_CtaCorAho LIKE Cuentas.Cuenta.
  DEFINE VAR W_CtaBanco  LIKE Cuentas.Cuenta.
  DEFINE VAR W_Des       AS LOGICAL.
  DEFINE VAR W_MontoCre LIKE Creditos.Sdo_Capital.
  DEFINE VAR W_MontoDeb LIKE Creditos.Sdo_Capital.
  DEFINE VAR W_Cbte     LIKE Comprobantes.Comprobante.
 
/*para buscar un cliente*/
  DEFINE VARIABLE P_Nit      LIKE Clientes.Nit.
  DEFINE VARIABLE p_Nombre   LIKE Clientes.Nombre.
  DEFINE VARIABLE P_Apellido LIKE Clientes.Apellido1.
  DEFINE VARIABLE P_AgeCli   LIKE Clientes.Agencia.
  DEFINE VARIABLE W_Nuevo    AS LOGICAL.
  
  DEFINE VAR W_Contenido AS CHARACTER FORMAT "X(400)".
  
/*para buscar una cuenta de ahorro*/
  DEFINE VARIABLE A_Nit LIKE Ahorros.Nit.
  DEFINE VARIABLE A_Age LIKE Ahorros.Agencia.
  DEFINE VARIABLE A_Pro LIKE Ahorros.Cod_Ahorro.
  DEFINE VARIABLE A_NitW LIKE Ahorros.Nit.
  DEFINE VARIABLE A_Cue LIKE Ahorros.Cue_Ahorros.
  
  DEFINE VARIABLE i AS INTEGER.
  DEFINE VARIABLE W_Ok AS LOGICAL.
  DEFINE VARIABLE W_TipoProducto LIKE Pro_Creditos.Tip_Credito.
  DEFINE VARIABLE Dias AS DECIMAL.
      
  DEFINE TEMP-TABLE Consulta
      FIELD Num_Credito   LIKE Creditos.Num_Credito
      FIELD Num_Solicitud LIKE Creditos.Num_Solicitud
      FIELD AgeCredito    LIKE Agencias.Agencia
      FIELD CodProducto   LIKE Creditos.Cod_Credito
      FIELD TipProducto   LIKE Creditos.Tip_Credito
      FIELD NomProducto   AS CHARACTER FORMAT "X(40)"
      FIELD Fec_Ingreso   LIKE Mov_Instancias.Fec_Ingreso
      FIELD Hor_Ingreso   AS CHARACTER FORMAT "X(15)"
      FIELD Sdo_Capital   LIKE Creditos.Sdo_Capital
      FIELD INT_Corrientes   LIKE Creditos.Sdo_Capital
      FIELD INT_Anticipados   LIKE Creditos.Sdo_Capital
      FIELD Costas LIKE Creditos.Costas
      FIELD Honorarios LIKE Creditos.Honorarios
      FIELD Polizas LIKE Creditos.Polizas.

   DEFINE VAR W_Consulta   AS   LOGICAL.
   DEFINE VAR W_TotCuoEsp  LIKE Pro_Especiales.Ran_FinCuota.
   DEFINE VAR W_NomTer     LIKE  Terceros.Nombre.
   DEFINE VAR W_Tercero    LIKE  Tercero.Nit.
   DEFINE VAR W_NvaAse     AS LOGICAL INITIAL FALSE.
   DEFINE VAR W_Rpta1      AS LOGICAL INITIAL FALSE.
   DEFINE VAR W_NomLin     AS    CHARACTER FORMAT "X(15)".
   DEFINE VAR W_P          AS    INTEGER.
   DEFINE VAR W_ConDed     AS    INTEGER INITIAL 1.
   DEFINE VAR W_Ind        AS    INTEGER INITIAL 0.  
   DEFINE VAR W_WidSel     AS    WIDGET-HANDLE.
   DEFINE VAR W_Cerrado    AS    INTEGER INITIAL 0.
   DEFINE VAR W_VlrAux     AS    DECIMAL FORMAT ">>>,>>>,>>>,>>9" INITIAL 0.
   DEFINE VAR W_PerDed     AS    INTEGER INITIAL 1.
   DEFINE VAR W_Codfor     LIKE  Formatos.Cod_Formato INITIAL 0.
   DEFINE VAR W_Interplazo AS    DECIMAL FORMAT "->>>>>>>>>>>9" INITIAL 0.   
   DEFINE VAR W_Destino    LIKE  Solicitud.Destino.
   DEFINE VAR W_TasEfe     AS    DECIMAL FORMAT ">>9.999999".
   DEFINE VAR W_TasaCont   AS    DECIMAL FORMAT ">>9.999999".
   DEFINE VAR W_Liquidar   AS    LOGICAL INITIAL FALSE.
   DEFINE VAR W_Existe     AS    LOGICAL INITIAL FALSE.
   DEFINE VAR W_NroPer     AS    INTEGER INITIAL 1.
   DEFINE VAR W_MonMul     AS    DECIMAL FORMAT ">>>,>>>,>>>,>>9" INITIAL 0.
   DEFINE VAR W_Razon      AS    DECIMAL FORMAT "->>>>>>>>>>>9.999" INITIAL 0.
   DEFINE VAR P_Band       AS    LOGICAL INITIAL FALSE.
   DEFINE VAR W_TasNom     AS    DECIMAL FORMAT "->>>>>>>>>>>9.999" INITIAL 0.
   DEFINE VAR W_ErrIndica  AS    LOGICAL.
   DEFINE VAR W_TotPorDed  AS    DECIMAL FORMAT ">>9.9999" INITIAL 0.
   DEFINE VAR Suma         AS    DECIMAL FORMAT ">>>,>>>,>>>,>>9" INITIAL 0.
   DEFINE VAR W_Suma       AS    DECIMAL FORMAT ">>>,>>>,>>>,>>9" INITIAL 0.
   DEFINE VAR W_PlaPer     AS    INTEGER INITIAL 0.   
   DEFINE VAR W_PlazoDias  AS    INTEGER INITIAL 0.   
   DEFINE VAR W_DesAse     LIKE  Varios.Codigo.
   DEFINE VAR W_MonMin     LIKE  Pro_Creditos.Val_Montominimo.
   DEFINE VAR W_MonMax     LIKE  Pro_Creditos.Val_Montomaximo.
   DEFINE VAR W_PlaMin     LIKE  Pro_Creditos.Pla_Minimo.
   DEFINE VAR W_PlaMax     LIKE  Pro_Creditos.Pla_Maximo.    
   DEFINE VAR W_Sistema    LIKE  Solicitud.Sistema INITIAL 0.      
   DEFINE VAR W_Gracia     LIKE  Solicitud.Per_Gracia.
   DEFINE VAR W_TipInt     LIKE  Solicitud.For_Interes INITIAL 1.
   DEFINE VAR W_Incremento LIKE  Solicitud.Incremento.
   DEFINE VAR W_ClaTas     LIKE  Pro_Creditos.Tip-Tasa.
   DEFINE VAR W_PNegocia   LIKE  Ran_Interes.Pun_Negociables.
   DEFINE VAR W_TasDif     LIKE  Pro_Creditos.Id_TasDiferencial.
   DEFINE VAR W_TotExtras  LIKE  Pro_Creditos.Val_Montomaximo.
   DEFINE VAR W_DiaPer     AS    INTEGER   FORMAT "9999".
   DEFINE VAR W_PerLiqui   AS    INTEGER   FORMAT "99".
   DEFINE VAR W_Per        AS    DECIMAL   FORMAT "999.9999999".
   DEFINE VAR W_LinAho     LIKE Solicitud.Lin_Ahorro.
   DEFINE VAR W_CueAho     LIKE Solicitud.Cue_desembolso.
   DEFINE VAR W_CreMixto   LIKE Solicitud.Monto.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F_Creditos

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-2 Btn_Opera Cmb_Instancias BUTTON-1 ~
Btn_Imprimir BUTTON-2 BUTTON-4 
&Scoped-Define DISPLAYED-OBJECTS Cmb_Instancias 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Imprimir 
     IMAGE-UP FILE "imagenes/impresora2.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/impresora2.bmp":U
     LABEL "Button 8" 
     SIZE 10 BY 1.62.

DEFINE BUTTON Btn_Opera 
     LABEL "Realizar Operación" 
     SIZE 20 BY 1.08
     FONT 4.

DEFINE BUTTON BUTTON-1 
     IMAGE-UP FILE "imagenes/informacion.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/informacion.bmp":U
     LABEL "Button 1" 
     SIZE 10 BY 1.62.

DEFINE BUTTON BUTTON-2 
     LABEL "Salir" 
     SIZE 10 BY 1.62.

DEFINE BUTTON BUTTON-4 
     IMAGE-UP FILE "imagenes/interrogacion.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/interrogacion.bmp":U
     LABEL "Button 4" 
     SIZE 5 BY 1.12.

DEFINE VARIABLE Cmb_Instancias AS CHARACTER FORMAT "X(256)":U 
     LABEL "Operaciones" 
     VIEW-AS COMBO-BOX INNER-LINES 10
     DROP-DOWN-LIST
     SIZE 60 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 12 BY 6.46.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F_Creditos
     Btn_Opera AT ROW 1.46 COL 78
     Cmb_Instancias AT ROW 1.54 COL 13 COLON-ALIGNED
     BUTTON-1 AT ROW 1.54 COL 103
     Btn_Imprimir AT ROW 3.15 COL 103
     BUTTON-2 AT ROW 5.31 COL 103
     BUTTON-4 AT ROW 8 COL 106
     RECT-2 AT ROW 1.27 COL 102
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 113.72 BY 21.38
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
         TITLE              = "SFG - Administración de Compras"
         HEIGHT             = 21.38
         WIDTH              = 113.72
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
/* SETTINGS FOR FRAME F_Creditos
                                                                        */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* SFG - Administración de Compras */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* SFG - Administración de Compras */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Imprimir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Imprimir wWin
ON CHOOSE OF Btn_Imprimir IN FRAME F_Creditos /* Button 8 */
DO:
  DEFINE VAR Listado     AS CHARACTER INITIAL "".
  Listado = W_PathSpl + "Proyeccion.LST".
  {INCLUIDO\Imprimir.I "Listado"}  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Opera
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Opera wWin
ON CHOOSE OF Btn_Opera IN FRAME F_Creditos /* Realizar Operación */
DO:
DO WITH FRAME F_Cre:
FIND Instancias WHERE Instancias.Instancia EQ INTEGER(SUBSTRING(Cmb_Instancias:SCREEN-VALUE,1,5)) NO-LOCK NO-ERROR.
  IF AVAILABLE Instancias THEN DO:
      IF Instancias.Programa NE 0 THEN DO:
       FIND Programas WHERE Programas.Programa EQ Instancias.Programa NO-LOCK NO-ERROR.
       IF AVAILABLE Programas THEN DO:
          DISABLE ALL WITH FRAME F_Creditos.
          RUN VALUE(Programas.Ejecutable)
              (INPUT Cmb_Instancias:SCREEN-VALUE,
               INPUT STRING(Instancias.Cod_Operacion,"999999999"),
               INPUT 1).
       END.
       ENABLE ALL WITH FRAME F_Creditos.
    END.
    ELSE DO:
      MESSAGE "La instancia no tiene ningún programa asociado" SKIP
              "comunique esta inconsistencia al administrador" VIEW-AS ALERT-BOX.
    END.
  END.
  ELSE MESSAGE "No existe la instancia" VIEW-AS ALERT-BOX.
END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-1 wWin
ON CHOOSE OF BUTTON-1 IN FRAME F_Creditos /* Button 1 */
DO:
  RUN W-InfDia.r NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-2 wWin
ON CHOOSE OF BUTTON-2 IN FRAME F_Creditos /* Salir */
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


&Scoped-define SELF-NAME Cmb_Instancias
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_Instancias wWin
ON VALUE-CHANGED OF Cmb_Instancias IN FRAME F_Creditos /* Operaciones */
DO:
  FIND Instancias WHERE Instancias.Instancia EQ INTEGER(SUBSTRING(Cmb_Instancias:SCREEN-VALUE IN FRAME F_Creditos,1,3)) NO-LOCK NO-ERROR.
  VIEW FRAME F_Consulta.
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
  DISPLAY Cmb_Instancias 
      WITH FRAME F_Creditos IN WINDOW wWin.
  ENABLE RECT-2 Btn_Opera Cmb_Instancias BUTTON-1 Btn_Imprimir BUTTON-2 
         BUTTON-4 
      WITH FRAME F_Creditos IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Creditos}
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Informe wWin 
PROCEDURE Informe :
{Incluido\RepEncabezado.i}
/*    DEFINE VAR W_Cliente AS CHARACTER FORMAT "X(60)".
    DEFINE VAR T_Plazo   AS CHARACTER FORMAT "X(30)".
    DEFINE VAR T_Dedu    AS CHARACTER FORMAT "X(30)".
    
    ASSIGN W_Cliente = Creditos.Nit:SCREEN-VALUE IN FRAME F_Solicitud + " - " + NomNit:SCREEN-VALUE IN FRAME F_Solicitud.
 
    W_Reporte   = "REPORTE   : CREDITO - FECHA: " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am").
    W_EncColumna = "Cliente Solicitante         :   " + W_Cliente.
    
    IF W_Re EQ 1 AND FRAME F_Repro:HIDDEN EQ NO THEN
       W_Reporte   = "REESTRUCTURACION   : CREDITO - FECHA: " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am").
    IF W_Re EQ 2 AND FRAME F_Repro:HIDDEN EQ NO THEN
       W_Reporte   = "PRORROGA   : CREDITO - FECHA: " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am").
    
 VIEW FRAME F-Encabezado.
 VIEW FRAME F-Ftr.

 DO WITH FRAME F_Solicitud:
   T_Plazo = Creditos.Plazo:SCREEN-VALUE  + " (" + SUBSTRING(Cmb_PerPago:SCREEN-VALUE,5,15) + ")".
   DISPLAY 
  /* 1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890*/
     "=============================================DATOS GENERALES DE LA SOLICITUD==============================================" AT 1
     "Agencia de Radicación       : " AT 1
     Cmb_Agencias:SCREEN-VALUE        AT 33 FORMAT "X(30)"
     "Número del Crédito          : " AT 65
     STRING(Creditos.Num_Credito:SCREEN-VALUE) AT 98
     "Número de Solicitud         : " AT 1
     STRING(Creditos.Num_Solicitud:SCREEN-VALUE) AT 33
     "Fecha de Aprobación         : " AT 65
     Creditos.Fec_Aprobacion:SCREEN-VALUE  AT 98  FORMAT "X(10)"
     "Producto de Crédito         : " AT 1
     Nom_Producto:SCREEN-VALUE        AT 33 FORMAT "X(30)"
     "Tipo de Producto            : " AT 65
     TRIM(W_Tipo_Credito:SCREEN-VALUE) AT 98 FORMAT "X(30)"
     "Instancia Actual            : " AT 1
     Cmb_Instancias:SCREEN-VALUE IN FRAME F_Creditos AT 33  FORMAT "X(30)"
     "Usuario Actualmente Procesa : " AT 65
     NomUsuario:SCREEN-VALUE IN FRAME F_Creditos AT 98  FORMAT "X(30)" 
     "Forma de Pago de la Cuota   : " AT 1
     W_ForPago:SCREEN-VALUE           AT 33 FORMAT "X(30)"
     "=============================================DETALLE DE VALORES DEL CREDITO==============================================" AT 1
     "Monto a Prestar             : " AT 1
     Creditos.Monto:SCREEN-VALUE     AT 33  FORMAT "X(30)"
     "Tasa Efectiva Anual         : " AT 65
     Creditos.Tasa:SCREEN-VALUE      AT 98  FORMAT "X(30)" SKIP(1)
     WITH FRAME F_Sol WIDTH 132 NO-BOX USE-TEXT NO-LABELS STREAM-IO.
   IF W_Re EQ 1 OR W_Re EQ 2 THEN
     DISPLAY "CAMBIOS POR REESTRUCTURACION O PRORROGA" AT 1
     "Sdo Capital Anterior        : " AT 1
     Creditos.Sdo_Capital:SCREEN-VALUE IN FRAME F_Solicitud AT 33  FORMAT "X(30)"
     "Saldo Capital Reestructurado: " AT 65
     W_NvoSdoCap:SCREEN-VALUE IN FRAME F_Repro AT 98
     "Plazo Anterior              : " AT 1
     T_Plazo                          AT 33  FORMAT "X(30)"
     "Nuevo Plazo                 : " AT 65
     W_NvoPlazo:SCREEN-VALUE IN FRAME F_Repro AT 98
     "Cuota Anterior              : " AT 1
     Creditos.Cuota:SCREEN-VALUE IN FRAME F_Solicitud AT 33  FORMAT "X(30)"
     "Nuevo Cuota                 : " AT 65
     W_NvaCuota:SCREEN-VALUE IN FRAME F_Repro AT 98
   WITH FRAME F_Sol2 WIDTH 132 NO-BOX USE-TEXT NO-LABELS STREAM-IO.
   
   IF W_Re EQ 2 THEN
   DISPLAY 
     "=============================================DETALLE DEL ATRASO==============================================" AT 1
     "Valor del Atraso Actual     : " AT 1
     Creditos.Val_Atraso:SCREEN-VALUE      AT 33  FORMAT "X(30)"
     "Dias Atrasados              : " AT 65
     Creditos.Dias_Atraso:SCREEN-VALUE       AT 98  FORMAT "X(30)"
     "Cuotas Atrasadas            : " AT 1
     Creditos.Cuo_Atraso:SCREEN-VALUE      AT 33  FORMAT "X(30)"
   WITH FRAME F_Sol3 WIDTH 132 NO-BOX USE-TEXT NO-LABELS STREAM-IO.
 END.
  
 PAGE.*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject wWin 
PROCEDURE initializeObject :
FOR EACH Instancias WHERE Instancias.Tipo_Instancia EQ 7 AND
                          Instancias.Estado         EQ 1 
                          /*Instancias.Tipo_Producto  EQ 2*/ NO-LOCK BREAK BY Instancias.Orden:
     IF Instancias.Ultima THEN W_Ultima = Instancias.Instancia.
     IF Instancias.Primera THEN W_Primera = Instancias.Instancia.
     FIND FIRST Cfg_Instancias WHERE
          /* Cfg_Instancias.Agencia EQ W_Agencia AND*/
           Cfg_Instancias.Tipo_Instancia EQ 7  AND
           Cfg_Instancias.Instancia EQ Instancias.Instancia AND
           Cfg_Instancias.Usuario EQ W_Usuario AND
           Cfg_Instancias.Estado  EQ 1 NO-LOCK NO-ERROR.
     IF AVAILABLE Cfg_Instancias THEN DO:
        W_Ok = Cmb_Instancias:ADD-LAST(STRING(Instancias.Instancia,"99999") + " - " + Instancias.Nom_Instancia) IN FRAME F_Creditos.
     END.
  END.
  FIND Usuarios WHERE Usuarios.Usuario EQ W_Usuario NO-LOCK NO-ERROR.
  ASSIGN W_SucAgen  = Usuarios.Id_OpeOfi
         W_Grupo    = Usuarios.Grupo.
  
                             
  
  RUN SUPER.
  
  Cmb_Instancias:SCREEN-VALUE = Cmb_Instancias:ENTRY(1).
  WWin:TITLE = "SFG - Administración de Compras - Agencia Actual: " + STRING(W_Agencia).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcesoImprimir wWin 
PROCEDURE ProcesoImprimir :
RUN Informe.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

