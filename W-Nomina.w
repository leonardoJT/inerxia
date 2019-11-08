&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS wWin 

/* oakley */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

{src/adm2/widgetprto.i}
{Incluido/Variable.I "SHARED"}

DEFINE VAR Id_Adicion AS LOGICAL INITIAL NO.
DEFINE VAR NitAdicion AS CHARACTER.
DEFINE VAR W_Puntero AS ROWID.
DEFINE VAR W_Puntero2 AS ROWID.
DEFINE VAR WNit AS CHARACTER.
DEFINE VAR WEst AS CHARACTER FORMAT "X(25)".
DEFINE VAR WVal AS DECIMAL.
DEFINE VAR Faltan AS INTEGER.
DEFINE VAR WHistorico AS LOGICAL INITIAL NO.
DEFINE VAR i AS INTEGER FORMAT "99".
DEFINE VAR WVAnt AS DECIMAL FORMAT ">>>,>>>,>>9.99".
DEFINE VAR W_Ok AS LOGICAL.
DEFINE VAR WTipo AS INTEGER FORMAT "9".
DEFINE VAR Basico AS DECIMAL.
DEFINE VAR Linea AS INTEGER FORMAT "99".
DEFINE VAR WDiferencia AS DECIMAL FORMAT "->>>,>>>,>>9.99".
DEFINE VAR Choice AS LOGICAL.
DEFINE VAR W_DocContab AS INTEGER.
DEFINE VAR W_Error AS LOGICAL.
DEFINE VARIABLE Cbte AS INTEGER.
DEFINE VAR WDes AS CHARACTER FORMAT "X(46)".
DEFINE VAR WNom AS CHARACTER FORMAT "X(25)".
DEFINE VAR WFila AS INTEGER FORMAT "99".

   DEFINE VARIABLE P_Nit      LIKE Clientes.Nit.
   DEFINE VARIABLE p_Nombre   LIKE Clientes.Nombre.
   DEFINE VARIABLE P_Apellido LIKE Clientes.Apellido1.
   DEFINE VARIABLE P_CliAge   LIKE Clientes.Agencia.
   

DEFINE BUFFER WCfg FOR Cfg_Novedades.
DEFINE BUFFER WCfgPrc FOR Cfg_Novedades.

DEFINE VAR WSMLV LIKE Indicadores.Valor.
DEFINE TEMP-TABLE TEmpleados
    FIELD Age    LIKE Agencias.Agencia
    FIELD Nit    LIKE Clientes.Nit
    FIELD Nom    AS CHARACTER FORMAT "X(45)"
    FIELD Car    AS CHARACTER FORMAT "X(40)"
    FIELD Est    LIKE Empleados.Estado
    FIELD TDv    AS DECIMAL FORMAT ">>>,>>>,>>9.99"
    FIELD TDd    AS DECIMAL FORMAT ">>>,>>>,>>9.99"
    FIELD Fec    AS DATE FORMAT "99/99/9999"
    FIELD Dia    AS INTEGER FORMAT "99"
    FIELD Bas    LIKE Empleados.Salario_Mensual
    FIELD Lin    AS INTEGER FORMAT "99"
    FIELD Ima    LIKE Empleados.Id_Manejo.

DEFINE TEMP-TABLE TProv
    FIELD Nit   LIKE Clientes.Nit
    FIELD CtaDb LIKE Cuentas.Cuenta
    FIELD CtaCr LIKE Cuentas.cuenta
    FIELD Valor LIKE Ahorros.Sdo_Disponible.

DEFINE TEMP-TABLE TExtras
    FIELD Age    LIKE Agencias.Agencia
    FIELD Nit    LIKE Clientes.Nit
    FIELD NmE    AS DECIMAL FORMAT "99.9"
    FIELD VrE    AS DECIMAL FORMAT ">>,>>>,>>9.99".


DEFINE TEMP-TABLE TDedDev
    FIELD Age    LIKE Agencias.Agencia
    FIELD Tip    LIKE Cfg_Novedades.Tipo
    FIELD Cod    LIKE Cfg_Novedades.Codigo
    FIELD Cla    LIKE Cfg_Novedades.Clase
    FIELD Nom    AS CHARACTER FORMAT "X(40)"
    FIELD Nit    LIKE Clientes.Nit
    FIELD Fec    AS DATE FORMAT "99/99/9999"
    FIELD Val    AS DECIMAL FORMAT ">>>,>>>,>>9.99"
    FIELD Dia    AS INTEGER FORMAT "99"
    FIELD IMo    LIKE Cfg_Novedades.Id_Valor
    FIELD Nat    LIKE Cfg_Novedades.Naturaleza
    FIELD Cta    LIKE Cfg_Novedades.Cuenta
    FIELD Est    LIKE Novedades_Nomina.Estado_Liquidacion
    FIELD Usu    LIKE Usuarios.Usuario
    FIELD Per    LIKE Empleados.PerNom_Actual
    FIELD IPa    LIKE Cfg_Novedades.Id_Pago
    FIELD NmE    AS DECIMAL FORMAT "99.9"
    FIELD VrE    AS DECIMAL FORMAT ">>,>>>,>>9.99"
    FIELD IEx    LIKE Cfg_Novedades.Id_Extras
    FIELD ISa    LIKE Cfg_Novedades.Id_Salario
    FIELD IBa    LIKE Cfg_Novedades.Id_Base
    FIELD Nco    LIKE Clientes.Nit.

DEFINE TEMP-TABLE TImpresion
    FIELD Num AS INTEGER FORMAT "999"
    FIELD Nit LIKE Clientes.Nit
    FIELD Vec AS CHARACTER FORMAT "X(115)".

DEFINE TEMP-TABLE TConceptos
    FIELD Tip    LIKE Cfg_Novedades.Tipo
    FIELD Cod    LIKE Cfg_Novedades.Codigo
    FIELD Nom    LIKE Cfg_Novedades.Nombre
    FIELD Idv    LIKE Cfg_Novedades.Id_Valor
    FIELD ISm    LIKE Cfg_Novedades.Id_Smlv
    FIELD NSm    LIKE Cfg_Novedades.Num_SMLV
    FIELD Pnt    LIKE Cfg_Novedades.Puntos
    FIELD Nat    LIKE Cfg_Novedades.Naturaleza
    FIELD Pad    LIKE Cfg_Novedades.PunAd_Calculo
    FIELD Cta    LIKE Cfg_Novedades.Cuenta
    FIELD Val    LIKE Cfg_Novedades.Valor
    FIELD Cla    LIKE Cfg_Novedades.Clase
    FIELD IEs    AS LOGICAL
    FIELD Prg    LIKE Cfg_Novedades.Programa
    FIELD IEx    LIKE Cfg_Novedades.Id_Extras
    FIELD ISa    LIKE Cfg_Novedades.Id_Salario
    FIELD IBa    LIKE Cfg_Novedades.Id_Base
    FIELD IPa    LIKE Cfg_Novedades.Id_Pago
    FIELD Nit    LIKE Cfg_Novedades.Nit_Contable
    FIELD Por    LIKE Cfg_Novedades.Porc_Parafiscal
    FIELD Cta2   LIKE Cfg_Novedades.Cta_Contrapartida.

DEFINE TEMP-TABLE TConta LIKE Mov_Contable.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME fMain
&Scoped-define BROWSE-NAME BConceptos

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES TConceptos Empleados TDeddev TEmpleados ~
TConta

/* Definitions for BROWSE BConceptos                                    */
&Scoped-define FIELDS-IN-QUERY-BConceptos TConceptos.Cod TConceptos.Nom TConceptos.Val   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BConceptos   
&Scoped-define SELF-NAME BConceptos
&Scoped-define QUERY-STRING-BConceptos FOR EACH TConceptos
&Scoped-define OPEN-QUERY-BConceptos OPEN QUERY {&SELF-NAME} FOR EACH TConceptos.
&Scoped-define TABLES-IN-QUERY-BConceptos TConceptos
&Scoped-define FIRST-TABLE-IN-QUERY-BConceptos TConceptos


/* Definitions for BROWSE BConEmpleados                                 */
&Scoped-define FIELDS-IN-QUERY-BConEmpleados Empleados.Nit WNom   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BConEmpleados   
&Scoped-define SELF-NAME BConEmpleados
&Scoped-define QUERY-STRING-BConEmpleados FOR EACH Empleados WHERE Empleados.Agencia EQ INTEGER(SUBSTRING(CAgencia, ~
      1, ~
      3))
&Scoped-define OPEN-QUERY-BConEmpleados OPEN QUERY {&SELF-NAME} FOR EACH Empleados WHERE Empleados.Agencia EQ INTEGER(SUBSTRING(CAgencia, ~
      1, ~
      3)).
&Scoped-define TABLES-IN-QUERY-BConEmpleados Empleados
&Scoped-define FIRST-TABLE-IN-QUERY-BConEmpleados Empleados


/* Definitions for BROWSE BDeducciones                                  */
&Scoped-define FIELDS-IN-QUERY-BDeducciones TDedDev.Cod TDedDev.Nom TDedDev.Val   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BDeducciones TDedDev.Val   
&Scoped-define ENABLED-TABLES-IN-QUERY-BDeducciones TDedDev
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BDeducciones TDedDev
&Scoped-define SELF-NAME BDeducciones
&Scoped-define QUERY-STRING-BDeducciones FOR EACH TDeddev WHERE TDeddev.Tip EQ 2
&Scoped-define OPEN-QUERY-BDeducciones OPEN QUERY {&SELF-NAME} FOR EACH TDeddev WHERE TDeddev.Tip EQ 2.
&Scoped-define TABLES-IN-QUERY-BDeducciones TDeddev
&Scoped-define FIRST-TABLE-IN-QUERY-BDeducciones TDeddev


/* Definitions for BROWSE BDevengados                                   */
&Scoped-define FIELDS-IN-QUERY-BDevengados TDedDev.Cod TDedDev.Nom TDedDev.Val   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BDevengados TDedDev.Val   
&Scoped-define ENABLED-TABLES-IN-QUERY-BDevengados TDedDev
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BDevengados TDedDev
&Scoped-define SELF-NAME BDevengados
&Scoped-define QUERY-STRING-BDevengados FOR EACH TDedDev WHERE TDeddev.Tip EQ 1
&Scoped-define OPEN-QUERY-BDevengados OPEN QUERY {&SELF-NAME} FOR EACH TDedDev WHERE TDeddev.Tip EQ 1.
&Scoped-define TABLES-IN-QUERY-BDevengados TDedDev
&Scoped-define FIRST-TABLE-IN-QUERY-BDevengados TDedDev


/* Definitions for BROWSE BEmpAgencia                                   */
&Scoped-define FIELDS-IN-QUERY-BEmpAgencia Empleados.Agencia Empleados.Nit WNom Empleados.Cargo WEst Empleados.PerNom_Actual   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BEmpAgencia   
&Scoped-define SELF-NAME BEmpAgencia
&Scoped-define QUERY-STRING-BEmpAgencia FOR EACH Empleados WHERE      Empleados.Agencia EQ INTEGER(SUBSTRING(CAgencia:SCREEN-VALUE, ~
      1, ~
      3))
&Scoped-define OPEN-QUERY-BEmpAgencia OPEN QUERY {&SELF-NAME} FOR EACH Empleados WHERE      Empleados.Agencia EQ INTEGER(SUBSTRING(CAgencia:SCREEN-VALUE, ~
      1, ~
      3)).
&Scoped-define TABLES-IN-QUERY-BEmpAgencia Empleados
&Scoped-define FIRST-TABLE-IN-QUERY-BEmpAgencia Empleados


/* Definitions for BROWSE BEmpleados                                    */
&Scoped-define FIELDS-IN-QUERY-BEmpleados TEmpleados.Age TEmpleados.Nit TEmpleados.Nom TEmpleados.Car TEmpleados.Est TEmpleados.Dia TEmpleados.TDv TEmpleados.TDd WDiferencia   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BEmpleados TEmpleados.Dia   
&Scoped-define ENABLED-TABLES-IN-QUERY-BEmpleados TEmpleados
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BEmpleados TEmpleados
&Scoped-define SELF-NAME BEmpleados
&Scoped-define QUERY-STRING-BEmpleados FOR EACH TEmpleados
&Scoped-define OPEN-QUERY-BEmpleados OPEN QUERY {&SELF-NAME} FOR EACH TEmpleados.
&Scoped-define TABLES-IN-QUERY-BEmpleados TEmpleados
&Scoped-define FIRST-TABLE-IN-QUERY-BEmpleados TEmpleados


/* Definitions for BROWSE BEspeciales                                   */
&Scoped-define FIELDS-IN-QUERY-BEspeciales TConceptos.Cod TConceptos.Nom   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BEspeciales   
&Scoped-define SELF-NAME BEspeciales
&Scoped-define QUERY-STRING-BEspeciales FOR EACH TConceptos
&Scoped-define OPEN-QUERY-BEspeciales OPEN QUERY {&SELF-NAME} FOR EACH TConceptos.
&Scoped-define TABLES-IN-QUERY-BEspeciales TConceptos
&Scoped-define FIRST-TABLE-IN-QUERY-BEspeciales TConceptos


/* Definitions for BROWSE BLiquida                                      */
&Scoped-define FIELDS-IN-QUERY-BLiquida TConta.Cuenta WDes TConta.Nit TConta.Db TConta.Cr   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BLiquida   
&Scoped-define SELF-NAME BLiquida
&Scoped-define QUERY-STRING-BLiquida FOR EACH TConta
&Scoped-define OPEN-QUERY-BLiquida OPEN QUERY {&SELF-NAME} FOR EACH TConta.
&Scoped-define TABLES-IN-QUERY-BLiquida TConta
&Scoped-define FIRST-TABLE-IN-QUERY-BLiquida TConta


/* Definitions for FRAME FConceptos                                     */
&Scoped-define OPEN-BROWSERS-IN-QUERY-FConceptos ~
    ~{&OPEN-QUERY-BConceptos}

/* Definitions for FRAME FConEmpleados                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-FConEmpleados ~
    ~{&OPEN-QUERY-BConEmpleados}

/* Definitions for FRAME FEmpAgencia                                    */
&Scoped-define OPEN-BROWSERS-IN-QUERY-FEmpAgencia ~
    ~{&OPEN-QUERY-BEmpAgencia}

/* Definitions for FRAME FEspeciales                                    */
&Scoped-define OPEN-BROWSERS-IN-QUERY-FEspeciales ~
    ~{&OPEN-QUERY-BEspeciales}

/* Definitions for FRAME FLiquidacion                                   */
&Scoped-define OPEN-BROWSERS-IN-QUERY-FLiquidacion ~
    ~{&OPEN-QUERY-BLiquida}

/* Definitions for FRAME fMain                                          */
&Scoped-define OPEN-BROWSERS-IN-QUERY-fMain ~
    ~{&OPEN-QUERY-BDeducciones}~
    ~{&OPEN-QUERY-BDevengados}~
    ~{&OPEN-QUERY-BEmpleados}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-11 Cmb_Inicio BUTTON-209 Btn_Prenomina ~
CAgencia BUTTON-210 BUTTON-213 BUTTON-208 CTipoPago CPeriodo BUTTON-40 ~
BtnDone RCuantos BEmpleados BDevengados BDeducciones 
&Scoped-Define DISPLAYED-OBJECTS Cmb_Inicio Fecha_Trabajo CAgencia ~
CTipoPago CPeriodo RCuantos NitW TE_Devengados TE_Deducciones TE_Diferencia ~
TTDev TTDed 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 Btn_Prenomina CAgencia Btn_Liquidar CTipoPago ~
CPeriodo BUTTON-40 RCuantos BEmpleados BDevengados BDeducciones 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Menu Definitions                                                     */
DEFINE MENU POPUP-MENU-BDeducciones 
       MENU-ITEM m_Adicionar_Deduccion LABEL "Adicionar Deduccion"
       MENU-ITEM m_Eliminar_Deduccion LABEL "Eliminar Deduccion"
       MENU-ITEM m_Salvar_Prenomina3 LABEL "Salvar Prenomina".

DEFINE MENU POPUP-MENU-BDevengados 
       MENU-ITEM m_Adicionar_Devengado LABEL "Adicionar Devengado"
       MENU-ITEM m_Eliminar_Devengado LABEL "Eliminar Devengado"
       MENU-ITEM m_Salvar_Prenomina2 LABEL "Salvar Prenomina".

DEFINE MENU POPUP-MENU-BEmpleados 
       MENU-ITEM m_Informacion_Empleado LABEL "Informacion Empleado"
       MENU-ITEM m_Eliminar_Empleado_de_la_lis LABEL "Eliminar Empleado de la lista"
       MENU-ITEM m_Adicionar_Concepto_a_Todos_ LABEL "Adicionar Concepto a Todos los empleados"
       MENU-ITEM m_Adicionar_un_empleado LABEL "Adicionar un Empleado"
       MENU-ITEM m_Salvar_Prenomina LABEL "Salvar Prenomina".


/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Adiciona 
     LABEL "Adicionar" 
     SIZE 15 BY 1.12.

DEFINE BUTTON BUTTON-37 
     LABEL "Ocultar" 
     SIZE 15 BY 1.12.

DEFINE BUTTON BUTTON-196 
     LABEL "Ocultar" 
     SIZE 15 BY 1.12.

DEFINE BUTTON BUTTON-212 
     LABEL "Ocultar" 
     SIZE 15 BY 1.12.

DEFINE BUTTON BUTTON-198 
     LABEL "Ocultar" 
     SIZE 15 BY 1.12.

DEFINE VARIABLE FecF AS DATE FORMAT "99/99/9999":U 
     LABEL "Hasta" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FecI AS DATE FORMAT "99/99/9999":U 
     LABEL "Desde" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-304
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 48 BY 1.62.

DEFINE BUTTON BUTTON-200 
     LABEL "Ocultar" 
     SIZE 10 BY .81.

DEFINE VARIABLE NEx AS DECIMAL FORMAT ">9.9":U INITIAL 0 
     LABEL "Número de Horas Extras" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE PorEx AS DECIMAL FORMAT "->>,>>9.9999":U INITIAL 0 
     LABEL "Porcentaje a usar" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE TEx AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "Valor una hora Extra" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE TotExt AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "Total Valor Extras" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE ValEx AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "Valor Hora este empleado" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE BUTTON BUTTON-186 
     LABEL "Ocultar" 
     SIZE 15 BY 1.12.

DEFINE VARIABLE SIE AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 67 BY 9.96
     BGCOLOR 15 FONT 2 NO-UNDO.

DEFINE BUTTON BUTTON-183 
     LABEL "Imprimir" 
     SIZE 15 BY 1.12.

DEFINE BUTTON BUTTON-184 
     LABEL "Ocultar" 
     SIZE 15 BY 1.12.

DEFINE VARIABLE RImpresion AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Prenomina", 1,
"Prenomina Detallado", 2,
"Tirillas de Pago", 3,
"Informes de Nómina", 4
     SIZE 20 BY 3.5 NO-UNDO.

DEFINE BUTTON BUTTON-193 
     LABEL "Ocultar" 
     SIZE 15 BY 1.12.

DEFINE BUTTON BUTTON-194 
     LABEL "Contabilizar Comprobante Prenomina" 
     SIZE 28 BY 1.12.

DEFINE BUTTON BUTTON-214 
     LABEL "Imprimir Precontabilidad" 
     SIZE 21 BY 1.12.

DEFINE VARIABLE TCre AS DECIMAL FORMAT "->>>,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 13.86 BY .81
     BGCOLOR 5 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE TDeb AS DECIMAL FORMAT "->>>,>>>,>>9":U INITIAL 0 
     LABEL "Totales" 
     VIEW-AS FILL-IN 
     SIZE 12.86 BY .81
     BGCOLOR 7 FGCOLOR 15  NO-UNDO.

DEFINE BUTTON BtnDone DEFAULT 
     LABEL "&Salir" 
     SIZE 17 BY 1.12
     BGCOLOR 8 .

DEFINE BUTTON Btn_Liquidar 
     LABEL "Liquidar" 
     SIZE 17 BY 1.12.

DEFINE BUTTON Btn_Prenomina 
     LABEL "Prenomina" 
     SIZE 17 BY 1.12.

DEFINE BUTTON BUTTON-208 
     LABEL "Configuracion Nomina" 
     SIZE 17 BY 1.12.

DEFINE BUTTON BUTTON-209 
     LABEL "Maestro Empleados" 
     SIZE 17 BY 1.12.

DEFINE BUTTON BUTTON-210 
     LABEL "E" 
     SIZE 3 BY .85.

DEFINE BUTTON BUTTON-213 
     LABEL "Consultas de Nomina" 
     SIZE 17 BY 1.12.

DEFINE BUTTON BUTTON-40 
     LABEL "Imprimir" 
     SIZE 17 BY 1.08.

DEFINE VARIABLE CAgencia AS CHARACTER FORMAT "X(256)":U 
     LABEL "Agencia" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 39 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Cmb_Inicio AS CHARACTER FORMAT "X(256)":U INITIAL "Nómina Normal" 
     LABEL "Acción" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Nómina Normal","Proceso Especial","Nómina Normal y Proceso Especial" 
     DROP-DOWN-LIST
     SIZE 39 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE CPeriodo AS CHARACTER FORMAT "X(256)":U INITIAL "0" 
     LABEL "Periodo" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 13 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE CTipoPago AS CHARACTER FORMAT "X(256)":U INITIAL "02 - Quincenal" 
     LABEL "Tipo de Pago" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "01 - Semanal","02 - Quincenal","03 - Mensual" 
     DROP-DOWN-LIST
     SIZE 19 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Fecha_Trabajo AS DATE FORMAT "99/99/9999":U 
     LABEL "Fecha" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE NitW AS CHARACTER FORMAT "X(14)":U 
     LABEL "Nit" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE TE_Deducciones AS DECIMAL FORMAT ">>>,>>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 12.57 BY .81
     BGCOLOR 5 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE TE_Devengados AS DECIMAL FORMAT ">>>,>>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 12.86 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE TE_Diferencia AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY .81
     BGCOLOR 3 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE TTDed AS DECIMAL FORMAT "->>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Total Deducciones" 
     VIEW-AS FILL-IN 
     SIZE 14.86 BY .81
     BGCOLOR 5 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE TTDev AS DECIMAL FORMAT "->>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Total Devengados" 
     VIEW-AS FILL-IN 
     SIZE 16 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE RCuantos AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Todos los Empleados de la Agencia", 1,
"Un Solo Empleado", 2
     SIZE 47 BY .81 NO-UNDO.

DEFINE RECTANGLE RECT-11
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 73 BY 1.35.

DEFINE BUTTON BUTTON-201 
     LABEL "Aceptar" 
     SIZE 15 BY 1.12.

DEFINE VARIABLE WKNit AS CHARACTER FORMAT "X(14)":U 
     VIEW-AS FILL-IN 
     SIZE 18 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE WkNom AS CHARACTER FORMAT "X(70)":U 
     VIEW-AS FILL-IN 
     SIZE 62 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE BUTTON Btn_Cargar 
     LABEL "Cargar" 
     SIZE 11 BY 1.12.

DEFINE BUTTON Btn_Generar 
     LABEL "Generar" 
     SIZE 11 BY 1.12.

DEFINE BUTTON Btn_OutPre 
     LABEL "Ocultar" 
     SIZE 11 BY 1.12.

DEFINE BUTTON Btn_SalvarPre 
     LABEL "Salvar" 
     SIZE 11 BY 1.12.

DEFINE BUTTON BUTTON-190 
     LABEL "Borrar" 
     SIZE 11 BY 1.12.

DEFINE RECTANGLE RECT-12
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 14 BY 5.65.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BConceptos FOR 
      TConceptos SCROLLING.

DEFINE QUERY BConEmpleados FOR 
      Empleados SCROLLING.

DEFINE QUERY BDeducciones FOR 
      TDeddev SCROLLING.

DEFINE QUERY BDevengados FOR 
      TDedDev SCROLLING.

DEFINE QUERY BEmpAgencia FOR 
      Empleados SCROLLING.

DEFINE QUERY BEmpleados FOR 
      TEmpleados SCROLLING.

DEFINE QUERY BEspeciales FOR 
      TConceptos SCROLLING.

DEFINE QUERY BLiquida FOR 
      TConta SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BConceptos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BConceptos wWin _FREEFORM
  QUERY BConceptos DISPLAY
      TConceptos.Cod
 TConceptos.Nom WIDTH 25
 TConceptos.Val
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 44 BY 13.19
         BGCOLOR 15 FONT 4 ROW-HEIGHT-CHARS .51 FIT-LAST-COLUMN.

DEFINE BROWSE BConEmpleados
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BConEmpleados wWin _FREEFORM
  QUERY BConEmpleados DISPLAY
      Empleados.Nit
 WNom COLUMN-LABEL "Nombre Empleado"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 54 BY 8.88
         BGCOLOR 15 FONT 4 FIT-LAST-COLUMN.

DEFINE BROWSE BDeducciones
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BDeducciones wWin _FREEFORM
  QUERY BDeducciones DISPLAY
      TDedDev.Cod
TDedDev.Nom COLUMN-LABEL "Descripción" WIDTH 30
TDedDev.Val COLUMN-LABEL "Valor"
            ENABLE TDedDev.Val
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 54 BY 4.85
         BGCOLOR 15 FONT 4 ROW-HEIGHT-CHARS .5 FIT-LAST-COLUMN.

DEFINE BROWSE BDevengados
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BDevengados wWin _FREEFORM
  QUERY BDevengados DISPLAY
      TDedDev.Cod
TDedDev.Nom COLUMN-LABEL "Descripción" WIDTH 30
TDedDev.Val COLUMN-LABEL "Valor"
ENABLE TDedDev.Val
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 54 BY 4.85
         BGCOLOR 15 FONT 4 ROW-HEIGHT-CHARS .5 FIT-LAST-COLUMN.

DEFINE BROWSE BEmpAgencia
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BEmpAgencia wWin _FREEFORM
  QUERY BEmpAgencia DISPLAY
      Empleados.Agencia COLUMN-LABEL "Age"
 Empleados.Nit 
 WNom FORMAT "X(35)" COLUMN-LABEL "Nombre del Empleado"
 Empleados.Cargo FORMAT "X(20)"
 WEst FORMAT "X(15)" COLUMN-LABEL "Estado"
 Empleados.PerNom_Actual COLUMN-LABEL "Nom"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 71 BY 8.08
         BGCOLOR 15 FONT 4 ROW-HEIGHT-CHARS .51 FIT-LAST-COLUMN.

DEFINE BROWSE BEmpleados
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BEmpleados wWin _FREEFORM
  QUERY BEmpleados DISPLAY
      TEmpleados.Age COLUMN-LABEL "Age"
      TEmpleados.Nit
      TEmpleados.Nom COLUMN-LABEL "Nombre del Empleado"
      TEmpleados.Car COLUMN-LABEL "Cargo Desempeñado" WIDTH 15
      TEmpleados.Est COLUMN-LABEL "EST"
      TEmpleados.Dia COLUMN-LABEL "Dias"
      TEmpleados.TDv WIDTH 12 COLUMN-LABEL "Devengados"
      TEmpleados.TDd WIDTH 12 COLUMN-LABEL "Deducciones"
      WDiferencia WIDTH 12 COLUMN-LABEL "Neto Pagar"
ENABLE TEmpleados.Dia
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 110 BY 6.73
         BGCOLOR 15 FONT 4 ROW-HEIGHT-CHARS .5 FIT-LAST-COLUMN.

DEFINE BROWSE BEspeciales
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BEspeciales wWin _FREEFORM
  QUERY BEspeciales DISPLAY
      TConceptos.Cod
 TConceptos.Nom
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS MULTIPLE SIZE 48 BY 6.19
         BGCOLOR 15 FONT 4 FIT-LAST-COLUMN.

DEFINE BROWSE BLiquida
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BLiquida wWin _FREEFORM
  QUERY BLiquida DISPLAY
      TConta.Cuenta COLUMN-LABEL "Cuenta"
 WDes          COLUMN-LABEL "Descripcion"
 TConta.Nit    COLUMN-LABEL "Nit"
 TConta.Db     COLUMN-LABEL "Debito"
 TConta.Cr     COLUMN-LABEL "Credito"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 83 BY 8.08
         BGCOLOR 15 FONT 4 ROW-HEIGHT-CHARS .5 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     Cmb_Inicio AT ROW 1.27 COL 11 COLON-ALIGNED
     Fecha_Trabajo AT ROW 1.27 COL 58 COLON-ALIGNED
     BUTTON-209 AT ROW 1.31 COL 77
     Btn_Prenomina AT ROW 1.31 COL 95
     CAgencia AT ROW 2.35 COL 11 COLON-ALIGNED
     BUTTON-210 AT ROW 2.35 COL 52.14
     BUTTON-213 AT ROW 2.88 COL 57
     BUTTON-208 AT ROW 2.88 COL 77
     Btn_Liquidar AT ROW 2.88 COL 95
     CTipoPago AT ROW 3.42 COL 11 COLON-ALIGNED
     CPeriodo AT ROW 3.42 COL 37 COLON-ALIGNED
     BUTTON-40 AT ROW 4.5 COL 77
     BtnDone AT ROW 4.5 COL 95
     RCuantos AT ROW 4.77 COL 5 NO-LABEL
     NitW AT ROW 4.77 COL 58 COLON-ALIGNED
     BEmpleados AT ROW 6.92 COL 3
     TE_Devengados AT ROW 13.73 COL 82.86 RIGHT-ALIGNED NO-LABEL
     TE_Deducciones AT ROW 13.73 COL 95.43 RIGHT-ALIGNED NO-LABEL
     TE_Diferencia AT ROW 13.73 COL 94.43 COLON-ALIGNED NO-LABEL
     BDevengados AT ROW 15.73 COL 3
     BDeducciones AT ROW 15.73 COL 59
     TTDev AT ROW 20.62 COL 54 RIGHT-ALIGNED
     TTDed AT ROW 20.62 COL 94 COLON-ALIGNED
     "  Empleados de la Agencia" VIEW-AS TEXT
          SIZE 110 BY .81 AT ROW 6.12 COL 3
          BGCOLOR 18 FGCOLOR 15 FONT 1
     "  Deducciones del periodo" VIEW-AS TEXT
          SIZE 54 BY .81 AT ROW 14.92 COL 59
          BGCOLOR 18 FGCOLOR 15 FONT 1
     "  Devengados del periodo" VIEW-AS TEXT
          SIZE 54 BY .81 AT ROW 14.92 COL 3
          BGCOLOR 18 FGCOLOR 15 FONT 1
     RECT-11 AT ROW 4.5 COL 3
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 113.86 BY 21.42
         BGCOLOR 17 FONT 4
         DEFAULT-BUTTON BtnDone.

DEFINE FRAME FConEmpleados
     BConEmpleados AT ROW 1.27 COL 3
     BUTTON-196 AT ROW 10.42 COL 41
     "Haga doble click, para escoger un empleado" VIEW-AS TEXT
          SIZE 37 BY 1.08 AT ROW 10.42 COL 4
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 23 ROW 7.19
         SIZE 58 BY 11.85
         BGCOLOR 17 FONT 4
         TITLE "Consulta de Empleados".

DEFINE FRAME FNitContable
     WKNit AT ROW 1.27 COL 43 COLON-ALIGNED NO-LABEL
     WkNom AT ROW 2.35 COL 2 NO-LABEL
     BUTTON-201 AT ROW 3.42 COL 25
     " Digite el NIT de la Entidad Prestadora de Salud" VIEW-AS TEXT
          SIZE 42 BY .81 AT ROW 1.27 COL 2
          BGCOLOR 17 FGCOLOR 7 FONT 5
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 27 ROW 11.23
         SIZE 65 BY 4.58
         BGCOLOR 17 FONT 4
         TITLE "Registro Contable".

DEFINE FRAME FEspeciales
     FecI AT ROW 2.08 COL 10 COLON-ALIGNED
     FecF AT ROW 2.08 COL 31 COLON-ALIGNED
     BEspeciales AT ROW 3.69 COL 2
     BUTTON-198 AT ROW 10.15 COL 34
     " Elija el rango de fechas para el proceso especial" VIEW-AS TEXT
          SIZE 43 BY .81 AT ROW 1.27 COL 3
          FGCOLOR 7 FONT 5
     RECT-304 AT ROW 1.54 COL 2
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 29 ROW 6.12
         SIZE 50 BY 11.58
         BGCOLOR 17 FONT 4
         TITLE "Proceso Especiales Disponibles".

DEFINE FRAME FIE
     SIE AT ROW 1.27 COL 3 NO-LABEL
     BUTTON-186 AT ROW 11.5 COL 55
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 23 ROW 6.12
         SIZE 71 BY 12.92
         BGCOLOR 17 FONT 4
         TITLE "Información Empleados".

DEFINE FRAME FEmpAgencia
     BEmpAgencia AT ROW 1.54 COL 3
     BUTTON-212 AT ROW 9.88 COL 59
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 11 ROW 6.12
         SIZE 75 BY 11.04
         BGCOLOR 17 FONT 4
         TITLE "".

DEFINE FRAME FLiquidacion
     BLiquida AT ROW 1.27 COL 3
     TDeb AT ROW 9.35 COL 55 COLON-ALIGNED
     TCre AT ROW 9.35 COL 68 COLON-ALIGNED NO-LABEL
     BUTTON-214 AT ROW 10.42 COL 3
     BUTTON-194 AT ROW 10.42 COL 25
     BUTTON-193 AT ROW 10.69 COL 69
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 14 ROW 6.12
         SIZE 87 BY 11.85
         BGCOLOR 17 FONT 4
         TITLE "Partidas a Contabilizar".

DEFINE FRAME FPrenomina
     Btn_Generar AT ROW 1.54 COL 3.57
     Btn_SalvarPre AT ROW 2.88 COL 3.57
     Btn_Cargar AT ROW 4.23 COL 3.57
     BUTTON-190 AT ROW 5.58 COL 3.57
     Btn_OutPre AT ROW 7.19 COL 3.57
     RECT-12 AT ROW 1.27 COL 2
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 96 ROW 1.27
         SIZE 16.43 BY 8.62
         BGCOLOR 17 FONT 4
         TITLE "Prenomina".

DEFINE FRAME FImpresion
     RImpresion AT ROW 1.27 COL 4 NO-LABEL
     BUTTON-183 AT ROW 1.81 COL 27
     BUTTON-184 AT ROW 3.42 COL 27
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 35 ROW 8.54
         SIZE 43 BY 4.85
         BGCOLOR 17 FONT 4
         TITLE "Opciones de Impresión".

DEFINE FRAME FExtras
     NEx AT ROW 1.27 COL 19 COLON-ALIGNED
     ValEx AT ROW 2.35 COL 19 COLON-ALIGNED
     PorEx AT ROW 3.42 COL 19 COLON-ALIGNED
     TEx AT ROW 4.5 COL 19 COLON-ALIGNED
     TotExt AT ROW 5.58 COL 19 COLON-ALIGNED
     BUTTON-200 AT ROW 6.65 COL 21
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 42 ROW 8.27
         SIZE 32 BY 7.54
         BGCOLOR 8 FONT 4
         TITLE "Horas Extras".

DEFINE FRAME FConceptos
     BConceptos AT ROW 1.27 COL 3
     Btn_Adiciona AT ROW 14.73 COL 16
     BUTTON-37 AT ROW 14.73 COL 32
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 17 ROW 5.85
         SIZE 48 BY 15.88
         BGCOLOR 17 FONT 4
         TITLE "Conceptos Disponibles".


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
         TITLE              = "Pago de Nómina"
         HEIGHT             = 21.42
         WIDTH              = 113.86
         MAX-HEIGHT         = 28.81
         MAX-WIDTH          = 146.29
         VIRTUAL-HEIGHT     = 28.81
         VIRTUAL-WIDTH      = 146.29
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
/* REPARENT FRAME */
ASSIGN FRAME FConceptos:FRAME = FRAME fMain:HANDLE
       FRAME FConEmpleados:FRAME = FRAME fMain:HANDLE
       FRAME FEmpAgencia:FRAME = FRAME fMain:HANDLE
       FRAME FEspeciales:FRAME = FRAME fMain:HANDLE
       FRAME FExtras:FRAME = FRAME fMain:HANDLE
       FRAME FIE:FRAME = FRAME fMain:HANDLE
       FRAME FImpresion:FRAME = FRAME fMain:HANDLE
       FRAME FLiquidacion:FRAME = FRAME fMain:HANDLE
       FRAME FNitContable:FRAME = FRAME fMain:HANDLE
       FRAME FPrenomina:FRAME = FRAME fMain:HANDLE.

/* SETTINGS FOR FRAME FConceptos
   NOT-VISIBLE                                                          */
/* BROWSE-TAB BConceptos 1 FConceptos */
ASSIGN 
       FRAME FConceptos:HIDDEN           = TRUE
       FRAME FConceptos:MOVABLE          = TRUE.

/* SETTINGS FOR FRAME FConEmpleados
   NOT-VISIBLE                                                          */
/* BROWSE-TAB BConEmpleados TEXT-6 FConEmpleados */
ASSIGN 
       FRAME FConEmpleados:HIDDEN           = TRUE.

/* SETTINGS FOR FRAME FEmpAgencia
   NOT-VISIBLE                                                          */
/* BROWSE-TAB BEmpAgencia 1 FEmpAgencia */
ASSIGN 
       FRAME FEmpAgencia:HIDDEN           = TRUE
       FRAME FEmpAgencia:MOVABLE          = TRUE.

/* SETTINGS FOR FRAME FEspeciales
   NOT-VISIBLE                                                          */
/* BROWSE-TAB BEspeciales FecF FEspeciales */
ASSIGN 
       FRAME FEspeciales:HIDDEN           = TRUE.

/* SETTINGS FOR FRAME FExtras
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME FExtras:HIDDEN           = TRUE
       FRAME FExtras:MOVABLE          = TRUE.

/* SETTINGS FOR FILL-IN PorEx IN FRAME FExtras
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN TEx IN FRAME FExtras
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN TotExt IN FRAME FExtras
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ValEx IN FRAME FExtras
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME FIE
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME FIE:HIDDEN           = TRUE.

/* SETTINGS FOR FRAME FImpresion
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME FImpresion:HIDDEN           = TRUE.

/* SETTINGS FOR FRAME FLiquidacion
   NOT-VISIBLE                                                          */
/* BROWSE-TAB BLiquida 1 FLiquidacion */
ASSIGN 
       FRAME FLiquidacion:HIDDEN           = TRUE
       FRAME FLiquidacion:MOVABLE          = TRUE.

ASSIGN 
       BLiquida:SEPARATOR-FGCOLOR IN FRAME FLiquidacion      = 8.

/* SETTINGS FOR FILL-IN TCre IN FRAME FLiquidacion
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN TDeb IN FRAME FLiquidacion
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME fMain
                                                                        */
/* BROWSE-TAB BEmpleados FEspeciales fMain */
/* BROWSE-TAB BDevengados TE_Diferencia fMain */
/* BROWSE-TAB BDeducciones BDevengados fMain */
/* SETTINGS FOR BROWSE BDeducciones IN FRAME fMain
   1                                                                    */
ASSIGN 
       BDeducciones:POPUP-MENU IN FRAME fMain             = MENU POPUP-MENU-BDeducciones:HANDLE.

/* SETTINGS FOR BROWSE BDevengados IN FRAME fMain
   1                                                                    */
ASSIGN 
       BDevengados:POPUP-MENU IN FRAME fMain             = MENU POPUP-MENU-BDevengados:HANDLE.

/* SETTINGS FOR BROWSE BEmpleados IN FRAME fMain
   1                                                                    */
ASSIGN 
       BEmpleados:POPUP-MENU IN FRAME fMain             = MENU POPUP-MENU-BEmpleados:HANDLE.

/* SETTINGS FOR BUTTON Btn_Liquidar IN FRAME fMain
   NO-ENABLE 1                                                          */
/* SETTINGS FOR BUTTON Btn_Prenomina IN FRAME fMain
   1                                                                    */
/* SETTINGS FOR BUTTON BUTTON-40 IN FRAME fMain
   1                                                                    */
/* SETTINGS FOR COMBO-BOX CAgencia IN FRAME fMain
   1                                                                    */
/* SETTINGS FOR COMBO-BOX CPeriodo IN FRAME fMain
   1                                                                    */
/* SETTINGS FOR COMBO-BOX CTipoPago IN FRAME fMain
   1                                                                    */
/* SETTINGS FOR FILL-IN Fecha_Trabajo IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN NitW IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR RADIO-SET RCuantos IN FRAME fMain
   1                                                                    */
/* SETTINGS FOR FILL-IN TE_Deducciones IN FRAME fMain
   NO-ENABLE ALIGN-R                                                    */
/* SETTINGS FOR FILL-IN TE_Devengados IN FRAME fMain
   NO-ENABLE ALIGN-R                                                    */
/* SETTINGS FOR FILL-IN TE_Diferencia IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN TTDed IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN TTDev IN FRAME fMain
   NO-ENABLE ALIGN-R                                                    */
/* SETTINGS FOR FRAME FNitContable
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME FNitContable:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN WkNom IN FRAME FNitContable
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FRAME FPrenomina
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME FPrenomina:HIDDEN           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BConceptos
/* Query rebuild information for BROWSE BConceptos
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH TConceptos.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BConceptos */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BConEmpleados
/* Query rebuild information for BROWSE BConEmpleados
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH Empleados WHERE Empleados.Agencia EQ INTEGER(SUBSTRING(CAgencia,1,3)).
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BConEmpleados */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BDeducciones
/* Query rebuild information for BROWSE BDeducciones
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH TDeddev WHERE TDeddev.Tip EQ 2.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BDeducciones */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BDevengados
/* Query rebuild information for BROWSE BDevengados
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH TDedDev WHERE TDeddev.Tip EQ 1.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BDevengados */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BEmpAgencia
/* Query rebuild information for BROWSE BEmpAgencia
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH Empleados WHERE
     Empleados.Agencia EQ INTEGER(SUBSTRING(CAgencia:SCREEN-VALUE,1,3)).
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BEmpAgencia */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BEmpleados
/* Query rebuild information for BROWSE BEmpleados
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH TEmpleados.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BEmpleados */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BEspeciales
/* Query rebuild information for BROWSE BEspeciales
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH TConceptos.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BEspeciales */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BLiquida
/* Query rebuild information for BROWSE BLiquida
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH TConta.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BLiquida */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FConceptos
/* Query rebuild information for FRAME FConceptos
     _Query            is NOT OPENED
*/  /* FRAME FConceptos */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FEmpAgencia
/* Query rebuild information for FRAME FEmpAgencia
     _Query            is NOT OPENED
*/  /* FRAME FEmpAgencia */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FIE
/* Query rebuild information for FRAME FIE
     _Query            is NOT OPENED
*/  /* FRAME FIE */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FNitContable
/* Query rebuild information for FRAME FNitContable
     _Query            is NOT OPENED
*/  /* FRAME FNitContable */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Pago de Nómina */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Pago de Nómina */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BConceptos
&Scoped-define FRAME-NAME FConceptos
&Scoped-define SELF-NAME BConceptos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BConceptos wWin
ON MOUSE-SELECT-CLICK OF BConceptos IN FRAME FConceptos
DO:
  ASSIGN Nex:SCREEN-VALUE IN FRAME FExtras = "0"
         Tex:SCREEN-VALUE IN FRAME FExtras = "0"
         PorEx:SCREEN-VALUE IN FRAME FExtras = "0"
         TotExt:SCREEN-VALUE IN FRAME FExtras = "0".
  IF TEmpleados.IMa AND TConceptos.IEx THEN DO:
     MESSAGE "Este empleados es de Manejo y Confianza" SKIP
             "no se aplica el pago de horas extras" SKIP
             "a este tipo de empleado." VIEW-AS ALERT-BOX ERROR.
     DISABLE Btn_Adiciona WITH FRAME FConceptos.
  END.
  ELSE DO:
      ENABLE Btn_Adiciona WITH FRAME FConceptos.
      IF TConceptos.IEx THEN DO: /*para pedir numero de horas extras que trabajo*/
        IF TEmpleados.Bas LT WSmlv THEN
           ASSIGN ValEx = WSmlv / 30 / 8.
        ELSE 
           ASSIGN ValEx = TEmpleados.Bas / 30 / 8.
        ASSIGN PorEx = TConceptos.Val
               TEx   = ValEx + (ValEx * PorEx)
               FRAME FExtras:TITLE = TConceptos.Nom.
        DISPLAY ValEx PorEx TEx WITH FRAME FExtras.
        VIEW FRAME FExtras.
        APPLY "entry" TO NEx IN FRAME FExtras.
        RETURN NO-APPLY.
      END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BConEmpleados
&Scoped-define FRAME-NAME FConEmpleados
&Scoped-define SELF-NAME BConEmpleados
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BConEmpleados wWin
ON MOUSE-SELECT-DBLCLICK OF BConEmpleados IN FRAME FConEmpleados
DO:
  /*CREATE Templeados.
  ASSIGN TEmpleados.Age  = INTEGER(SUBSTRING(CAgencia,1,3))
         TEmpleados.Nit  = Clientes.Nit
         TEmpleados.Nom  = Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2
         TEmpleados.Car  = Empleados.Cargo
         TEmpleados.Est  = Empleados.Estado
         TEmpleados.TDv  = 0
         TEmpleados.TDd  = 0.*/
/*         TEmpleados.Fec  = Novedades_Nomina.Fecha
         TEmpleados.Dia  = Novedades_Nomina.Dias_Trabajados
         TEmpleados.Bas  = Empleados.Salario_Mensual.*/
  NitW:SCREEN-VALUE IN FRAME FMain = Empleados.Nit.
  HIDE FRAME FConEmpleados.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BConEmpleados wWin
ON ROW-DISPLAY OF BConEmpleados IN FRAME FConEmpleados
DO:
  FIND Clientes WHERE Clientes.Nit EQ Empleados.Nit NO-LOCK NO-ERROR.
  WNom = "Empleado no Existe en Clientes".
  IF AVAILABLE Clientes THEN
     WNom = Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BDeducciones
&Scoped-define FRAME-NAME fMain
&Scoped-define SELF-NAME BDeducciones
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BDeducciones wWin
ON ROW-DISPLAY OF BDeducciones IN FRAME fMain
DO:
  IF TDedDev.Ipa THEN
     ASSIGN TDedDev.Cod:FGCOL IN BROWSE BDeducciones = 15
            TDedDev.Nom:FGCOL = 15 
            TDedDev.Val:FGCOL = 15.
  ELSE
     ASSIGN TDedDev.Cod:FGCOL = 0
            TDedDev.Nom:FGCOL = 0 
            TDedDev.Val:FGCOL = 0.
  ASSIGN TTDed = TTDed + TDedDev.Val.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BEmpAgencia
&Scoped-define FRAME-NAME FEmpAgencia
&Scoped-define SELF-NAME BEmpAgencia
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BEmpAgencia wWin
ON ROW-DISPLAY OF BEmpAgencia IN FRAME FEmpAgencia
DO:
  WNom = "No Existe en Clientes".
  FIND Clientes WHERE Clientes.Nit EQ Empleados.Nit NO-LOCK NO-ERROR.
  IF AVAILABLE Clientes THEN WNom = Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2.
  CASE Empleados.Estado:
      WHEN 1 THEN WEst = "Activo".
      WHEN 2 THEN WEst = "Retirado".
      WHEN 3 THEN WEst = "Lic. Maternidad".
      WHEN 4 THEN WEst = "Lic. Remunerada".
      WHEN 5 THEN WEst = "Lic. No Remunerada".
      WHEN 6 THEN WEst = "Vacaciones".
      WHEN 7 THEN WEst = "Comisión".
      WHEN 8 THEN WEst = "Calamidad Doméstica".
      WHEN 9 THEN WEst = "Incapacidad".
      WHEN 10 THEN WEst = "Suspendido".
  END CASE.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BEmpleados
&Scoped-define FRAME-NAME fMain
&Scoped-define SELF-NAME BEmpleados
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BEmpleados wWin
ON MOUSE-SELECT-CLICK OF BEmpleados IN FRAME fMain
DO:
  OPEN QUERY BDevengados FOR EACH TDedDev WHERE TDedDev.Nit EQ TEmpleados.Nit AND TDeddev.Tip EQ 1 BY TDedDev.IBa DESCENDING.
  FOR EACH TDedDev WHERE TDedDev.Nit EQ TEmpleados.Nit AND TDeddev.Tip EQ 2:
      ASSIGN WDiferencia = TEmpleados.TDv - TEmpleados.TDd.
  END.
  OPEN QUERY BDeducciones FOR EACH TDedDev WHERE TDedDev.Nit EQ TEmpleados.Nit AND TDeddev.Tip EQ 2 BY TDedDev.Ipa.
  RUN TotalesEmpleado.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BEmpleados wWin
ON ROW-DISPLAY OF BEmpleados IN FRAME fMain
DO:
  ASSIGN WDiferencia = TEmpleados.TDv - TEmpleados.TDd.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BLiquida
&Scoped-define FRAME-NAME FLiquidacion
&Scoped-define SELF-NAME BLiquida
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BLiquida wWin
ON ROW-DISPLAY OF BLiquida IN FRAME FLiquidacion
DO:
  ASSIGN WDes = TConta.Comentario.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fMain
&Scoped-define SELF-NAME BtnDone
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnDone wWin
ON CHOOSE OF BtnDone IN FRAME fMain /* Salir */
DO:
  ON RETURN RETURN.
        
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


&Scoped-define FRAME-NAME FConceptos
&Scoped-define SELF-NAME Btn_Adiciona
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Adiciona wWin
ON CHOOSE OF Btn_Adiciona IN FRAME FConceptos /* Adicionar */
DO:
 DEFINE VAR WBase LIKE Empleados.Salario.
 DEFINE VAR WMod AS LOGICAL INITIAL NO.
 DEFINE VAR ValExtras AS DECIMAL FORMAT ">>>,>>>,>>9.99".
 DEFINE VAR WConDv AS INTEGER FORMAT "99".
 DEFINE VAR WConDd AS INTEGER FORMAT "99".
 DEFINE VAR i AS INTEGER FORMAT "99" INITIAL 0.
 ASSIGN TE_Devengados = 0 TE_Deducciones = 0 TE_Diferencia = 0.
 FOR EACH TDedDev WHERE TDedDev.Tip EQ 1 AND TDedDev.Nit EQ TEmpleados.Nit: WConDv = WConDv + 1. END.
 FOR EACH TDedDev WHERE TDedDev.Tip EQ 2 AND TDedDev.Nit EQ TEmpleados.Nit: WConDd = WConDd + 1. END.
 IF WTipo EQ 1 OR WTipo EQ 2 THEN DO: /*adiciona concepto devengado*/
     IF AVAILABLE TConceptos THEN DO:
        IF TConceptos.Cod EQ 117 AND WkNit EQ "" THEN DO:
            VIEW FRAME FNitContable.
            APPLY "entry" TO WkNit.
            RETURN NO-APPLY.
        END.
        CREATE TDedDev.
        ASSIGN TDedDev.Age = TEmpleados.Age 
               TDedDev.Tip = TConceptos.Tip
               TDedDev.Cod = TConceptos.Cod
               TDedDev.Nom = TConceptos.Nom
               TDedDev.Nit = TEmpleados.Nit
               TDedDev.Cta = TConceptos.Cta
               TDedDev.Nat = TConceptos.Nat
               TDedDev.Fec = Fecha_Trabajo
               TDedDev.Cla = TConceptos.Cla
               TDedDev.IMo = TConceptos.Idv
               TDedDev.IEx = TConceptos.IEx
               TDedDev.ISa = TConceptos.ISa
               TDedDev.NmE = Nex
               TDedDev.VrE = Tex
               TDedDev.Nco = WkNit.
        WkNit = "".
        IF TDedDev.Nme NE 0 THEN TDedDev.Nom + " (" + STRING(TDedDev.Nme) + ")".
        IF TConceptos.Cla NE 6 THEN DO:
            IF NOT TConceptos.IEx THEN DO:
                CASE TConceptos.Idv:
                    WHEN 1 THEN DO:
                        WBase = TEmpleados.Bas.
                        IF Cfg_Novedades.Clase EQ 5 AND WBase LT WSMLV THEN
                           WBase = WSMLV.

                        IF TConceptos.ISm THEN DO:
                            IF TEmpleados.Bas GT WSMLV * TConceptos.NSm THEN
                               ASSIGN TDedDev.Val = ((WBase / 30) * TEmpleados.Dia)
                                      TDedDev.Val = TDedDev.Val * ((TConceptos.Val + TConceptos.Pad) / 100).
                            ELSE
                               ASSIGN TDedDev.Val = ((WBase / 30) * TEmpleados.Dia)
                                      TDedDev.Val = TDedDev.Val * ((TConceptos.Val) / 100).
                        END.
                        ELSE TDedDev.Val = ((WBase / 30) * TEmpleados.Dia) * (TConceptos.Val / 100).
                    END.
                    WHEN 2 THEN TDedDev.Val = TConceptos.Val.
                    WHEN 3 THEN ASSIGN WMod = YES
                                       TDedDev.Val = TConceptos.Val.
                END CASE.
            END.
            ELSE TDedDev.Val = TDedDev.NmE * TDedDev.VrE.
        END.
        ELSE DO:
           RUN VALUE(TConceptos.Prg) (INPUT TEmpleados.Nit, INPUT FecI, INPUT FecF, OUTPUT WVal).
           TDedDev.Val = WVal.
        END.
     END.
     W_Puntero2 = ROWID(TDedDev).
     IF WTipo EQ 1 THEN DO:
         OPEN QUERY BDevengados FOR EACH TDedDev WHERE TDedDev.Nit EQ TEmpleados.Nit AND TDeddev.Tip EQ 1 BY TDedDev.Cod.
         ASSIGN TEmpleados.TDv = 0
                TE_Devengados  = 0.
         FOR EACH TDedDev WHERE TDedDev.Nit EQ TEmpleados.Nit AND TDedDev.Tip EQ 1:
             ASSIGN TEmpleados.TDv = TEmpleados.TDv + TDedDev.Val.
         END.
         WFila = Templeados.Lin.
         W_Puntero = ROWID(TEmpleados).
         RUN Recalculo.             
         DISPLAY TE_Devengados WITH FRAME FMain.
         IF WMod THEN DO:
            ENABLE BROWSE BDevengados TDedDev.Val.
            REPOSITION BDevengados TO ROWID W_Puntero2.
            /*w_ok = BROWSE BDevengados:SELECT-ROW(WConDv + 1).*/
            APPLY "entry" TO TDedDev.Val IN BROWSE BDevengados.
         END.
     END.
     IF WTipo EQ 2 THEN DO:
         OPEN QUERY BDeducciones FOR EACH TDedDev WHERE TDedDev.Nit EQ TEmpleados.Nit AND TDeddev.Tip EQ 2.
         ASSIGN TEmpleados.TDd = 0
                TE_Deducciones  = 0.
         FOR EACH TDedDev WHERE TDedDev.Nit EQ TEmpleados.Nit AND TDedDev.Tip EQ 2:
             ASSIGN TEmpleados.TDd = TEmpleados.TDd + TDedDev.Val.
         END.
         WFila = Templeados.Lin.
         W_Puntero = ROWID(TEmpleados).
         RUN Recalculo.
         DISPLAY TE_Deducciones WITH FRAME FMain.
         IF WMod THEN DO:
            ENABLE BROWSE BDeducciones TDedDev.Val.
            REPOSITION BDeducciones TO ROWID W_Puntero2.
            /*w_ok = BROWSE BDeducciones:SELECT-ROW(WConDd).*/
            APPLY "entry" TO TDedDev.Val IN BROWSE BDeducciones.
         END.
     END.
     HIDE FRAME FConceptos.
 END.
 IF WTipo EQ 3 THEN DO:
     FOR EACH TEmpleados:
         CREATE TDedDev.
         ASSIGN TDedDev.Age = TEmpleados.Age 
                TDedDev.Tip = TConceptos.Tip
                TDedDev.Cod = TConceptos.Cod
                TDedDev.Nom = TConceptos.Nom
                TDedDev.Nit = TEmpleados.Nit
                TDedDev.Cta = TConceptos.Cta
                TDedDev.Nat = TConceptos.Nat
                TDedDev.Fec = Fecha_Trabajo
                TDedDev.IMo = TConceptos.Idv
                TDedDev.IEx = TConceptos.IEx
                TDedDev.ISa = TConceptos.ISa
                TDedDev.NmE = Nex
                TDedDev.VrE = Tex.
         IF TConceptos.Cla NE 6 THEN DO:
             CASE TConceptos.Idv:
                 WHEN 1 THEN DO: 
                     WBase = TEmpleados.Bas.
                     IF Cfg_Novedades.Clase EQ 5 AND WBase LT WSMLV THEN
                        WBase = WSMLV.
                     IF TConceptos.ISm THEN DO:
                         IF TEmpleados.Bas GT WSMLV * TConceptos.NSm THEN
                            TDedDev.Val = ((WBase / 30) * TEmpleados.Dia) * 
                                          ((TConceptos.Val + TConceptos.Pad) / 100).
                         ELSE
                            TDedDev.Val = ((WBase / 30) * TEmpleados.Dia) * 
                                           ((TConceptos.Val) / 100).
                     END.
                     ELSE TDedDev.Val = ((WBase / 30) * TEmpleados.Dia) * (TConceptos.Val / 100).
                 END.
                 WHEN 2 THEN TDedDev.Val = TConceptos.Val.
                 WHEN 3 THEN WMod = YES.
             END CASE.
         END.
         ELSE DO:
            RUN VALUE(TConceptos.Prg) (INPUT TEmpleados.Nit, INPUT FecI, INPUT FecF, OUTPUT WVal).
            TDedDev.Val = WVal.
         END.
         ASSIGN TEmpleados.TDd = 0
                TEmpleados.TDv = 0
                TE_Deducciones = 0
                TE_Devengados  = 0.
         FOR EACH TDedDev WHERE TDedDev.Nit EQ TEmpleados.Nit AND TDedDev.Tip EQ 2:
             ASSIGN TEmpleados.TDd = TEmpleados.TDd + TDedDev.Val.
         END.
         FOR EACH TDedDev WHERE TDedDev.Nit EQ TEmpleados.Nit AND TDedDev.Tip EQ 1:
             ASSIGN TEmpleados.TDv = TEmpleados.TDv + TDedDev.Val.
         END.
         WFila = Templeados.Lin.
         W_Puntero = ROWID(TEmpleados).
         RUN Recalculo.
     END.
     OPEN QUERY BDevengados FOR EACH TDedDev WHERE TDedDev.Nit EQ TEmpleados.Nit AND TDeddev.Tip EQ 1.
     OPEN QUERY BDeducciones FOR EACH TDedDev WHERE TDedDev.Nit EQ TEmpleados.Nit AND TDeddev.Tip EQ 2.
     DISPLAY TE_Devengados TE_Deducciones WITH FRAME FMain.
     HIDE FRAME FConceptos.
     ASSIGN NEx:SCREEN-VALUE IN FRAME FExtras   = "0.00"
            TEx:SCREEN-VALUE IN FRAME FExtras   = "0.00"
            PorEx:SCREEN-VALUE IN FRAME FExtras = "0.00"
            ValEx:SCREEN-VALUE IN FRAME FExtras = "0.00".
 END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FPrenomina
&Scoped-define SELF-NAME Btn_Cargar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cargar wWin
ON CHOOSE OF Btn_Cargar IN FRAME FPrenomina /* Cargar */
DO:
  ASSIGN FRAME FMain CAgencia CPeriodo RCuantos.
  IF RCuantos EQ 1 THEN
     FIND FIRST Novedades_Nomina WHERE
                Novedades_Nomina.Agencia   EQ INTEGER(SUBSTRING(CAgencia,1,3)) AND
                Novedades_Nomina.PerNomina EQ INTEGER(CPeriodo) AND
                Novedades_Nomina.Estado_Liquidacion EQ 1 NO-LOCK NO-ERROR.
  ELSE
     FIND FIRST Novedades_Nomina WHERE
                Novedades_Nomina.Agencia   EQ INTEGER(SUBSTRING(CAgencia,1,3)) AND
                Novedades_Nomina.Nit       EQ NitW AND
                Novedades_Nomina.PerNomina EQ INTEGER(CPeriodo) NO-LOCK NO-ERROR.
  IF NOT AVAILABLE Novedades_Nomina THEN
     MESSAGE "No existe una prenomina con la misma agencia y Periodo" SKIP
             "debera generarse una nueva prenomina." VIEW-AS ALERT-BOX INFORMATION.
  ELSE DO:
      IF Novedades_Nomina.Estado_Liquidacion NE 1 THEN DO:
         MESSAGE "Esta prenomina ya ha sido liquidada!" SKIP
                 "no podra ser modificada ni liquidada" SKIP
                 "de nuevo."  VIEW-AS ALERT-BOX ERROR.
         WHistorico = YES.
      END.
      ELSE DO:
          FOR EACH TEmpleados: DELETE TEmpleados. END.
          FOR EACH TDedDev:    DELETE TDedDev. END.
          ASSIGN TE_Devengados = 0 TE_Deducciones = 0.
          RUN Cargar.
          OPEN QUERY BEmpleados FOR EACH TEmpleados.
          OPEN QUERY BDevengados FOR EACH TDedDev WHERE TDedDev.Nit EQ TEmpleados.Nit AND TDeddev.Tip EQ 1 INDEXED-REPOSITION.
          OPEN QUERY BDeducciones FOR EACH TDedDev WHERE TDedDev.Nit EQ TEmpleados.Nit AND TDeddev.Tip EQ 2 BY TDedDev.IPa INDEXED-REPOSITION.
      END.
  END.
  RUN TotalesEmpleado.
  ENABLE Btn_Liquidar WITH FRAME Fmain.
  HIDE FRAME FPrenomina.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Generar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Generar wWin
ON CHOOSE OF Btn_Generar IN FRAME FPrenomina /* Generar */
DO:

  FOR EACH TEmpleados: DELETE TEmpleados. END.
  FOR EACH TDedDev:    DELETE TDedDev. END.
  WHistorico = NO.
  ASSIGN FRAME Fmain Cagencia CPeriodo RCuantos NitW CTipoPago RCuantos NitW.
  ASSIGN TE_Devengados = 0 TE_Deducciones = 0 TE_Diferencia = 0.
  IF CAgencia EQ "" OR CPeriodo EQ "" THEN DO:
     MESSAGE "Para generar una prenomina deben ser ingresados" SKIP
             "los datos de Agencia y Periodo de Pago" VIEW-AS ALERT-BOX ERROR.
     APPLY "entry" TO Cagencia.
     RETURN NO-APPLY.
  END.
  IF RCuantos EQ 1 THEN
     FOR EACH TEmpleados: DELETE TEmpleados. END.
  ELSE DO:
     IF NitW EQ "" THEN DO:
        MESSAGE "No se puede hacer una nomina para el nit en blancos" SKIP
                "rectifique, digite la id. del empleado" VIEW-AS ALERT-BOX.
        APPLY "entry" TO NitW IN FRAME FMain.
        RETURN NO-APPLY.
     END.
  END.
  IF RCuantos EQ 1 THEN 
     FIND FIRST Novedades_Nomina WHERE
                Novedades_Nomina.Agencia   EQ INTEGER(SUBSTRING(CAgencia,1,3)) AND
                Novedades_Nomina.PerNomina EQ INTEGER(CPeriodo) AND 
                Novedades_Nomina.Tipo      NE 3 AND 
                Novedades_Nomina.Clase     NE 6 NO-LOCK NO-ERROR.
  ELSE
     FIND FIRST Novedades_Nomina WHERE
                Novedades_Nomina.Agencia   EQ INTEGER(SUBSTRING(CAgencia,1,3)) AND
                Novedades_Nomina.PerNomina EQ INTEGER(CPeriodo) AND 
                Novedades_Nomina.Nit       EQ NitW AND
                Novedades_Nomina.Tipo      NE 3 AND
                Novedades_Nomina.Clase     NE 6 NO-LOCK NO-ERROR.
  IF AVAILABLE Novedades_Nomina THEN DO:
     RUN Verifica_Empleado (OUTPUT W_Ok).
     IF NOT W_Ok THEN DO:
         IF Novedades_Nomina.Estado_Liquidacion EQ 2 THEN DO:
            MESSAGE "Ya fue liquidada una nomina para este periodo" VIEW-AS ALERT-BOX.
            WHistorico = YES.
            APPLY "choose" TO Btn_OutPre IN FRAME FPrenomina.
         END.
         ELSE DO:
             MESSAGE "Ya existe una prenomina con la misma agencia y Periodo" SKIP
                     "se cargara en pantalla la información de prenomina" SKIP
                     "existente en la tabla de Novedades_Nomina" VIEW-AS ALERT-BOX INFORMATION.
             APPLY "choose" TO Btn_Cargar.
         END.
     END.
     ELSE DO:
         RUN Generar.
         OPEN QUERY BEmpleados FOR EACH TEmpleados.
         OPEN QUERY BDevengados FOR EACH TDedDev WHERE TDedDev.Nit EQ TEmpleados.Nit AND TDeddev.Tip EQ 1.
         OPEN QUERY BDeducciones FOR EACH TDedDev WHERE TDedDev.Nit EQ TEmpleados.Nit AND TDeddev.Tip EQ 2.
     END.
  END.
  ELSE DO:
         RUN Generar.
         OPEN QUERY BEmpleados FOR EACH TEmpleados.
         OPEN QUERY BDevengados FOR EACH TDedDev WHERE TDedDev.Nit EQ TEmpleados.Nit AND TDeddev.Tip EQ 1.
         OPEN QUERY BDeducciones FOR EACH TDedDev WHERE TDedDev.Nit EQ TEmpleados.Nit AND TDeddev.Tip EQ 2.
  END.
  RUN TotalesEmpleado.
  ENABLE Btn_Liquidar WITH FRAME Fmain.
  HIDE FRAME FPrenomina.        
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fMain
&Scoped-define SELF-NAME Btn_Liquidar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Liquidar wWin
ON CHOOSE OF Btn_Liquidar IN FRAME fMain /* Liquidar */
DO:
  IF WHistorico THEN 
     MESSAGE "Se esta accesando información de una nómina ya liquidada" SKIP
             "No se puede liquidar de nuevo esta nómina" VIEW-AS ALERT-BOX.
  ELSE DO:
      APPLY "choose" TO Btn_SalvarPre IN FRAME FPrenomina.
      DISABLE {&List-1} WITH FRAME Fmain.
      ASSIGN TCre = 0 TDeb = 0.
      ASSIGN FRAME FMain CAgencia Fecha_Trabajo CTipoPago CPeriodo.
      IF SUBSTRING(CAgencia,1,3) EQ "000" THEN DO:
         MESSAGE "No Puede procesarse una nómina para la agencia 000" SKIP
                 "rectifique la agencia a procesar." VIEW-AS ALERT-BOX.
         APPLY "entry" TO CAgencia IN FRAME FMain.
         RETURN NO-APPLY.
      END.
      IF Fecha_Trabajo EQ ? THEN DO:
         MESSAGE "No se puede procesarse una nómina sin una fecha" SKIP
                 "rectifique la fecha." VIEW-AS ALERT-BOX.
         APPLY "entry" TO Fecha_Trabajo IN FRAME FMain.
         RETURN NO-APPLY.
      END.
      IF CPeriodo EQ ? THEN DO:
         MESSAGE "No se puede procesarse una nómina sin periodo" SKIP
                 "Escoja el periodo de pago a procesar." VIEW-AS ALERT-BOX.
         APPLY "entry" TO CPeriodo IN FRAME FMain.
         RETURN NO-APPLY.
      END.
      RUN Liquidar_Nomina.
      IF ERROR-STATUS:ERROR THEN DO:
         MESSAGE "La Nomina no pudo ser liquidada" SKIP
                 "Existe algún problema de configuracion" VIEW-AS ALERT-BOX.
      END.
      WHistorico = NO.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FPrenomina
&Scoped-define SELF-NAME Btn_OutPre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OutPre wWin
ON CHOOSE OF Btn_OutPre IN FRAME FPrenomina /* Ocultar */
DO:
  HIDE FRAME FPrenomina.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fMain
&Scoped-define SELF-NAME Btn_Prenomina
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Prenomina wWin
ON CHOOSE OF Btn_Prenomina IN FRAME fMain /* Prenomina */
DO:
  VIEW FRAME FPrenomina.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FPrenomina
&Scoped-define SELF-NAME Btn_SalvarPre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_SalvarPre wWin
ON CHOOSE OF Btn_SalvarPre IN FRAME FPrenomina /* Salvar */
DO:
  IF WHistorico THEN 
     MESSAGE "No se pueden salvar los cambios" SKIP
             "esta nomina ya fue liquidada" VIEW-AS ALERT-BOX.
  ELSE DO:
      ASSIGN FRAME FMain CAgencia Fecha_Trabajo CPeriodo.
      FOR EACH TDedDev WHERE TDedDev.Cod LT 500:
          FIND Novedades_Nomina WHERE 
               Novedades_Nomina.Agencia   EQ TDedDev.Age AND
               Novedades_Nomina.Tipo      EQ TDedDev.Tip AND
               Novedades_Nomina.Codigo    EQ TDedDev.Cod AND
               Novedades_Nomina.Nit       EQ TDedDev.Nit AND
               Novedades_Nomina.PerNomina EQ INTEGER(CPeriodo) NO-ERROR.
          FIND Cfg_Novedades WHERE Cfg_Novedades.Codigo EQ TDedDev.Cod NO-LOCK NO-ERROR.
          IF NOT AVAILABLE Novedades_Nomina THEN DO:
              CREATE Novedades_Nomina.
              ASSIGN Novedades_Nomina.Agencia             = TDedDev.Age
                     Novedades_Nomina.Tipo                = Cfg_Novedades.Tipo
                     Novedades_Nomina.Codigo              = TDedDev.Cod
                     Novedades_Nomina.Estado_Liquidacion  = 1
                     Novedades_Nomina.Fecha               = TDedDev.Fec
                     Novedades_Nomina.Nit                 = TDedDev.Nit
                     Novedades_Nomina.Usuario             = W_Usuario
                     Novedades_Nomina.Clase               = Cfg_Novedades.Clase
                     Novedades_Nomina.Cuenta              = TDedDev.Cta
                     Novedades_Nomina.Naturaleza          = TDedDev.Nat
                     Novedades_Nomina.PerNomina           = INTEGER(CPeriodo)
                     Novedades_Nomina.Horas_Extras        = TDedDev.NmE.
          END.
          ELSE DO:
              IF Novedades_Nomina.Valor NE TDedDev.Val THEN
                 Novedades_Nomina.Valor = TDedDev.Val.
          END.
          ASSIGN Novedades_Nomina.Valor               = TDedDev.Val
                 Novedades_Nomina.Dias_Trabajados     = TDedDev.Dia.
          IF cfg_novedades.clase EQ 5 THEN Novedades_Nomina.Clase = Cfg_Novedades.Clase.
      END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FImpresion
&Scoped-define SELF-NAME BUTTON-183
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-183 wWin
ON CHOOSE OF BUTTON-183 IN FRAME FImpresion /* Imprimir */
DO:
  DEFINE VAR Listado AS CHARACTER INITIAL "".  
  IF RImpresion:SCREEN-VALUE IN FRAME FImpresion EQ "4" THEN
     RUN W-Informes_Nomina.r.
  ELSE DO:
     ASSIGN FRAME FImpresion RImpresion.
     ASSIGN FRAME FImpresion RImpresion.
     listado = W_PathSpl + "Nomina.Lst".
     {Incluido\Imprimir.I "listado"}
  END.
  HIDE FRAME FImpresion.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-184
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-184 wWin
ON CHOOSE OF BUTTON-184 IN FRAME FImpresion /* Ocultar */
DO:
  HIDE FRAME FImpresion.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FIE
&Scoped-define SELF-NAME BUTTON-186
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-186 wWin
ON CHOOSE OF BUTTON-186 IN FRAME FIE /* Ocultar */
DO:
  HIDE FRAME FIE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FPrenomina
&Scoped-define SELF-NAME BUTTON-190
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-190 wWin
ON CHOOSE OF BUTTON-190 IN FRAME FPrenomina /* Borrar */
DO:
  ASSIGN FRAME FMain CAgencia Fecha_Trabajo CPeriodo.
  MESSAGE "Esta seguro de borrar la prenomina?" VIEW-AS ALERT-BOX WARNING BUTTONS YES-NO UPDATE choice.
  IF choice THEN DO:
     FOR EACH Novedades_Nomina WHERE Novedades_Nomina.Agencia EQ INTEGER(SUBSTRING(CAgencia,1,3)) AND
              Novedades_Nomina.PerNomina EQ INTEGER(CPeriodo) AND
              Novedades_Nomina.Estado_Liquidacion EQ 1:
         DELETE Novedades_Nomina.
     END.
     FOR EACH TEmpleados: DELETE TEmpleados. END.
     FOR EACH TDedDev: DELETE TDedDev. END.
     OPEN QUERY BEmpleados   FOR EACH TEmpleados.
     OPEN QUERY BDevengados  FOR EACH TDedDev WHERE TDedDev.Nit EQ TEmpleados.Nit AND TDeddev.Tip EQ 1.
     OPEN QUERY BDeducciones FOR EACH TDedDev WHERE TDedDev.Nit EQ TEmpleados.Nit AND TDeddev.Tip EQ 2.
     ASSIGN TE_Devengados = 0 TE_Deducciones = 0 TE_Diferencia = 0.
     DISPLAY TE_Diferencia TE_Devengados TE_Deducciones.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FLiquidacion
&Scoped-define SELF-NAME BUTTON-193
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-193 wWin
ON CHOOSE OF BUTTON-193 IN FRAME FLiquidacion /* Ocultar */
DO:
   ENABLE {&List-1} WITH FRAME Fmain.
   HIDE FRAME FLiquidacion.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-194
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-194 wWin
ON CHOOSE OF BUTTON-194 IN FRAME FLiquidacion /* Contabilizar Comprobante Prenomina */
DO:
ASSIGN FRAME fmain CAgencia CPeriodo Cmb_Inicio.
ASSIGN FRAME FLiquidacion TDeb TCre.
IF TDeb NE TCre THEN DO:
   MESSAGE "La suma de las partidas contables no es igual" SKIP
           "Debitos : " TDeb SKIP
           "Creditos: " TCre VIEW-AS ALERT-BOX ERROR.

END.
ELSE DO:
  Faltan = 0.
  MESSAGE "Esta seguro de contabilizar la nomina" SKIP
          "de la oficina" CAgencia SKIP
          "Periodo: " CPeriodo VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO 
          UPDATE wchoice AS LOGICAL.
  IF Wchoice THEN DO:
    DO TRANSACTION:
      FIND Entidad WHERE Entidad.Entidad EQ W_Entidad NO-LOCK NO-ERROR.
      IF AVAILABLE Entidad THEN DO:
         IF Entidad.Cpte_NomEmp EQ 0 THEN DO:
            MESSAGE "No esta configurado el comprobante de pago de nomina en la organizacion" SKIP
                    "comuniquese con el administrador del sistema y comentele este problema"
                    VIEW-AS ALERT-BOX.
         END.
         ELSE DO:
             FIND FIRST Comprobantes WHERE Comprobantes.Agencia     EQ INTEGER(SUBSTRING(CAgencia,1,3))
                                       AND Comprobantes.Comprobante EQ Entidad.Cpte_NomEmp NO-ERROR.
             IF AVAIL(Comprobantes) THEN DO:
                ASSIGN Comprobantes.Secuencia = Comprobantes.Secuencia + 1
                       W_DocContab            = Comprobantes.Secuencia
                       Cbte                   = Comprobantes.Comprobante.
                FIND Formatos WHERE Formatos.Agencia     EQ INTEGER(SUBSTRING(CAgencia,1,3))
                                AND Formatos.Cod_Formato EQ Comprobantes.Cod_formato 
                            NO-LOCK NO-ERROR.
                FIND CURRENT Comprobantes NO-LOCK NO-ERROR.
             END.
             ELSE DO:
                RUN MostrarMensaje IN W_Manija (INPUT 143,OUTPUT W_Error).
                RETURN NO-APPLY.
             END.
         END.
      END.
      FOR EACH TConta:
          CREATE Mov_Contable.
          ASSIGN Mov_Contable.Agencia        = TConta.Agencia       
                 Mov_Contable.Cuenta         = TConta.Cuenta        
                 Mov_Contable.Fec_Contable   = TConta.Fec_Contable  
                 Mov_Contable.Comentario     = TConta.Comentario    
                 Mov_Contable.Usuario        = TConta.Usuario       
                 Mov_Contable.Nit            = TConta.Nit           
                 Mov_Contable.Cen_Costos     = TConta.Cen_Costos    
                 Mov_Contable.Destino        = TConta.Destino       
                 Mov_Contable.Comprobante    = Cbte   
                 Mov_Contable.Num_Documento  = W_DocContab
                 Mov_Contable.Doc_Referencia = TConta.Doc_Referencia
                 Mov_Contable.Fec_Grabacion  = TConta.Fec_Grabacion 
                 Mov_Contable.Hora           = TConta.Hora          
                 Mov_Contable.Estacion       = TConta.Estacion
                 Mov_Contable.DB             = TConta.DB
                 Mov_Contable.CR             = TConta.CR NO-ERROR.
      END.
      FOR EACH Novedades_Nomina WHERE 
             Novedades_Nomina.Agencia   EQ INTEGER(SUBSTRING(CAgencia,1,3)) AND 
             Novedades_Nomina.Tipo      NE 3 AND
             Novedades_Nomina.PerNomina EQ INTEGER(CPeriodo):
         Novedades_Nomina.Estado_Liquidacion = 2.
         FOR EACH TEmpleados: DELETE TEmpleados. END.
         FOR EACH TDedDev: DELETE TDedDev. END.
         OPEN QUERY BEmpleados   FOR EACH TEmpleados.
         OPEN QUERY BDevengados  FOR EACH TDedDev WHERE TDedDev.Nit EQ TEmpleados.Nit AND TDeddev.Tip EQ 1.
         OPEN QUERY BDeducciones FOR EACH TDedDev WHERE TDedDev.Nit EQ TEmpleados.Nit AND TDeddev.Tip EQ 2.
         ASSIGN TE_Devengados = 0 TE_Deducciones = 0 TE_Diferencia = 0 NitW = "".
         DISPLAY TE_Diferencia TE_Devengados TE_Deducciones NitW WITH FRAME FMain.
      END.
      FOR EACH Empleados WHERE Empleados.Agencia EQ INTEGER(SUBSTRING(CAGencia,1,3)):
        IF Empleados.PerNom_Actual EQ INTEGER(CPeriodo) THEN NEXT.
        ELSE DO:
            FIND FIRST Novedades_Nomina WHERE 
                 Novedades_Nomina.Agencia   EQ INTEGER(SUBSTRING(CAGencia,1,3)) AND
                 Novedades_Nomina.Nit       EQ Empleados.Nit                    AND
                 Novedades_Nomina.PerNomina EQ INTEGER(CPeriodo) AND
                 Novedades_Nomina.Estado_Liquidacion EQ 2 NO-LOCK NO-ERROR.
            IF AVAILABLE Novedades_Nomina THEN
                Empleados.PerNom_Actual = INTEGER(CPeriodo).
            ELSE Faltan = Faltan + 1.
        END.
      END.
      IF Cmb_Inicio NE "Proceso Especial" THEN DO:
          FIND Agencias WHERE Agencias.Agencia EQ INTEGER(SUBSTRING(CAgencia:SCREEN-VALUE IN FRAME fmain,1,3)) NO-ERROR.
          IF AVAILABLE Agencias AND Faltan EQ 0 THEN 
              Agencias.PerNom_Actual = INTEGER(CPeriodo).
      END.

      IF AVAILABLE(Formatos) THEN
         RUN VALUE(Formatos.Nom_Proceso) (INPUT Cbte,
                                         INPUT W_DocContab, INPUT W_DocContab,
                                         INPUT INTEGER(SUBSTRING(CAgencia,1,3))).
      FOR EACH TConta: DELETE TConta. END.
      ENABLE {&List-1} WITH FRAME Fmain.
      HIDE FRAME FLiquidacion.
    END. /*fin transaccion*/
  END.
END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FConEmpleados
&Scoped-define SELF-NAME BUTTON-196
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-196 wWin
ON CHOOSE OF BUTTON-196 IN FRAME FConEmpleados /* Ocultar */
DO:
  IF Id_Adicion THEN DO:
     Id_Adicion = NO.
     FIND FIRST TEmpleados WHERE TEmpleados.Nit EQ Empleados.Nit NO-ERROR.
     IF AVAILABLE TEmpleados THEN DO:
        MESSAGE "No se puede adicionar un empleado que ya se" SKIP
                "encuentre en la prenomina" VIEW-AS ALERT-BOX ERROR.
     END.
     ELSE DO:
       NitAdicion = Empleados.Nit.
       RCuantos = 2.
       RUN Generar.
       OPEN QUERY BEmpleados FOR EACH TEmpleados.
       OPEN QUERY BDevengados FOR EACH TDedDev WHERE TDedDev.Nit EQ TEmpleados.Nit AND TDeddev.Tip EQ 1.
       OPEN QUERY BDeducciones FOR EACH TDedDev WHERE TDedDev.Nit EQ TEmpleados.Nit AND TDeddev.Tip EQ 2.
       RCuantos = 1.
     END.
  END.
  HIDE FRAME FConEmpleados.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FEspeciales
&Scoped-define SELF-NAME BUTTON-198
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-198 wWin
ON CHOOSE OF BUTTON-198 IN FRAME FEspeciales /* Ocultar */
DO:
  ASSIGN FRAME FEspeciales FecI FecF.
  IF FecI GT FecF THEN DO:
     MESSAGE "El rango de fechas es incorrecto." SKIP
             "La fecha inicial debe ser menor a" SKIP
             "la fecha final. Rectifique!" VIEW-AS ALERT-BOX.
     APPLY "entry" TO FecI IN FRAME FEspeciales.
     RETURN NO-APPLY.
  END.
  ELSE 
     HIDE FRAME FEspeciales.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FExtras
&Scoped-define SELF-NAME BUTTON-200
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-200 wWin
ON CHOOSE OF BUTTON-200 IN FRAME FExtras /* Ocultar */
DO:
  APPLY "leave" TO Nex IN FRAME FExtras.
  ASSIGN FRAME FExtras Nex PorEx TEx ValEx TotExt.
  HIDE FRAME FExtras.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FNitContable
&Scoped-define SELF-NAME BUTTON-201
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-201 wWin
ON CHOOSE OF BUTTON-201 IN FRAME FNitContable /* Aceptar */
DO:
  ASSIGN FRAME Fnitcontable Wknit.
  IF WkNit EQ "" THEN DO:
     MESSAGE "El concepto que se trata de adicionar al empleado," SKIP
             "requiere de un nit diferente al suyo para efectos" SKIP
             "de contabilidad." SKIP(2)
             "por favor digite un nit valido de una la empresa" SKIP
             "prestadora de salud a la cual pertenece el empleado!" VIEW-AS ALERT-BOX ERROR.
     APPLY "entry" TO WkNit.
  END.
  ELSE DO: 
      HIDE FRAME FNitcontable.
      APPLY "choose" TO Btn_Adiciona IN FRAME FConceptos.
      RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fMain
&Scoped-define SELF-NAME BUTTON-208
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-208 wWin
ON CHOOSE OF BUTTON-208 IN FRAME fMain /* Configuracion Nomina */
DO:
  RUN W-Cf_NovedadesNomina.r.
  RUN initializeObject.
OPEN QUERY BEmpleados FOR EACH TEmpleados.
w_ok = BROWSE BEmpleados:SELECT-ROW(1).
OPEN QUERY BDevengados FOR EACH TDedDev WHERE TDedDev.Nit EQ TEmpleados.Nit AND TDeddev.Tip EQ 1 INDEXED-REPOSITION.
OPEN QUERY BDeducciones FOR EACH TDedDev WHERE TDedDev.Nit EQ TEmpleados.Nit AND TDeddev.Tip EQ 2 BY TDedDev.IPa INDEXED-REPOSITION.
W_ok = BEmpleados:SELECT-ROW(1).
MESSAGE "Desea recalcular los valores de la prenomina" SKIP
        "para que tome los cambios realizados en la" SKIP
        "configuracion de conceptos?"
        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE choice AS LOGICAL.
IF choice THEN DO:
   W_Puntero = ROWID(TEmpleados).
   FOR EACH TEmpleados:
       RUN Recalculo.
   END.

END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-209
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-209 wWin
ON CHOOSE OF BUTTON-209 IN FRAME fMain /* Maestro Empleados */
DO:
  RUN W-Maestro_Empleados.r.
  RUN initializeObject.
FIND FIRST TEmpleados NO-ERROR.
IF AVAILABLE TEmpleados THEN DO:
    OPEN QUERY BEmpleados FOR EACH TEmpleados.
    w_ok = BROWSE BEmpleados:SELECT-ROW(1).
    OPEN QUERY BDevengados FOR EACH TDedDev WHERE TDedDev.Nit EQ TEmpleados.Nit AND TDeddev.Tip EQ 1 INDEXED-REPOSITION.
    OPEN QUERY BDeducciones FOR EACH TDedDev WHERE TDedDev.Nit EQ TEmpleados.Nit AND TDeddev.Tip EQ 2 BY TDedDev.IPa INDEXED-REPOSITION.
    W_ok = BEmpleados:SELECT-ROW(1).
END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-210
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-210 wWin
ON CHOOSE OF BUTTON-210 IN FRAME fMain /* E */
DO:
OPEN QUERY BEmpAgencia FOR EACH Empleados WHERE
      Empleados.Agencia EQ INTEGER(SUBSTRING(CAgencia:SCREEN-VALUE,1,3)).
  FRAME FEmpAgencia:TITLE = CAgencia:SCREEN-VALUE + ". Periodo Actual de Nomina para esta agencia: " + STRING(Agencias.PerNom_Actual).
  VIEW FRAME FEmpAgencia.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FEmpAgencia
&Scoped-define SELF-NAME BUTTON-212
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-212 wWin
ON CHOOSE OF BUTTON-212 IN FRAME FEmpAgencia /* Ocultar */
DO:
  HIDE FRAME FEmpAgencia.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fMain
&Scoped-define SELF-NAME BUTTON-213
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-213 wWin
ON CHOOSE OF BUTTON-213 IN FRAME fMain /* Consultas de Nomina */
DO:
  RUN w-Informes_Nomina.w.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FLiquidacion
&Scoped-define SELF-NAME BUTTON-214
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-214 wWin
ON CHOOSE OF BUTTON-214 IN FRAME FLiquidacion /* Imprimir Precontabilidad */
DO:
  DEFINE VAR Listado AS CHARACTER INITIAL "".
  DEFINE VAR Tamano  AS INTEGER   INITIAL 2.
  RImpresion = 5.
  listado = W_PathSpl + "L_Empresas.Lst".
  {Incluido\Imprimir.i "Listado" Tamano}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FConceptos
&Scoped-define SELF-NAME BUTTON-37
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-37 wWin
ON CHOOSE OF BUTTON-37 IN FRAME FConceptos /* Ocultar */
DO:
  HIDE FRAME FConceptos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fMain
&Scoped-define SELF-NAME BUTTON-40
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-40 wWin
ON CHOOSE OF BUTTON-40 IN FRAME fMain /* Imprimir */
DO:
 FIND FIRST TEmpleados NO-ERROR.
 IF NOT AVAILABLE TEmpleados THEN
    MESSAGE "No esta cargada ninguna informacion en la pantalla" SKIP
            "No funcionara el modulo de impresión si no se ha" SKIP
            "cargado una prenomina o una nomina liquidada" VIEW-AS ALERT-BOX WARNING.
 ELSE VIEW FRAME FImpresion.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CAgencia
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CAgencia wWin
ON VALUE-CHANGED OF CAgencia IN FRAME fMain /* Agencia */
DO:
  FIND Agencias WHERE Agencias.Agencia EQ INTEGER(SUBSTRING(SELF:SCREEN-VALUE,1,3)) NO-LOCK NO-ERROR.
  IF AVAILABLE Agencias THEN DO:
     CPeriodo:SCREEN-VALUE = STRING(Agencias.PerNom_Actual + 1).
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Cmb_Inicio
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_Inicio wWin
ON VALUE-CHANGED OF Cmb_Inicio IN FRAME fMain /* Acción */
DO:
  ASSIGN FRAME FMain Cmb_Inicio.
  IF Cmb_Inicio NE "Nómina Normal" THEN DO:
     OPEN QUERY BEspeciales FOR EACH TConceptos WHERE TConceptos.Ies.
     VIEW FRAME FEspeciales.
     APPLY "entry" TO FecI.
     RETURN NO-APPLY.
  END.
  ELSE
     HIDE FRAME FEspeciales.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CPeriodo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CPeriodo wWin
ON VALUE-CHANGED OF CPeriodo IN FRAME fMain /* Periodo */
DO:
  IF Agencias.PerNom_Actual GE INTEGER(CPeriodo:SCREEN-VALUE) THEN DO:
     MESSAGE "Esta nomina ya fue anteriomente liquidada" SKIP
             "La nomina que sigue para esta agencia es" SKIP
             "para el periodo: " Agencias.PerNom_Actual + 1.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CTipoPago
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CTipoPago wWin
ON VALUE-CHANGED OF CTipoPago IN FRAME fMain /* Tipo de Pago */
DO:
  CASE SELF:SCREEN-VALUE:
      WHEN "01 - Semanal" THEN DO:
         CPeriodo:LIST-ITEMS = "".
         DO i = 1 TO 52 BY 1:
           W_Ok = CPeriodo:ADD-LAST(STRING(i)) IN FRAME FMain.
         END.
      END.
      WHEN "02 - Quincenal" THEN DO:
         CPeriodo:LIST-ITEMS = "".
         DO i = 1 TO 26 BY 1:
           W_Ok = CPeriodo:ADD-LAST(STRING(i)) IN FRAME FMain.
         END.
      END.
      WHEN "03 - Mensual" THEN DO:
         CPeriodo:LIST-ITEMS = "".
         DO i = 1 TO 12 BY 1:
           W_Ok = CPeriodo:ADD-LAST(STRING(i)) IN FRAME FMain.
         END.
      END.
  END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Adicionar_Concepto_a_Todos_
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Adicionar_Concepto_a_Todos_ wWin
ON CHOOSE OF MENU-ITEM m_Adicionar_Concepto_a_Todos_ /* Adicionar Concepto a Todos los empleados */
DO:
  WTipo = 3.
  OPEN QUERY BConceptos FOR EACH TConceptos WHERE NOT TConceptos.IBa AND NOT TConceptos.IPa 
      AND NOT TConceptos.IBa AND NOT TConceptos.IEX.
  VIEW FRAME FConceptos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Adicionar_Deduccion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Adicionar_Deduccion wWin
ON CHOOSE OF MENU-ITEM m_Adicionar_Deduccion /* Adicionar Deduccion */
DO:
  IF WHistorico THEN
     MESSAGE "Se encuentra consultando una nomina" SKIP
             "que ya ha sido liquidada." SKIP(1)
             "No puede realizarse la acción de actualización de la nómina"
         VIEW-AS ALERT-BOX.
  ELSE DO:
      IF NOT AVAILABLE TEmpleados THEN
         MESSAGE "No se pueden agregar deducciones si no esta seleccionado" SKIP
                 "algun empleado." VIEW-AS ALERT-BOX.
      ELSE DO:
        WTipo = 2.
        OPEN QUERY BConceptos FOR EACH TConceptos WHERE TConceptos.Tip EQ WTipo.
        VIEW FRAME FConceptos.
      END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Adicionar_Devengado
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Adicionar_Devengado wWin
ON CHOOSE OF MENU-ITEM m_Adicionar_Devengado /* Adicionar Devengado */
DO:
  IF WHistorico THEN
     MESSAGE "Se encuentra consultando una nomina" SKIP
             "que ya ha sido liquidada." SKIP(1)
             "No puede realizarse la acción de actualización de la nómina"
         VIEW-AS ALERT-BOX.
  ELSE DO:
      IF NOT AVAILABLE TEmpleados THEN
         MESSAGE "No se pueden crear devengados si no esta seleccionado" SKIP
                 "algún empleado" VIEW-AS ALERT-BOX.
      ELSE DO:  
        WTipo = 1.
        OPEN QUERY BConceptos FOR EACH TConceptos WHERE TConceptos.Tip EQ WTipo.
        VIEW FRAME FConceptos.
      END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Adicionar_un_empleado
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Adicionar_un_empleado wWin
ON CHOOSE OF MENU-ITEM m_Adicionar_un_empleado /* Adicionar un Empleado */
DO:
  Id_Adicion = YES.
  ASSIGN FRAME FMain CAgencia.
  OPEN QUERY BConEmpleados FOR EACH Empleados WHERE Empleados.Agencia EQ INTEGER(SUBSTRING(CAgencia:SCREEN-VALUE IN FRAME FMain,1,3)).
  VIEW FRAME FConEmpleados.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Eliminar_Deduccion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Eliminar_Deduccion wWin
ON CHOOSE OF MENU-ITEM m_Eliminar_Deduccion /* Eliminar Deduccion */
DO:
  IF WHistorico THEN
     MESSAGE "Se encuentra consultando una nomina" SKIP
             "que ya ha sido liquidada." SKIP(1)
             "No puede realizarse la acción de actualización de la nómina"
         VIEW-AS ALERT-BOX.
  ELSE DO:
      IF NOT AVAILABLE TEmpleados THEN
         MESSAGE "No se pueden borrar deducciones si no esta seleccionado" SKIP
                 "algún empleado" VIEW-AS ALERT-BOX.
      ELSE DO:
        IF TDedDev.Ipa THEN
           MESSAGE "No puede ser eliminado el concepto de pago" SKIP
                   "ya que este es el que contiene el valor neto" SKIP
                   "a pagarle al empleado." VIEW-AS ALERT-BOX WARNING.
        ELSE DO:
            FIND Novedades_Nomina WHERE
                 Novedades_Nomina.Agencia   EQ TDedDev.Age AND
                 Novedades_Nomina.Nit       EQ TDedDev.Nit AND
                 Novedades_Nomina.Tipo      EQ 2 AND
                 Novedades_Nomina.Codigo    EQ TDedDev.Cod AND
                 Novedades_Nomina.PerNomina EQ TDedDev.Per NO-ERROR.
            IF AVAILABLE Novedades_Nomina THEN DELETE Novedades_Nomina.
            DELETE TDedDev.
            RUN Recalculo.
        END.
      END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Eliminar_Devengado
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Eliminar_Devengado wWin
ON CHOOSE OF MENU-ITEM m_Eliminar_Devengado /* Eliminar Devengado */
DO:
  IF WHistorico THEN
     MESSAGE "Se encuentra consultando una nomina" SKIP
             "que ya ha sido liquidada." SKIP(1)
             "No puede realizarse la acción de actualización de la nómina"
         VIEW-AS ALERT-BOX.
  ELSE DO:
      IF NOT AVAILABLE TEmpleados THEN
         MESSAGE "No se pueden borrar devengados si no esta seleccionado" SKIP
                 "algún empleado" VIEW-AS ALERT-BOX.
      ELSE DO:
        IF TDedDev.IBa THEN
           MESSAGE "No puede ser eliminado el concepto que" SKIP
                   "contiene el salario." VIEW-AS ALERT-BOX ERROR.
        ELSE DO:
            FIND Novedades_Nomina WHERE
                 Novedades_Nomina.Agencia   EQ TDedDev.Age AND
                 Novedades_Nomina.Nit       EQ TDedDev.Nit AND
                 Novedades_Nomina.Tipo      EQ 1 AND
                 Novedades_Nomina.Codigo    EQ TDedDev.Cod AND
                 Novedades_Nomina.PerNomina EQ TDedDev.Per NO-ERROR.
            IF AVAILABLE Novedades_Nomina THEN DELETE Novedades_Nomina.
            DELETE TDedDev.
            RUN Recalculo.
        END.
      END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Eliminar_Empleado_de_la_lis
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Eliminar_Empleado_de_la_lis wWin
ON CHOOSE OF MENU-ITEM m_Eliminar_Empleado_de_la_lis /* Eliminar Empleado de la lista */
DO:
  MESSAGE "Esta seguro de borrar a " TEmpleados.Nom VIEW-AS ALERT-BOX BUTTONS YES-NO UPDATE choice.
  IF choice THEN DO:
      FOR EACH TDedDev WHERE TDedDev.Nit EQ TEmpleados.Nit:
          DELETE TDedDev.
      END.
      FOR EACH Novedades_Nomina WHERE 
               Novedades_Nomina.Nit EQ TEmpleados.Nit AND
               Novedades_Nomina.PerNomina EQ INTEGER(CPeriodo) AND 
          YEAR(Novedades_Nomina.Fecha) EQ YEAR(W_Fecha):
          DELETE Novedades_Nomina.
      END.
      DELETE TEmpleados.
      ASSIGN TE_Devengados = 0 TE_Deducciones = 0 TE_Diferencia = 0.
      DISPLAY TE_Devengados TE_Deducciones TE_Diferencia WITH FRAME FMain.
      OPEN QUERY BEmpleados FOR EACH TEmpleados.
      OPEN QUERY BDevengados FOR EACH TDedDev WHERE TDedDev.Nit EQ TEmpleados.Nit AND TDeddev.Tip EQ 1.
      OPEN QUERY BDeducciones FOR EACH TDeddev WHERE TDedDev.Nit EQ TEmpleados.Nit AND TDeddev.Tip EQ 2.
      RUN Recalculo.
      Linea = 0.
      FOR EACH Templeados:
          ASSIGN TEmpleados.Lin = Linea + 1
                 Linea = Linea + 1.      
      END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Informacion_Empleado
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Informacion_Empleado wWin
ON CHOOSE OF MENU-ITEM m_Informacion_Empleado /* Informacion Empleado */
DO:
  Sie:LIST-ITEMS IN FRAME fie = "".
  FIND Empleados WHERE Empleados.Nit EQ TEmpleados.Nit NO-LOCK NO-ERROR.
  IF AVAILABLE Empleados THEN DO:
     ASSIGN FRAME FIE:TITLE = TEmpleados.Nom.
     W_Ok = SIE:ADD-LAST("Cargo desempeñado         :   " + Empleados.Cargo). 
     CASE Empleados.Estado:
         WHEN 1 THEN W_Ok = SIE:ADD-LAST("Estado actual             :   " + "Activo - Normal"). 
         WHEN 2 THEN W_Ok = SIE:ADD-LAST("Estado actual             :   " + "Retirado"). 
         WHEN 3 THEN W_Ok = SIE:ADD-LAST("Estado actual             :   " + "Lic.Maternindad"). 
         WHEN 4 THEN W_Ok = SIE:ADD-LAST("Estado actual             :   " + "Lic.Remunerada"). 
         WHEN 5 THEN W_Ok = SIE:ADD-LAST("Estado actual             :   " + "Lic.No.Remunerada"). 
         WHEN 6 THEN W_Ok = SIE:ADD-LAST("Estado actual             :   " + "Vacaciones"). 
         WHEN 7 THEN W_Ok = SIE:ADD-LAST("Estado actual             :   " + "Comision"). 
         WHEN 8 THEN W_Ok = SIE:ADD-LAST("Estado actual             :   " + "Calamidad Domestica"). 
         WHEN 9 THEN W_Ok = SIE:ADD-LAST("Estado actual             :   " + "Incapacidad"). 
         WHEN 10 THEN W_Ok = SIE:ADD-LAST("Estado actual             :   " + "Suspendido"). 
     END CASE.
     W_Ok = SIE:ADD-LAST("Fecha de Ingreso          :   " + STRING(Empleados.Fec_Ingreso)). 
     CASE Empleados.Forma_Pago:
         WHEN 1 THEN W_Ok = SIE:ADD-LAST("Forma de Pago             :   " + "Efectivo"). 
         WHEN 2 THEN W_Ok = SIE:ADD-LAST("Forma de Pago             :   " + "Cheque"). 
         WHEN 3 THEN W_Ok = SIE:ADD-LAST("Forma de Pago             :   " + "Banca Electronica"). 
     END CASE.
     FIND Varios WHERE Varios.Tipo EQ 32 AND Varios.Codigo EQ Empleados.Fon_Cesantias NO-LOCK NO-ERROR.
     IF AVAILABLE Varios THEN W_Ok = SIE:ADD-LAST("Fondo de Cesantias        :   " + Varios.Descripcion).
     ELSE W_Ok = SIE:ADD-LAST("Fondo de Cesantias        :   " + "No Matriculado"). 
     FIND Varios WHERE Varios.Tipo EQ 31 AND Varios.Codigo EQ Empleados.Fon_Pensiones NO-LOCK NO-ERROR.
     IF AVAILABLE Varios THEN W_Ok = SIE:ADD-LAST("Fondo de Pensiones        :   " + Varios.Descripcion).
     ELSE W_Ok = SIE:ADD-LAST("Fondo de Pensiones        :   " + "No Matriculado"). 
     FIND Varios WHERE Varios.Tipo EQ 33 AND Varios.Codigo EQ Empleados.EPS NO-LOCK NO-ERROR.
     IF AVAILABLE Varios THEN W_Ok = SIE:ADD-LAST("EPS                       :   " + Varios.Descripcion).
     ELSE W_Ok = SIE:ADD-LAST("EPS                       :   " + "No Matriculada"). 
     SIE:ADD-LAST("Salario Basico Mensual    :   " + STRING(Empleados.Salario_Mensual)). 
     CASE Empleados.Periodo_Pago:
         WHEN 1 THEN SIE:ADD-LAST("Periodo de Pago           :   " + "Semanal"). 
         WHEN 2 THEN SIE:ADD-LAST("Periodo de Pago           :   " + "Quincenal"). 
         WHEN 3 THEN SIE:ADD-LAST("Periodo de Pago           :   " + "Mensual"). 
     END CASE.
     IF Empleados.Prestamo_Especial GT 0 THEN DO:
        SIE:ADD-LAST("Prestamo Especial         :   " + STRING(Empleados.Prestamo_Especial)). 
        SIE:ADD-LAST("Plazo Prestamo            :   " + STRING(Empleados.Plazo_Prestamo)). 
        SIE:ADD-LAST("Fec.Inicio Prestamo       :   " + STRING(Empleados.Fec_InicioPrestamo)). 
     END.
  END.
  VIEW FRAME FIE.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Salvar_Prenomina
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Salvar_Prenomina wWin
ON CHOOSE OF MENU-ITEM m_Salvar_Prenomina /* Salvar Prenomina */
DO:
  APPLY "choose" TO Btn_SalvarPre IN FRAME FPrenomina.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Salvar_Prenomina2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Salvar_Prenomina2 wWin
ON CHOOSE OF MENU-ITEM m_Salvar_Prenomina2 /* Salvar Prenomina */
DO:
  APPLY "choose" TO Btn_SalvarPre IN FRAME FPrenomina.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Salvar_Prenomina3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Salvar_Prenomina3 wWin
ON CHOOSE OF MENU-ITEM m_Salvar_Prenomina3 /* Salvar Prenomina */
DO:
  APPLY "choose" TO Btn_SalvarPre IN FRAME FPrenomina.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FExtras
&Scoped-define SELF-NAME NEx
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL NEx wWin
ON LEAVE OF NEx IN FRAME FExtras /* Número de Horas Extras */
DO:
  ASSIGN FRAME FExtras Nex PorEx ValEx Tex TotExt.
  TotExt = NEx * Tex.
   DISPLAY TotExt WITH FRAME FExtras.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fMain
&Scoped-define SELF-NAME NitW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL NitW wWin
ON LEAVE OF NitW IN FRAME fMain /* Nit */
DO:
  FIND Empleados WHERE Empleados.Nit EQ SELF:SCREEN-VALUE NO-LOCK NO-ERROR.
  IF AVAILABLE Empleados THEN DO:
     FIND Clientes WHERE Clientes.Nit EQ SELF:SCREEN-VALUE NO-LOCK NO-ERROR.
     IF Empleados.Periodo_Pago NE INTEGER(SUBSTRING(CTipoPago:SCREEN-VALUE,1,2)) THEN DO:
        MESSAGE "El empleado " Clientes.Nombre " " Clientes.Apellido1 " " Clientes.Apellido2 SKIP
                "No es de pago " CTipoPago:SCREEN-VALUE SKIP
                "no se puede generar una prenomina de este cliente para este" SKIP
                "tipo de pago! cambie el tipo de pago al del empleado" VIEW-AS ALERT-BOX INFORMATION.
        SELF:SCREEN-VALUE = "".
        APPLY "entry" TO CTipoPago.
        RETURN NO-APPLY.
     END.
  END.
  ELSE DO:
      ASSIGN FRAME FMain CAgencia.
      OPEN QUERY BConEmpleados FOR EACH Empleados WHERE Empleados.Agencia EQ INTEGER(SUBSTRING(CAgencia:SCREEN-VALUE IN FRAME FMain,1,3)).
      VIEW FRAME FConEmpleados.
  END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RCuantos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RCuantos wWin
ON VALUE-CHANGED OF RCuantos IN FRAME fMain
DO:
  ASSIGN TE_Devengados = 0 TE_Deducciones = 0 TE_Diferencia = 0.
  NitW:SCREEN-VALUE IN FRAME FMain = "".
  FOR EACH TEmpleados: DELETE TEmpleados. END.
  FOR EACH TDedDev: DELETE TDedDev. END.
  OPEN QUERY BEmpleados FOR EACH TEmpleados.
  OPEN QUERY BDevengados FOR EACH TDedDev WHERE TDedDev.Nit EQ TEmpleados.Nit AND TDeddev.Tip EQ 1.
  OPEN QUERY BDeducciones FOR EACH TDedDev WHERE TDedDev.Nit EQ TEmpleados.Nit AND TDeddev.Tip EQ 2.
  IF SELF:SCREEN-VALUE EQ "1" THEN DISABLE Nitw WITH FRAME FMain.
  ELSE DO: 
      ENABLE Nitw WITH FRAME FMain.
      APPLY "entry" TO NitW IN FRAME FMain.
      RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FNitContable
&Scoped-define SELF-NAME WKNit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL WKNit wWin
ON LEAVE OF WKNit IN FRAME FNitContable
DO:
  DO WITH FRAME {&FRAME-NAME}:
     FIND Clientes WHERE Clientes.Nit EQ WKNit:SCREEN-VALUE IN FRAME FNitContable NO-LOCK NO-ERROR.
     IF AVAILABLE(Clientes) THEN
        WkNom:SCREEN-VALUE = TRIM(Clientes.Nombre) + " " + Clientes.Apellido1 + " " + Clientes.Apellido2.
     ELSE DO:
        RUN C-Clientes.R(INPUT 1, INPUT W_Agencia, OUTPUT P_Nit,OUTPUT P_Nombre, OUTPUT P_Apellido, OUTPUT P_CliAge).
        ASSIGN WKNom:SCREEN-VALUE    = TRIM(P_Nombre) + " " + P_Apellido
               WKNit:SCREEN-VALUE = P_Nit.
    FIND Clientes WHERE Clientes.Agencia EQ P_CliAge AND Clientes.Nit EQ P_Nit NO-LOCK NO-ERROR.
  END.
  IF AVAILABLE(Clientes) THEN DO:
   IF Clientes.Tipo_Identificacion NE "NIT" THEN DO:
       MESSAGE "Debe ser escogida una Persona Juridica" VIEW-AS ALERT-BOX.
       RETURN NO-APPLY.
   END.
  END.
END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fMain
&Scoped-define BROWSE-NAME BConceptos
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK wWin 


/* ***************************  Main Block  *************************** */
/*trigers para freeform query de empleados*/


ON LEAVE OF TEmpleados.Dia IN BROWSE BEmpleados DO:
   DEFINE VAR fil AS INTEGER.
   FIND TEmpleados WHERE TEmpleados.Nit EQ TEmpleados.Nit:SCREEN-VALUE IN BROWSE BEmpleados NO-ERROR.
   IF AVAILABLE TEmpleados THEN DO:
       ASSIGN TEmpleados.Dia = INTEGER(TEmpleados.Dia:SCREEN-VALUE IN BROWSE BEmpleados).
       RUN Recalculo.
       W_Puntero = ROWID(TEmpleados).
       OPEN QUERY BEmpleados FOR EACH TEmpleados INDEXED-REPOSITION.
       REPOSITION BEmpleados TO ROWID W_Puntero.
      /* fil = BROWSE BEmpleados:GET-REPOSITIONED-ROW( ).
       W_Ok = BROWSE BEmpleados:SELECT-ROW(fil).*/
   END.
END.

ON ENTRY OF TDedDev.Val IN BROWSE BDevengados DO:
    WVAnt = INTEGER(TDedDev.Val:SCREEN-VALUE IN BROWSE BDevengados).
END.

ON LEAVE OF TDedDev.Val IN BROWSE BDevengados DO:
    /*IF TDedDev.Cod EQ 100 THEN TDedDev.IMo = 3.*/
    IF NOT AVAILABLE TDedDev THEN 
       REPOSITION BDevengados TO ROWID W_Puntero2.
    ELSE W_Puntero2 = ROWID(TDedDev).
    IF TDedDev.IMo NE 3 AND WVant NE INTEGER(TDedDev.Val:SCREEN-VALUE IN BROWSE BDevengados) THEN DO:
       MESSAGE "Este concepto no esta configurado para que su" SKIP
               "valor sea entrado de manera manual" SKIP(1)
               "No se permite el cambio del valor presente!" 
                VIEW-AS ALERT-BOX.
       TDedDev.Val:SCREEN-VALUE IN BROWSE BDevengados = STRING(WVAnt).
    END.
    ELSE DO:
        ASSIGN TDedDev.Val = INTEGER(TDedDev.Val:SCREEN-VALUE IN BROWSE BDevengados)
               WNit = TDedDev.Nit.
        RUN Recalculo.
    END.
    IF AVAILABLE TDedDev THEN W_Puntero2 = ROWID(TDedDev).
END.

ON ENTRY OF TDedDev.Val IN BROWSE BDeducciones DO:
    WVAnt = INTEGER(TDedDev.Val:SCREEN-VALUE IN BROWSE BDeducciones).
END.


ON LEAVE OF TDedDev.Val IN BROWSE BDeducciones DO:
    IF NOT AVAILABLE TDedDev THEN 
       REPOSITION BDeducciones TO ROWID W_Puntero2.
    ELSE W_Puntero2 = ROWID(TDedDev).
    IF TDedDev.IMo NE 3 AND WVant NE INTEGER(TDedDev.Val:SCREEN-VALUE IN BROWSE BDeducciones) THEN DO:
       MESSAGE "Este concepto no esta configurado para que su" SKIP
               "valor sea entrado de manera manual" SKIP(1)
               "No se permite el cambio del valor presente!" VIEW-AS ALERT-BOX.
       TDedDev.Val:SCREEN-VALUE IN BROWSE BDevengados = STRING(WVAnt).
    END.
    ELSE DO:
        ASSIGN TDedDev.Val = INTEGER(TDedDev.Val:SCREEN-VALUE IN BROWSE BDeducciones).
        RUN Recalculo.
    END.
END.

/* Include custom  Main Block code for SmartWindows. */
{src/adm2/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Adicionar_ConPago wWin 
PROCEDURE Adicionar_ConPago :
CREATE TDedDev.
  ASSIGN TDedDev.Age = INTEGER(SUBSTRING(CAgencia,1,3))
         TDedDev.Tip = WCfg.Tipo
         TDedDev.Cod = WCfg.Codigo
         TDedDev.Nom = WCfg.Nombre
         TDedDev.Nit = Empleados.Nit
         TDedDev.Fec = Fecha_Trabajo
         TDedDev.IMo = WCfg.Id_Valor
         TDedDev.Dia = TEmpleados.Dia
         TDedDev.Nat = WCfg.Naturaleza
         TDedDev.Cta = WCfg.Cuenta
         TDedDev.Per = INTEGER(CPeriodo)
         TDedDev.Ipa = WCfg.Id_Pago
         TDedDev.Val = TEmpleados.TDv - TEmpleados.TDd.

  ASSIGN TE_Devengados  = TE_Devengados + TEmpleados.TDv
         TE_Deducciones = TE_Deducciones + TEmpleados.TDd.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Adicionar_Especiales wWin 
PROCEDURE Adicionar_Especiales :
ASSIGN FRAME FEspeciales FecI FecF.
DO i = 1 TO BEspeciales:NUM-SELECTED-ROWS IN FRAME FEspeciales BY 1:
    W_OK = BEspeciales:FETCH-SELECTED-ROW(I).
    FIND WCfgPrc WHERE WCfgPrc.Codigo EQ TConceptos.Cod NO-ERROR.  
    RUN VALUE(TConceptos.Prg) (INPUT Empleados.Nit, INPUT FecI, INPUT FecF, OUTPUT WVal).
    CREATE TDedDev.
    ASSIGN TDedDev.Age = INTEGER(SUBSTRING(CAgencia,1,3))
           TDedDev.Tip = WCfgPrc.Tipo
           TDedDev.Cod = WCfgPrc.Codigo
           TDedDev.Nom = WCfgPrc.Nombre
           TDedDev.Nit = Empleados.Nit
           TDedDev.Fec = Fecha_Trabajo
           TDedDev.IMo = WCfgPrc.Id_Valor
           TDedDev.Dia = TEmpleados.Dia
           TDedDev.Nat = WCfgPrc.Naturaleza
           TDedDev.Cta = WCfgPrc.Cuenta
           TDedDev.Per = INTEGER(CPeriodo)
           TDedDev.Val = WVal
           TDedDev.ISa = WCfgPrc.Id_Salario.
    IF WCfgPrc.Tipo EQ 1 THEN
       TEmpleados.TDv = TEmpleados.TDv + TDedDev.Val.
    IF WCfgPrc.Tipo EQ 2 THEN
       TEmpleados.TDd = TEmpleados.TDd + TDedDev.Val.
 END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Cargar wWin 
PROCEDURE Cargar :
ASSIGN FRAME FMain CAgencia Fecha_Trabajo.
DEFINE VAR TTDv AS DECIMAL FORMAT ">>>,>>>,>>9.99".
DEFINE VAR TTDd AS DECIMAL FORMAT ">>>,>>>,>>9.99".
Linea = 0.
FOR EACH Novedades_Nomina WHERE
         Novedades_Nomina.Agencia   EQ INTEGER(SUBSTRING(CAgencia,1,3)) AND
         /*novedades_nomina.nit EQ "71756237" AND*/
         Novedades_Nomina.PerNomina EQ INTEGER(CPeriodo) NO-LOCK BREAK BY Novedades_Nomina.Nit:
    IF Novedades_Nomina.Estado_Liquidacion NE 1 THEN NEXT.
    IF FIRST-OF(novedades_nomina.nit) THEN DO:
      FIND Clientes WHERE Clientes.Nit EQ Novedades_Nomina.Nit NO-LOCK NO-ERROR.
      IF AVAILABLE Clientes THEN
         FIND Empleados WHERE Empleados.Agencia EQ Novedades_Nomina.Agencia AND
              Empleados.Nit EQ Clientes.Nit NO-LOCK NO-ERROR.
    END.
    FIND Cfg_Novedades WHERE Cfg_Novedades.Codigo EQ Novedades_Nomina.Codigo NO-LOCK NO-ERROR.
    CREATE TDedDev.
    ASSIGN TDedDev.Age = Novedades_Nomina.Agencia
           TDedDev.Tip = Novedades_Nomina.Tipo
           TDedDev.Cod = Novedades_Nomina.Codigo
           TDedDev.Nom = Cfg_Novedades.Nombre
           TDedDev.Nit = Novedades_Nomina.Nit
           TDedDev.Fec = Novedades_Nomina.Fecha
           TDedDev.Val = Novedades_Nomina.Valor
           TDedDev.Cla = Novedades_Nomina.Clase
           TDedDev.Dia = Novedades_Nomina.Dias_Trabajados
           TDedDev.IMo = Cfg_Novedades.Id_Valor
           TDedDev.Nat = Novedades_Nomina.Naturaleza
           TDedDev.Cta = Novedades_Nomina.Cuenta
           TDedDev.Est = Novedades_Nomina.Estado_Liquidacion
           TDedDev.Usu = Novedades_Nomina.Usuario
           TDedDev.Per = Novedades_Nomina.PerNomina
           TDedDev.Isa = Cfg_Novedades.Id_Salario
           TDedDev.IBa = Cfg_Novedades.Id_Base.
    /*IF Cfg_Novedades.Id_Base THEN DO:
       IF Empleados.Estado EQ 3 THEN
          TDedDev.Nom = "Licencia Maternidad".
    END.*/
    IF Novedades_Nomina.Horas_Extras NE 0 THEN
       ASSIGN TDedDev.IEx = YES
              TDedDev.Nme = Novedades_Nomina.Horas_Extras
              TDedDev.Nom = TDedDev.Nom + " (" + STRING(TDedDev.Nme) + ")".
    IF NOT Cfg_Novedades.Id_Pago THEN DO:
       IF Novedades_Nomina.Tipo EQ 1 THEN
          ASSIGN TTDv = TTDv + Novedades_Nomina.Valor.
       ELSE
          ASSIGN TTDd = TTDd + Novedades_Nomina.Valor.
    END.
    ELSE TDedDev.Ipa = YES.
    IF LAST-OF(Novedades_Nomina.Nit) THEN DO:
       ASSIGN TTDed = 0 TTDev = 0.
       CREATE TEmpleados.
       ASSIGN TEmpleados.Age  = Novedades_Nomina.Agencia
              TEmpleados.Nit  = Clientes.Nit
              TEmpleados.Nom  = Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2
              TEmpleados.Car  = Empleados.Cargo
              TEmpleados.Est  = Empleados.Estado
              TEmpleados.TDv  = TTDv
              TEmpleados.TDd  = TTDd
              TEmpleados.Fec  = Novedades_Nomina.Fecha
              TEmpleados.Dia  = Novedades_Nomina.Dias_Trabajados
              TEmpleados.Bas  = Empleados.Salario_Mensual
              TEmpleados.Lin  = Linea + 1
              Linea           = Linea + 1.
       ASSIGN TE_Devengados  = TE_Devengados + TTDv
              TE_Deducciones = TE_Deducciones + TTDd
              TTDv = 0 TTDd = 0.
       APPLY "row-display" TO BROWSE BEmpleados.
    END.
END. 
TE_Diferencia = TE_Devengados - TE_Deducciones.
DISPLAY TE_Devengados TE_Deducciones TE_Diferencia WITH FRAME FMain.
RUN TotalesEmpleado.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Deducc_Productos wWin 
PROCEDURE Deducc_Productos :
FIND TConceptos WHERE TConceptos.Cod EQ TDedDev.Cod NO-ERROR.
IF TConceptos.Prg NE "" THEN DO:
    RUN VALUE(TConceptos.Prg) (INPUT TEmpleados.Nit, INPUT FecI, INPUT FecF, OUTPUT WVal).
    TDedDev.Val = WVal.
END.
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
  DISPLAY Cmb_Inicio Fecha_Trabajo CAgencia CTipoPago CPeriodo RCuantos NitW 
          TE_Devengados TE_Deducciones TE_Diferencia TTDev TTDed 
      WITH FRAME fMain IN WINDOW wWin.
  ENABLE RECT-11 Cmb_Inicio BUTTON-209 Btn_Prenomina CAgencia BUTTON-210 
         BUTTON-213 BUTTON-208 CTipoPago CPeriodo BUTTON-40 BtnDone RCuantos 
         BEmpleados BDevengados BDeducciones 
      WITH FRAME fMain IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-fMain}
  ENABLE RECT-12 Btn_Generar Btn_SalvarPre Btn_Cargar BUTTON-190 Btn_OutPre 
      WITH FRAME FPrenomina IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-FPrenomina}
  ENABLE BConceptos Btn_Adiciona BUTTON-37 
      WITH FRAME FConceptos IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-FConceptos}
  ENABLE BEmpAgencia BUTTON-212 
      WITH FRAME FEmpAgencia IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-FEmpAgencia}
  DISPLAY TDeb TCre 
      WITH FRAME FLiquidacion IN WINDOW wWin.
  ENABLE BLiquida BUTTON-214 BUTTON-194 BUTTON-193 
      WITH FRAME FLiquidacion IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-FLiquidacion}
  DISPLAY SIE 
      WITH FRAME FIE IN WINDOW wWin.
  ENABLE SIE BUTTON-186 
      WITH FRAME FIE IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-FIE}
  DISPLAY FecI FecF 
      WITH FRAME FEspeciales IN WINDOW wWin.
  ENABLE RECT-304 FecI FecF BEspeciales BUTTON-198 
      WITH FRAME FEspeciales IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-FEspeciales}
  ENABLE BConEmpleados BUTTON-196 
      WITH FRAME FConEmpleados IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-FConEmpleados}
  DISPLAY NEx ValEx PorEx TEx TotExt 
      WITH FRAME FExtras IN WINDOW wWin.
  ENABLE NEx BUTTON-200 
      WITH FRAME FExtras IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-FExtras}
  DISPLAY RImpresion 
      WITH FRAME FImpresion IN WINDOW wWin.
  ENABLE RImpresion BUTTON-183 BUTTON-184 
      WITH FRAME FImpresion IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-FImpresion}
  DISPLAY WKNit WkNom 
      WITH FRAME FNitContable IN WINDOW wWin.
  ENABLE WKNit BUTTON-201 
      WITH FRAME FNitContable IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-FNitContable}
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Generar wWin 
PROCEDURE Generar :
DEFINE VAR WNitIni LIKE Clientes.Nit INITIAL "0".
DEFINE VAR WNitFin LIKE Clientes.Nit INITIAL "99999999999999".
DEFINE VAR WBase   LIKE Empleados.Salario.
ASSIGN FRAME FMain CPeriodo Fecha_Trabajo Cmb_Inicio CAgencia CTipoPago.
Linea = 0.
IF RCuantos EQ 2 THEN ASSIGN WNitIni = NitW WNitFin = NitW. 
FIND WCfg WHERE WCfg.Id_Pago NO-LOCK NO-ERROR.
IF NOT AVAILABLE WCfg THEN DO:
   MESSAGE "No hay matriculado un concepto de pago!"  SKIP
           "no se puede generar una nomina sin este concepto"
       VIEW-AS ALERT-BOX.
   RETURN ERROR.
END.
IF NitAdicion NE "" THEN
   ASSIGN WNitIni = NitAdicion WNitFin = WNitIni.
IF Cmb_Inicio NE "Proceso Especial" THEN DO:
    FOR EACH Empleados WHERE 
             Empleados.Agencia      EQ INTEGER(SUBSTRING(CAgencia,1,3)) AND
             Empleados.Nit          GE WNitIni AND
             Empleados.Nit          LE WNitFin AND
             Empleados.Periodo_Pago EQ INTEGER(SUBSTRING(CTipoPago,1,2)) AND
             Empleados.Estado       NE 2       AND 
             Empleados.Estado       NE 5 NO-LOCK:
        FIND FIRST Novedades_Nomina WHERE 
             Novedades_Nomina.Agencia   EQ INTEGER(SUBSTRING(CAGencia,1,3)) AND
             Novedades_Nomina.Nit       EQ Empleados.Nit                    AND
             Novedades_Nomina.PerNomina EQ INTEGER(CPeriodo) AND 
             Novedades_Nomina.Estado_Liquidacion EQ 2 AND
             Novedades_Nomina.Clase     NE 6 NO-ERROR.
        IF NOT AVAILABLE Novedades_Nomina THEN DO:
            FIND Clientes WHERE Clientes.Nit EQ Empleados.Nit NO-LOCK.
            CREATE TEmpleados.
            ASSIGN TEmpleados.Age = Empleados.Agencia
                   TEmpleados.Nit = Empleados.Nit
                   TEmpleados.Est = Empleados.Estado
                   Templeados.Fec = Fecha_Trabajo
                   TEmpleados.Car = Empleados.Cargo
                   TEmpleados.Bas = Empleados.Salario_Mensual
                   TEmpleados.IMa = Empleados.Id_Manejo
                   TEmpleados.Lin = Linea + 1
                   Linea = Linea + 1.
            CASE Empleados.Periodo_Pago: /*dias por defecto segun periodo de pago*/
                WHEN 1 THEN TEmpleados.Dia = 7.
                WHEN 2 THEN TEmpleados.Dia = 15.
                WHEN 3 THEN TEmpleados.Dia = 30.
            END CASE.
            TEmpleados.Nom = "No se encontro en clientes".
            IF AVAILABLE Clientes THEN
               TEmpleados.Nom = Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2.
            FOR EACH Novedades_Nit WHERE 
                     Novedades_Nit.Agencia EQ Empleados.Agencia AND 
                     Novedades_Nit.Nit     EQ Empleados.Nit:
                FIND Cfg_Novedades WHERE 
                     Cfg_Novedades.Tipo     EQ Novedades_Nit.Tipo   AND
                     Cfg_Novedades.Codigo   EQ Novedades_Nit.Codigo AND
                     Cfg_Novedades.Estado   EQ 1                    NO-LOCK NO-ERROR.
                IF AVAILABLE Cfg_Novedades THEN DO:
                   CREATE TDedDev.
                   ASSIGN TDedDev.Age = INTEGER(SUBSTRING(CAgencia,1,3))
                          TDedDev.Tip = Cfg_Novedades.Tipo
                          TDedDev.Cod = Cfg_Novedades.Codigo
                          TDedDev.Nom = Cfg_Novedades.Nombre
                          TDedDev.Cla = Cfg_Novedades.Clase
                          TDedDev.Nit = Empleados.Nit
                          TDedDev.Fec = Fecha_Trabajo
                          TDedDev.IMo = Cfg_Novedades.Id_Valor
                          TDedDev.Dia = TEmpleados.Dia
                          TDedDev.Nat = Cfg_Novedades.Naturaleza
                          TDedDev.Cta = Cfg_Novedades.Cuenta
                          TDedDev.Per = INTEGER(CPeriodo)
                          TDedDev.ISa = Cfg_Novedades.Id_Salario
                          TDedDev.IBa = Cfg_Novedades.Id_Base.
                   IF Cfg_Novedades.Id_Base THEN DO:
                      TDedDev.Val = (Empleados.Salario_Mensual / 30) * TEmpleados.Dia.
                      Basico = TDedDev.Val.
                      /*IF TEmpleados.Est EQ 3 THEN
                         TDedDev.Nom = "Licencia Maternidad".*/
                   END.
                   ELSE DO:
                      CASE Cfg_Novedades.Id_Valor:
                          WHEN 1 THEN DO: 
                              WBase = TEmpleados.Bas.
                              IF Cfg_Novedades.Clase EQ 5 AND WBase LT WSMLV THEN
                                 WBase = WSMLV.
                              IF Cfg_Novedades.Id_SMLV THEN DO:
                                  IF TEmpleados.Bas GT WSMLV * Cfg_Novedades.Num_SMLV THEN
                                     ASSIGN TDedDev.Val = ((WBase / 30) * TEmpleados.Dia)
                                            TDedDev.Val = TDedDev.Val * ((Cfg_Novedades.Valor + Cfg_Novedades.PunAd_Calculo) / 100).
                                  ELSE
                                     ASSIGN TDedDev.Val = ((WBase / 30) * TEmpleados.Dia)
                                            TDedDev.Val = TDedDev.Val * ((Cfg_Novedades.Valor) / 100).
                              END.
                              ELSE TDedDev.Val = ((WBase / 30) * TEmpleados.Dia) * (Cfg_Novedades.Valor / 100).
                          END.
                          WHEN 2 THEN TDedDev.Val = Cfg_Novedades.Valor.
                          WHEN 3 THEN DO:
                              TDedDev.Val = Cfg_Novedades.Valor.
                              RUN Deducc_Productos.
                          END.
                      END CASE.
                   END.
                   IF Cfg_Novedades.Tipo EQ 1 THEN
                      ASSIGN TEmpleados.TDv = TEmpleados.TDv + TDedDev.Val
                             TTDev          = TTDev + TDedDev.Val.
                   IF Cfg_Novedades.Tipo EQ 2 THEN
                      ASSIGN TEmpleados.TDd = TEmpleados.TDd + TDedDev.Val
                             TTDed          = TTDed + TDedDev.Val.
                END.
            END.
            IF Cmb_Inicio EQ "Nómina Normal y Proceso Especial" AND
               BEspeciales:NUM-SELECTED-ROWS IN FRAME FEspeciales NE 0 THEN
               RUN Adicionar_Especiales.
            RUN Adicionar_ConPago.
        END.
    END.
END.
ELSE DO:
  FOR EACH Empleados WHERE 
           Empleados.Agencia      EQ INTEGER(SUBSTRING(CAgencia,1,3)) AND
           Empleados.Nit          GE WNitIni AND
           Empleados.Nit          LE WNitFin AND
           Empleados.Periodo_Pago EQ INTEGER(SUBSTRING(CTipoPago,1,2)) AND
           Empleados.Estado       NE 2       AND 
           Empleados.Estado       NE 5 NO-LOCK:
          FIND Clientes WHERE Clientes.Nit EQ Empleados.Nit NO-LOCK.
          CREATE TEmpleados.
          ASSIGN TEmpleados.Age = Empleados.Agencia
                 TEmpleados.Nit = Empleados.Nit
                 TEmpleados.Est = Empleados.Estado
                 Templeados.Fec = Fecha_Trabajo
                 TEmpleados.Car = Empleados.Cargo
                 TEmpleados.Bas = Empleados.Salario_Mensual
                 TEmpleados.Lin = Linea + 1
                 Linea = Linea + 1.
          CASE Empleados.Periodo_Pago: /*dias por defecto segun periodo de pago*/
              WHEN 1 THEN TEmpleados.Dia = 7.
              WHEN 2 THEN TEmpleados.Dia = 15.
              WHEN 3 THEN TEmpleados.Dia = 30.
          END CASE.
          TEmpleados.Nom = "No se encontro en clientes".
          IF AVAILABLE Clientes THEN
             TEmpleados.Nom = Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2.
          IF Cmb_Inicio EQ "Proceso Especial" AND
             BEspeciales:NUM-SELECTED-ROWS IN FRAME FEspeciales NE 0 THEN
             RUN Adicionar_Especiales.
          RUN Adicionar_ConPago.
  END.
END.
IF NitAdicion NE "" THEN ASSIGN NitAdicion = "" Id_Adicion = NO.
TE_Diferencia = TE_Devengados - TE_Deducciones.
DISPLAY TE_Devengados TE_Deducciones TE_Diferencia TTDev TTDed  WITH FRAME FMain.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject wWin 
PROCEDURE initializeObject :
RUN SUPER.
  Fecha_Trabajo:SCREEN-VALUE IN FRAME FMain = STRING(W_Fecha).
  DO i = 1 TO 26 BY 1:
     W_Ok = CPeriodo:ADD-LAST(STRING(i)) IN FRAME FMain.
  END.
  
  IF CAgencia:LIST-ITEMS EQ ? THEN DO:
      FOR EACH Agencias WHERE Agencias.Estado EQ 1 NO-LOCK BREAK BY Agencias.Agencia:
          W_Ok = CAgencia:ADD-LAST(STRING(Agencias.Agencia,"999") + " - " + Agencias.Nombre) IN FRAME FMain.
          IF FIRST-OF(Agencias.Agencia) THEN
             CAgencia:SCREEN-VALUE = CAgencia:ENTRY(1).
      END.
  END.
  APPLY "value-changed" TO CAgencia.
  FOR EACH TConceptos: DELETE TConceptos. END.
  FOR EACH Cfg_Novedades WHERE Cfg_Novedades.Tipo LT 3 AND Cfg_Novedades.Estado EQ 1 NO-LOCK:
      IF (Cfg_Novedades.Clase EQ 2 OR Cfg_Novedades.Clase EQ 6 OR Cfg_Novedades.Clase EQ 5) AND Cfg_Novedades.Codigo LT 500 THEN DO:
          CREATE TConceptos.
          ASSIGN TConceptos.Tip = Cfg_Novedades.Tipo
                 TConceptos.Cod = Cfg_Novedades.Codigo
                 TConceptos.Cla = Cfg_Novedades.Clase
                 TConceptos.Nom = Cfg_Novedades.Nombre
                 TConceptos.Idv = Cfg_Novedades.Id_Valor
                 TConceptos.Val = Cfg_Novedades.Valor
                 TConceptos.Cta = Cfg_Novedades.Cuenta
                 TConceptos.ISm = Cfg_Novedades.Id_Smlv
                 TConceptos.NSm = Cfg_Novedades.Num_Smlv
                 TConceptos.Pnt = Cfg_Novedades.Puntos
                 TConceptos.PAd = Cfg_Novedades.PunAd_Calculo
                 Tconceptos.Prg = Cfg_Novedades.Programa
                 TConceptos.Nat = Cfg_Novedades.Naturaleza
                 TConceptos.IEx = Cfg_Novedades.Id_Extras
                 TConceptos.ISa = Cfg_Novedades.Id_Salario
                 TConceptos.Ipa = Cfg_Novedades.Id_Pago
                 TConceptos.IBa = Cfg_Novedades.Id_Base
                 Tconceptos.Nit = Cfg_Novedades.Nit_Contable
                 TConceptos.Por = Cfg_Novedades.Porc_Parafiscal
                 TConceptos.Cta2 = Cfg_Novedades.Cta_Contrapartida.
      END.
      IF Cfg_Novedades.Clase EQ 6 AND Cfg_Novedades.Programa NE "" THEN
         TConceptos.Ies = YES.
      IF Cfg_Novedades.Clase EQ 6 AND TConceptos.Prg EQ "" THEN DELETE TConceptos.
  END.
  FIND Entidad WHERE Entidad.Entidad EQ W_Entidad NO-LOCK NO-ERROR.
  IF AVAILABLE Entidad THEN DO:
     IF Entidad.Ind_SMLV EQ 0 THEN DO:
        MESSAGE "No se ha entrado el indicador del SMLV en la Configuracion" SKIP
                "del registro de la organizacion. para hacer el calculo" SKIP
                "de algunas novedades de nomina, este valor debe estar" SKIP
                "configurado en la informacion de la entidad" SKIP(1)
                "Configure este indicador y vuelva a intentar hacer una prenomina"
                VIEW-AS ALERT-BOX ERROR.
        APPLY "choose" TO BtnDone IN FRAME FMain.
     END.
     ELSE DO:
        FIND Indicadores WHERE Indicadores.Indicador EQ Entidad.Ind_SMLV NO-LOCK NO-ERROR.
        IF NOT AVAILABLE Indicadores THEN DO:
            MESSAGE "No se ha entrado el indicador del SMLV en la" SKIP 
                    "configuracion de indicadores." SKIP(1)
                    "Configure este indicador y vuelva a intentar hacer una prenomina"
                    VIEW-AS ALERT-BOX ERROR.
            APPLY "choose" TO BtnDone IN FRAME FMain.
        END.
        ELSE WSMLV = Indicadores.Valor.
     END.
  END.
  ELSE DO:
      MESSAGE "No se encontro el registro de entidad para sacar" SKIP
              "el valor de el SMLV" VIEW-AS ALERT-BOX. 
  END.

  IF W_Fecha GT DATE("30/06/" + STRING(YEAR(W_Fecha))) THEN
    ASSIGN FecI = DATE("01/07/" + STRING(YEAR(W_Fecha)))
           FecF = DATE("31/12/" + STRING(YEAR(W_Fecha))).
  ELSE
    ASSIGN FecI = DATE("01/01/" + STRING(YEAR(W_Fecha)))
           FecF = DATE("30/06/" + STRING(YEAR(W_Fecha))).
  DISPLAY FecI FecF WITH FRAME FEspeciales.
  HIDE FRAME FEspeciales.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Liquidar_Nomina wWin 
PROCEDURE Liquidar_Nomina :
DEFINE VAR APagar LIKE Ahorros.Sdo_Disponible.
  DEFINE VAR CtaPago LIKE Cuentas.Cuenta.
  ASSIGN TCre = 0 TDeb = 0.
  FIND Cfg_Novedades WHERE Cfg_Novedades.Id_Pago NO-LOCK NO-ERROR.
  IF NOT AVAILABLE Cfg_Novedades THEN DO: 
     MESSAGE "No se encontro una cuenta para el pago de la cooperativa" SKIP
             "se cancela la liquidación de la nómina" SKIP
             "Comuniquese con el departamento de sistemas"
             VIEW-AS ALERT-BOX. 
     RETURN ERROR.
  END.
  ELSE CtaPago = Cfg_Novedades.Cuenta.
  FOR EACH TConta: DELETE TConta. END.
  FOR EACH TDedDev WHERE TDedDev.Val GT 0 AND NOT TDedDev.Ipa BREAK BY TDedDev.Nit:
      IF FIRST-OF(TDedDev.Nit) THEN APagar = 0.
      FIND Cuentas WHERE Cuentas.Cuenta EQ TDedDev.Cta NO-LOCK NO-ERROR.
      IF NOT AVAILABLE Cuentas THEN DO:
         MESSAGE "No hay cuenta configurada para este concepto" SKIP
                 "Concepto : " TDedDev.Cta VIEW-AS ALERT-BOX.
         RETURN ERROR.
      END.
      CREATE TConta.
      ASSIGN TConta.Agencia        = TDedDev.Age
             TConta.Cuenta         = TDedDev.Cta
             TConta.Fec_Contable   = W_Fecha
             TConta.Comentario     = TDedDev.Nom
             TConta.Usuario        = W_Usuario
             TConta.Cen_Costos     = 999
             TConta.Destino        = W_Agencia
             TConta.Doc_Referencia = CPeriodo + STRING(MONTH(W_Fecha))
             TConta.Fec_Grabacion  = TODAY
             TConta.Hora           = TIME
             TConta.Estacion       = W_Estacion NO-ERROR.
      IF TDedDev.Nco NE "" THEN 
         TConta.Nit            = TDedDev.Nco.
      ELSE 
         TConta.Nit            = TDedDev.Nit.
      IF TDedDev.Nat EQ "DB" THEN TConta.DB = TDedDev.Val NO-ERROR.
      ELSE TConta.CR = TDedDev.Val NO-ERROR.
      IF TDedDev.Tip EQ 1 THEN
         APagar = APagar + TDedDev.Val.
      ELSE
         APagar = APagar - TDedDev.Val.
      ASSIGN TCre = TCre + TConta.Cr
             TDeb = TDeb + TConta.Db.
      IF LAST-OF(TDedDev.Nit) THEN DO:
          CREATE TConta.
          ASSIGN TConta.Agencia        = INTEGER(SUBSTRING(CAgencia,1,3))
                 TConta.Comprobante    = Cbte
                 TConta.Cuenta         = Cfg_Novedades.Cuenta
                 TConta.Fec_Contable   = W_Fecha
                 TConta.Comentario     = Cfg_Novedades.Nombre
                 TConta.Usuario        = W_Usuario
                 TConta.Nit            = TDedDev.Nit
                 TConta.Cen_Costos     = 999
                 TConta.Destino        = W_Agencia
                 TConta.Num_Documento  = W_DocContab
                 TConta.Doc_Referencia = CPeriodo + STRING(MONTH(W_Fecha))
                 TConta.Fec_Grabacion  = TODAY
                 TConta.Hora           = TIME
                 TConta.Estacion       = W_Estacion NO-ERROR.
          IF Cfg_Novedades.Naturaleza EQ "DB" THEN 
             TConta.DB = APagar NO-ERROR.
          ELSE
             TConta.CR = APagar NO-ERROR.
          ASSIGN TCre = TCre + TConta.Cr
                 TDeb = TDeb + TConta.Db.
      END.
  END.
  RUN Provisiones.
  OPEN QUERY BLiquida FOR EACH TConta WHERE TConta.Cuenta NE "" BY TConta.Nit.
  DISPLAY TCre TDeb WITH FRAME FLiquidacion.
  VIEW FRAME FLiquidacion.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcesoImprimir wWin 
PROCEDURE ProcesoImprimir :
DEFINE VAR WNomEst  AS CHARACTER FORMAT "X(15)".
DEFINE VAR XLin     AS CHARACTER FORMAT "X(120)".
DEFINE VAR Registro AS INTEGER FORMAT "999".
DEFINE VAR WNomNit  AS CHARACTER FORMAT "X(30)".
DEFINE VAR WNomCta  AS CHARACTER FORMAT "X(30)".
DEFINE VAR WNomCod  AS CHARACTER FORMAT "X(30)".
DEFINE VAR WNomCon  AS CHARACTER FORMAT "X(30)".
DEFINE VAR WLinea   AS CHARACTER FORMAT "X(132)".
DEFINE VAR TotDev   AS DECIMAL FORMAT ">>>,>>>,>>9.99".
DEFINE VAR TotDed   AS DECIMAL FORMAT ">>>,>>>,>>9.99".
DEFINE VAR TotTot   AS DECIMAL FORMAT ">>>,>>>,>>9.99".

DEFINE VAR ConPag   LIKE Cfg_Novedades.Codigo.
{Incluido\RepEncabezado.i}
 FIND Cfg_Novedades WHERE Cfg_Novedades.Id_Pago NO-LOCK NO-ERROR.
 IF AVAILABLE Cfg_Novedades THEN ConPag = Cfg_Novedades.Codigo.
 CASE RImpresion:
     WHEN 1 OR WHEN 2 OR WHEN 3 THEN DO:
         CASE RImpresion:
           WHEN 1 THEN W_Reporte   = "REPORTE   : PRENOMINA AGENCIA - "  + CAgencia:SCREEN-VALUE IN FRAME FMain + " - " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am").
           WHEN 2 THEN W_Reporte   = "REPORTE   : PRENOMINA DETALLADA AGENCIA - " + CAgencia:SCREEN-VALUE IN FRAME FMain + " - " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am").
           WHEN 3 THEN W_Reporte   = "REPORTE   : TIRILLAS DE PAGO - AGENCIA " + CAgencia:SCREEN-VALUE IN FRAME FMain + " - " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am").
         END CASE.
         W_EncColumna = "AGE NIT          NOMBRE EMPLEADO                       DT          SAL.BASICO".
         IF RImpresion EQ 1 THEN
            W_EncColumna = W_EncColumna + "    TOT.DEVENG      TOT.DEDUCC   TOT.A.PAGAR".
     END.
 END CASE. 
 VIEW FRAME F-Encabezado.
 VIEW FRAME f-ftr.
 IF RImpresion EQ 5 THEN DO:
    FOR EACH TConta WHERE Tconta.Cuenta NE "":
        FIND Cuentas WHERE Cuentas.Cuenta EQ TConta.Cuenta NO-LOCK NO-ERROR.
        IF AVAILABLE Cuentas THEN WNomCta = Cuentas.Nombre.
        FIND Clientes WHERE Clientes.Nit EQ TConta.Nit NO-LOCK NO-ERROR.
        IF AVAILABLE Clientes THEN WNomNit = Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2.
        DISPLAY TConta.Cuenta
                TConta.Comentario FORMAT "X(30)" /*WNomCta*/
                TConta.Nit
                WNomNit  FORMAT "X(20)"
                Tconta.Db
                TConta.Cr
        WITH FRAME Fconta WITH WIDTH 132 NO-LABELS USE-TEXT NO-BOX STREAM-IO.
    END.
 END.
 ELSE DO:
     APPLY "chose" TO Btn_SalvarPre IN FRAME FPrenomina.
     FOR EACH TEmpleados NO-LOCK 
              BREAK BY Templeados.Nit:
         ASSIGN Registro = 0
                WLinea   = "".
      CASE TEmpleados.Est:
          WHEN 1  THEN WNomEst = "Activo".
          WHEN 2  THEN WNomEst = "Retirado".
          WHEN 3  THEN WNomEst = "Lic. Maternidad".
          WHEN 4  THEN WNomEst = "Lic. Remunerada".
          WHEN 5  THEN WNomEst = "Lic. No Remunerada".
          WHEN 6  THEN WNomEst = "Vacaciones".
          WHEN 7  THEN WNomEst = "Comisión".
          WHEN 8  THEN WNomEst = "Calamidad Doméstica".
          WHEN 9  THEN WNomEst = "Incapacidad".
          WHEN 10 THEN WNomEst = "Suspendido".
      END CASE.
         WLinea = STRING(TEmpleados.Age,"999") + " " + STRING(TEmpleados.Nit,"X(14)") 
                + " " + STRING(TEmpleados.Nom,"X(35)") + " " + STRING(TEmpleados.Dia,"99") + "        " + STRING(TEmpleados.Bas,">,>>>,>>9.99")
                +  " (" + WNomEst + ")  [Periodo: " + Cperiodo + "]".
         IF RImpresion EQ 1 THEN
            WLinea = WLinea + " " + STRING(TEmpleados.TDv,">>,>>>,>>9.99") + "   " + STRING(TEmpleados.TDd,">>,>>>,>>9.99") 
                     + STRING(TEmpleados.TDv - TEmpleados.TDd,">>>,>>>,>>9.99").
         TotDev = TotDev + TEmpleados.TDv.
         TotDed = TotDed + TEmpleados.TDd.

         DISPLAY WLinea
                 "------------------------------------------------------------------------------------------------------------------------"
         WITH FRAME FR_Empleados WIDTH 132 NO-LABELS USE-TEXT NO-BOX STREAM-IO.
        IF RImpresion EQ 2 OR RImpresion EQ 3 THEN DO:
           FOR EACH TDedDev WHERE
                    TDedDev.Nit EQ TEmpleados.Nit AND
                    TDedDev.Tip EQ 1 AND
                    TDedDev.Val GT 0
                    BREAK BY TDedDev.Tip BY TDedDev.Cod:
               WNomCon = TDedDev.Nom.
               IF TDedDev.NmE GT 0 THEN 
                  WNomCon = WNomCon + " (" + STRING(TDedDev.NmE) + ")".
               CREATE TImpresion.
               ASSIGN Registro       = Registro + 1
                      TImpresion.Num = Registro
                      TImpresion.Nit = TEmpleados.Nit
                      TImpresion.Vec = STRING(TDedDev.Cod,"999")  + "  " +
                                       STRING(WNomCon,"X(30)") + "  " +
                                       STRING(TDedDev.Val,">>>,>>>,>>9.99").
           END.
           Registro = 0.
           FOR EACH TDedDev WHERE
                    TDedDev.Nit EQ TEmpleados.Nit AND
                    TDedDev.Tip EQ 2 AND
                    TDedDev.Val GT 0 AND
                    TDedDev.Cod NE ConPag
                    BREAK BY TDedDev.Tip BY TDedDev.Cod:
               Registro       = Registro + 1.
               FIND TImpresion WHERE TImpresion.Num EQ Registro NO-ERROR.
               IF AVAILABLE TImpresion THEN DO:
                   TImpresion.Vec = TImpresion.Vec + "    " + 
                                    STRING(TDedDev.Cod,"999")  + "  " +
                                    STRING(TDedDev.Nom,"X(30)") + "  " +
                                    STRING(TDedDev.Val,">>>,>>>,>>9.99").
               END.
               ELSE DO:
                   CREATE TImpresion.
                   ASSIGN TImpresion.Num = Registro
                          TImpresion.Nit = TEmpleados.Nit
                          TImpresion.Vec = "                                                       " +
                                           STRING(TDedDev.Cod,"999")  + "  " +
                                           STRING(TDedDev.Nom,"X(30)") + "  " +
                                           STRING(TDedDev.Val,">>>,>>>,>>9.99").
               END.
           END.
           FOR EACH TImpresion BREAK BY TImpresion.Num:
               DISPLAY TImpresion.Vec WITH WIDTH 132 NO-LABELS NO-BOX USE-TEXT STREAM-IO.
           END.

           DISPLAY SKIP(1)
               "                   Devengados  :    "
               TEmpleados.TDv
               "                        Deducciones :  " 
               TEmpleados.TDd SKIP
               "Neto a Pagar : " AT 76
               (TEmpleados.TDv - TEmpleados.TDd) FORMAT ">>>,>>>,>>9.99" AT 93
           WITH FRAME FR_Empleados2 WIDTH 132 NO-LABELS USE-TEXT NO-BOX STREAM-IO.
           IF RImpresion EQ 3 THEN
              DISPLAY SKIP(2)
                   "              _____________________________                _____________________________" SKIP
                   "                     Gestion Humana                                      Empleado" SKIP
                   "________________________________________________________________________________________________________________________"
              WITH FRAME FtotEmp WIDTH 132 NO-LABELS USE-TEXT NO-BOX STREAM-IO.
           IF RImpresion EQ 2 THEN
              DISPLAY "________________________________________________________________________________________________________________________"
                  WITH FRAME FLinea WIDTH 132 NO-LABELS USE-TEXT NO-BOX STREAM-IO.
           FOR EACH TImpresion: DELETE TImpresion. END.
           DISPLAY SKIP(1) WITH FRAME FSalto.
        END.
     END.
     DISPLAY SKIP(1)
         "        Total Devengados  :    "
         TotDev
         "             Total Deducciones :  " 
         TotDed
         "  "
         (TotDev - TotDed) FORMAT ">>>,>>>,>>9.99"
         "------------------------------------------------------------------------------------------------------------------------"
     WITH FRAME FR_totales WIDTH 132 NO-LABELS USE-TEXT NO-BOX STREAM-IO.
     PAGE.    
 END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Provisiones wWin 
PROCEDURE Provisiones :
DEFINE VAR Valor LIKE Ahorros.Sdo_Disponible.
DEFINE VAR ValorTotal LIKE Ahorros.Sdo_Disponible.
FOR EACH TEmpleados:
  FOR EACH TDedDev WHERE TDedDev.Nit EQ TEmpleados.Nit AND (TDedDev.ISa OR TDedDev.Iba):
    ASSIGN Valor = Valor + TDedDev.Val
           ValorTotal = ValorTotal + Valor.
  END.
  FOR EACH TDedDev WHERE TDedDev.Nit EQ TEmpleados.Nit AND
                         TDedDev.Cla EQ 5 AND
                         TDedDev.Val GT 0:
      FIND Cfg_Novedades WHERE Cfg_Novedades.Codigo EQ TDedDev.Cod NO-LOCK NO-ERROR.
      IF AVAILABLE TConceptos THEN DO:
          CREATE TConta.
          ASSIGN TConta.Agencia        = TDedDev.Age
                 TConta.Cuenta         = TDedDev.Cta
                 TConta.Fec_Contable   = W_Fecha
                 TConta.Comentario     = "Prov: " + TDedDev.Nom
                 TConta.Usuario        = W_Usuario
                 TConta.Cen_Costos     = 999
                 TConta.Destino        = W_Agencia
                 TConta.Doc_Referencia = CPeriodo + STRING(MONTH(W_Fecha))
                 TConta.Fec_Grabacion  = TODAY
                 TConta.Hora           = TIME
                 TConta.Estacion       = W_Estacion NO-ERROR.
          IF TDedDev.Nco NE "" THEN 
             TConta.Nit            = TDedDev.Nco.
          ELSE 
             TConta.Nit            = TDedDev.Nit.
          TConta.CR = Valor * (Cfg_Novedades.Porc_Parafiscal / 100) NO-ERROR.
          ASSIGN TCre = TCre + TConta.Cr
                 TDeb = TDeb + TConta.Db.          /**/
          FIND TConta WHERE TConta.Nit EQ Cfg_Novedades.Nit NO-ERROR.
          IF NOT AVAILABLE TConta THEN DO:
              CREATE TConta.
              ASSIGN TConta.Agencia        = TDedDev.Age
                     TConta.Cuenta         = Cfg_Novedades.Cta_Contrapartida
                     TConta.Fec_Contable   = W_Fecha
                     TConta.Comentario     = "Prov: " + STRING(Cfg_Novedades.Porc_Parafiscal,">9.999") + " " + TDedDev.Nom
                     TConta.Usuario        = W_Usuario
                     TConta.Cen_Costos     = 999
                     TConta.Destino        = W_Agencia
                     TConta.Doc_Referencia = CPeriodo + STRING(MONTH(W_Fecha))
                     TConta.Fec_Grabacion  = TODAY
                     TConta.Hora           = TIME
                     TConta.Estacion       = W_Estacion
                     TConta.Nit            = Cfg_Novedades.Nit NO-ERROR.
          END.
          TConta.DB = TConta.DB + Valor * (Cfg_Novedades.Porc_Parafiscal / 100) NO-ERROR.
          ASSIGN TDeb = TDeb + (Valor * (Cfg_Novedades.Porc_Parafiscal / 100)).
      END.
  END.
  Valor = 0.
END.
FOR EACH Cfg_Novedades WHERE Cfg_Novedades.Tipo EQ 4 NO-LOCK:
    /*MESSAGE Valortotal Cfg_Novedades.Porc_Parafiscal.*/
    IF Cfg_Novedades.Porc_Parafiscal EQ 0.000 THEN 
       MESSAGE Cfg_Novedades.Codigo.
    CREATE TConta.
    ASSIGN TConta.Agencia        = INTEGER(SUBSTRING(CAgencia:SCREEN-VALUE IN FRAME Fmain,1,3))
           TConta.Cuenta         = Cfg_Novedades.Cuenta
           TConta.Fec_Contable   = W_Fecha
           TConta.Comentario     = STRING(Cfg_Novedades.Porc_Parafiscal) + " %" + Cfg_Novedades.Nombre
           TConta.Usuario        = W_Usuario
           TConta.Cen_Costos     = 999
           TConta.Destino        = W_Agencia
           TConta.Doc_Referencia = CPeriodo + STRING(MONTH(W_Fecha))
           TConta.Fec_Grabacion  = TODAY
           TConta.Hora           = TIME
           TConta.Estacion       = W_Estacion NO-ERROR.
           TConta.Nit            = Cfg_Novedades.Nit.
    TConta.CR = ValorTotal * (Cfg_Novedades.Porc_Parafiscal / 100) NO-ERROR.
    ASSIGN TCre = TCre + TConta.Cr.          /**/
    CREATE TConta.
    ASSIGN TConta.Agencia        = INTEGER(SUBSTRING(CAgencia:SCREEN-VALUE IN FRAME Fmain,1,3))
           TConta.Cuenta         = Cfg_Novedades.Cta_Contrapartida
           TConta.Fec_Contable   = W_Fecha
           TConta.Comentario     = STRING(Cfg_Novedades.Porc_Parafiscal) + " %" + Cfg_Novedades.Nombre
           TConta.Usuario        = W_Usuario
           TConta.Cen_Costos     = 999
           TConta.Destino        = W_Agencia
           TConta.Doc_Referencia = CPeriodo + STRING(MONTH(W_Fecha))
           TConta.Fec_Grabacion  = TODAY
           TConta.Hora           = TIME
           TConta.Estacion       = W_Estacion
           TConta.Nit            = Cfg_Novedades.Nit NO-ERROR.
    TConta.DB = ValorTotal * (Cfg_Novedades.Porc_Parafiscal / 100) NO-ERROR.
    ASSIGN TDeb = TDeb + TConta.DB. /*(ValorTotal * (TConceptos.Por / 100)).*/

END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Recalculo wWin 
PROCEDURE Recalculo :
DEFINE VAR WBase       LIKE Empleados.Salario.
DEFINE VAR WLin        AS INTEGER FORMAT "99".
DEFINE VAR ValExtras   AS DECIMAL FORMAT ">>>,>>>,>>9.99".
DEFINE VAR TotExtras   AS DECIMAL FORMAT ">>>,>>>,>>9.99".
DEFINE VAR OtroSalario AS DECIMAL FORMAT ">>>,>>>,>>9.99".
DEFINE VAR TotOtroSal  AS DECIMAL FORMAT ">>>,>>>,>>9.99".
DEFINE VAR VrExtra     AS DECIMAL FORMAT ">>>,>>>,>>9.99".

WLin = TEmpleados.Lin. 
ASSIGN TEmpleados.TDv = 0
       TTDed = 0
       TTDev = 0
       TEmpleados.TDd = 0
       TE_Devengados  = 0
       TE_Deducciones = 0.


FOR EACH TExtras: DELETE TExtras. END.

FOR EACH TDedDev WHERE TDedDev.Nit EQ TEmpleados.Nit AND TDedDev.ISa:
    /*CREATE TExtras.
    ASSIGN TExtras.Nit = TDedDev.Nit
           TExtras.VrE = TExtras.VrE + (TDedDev.VrE * TDedDev.NmE). */
    OtroSalario = OtroSalario + TDedDev.Val.
END.
/*DEFINE VARIABLE SalPeriodo AS DECIMAL FORMAT ">>>,>>>,>>>9.99".
FIND TDedDev WHERE TDedDev.Cod EQ 100 AND TDedDev.Nit EQ TEmpleados.Nit NO-ERROR.
IF AVAILABLE TDedDev THEN SalPeriodo = TDedDev.Val * 2.*/
FOR EACH TDedDev WHERE TDedDev.Nit EQ TEmpleados.Nit AND NOT TDedDev.IPa:
       TDedDev.Dia = TEmpleados.Dia.
       FIND Cfg_Novedades WHERE Cfg_Novedades.Codigo EQ TDedDev.Cod NO-LOCK NO-ERROR.
       IF Cfg_Novedades.Id_Base THEN DO:
          /*IF TDedDev.Imo NE 3 THEN*/
             ASSIGN TDedDev.Val = (TEmpleados.Bas / 30) * TEmpleados.Dia
                    Basico = TDedDev.Val.
/*          ELSE
             ASSIGN Basico = TDedDev.Val
                    TEmpleados.Bas = TDedDev.Val.*/
       END.
       ELSE DO:
           IF Cfg_Novedades.Clase NE 6 THEN DO:
               IF NOT TDedDev.IEx THEN DO:
                   IF Cfg_Novedades.Clase NE 5 THEN 
                      ASSIGN TotExtras  = 0
                             TotOtroSal = 0.
                   ELSE
                      ASSIGN TotExtras  = ValExtras
                             TotOtroSal = OtroSalario.
                   CASE Cfg_Novedades.Id_Valor:
                       WHEN 1 THEN DO: 
                           WBase =  TEmpleados.Bas. /*SalPeriodo.*/
                           IF Cfg_Novedades.Clase EQ 5 AND WBase LT WSMLV THEN
                              WBase = WSMLV.
                           IF Cfg_Novedades.Id_SMLV THEN DO:
                               IF TEmpleados.Bas GT WSMLV * Cfg_Novedades.Num_SMLV THEN
                                  ASSIGN TDedDev.Val = ((WBase / 30) * TEmpleados.Dia) + ValExtras + TotOtroSal
                                         TDedDev.Val = TDedDev.Val * ((Cfg_Novedades.Valor + Cfg_Novedades.PunAd_Calculo) / 100).
                               ELSE
                                  ASSIGN TDedDev.Val = ((WBase / 30) * TEmpleados.Dia) + ValExtras + TotOtroSal
                                         TDedDev.Val = TDedDev.Val * ((Cfg_Novedades.Valor) / 100).
                           END.
                           ELSE ASSIGN TDedDev.Val = ((WBase / 30) * TEmpleados.Dia + ValExtras + TotOtroSal)
                                       TDedDev.Val = TDedDev.Val  * (Cfg_Novedades.Valor / 100).
                       END.
                       WHEN 2 THEN TDedDev.Val = Cfg_Novedades.Valor.
                       WHEN 3 THEN DO:
                           IF Cfg_Novedades.Clase EQ 4 THEN RUN Deducc_Productos.
                       END.
                   END CASE.
               END.
               ELSE DO: 
                   IF TDedDev.VrE EQ 0.00 THEN
                      TDedDev.Val = ValExtras.
                   ELSE
                      TDedDev.Val = TDedDev.NmE * TDedDev.VrE.
               END.
           END.
       END.
       IF Cfg_Novedades.Tipo EQ 1 THEN
          ASSIGN TEmpleados.TDv = TEmpleados.TDv + TDedDev.Val.
       IF Cfg_Novedades.Tipo EQ 2 THEN
          ASSIGN TEmpleados.TDd = TEmpleados.TDd + TDedDev.Val.
END.

FIND TDedDev WHERE TDedDev.Nit EQ TEmpleados.Nit AND TDedDev.IPa NO-ERROR.
WNit = TDedDev.Nit.

IF AVAILABLE TDedDev THEN
   TDedDev.Val = TEmpleados.TDv - TEmpleados.TDd.

FOR EACH TEmpleados:
    TE_Devengados  = TE_Devengados + TEmpleados.TDv.
END.
FOR EACH TEmpleados:
    TE_Deducciones  = TE_Deducciones + TEmpleados.TDd.
END.
TE_Diferencia = TE_Devengados - TE_Deducciones.

DISPLAY TE_Devengados TE_Deducciones TE_Diferencia WITH FRAME FMain.

FOR EACH TEmpleados:
    ASSIGN WDiferencia = TEmpleados.TDv - TEmpleados.TDd.
END.

/*OPEN QUERY BEmpleados FOR EACH TEmpleados.*/
w_ok = BROWSE BEmpleados:REFRESH().
w_ok = BROWSE BEmpleados:SELECT-FOCUSED-ROW().


OPEN QUERY BDevengados FOR EACH TDedDev WHERE TDedDev.Nit EQ TEmpleados.Nit AND TDeddev.Tip EQ 1 BY TDedDev.Cod.
OPEN QUERY BDeducciones FOR EACH TDedDev WHERE TDedDev.Nit EQ TEmpleados.Nit AND TDeddev.Tip EQ 2 BY TDedDev.IPa.
/*IF Fila EQ 0 THEN Fila = 1.
W_ok = BEmpleados:SELECT-ROW(Fila).
w_ok = BROWSE BEmpleados:SELECT-FOCUSED-ROW(). */
RUN TotalesEmpleado.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE TotalesEmpleado wWin 
PROCEDURE TotalesEmpleado :
ASSIGN TTDev = 0 TTDed = 0.
FOR EACH TDedDev WHERE TDedDev.Nit EQ TEmpleados.Nit AND NOT TDedDev.Ipa:
    IF TDedDev.Tip EQ 1 THEN
       TTDev = TTDev + TDedDev.Val.
    ELSE
       TTDed = TTDed + TDedDev.Val.
END.
DISPLAY TTDed TTDev WITH FRAME FMain.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Verifica_Empleado wWin 
PROCEDURE Verifica_Empleado :
DEFINE OUTPUT PARAMETER WOk2 AS LOGICAL INITIAL NO.   
   Faltan = 0.
  FOR EACH Empleados WHERE Empleados.Agencia EQ INTEGER(SUBSTRING(CAGencia,1,3)):
    IF Empleados.PerNom_Actual EQ INTEGER(CPeriodo) THEN NEXT.
    ELSE DO:
        FIND FIRST Novedades_Nomina WHERE 
             Novedades_Nomina.Agencia   EQ INTEGER(SUBSTRING(CAGencia,1,3)) AND
             Novedades_Nomina.Nit       EQ Empleados.Nit                    AND
             Novedades_Nomina.PerNomina EQ INTEGER(CPeriodo) AND
             Novedades_Nomina.Estado_Liquidacion EQ 2 NO-LOCK NO-ERROR.
        IF NOT AVAILABLE Novedades_Nomina THEN
           Faltan = Faltan + 1.
    END.
  END.
  IF Faltan GT 0 THEN
     WOk2 = YES.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

