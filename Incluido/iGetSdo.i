&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Include 
/*------------------------------------------------------------------------
    File        : 
    Purpose     :

    Syntax      :

    Description :

    Author(s)   :
    Created     :
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getAgencia Include 
FUNCTION getAgencia RETURNS CHARACTER
    ( INPUT pcCodigo AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getAhorro Include 
FUNCTION getAhorro RETURNS CHARACTER
  ( INPUT piCodigo AS INTEGER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getCargo Include 
FUNCTION getCargo RETURNS CHARACTER
    ( INPUT piCodigo AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getCliente Include 
FUNCTION getCliente RETURNS CHARACTER
  ( INPUT pcCodigo AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getCredito Include 
FUNCTION getCredito RETURNS CHARACTER
    ( INPUT piCodigo AS INTEGER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getCtrlEfeChe Include 
FUNCTION getCtrlEfeChe RETURNS CHARACTER
  (INPUT piCodOpe   AS INTEGER) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getEmpresa Include 
FUNCTION getEmpresa RETURNS CHARACTER
  ( INPUT piCodigo AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getHVEncabezado Include 
FUNCTION getHVEncabezado RETURNS CHARACTER
    ( INPUT piCodigo AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getInstancia Include 
FUNCTION getInstancia RETURNS CHARACTER
    ( INPUT piCodigo AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getProducto Include 
FUNCTION getProducto RETURNS CHARACTER
  ( INPUT piClase AS INTEGER,   /* Clase de Producto */
    INPUT piCodPro AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getProfesion Include 
FUNCTION getProfesion RETURNS CHARACTER
  ( INPUT piCodigo AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getRegalo Include 
FUNCTION getRegalo RETURNS CHARACTER
  ( INPUT piCodigo AS INTEGER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getRelacion Include 
FUNCTION getRelacion RETURNS CHARACTER
    ( INPUT piCodigo AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getTpContrato Include 
FUNCTION getTpContrato RETURNS CHARACTER
  ( INPUT piCodigo AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getTpVinculo Include 
FUNCTION getTpVinculo RETURNS CHARACTER
  ( INPUT piCodigo AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getUbicacion Include 
FUNCTION getUbicacion RETURNS CHARACTER
  (INPUT pcCodigo AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getUsuario Include 
FUNCTION getUsuario RETURNS CHARACTER
  ( INPUT pcCodigo AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getVNUD Include 
FUNCTION getVNUD RETURNS DECIMAL
  ( INPUT pcCodigo  AS CHARACTER,
    INPUT pdaFecha  AS DATE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getVNUM Include 
FUNCTION getVNUM RETURNS DECIMAL
  ( INPUT pcCodigo  AS CHARACTER,
    INPUT pdaFecha  AS DATE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Include 


/* ***************************  Main Block  *************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getAgencia Include 
FUNCTION getAgencia RETURNS CHARACTER
    ( INPUT pcCodigo AS INTEGER) :
  /*------------------------------------------------------------------------------
    Purpose:  
      Notes:  
  ------------------------------------------------------------------------------*/

      DEFINE VARIABLE vcReturn AS CHARACTER   NO-UNDO.
      FIND FIRST agencias WHERE agencias.agencia EQ pcCodigo NO-LOCK NO-ERROR.
      IF AVAILABLE agencias THEN DO:
          ASSIGN vcReturn = TRIM(STRING(agencias.agencia)) + " - " + agencias.nombre.
          RETURN vcReturn.
      END.
      ELSE
          RETURN "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getAhorro Include 
FUNCTION getAhorro RETURNS CHARACTER
  ( INPUT piCodigo AS INTEGER ) :
/*------------------------------------------------------------------------------
  Purpose:  Devuelve descripción del código de Ahorro
    Notes:  
------------------------------------------------------------------------------*/
    DEFINE VARIABLE vcValor AS CHARACTER INITIAL "" NO-UNDO.
    FIND FIRST pro_Ahorros WHERE Pro_Ahorros.Cod_Ahorro EQ piCodigo NO-LOCK NO-ERROR.
    IF AVAILABLE Pro_Ahorros THEN 
        ASSIGN vcValor = TRIM(STRING(Pro_Ahorros.Cod_Ahorro)) + " - " + Pro_Ahorros.Nom_Producto.
    ELSE
        vcValor = "".

    RETURN vcValor.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getCargo Include 
FUNCTION getCargo RETURNS CHARACTER
    ( INPUT piCodigo AS INTEGER) :
  /*------------------------------------------------------------------------------
    Purpose:  devuelve la descripción del código de Cargo
      Notes:  en tabla varios, varios.tipo = 2 corresponde a Cargos
  ------------------------------------------------------------------------------*/

      FIND FIRST Varios WHERE Varios.Tipo EQ 2 AND  /*Tipo = 2 Corresponde a Cargos*/
          Varios.Codigo EQ piCodigo NO-LOCK NO-ERROR.
          IF AVAILABLE Varios THEN 
              RETURN Varios.Descripcion.
          ELSE
              RETURN "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getCliente Include 
FUNCTION getCliente RETURNS CHARACTER
  ( INPUT pcCodigo AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    DEFINE VARIABLE vcReturn AS CHARACTER   NO-UNDO.
    FIND FIRST clientes WHERE clientes.nit EQ pcCodigo NO-LOCK NO-ERROR.
    IF AVAILABLE clientes THEN DO:
        IF clientes.nit EQ "" THEN 
            ASSIGN vcReturn = "".
        ELSE
            ASSIGN vcReturn = TRIM(clientes.nit) + " - "  + 
                TRIM(clientes.apellido1) + " " + 
                TRIM(clientes.apellido2) + " " + 
                TRIM(Clientes.nombre).
        RETURN vcReturn.
    END.
    ELSE
        RETURN "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getCredito Include 
FUNCTION getCredito RETURNS CHARACTER
    ( INPUT piCodigo AS INTEGER ) :
  /*------------------------------------------------------------------------------
    Purpose:  Devuelve descripción del código de Credito   
      Notes:  
  ------------------------------------------------------------------------------*/

    DEFINE VARIABLE vcValor AS CHARACTER INITIAL "" NO-UNDO.
    FIND FIRST pro_Credito WHERE pro_Credito.Cod_credito EQ piCodigo NO-LOCK NO-ERROR.
    IF AVAILABLE pro_Credito THEN 
        ASSIGN vcValor = TRIM(STRING(pro_Credito.Cod_Credito)) + " - " + pro_Credito.Nom_Producto.
    ELSE
        vcValor = "".

    RETURN vcValor.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getCtrlEfeChe Include 
FUNCTION getCtrlEfeChe RETURNS CHARACTER
  (INPUT piCodOpe   AS INTEGER):     /* Cód. Operacion*/
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    DEFINE VARIABLE vcValor AS CHARACTER INITIAL "" NO-UNDO.

      FIND Operacion WHERE Operacion.Cod_Operacion EQ piCodOpe NO-LOCK NO-ERROR.
      IF AVAILABLE Operacion THEN DO:
          CASE Operacion.Ctrl_EfeChe:
              WHEN 1 THEN
                  ASSIGN vcValor = "Efe.".
              WHEN 2 THEN
                  ASSIGN vcValor = "Che.".
              OTHERWISE
                  ASSIGN vcValor = "N/A".
          END CASE.
      END.

      RETURN vcValor.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getEmpresa Include 
FUNCTION getEmpresa RETURNS CHARACTER
  ( INPUT piCodigo AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

    FIND FIRST Empresas WHERE Empresas.Cod_Empresa EQ piCodigo NO-LOCK NO-ERROR.
    IF AVAILABLE Empresas THEN 
        RETURN Empresas.Alias_Empresa.
    ELSE
        RETURN "".

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getHVEncabezado Include 
FUNCTION getHVEncabezado RETURNS CHARACTER
    ( INPUT piCodigo AS INTEGER) :
  /*------------------------------------------------------------------------------
    Purpose:  devuelve la descripción del concepto Observaciones hoja de vida
      Notes:  en tabla varios, varios.tipo = 18 corresponde a Conceptos observacion hoja de vida
  ------------------------------------------------------------------------------*/
    DEFINE VARIABLE vcValor AS CHARACTER INITIAL "" NO-UNDO.
    FIND FIRST Varios WHERE Varios.Tipo EQ 18 AND  /*Tipo = 18 Corresponde a Conceptos observacion hoja de vida*/
        Varios.Codigo EQ piCodigo NO-LOCK NO-ERROR.
    IF AVAILABLE Varios THEN 
        ASSIGN vcValor = TRIM(STRING(Varios.Codigo)) + " - " + Varios.Descripcion.
    ELSE
        ASSIGN vcValor = "".

    RETURN vcValor.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getInstancia Include 
FUNCTION getInstancia RETURNS CHARACTER
    ( INPUT piCodigo AS INTEGER) :
  /*------------------------------------------------------------------------------
    Purpose:  
      Notes:  
  ------------------------------------------------------------------------------*/

      DEFINE VARIABLE vcReturn AS CHARACTER   NO-UNDO.

      FIND FIRST instancias WHERE instancias.instancia EQ piCodigo NO-LOCK NO-ERROR.
      IF AVAILABLE instancias THEN DO:
          ASSIGN vcReturn = TRIM(STRING(instancias.instancia)) + " - " + Instancias.Nom_Instancia.
          RETURN vcReturn.
      END.
      ELSE
          RETURN "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getProducto Include 
FUNCTION getProducto RETURNS CHARACTER
  ( INPUT piClase AS INTEGER,   /* Clase de Producto */
    INPUT piCodPro AS INTEGER) : /* Codigo Producto */
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

DEFINE VARIABLE vcValor AS CHARACTER FORMAT "X(30)"  NO-UNDO.

CASE piClase:
    WHEN 1 THEN DO:
        FIND FIRST pro_Ahorros WHERE Pro_Ahorros.Cod_Ahorro EQ piCodPro NO-LOCK NO-ERROR.
        IF AVAILABLE Pro_Ahorros THEN 
            ASSIGN vcValor = TRIM(STRING(Pro_Ahorros.Cod_Ahorro)) + " - " + Pro_Ahorros.Nom_Producto.
        ELSE
            vcValor = "".
    END.
    WHEN 2 THEN DO:
        FIND FIRST pro_Credito WHERE pro_Credito.Cod_credito EQ piCodPro NO-LOCK NO-ERROR.
        IF AVAILABLE pro_Credito THEN 
            ASSIGN vcValor = TRIM(STRING(pro_Credito.Cod_Credito)) + " - " + pro_Credito.Nom_Producto.
        ELSE
            vcValor = "".
    END.
END CASE.
RETURN vcValor.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getProfesion Include 
FUNCTION getProfesion RETURNS CHARACTER
  ( INPUT piCodigo AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  devuelve la descripción del código de profeción
    Notes:  en tabla varios, varios.tipo = 1 corresponde a profesiones
------------------------------------------------------------------------------*/

    FIND FIRST Varios WHERE Varios.Tipo EQ 1 AND 
        Varios.Codigo EQ piCodigo NO-LOCK NO-ERROR.
        IF AVAILABLE Varios THEN 
            RETURN Varios.Descripcion.
        ELSE
            RETURN "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getRegalo Include 
FUNCTION getRegalo RETURNS CHARACTER
  ( INPUT piCodigo AS INTEGER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    DEFINE VARIABLE vcReturn AS CHARACTER   NO-UNDO.
    FIND FIRST regalos WHERE regalos.regalo EQ piCodigo NO-LOCK NO-ERROR.
    IF AVAILABLE regalos THEN DO:
        ASSIGN vcReturn = (regalos.nombre).
        RETURN vcReturn.
    END.
    ELSE
        RETURN "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getRelacion Include 
FUNCTION getRelacion RETURNS CHARACTER
    ( INPUT piCodigo AS INTEGER) :
  /*------------------------------------------------------------------------------
    Purpose:  devuelve la descripción del código de Relación
      Notes:  en tabla varios, varios.tipo = 3 corresponde a Relaciones
  ------------------------------------------------------------------------------*/

      FIND FIRST Varios WHERE Varios.Tipo EQ 3 AND  /*Tipo = 3 Corresponde a Relaciones */
          Varios.Codigo EQ piCodigo NO-LOCK NO-ERROR.
          IF AVAILABLE Varios THEN 
              RETURN Varios.Descripcion.
          ELSE
              RETURN "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getTpContrato Include 
FUNCTION getTpContrato RETURNS CHARACTER
  ( INPUT piCodigo AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    DEFINE VARIABLE vcValor AS CHARACTER   NO-UNDO.

    CASE piCodigo:
        WHEN 0 THEN vcValor = "Ninguno".
        WHEN 1 THEN vcValor = "Indefinido".
        WHEN 2 THEN vcValor = "Fijo".
        WHEN 3 THEN vcValor = "Labor Prestada".
        WHEN 4 THEN vcValor = "Prestacion de Servicios".
    END CASE.


  RETURN vcValor.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getTpVinculo Include 
FUNCTION getTpVinculo RETURNS CHARACTER
  ( INPUT piCodigo AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    DEFINE VARIABLE vcValor AS CHARACTER   NO-UNDO.

    CASE piCodigo:
        WHEN 1 THEN vcValor = "Asociado".           
        WHEN 2 THEN vcValor = "Cliente No Asociado".
        WHEN 3 THEN vcValor = "Tercero".            
        WHEN 4 THEN vcValor = "Proveedor".          
    END CASE.

  RETURN vcValor.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getUbicacion Include 
FUNCTION getUbicacion RETURNS CHARACTER
  (INPUT pcCodigo AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

    DEFINE VARIABLE vcValor AS CHARACTER INITIAL "" NO-UNDO.

    FIND FIRST ubicacion WHERE Ubicacion.Ubicacion BEGINS SUBSTRING(pcCodigo,1,5) AND
             Ubicacion.Tipo EQ "C" NO-LOCK NO-ERROR. 
    IF AVAILABLE ubicacion THEN DO:
        ASSIGN vcValor = nombre.
        FIND Ubicacion WHERE Ubicacion.Ubicacion EQ pcCodigo AND
            Ubicacion.Tipo EQ "B" NO-LOCK NO-ERROR.
       IF AVAILABLE Ubicacion THEN 
       DO: 
          vcValor = vcValor + "  " + Ubicacion.Nombre.
       END.
    END.
    
    RETURN vcValor.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getUsuario Include 
FUNCTION getUsuario RETURNS CHARACTER
  ( INPUT pcCodigo AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

    DEFINE VARIABLE vcReturn AS CHARACTER   NO-UNDO.

    FIND FIRST usuarios WHERE usuarios.usuario EQ pcCodigo NO-LOCK NO-ERROR.
    IF AVAILABLE usuarios THEN DO:
        ASSIGN vcReturn = usuarios.usuario + " - " + usuarios.nombre.
        RETURN vcReturn.
    END.
    ELSE
        RETURN "".


END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getVNUD Include 
FUNCTION getVNUD RETURNS DECIMAL
  ( INPUT pcCodigo  AS CHARACTER,
    INPUT pdaFecha  AS DATE ) :
/*------------------------------------------------------------------------------
  Purpose:  Devuelve el valor acumulado de transacciones dia para un NIT
    Notes:  
------------------------------------------------------------------------------*/
    
    DEFINE VARIABLE vdeValor AS DECIMAL INITIAL 0 NO-UNDO.
    FOR EACH ControlSipla WHERE ControlSipla.Nit EQ pcCodigo NO-LOCK 
        BY ControlSipla.Nit
        BY ControlSipla.fecha:
        IF (MONTH(ControlSipla.fecha) EQ MONTH(pdaFecha) AND YEAR(ControlSipla.fecha) EQ YEAR(pdaFecha) )
            THEN
            ASSIGN vdeValor = vdeValor + ControlSipla.CS_TotalDia.
    END.

    RETURN vdeValor.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getVNUM Include 
FUNCTION getVNUM RETURNS DECIMAL
  ( INPUT pcCodigo  AS CHARACTER,
    INPUT pdaFecha  AS DATE ) :
/*------------------------------------------------------------------------------
  Purpose:  Devuelve el valor acumulado de transacciones MES para un NIT
    Notes:  
------------------------------------------------------------------------------*/
    
    DEFINE VARIABLE vdeValor AS DECIMAL INITIAL 0 NO-UNDO.
    FOR EACH ControlSipla WHERE ControlSipla.Nit EQ pcCodigo NO-LOCK 
        BY ControlSipla.Nit
        BY ControlSipla.fecha:
        IF (MONTH(ControlSipla.fecha) EQ MONTH(pdaFecha) AND 
            YEAR(ControlSipla.fecha) EQ YEAR(pdaFecha) ) THEN
            ASSIGN vdeValor = vdeValor + ControlSipla.CS_TotalMes.
    END.

    RETURN vdeValor.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

