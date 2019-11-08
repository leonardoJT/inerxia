{Incluido\VARIABLE.I "SHARED"}

RUN Rutinas.r PERSISTENT SET W_Manija.

DEFINE INPUT PARAMETER p_transa AS INTEGER.
DEFINE INPUT PARAMETER P_Comentario AS CHARACTER FORMAT "X(62)".

DEFINE VAR W_NomPcto AS CHARACTER FORMAT "X(15)".
DEFINE VAR W_NomEntidad AS CHARACTER FORMAT "X(30)".
DEFINE VAR W_NitEnti AS CHARACTER.
DEFINE VAR W_ConcatEnti AS CHARACTER FORMAT "X(57)".
DEFINE VAR W_DescOpe AS CHARACTER FORMAT "X(40)".
DEFINE VAR W_VrOpera AS DECIMAL FORMAT ">>,>>>,>>>,>>>".
DEFINE VAR W_Comentario AS CHARACTER FORMAT "X(32)".
DEFINE VAR W_Nomofi AS CHARACTER FORMAT "X(30)".
DEFINE VAR W_NomCli AS CHARACTER FORMAT "X(30)".
DEFINE VAR W_totopera AS DECIMAL FORMAT ">>,>>>,>>>,>>>".
DEFINE VAR W_NomUsu AS CHARACTER.
DEFINE VAR W_Rpta AS LOGICAL.
DEFINE VAR Listado AS CHARACTER.

FIND FIRST Taquilla WHERE Taquilla.Nro_Transaccion = p_transa NO-LOCK NO-ERROR.
IF NOT AVAILABLE(Taquilla) THEN DO:
    MESSAGE "No se encuentra en taquilla el documento que desea imprimir."
        VIEW-AS ALERT-BOX TITLE "Validación.(f-Taquil.p)".

    RETURN.
END.

w_NomCli = "".

FIND FIRST Clientes WHERE Clientes.nit = Taquilla.Nit NO-LOCK NO-ERROR.
IF AVAILABLE(Clientes) THEN
    /* oakley */

    W_Nomcli = Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2.

  FIND Usuarios WHERE Usuarios.Usuario EQ Taquilla.Usuario 
                  AND Usuarios.agencia EQ Taquilla.agencia
              NO-LOCK NO-ERROR.
  IF AVAILABLE(Usuarios) THEN
    W_NomUsu = STRING(Usuarios.Usuario) + " " + Usuarios.Nombre.
  ELSE
    W_NomUsu = "No encontro".
  w_NomOfi = "".
  w_ConcatEnti = "".
  FIND agencia WHERE agencia.agencia = Taquilla.agencia NO-LOCK NO-ERROR.
  IF AVAILABLE(agencia) THEN DO:
   W_NomOfi = agencia.Nombre.
   FIND Entidad WHERE Entidad.Entidad EQ agencia.Entidad NO-LOCK NO-ERROR.
   IF AVAILABLE(Entidad) THEN DO:
      ASSIGN W_NomEntidad   = Entidad.Nombre. 
           W_NitEnti   = Entidad.Nit.
          W_ConcatEnti = TRIM(W_NomEntidad) + " " + "  Nit: " + w_NitEnti.
   END.
  END.


  DEFINE FRAME F-Encabezado
    HEADER
        W_ConCatenti                  AT COL 3  ROW 1
        "Comprobante de Taquilla"     AT COL 3  ROW 2
        "Fecha:"                      AT COL 68 ROW 2
        TODAY                         AT COL 77 ROW 2
       
        "Oficina Nro          :"      AT COL 3  ROW 3
        Taquilla.agencia              AT COL 26 ROW 3
        W_NomOfi                      AT COL 31 ROW 3
        "Hora :"                      AT COL 68 ROW 3 STRING(TIME,"HH:MM:SS") SKIP
       
        "Nro Transacción      :"      AT COL 3  ROW 4
        Taquilla.Nro_Transaccion      AT COL 26 ROW 4        
        "Pagina:"                     AT COL 68 ROW 4 PAGE-NUMBER FORMAT ">9"
       
        "Fecha Contabilización:"      AT COL 3  ROW 5
        Taquilla.Fec_Transaccion     AT COL 26 ROW 5
       
        "Usuario Digitador    :"      AT COL 3  ROW 6
        W_NomUsu                      AT COL 26 ROW 6

        "Comentario           :"      AT COL 3  ROW 7
        P_Comentario                  AT COL 26 ROW 7 SKIP(1)
       
        "Cuenta"                      AT COL 3  ROW 8
        "Nit"                         AT COL 19 ROW 8
        "Nombre"                      AT COL 38 ROW 8 
/*        "Consigno"                  AT COL 60 ROW 8*/
        "V A L O R "                  AT COL 107 ROW 8 /*skip(1)*/
  
  WITH DOWN WIDTH 300 FRAME F-Encabezado NO-LABEL NO-BOX NO-UNDERLINE PAGE-TOP 
                              USE-TEXT.

  DEFINE FRAME F_Movimiento
     Taquilla.Nro_cuenta      AT 3
     Taquilla.Nit             AT 19
     W_NomCli                 AT 35  FORMAT "X(25)"
     w_Comentario             AT 62
     W_VrOpera                AT 103
  WITH 9 DOWN size 150 by 10 FRAME F_Movimiento USE-TEXT NO-BOX NO-LABEL STREAM-IO.

  DEFINE FRAME F-Totales
    HEADER
    "Total Operación "                      AT COL  50 ROW 1
    W_totopera                              AT COL 103 ROW 1
    "Elaboró:______________________"        AT COL 2  ROW 5
    "Aprobó:______________________"         AT COL 47 ROW 5
    "Ordenó:______________________"         AT COL 92 ROW 5
  WITH WIDTH 150 FRAME F-Totales PAGE-BOTTOM NO-LABEL NO-BOX NO-UNDERLINE USE-TEXT.

  Listado = W_PathSpl + "L_Ingreso.Lst".

  DEFINE VAR W_Dispositivo AS CHARACTER INITIAL "P".  
  RUN P-DisPos IN W_Manija (INPUT-OUTPUT Listado, INPUT-OUTPUT W_Dispositivo).
  IF W_Dispositivo = "" THEN
     RETURN.
  OUTPUT TO VALUE(listado) NO-ECHO PAGED PAGE-SIZE 22.

/*  CASE W_Dispositivo:
 *     WHEN "I" THEN
 *       OUTPUT TO PRINTER        NO-ECHO PAGE-SIZE 28.
 *     OTHERWISE 
 *       OUTPUT TO VALUE(listado) NO-ECHO PAGED PAGE-SIZE 28.
 *   END CASE.*/

  VIEW FRAME F-Encabezado.
  VIEW FRAME F-Totales.  
  w_comentario = "Comentario: ".

  FOR EACH Taquilla WHERE Taquilla.Nro_Transaccion EQ P_Transa
                  NO-LOCK
                  BREAK BY Taquilla.Nro_Transaccion:
      IF FIRST-OF(Taquilla.Nro_Transaccion) THEN DO:      
        FIND Operacion WHERE Operacion.Cod_Operacion EQ Taquilla.Cod_Operacion
                     NO-LOCK NO-ERROR.
        IF AVAILABLE(Operacion) THEN
          W_DescOpe = Operacion.Nom_Operacion.
        ELSE
          W_DescOpe = "Operación no Existe".
      END.

      IF Taquilla.Tip_Producto = 1 THEN DO:  /* Ahorros */
        FIND Pro_ahorros WHERE Pro_Ahorros.Cod_Ahorro EQ Taquilla.Cod_Producto
                           NO-LOCK NO-ERROR.
        W_NomPcto = "".
        IF AVAILABLE(Pro_Ahorros) THEN 
          W_NomPcto = TRIM(Pro_Ahorros.Nom_Producto).

        IF Taquilla.Naturaleza = "DB" THEN DO:  /* Es un Retiro */
          ASSIGN w_VrOpera = 0.
          IF Taquilla.Val_Cheque GT 0 THEN 
            ASSIGN w_VrOpera = Taquilla.Val_Cheque.
          ELSE
            ASSIGN w_VrOpera = Taquilla.Val_Efectivo.
        END.
        ELSE DO:
          IF Taquilla.Naturaleza = "CR" THEN DO:  /* Es una Consignacion */
            ASSIGN w_VrOpera = 0.
            IF Taquilla.Val_Cheque GT 0 THEN 
              ASSIGN w_VrOpera = Taquilla.Val_Cheque.
            ELSE
              ASSIGN w_VrOpera = Taquilla.Val_Efectivo.
          END.
        END.
      END.
      ELSE DO:
        IF Taquilla.Tip_Producto = 2 OR Taquilla.Tip_Producto = 4 THEN DO:  /* Credito y Especiales */
          IF Taquilla.Tip_Producto = 2 THEN DO:
            FIND Pro_Creditos WHERE Pro_Creditos.Cod_Credito EQ Taquilla.Cod_Producto
                                NO-LOCK NO-ERROR.
            W_NomPcto = "".
            IF AVAILABLE(Pro_Creditos) THEN 
              W_NomPcto = TRIM(Pro_Creditos.Nom_Producto).
          END.
          ELSE DO:
            FIND Pro_Especiales WHERE Pro_Especiales.Cod_Producto EQ Taquilla.Cod_Producto
                                  AND Pro_Especiales.agencia      EQ  Taquilla.agencia
                              NO-LOCK NO-ERROR.
            W_NomPcto = "".
            IF AVAILABLE(Pro_Especiales) THEN 
              W_NomPcto = TRIM(Pro_Especiales.Nom_Producto).
          END.

          IF Taquilla.Naturaleza = "DB" THEN DO:  /* Es un cargo */
            ASSIGN w_VrOpera = 0.
            IF Taquilla.Val_Cheque GT 0 THEN 
              ASSIGN w_VrOpera = Taquilla.Val_Cheque.
            ELSE
              ASSIGN w_VrOpera = Taquilla.Val_Efectivo.
          END.
          ELSE DO:
            IF Taquilla.Naturaleza = "Cr" THEN DO:  /* Es un cargo */
              ASSIGN w_VrOpera = 0.
              IF Taquilla.Val_Cheque GT 0 THEN 
                ASSIGN w_VrOpera = Taquilla.Val_Cheque.
              ELSE
                ASSIGN w_VrOpera = Taquilla.Val_Efectivo.
            END.
          END.
        END.
        ELSE DO:  /* Otros Pctos */
          IF Taquilla.Naturaleza = "Cr" THEN DO:
            ASSIGN w_VrOpera = 0.
            IF Taquilla.Val_Cheque GT 0 THEN 
              ASSIGN w_VrOpera = Taquilla.Val_Cheque.
            ELSE
              ASSIGN w_VrOpera = Taquilla.Val_Efectivo.
          END.
          ELSE DO:
            IF Taquilla.Naturaleza = "Db" THEN DO:
              ASSIGN w_VrOpera = 0.
              IF Taquilla.Val_Cheque GT 0 THEN 
                ASSIGN w_VrOpera = Taquilla.Val_Cheque.
              ELSE
                ASSIGN w_VrOpera = Taquilla.Val_Efectivo.
            END.
          END.
        END.
      END.

      FIND Operacion WHERE Operacion.Cod_Operacion EQ Taquilla.Cod_Operacion
                   NO-LOCK NO-ERROR.
      IF AVAILABLE(Operacion) THEN
        W_DescOpe = Operacion.Nom_Operacion.
      ELSE
        W_DescOpe = "Operación no Existe".
      W_Comentario = SUBSTRING(TRIM(W_DescOpe),1,16) + " " + TRIM(W_NomPcto).

      DISPLAY 
        Taquilla.Nro_cuenta 
        Taquilla.Nit
        W_NomCli
        W_Comentario
/*        w_VrOpera */
        w_VrOpera 
      WITH /*10 down*/ FRAME F_Movimiento.
      Assign W_totopera= W_totopera + W_Vropera.
  END.  
/*  DISPLAY 
 *     W_Comentario      AT 1
 *   WITH WIDTH 200 FRAME f-cuerpo USE-TEXT STREAM-IO NO-LABELS NO-BOX.*/

  OUTPUT CLOSE.
  IF W_Dispositivo = "P" THEN  
    RUN Pantalla IN W_Manija (INPUT listado).
  IF W_Dispositivo = "I" THEN
      RUN adecomm/_osprint.r ( INPUT  ?, INPUT listado,INPUT 2,INPUT  1,INPUT  1,
                               INPUT  99999,OUTPUT W_Rpta).
  ELSE
    IF W_Dispositivo <> "A" THEN
       OS-DELETE VALUE(LISTADO).
