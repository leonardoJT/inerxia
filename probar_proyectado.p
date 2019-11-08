DEFINE VAriable W_fec AS DATE NO-UNDO.                            /* Fecha 1er Pago */
DEFINE VARIABLE wcuo             LIKE creditos.cuota NO-UNDO.     /* Cuota */
DEFINE VARIABLE wtas             LIKE creditos.tasa NO-UNDO.      /* Tasa */
DEFINE VARIABLE Wpla             LIKE creditos.plazo NO-UNDO.     /* Plazo */
DEFINE VARIABLE WFrec            AS INTEGER NO-UNDO.              /* Frecuencia de pago creditos.Per_Pago*/
DEFINE variable P_Proyectado    AS DECIMAL FORMAT "->>>,>>>,>>>,>>9" INITIAL 0 NO-UNDO.
DEFINE VARIABLE WPeriodoTrans           AS INTEGER NO-UNDO.

FIND FIRST creditos WHERE nit = "22098388" .

      ASSIGN W_fec = creditos.fec_desembolso
      wcuo  = creditos.cuota       
      wtas  = creditos.tasa / 1200
      Wpla  = creditos.plazo       
      WFrec = 30.
  ASSIGN WPeriodoTrans = TRUNCATE((TODAY - fec_desembolso) / 30, 0).
  ASSIGN P_Proyectado = decimal(wcuo * ( EXP(  (1 + wtas), ( (wpla - WPeriodoTrans) - 1) )  / (wtas *  EXP(( 1 + wtas), (wpla - WPeriodoTrans)))) ).
  DISPLA p_proyectado monto num_credito pagare tasa / 12 plazo.
