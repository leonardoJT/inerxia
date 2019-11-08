
OUTPUT TO c:\INFO_cooprudea\AhorrosSub.csv.

define variable  tNit LIKE Clientes.Nit.
define variable  tNom AS CHARACTER FORMAT "X(30)".
define variable  tFec AS DATE FORMAT "99/99/99".
define variable  tTip AS CHARACTER FORMAT "X(3)".
define variable  tDoc LIKE Mov_Ahorros.Num_Documento.
define variable  tUsu LIKE Mov_Ahorros.Usuario.
define variable  tCon AS DECIMAL FORMAT "->>>,>>>,>>>,>>9".
define variable  tRet AS DECIMAL FORMAT "->>>,>>>,>>>,>>9".
DEFINE VARIABLE  tLiquidado AS INTEGER     NO-UNDO.

DEFINE VAR WTipo AS CHARACTER FORMAT "X(8)".
DEFINE VAR TotCon AS DECIMAL FORMAT ">>>>,>>>,>>9".
DEFINE VAR TotRet AS DECIMAL FORMAT ">>>>,>>>,>>9".
DEFINE VARIABLE tNumMto AS INTEGER     NO-UNDO.
DEFINE VARIABLE tIdt AS INTEGER     NO-UNDO.
DEFINE VARIABLE tConcepto AS CHARACTER FORMAT "x(6)"  NO-UNDO.
DEFINE VAR SALDO AS DECIMAL FORMAT "->>>>,>>>,>>9".


FOR EACH clientes WHERE Tipo_Vinculo = 1 NO-LOCK:
    tIdt = 0.
    FOR EACH ahorros WHERE ahorros.nit = clientes.nit AND (tip_ahorro EQ 1 OR tip_ahorro EQ 2) NO-LOCK BY cue_ahorro:
        FOR EACH  Mov_Ahorros WHERE
                  Mov_Ahorros.Agencia       EQ 1
             AND Mov_Ahorros.Cod_Ahorro     EQ ahorros.cod_ahorro
             AND mov_ahorros.cue_ahorros    EQ ahorros.cue_ahorros
             AND Mov_Ahorros.Nit            EQ ahorros.nit
             AND Mov_Ahorros.Fecha          GE DATE(11, 01, 2008)
             AND Mov_Ahorros.Fecha          LE TODAY NO-LOCK BY Mov_Ahorros.cod_ahorro BY Mov_Ahorros.Cue_Ahorros BY Mov_Ahorros.Fecha:
             FIND Operacion WHERE Operacion.Cod_Operacion EQ Mov_Ahorros.Cod_Operacion NO-LOCK NO-ERROR.
             IF AVAILABLE(Operacion) THEN DO:
                 ASSIGN tCon = 0
                        tRet = 0.

                 ASSIGN tNumMto = tNumMto + 1.
                 
                 ASSIGN tNit = Mov_Ahorros.nit
                        tFec = Mov_Ahorros.Fecha
                        tDoc = Mov_Ahorros.Num_Documento
                        tUsu = Mov_Ahorros.Usuario.
                 CASE Operacion.Ctrl_EfeChe:
                     WHEN 1 THEN tTip = "Efectivo".
                     WHEN 2 THEN tTip = "Cheques".
                     WHEN 3 THEN tTip = "Ninguno".
                 END CASE.
                 IF Operacion.Tipo_Operacion EQ 1 THEN 
                    tCon = Mov_Ahorros.Val_Efectivo + Mov_Ahorros.Val_Cheque.
                 ELSE
                    tRet = Mov_Ahorros.Val_Efectivo + Mov_Ahorros.Val_Cheque.

                    IF mov_Ahorros.Cpte NE 5 THEN
                       DO:
                           /*DISP tnit tConcepto tNumMto tfec tNumMto Mov_Ahorros.Sdo_Disponible tNumMto  tret tcon Mov_Ahorros.Sdo_Disponible Mov_Ahorros.Descrip tIdt.*/
                           PUT tnit ";" Mov_Ahorros.Cue_Ahorros ";" tNumMto ";" tfec ";" tNumMto ";" tNumMto ";" Mov_Ahorros.Sdo_Disponible ";" tNumMto ";" tret ";" tcon ";" Mov_Ahorros.Sdo_Disponible ";" Mov_Ahorros.Descrip ";" tIdt SKIP(0).
                           ASSIGN SALDO = Mov_Ahorros.Sdo_Disponible.
                       END.
                    ELSE
                        DO:
                           ASSIGN tLiquidado =  tLiquidado + tCon - tRet.
                        END.

             END.   /* Cierra Available Operacion */
        END.        /* Cierra ciclo mov_ahorros */
        
 /* Si no tiene movimientos, coloca el saldo */

        FIND FIRST Mov_Ahorros WHERE Mov_Ahorros.Fecha GE DATE(11, 01, 2008) AND Mov_Ahorros.Fecha LE TODAY AND Mov_Ahorros.nit = ahorros.nit AND Mov_Ahorros.Cod_Ahorro EQ ahorros.cod_ahorro AND mov_ahorros.cue_ahorros = ahorros.cue_ahorros NO-LOCK NO-ERROR.
        IF AVAILABLE Mov_Ahorros THEN DO:
            PUT ahorros.nit ";" ahorros.cue_ahorros ";" tNumMto ";;" tNumMto ";" tNumMto ";;" tNumMto  ";0;" tLiquidado ";"  ahorros.sdo_disponible ";Suma de Saldo + intereses liquidados en el mes;"  tIdt SKIP(0).
        END.
        ELSE DO:
            PUT ahorros.nit ";" ahorros.cue_ahorros ";" tNumMto ";" TODAY ";" tNumMto ";" tNumMto ";;" tNumMto  ";;"  ";"  ahorros.sdo_disponible ";Saldo disponible;"  tIdt SKIP(0).
        END.


        ASSIGN  SALDO = 0
                tnit = ""
                tLiquidado = 0.
        tIdt = tIdt + 1.
    END.            /* Cierra ciclo ahorros */
END.                /* Cierra el ciclo de clientes */




RUN ExportaAhorros.p.
RUN ExportaAportes.p.
RUN ExportaCreditos.p.
