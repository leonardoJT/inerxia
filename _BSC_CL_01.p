DEFINE INPUT  PARAMETER Ind  LIKE Ind_PUB.Codigo.
DEFINE INPUT  PARAMETER Ent  LIKE Cfg_Pub.Ente.
DEFINE INPUT  PARAMETER Mes  AS INTEGER FORMAT "99".
DEFINE INPUT  PARAMETER Cta  LIKE Cuentas.Cuenta.
DEFINE OUTPUT PARAMETER Tot  AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>9".
DEFINE OUTPUT PARAMETER Met  AS DECIMAL FORMAT "->>>,>>>,>>>,>>9".
DEFINE OUTPUT PARAMETER Cum  AS DECIMAL FORMAT "->>>>.99".

DEFINE VAR i AS INTEGER.
DEFINE VAR Sf LIKE Sal_Cuenta.Sal_Inicial.
DEFINE VAR Si LIKE Sal_Cuenta.Sal_Inicial.

DEFINE VAR S1  LIKE Sal_Cuenta.Sal_Inicial.
DEFINE VAR S59 LIKE Sal_Cuenta.Sal_Inicial.
DEFINE VAR S31 LIKE Sal_Cuenta.Sal_Inicial.
DEFINE VAR S32 LIKE Sal_Cuenta.Sal_Inicial.
DEFINE VAR S33 LIKE Sal_Cuenta.Sal_Inicial.


CASE Ind:
    /*ind019*/ /*incremento de asociados*/
    WHEN 19 THEN DO:
        FOR EACH Clientes WHERE 
                 MONTH(Clientes.Fec_ingreso) EQ Mes AND
                 YEAR(Clientes.Fec_ingreso)  EQ YEAR(TODAY) AND
                 Clientes.Estado EQ 1 AND
                 Clientes.Usuario EQ SUBSTRING(Ent,7,2) NO-LOCK:
                 Tot = Tot + 1.
        END.
    END.
    /*ind046*/ /*a la vista*/
    WHEN 46 THEN DO:
        FOR EACH Ahorros WHERE 
                 Ahorros.Tip_Ahorro          EQ 1   AND
                 MONTH(Ahorros.Fec_Apertura) EQ Mes AND
                 YEAR(Ahorros.Fec_Apertura)  EQ YEAR(TODAY) AND
                 Ahorros.Estado              EQ 1   AND 
                 Ahorros.Usu_Creacion        EQ SUBSTRING(Ent,7,2) NO-LOCK:
                 Tot = Tot + 1.
        END.
    END.
    /*ind047*/ /*contractual*/
    WHEN 47 THEN DO:
        FOR EACH Ahorros WHERE 
                 Ahorros.Tip_Ahorro          EQ 2   AND
                 MONTH(Ahorros.Fec_Apertura) EQ Mes AND
                 YEAR(Ahorros.Fec_Apertura)  EQ YEAR(TODAY) AND
                 Ahorros.Usu_Creacion        EQ SUBSTRING(Ent,7,2) AND
                 Ahorros.Estado              EQ 1 NO-LOCK:
                 Tot = Tot + 1.
        END.
    END.
    /*ind048*/ /*a termino*/
    WHEN 48 THEN DO:
        FOR EACH Ahorros WHERE 
                 Ahorros.Tip_Ahorro          EQ 3   AND
                 MONTH(Ahorros.Fec_Apertura) EQ Mes AND
                 YEAR(Ahorros.Fec_Apertura)  EQ YEAR(TODAY) AND
                 Ahorros.Estado              EQ 1   AND
                 Ahorros.Usu_Creacion        EQ SUBSTRING(Ent,7,2) NO-LOCK:
                 Tot = Tot + 1.
        END.
    END.
END CASE.


FIND Cfg_PUB WHERE 
     Cfg_PUB.Ente   EQ Ent AND
     Cfg_PUB.Codigo EQ Ind NO-LOCK NO-ERROR.
IF AVAILABLE Cfg_PUB THEN DO:
   
   ASSIGN Met = Cfg_PUB.Metas[Mes].
          Cum = (Tot / Met).
END.











