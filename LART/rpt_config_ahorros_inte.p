DEFINE VAR wlinea AS CHAR FORMAT "x(30)".
DEFINE VAR wtitulo AS CHAR FORMAT "x(300)" NO-UNDO.
ASSIGN  wtitulo = "Clase_Producto;Cod_Producto;linea;Cod_Operacion;Base; Cod_base;".
        ASSIGN  wtitulo = wtitulo + "CtaCr_DifCob;CtaCr_DifCobAso;CtaCr_Liq;CtaCr_LiqAso;".
        ASSIGN  wtitulo = wtitulo + "CtaCr_Mora; CtaCr_MoraAso;CtaCr_Ret;CtaDb_DifCob;".
        ASSIGN  wtitulo = wtitulo + "CtaDb_DifCobAso;CtaDb_Liq;CtaDb_LiqAso; CtaDb_Mora; CtaDb_MoraAso;CtaDb_Ret;".
        ASSIGN  wtitulo = wtitulo + "CtaInt_Ant;CtaInt_AntAso;Cta_CauCr;Cta_SucyAge".
DEFINE VAR wsalida AS CHAR FORMAT "x(300)" NO-UNDO.    
OUTPUT TO c:\INFO_fodun\config_inte.csv.
        DISPLAY  wtitulo 
        NO-LABEL
        WITH WIDTH 400.
    
FOR EACH Liqui_Int WHERE Liqui_Int.Clase_Producto = 1 NO-LOCK BY Liqui_Int.Cod_Producto :
    FIND FIRST Pro_Ahorros WHERE Pro_Ahorros.Cod_ahorro = Liqui_Int.Cod_Producto NO-LOCK NO-ERROR.
    wlinea =  " ".    
    IF AVAILABLE Pro_Ahorros  THEN  DO: 
                wlinea = Pro_Ahorros.Nom_Producto.
    END.
    ASSIGN  wsalida = string(Liqui_Int.Clase_Producto)   + ";" +
            string(Liqui_Int.Cod_Producto)      + ";" +
            wlinea + ";" +
            STRING (Liqui_Int.Cod_Operacion)    + ";" +
            STRING (Liqui_Int.Base )            + ";" +
            STRING (Liqui_Int.Cod_base)         + ";" +
            Liqui_Int.CtaCr_DifCob     + ";" +
            Liqui_Int.CtaCr_DifCobAso  + ";" +
            Liqui_Int.CtaCr_Liq        + ";" +
            Liqui_Int.CtaCr_LiqAso     + ";" +
            Liqui_Int.CtaCr_Mora       + ";" +
            Liqui_Int.CtaCr_MoraAso    + ";" +
            Liqui_Int.CtaCr_Ret        + ";" +
            Liqui_Int.CtaDb_DifCob     + ";" +
            Liqui_Int.CtaDb_DifCobAso  + ";" +
            Liqui_Int.CtaDb_Liq        + ";" +
            Liqui_Int.CtaDb_LiqAso     + ";" +
            Liqui_Int.CtaDb_Mora       + ";" +
            Liqui_Int.CtaDb_MoraAso    + ";" +
            Liqui_Int.CtaDb_Ret        + ";" +
            Liqui_Int.CtaInt_Ant       + ";" +
            Liqui_Int.CtaInt_AntAso    + ";" +
            Liqui_Int.Cta_CauCr        + ";" +
            Liqui_Int.Cta_SucyAge.
        DISPLAY  wsalida 
        NO-LABEL
        WITH WIDTH 500.
    
END.
OUTPUT CLOSE.    
