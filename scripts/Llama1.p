DEFINE VAR cont AS INTEGER.

MESSAGE "Inicia"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

/*OUTPUT TO c:\INFO_Fodun\FechasInicioCreditos.txt.*/

FOR EACH creditos WHERE /*creditos.agencia = 4
                    AND*/ creditos.sdo_capital > 0
                    AND creditos.plazo > 1
                    AND creditos.cod_credito = 123
                    AND creditos.fec_desembolso <= 02/28/2011
                    AND creditos.monto < creditos.sdo_capital
                    /*AND nit = "23267116"*/
                    /*AND num_credito = 620020802*/
                    /*AND creditos.fec_desembolso <> creditos.fec_pagAnti*/
                    AND creditos.FOR_pago = 1
                    /*AND DAY(creditos.fec_desembolso) = 31*/:
    /*DISPLAY creditos.nit creditos.num_credito creditos.fec_pagAnti.*/

    /*IF DAY(creditos.fec_pagAnti) = 30 OR DAY(creditos.fec_pagAnti) = 31 THEN
        creditos.fec_pagAnti = creditos.fec_pagAnti + 10.
    ELSE DO:
        creditos.fec_pagAnti = ADD-INTERVAL(creditos.fec_pagAnti,1,"months").
        creditos.fec_pagAnti = DATE(MONTH(creditos.fec_pagAnti),10,YEAR(creditos.fec_pagAnti)).
    END.*/

    /*creditos.fec_pagAnti = creditos.fec_pagAnti + 21.*/
    
    creditos.fec_desembolso = 01/06/2011.
    creditos.fec_PagAnti = 07/05/2011.
    creditos.monto = creditos.sdo_capital.
    
    RUN U:\Prog\crearControlPagos.p(INPUT creditos.nit,
                            INPUT creditos.num_credito,
                            INPUT creditos.tasa).

    cont = cont + 1.
END.

MESSAGE "Finalizó" cont
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
