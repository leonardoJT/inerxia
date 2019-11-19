
OUTPUT TO "c:\info_fodun\PUC.csv".

FOR EACH cuentas WHERE  cuenta NE "" AND
/*                         cuenta BEGINS "28" AND */
/*                         tipo EQ 2 AND          */
                        TRUE:
/*     UPDATE  id_nit      = TRUE. */

    DISPLAY cuenta nombre
            IF tipo EQ 1 THEN "Mayor" ELSE "Mvto"
            id_nit VIEW-AS FILL-IN
            id_cenCosto VIEW-AS FILL-IN
            id_base VIEW-AS FILL-IN WITH WIDTH 150.
END.
OUTPUT CLOSE.
