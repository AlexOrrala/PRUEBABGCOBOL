      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. YOUR-PROGRAM-NAME.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01  BANDERA-SALIR           PIC X VALUE "N".
       01  OPCION                  PIC X(2).
       01  IDEMPLEADO              PIC 9 VALUE 1.
       01  IDEMPLEADOTEMP          PIC 9 VALUE 1.
       01  CEDULABUSCA             PIC X(10).
       01  SALARIONETOBUSCA        PIC 9999V99.
       01  SALARIO-STRING          PIC X(10).

       01  EMPLEADO OCCURS 10 TIMES.
           05 CEDULA               PIC X(10).
           05 Nombre               PIC X(10).
           05 Salario              PIC 9999V99.
           05 DeduccionImpuestos   PIC 9999V99.
           05 DeduccionSeguro      PIC 9999V99.
           05 SalarioNeto          PIC 9999V99.
           05 DatosSocioeconomicos PIC X(10).



       01  DeduccionesGeneral      PIC 9999V99.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM UNTIL BANDERA-SALIR = "S"
            DISPLAY "[1] Ingresar nuevo empleado."
            DISPLAY "[2] Calcular salario neto."
            DISPLAY "[3] Mostrar informaci�n de empleado."
            DISPLAY "[4] Filtrar"
            DISPLAY "[5] Salir."
            ACCEPT OPCION
               EVALUATE OPCION
                   WHEN '1'
                       PERFORM INGRESAR-EMPLEADO
                   WHEN '2'
                       PERFORM CALCULAR-NETO
                   WHEN '3'
                       PERFORM MOSTRAR-INFO
                   WHEN '4'
                       DISPLAY "[1] Salario neto"
                       DISPLAY "[2] Nivel educativo"
                       DISPLAY "[3] Salario o nivel educativo"
                       ACCEPT OPCION
                        EVALUATE OPCION
                        WHEN '1'
                           PERFORM FILTRAR-NETO
                        WHEN OTHER
                        DISPLAY "Opci�n inv�lida. Intente de nuevo."
                   WHEN '5'
                       MOVE "S" TO BANDERA-SALIR
                       STOP RUN
                   WHEN OTHER
                       DISPLAY "Opci�n inv�lida. Intente de nuevo."
            END-PERFORM.
       VALIDAR-ENTRADA.
           IF CEDULABUSCA ALPHABETIC
               AND FUNCTION LENGTH(CEDULABUSCA)= 10
               DISPLAY "�Error! La c�dula no puede estar vac�a."
               DISPLAY "Por favor ingrese nuevamente la c�dula."
               ACCEPT CEDULABUSCA
               PERFORM VALIDAR-ENTRADA
           END-IF.

        VALIDAR-DECIMAL.
           IF SALARIO-STRING = SPACES
               DISPLAY "�Error! El salario no puede estar vac�o."
               PERFORM VALIDAR-DECIMAL
           ELSE
               IF SALARIO-STRING NUMERIC
                 MOVE FUNCTION NUMVAL(SALARIO-STRING)
                 TO SALARIO(IDEMPLEADO)
               ELSE
                   DISPLAY "El valor ingresado no es un n�mero decimal."
                   PERFORM VALIDAR-DECIMAL
               END-IF.

       INGRESAR-EMPLEADO.

           IF IDEMPLEADO <=9
            DISPLAY "Ingrese ID del empleado:"
            ACCEPT CEDULABUSCA
            PERFORM VALIDAR-ENTRADA

            DISPLAY "Ingrese Nombre del empleado:"
            ACCEPT Nombre(IDEMPLEADO)

            DISPLAY "Ingrese Salario bruto (n�mero decimal)."
            ACCEPT Salario(IDEMPLEADO)

            DISPLAY "Ingrese Deducci�n por impuestos (n�mero decimal)."
            ACCEPT DeduccionImpuestos(IDEMPLEADO)
            DISPLAY "Ingrese Deducci�n por seguro (n�mero decimal)."
            ACCEPT DeduccionSeguro(IDEMPLEADO)
            DISPLAY "Ingrese DatosSocioecon�micos (n�mero decimal)."
            ACCEPT DatosSocioeconomicos(IDEMPLEADO)

            ADD 1 TO IDEMPLEADO
           END-IF.

       CALCULAR-NETO.

           DISPLAY "INGRESE LA CEDULA DEL USUARIO A CALCULAR"
           ACCEPT CEDULABUSCA.

           PERFORM VARYING IDEMPLEADOTEMP FROM 1 BY 1 UNTIL
           IDEMPLEADOTEMP > 10
               IF CEDULA(IDEMPLEADOTEMP) = CEDULABUSCA
                   MOVE IDEMPLEADOTEMP TO IDEMPLEADO
                   EXIT PERFORM
               END-IF
           END-PERFORM.

           ADD DeduccionSeguro(IDEMPLEADO)
           TO DeduccionImpuestos(IDEMPLEADO)
           GIVING DeduccionesGeneral.
           SUBTRACT Salario(IDEMPLEADO) FROM DeduccionesGeneral
           GIVING SalarioNeto(IDEMPLEADO).
           DISPLAY "El salario neto del usuario de ",
           CEDULA(IDEMPLEADO), " es:",SalarioNeto(IDEMPLEADO).


       MOSTRAR-INFO.
           DISPLAY "INGRESE LA CEDULA DEL USUARIO A CONSULTAR"
           ACCEPT CEDULABUSCA.

           PERFORM VARYING IDEMPLEADOTEMP FROM 1 BY 1 UNTIL
           IDEMPLEADOTEMP > 10
               IF CEDULA(IDEMPLEADOTEMP) = CEDULABUSCA
                   MOVE IDEMPLEADOTEMP TO IDEMPLEADO
                   EXIT PERFORM
               END-IF
           END-PERFORM.

           DISPLAY "ID del empleado:",CEDULA(IDEMPLEADO).

           DISPLAY "Nombre del empleado:",Nombre(IDEMPLEADO).

           DISPLAY "Salario bruto (n�mero decimal)."
           ,Salario(IDEMPLEADO).
           DISPLAY "Deducci�n por impuestos (n�mero decimal)."
           ,DeduccionImpuestos(IDEMPLEADO).
           DISPLAY "Deducci�n por seguro (n�mero decimal)."
           ,DeduccionSeguro(IDEMPLEADO).
           DISPLAY "Salario Neto (n�mero decimal)."
           ,SalarioNeto(IDEMPLEADO).
           DISPLAY "DatosSocioecon�micos (n�mero decimal)."
           ,DatosSocioeconomicos(IDEMPLEADO).


       FILTRAR-NETO.
           DISPLAY "INGRESE EL NETO A FILTRAR"
           ACCEPT SALARIONETOBUSCA.

           PERFORM VARYING IDEMPLEADOTEMP FROM 1 BY 1 UNTIL
           IDEMPLEADOTEMP > 10
               IF SalarioNeto(IDEMPLEADOTEMP) = SALARIONETOBUSCA
                   DISPLAY "ID del empleado:",CEDULA(IDEMPLEADO)
                   DISPLAY "Nombre del empleado:",Nombre(IDEMPLEADO)
                   DISPLAY "Ingrese Salario bruto (n�mero decimal)."
                   ,Salario(IDEMPLEADO)
                   DISPLAY "Deducci�n por impuestos (n�mero decimal)."
                   ,DeduccionImpuestos(IDEMPLEADO)
                   DISPLAY "Deducci�n por seguro (n�mero decimal)."
                   ,DeduccionSeguro(IDEMPLEADO)
                   DISPLAY "DatosSocioecon�micos (n�mero decimal)."
                   ,DatosSocioeconomicos(IDEMPLEADO)
               END-IF
           END-PERFORM.



       STOP RUN.
       END PROGRAM YOUR-PROGRAM-NAME.
