****************************************************************
*    å⁄ãqñºÅ@Å@Å@Å@Å@Å@Å@ÅFÅ@ÅiäîÅjÉTÉJÉ^ÇÃÉ^ÉlìaÅ@Å@Å@Å@Å@Å@Å@*
*    ã∆ñ±ñºÅ@Å@Å@Å@Å@Å@Å@ÅFÅ@ç›å…ä«óùÉVÉXÉeÉÄÅ@Å@Å@Å@Å@Å@Å@Å@Å@*
*    ÉÇÉWÉÖÅ[ÉãñºÅ@Å@Å@Å@ÅFÅ@ì˙éüêUë÷ÅiÉfÅ[É^ï“èWÅ^êUï™ÇØÅjÅ@Å@*
*    çÏê¨ì˙Å^çXêVì˙Å@Å@Å@ÅFÅ@2000/06/21                        *
*    çÏê¨é“Å^çXêVé“Å@Å@Å@ÅFÅ@NAV                               *
*    èàóùäTóvÅ@Å@Å@Å@Å@Å@ÅFÅ@êUë÷ÉfÅ[É^Çï“èWÇµÅAäeïîñÂñàÇ…ÉfÅ[*
*                            É^ÇêUÇËï™ÇØÇÈÅB                  *
*2002/05/13 ì`ãÊÅÅÇSÇPÅCÇTÇPÇÃéûÅAçXêVãÊï™ÇèCê≥               *
*2002/08/28 ì`ãÊÅÅÇRÇOÅiçÏã∆é¿ê—ÇÃéûÅjÅ@çXêVãÊï™ÇèCê≥         *
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SFU0103B.
 AUTHOR.                NAV.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU-GP6000.
 OBJECT-COMPUTER.       FUJITSU-GP6000.
 SPECIAL-NAMES.
         STATION   IS   STAT
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*---<<  ÇsÇnÇjÇtÉtÉ@ÉCÉã  >>---*
     SELECT   TOKU      ASSIGN    TO        DA-01-S-TOKU
                        ORGANIZATION        IS   SEQUENTIAL
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   TOK-STATUS.
*
*---<<  ñ{é–ï™  >>---*
     SELECT   HON       ASSIGN    TO        DA-01-S-HON
                        ORGANIZATION        IS   SEQUENTIAL
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   HON-STATUS.
*
*---<<  ïüâ™ï™  >>---*
     SELECT   FUK       ASSIGN    TO        DA-01-S-FUK
                        ORGANIZATION        IS   SEQUENTIAL
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   FUK-STATUS.
*
*---<<  êÂë‰ï™  >>---*
     SELECT   SEN       ASSIGN    TO        DA-01-S-SEN
                        ORGANIZATION        IS   SEQUENTIAL
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   SEN-STATUS.
*
*---<<  â™éRï™  >>---*
     SELECT   OKA       ASSIGN    TO        DA-01-S-OKA
                        ORGANIZATION        IS   SEQUENTIAL
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   OKA-STATUS.
*
*---<<  ñkäCìπï™  >>---*
     SELECT   HOK       ASSIGN    TO        DA-01-S-HOK
                        ORGANIZATION        IS   SEQUENTIAL
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   HOK-STATUS.
*
*---<<  ëÂç„ï™  >>---*
     SELECT   OSA       ASSIGN    TO        DA-01-S-OSA
                        ORGANIZATION        IS   SEQUENTIAL
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   OSA-STATUS.
*
*---<<  ïîñÂéÊà¯êÊÉ}ÉXÉ^  >>---*
     SELECT   BUTOKMF1  ASSIGN    TO        DA-01-VI-BUTOKMF1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   BU1-F01
                        FILE      STATUS    IS   BU1-STATUS.
*
*---<<  ïîñÂéÊà¯êÊÉ}ÉXÉ^  >>---*
     SELECT   BUTOKMF2  ASSIGN    TO        DA-01-VI-BUTOKMF2
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   BU2-F03
                        FILE      STATUS    IS   BU2-STATUS.
*
 DATA                   DIVISION.
 FILE                   SECTION.
*---<<  ÇsÇnÇjÇtÉtÉ@ÉCÉã  >>---*
 FD  TOKU     BLOCK  CONTAINS  1  RECORDS
              LABEL  RECORD    IS STANDARD.
     COPY     FURIKAE   OF        XFDLIB
              JOINING   TO0       PREFIX.
*---<<  ñ{é–ÉtÉ@ÉCÉã  >>---*
 FD  HON      BLOCK  CONTAINS  1  RECORDS
              LABEL  RECORD    IS STANDARD.
     COPY     FURIKAF   OF        XFDLIB
              JOINING   HON       PREFIX.
*---<<  ïüâ™ÉtÉ@ÉCÉã  >>---*
 FD  FUK      BLOCK  CONTAINS  1  RECORDS
              LABEL  RECORD    IS STANDARD.
     COPY     FURIKAF   OF        XFDLIB
              JOINING   FUK       PREFIX.
*---<<  êÂë‰ÉtÉ@ÉCÉã  >>---*
 FD  SEN      BLOCK  CONTAINS  1  RECORDS
              LABEL  RECORD    IS STANDARD.
     COPY     FURIKAF   OF        XFDLIB
              JOINING   SEN       PREFIX.
*---<<  â™éRÉtÉ@ÉCÉã  >>---*
 FD  OKA      BLOCK  CONTAINS  1  RECORDS
              LABEL  RECORD    IS STANDARD.
     COPY     FURIKAF   OF        XFDLIB
              JOINING   OKA       PREFIX.
*---<<  ñkäCìπÉtÉ@ÉCÉã  >>---*
 FD  HOK      BLOCK  CONTAINS  1  RECORDS
              LABEL  RECORD    IS STANDARD.
     COPY     FURIKAF   OF        XFDLIB
              JOINING   HOK       PREFIX.
*---<<  ëÂç„ÉtÉ@ÉCÉã  >>---*
 FD  OSA      BLOCK  CONTAINS  1  RECORDS
              LABEL  RECORD    IS STANDARD.
     COPY     FURIKAF   OF        XFDLIB
              JOINING   OSA       PREFIX.
*---<<  ïîñÂéÊà¯êÊÉ}ÉXÉ^ÇP  >>---*
 FD  BUTOKMF1.
     COPY     BUTOKMF   OF        XFDLIB
              JOINING   BU1       PREFIX.
*----<< ïîñÂéÊà¯êÊÉ}ÉXÉ^ÇQ  >>-*
 FD  BUTOKMF2.
     COPY     BUTOKMF   OF        XFDLIB
              JOINING   BU2       PREFIX.
****  çÏã∆óÃàÊ  ***
 WORKING-STORAGE             SECTION.
****  ÉXÉeÉCÉ^ÉXèÓïÒ  ***
 01  STATUS-AREA.
     02  TOK-STATUS          PIC  X(02).
     02  HON-STATUS          PIC  X(02).
     02  FUK-STATUS          PIC  X(02).
     02  SEN-STATUS          PIC  X(02).
     02  OKA-STATUS          PIC  X(02).
     02  HOK-STATUS          PIC  X(02).
     02  OSA-STATUS          PIC  X(02).
     02  BU1-STATUS          PIC  X(02).
     02  BU2-STATUS          PIC  X(02).
****  ÉtÉâÉO  ***
 01  PSW-AREA.
     02  END-FLG             PIC  X(03)  VALUE SPACE.
     02  SKIP-FLG            PIC  X(01)  VALUE SPACE.
 01  CNT-AREA.
     02  READ-CNT            PIC  9(07)  VALUE ZERO.
     02  HON-CNT             PIC  9(07)  VALUE ZERO.
     02  FUK-CNT             PIC  9(07)  VALUE ZERO.
     02  SEN-CNT             PIC  9(07)  VALUE ZERO.
     02  OKA-CNT             PIC  9(07)  VALUE ZERO.
     02  HOK-CNT             PIC  9(07)  VALUE ZERO.
     02  OSA-CNT             PIC  9(07)  VALUE ZERO.
     02  SKIP-CNT            PIC  9(07)  VALUE ZERO.
****  ÇvÇqÇjóÃàÊ  *** 1999/12/27 NAV START
 01  WRK-AREA.
     02  WRK-DATE1           PIC  9(06).
     02  WRK-DATE1R          REDEFINES   WRK-DATE1.
         04  WRK-DATE1R1     PIC  9(04).
         04  WRK-DATE1R2     PIC  9(02).
     02  WRK-DATE2           PIC  9(06).
**** ÉÅÉbÉZÅ[ÉWèÓïÒ  ***
 01  MSG-AREA1-1.
     02  MSG-ABEND1.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-PG-ID         PIC  X(08)  VALUE  "SFU0103B".
       03  FILLER            PIC  X(10)  VALUE  " ABEND ###".
     02  MSG-ABEND2.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-FL-ID         PIC  X(08).
       03  FILLER            PIC  X(04)  VALUE  " ST-".
       03  ERR-STCD          PIC  X(02).
       03  FILLER            PIC  X(04)  VALUE  " ###".
*ÉèÅ[ÉNÇbÇnÇoÇxãÂíËã`
     COPY  FURIKAF  OF  XFDLIB  JOINING  WRK  PREFIX.
     COPY  FURIKAF  OF  XFDLIB  JOINING  WRK1 PREFIX.
*-------------------------------------------------------------*
*             ÇlÇ`ÇhÇmÅ@Å@Å@Å@ÇlÇnÇcÇtÇkÇd                    *
*-------------------------------------------------------------*
 PROCEDURE                   DIVISION.
**
 DECLARATIVES.
 FILEERR-SEC1                SECTION.
     USE AFTER       EXCEPTION
                     PROCEDURE    TOKU.
     MOVE     "TOKU"         TO   ERR-FL-ID.
     MOVE     TOK-STATUS     TO   ERR-STCD.
     DISPLAY  MSG-ABEND1     UPON   CONS.
     DISPLAY  MSG-ABEND2     UPON   CONS.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
 FILEERR-SEC2                SECTION.
     USE AFTER       EXCEPTION
                     PROCEDURE    HON.
     MOVE     "HON"          TO   ERR-FL-ID.
     MOVE     HON-STATUS     TO   ERR-STCD.
     DISPLAY  MSG-ABEND1     UPON   CONS.
     DISPLAY  MSG-ABEND2     UPON   CONS.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
 FILEERR-SEC3                SECTION.
     USE AFTER       EXCEPTION
                     PROCEDURE    FUK.
     MOVE     "FUK"          TO   ERR-FL-ID.
     MOVE     FUK-STATUS     TO   ERR-STCD.
     DISPLAY  MSG-ABEND1     UPON   CONS.
     DISPLAY  MSG-ABEND2     UPON   CONS.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
 FILEERR-SEC4                SECTION.
     USE AFTER       EXCEPTION
                     PROCEDURE    SEN.
     MOVE     "SEN"          TO   ERR-FL-ID.
     MOVE     SEN-STATUS     TO   ERR-STCD.
     DISPLAY  MSG-ABEND1     UPON   CONS.
     DISPLAY  MSG-ABEND2     UPON   CONS.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
 FILEERR-SEC5                SECTION.
     USE AFTER       EXCEPTION
                     PROCEDURE    OKA.
     MOVE     "OKA"          TO   ERR-FL-ID.
     MOVE     OKA-STATUS     TO   ERR-STCD.
     DISPLAY  MSG-ABEND1     UPON   CONS.
     DISPLAY  MSG-ABEND2     UPON   CONS.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
 FILEERR-SEC6                SECTION.
     USE AFTER       EXCEPTION
                     PROCEDURE    HOK.
     MOVE     "HOK"          TO   ERR-FL-ID.
     MOVE     HOK-STATUS     TO   ERR-STCD.
     DISPLAY  MSG-ABEND1     UPON   CONS.
     DISPLAY  MSG-ABEND2     UPON   CONS.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
 FILEERR-SEC7                SECTION.
     USE AFTER       EXCEPTION
                     PROCEDURE    OSA.
     MOVE     "OSA"          TO   ERR-FL-ID.
     MOVE     OSA-STATUS     TO   ERR-STCD.
     DISPLAY  MSG-ABEND1     UPON   CONS.
     DISPLAY  MSG-ABEND2     UPON   CONS.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
 FILEERR-SEC8                SECTION.
     USE AFTER       EXCEPTION
                     PROCEDURE    BUTOKMF1.
     MOVE     "BUTOKMF1"     TO   ERR-FL-ID.
     MOVE     BU1-STATUS     TO   ERR-STCD.
     DISPLAY  MSG-ABEND1     UPON   CONS.
     DISPLAY  MSG-ABEND2     UPON   CONS.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
 FILEERR-SEC9                SECTION.
     USE AFTER       EXCEPTION
                     PROCEDURE    BUTOKMF2.
     MOVE     "BUTOKMF2"     TO   ERR-FL-ID.
     MOVE     BU2-STATUS     TO   ERR-STCD.
     DISPLAY  MSG-ABEND1     UPON   CONS.
     DISPLAY  MSG-ABEND2     UPON   CONS.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
 END     DECLARATIVES.
****************************************************************
 KEI0100-START               SECTION.
     PERFORM       INIT-SEC.
     PERFORM       MAIN-SEC   UNTIL  END-FLG = "END".
     PERFORM       END-SEC.
     STOP      RUN.
 KEI0100-END.
     EXIT.
****************************************************************
*      Å°ÇOÅ@Å@èâä˙èàóù                                        *
****************************************************************
 INIT-SEC                    SECTION.
*ÉtÉ@ÉCÉãÇÃÇnÇoÇdÇm
     OPEN    INPUT           TOKU  BUTOKMF1  BUTOKMF2.
     OPEN    OUTPUT          HON  FUK  SEN  HOK  OSA  OKA.
*êUë÷ÉfÅ[É^èâä˙ì«çûÇ›
     PERFORM TOKU-READ-SEC.
*
 INIT-END.
     EXIT.
****************************************************************
*      ìdéZé∫ìaÅÀêUë÷ÉfÅ[É^ÉtÉ@ÉCÉãì«çûÇ›                      *
****************************************************************
 TOKU-READ-SEC               SECTION.
*
     READ    TOKU
             AT  END
             MOVE   "END"      TO   END-FLG
             NOT AT END
             ADD     1         TO   READ-CNT
     END-READ.
*
     IF      TO0-F02  =  70
             GO                TO   TOKU-READ-SEC
     END-IF.
*
 TOKU-READ-EXIT.
     EXIT.
****************************************************************
*      Å°ÇOÅ@Å@ÉÅÉCÉìèàóù                                      *
****************************************************************
 MAIN-SEC                    SECTION.
*ÉtÉâÉOèâä˙âª
     MOVE         SPACE        TO   SKIP-FLG.
*ÉèÅ[ÉNèâä˙âª
     MOVE         SPACE        TO   WRK-REC WRK1-REC.
     INITIALIZE                     WRK-REC WRK1-REC.
*îªíËèàóù
     EVALUATE   TO0-F02
         WHEN   20
                PERFORM  FURI-20-SEC
         WHEN   70
***             IF  TO0-F01(1:2)  NOT =  "29"
***                 PERFORM  FURI-701-SEC
***             ELSE
***                 PERFORM  FURI-70-SEC
***             END-IF
                IF  TO0-F01 =  3346 OR 3356 OR 3346 OR 3136 OR
                               3236 OR 3266
                    PERFORM  FURI-70-SEC
                ELSE
                    PERFORM  FURI-701-SEC
                END-IF
         WHEN   71
***             IF  TO0-F01(1:2)  NOT =  "29"
***                 PERFORM  FURI-711-SEC
***             ELSE
***                 PERFORM  FURI-71-SEC
***             END-IF
                IF  TO0-F01 =  3346 OR 3356 OR 3346 OR 3136 OR
                               3236 OR 3266
                    PERFORM  FURI-71-SEC
                ELSE
                    PERFORM  FURI-711-SEC
                END-IF
         WHEN   35
                PERFORM FURI-35-SEC
         WHEN   36
                PERFORM FURI-36-SEC
         WHEN   40
                PERFORM FURI-40-SEC
         WHEN   41
                PERFORM FURI-41-SEC
         WHEN   50
                PERFORM FURI-50-SEC
         WHEN   51
                PERFORM FURI-51-SEC
         WHEN   30
                PERFORM FURI-30-SEC
         WHEN   31
                PERFORM FURI-31-SEC
         WHEN   32
                PERFORM FURI-32-SEC
         WHEN   OTHER
                MOVE     "1"     TO   SKIP-FLG
     END-EVALUATE.
*
     IF  SKIP-FLG  =  "1"
         GO     TO               MAIN-010
     END-IF.
*çXêVîªíË
     EVALUATE   WRK-F01
         WHEN   "3266"
                MOVE  WRK-REC   TO   HON-REC
                WRITE HON-REC
                ADD   1         TO   HON-CNT
         WHEN   "3436"
                MOVE  WRK-REC   TO   FUK-REC
                WRITE FUK-REC
                ADD   1         TO   FUK-CNT
         WHEN   "3236"
                MOVE  WRK-REC   TO   SEN-REC
                WRITE SEN-REC
                ADD   1         TO   SEN-CNT
         WHEN   "3356"
                MOVE  WRK-REC   TO   OKA-REC
                WRITE OKA-REC
                ADD   1         TO   OKA-CNT
         WHEN   "3136"
                MOVE  WRK-REC   TO   HOK-REC
                WRITE HOK-REC
                ADD   1         TO   HOK-CNT
         WHEN   "3346"
                MOVE  WRK-REC   TO   OSA-REC
                WRITE OSA-REC
                ADD   1         TO   OSA-CNT
         WHEN   OTHER
                ADD   1         TO   SKIP-CNT
     END-EVALUATE.
*****DISPLAY "WRK-F01 = " WRK-F01 UPON CONS.
*
     IF  WRK1-F01  NOT =  SPACE
         EVALUATE   WRK1-F01
             WHEN   "3266"
                    MOVE  WRK1-REC  TO   HON-REC
                    WRITE HON-REC
                    ADD   1         TO   HON-CNT
             WHEN   "3436"
                    MOVE  WRK1-REC  TO   FUK-REC
                    WRITE FUK-REC
                    ADD   1         TO   FUK-CNT
             WHEN   "3236"
                    MOVE  WRK1-REC  TO   SEN-REC
                    WRITE SEN-REC
                    ADD   1         TO   SEN-CNT
             WHEN   "3356"
                    MOVE  WRK1-REC  TO   OKA-REC
                    WRITE OKA-REC
                    ADD   1         TO   OKA-CNT
             WHEN   "3136"
                    MOVE  WRK1-REC  TO   HOK-REC
                    WRITE HOK-REC
                    ADD   1         TO   HOK-CNT
             WHEN   "3136"
                    MOVE  WRK1-REC  TO   OSA-REC
                    WRITE OSA-REC
                    ADD   1         TO   OSA-CNT
             WHEN   OTHER
                    ADD   1         TO   SKIP-CNT
         END-EVALUATE
     END-IF.
*
 MAIN-010.
     PERFORM TOKU-READ-SEC.
 MAIN-END.
     EXIT.
****************************************************************
*    êUë÷ÉfÅ[É^Åiî≠íçÅj                                        *
****************************************************************
 FURI-20-SEC                 SECTION.
*ïîñÂÇbÇc
     MOVE   TO0-F01          TO   WRK-F01.
*ì`ï[ãÊï™
     MOVE   TO0-F02          TO   WRK-F02.
*ì`ï[Å°
     MOVE   TO0-F03          TO   WRK-F03.
*çsÅ°
     MOVE   TO0-F04          TO   WRK-F04.
*ê‘çïãÊï™
     MOVE   TO0-F05          TO   WRK-F05.
*êUë÷å≥Å^êUë÷êÊ
     MOVE   TO0-F06          TO   WRK-F06.
*î≠íçì˙
     MOVE   TO0-F08          TO   WRK-F07.
*î[ïiì˙
     MOVE   TO0-F09          TO   WRK-F08.
*èoâ◊ì˙
     MOVE   TO0-F10          TO   WRK-F09.
*è§ïiÇbÇc
     MOVE   TO0-F11          TO   WRK-F10
*ïiíPÇbÇc
     MOVE   TO0-F12          TO   WRK-F11.
*íPâøãÊï™Å^êîó Å^íPâøÅ^ã‡äz
     MOVE   TO0-F14          TO   WRK-F12.
     MOVE   TO0-F15          TO   WRK-F13.
     MOVE   TO0-F16          TO   WRK-F14.
     MOVE   TO0-F17          TO   WRK-F15.
*ì¸å…Å^èoå…Å^î≠íçãÊï™
     MOVE   "3"              TO   WRK-F16.
*èÍèä
     MOVE   TO0-F18          TO   WRK-F17.
*É}ÉXÉ^ãÊï™
     MOVE   "3"              TO   WRK-F20.
*
 FURI-20-EXIT.
     EXIT.
****************************************************************
*    êUë÷ÉfÅ[É^ÅiêUë÷ÅjëºïîÅÀì¡îÃïî                            *
****************************************************************
 FURI-701-SEC                SECTION.
*ïîñÂÉRÅ[ÉhéÊìæ
     MOVE   TO0-F06(5:4)     TO    BU1-F01.
***  READ   BUTOKMF1
***         INVALID
***         DISPLAY "BUMON-CD INVALID = " TO0-F06  UPON CONS
***         STOP RUN
***         NOT INVALID
***         MOVE     BU1-F03 TO    WRK-F01
***  END-READ.
*ì`ï[ãÊï™
     MOVE   TO0-F02          TO   WRK-F02.
*ì`ï[Å°
     MOVE   TO0-F03          TO   WRK-F03.
*çsÅ°
     MOVE   TO0-F04          TO   WRK-F04.
*ê‘çïãÊï™
     MOVE   TO0-F05          TO   WRK-F05.
*êUë÷å≥Å^êUë÷êÊ
     MOVE   TO0-F01          TO   BU2-F03.
     READ   BUTOKMF2
            INVALID
            MOVE     SPACE   TO   WRK-F06
            NOT INVALID
            MOVE     BU2-F01 TO   WRK-F06
     END-READ.
*î≠íçì˙
     MOVE   TO0-F08          TO   WRK-F07.
*î[ïiì˙
     MOVE   TO0-F09          TO   WRK-F08.
*èoâ◊ì˙
     MOVE   TO0-F10          TO   WRK-F09.
*è§ïiÇbÇc
     MOVE   TO0-F11          TO   WRK-F10
*ïiíPÇbÇc
     MOVE   TO0-F12          TO   WRK-F11.
*íPâøãÊï™Å^êîó Å^íPâøÅ^ã‡äz
     MOVE   TO0-F14          TO   WRK-F12.
     MOVE   TO0-F15          TO   WRK-F13.
     MOVE   TO0-F16          TO   WRK-F14.
     MOVE   TO0-F17          TO   WRK-F15.
*ì¸å…Å^èoå…Å^î≠íçãÊï™
*****MOVE          "1"       TO   WRK-F16.
*## 2002/03/04 NAV START ## êîó œ≤≈ΩéûÇ…ãÊï™ÇîΩëŒÇ…Ç∑ÇÈ
     IF     TO0-F15  >=  ZERO
            MOVE   "1"       TO   WRK-F16
     ELSE
            MOVE   "2"       TO   WRK-F16
     END-IF.
*## 2002/03/04 NAV END   ##
*èÍèä
     MOVE   TO0-F21          TO   WRK-F17.
*é–ì‡î≠íçÅ°
     MOVE   TO0-F35          TO   WRK-F18.
*äÆî[ãÊï™
     MOVE   TO0-F36          TO   WRK-F20.
*É}ÉXÉ^ãÊï™
     MOVE   "3"              TO   WRK-F20.
 FURI-701-EXIT.
     EXIT.
****************************************************************
*    êUë÷ÉfÅ[É^ÅiêUë÷ÉfÅ[É^Åjì¡îÃïîÅÀëºïî                      *
****************************************************************
 FURI-70-SEC                 SECTION.
*ïîñÂÉRÅ[ÉhéÊìæ
     MOVE   TO0-F01          TO   WRK-F01.
*ì`ï[ãÊï™
     MOVE   TO0-F02          TO   WRK-F02.
*ì`ï[Å°
     MOVE   TO0-F03          TO   WRK-F03.
*çsÅ°
     MOVE   TO0-F04          TO   WRK-F04.
*ê‘çïãÊï™
     MOVE   TO0-F05          TO   WRK-F05.
*êUë÷å≥Å^êUë÷êÊ
     MOVE   TO0-F06          TO   WRK-F06.
*î≠íçì˙
     MOVE   TO0-F08          TO   WRK-F07.
*î[ïiì˙
     MOVE   TO0-F09          TO   WRK-F08.
*èoâ◊ì˙
     MOVE   TO0-F10          TO   WRK-F09.
*è§ïiÇbÇc
     MOVE   TO0-F11          TO   WRK-F10
*ïiíPÇbÇc
     MOVE   TO0-F12          TO   WRK-F11.
*íPâøãÊï™Å^êîó Å^íPâøÅ^ã‡äz
     MOVE   TO0-F14          TO   WRK-F12.
     MOVE   TO0-F15          TO   WRK-F13.
     MOVE   TO0-F16          TO   WRK-F14.
     MOVE   TO0-F17          TO   WRK-F15.
*ì¸å…Å^èoå…Å^î≠íçãÊï™
*****MOVE   "2"              TO   WRK-F16.
*## 2002/03/04 NAV START ## êîó œ≤≈ΩéûÇ…ãÊï™ÇîΩëŒÇ…Ç∑ÇÈ
     IF     TO0-F15  >=  ZERO
            MOVE   "2"       TO   WRK-F16
     ELSE
            MOVE   "1"       TO   WRK-F16
     END-IF.
*## 2002/03/04 NAV END   ##
*èÍèä
     MOVE   TO0-F18          TO   WRK-F17.
*É}ÉXÉ^ãÊï™
     MOVE   "3"              TO   WRK-F20.
 FURI-70-EXIT.
     EXIT.
****************************************************************
*    êUë÷ÉfÅ[É^ÅiêUë÷ï‘ïiÅjëºïîÅÀì¡îÃïî                        *
****************************************************************
 FURI-711-SEC                SECTION.
*ïîñÂÉRÅ[ÉhéÊìæ
     MOVE   TO0-F06(5:4)     TO    BU1-F01.
***  READ   BUTOKMF1
***         INVALID
***         DISPLAY "BUMON-CD INVALID = " TO0-F06  UPON CONS
***         STOP RUN
***         NOT INVALID
***         MOVE     BU1-F03 TO    WRK-F01
***  END-READ.
*ì`ï[ãÊï™
     MOVE   TO0-F02          TO   WRK-F02.
*ì`ï[Å°
     MOVE   TO0-F03          TO   WRK-F03.
*çsÅ°
     MOVE   TO0-F04          TO   WRK-F04.
*ê‘çïãÊï™
     MOVE   TO0-F05          TO   WRK-F05.
*êUë÷å≥Å^êUë÷êÊ
     MOVE   TO0-F06          TO   WRK-F06.
     MOVE   TO0-F01          TO   BU2-F03.
     READ   BUTOKMF2
            INVALID
            MOVE     SPACE   TO   WRK-F06
            NOT INVALID
            MOVE     BU2-F01 TO   WRK-F06
     END-READ.
*î≠íçì˙
     MOVE   TO0-F08          TO   WRK-F07.
*î[ïiì˙
     MOVE   TO0-F09          TO   WRK-F08.
*èoâ◊ì˙
     MOVE   TO0-F10          TO   WRK-F09.
*è§ïiÇbÇc
     MOVE   TO0-F11          TO   WRK-F10
*ïiíPÇbÇc
     MOVE   TO0-F12          TO   WRK-F11.
*íPâøãÊï™Å^êîó Å^íPâøÅ^ã‡äz
     MOVE   TO0-F14          TO   WRK-F12.
     MOVE   TO0-F15          TO   WRK-F13.
     MOVE   TO0-F16          TO   WRK-F14.
     MOVE   TO0-F17          TO   WRK-F15.
*ì¸å…Å^èoå…Å^î≠íçãÊï™
*****MOVE   "1"              TO   WRK-F16.
*## 2002/03/04 NAV START ## êîó œ≤≈ΩéûÇ…ãÊï™ÇîΩëŒÇ…Ç∑ÇÈ
     IF     TO0-F15  >=  ZERO
            MOVE   "1"       TO   WRK-F16
     ELSE
            MOVE   "2"       TO   WRK-F16
     END-IF.
*## 2002/03/04 NAV END   ##
*èÍèä
     MOVE   TO0-F21          TO   WRK-F17.
*é–ì‡î≠íçÅ°
     MOVE   TO0-F35          TO   WRK-F18.
*äÆî[ãÊï™
     MOVE   TO0-F36          TO   WRK-F20.
*É}ÉXÉ^ãÊï™
     MOVE   "3"              TO   WRK-F20.
 FURI-711-EXIT.
     EXIT.
****************************************************************
*    êUë÷ÉfÅ[É^ÅiêUë÷ï‘ïiÅjì¡îÃïîÅÀëºïî                        *
****************************************************************
 FURI-71-SEC                 SECTION.
*ïîñÂÉRÅ[ÉhéÊìæ
     MOVE   TO0-F01          TO   WRK-F01.
*ì`ï[ãÊï™
     MOVE   TO0-F02          TO   WRK-F02.
*ì`ï[Å°
     MOVE   TO0-F03          TO   WRK-F03.
*çsÅ°
     MOVE   TO0-F04          TO   WRK-F04.
*ê‘çïãÊï™
     MOVE   TO0-F05          TO   WRK-F05.
*êUë÷å≥Å^êUë÷êÊ
     MOVE   TO0-F06          TO   WRK-F06.
*î≠íçì˙
     MOVE   TO0-F08          TO   WRK-F07.
*î[ïiì˙
     MOVE   TO0-F09          TO   WRK-F08.
*èoâ◊ì˙
     MOVE   TO0-F10          TO   WRK-F09.
*è§ïiÇbÇc
     MOVE   TO0-F11          TO   WRK-F10
*ïiíPÇbÇc
     MOVE   TO0-F12          TO   WRK-F11.
*íPâøãÊï™Å^êîó Å^íPâøÅ^ã‡äz
     MOVE   TO0-F14          TO   WRK-F12.
     MOVE   TO0-F15          TO   WRK-F13.
     MOVE   TO0-F16          TO   WRK-F14.
     MOVE   TO0-F17          TO   WRK-F15.
*ì¸å…Å^èoå…Å^î≠íçãÊï™
*****MOVE   "2"              TO   WRK-F16.
*## 2002/03/04 NAV START ## êîó œ≤≈ΩéûÇ…ãÊï™ÇîΩëŒÇ…Ç∑ÇÈ
     IF     TO0-F15  >=  ZERO
            MOVE   "2"       TO   WRK-F16
     ELSE
            MOVE   "1"       TO   WRK-F16
     END-IF.
*## 2002/03/04 NAV END   ##
*èÍèä
     MOVE   TO0-F18          TO   WRK-F17.
*É}ÉXÉ^ãÊï™
     MOVE   "3"              TO   WRK-F20.
 FURI-71-EXIT.
     EXIT.
****************************************************************
*    êUë÷ÉfÅ[É^ÅiÇRÇTÅFà⁄ìÆÅj                                  *
****************************************************************
 FURI-35-SEC                 SECTION.
*ïîñÂÉRÅ[ÉhéÊìæ
     MOVE   TO0-F01          TO   WRK-F01.
*èoå…ÉfÅ[É^êUë÷èÍèäéÊìæ
*    MOVE   TO0-F01          TO   BU2-F03.
*    READ   BUTOKMF2
*           INVALID
*           DISPLAY "BUMON-CD INVALID = " TO0-F01  UPON CONS
*           STOP RUN
*           NOT INVALID
*           MOVE     BU2-F01 TO   WRK1-F06
*    END-READ.
     EVALUATE TO0-F01
         WHEN 3266
              MOVE 99999290  TO  WRK1-F06
         WHEN 3436
              MOVE 99999291  TO  WRK1-F06
         WHEN 3236
              MOVE 99999292  TO  WRK1-F06
         WHEN 2930
              MOVE 99999293  TO  WRK1-F06
         WHEN 3356
              MOVE 99999294  TO  WRK1-F06
         WHEN 3136
              MOVE 99999295  TO  WRK1-F06
         WHEN 2960
              MOVE 99999296  TO  WRK1-F06
         WHEN 2970
              MOVE 99999297  TO  WRK1-F06
         WHEN 2980
              MOVE 99999298  TO  WRK1-F06
         WHEN 3346
              MOVE 99999299  TO  WRK1-F06
     END-EVALUATE.
*ì`ï[ãÊï™
     MOVE   TO0-F02          TO   WRK-F02 WRK1-F02.
*ì`ï[Å°
     MOVE   TO0-F03          TO   WRK-F03 WRK1-F03.
*çsÅ°
     MOVE   TO0-F04          TO   WRK-F04 WRK1-F04.
*ê‘çïãÊï™
     MOVE   TO0-F05          TO   WRK-F05 WRK1-F05.
*êUë÷å≥Å^êUë÷êÊ
     MOVE   TO0-F06          TO   WRK-F06.
     MOVE   TO0-F06          TO   BU1-F01.
     READ   BUTOKMF1
            INVALID
            MOVE     SPACE   TO   WRK1-F01
            NOT INVALID
            MOVE     BU1-F03 TO   WRK1-F01
     END-READ.
*î≠íçì˙
     MOVE   TO0-F08          TO   WRK-F07 WRK1-F07.
*î[ïiì˙
     MOVE   TO0-F09          TO   WRK-F08 WRK1-F08.
*èoâ◊ì˙
     MOVE   TO0-F10          TO   WRK-F09 WRK1-F09.
*è§ïiÇbÇc
     MOVE   TO0-F11          TO   WRK-F10 WRK1-F10.
*ïiíPÇbÇc
     MOVE   TO0-F12          TO   WRK-F11 WRK1-F11.
*íPâøãÊï™Å^êîó Å^íPâøÅ^ã‡äz
     MOVE   TO0-F14          TO   WRK-F12 WRK1-F12.
     MOVE   TO0-F15          TO   WRK-F13 WRK1-F13.
     MOVE   TO0-F16          TO   WRK-F14 WRK1-F14.
     MOVE   TO0-F17          TO   WRK-F15 WRK1-F15.
*ì¸å…Å^èoå…Å^î≠íçãÊï™Å|èÍèä
*****MOVE   "2"              TO   WRK-F16.
*## 2002/03/13 NAV START ## êîó œ≤≈ΩéûÇ…ãÊï™ÇîΩëŒÇ…Ç∑ÇÈ
     IF     TO0-F15  >=  ZERO
            MOVE   "2"       TO   WRK-F16
     ELSE
            MOVE   "1"       TO   WRK-F16
     END-IF.
*## 2002/03/13 NAV END   ##
     MOVE   TO0-F18          TO   WRK-F17.
*ì¸å…Å^èoå…Å^î≠íçãÊï™Å|èÍèä
*****MOVE   "1"              TO   WRK1-F16.
*## 2002/03/13 NAV START ## êîó œ≤≈ΩéûÇ…ãÊï™ÇîΩëŒÇ…Ç∑ÇÈ
     IF     TO0-F15  >=  ZERO
            MOVE   "1"       TO   WRK1-F16
     ELSE
            MOVE   "2"       TO   WRK1-F16
     END-IF.
*## 2002/03/13 NAV END   ##
     MOVE   TO0-F21          TO   WRK1-F17.
*É}ÉXÉ^ãÊï™
     MOVE   "3"              TO   WRK-F20 WRK1-F20.
 FURI-35-EXIT.
     EXIT.
****************************************************************
*    êUë÷ÉfÅ[É^ÅiÇRÇUÅFà⁄ìÆï‘ïiÅj                              *
****************************************************************
 FURI-36-SEC                 SECTION.
*ïîñÂÉRÅ[ÉhéÊìæ
     MOVE   TO0-F01          TO   WRK-F01.
*èoå…ÉfÅ[É^êUë÷èÍèäéÊìæ
*    MOVE   TO0-F01          TO   BU2-F03.
*    READ   BUTOKMF2
*           INVALID
*           DISPLAY "BUMON-CD INVALID = " TO0-F01  UPON CONS
*           STOP RUN
*           NOT INVALID
*           MOVE     BU2-F01 TO   WRK1-F06
*    END-READ.
     EVALUATE TO0-F01
         WHEN 3266
              MOVE 99999290  TO  WRK1-F06
         WHEN 3436
              MOVE 99999291  TO  WRK1-F06
         WHEN 3236
              MOVE 99999292  TO  WRK1-F06
         WHEN 2930
              MOVE 99999293  TO  WRK1-F06
         WHEN 3356
              MOVE 99999294  TO  WRK1-F06
         WHEN 3136
              MOVE 99999295  TO  WRK1-F06
         WHEN 2960
              MOVE 99999296  TO  WRK1-F06
         WHEN 2970
              MOVE 99999297  TO  WRK1-F06
         WHEN 2980
              MOVE 99999298  TO  WRK1-F06
         WHEN 3346
              MOVE 99999299  TO  WRK1-F06
     END-EVALUATE.
*ì`ï[ãÊï™
     MOVE   TO0-F02          TO   WRK-F02 WRK1-F02.
*ì`ï[Å°
     MOVE   TO0-F03          TO   WRK-F03 WRK1-F03.
*çsÅ°
     MOVE   TO0-F04          TO   WRK-F04 WRK1-F04.
*ê‘çïãÊï™
     MOVE   TO0-F05          TO   WRK-F05 WRK1-F05.
*êUë÷å≥Å^êUë÷êÊ
     MOVE   TO0-F06          TO   WRK-F06.
     MOVE   TO0-F06          TO   BU1-F01.
     READ   BUTOKMF1
            INVALID
            MOVE     SPACE   TO   WRK1-F01
            NOT INVALID
            MOVE     BU1-F03 TO   WRK1-F01
     END-READ.
*î≠íçì˙
     MOVE   TO0-F08          TO   WRK-F07 WRK1-F07.
*î[ïiì˙
     MOVE   TO0-F09          TO   WRK-F08 WRK1-F08.
*èoâ◊ì˙
     MOVE   TO0-F10          TO   WRK-F09 WRK1-F09.
*è§ïiÇbÇc
     MOVE   TO0-F11          TO   WRK-F10 WRK1-F10.
*ïiíPÇbÇc
     MOVE   TO0-F12          TO   WRK-F11 WRK1-F11.
*íPâøãÊï™Å^êîó Å^íPâøÅ^ã‡äz
     MOVE   TO0-F14          TO   WRK-F12 WRK1-F12.
     MOVE   TO0-F15          TO   WRK-F13 WRK1-F13.
     MOVE   TO0-F16          TO   WRK-F14 WRK1-F14.
     MOVE   TO0-F17          TO   WRK-F15 WRK1-F15.
*ì¸å…Å^èoå…Å^î≠íçãÊï™Å|èÍèä
*****MOVE   "2"              TO   WRK-F16.
*## 2002/03/13 NAV START ## êîó œ≤≈ΩéûÇ…ãÊï™ÇîΩëŒÇ…Ç∑ÇÈ
     IF     TO0-F15  >=  ZERO
            MOVE   "2"       TO   WRK-F16
     ELSE
            MOVE   "1"       TO   WRK-F16
     END-IF.
*## 2002/03/13 NAV END   ##
     MOVE   TO0-F18          TO   WRK-F17.
*ì¸å…Å^èoå…Å^î≠íçãÊï™Å|èÍèä
*****MOVE   "1"              TO   WRK1-F16.
*## 2002/03/13 NAV START ## êîó œ≤≈ΩéûÇ…ãÊï™ÇîΩëŒÇ…Ç∑ÇÈ
     IF     TO0-F15  >=  ZERO
            MOVE   "1"       TO   WRK1-F16
     ELSE
            MOVE   "2"       TO   WRK1-F16
     END-IF.
*## 2002/03/13 NAV END   ##
     MOVE   TO0-F21          TO   WRK1-F17.
*É}ÉXÉ^ãÊï™
     MOVE   "3"              TO   WRK-F20 WRK1-F20.
 FURI-36-EXIT.
     EXIT.
****************************************************************
*    êUë÷ÉfÅ[É^ÅiÇSÇOÅFîÑè„Åj                                  *
****************************************************************
 FURI-40-SEC                 SECTION.
*ïîñÂÉRÅ[ÉhéÊìæ
     MOVE   TO0-F01          TO   WRK-F01.
*ì`ï[ãÊï™
     MOVE   TO0-F02          TO   WRK-F02.
*ì`ï[Å°
     MOVE   TO0-F03          TO   WRK-F03.
*çsÅ°
     MOVE   TO0-F04          TO   WRK-F04.
*ê‘çïãÊï™
     MOVE   TO0-F05          TO   WRK-F05.
*êUë÷å≥Å^êUë÷êÊ
     MOVE   TO0-F06          TO   WRK-F06.
*î≠íçì˙
     MOVE   TO0-F08          TO   WRK-F07.
*î[ïiì˙
     MOVE   TO0-F09          TO   WRK-F08.
*èoâ◊ì˙
     MOVE   TO0-F10          TO   WRK-F09.
*è§ïiÇbÇc
     MOVE   TO0-F11          TO   WRK-F10.
*ïiíPÇbÇc
     MOVE   TO0-F12          TO   WRK-F11.
*íPâøãÊï™Å^êîó Å^íPâøÅ^ã‡äz
     MOVE   TO0-F14          TO   WRK-F12.
     MOVE   TO0-F15          TO   WRK-F13.
     MOVE   TO0-F16          TO   WRK-F14.
     MOVE   TO0-F17          TO   WRK-F15.
*ì¸å…Å^èoå…Å^î≠íçãÊï™Å|èÍèä
*****MOVE   "2"              TO   WRK-F16.
*## 2002/03/13 NAV START ## êîó œ≤≈ΩéûÇ…ãÊï™ÇîΩëŒÇ…Ç∑ÇÈ
     IF     TO0-F15  >=  ZERO
            MOVE   "2"       TO   WRK-F16
     ELSE
            MOVE   "1"       TO   WRK-F16
     END-IF.
*## 2002/03/13 NAV END   ##
     MOVE   TO0-F18          TO   WRK-F17.
*É}ÉXÉ^ãÊï™
     MOVE   "3"              TO   WRK-F20.
 FURI-40-EXIT.
     EXIT.
****************************************************************
*    êUë÷ÉfÅ[É^ÅiÇSÇPÅFîÑè„ï‘ïiÅj                              *
****************************************************************
 FURI-41-SEC                 SECTION.
*ïîñÂÉRÅ[ÉhéÊìæ
     MOVE   TO0-F01          TO   WRK-F01.
*ì`ï[ãÊï™
     MOVE   TO0-F02          TO   WRK-F02.
*ì`ï[Å°
     MOVE   TO0-F03          TO   WRK-F03.
*çsÅ°
     MOVE   TO0-F04          TO   WRK-F04.
*ê‘çïãÊï™
     MOVE   TO0-F05          TO   WRK-F05.
*êUë÷å≥Å^êUë÷êÊ
     MOVE   TO0-F06          TO   WRK-F06.
*î≠íçì˙
     MOVE   TO0-F08          TO   WRK-F07.
*î[ïiì˙
     MOVE   TO0-F09          TO   WRK-F08.
*èoâ◊ì˙
     MOVE   TO0-F10          TO   WRK-F09.
*è§ïiÇbÇc
     MOVE   TO0-F11          TO   WRK-F10.
*ïiíPÇbÇc
     MOVE   TO0-F12          TO   WRK-F11.
*íPâøãÊï™Å^êîó Å^íPâøÅ^ã‡äz
     MOVE   TO0-F14          TO   WRK-F12.
     MOVE   TO0-F15          TO   WRK-F13.
     MOVE   TO0-F16          TO   WRK-F14.
     MOVE   TO0-F17          TO   WRK-F15.
*ì¸å…Å^èoå…Å^î≠íçãÊï™Å|èÍèä
*****MOVE   "1"              TO   WRK-F16.
*## 2002/03/13 NAV START ## êîó œ≤≈ΩéûÇ…ãÊï™ÇîΩëŒÇ…Ç∑ÇÈ
     IF     TO0-F15  >=  ZERO
            MOVE   "2"       TO   WRK-F16
     ELSE
            MOVE   "1"       TO   WRK-F16
     END-IF.
*## 2002/03/13 NAV END   ##
     MOVE   TO0-F18          TO   WRK-F17.
*É}ÉXÉ^ãÊï™
     MOVE   "3"              TO   WRK-F20.
 FURI-41-EXIT.
     EXIT.
****************************************************************
*    êUë÷ÉfÅ[É^ÅiÇTÇOÅFédì¸Åj                                  *
****************************************************************
 FURI-50-SEC                 SECTION.
*ïîñÂÉRÅ[ÉhéÊìæ
     MOVE   TO0-F01          TO   WRK-F01.
*ì`ï[ãÊï™
     MOVE   TO0-F02          TO   WRK-F02.
*ì`ï[Å°
     MOVE   TO0-F03          TO   WRK-F03.
*çsÅ°
     MOVE   TO0-F04          TO   WRK-F04.
*ê‘çïãÊï™
     MOVE   TO0-F05          TO   WRK-F05.
*êUë÷å≥Å^êUë÷êÊ
     MOVE   TO0-F06          TO   WRK-F06.
*î≠íçì˙
     MOVE   TO0-F08          TO   WRK-F07.
*î[ïiì˙
     MOVE   TO0-F09          TO   WRK-F08.
*èoâ◊ì˙
     MOVE   TO0-F10          TO   WRK-F09.
*è§ïiÇbÇc
     MOVE   TO0-F11          TO   WRK-F10.
*ïiíPÇbÇc
     MOVE   TO0-F12          TO   WRK-F11.
*íPâøãÊï™Å^êîó Å^íPâøÅ^ã‡äz
     MOVE   TO0-F14          TO   WRK-F12.
     MOVE   TO0-F15          TO   WRK-F13.
     MOVE   TO0-F16          TO   WRK-F14.
     MOVE   TO0-F17          TO   WRK-F15.
*ì¸å…Å^èoå…Å^î≠íçãÊï™Å|èÍèä
*****MOVE   "1"              TO   WRK-F16.
*## 2002/03/13 NAV START ## êîó œ≤≈ΩéûÇ…ãÊï™ÇîΩëŒÇ…Ç∑ÇÈ
     IF     TO0-F15  >=  ZERO
            MOVE   "1"       TO   WRK-F16
     ELSE
            MOVE   "2"       TO   WRK-F16
     END-IF.
*## 2002/03/13 NAV END   ##
     MOVE   TO0-F18          TO   WRK-F17.
*É}ÉXÉ^ãÊï™
     MOVE   "2"              TO   WRK-F20.
 FURI-50-EXIT.
     EXIT.
****************************************************************
*    êUë÷ÉfÅ[É^ÅiÇTÇPÅFédì¸ï‘ïiÅj                              *
****************************************************************
 FURI-51-SEC                 SECTION.
*ïîñÂÉRÅ[ÉhéÊìæ
     MOVE   TO0-F01          TO   WRK-F01.
*ì`ï[ãÊï™
     MOVE   TO0-F02          TO   WRK-F02.
*ì`ï[Å°
     MOVE   TO0-F03          TO   WRK-F03.
*çsÅ°
     MOVE   TO0-F04          TO   WRK-F04.
*ê‘çïãÊï™
     MOVE   TO0-F05          TO   WRK-F05.
*êUë÷å≥Å^êUë÷êÊ
     MOVE   TO0-F06          TO   WRK-F06.
*î≠íçì˙
     MOVE   TO0-F08          TO   WRK-F07.
*î[ïiì˙
     MOVE   TO0-F09          TO   WRK-F08.
*èoâ◊ì˙
     MOVE   TO0-F10          TO   WRK-F09.
*è§ïiÇbÇc
     MOVE   TO0-F11          TO   WRK-F10.
*ïiíPÇbÇc
     MOVE   TO0-F12          TO   WRK-F11.
*íPâøãÊï™Å^êîó Å^íPâøÅ^ã‡äz
     MOVE   TO0-F14          TO   WRK-F12.
     MOVE   TO0-F15          TO   WRK-F13.
     MOVE   TO0-F16          TO   WRK-F14.
     MOVE   TO0-F17          TO   WRK-F15.
*ì¸å…Å^èoå…Å^î≠íçãÊï™Å|èÍèä
*****MOVE   "2"              TO   WRK-F16.
*## 2002/03/13 NAV START ## êîó œ≤≈ΩéûÇ…ãÊï™ÇîΩëŒÇ…Ç∑ÇÈ
     IF     TO0-F15  >=  ZERO
            MOVE   "1"       TO   WRK-F16
     ELSE
            MOVE   "2"       TO   WRK-F16
     END-IF.
*## 2002/03/13 NAV END   ##
     MOVE   TO0-F18          TO   WRK-F17.
*É}ÉXÉ^ãÊï™
     MOVE   "2"              TO   WRK-F20.
 FURI-51-EXIT.
     EXIT.
****************************************************************
*    êUë÷ÉfÅ[É^ÅiÇRÇOÅFçÏã∆é¿ê—Åj                              *
****************************************************************
 FURI-30-SEC                 SECTION.
*ïîñÂÉRÅ[ÉhéÊìæ
     MOVE   TO0-F01          TO   WRK-F01.
*ì`ï[ãÊï™
     MOVE   TO0-F02          TO   WRK-F02.
*ì`ï[Å°
     MOVE   TO0-F03          TO   WRK-F03.
*çsÅ°
     MOVE   TO0-F04          TO   WRK-F04.
*ê‘çïãÊï™
     MOVE   TO0-F05          TO   WRK-F05.
*êUë÷å≥Å^êUë÷êÊ
     MOVE   TO0-F06          TO   WRK-F06.
*î≠íçì˙
     MOVE   TO0-F08          TO   WRK-F07.
*î[ïiì˙
     MOVE   TO0-F09          TO   WRK-F08.
*èoâ◊ì˙
     MOVE   TO0-F10          TO   WRK-F09.
*è§ïiÇbÇc
     MOVE   TO0-F11          TO   WRK-F10.
*ïiíPÇbÇc
     MOVE   TO0-F12          TO   WRK-F11.
*íPâøãÊï™Å^êîó Å^íPâøÅ^ã‡äz
     MOVE   TO0-F14          TO   WRK-F12.
     MOVE   TO0-F15          TO   WRK-F13.
     MOVE   TO0-F16          TO   WRK-F14.
     MOVE   TO0-F17          TO   WRK-F15.
*ì¸å…Å^èoå…Å^î≠íçãÊï™Å|èÍèä
*## 2002/08/28 NAV START ## êîó œ≤≈ΩéûÇ…ãÊï™ÇîΩëŒÇ…Ç∑ÇÈ
     IF     TO0-F15  >=  ZERO
            MOVE   TO0-F19   TO   WRK-F16
     ELSE
            IF  TO0-F19  =  "1"
                MOVE "2"     TO   WRK-F16
            END-IF
            IF  TO0-F19  =  "2"
                MOVE "1"     TO   WRK-F16
            END-IF
     END-IF.
*****MOVE   TO0-F19          TO   WRK-F16.
     MOVE   TO0-F18          TO   WRK-F17.
*çÏã∆ãÊï™
     MOVE   TO0-F21          TO   WRK-F22.
*É}ÉXÉ^ãÊï™
     MOVE   "3"              TO   WRK-F20.
 FURI-30-EXIT.
     EXIT.
****************************************************************
*    êUë÷ÉfÅ[É^ÅiÇRÇPÅFçÏã∆é¿ê—Åj                              *
****************************************************************
 FURI-31-SEC                 SECTION.
*ïîñÂÉRÅ[ÉhéÊìæ
     MOVE   TO0-F01          TO   WRK-F01.
*ì`ï[ãÊï™
     MOVE   TO0-F02          TO   WRK-F02.
*ì`ï[Å°
     MOVE   TO0-F03          TO   WRK-F03.
*çsÅ°
     MOVE   TO0-F04          TO   WRK-F04.
*ê‘çïãÊï™
     MOVE   TO0-F05          TO   WRK-F05.
*êUë÷å≥Å^êUë÷êÊ
     MOVE   TO0-F06          TO   WRK-F06.
*î≠íçì˙
     MOVE   TO0-F08          TO   WRK-F07.
*î[ïiì˙
     MOVE   TO0-F09          TO   WRK-F08.
*èoâ◊ì˙
     MOVE   TO0-F10          TO   WRK-F09.
*è§ïiÇbÇc
     MOVE   TO0-F11          TO   WRK-F10.
*ïiíPÇbÇc
     MOVE   TO0-F12          TO   WRK-F11.
*íPâøãÊï™Å^êîó Å^íPâøÅ^ã‡äz
     MOVE   TO0-F14          TO   WRK-F12.
     MOVE   TO0-F15          TO   WRK-F13.
     MOVE   TO0-F16          TO   WRK-F14.
     MOVE   TO0-F17          TO   WRK-F15.
*ì¸å…Å^èoå…Å^î≠íçãÊï™Å|èÍèä
*****MOVE   "1"              TO   WRK-F16.
*## 2002/03/13 NAV START ## êîó œ≤≈ΩéûÇ…ãÊï™ÇîΩëŒÇ…Ç∑ÇÈ
     IF     TO0-F15  >=  ZERO
            MOVE   "1"       TO   WRK-F16
     ELSE
            MOVE   "2"       TO   WRK-F16
     END-IF.
*## 2002/03/13 NAV END   ##
     MOVE   TO0-F18          TO   WRK-F17.
*çÏã∆ãÊï™
     MOVE   TO0-F21          TO   WRK-F22.
*É}ÉXÉ^ãÊï™
     MOVE   "3"              TO   WRK-F20.
 FURI-31-EXIT.
     EXIT.
****************************************************************
*    êUë÷ÉfÅ[É^ÅiÇRÇQÅFçÏã∆é¿ê—Åj                              *
****************************************************************
 FURI-32-SEC                 SECTION.
*ïîñÂÉRÅ[ÉhéÊìæ
     MOVE   TO0-F01          TO   WRK-F01.
*ì`ï[ãÊï™
     MOVE   TO0-F02          TO   WRK-F02.
*ì`ï[Å°
     MOVE   TO0-F03          TO   WRK-F03.
*çsÅ°
     MOVE   TO0-F04          TO   WRK-F04.
*ê‘çïãÊï™
     MOVE   TO0-F05          TO   WRK-F05.
*êUë÷å≥Å^êUë÷êÊ
     MOVE   TO0-F06          TO   WRK-F06.
*î≠íçì˙
     MOVE   TO0-F08          TO   WRK-F07.
*î[ïiì˙
     MOVE   TO0-F09          TO   WRK-F08.
*èoâ◊ì˙
     MOVE   TO0-F10          TO   WRK-F09.
*è§ïiÇbÇc
     MOVE   TO0-F11          TO   WRK-F10.
*ïiíPÇbÇc
     MOVE   TO0-F12          TO   WRK-F11.
*íPâøãÊï™Å^êîó Å^íPâøÅ^ã‡äz
     MOVE   TO0-F14          TO   WRK-F12.
     MOVE   TO0-F15          TO   WRK-F13.
     MOVE   TO0-F16          TO   WRK-F14.
     MOVE   TO0-F17          TO   WRK-F15.
*ì¸å…Å^èoå…Å^î≠íçãÊï™Å|èÍèä
*****MOVE   "2"              TO   WRK-F16.
*## 2002/03/13 NAV START ## êîó œ≤≈ΩéûÇ…ãÊï™ÇîΩëŒÇ…Ç∑ÇÈ
     IF     TO0-F15  >=  ZERO
            MOVE   "2"       TO   WRK-F16
     ELSE
            MOVE   "1"       TO   WRK-F16
     END-IF.
*## 2002/03/13 NAV END   ##
     MOVE   TO0-F18          TO   WRK-F17.
*çÏã∆ãÊï™
     MOVE   TO0-F21          TO   WRK-F22.
*É}ÉXÉ^ãÊï™
     MOVE   "3"              TO   WRK-F20.
 FURI-32-EXIT.
     EXIT.
****************************************************************
*      3.0        èIóπèàóù                                     *
****************************************************************
 END-SEC                SECTION.
     CLOSE              TOKU  HON  FUK  SEN  HOK  OSA  OKA
                        BUTOKMF1 BUTOKMF2.
     DISPLAY  "TOKU       (IN) = "  READ-CNT      UPON   CONS.
     DISPLAY  "HON      (Œ›º¨) = "  HON-CNT       UPON   CONS.
     DISPLAY  "FUK      (Ã∏µ∂) = "  FUK-CNT       UPON   CONS.
     DISPLAY  "SEN     (æ›¿ﬁ≤) = "  SEN-CNT       UPON   CONS.
     DISPLAY  "OKA      (µ∂‘œ) = "  OKA-CNT       UPON   CONS.
     DISPLAY  "HOK   (ŒØ∂≤ƒﬁ≥) = "  HOK-CNT       UPON   CONS.
     DISPLAY  "OSA      (µµª∂) = "  OSA-CNT       UPON   CONS.
     DISPLAY  "SKIP  (Õﬁ¬ÃﬁºÆ) = "  SKIP-CNT      UPON   CONS.
 END-END.
     EXIT.
******************<<  PROGRAM  END  >>**************************
