****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿              *
*    業務名　　　　　　　：　営業所間在庫データ　　　　　　　　*
*    モジュール名　　　　：　倉庫データ更新                    *
*    作成日／作成者　　　：　2000/05/20 TAKAHASHI              *
*    再利用ＰＧ　　　　　：                                    *
*    更新日／更新者　　　：　                                  *
*                                                              *
****************************************************************
 IDENTIFICATION              DIVISION.
 PROGRAM-ID.                 SZA0020B.
 ENVIRONMENT                 DIVISION.
 CONFIGURATION               SECTION.
 SOURCE-COMPUTER.
 OBJECT-COMPUTER.
 SPECIAL-NAMES.
     CONSOLE       IS        CONS
     STATION       IS        STAT.
****************************************************************
 INPUT-OUTPUT              SECTION.
****************************************************************
 FILE-CONTROL.
*送信用在庫マスタ
     SELECT     ZAIKOSND     ASSIGN    TO       DA-01-S-ZAIKOSND
                             FILE      STATUS   ZAR-ST.
*商品在庫マスタ
     SELECT     ZAMZAIF       ASSIGN    TO       DA-01-VI-ZAMZAIF
                             ORGANIZATION       INDEXED
                             ACCESS    MODE     RANDOM
                             RECORD    KEY      ZAI-F01
                                                ZAI-F02
                                                ZAI-F03
                             FILE      STATUS   ZAI-ST.
****************************************************************
 DATA                        DIVISION.
****************************************************************
 FILE                        SECTION.
*送信用在庫マスタ
 FD  ZAIKOSND.
     COPY     ZAMZAIF    OF        XFDLIB
              JOINING   ZAR       PREFIX.
*商品在庫マスタ
 FD  ZAMZAIF.
     COPY     ZAMZAIF    OF        XFDLIB
              JOINING   ZAI       PREFIX.
****************************************************************
 WORKING-STORAGE           SECTION.
****************************************************************
 01  ST-AREA.
     03  IN-DATA             PIC  X(01)  VALUE  SPACE.
     03  ZAR-ST              PIC  X(02)  VALUE  SPACE.
     03  ZAI-ST              PIC  X(02)  VALUE  SPACE.
 01  WK-AREA.
     03  END-FLG             PIC  9(01)  VALUE  ZERO.
     03  WT-CNT              PIC  9(07)  VALUE  ZERO.
     03  REWT-CNT            PIC  9(07)  VALUE  ZERO.
     03  READ-CNT            PIC  9(07)  VALUE  ZERO.
 01  WK-SYSTEM-DATE.
     03  SYS-DATE            PIC  9(06)  VALUE  ZERO.
     03  SYS-DATE-8          PIC  9(08)  VALUE  ZERO.
*
 01  FILE-ERR.
     03  ZAR-ERR             PIC  N(10)  VALUE
                   NC"送信用在庫マスタ異常".
     03  ZAI-ERR             PIC  N(10)  VALUE
                   NC"在庫マスタ異常".
*
 01  SEC-NAME.
     03  FILLER              PIC  X(16)  VALUE "## ｴﾗｰSECTION = ".
     03  S-NAME              PIC  X(20).
     03  FILLER              PIC  X(03)  VALUE " ##".
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN             PIC X(01).
 01  LINK-IN-YMD6            PIC 9(06).
 01  LINK-IN-YMD8            PIC 9(08).
 01  LINK-OUT-RET            PIC X(01).
 01  LINK-OUT-YMD            PIC 9(08).
*
****************************************************************
 PROCEDURE                   DIVISION.
****************************************************************
 DECLARATIVES.
 ZAR-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE ZAIKOSND.
     DISPLAY     ZAR-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     ZAR-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     STOP        RUN.
 ZAI-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE ZAMZAIF.
     DISPLAY     ZAI-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     ZAI-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     STOP        RUN.
 END DECLARATIVES.
****************************************************************
*                 P R O G R A M - S E C
****************************************************************
 PROGRAM-SEC                 SECTION.
     PERFORM     INIT-SEC.
     PERFORM     MAIN-SEC    UNTIL     END-FLG  = 9.
     PERFORM     END-SEC.
     STOP        RUN.
*PROGRAM-END.
****************************************************************
*                 I N I T - S E C
****************************************************************
 INIT-SEC                    SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
*
     OPEN        I-O         ZAMZAIF
                 INPUT       ZAIKOSND.
*受信在庫マスタ読込
     PERFORM   ZAIKOSND-READ-SEC.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*                 M A I N - S E C
****************************************************************
 MAIN-SEC                    SECTION.
     MOVE     "MAIN-SEC"          TO   S-NAME.
*在庫マスタ検索
     MOVE      ZAR-F01            TO   ZAI-F01.
     MOVE      ZAR-F02            TO   ZAI-F02.
     MOVE      ZAR-F03            TO   ZAI-F03.
     READ      ZAMZAIF
               INVALID
                 MOVE    SPACE    TO   ZAI-REC
                 INITIALIZE            ZAI-REC
                 MOVE    ZAR-REC  TO   ZAI-REC
                 WRITE   ZAI-REC
                 ADD     1        TO   WT-CNT
               NOT  INVALID
                 MOVE    ZAR-REC  TO   ZAI-REC
                 REWRITE ZAI-REC
                 ADD     1        TO   REWT-CNT
     END-READ.
*
     PERFORM  ZAIKOSND-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*              在庫マスタ読込
****************************************************************
 ZAIKOSND-READ-SEC           SECTION.
     MOVE     "ZAIKOSND-READ-SEC" TO   S-NAME.
*
     READ     ZAIKOSND
              AT  END
               MOVE      9         TO   END-FLG
              NOT AT  END
               ADD       1         TO   READ-CNT
     END-READ.
*
 ZAIKOSND-REAC-EXIT.
     EXIT.
****************************************************************
*    終了
****************************************************************
 END-SEC                   SECTION.
*
     CLOSE ZAIKOSND ZAMZAIF.
*
     DISPLAY "## ｴｲｷﾞｮｳｼｮ ｻﾞｲｺ ﾖﾐｺﾐ = " READ-CNT UPON  CONS.
     DISPLAY "## ｴｲｷﾞｮｳｼｮ ｻﾞｲｺ ｺｳｼﾝ = " REWT-CNT UPON  CONS.
     DISPLAY "## ｴｲｷﾞｮｳｼｮ ｻﾞｲｺ ｻｸｾｲ = " WT-CNT   UPON  CONS.
*
 END-EXIT.
     EXIT.
