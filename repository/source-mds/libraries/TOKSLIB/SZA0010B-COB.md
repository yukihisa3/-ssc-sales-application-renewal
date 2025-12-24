# SZA0010B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIB/SZA0010B.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿              *
*    業務名　　　　　　　：　営業所間在庫データ　　　　　　　　*
*    モジュール名　　　　：　指定倉庫データ抽出                *
*    作成日／作成者　　　：　2000/03/02 TAKAHASHI              *
*    再利用ＰＧ　　　　　：                                    *
*    更新日／更新者　　　：　                                  *
*                                                              *
****************************************************************
 IDENTIFICATION              DIVISION.
 PROGRAM-ID.                 SZA0010B.
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
     SELECT      ZAIKORCV    ASSIGN    TO        DA-01-S-ZAIKORCV
                             FILE      STATUS    ZAR-ST.
*商品在庫マスタ
     SELECT     ZAMZAIF      ASSIGN    TO        DA-01-VI-ZAMZAIL1
                             ORGANIZATION        INDEXED
                             ACCESS    MODE      DYNAMIC
                             RECORD    KEY       ZAI-F01
                                                 ZAI-F02
                                                 ZAI-F03
                             FILE      STATUS    ZAI-ST.
****************************************************************
 DATA                        DIVISION.
****************************************************************
 FILE                        SECTION.
*送信用在庫マスタ
 FD  ZAIKORCV.
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
     03  OUT-CNT             PIC  9(07)  VALUE  ZERO.
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
 LINKAGE                     SECTION.
 01  LINK-SOKCD              PIC  9(02).
****************************************************************
 PROCEDURE                   DIVISION  USING LINK-SOKCD.
****************************************************************
 DECLARATIVES.
 ZAR-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE ZAIKORCV.
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
     OPEN        INPUT       ZAMZAIF
                 OUTPUT      ZAIKORCV.
*在庫マスタスタート
     MOVE      LINK-SOKCD      TO   ZAI-F01.
     MOVE      SPACE           TO   ZAI-F02.
     MOVE      SPACE           TO   ZAI-F03.
     START ZAMZAIF KEY IS   >=   ZAI-F01 ZAI-F02 ZAI-F03
           INVALID
           MOVE       9    TO   END-FLG
           DISPLAY "##ﾁｭｳｼｭﾂ ﾀｲｼｮｳ ｿｳｺ ﾅｼ##"   UPON CONS
           DISPLAY "##ｿｳｺｺｰﾄﾞ = " LINK-SOKCD "       #" UPON CONS
           GO                  TO   INIT-EXIT
     END-START.
*在庫マスタ読込
     PERFORM   ZAIKO-READ-SEC.
     IF    END-FLG = 9
           DISPLAY "##ﾁｭｳｼｭﾂ ﾀｲｼｮｳ ｿｳｺ ﾅｼ##"   UPON CONS
           DISPLAY "##ｿｳｺｺｰﾄﾞ = " LINK-SOKCD "       #" UPON CONS
           GO                  TO   INIT-EXIT
     END-IF.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*                 M A I N - S E C
****************************************************************
 MAIN-SEC                    SECTION.
     MOVE     "MAIN-SEC"          TO   S-NAME.
*送信用レコード初期化
     MOVE      SPACE              TO   ZAR-REC.
     INITIALIZE                        ZAR-REC.
*送信用レコードセット
     MOVE      ZAI-REC            TO   ZAR-REC.
     ADD       1                  TO   OUT-CNT.
*送信用レコード更新
     WRITE     ZAR-REC.
*在庫マスタ読込
     PERFORM   ZAIKO-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*              在庫マスタ読込
****************************************************************
 ZAIKO-READ-SEC              SECTION.
     MOVE     "ZAIKO-READ-SEC"    TO   S-NAME.
*
     READ     ZAMZAIF    NEXT   AT   END
              MOVE      9         TO   END-FLG
              GO        TO        ZAIKO-READ-EXIT
     END-READ.
 READ001.
*指定倉庫ＣＤ以外はエラー
     IF       ZAI-F01  >  LINK-SOKCD
              MOVE      9         TO   END-FLG
              GO        TO        ZAIKO-READ-EXIT
     END-IF.
 READ002.
*読込件数
     ADD      1                   TO   READ-CNT.
     IF       READ-CNT(6:2) = "00"
              DISPLAY "## READ-CNT = " READ-CNT UPON CONS
     END-IF.
*
 ZAIKO-READ-EXIT.
     EXIT.
****************************************************************
*    終了
****************************************************************
 END-SEC                   SECTION.
*
     CLOSE ZAIKORCV ZAMZAIF.
*
     DISPLAY "## READ-CNT = " READ-CNT " ##"  UPON  CONS.
     DISPLAY "## OUT-CNT  = " OUT-CNT  " ##"  UPON  CONS.
*
 END-EXIT.
     EXIT.

```
