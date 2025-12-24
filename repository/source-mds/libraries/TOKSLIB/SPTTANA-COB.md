# SPTTANA

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIB/SPTTANA.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿              *
*    業務名　　　　　　　：　倉庫別_番データ                  *
*    モジュール名　　　　：　倉庫別_番マスタ抽出              *
*    作成日／作成者　　　：　2000/03/22 TAKAHASHI              *
*    再利用ＰＧ　　　　　：                                    *
*    更新日／更新者　　　：　                                  *
*                                                              *
****************************************************************
 IDENTIFICATION              DIVISION.
 PROGRAM-ID.                 SPTTANA.
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
*送信用倉庫別_番マスタ
     SELECT      JHMTANWK    ASSIGN    TO        DA-01-S-JHMTANWK
                             FILE      STATUS    TAW-ST.
*倉庫別_番マスタ
     SELECT      JHMTANF     ASSIGN    TO        DA-01-VI-JHMTANL1
                             ORGANIZATION        INDEXED
                             ACCESS    MODE      DYNAMIC
                             RECORD    KEY       TAN-F01
                                                 TAN-F021
                                                 TAN-F022
                                                 TAN-F023
                                                 TAN-F024
                             FILE      STATUS    TAN-ST.
****************************************************************
 DATA                        DIVISION.
****************************************************************
 FILE                        SECTION.
*送信用倉庫別_番マスタ
 FD  JHMTANWK.
     COPY     JHMTANF   OF        XFDLIB
              JOINING   TAW       PREFIX.
*倉庫別_番マスタ
 FD  JHMTANF.
     COPY     JHMTANF   OF        XFDLIB
              JOINING   TAN       PREFIX.
****************************************************************
 WORKING-STORAGE           SECTION.
****************************************************************
 01  ST-AREA.
     03  IN-DATA             PIC  X(01)  VALUE  SPACE.
     03  TAW-ST              PIC  X(02)  VALUE  SPACE.
     03  TAN-ST              PIC  X(02)  VALUE  SPACE.
 01  WK-AREA.
     03  END-FLG             PIC  9(01)  VALUE  ZERO.
     03  OUT-CNT             PIC  9(07)  VALUE  ZERO.
     03  READ-CNT            PIC  9(07)  VALUE  ZERO.
 01  WK-SYSTEM-DATE.
     03  SYS-DATE            PIC  9(06)  VALUE  ZERO.
     03  SYS-DATE-8          PIC  9(08)  VALUE  ZERO.
*
 01  FILE-ERR.
     03  TAW-ERR             PIC  N(10)  VALUE
                   NC"送信用倉庫別_番異常".
     03  TAN-ERR             PIC  N(10)  VALUE
                   NC"倉庫別_番マスタ異常".
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
 TAW-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE JHMTANWK.
     DISPLAY     TAW-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     TAW-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     STOP        RUN.
 TAN-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE JHMTANF.
     DISPLAY     TAN-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     TAN-ST      UPON      CONS.
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
     OPEN        INPUT       JHMTANF
                 OUTPUT      JHMTANWK.
*倉庫別_番マスタスタート
     MOVE      SPACE           TO   TAN-REC.
     INITIALIZE                     TAN-REC.
     MOVE      LINK-SOKCD      TO   TAN-F01.
     START JHMTANF KEY IS >= TAN-F01 TAN-F021 TAN-F022 TAN-F023
                             TAN-F024
           INVALID
           MOVE       9    TO   END-FLG
           DISPLAY "##ﾁｭｳｼｭﾂ ﾀｲｼｮｳ ｿｳｺ ﾅｼ##"   UPON CONS
           DISPLAY "##ｿｳｺｺｰﾄﾞ = " LINK-SOKCD "       #" UPON CONS
           GO                  TO   INIT-EXIT
     END-START.
*倉庫別_番マスタ読込
     PERFORM   JHMTANF-READ-SEC.
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
     MOVE      SPACE              TO   TAW-REC.
     INITIALIZE                        TAW-REC.
*送信用レコードセット
     MOVE      TAN-REC            TO   TAW-REC.
     ADD       1                  TO   OUT-CNT.
*送信用レコード更新
     WRITE     TAW-REC.
*倉庫別_番マスタ読込み
     PERFORM   JHMTANF-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*              在庫マスタ読込
****************************************************************
 JHMTANF-READ-SEC            SECTION.
     MOVE     "JHMTANF-READ-SEC"  TO   S-NAME.
*
     READ     JHMTANF   NEXT   AT   END
              MOVE      9         TO   END-FLG
              GO        TO        JHMTANF-READ-EXIT
     END-READ.
*読込件数
     ADD      1                   TO   READ-CNT.
     IF       READ-CNT(6:2) = "00"
              DISPLAY "## READ-CNT = " READ-CNT UPON CONS
     END-IF.
 READ001.
*指定倉庫ＣＤ以外はエラー
     IF       TAN-F01  >  LINK-SOKCD
              MOVE      9         TO   END-FLG
              GO        TO        JHMTANF-READ-EXIT
     END-IF.
*
 JHMTANF-READ-EXIT.
     EXIT.
****************************************************************
*    終了
****************************************************************
 END-SEC                   SECTION.
*
     CLOSE JHMTANWK JHMTANF.
*
     DISPLAY "## READ-CNT = " READ-CNT " ##"  UPON  CONS.
     DISPLAY "## OUT-CNT  = " OUT-CNT  " ##"  UPON  CONS.
*
 END-EXIT.
     EXIT.

```
