****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿              *
*    業務名　　　　　　　：　振替依頼処理　　　　　　　　　　　*
*    モジュール名　　　　：　本社振替依頼データ抽出            *
*    作成日／作成者　　　：　2000/06/20 Y.YOSHIDA              *
*    再利用ＰＧ　　　　　：                                    *
*    更新日／更新者　　　：　                                  *
*                            振替依頼データより依頼区分１の    *
*                            データを抽出し、元は削除する。    *
****************************************************************
 IDENTIFICATION              DIVISION.
 PROGRAM-ID.                 SFU0040B.
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
*送信用振替依頼データ
     SELECT     FUIRAI2      ASSIGN    TO        DA-01-S-FUIRAI2
                             FILE      STATUS    FI2-ST.
*振替依頼ファイル
     SELECT     FUIRAIF      ASSIGN    TO        DA-01-VI-FUIRAIL1
                             ORGANIZATION        INDEXED
                             ACCESS    MODE      SEQUENTIAL
                             RECORD    KEY       FIR-F01
                                                 FIR-F02
                                                 FIR-F03
                             FILE      STATUS    FIR-ST.
****************************************************************
 DATA                        DIVISION.
****************************************************************
 FILE                        SECTION.
*送信用振替依頼データ
 FD  FUIRAI2.
     COPY     FUIRAIF    OF        XFDLIB
              JOINING    FI2       PREFIX.
*振替依頼マスタ
 FD  FUIRAIF.
     COPY     FUIRAIF    OF        XFDLIB
              JOINING    FIR       PREFIX.
****************************************************************
 WORKING-STORAGE           SECTION.
****************************************************************
 01  ST-AREA.
     03  FI2-ST              PIC  X(02)  VALUE  SPACE.
     03  FIR-ST              PIC  X(02)  VALUE  SPACE.
 01  WK-AREA.
     03  END-FLG             PIC  9(01)  VALUE  ZERO.
     03  OUT-CNT             PIC  9(07)  VALUE  ZERO.
     03  READ-CNT            PIC  9(07)  VALUE  ZERO.
     03  SELECT-CNT          PIC  9(07)  VALUE  ZERO.
*
 01  FILE-ERR.
     03  FI2-ERR             PIC  N(12)  VALUE
                   NC"送信用振替依頼データ異常".
     03  FIR-ERR             PIC  N(10)  VALUE
                   NC"振替依頼ファイル異常".
*
 01  SEC-NAME.
     03  FILLER              PIC  X(16)  VALUE "## ｴﾗｰSECTION = ".
     03  S-NAME              PIC  X(20).
     03  FILLER              PIC  X(03)  VALUE " ##".
*
 LINKAGE                     SECTION.
 01  LINK-SOKCD              PIC  X(02).
 01  LINK-DSOKCD             PIC  X(02).
****************************************************************
 PROCEDURE                   DIVISION  USING LINK-SOKCD
                                             LINK-DSOKCD.
****************************************************************
 DECLARATIVES.
 FI2-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE FUIRAI2.
     DISPLAY     FI2-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     FI2-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     STOP        RUN.
 FIR-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE FUIRAIF.
     DISPLAY     FIR-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     FIR-ST      UPON      CONS.
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
     OPEN        I-O         FUIRAIF
                 OUTPUT      FUIRAI2.
*振替依頼ファイルスタート
**   MOVE      SPACE         TO   FIR-REC.
**   INITIALIZE                   FIR-REC.
**   MOVE      LINK-SOKCD    TO   FIR-F01.
**   START FUIRAIF KEY IS    >=   FIR-F01 FIR-F02 FIR-F03
**         INVALID
**         MOVE    9         TO   END-FLG
**         GO      TO   INIT-EXIT
**   END-START.
*振替依頼ファイル読込
     PERFORM   FUIRAI-READ-SEC.
     IF    END-FLG = 9
***********DISPLAY "##ﾁｭｳｼｭﾂ ﾀｲｼｮｳ ｿｳｺ ﾅｼ##"   UPON CONS
***********DISPLAY "##ｿｳｺｺｰﾄﾞ = " LINK-SOKCD "       #" UPON CONS
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
     MOVE      SPACE              TO   FI2-REC.
     INITIALIZE                        FI2-REC.
*送信用レコードセット
     MOVE      FIR-REC            TO   FI2-REC.
     ADD       1                  TO   OUT-CNT.
*送信用レコード更新
     WRITE     FI2-REC.
*対象の振替依頼ファイル削除
     DELETE    FUIRAIF.
*振替依頼ファイル読込
     PERFORM   FUIRAI-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*              振替依頼ファイル読込
****************************************************************
 FUIRAI-READ-SEC             SECTION.
     MOVE     "FUIRAI-READ-SEC"   TO   S-NAME.
*
     READ     FUIRAIF   NEXT      AT END
              MOVE      9         TO   END-FLG
              GO        TO        FUIRAI-READ-EXIT
     END-READ.
 READ001.
*指定倉庫ＣＤ以外はエラー
**   IF       FIR-F01   >    LINK-SOKCD
**            MOVE      9         TO   END-FLG
**            GO        TO        FUIRAI-READ-EXIT
**   END-IF.
*読込件数
     ADD      1    TO   READ-CNT.
*依頼区分＝１のみ対象
     IF       FIR-F10   =   "1"
              ADD       1    TO   SELECT-CNT
     ELSE
              GO   TO   FUIRAI-READ-SEC
     END-IF.
*
 FUIRAI-READ-EXIT.
     EXIT.
****************************************************************
*    終了
****************************************************************
 END-SEC                   SECTION.
*
     CLOSE FUIRAI2 FUIRAIF.
*
     DISPLAY "## READ-CNT   = " READ-CNT   " ##"  UPON  CONS.
     DISPLAY "## SELECT-CNT = " SELECT-CNT " ##"  UPON  CONS.
     DISPLAY "## OUT-CNT    = " OUT-CNT    " ##"  UPON  CONS.
*
 END-EXIT.
     EXIT.
