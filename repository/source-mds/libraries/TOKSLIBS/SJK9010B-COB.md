# SJK9010B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SJK9010B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　サカタのタネ（株）殿　　　　　　　*
*    業務名　　　　　　　：　受注自動欠品業務　　　　　　　　　*
*    モジュール名　　　　：　受注自動欠品　在庫更新　　　　　　*
*    作成日／作成者　　　：　2015/10/13 TAKAHASHI              *
*    更新日／更新者　　　：　XXXX/XX/XX XXXXXXXXXXXXX          *
*                            パラメタを受け取り、在庫更新処理　*
*                            を行なう。　　　　　　　　　　　　*
****************************************************************
 IDENTIFICATION              DIVISION.
 PROGRAM-ID.                 SJK9010B.
 ENVIRONMENT                 DIVISION.
 CONFIGURATION               SECTION.
 SOURCE-COMPUTER.
 OBJECT-COMPUTER.
 SPECIAL-NAMES.
     CONSOLE       IS        CONS.
***************************************************************
 INPUT-OUTPUT              SECTION.
****************************************************************
 FILE-CONTROL.
*商品在庫マスタ
     SELECT     ZAMZAIF      ASSIGN    TO        DA-01-VI-ZAMZAIL1
                             ORGANIZATION        INDEXED
                             ACCESS    MODE      RANDOM
                             RECORD    KEY       ZAI-F01
                                                 ZAI-F02
                                                 ZAI-F03
                             FILE      STATUS    ZAI-ST.
*商品コード変換テーブル
     SELECT     HSHOTBL      ASSIGN    TO        DA-01-VI-SHOTBL1
                             ORGANIZATION        INDEXED
                             ACCESS    MODE      RANDOM
                             RECORD    KEY       SHO-F01
                                                 SHO-F02
                             FILE      STATUS    SHO-ST.
*商品名称マスタ
     SELECT     HMEIMS       ASSIGN    TO        DA-01-VI-MEIMS1
                             ORGANIZATION        INDEXED
                             ACCESS    MODE      RANDOM
                             RECORD    KEY       MEI-F01
                             FILE      STATUS    MEI-ST.
****************************************************************
 DATA                        DIVISION.
****************************************************************
 FILE                        SECTION.
*商品在庫マスタ
 FD  ZAMZAIF.
     COPY     ZAMZAIF   OF        XFDLIB
              JOINING   ZAI       PREFIX.
*商品変換テーブル
 FD  HSHOTBL.
     COPY     HSHOTBL   OF        XFDLIB
              JOINING   SHO       PREFIX.
*商品名称マスタ
 FD  HMEIMS.
     COPY     HMEIMS    OF        XFDLIB
              JOINING   MEI       PREFIX.
****************************************************************
 WORKING-STORAGE           SECTION.
****************************************************************
 01  ST-AREA.
     03  ZAI-ST              PIC  X(02)  VALUE  SPACE.
     03  SHO-ST              PIC  X(02)  VALUE  SPACE.
     03  MEI-ST              PIC  X(02)  VALUE  SPACE.
 01  WK-AREA.
     03  ZAMZAIF-INV-FLG     PIC  X(03)  VALUE  SPACE.
     03  HMEIMS-INV-FLG      PIC  X(03)  VALUE  SPACE.
     03  HSHOTBL-INV-FLG     PIC  X(03)  VALUE  SPACE.
*計算領域
 01  WRK-AREA.
     03  WRK-HIK             PIC S9(09)V9(02)  VALUE ZERO.
     03  WRK-ZAI             PIC S9(09)V9(02)  VALUE ZERO.
*日付取得
 01  SYS-DATE                PIC  9(06)  VALUE  ZERO.
 01  WK-DATE8.
     03  WK-Y                PIC  9(04)  VALUE  ZERO.
     03  WK-M                PIC  9(02)  VALUE  ZERO.
     03  WK-D                PIC  9(02)  VALUE  ZERO.
***  エラーセクション名
 01  SEC-NAME.
     03  FILLER                   PIC  X(18)
         VALUE "### ERR-SEC    => ".
     03  S-NAME                   PIC  X(20).
*
 01  FILE-ERR.
     03  ZAI-ERR             PIC  N(10)  VALUE
                   NC"在庫マスタ異常".
     03  SHO-ERR             PIC  N(10)  VALUE
                   NC"商品変換テーブル異常".
     03  MEI-ERR             PIC  N(10)  VALUE
                   NC"商品名称マスタ異常".
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN             PIC X(01).
 01  LINK-IN-YMD6            PIC 9(06).
 01  LINK-IN-YMD8            PIC 9(08).
 01  LINK-OUT-RET            PIC X(01).
 01  LINK-OUT-YMD            PIC 9(08).
*
****************************************************************
 LINKAGE                     SECTION.
****************************************************************
 01  LINK-MODE                   PIC  X(01).
 01  LINK-TOKCD                  PIC  9(08).
 01  LINK-AITESYOCD              PIC  X(13).
 01  LINK-SOKCD                  PIC  X(02).
 01  LINK-SKTSYOCD               PIC  X(08).
 01  LINK-SKTHINCD               PIC  X(08).
 01  LINK-NOUHIN                 PIC  9(08).
 01  LINK-MAE-SURYO              PIC  9(09).
 01  LINK-ATO-SURYO              PIC  9(09).
 01  LINK-TANABAN                PIC  X(06).
 01  LINK-HIKIATEFLG             PIC  9(01).
****************************************************************
 PROCEDURE                   DIVISION  USING LINK-MODE
                                             LINK-TOKCD
                                             LINK-AITESYOCD
                                             LINK-SOKCD
                                             LINK-SKTSYOCD
                                             LINK-SKTHINCD
                                             LINK-NOUHIN
                                             LINK-MAE-SURYO
                                             LINK-ATO-SURYO
                                             LINK-TANABAN
                                             LINK-HIKIATEFLG.
****************************************************************
 DECLARATIVES.
 ZAI-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE ZAMZAIF.
     DISPLAY     ZAI-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     ZAI-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     STOP        RUN.
 SHO-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE HSHOTBL.
     DISPLAY     SHO-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     SHO-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     STOP        RUN.
 MEI-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE HMEIMS.
     DISPLAY     MEI-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     MEI-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     STOP        RUN.
 END DECLARATIVES.
****************************************************************
*                 P R O G R A M - S E C
****************************************************************
 PROGRAM-SEC                 SECTION.
     PERFORM     INIT-SEC.
     PERFORM     MAIN-SEC.
     PERFORM     END-SEC.
 PROGRAM-END.
     EXIT     PROGRAM.
****************************************************************
*                 I N I T - S E C
****************************************************************
 INIT-SEC                    SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
*
     OPEN        INPUT       HMEIMS    HSHOTBL.
     OPEN        I-O         ZAMZAIF.
*システム日付・時刻の取得
     ACCEPT   SYS-DATE          FROM   DATE.
     MOVE     "3"                 TO   LINK-IN-KBN.
     MOVE     SYS-DATE            TO   LINK-IN-YMD6.
     MOVE     ZERO                TO   LINK-IN-YMD8.
     MOVE     ZERO                TO   LINK-OUT-RET.
     MOVE     ZERO                TO   LINK-OUT-YMD.
     CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD.
     MOVE      LINK-OUT-YMD       TO   WK-DATE8.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*                 M A I N - S E C
****************************************************************
 MAIN-SEC                    SECTION.
     MOVE     "MAIN-SEC"          TO   S-NAME.
*商品変換ＴＢＬ索引
     PERFORM  HSHOTBL-READ-SEC.
     IF  HSHOTBL-INV-FLG = "INV"
*********商品変換ＴＢＬが未登録の場合は、更新しない。
         MOVE   ZERO              TO   LINK-HIKIATEFLG
         GO                       TO   MAIN-EXIT
     END-IF.
*パラメタ：モードが１（欠品）の時
     IF   LINK-MODE  =  "1"
**********在庫マスタ索引
          PERFORM  ZAIKO-SEC
          IF   ZAMZAIF-INV-FLG = "INV"
               MOVE   ZERO        TO   LINK-HIKIATEFLG
               GO                 TO   MAIN-EXIT
          ELSE
               IF  LINK-HIKIATEFLG = 1
                   COMPUTE ZAI-F27 = ZAI-F27 - LINK-MAE-SURYO
                   COMPUTE ZAI-F28 = ZAI-F28 - LINK-MAE-SURYO
                   MOVE  ZERO     TO   LINK-HIKIATEFLG
               ELSE
                   COMPUTE ZAI-F27 = ZAI-F27 - LINK-MAE-SURYO
                   MOVE  ZERO     TO   LINK-HIKIATEFLG
               END-IF
               MOVE  WK-DATE8     TO   ZAI-F99
               REWRITE ZAI-REC
         END-IF
     END-IF.
*パラメタ：モードが２（戻し）の時
     IF   LINK-MODE  =  "2"
**********在庫マスタ索引
         PERFORM  ZAIKO-SEC
         IF  ZAMZAIF-INV-FLG = "INV"
*************在庫マスタ作成
             MOVE  SPACE          TO  ZAI-REC
             INITIALIZE               ZAI-REC
             MOVE  LINK-SOKCD     TO  ZAI-F01
             MOVE  LINK-SKTSYOCD  TO  ZAI-F021
             MOVE  LINK-SKTHINCD  TO  ZAI-F022
             MOVE  SHO-F08        TO  ZAI-F03
             PERFORM HMEIMS-READ-SEC
             IF  HMEIMS-INV-FLG = SPACE
                 MOVE  MEI-F031   TO   ZAI-F30
             END-IF
             MOVE  LINK-TOKCD     TO   ZAI-F29
             MOVE  WK-DATE8       TO   ZAI-F98
             MOVE  WK-DATE8       TO   ZAI-F99
             MOVE  LINK-ATO-SURYO TO   ZAI-F27
             WRITE ZAI-REC
         ELSE
             IF  LINK-HIKIATEFLG = 1
                 COMPUTE ZAI-F27 = ZAI-F27 - LINK-MAE-SURYO
                 COMPUTE ZAI-F28 = ZAI-F28 - LINK-MAE-SURYO
                 MOVE  ZERO       TO   LINK-HIKIATEFLG
             ELSE
                 COMPUTE ZAI-F27 = ZAI-F27 - LINK-MAE-SURYO
                 MOVE  ZERO       TO   LINK-HIKIATEFLG
             END-IF
             COMPUTE WRK-ZAI = ZAI-F04 - ZAI-F28
             COMPUTE WRK-HIK = WRK-ZAI - LINK-ATO-SURYO
             IF  WRK-HIK  >=  ZERO
                 COMPUTE ZAI-F27 = ZAI-F27 + LINK-ATO-SURYO
                 COMPUTE ZAI-F28 = ZAI-F28 + LINK-ATO-SURYO
                 MOVE  1          TO   LINK-HIKIATEFLG
             ELSE
                 COMPUTE ZAI-F27 = ZAI-F27 + LINK-ATO-SURYO
                 MOVE  ZERO       TO   LINK-HIKIATEFLG
             END-IF
             MOVE      WK-DATE8   TO   ZAI-F99
             REWRITE  ZAI-REC
     END-IF.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*      2.2     在庫引当                                        *
****************************************************************
 ZAIKO-SEC              SECTION.
*
     MOVE     "ZAIKO-SEC"         TO   S-NAME.
*商品在庫マスタ存在チェック
     MOVE    LINK-SOKCD      TO   ZAI-F01.
     MOVE    LINK-SKTSYOCD   TO   ZAI-F021.
     MOVE    LINK-SKTHINCD   TO   ZAI-F022.
     MOVE    SHO-F08         TO   ZAI-F03.
     READ    ZAMZAIF
             INVALID
             MOVE  "INV"     TO   ZAMZAIF-INV-FLG
             NOT  INVALID
             MOVE  SPACE     TO   ZAMZAIF-INV-FLG
     END-READ.
*
 ZAIKO-EXIT.
     EXIT.
****************************************************************
*                商品変換テーブル読込み                        *
****************************************************************
 HSHOTBL-READ-SEC          SECTION.
*
     MOVE      "HSHOTBL-READ-SEC" TO    S-NAME.
*
     MOVE      LINK-TOKCD         TO     SHO-F01.
     MOVE      LINK-AITESYOCD     TO     SHO-F02.
     READ      HSHOTBL
               INVALID
               MOVE      "INV"    TO    HSHOTBL-INV-FLG
               NOT  INVALID
               MOVE      SPACE    TO    HSHOTBL-INV-FLG
     END-READ.
*
 HSHOTBL-READ-EXIT.
     EXIT.
****************************************************************
*                商品名称マスタ読込み                          *
****************************************************************
 HMEIMS-READ-SEC           SECTION.
*
     MOVE      "HMEIMS-READ-SEC"  TO    S-NAME.
*
     MOVE      LINK-SKTSYOCD      TO    MEI-F011.
     MOVE      LINK-SKTHINCD      TO    MEI-F012.
     READ      HMEIMS
               INVALID
               MOVE      "INV"    TO    HMEIMS-INV-FLG
               NOT  INVALID
               MOVE      SPACE    TO    HMEIMS-INV-FLG
     END-READ.
*
 HMEIMS-READ-EXIT.
     EXIT.
****************************************************************
*    終了
****************************************************************
 END-SEC                   SECTION.
*
     CLOSE HSHOTBL HMEIMS ZAMZAIF.
*
 END-EXIT.
     EXIT.

```
