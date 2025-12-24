# TAB0020B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/TAB0020B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　マスタ保守　　　　　　　　　　　　*
*    モジュール名　　　　：（ＥＸＣＥＬ取込）　　　　　　　　　*
*    　　　　　　　　　　　　倉庫商品別_番設定マスタ更新　　　*
*    　　　　　　　　　　　　※Ｄ３６５連携対応　　　　　　　　*
*    作成日／作成者　　　：　2020/04/24 INOUE                  *
*    処理内容　　　　　　：　取込チェックにてＯＫとなった　　　*
*    　　　　　　　　　　　　データより、倉庫商品別_番　　　　*
*                            設定マスタを更新する。　　　　　　*
*    流用元　　　　　　　：　STB0020B                          *
*    変更日／変更者　　　：　                                  *
*    変更内容　　　　　　：　                                  *
****************************************************************
 IDENTIFICATION              DIVISION.
 PROGRAM-ID.                 TAB0020B.
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
*取込ファイル
     SELECT      SYOTANW1    ASSIGN    TO       DA-01-VI-SYOTANW1
                             ORGANIZATION       INDEXED
                             ACCESS    MODE     SEQUENTIAL
                             RECORD    KEY      WRK-F112
                                                WRK-F113
                                                WRK-F20
                             FILE      STATUS   WRK-ST.
*倉庫商品別_番設定マスタ
     SELECT      SOKTANL1    ASSIGN    TO       DA-01-VI-SOKTANL1
                             ORGANIZATION       INDEXED
                             ACCESS    MODE     DYNAMIC
                             RECORD    KEY      SOK-F01
                                                SOK-F02
                             FILE      STATUS   SOK-ST.
****************************************************************
 DATA                        DIVISION.
****************************************************************
 FILE                        SECTION.
*取込ファイル
 FD  SYOTANW1.
     COPY        SYOTANW1    OF        XFDLIB
     JOINING     WRK         AS        PREFIX.
*倉庫商品別_番設定マスタ
 FD  SOKTANL1.
     COPY        SOKTANL1    OF        XFDLIB
     JOINING     SOK         AS        PREFIX.
****************************************************************
 WORKING-STORAGE           SECTION.
****************************************************************
 01  ST-AREA.
     03  WRK-ST              PIC  X(02)  VALUE  SPACE.
     03  SOK-ST              PIC  X(02)  VALUE  SPACE.
 01  WK-AREA.
     03  END-FLG             PIC  X(03)  VALUE  SPACE.
     03  SOK-FLG             PIC  X(03)  VALUE  SPACE.
     03  WRK-ENDFLG          PIC  X(01)  VALUE  SPACE.
     03  WRK-RD-CNT          PIC  9(07)  VALUE  ZERO.
     03  SOK-WT-CNT          PIC  9(07)  VALUE  ZERO.
     03  SOK-RT-CNT          PIC  9(07)  VALUE  ZERO.
     03  SOK-DL-CNT          PIC  9(07)  VALUE  ZERO.
**
*日付取得
 01  SYS-DATE                PIC  9(06)  VALUE  ZERO.
*
 01  SYS-DATE8               PIC  9(08)  VALUE  ZERO.
*
 01  WK-DATE8.
     03  WK-Y                PIC  9(04)  VALUE  ZERO.
     03  WK-M                PIC  9(02)  VALUE  ZERO.
     03  WK-D                PIC  9(02)  VALUE  ZERO.
*
 01  SYS-DATE2               PIC  9(08).
 01  FILLER                  REDEFINES  SYS-DATE2.
     03  SYS-YYYY.
         05  SYS-YY2-1       PIC  9(02).
         05  SYS-YY2-2       PIC  9(02).
     03  SYS-MM2             PIC  9(02).
     03  SYS-DD2             PIC  9(02).
*
 01  SYS-TIME                PIC  9(08).
 01  WK-TIME      REDEFINES  SYS-TIME.
   03  WK-TIME-HM            PIC  9(06).
   03  WK-TIME-FIL           PIC  X(02).
 01  FILLER       REDEFINES  SYS-TIME.
     03  SYS-HH              PIC  9(02).
     03  SYS-MN              PIC  9(02).
     03  SYS-SS              PIC  9(02).
     03  FILLER              PIC  9(02).
*
***  エラーセクション名
 01  SEC-NAME.
     03  FILLER                   PIC  X(18)
         VALUE "### ERR-SEC    => ".
     03  S-NAME                   PIC  X(20).
*
 01  FILE-ERR.
     03  WRK-ERR            PIC  N(10)  VALUE
                   NC"ＥＸＣＥＬ取込Ｆ異常".
     03  SOK-ERR             PIC  N(14)  VALUE
                   NC"倉庫商品別_番設定マスタ異常".
*
*メッセージ出力領域
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER          PIC  X(05)  VALUE " *** ".
         05  ST-PG           PIC  X(08)  VALUE "TAB0020B".
         05  FILLER          PIC  X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER          PIC  X(05)  VALUE " *** ".
         05  END-PG          PIC  X(08)  VALUE "TAB0020B".
         05  FILLER          PIC  X(11)  VALUE
                                         " END   *** ".
     03  MSG-OUT1.
         05  MSG-OUT1-FIL1   PIC  X(02)  VALUE "##".
         05  MSG-OUT1-FIL2   PIC  N(11)  VALUE
                             NC"ＥＸＣＥＬ読込　＝".
         05  MSG-OUT01       PIC  ZZZ,ZZ9.
         05  MSG-OUT1-FIL3   PIC  X(01)  VALUE " ".
         05  MSG-OUT1-FIL4   PIC  N(01)  VALUE NC"件".
     03  MSG-OUT2.
         05  MSG-OUT2-FIL1   PIC  X(02)  VALUE "##".
         05  MSG-OUT2-FIL2   PIC  N(11)  VALUE
                             NC"設定マスタ　作成＝".
         05  MSG-OUT02       PIC  ZZZ,ZZ9.
         05  MSG-OUT2-FIL3   PIC  X(01)  VALUE " ".
         05  MSG-OUT2-FIL4   PIC  N(01)  VALUE NC"件".
     03  MSG-OUT3.
         05  MSG-OUT3-FIL1   PIC  X(02)  VALUE "##".
         05  MSG-OUT3-FIL2   PIC  N(11)  VALUE
                             NC"設定マスタ　更新＝".
         05  MSG-OUT03       PIC  ZZZ,ZZ9.
         05  MSG-OUT3-FIL3   PIC  X(01)  VALUE " ".
         05  MSG-OUT3-FIL4   PIC  N(01)  VALUE NC"件".
     03  MSG-OUT4.
         05  MSG-OUT4-FIL1   PIC  X(02)  VALUE "##".
         05  MSG-OUT4-FIL2   PIC  N(11)  VALUE
                             NC"設定マスタ　削除＝".
         05  MSG-OUT04       PIC  ZZZ,ZZ9.
         05  MSG-OUT4-FIL3   PIC  X(01)  VALUE " ".
         05  MSG-OUT4-FIL4   PIC  N(01)  VALUE NC"件".
*
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN             PIC X(01).
 01  LINK-IN-YMD6            PIC 9(06).
 01  LINK-IN-YMD8            PIC 9(08).
 01  LINK-OUT-RET            PIC X(01).
 01  LINK-OUT-YMD            PIC 9(08).
****************************************************************
 LINKAGE                     SECTION.
****************************************************************
 01  LINK-IN-BUMON               PIC  X(04).
 01  LINK-IN-TANCD               PIC  X(02).
****************************************************************
 PROCEDURE                   DIVISION  USING LINK-IN-BUMON
                                             LINK-IN-TANCD.
****************************************************************
 DECLARATIVES.
 WRK-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE SYOTANW1.
     DISPLAY     WRK-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     WRK-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     STOP        RUN.
 SOK-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE SOKTANL1.
     DISPLAY     SOK-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     SOK-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     STOP        RUN.
 END DECLARATIVES.
****************************************************************
*                 P R O G R A M - S E C
****************************************************************
 PROGRAM-SEC                 SECTION.
     PERFORM     INIT-SEC.
     PERFORM     MAIN-SEC    UNTIL     END-FLG  =  "END".
     PERFORM     END-SEC.
     STOP        RUN.
*PROGRAM-END.
****************************************************************
*                 I N I T - S E C
****************************************************************
 INIT-SEC                    SECTION.
     MOVE       "INIT-SEC"   TO   S-NAME.
*
     DISPLAY     MSG-START   UPON  CONS.
*
     OPEN        INPUT       SYOTANW1.
     OPEN        I-O         SOKTANL1.
*
*システム日付・時刻の取得
     ACCEPT   SYS-DATE          FROM   DATE.
     ACCEPT   SYS-TIME          FROM   TIME.
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
     MOVE      LINK-OUT-YMD       TO   WK-DATE8
                                       SYS-DATE8.
     DISPLAY "# DATE  = " WK-DATE8     UPON CONS.
     DISPLAY "# TIME  = " WK-TIME-HM   UPON CONS.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*                 M A I N - S E C
****************************************************************
 MAIN-SEC                    SECTION.
     MOVE     "MAIN-SEC"          TO   S-NAME.
*
 MAIN-100.
* 取込ファイル読込　終了するまで繰り返す。
     PERFORM  READ-WRK-SEC.
*
     IF       WRK-ENDFLG  =   SPACE
              PERFORM         HENSYU-WRK-SEC
              GO          TO  MAIN-100
     END-IF.
*
     MOVE    "END"        TO  END-FLG.
*
 MAIN-SEC-EXIT.
     EXIT.
****************************************************************
*  取込ファイル　順読込
****************************************************************
 READ-WRK-SEC                SECTION.
     MOVE     "READ-WRK-SEC"      TO   S-NAME.
*
     READ     SYOTANW1   AT   END
              MOVE      "Y"       TO   WRK-ENDFLG
              GO                  TO   READ-WRK-EXIT
     END-READ.
     IF       WRK-F20  =  "1"
              GO                  TO   READ-WRK-SEC
     END-IF.
*カウント（取込件数）
     ADD      1                   TO   WRK-RD-CNT.
*
 READ-WRK-EXIT.
     EXIT.
***************************************************************
*    作成更新・削除制御
***************************************************************
 HENSYU-WRK-SEC             SECTION.
*
     MOVE    "HENSYU-WRK-SEC"      TO   S-NAME.
*
*
*   倉庫コード
     MOVE  WRK-F112           TO   SOK-F01.
*
*   D365JANコード
     MOVE  WRK-F113           TO   SOK-F02.
*
     PERFORM  SOK-READ-SEC.
*
*作成更新・削除制御
     IF       WRK-F10  =  "1"
         IF   SOK-FLG  =  "INV"
              PERFORM  SOK-NEW-SEC
         ELSE
              PERFORM  SOK-UPD-SEC
         END-IF
     END-IF.
     IF       WRK-F10  =  "2"
         IF   SOK-FLG  =  "INV"
              CONTINUE
         ELSE
              PERFORM  SOK-DEL-SEC
         END-IF
     END-IF.
*
 HENSYU-WRK-EXIT.
     EXIT.
****************************************************************
*   倉庫商品別_番設定マスタ作成　　　　　　　　　　　　 *
****************************************************************
 SOK-NEW-SEC        SECTION.
*
     MOVE "SOK-NEW-SEC"            TO   S-NAME.
     INITIALIZE                         SOK-REC.
*
*   倉庫コード
     MOVE  WRK-F112          TO   SOK-F01.
*
*   D365JANコード
     MOVE  WRK-F113          TO   SOK-F02.
*
*   商品名１
     MOVE  WRK-F114          TO   SOK-F03.
*
*   商品名２
     MOVE  WRK-F115          TO   SOK-F04.
*
*   _番　
     MOVE  WRK-F116          TO   SOK-F05.
*
*   改定日
     MOVE  WRK-F117          TO   SOK-F06.
*
*   改定後_番
     MOVE  WRK-F118          TO   SOK-F07.
*
*   登録担当者
     MOVE  LINK-IN-TANCD     TO   SOK-F96.
*
*   登録日
     MOVE  WK-DATE8          TO   SOK-F97.
*
*   更新担当者
*    F98:INIT
*
*   更新日
*    F99:INIT
*
     WRITE      SOK-REC.
     ADD        1            TO   SOK-WT-CNT.
*
 SOK-NEW-EXIT.
     EXIT.
****************************************************************
*   倉庫商品別_番設定マスタ更新　　　　　　　　　　　　 *
****************************************************************
 SOK-UPD-SEC        SECTION.
*
     MOVE "SOK-UPD-SEC"            TO   S-NAME.
*    INITIALIZE                         SOK-REC.
*
*   倉庫コード
     MOVE  WRK-F112          TO   SOK-F01.
*
*   D365JANコード
     MOVE  WRK-F113          TO   SOK-F02.
*
*   商品名１
     MOVE  WRK-F114          TO   SOK-F03.
*
*   商品名２
     MOVE  WRK-F115          TO   SOK-F04.
*
*   _番　
     MOVE  WRK-F116          TO   SOK-F05.
*
*   改定日
     MOVE  WRK-F117          TO   SOK-F06.
*
*   改定後_番
     MOVE  WRK-F118          TO   SOK-F07.
*
*   登録担当者
*    SOK-F96 : INIT
*
*   登録日
*    SOK-F97 : INIT
*
*   更新担当者
     MOVE  LINK-IN-TANCD     TO   SOK-F98.
*
*   更新日
     MOVE  WK-DATE8          TO   SOK-F99.
*
     REWRITE    SOK-REC.
     ADD        1            TO   SOK-RT-CNT.
*
 SOK-UPD-EXIT.
     EXIT.
*
****************************************************************
*   倉庫商品別_番設定マスタ削除　　　　　　　　　　　　 *
****************************************************************
 SOK-DEL-SEC        SECTION.
*
     MOVE "SOK-DEL-SEC"            TO   S-NAME.
*
     DELETE     SOKTANL1.
     ADD        1            TO   SOK-DL-CNT.
*
 SOK-DEL-EXIT.
     EXIT.
*
****************************************************************
*    倉庫商品別_番設定マスタ検索
****************************************************************
 SOK-READ-SEC               SECTION.
     MOVE     "SOK-READ-SEC" TO   S-NAME.
*
     READ     SOKTANL1
       INVALID
              MOVE "INV"     TO   SOK-FLG
       NOT  INVALID
              MOVE SPACE     TO   SOK-FLG
     END-READ.
 SOK-READ-EXIT.
     EXIT.
****************************************************************
*    終了
****************************************************************
 END-SEC                   SECTION.
*
     MOVE     "END-SEC"     TO    S-NAME.
*
     MOVE      WRK-RD-CNT   TO    MSG-OUT01.
     MOVE      SOK-WT-CNT   TO    MSG-OUT02.
     MOVE      SOK-RT-CNT   TO    MSG-OUT03.
     MOVE      SOK-DL-CNT   TO    MSG-OUT04.
     DISPLAY   MSG-OUT1-FIL1 MSG-OUT1-FIL2 MSG-OUT01
               MSG-OUT1-FIL3 MSG-OUT1-FIL4 UPON CONS.
     DISPLAY   MSG-OUT2-FIL1 MSG-OUT2-FIL2 MSG-OUT02
               MSG-OUT2-FIL3 MSG-OUT2-FIL4 UPON CONS.
     DISPLAY   MSG-OUT3-FIL1 MSG-OUT3-FIL2 MSG-OUT03
               MSG-OUT3-FIL3 MSG-OUT3-FIL4 UPON CONS.
     DISPLAY   MSG-OUT4-FIL1 MSG-OUT4-FIL2 MSG-OUT04
               MSG-OUT4-FIL3 MSG-OUT4-FIL4 UPON CONS.
*
     CLOSE     SYOTANW1
               SOKTANL1.
*
     DISPLAY   MSG-END      UPON  CONS.
*
 END-EXIT.
     EXIT.

```
