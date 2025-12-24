# STB0020B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/STB0020B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　マスタ保守　　　　　　　　　　　　*
*    モジュール名　　　　：　商品変換テーブル更新　　　　　　　*
*    作成日／作成者　　　：　2017/12/08 INOUE                  *
*    処理内容　　　　　　：　取込チェックにてＯＫとなった　　　*
*    　　　　　　　　　　　　データより、商品変換テーブル　　　*
*                            を更新する。　　　　　　　　　　　*
*    流用元　　　　　　　：　SSY3932B                          *
*    変更日／変更者　　　：　                                  *
*    変更内容　　　　　　：　                                  *
****************************************************************
 IDENTIFICATION              DIVISION.
 PROGRAM-ID.                 STB0020B.
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
*商品変換テーブル取込ファイル
     SELECT      STBXXXW1    ASSIGN    TO       DA-01-VI-STBXXXW1
                             ORGANIZATION       INDEXED
                             ACCESS    MODE     SEQUENTIAL
                             RECORD    KEY      STB-F13
                                                STB-F14
                                                STB-F060
                             FILE      STATUS   STB-ST.
*商品変換テーブル
     SELECT      SHOTBL1     ASSIGN    TO       DA-01-VI-SHOTBL1
                             ORGANIZATION       INDEXED
                             ACCESS    MODE     DYNAMIC
                             RECORD    KEY      SHO-F01
                                                SHO-F02
                             FILE      STATUS   SHO-ST.
****************************************************************
 DATA                        DIVISION.
****************************************************************
 FILE                        SECTION.
*商品変換テーブル取込ファイル
 FD  STBXXXW1.
     COPY        STBXXXW1    OF        XFDLIB
     JOINING     STB         AS        PREFIX.
*商品変換テーブル
 FD  SHOTBL1.
     COPY        SHOTBL1     OF        XFDLIB
     JOINING     SHO         AS        PREFIX.
****************************************************************
 WORKING-STORAGE           SECTION.
****************************************************************
 01  ST-AREA.
     03  STB-ST              PIC  X(02)  VALUE  SPACE.
     03  SHO-ST              PIC  X(02)  VALUE  SPACE.
 01  WK-AREA.
     03  END-FLG             PIC  X(03)  VALUE  SPACE.
     03  SHO-FLG             PIC  X(03)  VALUE  SPACE.
     03  STB-ENDFLG          PIC  X(01)  VALUE  SPACE.
     03  STB-RD-CNT          PIC  9(07)  VALUE  ZERO.
     03  SHO-WT-CNT          PIC  9(07)  VALUE  ZERO.
     03  SHO-RT-CNT          PIC  9(07)  VALUE  ZERO.
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
     03  STB-ERR            PIC  N(10)  VALUE
                   NC"ＥＸＣＥＬ取込Ｆ異常".
     03  SHO-ERR             PIC  N(10)  VALUE
                   NC"商品変換テーブル異常".
*
*メッセージ出力領域
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER          PIC  X(05)  VALUE " *** ".
         05  ST-PG           PIC  X(08)  VALUE "STB0020B".
         05  FILLER          PIC  X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER          PIC  X(05)  VALUE " *** ".
         05  END-PG          PIC  X(08)  VALUE "STB0020B".
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
                             NC"変換テーブル作成＝".
         05  MSG-OUT02       PIC  ZZZ,ZZ9.
         05  MSG-OUT2-FIL3   PIC  X(01)  VALUE " ".
         05  MSG-OUT2-FIL4   PIC  N(01)  VALUE NC"件".
     03  MSG-OUT3.
         05  MSG-OUT3-FIL1   PIC  X(02)  VALUE "##".
         05  MSG-OUT3-FIL2   PIC  N(11)  VALUE
                             NC"変換テーブル更新＝".
         05  MSG-OUT03       PIC  ZZZ,ZZ9.
         05  MSG-OUT3-FIL3   PIC  X(01)  VALUE " ".
         05  MSG-OUT3-FIL4   PIC  N(01)  VALUE NC"件".
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
 STB-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE STBXXXW1.
     DISPLAY     STB-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     STB-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     STOP        RUN.
 SHO-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE SHOTBL1.
     DISPLAY     SHO-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     SHO-ST      UPON      CONS.
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
     OPEN        INPUT       STBXXXW1.
     OPEN        I-O         SHOTBL1.
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
* 変換テーブルＥＸＣＥＬ読込　終了するまで繰り返す。
     PERFORM  READ-STB-SEC.
*
     IF       STB-ENDFLG  =   SPACE
              PERFORM         HENSYU-STB-SEC
              GO          TO  MAIN-100
     END-IF.
*
     MOVE    "END"        TO  END-FLG.
*
 MAIN-SEC-EXIT.
     EXIT.
****************************************************************
*  変換テーブルＥＸＣＥＬ　順読込
****************************************************************
 READ-STB-SEC                SECTION.
     MOVE     "READ-STB-SEC"      TO   S-NAME.
*
     READ     STBXXXW1   AT   END
              MOVE      "Y"       TO   STB-ENDFLG
              GO                  TO   READ-STB-EXIT
     END-READ.
     IF       STB-F060  =  "1"
              GO                  TO   READ-STB-SEC
     END-IF.
*カウント（取込件数）
     ADD      1                   TO   STB-RD-CNT.
*
 READ-STB-EXIT.
     EXIT.
***************************************************************
*    変換テーブルＥＸＣＥＬ　作成・更新制御
***************************************************************
 HENSYU-STB-SEC             SECTION.
*
     MOVE    "HENSYU-STB-SEC"      TO   S-NAME.
*
*作成・更新制御
     IF       STB-F11  =  "1"
              PERFORM  SHO-NEW-SEC
     ELSE
              PERFORM  SHO-UPD-SEC
     END-IF.
*
 HENSYU-STB-EXIT.
     EXIT.
****************************************************************
*   商品変換テーブル作成　　　　　　　　　　　　　　　　 *
****************************************************************
 SHO-NEW-SEC        SECTION.
*
     MOVE "SHO-NEW-SEC"            TO   S-NAME.
     INITIALIZE                         SHO-REC.
*
*   取引先コード
     MOVE  STB-F13           TO   SHO-F01.
*
*   量販店商品コード
     MOVE  STB-F14           TO   SHO-F02.
*
*   商品コード
     MOVE  STB-F152          TO   SHO-F031.
*
*   品単１
     MOVE  STB-F162          TO   SHO-F0321.
*
*   品単２
     MOVE  STB-F172          TO   SHO-F0322.
*
*   品単３
     MOVE  STB-F182          TO   SHO-F0323.
*
*   出荷場所
     MOVE  STB-F192          TO   SHO-F04.
*
*   原価単価
     MOVE  STB-F202          TO   SHO-F05.
*
*   売価単価
     MOVE  STB-F212          TO   SHO-F06.
*
*   分類コード
     MOVE  STB-F222          TO   SHO-F07.
*
*   棚番
     MOVE  STB-F232          TO   SHO-F08.
*
*   仕入単価
     MOVE  STB-F242          TO   SHO-F09.
*
*   ﾗﾍﾞﾙ張替区分
*    F10:INIT
*
*   検品Ｇ
     MOVE  STB-F252          TO   SHO-F11.
*
*   出荷地域区分
*    F12:INIT
*
*   登録担当者
     MOVE  STB-F04           TO   SHO-F13.
*
*   最終更新担当者
     MOVE  STB-F04           TO   SHO-F14.
*
*   登録日
     MOVE  STB-F01           TO   SHO-F98.
*
*   更新日
*    F99:INIT
*
     WRITE      SHO-REC.
     ADD        1            TO   SHO-WT-CNT.
*
 SHO-NEW-EXIT.
     EXIT.
****************************************************************
*   商品変換テーブル更新　　　　　　　　　　　　　　　　 *
****************************************************************
 SHO-UPD-SEC        SECTION.
*
     MOVE "SHO-UPD-SEC"            TO   S-NAME.
     INITIALIZE                         SHO-REC.
*
*   取引先コード
     MOVE  STB-F13           TO   SHO-F01.
*
*   量販店商品コード
     MOVE  STB-F14           TO   SHO-F02.
*
*   変換テーブルＲＥＡＤ
*   　先行処理にてチェック済であるため
*   　当条件はヒットしないが
*   　ロジックとしては組み込んでおく
     PERFORM  SHO-READ-SEC.
     IF       SHO-FLG  =  "INV"
              DISPLAY NC"商品変換テーブル存在しません" UPON CONS
              DISPLAY NC"　取引先ＣＤ＝" STB-F13       UPON CONS
              DISPLAY NC"　量販店商品＝" STB-F14       UPON CONS
              MOVE    4000        TO        PROGRAM-STATUS
              STOP    RUN
     END-IF.
*
*   商品コード
     IF    STB-F151  =  "Y"
           MOVE  STB-F152    TO   SHO-F031
     END-IF.
*
*   品単１
     IF    STB-F161  =  "Y"
           MOVE  STB-F162    TO   SHO-F0321
     END-IF.
*
*   品単２
     IF    STB-F171  =  "Y"
           MOVE  STB-F172    TO   SHO-F0322
     END-IF.
*   品単３
     IF    STB-F181  =  "Y"
           MOVE  STB-F182    TO   SHO-F0323
     END-IF.
*
*   出荷場所
     IF    STB-F191  =  "Y"
           MOVE  STB-F192    TO   SHO-F04
     END-IF.
*
*   原価単価
     IF    STB-F201  =  "Y"
           MOVE  STB-F202    TO   SHO-F05
     END-IF.
*
*   売価単価
     IF    STB-F211  =  "Y"
           MOVE  STB-F212    TO   SHO-F06
     END-IF.
*
*   分類コード
     IF    STB-F221  =  "Y"
           MOVE  STB-F222    TO   SHO-F07
     END-IF.
*
*   棚番
     IF    STB-F231  =  "Y"
           MOVE  STB-F232    TO   SHO-F08
     END-IF.
*
*   仕入単価
     IF    STB-F241  =  "Y"
           MOVE  STB-F242    TO   SHO-F09
     END-IF.
*
*   ﾗﾍﾞﾙ張替区分
*    F10:INIT
*
*   検品Ｇ
     IF    STB-F251  =  "Y"
           MOVE  STB-F252    TO   SHO-F11
     END-IF.
*
*   出荷地域区分
*    F12:INIT
*
*   登録担当者
*    F13:-
*
*   最終更新担当者
     MOVE  STB-F04           TO   SHO-F14.
*
*   登録日
*    F98:-
*
*   更新日
     MOVE  STB-F01           TO   SHO-F99.
*
     REWRITE    SHO-REC.
     ADD        1            TO   SHO-RT-CNT.
*
 SHO-UPD-EXIT.
     EXIT.
*
****************************************************************
*    商品変換ＴＢＬ検索
****************************************************************
 SHO-READ-SEC               SECTION.
     MOVE     "SHO-READ-SEC" TO   S-NAME.
*
     READ     SHOTBL1
       INVALID
              MOVE "INV"     TO   SHO-FLG
       NOT  INVALID
              MOVE SPACE     TO   SHO-FLG
     END-READ.
 SHO-READ-EXIT.
     EXIT.
****************************************************************
*    終了
****************************************************************
 END-SEC                   SECTION.
*
     MOVE     "END-SEC"     TO    S-NAME.
*
     MOVE      STB-RD-CNT   TO    MSG-OUT01.
     MOVE      SHO-WT-CNT   TO    MSG-OUT02.
     MOVE      SHO-RT-CNT   TO    MSG-OUT03.
     DISPLAY   MSG-OUT1-FIL1 MSG-OUT1-FIL2 MSG-OUT01
               MSG-OUT1-FIL3 MSG-OUT1-FIL4 UPON CONS.
     DISPLAY   MSG-OUT2-FIL1 MSG-OUT2-FIL2 MSG-OUT02
               MSG-OUT2-FIL3 MSG-OUT2-FIL4 UPON CONS.
     DISPLAY   MSG-OUT3-FIL1 MSG-OUT3-FIL2 MSG-OUT03
               MSG-OUT3-FIL3 MSG-OUT3-FIL4 UPON CONS.
*
     CLOSE     STBXXXW1
               SHOTBL1.
*
*ＯＵＴパラメタセット
* 取込件数
*    MOVE      STB-RD-CNT      TO    LINK-OUT-CNT1.
* 登録件数
*    MOVE      SHO-WT-CNT      TO    LINK-OUT-CNT2.
* エラー件数
     DISPLAY   MSG-END      UPON  CONS.
*
 END-EXIT.
     EXIT.

```
