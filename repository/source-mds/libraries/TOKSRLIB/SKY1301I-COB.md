# SKY1301I

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/SKY1301I.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　在庫管理システム　　　　　　　　　*
*    モジュール名　　　　：　タックシール入力　　　　　　　　　*
*    作成日／作成者　　　：　99/10/19  /HAGIWARA               *
*    更新日／更新者　　　：　                                  *
*    処理概要　　　　　　：　　　　　　　　　　　　　　　　　  *
*                            　　　　　　　　　　　　　　　　　*
****************************************************************
 IDENTIFICATION         DIVISION.
 PROGRAM-ID.            SKY1301I.
 AUTHOR.                HAGIWARA.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITU.
 OBJECT-COMPUTER.       FUJITU.
 SPECIAL-NAMES.
         STATION   IS   STAT
         CONSOLE   IS   CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*-----<<  取引先マスタ  >>-----*
     SELECT   HTOKMS    ASSIGN    TO        TOKMS2
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   TOK-F01
                        FILE      STATUS    IS   TOK-STS.
*-----<<  店舗マスタ  >>-----*
     SELECT   HTENMS    ASSIGN    TO        TENMS1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   DYNAMIC
                        RECORD    KEY       IS   TEN-F52
                                                 TEN-F011
                        FILE      STATUS    IS   TEN-STS.
*-----<<  送り状ファイル  >>-----*
     SELECT   ZOKURIF   ASSIGN    TO        ZOKURIF
                        ORGANIZATION        IS   SEQUENTIAL
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   OKU-STS.
*-----<<  画面ファイル  >>-----*
     SELECT   DSPF      ASSIGN    TO        GS-DSPF
                        ORGANIZATION        IS   SEQUENTIAL
                        ACCESS    MODE      IS   SEQUENTIAL
                        SYMBOLIC  DESTINATION    IS  "DSP"
                        PROCESSING MODE     IS   DSP-PROC
                        GROUP               IS   DSP-GROUP
                        FORMAT              IS   DSP-FORMAT
                        SELECTED  FUNCTION  IS   DSP-FUNC
                        FILE      STATUS    IS   DSP-STATUS.
****************************************************************
 DATA                   DIVISION.
 FILE                   SECTION.
*-----<<  取引先マスタ  >>-----*
 FD  HTOKMS.
     COPY     HTOKMS    OF   XFDLIB  JOINING   TOK  PREFIX.
*-----<<  店舗マスタ  >>-----*
 FD  HTENMS.
     COPY     HTENMS    OF   XFDLIB  JOINING   TEN  PREFIX.
*-----<<  送り状ファイル  >>-----*
 FD  ZOKURIF.
     COPY     ZOKURIF   OF   XFDLIB  JOINING   OKU  PREFIX.
*-----<<  画面ファイル  >>-----*
 FD  DSPF.
 01  DSP-AREA                PIC  X(2000).
****************************************************************
*   ＷＯＲＫＩＮＧ－ＳＴＯＲＡＧＥ　　　ＳＥＣＴＩＯＮ
****************************************************************
 WORKING-STORAGE             SECTION.
****  画面ファイル  ****
 COPY   FKY13013         OF        XMDLIB.
 COPY   FKY13014         OF        XMDLIB.
****  画面制御項目  ****
 01  DSP-CONTROL.
     03  DSP-PROC            PIC  X(02).
     03  DSP-GROUP           PIC  X(08).
     03  DSP-FORMAT          PIC  X(08).
     03  DSP-STATUS          PIC  X(02).
     03  DSP-FUNC            PIC  X(04).
****  ステイタス情報  ***
 01  STATUS-AREA.
     03  TOK-STS             PIC  X(02).
     03  TEN-STS             PIC  X(02).
     03  OKU-STS             PIC  X(02).
****  フラグ  ***
 01  PSW-AREA.
     03  END-FLG             PIC  X(03)  VALUE SPACE.
     03  SYORI-FLG           PIC  9(01)  VALUE ZERO.
     03  INV-FLG             PIC  9(01)  VALUE ZERO.
     03  TEN-FLG             PIC  9(01)  VALUE ZERO.
     03  ERR-FLG             PIC  9(02)  VALUE ZERO.
     03  M-FLG               PIC  9(01)  VALUE ZERO.
     03  K-FLG               PIC  9(01)  VALUE ZERO.
***  明細エラーチェック用
     03  BD1CHK-FLG          PIC  9(01)  VALUE ZERO.
***  前ページ存在チェック用
     03  MAEP1-FLG           PIC  9(01)  VALUE ZERO.
***  次ページ存在チェック用
     03  ATOP1-FLG           PIC  9(01)  VALUE ZERO.
****  インデックス            ****
 01  WK-AREA1.
     03  IX                  PIC  9(04)  VALUE ZERO.
     03  IY                  PIC  9(04)  VALUE ZERO.
***  次ページ存在判定用
     03  IZ                  PIC  9(04)  VALUE ZERO.
***
     03  WK-MAX              PIC  9(04)  VALUE ZERO.
     03  WK-TOKCD            PIC  9(08)  VALUE ZERO.
*日付／時刻
 01  TIME-AREA.
     03  WK-TIME             PIC  9(08)  VALUE  ZERO.
 01  DATE-AREA.
     03  WK-YS               PIC  9(02)  VALUE  ZERO.
     03  WK-DATE.
         05  WK-Y            PIC  9(02)  VALUE  ZERO.
         05  WK-M            PIC  9(02)  VALUE  ZERO.
         05  WK-D            PIC  9(02)  VALUE  ZERO.
 01  DATE-AREAR2       REDEFINES      DATE-AREA.
     03  SYS-DATE            PIC  9(08).
*画面表示日付編集
 01  HEN-DATE.
     03  HEN-DATE-YYYY       PIC  9(04)  VALUE  ZERO.
     03  FILLER              PIC  X(01)  VALUE  "/".
     03  HEN-DATE-MM         PIC  9(02)  VALUE  ZERO.
     03  FILLER              PIC  X(01)  VALUE  "/".
     03  HEN-DATE-DD         PIC  9(02)  VALUE  ZERO.
*画面表示時刻編集
 01  HEN-TIME.
     03  HEN-TIME-HH         PIC  9(02)  VALUE  ZERO.
     03  FILLER              PIC  X(01)  VALUE  ":".
     03  HEN-TIME-MM         PIC  9(02)  VALUE  ZERO.
     03  FILLER              PIC  X(01)  VALUE  ":".
     03  HEN-TIME-SS         PIC  9(02)  VALUE  ZERO.
****  退避領域                ****
 01  WK-OKU.
     03  WK-OKUTBL           OCCURS   1290.
         05  WK-TEN          PIC  9(05)  VALUE ZERO.
         05  WK-YU1          PIC  X(03)  VALUE SPACE.
         05  WK-YU2          PIC  X(04)  VALUE SPACE.
         05  WK-NM1          PIC  N(15)  VALUE SPACE.
         05  WK-NM2          PIC  N(15)  VALUE SPACE.
         05  WK-JYU1         PIC  N(15)  VALUE SPACE.
         05  WK-JYU2         PIC  N(15)  VALUE SPACE.
         05  WK-TEL          PIC  X(12)  VALUE SPACE.
         05  WK-MAI          PIC  9(04)  VALUE ZERO.
         05  WK-KEISHO       PIC  9(01)  VALUE ZERO.
****  ＰＦキーガイド  ***
 01  MSG-AREA.
     03  PMSG01            PIC N(20) VALUE
                           NC"_終了".
     03  PMSG02            PIC N(20) VALUE
                           NC"_取消　_終了　_項目戻し".
     03  PMSG03            PIC N(25) VALUE
           NC"_取消　_終了　_項目戻し　_前頁　_次頁".
     03  PMSG04            PIC N(20) VALUE
                           NC"_取消　_終了".
****  メッセージ情報  ***
 01  MSG-AREA1-1.
     02  MSG-ABEND1.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-PG-ID         PIC  X(08)  VALUE  "SKY1301I".
       03  FILLER            PIC  X(10)  VALUE  " ABEND ###".
     02  MSG-ABEND2.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-FL-ID         PIC  X(08).
       03  FILLER            PIC  X(04)  VALUE  " ST-".
       03  ERR-STCD          PIC  X(02).
       03  FILLER            PIC  X(04)  VALUE  " ###".
****  エラーメッセージ  ***
 01  ERR-TAB.
     03  MSG1                PIC  N(28)  VALUE
            NC"無効ＰＦキーです。".
     03  MSG2                PIC  N(28)  VALUE
            NC"１，２以外はエラーです。".
     03  MSG3                PIC  N(28)  VALUE
            NC"１，２，３以外はエラーです。".
     03  MSG4                PIC  N(28)  VALUE
            NC"０，１以外はエラーです。".
     03  MSG5                PIC  N(28)  VALUE
            NC"取引先コードが未入力です。".
     03  MSG6                PIC  N(28)  VALUE
            NC"取引先マスタに未登録です。".
     03  MSG7                PIC  N(28)  VALUE
            NC"個数が未入力です。".
     03  MSG8                PIC  N(28)  VALUE
            NC"印刷枚数が未入力です。".
     03  MSG9                PIC  N(28) VALUE
            NC"前頁はありません。".
     03  MSG10               PIC  N(28) VALUE
            NC"次頁はありません。".
     03  MSG11               PIC  N(28)  VALUE
            NC"Ｙで入力して下さい".
     03  MSG12               PIC  N(28)  VALUE
            NC"店舗マスタに未登録です。".
     03  MSG13               PIC  N(28)  VALUE
            NC"１，２，９以外はエラーです。".
 01  FILLER                  REDEFINES    ERR-TAB.
     03  MSG-TBL             PIC  N(28)  OCCURS   13.
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN             PIC X(01).
 01  LINK-IN-YMD6            PIC 9(06).
 01  LINK-IN-YMD8            PIC 9(08).
 01  LINK-OUT-RET            PIC X(01).
 01  LINK-OUT-YMD            PIC 9(08).
*----------------------------------------------------------*
*             ＭＡＩＮ         ＭＯＤＵＬＥ                *
*----------------------------------------------------------*
 PROCEDURE              DIVISION.
**
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   DSPF.
     MOVE     "DSPF    "       TO   ERR-FL-ID.
     MOVE      DSP-STATUS      TO   ERR-STCD.
     DISPLAY   MSG-ABEND1    UPON   CONS.
     DISPLAY   MSG-ABEND2    UPON   CONS.
     MOVE      4000            TO   PROGRAM-STATUS.
     STOP      RUN.
**
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   HTOKMS.
     MOVE     "HTOKMS "        TO   ERR-FL-ID.
     MOVE      TOK-STS         TO   ERR-STCD.
     DISPLAY   MSG-ABEND1    UPON   CONS.
     DISPLAY   MSG-ABEND2    UPON   CONS.
     MOVE      4000            TO   PROGRAM-STATUS.
     STOP      RUN.
**
 FILEERR-SEC3           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   HTENMS.
     MOVE     "HTENMS "        TO   ERR-FL-ID.
     MOVE      TEN-STS         TO   ERR-STCD.
     DISPLAY   MSG-ABEND1    UPON   CONS.
     DISPLAY   MSG-ABEND2    UPON   CONS.
     MOVE      4000            TO   PROGRAM-STATUS.
     STOP      RUN.
**
 FILEERR-SEC4           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   ZOKURIF.
     MOVE     "ZOKURIF"        TO   ERR-FL-ID.
     MOVE      OKU-STS         TO   ERR-STCD.
     DISPLAY   MSG-ABEND1    UPON   CONS.
     DISPLAY   MSG-ABEND2    UPON   CONS.
     MOVE      4000            TO   PROGRAM-STATUS.
     STOP      RUN.
 END     DECLARATIVES.
************************************************************
*      _０     メインモジュール                           *
************************************************************
 SKY1301I-START         SECTION.
     PERFORM      INIT-SEC.
     PERFORM      MAIN-SEC
                  UNTIL     END-FLG  =    "END".
     PERFORM      END-SEC.
     STOP      RUN.
 SKY1301I-END.
     EXIT.
************************************************************
*      _０     初期処理                                   *
************************************************************
 INIT-SEC               SECTION.
     OPEN     INPUT     HTOKMS    HTENMS.
     OPEN     I-O       DSPF.
     OPEN     OUTPUT    ZOKURIF.
*システム日付／時刻取得
*****PERFORM  GET-DATE-SEC.
*
     MOVE    1               TO   SYORI-FLG.
 INIT-END.
     EXIT.
************************************************************
*      _０      メイン処理                                *
************************************************************
 MAIN-SEC          SECTION.
     EVALUATE      SYORI-FLG
         WHEN      1    PERFORM   INIT1-DSP-SEC
         WHEN      2    PERFORM   SYORI-SEC
         WHEN      3    PERFORM   HEAD1-SEC
         WHEN      4    PERFORM   BODY1-SEC
         WHEN      5    PERFORM   KAKNIN1-SEC
         WHEN      6    PERFORM   INIT2-DSP-SEC
         WHEN      7    PERFORM   HEAD2-SEC
         WHEN      8    PERFORM   BODY2-SEC
         WHEN      9    PERFORM   KAKNIN2-SEC
     END-EVALUATE.
 MAIN-END.
     EXIT.
************************************************************
*      3.0        終了処理                                 *
************************************************************
 END-SEC                SECTION.
     CLOSE    HTOKMS    HTENMS    ZOKURIF   DSPF.
 END-END.
     EXIT.
*----------------------------------------------------------*
*      2.1       初期画面処理                              *
*----------------------------------------------------------*
 INIT1-DSP-SEC        SECTION.
*
     MOVE     SPACE          TO   FKY13013.
     MOVE     SPACE          TO   FKY13014.
     MOVE    "FKY13013"      TO   DSP-FORMAT.
     MOVE     SPACE          TO   DSP-PROC.
*システム日付／時刻取得
     PERFORM  GET-DATE-SEC.
     MOVE     HEN-DATE       TO   SDATE1.
     MOVE     HEN-TIME       TO   STIME1.
*
     MOVE     2              TO   SYORI-FLG.
 INIT1-DSP-EXIT.
     EXIT.
*----------------------------------------------------------*
*      2.2       処理区分入力                              *
*----------------------------------------------------------*
 SYORI-SEC            SECTION.
     PERFORM        DSP-SHOKIKA1-SEC.
     MOVE   " "     TO   EDIT-CURSOR  OF  TORCD1
     MOVE   "M"     TO   EDIT-OPTION  OF  TORCD1
*
     MOVE     PMSG01         TO   PFGID1.
     PERFORM         DSP-WRITE-SEC1.
*
     MOVE    "SYOKBN" TO   DSP-GROUP.
     PERFORM         DSP-READ-SEC.
* アテンション判定
     EVALUATE    DSP-FUNC
         WHEN   "F005"
                        MOVE   "END"       TO   END-FLG
         WHEN   "E000"
                        PERFORM   SYORICHK-SEC
         WHEN    OTHER
                        MOVE   1       TO   ERR-FLG
     END-EVALUATE.
 SYORI-EXIT.
     EXIT.
*----------------------------------------------------------*
*      2.2.1     処理区分の入力チェック                    *
*----------------------------------------------------------*
 SYORICHK-SEC             SECTION.
     MOVE   " "     TO   EDIT-CURSOR  OF  SHORI.
     MOVE   "M"     TO   EDIT-OPTION  OF  SHORI.
*処理区分チェック
     IF  ( SHORI  NOT  NUMERIC   )
         MOVE   2         TO   ERR-FLG
         MOVE   "R"     TO   EDIT-OPTION  OF  SHORI
         MOVE   "C"     TO   EDIT-CURSOR  OF  SHORI
     ELSE
         IF  ( SHORI   =  1 OR 2  )
              IF   SHORI   =  1
                   MOVE     3              TO   SYORI-FLG
              ELSE
                   MOVE     6              TO   SYORI-FLG
              END-IF
         ELSE
              MOVE   2         TO   ERR-FLG
              MOVE   "R"     TO   EDIT-OPTION  OF  SHORI
              MOVE   "C"     TO   EDIT-CURSOR  OF  SHORI
         END-IF
     END-IF.
 SYORICHK-EXIT.
     EXIT.
*----------------------------------------------------------*
*      2.3       ＨＥＡＤ１処理 （複数）　　               *
*----------------------------------------------------------*
 HEAD1-SEC            SECTION.
**** PERFORM         DSP-SHOKIKA1-SEC.
     PERFORM   VARYING   IX   FROM  1  BY  1   UNTIL  IX  >   15
         MOVE   " "     TO   EDIT-CURSOR  OF  KEISH1(IX)
         MOVE   "M"     TO   EDIT-OPTION  OF  KEISH1(IX)
     END-PERFORM.
     MOVE     PMSG02         TO   PFGID1.
     PERFORM         DSP-WRITE-SEC1.
*
     MOVE    "HEAD1" TO   DSP-GROUP.
     PERFORM         DSP-READ-SEC.
* アテンション判定
     EVALUATE    DSP-FUNC
         WHEN   "F004"
                        MOVE   1           TO   SYORI-FLG
         WHEN   "F005"
                        MOVE   "END"       TO   END-FLG
         WHEN   "F006"
                        MOVE   2           TO   SYORI-FLG
         WHEN   "E000"
                        PERFORM   HEAD1-CHK-SEC
         WHEN    OTHER
                        MOVE   1         TO   ERR-FLG
     END-EVALUATE.
 HEAD1-EXIT.
     EXIT.
*----------------------------------------------------------*
*      2.3.1     ＨＥＡＤ部の入力チェック （複数）         *
*----------------------------------------------------------*
 HEAD1-CHK-SEC             SECTION.
*伝票種類チェック
     MOVE   "M"     TO   EDIT-OPTION  OF  TORCD1.
     MOVE   " "     TO   EDIT-CURSOR  OF  TORCD1.
*取引先コードチェック
     IF  ( TORCD1  NOT  NUMERIC   )
         IF    ERR-FLG    =  ZERO
               MOVE   5       TO   ERR-FLG
         END-IF
         MOVE   SPACE     TO   TORNM
         MOVE   "R"     TO   EDIT-OPTION  OF  TORCD1
         MOVE   "C"     TO   EDIT-CURSOR  OF  TORCD1
         GO             TO   HEAD1-CHK-EXIT
     END-IF.
     MOVE   TORCD1        TO    TOK-F01.
     MOVE   ZERO          TO    INV-FLG.
     PERFORM    TOK-READ-SEC.
     IF  INV-FLG   NOT   =    ZERO
         MOVE   SPACE        TO   TORNM
         IF    ERR-FLG    =  ZERO
               MOVE   6       TO   ERR-FLG
         END-IF
         MOVE   "R"     TO   EDIT-OPTION  OF  TORCD1
         MOVE   "C"     TO   EDIT-CURSOR  OF  TORCD1
         GO             TO   HEAD1-CHK-EXIT
     ELSE
         MOVE   TOK-F02      TO   TORNM
     END-IF.
*店舗マスタＳＴＡＲＴリード
     PERFORM   TEN-START-SEC.
     IF  TEN-FLG   =  1
         IF    ERR-FLG    =  ZERO
               MOVE   12      TO   ERR-FLG
         END-IF
         MOVE   "R"     TO   EDIT-OPTION  OF  TORCD1
         MOVE   "C"     TO   EDIT-CURSOR  OF  TORCD1
     END-IF.
*ＷＫテーブルクリア
     MOVE    SPACE      TO  WK-OKU.
     INITIALIZE         WK-OKU.
*店舗マスタＴＢＬ（ＷＫ）へＳＥＴ
     PERFORM   VARYING   IX   FROM  1  BY  1   UNTIL
                          (   TEN-FLG  =  1    )
                      OR  (   IX       >  1289 )
               MOVE   TEN-F011      TO    WK-TEN(IX)
               MOVE   TEN-F781      TO    WK-YU1(IX)
               MOVE   TEN-F782      TO    WK-YU2(IX)
               MOVE   TEN-F02       TO    WK-NM1(IX)
               MOVE   TEN-F06       TO    WK-JYU1(IX)
               MOVE   TEN-F07       TO    WK-JYU2(IX)
               MOVE   TEN-F08       TO    WK-TEL(IX)
               PERFORM   TEN-READ1-SEC
     END-PERFORM.

*退避数
     MOVE     IX             TO   WK-MAX.
     MOVE     SPACE          TO   MAS002.
     INITIALIZE                   MAS002.
*ＷＫから表示ファイルへＳＥＴ
     PERFORM   VARYING   IX   FROM  1  BY  1   UNTIL  IX  >   15
                                       OR  IX   >=   WK-MAX
               MOVE   IX            TO    SEQNO(IX)
***************DISPLAY    WK-TEN(IX)    UPON CONS
               MOVE   WK-TEN(IX)    TO    TENCD1(IX)
               MOVE   WK-NM1(IX)    TO    TENNM(IX)
     END-PERFORM.
*
     IF    ERR-FLG    =    ZERO
           MOVE       4             TO   SYORI-FLG
***********PERFORM    DSP-SHOKIKA1-SEC
     END-IF.
 HEAD1-CHK-EXIT.
     EXIT.
*----------------------------------------------------------*
*      2.3.1.1    店舗Ｍ　ＳＴＡＲＴ　処理                 *
*----------------------------------------------------------*
 TEN-START-SEC             SECTION.
     MOVE   TORCD1        TO    TEN-F52   WK-TOKCD.
*****DISPLAY TORCD1 UPON CONS.
     MOVE   ZERO          TO    TEN-F011.
*
     START  HTENMS    KEY  IS  >=  TEN-F52  TEN-F011
            INVALID
               MOVE   1       TO   TEN-FLG
********       DISPLAY "INVALID " UPON CONS
       NOT  INVALID
********       DISPLAY "N INVALID " UPON CONS
               PERFORM   TEN-READ1-SEC
     END-START.
 TEN-START-EXIT.
     EXIT.
*----------------------------------------------------------*
*      2.4       ＢＯＤＹ１処理 （複数）　　               *
*----------------------------------------------------------*
 BODY1-SEC            SECTION.
     MOVE     PMSG03         TO   PFGID1.
     PERFORM         DSP-WRITE-SEC1.
*
     MOVE    "BODY" TO   DSP-GROUP.
     PERFORM         DSP-READ-SEC.
* アテンション判定
     EVALUATE    DSP-FUNC
         WHEN   "F004"
                        MOVE   1           TO   SYORI-FLG
                        MOVE   ZERO        TO   IY
         WHEN   "F005"
                        MOVE   "END"       TO   END-FLG
         WHEN   "F006"
                        MOVE   3           TO   SYORI-FLG
                        MOVE   ZERO        TO   IY
*前ページ
         WHEN   "F011"
***                   前ページ存在チェック
                        PERFORM   MAEP1-SEC
***                     前ページが有れば
                        IF   MAEP1-FLG = 0
                             PERFORM       BODY1-CHK-SEC
***                          明細にエラーが有れば
                             IF   BD1CHK-FLG  = 1
                                  GO   TO       BODY1-EXIT
                             END-IF
                        ELSE
                             GO   TO   BODY1-EXIT
                        END-IF
***         前ページ有，明細エラー無時，画面⇒ＷＫへセット
                        PERFORM   BODY1-WKSET-SEC
***         前ページ明細を，ＷＫ⇒画面へセット
                        PERFORM   MAEP2-SEC
*次ページ
         WHEN   "F012"
                        PERFORM   ATOP1-SEC
***                     次ページが有れば
                        IF   ATOP1-FLG = 0
                             PERFORM       BODY1-CHK-SEC
***                          明細にエラーが有れば
                             IF   BD1CHK-FLG  = 1
                                  GO   TO       BODY1-EXIT
                             END-IF
                        ELSE
                             GO   TO   BODY1-EXIT
                        END-IF
***         次ページ有，明細エラー無時，画面⇒ＷＫへセット
                        PERFORM   BODY1-WKSET-SEC
***         次ページ明細を，ＷＫ⇒画面へセット
                        PERFORM   ATOP2-SEC
         WHEN   "E000"
***                     明細チェック
                        PERFORM   BODY1-CHK-SEC
                        IF   BD1CHK-FLG  = 1
                             GO   TO       BODY1-EXIT
                        END-IF
*
                        PERFORM   BODY1-WKSET-SEC
                        MOVE      5       TO   SYORI-FLG
         WHEN    OTHER
                        MOVE   1         TO   ERR-FLG
     END-EVALUATE.
 BODY1-EXIT.
     EXIT.
*----------------------------------------------------------*
*      2.4.1      ボディー部の入力チェック（複数）         *
*----------------------------------------------------------*
 BODY1-CHK-SEC             SECTION.
*
     MOVE       ZERO    TO   BD1CHK-FLG.
*
     PERFORM   VARYING   IX   FROM  1  BY  1   UNTIL  IX  >   15
                               OR  SEQNO(IX)   NOT  NUMERIC
                               OR  BD1CHK-FLG  = 1
***  敬称区分チェック
***    枚数入力済み時，敬称区分が未入力ならば敬称区分に２を入力
         IF   MAISU1(IX)  IS NUMERIC
          AND MAISU1(IX)  NOT =  ZERO
              IF   KEISH1(IX)  =  ZERO
                   MOVE   2       TO   KEISH1(IX)
              END-IF
***    枚数入力時のみ，敬称区分存在チェック
************* DISPLAY "KEISH1 = " KEISH1(IX) UPON CONS
              IF   KEISH1(IX)   =  1 OR 2 OR 9
                   MOVE   "M"     TO   EDIT-OPTION  OF  KEISH1(IX)
                   MOVE   " "     TO   EDIT-CURSOR  OF  KEISH1(IX)
              ELSE
                   IF    ERR-FLG    =  ZERO
                         MOVE   13     TO   ERR-FLG
                   END-IF
                   MOVE    1      TO   BD1CHK-FLG
                   MOVE   "R"     TO   EDIT-OPTION  OF  KEISH1(IX)
                   MOVE   "C"     TO   EDIT-CURSOR  OF  KEISH1(IX)
              END-IF
         END-IF
     END-PERFORM.
*
 BODY1-CHK-EXIT.
     EXIT.
*----------------------------------------------------------*
*      2.4.2      ＷＫへ転送　    （複数）                 *
*----------------------------------------------------------*
 BODY1-WKSET-SEC           SECTION.
     MOVE      SEQNO(1)       TO   IY.
     PERFORM   VARYING  IX   FROM  1  BY  1  UNTIL  IX  >  15
                               OR  SEQNO(IX)   NOT  NUMERIC
*印刷枚数の未入力チェック
               IF   MAISU1(IX)   NOT  NUMERIC
                    MOVE    ZERO      TO   MAISU1(IX)
               END-IF
*敬称区分の未入力チェック
               IF   KEISH1(IX)   NOT  NUMERIC
                    MOVE    ZERO      TO   KEISH1(IX)
               END-IF
*印刷枚数をＷＫへ転送
               MOVE    MAISU1(IX)      TO   WK-MAI(IY)
*敬称区分をＷＫへ転送
               MOVE    KEISH1(IX)      TO   WK-KEISHO(IY)
*
               ADD     1               TO   IY
     END-PERFORM.
 BODY1-WKSET-EXIT.
     EXIT.
*----------------------------------------------------------*
*      2.5       確認入力１　  （複数）                    *
*----------------------------------------------------------*
 KAKNIN1-SEC       SECTION.
     MOVE     "Y"            TO   CHECK1.
     MOVE     PMSG03         TO   PFGID1.
     PERFORM       DSP-WRITE-SEC1.
*
     MOVE    "KAKN1"         TO    DSP-GROUP.
     PERFORM       DSP-READ-SEC.
* アテンション判定
     EVALUATE  DSP-FUNC
         WHEN   "F004"
                        MOVE    1      TO   SYORI-FLG
                        MOVE    ZERO   TO   IY
         WHEN   "F005"
                        MOVE  "END"    TO   END-FLG
         WHEN   "F006"
                        MOVE    4      TO   SYORI-FLG
                        MOVE    SPACE  TO   CHECK1
         WHEN   "F011"
***                     前ページ存在チェック
                        PERFORM   MAEP1-SEC
***                     前ページが存在すればＷＫ⇒画面
                        IF      MAEP1-FLG = 0
                                PERFORM   MAEP2-SEC
                        END-IF
                        MOVE    SPACE  TO   CHECK1
         WHEN   "F012"
***                     次ページ存在チェック
                        PERFORM   ATOP1-SEC
***                     次ページが存在すればＷＫ⇒画面
                        IF      ATOP1-FLG = 0
                                PERFORM   ATOP2-SEC
                        END-IF
                        MOVE    SPACE  TO   CHECK1
         WHEN   "E000"
                        IF  CHECK1  NOT  =  "Y"
                            MOVE    11      TO  ERR-FLG
                        ELSE
                            PERFORM  OKU1-WTR-SEC
                        END-IF
         WHEN    OTHER
                        MOVE    1      TO   ERR-FLG
     END-EVALUATE.
 KAKNIN1-EXIT.
     EXIT.
*----------------------------------------------------------*
*      2.5.1.1    前ページ処理 -前Ｐ存在チェック-（複数）  *
*----------------------------------------------------------*
 MAEP1-SEC                  SECTION.
*
*フラグの初期化
     MOVE          ZERO      TO   MAEP1-FLG.
*前ページ存在チェック
     IF   SEQNO(1)    =   1
          IF    ERR-FLG    =  ZERO
                MOVE   9     TO   ERR-FLG
                MOVE   1     TO   MAEP1-FLG
          END-IF
                GO     TO    MAEP1-EXIT
     END-IF.
*前ページデータ，ＷＫ⇒画面処理
*
 MAEP1-EXIT.
     EXIT.
*----------------------------------------------------------*
*      2.5.1.2    前ページ処理 -前Ｐ，ＷＫ⇒画面（複数）   *
*----------------------------------------------------------*
 MAEP2-SEC                  SECTION.
*ＷＫから画面へ
     COMPUTE   IY    =    SEQNO(1)  -    15.
     PERFORM   WD-MOVE-SEC.
     PERFORM   VARYING  IX   FROM  1  BY  1  UNTIL  IX  >  15
                MOVE    " "     TO   EDIT-STATUS  OF  MAISU1(IX)
     END-PERFORM.
 MAEP2-EXIT.
     EXIT.
*----------------------------------------------------------*
*      2.5.1.1    ＷＫから画面へ（複数）　　               *
*----------------------------------------------------------*
 WD-MOVE-SEC               SECTION.
     MOVE     SPACE          TO   MAS002.
     PERFORM   DSP-SHOKIKA1-SEC.
     PERFORM   VARYING  IX   FROM  1  BY  1  UNTIL  IX  >  15
                               OR  IY   >=  WK-MAX
               MOVE    IY              TO   SEQNO(IX)
               MOVE    WK-TEN(IY)      TO   TENCD1(IX)
               MOVE    WK-NM1(IY)      TO   TENNM(IX)
               MOVE    WK-MAI(IY)      TO   MAISU1(IX)
               MOVE    WK-KEISHO(IY)   TO   KEISH1(IX)
               ADD     1               TO   IY
     END-PERFORM.
*
     MOVE     4              TO   SYORI-FLG.
 WD-MOVE-EXIT.
     EXIT.
*----------------------------------------------------------*
*      2.5.2.1    後ページ処理 -後Ｐ存在チェック- （複数） *
*----------------------------------------------------------*
 ATOP1-SEC                 SECTION.
*
*フラグ初期化
     MOVE          ZERO      TO   ATOP1-FLG.
*次ページ存在判定用変数IZの取得
***  現ページが１ページ目で且つＩＹがゼロの時
***  （２ページ以降から１ページに戻った時，ＩＹが１６になる為）
     IF   SEQNO(1) =   1  AND
          IY = ZERO
          COMPUTE  IZ  =  IY + 81
***  ２ページ以降又は，再度１ページ目から次ページ表示の時
     ELSE
          COMPUTE  IZ  =  IY
     END-IF.
*****DISPLAY       "AT-IY= " IY UPON CONS.
*****DISPLAY       "AT-IZ= " IZ UPON CONS.
*
*次ページ存在チェック
*****DISPLAY       "AT-WK-TEN(IZ)=" WK-TEN(IZ) UPON CONS.
     IF   WK-TEN(IZ)   NOT NUMERIC
       OR WK-TEN(IZ)   =   ZERO
          MOVE     10        TO   ERR-FLG
          MOVE     1         TO   ATOP1-FLG
          GO   TO       ATOP1-EXIT
      END-IF.
*
*後ページデータ，ＷＫ⇒画面処理
*
 ATOP1-EXIT.
     EXIT.
*----------------------------------------------------------*
*      2.5.2.2    後ページ処理 -ＷＫ⇒画面- （複数）       *
*----------------------------------------------------------*
 ATOP2-SEC                 SECTION.
*ＷＫから画面へ
     COMPUTE   IY    =    SEQNO(15)   +  1.
     PERFORM   WD-MOVE-SEC.
     IF     IX   <    15
            PERFORM  PRO-SEC
            GO    TO    ATOP2-EXIT
     END-IF.
     PERFORM   VARYING  IX   FROM  1  BY  1  UNTIL  IX  >  15
                MOVE    " "     TO   EDIT-STATUS  OF  MAISU1(IX)
                MOVE    " "     TO   EDIT-STATUS  OF  KEISH1(IX)
     END-PERFORM.
 ATOP2-EXIT.
     EXIT.
*----------------------------------------------------------*
*      2.5.2.1    プロテクト処理                           *
*----------------------------------------------------------*
 PRO-SEC                   SECTION.
     PERFORM   VARYING  IX   FROM  IX  BY  1  UNTIL  IX  >  15
         MOVE    "X"     TO   EDIT-STATUS  OF  MAISU1(IX)
         MOVE    "X"     TO   EDIT-STATUS  OF  KEISH1(IX)
     END-PERFORM.
 PRO-EXIT.
     EXIT.
*----------------------------------------------------------*
*      2.5.3      ファイル出力    （複数）                 *
*----------------------------------------------------------*
 OKU1-WTR-SEC           SECTION.
     PERFORM   VARYING  IX   FROM   1  BY  1  UNTIL IX  > 1290
                               OR   IX   >=    WK-MAX
               IF  WK-MAI(IX)   NOT  =   ZERO
                   MOVE    SPACE          TO   OKU-REC
                   INITIALIZE       OKU-REC
                   MOVE    TORCD1         TO   OKU-F02
                   MOVE    WK-TEN(IX)     TO   OKU-F03
                   MOVE    WK-YU1(IX)     TO   OKU-F151
                   MOVE    WK-YU2(IX)     TO   OKU-F152
                   MOVE    WK-NM1(IX)     TO   OKU-F05
                   MOVE    WK-NM2(IX)     TO   OKU-F06
                   MOVE    WK-JYU1(IX)    TO   OKU-F07
                   MOVE    WK-JYU2(IX)    TO   OKU-F08
                   MOVE    WK-TEL(IX)     TO   OKU-F09
                   MOVE    WK-MAI(IX)     TO   OKU-F12
                   MOVE    WK-KEISHO(IX)  TO   OKU-F14
                   WRITE   OKU-REC
               END-IF
     END-PERFORM.
*
     MOVE     1              TO   SYORI-FLG.
 OKU2-WTR-EXIT.
     EXIT.
*----------------------------------------------------------*
*      2.6       初期画面処理   （単一）                   *
*----------------------------------------------------------*
 INIT2-DSP-SEC        SECTION.
     MOVE     SPACE          TO   FKY13014.
     MOVE    "FKY13014"      TO   DSP-FORMAT.
     MOVE     SPACE          TO   DSP-PROC.
*システム日付／時刻取得
     PERFORM  GET-DATE-SEC.
     MOVE     HEN-DATE       TO   SDATE2.
     MOVE     HEN-TIME       TO   STIME2.
*
     MOVE     7              TO   SYORI-FLG.
 INIT2-DSP-EXIT.
     EXIT.
*----------------------------------------------------------*
*      2.7       ＨＥＡＤ２処理 （単一）　　               *
*----------------------------------------------------------*
 HEAD2-SEC            SECTION.
     PERFORM         DSP-SHOKIKA2-SEC.
     MOVE     PMSG04         TO   PFGID2.
     PERFORM         DSP-WRITE-SEC2.
*
     MOVE    "HEAD2" TO   DSP-GROUP.
     PERFORM         DSP-READ-SEC.
* アテンション判定
     EVALUATE    DSP-FUNC
         WHEN   "F004"
                        MOVE   1           TO   SYORI-FLG
         WHEN   "F005"
                        MOVE   "END"       TO   END-FLG
         WHEN   "E000"
                        PERFORM   HEAD2-CHK-SEC
         WHEN    OTHER
                        MOVE   1         TO   ERR-FLG
     END-EVALUATE.
 HEAD2-EXIT.
     EXIT.
*----------------------------------------------------------*
*      2.7.1     ＨＥＡＤ部の入力チェック （単一）         *
*----------------------------------------------------------*
 HEAD2-CHK-SEC             SECTION.
     MOVE   "M"     TO   EDIT-OPTION  OF  TORCD2
                         EDIT-OPTION  OF  TENCD2.
     MOVE   " "     TO   EDIT-CURSOR  OF  TORCD2
                         EDIT-CURSOR  OF  TENCD2.
     MOVE  SPACE                 TO  MAS201.
     MOVE  ZERO                  TO  M-FLG.
*取引先コードチェック
     IF  ( TORCD2  NOT  NUMERIC   )
         IF    ERR-FLG    =  ZERO
               MOVE   5       TO   ERR-FLG
         END-IF
         MOVE   "R"     TO   EDIT-OPTION  OF  TORCD2
         MOVE   "C"     TO   EDIT-CURSOR  OF  TORCD2
     END-IF.
*取引先Ｍ,店舗Ｍ存在チェック
     IF  ( TENCD2  NOT  NUMERIC   )
          MOVE   TORCD2        TO    TOK-F01
          PERFORM    TOK-READ-SEC
          IF  INV-FLG     =    1
              IF    ERR-FLG    =  ZERO
                    MOVE   6       TO   ERR-FLG
              END-IF
              MOVE   "R"     TO   EDIT-OPTION  OF  TORCD2
              MOVE   "C"     TO   EDIT-CURSOR  OF  TORCD2
          ELSE
              PERFORM    TOK-SET-SEC
          END-IF
     ELSE
         PERFORM    TEN-READ-SEC
         IF  INV-FLG    =   1
             IF    ERR-FLG    =  ZERO
                   MOVE   12      TO   ERR-FLG
             END-IF
             MOVE   "R"     TO   EDIT-OPTION  OF  TENCD2
             MOVE   "C"     TO   EDIT-CURSOR  OF  TENCD2
         ELSE
             PERFORM    TEN2-SET-SEC
         END-IF
     END-IF.
*
     IF     ERR-FLG    =     ZERO
            MOVE     8              TO   SYORI-FLG
            PERFORM         DSP-SHOKIKA2-SEC
     END-IF.
 HEAD2-CHK-EXIT.
     EXIT.
*----------------------------------------------------------*
*      2.7.1.1   店舗情報ＳＥＴ   （単一）                 *
*----------------------------------------------------------*
 TEN2-SET-SEC             SECTION.
     MOVE     1              TO   M-FLG.
     MOVE     TEN-F781       TO   YUBIN1.
     MOVE     TEN-F782       TO   YUBIN2.
     MOVE     TEN-F02        TO   OKURI1.
     MOVE     SPACE          TO   OKURI2.
     MOVE     TEN-F06        TO   JUSHO1.
     MOVE     TEN-F07        TO   JUSHO2.
     MOVE     TEN-F08        TO   TELNO.
 TEN2-SET-EXIT.
     EXIT.
*----------------------------------------------------------*
*      2.7.2.2   得意先情報ＳＥＴ  （単一）                *
*----------------------------------------------------------*
 TOK-SET-SEC             SECTION.
     MOVE     TOK-F801       TO   YUBIN1.
     MOVE     TOK-F802       TO   YUBIN2.
     MOVE     TOK-F02        TO   OKURI1.
     MOVE     SPACE          TO   OKURI2.
     MOVE     TOK-F06        TO   JUSHO1.
     MOVE     TOK-F07        TO   JUSHO2.
     MOVE     TOK-F08        TO   TELNO.
 TOK-SET-EXIT.
     EXIT.
*----------------------------------------------------------*
*      2.8       ＢＯＤＹ２処理 （単一）　　               *
*----------------------------------------------------------*
 BODY2-SEC            SECTION.
     MOVE     PMSG02         TO   PFGID2.
     PERFORM         DSP-WRITE-SEC2.
*
     MOVE    "BODY2" TO   DSP-GROUP.
     PERFORM         DSP-READ-SEC.
* アテンション判定
     EVALUATE    DSP-FUNC
         WHEN   "F004"
                 MOVE   "M"     TO   EDIT-OPTION  OF  TORCD2
                                     EDIT-OPTION  OF  TENCD2
                 MOVE   " "     TO   EDIT-CURSOR  OF  TORCD2
                                     EDIT-CURSOR  OF  TENCD2
                        MOVE   6           TO   SYORI-FLG
         WHEN   "F005"
                        MOVE   "END"       TO   END-FLG
         WHEN   "F006"
                 MOVE   "M"     TO   EDIT-OPTION  OF  TORCD2
                                     EDIT-OPTION  OF  TENCD2
                 MOVE   " "     TO   EDIT-CURSOR  OF  TORCD2
                                     EDIT-CURSOR  OF  TENCD2
                        PERFORM         DSP-SHOKIKA2-SEC
                        MOVE   7           TO   SYORI-FLG
         WHEN   "E000"
                        PERFORM   BODY2-CHK-SEC
         WHEN    OTHER
                 MOVE   1         TO   ERR-FLG
     END-EVALUATE.
 BODY2-EXIT.
     EXIT.
*----------------------------------------------------------*
*      2.8.1     ＢＯＤＹ部の入力チェック  （単一）        *
*----------------------------------------------------------*
 BODY2-CHK-SEC             SECTION.
     PERFORM         DSP-SHOKIKA2-SEC.
*敬称区分チェック
     IF  ( KEISH2  NOT  NUMERIC   )
         MOVE   2       TO   KEISH2
     END-IF.
     IF  ( KEISH2   =  1 OR 2 OR 9 )
         CONTINUE
     ELSE
         IF    ERR-FLG    =  ZERO
               MOVE   13     TO   ERR-FLG
         END-IF
         MOVE   "R"     TO   EDIT-OPTION  OF  KEISH2
         MOVE   "C"     TO   EDIT-CURSOR  OF  KEISH2
     END-IF.
*印刷枚数チェック
     IF  ( MAISU2  NOT  NUMERIC   )
         IF    ERR-FLG    =  ZERO
               MOVE   8      TO   ERR-FLG
         END-IF
         MOVE   "R"     TO   EDIT-OPTION  OF  MAISU2
         MOVE   "C"     TO   EDIT-CURSOR  OF  MAISU2
     ELSE
         IF  MAISU2    =   ZERO
             IF    ERR-FLG    =  ZERO
                   MOVE   8      TO   ERR-FLG
             END-IF
             MOVE   "R"     TO   EDIT-OPTION  OF  MAISU2
             MOVE   "C"     TO   EDIT-CURSOR  OF  MAISU2
         END-IF
     END-IF.
*
     IF       ERR-FLG   =    ZERO
              MOVE     9              TO   SYORI-FLG
     END-IF.
 BODY2-CHK-EXIT.
     EXIT.
*----------------------------------------------------------*
*      2.9       確認入力２    （単一）                    *
*----------------------------------------------------------*
 KAKNIN2-SEC       SECTION.
     MOVE     "Y"            TO   CHECK2.
     PERFORM       DSP-WRITE-SEC2.
*
     MOVE    "KAKN2"         TO    DSP-GROUP.
     PERFORM       DSP-READ-SEC.
*
* アテンション判定
     EVALUATE  DSP-FUNC
         WHEN   "F004"
                        MOVE    6      TO   SYORI-FLG
         WHEN   "F005"
                        MOVE  "END"    TO   END-FLG
         WHEN   "F006"
                        MOVE    8      TO   SYORI-FLG
                        MOVE    SPACE  TO   CHECK2
         WHEN   "E000"
                        IF  CHECK2  NOT  =  "Y"
                            MOVE    11      TO  ERR-FLG
                        ELSE
                            PERFORM  OKU2-WTR-SEC
                        END-IF
         WHEN    OTHER
                        MOVE    1      TO   ERR-FLG
     END-EVALUATE.
 KAKNIN2-EXIT.
     EXIT.
*----------------------------------------------------------*
*      2.9.1      ファイル出力    （単一）                 *
*----------------------------------------------------------*
 OKU2-WTR-SEC           SECTION.
     MOVE   SPACE     TO   OKU-REC.
     INITIALIZE       OKU-REC.
*
***  取引先コード
     MOVE    TORCD2         TO   OKU-F02.
***  店舗コード
     IF    M-FLG    =   1
           MOVE    TENCD2         TO   OKU-F03
     ELSE
           MOVE    ZERO           TO   OKU-F03
     END-IF.
***  旧郵便番号
***  MOVE    R20003         TO   OKU-F04.
***  新郵便番号
     MOVE    YUBIN1         TO   OKU-F151.
     MOVE    YUBIN2         TO   OKU-F152.
***  送り先１
     MOVE    OKURI1         TO   OKU-F05.
***  送り先２
     MOVE    OKURI2         TO   OKU-F06.
***  住所１
     MOVE    JUSHO1         TO   OKU-F07.
***  住所２
     MOVE    JUSHO2         TO   OKU-F08.
***  電話番号
     MOVE    TELNO          TO   OKU-F09.
***  敬称区分
     MOVE    KEISH2         TO   OKU-F14.
***  印刷枚数　
     MOVE    MAISU2         TO   OKU-F12.
*
     WRITE   OKU-REC.
*
     MOVE    6               TO   SYORI-FLG.
 OKU2-WTR-EXIT.
     EXIT.
*----------------------------------------------------------*
*                取引先マスタＲＥＡＤ                      *
*----------------------------------------------------------*
 TOK-READ-SEC           SECTION.
     READ    HTOKMS
       INVALID
          MOVE      1        TO   INV-FLG
       NOT INVALID
          MOVE      0        TO   INV-FLG
     END-READ.
 TOK-READ-EXIT.
     EXIT.
*----------------------------------------------------------*
*                店舗マスタＲＥＡＤ （複数）               *
*----------------------------------------------------------*
 TEN-READ1-SEC           SECTION.
     READ    HTENMS   NEXT
             AT   END
                  MOVE      1     TO   TEN-FLG
       NOT   AT   END
                  MOVE      0     TO   TEN-FLG
                  IF   WK-TOKCD   NOT   =   TEN-F52
                       MOVE      1     TO   TEN-FLG
********************   DISPLAY   TEN-F52 UPON CONS
                  END-IF
     END-READ.
 TEN-READ1-EXIT.
     EXIT.
*----------------------------------------------------------*

*                店舗マスタＲＥＡＤ （単一）               *
*----------------------------------------------------------*
 TEN-READ-SEC           SECTION.
     MOVE   TORCD2        TO    TEN-F52.
     MOVE   TENCD2        TO    TEN-F011.
     READ    HTENMS
       INVALID
          MOVE      1        TO   INV-FLG
       NOT INVALID
          MOVE      0        TO   INV-FLG
     END-READ.
 TEN-READ-EXIT.
     EXIT.
*----------------------------------------------------------*
*                画面表示処理１                            *
*----------------------------------------------------------*
 DSP-WRITE-SEC1         SECTION.
     IF    ERR-FLG   =    0
           MOVE    SPACE    TO   ERRMG1
     ELSE
           MOVE    MSG-TBL(ERR-FLG)     TO   ERRMG1
     END-IF.
*
     MOVE    "FKY13013"      TO   DSP-FORMAT.
     MOVE    "ALLF "        TO   DSP-GROUP.
     MOVE     SPACE          TO   DSP-PROC.
**** TEST
*****DISPLAY  TENCD1(1) UPON CONS.
*****DISPLAY  TENCD1(2) UPON CONS.
*
     WRITE    DSP-AREA       FROM FKY13013.
*
     IF    ERR-FLG   NOT   =    0
           MOVE    0        TO   ERR-FLG
     END-IF.
 DSP-WRITE1-EXIT.
     EXIT.
*----------------------------------------------------------*
*                画面表示処理２                            *
*----------------------------------------------------------*
 DSP-WRITE-SEC2         SECTION.
     IF    ERR-FLG   =    0
           MOVE    SPACE    TO   ERRMG2
     ELSE
           MOVE    MSG-TBL(ERR-FLG)     TO   ERRMG2
     END-IF.
*
     MOVE    "FKY13014"      TO   DSP-FORMAT.
     MOVE    "SCREEN"        TO   DSP-GROUP.
     MOVE     SPACE          TO   DSP-PROC.
*
**** TEST
*****DISPLAY  TENCD1(1) UPON CONS.
*****DISPLAY  TENCD1(2) UPON CONS.
     WRITE    DSP-AREA       FROM FKY13014.
*
     IF    ERR-FLG   NOT   =    0
           MOVE    0        TO   ERR-FLG
     END-IF.
 DSP-WRITE2-EXIT.
     EXIT.
*----------------------------------------------------------*
*                画面データの入力処理                      *
*----------------------------------------------------------*
 DSP-READ-SEC           SECTION.
     MOVE  "NE"    TO   DSP-PROC.
     READ   DSPF.
     IF  DSP-FORMAT  = "FKY13013"
         MOVE    DSP-AREA    TO   FKY13013
     ELSE
         MOVE    DSP-AREA    TO   FKY13014
     END-IF.
 DSP-READ-EXIT.
     EXIT.
*----------------------------------------------------------*
*                画面項目定義初期化（複数）　              *
*----------------------------------------------------------*
 DSP-SHOKIKA1-SEC        SECTION.
*
     MOVE   " "     TO   EDIT-CURSOR  OF  SHORI.
     MOVE   "M"     TO   EDIT-OPTION  OF  SHORI.
     MOVE   " "     TO   EDIT-CURSOR  OF  TORCD1.
     MOVE   "M"     TO   EDIT-OPTION  OF  TORCD1.
     PERFORM   VARYING   IX   FROM  1  BY  1   UNTIL  IX  >   15
****                           OR  SEQNO(IX)   NOT  NUMERIC
         MOVE   " "     TO   EDIT-CURSOR  OF  KEISH1(IX)
         MOVE   "M"     TO   EDIT-OPTION  OF  KEISH1(IX)
     END-PERFORM.
 DSP-SHOKIKA1-EXIT.
     EXIT.
*----------------------------------------------------------*
*                画面項目定義初期化（単一）　              *
*----------------------------------------------------------*
 DSP-SHOKIKA2-SEC        SECTION.
     MOVE   " "     TO   EDIT-CURSOR  OF  MAISU2.
     MOVE   "M"     TO   EDIT-OPTION  OF  MAISU2.
     MOVE   " "     TO   EDIT-CURSOR  OF  KEISH2.
     MOVE   "M"     TO   EDIT-OPTION  OF  KEISH2.
 DSP-SHOKIKA2-EXIT.
     EXIT.
*----------------------------------------------------------*
*                システム日付・時刻取得                    *
*----------------------------------------------------------*
 GET-DATE-SEC          SECTION.
*システム日付・時刻の取得
     ACCEPT   WK-DATE           FROM   DATE.
     MOVE     "3"                 TO   LINK-IN-KBN.
     MOVE     WK-DATE             TO   LINK-IN-YMD6.
     MOVE     ZERO                TO   LINK-IN-YMD8.
     MOVE     ZERO                TO   LINK-OUT-RET.
     MOVE     ZERO                TO   LINK-OUT-YMD.
 GET010.
     CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD.
     MOVE      LINK-OUT-YMD       TO   DATE-AREA.
 GET020.
*画面表示日付編集
     MOVE      SYS-DATE(1:4)      TO   HEN-DATE-YYYY.
     MOVE      SYS-DATE(5:2)      TO   HEN-DATE-MM.
     MOVE      SYS-DATE(7:2)      TO   HEN-DATE-DD.
*システム日付取得
     ACCEPT    WK-TIME          FROM   TIME.
*画面表示時刻編集
     MOVE      WK-TIME(1:2)       TO   HEN-TIME-HH.
     MOVE      WK-TIME(3:2)       TO   HEN-TIME-MM.
     MOVE      WK-TIME(5:2)       TO   HEN-TIME-SS.
 GET-DATE-EXIT.
     EXIT.
*****************<<  PROGRAM  END  >>***********************

```
