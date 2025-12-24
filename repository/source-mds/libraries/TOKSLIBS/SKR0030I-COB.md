# SKR0030I

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SKR0030I.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　在庫管理システム　　　　　　　　　*
*    モジュール名　　　　：　送り状印刷（送り状ファイル作成）　*
*    作成日／更新日　　　：　2002/02/20                        *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    作成日／更新日　　　：　2006/02/01                        *
*    作成者／更新者　　　：　ＮＡＶ松野　　　　　　　　　　　　*
*    処理概要　　　　　　：　取引先マスタ・店舗マスタから送り  *
*                            状ファイルの登録を行う。　　　　　*
*    更新概要　　　　　　：　出荷日入力追加(2006/02/01)        *
****************************************************************
 IDENTIFICATION         DIVISION.
 PROGRAM-ID.            SKR0030I.
 AUTHOR.                NAV.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       GP6000.
 OBJECT-COMPUTER.       GP6000.
 SPECIAL-NAMES.
         STATION   IS   STAT
         CONSOLE   IS   CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*-----<<  取引先マスタ  >>-----*
     SELECT   HTOKMS    ASSIGN    TO        DA-01-VI-TOKMS2
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   TOK-F01
                        FILE      STATUS    IS   TOK-STS.
*-----<<  店舗マスタ  >>-----*
     SELECT   HTENMS    ASSIGN    TO        DA-01-VI-TENMS1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   DYNAMIC
                        RECORD    KEY       IS   TEN-F52
                                                 TEN-F011
                        FILE      STATUS    IS   TEN-STS.
*-----<<  送り状ファイル  >>-----*
     SELECT   KOKURIF   ASSIGN    TO        DA-01-S-KOKURIF
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
 FD  KOKURIF.
     COPY     KOKURIF   OF   XFDLIB  JOINING   OKU  PREFIX.
*-----<<  画面ファイル  >>-----*
 FD  DSPF.
 01  DSP-AREA                PIC  X(2000).
****************************************************************
*   ＷＯＲＫＩＮＧ－ＳＴＯＲＡＧＥ　　　ＳＥＣＴＩＯＮ
****************************************************************
 WORKING-STORAGE             SECTION.
****  画面ファイル  ****
 COPY   FKR00301         OF        XMDLIB.
 COPY   FKR00302         OF        XMDLIB.
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
****  インデックス            ****
 01  WK-AREA1.
     03  IX                  PIC  9(03)  VALUE ZERO.
     03  IY                  PIC  9(03)  VALUE ZERO.
     03  WK-MAX              PIC  9(03)  VALUE ZERO.
     03  WK-TOKCD            PIC  9(08)  VALUE ZERO.
*
*日付／時刻
 01  TIME-AREA.
     03  WK-TIME                  PIC  9(08)  VALUE  ZERO.
 01  DATE-AREA.
     03  WK-YS                    PIC  9(02)  VALUE  ZERO.
     03  WK-DATE.
         05  WK-Y                 PIC  9(02)  VALUE  ZERO.
         05  WK-M                 PIC  9(02)  VALUE  ZERO.
         05  WK-D                 PIC  9(02)  VALUE  ZERO.
 01  DATE-AREAR2       REDEFINES      DATE-AREA.
     03  SYS-DATE                 PIC  9(08).
*画面表示日付編集
 01  HEN-DATE.
     03  HEN-DATE-YYYY            PIC  9(04)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  "/".
     03  HEN-DATE-MM              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  "/".
     03  HEN-DATE-DD              PIC  9(02)  VALUE  ZERO.
*画面表示時刻編集
 01  HEN-TIME.
     03  HEN-TIME-HH              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  ":".
     03  HEN-TIME-MM              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  ":".
     03  HEN-TIME-SS              PIC  9(02)  VALUE  ZERO.
****  退避領域                ****
 01  WK-OKU.
     03  WK-OKUTBL           OCCURS   300.
         05  WK-TEN          PIC  9(05)  VALUE ZERO.
         05  WK-YU1          PIC  X(03)  VALUE SPACE.
         05  WK-YU2          PIC  X(04)  VALUE SPACE.
         05  WK-NM1          PIC  N(15)  VALUE SPACE.
         05  WK-NM2          PIC  N(15)  VALUE SPACE.
         05  WK-JYU1         PIC  N(15)  VALUE SPACE.
         05  WK-JYU2         PIC  N(15)  VALUE SPACE.
         05  WK-TEL          PIC  X(12)  VALUE SPACE.
         05  WK-KOSU         PIC  9(03)  VALUE ZERO.
         05  WK-OIN          PIC  9(04)  VALUE ZERO.
         05  WK-KIN          PIC  9(04)  VALUE ZERO.
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
       03  ERR-PG-ID         PIC  X(08)  VALUE  "SKR0030I".
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
            NC"出荷日論理エラー。".
 01  FILLER                  REDEFINES    ERR-TAB.
     03  MSG-TBL             PIC  N(28)  OCCURS   13.
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
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
                        PROCEDURE   KOKURIF.
     MOVE     "KOKURIF"        TO   ERR-FL-ID.
     MOVE      OKU-STS         TO   ERR-STCD.
     DISPLAY   MSG-ABEND1    UPON   CONS.
     DISPLAY   MSG-ABEND2    UPON   CONS.
     MOVE      4000            TO   PROGRAM-STATUS.
     STOP      RUN.
 END     DECLARATIVES.
************************************************************
*      _０     メインモジュール                           *
************************************************************
 SKR0030I-START         SECTION.
     PERFORM      INIT-SEC.
     PERFORM      MAIN-SEC
                  UNTIL     END-FLG  =    "END".
     PERFORM      END-SEC.
     STOP      RUN.
 SKR0030I-END.
     EXIT.
************************************************************
*      _０     初期処理                                   *
************************************************************
 INIT-SEC               SECTION.
     OPEN     INPUT     HTOKMS    HTENMS.
     OPEN     I-O       DSPF.
     OPEN     OUTPUT    KOKURIF.
*
*システム日付・時刻の取得
     ACCEPT   WK-DATE           FROM   DATE.
     MOVE     "3"                 TO   LINK-IN-KBN.
     MOVE     WK-DATE             TO   LINK-IN-YMD6.
     MOVE     ZERO                TO   LINK-IN-YMD8.
     MOVE     ZERO                TO   LINK-OUT-RET.
     MOVE     ZERO                TO   LINK-OUT-YMD.
     CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD.
     MOVE      LINK-OUT-YMD       TO   DATE-AREA.
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
     CLOSE    HTOKMS    HTENMS    KOKURIF   DSPF.
 END-END.
     EXIT.
*----------------------------------------------------------*
*      2.1       初期画面処理                              *
*----------------------------------------------------------*
 INIT1-DSP-SEC        SECTION.
     MOVE     SPACE          TO   FKR00301.
     MOVE     SPACE          TO   FKR00302.
     MOVE    "FKR00301"      TO   DSP-FORMAT.
     MOVE     SPACE          TO   DSP-PROC.
     MOVE     SYS-DATE       TO   DATE1.
*
     MOVE     2              TO   SYORI-FLG.
 INIT1-DSP-EXIT.
     EXIT.
*----------------------------------------------------------*
*      2.2       処理区分入力                              *
*----------------------------------------------------------*
 SYORI-SEC            SECTION.
     MOVE   " "     TO   EDIT-CURSOR  OF  R10002
                         EDIT-CURSOR  OF  R10003
                         EDIT-CURSOR  OF  DATE1
                         EDIT-CURSOR  OF  R10006.
     MOVE   "M"     TO   EDIT-OPTION  OF  R10002
                         EDIT-OPTION  OF  R10003
                         EDIT-OPTION  OF  DATE1
                         EDIT-OPTION  OF  R10006.
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
     MOVE   " "     TO   EDIT-CURSOR  OF  R10001.
     MOVE   "M"     TO   EDIT-OPTION  OF  R10001.
*処理区分チェック
     IF  ( R10001  NOT  NUMERIC   )
         MOVE   2         TO   ERR-FLG
         MOVE   "R"     TO   EDIT-OPTION  OF  R10001
         MOVE   "C"     TO   EDIT-CURSOR  OF  R10001
     ELSE
         IF  ( R10001   =  1 OR 2  )
              IF   R10001   =  1
                   MOVE     3              TO   SYORI-FLG
              ELSE
                   MOVE     6              TO   SYORI-FLG
              END-IF
         ELSE
              MOVE   2         TO   ERR-FLG
              MOVE   "R"     TO   EDIT-OPTION  OF  R10001
              MOVE   "C"     TO   EDIT-CURSOR  OF  R10001
         END-IF
     END-IF.
 SYORICHK-EXIT.
     EXIT.
*----------------------------------------------------------*
*      2.3       ＨＥＡＤ１処理 （複数）　　               *
*----------------------------------------------------------*
 HEAD1-SEC            SECTION.
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
     MOVE   "M"     TO   EDIT-OPTION  OF  R10002.
     MOVE   " "     TO   EDIT-CURSOR  OF  R10002.
     MOVE   "M"     TO   EDIT-OPTION  OF  R10006.
     MOVE   " "     TO   EDIT-CURSOR  OF  R10006.
     MOVE   "M"     TO   EDIT-OPTION  OF  R10003.
     MOVE   " "     TO   EDIT-CURSOR  OF  R10003.
     MOVE   "M"     TO   EDIT-OPTION  OF  DATE1.
     MOVE   " "     TO   EDIT-CURSOR  OF  DATE1.
     IF  ( R10002  NOT  NUMERIC   )
         MOVE   "R"     TO   EDIT-OPTION  OF  R10002
         MOVE   "C"     TO   EDIT-CURSOR  OF  R10002
         MOVE   3       TO   ERR-FLG
     ELSE
         IF  (  R10002 =  1  OR  2  OR  3  )
              CONTINUE
         ELSE
              IF    ERR-FLG    =  ZERO
                    MOVE   3       TO   ERR-FLG
              END-IF
              MOVE   "R"     TO   EDIT-OPTION  OF  R10002
              MOVE   "C"     TO   EDIT-CURSOR  OF  R10002
         END-IF
     END-IF.
*発送元印刷チェック
     IF  ( R10006  NOT  NUMERIC   )
         MOVE   0         TO   R10006
     END-IF.
     IF  ( R10006   =  0 OR 1  )
         CONTINUE
     ELSE
         IF    ERR-FLG    =  ZERO
               MOVE   4       TO   ERR-FLG
         END-IF
         MOVE   "R"     TO   EDIT-OPTION  OF  R10006
         MOVE   "C"     TO   EDIT-CURSOR  OF  R10006
     END-IF.
*取引先コードチェック
     IF  ( R10003  NOT  NUMERIC   )
         IF    ERR-FLG    =  ZERO
               MOVE   5       TO   ERR-FLG
         END-IF
         MOVE   SPACE     TO   W10001
         MOVE   "R"     TO   EDIT-OPTION  OF  R10003
         MOVE   "C"     TO   EDIT-CURSOR  OF  R10003
         GO             TO   HEAD1-CHK-EXIT
     END-IF.
     MOVE   R10003        TO    TOK-F01.
     MOVE   ZERO          TO    INV-FLG.
     PERFORM    TOK-READ-SEC.
     IF  INV-FLG   NOT   =    ZERO
         MOVE   SPACE        TO   W10001
         IF    ERR-FLG    =  ZERO
               MOVE   6       TO   ERR-FLG
         END-IF
         MOVE   "R"     TO   EDIT-OPTION  OF  R10003
         MOVE   "C"     TO   EDIT-CURSOR  OF  R10003
     ELSE
         MOVE   TOK-F02      TO   W10001
     END-IF.
*日付チェック
     IF  ( DATE1 NOT  NUMERIC   )
         MOVE   "R"     TO   EDIT-OPTION  OF  DATE1
         MOVE   "C"     TO   EDIT-CURSOR  OF  DATE1
         IF     ERR-FLG =    ZERO
                MOVE    13   TO   ERR-FLG
         END-IF
     ELSE
         MOVE     "2"                 TO   LINK-IN-KBN
         MOVE     ZERO                TO   LINK-IN-YMD6
         MOVE     DATE1               TO   LINK-IN-YMD8
         MOVE     ZERO                TO   LINK-OUT-RET
         MOVE     ZERO                TO   LINK-OUT-YMD
         CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                           LINK-IN-YMD6
                                           LINK-IN-YMD8
                                           LINK-OUT-RET
                                           LINK-OUT-YMD
         IF   LINK-OUT-RET   NOT = ZERO
              IF    ERR-FLG    =  ZERO
                    MOVE   13      TO   ERR-FLG
              END-IF
              MOVE   "R"     TO   EDIT-OPTION  OF  DATE1
              MOVE   "C"     TO   EDIT-CURSOR  OF  DATE1
         END-IF
     END-IF.
*店舗マスタＳＴＡＲＴリード
     PERFORM   TEN-START-SEC.
     IF  TEN-FLG   =  1
         IF    ERR-FLG    =  ZERO
               MOVE   12      TO   ERR-FLG
         END-IF
         MOVE   "R"     TO   EDIT-OPTION  OF  R10003
         MOVE   "C"     TO   EDIT-CURSOR  OF  R10003
     END-IF.
*ＷＫテーブルクリア
     MOVE    SPACE      TO  WK-OKU.
     INITIALIZE         WK-OKU.
*店舗マスタＴＢＬ（ＷＫ）へＳＥＴ
     PERFORM   VARYING   IX   FROM  1  BY  1   UNTIL
                          (   TEN-FLG  =  1    )
                      OR  (   IX       >  299  )
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
               MOVE   IX            TO    W10101(IX)
***************DISPLAY    WK-TEN(IX)    UPON CONS
               MOVE   WK-TEN(IX)    TO    W10201(IX)
               MOVE   WK-NM1(IX)    TO    W10301(IX)
     END-PERFORM.
*
     IF    ERR-FLG    =    ZERO
           MOVE     4              TO   SYORI-FLG
     END-IF.
 HEAD1-CHK-EXIT.
     EXIT.
*----------------------------------------------------------*
*      2.3.1.1    店舗Ｍ　ＳＴＡＲＴ　処理                 *
*----------------------------------------------------------*
 TEN-START-SEC             SECTION.
     MOVE   R10003        TO    TEN-F52   WK-TOKCD.
*****DISPLAY R10003 UPON CONS.
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
     MOVE    "BODY1" TO   DSP-GROUP.
     PERFORM         DSP-READ-SEC.
* アテンション判定
     EVALUATE    DSP-FUNC
         WHEN   "F004"
                        MOVE   1           TO   SYORI-FLG
         WHEN   "F005"
                        MOVE   "END"       TO   END-FLG
         WHEN   "F006"
                        MOVE   3           TO   SYORI-FLG
         WHEN   "F011"
                        PERFORM   BODY1-WKSET-SEC
                        PERFORM   MAEP-SEC
         WHEN   "F012"
                        PERFORM   BODY1-WKSET-SEC
                        PERFORM   ATOP-SEC
         WHEN   "E000"
                        PERFORM   BODY1-WKSET-SEC
                        MOVE      5       TO   SYORI-FLG
         WHEN    OTHER
                        MOVE   1         TO   ERR-FLG
     END-EVALUATE.
 BODY1-EXIT.
     EXIT.
*----------------------------------------------------------*
*      2.4.1      ＷＫへ転送　    （複数）                 *
*----------------------------------------------------------*
 BODY1-WKSET-SEC           SECTION.
     MOVE      W10101(1)       TO   IY.
     PERFORM   VARYING  IX   FROM  1  BY  1  UNTIL  IX  >  15
                               OR  W10101(IX)   NOT  NUMERIC
               IF   R10201(IX)   NOT  NUMERIC
                    MOVE    ZERO      TO   R10201(IX)
               END-IF
               IF   R10101(IX)   NOT  NUMERIC
                    MOVE    ZERO      TO   R10101(IX)
               END-IF
               IF   R10301(IX)   NOT  NUMERIC
                    MOVE    ZERO      TO   R10301(IX)
               END-IF
               MOVE    R10201(IX)      TO   WK-KOSU(IY)
               MOVE    R10101(IX)      TO   WK-OIN(IY)
               MOVE    R10301(IX)      TO   WK-KIN(IY)
               ADD     1               TO   IY
     END-PERFORM.
 BODY1-WKSET-EXIT.
     EXIT.
*----------------------------------------------------------*
*      2.5       確認入力１　  （複数）                    *
*----------------------------------------------------------*
 KAKNIN1-SEC       SECTION.
     MOVE     "Y"            TO   R10005.
     MOVE     PMSG03         TO   PFGID1.
     PERFORM       DSP-WRITE-SEC1.
*
     MOVE    "KAKN1"         TO    DSP-GROUP.
     PERFORM       DSP-READ-SEC.
* アテンション判定
     EVALUATE  DSP-FUNC
         WHEN   "F004"
                        MOVE    1      TO   SYORI-FLG
         WHEN   "F005"
                        MOVE  "END"    TO   END-FLG
         WHEN   "F006"
                        MOVE    4      TO   SYORI-FLG
                        MOVE    SPACE  TO   R10005
         WHEN   "F011"
                        PERFORM   MAEP-SEC
                        MOVE    SPACE  TO   R10005
         WHEN   "F012"
                        PERFORM   ATOP-SEC
                        MOVE    SPACE  TO   R10005
         WHEN   "E000"
                        IF  R10005  NOT  =  "Y"
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
*      2.5.1      前ページ処理　（複数）　　               *
*----------------------------------------------------------*
 MAEP-SEC                  SECTION.
     IF   W10101(1)    =   1
          IF    ERR-FLG    =  ZERO
                MOVE   9       TO   ERR-FLG
          END-IF
                GO     TO      MAEP-EXIT
     END-IF.
*ＷＫから画面へ
     COMPUTE   IY    =    W10101(1)  -    15.
     PERFORM   WD-MOVE-SEC.
     PERFORM   VARYING  IX   FROM  1  BY  1  UNTIL  IX  >  15
                MOVE    " "     TO   EDIT-STATUS  OF  R10101(IX)
                MOVE    " "     TO   EDIT-STATUS  OF  R10201(IX)
                MOVE    " "     TO   EDIT-STATUS  OF  R10301(IX)
     END-PERFORM.
 MAEP-EXIT.
     EXIT.
*----------------------------------------------------------*
*      2.5.1.1    ＷＫから画面へ（複数）　　               *
*----------------------------------------------------------*
 WD-MOVE-SEC               SECTION.
     MOVE     SPACE          TO   MAS002.
     PERFORM   VARYING  IX   FROM  1  BY  1  UNTIL  IX  >  15
                               OR  IY   >=  WK-MAX
               MOVE    IY              TO   W10101(IX)
               MOVE    WK-TEN(IY)      TO   W10201(IX)
               MOVE    WK-NM1(IY)      TO   W10301(IX)
               MOVE    WK-OIN(IY)      TO   R10101(IX)
               MOVE    WK-KOSU(IY)     TO   R10201(IX)
               MOVE    WK-KIN(IY)      TO   R10301(IX)
               ADD     1               TO   IY
     END-PERFORM.
*
     MOVE     4              TO   SYORI-FLG.
 WD-MOVE-EXIT.
     EXIT.
*----------------------------------------------------------*
*      2.5.2      後ページ処理　（複数）　　               *
*----------------------------------------------------------*
 ATOP-SEC                  SECTION.
*
*次ページ存在チェック
*****DISPLAY       "IY=" IY UPON CONS.
*****DISPLAY       "AT-WK-TEN(IY)=" WK-TEN(IY) UPON CONS.
     IF   WK-TEN(IY)   NOT NUMERIC
       OR WK-TEN(IY)   =   ZERO
          MOVE     10        TO   ERR-FLG
          GO   TO       ATOP-EXIT
      END-IF.
*
*ＷＫから画面へ
     COMPUTE   IY    =    W10101(15)   +  1.
     PERFORM   WD-MOVE-SEC.
     IF     IX   <    15
            PERFORM  PRO-SEC
            GO    TO    ATOP-EXIT
     END-IF.
     PERFORM   VARYING  IX   FROM  1  BY  1  UNTIL  IX  >  15
                MOVE    " "     TO   EDIT-STATUS  OF  R10101(IX)
                MOVE    " "     TO   EDIT-STATUS  OF  R10201(IX)
                MOVE    " "     TO   EDIT-STATUS  OF  R10301(IX)
     END-PERFORM.
 ATOP-EXIT.
     EXIT.
*----------------------------------------------------------*
*      2.5.2.1    プロテクト処理                           *
*----------------------------------------------------------*
 PRO-SEC                   SECTION.
     PERFORM   VARYING  IX   FROM  IX  BY  1  UNTIL  IX  >  15
         MOVE    "X"     TO   EDIT-STATUS  OF  R10101(IX)
         MOVE    "X"     TO   EDIT-STATUS  OF  R10201(IX)
         MOVE    "X"     TO   EDIT-STATUS  OF  R10301(IX)
     END-PERFORM.
 PRO-EXIT.
     EXIT.
*----------------------------------------------------------*
*      2.5.3      ファイル出力    （複数）                 *
*----------------------------------------------------------*
 OKU1-WTR-SEC           SECTION.
     PERFORM   VARYING  IX   FROM   1  BY  1  UNTIL IX  > 300
                               OR   IX   >=    WK-MAX
               IF  WK-OIN(IX)   NOT  =   ZERO    OR
                   WK-KIN(IX)   NOT  =   ZERO
                   MOVE    SPACE          TO   OKU-REC
                   INITIALIZE       OKU-REC
                   MOVE    R10002         TO   OKU-F01
                   MOVE    R10003         TO   OKU-F02
                   MOVE    WK-TEN(IX)     TO   OKU-F03
                   MOVE    DATE1(3:6)     TO   OKU-F04
                   MOVE    WK-YU1(IX)     TO   OKU-F151
                   MOVE    WK-YU2(IX)     TO   OKU-F152
                   MOVE    WK-NM1(IX)     TO   OKU-F05
                   MOVE    WK-NM2(IX)     TO   OKU-F06
                   MOVE    WK-JYU1(IX)    TO   OKU-F07
                   MOVE    WK-JYU2(IX)    TO   OKU-F08
                   MOVE    WK-TEL(IX)     TO   OKU-F09
                   MOVE    R10004         TO   OKU-F10
                   MOVE    WK-KOSU(IX)    TO   OKU-F11
                   MOVE    WK-OIN(IX)     TO   OKU-F12
                   MOVE    WK-KIN(IX)     TO   OKU-F13
                   MOVE    R10006         TO   OKU-F14
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
     MOVE     SPACE          TO   FKR00302.
     MOVE    "FKR00302"      TO   DSP-FORMAT.
     MOVE     SPACE          TO   DSP-PROC.
     MOVE     SYS-DATE       TO   DATE2.
*
     MOVE     7              TO   SYORI-FLG.
 INIT2-DSP-EXIT.
     EXIT.
*----------------------------------------------------------*
*      2.7       ＨＥＡＤ２処理 （単一）　　               *
*----------------------------------------------------------*
 HEAD2-SEC            SECTION.
     PERFORM         HEAD-CLR2-SEC.
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
     MOVE   "M"     TO   EDIT-OPTION  OF  R20001
                         EDIT-OPTION  OF  R20002.
     MOVE   " "     TO   EDIT-CURSOR  OF  R20001
                         EDIT-CURSOR  OF  R20002.
     MOVE  SPACE                 TO  MAS201.
     MOVE  ZERO                  TO  M-FLG.
*取引先コードチェック
     IF  ( R20001  NOT  NUMERIC   )
         IF    ERR-FLG    =  ZERO
               MOVE   5       TO   ERR-FLG
         END-IF
         MOVE   "R"     TO   EDIT-OPTION  OF  R20001
         MOVE   "C"     TO   EDIT-CURSOR  OF  R20001
     END-IF.
*取引先コードＭ,店舗コードＭ存在チェック
     IF  ( R20002  NOT  NUMERIC   )
          MOVE   R20001        TO    TOK-F01
          PERFORM    TOK-READ-SEC
          IF  INV-FLG     =    1
         IF    ERR-FLG    =  ZERO
               MOVE   6       TO   ERR-FLG
         END-IF
              MOVE   "R"     TO   EDIT-OPTION  OF  R20001
              MOVE   "C"     TO   EDIT-CURSOR  OF  R20001
          ELSE
              PERFORM    TOK-SET-SEC
          END-IF
     ELSE
         PERFORM    TEN-READ-SEC
         IF  INV-FLG    =   1
             IF    ERR-FLG    =  ZERO
                   MOVE   12      TO   ERR-FLG
             END-IF
             MOVE   "R"     TO   EDIT-OPTION  OF  R20002
             MOVE   "C"     TO   EDIT-CURSOR  OF  R20002
         ELSE
             PERFORM    TEN2-SET-SEC
         END-IF
     END-IF.
*
     IF     ERR-FLG    =     ZERO
            MOVE     8              TO   SYORI-FLG
            PERFORM         HEAD-CLR2-SEC
     END-IF.
 HEAD2-CHK-EXIT.
     EXIT.
*----------------------------------------------------------*
*      2.7.1.1   店舗情報ＳＥＴ   （単一）                 *
*----------------------------------------------------------*
 TEN2-SET-SEC             SECTION.
     MOVE     1              TO   M-FLG.
     MOVE     TEN-F781       TO   R200Y1.
     MOVE     TEN-F782       TO   R200Y2.
     MOVE     TEN-F02        TO   R20004.
     MOVE     SPACE          TO   R20005.
     MOVE     TEN-F06        TO   R20006.
     MOVE     TEN-F07        TO   R20007.
     MOVE     TEN-F08        TO   R20008.
 TEN2-SET-EXIT.
     EXIT.
*----------------------------------------------------------*
*      2.7.2.2   得意先情報ＳＥＴ  （単一）                *
*----------------------------------------------------------*
 TOK-SET-SEC             SECTION.
     MOVE     TOK-F801       TO   R200Y1.
     MOVE     TOK-F802       TO   R200Y2.
     MOVE     TOK-F02        TO   R20004.
     MOVE     SPACE          TO   R20005.
     MOVE     TOK-F06        TO   R20006.
     MOVE     TOK-F07        TO   R20007.
     MOVE     TOK-F08        TO   R20008.
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
                 MOVE   "M"     TO   EDIT-OPTION  OF  R20001
                                     EDIT-OPTION  OF  R20002
                 MOVE   " "     TO   EDIT-CURSOR  OF  R20001
                                     EDIT-CURSOR  OF  R20002
                        MOVE   6           TO   SYORI-FLG
         WHEN   "F005"
                        MOVE   "END"       TO   END-FLG
         WHEN   "F006"
                 MOVE   "M"     TO   EDIT-OPTION  OF  R20001
                                     EDIT-OPTION  OF  R20002
                 MOVE   " "     TO   EDIT-CURSOR  OF  R20001
                                     EDIT-CURSOR  OF  R20002
                        PERFORM         HEAD-CLR2-SEC
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
     PERFORM         HEAD-CLR2-SEC.
*個数チェック
     IF  ( R20014  NOT  NUMERIC   )
         IF    ERR-FLG    =  ZERO
               MOVE   7      TO   ERR-FLG

         END-IF
         MOVE   "R"     TO   EDIT-OPTION  OF  R20014
         MOVE   "C"     TO   EDIT-CURSOR  OF  R20014
     ELSE
         IF  R20014    =   ZERO
             IF    ERR-FLG    =  ZERO
*******************MOVE   7      TO   ERR-FLG
                   CONTINUE
             END-IF
*************MOVE   "R"     TO   EDIT-OPTION  OF  R20014
*************MOVE   "C"     TO   EDIT-CURSOR  OF  R20014
         END-IF
     END-IF.
*伝票種類チェック
     IF  ( R20010  NOT  NUMERIC   )
         IF    ERR-FLG    =  ZERO
               MOVE   3      TO   ERR-FLG
         END-IF
         MOVE   "R"     TO   EDIT-OPTION  OF  R20010
         MOVE   "C"     TO   EDIT-CURSOR  OF  R20010
     ELSE
         IF  (  R20010 =  1  OR  2  OR  3  )
             CONTINUE
         ELSE
             IF    ERR-FLG    =  ZERO
                   MOVE   3      TO   ERR-FLG
             END-IF
             MOVE   "R"     TO   EDIT-OPTION  OF  R20010
             MOVE   "C"     TO   EDIT-CURSOR  OF  R20010
         END-IF
     END-IF.
*日付チェック
     IF  ( DATE2 NOT  NUMERIC   )
         MOVE   "R"     TO   EDIT-OPTION  OF  DATE2
         MOVE   "C"     TO   EDIT-CURSOR  OF  DATE2
         IF     ERR-FLG =    ZERO
                MOVE    13   TO   ERR-FLG
         END-IF
     ELSE
         MOVE     "2"                 TO   LINK-IN-KBN
         MOVE     ZERO                TO   LINK-IN-YMD6
         MOVE     DATE2               TO   LINK-IN-YMD8
         MOVE     ZERO                TO   LINK-OUT-RET
         MOVE     ZERO                TO   LINK-OUT-YMD
         CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                           LINK-IN-YMD6
                                           LINK-IN-YMD8
                                           LINK-OUT-RET
                                           LINK-OUT-YMD
         IF   LINK-OUT-RET   NOT = ZERO
              IF    ERR-FLG    =  ZERO
                    MOVE   13      TO   ERR-FLG
              END-IF
              MOVE   "R"     TO   EDIT-OPTION  OF  DATE2
              MOVE   "C"     TO   EDIT-CURSOR  OF  DATE2
         END-IF
     END-IF.
*印刷枚数チェック
     IF  ( R20011  NOT  NUMERIC   )
         IF    ERR-FLG    =  ZERO
               MOVE   8      TO   ERR-FLG
         END-IF
         MOVE   "R"     TO   EDIT-OPTION  OF  R20011
         MOVE   "C"     TO   EDIT-CURSOR  OF  R20011
     ELSE
         IF  R20011    =   ZERO
             IF    ERR-FLG    =  ZERO
                   MOVE   8      TO   ERR-FLG
             END-IF
             MOVE   "R"     TO   EDIT-OPTION  OF  R20011
             MOVE   "C"     TO   EDIT-CURSOR  OF  R20011
         END-IF
     END-IF.
     IF  ( R20012  NOT  NUMERIC   )
         MOVE   0       TO   K-FLG
     ELSE
         MOVE   1       TO   K-FLG
     END-IF.
*発送元印刷チェック
     IF  ( R20013  NOT  NUMERIC   )
         MOVE   0       TO   R20013
     END-IF.
     IF  ( R20013   =  0 OR 1  )
         CONTINUE
     ELSE
         IF    ERR-FLG    =  ZERO
               MOVE   4      TO   ERR-FLG
         END-IF
         MOVE   "R"     TO   EDIT-OPTION  OF  R20013
         MOVE   "C"     TO   EDIT-CURSOR  OF  R20013
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
     MOVE     "Y"            TO   R20015.
     PERFORM       DSP-WRITE-SEC2.
*
     MOVE    "KAKN2"         TO    DSP-GROUP.
     PERFORM       DSP-READ-SEC.
* アテンション判定
     EVALUATE  DSP-FUNC
         WHEN   "F004"
                        MOVE    6      TO   SYORI-FLG
         WHEN   "F005"
                        MOVE  "END"    TO   END-FLG
         WHEN   "F006"
                        MOVE    8      TO   SYORI-FLG
                        MOVE    SPACE  TO   R20015
         WHEN   "E000"
                        IF  R20015  NOT  =  "Y"
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
     MOVE    R20010         TO   OKU-F01.
     MOVE    R20001         TO   OKU-F02.
     IF    M-FLG    =   1
           MOVE    R20002         TO   OKU-F03
     ELSE
           MOVE    ZERO           TO   OKU-F03
     END-IF.
*****MOVE    R20003         TO   OKU-F04.
     MOVE    DATE2(3:6)     TO   OKU-F04.
     MOVE    R200Y1         TO   OKU-F151.
     MOVE    R200Y2         TO   OKU-F152.
     MOVE    R20004         TO   OKU-F05.
     MOVE    R20005         TO   OKU-F06.
     MOVE    R20006         TO   OKU-F07.
     MOVE    R20007         TO   OKU-F08.
     MOVE    R20008         TO   OKU-F09.
     MOVE    R20009         TO   OKU-F10.
     MOVE    R20014         TO   OKU-F11.
     MOVE    R20011         TO   OKU-F12.
     IF    K-FLG    =   1
           MOVE    R20012         TO   OKU-F13
     ELSE
           MOVE    ZERO           TO   OKU-F13
     END-IF.
     MOVE    R20013         TO   OKU-F14.
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
     MOVE   R20001        TO    TEN-F52.
     MOVE   R20002        TO    TEN-F011.
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
     MOVE    "FKR00301"      TO   DSP-FORMAT.
     MOVE    "ALLF1 "        TO   DSP-GROUP.
     MOVE     SPACE          TO   DSP-PROC.
     MOVE     HEN-DATE       TO   SDATE.
     MOVE     HEN-TIME       TO   STIME.
**** TEST
*****DISPLAY  W10201(1) UPON CONS.
*****DISPLAY  W10201(2) UPON CONS.
*
     WRITE    DSP-AREA       FROM FKR00301.
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
     MOVE    "FKR00302"      TO   DSP-FORMAT.
     MOVE    "SCREEN"        TO   DSP-GROUP.
     MOVE     SPACE          TO   DSP-PROC.
*
**** TEST
*****DISPLAY  W10201(1) UPON CONS.
*****DISPLAY  W10201(2) UPON CONS.
     WRITE    DSP-AREA       FROM FKR00302.
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
     IF  DSP-FORMAT  = "FKR00301"
         MOVE    DSP-AREA    TO   FKR00301
     ELSE
         MOVE    DSP-AREA    TO   FKR00302
     END-IF.
 DSP-READ-EXIT.
     EXIT.
*----------------------------------------------------------*
*      2.7       ＨＥＡＤ２処理 （単一）　　               *
*----------------------------------------------------------*
 HEAD-CLR2-SEC        SECTION.
     MOVE   " "     TO   EDIT-CURSOR  OF  R20010
                         EDIT-CURSOR  OF  R20011
                         EDIT-CURSOR  OF  R20012
                         EDIT-CURSOR  OF  R20013
                         EDIT-CURSOR  OF  DATE2
                         EDIT-CURSOR  OF  R20014.
     MOVE   "M"     TO   EDIT-OPTION  OF  R20010
                         EDIT-OPTION  OF  R20011
                         EDIT-OPTION  OF  R20012
                         EDIT-OPTION  OF  R20013
                         EDIT-OPTION  OF  DATE2
                         EDIT-OPTION  OF  R20014.
 HEAD-CLR2-EXIT.
     EXIT.
*****************<<  PROGRAM  END  >>***********************

```
