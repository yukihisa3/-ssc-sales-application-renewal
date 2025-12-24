# SKY9999B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SKY9999B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　共通システム                      *
*    業務名　　　　　　　：　取引先所属部門ＣＤ取得　　        *
*    モジュール名　　　　：　取引先所属部門ＣＤ取得　　        *
*    作成日／作成者　　　：　2012/03/28 TAKAHASHI              *
*    更新日／更新者　　　：　                                  *
*    更新内容　　　　　　：　                                  *
*    再利用ＰＧ　　　　　：　                                  *
*    処理概要　　　　　　：　取引先ＣＤより所属の部門を取得　　*
****************************************************************
 IDENTIFICATION         DIVISION.
 PROGRAM-ID.            SKY9999B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          12/03/28.
****************************************************************
*                                                              *
 ENVIRONMENT            DIVISION.
*                                                              *
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FACOM.
 OBJECT-COMPUTER.       FACOM.
 SPECIAL-NAMES.
         CONSOLE   IS   CONS
            YA     IS   YA.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*    取引先マスタ
     SELECT   HTOKMS             ASSIGN    TO        TOKMS2
                                 ORGANIZATION        INDEXED
                                 ACCESS    MODE      RANDOM
                                 RECORD    KEY       TOK-F01
                                 FILE      STATUS    TOK-ST.
*--------------------------------------------------------------*
 DATA                   DIVISION.
*--------------------------------------------------------------*
 FILE                   SECTION.
*****＜取引先マスタ＞*****
 FD  HTOKMS             BLOCK     CONTAINS   8   RECORDS
                        LABEL     RECORD     IS  STANDARD.
                        COPY      HTOKMS     OF  XFDLIB
                        JOINING   TOK        AS  PREFIX.
*----------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
***  ｽﾃｰﾀｽ ｴﾘｱ
 01  STATUS-AREA.
     03  TOK-ST                   PIC  X(02).
***  ﾌﾗｸﾞ ｴﾘｱ
 01  FLG-AREA.
     03  END-FLG                  PIC  X(03)     VALUE   ZERO.
     03  ERR-FLG                  PIC  9(01)     VALUE   ZERO.
     03  SYORI-FLG                PIC  X(03)     VALUE   SPACE.
     03  CHK-FLG                  PIC  X(03)     VALUE   SPACE.
*日付変換
 01  WK-TOK-F76                   PIC  9(08)     VALUE   ZERO.
*
 01  FILE-ERR050                  PIC  N(11)     VALUE
         NC"得意先マスタ　異常！！".
*日付
 01  DATE-AREA.
     03  WK-YS                    PIC  9(02)  VALUE  ZERO.
     03  WK-DATE.
         05  WK-Y                 PIC  9(02)  VALUE  ZERO.
         05  WK-M                 PIC  9(02)  VALUE  ZERO.
         05  WK-D                 PIC  9(02)  VALUE  ZERO.
 01  DATE-AREAR2       REDEFINES      DATE-AREA.
     03  SYS-DATE                 PIC  9(08).
*
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
***** メッセージエリア
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SKY9999B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SKY9999B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND1.
         05  FILLER               PIC       X(04)  VALUE
                       "### ".
         05  ERR-PG-ID            PIC       X(08)  VALUE
                       "SSY3776L".
         05  FILLER               PIC       X(10)  VALUE
                       " ABEND ###".
*
     03  MSG-ABEND2.
         05  FILLER               PIC       X(04)  VALUE
                       "### ".
         05  ERR-FL-ID            PIC       X(08).
         05  FILLER               PIC       X(04)  VALUE
                       " ST-".
         05  ERR-STCD             PIC       X(02).
         05  FILLER               PIC       X(04)  VALUE
                       " ###".
*
     03  SEC-NAME.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(07)  VALUE " SEC = ".
         05  S-NAME         PIC   X(30).
     03  MSG-IN.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(09)  VALUE " INPUT = ".
         05  IN-CNT         PIC   9(06).
         05  FILLER         PIC   X(05)  VALUE " *** ".
     03  MSG-OUT.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(09)  VALUE " OUTPG= ".
         05  OUT-CNT        PIC   9(06).
         05  FILLER         PIC   X(05)  VALUE " *** ".
*
 LINKAGE                SECTION.
 01  PARA-IN-TOKCD         PIC 9(08).
 01  PARA-OUT-BUMON        PIC X(04).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION  USING  PARA-IN-TOKCD
                                         PARA-OUT-BUMON.
******************************************************************
*                       SHORI                         0.0.0      *
******************************************************************
 DECLARATIVES.
*--- << 取引先マスタ >> ---*
 FILEERROR-SEC1         SECTION.
     USE AFTER          EXCEPTION
                        PROCEDURE HTOKMS.
     MOVE     "TOKMS2  "          TO        ERR-FL-ID.
     MOVE     TOK-ST              TO        ERR-STCD.
     DISPLAY  MSG-ABEND1          UPON      CONS.
     DISPLAY  MSG-ABEND2          UPON      CONS.
     DISPLAY  SEC-NAME            UPON      CONS.
     STOP     RUN.
 END DECLARATIVES.
******************************************************************
*            M  A  I  N          M  O  D  U  L  E                *
******************************************************************
 CONTROL-START          SECTION.
*
     PERFORM            INIT-SEC.
     PERFORM            MAIN-SEC.
     PERFORM            END-SEC.
*
 CONTROL-END.
     STOP     RUN.
****************************************************************
*             初期処理                              1.0        *
****************************************************************
 INIT-SEC               SECTION.
*    ファイルのＯＰＥＮ
     OPEN     INPUT     HTOKMS.
*システム日付・時刻の取得
     ACCEPT   WK-DATE          FROM   DATE.
     MOVE     "3"                TO   LINK-IN-KBN.
     MOVE     WK-DATE            TO   LINK-IN-YMD6.
     MOVE     ZERO               TO   LINK-IN-YMD8.
     MOVE     ZERO               TO   LINK-OUT-RET.
     MOVE     ZERO               TO   LINK-OUT-YMD.
     CALL     "SKYDTCKB"      USING   LINK-IN-KBN
                                      LINK-IN-YMD6
                                      LINK-IN-YMD8
                                      LINK-OUT-RET
                                      LINK-OUT-YMD.
     MOVE     LINK-OUT-YMD       TO   DATE-AREA.
*取得対象取引先ＣＤ表示
     DISPLAY NC"取得対象取引先＝" PARA-IN-TOKCD UPON CONS.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*             メイン処理                              2.0      *
****************************************************************
 MAIN-SEC               SECTION.
*****取引先マスタ検索*****
     MOVE     PARA-IN-TOKCD       TO  TOK-F01.
     READ     HTOKMS
        INVALID
              MOVE  "2920"        TO  PARA-OUT-BUMON
              GO                  TO  MAIN010
     END-READ.
*
*****DISPLAY "TOK-F75 = " TOK-F75 UPON CONS.
*****DISPLAY "TOK-F76 = " TOK-F76 UPON CONS.
*****DISPLAY "TOK-F77 = " TOK-F77 UPON CONS.
*部門ＣＤ判定
     IF  TOK-F76  NUMERIC
     AND TOK-F76  NOT = ZERO
         MOVE  WK-TOK-F76         TO  WK-TOK-F76
         IF  WK-TOK-F76 < SYS-DATE
             MOVE TOK-F77         TO  PARA-OUT-BUMON
         ELSE
             MOVE TOK-F75         TO  PARA-OUT-BUMON
         END-IF
     ELSE
         MOVE  TOK-F75            TO  PARA-OUT-BUMON
     END-IF.
*
 MAIN010.
*
     DISPLAY NC"＃＃対象部門＝" PARA-OUT-BUMON UPON CONS.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*             終了処理                              3.0        *
****************************************************************
 END-SEC                SECTION.
*    ファイルのＣＬＯＳＥ
     CLOSE    HTOKMS.
*
 END-EXIT.
     EXIT.
******************************************************************
 END PROGRAM  SKY9999B.
******************************************************************

```
