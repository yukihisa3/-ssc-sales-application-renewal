# SSK0011B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SSK0011B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　ケーヨー伝票レスシステム　　　　　*
*    業務名　　　　　　　：　オンライン業務　　　　　　　　　　*
*    モジュール名　　　　：　受領返品データ累積処理　　　　　　*
*    作成日／更新日　　　：　2014/03/25                        *
*    作成者／更新者　　　：　NAV TAKAHASHI                     *
*    処理概要　　　　　　：　パラメタを受取り、受領累積データ  *
*                            より返品データを抽出し、受領返品　*
*                            累積データへの抽出を行なう。      *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SSK0011B.
 AUTHOR.                NAV TAKAHASHI.
 DATE-WRITTEN.          2014/03/25.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*商品変換テーブル
     SELECT   HSHOTBL   ASSIGN    TO        DA-01-VI-SHOTBL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       TBL-F01   TBL-F02
                        FILE STATUS    IS   TBL-STATUS.
*受領受信状況データ
     SELECT   KEIJYOF   ASSIGN    TO        DA-01-VI-KEIJYOL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       JYO-F01   JYO-F02
                                            JYO-F03   JYO-F04
                        FILE STATUS    IS   JYO-STATUS.
*受領累積データ
     SELECT   KEIJYRF   ASSIGN    TO        DA-01-VI-KEIJYRL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       JYR-F01   JYR-F02
                                            JYR-F03   JYR-F04
                                            JYR-F05   JYR-F06
                                            JYR-F08   JYR-F10
                        FILE  STATUS   IS   JYR-STATUS.
*受領返品データ
     SELECT   KEIJHRF   ASSIGN    TO        DA-01-VI-KEIJHRL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       JHR-F01   JHR-F02
                                            JHR-F03   JHR-F04
                                            JHR-F05
                        FILE  STATUS   IS   JHR-STATUS.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    商品変換テーブル
******************************************************************
 FD  HSHOTBL            LABEL RECORD   IS   STANDARD.
     COPY     HSHOTBL   OF        XFDLIB
              JOINING   TBL       PREFIX.
******************************************************************
*    受領受信状況データ
******************************************************************
 FD  KEIJYOF            LABEL RECORD   IS   STANDARD.
     COPY     KEIJYOF   OF        XFDLIB
              JOINING   JYO       PREFIX.
******************************************************************
*    受領累積データ
******************************************************************
 FD  KEIJYRF            LABEL RECORD   IS   STANDARD.
     COPY     KEIJYRF   OF        XFDLIB
              JOINING   JYR       PREFIX.
******************************************************************
*    受領返品データ
******************************************************************
 FD  KEIJHRF            LABEL RECORD   IS   STANDARD.
     COPY     KEIJHRF   OF        XFDLIB
              JOINING   JHR       PREFIX.
*
*****************************************************************
*
 WORKING-STORAGE        SECTION.
*    ｶｳﾝﾄ
 01  END-FG                  PIC  9(01)     VALUE  ZERO.
 01  IDX                     PIC  9(02)     VALUE  ZERO.
 01  RD-CNT                  PIC  9(08)     VALUE  ZERO.
 01  WRT-CNT                 PIC  9(08)     VALUE  ZERO.
 01  NG-CNT                  PIC  9(08)     VALUE  ZERO.
*
 01  WK-DENKU                PIC  X(02)     VALUE  SPACE.
 01  KEIJHRF-INV-FLG         PIC  X(03)     VALUE  SPACE.
 01  HSHOTBL-INV-FLG         PIC  X(03)     VALUE  SPACE.
*
 01  WK-AREA.
*システム日付の編集
     03  SYS-DATE          PIC 9(06).
     03  SYS-DATEW         PIC 9(08).
 01  WK-ST.
     03  TBL-STATUS        PIC  X(02).
     03  JYO-STATUS        PIC  X(02).
     03  JYR-STATUS        PIC  X(02).
     03  JHR-STATUS        PIC  X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SSK0011B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSK0011B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSK0011B".
         05  FILLER         PIC   X(11)  VALUE
                                         " ABEND *** ".
     03  ABEND-FILE.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  AB-FILE        PIC   X(08).
         05  FILLER         PIC   X(06)  VALUE " ST = ".
         05  AB-STS         PIC   X(02).
         05  FILLER         PIC   X(05)  VALUE " *** ".
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
         05  FILLER         PIC   X(09)  VALUE " OUTPUT= ".
         05  OUT-CNT        PIC   9(06).
         05  FILLER         PIC   X(05)  VALUE " *** ".
     03  MSG-NG.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(09)  VALUE " NG-CNT= ".
         05  CNT-NG         PIC   9(06).
         05  FILLER         PIC   X(05)  VALUE " *** ".
*時刻
 01  TIME-AREA.
     03  WK-TIME                  PIC  9(08)  VALUE  ZERO.
*
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
*
 LINKAGE                SECTION.
 01  PARA-AREA.
     03  PARA-JDATE         PIC   9(08).
     03  PARA-JTIME         PIC   9(04).
     03  PARA-KSYU          PIC   X(01).
     03  PARA-YUSEN         PIC   X(01).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION USING PARA-AREA.
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   KEIJYRF.
     MOVE      "KEIJYRL1"   TO   AB-FILE.
     MOVE      JYR-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   HSHOTBL.
     MOVE      "SHOTBL1 "   TO   AB-FILE.
     MOVE      TBL-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC3           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   KEIJYOF.
     MOVE      "KEIJYOL1"   TO   AB-FILE.
     MOVE      JYO-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC4           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   KEIJHRF.
     MOVE      "KEIJHRL1"   TO   AB-FILE.
     MOVE      JHR-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 END     DECLARATIVES.
*****************************************************************
*                                                                *
******************************************************************
 GENERAL-PROCESS       SECTION.
*
     MOVE     "PROCESS-START"     TO   S-NAME.
     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC
              UNTIL     END-FG    =    9.
     PERFORM  END-SEC.
*
****************************************************************
*　　　　　　　初期処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-SEC               SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
     OPEN     INPUT     HSHOTBL.
     OPEN     I-O       KEIJYRF.
     OPEN     I-O       KEIJYOF.
     OPEN     I-O       KEIJHRF.
     DISPLAY  MSG-START UPON CONS.
*
     MOVE     ZERO      TO        END-FG    RD-CNT    WRT-CNT.
     MOVE     ZERO      TO        IN-CNT    OUT-CNT.
*
******************
*システム日付編集*
******************
     ACCEPT      SYS-DATE  FROM      DATE.
     MOVE       "3"        TO        LINK-IN-KBN.
     MOVE        SYS-DATE  TO        LINK-IN-YMD6.
     CALL       "SKYDTCKB"   USING   LINK-IN-KBN
                                     LINK-IN-YMD6
                                     LINK-IN-YMD8
                                     LINK-OUT-RET
                                     LINK-OUT-YMD8.
     IF          LINK-OUT-RET   =    ZERO
         MOVE    LINK-OUT-YMD8  TO   SYS-DATEW
     ELSE
         MOVE    ZERO           TO   SYS-DATEW
     END-IF.
*システム時間取得
     ACCEPT    WK-TIME          FROM   TIME.
*受領累積データスタート
     MOVE      SPACE            TO     JYR-REC.
     INITIALIZE                        JYR-REC.
     MOVE      PARA-JDATE       TO     JYR-F01.
     MOVE      PARA-JTIME       TO     JYR-F02.
     MOVE      173              TO     JYR-F03.
     START  KEIJYRF  KEY  IS  >=  JYR-F01  JYR-F02  JYR-F03
                                  JYR-F04  JYR-F05  JYR-F06
                                  JYR-F08  JYR-F10
            INVALID  MOVE   9   TO     END-FG
                     DISPLAY NC"＃＃対象データ無１！！"
                             UPON  CONS
                     GO         TO     INIT-EXIT
     END-START.
*受領累積データ読込
     PERFORM  KEIJYRF-READ-SEC.
     IF  END-FG = 9
         DISPLAY NC"＃＃対象データ無２！！" UPON CONS
     END-IF.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"          TO   S-NAME.
*返品累積データ作成
     PERFORM  EDIT-SEC.
*
     PERFORM KEIJYRF-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*　　　　　　　ファイル出力　　　　　　　　　　　　　　　　　　*
****************************************************************
 EDIT-SEC              SECTION.
*
     MOVE    "EDIT-SEC"     TO        S-NAME.
*
     MOVE     SPACE         TO        JHR-REC.
     INITIALIZE                       JHR-REC.
*    取引先ＣＤ
     MOVE     JYR-F03       TO        JHR-F01.
*    店舗ＣＤ
     MOVE     JYR-F04       TO        JHR-F02.
*    検収日
     MOVE     JYR-F10       TO        JHR-F03.
*    伝票番号
     MOVE     JYR-F05       TO        JHR-F04.
*    行番号
     MOVE     JYR-F06       TO        JHR-F05.
*    伝票区分（サカタ）
     MOVE     JYR-F07       TO        JHR-F06.
     MOVE     JYR-F07       TO        WK-DENKU.
*    検収日（サカタ）画面より入力される項目
     MOVE     JYR-F10       TO        JHR-F07.
*    出荷場所
     MOVE     JYR-F27       TO        JHR-F08.
*    ＪＡＮＣＤ
     MOVE     JYR-F11       TO        JHR-F09.
*    商品変換ＴＢＬを索引し、_番を取得する
     MOVE     173           TO        TBL-F01.
     MOVE     JYR-F11       TO        TBL-F02.
     PERFORM HSHOTBL-READ-SEC.
     IF  HSHOTBL-INV-FLG  =  SPACE
         MOVE TBL-F08       TO        JHR-F26
     END-IF.
*    サカタ商品ＣＤ＋品単ＣＤ
     MOVE     JYR-F16       TO        JHR-F10.
     MOVE     JYR-F17       TO        JHR-F11.
     MOVE     JYR-F18       TO        JHR-F12.
     MOVE     JYR-F19       TO        JHR-F13.
*    商品名カナ
     MOVE     JYR-F20       TO        JHR-F14.
     MOVE     JYR-F21       TO        JHR-F15.
*    返品数量
     MOVE     JYR-F12       TO        JHR-F16.
*    原価単価
     MOVE     JYR-F13       TO        JHR-F17.
*    原価金額
     MOVE     JYR-F14       TO        JHR-F18.
*    エラーセット
     MOVE     JYR-F83       TO        JHR-F87.
     MOVE     JYR-F85       TO        JHR-F88.
*    登録情報／更新情報
     MOVE     "2920"        TO        JHR-F94.
     MOVE     "99"          TO        JHR-F95.
     MOVE     SYS-DATEW     TO        JHR-F92.
     MOVE     WK-TIME(1:6)  TO        JHR-F93.
*    受信日／受信時刻セット
     MOVE     JYR-F01       TO        JHR-F78.
     MOVE     JYR-F02       TO        JHR-F79.
*
     PERFORM KEIJHRF-READ-SEC.
     IF  KEIJHRF-INV-FLG  =  "INV"
*        受領累積データが存在しない場合、出力する。
         WRITE  JHR-REC
*        受領受信状況データ出力
         PERFORM KEIJYOF-WRT-SEC
*        返品累積データ作成ＦＬＧ／作成日更新
         MOVE   "1"         TO        JYR-F94
         MOVE    SYS-DATEW  TO        JYR-F95
         REWRITE JYR-REC
         ADD      1         TO        WRT-CNT
     ELSE
         ADD      1         TO        NG-CNT
     END-IF.
*
 EDIT-EXIT.
     EXIT.
****************************************************************
*　　受領受信状況データ出力
****************************************************************
 KEIJYOF-WRT-SEC         SECTION.
*
     MOVE   "KEIJYOF-WRT-SEC"   TO   S-NAME.
     MOVE    SPACE         TO        JYO-REC.
     INITIALIZE                      JYO-REC.
     MOVE    PARA-JDATE    TO        JYO-F01.
     MOVE    PARA-JTIME    TO        JYO-F02.
     MOVE    173           TO        JYO-F03.
     MOVE    WK-DENKU      TO        JYO-F04.
     READ    KEIJYOF
       INVALID
         CONTINUE
       NOT INVALID
         GO  TO   KEIJYOF-010
     END-READ.
*
     MOVE    SPACE         TO        JYO-REC.
     INITIALIZE                      JYO-REC.
     MOVE    PARA-JDATE    TO        JYO-F01.
     MOVE    PARA-JTIME    TO        JYO-F02.
     MOVE    173           TO        JYO-F03.
     MOVE    WK-DENKU      TO        JYO-F04.
*    伝票区分（サカタ）
     IF  WK-DENKU  =  "07"
         MOVE "41"         TO        JYO-F05
     ELSE
         MOVE "40"         TO        JYO-F05
     END-IF.
     ADD     1             TO        JYO-F07.
     MOVE    SYS-DATEW     TO        JYO-F98.
     MOVE    WK-TIME(1:6)  TO        JYO-F99.
     WRITE   JYO-REC.
     GO      TO   KEIJYOF-WRT-EXIT.
*
 KEIJYOF-010.
*
     ADD     1             TO        JYO-F07.
     REWRITE JYO-REC.
*
 KEIJYOF-WRT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
*
     MOVE      RD-CNT    TO      IN-CNT.
     MOVE      WRT-CNT   TO      OUT-CNT.
     MOVE      NG-CNT    TO      CNT-NG.
     DISPLAY   MSG-IN    UPON CONS.
     DISPLAY   MSG-OUT   UPON CONS.
     DISPLAY   MSG-NG    UPON CONS.
     DISPLAY   MSG-END   UPON CONS.
*
     CLOSE     KEIJYRF  HSHOTBL  KEIJYOF  KEIJHRF.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
****************************************************************
*　　返品累積データ読込（存在チェック）
****************************************************************
 KEIJHRF-READ-SEC        SECTION.
*
     MOVE   "KEIJYRF-READ-SEC"  TO   S-NAME.
*
     READ  KEIJHRF
           INVALID     MOVE  "INV"  TO  KEIJHRF-INV-FLG
           NOT INVALID MOVE  SPACE  TO  KEIJHRF-INV-FLG
     END-READ.
*
 KEIJHRF-READ-EXIT.
     EXIT.
****************************************************************
*　　商品変換ＴＢＬ読込
****************************************************************
 HSHOTBL-READ-SEC        SECTION.
*
     MOVE   "HSHOTBL-READ-SEC"  TO   S-NAME.
*
     READ  HSHOTBL
           INVALID     MOVE  "INV"  TO  HSHOTBL-INV-FLG
           NOT INVALID MOVE  SPACE  TO  HSHOTBL-INV-FLG
     END-READ.
*
 HSHOTBL-READ-EXIT.
     EXIT.
****************************************************************
*　　受領累積データ読込
****************************************************************
 KEIJYRF-READ-SEC        SECTION.
*
     MOVE   "KEIJYRF-READ-SEC"  TO   S-NAME.
*
     READ  KEIJYRF
           AT  END     MOVE  9      TO  END-FG
                       GO           TO  KEIJYRF-READ-EXIT
     END-READ.
*処理件数カウント／画面表示
     ADD   1                    TO   RD-CNT.
     IF  RD-CNT(6:3)  =  "000" OR "500"
         DISPLAY "READ-CNT = " RD-CNT UPON  CONS
     END-IF.
*バッチ番号チェック
     IF  PARA-JDATE  =  JYR-F01
     AND PARA-JTIME  =  JYR-F02
     AND JYR-F03     =  173
         CONTINUE
     ELSE
         GO                     TO   KEIJYRF-READ-SEC
     END-IF.
*伝票区分＝０７のみを対象とする。
     IF  JYR-F07  NOT =  "07"
         GO                     TO   KEIJYRF-READ-SEC
     END-IF.
*返品伝票データ作成区分が１の場合
     IF  JYR-F94  =  "1"
         GO                     TO   KEIJYRF-READ-SEC
     END-IF.
*
 KEIJYRF-READ-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*

```
