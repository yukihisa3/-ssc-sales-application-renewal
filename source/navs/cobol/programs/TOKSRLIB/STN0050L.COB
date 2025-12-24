****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　_卸ローカル運用　　　　　　　　　*
*    モジュール名　　　　：　_卸予定確認リスト発行            *
*    作成日／作成者　　　：　2021/03/15 INOUE                  *
*    更新日／更新者　　　：　                                  *
*    処理概要　　　　　　：　_卸予定確認用リストの発行　　　　*
*                                                              *
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            STN0050L.
*                  流用:STA0040L.
 AUTHOR.                NAV.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FACOM-K150.
 OBJECT-COMPUTER.       FACOM-K150.
 SPECIAL-NAMES.
         STATION   IS   STAT
         YA        IS   NIHONGO
         YB        IS   YB1
*        YB-21     IS   YB2
         YB-22     IS   YB2
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
****<< 予定確認リストデータ  >>
     SELECT   YTTANAW   ASSIGN  TO   DA-01-VI-YTTANAW1
                        ORGANIZATION         IS   INDEXED
                        ACCESS  MODE         IS   SEQUENTIAL
                        RECORD  KEY          IS   TANA-F83
                                                  TANA-F04
                                                  TANA-F01
                                                  TANA-F15
                                                  TANA-F88
                        FILE    STATUS       IS   TANA-STATUS.
****<< 倉庫マスタ           >>******************************
     SELECT   ZSOKMS    ASSIGN  TO   DA-01-VI-ZSOKMS1
                        ORGANIZATION         IS   INDEXED
                        ACCESS  MODE         IS   RANDOM
                        RECORD  KEY          IS   SOK-F01
                        FILE    STATUS       IS   SOK-STATUS.
****<< SUB商品名称マスタ       >>******************************
     SELECT   SUBMEIF   ASSIGN  TO   DA-01-VI-SUBMEIL1
                        ORGANIZATION         IS   INDEXED
                        ACCESS  MODE         IS   RANDOM
                        RECORD  KEY          IS   MEI-F011
                                                  MEI-F0121
                                                  MEI-F0122
                                                  MEI-F0123
                        FILE    STATUS       IS   MEI-STATUS.
****<<  プリントファイル    >>******************************
     SELECT   PRINTF    ASSIGN  TO   LP-04.
*----------------------------------------------------------
*----------------------------------------------------------
 DATA                   DIVISION.
 FILE                   SECTION.
****<< _卸予定確認リストデータ   >>
 FD    YTTANAW
       LABEL     RECORD    IS    STANDARD.
       COPY      YTTANAW   OF    XFDLIB
                 JOINING   TANA  PREFIX.
*
****<< 倉庫マスタ           >>******************************
 FD    ZSOKMS
       LABEL     RECORD    IS    STANDARD.
       COPY      ZSOKMS    OF    XFDLIB
                 JOINING   SOK   PREFIX.
****<< 商品名称マスタ       >>******************************
 FD    SUBMEIF
       LABEL     RECORD    IS    STANDARD.
       COPY      SUBMEIF    OF    XFDLIB
                 JOINING   MEI   PREFIX.
****<<  プリントファイル  >>********************************
 FD    PRINTF    LINAGE  IS  66.
 01    P-REC                 PIC  X(200).
*
****  作業領域  ********************************************
 WORKING-STORAGE        SECTION.
****  ステイタス情報          ****
 01  STATUS-AREA.
     02 TANA-STATUS          PIC  X(02).
     02 SOK-STATUS           PIC  X(02).
     02 MEI-STATUS           PIC  X(02).
     02 JYO-STATUS           PIC  X(02).
****  フラグ                  ****
 01  PSW-AREA.
     02  END-FLG             PIC  X(03)  VALUE  SPACE.
**** 日付／時刻 ****
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
**** 画面表示日付編集 ****
 01  HEN-DATE.
     03  HEN-DATE-YYYY            PIC  9(04)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  "/".
     03  HEN-DATE-MM              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  "/".
     03  HEN-DATE-DD              PIC  9(02)  VALUE  ZERO.
**** 画面表示時刻編集 ****
 01  HEN-TIME.
     03  HEN-TIME-HH              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  ":".
     03  HEN-TIME-MM              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  ":".
     03  HEN-TIME-SS              PIC  9(02)  VALUE  ZERO.
****  カウンタ                ****
 01  CNT-AREA.
     02  L-CNT               PIC  9(02)  VALUE  99.
     02  P-CNT               PIC  9(07)  VALUE  ZERO.
     02  READ-CNT            PIC  9(07)  VALUE  ZERO.
     02  WRITE-CNT           PIC  9(07)  VALUE  ZERO.
     02  SKIP-CNT            PIC  9(07)  VALUE  ZERO.
****  ワーク                  ****
 01  WK-SOKO                 PIC  X(02)  VALUE  SPACE.
 01  WK-TDATE                PIC  9(08)  VALUE  ZERO.
 01  WORK-AREA.
     02  WK-SHOCD            PIC  X(08)  VALUE  SPACE.
     02  WK-HINTAN           PIC  X(08)  VALUE  SPACE.
     02  WK-SHOMEI1          PIC  N(15)  VALUE  SPACE.
     02  WK-SHOMEI2          PIC  N(15)  VALUE  SPACE.
     02  WK-JYO-SOKCD        PIC  X(02).
     02  WK-DAISOKCD         PIC  X(02).
****  見出し行0             ****
 01  MIDASI-0.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  FILLER              PIC  X(08)  VALUE  "STN0050L".
     02  FILLER              PIC  X(105) VALUE  SPACE.
     02  H-YY                PIC  9(04).
     02  FILLER              PIC  N(01)  VALUE  NC"年"
                             CHARACTER   TYPE   IS   NIHONGO.
     02  H-MM                PIC  Z9.
     02  FILLER              PIC  N(01)  VALUE  NC"月"
                             CHARACTER   TYPE   IS   NIHONGO.
     02  H-DD                PIC  Z9.
     02  FILLER              PIC  N(01)  VALUE  NC"日"
                             CHARACTER   TYPE   IS   NIHONGO.
     02  FILLER              PIC  X(03)  VALUE  SPACE.
     02  H-PAGE              PIC  ZZZ9   VALUE  ZERO.
     02  FILLER              PIC  N(01)  VALUE  NC"頁"
                             CHARACTER   TYPE   IS   NIHONGO.
****  見出し行１             ****
 01  MIDASI-1.
     02  FILLER              PIC  X(50)  VALUE  SPACE.
     02  FILLER              PIC  N(13)  VALUE
         NC"＜　_卸予定確認リスト　＞"
                             CHARACTER   TYPE   IS   YB2.
     02  FILLER              PIC  X(27)  VALUE  SPACE.
     02  H-TIMEH             PIC  9(02).
     02  FILLER              PIC  N(01)  VALUE  NC"："
                             CHARACTER   TYPE   IS   NIHONGO.
     02  H-TIMEM             PIC  9(02).
     02  FILLER              PIC  N(01)  VALUE  NC"："
                             CHARACTER   TYPE   IS   NIHONGO.
     02  H-TIMES             PIC  9(02).
****  見出し行２             ****
 01  MIDASI-2       CHARACTER     TYPE   IS   NIHONGO.
     02  FILLER              PIC  X(02)  VALUE  SPACE.
     02  FILLER              PIC  N(05)  VALUE  NC"_卸予定日".
     02  FILLER              PIC  N(01)  VALUE  NC"：".
     02  H-TDATEY            PIC  9(04).
     02  FILLER              PIC  N(01)  VALUE  NC"年".
     02  H-TDATEM            PIC  Z9.
     02  FILLER              PIC  N(01)  VALUE  NC"月".
     02  H-TDATED            PIC  Z9.
     02  FILLER              PIC  N(01)  VALUE  NC"日".
     02  FILLER              PIC  X(02)  VALUE  SPACE.
     02  FILLER              PIC  N(02)  VALUE  NC"倉庫".
     02  FILLER              PIC  X(01)  VALUE  ":".
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  H-SOKOCD            PIC  X(02).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  H-SOKOMEI           PIC  N(18).
     02  FILLER              PIC  X(38)  VALUE  SPACE.
****  線
 01  SEN.
     02  FILLER              PIC  X(136) VALUE  ALL "-".
****  見出し行３             ****
 01  MIDASI-3       CHARACTER     TYPE   IS   NIHONGO.
     02  FILLER              PIC  X(02)  VALUE  SPACE.
*    02  FILLER              PIC  N(01)  VALUE  NC"削".
*    02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  FILLER              PIC  N(03)  VALUE  NC"原票_".
     02  FILLER              PIC  X(03)  VALUE  SPACE.
     02  FILLER              PIC  N(02)  VALUE  NC"_番".
     02  FILLER              PIC  X(05)  VALUE  SPACE.
     02  FILLER              PIC  N(05)  VALUE  NC"ＪＡＮＣＤ".
     02  FILLER              PIC  X(05)  VALUE  SPACE.
     02  FILLER              PIC  N(04)  VALUE  NC"商品ＣＤ".
     02  FILLER              PIC  X(02)  VALUE  SPACE.
     02  FILLER              PIC  N(04)  VALUE  NC"品単ＣＤ".
     02  FILLER              PIC  X(04)  VALUE  SPACE.
*    02  FILLER              PIC  X(04)  VALUE  "STNO".
*    02  FILLER              PIC  X(02)  VALUE  SPACE.
     02  FILLER              PIC  N(05)  VALUE  NC"商　品　名".
     02  FILLER              PIC  X(36)  VALUE  SPACE.
     02  FILLER              PIC  N(05)  VALUE  NC"帳簿在庫数".
*    02  FILLER              PIC  X(07)  VALUE  SPACE.
*    02  FILLER              PIC  N(03)  VALUE  NC"単　価".
*    02  FILLER              PIC  X(03)  VALUE  SPACE.
*    02  FILLER              PIC  N(02)  VALUE  NC"年度".
*    02  FILLER              PIC  X(01)  VALUE  SPACE.
*    02  FILLER              PIC  N(03)  VALUE  NC"備　考".
*    02  FILLER              PIC  X(06)  VALUE  SPACE.
****  明細行１               ****
 01  MEISAI-1.
     02  FILLER              PIC  X(02)  VALUE  SPACE.
*    02  M-SAKUJYO           PIC  N(01)
*                            CHARACTER   TYPE   IS   NIHONGO.
*    02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  M-GENPYOU1          PIC  9(06).
     02  FILLER              PIC  X(01)  VALUE  "-".
     02  M-GENPYOU2          PIC  9(01).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  M-TANABAN1          PIC  X(01).
     02  FILLER              PIC  X(01)  VALUE  "-".
     02  M-TANABAN2          PIC  X(03).
     02  FILLER              PIC  X(01)  VALUE  "-".
     02  M-TANABAN3          PIC  X(02).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  M-JANCD             PIC  X(13).
     02  FILLER              PIC  X(02)  VALUE  SPACE.
     02  M-SHOCD             PIC  X(08).
     02  FILLER              PIC  X(02)  VALUE  SPACE.
     02  M-HINTAN1           PIC  X(05).
     02  FILLER              PIC  X(01)  VALUE  "-".
     02  M-HINTAN2           PIC  X(02).
     02  FILLER              PIC  X(01)  VALUE  "-".
     02  M-HINTAN3           PIC  X(01).
     02  FILLER              PIC  X(02)  VALUE  SPACE.
*    02  M-STOCK             PIC  X(05).
*    02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  M-SHOMEI            PIC  N(30)
                             CHARACTER   TYPE   IS   YB1.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  M-CYOUBO            PIC  --,---,--9.
*    02  FILLER              PIC  X(01)  VALUE  SPACE.
*    02  M-TANKA             PIC  Z,ZZZ,ZZ9.99.
*    02  FILLER              PIC  X(01)  VALUE  SPACE.
*    02  M-NENDO             PIC  99.
*    02  FILLER              PIC  X(02)  VALUE  SPACE.
*    02  M-BIKOU             PIC  X(10).
*    02  FILLER              PIC  X(02)  VALUE  SPACE.
**** メッセージ情報           ****
 01  MSG-AREA1-1.
     02  MSG-ABEND1.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-PG-ID         PIC  X(08)  VALUE  "STN0050L".
       03  FILLER            PIC  X(10)  VALUE
          " ABEND ###".
     02  MSG-ABEND2.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-FL-ID         PIC  X(08).
       03  FILLER            PIC  X(04)  VALUE  " ST-".
       03  ERR-STCD          PIC  X(02).
       03  FILLER            PIC  X(04)  VALUE  " ###".
****  エラーメッセージコード  ***
 01  CODE-AREA.
     02  ERR-CD              PIC  9(02)  VALUE  ZERO.
****  エラーメッセージ　ＦＯＲ　コンソール  ***
 01  FILE-ERROR.
     02  FILE-ERR1           PIC  N(10)  VALUE
            NC"リストデータ　異常！".
     02  FILE-ERR2           PIC  N(10)  VALUE
            NC"倉庫マスタ　　異常！".
     02  FILE-ERR3           PIC  N(10)  VALUE
            NC"商品名称マスタ異常！".
     02  FILE-ERR4           PIC  N(10)  VALUE
            NC"条件ファイル　異常！".
     02  FILE-ERR5           PIC  N(10)  VALUE
            NC"画面ファイル　異常！".
**** 日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
**** パラメータ               ****
*LINKAGE                SECTION.
*01  PARA-AREA.
*    02    PARA-WKSTN        PIC  X(08).
************************************************************
*             ＭＡＩＮ         ＭＯＤＵＬＥ                *
************************************************************
*
*PROCEDURE              DIVISION      USING   PARA-AREA.
 PROCEDURE              DIVISION.
*
 DECLARATIVES.
**予定確認リストデータ
 FILEERR-SEC1           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  YTTANAW.
     MOVE   "YTTANAW "        TO    ERR-FL-ID.
     MOVE    TANA-STATUS      TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
**倉庫ファイル
 FILEERR-SEC2           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  ZSOKMS.
     MOVE   "ZSOKMS  "        TO    ERR-FL-ID.
     MOVE    SOK-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
**商品名称マスタ
 FILEERR-SEC3           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  SUBMEIF.
     MOVE   "SUBMEIF "        TO    ERR-FL-ID.
     MOVE    MEI-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
*
 END     DECLARATIVES.
************************************************************
 STN0050L-START         SECTION.
     PERFORM       INIT-SEC.
     PERFORM       MAIN-SEC
                   UNTIL     END-FLG  =    "END".
     PERFORM       END-SEC.
     STOP     RUN.
 STN0050L-END.
     EXIT.
************************************************************
*      1.0      初期処理                                   *
************************************************************
 INIT-SEC               SECTION.
     OPEN     INPUT     YTTANAW.
     OPEN     INPUT     ZSOKMS.
     OPEN     INPUT     SUBMEIF.
*    OPEN     INPUT     HJYOKEN.
     OPEN     OUTPUT    PRINTF.
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
     MOVE      SYS-DATE(1:4)      TO   HEN-DATE-YYYY H-YY.
     MOVE      SYS-DATE(5:2)      TO   HEN-DATE-MM   H-MM.
     MOVE      SYS-DATE(7:2)      TO   HEN-DATE-DD   H-DD.
*システム日付取得
     ACCEPT    WK-TIME          FROM   TIME.
*画面表示時刻編集
     MOVE      WK-TIME(1:2)       TO   HEN-TIME-HH   H-TIMEH.
     MOVE      WK-TIME(3:2)       TO   HEN-TIME-MM   H-TIMEM.
     MOVE      WK-TIME(5:2)       TO   HEN-TIME-SS   H-TIMES.
*初期ＲＥＡＤ
     PERFORM    YTTANAW-READ-SEC.
     IF   END-FLG    NOT =   "END"
         MOVE     TANA-F04    TO     WK-SOKO
         MOVE     TANA-F83    TO     WK-TDATE
     END-IF.
*
 INIT-SEC-EXIT.
     EXIT.
************************************************************
*      1.2       リストファイルＲＥＡＤ処理                *
************************************************************
 YTTANAW-READ-SEC       SECTION.
     READ    YTTANAW
       AT  END
           MOVE    "END"       TO   END-FLG
           GO       TO   YTTANAW-READ-SEC-EXIT
     END-READ.
     ADD      1          TO   READ-CNT.
*
*    IF  SOKCD   NOT =  SPACE      AND
*        SOKCD      <   TANA-F04
*          MOVE    "END"       TO   END-FLG
*    END-IF.
 YTTANAW-READ-SEC-EXIT.
     EXIT.
************************************************************
*      2.0       メイン処理                                *
************************************************************
 MAIN-SEC               SECTION.
     PERFORM  HENSYU-SEC.
     PERFORM  YTTANAW-READ-SEC.
 MAIN-SEC-EXIT.
     EXIT.
************************************************************
*      2.1       編集処理                                  *
************************************************************
 HENSYU-SEC             SECTION.
     IF       L-CNT         >=      51
              PERFORM       MIDASI-SEC
              GO TO         HEN09
     END-IF.
*
     IF       READ-CNT      NOT =   ZERO
        IF  ( TANA-F04      NOT =   WK-SOKO  ) OR
            ( TANA-F83      NOT =   WK-TDATE )
              PERFORM       MIDASI-SEC
              MOVE          TANA-F04     TO   WK-SOKO
              MOVE          TANA-F83     TO   WK-TDATE
        END-IF
     END-IF.
*
 HEN09.
     MOVE     TANA-F83(1:4) TO      H-TDATEY.
     MOVE     TANA-F83(5:2) TO      H-TDATEM.
     MOVE     TANA-F83(7:2) TO      H-TDATED.
     MOVE     TANA-F04      TO      H-SOKOCD.
     IF   WK-SHOCD   NOT =  TANA-F05
              OR      WK-HINTAN   NOT =  TANA-F06X
       MOVE   TANA-F05      TO        MEI-F011
       MOVE   TANA-F06      TO        MEI-F0121
       MOVE   TANA-F07      TO        MEI-F0122
       MOVE   TANA-F08      TO        MEI-F0123
       READ   SUBMEIF
           INVALID  KEY
              DISPLAY "商品　＝　" TANA-F05   UPON CONS
              DISPLAY TANA-F06X     UPON CONS
              MOVE    ALL NC"＊"    TO    WK-SHOMEI1
              MOVE    ALL NC"＊"    TO    WK-SHOMEI2
           NOT  INVALID  KEY
              MOVE    MEI-F021      TO    WK-SHOMEI1
              MOVE    MEI-F022      TO    WK-SHOMEI2
              MOVE    TANA-F05      TO    WK-SHOCD
              MOVE    TANA-F06X     TO    WK-HINTAN
       END-READ
     END-IF.
*
     MOVE     SPACE         TO      P-REC.
     INITIALIZE                     P-REC.
*
*    IF    TANA-F99   =   "9"
*      MOVE   NC"＊"        TO      M-SAKUJYO
*    ELSE
*      MOVE   NC"　"        TO      M-SAKUJYO
*    END-IF.
     MOVE     TANA-F01(1:6) TO      M-GENPYOU1.
     MOVE     TANA-F01(7:1) TO      M-GENPYOU2.
     MOVE     TANA-F15(1:1) TO      M-TANABAN1.
     MOVE     TANA-F15(2:3) TO      M-TANABAN2.
     MOVE     TANA-F15(5:2) TO      M-TANABAN3.
     MOVE     TANA-F88      TO      M-JANCD.
     MOVE     TANA-F05      TO      M-SHOCD.
     MOVE     TANA-F06      TO      M-HINTAN1.
     MOVE     TANA-F07      TO      M-HINTAN2.
     MOVE     TANA-F08      TO      M-HINTAN3.
*    MOVE     TANA-F09      TO      M-STOCK.
     MOVE     WK-SHOMEI1    TO      M-SHOMEI(1:15).
     MOVE     WK-SHOMEI2    TO      M-SHOMEI(16:15).
     MOVE     TANA-F16      TO      M-CYOUBO.
*    MOVE     TANA-F12      TO      M-TANKA.
*    MOVE     TANA-F13      TO      M-NENDO.
*    MOVE     TANA-F14      TO      M-BIKOU.
*    WRITE    P-REC         FROM    MEISAI-1    AFTER  2.
     WRITE    P-REC         FROM    MEISAI-1    AFTER  1.
*    ADD      2             TO      L-CNT.
     ADD      1             TO      L-CNT.
     ADD      1             TO      WRITE-CNT.
*
 HENSYU-SEC-EXIT.
     EXIT.
************************************************************
*      2.1.1       見出し処理                              *
************************************************************
 MIDASI-SEC             SECTION.
     MOVE         SPACE         TO      P-REC.
     INITIALIZE                         P-REC.
     ADD          1             TO      P-CNT.
     MOVE         P-CNT         TO      H-PAGE.
*
     MOVE         TANA-F83(1:4) TO      H-TDATEY.
     MOVE         TANA-F83(5:2) TO      H-TDATEM.
     MOVE         TANA-F83(7:2) TO      H-TDATED.
     MOVE         TANA-F04      TO      H-SOKOCD
                                        SOK-F01.
     READ    ZSOKMS
         INVALID KEY
             MOVE    SPACE        TO   H-SOKOMEI
         NOT INVALID KEY
             MOVE    SOK-F02      TO   H-SOKOMEI
     END-READ.
*
     IF       P-CNT  =   1
              WRITE      P-REC   FROM    MIDASI-0   AFTER  1
              WRITE      P-REC   FROM    MIDASI-1   AFTER  1
              WRITE      P-REC   FROM    MIDASI-2   AFTER  2
              WRITE      P-REC   FROM    SEN        AFTER  1
              WRITE      P-REC   FROM    MIDASI-3   AFTER  1
              WRITE      P-REC   FROM    SEN        AFTER  1
        ELSE
              WRITE      P-REC   AFTER   PAGE
              WRITE      P-REC   FROM    MIDASI-0   AFTER  1
              WRITE      P-REC   FROM    MIDASI-1   AFTER  1
              WRITE      P-REC   FROM    MIDASI-2   AFTER  2
              WRITE      P-REC   FROM    SEN        AFTER  1
              WRITE      P-REC   FROM    MIDASI-3   AFTER  1
              WRITE      P-REC   FROM    SEN        AFTER  1
     END-IF.
     MOVE        7        TO      L-CNT.
 MIDASI-SEC-EXIT.
     EXIT.
************************************************************
*      3.0        終了処理                                 *
************************************************************
 END-SEC                SECTION.
     CLOSE    YTTANAW  ZSOKMS  SUBMEIF  PRINTF.
     DISPLAY  "ﾀﾅｵﾛｼﾌｧｲﾙ    (IN) = "  READ-CNT   UPON  CONS.
*****DISPLAY  "ﾀﾅｵﾛｼﾌｧｲﾙ  (SKIP) = "  SKIP-CNT   UPON  CONS.
     DISPLAY  "ﾀﾅｵﾛｼﾌｧｲﾙ   (OUT) = "  WRITE-CNT  UPON  CONS.
     DISPLAY  "ﾁｪｯｸﾘｽﾄ (ﾍﾟｰｼﾞｽｳ) = "  P-CNT      UPON  CONS.
 END-SEC-EXIT.
     EXIT.
*****************<<  PROGRAM  END  >>***********************
