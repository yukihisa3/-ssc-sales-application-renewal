****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　在庫管理システム　　　　　　　　　*
*    モジュール名　　　　：　実卸表                            *
*    作成日／更新日　　　：　00/05/29                          *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　_卸表の出力を行う　　　　　　　　*
*                                                              *
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            STA0050L.
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
         YB-21     IS   YB2
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
****<< _卸ファイル         >>******************************
     SELECT   ZTANADT   ASSIGN  TO   DA-01-VI-ZTANADT2
                        ORGANIZATION         IS   INDEXED
                        ACCESS  MODE         IS   SEQUENTIAL
                        RECORD  KEY          IS   TANA-F04
                                                  TANA-F05
                                                  TANA-F06X
                                                  TANA-F15
                        FILE    STATUS       IS   TANA-STATUS.
****<< 倉庫マスタ           >>******************************
     SELECT   ZSOKMS    ASSIGN  TO   DA-01-VI-ZSOKMS1
                        ORGANIZATION         IS   INDEXED
                        ACCESS  MODE         IS   RANDOM
                        RECORD  KEY          IS   SOK-F01
                        FILE    STATUS       IS   SOK-STATUS.
****<< 商品名称マスタ       >>******************************
     SELECT   HMEIMS    ASSIGN  TO   DA-01-VI-MEIMS1
                        ORGANIZATION         IS   INDEXED
                        ACCESS  MODE         IS   RANDOM
                        RECORD  KEY          IS   MEI-F01
                        FILE    STATUS       IS   MEI-STATUS.
****<< 条件ファイル         >>******************************
     SELECT   HJYOKEN   ASSIGN  TO   DA-01-VI-JYOKEN1
                        ORGANIZATION         IS   INDEXED
                        ACCESS  MODE         IS   RANDOM
                        RECORD  KEY          IS   JYO-F01
                                                  JYO-F02
                        FILE    STATUS       IS   JYO-STATUS.
****<<  プリントファイル    >>******************************
     SELECT   PRINTF    ASSIGN  TO   LP-04.
****<<  画面ファイル        >>******************************
     SELECT   DSPF      ASSIGN  TO   GS-DSPF
                        ORGANIZATION         IS  SEQUENTIAL
                        ACCESS  MODE         IS  SEQUENTIAL
                        SYMBOLIC DESTINATION IS  "DSP"
                        PROCESSING MODE      IS   DSP-PROC
                        GROUP                IS   DSP-GROUP
                        FORMAT               IS   DSP-FORMAT
                        SELECTED FUNCTION    IS   DSP-FUNC
                        FILE STATUS          IS   DSP-STATUS.
 DATA                   DIVISION.
 FILE                   SECTION.
****<< _卸ファイル         >>******************************
 FD    ZTANADT
       LABEL     RECORD    IS    STANDARD.
       COPY      ZTANADT   OF    XFDLIB
                 JOINING   TANA  PREFIX.
*
****<< 倉庫マスタ           >>******************************
 FD    ZSOKMS
       LABEL     RECORD    IS    STANDARD.
       COPY      ZSOKMS    OF    XFDLIB
                 JOINING   SOK   PREFIX.
****<< 商品名称マスタ       >>******************************
 FD    HMEIMS
       LABEL     RECORD    IS    STANDARD.
       COPY      HMEIMS    OF    XFDLIB
                 JOINING   MEI   PREFIX.
****<< 条件ファイル         >>******************************
 FD    HJYOKEN
       LABEL     RECORD    IS    STANDARD.
       COPY      HJYOKEN   OF    XFDLIB
                 JOINING   JYO   PREFIX.
****<<  プリントファイル  >>********************************
 FD    PRINTF    LINAGE  IS  66.
 01    P-REC                 PIC  X(200).
****<<  画面ファイル        >>******************************
 FD    DSPF.
 COPY     FTA00501           OF   XMDLIB.
*
****  作業領域  ********************************************
 WORKING-STORAGE        SECTION.
****  画面制御項目            ****
 01  DSP-CONTROL.
     02 DSP-PROC             PIC  X(02).
     02 DSP-GROUP            PIC  X(08).
     02 DSP-FORMAT           PIC  X(08).
     02 DSP-STATUS           PIC  X(02).
     02 DSP-FUNC             PIC  X(04).
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
 01  WORK-AREA.
     02  WK-SHOCD            PIC  X(08)  VALUE  SPACE.
     02  WK-HINTAN           PIC  X(08)  VALUE  SPACE.
     02  WK-SHOMEI1          PIC  N(15)  VALUE  SPACE.
     02  WK-SHOMEI2          PIC  N(15)  VALUE  SPACE.
     02  WK-JYO-SOKCD        PIC  X(02).
     02  WK-DAISOKCD         PIC  X(02).
****  見出し行１             ****
 01  MIDASI-1.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  FILLER              PIC  X(08)  VALUE  "STA0050L".
     02  FILLER              PIC  X(25)  VALUE  SPACE.
     02  FILLER              PIC  N(13)  VALUE
         NC"＊＊＊　_　卸　表　＊＊＊"
                             CHARACTER   TYPE   IS   YB2.
     02  FILLER              PIC  X(25)  VALUE  SPACE.
     02  FILLER              PIC  N(03)  VALUE  NC"処理日"
                             CHARACTER   TYPE   IS   NIHONGO.
     02  FILLER              PIC  X(01)  VALUE  ":".
     02  H-YY                PIC  9(04).
     02  FILLER              PIC  X(01)  VALUE  ".".
     02  H-MM                PIC  Z9.
     02  FILLER              PIC  X(01)  VALUE  ".".
     02  H-DD                PIC  Z9.
     02  FILLER              PIC  X(03)  VALUE  SPACE.
     02  FILLER              PIC  N(01)  VALUE  NC"頁"
                             CHARACTER   TYPE   IS   NIHONGO.
     02  FILLER              PIC  X(01)  VALUE  ":".
     02  H-PAGE              PIC  ZZZ9   VALUE  ZERO.
     02  FILLER              PIC  X(11)  VALUE  SPACE.
****  見出し行２             ****
 01  MIDASI-2       CHARACTER     TYPE   IS   NIHONGO.
     02  FILLER              PIC  X(05)  VALUE  SPACE.
     02  FILLER              PIC  N(03)  VALUE  NC"倉　庫".
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  H-SOKOCD            PIC  X(02)  VALUE  SPACE.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  H-SOKOMEI           PIC  N(18).
     02  FILLER              PIC  X(85)  VALUE  SPACE.
****  見出し行３             ****
 01  MIDASI-3       CHARACTER     TYPE   IS   NIHONGO.
     02  FILLER              PIC  X(05)  VALUE  SPACE.
     02  FILLER              PIC  N(02)  VALUE  NC"商品".
     02  FILLER              PIC  X(04)  VALUE  "ｺｰﾄﾞ".
     02  FILLER              PIC  X(02)  VALUE  SPACE.
     02  FILLER              PIC  N(03)  VALUE  NC"品　単".
     02  FILLER              PIC  X(06)  VALUE  SPACE.
     02  FILLER              PIC  N(03)  VALUE  NC"_　番".
     02  FILLER              PIC  X(04)  VALUE  SPACE.
     02  FILLER              PIC  X(06)  VALUE  "ｽﾄｯｸNO".
     02  FILLER              PIC  X(02)  VALUE  SPACE.
     02  FILLER              PIC  N(05)  VALUE  NC"商　品　名".
     02  FILLER              PIC  X(37)  VALUE  SPACE.
     02  FILLER              PIC  N(04)  VALUE  NC"実_卸数".
     02  FILLER              PIC  X(07)  VALUE  SPACE.
     02  FILLER              PIC  N(03)  VALUE  NC"単　価".
     02  FILLER              PIC  X(04)  VALUE  SPACE.
     02  FILLER              PIC  N(02)  VALUE  NC"年度".
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  FILLER              PIC  N(03)  VALUE  NC"備　考".
     02  FILLER              PIC  X(08)  VALUE  SPACE.
****  明細行１               ****
 01  MEISAI-1.
     02  FILLER              PIC  X(05)  VALUE  SPACE.
     02  M-SHOCD             PIC  X(08).
     02  FILLER              PIC  X(02)  VALUE  SPACE.
     02  M-HINTAN1           PIC  X(05).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  M-HINTAN2           PIC  X(02).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  M-HINTAN3           PIC  X(01).
     02  FILLER              PIC  X(02)  VALUE  SPACE.
     02  M-TANABAN1          PIC  X(01).
     02  FILLER              PIC  X(01)  VALUE  "-".
     02  M-TANABAN2          PIC  X(03).
     02  FILLER              PIC  X(01)  VALUE  "-".
     02  M-TANABAN3          PIC  X(02).
     02  FILLER              PIC  X(02)  VALUE  SPACE.
     02  M-STOCK1            PIC  X(04).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  M-STOCK2            PIC  X(01).
     02  FILLER              PIC  X(02)  VALUE  SPACE.
     02  M-SHOMEI            PIC  N(30)
                             CHARACTER   TYPE   IS   YB1.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  M-JITTANA           PIC  -,---,--9.99.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  M-TANKA             PIC  Z,ZZZ,ZZ9.99.
     02  FILLER              PIC  X(02)  VALUE  SPACE.
     02  M-NENDO             PIC  99.
     02  FILLER              PIC  X(02)  VALUE  SPACE.
     02  M-BIKOU             PIC  X(10).
     02  FILLER              PIC  X(04)  VALUE  SPACE.
****  ＰＦキーガイド  ***
 01  MSG-AREA.
     02  PMSG01              PIC N(20) VALUE
                             NC"_終了".
     02  PMSG02              PIC N(20) VALUE
                             NC"_取消　_終了　_項目戻し".
**** メッセージ情報           ****
 01  MSG-AREA1-1.
     02  MSG-ABEND1.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-PG-ID         PIC  X(08)  VALUE  "STA0050L".
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
****  エラーメッセージ        ***
 01  ERR-TAB.
     02  MSG-ERR1            PIC  N(20)  VALUE
            NC"無効ＰＦキーです。".
     02  MSG-ERR2            PIC  N(20)  VALUE
            NC"倉庫コードを入力して下さい。".
     02  MSG-ERR3            PIC  N(20)  VALUE
            NC"倉庫コードに誤りがあります。".
     02  MSG-ERR4            PIC  N(20)  VALUE
            NC"指定の倉庫コードは扱えません。".
     02  MSG-ERR5            PIC  N(20)  VALUE
            NC"確認”Ｙ”以外は扱えません。".
 01  ERR-MSG-ALL             REDEFINES    ERR-TAB.
     02  ERR-MSG             PIC  N(20)
                             OCCURS   5  TIMES.
****  エラーメッセージ　ＦＯＲ　コンソール  ***
 01  FILE-ERROR.
     02  FILE-ERR1           PIC  N(10)  VALUE
            NC"_卸ファイル　異常！".
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
 LINKAGE                SECTION.
 01  PARA-AREA.
     02    PARA-WKSTN        PIC  X(08).
************************************************************
*             ＭＡＩＮ         ＭＯＤＵＬＥ                *
************************************************************
*
 PROCEDURE              DIVISION      USING   PARA-AREA.
*
 DECLARATIVES.
**_卸ファイル
 FILEERR-SEC1           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  ZTANADT.
     MOVE   "ZTANADT "        TO    ERR-FL-ID.
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
                   PROCEDURE  HMEIMS.
     MOVE   "HMEIMS  "        TO    ERR-FL-ID.
     MOVE    MEI-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
**条件ファイル
 FILEERR-SEC4           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  HJYOKEN.
     MOVE   "HJYOKEN "        TO    ERR-FL-ID.
     MOVE    JYO-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
**画面ファイル
 FILEERR-SEC5           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  DSPF.
     MOVE   "DSPF    "        TO    ERR-FL-ID.
     MOVE    DSP-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
     STOP     RUN.
 END     DECLARATIVES.
************************************************************
 STA0050L-START         SECTION.
     PERFORM       INIT-SEC.
     PERFORM       MAIN-SEC
                   UNTIL     END-FLG  =    "END".
     PERFORM       END-SEC.
     STOP     RUN.
 STA0050L-END.
     EXIT.
************************************************************
*      1.0      初期処理                                   *
************************************************************
 INIT-SEC               SECTION.
     OPEN     I-O       DSPF.
     OPEN     INPUT     ZTANADT.
     OPEN     INPUT     ZSOKMS.
     OPEN     INPUT     HMEIMS.
     OPEN     INPUT     HJYOKEN.
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
     MOVE      WK-TIME(1:2)       TO   HEN-TIME-HH.
     MOVE      WK-TIME(3:2)       TO   HEN-TIME-MM.
     MOVE      WK-TIME(5:2)       TO   HEN-TIME-SS.
*
*---- 条件ファイル　索引
     MOVE     65          TO     JYO-F01.
     MOVE     PARA-WKSTN  TO     JYO-F02.
     READ    HJYOKEN
           INVALID
             DISPLAY  FILE-ERR4               UPON CONS
             DISPLAY "ERR INVALID KEY1= " JYO-F01 UPON CONS
             DISPLAY "ERR INVALID KEY2= " JYO-F02 UPON CONS
             PERFORM    END-SEC
             STOP RUN
     END-READ.
     MOVE     JYO-F14(1:2)  TO   WK-JYO-SOKCD.
     MOVE     JYO-F15(1:2)  TO   WK-DAISOKCD.
*******
     PERFORM     DSP-INIT-SEC.
*******
     PERFORM     WRITE-SELECT-SEC.
*---- _卸Ｆスタート
     IF   END-FLG    NOT =   "END"
       MOVE   SOKCD       TO     TANA-F04
       MOVE   ALL ZERO    TO     TANA-F05
       MOVE   SPACE       TO     TANA-F06X
       MOVE   SPACE       TO     TANA-F15
       START  ZTANADT  KEY IS  >=  TANA-F04 TANA-F05 TANA-F06X
                                                     TANA-F15
         INVALID  KEY
           MOVE   "END"   TO     END-FLG
         NOT  INVALID  KEY
           PERFORM      ZTANADT-READ-SEC
       END-START
     END-IF.
 INIT-SEC-EXIT.
     EXIT.
****************************************************************
*      1.1 　　出力条件指定　処理                              *
****************************************************************
 WRITE-SELECT-SEC            SECTION.
*----- 初画面出力 ------*
     MOVE    PMSG01              TO   PFKGID.
     PERFORM    DSP-WRITE-SEC.
*----- 画面ＲＥＡＤ ----*
 WS010.
     MOVE    "BODY"              TO   DSP-GROUP.
     PERFORM    DSP-READ-SEC.
*----- ＰＦキー判定　_１ ----*
     MOVE    "M"                 TO   EDIT-OPTION   OF   SOKCD.
     MOVE    " "                 TO   EDIT-CURSOR   OF   SOKCD.
     EVALUATE      DSP-FUNC
*****    WHEN     "F004"
*****        PERFORM    DSP-INIT-SEC
*****        GO                  TO   WRITE-SELECT-SEC
         WHEN     "F005"
             MOVE    "END"       TO   END-FLG
             GO                  TO   WRITE-SELECT-END
         WHEN     "E000"
             CONTINUE
         WHEN      OTHER
             MOVE    1           TO   ERR-CD
             PERFORM    DSP-ERR-SEC
             GO                  TO   WS010
     END-EVALUATE.
*----- 入力項目チェック---*
*（条件ファイルでの存在チェック・本社の時は全倉庫可）
     MOVE    SPACE               TO   SOKMEI.
     IF   WK-DAISOKCD  NOT =  "01"
          IF   SOKCD   NOT =  WK-JYO-SOKCD
               IF   ERR-CD   =   ZERO
                    MOVE    4           TO   ERR-CD
               END-IF
               GO           TO        WS015
          END-IF
     END-IF.
     IF  SOKCD   =   SPACE
*********MOVE    NC"全倉庫"          TO   SOKMEI
                 IF   ERR-CD    =    ZERO
                    MOVE    2        TO   ERR-CD
                 END-IF
     ELSE
*（存在チェック）*
         MOVE    SOKCD               TO   SOK-F01
         READ    ZSOKMS
             INVALID KEY
                 MOVE    SPACE       TO   SOKMEI
                 IF   ERR-CD    =    ZERO
                    MOVE    3        TO   ERR-CD
                 END-IF
             NOT INVALID KEY
                 MOVE    SOK-F02     TO   SOKMEI
         END-READ
     END-IF.
 WS015.
*----- エラーが有った時 ----*
     IF   ERR-CD  NOT =   ZERO
         MOVE    "R"             TO   EDIT-OPTION   OF   SOKCD
         MOVE    "C"             TO   EDIT-CURSOR   OF   SOKCD
         PERFORM    DSP-ERR-SEC
         GO                      TO   WS010
     END-IF.
*----- ボディー部の出力 ----*
     MOVE    PMSG02              TO   PFKGID.
     MOVE    "Y"                 TO   KAKNIN.
     MOVE    "M"                 TO   EDIT-OPTION   OF   SOKCD.
     MOVE    " "                 TO   EDIT-CURSOR   OF   SOKCD.
     MOVE    SPACE               TO   ERRMSG.
     PERFORM    DSP-WRITE-SEC.
*----- 確認画面の入力-----*
 WS020.
     MOVE    "TAIL"              TO   DSP-GROUP.
     PERFORM    DSP-READ-SEC.
*----- ＰＦキー判定　_２ ----*
     EVALUATE      DSP-FUNC
         WHEN     "F004"
             PERFORM    DSP-INIT-SEC
             GO                  TO   WRITE-SELECT-SEC
         WHEN     "F005"
             MOVE    "END"       TO   END-FLG
             GO                  TO   WRITE-SELECT-END
         WHEN     "F006"
             GO                  TO   WRITE-SELECT-SEC
         WHEN     "E000"
             IF   KAKNIN    NOT =     "Y"
                  MOVE    5           TO   ERR-CD
                  MOVE    "TAIL"      TO   DSP-GROUP
                  PERFORM    DSP-ERR-SEC
                  GO                  TO   WS020
             END-IF
         WHEN      OTHER
             MOVE    1           TO   ERR-CD
             PERFORM    DSP-ERR-SEC
             GO                  TO   WS020
     END-EVALUATE.
 WRITE-SELECT-END.
     EXIT.
****************************************************************
*      1.1.1 　初期画面　出力　処理                            *
****************************************************************
 DSP-INIT-SEC           SECTION.
     MOVE    SPACE               TO   FTA00501.
     MOVE    HEN-DATE            TO   SDATE.
     MOVE    HEN-TIME            TO   STIME.
     IF  WK-DAISOKCD   NOT =  "01"
         MOVE    WK-JYO-SOKCD        TO   SOKCD  SOK-F01
         READ    ZSOKMS
             INVALID KEY
                 CONTINUE
             NOT INVALID KEY
                 MOVE    SOK-F02     TO   SOKMEI
         END-READ
     END-IF.
*
     MOVE    SPACE               TO   DSP-CONTROL.
     MOVE    "CL"                TO   DSP-PROC.
     MOVE    "FTA00501"          TO   DSP-FORMAT.
     MOVE    "ALL"               TO   DSP-GROUP.
     MOVE    PMSG01              TO   PFKGID.
     WRITE                  FTA00501.
 DSP-INIT-END.
     EXIT.
****************************************************************
*      1.1.2 　画面ＲＥＡＤ　処理                              *
****************************************************************
 DSP-READ-SEC           SECTION.
     MOVE    "NE"                TO   DSP-PROC.
     READ    DSPF
         AT   END
             GO                  TO   DSP-READ-END
     END-READ.
     IF   DSP-STATUS   NOT =   ZERO
         DISPLAY   FILE-ERR5   UPON   CONS
     END-IF.
 DSP-READ-END.
     EXIT.
****************************************************************
*      1.1.3 　画面ＷＲＩＴＥ　処理                            *
****************************************************************
 DSP-WRITE-SEC          SECTION.
******
     MOVE    "ALL"               TO   DSP-GROUP.
******
     MOVE    SPACE               TO   DSP-PROC.
     WRITE                  FTA00501.
 DSP-WRITE-END.
     EXIT.
****************************************************************
*      1.1.4     エラーメッセージセット　処理                  *
****************************************************************
 DSP-ERR-SEC                 SECTION.
*---- エラー メッセージ セット ----*
     MOVE    ERR-MSG(ERR-CD)     TO   ERRMSG.
     MOVE    ZERO                TO   ERR-CD.
     PERFORM    DSP-WRITE-SEC.
 DSP-ERR-END.
     EXIT.
************************************************************
*      1.2       _卸ファイル　ＲＥＡＤ処理                *
************************************************************
 ZTANADT-READ-SEC       SECTION.
     READ    ZTANADT
       AT  END
           MOVE    "END"       TO   END-FLG
           GO       TO   ZTANADT-READ-SEC-EXIT
     END-READ.
     ADD      1          TO   READ-CNT.
*
     IF  SOKCD   NOT =  SPACE     AND
         SOKCD       <  TANA-F04
           MOVE      "END"       TO   END-FLG
           GO       TO   ZTANADT-READ-SEC-EXIT
     END-IF.
     IF  TANA-F99    =   "9"
           ADD  1   TO   SKIP-CNT
           GO       TO   ZTANADT-READ-SEC
     END-IF.
 ZTANADT-READ-SEC-EXIT.
     EXIT.
************************************************************
*      2.0       メイン処理                                *
************************************************************
 MAIN-SEC               SECTION.
     PERFORM  HENSYU-SEC.
     PERFORM  ZTANADT-READ-SEC.
 MAIN-SEC-EXIT.
     EXIT.
************************************************************
*      2.1       編集処理                                  *
************************************************************
 HENSYU-SEC             SECTION.
     IF       L-CNT         >=      62     OR
              TANA-F04   NOT =    H-SOKOCD
              PERFORM       MIDASI-SEC
     END-IF.
*
     IF   WK-SHOCD   NOT =  TANA-F05
              OR      WK-HINTAN   NOT =  TANA-F06X
       MOVE   TANA-F05      TO        MEI-F011
       MOVE   TANA-F06X     TO        MEI-F012
       READ   HMEIMS
           INVALID  KEY
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
     MOVE     TANA-F05      TO      M-SHOCD.
     MOVE     TANA-F06      TO      M-HINTAN1.
     MOVE     TANA-F07      TO      M-HINTAN2.
     MOVE     TANA-F08      TO      M-HINTAN3.
     MOVE     TANA-F15(1:1) TO      M-TANABAN1.
     MOVE     TANA-F15(2:3) TO      M-TANABAN2.
     MOVE     TANA-F15(5:2) TO      M-TANABAN3.
     MOVE     TANA-F09(1:4) TO      M-STOCK1.
     MOVE     TANA-F09(5:1) TO      M-STOCK2.
     MOVE     WK-SHOMEI1    TO      M-SHOMEI(1:15).
     MOVE     WK-SHOMEI2    TO      M-SHOMEI(16:15).
     MOVE     TANA-F11      TO      M-JITTANA.
     MOVE     TANA-F12      TO      M-TANKA.
     MOVE     TANA-F13      TO      M-NENDO.
     MOVE     TANA-F14      TO      M-BIKOU.
     WRITE    P-REC         FROM    MEISAI-1    AFTER  2.
     ADD      2             TO      L-CNT.
     ADD      1             TO      WRITE-CNT.
*
 HENSYU-SEC-EXIT.
     EXIT.
************************************************************
*      2.1.1       見出し処理                              *
************************************************************
 MIDASI-SEC             SECTION.
     MOVE         SPACE   TO      P-REC.
     INITIALIZE                   P-REC.
     ADD          1       TO      P-CNT.
     MOVE         P-CNT   TO      H-PAGE.
*
     MOVE    TANA-F04     TO      H-SOKOCD
                                  SOK-F01.
     READ    ZSOKMS
         INVALID KEY
             MOVE    SPACE        TO   H-SOKOMEI
         NOT INVALID KEY
             MOVE    SOK-F02      TO   H-SOKOMEI
     END-READ.
*
     IF       P-CNT  =   1
              WRITE      P-REC   FROM    MIDASI-1   AFTER  2
              WRITE      P-REC   FROM    MIDASI-2   AFTER  2
              WRITE      P-REC   FROM    MIDASI-3   AFTER  2
        ELSE
              WRITE      P-REC   AFTER   PAGE
              WRITE      P-REC   FROM    MIDASI-1   AFTER  2
              WRITE      P-REC   FROM    MIDASI-2   AFTER  2
              WRITE      P-REC   FROM    MIDASI-3   AFTER  2
     END-IF.
     MOVE        6        TO      L-CNT.
 MIDASI-SEC-EXIT.
     EXIT.
************************************************************
*      3.0        終了処理                                 *
************************************************************
 END-SEC                SECTION.
     CLOSE    ZTANADT  ZSOKMS  HMEIMS  HJYOKEN  PRINTF.
     DISPLAY  "ﾀﾅｵﾛｼﾌｧｲﾙ     (IN) = "  READ-CNT  UPON  CONS.
     DISPLAY  "ﾀﾅｵﾛｼﾌｧｲﾙ   (SKIP) = "  SKIP-CNT  UPON  CONS.
     DISPLAY  "ﾀﾅｵﾛｼﾌｧｲﾙ    (OUT) = "  WRITE-CNT UPON  CONS.
     DISPLAY  "ｼﾞﾂﾀﾅﾋｮｳ (ﾍﾟｰｼﾞｽｳ) = "  P-CNT     UPON  CONS.
 END-SEC-EXIT.
     EXIT.
*****************<<  PROGRAM  END  >>***********************
