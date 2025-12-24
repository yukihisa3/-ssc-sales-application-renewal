# ZTA0040B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIB/ZTA0040B.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　在庫管理システム　　　　　　　　　*
*    モジュール名　　　　：　_卸入力チェックリスト            *
*    作成日／更新日　　　：　93/04/28                          *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　_卸入力チェックリストの出力を行う*
*                                                              *
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            ZTA0040B.
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
                                                  TANA-F06
                                                  TANA-F07
                                                  TANA-F08
                                                  TANA-F15
                        FILE    STATUS       IS   TANA-STATUS.
****<< 倉庫マスタ           >>******************************
     SELECT   ZSOKMS    ASSIGN  TO   DA-01-VI-ZSOKMS1
                        ORGANIZATION         IS   INDEXED
                        ACCESS  MODE         IS   RANDOM
                        RECORD  KEY          IS   SOK-F01
                        FILE    STATUS       IS   SOK-STATUS.
****<< 商品名称マスタ       >>******************************
     SELECT   HMEIMS     ASSIGN  TO   DA-01-VI-MEIMS1
                        ORGANIZATION         IS   INDEXED
                        ACCESS  MODE         IS   RANDOM
                        RECORD  KEY          IS   MEI-F01
                        FILE    STATUS       IS   MEI-STATUS.
****<< 条件ファイル         >>******************************
     SELECT   HJYOKEN    ASSIGN  TO   DA-01-VI-JYOKEN1
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
 COPY     ZTA0040           OF   XMDLIB.
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
****  日付保存                ****
 01  SYSTEM-HIZUKE.
     02  SYSYMD              PIC  9(06).
     02  SYSYMD-R            REDEFINES SYSYMD.
       03  SYS-YY            PIC  99.
       03  SYS-MM            PIC  99.
       03  SYS-DD            PIC  99.
****  カウンタ                ****
 01  CNT-AREA.
     02  L-CNT               PIC  9(02)  VALUE  99.
     02  P-CNT               PIC  9(07)  VALUE  ZERO.
     02  READ-CNT            PIC  9(07)  VALUE  ZERO.
     02  WRITE-CNT           PIC  9(07)  VALUE  ZERO.
     02  SKIP-CNT            PIC  9(07)  VALUE  ZERO.
****  ワーク                  ****
 01  WK-SOKO                 PIC  X(02)  VALUE  SPACE.
 01  WORK-AREA.
     02  WK-SHOCD            PIC  9(08)  VALUE  ZERO.
     02  WK-HINTAN           PIC  X(08)  VALUE  SPACE.
     02  WK-SHOMEI1          PIC  N(15)  VALUE  SPACE.
     02  WK-SHOMEI2          PIC  N(15)  VALUE  SPACE.
     02  WK-JYO-F04          PIC  9(02).
     02  WK-JYO-SOKCD
                  REDEFINES  WK-JYO-F04    PIC  X(02).
****  見出し行１             ****
 01  MIDASI-1.
     02  FILLER              PIC  X(34)  VALUE  SPACE.
     02  FILLER              PIC  N(19)  VALUE
         NC"＊＊＊　_卸入力チェックリスト　＊＊＊"
                             CHARACTER   TYPE   IS   YB2.
     02  FILLER              PIC  X(09)  VALUE  SPACE.
     02  FILLER              PIC  N(03)  VALUE  NC"処理日"
                             CHARACTER   TYPE   IS   NIHONGO.
     02  FILLER              PIC  X(01)  VALUE  ":".
     02  H-YY                PIC  Z9.
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
     02  FILLER              PIC  X(02)  VALUE  SPACE.
     02  FILLER              PIC  N(02)  VALUE  NC"倉庫".
     02  FILLER              PIC  X(01)  VALUE  ":".
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  H-SOKOCD            PIC  X(02).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  H-SOKOMEI           PIC  N(18).
     02  FILLER              PIC  X(89)  VALUE  SPACE.
****  見出し行３             ****
 01  MIDASI-3       CHARACTER     TYPE   IS   NIHONGO.
     02  FILLER              PIC  X(02)  VALUE  SPACE.
     02  FILLER              PIC  N(01)  VALUE  NC"削".
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  FILLER              PIC  N(03)  VALUE  NC"原票_".
     02  FILLER              PIC  X(04)  VALUE  SPACE.
     02  FILLER              PIC  N(03)  VALUE  NC"_　番".
     02  FILLER              PIC  X(03)  VALUE  SPACE.
     02  FILLER              PIC  N(02)  VALUE  NC"商品".
     02  FILLER              PIC  X(04)  VALUE  "ｺｰﾄﾞ".
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  FILLER              PIC  N(03)  VALUE  NC"品　単".
     02  FILLER              PIC  X(03)  VALUE  SPACE.
     02  FILLER              PIC  X(04)  VALUE  "STNO".
     02  FILLER              PIC  X(02)  VALUE  SPACE.
     02  FILLER              PIC  N(05)  VALUE  NC"商　品　名".
     02  FILLER              PIC  X(37)  VALUE  SPACE.
     02  FILLER              PIC  N(04)  VALUE  NC"実_卸数".
     02  FILLER              PIC  X(07)  VALUE  SPACE.
     02  FILLER              PIC  N(03)  VALUE  NC"単　価".
     02  FILLER              PIC  X(03)  VALUE  SPACE.
     02  FILLER              PIC  N(02)  VALUE  NC"年度".
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  FILLER              PIC  N(03)  VALUE  NC"備　考".
     02  FILLER              PIC  X(06)  VALUE  SPACE.
****  明細行１               ****
 01  MEISAI-1.
     02  FILLER              PIC  X(02)  VALUE  SPACE.
     02  M-SAKUJYO           PIC  N(01)
                             CHARACTER   TYPE   IS   NIHONGO.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  M-GENPYOU1          PIC  9(06).
     02  FILLER              PIC  X(01)  VALUE  "-".
     02  M-GENPYOU2          PIC  9(01).
     02  FILLER              PIC  X(02)  VALUE  SPACE.
     02  M-TANABAN1          PIC  X(01).
     02  FILLER              PIC  X(01)  VALUE  "-".
     02  M-TANABAN2          PIC  X(03).
     02  FILLER              PIC  X(01)  VALUE  "-".
     02  M-TANABAN3          PIC  X(02).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  M-SHOCD             PIC  X(08).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  M-HINTAN            PIC  X(08).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  M-STOCK             PIC  X(05).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  M-SHOMEI            PIC  N(30)
                             CHARACTER   TYPE   IS   YB1.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  M-JITTANA           PIC  -,---,--9.99.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  M-TANKA             PIC  Z,ZZZ,ZZ9.99.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  M-NENDO             PIC  99.
     02  FILLER              PIC  X(02)  VALUE  SPACE.
     02  M-BIKOU             PIC  X(10).
     02  FILLER              PIC  X(02)  VALUE  SPACE.
****  ＰＦキーガイド  ***
 01  MSG-AREA.
     02  PMSG01              PIC N(20) VALUE
                             NC"_終了".
     02  PMSG02              PIC N(20) VALUE
                             NC"_取消　_終了".
**** メッセージ情報           ****
 01  MSG-AREA1-1.
     02  MSG-ABEND1.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-PG-ID         PIC  X(08)  VALUE  "ZTA0040B".
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
     MOVE   "HMEIMS   "        TO    ERR-FL-ID.
     MOVE    MEI-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
**条件ファイル
 FILEERR-SEC4           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  HJYOKEN.
     MOVE   "HJYOKEN  "       TO    ERR-FL-ID.
     MOVE    JYO-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
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
 END     DECLARATIVES.
************************************************************
 ZTA0040B-START         SECTION.
     PERFORM       INIT-SEC.
     PERFORM       MAIN-SEC
                   UNTIL     END-FLG  =    "END".
     PERFORM       END-SEC.
     STOP     RUN.
 ZTA0040B-END.
     EXIT.
************************************************************
*      _０     初期処理                                   *
************************************************************
 INIT-SEC               SECTION.
     OPEN     I-O       DSPF.
     OPEN     INPUT     ZTANADT.
     OPEN     INPUT     ZSOKMS.
     OPEN     INPUT     HMEIMS.
     OPEN     INPUT     HJYOKEN.
     OPEN     OUTPUT    PRINTF.
*---- システム日付　取得
     ACCEPT   SYSYMD    FROM   DATE.
     MOVE     SYS-YY    TO     H-YY.
     MOVE     SYS-MM    TO     H-MM.
     MOVE     SYS-DD    TO     H-DD.
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
     MOVE    JYO-F04             TO   WK-JYO-F04.
*----- 初画面出力 ------*
*****
     PERFORM    DSP-INIT-SEC.
*****
     PERFORM     WRITE-SELECT-SEC.
     IF   END-FLG    NOT =   "END"
         PERFORM      ZTANADT-READ-SEC
*95/05/31
         MOVE         TANA-F04  TO  WK-SOKO
     END-IF.
 INIT-SEC-EXIT.
     EXIT.
****************************************************************
*      _１　　出力条件指定　処理                              *
****************************************************************
 WRITE-SELECT-SEC            SECTION.
*----- 初画面出力 ------*
**** PERFORM    DSP-INIT-SEC.
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
*（入力有無チェック）*
     IF   SOKCD   =   SPACE
         GO TO                        WS015
     END-IF.
*
*（存在チェック）*
     MOVE    SOKCD               TO   SOK-F01.
     READ    ZSOKMS
         INVALID KEY
             MOVE    SPACE       TO   SOKMEI
             IF   ERR-CD    =    ZERO
                MOVE    3        TO   ERR-CD
             END-IF
         NOT INVALID KEY
             MOVE    SOK-F02     TO   SOKMEI
             MOVE    SOK-F02     TO   H-SOKOMEI
             MOVE    SOKCD       TO   H-SOKOCD
     END-READ.
*（条件ファイルでの存在チェック・本社は全倉庫可）
     IF  WK-JYO-F04 NOT =   01
         IF  SOKCD      NOT =   WK-JYO-SOKCD
             IF   ERR-CD    =   ZERO
                  MOVE    4        TO   ERR-CD
             END-IF
         END-IF
     END-IF.
 WS015.
*----- エラーが有った時 ----*
     IF   ERR-CD  NOT =   ZERO
         MOVE    "R"             TO   EDIT-OPTION   OF   SOKCD
         MOVE    "C"             TO   EDIT-CURSOR   OF   SOKCD
******** MOVE    "BODY"          TO   DSP-GROUP
         PERFORM    DSP-ERR-SEC
         GO                      TO   WS010
     END-IF.
*----- ボディー部の出力 ----*
     MOVE    PMSG02              TO   PFKGID.
     MOVE    "Y"                 TO   KAKNIN.
     MOVE    "M"                 TO   EDIT-OPTION   OF   SOKCD.
     MOVE    " "                 TO   EDIT-CURSOR   OF   SOKCD.
**** MOVE    "BODY"              TO   DSP-GROUP.
     MOVE    SPACE               TO   ERRMSG.
     PERFORM    DSP-WRITE-SEC.
*----- 確認画面の出力-----*
**** MOVE    "TAIL"              TO   DSP-GROUP.
**** PERFORM    DSP-WRITE-SEC.
*----- 確認画面の入力-----*
 WS020.
     MOVE    "TAIL"              TO   DSP-GROUP.
     PERFORM    DSP-READ-SEC.
*----- ＰＦキー判定　_２ ----*
     EVALUATE      DSP-FUNC
         WHEN     "F004"
** ｶﾞﾒﾝ ｸﾘｱ **
             MOVE  SPACE         TO   ZTA0040
**************
             GO                  TO   WRITE-SELECT-SEC
         WHEN     "F005"
             MOVE    "END"       TO   END-FLG
             GO                  TO   WRITE-SELECT-END
         WHEN     "E000"
             IF    KAKNIN    NOT =    "Y"
                   MOVE    5           TO   ERR-CD
                   MOVE    "TAIL"      TO   DSP-GROUP
                   PERFORM    DSP-ERR-SEC
                   GO                  TO   WS020
             END-IF
*********    CONTINUE
         WHEN      OTHER
             MOVE    1           TO   ERR-CD
             MOVE    "TAIL"      TO   DSP-GROUP
             PERFORM    DSP-ERR-SEC
             GO                  TO   WS020
     END-EVALUATE.
 WRITE-SELECT-END.
     EXIT.
****************************************************************
*      __１　初期画面　出力　処理                            *
****************************************************************
 DSP-INIT-SEC           SECTION.
     MOVE    SPACE               TO   ZTA0040.
     MOVE    SPACE               TO   DSP-CONTROL.
     MOVE    "CL"                TO   DSP-PROC.
     MOVE    "ZTA0040"           TO   DSP-FORMAT.
     MOVE    "ALL"               TO   DSP-GROUP.
     MOVE    PMSG01              TO   PFKGID.
     WRITE                  ZTA0040.
     INITIALIZE             ZTA0040.
 DSP-INIT-END.
     EXIT.
****************************************************************
*      __２　画面ＲＥＡＤ　処理                              *
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
*      __３　画面ＷＲＩＴＥ　処理                            *
****************************************************************
 DSP-WRITE-SEC          SECTION.
*****
     MOVE    "ALL"               TO   DSP-GROUP.
*****
     MOVE    SPACE               TO   DSP-PROC.
     WRITE                  ZTA0040.
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
*95/05/31
**** MOVE     TANA-F04   TO   WK-SOKO.
     IF  SOKCD   NOT =  SPACE
          IF  SOKCD   NOT =  TANA-F04
              ADD      1    TO   SKIP-CNT
              GO       TO   ZTANADT-READ-SEC
          END-IF
     END-IF.
 ZTANADT-READ-SEC-EXIT.
     EXIT.
************************************************************
*      _０      メイン処理                                *
************************************************************
 MAIN-SEC               SECTION.
     PERFORM  HENSYU-SEC.
     PERFORM  ZTANADT-READ-SEC.
 MAIN-SEC-EXIT.
     EXIT.
************************************************************
*      _1       編集処理                                  *
************************************************************
 HENSYU-SEC             SECTION.
     IF       L-CNT         >=      62
              PERFORM       MIDASI-SEC
              GO TO         HEN09
     END-IF.
*
     IF       READ-CNT      NOT =   ZERO
        IF    TANA-F04      NOT =   WK-SOKO
              PERFORM       MIDASI-SEC
*95/05/31
              MOVE          TANA-F04     TO   WK-SOKO
        END-IF
     END-IF.
*
 HEN09.
     MOVE     TANA-F04      TO      H-SOKOCD.
     IF   WK-SHOCD   NOT =  TANA-F05
              OR      WK-HINTAN   NOT =  TANA-F06X
       MOVE   TANA-F05      TO        MEI-F011
       MOVE   TANA-F06X     TO        MEI-F012
       READ   HMEIMS
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
     IF    TANA-F99   =   "9"
       MOVE   NC"＊"        TO      M-SAKUJYO
     ELSE
       MOVE   NC"　"        TO      M-SAKUJYO
     END-IF.
     MOVE     TANA-F01(1:6) TO      M-GENPYOU1.
     MOVE     TANA-F01(7:1) TO      M-GENPYOU2.
     MOVE     TANA-F15(1:1) TO      M-TANABAN1.
     MOVE     TANA-F15(2:3) TO      M-TANABAN2.
     MOVE     TANA-F15(5:2) TO      M-TANABAN3.
     MOVE     TANA-F05      TO      M-SHOCD.
     MOVE     TANA-F06X     TO      M-HINTAN.
     MOVE     TANA-F09      TO      M-STOCK.
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
*95/05/31
     MOVE         TANA-F04 TO     H-SOKOCD.
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
     DISPLAY  "ﾀﾅｵﾛｼﾌｧｲﾙ    (IN) = "  READ-CNT   UPON  CONS.
     DISPLAY  "ﾀﾅｵﾛｼﾌｧｲﾙ  (SKIP) = "  SKIP-CNT   UPON  CONS.
     DISPLAY  "ﾀﾅｵﾛｼﾌｧｲﾙ   (OUT) = "  WRITE-CNT  UPON  CONS.
     DISPLAY  "ﾁｪｯｸﾘｽﾄ (ﾍﾟｰｼﾞｽｳ) = "  P-CNT      UPON  CONS.
 END-SEC-EXIT.
     EXIT.
*****************<<  PROGRAM  END  >>***********************

```
