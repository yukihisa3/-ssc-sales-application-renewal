# CSV0120B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/CSV0120B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　ＥＸＣＥＬ連携                    *
*    モジュール名　　　　：　店舗横並発注集計データ作成        *
*    作成日／更新日　　　：　06/10/10                          *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　発注集計Ｆを読み、店舗横並データ　*
*                          を作成する。　　　　　　　　　　　　*
*    作成日／更新日　　　：　20/07/17                          *
*    作成者／更新者　　　：　ＮＡＶ高橋　　　　　　　　　　　　*
*    処理概要　　　　　　：　在庫引当、小売連携、ＳＴＮＯ管理　*
*                          　　　　　　　　　　　　　　　　　　*
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            CSV0120B.
 AUTHOR.                NAV.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       GP6000.
 OBJECT-COMPUTER.       GP6000.
 SPECIAL-NAMES.
         STATION   IS   STAT
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
****<< 店舗横並発注集計データ >>******************************
     SELECT   CSSTBLF  ASSIGN    TO        DA-01-VI-CSSTBLL1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   TBL-F01
                                                 TBL-F02
                                                 TBL-F03
                                                 TBL-F04
                                                 TBL-F05
                                                 TBL-F06
                        FILE      STATUS    IS   TBL-STATUS.
****<< 店舗順ワーク >>****************************************
     SELECT   SEQTENF  ASSIGN    TO        DA-01-VI-SEQTENL1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   SEQUENTIAL
                        RECORD    KEY       IS   SET-F01
                        FILE      STATUS    IS   SET-STATUS.
****<< 発注集計データ >>**********************************
     SELECT   HACYUPPT     ASSIGN    TO        DA-01-S-HACYUPPT
                        FILE      STATUS    IS   HAC-STATUS.
***************************************************************
 DATA                   DIVISION.
***************************************************************
 FILE                   SECTION.
***************************************************************
****<< 店舗マスタ >>***************************************
 FD  CSSTBLF.
     COPY     CSSTBLF  OF        XFDLIB
              JOINING   TBL       PREFIX.
****<< 店舗順ワーク >>*************************************
 FD  SEQTENF.
     COPY     SEQTENF  OF        XFDLIB
              JOINING   SET       PREFIX.
****<< 発注集計データ >>***********************************
 FD  HACYUPPT
              BLOCK CONTAINS  27  RECORDS.
     COPY     HACYUPPT OF        XFDLIB
              JOINING   HAC       PREFIX.
****  作業領域  ************************************************
 WORKING-STORAGE        SECTION.
****************************************************************
****  ステイタス情報          ****
 01  STATUS-AREA.
     02 TBL-STATUS           PIC  X(02).
     02 SET-STATUS           PIC  X(02).
     02 HAC-STATUS           PIC  X(02).
**** メッセージ情報           ****
 01  MSG-AREA1-1.
     02  MSG-ABEND1.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-PG-ID         PIC  X(08)  VALUE  "CSV0120B".
       03  FILLER            PIC  X(10)  VALUE
          " ABEND ###".
     02  MSG-ABEND2.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-FL-ID         PIC  X(08).
       03  FILLER            PIC  X(04)  VALUE  " ST-".
       03  ERR-STCD          PIC  X(02).
       03  FILLER            PIC  X(04)  VALUE  " ###".
****  フラグ                  ****
 01  END-FLG                 PIC  X(03)  VALUE  SPACE.
 01  END-FLG1                PIC  X(03)  VALUE  SPACE.
 01  CSSTBLF-INV-FLG         PIC  X(03)  VALUE  SPACE.
 01  SEQTENF-INV-FLG         PIC  X(03)  VALUE  SPACE.
 01  IX                      PIC  9(03)  VALUE  ZERO.
 01  IY                      PIC  9(03)  VALUE  ZERO.
 01  OK-FLG                  PIC  X(02)  VALUE  SPACE.
****  カウント                ****
 01  CNT-AREA.
     03  HAC-CNT             PIC  9(07)   VALUE  0.
     03  WRT-CNT             PIC  9(07)   VALUE  0.
     03  REW-CNT             PIC  9(07)   VALUE  0.
****  店舗格納ワーク          ****
 01  WK-TBL-TENPO.
     03  TBL-WORK            OCCURS  300.
         05  TBL-TENPOCD     PIC  9(05).
         05  TBL-TENPONM     PIC  N(05).
*
************************************************************
 LINKAGE            SECTION.
 01  LINK-HIDUKE             PIC  9(08).
 01  LINK-JIKAN              PIC  9(04).
 01  LINK-TOKCD              PIC  9(08).
************************************************************
 PROCEDURE              DIVISION       USING  LINK-HIDUKE
                                              LINK-JIKAN
                                              LINK-TOKCD.
************************************************************
 DECLARATIVES.
***
 FILEERR-SEC1           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  CSSTBLF.
     MOVE   "CSSTBLL1"        TO    ERR-FL-ID.
     MOVE    TBL-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 FILEERR-SEC2           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  SEQTENF.
     MOVE   "SEQTENL1"        TO    ERR-FL-ID.
     MOVE    SET-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 FILEERR-SEC3           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  HACYUPPT.
     MOVE   "HACYUPPT"        TO    ERR-FL-ID.
     MOVE    HAC-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 END     DECLARATIVES.
************************************************************
*             基本処理
************************************************************
 PGM-CONTROL                     SECTION.
     PERFORM           100-INIT-SEC.
     PERFORM           200-MAIN-SEC
             UNTIL     END-FLG   =    "END".
     PERFORM           300-END-SEC.
     STOP     RUN.
 PGM-CONTROL-EXT.
     EXIT.
************************************************************
*      １００   初期処理                                   *
************************************************************
 100-INIT-SEC           SECTION.
*
     OPEN         I-O       CSSTBLF.
     OPEN         INPUT     HACYUPPT.
     OPEN         INPUT     SEQTENF.
*    店舗情報ワーク退避
     INITIALIZE             WK-TBL-TENPO.
     MOVE      ZERO     TO   IX.
     PERFORM   TENPO-TBL-SEC  UNTIL  END-FLG1 = "END".
     IF    IX = ZERO
           MOVE  "END"  TO   END-FLG
           GO           TO   100-INIT-END
     END-IF.
*
     PERFORM HACYUPPT-READ-SEC.
*
 100-INIT-END.
     EXIT.
************************************************************
*      ２００   主処理　                                   *
************************************************************
 200-MAIN-SEC           SECTION.
*発注集計表展開データ存在チェック
     PERFORM  CSSTBLF-READ-SEC.
*
     IF   CSSTBLF-INV-FLG = "INV"
          MOVE  SPACE        TO  TBL-REC
          INITIALIZE             TBL-REC
          MOVE  LINK-HIDUKE  TO  TBL-F01
          MOVE  LINK-JIKAN   TO  TBL-F02
          MOVE  LINK-TOKCD   TO  TBL-F03
          MOVE  HAC-F02      TO  TBL-F04
          MOVE  HAC-F03      TO  TBL-F05
          MOVE  HAC-F04      TO  TBL-F06
          MOVE  HAC-F05      TO  TBL-F07
          MOVE  HAC-F06      TO  TBL-F08
          MOVE  HAC-F08      TO  TBL-F09
          MOVE  HAC-F09      TO  TBL-F10
          MOVE  HAC-F10      TO  TBL-F11
*# 2020/07/17 NAV ST
          MOVE  HAC-F18      TO  TBL-F11(2:1)
          MOVE  HAC-F19      TO  TBL-F11(3:1)
          IF  HAC-F17 = "0"
                MOVE  "1"    TO  TBL-F11(1:1)
          END-IF
*# 2020/07/17 NAV ED
          MOVE  HAC-F111     TO  TBL-F121
          MOVE  HAC-F112     TO  TBL-F122
          MOVE  HAC-F12      TO  TBL-F13
          MOVE  HAC-F13      TO  TBL-F14
          PERFORM  TENPO-SET-SEC
          WRITE TBL-REC
          ADD   1            TO  WRT-CNT
     ELSE
*# 2020/07/17 NAV ST
          MOVE  HAC-F18      TO  TBL-F11(2:1)
          MOVE  HAC-F19      TO  TBL-F11(3:1)
          IF  HAC-F17 = "0"
                MOVE  "1"    TO  TBL-F11(1:1)
          END-IF
*# 2020/07/17 NAV ED
          PERFORM  TENPO-SET-SEC
          REWRITE TBL-REC
          ADD   1            TO  REW-CNT
     END-IF.
*
     PERFORM HACYUPPT-READ-SEC.
*
 200-MAIN-SEC-EXT.
     EXIT.
************************************************************
*            店舗テーブルの読込処理
************************************************************
 TENPO-TBL-SEC                     SECTION.
*
     READ    SEQTENF
             AT  END  MOVE  "END"  TO  END-FLG1
             GO                    TO  TENPO-TBL-EXIT
     END-READ.
*
     ADD     1        TO     IX.
     MOVE    SET-F01  TO     TBL-TENPOCD(IX).
     MOVE    SET-F02  TO     TBL-TENPONM(IX).
*
 TENPO-TBL-EXIT.
     EXIT.
************************************************************
*    発注集計表ファイルデータ読込み
************************************************************
 HACYUPPT-READ-SEC                 SECTION.
*
     READ    HACYUPPT
             AT   END
             MOVE      "END"     TO   END-FLG
             GO        TO        HACYUPPT-READ-EXIT
     END-READ.
     ADD     1         TO        HAC-CNT.
*
 HACYUPPT-READ-EXIT.
     EXIT.
************************************************************
*    ＣＳＶ商品ファイル読込み
************************************************************
 CSSTBLF-READ-SEC                  SECTION.
*
     MOVE    LINK-HIDUKE         TO   TBL-F01.
     MOVE    LINK-JIKAN          TO   TBL-F02.
     MOVE    LINK-TOKCD          TO   TBL-F03.
     MOVE    HAC-F02             TO   TBL-F04.
     MOVE    HAC-F03             TO   TBL-F05.
     MOVE    HAC-F04             TO   TBL-F06.
     READ    CSSTBLF
             INVALID     MOVE "INV" TO CSSTBLF-INV-FLG
             NOT INVALID MOVE SPACE TO CSSTBLF-INV-FLG
     END-READ.
*
 CSSTBLF-READ-EXIT.
     EXIT.
************************************************************
*    店舗テーブル検索
************************************************************
 TENPO-SET-SEC                     SECTION.
*
     MOVE    SPACE      TO     OK-FLG.
     PERFORM VARYING IY FROM 1 BY 1 UNTIL TBL-TENPOCD(IY) = ZERO
                                       OR OK-FLG = "OK"
*************DISPLAY "HAC-F06    = " HAC-F06      UPON CONS
*************DISPLAY "TBL-TENPO  = " TBL-TENPOCD(IY)  UPON CONS
             IF   HAC-F07  =  TBL-TENPOCD(IY)
                  MOVE   "OK"    TO   OK-FLG
                  ADD    HAC-F16 TO   TBL-F15(IY)
             END-IF
     END-PERFORM.
*
 TENPO-SET-EXIT.
     EXIT.
************************************************************
*      ３００     終了処理                                 *
************************************************************
 300-END-SEC           SECTION.
     CLOSE             CSSTBLF  HACYUPPT  SEQTENF.
*
     DISPLAY "* HACYUPPT (INPUT) = " HAC-CNT  " *"  UPON CONS.
     DISPLAY "* CVSTBLF  (WRITE) = " WRT-CNT  " *"  UPON CONS.
     DISPLAY "* CVSTBLF(REWRITE) = " REW-CNT  " *"  UPON CONS.
*
 300-END-SEC-EXT.
     EXIT.
*****************<<  PROGRAM  END  >>***********************

```
