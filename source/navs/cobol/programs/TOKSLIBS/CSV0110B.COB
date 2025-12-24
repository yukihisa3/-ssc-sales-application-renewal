****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　ＥＸＣＥＬ連携                    *
*    モジュール名　　　　：　店舗順作成　　　                  *
*    作成日／更新日　　　：　06/10/10                          *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　発注集計Ｆを読み、店舗順Ｆを作成　*
*                          する。　　　　　　　　　　　　　　　*
*                          　　　　　　　　　　　　　　　　　　*
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            CSV0110B.
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
****<< 店舗マスタ >>******************************************
     SELECT   HTENMS   ASSIGN    TO        DA-01-VI-TENMS1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   TEN-F52
                                                 TEN-F011
                        FILE      STATUS    IS   TEN-STATUS.
****<< 店舗順ワーク >>****************************************
     SELECT   SEQTENF  ASSIGN    TO        DA-01-VI-SEQTENL1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
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
 FD  HTENMS.
     COPY     HTENMS   OF        XFDLIB
              JOINING   TEN       PREFIX.
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
     02 TEN-STATUS           PIC  X(02).
     02 SET-STATUS           PIC  X(02).
     02 HAC-STATUS           PIC  X(02).
**** メッセージ情報           ****
 01  MSG-AREA1-1.
     02  MSG-ABEND1.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-PG-ID         PIC  X(08)  VALUE  "CSV0110B".
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
 01  TENPO-END               PIC  X(03)  VALUE  SPACE.
 01  HTENMS-INV-FLG          PIC  X(03)  VALUE  SPACE.
 01  SEQTENF-INV-FLG         PIC  X(03)  VALUE  SPACE.
 01  IX                      PIC  9(03)  VALUE  ZERO.
 01  TBL-FLG                 PIC  9(01)  VALUE  ZERO.
****  カウント                ****
 01  CNT-AREA.
     03  SET-CNT             PIC  9(07)   VALUE  0.
     03  HAC-CNT             PIC  9(07)   VALUE  0.
*
************************************************************
 LINKAGE            SECTION.
 01  LINK-TOKCD1        PIC  9(08).
************************************************************
 PROCEDURE              DIVISION       USING  LINK-TOKCD1.
************************************************************
 DECLARATIVES.
***
 FILEERR-SEC1           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  HTENMS.
     MOVE   "HTENMS "         TO    ERR-FL-ID.
     MOVE    TEN-STATUS       TO    ERR-STCD.
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
     OPEN         INPUT     HTENMS.
     OPEN         INPUT     HACYUPPT.
     OPEN         I-O       SEQTENF.
*
     PERFORM HACYUPPT-READ-SEC.
*
 100-INIT-END.
     EXIT.
************************************************************
*      ２００   主処理　                                   *
************************************************************
 200-MAIN-SEC           SECTION.
*店舗順ワーク存在チェック
     PERFORM  SEQTENF-READ-SEC.
*
     IF   SEQTENF-INV-FLG = "INV"
          MOVE  SPACE     TO  SET-REC
          INITIALIZE          SET-REC
          MOVE  HAC-F07   TO  SET-F01
          PERFORM  HTENMS-READ-SEC
          IF  HTENMS-INV-FLG = "INV"
              MOVE  ALL NC"＊"   TO  SET-F02
          ELSE
              MOVE  TEN-F03      TO  SET-F02
          END-IF
          WRITE SET-REC
          ADD   1         TO  SET-CNT
     END-IF.
*
     PERFORM HACYUPPT-READ-SEC.
*
 200-MAIN-SEC-EXT.
     EXIT.
************************************************************
*            店舗テーブルの読込処理
************************************************************
 HTENMS-READ-SEC                   SECTION.
*
     MOVE    LINK-TOKCD1        TO    TEN-F52.
     MOVE    HAC-F07            TO    TEN-F011.
     READ    HTENMS
             INVALID     MOVE  "INV"  TO  HTENMS-INV-FLG
             NOT INVALID MOVE  SPACE  TO  HTENMS-INV-FLG
     END-READ.
*
 HTENMS-READ-EXT.
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
 SEQTENF-READ-SEC                  SECTION.
*
     MOVE    HAC-F07             TO   SET-F01.
     READ    SEQTENF
             INVALID     MOVE "INV" TO SEQTENF-INV-FLG
             NOT INVALID MOVE SPACE TO SEQTENF-INV-FLG
     END-READ.
*
 SEQTENF-READ-EXIT.
     EXIT.
************************************************************
*      ３００     終了処理                                 *
************************************************************
 300-END-SEC           SECTION.
     CLOSE             HTENMS   HACYUPPT  SEQTENF.
*
     DISPLAY "* HACYUPPT (INPUT) = " HAC-CNT  " *"  UPON CONS.
     DISPLAY "* SEQTENF  (WRITE) = " SET-CNT  " *"  UPON CONS.
*
 300-END-SEC-EXT.
     EXIT.
*****************<<  PROGRAM  END  >>***********************
