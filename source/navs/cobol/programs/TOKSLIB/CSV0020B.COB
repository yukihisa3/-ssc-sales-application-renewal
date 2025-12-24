****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　ＥＸＣＥＬ連携                    *
*    モジュール名　　　　：　商品ＴＢＬ作成                    *
*    作成日／更新日　　　：　03/07/14                          *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　発注集計Ｆを読み、商品ＴＢＬＦを　*
*                          作成する。　　　　　　　　　　　　　*
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            CSV0020B.
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
                        RECORD    KEY       IS   TEN-F52
                                                 TEN-F011
                        FILE      STATUS    IS   TEN-STATUS.
****<< 商品ＴＢＬワーク >>************************************
     SELECT   CSVSYOF  ASSIGN    TO        DA-01-VI-CSVSYOL1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   CSV-F01
                                                 CSV-F02
                                                 CSV-F03
                                                 CSV-F04
                                                 CSV-F05
                        FILE      STATUS    IS   CSV-STATUS.
****<< 発注集計データ >>**********************************
     SELECT   HACYUPRT     ASSIGN    TO        DA-01-S-HACYUPRT
                        FILE      STATUS    IS   HAC-STATUS.
***************************************************************
 DATA                   DIVISION.
***************************************************************
 FILE                   SECTION.
***************************************************************
****<< 店舗テーブル >>*************************************
 FD  HTENMS.
     COPY     HTENMS   OF        XFDLIB
              JOINING   TEN       PREFIX.
****<< 商品ＴＢＬワーク >>*********************************
 FD  CSVSYOF.
     COPY     CSVSYOF  OF        XFDLIB
              JOINING   CSV       PREFIX.
****<< 発注集計データ >>***********************************
 FD  HACYUPRT
              BLOCK CONTAINS  27  RECORDS.
     COPY     HACYUPRT OF        XFDLIB
              JOINING   HAC       PREFIX.
****  作業領域  ************************************************
 WORKING-STORAGE        SECTION.
****************************************************************
****  ステイタス情報          ****
 01  STATUS-AREA.
     02 TEN-STATUS           PIC  X(02).
     02 CSV-STATUS           PIC  X(02).
     02 HAC-STATUS           PIC  X(02).
**** メッセージ情報           ****
 01  MSG-AREA1-1.
     02  MSG-ABEND1.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-PG-ID         PIC  X(08)  VALUE  "CSV0020B".
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
 01  CSVSYOF-INV-FLG         PIC  X(03)  VALUE  SPACE.
 01  IX                      PIC  9(03)  VALUE  ZERO.
 01  TBL-FLG                 PIC  9(01)  VALUE  ZERO.
****  カウント                ****
 01  CNT-AREA.
     03  HAC-CNT             PIC  9(07)   VALUE  0.
     03  TEN-CNT             PIC  9(07)   VALUE  0.
     03  WRT-CNT             PIC  9(07)   VALUE  0.
     03  RWT-CNT             PIC  9(07)   VALUE  0.
****  店舗格納ワーク          ****
 01  WK-TBL-TENPO.
     03  TBL-TENPO           PIC  X(05)  OCCURS  200.
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
                   PROCEDURE  CSVSYOF.
     MOVE   "CSVSYOL1"        TO    ERR-FL-ID.
     MOVE    CSV-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 FILEERR-SEC3           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  HACYUPRT.
     MOVE   "HACYUPRT"        TO    ERR-FL-ID.
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
     OPEN         INPUT     HTENMS
     OPEN         INPUT     HACYUPRT.
     OPEN         I-O       CSVSYOF.
*
     MOVE         ZERO          TO   WK-TBL-TENPO.
*
     MOVE         LINK-TOKCD1   TO   TEN-F52.
     MOVE         ZERO          TO   TEN-F011.
     START  HTENMS  KEY  IS  >=  TEN-F52 TEN-F011
            INVALID
            DISPLAY  NC"対象データ無し" UPON CONS
            MOVE     "END"      TO   END-FLG
            GO                  TO   100-INIT-END
     END-START.
*
     PERFORM  HTENMS-READ-SEC.
     IF       TENPO-END  =  "END"
              DISPLAY  NC"対象データ無し" UPON CONS
              MOVE     "END"      TO   END-FLG
              GO                  TO   100-INIT-END
     END-IF.
*
     PERFORM VARYING IX FROM 1 BY 1 UNTIL TENPO-END = "END"
             MOVE  TEN-F011  TO  TBL-TENPO(IX)
             PERFORM  HTENMS-READ-SEC
     END-PERFORM.
*
     PERFORM HACYUPRT-READ-SEC.
*
 100-INIT-END.
     EXIT.
************************************************************
*      ２００   主処理　                                   *
************************************************************
 200-MAIN-SEC           SECTION.
*ＣＳＶﾌｧｲﾙ存在ﾁｪｯｸ
     PERFORM  CSVSYOF-READ-SEC.
*
     IF   CSVSYOF-INV-FLG = "INV"
          MOVE  SPACE     TO  CSV-REC
          INITIALIZE          CSV-REC
          MOVE  HAC-F01   TO  CSV-F01
          MOVE  HAC-F02   TO  CSV-F02
          MOVE  HAC-F03   TO  CSV-F03
          MOVE  HAC-F04   TO  CSV-F04
          MOVE  HAC-F05   TO  CSV-F05
          MOVE  HAC-F07   TO  CSV-F06
          MOVE  HAC-F08   TO  CSV-F07
          MOVE  HAC-F09   TO  CSV-F08
          MOVE  HAC-F101  TO  CSV-F091
          MOVE  HAC-F102  TO  CSV-F092
          MOVE  HAC-F11   TO  CSV-F10
          MOVE  HAC-F12   TO  CSV-F11
*         店舗ﾃｰﾌﾞﾙよりﾃｰﾌﾞﾙ位置検索
          PERFORM  TENPO-TBL-SEC
          IF   TBL-FLG  =  1
               WRITE CSV-REC
               ADD      1      TO   WRT-CNT
          END-IF
     ELSE
*         店舗ﾃｰﾌﾞﾙよりﾃｰﾌﾞﾙ位置検索
          PERFORM  TENPO-TBL-SEC
          IF   TBL-FLG  =  1
               REWRITE CSV-REC
               ADD      1      TO   RWT-CNT
          END-IF
     END-IF.
*
     PERFORM HACYUPRT-READ-SEC.
*
 200-MAIN-SEC-EXT.
     EXIT.
************************************************************
*            店舗テーブルの読込処理
************************************************************
 HTENMS-READ-SEC                   SECTION.
*
     READ    HTENMS
        NEXT   AT   END
             MOVE      "END"     TO   TENPO-END
             GO        TO        HTENMS-READ-EXT
     END-READ.
     ADD     1         TO        TEN-CNT
*
     IF      LINK-TOKCD1  NOT =  TEN-F52
             MOVE      "END"     TO   TENPO-END
             GO        TO        HTENMS-READ-EXT
     END-IF.
*対象外区分
     IF      TEN-F76  =  "1"
             GO                  TO   HTENMS-READ-SEC
     END-IF.
*
 HTENMS-READ-EXT.
     EXIT.
************************************************************
*    発注集計表ファイルデータ読込み
************************************************************
 HACYUPRT-READ-SEC                 SECTION.
*
     READ    HACYUPRT
             AT   END
             MOVE      "END"     TO   END-FLG
             GO        TO        HACYUPRT-READ-EXIT
     END-READ.
     ADD     1         TO        HAC-CNT.
*
 HACYUPRT-READ-EXIT.
     EXIT.
************************************************************
*    ＣＳＶ商品ファイル読込み
************************************************************
 CSVSYOF-READ-SEC                  SECTION.
*
     MOVE    HAC-F01             TO   CSV-F01.
     MOVE    HAC-F02             TO   CSV-F02.
     MOVE    HAC-F03             TO   CSV-F03.
     MOVE    HAC-F04             TO   CSV-F04.
     MOVE    HAC-F05             TO   CSV-F05.
     READ    CSVSYOF
             INVALID     MOVE "INV" TO CSVSYOF-INV-FLG
             NOT INVALID MOVE SPACE TO CSVSYOF-INV-FLG
     END-READ.
*
 CVSSYOF-READ-EXIT.
     EXIT.
************************************************************
*    店舗番号検索
************************************************************
 TENPO-TBL-SEC                     SECTION.
*
     MOVE    ZERO                TO   TBL-FLG.
     PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > 200
                                       OR TBL-FLG = 1
*    IF  HAC-F04 = "4943421002955"
*    AND HAC-F06 = "00194"
*        DISPLAY "TBL-TENPO = " TBL-TENPO(IX) UPON CONS
*        DISPLAY "HAC-F06   = " HAC-F06       UPON CONS
*        DISPLAY "IX        = " IX            UPON CONS
*    END-IF
             IF  TBL-TENPO(IX) = HAC-F06
                 ADD   HAC-F15   TO   CSV-F12(IX)
                 MOVE  1         TO   TBL-FLG
             END-IF
     END-PERFORM.
*
 TENPO-TBL-EXIT.
     EXIT.
************************************************************
*      ３００     終了処理                                 *
************************************************************
 300-END-SEC           SECTION.
     CLOSE             HTENMS   HACYUPRT  CSVSYOF.
*
     DISPLAY "* HTENMS   (INPUT) = " TEN-CNT  " *"  UPON CONS.
     DISPLAY "* HACYUPRT (INPUT) = " HAC-CNT  " *"  UPON CONS.
     DISPLAY "* CSVSYOF  (WRITE) = " WRT-CNT  " *"  UPON CONS.
     DISPLAY "* CSVSYOF(REWRITE) = " RWT-CNT  " *"  UPON CONS.
*
 300-END-SEC-EXT.
     EXIT.
*****************<<  PROGRAM  END  >>***********************
