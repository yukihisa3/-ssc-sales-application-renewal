# CSV0016B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/CSV0016B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　ＥＸＣＥＬ連携                    *
*    モジュール名　　　　：　店舗名Ｆ作成　　　　　　　　　　　*
*    作成日／更新日　　　：　03/07/15                          *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    作成日／更新日　　　：　06/09/04                          *
*    作成者／更新者　　　：　ＮＡＶ松野　　　　　　　　　　　　*
*    処理概要　　　　　　：　パラメタより受取った取引先の店舗名*
*                          Ｆを作成する。　　　　　　　　　　　*
*                                                            　*
*    店舗が２０７店舗以上バージョン　　　　　　　　　　　　　　*
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            CSV0016B.
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
****<< 店舗名ワーク >>************************************
     SELECT   HACTEN  ASSIGN    TO        DA-01-S-HACTEN
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
****<< 発注集計データ >>***********************************
 FD  HACTEN
              BLOCK CONTAINS   8  RECORDS.
     COPY     HACTEN OF        XFDLIB
              JOINING   HAC       PREFIX.
****  作業領域  ************************************************
 WORKING-STORAGE        SECTION.
****************************************************************
****  ステイタス情報          ****
 01  STATUS-AREA.
     02 TEN-STATUS           PIC  X(02).
     02 HAC-STATUS           PIC  X(02).
**** メッセージ情報           ****
 01  MSG-AREA1-1.
     02  MSG-ABEND1.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-PG-ID         PIC  X(08)  VALUE  "CSV0016B".
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
 01  CHK-FLG                 PIC  X(03)  VALUE  SPACE.
 01  TENPO-END               PIC  X(03)  VALUE  SPACE.
 01  CSVSYOF-INV-FLG         PIC  X(03)  VALUE  SPACE.
 01  IX                      PIC  9(02)  VALUE  ZERO.
 01  IY                      PIC  9(02)  VALUE  ZERO.
 01  TBL-FLG                 PIC  9(01)  VALUE  ZERO.
****  カウント                ****
 01  CNT-AREA.
     03  HAC-CNT             PIC  9(07)   VALUE  0.
     03  TEN-CNT             PIC  9(07)   VALUE  0.
     03  WRT-CNT             PIC  9(07)   VALUE  0.
     03  RWT-CNT             PIC  9(07)   VALUE  0.
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
                   PROCEDURE  HACTEN.
     MOVE   "HACTEN  "        TO    ERR-FL-ID.
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
     OPEN         OUTPUT    HACTEN.
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
     PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > 20
             MOVE    SPACE   TO   HAC-REC
             INITIALIZE           HAC-REC
             MOVE    IX      TO   HAC-F01
             MOVE    SPACE   TO   CHK-FLG
             PERFORM VARYING IY FROM 1 BY 1 UNTIL IY > 30
                                               OR CHK-FLG = "CHK"
*            合計行の為、７列目の２９、３０にはｾｯﾄしない
                     IF  IX  =  7
                     AND IY  =  29
                         MOVE "CHK"       TO  CHK-FLG
                     ELSE
                         IF  END-FLG NOT = "END"
                             MOVE  TEN-F011   TO  HAC-F021(IY)
                             MOVE  TEN-F03    TO  HAC-F022(IY)
                         ELSE
                             MOVE  ZERO       TO  HAC-F021(IY)
                             MOVE  SPACE      TO  HAC-F022(IY)
                         END-IF
                         PERFORM   HTENMS-READ-SEC
                     END-IF
             END-PERFORM
             WRITE  HAC-REC
             ADD    1                     TO  HAC-CNT
     END-PERFORM.
*
 100-INIT-END.
     EXIT.
************************************************************
*            店舗テーブルの読込処理
************************************************************
 HTENMS-READ-SEC                   SECTION.
*
     IF      END-FLG = "END"
             GO        TO        HTENMS-READ-EXT
     END-IF.
*
     READ    HTENMS
        NEXT   AT   END
             MOVE      "END"     TO   END-FLG
             GO        TO        HTENMS-READ-EXT
     END-READ.
     ADD     1         TO        TEN-CNT
*
     IF      LINK-TOKCD1  NOT =  TEN-F52
             MOVE      "END"     TO   END-FLG
     END-IF.
*対象外区分
     IF      TEN-F76  =  "1"
             GO                  TO   HTENMS-READ-SEC
     END-IF.
*
 HTENMS-READ-EXT.
     EXIT.
************************************************************
*      ３００     終了処理                                 *
************************************************************
 300-END-SEC           SECTION.
     CLOSE             HTENMS   HACTEN.
*
     DISPLAY "* HTENMS   (INPUT) = " TEN-CNT  " *"  UPON CONS.
     DISPLAY "* HACTEN (WRITE) = " HAC-CNT  " *"  UPON CONS.
*
 300-END-SEC-EXT.
     EXIT.
*****************<<  PROGRAM  END  >>***********************

```
