# CNV0505B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/CNV0505B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　マスタデータコンバート　　　　　　*
*    モジュール名　　　　：　店舗マスタコピー　　　　          *
*    作成日／更新日　　　：　23/01/23                          *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　店舗ＷＫ⇒店舗Ｍコピー　　　　　　*
*                          　　　　　　　　　　　　            *
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            CNV0505B.
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
****<< 変換済店舗テーブル >>******************************
     SELECT   TENWK     ASSIGN    TO        DA-01-S-TENWK
                        FILE      STATUS    IS   TWK-STATUS.
***************************************************************
 DATA                   DIVISION.
***************************************************************
 FILE                   SECTION.
***************************************************************
****<< 店舗テーブル >>*************************************
 FD  HTENMS.
     COPY     HTENMS   OF        XFDLIB
              JOINING   TEN       PREFIX.
****<< 変換後店舗マスタ >>*********************************
 FD  TENWK
              BLOCK CONTAINS  1   RECORDS.
     COPY     HTENMS   OF        XFDLIB
              JOINING   TWK       PREFIX.
****  作業領域  ************************************************
 WORKING-STORAGE        SECTION.
****************************************************************
****  ステイタス情報          ****
 01  STATUS-AREA.
     02 TEN-STATUS           PIC  X(02).
     02 TWK-STATUS           PIC  X(02).
 01  HTENMS-INV-FLG          PIC  X(03)  VALUE  SPACE.
**** メッセージ情報           ****
 01  MSG-AREA1-1.
     02  MSG-ABEND1.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-PG-ID         PIC  X(08)  VALUE  "CNV0505B".
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
 01  HTENMS-END             PIC  X(03)  VALUE  SPACE.
****  カウント                ****
 01  CNT-AREA.
     03  IN-CNT              PIC  9(07)   VALUE  0.
     03  OUT-CNT1            PIC  9(07)   VALUE  0.
     03  OUT-CNT2            PIC  9(07)   VALUE  0.
     03  CHG-CNT             PIC  9(07)   VALUE  0.
*
************************************************************
 LINKAGE            SECTION.
 01  LINK-TOKCD1        PIC  9(08).
************************************************************
 PROCEDURE              DIVISION       USING  LINK-TOKCD1.
************************************************************
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  HTENMS.
     MOVE   "HTENMS "        TO    ERR-FL-ID.
     MOVE    TEN-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 FILEERR-SEC2           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  TENWK.
     MOVE   "TENWK   "        TO    ERR-FL-ID.
     MOVE    TWK-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
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
     OPEN         I-O       HTENMS
     OPEN         INPUT     TENWK.
*
     PERFORM  TENWK-READ-SEC.
*
 100-INIT-END.
     EXIT.
************************************************************
*      ２００   主処理　                                   *
************************************************************
 200-MAIN-SEC           SECTION.
*
     MOVE  TWK-F52        TO   TEN-F52.
     MOVE  TWK-F011       TO   TEN-F011.
     READ  HTENMS
           INVALID      MOVE "INV" TO  HTENMS-INV-FLG
           NOT INVALID  MOVE SPACE TO  HTENMS-INV-FLG
     END-READ.
*
     IF    HTENMS-INV-FLG = "INV"
           MOVE  SPACE          TO   TEN-REC
           INITIALIZE                TEN-REC
           MOVE  TWK-REC        TO   TEN-REC
           WRITE TEN-REC
           ADD   1              TO   OUT-CNT1
     ELSE
           MOVE  TWK-REC        TO   TEN-REC
           REWRITE TEN-REC
           ADD   1              TO   OUT-CNT2
     END-IF.
*
     PERFORM TENWK-READ-SEC.
*
 200-MAIN-SEC-EXT.
     EXIT.
************************************************************
*            店舗テーブルの読込処理
************************************************************
 TENWK-READ-SEC                   SECTION.
     READ    TENWK
             AT   END
             MOVE      "END"     TO   END-FLG
             GO        TO        TENWK-READ-EXT
     END-READ.
     ADD     1         TO        IN-CNT.
*
     EVALUATE  TWK-F52
         WHEN  8737 MOVE 87373 TO  TWK-F52
     END-EVALUATE.
*
 TENWK-READ-EXT.
     EXIT.
************************************************************
*      ３００     終了処理                                 *
************************************************************
 300-END-SEC           SECTION.
     CLOSE             HTENMS   TENWK.
*
     DISPLAY "* TENWK  (INPUT) = " IN-CNT   " *"  UPON CONS.
     DISPLAY "* HTENMS (WRITE )= " OUT-CNT1 " *"  UPON CONS.
     DISPLAY "* HTENMS(REWRITE)= " OUT-CNT2 " *"  UPON CONS.
*
 300-END-SEC-EXT.
     EXIT.
*****************<<  PROGRAM  END  >>***********************

```
