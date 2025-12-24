# SPD9996B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SPD9996B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　種子販売実績システム　　　　　　　*
*    モジュール名　　　　：　在庫マスタ件数カウント　　　　　　*
*    作成日／更新日　　　：　09/12/28                          *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　在庫マスタを読み、在庫マスタの件数*
*                          をカウントする。　　　　　　　      *
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SPD9996B.
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
****<< 在庫マスタ       >>************************************
     SELECT   ZAMZAIF   ASSIGN    TO        DA-01-VI-ZAMZAIL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       ZAI-F01
                                            ZAI-F02
                                            ZAI-F03
                        FILE      STATUS    IS   ZAI-STATUS.
***************************************************************
 DATA                   DIVISION.
***************************************************************
 FILE                   SECTION.
***************************************************************
****<< 在庫マスタ >>*******************************************
 FD  ZAMZAIF            LABEL     RECORD     IS  STANDARD.
     COPY     ZAMZAIF   OF   XFDLIB    JOINING   ZAI  AS   PREFIX.
****  作業領域  ************************************************
 WORKING-STORAGE        SECTION.
****************************************************************
****  ステイタス情報          ****
 01  STATUS-AREA.
     02 ZAI-STATUS           PIC  X(02).
*システム日付の編集
 01  SYS-DATE          PIC 9(06).
 01  SYS-DATEW         PIC 9(08).
****  日付保存                ****
 01  SYSTEM-HIZUKE.
     02  SYSYMD              PIC  9(8).
**** メッセージ情報           ****
 01  MSG-AREA1-1.
     02  MSG-ABEND1.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-PG-ID         PIC  X(08)  VALUE  "SPD9996B".
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
****  カウント                ****
 01  CNT-AREA.
     03  READ-CNT            PIC  9(09)   VALUE  0.
*
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
 LINKAGE               SECTION.
 01  PARA-KENSU              PIC 9(09).
************************************************************
 PROCEDURE             DIVISION USING PARA-KENSU.
************************************************************
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  ZAMZAIF.
     MOVE   "ZAMZAIF "        TO    ERR-FL-ID.
     MOVE    ZAI-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP    RUN.
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
     OPEN         INPUT     ZAMZAIF.
*    在庫マスタの読込み
     PERFORM  ZAMZAIF-READ-SEC.
*
 100-INIT-END.
     EXIT.
************************************************************
*      ２００   主処理　                                   *
************************************************************
 200-MAIN-SEC           SECTION.
*
     ADD   1              TO   READ-CNT.
*
     PERFORM ZAMZAIF-READ-SEC.
*
 200-MAIN-SEC-EXT.
     EXIT.
************************************************************
*            在庫マスタの読込処理
************************************************************
 ZAMZAIF-READ-SEC                   SECTION.
     READ    ZAMZAIF
             AT   END
             MOVE      "END"     TO   END-FLG
             GO        TO        ZAMZAIF-READ-EXT
     END-READ.
*
 ZAMZAIF-READ-EXT.
     EXIT.
************************************************************
*      ３００     終了処理                                 *
************************************************************
 300-END-SEC           SECTION.
*
     MOVE    READ-CNT  TO        PARA-KENSU.
*
     CLOSE             ZAMZAIF.
*
     DISPLAY  "## ZAMZAIF READ CNT = " READ-CNT " ##" UPON CONS.
*
 300-END-SEC-EXT.
     EXIT.
*****************<<  PROGRAM  END  >>***********************

```
