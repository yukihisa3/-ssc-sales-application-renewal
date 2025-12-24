# CSV0035B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/CSV0035B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　ＥＸＣＥＬ連携                    *
*    モジュール名　　　　：　商品ＴＢＬ分割                    *
*    作成日／更新日　　　：　03/07/16                          *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    作成日／更新日　　　：　06/09/04                          *
*    作成者／更新者　　　：　ＮＡＶ松野　　　　　　　　　　　　*
*    処理概要　　　　　　：　発注集計Ｆを読み、商品ＴＢＬＦを　*
*                          分割する。　　　　　　　　　　　　　*
*                                                              *
*    2006/09/04            ＯＣＣＵＲＳ２５⇒３０へ変更　　    *
*    2006/10/13            ２０７店舗以上対応　　　　　　　    *
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            CSV0035B.
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
****<< 商品ＴＢＬワーク >>************************************
     SELECT   CSYOF  ASSIGN    TO        DA-01-VI-CSYOL1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   SEQUENTIAL
                        RECORD    KEY       IS   CSV-F01
                                                 CSV-F02
                                                 CSV-F03
                                                 CSV-F04
                                                 CSV-F05
                        FILE      STATUS    IS   CSV-STATUS.
****<< 発注集計データ >>**********************************
     SELECT   CSYOSF     ASSIGN    TO        DA-01-S-CSYOSF
                        FILE      STATUS    IS   SYO-STATUS.
***************************************************************
 DATA                   DIVISION.
***************************************************************
 FILE                   SECTION.
***************************************************************
****<< 商品ＴＢＬワーク >>*********************************
 FD  CSYOF.
     COPY     CSYOF  OF        XFDLIB
              JOINING  CSV       PREFIX.
****<< 発注集計データ >>***********************************
 FD  CSYOSF
              BLOCK CONTAINS  12  RECORDS.
     COPY     CSYOSF OF        XFDLIB
              JOINING  SYO       PREFIX.
****  作業領域  ************************************************
 WORKING-STORAGE        SECTION.
****************************************************************
****  ステイタス情報          ****
 01  STATUS-AREA.
     02 CSV-STATUS           PIC  X(02).
     02 SYO-STATUS           PIC  X(02).
**** メッセージ情報           ****
 01  MSG-AREA1-1.
     02  MSG-ABEND1.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-PG-ID         PIC  X(08)  VALUE  "CSV0035B".
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
 01  IX                      PIC  9(03)  VALUE  ZERO.
 01  IY                      PIC  9(03)  VALUE  ZERO.
 01  IZ                      PIC  9(04)  VALUE  ZERO.
 01  IA                      PIC  9(01)  VALUE  ZERO.
****  カウント                ****
 01  CNT-AREA.
     03  SYO-CNT             PIC  9(07)   VALUE  ZERO.
     03  CSV-CNT             PIC  9(07)   VALUE  ZERO.
****  キー退避                ****
 01  KEY-BACKUP.
     03  WK-CSV-F02          PIC  X(02)   VALUE  SPACE.
     03  WK-CSV-F03          PIC  9(08)   VALUE  ZERO.
*
************************************************************
 PROCEDURE              DIVISION.
************************************************************
 DECLARATIVES.
***
 FILEERR-SEC1           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  CSYOF.
     MOVE   "CSYOL1"        TO    ERR-FL-ID.
     MOVE    CSV-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 FILEERR-SEC2           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  CSYOSF.
     MOVE   "CSYOSF"        TO    ERR-FL-ID.
     MOVE    SYO-STATUS       TO    ERR-STCD.
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
     OPEN         OUTPUT    CSYOSF.
     OPEN         INPUT     CSYOF.
*
     PERFORM CSYOF-READ-SEC.
*
 100-INIT-END.
     EXIT.
************************************************************
*      ２００   主処理　                                   *
************************************************************
 200-MAIN-SEC           SECTION.
*
     MOVE     SPACE       TO  SYO-REC.
     INITIALIZE               SYO-REC.
*
*----< 2006/09/04 START >----*
     IF       IA  >=  6
*----< 2006/09/04  END  >----*
     OR       CSV-F02  NOT =  WK-CSV-F02
     OR       CSV-F03  NOT =  WK-CSV-F03
         IF   IZ  =  ZERO
              ADD  1       TO IZ
         ELSE
              ADD  2       TO IZ
         END-IF
              MOVE ZERO    TO IA
              MOVE CSV-F02 TO WK-CSV-F02
              MOVE CSV-F03 TO WK-CSV-F03
     END-IF.
*
     ADD      1           TO  IA.
     MOVE     CSV-F01     TO  SYO-F01.
     MOVE     CSV-F02     TO  SYO-F02.
     MOVE     CSV-F03     TO  SYO-F03.
     MOVE     CSV-F04     TO  SYO-F04.
     MOVE     CSV-F05     TO  SYO-F05.
     MOVE     IZ          TO  SYO-F07.
     MOVE     CSV-F06     TO  SYO-F08.
     MOVE     CSV-F07     TO  SYO-F09.
     MOVE     CSV-F08     TO  SYO-F10.
     MOVE     CSV-F091    TO  SYO-F111.
     MOVE     CSV-F092    TO  SYO-F112.
     MOVE     CSV-F10     TO  SYO-F12.
     MOVE     CSV-F11     TO  SYO-F13.
*****１レコード目
     MOVE     1           TO  IY.
     MOVE     1           TO  SYO-F06  SYO-F15.
     PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > 30
             MOVE   CSV-F12(IY)  TO  SYO-F14(IX)
             ADD    1            TO  IY
     END-PERFORM.
*
     WRITE   SYO-REC.
     ADD     1                   TO  SYO-CNT.
*****２レコード目
     MOVE     31          TO  IY.
     MOVE     2           TO  SYO-F06  SYO-F15.
     PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > 30
             MOVE   CSV-F12(IY)  TO  SYO-F14(IX)
             ADD    1            TO  IY
     END-PERFORM.
*
     WRITE   SYO-REC.
     ADD     1                   TO  SYO-CNT.
*****３レコード目
     MOVE     61          TO  IY.
     MOVE     3           TO  SYO-F06  SYO-F15.
     PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > 30
             MOVE   CSV-F12(IY)  TO  SYO-F14(IX)
             ADD    1            TO  IY
     END-PERFORM.
*
     WRITE   SYO-REC.
     ADD     1                   TO  SYO-CNT.
*****４レコード目
     MOVE     91          TO  IY.
     MOVE     4           TO  SYO-F06  SYO-F15.
     PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > 30
             MOVE   CSV-F12(IY)  TO  SYO-F14(IX)
             ADD    1            TO  IY
     END-PERFORM.
*
     WRITE   SYO-REC.
     ADD     1                   TO  SYO-CNT.
*****５レコード目
     MOVE     121         TO  IY.
     MOVE     5           TO  SYO-F06  SYO-F15.
     PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > 30
             MOVE   CSV-F12(IY)  TO  SYO-F14(IX)
             ADD    1            TO  IY
     END-PERFORM.
*
     WRITE   SYO-REC.
     ADD     1                   TO  SYO-CNT.
*****６レコード目
     MOVE     151         TO  IY.
     MOVE     6           TO  SYO-F06  SYO-F15.
     PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > 30
             MOVE   CSV-F12(IY)  TO  SYO-F14(IX)
             ADD    1            TO  IY
     END-PERFORM.
*
     WRITE   SYO-REC.
     ADD     1                   TO  SYO-CNT.
*****７レコード目
     MOVE     181         TO  IY.
     MOVE     7           TO  SYO-F06  SYO-F15.
     PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > 30
             MOVE   CSV-F12(IY)  TO  SYO-F14(IX)
             ADD    1            TO  IY
     END-PERFORM.
*
     WRITE   SYO-REC.
     ADD     1                   TO  SYO-CNT.
*****８レコード目
*****ADD      1           TO  IZ.
*****MOVE     IZ          TO  SYO-F07.
     COMPUTE  SYO-F07  =  IZ  +  1.
     MOVE     208         TO  IY.
     MOVE     211         TO  IY.
     MOVE     1           TO  SYO-F06.
     MOVE     8           TO  SYO-F15.
     PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > 30
             MOVE   CSV-F12(IY)  TO  SYO-F14(IX)
             ADD    1            TO  IY
     END-PERFORM.
*
     WRITE   SYO-REC.
     ADD     1                   TO  SYO-CNT.
*****９レコード目
     MOVE     239         TO  IY.
     MOVE     241         TO  IY.
     MOVE     2           TO  SYO-F06.
     MOVE     9           TO  SYO-F15.
     PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > 30
             MOVE   CSV-F12(IY)  TO  SYO-F14(IX)
             ADD    1            TO  IY
     END-PERFORM.
*
     WRITE   SYO-REC.
     ADD     1                   TO  SYO-CNT.
*****１０レコード目
     MOVE     269         TO  IY.
     MOVE     271         TO  IY.
     MOVE     3           TO  SYO-F06.
     MOVE     10          TO  SYO-F15.
     PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > 30
             MOVE   CSV-F12(IY)  TO  SYO-F14(IX)
             ADD    1            TO  IY
     END-PERFORM.
*
     WRITE   SYO-REC.
     ADD     1                   TO  SYO-CNT.
*****１１レコード目
     MOVE     299         TO  IY.
     MOVE     301         TO  IY.
     MOVE     4           TO  SYO-F06.
     MOVE     11          TO  SYO-F15.
     PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > 30
             MOVE   CSV-F12(IY)  TO  SYO-F14(IX)
             ADD    1            TO  IY
     END-PERFORM.
*
     WRITE   SYO-REC.
     ADD     1                   TO  SYO-CNT.
*****１２レコード目
     MOVE     329         TO  IY.
     MOVE     331         TO  IY.
     MOVE     5           TO  SYO-F06.
     MOVE     12          TO  SYO-F15.
     PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > 30
             MOVE   CSV-F12(IY)  TO  SYO-F14(IX)
             ADD    1            TO  IY
     END-PERFORM.
*
     WRITE   SYO-REC.
     ADD     1                   TO  SYO-CNT.
*****１３レコード目
     MOVE     359         TO  IY.
     MOVE     361         TO  IY.
     MOVE     6           TO  SYO-F06.
     MOVE     13          TO  SYO-F15.
     PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > 30
             MOVE   CSV-F12(IY)  TO  SYO-F14(IX)
             ADD    1            TO  IY
     END-PERFORM.
*
     WRITE   SYO-REC.
     ADD     1                   TO  SYO-CNT.
*****１４レコード目
     MOVE     389         TO  IY.
     MOVE     391         TO  IY.
     MOVE     7           TO  SYO-F06.
     MOVE     14          TO  SYO-F15.
     PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > 30
             MOVE   CSV-F12(IY)  TO  SYO-F14(IX)
             ADD    1            TO  IY
     END-PERFORM.
*
     WRITE   SYO-REC.
     ADD     1                   TO  SYO-CNT.
*
     PERFORM CSYOF-READ-SEC.
*
 200-MAIN-SEC-EXT.
     EXIT.
************************************************************
*    発注集計表ファイルデータ読込み
************************************************************
 CSYOF-READ-SEC                 SECTION.
*
     READ    CSYOF
             AT   END
             MOVE      "END"     TO   END-FLG
             GO        TO        CSYOF-READ-EXIT
     END-READ.
     ADD     1         TO        CSV-CNT.
*
 CSYOF-READ-EXIT.
     EXIT.
************************************************************
*      ３００     終了処理                                 *
************************************************************
 300-END-SEC           SECTION.
     CLOSE             CSYOF  CSYOSF.
*
     DISPLAY "* CSYOF  (INPUT) = " CSV-CNT  " *"  UPON CONS.
     DISPLAY "* CSYOSF (WRITE) = " SYO-CNT  " *"  UPON CONS.
*
 300-END-SEC-EXT.
     EXIT.
*****************<<  PROGRAM  END  >>***********************

```
