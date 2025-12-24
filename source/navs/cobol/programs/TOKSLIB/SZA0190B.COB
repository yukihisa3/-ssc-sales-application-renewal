****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　在庫管理システム　　　　　　　　　*
*    モジュール名　　　　：　期末繰越　　　　　　　　　　　　　*
*    作成日／更新日　　　：　00/05/15                          *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　当年実績を前年実績にシフトし，　　*
*                            当年実績をクリアする．　　　　　　*
*                                                              *
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SZA0190B.
 AUTHOR.                NAV.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FACOM-K150.
 OBJECT-COMPUTER.       FACOM-K150.
 SPECIAL-NAMES.
         STATION   IS   STAT
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*---<<  商品在庫マスタ（実績）  >>---*
     SELECT   ZAMJISF   ASSIGN    TO        DA-01-VS-ZAMJISF
                        ORGANIZATION        IS   SEQUENTIAL
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   ZAI-STATUS.
*
 DATA                   DIVISION.
 FILE                   SECTION.
*---<<  商品在庫マスタ  >>---*
 FD  ZAMJISF.
     COPY     ZAMJISF    OF        XFDLIB
              JOINING   ZAI       PREFIX.
****  作業領域  ***
 WORKING-STORAGE             SECTION.
****  ステイタス情報  ***
 01  STATUS-AREA.
     02  ZAI-STATUS          PIC  X(02).
****  フラグ  ***
 01  PSW-AREA.
     02  END-FLG             PIC  X(03)  VALUE SPACE.
 01  CNT-AREA.
     02  READ-CNT            PIC  9(07)  VALUE ZERO.
     02  REWRITE-CNT         PIC  9(07)  VALUE ZERO.
 01  INDEX-AREA.
     02  IX                  PIC  9(02)  VALUE ZERO.
**** メッセージ情報  ***
 01  MSG-AREA1-1.
     02  MSG-ABEND1.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-PG-ID         PIC  X(08)  VALUE  "SZA0190B".
       03  FILLER            PIC  X(10)  VALUE  " ABEND ###".
     02  MSG-ABEND2.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-FL-ID         PIC  X(08).
       03  FILLER            PIC  X(04)  VALUE  " ST-".
       03  ERR-STCD          PIC  X(02).
       03  FILLER            PIC  X(04)  VALUE  " ###".
*-------------------------------------------------------------*
*             ＭＡＩＮ　　　　ＭＯＤＵＬＥ                    *
*-------------------------------------------------------------*
 PROCEDURE                   DIVISION.
**
 DECLARATIVES.
 FILEERR-SEC1                SECTION.
     USE AFTER       EXCEPTION
                     PROCEDURE    ZAMJISF.
     MOVE     "ZAMJISF"      TO   ERR-FL-ID.
     MOVE     ZAI-STATUS     TO   ERR-STCD.
     DISPLAY  MSG-ABEND1     UPON   CONS.
     DISPLAY  MSG-ABEND2     UPON   CONS.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
 END     DECLARATIVES.
****************************************************************
 KEI0100-START               SECTION.
     PERFORM       INIT-SEC.
     PERFORM       MAIN-SEC
                   UNTIL     END-FLG   =    "END".
     PERFORM       END-SEC.
     STOP      RUN.
 KEI0100-END.
     EXIT.
****************************************************************
*      _０　　初期処理                                        *
****************************************************************
 INIT-SEC                    SECTION.
     OPEN    I-O             ZAMJISF.
     PERFORM    READ-SEC.
 INIT-END.
     EXIT.
****************************************************************
*      _１　　商品在庫ＲＥＡＤ処理                            *
****************************************************************
 READ-SEC                    SECTION.
     READ    ZAMJISF
         AT   END
           MOVE   "END"      TO   END-FLG
         NOT   AT   END
           ADD   1           TO   READ-CNT
     END-READ.
 READ-END.
     EXIT.
****************************************************************
*      _０　　メイン処理                                      *
****************************************************************
 MAIN-SEC                    SECTION.
     PERFORM    VARYING   IX   FROM   1   BY   1
                UNTIL     IX   >     12
         MOVE   ZAI-F05(IX)  TO   ZAI-F04(IX)
         INITIALIZE               ZAI-F05(IX)
     END-PERFORM.
     REWRITE    ZAI-REC.
     ADD   1                 TO   REWRITE-CNT.
*
     PERFORM    READ-SEC.
 MAIN-END.
     EXIT.
****************************************************************
*      3.0        終了処理                                     *
****************************************************************
 END-SEC                SECTION.
     CLOSE              ZAMJISF.
     DISPLAY  "* ZAMJISF (UPDATE)= " REWRITE-CNT " *" UPON CONS.
 END-END.
     EXIT.
******************<<  PROGRAM  END  >>**************************
