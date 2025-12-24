****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　在庫管理システム　　　　　　　　　*
*    モジュール名　　　　：　入出庫ファイル消込　              *
*    作成日／更新日　　　：　00/05/15                          *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　入出庫Ｆを読み，作業日と削除フラグ*
*                            の条件を判定し，該当データを削除す*
*                            る．　　　　　　　　　　　　　　　*
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SZA0160B.
*AUTHER.                NAV.
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
****<< 入出庫ファイル >>************************************
     SELECT   NYSFILF   ASSIGN  TO   DA-01-VI-NYSFILL1
                        ORGANIZATION         IS   INDEXED
                        ACCESS  MODE         IS   SEQUENTIAL
                        RECORD  KEY          IS   NYUS-F02
                                                  NYUS-F03
                        FILE    STATUS       IS   NYUS-STATUS.
****<< 条件ファイル >>**************************************
     SELECT   HJYOKEN   ASSIGN  TO   DA-01-VI-JYOKEN1
                        ORGANIZATION         IS   INDEXED
                        ACCESS  MODE         IS   RANDOM
                        RECORD  KEY          IS   JYO-F01
                                                  JYO-F02
                        FILE    STATUS       IS   JYO-STATUS.
***
 DATA                   DIVISION.
 FILE                   SECTION.
*
****<< 入出庫ファイル   >>**********************************
 FD    NYSFILF
       LABEL  RECORD    IS      STANDARD.
       COPY   NYSFILF   OF      XFDLIB
              JOINING   NYUS    PREFIX.
****<< 条件ファイル >>**************************************
 FD    HJYOKEN
       LABEL  RECORD    IS      STANDARD.
       COPY   HJYOKEN   OF      XFDLIB
              JOINING   JYO     PREFIX.
****  作業領域  ********************************************
 WORKING-STORAGE        SECTION.
****  カウント  エリア        ****
 01  CNT-AREA                VALUE    ZERO.
     02 NYUS-CNT             PIC  9(07).
     02 SELECT-CNT           PIC  9(07).
     02 DELETE-CNT           PIC  9(07).
     02 SKIP-CNT             PIC  9(07).
****  ステイタス情報          ****
 01  STATUS-AREA.
     02 NYUS-STATUS           PIC  X(2).
     02 JYO-STATUS            PIC  X(2).
****  フラグ                  ****
 01  END-FLG                 PIC  X(03)  VALUE  SPACE.
****  ワーク                  ****
 01  WK-SHIMEBI              PIC  9(06)V9(02).
**** メッセージ情報           ****
 01  MSG-AREA1-1.
     02  MSG-ABEND1.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-PG-ID         PIC  X(08)  VALUE  "SZA0160B".
       03  FILLER            PIC  X(10)  VALUE
          " ABEND ###".
     02  MSG-ABEND2.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-FL-ID         PIC  X(08).
       03  FILLER            PIC  X(04)  VALUE  " ST-".
       03  ERR-STCD          PIC  X(02).
       03  FILLER            PIC  X(04)  VALUE  " ###".
************************************************************
*             ＭＡＩＮ         ＭＯＤＵＬＥ                *
************************************************************
*
 PROCEDURE              DIVISION.
*
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  NYSFILF.
     MOVE   "NYSFILF "        TO    ERR-FL-ID.
     MOVE    NYUS-STATUS      TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 FILEERR-SEC2           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  HJYOKEN.
     MOVE   "HJYOKEN "        TO    ERR-FL-ID.
     MOVE    JYO-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
 END     DECLARATIVES.
************************************************************
 SZA0160B-START         SECTION.
     PERFORM       INIT-SEC.
     PERFORM       MAIN-SEC
                   UNTIL     END-FLG  =    "END".
     PERFORM       END-SEC.
     STOP     RUN.
 SZA0160B-END.
     EXIT.
************************************************************
*      _０     初期処理                                   *
************************************************************
 INIT-SEC               SECTION.
     OPEN     I-O       NYSFILF.
     OPEN     INPUT     HJYOKEN.
     INITIALIZE         CNT-AREA    WK-SHIMEBI.
     MOVE     99              TO    JYO-F01.
     MOVE     "ZAI"           TO    JYO-F02.
     READ     HJYOKEN
         INVALID
              DISPLAY  "(SZA0160B) ｴﾗｰﾒｯｾｰｼﾞ "     UPON CONS
              DISPLAY  " HJYOKEN READ INVALID-KEY" UPON CONS
              DISPLAY  "   JYO-F01 = " JYO-F01     UPON CONS
              DISPLAY  "   JYO-F02 = " JYO-F02     UPON CONS
              MOVE     "END"        TO     END-FLG
              GO                    TO     INIT-END
         NOT  INVALID
              MOVE      JYO-F05     TO     WK-SHIMEBI
**************ADD       1           TO     WK-SHIMEBI
     END-READ.
     PERFORM  NYUS-READ-SEC.
 INIT-END.
     EXIT.
************************************************************
*      _１      入出庫ファイル ＲＥＡＤ処理               *
************************************************************
 NYUS-READ-SEC          SECTION.
     READ     NYSFILF
       AT  END
         MOVE     "END"      TO     END-FLG
         GO        TO        NYUS-READ-END
     END-READ.
     ADD      1              TO     NYUS-CNT.
*削除フラグ・作業日
     IF        (NYUS-F96  =   "1"                  ) OR
               (NYUS-F15(1:6)  <=   WK-SHIMEBI(1:6))
         ADD       1         TO     SELECT-CNT
       ELSE
         ADD       1         TO     SKIP-CNT
         GO                  TO     NYUS-READ-SEC
     END-IF.
 NYUS-READ-END.
     EXIT.
************************************************************
*      _０      メイン処理                                *
************************************************************
 MAIN-SEC               SECTION.
     DELETE   NYSFILF.
     ADD      1              TO     DELETE-CNT.
     PERFORM  NYUS-READ-SEC.
 MAIN-END.
     EXIT.
************************************************************
*      3.0        終了処理                                 *
************************************************************
 END-SEC                SECTION.
     CLOSE    NYSFILF   HJYOKEN.
     DISPLAY "* NYSFILF (INPUT) = " NYUS-CNT   " *" UPON CONS.
     DISPLAY "* NYSFILF (SKIP)  = " SKIP-CNT   " *" UPON CONS.
*****DISPLAY "* NYSFILF (SELECT)= " SELECT-CNT " *" UPON CONS.
     DISPLAY "* NYSFILF (DELETE)= " DELETE-CNT " *" UPON CONS.
 END-END.
     EXIT.
*****************<<  PROGRAM  END  >>***********************
