****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　富岡　_番表                      *
*    モジュール名　　　　：　富岡　_番表                      *
*    作成日／更新日　　　：　06/05/24                          *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　指定された倉庫の商品変換ＴＢＬより*
*                          _番表データを作成する。            *
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            STN0020B.
 AUTHOR.                NAV.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       PG6000.
 OBJECT-COMPUTER.       PG6000.
 SPECIAL-NAMES.
         STATION   IS   STAT
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
****<< 商品変換テーブル >>************************************
     SELECT   HSHOTBL   ASSIGN    TO        DA-01-VI-SHOTBL5
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       IS   TBL-F04
                                                 TBL-F081
                                                 TBL-F082
                                                 TBL-F083
                                                 TBL-F031
                                                 TBL-F0321
                                                 TBL-F0322
                                                 TBL-F0323
                        FILE      STATUS    IS   TBL-STATUS.
****<< _番表データ >>******************************
     SELECT   ZTANAF    ASSIGN    TO        DA-01-VI-ZTANAL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       IS   TAN-F01
                                                 TAN-F02
                                                 TAN-F03
                                                 TAN-F04
                        FILE      STATUS    IS   TAN-STATUS.
*----<< 商品名称マスタ >>-*
     SELECT   HMEIMS    ASSIGN    TO        DA-01-VI-MEIMS1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       MEI-F01
                        FILE      STATUS    MEI-STATUS.
***************************************************************
 DATA                   DIVISION.
***************************************************************
 FILE                   SECTION.
***************************************************************
****<< 商品変換テーブル >>*************************************
 FD  HSHOTBL.
     COPY     HSHOTBL   OF        XFDLIB
              JOINING   TBL       PREFIX.
****<< _番表データ >>*********************************
 FD  ZTANAF
              BLOCK CONTAINS  40  RECORDS.
     COPY     ZTANAF    OF        XFDLIB
              JOINING   TAN       PREFIX.
*----<< 商品名称マスタ >>-*
 FD  HMEIMS             BLOCK     CONTAINS   6   RECORDS
                        LABEL     RECORD     IS  STANDARD.
     COPY     HMEIMS    OF   XFDLIB    JOINING   MEI  AS   PREFIX.
****  作業領域  ************************************************
 WORKING-STORAGE        SECTION.
****************************************************************
****  ステイタス情報          ****
 01  STATUS-AREA.
     02 TBL-STATUS           PIC  X(02).
     02 TAN-STATUS           PIC  X(02).
     02 MEI-STATUS           PIC  X(02).
**** メッセージ情報           ****
 01  MSG-AREA1-1.
     02  MSG-ABEND1.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-PG-ID         PIC  X(08)  VALUE  "STN0020B".
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
 01  HSHOTBL-INV-FLG         PIC  X(03)  VALUE  SPACE.
 01  ZTANAF-INV-FLG          PIC  X(03)  VALUE  SPACE.
 01  HMEIMS-INV-FLG          PIC  X(03)  VALUE  SPACE.
****  カウント                ****
 01  CNT-AREA.
     03  IN-CNT              PIC  9(07)   VALUE  0.
     03  OUT-CNT             PIC  9(07)   VALUE  0.
     03  CHG-CNT             PIC  9(07)   VALUE  0.
*
************************************************************
 LINKAGE            SECTION.
 01  LINK-SOKCD         PIC  X(02).
 01  LINK-TANAST        PIC  X(08).
 01  LINK-TANAED        PIC  X(08).
************************************************************
 PROCEDURE              DIVISION       USING  LINK-SOKCD
                                              LINK-TANAST
                                              LINK-TANAED.
************************************************************
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  HSHOTBL.
     MOVE   "HSHOTBL "        TO    ERR-FL-ID.
     MOVE    TBL-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 FILEERR-SEC2           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  ZTANAF.
     MOVE   "ZTANAF  "        TO    ERR-FL-ID.
     MOVE    TAN-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 FILEERR-SEC3           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  HMEIMS.
     MOVE   "HMEIMS  "        TO    ERR-FL-ID.
     MOVE    MEI-STATUS       TO    ERR-STCD.
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
     OPEN         INPUT     HSHOTBL HMEIMS.
     OPEN         I-O       ZTANAF.
*商品変換ﾃｰﾌﾞﾙｽﾀｰﾄ
     MOVE     SPACE         TO   TBL-REC.
     INITIALIZE                  TBL-REC.
     MOVE     LINK-SOKCD    TO   TBL-F04.
     START  HSHOTBL  KEY  IS  >=  TBL-F04  TBL-F081  TBL-F082
                                  TBL-F083 TBL-F031  TBL-F0321
                                  TBL-F0322 TBL-F0323
            INVALID  MOVE "END"  TO  END-FLG
                     GO          TO  100-INIT-END
     END-START.
*
     PERFORM  HSHOTBL-READ-SEC.
*
 100-INIT-END.
     EXIT.
************************************************************
*      ２００   主処理　                                   *
************************************************************
 200-MAIN-SEC           SECTION.
*
     MOVE  TBL-F04        TO   TAN-F01.
     MOVE  TBL-F08        TO   TAN-F02.
     MOVE  TBL-F031       TO   TAN-F03.
     MOVE  TBL-F032       TO   TAN-F04.
     PERFORM ZTANAF-READ-SEC.
*
     IF  ZTANAF-INV-FLG  =  "INV"
         MOVE   SPACE     TO   TAN-REC
         INITIALIZE            TAN-REC
         MOVE   TBL-F04   TO   TAN-F01
         MOVE   TBL-F08   TO   TAN-F02
         MOVE   TBL-F031  TO   TAN-F03 MEI-F011
         MOVE   TBL-F032  TO   TAN-F04 MEI-F012
         PERFORM HMEIMS-READ-SEC
         IF     HMEIMS-INV-FLG = "INV"
                MOVE ALL NC"＊" TO  TAN-F05 TAN-F06
         ELSE
                MOVE MEI-F021   TO  TAN-F05
                MOVE MEI-F022   TO  TAN-F06
         END-IF
         WRITE  TAN-REC
         ADD    1               TO  OUT-CNT
     END-IF.
*
     PERFORM HSHOTBL-READ-SEC.
*
 200-MAIN-SEC-EXT.
     EXIT.
************************************************************
*            商品変換テーブルの読込処理
************************************************************
 HSHOTBL-READ-SEC                 SECTION.
     READ    HSHOTBL
             AT   END
             MOVE      "END"     TO   END-FLG
             GO        TO        HSHOTBL-READ-EXIT
     END-READ.
*
     ADD     1         TO        IN-CNT.
*
     IF   IN-CNT(5:3) = "000" OR "500"
          DISPLAY "READ-CNT = " IN-CNT UPON CONS
     END-IF.
*
     IF   TBL-F04  NOT =  LINK-SOKCD
          MOVE      "END"   TO   END-FLG
          GO          TO         HSHOTBL-READ-EXIT
     END-IF.
*
     IF   TBL-F08 NOT = SPACE
          CONTINUE
     ELSE
          GO           TO         HSHOTBL-READ-SEC
     END-IF.
*
 HSHOTBL-READ-EXIT.
     EXIT.
************************************************************
*            商品変換テーブルの読込処理
************************************************************
 ZTANAF-READ-SEC                  SECTION.
*
     READ    ZTANAF
             INVALID      MOVE "INV" TO ZTANAF-INV-FLG
             NOT  INVALID MOVE SPACE TO ZTANAF-INV-FLG
     END-READ.
*
 ZTANAF-READ-EXIT.
     EXIT.
************************************************************
*            商品名称マスタ読込処理
************************************************************
 HMEIMS-READ-SEC                  SECTION.
*
     READ    HMEIMS
             INVALID      MOVE "INV" TO HMEIMS-INV-FLG
             NOT  INVALID MOVE SPACE TO HMEIMS-INV-FLG
     END-READ.
*
 HMEIMS-READ-EXIT.
     EXIT.
************************************************************
*      ３００     終了処理                                 *
************************************************************
 300-END-SEC           SECTION.
     CLOSE             HSHOTBL  ZTANAF  HMEIMS.
*
     DISPLAY "* HSHOTBL (INPUT) = " IN-CNT   " *"  UPON CONS.
     DISPLAY "* ZTANAF  (OUTPUT)= " OUT-CNT  " *"  UPON CONS.
*
 300-END-SEC-EXT.
     EXIT.
*****************<<  PROGRAM  END  >>***********************
