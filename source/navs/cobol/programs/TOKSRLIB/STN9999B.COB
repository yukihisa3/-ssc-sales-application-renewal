****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　ＳＵＢ商品名称マスタ作成　　　　　*
*    モジュール名　　　　：　ＳＵＢ商品名称マスタ作成　　　　　*
*    作成日／更新日　　　：　2021/03/12                        *
*    作成者／更新者　　　：　NAV T.TAKAHASHI                   *
*    処理概要　　　　　　：　商品変換ＴＢＬから商品名称Ｍを参照*
*                          し、ＳＵＢ商品名称マスタを作成する。*
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            STN9999B.
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
****<< 商品変換テーブル >>************************************
     SELECT   HSHOTBL   ASSIGN    TO        DA-01-VI-SHOTBL1
                        ORGANIZATION        IS   INDEXED
                        RECORD    KEY       IS   TBL-F01
                                                 TBL-F02
                        FILE      STATUS    IS   TBL-STATUS.
****<< 商品名称マスタ >>************************************
     SELECT   HMEIMS   ASSIGN    TO        DA-01-VI-MEIMS1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   MEI-F011
                                                 MEI-F0121
                                                 MEI-F0122
                                                 MEI-F0123
                        FILE      STATUS    IS   MEI-STATUS.
****<< ＳＵＢ商品名称（Ｌ１） >>******************************
     SELECT   SUBMEIL1 ASSIGN    TO        DA-01-VI-SUBMEIL1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   ME1-F011
                                                 ME1-F0121
                                                 ME1-F0122
                                                 ME1-F0123
                        FILE      STATUS    IS   ME1-STATUS.
****<< ＳＵＢ商品名称（Ｌ７） >>******************************
     SELECT   SUBMEIL7 ASSIGN    TO        DA-01-VI-SUBMEIL7
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   ME2-D01
                        FILE      STATUS    IS   ME2-STATUS.
***************************************************************
 DATA                   DIVISION.
***************************************************************
 FILE                   SECTION.
***************************************************************
****<< 商品変換テーブル >>*************************************
 FD  HSHOTBL.
     COPY     HSHOTBL   OF        XFDLIB
              JOINING   TBL       PREFIX.
****<< 商品名称マスタ >>*************************************
 FD  HMEIMS.
     COPY     HMEIMS   OF        XFDLIB
              JOINING   MEI       PREFIX.
****<< ＳＵＢ商品名称（Ｌ１） >>******************************
 FD  SUBMEIL1.
     COPY     SUBMEIF  OF        XFDLIB
              JOINING   ME1       PREFIX.
****<< ＳＵＢ商品名称（Ｌ７） >>******************************
 FD  SUBMEIL7.
     COPY     SUBMEIF  OF        XFDLIB
              JOINING   ME2       PREFIX.
****  作業領域  ************************************************
 WORKING-STORAGE        SECTION.
****************************************************************
****  ステイタス情報          ****
 01  STATUS-AREA.
     02 TBL-STATUS           PIC  X(02).
     02 MEI-STATUS           PIC  X(02).
     02 ME1-STATUS           PIC  X(02).
     02 ME2-STATUS           PIC  X(02).
**** メッセージ情報           ****
 01  MSG-AREA1-1.
     02  MSG-ABEND1.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-PG-ID         PIC  X(08)  VALUE  "STN9999B".
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
 01  HSHOTBL-END             PIC  X(03)  VALUE  SPACE.
 01  SUBMEIL1-INV-FLG        PIC  X(03)  VALUE  SPACE.
 01  SUBMEIL7-INV-FLG        PIC  X(03)  VALUE  SPACE.
 01  HMEIMS-INV-FLG          PIC  X(03)  VALUE  SPACE.
****  カウント                ****
 01  CNT-AREA.
     03  IN-CNT              PIC  9(07)   VALUE  0.
     03  OK-CNT              PIC  9(07)   VALUE  0.
     03  NG-CNT              PIC  9(07)   VALUE  0.
 LINKAGE                SECTION.
 01  PARA-TOKCD              PIC  9(08).
*
************************************************************
 PROCEDURE              DIVISION USING  PARA-TOKCD.
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
                   PROCEDURE  HMEIMS.
     MOVE   "HMEIMS  "        TO    ERR-FL-ID.
     MOVE    MEI-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 FILEERR-SEC3           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  SUBMEIL1.
     MOVE   "SUBMEIL1"         TO    ERR-FL-ID.
     MOVE    ME1-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 FILEERR-SEC4           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  SUBMEIL7.
     MOVE   "SUBMEIL7"         TO    ERR-FL-ID.
     MOVE    ME2-STATUS       TO    ERR-STCD.
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
     OPEN         INPUT     HSHOTBL.
     OPEN         INPUT     HMEIMS.
     OPEN         INPUT     SUBMEIL1.
     OPEN         I-O       SUBMEIL7.
*
     MOVE    PARA-TOKCD     TO    TBL-F01.
     MOVE    SPACE          TO    TBL-F02.
     START   HSHOTBL  KEY  IS  >=  TBL-F01 TBL-F02
             INVALID
             MOVE   "END"   TO    END-FLG
             GO             TO    100-INIT-END
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
*商品名称マスタ検索
     MOVE    TBL-F031             TO   MEI-F011.
     MOVE    TBL-F0321            TO   MEI-F0121.
     MOVE    TBL-F0322            TO   MEI-F0122.
     MOVE    TBL-F0323            TO   MEI-F0123.
     READ    HMEIMS
             INVALID
             MOVE     SPACE       TO   MEI-REC
             INITIALIZE                MEI-REC
             GO                   TO   MAIN-010
     END-READ.
     IF   TBL-F02  =  "4906015044274"
          DISPLAY "DDDDD"   UPON CONS
     END-IF.
*ＳＵＢ商品名称マスタ索引（ＫＥＹ１）
     MOVE    TBL-F031             TO   ME1-F011.
     MOVE    TBL-F0321            TO   ME1-F0121.
     MOVE    TBL-F0322            TO   ME1-F0122.
     MOVE    TBL-F0323            TO   ME1-F0123.
     READ    SUBMEIL1
             INVALID
             MOVE     "INV"       TO   SUBMEIL1-INV-FLG
             NOT  INVALID
             MOVE     SPACE       TO   SUBMEIL1-INV-FLG
     END-READ.
*ＳＵＢ商品名称マスタ索引（ＫＥＹ７）
     MOVE    TBL-F02              TO   ME2-D01.
     READ    SUBMEIL7
             INVALID
             MOVE     "INV"       TO   SUBMEIL7-INV-FLG
             NOT  INVALID
             MOVE     SPACE       TO   SUBMEIL7-INV-FLG
     END-READ.
*レコード作成判定
*    DISPLAY "SUBMEIL1-INV-FLG = " SUBMEIL1-INV-FLG UPON CONS.
*    DISPLAY "SUBMEIL7-INV-FLG = " SUBMEIL7-INV-FLG UPON CONS.
*    DISPLAY "TBL-F031         = " TBL-F031         UPON CONS.
*    DISPLAY "TBL-F0321        = " TBL-F0321        UPON CONS.
*    DISPLAY "TBL-F0322        = " TBL-F0322        UPON CONS.
*    DISPLAY "TBL-F0323        = " TBL-F0323        UPON CONS.
*    DISPLAY "TBL-F02          = " TBL-F02          UPON CONS.
*
     IF   TBL-F02  =  "4906015044274"
       DISPLAY "SUBMEIL1-INV-FLG = " SUBMEIL1-INV-FLG UPON CONS
       DISPLAY "SUBMEIL7-INV-FLG = " SUBMEIL7-INV-FLG UPON CONS
     END-IF.
     IF      SUBMEIL1-INV-FLG  =  "INV"
     AND     SUBMEIL7-INV-FLG  =  "INV"
             CONTINUE
     ELSE
             ADD       1          TO   NG-CNT
             GO                   TO   MAIN-010
     END-IF.
*初期化
     MOVE    SPACE                TO   ME2-REC.
     INITIALIZE                        ME2-REC.
*項目セット
     MOVE    MEI-F011             TO   ME2-F011.
     MOVE    MEI-F012             TO   ME2-F012.
     MOVE    MEI-F02              TO   ME2-F02.
     MOVE    MEI-F03              TO   ME2-F03.
     MOVE    MEI-F04              TO   ME2-F04.
     MOVE    MEI-F05              TO   ME2-F05.
     MOVE    MEI-F06              TO   ME2-F06.
     MOVE    MEI-F07              TO   ME2-F07.
     MOVE    MEI-F08              TO   ME2-F08.
     MOVE    MEI-F09              TO   ME2-F09.
     MOVE    MEI-F10              TO   ME2-F10.
     MOVE    MEI-FIL1             TO   ME2-FIL1.
     MOVE    MEI-F89              TO   ME2-F89.
     MOVE    MEI-F90              TO   ME2-F90.
     MOVE    MEI-F91              TO   ME2-F91.
     MOVE    MEI-F92              TO   ME2-F92.
     MOVE    MEI-F93              TO   ME2-F93.
     MOVE    MEI-F94              TO   ME2-F94.
     MOVE    MEI-F95              TO   ME2-F95.
     MOVE    MEI-F96              TO   ME2-F96.
     MOVE    MEI-F97              TO   ME2-F97.
     MOVE    MEI-F98              TO   ME2-F98.
     MOVE    MEI-F99              TO   ME2-F99.
     MOVE    TBL-F02              TO   ME2-D01.
*****DISPLAY "TBL-F02 = " TBL-F02 UPON CONS.
     MOVE    MEI-F011             TO   ME2-D02(1:8).
     MOVE    "-"                  TO   ME2-D02(9:1).
     MOVE    MEI-F0121            TO   ME2-D02(10:5).
     MOVE    "-"                  TO   ME2-D02(15:1).
     MOVE    MEI-F0122            TO   ME2-D02(16:2).
     MOVE    "-"                  TO   ME2-D02(18:1).
     MOVE    MEI-F0123            TO   ME2-D02(19:1).
     MOVE    "D04"                TO   ME2-D03.
     WRITE   ME2-REC.
     ADD     1                    TO   OK-CNT.
*
 MAIN-010.
     PERFORM  HSHOTBL-READ-SEC.
*
 200-MAIN-SEC-EXT.
     EXIT.
************************************************************
*            商品変換テーブルの読込処理
************************************************************
 HSHOTBL-READ-SEC                   SECTION.
     READ    HSHOTBL
        AT   END
             MOVE      "END"     TO   END-FLG
             GO                  TO   HSHOTBL-READ-EXT
     END-READ.
*
     IF   TBL-F02  =  "4906015044274"
          DISPLAY "AAAAA"   UPON CONS
     END-IF.
     IF      PARA-TOKCD  <   TBL-F01
             MOVE      "END"     TO   END-FLG
             GO                  TO   HSHOTBL-READ-EXT
     END-IF.
*
     ADD     1         TO        IN-CNT.
*
     IF      IN-CNT(5:3)  =  "000" OR "500"
             DISPLAY "ｼｮﾘｹﾝｽｳ = " IN-CNT  UPON CONS
     END-IF.
*
     IF   TBL-F02  =  "4906015044274"
          DISPLAY "BBBBB"   UPON CONS
     END-IF.
     IF      TBL-F02(13:1)  =  SPACE
             GO                TO   HSHOTBL-READ-SEC
     END-IF.
     IF   TBL-F02  =  "4906015044274"
          DISPLAY "CCCCC"   UPON CONS
     END-IF.
*
 HSHOTBL-READ-EXT.
     EXIT.
************************************************************
*      ３００     終了処理                                 *
************************************************************
 300-END-SEC           SECTION.
     CLOSE             HSHOTBL   HMEIMS  SUBMEIL1  SUBMEIL7.
*
     DISPLAY "* HSHOTBL (INPUT) = " IN-CNT   " *"  UPON CONS.
     DISPLAY "* OK-CNT          = " OK-CNT   " *"  UPON CONS.
     DISPLAY "* NG-CNT          = " NG-CNT   " *"  UPON CONS.
*
 300-END-SEC-EXT.
     EXIT.
*****************<<  PROGRAM  END  >>***********************
