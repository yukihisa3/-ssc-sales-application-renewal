****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　在庫管理システム　　　　　　　　　*
*    モジュール名　　　　：　実_更新　　　　                  *
*    作成日／更新日　　　：　93/04/26                          *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　_卸Ｆを読込み，商品在庫マスタを　*
*                            編集出力する                      *
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            ZTA0060B.
*AUTHER.                NAV.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FACOM-K6500.
 OBJECT-COMPUTER.       FACOM-K6500.
 SPECIAL-NAMES.
         STATION   IS   STAT
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
****<< _卸ファイル >>**************************************
     SELECT   ZTANADT   ASSIGN  TO   DA-01-S-ZTANADT
                        ACCESS  MODE         IS   SEQUENTIAL
                        FILE    STATUS       IS   TANA-STATUS.
****<< 商品在庫マスタ >>*************************************
     SELECT   ZZAIMS    ASSIGN  TO   DA-01-VI-ZZAIMS1
                        ORGANIZATION        IS   INDEXED
                        ACCESS  MODE        IS   RANDOM
                        RECORD  KEY         IS   ZAI-F01
                                                 ZAI-F02
                                                 ZAI-F03
                        FILE    STATUS      IS   ZAI-STATUS.
****<< 商品名称マスタ >>*************************************
     SELECT   MEIMS1    ASSIGN    TO        DA-01-VI-MEIMS1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   MEI-F011
                                                 MEI-F0121
                                                 MEI-F0122
                                                 MEI-F0123
                        FILE      STATUS    IS   MEI-STATUS.
***
 DATA                   DIVISION.
 FILE                   SECTION.
*
****<< _卸ファイル >>**************************************
 FD    ZTANADT
       LABEL  RECORD    IS      STANDARD.
       COPY   ZTANADT   OF      XFDLIB
              JOINING   TANA    PREFIX.
****<<  商品在庫マスタ >>***********************************
 FD    ZZAIMS
       LABEL  RECORD    IS      STANDARD.
       COPY   ZZAIMS    OF      XFDLIB
              JOINING   ZAI     PREFIX.
****<<  商品名称マスタ >>***********************************
 FD    MEIMS1.
       COPY   HMEIMS    OF      XFDLIB
              JOINING   MEI     AS    PREFIX.
****  作業領域  ********************************************
 WORKING-STORAGE        SECTION.
****  カウント  エリア        ****
 01  CNT-AREA                VALUE    ZERO.
     02 TANA-CNT             PIC  9(07).
     02 SKIP-CNT             PIC  9(07).
     02 ZAIMS-CNT            PIC  9(07).
     02 INS-CNT              PIC  9(07).
     02 MEIMS-CNT            PIC  9(07).
****  ステイタス情報          ****
 01  STATUS-AREA.
     02 TANA-STATUS          PIC  X(2).
     02 ZAI-STATUS           PIC  X(2).
     02 MEI-STATUS           PIC  X(2).
****  フラグ                  ****
 01  END-FLG                 PIC  X(03)  VALUE  SPACE.
**** メッセージ情報           ****
 01  MSG-AREA1-1.
     02  MSG-ABEND1.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-PG-ID         PIC  X(08)  VALUE  "ZTA0060B".
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
                   PROCEDURE  ZTANADT.
     MOVE   "ZTANADT "        TO    ERR-FL-ID.
     MOVE    TANA-STATUS      TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 FILEERR-SEC2           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  ZZAIMS.
     MOVE   "ZZAIMS  "        TO    ERR-FL-ID.
     MOVE    ZAI-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 FILEERR-SEC3           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  MEIMS1.
     MOVE   "MEIMS1  "        TO    ERR-FL-ID.
     MOVE    MEI-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
 END     DECLARATIVES.
************************************************************
 ZTA0060B-START         SECTION.
     PERFORM       INIT-SEC.
     PERFORM       MAIN-SEC
                   UNTIL     END-FLG  =    "END".
     PERFORM       END-SEC.
     STOP     RUN.
 ZTA0060B-END.
     EXIT.
************************************************************
*      _０     初期処理                                   *
************************************************************
 INIT-SEC               SECTION.
     OPEN     INPUT     ZTANADT.
     OPEN     INPUT     MEIMS1.
     OPEN     I-O       ZZAIMS.
     INITIALIZE         CNT-AREA.
     PERFORM  TANA-READ-SEC.
 INIT-END.
     EXIT.
************************************************************
*      _１     _卸ファイルＲＥＡＤ処理                   *
************************************************************
 TANA-READ-SEC          SECTION.
     READ     ZTANADT
       AT  END
         MOVE     "END"      TO     END-FLG
         GO        TO        TANA-READ-END
     END-READ.
     ADD      1              TO     TANA-CNT.
     IF  TANA-F99  =   "9"
       ADD    1              TO     SKIP-CNT
       GO          TO        TANA-READ-SEC
     END-IF.
 TANA-READ-END.
     EXIT.
************************************************************
*      _０      メイン処理                                *
************************************************************
 MAIN-SEC               SECTION.
     MOVE     TANA-F04       TO     ZAI-F01.
     MOVE     TANA-F05       TO     ZAI-F021.
     MOVE     TANA-F06X      TO     ZAI-F022.
     MOVE     TANA-F15       TO     ZAI-F03.
     READ     ZZAIMS
       INVALID       KEY
              PERFORM  ZAIMS-INS-SEC
       NOT  INVALID  KEY
              COMPUTE  ZAI-F06   =  TANA-F11  +  ZAI-F10
                                              -  ZAI-F11
              COMPUTE  ZAI-F07   =  TANA-F11
              REWRITE  ZAI-REC
              ADD      1            TO        ZAIMS-CNT
     END-READ.
     PERFORM  TANA-READ-SEC.
 MAIN-END.
     EXIT.
************************************************************
*      _１      商品在庫マスタ追加処理                    *
************************************************************
 ZAIMS-INS-SEC          SECTION.
     MOVE     SPACE          TO     ZAI-REC.
     INITIALIZE                     ZAI-REC.
     MOVE     TANA-F04       TO     ZAI-F01.
     MOVE     TANA-F05       TO     ZAI-F021.
     MOVE     TANA-F06X      TO     ZAI-F022.
     MOVE     TANA-F15       TO     ZAI-F03.
     MOVE     TANA-F11       TO     ZAI-F06.
     MOVE     TANA-F11       TO     ZAI-F07.
*商品名称マスタよりカナ名セット
     PERFORM  MEIMS-RD-SEC.
*
     WRITE    ZAI-REC.
     ADD      1              TO     INS-CNT.
 ZAIMS-INS-SEC-EXIT.
     EXIT.
************************************************************
*      2.1.1     商品名称マスタＲＥＡＤ                    *
************************************************************
 MEIMS-RD-SEC           SECTION.
     MOVE     TANA-F05       TO     MEI-F011.
     MOVE     TANA-F06X      TO     MEI-F012.
     READ     MEIMS1
       INVALID       KEY
              MOVE  SPACE    TO     ZAI-F04
              ADD      1     TO     MEIMS-CNT
       NOT  INVALID  KEY
              MOVE  MEI-F031 TO     ZAI-F04
     END-READ.
*
 MEIMS-RD-SEC-EXIT.
     EXIT.
************************************************************
*      3.0        終了処理                                 *
************************************************************
 END-SEC                SECTION.
     CLOSE    ZTANADT   ZZAIMS   MEIMS1.
     DISPLAY "(ﾀﾅｵﾛｼ F)    IN ｹﾝｽｳ = " TANA-CNT  UPON  CONS.
     DISPLAY "(ﾀﾅｵﾛｼ F)  SKIP ｹﾝｽｳ = " SKIP-CNT  UPON  CONS.
     DISPLAY "(ｻﾞｲｺ  M) REWRT ｹﾝｽｳ = " ZAIMS-CNT UPON  CONS.
     DISPLAY "(ｻﾞｲｺ  M)   ADD ｹﾝｽｳ = " INS-CNT   UPON  CONS.
     DISPLAY "(ﾒｲｼｮｳ M)   INV ｹﾝｽｳ = " MEIMS-CNT UPON  CONS.
 END-END.
     EXIT.
*****************<<  PROGRAM  END  >>***********************
