****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　在庫管理システム　　　　　　　　　*
*    モジュール名　　　　：　在庫引当                          *
*    作成日／更新日　　　：　05/12/22                          *
*    作成者／更新者　　　：　ＮＡＶ松野　　　　　　　　　　　　*
*    処理概要　　　　　　：　伝票データＦを読込み，商品在庫Ｆ　*
*                            と在庫引当を行う　　　　　　　　  *
*    更新日／更新者　　　：　11/10/13  /YOSHIDA.M              *
*    更新概要　　　　　　：　基幹サーバ統合                    *
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SZA0550B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          05/12/22.
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       K-150SI.
 OBJECT-COMPUTER.       K-150SI.
 SPECIAL-NAMES.
     CONSOLE     IS     CONS
     STATION     IS     STAT.
*--------------------------------------------------------------*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*---<<  伝票データファイル　    >>---*
     SELECT   INFILE   ASSIGN     TO        DA-01-VI-DENJNL6
                       ORGANIZATION         INDEXED
                       ACCESS     MODE      SEQUENTIAL
                       RECORD     KEY       IN-F277 IN-F274
                                            IN-F09  IN-F02
***2011.10.13 ST
                                            IN-F04  IN-F051
                                            IN-F07  IN-F112
***2011.10.13 EN
                                            IN-F03
                        FILE      STATUS    IN-STATUS.
*---<<  商品在庫マスタ  >>---*
     SELECT   ZAMZAIF   ASSIGN    TO        DA-01-VI-ZAMZAIL1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   ZAI-F01
                                                 ZAI-F02
                                                 ZAI-F03
                        FILE      STATUS    IS   ZAI-STATUS.
*---<<  商品コード変換テーブル  >>---*
     SELECT   HSHOTBL   ASSIGN    TO        DA-01-VI-SHOTBL1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   SHO-F01
                                                 SHO-F02
                        FILE      STATUS    IS   SHO-STATUS.
*
*---<<  商品名称マスタ  >>---*
     SELECT   HMEIMS    ASSIGN    TO        DA-01-VI-MEIMS1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   MEI-F01
                        FILE      STATUS    IS   MEI-STATUS.
*---<<  受注残トラン　  >>---*
     SELECT   JHTZANF   ASSIGN    TO        DA-01-VI-JHTZANL1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   ZAN-F01 ZAN-F02
                                                 ZAN-F04 ZAN-F051
                                                 ZAN-F03
                        FILE      STATUS    IS   ZAN-STATUS.
*
*--------------------------------------------------------------*
 DATA                   DIVISION.
 FILE                   SECTION.
*---<<  ＩＮファイル  >>---*
 FD  INFILE
     BLOCK       CONTAINS   4        RECORDS
     LABEL       RECORD    IS        STANDARD.
     COPY        SHTDENF   OF        XFDLIB
                 JOINING   IN        PREFIX.
*---<<  商品在庫マスタ  >>---*
 FD  ZAMZAIF.
     COPY     ZAMZAIF   OF        XFDLIB
              JOINING   ZAI       PREFIX.
*---<<  商品在庫マスタ  >>---*
 FD  HSHOTBL.
     COPY     HSHOTBL   OF        XFDLIB
              JOINING   SHO       PREFIX.
*---<<  商品名称マスタ>>---*
 FD  HMEIMS.
     COPY     HMEIMS    OF        XFDLIB
              JOINING   MEI       PREFIX.
*---<<  受注残トラン　>>---*
 FD  JHTZANF.
     COPY     JHTZANF   OF        XFDLIB
              JOINING   ZAN       PREFIX.
*--------------------------------------------------------------*
 WORKING-STORAGE     SECTION.
*--------------------------------------------------------------*
 01  SYS-DATE                PIC  9(08).
 01  STATUS-AREA.
     03  IN-STATUS           PIC  X(02).
     03  OUT-STATUS          PIC  X(02).
     03  ZAI-STATUS          PIC  X(02).
     03  SHO-STATUS          PIC  X(02).
     03  MEI-STATUS          PIC  X(02).
     03  ZAN-STATUS          PIC  X(02).
 01  WK-SURYO                PIC  9(10)V99.
 01  PSW-AREA.
     03  END-FLG             PIC  X(03)  VALUE SPACE.
 01  CNT-AREA.
     03  KEP-FLG             PIC  X(01)  VALUE SPACE.
     03  MEI-INV             PIC  9(01)  VALUE ZERO.
     03  IN-CNT              PIC  9(07)  VALUE ZERO.
     03  OUT-CNT1            PIC  9(07)  VALUE ZERO.
     03  OUT-CNT2            PIC  9(07)  VALUE ZERO.
     03  OUT-CNT3            PIC  9(07)  VALUE ZERO.
 01  WRK-AREA.
     03  WRK-TANA            PIC  X(06).
     03  WRK-ZAI             PIC S9(09)V99.
     03  WRK-HIK             PIC S9(09)V99.
 01  MSG-AREA1-1.
     03  MSG-ABEND1.
       05  FILLER            PIC  X(04)  VALUE "### ".
       05  ERR-PG-ID         PIC  X(08)  VALUE "SZA0550B".
       05  FILLER            PIC  X(10)  VALUE " ABEND ###".
     03  MSG-ABEND2.
       05  FILLER            PIC  X(04)  VALUE "### ".
       05  ERR-FL-ID         PIC  X(08).
       05  FILLER            PIC  X(04)  VALUE " ST-".
       05  ERR-STCD          PIC  X(02).
       05  FILLER            PIC  X(04)  VALUE " ###".
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
*--------------------------------------------------------------*
*             ＭＡＩＮ　　　　ＭＯＤＵＬＥ                     *
*--------------------------------------------------------------*
 PROCEDURE              DIVISION.
**
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE      INFILE.
     MOVE   "INFILE  "       TO   ERR-FL-ID.
     MOVE    IN-STATUS       TO   ERR-STCD.
     DISPLAY   MSG-ABEND1    UPON   CONS.
     DISPLAY   MSG-ABEND2    UPON   CONS.
     MOVE      4000          TO   PROGRAM-STATUS.
     STOP     RUN.
**
**
 FILEERR-SEC3        SECTION.
     USE AFTER       EXCEPTION
                     PROCEDURE    ZAMZAIF.
     MOVE     "ZAMZAIF "      TO   ERR-FL-ID.
     MOVE      ZAI-STATUS    TO   ERR-STCD.
     DISPLAY   MSG-ABEND1    UPON   CONS.
     DISPLAY   MSG-ABEND2    UPON   CONS.
     MOVE      4000          TO   PROGRAM-STATUS.
     STOP      RUN.
**
 FILEERR-SEC4        SECTION.
     USE AFTER       EXCEPTION
                     PROCEDURE    HSHOTBL.
     MOVE     "HSHOTBL"      TO   ERR-FL-ID.
     MOVE      SHO-STATUS    TO   ERR-STCD.
     DISPLAY   MSG-ABEND1    UPON   CONS.
     DISPLAY   MSG-ABEND2    UPON   CONS.
     MOVE      4000          TO   PROGRAM-STATUS.
     STOP      RUN.
**
 FILEERR-SEC5        SECTION.
     USE AFTER       EXCEPTION
                     PROCEDURE    HMEIMS.
     MOVE     "HMEIMS "      TO   ERR-FL-ID.
     MOVE      MEI-STATUS    TO   ERR-STCD.
     DISPLAY   MSG-ABEND1    UPON   CONS.
     DISPLAY   MSG-ABEND2    UPON   CONS.
     MOVE      4000          TO   PROGRAM-STATUS.
     STOP      RUN.
**
 FILEERR-SEC6        SECTION.
     USE AFTER       EXCEPTION
                     PROCEDURE    JHTZANF.
     MOVE     "JHTZANF"      TO   ERR-FL-ID.
     MOVE      ZAN-STATUS    TO   ERR-STCD.
     DISPLAY   MSG-ABEND1    UPON   CONS.
     DISPLAY   MSG-ABEND2    UPON   CONS.
     MOVE      4000          TO   PROGRAM-STATUS.
     STOP      RUN.
 END     DECLARATIVES.
****************************************************************
 SZA0550B-START              SECTION.
     PERFORM       INIT-SEC.
     PERFORM       MAIN-SEC
                   UNTIL     END-FLG   =    "END".
     PERFORM       END-SEC.
     STOP      RUN.
 SZA0550B-END.
     EXIT.
****************************************************************
*      1.0 　　初期処理                                        *
****************************************************************
 INIT-SEC               SECTION.
     OPEN     INPUT     HSHOTBL  HMEIMS.
     OPEN     I-O       INFILE   ZAMZAIF  JHTZANF.
     MOVE     SPACE          TO   END-FLG.
     MOVE     SPACE          TO   KEP-FLG.
     MOVE     ZERO           TO   IN-CNT.
     MOVE     ZERO           TO   OUT-CNT1.
     MOVE     ZERO           TO   OUT-CNT2.
*システム日付・時刻の取得
     MOVE     "3"                 TO   LINK-IN-KBN.
     ACCEPT   LINK-IN-YMD6      FROM   DATE.
     MOVE     ZERO                TO   LINK-IN-YMD8.
     MOVE     ZERO                TO   LINK-OUT-RET.
     MOVE     ZERO                TO   LINK-OUT-YMD.
     CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD.
     MOVE     LINK-OUT-YMD        TO   SYS-DATE.
*
     READ    INFILE
          AT END
             MOVE  "END"       TO   END-FLG
          NOT AT END
             ADD    1          TO   IN-CNT
     END-READ.
 INIT-END.
     EXIT.
****************************************************************
*      2.0 　　メイン処理                                      *
****************************************************************
 MAIN-SEC               SECTION.
*
     IF    IN-CNT(5:3) =     "000" OR "500"
           DISPLAY  IN-CNT   UPON CONS
     END-IF.
*
*数量変換
     MOVE  IN-F15      TO    WK-SURYO.
     IF    IN-F1412    =     "     20 "
           COMPUTE           WK-SURYO  =  IN-F15 / 10
     END-IF.
** 行_が１２より大きい時読み飛ばし
     IF    IN-F03      >     12
           GO                TO   MAIN-READ
     END-IF.
*計上済データは対象外とする
     IF    IN-F277           =    9
           MOVE   "END"      TO   END-FLG
           GO                TO   MAIN-READ
     END-IF.
*
*引落済の時　処理対象外
     IF    IN-F27C     NOT =      ZERO
           GO                TO   MAIN-READ
     END-IF.
*
*DENREAD-60.
*伝票区分が４０の伝票のみ対象とする
     IF      IN-F051     =   40
             CONTINUE
     ELSE
           GO                TO   MAIN-READ
     END-IF.
*
*---<  _番取得  >---*
     MOVE     SPACE          TO   KEP-FLG.
     PERFORM   SHO-READ-SEC.
*---<  在庫引当  >---*
     IF  KEP-FLG    =  SPACE
         PERFORM   ZAIHIKI-SEC
     ELSE
         PERFORM   REWRITE-SEC
         ADD   1         TO   OUT-CNT2
     END-IF.
*****受注残トラン レコード作成
     PERFORM   JHTZANF-WRITE-SEC.
 MAIN-READ.
*---<  伝票データＦ　ＲＥＡＤ  >---*
     READ    INFILE
          AT END
             MOVE  "END"     TO   END-FLG
          NOT AT END
             ADD   1         TO   IN-CNT
     END-READ.
 MAIN-END.
     EXIT.
****************************************************************
*      2.1     _番取得                                        *
****************************************************************
 SHO-READ-SEC           SECTION.
     MOVE    IN-F01          TO   SHO-F01.
     MOVE    IN-F25          TO   SHO-F02.
     READ    HSHOTBL
       INVALID      KEY
          MOVE      "1"      TO   KEP-FLG
       NOT INVALID  KEY
          MOVE      SHO-F08  TO   WRK-TANA
     END-READ.
 SHO-READ-END.
     EXIT.
****************************************************************
*      2.2     在庫引当                                        *
****************************************************************
 ZAIHIKI-SEC            SECTION.
     MOVE    IN-F08          TO   ZAI-F01.
     MOVE    IN-F1411        TO   ZAI-F021.
     MOVE    IN-F1412        TO   ZAI-F022.
     MOVE    WRK-TANA        TO   ZAI-F03.
     READ    ZAMZAIF
       INVALID      KEY
          MOVE      "1"      TO   KEP-FLG
          MOVE      SPACE    TO   ZAI-REC
          INITIALIZE              ZAI-REC
          MOVE      IN-F08   TO   ZAI-F01
          MOVE    IN-F1411   TO   ZAI-F021
          MOVE    IN-F1412   TO   ZAI-F022
          MOVE    WRK-TANA   TO   ZAI-F03
          COMPUTE  ZAI-F27   =   ZAI-F27  +  WK-SURYO
          MOVE    SYS-DATE   TO   ZAI-F98
          MOVE     ZERO      TO   MEI-INV
          PERFORM  MEI-READ-SEC
          IF       MEI-INV   =    ZERO
                   WRITE  ZAI-REC
                   END-WRITE
          END-IF
******
          PERFORM   REWRITE-SEC
          ADD   1         TO   OUT-CNT2
       NOT INVALID  KEY
          COMPUTE   WRK-ZAI   =   ZAI-F04  -  ZAI-F28
          COMPUTE   WRK-HIK   =   WRK-ZAI  -  WK-SURYO
          IF  WRK-HIK  <  0
              MOVE      "1"      TO   KEP-FLG
              PERFORM   REWRITE-SEC
              PERFORM   UPDATE2-SEC
              ADD   1         TO   OUT-CNT2
          ELSE
              PERFORM   REWRITE-SEC
              PERFORM   UPDATE-SEC
              ADD   1         TO   OUT-CNT1
          END-IF
     END-READ.
 ZAIHIKI-END.
     EXIT.
****************************************************************
*      2.3     受注残トラン                                    *
****************************************************************
 JHTZANF-WRITE-SEC      SECTION.
     MOVE      IN-F01        TO   ZAN-F01.
     MOVE      IN-F02        TO   ZAN-F02.
     MOVE      IN-F04        TO   ZAN-F04.
     MOVE      IN-F051       TO   ZAN-F051.
     MOVE      IN-F03        TO   ZAN-F03.
     READ      JHTZANF
       INVALID      KEY
          MOVE SPACE         TO  ZAN-REC
          INITIALIZE             ZAN-REC
          MOVE    IN-F01     TO  ZAN-F01
          MOVE    IN-F02     TO  ZAN-F02
          MOVE    IN-F03     TO  ZAN-F03
          IF  IN-F02 = 30947246
              DISPLAY "IN-F03  = " IN-F03  UPON CONS
              DISPLAY "ZAN-F03 = " ZAN-F03 UPON CONS
          END-IF
          MOVE    IN-F04     TO  ZAN-F04
          MOVE    IN-F051    TO  ZAN-F051
          MOVE    IN-F052    TO  ZAN-F052
          MOVE    IN-F07     TO  ZAN-F06
          MOVE    IN-F08     TO  ZAN-F07
          MOVE    IN-F111    TO  ZAN-F081
          MOVE    IN-F112    TO  ZAN-F082
          MOVE    IN-F113    TO  ZAN-F083
          MOVE    IN-F1411   TO  ZAN-F091
          MOVE    IN-F1412   TO  ZAN-F092
          MOVE    IN-F49     TO  ZAN-F093
          MOVE    IN-F15     TO  ZAN-F10
          MOVE    IN-F16     TO  ZAN-F11
          MOVE    IN-F171    TO  ZAN-F121
          MOVE    IN-F172    TO  ZAN-F122
          MOVE    IN-F173    TO  ZAN-F123
          MOVE    IN-F181    TO  ZAN-F131
          MOVE    IN-F182    TO  ZAN-F132
          MOVE    IN-F25     TO  ZAN-F14
          MOVE    IN-F48     TO  ZAN-F94
          MOVE    IN-F46     TO  ZAN-F95
          MOVE    IN-F47     TO  ZAN-F96
          IF      IN-F27D   =   1
                  MOVE   1   TO  ZAN-F97
          ELSE
                  MOVE  ZERO TO  ZAN-F97
          END-IF
          MOVE    SYS-DATE   TO  ZAN-F98  ZAN-F99
          WRITE ZAN-REC
          ADD     1          TO  OUT-CNT3
     END-READ.
 JHTZANF-WRITE-EXIT.
     EXIT.
****************************************************************
*      2.2.1   伝票データＦ出力                                *
****************************************************************
 REWRITE-SEC            SECTION.
     IF      KEP-FLG   =  SPACE
             MOVE    1         TO     IN-F27D
     END-IF.
     REWRITE   IN-REC
     END-REWRITE.
 REWRITE-END.
     EXIT.
*----------------------------------------------------------*
*                商品名の取得                              *
*----------------------------------------------------------*
 MEI-READ-SEC           SECTION.
     MOVE    IN-F1411        TO   MEI-F01.
     MOVE    IN-F1412        TO   MEI-F012.
     READ    HMEIMS
       INVALID      KEY
          MOVE     SPACE     TO   ZAI-F30
          MOVE      1        TO   MEI-INV
       NOT INVALID  KEY
          MOVE     MEI-F031  TO   ZAI-F30
          MOVE     ZERO      TO   MEI-INV
     END-READ.
 MEI-READ-END.
     EXIT.
****************************************************************
*      2.2.2   商品在庫マスタ更新                              *
****************************************************************
 UPDATE-SEC             SECTION.
     COMPUTE  ZAI-F28   =   ZAI-F28  +  WK-SURYO.
     COMPUTE  ZAI-F27   =   ZAI-F27  +  WK-SURYO.
     REWRITE  ZAI-REC
     END-REWRITE.
 UPDATE-END.
     EXIT.
****************************************************************
*              商品在庫マスタ更新                              *
****************************************************************
 UPDATE2-SEC            SECTION.
     COMPUTE  ZAI-F27   =   ZAI-F27  +  WK-SURYO.
     REWRITE  ZAI-REC
     END-REWRITE.
 UPDATE2-EXT.
     EXIT.
****************************************************************
*      3.0        終了処理                                     *
****************************************************************
 END-SEC                SECTION.
     DISPLAY "* DENJNL     (IN)= "  IN-CNT   " *" UPON CONS.
     DISPLAY "* ZAMZAIF  (UPD)1= "  OUT-CNT1 " *" UPON CONS.
     DISPLAY "* ZAMZAIF  (UPD)2= "  OUT-CNT2 " *" UPON CONS.
     DISPLAY "* JHTZANF  (CRT) = "  OUT-CNT3 " *" UPON CONS.
     CLOSE    INFILE    HMEIMS
              ZAMZAIF   HSHOTBL   JHTZANF.
 END-END.
     EXIT.
*****************<<  PROGRAM  END  >>***********************
