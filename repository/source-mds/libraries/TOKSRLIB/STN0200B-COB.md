# STN0200B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/STN0200B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　Ｄ３６５連携　_卸業務　　　　　　*
*    モジュール名　　　　：　_卸ファイル数量計算              *
*    作成日／更新日　　　：　2021/12/10                        *
*    作成者／更新者　　　：　ＮＡＶ高橋　　　　　　　　　　　　*
*    処理概要　　　　　　：　_卸ファイルの倉庫・商品・品単    *
*                            _番の同一のものの数量を集計する  *
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            STN0200B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          00/05/29.
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       K6500.
 OBJECT-COMPUTER.       K6500.
 SPECIAL-NAMES.
     CONSOLE     IS     CONS
     STATION     IS     STAT.
*--------------------------------------------------------------*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*---<<  _卸ファイル  >>---*
     SELECT   KKTANAL2  ASSIGN    TO        DA-01-VI-KKTANAL2
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       IS   TAN-F04
                                                 TAN-F05
                                                 TAN-F06
                                                 TAN-F07
                                                 TAN-F08
                                                 TAN-F15
                        FILE      STATUS    IS   TAN-ST.
*----<< ＯＵＴファイル >>-*
     SELECT   OUTFILE   ASSIGN    TO        DA-01-S-ZTANAWK
                        ACCESS    MODE      SEQUENTIAL
                        FILE      STATUS    IS   OUT-ST.
*
*--------------------------------------------------------------*
 DATA                   DIVISION.
 FILE                   SECTION.
*---<<  _卸ファイル  >>---*
 FD  KKTANAL2.
     COPY     KKTANAF     OF        XFDLIB
              JOINING   TAN       PREFIX.
*----<< ＯＵＴファイル >>-*
 FD  OUTFILE.
     COPY     ZTANADT     OF        XFDLIB
              JOINING   OUT       PREFIX.
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  STATUS-AREA.
     03  TAN-ST              PIC  X(02).
     03  OUT-ST              PIC  X(02).
 01  PSW-AREA.
     03  END-FLG             PIC  X(03)  VALUE SPACE.
 01  CNT-AREA.
     03  IN-CNT              PIC  9(07)  VALUE ZERO.
     03  OUT-CNT             PIC  9(07)  VALUE ZERO.
 01  WK-AREA.
     03  WK-GOKEI            PIC S9(07)V99  VALUE ZERO.
 01  NEW-KEY.
     03  NEW-F04             PIC  X(02)  VALUE SPACE.
     03  NEW-F05             PIC  X(08)  VALUE SPACE.
     03  NEW-F06             PIC  X(05)  VALUE SPACE.
     03  NEW-F07             PIC  X(02)  VALUE SPACE.
     03  NEW-F08             PIC  X(01)  VALUE SPACE.
     03  NEW-F15             PIC  X(06)  VALUE SPACE.
 01  OLD-KEY.
     03  OLD-F04             PIC  X(02)  VALUE SPACE.
     03  OLD-F05             PIC  X(08)  VALUE SPACE.
     03  OLD-F06             PIC  X(05)  VALUE SPACE.
     03  OLD-F07             PIC  X(02)  VALUE SPACE.
     03  OLD-F08             PIC  X(01)  VALUE SPACE.
     03  OLD-F15             PIC  X(06)  VALUE SPACE.
 01  OLD-REC.
     03  OLD-DATA            PIC  X(80)  VALUE SPACE.
 01  MSG-AREA1-1.
     03  MSG-ABEND1.
       05  FILLER            PIC  X(04)  VALUE "### ".
       05  ERR-PG-ID         PIC  X(08)  VALUE "STN0200B".
       05  FILLER            PIC  X(10)  VALUE " ABEND ###".
     03  MSG-ABEND2.
       05  FILLER            PIC  X(04)  VALUE "### ".
       05  ERR-FL-ID         PIC  X(08).
       05  FILLER            PIC  X(04)  VALUE " ST-".
       05  ERR-STCD          PIC  X(02).
       05  FILLER            PIC  X(04)  VALUE " ###".
*--------------------------------------------------------------*
*             ＭＡＩＮ　　　　ＭＯＤＵＬＥ                     *
*--------------------------------------------------------------*
 PROCEDURE               DIVISION.
**
 DECLARATIVES.
 FILEERR-SEC1        SECTION.
     USE AFTER       EXCEPTION
                     PROCEDURE    KKTANAL2.
     MOVE     "KKTANAL2"     TO   ERR-FL-ID.
     MOVE      TAN-ST        TO   ERR-STCD.
     DISPLAY   MSG-ABEND1    UPON   CONS.
     DISPLAY   MSG-ABEND2    UPON   CONS.
     MOVE      4000          TO   PROGRAM-STATUS.
     STOP      RUN.
**
 FILEERR-SEC2        SECTION.
     USE AFTER       EXCEPTION
                     PROCEDURE    OUTFILE.
     MOVE     "OUTFILE "     TO   ERR-FL-ID.
     MOVE      OUT-ST        TO   ERR-STCD.
     DISPLAY   MSG-ABEND1    UPON   CONS.
     DISPLAY   MSG-ABEND2    UPON   CONS.
     MOVE      4000          TO   PROGRAM-STATUS.
     STOP      RUN.
 END     DECLARATIVES.
****************************************************************
 STN0200B-START              SECTION.
     PERFORM       INIT-SEC.
     PERFORM       MAIN-SEC
                   UNTIL     END-FLG   =    "END".
     PERFORM       END-SEC.
     STOP      RUN.
 STN0200B-END.
     EXIT.
****************************************************************
*      1.0 　　初期処理                                        *
****************************************************************
 INIT-SEC               SECTION.
     OPEN     INPUT     KKTANAL2.
     OPEN     OUTPUT    OUTFILE.
     MOVE     SPACE          TO   END-FLG.
     MOVE     ZERO           TO   IN-CNT.
     MOVE     ZERO           TO   OUT-CNT.
     MOVE     ZERO           TO   WK-GOKEI.
     READ     KKTANAL2
          AT END
             MOVE  "END"       TO   END-FLG
          NOT AT END
             MOVE   TAN-F04       TO   NEW-F04   OLD-F04
             MOVE   TAN-F05       TO   NEW-F05   OLD-F05
             MOVE   TAN-F06       TO   NEW-F06   OLD-F06
             MOVE   TAN-F07       TO   NEW-F07   OLD-F07
             MOVE   TAN-F08       TO   NEW-F08   OLD-F08
             MOVE   TAN-F15       TO   NEW-F15   OLD-F15
             MOVE   TAN-REC(1:80) TO   OLD-REC
             ADD    1          TO   IN-CNT
     END-READ.
 INIT-END.
     EXIT.
****************************************************************
*      2.0 　　メイン処理                                      *
****************************************************************
 MAIN-SEC               SECTION.
* 倉庫・商品・品単・_番が同一の時、数量を加算
     IF      NEW-KEY   =     OLD-KEY
             COMPUTE   WK-GOKEI  =    WK-GOKEI   +    TAN-F11
             IF    IN-CNT   NOT =    1
                   DISPLAY  "ﾄﾞｳｲﾂｼｮｳﾋﾝ= "
                                  TAN-F04   "-"  TAN-F05
                            "-"   TAN-F06X  "-"  TAN-F15
**********************************************   UPON  CONS
             END-IF
     ELSE
             MOVE   OLD-REC       TO   OUT-REC
             MOVE   WK-GOKEI      TO   OUT-F11
             WRITE  OUT-REC
*
             MOVE   TAN-F11       TO   WK-GOKEI
             MOVE   NEW-KEY       TO   OLD-KEY
             MOVE   TAN-REC(1:80) TO   OLD-REC
*
             ADD    1             TO   OUT-CNT
     END-IF.
*---<  _卸ファイル　ＲＥＡＤ  >---*
     READ    KKTANAL2
          AT END
             MOVE  "END"          TO   END-FLG
*
             MOVE   OLD-REC       TO   OUT-REC
             MOVE   WK-GOKEI      TO   OUT-F11
             WRITE  OUT-REC
             ADD    1             TO   OUT-CNT
          NOT AT END
             MOVE   TAN-F04       TO   NEW-F04
             MOVE   TAN-F05       TO   NEW-F05
             MOVE   TAN-F06       TO   NEW-F06
             MOVE   TAN-F07       TO   NEW-F07
             MOVE   TAN-F08       TO   NEW-F08
             MOVE   TAN-F15       TO   NEW-F15
             ADD   1              TO   IN-CNT
     END-READ.
*
     IF  IN-CNT(5:3)  =  "000"  OR  "500"
         DISPLAY "IN-CNT = " IN-CNT UPON CONS
     END-IF.
***
 MAIN-END.
     EXIT.
****************************************************************
*      3.0        終了処理                                     *
****************************************************************
 END-SEC                SECTION.
     DISPLAY "* ZTANADT    (IN)= "  IN-CNT   " *" UPON CONS.
     DISPLAY "* OUTFILE   (OUT)= "  OUT-CNT  " *" UPON CONS.
     CLOSE    KKTANAL2  OUTFILE.
 END-END.
     EXIT.
*****************<<  PROGRAM  END  >>***********************

```
