# SSE0032B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIB/SSE0032B.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　在庫管理システム　　　　　　　　　*
*    モジュール名　　　　：　在庫引当                          *
*    作成日／更新日　　　：　93/05/25                          *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　請求合計Ｆと，請求合計Ｆ２でマッチ*
*                            ングを行い．請求合計の合計が０以外*
*                            の時ＯＵＴＦＩＬＥに出力する．    *
*    （？？？？？？？？？？？） OLD ID = ZMO9999B              *
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SSE0032B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          93/05/25.
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SPECIAL-NAMES.
     CONSOLE     IS     CONS
     STATION     IS     STAT.
*--------------------------------------------------------------*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*---<<  ＩＮ１ファイル  >>---*
     SELECT   HSEIGKF2  ASSIGN    TO        DA-01-HSEIGKF3
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   IN-ST.
*----<< 請求合計Ｆ >>--*
     SELECT   HSEIGKF   ASSIGN    TO        DA-01-VI-SEIGKF1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   OUT-F01
                                                 OUT-F05
                        FILE      STATUS    IS   OUT-ST.
*
*--------------------------------------------------------------*
 DATA                   DIVISION.
 FILE                   SECTION.
*---<<  ＩＮ１ファイル  >>---*
 FD  HSEIGKF2.
     COPY        SETGKFA   OF        XFDLIB
                 JOINING   IN        PREFIX.
*----<< 請求合計Ｆ >>--*
 FD  HSEIGKF.
     COPY        SETGKFA   OF        XFDLIB
                 JOINING   OUT       PREFIX.
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  STATUS-AREA.
     03  IN-ST               PIC  X(02).
     03  OUT-ST              PIC  X(02).
 01  PSW-AREA.
     03  END-FLG             PIC  X(03)  VALUE SPACE.
 01  CNT-AREA.
     03  KEP-FLG             PIC  X(01)  VALUE SPACE.
     03  IN-CNT1             PIC  9(06)  VALUE ZERO.
     03  IN-CNT2             PIC  9(06)  VALUE ZERO.
     03  OUT-CNT             PIC  9(06)  VALUE ZERO.
*
*--------------------------------------------------------------*
*             ＭＡＩＮ　　　　ＭＯＤＵＬＥ                     *
*--------------------------------------------------------------*
 PROCEDURE              DIVISION.
**
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE      HSEIGKF2.
     MOVE      4000          TO   PROGRAM-STATUS.
     STOP     RUN.
**
 FILEERR-SEC2           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE      HSEIGKF.
     MOVE      4000          TO   PROGRAM-STATUS.
     STOP     RUN.
 END     DECLARATIVES.
****************************************************************
 GENERAL-PROCESS             SECTION.
     PERFORM       INIT-SEC.
     PERFORM       MAIN-SEC
                   UNTIL     END-FLG   =    "END".
     PERFORM       END-SEC.
     STOP      RUN.
 GENERAL-END.
     EXIT.
****************************************************************
*      _０　　初期処理                                        *
****************************************************************
 INIT-SEC               SECTION.
*
     OPEN     INPUT     HSEIGKF2.
     OPEN     I-O       HSEIGKF.
     MOVE     SPACE          TO   END-FLG.
*
     READ     HSEIGKF2
          AT  END
              MOVE     "END" TO   END-FLG
     END-READ.
*
 INIT-END.
     EXIT.
****************************************************************
*      _０　　メイン処理                                      *
****************************************************************
 MAIN-SEC               SECTION.
*
     MOVE     IN-F01        TO   OUT-F01.
     MOVE     IN-F05        TO   OUT-F05.
     READ     HSEIGKF
          INVALID KEY
              PERFORM       OUTPUT-SEC
          NOT INVALID KEY
              PERFORM       REWRITE-SEC
     END-READ.
*
     READ     HSEIGKF2
          AT  END
              MOVE     "END" TO   END-FLG
     END-READ.
*
 MAIN-END.
     EXIT.
****************************************************************
*　　　　　　　　　　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 OUTPUT-SEC             SECTION.
*
     MOVE    SPACE     TO     OUT-REC.
     INITIALIZE               OUT-REC.
     MOVE    IN-REC    TO     OUT-REC.
     WRITE   OUT-REC
     END-WRITE.
*
 OUTPUT-END.
     EXIT.
****************************************************************
*　　　　　　　　　　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 REWRITE-SEC            SECTION.
*
     ADD     IN-F06    TO     OUT-F06.
     IF      OUT-F06   =      ZERO
             DELETE           HSEIGKF
     ELSE
             REWRITE          OUT-REC
     END-IF.
*
 REWRITE-END.
     EXIT.
****************************************************************
*      3.0        終了処理                                     *
****************************************************************
 END-SEC                SECTION.
*
     CLOSE    HSEIGKF2  HSEIGKF.
*
 END-END.
     EXIT.
*****************<<  PROGRAM  END  >>***********************

```
