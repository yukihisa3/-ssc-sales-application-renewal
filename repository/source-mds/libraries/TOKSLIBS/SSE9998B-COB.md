# SSE9998B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SSE9998B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　請求管理システム　　　　　　　　　*
*    モジュール名　　　　：　請求データ削除（締日指定）        *
*    作成日／更新日　　　：　06/04/28                          *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　パラメタで受け取った請求締日（６桁*
*                            ）以下の請求データを削除する。　　*
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SSE9998B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          06/04/28.
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SPECIAL-NAMES.
     CONSOLE     IS     CONS
     STATION     IS     STAT.
*--------------------------------------------------------------*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*----<< 請求合計Ｆ >>--*
     SELECT   SETGKFA   ASSIGN    TO        DA-01-VI-SETGKFA1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   SEQUENTIAL
                        RECORD    KEY       IS   OUT-F01
                                                 OUT-F05
                        FILE      STATUS    IS   OUT-ST.
*
*--------------------------------------------------------------*
 DATA                   DIVISION.
 FILE                   SECTION.
*----<< 請求合計Ｆ >>--*
 FD  SETGKFA.
     COPY        SETGKFA   OF        XFDLIB
                 JOINING   OUT       PREFIX.
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  STATUS-AREA.
     03  OUT-ST              PIC  X(02).
 01  PSW-AREA.
     03  END-FLG             PIC  X(03)  VALUE SPACE.
 01  CNT-AREA.
     03  KEP-FLG             PIC  X(01)  VALUE SPACE.
     03  DEL-CNT             PIC  9(06)  VALUE ZERO.
     03  READ-CNT            PIC  9(06)  VALUE ZERO.
     03  OUT-CNT             PIC  9(06)  VALUE ZERO.
*
 LINKAGE                SECTION.
 01  PARA-SIMEBI             PIC  9(06).
 01  PARA-TORICD             PIC  9(08).
*--------------------------------------------------------------*
*             ＭＡＩＮ　　　　ＭＯＤＵＬＥ                     *
*--------------------------------------------------------------*
 PROCEDURE              DIVISION  USING  PARA-SIMEBI PARA-TORICD.
**
 DECLARATIVES.
**
 FILEERR-SEC2           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE      SETGKFA.
     MOVE      4000          TO   PROGRAM-STATUS.
     DISPLAY "## SSE9998B ABEND ST = " OUT-ST " ##" UPON CONS.
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
*      ■０　　初期処理                                        *
****************************************************************
 INIT-SEC               SECTION.
*
     OPEN     I-O       SETGKFA.
     MOVE     SPACE          TO   END-FLG.
*
     MOVE     SPACE          TO   OUT-REC.
     INITIALIZE                   OUT-REC.
     MOVE     PARA-TORICD    TO   OUT-F01.
     START  SETGKFA KEY IS >=  OUT-F01  OUT-F05
            INVALID
            MOVE       "END" TO   END-FLG
            GO               TO   INIT-END
     END-START.
*
     PERFORM  SETGKFA-READ-SEC.
*
 INIT-END.
     EXIT.
****************************************************************
*      ■０　　メイン処理                                      *
****************************************************************
 MAIN-SEC               SECTION.
*
      DELETE  SETGKFA.
      ADD     1     TO     DEL-CNT.
*
      PERFORM SETGKFA-READ-SEC.
*
 MAIN-END.
     EXIT.
****************************************************************
*      3.0        終了処理                                     *
****************************************************************
 END-SEC                SECTION.
*
     CLOSE    SETGKFA.
*
     DISPLAY "READ-CNT = " READ-CNT  UPON CONS.
     DISPLAY "DEL-CNT  = " DEL-CNT   UPON CONS.
*
 END-END.
     EXIT.
****************************************************************
*      3.0        請求ﾌｧｲﾙ読込み
****************************************************************
 SETGKFA-READ-SEC       SECTION.
*
     READ     SETGKFA
          AT  END
              MOVE     "END" TO   END-FLG
              GO             TO   SETGKFA-READ-EXIT
          NOT  AT  END
              ADD       1    TO   READ-CNT
     END-READ.
*件数表示
     IF       READ-CNT(4:3) = "000" OR "500"
              DISPLAY "READ-CNT = " READ-CNT  UPON CONS
     END-IF.
*取引先ＣＤチェック
     IF       OUT-F01  >  PARA-TORICD
              MOVE     "END" TO   END-FLG
              GO             TO   SETGKFA-READ-EXIT
     END-IF.
*
     IF       OUT-F02  <  PARA-SIMEBI
              CONTINUE
     ELSE
              GO          TO       SETGKFA-READ-SEC
     END-IF.
*
 SETGKFA-READ-EXIT.
     EXIT.
*****************<<  PROGRAM  END  >>***********************

```
