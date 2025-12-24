****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　在庫管理システム　　　　　　　　　*
*    モジュール名　　　　：　在庫引当                          *
*    作成日／更新日　　　：　2004/05/28                        *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　請求合計Ｆからケーヨー分を抽出す　*
*                            る。　　　　　　　　　　　　　　　*
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SSE0035B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          04/05/28.
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SPECIAL-NAMES.
     CONSOLE     IS     CONS
     STATION     IS     STAT.
*--------------------------------------------------------------*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*---<< ケーヨー請求Ｆ >>---*
     SELECT   SETGKFB   ASSIGN    TO        DA-01-VI-SETGKFB1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   OT-F01
                                                 OT-F05
                        FILE      STATUS    IS   OT-ST.
*----<< 請求合計Ｆ >>--*
     SELECT   SETGKFA   ASSIGN    TO        DA-01-VI-SETGKFA1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   SEQUENTIAL
                        RECORD    KEY       IS   IN-F01
                                                 IN-F05
                        FILE      STATUS    IS   OT-ST.
*
*--------------------------------------------------------------*
 DATA                   DIVISION.
 FILE                   SECTION.
*---<<  ＩＮ１ファイル  >>---*
 FD  SETGKFB.
     COPY        SETGKFA   OF        XFDLIB
                 JOINING   OT        PREFIX.
*----<< 請求合計Ｆ >>--*
 FD  SETGKFA.
     COPY        SETGKFA   OF        XFDLIB
                 JOINING   IN        PREFIX.
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  STATUS-AREA.
     03  IN-ST               PIC  X(02).
     03  OT-ST               PIC  X(02).
 01  PSW-AREA.
     03  END-FLG             PIC  X(03)  VALUE SPACE.
 01  CNT-AREA.
     03  KEP-FLG             PIC  X(01)  VALUE SPACE.
     03  IN-CNT              PIC  9(06)  VALUE ZERO.
     03  OT-CNT              PIC  9(06)  VALUE ZERO.
*
*--------------------------------------------------------------*
*             ＭＡＩＮ　　　　ＭＯＤＵＬＥ                     *
*--------------------------------------------------------------*
 PROCEDURE              DIVISION.
**
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE      SETGKFB.
     DISPLAY "## ST= " OT-ST  UPON CONS.
     MOVE      4000          TO   PROGRAM-STATUS.
     STOP     RUN.
**
 FILEERR-SEC2           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE      SETGKFA.
     DISPLAY "## ST= " IN-ST  UPON CONS.
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
     OPEN     I-O       SETGKFB.
     OPEN     INPUT     SETGKFA.
*
     MOVE     SPACE          TO   END-FLG.
*請求合計Ｆスタート
     MOVE     173            TO   IN-F01.
     MOVE     SPACE          TO   IN-F05.
     START    SETGKFA  KEY  IS  >=  IN-F01  IN-F05
       INVALID
              MOVE  "END"    TO   END-FLG
              DISPLAY NC"＃＃対象無し＃＃" UPON CONS
              GO             TO   INIT-END
     END-START.
*
     PERFORM SETGKFA-READ-SEC.
*
     IF  END-FLG  = "END"
              DISPLAY NC"＃＃対象無し＃＃" UPON CONS
              GO             TO   INIT-END
     END-IF.
*
 INIT-END.
     EXIT.
****************************************************************
*      請求合計Ｆ　読込み
****************************************************************
 SETGKFA-READ-SEC       SECTION.
*
     READ   SETGKFA
            NEXT  AT  END
              MOVE     "END" TO   END-FLG
              GO             TO   SETGKFA-READ-END
     END-READ.
*
     IF     IN-F01  >  173
              MOVE     "END" TO   END-FLG
              GO             TO   SETGKFA-READ-END
     END-IF.
*
     ADD     1              TO   IN-CNT.
*
     IF      IN-CNT(4:3) = "500" OR "000"
             DISPLAY "READ-CNT  = " IN-CNT UPON CONS
     END-IF.
*
 SETGKFA-READ-END.
     EXIT.
****************************************************************
*      _０　　メイン処理                                      *
****************************************************************
 MAIN-SEC               SECTION.
*
     MOVE     IN-F01        TO   OT-F01.
     MOVE     IN-F05        TO   OT-F05.
     READ     SETGKFB
          INVALID KEY
              PERFORM       OUTPUT-SEC
              ADD    1      TO   OT-CNT
          NOT INVALID KEY
              CONTINUE
     END-READ.
*
     PERFORM  SETGKFA-READ-SEC.
*
 MAIN-END.
     EXIT.
****************************************************************
*　　　　　　　　　　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 OUTPUT-SEC             SECTION.
*
     MOVE    SPACE     TO     OT-REC.
     INITIALIZE               OT-REC.
     MOVE    IN-REC    TO     OT-REC.
     WRITE   OT-REC
     END-WRITE.
*
 OUTPUT-END.
     EXIT.
****************************************************************
*      3.0        終了処理                                     *
****************************************************************
 END-SEC                SECTION.
*
     CLOSE    SETGKFB  SETGKFA.
*
     DISPLAY "READ-CNT  = " IN-CNT  UPON  CONS.
     DISPLAY "WRITE-CNT = " OT-CNT  UPON CONS.
*
 END-END.
     EXIT.
*****************<<  PROGRAM  END  >>***********************
