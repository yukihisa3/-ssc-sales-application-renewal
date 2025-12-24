# SSE8798B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SSE8798B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　在庫管理システム　　　　　　　　　*
*    モジュール名　　　　：　繰越ＤＣＭデータチェック          *
*    作成日／更新日　　　：　2011/11/01                        *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　ＤＣＭ請求データを読み、発注日、　*
*                            納品日が空白の場合、６桁納品日を　*
*                            セットする。　　　　　　　　　　  *
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SSE8798B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          11/11/01.
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SPECIAL-NAMES.
     CONSOLE     IS     CONS
     STATION     IS     STAT.
*--------------------------------------------------------------*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*---<<  ＩＮ１ファイル  >>---*
     SELECT   SETGK87  ASSIGN    TO        DA-01-SETGK87
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   IN-ST.
*
*--------------------------------------------------------------*
 DATA                   DIVISION.
 FILE                   SECTION.
*---<<  ＩＮ１ファイル  >>---*
 FD  SETGK87.
     COPY        SETGK87   OF        XFDLIB
                 JOINING   IN        PREFIX.
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  STATUS-AREA.
     03  IN-ST               PIC  X(02).
 01  PSW-AREA.
     03  END-FLG             PIC  X(03)  VALUE SPACE.
     03  ERR-FLG             PIC  X(01)  VALUE SPACE.
 01  CNT-AREA.
     03  KEP-FLG             PIC  X(01)  VALUE SPACE.
     03  IN-CNT              PIC  9(07)  VALUE ZERO.
     03  OUT-CNT             PIC  9(07)  VALUE ZERO.
*
 01  LINK-AREA2.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
*
*--------------------------------------------------------------*
*             ＭＡＩＮ　　　　ＭＯＤＵＬＥ                     *
*--------------------------------------------------------------*
 PROCEDURE              DIVISION.
**
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE      SETGK87.
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
     OPEN     I-O       SETGK87.
     MOVE     SPACE          TO   END-FLG.
*
     READ     SETGK87
          AT  END
              MOVE     "END" TO   END-FLG
          NOT  AT  END
              ADD       1    TO   IN-CNT
     END-READ.
*
 INIT-END.
     EXIT.
****************************************************************
*      _０　　メイン処理                                      *
****************************************************************
 MAIN-SEC               SECTION.
*納品日をチェック
     MOVE  SPACE                  TO        ERR-FLG.
     IF    IN-F14 = SPACE
     OR    IN-F14 = "00000000"
              MOVE     "3"        TO        LINK-IN-KBN
              MOVE      IN-F04    TO        LINK-IN-YMD6
              CALL     "SKYDTCKB" USING     LINK-IN-KBN
                                            LINK-IN-YMD6
                                            LINK-IN-YMD8
                                            LINK-OUT-RET
                                            LINK-OUT-YMD8
              IF   LINK-OUT-RET   =    ZERO
                   MOVE      LINK-OUT-YMD8  TO   IN-F14
              ELSE
                   MOVE      "20"           TO   IN-F14(1:2)
                   MOVE      IN-F04         TO   IN-F14(3:6)
              END-IF
              MOVE "1"                      TO   ERR-FLG
     END-IF.
*
     IF    IN-F13 = SPACE
     OR    IN-F13 = "00000000"
            MOVE IN-F14     TO   IN-F13
            MOVE "1"        TO   ERR-FLG
     END-IF.
*更新（発注日／納品日を変更した場合）
     IF  ERR-FLG = "1"
         REWRITE  IN-REC
         ADD      1             TO   OUT-CNT
     END-IF.
*
     READ     SETGK87
          AT  END
              MOVE     "END" TO   END-FLG
          NOT  AT  END
              ADD       1    TO   IN-CNT
     END-READ.
*
 MAIN-END.
     EXIT.
****************************************************************
*      3.0        終了処理                                     *
****************************************************************
 END-SEC                SECTION.
*
     CLOSE    SETGK87.
*
     DISPLAY "IN-CNT  = " IN-CNT    UPON CONS.
     DISPLAY "OUT-CNT = " OUT-CNT   UPON CONS.
*
 END-END.
     EXIT.
*****************<<  PROGRAM  END  >>***********************

```
