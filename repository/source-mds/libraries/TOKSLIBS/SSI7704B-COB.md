# SSI7704B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SSI7704B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　ロイヤルＨＣ　支払照合　　　　　　*
*    モジュール名　　　　：　請求データ編集　　　              *
*    作成日／更新日　　　：　08/11/17                          *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　請求伝票番号の下１桁をカットする。*
*                                                              *
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SSI7704B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          02/12/05.
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SPECIAL-NAMES.
     CONSOLE     IS     CONS
     STATION     IS     STAT.
*--------------------------------------------------------------*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*----<< 請求合計Ｆ >>--*
     SELECT   SETGKFS   ASSIGN    TO        DA-01-VI-SETGKFS1
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
 FD  SETGKFS.
     COPY        SETGKFS   OF        XFDLIB
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
**
 FILEERR-SEC2           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE      SETGKFS.
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
     OPEN     I-O       SETGKFS.
     MOVE     SPACE          TO   END-FLG.
*
     READ     SETGKFS
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
     MOVE     "0"           TO   OUT-F13(1:1).
     MOVE     OUT-F05(1:8)  TO   OUT-F13(2:8).
     REWRITE  OUT-REC.
*
     READ     SETGKFS
          AT  END
              MOVE     "END" TO   END-FLG
     END-READ.
*
 MAIN-END.
     EXIT.
****************************************************************
*      3.0        終了処理                                     *
****************************************************************
 END-SEC                SECTION.
*
     CLOSE    SETGKFS.
*
 END-END.
     EXIT.
*****************<<  PROGRAM  END  >>***********************

```
