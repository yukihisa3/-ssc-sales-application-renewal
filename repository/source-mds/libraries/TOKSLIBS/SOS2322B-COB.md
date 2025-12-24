# SOS2322B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SOS2322B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　富岡場所ＣＤ変更　　　　　　　　　*
*    業務名　　　　　　　：　富岡場所ＣＤ変更　　　　　　　　　*
*    モジュール名　　　　：　富岡場所ＣＤ変更　　　　          *
*    作成日／更新日　　　：　06/11/27                          *
*    作成者／更新者　　　：　NAV                               *
*    処理概要　　　　　　：　売上伝票Ｆ内の未売上の場所８３を  *
*                            ６Ｍに変更する。                  *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SOS2322B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          03/02/07.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*売上伝票データ
     SELECT   SHTDENF  ASSIGN    TO        DA-01-VI-SHTDENL6
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       DEN-F277  DEN-F274
                                            DEN-F09   DEN-F02
                                            DEN-F03
                        FILE      STATUS    IS   DEN-STATUS.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    売上伝票データ　ＲＬ＝１０２０
******************************************************************
 FD  SHTDENF
                        LABEL RECORD   IS   STANDARD.
     COPY     SHTDENF   OF        XFDLIB
              JOINING   DEN  AS   PREFIX.
*
*****************************************************************
*
 WORKING-STORAGE        SECTION.
*    ｶｳﾝﾄ
 01  END-FG                  PIC  9(01)     VALUE  ZERO.
 01  READ-CNT                PIC  9(08)     VALUE  ZERO.
 01  CRT-CNT                 PIC  9(08)     VALUE  ZERO.
 01  AKA-CNT                 PIC  9(08)     VALUE  ZERO.
 01  KUR-CNT                 PIC  9(08)     VALUE  ZERO.
*
 01  WK-AREA.
*システム日付の編集
     03  SYS-DATE          PIC 9(06).
     03  SYS-DATEW         PIC 9(08).
 01  WK-ST.
     03  DEN-STATUS        PIC  X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SOS2322B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SOS2322B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SOS2322B".
         05  FILLER         PIC   X(11)  VALUE
                                         " ABEND *** ".
     03  ABEND-FILE.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  AB-FILE        PIC   X(08).
         05  FILLER         PIC   X(06)  VALUE " ST = ".
         05  AB-STS         PIC   X(02).
         05  FILLER         PIC   X(05)  VALUE " *** ".
     03  SEC-NAME.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(07)  VALUE " SEC = ".
         05  S-NAME         PIC   X(30).
     03  MSG-IN.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(09)  VALUE " INPUT = ".
         05  IN-CNT         PIC   9(06).
         05  FILLER         PIC   X(05)  VALUE " *** ".
     03  MSG-OUT.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(09)  VALUE " OUTPUT= ".
         05  OUT-CNT        PIC   9(06).
         05  FILLER         PIC   X(05)  VALUE " *** ".
*
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
*
 LINKAGE                SECTION.
 01  PARA-SOKCD1            PIC   X(02).
 01  PARA-SOKCD2            PIC   X(02).
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION USING PARA-SOKCD1 PARA-SOKCD2.
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   SHTDENF.
     MOVE      "SHTDENF"   TO   AB-FILE.
     MOVE      DEN-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 END     DECLARATIVES.
*****************************************************************
*                                                                *
******************************************************************
 GENERAL-PROCESS       SECTION.
*
     MOVE     "PROCESS-START"     TO   S-NAME.
     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC
              UNTIL     END-FG    =    9.
     PERFORM  END-SEC.
*
****************************************************************
*　　　　　　　初期処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-SEC               SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
     OPEN     INPUT     SHTDENF.
 INIT-010.
*
     PERFORM  SHTDENF-READ-SEC.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"           TO   S-NAME.
*倉庫ＣＤ入替え
*****MOVE     PARA-SOKCD2         TO   DEN-F08  DEN-F09  DEN-F48.
*****REWRITE  DEN-REC.
*
     PERFORM  SHTDENF-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
*
     DISPLAY "READ-CNT  =  "  READ-CNT  UPON CONS.
     DISPLAY "TAIS-CNT  =  "  CRT-CNT   UPON CONS.
*
     CLOSE    SHTDENF.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
****************************************************************
*　　売上伝票ファイル読込処理　　　　　　　　　　　　　　　　　*
****************************************************************
 SHTDENF-READ-SEC     SECTION.
*
     MOVE "SHTDENF-READ-SEC" TO   S-NAME.
*
     READ SHTDENF NEXT AT END
          MOVE     9          TO   END-FG
          GO                  TO   SHTDENF-READ-EXIT
          NOT  AT  END
          ADD      1          TO   READ-CNT
     END-READ.
*件数表示
     IF   READ-CNT(6:3) =  "000" OR "500"
          DISPLAY "READ-CNT = " READ-CNT  UPON CONS
     END-IF.
*売上計上済は対象外
     IF   DEN-F277  =  9
          MOVE     9          TO   END-FG
          GO                  TO   SHTDENF-READ-EXIT
     END-IF.
*指定倉庫の場合
     IF   DEN-F08  =  PARA-SOKCD1
     OR   DEN-F09  =  PARA-SOKCD1
     OR   DEN-F48  =  PARA-SOKCD1
          CONTINUE
     ELSE
          GO                  TO   SHTDENF-READ-SEC
     END-IF.
*
     IF   DEN-F03  =  80
          GO                  TO   SHTDENF-READ-SEC
     END-IF.
     DISPLAY "DEN-F112 = " DEN-F112  UPON CONS.
     ADD        1             TO   CRT-CNT.
*
 SHTDENF-READ-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*

```
