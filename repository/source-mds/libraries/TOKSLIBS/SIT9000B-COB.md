# SIT9000B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SIT9000B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　ＩＴ統制（共通）　　　　　　　　　*
*    業務名　　　　　　　：　ＩＴ統制（共通）　　　　　　　　　*
*    モジュール名　　　　：　ログインユーザー取得　　　　　　　*
*    作成日／更新日　　　：　09/03/11                          *
*    作成者／更新者　　　：　NAV TAKAHASHI                     *
*    処理概要　　　　　　：　テンポラリのログインユーザーＦよ　*
*                            り、担当者ＣＤを取得する。　　　　*
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SIT9000B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          06/05/16.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*ログインユーザーファイル
     SELECT   LOGINUSR  ASSIGN    TO        DA-01-S-LOGINUSR
                        ACCESS    MODE IS   SEQUENTIAL
                        FILE      STATUS    LOG-STATUS
                        ORGANIZATION   IS   SEQUENTIAL.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    受信データ　ＲＬ＝　１２８　  ＢＦ＝　１
******************************************************************
 FD  LOGINUSR
                        BLOCK CONTAINS      1    RECORDS
                        LABEL RECORD   IS   STANDARD.
*
 01  LOG-REC.
     03  LOG-F01                 PIC  X(08).
     03  LOG-F02                 PIC  X(04).
     03  LOG-F03                 PIC  X(08).
*
*****************************************************************
*
 WORKING-STORAGE        SECTION.
*    ｶｳﾝﾄ
 01  END-FLG                 PIC  X(03)     VALUE  SPACE.
*ステータス
 01  WORK-STATUS.
     03  LOG-STATUS        PIC  X(02).
*メッセージエリア
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SIT9000B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SIT9000B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SIT9000B".
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
*
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
*
 LINKAGE                SECTION.
 01  PARA-BUMON             PIC   X(04).
 01  PARA-TANCD             PIC   X(08).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION USING PARA-BUMON PARA-TANCD.
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   LOGINUSR.
     MOVE      "LOGINUSR"   TO   AB-FILE.
     MOVE      LOG-STATUS   TO   AB-STS.
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
     MOVE     "PROCESS-START"           TO   S-NAME.
*
     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC.
     PERFORM  END-SEC.
     STOP RUN.
*
 GENERAL-PROCESS-EXIT.
     EXIT.
*
****************************************************************
*　　　　　　　初期処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-SEC               SECTION.
     MOVE     "INIT-SEC"                TO   S-NAME.
*
     OPEN     INPUT     LOGINUSR.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"                 TO  S-NAME.
*初期レコード読み込み
     READ     LOGINUSR
              AT END  MOVE 4050         TO  PROGRAM-STATUS
                      GO                TO  MAIN-EXIT
          NOT AT END  MOVE LOG-F02      TO  PARA-BUMON
                      MOVE LOG-F03      TO  PARA-TANCD
     END-READ.
*担当者CD判定
     IF   PARA-TANCD  =  SPACE
          MOVE   4050                   TO  PROGRAM-STATUS
     END-IF.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"                 TO  S-NAME.
*
     CLOSE     LOGINUSR.
*
     DISPLAY NC"＃担当者" " = " PARA-TANCD  UPON CONS.
*
 END-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*

```
