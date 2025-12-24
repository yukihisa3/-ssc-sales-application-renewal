# SSE7199B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SSE7199B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　Ｊ本田請求処理　　　　　　　　　　*
*    モジュール名　　　　：　請求データ作成（オンライン用）　　*
*    作成日／更新日　　　：　05/10/07                          *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　請求ファイルを削除する。　　　　　*
*　　　　　　　　　　　　：　　　　　　　　　　　　　　　　　　*
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SSE7199B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          05/10/07.
 DATE-COMPILED.
 SECURITY.              NONE.
*
 ENVIRONMENT            DIVISION.
*
 CONFIGURATION          SECTION.
 SPECIAL-NAMES.
         YB        IS   PITCH-1-5
         YB-21     IS   BAIKAKU-1-5
         YA-21     IS   BAIKAKU
         STATION   IS   STAT
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*----<< 請求合計ファイル>>-*
     SELECT   JDJSEKF   ASSIGN         DA-01-VI-JDJSEKL1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE SEQUENTIAL
                        RECORD    KEY  SEI-F01
                                       SEI-F02
                                       SEI-F03
                        STATUS         SEI-ST.
*
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< 請求合計ファイル>>-*
 FD  JDJSEKF            LABEL RECORD   IS   STANDARD.
     COPY     JDJSEKF   OF        XFDLIB
              JOINING   SEI       PREFIX.
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  FLAGS.
     03  END-FLG        PIC  9(01)    VALUE  ZERO.
     03  IDX            PIC  9(01)    VALUE  ZERO.
 01  COUNTERS.
     03  READ-CNT       PIC  9(06)    VALUE  ZERO.
     03  DEL-CNT        PIC  9(06)    VALUE  ZERO.
     03  WRITE-CNT      PIC  9(06)    VALUE  ZERO.
     03  OUT-CNT        PIC  9(06).
     03  LINE-CNT       PIC  9(03).
     03  PAGE-CNT       PIC  9(03).
     03  TEN-CNT        PIC  9(05)    VALUE  ZERO.
     03  MEI-CNT        PIC  9(05)    VALUE  ZERO.
     03  GYO-CNT        PIC  9(05)    VALUE  ZERO.
     03  ZEN-CNT        PIC  9(05)    VALUE  ZERO.
*
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  SEI-ST             PIC  X(02).
 01  TEN-ST             PIC  X(02).
 01  CVCS-ST            PIC  X(02).
*
*----<< ｼｭｳｹｲ ﾜｰｸ >>--*
 01  GOKEI-AREA.
     03  KEI-KEN        PIC  9(05)     VALUE  ZERO.
     03  KEI-KIN        PIC S9(09)     VALUE  ZERO.
*
*----<< ﾜｰｸ ｴﾘｱ >>--*
 01  WK-AREA.
     03  WK-TORICD      PIC  9(06).
 01  IND                PIC  9(02)     VALUE 1.
 01  WK-TENCD           PIC  9(04)     VALUE ZERO.
 01  WK-SEI-F02         PIC  9(04)     VALUE ZERO.
*
*----<< ﾋﾂﾞｹ ﾜｰｸ >>--*
 01  SYS-DATEW          PIC  9(08).
 01  SYS-DATE           PIC  9(06).
 01  FILLER             REDEFINES      SYS-DATE.
     03  SYS-YY         PIC  9(02).
     03  SYS-MM         PIC  9(02).
     03  SYS-DD         PIC  9(02).
 01  SYS-TIME           PIC  9(08).
 01  FILLER             REDEFINES      SYS-TIME.
     03  SYS-HH         PIC  9(02).
     03  SYS-MN         PIC  9(02).
     03  SYS-SS         PIC  9(02).
     03  SYS-MS         PIC  9(02).
*
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
****************************************************************
 PROCEDURE              DIVISION.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
*----<< ワークファイル >>--*
 JDJSEKF-ERR            SECTION.
     USE AFTER     EXCEPTION PROCEDURE      JDJSEKF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SSE7199B JDJSEKF ERROR " SEI-ST  " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     MOVE     "4000"              TO      PROGRAM-STATUS.
     STOP     RUN.
 END DECLARATIVES.
****************************************************************
*　　　　メインモジュール　　　　　　　　　　　　　　　　　　　*
****************************************************************
 GENERAL-PROCESS        SECTION.
*
     PERFORM  INIT-RTN.
     IF       END-FLG   =    0
              PERFORM       MAIN-RTN
                            UNTIL END-FLG = 1
     END-IF.
     PERFORM  END-RTN.
     STOP RUN.
 GENERAL-EXIT.
     EXIT.
****************************************************************
*　　　　初期処理　　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-RTN               SECTION.
     ACCEPT   SYS-DATE       FROM DATE.
******************
*システム日付編集*
******************
     ACCEPT      SYS-DATE  FROM      DATE.
     MOVE       "3"        TO        LINK-IN-KBN.
     MOVE        SYS-DATE  TO        LINK-IN-YMD6.
     CALL       "SKYDTCKB"   USING   LINK-IN-KBN
                                     LINK-IN-YMD6
                                     LINK-IN-YMD8
                                     LINK-OUT-RET
                                     LINK-OUT-YMD8.
     IF          LINK-OUT-RET   =    ZERO
         MOVE    LINK-OUT-YMD8  TO   SYS-DATEW
     ELSE
         MOVE    ZERO           TO   SYS-DATEW
     END-IF.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SSE7199B START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
     OPEN     I-O       JDJSEKF.
*----<< ﾜｰｸ ｼｮｷｾｯﾄ >>-*
     INITIALIZE         COUNTERS.
     INITIALIZE         FLAGS.
     INITIALIZE         GOKEI-AREA.
     INITIALIZE         WK-AREA.
*
*
     PERFORM  SEI-READ.
*
 INIT-RTN-EXIT.
     EXIT.
****************************************************************
*　　　　メイン処理　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-RTN               SECTION.
*
     IF    SEI-F01  <  20060321
           DELETE  JDJSEKF
           ADD     1               TO   DEL-CNT
     END-IF.
*
     PERFORM  SEI-READ.
*
 MAIN-RTN-EXIT.
     EXIT.
****************************************************************
*　　　　終了処理　　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-RTN                SECTION.
*
*---<< ｺﾞｳｹｲ ｲﾝｻﾂ >>---*
*
     CLOSE    JDJSEKF.
*
     DISPLAY "+++ READ-CNT   = " READ-CNT  " +++" UPON CONS.
     DISPLAY "+++ DELETE-CNT = " DEL-CNT   " +++" UPON CONS.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SSE7199B END *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS        UPON CONS.
 END-RTN-EXIT.
     EXIT.
****************************************************************
*　　　　請求合計ＦＲＥＡＤ　　　　　　　　　　　　　　　　　　*
****************************************************************
 SEI-READ               SECTION.
     READ     JDJSEKF   AT   END
              MOVE      1         TO   END-FLG
              GO   TO   SEI-READ-EXIT
     END-READ.
*
     ADD      1              TO   READ-CNT  MEI-CNT.
*
 SEI-READ-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*

```
