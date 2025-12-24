# SNA0110B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SNA0110B.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　ＨＧ苗業務連携　　　　 　       *
*    業務名　　　　　　　：　                                  *
*    モジュール名　　　　：　小売連携累積データ削除　　　　　　*
*    作成日／更新日　　　：　11/10/20                          *
*    作成者／更新者　　　：　ＮＡＶ畠山　                      *
*    処理概要　　　　　　：　小売連携累積データを削除する　　　*
*                                                              *
****************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SNA0110B.
 AUTHOR.               HATAKEYAMA.
 DATE-WRITTEN.         11/10/20.
****************************************************************
 ENVIRONMENT           DIVISION.
****************************************************************
 CONFIGURATION         SECTION.
 SPECIAL-NAMES.
     CONSOLE      IS   CONS.
*
 INPUT-OUTPUT          SECTION.
 FILE-CONTROL.
*条件ファイル
     SELECT  JYOKEN  ASSIGN TO     DA-01-VI-JYOKEN1
             ORGANIZAITION         INDEXED
             ACCESS      MODE      RANDOM
             RECORD      KEY       JYO-F01 JYO-F02
                         FILE      STATUS    JYO-ST.
*
*連携NO管理テーブル
     SELECT  NARKANL ASSIGN TO     DA-01-VI-NARKANL4
             ORGANIZAITION         INDEXED
             ACCESS      MODE      SEQUENTIAL
             RECORD      KEY       NAK-F01
                         FILE      STATUS    NAK-ST.
*小売携累積ファイル
     SELECT  NARRUIF ASSIGN   TO   DA-01-VI-NARRUIL1
             ORGANIZAITION         INDEXED
             ACCESS      MODE      SEQUENTIAL
             RECORD      KEY       NAR-F01
                         FILE      STATUS    NAR-ST.
****************************************************************
 DATA                DIVISION.
*************************************************************
 FILE                SECTION.
****************************************************************
*    FILE = 条件ファイル　　　　　　　　　　                *
****************************************************************
 FD  JYOKEN
                       LABEL     RECORD    IS   STANDARD.
                       COPY      HJYOKEN   OF   XFDLIB
                       JOINING   JYO       AS   PREFIX.
*
****************************************************************
*    FILE = 連携NO管理テーブル　　　　　　　　　             *
****************************************************************
 FD  NARKANL
                       LABEL     RECORD    IS   STANDARD.
                       COPY      NARKANF   OF   XFDLIB
                       JOINING   NAK       AS   PREFIX.
****************************************************************
*    FILE = 小売連携累積ファイル　　　　　　　　　             *
****************************************************************
 FD  NARRUIF
                       LABEL     RECORD    IS   STANDARD.
                       COPY      NARRUIF   OF   XFDLIB
                       JOINING   NAR       AS   PREFIX.
*
*************C**************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
 01  END-FLG                   PIC  X(01)     VALUE   SPACE.
 01  SAKUJO-FLG                PIC  X(01)     VALUE   SPACE.
 01  DLT-CNT                   PIC  9(06)     VALUE   ZERO.
 01  DLT-CNT1                  PIC  9(06)     VALUE   ZERO.
 01  WK-TUKISUU                PIC  9(02)     VALUE   ZERO.
 01  WK-RENBAN                 PIC  X(09)     VALUE   SPACE.
 01  WK-HZKSA                  PIC  9(02)     VALUE   ZERO.
 01  WK-NENSA                  PIC  9(04)     VALUE   ZERO.
 01  WK-F074.
     03  WK-F074A              PIC  9(02)     VALUE   ZERO.
     03  WK-F074B              PIC  9(06)     VALUE   ZERO.
 01  WK-DATE.
     03  WK-YY                 PIC  9(02)     VALUE   ZERO.
     03  WK-MM                 PIC  9(02)     VALUE   ZERO.
     03  WK-DD                 PIC  9(02)     VALUE   ZERO.
*----削除条件１
 01  SAKUJO1.
     03  SAKUJO1-YY             PIC  9(02)     VALUE   ZERO.
     03  SAKUJO1-MM             PIC  9(02)     VALUE   ZERO.
     03  SAKUJO1-DD             PIC  9(02)     VALUE   ZERO.
*----削除条件2
 01  SAKUJO2.
     03  SAKUJO2-YY             PIC  9(02)     VALUE   ZERO.
     03  SAKUJO2-MM             PIC  9(02)     VALUE   ZERO.
     03  SAKUJO2-DD             PIC  9(02)     VALUE   ZERO.

*ステータス領域
 01  STATUS-AREA.
     03  JYO-ST                PIC  X(02).
     03  NAK-ST                PIC  X(02).
     03  NAR-ST                PIC  X(02).
*ファイルエラーメッセージ
 01  FILE-ERR.
     03  JYO-ERR           PIC N(09) VALUE
         NC"条件ファイルエラー".
     03  NAK-ERR           PIC N(13) VALUE
         NC"連携ＮＯ管理テーブルエラー".
     03  NAR-ERR           PIC N(13) VALUE
         NC"小売連携累積ファイルエラー".
***  エラーセクション名
 01  SEC-NAME.
     03  FILLER                   PIC  X(18)
         VALUE "### ERR-SEC    => ".
     03  S-NAME                   PIC  X(20).
***  エラーファイル名
 01  ERR-FILE.
     03  FILLER                   PIC  X(18)
         VALUE "### ERR-FILE   => ".
     03  E-FILE                   PIC  X(08).
***  エラーステータス名
 01  ERR-NAME.
    03  FILLER                   PIC  X(18)
         VALUE "### ERR-STATUS => ".
     03  E-ST                     PIC  9(02).
*------------------------------------------------------------*
 01  MSG-AREA.
     03  MSG-KENSU1.
         05  KENSU1A PIC  N(22) VALUE
         NC"＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
     03  MSG-KENSU2.
         05  KENSU2A PIC N(20) VALUE
             NC"＊　連携ＮＯ管理テーブル　削除件数　＝　".
         05  SAKUJO-KANRI  PIC 9(06) VALUE ZERO.
         05  KENSU2B PIC    N(02) VALUE NC"　＊".
     03  MSG-KENSU3.
         05  KENSU3A PIC N(20) VALUE
         NC"＊　連携累積ファイル　　　削除件数　＝　".
         05  SAKUJO-RUISEKI  PIC 9(06) VALUE ZERO.
         05  KENSU3B        PIC N(02) VALUE NC"　＊".
 LINKAGE              SECTION.
*------------------------------------------------------------*
*
**************************************************************
 PROCEDURE             DIVISION.
**************************************************************
 DECLARATIVES.
 FILEERR-SEC1              SECTION.
     USE         AFTER     EXCEPTION PROCEDURE JYOKEN.
     MOVE        JYO-ST    TO        E-ST.
     MOVE        "JYOKEN" TO       E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     JYO-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 FILEERR-SEC2              SECTION.
     USE         AFTER     EXCEPTION PROCEDURE NARKANL.
     MOVE        NAK-ST     TO       E-ST.
     MOVE        "NARKANL"  TO       E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     NAK-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 FILEERR-SEC3             SECTION.
     USE         AFTER     EXCEPTION PROCEDURE NARRUIF.
     MOVE        NAR-ST     TO       E-ST.
     MOVE        "NARRUIF"  TO       E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     NAR-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 END  DECLARATIVES.
****************************************************************
*             MAIN        MODULE                     0.0       *
****************************************************************
 PROCESS-START         SECTION.
     MOVE     "PROCESS-START"     TO   S-NAME.
     PERFORM   INIT-SEC.
     PERFORM   MAIN-SEC  THRU  MAIN-EXIT
               UNTIL  END-FLG = 1.
     PERFORM   END-SEC.
     STOP  RUN.
 PROCESS-END.
     EXIT.
****************************************************************
*             初期処理                               0.0       *
****************************************************************
 INIT-SEC              SECTION.
     MOVE     "INIT-SEC"     TO   S-NAME.
*ファイルのＯＰＥＮ
     OPEN      INPUT  JYOKEN.
     OPEN      I-O    NARKANL  NARRUIF.
*
*
*----- 条件ファイルの読み込み
     MOVE      "99"   TO       JYO-F01.
     MOVE      "NAE"  TO       JYO-F02.
     READ      JYOKEN
     INVALID
     DISPLAY "##  条件ファイル対象レコード無し ##" UPON CONS
     PERFORM   IJYOSHORI-SEC   THRU   IJYOSHORI-EXIT
     NOT INVALID
     PERFORM   SAKUJOJOKEN-SEC THRU   SAKUJOJOKEN-EXIT
     END-READ.
 INIT-EXIT.
     EXIT.
****************************************************************
*             メイン処理                             1.0       *
****************************************************************
 MAIN-SEC              SECTION.
     MOVE     "MAIN-SEC"     TO   S-NAME.
*連携ＮＯ管理テーブルの読み込み
 MAIN010.
     READ  NARKANL  AT  END
           MOVE   "1"        TO   END-FLG
           GO                TO   MAIN-EXIT
     END-READ.
 MAIN011.
     MOVE   NAK-F074         TO   WK-F074.
     IF  (  WK-F074B  <=  SAKUJO1 )  OR
       ( ( WK-F074B  <=  SAKUJO2 ) AND ( NAK-F04 = 3 OR 5 ) )
            DELETE   NARKANL
            ADD      1           TO   DLT-CNT
            MOVE     SPACE       TO   SAKUJO-FLG
            PERFORM SAKUJO-SEC UNTIL  SAKUJO-FLG = "1"
     END-IF.
 MAIN-EXIT.
     EXIT.
****************************************************************
*             終了処理                               3.0       *
****************************************************************
 END-SEC               SECTION.
*------削除件数の表示
*
     MOVE         DLT-CNT      TO    SAKUJO-KANRI.
     MOVE         DLT-CNT1     TO    SAKUJO-RUISEKI.
*
     DISPLAY   KENSU1A  UPON  CONS
     DISPLAY   KENSU2A  SAKUJO-KANRI KENSU2B    UPON  CONS
     DISPLAY   KENSU3A  SAKUJO-RUISEKI KENSU3B  UPON  CONS
     DISPLAY   KENSU1A  UPON  CONS
*
     MOVE     "END-SEC"      TO   S-NAME.
*ファイルのCLOSE処理
     CLOSE     JYOKEN  NARKANL NARRUIF.
*
 END-EXIT.
     EXIT.
****************************************************************
*             削除条件１、２の算出
****************************************************************
 SAKUJOJOKEN-SEC          SECTION.
*------削除条件１の算出.
*
     ACCEPT   WK-DATE      FROM  DATE.
     MOVE     WK-DATE      TO    SAKUJO1.
     MOVE     "99"         TO    SAKUJO1-DD.
     IF      SAKUJO1-MM >  JYO-F04
             COMPUTE SAKUJO1-MM  = SAKUJO1-MM -  JYO-F04
     ELSE
             DIVIDE  JYO-F04  BY  12  GIVING WK-NENSA
                     REMAINDER  WK-TUKISUU
             COMPUTE SAKUJO1-YY  = SAKUJO1-YY  -  WK-NENSA
             IF      WK-TUKISUU > SAKUJO1-MM
                COMPUTE SAKUJO1-MM = 12 - WK-TUKISUU
                COMPUTE SAKUJO1-YY = SAKUJO1-YY  -  1
             ELSE
                COMPUTE SAKUJO1-MM = SAKUJO1-MM  -  WK-TUKISUU
     END-IF.
*
*
*---削除条件2の算出.
     MOVE     WK-DATE      TO    SAKUJO2.
     IF       WK-DD  >     10
              COMPUTE SAKUJO2-DD  =  WK-DD  - 10
     ELSE
     COMPUTE   WK-HZKSA   =  10  -  SAKUJO2-DD
     EVALUATE  SAKUJO2-MM
               WHEN  "12"
                     MOVE "11"    TO SAKUJO2-MM
                     MOVE "30"    TO SAKUJO2-DD
                     COMPUTE SAKUJO2-DD = SAKUJO2-DD -  WK-HZKSA
               WHEN  "11"
                     MOVE "10"    TO SAKUJO2-MM
                     MOVE "31"    TO SAKUJO2-DD
                     COMPUTE SAKUJO2-DD = SAKUJO2-DD -  WK-HZKSA
               WHEN  "10"
                     MOVE "09"    TO SAKUJO2-MM
                     MOVE "30"    TO SAKUJO2-DD
                     COMPUTE SAKUJO2-DD = SAKUJO2-DD -  WK-HZKSA
               WHEN  "09"
                     MOVE "08"    TO SAKUJO2-MM
                     MOVE "31"    TO SAKUJO2-DD
                     COMPUTE SAKUJO2-DD = SAKUJO2-DD -  WK-HZKSA
               WHEN  "08"
                     MOVE "07"    TO SAKUJO2-MM
                     MOVE "31"    TO SAKUJO2-DD
                     COMPUTE SAKUJO2-DD = SAKUJO2-DD -  WK-HZKSA
               WHEN  "07"
                     MOVE "06"    TO SAKUJO2-MM
                     MOVE "30"    TO SAKUJO2-DD
                     COMPUTE SAKUJO2-DD = SAKUJO2-DD -  WK-HZKSA
               WHEN  "06"
                     MOVE "05"    TO SAKUJO2-MM
                     MOVE "31"    TO SAKUJO2-DD
                     COMPUTE SAKUJO2-DD = SAKUJO2-DD -  WK-HZKSA
               WHEN  "05"
                     MOVE "04"    TO SAKUJO2-MM
                     MOVE "30"    TO SAKUJO2-DD
                     COMPUTE SAKUJO2-DD = SAKUJO2-DD -  WK-HZKSA
               WHEN  "04"
                     MOVE "03"    TO SAKUJO2-MM
                     MOVE "31"    TO SAKUJO2-DD
                     COMPUTE SAKUJO2-DD = SAKUJO2-DD -  WK-HZKSA
               WHEN  "03"
                     MOVE "02"    TO SAKUJO2-MM
                     MOVE "28"    TO SAKUJO2-DD
                     COMPUTE SAKUJO2-DD = SAKUJO2-DD -  WK-HZKSA
               WHEN  "02"
                     MOVE "01"    TO SAKUJO2-MM
                     MOVE "31"    TO SAKUJO2-DD
                     COMPUTE SAKUJO2-DD = SAKUJO2-DD -  WK-HZKSA
               WHEN  "01"
                     MOVE "12"    TO SAKUJO2-MM
                     MOVE "31"    TO SAKUJO2-DD
                     COMPUTE SAKUJO2-DD = SAKUJO2-DD -  WK-HZKSA
                     COMPUTE SAKUJO2-YY = SAKUJO2-YY -  1
     END-EVALUATE
     END-IF.
*
**** DISPLAY JYO-F04 "  "  WK-TUKISUU  "  "
****         SAKUJO1  "  "  SAKUJO2   UPON  CONS.
*
 SAKUJOJOKEN-EXIT.
     EXIT.
****************************************************************
*             小売連携累積ファイル出力処理　異常
****************************************************************
 IJYOSHORI-SEC    SECTION.
*
     MOVE    "4000"    TO   PROGRAM-STATUS
     STOP   RUN.
 IJYOSHORI-EXIT.
     EXIT.
****************************************************************
*             小売連携累積ファイル削除　処理
****************************************************************
 SAKUJO-SEC       SECTION.
     MOVE   NAK-F01   TO    NAR-F01.
     START  NARRUIF   KEY  IS = NAR-F01
            INVALID
            MOVE    "1" TO  SAKUJO-FLG
            NOT INVALID
            READ     NARRUIF
            DELETE   NARRUIF
            ADD      1  TO  DLT-CNT1
      END-START.
*
*
 SAKUJO-EXIT.
        EXIT.

```
