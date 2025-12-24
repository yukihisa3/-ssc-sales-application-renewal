# SJR9901B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/SJR9901B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　受領返品取込機能構築　　　　　　　*
*    業務名　　　　　　　：　分類（部門）コンバード　　        *
*    モジュール名　　　　：　累積Ｆ→返品Ｆ分類（部門）セット  *
*    作成日／更新日　　　：　2019/01/27                        *
*    作成者／更新者　　　：　ＮＡＶ高橋                        *
*    処理概要　　　　　　：　返品Ｆより累積Ｆを読み、分類（部　*
*                            門）をコンバートする。　　　　    *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SJR9901B.
 AUTHOR.                NAV TAKAHASHI.
 DATE-WRITTEN.          2019/01/27.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*受領累積データ
     SELECT   COMRUIF   ASSIGN    TO        DA-01-VI-COMRUIL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       JYR-F03   JYR-F01
                                            JYR-F02   JYR-F04
                                            JYR-F31
                        FILE  STATUS   IS   JYR-STATUS.
*受領返品データ
     SELECT   COMRHEF   ASSIGN    TO        DA-01-VI-COMRHEL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       JHR-F01   JHR-F03
                                            JHR-F02   JHR-F04
                                            JHR-F05
                        FILE  STATUS   IS   JHR-STATUS.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    受領累積データ
******************************************************************
 FD  COMRUIF            LABEL RECORD   IS   STANDARD.
     COPY     COMRUIF   OF        XFDLIB
              JOINING   JYR       PREFIX.
******************************************************************
*    受領返品データ
******************************************************************
 FD  COMRHEF            LABEL RECORD   IS   STANDARD.
     COPY     COMRHEF   OF        XFDLIB
              JOINING   JHR       PREFIX.
*
*****************************************************************
*
 WORKING-STORAGE        SECTION.
*    ｶｳﾝﾄ
 01  END-FG                  PIC  9(01)     VALUE  ZERO.
 01  IDX                     PIC  9(02)     VALUE  ZERO.
 01  RD-CNT                  PIC  9(08)     VALUE  ZERO.
 01  SK-CNT                  PIC  9(08)     VALUE  ZERO.
 01  WT-CNT                  PIC  9(08)     VALUE  ZERO.
*
 01  WK-DENKU                PIC  X(02)     VALUE  SPACE.
 01  COMRUIF-INV-FLG         PIC  X(03)     VALUE  SPACE.
*
 01  WK-AREA.
*システム日付の編集
     03  SYS-DATE          PIC 9(06).
     03  SYS-DATEW         PIC 9(08).
 01  WK-ST.
     03  JYR-STATUS        PIC  X(02).
     03  JHR-STATUS        PIC  X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SJR9901B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SJR9901B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SJR9901B".
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
*時刻
 01  TIME-AREA.
     03  WK-TIME                  PIC  9(08)  VALUE  ZERO.
*
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
*
 LINKAGE                SECTION.
 01  PARA-JTOKCD            PIC   9(08).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION USING PARA-JTOKCD.
 DECLARATIVES.
*
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   COMRUIF.
     MOVE      "COMRUIL1"   TO   AB-FILE.
     MOVE      JYR-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   COMRHEF.
     MOVE      "COMRHEL1"   TO   AB-FILE.
     MOVE      JHR-STATUS   TO   AB-STS.
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
     OPEN     INPUT     COMRUIF.
     OPEN     I-O       COMRHEF.
     DISPLAY  MSG-START UPON CONS.
*
     MOVE     ZERO      TO        END-FG    RD-CNT    WT-CNT.
     MOVE     ZERO      TO        SK-CNT.
*
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
*システム時間取得
     ACCEPT    WK-TIME          FROM   TIME.
*受領返品データスタート
     MOVE      SPACE            TO     JHR-REC.
     INITIALIZE                        JHR-REC.
     MOVE      PARA-JTOKCD      TO     JHR-F01.
     START  COMRHEF  KEY  IS  >=  JHR-F01  JHR-F03  JHR-F02
                                  JHR-F04  JHR-F05
            INVALID  MOVE   9   TO     END-FG
                     DISPLAY NC"＃＃対象データ無１！！"
                             UPON  CONS
                     GO         TO     INIT-EXIT
     END-START.
*受領返品データ読込
     PERFORM  COMRHEF-READ-SEC.
     IF  END-FG = 9
         DISPLAY NC"＃＃対象データ無２！！" UPON CONS
     END-IF.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"          TO   S-NAME.
*受領累積データ索引
     MOVE     JHR-F01            TO   JYR-F03.
     MOVE     JHR-F78            TO   JYR-F01.
     MOVE     JHR-F79            TO   JYR-F02.
     MOVE     JHR-F04            TO   JYR-F04.
     MOVE     JHR-F05            TO   JYR-F31.
     PERFORM  COMRUIF-READ-SEC.
     IF       COMRUIF-INV-FLG  =  "INV"
              ADD     1          TO   SK-CNT
              GO                 TO   MAIN-010
     END-IF.
*
     MOVE     JYR-F09            TO   JHR-F34.
     REWRITE  JHR-REC.
     ADD      1                  TO   WT-CNT.
*
 MAIN-010.
     PERFORM COMRHEF-READ-SEC.
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
     DISPLAY "READ-CNT    = "   RD-CNT  UPON  CONS.
     DISPLAY "SKIP-CNT    = "   SK-CNT  UPON  CONS.
     DISPLAY "REWRITE-CNT = "  WT-CNT  UPON  CONS.
*
     CLOSE     COMRUIF  COMRHEF.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
****************************************************************
*　　受領累積データ読込（存在チェック）
****************************************************************
 COMRUIF-READ-SEC        SECTION.
*
     MOVE   "COMRUIF-READ-SEC"  TO   S-NAME.
*
     READ  COMRUIF
           INVALID     MOVE  "INV"  TO  COMRUIF-INV-FLG
           NOT INVALID MOVE  SPACE  TO  COMRUIF-INV-FLG
     END-READ.
*
 COMRUIF-READ-EXIT.
     EXIT.
****************************************************************
*　　受領返品データ読込
****************************************************************
 COMRHEF-READ-SEC        SECTION.
*
     MOVE   "COMRHEF-READ-SEC"  TO   S-NAME.
*
 READ001.
     READ  COMRHEF
           AT  END     MOVE  9      TO  END-FG
                       GO           TO  COMRUIF-READ-EXIT
     END-READ.
*処理件数カウント／画面表示
     ADD   1                    TO   RD-CNT.
     IF  RD-CNT(6:3)  =  "000" OR "500"
         DISPLAY "READ-CNT = " RD-CNT UPON  CONS
     END-IF.
 READ002.
*バッチ番号チェック
     IF  PARA-JTOKCD =  JYR-F03
         CONTINUE
     ELSE
         MOVE  9                TO   END-FG
         GO                     TO   COMRHEF-READ-EXIT
     END-IF.
*
 COMRHEF-READ-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*

```
