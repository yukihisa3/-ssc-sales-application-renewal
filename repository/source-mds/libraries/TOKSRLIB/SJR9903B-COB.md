# SJR9903B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/SJR9903B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　受領返品取込機能（リカバリ）　　　*
*    業務名　　　　　　　：　マキバ相手商品ＣＤ不要０削除      *
*    モジュール名　　　　：　マキバ相手商品ＣＤ不要０削除      *
*    作成日／更新日　　　：　2021/01/18                        *
*    作成者／更新者　　　：　ＮＡＶ高橋                        *
*    処理概要　　　　　　：　受領累積データより、返品データを　*
*                            受領返品累積データの抽出する。    *
*    更新日／更新者　　　：　　　　　　　　　　　　　　　　　　*
*    修正概要　　　　　　：　　　　　　　　　　　　　　　　　　*
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SJR9903B.
 AUTHOR.                NAV TAKAHASHI.
 DATE-WRITTEN.          2017/08/28.
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
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       JYR-F03   JYR-F01
                                            JYR-F02   JYR-F04
                                            JYR-F31
                        FILE  STATUS   IS   JYR-STATUS.
*受領返品データ
     SELECT   COMRHEF   ASSIGN    TO        DA-01-VI-COMRHEL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
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
 01  WRT-CNT                 PIC  9(08)     VALUE  ZERO.
 01  NG-CNT                  PIC  9(08)     VALUE  ZERO.
*
 01  WK-DENKU                PIC  X(02)     VALUE  SPACE.
 01  COMRHEF-INV-FLG         PIC  X(03)     VALUE  SPACE.
 01  HSHOTBL-INV-FLG         PIC  X(03)     VALUE  SPACE.
 01  DENHENF-INV-FLG         PIC  X(03)     VALUE  SPACE.
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
         05  ST-PG          PIC   X(08)  VALUE "SJR9903B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SJR9903B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SJR9903B".
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
     03  MSG-NG.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(09)  VALUE " NG-CNT= ".
         05  CNT-NG         PIC   9(06).
         05  FILLER         PIC   X(05)  VALUE " *** ".
*時刻
 01  TIME-AREA.
     03  WK-TIME                  PIC  9(08)  VALUE  ZERO.
*相手商品ＣＤ編集
 01  WK-AITE-JANCD.
     03  WK-AITE-JANCD-1          PIC  X(01)  VALUE  SPACE.
     03  WK-AITE-JANCD-2          PIC  X(13)  VALUE  SPACE.
*
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
*
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION.
 DECLARATIVES.
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
     OPEN     I-O       COMRUIF.
     OPEN     I-O       COMRHEF.
     DISPLAY  MSG-START UPON CONS.
*
     MOVE     ZERO      TO        END-FG    RD-CNT    WRT-CNT.
     MOVE     ZERO      TO        IN-CNT    OUT-CNT.
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
*受領累積データスタート
     MOVE      SPACE            TO     JYR-REC.
     INITIALIZE                        JYR-REC.
     MOVE      275              TO     JYR-F03.
     START  COMRUIF  KEY  IS  >=  JYR-F03  JYR-F01  JYR-F02
                                  JYR-F04  JYR-F31
            INVALID  MOVE   9   TO     END-FG
                     DISPLAY NC"＃＃対象データ無１！！"
                             UPON  CONS
                     GO         TO     INIT-EXIT
     END-START.
*受領累積データ読込
     PERFORM  COMRUIF-READ-SEC.
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
*返品累積データ作成
     PERFORM  EDIT-SEC.
*
     PERFORM COMRUIF-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*　　　　　　　ファイル出力　　　　　　　　　　　　　　　　　　*
****************************************************************
 EDIT-SEC              SECTION.
*
     MOVE    "EDIT-SEC"     TO        S-NAME.
*
*    取引先ＣＤ
     MOVE     JYR-F03       TO        JHR-F01.
*    店舗ＣＤ
     MOVE     JYR-F07       TO        JHR-F02.
*    検収日
     MOVE     JYR-F08       TO        JHR-F03.
*    伝票番号
     MOVE     JYR-F04       TO        JHR-F04.
*    行番号
     MOVE     JYR-F31       TO        JHR-F05.
*
     PERFORM COMRHEF-READ-SEC.
     IF  COMRHEF-INV-FLG  =  "INV"
         ADD      1         TO        NG-CNT
     ELSE
*        ＪＡＮＣＤセット
         MOVE WK-AITE-JANCD-2   TO    JYR-F33  JHR-F09
         REWRITE  JYR-REC
         REWRITE  JHR-REC
         ADD      1         TO        WRT-CNT
     END-IF.
*
 EDIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
*
     MOVE      RD-CNT    TO      IN-CNT.
     MOVE      WRT-CNT   TO      OUT-CNT.
     MOVE      NG-CNT    TO      CNT-NG.
     DISPLAY   MSG-IN    UPON CONS.
     DISPLAY   MSG-OUT   UPON CONS.
     DISPLAY   MSG-NG    UPON CONS.
     DISPLAY   MSG-END   UPON CONS.
*
     CLOSE     COMRUIF  COMRHEF.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
****************************************************************
*　　返品累積データ読込（存在チェック）
****************************************************************
 COMRHEF-READ-SEC        SECTION.
*
     MOVE   "COMRUIF-READ-SEC"  TO   S-NAME.
*
     READ  COMRHEF
           INVALID     MOVE  "INV"  TO  COMRHEF-INV-FLG
           NOT INVALID MOVE  SPACE  TO  COMRHEF-INV-FLG
     END-READ.
*
 COMRHEF-READ-EXIT.
     EXIT.
****************************************************************
*　　受領累積データ読込
****************************************************************
 COMRUIF-READ-SEC        SECTION.
*
     MOVE   "COMRUIF-READ-SEC"  TO   S-NAME.
*
 READ001.
     READ  COMRUIF
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
     IF  JYR-F03 = 275
         CONTINUE
     ELSE
         MOVE  9                TO   END-FG
         GO                     TO   COMRUIF-READ-EXIT
     END-IF.
*相手商品ＣＤチェック
     IF   JYR-F33(1:1) = "0"
          MOVE  JYR-F33         TO   WK-AITE-JANCD
     ELSE
          ADD   1               TO   NG-CNT
          GO                    TO   COMRUIF-READ-SEC
     END-IF.
*
 COMRUIF-READ-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*

```
