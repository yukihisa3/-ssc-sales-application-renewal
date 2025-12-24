# SNA0250B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SNA0250B.COB`

## ソースコード

```cobol
**************************************************************
*                                                            *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　*
*    サブシステム　　　　：　ＨＧ苗業務連携                  *
*    業務名　　　　　　　：　                                *
*    モジュール名　　　　：　HULFT削除期間日付取得           *
*    作成日／更新日　　　：　12/04/12                        *
*    作成者／更新者　　　：　ＮＡＶ三浦　                    *
*    処理概要　　　　　　：　HULFT履歴ファイルの削除期間     *
*                            日付を設定する。　　　　　　    *
*                                                            *
**************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SNA0250B.
 AUTHOR.               INO.
 DATE-WRITTEN.         12/04/12.
****************************************************************
 ENVIRONMENT           DIVISION.
****************************************************************
 CONFIGURATION         SECTION.
 SPECIAL-NAMES.
     CONSOLE      IS   CONS.
*
 INPUT-OUTPUT          SECTION.
 FILE-CONTROL.
*
*日付曜日ファイル
     SELECT  HIDUKEL1 ASSIGN TO    DA-01-VI-HIDUKEL1
             ORGANIZAITION         INDEXED
             ACCESS      MODE      SEQUENTIAL
             RECORD      KEY       HIZ-F01
                         FILE      STATUS    HIZ-ST.
*削除条件ファイル　
     SELECT  HLLOGW   ASSIGN TO    DA-01-S-HLLOGW
             ORGANIZATION          IS   SEQUENTIAL
             ACCESS      MODE      SEQUENTIAL
                         FILE      STATUS    DLT-ST.
*
****************************************************************
 DATA                DIVISION.
*************************************************************
 FILE                SECTION.
****************************************************************
*    FILE = 日付曜日ファイル　　　　　　　　　             *
****************************************************************
 FD  HIDUKEL1          BLOCK     CONTAINS  1    RECORDS
                       LABEL     RECORD    IS   STANDARD.
                       COPY      HIDUKEL1  OF   XFDLIB
                       JOINING   HIZ       AS   PREFIX.
****************************************************************
*    FILE = 削除条件ファイル  　　　　　　　　             *
****************************************************************
 FD  HLLOGW            BLOCK     CONTAINS  1    RECORDS
                       LABEL     RECORD    IS   STANDARD.
                       COPY      HLLOGW    OF   XFDLIB
                       JOINING   DLT       AS   PREFIX.
*
****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
 01  END-FLG                   PIC  X(01)     VALUE   SPACE.
 01  RWT-FLG                   PIC  X(01)     VALUE   SPACE.
 01  READ-CNT                  PIC  9(01)     VALUE   ZERO.
 01  WK-HIDUKE                 PIC  9(08)     VALUE   ZERO.
 01  WK-YOBI                   PIC  9(01)     VALUE   ZERO.
 01  HZK.
     03  YMD01                 PIC  9(02)     VALUE   20.
     03  YMD02                 PIC  9(06)     VALUE   ZERO.
*ステータス領域
 01  STATUS-AREA.
     03  HIZ-ST                PIC  X(02).
     03  DLT-ST                PIC  X(02).
*ファイルエラーメッセージ
 01  FILE-ERR.
     03  HIZ-ERR           PIC N(13) VALUE
         NC"日付曜日ファイル　　エラー".
     03  DLT-ERR           PIC N(13) VALUE
         NC"削除条件ファイル　エラー".
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
**************************************************************
 PROCEDURE             DIVISION.
**************************************************************
 DECLARATIVES.
 FILEERR-SEC1              SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HIDUKEL1.
     MOVE        HIZ-ST     TO       E-ST.
     MOVE       "HIDUKEL1"  TO       E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     HIZ-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 FILEERR-SEC2             SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HLLOGW.
     MOVE        DLT-ST     TO       E-ST.
     MOVE       "HLLOGW  "  TO       E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     DLT-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 END  DECLARATIVES.
****************************************************************
*             MAIN        MODULE                     0.0       *
****************************************************************
 PROCESS-START         SECTION.
     MOVE     "PROCESS-START"     TO   S-NAME.
     PERFORM   INIT-SEC.
     PERFORM   MAIN-SEC.
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
     OPEN      INPUT        HIDUKEL1.
     OPEN      I-O          HLLOGW.
     ACCEPT    YMD02        FROM   DATE.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*             メイン処理                             1.0       *
****************************************************************
 MAIN-SEC              SECTION.
     MOVE     "MAIN-SEC"     TO   S-NAME.
*-------日付曜日ファイルの読み込み
 MAIN010.
     MOVE  SPACE     TO        HIZ-REC.
     INITIALIZE                HIZ-REC.
     MOVE  HZK       TO        HIZ-F01.
     START HIDUKEL1  KEY IS  <=  HIZ-F01   WITH REVERSED
           INVALID      KEY
                GO TO MAIN-EXIT
     END-START.
     PERFORM HIZ-READ-SEC.
     PERFORM HIZ-READ2-SEC.
     IF   RWT-FLG = "1"
          PERFORM DLT-REWITE-SEC
     END-IF.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*      日付曜日Ｆ読込み                                        *
****************************************************************
 HIZ-READ-SEC                SECTION.
*
     READ    HIDUKEL1
             AT  END
             MOVE   "1"        TO   END-FLG
             GO                TO   HIZ-READ-EXIT
             NOT AT END
             MOVE    HIZ-F02   TO   WK-YOBI
             ADD     1         TO   READ-CNT
     END-READ.
*
 HIZ-READ-EXIT.
     EXIT.
****************************************************************
*      日付曜日Ｆ逆読込み
****************************************************************
 HIZ-READ2-SEC                SECTION.
*
     READ    HIDUKEL1  NEXT
             AT  END
             MOVE   "1"        TO   END-FLG
             GO                TO   HIZ-READ2-EXIT
             NOT AT END
             ADD     1         TO   READ-CNT
     END-READ.
*曜日判定
     IF   READ-CNT > 1  AND  WK-YOBI = HIZ-F02
          MOVE   "1"        TO   RWT-FLG
          GO                TO   HIZ-READ2-EXIT
     ELSE
          IF   READ-CNT > 7
               MOVE   "1"        TO   END-FLG
               GO                TO   HIZ-READ2-EXIT
          END-IF
     END-IF.
     GO                TO   HIZ-READ2-SEC
*
 HIZ-READ2-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    削除条件Ｆ更新  　　　　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DLT-REWITE-SEC         SECTION.
     READ     HLLOGW   AT   END
              MOVE     "1"        TO   END-FLG
              GO   TO   DLT-REWITE-EXIT
     END-READ.
     MOVE     HIZ-F01     TO   DLT-DSLTO.
     REWRITE  DLT-REC.
 DLT-REWITE-EXIT.
     EXIT.
**************************************************************
*             終了処理                               3.0       *
****************************************************************
 END-SEC               SECTION.
*
     MOVE     "END-SEC"      TO   S-NAME.
*
*ファイルのCLOSE処理
     CLOSE     HIDUKEL1   HLLOGW.
*
 END-EXIT.
     EXIT.

```
