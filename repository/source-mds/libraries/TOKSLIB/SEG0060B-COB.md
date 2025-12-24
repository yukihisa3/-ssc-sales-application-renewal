# SEG0060B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIB/SEG0060B.COB`

## ソースコード

```cobol
****************************************************************
*    （大阪営業所－全データ抽出バージョン）                    *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　受配信管理システム　　　　　　　　*
*    業務名　　　　　　　：　ＣＶＣＳ管理                      *
*    モジュール名　　　　：　当日スケジュールマスタ更新        *
*    作成日／更新日　　　：　2000/03/28                        *
*    作成者／更新者　　　：　ＮＡＶ高橋                        *
*    処理概要　　　　　　：　受信した当日スケジュールマスタを　*
*                            営業所当日スケジュールへ更新する。*
****************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SEG0060B.
 AUTHOR.               NAV.
 DATE-WRITTEN.         00/03/28.
****************************************************************
 ENVIRONMENT           DIVISION.
****************************************************************
 CONFIGURATION         SECTION.
 SPECIAL-NAMES.
     CONSOLE      IS   CONS.
*
 INPUT-OUTPUT          SECTION.
 FILE-CONTROL.
*当日スケジュールマスタワーク
     SELECT  JHMTJSWK  ASSIGN    TO        DA-01-S-JHMTJSWK
                       FILE      STATUS    TJW-ST.
*当日スケジュールマスタ
     SELECT  JHMTJSF   ASSIGN    TO        DA-01-VI-JHMTJSL1
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      RANDOM
                       RECORD    KEY       TJS-F01
                                           TJS-F02
                                           TJS-F03
                       FILE      STATUS    TJS-ST.
*
****************************************************************
 DATA                DIVISION.
****************************************************************
 FILE                SECTION.
****************************************************************
*    FILE = 当日スケジュールマスタワーク                       *
****************************************************************
 FD  JHMTJSWK
                       LABEL     RECORD    IS   STANDARD.
                       COPY      JHMTJSF   OF   XFDLIB
                       JOINING   TJW       AS   PREFIX.
*
****************************************************************
*    FILE = 当日スケジュールマスタ                             *
****************************************************************
 FD  JHMTJSF
                       LABEL     RECORD    IS   STANDARD.
                       COPY      JHMTJSF   OF   XFDLIB
                       JOINING   TJS       AS   PREFIX.
*
****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
*ステータス領域
 01  STATUS-AREA.
     03  TJW-ST                   PIC  X(02).
     03  TJS-ST                   PIC  X(02).
*時間編集領域
 01  WK-TIME.
     03  WK-TIME-HHMM             PIC  9(04)  VALUE  ZERO.
     03  WK-TIME-SS               PIC  9(04)  VALUE  ZERO.
*フラグ領域
 01  WK-FLG.
     03  END-FLG                  PIC  X(03)  VALUE  SPACE.
     03  JHMTJSF-INV-FLG          PIC  X(03)  VALUE  SPACE.
*ファイルエラーメッセージ
 01  FILE-ERR.
     03  TJW-ERR           PIC N(15) VALUE
         NC"当日スケジュールＷエラー".
     03  TJS-ERR           PIC N(15) VALUE
         NC"当日スケジュールエラー".
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
*
**************************************************************
 PROCEDURE             DIVISION.
**************************************************************
 DECLARATIVES.
 TJW-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE JHMTJSWK.
     MOVE        TJW-ST    TO        E-ST.
     MOVE        "JHMTJSWK" TO        E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     TJW-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 TJS-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE JHMTJSF.
     MOVE        TJS-ST    TO        E-ST.
     MOVE        "JHMTJSF" TO        E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     TJS-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 END  DECLARATIVES.
****************************************************************
*             MAIN        MODULE                     0.0       *
****************************************************************
 PROCESS-START         SECTION.
     MOVE     "PROCESS-START"     TO   S-NAME.
     PERFORM   INIT-SEC.
     PERFORM   MAIN-SEC  UNTIL  END-FLG = "END".
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
     OPEN      INPUT   JHMTJSWK.
     OPEN      I-O     JHMTJSF.
*受信当日スケジュールデータ読込み
     READ      JHMTJSWK  AT  END
               MOVE    "END"    TO   END-FLG
     END-READ.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*             メイン処理                             1.0       *
****************************************************************
 MAIN-SEC              SECTION.
     MOVE     "MAIN-SEC"     TO   S-NAME.
*ファイルＫＥＹ項目セット
     MOVE      TJW-F01       TO   TJS-F01.
     MOVE      TJW-F02       TO   TJS-F02.
     MOVE      TJW-F03       TO   TJS-F03.
*ファイル読込み
     READ      JHMTJSF
               INVALID
                 MOVE  "INV"   TO   JHMTJSF-INV-FLG
               NOT  INVALID
                 MOVE  SPACE   TO   JHMTJSF-INV-FLG
     END-READ.
*処理区分により各項目更新
     IF   JHMTJSF-INV-FLG  =  "INV"
          MOVE SPACE         TO  TJS-REC
          INITIALIZE             TJS-REC
          MOVE TJW-REC       TO  TJS-REC
          WRITE TJS-REC
     ELSE
          MOVE TJW-F04       TO  TJS-F04
          MOVE TJW-F05       TO  TJS-F05
          MOVE TJW-F06       TO  TJS-F06
          MOVE TJW-F07       TO  TJS-F07
          MOVE TJW-F08       TO  TJS-F08
          MOVE TJW-F09       TO  TJS-F09
          MOVE TJW-F10       TO  TJS-F10
          MOVE TJW-F11       TO  TJS-F11
          REWRITE TJS-REC
     END-IF.
*
     MOVE     "END"          TO  END-FLG.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*             終了処理                               3.0       *
****************************************************************
 END-SEC               SECTION.
     MOVE     "END-SEC"      TO   S-NAME.
*ファイルのＯＰＥＮ
     CLOSE     JHMTJSWK JHMTJSF.
*
 END-EXIT.
     EXIT.
*****************<<  SEG0060B   END PROGRAM  >>******************

```
