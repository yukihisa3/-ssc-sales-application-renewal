# SEG0030B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIB/SEG0030B.COB`

## ソースコード

```cobol
****************************************************************
*    （大阪営業所－全データ抽出バージョン）                    *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　受配信管理システム　　　　　　　　*
*    業務名　　　　　　　：　ＣＶＣＳ管理                      *
*    モジュール名　　　　：　当日スケジュールマスタ抽出        *
*    作成日／更新日　　　：　2000/03/28                        *
*    作成者／更新者　　　：　ＮＡＶ高橋                        *
*    処理概要　　　　　　：　パラメタより受け取ったバッチ_の　*
*                            当日スケジュールを抽出する。      *
****************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SEG0030B.
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
*当日スケジュールマスタ
     SELECT  JHMTJSF   ASSIGN    TO        DA-01-VI-JHMTJSL1
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      RANDOM
                       RECORD    KEY       TJS-F01
                                           TJS-F02
                                           TJS-F03
                       FILE      STATUS    TJS-ST.
*当日スケジュールマスタワーク
     SELECT  JHMTJSWK  ASSIGN    TO        DA-01-S-JHMTJSWK
                       FILE      STATUS    TJW-ST.
*
****************************************************************
 DATA                DIVISION.
****************************************************************
 FILE                SECTION.
****************************************************************
*    FILE = 当日スケジュールマスタ                             *
****************************************************************
 FD  JHMTJSF
                       LABEL     RECORD    IS   STANDARD.
                       COPY      JHMTJSF   OF   XFDLIB
                       JOINING   TJS       AS   PREFIX.
*
****************************************************************
*    FILE = 当日スケジュールマスタワーク                       *
****************************************************************
 FD  JHMTJSWK
                       LABEL     RECORD    IS   STANDARD.
                       COPY      JHMTJSF   OF   XFDLIB
                       JOINING   TJW       AS   PREFIX.
*
****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
*ステータス領域
 01  STATUS-AREA.
     03  TJS-ST                   PIC  X(02).
     03  TJW-ST                   PIC  X(02).
*時間編集領域
 01  WK-TIME.
     03  WK-TIME-HHMM             PIC  9(04)  VALUE  ZERO.
     03  WK-TIME-SS               PIC  9(04)  VALUE  ZERO.
*フラグ領域
 01  WK-FLG.
     03  JHMTJSF-INV-FLG          PIC  X(03)  VALUE  SPACE.
*ファイルエラーメッセージ
 01  FILE-ERR.
     03  TJS-ERR           PIC N(15) VALUE
         NC"当日スケジュールエラー".
     03  TJW-ERR           PIC N(15) VALUE
         NC"当日スケジュールＷエラー".
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
 LINKAGE              SECTION.
*------------------------------------------------------------*
*日付変換サブルーチン用ワーク
 01  LINK-IN-JDATE         PIC 9(08).
 01  LINK-IN-JTIME         PIC 9(04).
 01  LINK-IN-JTOKCD        PIC 9(08).
*
**************************************************************
 PROCEDURE             DIVISION   USING    LINK-IN-JDATE
                                           LINK-IN-JTIME
                                           LINK-IN-JTOKCD.
**************************************************************
 DECLARATIVES.
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
     OPEN      INPUT   JHMTJSF.
     OPEN      OUTPUT  JHMTJSWK.
*現在、時刻の取得
     ACCEPT    WK-TIME   FROM  TIME.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*             メイン処理                             1.0       *
****************************************************************
 MAIN-SEC              SECTION.
     MOVE     "MAIN-SEC"     TO   S-NAME.
*ファイルＫＥＹ項目セット
     MOVE      LINK-IN-JDATE TO   TJS-F01.
     MOVE      LINK-IN-JTIME TO   TJS-F02.
     MOVE      LINK-IN-JTOKCD TO  TJS-F03.
*ファイル読込み
     READ      JHMTJSF
               INVALID
                 MOVE  "INV"   TO   JHMTJSF-INV-FLG
               NOT  INVALID
                 MOVE  SPACE   TO   JHMTJSF-INV-FLG
     END-READ.
*処理区分により各項目更新
     IF   JHMTJSF-INV-FLG  =  "INV"
          MOVE SPACE         TO  TJW-REC
          INITIALIZE             TJW-REC
          MOVE LINK-IN-JDATE TO  TJW-F01
          MOVE LINK-IN-JTIME TO  TJW-F02
          MOVE LINK-IN-JTOKCD TO TJW-F03
          MOVE 04            TO  TJW-F11
          WRITE TJW-REC
     ELSE
          MOVE SPACE         TO  TJW-REC
          INITIALIZE             TJW-REC
          MOVE TJS-REC       TO  TJW-REC
          WRITE TJW-REC
     END-IF.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*             終了処理                               3.0       *
****************************************************************
 END-SEC               SECTION.
     MOVE     "END-SEC"      TO   S-NAME.
*ファイルのＯＰＥＮ
     CLOSE     JHMTJSF  JHMTJSWK.
*
 END-EXIT.
     EXIT.
*****************<<  SEG0030B   END PROGRAM  >>******************

```
