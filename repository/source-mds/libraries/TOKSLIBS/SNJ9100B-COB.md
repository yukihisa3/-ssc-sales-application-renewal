# SNJ9100B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SNJ9100B.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　受配信管理システム　　　　　　　　*
*    業務名　　　　　　　：　ＣＶＣＳ管理                      *
*    モジュール名　　　　：　当日スケジュールＭ更新           *
*    作成日／更新日　　　：　2010/09/06                        *
*    作成者／更新者　　　：　ＮＡＶ阿部                        *
*    処理概要　　　　　　：　更新結果フラグをパラメータより受取*
*                            更新する。                        *
****************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SNJ9100B.
 AUTHOR.               ABE.
 DATE-WRITTEN.         2010/09/06.
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
     SELECT  JSMDAYF   ASSIGN    TO        DA-01-VI-JSMDAYL1
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
*    FILE = 当日スケジュールマスタ                             *
****************************************************************
 FD  JSMDAYF
                       LABEL     RECORD    IS   STANDARD.
                       COPY      JSMDAYF   OF   XFDLIB
                       JOINING   TJS       AS   PREFIX.
*
****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
*ステータス領域
 01  STATUS-AREA.
     03  TJS-ST                   PIC  X(02).
*時間編集領域
 01  WK-TIME.
     03  WK-TIME-HHMM             PIC  9(04)  VALUE  ZERO.
     03  WK-TIME-SS               PIC  9(04)  VALUE  ZERO.
*フラグ領域
 01  WK-FLG.
     03  JSMDAYF-INV-FLG          PIC  X(03)  VALUE  SPACE.
*ファイルエラーメッセージ
 01  FILE-ERR.
     03  TJS-ERR           PIC N(15) VALUE
         NC"当日スケジュールマスタエラー".
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
*入力パラメータ領域
 01  LINK-IN-JDATE          PIC  9(08). *> 日付
 01  LINK-IN-JTIME          PIC  9(04). *> 時刻
 01  LINK-IN-JTOKCD         PIC  9(08). *> 取引先コード
 01  LINK-IN-UPKBN          PIC  9(01). *> 更新区分
 01  LINK-IN-STACD          PIC  X(04). *> 処理結果コード
*
**************************************************************
 PROCEDURE             DIVISION  USING LINK-IN-JDATE
                                       LINK-IN-JTIME
                                       LINK-IN-JTOKCD
                                       LINK-IN-UPKBN
                                       LINK-IN-STACD.
**************************************************************
 DECLARATIVES.
 TJS-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE JSMDAYF.
     MOVE        TJS-ST    TO        E-ST.
     MOVE        "JSMDAYF" TO        E-FILE.
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
     OPEN      I-O     JSMDAYF.
*現在、時刻の取得
     ACCEPT    WK-TIME   FROM  TIME.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*             メイン処理                             1.0       *
****************************************************************
 MAIN-SEC              SECTION.
     MOVE  "MAIN-SEC"            TO  S-NAME.
*ファイルＫＥＹ項目セット
     MOVE  LINK-IN-JDATE         TO  TJS-F01.
     MOVE  LINK-IN-JTIME         TO  TJS-F02.
     MOVE  LINK-IN-JTOKCD        TO  TJS-F03.
*ファイル読込み
     READ  JSMDAYF
       INVALID
         MOVE  "INV"             TO  JSMDAYF-INV-FLG
       NOT INVALID
         MOVE  SPACE             TO  JSMDAYF-INV-FLG
     END-READ.
*処理区分により各項目更新
     IF  JSMDAYF-INV-FLG = "INV"
**     登録
         MOVE SPACE                TO  TJS-REC
         INITIALIZE  TJS-REC
         MOVE  LINK-IN-JDATE       TO  TJS-F01  *> 日付
         MOVE  LINK-IN-JTIME       TO  TJS-F02  *> 時間
         MOVE  LINK-IN-JTOKCD      TO  TJS-F03  *> 取引先コード

         MOVE  LINK-IN-UPKBN       TO  TJS-F13  *> 更新ﾌﾗｸﾞ
         MOVE  LINK-IN-STACD       TO  TJS-F15  *> 結果ＦＬＧ

         WRITE TJS-REC
     ELSE
**     更新
         MOVE  LINK-IN-UPKBN          TO  TJS-F13  *> 更新ﾌﾗｸﾞ
         MOVE  LINK-IN-STACD          TO  TJS-F15  *> 結果ＦＬＧ

         REWRITE TJS-REC
     END-IF.

 MAIN-EXIT.
     EXIT.
****************************************************************
*             終了処理                               3.0       *
****************************************************************
 END-SEC               SECTION.
     MOVE     "END-SEC"      TO   S-NAME.
*ファイルのＯＰＥＮ
     CLOSE     JSMDAYF.
*
 END-EXIT.
     EXIT.
*****************<<  SCV0060B   END PROGRAM  >>******************

```
