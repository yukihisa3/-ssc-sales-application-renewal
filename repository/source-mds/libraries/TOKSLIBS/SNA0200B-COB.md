# SNA0200B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SNA0200B.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　ＨＧ苗業務連携                    *
*    業務名　　　　　　　：　                                  *
*    モジュール名　　　　：　連携NO存在チェック              *
*    作成日／更新日　　　：　11/10/25                          *
*    作成者／更新者　　　：　ＮＡＶ畠山　                      *
*    処理概要　　　　　　：　パラメータより連携NO存在チェック*
*                                                              *
****************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SNA0200B.
 AUTHOR.               HATAKEYAMA.
 DATE-WRITTEN.         11/10/25.
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
*小売連携発注データ
     SELECT  EXXXXXXA  ASSIGN TO   DA-01-S-EXXFIL
             ORGANIZAITION         SEQUENTIAL
                         FILE      STATUS    EXX-ST.
*
****************************************************************
 DATA                DIVISION.
*************************************************************
 FILE                SECTION.
****************************************************************
*    FILE = 小売連携発注データ                                *
****************************************************************
 FD  EXXXXXXA          BLOCK     CONTAINS  1    RECORDS
                       LABEL     RECORD    IS   STANDARD.
                       COPY      EXXXXXXA  OF   XFDLIB
                       JOINING   EXX       AS   PREFIX.
*
****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
 01  END-FLG                   PIC  X(01)     VALUE   SPACE.
 01  ITCHI-FLG                 PIC  X(01)     VALUE   SPACE.
*ステータス領域
 01  STATUS-AREA.
     03  EXX-ST                PIC  X(02).
*ファイルエラーメッセージ
 01  FILE-ERR.
     03  EXX-ERR           PIC N(12) VALUE
         NC"小売連携発注データエラー".
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
*----- 連携NOが送信ファイルに存在しないメッセージ
*
     03  MSG-KENSU1.
         05  KENSU1A PIC N(12) VALUE
         NC"＊＊＊＊＊＊＊＊＊＊＊＊".
         05  KENSU1B PIC N(12) VALUE
         NC"＊＊＊＊＊＊＊＊＊＊＊＊".
     03  MSG-KENSU2.
         05  KENSU2A PIC N(24) VALUE
       NC"＊　以下の連携ＮＯは送信ファイルに存在しません＊".
     03  MSG-KENSU3.
         05  KENSU3A PIC N(10) VALUE NC"＊　　　　　　　　　".
         05  RENKEI-NO-FROM PIC X(09) VALUE SPACE.
         05  KENSU3B PIC N(10) VALUE NC"　　　　　　　　　＊".
*-----
     03  MSG-KENSU4.
         05  KENSU4A PIC N(24) VALUE
       NC"＊　以下の連携ＮＯの存在を確認しました　　　　＊".
 LINKAGE              SECTION.
* パラメーター（B) エリア
 01  PARA-BT.
   03  PARA-BUMON                 PIC  X(04).
   03  PARA-TANTOU                PIC  X(02).
*01  PARA-BUMON                   PIC  X(04).
*01  PARA-TANTOU                  PIC  X(02).
 01  PARA-SHUBETU                 PIC  X(01).
 01  PARA-RENKEI-NO-FROM          PIC  X(09).
 01  PARA-RENKEI-NO-TO            PIC  X(09).
 01  PARA-TANMATU-IP              PIC  X(15).
*------------------------------------------------------------*
*
**************************************************************
 PROCEDURE             DIVISION  USING  PARA-BT
*                                       PARA-TANTOU
                                        PARA-SHUBETU
                                        PARA-RENKEI-NO-FROM
                                        PARA-RENKEI-NO-TO
                                        PARA-TANMATU-IP.
**************************************************************
 DECLARATIVES.
 FILEERR-SEC1             SECTION.
     USE         AFTER     EXCEPTION PROCEDURE EXXXXXXA.
     MOVE        EXX-ST      TO       E-ST.
     MOVE        "EXXXXXXA"  TO       E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     EXX-ERR   UPON      CONS.
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
               UNTIL   END-FLG =  "1".
*
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
     OPEN      INPUT        EXXXXXXA.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*             メイン処理                             1.0       *
****************************************************************
 MAIN-SEC              SECTION.
     MOVE     "MAIN-SEC"     TO   S-NAME.
*小売連携発注データの読み込み
 MAIN010.
     READ  EXXXXXXA
           AT   END
     PERFORM DSP-TAISHOUNASI-SEC THRU DSP-TAISHOUNASI-EXIT
           MOVE   "1"        TO   END-FLG
           GO                TO   MAIN-EXIT
     END-READ.
 MAIN011.
     IF  EXX-F01   =  PARA-RENKEI-NO-FROM
         PERFORM DSP-TAISHOUARI-SEC THRU DSP-TAISHOUARI-EXIT
     END-IF.
 MAIN-EXIT.
     EXIT.
****************************************************************
*             終了処理                               3.0       *
****************************************************************
 END-SEC               SECTION.
*
     MOVE     "END-SEC"      TO   S-NAME.
*ファイルのCLOSE処理
     CLOSE     EXXXXXXA.
*
 END-EXIT.
     EXIT.
**************************************************************
*        小売連携発注データがゼロ件
**************************************************************
 DSP-TAISHOUNASI-SEC SECTION.
*
      IF       ITCHI-FLG  = "1"
               GO   TO  DSP-TAISHOUNASI-EXIT.
*
      MOVE      PARA-RENKEI-NO-FROM   TO  RENKEI-NO-FROM.
      DISPLAY   KENSU1A KENSU1B     UPON  CONS.
      DISPLAY   KENSU2A             UPON  CONS.
      DISPLAY   KENSU3A RENKEI-NO-FROM KENSU3B  UPON  CONS.
      DISPLAY   KENSU1A KENSU1B     UPON  CONS.
      MOVE    "4002"    TO  PROGRAM-STATUS.
      MOVE    "1"       TO  END-FLG.
 DSP-TAISHOUNASI-EXIT.
     EXIT.
**************************************************************
*        パラメーターＢの連携NOと一致
**************************************************************
 DSP-TAISHOUARI-SEC SECTION.
*
      MOVE      PARA-RENKEI-NO-FROM   TO  RENKEI-NO-FROM.
      DISPLAY   KENSU1A KENSU1B     UPON  CONS.
      DISPLAY   KENSU4A             UPON  CONS.
      DISPLAY   KENSU3A RENKEI-NO-FROM KENSU3B  UPON  CONS.
      DISPLAY   KENSU1A KENSU1B     UPON  CONS.
      MOVE      "1"       TO          ITCHI-FLG.
      MOVE      "4001"    TO  PROGRAM-STATUS.
      MOVE      "1"       TO  END-FLG.
 DSP-TAISHOUARI-EXIT.
     EXIT.

```
