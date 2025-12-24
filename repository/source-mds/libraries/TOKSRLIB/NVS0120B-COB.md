# NVS0120B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/NVS0120B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　Ｄ３６５連携　　　　　　　　　　　*
*    モジュール名　　　　：　D365送受信制御　　　　　　　　　
*    作成日／作成者　　　：　2020/02/16   ASS.II               *
*    処理内容　　　　　　：　Ｄ３６５送受信制御れレコードより　*
*    　　　　　　　　　　　　Ｄ３６５送受信実行制御を実行する。*
****************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           NVS0120B.
 AUTHOR.               ABE.
 DATE-WRITTEN.         20/02/16.
****************************************************************
 ENVIRONMENT           DIVISION.
****************************************************************
 CONFIGURATION         SECTION.
 SPECIAL-NAMES.
     CONSOLE      IS   CONS.
*
 INPUT-OUTPUT          SECTION.
 FILE-CONTROL.
*D365送受信実行制御ファイル
     SELECT  D365RUNF  ASSIGN    TO        DA-01-S-D365RUNF
                       ORGANIZATION        SEQUENTIAL
                       ACCESS    MODE      SEQUENTIAL
                       FILE      STATUS    RUN-ST.
*
****************************************************************
 DATA                DIVISION.
****************************************************************
 FILE                SECTION.
****************************************************************
*    FILE = D365送受信実行制御ファイル                         *
****************************************************************
 FD  D365RUNF
                       LABEL     RECORD    IS   STANDARD.
                       COPY      D365RUNF  OF   XFDLIB
                       JOINING   RUN       AS   PREFIX.
****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
*ステータス領域
 01  STATUS-AREA.
     03  RUN-ST                   PIC  X(02).
*区分、ＦＬＧエリア
 01  FLG-AREA.
     03  END-FLG                  PIC  X(03)  VALUE  SPACE.
     03  GO-FLG                   PIC  X(02)  VALUE  SPACE.
*システム日付格納
 01  SYS-DATE                     PIC  9(06)  VALUE  ZERO.
*システム時間格納
 01  WK-TIME.
     03  WK-TIME-1                PIC  9(04)  VALUE  ZERO.
     03  WK-TIME-2                PIC  9(04)  VALUE  ZERO.
*時間編集領域
 01  WK-DATE.
     03  WK-YYYY                  PIC  9(04)  VALUE  ZERO.
     03  WK-MM                    PIC  9(02)  VALUE  ZERO.
     03  WK-DD                    PIC  9(02)  VALUE  ZERO.
*ファイルエラーメッセージ
 01  FILE-ERR.
     03  RUN-ERR           PIC N(15) VALUE
         NC"Ｄ３６５送受信実行制御エラー".
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
*受信パスワード編集エリア
 01  WK-PASS.
     03  PASS-YOKYU               PIC  X(02).
     03  PASS-ID                  PIC  X(02).
     03  PASS-CENTER              PIC  X(06).
     03  PASS-DATA                PIC  X(02).
     03  PASS-SIKIBETU            PIC  X(01).
 01  WK-KEKA                      PIC  X(01).
*受信モード
 01  WK-JYUKBN                    PIC  X(01)  VALUE  "1".
*伝票更新起動チェック用
 01  LINK-CHK                     PIC  X(01)  VALUE  SPACE.
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
*
*------------------------------------------------------------*
 LINKAGE              SECTION.
*------------------------------------------------------------*
*処理結果
 01  LINK-KEKA             PIC X(01).
*結果区分
 01  LINK-KBN              PIC X(04).
**************************************************************
 PROCEDURE             DIVISION   USING   LINK-KEKA  LINK-KBN.
**************************************************************
 DECLARATIVES.
 RUN-ERR-SEC               SECTION.
     USE         AFTER     EXCEPTION PROCEDURE D365RUNF.
     MOVE        RUN-ST    TO        E-ST.
     MOVE        "D365RUNF" TO       E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     RUN-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 END  DECLARATIVES.
****************************************************************
*             MAIN        MODULE                     0.0       *
****************************************************************
 PROCESS-START         SECTION.
     MOVE     "PROCESS-START"     TO   S-NAME.
*    システム日付／時刻取得
     PERFORM   INIT-SEC.
     PERFORM   MAIN-SEC    UNTIL   END-FLG  =  "END".
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
     OPEN      INPUT   D365RUNF.
*Ｄ３６５送受信実行制御読込み
     PERFORM   D365RUNF-READ-SEC.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*             メイン処理                             1.0       *
****************************************************************
 MAIN-SEC              SECTION.
     MOVE     "MAIN-SEC"     TO   S-NAME.
*D365送受信実行起動
     CALL "D365SEG2"   USING RUN-F01 RUN-F02 RUN-F03 RUN-F04
                             RUN-F05 RUN-F06 RUN-F07 RUN-F08
                             RUN-F09 RUN-F10 RUN-F11 RUN-F12
                             RUN-F13 RUN-F14.
**                           LINK-KBN.
****************************
     DISPLAY "CNVSSBM 起動"     UPON CONS.
     DISPLAY "RUN-F01="  RUN-F01 UPON CONS.
     DISPLAY "RUN-F02="  RUN-F02 UPON CONS.
     DISPLAY "RUN-F03="  RUN-F03 UPON CONS.
     DISPLAY "RUN-F04="  RUN-F04 UPON CONS.
     DISPLAY "RUN-F05="  RUN-F05 UPON CONS.
     DISPLAY "RUN-F06="  RUN-F06 UPON CONS.
     DISPLAY "RUN-F07="  RUN-F07 UPON CONS.
     DISPLAY "RUN-F08="  RUN-F08 UPON CONS.
     DISPLAY "RUN-F09="  RUN-F09 UPON CONS.
     DISPLAY "RUN-F10="  RUN-F10 UPON CONS.
     DISPLAY "RUN-F11="  RUN-F11 UPON CONS.
     DISPLAY "RUN-F12="  RUN-F12 UPON CONS.
     DISPLAY "RUN-F13="  RUN-F13 UPON CONS.
     DISPLAY "RUN-F14="  RUN-F14 UPON CONS.
     DISPLAY "LINK-KBN=" LINK-KBN UPON CONS.
****************************
     IF         LINK-KBN      =    "0000"
                MOVE  SPACE   TO    WK-KEKA
     ELSE
                MOVE  "1"     TO    WK-KEKA
     END-IF.
*
*Ｄ３６５送受信実行制御読込み
     PERFORM   D365RUNF-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*             終了処理                               3.0       *
****************************************************************
 END-SEC               SECTION.
     MOVE     "END-SEC"            TO   S-NAME.
*ファイルのＣＬＯＳＥ
     CLOSE     D365RUNF.
     MOVE    WK-KEKA               TO   LINK-KEKA.
*
****************************
     DISPLAY "処理結果=" WK-KEKA   UPON CONS.
     DISPLAY "結果区分=" LINK-KBN  UPON CONS.
****************************
 END-EXIT.
     EXIT.
****************************************************************
*             Ｄ３６５送受信実行制御読込み           2.1       *
****************************************************************
 D365RUNF-READ-SEC     SECTION.
     MOVE     "JD365RUNF-READ-SEC" TO   S-NAME.
*当日送受信制御読込み
     READ      D365RUNF  AT   END
               MOVE     "END"      TO   END-FLG
               GO                  TO   D365RUNF-READ-EXIT
     END-READ.
*
 D365RUNF-READ-EXIT.
     EXIT.
*****************<<  NVS0120B   END PROGRAM  >>******************

```
