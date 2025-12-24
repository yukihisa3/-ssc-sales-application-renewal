# NVS0110B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/NVS0110B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　Ｄ３６５連携　　　　　　　　　　　*
*    モジュール名　　　　：　D365当日送受信制御ﾌｧｲﾙ情報更新
*    作成日／作成者　　　：　2020/02/24   ASS.II               *
*    処理内容　　　　　　：　Ｄ３６５当日送受信制御ファイルに　*
*    　　　　　　　　　　　　実行状況を更新する。　　　　　　　*
*                            リストWKを作成する。　　　　　　
****************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           NVS0110B.
 AUTHOR.               ASS.II.
 DATE-WRITTEN.         2020/02/24.
****************************************************************
 ENVIRONMENT           DIVISION.
****************************************************************
 CONFIGURATION         SECTION.
 SPECIAL-NAMES.
     CONSOLE      IS   CONS.
*
 INPUT-OUTPUT          SECTION.
 FILE-CONTROL.
*D365当日送受信制御ファイル
     SELECT  D365DAYF  ASSIGN    TO        DA-01-VI-D365DAY1
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      RANDOM
                       RECORD    KEY       TJS-F01
                                           TJS-F02
                       FILE      STATUS    TJS-ST.
*
****************************************************************
 DATA                DIVISION.
****************************************************************
 FILE                SECTION.
****************************************************************
*    FILE = D365当日送受信制御ファイル                         *
****************************************************************
 FD  D365DAYF
                       LABEL     RECORD    IS   STANDARD.
                       COPY      D365DAYF  OF   XFDLIB
                       JOINING   TJS       AS   PREFIX.
*
****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
*ステータス領域
 01  STATUS-AREA.
     03  TJS-ST                   PIC  X(02).
*フラグ領域
 01  WK-FLG.
     03  JSMDAYF-INV-FLG          PIC  X(03)  VALUE  SPACE.
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
     03  TJS-ERR           PIC N(15) VALUE
         NC"当日送受信制御ファイル".
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
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
*------------------------------------------------------------*
 LINKAGE              SECTION.
*------------------------------------------------------------*
*入力パラメータ領域
 01  LINK-IN-JDATE          PIC  9(08). *> 日付
 01  LINK-IN-JTIME          PIC  9(06). *> 時刻
 01  LINK-IN-UPDKBN         PIC  X(01). *> 更新区分
 01  LINK-IN-RUNNO          PIC  9(07). *> 実行NO
 01  LINK-IN-GRPNO          PIC  9(02). *> 送受信グループNO
 01  LINK-IN-KEKA           PIC  X(04). *> 処理区分
*
**************************************************************
 PROCEDURE             DIVISION  USING LINK-IN-JDATE
                                       LINK-IN-JTIME
                                       LINK-IN-UPDKBN
                                       LINK-IN-RUNNO
                                       LINK-IN-GRPNO
                                       LINK-IN-KEKA.
**************************************************************
 DECLARATIVES.
 TJS-ERR-SEC               SECTION.
     USE         AFTER     EXCEPTION PROCEDURE D365DAYF.
     MOVE        TJS-ST    TO        E-ST.
     MOVE       "D365DAYF" TO        E-FILE.
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
***********************************
*    MOVE "20200201"  TO        LINK-IN-JDATE
*    MOVE "120110"    TO        LINK-IN-JTIME
*    MOVE "2"         TO        LINK-IN-UPDKBN
*    MOVE "9000009"   TO        LINK-IN-RUNNO
*    MOVE "909"       TO        LINK-IN-GRPNO
*    MOVE "8888"      TO        LINK-IN-KEKA
*
     DISPLAY "日付            ="  LINK-IN-JDATE   UPON CONS
     DISPLAY "時刻            ="  LINK-IN-JTIME   UPON CONS
     DISPLAY "更新区分        ="  LINK-IN-UPDKBN  UPON CONS
     DISPLAY "実行_          ="  LINK-IN-RUNNO   UPON CONS
     DISPLAY "送受信グループ_="  LINK-IN-GRPNO   UPON CONS
     DISPLAY "結果区分        ="  LINK-IN-KEKA    UPON CONS
***********************************
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
     OPEN      I-O     D365DAYF.
*システム日付取得／時刻取得
     ACCEPT    SYS-DATE    FROM    DATE.
     ACCEPT    WK-TIME     FROM    TIME.
*システム日付６桁→８桁変換（サブ日付チェック／変換）
 INIT010.
     MOVE      "3"         TO      LINK-IN-KBN.
     MOVE      SYS-DATE    TO      LINK-IN-YMD6.
     MOVE      ZERO        TO      LINK-IN-YMD8.
     MOVE      ZERO        TO      LINK-OUT-RET.
     MOVE      ZERO        TO      LINK-OUT-YMD.
     CALL     "SKYDTCKB"   USING   LINK-IN-KBN
                                   LINK-IN-YMD6
                                   LINK-IN-YMD8
                                   LINK-OUT-RET
                                   LINK-OUT-YMD.
     MOVE      LINK-OUT-YMD TO     WK-DATE.
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
     MOVE  LINK-IN-JTIME(1:4)    TO  TJS-F02.
*ファイル読込み
     READ  D365DAYF
       INVALID
         PERFORM D365DAYF-OUT-SEC
       NOT INVALID
         PERFORM D365DAYF-UPD-SEC
     END-READ.

 MAIN-EXIT.
     EXIT.
****************************************************************
*             終了処理                               3.0       *
****************************************************************
 END-SEC               SECTION.
     MOVE     "END-SEC"      TO   S-NAME.
*ファイルのＯＰＥＮ
     CLOSE     D365DAYF.
*
 END-EXIT.
     EXIT.
****************************************************************
*             D365当日送受信制御ファイル出力         2.1       *
****************************************************************
 D365DAYF-OUT-SEC     SECTION.
     MOVE     "D365DAYF-OUT-SEC"   TO   S-NAME.
*D365当日送受信制御編集
     MOVE      SPACE               TO   TJS-REC.
     INITIALIZE                         TJS-REC.
     MOVE      LINK-IN-JDATE       TO   TJS-F01.
     MOVE      LINK-IN-JTIME(1:4)  TO   TJS-F02.
     MOVE      LINK-IN-GRPNO       TO   TJS-F03.
     IF        LINK-IN-UPDKBN      =    "1"
         MOVE      WK-DATE              TO   TJS-F04
         MOVE      WK-TIME              TO   TJS-F05
     END-IF.
     IF        LINK-IN-UPDKBN      =    "2"
         MOVE      WK-DATE              TO   TJS-F06
         MOVE      WK-TIME              TO   TJS-F07
     END-IF.
     MOVE      LINK-IN-KEKA       TO   TJS-F08.
     MOVE      LINK-IN-RUNNO       TO   TJS-F09.
*
*D365当日送受信制御出力
     WRITE      TJS-REC.
*
 D365DAYF-OUT-EXIT.
     EXIT.
****************************************************************
*             D365当日送受信制御ファイル更新         2.2       *
****************************************************************
 D365DAYF-UPD-SEC     SECTION.
     MOVE     "D365DAYF-UPD-SEC"   TO   S-NAME.
*D365当日送受信制御編集
     IF        LINK-IN-UPDKBN      =    "1"
         MOVE      WK-DATE              TO   TJS-F04
         MOVE      WK-TIME              TO   TJS-F05
     END-IF.
     IF        LINK-IN-UPDKBN      =    "2"
         MOVE      WK-DATE              TO   TJS-F06
         MOVE      WK-TIME              TO   TJS-F07
     END-IF.
     MOVE      LINK-IN-KEKA       TO   TJS-F08.
*
*D365当日送受信制御更新
     REWRITE   TJS-REC.
*
 D365DAYF-UPD-EXIT.
     EXIT.
*****************<<  SCV0060B   END PROGRAM  >>******************

```
