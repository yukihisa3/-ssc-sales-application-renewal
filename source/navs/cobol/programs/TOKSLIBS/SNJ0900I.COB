****************************************************************
*
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　
*    サブシステム　　　　：　受配信管理システム　　　　　　　　
*    業務名　　　　　　　：　ＥＯＳ管理　　　　　　　　
*    モジュール名　　　　：　受信状況照会
*    作成日／更新日　　　：　2010/08/20
*    作成者／更新者　　　：　ＮＡＶ飯田
*    処理概要　　　　　　：　　　　　　　　　　　　　　　　　　
*      下記の下位ＰＧに遷移する。
*      SNJ0900I <-----------------
*               <---             !
*                  !             !
*                  !             !
*                  --> SNJ0910I  --> SNJ0920I
*      F7で選択時
*        PSSY0005、発注集計表、CLへ遷移する。
*        PSSY0038、発注集計表（税込）、CLへ遷移する。
*
*    更新履歴　　　　　　：
*      2011/10/12 飯田/NAV 基幹サーバ統合・業務改善
*      2012/05/08 三浦/NAV 排他制御処理追加（発注集計表）
*
****************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SNJ0900I.
 AUTHOR.               S.I.
 DATE-WRITTEN.         2010/08/20.
****************************************************************
 ENVIRONMENT           DIVISION.
****************************************************************
 CONFIGURATION         SECTION.
 SPECIAL-NAMES.
     CONSOLE      IS   CONS.
*
 INPUT-OUTPUT          SECTION.
 FILE-CONTROL.
*画面ファイル
*当日スケジュールマスタ
     SELECT  JSMDAYF   ASSIGN    TO        DA-01-VI-JSMDAYL1
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      DYNAMIC
                       RECORD    KEY       TJS-F01
                                           TJS-F02
                                           TJS-F03
                       FILE      STATUS    TJS-ST.
*取引先マスタ
     SELECT  HTOKMS    ASSIGN    TO        DA-01-VI-TOKMS2
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      RANDOM
                       RECORD    KEY       TOK-F01
                       FILE      STATUS    TOK-ST.
*ＥＤＩメッセージマスタ
     SELECT  JSMMSGF   ASSIGN    TO        DA-01-VI-JSMMSGL1
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      RANDOM
                       RECORD    KEY       EMG-F01
                       FILE      STATUS    EMG-ST.
*排他制御ファイル
     SELECT  HACHLOCF  ASSIGN TO    DA-01-VI-HACHLOC1
                       ORGANIZATION      IS   INDEXED
                       ACCESS    MODE    IS   RANDOM
                       RECORD    KEY     IS   LOC-F01
                       FILE      STATUS  IS   LOC-ST.
*画面定義ファイル
     SELECT  DSPFILE   ASSIGN    TO        GS-DSPF
                       FORMAT              DSP-FMT
                       GROUP               DSP-GRP
                       PROCESSING          DSP-PRO
                       FUNCTION            DSP-FNC
                       FILE      STATUS    DSP-ST.
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
****************************************************************
*    FILE = 取引先マスタ                                       *
****************************************************************
 FD  HTOKMS
                       BLOCK     CONTAINS  8    RECORDS
                       LABEL     RECORD    IS   STANDARD.
                       COPY      HTOKMS    OF   XFDLIB
                       JOINING   TOK       AS   PREFIX.
****************************************************************
*    FILE = ＥＤＩメッセージマスタ                             *
****************************************************************
 FD  JSMMSGF
                       LABEL     RECORD    IS   STANDARD.
                       COPY      JSMMSGF   OF   XFDLIB
                       JOINING   EMG       AS   PREFIX.
****************************************************************
*    FILE = 発注集計表排他制御ファイル                         *
****************************************************************
 FD  HACHLOCF
                       LABEL     RECORD    IS   STANDARD.
                       COPY      HACHLOCF  OF   XFDLIB
                       JOINING   LOC       AS   PREFIX.
****************************************************************
*    FILE = 画面ファイル                                       *
****************************************************************
 FD  DSPFILE
                       LABEL     RECORD    IS   OMITTED.
                       COPY      FNJ0900I  OF   XMDLIB
                       JOINING   DSP       AS   PREFIX.
*
****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
*ステータス領域
 01  STATUS-AREA.
     03  TJS-ST                   PIC  X(02).
     03  TOK-ST                   PIC  X(02).
     03  EMG-ST                   PIC  X(02).
     03  LOC-ST                   PIC  X(02).
     03  DSP-ST                   PIC  X(02).
*画面制御用領域
 01  DSP-CONTROL.
     03  DSP-FMT                  PIC  X(08).
     03  DSP-GRP                  PIC  X(08).
     03  DSP-PRO                  PIC  X(02).
     03  DSP-FNC                  PIC  X(04).
*フラグ領域
 01  FLG-AREA.
     03  HTOKMS-INV-FLG           PIC  X(03)  VALUE  SPACE.
     03  CHK-FLG                  PIC  X(03)  VALUE  SPACE.
     03  ERR-FLG                  PIC  9(02)  VALUE  ZERO.
     03  END-FLG                  PIC  X(03)  VALUE  SPACE.
     03  RD-FLG                   PIC  X(03)  VALUE  SPACE.
     03  SET-FLG                  PIC  X(03)  VALUE  SPACE.
* 2011/10/13,S  S.I/NAV
     03  FG-MINYURYOKU            PIC  9(01).
* 2011/10/13,E  S.I/NAV
*ワーク領域
 01  WRK-AREA.
***  プログラムスイッチ（画面遷移制御）
     03  PSW                      PIC  X(01)  VALUE  SPACE.
     03  P-CNT                    PIC  9(01)  VALUE  ZERO.
     03  S-CNT                    PIC  9(01)  VALUE  ZERO.
     03  C-CNT                    PIC  9(01)  VALUE  ZERO.

     03  X                        PIC  9(02)  VALUE  ZERO.
     03  Y                        PIC  9(02)  VALUE  ZERO.
     03  IX                       PIC  9(03)  VALUE  ZERO.
     03  SV-SYOKBN                PIC   9(01).
     03  SV-SYOGYO                PIC   9(01).
     03  WK-GYO                   PIC   9(02)  VALUE  ZERO.
     03  FG-JSMMSGF-INV           PIC   9(01).
     03  IX-PGMAX                 PIC   9(03).
     03  IX-PGMAX-GYO             PIC   9(03).
* 2011/10/13,S  S.I/NAV
     03  WK-JDATE.
       05  WK-JDATE-Y       PIC  9(04).
       05  WK-JDATE-M       PIC  9(02).
       05  WK-JDATE-D       PIC  9(02).
       05  WK-HCSYU-KBN     PIC  X(01).
* 2011/10/13,E  S.I/NAV

*
*日付／時刻
 01  TIME-AREA.
     03  WK-TIME                  PIC  9(08)  VALUE  ZERO.
 01  DATE-AREA.
     03  WK-YS                    PIC  9(02)  VALUE  ZERO.
     03  WK-DATE.
         05  WK-Y                 PIC  9(02)  VALUE  ZERO.
         05  WK-M                 PIC  9(02)  VALUE  ZERO.
         05  WK-D                 PIC  9(02)  VALUE  ZERO.
 01  DATE-AREAR2       REDEFINES      DATE-AREA.
     03  SYS-DATE                 PIC  9(08).
*画面表示日付編集
 01  HEN-DATE.
     03  HEN-DATE-YYYY            PIC  9(04)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  "/".
     03  HEN-DATE-MM              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  "/".
     03  HEN-DATE-DD              PIC  9(02)  VALUE  ZERO.
*画面表示時刻編集
 01  HEN-TIME.
     03  HEN-TIME-HH              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  ":".
     03  HEN-TIME-MM              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  ":".
     03  HEN-TIME-SS              PIC  9(02)  VALUE  ZERO.
*
*ＰＦガイド
* 2011/10/13,S  S.I/NAV
** 01  PF-MSG-AREA.
**     03  PF-MSG1.
**         05  FILLER               PIC   N(20)
**      VALUE NC"_取消　_終了　　　　　　　　　　　　　".
**     03  PF-MSG2.
**         05  FILLER               PIC   N(20)
**      VALUE NC"_取消　_終了　_前頁　_次頁　　　　　".
** 01  PF-MSG-AREA-R       REDEFINES     PF-MSG-AREA.
**     03  PF-MSG-R   OCCURS   2   PIC   N(20).
* 2011/10/13,E  S.I/NAV
* 2011/10/13,S  S.I/NAV
 01  PF-MSG-AREA.
     03  PF-MSG1.
       05  FILLER           PIC  N(30)  VALUE
       NC"_取消　_終了".
     03  PF-MSG2.
       05  FILLER           PIC  N(30)  VALUE
*---------------------------------------------------------------E*
       NC"_取消　_終了　_項目戻り　_発注集計表　_前頁　_次
-        "頁".
     03  PF-MSG3.
       05  FILLER           PIC  N(30)  VALUE
       NC"_取消　_終了　_項目戻り　_前頁　_次頁".
     03  PF-MSG4.
       05  FILLER           PIC  N(30)  VALUE
       NC"_取消　_終了　_項目戻り".
 01  PF-MSG-AREA-R       REDEFINES     PF-MSG-AREA.
     03  PF-MSG-R           PIC  N(30)  OCCURS 4.
* 2011/10/13,E  S.I/NAV
*
*メッセージの取得
 01  ERR-MSG-AREA.
     03  ERR-MSG1.
       05  FILLER              PIC   N(30)  VALUE
       NC"照会日付を正しく入力して下さい。".
     03  ERR-MSG2.
       05  FILLER              PIC   N(30)  VALUE
       NC"無効キーです。".
     03  ERR-MSG3.
       05  FILLER              PIC   N(30)  VALUE
       NC"前頁はありません。".
     03  ERR-MSG4.
       05  FILLER              PIC   N(30)  VALUE
       NC"次頁はありません。".
     03  ERR-MSG5.
       05  FILLER              PIC   N(30)  VALUE
       NC"入力した日付のデータは存在しません。".
     03  ERR-MSG6.
       05  FILLER              PIC   N(30)  VALUE
       NC"照会画面区分に誤りがあります。".
     03  ERR-MSG7.
       05  FILLER              PIC   N(30)  VALUE
       NC"照会行番号に誤りがあります。".
     03  ERR-MSG8.
*---------------------------------------------------------------E*
       05  FILLER              PIC   N(30)  VALUE
         NC"指定条件に誤りがないか確認し、ＥＮＴＥＲを押下して下
-          "さい。".
     03  ERR-MSG9.
       05  FILLER              PIC   N(30)  VALUE
       NC"発注集計表発行が終了しました。".
     03  ERR-MSG10.
       05  FILLER              PIC   N(30)  VALUE
       NC"　".
     03  ERR-MSG11.
       05  FILLER              PIC   N(30)  VALUE
   NC"他端末で発行中です。少し時間をおいて再度実行して下さい。".
 01  ERR-MSG-AREA-R  REDEFINES ERR-MSG-AREA.
     03  ERR-MSG-R             PIC   N(30)  OCCURS 11.
*基本スケジュール退避エリア
 01  TABLE-AREA.
     03  TBL-JDATE.
       05  TBL-JDATE1       PIC   9(04).
       05  TBL-JDATE2       PIC   9(02).
       05  TBL-JDATE3       PIC   9(02).
     03  TABLE1  OCCURS 8.    *> 頁
       05  TABLE2  OCCURS 15. *> 行
         07  TBL-MTIME      PIC   9(04). *>時間
         07  TBL-MTOKCD     PIC   9(08). *>取引先コード
         07  TBL-DSYUBT     PIC   9(02). *>データ種別
         07  TBL-MJKEN      PIC   9(05). *>受信件数
         07  TBL-MDKEN      PIC   9(05). *>伝票枚数
         07  TBL-JYSN-KBN   PIC   9(01). *>受信・状況
         07  TBL-HENKN-KBN  PIC   9(01). *>変換・状況
         07  TBL-KOSIN-KBN  PIC   9(01). *>更新・状況
         07  TBL-MKEKA      PIC   X(04). *>受信状態
*照会日付退避ワーク
 01  GAMEN-DATE.
     03  GAMEN-YY                 PIC   9(04)  VALUE  ZERO.
     03  GAMEN-MM                 PIC   9(02)  VALUE  ZERO.
     03  GAMEN-DD                 PIC   9(02)  VALUE  ZERO.
*
*ファイルエラーメッセージ
 01  FILE-ERR.
     03  TJS-ERR           PIC N(15) VALUE
                        NC"当日スケジュールエラー".
     03  TOK-ERR           PIC N(15) VALUE
                        NC"取引先マスタエラー".
     03  EMG-ERR           PIC N(15) VALUE
                        NC"ＥＤＩメッセージマスタエラー".
     03  LOC-ERR           PIC N(15) VALUE
                        NC"排他制御ファイルエラー".
     03  DSP-ERR           PIC N(15) VALUE
                        NC"画面ファイルエラー".
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
*    受信件数照会・連絡領域
 01  PA-SNJ0910I-AREA.
     03  PA-SNJ0910I-SYORI   PIC  X(01). *> 処理区分
     03  PA-SNJ0910I-JYSNBI  PIC  9(08). *> 受信日
     03  PA-SNJ0910I-JYSNTM  PIC  9(04). *> 受信時間
     03  PA-SNJ0910I-TORCD   PIC  9(08). *> 取引先コード

*    受配信結果照会・連絡領域
 01  PA-SNJ0920I-AREA.
     03  PA-SNJ0920I-JYSNBI  PIC  9(08).  *> 受信日
     03  PA-SNJ0920I-JYSNTM  PIC  9(04).  *> 受信時間
     03  PA-SNJ0920I-TORCD   PIC  9(08).  *> 受信取引先コード
     03  PA-SNJ0920I-DSYUBT  PIC  9(02).  *> データ種別
     03  PA-SNJ0920I-ERMSGNO PIC  X(04).  *> エラーメッセージNO
*    発注集計表出力・連絡領域
 01  PA-PSSY0005-AREA.
     03  PA-PSSY0005-JYSNBI  PIC  X(08).  *> 受信日
     03  PA-PSSY0005-JYSNTM  PIC  X(04).  *> 受信時間
     03  PA-PSSY0005-TORCD   PIC  X(08).  *> 受信取引先コード
     03  PA-PSSY0005-KBN     PIC  X(02).  *> 実行区分　　　　

****************************************************************
 LINKAGE              SECTION.
****************************************************************
* パラメーター エリア
 01  PARA-WKSTN                   PIC  X(08).
*
*
**************************************************************
 PROCEDURE             DIVISION   USING   PARA-WKSTN.
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
 TOK-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HTOKMS.
     MOVE        TOK-ST    TO        E-ST.
     MOVE        "HTOKMS"  TO        E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     TOK-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 EGM-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE JSMMSGF.
     MOVE        EMG-ST    TO        E-ST.
     MOVE        "JSMMSGF" TO        E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     EMG-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 LOC-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HACHLOCF.
     MOVE        LOC-ST    TO        E-ST.
     MOVE        "HACHLOCF" TO        E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     LOC-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 DSP-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE DSPFILE.
     MOVE        DSP-ST    TO        E-ST.
     MOVE        "DSPFILE" TO        E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     DSP-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 END  DECLARATIVES.
****************************************************************
*             MAIN        MODULE                     0.0       *
****************************************************************
 PROCESS-START         SECTION.
     MOVE  "PROCESS-START"  TO  S-NAME.

     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC   UNTIL END-FLG = "END".
     PERFORM  END-SEC.

     STOP RUN.
 CONTROL-EXIT.
     EXIT.
****************************************************************
*             初期処理                               1.0
****************************************************************
 INIT-SEC              SECTION.
     MOVE  "INIT-SEC"       TO  S-NAME.
* システム日付・時刻の取得
     PERFORM  SDATE-GET-SEC.
* ファイルのＯＰＥＮ
     OPEN  INPUT JSMDAYF.
     OPEN  I-O   DSPFILE  HACHLOCF.
     OPEN  INPUT HTOKMS.
     OPEN  INPUT JSMMSGF.
* ワークの初期化
     INITIALIZE  FLG-AREA.
     INITIALIZE  TABLE-AREA.
* 初期画面の表示
     MOVE  SPACE            TO  DSP-PRO.
* ヘッド入力へ
     MOVE  "1"              TO  PSW.

 INIT-EXIT.
     EXIT.
****************************************************************
*    システム日付取得処理                                      *
****************************************************************
 SDATE-GET-SEC          SECTION.
     ACCEPT  WK-DATE   FROM DATE.
     MOVE  "3"              TO  LINK-IN-KBN.
     MOVE  WK-DATE          TO  LINK-IN-YMD6.
     MOVE  ZERO             TO  LINK-IN-YMD8.
     MOVE  ZERO             TO  LINK-OUT-RET.
     MOVE  ZERO             TO  LINK-OUT-YMD.
     CALL  "SKYDTCKB"   USING LINK-IN-KBN
                              LINK-IN-YMD6
                              LINK-IN-YMD8
                              LINK-OUT-RET
                              LINK-OUT-YMD.
     MOVE  LINK-OUT-YMD     TO  DATE-AREA.
* 画面表示日付編集
     MOVE  SYS-DATE(1:4)    TO  HEN-DATE-YYYY.
     MOVE  SYS-DATE(5:2)    TO  HEN-DATE-MM.
     MOVE  SYS-DATE(7:2)    TO  HEN-DATE-DD.
* システム日付取得
     ACCEPT  WK-TIME   FROM TIME.

* 画面表示時刻編集
     MOVE  WK-TIME(1:2)     TO  HEN-TIME-HH.
     MOVE  WK-TIME(3:2)     TO  HEN-TIME-MM.
     MOVE  WK-TIME(5:2)     TO  HEN-TIME-SS.
 SDATE-GET-EXIT.
     EXIT.
****************************************************************
*             メイン処理                             2.0
****************************************************************
 MAIN-SEC              SECTION.
     MOVE  "MAIN-SEC"       TO  S-NAME.
*
     EVALUATE  PSW
    *> 初画面編集
       WHEN  "1"
         PERFORM  DSP-INIT-SEC
         MOVE  "2"          TO  PSW

    *> 照会日付入力
       WHEN  "2"
         PERFORM  DSP-HEAD-SEC

    *> 照会画面・行選択
       WHEN  "3"
         PERFORM  DSP-SELNO-SEC

*  2011/10/13,S  S.I/NAV
    *> 発注集計表・行選択
       WHEN  "4"
         PERFORM  DSP-HACSYU-SEC
*  2011/10/13,E  S.I/NAV
    *> 確認
       WHEN  "5"
         PERFORM  DSP-KAKU-SEC

    *> 以外
       WHEN  OTHER
         DISPLAY "SNJ0900I PSW HANIGAI ??? PSW=" PSW
            "*" UPON CONS
         MOVE  4000         TO  PROGRAM-STATUS
         EXIT PROGRAM
     END-EVALUATE.

 MAIN-EXIT.
     EXIT.
****************************************************************
*             初期画面表示                                     *
****************************************************************
 DSP-INIT-SEC          SECTION.
     MOVE  "DSP-INIT-SEC"   TO  S-NAME.
* 初期画面の表示
     MOVE  SPACE            TO  DSP-PRO.
* 画面の初期化
*    画面クリア
       IF PSW = "1" OR "2"
          *> 初期画面、日付入力、確認
          MOVE  SPACE       TO  DSP-FNJ0900I
          MOVE  ZERO        TO  TBL-JDATE
       END-IF.

       IF PSW = "3"
          *> 照会画面・行選択
          MOVE  SPACE       TO  DSP-SYOKBN (1:)
          MOVE  SPACE       TO  DSP-SYOGYO (1:)
       END-IF.

       IF PSW = "4" OR "5"
          *> 発注集計表・行選択
          MOVE  SPACE       TO  DSP-HCHYO (1:)
          MOVE  SPACE       TO  DSP-HCTORN
       END-IF.

* リバース，カーソルパーク解除
     EVALUATE  TRUE
    *> 初画面編集、確認入力
       WHEN  PSW = "1" OR "2"
         MOVE  "M"   TO EDIT-OPTION OF DSP-JDATE1
         MOVE  "M"   TO EDIT-OPTION OF DSP-JDATE2
         MOVE  "M"   TO EDIT-OPTION OF DSP-JDATE3
         MOVE  "M"   TO EDIT-OPTION OF DSP-SYOKBN
         MOVE  "M"   TO EDIT-OPTION OF DSP-SYOGYO
         MOVE  SPACE TO EDIT-CURSOR OF DSP-JDATE1
         MOVE  SPACE TO EDIT-CURSOR OF DSP-JDATE2
         MOVE  SPACE TO EDIT-CURSOR OF DSP-JDATE3
         MOVE  SPACE TO EDIT-CURSOR OF DSP-SYOKBN
         MOVE  SPACE TO EDIT-CURSOR OF DSP-SYOGYO

    *> 照会画面・行選択
       WHEN  PSW = "3"
         MOVE  "M"   TO EDIT-OPTION OF DSP-SYOKBN
         MOVE  "M"   TO EDIT-OPTION OF DSP-SYOGYO
         MOVE  SPACE TO EDIT-CURSOR OF DSP-SYOKBN
         MOVE  SPACE TO EDIT-CURSOR OF DSP-SYOGYO

    *> 発注集計表・行選択
       WHEN  PSW = "4"
         MOVE  "M"   TO EDIT-OPTION OF DSP-HCHYO
         MOVE  SPACE TO EDIT-CURSOR OF DSP-HCHYO

     END-EVALUATE.


* ＰＲＯＧＲＡＭ－ＩＤセット
     MOVE  "SNJ0900I"       TO  DSP-PGID.
* システム日付転送
     MOVE  SYS-DATE         TO  DSP-SDATE.
* システム時間転送
     MOVE  WK-TIME(1:6)     TO  DSP-STIME.
* 日付の初期表示
     IF     TBL-JDATE1 = ZERO
        AND TBL-JDATE2 = ZERO
        AND TBL-JDATE3 = ZERO
        MOVE  SYS-DATE(1:4) TO  DSP-JDATE1
        MOVE  SYS-DATE(5:2) TO  DSP-JDATE2
        MOVE  SYS-DATE(7:2) TO  DSP-JDATE3
     ELSE
        MOVE  TBL-JDATE1    TO  DSP-JDATE1
        MOVE  TBL-JDATE2    TO  DSP-JDATE2
        MOVE  TBL-JDATE3    TO  DSP-JDATE3
     END-IF.

 DSP-INIT-EXIT.
     EXIT.
****************************************************************
*             処理区分入力( PSW = 1 )                2.1       *
****************************************************************
 DSP-HEAD-SEC          SECTION.
     MOVE  "DSP-HEAD-SEC"        TO  S-NAME.

     PERFORM  DSP-WRITE-SEC.
     PERFORM  DSP-READ-SEC.

     EVALUATE  DSP-FNC
*      実行
       WHEN  "E000"
         PERFORM  HEAD-CHK-SEC

*      取消
       WHEN  "F004"
         MOVE  "1"               TO  PSW

*      終了
       WHEN  "F005"
         MOVE  "END"             TO  END-FLG

       WHEN  OTHER
         MOVE  2                 TO  ERR-FLG

     END-EVALUATE.

 DSP-HEAD-EXIT.
     EXIT.
****************************************************************
*             画面表示処理                                     *
****************************************************************
 DSP-WRITE-SEC         SECTION.
     MOVE  "DSP-WRITE-SEC"  TO  S-NAME.

* システム日付・時刻の取得
     PERFORM  SDATE-GET-SEC.
* システム日付転送
     MOVE  SYS-DATE         TO  DSP-SDATE.
* システム時間転送
     MOVE  WK-TIME(1:6)     TO  DSP-STIME.
* エラーメッセージセット
     IF  ERR-FLG = ZERO
         MOVE  SPACE              TO  DSP-ERRMSG
     ELSE
         MOVE  ERR-MSG-R(ERR-FLG) TO  DSP-ERRMSG
         MOVE  ZERO               TO  ERR-FLG
     END-IF.

* ガイドメッセージの設定
     EVALUATE  PSW
     *>曜日
       WHEN  "1" WHEN "2"
         MOVE  PF-MSG-R(1)  TO  DSP-PFGAID

     *>照会画面・行選択
       WHEN  "3"
         MOVE  PF-MSG-R(2)  TO  DSP-PFGAID

  *> 2011/10/13,S  S.I/NAV
     *>発注集計表・行選択
       WHEN  "4"
         MOVE  PF-MSG-R(3)  TO  DSP-PFGAID
  *> 2011/10/13,E  S.I/NAV
     *>確認
       WHEN  "5"
         MOVE  PF-MSG-R(4)  TO  DSP-PFGAID

     END-EVALUATE.
* 画面の表示
     MOVE  "SCREEN"         TO  DSP-GRP.
     MOVE  "FNJ0900I"       TO  DSP-FMT.
     WRITE  DSP-FNJ0900I.

 DSP-WRITE-EXIT.
     EXIT.
****************************************************************
*             画面読込処理                                     *
****************************************************************
 DSP-READ-SEC          SECTION.
     MOVE  "DSP-READ-SEC"   TO  S-NAME.

     MOVE  "NE"             TO  DSP-PRO.

     EVALUATE  PSW
    *> 曜日
       WHEN  "2"
         MOVE  "HEAD"       TO  DSP-GRP

    *> 照会画面・行選択
       WHEN  "3"
         MOVE  "SELNO"       TO  DSP-GRP

  *> 2011/10/13,S  S.I/NAV
    *> 発注集計表・行選択
       WHEN  "4"
         MOVE  "GHCSYU"     TO  DSP-GRP
  *> 2011/10/13,E  S.I/NAV

    *> 確認
       WHEN  "5"
         MOVE  "KAKU"       TO  DSP-GRP

     END-EVALUATE.

     MOVE  "FNJ0900I"       TO  DSP-FMT.
     READ  DSPFILE.
*入力項目の属性を通常にする
 DSP-READ-010.
*    MOVE  ZERO             TO  ERR-FLG.
     MOVE  SPACE            TO  DSP-PRO.
*
 DSP-READ-EXIT.
     EXIT.
****************************************************************
*             ヘッド部チェック                                 *
****************************************************************
 HEAD-CHK-SEC             SECTION.
     PERFORM  HEAD-CHKB-SEC.
     IF  ERR-FLG NOT = ZERO
         GO TO  HEAD-CHK-EXIT
     END-IF.

**   受信状況照会（明細）
     MOVE  "3"                   TO  PSW.
     MOVE  1                     TO  P-CNT.
     PERFORM  WORK-DSP-SEC.

 HEAD-CHK-EXIT.
     EXIT.
****************************************************************
*             日付チェック                                     *
****************************************************************
 HEAD-CHKB-SEC             SECTION.
     MOVE     "HEAD-CHK-SEC"     TO   S-NAME.

     MOVE  SPACE  TO  DSP-HCHYO (1:).
     MOVE  SPACE  TO  DSP-HCTORN.
     MOVE  "M"    TO  EDIT-OPTION OF DSP-HCHYO.
     MOVE  SPACE  TO  EDIT-CURSOR OF DSP-HCHYO.

*日付チェック（未入力／範囲外）
     IF       DSP-JDATE1     =   ZERO
     OR       DSP-JDATE2     =   ZERO
     OR       DSP-JDATE3     =   ZERO
     OR       DSP-JDATE1     NOT NUMERIC
     OR       DSP-JDATE2     NOT NUMERIC
     OR       DSP-JDATE3     NOT NUMERIC
              MOVE   1       TO  ERR-FLG

              IF     (DSP-JDATE1 = ZERO OR SPACE)
                 AND (DSP-JDATE2 = ZERO OR SPACE)
                 AND (DSP-JDATE3 = ZERO OR SPACE)
                 MOVE  SPACE TO  DSP-JDATE1 (1:)
                 MOVE  SPACE TO  DSP-JDATE2 (1:)
                 MOVE  SPACE TO  DSP-JDATE3 (1:)
              END-IF

              MOVE  "R"      TO  EDIT-OPTION  OF  DSP-JDATE1
              MOVE  "R"      TO  EDIT-OPTION  OF  DSP-JDATE2
              MOVE  "R"      TO  EDIT-OPTION  OF  DSP-JDATE3
              MOVE  "C"      TO  EDIT-CURSOR  OF  DSP-JDATE1
     ELSE
              MOVE  "M"      TO  EDIT-OPTION  OF  DSP-JDATE1
              MOVE  "M"      TO  EDIT-OPTION  OF  DSP-JDATE2
              MOVE  "M"      TO  EDIT-OPTION  OF  DSP-JDATE3
              MOVE   SPACE   TO  EDIT-CURSOR  OF  DSP-JDATE1
     END-IF.
*日付論理チェック（日付チェックサブルーチン使用）
     MOVE     "2"                 TO   LINK-IN-KBN.
     MOVE     ZERO                TO   LINK-IN-YMD6.
     MOVE     DSP-JDATE1          TO   GAMEN-YY.
     MOVE     DSP-JDATE2          TO   GAMEN-MM.
     MOVE     DSP-JDATE3          TO   GAMEN-DD.
     MOVE     GAMEN-DATE          TO   LINK-IN-YMD8.
     MOVE     ZERO                TO   LINK-OUT-RET.
     MOVE     ZERO                TO   LINK-OUT-YMD.
     CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD.
     IF    LINK-OUT-RET  NOT =  ZERO
              MOVE   1       TO  ERR-FLG
              MOVE  "R"      TO  EDIT-OPTION OF DSP-JDATE1
              MOVE  "R"      TO  EDIT-OPTION OF DSP-JDATE2
              MOVE  "R"      TO  EDIT-OPTION OF DSP-JDATE3
              MOVE  "C"      TO  EDIT-CURSOR OF DSP-JDATE1
     ELSE
              MOVE  "M"      TO  EDIT-OPTION OF DSP-JDATE1
              MOVE  "M"      TO  EDIT-OPTION OF DSP-JDATE2
              MOVE  "M"      TO  EDIT-OPTION OF DSP-JDATE3
              MOVE   SPACE   TO  EDIT-CURSOR OF DSP-JDATE1
     END-IF.

*        マスタからワークへデータセット
     IF  ERR-FLG = ZERO
         PERFORM  MST-WORK-SEC
     END-IF.

 HEAD-CHKB-EXIT.
     EXIT.
****************************************************************
*             マスタ→ワーク（退避）                           *
****************************************************************
 MST-WORK-SEC          SECTION.
     MOVE     "MST-WORK-SEC"   TO   S-NAME.
* ワークテーブルクリア
     MOVE     SPACE            TO   TABLE-AREA.
     INITIALIZE                     TABLE-AREA.
* 照会日付のセット
     MOVE     DSP-JDATE1       TO   TBL-JDATE1.
     MOVE     DSP-JDATE2       TO   TBL-JDATE2.
     MOVE     DSP-JDATE3       TO   TBL-JDATE3.
* 曜日により当日スケジュールマスタスタート
     PERFORM JSMDAYF-START-SEC.
     IF       CHK-FLG  =  "CHK"
              MOVE   5       TO  ERR-FLG
              MOVE  "R"      TO  EDIT-OPTION  OF  DSP-JDATE1
              MOVE  "R"      TO  EDIT-OPTION  OF  DSP-JDATE2
              MOVE  "R"      TO  EDIT-OPTION  OF  DSP-JDATE3
              MOVE  "C"      TO  EDIT-CURSOR  OF  DSP-JDATE1
              GO             TO  MST-WORK-EXIT
     ELSE
              MOVE  "M"      TO  EDIT-OPTION  OF  DSP-JDATE1
              MOVE  "M"      TO  EDIT-OPTION  OF  DSP-JDATE2
              MOVE  "M"      TO  EDIT-OPTION  OF  DSP-JDATE3
              MOVE   SPACE   TO  EDIT-CURSOR  OF  DSP-JDATE1
     END-IF.

* 当日スケジュールマスタ読込み
     PERFORM  JSMDAYF-READ-SEC.
     IF       RD-FLG  =  "END"
              MOVE   5       TO  ERR-FLG
              MOVE  "R"      TO  EDIT-OPTION  OF  DSP-JDATE1
              MOVE  "R"      TO  EDIT-OPTION  OF  DSP-JDATE2
              MOVE  "R"      TO  EDIT-OPTION  OF  DSP-JDATE3
              MOVE  "C"      TO  EDIT-CURSOR  OF  DSP-JDATE1
              GO             TO  MST-WORK-EXIT
     ELSE
              MOVE  "M"      TO  EDIT-OPTION  OF  DSP-JDATE1
              MOVE  "M"      TO  EDIT-OPTION  OF  DSP-JDATE2
              MOVE  "M"      TO  EDIT-OPTION  OF  DSP-JDATE3
              MOVE   SPACE   TO  EDIT-CURSOR  OF  DSP-JDATE1
     END-IF.
* 当日スケジュールマスタを読込みながらワークへセット
     MOVE ZERO              TO  IX-PGMAX.
     MOVE ZERO              TO  IX-PGMAX-GYO.

     PERFORM  VARYING X  FROM 1 BY 1
              UNTIL   X > 8
                   OR GAMEN-DATE NOT = TJS-F01
                   OR RD-FLG         = "END"
       PERFORM  VARYING Y  FROM 1 BY 1
                UNTIL   Y > 15
                     OR GAMEN-DATE NOT = TJS-F01
                     OR RD-FLG         = "END"
         MOVE  TJS-F02  TO  TBL-MTIME    (X Y) *> 時間
         MOVE  TJS-F03  TO  TBL-MTOKCD   (X Y) *> 取引先
         MOVE  TJS-F04  TO  TBL-DSYUBT   (X Y) *> データ種別
         MOVE  TJS-F09  TO  TBL-MJKEN    (X Y) *> 受信件数
         MOVE  TJS-F10  TO  TBL-MDKEN    (X Y) *> 伝票枚数
         MOVE  TJS-F11  TO  TBL-JYSN-KBN (X Y) *> 受信状況
         MOVE  TJS-F12  TO  TBL-HENKN-KBN(X Y) *> 変換状況
         MOVE  TJS-F13  TO  TBL-KOSIN-KBN(X Y) *> 更新状況
         MOVE  TJS-F15  TO  TBL-MKEKA    (X Y) *> 受信状態
         MOVE  X        TO  IX-PGMAX
         MOVE  Y        TO  IX-PGMAX-GYO
         PERFORM  JSMDAYF-READ-SEC

       END-PERFORM

     END-PERFORM.

     CLOSE  JSMDAYF.
     OPEN   INPUT JSMDAYF.
**
 MST-WORK-EXIT.
     EXIT.
****************************************************************
*             当日スケジュールマスタ読込み                     *
****************************************************************
 JSMDAYF-START-SEC     SECTION.
     MOVE  "JSMDAYF-START-SEC" TO  S-NAME.
* 曜日により基本スケジュールマスタスタート
     MOVE  SPACE            TO  RD-FLG.
     MOVE  SPACE            TO  CHK-FLG.
     MOVE  GAMEN-DATE       TO  TJS-F01.
     MOVE  ZERO             TO  TJS-F02.
     MOVE  ZERO             TO  TJS-F03.

     START  JSMDAYF  KEY IS >= TJS-F01  TJS-F02  TJS-F03
       INVALID
         MOVE  "CHK"        TO  CHK-FLG
     END-START.

 JSMDAYF-START-EXIT.
     EXIT.
****************************************************************
*             当日スケジュールマスタ読込み                     *
****************************************************************
 JSMDAYF-READ-SEC      SECTION.
     MOVE  "JSMDAYF-READ-SEC"  TO  S-NAME.
* マスタ読込み
     READ JSMDAYF NEXT
       AT END
         MOVE  "END"        TO  RD-FLG
     END-READ.
* 画面曜日以外の時、終了へ
     IF  TJS-F01 > GAMEN-DATE
         MOVE  "END"        TO  RD-FLG
     END-IF.

 JSMDAYF-READ-EXIT.
     EXIT.
****************************************************************
*             ワーク→画面表示                                 *
****************************************************************
 WORK-DSP-SEC          SECTION.
     MOVE  "WORK-DSP-SEC"        TO  S-NAME.
*項目画面セット
     MOVE  ZERO                  TO  WK-GYO.
     MOVE  TBL-JDATE1            TO  DSP-JDATE1.
     MOVE  TBL-JDATE2            TO  DSP-JDATE2.
     MOVE  TBL-JDATE3            TO  DSP-JDATE3.
     PERFORM  VARYING Y  FROM 1 BY 1
              UNTIL   Y > 15
       PERFORM  WORK-DSPB-SEC

     END-PERFORM.
*
 WORK-DSP-EXIT.
     EXIT.
****************************************************************
*            ワーク→画面表示Ｂ
****************************************************************
 WORK-DSPB-SEC          SECTION.
     MOVE  "WORK-DSPB-SEC"       TO  S-NAME.

* 行番号設定
     ADD 1  TO WK-GYO
     MOVE WK-GYO                 TO  DSP-NO (Y)

     IF     P-CNT = IX-PGMAX
        AND Y     > IX-PGMAX-GYO
        MOVE  SPACE              TO  DSP-MAS001 (Y)
        GO TO  WORK-DSPB-EXIT
     END-IF.

     MOVE  TBL-MTIME (P-CNT Y)   TO  DSP-MTIME (Y).
     MOVE  TBL-MTOKCD(P-CNT Y)   TO  DSP-MTOKCD(Y).
     MOVE  TBL-MTOKCD(P-CNT Y)   TO  TOK-F01.

     PERFORM  HTOKMS-READ-SEC.
     IF  HTOKMS-INV-FLG = SPACE
         MOVE  TOK-F03           TO  DSP-MTOKNM(Y)
     ELSE
         MOVE  ALL NC"＊"        TO  DSP-MTOKNM(Y)
     END-IF.

     MOVE  "WORK-DSPB-SEC"       TO  S-NAME.
*  データ種別
     EVALUATE  TBL-DSYUBT (P-CNT Y)
**     01:発注
       WHEN  01
         MOVE NC"発注"           TO  DSP-DSYUBT(Y)

**     02:受領
       WHEN  02
         MOVE NC"受領"           TO  DSP-DSYUBT(Y)

**     03:支払
       WHEN  03
         MOVE NC"支払"           TO  DSP-DSYUBT(Y)

**     04:請求
       WHEN  04
         MOVE NC"請求"           TO  DSP-DSYUBT(Y)

**     05:出荷
       WHEN  05
         MOVE NC"出荷"           TO  DSP-DSYUBT(Y)

**     06:欠品案内
       WHEN  06
         MOVE NC"欠品案内"       TO  DSP-DSYUBT(Y)

**     07:商品台帳
       WHEN  07
         MOVE NC"商品台帳"       TO  DSP-DSYUBT(Y)

       WHEN  08
         MOVE NC"他受"           TO  DSP-DSYUBT(Y)
**2012/12/21 NAV ST
       WHEN  09
         MOVE NC"ＢＭ"           TO  DSP-DSYUBT(Y)
**2012/12/21 NAV ED
       WHEN  OTHER
         MOVE NC"？？"           TO  DSP-DSYUBT(Y)

     END-EVALUATE.

*  受信状況
     EVALUATE  TBL-JYSN-KBN (P-CNT Y)
       WHEN  1
         MOVE  NC"ＯＫ"  TO  DSP-JYSN  (Y)

       WHEN  9
         MOVE  NC"ＮＧ"  TO  DSP-JYSN  (Y)

       WHEN  OTHER
         MOVE  SPACE     TO  DSP-JYSN  (Y)

     END-EVALUATE.

*  変換状況
     EVALUATE  TBL-HENKN-KBN (P-CNT Y)
       WHEN  1
         MOVE  NC"ＯＫ"  TO  DSP-HENKN  (Y)

       WHEN  9
         MOVE  NC"ＮＧ"  TO  DSP-HENKN  (Y)

       WHEN  OTHER
         MOVE  SPACE     TO  DSP-HENKN  (Y)

     END-EVALUATE.

*  更新状況
     EVALUATE  TBL-KOSIN-KBN (P-CNT Y)
       WHEN  1
         MOVE  NC"ＯＫ"  TO  DSP-KOSIN  (Y)

       WHEN  9
         MOVE  NC"ＮＧ"  TO  DSP-KOSIN  (Y)

       WHEN  OTHER
         MOVE  SPACE     TO  DSP-KOSIN  (Y)

     END-EVALUATE.

*  受信件数
     MOVE  TBL-MJKEN(P-CNT Y)  TO  DSP-MJKEN(Y).
*  伝票枚数
     MOVE  TBL-MDKEN(P-CNT Y)  TO  DSP-MDKEN(Y).
*  受信状態
**     EVALUATE TBL-MKEKA(P-CNT Y)
**       WHEN   0  MOVE NC"未受信"       TO  DSP-MKEKA(Y)
**       WHEN   1  MOVE NC"受信中"       TO  DSP-MKEKA(Y)
**       WHEN   2  MOVE NC"変換中"       TO  DSP-MKEKA(Y)
**       WHEN   3  MOVE NC"伝票更新中"   TO  DSP-MKEKA(Y)
**       WHEN   4  MOVE NC"受信完了"     TO  DSP-MKEKA(Y)
**       WHEN   5  MOVE NC"送信Ｆ無し"   TO  DSP-MKEKA(Y)
**       WHEN  50  MOVE NC"パラ作成Ｅ"   TO  DSP-MKEKA(Y)
**       WHEN  51  MOVE NC"待受ＪＢＥ"   TO  DSP-MKEKA(Y)
**       WHEN  52  MOVE NC"待受ＰＧＥ"   TO  DSP-MKEKA(Y)
**       WHEN  53  MOVE NC"ＤＴ存在無"   TO  DSP-MKEKA(Y)
**       WHEN  54  MOVE NC"ＴＩＭＥＥ"   TO  DSP-MKEKA(Y)
**       WHEN  55  MOVE NC"ＤＴＢＫＥ"   TO  DSP-MKEKA(Y)
**       WHEN  56  MOVE NC"受信ＦＣＥ"   TO  DSP-MKEKA(Y)
**       WHEN  57  MOVE NC"受信確認Ｅ"   TO  DSP-MKEKA(Y)
**       WHEN  58  MOVE NC"変換パラＥ"   TO  DSP-MKEKA(Y)
**       WHEN  59  MOVE NC"変換ＰＧＥ"   TO  DSP-MKEKA(Y)
**       WHEN  60  MOVE NC"更新時間Ｅ"   TO  DSP-MKEKA(Y)
**       WHEN  61  MOVE NC"Ｐ強制終了"   TO  DSP-MKEKA(Y)
**       WHEN  62  MOVE NC"受信ＤＴ無"   TO  DSP-MKEKA(Y)
**       WHEN  63  MOVE NC"変換起動Ｅ"   TO  DSP-MKEKA(Y)
**       WHEN  64  MOVE NC"ＰＣ受信中"   TO  DSP-MKEKA(Y)
**       WHEN  65  MOVE NC"取引先ＣＥ"   TO  DSP-MKEKA(Y)
**       WHEN  66  MOVE NC"ＰＣ確認全"   TO  DSP-MKEKA(Y)
**       WHEN  70  MOVE NC"パラＦ異常"   TO  DSP-MKEKA(Y)
**       WHEN  71  MOVE NC"変換起動Ｅ"   TO  DSP-MKEKA(Y)
**       WHEN  72  MOVE NC"件数Ｌ　Ｅ"   TO  DSP-MKEKA(Y)
**       WHEN  73  MOVE NC"回線Ｃ　Ｅ"   TO  DSP-MKEKA(Y)
**       WHEN  74  MOVE NC"変換時間Ｅ"   TO  DSP-MKEKA(Y)
**       WHEN  80  MOVE NC"パラ作成Ｅ"   TO  DSP-MKEKA(Y)
**       WHEN  81  MOVE NC"パラ送信Ｅ"   TO  DSP-MKEKA(Y)
**       WHEN  82  MOVE NC"Ｋ起動　Ｅ"   TO  DSP-MKEKA(Y)
**       WHEN  83  MOVE NC"パラ取込Ｅ"   TO  DSP-MKEKA(Y)
**       WHEN  84  MOVE NC"相手話中　"   TO  DSP-MKEKA(Y)
**       WHEN  85  MOVE NC"受信エラー"   TO  DSP-MKEKA(Y)
**       WHEN  86  MOVE NC"受信時間Ｅ"   TO  DSP-MKEKA(Y)
**       WHEN  87  MOVE NC"受信転送Ｅ"   TO  DSP-MKEKA(Y)
**       WHEN  88  MOVE NC"パラ送信Ｅ"   TO  DSP-MKEKA(Y)
**       WHEN  89  MOVE NC"ＧＰＪＢＥ"   TO  DSP-MKEKA(Y)
**       WHEN  90  MOVE NC"話中終了"     TO  DSP-MKEKA(Y)
**       WHEN  91  MOVE NC"送信Ｆ無し"   TO  DSP-MKEKA(Y)
**       WHEN  92  MOVE NC"変換異常"     TO  DSP-MKEKA(Y)
**       WHEN  93  MOVE NC"伝更異常"     TO  DSP-MKEKA(Y)
**       WHEN  99  MOVE NC"受信異常Ｋ"   TO  DSP-MKEKA(Y)
**     END-EVALUATE.

     MOVE  TBL-MKEKA(P-CNT Y)    TO  EMG-F01.

     READ  JSMMSGF
       INVALID
         MOVE  1                 TO  FG-JSMMSGF-INV
       NOT INVALID
         MOVE  ZERO              TO  FG-JSMMSGF-INV
     END-READ.

     IF  FG-JSMMSGF-INV = ZERO
         MOVE  EMG-F03           TO  DSP-MKEKA(Y)
     ELSE
         MOVE  SPACE             TO  DSP-MKEKA(Y)
     END-IF.

 WORK-DSPB-EXIT.
     EXIT.
****************************************************************
*             取引先マスタ読込み                     3.0       *
****************************************************************
 HTOKMS-READ-SEC       SECTION.
     MOVE  "HTOKMS-READ-SEC"  TO  S-NAME.

     READ HTOKMS  INVALID
          MOVE    "INV"     TO    HTOKMS-INV-FLG
          NOT  INVALID
          MOVE    SPACE     TO    HTOKMS-INV-FLG
     END-READ.
*
 HTOKMS-READ-EXIT.
     EXIT.
****************************************************************
*    照会画面・行選択  （ PSW = 3 ）             2.4
****************************************************************
 DSP-SELNO-SEC          SECTION.
     MOVE  "DSP-SELNO-SEC"   TO  S-NAME.

     PERFORM  DSP-WRITE-SEC.
     PERFORM  DSP-READ-SEC.

     EVALUATE  DSP-FNC
    *> 実行
       WHEN  "E000"
         PERFORM  SELNO-CHK-SEC

    *> 取消
       WHEN  "F004"
         MOVE  "1"          TO  PSW

    *> 終了
       WHEN  "F005"
         MOVE  "END"        TO  END-FLG

    *> 項目戻り
       WHEN  "F006"
         MOVE  "2"          TO  PSW

* 2011/10/13,S  S.I/NAV
       WHEN  "F007"  *> 発注集計表発行取引先選択
         PERFORM  HACSYU-ST-SEC
* 2011/10/13,E  S.I/NAV

    *> 前頁
       WHEN  "F011"
         COMPUTE C-CNT = P-CNT - 1
         IF  C-CNT = ZERO
             MOVE  3        TO  ERR-FLG
         ELSE
             COMPUTE P-CNT = P-CNT - 1
             PERFORM  DSP-INIT-SEC
             PERFORM  WORK-DSP-SEC
         END-IF

    *> 次頁
       WHEN  "F012"
         MOVE  P-CNT        TO  S-CNT
         COMPUTE C-CNT = P-CNT + 1
         IF  C-CNT > 8
             MOVE  4        TO  ERR-FLG
         ELSE
             IF  TBL-MTIME(C-CNT 1) = ZERO
                 MOVE  4    TO  ERR-FLG
             ELSE
                 COMPUTE P-CNT = P-CNT + 1
                 PERFORM DSP-INIT-SEC
                 PERFORM WORK-DSP-SEC
             END-IF
         END-IF

       WHEN  OTHER
         MOVE  2            TO  ERR-FLG

     END-EVALUATE.

 DSP-SELNO-EXIT.
     EXIT.
****************************************************************
*             選択行情報チェック                               *
****************************************************************
 SELNO-CHK-SEC         SECTION.
     MOVE  "SELNO-CHK-SEC"  TO  S-NAME.

     PERFORM  SELNO-CHKB-SEC.
     IF  ERR-FLG NOT = ZERO
         GO TO  SELNO-CHK-EXIT
     END-IF.

     EVALUATE  TRUE
       WHEN  (   DSP-SYOKBN = SPACE
              OR DSP-SYOKBN = ZERO)
         AND (   DSP-SYOGYO = SPACE
              OR DSP-SYOGYO = ZERO)  *> 未指定時
         CONTINUE

       WHEN  DSP-SYOKBN = 1  *> 受信件数照会
         MOVE  "1"               TO  PA-SNJ0910I-SYORI
         MOVE  TBL-JDATE         TO  PA-SNJ0910I-JYSNBI
         MOVE  TBL-MTIME (P-CNT DSP-SYOGYO)
                                 TO  PA-SNJ0910I-JYSNTM
         MOVE  TBL-MTOKCD(P-CNT DSP-SYOGYO)
                                 TO  PA-SNJ0910I-TORCD
         CALL "SNJ0910I"  USING PA-SNJ0910I-AREA

*      全画面クリア（呼んだPGの画面表示をクリアする)
         MOVE  "CL"              TO  DSP-PRO
         MOVE  "SCREEN"          TO  DSP-GRP
         MOVE  "FNJ0900I"        TO  DSP-FMT
         WRITE  DSP-FNJ0900I
*   　 通常出力指定に戻す。
         MOVE  SPACE             TO  DSP-PRO

       WHEN  DSP-SYOKBN = 2  *> 受信結果照会
         MOVE  TBL-JDATE         TO  PA-SNJ0920I-JYSNBI
         MOVE  TBL-MTIME (P-CNT DSP-SYOGYO)
                                 TO  PA-SNJ0920I-JYSNTM
         MOVE  TBL-MTOKCD(P-CNT DSP-SYOGYO)
                                 TO  PA-SNJ0920I-TORCD
         MOVE  TBL-DSYUBT(P-CNT DSP-SYOGYO)
                                 TO  PA-SNJ0920I-DSYUBT
         MOVE  TBL-MKEKA (P-CNT DSP-SYOGYO)
                                 TO  PA-SNJ0920I-ERMSGNO
         CALL "SNJ0920I"  USING PA-SNJ0920I-AREA

*      全画面クリア（呼んだPGの画面表示をクリアする)
         MOVE  "CL"              TO  DSP-PRO
         MOVE  "SCREEN"          TO  DSP-GRP
         MOVE  "FNJ0900I"        TO  DSP-FMT
         WRITE  DSP-FNJ0900I
*      通常出力指定に戻す。
         MOVE  SPACE             TO  DSP-PRO

       WHEN  OTHER
         CONTINUE

     END-EVALUATE.

 SELNO-CHK-EXIT.
     EXIT.
****************************************************************
*             選択行情報チェック                               *
****************************************************************
 SELNO-CHKB-SEC        SECTION.
     MOVE  "SELNO-CHKB-SEC"  TO  S-NAME.

     MOVE  SPACE  TO  DSP-HCHYO (1:).
     MOVE  SPACE  TO  DSP-HCTORN.
     MOVE  "M"    TO  EDIT-OPTION OF DSP-HCHYO.
     MOVE  SPACE  TO  EDIT-CURSOR OF DSP-HCHYO.
     MOVE  "M"    TO  EDIT-OPTION OF DSP-SYOKBN.
     MOVE  SPACE  TO  EDIT-CURSOR OF DSP-SYOKBN.
     MOVE  "M"    TO  EDIT-OPTION OF DSP-SYOGYO.
     MOVE  SPACE  TO  EDIT-CURSOR OF DSP-SYOGYO.

     IF      (   DSP-SYOKBN = SPACE
              OR DSP-SYOKBN = ZERO)
         AND (   DSP-SYOGYO = SPACE
              OR DSP-SYOGYO = ZERO)

         IF  DSP-SYOKBN = ZERO
             MOVE  SPACE         TO  DSP-SYOKBN (1:1)
         END-IF

         IF  DSP-SYOGYO = ZERO
             MOVE  SPACE         TO  DSP-SYOGYO (1:2)
         END-IF

         GO TO  SELNO-CHKB-EXIT

     END-IF.

     IF  DSP-SYOKBN = ZERO
         MOVE  SPACE             TO  DSP-SYOKBN (1:1)
     END-IF.

     IF  DSP-SYOGYO = ZERO
         MOVE  SPACE             TO  DSP-SYOGYO (1:2)
     END-IF.

     IF      DSP-SYOKBN  IS NUMERIC
         AND (DSP-SYOKBN = 1 OR 2)
         CONTINUE
     ELSE
         IF  ERR-FLG = ZERO
             MOVE  6             TO  ERR-FLG
         END-IF

         MOVE  "C"               TO  EDIT-CURSOR OF DSP-SYOKBN
         MOVE  "R"               TO  EDIT-OPTION OF DSP-SYOKBN
     END-IF.

     IF      DSP-SYOGYO  IS NUMERIC
         AND (DSP-SYOGYO >= 1 AND <= 15)
** 2010/11/18 CHG ↓
         IF  ( DSP-MTIME (DSP-SYOGYO)  = SPACE ) OR
             ( DSP-MTIME (DSP-SYOGYO)  = ZERO  )
              MOVE   "7"         TO  ERR-FLG
              MOVE  "C"          TO  EDIT-CURSOR OF DSP-SYOGYO
              MOVE  "R"          TO  EDIT-OPTION OF DSP-SYOGYO
         END-IF
** 2010/11/18 CHG ↑
     ELSE
         IF  ERR-FLG = ZERO
             MOVE  7             TO  ERR-FLG
         END-IF
         MOVE  "C"               TO  EDIT-CURSOR OF DSP-SYOGYO
         MOVE  "R"               TO  EDIT-OPTION OF DSP-SYOGYO
     END-IF.

 SELNO-CHKB-EXIT.
     EXIT.
****************************************************************
*  発注集計表遷移チェック                                      *
****************************************************************
 HACSYU-ST-SEC         SECTION.
     MOVE  "HACSYU-ST-SEC"  TO  S-NAME.

     PERFORM  SELNO-CHKB-SEC.
     IF  ERR-FLG NOT = ZERO
         GO TO  HACSYU-ST-EXIT
     END-IF.

     MOVE  4                TO  PSW.

 HACSYU-ST-EXIT.
     EXIT.
****************************************************************
*  発注集計表・行選択  ( PSW = 4 ) 2011/10/13 追加
****************************************************************
 DSP-HACSYU-SEC             SECTION.
     MOVE  "DSP-HACSYU-SEC" TO  S-NAME.

     PERFORM  DSP-WRITE-SEC.
     PERFORM  DSP-READ-SEC.

     EVALUATE  DSP-FNC
       WHEN  "E000"  *> 実行
         PERFORM  HACSYU-SEC

       WHEN  "F004"  *> 取消
         PERFORM  DSP-INIT-SEC
         MOVE  "3"          TO  PSW

       WHEN  "F006" *> 項目戻り
         MOVE  "3"          TO  PSW

       WHEN  "F005"  *> 終了
         MOVE  "END"        TO  END-FLG

    *> 前頁
       WHEN  "F011"
         COMPUTE C-CNT = P-CNT - 1
         IF  C-CNT = ZERO
             MOVE  3        TO  ERR-FLG
         ELSE
             COMPUTE P-CNT = P-CNT - 1
             PERFORM  DSP-INIT-SEC
             PERFORM  WORK-DSP-SEC
         END-IF

    *> 次頁
       WHEN  "F012"
         MOVE  P-CNT        TO  S-CNT
         COMPUTE C-CNT = P-CNT + 1
         IF  C-CNT > 8
             MOVE  4        TO  ERR-FLG
         ELSE
             IF  TBL-MTIME(C-CNT 1) = ZERO
                 MOVE  4    TO  ERR-FLG
             ELSE
                 COMPUTE P-CNT = P-CNT + 1
                 PERFORM DSP-INIT-SEC
                 PERFORM WORK-DSP-SEC
             END-IF
         END-IF
       WHEN  OTHER
         MOVE  2            TO  ERR-FLG

     END-EVALUATE.

 DSP-HACSYU-EXIT.
     EXIT.
****************************************************************
*  発注集計表処理選択  2011/10/13 追加
****************************************************************
 HACSYU-SEC            SECTION.
     MOVE  "HACSYU-SEC"     TO  S-NAME.

     PERFORM  HACSYU-CHK-SEC.
     IF      ERR-FLG       = ZERO
         AND FG-MINYURYOKU = ZERO
         MOVE  8            TO  ERR-FLG
         MOVE  "5"          TO  PSW
     END-IF.

 HACSYU-EXIT.
     EXIT.
****************************************************************
*  発注集計表処理選択チェック   2011/10/13 追加
****************************************************************
 HACSYU-CHK-SEC            SECTION.
     MOVE  "HACSYU-CHK-SEC" TO  S-NAME.

     MOVE  ZERO             TO  FG-MINYURYOKU.

     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-HCHYO.
     MOVE  "M"              TO  EDIT-OPTION OF DSP-HCHYO.

     IF     DSP-HCHYO NOT NUMERIC
        OR  DSP-HCHYO = ZERO
        MOVE  1             TO  FG-MINYURYOKU
        GO TO  HACSYU-CHK-EXIT
     END-IF.

     IF DSP-HCHYO >= 1 AND <= 15
        CONTINUE
     ELSE
        MOVE   7            TO  ERR-FLG
        MOVE  "C"           TO  EDIT-CURSOR OF DSP-HCHYO
        MOVE  "R"           TO  EDIT-OPTION OF DSP-HCHYO
        GO TO  HACSYU-CHK-EXIT
     END-IF.

     IF    DSP-MTOKCD (DSP-HCHYO) NOT NUMERIC
        OR DSP-MTOKCD (DSP-HCHYO) = ZERO
        MOVE  7             TO  ERR-FLG
        MOVE  "C"           TO  EDIT-CURSOR OF DSP-HCHYO
        MOVE  "R"           TO  EDIT-OPTION OF DSP-HCHYO
        GO TO  HACSYU-CHK-EXIT
     END-IF.

     MOVE  SPACE            TO  WK-HCSYU-KBN.
     MOVE  DSP-MTOKCD (DSP-HCHYO)  TO  TOK-F01.

     PERFORM  HTOKMS-READ-SEC.
     IF  HTOKMS-INV-FLG = SPACE
         MOVE  TOK-F03      TO  DSP-HCTORN
       *>発注集計表区分
         MOVE  TOK-FIL1(2:1) TO  WK-HCSYU-KBN
     ELSE
         MOVE  ALL NC"＊"   TO  DSP-HCTORN
         MOVE  7            TO  ERR-FLG
         MOVE  "C"          TO  EDIT-CURSOR OF DSP-HCHYO
         MOVE  "R"          TO  EDIT-OPTION OF DSP-HCHYO
     END-IF.

 HACSYU-CHK-EXIT.
     EXIT.
****************************************************************
*  確認処理  ( PSW = 5 )                                       *
****************************************************************
 DSP-KAKU-SEC          SECTION.
     MOVE  "DSP-KAKU-SEC"  TO  S-NAME.
     PERFORM  DSP-WRITE-SEC.
     PERFORM  DSP-READ-SEC.

     EVALUATE  DSP-FNC
       WHEN  "E000"  *> 実行
         PERFORM  HACSYU-SENI-SEC

       WHEN  "F004"  *> 取消
         PERFORM  DSP-INIT-SEC
         MOVE  "4"          TO  PSW

       WHEN  "F005"  *> 終了
         MOVE  "END"        TO  END-FLG

       WHEN  "F006" *> 項目戻り
         MOVE  "4"          TO  PSW

       WHEN  OTHER
         MOVE  2            TO  ERR-FLG

     END-EVALUATE.
 DSP-KAKU-EXIT.
     EXIT.
****************************************************************
*  発注集計表処理遷移   2011/10/13 追加
****************************************************************
 HACSYU-SENI-SEC            SECTION.
     MOVE  "HACSYU-SENI-SEC" TO  S-NAME.

* TOK-FIL1(10:1) : 発注集計表出力区分
     MOVE  DSP-JDATE1       TO  WK-JDATE-Y.
     MOVE  DSP-JDATE2       TO  WK-JDATE-M.
     MOVE  DSP-JDATE3       TO  WK-JDATE-D.
     MOVE  WK-JDATE                TO  PA-PSSY0005-JYSNBI.
     MOVE  DSP-MTIME  (DSP-HCHYO)  TO  PA-PSSY0005-JYSNTM.
     MOVE  DSP-MTOKCD (DSP-HCHYO)  TO  PA-PSSY0005-TORCD.
     MOVE  "ON"                    TO  PA-PSSY0005-KBN.

     IF  WK-HCSYU-KBN = "1"  *>発注集計表(税込)
         CALL "PSSY0038"  USING
               PA-PSSY0005-AREA
*      全画面クリア（呼んだPGの画面表示をクリアする)
         MOVE  "CL"              TO  DSP-PRO
         MOVE  "SCREEN"          TO  DSP-GRP
         MOVE  "FNJ0900I"        TO  DSP-FMT
         WRITE  DSP-FNJ0900I
*      通常出力指定に戻す。
         MOVE  SPACE             TO  DSP-PRO
     ELSE                      *>発注集計表
         CALL "PSSY0005"  USING
               PA-PSSY0005-AREA
*      全画面クリア（呼んだPGの画面表示をクリアする)
         MOVE  "CL"              TO  DSP-PRO
         MOVE  "SCREEN"          TO  DSP-GRP
         MOVE  "FNJ0900I"        TO  DSP-FMT
         WRITE  DSP-FNJ0900I
*      通常出力指定に戻す。
         MOVE  SPACE             TO  DSP-PRO
     END-IF.
     MOVE   9           TO  ERR-FLG.
     PERFORM  HACHU-GET-SEC.
 HACSYU-SENI-EXIT.
     EXIT.
****************************************************************
*    排他制御ファイルクリア                                    *
****************************************************************
 HACHU-GET-SEC          SECTION.
     MOVE  "HACHU-GET-SEC" TO  S-NAME.
     MOVE  PARA-WKSTN         TO   LOC-F01
     READ  HACHLOCF
           INVALID KEY
              GO  TO          MAIN-EXIT
           NOT INVALID KEY
           DELETE          HACHLOCF
           MOVE   11          TO  ERR-FLG
     END-READ

 HACHU-GET-EXIT.
     EXIT.
****************************************************************
*             終了処理                               3.0       *
****************************************************************
 END-SEC               SECTION.
     MOVE  "END-SEC"        TO  S-NAME.
* ファイル ＣＬＯＳＥ
     CLOSE  DSPFILE  JSMDAYF  HTOKMS  JSMMSGF HACHLOCF.

 END-EXIT.
     EXIT.
*******************< PROGRAM-END SNJ0900I >*********************
