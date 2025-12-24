# SNJ0070I

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SNJ0070I.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　受配信管理システム　　　　　　　　*
*    業務名　　　　　　　：　ＥＯＳ管理　　　　　　　　        *
*    モジュール名　　　　：　基本スケジュールマスタ保守        *
*    作成日／更新日　　　：　2010/08/12                        *
*    作成者／更新者　　　：　ＮＡＶ飯田                        *
*    処理概要　　　　　　：　　　　　　　　　　　　　　　　　　*
*                                                              *
*    更新日／更新者　　　：　2012/10/31                        *
*    修正概要　　　　　　：　流通ＢＭＳ対応　　　　　　　　　　*
****************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SNJ0070I.
 AUTHOR.               S.I.
 DATE-WRITTEN.         2010/08/121.
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
*基本スケジュールマスタ
     SELECT  JSMKIHF   ASSIGN    TO        DA-01-VI-JSMKIHL1
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      DYNAMIC
                       RECORD    KEY       KIH-F01
                                           KIH-F02
                       FILE      STATUS    KIH-ST.
*ＥＤＩ管理マスタ
     SELECT  JSMEDIF   ASSIGN    TO        DA-01-VI-JSMEDIL1
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      RANDOM
                       RECORD    KEY       EDI-F01
                                           EDI-F02
                                           EDI-F03
                       FILE      STATUS    EDI-ST.
*取引先マスタ
     SELECT  HTOKMS    ASSIGN    TO        DA-01-VI-TOKMS2
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      RANDOM
                       RECORD    KEY       TOK-F01
                       FILE      STATUS    TOK-ST.
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
*    FILE = 基本スケジュールマスタ                             *
****************************************************************
 FD  JSMKIHF
                       LABEL     RECORD    IS   STANDARD.
                       COPY      JSMKIHF   OF   XFDLIB
                       JOINING   KIH       AS   PREFIX.
****************************************************************
*    FILE = ＥＤＩ管理マスタ                                   *
****************************************************************
 FD  JSMEDIF
                       LABEL     RECORD    IS   STANDARD.
                       COPY      JSMEDIF   OF   XFDLIB
                       JOINING   EDI       AS   PREFIX.
****************************************************************
*    FILE = 取引先マスタ                                       *
****************************************************************
 FD  HTOKMS
                       BLOCK     CONTAINS  8    RECORDS
                       LABEL     RECORD    IS   STANDARD.
                       COPY      HTOKMS    OF   XFDLIB
                       JOINING   TOK       AS   PREFIX.
****************************************************************
*    FILE = 画面ファイル                                       *
****************************************************************
 FD  DSPFILE
                       LABEL     RECORD    IS   OMITTED.
                       COPY      FNJ0070I  OF   XMDLIB
                       JOINING   DSP       AS   PREFIX.
*
****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
*ステータス領域
 01  STATUS-AREA.
     03  KIH-ST                   PIC  X(02).
     03  EDI-ST                   PIC  X(02).
     03  TOK-ST                   PIC  X(02).
     03  DSP-ST                   PIC  X(02).
*画面制御用領域
 01  DSP-CONTROL.
     03  DSP-FMT                  PIC  X(08).
     03  DSP-GRP                  PIC  X(08).
     03  DSP-PRO                  PIC  X(02).
     03  DSP-FNC                  PIC  X(04).
*フラグ領域
 01  FLG-AREA.
     03  JSMEDIF-INV-FLG          PIC  X(03)  VALUE  SPACE.
     03  HTOKMS-INV-FLG           PIC  X(03)  VALUE  SPACE.
     03  CHK-FLG                  PIC  X(03)  VALUE  SPACE.
     03  ERR-FLG                  PIC  9(02)  VALUE  ZERO.
     03  END-FLG                  PIC  X(03)  VALUE  SPACE.
     03  RD-FLG                   PIC  X(03)  VALUE  SPACE.
     03  GYO-CNT                  PIC  9(02)  VALUE  ZERO.
     03  CHK-CNT                  PIC  9(01)  VALUE  ZERO.
     03  CHK-MEI                  PIC  9(02)  VALUE  ZERO.
     03  CHK-MEI2                 PIC  9(02)  VALUE  ZERO.
     03  P-CNT                    PIC  9(01)  VALUE  ZERO.
     03  S-CNT                    PIC  9(01)  VALUE  ZERO.
     03  C-CNT                    PIC  9(01)  VALUE  ZERO.
     03  X                        PIC  9(02)  VALUE  ZERO.
     03  Y                        PIC  9(02)  VALUE  ZERO.
     03  FG-HANUKE                PIC  9(01)  VALUE  ZERO.
     03  FG-DUPLICATE             PIC  9(01)  VALUE  ZERO.
     03  CT-TBLMAX                PIC  9(03)  VALUE  ZERO.
     03  WK-IX                    PIC  9(03)  VALUE  ZERO.
     03  WK-IX2                   PIC  9(03)  VALUE  ZERO.
     03  WK-IX3                   PIC  9(03)  VALUE  ZERO.
     03  IX-JIGMN-PG              PIC  9(03)  VALUE  ZERO.
     03  IX-JIGMN-GYO             PIC  9(03)  VALUE  ZERO.
     03  CT-TBLPG-MAX             PIC  9(02)  VALUE  ZERO.
     03  WK-AMARI                 PIC  9(02)  VALUE  ZERO.
*ワーク領域
 01  WRK-AREA.
***  プログラムスイッチ（画面遷移制御）
     03  PSW                      PIC  X(01)  VALUE  SPACE.
*
 01  WRK-YOBI                     PIC  9(01)  VALUE  ZERO.
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
*
*ＰＦガイド
 01  PF-MSG-AREA.
     03  PF-MSG1.
         05  FILLER               PIC  N(20)  VALUE
             NC"_取消　_終了　　　　　　　　　　　　　".
         05  FILLER               PIC  N(10)  VALUE
             NC"　　　　　　　　　　".
     03  PF-MSG2.
         05  FILLER               PIC  N(20)  VALUE
             NC"_取消　_終了　_項目戻し　_前頁　_次".
         05  FILLER               PIC  N(10)  VALUE
             NC"頁　　　　　　　　　".
 01  PF-MSG-AREA-R  REDEFINES PF-MSG-AREA.
     03  PF-MSG-R  OCCURS 2       PIC  N(30).
*
*メッセージの取得
 01  ERR-MSG-AREA.
     03  ERR-MSG1.
         05  FILLER              PIC   N(20)
             VALUE NC"曜日を入力して下さい。".
     03  ERR-MSG2.
         05  FILLER              PIC   N(20)
             VALUE NC"時間を入力して下さい。".
     03  ERR-MSG3.
         05  FILLER              PIC   N(20)
             VALUE NC"取引先コードを入力して下さい。".
     03  ERR-MSG4.
         05  FILLER              PIC   N(20)
             VALUE NC"無効キーです。".
     03  ERR-MSG5.
         05  FILLER              PIC   N(20)
             VALUE NC"前頁はありません。　　　".
     03  ERR-MSG6.
         05  FILLER              PIC   N(20)
             VALUE NC"次頁はありません。　　　".
     03  ERR-MSG7.
         05  FILLER              PIC   N(20)
             VALUE NC"受信時間論理エラー。　　".
     03  ERR-MSG8.
         05  FILLER              PIC   N(20)
             VALUE NC"分は、３０分単位で入力して下さい。".
     03  ERR-MSG9.
         05  FILLER              PIC   N(20)
             VALUE NC"明細を入力して下さい。　　　　　　".
     03  ERR-MSG10.
         05  FILLER              PIC   N(20)
             VALUE NC"行を詰めて入力して下さい　　　　　".
     03  ERR-MSG11.
         05  FILLER              PIC   N(20)
             VALUE NC"時間が重複しています。　　　　　　".
     03  ERR-MSG12.
         05  FILLER              PIC   N(20)
             VALUE NC"他ページの時間が重複しています。　".
     03  ERR-MSG13.
         05  FILLER              PIC   N(20)
             VALUE NC"取引先コードに誤りがあります。　　　　　".
     03  ERR-MSG14.
         05  FILLER              PIC   N(20)
             VALUE NC"データ区分に誤りがあります。　　　　　　".
     03  ERR-MSG15.
         05  FILLER              PIC   N(20)
             VALUE NC"ＥＤＩ管理Ｍにデータ区分が未登録です。　".
     03  ERR-MSG16.
         05  FILLER              PIC   N(20)
             VALUE NC"　　　　　　　　　　　　　　　　　　　　".
     03  ERR-MSG17.
         05  FILLER              PIC   N(20)
             VALUE NC"　　　　　　　　　　　　　　　　　　　　".
     03  ERR-MSG18.
         05  FILLER              PIC   N(20)
             VALUE NC"　　　　　　　　　　　　　　　　　　　　".
     03  ERR-MSG19.
         05  FILLER              PIC   N(20)
             VALUE NC"　　　　　　　　　　　　　　　　　　　　".
     03  ERR-MSG20.
         05  FILLER              PIC   N(20)
             VALUE NC"　　　　　　　　　　　　　　　　　　　　".
 01  ERR-MSG-AREA-R      REDEFINES     ERR-MSG-AREA.
     03  ERR-MSG-R   OCCURS  20   PIC   N(20).
*基本スケジュール退避エリア
 01  TABLE-AREA.
     03  TBL-YOUBI                PIC   9(01).

     03  TABLE-G.
       05  TABLE1      OCCURS  6.
           07  TABLE2  OCCURS  15.
               09  TBL-GYO          PIC   9(02).
               09  TBL-JIKAN        PIC   9(04).
               09  TBL-TOKCD        PIC   9(08).
               09  TBL-SEN          PIC   X(01).
               09  TBL-DTKBN        PIC   X(02).
               09  TBL-TOROKU-TANT  PIC   X(02).
               09  TBL-TOROKUBI     PIC   9(08).
               09  TBL-TOROKUTM     PIC   9(04).
               09  TBL-KOSIN-TANT   PIC   X(02).
               09  TBL-KOSINBI      PIC   9(08).
               09  TBL-KOSINTM      PIC   9(04).
     03  TABLE-G-R  REDEFINES TABLE-G.
         05  TABLE2-R  OCCURS  90.
           07  TBL-GYO-R            PIC   9(02).
           07  TBL-JIKAN-R          PIC   9(04).
           07  TBL-TOKCD-R          PIC   9(08).
           07  TBL-SEN-R            PIC   X(01).
           07  TBL-DTKBN-R          PIC   X(02).
           07  TBL-TOROKU-TANT-R    PIC   X(02).
           07  TBL-TOROKUBI-R       PIC   9(08).
           07  TBL-TOROKUTM-R       PIC   9(04).
           07  TBL-KOSIN-TANT-R     PIC   X(02).
           07  TBL-KOSINBI-R        PIC   9(08).
           07  TBL-KOSINTM-R        PIC   9(04).

     03  TABLE-G-INI.
       05  TABLE1-INI      OCCURS  6.
           07  TABLE2-INI  OCCURS  15.
               09  TBL-GYO-INI          PIC   9(02).
               09  TBL-JIKAN-INI        PIC   9(04).
               09  TBL-TOKCD-INI        PIC   9(08).
               09  TBL-SEN-INI          PIC   X(01).
               09  TBL-DTKBN-INI        PIC   X(02).
               09  TBL-TOROKU-TANT-INI  PIC   X(02).
               09  TBL-TOROKUBI-INI     PIC   9(08).
               09  TBL-TOROKUTM-INI     PIC   9(04).
               09  TBL-KOSIN-TANT-INI   PIC   X(02).
               09  TBL-KOSINBI-INI      PIC   9(08).
               09  TBL-KOSINTM-INI      PIC   9(04).
     03  TABLE-G-INI-R  REDEFINES TABLE-G-INI.
         05  TABLE2-INI-R  OCCURS  90.
           07  TBL-GYO-INI-R         PIC   9(02).
           07  TBL-JIKAN-INI-R       PIC   9(04).
           07  TBL-TOKCD-INI-R       PIC   9(08).
           07  TBL-SEN-INI-R         PIC   X(01).
           07  TBL-DTKBN-INI-R       PIC   X(02).
           07  TBL-TOROKU-TANT-INI-R PIC   X(02).
           07  TBL-TOROKUBI-INI-R    PIC   9(08).
           07  TBL-TOROKUTM-INI-R    PIC   9(04).
           07  TBL-KOSIN-TANT-INI-R  PIC   X(02).
           07  TBL-KOSINBI-INI-R     PIC   9(08).
           07  TBL-KOSINTM-INI-R     PIC   9(04).

     03  TABLE-G-BK.
       05  TABLE1-BK      OCCURS  6.
           07  TABLE2-BK  OCCURS  15.
               09  TBL-GYO-BK          PIC   9(02).
               09  TBL-JIKAN-BK        PIC   9(04).
               09  TBL-TOKCD-BK        PIC   9(08).
               09  TBL-SEN-BK          PIC   X(01).
               09  TBL-DTKBN-BK        PIC   X(02).
               09  TBL-TOROKU-TANT-BK  PIC   X(02).
               09  TBL-TOROKUBI-BK     PIC   9(08).
               09  TBL-TOROKUTM-BK     PIC   9(04).
               09  TBL-KOSIN-TANT-BK   PIC   X(02).
               09  TBL-KOSINBI-BK      PIC   9(08).
               09  TBL-KOSINTM-BK      PIC   9(04).
     03  TABLE-G-BK-R  REDEFINES TABLE-G-BK.
         05  TABLE2-BK-R  OCCURS  90.
           07  TBL-GYO-BK-R         PIC   9(02).
           07  TBL-JIKAN-BK-R       PIC   9(04).
           07  TBL-TOKCD-BK-R       PIC   9(08).
           07  TBL-SEN-BK-R         PIC   X(01).
           07  TBL-DTKBN-BK-R       PIC   X(02).
           07  TBL-TOROKU-TANT-BK-R PIC   X(02).
           07  TBL-TOROKUBI-BK-R    PIC   9(08).
           07  TBL-TOROKUTM-BK-R    PIC   9(04).
           07  TBL-KOSIN-TANT-BK-R  PIC   X(02).
           07  TBL-KOSINBI-BK-R     PIC   9(08).
           07  TBL-KOSINTM-BK-R     PIC   9(04).

*受信時間チェック
 01  WK-JIKAN.
     03  WK-HH                    PIC   9(02)  VALUE  ZERO.
     03  WK-MM                    PIC   9(02)  VALUE  ZERO.
*
*ファイルエラーメッセージ
 01  FILE-ERR.
     03  KIH-ERR           PIC N(15) VALUE
                        NC"基本スケジュールエラー".
     03  EDI-ERR           PIC N(15) VALUE
                        NC"ＥＤＩ管理マスタエラー".
     03  TOK-ERR           PIC N(15) VALUE
                        NC"取引先マスタエラー".
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
*
****************************************************************
 LINKAGE               SECTION.
****************************************************************
 01  PAR-AREA.
     03  PAR-TANCD     PIC  X(02).
*
**************************************************************
 PROCEDURE             DIVISION   USING  PAR-AREA.
**************************************************************
 DECLARATIVES.
 KIH-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE JSMKIHF.
     MOVE        KIH-ST    TO        E-ST.
     MOVE        "JSMKIHF" TO        E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     KIH-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 EDI-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE JSMEDIF.
     MOVE        EDI-ST    TO        E-ST.
     MOVE        "JSMEDIF" TO        E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     EDI-ERR   UPON      CONS.
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
     MOVE  "PROCESS-START"       TO  S-NAME.

     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC  UNTIL  END-FLG = "END".
     PERFORM  END-SEC.

     STOP    RUN.
 CONTROL-EXIT.
     EXIT.
****************************************************************
*             初期処理                               1.0
****************************************************************
 INIT-SEC              SECTION.
     MOVE  "INIT-SEC"            TO  S-NAME.
*ファイルのＯＰＥＮ
     OPEN  I-O    JSMKIHF.
     OPEN  I-O    DSPFILE.
     OPEN  INPUT  JSMEDIF HTOKMS.
*ワークの初期化
     INITIALIZE  FLG-AREA.
*初期画面の表示
     MOVE  SPACE                 TO  DSP-PRO.
     PERFORM  INIT-DSP-SEC.
*ヘッド入力へ
     MOVE  "1"                   TO  PSW.
*
 INIT-EXIT.
     EXIT.

****************************************************************
*             初期画面表示                                     *
****************************************************************
 INIT-DSP-SEC          SECTION.
     MOVE  "INIT-DSP-SEC"        TO  S-NAME.
     PERFORM  SDATE-GET-SEC.
*画面の初期化
     MOVE  SPACE                 TO  DSP-FNJ0070I.
*システム日付転送
     MOVE  SYS-DATE              TO  DSP-SDATE.
*システム時間転送
     MOVE  WK-TIME(1:6)          TO  DSP-STIME.
*プログラムＩＤ
     MOVE  "SNJ0070I"            TO  DSP-PGMID.
*項目属性クリア
     PERFORM  DSP-SYOKI-SEC.
*
 INIT-DSP-EXIT.
     EXIT.

****************************************************************
*    システム日付取得処理                                      *
****************************************************************
 SDATE-GET-SEC          SECTION.
*システム日付・時刻の取得
     ACCEPT  WK-DATE  FROM DATE.
     MOVE  "3"                   TO  LINK-IN-KBN.
     MOVE  WK-DATE               TO  LINK-IN-YMD6.
     MOVE  ZERO                  TO  LINK-IN-YMD8.
     MOVE  ZERO                  TO  LINK-OUT-RET.
     MOVE  ZERO                  TO  LINK-OUT-YMD.
     CALL  "SKYDTCKB"  USING LINK-IN-KBN
                             LINK-IN-YMD6
                             LINK-IN-YMD8
                             LINK-OUT-RET
                             LINK-OUT-YMD.
     MOVE  LINK-OUT-YMD          TO  DATE-AREA.
     ACCEPT  WK-TIME  FROM TIME.
*
 SDATE-GET-EXIT.
     EXIT.

****************************************************************
*             画面制御項目初期化                               *
****************************************************************
 DSP-SYOKI-SEC         SECTION.
     MOVE  "DSP-SYOKI-SEC"       TO  S-NAME.
*リバース，カーソルパーク解除
***  曜日
     MOVE  "M"              TO  EDIT-OPTION OF DSP-YOUBI.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-YOUBI.
     PERFORM  VARYING X  FROM 1 BY 1
              UNTIL   X > 15
***  時間
       MOVE  "M"            TO  EDIT-OPTION OF DSP-JIKAN(X)
       MOVE  SPACE          TO  EDIT-CURSOR OF DSP-JIKAN(X)
***  取引先
       MOVE  "M"            TO  EDIT-OPTION OF DSP-TOKCD(X)
       MOVE  SPACE          TO  EDIT-CURSOR OF DSP-TOKCD(X)
     END-PERFORM.
*
 DSP-SYOKI-EXIT.
     EXIT.
****************************************************************
*             メイン処理                             2.0
****************************************************************
 MAIN-SEC              SECTION.
     MOVE  "MAIN-SEC"            TO  S-NAME.
*
     EVALUATE  PSW
*      処理区分入力(2.1)
       WHEN  "1"  PERFORM  DSP-HEAD-SEC
*      明細入力    (2.2)
       WHEN  "2"  PERFORM  DSP-BODY-SEC
*      確認入力    (2.3)
       WHEN  "3"  PERFORM  DSP-KAKU-SEC
*      以外
       WHEN  OTHER  CONTINUE
     END-EVALUATE.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*             処理区分入力( PSW = 1 )                          *
****************************************************************
 DSP-HEAD-SEC          SECTION.
     MOVE  "DSP-HEAD-SEC"        TO  S-NAME.

     PERFORM  DSP-WRITE-SEC.
     PERFORM  DSP-READ-SEC.

     EVALUATE  DSP-FNC
*      実行
       WHEN  "E000"
*2012/10/31  ADD
         MOVE  DSP-YOUBI         TO  WRK-YOBI
         PERFORM  INIT-DSP-SEC
         MOVE  WRK-YOBI          TO  DSP-YOUBI
*2012/10/31  ADD
         PERFORM  HEAD-CHK-SEC
*      取消
       WHEN  "F004"
         MOVE  "1"               TO  PSW
         PERFORM   INIT-DSP-SEC
*      終了
       WHEN  "F005"
         MOVE  "END"             TO  END-FLG
       WHEN  OTHER
         MOVE  4                 TO  ERR-FLG
     END-EVALUATE.
*
 DSP-HEAD-EXIT.
     EXIT.
****************************************************************
*             画面表示処理                                     *
****************************************************************
 DSP-WRITE-SEC         SECTION.
     MOVE  "DSP-WRITE-SEC"       TO  S-NAME.

     PERFORM  SDATE-GET-SEC.
*システム日付転送
     MOVE  SYS-DATE              TO  DSP-SDATE.
*システム時間転送
     MOVE  WK-TIME(1:6)          TO  DSP-STIME.

*エラーメッセージセット
     IF  ERR-FLG = ZERO
         MOVE  SPACE               TO  DSP-ERRMSG
     ELSE
         MOVE  ERR-MSG-R(ERR-FLG)  TO  DSP-ERRMSG
         MOVE  ZERO                TO  ERR-FLG
     END-IF.
*ガイドメッセージの設定
     EVALUATE  PSW
*      曜日
       WHEN  "1"
         MOVE  PF-MSG-R(1)       TO  DSP-PFGAID
*      明細／確認
       WHEN  "2"  WHEN  "3"
         MOVE  PF-MSG-R(2)       TO  DSP-PFGAID
     END-EVALUATE.
* 画面の表示
     MOVE  "SCREEN"              TO  DSP-GRP.
     MOVE  "FNJ0070I"            TO  DSP-FMT.
     WRITE  DSP-FNJ0070I.
     PERFORM  DSP-SYOKI-SEC.
*
 DSP-WRITE-EXIT.
     EXIT.
****************************************************************
*             画面読込処理                                     *
****************************************************************
 DSP-READ-SEC          SECTION.
     MOVE  "DSP-READ-SEC"        TO  S-NAME.
*
     MOVE  "NE"                  TO  DSP-PRO.
*
*    MOVE  "SCREEN"              TO  DSP-GRP.
     EVALUATE   PSW
*      曜日
       WHEN  "1"
         MOVE  "HEAD"            TO  DSP-GRP
*      明細
       WHEN  "2"
         MOVE  "BODY"            TO  DSP-GRP
*      確認
       WHEN  "3"
         MOVE  "KAKU"            TO  DSP-GRP
     END-EVALUATE.

     MOVE  "FNJ0070I"            TO  DSP-FMT.
     READ  DSPFILE.
*入力項目の属性を通常にする
 DSP-READ-010.
*    MOVE  ZERO                  TO  ERR-FLG.
     MOVE  SPACE                 TO  DSP-PRO.
*
 DSP-READ-EXIT.
     EXIT.
****************************************************************
*             曜日チェック                                     *
****************************************************************
 HEAD-CHK-SEC             SECTION.
     MOVE  "HEAD-CHK-SEC"        TO  S-NAME.
*曜日チェック（未入力／範囲外）
     IF     DSP-YOUBI NOT NUMERIC
         OR DSP-YOUBI = ZERO
         MOVE  1                 TO  ERR-FLG
         MOVE  "R"               TO  EDIT-OPTION  OF  DSP-YOUBI
         MOVE  "C"               TO  EDIT-CURSOR  OF  DSP-YOUBI
     ELSE
         MOVE  "M"               TO  EDIT-OPTION  OF  DSP-YOUBI
         MOVE  SPACE             TO  EDIT-CURSOR  OF  DSP-YOUBI
     END-IF.

     IF      ( DSP-YOUBI IS NUMERIC )
         AND ( DSP-YOUBI =  1 OR 2 OR 3 OR 4 OR 5 )
         MOVE  "M"               TO  EDIT-OPTION  OF  DSP-YOUBI
         MOVE  SPACE             TO  EDIT-CURSOR  OF  DSP-YOUBI
     ELSE
         MOVE  1                 TO  ERR-FLG
         MOVE  "R"               TO  EDIT-OPTION  OF  DSP-YOUBI
         MOVE  "C"               TO  EDIT-CURSOR  OF  DSP-YOUBI
     END-IF.
*エラーフラグがゼロの場合
     IF  ERR-FLG = ZERO
         PERFORM  MST-WORK-SEC   *> MST -> TBL
         MOVE  1                 TO  P-CNT
         PERFORM  WORK-DSP-SEC   *> TBL -> DSP
         MOVE  "2"               TO  PSW
     END-IF.
*
 HEAD-CHK-EXIT.
     EXIT.
****************************************************************
*             マスタ→ワーク（退避）                           *
****************************************************************
 MST-WORK-SEC          SECTION.
     MOVE  "MST-WORK-SEC"        TO  S-NAME.
*ワークテーブルクリア
     MOVE  SPACE                 TO  TABLE-AREA.
     INITIALIZE  TABLE-AREA.

*曜日のワーク退避
     MOVE  DSP-YOUBI             TO  TBL-YOUBI.
*ＳＥＱの作成
     MOVE  ZERO                  TO  GYO-CNT.
     PERFORM  VARYING X  FROM 1 BY 1
              UNTIL   X > 6
       PERFORM  VARYING Y FROM  1 BY 1
                UNTIL Y > 15
         ADD 1  TO  GYO-CNT
         MOVE  GYO-CNT           TO  TBL-GYO   (X Y)
*      時間の初期値は999、画面表示は空白とする。
         MOVE  9999              TO  TBL-JIKAN (X Y)
       END-PERFORM
     END-PERFORM.
*曜日により基本スケジュールマスタスタート
     PERFORM JSMKIHF-START-SEC.
     IF  CHK-FLG = "CHK"
         GO TO  MST-WORK-010
     END-IF.
*
*基本スケジュールマスタ読込み
     PERFORM  JSMKIHF-READ-SEC.
*基本スケジュールマスタを読込みながらワークへセット
     MOVE  ZERO                  TO  CT-TBLMAX.
     PERFORM  VARYING X  FROM 1 BY 1
              UNTIL   X > 6
                   OR DSP-YOUBI NOT = KIH-F01
                   OR RD-FLG        = "END"
       PERFORM  VARYING Y  FROM 1 BY 1
                UNTIL   Y > 15
                     OR DSP-YOUBI NOT = KIH-F01
                     OR RD-FLG        = "END"
         ADD  1   TO  CT-TBLMAX
         MOVE  KIH-F02           TO  TBL-JIKAN       (X Y)
         MOVE  KIH-F03           TO  TBL-TOKCD       (X Y)
         MOVE  KIH-F04           TO  TBL-SEN         (X Y)
         MOVE  KIH-F05           TO  TBL-DTKBN       (X Y)
         MOVE  KIH-F06           TO  TBL-TOROKU-TANT (X Y)
         MOVE  KIH-F07           TO  TBL-TOROKUBI    (X Y)
         MOVE  KIH-F08           TO  TBL-TOROKUTM    (X Y)
         MOVE  KIH-F09           TO  TBL-KOSIN-TANT  (X Y)
         MOVE  KIH-F10           TO  TBL-KOSINBI     (X Y)
         MOVE  KIH-F11           TO  TBL-KOSINTM     (X Y)
         PERFORM  JSMKIHF-READ-SEC

       END-PERFORM

     END-PERFORM.


     CLOSE  JSMKIHF.
     OPEN   I-O  JSMKIHF.
**
 MST-WORK-010.
     MOVE  TABLE-G               TO  TABLE-G-INI.

 MST-WORK-EXIT.
     EXIT.
****************************************************************
*             基本スケジュールマスタ読込み                     *
****************************************************************
 JSMKIHF-START-SEC     SECTION.
     MOVE  "JSMKIHF-START-SEC"   TO  S-NAME.
*曜日により基本スケジュールマスタスタート
     MOVE  SPACE                 TO  RD-FLG.
     MOVE  SPACE                 TO  CHK-FLG.
     MOVE  DSP-YOUBI             TO  KIH-F01.
     MOVE  ZERO                  TO  KIH-F02.
     START  JSMKIHF  KEY IS >= KIH-F01 KIH-F02
       INVALID
         MOVE  "CHK"             TO  CHK-FLG
     END-START.
*
 JSMKIHF-START-EXIT.
     EXIT.
****************************************************************
*             基本スケジュールマスタ読込み                     *
****************************************************************
 JSMKIHF-READ-SEC      SECTION.
     MOVE  "JSMKIHF-READ-SEC"    TO  S-NAME.
*マスタ読込み
     READ  JSMKIHF  NEXT
       AT END
         MOVE  "END"             TO  RD-FLG
     END-READ.
*
 JSMKIHF-READ-EXIT.
     EXIT.
****************************************************************
*             ワーク→画面表示                                 *
****************************************************************
 WORK-DSP-SEC          SECTION.
     MOVE  "WORK-DSP-SEC"        TO  S-NAME.
*項目画面セット
     MOVE  ZERO                  TO  CHK-CNT.
     MOVE  TBL-YOUBI             TO  DSP-YOUBI.
     PERFORM  VARYING Y  FROM 1 BY 1
              UNTIL   Y > 15
       MOVE    TBL-GYO(P-CNT Y)   TO DSP-GYO(Y)
       IF  TBL-JIKAN(P-CNT Y)  NOT =  9999
           IF  TBL-JIKAN(P-CNT Y) = 8888
               MOVE  SPACE               TO  DSP-JIKAN(Y)
           ELSE
               MOVE  TBL-JIKAN(P-CNT Y)  TO  DSP-JIKAN(Y)
           END-IF

           IF TBL-TOKCD(P-CNT Y) NOT = ZERO
              MOVE  TBL-TOKCD(P-CNT Y)  TO  DSP-TOKCD(Y)
           ELSE
              MOVE  SPACE               TO  DSP-TOKCD(Y)(1:8)
           END-IF
           MOVE  TBL-TOKCD(P-CNT Y)   TO  TOK-F01
           PERFORM HTOKMS-READ-SEC
           IF  HTOKMS-INV-FLG = SPACE
               MOVE  TOK-F03          TO  DSP-TOKNM(Y)
           ELSE
               MOVE  ALL NC"＊"       TO  DSP-TOKNM(Y)
           END-IF
           MOVE  TBL-DTKBN(P-CNT Y)   TO  EDI-F01
           MOVE  1                    TO  EDI-F02
           MOVE  TBL-TOKCD(P-CNT Y)   TO  EDI-F03
           PERFORM JSMEDIF-READ-SEC
           IF  JSMEDIF-INV-FLG = SPACE
               MOVE  EDI-F04           TO  DSP-SEN(Y)
               IF  EDI-F04 = "1"
                   MOVE  NC"ＩＳＤＮ"  TO  DSP-SENNM(Y)
*** 2012/10/31
************** ELSE
               END-IF
               IF  EDI-F04 = "2"
                   MOVE  NC"公衆回線"  TO  DSP-SENNM(Y)
               END-IF
               IF  EDI-F04 = "3"
                   MOVE  NC"ＢＭＳ　"  TO  DSP-SENNM(Y)
               END-IF
***
           ELSE
               MOVE  "*"               TO  DSP-SEN(Y)
               MOVE  ALL NC"＊"        TO  DSP-SENNM(Y)
           END-IF
           MOVE  TBL-DTKBN(P-CNT Y)    TO  DSP-DTKBN(Y)
           EVALUATE  TBL-DTKBN(P-CNT Y)
               WHEN  "01"  MOVE NC"発　注"   TO  DSP-DTMN(Y)
               WHEN  "02"  MOVE NC"受　領"   TO  DSP-DTMN(Y)
               WHEN  "03"  MOVE NC"支　払"   TO  DSP-DTMN(Y)
               WHEN  "04"  MOVE NC"請　求"   TO  DSP-DTMN(Y)
               WHEN  "05"  MOVE NC"出　荷"   TO  DSP-DTMN(Y)
               WHEN  "06"  MOVE NC"欠品案"   TO  DSP-DTMN(Y)
               WHEN  "07"  MOVE NC"商品台"   TO  DSP-DTMN(Y)
               WHEN  "08"  MOVE NC"その他"   TO  DSP-DTMN(Y)
*** 2012/10/31
               WHEN  "09"  MOVE NC"ＢＭＳ"   TO  DSP-DTMN(Y)
           END-EVALUATE
           ADD  1  TO  CHK-CNT
       ELSE
           MOVE  SPACE                TO  DSP-JIKAN (Y) (1:4)
           MOVE  SPACE                TO  DSP-TOKCD (Y) (1:8)
           MOVE  SPACE                TO  DSP-TOKNM (Y)
           MOVE  SPACE                TO  DSP-SEN   (Y)
           MOVE  SPACE                TO  DSP-SENNM (Y)
           MOVE  SPACE                TO  DSP-DTKBN (Y)
       END-IF
     END-PERFORM.
*
 WORK-DSP-EXIT.
     EXIT.
****************************************************************
*             取引先マスタ読込み                               *
****************************************************************
 HTOKMS-READ-SEC       SECTION.
     MOVE  "HTOKMS-READ-SEC"     TO  S-NAME.
     READ  HTOKMS
       INVALID
          MOVE "INV"             TO  HTOKMS-INV-FLG
       NOT INVALID
          MOVE SPACE             TO  HTOKMS-INV-FLG
     END-READ.
*
 HTOKMS-READ-EXIT.
     EXIT.
****************************************************************
*             ＥＤＩ管理マスタ読込み                           *
****************************************************************
 JSMEDIF-READ-SEC      SECTION.
     MOVE  "JSMEDIF-READ-SEC"    TO  S-NAME.
     READ JSMEDIF
       INVALID
         MOVE  "INV"             TO  JSMEDIF-INV-FLG
       NOT  INVALID
         MOVE  SPACE             TO  JSMEDIF-INV-FLG
     END-READ.
*
 JSMEDIF-READ-EXIT.
     EXIT.
****************************************************************
*             明細項目　入力( PSW = 2 )                        *
****************************************************************
 DSP-BODY-SEC          SECTION.
     MOVE  "DSP-BODY-SEC"        TO  S-NAME.

     PERFORM  DSP-WRITE-SEC.
     PERFORM  DSP-READ-SEC.

     EVALUATE  DSP-FNC
*      実行
       WHEN  "E000"
         PERFORM  BODY-CHK-SEC

*      取消
       WHEN  "F004"
         MOVE  1                 TO  P-CNT
         MOVE  "1"               TO  PSW
         PERFORM  INIT-DSP-SEC

*      終了
       WHEN  "F005"
         MOVE  "END"             TO  END-FLG

*      項目戻し
       WHEN  "F006"
         MOVE  "1"               TO  PSW

*      前頁
       WHEN  "F011"
         COMPUTE  C-CNT = P-CNT - 1
         IF  C-CNT = ZERO
             MOVE  5             TO  ERR-FLG
         ELSE
             PERFORM  DSP-WORK-SEC
             COMPUTE  P-CNT = P-CNT - 1
             PERFORM  INIT-DSP-SEC
             PERFORM  WORK-DSP-SEC
         END-IF

*      次頁
       WHEN  "F012"
             MOVE  P-CNT         TO  S-CNT
             COMPUTE C-CNT  = P-CNT + 1
             IF  C-CNT > 6
                 MOVE  6         TO  ERR-FLG
             ELSE
                 IF       (   DSP-JIKAN(15) = ZERO
                           OR DSP-JIKAN(15) NOT NUMERIC )
                      AND (   DSP-TOKCD(15) = ZERO
                           OR DSP-TOKCD(15) NOT NUMERIC )
**                  最大ページ以下でも、そのページの最後の行
**                  が未入力なら次ページなしとする。
                      MOVE  6    TO  ERR-FLG
                  ELSE
                      PERFORM  DSP-WORK-SEC
                      COMPUTE  P-CNT = P-CNT + 1
                      PERFORM  INIT-DSP-SEC
                      PERFORM  WORK-DSP-SEC
                  END-IF
             END-IF

       WHEN  OTHER
         MOVE  4                 TO  ERR-FLG
     END-EVALUATE.
*
 DSP-BODY-EXIT.
     EXIT.

****************************************************************
*             明細項目チェック                                 *
****************************************************************
 BODY-CHK-SEC          SECTION.
     MOVE  "BODY-CHK-SEC"        TO  S-NAME.
     MOVE  ZERO                  TO  ERR-FLG.
     MOVE  ZERO                  TO  CHK-MEI.
     MOVE  ZERO                  TO  CHK-MEI2.
     MOVE  ZERO                  TO  FG-HANUKE.

     PERFORM  VARYING Y  FROM 1 BY 1
              UNTIL   Y > 15

       PERFORM  BODY-CHK-B-SEC

     END-PERFORM

     PERFORM BODY-CHK-C-SEC.

     IF  ERR-FLG  =  ZERO
         IF      P-CNT    = 1
             AND CHK-MEI  = ZERO
             AND CHK-MEI2 = ZERO
             MOVE  9         TO  ERR-FLG
         ELSE
**           テーブル領域に設定する。
             PERFORM  DSP-WORK-SEC
             MOVE "3"        TO  PSW
         END-IF
     END-IF.

 BODY-CHK-EXIT.
     EXIT.
****************************************************************
*             明細項目チェック                                 *
****************************************************************
 BODY-CHK-B-SEC          SECTION.
     MOVE  "BODY-CHK-B-SEC"      TO  S-NAME.

     IF  TBL-JIKAN-INI (P-CNT Y) NOT = 9999
**       初期表示のデータ有り行
         ADD  1   TO  CHK-MEI
     END-IF.

     IF      TBL-JIKAN-INI (P-CNT Y) = 9999
         AND DSP-JIKAN (Y)           = SPACE
**       画面のデータ無し行
         MOVE  1            TO  FG-HANUKE
         MOVE  SPACE        TO  DSP-JIKAN (Y)(1:4)
         MOVE  SPACE        TO  DSP-TOKCD (Y)(1:8)
         MOVE  SPACE        TO  DSP-SEN   (Y)
         MOVE  SPACE        TO  DSP-TOKNM (Y)
         MOVE  SPACE        TO  DSP-SENNM (Y)
         GO TO  BODY-CHK-B-EXIT
     ELSE
**       データ有り行
         ADD  1   TO  CHK-MEI2
     END-IF.
*
*時間
     IF  DSP-JIKAN(Y)  IS NUMERIC
         MOVE  DSP-JIKAN(Y)      TO  WK-JIKAN
         IF     WK-HH <= ZERO OR WK-HH > 24
             OR WK-MM <  ZERO OR WK-MM > 59
             IF  ERR-FLG = ZERO
                 MOVE  7         TO  ERR-FLG
             END-IF
                 MOVE  "R"       TO  EDIT-OPTION OF DSP-JIKAN(Y)
                 MOVE  "C"       TO  EDIT-CURSOR OF DSP-JIKAN(Y)
         ELSE
*******      IF WK-MM = ZERO OR WK-MM = 30
                MOVE  "M"        TO  EDIT-OPTION OF DSP-JIKAN(Y)
                MOVE   SPACE     TO  EDIT-CURSOR OF DSP-JIKAN(Y)
*******      ELSE
*******         IF  ERR-FLG = ZERO
*******             MOVE  8      TO  ERR-FLG
*******         END-IF
*******         MOVE  "R"        TO  EDIT-OPTION OF DSP-JIKAN(Y)
*******         MOVE  "C"        TO  EDIT-CURSOR OF DSP-JIKAN(Y)
*******      END-IF
         END-IF
     ELSE
         IF  DSP-JIKAN(Y) NOT = SPACE
             IF  ERR-FLG = ZERO
                 MOVE  7         TO  ERR-FLG
             END-IF
             MOVE  "R"           TO  EDIT-OPTION OF DSP-JIKAN(Y)
             MOVE  "C"           TO  EDIT-CURSOR OF DSP-JIKAN(Y)
         END-IF
     END-IF.
*
*取引先コード
     IF  DSP-JIKAN (Y) NOT = SPACE
**       通常行
         IF     DSP-TOKCD(Y)  NOT NUMERIC
             OR DSP-TOKCD(Y) = ZERO
             IF  ERR-FLG = ZERO
                 MOVE  3         TO  ERR-FLG
             END-IF
             MOVE  SPACE         TO  DSP-TOKCD(Y)(1:8)
             MOVE  "R"           TO  EDIT-OPTION OF DSP-TOKCD(Y)
             MOVE  "C"           TO  EDIT-CURSOR OF DSP-TOKCD(Y)
             MOVE  SPACE         TO  DSP-TOKNM(Y)
         ELSE
             MOVE  DSP-TOKCD(Y)  TO  TOK-F01
             PERFORM  HTOKMS-READ-SEC
             IF  HTOKMS-INV-FLG = SPACE
                 MOVE  TOK-F03   TO  DSP-TOKNM(Y)
                 MOVE  "M"       TO  EDIT-OPTION OF DSP-TOKCD(Y)
                 MOVE  SPACE     TO  EDIT-CURSOR OF DSP-TOKCD(Y)
             ELSE
                 MOVE  ALL NC"＊"  TO  DSP-TOKNM(Y)
                 IF  ERR-FLG = ZERO
                     MOVE 13       TO  ERR-FLG
                 END-IF
                 MOVE  "R"       TO  EDIT-OPTION OF DSP-TOKCD(Y)
                 MOVE  "C"       TO  EDIT-CURSOR OF DSP-TOKCD(Y)
             END-IF
         END-IF
     ELSE
**       削除行
         IF     DSP-TOKCD(Y)  NOT NUMERIC
             OR DSP-TOKCD(Y) = ZERO
             MOVE  SPACE         TO  DSP-TOKCD(Y)(1:8)
             MOVE  "M"           TO  EDIT-OPTION OF DSP-TOKCD(Y)
             MOVE  SPACE         TO  EDIT-CURSOR OF DSP-TOKCD(Y)
             MOVE  SPACE         TO  DSP-TOKNM(Y)
         ELSE
             MOVE  DSP-TOKCD(Y)  TO  TOK-F01
             PERFORM  HTOKMS-READ-SEC
             IF  HTOKMS-INV-FLG = SPACE
                 MOVE  TOK-F03   TO  DSP-TOKNM(Y)
                 MOVE  "M"       TO  EDIT-OPTION OF DSP-TOKCD(Y)
                 MOVE  SPACE     TO  EDIT-CURSOR OF DSP-TOKCD(Y)
             ELSE
                 MOVE  ALL NC"＊"  TO  DSP-TOKNM(Y)
                 MOVE  "M"       TO  EDIT-OPTION OF DSP-TOKCD(Y)
                 MOVE  SPACE     TO  EDIT-CURSOR OF DSP-TOKCD(Y)
             END-IF
         END-IF
     END-IF.
*データ区分チェック
     IF     DSP-DTKBN(Y) NOT NUMERIC
         OR DSP-DTKBN(Y) = ZERO
         MOVE  14                TO  ERR-FLG
         MOVE  "R"               TO  EDIT-OPTION OF DSP-DTKBN(Y)
         MOVE  "C"               TO  EDIT-CURSOR OF DSP-DTKBN(Y)
     ELSE
         MOVE  "M"               TO  EDIT-OPTION OF DSP-DTKBN(Y)
         MOVE  SPACE             TO  EDIT-CURSOR OF DSP-DTKBN(Y)
     END-IF.

     IF  ( DSP-DTKBN(Y) IS NUMERIC )
     AND ( DSP-DTKBN(Y) = 01 OR 02 OR 03 OR 04 OR 05 OR 06 OR 07
*** 2012/10/31
*********************  OR 08 )
                       OR 08 OR 09 )
         MOVE  "M"               TO  EDIT-OPTION OF DSP-DTKBN(Y)
         MOVE  SPACE             TO  EDIT-CURSOR OF DSP-DTKBN(Y)
         EVALUATE  DSP-DTKBN(Y)
             WHEN  "01"  MOVE NC"発　注"   TO  DSP-DTMN(Y)
             WHEN  "02"  MOVE NC"受　領"   TO  DSP-DTMN(Y)
             WHEN  "03"  MOVE NC"支　払"   TO  DSP-DTMN(Y)
             WHEN  "04"  MOVE NC"請　求"   TO  DSP-DTMN(Y)
             WHEN  "05"  MOVE NC"出　荷"   TO  DSP-DTMN(Y)
             WHEN  "06"  MOVE NC"欠品案"   TO  DSP-DTMN(Y)
             WHEN  "07"  MOVE NC"商品台"   TO  DSP-DTMN(Y)
*** 2012/10/31
             WHEN  "09"  MOVE NC"ＢＭＳ"   TO  DSP-DTMN(Y)
         END-EVALUATE
         MOVE  DSP-DTKBN(Y)      TO  EDI-F01
         MOVE  1                 TO  EDI-F02
         MOVE  DSP-TOKCD(Y)      TO  EDI-F03
         PERFORM JSMEDIF-READ-SEC
         IF JSMEDIF-INV-FLG = "INV"
            MOVE  15             TO  ERR-FLG
            MOVE  "R"            TO  EDIT-OPTION OF DSP-DTKBN(Y)
            MOVE  "C"            TO  EDIT-CURSOR OF DSP-DTKBN(Y)
         END-IF
     ELSE
         MOVE  14                TO  ERR-FLG
         MOVE  NC"＊＊＊"        TO  DSP-DTMN(Y)
         MOVE  "R"               TO  EDIT-OPTION OF DSP-DTKBN(Y)
         MOVE  "C"               TO  EDIT-CURSOR OF DSP-DTKBN(Y)
     END-IF.

** 歯抜け入力チェック
**   IF  FG-HANUKE = 1
**         IF  ERR-FLG = ZERO
**             MOVE  10            TO  ERR-FLG
**         END-IF
**         MOVE  "R"               TO  EDIT-OPTION OF DSP-JIKAN(Y)
**         MOVE  "C"               TO  EDIT-CURSOR OF DSP-JIKAN(Y)
**         MOVE  "R"               TO  EDIT-OPTION OF DSP-TOKCD(Y)
**         MOVE  "C"               TO  EDIT-CURSOR OF DSP-TOKCD(Y)
**     END-IF.

** 重複チェック
     MOVE  ZERO                  TO  FG-DUPLICATE.
** 自頁－自頁の重複チェック
     PERFORM  VARYING WK-IX  FROM 1 BY 1
              UNTIL   WK-IX  > 15
                   OR FG-DUPLICATE = 1
       IF      WK-IX             NOT = Y
           AND DSP-JIKAN (WK-IX) IS NUMERIC
           AND DSP-JIKAN (WK-IX)     = DSP-JIKAN(Y)
           MOVE  1               TO  FG-DUPLICATE
       END-IF

     END-PERFORM.

     IF  FG-DUPLICATE = 1
         IF  ERR-FLG = ZERO
             MOVE  11            TO  ERR-FLG
         END-IF
         MOVE  "R"               TO  EDIT-OPTION OF DSP-JIKAN(Y)
         MOVE  "C"               TO  EDIT-CURSOR OF DSP-JIKAN(Y)
         MOVE  "R"               TO  EDIT-OPTION OF DSP-TOKCD(Y)
         MOVE  "C"               TO  EDIT-CURSOR OF DSP-TOKCD(Y)
         GO TO  BODY-CHK-B-JIKAN-E
     END-IF.

** 自頁－他頁の重複チェック
     PERFORM  VARYING WK-IX  FROM 1 BY 1
              UNTIL   WK-IX  > 6
                   OR FG-DUPLICATE = 1
       IF  WK-IX NOT = P-CNT
           PERFORM  VARYING WK-IX2  FROM 1 BY 1
                    UNTIL   WK-IX2  > 15
             OR ((WK-IX - 1) * 15 + WK-IX2) > CT-TBLMAX
                         OR FG-DUPLICATE = 1
             IF      (TBL-JIKAN (WK-IX WK-IX2)
                                     NOT = 9999 AND 8888)
                 AND TBL-JIKAN (WK-IX WK-IX2)  = DSP-JIKAN(Y)
                 MOVE  1     TO  FG-DUPLICATE
             END-IF
           END-PERFORM
       END-IF

     END-PERFORM.

     IF  FG-DUPLICATE = 1
         IF  ERR-FLG = ZERO
             MOVE  11            TO  ERR-FLG
         END-IF
         MOVE  "R"               TO  EDIT-OPTION OF DSP-JIKAN(Y)
         MOVE  "C"               TO  EDIT-CURSOR OF DSP-JIKAN(Y)
         MOVE  "R"               TO  EDIT-OPTION OF DSP-TOKCD(Y)
         MOVE  "C"               TO  EDIT-CURSOR OF DSP-TOKCD(Y)
         GO TO  BODY-CHK-B-JIKAN-E
     END-IF.

 BODY-CHK-B-JIKAN-E.
*
*得意先コードから回線種別、端末名を取得する。
     IF  ERR-FLG = ZERO
         MOVE  "01"              TO  EDI-F01
         MOVE  1                 TO  EDI-F02
         MOVE  DSP-TOKCD(Y)      TO  EDI-F03
         PERFORM JSMEDIF-READ-SEC
         IF  JSMEDIF-INV-FLG = SPACE
             MOVE  EDI-F04           TO  DSP-SEN(Y)
             IF  EDI-F04 = "1"
                 MOVE  NC"ＩＳＤＮ"  TO  DSP-SENNM(Y)
*** 2012/10/31
************ ELSE
             END-IF
             IF  EDI-F04 = "2"
                 MOVE  NC"公衆回線"  TO  DSP-SENNM(Y)
             END-IF
             IF  EDI-F04 = "3"
                 MOVE  NC"ＢＭＳ　"  TO  DSP-SENNM(Y)
             END-IF
***

         ELSE
             MOVE  "*"               TO  DSP-SEN(Y)
             MOVE  ALL NC"＊"        TO  DSP-SENNM(Y)
         END-IF
     END-IF.
*
 BODY-CHK-B-EXIT.
     EXIT.
****************************************************************
*             明細項目チェック２                               *
****************************************************************
 BODY-CHK-C-SEC          SECTION.
** 他頁－他頁の重複チェック
     IF  ERR-FLG  =  ZERO
         CONTINUE
     ELSE
         GO TO  BODY-CHK-C-EXIT
     END-IF.
**   テーブル領域を一時退避する。
     MOVE TABLE-G                TO TABLE-G-BK.
**   テーブル領域に仮設定する。
     PERFORM  DSP-WORK-SEC.
**   最終的な重複チェック
     PERFORM  VARYING IX-JIGMN-PG  FROM 1 BY 1
              UNTIL   IX-JIGMN-PG  > 6
                   OR FG-DUPLICATE = 1
       PERFORM  VARYING IX-JIGMN-GYO  FROM 1 BY 1
                UNTIL   IX-JIGMN-GYO > 15
          OR ((IX-JIGMN-PG - 1) * 15 + IX-JIGMN-GYO) > CT-TBLMAX
                     OR FG-DUPLICATE = 1

         IF  TBL-JIKAN (IX-JIGMN-PG IX-JIGMN-GYO)
                     NOT = 9999 AND 8888

             PERFORM  VARYING WK-IX2  FROM 1 BY 1
                      UNTIL   WK-IX2  > 6
                           OR FG-DUPLICATE = 1
               IF  WK-IX2 NOT = IX-JIGMN-PG
                   PERFORM  VARYING WK-IX3  FROM 1 BY 1
                            UNTIL   WK-IX3  > 15
                     OR ((WK-IX2 - 1) * 15 + WK-IX3) > CT-TBLMAX
                                 OR FG-DUPLICATE = 1
                     IF      (TBL-JIKAN (WK-IX2 WK-IX3)
                                      NOT = 9999 AND 8888)
                         AND TBL-JIKAN (WK-IX2 WK-IX3)
                           = TBL-JIKAN (IX-JIGMN-PG IX-JIGMN-GYO)
                         MOVE  1     TO  FG-DUPLICATE
                     END-IF
                   END-PERFORM
               END-IF

             END-PERFORM

         END-IF

       END-PERFORM

     END-PERFORM.

     IF  FG-DUPLICATE = 1
         IF  ERR-FLG = ZERO
             MOVE  12            TO  ERR-FLG
         END-IF
     END-IF.

**   テーブル領域をを復元する。
     MOVE TABLE-G-BK             TO TABLE-G.

 BODY-CHK-C-EXIT.
     EXIT.
****************************************************************
*             画面→ワーク退避                                 *
****************************************************************
 DSP-WORK-SEC          SECTION.
     MOVE  "DSP-WORK-SEC"        TO  S-NAME.
*
* DSP-JIKAN   TBL-JIKAN-INI
* ＝ SPACE    ＝ 9999  ：データ無し行 ：入れ替えなし。
* ＝ SPACE    ≠ 9999  ：行削除       ：TBL-JIKAN <- 8888
*
* ≠ SPACE    ＝ 9999  ：行追加       ：TBL-JIKAN <- DSP-J
* ≠ SPACE    ≠ 9999  ：行変更       ：TBL-JIKAN <- DSP-J
*
* 初期状態
*   DSP-JIKAN   TBL-JIKAN  TBL-JIKAN-INI
*   1030        1030       1030
*   1031        1031       1031
*   SPACE       9999       9999
*   SPACE       9999       9999
*
* 画面入力後状態
*   DSP-JIKAN   TBL-JIKAN  TBL-JIKAN-INI
*   1130        1130       1030  ：変更
*   SPACE       8888       1031  ：削除
*   1230        1230       9999  ：追加
*   SPACE       9999       9999
*
*項目画面セット
     PERFORM  VARYING Y  FROM 1 BY 1
              UNTIL   Y > 15
       IF      (    DSP-JIKAN(Y) NUMERIC )
           AND (    DSP-TOKCD(Y) NOT = ZERO
                AND DSP-TOKCD(Y) NUMERIC )

           IF  (P-CNT - 1)  * 15 + Y > CT-TBLMAX
               COMPUTE CT-TBLMAX = (P-CNT - 1)  * 15 + Y
           END-IF
       END-IF

       EVALUATE  TRUE
**       データなし行は何もしない。
         WHEN      DSP-JIKAN(Y)           = SPACE
               AND TBL-JIKAN-INI(P-CNT Y) = 9999
           CONTINUE

**       行削除
         WHEN      DSP-JIKAN(Y)               = SPACE
               AND TBL-JIKAN-INI(P-CNT Y) NOT = 9999
          MOVE  8888             TO  TBL-JIKAN (P-CNT Y)

**       行追加、行変更
         WHEN  OTHER
          MOVE  DSP-JIKAN (Y)    TO  TBL-JIKAN (P-CNT Y)
       END-EVALUATE

       MOVE  DSP-TOKCD    (Y)    TO  TBL-TOKCD (P-CNT Y)
       MOVE  DSP-SEN      (Y)    TO  TBL-SEN   (P-CNT Y)
       MOVE  DSP-DTKBN    (Y)    TO  TBL-DTKBN (P-CNT Y)

     END-PERFORM.
*
 DSP-WORK-EXIT.
     EXIT.
****************************************************************
*             確認処理　入力（ PSW = 4 ）
****************************************************************
 DSP-KAKU-SEC          SECTION.
     MOVE  "DSP-KAKU-SEC"        TO  S-NAME.

     PERFORM  DSP-WRITE-SEC.
     PERFORM  DSP-READ-SEC.

     EVALUATE  DSP-FNC
*      実行
       WHEN  "E000"
         PERFORM  DSP-WORK-SEC
         PERFORM  JSMKIHF-DEL-SEC
         PERFORM  JSMKIHF-WRITE-SEC
         PERFORM  INIT-DSP-SEC
         MOVE  "1"               TO  PSW
*      取消
       WHEN  "F004"
         MOVE  1                 TO  P-CNT
         MOVE  "1"               TO  PSW
         PERFORM  INIT-DSP-SEC
*      終了
       WHEN  "F005"
         MOVE  "END"             TO  END-FLG
*      項目戻し
       WHEN  "F006"
         MOVE  "2"               TO  PSW
*      前頁
       WHEN  "F011"
         COMPUTE  C-CNT = P-CNT - 1
         IF  C-CNT = ZERO
             MOVE  5             TO  ERR-FLG
         ELSE
             PERFORM  DSP-WORK-SEC
             COMPUTE  P-CNT = P-CNT - 1
             PERFORM  INIT-DSP-SEC
             PERFORM  WORK-DSP-SEC
             MOVE "2"            TO  PSW
         END-IF
*      次頁
       WHEN  "F012"
         MOVE  P-CNT             TO  S-CNT
         COMPUTE  C-CNT = P-CNT + 1
         IF  C-CNT > 6
             MOVE  6             TO  ERR-FLG
         ELSE
*************IF  CHK-CNT = ZERO
             IF       (    DSP-JIKAN(15)  =  ZERO
                        OR DSP-JIKAN(15)  NOT  NUMERIC )
                  AND (    DSP-TOKCD(15)  =  ZERO
                        OR DSP-TOKCD(15)  NOT  NUMERIC )
                 MOVE 6          TO  ERR-FLG
             ELSE
                 PERFORM  DSP-WORK-SEC
                 COMPUTE  P-CNT = P-CNT + 1
                 PERFORM  INIT-DSP-SEC
                 PERFORM  WORK-DSP-SEC
                 MOVE  "2"       TO  PSW
             END-IF

         END-IF

       WHEN   OTHER
         MOVE  4                 TO  ERR-FLG
     END-EVALUATE.
*
 DSP-KAKU-EXIT.
     EXIT.
****************************************************************
*             基本スケジュールマスタ削除位置づけ               *
****************************************************************
 JSMKIHF-DEL-SEC       SECTION.
     MOVE  "JHKIHD-DEL-SEC"      TO  S-NAME.
*
     MOVE  SPACE                 TO  RD-FLG.
     PERFORM JSMKIHF-START-SEC.
     IF  CHK-FLG = "CHK"
         GO TO  JSMKIHF-DEL-EXIT
     END-IF.

     PERFORM JSMKIHF-READ-SEC.
     PERFORM DLT-SEC
             UNTIL  DSP-YOUBI NOT = KIH-F01
                 OR RD-FLG        = "END".
*
 JSMKIHF-DEL-EXIT.
     EXIT.
****************************************************************
*             基本スケジュールマスタ削除                       *
****************************************************************
 DLT-SEC               SECTION.
     MOVE  "DLT-SEC"             TO  S-NAME.

     DELETE  JSMKIHF.
     PERFORM  JSMKIHF-READ-SEC.

 DLT-EXIT.
     EXIT.
****************************************************************
*             基本スケジュールマスタ作成                       *
****************************************************************
 JSMKIHF-WRITE-SEC     SECTION.
     MOVE  "JSMKIHF-WRITE-SEC"   TO  S-NAME.

     PERFORM  SDATE-GET-SEC.

     MOVE  1                     TO  Y.
     PERFORM  VARYING X  FROM 1 BY 1
              UNTIL   X > 6
       PERFORM  VARYING Y  FROM 1 BY 1
                UNTIL   Y > 15
         IF  TBL-JIKAN(X Y)  NOT =  9999 AND 8888
             MOVE  SPACE           TO  KIH-REC
             INITIALIZE  KIH-REC
             MOVE  DSP-YOUBI       TO  KIH-F01
             MOVE  TBL-JIKAN      (X Y)  TO  KIH-F02

             EVALUATE  TRUE
**             初期データが存在しない。
**             キーが変わっていたら登録
**             X:0 X:Y
**             登録
               WHEN  (    TBL-JIKAN-INI (X Y) NOT = 9999
                      AND TBL-JIKAN (X Y) NOT =
                              TBL-JIKAN-INI (X Y))
                  OR (TBL-JIKAN-INI  (X Y) = 9999)
                 MOVE  TBL-TOKCD      (X Y)  TO  KIH-F03
                 MOVE  TBL-SEN        (X Y)  TO  KIH-F04
                 MOVE  TBL-DTKBN      (X Y)  TO  KIH-F05
                 MOVE  PAR-TANCD             TO  KIH-F06
                 MOVE  SYS-DATE              TO  KIH-F07
                 MOVE  WK-TIME(1:4)          TO  KIH-F08
                 MOVE  SPACE                 TO  KIH-F09
                 MOVE  ZERO                  TO  KIH-F10
                 MOVE  ZERO                  TO  KIH-F11

**             修正
               WHEN  TBL-TOKCD  (X Y) NOT =
                         TBL-TOKCD-INI (X Y)
                 MOVE  TBL-TOKCD      (X Y)  TO  KIH-F03
                 MOVE  TBL-SEN        (X Y)  TO  KIH-F04
                 MOVE  TBL-DTKBN      (X Y)  TO  KIH-F05
                 MOVE  TBL-TOROKU-TANT(X Y)  TO  KIH-F06
                 MOVE  TBL-TOROKUBI   (X Y)  TO  KIH-F07
                 MOVE  TBL-TOROKUTM   (X Y)  TO  KIH-F08
                 MOVE  PAR-TANCD             TO  KIH-F09
                 MOVE  SYS-DATE              TO  KIH-F10
                 MOVE  WK-TIME(1:4)          TO  KIH-F11
**             変更なし
               WHEN  OTHER
                 MOVE  TBL-TOKCD      (X Y)  TO  KIH-F03
                 MOVE  TBL-SEN        (X Y)  TO  KIH-F04
                 MOVE  TBL-DTKBN      (X Y)  TO  KIH-F05
                 MOVE  TBL-TOROKU-TANT(X Y)  TO  KIH-F06
                 MOVE  TBL-TOROKUBI   (X Y)  TO  KIH-F07
                 MOVE  TBL-TOROKUTM   (X Y)  TO  KIH-F08
                 MOVE  TBL-KOSIN-TANT (X Y)  TO  KIH-F09
                 MOVE  TBL-KOSINBI    (X Y)  TO  KIH-F10
                 MOVE  TBL-KOSINTM    (X Y)  TO  KIH-F11

             END-EVALUATE

             WRITE  KIH-REC
         END-IF
       END-PERFORM

       MOVE  1                   TO  Y
     END-PERFORM.
*
 JSMKIHF-WRITE-EXIT.
     EXIT.
****************************************************************
*             終了処理                               3.0       *
****************************************************************
 END-SEC               SECTION.
     MOVE  "END-SEC"             TO  S-NAME.
*ファイル ＣＬＯＳＥ
     CLOSE  DSPFILE JSMKIHF
            JSMEDIF HTOKMS.

 END-EXIT.
     EXIT.
*****************<<  SNJ0070I   END PROGRAM  >>******************

```
