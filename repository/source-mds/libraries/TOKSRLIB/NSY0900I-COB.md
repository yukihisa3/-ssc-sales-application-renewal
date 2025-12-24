# NSY0900I

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/NSY0900I.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　ＥＤＩ　　　　　　　　　　        *
*    サブシステム　　　　：　ＤＣＭ　ＥＤＩ　　　　　　　　　　*
*    モジュール名　　　　：　ＤＣＭ取引先設定マスタ保守        *
*    作成日／作成者　　　：　2021/02/15 INOUE                  *
*    処理概要　　　　　　：　　　　　　　　　　　　　　　　　　*
*                                                              *
*    更新日／更新者　　　：　                                  *
*    修正概要　　　　　　：　　　　　　　　　　　　　　　　　　*
****************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           NSY0900I.
 AUTHOR.               NAV.
 DATE-WRITTEN.         2021/02/15.
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
*ＤＣＭ取引先設定マスタ
     SELECT  DCMTOKF   ASSIGN    TO        DA-01-VI-DCMTOKL1
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      DYNAMIC
                       RECORD    KEY       DCM-F01
                                           DCM-F02
                                           DCM-F03
                                           DCM-F04
                       FILE      STATUS    DCM-ST.
*条件ファイル
     SELECT  HJYOKEN   ASSIGN    TO        DA-01-VI-JYOKEN1
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      RANDOM
                       RECORD    KEY       JYO-F01
                                           JYO-F02
                       FILE      STATUS    JYO-ST.
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
*    FILE = ＤＣＭ取引先設定マスタ                             *
****************************************************************
 FD  DCMTOKF
                       LABEL     RECORD    IS   STANDARD.
                       COPY      DCMTOKF   OF   XFDLIB
                       JOINING   DCM       AS   PREFIX.
****************************************************************
*    FILE = 条件ファイル                                   *
****************************************************************
 FD  HJYOKEN
                       LABEL     RECORD    IS   STANDARD.
                       COPY      HJYOKEN   OF   XFDLIB
                       JOINING   JYO       AS   PREFIX.
****************************************************************
*    FILE = 取引先マスタ                                       *
****************************************************************
 FD  HTOKMS
*                      BLOCK     CONTAINS  8    RECORDS
                       LABEL     RECORD    IS   STANDARD.
                       COPY      HTOKMS    OF   XFDLIB
                       JOINING   TOK       AS   PREFIX.
****************************************************************
*    FILE = 画面ファイル                                       *
****************************************************************
 FD  DSPFILE
                       LABEL     RECORD    IS   OMITTED.
                       COPY      FSY09001  OF   XMDLIB
                       JOINING   DSP       AS   PREFIX.
*
****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
*ステータス領域
 01  STATUS-AREA.
     03  DCM-ST                   PIC  X(02).
     03  JYO-ST                   PIC  X(02).
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
     03  HJYOKEN-INV-FLG          PIC  X(03)  VALUE  SPACE.
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
*01  WRK-YOBI                     PIC  9(01)  VALUE  ZERO.
 01  WRK-KYTOKC                   PIC  X(06)  VALUE  SPACE.
 01  WRK-BLKCD                    PIC  X(02)  VALUE  SPACE.
 01  WRK-KBTOKC                   PIC  X(06)  VALUE  SPACE.
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
             VALUE NC"共通取引先ＣＤを入力して下さい。　　　　".
     03  ERR-MSG2.
         05  FILLER              PIC   N(20)
             VALUE NC"共通取引先ＣＤが未登録です。　　　　　　".
     03  ERR-MSG3.
         05  FILLER              PIC   N(20)
             VALUE NC"ブロックＣＤを入力して下さい。　　　　　".
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
             VALUE NC"個別取引先ＣＤを入力して下さい。　　　　".
     03  ERR-MSG8.
         05  FILLER              PIC   N(20)
             VALUE NC"個別取引先ＣＤが未登録です。　　　　　　".
     03  ERR-MSG9.
         05  FILLER              PIC   N(20)
             VALUE NC"明細を入力して下さい。　　　　　　".
     03  ERR-MSG10.
         05  FILLER              PIC   N(20)
             VALUE NC"行を詰めて入力して下さい　　　　　".
     03  ERR-MSG11.
         05  FILLER              PIC   N(20)
             VALUE NC"ゾーンが重複しています。　　　　　".
     03  ERR-MSG12.
         05  FILLER              PIC   N(20)
             VALUE NC"他ページのゾーンが重複しています。".
     03  ERR-MSG13.
         05  FILLER              PIC   N(20)
             VALUE NC"ゾーンＣＤが未登録です。　　　　　　　　".
     03  ERR-MSG14.
         05  FILLER              PIC   N(20)
             VALUE NC"ＮＡＶＳ取引先ＣＤを入力して下さい。　　".
     03  ERR-MSG15.
         05  FILLER              PIC   N(20)
             VALUE NC"ＮＡＶＳ取引先マスタ未登録です。　　　　".
     03  ERR-MSG16.
         05  FILLER              PIC   N(20)
             VALUE NC"ブロックＣＤが未登録です。　　　　　　　".
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
*
*ＤＣＭ設定マスタ退避エリア
 01  TABLE-AREA.
     03  TBL-KYTOKC               PIC   X(06).
     03  TBL-KYTOKN               PIC   N(20).
     03  TBL-BLKCD                PIC   X(02).
     03  TBL-BLKNM                PIC   N(20).
     03  TBL-KBTOKC               PIC   X(06).
     03  TBL-KBTOKN               PIC   N(20).
     03  TABLE-G.
       05  TABLE1      OCCURS  6.
           07  TABLE2  OCCURS  12.
               09  TBL-GYO          PIC   9(02).
               09  TBL-MSZOCD       PIC   X(02).
               09  TBL-MSTOKC       PIC   9(08).
               09  TBL-TOROKU-TANT  PIC   X(02).
               09  TBL-TOROKUBI     PIC   9(08).
               09  TBL-TOROKUTM     PIC   9(04).
     03  TABLE-G-R  REDEFINES TABLE-G.
         05  TABLE2-R  OCCURS  72.
               09  TBL-GYO-R        PIC   9(02).
               09  TBL-MSZOCD-R     PIC   X(02).
               09  TBL-MSTOKC-R    PIC   9(08).
               09  TBL-TOROKU-TANT-R  PIC   X(02).
               09  TBL-TOROKUBI-R     PIC   9(08).
               09  TBL-TOROKUTM-R     PIC   9(04).
     03  TABLE-G-INI.
       05  TABLE1-INI      OCCURS  6.
           07  TABLE2-INI  OCCURS  12.
               09  TBL-GYO-INI      PIC   9(02).
               09  TBL-MSZOCD-INI   PIC   X(02).
               09  TBL-MSTOKC-INI  PIC   9(08).
               09  TBL-TOROKU-TANT-INI PIC   X(02).
               09  TBL-TOROKUBI-INI     PIC   9(08).
               09  TBL-TOROKUTM-INI     PIC   9(04).
     03  TABLE-G-INI-R  REDEFINES TABLE-G-INI.
         05  TABLE2-INI-R  OCCURS  72.
               09  TBL-GYO-INI-R      PIC   9(02).
               09  TBL-MSZOCD-INI-R   PIC   X(02).
               09  TBL-MSTOKC-INI-R  PIC   9(08).
               09  TBL-TOROKU-TANT-INI-R PIC   X(02).
               09  TBL-TOROKUBI-INI-R   PIC   9(08).
               09  TBL-TOROKUTM-INI-R   PIC   9(04).
     03  TABLE-G-BK.
       05  TABLE1-BK      OCCURS  6.
           07  TABLE2-BK  OCCURS  12.
               09  TBL-GYO-BK       PIC   9(02).
               09  TBL-MSZOCD-BK    PIC   X(02).
               09  TBL-MSTOKC-BK   PIC   9(08).
               09  TBL-TOROKU-TANT-BK  PIC   X(02).
               09  TBL-TOROKUBI-BK      PIC   9(08).
               09  TBL-TOROKUTM-BK      PIC   9(04).
     03  TABLE-G-BK-R  REDEFINES TABLE-G-BK.
         05  TABLE2-BK-R  OCCURS  72.
               09  TBL-GYO-BK-R     PIC   9(02).
               09  TBL-MSZOCD-BK-R  PIC   X(02).
               09  TBL-MSTOKC-BK-R PIC   9(08).
               09  TBL-TOROKU-TANT-BK-R PIC   X(02).
               09  TBL-TOROKUBI-BK-R      PIC   9(08).
               09  TBL-TOROKUTM-BK      PIC   9(04).
*
*受信時間チェック
 01  WK-JIKAN.
     03  WK-HH                    PIC   9(02)  VALUE  ZERO.
     03  WK-MM                    PIC   9(02)  VALUE  ZERO.
*
*ファイルエラーメッセージ
 01  FILE-ERR.
     03  DCM-ERR           PIC N(15) VALUE
                        NC"ＤＣＭ取引先設定マスタエラー".
     03  JYO-ERR           PIC N(15) VALUE
                        NC"条件ファイルエラー".
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
 01  PAR-BUMCD     PIC  X(04).
 01  PAR-TANCD     PIC  X(02).
*
**************************************************************
 PROCEDURE             DIVISION   USING  PAR-BUMCD PAR-TANCD.
**************************************************************
 DECLARATIVES.
 DCM-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE DCMTOKF.
     MOVE        DCM-ST    TO        E-ST.
     MOVE        "DCMTOKF" TO        E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     DCM-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 JYO-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HJYOKEN.
     MOVE        JYO-ST    TO        E-ST.
     MOVE        "HJYOKEN" TO        E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     JYO-ERR   UPON      CONS.
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
     OPEN  I-O    DCMTOKF.
     OPEN  I-O    DSPFILE.
     OPEN  INPUT  HJYOKEN HTOKMS.
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
     MOVE  SPACE                 TO  DSP-FSY09001.
*システム日付転送
     MOVE  SYS-DATE              TO  DSP-SDATE.
*システム時間転送
     MOVE  WK-TIME(1:6)          TO  DSP-STIME.
*ＦＯＲＭ　ＩＤ
     MOVE  "FSY09001"            TO  DSP-FMID.
*プログラムＩＤ
     MOVE  "NSY0900I"            TO  DSP-PGMID.
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
*    MOVE  "M"              TO  EDIT-OPTION OF DSP-KYTOKC.
     MOVE  "M"              TO  EDIT-OPTION OF DSP-KYTOKC
                                EDIT-OPTION OF DSP-BLKCD
                                EDIT-OPTION OF DSP-KBTOKC.
*    MOVE  SPACE            TO  EDIT-CURSOR OF DSP-KYTOKC.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-KYTOKC
                                EDIT-CURSOR OF DSP-BLKCD
                                EDIT-CURSOR OF DSP-KBTOKC.
     PERFORM  VARYING X  FROM 1 BY 1
*             UNTIL   X > 15
              UNTIL   X > 12
***
       MOVE  "M"            TO  EDIT-OPTION OF DSP-MSZOCD(X)
       MOVE  SPACE          TO  EDIT-CURSOR OF DSP-MSZOCD(X)
       MOVE  "M"            TO  EDIT-OPTION OF DSP-MSTOKC(X)
       MOVE  SPACE          TO  EDIT-CURSOR OF DSP-MSTOKC(X)
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
         MOVE  DSP-KYTOKC        TO  WRK-KYTOKC
         MOVE  DSP-BLKCD         TO  WRK-BLKCD
         MOVE  DSP-KBTOKC        TO  WRK-KBTOKC
         PERFORM  INIT-DSP-SEC
         MOVE  WRK-KYTOKC        TO  DSP-KYTOKC
         MOVE  WRK-BLKCD         TO  DSP-BLKCD
         MOVE  WRK-KBTOKC        TO  DSP-KBTOKC
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
*      ヘッド
       WHEN  "1"
         MOVE  PF-MSG-R(1)       TO  DSP-PFGAID
*      明細／確認
       WHEN  "2"  WHEN  "3"
         MOVE  PF-MSG-R(2)       TO  DSP-PFGAID
     END-EVALUATE.
* 画面の表示
     MOVE  "SCREEN"              TO  DSP-GRP.
     MOVE  "FSY09001"            TO  DSP-FMT.
     WRITE  DSP-FSY09001.
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
*      ヘッド
       WHEN  "1"
         MOVE  "HEAD"            TO  DSP-GRP
*      明細
       WHEN  "2"
         MOVE  "BODY"            TO  DSP-GRP
*      確認
       WHEN  "3"
         MOVE  "KAKU"            TO  DSP-GRP
     END-EVALUATE.

     MOVE  "FSY09001"            TO  DSP-FMT.
     READ  DSPFILE.
*入力項目の属性を通常にする
 DSP-READ-010.
*    MOVE  ZERO                  TO  ERR-FLG.
     MOVE  SPACE                 TO  DSP-PRO.
*
 DSP-READ-EXIT.
     EXIT.
****************************************************************
*           ヘッドチェック                                     *
****************************************************************
 HEAD-CHK-SEC             SECTION.
     MOVE  "HEAD-CHK-SEC"        TO  S-NAME.
*共通取引先ＣＤ
     IF  DSP-KYTOKC = SPACE
         MOVE   1                TO  ERR-FLG
         MOVE  "R"               TO  EDIT-OPTION  OF  DSP-KYTOKC
         MOVE  "C"               TO  EDIT-CURSOR  OF  DSP-KYTOKC
         GO                      TO  HEAD-CHK-EXIT
     ELSE
         MOVE   7                TO  JYO-F01
         MOVE   DSP-KYTOKC       TO  JYO-F02
         PERFORM  HJYOKEN-READ-SEC
         IF     HJYOKEN-INV-FLG  =   SPACE
                MOVE  JYO-F03    TO  DSP-KYTOKN
                MOVE  "M"        TO  EDIT-OPTION  OF  DSP-KYTOKC
                MOVE  SPACE      TO  EDIT-CURSOR  OF  DSP-KYTOKC
         ELSE
                MOVE  2          TO  ERR-FLG
                MOVE  "R"        TO  EDIT-OPTION  OF  DSP-KYTOKC
                MOVE  "C"        TO  EDIT-CURSOR  OF  DSP-KYTOKC
                MOVE  ALL NC"＊" TO  DSP-KYTOKN
                GO               TO  HEAD-CHK-EXIT
         END-IF
     END-IF.
*
*ブロックＣＤ
     IF  DSP-BLKCD  = SPACE
         MOVE   3                TO  ERR-FLG
         MOVE  "R"               TO  EDIT-OPTION  OF  DSP-BLKCD
         MOVE  "C"               TO  EDIT-CURSOR  OF  DSP-BLKCD
         GO                      TO  HEAD-CHK-EXIT
     ELSE
         MOVE   8                TO  JYO-F01
         MOVE   DSP-BLKCD        TO  JYO-F02
         PERFORM  HJYOKEN-READ-SEC
         IF     HJYOKEN-INV-FLG  =   SPACE
                MOVE  JYO-F03    TO  DSP-BLKNM
                MOVE  "M"        TO  EDIT-OPTION  OF  DSP-BLKCD
                MOVE  SPACE      TO  EDIT-CURSOR  OF  DSP-BLKCD
         ELSE
                MOVE  16         TO  ERR-FLG
                MOVE  "R"        TO  EDIT-OPTION  OF  DSP-BLKCD
                MOVE  "C"        TO  EDIT-CURSOR  OF  DSP-BLKCD
                MOVE  ALL NC"＊" TO  DSP-BLKNM
                GO               TO  HEAD-CHK-EXIT
         END-IF
     END-IF.
*
*個別取引先ＣＤ
     IF  DSP-KBTOKC = SPACE
         MOVE   7                TO  ERR-FLG
         MOVE  "R"               TO  EDIT-OPTION  OF  DSP-KBTOKC
         MOVE  "C"               TO  EDIT-CURSOR  OF  DSP-KBTOKC
         GO                      TO  HEAD-CHK-EXIT
     ELSE
         MOVE   9                TO  JYO-F01
         MOVE   DSP-KBTOKC       TO  JYO-F02
         PERFORM  HJYOKEN-READ-SEC
         IF     HJYOKEN-INV-FLG  =   SPACE
                MOVE  JYO-F03    TO  DSP-KBTOKN
                MOVE  "M"        TO  EDIT-OPTION  OF  DSP-KBTOKC
                MOVE  SPACE      TO  EDIT-CURSOR  OF  DSP-KBTOKC
         ELSE
                MOVE  8          TO  ERR-FLG
                MOVE  "R"        TO  EDIT-OPTION  OF  DSP-KBTOKC
                MOVE  "C"        TO  EDIT-CURSOR  OF  DSP-KBTOKC
                MOVE  ALL NC"＊" TO  DSP-KBTOKN
                GO               TO  HEAD-CHK-EXIT
         END-IF
     END-IF.
*
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

*ヘッダのワーク退避
*    MOVE  DSP-KYTOKC             TO  TBL-YOUBI.
     MOVE  DSP-KYTOKC            TO  TBL-KYTOKC.
     MOVE  DSP-KYTOKN            TO  TBL-KYTOKN.
     MOVE  DSP-BLKCD             TO  TBL-BLKCD.
     MOVE  DSP-BLKNM             TO  TBL-BLKNM.
     MOVE  DSP-KBTOKC            TO  TBL-KBTOKC.
     MOVE  DSP-KBTOKN            TO  TBL-KBTOKN.
*ＳＥＱの作成
     MOVE  ZERO                  TO  GYO-CNT.
     PERFORM  VARYING X  FROM 1 BY 1
              UNTIL   X > 6
       PERFORM  VARYING Y FROM  1 BY 1
*               UNTIL Y > 15
                UNTIL Y > 12
         ADD 1  TO  GYO-CNT
         MOVE  GYO-CNT           TO  TBL-GYO   (X Y)
*      ゾーンの初期値は99、画面表示は空白とする。
         MOVE  "ZZ"              TO  TBL-MSZOCD (X Y)
       END-PERFORM
     END-PERFORM.
*ＤＣＭ取引先設定マスタスタート
     PERFORM DCMTOKF-START-SEC.
     IF  CHK-FLG = "CHK"
         GO TO  MST-WORK-010
     END-IF.
*
*ＤＣＭ取引先設定マスタ読込み
     PERFORM  DCMTOKF-READ-SEC.
*ＤＣＭ取引先設定マスタを読込みながらワークへセット
     MOVE  ZERO                  TO  CT-TBLMAX.
     PERFORM  VARYING X  FROM 1 BY 1
              UNTIL   X > 6
                   OR DSP-KYTOKC NOT = DCM-F01
                   OR RD-FLG        = "END"
       PERFORM  VARYING Y  FROM 1 BY 1
*               UNTIL   Y > 15
                UNTIL   Y > 12
*                    OR DSP-KYTOKC NOT = DCM-F01
                     OR DSP-KYTOKC NOT = DCM-F01
                     OR DSP-BLKCD  NOT = DCM-F02
                     OR DSP-KBTOKC NOT = DCM-F03
                     OR RD-FLG        = "END"
         ADD  1   TO  CT-TBLMAX
         MOVE  DCM-F04           TO  TBL-MSZOCD      (X Y)
         MOVE  DCM-F05           TO  TBL-MSTOKC      (X Y)
         MOVE  DCM-F97           TO  TBL-TOROKU-TANT (X Y)
         MOVE  DCM-F98           TO  TBL-TOROKUBI    (X Y)
         MOVE  DCM-F99           TO  TBL-TOROKUTM    (X Y)
         PERFORM  DCMTOKF-READ-SEC
*
       END-PERFORM
*
     END-PERFORM.
*
     CLOSE  DCMTOKF.
     OPEN   I-O  DCMTOKF.
**
 MST-WORK-010.
     MOVE  TABLE-G               TO  TABLE-G-INI.
*
 MST-WORK-EXIT.
     EXIT.
****************************************************************
*             ＤＣＭ取引先設定マスタ読込み                     *
****************************************************************
 DCMTOKF-START-SEC     SECTION.
     MOVE  "DCMTOKF-START-SEC"   TO  S-NAME.
*ＤＣＭ取引先設定マスタスタート
     MOVE  SPACE                 TO  RD-FLG.
     MOVE  SPACE                 TO  CHK-FLG.
     MOVE  DSP-KYTOKC            TO  DCM-F01.
     MOVE  DSP-BLKCD             TO  DCM-F02.
     MOVE  DSP-KBTOKC            TO  DCM-F03.
     MOVE  SPACE                 TO  DCM-F04.
     START  DCMTOKF  KEY IS >= DCM-F01 DCM-F02 DCM-F03 DCM-F04
       INVALID
         MOVE  "CHK"             TO  CHK-FLG
     END-START.
*
 DCMTOKF-START-EXIT.
     EXIT.
****************************************************************
*             ＤＣＭ取引先設定マスタ読込み                     *
****************************************************************
 DCMTOKF-READ-SEC      SECTION.
     MOVE  "DCMTOKF-READ-SEC"    TO  S-NAME.
*マスタ読込み
     READ  DCMTOKF  NEXT
       AT END
         MOVE  "END"             TO  RD-FLG
     END-READ.
*
 DCMTOKF-READ-EXIT.
     EXIT.
****************************************************************
*             ワーク→画面表示                                 *
****************************************************************
 WORK-DSP-SEC          SECTION.
     MOVE  "WORK-DSP-SEC"        TO  S-NAME.
*項目画面セット
     MOVE  ZERO                  TO  CHK-CNT.
     MOVE  TBL-KYTOKC            TO  DSP-KYTOKC.
     MOVE  TBL-KYTOKN            TO  DSP-KYTOKN.
     MOVE  TBL-BLKCD             TO  DSP-BLKCD.
     MOVE  TBL-BLKNM             TO  DSP-BLKNM.
     MOVE  TBL-KBTOKC            TO  DSP-KBTOKC.
     MOVE  TBL-KBTOKN            TO  DSP-KBTOKN.
     PERFORM  VARYING Y  FROM 1 BY 1
              UNTIL   Y > 12
*      MOVE    TBL-GYO(P-CNT Y)   TO DSP-GYO(Y)
       IF  TBL-MSZOCD(P-CNT Y)  NOT =  "ZZ"
           IF  TBL-MSZOCD(P-CNT Y)  =  "YY"
               MOVE  SPACE                TO  DSP-MSZOCD(Y)
           ELSE
               MOVE  TBL-MSZOCD(P-CNT Y)  TO  DSP-MSZOCD(Y)
               MOVE  29                   TO  JYO-F01
               MOVE  TBL-MSZOCD(P-CNT Y)  TO  JYO-F02
               PERFORM HJYOKEN-READ-SEC
               IF  HJYOKEN-INV-FLG = SPACE
                   MOVE  JYO-F03          TO  DSP-MSZONM(Y)
               ELSE
                   MOVE  ALL NC"＊"       TO  DSP-MSZONM(Y)
               END-IF
           END-IF
*
           IF TBL-MSTOKC(P-CNT Y) NOT = ZERO
              MOVE  TBL-MSTOKC(P-CNT Y)   TO  DSP-MSTOKC(Y)
           ELSE
              MOVE  SPACE                 TO  DSP-MSTOKC(Y)(1:8)
           END-IF
           MOVE  TBL-MSTOKC(P-CNT Y)   TO  TOK-F01
           PERFORM HTOKMS-READ-SEC
           IF  HTOKMS-INV-FLG = SPACE
               MOVE  TOK-F03          TO  DSP-MSTOKN(Y)
           ELSE
               MOVE  ALL NC"＊"       TO  DSP-MSTOKN(Y)
           END-IF
*
           ADD  1  TO  CHK-CNT
       ELSE
           MOVE  SPACE                TO  DSP-MSZOCD (Y) (1:2)
           MOVE  SPACE                TO  DSP-MSZONM (Y)
           MOVE  SPACE                TO  DSP-MSTOKC (Y) (1:8)
           MOVE  SPACE                TO  DSP-MSTOKN (Y)
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
*             条件ファイル読込み                           *
****************************************************************
 HJYOKEN-READ-SEC      SECTION.
     MOVE  "HJYOKEN-READ-SEC"    TO  S-NAME.
     READ HJYOKEN
       INVALID
         MOVE  "INV"             TO  HJYOKEN-INV-FLG
       NOT  INVALID
         MOVE  SPACE             TO  HJYOKEN-INV-FLG
     END-READ.
*
 HJYOKEN-READ-EXIT.
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
                 IF        (  DSP-MSZOCD(12) = SPACE )
                      AND  (  DSP-MSTOKC(12) = ZERO
                           OR DSP-MSTOKC(12) NOT NUMERIC )
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
*             UNTIL   Y > 15
              UNTIL   Y > 12

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

     IF  TBL-MSZOCD-INI (P-CNT Y) NOT = "ZZ"
**       初期表示のデータ有り行
         ADD  1   TO  CHK-MEI
     END-IF.

     IF      TBL-MSZOCD-INI (P-CNT Y) = "ZZ"
         AND DSP-MSZOCD (Y)           = SPACE
**       画面のデータ無し行
         MOVE  1            TO  FG-HANUKE
         MOVE  SPACE        TO  DSP-MSZOCD (Y)(1:2)
         MOVE  SPACE        TO  DSP-MSZONM (Y)
         MOVE  SPACE        TO  DSP-MSTOKC (Y)(1:8)
         MOVE  SPACE        TO  DSP-MSTOKN (Y)
         GO TO  BODY-CHK-B-EXIT
     ELSE
**       データ有り行
         ADD  1   TO  CHK-MEI2
     END-IF.
*
*ゾーン
     IF  DSP-MSZOCD(Y)  NOT = "  "
         MOVE   29                  TO  JYO-F01
         MOVE   DSP-MSZOCD(Y)       TO  JYO-F02
         PERFORM  HJYOKEN-READ-SEC
         IF     HJYOKEN-INV-FLG  =  SPACE
                MOVE  "M"        TO  EDIT-OPTION OF DSP-MSZOCD(Y)
                MOVE   SPACE     TO  EDIT-CURSOR OF DSP-MSZOCD(Y)
                MOVE   JYO-F03   TO  DSP-MSZONM(Y)
         ELSE
                IF     ERR-FLG = ZERO
                       MOVE  13  TO  ERR-FLG
                       MOVE  "R" TO  EDIT-OPTION OF DSP-MSZOCD(Y)
                       MOVE  "C" TO  EDIT-CURSOR OF DSP-MSZOCD(Y)
                       MOVE  ALL NC"＊"   TO DSP-MSZONM(Y)
                END-IF
     END-IF.
*
*取引先コード
     IF  DSP-MSZOCD (Y) NOT = SPACE
**       通常行
         IF     DSP-MSTOKC(Y)  NOT NUMERIC
             OR DSP-MSTOKC(Y) = ZERO
             IF  ERR-FLG = ZERO
                 MOVE  14        TO  ERR-FLG
             END-IF
             MOVE  SPACE         TO  DSP-MSTOKC(Y)(1:8)
             MOVE  "R"           TO  EDIT-OPTION OF DSP-MSTOKC(Y)
             MOVE  "C"           TO  EDIT-CURSOR OF DSP-MSTOKC(Y)
             MOVE  SPACE         TO  DSP-MSTOKN(Y)
         ELSE
             MOVE  DSP-MSTOKC(Y)  TO  TOK-F01
             PERFORM  HTOKMS-READ-SEC
             IF  HTOKMS-INV-FLG = SPACE
                 MOVE  TOK-F03   TO  DSP-MSTOKN(Y)
                 MOVE  "M"       TO  EDIT-OPTION OF DSP-MSTOKC(Y)
                 MOVE  SPACE     TO  EDIT-CURSOR OF DSP-MSTOKC(Y)
             ELSE
                 MOVE  ALL NC"＊"  TO  DSP-MSTOKN(Y)
                 IF  ERR-FLG = ZERO
                     MOVE 15       TO  ERR-FLG
                 END-IF
                 MOVE  "R"       TO  EDIT-OPTION OF DSP-MSTOKC(Y)
                 MOVE  "C"       TO  EDIT-CURSOR OF DSP-MSTOKC(Y)
             END-IF
         END-IF
     ELSE
**       削除行
         IF     DSP-MSTOKC(Y)  NOT NUMERIC
             OR DSP-MSTOKC(Y) = ZERO
             MOVE  SPACE         TO  DSP-MSTOKC(Y)(1:8)
             MOVE  "M"           TO  EDIT-OPTION OF DSP-MSTOKC(Y)
             MOVE  SPACE         TO  EDIT-CURSOR OF DSP-MSTOKC(Y)
             MOVE  SPACE         TO  DSP-MSTOKN(Y)
         ELSE
             MOVE  DSP-MSTOKC(Y)  TO  TOK-F01
             PERFORM  HTOKMS-READ-SEC
             IF  HTOKMS-INV-FLG = SPACE
                 MOVE  TOK-F03   TO  DSP-MSTOKN(Y)
                 MOVE  "M"       TO  EDIT-OPTION OF DSP-MSTOKC(Y)
                 MOVE  SPACE     TO  EDIT-CURSOR OF DSP-MSTOKC(Y)
             ELSE
                 MOVE  ALL NC"＊"  TO  DSP-MSTOKN(Y)
                 MOVE  "M"       TO  EDIT-OPTION OF DSP-MSTOKC(Y)
                 MOVE  SPACE     TO  EDIT-CURSOR OF DSP-MSTOKC(Y)
             END-IF
         END-IF
     END-IF.
** 重複チェック
     MOVE  ZERO                  TO  FG-DUPLICATE.
** 自頁－自頁の重複チェック
     PERFORM  VARYING WK-IX  FROM 1 BY 1
*             UNTIL   WK-IX  > 15
              UNTIL   WK-IX  > 12
                   OR FG-DUPLICATE = 1
       IF      WK-IX             NOT = Y
*          AND DSP-MSZOCD (WK-IX) IS NUMERIC
           AND DSP-MSZOCD (WK-IX) NOT = SPACE
           AND DSP-MSZOCD (WK-IX)     = DSP-MSZOCD(Y)
           MOVE  1               TO  FG-DUPLICATE
       END-IF
*
     END-PERFORM.
*
     IF  FG-DUPLICATE = 1
         IF  ERR-FLG = ZERO
             MOVE  11            TO  ERR-FLG
         END-IF
         MOVE  "R"               TO  EDIT-OPTION OF DSP-MSZOCD(Y)
         MOVE  "C"               TO  EDIT-CURSOR OF DSP-MSZOCD(Y)
         MOVE  "R"               TO  EDIT-OPTION OF DSP-MSTOKC(Y)
         MOVE  "C"               TO  EDIT-CURSOR OF DSP-MSTOKC(Y)
         GO TO  BODY-CHK-B-JIKAN-E
     END-IF.

** 自頁－他頁の重複チェック
     PERFORM  VARYING WK-IX  FROM 1 BY 1
              UNTIL   WK-IX  > 6
                   OR FG-DUPLICATE = 1
       IF  WK-IX NOT = P-CNT
           PERFORM  VARYING WK-IX2  FROM 1 BY 1
                    UNTIL   WK-IX2  > 12
             OR ((WK-IX - 1) * 12 + WK-IX2) > CT-TBLMAX
                         OR FG-DUPLICATE = 1
             IF      (TBL-MSZOCD (WK-IX WK-IX2)
                                     NOT = "ZZ" AND "YY")
                 AND TBL-MSZOCD (WK-IX WK-IX2)  = DSP-MSZOCD(Y)
                 MOVE  1     TO  FG-DUPLICATE
             END-IF
           END-PERFORM
       END-IF
*
     END-PERFORM.
*
     IF  FG-DUPLICATE = 1
         IF  ERR-FLG = ZERO
             MOVE  11            TO  ERR-FLG
         END-IF
         MOVE  "R"               TO  EDIT-OPTION OF DSP-MSZOCD(Y)
         MOVE  "C"               TO  EDIT-CURSOR OF DSP-MSZOCD(Y)
         MOVE  "R"               TO  EDIT-OPTION OF DSP-MSTOKC(Y)
         MOVE  "C"               TO  EDIT-CURSOR OF DSP-MSTOKC(Y)
         GO TO  BODY-CHK-B-JIKAN-E
     END-IF.
*
 BODY-CHK-B-JIKAN-E.
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
                UNTIL   IX-JIGMN-GYO > 12
          OR ((IX-JIGMN-PG - 1) * 12 + IX-JIGMN-GYO) > CT-TBLMAX
                     OR FG-DUPLICATE = 1
*
         IF  TBL-MSZOCD (IX-JIGMN-PG IX-JIGMN-GYO)
                     NOT = "ZZ" AND "YY"
*
             PERFORM  VARYING WK-IX2  FROM 1 BY 1
                      UNTIL   WK-IX2  > 6
                           OR FG-DUPLICATE = 1
               IF  WK-IX2 NOT = IX-JIGMN-PG
                   PERFORM  VARYING WK-IX3  FROM 1 BY 1
                            UNTIL   WK-IX3  > 12
                     OR ((WK-IX2 - 1) * 12 + WK-IX3) > CT-TBLMAX
                                 OR FG-DUPLICATE = 1
                     IF      (TBL-MSZOCD (WK-IX2 WK-IX3)
                                      NOT = "ZZ" AND "YY")
                         AND TBL-MSZOCD (WK-IX2 WK-IX3)
                           = TBL-MSZOCD (IX-JIGMN-PG IX-JIGMN-GYO)
                         MOVE  1     TO  FG-DUPLICATE
                     END-IF
                   END-PERFORM
               END-IF
*
             END-PERFORM
*
         END-IF
*
       END-PERFORM
*
     END-PERFORM.
*
     IF  FG-DUPLICATE = 1
         IF  ERR-FLG = ZERO
             MOVE  12            TO  ERR-FLG
         END-IF
     END-IF.
*
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
* DSP-MSZOCD   TBL-MSZOCD-INI
* ＝ SPACE    ＝ "ZZ"  ：データ無し行 ：入れ替えなし。
* ＝ SPACE    ≠ "ZZ"  ：行削除  ：TBL-MSZOCD <- "YY"
*
* ≠ SPACE    ＝ "ZZ"  ：行追加  ：TBL-MSZOCD<- DSP-MSZOCD
* ≠ SPACE    ≠ "ZZ"  ：行変更  ：TBL-MSZOCD<- DSP-MSZOCD
*
*項目画面セット
     PERFORM  VARYING Y  FROM 1 BY 1
              UNTIL   Y > 12
       IF      (    DSP-MSZOCD(Y) NOT = SPACE )
           AND (    DSP-MSTOKC(Y) NOT = ZERO
                AND DSP-MSTOKC(Y) NUMERIC )
*
           IF  (P-CNT - 1)  * 12 + Y > CT-TBLMAX
               COMPUTE CT-TBLMAX = (P-CNT - 1)  * 12 + Y
           END-IF
       END-IF
*
       EVALUATE  TRUE
**       データなし行は何もしない。
         WHEN      DSP-MSZOCD(Y)           = SPACE
               AND TBL-MSZOCD-INI(P-CNT Y) = "ZZ"
           CONTINUE
*
**       行削除
         WHEN      DSP-MSZOCD(Y)               = SPACE
               AND TBL-MSZOCD-INI(P-CNT Y) NOT = "ZZ"
          MOVE  "YY"             TO  TBL-MSZOCD (P-CNT Y)
*
**       行追加、行変更
         WHEN  OTHER
          MOVE  DSP-MSZOCD(Y)    TO  TBL-MSZOCD (P-CNT Y)
       END-EVALUATE
*
       MOVE  DSP-MSTOKC   (Y)    TO  TBL-MSTOKC (P-CNT Y)
*
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
         PERFORM  DCMTOKF-DEL-SEC
         PERFORM  DCMTOKF-WRITE-SEC
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
             IF       (    DSP-MSZOCD(12)  =  SPACE  )
                  AND (    DSP-MSTOKC(12)  =  ZERO    )
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
*             ＤＣＭ取引先設定マスタ削除位置づけ               *
****************************************************************
 DCMTOKF-DEL-SEC       SECTION.
     MOVE  "DCMTOKF-DEL-SEC"     TO  S-NAME.
*
     MOVE  SPACE                 TO  RD-FLG.
     PERFORM DCMTOKF-START-SEC.
     IF  CHK-FLG = "CHK"
         GO TO  DCMTOKF-DEL-EXIT
     END-IF.

     PERFORM DCMTOKF-READ-SEC.
     PERFORM DLT-SEC
             UNTIL  ( DSP-KYTOKC NOT = DCM-F01 ) OR
                    ( DSP-BLKCD  NOT = DCM-F02 ) OR
                    ( DSP-KBTOKC NOT = DCM-F03 ) OR
                    ( RD-FLG         = "END"   ).
*
 DCMTOKF-DEL-EXIT.
     EXIT.
****************************************************************
*             ＤＣＭ取引先設定マスタ削除                       *
****************************************************************
 DLT-SEC               SECTION.
     MOVE  "DLT-SEC"             TO  S-NAME.
*
     DELETE  DCMTOKF.
     PERFORM  DCMTOKF-READ-SEC.
*
 DLT-EXIT.
     EXIT.
****************************************************************
*             ＤＣＭ取引先設定マスタ作成                       *
****************************************************************
 DCMTOKF-WRITE-SEC     SECTION.
     MOVE  "DCMTOKF-WRITE-SEC"   TO  S-NAME.
*
     PERFORM  SDATE-GET-SEC.
*
     MOVE  1                     TO  Y.
     PERFORM  VARYING X  FROM 1 BY 1
              UNTIL   X > 6
       PERFORM  VARYING Y  FROM 1 BY 1
*               UNTIL   Y > 15
                UNTIL   Y > 12
         IF  TBL-MSZOCD(X Y)  NOT =  "ZZ" AND "YY"
             MOVE  SPACE            TO  DCM-REC
             INITIALIZE                 DCM-REC
             MOVE  DSP-KYTOKC       TO  DCM-F01
             MOVE  DSP-BLKCD        TO  DCM-F02
             MOVE  DSP-KBTOKC       TO  DCM-F03
             MOVE  TBL-MSZOCD(X Y)  TO  DCM-F04
*
             EVALUATE  TRUE
**             初期データが存在しない。
**             キーが変わっていたら登録
**             X:0 X:Y
**             登録
               WHEN  (    TBL-MSZOCD-INI (X Y) NOT = "ZZ"
                      AND TBL-MSZOCD (X Y) NOT =
                              TBL-MSZOCD-INI (X Y))
                  OR (TBL-MSZOCD-INI  (X Y) = "ZZ")
                 MOVE  TBL-MSTOKC     (X Y)  TO  DCM-F05
                 MOVE  PAR-TANCD             TO  DCM-F97
                 MOVE  SYS-DATE              TO  DCM-F98
                 MOVE  WK-TIME(1:4)          TO  DCM-F99
                 MOVE  SPACE                 TO  DCM-FIL1

**             修正
               WHEN  TBL-MSTOKC  (X Y) NOT =
                         TBL-MSTOKC-INI (X Y)
                 MOVE  TBL-MSTOKC     (X Y)  TO  DCM-F05
                 MOVE  TBL-TOROKU-TANT(X Y)  TO  DCM-F97
                 MOVE  TBL-TOROKUBI   (X Y)  TO  DCM-F98
                 MOVE  TBL-TOROKUTM   (X Y)  TO  DCM-F99
**             変更なし
               WHEN  OTHER
                 MOVE  TBL-MSTOKC     (X Y)  TO  DCM-F05
                 MOVE  TBL-TOROKU-TANT(X Y)  TO  DCM-F97
                 MOVE  TBL-TOROKUBI   (X Y)  TO  DCM-F98
                 MOVE  TBL-TOROKUTM   (X Y)  TO  DCM-F99
             END-EVALUATE
*
             WRITE  DCM-REC
         END-IF
       END-PERFORM
*
       MOVE  1                   TO  Y
     END-PERFORM.
*
 DCMTOKF-WRITE-EXIT.
     EXIT.
****************************************************************
*             終了処理                               3.0       *
****************************************************************
 END-SEC               SECTION.
     MOVE  "END-SEC"             TO  S-NAME.
*ファイル ＣＬＯＳＥ
     CLOSE  DSPFILE DCMTOKF
            HJYOKEN HTOKMS.
*
 END-EXIT.
     EXIT.
*****************<<  NSY0900I   END PROGRAM  >>******************

```
