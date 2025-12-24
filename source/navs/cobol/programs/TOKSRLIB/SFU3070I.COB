****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　ＡＣＯＳ振替改善　　　　　　　　　*
*    業務名　　　　　　　：　ＡＣＯＳ振替改善                  *
*    モジュール名　　　　：　社内振替情報照会（明細）　        *
*    作成日／作成者　　　：　2017.01.25   /  T.TAKAHASHI       *
*    更新日／更新者　　　：　　　　　　                        *
*    処理概要　　　　　　：　社内振替情報の明細照会を行う。    *
*                                                              *
****************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SFU3070I.
 AUTHOR.               TAKAHAASHI.
 DATE-WRITTEN.         17/01/25.
****************************************************************
 ENVIRONMENT           DIVISION.
****************************************************************
 CONFIGURATION         SECTION.
 SPECIAL-NAMES.
     CONSOLE      IS   CONS.
*
 INPUT-OUTPUT          SECTION.
 FILE-CONTROL.
*社内振替明細情報ファイル
     SELECT  SFRMEIF   ASSIGN    TO        SFRMEIL3
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      SEQUENTIAL
                       RECORD    KEY       SFR-F01
                                           SFR-F02
                                           SFR-F03
                                           SFR-F04
                                           SFR-F05
                                           SFR-F06
                                           SFR-F07
                                           SFR-F08
                                           SFR-F12
                                           SFR-F11
                       FILE      STATUS    SFR-ST.
*倉庫マスタ
     SELECT  ZSOKMS    ASSIGN    TO        ZSOKMS1
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      RANDOM
                       RECORD    KEY       SOK-F01
                       FILE      STATUS    SOK-ST.
*条件ファイル
     SELECT  HJYOKEN   ASSIGN    TO        JYOKEN1
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      RANDOM
                       RECORD    KEY       JYO-F01
                                           JYO-F02
                       FILE      STATUS    JYO-ST.
*仕入先マスタ
     SELECT  ZSHIMS    ASSIGN    TO        ZSHIMS1
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      RANDOM
                       RECORD    KEY       SHI-F01
                       FILE      STATUS    SHI-ST.
*担当者マスタ
     SELECT  HTANMS    ASSIGN    TO        TANMS1
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      RANDOM
                       RECORD    KEY       TAN-F01
                                           TAN-F02
                       FILE      STATUS    TAN-ST.
*商品名称マスタ
     SELECT  HMEIMS    ASSIGN    TO        MEIMS1
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      RANDOM
                       RECORD    KEY       MEI-F011
                                           MEI-F0121
                                           MEI-F0122
                                           MEI-F0123
                       FILE      STATUS    TAN-ST.
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
*    FILE = 社内振替明細情報ファイル                           *
****************************************************************
 FD  SFRMEIF
                       LABEL     RECORD    IS   STANDARD.
                       COPY      SFRMEIF   OF   XFDLIB
                       JOINING   SFR       AS   PREFIX.
****************************************************************
*    FILE = 倉庫マスタ　　　                                   *
****************************************************************
 FD  ZSOKMS
                       LABEL     RECORD    IS   STANDARD.
                       COPY      ZSOKMS    OF   XFDLIB
                       JOINING   SOK       AS   PREFIX.
****************************************************************
*    FILE = 条件ファイル                                       *
****************************************************************
 FD  HJYOKEN
                       LABEL     RECORD    IS   STANDARD.
                       COPY      HJYOKEN   OF   XFDLIB
                       JOINING   JYO       AS   PREFIX.
****************************************************************
*    FILE = 仕入先マスタ                                       *
****************************************************************
 FD  ZSHIMS
                       LABEL     RECORD    IS   STANDARD.
                       COPY      ZSHIMS    OF   XFDLIB
                       JOINING   SHI       AS   PREFIX.
****************************************************************
*    FILE = 担当者マスタ                                       *
****************************************************************
 FD  HTANMS
                       LABEL     RECORD    IS   STANDARD.
                       COPY      HTANMS    OF   XFDLIB
                       JOINING   TAN       AS   PREFIX.
****************************************************************
*    FILE = 商品名称マスタ                                     *
****************************************************************
 FD  HMEIMS
                       LABEL     RECORD    IS   STANDARD.
                       COPY      HMEIMS    OF   XFDLIB
                       JOINING   MEI       AS   PREFIX.
****************************************************************
*    FILE = 画面ファイル                                       *
****************************************************************
 FD  DSPFILE
                       LABEL     RECORD    IS   OMITTED.
                       COPY      FFU30701  OF   XMDLIB
                       JOINING   DSP       AS   PREFIX.
*
****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
*ステータス領域
 01  STATUS-AREA.
     03  SFR-ST                   PIC  X(02).
     03  SOK-ST                   PIC  X(02).
     03  JYO-ST                   PIC  X(02).
     03  SHI-ST                   PIC  X(02).
     03  TAN-ST                   PIC  X(02).
     03  MEI-ST                   PIC  X(02).
     03  DSP-ST                   PIC  X(02).
*画面制御用領域
 01  DSP-CONTROL.
     03  DSP-FMT                  PIC  X(08).
     03  DSP-GRP                  PIC  X(08).
     03  DSP-PRO                  PIC  X(02).
     03  DSP-FNC                  PIC  X(04).
*フラグ領域
 01  FLG-AREA.
     03  ZSOKMS-INV-FLG           PIC  X(03)  VALUE  SPACE.
     03  HJYOKEN-INV-FLG          PIC  X(03)  VALUE  SPACE.
     03  ZSHIMS-INV-FLG           PIC  X(03)  VALUE  SPACE.
     03  HTANMS-INV-FLG           PIC  X(03)  VALUE  SPACE.
     03  HMEIMS-INV-FLG           PIC  X(03)  VALUE  SPACE.
     03  ERR-FLG                  PIC  9(01)  VALUE  ZERO.
     03  END-FLG                  PIC  X(03)  VALUE  SPACE.
     03  TAIS-FLG                 PIC  X(03)  VALUE  SPACE.
     03  P-CNT                    PIC S9(01)  VALUE  ZERO.
     03  G-CNT                    PIC S9(01)  VALUE  ZERO.
     03  C-CNT                    PIC S9(01)  VALUE  ZERO.
     03  X                        PIC  9(02)  VALUE  ZERO.
     03  Y                        PIC  9(02)  VALUE  ZERO.
*ワーク領域
 01  WRK-AREA.
***  プログラムスイッチ（画面遷移制御）
     03  PSW                      PIC  X(01)  VALUE  SPACE.
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
*特販部名称編集
 01  HEN-TOKHAN-AREA.
     03  FILLER                   PIC  N(01)  VALUE  NC"（".
     03  HEN-TOKHAN               PIC  N(06)  VALUE  SPACE.
     03  FILLER                   PIC  N(01)  VALUE  NC"）".
 01  HND-DATE.
     03  HND-DATE-YYYY            PIC  9(04)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  "/".
     03  HND-DATE-MM              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  "/".
     03  HND-DATE-DD              PIC  9(02)  VALUE  ZERO.
 01  HEN-TANCD.
     03  HEN-TANCD1               PIC  X(04)  VALUE  SPACE.
     03  HEN-TANCD2               PIC  X(01)  VALUE  SPACE.
     03  HEN-TANCD3               PIC  X(02)  VALUE  SPACE.
*
*ＰＦガイド
 01  PF-MSG-AREA.
     03  PF-MSG1.
         05  FILLER               PIC   N(20)
      VALUE NC"_終了　　　　　　　　　　　".
     03  PF-MSG2.
         05  FILLER               PIC   N(20)
      VALUE NC"_終了_前頁_次頁".
 01  PF-MSG-AREA-R       REDEFINES     PF-MSG-AREA.
     03  PF-MSG-R   OCCURS   2   PIC   N(20).
*
*メッセージの取得
 01  ERR-MSG-AREA.
     03  ERR-MSG1.
         05  FILLER              PIC   N(20)
             VALUE NC"表示対象の明細情報が存在しません！".
     03  ERR-MSG2.
         05  FILLER              PIC   N(20)
             VALUE NC"無効キーです。".
     03  ERR-MSG3.
         05  FILLER              PIC   N(20)
             VALUE NC"前頁はありません。　　　".
     03  ERR-MSG4.
         05  FILLER              PIC   N(20)
             VALUE NC"次頁はありません。　　　".
     03  ERR-MSG5.
         05  FILLER              PIC   N(20)
             VALUE NC"入力されたコードは存在しません。".
     03  ERR-MSG6.
         05  FILLER              PIC   N(20)
             VALUE NC"入力されたコードの名称が存在しません。".
 01  ERR-MSG-AREA-R      REDEFINES     ERR-MSG-AREA.
     03  ERR-MSG-R   OCCURS  6   PIC   N(20).
*商品変換テーブル退避エリア
 01  TABLE-AREA.
     03  TBL-HSOKCD               PIC   X(02).
     03  TBL-HSOKNM               PIC   N(08).
     03  TBL-NENDO                PIC   9(04).
     03  TBL-SEASON               PIC   X(02).
     03  TBL-DENKU                PIC   X(02).
     03  TBL-DENKUN               PIC   N(04).
     03  TBL-HSYOCD               PIC   X(08).
     03  TBL-HHINT1               PIC   X(05).
     03  TBL-HHINT2               PIC   X(02).
     03  TBL-HHINT3               PIC   X(01).
     03  TBL-SIRCD                PIC   9(08).
     03  TBL-SIRNM                PIC   N(15).
     03  TBL-SYONM1               PIC   N(15).
     03  TBL-SYONM2               PIC   N(15).
     03  TBL-HACGK                PIC  S9(09).
     03  TBL-NYUGK                PIC  S9(09).
*
     03  TABLE1      OCCURS  6.
         05  TABLE2  OCCURS  15.
             07  TBL-HNMEI        PIC   N(05).
             07  TBL-HNDATE       PIC   X(10).
             07  TBL-HACSUU       PIC  S9(09).
             07  TBL-NYKSUU       PIC  S9(09).
             07  TBL-TANCD        PIC   X(07).
             07  TBL-TANNM        PIC   N(08).
*
*ファイルエラーメッセージ
 01  FILE-ERR.
     03  SFR-ERR           PIC N(15) VALUE
                        NC"社内振替明細情報Ｆエラー".
     03  SOK-ERR           PIC N(15) VALUE
                        NC"倉庫マスタエラー".
     03  JYO-ERR           PIC N(15) VALUE
                        NC"条件ファイルエラー".
     03  SHI-ERR           PIC N(15) VALUE
                        NC"仕入先マスタエラー".
     03  TAN-ERR           PIC N(15) VALUE
                        NC"担当者マスタエラー".
     03  MEI-ERR           PIC N(15) VALUE
                        NC"商品名称マスタエラー".
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
 LINKAGE                    SECTION.
 01  PARA-DENKU            PIC X(02).
 01  PARA-NENDO            PIC 9(04).
 01  PARA-SEASON           PIC X(02).
 01  PARA-SOKCD            PIC X(02).
 01  PARA-SYOCD            PIC X(08).
 01  PARA-HINTAN1          PIC X(05).
 01  PARA-HINTAN2          PIC X(02).
 01  PARA-HINTAN3          PIC X(01).
 01  PARA-SIRCD            PIC 9(08).
 01  PARA-HACGK            PIC S9(09).
 01  PARA-NYUGK            PIC S9(09).
**************************************************************
 PROCEDURE             DIVISION   USING  PARA-DENKU
                                         PARA-NENDO
                                         PARA-SEASON
                                         PARA-SOKCD
                                         PARA-SYOCD
                                         PARA-HINTAN1
                                         PARA-HINTAN2
                                         PARA-HINTAN3
                                         PARA-SIRCD
                                         PARA-HACGK
                                         PARA-NYUGK.
**************************************************************
 DECLARATIVES.
 SFR-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE SFRMEIF.
     MOVE        SFR-ST    TO        E-ST.
     MOVE       "SFRMEIL3" TO        E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     SFR-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
*****STOP        RUN.
     EXIT PROGRAM.
 SOK-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE ZSOKMS.
     MOVE        SOK-ST    TO        E-ST.
     MOVE       "ZSOKMS1"  TO        E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     SOK-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
*****STOP        RUN.
     EXIT PROGRAM.
 JYO-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HJYOKEN.
     MOVE        JYO-ST    TO        E-ST.
     MOVE       "JYOKEN1"  TO        E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     JYO-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
*****STOP        RUN.
     EXIT PROGRAM.
 SHI-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE ZSHIMS.
     MOVE        SHI-ST    TO        E-ST.
     MOVE       "ZSHIMS1"  TO        E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     SHI-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
*****STOP        RUN.
     EXIT PROGRAM.
 TAN-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HTANMS.
     MOVE        TAN-ST    TO        E-ST.
     MOVE       "TANMS1"   TO        E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     TAN-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
*****STOP        RUN.
     EXIT PROGRAM.
 DSP-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE DSPFILE.
     MOVE        DSP-ST    TO        E-ST.
     MOVE        "DSPFILE" TO        E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     DSP-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
*****STOP        RUN.
     EXIT PROGRAM.
 END  DECLARATIVES.
****************************************************************
*             MAIN        MODULE                     0.0       *
****************************************************************
 PROCESS-START         SECTION.
     MOVE     "PROCESS-START"     TO   S-NAME.
***
     PERFORM   INIT-SEC.
     PERFORM   MAIN-SEC          UNTIL   END-FLG   =   "END".
     PERFORM   END-SEC.
***
*****STOP    RUN.
     EXIT PROGRAM.
 CONTROL-EXIT.
     EXIT.
****************************************************************
*             初期処理                               1.0
****************************************************************
 INIT-SEC              SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
*ファイルのＯＰＥＮ
     OPEN      INPUT    SFRMEIF ZSOKMS HJYOKEN ZSHIMS HTANMS.
     OPEN      I-O      DSPFILE.
*ワークの初期化
     INITIALIZE                 FLG-AREA.
*
*システム日付・時刻の取得
     ACCEPT   WK-DATE           FROM   DATE.
     MOVE     "3"                 TO   LINK-IN-KBN.
     MOVE     WK-DATE             TO   LINK-IN-YMD6.
     MOVE     ZERO                TO   LINK-IN-YMD8.
     MOVE     ZERO                TO   LINK-OUT-RET.
     MOVE     ZERO                TO   LINK-OUT-YMD.
     CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD.
     MOVE      LINK-OUT-YMD       TO   DATE-AREA.
*画面表示日付編集
     MOVE      SYS-DATE(1:4)      TO   HEN-DATE-YYYY.
     MOVE      SYS-DATE(5:2)      TO   HEN-DATE-MM.
     MOVE      SYS-DATE(7:2)      TO   HEN-DATE-DD.
*システム日付取得
     ACCEPT    WK-TIME          FROM   TIME.
*画面表示時刻編集
     MOVE      WK-TIME(1:2)       TO   HEN-TIME-HH.
     MOVE      WK-TIME(3:2)       TO   HEN-TIME-MM.
     MOVE      WK-TIME(5:2)       TO   HEN-TIME-SS.
*初期画面の表示
     MOVE     SPACE               TO   DSP-PRO.
*ヘッド入力へ
     MOVE    "1"                  TO   PSW.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*             メイン処理                             2.0
****************************************************************
 MAIN-SEC              SECTION.
     MOVE     "MAIN-SEC"     TO   S-NAME.
*
     EVALUATE      PSW
*初期画面表示(2.1)
         WHEN      "1"       PERFORM   DSP-INIT-SEC
*ヘッダ部入力(2.2)
         WHEN      "2"       PERFORM   DSP-SET-SEC
*確認入力    (2.3)
         WHEN      "3"       PERFORM   DSP-KAKU-SEC
*以外
         WHEN      OTHER     CONTINUE
     END-EVALUATE.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*             終了処理                               3.0       *
****************************************************************
 END-SEC               SECTION.
     MOVE     "END-SEC"          TO   S-NAME.
*ファイル ＣＬＯＳＥ
     CLOSE    SFRMEIF ZSOKMS HJYOKEN ZSHIMS HTANMS DSPFILE.
**
 END-EXIT.
     EXIT.
****************************************************************
*             初期画面表示( PSW = 1 )                2.1       *
****************************************************************
 DSP-INIT-SEC          SECTION.
     MOVE     "DSP-INIT-SEC"      TO   S-NAME.
*初期画面の表示
     MOVE     SPACE               TO   DSP-PRO.
*画面の初期化
     MOVE    SPACE                TO   DSP-FFU30701.
*PGID/FORMID
     MOVE    "SFU3070I"           TO   DSP-PGID.
     MOVE    "FFU30701"           TO   DSP-FORMID.
*システム日付転送
     MOVE    HEN-DATE             TO   DSP-SDATE.
*システム時間転送
     MOVE    HEN-TIME             TO   DSP-STIME.
*明細情報退避初期画面セット
     MOVE     "2"                 TO   PSW.
*
 DSP-INIT-EXIT.
     EXIT.
****************************************************************
*             ヘッダ部入力( PSW = 2 )                2.2       *
****************************************************************
 DSP-SET-SEC           SECTION.
     MOVE     "DSP-SET-SEC"      TO   S-NAME.
*倉庫ＣＤ／倉庫名
     MOVE       PARA-SOKCD       TO   TBL-HSOKCD.
     MOVE       PARA-SOKCD       TO   SOK-F01.
     PERFORM ZSOKMS-READ-SEC.
     IF  ZSOKMS-INV-FLG = "INV"
                MOVE  ALL NC"＊" TO   TBL-HSOKNM
     ELSE
         MOVE   SOK-F02          TO   TBL-HSOKNM
     END-IF.
*年度
     MOVE       PARA-NENDO       TO   TBL-NENDO.
*シーズン
     MOVE       PARA-SEASON      TO   TBL-SEASON.
*伝票区分／伝票区分名
     MOVE       PARA-DENKU       TO   TBL-DENKU.
     MOVE       1                TO   JYO-F01.
     MOVE       PARA-DENKU       TO   JYO-F02.
     PERFORM  HJYOKEN-READ-SEC.
     IF  HJYOKEN-INV-FLG = "INV"
         MOVE  ALL NC"＊"        TO   TBL-DENKUN
     ELSE
         MOVE  JYO-F03           TO   TBL-DENKUN
     END-IF.
*商品情報
     MOVE    PARA-SYOCD          TO   TBL-HSYOCD  MEI-F011.
     MOVE    PARA-HINTAN1        TO   TBL-HHINT1  MEI-F0121.
     MOVE    PARA-HINTAN2        TO   TBL-HHINT2  MEI-F0122.
     MOVE    PARA-HINTAN3        TO   TBL-HHINT3  MEI-F0123.
*仕入先ＣＤ／仕入先名
     MOVE    PARA-SIRCD          TO   TBL-SIRCD.
     MOVE    PARA-SIRCD          TO   SHI-F01.
     PERFORM ZSHIMS-READ-SEC.
     IF  ZSHIMS-INV-FLG = "INV"
         MOVE  ALL NC"＊"        TO   TBL-SIRNM
     ELSE
         MOVE  SHI-F02           TO   TBL-SIRNM
     END-IF.
*発注合計数
     MOVE  PARA-HACGK            TO   TBL-HACGK.
*入荷合計数
     MOVE  PARA-NYUGK            TO   TBL-NYUGK.
*ヘッダ部表示
     MOVE    TBL-HSOKCD          TO   DSP-HSOKCD.
     MOVE    TBL-HSOKNM          TO   DSP-HSOKNM.
     MOVE    TBL-NENDO           TO   DSP-NENDO.
     MOVE    TBL-SEASON          TO   DSP-SEASON.
     MOVE    TBL-DENKU           TO   DSP-DENKU.
     MOVE    TBL-DENKUN          TO   DSP-DENKUN.
     MOVE    TBL-HSYOCD          TO   DSP-HSYOCD.
     MOVE    TBL-HHINT1          TO   DSP-HHINT1.
     MOVE    TBL-HHINT2          TO   DSP-HHINT2.
     MOVE    TBL-HHINT3          TO   DSP-HHINT3.
     MOVE    TBL-SIRCD           TO   DSP-SIRCD.
     MOVE    TBL-SIRNM           TO   DSP-SIRNM.
     MOVE    TBL-SYONM1          TO   DSP-SYONM1.
     MOVE    TBL-SYONM2          TO   DSP-SYONM2.
     MOVE    TBL-HACGK           TO   DSP-HACGK.
     MOVE    TBL-NYUGK           TO   DSP-NYUGK.
*社内振替明細情報ファイルスタート
     PERFORM   SFRMEIF-START-SEC.
*
     IF  TAIS-FLG  =  "END"
         MOVE  1        TO   ERR-FLG
         MOVE  "R"      TO   EDIT-OPTION  OF  DSP-HSOKCD
         MOVE  "R"      TO   EDIT-OPTION  OF  DSP-HSOKNM
         MOVE  "R"      TO   EDIT-OPTION  OF  DSP-SEASON
         MOVE  "R"      TO   EDIT-OPTION  OF  DSP-DENKU
         MOVE  "R"      TO   EDIT-OPTION  OF  DSP-HSYOCD
         MOVE  "R"      TO   EDIT-OPTION  OF  DSP-HHINT1
         MOVE  "R"      TO   EDIT-OPTION  OF  DSP-HHINT2
         MOVE  "R"      TO   EDIT-OPTION  OF  DSP-HHINT3
         MOVE  "R"      TO   EDIT-OPTION  OF  DSP-SIRCD
         MOVE  "R"      TO   EDIT-OPTION  OF  DSP-SYONM1
         MOVE  "R"      TO   EDIT-OPTION  OF  DSP-SYONM2
         GO                      TO   DSP-HEAD-010
     END-IF.
*
     MOVE    SPACE      TO   TABLE1(1).
     MOVE    SPACE      TO   TABLE1(2).
     MOVE    SPACE      TO   TABLE1(3).
     MOVE    SPACE      TO   TABLE1(4).
     MOVE    SPACE      TO   TABLE1(5).
     MOVE    SPACE      TO   TABLE1(6).
     PERFORM VARYING X FROM 1 BY 1 UNTIL X > 6
                                      OR TAIS-FLG = "END"
             MOVE    X         TO     G-CNT
             PERFORM VARYING Y FROM 1 BY 1 UNTIL Y > 15
                                              OR TAIS-FLG = "END"
                          PERFORM  FILE-WORK-SEC
                          PERFORM  SFRMEIF-READ-SEC
             END-PERFORM
     END-PERFORM.
*１ページ目の情報をセット
     MOVE    1                   TO   P-CNT.
     PERFORM WORK-DSP-SEC.
*
 DSP-HEAD-010.
     MOVE     "3"                 TO   PSW.
*
 DSP-HEAD-EXIT.
     EXIT.
****************************************************************
*    ワーク⇒画面セット処理　　　　　　　　　　　　　　        *
****************************************************************
 WORK-DSP-SEC             SECTION.
     MOVE     "WORK-DSP-SEC"     TO   S-NAME.
*
     MOVE    TBL-HSOKCD          TO   DSP-HSOKCD.
     MOVE    TBL-HSOKNM          TO   DSP-HSOKNM.
     MOVE    TBL-NENDO           TO   DSP-NENDO.
     MOVE    TBL-SEASON          TO   DSP-SEASON.
     MOVE    TBL-DENKU           TO   DSP-DENKU.
     MOVE    TBL-DENKUN          TO   DSP-DENKUN.
     MOVE    TBL-HSYOCD          TO   DSP-HSYOCD.
     MOVE    TBL-HHINT1          TO   DSP-HHINT1.
     MOVE    TBL-HHINT2          TO   DSP-HHINT2.
     MOVE    TBL-HHINT3          TO   DSP-HHINT3.
     MOVE    TBL-SIRCD           TO   DSP-SIRCD.
     MOVE    TBL-SIRNM           TO   DSP-SIRNM.
     MOVE    TBL-SYONM1          TO   DSP-SYONM1.
     MOVE    TBL-SYONM2          TO   DSP-SYONM2.
     MOVE    TBL-HACGK           TO   DSP-HACGK.
     MOVE    TBL-NYUGK           TO   DSP-NYUGK.
*
     PERFORM VARYING Y FROM 1 BY 1 UNTIL Y > 15
             MOVE   SPACE        TO   DSP-MAS001(Y)
             IF  TBL-HNMEI(P-CNT Y)  NOT =  SPACE
                 MOVE  TBL-HNMEI(P-CNT Y)     TO   DSP-HNMEI(Y)
                 MOVE  TBL-HNDATE(P-CNT Y)    TO   DSP-HNDATE(Y)
                 MOVE  TBL-HACSUU(P-CNT Y)    TO   DSP-HACSUU(Y)
                 MOVE  TBL-NYKSUU(P-CNT Y)    TO   DSP-NYKSUU(Y)
                 MOVE  TBL-TANCD(P-CNT Y)     TO   DSP-TANCD(Y)
                 MOVE  TBL-TANNM(P-CNT Y)     TO   DSP-TANNM(Y)
             END-IF
     END-PERFORM.
*
 WORK-DSP-EXIT.
     EXIT.
****************************************************************
*             ヘッダ部チェック                 2.2.1           *
****************************************************************
 FILE-WORK-SEC            SECTION.
     MOVE     "FILE-WORK-SEC"    TO   S-NAME.
*明細種別
     EVALUATE  SFR-F91
         WHEN  " "  MOVE  NC"手入力"     TO  TBL-HNMEI(X Y)
         WHEN  "1"  MOVE  NC"ＥＸＣＥＬ" TO  TBL-HNMEI(X Y)
         WHEN OTHER MOVE  ALL NC"＊"     TO  TBL-HNMEI(X Y)
     END-EVALUATE.
*日付
     MOVE      SFR-F12(1:4)      TO   HND-DATE-YYYY.
     MOVE      SFR-F12(5:2)      TO   HND-DATE-MM.
     MOVE      SFR-F12(7:2)      TO   HND-DATE-DD.
     MOVE      HND-DATE          TO   TBL-HNDATE(X Y).
*発注数／入荷数
     IF  SFR-F11  =  "1"
         MOVE  SFR-F14           TO   TBL-HACSUU(X Y)
*********MOVE  ZERO              TO   TBL-NYKSUU(X Y)
     END-IF.
     IF  SFR-F11  =  "2"
*********MOVE  ZERO              TO   TBL-HACSUU(X Y)
         MOVE  SFR-F14           TO   TBL-NYKSUU(X Y)
     END-IF.
*担当者ＣＤ／担当者名
     MOVE     SFR-F96            TO   HEN-TANCD1.
     MOVE     "-"                TO   HEN-TANCD2.
     MOVE     SFR-F97            TO   HEN-TANCD3.
     MOVE     HEN-TANCD          TO   TBL-TANCD(X Y).
     MOVE     SFR-F96            TO   TAN-F01.
     MOVE     SFR-F97            TO   TAN-F02.
     PERFORM  HTANMS-READ-SEC.
     IF  HTANMS-INV-FLG = "INV"
         MOVE  ALL NC"＊"        TO   TBL-TANNM(X Y)
     ELSE
         MOVE  TAN-F03           TO   TBL-TANNM(X Y)
     END-IF.
*商品名
     MOVE     SFR-F09            TO   TBL-SYONM1.
     MOVE     SFR-F10            TO   TBL-SYONM2.
*
 FILE-WORK-EXIT.
     EXIT.
****************************************************************
*    社内振替明細情報ファイルスタート　                        *
****************************************************************
 SFRMEIF-START-SEC     SECTION.
     MOVE  "SFRMEIF-START-SEC" TO   S-NAME.
*ワークテーブルクリア
     MOVE   SPACE              TO   SFR-REC.
     INITIALIZE                     SFR-REC.
*
     MOVE     PARA-DENKU       TO   SFR-F01.
     MOVE     PARA-NENDO       TO   SFR-F02.
     MOVE     PARA-SEASON      TO   SFR-F03.
     MOVE     PARA-SOKCD       TO   SFR-F04.
     MOVE     PARA-SYOCD       TO   SFR-F05.
     MOVE     PARA-HINTAN1     TO   SFR-F06.
     MOVE     PARA-HINTAN2     TO   SFR-F07.
     MOVE     PARA-HINTAN3     TO   SFR-F08.
     START  SFRMEIF  KEY  IS  >=    SFR-F01  SFR-F02  SFR-F03
                                    SFR-F04  SFR-F05  SFR-F06
                                    SFR-F07  SFR-F08  SFR-F12
                                    SFR-F11
            INVALID
              MOVE   "END"     TO   TAIS-FLG
              GO               TO   SFRMEIF-START-EXIT
     END-START.
*
     PERFORM  SFRMEIF-READ-SEC.
*
 SFRMEIF-START-EXIT.
     EXIT.
****************************************************************
*    社内振替明細情報ファイル読込　　　                        *
****************************************************************
 SFRMEIF-READ-SEC      SECTION.
     MOVE  "SFRMEIF-READ-SEC"  TO   S-NAME.
*ワークテーブルクリア
     READ  SFRMEIF  NEXT  AT  END
                    MOVE "END" TO   TAIS-FLG
                    GO         TO   SFRMEIF-READ-EXIT
     END-READ.
*指定キーチェック
     IF    PARA-DENKU   =  SFR-F01
     AND   PARA-NENDO   =  SFR-F02
     AND   PARA-SEASON  =  SFR-F03
     AND   PARA-SOKCD   =  SFR-F04
     AND   PARA-SYOCD   =  SFR-F05
     AND   PARA-HINTAN1 =  SFR-F06
     AND   PARA-HINTAN2 =  SFR-F07
     AND   PARA-HINTAN3 =  SFR-F08
           CONTINUE
     ELSE
           MOVE   "END"        TO   TAIS-FLG
     END-IF.
*
 SFRMEIF-READ-EXIT.
     EXIT.
****************************************************************
*             確認処理　入力（ PSW = 3 ）            2.4
****************************************************************
 DSP-KAKU-SEC          SECTION.
     MOVE     "DSP-KAKU-SEC"      TO   S-NAME.
*
     PERFORM    DSP-WRITE-SEC.
     PERFORM    DSP-READ-SEC.
*
     EVALUATE   DSP-FNC
*終了
         WHEN   "F005"
                MOVE    "END"    TO   END-FLG
*前頁
         WHEN   "F011"
                COMPUTE C-CNT = P-CNT - 1
                IF      C-CNT <= ZERO
                        MOVE  3  TO  ERR-FLG
                ELSE
                        COMPUTE P-CNT = P-CNT - 1
                        PERFORM WORK-DSP-SEC
                END-IF
*次頁
         WHEN   "F012"
                COMPUTE C-CNT = P-CNT + 1
                IF      C-CNT > G-CNT
                        MOVE   4   TO   ERR-FLG
                ELSE
                        COMPUTE P-CNT = P-CNT + 1
                        PERFORM WORK-DSP-SEC
                END-IF
         WHEN   OTHER
                MOVE     2       TO   ERR-FLG
     END-EVALUATE.
*
 DSP-KAKU-EXIT.
     EXIT.
****************************************************************
*             画面表示処理                                     *
****************************************************************
 DSP-WRITE-SEC         SECTION.
     MOVE     "DSP-WRITE-SEC"     TO   S-NAME.
*エラーメッセージセット
     IF    ERR-FLG   =    ZERO
           MOVE    SPACE              TO   DSP-ERRMSG
     ELSE
           MOVE    ERR-MSG-R(ERR-FLG) TO   DSP-ERRMSG
           MOVE    ZERO               TO   ERR-FLG
     END-IF.
*ガイドメッセージの設定
     MOVE    PF-MSG2             TO   DSP-PFMSG.
*画面の表示
     MOVE    "SCREEN"            TO   DSP-GRP.
     MOVE    "FFU30701"          TO   DSP-FMT.
     WRITE    DSP-FFU30701.
*
 DSP-WRITE-EXIT.
     EXIT.
****************************************************************
*             画面読込処理                                     *
****************************************************************
 DSP-READ-SEC          SECTION.
     MOVE     "DSP-READ-SEC"      TO   S-NAME.
*
     MOVE    "NE"                 TO   DSP-PRO.
*
     MOVE    "KAKU"               TO   DSP-GRP.
     MOVE    "FFU30701"           TO   DSP-FMT.
     READ    DSPFILE.
*入力項目の属性を通常にする
 DSP-READ-010.
*    MOVE    ZERO                 TO   ERR-FLG.
     MOVE    SPACE                TO   DSP-PRO.
*
 DSP-READ-EXIT.
     EXIT.
****************************************************************
*    倉庫マスタ読込　　　　　                                  *
****************************************************************
 ZSOKMS-READ-SEC       SECTION.
     MOVE "ZSOKMS-READ-SEC"       TO   S-NAME.
*
     READ  ZSOKMS
           INVALID     MOVE  "INV"  TO   ZSOKMS-INV-FLG
           NOT INVALID MOVE  SPACE  TO   ZSOKMS-INV-FLG
     END-READ.
*
 ZSOKMS-READ-EXIT.
     EXIT.
****************************************************************
*    商品名称マスタ読込　　　                                  *
****************************************************************
 HMEIMS-READ-SEC       SECTION.
     MOVE "HMEIMS-READ-SEC"       TO   S-NAME.
*
     READ  HMEIMS
           INVALID     MOVE  "INV"  TO   HMEIMS-INV-FLG
           NOT INVALID MOVE  SPACE  TO   HMEIMS-INV-FLG
     END-READ.
*
 HMEIMS-READ-EXIT.
     EXIT.
****************************************************************
*    条件ファイル読込　　　　                                  *
****************************************************************
 HJYOKEN-READ-SEC      SECTION.
     MOVE "HJYOKEN-READ-SEC"      TO   S-NAME.
*
     READ  HJYOKEN
           INVALID     MOVE  "INV"  TO   HJYOKEN-INV-FLG
           NOT INVALID MOVE  SPACE  TO   HJYOKEN-INV-FLG
     END-READ.
*
 HJYOKEN-READ-EXIT.
     EXIT.
****************************************************************
*    仕入先マスタ読込　　　　                                  *
****************************************************************
 ZSHIMS-READ-SEC       SECTION.
     MOVE "ZSHIMS-READ-SEC"       TO   S-NAME.
*
     READ  ZSHIMS
           INVALID     MOVE  "INV"  TO   ZSHIMS-INV-FLG
           NOT INVALID MOVE  SPACE  TO   ZSHIMS-INV-FLG
     END-READ.
*
 ZSHIMS-READ-EXIT.
     EXIT.
****************************************************************
*    担当者マスタ読込　　　　                                  *
****************************************************************
 HTANMS-READ-SEC       SECTION.
     MOVE "HTANMA-READ-SEC"       TO   S-NAME.
*
     READ  HTANMS
           INVALID     MOVE  "INV"  TO   HTANMS-INV-FLG
           NOT INVALID MOVE  SPACE  TO   HTANMS-INV-FLG
     END-READ.
*
 HTANMS-READ-EXIT.
     EXIT.
*******************< PROGRAM-END SFU3070I >*********************
