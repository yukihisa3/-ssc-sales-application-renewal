****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　出荷管理システム　　　　　　　　　*
*    業務名　　　　　　　：　欠品リスト　　　　　　　　        *
*    モジュール名　　　　：　オンラインデータ欠品リスト　　　　*
*    作成日／更新日　　　：　99/10/18                          *
*    作成者／更新者　　　：　ＮＡＶ萩原                        *
*    処理概要　　　　　　：　　　　　　　　　　　　　　　　　　*
*    作成日／更新日　　　：　11/10/07 /YOSHIDA.M               *
*    更新概要　　　　　　：　基幹サーバ統合　　　　　　　　　　*
*                                                              *
****************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SSY0801L.
 AUTHOR.               HAGIWARA.
 DATE-WRITTEN.         99/10/18.
****************************************************************
 ENVIRONMENT           DIVISION.
****************************************************************
 CONFIGURATION         SECTION.
 SPECIAL-NAMES.
     YA           IS   CHR-2
     YB-21        IS   CHR-21
     YB           IS   CHR-15
     CONSOLE      IS   CONS.
*
 INPUT-OUTPUT          SECTION.
 FILE-CONTROL.
*画面ファイル
*売上伝票ファイル
     SELECT  SHTDENF   ASSIGN    TO        SHTDENLA
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      SEQUENTIAL
                       RECORD    KEY       DEN-F46
                                           DEN-F47
                                           DEN-F01
                                           DEN-F48
                                           DEN-F02
                                           DEN-F04
***2011.10.07 ST
***                                        DEN-F132
                                           DEN-F051
                                           DEN-F07
                                           DEN-F112
***2011.10.07 EN
                                           DEN-F03
                       FILE      STATUS    DEN-ST.
*取引先マスタ
     SELECT  HTOKMS    ASSIGN    TO        TOKMS2
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      RANDOM
                       RECORD    KEY       TOK-F01
                       FILE      STATUS    TOK-ST.
*店舗マスタ
     SELECT  HTENMS    ASSIGN    TO        TENMS1
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      RANDOM
                       RECORD    KEY       TEN-F52
                                           TEN-F011
                       FILE      STATUS    TEN-ST.
*倉庫マスタ
     SELECT  ZSOKMS    ASSIGN    TO        ZSOKMS1
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      RANDOM
                       RECORD    KEY       SOK-F01
                       FILE      STATUS    SOK-ST.
*画面定義ファイル
     SELECT  DSPFILE   ASSIGN    TO        GS-DSPF
                       FORMAT              DSP-FMT
                       GROUP               DSP-GRP
                       PROCESSING          DSP-PRO
                       FUNCTION            DSP-FNC
                       FILE      STATUS    DSP-ST.
*
*プリント定義ファイル
     SELECT     PRTFILE      ASSIGN    TO        LP-04-PRTF
                             FILE      STATUS    PRT-ST.
****************************************************************
 DATA                DIVISION.
****************************************************************
 FILE                SECTION.
****************************************************************
*    FILE = 売上伝票ファイル                     *
****************************************************************
 FD  SHTDENF
                       LABEL     RECORD    IS   STANDARD.
                       COPY      SHTDENF   OF   XFDLIB
                       JOINING   DEN       AS   PREFIX.
****************************************************************
*    FILE = 取引先マスタ                                       *
****************************************************************
 FD  HTOKMS
                       BLOCK     CONTAINS  8    RECORDS
                       LABEL     RECORD    IS   STANDARD.
                       COPY      HTOKMS    OF   XFDLIB
                       JOINING   TOK       AS   PREFIX.
****************************************************************
*  FILE= 店舗マスタ                                          *
****************************************************************
 FD  HTENMS
                       BLOCK     CONTAINS  8    RECORDS
                       LABEL     RECORD    IS   STANDARD.
                       COPY      HTENMS    OF   XFDLIB
                       JOINING   TEN       AS   PREFIX.
****************************************************************
*  FILE= 倉庫マスタ                                          *
****************************************************************
 FD  ZSOKMS
                       BLOCK     CONTAINS  8    RECORDS
                       LABEL     RECORD    IS   STANDARD.
                       COPY      ZSOKMS    OF   XFDLIB
                       JOINING   SOK       AS   PREFIX.
****************************************************************
*    FILE = 画面ファイル                                       *
****************************************************************
 FD  DSPFILE
                       LABEL     RECORD    IS   OMITTED.
                       COPY      FSY08011  OF   XMDLIB
                       JOINING   DSP       AS   PREFIX.
****************************************************************
*    FILE = プリントファイル                                   *
****************************************************************
 FD  PRTFILE
     LABEL       RECORD    IS        OMITTED.
 01  PRT-REC.
     03  FILLER            PIC X(200).
****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
*ステータス領域
 01  STATUS-AREA.
     03  DEN-ST                   PIC  X(02).
     03  TOK-ST                   PIC  X(02).
     03  TEN-ST                   PIC  X(02).
     03  SOK-ST                   PIC  X(02).
     03  DSP-ST                   PIC  X(02).
     03  PRT-ST                   PIC  X(02).
*画面制御用領域
 01  DSP-CONTROL.
     03  DSP-FMT                  PIC  X(08).
     03  DSP-GRP                  PIC  X(08).
     03  DSP-PRO                  PIC  X(02).
     03  DSP-FNC                  PIC  X(04).
*フラグ領域
 01  FLG-AREA.
     03  READ-FLG                 PIC  X(03)  VALUE  ZERO.
     03  ERR-FLG                  PIC  9(02)  VALUE  ZERO.
     03  END-FLG                  PIC  X(03)  VALUE  SPACE.
***  バッチ_の売上伝票Ｆへの存在チェック
     03  DEN-FLG                  PIC  9(01)  VALUE  ZERO.
*ワーク領域
 01  WRK-AREA.
***  プログラムスイッチ（画面遷移制御）
     03  PSW                      PIC  X(01)  VALUE  SPACE.
***  明細ブレイク判定
     03  WK-DENNO                 PIC  9(09)  VALUE  ZERO.
***  倉庫ブレイク判定
     03  WK-SOKO                  PIC  X(02)  VALUE  ZERO.
***  ラインカウント
     03  L-CNT                    PIC  9(02)  VALUE  ZERO.
***  ページカウント
     03  P-CNT                    PIC  9(03)  VALUE  ZERO.
***  伝票_１枚目判定
     03  RD-SW                    PIC  9(01)  VALUE  ZERO.
***  モード退避
     03  SAV-SHORI                PIC  9(01)  VALUE  ZERO.
***  開始倉庫コード
     03  SOKCD1                   PIC  X(02)  VALUE  ZERO.
***  終了倉庫コード
     03  SOKCD2                   PIC  X(02)  VALUE  ZERO.
***  欠品数量計（伝票毎）
     03  WK-KEPPINKEI1            PIC  9(09)  VALUE  ZERO.
***  欠品総原価計（伝票毎）
     03  WK-GENKAKEI1             PIC  9(09)  VALUE  ZERO.
***  欠品総売価計（伝票毎）
     03  WK-BAIKAKEI1             PIC  9(09)  VALUE  ZERO.
***  欠品数量計（総合計）
     03  WK-KEPPINKEI2            PIC  9(09)  VALUE  ZERO.
***  欠品総原価計（総合計）
     03  WK-GENKAKEI2             PIC  9(09)  VALUE  ZERO.
***  欠品総売価計（総合計）
     03  WK-BAIKAKEI2             PIC  9(09)  VALUE  ZERO.
***  エラーセクション名
 01  SEC-NAME.
     03  FILLER                   PIC  X(18)
         VALUE "### ERR-SEC    => ".
     03  S-NAME                   PIC  X(20).
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
*受信時間チェック
 01  WK-JIKAN.
     03  WK-HH                    PIC   9(02)  VALUE  ZERO.
     03  WK-MM                    PIC   9(02)  VALUE  ZERO.
*
*発注日、納品日編集
 01  WK-HENKAN-DATE               PIC   9(08)  VALUE  ZERO.
 01  WK-OUT-DATE.
     03  WK-OUT-YYYY              PIC   9(04)  VALUE  ZERO.
     03  WK-OUT-MM                PIC   9(02)  VALUE  ZERO.
     03  WK-OUT-DD                PIC   9(02)  VALUE  ZERO.
*
*日付論理チェック
 01  WK-CHKDATE.
     03  WK-CHKDATE-YYYY          PIC   9(04)  VALUE  ZERO.
     03  WK-CHKDATE-MM            PIC   9(02)  VALUE  ZERO.
     03  WK-CHKDATE-DD            PIC   9(02)  VALUE  ZERO.
*
*ＰＦガイド
 01  PF-MSG-AREA.
     03  PF-MSG1.
         05  FILLER               PIC   N(15)
             VALUE NC"_取消　_終了".
     03  PF-MSG2.
         05  FILLER               PIC   N(15)
             VALUE NC"_取消　_終了　_項目戻し".
 01  PF-MSG-AREA-R       REDEFINES     PF-MSG-AREA.
     03  PF-MSG-R   OCCURS   2   PIC   N(15).
*
*メッセージの取得
 01  ERR-MSG-AREA.
     03  ERR-MSG1.
         05  FILLER              PIC   N(20)
             VALUE NC"バッチ_を入力して下さい".
     03  ERR-MSG2.
         05  FILLER              PIC   N(20)
             VALUE NC"取引先マスタに登録されていません".
     03  ERR-MSG3.
         05  FILLER              PIC   N(20)
             VALUE NC"無効キーです".
     03  ERR-MSG4.
         05  FILLER              PIC   N(20)
             VALUE NC"バッチ_（日付）論理エラー".
     03  ERR-MSG5.
         05  FILLER              PIC   N(20)
             VALUE NC"バッチ_（時間）論理エラー".
     03  ERR-MSG6.
         05  FILLER              PIC   N(20)
             VALUE NC"正しい値を入力してください".
     03  ERR-MSG7.
         05  FILLER              PIC   N(20)
             VALUE NC"売上伝票ファイルに登録されていません".
     03  ERR-MSG8.
         05  FILLER              PIC   N(20)
             VALUE NC"倉庫マスタに登録されていません".
     03  ERR-MSG9.
         05  FILLER              PIC   N(20)
             VALUE NC"開始納品日を正しく入力して下さい".
     03  ERR-MSG10.
         05  FILLER              PIC   N(20)
             VALUE NC"終了納品日を正しく入力して下さい".
     03  ERR-MSG11.
         05  FILLER              PIC   N(20)
             VALUE NC"開始が終了を超えています".
 01  ERR-MSG-AREA-R      REDEFINES     ERR-MSG-AREA.
     03  ERR-MSG-R   OCCURS  11  PIC   N(20).
*
 01  FILE-ERR.
     03  DEN-ERR           PIC N(20) VALUE
                     NC"売上伝票ファイルエラー".
     03  TOK-ERR           PIC N(20) VALUE
                        NC"取引先マスタエラー".
     03  TEN-ERR           PIC N(20) VALUE
                        NC"店舗マスタエラー".
     03  SOK-ERR           PIC N(20) VALUE
                        NC"倉庫マスタエラー".
     03  DSP-ERR           PIC N(20) VALUE
                        NC"画面ファイルエラー".
     03  PRT-ERR           PIC N(20) VALUE
                        NC"プリンタファイルエラー".
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
*
****************************************************************
*    プリントエリア                                            *
****************************************************************
*--------------------------------------------------------------*
*    ヘッダ                                                    *
*--------------------------------------------------------------*
*
 01  HD1.
     03  FILLER                  PIC  X(02)  VALUE  SPACE.
     03  HD1-01                  PIC  X(08).
     03  FILLER                  PIC  X(37)  VALUE  SPACE.
     03  FILLER                  PIC  N(15)  VALUE
         NC"※※　欠　品　リ　ス　ト　※※"
                                 CHARACTER  TYPE  IS  CHR-21.
     03  FILLER                  PIC  X(21)  VALUE  SPACE.
     03  HD1-02                  PIC  9(04).
     03  FILLER                  PIC  N(01)  VALUE  NC"年"
                                 CHARACTER  TYPE  IS  CHR-2.
     03  HD1-03                  PIC  Z9.
     03  FILLER                  PIC  N(01)  VALUE  NC"月"
                                 CHARACTER  TYPE  IS  CHR-2.
     03  HD1-04                  PIC  Z9.
     03  FILLER                  PIC  N(01)  VALUE  NC"日"
                                 CHARACTER  TYPE  IS  CHR-2.
     03  FILLER                  PIC  X(01)  VALUE  SPACE.
     03  HD1-05                  PIC  ZZ9.
     03  FILLER                  PIC  N(01)  VALUE  NC"頁"
                                 CHARACTER  TYPE  IS  CHR-2.
*
 01  HD2.
     03  FILLER                  PIC  X(02)  VALUE  SPACE.
     03  FILLER                  PIC  X(04)  VALUE  "ﾊﾞｯﾁ".
     03  FILLER                  PIC  N(02)  VALUE NC"_："
                                 CHARACTER  TYPE  IS  CHR-2.
     03  FILLER                  PIC  X(01)  VALUE  SPACE.
     03  HD2-01                  PIC  9(08).
     03  FILLER                  PIC  X(01)  VALUE  "-".
     03  HD2-02                  PIC  9(04).
     03  FILLER                  PIC  X(01)  VALUE  "-".
     03  HD2-03                  PIC  9(08).
*
 01  HD3.
     03  FILLER                  PIC  X(02)  VALUE  SPACE.
     03  FILLER                  PIC  N(04)  VALUE NC"取引先："
                                 CHARACTER  TYPE  IS  CHR-2.
     03  FILLER                  PIC  X(01)  VALUE  SPACE.
     03  HD3-01                  PIC  N(15)
                                 CHARACTER  TYPE  IS  CHR-2.
*
 01  HD4                         CHARACTER   TYPE  IS  CHR-2.
     03  FILLER                  PIC  X(02)  VALUE  SPACE.
     03  FILLER                  PIC  N(04)  VALUE NC"倉庫　：".
     03  FILLER                  PIC  X(01)  VALUE  SPACE.
     03  HD4-01                  PIC  X(02)  VALUE  ZERO.
     03  FILLER                  PIC  X(01)  VALUE  SPACE.
     03  HD4-02                  PIC  N(18)  VALUE  SPACE.
     03  FILLER                  PIC  X(45)  VALUE  SPACE.
     03  FILLER                  PIC  N(08)
                                 VALUE  NC"出力納品日範囲：".
     03  HD4-03                  PIC  9(08).
     03  FILLER                  PIC  N(01)  VALUE  NC"～".
     03  HD4-04                  PIC  9(08).
*
 01  HD5                         CHARACTER   TYPE  IS  CHR-2.
     03  FILLER                  PIC  X(02)  VALUE  SPACE.
     03  FILLER                  PIC  N(02)  VALUE
                                 NC"店舗".
     03  FILLER                  PIC  X(24)  VALUE  SPACE.
     03  FILLER                  PIC  N(05)  VALUE
                                 NC"発　注　日".
     03  FILLER                  PIC  X(02)  VALUE  SPACE.
     03  FILLER                  PIC  N(05)  VALUE
                                 NC"納　品　日".
 01  HD6                         CHARACTER   TYPE  IS  CHR-2.
     03  FILLER                  PIC  X(02)  VALUE  SPACE.
     03  FILLER                  PIC  N(04)  VALUE
                                 NC"伝票番号".
     03  FILLER                  PIC  X(03)  VALUE  SPACE.
     03  FILLER                  PIC  N(01)  VALUE
                                 NC"行".
     03  FILLER                  PIC  X(02)  VALUE  SPACE.
     03  FILLER                  PIC  N(05)  VALUE
                                 NC"量販店商品".
     03  FILLER                  PIC  X(41)  VALUE  SPACE.
     03  FILLER                  PIC  N(04)  VALUE
                                 NC"発注数量".
     03  FILLER                  PIC  X(07)  VALUE  SPACE.
     03  FILLER                  PIC  N(04)  VALUE
                                 NC"訂正数量".
     03  FILLER                  PIC  X(08)  VALUE  SPACE.
     03  FILLER                  PIC  N(04)  VALUE
                                 NC"欠品数量".
     03  FILLER                  PIC  X(07)  VALUE  SPACE.
     03  FILLER                  PIC  N(03)  VALUE
                                 NC"原単価".
     03  FILLER                  PIC  X(04)  VALUE  SPACE.
     03  FILLER                  PIC  N(03)  VALUE
                                 NC"売単価".
*****03  FILLER                  PIC  X(02)  VALUE  SPACE.
*****03  FILLER                  PIC  N(02)  VALUE
*****                            NC"倉庫".
*
 01  SEN                         CHARACTER  TYPE  IS  CHR-2.
     03  FILLER                  PIC  N(25)  VALUE
         NC"─────────────────────────".
     03  FILLER                  PIC  N(25)  VALUE
         NC"─────────────────────────".
     03  FILLER                  PIC  N(18)  VALUE
         NC"──────────────────".
 01  SEN1.
     03  FILLER                  PIC  X(50)  VALUE
         "--------------------------------------------------".
     03  FILLER                  PIC  X(50)  VALUE
         "--------------------------------------------------".
     03  FILLER                  PIC  X(36)  VALUE
         "------------------------------------".
 01  DT1                         CHARACTER  TYPE  IS  CHR-2.
     03  FILLER                  PIC  X(02)  VALUE  SPACE.
     03  DT1-01                  PIC  ZZ999.
     03  FILLER                  PIC  X(01)  VALUE  SPACE.
     03  DT1-02                  PIC  N(10).
     03  FILLER                  PIC  X(02)  VALUE  SPACE.
     03  DT1-03-1                PIC  9999.
     03  DT1-03-2                PIC  X(01).
     03  DT1-03-3                PIC  Z9.
     03  DT1-03-4                PIC  X(01).
     03  DT1-03-5                PIC  Z9.
     03  FILLER                  PIC  X(02)  VALUE  SPACE.
     03  DT1-04-1                PIC  9999.
     03  DT1-04-2                PIC  X(01).
     03  DT1-04-3                PIC  Z9.
     03  DT1-04-4                PIC  X(01).
     03  DT1-04-5                PIC  Z9.
 01  DT2.
     03  FILLER                  PIC  X(02)  VALUE  SPACE.
     03  DT2-01                  PIC  9(09).
     03  FILLER                  PIC  X(02)  VALUE  SPACE.
     03  DT2-02                  PIC  Z9.
     03  FILLER                  PIC  X(01)  VALUE  SPACE.
     03  DT2-03                  PIC  X(13).
     03  FILLER                  PIC  X(01)  VALUE  SPACE.
     03  DT2-041                 PIC  X(15).
*****03  FILLER                  PIC  X(01)  VALUE  SPACE.
     03  DT2-042                 PIC  X(15).
     03  FILLER                  PIC  X(02)  VALUE  SPACE.
     03  DT2-05                  PIC  --,---,--9.99.
     03  FILLER                  PIC  X(01)  VALUE  SPACE.
     03  DT2-061                 PIC  X(01)  VALUE  "(".
     03  DT2-062                 PIC  --,---,--9.99.
     03  DT2-063                 PIC  X(01)  VALUE  ")".
     03  FILLER                  PIC  X(02)  VALUE  SPACE.
     03  DT2-07                  PIC  --,---,--9.99.
     03  FILLER                  PIC  X(02)  VALUE  SPACE.
     03  DT2-08                  PIC  ----,--9.99.
     03  FILLER                  PIC  X(02)  VALUE  SPACE.
     03  DT2-09                  PIC  ----,--9.
*****03  FILLER                  PIC  X(03)  VALUE  SPACE.
*****03  DT2-10                  PIC  99.
*
 01  DT3                         CHARACTER  TYPE  IS  CHR-2.
     03  FILLER                  PIC  X(49)  VALUE  SPACE.
     03  FILLER                  PIC  N(06)  VALUE
                                 NC"［欠品合計］".
     03  FILLER                  PIC  X(01)  VALUE  SPACE.
     03  DT3-01                  PIC  ---,---,--9.
     03  FILLER                  PIC  X(01)  VALUE  SPACE.
     03  FILLER                  PIC  N(08)  VALUE
                                 NC"［欠品総原価計］".
     03  FILLER                  PIC  X(01)  VALUE  SPACE.
     03  DT3-02                  PIC  ---,---,--9.
     03  FILLER                  PIC  X(01)  VALUE  SPACE.
     03  FILLER                  PIC  N(08)  VALUE
                                 NC"［欠品総売価計］".
     03  FILLER                  PIC  X(01)  VALUE  SPACE.
     03  DT3-03                  PIC  ---,---,--9.
*
 01  DT4                         CHARACTER  TYPE  IS  CHR-2.
     03  FILLER                  PIC  X(48)  VALUE  SPACE.
     03  FILLER                  PIC  N(23)  VALUE
         NC"対　象　デ　ー　タ　は　存　在　し　ま　せ　ん".
*
 01  TL1                         CHARACTER  TYPE  IS  CHR-2.
     03  FILLER                  PIC  X(34)  VALUE  SPACE.
     03  FILLER                  PIC  N(07)  VALUE
                                 NC"［取引先合計］".
     03  FILLER                  PIC  X(01)  VALUE  SPACE.
     03  FILLER                  PIC  N(06)  VALUE
                                 NC"［欠品合計］".
     03  FILLER                  PIC  X(01)  VALUE  SPACE.
     03  TL1-01                  PIC  ---,---,--9.
     03  FILLER                  PIC  X(01)  VALUE  SPACE.
     03  FILLER                  PIC  N(08)  VALUE
                                 NC"［欠品総原価計］".
     03  FILLER                  PIC  X(01)  VALUE  SPACE.
     03  TL1-02                  PIC  ---,---,--9.
     03  FILLER                  PIC  X(01)  VALUE  SPACE.
     03  FILLER                  PIC  N(08)  VALUE
                                 NC"［欠品総売価計］".
     03  FILLER                  PIC  X(01)  VALUE  SPACE.
     03  TL1-03                  PIC  ---,---,--9.
 LINKAGE                   SECTION.
 01  PARA-SOKO                   PIC  X(02).
 01  PARA-DSOKO                  PIC  X(02).
**************************************************************
 PROCEDURE             DIVISION  USING  PARA-SOKO PARA-DSOKO.
**************************************************************
 DECLARATIVES.
 DEN-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE SHTDENF.
     DISPLAY     DEN-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     DEN-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 TOK-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HTOKMS.
     DISPLAY     TOK-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     TOK-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 TEN-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HTENMS.
     DISPLAY     TEN-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     TEN-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 SOK-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE ZSOKMS.
     DISPLAY     SOK-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     SOK-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 DSP-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE DSPFILE.
     DISPLAY     DSP-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     DSP-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 PRT-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE PRTFILE.
     DISPLAY     PRT-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     PRT-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 END  DECLARATIVES.
****************************************************************
*             MAIN        MODULE                     0.0       *
****************************************************************
 PROCESS-START         SECTION.
     MOVE     "PROCESS START"     TO   S-NAME.
***
     PERFORM   INIT-SEC.
     PERFORM   MAIN-SEC          UNTIL   END-FLG   =   "END".
     PERFORM   END-SEC.
***
     STOP    RUN.
 PROCESS-EXIT.
     EXIT.
****************************************************************
*             初期処理                               1.0
****************************************************************
 INIT-SEC              SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
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
     MOVE      LINK-OUT-YMD       TO   SYS-DATE.
*画面表示日付編集
     MOVE      SYS-DATE(1:4)      TO   HEN-DATE-YYYY.
     MOVE      SYS-DATE(5:2)      TO   HEN-DATE-MM.
     MOVE      SYS-DATE(7:2)      TO   HEN-DATE-DD.
*システム時間取得
     ACCEPT    WK-TIME          FROM   TIME.
*画面表示時刻編集
     MOVE      WK-TIME(1:2)       TO   HEN-TIME-HH.
     MOVE      WK-TIME(3:2)       TO   HEN-TIME-MM.
     MOVE      WK-TIME(5:2)       TO   HEN-TIME-SS.
*ファイルのＯＰＥＮ
     OPEN     I-O       DSPFILE.
     OPEN     INPUT     SHTDENF   HTOKMS   HTENMS   ZSOKMS.
     OPEN     OUTPUT    PRTFILE.
*ワークの初期化
     INITIALIZE         FLG-AREA.
*
     MOVE     SPACE               TO   DSP-PRO.
     PERFORM  INIT-DSP-SEC.
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
*パラメタ入力
         WHEN      "1"  PERFORM   DSP-PARA-SEC
*確認入力
         WHEN      "2"  PERFORM   DSP-KAKU-SEC
*
         WHEN      OTHER  CONTINUE
     END-EVALUATE.
*
     CLOSE    PRTFILE   SHTDENF.
     OPEN     INPUT     SHTDENF.
     OPEN     OUTPUT    PRTFILE.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*             パラメタ入力( PSW = 1 )                2.1       *
****************************************************************
 DSP-PARA-SEC         SECTION.
     MOVE     "DSP-PARA-SEC"     TO   S-NAME.
*
     PERFORM    DSP-WRITE-SEC.
     PERFORM    DSP-READ-SEC.
*
     EVALUATE   DSP-FNC
*実行
         WHEN   "E000"
                PERFORM   PARA-CHK-SEC
*終了
         WHEN   "F005"
                MOVE    "END"    TO   END-FLG
*               MOVE    "4001"   TO   PROGRAM-STATUS
*取消
         WHEN   "F004"
                MOVE    "1"      TO   PSW
                PERFORM   INIT-DSP-SEC
         WHEN   OTHER
                MOVE     3       TO   ERR-FLG
                GO       TO      DSP-PARA-SEC
     END-EVALUATE.
*
 DSP-PARA-EXIT.
     EXIT.
****************************************************************
*             パラメタチェック                       2.1.1     *
****************************************************************
 PARA-CHK-SEC             SECTION.
     MOVE     "PARA-CHK-SEC"     TO   S-NAME.
*
*バッチ_（日付）チェック
***  バッチ_（日付）未入力チェック
     IF       DSP-BTDATE  NOT NUMERIC
         OR   DSP-BTDATE  =  ZERO
              MOVE   1       TO   ERR-FLG
              MOVE  "R"      TO   EDIT-OPTION  OF  DSP-BTDATE
              MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-BTDATE
              GO             TO   PARA-CHK-EXIT
     ELSE
***           バッチ_（日付）論理チェック
              MOVE     "2"            TO   LINK-IN-KBN
              MOVE     ZERO           TO   LINK-IN-YMD6
              MOVE     DSP-BTDATE     TO   LINK-IN-YMD8
              MOVE     ZERO           TO   LINK-OUT-RET
              MOVE     ZERO           TO   LINK-OUT-YMD
              CALL     "SKYDTCKB"     USING   LINK-IN-KBN
                                              LINK-IN-YMD6
                                              LINK-IN-YMD8
                                              LINK-OUT-RET
                                              LINK-OUT-YMD
              IF   LINK-OUT-RET   = 9
                   MOVE  4        TO   ERR-FLG
                   MOVE  "R"      TO   EDIT-OPTION  OF  DSP-BTDATE
                   MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-BTDATE
                   GO             TO   PARA-CHK-EXIT
              END-IF
*
              MOVE  "M"      TO   EDIT-OPTION  OF  DSP-BTDATE
              MOVE  SPACE    TO   EDIT-CURSOR  OF  DSP-BTDATE
     END-IF.
*
*バッチ_（時間）チェック
***  バッチ_（時間）未入力チェック
     IF       DSP-BTTIME  NOT NUMERIC
         OR   DSP-BTTIME  =   ZERO
                   MOVE   1       TO   ERR-FLG
                   MOVE  "R"      TO   EDIT-OPTION  OF  DSP-BTTIME
                   MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-BTTIME
                   GO             TO   PARA-CHK-EXIT
     ELSE
***           バッチ_（時間）論理チェック
              MOVE     DSP-BTTIME    TO  WK-JIKAN
              IF       WK-HH  <=  ZERO  OR  WK-HH  >  24 OR
                       WK-MM  <   ZERO  OR  WK-MM  >  59
                       IF     ERR-FLG  =  ZERO
                              MOVE    5    TO    ERR-FLG
                       END-IF
                       MOVE "R"    TO EDIT-OPTION OF DSP-BTTIME
                       MOVE "C"    TO EDIT-CURSOR OF DSP-BTTIME
                       GO          TO PARA-CHK-EXIT
              ELSE
                       MOVE "M"    TO EDIT-OPTION OF DSP-BTTIME
                       MOVE SPACE  TO EDIT-CURSOR OF DSP-BTTIME
              END-IF
     END-IF.
*
*バッチ_（取引先）チェック
***  バッチ_（取引先）未入力チェック
     IF       DSP-BTTORI  NOT NUMERIC
         OR   DSP-BTTORI  =  ZERO
              MOVE   1       TO   ERR-FLG
              MOVE  "R"      TO   EDIT-OPTION  OF  DSP-BTTORI
              MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-BTTORI
              GO             TO   PARA-CHK-EXIT
     ELSE
***           取引先ＲＥＡＤ
              MOVE     DSP-BTTORI TO   TOK-F01
              READ     HTOKMS
              INVALID
                     MOVE   2     TO   ERR-FLG
                     MOVE  "R"    TO   EDIT-OPTION  OF  DSP-BTTORI
                     MOVE  "C"    TO   EDIT-CURSOR  OF  DSP-BTTORI
                     MOVE   SPACE TO   DSP-TORINM
                     GO           TO   PARA-CHK-EXIT
              NOT INVALID
                     MOVE  "M"    TO   EDIT-OPTION  OF  DSP-BTTORI
                     MOVE  SPACE  TO   EDIT-CURSOR  OF  DSP-BTTORI
                     MOVE  TOK-F03  TO DSP-TORINM
              END-READ
     END-IF.
*
*    倉庫コードチェック
*****IF     ( DSP-SOKCD IS   NUMERIC   )   AND
************( DSP-SOKCD NOT =     ZERO )
     IF     ( DSP-SOKCD NOT =  SPACE   )
     AND    ( DSP-SOKCD NOT =  "00"    )
              MOVE      DSP-SOKCD TO   SOK-F01
              READ      ZSOKMS
                  INVALID  KEY
                        MOVE      SPACE     TO   DSP-SOKNM
                        IF   ERR-FLG   =    ZERO
                             MOVE      8    TO   ERR-FLG
                        END-IF
                        MOVE  "C"  TO  EDIT-CURSOR OF DSP-SOKCD
                        MOVE  "R"  TO  EDIT-OPTION OF DSP-SOKCD
              NOT INVALID  KEY
                        MOVE      SOK-F02   TO   DSP-SOKNM
***  画面の倉庫コードを開始，終了倉庫コードとして使用
                        MOVE      DSP-SOKCD TO   SOKCD1
                        MOVE      DSP-SOKCD TO   SOKCD2
                        MOVE  "M"  TO  EDIT-CURSOR OF DSP-SOKCD
                        MOVE SPACE TO  EDIT-OPTION OF DSP-SOKCD
              END-READ
     ELSE
**************MOVE      SPACE        TO DSP-SOKCD
              MOVE      NC"全倉庫"   TO DSP-SOKNM
***  全件時，ＺＥＲＯから９９
              MOVE      SPACE        TO SOKCD1
              MOVE      99           TO SOKCD2
              MOVE      "M"          TO EDIT-CURSOR OF DSP-SOKCD
              MOVE      SPACE        TO EDIT-OPTION OF DSP-SOKCD
     END-IF.
*納品日範囲チェック
*    （開始納品日チェック）
     IF       DSP-NOUDT1  NOT NUMERIC
     OR       DSP-NOUDT1  =   ZERO
              MOVE     ZERO  TO   DSP-NOUDT1
     END-IF.
*    （終了納品日チェック）
     IF       DSP-NOUDT2  NOT NUMERIC
     OR       DSP-NOUDT2  =   ZERO
              MOVE  99999999 TO   DSP-NOUDT2
     END-IF.
*    開始納品日論理チェック
     IF       DSP-NOUDT1  NOT =  ZERO
              MOVE     "2"            TO   LINK-IN-KBN
              MOVE     ZERO           TO   LINK-IN-YMD6
              MOVE     DSP-NOUDT1     TO   LINK-IN-YMD8
              MOVE     ZERO           TO   LINK-OUT-RET
              MOVE     ZERO           TO   LINK-OUT-YMD
              CALL     "SKYDTCKB"     USING   LINK-IN-KBN
                                              LINK-IN-YMD6
                                              LINK-IN-YMD8
                                              LINK-OUT-RET
                                              LINK-OUT-YMD
              IF   LINK-OUT-RET   = 9
                   MOVE   9       TO   ERR-FLG
                   MOVE  "R"      TO   EDIT-OPTION  OF  DSP-NOUDT1
                   MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-NOUDT1
                   GO             TO   PARA-CHK-EXIT
              ELSE
                   MOVE  "M"      TO   EDIT-OPTION  OF  DSP-NOUDT1
                   MOVE  SPACE    TO   EDIT-CURSOR  OF  DSP-NOUDT1
              END-IF
     END-IF.
*    終了納品日論理チェック
     IF       DSP-NOUDT2  NOT =  99999999
              MOVE     "2"            TO   LINK-IN-KBN
              MOVE     ZERO           TO   LINK-IN-YMD6
              MOVE     DSP-NOUDT2     TO   LINK-IN-YMD8
              MOVE     ZERO           TO   LINK-OUT-RET
              MOVE     ZERO           TO   LINK-OUT-YMD
              CALL     "SKYDTCKB"     USING   LINK-IN-KBN
                                              LINK-IN-YMD6
                                              LINK-IN-YMD8
                                              LINK-OUT-RET
                                              LINK-OUT-YMD
              IF   LINK-OUT-RET   = 9
                   MOVE  10       TO   ERR-FLG
                   MOVE  "R"      TO   EDIT-OPTION  OF  DSP-NOUDT2
                   MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-NOUDT2
                   GO             TO   PARA-CHK-EXIT
              ELSE
                   MOVE  "M"      TO   EDIT-OPTION  OF  DSP-NOUDT2
                   MOVE  SPACE    TO   EDIT-CURSOR  OF  DSP-NOUDT2
              END-IF
     END-IF.
*納品日入力範囲チェック
     IF       DSP-NOUDT1  >  DSP-NOUDT2
              MOVE  11       TO   ERR-FLG
              MOVE  "R"      TO   EDIT-OPTION  OF  DSP-NOUDT1
              MOVE  "R"      TO   EDIT-OPTION  OF  DSP-NOUDT2
              MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-NOUDT1
              GO             TO   PARA-CHK-EXIT
     ELSE
              MOVE  "M"      TO   EDIT-OPTION  OF  DSP-NOUDT2
              MOVE  "M"      TO   EDIT-OPTION  OF  DSP-NOUDT2
              MOVE  SPACE    TO   EDIT-CURSOR  OF  DSP-NOUDT1
     END-IF.
*売上伝票ファイル存在チェック
     PERFORM  DEN-START-SEC.
     IF       DEN-FLG   =    9
              MOVE      7    TO   ERR-FLG
              MOVE     "R"   TO   EDIT-OPTION  OF  DSP-BTDATE
              MOVE     "R"   TO   EDIT-OPTION  OF  DSP-BTTIME
              MOVE     "R"   TO   EDIT-OPTION  OF  DSP-BTTORI
              MOVE     "C"   TO   EDIT-CURSOR  OF  DSP-BTDATE
              MOVE           ZERO      TO    DEN-FLG
              GO             TO   PARA-CHK-EXIT
     ELSE
              MOVE     "2"   TO    PSW
              MOVE     "M"   TO   EDIT-OPTION  OF  DSP-BTDATE
              MOVE     "M"   TO   EDIT-OPTION  OF  DSP-BTTIME
              MOVE     "M"   TO   EDIT-OPTION  OF  DSP-BTTORI
              MOVE     SPACE TO   EDIT-CURSOR  OF  DSP-BTDATE
              CLOSE    SHTDENF
              OPEN     INPUT  SHTDENF
     END-IF.
*
 PARA-CHK-EXIT.
     EXIT.
****************************************************************
*             確認項目入力( PSW = 2 )                2.2       *
****************************************************************
 DSP-KAKU-SEC         SECTION.
     MOVE     "DSP-KAKU-SEC"     TO   S-NAME.
*
     PERFORM    DSP-WRITE-SEC.
     PERFORM    DSP-READ-SEC.
*
     EVALUATE   DSP-FNC
*実行
         WHEN   "E000"
                PERFORM  PRT-PROCESS-SEC
                MOVE    "1"      TO   PSW
*終了
         WHEN   "F005"
                MOVE    "END"    TO   END-FLG
*               MOVE    "4001"   TO   PROGRAM-STATUS
*項目戻し
         WHEN   "F006"
                MOVE    "1"      TO   PSW
*取消
         WHEN   "F004"
                MOVE    "1"      TO   PSW
                PERFORM   INIT-DSP-SEC
         WHEN   OTHER
                MOVE     6       TO   ERR-FLG
                GO       TO      DSP-KAKU-SEC
     END-EVALUATE.
*
 DSP-KAKU-EXIT.
     EXIT.
****************************************************************
*             PRT   MAIN       MODULE                3.0       *
****************************************************************
 PRT-PROCESS-SEC             SECTION.
     MOVE     "PRT-PROCESS-SEC" TO   S-NAME.
*
*ワーク初期化
     MOVE      ZERO     TO    P-CNT.
     MOVE      ZERO     TO    L-CNT.
     MOVE      ZERO     TO    RD-SW.
     MOVE      SPACE    TO    READ-FLG.
*
     PERFORM   PRT-INIT-SEC.
     PERFORM   PRT-MAIN-SEC   UNTIL     READ-FLG  =   "END".
*
 PRT-PROCESS-EXIT.
     EXIT.
****************************************************************
*             ＰＲＴファイル初期処理                 4.0       *
****************************************************************
 PRT-INIT-SEC                SECTION.
     MOVE     "PRT-INIT-SEC"    TO   S-NAME.
*
     PERFORM  DEN-START2-SEC.
     IF       DEN-FLG  NOT =  9
              PERFORM  DEN-READ-SEC
              IF       READ-FLG  = "END"
                       PERFORM  BODY-WRITE2-SEC
                       GO   TO   PRT-INIT-EXIT
              END-IF
              PERFORM  HEAD-WRITE-SEC
*ワーク初期値入力
              MOVE     DEN-F48   TO   WK-SOKO
     ELSE
              DISPLAY NC"＃＃印字スタートエラー＃＃"  UPON  CONS
              STOP  RUN
     END-IF.
*
 PRT-INIT-EXIT.
     EXIT.
****************************************************************
*             明細出力処理２　　                     4.1       *
****************************************************************
 BODY-WRITE2-SEC        SECTION.
     MOVE     "BODY-WRITE2-SEC"    TO   S-NAME.
*
*明細部出力
     WRITE    PRT-REC      FROM   DT4       AFTER  3.
*欠品リスト出力終了
     MOVE    "END"         TO     READ-FLG.
*
 BODY-WRITE2-EXIT.
     EXIT.
****************************************************************
*             ＰＲＴファイルメイン処理               5.0       *
****************************************************************
 PRT-MAIN-SEC                SECTION.
     MOVE     "PRT-MAIN-SEC"    TO   S-NAME.
*
*改ページ判定
     IF       L-CNT     >=   60
              MOVE      SPACE    TO    PRT-REC
              WRITE     PRT-REC  AFTER PAGE
              MOVE      ZERO     TO    L-CNT
              MOVE      ZERO     TO    RD-SW
              MOVE      ZERO     TO    WK-DENNO
              PERFORM   HEAD-WRITE-SEC
     END-IF.
*倉庫コードブレイク判定
     IF       DEN-F48   NOT = WK-SOKO
              PERFORM   TAIL-WRITE-SEC
              MOVE      SPACE    TO    PRT-REC
              WRITE     PRT-REC  AFTER PAGE
              MOVE      ZERO     TO    L-CNT
              MOVE      ZERO     TO    RD-SW
              MOVE      ZERO     TO    WK-DENNO
              MOVE      DEN-F48  TO    WK-SOKO
              PERFORM   HEAD-WRITE-SEC
     END-IF.
*
     IF       READ-FLG  NOT = "END"
              PERFORM   BODY-WRITE1-SEC
     ELSE
              GO   TO   PRT-MAIN-EXIT
     END-IF.
*
 PRT-MAIN-EXIT.
     EXIT.
****************************************************************
*             ヘッダ部出力処理                       5.1       *
****************************************************************
 HEAD-WRITE-SEC                  SECTION.
     MOVE     "HEAD-WRITE-SEC"    TO   S-NAME.
*ページカウント
     ADD      1         TO        P-CNT.
*項目設定
*ヘッダ１
***  ＰＧＩＤ
     MOVE     "SSY0801L"          TO        HD1-01.
***  日付
     MOVE     HEN-DATE-YYYY       TO        HD1-02.
     MOVE     HEN-DATE-MM         TO        HD1-03.
     MOVE     HEN-DATE-DD         TO        HD1-04.
***  ページ_
     MOVE     P-CNT               TO        HD1-05.
*
*ヘッダ２
***  バッチ_
     MOVE     DSP-BTDATE          TO        HD2-01.
     MOVE     DSP-BTTIME          TO        HD2-02.
     MOVE     DSP-BTTORI          TO        HD2-03.
*ヘッダ３
***  取引先名
     MOVE     DSP-BTTORI          TO        TOK-F01.
     READ     HTOKMS
       INVALID
              MOVE      SPACE     TO        HD3-01
       NOT INVALID
              MOVE      TOK-F03   TO        HD3-01
     END-READ.
*ヘッダ４
***  倉庫マスタＲＥＡＤ
     MOVE      DEN-F48        TO   HD4-01   SOK-F01.
     READ      ZSOKMS
       INVALID
               MOVE   SPACE   TO   HD4-02
       NOT INVALID
               MOVE   SOK-F02 TO   HD4-02
     END-READ.
*納品日範囲出力
     MOVE     DSP-NOUDT1      TO   HD4-03.
     MOVE     DSP-NOUDT2      TO   HD4-04.
*ヘッダ部出力
     WRITE    PRT-REC      FROM   HD1       AFTER  2.
     WRITE    PRT-REC      FROM   HD2       AFTER  2.
     WRITE    PRT-REC      FROM   HD3       AFTER  1.
     WRITE    PRT-REC      FROM   HD4       AFTER  1.
     WRITE    PRT-REC      FROM   SEN       AFTER  1.
     WRITE    PRT-REC      FROM   HD5       AFTER  1.
     WRITE    PRT-REC      FROM   HD6       AFTER  1.
     WRITE    PRT-REC      FROM   SEN       AFTER  1.
*
     ADD      10           TO        L-CNT.
 HEAD-WRITE-EXIT.
     EXIT.
****************************************************************
*             明細部出力処理１                       5.2       *
****************************************************************
 BODY-WRITE1-SEC              SECTION.
     MOVE     "BODY-WT-SEC"       TO   S-NAME.
*
     MOVE     SPACE     TO        DT1 DT2.
*
*明細部出力
***    伝票_ブレイク時，合計及び破線表示
     IF       DEN-F02   NOT =     WK-DENNO
              IF   RD-SW   =   ZERO
                   MOVE      1    TO   RD-SW
                   MOVE      DEN-F02   TO   WK-DENNO
              ELSE
***              合計欄出力
                   PERFORM   DEN-GOKEI-SEC
***              破線出力
                   WRITE     PRT-REC   FROM SEN1 AFTER 1
***
                   MOVE      DEN-F02   TO   WK-DENNO
                   ADD       1         TO   L-CNT
              END-IF
*    店舗，伝票_の重複表示不可
***           店舗コード
              MOVE      DEN-F07   TO   DT1-01  TEN-F011
***           店舗名称
              MOVE     DSP-BTTORI          TO   TEN-F52
              READ     HTENMS
                INVALID
                       MOVE       SPACE    TO   DT1-02
                NOT INVALID
                       MOVE       TEN-F03  TO   DT1-02
              END-READ
              MOVE     DEN-F111            TO   WK-HENKAN-DATE
              MOVE     WK-HENKAN-DATE      TO   WK-OUT-DATE
              MOVE     WK-OUT-YYYY         TO   DT1-03-1
              MOVE     "/"                 TO   DT1-03-2
              MOVE     WK-OUT-MM           TO   DT1-03-3
              MOVE     "/"                 TO   DT1-03-4
              MOVE     WK-OUT-DD           TO   DT1-03-5
              MOVE     DEN-F112            TO   WK-HENKAN-DATE
              MOVE     WK-HENKAN-DATE      TO   WK-OUT-DATE
              MOVE     WK-OUT-YYYY         TO   DT1-04-1
              MOVE     "/"                 TO   DT1-04-2
              MOVE     WK-OUT-MM           TO   DT1-04-3
              MOVE     "/"                 TO   DT1-04-4
              MOVE     WK-OUT-DD           TO   DT1-04-5
***  明細１行目出力
              WRITE    PRT-REC    FROM     DT1  AFTER  1
              ADD      1          TO       L-CNT
***  伝票_
              MOVE     DEN-F02    TO   DT2-01
     END-IF.
***  行
     MOVE     DEN-F03             TO   DT2-02.
***  量販店商品
     MOVE     DEN-F25             TO   DT2-03.
***  商品名１
     MOVE     DEN-F1421           TO   DT2-041.
***  商品名２
     MOVE     DEN-F1422           TO   DT2-042.
***  発注数量
     MOVE     DEN-F50             TO   DT2-05.
***  訂正数量
     MOVE     "("                 TO   DT2-061.
     MOVE     DEN-F15             TO   DT2-062.
     MOVE     ")"                 TO   DT2-063.
***  欠品数量
     COMPUTE  DT2-07 = DEN-F50 - DEN-F15.
***  原単価
     MOVE     DEN-F172            TO   DT2-08.
***  売単価
     MOVE     DEN-F173            TO   DT2-09.
***  倉庫コード
***  MOVE     DEN-F48             TO   DT2-10.
*合計
***伝票毎
***  欠品数量合計
     COMPUTE  WK-KEPPINKEI1 =  ( DEN-F50 - DEN-F15 )
                                  +  WK-KEPPINKEI1
***  欠品総原価計
     COMPUTE  WK-GENKAKEI1  = (( DEN-F50 - DEN-F15 ) * DEN-F512 )
                                  + WK-GENKAKEI1
***  欠品総売価計
     COMPUTE  WK-BAIKAKEI1  = (( DEN-F50 - DEN-F15 ) * DEN-F513 )
                                  + WK-BAIKAKEI1
***総合計
***  欠品数量合計
     COMPUTE  WK-KEPPINKEI2 =  ( DEN-F50 - DEN-F15 )
                                  +  WK-KEPPINKEI2
***  欠品総原価計
     COMPUTE  WK-GENKAKEI2  = (( DEN-F50 - DEN-F15 ) * DEN-F512 )
                                  + WK-GENKAKEI2
***  欠品総売価計
     COMPUTE  WK-BAIKAKEI2  = (( DEN-F50 - DEN-F15 ) * DEN-F513 )
                                  + WK-BAIKAKEI2
*伝票２行目出力
     WRITE    PRT-REC      FROM   DT2       AFTER  1.
*
     ADD      1    TO   L-CNT.

     PERFORM  DEN-READ-SEC.
     IF        READ-FLG    = "END"
               PERFORM     TAIL-WRITE-SEC
               GO   TO     BODY-WRITE1-EXIT
     END-IF.
*６０行以上で，かつ伝票_ブレイク時，合計行出力
     IF       L-CNT >= 60
        AND   DEN-F02   NOT =  WK-DENNO
              PERFORM   DEN-GOKEI-SEC
     END-IF.
*
 BODY-WRITE1-EXIT.
     EXIT.
****************************************************************
*             伝票毎合計欄出力処理                   5.2.1     *
****************************************************************
 DEN-GOKEI-SEC         SECTION.
     MOVE     "DEN-GOKEI-SEC"     TO   S-NAME.
*
***  合計欄出力
     MOVE      WK-KEPPINKEI1  TO   DT3-01.
     MOVE      WK-GENKAKEI1   TO   DT3-02.
     MOVE      WK-BAIKAKEI1   TO   DT3-03.
     WRITE     PRT-REC   FROM DT3  AFTER 2.
***  合計初期化
     MOVE      ZERO      TO   WK-KEPPINKEI1
                              WK-GENKAKEI1
                              WK-BAIKAKEI1.
*
     ADD       2         TO   L-CNT.
*
 DEN-GOKEI-EXIT.
     EXIT.
****************************************************************
*             テール部出力処理                       5.3       *
****************************************************************
 TAIL-WRITE-SEC        SECTION.
     MOVE     "TAIL-WRITE-SEC"    TO   S-NAME.
*
*最後の伝票毎合計を出力
     PERFORM   DEN-GOKEI-SEC.
*総合計出力
     MOVE      WK-KEPPINKEI2       TO    TL1-01.
     MOVE      WK-GENKAKEI2        TO    TL1-02.
     MOVE      WK-BAIKAKEI2        TO    TL1-03.
*
     WRITE     PRT-REC   FROM      TL1   AFTER  1.
*
     MOVE     ZERO TO   WK-KEPPINKEI2
                        WK-GENKAKEI2
                        WK-BAIKAKEI2.
 TAIL-WRITE-EXIT.
     EXIT.
****************************************************************
*             売上伝票ファイルＳＴＡＲＴ                       *
****************************************************************
 DEN-START-SEC         SECTION.
     MOVE     "DEN-START-SEC"     TO   S-NAME.
*
***2011.10.07 ST
     MOVE     SPACE          TO   DEN-REC.
     INITIALIZE                   DEN-REC.
***2011.10.07 EN
     MOVE     DSP-BTDATE     TO   DEN-F46.
     MOVE     DSP-BTTIME     TO   DEN-F47.
     MOVE     DSP-BTTORI     TO   DEN-F01.
*****MOVE     DSP-SOKCD      TO   DEN-F48.
     MOVE     SOKCD1         TO   DEN-F48.
***2011.10.07 ST
***  START    SHTDENF   KEY  IS   >=   DEN-F46
***                                    DEN-F47
***                                    DEN-F01
***                                    DEN-F48
     START    SHTDENF   KEY  IS   >=   DEN-F46
                                       DEN-F47
                                       DEN-F01
                                       DEN-F48
                                       DEN-F02
                                       DEN-F04
                                       DEN-F051
                                       DEN-F07
                                       DEN-F112
                                       DEN-F03
***2011.10.07 EN
        INVALID
              MOVE      9    TO   DEN-FLG
        NOT INVALID
***           売上ファイル存在チェック
              READ     SHTDENF
              END-READ
***           開始倉庫コードがゼロの時，バッチ_のみチェック
**************IF   DSP-SOKCD   = ZERO
              IF   SOKCD1  =   SPACE
                   IF  DSP-BTDATE =    DEN-F46 AND
                       DSP-BTTIME =    DEN-F47 AND
                       DSP-BTTORI =    DEN-F01
                       CONTINUE
                   ELSE
                       MOVE   9   TO   DEN-FLG
                   END-IF
***           ゼロ以外，開始倉庫コードまでチェック
              ELSE
                   IF  DSP-BTDATE =    DEN-F46 AND
                       DSP-BTTIME =    DEN-F47 AND
                       DSP-BTTORI =    DEN-F01 AND
                       DSP-SOKCD  =    DEN-F48
                       CONTINUE
                   ELSE
                       MOVE   9   TO   DEN-FLG
                   END-IF
              END-IF
     END-START.
 DEN-START-EXIT.
     EXIT.
****************************************************************
*             売上伝票ファイルＲＥＡＤ　                       *
****************************************************************
 DEN-READ-SEC          SECTION.
     MOVE     "DEN-READ-SEC"      TO   S-NAME.
*
     READ     SHTDENF       AT    END
              MOVE         "END"  TO   READ-FLG
              GO   TO   DEN-READ-EXIT
     END-READ.
*終了判定
     IF       DEN-F46   NOT =     DSP-BTDATE
          OR  DEN-F47   NOT =     DSP-BTTIME
          OR  DEN-F01   NOT =     DSP-BTTORI
          OR  DEN-F48       >     SOKCD2
**********OR  DEN-F48       >     DSP-SOKCD2
              MOVE     "END"      TO   READ-FLG
              GO   TO   DEN-READ-EXIT
     END-IF.
*訂正数量と発注数量が同じ物は読み飛ばし
     IF       DEN-F50   =   DEN-F15
              GO   TO   DEN-READ-SEC
     END-IF.
*納品日範囲内チェック
     IF       DSP-NOUDT1  >  DEN-F112
     OR       DSP-NOUDT2  <  DEN-F112
              GO   TO   DEN-READ-SEC
     END-IF.
*
 DEN-READ-EXIT.
     EXIT.
****************************************************************
*             画面表示処理                                     *
****************************************************************
 DSP-WRITE-SEC         SECTION.
     MOVE     "DSP-WRITE-SEC"     TO   S-NAME.
*エラーメッセージセット
     IF    ERR-FLG   =    ZERO
           MOVE    SPACE              TO   DSP-MSGSPC
     ELSE
           MOVE    ERR-MSG-R(ERR-FLG) TO   DSP-MSGSPC
           MOVE    ZERO               TO   ERR-FLG
     END-IF.
*ガイドメッセージの設定
     EVALUATE   PSW
***      パラメタ項目
         WHEN   "1"
                MOVE    PF-MSG-R(1)        TO   DSP-FNCSPC
***      確認
         WHEN   "2"
                MOVE    PF-MSG-R(2)        TO   DSP-FNCSPC
***      その他
         WHEN   OTHER
                MOVE    SPACE              TO   DSP-FNCSPC
     END-EVALUATE.
*
*画面の表示
     MOVE    "SCREEN"            TO   DSP-GRP.
     MOVE    "FSY08011"          TO   DSP-FMT.
     WRITE    DSP-FSY08011.
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
     EVALUATE   PSW
*パラメタ項目
         WHEN   "1"
                MOVE    "PARGRP"  TO   DSP-GRP
*確認
         WHEN   "2"
                MOVE    "CHKGRP"  TO   DSP-GRP
     END-EVALUATE.
*
     MOVE    "FSY08011"           TO   DSP-FMT.
     READ    DSPFILE.
*入力項目の属性を通常にする
 DSP-READ-010.
     MOVE    SPACE                TO   DSP-PRO.
*
 DSP-READ-EXIT.
     EXIT.
****************************************************************
*             初期画面表示                                     *
****************************************************************
 INIT-DSP-SEC          SECTION.
     MOVE     "INIT-DSP-SEC"      TO   S-NAME.
*画面の初期化
     MOVE    SPACE                TO   DSP-FSY08011.
*    MOVE   "FKY13011"            TO   DSP-FMT.
*システム日付転送
     MOVE    HEN-DATE             TO   DSP-SDATE.
*システム時間転送
     MOVE    HEN-TIME             TO   DSP-STIME.
*倉庫コードパラメタより倉庫コード入力チェック
     IF       PARA-DSOKO  =  "01"
*****IF       PARA-SOKO  =  0  OR  1
              MOVE    " "    TO   EDIT-STATUS OF DSP-SOKCD
     ELSE
              MOVE      PARA-SOKO TO   SOK-F01   DSP-SOKCD
              READ      ZSOKMS
                  INVALID  KEY
                        MOVE      ALL NC"＊" TO  DSP-SOKNM
              NOT INVALID  KEY
                        MOVE      SOK-F02    TO  DSP-SOKNM
              END-READ
              MOVE    "X"    TO   EDIT-STATUS OF DSP-SOKCD
     END-IF.
*項目属性クリア　
     PERFORM      DSP-SYOKI-SEC.
*
 INT-DSP-EXIT.
     EXIT.
****************************************************************
*             画面制御項目初期化                               *
****************************************************************
 DSP-SYOKI-SEC         SECTION.
     MOVE     "INIT-DSP-SEC"      TO   S-NAME.
*
*リバース，カーソルパーク解除
***  バッチ_（日付）
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-BTDATE.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-BTDATE.
***  バッチ_（時間）
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-BTTIME.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-BTTIME.
***  バッチ_（取引先）
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-BTTORI.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-BTTORI.
***  開始倉庫コード
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-SOKCD.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-SOKCD.
***  終了倉庫コード
***  MOVE    "M"      TO  EDIT-OPTION  OF  DSP-SOKCD2.
***  MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-SOKCD2.
***  開始納品日
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-NOUDT1.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-NOUDT1.
***  終了納品日
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-NOUDT2.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-NOUDT2.
*
 DSP-SYOKI-EXIT.
     EXIT.
****************************************************************
*             終了処理                               6.0       *
****************************************************************
 END-SEC               SECTION.
*ファイル ＣＬＯＳＥ
     CLOSE             SHTDENF  HTOKMS  HTENMS  DSPFILE  PRTFILE
                       ZSOKMS.
**
 END-EXIT.
     EXIT.
****************************************************************
*             売上伝票ファイルＳＴＡＲＴ                       *
****************************************************************
 DEN-START2-SEC         SECTION.
     MOVE     "DEN-START2-SEC"     TO   S-NAME.
*
***2011.10.07 ST
     MOVE     SPACE          TO   DEN-REC.
     INITIALIZE                   DEN-REC.
***2011.10.07 EN
     MOVE     DSP-BTDATE     TO   DEN-F46.
     MOVE     DSP-BTTIME     TO   DEN-F47.
     MOVE     DSP-BTTORI     TO   DEN-F01.
     MOVE     DSP-SOKCD      TO   DEN-F48.
***2011.10.07 ST
***  START    SHTDENF   KEY  IS   >=   DEN-F46
***                                    DEN-F47
***                                    DEN-F01
***                                    DEN-F48
     START    SHTDENF   KEY  IS   >=   DEN-F46
                                       DEN-F47
                                       DEN-F01
                                       DEN-F48
                                       DEN-F02
                                       DEN-F04
                                       DEN-F051
                                       DEN-F07
                                       DEN-F112
                                       DEN-F03
***2011.10.07 EN
        INVALID
              MOVE      9    TO   DEN-FLG
     END-START.
 DEN-START2-EXIT.
     EXIT.
*****************<<  SSY0801L   END PROGRAM  >>******************
