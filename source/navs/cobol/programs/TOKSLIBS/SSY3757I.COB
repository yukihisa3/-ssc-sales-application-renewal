***********************************************************
*                                                         *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　 *
*    サブシステム　　　　：　ナフコＥＤＩ受信システム　   *
*    業務名　　　　　　　：　ナフコＥＤＩ受信             *
*    モジュール名　　　　：　出荷変更入力                 *
*    作成日／更新日　　　：　2010/10/12                   *
*    作成者／更新者　　　：　ＮＡＶ飯田                   *
*    処理概要　　　　　　：　　　　　　　　　　　　　　　 *
*      ナフコ基本情報ファイルの作場ＣＤ、数量、箱数の     *
*      変更をおこなう。関連し、売上伝票ファイルの数量、   *
*      在庫マスタの引当済数、未出庫数の変更も行う。       *
*                                                         *
***********************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SSY3757I.
 AUTHOR.               S.I.
 DATE-WRITTEN.         2010/10/12.
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
     SELECT  DSPFILE
       ASSIGN      TO  GS-DSPF
       FORMAT          DSP-FMT
       GROUP           DSP-GRP
       PROCESSING      DSP-PRO
       FUNCTION        DSP-FNC
       FILE STATUS     DSP-ST.
*基本情報ファイル
     SELECT  NFJOHOF
       ASSIGN        TO  NFJOHOL3
       ORGANIZATION  IS  INDEXED
       ACCESS MODE   IS  DYNAMIC
       RECORD KEY    IS  KH1-F01 *> 管理番号
                         KH1-F05 *> 作場ＣＤ
                         KH1-F13 *> 相手商品ＣＤ
                         KH1-F06 *> 店舗ＣＤ
       FILE STATUS   IS  KH1-ST.
*基本情報ファイル２
     SELECT  NFJOHOF2
       ASSIGN        TO  NFJOHOL4
       ORGANIZATION  IS  INDEXED
       ACCESS MODE   IS  DYNAMIC
       RECORD KEY    IS  KH2-F02 *> バッチ日付
                         KH2-F03 *> バッチ時間
                         KH2-F04 *> バッチ取引先
                         KH2-F05 *> 作場ＣＤ
                         KH2-F13 *> 相手商品ＣＤ
                         KH2-F06 *> 店舗ＣＤ
       FILE STATUS   IS  KH2-ST.
*基本情報ファイル３（基本キー）
     SELECT  NFJOHOF3
       ASSIGN        TO  NFJOHOL1
       ORGANIZATION  IS  INDEXED
       ACCESS MODE   IS  RANDOM
       RECORD KEY    IS  KH3-F01 *> 管理番号
                         KH3-F05 *> 作場ＣＤ
                         KH3-F06 *> 店舗ＣＤ
                         KH3-F07 *> 伝票番号
                         KH3-F08 *> 行番
                         KH3-F09 *> 納品日
       FILE STATUS   IS  KH3-ST.

*作場マスタ
     SELECT  SAKUBAF
       ASSIGN        TO  SAKUBAL1
       ORGANIZATION  IS  INDEXED
       ACCESS MODE   IS  RANDOM
       RECORD KEY    IS  SKB-F01
       FILE STATUS   IS  SKB-ST.
*店舗マスタ
     SELECT  HTENMS
       ASSIGN        TO  TENMS1
       ORGANIZATION  IS  INDEXED
       ACCESS MODE   IS  RANDOM
       RECORD KEY    IS  TEN-F52  *> 相手取引先ＣＤ
                         TEN-F011 *> 店舗コード
       FILE STATUS   IS  TEN-ST.
*商品変換ＴＢＬ
     SELECT  HSHOTBL
       ASSIGN        TO  SHOTBL1
       ORGANIZATION  IS  INDEXED
       ACCESS MODE   IS  RANDOM
       RECORD KEY    IS  SCV-F01 *> 相手取引先ＣＤ
                         SCV-F02 *> 相手商品コード
       FILE STATUS   IS  SCV-ST.
*ナフコ商品名称マスタ
     SELECT  NFMEIMS
       ASSIGN        TO  NFMEIMS1
       ORGANIZATION  IS  INDEXED
       ACCESS MODE   IS  RANDOM
       RECORD KEY    IS  SYO-F01 *> ナフコ商品ＣＤ
       FILE STATUS   IS  SYO-ST.
*取引先マスタ
     SELECT  HTOKMS
       ASSIGN        TO  TOKMS2
       ORGANIZATION  IS  INDEXED
       ACCESS MODE   IS  RANDOM
       RECORD KEY    IS  TOK-F01 *> 相手取引先ＣＤ
       FILE STATUS   IS  TOK-ST.
*在庫マスタ
     SELECT  ZAMZAIF
       ASSIGN        TO  ZAMZAIL1
       ORGANIZATION  IS  INDEXED
       ACCESS MODE   IS  RANDOM
       RECORD KEY    IS  ZAK-F01  *> 倉庫コード
                         ZAK-F021 *> 商品ＣＤ
                         ZAK-F022 *> 品単
                         ZAK-F031 *> _番１
                         ZAK-F032 *> _番２
                         ZAK-F033 *> _番３
       FILE STATUS   IS  ZAK-ST.
*売上伝票ファイル
     SELECT  SHTDENF
       ASSIGN        TO  SHTDENL1
       ORGANIZATION  IS  INDEXED
       ACCESS MODE   IS  RANDOM
       RECORD KEY    IS  URI-F01  *> 取引先コード
                         URI-F02  *> 伝票番号
                         URI-F04  *> 相殺区分
                         URI-F051 *> 伝区コード
                         URI-F03  *> 行番号
       FILE STATUS   IS  URI-ST.

****************************************************************
 DATA                DIVISION.
****************************************************************
 FILE                SECTION.
****************************************************************
*    FILE = 画面ファイル                                       *
****************************************************************
 FD  DSPFILE
                       LABEL     RECORD    IS   OMITTED.
                       COPY      FSY37571  OF   XMDLIB
                       JOINING   DSP       AS   PREFIX.
****************************************************************
*    FILE = 基本情報ファイル                                   *
****************************************************************
 FD  NFJOHOF
                       LABEL     RECORD    IS   STANDARD.
                       COPY      NFJOHOF   OF   XFDLIB
                       JOINING   KH1       AS   PREFIX.
****************************************************************
*    FILE = 基本情報ファイル２                                 *
****************************************************************
 FD  NFJOHOF2
                       LABEL     RECORD    IS   STANDARD.
                       COPY      NFJOHOF   OF   XFDLIB
                       JOINING   KH2       AS   PREFIX.

****************************************************************
*    FILE = 基本情報ファイル３                                 *
****************************************************************
 FD  NFJOHOF3
                       LABEL     RECORD    IS   STANDARD.
                       COPY      NFJOHOF   OF   XFDLIB
                       JOINING   KH3       AS   PREFIX.

****************************************************************
*    FILE = 作場マスタ                                         *
****************************************************************
 FD  SAKUBAF
                       LABEL     RECORD    IS   STANDARD.
                       COPY      SAKUBAF   OF   XFDLIB
                       JOINING   SKB       AS   PREFIX.
****************************************************************
*    FILE = 店舗マスタ                                         *
****************************************************************
 FD  HTENMS
                       LABEL     RECORD    IS   STANDARD.
                       COPY      HTENMS    OF   XFDLIB
                       JOINING   TEN       AS   PREFIX.
****************************************************************
*    FILE = 商品変換ＴＢＬ                                     *
****************************************************************
 FD  HSHOTBL
                       LABEL     RECORD    IS   STANDARD.
                       COPY      HSHOTBL   OF   XFDLIB
                       JOINING   SCV       AS   PREFIX.
****************************************************************
*    FILE = ナフコ商品名称マスタ                               *
****************************************************************
 FD  NFMEIMS
                       LABEL     RECORD    IS   STANDARD.
                       COPY      NFMEIMS   OF   XFDLIB
                       JOINING   SYO       AS   PREFIX.
****************************************************************
*    FILE = 取引先マスタ                                       *
****************************************************************
 FD  HTOKMS
                       LABEL     RECORD    IS   STANDARD.
                       COPY      HTOKMS    OF   XFDLIB
                       JOINING   TOK       AS   PREFIX.
****************************************************************
*    FILE = 在庫マスタ                                         *
****************************************************************
 FD  ZAMZAIF
                       LABEL     RECORD    IS   STANDARD.
                       COPY      ZAMZAIF   OF   XFDLIB
                       JOINING   ZAK       AS   PREFIX.
****************************************************************
*    FILE = 売上伝票ファイル                                   *
****************************************************************
 FD  SHTDENF
                       LABEL     RECORD    IS   STANDARD.
                       COPY      SHTDENF   OF   XFDLIB
                       JOINING   URI       AS   PREFIX.
****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
*ステータス領域
 01  STATUS-AREA.
     03  DSP-ST                   PIC  X(02).
     03  KH1-ST                   PIC  X(02).
     03  KH2-ST                   PIC  X(02).
     03  KH3-ST                   PIC  X(02).
     03  SKB-ST                   PIC  X(02).
     03  TEN-ST                   PIC  X(02).
     03  SCV-ST                   PIC  X(02).
     03  SYO-ST                   PIC  X(02).
     03  TOK-ST                   PIC  X(02).
     03  ZAK-ST                   PIC  X(02).
     03  URI-ST                   PIC  X(02).
*画面制御用領域
 01  DSP-CONTROL.
     03  DSP-FMT                  PIC  X(08).
     03  DSP-GRP                  PIC  X(08).
     03  DSP-PRO                  PIC  X(02).
     03  DSP-FNC                  PIC  X(04).
*フラグ領域
 01  FLG-AREA.
     03  READ-FLG                 PIC  9(01)  VALUE  ZERO.
     03  ERR-FLG                  PIC  9(02)  VALUE  ZERO.
     03  END-FLG                  PIC  X(03)  VALUE  SPACE.
     03  SAKKBN-FLG               PIC  9(01)  VALUE  ZERO.
     03  FG-HTOKMS-INV            PIC  9(01)  VALUE  ZERO.
     03  FG-SAKUBAF-INV           PIC  9(01)  VALUE  ZERO.
     03  FG-HSHOTBL-INV           PIC  9(01)  VALUE  ZERO.
     03  FG-NFMEIMS-INV           PIC  9(01)  VALUE  ZERO.
     03  FG-NFJOHOF-INV           PIC  9(01)  VALUE  ZERO.
     03  FG-NFJOHOF2-INV          PIC  9(01)  VALUE  ZERO.
     03  FG-NFJOHOF3-INV          PIC  9(01)  VALUE  ZERO.
     03  FG-SHTDENF-INV           PIC  9(01)  VALUE  ZERO.
     03  FG-ZAMZAIF-INV           PIC  9(01)  VALUE  ZERO.
     03  FG-HTENMS-INV            PIC  9(01)  VALUE  ZERO.
     03  FG-NFJOHOF-END           PIC  X(03)  VALUE  SPACE.
     03  FG-NFJOHOF2-END          PIC  X(03)  VALUE  SPACE.
*ワーク領域
 01  WRK-AREA.
***  プログラムスイッチ（画面遷移制御）
     03  PSW                      PIC  X(01)  VALUE  SPACE.
***  モード退避
     03  SAV-SHORI                PIC  9(01)  VALUE  ZERO.
     03  IX                       PIC  9(05).
     03  IX-PG                    PIC  9(05).
     03  IX-PGAL                  PIC  9(05).
     03  SV-IX-PG                 PIC  9(05).
     03  WK-AMARI                 PIC  9(05).
     03  WK-RD-KANRNO             PIC  9(08).
     03  SV-HD-BCHNO1             PIC  9(08).
     03  SV-HD-BCHNO2             PIC  9(04).
     03  SV-HD-BCHNO3             PIC  9(08).
     03  SV-HD-KNRINO             PIC  9(08).
     03  SV-HD-TENYMD             PIC  9(08).
     03  SV-HD-SKBCD              PIC  X(02).
     03  WK-CASESU                PIC  9(05).
     03  WK-HAKOSU                PIC  9(05).
     03  WK-HIKI-KANO-ZAIKOSU     PIC S9(08)V99.
     03  FG-URI-ZKHKATE           PIC  9(01).
     03  WK-UPDMAE-SURYO          PIC S9(05)V9.
     03  WK-KH-IRISU              PIC  9(05)V9.

***  エラーセクション名
 01  SEC-NAME.
     03  FILLER                   PIC  X(05)     VALUE " *** ".
     03  S-NAME                   PIC  X(30).
*
*システム日付／時刻
 01  TIME-AREA.
     03  WK-TIME                  PIC  9(08)  VALUE  ZERO.
 01  DATE-AREA.
     03  WK-DATE                  PIC  9(06)  VALUE  ZERO.
     03  SYS-DATE                 PIC  9(08)  VALUE  ZERO.
*受信日付チェック用
* 01  JUSIN-DATE.
*    03  JUSIN-DATEY              PIC  9(04)  VALUE  ZERO.
*    03  JUSIN-DATEM              PIC  9(02)  VALUE  ZERO.
*    03  JUSIN-DATED              PIC  9(02)  VALUE  ZERO.
*
*受信時間チェック
 01  WK-JIKAN.
     03  WK-HH                    PIC   9(02)  VALUE  ZERO.
     03  WK-MM                    PIC   9(02)  VALUE  ZERO.
*
*日付論理チェック
 01  WK-CHKDATE.
     03  WK-CHKDATE-YYYY          PIC   9(04)  VALUE  ZERO.
     03  WK-CHKDATE-MM            PIC   9(02)  VALUE  ZERO.
     03  WK-CHKDATE-DD            PIC   9(02)  VALUE  ZERO.

* 基本情報ファイルデータ格納テーブル
 01  TBL-HAKO-G.
    03  TBL-HAKO-G1.
      05  TBL-KANRNO        PIC  9(08).  *> 管理番号

    03  TBL-HAKO-G2.
      05  TBL-HAKO-G3  OCCURS 600.
        07  TBL-KAKUNO-FG     PIC  X(01).   *> テーブル格納

        07  TBL-TENCD         PIC  9(05).   *> 店舗ＣＤ
        07  TBL-DENNO         PIC  9(09).   *> 伝票番号
        07  TBL-GYONO         PIC  9(02).   *> 行番号
        07  TBL-NOHYMD        PIC  9(08).   *> 納品日

        07  TBL-HSURYO        PIC  9(05)V9. *> 発注数量
        07  TBL-HHAKOSU       PIC  9(05)V9. *> 発注箱数
        07  TBL-MSSAKU        PIC  X(02).   *> 明細作場
        07  TBL-SURYO         PIC  9(05)V9. *> 訂正数量
        07  TBL-HAKOSU        PIC  9(05)V9. *> 訂正箱数
        07  TBL-TEIKBN        PIC  X(01).   *> 訂正区分
        07  TBL-SYYMD         PIC  9(08).   *> 出荷日
        07  TBL-TNYMD         PIC  9(08).   *> 店着日
    03  TBL-HAKO-G2R  REDEFINES TBL-HAKO-G2.
      05  TBL-HAKO-G3R  OCCURS 60.   *> ページ数
        07  TBL-HAKO-G4R  OCCURS 10. *> 行数
          09  TBL-KAKUNO-FGR  PIC  X(01).   *> テーブル格納

          09  TBL-TENCDR      PIC  9(05).   *> 店舗ＣＤ
          09  TBL-DENNOR      PIC  9(09).   *> 伝票番号
          09  TBL-GYONOR      PIC  9(02).   *> 行番号
          09  TBL-NOHYMDR     PIC  9(08).   *> 納品日

          09  TBL-HSURYOR     PIC  9(05)V9. *> 発注数量
          09  TBL-HHAKOSUR    PIC  9(05)V9. *> 発注箱数
          09  TBL-MSSAKUR     PIC  X(02).   *> 明細作場
          09  TBL-SURYOR      PIC  9(05)V9. *> 訂正数量
          09  TBL-HAKOSUR     PIC  9(05)V9. *> 訂正箱数
          09  TBL-TEIKBNR     PIC  X(01).   *> 訂正区分
          09  TBL-SYYMDR      PIC  9(08).   *> 出荷日
          09  TBL-TNYMDR      PIC  9(08).   *> 店着日


 01  TBL-HAKO-IXMAX         PIC  9(05).
 01  TBL-HAKO-PGMAX         PIC  9(03).
*
* 基本情報ファイルレコード退避領域
     COPY  NFJOHOF OF XFDLIB  JOINING KHW1  AS PREFIX.
     COPY  NFJOHOF OF XFDLIB  JOINING KHW2  AS PREFIX.

*ＰＦガイド
 01  PF-MSG-AREA.
     03  PF-MSG1.
       05  FILLER           PIC  N(30)  VALUE
       NC"_取消　_終了　　　　　　　　".
     03  PF-MSG2.
       05  FILLER           PIC  N(30)  VALUE
       NC"_取消　_終了　_項目戻し　　".
     03  PF-MSG3.
       05  FILLER           PIC  N(30)  VALUE
       NC"_取消　_終了　_項目戻し　_前頁　_次頁".
 01  PF-MSG-AREA-R  REDEFINES PF-MSG-AREA.
     03  PF-MSG-R  OCCURS 3 PIC  N(30).
*
*メッセージの取得
 01  ERR-MSG-AREA.
     03  ERR-MSG1.
       05  FILLER              PIC   N(25)  VALUE
       NC"正しい値を入力して下さい。".
     03  ERR-MSG2.
       05  FILLER              PIC   N(25)  VALUE
       NC"対象データが存在しません。".
     03  ERR-MSG3.
       05  FILLER              PIC   N(25)  VALUE
       NC"区分を入力して下さい。".
     03  ERR-MSG4.
       05  FILLER              PIC   N(25)  VALUE
       NC"バッチＮＯを入力して下さい。".
     03  ERR-MSG5.
       05  FILLER              PIC   N(25)  VALUE
       NC"作業ＮＯを入力して下さい。".
     03  ERR-MSG6.
       05  FILLER              PIC   N(25)  VALUE
       NC"無効キーです。".
     03  ERR-MSG7.
       05  FILLER              PIC   N(25)  VALUE
       NC"前頁はありません。".
     03  ERR-MSG8.
       05  FILLER              PIC   N(25)  VALUE
       NC"次頁はありません。".
     03  ERR-MSG9.
       05  FILLER              PIC   N(25)  VALUE
       NC"作場コードを入力して下さい。".
     03  ERR-MSG10.
       05  FILLER              PIC   N(25)  VALUE
       NC"商品コードを入力して下さい。".
     03  ERR-MSG11.
       05  FILLER              PIC   N(25)  VALUE
       NC"商品変換ＴＢＬに存在しません。".
     03  ERR-MSG12.
       05  FILLER              PIC   N(25)  VALUE
       NC"変更する作場、店着日で同一のレコードが存在します。".
     03  ERR-MSG13.
       05  FILLER              PIC   N(25)  VALUE
       NC"　".
     03  ERR-MSG14.
       05  FILLER              PIC   N(25)  VALUE
       NC"　".
 01  ERR-MSG-AREA-R      REDEFINES     ERR-MSG-AREA.
     03  ERR-MSG-R   OCCURS  14  PIC   N(25).
*
 01  FILE-ERR.
     03  DSP-ERR           PIC  N(20)  VALUE
         NC"画面ファイルエラー".
     03  KH1-ERR           PIC  N(20)  VALUE
         NC"基本情報ファイル１エラー".
     03  KH2-ERR           PIC  N(20)  VALUE
         NC"基本情報ファイル２エラー".
     03  KH3-ERR           PIC  N(20)  VALUE
         NC"基本情報ファイル３エラー".
     03  SKB-ERR           PIC  N(20)  VALUE
         NC"作場マスタエラー".
     03  TEN-ERR           PIC  N(20)  VALUE
         NC"店舗マスタエラー".
     03  SCV-ERR           PIC  N(20)  VALUE
         NC"商品変換ＴＢＬエラー".
     03  SYO-ERR           PIC  N(20)  VALUE
         NC"商品名称マスタエラー".
     03  TOK-ERR           PIC  N(20)  VALUE
         NC"取引先マスタエラー".
     03  ZAK-ERR           PIC  N(20)  VALUE
         NC"在庫マスタエラー".
     03  URI-ERR           PIC  N(20)  VALUE
         NC"売上伝票ファイルエラー".

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
*
**************************************************************
 PROCEDURE             DIVISION.
**************************************************************
 DECLARATIVES.
 DSP-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE DSPFILE.
     DISPLAY     DSP-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     DSP-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 KH1-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE NFJOHOF.
     DISPLAY     KH1-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     KH1-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 KH2-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE NFJOHOF2.
     DISPLAY     KH2-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     KH2-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 KH3-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE NFJOHOF3.
     DISPLAY     KH3-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     KH3-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 SKB-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE SAKUBAF.
     DISPLAY     SKB-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     SKB-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 TEN-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HTENMS.
     DISPLAY     TEN-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     TEN-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 SCV-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HSHOTBL.
     DISPLAY     SCV-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     SCV-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 SYO-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE NFMEIMS.
     DISPLAY     SYO-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     SYO-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 TOK-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HTOKMS.
     DISPLAY     TOK-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     TOK-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 ZAK-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE ZAMZAIF.
     DISPLAY     ZAK-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ZAK-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 URI-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE SHTDENF.
     DISPLAY     URI-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     URI-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 END  DECLARATIVES.
****************************************************************
*             MAIN        MODULE                     0.0       *
****************************************************************
 PROCESS-START         SECTION.
     MOVE     "PROCESS START"     TO   S-NAME.

     PERFORM   INIT-SEC.
     PERFORM   MAIN-SEC          UNTIL   END-FLG   =   "END".
     PERFORM   END-SEC.

     STOP    RUN.
 CONTROL-EXIT.
     EXIT.
****************************************************************
*             初期処理                               1.0
****************************************************************
 INIT-SEC              SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
     PERFORM  SDATE-GET-SEC.
*ファイルのＯＰＥＮ
     OPEN  I-O   DSPFILE.
     OPEN  INPUT NFJOHOF.
     OPEN  INPUT NFJOHOF2.
     OPEN  I-O   NFJOHOF3.
     OPEN  INPUT SAKUBAF.
     OPEN  INPUT HTENMS.
     OPEN  INPUT HSHOTBL.
     OPEN  INPUT NFMEIMS.
     OPEN  INPUT HTOKMS.
     OPEN  I-O   ZAMZAIF.
     OPEN  I-O   SHTDENF.
*ワークの初期化
     INITIALIZE         FLG-AREA.
*初期画面の表示
     MOVE     SPACE               TO   DSP-PRO.
     PERFORM  INIT-DSP-SEC.
*ヘッド入力へ
     MOVE    "1"                  TO   PSW.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*             システム日付取得
****************************************************************
 SDATE-GET-SEC              SECTION.
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
     ACCEPT    WK-TIME          FROM   TIME.
 SDATE-GET-EXIT.
     EXIT.
****************************************************************
*             メイン処理                             2.0
****************************************************************
 MAIN-SEC              SECTION.
     MOVE     "MAIN-SEC"     TO   S-NAME.
*
     EVALUATE      PSW
*処理区分入力
         WHEN      "1"  PERFORM   DSP-HEAD1-SEC
*キー項目入力
         WHEN      "2"  PERFORM   DSP-HEAD21-SEC
*キー項目入力２
         WHEN      "3"  PERFORM   DSP-HEAD22-SEC
*ヘッド部入力
         WHEN      "6"  PERFORM   DSP-HEAD3-SEC
*明細項目入力
         WHEN      "4"  PERFORM   DSP-BODY-SEC
*確認入力
         WHEN      "5"  PERFORM   DSP-KAKU-SEC
*
         WHEN      OTHER  CONTINUE
     END-EVALUATE.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*             処理区分入力( PSW = 1 )                2.1       *
****************************************************************
 DSP-HEAD1-SEC         SECTION.
     MOVE     "DSP-HEAD1-SEC"     TO   S-NAME.
*
     PERFORM    DSP-WRITE-SEC.
     PERFORM    DSP-READ-SEC.
*
     EVALUATE   DSP-FNC
*実行
         WHEN   "E000"
                PERFORM   HEAD1-CHK-SEC
*終了
         WHEN   "F005"
                MOVE    "END"    TO   END-FLG
*取消
         WHEN   "F004"
                MOVE    "1"      TO   PSW
                PERFORM   INIT-DSP-SEC
         WHEN   OTHER
                MOVE     6       TO   ERR-FLG
                GO       TO      DSP-HEAD1-SEC
     END-EVALUATE.
*
 DSP-HEAD1-EXIT.
     EXIT.
****************************************************************
*             画面表示処理                                     *
****************************************************************
 DSP-WRITE-SEC         SECTION.
     MOVE     "DSP-WRITE-SEC"     TO   S-NAME.

     PERFORM  SDATE-GET-SEC.
*システム日付転送
     MOVE  SYS-DATE              TO  DSP-SDATE.
*システム時間転送
     MOVE  WK-TIME(1:6)          TO  DSP-STIME.
*エラーメッセージセット
     IF    ERR-FLG   =    ZERO
           MOVE    SPACE              TO   DSP-ERRMSG
     ELSE
           MOVE    ERR-MSG-R(ERR-FLG) TO   DSP-ERRMSG
           MOVE    ZERO               TO   ERR-FLG
     END-IF.
*ガイドメッセージの設定
     EVALUATE  PSW
*処理区分
       WHEN  "1"
         MOVE  PF-MSG-R(1)       TO  DSP-PFGAID
*キー項目
       WHEN  "2"
         PERFORM  DSP-ZERO-SPACE-SEC
         MOVE  DSP-BCHNO1        TO  SV-HD-BCHNO1
         MOVE  DSP-BCHNO2        TO  SV-HD-BCHNO2
         MOVE  DSP-BCHNO3        TO  SV-HD-BCHNO3
         MOVE  DSP-JOBNO         TO  SV-HD-KNRINO
         MOVE  DSP-TENYMD        TO  SV-HD-TENYMD
         MOVE  DSP-SKBACD        TO  SV-HD-SKBCD
         MOVE  PF-MSG-R(2)       TO  DSP-PFGAID
*キー項目２
       WHEN  "3"
         PERFORM  DSP-ZERO-SPACE-SEC
         MOVE  DSP-BCHNO1        TO  SV-HD-BCHNO1
         MOVE  DSP-BCHNO2        TO  SV-HD-BCHNO2
         MOVE  DSP-BCHNO3        TO  SV-HD-BCHNO3
         MOVE  DSP-JOBNO         TO  SV-HD-KNRINO
         MOVE  DSP-TENYMD        TO  SV-HD-TENYMD
         MOVE  DSP-SKBACD        TO  SV-HD-SKBCD
         MOVE  PF-MSG-R(2)       TO  DSP-PFGAID
*ヘッド部入力
       WHEN  "6"
         MOVE  DSP-BCHNO1        TO  SV-HD-BCHNO1
         MOVE  DSP-BCHNO2        TO  SV-HD-BCHNO2
         MOVE  DSP-BCHNO3        TO  SV-HD-BCHNO3
         PERFORM  DSP-ZERO-SPACE-SEC
         PERFORM  DSP-ZERO-SPACE2-SEC
         MOVE  PF-MSG-R(2)       TO  DSP-PFGAID
*明細項目
       WHEN  "4"
         MOVE  DSP-BCHNO1        TO  SV-HD-BCHNO1
         MOVE  DSP-BCHNO2        TO  SV-HD-BCHNO2
         MOVE  DSP-BCHNO3        TO  SV-HD-BCHNO3
         PERFORM  DSP-ZERO-SPACE2-SEC
         PERFORM  DSP-ZERO-SPACE3-SEC
         MOVE  PF-MSG-R(3)       TO  DSP-PFGAID
*確認
       WHEN  "5"
         MOVE  DSP-BCHNO1        TO  SV-HD-BCHNO1
         MOVE  DSP-BCHNO2        TO  SV-HD-BCHNO2
         MOVE  DSP-BCHNO3        TO  SV-HD-BCHNO3
         PERFORM  DSP-ZERO-SPACE3-SEC
         MOVE  PF-MSG-R(2)       TO  DSP-PFGAID
       WHEN  OTHER
         MOVE  SPACE             TO  DSP-PFGAID
     END-EVALUATE.
*画面の表示
     MOVE    "SCREEN"            TO  DSP-GRP.
     MOVE    "FSY37571"          TO  DSP-FMT.
     WRITE    DSP-FSY37571.
*
 DSP-WRITE-EXIT.
     EXIT.
****************************************************************
*             ゼロを空白表示する                               *
****************************************************************
 DSP-ZERO-SPACE-SEC          SECTION.
     IF  DSP-KBN = 2
         MOVE  99999999          TO  DSP-BCHNO1
         MOVE  9999              TO  DSP-BCHNO2
*********MOVE  99999999          TO  DSP-BCHNO3
     ELSE
         IF  DSP-BCHNO1 = ZERO
             MOVE  SPACE         TO  DSP-BCHNO1(1:8)
         END-IF
         IF  DSP-BCHNO2 = ZERO
             MOVE  SPACE         TO  DSP-BCHNO2(1:4)
         END-IF
         IF  DSP-BCHNO3 = ZERO
             MOVE  SPACE         TO  DSP-BCHNO3(1:8)
         END-IF
     END-IF.

     IF  DSP-JOBNO = ZERO
         MOVE  SPACE             TO  DSP-JOBNO(1:8)
     END-IF.

 DSP-ZERO-SPACE-EXIT.
     EXIT.
****************************************************************
*             ゼロを空白表示する２                             *
****************************************************************
 DSP-ZERO-SPACE2-SEC          SECTION.
     IF  DSP-SYUYMD = ZERO
         MOVE  SPACE        TO  DSP-SYUYMD(1:8)
     END-IF.
     IF  DSP-TENYMD = ZERO
         MOVE  SPACE        TO  DSP-TENYMD(1:8)
     END-IF.
     IF  DSP-HCACE = ZERO
         MOVE  SPACE        TO  DSP-HCACE(1:6)
     END-IF.
 DSP-ZERO-SPACE2-EXIT.
     EXIT.
****************************************************************
*             ゼロを空白表示する３                             *
****************************************************************
 DSP-ZERO-SPACE3-SEC          SECTION.
*****PERFORM  VARYING IX  FROM 1 BY 1
*             UNTIL   IX > 10
*      IF  DSP-SURYO (IX) = ZERO
*          MOVE  SPACE      TO  DSP-SURYO  (IX)(1:6)
*      END-IF
*      IF  DSP-HAKOSU (IX) = ZERO
*          MOVE  SPACE      TO  DSP-HAKOSU (IX)(1:6)
*      END-IF
*      IF  DSP-SYYMD (IX) = ZERO
*          MOVE  SPACE      TO  DSP-SYYMD  (IX)(1:8)
*      END-IF
*      IF  DSP-TNYMD (IX) = ZERO
*          MOVE  SPACE      TO  DSP-TNYMD  (IX)(1:8)
*      END-IF
*****END-PERFORM.
 DSP-ZERO-SPACE3-EXIT.
     EXIT.
****************************************************************
*             画面読込処理                                     *
****************************************************************
 DSP-READ-SEC          SECTION.
     MOVE     "DSP-READ-SEC"      TO   S-NAME.
*
     MOVE    "NE"                 TO   DSP-PRO.
*
*    MOVE    "SCREEN"             TO   DSP-GRP.

     EVALUATE   PSW
*処理区分
         WHEN   "1"
                MOVE    "SMODE"   TO   DSP-GRP
*キー項目
         WHEN   "2"
                MOVE    "KEYGRP"  TO   DSP-GRP
*キー項目２
         WHEN   "3"
                MOVE    "KEYGR2"  TO   DSP-GRP
*ヘッド部入力
         WHEN   "6"
                MOVE    "HEAD"    TO   DSP-GRP
*明細項目
         WHEN   "4"
                MOVE    "MAIN"    TO   DSP-GRP
*確認
         WHEN   "5"
                MOVE    "KAKU"    TO   DSP-GRP
     END-EVALUATE.

     MOVE    "FSY37571"           TO   DSP-FMT.
     READ    DSPFILE.
*入力項目の属性を通常にする
 DSP-READ-010.
*    MOVE    ZERO                 TO   ERR-FLG.
     MOVE    SPACE                TO   DSP-PRO.
*
 DSP-READ-EXIT.
     EXIT.
****************************************************************
*             処理区分チェック                                 *
****************************************************************
 HEAD1-CHK-SEC             SECTION.
*処理区分 未入力はエラー
     IF     DSP-KBN NOT NUMERIC
         OR DSP-KBN = ZERO
         MOVE  3                 TO  ERR-FLG
         MOVE  SPACE             TO  DSP-KBN (1:1)
         GO TO  HEAD1-CHK-EXIT
     END-IF.

     IF  DSP-KBN = 1 OR 2
         IF  DSP-KBN = 1   *> オンライン
             MOVE  "2"           TO  PSW
         ELSE              *> 手書き
             MOVE  "3"           TO  PSW
         END-IF

*        同一モードでループさせるため
         MOVE  DSP-KBN           TO  SAV-SHORI
     ELSE
         MOVE  1                 TO  ERR-FLG
     END-IF.
*
 HEAD1-CHK-EXIT.
     EXIT.
****************************************************************
*             キー項目入力( PSW = 2 )                          *
****************************************************************
 DSP-HEAD21-SEC         SECTION.
     MOVE     "DSP-HEAD21-SEC"    TO   S-NAME.
*
     PERFORM    DSP-WRITE-SEC.
     PERFORM    DSP-READ-SEC.
*
     EVALUATE   DSP-FNC
*実行
         WHEN   "E000"
                PERFORM  HEAD2-CHK-SEC
*終了
         WHEN   "F005"
                MOVE    "END"    TO   END-FLG
*項目戻し
         WHEN   "F006"
                MOVE    "1"      TO   PSW
*取消
         WHEN   "F004"
                MOVE    "1"      TO   PSW
                PERFORM   INIT-DSP-SEC
         WHEN   OTHER
                MOVE     6       TO   ERR-FLG
                GO       TO      DSP-HEAD21-SEC
     END-EVALUATE.
*
 DSP-HEAD21-EXIT.
     EXIT.
****************************************************************
*    キー項目チェック                                          *
****************************************************************
 HEAD2-CHK-SEC         SECTION.
     INITIALIZE  TBL-HAKO-G.

     PERFORM  HEAD2B-CHK-SEC.
     IF  ERR-FLG NOT = ZERO
         GO TO  HEAD2-CHK-EXIT
     END-IF.

     MOVE  "6"              TO  PSW.
 HEAD2-CHK-EXIT.
     EXIT.
****************************************************************
*    キー項目チェックＢ                                        *
****************************************************************
 HEAD2B-CHK-SEC        SECTION.
     MOVE  "HEAD2-CHK-SEC"       TO  S-NAME.
     MOVE  "M"              TO  EDIT-OPTION OF DSP-BCHNO1.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-BCHNO1.
     MOVE  "M"              TO  EDIT-OPTION OF DSP-BCHNO2.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-BCHNO2.
     MOVE  "M"              TO  EDIT-OPTION OF DSP-BCHNO3.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-BCHNO3.
     MOVE  "M"              TO  EDIT-OPTION OF DSP-JOBNO.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-JOBNO.
     MOVE  "M"              TO  EDIT-OPTION OF DSP-SHOCD.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-SHOCD.
     MOVE  "M"              TO  EDIT-OPTION OF DSP-SKBACD.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-SKBACD.
* バッチＮＯ（日付）
     MOVE  SPACE            TO  DSP-TORINM.
     IF  DSP-KBN = "1"  *> オンライン
         IF     (DSP-BCHNO1  NOT NUMERIC  OR DSP-BCHNO1 = ZERO)
            AND (DSP-BCHNO2  NOT NUMERIC  OR DSP-BCHNO2 = ZERO)
            AND (DSP-BCHNO3  NOT NUMERIC  OR DSP-BCHNO3 = ZERO)
            AND (DSP-JOBNO  NOT NUMERIC  OR DSP-JOBNO = ZERO)
            MOVE  4              TO  ERR-FLG
            MOVE  "C"            TO  EDIT-CURSOR OF DSP-BCHNO1
            MOVE  "R"            TO  EDIT-OPTION OF DSP-BCHNO1
            MOVE  "R"            TO  EDIT-OPTION OF DSP-BCHNO2
            MOVE  "R"            TO  EDIT-OPTION OF DSP-BCHNO3
            MOVE  "R"            TO  EDIT-OPTION OF DSP-JOBNO
            GO TO  HEAD2B-CHK-EXIT
         END-IF

         IF     DSP-BCHNO1 NOT NUMERIC
             OR DSP-BCHNO1 = ZERO
             CONTINUE
         ELSE
             MOVE  "2"           TO  LINK-IN-KBN
             MOVE  DSP-BCHNO1       TO  LINK-IN-YMD8
             CALL  "SKYDTCKB" USING LINK-IN-KBN
                                    LINK-IN-YMD6
                                    LINK-IN-YMD8
                                    LINK-OUT-RET
                                    LINK-OUT-YMD
             IF  LINK-OUT-RET NOT = ZERO
                 IF  ERR-FLG = ZERO
                     MOVE  1     TO  ERR-FLG
                 END-IF
                 MOVE  "C"       TO  EDIT-CURSOR OF DSP-BCHNO1
                 MOVE  "R"       TO  EDIT-OPTION OF DSP-BCHNO1
                 GO TO    HEAD2B-CHK-EXIT
             END-IF
         END-IF
* バッチＮＯ（時間）

* バッチＮＯ（取引先）
         IF      DSP-BCHNO1  IS NUMERIC
             AND DSP-BCHNO1  NOT = ZERO
             IF     DSP-BCHNO3  NOT NUMERIC
                 OR DSP-BCHNO3 = ZERO
                 IF  ERR-FLG = ZERO
                     MOVE  1     TO  ERR-FLG
                 END-IF
                 MOVE  "C"       TO  EDIT-CURSOR OF DSP-BCHNO3
                 MOVE  "R"       TO  EDIT-OPTION OF DSP-BCHNO3
                 GO TO  HEAD2B-CHK-EXIT
             ELSE
                 MOVE DSP-BCHNO3 TO  TOK-F01 *> 相手取引先ＣＤ
                 PERFORM  RD-HTOKMS-SEC
                 IF  FG-HTOKMS-INV = ZERO
                     MOVE  TOK-F03  TO  DSP-TORINM
                 ELSE
                     MOVE  SPACE    TO  DSP-TORINM
                     IF  ERR-FLG = ZERO
                         MOVE  1    TO  ERR-FLG
                     END-IF
                     MOVE  "C"      TO  EDIT-CURSOR OF DSP-BCHNO3
                     MOVE  "R"      TO  EDIT-OPTION OF DSP-BCHNO3
                     GO TO  HEAD2B-CHK-EXIT
                 END-IF
             END-IF
         END-IF

* 管理番号

     ELSE               *> 手書き
* 管理番号
         IF      DSP-JOBNO  NOT NUMERIC
             OR  DSP-JOBNO = ZERO
             IF  ERR-FLG = ZERO
                 MOVE  5         TO  ERR-FLG
             END-IF
             MOVE  SPACE         TO  DSP-JOBNO(1:8)
             MOVE  "C"           TO  EDIT-CURSOR OF DSP-JOBNO
             MOVE  "R"           TO  EDIT-OPTION OF DSP-JOBNO
             GO TO  HEAD2B-CHK-EXIT
         END-IF
     END-IF.

* 商品コード
     MOVE  SPACE            TO  DSP-SHONM.
     IF  DSP-SHOCD = SPACE
         IF  ERR-FLG = ZERO
             MOVE  10       TO  ERR-FLG
         END-IF
         MOVE  "C"          TO  EDIT-CURSOR OF DSP-SHOCD
         MOVE  "R"          TO  EDIT-OPTION OF DSP-SHOCD
         GO TO  HEAD2B-CHK-EXIT
     END-IF.

*    商品変換ＴＢＬ
     IF      DSP-KBN = 1
         AND (    DSP-BCHNO3 NUMERIC
              AND DSP-BCHNO3 NOT = ZERO)
         MOVE  DSP-BCHNO3   TO  SCV-F01 *> 相手取引先ＣＤ
     ELSE
*********MOVE  137607       TO  SCV-F01 *> 相手取引先ＣＤ
         MOVE  999977       TO  SCV-F01 *> 相手取引先ＣＤ
     END-IF.

     MOVE  DSP-SHOCD        TO  SCV-F02 *> 相手商品コード
     PERFORM  RD-HSHOTBL-SEC.
     IF  FG-HSHOTBL-INV = 1
         IF  ERR-FLG = ZERO
             MOVE  11       TO  ERR-FLG
         END-IF
         MOVE  "C"          TO  EDIT-CURSOR OF DSP-SHOCD
         MOVE  "R"          TO  EDIT-OPTION OF DSP-SHOCD
         GO TO  HEAD2B-CHK-EXIT
     END-IF.

*    ナフコ商品名称マスタ
     MOVE  DSP-SHOCD        TO  SYO-F01. *> ナフコ商品ＣＤ
     PERFORM  RD-NFMEIMS-SEC.
     IF  FG-NFMEIMS-INV = 1
         MOVE  SPACE        TO  DSP-SHONM
     ELSE
         MOVE  SYO-F05      TO  DSP-SHONM
     END-IF.

* 作場ＣＤ
     IF  DSP-SKBACD = SPACE
         IF  ERR-FLG = ZERO
             MOVE  9        TO  ERR-FLG
         END-IF
         MOVE  "C"          TO  EDIT-CURSOR OF DSP-SKBACD
         MOVE  "R"          TO  EDIT-OPTION OF DSP-SKBACD
         GO TO  HEAD2B-CHK-EXIT
     END-IF.

     MOVE  DSP-SKBACD       TO  SKB-F01.
     PERFORM  RD-SAKUBAF-SEC.
     IF  FG-SAKUBAF-INV = 1
         MOVE  SPACE        TO  DSP-SKBNM
         IF  ERR-FLG = ZERO
             MOVE  1        TO  ERR-FLG
         END-IF
         MOVE  "C"          TO  EDIT-CURSOR OF DSP-SKBACD
         MOVE  "R"          TO  EDIT-OPTION OF DSP-SKBACD
         GO TO  HEAD2B-CHK-EXIT
     ELSE
         MOVE  SKB-F02      TO  DSP-SKBNM
     END-IF.

* 入数（表示）
     PERFORM  NFHAKO-CHK-SEC.
     IF  ERR-FLG = ZERO
         MOVE WK-KH-IRISU   TO  DSP-IRISU
     ELSE
         MOVE SPACE         TO  DSP-IRISU (1:6)
         GO TO  HEAD2B-CHK-EXIT
     END-IF.

 HEAD2B-CHK-EXIT.
     EXIT.

****************************************************************
*    取引先マスタ検索                                          *
****************************************************************
 RD-HTOKMS-SEC          SECTION.
     READ  HTOKMS
       INVALID
         MOVE  1                 TO  FG-HTOKMS-INV
       NOT INVALID
         MOVE  ZERO              TO  FG-HTOKMS-INV
     END-READ.

 RD-HTOKMS-EXIT.
     EXIT.

****************************************************************
*    商品変換ＴＢＬ検索                                        *
****************************************************************
 RD-HSHOTBL-SEC         SECTION.
     READ  HSHOTBL
       INVALID
         MOVE  1                 TO  FG-HSHOTBL-INV
       NOT INVALID
         MOVE  ZERO              TO  FG-HSHOTBL-INV
     END-READ.

 RD-HSHOTBL-EXIT.
     EXIT.

****************************************************************
*    作場マスタ検索                                            *
****************************************************************
 RD-SAKUBAF-SEC          SECTION.
     READ  SAKUBAF
       INVALID
         MOVE  1                 TO  FG-SAKUBAF-INV
       NOT INVALID
         MOVE  ZERO              TO  FG-SAKUBAF-INV
     END-READ.

 RD-SAKUBAF-EXIT.
     EXIT.

****************************************************************
*    ナフコ商品名称マスタ検索                                  *
****************************************************************
 RD-NFMEIMS-SEC          SECTION.
     READ  NFMEIMS
       INVALID
         MOVE  1                 TO  FG-NFMEIMS-INV
       NOT INVALID
         MOVE  ZERO              TO  FG-NFMEIMS-INV
     END-READ.

 RD-NFMEIMS-EXIT.
     EXIT.

****************************************************************
*    基本情報ファイル存在チェック                              *
****************************************************************
 NFHAKO-CHK-SEC         SECTION.
     IF  DSP-KBN = "1"  *> オンライン
         IF  DSP-BCHNO1 IS NUMERIC AND DSP-BCHNO1 NOT = ZERO
               *> バッチNO指定
             MOVE  LOW-VALUE     TO FG-NFJOHOF2-END
             MOVE  DSP-BCHNO1    TO  KH2-F02  *> バッチ日付
             MOVE  DSP-BCHNO2    TO  KH2-F03  *> バッチ時間
             MOVE  DSP-BCHNO3    TO  KH2-F04  *> バッチ取引先
             MOVE  DSP-SKBACD    TO  KH2-F05  *> 作場ＣＤ
             MOVE  DSP-SHOCD     TO  KH2-F13  *> 相手商品ＣＤ
             MOVE  ZERO          TO  KH2-F06  *> 店舗ＣＤ
             PERFORM  RD-NFJOHOF2-SEC
             IF  FG-NFJOHOF2-END = "END"
                 IF  ERR-FLG = ZERO
                     MOVE  2     TO  ERR-FLG
                 END-IF
                 MOVE  "C"       TO  EDIT-CURSOR OF DSP-BCHNO1
                 MOVE  "R"       TO  EDIT-OPTION OF DSP-BCHNO1
                 MOVE  "R"       TO  EDIT-OPTION OF DSP-BCHNO2
                 MOVE  "R"       TO  EDIT-OPTION OF DSP-BCHNO3
                 MOVE  "R"       TO  EDIT-OPTION OF DSP-JOBNO
                 GO TO  NFHAKO-CHK-EXIT
             END-IF

             MOVE  KH2-F01       TO  TBL-KANRNO
             MOVE  KH2-F18       TO  WK-KH-IRISU
             IF    DSP-JOBNO NOT NUMERIC
                OR DSP-JOBNO = ZERO
                MOVE  KH2-F01    TO  DSP-JOBNO

             END-IF

         ELSE  *> 管理番号指定
             MOVE  LOW-VALUE     TO  FG-NFJOHOF-END
             MOVE  DSP-JOBNO     TO  WK-RD-KANRNO
             MOVE  DSP-JOBNO     TO  KH1-F01  *> 管理番号
             MOVE  DSP-SKBACD    TO  KH1-F05  *> 作場ＣＤ
             MOVE  DSP-SHOCD     TO  KH1-F13  *> 相手商品ＣＤ
             MOVE  ZERO          TO  KH1-F06  *> 店舗ＣＤ

             PERFORM  RD-NFJOHOF-SEC
             IF  FG-NFJOHOF-END = "END"
                 IF  ERR-FLG = ZERO
                     MOVE  2     TO  ERR-FLG
                 END-IF
                 MOVE  "R"       TO  EDIT-OPTION OF DSP-JOBNO
                 GO TO  NFHAKO-CHK-EXIT
             ELSE
                 IF KH1-F02 NOT = 99999999 *> バッチ日付
                    CONTINUE
                 ELSE
                     MOVE  2     TO  ERR-FLG
                     MOVE  "R"   TO  EDIT-OPTION OF DSP-JOBNO
                     GO TO  NFHAKO-CHK-EXIT
                 END-IF
             END-IF

             MOVE  KH1-F01        TO  TBL-KANRNO
             MOVE  KH1-F18        TO  WK-KH-IRISU
             MOVE  KH1-F02        TO  DSP-BCHNO1
             MOVE  KH1-F03        TO  DSP-BCHNO2
             MOVE  KH1-F04        TO  DSP-BCHNO3
             MOVE  DSP-BCHNO3     TO  TOK-F01 *> 相手取引先ＣＤ
             PERFORM  RD-HTOKMS-SEC
             IF  FG-HTOKMS-INV = ZERO
                 MOVE  TOK-F03    TO  DSP-TORINM
             ELSE
                 MOVE  SPACE      TO  DSP-TORINM
             END-IF
         END-IF
     ELSE               *> 手書き
         MOVE  LOW-VALUE    TO  FG-NFJOHOF-END
         MOVE  DSP-JOBNO    TO  WK-RD-KANRNO
         MOVE  DSP-JOBNO    TO  KH1-F01  *> 管理番号
         MOVE  DSP-SKBACD   TO  KH1-F05  *> 作場ＣＤ
         MOVE  DSP-SHOCD    TO  KH1-F13  *> 相手商品ＣＤ
         MOVE  ZERO         TO  KH1-F06  *> 店舗ＣＤ

         PERFORM  RD-NFJOHOF-SEC
         IF  FG-NFJOHOF-END = "END"
             IF  ERR-FLG = ZERO
                 MOVE  2    TO  ERR-FLG
             END-IF
             MOVE  "R"      TO  EDIT-OPTION OF DSP-JOBNO
             GO TO  NFHAKO-CHK-EXIT
         ELSE
             IF KH1-F02 = 99999999 *> バッチ日付
                CONTINUE
             ELSE
                 MOVE  2     TO  ERR-FLG
                 MOVE  "R"   TO  EDIT-OPTION OF DSP-JOBNO
                 GO TO  NFHAKO-CHK-EXIT
             END-IF
         END-IF

         MOVE  KH1-F01       TO  TBL-KANRNO
         MOVE  KH1-F18       TO  WK-KH-IRISU

     END-IF.

 NFHAKO-CHK-EXIT.
     EXIT.
****************************************************************
*    基本情報ファイル２読込み　                                *
****************************************************************
 RD-NFJOHOF2-SEC         SECTION.

     IF  FG-NFJOHOF2-END = LOW-VALUE
         START  NFJOHOF2  KEY >= KH2-F02
                                 KH2-F03
                                 KH2-F04
                                 KH2-F05
                                 KH2-F13
                                 KH2-F06
           INVALID KEY
              MOVE  "END"   TO  FG-NFJOHOF2-END
              GO TO  RD-NFJOHOF2-EXIT
         END-START

         MOVE  SPACE        TO  FG-NFJOHOF2-END

     END-IF.

 RD-NFJOHOF2-010.
     READ  NFJOHOF2  NEXT
       AT  END
         MOVE  "END"        TO  FG-NFJOHOF2-END
         GO TO  RD-NFJOHOF2-EXIT
     END-READ.

     IF  KH2-F26 = "1"
         GO TO  RD-NFJOHOF2-010
     END-IF.

     IF  KH2-F02 > DSP-BCHNO1
         MOVE  "END"        TO  FG-NFJOHOF2-END
         GO TO  RD-NFJOHOF2-EXIT
     END-IF.

     IF  KH2-F03 > DSP-BCHNO2
         MOVE  "END"        TO  FG-NFJOHOF2-END
         GO TO  RD-NFJOHOF2-EXIT
     END-IF.

     IF  KH2-F04 > DSP-BCHNO3
         MOVE  "END"        TO  FG-NFJOHOF2-END
         GO TO  RD-NFJOHOF2-EXIT
     END-IF.

     IF  KH2-F05 > DSP-SKBACD
         MOVE  "END"        TO  FG-NFJOHOF2-END
         GO TO  RD-NFJOHOF2-EXIT
     END-IF.

     IF  KH2-F13 > DSP-SHOCD
         MOVE  "END"        TO  FG-NFJOHOF2-END
         GO TO  RD-NFJOHOF2-EXIT
     END-IF.

     IF     DSP-JOBNO  NOT NUMERIC
         OR DSP-JOBNO = ZERO
         CONTINUE
     ELSE
         IF  KH2-F01 NOT = DSP-JOBNO
             GO TO  RD-NFJOHOF2-010
         END-IF
     END-IF.

 RD-NFJOHOF2-EXIT.
     EXIT.

****************************************************************
*    基本情報ファイル読込み　                                  *
****************************************************************
 RD-NFJOHOF-SEC          SECTION.

     IF  FG-NFJOHOF-END = LOW-VALUE
         START  NFJOHOF  KEY >= KH1-F01
                                KH1-F05
                                KH1-F13
                                KH1-F06
           INVALID KEY
              MOVE  "END"   TO  FG-NFJOHOF-END
              GO TO  RD-NFJOHOF-EXIT
         END-START

         MOVE  SPACE        TO  FG-NFJOHOF-END

     END-IF.

 RD-NFJOHOF-010.
     READ  NFJOHOF  NEXT
       AT  END
         MOVE  "END"        TO  FG-NFJOHOF-END
         GO TO  RD-NFJOHOF-EXIT
     END-READ.

     IF  KH1-F26 = "1"
         GO TO  RD-NFJOHOF-010
     END-IF.

     IF  KH1-F01 > WK-RD-KANRNO
         MOVE  "END"        TO  FG-NFJOHOF-END
         GO TO  RD-NFJOHOF-EXIT
     END-IF.

     IF  KH1-F05 > DSP-SKBACD
         MOVE  "END"        TO  FG-NFJOHOF-END
         GO TO  RD-NFJOHOF-EXIT
     END-IF.

     IF  KH1-F13 > DSP-SHOCD
         MOVE  "END"        TO  FG-NFJOHOF-END
         GO TO  RD-NFJOHOF-EXIT
     END-IF.

 RD-NFJOHOF-EXIT.
     EXIT.

****************************************************************
*             キー項目入力２( PSW = 3 )                        *
****************************************************************
 DSP-HEAD22-SEC         SECTION.
     MOVE     "DSP-HEAD21-SEC"    TO   S-NAME.
*
     PERFORM    DSP-WRITE-SEC.
     PERFORM    DSP-READ-SEC.
*
     EVALUATE   DSP-FNC
*実行
         WHEN   "E000"
                PERFORM   HEAD2-CHK-SEC

*終了
         WHEN   "F005"
                MOVE    "END"    TO   END-FLG
*項目戻し
         WHEN   "F006"
                MOVE    "1"      TO   PSW
*取消
         WHEN   "F004"
                MOVE    "1"      TO   PSW
                PERFORM   INIT-DSP-SEC
         WHEN   OTHER
                MOVE     6       TO   ERR-FLG
                GO       TO      DSP-HEAD22-SEC
     END-EVALUATE.
*
 DSP-HEAD22-EXIT.
     EXIT.
***************************************************************
*             ヘッド部入力( PSW = 6 )                         *
****************************************************************
 DSP-HEAD3-SEC         SECTION.
     MOVE     "DSP-HEAD3-SEC"    TO   S-NAME.
*
     PERFORM    DSP-WRITE-SEC.
     PERFORM    DSP-READ-SEC.
*
     EVALUATE   DSP-FNC
*実行
         WHEN   "E000"
                PERFORM  HEAD3-CHK-SEC
*終了
         WHEN   "F005"
                MOVE    "END"    TO   END-FLG
*項目戻し
         WHEN   "F006"
                IF  DSP-KBN = 1
                    MOVE  "2"    TO  PSW
                ELSE
                    MOVE  "3"    TO  PSW
                END-IF
*取消
         WHEN   "F004"
                MOVE    "1"      TO   PSW
                PERFORM   INIT-DSP-SEC
         WHEN   OTHER
                MOVE     6       TO   ERR-FLG
                GO       TO      DSP-HEAD3-SEC
     END-EVALUATE.
*
 DSP-HEAD3-EXIT.
     EXIT.
****************************************************************
*    キー項目チェック                                          *
****************************************************************
 HEAD3-CHK-SEC         SECTION.
     INITIALIZE  TBL-HAKO-G.

     PERFORM  HEAD3B-CHK-SEC.
     IF  ERR-FLG NOT = ZERO
         GO TO  HEAD3-CHK-EXIT
     END-IF.

     PERFORM  NFHAKO-CHK-SEC.
     IF  ERR-FLG NOT = ZERO
         IF  DSP-KBN = 1  *> オンライン
             MOVE  "2"      TO  PSW
         ELSE             *> 手書き
             MOVE  "3"      TO  PSW
         END-IF
         GO TO  HEAD3-CHK-EXIT
     END-IF.

     PERFORM  MS-EDT-SEC.
     MOVE  "4"              TO  PSW.

 HEAD3-CHK-EXIT.
     EXIT.
****************************************************************
*    キー項目チェックＢ                                        *
****************************************************************
 HEAD3B-CHK-SEC        SECTION.
     MOVE  "HEAD3-CHK-SEC"       TO  S-NAME.
     MOVE  "M"              TO  EDIT-OPTION OF DSP-CHGSAK.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-CHGSAK.
     MOVE  "M"              TO  EDIT-OPTION OF DSP-SYUYMD.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-SYUYMD.
     MOVE  "M"              TO  EDIT-OPTION OF DSP-TENYMD.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-TENYMD.
     MOVE  "M"              TO  EDIT-OPTION OF DSP-HCACE.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-HCACE.
* 変更作場
     MOVE  SPACE            TO  DSP-CHGSKN.
     IF  DSP-CHGSAK NOT = SPACE
         MOVE  DSP-CHGSAK    TO  SKB-F01
         PERFORM  RD-SAKUBAF-SEC
         IF  FG-SAKUBAF-INV = 1
             IF  ERR-FLG = ZERO
                 MOVE  1    TO  ERR-FLG
             END-IF
             MOVE  "C"      TO  EDIT-CURSOR OF DSP-CHGSAK
             MOVE  "R"      TO  EDIT-OPTION OF DSP-CHGSAK
             GO TO  HEAD3B-CHK-EXIT
         ELSE
             MOVE  SKB-F02  TO  DSP-CHGSKN
         END-IF

         IF  DSP-CHGSAK = DSP-SKBACD
             IF  ERR-FLG = ZERO
                 MOVE  1    TO  ERR-FLG
             END-IF
             MOVE  "C"      TO  EDIT-CURSOR OF DSP-CHGSAK
             MOVE  "R"      TO  EDIT-OPTION OF DSP-CHGSAK
             GO TO  HEAD3B-CHK-EXIT
         END-IF
     END-IF.

* 出荷日
     IF      DSP-SYUYMD IS NUMERIC
         AND DSP-SYUYMD NOT = ZERO
         MOVE  "2"          TO  LINK-IN-KBN
         MOVE  DSP-SYUYMD   TO  LINK-IN-YMD8
         CALL  "SKYDTCKB" USING LINK-IN-KBN
                                LINK-IN-YMD6
                                LINK-IN-YMD8
                                LINK-OUT-RET
                                LINK-OUT-YMD
         IF  LINK-OUT-RET NOT = ZERO
             IF  ERR-FLG = ZERO
                 MOVE  1    TO  ERR-FLG
             END-IF
             MOVE  "C"      TO  EDIT-CURSOR OF DSP-SYUYMD
             MOVE  "R"      TO  EDIT-OPTION OF DSP-SYUYMD
             GO TO  HEAD3B-CHK-EXIT
         END-IF
     END-IF.

* 店着日
     IF      DSP-TENYMD IS NUMERIC
         AND DSP-TENYMD NOT = ZERO
         MOVE  "2"          TO  LINK-IN-KBN
         MOVE  DSP-TENYMD   TO  LINK-IN-YMD8
         CALL  "SKYDTCKB" USING LINK-IN-KBN
                                LINK-IN-YMD6
                                LINK-IN-YMD8
                                LINK-OUT-RET
                                LINK-OUT-YMD
         IF  LINK-OUT-RET NOT = ZERO
             IF  ERR-FLG = ZERO
                 MOVE  1    TO  ERR-FLG
             END-IF
             MOVE  "C"      TO  EDIT-CURSOR OF DSP-TENYMD
             MOVE  "R"      TO  EDIT-OPTION OF DSP-TENYMD
             GO TO  HEAD3B-CHK-EXIT
         END-IF
     END-IF.

* １ケース当たりケース数

 HEAD3B-CHK-EXIT.
     EXIT.

****************************************************************
*  明細編集処理                                                *
****************************************************************
 MS-EDT-SEC         SECTION.

* 対象データ内部テーブル格納
     MOVE  ZERO             TO  IX.
     MOVE  LOW-VALUE        TO  FG-NFJOHOF-END.
     MOVE  TBL-KANRNO       TO  WK-RD-KANRNO.
     MOVE  TBL-KANRNO       TO  KH1-F01.  *> 管理番号
     MOVE  DSP-SKBACD       TO  KH1-F05.  *> 作場ＣＤ
     MOVE  DSP-SHOCD        TO  KH1-F13.  *> 相手商品ＣＤ
     MOVE  ZERO             TO  KH1-F06.  *> 店舗ＣＤ

     PERFORM  RD-NFJOHOF-SEC.

     PERFORM  UNTIL  FG-NFJOHOF-END = "END"
       ADD 1  TO IX
       IF  IX <= 600
           MOVE  IX         TO  TBL-HAKO-IXMAX
           PERFORM  MS-TBL-EDT-SEC
       ELSE
           DISPLAY "SSY3757I TBL-HAKO TBL OVER ]]]]"  UPON CONS
       END-IF

       PERFORM  RD-NFJOHOF-SEC
     END-PERFORM.

* 最大格納ページ計算
     MOVE  ZERO            TO  WK-AMARI.
     DIVIDE 10  INTO       TBL-HAKO-IXMAX
                GIVING     TBL-HAKO-PGMAX
                REMAINDER  WK-AMARI.
     IF  WK-AMARI NOT = ZERO
         ADD  1  TO TBL-HAKO-PGMAX
     END-IF.

* 先頭ページ編集
     MOVE  1                TO  IX-PG.
     PERFORM  MS-GMN-EDT-SEC.

 MS-EDT-EXIT.
     EXIT.
****************************************************************
*  明細テーブル編集処理                                        *
****************************************************************
 MS-TBL-EDT-SEC         SECTION.
     MOVE  "1"              TO  TBL-KAKUNO-FG (IX).
     MOVE  KH1-F06          TO  TBL-TENCD  (IX). *> 店舗ＣＤ
     MOVE  KH1-F07          TO  TBL-DENNO  (IX). *> 伝票番号
     MOVE  KH1-F08          TO  TBL-GYONO  (IX). *> 行番号
     MOVE  KH1-F09          TO  TBL-NOHYMD (IX). *> 納品日

     MOVE  KH1-F19          TO  TBL-HSURYO (IX). *> 発注数量
     MOVE  KH1-F21          TO  TBL-HHAKOSU(IX). *> 発注箱数
* 作場ＣＤ
     IF DSP-CHGSAK NOT = SPACE  *> 一括変更あり
        MOVE  DSP-CHGSAK    TO  TBL-MSSAKU (IX)
     ELSE
        MOVE  KH1-F05       TO  TBL-MSSAKU (IX)
     END-IF.

     MOVE  KH1-F20          TO  TBL-SURYO  (IX). *> 訂正数量
* 訂正箱数
*   数 量、F20（訂正数）
*   入 数、F18（発注単位）
*   １箱当たりのケース数、画面入力
*     箱数 ＝ 数量 ／ 入数 ／ １箱当たりのケース数
*                             [切上げて整数にする-
     IF     DSP-HCACE IS NUMERIC  *> 一括変更あり
        AND DSP-HCACE NOT = ZERO
        MOVE  ZERO          TO  WK-AMARI
        DIVIDE KH1-F18  INTO KH1-F20
            GIVING WK-CASESU  REMAINDER WK-AMARI
        IF WK-AMARI NOT = ZERO
           ADD  1  TO  WK-CASESU
        END-IF

        MOVE  ZERO          TO  WK-AMARI
        DIVIDE DSP-HCACE INTO WK-CASESU
            GIVING WK-HAKOSU  REMAINDER WK-AMARI
        IF WK-AMARI NOT = ZERO
           ADD  1  TO  WK-HAKOSU
        END-IF

        MOVE  WK-HAKOSU     TO  TBL-HAKOSU (IX)
     ELSE
        MOVE  KH1-F22       TO  TBL-HAKOSU (IX)
     END-IF.

     MOVE  KH1-F23          TO  TBL-TEIKBN (IX). *> 訂正区分
* 出荷日
     IF     DSP-SYUYMD IS NUMERIC
        AND DSP-SYUYMD NOT = ZERO  *> 一括変更あり
        MOVE  DSP-SYUYMD    TO  TBL-SYYMD  (IX)
     ELSE
        MOVE  KH1-F10       TO  TBL-SYYMD  (IX)
     END-IF.

* 店着日
     IF     DSP-TENYMD IS NUMERIC
        AND DSP-TENYMD NOT = ZERO  *> 一括変更あり
        MOVE  DSP-TENYMD    TO  TBL-TNYMD  (IX)
     ELSE
        MOVE  KH1-F11       TO  TBL-TNYMD  (IX)
     END-IF.

 MS-TBL-EDT-EXIT.
     EXIT.
****************************************************************
*  明細画面編集処理                                            *
****************************************************************
 MS-GMN-EDT-SEC         SECTION.

     PERFORM  VARYING IX  FROM 1 BY 1
              UNTIL   IX > 10
       IF  TBL-KAKUNO-FGR (IX-PG IX) NOT = SPACE
           MOVE  "M"    TO  EDIT-OPTION OF DSP-MEISAK (IX)
           MOVE  "M"    TO  EDIT-OPTION OF DSP-SURYO  (IX)
           MOVE  "M"    TO  EDIT-OPTION OF DSP-HAKOSU (IX)
           MOVE  "M"    TO  EDIT-OPTION OF DSP-TEISEI (IX)
           MOVE  "M"    TO  EDIT-OPTION OF DSP-SYYMD  (IX)
           MOVE  "M"    TO  EDIT-OPTION OF DSP-TNYMD  (IX)

           MOVE  SPACE  TO  EDIT-CURSOR OF DSP-MEISAK (IX)
           MOVE  SPACE  TO  EDIT-CURSOR OF DSP-SURYO  (IX)
           MOVE  SPACE  TO  EDIT-CURSOR OF DSP-HAKOSU (IX)
           MOVE  SPACE  TO  EDIT-CURSOR OF DSP-TEISEI (IX)
           MOVE  SPACE  TO  EDIT-CURSOR OF DSP-SYYMD  (IX)
           MOVE  SPACE  TO  EDIT-CURSOR OF DSP-TNYMD  (IX)

           MOVE  TBL-TENCDR  (IX-PG IX)
                                 TO  DSP-TENCD  (IX)
           IF      DSP-KBN = 1  *> 相手取引先コード
               AND (    SV-HD-BCHNO3 IS NUMERIC
                    AND SV-HD-BCHNO3 NOT = ZERO)
               MOVE  SV-HD-BCHNO3  TO  TEN-F52
           ELSE
***************MOVE  137607        TO  TEN-F52
               MOVE  999977        TO  TEN-F52
           END-IF
           MOVE  TBL-TENCDR  (IX-PG IX)
                                 TO  TEN-F011 *> 店舗コード
           PERFORM  RD-HTENMS-SEC
           IF  FG-HTENMS-INV = 1
               MOVE  SPACE       TO  DSP-TENNM (IX)
           ELSE
               MOVE  TEN-F03     TO  DSP-TENNM (IX)
           END-IF

           MOVE  TBL-HSURYOR (IX-PG IX) TO
                            DSP-HACSUU (IX) *> 発注数量
           MOVE  TBL-HHAKOSUR(IX-PG IX) TO
                            DSP-HACHAK (IX) *> 発注箱数
           MOVE  TBL-MSSAKUR (IX-PG IX) TO
                            DSP-MEISAK (IX) *> 明細作場
           MOVE  TBL-SURYOR  (IX-PG IX) TO
                            DSP-SURYO  (IX) *> 訂正数量
           MOVE  TBL-HAKOSUR (IX-PG IX) TO
                            DSP-HAKOSU (IX) *> 訂正箱数
           MOVE  TBL-TEIKBNR (IX-PG IX) TO
                            DSP-TEISEI (IX) *> 訂正区分
           MOVE  TBL-SYYMDR  (IX-PG IX) TO
                            DSP-SYYMD  (IX) *> 出荷日
           MOVE  TBL-TNYMDR  (IX-PG IX) TO
                            DSP-TNYMD  (IX) *> 店着日
*        入力項目属性
           MOVE  SPACE  TO  EDIT-STATUS OF DSP-MEISAK (IX)
           MOVE  SPACE  TO  EDIT-STATUS OF DSP-SURYO  (IX)
           MOVE  SPACE  TO  EDIT-STATUS OF DSP-HAKOSU (IX)
           MOVE  SPACE  TO  EDIT-STATUS OF DSP-TEISEI (IX)
           MOVE  SPACE  TO  EDIT-STATUS OF DSP-SYYMD  (IX)
           MOVE  SPACE  TO  EDIT-STATUS OF DSP-TNYMD  (IX)

       ELSE
           MOVE  SPACE  TO  DSP-TENCD  (IX)(1:5)
           MOVE  SPACE  TO  DSP-TENNM  (IX)
           MOVE  SPACE  TO  DSP-HACSUU (IX)(1:6)
           MOVE  SPACE  TO  DSP-HACHAK (IX)(1:6)
           MOVE  SPACE  TO  DSP-MEISAK (IX)
           MOVE  SPACE  TO  DSP-SURYO  (IX)(1:6)
           MOVE  SPACE  TO  DSP-HAKOSU (IX)(1:6)
           MOVE  SPACE  TO  DSP-TEISEI (IX)
           MOVE  SPACE  TO  DSP-SYYMD  (IX)(1:8)
           MOVE  SPACE  TO  DSP-TNYMD  (IX)(1:8)
*        プロテクト項目属性
           MOVE  "X"    TO  EDIT-STATUS OF DSP-MEISAK (IX)
           MOVE  "X"    TO  EDIT-STATUS OF DSP-SURYO  (IX)
           MOVE  "X"    TO  EDIT-STATUS OF DSP-HAKOSU (IX)
           MOVE  "X"    TO  EDIT-STATUS OF DSP-TEISEI (IX)
           MOVE  "X"    TO  EDIT-STATUS OF DSP-SYYMD  (IX)
           MOVE  "X"    TO  EDIT-STATUS OF DSP-TNYMD  (IX)

       END-IF

     END-PERFORM.

 MS-GMN-EDT-EXIT.
     EXIT.
****************************************************************
*  店舗マスタ読込み                                            *
****************************************************************
 RD-HTENMS-SEC         SECTION.
     READ  HTENMS
       INVALID
         MOVE  1            TO  FG-HTENMS-INV
       NOT INVALID
         MOVE  ZERO         TO  FG-HTENMS-INV
     END-READ.

 RD-HTENMS-EXIT.
     EXIT.
****************************************************************
*             明細項目　入力( PSW = 4 )                        *
****************************************************************
 DSP-BODY-SEC          SECTION.
     MOVE     "DSP-BODY-SEC"      TO   S-NAME.
*
     PERFORM    DSP-WRITE-SEC.
     PERFORM    DSP-READ-SEC.
*
     EVALUATE   DSP-FNC
*実行
         WHEN   "E000"
                PERFORM  BODY-CHK-SEC
                IF  ERR-FLG = ZERO
*                   全体の再チェック
                    PERFORM  MS-CHK-ALL-SEC
                    IF  ERR-FLG = ZERO
                        MOVE  "5"   TO  PSW
                    END-IF
                END-IF
*終了
         WHEN   "F005"
                MOVE    "END"    TO   END-FLG
*前頁
         WHEN   "F011"
           IF  IX-PG = 1
               MOVE  7           TO  ERR-FLG
           ELSE  *> 前頁編集
               PERFORM  BODY-CHK-SEC
               IF  ERR-FLG = ZERO
                   COMPUTE  IX-PG = IX-PG - 1
                   PERFORM  MS-GMN-EDT-SEC
               END-IF
           END-IF
*次頁
         WHEN   "F012"
           IF  IX-PG >= TBL-HAKO-PGMAX
               MOVE  8           TO  ERR-FLG
           ELSE  *> 次頁編集
               PERFORM  BODY-CHK-SEC
               IF  ERR-FLG = ZERO
                   COMPUTE  IX-PG = IX-PG + 1
                   PERFORM  MS-GMN-EDT-SEC
               END-IF
           END-IF
*項目戻し
         WHEN   "F006"
                MOVE    "6"      TO   PSW
*取消
         WHEN   "F004"
                MOVE    "1"      TO   PSW
                PERFORM   INIT-DSP-SEC
         WHEN   OTHER
                MOVE     6       TO   ERR-FLG
                GO       TO      DSP-BODY-SEC
     END-EVALUATE.
*
 DSP-BODY-EXIT.
     EXIT.
****************************************************************
*             明細項目チェック                                 *
****************************************************************
 BODY-CHK-SEC          SECTION.
     MOVE  "BODY-CHK-SEC"        TO  S-NAME.

     PERFORM  VARYING IX  FROM 1 BY 1
              UNTIL IX > 10
       PERFORM  BODY-CHKB-SEC

     END-PERFORM.

     IF  ERR-FLG NOT = ZERO
         GO TO  BODY-CHK-EXIT
     END-IF.

     PERFORM  GMN-TO-TBL-SEC.

 BODY-CHK-EXIT.
     EXIT.
****************************************************************
*             明細項目チェックＢ                               *
****************************************************************
 BODY-CHKB-SEC          SECTION.
     IF  TBL-KAKUNO-FGR (IX-PG IX) = SPACE
         GO TO  BODY-CHKB-EXIT
     END-IF.

     MOVE  "M"              TO  EDIT-OPTION OF DSP-MEISAK (IX).
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-MEISAK (IX).
     MOVE  "M"              TO  EDIT-OPTION OF DSP-SURYO  (IX).
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-SURYO  (IX).
     MOVE  "M"              TO  EDIT-OPTION OF DSP-HAKOSU (IX).
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-HAKOSU (IX).
     MOVE  "M"              TO  EDIT-OPTION OF DSP-TEISEI (IX).
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-TEISEI (IX).
     MOVE  "M"              TO  EDIT-OPTION OF DSP-SYYMD  (IX).
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-SYYMD  (IX).
     MOVE  "M"              TO  EDIT-OPTION OF DSP-TNYMD  (IX).
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-TNYMD  (IX).
* 作場ＣＤ
     IF  DSP-MEISAK (IX) = SPACE
         IF  ERR-FLG = ZERO
             MOVE  9        TO  ERR-FLG
         END-IF
         MOVE  "C"          TO  EDIT-CURSOR OF DSP-MEISAK (IX)
         MOVE  "R"          TO  EDIT-OPTION OF DSP-MEISAK (IX)
         GO TO  BODY-CHKB-EXIT
     END-IF.

     MOVE  DSP-MEISAK (IX)  TO  SKB-F01.
     PERFORM  RD-SAKUBAF-SEC.
     IF  FG-SAKUBAF-INV = 1
         IF  ERR-FLG = ZERO
             MOVE  1        TO  ERR-FLG
         END-IF
         MOVE  "C"          TO  EDIT-CURSOR OF DSP-MEISAK (IX)
         MOVE  "R"          TO  EDIT-OPTION OF DSP-MEISAK (IX)
         GO TO  BODY-CHKB-EXIT
     END-IF.
* 訂正数量
     IF  DSP-SURYO (IX) > DSP-HACSUU (IX)
         IF  ERR-FLG = ZERO
             MOVE  1        TO  ERR-FLG
         END-IF
         MOVE  "C"          TO  EDIT-CURSOR OF DSP-SURYO (IX)
         MOVE  "R"          TO  EDIT-OPTION OF DSP-SURYO (IX)
         GO TO  BODY-CHKB-EXIT
     END-IF.

* 訂正箱数

* 訂正区分
     IF  DSP-SURYO (IX) NOT = DSP-HACSUU (IX)
         IF  DSP-TEISEI (IX)  = SPACE
             IF  ERR-FLG = ZERO
                 MOVE  1    TO  ERR-FLG
             END-IF
             MOVE  "C"      TO  EDIT-CURSOR OF DSP-TEISEI (IX)
             MOVE  "R"      TO  EDIT-OPTION OF DSP-TEISEI (IX)
             GO TO  BODY-CHKB-EXIT
         ELSE
             IF  DSP-TEISEI (IX)  = "1" OR "2" OR "3" OR "4" OR
                                    "5" OR "6" OR "7" OR "8" OR
                                    "9"
                 CONTINUE
             ELSE
                 IF  ERR-FLG = ZERO
                     MOVE  1    TO  ERR-FLG
                 END-IF
                 MOVE  "C"  TO  EDIT-CURSOR OF DSP-TEISEI (IX)
                 MOVE  "R"  TO  EDIT-OPTION OF DSP-TEISEI (IX)
                 GO TO  BODY-CHKB-EXIT
             END-IF
         END-IF
     ELSE
*************発注数と訂正数が同じ場合、訂正区分は初期化する。
             MOVE  SPACE    TO  DSP-TEISEI (IX)
     END-IF.

* 出荷日
     IF      DSP-SYYMD (IX) IS NUMERIC
         AND DSP-SYYMD (IX) NOT = ZERO
         MOVE  "2"            TO  LINK-IN-KBN
         MOVE  DSP-SYYMD(IX)  TO  LINK-IN-YMD8
         CALL  "SKYDTCKB" USING LINK-IN-KBN
                                LINK-IN-YMD6
                                LINK-IN-YMD8
                                LINK-OUT-RET
                                LINK-OUT-YMD
         IF  LINK-OUT-RET NOT = ZERO
             IF  ERR-FLG = ZERO
                 MOVE  1    TO  ERR-FLG
             END-IF
             MOVE  "C"      TO  EDIT-CURSOR OF DSP-SYYMD (IX)
             MOVE  "R"      TO  EDIT-OPTION OF DSP-SYYMD (IX)
             GO TO  HEAD3B-CHK-EXIT
         END-IF
     END-IF.

* 店着日
     IF      DSP-TNYMD (IX) IS NUMERIC
         AND DSP-TNYMD (IX) NOT = ZERO
         MOVE  "2"             TO  LINK-IN-KBN
         MOVE  DSP-TNYMD (IX)  TO  LINK-IN-YMD8
         CALL  "SKYDTCKB" USING LINK-IN-KBN
                                LINK-IN-YMD6
                                LINK-IN-YMD8
                                LINK-OUT-RET
                                LINK-OUT-YMD
         IF  LINK-OUT-RET NOT = ZERO
             IF  ERR-FLG = ZERO
                 MOVE  1    TO  ERR-FLG
             END-IF
             MOVE  "C"      TO  EDIT-CURSOR OF DSP-TNYMD (IX)
             MOVE  "R"      TO  EDIT-OPTION OF DSP-TNYMD (IX)
             GO TO  HEAD3B-CHK-EXIT
         END-IF
     END-IF.

     IF     DSP-MEISAK (IX)  NOT = DSP-SKBACD       *> 作場ＣＤ
*********OR DSP-TNYMD  (IX)  NOT = TBL-NOHYMD (IX)  *> 店着日
            *> キー項目変更
         MOVE  TBL-KANRNO        TO  KH3-F01  *> 管理番号
         MOVE  DSP-MEISAK (IX)   TO  KH3-F05  *> 作場ＣＤ
         MOVE  TBL-TENCD  (IX)   TO  KH3-F06  *> 店舗ＣＤ
         MOVE  TBL-DENNO  (IX)   TO  KH3-F07  *> 伝票番号
         MOVE  TBL-GYONO  (IX)   TO  KH3-F08  *> 行番
         MOVE  TBL-NOHYMD (IX)   TO  KH3-F09  *> 納品日
         PERFORM  RD-NFJOHOF3-SEC
         IF  FG-NFJOHOF3-INV = ZERO
             IF  ERR-FLG = ZERO
                 MOVE  12   TO  ERR-FLG
             END-IF
             MOVE  "C"      TO  EDIT-CURSOR OF DSP-MEISAK (IX)
             MOVE  "R"      TO  EDIT-OPTION OF DSP-MEISAK (IX)
             MOVE  "R"      TO  EDIT-OPTION OF DSP-TNYMD (IX)
             GO TO  HEAD3B-CHK-EXIT
         END-IF
     END-IF.


 BODY-CHKB-EXIT.
     EXIT.
****************************************************************
*  現画面内容をＴＢＬに格納する                                *
****************************************************************
 GMN-TO-TBL-SEC             SECTION.

     PERFORM  VARYING IX  FROM 1 BY 1
              UNTIL IX > 10
       IF  TBL-KAKUNO-FGR (IX-PG IX) NOT = SPACE
           MOVE  DSP-MEISAK (IX)  TO
                 TBL-MSSAKUR (IX-PG IX) *> 明細作場
           MOVE  DSP-SURYO  (IX)  TO
                 TBL-SURYOR  (IX-PG IX) *> 訂正数量
           MOVE  DSP-HAKOSU (IX)  TO
                 TBL-HAKOSUR (IX-PG IX) *> 訂正箱数
           MOVE  DSP-TEISEI (IX)  TO
                 TBL-TEIKBNR (IX-PG IX) *> 訂正区分
           MOVE  DSP-SYYMD  (IX)  TO
                 TBL-SYYMDR  (IX-PG IX) *> 出荷日
           MOVE  DSP-TNYMD  (IX)  TO
                 TBL-TNYMDR  (IX-PG IX) *> 店着日
       END-IF

     END-PERFORM.

 GMN-TO-TBL-EXIT.
     EXIT.
****************************************************************
*  明細データ最終チェック
****************************************************************
 MS-CHK-ALL-SEC          SECTION.
     MOVE  "MS-CHK-ALL-SEC" TO  S-NAME.

     MOVE  IX-PG            TO  SV-IX-PG.
     PERFORM  VARYING IX-PG  FROM 1 BY 1
              UNTIL   IX-PG > TBL-HAKO-PGMAX
       MOVE  IX-PG          TO  IX-PGAL
       PERFORM  MS-GMN-EDT-SEC

       PERFORM  VARYING IX  FROM 1 BY 1
                UNTIL IX > 10
         PERFORM  BODY-CHKB-SEC

       END-PERFORM

       IF  ERR-FLG NOT = ZERO
           MOVE  TBL-HAKO-PGMAX  TO  IX-PG
       END-IF

     END-PERFORM.

     IF  ERR-FLG NOT = ZERO *> エラー画面を表示
         MOVE  IX-PGAL           TO  IX-PG
         GO TO  MS-CHK-ALL-EXIT
     END-IF.

* 元の画面に戻す。
     MOVE  SV-IX-PG              TO  IX-PG.
     PERFORM  MS-GMN-EDT-SEC.

 MS-CHK-ALL-EXIT.
     EXIT.
****************************************************************
*             確認処理　入力（ PSW = 5 ）
****************************************************************
 DSP-KAKU-SEC          SECTION.
     MOVE     "DSP-KAKU-SEC"      TO   S-NAME.
*
     PERFORM    DSP-WRITE-SEC.
     PERFORM    DSP-READ-SEC.
*
     EVALUATE   DSP-FNC
*実行
         WHEN   "E000"
                PERFORM  FILE-UPD-SEC
                IF  ERR-FLG NOT = ZERO
                    IF DSP-KBN = "1"  *> オンライン
                       MOVE  "2"  TO  PSW
                    ELSE              *> 手書き
                       MOVE  "3"  TO  PSW
                    END-IF
                ELSE
                    PERFORM  INIT-DSP-SEC
                    MOVE  SAV-SHORI  TO  DSP-KBN
                    MOVE  SV-HD-BCHNO1 TO  DSP-BCHNO1
                    MOVE  SV-HD-BCHNO2 TO  DSP-BCHNO2
                    MOVE  SV-HD-BCHNO3 TO  DSP-BCHNO3
                    IF DSP-KBN = "1"  *> オンライン
                       MOVE  "2"  TO  PSW
                    ELSE              *> 手書き
                       MOVE  "3"  TO  PSW
                    END-IF
                END-IF

*終了
         WHEN   "F005"
                MOVE    "END"    TO   END-FLG
*項目戻し
         WHEN   "F006"
                MOVE    "4"      TO   PSW
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
*    ファイル更新                                              *
****************************************************************
 FILE-UPD-SEC           SECTION.
     MOVE  "FILE-UPD-SEC"   TO  S-NAME.

     PERFORM  SDATE-GET-SEC.

     PERFORM  VARYING IX  FROM 1 BY 1
              UNTIL   IX > TBL-HAKO-IXMAX
       PERFORM  FILE-UPDB-SEC

     END-PERFORM.

 FILE-UPD-EXIT.
     EXIT.
****************************************************************
*    ファイル更新Ｂ                                            *
****************************************************************
 FILE-UPDB-SEC           SECTION.
     MOVE  "FILE-UPDB-SEC"   TO  S-NAME.

     MOVE  TBL-KANRNO       TO  KH3-F01.  *> 管理番号
     MOVE  DSP-SKBACD       TO  KH3-F05.  *> 作場ＣＤ
     MOVE  TBL-TENCD  (IX)  TO  KH3-F06.  *> 店舗ＣＤ
     MOVE  TBL-DENNO  (IX)  TO  KH3-F07.  *> 伝票番号
     MOVE  TBL-GYONO  (IX)  TO  KH3-F08.  *> 行番
     MOVE  TBL-NOHYMD (IX)  TO  KH3-F09.  *> 納品日

     PERFORM  RD-NFJOHOF3-SEC.
     IF  FG-NFJOHOF3-INV = 1
         GO TO  FILE-UPDB-EXIT
     END-IF.

     IF     KH3-F05 NOT = TBL-MSSAKU (IX)
         OR KH3-F20 NOT = TBL-SURYO  (IX)
         OR KH3-F22 NOT = TBL-HAKOSU (IX)
         OR KH3-F23 NOT = TBL-TEIKBN (IX)
         OR KH3-F10 NOT = TBL-SYYMD  (IX)
         OR KH3-F11 NOT = TBL-TNYMD  (IX)

         MOVE  KH3-REC      TO  KHW1-REC *> 更新前ﾚｺｰﾄﾞの退避

         MOVE  TBL-MSSAKU (IX)    TO  KH3-F05 *> 明細作場
         MOVE  KH3-F20            TO  WK-UPDMAE-SURYO
         MOVE  TBL-SURYO  (IX)    TO  KH3-F20 *> 訂正数量
         MOVE  TBL-HAKOSU (IX)    TO  KH3-F22 *> 訂正箱数
         MOVE  TBL-TEIKBN (IX)    TO  KH3-F23 *> 訂正区分
         MOVE  TBL-SYYMD  (IX)    TO  KH3-F10 *> 出荷日
         MOVE  TBL-TNYMD  (IX)    TO  KH3-F11 *> 店着日

*    商品変換ＴＢＬ
         MOVE  KH3-F04           TO  SCV-F01 *> 相手取引先ＣＤ
         MOVE  KH3-F13           TO  SCV-F02 *> 相手商品コード
         PERFORM  RD-HSHOTBL-SEC
         IF  FG-HSHOTBL-INV = 1
             IF  ERR-FLG = ZERO
                 MOVE  11        TO  ERR-FLG
             END-IF
             MOVE  "C"           TO  EDIT-CURSOR OF DSP-SHOCD
             MOVE  "R"           TO  EDIT-OPTION OF DSP-SHOCD
             GO TO  FILE-UPDB-EXIT
         END-IF

         PERFORM  FILE-UPDC-SEC

         IF     KH3-F05 NOT = KHW1-F05  *> 作場ＣＤ
*************OR KH3-F09 NOT = KHW1-F09  *> 店着日
               *> キー項目変更の場合
             MOVE  KH3-REC   TO  KHW2-REC *> 更新後ﾚｺｰﾄﾞの退避
             MOVE  KHW1-REC  TO  KH3-REC  *> 更新前ﾚｺｰﾄﾞの復元
             DELETE  NFJOHOF3
             MOVE  KHW2-REC  TO  KH3-REC  *> 更新後ﾚｺｰﾄﾞの復元
             WRITE  KH3-REC  *> 基本情報ファイル更新
         ELSE  *> 通常の場合
             REWRITE  KH3-REC  *> 基本情報ファイル更新
         END-IF
     END-IF.


 FILE-UPDB-EXIT.
     EXIT.
****************************************************************
*    基本情報ファイル読込み
****************************************************************
 RD-NFJOHOF3-SEC           SECTION.
     READ  NFJOHOF3  KEY IS KH3-F01
                            KH3-F05
                            KH3-F06
                            KH3-F07
                            KH3-F08
                            KH3-F09
       INVALID
         MOVE  1            TO  FG-NFJOHOF3-INV
       NOT INVALID
         MOVE  ZERO         TO  FG-NFJOHOF3-INV
     END-READ.

 RD-NFJOHOF3--EXIT.
     EXIT.
****************************************************************
*    ファイル更新Ｃ                                            *
****************************************************************
 FILE-UPDC-SEC           SECTION.
     IF  WK-UPDMAE-SURYO = TBL-SURYO  (IX)
         GO TO  FILE-UPDC-EXIT
     END-IF.

     MOVE  KH3-F04          TO  URI-F01.  *> 取引先コード
     MOVE  KH3-F07          TO  URI-F02.  *> 伝票番号
     MOVE  ZERO             TO  URI-F04.  *> 相殺区分
     MOVE  40               TO  URI-F051. *> 伝区コード
     MOVE  KH3-F08          TO  URI-F03.  *> 行番号

     PERFORM  RD-SHTDENF-SEC.
     IF  FG-SHTDENF-INV = 1
         GO TO  FILE-UPDC-EXIT
     END-IF.

* 出荷場所、SCV-F28
* 商品ＣＤ、SCV-F031
* 品単ＣＤ、SCV-F032
* _番    、SCV-F08

     MOVE  SCV-F04          TO  ZAK-F01.  *> 倉庫コード
     MOVE  SCV-F031         TO  ZAK-F021. *> 商品ＣＤ
     MOVE  SCV-F032         TO  ZAK-F022. *> 品単
     MOVE  SCV-F08          TO  ZAK-F03.  *> _番１

     PERFORM  RD-ZAMZAIF-SEC.

* 在庫マスタなしの場合
 FILE-UPDC-010.
     IF  FG-ZAMZAIF-INV = 1
         CONTINUE
     ELSE
         GO TO  FILE-UPDC-010B
     END-IF.

     MOVE  ZERO             TO  FG-URI-ZKHKATE.
*    ナフコ商品名称マスタ
     MOVE  KH3-F13          TO  SYO-F01. *> ナフコ商品ＣＤ
     PERFORM  RD-NFMEIMS-SEC.
     IF  FG-NFMEIMS-INV = 1
         GO TO  FILE-UPDC-010-EXIT
     END-IF.

     MOVE  SPACE            TO  ZAK-REC.
     INITIALIZE  ZAK-REC.

* 出荷場所、KH3-F28
* 商品ＣＤ、KH3-F15
* 品単ＣＤ、KH3-F16
* _番    、KH3-F29、X(06)
     MOVE  KH3-F28          TO  ZAK-F01.  *> 倉庫ＣＤ
     MOVE  KH3-F15          TO  ZAK-F021. *> サカタ商品ＣＤ
     MOVE  KH3-F16          TO  ZAK-F022. *> サカタ品単ＣＤ
     MOVE  KH3-F29          TO  ZAK-F03.  *> _番
     MOVE  TBL-SURYO (IX)   TO  ZAK-F27   *> 未出庫数
     MOVE  KH3-F04          TO  ZAK-F29.  *> 取引先ＣＤ
     MOVE  SYO-F07          TO  ZAK-F30.  *> カナ名称
     MOVE  SYS-DATE         TO  ZAK-F98.  *> 登録日
     MOVE  SYS-DATE         TO  ZAK-F99.  *> 更新日
     WRITE  ZAK-REC.  *> 在庫マスタ更新

     GO TO  FILE-UPDC-010-EXIT.
* 在庫マスタありの場合
 FILE-UPDC-010B.
*   引当済数      、ZAK-F28
*   未出庫数      、ZAK-F27
*   在庫引当ＦＬＧ、URI-F27D

     IF URI-F27D = "1"  *> 引当あり
*      在庫引当済数を解除する。
        SUBTRACT  WK-UPDMAE-SURYO
                      FROM  ZAK-F28 *> 引当済数
     END-IF.
*   未出庫数を解除する。
     SUBTRACT  WK-UPDMAE-SURYO
                      FROM  ZAK-F27 *> 未出庫数

* 在庫引当可能数の計算
*   在庫引当可能数 ＝ 現在庫数 － 引当済み数
*     現在庫数、ZAK-F04
*     引当済数、ZAK-F28

     COMPUTE  WK-HIKI-KANO-ZAIKOSU =
              ZAK-F04 - ZAK-F28.

* 在庫引当可能数に当データの数量を引当計算する。
     COMPUTE  WK-HIKI-KANO-ZAIKOSU =
              WK-HIKI-KANO-ZAIKOSU - TBL-SURYO (IX).

* 引当計算の結果で在庫引当を行う。
     IF WK-HIKI-KANO-ZAIKOSU <= ZERO  *> 引当なし
        MOVE  ZERO          TO  FG-URI-ZKHKATE
     ELSE                            *> 引当あり
        MOVE  1             TO  FG-URI-ZKHKATE
        COMPUTE  ZAK-F28 =      *> 引当済数
             ZAK-F28 + TBL-SURYO (IX)
     END-IF.

     COMPUTE  ZAK-F27 =      *> 未出庫数
         ZAK-F27 + TBL-SURYO (IX).
     MOVE  SYS-DATE         TO  ZAK-F99. *> 更新日

 FILE-UPDC-010-EXIT.
     EXIT.

 FILE-UPDC-020.
     MOVE  FG-URI-ZKHKATE   TO  URI-F27D.   *> 在庫引当ＦＬＧ
     MOVE  TBL-SURYO  (IX)  TO  URI-F15.    *> 数量
     COMPUTE  URI-F181 = URI-F172 * TBL-SURYO (IX). *> 原価金額
     COMPUTE  URI-F182 = URI-F173 * TBL-SURYO (IX). *> 売価金額

     REWRITE  URI-REC. *> 売上伝票ファイル更新

 FILE-UPDC-EXIT.
     EXIT.
****************************************************************
*    売上伝票ファイル読込み                                    *
****************************************************************
 RD-SHTDENF-SEC           SECTION.
     READ  SHTDENF  KEY IS URI-F01
                           URI-F02
                           URI-F04
                           URI-F051
                           URI-F03
       INVALID
         MOVE  1            TO  FG-SHTDENF-INV
       NOT INVALID
         MOVE  ZERO         TO  FG-SHTDENF-INV
     END-READ.

 RD-SHTDENF--EXIT.
     EXIT.
****************************************************************
*   在庫マスタ検索                                             *
****************************************************************
 RD-ZAMZAIF-SEC          SECTION.
     READ  ZAMZAIF  KEY IS ZAK-F01
                           ZAK-F021
                           ZAK-F022
                           ZAK-F031
                           ZAK-F032
                           ZAK-F033
       INVALID
         MOVE  1            TO  FG-ZAMZAIF-INV
       NOT INVALID
         MOVE  ZERO         TO  FG-ZAMZAIF-INV
     END-READ.

 RD-ZAMZAIF-EXIT.
     EXIT.
****************************************************************
*             初期画面表示                                     *
****************************************************************
 INIT-DSP-SEC          SECTION.
     MOVE  "INIT-DSP-SEC"   TO  S-NAME.
*画面の初期化
     MOVE  SPACE            TO  DSP-FSY37571.
*ＰＧＩＤ
     MOVE  "SSY3757I"       TO  DSP-PGID.
*システム日付転送
     MOVE  SYS-DATE         TO  DSP-SDATE.
*システム時間転送
     MOVE  WK-TIME(1:6)     TO  DSP-STIME.
*リバース，カーソルパーク解除
***  メッセージＮＯ
     MOVE  "M"              TO  EDIT-OPTION OF DSP-BCHNO1.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-BCHNO1.
     MOVE  "M"              TO  EDIT-OPTION OF DSP-BCHNO2.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-BCHNO2.
     MOVE  "M"              TO  EDIT-OPTION OF DSP-BCHNO3.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-BCHNO3.
     MOVE  "M"              TO  EDIT-OPTION OF DSP-JOBNO.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-JOBNO.
     MOVE  "M"              TO  EDIT-OPTION OF DSP-SHOCD.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-SHOCD.
     MOVE  "M"              TO  EDIT-OPTION OF DSP-SKBACD.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-SKBACD.
     MOVE  "M"              TO  EDIT-OPTION OF DSP-CHGSAK.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-CHGSAK.
     MOVE  "M"              TO  EDIT-OPTION OF DSP-SYUYMD.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-SYUYMD.
     MOVE  "M"              TO  EDIT-OPTION OF DSP-TENYMD.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-TENYMD.
     MOVE  "M"              TO  EDIT-OPTION OF DSP-HCACE.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-HCACE.
     PERFORM  VARYING IX  FROM 1 BY 1
              UNTIL   IX > 11
       MOVE  "M"            TO  EDIT-OPTION OF DSP-MEISAK (IX)
       MOVE  SPACE          TO  EDIT-CURSOR OF DSP-MEISAK (IX)
       MOVE  "M"            TO  EDIT-OPTION OF DSP-SURYO  (IX)
       MOVE  SPACE          TO  EDIT-CURSOR OF DSP-SURYO  (IX)
       MOVE  "M"            TO  EDIT-OPTION OF DSP-HAKOSU (IX)
       MOVE  SPACE          TO  EDIT-CURSOR OF DSP-HAKOSU (IX)
       MOVE  "M"            TO  EDIT-OPTION OF DSP-TEISEI (IX)
       MOVE  SPACE          TO  EDIT-CURSOR OF DSP-TEISEI (IX)
       MOVE  "M"            TO  EDIT-OPTION OF DSP-SYYMD  (IX)
       MOVE  SPACE          TO  EDIT-CURSOR OF DSP-SYYMD  (IX)
       MOVE  "M"            TO  EDIT-OPTION OF DSP-TNYMD  (IX)
       MOVE  SPACE          TO  EDIT-CURSOR OF DSP-TNYMD  (IX)

     END-PERFORM.
*
 INT-DSP-EXIT.
     EXIT.
****************************************************************
*             終了処理                               3.0       *
****************************************************************
 END-SEC               SECTION.
*ファイル ＣＬＯＳＥ
     CLOSE  NFJOHOF.
     CLOSE  NFJOHOF2.
     CLOSE  NFJOHOF3.
     CLOSE  SAKUBAF.
     CLOSE  HTENMS.
     CLOSE  HSHOTBL.
     CLOSE  NFMEIMS.
     CLOSE  HTOKMS.
     CLOSE  ZAMZAIF.
     CLOSE  SHTDENF.
     CLOSE  DSPFILE.
**
 END-EXIT.
     EXIT.
*****************<<  SSY3757I   END PROGRAM  >>******************
