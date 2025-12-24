****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　受領返品取込機能構築　　　　　　　*
*    業務名　　　　　　　：　受領返品計上　　　　　　　　　　　*
*    モジュール名　　　　：　受領返品データ承認入力　　　　　　*
*    作成日　　　　　　　：　2022/07/06                        *
*    作成者　　　　　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　受領返品計上伝票データを一覧照会　*
*                            する。　　　　　　　　　　　　　  *
*                            明細選択欄でＹを指定した場合は、  *
*                            伝票入力画面に遷移する。　　　　  *
*                            計上欄でＹを指定した明細は、計上  *
*                            対象として更新する。　　　　　　  *
*    変更日　　　　　　　：　                                  *
*    変更内容　　　　　　：                                    *
*    　　　　　　　　　　：                                    *
****************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SJR0480I.
*                 流用:SJR0290I.
 AUTHOR.               NAV.
 DATE-WRITTEN.         2022/07/06.
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
       ASSIGN    TO        GS-DSPF
       FORMAT              DSP-FMT
       GROUP               DSP-GRP
       PROCESSING          DSP-PRO
       FUNCTION            DSP-FNC
       FILE      STATUS    DSP-ST.

*返品累積データ
     SELECT  COMRHEF
       ASSIGN    TO    COMRHEL1
       ORGANIZATION    INDEXED
       ACCESS    MODE  DYNAMIC
       RECORD    KEY
         RHE-F01  *> 取引先ＣＤ
         RHE-F03  *> 検収日
         RHE-F02  *> 店舗ＣＤＤ
         RHE-F04  *> 伝票番号
         RHE-F05  *> 行番号
       FILE STATUS     RHE-ST.

*取引先マスタ
     SELECT  HTOKMS
       ASSIGN    TO    TOKMS2
       ORGANIZATION    INDEXED
       ACCESS    MODE  RANDOM
       RECORD    KEY
         TOK-F01 *> 相手取引先コード
       FILE STATUS     TOK-ST.

*店舗マスタ
     SELECT  HTENMS
       ASSIGN    TO    TENMS1
       ORGANIZATION    INDEXED
       ACCESS    MODE  RANDOM
       RECORD    KEY
         TEN-F52  *> 取引先ＣＤ
         TEN-F011 *> 店舗ＣＤ
       FILE STATUS     TEN-ST.
*伝票区分変換マスタ
     SELECT  DENHENF
       ASSIGN    TO    DENHENL1
       ORGANIZATION    INDEXED
       ACCESS    MODE  RANDOM
       RECORD    KEY
         HEN-F01  *> 取引先ＣＤ
         HEN-F02  *> 相手伝票区分
       FILE STATUS     TEN-ST.
*条件ファイル
     SELECT   JYOKEN1   ASSIGN    TO        DA-01-VI-JYOKEN1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       JYO-F01   JYO-F02
                        FILE      STATUS    JYO-ST.
*返品累積一覧ワーク
     SELECT  COMRWKF
       ASSIGN    TO    COMRWKL1
       ORGANIZATION    INDEXED
       ACCESS    MODE  DYNAMIC
       RECORD    KEY
         RWK-F01  *> 取引先ＣＤ
         RWK-F02  *> 店舗ＣＤ
         RWK-F03  *> 検収日
         RWK-F07  *> 伝票番号
       FILE STATUS     RWK-ST.

****************************************************************
 DATA                DIVISION.
****************************************************************
 FILE                SECTION.
****************************************************************
*    FILE = 画面ファイル                                       *
****************************************************************
 FD  DSPFILE
     LABEL     RECORD    IS   OMITTED.
     COPY      FJR04801  OF   XMDLIB
     JOINING   DSP       AS   PREFIX.

****************************************************************
*    FILE = 返品累積データ　　　　                             *
****************************************************************
 FD  COMRHEF
     LABEL     RECORD    IS   STANDARD.
     COPY      COMRHEF   OF   XFDLIB
     JOINING   RHE       AS   PREFIX.
****************************************************************
*    FILE = 取引先マスタ
****************************************************************
 FD  HTOKMS
     LABEL     RECORD    IS   STANDARD.
     COPY      HTOKMS    OF   XFDLIB
     JOINING   TOK       AS   PREFIX.
****************************************************************
*    FILE = 店舗マスタ　                                       *
****************************************************************
 FD  HTENMS
     LABEL     RECORD    IS   STANDARD.
     COPY      HTENMS    OF   XFDLIB
     JOINING   TEN       AS   PREFIX.
****************************************************************
*    FILE = 伝票区分変換マスタ　　                             *
****************************************************************
 FD  DENHENF
     LABEL     RECORD    IS   STANDARD.
     COPY      DENHENF   OF   XFDLIB
     JOINING   HEN       AS   PREFIX.
****************************************************************
*    FILE = 条件ファイル　　　　　                             *
****************************************************************
 FD  JYOKEN1
     LABEL     RECORD    IS   STANDARD.
     COPY      JYOKEN1   OF   XFDLIB
     JOINING   JYO       AS   PREFIX.
****************************************************************
*    FILE = 返品累積一覧ワーク　　                             *
****************************************************************
 FD  COMRWKF
     LABEL     RECORD    IS   STANDARD.
     COPY      COMRWKF   OF   XFDLIB
     JOINING   RWK       AS   PREFIX.
****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
*ステータス領域
 01  STATUS-AREA.
     03  DSP-ST             PIC  X(02).
     03  RHE-ST             PIC  X(02).
     03  TOK-ST             PIC  X(02).
     03  TEN-ST             PIC  X(02).
     03  HEN-ST             PIC  X(02).
     03  JYO-ST             PIC  X(02).
     03  RWK-ST             PIC  X(02).
*画面制御用領域
 01  DSP-CONTROL.
     03  DSP-FMT            PIC  X(08).
     03  DSP-GRP            PIC  X(08).
     03  DSP-PRO            PIC  X(02).
     03  DSP-FNC            PIC  X(04).
*フラグ領域
 01  FLG-AREA.
     03  READ-FLG           PIC  9(01)  VALUE  ZERO.
     03  ERR-FLG            PIC  9(02)  VALUE  ZERO.
     03  END-FLG            PIC  X(03)  VALUE  SPACE.
     03  FG-NARKANF-END     PIC  X(03)  VALUE  SPACE.
     03  JYOKEN1-INV-FLG    PIC  X(03)  VALUE  SPACE.
     03  FG-HTOKMS-INV      PIC  9(01)  VALUE  ZERO.
     03  FG-HTENMS-INV      PIC  9(01)  VALUE  ZERO.
     03  FG-DENHENF-INV     PIC  9(01)  VALUE  ZERO.
     03  FG-TBL-MAXOVER     PIC  9(01).
     03  P-CNT              PIC  9(03)  VALUE  ZERO.
     03  MAX-PAGE           PIC  9(03)  VALUE  ZERO.
     03  TAISYO-CNT         PIC  9(07)  VALUE  ZERO.
     03  TAISYO-MEISAI      PIC  9(07)  VALUE  ZERO.
     03  PAGE-AMARI         PIC  9(01)  VALUE  ZERO.
     03  OPEN-FLG           PIC  9(01)  VALUE  ZERO.
     03  COM-END            PIC  X(03)  VALUE  SPACE.
     03  WK-KEIJYO-KBN      PIC  X(01)  VALUE  SPACE.
     03  COMRHEF-UPD-CNT    PIC  9(07)  VALUE  ZERO.
*
*ワーク領域
 01  WRK-AREA.
***  プログラムスイッチ（画面遷移制御）
     03  PSW                PIC  X(01)  VALUE  SPACE.
***  モード退避
     03  SAV-SHORI          PIC  9(01)  VALUE  ZERO.
     03  WK-SEL-KANRNO      PIC  9(08)  VALUE  ZERO.
     03  WK-RD-KANRNO       PIC  9(08)  VALUE  ZERO.

     03  WK-JNOHNBI-ST      PIC  9(08).
     03  WK-JNOHNBI-ED      PIC  9(08).

     03  WK-YMD             PIC  9(08).
     03  WK-YMDR  REDEFINES WK-YMD.
       05  WK-YMD-Y         PIC  9(04).
       05  WK-YMD-M         PIC  9(02).
       05  WK-YMD-D         PIC  9(02).

     03  WK-CMPYMD.
       05  WK-CMPYMD-Y      PIC S9(04).
       05  WK-CMPYMD-M      PIC S9(02).
       05  WK-CMPYMD-D      PIC S9(02).

     03  WK-SYO             PIC  9(06).
     03  WK-AMARI           PIC  9(06).
     03  WK-MATUBI          PIC  9(02).

     03  WK-BATNO.
       05  WK-BATNO-YMD     PIC  9(08).
       05  WK-BATNO-TIME    PIC  9(04).
       05  WK-BATNO-TORCD   PIC  9(08).
     03  CT-SEQ             PIC  9(03).
     03  CT-PAGE            PIC  9(04).
     03  IX                 PIC  9(04).
     03  IX-TBL             PIC  9(04).
     03  IX-GYO             PIC  9(03).
     03  CT-SEL             PIC  9(03).
     03  FG-SEL             PIC  9(01).
     03  KJ-SEL             PIC  9(03).
     03  WK-SELGYO          PIC  9(02).
     03  MEISAI-GYO         PIC  9(03).

***  エラーセクション名
 01  SEC-NAME.
     03  FILLER             PIC  X(05)     VALUE " *** ".
     03  S-NAME             PIC  X(30).
*
*システム日付／時刻
 01  TIME-AREA.
     03  WK-TIME            PIC  9(08)  VALUE  ZERO.
 01  DATE-AREA.
     03  WK-DATE            PIC  9(06)  VALUE  ZERO.
     03  SYS-DATE           PIC  9(08)  VALUE  ZERO.
*
*受信時間チェック
 01  WK-JIKAN.
     03  WK-HH              PIC   9(02)  VALUE  ZERO.
     03  WK-MM              PIC   9(02)  VALUE  ZERO.
*
*日付論理チェック
 01  WK-CHKDATE.
     03  WK-CHKDATE-YYYY    PIC   9(04)  VALUE  ZERO.
     03  WK-CHKDATE-MM      PIC   9(02)  VALUE  ZERO.
     03  WK-CHKDATE-DD      PIC   9(02)  VALUE  ZERO.
*日付ワーク
 01  SYS-DATEA          PIC  9(06).
 01  FILLER             REDEFINES      SYS-DATEA.
     03  SYS-YY         PIC  9(02).
     03  SYS-MM         PIC  9(02).
     03  SYS-DD         PIC  9(02).
 01  SYS-DATEW          PIC  9(08).
 01  FILLER             REDEFINES      SYS-DATEW.
     03  SYS-YYW        PIC  9(04).
     03  SYS-MMW        PIC  9(02).
     03  SYS-DDW        PIC  9(02).
 01  WK-SYS-DATE             PIC  9(08).
 01  FILLER                  REDEFINES   WK-SYS-DATE.
     03  WK-SYS-YY           PIC  9(04).
     03  WK-SYS-MM           PIC  9(02).
     03  WK-SYS-DD           PIC  9(02).
*
 01  GETUDO                  PIC  9(02).
 01  GETUDO2                 PIC S9(02).
 01  GETUDO2-YMD.
     03  GETUDO2-YY          PIC  9(04).
     03  GETUDO2-MM          PIC  9(02).
     03  GETUDO2-DD          PIC  9(02).
 01  GETUDO2-YMDR      REDEFINES  GETUDO2-YMD.
     03  GETUDO2-YYMMR       PIC  9(06).
     03  FILLER              PIC  X(02).
 01  ACOS-DATE               PIC  9(08).
 01  FILLER                  REDEFINES      ACOS-DATE.
     03  ACOS-YYMM           PIC  9(06).
     03  ACOS-DD             PIC  9(02).
 01  START-YYMM              PIC  9(06).
 01  FILLER                  REDEFINES      START-YYMM.
     03  START-YY            PIC  9(04).
     03  START-MM            PIC  9(02).
 01  END-YYMM                PIC  9(06).
 01  FILLER                  REDEFINES      END-YYMM.
     03  END-YY              PIC  9(04).
     03  END-MM              PIC  9(02).
 01  CHK-DATE                PIC  9(08).
 01  FILLER                  REDEFINES      CHK-DATE.
     03  CHK-YYMM            PIC  9(06).
     03  FILLER              REDEFINES      CHK-YYMM.
         05  CHK-YY          PIC  9(04).
         05  CHK-MM          PIC  9(02).
     03  CHK-DD              PIC  9(02).
 01  SAV-NOU-DATE2           PIC  9(08)    VALUE ZERO.
 01  SAV-NOU-DATE.
     03  SAV-NOU-YYMM        PIC  9(06).
     03  SAV-NOU-DD          PIC  9(02).
 01  SAV-HNOU-DATE2          PIC  9(08)    VALUE ZERO.
 01  SAV-HNOU-DATE.
     03  SAV-HNOU-YYMM       PIC  9(06).
     03  SAV-HNOU-DD         PIC  9(02).
 01  SAV-HTENCD              PIC  9(05)    VALUE ZERO.
 01  SAV-CYU-DATE            PIC  9(08)    VALUE ZERO.
 01  SAV-SYU-DATE            PIC  9(08)    VALUE ZERO.
 01  SAV-ZENGETU             PIC  9(08).
 01  SAV-ACOS-YYMM.
     03  SAV-ACOS-YY         PIC  9(04).
     03  SAV-ACOS-MM         PIC  9(02).
 01  SAV-ACOS-MMS            PIC  9(02).
***
 01  CHK-DATE-WORK.
     03  CHK-01              PIC  9(04).
     03  CHK-02              PIC  9(02).
     03  MATUBI              PIC  X(24)  VALUE
         "312831303130313130313031".
     03  FILLER              REDEFINES   MATUBI.
         05  WK-MATUBI       PIC  9(02)  OCCURS  12.
*
 01  INDEXES.
     03  I                   PIC  9(02).
     03  J                   PIC  9(02).
     03  K                   PIC  9(02).
     03  L                   PIC  9(02).
     03  T                   PIC  9(02).
     03  IL                  PIC  9(02).
*
*日付表示編集
 01  WK-HEN-DATE.
     03  WK-HEN-YYYY        PIC   9(04).
     03  WK-HEN-KU1         PIC   X(01).
     03  WK-HEN-MM          PIC   9(02).
     03  WK-HEN-KU2         PIC   X(01).
     03  WK-HEN-DD          PIC   9(02).
*サカタ商品ＣＤ編集
 01  WK-SKT-HEN.
     03  WK-SKT-SYOCD       PIC   X(08).
     03  WK-SKT-KU1         PIC   X(01).
     03  WK-SKT-HINTAN1     PIC   X(05).
     03  WK-SKT-KU2         PIC   X(01).
     03  WK-SKT-HINTAN2     PIC   X(02).
     03  WK-SKT-KU3         PIC   X(01).
     03  WK-SKT-HINTAN3     PIC   X(01).
*受領返品確認入力用パラメタ
 01  NYURYOKU-PARA.
     03  LINK-JIKKBN        PIC   X(01).
     03  LINK-BUMON         PIC   X(04).
     03  LINK-TANCD         PIC   X(02).
     03  LINK-TOKCD         PIC   9(08).
     03  LINK-TENCD         PIC   9(05).
     03  LINK-KENDT         PIC   9(08).
     03  LINK-DENNO         PIC   9(09).
     03  LINK-CHGKDT        PIC   9(08).
*
*ＰＦガイド
 01  PF-MSG-AREA.
     03  PF-MSG1.
       05  FILLER           PIC  N(30)
       VALUE NC"_取消　_終了".
     03  PF-MSG2.
       05  FILLER           PIC  N(30)
       VALUE NC"_取消　_終了　_項目戻り".
     03  PF-MSG3.
       05  FILLER           PIC  N(30)
*-     VALUE NC"_取消　_終了　_前頁　_次頁".
       VALUE NC"_取消　_終了　_項目戻り　_前頁　_次頁".
 01  PF-MSG-AREA-R  REDEFINES PF-MSG-AREA.
     03  PF-MSG-R           PIC  N(30)  OCCURS 3.

*メッセージの取得
*A---B-------2---------3---------4---------5---------6---------7E*
 01  ERR-MSG-AREA.
     03  ERR-MSG1.
         05  FILLER         PIC  N(30)  VALUE
         NC"無効キーです。".
     03  ERR-MSG2.
         05  FILLER         PIC  N(30)  VALUE
         NC"取引先コードが未入力です。入力して下さい。".
     03  ERR-MSG3.
         05  FILLER         PIC  N(30)  VALUE
*A---B-------2---------3---------4---------5---------6---------7E*
           NC"入力された取引先コードは取引先マスタに存在しません
-           "。".
     03  ERR-MSG4.
         05  FILLER         PIC  N(30)  VALUE
         NC"開始が終了を超えています。".
     03  ERR-MSG5.
         05  FILLER         PIC  N(30)  VALUE
         NC"対象データが存在しません。".
     03  ERR-MSG6.
         05  FILLER         PIC  N(30)  VALUE
         NC"前頁はありません。".
     03  ERR-MSG7.
         05  FILLER         PIC  N(30)  VALUE
*A---B-------2---------3---------4---------5---------6---------7E*
         NC"次頁はありません。".
     03  ERR-MSG8.
         05  FILLER         PIC  N(30)  VALUE
         NC"複数Ｙ選択されています。１件だけ選択して下さい。".
     03  ERR-MSG9.
         05  FILLER         PIC  N(30)  VALUE
*-       NC"選択は、”Ｙ”のみ入力可能です。".
         NC"”Ｙ”のみ入力可能です。".
     03  ERR-MSG10.
         05  FILLER         PIC  N(30)  VALUE
*A---B-------2---------3---------4---------5---------6---------7E*
         NC"データはありますが、これ以上のスクロールはできません
-          "。".
     03  ERR-MSG11.
*A---B-------2---------3---------4---------5---------6---------7E
         05  FILLER         PIC  N(30)  VALUE
         NC"１件も選択されていません、照会する行をＹで選択して下
-         "さい。".
     03  ERR-MSG12.
         05  FILLER         PIC  N(30)  VALUE
         NC"複数Ｙ選択されています。１件だけ選択して下さい。".
     03  ERR-MSG13.
         05  FILLER         PIC  N(30)  VALUE
*A---B-------2---------3---------4---------5---------6---------7E*
         NC"指定条件に誤りがないか確認し、ＥＮＴＥＲを押下して下
-          "さい。".
     03  ERR-MSG14.
*A---B-------2---------3---------4---------5---------6---------7E*
         05  FILLER         PIC  N(30)  VALUE
         NC"伝票纏め対象の取引先です。別メニューで実行して下さい
-          "。".
     03  ERR-MSG15.
*A---B-------2---------3---------4---------5---------6---------7E*
         05  FILLER         PIC  N(30)  VALUE
         NC"伝票纏め対象の取引先ではありません。通常版で実行して
-          "下さい。".
     03  ERR-MSG16.
         05  FILLER         PIC  N(30)  VALUE
         NC"表示する計上区分指定が間違えています。".
     03  ERR-MSG17.
         05  FILLER         PIC  N(30)  VALUE
         NC"正しい日付を入力してください。".
     03  ERR-MSG18.
         05  FILLER         PIC  N(30)  VALUE
         NC"更新する場合はＹを入力しＥＮＴＥＲです。".
     03  ERR-MSG19.
         05  FILLER         PIC  N(30)  VALUE
         NC"　".
     03  ERR-MSG20.
         05  FILLER         PIC  N(30)  VALUE
         NC"　".
 01  ERR-MSG-AREA-R  REDEFINES ERR-MSG-AREA.
     03  ERR-MSG-R          PIC  N(30)  OCCURS 20.
*
 01  FILE-ERR.
     03  DSP-ERR           PIC  N(20)  VALUE
         NC"画面ファイルエラー".
     03  RHE-ERR           PIC  N(20)  VALUE
         NC"返品累積データエラー".
     03  TOK-ERR           PIC  N(20)  VALUE
         NC"取引先マスタエラー".
     03  TEN-ERR           PIC  N(20)  VALUE
         NC"店舗マスタエラー".
     03  HEN-ERR           PIC  N(20)  VALUE
         NC"伝票区分変換マスタエラー".
     03  RWK-ERR           PIC  N(20)  VALUE
         NC"返品累積一覧ワークエラー".

*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
*
 COPY      FJR04801  OF   XMDLIB
 JOINING   WK        AS   PREFIX.
****************************************************************
 LINKAGE               SECTION.
****************************************************************
* 入力パラメータ
 01  PARA-JIKKBN            PIC  X(01).
 01  PARA-BUMON             PIC  X(04).
 01  PARA-TANCD             PIC  X(02).
 01  PARA-MATOME            PIC  X(01).
*
**************************************************************
 PROCEDURE             DIVISION
                           USING  PARA-JIKKBN
                                  PARA-BUMON
                                  PARA-TANCD
                                  PARA-MATOME.
**************************************************************
 DECLARATIVES.
 DSP-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE DSPFILE.
     DISPLAY     DSP-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     DSP-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 RHE-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE COMRHEF.
     DISPLAY     RHE-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     RHE-ST    UPON      CONS.
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
 HEN-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE DENHENF.
     DISPLAY     HEN-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     HEN-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 RWK-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE COMRWKF.
     DISPLAY     RWK-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     RWK-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 END  DECLARATIVES.
****************************************************************
*             MAIN        MODULE                     0.0       *
****************************************************************
 PROCESS-START         SECTION.
     MOVE  "PROCESS START"       TO  S-NAME.

     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC  UNTIL END-FLG = "END".
     PERFORM  END-SEC.
     DISPLAY NC"計上対象　更新件数＝" COMRHEF-UPD-CNT UPON CONS.

     STOP RUN.
*
*****EXIT PROGRAM.
 CONTROL-EXIT.
     EXIT.
****************************************************************
*             初期処理                               1.0
****************************************************************
 INIT-SEC              SECTION.
     MOVE  "INIT-SEC"       TO  S-NAME.
     PERFORM  SDATE-GET-SEC.
*ファイルのＯＰＥＮ
     OPEN  I-O   DSPFILE.
     OPEN  INPUT COMRHEF
     OPEN  INPUT HTOKMS.
     OPEN  INPUT HTENMS.
     OPEN  INPUT DENHENF.
     OPEN  INPUT JYOKEN1.
     OPEN  INPUT COMRWKF.
*ワークの初期化
     INITIALIZE  FLG-AREA.
     MOVE  ZERO             TO  PSW.
*条件ファイルより日付取得
*　（経理月）
     MOVE     "58"           TO   JYO-F01.
     MOVE     SPACE          TO   JYO-F02.
     PERFORM  JYOKEN1-READ-SEC.
     IF       JYOKEN1-INV-FLG  =  "INV"
              DISPLAY   "HJYOKEN INV KEY=58"  UPON CONS
              MOVE      "END"     TO   END-FLG
              GO   TO   INIT-EXIT
     END-IF.
     MOVE     JYO-F04        TO   GETUDO.
     MOVE     JYO-F04        TO   GETUDO2.
*TEST↓
*    DISPLAY "-----------------------------" UPON CONS.
*    DISPLAY NC"取得：条件Ｆ（経理月）＝"  GETUDO   UPON CONS.
*    DISPLAY "-----------------------------" UPON CONS.
*TEST↑
*　（ＡＣＯＳ用締日）
     MOVE     "99"           TO   JYO-F01.
     MOVE     SPACE          TO   JYO-F02.
     PERFORM  JYOKEN1-READ-SEC.
     IF       JYOKEN1-INV-FLG  =  "INV"
              DISPLAY   "HJYOKEN INV KEY=99"  UPON CONS
              MOVE      "END"     TO   END-FLG
              GO   TO   INIT-EXIT
     END-IF.
     EVALUATE GETUDO
         WHEN 1
              MOVE      JYO-F04        TO   ACOS-DATE
         WHEN 2
              MOVE      JYO-F05        TO   ACOS-DATE
         WHEN 3
              MOVE      JYO-F06        TO   ACOS-DATE
         WHEN 4
              MOVE      JYO-F07        TO   ACOS-DATE
         WHEN 5
              MOVE      JYO-F08        TO   ACOS-DATE
         WHEN 6
              MOVE      JYO-F09        TO   ACOS-DATE
         WHEN 7
              MOVE      JYO-F10        TO   ACOS-DATE
         WHEN 8
              MOVE      JYO-F11        TO   ACOS-DATE
         WHEN 9
              MOVE      JYO-F12        TO   ACOS-DATE
         WHEN 10
              MOVE      JYO-F12A       TO   ACOS-DATE
         WHEN 11
              MOVE      JYO-F12B       TO   ACOS-DATE
         WHEN 12
              MOVE      JYO-F12C       TO   ACOS-DATE
         WHEN OTHER
              DISPLAY  "### ｹﾞﾂﾄﾞ ｲｼﾞｮｳ  ｹﾞﾂﾄﾞ= " GETUDO " ###"
                                       UPON CONS
              MOVE      "END"     TO   END-FLG
              GO   TO   INIT-EXIT
     END-EVALUATE.
*TEST↓
*    DISPLAY "-------------------------------------" UPON CONS.
*    DISPLAY NC"条件Ｆ（ＡＣＯＳ締日）＝"  ACOS-DATE UPON CONS.
*    DISPLAY "-------------------------------------" UPON CONS.
*TEST↑
*
     MOVE     ACOS-DATE      TO   GETUDO2-YMD.
     MOVE     99             TO   GETUDO2-DD.
     IF       GETUDO2        =    12
              COMPUTE        GETUDO2-YY     =
                             GETUDO2-YY     -        1
              MOVE     12    TO             GETUDO2-MM
     ELSE
              MOVE      GETUDO2   TO        GETUDO2-MM
     END-IF.
*
     MOVE     SYS-YY         TO   START-YY
                                  END-YY.
     MOVE     SYS-MM         TO   START-MM
                                  END-MM.
     ADD      1              TO   END-MM.
     IF       END-MM  >  12
              MOVE      1         TO   END-MM
              COMPUTE   END-YY   =  WK-SYS-YY  +  1
     ELSE
              MOVE      WK-SYS-YY   TO      END-YY
     END-IF.
     SUBTRACT 1              FROM START-MM.
     IF       START-MM  <  1
              MOVE      12        TO   START-MM
              COMPUTE   START-YY  =  WK-SYS-YY  -  1
     ELSE
              MOVE      WK-SYS-YY      TO   START-YY
     END-IF.
*
     MOVE     GETUDO2-YYMMR  TO      START-YYMM.
*
*TEST↓
*    DISPLAY "------------------------------" UPON CONS.
*    DISPLAY NC"算出：開始日付＝"  START-YYMM UPON CONS.
*    DISPLAY NC"算出：終了日付＝"  END-YYMM   UPON CONS.
*    DISPLAY "------------------------------" UPON CONS.
*TEST↑
*
*初期画面の表示
     MOVE  SPACE            TO  DSP-PRO.
     PERFORM  INIT-DSP-SEC.
*ヘッド入力へ
     MOVE  "1"              TO  PSW.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*             システム日付取得
****************************************************************
 SDATE-GET-SEC              SECTION.
*システム日付・時刻の取得
     ACCEPT  WK-DATE   FROM DATE.
     ACCEPT  SYS-DATEA FROM DATE.
     MOVE  "3"              TO  LINK-IN-KBN.
     MOVE  WK-DATE          TO  LINK-IN-YMD6.
     MOVE  ZERO             TO  LINK-IN-YMD8.
     MOVE  ZERO             TO  LINK-OUT-RET.
     MOVE  ZERO             TO  LINK-OUT-YMD.
     CALL  "SKYDTCKB"  USING LINK-IN-KBN
                             LINK-IN-YMD6
                             LINK-IN-YMD8
                             LINK-OUT-RET
                             LINK-OUT-YMD.
     MOVE  LINK-OUT-YMD     TO  SYS-DATE.
     MOVE  LINK-OUT-YMD     TO  SYS-DATEW.
     MOVE  LINK-OUT-YMD     TO  WK-SYS-DATE.
     ACCEPT  WK-TIME   FROM TIME.
 SDATE-GET-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    条件ファイル　読込
*--------------------------------------------------------------*
 JYOKEN1-READ-SEC       SECTION.
*
     READ  JYOKEN1
           INVALID
           MOVE  "INV"  TO   JYOKEN1-INV-FLG
           NOT  INVALID
           MOVE  SPACE  TO   JYOKEN1-INV-FLG
     END-READ.
*
 JYOKEN1-READ-EXIT.
     EXIT.
****************************************************************
*             メイン処理                             2.0
****************************************************************
 MAIN-SEC              SECTION.
     MOVE  "MAIN-SEC"       TO  S-NAME.

     EVALUATE  PSW
*    ヘッダ入力
       WHEN  "1"  PERFORM  DSP-HEAD-SEC
*    メイン入力
       WHEN  "2"  PERFORM  DSP-BODY-SEC
*    確認入力
       WHEN  "3"  PERFORM  DSP-KAKU-SEC

       WHEN  OTHER  CONTINUE
     END-EVALUATE.

 MAIN-EXIT.
     EXIT.
****************************************************************
*  ヘッド入力処理  ( PSW = 1 )                                 *
****************************************************************
 DSP-HEAD-SEC         SECTION.
     MOVE  "DSP-HEAD-SEC"  TO  S-NAME.

     PERFORM  DSP-WRITE-SEC.
     PERFORM  DSP-READ-SEC.

     EVALUATE  DSP-FNC
*      実行
       WHEN  "E000"
         PERFORM  HEAD-CHK-SEC

*      終了
       WHEN  "F005"
         MOVE  "END"        TO  END-FLG

*      取消
       WHEN  "F004"
         PERFORM  INIT-DSP-SEC

       WHEN  OTHER
         MOVE  1            TO  ERR-FLG
     END-EVALUATE.

 DSP-HEAD-EXIT.
     EXIT.
****************************************************************
*  画面表示処理                                                *
****************************************************************
 DSP-WRITE-SEC         SECTION.
     MOVE  "DSP-WRITE-SEC"  TO  S-NAME.

     PERFORM  SDATE-GET-SEC.
*  システム日付転送
     MOVE  SYS-DATE         TO  DSP-SDATE.
*  システム時間転送
     MOVE  WK-TIME(1:6)     TO  DSP-STIME.
*  エラーメッセージセット
     IF  ERR-FLG = ZERO
         MOVE  SPACE        TO  DSP-MSGSPC
     ELSE
         MOVE  ERR-MSG-R(ERR-FLG) TO  DSP-MSGSPC
         MOVE  ZERO         TO  ERR-FLG
     END-IF.
*  ガイドメッセージの設定（次入力モードに切換後）
     EVALUATE  PSW
*    ヘッダ入力
       WHEN  "1"
         MOVE  PF-MSG-R (1) TO  DSP-FNCSPC
*    メイン入力
       WHEN  "2"
         MOVE  PF-MSG-R (3) TO  DSP-FNCSPC
*    確認入力
       WHEN  "3"
         MOVE  PF-MSG-R (2) TO  DSP-FNCSPC
       WHEN  OTHER
         MOVE  SPACE        TO  DSP-FNCSPC

     END-EVALUATE.
     IF  PARA-JIKKBN = SPACE
         MOVE NC"＜　通　　　常　＞" TO DSP-JIKMSG
     ELSE
         MOVE NC"＜　伝票纏め分　＞" TO DSP-JIKMSG
     END-IF.
     MOVE   P-CNT           TO       DSP-PGCNT.
*  画面の表示
     MOVE  "SCREEN"         TO  DSP-GRP.
     MOVE  "FJR04801"       TO  DSP-FMT.
     WRITE  DSP-FJR04801.

 DSP-WRITE-EXIT.
     EXIT.
****************************************************************
*             初期画面表示                                     *
****************************************************************
 INIT-DSP-SEC          SECTION.
     MOVE  "INIT-DSP-SEC"   TO  S-NAME.

*  項目クリア
     MOVE  SPACE            TO  DSP-FJR04801.
*  ＰＧＩＤ
     MOVE  "SJR0480I"       TO  DSP-PGID.
*  ＦＯＲＭＩＤ
     MOVE  "FJR04801"       TO  DSP-FORMID.
*  システム日付転送
     MOVE  SYS-DATE         TO  DSP-SDATE.
*  システム時間転送
     MOVE  WK-TIME(1:6)     TO  DSP-STIME.
     MOVE  ZERO             TO  DSP-PGCNT.
     MOVE  ZERO             TO  P-CNT.
     MOVE  ZERO             TO  TAISYO-CNT.
     MOVE  ZERO             TO  TAISYO-MEISAI.
*属性初期化
     MOVE  "M"              TO  EDIT-OPTION OF DSP-TOKCD.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-TOKCD.
     MOVE  "M"              TO  EDIT-OPTION OF DSP-KENST.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-KENST.
     MOVE  "M"              TO  EDIT-OPTION OF DSP-KENED.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-KENED.
     MOVE  "M"              TO  EDIT-OPTION OF DSP-TENST.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-TENST.
     MOVE  "M"              TO  EDIT-OPTION OF DSP-TENED.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-TENED.
     MOVE  "M"              TO  EDIT-OPTION OF DSP-DENST.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-DENST.
     MOVE  "M"              TO  EDIT-OPTION OF DSP-DENED.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-DENED.
     MOVE  "M"              TO  EDIT-OPTION OF DSP-CHGKDT.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-CHGKDT.
*-   MOVE  "M"              TO  EDIT-OPTION OF DSP-HDKKBN
*-   MOVE  SPACE            TO  EDIT-CURSOR OF DSP-HDKKBN
*
     PERFORM VARYING IX FROM 1 BY 1  UNTIL  IX  >  7
        MOVE  "M"      TO       EDIT-OPTION OF DSP-MSENTA(IX)
        MOVE  SPACE    TO       EDIT-CURSOR OF DSP-MSENTA(IX)
        MOVE  SPACE    TO       DSP-MSENTA(IX)
        MOVE  "M"      TO       EDIT-OPTION OF DSP-MKEIJ (IX)
        MOVE  SPACE    TO       EDIT-CURSOR OF DSP-MKEIJ (IX)
        MOVE  "M"      TO       EDIT-OPTION OF DSP-MKEIJN(IX)
        MOVE  "M"      TO       EDIT-COLOR  OF DSP-MKEIJN(IX)
     END-PERFORM.

 INT-DSP-EXIT.
     EXIT.
****************************************************************
*  画面読込処理                                                *
****************************************************************
 DSP-READ-SEC          SECTION.
     MOVE  "DSP-READ-SEC"   TO  S-NAME.

     MOVE  "NE"             TO  DSP-PRO.

     EVALUATE  PSW
*    ヘッダ入力
       WHEN  "1"
         MOVE  "HEAD01"     TO  DSP-GRP
*    メイン入力
       WHEN  "2"
         MOVE  "MEIS01"     TO  DSP-GRP
*    確認入力
       WHEN  "3"
         MOVE  "KAKU"       TO  DSP-GRP

     END-EVALUATE.

     MOVE  "FJR04801"       TO  DSP-FMT.
     READ  DSPFILE.
*  入力項目の属性を通常にする
     MOVE  SPACE            TO   DSP-PRO.

 DSP-READ-EXIT.
     EXIT.
****************************************************************
*  ヘッダ入力チェック                                          *
****************************************************************
 HEAD-CHK-SEC          SECTION.
     MOVE  "HEAD-CHK-SEC"  TO  S-NAME.
*属性初期化
     MOVE  "M"              TO  EDIT-OPTION OF DSP-TOKCD.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-TOKCD.
     MOVE  "M"              TO  EDIT-OPTION OF DSP-KENST.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-KENST.
     MOVE  "M"              TO  EDIT-OPTION OF DSP-KENED.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-KENED.
     MOVE  "M"              TO  EDIT-OPTION OF DSP-TENST.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-TENST.
     MOVE  "M"              TO  EDIT-OPTION OF DSP-TENED.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-TENED.
     MOVE  "M"              TO  EDIT-OPTION OF DSP-DENST.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-DENST.
     MOVE  "M"              TO  EDIT-OPTION OF DSP-DENED.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-DENED.
     MOVE  "M"              TO  EDIT-OPTION OF DSP-CHGKDT.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-CHGKDT.
*-   MOVE  "M"              TO  EDIT-OPTION OF DSP-HDKKBN.
*-   MOVE  SPACE            TO  EDIT-CURSOR OF DSP-HDKKBN.
     MOVE  SPACE            TO  DSP-TOKNM.

* 取引先ＣＤ
     IF  DSP-TOKCD  NOT  NUMERIC
     OR  DSP-TOKCD  =    ZERO
         IF  ERR-FLG = ZERO
             MOVE  2        TO  ERR-FLG
         END-IF
         MOVE  "C"          TO  EDIT-CURSOR OF DSP-TOKCD
         MOVE  "R"          TO  EDIT-OPTION OF DSP-TOKCD
         GO TO  HEAD-CHK-EXIT
     ELSE
         MOVE   DSP-TOKCD   TO  TOK-F01
         PERFORM RD-HTOKMS-SEC
         IF  FG-HTOKMS-INV  =  ZERO
             MOVE  TOK-F02  TO  DSP-TOKNM
             IF  PARA-MATOME = SPACE
             AND TOK-FIL1(1:1) = "1"
                 IF  ERR-FLG = ZERO
                     MOVE  14   TO  ERR-FLG
                 END-IF
                 MOVE  "C"      TO  EDIT-CURSOR OF DSP-TOKCD
                 MOVE  "R"      TO  EDIT-OPTION OF DSP-TOKCD
                 GO TO  HEAD-CHK-EXIT
             END-IF
             IF  PARA-MATOME = "1"
             AND TOK-FIL1(1:1) = SPACE
                 IF  ERR-FLG = ZERO
                     MOVE  15   TO  ERR-FLG
                 END-IF
                 MOVE  "C"      TO  EDIT-CURSOR OF DSP-TOKCD
                 MOVE  "R"      TO  EDIT-OPTION OF DSP-TOKCD
                 GO TO  HEAD-CHK-EXIT
             END-IF
         ELSE
             IF  ERR-FLG = ZERO
                 MOVE  3    TO  ERR-FLG
             END-IF
             MOVE  "C"      TO  EDIT-CURSOR OF DSP-TOKCD
             MOVE  "R"      TO  EDIT-OPTION OF DSP-TOKCD
             GO TO  HEAD-CHK-EXIT
         END-IF
     END-IF.
*検収日開始
     IF   DSP-KENST  NOT    NUMERIC
     OR   DSP-KENST      =  ZERO
          MOVE  ZERO        TO  DSP-KENST
     END-IF.
*検収日終了
     IF   DSP-KENED  NOT    NUMERIC
     OR   DSP-KENED      =  ZERO
          MOVE  99999999    TO  DSP-KENED
     END-IF.
*検収日大小チェック
     IF   DSP-KENST  >  DSP-KENED
         IF  ERR-FLG = ZERO
             MOVE  4        TO  ERR-FLG
         END-IF
         MOVE  "C"          TO  EDIT-CURSOR OF DSP-KENST
         MOVE  "R"          TO  EDIT-OPTION OF DSP-KENST
         MOVE  "R"          TO  EDIT-OPTION OF DSP-KENED
         GO TO  HEAD-CHK-EXIT
     END-IF.
*店舗ＣＤ開始
     IF   DSP-TENST  NOT    NUMERIC
     OR   DSP-TENST      =  ZERO
          MOVE  ZERO        TO  DSP-TENST
     END-IF.
*店舗ＣＤ終了
     IF   DSP-TENED  NOT    NUMERIC
     OR   DSP-TENED      =  ZERO
          MOVE  99999       TO  DSP-TENED
     END-IF.
*店舗ＣＤ大小チェック
     IF   DSP-TENST  >  DSP-TENED
         IF  ERR-FLG = ZERO
             MOVE  4        TO  ERR-FLG
         END-IF
         MOVE  "C"          TO  EDIT-CURSOR OF DSP-TENST
         MOVE  "R"          TO  EDIT-OPTION OF DSP-TENST
         MOVE  "R"          TO  EDIT-OPTION OF DSP-TENED
         GO TO  HEAD-CHK-EXIT
     END-IF.
*伝票番号　開始
     IF   DSP-DENST  NOT    NUMERIC
     OR   DSP-DENST      =  ZERO
          MOVE  ZERO        TO  DSP-DENST
     END-IF.
*伝票番号　終了
     IF   DSP-DENED  NOT    NUMERIC
     OR   DSP-DENED      =  ZERO
          MOVE  999999999   TO  DSP-DENED
     END-IF.
*伝票番号　大小チェック
     IF   DSP-DENST  >  DSP-DENED
         IF  ERR-FLG = ZERO
             MOVE  4        TO  ERR-FLG
         END-IF
         MOVE  "C"          TO  EDIT-CURSOR OF DSP-DENST
         MOVE  "R"          TO  EDIT-OPTION OF DSP-DENST
         MOVE  "R"          TO  EDIT-OPTION OF DSP-DENED
         GO TO  HEAD-CHK-EXIT
     END-IF.
*変更計上日
     IF   DSP-CHGKDT NOT    NUMERIC
     OR   DSP-CHGKDT     =  ZERO
          MOVE  ZERO        TO  DSP-CHGKDT
     END-IF.
     IF   DSP-CHGKDT NOT =  ZERO
          MOVE     "2"         TO       LINK-IN-KBN
          MOVE      DSP-CHGKDT TO       LINK-IN-YMD8
          CALL     "SKYDTCKB"  USING    LINK-IN-KBN
                                        LINK-IN-YMD6
                                        LINK-IN-YMD8
                                        LINK-OUT-RET
                                        LINK-OUT-YMD
          IF   LINK-OUT-RET   NOT =     ZERO
               IF        ERR-FLG   =    ZERO
                         MOVE     17    TO   ERR-FLG
               END-IF
               MOVE     "C"        TO   EDIT-CURSOR OF DSP-CHGKDT
               MOVE     "R"        TO   EDIT-OPTION OF DSP-CHGKDT
               GO TO  HEAD-CHK-EXIT
          END-IF
     END-IF.
*   規定範囲外日付
     MOVE     DSP-CHGKDT      TO   CHK-DATE.
*   規定範囲外日付１
*TEST↓
*    DISPLAY "-----------------------------" UPON CONS.
*    DISPLAY NC"規定範囲外日付_"            UPON CONS.
*    DISPLAY NC"許可開始年月＝" START-YYMM   UPON CONS.
*    DISPLAY NC"許可終了年月＝" END-YYMM     UPON CONS.
*    DISPLAY NC"実検収日　　＝" CHK-YYMM     UPON CONS.
*TEST↑
     IF     ( DSP-CHGKDT      NOT  =  0      )  AND
            ( CHK-YYMM    <   START-YYMM     )  OR
            ( CHK-YYMM    >   END-YYMM       )
              IF   ERR-FLG    =  ZERO
                   MOVE       17   TO   ERR-FLG
              END-IF
              MOVE "C"  TO   EDIT-CURSOR OF DSP-CHGKDT
              MOVE "R"  TO   EDIT-OPTION OF DSP-CHGKDT
*TEST↓
*             DISPLAY NC"－－－＞　ＮＧ　"  UPON CONS
*             DISPLAY "-----------------------------" UPON CONS
*TEST↑
              GO        TO    HEAD-CHK-EXIT
*TEST↓
*    ELSE
*             DISPLAY NC"－－－＞　ＯＫ　"  UPON CONS
*             DISPLAY "-----------------------------" UPON CONS
*TEST↑
     END-IF.
*
*   規定範囲外日付２
*TEST↓
*    DISPLAY "-----------------------------" UPON CONS.
*    DISPLAY NC"規定範囲外日付_"            UPON CONS.
*    DISPLAY NC"ＡＣＯＳ締日＝" ACOS-DATE    UPON CONS.
*    DISPLAY NC"システム日付＝" SYS-DATEW    UPON CONS.
*    DISPLAY NC"実検収日年月＝" CHK-YYMM     UPON CONS.
*    DISPLAY NC"ＡＣＯＳ年月＝" ACOS-YYMM    UPON CONS.
*TEST↑
     IF     ( DSP-CHGKDT      NOT  =  0      )  AND
            ( ACOS-DATE       <  SYS-DATEW   )  AND
            ( CHK-YYMM        <  ACOS-YYMM   )
              IF   ERR-FLG    =  ZERO
                   MOVE       17   TO   ERR-FLG
              END-IF
              MOVE "C"  TO   EDIT-CURSOR OF DSP-CHGKDT
              MOVE "R"  TO   EDIT-OPTION OF DSP-CHGKDT
*TEST↓
*             DISPLAY NC"－－－＞　ＮＧ　"  UPON CONS
*             DISPLAY "-----------------------------" UPON CONS
*    ELSE
*             DISPLAY NC"－－－＞　ＯＫ　"  UPON CONS
*             DISPLAY "-----------------------------" UPON CONS
*TEST↑
              GO        TO    HEAD-CHK-EXIT
     END-IF.
*
*表示計上区分チェック
*-   IF   DSP-HDKKBN  =  SPACE OR "1" OR "2" OR "3" OR "4"
*-       CONTINUE
*-   ELSE
*-       IF  ERR-FLG = ZERO
*-           MOVE  16       TO  ERR-FLG
*-       END-IF
*-       MOVE  "C"          TO  EDIT-CURSOR OF DSP-HDKKBN
*-       MOVE  "R"          TO  EDIT-OPTION OF DSP-HDKKBN
*-       GO TO  HEAD-CHK-EXIT
*-   END-IF.
*返品累積データ⇒返品累積一覧ワーク作成
*    ファイルスタート
     MOVE SPACE             TO  RHE-REC.
     INITIALIZE                 RHE-REC.
     MOVE ZERO              TO  OPEN-FLG  READ-FLG.
     MOVE SPACE             TO  COM-END.
*
     MOVE DSP-TOKCD         TO  RHE-F01.
     MOVE DSP-KENST         TO  RHE-F03
*### MOVE DSP-TENST         TO  RHE-F02.
     START  COMRHEF  KEY  IS  > RHE-F01  RHE-F03  RHE-F02
            INVALID
            IF  ERR-FLG = ZERO
                MOVE  5     TO  ERR-FLG
            END-IF
            MOVE  "C"       TO  EDIT-CURSOR OF DSP-TOKCD
            MOVE  "R"       TO  EDIT-OPTION OF DSP-TOKCD
            MOVE  "R"       TO  EDIT-OPTION OF DSP-KENST
            MOVE  "R"       TO  EDIT-OPTION OF DSP-KENED
            MOVE  "R"       TO  EDIT-OPTION OF DSP-TENST
            MOVE  "R"       TO  EDIT-OPTION OF DSP-TENED
            MOVE  "R"       TO  EDIT-OPTION OF DSP-DENST
            MOVE  "R"       TO  EDIT-OPTION OF DSP-DENED
            GO TO  HEAD-CHK-EXIT
     END-START.
     MOVE   ZERO            TO  TAISYO-CNT.
*    返品累積データ読込
     PERFORM  COMRHEF-READ-SEC.
     IF     TAISYO-CNT  =  ZERO
            IF  ERR-FLG = ZERO
                MOVE  5     TO  ERR-FLG
            END-IF
            MOVE  "C"       TO  EDIT-CURSOR OF DSP-TOKCD
            MOVE  "R"       TO  EDIT-OPTION OF DSP-TOKCD
            MOVE  "R"       TO  EDIT-OPTION OF DSP-KENST
            MOVE  "R"       TO  EDIT-OPTION OF DSP-KENED
            MOVE  "R"       TO  EDIT-OPTION OF DSP-TENST
            MOVE  "R"       TO  EDIT-OPTION OF DSP-TENED
            MOVE  "R"       TO  EDIT-OPTION OF DSP-DENST
            MOVE  "R"       TO  EDIT-OPTION OF DSP-DENED
*-          MOVE  "R"       TO  EDIT-OPTION OF DSP-HDKKBN
            GO TO  HEAD-CHK-EXIT
     END-IF.
*    ファイルクローズ
     CLOSE         COMRWKF.
*    ファイル初期化
     OPEN  OUTPUT  COMRWKF.
     CLOSE         COMRWKF.
*    ワーク作成ＯＰＥＮ
     OPEN  I-O     COMRWKF.
     MOVE          ZERO     TO  MEISAI-GYO.
     MOVE          ZERO     TO  TAISYO-CNT TAISYO-MEISAI.
     PERFORM  MEISAI-WT-SEC  UNTIL  COM-END = "END".
     IF     TAISYO-CNT  =  ZERO
            IF  ERR-FLG = ZERO
                MOVE  5     TO  ERR-FLG
            END-IF
            MOVE  "C"       TO  EDIT-CURSOR OF DSP-TOKCD
            MOVE  "R"       TO  EDIT-OPTION OF DSP-TOKCD
            MOVE  "R"       TO  EDIT-OPTION OF DSP-KENST
            MOVE  "R"       TO  EDIT-OPTION OF DSP-KENED
            MOVE  "R"       TO  EDIT-OPTION OF DSP-TENST
            MOVE  "R"       TO  EDIT-OPTION OF DSP-TENED
            MOVE  "R"       TO  EDIT-OPTION OF DSP-DENST
            MOVE  "R"       TO  EDIT-OPTION OF DSP-DENED
*-          MOVE  "R"       TO  EDIT-OPTION OF DSP-HDKKBN
            GO TO  HEAD-CHK-EXIT
     END-IF.
*ＭＡＸ頁算出
     DIVIDE   TAISYO-MEISAI BY   7
              GIVING    MAX-PAGE  REMAINDER      PAGE-AMARI.
     IF  PAGE-AMARI  >  ZERO
         ADD  1          TO  MAX-PAGE
     END-IF.
*
     DISPLAY NC"＃ＭＡＸ頁" " = " MAX-PAGE  UPON CONS.
     MOVE    MAX-PAGE    TO  DSP-MXCNT.
*明細１頁目セット
     CLOSE        COMRWKF.
     OPEN  INPUT  COMRWKF.
     MOVE  1             TO  OPEN-FLG.
     PERFORM MEISAI-SET-SEC
             VARYING  IX  FROM  1  BY  1  UNTIL  IX  >   7.
     MOVE  1             TO  P-CNT.
*
     MOVE  2             TO  PSW.

 HEAD-CHK-EXIT.
     EXIT.
****************************************************************
*    返品累積データ読込                                        *
****************************************************************
 COMRHEF-READ-SEC       SECTION.
*
     MOVE  "COMRHEF-READ-SEC" TO  S-NAME.
*
     READ  COMRHEF NEXT
           AT  END
           MOVE  "END"           TO  COM-END
           GO                    TO  COMRHEF-READ-EXIT
     END-READ.
 CHK010.
*取引先ＣＤチェック
     IF  DSP-TOKCD  NOT =  RHE-F01
           MOVE  "END"           TO  COM-END
           GO                    TO  COMRHEF-READ-EXIT
     END-IF.
 CHK020.
*検収日チェック
     IF  DSP-KENST  <=  RHE-F03
     AND DSP-KENED  >=  RHE-F03
         CONTINUE
     ELSE
         MOVE  "END"             TO  COM-END
         GO                      TO  COMRHEF-READ-EXIT
     END-IF.
 CHK030.
*店舗ＣＤチェック
     IF  DSP-TENST  <=  RHE-F02
     AND DSP-TENED  >=  RHE-F02
         CONTINUE
     ELSE
         GO                      TO  COMRHEF-READ-SEC
     END-IF.
 CHK040.
*伝票番号チェック
     IF  DSP-DENST  <=  RHE-F04
     AND DSP-DENED  >=  RHE-F04
         CONTINUE
     ELSE
         GO                      TO   COMRHEF-READ-SEC
     END-IF.
 CHK040.
*計上区分チェック  未確認のみ対象
     IF  RHE-F80  =  " "
         CONTINUE
     ELSE
         GO                      TO   COMRHEF-READ-SEC
     END-IF.
*
 CHK050.
     EVALUATE  RHE-F80
         WHEN  SPACE
               MOVE  "1"         TO   WK-KEIJYO-KBN
         WHEN  "1"
               IF  RHE-F86 = ZERO
                   MOVE "2"      TO   WK-KEIJYO-KBN
               ELSE
                   MOVE "3"      TO   WK-KEIJYO-KBN
               END-IF
         WHEN  "9"
               MOVE  "4"         TO   WK-KEIJYO-KBN
         WHEN  OTHER
               MOVE   SPACE      TO   WK-KEIJYO-KBN
     END-EVALUATE.
 CHK060.
*表示計上区分チェック＝空白の時は全てが対象
*-   IF  DSP-HDKKBN  =  SPACE
*-       GO                      TO   COMRHEF-READ-010
*-   END-IF.
 CHK070.
*表示計上区分指定の場合
*    EVALUATE  DSP-HDKKBN
*        WHEN  "1"    *>未確認
*              IF  WK-KEIJYO-KBN  NOT = "1"
*                  GO            TO   COMRHEF-READ-SEC
*              END-IF
*        WHEN  "2"    *>未計上
*              IF  WK-KEIJYO-KBN  NOT = "2"
*                  GO            TO   COMRHEF-READ-SEC
*              END-IF
*        WHEN  "3"    *>計上済
*              IF  WK-KEIJYO-KBN  NOT = "3"
*                  GO            TO   COMRHEF-READ-SEC
*              END-IF
*        WHEN  "4"    *>取消
*              IF  WK-KEIJYO-KBN  NOT = "4"
*                  GO            TO   COMRHEF-READ-SEC
*              END-IF
*    END-EVALUATE.
     IF  WK-KEIJYO-KBN  NOT = "1"
         GO            TO   COMRHEF-READ-SEC
     END-IF.
*
 COMRHEF-READ-010.
     ADD     1                   TO   TAISYO-CNT.
*
 COMRHEF-READ-EXIT.
     EXIT.

****************************************************************
*  明細ワーク作成　　　　　　　　　                            *
****************************************************************
 MEISAI-WT-SEC         SECTION.
*
     MOVE  "MEISAI-WT-SEC"  TO  S-NAME.
*返品累積一覧ワーク存在チェック
     MOVE   SPACE           TO  RWK-REC.
     INITIALIZE                 RWK-REC.
*
     MOVE   RHE-F01         TO  RWK-F01.
     MOVE   RHE-F02         TO  RWK-F03.
     MOVE   RHE-F03         TO  RWK-F02.
     MOVE   RHE-F04         TO  RWK-F07.
*
     READ   COMRWKF
            INVALID
            MOVE    1       TO  FG-DENHENF-INV
            NOT  INVALID
            MOVE    ZERO    TO  FG-DENHENF-INV
     END-READ.
*
     IF  FG-DENHENF-INV  =  0
         GO                 TO  READ-010
     END-IF.
*登録時
*    初期化
     MOVE    SPACE          TO  RWK-REC
     INITIALIZE                 RWK-REC
*取引先ＣＤ
     MOVE    RHE-F01        TO  RWK-F01
*検収日
     MOVE    RHE-F03        TO  RWK-F02
*店舗ＣＤ
     MOVE    RHE-F02        TO  RWK-F03
*店舗名
     MOVE    RHE-F01        TO  TEN-F52.
     MOVE    RHE-F02        TO  TEN-F011.
     PERFORM  RD-HTENMS-SEC.
     IF  FG-HTENMS-INV  =  ZERO
         MOVE  TEN-F03      TO  RWK-F04
     ELSE
         MOVE ALL NC"＊"    TO  RWK-F04
     END-IF.
*伝票区分
     MOVE    RHE-F06        TO  RWK-F05.
*伝票区分名
     MOVE    RHE-F01        TO  HEN-F01.
     MOVE    RHE-F06        TO  HEN-F02.
     PERFORM  RD-DENHENF-SEC.
     IF   FG-DENHENF-INV  =  ZERO
          MOVE HEN-F03      TO  RWK-F06
     ELSE
          MOVE ALL NC"＊"   TO  RWK-F06
     END-IF.
*伝票番号
     MOVE    RHE-F04        TO  RWK-F07.
*伝票内行数
     MOVE    RHE-F05        TO  RWK-F08.
*代表相手商品ＣＤ
     MOVE    RHE-F09        TO  RWK-F09.
*代表サカタ商品ＣＤ
     MOVE    RHE-F10        TO  RWK-F10.
     MOVE    RHE-F11        TO  RWK-F11.
     MOVE    RHE-F12        TO  RWK-F12.
     MOVE    RHE-F13        TO  RWK-F13.
*代表商品名
     MOVE    RHE-F14        TO  RWK-F14.
     MOVE    RHE-F15        TO  RWK-F15.
*代表数量
     MOVE    RHE-F16        TO  RWK-F16.
*代表単価
     MOVE    RHE-F17        TO  RWK-F17.
*計上区分
     MOVE    RHE-F80        TO  RWK-F18.
     EVALUATE  RHE-F80
         WHEN  SPACE
               MOVE  "1"    TO  RWK-F18
         WHEN  "1"
               IF  RHE-F86 = ZERO
                   MOVE "2" TO  RWK-F18
               ELSE
                   MOVE "3" TO  RWK-F18
               END-IF
         WHEN  "9"
               MOVE  "4"    TO  RWK-F18
         WHEN  OTHER
               MOVE   SPACE TO  RWK-F18
     END-EVALUATE.
*計上フラグ
     MOVE    "Y"            TO  RWK-F20.
*
 READ-000.
     ADD       1            TO  MEISAI-GYO.
     MOVE      MEISAI-GYO   TO  RWK-F19.
*レコード出力
     WRITE  RWK-REC.
     ADD    1               TO  TAISYO-MEISAI.
     GO                     TO  MEISAI-WT-EXIT.
 READ-010.
*伝票内行数
     MOVE    RHE-F05        TO  RWK-F08.
*レコード更新
     REWRITE  RWK-REC.
*
 READ-020.
     PERFORM  COMRHEF-READ-SEC.
*
 MEISAI-WT-EXIT.
     EXIT.
****************************************************************
*  明細セット　　　　                                          *
****************************************************************
 MEISAI-SET-SEC        SECTION.
*
     MOVE  "MEISAI-SET-SEC" TO  S-NAME.
*
     MOVE  SPACE            TO  DSP-MAS001(IX).
     MOVE  SPACE            TO  EDIT-STATUS OF DSP-MSENTA(IX).
     MOVE  SPACE            TO  EDIT-STATUS OF DSP-MKEIJ (IX).
*
     IF  READ-FLG  =  ZERO
         PERFORM  COMRWKF-READ-SEC
         IF  READ-FLG  NOT =  ZERO
             MOVE "X"       TO  EDIT-STATUS OF DSP-MSENTA(IX)
             MOVE "X"       TO  EDIT-STATUS OF DSP-MKEIJ (IX)
             MOVE "M"       TO  EDIT-OPTION OF DSP-MKEIJN(IX)
             MOVE "M"       TO  EDIT-COLOR  OF DSP-MKEIJN(IX)
             GO             TO  MEISAI-SET-EXIT
         END-IF
     ELSE
         MOVE "X"           TO  EDIT-STATUS OF DSP-MSENTA(IX)
         MOVE "X"           TO  EDIT-STATUS OF DSP-MKEIJ (IX)
         MOVE "M"           TO  EDIT-OPTION OF DSP-MKEIJN(IX)
         MOVE "M"           TO  EDIT-COLOR  OF DSP-MKEIJN(IX)
         GO                 TO  MEISAI-SET-EXIT
     END-IF.
*計上フラグ
     MOVE    RWK-F20        TO  DSP-MKEIJ(IX).
*ＳＥＱ
     MOVE    RWK-F19        TO  DSP-MGYO(IX).
*検収日
     MOVE    RWK-F02(1:4)   TO  WK-HEN-YYYY.
     MOVE    "/"            TO  WK-HEN-KU1.
     MOVE    RWK-F02(5:2)   TO  WK-HEN-MM.
     MOVE    "/"            TO  WK-HEN-KU2.
     MOVE    RWK-F02(7:2)   TO  WK-HEN-DD.
     MOVE    WK-HEN-DATE    TO  DSP-MKENDT(IX).
*伝票区分／伝票区分名
     MOVE    RWK-F05        TO  DSP-MDENKU(IX).
     MOVE    RWK-F06        TO  DSP-MDENNM(IX).
*店舗ＣＤ／店舗名
     MOVE    RWK-F03        TO  DSP-MTENCD(IX).
     MOVE    RWK-F04        TO  DSP-MTENNM(IX).
*伝票番号
     MOVE    RWK-F07        TO  DSP-MDENNO(IX).
*行数
     MOVE    RWK-F08        TO  DSP-MSOGYO(IX).
*計上状況
     MOVE    RWK-F18        TO  DSP-MKEIJO(IX).
     EVALUATE  RWK-F18
         WHEN  "1"     MOVE NC"未確認"  TO  DSP-MKEIJN(IX)
                       MOVE "R" TO EDIT-OPTION  OF DSP-MKEIJN(IX)
                       MOVE "R" TO EDIT-COLOR   OF DSP-MKEIJN(IX)
         WHEN  "2"     MOVE NC"未計上"  TO  DSP-MKEIJN(IX)
                       MOVE "R" TO EDIT-OPTION  OF DSP-MKEIJN(IX)
                       MOVE "Y" TO EDIT-COLOR   OF DSP-MKEIJN(IX)
         WHEN  "3"     MOVE NC"計上済"  TO  DSP-MKEIJN(IX)
                       MOVE "R" TO EDIT-OPTION  OF DSP-MKEIJN(IX)
                       MOVE "B" TO EDIT-COLOR   OF DSP-MKEIJN(IX)
         WHEN  "4"     MOVE NC"取　消" TO  DSP-MKEIJN(IX)
                       MOVE "R" TO EDIT-OPTION  OF DSP-MKEIJN(IX)
                       MOVE "G" TO EDIT-COLOR   OF DSP-MKEIJN(IX)
     END-EVALUATE.
*代表相手商品
     MOVE    RWK-F09        TO  DSP-MJAN(IX).
*代表ｻｶﾀ商品情報
     MOVE    RWK-F10        TO  WK-SKT-SYOCD.
     MOVE    SPACE          TO  WK-SKT-KU1.
     MOVE    RWK-F11        TO  WK-SKT-HINTAN1.
     MOVE    "-"            TO  WK-SKT-KU2.
     MOVE    RWK-F12        TO  WK-SKT-HINTAN2.
     MOVE    "-"            TO  WK-SKT-KU3.
     MOVE    RWK-F13        TO  WK-SKT-HINTAN3.
     MOVE    WK-SKT-HEN     TO  DSP-MSKTCD(IX).
*代表商品名
     MOVE    RWK-F14        TO  DSP-MSYONM(IX).
*代表数量
     MOVE    RWK-F16        TO  DSP-MSURYO(IX).
*代表単価
     MOVE    RWK-F17        TO  DSP-MTANKA(IX).
*
 MEISAI-SET-EXIT.
     EXIT.
****************************************************************
*             前ページ                                         *
****************************************************************
 ZEN-PAGE-SEC          SECTION.
     MOVE     "ZEN-PAGE-SEC"      TO   S-NAME.
**
     IF        P-CNT     =    1
               MOVE    6          TO   ERR-FLG
               GO      TO         ZEN-PAGE-EXIT
     END-IF.
*
*スクロール前に　画面上の計上区分をワークに更新
     CLOSE        COMRWKF.
     OPEN  I-O    COMRWKF.
     PERFORM MEISAI-UPD-SEC
             VARYING  IX  FROM  1  BY  1  UNTIL  IX  >   7.
     CLOSE        COMRWKF.
     OPEN  INPUT  COMRWKF.
*スクロール
     MOVE      DSP-TOKCD          TO   RWK-F01.
     MOVE      DSP-MTENCD(1)      TO   RWK-F03.
     MOVE      DSP-MKENDT(1)(1:4) TO   RWK-F02(1:4).
     MOVE      DSP-MKENDT(1)(6:2) TO   RWK-F02(5:2).
     MOVE      DSP-MKENDT(1)(9:2) TO   RWK-F02(7:2).
     MOVE      DSP-MDENNO(1)      TO   RWK-F07.
     START     COMRWKF   KEY IS   <    RWK-F01
                                       RWK-F02
                                       RWK-F03
                                       RWK-F07
               WITH REVERSED ORDER
        INVALID
               MOVE     6         TO    ERR-FLG
               GO       TO        ZEN-PAGE-EXIT
     END-START.
*
     SUBTRACT  1             FROM   P-CNT.
     MOVE      0               TO   READ-FLG.
     PERFORM   MEISAI-SET-SEC
               VARYING IX FROM 7 BY -1  UNTIL IX = 0.
*
 ZEN-PAGE-EXIT.
     EXIT.
****************************************************************
*             次ページ                                         *
****************************************************************
 JI-PAGE-SEC           SECTION.
     MOVE     "JI-PAGE-SEC"       TO   S-NAME.
**
     IF        P-CNT     =    MAX-PAGE
               MOVE    7          TO   ERR-FLG
               GO      TO         JI-PAGE-EXIT
     END-IF.
*
*スクロール前に　画面上の計上区分をワークに更新
     CLOSE        COMRWKF.
     OPEN  I-O    COMRWKF.
     PERFORM MEISAI-UPD-SEC
             VARYING  IX  FROM  1  BY  1  UNTIL  IX  >   7.
     CLOSE        COMRWKF.
     OPEN  INPUT  COMRWKF.
*スクロール
     MOVE      DSP-TOKCD          TO   RWK-F01.
     MOVE      DSP-MTENCD(7)      TO   RWK-F03.
     MOVE      DSP-MKENDT(7)(1:4) TO   RWK-F02(1:4).
     MOVE      DSP-MKENDT(7)(6:2) TO   RWK-F02(5:2).
     MOVE      DSP-MKENDT(7)(9:2) TO   RWK-F02(7:2).
     MOVE      DSP-MDENNO(7)      TO   RWK-F07.
     START     COMRWKF   KEY IS   >    RWK-F01
                                       RWK-F02
                                       RWK-F03
                                       RWK-F07
        INVALID
               MOVE     7         TO    ERR-FLG
               GO       TO        JI-PAGE-EXIT
     END-START.
*
     ADD       1               TO   P-CNT.
     MOVE      0               TO   READ-FLG.
     PERFORM   MEISAI-SET-SEC
               VARYING IX  FROM 1 BY 1   UNTIL IX  > 7.
*
 JI-PAGE-EXIT.
     EXIT.
****************************************************************
*  画面上の計上区分をワークに更新                            *
****************************************************************
 MEISAI-UPD-SEC        SECTION.
*
     MOVE  "MEISAI-UPD-SEC"   TO  S-NAME.
*
 MEISAI-UPD-01.
     MOVE      DSP-TOKCD           TO   RWK-F01.
     MOVE      DSP-MTENCD(IX)      TO   RWK-F03.
     MOVE      DSP-MKENDT(IX)(1:4) TO   RWK-F02(1:4).
     MOVE      DSP-MKENDT(IX)(6:2) TO   RWK-F02(5:2).
     MOVE      DSP-MKENDT(IX)(9:2) TO   RWK-F02(7:2).
     MOVE      DSP-MDENNO(IX)      TO   RWK-F07.
     START     COMRWKF   KEY IS    >=   RWK-F01
                                        RWK-F02
                                        RWK-F03
                                        RWK-F07
        INVALID
               DISPLAY NC"返品一覧ワーク異常！" UPON CONS
               MOVE     4010       TO   PROGRAM-STATUS
               STOP     RUN
     END-START.
*
 MEISAI-UPD-02.
     READ  COMRWKF  NEXT
           AT  END  GO  TO  MEISAI-UPD-SEC-EXIT
     END-READ.
*
 MEISAI-UPD-03.
     IF  (  RWK-F01      =  DSP-TOKCD           ) AND
         (  RWK-F03      =  DSP-MTENCD(IX)      ) AND
         (  RWK-F02(1:4) =  DSP-MKENDT(IX)(1:4) ) AND
         (  RWK-F02(5:2) =  DSP-MKENDT(IX)(6:2) ) AND
         (  RWK-F02(7:2) =  DSP-MKENDT(IX)(9:2) ) AND
         (  RWK-F07      =  DSP-MDENNO(IX)      )
            MOVE         DSP-MKEIJ(IX)  TO  RWK-F20
            REWRITE      RWK-REC
            GO           TO  MEISAI-UPD-02
     ELSE
            GO           TO  MEISAI-UPD-SEC-EXIT
     END-IF.
*
 MEISAI-UPD-SEC-EXIT.
     EXIT.
****************************************************************
*  返品累積一覧ワーク読込　　　　                            *
****************************************************************
 COMRWKF-READ-SEC      SECTION.
*
     MOVE  "CONRWKF-RAD-SEC"  TO  S-NAME.
*
     READ  COMRWKF  NEXT
           AT  END  MOVE  1   TO  READ-FLG
     END-READ.
*
 COMRWKF-READ-EXIT.
     EXIT.
****************************************************************
*    取引先マスタ検索                                          *
****************************************************************
 RD-HTOKMS-SEC          SECTION.
     MOVE  "RD-HTOKMS-SEC"  TO  S-NAME.

     READ  HTOKMS
       INVALID
         MOVE  1                 TO  FG-HTOKMS-INV
       NOT INVALID
         MOVE  ZERO              TO  FG-HTOKMS-INV
     END-READ.

 RD-HTOKMS-EXIT.
     EXIT.

****************************************************************
*    店舗マスタ検索　　　                                      *
****************************************************************
 RD-HTENMS-SEC          SECTION.
*
     MOVE  "RD-HTENMS-SEC"  TO  S-NAME.
*
     READ  HTENMS
       INVALID
         MOVE  1                 TO  FG-HTENMS-INV
       NOT INVALID
         MOVE  ZERO              TO  FG-HTENMS-INV
     END-READ.
*
 RD-HTENMS-EXIT.
     EXIT.
****************************************************************
*    伝票区分変換マスタ検索                                    *
****************************************************************
 RD-DENHENF-SEC         SECTION.
*
     MOVE  "RD-DENHENF-SEC" TO  S-NAME.
*
     READ  DENHENF
       INVALID
         MOVE  1                 TO  FG-DENHENF-INV
       NOT INVALID
         MOVE  ZERO              TO  FG-DENHENF-INV
     END-READ.
*
 RD-DENHENF-EXIT.
     EXIT.
****************************************************************
*  明細項目入力  ( PSW = 4 )                     *
****************************************************************
 DSP-BODY-SEC          SECTION.
     MOVE  "DSP-BODY-SEC"   TO  S-NAME.

     PERFORM  DSP-WRITE-SEC.
     PERFORM  DSP-READ-SEC.

     EVALUATE  DSP-FNC
*    実行
       WHEN  "E000"
         PERFORM  BODY-CHK-SEC
         PERFORM  BODY-CHK2-SEC
         IF       ERR-FLG  =  ZERO
*                 画面上の計上区分をワークに更新
                  CLOSE        COMRWKF
                  OPEN  I-O    COMRWKF
                  PERFORM MEISAI-UPD-SEC
                  VARYING  IX  FROM   1  BY   1
                               UNTIL  IX  >   7
                  CLOSE        COMRWKF
                  OPEN  INPUT  COMRWKF
         END-IF
*
*    項目戻り
       WHEN  "F006"
         MOVE  "1"          TO  PSW
*    終了
       WHEN  "F005"
         MOVE  "END"        TO  END-FLG

*    取消
       WHEN  "F004"
         MOVE  "1"          TO  PSW
         PERFORM  INIT-DSP-SEC

*    前頁
       WHEN  "F011"
         PERFORM  ZEN-PAGE-SEC

*    次頁
       WHEN  "F012"
         PERFORM  JI-PAGE-SEC

       WHEN  OTHER
         MOVE  1            TO  ERR-FLG
         GO TO  DSP-BODY-SEC

     END-EVALUATE.

 DSP-BODY-EXIT.
     EXIT.
****************************************************************
*             明細項目チェック                                 *
****************************************************************
 BODY-CHK-SEC          SECTION.
     MOVE "BODY-CHK-SEC"    TO  S-NAME.

     MOVE  ZERO             TO  CT-SEL.
     MOVE  ZERO             TO  FG-SEL.
     PERFORM  VARYING IX-GYO  FROM 1 BY 1
              UNTIL   IX-GYO  > 7
       PERFORM  BODY-CHKB-SEC

     END-PERFORM.
*
*    IF  CT-SEL = ZERO *> 選択なし
*        IF  ERR-FLG = ZERO
*            MOVE  11       TO  ERR-FLG
*        END-IF
*    END-IF.
*
* チェックＮＧ
     IF  ERR-FLG NOT = ZERO
         GO TO  BODY-CHK-EXIT
     END-IF.
*
*-   MOVE  13               TO  ERR-FLG.
*-   MOVE  "3"              TO  PSW.
 BODY-CHK-EXIT.
     EXIT.

****************************************************************
*             明細項目チェックＢ                               *
****************************************************************
 BODY-CHKB-SEC         SECTION.
     MOVE "BODY-CHKB-SEC"   TO  S-NAME.

     IF  DSP-MKENDT(IX-GYO) = SPACE *> データ無し行
         GO TO  BODY-CHKB-EXIT
     END-IF.

     MOVE  "M"         TO  EDIT-OPTION OF DSP-MSENTA(IX-GYO).
     MOVE  SPACE       TO  EDIT-CURSOR OF DSP-MSENTA(IX-GYO).

     IF  DSP-MSENTA(IX-GYO) = SPACE *> 選択無し行
         GO TO  BODY-CHKB-EXIT
     END-IF.

     IF  DSP-MSENTA(IX-GYO) = "Y"
         CONTINUE
     ELSE
         IF  ERR-FLG = ZERO
             MOVE  9        TO  ERR-FLG
         END-IF
         MOVE  "C"          TO  EDIT-CURSOR OF DSP-MSENTA(IX-GYO)
         MOVE  "R"          TO  EDIT-OPTION OF DSP-MSENTA(IX-GYO)
         GO TO  BODY-CHKB-EXIT
     END-IF.

     ADD  1   TO  CT-SEL.

     IF  FG-SEL = ZERO
         MOVE  1            TO  FG-SEL
         MOVE  IX-GYO       TO  WK-SELGYO
     ELSE
         IF  ERR-FLG = ZERO
             MOVE  12       TO  ERR-FLG
         END-IF
         MOVE  "C"          TO  EDIT-CURSOR OF DSP-MSENTA(IX-GYO)
         MOVE  "R"          TO  EDIT-OPTION OF DSP-MSENTA(IX-GYO)
     END-IF.

 BODY-CHKB-EXIT.
     EXIT.
*
****************************************************************
*             明細項目チェック２（計上フラグ）                 *
****************************************************************
 BODY-CHK2-SEC          SECTION.
     MOVE "BODY-CHK2-SEC"   TO  S-NAME.
*
     MOVE  ZERO             TO  KJ-SEL.
     PERFORM  VARYING IX-GYO  FROM 1 BY 1
              UNTIL   IX-GYO  > 7
       PERFORM  BODY-CHKC-SEC
*
     END-PERFORM.
*
* チェックＮＧ
     IF  ERR-FLG NOT = ZERO
         GO TO  BODY-CHK2-EXIT
     END-IF.
*
     MOVE  13               TO  ERR-FLG.
     MOVE  "3"              TO  PSW.
 BODY-CHK2-EXIT.
     EXIT.
*
****************************************************************
*             明細項目チェックＣ（計上フラグ）                 *
****************************************************************
 BODY-CHKC-SEC         SECTION.
     MOVE "BODY-CHKC-SEC"   TO  S-NAME.
*
     IF  DSP-MKENDT(IX-GYO) = SPACE *> データ無し行
         GO TO  BODY-CHKC-EXIT
     END-IF.
*
     MOVE  "M"         TO  EDIT-OPTION OF DSP-MKEIJ (IX-GYO).
     MOVE  SPACE       TO  EDIT-CURSOR OF DSP-MKEIJ (IX-GYO).

     IF  DSP-MKEIJ (IX-GYO) = SPACE *> 選択無し行
         GO TO  BODY-CHKC-EXIT
     END-IF.
*
     IF  DSP-MKEIJ (IX-GYO) = "Y"
         CONTINUE
     ELSE
         IF  ERR-FLG = ZERO
             MOVE  9        TO  ERR-FLG
         END-IF
         MOVE  "C"          TO  EDIT-CURSOR OF DSP-MKEIJ (IX-GYO)
         MOVE  "R"          TO  EDIT-OPTION OF DSP-MKEIJ (IX-GYO)
         GO TO  BODY-CHKC-EXIT
     END-IF.
*
     ADD  1   TO  KJ-SEL.
*
 BODY-CHKC-EXIT.
     EXIT.
*
****************************************************************
*  確認処理入力 （ PSW = 5 ）
****************************************************************
 DSP-KAKU-SEC          SECTION.
     MOVE  "DSP-KAKU-SEC"   TO  S-NAME.
*
     PERFORM  DSP-WRITE-SEC.
     PERFORM  DSP-READ-SEC.
*
     EVALUATE  DSP-FNC
*    実行
       WHEN  "E000"
*       画面上の計上区分をワークに更新
         CLOSE        COMRWKF
         OPEN  I-O    COMRWKF
         PERFORM MEISAI-UPD-SEC
                 VARYING  IX  FROM  1  BY  1  UNTIL  IX  >   7
         CLOSE        COMRWKF
         OPEN  INPUT  COMRWKF
*
         IF    CT-SEL   NOT =   ZERO
               PERFORM  PARA-OT-SEC
               CALL "SJR0490I" USING  LINK-JIKKBN
                                      LINK-BUMON
                                      LINK-TANCD
                                      LINK-TOKCD
                                      LINK-TENCD
                                      LINK-KENDT
                                      LINK-DENNO
                                      LINK-CHGKDT
*              呼んだPGの画面表示をクリアする
               MOVE  "CL"             TO  DSP-PRO
               PERFORM  MSENTA-CLR-SEC
               MOVE  "SCREEN"         TO  DSP-GRP
               MOVE  "FJR04801"       TO  DSP-FMT
               WRITE  DSP-FJR04801
               MOVE  SPACE            TO  DSP-PRO
               MOVE  "2"              TO  PSW
         ELSE
               IF   DSP-ENDCHK  =  "Y"
                    PERFORM  COMRHEF-UPD-SEC
                    MOVE  "END"            TO  END-FLG
               ELSE
                    MOVE  18               TO  ERR-FLG
               END-IF
         END-IF
*
*    終了
       WHEN  "F005"
         MOVE  "END"        TO  END-FLG
*    項目戻し
       WHEN  "F006"
         MOVE  "2"          TO  PSW
*    取消
       WHEN  "F004"
         MOVE  "1"          TO  PSW
         PERFORM  INIT-DSP-SEC
*
       WHEN  OTHER
         MOVE  1            TO  ERR-FLG
         GO TO  DSP-KAKU-SEC
*
     END-EVALUATE.

 DSP-KAKU-EXIT.
     EXIT.
****************************************************************
*    パラメータ出力処理                                        *
****************************************************************
 PARA-OT-SEC          SECTION.
     MOVE  "PARA-OT-SEC"    TO  S-NAME.

* 実行区分
     MOVE  PARA-JIKKBN      TO  LINK-JIKKBN.
* 部門ＣＤ
     MOVE  PARA-BUMON       TO  LINK-BUMON.
* 担当者ＣＤ
     MOVE  PARA-TANCD       TO  LINK-TANCD.
* 取引先ＣＤ
     MOVE  DSP-TOKCD        TO  LINK-TOKCD.
* 実検収日
     MOVE  DSP-CHGKDT       TO  LINK-CHGKDT.
* 店舗ＣＤ
     MOVE  DSP-MTENCD(WK-SELGYO)   TO  LINK-TENCD.
* 検収日
     MOVE  DSP-MKENDT(WK-SELGYO)(1:4)  TO  LINK-KENDT(1:4).
     MOVE  DSP-MKENDT(WK-SELGYO)(6:2)  TO  LINK-KENDT(5:2).
     MOVE  DSP-MKENDT(WK-SELGYO)(9:2)  TO  LINK-KENDT(7:2).
* 伝票番号
     MOVE  DSP-MDENNO(WK-SELGYO)   TO  LINK-DENNO.

 PARA-OT-EXIT.
     EXIT.
****************************************************************
*    明細選択欄初期化　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MSENTA-CLR-SEC        SECTION.
*
     PERFORM VARYING IX FROM 1 BY 1  UNTIL  IX  >  7
        MOVE  "M"      TO       EDIT-OPTION OF DSP-MSENTA(IX)
        MOVE  SPACE    TO       EDIT-CURSOR OF DSP-MSENTA(IX)
        MOVE  SPACE    TO       EDIT-OPTION OF DSP-MSENTA(IX)
        MOVE  SPACE    TO       DSP-MSENTA(IX)
     END-PERFORM.
*
 MSENTA-CLR-EXIT.
     EXIT.
****************************************************************
*  返品累積ファイル更新                                        *
****************************************************************
 COMRHEF-UPD-SEC          SECTION.
     MOVE  "COMRHEF-UPD-SEC"  TO  S-NAME.
*
 COMRHEF-UPD-01.
     CLOSE     COMRHEF  COMRWKF.
     OPEN      INPUT    COMRWKF.
     OPEN      I-O      COMRHEF.
     MOVE      0            TO  COMRHEF-UPD-CNT.
*
 COMRHEF-UPD-02.
     READ      COMRWKF NEXT
         AT END
               GO           TO  COMRHEF-UPD-EXIT
     END-READ.
*
 COMRHEF-UPD-03.
     IF        RWK-F20      =   " "
               GO           TO  COMRHEF-UPD-02
     END-IF.
*
 COMRHEF-UPD-04.
     MOVE SPACE             TO  RHE-REC.
     INITIALIZE                 RHE-REC.
     MOVE      RWK-F01      TO  RHE-F01.
     MOVE      RWK-F02      TO  RHE-F03.
     MOVE      RWK-F03      TO  RHE-F02.
     MOVE      RWK-F07      TO  RHE-F04.
     MOVE      1            TO  RHE-F05.
*
     START  COMRHEF  KEY IS >=  RHE-F01  RHE-F03  RHE-F02
                                RHE-F04  RHE-F05
            INVALID
               DISPLAY NC"返品累積ファイル異常１" UPON CONS
               MOVE  4010   TO  PROGRAM-STATUS
               STOP  RUN
     END-START.
*
 COMRHEF-UPD-05.
*    返品累積データ読込1件目
     READ   COMRHEF  NEXT
        AT END
            DISPLAY NC"返品累積ファイル異常２" UPON CONS
            MOVE  4010   TO  PROGRAM-STATUS
            STOP  RUN
     END-READ.
*
 COMRHEF-UPD-06.
     IF   ( RHE-F01  =  RWK-F01 ) AND
          ( RHE-F02  =  RWK-F03 ) AND
          ( RHE-F03  =  RWK-F02 ) AND
          ( RHE-F04  =  RWK-F07 )
            CONTINUE
     ELSE
            DISPLAY NC"返品累積ファイル異常３" UPON CONS
            DISPLAY NC"取引先＝" RWK-F01 UPON CONS
            DISPLAY NC"　　　　" RHE-F01 UPON CONS
            DISPLAY NC"店舗　＝" RWK-F03 UPON CONS
            DISPLAY NC"　　　　" RHE-F02 UPON CONS
            DISPLAY NC"検収日＝" RWK-F02 UPON CONS
            DISPLAY NC"　　　　" RHE-F03 UPON CONS
            DISPLAY NC"伝票　＝" RWK-F07 UPON CONS
            DISPLAY NC"　　　　" RHE-F04 UPON CONS
            MOVE  4010   TO  PROGRAM-STATUS
            STOP  RUN
     END-IF.
 COMRHEF-UPD-07.
     IF        DSP-CHGKDT   NOT = ZERO
               MOVE       RHE-F07        TO    RHE-F35
               MOVE       DSP-CHGKDT     TO    RHE-F07
     END-IF.
     MOVE      "9"          TO  RHE-F22.
     MOVE      RHE-F23      TO  RHE-F37.
     MOVE      "42"         TO  RHE-F23.
     MOVE      "1"          TO  RHE-F80.
     MOVE      PARA-BUMON   TO  RHE-F81 RHE-F98.
     MOVE      PARA-TANCD   TO  RHE-F82 RHE-F99.
     MOVE      SYS-DATE     TO  RHE-F83 RHE-F96.
     MOVE      WK-TIME(1:6) TO  RHE-F97.
     REWRITE   RHE-REC.
     ADD       1            TO  COMRHEF-UPD-CNT.
*
 COMRHEF-UPD-08.
*    返品累積データ読込2件目以降
     READ   COMRHEF  NEXT
        AT END
            GO              TO  COMRHEF-UPD-02
     END-READ.
*
 COMRHEF-UPD-09.
     IF   ( RHE-F01  =  RWK-F01 ) AND
          ( RHE-F02  =  RWK-F03 ) AND
          ( RHE-F03  =  RWK-F02 ) AND
          ( RHE-F04  =  RWK-F07 )
            CONTINUE
     ELSE
            GO              TO  COMRHEF-UPD-02
     END-IF.
*
 COMRHEF-UPD-10.
     IF        DSP-CHGKDT   NOT = ZERO
               MOVE       RHE-F07        TO    RHE-F35
               MOVE       DSP-CHGKDT     TO    RHE-F07
     END-IF.
     MOVE      "9"          TO  RHE-F22.
     MOVE      RHE-F23      TO  RHE-F37.
     MOVE      "42"         TO  RHE-F23.
     MOVE      "1"          TO  RHE-F80.
     MOVE      PARA-BUMON   TO  RHE-F81 RHE-F98.
     MOVE      PARA-TANCD   TO  RHE-F82 RHE-F99.
     MOVE      SYS-DATE     TO  RHE-F83 RHE-F96.
     MOVE      WK-TIME(1:6) TO  RHE-F97.
     REWRITE   RHE-REC.
     ADD       1            TO  COMRHEF-UPD-CNT.
     GO                     TO  COMRHEF-UPD-08.
*
 COMRHEF-UPD-EXIT.
     EXIT.
****************************************************************
*             終了処理                               3.0       *
****************************************************************
 END-SEC               SECTION.
*ファイル ＣＬＯＳＥ
     CLOSE  DSPFILE  COMRHEF  HTOKMS  HTENMS
            DENHENF  JYOKEN1  COMRWKF.
*
 END-EXIT.
     EXIT.
*****************<<  SJR0480I   END PROGRAM  >>******************
