****************************************************************
*
*    顧客名　　　　　　　：　（株）サカタのタネ殿
*    サブシステム　　　　：　マスタメンテ
*    業務名　　　　　　　：　ＥＯＳ管理
*    モジュール名　　　　：　_番一括変更入力
*    処理概要　　　　　　：　
*      相手商品ＣＤを指定して、_番一括更新処理の入力データ
*      の作成を行う。変更した内容は_卸一括更新処理にて更新
*      される。
*    作成日／更新日　　　：　2011/11/21
*    作成者／更新者　　　：　飯田/NAV
*
****************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           ZMT995I.
 AUTHOR.               S.I.
 DATE-WRITTEN.         2011/11/21.
****************************************************************
 ENVIRONMENT           DIVISION.
****************************************************************
 CONFIGURATION         SECTION.
 SPECIAL-NAMES.
     CONSOLE      IS   CONS.

 INPUT-OUTPUT          SECTION.
 FILE-CONTROL.
*画面定義ファイル
     SELECT  DSPFILE  ASSIGN    TO        GS-DSPF
                      FORMAT              DSP-FMT
                      GROUP               DSP-GRP
                      PROCESSING          DSP-PRO
                      FUNCTION            DSP-FNC
                      FILE      STATUS    DSP-ST.
*----<< 商品変換テーブル >>-*
     SELECT  HSHOTBL  ASSIGN    TO        SHOTBL13
                      ORGANIZATION        INDEXED
                      ACCESS    MODE      DYNAMIC
                      RECORD    KEY
                        STB-F02 *> 量販店商品ＣＤ
                        STB-F04 *> 出荷場所
                        STB-F08 *> _番
                        STB-F01 *> 取引先ＣＤ
                        STB-F03 *> 自社商品ＣＤ
                      FILE      STATUS    STB-ST.
*----<< _番一括メンテデータ >>-*
     SELECT  TANAMTBL ASSIGN    TO        TANAMTB3
                      ORGANIZATION        INDEXED
                      ACCESS    MODE      DYNAMIC
                      RECORD    KEY
                        TNM-F02 *> 量販店商品ＣＤ
                        TNM-F04 *> 出荷場所
                        TNM-F20 *> 旧_番
                        TNM-F01 *> 取引先ＣＤ
                        TNM-F03 *> 自社商品ＣＤ
                      FILE      STATUS    TNM-ST.
*----<< 取引先マスタ >>-*
     SELECT  HTOKMS   ASSIGN    TO        TOKMS2
                      ORGANIZATION        INDEXED
                      ACCESS    MODE      DYNAMIC
                      RECORD    KEY
                        TOK-F01
                      FILE      STATUS    TOK-ST.
*----<< 商品名称マスタ >>-*
     SELECT  HMEIMS   ASSIGN    TO        MEIMS1
                      ORGANIZATION        INDEXED
                      ACCESS    MODE      DYNAMIC
                      RECORD    KEY
                        MEI-F01
                      FILE      STATUS    MEI-ST.
*----<< 倉庫マスタ >>-*
     SELECT  ZSOKMS   ASSIGN    TO        ZSOKMS1
                      ORGANIZATION        INDEXED
                      ACCESS    MODE      RANDOM
                      RECORD    KEY
                        SOK-F01
                      FILE      STATUS    SOK-ST.
*
****************************************************************
 DATA                DIVISION.
****************************************************************
 FILE                SECTION.
****************************************************************
*    FILE = 画面ファイル
****************************************************************
 FD  DSPFILE.
     COPY      FMT995    OF   XMDLIB
     JOINING   DSP       AS   PREFIX.
****************************************************************
*    FILE = 商品変換テーブル
****************************************************************
 FD  HSHOTBL
     LABEL     RECORD     IS  STANDARD.
     COPY      HSHOTBL   OF   XFDLIB
     JOINING   STB  AS   PREFIX.
****************************************************************
*    FILE = _番一括メンテデータ
****************************************************************
 FD  TANAMTBL
     LABEL     RECORD     IS  STANDARD.
     COPY      TANAMTBL   OF   XFDLIB
     JOINING   TNM  AS   PREFIX.
****************************************************************
*    FILE = 取引先マスタ
****************************************************************
 FD  HTOKMS
     LABEL     RECORD    IS   STANDARD.
     COPY      HTOKMS    OF   XFDLIB
     JOINING   TOK       AS   PREFIX.
****************************************************************
*    FILE = 商品名称マスタ
****************************************************************
 FD  HMEIMS
     LABEL     RECORD     IS  STANDARD.
     COPY      HMEIMS    OF   XFDLIB
     JOINING   MEI  AS   PREFIX.
****************************************************************
*    FILE = 倉庫マスタ
****************************************************************
 FD  ZSOKMS
     LABEL     RECORD     IS  STANDARD.
     COPY      ZSOKMS1   OF   XFDLIB
     JOINING   SOK  AS PREFIX.
*
****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
*ステータス領域
 01  STATUS-AREA.
     03  DSP-ST                   PIC  X(02).
     03  STB-ST                   PIC  X(02).
     03  TNM-ST                   PIC  X(02).
     03  TOK-ST                   PIC  X(02).
     03  MEI-ST                   PIC  X(02).
     03  SOK-ST                   PIC  X(02).
*画面制御用領域
 01  DSP-CONTROL.
     03  DSP-FMT                  PIC  X(08).
     03  DSP-GRP                  PIC  X(08).
     03  DSP-PRO                  PIC  X(02).
     03  DSP-FNC                  PIC  X(04).
 01  SET-PGID                     PIC  X(08)  VALUE "ZMT995I ".
 01  SET-FORMID                   PIC  X(08)  VALUE "FMT995  ".
 01  MAX-PGCNT                    PIC  9(03)  VALUE 20.
 01  MAX-LNCNT                    PIC  9(03)  VALUE 07.
*フラグ領域
 01  FLG-AREA.
     03  FG-HSHOTBL-END     PIC  X(03)  VALUE  SPACE.
     03  FG-TANAMTBL-END    PIC  X(03)  VALUE  SPACE.
     03  FG-TANAMTBL-INV    PIC  9(01)  VALUE  ZERO.
     03  FG-HTOKMS-INV      PIC  9(01)  VALUE  ZERO.
     03  FG-HMEIMS-INV      PIC  9(01)  VALUE  ZERO.
     03  FG-ZSOKMS-INV      PIC  9(01)  VALUE  ZERO.
     03  INV-FLG            PIC  X(03)  VALUE  SPACE.
     03  CHK-FLG            PIC  X(03)  VALUE  SPACE.
     03  ERR-FLG            PIC  9(02)  VALUE  ZERO.
     03  END-FLG            PIC  X(03)  VALUE  SPACE.
     03  RD-FLG             PIC  X(03)  VALUE  SPACE.
     03  SET-FLG            PIC  X(03)  VALUE  SPACE.
     03  P-CNT              PIC  9(02)  VALUE  ZERO.
     03  S-CNT              PIC  9(02)  VALUE  ZERO.
     03  C-CNT              PIC  9(02)  VALUE  ZERO.
     03  X                  PIC  9(02)  VALUE  ZERO.
     03  Y                  PIC  9(02)  VALUE  ZERO.
     03  IX                 PIC  9(02)  VALUE  ZERO.
     03  IY                 PIC  9(02)  VALUE  ZERO.
     03  WK-SEQ             PIC  9(03)  VALUE  ZERO.
     03  END-SW             PIC  X(01)  VALUE  SPACE.
     03  WC-FLG             PIC  X(01)  VALUE  SPACE.
     03  FG-TBLOVL          PIC  9(01).
     03  FG-CHKLVL          PIC  9(01).
     03  FG-UPD             PIC  9(01).


*ワーク領域
 01  WRK-AREA.
***  プログラムスイッチ（画面遷移制御）
     03  PSW                PIC  X(01)  VALUE  SPACE.
     03  S-NAME-SV          PIC  X(20).
     03  CT-REG             PIC  9(07)  VALUE  ZERO.
     03  CT-UPD             PIC  9(07)  VALUE  ZERO.
     03  CT-DLT             PIC  9(07)  VALUE  ZERO.
     03  ERR-CNT            PIC  9(07)  VALUE  ZERO.

     03  IX-TBL             PIC  9(04)  VALUE  ZERO.
     03  IX-TBLMAX          PIC  9(04)  VALUE  ZERO.
     03  IX-GYO             PIC  9(03)  VALUE  ZERO.
     03  IX-GYOMAX          PIC  9(03)  VALUE  ZERO.
     03  CT-PG              PIC  9(04)  VALUE  ZERO.
     03  CT-PGMAX           PIC  9(04)  VALUE  ZERO.

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
 01  PF-MSG-AREA.
     03  PF-MSG1.
         05  FILLER         PIC  N(20)
      VALUE NC"_取消　_終了".
     03  PF-MSG2.
         05  FILLER         PIC  N(20)
      VALUE NC"_取消　_終了　_項目戻り".
     03  PF-MSG3.
         05  FILLER         PIC  N(20)
      VALUE NC"_取消　_項目戻り　_前頁　_次頁".
 01  PF-MSG-AREA-R       REDEFINES     PF-MSG-AREA.
     03  PF-MSG-R           PIC  N(20)  OCCURS 3.

*メッセージの取得
 01  ERR-MSG-AREA.
     03  ERR-MSG1.
       05  FILLER           PIC  N(35)  VALUE
       NC"無効ＰＦキーです。".
     03  ERR-MSG2.
       05  FILLER           PIC  N(35)  VALUE
       NC"前頁はありません。".
     03  ERR-MSG3.
       05  FILLER           PIC  N(35)  VALUE
       NC"次頁はありません。".
     03  ERR-MSG4.
       05  FILLER           PIC  N(35)  VALUE
       NC"処理区分を入力して下さい。".
     03  ERR-MSG5.
       05  FILLER           PIC  N(35)  VALUE
       NC"処理区分に誤りがあります。".
     03  ERR-MSG6.
       05  FILLER           PIC  N(35)  VALUE
       NC"相手先商品コードを入力して下さい。".
     03  ERR-MSG7.
       05  FILLER           PIC  N(35)  VALUE
       NC"出荷場所を入力して下さい。".
     03  ERR-MSG8.
       05  FILLER           PIC  N(35)  VALUE
       NC"出荷場所に誤りがあります。".
     03  ERR-MSG9.
       05  FILLER           PIC  N(35)  VALUE
       NC"対象データが既に存在します。".
     03  ERR-MSG10.
       05  FILLER           PIC  N(35)  VALUE
       NC"対象データがありません。".
     03  ERR-MSG11.
       05  FILLER           PIC  N(35)  VALUE
       NC"相手先商品ＣＤに誤りがあります。".
     03  ERR-MSG12.
*---------------------------------------------------------------E*
       05  FILLER           PIC  N(35)  VALUE
           NC"仕入単価　≦　原価単価　≦　売価単価でなければなり
-            "ません。".
     03  ERR-MSG13.
       05  FILLER           PIC  N(35)  VALUE
       NC"対象データが商品変換テーブルに存在しません。".
     03  ERR-MSG14.
*---------------------------------------------------------------E*
       05  FILLER           PIC  N(35)  VALUE
           NC"データはありますが、これ以上のスクロールはできませ
-            "ん。".
     03  ERR-MSG15.
*---------------------------------------------------------------E*
       05  FILLER           PIC  N(35)  VALUE
         NC"指定条件に誤りがないか確認し、ＥＮＴＥＲを押下して下
-          "さい。".
     03  ERR-MSG16.
       05  FILLER           PIC  N(35)  VALUE
       NC"　".
     03  ERR-MSG17.
       05  FILLER           PIC  N(35)  VALUE
       NC"　".
     03  ERR-MSG18.
       05  FILLER           PIC  N(35)  VALUE
       NC"　".
     03  ERR-MSG19.
       05  FILLER           PIC  N(35)  VALUE
       NC"　".
     03  ERR-MSG20.
       05  FILLER           PIC  N(35)  VALUE
       NC"　".
 01  ERR-MSG-AREA-R  REDEFINES ERR-MSG-AREA.
     03  ERR-MSG-R          PIC  N(35)  OCCURS 20.
*メンテデータ退避エリア
 01  TB-AREA.
     03  TB-G.
       05  TB-G2    OCCURS 20. *> 頁
         07  TB-G3  OCCURS  7. *> 行
           09  TB-GYO             PIC  9(02).
           09  TB-TORCD           PIC  9(08).
           09  TB-SHOCD           PIC  X(08).
           09  TB-HINTAN          PIC  X(08).
           09  TB-GTANKA          PIC  9(07)V9(02).
           09  TB-UTANKA          PIC  9(07)V9(02).
           09  TB-BNRCD           PIC  X(04).
           09  TB-NTNNO           PIC  X(06).
           09  TB-STANKA          PIC  9(07)V9(02).
           09  TB-YOBI1           PIC  X(01).
           09  TB-KENPNG          PIC  9(05).
           09  TB-SHK-AREA-KBN    PIC  X(01).
           09  TB-REG-TANCD       PIC  X(02).
           09  TB-LAST-UPD-TANCD  PIC  X(02).
           09  TB-REG-DATE        PIC  9(08).
           09  TB-OTNNO           PIC  X(06).
           09  TB-UPD-DATE        PIC  9(08).
           09  TB-MAE-GTANKA      PIC  9(07)V9(02).
           09  TB-MAE-UTANKA      PIC  9(07)V9(02).
           09  TB-MAE-NTNNO       PIC  X(06).
           09  TB-MAE-STANKA      PIC  9(07)V9(02).
     03  TB-GR  REDEFINES TB-G.
       05  TB-G2R   OCCURS 140.
         07  TB-GYO-R             PIC  9(02).
         07  TB-TORCD-R           PIC  9(08).
         07  TB-SHOCD-R           PIC  X(08).
         07  TB-HINTAN-R          PIC  X(08).
         07  TB-GTANKA-R          PIC  9(07)V9(02).
         07  TB-UTANKA-R          PIC  9(07)V9(02).
         07  TB-BNRCD-R           PIC  X(04).
         07  TB-NTNNO-R           PIC  X(06).
         07  TB-STANKA-R          PIC  9(07)V9(02).
         07  TB-YOBI1-R           PIC  X(01).
         07  TB-KENPNG-R          PIC  9(05).
         07  TB-SHK-AREA-KBN-R    PIC  X(01).
         07  TB-REG-TANCD-R       PIC  X(02).
         07  TB-LAST-UPD-TANCD-R  PIC  X(02).
         07  TB-REG-DATE-R        PIC  9(08).
         07  TB-OTNNO-R           PIC  X(06).
         07  TB-UPD-DATE-R        PIC  9(08).
         07  TB-MAE-GTANKA-R      PIC  9(07)V9(02).
         07  TB-MAE-UTANKA-R      PIC  9(07)V9(02).
         07  TB-MAE-NTNNO-R       PIC  X(06).
         07  TB-MAE-STANKA-R      PIC  9(07)V9(02).
*ファイルエラーメッセージ
 01  FILE-ERR.
     03  DSP-ERR            PIC  N(15) VALUE
         NC"画面ファイルエラー".
     03  STB-ERR            PIC  N(15) VALUE
         NC"商品変換テーブルエラー".
     03  TNM-ERR            PIC  N(15) VALUE
         NC"_番一括メンテデータエラー".
     03  TOK-ERR            PIC  N(15) VALUE
         NC"取引先マスタエラー".
     03  MEI-ERR            PIC  N(15) VALUE
         NC"商品名称マスタエラー".
     03  SOK-ERR            PIC  N(15) VALUE
         NC"倉庫マスタエラー".
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
**************************************************************
 PROCEDURE             DIVISION.
**************************************************************
 DECLARATIVES.
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
 STB-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HSHOTBL.
     MOVE        STB-ST    TO        E-ST.
     MOVE        "HSHOTBL"  TO        E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     STB-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 TNM-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE TANAMTBL.
     MOVE        TNM-ST    TO        E-ST.
     MOVE        "TANAMTBL"  TO        E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     TNM-ERR   UPON      CONS.
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
 MEI-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HMEIMS.
     MOVE        MEI-ST    TO        E-ST.
     MOVE        "HMEIMS"  TO        E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     MEI-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 SOK-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE ZSOKMS.
     MOVE        SOK-ST    TO        E-ST.
     MOVE        "ZSOKMS"  TO        E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     SOK-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 END  DECLARATIVES.
****************************************************************
*             MAIN        MODULE                     0.0
****************************************************************
 PROCESS-START         SECTION.
     MOVE  "PROCESS-START"  TO  S-NAME.

     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC  UNTIL END-FLG  = "END".
     PERFORM  END-SEC.

 CONTROL-EXIT.
     STOP  RUN.
****************************************************************
*             初期処理                               1.0
****************************************************************
 INIT-SEC              SECTION.
     MOVE  S-NAME           TO  S-NAME-SV.
     MOVE  "INIT-SEC"       TO  S-NAME.
*ファイルのＯＰＥＮ
     OPEN  I-O    DSPFILE.
     OPEN  INPUT  HSHOTBL.
     OPEN  INPUT  HTOKMS.
     OPEN  INPUT  HMEIMS.
     OPEN  INPUT  ZSOKMS.
     OPEN  I-O    TANAMTBL.
*ワークの初期化
     INITIALIZE  FLG-AREA.
     INITIALIZE  WRK-AREA.
     MOVE  ZERO             TO  PSW.
*初期画面の表示
     MOVE  SPACE            TO  DSP-PRO.
     PERFORM  INIT-DSP-SEC.
     MOVE  "1"              TO  PSW.

     MOVE  S-NAME-SV        TO  S-NAME.
 INIT-EXIT.
     EXIT.
****************************************************************
*             初期画面表示
****************************************************************
 INIT-DSP-SEC          SECTION.
     MOVE  S-NAME           TO  S-NAME-SV.
     MOVE  "INIT-DSP-SEC"   TO  S-NAME.

* 画面の初期化
     IF  PSW = "1" OR ZERO   *> 処理選択、初画面
         MOVE  SPACE        TO  DSP-FMT995
     END-IF.

     IF  PSW = "2"            *> 相手先商品ＣＤ入力
         MOVE SPACE         TO  DSP-AITSHO
         MOVE SPACE         TO  DSP-BASHO
         MOVE SPACE         TO  DSP-OTNNO
         MOVE SPACE         TO  DSP-NTNNO
         MOVE SPACE         TO  DSP-BASHNM
     END-IF.

     IF  PSW = "2" OR "3" OR "4"  *> 明細入力、確認
         PERFORM  VARYING IX-GYO  FROM 1 BY 1
                  UNTIL   IX-GYO > 7
           MOVE  SPACE      TO  DSP-MAS001 (IX-GYO)

         END-PERFORM

         MOVE  SPACE        TO  DSP-KKNN
     END-IF.

* ＰＧＩＤ・ＦＭＴＩＤセット
     MOVE  SET-PGID         TO  DSP-PGID.
     MOVE  SET-FORMID       TO  DSP-FORM.

* リバース，カーソルパーク解除
     EVALUATE   PSW
       WHEN  "1"    *> 処理選択
         MOVE SPACE TO EDIT-CURSOR OF DSP-SYOKBN
         MOVE "M"   TO EDIT-OPTION OF DSP-SYOKBN

       WHEN  "2"    *> 相手先商品ＣＤ入力
         MOVE SPACE TO  EDIT-CURSOR OF DSP-AITSHO
         MOVE "M"   TO  EDIT-OPTION OF DSP-AITSHO
         MOVE SPACE TO  EDIT-CURSOR OF DSP-BASHO
         MOVE "M"   TO  EDIT-OPTION OF DSP-BASHO
         MOVE SPACE TO  EDIT-CURSOR OF DSP-OTNNO
         MOVE "M"   TO  EDIT-OPTION OF DSP-OTNNO
         MOVE SPACE TO  EDIT-CURSOR OF DSP-NTNNO
         MOVE "M"   TO  EDIT-OPTION OF DSP-NTNNO

       WHEN  "3"    *> 明細入力
         PERFORM  VARYING IX-GYO  FROM 1 BY 1
                  UNTIL   IX-GYO > 7
           MOVE SPACE TO  EDIT-CURSOR OF DSP-NTNNOM (IX-GYO)
           MOVE "M"   TO  EDIT-OPTION OF DSP-NTNNOM (IX-GYO)
           MOVE SPACE TO  EDIT-CURSOR OF DSP-GTANKA (IX-GYO)
           MOVE "M"   TO  EDIT-OPTION OF DSP-GTANKA (IX-GYO)
           MOVE SPACE TO  EDIT-CURSOR OF DSP-UTANKA (IX-GYO)
           MOVE "M"   TO  EDIT-OPTION OF DSP-UTANKA (IX-GYO)
           MOVE SPACE TO  EDIT-CURSOR OF DSP-STANKA (IX-GYO)
           MOVE "M"   TO  EDIT-OPTION OF DSP-STANKA (IX-GYO)
         END-PERFORM

       WHEN  OTHER  *> 確認、初画面
         CONTINUE

     END-EVALUATE.

*頁セット
     MOVE  1                TO  CT-PG.

     MOVE  S-NAME-SV        TO  S-NAME.
 INIT-DSP-EXIT.
     EXIT.
****************************************************************
*             メイン処理                             2.0
****************************************************************
 MAIN-SEC              SECTION.
     MOVE  S-NAME           TO  S-NAME-SV.
     MOVE  "MAIN-SEC"       TO  S-NAME.

     EVALUATE  PSW
  *> 処理区分入力
       WHEN  "1"  PERFORM  DSP-HEAD-SEC
  *> 相手商品ＣＤ入力
       WHEN  "2"  PERFORM  DSP-HEAD2-SEC
  *> 明細入力
       WHEN  "3"  PERFORM  DSP-BODY-SEC
  *> 確認入力)
       WHEN  "4"  PERFORM  DSP-KAKU-SEC
  *> 以外
       WHEN  OTHER
         DISPLAY "ZMT995I PSW ??? PWS=" PSW "*" UPON CONS
         MOVE "END"         TO  END-FLG
     END-EVALUATE.

     MOVE  S-NAME-SV        TO  S-NAME.
 MAIN-EXIT.
     EXIT.
****************************************************************
*  処理区分入力 (PSW = 1)
****************************************************************
 DSP-HEAD-SEC          SECTION.
     MOVE  S-NAME           TO  S-NAME-SV.
     MOVE  "DSP-HEAD-SEC"   TO  S-NAME.

     PERFORM  DSP-WRITE-SEC.
     PERFORM  DSP-READ-SEC.

     EVALUATE  DSP-FNC
       WHEN   "E000"  *> 実行
         PERFORM  HEAD-CHK-SEC

       WHEN  "F004" *> 取消
         PERFORM  INIT-DSP-SEC

       WHEN   "F005" *> 終了
         MOVE  "END"        TO  END-FLG

       WHEN   OTHER
         MOVE  1            TO  ERR-FLG
     END-EVALUATE.

     MOVE  S-NAME-SV        TO  S-NAME.
 DSP-HEAD-EXIT.
     EXIT.
****************************************************************
*  画面表示処理
****************************************************************
 DSP-WRITE-SEC         SECTION.
     MOVE  S-NAME           TO  S-NAME-SV.
     MOVE  "DSP-WRITE-SEC"  TO  S-NAME.

* システム日付、時刻の編集
     PERFORM  SDATE-GET-SEC.

* エラーメッセージセット
     IF ERR-FLG = ZERO
        MOVE  SPACE         TO  DSP-ERRMSG
     ELSE
        MOVE  ERR-MSG-R(ERR-FLG) TO  DSP-ERRMSG
        MOVE  ZERO               TO  ERR-FLG
     END-IF.
* ガイドメッセージの設定
     EVALUATE  PSW
       WHEN  "1"
         MOVE  PF-MSG-R(1)   TO  DSP-GUIDE
       WHEN  "2"
         MOVE  PF-MSG-R(2)   TO  DSP-GUIDE
       WHEN  "3"
         MOVE  PF-MSG-R(3)   TO  DSP-GUIDE
       WHEN  "4"
         MOVE  PF-MSG-R(2)   TO  DSP-GUIDE
     END-EVALUATE.
* 画面の表示
     MOVE  "SCREEN"         TO  DSP-GRP.
     MOVE  "FMT995"         TO  DSP-FMT.
     WRITE  DSP-FMT995.

     MOVE  S-NAME-SV        TO  S-NAME.
 DSP-WRITE-EXIT.
     EXIT.
****************************************************************
*    日付時刻取得
****************************************************************
 SDATE-GET-SEC         SECTION.
     MOVE  S-NAME           TO  S-NAME-SV.
     MOVE  "SDATE-GET-SEC"  TO  S-NAME.

* システム日付・時刻の取得
     ACCEPT  WK-DATE   FROM  DATE.
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
     MOVE  LINK-OUT-YMD     TO  DATE-AREA.
* 表示日付編集
     MOVE  SYS-DATE(1:4)    TO  HEN-DATE-YYYY.
     MOVE  SYS-DATE(5:2)    TO  HEN-DATE-MM.
     MOVE  SYS-DATE(7:2)    TO  HEN-DATE-DD.
* システム時刻取得
     ACCEPT  WK-TIME   FROM  TIME.
* 表示時刻編集
     MOVE  WK-TIME(1:2)     TO  HEN-TIME-HH.
     MOVE  WK-TIME(3:2)     TO  HEN-TIME-MM.
     MOVE  WK-TIME(5:2)     TO  HEN-TIME-SS.
* 画面システム日付・時間編集
     MOVE  HEN-DATE         TO  DSP-SDATE.
     MOVE  HEN-TIME(1:5)    TO  DSP-STIME.

     MOVE  S-NAME-SV        TO  S-NAME.
 SDATE-GET-EXIT.
     EXIT.
****************************************************************
*             画面読込処理
****************************************************************
 DSP-READ-SEC          SECTION.
     MOVE  S-NAME           TO  S-NAME-SV.
     MOVE  "DSP-READ-SEC"   TO  S-NAME.

     MOVE  "NE"             TO  DSP-PRO.

     EVALUATE  PSW
       WHEN  "1"  *> 処理選択
         MOVE  "HEAD"       TO  DSP-GRP

       WHEN  "2"  *> 条件入力
         MOVE  "HEAD2"      TO  DSP-GRP

       WHEN  "3"  *> 明細入力
         MOVE  "BODY"       TO  DSP-GRP

       WHEN  "4"  *> 確認
         MOVE  "KAKU"       TO  DSP-GRP

     END-EVALUATE.

     MOVE  "FMT995"         TO  DSP-FMT.
* 画面読込み
     READ  DSPFILE.
* 入力項目の属性を通常にする
     MOVE  ZERO             TO   ERR-FLG.
     MOVE  SPACE            TO   DSP-PRO.

     MOVE  S-NAME-SV        TO  S-NAME.
 DSP-READ-EXIT.
     EXIT.
****************************************************************
*  処理区分チェック処理
****************************************************************
 HEAD-CHK-SEC          SECTION.
     MOVE  S-NAME           TO  S-NAME-SV.
     MOVE  "HEAD-CHK-SEC"   TO  S-NAME.

     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-SYOKBN.
     MOVE  "M"              TO  EDIT-OPTION OF DSP-SYOKBN.

     IF  DSP-SYOKBN = SPACE
         MOVE  4            TO  ERR-FLG
         MOVE  "C"          TO  EDIT-CURSOR OF DSP-SYOKBN
         MOVE  "R"          TO  EDIT-OPTION OF DSP-SYOKBN
         GO TO  SYOKBN-CHK-END
     END-IF.

     IF  DSP-SYOKBN = 1 OR 2 OR 3
         CONTINUE
     ELSE
         IF  ERR-FLG = ZERO
             MOVE  5        TO  ERR-FLG
         END-IF
         MOVE  "C"          TO  EDIT-CURSOR OF DSP-SYOKBN
         MOVE  "R"          TO  EDIT-OPTION OF DSP-SYOKBN
         GO TO  SYOKBN-CHK-END
     END-IF.

 SYOKBN-CHK-END.
     IF  ERR-FLG NOT =  ZERO
         GO TO  HEAD-CHK-090
     END-IF.

     MOVE  "2"              TO  PSW.
 HEAD-CHK-090.
     MOVE  S-NAME-SV        TO  S-NAME.
 HEAD-CHK-EXIT.
     EXIT.
****************************************************************
*  ヘッド２入力  ( PSW = 2 )
****************************************************************
 DSP-HEAD2-SEC          SECTION.
     MOVE  S-NAME           TO  S-NAME-SV.
     MOVE  "DSP-HEAD2-SEC"  TO  S-NAME.

     PERFORM  DSP-WRITE-SEC.
     PERFORM  DSP-READ-SEC.

     EVALUATE  DSP-FNC
       WHEN  "E000"  *> 実行
         PERFORM  HEAD2-CHK-SEC

       WHEN  "F004"  *> 取消
         PERFORM  INIT-DSP-SEC
         MOVE  "1"          TO  PSW

       WHEN  "F005"  *> 終了
         MOVE  "END"        TO  END-FLG

       WHEN  "F006"  *> 項目戻り
         MOVE  "1"          TO  PSW

       WHEN   OTHER
         MOVE  1            TO  ERR-FLG

     END-EVALUATE.

     MOVE  S-NAME-SV        TO  S-NAME.
 DSP-HEAD2-EXIT.
     EXIT.
****************************************************************
*  ヘッド２入力チェック
****************************************************************
 HEAD2-CHK-SEC         SECTION.
     MOVE  S-NAME           TO  S-NAME-SV.
     MOVE  "HEAD2-CHK-SEC"  TO  S-NAME.

     PERFORM  HEAD2-CHKB-SEC.
     IF  ERR-FLG NOT = ZERO
         GO TO  HEAD2-CHK-090
     END-IF.

     PERFORM  INIT-GMN-MSEDT-SEC.

 HEAD2-CHK-090.
     MOVE  S-NAME-SV        TO  S-NAME.
 HEAD2-CHK-EXIT.
     EXIT.
****************************************************************
*  ヘッド２入力チェックＢ
****************************************************************
 HEAD2-CHKB-SEC        SECTION.
     MOVE  S-NAME           TO  S-NAME-SV.
     MOVE  "HEAD2-CHKB-SEC" TO  S-NAME.

     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-AITSHO.
     MOVE  "M"              TO  EDIT-OPTION OF DSP-AITSHO.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-BASHO.
     MOVE  "M"              TO  EDIT-OPTION OF DSP-BASHO.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-OTNNO.
     MOVE  "M"              TO  EDIT-OPTION OF DSP-OTNNO.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-NTNNO.
     MOVE  "M"              TO  EDIT-OPTION OF DSP-NTNNO.

     PERFORM  VARYING IX-GYO  FROM 1 BY 1
              UNTIL   IX-GYO > 7
       MOVE  SPACE          TO  DSP-GYONO  (IX-GYO)(1:)
       MOVE  SPACE          TO  DSP-TORCD  (IX-GYO)(1:)
       MOVE  SPACE          TO  DSP-TORNM  (IX-GYO)
       MOVE  SPACE          TO  DSP-SHOCD  (IX-GYO)
       MOVE  SPACE          TO  DSP-OTNNOM (IX-GYO)
       MOVE  SPACE          TO  DSP-NTNNOM (IX-GYO)
       MOVE  SPACE          TO  DSP-GTANKA (IX-GYO)(1:)
       MOVE  SPACE          TO  DSP-UTANKA (IX-GYO)(1:)
       MOVE  SPACE          TO  DSP-STANKA (IX-GYO)(1:)

       MOVE  SPACE  TO  EDIT-CURSOR OF DSP-NTNNOM (IX-GYO)
       MOVE  "M"    TO  EDIT-OPTION OF DSP-NTNNOM (IX-GYO)
       MOVE  SPACE  TO  EDIT-CURSOR OF DSP-GTANKA (IX-GYO)
       MOVE  "M"    TO  EDIT-OPTION OF DSP-GTANKA (IX-GYO)
       MOVE  SPACE  TO  EDIT-CURSOR OF DSP-UTANKA (IX-GYO)
       MOVE  "M"    TO  EDIT-OPTION OF DSP-UTANKA (IX-GYO)
       MOVE  SPACE  TO  EDIT-CURSOR OF DSP-STANKA (IX-GYO)
       MOVE  "M"    TO  EDIT-OPTION OF DSP-STANKA (IX-GYO)

       MOVE  SPACE  TO  EDIT-STATUS OF DSP-NTNNOM (IX-GYO)
       MOVE  SPACE  TO  EDIT-STATUS OF DSP-GTANKA (IX-GYO)
       MOVE  SPACE  TO  EDIT-STATUS OF DSP-UTANKA (IX-GYO)
       MOVE  SPACE  TO  EDIT-STATUS OF DSP-STANKA (IX-GYO)
     END-PERFORM.

* 相手先商品ＣＤ
     IF  DSP-AITSHO = SPACE
         IF  ERR-FLG = ZERO
             MOVE  6        TO  ERR-FLG
         END-IF
         MOVE  "C"          TO  EDIT-CURSOR OF DSP-AITSHO
         MOVE  "R"          TO  EDIT-OPTION OF DSP-AITSHO
         GO TO  AITSHO-CHK-END
     END-IF.

     MOVE  1                TO  FG-CHKLVL.
     INITIALIZE  STB-REC.
     MOVE  DSP-AITSHO       TO  STB-F02. *> 量販店商品ＣＤ
     MOVE  LOW-VALUE        TO  FG-HSHOTBL-END.
     PERFORM  RD-HSHOTBL-SEQ-SEC.
     IF FG-HSHOTBL-END = "END"
         IF  ERR-FLG = ZERO
             MOVE  11       TO  ERR-FLG
         END-IF
         MOVE  "C"          TO  EDIT-CURSOR OF DSP-AITSHO
         MOVE  "R"          TO  EDIT-OPTION OF DSP-AITSHO
     END-IF.
 AITSHO-CHK-END.
* 場所
     MOVE  SPACE            TO  DSP-BASHNM.
     IF  DSP-BASHO = SPACE
         IF  ERR-FLG = ZERO
             MOVE  7        TO  ERR-FLG
         END-IF
         MOVE  "C"          TO  EDIT-CURSOR OF DSP-BASHO
         MOVE  "R"          TO  EDIT-OPTION OF DSP-BASHO
         GO TO  BASHO-CHK-END
     END-IF.

     MOVE  DSP-BASHO        TO  SOK-F01.
     PERFORM  RD-ZSOKMS-SEC.
     IF  FG-ZSOKMS-INV = 1
         IF  ERR-FLG = ZERO
             MOVE  8        TO  ERR-FLG
         END-IF
         MOVE  "C"          TO  EDIT-CURSOR OF DSP-BASHO
         MOVE  "R"          TO  EDIT-OPTION OF DSP-BASHO
         GO TO  BASHO-CHK-END
     ELSE
         MOVE  SOK-F02      TO  DSP-BASHNM
     END-IF.

     IF  ERR-FLG NOT = ZERO
         GO TO  BASHO-CHK-END
     END-IF.

     MOVE  ZERO             TO  FG-CHKLVL.
     INITIALIZE  STB-REC.
     MOVE  DSP-AITSHO       TO  STB-F02. *> 量販店商品ＣＤ
     MOVE  DSP-BASHO        TO  STB-F04. *> 出荷場所
     MOVE  DSP-OTNNO        TO  STB-F08. *> _番
     MOVE  LOW-VALUE        TO  FG-HSHOTBL-END.
     PERFORM  RD-HSHOTBL-SEQ-SEC.
     IF FG-HSHOTBL-END = "END"
         IF  ERR-FLG = ZERO
             MOVE  13       TO  ERR-FLG
         END-IF
         MOVE  "C"          TO  EDIT-CURSOR OF DSP-AITSHO
         MOVE  "R"          TO  EDIT-OPTION OF DSP-AITSHO
         MOVE  "R"          TO  EDIT-OPTION OF DSP-BASHO
         MOVE  "R"          TO  EDIT-OPTION OF DSP-OTNNO
     END-IF.

 BASHO-CHK-END.
     IF  ERR-FLG NOT = ZERO
         GO TO  HEAD2-CHKB-090
     END-IF.

     PERFORM  DT-SONZAI-CHK-SEC.
     IF  ERR-FLG NOT = ZERO
         GO TO  HEAD2-CHKB-090
     END-IF.

     MOVE  "3"              TO  PSW.
 HEAD2-CHKB-090.
     MOVE  S-NAME-SV        TO  S-NAME.
 HEAD2-CHKB-EXIT.
     EXIT.
****************************************************************
*    商品変換テーブル読込み　
****************************************************************
 RD-HSHOTBL-SEQ-SEC         SECTION.
     MOVE  S-NAME           TO  S-NAME-SV.
     MOVE  "RD-HSHOTBL-SEQ-SEC" TO  S-NAME.

     IF  FG-HSHOTBL-END = LOW-VALUE
         START  HSHOTBL  KEY >=
                  STB-F02 *> 量販店商品ＣＤ
                  STB-F04 *> 出荷場所
                  STB-F08 *> _番
                  STB-F01 *> 取引先ＣＤ
                  STB-F03 *> 自社商品ＣＤ
           INVALID KEY
              MOVE  "END"   TO  FG-HSHOTBL-END
              GO TO  RD-HSHOTBL-SEQ-090
         END-START

         MOVE  SPACE        TO  FG-HSHOTBL-END

     END-IF.

     READ  HSHOTBL  NEXT
       AT  END
         MOVE  "END"        TO  FG-HSHOTBL-END
         GO TO  RD-HSHOTBL-SEQ-090
     END-READ.

     IF  STB-F02 > DSP-AITSHO  *> 量販店商品ＣＤ
         MOVE  "END"        TO  FG-HSHOTBL-END
         GO TO  RD-HSHOTBL-SEQ-090
     END-IF.

     IF  FG-CHKLVL = 1
         CONTINUE
     ELSE
         IF  STB-F04 > DSP-BASHO  *> 出荷場所
             MOVE  "END"        TO  FG-HSHOTBL-END
             GO TO  RD-HSHOTBL-SEQ-090
         END-IF

         IF  DSP-OTNNO = SPACE *> _番未指定
             CONTINUE
         ELSE                  *> _番指定
             IF  STB-F08 > DSP-OTNNO  *> _番
                 MOVE  "END"    TO  FG-HSHOTBL-END
                 GO TO  RD-HSHOTBL-SEQ-090
             END-IF
         END-IF
     END-IF.

 RD-HSHOTBL-SEQ-090.
     MOVE  S-NAME-SV        TO  S-NAME.
 RD-HSHOTBL-SEQ-EXIT.
     EXIT.
****************************************************************
*    倉庫マスタ検索
****************************************************************
 RD-ZSOKMS-SEC          SECTION.
     MOVE  S-NAME           TO  S-NAME-SV.
     MOVE  "RD-ZSOKMS-SEC"  TO  S-NAME.

     READ  ZSOKMS
       INVALID
         MOVE  1            TO  FG-ZSOKMS-INV
       NOT INVALID
         MOVE  ZERO         TO  FG-ZSOKMS-INV
     END-READ.

     MOVE  S-NAME-SV        TO  S-NAME.

 RD-ZSOKMS-EXIT.
     EXIT.
****************************************************************
*  データ存在チェック
****************************************************************
 DT-SONZAI-CHK-SEC     SECTION.
     MOVE  S-NAME           TO  S-NAME-SV.
     MOVE  "DT-SONZAI-CHK-SEC" TO  S-NAME.
* データ存在チェック
     IF  DSP-SYOKBN = 1  *> 登録
         PERFORM  DT-SONZAI-CHKCR-SEC
     ELSE                *> 修正、削除
         PERFORM  DT-SONZAI-CHKUPDL-SEC
     END-IF.

 HDT-SONZAI-CHK-090.
     MOVE  S-NAME-SV        TO  S-NAME.
 DT-SONZAI-CHK-EXIT.
     EXIT.

****************************************************************
*  データ存在チェック （登録）
****************************************************************
 DT-SONZAI-CHKCR-SEC      SECTION.
     MOVE  S-NAME           TO  S-NAME-SV.
     MOVE  "DT-SONZAI-CHKCR-SEC" TO  S-NAME.

* _卸一括メンテデータ
     INITIALIZE  TNM-REC.
     MOVE  DSP-AITSHO       TO  TNM-F02. *> 量販店商品ＣＤ
     MOVE  DSP-BASHO        TO  TNM-F04. *> 出荷場所
     MOVE  DSP-OTNNO        TO  TNM-F20. *> _番
     MOVE  LOW-VALUE        TO  FG-TANAMTBL-END.
     PERFORM  RD-TANAMTBL-SEQ-SEC.
     IF  DSP-OTNNO NOT = SPACE
         *> 旧_番号指定の場合のみチェック
         IF  FG-TANAMTBL-END NOT = "END"
             IF  ERR-FLG = ZERO
                 MOVE  9    TO  ERR-FLG
             END-IF
             MOVE  "C"      TO  EDIT-CURSOR OF DSP-AITSHO
             MOVE  "R"      TO  EDIT-OPTION OF DSP-AITSHO
             MOVE  "R"      TO  EDIT-OPTION OF DSP-BASHO
             MOVE  "R"      TO  EDIT-OPTION OF DSP-OTNNO
             GO TO  DT-SONZAI-CHKCR-090
          END-IF
     END-IF.

 DT-SONZAI-CHKCR-090.
     MOVE  S-NAME-SV        TO  S-NAME.
 DT-SONZAI-CHKCR-EXIT.
     EXIT.
****************************************************************
*    _卸一括メンテデータ読込み　
****************************************************************
 RD-TANAMTBL-SEQ-SEC        SECTION.
     MOVE  S-NAME           TO  S-NAME-SV.
     MOVE  "RD-TANAMTB-SEQL-SEC" TO  S-NAME.

     IF  FG-TANAMTBL-END = LOW-VALUE
         START  TANAMTBL  KEY >=
                  TNM-F02 *> 量販店商品ＣＤ
                  TNM-F04 *> 出荷場所
                  TNM-F20 *> 旧_番
                  TNM-F01 *> 取引先ＣＤ
                  TNM-F03 *> 自社商品ＣＤ
           INVALID KEY
              MOVE  "END"   TO  FG-TANAMTBL-END
              GO TO  RD-TANAMTBL-SEQ-090
         END-START

         MOVE  SPACE        TO  FG-TANAMTBL-END

     END-IF.

     READ  TANAMTBL  NEXT
       AT  END
         MOVE  "END"        TO  FG-TANAMTBL-END
         GO TO  RD-TANAMTBL-SEQ-090
     END-READ.

     IF  TNM-F02 > DSP-AITSHO  *> 量販店商品ＣＤ
         MOVE  "END"        TO  FG-TANAMTBL-END
         GO TO  RD-TANAMTBL-SEQ-090
     END-IF.

     IF  TNM-F04 > DSP-BASHO  *> 出荷場所
         MOVE  "END"        TO  FG-TANAMTBL-END
         GO TO  RD-TANAMTBL-SEQ-090
     END-IF.

     IF  DSP-OTNNO = SPACE *> _番未指定
         CONTINUE
     ELSE                  *> _番指定
         IF  TNM-F20 > DSP-OTNNO  *> _番
             MOVE  "END"    TO  FG-TANAMTBL-END
             GO TO  RD-TANAMTBL-SEQ-090
         END-IF
     END-IF.

 RD-TANAMTBL-SEQ-090.
     MOVE  S-NAME-SV        TO  S-NAME.
 RD-TANAMTBL-SEQ-EXIT.
     EXIT.
****************************************************************
*  データ存在チェック （修正/削除）
****************************************************************
 DT-SONZAI-CHKUPDL-SEC     SECTION.
     MOVE  S-NAME           TO  S-NAME-SV.
     MOVE  "DT-SONZAI-CHKUPDL-SEC" TO  S-NAME.

* _卸一括メンテデータ
     INITIALIZE  TNM-REC.
     MOVE  DSP-AITSHO       TO  TNM-F02. *> 量販店商品ＣＤ
     MOVE  DSP-BASHO        TO  TNM-F04. *> 出荷場所
     MOVE  DSP-OTNNO        TO  TNM-F20. *> _番
     MOVE  LOW-VALUE        TO  FG-TANAMTBL-END.
     PERFORM  RD-TANAMTBL-SEQ-SEC.
     IF FG-TANAMTBL-END = "END"
       IF  ERR-FLG = ZERO
           MOVE  10         TO  ERR-FLG
       END-IF
       MOVE  "C"            TO  EDIT-CURSOR OF DSP-AITSHO
       MOVE  "R"            TO  EDIT-OPTION OF DSP-AITSHO
       MOVE  "R"            TO  EDIT-OPTION OF DSP-BASHO
       MOVE  "R"            TO  EDIT-OPTION OF DSP-OTNNO
     END-IF.

 DT-SONZAI-CHKUPDL-090.
     MOVE  S-NAME-SV        TO  S-NAME.
 DT-SONZAI-CHKUPDL-EXIT.
     EXIT.
****************************************************************
*  明細初画面編集
****************************************************************
 INIT-GMN-MSEDT-SEC    SECTION.
     MOVE  S-NAME           TO  S-NAME-SV.
     MOVE  "INIT-GMN-MSEDT-SEC"  TO  S-NAME.

     PERFORM  TBL-SET-SEC.

     MOVE  1                TO  CT-PG.
     PERFORM  GMN-MSEDT-SEC.

 INIT-GMN-MSEDT-090.
     MOVE  S-NAME-SV        TO  S-NAME.
 INIT-GMN-MSEDT-EXIT.
     EXIT.

****************************************************************
*  データＴＢＬ格納
****************************************************************
 TBL-SET-SEC    SECTION.
     MOVE  S-NAME           TO  S-NAME-SV.
     MOVE  "TBL-SET-SEC"  TO  S-NAME.

     INITIALIZE  TB-AREA.
     MOVE  ZERO             TO  IX-TBL.
     MOVE  ZERO             TO  FG-TBLOVL.

     IF  DSP-SYOKBN = 1  *> 登録
         PERFORM  TBL-SETCR-SEC
     ELSE                *> 修正、削除
         PERFORM  TBL-SETUPDL-SEC
     END-IF.

* 格納最大ページの計算
     MOVE  ZERO             TO  CT-PGMAX.
     MOVE  ZERO             TO  IX-GYOMAX.
     DIVIDE  7  INTO IX-TBLMAX
       GIVING    CT-PGMAX
       REMAINDER IX-GYOMAX.

     IF  IX-GYOMAX = ZERO
         MOVE 7             TO  IX-GYOMAX
     ELSE
         ADD  1   TO  CT-PGMAX
     END-IF.


 TBL-SET-090.
     MOVE  S-NAME-SV        TO  S-NAME.
 TBL-SET-EXIT.
     EXIT.
****************************************************************
*  データＴＢＬ格納（登録）
****************************************************************
 TBL-SETCR-SEC         SECTION.
     MOVE  S-NAME           TO  S-NAME-SV.
     MOVE  "TBL-SETCRB-SEC" TO  S-NAME.

     PERFORM  UNTIL FG-HSHOTBL-END = "END"
       PERFORM  TBL-SETCRB-SEC
       PERFORM  RD-HSHOTBL-SEQ-SEC

     END-PERFORM.

 TBL-SETCR-090.
     MOVE  S-NAME-SV        TO  S-NAME.
 TBL-SETCR-EXIT.
     EXIT.
****************************************************************
*  データＴＢＬ格納（登録）Ｂ
****************************************************************
 TBL-SETCRB-SEC        SECTION.
     MOVE  S-NAME           TO  S-NAME-SV.
     MOVE  "TBL-SETCRB-SEC" TO  S-NAME.

     IF  IX-TBL >= 140
         MOVE  1            TO  FG-TBLOVL
         GO TO  TBL-SETCRB-090
     END-IF.

     MOVE  DSP-AITSHO       TO  TNM-F02.  *>量販店商品ＣＤ
     MOVE  DSP-BASHO        TO  TNM-F04.  *>出荷場所
     MOVE  STB-F08          TO  TNM-F20   *>旧_番
     MOVE  STB-F01          TO  TNM-F01.  *>取引先ＣＤ
     MOVE  STB-F031         TO  TNM-F031. *>自社商品ＣＤ
     MOVE  STB-F032         TO  TNM-F032. *>自社品単ＣＤ
     PERFORM  RD-TANAMTBL-SEC.

     ADD  1   TO  IX-TBL.
     MOVE  IX-TBL           TO  TB-GYO-R            (IX-TBL).
     MOVE  STB-F01          TO  TB-TORCD-R          (IX-TBL).
     MOVE  STB-F031         TO  TB-SHOCD-R          (IX-TBL).
     MOVE  STB-F032         TO  TB-HINTAN-R         (IX-TBL).
     MOVE  STB-F07          TO  TB-BNRCD-R          (IX-TBL).
     MOVE  STB-F10          TO  TB-YOBI1-R          (IX-TBL).
     MOVE  STB-F11          TO  TB-KENPNG-R         (IX-TBL).
     MOVE  STB-F12          TO  TB-SHK-AREA-KBN-R   (IX-TBL).
     MOVE  STB-F13          TO  TB-REG-TANCD-R      (IX-TBL).
     MOVE  STB-F14          TO  TB-LAST-UPD-TANCD-R (IX-TBL).
     MOVE  STB-F98          TO  TB-REG-DATE-R       (IX-TBL).
     MOVE  STB-F08          TO  TB-OTNNO-R          (IX-TBL).
     MOVE  ZERO             TO  TB-UPD-DATE-R       (IX-TBL).

     IF FG-TANAMTBL-INV = 1
        MOVE  STB-F05       TO  TB-GTANKA-R         (IX-TBL)
        MOVE  STB-F06       TO  TB-UTANKA-R         (IX-TBL)
        MOVE  SPACE         TO  TB-NTNNO-R          (IX-TBL)
        MOVE  STB-F09       TO  TB-STANKA-R         (IX-TBL)

        MOVE  STB-F05       TO  TB-MAE-GTANKA-R     (IX-TBL)
        MOVE  STB-F06       TO  TB-MAE-UTANKA-R     (IX-TBL)
        MOVE  SPACE         TO  TB-MAE-NTNNO-R      (IX-TBL)
        MOVE  STB-F09       TO  TB-MAE-STANKA-R     (IX-TBL)
     ELSE
        MOVE  TNM-F05       TO  TB-GTANKA-R         (IX-TBL)
        MOVE  TNM-F06       TO  TB-UTANKA-R         (IX-TBL)
        MOVE  TNM-F08       TO  TB-NTNNO-R          (IX-TBL)
        MOVE  TNM-F09       TO  TB-STANKA-R         (IX-TBL)

        MOVE  TNM-F05       TO  TB-MAE-GTANKA-R     (IX-TBL)
        MOVE  TNM-F06       TO  TB-MAE-UTANKA-R     (IX-TBL)
        MOVE  TNM-F08       TO  TB-MAE-NTNNO-R      (IX-TBL)
        MOVE  TNM-F09       TO  TB-MAE-STANKA-R     (IX-TBL)
     END-IF.

* 最大格納位置
     MOVE  IX-TBL           TO  IX-TBLMAX.

 TBL-SETCRB-090.
     MOVE  S-NAME-SV        TO  S-NAME.
 TBL-SETCRB-EXIT.
     EXIT.
****************************************************************
*  _卸メンテデータ読込み
****************************************************************
 RD-TANAMTBL-SEC       SECTION.
     MOVE  S-NAME           TO  S-NAME-SV.
     MOVE  "RD-TANAMTBL-SEC"  TO  S-NAME.

     READ  TANAMTBL  KEY IS TNM-F02 *> 量販店商品ＣＤ
                            TNM-F04 *> 出荷場所
                            TNM-F20 *> 旧_番
                            TNM-F01 *> 取引先ＣＤ
                            TNM-F03 *> 自社商品ＣＤ
       INVALID
         MOVE  1                 TO  FG-TANAMTBL-INV
       NOT INVALID
         MOVE  ZERO              TO  FG-TANAMTBL-INV
     END-READ.

 RD-TANAMTBL-090.
     MOVE  S-NAME-SV        TO  S-NAME.
 RD-TANAMTBL-EXIT.
     EXIT.
****************************************************************
*  データＴＢＬ格納 （修正/削除）
****************************************************************
 TBL-SETUPDL-SEC       SECTION.
     MOVE  S-NAME           TO  S-NAME-SV.
     MOVE  "TBL-SETUPDL-SEC"  TO  S-NAME.

     PERFORM  UNTIL FG-TANAMTBL-END = "END"
       PERFORM  TBL-SETUPDLB-SEC
       PERFORM  RD-TANAMTBL-SEQ-SEC

     END-PERFORM.

 TBL-SETUPDL-090.
     MOVE  S-NAME-SV        TO  S-NAME.
 TBL-SETUPDL-EXIT.
     EXIT.
****************************************************************
*  データＴＢＬ格納（修正/削除）Ｂ
****************************************************************
 TBL-SETUPDLB-SEC        SECTION.
     MOVE  S-NAME           TO  S-NAME-SV.
     MOVE  "TBL-SETUPDLB-SEC" TO  S-NAME.

     IF  IX-TBL >= 140
         MOVE  1            TO  FG-TBLOVL
         GO TO  TBL-SETUPDLB-090
     END-IF.

     ADD  1   TO  IX-TBL.
     MOVE  IX-TBL           TO  TB-GYO-R            (IX-TBL).
     MOVE  TNM-F01          TO  TB-TORCD-R          (IX-TBL).
     MOVE  TNM-F031         TO  TB-SHOCD-R          (IX-TBL).
     MOVE  TNM-F032         TO  TB-HINTAN-R         (IX-TBL).
     MOVE  TNM-F05          TO  TB-GTANKA-R         (IX-TBL).
     MOVE  TNM-F06          TO  TB-UTANKA-R         (IX-TBL).
     MOVE  TNM-F07          TO  TB-BNRCD-R          (IX-TBL).
     MOVE  TNM-F08          TO  TB-NTNNO-R          (IX-TBL).
     MOVE  TNM-F09          TO  TB-STANKA-R         (IX-TBL).
     MOVE  TNM-F10          TO  TB-YOBI1-R          (IX-TBL).
     MOVE  TNM-F11          TO  TB-KENPNG-R         (IX-TBL).
     MOVE  TNM-F12          TO  TB-SHK-AREA-KBN-R   (IX-TBL).
     MOVE  TNM-F13          TO  TB-REG-TANCD-R      (IX-TBL).
     MOVE  TNM-F14          TO  TB-LAST-UPD-TANCD-R (IX-TBL).
     MOVE  TNM-F98          TO  TB-REG-DATE-R       (IX-TBL).
     MOVE  TNM-F20          TO  TB-OTNNO-R          (IX-TBL).
     MOVE  TNM-F99          TO  TB-UPD-DATE-R       (IX-TBL).
     MOVE  TNM-F05          TO  TB-MAE-GTANKA-R     (IX-TBL).
     MOVE  TNM-F06          TO  TB-MAE-UTANKA-R     (IX-TBL).
     MOVE  TNM-F08          TO  TB-MAE-NTNNO-R      (IX-TBL).
     MOVE  TNM-F09          TO  TB-MAE-STANKA-R     (IX-TBL).
* 最大格納位置
     MOVE  IX-TBL           TO  IX-TBLMAX.

 TBL-SETUPDLB-090.
     MOVE  S-NAME-SV        TO  S-NAME.
 TBL-SETUPDLB-EXIT.
     EXIT.
****************************************************************
*  明細画面編集
****************************************************************
 GMN-MSEDT-SEC         SECTION.
     MOVE  S-NAME           TO  S-NAME-SV.
     MOVE  "GMN-MSEDT-SEC"  TO  S-NAME.

     PERFORM  VARYING IX-GYO  FROM 1 BY 1
              UNTIL   IX-GYO > 7
       PERFORM  GMN-MSEDTB-SEC

     END-PERFORM.

* 確認にカーソル指定
     IF  DSP-SYOKBN NOT = 3 *> 登録/修正
         MOVE "X"   TO  EDIT-STATUS OF DSP-KKNN
     ELSE                   *> 削除
         MOVE SPACE TO  EDIT-CURSOR OF DSP-KKNN
         MOVE "M"   TO  EDIT-OPTION OF DSP-KKNN
         MOVE SPACE TO  EDIT-STATUS OF DSP-KKNN
     END-IF.

 GMN-MSEDT-090.
     MOVE  S-NAME-SV        TO  S-NAME.
 GMN-MSEDT-EXIT.
     EXIT.
****************************************************************
*  明細画面編集Ｂ
****************************************************************
 GMN-MSEDTB-SEC         SECTION.
     MOVE  S-NAME           TO  S-NAME-SV.
     MOVE  "GMN-MSEDTB-SEC" TO  S-NAME.

     IF      CT-PG  = CT-PGMAX
         AND IX-GYO > IX-GYOMAX
         MOVE  SPACE        TO  DSP-GYONO  (IX-GYO)(1:)
         MOVE  SPACE        TO  DSP-TORCD  (IX-GYO)(1:)
         MOVE  SPACE        TO  DSP-TORNM  (IX-GYO)
         MOVE  SPACE        TO  DSP-SHOCD  (IX-GYO)
         MOVE  SPACE        TO  DSP-OTNNOM (IX-GYO)
         MOVE  SPACE        TO  DSP-NTNNOM (IX-GYO)
         MOVE  SPACE        TO  DSP-GTANKA (IX-GYO)(1:)
         MOVE  SPACE        TO  DSP-UTANKA (IX-GYO)(1:)
         MOVE  SPACE        TO  DSP-STANKA (IX-GYO)(1:)

         MOVE SPACE TO  EDIT-CURSOR OF DSP-NTNNOM (IX-GYO)
         MOVE "M"   TO  EDIT-OPTION OF DSP-NTNNOM (IX-GYO)
         MOVE SPACE TO  EDIT-CURSOR OF DSP-GTANKA (IX-GYO)
         MOVE "M"   TO  EDIT-OPTION OF DSP-GTANKA (IX-GYO)
         MOVE SPACE TO  EDIT-CURSOR OF DSP-UTANKA (IX-GYO)
         MOVE "M"   TO  EDIT-OPTION OF DSP-UTANKA (IX-GYO)
         MOVE SPACE TO  EDIT-CURSOR OF DSP-STANKA (IX-GYO)
         MOVE "M"   TO  EDIT-OPTION OF DSP-STANKA (IX-GYO)

         MOVE "X"   TO  EDIT-STATUS OF DSP-NTNNOM (IX-GYO)
         MOVE "X"   TO  EDIT-STATUS OF DSP-GTANKA (IX-GYO)
         MOVE "X"   TO  EDIT-STATUS OF DSP-UTANKA (IX-GYO)
         MOVE "X"   TO  EDIT-STATUS OF DSP-STANKA (IX-GYO)
         GO TO  GMN-MSEDTB-090
     END-IF.

* 取引先マスタ
     MOVE  TB-TORCD  (CT-PG IX-GYO)  TO  TOK-F01.
     PERFORM  RD-HTOKMS-SEC.

     MOVE  TB-GYO    (CT-PG IX-GYO)  TO  DSP-GYONO (IX-GYO).
     MOVE  TB-TORCD  (CT-PG IX-GYO)  TO  DSP-TORCD (IX-GYO).

     IF  FG-HTOKMS-INV = ZERO
         MOVE  TOK-F02      TO  DSP-TORNM  (IX-GYO)
     ELSE
         MOVE  SPACE        TO  DSP-TORNM  (IX-GYO)
     END-IF.

     MOVE  TB-SHOCD  (CT-PG IX-GYO)  TO  DSP-SHOCD  (IX-GYO).
     MOVE  TB-OTNNO  (CT-PG IX-GYO)  TO  DSP-OTNNOM (IX-GYO).
     MOVE  TB-NTNNO  (CT-PG IX-GYO)  TO  DSP-NTNNOM (IX-GYO).

     IF      TB-GTANKA (CT-PG IX-GYO) = ZERO
         AND TB-UTANKA (CT-PG IX-GYO) = ZERO
         AND TB-STANKA (CT-PG IX-GYO) = ZERO
         MOVE  SPACE        TO  DSP-GTANKA (IX-GYO)(1:)
         MOVE  SPACE        TO  DSP-UTANKA (IX-GYO)(1:)
         MOVE  SPACE        TO  DSP-STANKA (IX-GYO)(1:)
     ELSE
         MOVE  TB-GTANKA (CT-PG IX-GYO)  TO  DSP-GTANKA (IX-GYO)
         MOVE  TB-UTANKA (CT-PG IX-GYO)  TO  DSP-UTANKA (IX-GYO)
         MOVE  TB-STANKA (CT-PG IX-GYO)  TO  DSP-STANKA (IX-GYO)
     END-IF.

     MOVE SPACE TO  EDIT-CURSOR OF DSP-NTNNOM (IX-GYO).
     MOVE "M"   TO  EDIT-OPTION OF DSP-NTNNOM (IX-GYO).
     MOVE SPACE TO  EDIT-CURSOR OF DSP-GTANKA (IX-GYO).
     MOVE "M"   TO  EDIT-OPTION OF DSP-GTANKA (IX-GYO).
     MOVE SPACE TO  EDIT-CURSOR OF DSP-UTANKA (IX-GYO).
     MOVE "M"   TO  EDIT-OPTION OF DSP-UTANKA (IX-GYO).
     MOVE SPACE TO  EDIT-CURSOR OF DSP-STANKA (IX-GYO).
     MOVE "M"   TO  EDIT-OPTION OF DSP-STANKA (IX-GYO).

     IF  DSP-SYOKBN = 3 *> 削除（入力項目プロテクト）
         MOVE "X"   TO  EDIT-STATUS OF DSP-NTNNOM (IX-GYO)
         MOVE "X"   TO  EDIT-STATUS OF DSP-GTANKA (IX-GYO)
         MOVE "X"   TO  EDIT-STATUS OF DSP-UTANKA (IX-GYO)
         MOVE "X"   TO  EDIT-STATUS OF DSP-STANKA (IX-GYO)
     ELSE               *> 登録/修正
         MOVE SPACE TO  EDIT-STATUS OF DSP-NTNNOM (IX-GYO)
         MOVE SPACE TO  EDIT-STATUS OF DSP-GTANKA (IX-GYO)
         MOVE SPACE TO  EDIT-STATUS OF DSP-UTANKA (IX-GYO)
         MOVE SPACE TO  EDIT-STATUS OF DSP-STANKA (IX-GYO)
     END-IF.

 GMN-MSEDTB-090.
     MOVE  S-NAME-SV        TO  S-NAME.
 GMN-MSEDTB-EXIT.
     EXIT.
****************************************************************
*    取引先マスタ検索
****************************************************************
 RD-HTOKMS-SEC          SECTION.
     MOVE  S-NAME           TO  S-NAME-SV.
     MOVE  "RD-HTOKMS-SEC"  TO  S-NAME.

     READ  HTOKMS
       INVALID
         MOVE  1            TO  FG-HTOKMS-INV
       NOT INVALID
         MOVE  ZERO         TO  FG-HTOKMS-INV
     END-READ.

     MOVE  S-NAME-SV        TO  S-NAME.
 RD-HTOKMS-EXIT.
     EXIT.
****************************************************************
*  明細入力 (PSW = 3)
****************************************************************
 DSP-BODY-SEC          SECTION.
     MOVE  S-NAME           TO  S-NAME-SV.
     MOVE  "DSP-BODY-SEC"   TO  S-NAME.

     PERFORM  DSP-WRITE-SEC.
     PERFORM  DSP-READ-SEC.

     EVALUATE  DSP-FNC

       WHEN  "E000" *> 実行
         PERFORM  BODY-CHK-SEC
         IF  ERR-FLG = ZERO
             MOVE SPACE TO  EDIT-STATUS OF DSP-KKNN
             MOVE  15       TO  ERR-FLG
             MOVE  "4"      TO  PSW
         END-IF

       WHEN  "F004" *> 取消
         PERFORM  INIT-DSP-SEC
         MOVE "2"           TO  PSW

       WHEN  "F006" *> 項目戻り
         MOVE "2"           TO  PSW

       WHEN  "F011" *> 前頁
         PERFORM  BWD-PAGE-SEC

       WHEN   "F012" *> 次頁
         PERFORM  FWD-PAGE-SEC

       WHEN  OTHER
         MOVE  1            TO  ERR-FLG

     END-EVALUATE.

     MOVE  S-NAME-SV        TO  S-NAME.
 DSP-BODY-EXIT.
     EXIT.
****************************************************************
*  明細部チェック
****************************************************************
 BODY-CHK-SEC             SECTION.
     MOVE  S-NAME           TO  S-NAME-SV.
     MOVE  "BODY-CHK-SEC"   TO  S-NAME.

     PERFORM  BODY-CHKB-SEC.
     IF  ERR-FLG NOT = ZERO
         GO TO  BODY-CHK-090
     END-IF.

     PERFORM  DSP-TBL-SET-SEC.

 BODY-CHK-090.
     MOVE  S-NAME-SV        TO  S-NAME.
 BODY-CHK-EXIT.
     EXIT.
****************************************************************
*  明細部チェックＢ
****************************************************************
 BODY-CHKB-SEC             SECTION.
     MOVE  S-NAME           TO  S-NAME-SV.
     MOVE  "BODY-CHKB-SEC"  TO  S-NAME.

     PERFORM  VARYING  IX-GYO  FROM 1 BY 1
              UNTIL    IX-GYO > 7
       PERFORM  BODY-CHKC-SEC

     END-PERFORM.

 BODY-CHK-090.
     MOVE  S-NAME-SV        TO  S-NAME.
 BODY-CHK-EXIT.
     EXIT.
****************************************************************
*  明細部チェックＣ
****************************************************************
 BODY-CHKC-SEC             SECTION.
     MOVE  S-NAME           TO  S-NAME-SV.
     MOVE  "BODY-CHKC-SEC"  TO  S-NAME.

     IF      CT-PG  = CT-PGMAX
         AND IX-GYO > IX-GYOMAX *> 最終ページの終了行以降
         GO TO  BODY-CHKC-090
     END-IF.

     MOVE  SPACE  TO  EDIT-CURSOR OF DSP-NTNNOM (IX-GYO)
     MOVE  "M"    TO  EDIT-OPTION OF DSP-NTNNOM (IX-GYO).
     MOVE  SPACE  TO  EDIT-CURSOR OF DSP-GTANKA (IX-GYO)
     MOVE  "M"    TO  EDIT-OPTION OF DSP-GTANKA (IX-GYO).
     MOVE  SPACE  TO  EDIT-CURSOR OF DSP-UTANKA (IX-GYO)
     MOVE  "M"    TO  EDIT-OPTION OF DSP-UTANKA (IX-GYO).
     MOVE  SPACE  TO  EDIT-CURSOR OF DSP-STANKA (IX-GYO)
     MOVE  "M"    TO  EDIT-OPTION OF DSP-STANKA (IX-GYO).

     IF      DSP-GTANKA (IX-GYO) = ZERO
         AND DSP-UTANKA (IX-GYO) = ZERO
         AND DSP-STANKA (IX-GYO) = ZERO *> 未入力行
         MOVE  SPACE        TO  DSP-GTANKA (IX-GYO)(1:)
         MOVE  SPACE        TO  DSP-UTANKA (IX-GYO)(1:)
         MOVE  SPACE        TO  DSP-STANKA (IX-GYO)(1:)
         GO TO  BODY-CHKC-090
     END-IF.


     IF  DSP-GTANKA (IX-GYO) NOT NUMERIC
         MOVE  ZERO         TO  DSP-GTANKA (IX-GYO)
     END-IF.

     IF  DSP-UTANKA (IX-GYO) NOT NUMERIC
         MOVE  ZERO         TO  DSP-UTANKA (IX-GYO)
     END-IF.

     IF  DSP-STANKA (IX-GYO) NOT NUMERIC
         MOVE  ZERO         TO  DSP-STANKA (IX-GYO)
     END-IF.

* 仕入単価 ≦ 原価単価 ≦ 売価単価でなければならない。
     IF      DSP-STANKA (IX-GYO) <= DSP-GTANKA (IX-GYO)
         AND DSP-GTANKA (IX-GYO) <= DSP-UTANKA (IX-GYO)
         CONTINUE
     ELSE
         IF  ERR-FLG = ZERO
             MOVE  12        TO  ERR-FLG
         END-IF
         MOVE  "C"  TO  EDIT-CURSOR OF DSP-GTANKA (IX-GYO)
         MOVE  "R"  TO  EDIT-OPTION OF DSP-GTANKA (IX-GYO)
         MOVE  "R"  TO  EDIT-OPTION OF DSP-UTANKA (IX-GYO)
         MOVE  "R"  TO  EDIT-OPTION OF DSP-STANKA (IX-GYO)
     END-IF.

 BODY-CHKC-090.
     MOVE  S-NAME-SV        TO  S-NAME.
 BODY-CHKC-EXIT.
     EXIT.
****************************************************************
*  画面 -> ＴＢＬ編集
****************************************************************
 DSP-TBL-SET-SEC       SECTION.
     MOVE  S-NAME           TO  S-NAME-SV.
     MOVE  "DSP-TBL-SET-SEC"  TO  S-NAME.

     PERFORM  VARYING IX-GYO  FROM 1 BY 1
              UNTIL   IX-GYO > 7
       IF      CT-PG  = CT-PGMAX
           AND IX-GYO > IX-GYOMAX *> 最終ページの終了行以降
           MOVE  7          TO  IX-GYO
       ELSE
           MOVE  DSP-GTANKA (IX-GYO) TO
                             TB-GTANKA (CT-PG IX-GYO)
           MOVE  DSP-UTANKA (IX-GYO) TO
                             TB-UTANKA (CT-PG IX-GYO)
           MOVE  DSP-NTNNOM (IX-GYO) TO
                             TB-NTNNO  (CT-PG IX-GYO)
           MOVE  DSP-STANKA (IX-GYO) TO
                             TB-STANKA (CT-PG IX-GYO)
       END-IF

     END-PERFORM.

 DSP-TBL-SET-090.
     MOVE  S-NAME-SV        TO  S-NAME.
 DSP-TBL-SET-EXIT.
     EXIT.
****************************************************************
*  前頁画面処理
****************************************************************
 BWD-PAGE-SEC          SECTION.
     MOVE  S-NAME           TO  S-NAME-SV.
     MOVE  "BWD-PAGE-SEC"   TO  S-NAME.

     PERFORM  BODY-CHK-SEC.
     IF ERR-FLG NOT = ZERO
        GO TO  BWD-PAGE-090
     END-IF.

     IF  CT-PG = 1
         IF  ERR-FLG = ZERO
             MOVE  2        TO  ERR-FLG
         END-IF
         GO TO  BWD-PAGE-090
     END-IF.

     COMPUTE  CT-PG = CT-PG - 1.
     PERFORM  GMN-MSEDT-SEC.

 BWD-PAGE-090.
     MOVE  S-NAME-SV        TO  S-NAME.
 BWD-PAGE-EXIT.
     EXIT.
****************************************************************
*  次頁画面処理
****************************************************************
 FWD-PAGE-SEC          SECTION.
     MOVE  S-NAME           TO  S-NAME-SV.
     MOVE  "FWD-PAGE-SEC"   TO  S-NAME.

     PERFORM  BODY-CHK-SEC.
     IF ERR-FLG NOT = ZERO
        GO TO  FWD-PAGE-090
     END-IF.

     IF  CT-PG + 1 > CT-PGMAX
         IF  FG-TBLOVL = ZERO *> 通常
             IF  ERR-FLG = ZERO
                 MOVE  3    TO  ERR-FLG
             END-IF
         ELSE                 *> ＴＢＬオーバー
             IF  ERR-FLG = ZERO
                 MOVE  14   TO  ERR-FLG
             END-IF
         END-IF
         GO TO  BWD-PAGE-090
     END-IF.

     COMPUTE  CT-PG = CT-PG + 1.
     PERFORM  GMN-MSEDT-SEC.

 FWD-PAGE-090.
     MOVE  S-NAME-SV        TO  S-NAME.
 FWD-PAGE-EXIT.
     EXIT.
****************************************************************
*  確認処理入力（ PSW = 4 ）
****************************************************************
 DSP-KAKU-SEC          SECTION.
     MOVE  S-NAME           TO  S-NAME-SV.
     MOVE  "DSP-KAKU-SEC"   TO  S-NAME.

     PERFORM  DSP-WRITE-SEC.
     PERFORM  DSP-READ-SEC.

     EVALUATE   DSP-FNC

       WHEN  "E000" *>実行
         PERFORM  DT-KOSIN-SEC
         PERFORM  INIT-DSP-SEC
         MOVE  "2"          TO  PSW

       WHEN  "F004" *>取消
         PERFORM  INIT-DSP-SEC
         MOVE  "2"          TO  PSW

       WHEN  "F005" *>終了
         MOVE  "END"        TO  END-FLG

       WHEN  "F006" *>項目戻り
         IF  DSP-SYOKBN = 3 *> 削除
             MOVE  "2"      TO  PSW
         ELSE                *> 登録/修正
             MOVE  "3"      TO  PSW
         END-IF

       WHEN   OTHER
         MOVE  1            TO  ERR-FLG

     END-EVALUATE.

     MOVE  S-NAME-SV        TO  S-NAME.
 DSP-KAKU-EXIT.
     EXIT.
****************************************************************
*    データ更新処理
****************************************************************
 DT-KOSIN-SEC               SECTION.
     MOVE  S-NAME           TO  S-NAME-SV.
     MOVE  "DT-KOSIN-SEC"   TO  S-NAME.

     PERFORM  SDATE-GET-SEC.

* 前データを全て削除
     PERFORM  DT-KOSIN-DL-SEC.
     IF  DSP-SYOKBN NOT = 3  *> 登録、修正データを作成
         PERFORM  DT-KOSIN-CR-SEC
     END-IF.

     MOVE  S-NAME-SV        TO  S-NAME.
 DT-KOSIN-EXIT.
     EXIT.
****************************************************************
*  データ更新処理（削除）
****************************************************************
 DT-KOSIN-DL-SEC            SECTION.
     MOVE  S-NAME           TO  S-NAME-SV.
     MOVE  "DT-KOSIN-CRUP-SEC"  TO  S-NAME.

* _卸一括メンテデータ
     INITIALIZE  TNM-REC.
     MOVE  DSP-AITSHO       TO  TNM-F02. *> 量販店商品ＣＤ
     MOVE  DSP-BASHO        TO  TNM-F04. *> 出荷場所
     MOVE  DSP-OTNNO        TO  TNM-F08. *> _番
     MOVE  LOW-VALUE        TO  FG-TANAMTBL-END.
     PERFORM  RD-TANAMTBL-SEQ-SEC.

     PERFORM  UNTIL FG-TANAMTBL-END = "END"

       DELETE  TANAMTBL
       IF  DSP-SYOKBN = 3  *> 削除時
           ADD  1   TO  CT-DLT
       END-IF
       PERFORM  RD-TANAMTBL-SEQ-SEC

     END-PERFORM.

     MOVE  S-NAME-SV        TO  S-NAME.
 DT-KOSIN-DL-EXIT.
     EXIT.
****************************************************************
*  データ更新処理（登録）
****************************************************************
 DT-KOSIN-CR-SEC           SECTION.
     MOVE  S-NAME           TO  S-NAME-SV.
     MOVE  "DT-KOSIN-CR-SEC"  TO  S-NAME.

     PERFORM  VARYING IX-TBL  FROM 1 BY 1
              UNTIL   IX-TBL > IX-TBLMAX
       PERFORM  DT-KOSIN-CRB-SEC

     END-PERFORM.

     MOVE  S-NAME-SV        TO  S-NAME.
 DT-KOSIN-CR-EXIT.
     EXIT.
****************************************************************
*  データ更新Ｂ処理（登録）
****************************************************************
 DT-KOSIN-CRB-SEC         SECTION.
     MOVE  S-NAME           TO  S-NAME-SV.
     MOVE  "DT-KOSIN-CRB-SEC"  TO  S-NAME.

     MOVE  DSP-AITSHO            TO  TNM-F02.  *>量販店商品ＣＤ
     MOVE  DSP-BASHO             TO  TNM-F04.  *>出荷場所
     MOVE  TB-OTNNO-R  (IX-TBL)  TO  TNM-F20.  *>旧_番
     MOVE  TB-TORCD-R  (IX-TBL)  TO  TNM-F01.  *>取引先ＣＤ
     MOVE  TB-SHOCD-R  (IX-TBL)  TO  TNM-F031. *>自社商品ＣＤ
     MOVE  TB-HINTAN-R (IX-TBL)  TO  TNM-F032. *>自社品単
     PERFORM  RD-TANAMTBL-SEC.

     INITIALIZE TNM-REC.
     MOVE  TB-TORCD-R  (IX-TBL)  TO  TNM-F01.  *>取引先ＣＤ
     MOVE  DSP-AITSHO            TO  TNM-F02.  *>量販店商品ＣＤ
     MOVE  TB-SHOCD-R  (IX-TBL)  TO  TNM-F031. *>商品ＣＤ
     MOVE  TB-HINTAN-R (IX-TBL)  TO  TNM-F032. *>品単ＣＤ
     MOVE  DSP-BASHO             TO  TNM-F04.  *>出荷場所
     MOVE  TB-GTANKA-R (IX-TBL)  TO  TNM-F05.  *>原価単価
     MOVE  TB-UTANKA-R (IX-TBL)  TO  TNM-F06.  *>売価単価
     MOVE  TB-BNRCD-R  (IX-TBL)  TO  TNM-F07.  *>分類ＣＤ

     IF TB-NTNNO-R  (IX-TBL) NOT = SPACE *> 入力値あり
         MOVE  TB-NTNNO-R (IX-TBL)      TO  TNM-F08  *>_番
     ELSE                                *> 未入力時
         IF  DSP-NTNNO NOT = SPACE  *> 新_番指定あり
             MOVE  DSP-NTNNO            TO  TNM-F08  *>_番
         ELSE                       *> 新_番指定なし
             MOVE  TB-OTNNO-R (IX-TBL)  TO  TNM-F08  *>_番
         END-IF
     END-IF.

     MOVE  TB-STANKA-R (IX-TBL)  TO  TNM-F09.  *>仕入単価
     MOVE  TB-YOBI1-R  (IX-TBL)  TO  TNM-F10.  *>予備１
     MOVE  TB-KENPNG-R (IX-TBL)  TO  TNM-F11.  *>検品Ｇ
     MOVE  TB-SHK-AREA-KBN-R (IX-TBL)
                                 TO  TNM-F12.  *>出荷地域区分
     MOVE  TB-REG-TANCD-R    (IX-TBL)
                                 TO  TNM-F13.  *>登録担当者
     MOVE  TB-LAST-UPD-TANCD-R (IX-TBL)
                                 TO  TNM-F14.  *>最終更新担当者
     MOVE  TB-REG-DATE-R       (IX-TBL)
                                 TO  TNM-F98.  *>登録日
     MOVE  TB-OTNNO-R (IX-TBL)   TO  TNM-F20.  *>旧_番

     IF     TNM-F08 NOT = TB-MAE-NTNNO-R  (IX-TBL)
         OR TNM-F05 NOT = TB-MAE-GTANKA-R (IX-TBL)
         OR TNM-F06 NOT = TB-MAE-UTANKA-R (IX-TBL)
         OR TNM-F09 NOT = TB-MAE-STANKA-R (IX-TBL) *> 変化の判定
         MOVE  1            TO  FG-UPD
     ELSE
         MOVE  ZERO         TO  FG-UPD
     END-IF.

* 更新日
     IF  DSP-SYOKBN = 1 *> 登録
         MOVE  SYS-DATE          TO  TNM-F99
     ELSE               *> 修正
         IF  FG-UPD = 1
             MOVE  SYS-DATE                TO  TNM-F99
         ELSE
             MOVE  TB-UPD-DATE-R (IX-TBL)  TO  TNM-F99
         END-IF
     END-IF.

     IF  FG-TANAMTBL-INV = 1
         WRITE  TNM-REC
     ELSE
         REWRITE  TNM-REC
     END-IF.

     IF  DSP-SYOKBN = 1 *> 登録
         ADD 1  TO  CT-REG
     ELSE               *> 修正
         IF  FG-UPD = 1
             ADD 1  TO  CT-UPD
         END-IF
     END-IF.

 DT-KOSIN-CRB-090.
     MOVE  S-NAME-SV        TO  S-NAME.
 DT-KOSIN-CRB-EXIT.
     EXIT.
****************************************************************
*             終了処理                               3.0
****************************************************************
 END-SEC               SECTION.
     MOVE  S-NAME           TO  S-NAME-SV.
     MOVE  "END-SEC"        TO  S-NAME.
* ファイル ＣＬＯＳＥ
     CLOSE  HSHOTBL.
     CLOSE  HTOKMS.
     CLOSE  HMEIMS.
     CLOSE  ZSOKMS.
     CLOSE  TANAMTBL.
     CLOSE  DSPFILE.

* 件数メッセージ出力
     DISPLAY   "***ﾃﾞｰﾀ ﾄｳﾛｸ  ｹﾝｽｳ = " CT-REG  UPON CONS.
     DISPLAY   "***ﾃﾞｰﾀ ｼｭｳｾｲ ｹﾝｽｳ = " CT-UPD  UPON CONS.
     DISPLAY   "***ﾃﾞｰﾀ ｻｸｼﾞｮ ｹﾝｽｳ = " CT-DLT  UPON CONS.

     MOVE  S-NAME-SV        TO  S-NAME.
 END-EXIT.
     EXIT.
*******************< PROGRAM-END ZMT995I >*********************
