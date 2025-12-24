# SBM0960I

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SBM0960I.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　流通ＢＭＳ　　　　　　            *
*    業務名　　　　　　　：　マスタ保守                        *
*    モジュール名　　　　：　商品名称管理マスタ保守            *
*    作成日／更新日　　　：　2013/04/08                        *
*    作成者／更新者　　　：　ＮＡＶ高橋                        *
*    処理概要　　　　　　：　商品名称管理マスタの保守を行なう。*
****************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SBM0960I.
 AUTHOR.               NAV.
 DATE-WRITTEN.         13/04/08.
****************************************************************
 ENVIRONMENT           DIVISION.
****************************************************************
 CONFIGURATION         SECTION.
 SPECIAL-NAMES.
     CONSOLE      IS   CONS.
*
 INPUT-OUTPUT          SECTION.
 FILE-CONTROL.
*商品名称管理マスタ
     SELECT      SYOMEIF   ASSIGN    TO        DA-01-VI-SYOMEIL1
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      RANDOM
                           RECORD    KEY       SYO-F01  SYO-F02
                           STATUS              SYO-ST.
*取引先マスタ
     SELECT      HTOKMS    ASSIGN    TO        DA-01-VI-TOKMS2
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      RANDOM
                           RECORD    KEY       TOK-F01
                           STATUS              TOK-ST.
*商品変換テーブル
     SELECT      HSHOTBL   ASSIGN    TO        DA-01-VI-SHOTBL1
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      RANDOM
                           RECORD    KEY       TBL-F01   TBL-F02
                           STATUS              TBL-ST.
*商品名称マスタ
     SELECT      HMEIMS    ASSIGN    TO        DA-01-VI-MEIMS1
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      RANDOM
                           RECORD    KEY       MEI-F011
                                               MEI-F0121
                                               MEI-F0122
                                               MEI-F0123
                           STATUS              MEI-ST.
*画面定義ファイル
     SELECT      DSPFILE   ASSIGN    TO        GS-DSPF
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
*    FILE = 商品名称管理マスタ                                 *
****************************************************************
 FD  SYOMEIF
                       LABEL     RECORD    IS   STANDARD.
                       COPY      SYOMEIF   OF   XFDLIB
                       JOINING   SYO       AS   PREFIX.
****************************************************************
*    FILE = 取引先マスタ　　　                                 *
****************************************************************
 FD  HTOKMS
                       LABEL     RECORD    IS   STANDARD.
                       COPY      HTOKMS    OF   XFDLIB
                       JOINING   TOK       AS   PREFIX.
****************************************************************
*    FILE = 商品変換テーブル　                                 *
****************************************************************
 FD  HSHOTBL
                       LABEL     RECORD    IS   STANDARD.
                       COPY      HSHOTBL   OF   XFDLIB
                       JOINING   TBL       AS   PREFIX.
****************************************************************
*    FILE = 商品名称マスタ　　　                               *
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
                       COPY      FBM09601  OF   XMDLIB
                       JOINING   DSP       AS   PREFIX.
*
****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
*ステータス領域
 01  STATUS-AREA.
     03  SYO-ST                   PIC  X(02).
     03  TOK-ST                   PIC  X(02).
     03  TBL-ST                   PIC  X(02).
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
     03  ERR-FLG                  PIC  9(01)  VALUE  ZERO.
     03  END-FLG                  PIC  X(03)  VALUE  SPACE.
     03  SYOMEIF-INV-FLG          PIC  X(03)  VALUE  SPACE.
     03  HTOKMS-INV-FLG           PIC  X(03)  VALUE  SPACE.
     03  HMEIMS-INV-FLG           PIC  X(03)  VALUE  SPACE.
     03  HSHOTBL-INV-FLG          PIC  X(03)  VALUE  SPACE.
     03  WRT-CNT                  PIC  9(07)  VALUE  ZERO.
     03  UPD-CNT                  PIC  9(07)  VALUE  ZERO.
     03  DLT-CNT                  PIC  9(07)  VALUE  ZERO.
*ワーク領域
 01  WRK-AREA.
***  プログラムスイッチ（画面遷移制御）
     03  PSW                      PIC  X(01)  VALUE  SPACE.
***  モード退避
     03  SAV-SHORI                PIC  9(01)  VALUE  ZERO.
*
*システム日付／時刻
 01  TIME-AREA.
     03  WK-TIME                  PIC  9(08)  VALUE  ZERO.
 01  DATE-AREA.
     03  WK-DATE                  PIC  9(06)  VALUE  ZERO.
     03  SYS-DATE                 PIC  9(08)  VALUE  ZERO.
*システム日付変換用
  01  G-DATE.
     03  G-DATE-YY                PIC  9(04)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  "/".
     03  G-DATE-MM                PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  "/".
     03  G-DATE-DD                PIC  9(02)  VALUE  ZERO.
*システム日付変換用
  01  G-TIME.
     03  G-TIME-HH                PIC  Z9.
     03  FILLER                   PIC  X(01)  VALUE  ":".
     03  G-TIME-MM                PIC  Z9.
     03  FILLER                   PIC  X(01)  VALUE  ":".
     03  G-TIME-SS                PIC  Z9.
*
*ＰＦガイド
 01  PF-MSG-AREA.
     03  PF-MSG1.
         05  FILLER               PIC   N(30)
             VALUE NC"_取消　_終了".
     03  PF-MSG2.
         05  FILLER               PIC   N(30)
             VALUE NC"_取消　_終了　_項目戻し".
 01  PF-MSG-AREA-R       REDEFINES     PF-MSG-AREA.
     03  PF-MSG-R   OCCURS   2   PIC   N(30).
*
*メッセージの取得
 01  ERR-MSG-AREA.
     03  ERR-MSG1.
         05  FILLER              PIC   N(25)
             VALUE NC"無効キーです。".
     03  ERR-MSG2.
         05  FILLER              PIC   N(25)
             VALUE NC"正しい値を入力して下さい。".
     03  ERR-MSG3.
         05  FILLER              PIC   N(25)
             VALUE NC"マスタ未登録です。".
     03  ERR-MSG4.
         05  FILLER              PIC   N(25)
             VALUE NC"商品名称管理マスタに登録済です。".
     03  ERR-MSG5.
         05  FILLER              PIC   N(25)
             VALUE NC"商品名称管理マスタに未登録です。".
     03  ERR-MSG6.
         05  FILLER              PIC   N(25)
             VALUE NC"商品名称（漢字）を入力して下さい。".
     03  ERR-MSG7.
         05  FILLER              PIC   N(25)
             VALUE NC"商品名称（カナ）を入力して下さい。".
 01  ERR-MSG-AREA-R      REDEFINES     ERR-MSG-AREA.
     03  ERR-MSG-R   OCCURS  7   PIC   N(25).
*
*ファイルエラーメッセージ
 01  FILE-ERR.
     03  SYO-ERR           PIC N(15) VALUE
         NC"商品名称管理マスタエラー".
     03  TOK-ERR           PIC N(15) VALUE
         NC"取引先マスタエラー".
     03  TBL-ERR           PIC N(15) VALUE
         NC"商品変換ＴＢＬマスタエラー".
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
*
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
*
 LINKAGE               SECTION.
 01  PARA-BUMON            PIC X(04).
 01  PARA-TANCD            PIC X(02).
**************************************************************
 PROCEDURE             DIVISION USING PARA-BUMON PARA-TANCD.
**************************************************************
 DECLARATIVES.
*
 SYO-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE SYOMEIF.
     MOVE        SYO-ST    TO        E-ST.
     MOVE       "SYOMEIF " TO        E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     SYO-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
*
 TOK-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HTOKMS.
     MOVE        SYO-ST    TO        E-ST.
     MOVE       "TOKMS2  " TO        E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     TOK-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
*
 TBL-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HSHOTBL.
     MOVE        TBL-ST    TO        E-ST.
     MOVE       "SHOTBL1 " TO        E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     TBL-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
*
 MEI-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HMEIMS.
     MOVE        MEI-ST    TO        E-ST.
     MOVE       "MEIMS1  " TO        E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     MEI-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 DSP-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE DSPFILE.
     MOVE        DSP-ST    TO        E-ST.
     MOVE        "DSPFILE"  TO       E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     DSP-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
*
 END  DECLARATIVES.
****************************************************************
*             MAIN        MODULE                     0.0       *
****************************************************************
 PROCESS-START         SECTION.
*
     MOVE     "PROCESS START"     TO   S-NAME.
     PERFORM   INIT-SEC.
     PERFORM   MAIN-SEC          UNTIL   END-FLG   =   "END".
     PERFORM   END-SEC.
*
     STOP    RUN.
 CONTROL-EXIT.
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
     ACCEPT    WK-TIME          FROM   TIME.
*ファイルのＯＰＥＮ
     OPEN     I-O       SYOMEIF   DSPFILE.
     OPEN     INPUT     HTOKMS  HSHOTBL  HMEIMS.
*ワークの初期化
     INITIALIZE         FLG-AREA.
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
*画面初期化
         WHEN      "1"  PERFORM   DSP-INIT-SEC
*処理区分入力
         WHEN      "2"  PERFORM   DSP-HEAD1-SEC
*キー項目入力
         WHEN      "3"  PERFORM   DSP-HEAD2-SEC
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
*             初期画面表示( PSW = 1 )                          *
****************************************************************
 DSP-INIT-SEC          SECTION.
     MOVE     "DSP-INIT-SEC"      TO   S-NAME.
*初期画面の表示
     MOVE     SPACE               TO   DSP-PRO.
*画面の初期化
     MOVE    SPACE                TO   DSP-FBM09601.
*システム日付転送
     MOVE    SYS-DATE(1:4)        TO   G-DATE-YY.
     MOVE    SYS-DATE(5:2)        TO   G-DATE-MM.
     MOVE    SYS-DATE(7:2)        TO   G-DATE-DD.
     MOVE    G-DATE               TO   DSP-SDATE.
*システム時間転送
     MOVE    WK-TIME(1:2)         TO   G-TIME-HH.
     MOVE    WK-TIME(3:2)         TO   G-TIME-MM.
     MOVE    WK-TIME(5:2)         TO   G-TIME-SS.
     MOVE    G-TIME               TO   DSP-STIME.
*プログラムＩＤ
     MOVE    "SBM0970I"           TO   DSP-PGID.
*ＦＯＲＭＩＤ
     MOVE    "FBM09601"           TO   DSP-FORMID.
*リバース，カーソルパーク解除
     PERFORM  DSP-ZOKUSEICLR-SEC.
*処理区分入力へ
     MOVE    "2"                  TO   PSW.
*
 DSP-INIT-EXIT.
     EXIT.
****************************************************************
*             処理区分入力( PSW = 2 )                2.1       *
****************************************************************
 DSP-ZOKUSEICLR-SEC    SECTION.
     MOVE     "DSP-ZEKUSEICLR-SEC" TO   S-NAME.
*処理区分
     MOVE    "M"     TO      EDIT-OPTION   OF  DSP-SHORI.
     MOVE    SPACE   TO      EDIT-CURSOR   OF  DSP-SHORI.
*取引先コード
     MOVE    "M"     TO      EDIT-OPTION   OF  DSP-TORICD.
     MOVE    SPACE   TO      EDIT-CURSOR   OF  DSP-TORICD.
*相手商品コード
     MOVE    "M"     TO      EDIT-OPTION   OF  DSP-JANCD.
     MOVE    SPACE   TO      EDIT-CURSOR   OF  DSP-JANCD.
*商品名称（漢字）
     MOVE    "M"     TO      EDIT-OPTION   OF  DSP-SYONM.
     MOVE    SPACE   TO      EDIT-CURSOR   OF  DSP-SYONM.
*規格名称（漢字）
     MOVE    "M"     TO      EDIT-OPTION   OF  DSP-KIKNM.
     MOVE    SPACE   TO      EDIT-CURSOR   OF  DSP-KIKNM.
*商品名称（カナ）
     MOVE    "M"     TO      EDIT-OPTION   OF  DSP-SYOKN.
     MOVE    SPACE   TO      EDIT-CURSOR   OF  DSP-SYOKN.
*相手商品コード
     MOVE    "M"     TO      EDIT-OPTION   OF  DSP-KIKKN.
     MOVE    SPACE   TO      EDIT-CURSOR   OF  DSP-KIKKN.
*
 DSP-ZOKUSEICLR-EXIT.
     EXIT.
****************************************************************
*             処理区分入力( PSW = 2 )                2.1       *
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
         WHEN   OTHER
                MOVE     1       TO   ERR-FLG
     END-EVALUATE.
*
 DSP-HEAD1-EXIT.
     EXIT.
****************************************************************
*             処理区分チェック                       2.1.1     *
****************************************************************
 HEAD1-CHK-SEC             SECTION.
*処理区分 未入力はエラー
     IF       DSP-SHORI      NOT NUMERIC
     OR       DSP-SHORI      =    ZERO
              MOVE      2         TO   ERR-FLG
              GO                  TO   HEAD1-CHK-EXIT
     END-IF.
*処理区分＝１，２，３以外はエラー
     IF       DSP-SHORI      =    1   OR   2   OR   3
              MOVE     "3"        TO   PSW
*             同一モードでループさせるため
              MOVE     DSP-SHORI  TO   SAV-SHORI
     ELSE
              MOVE      2         TO   ERR-FLG
     END-IF.
*
 HEAD1-CHK-EXIT.
     EXIT.
****************************************************************
*             キー項目入力( PSW = 3 )                2.2       *
****************************************************************
 DSP-HEAD2-SEC         SECTION.
     MOVE     "DSP-HEAD2-SEC"     TO   S-NAME.
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
                MOVE    "2"      TO   PSW
*取消
         WHEN   "F004"
                MOVE    "1"      TO   PSW
         WHEN   OTHER
                MOVE     1       TO   ERR-FLG
     END-EVALUATE.
*
 DSP-HEAD2-EXIT.
     EXIT.
****************************************************************
*             キー項目チェック                       2.2.1     *
****************************************************************
 HEAD2-CHK-SEC         SECTION.
     MOVE     "HEAD2-CHK-SEC"     TO   S-NAME.
*キー項目 未入力チェック
***  取引先コード
     IF  DSP-TORICD  NOT  NUMERIC
     OR  DSP-TORICD  =    ZERO
         MOVE   2       TO   ERR-FLG
         MOVE  "R"      TO   EDIT-OPTION   OF  DSP-TORICD
         MOVE  "C"      TO   EDIT-CURSOR   OF  DSP-TORICD
     ELSE
         MOVE   DSP-TORICD   TO  TOK-F01
         PERFORM  HTOKMS-READ-SEC
         IF  HTOKMS-INV-FLG = "INV"
             MOVE   3       TO   ERR-FLG
             MOVE  "R"      TO   EDIT-OPTION   OF  DSP-TORICD
             MOVE  "C"      TO   EDIT-CURSOR   OF  DSP-TORICD
         ELSE
             MOVE  TOK-F02  TO   DSP-TORINM
             MOVE  "M"      TO   EDIT-OPTION   OF  DSP-TORICD
             MOVE  SPACE    TO   EDIT-CURSOR   OF  DSP-TORICD
         END-IF
     END-IF.
***  相手商品コード
     IF  DSP-JANCD   =    SPACE
         IF  ERR-FLG = ZERO
             MOVE   2       TO   ERR-FLG
         END-IF
         MOVE  "R"      TO   EDIT-OPTION   OF  DSP-JANCD
         MOVE  "C"      TO   EDIT-CURSOR   OF  DSP-JANCD
     ELSE
         MOVE   DSP-TORICD   TO  TBL-F01
         MOVE   DSP-JANCD    TO  TBL-F02
         PERFORM  HSHOTBL-READ-SEC
         IF  HSHOTBL-INV-FLG = "INV"
             IF  ERR-FLG = ZERO
                 MOVE   3       TO   ERR-FLG
             END-IF
             MOVE  "R"      TO   EDIT-OPTION   OF  DSP-JANCD
             MOVE  "C"      TO   EDIT-CURSOR   OF  DSP-JANCD
         ELSE
             MOVE  TBL-F031 TO   DSP-SYOCD(1:8)
             MOVE  "-"      TO   DSP-SYOCD(9:1)
             MOVE  TBL-F0321 TO  DSP-SYOCD(10:5)
             MOVE  "-"      TO   DSP-SYOCD(15:1)
             MOVE  TBL-F0322 TO  DSP-SYOCD(16:2)
             MOVE  "-"      TO   DSP-SYOCD(18:1)
             MOVE  TBL-F0323 TO  DSP-SYOCD(19:1)
             MOVE  "M"      TO   EDIT-OPTION   OF  DSP-JANCD
             MOVE  SPACE    TO   EDIT-CURSOR   OF  DSP-JANCD
         END-IF
     END-IF.
*エラーの場合は、ＥＸＩＴへ
     IF       ERR-FLG  NOT =  ZERO
              GO       TO     HEAD2-CHK-EXIT
     END-IF.
*部門取引先マスタ読込み
     MOVE     DSP-TORICD     TO   SYO-F01.
     MOVE     DSP-JANCD      TO   SYO-F02.
     PERFORM  SYOMEIF-READ-SEC.
*処理区分により判定
     EVALUATE DSP-SHORI
         WHEN  1
               IF   SYOMEIF-INV-FLG = SPACE
                    MOVE  4     TO ERR-FLG
                    MOVE "R"    TO EDIT-OPTION OF DSP-TORICD
                    MOVE "R"    TO EDIT-OPTION OF DSP-JANCD
                    MOVE "C"    TO EDIT-CURSOR OF DSP-TORICD
               ELSE
                    MOVE "4"    TO PSW
*                  *登録の場合、商品名称マスタより名称を初期表示
                    MOVE  TBL-F031  TO  MEI-F011
                    MOVE  TBL-F0321 TO  MEI-F0121
                    MOVE  TBL-F0322 TO  MEI-F0122
                    MOVE  TBL-F0323 TO  MEI-F0123
                    PERFORM  HMEIMS-READ-SEC
                    IF  HMEIMS-INV-FLG = SPACE
                        MOVE  MEI-F021  TO  DSP-SYONM
                        MOVE  MEI-F022  TO  DSP-KIKNM
                        MOVE  MEI-F031  TO  DSP-SYOKN
                        MOVE  MEI-F032  TO  DSP-KIKKN
                    END-IF
                    MOVE "M"    TO EDIT-OPTION OF DSP-TORICD
                    MOVE "M"    TO EDIT-OPTION OF DSP-JANCD
                    MOVE  SPACE TO EDIT-CURSOR OF DSP-TORICD
               END-IF
         WHEN  2
               IF   SYOMEIF-INV-FLG = "INV"
                    MOVE    5   TO ERR-FLG
                    MOVE "R"    TO EDIT-OPTION OF DSP-TORICD
                    MOVE "R"    TO EDIT-OPTION OF DSP-JANCD
                    MOVE "C"    TO EDIT-CURSOR OF DSP-TORICD
               ELSE
                    MOVE "4"    TO PSW
                    MOVE "M"    TO EDIT-OPTION OF DSP-TORICD
                    MOVE "M"    TO EDIT-OPTION OF DSP-JANCD
                    MOVE  SPACE TO EDIT-CURSOR OF DSP-TORICD
                    PERFORM MST-DSP-SEC
               END-IF
         WHEN  3
               IF   SYOMEIF-INV-FLG = "INV"
                    MOVE    5   TO ERR-FLG
                    MOVE "R"    TO EDIT-OPTION OF DSP-TORICD
                    MOVE "R"    TO EDIT-OPTION OF DSP-JANCD
                    MOVE "C"    TO EDIT-CURSOR OF DSP-TORICD
               ELSE
                    MOVE "5"    TO PSW
                    MOVE "M"    TO EDIT-OPTION OF DSP-TORICD
                    MOVE "M"    TO EDIT-OPTION OF DSP-JANCD
                    MOVE  SPACE TO EDIT-CURSOR OF DSP-TORICD
                    PERFORM MST-DSP-SEC
               END-IF
     END-EVALUATE.
*
 HEAD2-CHK-EXIT.
     EXIT.
****************************************************************
*             明細項目　入力( PSW = 4 )              2.3       *
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
                    MOVE    "5"      TO   PSW
                END-IF
*終了
         WHEN   "F005"
                MOVE    "END"    TO   END-FLG
*項目戻し
         WHEN   "F006"
                MOVE    "3"      TO   PSW
*取消
         WHEN   "F004"
                MOVE    "1"      TO   PSW
         WHEN   OTHER
                MOVE     1       TO   ERR-FLG
     END-EVALUATE.
*
 DSP-BODY-EXIT.
     EXIT.
****************************************************************
*    ボディー部入力チェック　　　　　　　　　　　　　　　　　  *
****************************************************************
 BODY-CHK-SEC          SECTION.
     MOVE     "BODY-CHK-SEC"      TO   S-NAME.
***  商品名称（漢字）
     IF  DSP-SYONM   =    SPACE
         MOVE   6       TO   ERR-FLG
         MOVE  "R"      TO   EDIT-OPTION   OF  DSP-SYONM
         MOVE  "C"      TO   EDIT-CURSOR   OF  DSP-SYONM
     ELSE
         MOVE  "M"      TO   EDIT-OPTION   OF  DSP-SYONM
         MOVE  SPACE    TO   EDIT-CURSOR   OF  DSP-SYONM
     END-IF.
***  商品名称（カナ）
     IF  DSP-SYOKN   =    SPACE
         IF  ERR-FLG  =  ZERO
             MOVE   7       TO   ERR-FLG
         END-IF
         MOVE  "R"      TO   EDIT-OPTION   OF  DSP-SYOKN
         MOVE  "C"      TO   EDIT-CURSOR   OF  DSP-SYOKN
     ELSE
         MOVE  "M"      TO   EDIT-OPTION   OF  DSP-SYOKN
         MOVE  SPACE    TO   EDIT-CURSOR   OF  DSP-SYOKN
     END-IF.
*
 BODY-CHK-EXIT.
     EXIT.
****************************************************************
*             確認処理　入力（ PSW = 5 ）            2.4
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
                EVALUATE  DSP-SHORI
*-------------------登録
                    WHEN  "1"
                          PERFORM     FILE-WRT-SEC
*-------------------修正
                    WHEN  "2"
                          PERFORM     FILE-UPD-SEC
*-------------------削除
                    WHEN  "3"
                          PERFORM     FILE-DLT-SEC
                END-EVALUATE
                PERFORM   DSP-INIT-SEC
                MOVE    "3"      TO   PSW
                MOVE   SAV-SHORI TO   DSP-SHORI
*終了
         WHEN   "F005"
                MOVE    "END"    TO   END-FLG
*項目戻し
         WHEN   "F006"
                IF  DSP-SHORI   =  1   OR   2
                    MOVE    "4"       TO   PSW
                ELSE
                    MOVE    "3"       TO   PSW
                END-IF
*取消
         WHEN   "F004"
                MOVE    "1"      TO   PSW
         WHEN   OTHER
                MOVE     1       TO   ERR-FLG
     END-EVALUATE.
*
 DSP-KAKU-EXIT.
     EXIT.
****************************************************************
*    商品名称管理マスタ更新　処理区分＝１（登録）              *
****************************************************************
 FILE-WRT-SEC           SECTION.
     MOVE     "FILE-WRT-SEC"      TO   S-NAME.
*レコード初期クリア
     MOVE     SPACE               TO   SYO-REC.
     INITIALIZE                        SYO-REC.
*項目転送
***  取引先コード
     MOVE     DSP-TORICD          TO   SYO-F01.
***  相手商品コード
     MOVE     DSP-JANCD           TO   SYO-F02.
***  商品名称漢字
     MOVE     DSP-SYONM           TO   SYO-F03.
***  規格名称漢字
     MOVE     DSP-KIKNM           TO   SYO-F04.
***  商品名称カナ
     MOVE     DSP-SYOKN           TO   SYO-F05.
***  規格名称カナ
     MOVE     DSP-KIKKN           TO   SYO-F06.
***  登録担当者部門
     MOVE     PARA-BUMON          TO   SYO-F92.
***  登録担当者
     MOVE     PARA-TANCD          TO   SYO-F93.
***  登録日
     MOVE     SYS-DATE            TO   SYO-F94.
***  登録時刻
     ACCEPT   WK-TIME             FROM TIME.
     MOVE     WK-TIME(1:6)        TO   SYO-F95.
*商品名称管理マスタ登録
     WRITE    SYO-REC.
     ADD      1                   TO   WRT-CNT.
*
 FILE-WRT-EXIT.
     EXIT.
****************************************************************
*    商品名称管理マスタ更新　処理区分＝２（修正）  2.4.2       *
****************************************************************
 FILE-UPD-SEC           SECTION.
     MOVE     "FILE-UPD-SEC"      TO   S-NAME.
***  商品名称漢字
     MOVE     DSP-SYONM           TO   SYO-F03.
***  規格名称漢字
     MOVE     DSP-KIKNM           TO   SYO-F04.
***  商品名称カナ
     MOVE     DSP-SYOKN           TO   SYO-F05.
***  規格名称カナ
     MOVE     DSP-KIKKN           TO   SYO-F06.
***  更新担当者部門
     MOVE     PARA-BUMON          TO   SYO-F96.
***  更新担当者
     MOVE     PARA-TANCD          TO   SYO-F97.
***  更新日
     MOVE     SYS-DATE            TO   SYO-F98.
***  更新時刻
     ACCEPT   WK-TIME             FROM TIME.
     MOVE     WK-TIME(1:6)        TO   SYO-F99.
*商品名称管理マスタ更新
     REWRITE  SYO-REC.
     ADD      1                   TO   UPD-CNT.
*
 FILE-UPD-EXIT.
     EXIT.
****************************************************************
*    部門取引先マスタ更新　処理区分＝３（削除）    2.4.3       *
****************************************************************
 FILE-DLT-SEC           SECTION.
     MOVE     "FILE-DLT-SEC"      TO   S-NAME.
***レコード削除
     DELETE   SYOMEIF.
     ADD      1                   TO   DLT-CNT.
*
 FILE-DLT-EXIT.
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
     EVALUATE   PSW
*処理区分
         WHEN   "2"
                MOVE    PF-MSG-R(1)        TO   DSP-PFGAID
*キー項目／ボディー部／確認部
         WHEN   "3"   WHEN   "4"   WHEN   "5"
                MOVE    PF-MSG-R(2)        TO   DSP-PFGAID
         WHEN   OTHER
                MOVE    SPACE              TO   DSP-PFGAID
     END-EVALUATE.
*画面の表示
     MOVE    "SCREEN"            TO   DSP-GRP.
     MOVE    "FBM09601"          TO   DSP-FMT.
     WRITE    DSP-FBM09601.
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
*    MOVE    "SCREEN"             TO   DSP-GRP.
     EVALUATE   PSW
*処理区分
         WHEN   "2"
                MOVE    "MODE"    TO   DSP-GRP
*キー項目
         WHEN   "3"
                MOVE    "KEY"     TO   DSP-GRP
*明細項目
         WHEN   "4"
                MOVE    "MEISAI"  TO   DSP-GRP
*確認
         WHEN   "5"
                MOVE    "KAKU"    TO   DSP-GRP
     END-EVALUATE.

     MOVE    "FBM09601"           TO   DSP-FMT.
     READ    DSPFILE.
*入力項目の属性を通常にする
 DSP-READ-010.
     MOVE    SPACE                TO   DSP-PRO.
*
 DSP-READ-EXIT.
     EXIT.
****************************************************************
*    商品名称管理マスタ内容→画面セット　　　                  *
****************************************************************
 MST-DSP-SEC              SECTION.
     MOVE   "MST-DSP-SEC"         TO   S-NAME.
*商品名称（漢字）
     MOVE    SYO-F03              TO   DSP-SYONM.
*規格名称（漢字）
     MOVE    SYO-F04              TO   DSP-KIKNM.
*商品名称（カナ）
     MOVE    SYO-F05              TO   DSP-SYOKN.
*規格名称（カナ）
     MOVE    SYO-F06              TO   DSP-KIKKN.
*
 MST-DSP-EXIT.
     EXIT.
****************************************************************
*    商品名称管理マスタ読込　　　　　　　                      *
****************************************************************
 SYOMEIF-READ-SEC         SECTION.
     MOVE   "SYOMEIF-READ-SEC"    TO   S-NAME.
*
     READ    SYOMEIF
         INVALID
             MOVE    "INV"        TO   SYOMEIF-INV-FLG
         NOT INVALID
             MOVE     SPACE       TO   SYOMEIF-INV-FLG
     END-READ.
*
 SYOMEIF-READ-EXIT.
     EXIT.
****************************************************************
*    取引先マスタ読込　　　　　　　　　                        *
****************************************************************
 HTOKMS-READ-SEC          SECTION.
     MOVE   "HTOKMS-READ-SEC"          TO   S-NAME.
*
     READ    HTOKMS
         INVALID
             MOVE    "INV"        TO   HTOKMS-INV-FLG
         NOT INVALID
             MOVE     SPACE       TO   HTOKMS-INV-FLG
     END-READ.
*
 HTOKMS-READ-EXIT.
     EXIT.
****************************************************************
*    商品変換ＴＢＬ読込　　　　　　　　                        *
****************************************************************
 HSHOTBL-READ-SEC         SECTION.
     MOVE   "HSHOTBL-READ-SEC"         TO   S-NAME.
*
     READ    HSHOTBL
         INVALID
             MOVE    "INV"        TO   HSHOTBL-INV-FLG
         NOT INVALID
             MOVE     SPACE       TO   HSHOTBL-INV-FLG
     END-READ.
*
 HSHOTBL-READ-EXIT.
     EXIT.
****************************************************************
*    商品名称マスタ読込　　　　　　　　                        *
****************************************************************
 HMEIMS-READ-SEC          SECTION.
     MOVE   "HMEIMS-READ-SEC"          TO   S-NAME.
*
     READ    HMEIMS
         INVALID
             MOVE    "INV"        TO   HMEIMS-INV-FLG
         NOT INVALID
             MOVE     SPACE       TO   HMEIMS-INV-FLG
     END-READ.
*
 HMEIMS-READ-EXIT.
     EXIT.
****************************************************************
*             終了処理                               3.0       *
****************************************************************
 END-SEC               SECTION.
*ファイル ＣＬＯＳＥ
     CLOSE             SYOMEIF  DSPFILE  HTOKMS  HMEIMS  HSHOTBL.
*件数表示
     IF  WRT-CNT = ZERO
     AND UPD-CNT = ZERO
     AND DLT-CNT = ZERO
         DISPLAY NC"＃＃マスタ修正無し！！＃＃" UPON CONS
     ELSE
         DISPLAY "# WRITE   CNT = " WRT-CNT " #" UPON CONS
         DISPLAY "# REWRITE CNT = " UPD-CNT " #" UPON CONS
         DISPLAY "# DELETE  CNT = " DLT-CNT " #" UPON CONS
     END-IF.
**
 END-EXIT.
     EXIT.
*****************<<  SBM0960I   END PROGRAM  >>******************

```
