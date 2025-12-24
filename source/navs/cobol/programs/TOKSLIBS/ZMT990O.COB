****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　マスタメンテ                      *
*    業務名　　　　　　　：　ＥＯＳ管理　　　　　　　　        *
*    モジュール名　　　　：　ＺＭＴ９９０Ｏ                    *
*    作成日／更新日　　　：　2005/10/31 - 11/10 (3)            *
*    作成者／更新者　　　：　ＮＡＶ武井                        *
*    処理概要　　　　　　：　商品変換テーブル一括メンテ入力    *
*                                                              *
****************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           ZMT990O.
 AUTHOR.               TAKEI.
 DATE-WRITTEN.         05/10/31.
****************************************************************
 ENVIRONMENT           DIVISION.
****************************************************************
 CONFIGURATION         SECTION.
 SPECIAL-NAMES.
     CONSOLE      IS   CONS.
*
 INPUT-OUTPUT          SECTION.
 FILE-CONTROL.
*----<< 商品変換テーブル >>-*
     SELECT   SHOTBL    ASSIGN    TO        DA-01-VI-SHOTBL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      DYNAMIC
                        RECORD    KEY       STB-F01   STB-F02
                        FILE      STATUS    STB-ST.
*----<< 一括メンテデータ >>-*
     SELECT   SHTBLM    ASSIGN    TO        DA-01-VI-SHTBLM1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      DYNAMIC
                        RECORD    KEY       STM-F031  STM-F032
                                            STM-F01
                        FILE      STATUS    STM-ST.
*----<< 商品名称マスタ >>-*
     SELECT   HMEIMS    ASSIGN    TO        DA-01-VI-MEIMS1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      DYNAMIC
                        RECORD    KEY       MEI-F01
                        FILE      STATUS    MEI-ST.
*----<< 取引先マスタ >>-*
     SELECT  HTOKMS    ASSIGN    TO        DA-01-VI-TOKMS2
                       ORGANIZATION         INDEXED
                       ACCESS    MODE       DYNAMIC
                       RECORD    KEY        TOK-F01
                       FILE      STATUS     TOK-ST.
*----<< 倉庫マスタ >>-*
     SELECT   ZSOKMS    ASSIGN    TO        DA-01-VI-ZSOKMS1
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
****************************************************************
 DATA                DIVISION.
****************************************************************
 FILE                SECTION.
****************************************************************
*    FILE = 商品変換テーブル                                   *
****************************************************************
 FD  SHOTBL
                     BLOCK     CONTAINS   12  RECORDS
                          LABEL     RECORD     IS  STANDARD.
                     COPY     HSHOTBL   OF   XFDLIB
                          JOINING   STB  AS   PREFIX.
****************************************************************
*    FILE = 一括メンテデータ                                   *
****************************************************************
 FD  SHTBLM
                     BLOCK     CONTAINS   11  RECORDS
                          LABEL     RECORD     IS  STANDARD.
                     COPY     HSHTBLM   OF   XFDLIB
                          JOINING   STM  AS   PREFIX.
****************************************************************
*    FILE = 商品名称マスタ                                     *
****************************************************************
 FD  HMEIMS
                   BLOCK     CONTAINS   6   RECORDS
                        LABEL     RECORD     IS  STANDARD.
                   COPY     HMEIMS    OF   XFDLIB
                        JOINING   MEI  AS   PREFIX.
****************************************************************
*    FILE = 取引先マスタ                                       *
****************************************************************
 FD  HTOKMS
                   BLOCK     CONTAINS  8    RECORDS
                       LABEL     RECORD    IS   STANDARD.
                   COPY      HTOKMS    OF   XFDLIB
                       JOINING   TOK       AS   PREFIX.
****************************************************************
*    FILE = 倉庫マスタ                                         *
****************************************************************
 FD  ZSOKMS
                   LABEL     RECORD     IS  STANDARD.
                   COPY     ZSOKMS1   OF   XFDLIB
                       JOINING   SOK  AS PREFIX.
****************************************************************
*    FILE = 画面ファイル                                       *
****************************************************************
 FD  DSPFILE
                  LABEL     RECORD    IS   OMITTED.
                  COPY      FMT990    OF   XMDLIB
                       JOINING   DSP       AS   PREFIX.
*
****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
*ステータス領域
 01  STATUS-AREA.
     03  STB-ST                   PIC  X(02).
     03  STM-ST                   PIC  X(02).
     03  MEI-ST                   PIC  X(02).
     03  TOK-ST                   PIC  X(02).
     03  SOK-ST                   PIC  X(02).
     03  DSP-ST                   PIC  X(02).
*画面制御用領域
 01  DSP-CONTROL.
     03  DSP-FMT                  PIC  X(08).
     03  DSP-GRP                  PIC  X(08).
     03  DSP-PRO                  PIC  X(02).
     03  DSP-FNC                  PIC  X(04).
 01  SET-PGID                     PIC  X(08)  VALUE "ZMT990O ".
 01  SET-FORMID                   PIC  X(08)  VALUE "FMT990  ".
 01  MAX-PGCNT                    PIC  9(03)  VALUE 20.
 01  MAX-LNCNT                    PIC  9(03)  VALUE 07.
*フラグ領域
 01  FLG-AREA.
     03  HTOKMS-INV-FLG           PIC  X(03)  VALUE  SPACE.
     03  INV-FLG                  PIC  X(03)  VALUE  SPACE.
     03  CHK-FLG                  PIC  X(03)  VALUE  SPACE.
     03  ERR-FLG                  PIC  9(02)  VALUE  ZERO.
     03  END-FLG                  PIC  X(03)  VALUE  SPACE.
     03  RD-FLG                   PIC  X(03)  VALUE  SPACE.
     03  SET-FLG                  PIC  X(03)  VALUE  SPACE.
     03  P-CNT                    PIC  9(02)  VALUE  ZERO.
     03  S-CNT                    PIC  9(02)  VALUE  ZERO.
     03  C-CNT                    PIC  9(02)  VALUE  ZERO.
     03  X                        PIC  9(02)  VALUE  ZERO.
     03  Y                        PIC  9(02)  VALUE  ZERO.
     03  IX                       PIC  9(02)  VALUE  ZERO.
     03  IY                       PIC  9(02)  VALUE  ZERO.
     03  WK-SEQ                   PIC  9(03)  VALUE  ZERO.
     03  END-SW                   PIC  X(01)  VALUE  SPACE.
     03  WC-FLG                   PIC  X(01)  VALUE  SPACE.
*ワーク領域
 01  WRK-AREA.
***  プログラムスイッチ（画面遷移制御）
     03  PSW                      PIC  X(01)  VALUE  SPACE.
*
     03  WK-SSHOCD                PIC  X(08)  VALUE  SPACE.
     03  WK-SHINCD.
         05  WK-SHINCD1           PIC  X(05)  VALUE  SPACE.
         05  WK-SHINCD2           PIC  X(02)  VALUE  SPACE.
         05  WK-SHINCD3           PIC  X(01)  VALUE  SPACE.
     03  WK-JANCD                 PIC  X(13)  VALUE  SPACE.
     03  WK-TANAB                 PIC  X(06)  VALUE  SPACE.
     03  WK-SOUK                  PIC  X(02)  VALUE  SPACE.
     03  WK-GENKA                 PIC  9(07)V99  VALUE  ZERO.
     03  WK-BAIKA                 PIC  9(07)V99  VALUE  ZERO.
     03  DLT-CNT                  PIC  9(07)  VALUE  ZERO.
     03  WRT-CNT                  PIC  9(07)  VALUE  ZERO.
     03  ERR-CNT                  PIC  9(07)  VALUE  ZERO.
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
         05  FILLER               PIC   N(20)
      VALUE NC"_取消_終了　　　　　　　　　　　".
     03  PF-MSG2.
         05  FILLER               PIC   N(20)
      VALUE NC"_取消_終了_項目戻り　　　　　　".
     03  PF-MSG3.
         05  FILLER               PIC   N(20)
      VALUE NC"_取消_項目戻り_前頁_次頁　　　".
 01  PF-MSG-AREA-R       REDEFINES     PF-MSG-AREA.
     03  PF-MSG-R   OCCURS   3   PIC   N(20).
*
*メッセージの取得
 01  ERR-MSG-AREA.
     03  ERR-MSG1.
         05  FILLER              PIC   N(20)
             VALUE NC"無効ＰＦキーです。".
     03  ERR-MSG2.
         05  FILLER              PIC   N(20)
             VALUE NC"前頁はありません。".
     03  ERR-MSG3.
         05  FILLER              PIC   N(20)
             VALUE NC"次頁はありません。".
     03  ERR-MSG4.
         05  FILLER              PIC   N(20)
             VALUE NC"処理区分を入力して下さい。".
     03  ERR-MSG5.
         05  FILLER              PIC   N(20)
             VALUE NC"一括処理を入力して下さい。".
     03  ERR-MSG6.
         05  FILLER              PIC   N(20)
             VALUE NC"商品ＣＤを入力して下さい。".
     03  ERR-MSG7.
         05  FILLER              PIC   N(20)
             VALUE NC"ＪＡＮコードを入力して下さい。".
     03  ERR-MSG8.
         05  FILLER              PIC   N(20)
             VALUE NC"倉庫コードを入力して下さい。".
     03  ERR-MSG9.
         05  FILLER              PIC   N(20)
             VALUE NC"取引先コードを入力して下さい。".
     03  ERR-MSG10.
         05  FILLER              PIC   N(20)
             VALUE NC"作成区分の入力に誤りがあります。".
     03  ERR-MSG11.
         05  FILLER              PIC   N(20)
             VALUE NC"取引先コードの入力にダブリあります。".
     03  ERR-MSG12.
         05  FILLER              PIC   N(20)
             VALUE NC"売価　＜　原価　の関係です。".
     03  ERR-MSG13.
         05  FILLER              PIC   N(20)
             VALUE NC"サカタ商品ＣＤが登録されていません。".
     03  ERR-MSG14.
         05  FILLER              PIC   N(20)
             VALUE NC"倉庫コードが登録されていません。".
     03  ERR-MSG15.
         05  FILLER              PIC   N(20)
             VALUE NC"取引先コードが登録されていません。".
     03  ERR-MSG16.
         05  FILLER              PIC   N(20)
             VALUE NC"既に商品変換テーブルに登録済です。".
     03  ERR-MSG17.
         05  FILLER              PIC   N(20)
             VALUE NC"対象データがありません。".
     03  ERR-MSG18.
         05  FILLER              PIC   N(20)
             VALUE NC"既にメンテデータが作成済です。".
     03  ERR-MSG19.
         05  FILLER              PIC   N(20)
             VALUE NC"ＭＳＧ１９　　　　　　　　　　".
     03  ERR-MSG20.
         05  FILLER              PIC   N(20)
             VALUE NC"ＭＳＧ２０　　　　　　　　　　".
 01  ERR-MSG-AREA-R      REDEFINES     ERR-MSG-AREA.
     03  ERR-MSG-R   OCCURS 20   PIC   N(20).
*メンテデータ退避エリア
 01  TABLE-AREA.
     03  TABLE1      OCCURS  20.
         05  TABLE2  OCCURS  07.
             07  TBL-SAKBN        PIC   X(01).
             07  TBL-TORICD       PIC   9(08).
             07  TBL-TJANCD       PIC   X(13).
             07  TBL-TTANAB       PIC   X(06).
             07  TBL-TSOUK        PIC   X(02).
             07  TBL-TGENKA       PIC   9(07)V99.
             07  TBL-TBAIKA       PIC   9(07)V99.
             07  TBL-TOROKUHI     PIC   9(08).
             07  TBL-KOUSINHI     PIC   9(08).
*ファイルエラーメッセージ
 01  FILE-ERR.
     03  STB-ERR           PIC N(15) VALUE
                        NC"商品変換テーブルエラー".
     03  STM-ERR           PIC N(15) VALUE
                        NC"一括メンテデータエラー".
     03  MEI-ERR           PIC N(15) VALUE
                        NC"商品名称マスタエラー".
     03  TOK-ERR           PIC N(15) VALUE
                        NC"取引先マスタエラー".
     03  SOK-ERR           PIC N(15) VALUE
                        NC"倉庫マスタエラー".
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
**************************************************************
 PROCEDURE             DIVISION.
**************************************************************
 DECLARATIVES.
 STB-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE SHOTBL.
     MOVE        STB-ST    TO        E-ST.
     MOVE        "SHOTBL"  TO        E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     STB-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 STM-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE SHTBLM.
     MOVE        STM-ST    TO        E-ST.
     MOVE        "SHTBLM"  TO        E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     STM-ERR   UPON      CONS.
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
     MOVE     "PROCESS-START"     TO   S-NAME.
***
     PERFORM   INIT-SEC.
     PERFORM   MAIN-SEC          UNTIL   END-FLG   =   "END".
     PERFORM   END-SEC.
***
*
 CONTROL-EXIT.
     STOP  RUN.
****************************************************************
*             初期処理                               1.0
****************************************************************
 INIT-SEC              SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
*ファイルのＯＰＥＮ
     OPEN      INPUT     SHOTBL  HMEIMS  HTOKMS  ZSOKMS.
     OPEN      I-O       DSPFILE.
     OPEN      I-O       SHTBLM.
*ワークの初期化
     INITIALIZE                 FLG-AREA.
*初期画面の表示
     MOVE     SPACE               TO   DSP-PRO.
     PERFORM   INIT-DSP-SEC.
     MOVE    "1"                  TO   PSW.
*頁セット
     MOVE     1                   TO   P-CNT.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*             メイン処理                             2.0       *
****************************************************************
 MAIN-SEC              SECTION.
     MOVE     "MAIN-SEC"     TO   S-NAME.
*
     EVALUATE      PSW
*処理区分入力(2.1)
         WHEN      "1"       PERFORM   DSP-HEAD-SEC
*商品ＣＤ入力(2.2)
         WHEN      "2"       PERFORM   DSP-HEAD2-SEC
*ＪＡＮコード入力(2.3)
         WHEN      "3"       PERFORM   DSP-HEAD3-SEC
*明細入力    (2.4)
         WHEN      "4"       PERFORM   DSP-BODY-SEC
*確認入力    (2.5)
         WHEN      "5"       PERFORM   DSP-KAKU-SEC
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
     CLOSE   SHOTBL   HMEIMS  HTOKMS  ZSOKMS.
     CLOSE   SHTBLM   DSPFILE.
**
*件数メッセージ出力
     DISPLAY   "***ﾃﾞｰﾀ ｻｸｾｲ  ｹﾝｽｳ   = " WRT-CNT  UPON CONS.
     DISPLAY   "***ﾃﾞｰﾀ ｻｸｼﾞﾖ ｹﾝｽｳ   = " DLT-CNT  UPON CONS.
     DISPLAY   "***ﾃﾞｰﾀ ｺｳｼﾝ ｴﾗｰｽｳ   = " ERR-CNT  UPON CONS.
 END-EXIT.
     EXIT.
****************************************************************
*             初期画面表示                            1.1      *
****************************************************************
 INIT-DSP-SEC          SECTION.
     MOVE     "INIT-DSP-SEC"      TO   S-NAME.
 INITD001.
*画面の初期化
     MOVE    SPACE                TO   DSP-FMT990.
 INITD003.
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
*システム時刻取得
     ACCEPT    WK-TIME    FROM   TIME.
*画面表示時刻編集
     MOVE      WK-TIME(1:2)       TO   HEN-TIME-HH.
     MOVE      WK-TIME(3:2)       TO   HEN-TIME-MM.
     MOVE      WK-TIME(5:2)       TO   HEN-TIME-SS.
*システム日付・時間転送
     MOVE    HEN-DATE             TO   DSP-SDATE.
     MOVE    HEN-TIME(1:5)        TO   DSP-STIME.
 INITD004.
*ＰＧＩＤ・ＦＭＴＩＤセット
     MOVE    SET-PGID             TO   DSP-PGID.
     MOVE    SET-FORMID           TO   DSP-FORM.
 INITD005.
*項目属性クリア
     PERFORM  DSP-SYOKI-SEC.
     PERFORM  DSP-BODYCL-SEC.
*
 INIT-DSP-EXIT.
     EXIT.
****************************************************************
*           処理区分入力(PSW = 1)                     2.1      *
****************************************************************
 DSP-HEAD-SEC          SECTION.
     MOVE     "DSP-HEAD-SEC"      TO   S-NAME.
*
     PERFORM  DSP-WRITE-SEC.
     PERFORM  DSP-READ-SEC.
*
     EVALUATE   DSP-FNC
*実行
       WHEN   "E000"
           IF  DSP-SRKBN  =  1 OR 2 OR 3
               MOVE  "M"    TO  EDIT-OPTION  OF  DSP-SRKBN
               MOVE  SPACE  TO  EDIT-CURSOR  OF  DSP-SRKBN
           ELSE
               MOVE   4       TO  ERR-FLG
               MOVE  "R"      TO  EDIT-OPTION  OF  DSP-SRKBN
               MOVE  "C"      TO  EDIT-CURSOR  OF  DSP-SRKBN
           END-IF
           IF  DSP-IKATU  =  0 OR 1 OR 2
               MOVE  "M"    TO  EDIT-OPTION  OF  DSP-IKATU
               MOVE  SPACE  TO  EDIT-CURSOR  OF  DSP-IKATU
           ELSE
               IF  ERR-FLG  =  ZERO
                 MOVE  5    TO  ERR-FLG
                 MOVE  "R"  TO  EDIT-OPTION  OF  DSP-IKATU
                 MOVE  "C"  TO  EDIT-CURSOR  OF  DSP-IKATU
               END-IF
           END-IF
           IF   DSP-SRKBN  =  1
               IF   DSP-IKATU  =  1  OR  2
                    CONTINUE
               ELSE
                  MOVE  "R"   TO  EDIT-OPTION  OF  DSP-IKATU
                  MOVE  "C"   TO  EDIT-CURSOR  OF  DSP-IKATU
                  IF  ERR-FLG =  ZERO
                      MOVE   5    TO  ERR-FLG
                  END-IF
               END-IF
           END-IF
           IF  ERR-FLG  =  ZERO
               MOVE  "2"    TO  PSW
           END-IF
*取消
         WHEN   "F004"
                MOVE    "1"      TO   PSW
                MOVE     1       TO   P-CNT
                PERFORM   INIT-DSP-SEC
*終了
         WHEN   "F005"
                MOVE    "END"    TO   END-FLG
                MOVE    "Y  "    TO   END-SW
         WHEN   OTHER
                MOVE     1       TO   ERR-FLG
     END-EVALUATE.
*
 DSP-HEAD-EXIT.
     EXIT.
****************************************************************
*             商品ＣＤ入力( PSW = 2 )                 2.2      *
****************************************************************
 DSP-HEAD2-SEC          SECTION.
     MOVE     "DSP-HEAD2-SEC"      TO   S-NAME.
*
     PERFORM    DSP-WRITE-SEC.
     PERFORM    DSP-READ-SEC.
*
     EVALUATE   DSP-FNC
*実行
         WHEN   "E000"
                PERFORM   HEAD2-CHK-SEC
                IF  ERR-FLG  =  ZERO
                  IF  DSP-SRKBN  =  1
                      MOVE  "3"  TO   PSW
                  END-IF
                  IF  DSP-SRKBN  =  2
                      MOVE  "4"  TO  PSW
                      PERFORM   MDT-WORK-SEC
                      PERFORM   WORK-DSP-SEC
                  END-IF
                  IF  DSP-SRKBN  =  3
                      MOVE  "5"   TO   PSW
                      PERFORM   MDT-WORK-SEC
                      PERFORM   WORK-DSP-SEC
                  END-IF
                END-IF
*取消
         WHEN   "F004"
                MOVE    "1"      TO   PSW
                MOVE    1        TO   P-CNT
                PERFORM   INIT-DSP-SEC
*終了
         WHEN   "F005"
                MOVE    "END"    TO   END-FLG
                MOVE    "Y"      TO   END-SW
*項目戻り
         WHEN   "F006"
                MOVE    "1"      TO   PSW
                MOVE    1        TO   P-CNT
         WHEN   OTHER
                MOVE     1       TO   ERR-FLG
     END-EVALUATE.
*
 DSP-HEAD2-EXIT.
     EXIT.
****************************************************************
*             商品ＣＤ等チェック                      2.2.1    *
****************************************************************
 HEAD2-CHK-SEC             SECTION.
     MOVE     "HEAD2-CHK-SEC"     TO   S-NAME.
*商品ＣＤチェック
     IF  DSP-SSHO1  =  SPACE
       OR  DSP-SSHO1  =  ZERO
              MOVE   6       TO  ERR-FLG
              MOVE  "R"      TO  EDIT-OPTION  OF  DSP-SSHO1
              MOVE  "C"      TO  EDIT-CURSOR  OF  DSP-SSHO1
              GO  TO    HEAD2-CHK-EXIT
     ELSE
              MOVE  "M"      TO  EDIT-OPTION  OF  DSP-SSHO1
              MOVE   SPACE   TO  EDIT-CURSOR  OF  DSP-SSHO1
     END-IF.
*品単チェック
     MOVE  "M"      TO  EDIT-OPTION  OF  DSP-SSHO2.
     MOVE   SPACE   TO  EDIT-CURSOR  OF  DSP-SSHO2.
     MOVE  "M"      TO  EDIT-OPTION  OF  DSP-SSHO3.
     MOVE   SPACE   TO  EDIT-CURSOR  OF  DSP-SSHO3.
     MOVE  "M"      TO  EDIT-OPTION  OF  DSP-SSHO4.
     MOVE   SPACE   TO  EDIT-CURSOR  OF  DSP-SSHO4.
*商品名称マスタチェック
     PERFORM  MEI-READ-SEC.
     IF  INV-FLG  =  "INV"
         MOVE   ALL NC"＊"   TO  DSP-SHON1  DSP-SHON2
         IF  DSP-SRKBN  =  1  OR  2
             MOVE  13        TO  ERR-FLG
             MOVE  "R"      TO  EDIT-OPTION  OF  DSP-SSHO1
             MOVE  "C"      TO  EDIT-CURSOR  OF  DSP-SSHO1
             MOVE  "R"      TO  EDIT-OPTION  OF  DSP-SSHO2
             MOVE  "C"      TO  EDIT-CURSOR  OF  DSP-SSHO2
             MOVE  "R"      TO  EDIT-OPTION  OF  DSP-SSHO3
             MOVE  "C"      TO  EDIT-CURSOR  OF  DSP-SSHO3
             MOVE  "R"      TO  EDIT-OPTION  OF  DSP-SSHO4
             MOVE  "C"      TO  EDIT-CURSOR  OF  DSP-SSHO4
         ELSE
             MOVE   DSP-SSHO1    TO  WK-SSHOCD
             MOVE   DSP-SSHO2    TO  WK-SHINCD1
             MOVE   DSP-SSHO3    TO  WK-SHINCD2
             MOVE   DSP-SSHO4    TO  WK-SHINCD3
             INITIALIZE     MEI-REC
         END-IF
     ELSE
         MOVE   MEI-F021     TO  DSP-SHON1
         MOVE   MEI-F022     TO  DSP-SHON2
         MOVE   DSP-SSHO1    TO  WK-SSHOCD
         MOVE   DSP-SSHO2    TO  WK-SHINCD1
         MOVE   DSP-SSHO3    TO  WK-SHINCD2
         MOVE   DSP-SSHO4    TO  WK-SHINCD3
     END-IF.
*
     IF  ERR-FLG  NOT =  ZERO
         GO  TO    HEAD2-CHK-EXIT
     END-IF.
*ヘッド項目セット
     MOVE  MEI-F06      TO   DSP-JANCD.
     MOVE  SPACE        TO   DSP-TANAB.
     MOVE  SPACE        TO   DSP-SOUK.
     MOVE  MEI-F041     TO   DSP-GENKA.
     MOVE  MEI-F042     TO   DSP-BAIKA.
*メンテデータ存在チェック
     IF  DSP-SRKBN  =  2  OR  3
           PERFORM  MDT-WORK-SEC
           IF  WK-SEQ  =  ZERO
               MOVE  17     TO   ERR-FLG
           ELSE
               PERFORM  PRO-HEAD-SET
           END-IF
     END-IF.
 HEAD2-CHK-EXIT.
     EXIT.
****************************************************************
*             ＪＡＮＣＤ入力( PSW = 3 )               2.3      *
****************************************************************
 DSP-HEAD3-SEC          SECTION.
     MOVE     "DSP-HEAD3-SEC"      TO   S-NAME.
*
     PERFORM    DSP-WRITE-SEC.
     PERFORM    DSP-READ-SEC.
*
     EVALUATE   DSP-FNC
*実行
         WHEN   "E000"
                PERFORM   HEAD3-CHK-SEC
                IF  ERR-FLG  =  ZERO
                    MOVE  "4"   TO   PSW
                    PERFORM     MDT-WORK-SEC
                    PERFORM     WORK-DSP-SEC
                END-IF
*取消
         WHEN   "F004"
                MOVE    "1"      TO   PSW
                MOVE    1        TO   P-CNT
                PERFORM   INIT-DSP-SEC
*終了
         WHEN   "F005"
                MOVE    "END"    TO   END-FLG
                MOVE    "Y"      TO   END-SW
*項目戻り
         WHEN   "F006"
                MOVE    "2"      TO   PSW
                MOVE    1        TO   P-CNT
         WHEN   OTHER
                MOVE     1       TO   ERR-FLG
     END-EVALUATE.
*
 DSP-HEAD2-EXIT.
     EXIT.
****************************************************************
*             ＪＡＮコード等チェック                  2.3.1    *
****************************************************************
 HEAD3-CHK-SEC             SECTION.
     MOVE     "HEAD3-CHK-SEC"     TO   S-NAME.
     IF  DSP-SRKBN  =  2 OR  3
         MOVE  DSP-JANCD     TO   WK-JANCD
         MOVE  DSP-TANAB     TO   WK-TANAB
         MOVE  DSP-SOUK      TO   WK-SOUK
         MOVE  DSP-GENKA     TO   WK-GENKA
         MOVE  DSP-BAIKA     TO   WK-BAIKA
         GO  TO  HEAD3-C990
     END-IF.
** 登録チェック
*ＪＡＮＣＤチェック
     IF   DSP-JANCD   =  SPACE
          MOVE  "R"      TO  EDIT-OPTION  OF  DSP-JANCD
          MOVE  "C"      TO  EDIT-CURSOR  OF  DSP-JANCD
          MOVE  7            TO   ERR-FLG
     ELSE
          MOVE  "M"      TO  EDIT-OPTION  OF  DSP-JANCD
          MOVE  SPACE    TO  EDIT-CURSOR  OF  DSP-JANCD
          MOVE  DSP-JANCD    TO   WK-JANCD
     END-IF.
*_番チェック
     MOVE  DSP-TANAB         TO  WK-TANAB.
*倉庫チェック
     IF  DSP-SOUK  =  SPACE
         MOVE  "R"      TO  EDIT-OPTION  OF  DSP-SOUK
         MOVE  "C"      TO  EDIT-CURSOR  OF  DSP-SOUK
         IF  ERR-FLG  =  ZERO
             MOVE  8        TO   ERR-FLG
         END-IF
     ELSE
         MOVE  DSP-SOUK      TO  WK-SOUK
         MOVE  DSP-SOUK      TO  SOK-F01
         PERFORM  SOK-READ-SEC
         IF  INV-FLG  =  "INV"
             MOVE "R"   TO    EDIT-OPTION OF DSP-SOUK
             MOVE "C"   TO    EDIT-CURSOR OF DSP-SOUK
             IF   ERR-FLG  =  ZERO
                  MOVE 14     TO  ERR-FLG
             END-IF
         ELSE
             MOVE "M"    TO   EDIT-OPTION OF DSP-SOUK
             MOVE SPACE  TO   EDIT-CURSOR OF DSP-SOUK
         END-IF
     END-IF.
*原価・売価チッェク
     IF  DSP-BAIKA  <  DSP-GENKA
         MOVE "R"        TO   EDIT-OPTION OF DSP-GENKA
         MOVE "C"        TO   EDIT-CURSOR OF DSP-GENKA
         MOVE "R"        TO   EDIT-OPTION OF DSP-BAIKA
         MOVE "C"        TO   EDIT-CURSOR OF DSP-BAIKA
         IF  ERR-FLG  =  ZERO
             MOVE  12    TO  ERR-FLG
         END-IF
     ELSE
         MOVE  DSP-GENKA   TO  WK-GENKA
         MOVE  DSP-BAIKA   TO  WK-BAIKA
         MOVE "M"        TO   EDIT-OPTION OF DSP-GENKA
         MOVE SPACE      TO   EDIT-CURSOR OF DSP-GENKA
         MOVE "M"        TO   EDIT-OPTION OF DSP-BAIKA
         MOVE SPACE      TO   EDIT-CURSOR OF DSP-BAIKA
     END-IF.
*
 HEAD3-C990.
     IF  DSP-SRKBN  =  2  OR  3
         PERFORM  PRO-HEAD-SET
     END-IF.
 HEAD3-CHK-EXIT.
     EXIT.
****************************************************************
*             明細入力( PSW = 4 )                     2.4      *
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
                PERFORM   BODY-CHK-SEC
***@@DISPLAY "*TESTMSG SRKBN ="  DSP-SRKBN  UPON CONS
                IF  ERR-FLG  =  ZERO
                    PERFORM     DSP-TBL-SEC
                    PERFORM     WORK-DSP-SEC
                    MOVE  "5"   TO   PSW
                END-IF
*取消
         WHEN   "F004"
                MOVE    "1"      TO   PSW
                MOVE    1        TO   P-CNT
                PERFORM     INIT-DSP-SEC
*項目戻り
         WHEN   "F006"
                IF  DSP-SRKBN  =  1
                    MOVE    "3"      TO    PSW
                ELSE
                    MOVE    "2"      TO    PSW
                END-IF
*前頁
         WHEN   "F011"
             PERFORM   BODY-CHK-SEC
             IF   ERR-FLG  =  ZERO
                  PERFORM  DSP-TBL-SEC
** 頁チェック
                COMPUTE C-CNT = P-CNT - 1
                IF      C-CNT = ZERO
                        MOVE  2  TO  ERR-FLG
                ELSE
                        COMPUTE P-CNT = P-CNT - 1
                        MOVE    "SET"   TO   SET-FLG
                        PERFORM WORK-DSP-SEC
                        MOVE    SPACE   TO   SET-FLG
                END-IF
             END-IF
*次頁
         WHEN   "F012"
             PERFORM   BODY-CHK-SEC
             IF   ERR-FLG  =  ZERO
                  PERFORM  DSP-TBL-SEC
** 頁チェック
                MOVE    P-CNT   TO      S-CNT
                COMPUTE C-CNT = P-CNT + 1
                IF      C-CNT > MAX-PGCNT
                        MOVE   3   TO   ERR-FLG
                ELSE
                  IF    TBL-TORICD(C-CNT 1)  =  ZERO
                        MOVE   3     TO    ERR-FLG
                  ELSE
                        COMPUTE P-CNT = P-CNT + 1
                        MOVE    "SET"   TO   SET-FLG
                        PERFORM WORK-DSP-SEC
                        MOVE    SPACE   TO   SET-FLG
                  END-IF
                END-IF
             END-IF
         WHEN   OTHER
                MOVE     1       TO   ERR-FLG
     END-EVALUATE.
*
 DSP-BODY-EXIT.
     EXIT.
****************************************************************
*             明細部チェック                          2.4.1    *
****************************************************************
 BODY-CHK-SEC             SECTION.
     MOVE     "BODY-CHK-SEC"     TO   S-NAME.
     IF  DSP-SRKBN  =  3
         GO  TO    BODY-CHK-EXIT
     END-IF.
*
     PERFORM VARYING Y FROM 1 BY 1 UNTIL  Y  >  MAX-LNCNT
*作成区分チェック
     IF  DSP-SAKBN(Y)  =  SPACE  OR  "9"
         MOVE  "M"      TO  EDIT-OPTION  OF  DSP-SAKBN(Y)
         MOVE   SPACE   TO  EDIT-CURSOR  OF  DSP-SAKBN(Y)
     ELSE
         MOVE  "R"      TO  EDIT-OPTION  OF  DSP-SAKBN(Y)
         MOVE  "C"      TO  EDIT-CURSOR  OF  DSP-SAKBN(Y)
         IF  ERR-FLG  =  ZERO
             MOVE  10   TO  ERR-FLG
         END-IF
     END-IF
*取引先チェック
     IF  ( DSP-TORICD(Y)  NUMERIC )  AND
         ( DSP-TORICD(Y)  NOT = ZERO ) AND
         ( DSP-SAKBN(Y)  =  SPACE )
         PERFORM   HTOKMS-READ-SEC
         IF  HTOKMS-INV-FLG  =  "INV"
             MOVE  "R"      TO  EDIT-OPTION  OF  DSP-TORICD(Y)
             MOVE  "C"      TO  EDIT-CURSOR  OF  DSP-TORICD(Y)
             IF  ERR-FLG  =  ZERO
                 MOVE  15   TO  ERR-FLG
             END-IF
         ELSE
             MOVE  "M"      TO  EDIT-OPTION  OF  DSP-TORICD(Y)
             MOVE  SPACE    TO  EDIT-CURSOR  OF  DSP-TORICD(Y)
         END-IF
     END-IF
*ＪＡＮコードチェック
     IF  ( DSP-TJANCD(Y)  NOT = SPACE ) AND
         ( DSP-SAKBN(Y)  =  SPACE )
*変換テーブルチェック
          PERFORM   SHOTBL-READ-SEC
          IF  INV-FLG  =  SPACE
              MOVE  "R"      TO  EDIT-OPTION  OF  DSP-TJANCD(Y)
              MOVE  "C"      TO  EDIT-CURSOR  OF  DSP-TJANCD(Y)
              IF  ERR-FLG  =  ZERO
                  MOVE  16   TO  ERR-FLG
              END-IF
          ELSE
*データ存在チェック
            IF  DSP-SRKBN  =  1  AND
                ERR-FLG  =  ZERO
                PERFORM   SHTBLM-READ-SEC
                IF  INV-FLG  =  SPACE
                MOVE  "R"   TO  EDIT-OPTION  OF  DSP-TORICD(Y)
                MOVE  "C"   TO  EDIT-CURSOR  OF  DSP-TORICD(Y)
************### MOVE  "R"   TO  EDIT-OPTION  OF  DSP-TJANCD(Y)
************### MOVE  "C"   TO  EDIT-CURSOR  OF  DSP-TJANCD(Y)
                IF  ERR-FLG  =  ZERO
                    MOVE  18   TO  ERR-FLG
                END-IF
            ELSE
              MOVE  "M"      TO  EDIT-OPTION  OF  DSP-TJANCD(Y)
              MOVE  SPACE    TO  EDIT-CURSOR  OF  DSP-TJANCD(Y)
            END-IF
          END-IF
     ELSE
       IF  ( DSP-TORICD(Y)  NUMERIC )  AND
           ( DSP-TJANCD(Y) =  SPACE )  AND
           ( DSP-SAKBN(Y)  =  SPACE )
           MOVE  "R"      TO  EDIT-OPTION  OF  DSP-TJANCD(Y)
           MOVE  "C"      TO  EDIT-CURSOR  OF  DSP-TJANCD(Y)
           IF  ERR-FLG  =  ZERO
               MOVE   7   TO  ERR-FLG
           END-IF
       END-IF
     END-IF
*倉庫チェック
     IF ( DSP-TSOUK(Y) NOT =  SPACE )  AND
        ( DSP-SAKBN(Y)  =  SPACE )
         MOVE  DSP-TSOUK(Y)  TO  SOK-F01
         PERFORM   SOK-READ-SEC
         IF  INV-FLG  =  "INV"
             MOVE  "R"      TO  EDIT-OPTION  OF  DSP-TSOUK(Y)
             MOVE  "C"      TO  EDIT-CURSOR  OF  DSP-TSOUK(Y)
             IF  ERR-FLG  =  ZERO
                 MOVE  14   TO  ERR-FLG
             END-IF
         ELSE
             MOVE  "M"      TO  EDIT-OPTION  OF  DSP-TSOUK(Y)
             MOVE  SPACE    TO  EDIT-CURSOR  OF  DSP-TSOUK(Y)
         END-IF
     END-IF
*原価・売価チェック
     IF  ( DSP-TGENK(Y) NUMERIC )  AND
         ( DSP-TBAIK(Y) NUMERIC )  AND
         ( DSP-SAKBN(Y)  =  SPACE )
       IF  DSP-TBAIK(Y)  <  DSP-TGENK(Y)
           MOVE "R"         TO  EDIT-OPTION  OF DSP-TGENK(Y)
           MOVE "C"         TO  EDIT-CURSOR  OF DSP-TGENK(Y)
           MOVE "R"         TO  EDIT-OPTION  OF DSP-TBAIK(Y)
           MOVE "C"         TO  EDIT-CURSOR  OF DSP-TBAIK(Y)
         IF  ERR-FLG  =  ZERO
             MOVE  12    TO  ERR-FLG
         END-IF
       ELSE
           MOVE "M"         TO  EDIT-OPTION  OF DSP-TGENK(Y)
           MOVE SPACE       TO  EDIT-CURSOR  OF DSP-TGENK(Y)
           MOVE "M"         TO  EDIT-OPTION  OF DSP-TBAIK(Y)
           MOVE SPACE       TO  EDIT-CURSOR  OF DSP-TBAIK(Y)
       END-IF
     END-IF
*取引先ダブリチェック
     IF  ERR-FLG  =  ZERO
         PERFORM  WTORICD-CHK-SEC
     END-IF
     END-PERFORM.
*
 BODY-CHK-EXIT.
     EXIT.
****************************************************************
*             取引先コードダブリチェック              2.4.1.1  *
****************************************************************
 WTORICD-CHK-SEC          SECTION.
     MOVE     "WTORICD-CHK-SEC"   TO   S-NAME.
*テーブルチェック
     MOVE      ZERO     TO    WK-SEQ.
     MOVE      SPACE    TO    WC-FLG.
     PERFORM VARYING IX FROM 1 BY 1 UNTIL IX  >  MAX-PGCNT
                                    OR  WC-FLG  = "Y"
        PERFORM VARYING IY FROM 1 BY 1 UNTIL  IY  >  MAX-LNCNT
                                    OR  WC-FLG  = "Y"
           ADD     1    TO     WK-SEQ
********###IF  DSP-SAKBN(Y)  =  SPACE  AND
           IF  DSP-TORICD(Y)  NUMERIC  AND
               DSP-TORICD(Y)  NOT = ZERO  AND
************###TBL-SAKBN(IX IY)  =  SPACE AND
               DSP-TORICD(Y)  =  TBL-TORICD(IX IY) AND
               TBL-TORICD(P-CNT Y)  NOT = TBL-TORICD(IX IY)
               MOVE  "Y"   TO    WC-FLG
           END-IF
        END-PERFORM
     END-PERFORM.
     IF  WC-FLG  = "Y"
         MOVE  "R"      TO  EDIT-OPTION  OF  DSP-TORICD(Y)
         MOVE  "C"      TO  EDIT-CURSOR  OF  DSP-TORICD(Y)
         IF  ERR-FLG  =  ZERO
             MOVE  11   TO  ERR-FLG
         END-IF
     ELSE
         MOVE  "M"      TO  EDIT-OPTION  OF  DSP-TORICD(Y)
         MOVE  SPACE    TO  EDIT-CURSOR  OF  DSP-TORICD(Y)
     END-IF.
*
 WTORI-CHK-EXIT.
     EXIT.
****************************************************************
*             確認処理　入力（ PSW = 5 ）             2.5
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
                PERFORM  MENT-WRITE-SEC
                PERFORM  DSP-BODYCL-SEC
                MOVE     1       TO   P-CNT
                MOVE    "2"      TO   PSW
*取消
         WHEN   "F004"
                PERFORM  INIT-DSP-SEC
                MOVE    "1"      TO   PSW
                MOVE     1       TO   P-CNT
*終了
         WHEN   "F005"
                MOVE    "END"    TO   END-FLG
                MOVE    "Y"      TO   END-SW
*項目戻り
         WHEN   "F006"
                IF  DSP-SRKBN  =  3
                    MOVE  "2"    TO   PSW
                    MOVE   1     TO   P-CNT
                ELSE
                    MOVE  "4"    TO   PSW
                END-IF
         WHEN   OTHER
                MOVE     1       TO   ERR-FLG
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
     EVALUATE   PSW
         WHEN   "1"
           MOVE    PF-MSG-R(1)        TO   DSP-GUIDE
         WHEN   "2"
           MOVE    PF-MSG-R(2)        TO   DSP-GUIDE
         WHEN   "3"
           MOVE    PF-MSG-R(2)        TO   DSP-GUIDE
         WHEN   "4"
           MOVE    PF-MSG-R(3)        TO   DSP-GUIDE
         WHEN   "5"
           MOVE    PF-MSG-R(2)        TO   DSP-GUIDE
     END-EVALUATE.
*画面の表示
     MOVE    "SCREEN"        TO   DSP-GRP.
     MOVE    "FMT990"        TO   DSP-FMT.
     WRITE    DSP-FMT990.
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
*選択
         WHEN   "1"
                MOVE    "HEAD"    TO   DSP-GRP
*商品ＣＤ
         WHEN   "2"
                MOVE    "HEAD2"   TO   DSP-GRP
*ＪＡＮコード
         WHEN   "3"
                MOVE    "HEAD3"   TO   DSP-GRP
*明細
         WHEN   "4"
                MOVE    "BODY"    TO   DSP-GRP
*確認
         WHEN   "5"
                MOVE    "KAKU"    TO   DSP-GRP
     END-EVALUATE.

     MOVE    "FMT990"             TO   DSP-FMT.
     READ    DSPFILE.
*入力項目の属性を通常にする
     MOVE    ZERO                 TO   ERR-FLG.
     MOVE    SPACE                TO   DSP-PRO.
*
 DSP-READ-EXIT.
     EXIT.
****************************************************************
*             画面制御項目初期化                               *
****************************************************************
 DSP-SYOKI-SEC         SECTION.
     MOVE     "DSP-SYOKI-SEC"    TO   S-NAME.
*リバース，カーソルパーク解除
***  処理区分
     MOVE "M"   TO EDIT-OPTION OF DSP-SRKBN.
     MOVE SPACE TO EDIT-CURSOR OF DSP-SRKBN.
***  一括処理
     MOVE "M"   TO EDIT-OPTION OF DSP-IKATU.
     MOVE SPACE TO EDIT-CURSOR OF DSP-IKATU.
***  商品ＣＤ
     MOVE "M"   TO EDIT-OPTION OF DSP-SSHO1.
     MOVE SPACE TO EDIT-CURSOR OF DSP-SSHO1.
***  品単
     MOVE "M"   TO EDIT-OPTION OF DSP-SSHO2.
     MOVE SPACE TO EDIT-CURSOR OF DSP-SSHO2.
     MOVE "M"   TO EDIT-OPTION OF DSP-SSHO3.
     MOVE SPACE TO EDIT-CURSOR OF DSP-SSHO3.
     MOVE "M"   TO EDIT-OPTION OF DSP-SSHO4.
     MOVE SPACE TO EDIT-CURSOR OF DSP-SSHO4.
***  ＪＡＮＣＤ
     MOVE "M"   TO EDIT-OPTION OF DSP-JANCD.
     MOVE SPACE TO EDIT-CURSOR OF DSP-JANCD.
***  _番
     MOVE "M"   TO EDIT-OPTION OF DSP-TANAB.
     MOVE SPACE TO EDIT-CURSOR OF DSP-TANAB.
***  倉庫
     MOVE "M"   TO EDIT-OPTION OF DSP-SOUK.
     MOVE SPACE TO EDIT-CURSOR OF DSP-SOUK.
***  原価
     MOVE "M"   TO EDIT-OPTION OF DSP-GENKA.
     MOVE SPACE TO EDIT-CURSOR OF DSP-GENKA.
***  売価
     MOVE "M"   TO EDIT-OPTION OF DSP-BAIKA.
     MOVE SPACE TO EDIT-CURSOR OF DSP-BAIKA.
*
 DSP-SYOKI-EXIT.
     EXIT.
****************************************************************
*         明細画面制御項目初期化                               *
****************************************************************
 DSP-BODYCL-SEC         SECTION.
     MOVE     "DSP-BODYCL-SEC"    TO   S-NAME.
     PERFORM VARYING Y FROM 1 BY 1 UNTIL  Y  >  MAX-LNCNT
         MOVE    SPACE    TO  DSP-BDY(Y)
*リバース，カーソルパーク解除
***  作成区分
         MOVE  "M"      TO  EDIT-OPTION  OF  DSP-SAKBN(Y)
         MOVE   SPACE   TO  EDIT-CURSOR  OF  DSP-SAKBN(Y)
***  取引先
         MOVE  "M"      TO  EDIT-OPTION  OF  DSP-TORICD(Y)
         MOVE  SPACE    TO  EDIT-CURSOR  OF  DSP-TORICD(Y)
***  ＪＡＮＣＤ
         MOVE "M"   TO EDIT-OPTION OF DSP-TJANCD(Y)
         MOVE SPACE TO EDIT-CURSOR OF DSP-TJANCD(Y)
***  _番
         MOVE "M"   TO EDIT-OPTION OF DSP-TTANAB(Y)
         MOVE SPACE TO EDIT-CURSOR OF DSP-TTANAB(Y)
***  倉庫
         MOVE "M"   TO EDIT-OPTION OF DSP-TSOUK(Y)
         MOVE SPACE TO EDIT-CURSOR OF DSP-TSOUK(Y)
***  原価
         MOVE "M"   TO EDIT-OPTION OF DSP-TGENK(Y)
         MOVE SPACE TO EDIT-CURSOR OF DSP-TGENK(Y)
***  売価
         MOVE "M"   TO EDIT-OPTION OF DSP-TBAIK(Y)
         MOVE SPACE TO EDIT-CURSOR OF DSP-BAIKA(Y)
     END-PERFORM.
*
 DSP-BODYCL-EXIT.
     EXIT.
****************************************************************
*             画面制御項目プロテクト（ヘッド部）               *
****************************************************************
 PRO-HEAD-SET         SECTION.
     MOVE     "PRO-HEAD-SET"    TO   S-NAME.
*未入力項目にプロテクト
***  ＪＡＮＣＤ
     MOVE "X"   TO EDIT-STATUS OF DSP-JANCD.
***  _番
     MOVE "X"   TO EDIT-STATUS OF DSP-TANAB.
***  倉庫
     MOVE "X"   TO EDIT-OPTION OF DSP-SOUK.
***  原価
     MOVE "X"   TO EDIT-OPTION OF DSP-GENKA.
***  売価
     MOVE "X"   TO EDIT-OPTION OF DSP-BAIKA.
*
 PRO-HEAD-EXIT.
     EXIT.
****************************************************************
*             画面制御項目プロテクト（明細部）        2.4.3.1  *
****************************************************************
 PRO-BODY-SET         SECTION.
     MOVE     "PRO-BODY-SET"    TO   S-NAME.
*未入力項目にプロテクト
***  取引先コード
     MOVE "X"   TO EDIT-STATUS OF DSP-TORICD(Y).
***  ＪＡＮＣＤ
     MOVE "X"   TO EDIT-STATUS OF DSP-TJANCD(Y).
*未入力行の判定
     IF  ( DSP-SRKBN  =  2 )  AND
         ( DSP-TORICD(Y)  NUMERIC  )  AND
         ( DSP-TORICD(Y)  NOT = ZERO )
         GO  TO    PRO-BODY-EXIT
     END-IF.
***  作成
     MOVE "X"   TO EDIT-STATUS OF DSP-SAKBN(Y).
***  _番
     MOVE "X"   TO EDIT-STATUS OF DSP-TTANAB(Y).
***  倉庫
     MOVE "X"   TO EDIT-STATUS OF DSP-TSOUK(Y).
***  原価
     MOVE "X"   TO EDIT-STATUS OF DSP-TGENK(Y).
***  売価
     MOVE "X"   TO EDIT-STATUS OF DSP-TBAIK(Y).
*
 PRO-BODY-EXIT.
     EXIT.
****************************************************************
*             データ→ワーク（退避）                  2.3.2    *
****************************************************************
 MDT-WORK-SEC          SECTION.
     MOVE     "MDT-WORK-SEC"   TO   S-NAME.
*テーブルクリア
     MOVE      ZERO     TO    WK-SEQ.
     MOVE      SPACE    TO    RD-FLG.
     PERFORM VARYING X FROM 1 BY 1 UNTIL  X  >  MAX-PGCNT
        PERFORM VARYING Y FROM 1 BY 1 UNTIL  Y  >  MAX-LNCNT
                INITIALIZE     TABLE2(X Y)
        END-PERFORM
     END-PERFORM.
     IF  DSP-SRKBN  =  2 OR 3
         GO  TO    MDT-W0020
     END-IF.
     IF  DSP-IKATU  NOT  =  1
         GO  TO    MDT-W9990
     END-IF.
*得意先マスタ位置付け
     MOVE     SPACE           TO  RD-FLG.
     MOVE     ZERO            TO  TOK-F01.
     START    HTOKMS    KEY  IS  >  TOK-F01
          INVALID
              MOVE  "END"    TO  RD-FLG
     END-START.
*マスタを読込みワークへセット
     PERFORM VARYING X FROM 1 BY 1 UNTIL  X  >  MAX-PGCNT
                                   OR RD-FLG = "END"
        PERFORM VARYING Y FROM 1 BY 1 UNTIL  Y  >  MAX-LNCNT
                                   OR RD-FLG = "END"
                PERFORM  HTOKMS-RNEXT-SEC
                PERFORM  WORK-TBL-SEC
        END-PERFORM
     END-PERFORM.
     GO  TO    MDT-W9990.
 MDT-W0020.
*メンテデータ位置付け
     MOVE     SPACE           TO  RD-FLG.
     MOVE     WK-SSHOCD       TO  STM-F031.
     MOVE     WK-SHINCD       TO  STM-F032.
     MOVE     ZERO            TO  STM-F01.
     START    SHTBLM    KEY  IS  >  STM-F031 STM-F032 STM-F01
          INVALID
              MOVE  "END"    TO  RD-FLG
     END-START.
*メンテデータを読込みワークへセット
     PERFORM VARYING X FROM 1 BY 1 UNTIL  X  >  MAX-PGCNT
                                   OR RD-FLG = "END"
        PERFORM VARYING Y FROM 1 BY 1 UNTIL  Y  >  MAX-LNCNT
                                   OR RD-FLG = "END"
                PERFORM  SHTBLM-RNEXT-SEC
                PERFORM  WORK2-TBL-SEC
        END-PERFORM
     END-PERFORM.
**
 MDT-W9990.
*
 MDT-WORK-EXIT.
     EXIT.
****************************************************************
*             ワークテーブルセット                   2.3.2.1   *
****************************************************************
 WORK-TBL-SEC          SECTION.
     MOVE     "WORK-TBL-SEC"      TO   S-NAME.
*
     IF  RD-FLG  =  "END"
         GO  TO    WORK-TBL-EXIT
     END-IF.
*
     MOVE  TOK-F01      TO   TBL-TORICD(X Y).
     MOVE  WK-JANCD     TO   TBL-TJANCD(X Y).
     MOVE  WK-SOUK      TO   TBL-TSOUK(X Y).
     MOVE  WK-GENKA     TO   TBL-TGENKA(X Y).
     MOVE  WK-BAIKA     TO   TBL-TBAIKA(X Y).
     MOVE  WK-TANAB     TO   TBL-TTANAB(X Y).
     MOVE  SYS-DATE     TO   TBL-TOROKUHI(X Y).
     MOVE  ZERO         TO   TBL-KOUSINHI(X Y).
     MOVE  SPACE        TO   TBL-SAKBN(X Y).
*
 WORK-TBL-EXIT.
     EXIT.
****************************************************************
*             ワークテーブルセット２                 2.3.2.2   *
****************************************************************
 WORK2-TBL-SEC          SECTION.
     MOVE     "WORK2-TBL-SEC"      TO   S-NAME.
*
     IF  RD-FLG  =  "END"
         GO  TO    WORK2-TBL-EXIT
     END-IF.
*
     MOVE  STM-F01      TO   TBL-TORICD(X Y).
     MOVE  STM-F02      TO   TBL-TJANCD(X Y).
     MOVE  STM-F04      TO   TBL-TSOUK(X Y).
     MOVE  STM-F05      TO   TBL-TGENKA(X Y).
     MOVE  STM-F06      TO   TBL-TBAIKA(X Y).
     MOVE  STM-F08      TO   TBL-TTANAB(X Y).
     MOVE  STM-F98      TO   TBL-TOROKUHI(X Y).
     MOVE  STM-F99      TO   TBL-KOUSINHI(X Y).
     MOVE  STM-F21      TO   TBL-SAKBN(X Y).
*
     ADD    1     TO    WK-SEQ.
*
 WORK2-TBL-EXIT.
     EXIT.
****************************************************************
*             画面→ワーク                            2.4.2    *
****************************************************************
 DSP-TBL-SEC          SECTION.
     MOVE     "DSP-TBL-SEC"      TO   S-NAME.
     IF   DSP-SRKBN  =  3
          GO  TO    DSP-TBL-EXIT
     END-IF.
*画面項目転送
     PERFORM VARYING Y FROM 1 BY 1 UNTIL  Y  >  MAX-LNCNT
         IF  DSP-BDY(Y)  NOT  =  SPACE
             MOVE    DSP-SAKBN(Y)      TO  TBL-SAKBN(P-CNT Y)
             MOVE    DSP-TORICD(Y)     TO  TBL-TORICD(P-CNT Y)
             MOVE    DSP-TJANCD(Y)     TO  TBL-TJANCD(P-CNT Y)
             MOVE    DSP-TTANAB(Y)     TO  TBL-TTANAB(P-CNT Y)
             MOVE    DSP-TSOUK(Y)      TO  TBL-TSOUK(P-CNT Y)
             MOVE    DSP-TGENK(Y)      TO  TBL-TGENKA(P-CNT Y)
             MOVE    DSP-TBAIK(Y)      TO  TBL-TBAIKA(P-CNT Y)
             MOVE    SYS-DATE          TO  TBL-TOROKUHI(P-CNT Y)
             IF  DSP-SRKBN  =  1
                 MOVE  ZERO            TO  TBL-KOUSINHI(P-CNT Y)
             ELSE
                 MOVE  SYS-DATE        TO  TBL-KOUSINHI(P-CNT Y)
             END-IF
         ELSE
             INITIALIZE   TABLE2(P-CNT Y)
         END-IF
     END-PERFORM.
*
 DSP-TBL-EXIT.
     EXIT.
****************************************************************
*             ワーク→画面表示                        2.4.3    *
****************************************************************
 WORK-DSP-SEC          SECTION.
     MOVE     "WORK-DSP-SEC"      TO   S-NAME.
*項目画面セット
     PERFORM VARYING Y FROM 1 BY 1 UNTIL  Y  >  MAX-LNCNT
         IF  ( TBL-TORICD(P-CNT Y)  NUMERIC ) AND
             ( TBL-TORICD(P-CNT Y)  NOT = ZERO )
             MOVE    TBL-SAKBN(P-CNT Y)    TO  DSP-SAKBN(Y)
             MOVE    TBL-TORICD(P-CNT Y)   TO  DSP-TORICD(Y)
             MOVE    TBL-TJANCD(P-CNT Y)   TO  DSP-TJANCD(Y)
             MOVE    TBL-TTANAB(P-CNT Y)   TO  DSP-TTANAB(Y)
             MOVE    TBL-TSOUK(P-CNT Y)    TO  DSP-TSOUK(Y)
             MOVE    TBL-TGENKA(P-CNT Y)   TO  DSP-TGENK(Y)
             MOVE    TBL-TBAIKA(P-CNT Y)   TO  DSP-TBAIK(Y)
             PERFORM  HTOKMS-READ-SEC
         ELSE
             MOVE    SPACE    TO  DSP-BDY(Y)
         END-IF
         IF  DSP-SRKBN = 2 OR 3
             PERFORM  PRO-BODY-SET
         END-IF
     END-PERFORM.
*
 WORK-DSP-EXIT.
     EXIT.
****************************************************************
*             メンテデータ読込み（乱）               2.4.1.1   *
****************************************************************
 SHTBLM-READ-SEC       SECTION.
     MOVE    "SHTBLM-READ-SEC"   TO   S-NAME.
*マスタ読込み
     MOVE    DSP-TORICD(Y)       TO   STM-F01.
     MOVE    WK-SSHOCD           TO   STM-F031.
     MOVE    WK-SHINCD           TO   STM-F032.
     READ  SHTBLM  INVALID
             MOVE   "INV"        TO   INV-FLG
         NOT  INVALID
             MOVE   SPACE        TO   INV-FLG
     END-READ.
*
 SHTBLM-READ-EXIT.
     EXIT.
****************************************************************
*             メンテデータ読込み                               *
****************************************************************
 SHTBLM-RNEXT-SEC      SECTION.
     MOVE     "SHTBLM-RNEXT-SEC"  TO   S-NAME.
*マスタ読込み
     READ  SHTBLM  NEXT   AT END
          MOVE   "END"            TO   RD-FLG
          GO   TO            SHTBLM-RNEXT-EXIT
     END-READ.
*対象の商品ＣＤのみ有効
     IF   STM-F031  =  WK-SSHOCD  AND
          STM-F032  =  WK-SHINCD
             CONTINUE
     ELSE
          MOVE   "END"            TO   RD-FLG
     END-IF.
*
 SHTBLM-RNEXT-EXIT.
     EXIT.
****************************************************************
*             メンテデータ書き込み                   2.5.1     *
****************************************************************
 MENT-WRITE-SEC      SECTION.
     MOVE     "MENT-WRITE-SEC"  TO   S-NAME.
     IF  DSP-SRKBN  =  1
         GO  TO    MENT-W0010
     END-IF.
     MOVE   SPACE      TO   RD-FLG.
*データ削除
     PERFORM VARYING X FROM 1 BY 1 UNTIL  X  >  MAX-PGCNT
                                   OR RD-FLG = "END"
        PERFORM VARYING Y FROM 1 BY 1 UNTIL Y > MAX-LNCNT
                                   OR RD-FLG = "END"
                IF  ( TBL-TORICD(X Y)  NOT = ZERO )
                  AND  ( TBL-TJANCD(X Y) NOT = SPACE )
                       MOVE  WK-SSHOCD  TO  STM-F031
                       MOVE  WK-SHINCD  TO  STM-F032
                       MOVE  TBL-TORICD(X Y)  TO  STM-F01
                       MOVE  SPACE  TO   INV-FLG
                       READ  SHTBLM  INVALID   KEY
                             MOVE "INV"  TO   INV-FLG
                       END-READ
                       IF  INV-FLG  =  SPACE
                           DELETE  SHTBLM
                           ADD  1  TO  DLT-CNT
                       ELSE
                           ADD  1  TO  ERR-CNT
                       END-IF
                END-IF
        END-PERFORM
     END-PERFORM.
 MENT-W0010.
     IF  DSP-SRKBN  =  3
         GO  TO    MENT-W0020
     END-IF.
     MOVE   SPACE      TO   RD-FLG.
*データ登録
     PERFORM VARYING X FROM 1 BY 1 UNTIL  X  >  MAX-PGCNT
                                   OR RD-FLG = "END"
        PERFORM VARYING Y FROM 1 BY 1 UNTIL Y > MAX-LNCNT
                                   OR RD-FLG = "END"
                IF  ( TBL-TORICD(X Y)  NOT = ZERO )
                  AND  ( TBL-TJANCD(X Y) NOT = SPACE )
                       INITIALIZE   STM-REC
                       MOVE  WK-SSHOCD  TO  STM-F031
                       MOVE  WK-SHINCD  TO  STM-F032
                       MOVE  TBL-TORICD(X Y)  TO  STM-F01
                       MOVE  TBL-TJANCD(X Y)  TO  STM-F02
                       MOVE  TBL-TSOUK(X Y)   TO  STM-F04
                       MOVE  TBL-TGENKA(X Y)  TO  STM-F05
                       MOVE  TBL-TBAIKA(X Y)  TO  STM-F06
                       MOVE  TBL-TTANAB(X Y)  TO  STM-F08
                       MOVE  TBL-SAKBN(X Y)   TO  STM-F21
                       MOVE  TBL-TOROKUHI(X Y)  TO  STM-F98
                       MOVE  TBL-KOUSINHI(X Y)  TO  STM-F99
                       MOVE  SPACE  TO   INV-FLG
                       WRITE  STM-REC    INVALID   KEY
                              MOVE "INV"  TO   INV-FLG
                       END-WRITE
                       IF  INV-FLG  =  "INV"
                           ADD  1   TO  ERR-CNT
                       ELSE
                           ADD  1   TO  WRT-CNT
                       END-IF
                END-IF
        END-PERFORM
     END-PERFORM.
*
 MENT-W0020.
*
 MENT-WRITE-EXIT.
     EXIT.
*--------------------------------------------------------------*
*             商品名称マスタ　　ＲＥＡＤ    　　　　　　　　　
*--------------------------------------------------------------*
 MEI-READ-SEC       SECTION.
     MOVE     SPACE          TO   INV-FLG.
     MOVE     DSP-SSHO1      TO   MEI-F011.
     MOVE     DSP-SSHO2      TO   MEI-F0121.
     MOVE     DSP-SSHO3      TO   MEI-F0122.
     MOVE     DSP-SSHO4      TO   MEI-F0123.
*
     READ     HMEIMS    INVALID   KEY
              MOVE      "INV"         TO   INV-FLG
     END-READ.
 MEI-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  ALL   倉庫マスタ　　　　ＲＥＡＤ　　　　　　　　　 *
*--------------------------------------------------------------*
 SOK-READ-SEC         SECTION.
     MOVE     SPACE          TO   INV-FLG.

     READ     ZSOKMS    INVALID   KEY
              MOVE      "INV"         TO   INV-FLG
     END-READ.
 SOK-READ-EXIT.
     EXIT.
****************************************************************
*             取引先マスタ読込み                               *
****************************************************************
 HTOKMS-RNEXT-SEC      SECTION.
     MOVE     "HTOKMS-RNEXT-SEC"  TO   S-NAME.
*マスタ読込み
     READ  HTOKMS  NEXT   AT END
          MOVE   "END"            TO   RD-FLG
          GO   TO            HTOKMS-RNEXT-EXIT
     END-READ.
*対象の取引先コードのみ有効
     IF   TOK-F01(1:1)  >  0
          MOVE   "END"            TO   RD-FLG
     END-IF.
*
 HTOKMS-RNEXT-EXIT.
     EXIT.
****************************************************************
*             取引先マスタ          ＲＥＡＤ
****************************************************************
 HTOKMS-READ-SEC       SECTION.
*
     MOVE     "HTOKMS-READ-SEC"   TO   S-NAME.
     MOVE   DSP-TORICD(Y)         TO   TOK-F01.
     MOVE   SPACE                 TO   HTOKMS-INV-FLG.
     READ   HTOKMS  INVALID
              MOVE    "INV"       TO   HTOKMS-INV-FLG
              MOVE  ALL NC"＊"    TO   DSP-TORIN(Y)
          NOT  INVALID
              MOVE    SPACE       TO   HTOKMS-INV-FLG
              MOVE  TOK-F03       TO   DSP-TORIN(Y)
     END-READ.
*
 HTOKMS-READ-EXIT.
     EXIT.
****************************************************************
*             商品変換テーブル     ＲＥＡＤ
****************************************************************
 SHOTBL-READ-SEC       SECTION.
*
     MOVE     "SHOTBL-READ-SEC"   TO   S-NAME.
     MOVE   DSP-TORICD(Y)         TO   STB-F01.
     MOVE   DSP-TJANCD(Y)         TO   STB-F02.
     MOVE   SPACE                 TO   INV-FLG.
*
     READ   SHOTBL  INVALID
              MOVE    "INV"       TO   INV-FLG
     END-READ.
*
 SHOTBL-READ-EXIT.
     EXIT.
*******************< PROGRAM-END ZMT990O >*********************
