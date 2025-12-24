# SKZ0100I

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SKZ0100I.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　出荷業務　　　　　　　　　　　　　*
*    業務名　　　　　　　：　業務改善                          *
*    モジュール名　　　　：　出荷内容確認＆完了報告入力        *
*    作成日／更新日　　　：　2011/04/26                        *
*    作成者／更新者　　　：　ＮＡＶ高橋　　　　　　　　　　　　*
*    処理概要　　　　　　：　出荷内容確認を行ない、出荷完了・  *
*                            送信完了の登録を行なう。          *
*                            　　　　　　　　　　　　　　　　　*
*　　更新日／更新者　　　：  2012/07/19 NAV TAKAHASHI          *
*　　　　　　　　　　　　　　部門ＣＤ定義の追加　　　　　　　　*
****************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SKZ0100I.
 AUTHOR.               NAV.
 DATE-WRITTEN.         11/04/26.
****************************************************************
 ENVIRONMENT           DIVISION.
****************************************************************
 CONFIGURATION         SECTION.
 SPECIAL-NAMES.
     CONSOLE      IS   CONS.
*
 INPUT-OUTPUT          SECTION.
 FILE-CONTROL.
*----<< 出荷状況管理ファイル（倉庫＋バッチ＋納品日） >>-*
     SELECT   SYUJISL6  ASSIGN    TO        DA-01-VI-SYUJISL6
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      DYNAMIC
                        RECORD    KEY       JI6-F95   JI6-F05
                                            JI6-F02   JI6-F03
                                            JI6-F04   JI6-F06
                        FILE      STATUS    JI6-ST.
*----<< 出荷状況管理ファイル（納品日＋倉庫＋バッチ） >>-*
     SELECT   SYUJISL7  ASSIGN    TO        DA-01-VI-SYUJISL7
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      DYNAMIC
                        RECORD    KEY       JI7-F95   JI7-F06
                                            JI7-F05   JI7-F02
                                            JI7-F03   JI7-F04
                        FILE      STATUS    JI7-ST.
*----<< 担当者マスタ >>-*
     SELECT   HTANMS    ASSIGN    TO        DA-01-VI-TANMS1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       TAN-F01   TAN-F02
                        FILE      STATUS    TAN-ST.
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
*----<< 日付曜日ファイル >>-*
     SELECT   HIDUKEF   ASSIGN    TO        DA-01-VI-HIDUKEL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       HIZ-F01
                        FILE      STATUS    HIZ-ST.
*----<< 画面定義ファイル  >>-*
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
*----<< 出荷状況管理ファイル（倉庫＋バッチ＋納品日） >>-*
 FD  SYUJISL6           LABEL  RECORD  IS  STANDARD.
     COPY     SYUJISF   OF     XFDLIB
              JOINING   JI6 AS PREFIX.
*----<< 出荷状況管理ファイル（納品日＋倉庫＋バッチ） >>-*      *
 FD  SYUJISL7           LABEL  RECORD  IS  STANDARD.
     COPY     SYUJISF   OF     XFDLIB
              JOINING   JI7 AS PREFIX.
*----<< 担当者マスタ >>-*                                      *
 FD  HTANMS             LABEL  RECORD  IS  STANDARD.
     COPY     HTANMS    OF     XFDLIB
              JOINING   TAN AS PREFIX.
*----<< 取引先マスタ >>-*                                      *
 FD  HTOKMS             LABEL  RECORD  IS  STANDARD.
     COPY     HTOKMS    OF     XFDLIB
              JOINING   TOK AS PREFIX.
*----<< 倉庫マスタ >>-*                                        *
 FD  ZSOKMS             LABEL  RECORD  IS  STANDARD.
     COPY     ZSOKMS1   OF     XFDLIB
              JOINING   SOK AS PREFIX.
*----<< 日付曜日ファイル >>-*
 FD  HIDUKEF            LABEL  RECORD  IS  STANDARD.
     COPY     HIDUKEF   OF     XFDLIB
              JOINING   HIZ AS PREFIX.
*----<< 画面定義ファイル  >>-*                                 *
 FD  DSPFILE            LABEL  RECORD  IS  OMITTED.
     COPY     FKZ01001  OF     XMDLIB
              JOINING   DSP AS PREFIX.
*
****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
*ステータス領域
 01  STATUS-AREA.
     03  JI6-ST                   PIC  X(02).
     03  JI7-ST                   PIC  X(02).
     03  TAN-ST                   PIC  X(02).
     03  TOK-ST                   PIC  X(02).
     03  SOK-ST                   PIC  X(02).
     03  HIZ-ST                   PIC  X(02).
     03  DSP-ST                   PIC  X(02).
*画面制御用領域
 01  DSP-CONTROL.
     03  DSP-FMT                  PIC  X(08).
     03  DSP-GRP                  PIC  X(08).
     03  DSP-PRO                  PIC  X(02).
     03  DSP-FNC                  PIC  X(04).
 01  SET-PGID                     PIC  X(08)  VALUE "SKZ0100I".
 01  SET-FORMID                   PIC  X(08)  VALUE "FKZ01001".
 01  MAX-PGCNT                    PIC  9(03)  VALUE 50.
 01  MAX-LNCNT                    PIC  9(03)  VALUE 06.
 01  MEISAI-CNT                   PIC  9(09)  VALUE ZERO.
 01  MEIALLCNT                    PIC  9(02)  VALUE ZERO.
 01  WK-HIDUKE-1                  PIC  9(08)  VALUE ZERO.
 01  WK-HIDUKE-2                  PIC  9(08)  VALUE ZERO.
 01  WK-HIDUKE-3                  PIC  9(08)  VALUE ZERO.
*フラグ領域
 01  FLG-AREA.
     03  HTOKMS-INV-FLG           PIC  X(03)  VALUE  SPACE.
     03  ZSOKMS-INV-FLG           PIC  X(03)  VALUE  SPACE.
     03  HTANMS-INV-FLG           PIC  X(03)  VALUE  SPACE.
     03  HIDUKEF-INV-FLG          PIC  X(03)  VALUE  SPACE.
     03  SYUJISF-INV-FLG          PIC  X(03)  VALUE  SPACE.
     03  SYUJISF-END-FLG          PIC  X(03)  VALUE  SPACE.
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
     03  WK-SIRTAN                PIC  9(07)V99  VALUE  ZERO.
     03  DLT-CNT                  PIC  9(07)  VALUE  ZERO.
     03  WRT-CNT                  PIC  9(07)  VALUE  ZERO.
     03  ERR-CNT                  PIC  9(07)  VALUE  ZERO.
*バッチ_編集
 01  WK-BATCH-NO.
     03  WK-BATCH-YMD             PIC  9(08)  VALUE  ZERO.
     03  WK-BATCH-FIL1            PIC  X(01)  VALUE  SPACE.
     03  WK-BATCH-TIME            PIC  9(04)  VALUE  ZERO.
     03  WK-BATCH-FIL2            PIC  X(01)  VALUE  SPACE.
     03  WK-BATCH-TOKCD           PIC  9(08)  VALUE  ZERO.
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
             VALUE NC"未入力エラー！！".
     03  ERR-MSG3.
         05  FILLER              PIC   N(20)
             VALUE NC"倉庫マスタ未登録です。".
     03  ERR-MSG4.
         05  FILLER              PIC   N(20)
             VALUE NC"区分選択エラーです。".
     03  ERR-MSG5.
         05  FILLER              PIC   N(20)
             VALUE NC"日付論理エラーです。".
     03  ERR-MSG6.
         05  FILLER              PIC   N(20)
             VALUE NC"取引先マスタ未登録です。".
     03  ERR-MSG7.
         05  FILLER              PIC   N(20)
             VALUE NC"出荷完了区分エラーです。".
     03  ERR-MSG8.
         05  FILLER              PIC   N(20)
             VALUE NC"送信完了区分エラーです。".
     03  ERR-MSG9.
         05  FILLER              PIC   N(20)
             VALUE NC"対象明細が存在しません。".
     03  ERR-MSG10.
         05  FILLER              PIC   N(20)
             VALUE NC"出荷完了担当者が未登録です。".
     03  ERR-MSG11.
         05  FILLER              PIC   N(20)
             VALUE NC"送信完了担当者が未登録です。".
     03  ERR-MSG12.
         05  FILLER              PIC   N(20)
             VALUE NC"前頁がありません。".
     03  ERR-MSG13.
         05  FILLER              PIC   N(20)
             VALUE NC"次頁がありません。".
     03  ERR-MSG14.
         05  FILLER              PIC   N(20)
             VALUE NC"出荷完了担当者コードエラーです。".
     03  ERR-MSG15.
         05  FILLER              PIC   N(20)
             VALUE NC"送信完了担当者コードエラーです。。".
     03  ERR-MSG16.
         05  FILLER              PIC   N(20)
             VALUE NC"出力順が違います。".
     03  ERR-MSG17.
         05  FILLER              PIC   N(20)
             VALUE NC"　".
     03  ERR-MSG18.
         05  FILLER              PIC   N(20)
             VALUE NC"　".
     03  ERR-MSG19.
         05  FILLER              PIC   N(20)
             VALUE NC"　".
     03  ERR-MSG20.
         05  FILLER              PIC   N(20)
             VALUE NC"　".
 01  ERR-MSG-AREA-R      REDEFINES     ERR-MSG-AREA.
     03  ERR-MSG-R   OCCURS 20   PIC   N(20).
*2012/07/19 ST ADD
*部門ＣＤテーブル
 01  WK-BUMON-CD.
     03  WK-BUMON-TAN    OCCURS  6.
         05  WK-SYUBUN   PIC  X(04).
         05  WK-SYUTAN   PIC  X(02).
         05  WK-SOUBUN   PIC  X(04).
         05  WK-SOUTAN   PIC  X(02).
*2012/07/19 ED ADD
*メンテデータ退避エリア
 01  TABLE-AREA.
     03  TABLE1      OCCURS  50.
         05  PAGE-SONZAI-CHK      PIC   X(01).
         05  TABLE2  OCCURS  06.
             07  TBL-MEIKBN       PIC   N(03).
             07  TBL-MEISOK       PIC   X(02).
             07  TBL-MEIBAT       PIC   X(22).
             07  TBL-MEIBAT1      PIC   9(08).
             07  TBL-MEIBAT2      PIC   9(04).
             07  TBL-MEIBAT3      PIC   9(08).
             07  TBL-MEITK1       PIC   N(05).
             07  TBL-MEINOU       PIC   9(08).
             07  TBL-MEIYOB       PIC   N(01).
             07  TBL-MEIDTK       PIC   9(07).
*************2012/07/19 ST ADD
             07  TBL-SYUBUN       PIC   X(04).
*************2012/07/19 ED ADD
             07  TBL-SYUTAN       PIC   X(02).
*************2012/07/19 ST ADD
             07  TBL-SOUBUN       PIC   X(04).
*************2012/07/19 ED ADD
             07  TBL-SOUTAN       PIC   X(02).
             07  TBL-MEISNM       PIC   N(02).
             07  TBL-DENST        PIC   9(09).
             07  TBL-DENED        PIC   9(09).
             07  TBL-MEITK2       PIC   N(05).
             07  TBL-MEIDEK       PIC   9(07).
             07  TBL-SYUTNM       PIC   N(01).
             07  TBL-SOUTNM       PIC   N(01).
             07  TBL-SYUKB1       PIC   X(01).
             07  TBL-SYUKB2       PIC   X(01).
*ファイルエラーメッセージ
 01  FILE-ERR.
     03  JI6-ERR           PIC N(15) VALUE
                        NC"出荷状況管理ファイル６エラー".
     03  JI7-ERR           PIC N(15) VALUE
                        NC"出荷状況管理ファイル７エラー".
     03  TOK-ERR           PIC N(15) VALUE
                        NC"取引先マスタエラー".
     03  SOK-ERR           PIC N(15) VALUE
                        NC"倉庫マスタエラー".
     03  TAN-ERR           PIC N(15) VALUE
                        NC"担当者マスタエラー".
     03  HIZ-ERR           PIC N(15) VALUE
                        NC"日付曜日ファイルエラー".
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
 LINKAGE               SECTION.
 01  LINK-BUMON            PIC X(04).
 01  LINK-TANCD            PIC X(02).
 01  LINK-SOKCD            PIC X(02).
 01  LINK-DSOKCD           PIC X(02).
**************************************************************
 PROCEDURE             DIVISION
                          USING  LINK-BUMON
                                 LINK-TANCD
                                 LINK-SOKCD
                                 LINK-DSOKCD.
**************************************************************
 DECLARATIVES.
 JI6-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE SYUJISL6.
     MOVE        JI6-ST    TO        E-ST.
     MOVE       "SYUJISL6" TO        E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     JI6-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 JI7-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE SYUJISL7.
     MOVE        JI7-ST    TO        E-ST.
     MOVE       "SYUJISL7" TO        E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     JI6-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 TOK-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HTOKMS.
     MOVE        TOK-ST    TO        E-ST.
     MOVE       "TOKMS1"   TO        E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     TOK-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 SOK-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE ZSOKMS.
     MOVE        SOK-ST    TO        E-ST.
     MOVE       "ZSOKMS1"  TO        E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     SOK-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 TAN-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HTANMS.
     MOVE        TAN-ST    TO        E-ST.
     MOVE       "TANMS1"   TO        E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     TAN-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 HIZ-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HIDUKEF.
     MOVE        HIZ-ST    TO        E-ST.
     MOVE       "HIDUKEL1" TO        E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     HIZ-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 DSP-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE DSPFILE.
     MOVE        DSP-ST    TO        E-ST.
     MOVE       "DSPFILE"  TO        E-FILE.
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
     OPEN      INPUT     HTOKMS  ZSOKMS  HTANMS  HIDUKEF.
     OPEN      I-O       DSPFILE.
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
*＋１日取得
     MOVE       "5"           TO     LINK-IN-KBN.
     MOVE        1            TO     LINK-IN-YMD6.
     MOVE        DATE-AREA    TO     LINK-IN-YMD8.
     CALL       "SKYDTCKB"   USING   LINK-IN-KBN
                                     LINK-IN-YMD6
                                     LINK-IN-YMD8
                                     LINK-OUT-RET
                                     LINK-OUT-YMD.
     MOVE        LINK-OUT-YMD TO     WK-HIDUKE-1.
*＋２日取得
     MOVE       "5"           TO     LINK-IN-KBN.
     MOVE        2            TO     LINK-IN-YMD6.
     MOVE        DATE-AREA    TO     LINK-IN-YMD8.
     CALL       "SKYDTCKB"   USING   LINK-IN-KBN
                                     LINK-IN-YMD6
                                     LINK-IN-YMD8
                                     LINK-OUT-RET
                                     LINK-OUT-YMD.
     MOVE        LINK-OUT-YMD TO     WK-HIDUKE-2.
*＋３日取得
     MOVE       "5"           TO     LINK-IN-KBN.
     MOVE        3            TO     LINK-IN-YMD6.
     MOVE        DATE-AREA    TO     LINK-IN-YMD8.
     CALL       "SKYDTCKB"   USING   LINK-IN-KBN
                                     LINK-IN-YMD6
                                     LINK-IN-YMD8
                                     LINK-OUT-RET
                                     LINK-OUT-YMD.
     MOVE        LINK-OUT-YMD TO     WK-HIDUKE-3.
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
*ヘッダ入力
         WHEN      "1"       PERFORM   DSP-HEAD-SEC
*明細入力    (2.4)
         WHEN      "2"       PERFORM   DSP-BODY-SEC
*確認入力    (2.5)
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
     CLOSE   HTOKMS  ZSOKMS  HTANMS  HIDUKEF.
     CLOSE   DSPFILE.
**
 END-EXIT.
     EXIT.
****************************************************************
*             初期画面表示                            1.1      *
****************************************************************
 INIT-DSP-SEC          SECTION.
     MOVE     "INIT-DSP-SEC"      TO   S-NAME.
 INITD001.
*画面の初期化
     MOVE    SPACE                TO   DSP-FKZ01001.
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
*明細頁
     MOVE    ZERO                 TO   DSP-MEICNT  MEIALLCNT.
 INITD004.
*ＰＧＩＤ・ＦＭＴＩＤセット
     MOVE    SET-PGID             TO   DSP-PGID.
     MOVE    SET-FORMID           TO   DSP-FORMID.
*倉庫コードパラメタより倉庫コード入力チェック
     IF       LINK-DSOKCD  =  "01" OR "88"
              MOVE    " "         TO   EDIT-STATUS OF DSP-SOKCD
     ELSE
              MOVE  LINK-SOKCD    TO   SOK-F01 DSP-SOKCD
              PERFORM ZSOKMS-READ-SEC
              IF  ZSOKMS-INV-FLG = "INV"
                  MOVE      ALL NC"＊" TO   DSP-SOKNM
              ELSE
                  MOVE      SOK-F02    TO   DSP-SOKNM
              END-IF
**************プロテクト追加
              MOVE    "X"    TO   EDIT-STATUS OF DSP-SOKCD
     END-IF.
 INITD005.
*項目属性クリア
     PERFORM  DSP-SYOKI-SEC.
     PERFORM  DSP-BODYCL-SEC.
*
     MOVE     1                   TO   P-CNT.
*
 INIT-DSP-EXIT.
     EXIT.
****************************************************************
*    ヘッダ入力
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
               PERFORM  HEAD-CHK-SEC
               IF  ERR-FLG  =  ZERO
                   MOVE  "2"   TO  PSW
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
*    ヘッダ項目チェック
****************************************************************
 HEAD-CHK-SEC              SECTION.
     MOVE     "HEAD-CHK-SEC"      TO   S-NAME.
*倉庫ＣＤチェック
     IF  DSP-SOKCD  =  SPACE
         MOVE SPACE     TO  DSP-SOKCD
         MOVE NC"全倉庫" TO DSP-SOKNM
         MOVE  "M"      TO  EDIT-OPTION  OF  DSP-SOKCD
         MOVE   SPACE   TO  EDIT-CURSOR  OF  DSP-SOKCD
     ELSE
         MOVE DSP-SOKCD TO  SOK-F01
         PERFORM ZSOKMS-READ-SEC
         IF  ZSOKMS-INV-FLG = "INV"
             MOVE   2       TO  ERR-FLG
             MOVE  "R"      TO  EDIT-OPTION  OF  DSP-SOKCD
             MOVE  "C"      TO  EDIT-CURSOR  OF  DSP-SOKCD
         ELSE
             MOVE  SOK-F02  TO  DSP-SOKNM
             MOVE  "M"      TO  EDIT-OPTION  OF  DSP-SOKCD
             MOVE   SPACE   TO  EDIT-CURSOR  OF  DSP-SOKCD
         END-IF
     END-IF.
*データ区分チェック
     IF  DSP-DTKBN  =  SPACE  OR  "1"  OR  "2"
         MOVE  "M"      TO  EDIT-OPTION  OF  DSP-DTKBN
         MOVE   SPACE   TO  EDIT-CURSOR  OF  DSP-DTKBN
     ELSE
         IF  ERR-FLG  =  ZERO
             MOVE   4       TO  ERR-FLG
         END-IF
         MOVE  "R"      TO  EDIT-OPTION  OF  DSP-DTKBN
         MOVE  "C"      TO  EDIT-CURSOR  OF  DSP-DTKBN
     END-IF.
*納品日チェック
     IF  DSP-NOUDT   NOT NUMERIC
     OR  DSP-NOUDT   =   ZERO
         MOVE     ZERO  TO   DSP-NOUDT
     END-IF.
*納品日論理チェック
     IF  DSP-NOUDT  NOT =  ZERO
         MOVE     "2"            TO   LINK-IN-KBN
         MOVE     ZERO           TO   LINK-IN-YMD6
         MOVE     DSP-NOUDT      TO   LINK-IN-YMD8
         MOVE     ZERO           TO   LINK-OUT-RET
         MOVE     ZERO           TO   LINK-OUT-YMD
         CALL     "SKYDTCKB"     USING   LINK-IN-KBN
                                         LINK-IN-YMD6
                                         LINK-IN-YMD8
                                         LINK-OUT-RET
                                         LINK-OUT-YMD
         IF   LINK-OUT-RET   = 9
              IF   ERR-FLG   =    ZERO
                   MOVE      5    TO   ERR-FLG
              END-IF
              MOVE  "R"      TO   EDIT-OPTION OF DSP-NOUDT
              MOVE  "C"      TO   EDIT-CURSOR OF DSP-NOUDT
         ELSE
              MOVE  "M"      TO   EDIT-OPTION OF DSP-NOUDT
              MOVE  SPACE    TO   EDIT-CURSOR OF DSP-NOUDT
         END-IF
     ELSE
         MOVE  "M"           TO   EDIT-OPTION OF DSP-NOUDT
         MOVE  SPACE         TO   EDIT-CURSOR OF DSP-NOUDT
     END-IF.
*取引先チェック
     IF  DSP-TORICD  NOT NUMERIC
     OR  DSP-TORICD  =   ZERO
         MOVE     ZERO  TO   DSP-TORICD
     END-IF.
*
     IF  DSP-TORICD  NOT =  ZERO
         MOVE DSP-TORICD  TO  TOK-F01
         PERFORM HTOKMS-READ-SEC
         IF  HTOKMS-INV-FLG = "INV"
             MOVE   6       TO  ERR-FLG
             MOVE  "R"      TO  EDIT-OPTION  OF  DSP-TORICD
             MOVE  "C"      TO  EDIT-CURSOR  OF  DSP-TORICD
         ELSE
             MOVE  TOK-F02  TO  DSP-TORINM
             MOVE  "M"      TO  EDIT-OPTION  OF  DSP-TORICD
             MOVE   SPACE   TO  EDIT-CURSOR  OF  DSP-TORICD
         END-IF
     ELSE
         MOVE  NC"全取引先"  TO   DSP-TORINM
         MOVE  "M"           TO   EDIT-OPTION OF DSP-TORICD
         MOVE  SPACE         TO   EDIT-CURSOR OF DSP-TORICD
     END-IF.
*出荷完了
     IF  DSP-SYUKBN =  SPACE  OR  "1"  OR  "2"
         MOVE  "M"      TO  EDIT-OPTION  OF  DSP-SYUKBN
         MOVE   SPACE   TO  EDIT-CURSOR  OF  DSP-SYUKBN
     ELSE
         IF  ERR-FLG  =  ZERO
             MOVE   7       TO  ERR-FLG
         END-IF
         MOVE  "R"      TO  EDIT-OPTION  OF  DSP-SYUKBN
         MOVE  "C"      TO  EDIT-CURSOR  OF  DSP-SYUKBN
     END-IF.
*送信完了
     IF  DSP-SOUKBN =  SPACE  OR  "1"  OR  "2"
         MOVE  "M"      TO  EDIT-OPTION  OF  DSP-SOUKBN
         MOVE   SPACE   TO  EDIT-CURSOR  OF  DSP-SOUKBN
     ELSE
         IF  ERR-FLG  =  ZERO
             MOVE   8       TO  ERR-FLG
         END-IF
         MOVE  "R"      TO  EDIT-OPTION  OF  DSP-SOUKBN
         MOVE  "C"      TO  EDIT-CURSOR  OF  DSP-SOUKBN
     END-IF.
*出力順
     IF  DSP-SYUJUN  =  SPACE
         MOVE  "2"      TO  DSP-SYUJUN
     END-IF.
*
     IF  DSP-SYUJUN =  "1"  OR  "2"
         MOVE  "M"      TO  EDIT-OPTION  OF  DSP-SYUJUN
         MOVE   SPACE   TO  EDIT-CURSOR  OF  DSP-SYUJUN
     ELSE
         IF  ERR-FLG  =  ZERO
             MOVE   16      TO  ERR-FLG
         END-IF
         MOVE  "R"      TO  EDIT-OPTION  OF  DSP-SYUJUN
         MOVE  "C"      TO  EDIT-CURSOR  OF  DSP-SYUJUN
     END-IF.
*エラーがあった場合はＥＸＩＴへ
     IF  ERR-FLG  NOT =  ZERO
         GO              TO      HEAD-CHK-EXIT
     END-IF.
*ファイルよりワークにセット
     EVALUATE DSP-SYUJUN
        WHEN  "1"   *>倉庫＋バッチ＋納品日
              PERFORM SYUJISL6-SET-SEC
        WHEN  "2"   *>納品日＋倉庫＋バッチ
              PERFORM SYUJISL7-SET-SEC
     END-EVALUATE.
*明細存在チェック判定
     IF  MEISAI-CNT  =  ZERO
         MOVE   9       TO  ERR-FLG
         MOVE  "R"      TO  EDIT-OPTION  OF  DSP-SOKCD
         MOVE  "R"      TO  EDIT-OPTION  OF  DSP-DTKBN
         MOVE  "R"      TO  EDIT-OPTION  OF  DSP-NOUDT
         MOVE  "R"      TO  EDIT-OPTION  OF  DSP-TORICD
         MOVE  "R"      TO  EDIT-OPTION  OF  DSP-SYUKBN
         MOVE  "R"      TO  EDIT-OPTION  OF  DSP-SOUKBN
         MOVE  "R"      TO  EDIT-OPTION  OF  DSP-SYUJUN
         IF LINK-DSOKCD = "01" OR "88"
            MOVE "C"    TO  EDIT-CURSOR  OF  DSP-SOKCD
         ELSE
            MOVE "C"    TO  EDIT-CURSOR  OF  DSP-DTKBN
         END-IF
     ELSE
         MOVE    1      TO  P-CNT
         PERFORM WORK-DSP-SEC
     END-IF.
*
 HEAD-CHK-EXIT.
     EXIT.
****************************************************************
*    出荷状況管理ファイル読込（ＫＥＹ＝６）
****************************************************************
 SYUJISL6-SET-SEC       SECTION.
*
     MOVE     "SYUJISL6-SET-SEC"   TO   S-NAME.
*
     MOVE      SPACE               TO   SYUJISF-END-FLG.
*ファイルをオープンする。
     OPEN  INPUT  SYUJISL6.
*ワーク領域初期化
     MOVE      SPACE               TO   TABLE-AREA.
     INITIALIZE                         TABLE-AREA.
     MOVE      ZERO                TO   MEISAI-CNT  MEIALLCNT.
*出荷状況管理ファイル読込
     PERFORM   SYUJISL6-READ-SEC.
*ファイルよりワークへセット
     PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > 50
                            OR SYUJISF-END-FLG = "END"
         PERFORM VARYING IY FROM 1 BY 1 UNTIL IY > 6
                            OR SYUJISF-END-FLG = "END"
*          *>明細区分
             MOVE  SPACE              TO  TBL-MEIKBN(IX IY)
             IF  JI6-F01  =  "1"
                 MOVE  NC"オン"       TO  TBL-MEIKBN(IX IY)
             END-IF
             IF  JI6-F01  =  "2"
                 MOVE  NC"手書"       TO  TBL-MEIKBN(IX IY)
             END-IF
*          *>倉庫ＣＤ
             MOVE JI6-F05             TO  TBL-MEISOK(IX IY)
             MOVE JI6-F05             TO  SOK-F01
             PERFORM ZSOKMS-READ-SEC
             IF   ZSOKMS-INV-FLG = "INV"
                  MOVE ALL NC"＊"     TO  TBL-MEISNM(IX IY)
             ELSE
                  MOVE SOK-F02        TO  TBL-MEISNM(IX IY)
             END-IF
*          *>バッチ_
             MOVE JI6-F02             TO  WK-BATCH-YMD
             MOVE JI6-F03             TO  WK-BATCH-TIME
             MOVE JI6-F04             TO  WK-BATCH-TOKCD
             MOVE JI6-F02             TO  TBL-MEIBAT1(IX IY)
             MOVE JI6-F03             TO  TBL-MEIBAT2(IX IY)
             MOVE JI6-F04             TO  TBL-MEIBAT3(IX IY)
             MOVE "-"                 TO  WK-BATCH-FIL1
             MOVE "-"                 TO  WK-BATCH-FIL2
             MOVE WK-BATCH-NO         TO  TBL-MEIBAT(IX IY)
*          *>取引先名
             MOVE JI6-F04             TO  TOK-F01
             PERFORM HTOKMS-READ-SEC
             IF   HTOKMS-INV-FLG = "INV"
                  MOVE ALL NC"＊"     TO  TBL-MEITK1(IX IY)
                  MOVE ALL NC"＊"     TO  TBL-MEITK2(IX IY)
             ELSE
                  MOVE TOK-F02(1:5)   TO  TBL-MEITK1(IX IY)
                  MOVE TOK-F02(6:5)   TO  TBL-MEITK2(IX IY)
             END-IF
*          *>納品日
             MOVE JI6-F06             TO  TBL-MEINOU(IX IY)
*          *>曜日取得
             MOVE JI6-F06             TO  HIZ-F01
             PERFORM HIDUKEF-READ-SEC
             IF   HIDUKEF-INV-FLG = "INV"
                  MOVE NC"＊"         TO  TBL-MEIYOB(IX IY)
             ELSE
                  MOVE HIZ-F03        TO  TBL-MEIYOB(IX IY)
             END-IF
*          *>データ件数
             IF   JI6-F09 > ZERO
                  MOVE JI6-F09        TO  TBL-MEIDTK(IX IY)
             END-IF
*          *>伝票枚数
             MOVE JI6-F10             TO  TBL-MEIDEK(IX IY)
*          *>出荷完了区分
             MOVE JI6-F11             TO  TBL-SYUTAN(IX IY)
             IF   JI6-F11 NOT = SPACE
                  MOVE   NC"完"       TO  TBL-SYUTNM(IX IY)
             ELSE
                  MOVE   SPACE        TO  TBL-SYUTNM(IX IY)
             END-IF
*          *>送信完了区分
             MOVE JI6-F13             TO  TBL-SOUTAN(IX IY)
             IF   JI6-F13 NOT = SPACE
                  MOVE   NC"完"       TO  TBL-SOUTNM(IX IY)
             ELSE
                  MOVE   SPACE        TO  TBL-SOUTNM(IX IY)
             END-IF
*          *>伝票番号
             MOVE JI6-F07             TO  TBL-DENST(IX IY)
             MOVE JI6-F08             TO  TBL-DENED(IX IY)
***********2012/07/19 NAV ST
*          *>出荷／送信部門
             MOVE JI6-F15             TO  TBL-SYUBUN(IX IY)
             MOVE JI6-F16             TO  TBL-SOUBUN(IX IY)
***********2012/07/19 NAV ED
*          *>出荷状況管理ファイル読込
             PERFORM SYUJISL6-READ-SEC
*          *>修正区分
             MOVE SPACE               TO  TBL-SYUKB1(IX IY)
             MOVE SPACE               TO  TBL-SYUKB2(IX IY)
*
             MOVE    "1"              TO  PAGE-SONZAI-CHK(IX)
             MOVE    IX               TO  MEIALLCNT
*
             ADD      1               TO  MEISAI-CNT
         END-PERFORM
     END-PERFORM.
*
     CLOSE  SYUJISL6.
*
 SYUJISL6-SET-EXIT.
     EXIT.
****************************************************************
*    出荷状況管理ファイル読込（ＫＥＹ＝６）
****************************************************************
 SYUJISL7-SET-SEC       SECTION.
*
     MOVE     "SYUJISL7-SET-SEC"   TO   S-NAME.
*
     MOVE      SPACE               TO   SYUJISF-END-FLG.
*ファイルをオープンする。
     OPEN  INPUT  SYUJISL7.
*ワーク領域初期化
     MOVE      SPACE               TO   TABLE-AREA.
     INITIALIZE                         TABLE-AREA.
     MOVE      ZERO                TO   MEISAI-CNT  MEIALLCNT.
*出荷状況管理ファイル読込
     PERFORM   SYUJISL7-READ-SEC.
*ファイルよりワークへセット
     PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > 50
                            OR SYUJISF-END-FLG = "END"
         PERFORM VARYING IY FROM 1 BY 1 UNTIL IY > 6
                            OR SYUJISF-END-FLG = "END"
*          *>明細区分
             MOVE  SPACE              TO  TBL-MEIKBN(IX IY)
             IF  JI7-F01  =  "1"
                 MOVE  NC"オン"       TO  TBL-MEIKBN(IX IY)
             END-IF
             IF  JI7-F01  =  "2"
                 MOVE  NC"手書"       TO  TBL-MEIKBN(IX IY)
             END-IF
*          *>倉庫ＣＤ
             MOVE JI7-F05             TO  TBL-MEISOK(IX IY)
             MOVE JI7-F05             TO  SOK-F01
             PERFORM ZSOKMS-READ-SEC
             IF   ZSOKMS-INV-FLG = "INV"
                  MOVE ALL NC"＊"     TO  TBL-MEISNM(IX IY)
             ELSE
                  MOVE SOK-F02        TO  TBL-MEISNM(IX IY)
             END-IF
*          *>バッチ_
             MOVE JI7-F02             TO  WK-BATCH-YMD
             MOVE JI7-F03             TO  WK-BATCH-TIME
             MOVE JI7-F04             TO  WK-BATCH-TOKCD
             MOVE JI7-F02             TO  TBL-MEIBAT1(IX IY)
             MOVE JI7-F03             TO  TBL-MEIBAT2(IX IY)
             MOVE JI7-F04             TO  TBL-MEIBAT3(IX IY)
             MOVE "-"                 TO  WK-BATCH-FIL1
             MOVE "-"                 TO  WK-BATCH-FIL2
             MOVE WK-BATCH-NO         TO  TBL-MEIBAT(IX IY)
*          *>取引先名
             MOVE JI7-F04             TO  TOK-F01
             PERFORM HTOKMS-READ-SEC
             IF   HTOKMS-INV-FLG = "INV"
                  MOVE ALL NC"＊"     TO  TBL-MEITK1(IX IY)
                  MOVE ALL NC"＊"     TO  TBL-MEITK2(IX IY)
             ELSE
                  MOVE TOK-F02(1:5)   TO  TBL-MEITK1(IX IY)
                  MOVE TOK-F02(6:5)   TO  TBL-MEITK2(IX IY)
             END-IF
*          *>納品日
             MOVE JI7-F06             TO  TBL-MEINOU(IX IY)
*          *>曜日取得
             MOVE JI7-F06             TO  HIZ-F01
             PERFORM HIDUKEF-READ-SEC
             IF   HIDUKEF-INV-FLG = "INV"
                  MOVE NC"＊"         TO  TBL-MEIYOB(IX IY)
             ELSE
                  MOVE HIZ-F03        TO  TBL-MEIYOB(IX IY)
             END-IF
*          *>データ件数
             IF   JI7-F09 > ZERO
                  MOVE JI7-F09        TO  TBL-MEIDTK(IX IY)
             END-IF
*          *>伝票枚数
             MOVE JI7-F10             TO  TBL-MEIDEK(IX IY)
*          *>出荷完了区分
             MOVE JI7-F11             TO  TBL-SYUTAN(IX IY)
             IF   JI7-F11 NOT = SPACE
                  MOVE   NC"完"       TO  TBL-SYUTNM(IX IY)
             ELSE
                  MOVE   SPACE        TO  TBL-SYUTNM(IX IY)
             END-IF
*          *>送信完了区分
             MOVE JI7-F13             TO  TBL-SOUTAN(IX IY)
             IF   JI7-F13 NOT = SPACE
                  MOVE   NC"完"       TO  TBL-SOUTNM(IX IY)
             ELSE
                  MOVE   SPACE        TO  TBL-SOUTNM(IX IY)
             END-IF
*          *>伝票番号
             MOVE JI7-F07             TO  TBL-DENST(IX IY)
             MOVE JI7-F08             TO  TBL-DENED(IX IY)
***********2012/07/19 NAV ST
*          *>出荷／送信部門
             MOVE JI7-F15             TO  TBL-SYUBUN(IX IY)
             MOVE JI7-F16             TO  TBL-SOUBUN(IX IY)
***********2012/07/19 NAV ED
*          *>出荷状況管理ファイル読込
             PERFORM SYUJISL7-READ-SEC
*          *>修正区分
             MOVE SPACE               TO  TBL-SYUKB1(IX IY)
             MOVE SPACE               TO  TBL-SYUKB2(IX IY)
*
             MOVE    "1"              TO  PAGE-SONZAI-CHK(IX)
             MOVE    IX               TO  MEIALLCNT
*
             ADD     1                TO  MEISAI-CNT
         END-PERFORM
     END-PERFORM.
*
     CLOSE  SYUJISL7.
*
 SYUJISL7-SET-EXIT.
     EXIT.
****************************************************************
*    ワークより、画面項目にセット
****************************************************************
 WORK-DSP-SEC              SECTION.
*
     MOVE "WORK-DSP-SEC"     TO   S-NAME.
*明細初期化
     PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > 6
             MOVE  SPACE     TO   DSP-MAS001(IX)
     END-PERFORM.
     MOVE  ZERO              TO   MEISAI-CNT.
*****2012/07/19 ST ADD
     MOVE  SPACE             TO   WK-BUMON-CD.
*****2012/07/19 ED ADD
*
     PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > 6
          IF TBL-MEIKBN(P-CNT IX) NOT = SPACE
             MOVE TBL-MEIKBN(P-CNT IX)  TO  DSP-MEIKBN(IX)
             MOVE TBL-MEISOK(P-CNT IX)  TO  DSP-MEISOK(IX)
             MOVE TBL-MEIBAT(P-CNT IX)  TO  DSP-MEIBAT(IX)
             MOVE TBL-MEITK1(P-CNT IX)  TO  DSP-MEITK1(IX)
             MOVE TBL-MEINOU(P-CNT IX)  TO  DSP-MEINOU(IX)
             MOVE "M"    TO   EDIT-COLOR OF DSP-MEINOU(IX)
             MOVE "M"    TO   EDIT-OPTION OF DSP-MEINOU(IX)
*            *>納品日以前の場合→黄色
             IF   TBL-MEINOU(P-CNT IX) <= DATE-AREA
                  MOVE  "Y"  TO   EDIT-COLOR OF DSP-MEINOU(IX)
             END-IF
*            *>システム日付＋１の場合→赤色
             IF   TBL-MEINOU(P-CNT IX) = WK-HIDUKE-1
                  MOVE  "R"  TO   EDIT-COLOR OF DSP-MEINOU(IX)
                  MOVE  "V"  TO   EDIT-OPTION OF DSP-MEINOU(IX)
             END-IF
*            *>システム日付＋２の場合→ピンク色
             IF   TBL-MEINOU(P-CNT IX) = WK-HIDUKE-2
                  MOVE  "P"  TO   EDIT-COLOR OF DSP-MEINOU(IX)
                  MOVE  "V"  TO   EDIT-OPTION OF DSP-MEINOU(IX)
             END-IF
*            *>システム日付＋３の場合→ピンク色
             IF   TBL-MEINOU(P-CNT IX) = WK-HIDUKE-3
                  MOVE  "P"  TO   EDIT-COLOR OF DSP-MEINOU(IX)
                  MOVE  "V"  TO   EDIT-OPTION OF DSP-MEINOU(IX)
             END-IF
*            *>システム日付＋３以上は元の色に戻す
             IF   TBL-MEINOU(P-CNT IX) > WK-HIDUKE-3
                  MOVE  "M"  TO   EDIT-COLOR OF DSP-MEINOU(IX)
                  MOVE  "M"  TO   EDIT-OPTION OF DSP-MEINOU(IX)
             END-IF
             MOVE TBL-MEIYOB(P-CNT IX)  TO  DSP-MEIYOB(IX)
             MOVE TBL-MEIDTK(P-CNT IX)  TO  DSP-MEIDTK(IX)
             MOVE TBL-SYUTAN(P-CNT IX)  TO  DSP-SYUTAN(IX)
             MOVE TBL-SOUTAN(P-CNT IX)  TO  DSP-SOUTAN(IX)
             MOVE TBL-MEISNM(P-CNT IX)  TO  DSP-MEISNM(IX)
             MOVE TBL-DENST (P-CNT IX)  TO  DSP-DENST (IX)
             MOVE TBL-DENED (P-CNT IX)  TO  DSP-DENED (IX)
             MOVE TBL-MEITK2(P-CNT IX)  TO  DSP-MEITK2(IX)
             MOVE TBL-MEIDEK(P-CNT IX)  TO  DSP-MEIDEK(IX)
             MOVE TBL-SYUTNM(P-CNT IX)  TO  DSP-SYUTNM(IX)
             MOVE TBL-SOUTNM(P-CNT IX)  TO  DSP-SOUTNM(IX)
             IF  LINK-DSOKCD  =  "01"
                 MOVE  " "  TO   EDIT-STATUS OF DSP-SOUTAN(IX)
             ELSE
                 MOVE  "X"  TO   EDIT-STATUS OF DSP-SOUTAN(IX)
             END-IF
             ADD  1                     TO  MEISAI-CNT
          ELSE
             MOVE "M"    TO   EDIT-COLOR OF DSP-MEINOU(IX)
             MOVE "M"    TO   EDIT-OPTION OF DSP-MEINOU(IX)
             MOVE "X" TO EDIT-STATUS OF DSP-SYUTAN(IX)
             MOVE "X" TO EDIT-STATUS OF DSP-SOUTAN(IX)
          END-IF
**********2012/07/19 ST ADD
             MOVE TBL-SYUBUN(P-CNT IX)  TO  WK-SYUBUN(IX)
             MOVE TBL-SYUTAN(P-CNT IX)  TO  WK-SYUTAN(IX)
             MOVE TBL-SOUBUN(P-CNT IX)  TO  WK-SOUBUN(IX)
             MOVE TBL-SOUTAN(P-CNT IX)  TO  WK-SOUTAN(IX)
**********2012/07/19 ED ADD
     END-PERFORM.
*
 WORK-DSP-EXIT.
     EXIT.
****************************************************************
*    画面よりワークにセット
****************************************************************
 DSP-TBL-SEC               SECTION.
*
     MOVE "DSP-TBL-SEC"      TO   S-NAME.
*
     PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > 6
         IF  DSP-MEIKBN(IX) NOT =  SPACE
             IF  DSP-SYUTAN(IX) NOT = TBL-SYUTAN(P-CNT IX)
                 MOVE DSP-SYUTAN(IX)    TO  TBL-SYUTAN(P-CNT IX)
*****************2012/07/19 ST ADD
                 MOVE WK-SYUBUN(IX)     TO  TBL-SYUBUN(P-CNT IX)
*****************2012/07/19 ED ADD
                 IF   DSP-SYUTAN(IX) NOT = SPACE
                      MOVE NC"完"       TO  TBL-SYUTNM(P-CNT IX)
                 ELSE
                      MOVE SPACE        TO  TBL-SYUTNM(P-CNT IX)
                 END-IF
                 MOVE "1"               TO  TBL-SYUKB1(P-CNT IX)
             ELSE
                 MOVE SPACE             TO  TBL-SYUKB1(P-CNT IX)
                 MOVE "1"               TO  TBL-SYUKB1(P-CNT IX)
             END-IF
             IF  DSP-SOUTAN(IX) NOT = TBL-SOUTAN(P-CNT IX)
                 MOVE DSP-SOUTAN(IX)    TO  TBL-SOUTAN(P-CNT IX)
*****************2012/07/19 ST ADD
                 MOVE WK-SOUBUN(IX)     TO  TBL-SOUBUN(P-CNT IX)
*****************2012/07/19 ED ADD
                 IF   DSP-SOUTAN(IX) NOT = SPACE
                      MOVE NC"完"       TO  TBL-SOUTNM(P-CNT IX)
                 ELSE
                      MOVE SPACE        TO  TBL-SOUTNM(P-CNT IX)
                 END-IF
                 MOVE "1"               TO  TBL-SYUKB2(P-CNT IX)
             ELSE
                 MOVE SPACE             TO  TBL-SYUKB2(P-CNT IX)
                 MOVE "1"               TO  TBL-SYUKB2(P-CNT IX)
             END-IF
         END-IF
     END-PERFORM.
*
 WORK-DSP-EXIT.
     EXIT.
****************************************************************
*             明細入力( PSW = 4 )                     2.4      *
****************************************************************
 DSP-BODY-SEC          SECTION.
     MOVE     "DSP-BODY-SEC"      TO   S-NAME.
*
     MOVE       MEIALLCNT         TO   DSP-MEIALL.
     MOVE       P-CNT             TO   DSP-MEICNT.
     PERFORM    DSP-WRITE-SEC.
     PERFORM    DSP-READ-SEC.
*
     EVALUATE   DSP-FNC
*実行
         WHEN   "E000"
                PERFORM   BODY-CHK-SEC
                IF  ERR-FLG  =  ZERO
                    PERFORM     DSP-TBL-SEC
                    PERFORM     WORK-DSP-SEC
                    MOVE  "3"   TO   PSW
                END-IF
*取消
         WHEN   "F004"
                MOVE    "1"      TO   PSW
                MOVE     1       TO   P-CNT
                PERFORM     INIT-DSP-SEC
*終了
         WHEN   "F005"
                MOVE    "END"    TO   END-FLG
                MOVE    "Y  "    TO   END-SW
*項目戻り
         WHEN   "F006"
                MOVE    "1"      TO    PSW
                PERFORM DSP-BODYCL-SEC
*前頁
         WHEN   "F011"
             PERFORM   BODY-CHK-SEC
             IF   ERR-FLG  =  ZERO
                  PERFORM  DSP-TBL-SEC
** 頁チェック
                COMPUTE C-CNT = P-CNT - 1
                IF      C-CNT = ZERO
                        MOVE  12 TO  ERR-FLG
                ELSE
                        COMPUTE P-CNT = P-CNT - 1
                        PERFORM WORK-DSP-SEC
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
                        MOVE   13  TO   ERR-FLG
                ELSE
                  IF    PAGE-SONZAI-CHK(C-CNT) = SPACE
                        MOVE   13    TO    ERR-FLG
                  ELSE
                        COMPUTE P-CNT = P-CNT + 1
                        PERFORM WORK-DSP-SEC
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
*
     MOVE     "BODY-CHK-SEC"     TO   S-NAME.
*
     PERFORM VARYING Y FROM 1 BY 1 UNTIL  Y  >  MEISAI-CNT
*****出荷担当者チェック
     IF  DSP-SYUTAN(Y) = SPACE
         MOVE   SPACE   TO  DSP-SYUTNM(Y)
         MOVE  "M"      TO  EDIT-OPTION  OF  DSP-SYUTAN(Y)
         MOVE   SPACE   TO  EDIT-CURSOR  OF  DSP-SYUTAN(Y)
*********2012/07/19 ST ADD
         MOVE   SPACE   TO  WK-SYUBUN(Y)  WK-SYUTAN(Y)
*********2012/07/19 ED ADD
     ELSE
*********担当者ＣＤをチェックする。
*********DISPLAY "LINK-BUMON = " LINK-BUMON UPON CONS
*********MOVE LINK-BUMON    TO TAN-F01
*********2012/07/19 ST ADD
         IF  WK-SYUTAN(Y)  NOT =  DSP-SYUTAN(Y)
             MOVE LINK-BUMON    TO TAN-F01  WK-SYUBUN(Y)
         ELSE
             MOVE WK-SYUBUN(Y)  TO TAN-F01  WK-SYUBUN(Y)
         END-IF
*********2012/07/19 ED ADD
         MOVE DSP-SYUTAN(Y) TO TAN-F02  WK-SYUTAN(Y)
         PERFORM HTANMS-READ-SEC
         IF  HTANMS-INV-FLG = "INV"
             IF  ERR-FLG = ZERO
                 MOVE 10    TO ERR-FLG
             END-IF
             MOVE "R"   TO  EDIT-OPTION  OF  DSP-SYUTAN(Y)
             MOVE "C"   TO  EDIT-CURSOR  OF  DSP-SYUTAN(Y)
         ELSE
             MOVE "M"   TO  EDIT-OPTION  OF  DSP-SYUTAN(Y)
             MOVE SPACE TO  EDIT-CURSOR  OF  DSP-SYUTAN(Y)
             MOVE NC"完" TO DSP-SYUTNM(Y)
         END-IF
     END-IF
*****送信担当者チェック
     IF  DSP-SOUTAN(Y) = SPACE
         MOVE   SPACE   TO  DSP-SOUTNM(Y)
         MOVE  "M"      TO  EDIT-OPTION  OF  DSP-SOUTAN(Y)
         MOVE   SPACE   TO  EDIT-CURSOR  OF  DSP-SOUTAN(Y)
*********2012/07/19 ST ADD
         MOVE   SPACE   TO  WK-SOUBUN(Y)  WK-SOUTAN(Y)
*********2012/07/19 ED ADD
     ELSE
*********担当者ＣＤをチェックする。
*********MOVE LINK-BUMON    TO TAN-F01
*********2012/07/19 ST ADD
         IF  WK-SOUTAN(Y)  NOT =  DSP-SOUTAN(Y)
             MOVE LINK-BUMON    TO TAN-F01  WK-SOUBUN(Y)
         ELSE
             MOVE WK-SOUBUN(Y)  TO TAN-F01  WK-SOUBUN(Y)
         END-IF
*********2012/07/19 ED ADD
         MOVE DSP-SOUTAN(Y) TO TAN-F02  WK-SOUTAN(Y)
         PERFORM HTANMS-READ-SEC
         IF  HTANMS-INV-FLG = "INV"
             IF  ERR-FLG = ZERO
                 MOVE 11    TO ERR-FLG
             END-IF
             MOVE "R"   TO  EDIT-OPTION  OF  DSP-SOUTAN(Y)
             MOVE "C"   TO  EDIT-CURSOR  OF  DSP-SOUTAN(Y)
         ELSE
             MOVE "M"   TO  EDIT-OPTION  OF  DSP-SOUTAN(Y)
             MOVE SPACE TO  EDIT-CURSOR  OF  DSP-SOUTAN(Y)
             MOVE NC"完" TO DSP-SOUTNM(Y)
         END-IF
     END-IF
     END-PERFORM.
*
 BODY-CHK-EXIT.
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
                PERFORM  INIT-DSP-SEC
                MOVE     1       TO   P-CNT
                MOVE    "1"      TO   PSW
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
                MOVE    "2"      TO   PSW
****************PERFORM DSP-BODYCL-SEC
*前頁
         WHEN   "F011"
** 頁チェック
                COMPUTE C-CNT = P-CNT - 1
                IF      C-CNT = ZERO
                        MOVE  12 TO  ERR-FLG
                ELSE
                        COMPUTE P-CNT = P-CNT - 1
                        PERFORM WORK-DSP-SEC
                        MOVE    "2"      TO   PSW
                END-IF
*次頁
         WHEN   "F012"
** 頁チェック
                MOVE    P-CNT   TO      S-CNT
                COMPUTE C-CNT = P-CNT + 1
                IF      C-CNT > MAX-PGCNT
                        MOVE   13  TO   ERR-FLG
                ELSE
                  IF    PAGE-SONZAI-CHK(C-CNT) = SPACE
                        MOVE   13    TO    ERR-FLG
                  ELSE
                        COMPUTE P-CNT = P-CNT + 1
                        PERFORM WORK-DSP-SEC
                        MOVE    "2"      TO   PSW
                  END-IF
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
           MOVE    SPACE              TO   DSP-MSG1
     ELSE
           MOVE    ERR-MSG-R(ERR-FLG) TO   DSP-MSG1
           MOVE    ZERO               TO   ERR-FLG
     END-IF.
*ガイドメッセージの設定
     EVALUATE   PSW
         WHEN   "1"
           MOVE    PF-MSG-R(1)        TO   DSP-MSG2
         WHEN   "2"
           MOVE    PF-MSG-R(3)        TO   DSP-MSG2
         WHEN   "3"
***********MOVE    PF-MSG-R(2)        TO   DSP-MSG2
           MOVE    PF-MSG-R(3)        TO   DSP-MSG2
     END-EVALUATE.
*画面の表示
     MOVE    "GPALL "          TO   DSP-GRP.
     MOVE    "FKZ01001"        TO   DSP-FMT.
     WRITE    DSP-FKZ01001.
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
*ヘッダ入力
         WHEN   "1"
                MOVE    "GPHEAD"  TO   DSP-GRP
*明細入力
         WHEN   "2"
                MOVE    "GPBODY"  TO   DSP-GRP
*確認
         WHEN   "3"
                MOVE    "GPKAKU"  TO   DSP-GRP
     END-EVALUATE.

     MOVE    "FKZ01001"             TO   DSP-FMT.
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
***  倉庫ＣＤ
     MOVE "M"   TO EDIT-OPTION OF DSP-SOKCD.
     MOVE SPACE TO EDIT-CURSOR OF DSP-SOKCD.
***  ＤＴ区分
     MOVE "M"   TO EDIT-OPTION OF DSP-DTKBN.
     MOVE SPACE TO EDIT-CURSOR OF DSP-DTKBN.
***  納品日
     MOVE "M"   TO EDIT-OPTION OF DSP-NOUDT.
     MOVE SPACE TO EDIT-CURSOR OF DSP-NOUDT.
***  取引先ＣＤ
     MOVE "M"   TO EDIT-OPTION OF DSP-TORICD.
     MOVE SPACE TO EDIT-CURSOR OF DSP-TORICD.
***  出荷完了区分
     MOVE "M"   TO EDIT-OPTION OF DSP-SYUKBN.
     MOVE SPACE TO EDIT-CURSOR OF DSP-SYUKBN.
***  送信完了区分
     MOVE "M"   TO EDIT-OPTION OF DSP-SOUKBN.
     MOVE SPACE TO EDIT-CURSOR OF DSP-SOUKBN.
***  出力順
     MOVE "M"   TO EDIT-OPTION OF DSP-SYUJUN.
     MOVE SPACE TO EDIT-CURSOR OF DSP-SYUJUN.
*
 DSP-SYOKI-EXIT.
     EXIT.
****************************************************************
*         明細画面制御項目初期化                               *
****************************************************************
 DSP-BODYCL-SEC         SECTION.
     MOVE     "DSP-BODYCL-SEC"    TO   S-NAME.
     MOVE     ZERO                TO   DSP-MEICNT  MEIALLCNT.
     MOVE     ZERO                TO   DSP-MEIALL.
     PERFORM VARYING Y FROM 1 BY 1 UNTIL  Y  >  MAX-LNCNT
         MOVE    SPACE    TO  DSP-MAS001(Y)
*リバース，カーソルパーク解除
***  納品日の色初期化
         MOVE  "M"      TO  EDIT-COLOR   OF  DSP-MEINOU(Y)
         MOVE  "M"      TO  EDIT-OPTION  OF  DSP-MEINOU(Y)
***  出荷完了区分
         MOVE  "M"      TO  EDIT-OPTION  OF  DSP-SYUTAN(Y)
         MOVE   SPACE   TO  EDIT-CURSOR  OF  DSP-SYUTAN(Y)
***  送信完了区分
         MOVE  "M"      TO  EDIT-OPTION  OF  DSP-SOUTAN(Y)
         MOVE  SPACE    TO  EDIT-CURSOR  OF  DSP-SOUTAN(Y)
     END-PERFORM.
*
 DSP-BODYCL-EXIT.
     EXIT.
****************************************************************
*             メンテデータ書き込み                   2.5.1     *
****************************************************************
 MENT-WRITE-SEC      SECTION.
*
     MOVE     "MENT-WRITE-SEC"  TO   S-NAME.
*
     MOVE   SPACE      TO   RD-FLG.
*
     OPEN   I-O  SYUJISL6.
*ワークテーブル更新
     PERFORM VARYING X FROM 1 BY 1 UNTIL  X  >  MAX-PGCNT
        PERFORM VARYING Y FROM 1 BY 1 UNTIL Y > MAX-LNCNT
                IF  TBL-SYUKB1(X Y) = "1"
                OR  TBL-SYUKB2(X Y) = "1"
                    MOVE  SPACE            TO  JI6-F95
                    MOVE  TBL-MEISOK(X Y)  TO  JI6-F05
                    MOVE  TBL-MEIBAT1(X Y) TO  JI6-F02
                    MOVE  TBL-MEIBAT2(X Y) TO  JI6-F03
                    MOVE  TBL-MEIBAT3(X Y) TO  JI6-F04
                    MOVE  TBL-MEINOU(X Y)  TO  JI6-F06
                    PERFORM SYUJISF-READ-SEC
                    IF  SYUJISF-INV-FLG  =  "INV"
                        DISPLAY NC"＃＃更新エラー＃＃" UPON CONS
                    ELSE
                        IF  TBL-SYUTAN(X Y) = SPACE
                            MOVE  SPACE    TO  JI6-F11
                            MOVE  ZERO     TO  JI6-F12
****************************2012/07/19 ST ADD
                            MOVE  SPACE    TO  JI6-F15
****************************2012/07/19 ED ADD
                        ELSE
                            MOVE  TBL-SYUTAN(X Y) TO JI6-F11
                            MOVE  DATE-AREA TO JI6-F12
****************************2012/07/19 ST ADD
                            MOVE  TBL-SYUBUN(X Y) TO JI6-F15
****************************2012/07/19 ED ADD
                        END-IF
                        IF  TBL-SOUTAN(X Y) = SPACE
                            MOVE  SPACE    TO  JI6-F13
                            MOVE  ZERO     TO  JI6-F14
****************************2012/07/19 ST ADD
                            MOVE  SPACE    TO  JI6-F16
****************************2012/07/19 ED ADD
                        ELSE
                            MOVE  TBL-SOUTAN(X Y) TO JI6-F13
                            MOVE  DATE-AREA TO JI6-F14
****************************2012/07/19 ST ADD
                            MOVE  TBL-SOUBUN(X Y) TO JI6-F16
****************************2012/07/19 ED ADD
                        END-IF
                        REWRITE JI6-REC
                    END-IF
                END-IF
        END-PERFORM
     END-PERFORM.
*
     CLOSE SYUJISL6.
*
 MENT-WRITE-EXIT.
     EXIT.
****************************************************************
*    倉庫マスタ読込
****************************************************************
 ZSOKMS-READ-SEC      SECTION.
*
     MOVE     "ZSOKMS-READ-SEC"   TO   S-NAME.

     READ   ZSOKMS
            INVALID     MOVE  "INV"   TO   ZSOKMS-INV-FLG
            NOT INVALID MOVE  SPACE   TO   ZSOKMS-INV-FLG
     END-READ.
*
 ZSOKMS-READ-EXIT.
     EXIT.
****************************************************************
*             取引先マスタ          ＲＥＡＤ
****************************************************************
 HTOKMS-READ-SEC       SECTION.
*
     MOVE     "HTOKMS-READ-SEC"   TO   S-NAME.
*
     READ   HTOKMS
            INVALID     MOVE  "INV"   TO   HTOKMS-INV-FLG
            NOT INVALID MOVE  SPACE   TO   HTOKMS-INV-FLG
     END-READ.
*
 HTOKMS-READ-EXIT.
     EXIT.
****************************************************************
*             担当者マスタ          ＲＥＡＤ
****************************************************************
 HTANMS-READ-SEC       SECTION.
*
     MOVE     "HTANMS-READ-SEC"   TO   S-NAME.
*
     READ   HTANMS
            INVALID     MOVE  "INV"   TO   HTANMS-INV-FLG
            NOT INVALID MOVE  SPACE   TO   HTANMS-INV-FLG
     END-READ.
*
 HTANMS-READ-EXIT.
     EXIT.
****************************************************************
*             日付曜日ファイル      ＲＥＡＤ
****************************************************************
 HIDUKEF-READ-SEC      SECTION.
*
     MOVE     "HIDUKEF-READ-SEC"  TO   S-NAME.
*
     READ   HIDUKEF
            INVALID     MOVE  "INV"   TO   HIDUKEF-INV-FLG
            NOT INVALID MOVE  SPACE   TO   HIDUKEF-INV-FLG
     END-READ.
*
 HIDUKEF-READ-EXIT.
     EXIT.
****************************************************************
*    出荷状況管理ファイル（ＫＥＹ＝６）読込
****************************************************************
 SYUJISL6-READ-SEC     SECTION.
*
     MOVE     "SYUJISL6-READ-SEC" TO   S-NAME.
*
 SYUJISL6-010.
     READ   SYUJISL6
            NEXT  AT  END    MOVE  "END"   TO   SYUJISF-END-FLG
                             GO            TO   SYUJISL6-READ-EXIT
     END-READ.
 SYUJISL6-020.
*削除区分が削除済の時は終了へ
     IF     JI6-F95 = "1"
            MOVE  "END"   TO   SYUJISF-END-FLG
            GO            TO   SYUJISL6-READ-EXIT
     END-IF.
 SYUJISL6-030.
*倉庫ＣＤチェック
     IF     DSP-SOKCD  NOT =  SPACE
            IF  DSP-SOKCD NOT = JI6-F05
                GO        TO   SYUJISL6-READ-SEC
            END-IF
     END-IF.
 SYUJISL6-040.
*データ区分チェック
     IF     DSP-DTKBN  NOT =  SPACE
            IF  DSP-DTKBN NOT = JI6-F01
                GO        TO   SYUJISL6-READ-SEC
            END-IF
     END-IF.
 SYUJISL6-050.
*納品日
     IF     DSP-NOUDT  NOT =  ZERO
            IF  DSP-NOUDT NOT = JI6-F06
                GO        TO   SYUJISL6-READ-SEC
            END-IF
     END-IF.
 SYUJISL6-055.
*取引先
     IF     DSP-TORICD  NOT =  ZERO
            IF  DSP-TORICD NOT = JI6-F04
                GO        TO   SYUJISL6-READ-SEC
            END-IF
     END-IF.
 SYUJISL6-060.
*出荷完了区分チェック
     IF     DSP-SYUKBN  NOT =  SPACE
            IF  DSP-SYUKBN  =  "1"
                IF  JI6-F11 NOT = SPACE
                    GO    TO   SYUJISL6-READ-SEC
                END-IF
            ELSE
                IF  JI6-F11  = SPACE
                    GO    TO   SYUJISL6-READ-SEC
                END-IF
            END-IF
     END-IF.
 SYUJISL6-070.
*送信完了区分チェック
     IF     DSP-SOUKBN  NOT =  SPACE
            IF  DSP-SOUKBN  =  "1"
                IF  JI6-F13 NOT = SPACE
                    GO    TO   SYUJISL6-READ-SEC
                END-IF
            ELSE
                IF  JI6-F13  = SPACE
                    GO    TO   SYUJISL6-READ-SEC
                END-IF
            END-IF
     END-IF.
*
 SYUJISL6-READ-EXIT.
     EXIT.
****************************************************************
*    出荷状況管理ファイル（ＫＥＹ＝７）読込
****************************************************************
 SYUJISL7-READ-SEC     SECTION.
*
     MOVE     "SYUJISL7-READ-SEC" TO   S-NAME.
*
     READ   SYUJISL7
            NEXT  AT  END    MOVE  "END"   TO   SYUJISF-END-FLG
                             GO            TO   SYUJISL7-READ-EXIT
     END-READ.
*削除区分が削除済の時は終了へ
     IF     JI7-F95 = "1"
            MOVE  "END"   TO   SYUJISF-END-FLG
            GO            TO   SYUJISL7-READ-EXIT
     END-IF.
*倉庫ＣＤチェック
     IF     DSP-SOKCD  NOT =  SPACE
            IF  DSP-SOKCD NOT = JI7-F05
                GO        TO   SYUJISL7-READ-SEC
            END-IF
     END-IF.
*データ区分チェック
     IF     DSP-DTKBN  NOT =  SPACE
            IF  DSP-DTKBN NOT = JI7-F01
                GO        TO   SYUJISL7-READ-SEC
            END-IF
     END-IF.
*納品日
     IF     DSP-NOUDT  NOT =  ZERO
            IF  DSP-NOUDT NOT = JI7-F06
                GO        TO   SYUJISL7-READ-SEC
            END-IF
     END-IF.
*取引先
     IF     DSP-TORICD  NOT =  ZERO
            IF  DSP-TORICD NOT = JI7-F04
                GO        TO   SYUJISL7-READ-SEC
            END-IF
     END-IF.
*出荷完了区分チェック
     IF     DSP-SYUKBN  NOT =  SPACE
            IF  DSP-SYUKBN  =  "1"
                IF  JI7-F11 NOT = SPACE
                    GO    TO   SYUJISL7-READ-SEC
                END-IF
            ELSE
                IF  JI7-F11  = SPACE
                    GO    TO   SYUJISL7-READ-SEC
                END-IF
            END-IF
     END-IF.
*送信完了区分チェック
     IF     DSP-SOUKBN  NOT =  SPACE
            IF  DSP-SOUKBN  =  "1"
                IF  JI7-F13 NOT = SPACE
                    GO    TO   SYUJISL7-READ-SEC
                END-IF
            ELSE
                IF  JI7-F13  = SPACE
                    GO    TO   SYUJISL7-READ-SEC
                END-IF
            END-IF
     END-IF.
*
 SYUJISL7-READ-EXIT.
     EXIT.
****************************************************************
*    出荷状況管理ファイル（ＫＥＹ＝６）読込　ＲＡＮＤＯＭ
****************************************************************
 SYUJISF-READ-SEC      SECTION.
*
     MOVE     "SYUJISF-READ-SEC"  TO   S-NAME.
*
     READ   SYUJISL6
            INVALID     MOVE  "INV"   TO   SYUJISF-INV-FLG
            NOT INVALID MOVE  SPACE   TO   SYUJISF-INV-FLG
     END-READ.
*
 SYUJISF-READ-EXIT.
     EXIT.
*******************< PROGRAM-END SKZ0100I >*********************

```
