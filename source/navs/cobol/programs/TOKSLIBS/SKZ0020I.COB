****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　出荷業務　　　　　　　　　　　　　*
*    業務名　　　　　　　：　業務改善                          *
*    モジュール名　　　　：　手書出荷指示入力　　　　　　      *
*    作成日／更新日　　　：　2011/04/26                        *
*    作成者／更新者　　　：　ＮＡＶ高橋　　　　　　　　　　　　*
*    処理概要　　　　　　：　手書分の出荷指示入力を行なう。　  *
*                            　　　　　　　　　　　　          *
*                            　　　　　　　　　　　　　　　　　*
*　　更新日／更新者　　　：                                    *
*　                                                            *
****************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SKZ0020I.
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
*----<< 出荷状況管理ファイル >>-*
     SELECT   SYUJISL1  ASSIGN    TO        DA-01-VI-SYUJISL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       JIS-F01   JIS-F02
                                            JIS-F03   JIS-F04
                                            JIS-F05   JIS-F06
                        FILE      STATUS    JIS-ST.
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
*----<< 出荷状況管理ファイル >>-*
 FD  SYUJISL1           LABEL  RECORD  IS  STANDARD.
     COPY     SYUJISF   OF     XFDLIB
              JOINING   JIS AS PREFIX.
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
*----<< 画面定義ファイル  >>-*                                 *
 FD  DSPFILE            LABEL  RECORD  IS  OMITTED.
     COPY     FKZ00201  OF     XMDLIB
              JOINING   DSP AS PREFIX.
*
****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
*ステータス領域
 01  STATUS-AREA.
     03  JIS-ST                   PIC  X(02).
     03  TAN-ST                   PIC  X(02).
     03  TOK-ST                   PIC  X(02).
     03  SOK-ST                   PIC  X(02).
     03  DSP-ST                   PIC  X(02).
*画面制御用領域
 01  DSP-CONTROL.
     03  DSP-FMT                  PIC  X(08).
     03  DSP-GRP                  PIC  X(08).
     03  DSP-PRO                  PIC  X(02).
     03  DSP-FNC                  PIC  X(04).
 01  SET-PGID                     PIC  X(08)  VALUE "SKZ0020I".
 01  SET-FORMID                   PIC  X(08)  VALUE "FKZ00201".
*フラグ領域
 01  FLG-AREA.
     03  HTOKMS-INV-FLG           PIC  X(03)  VALUE  SPACE.
     03  ZSOKMS-INV-FLG           PIC  X(03)  VALUE  SPACE.
     03  HTANMS-INV-FLG           PIC  X(03)  VALUE  SPACE.
     03  SYUJISF-INV-FLG          PIC  X(03)  VALUE  SPACE.
     03  SYUJISF-END-FLG          PIC  X(03)  VALUE  SPACE.
     03  ERR-FLG                  PIC  9(02)  VALUE  ZERO.
     03  END-FLG                  PIC  X(03)  VALUE  SPACE.
     03  X                        PIC  9(02)  VALUE  ZERO.
     03  Y                        PIC  9(02)  VALUE  ZERO.
     03  Z                        PIC  9(02)  VALUE  ZERO.
*ワーク領域
 01  WRK-AREA.
***  プログラムスイッチ（画面遷移制御）
     03  PSW                      PIC  X(01)  VALUE  SPACE.
     03  WRT-CNT                  PIC  9(07)  VALUE  ZERO.
     03  SONZAI-CHK               PIC  9(01)  VALUE  ZERO.
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
         05  FILLER               PIC   N(20)
      VALUE NC"_取消_終了　　　　　　　　　　　".
     03  PF-MSG2.
         05  FILLER               PIC   N(20)
      VALUE NC"_取消_終了_項目戻り　　　　　　".
     03  PF-MSG3.
         05  FILLER               PIC   N(20)
      VALUE NC"　".
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
             VALUE NC"担当者マスタ未登録です。".
     03  ERR-MSG4.
         05  FILLER              PIC   N(20)
             VALUE NC"倉庫マスタ未登録です。".
     03  ERR-MSG5.
         05  FILLER              PIC   N(20)
             VALUE NC"日付論理エラーです。".
     03  ERR-MSG6.
         05  FILLER              PIC   N(20)
             VALUE NC"取引先マスタ未登録です。".
     03  ERR-MSG7.
         05  FILLER              PIC   N(20)
             VALUE NC"伝票番号範囲指定エラー".
     03  ERR-MSG8.
         05  FILLER              PIC   N(20)
             VALUE NC"明細に同一納品日が指定されました。".
     03  ERR-MSG9.
         05  FILLER              PIC   N(20)
             VALUE NC"対象明細が存在しません。".
     03  ERR-MSG10.
         05  FILLER              PIC   N(20)
             VALUE NC"伝票番号範囲を入力して下さい。".
     03  ERR-MSG11.
         05  FILLER              PIC   N(20)
             VALUE NC"開始が終了を超えています。".
     03  ERR-MSG12.
         05  FILLER              PIC   N(20)
             VALUE NC"伝票枚数を入力して下さい。".
     03  ERR-MSG13.
         05  FILLER              PIC   N(20)
             VALUE NC"出荷送信対象の倉庫ではありません。".
     03  ERR-MSG14.
         05  FILLER              PIC   N(20)
             VALUE NC"出荷送信対象の取引先ではありません。".
     03  ERR-MSG15.
         05  FILLER              PIC   N(20)
             VALUE NC"　".
     03  ERR-MSG16.
         05  FILLER              PIC   N(20)
             VALUE NC"　".
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
*ファイルエラーメッセージ
 01  FILE-ERR.
     03  JIS-ERR           PIC N(15) VALUE
                        NC"出荷状況管理ファイル１エラー".
     03  TOK-ERR           PIC N(15) VALUE
                        NC"取引先マスタエラー".
     03  SOK-ERR           PIC N(15) VALUE
                        NC"倉庫マスタエラー".
     03  TAN-ERR           PIC N(15) VALUE
                        NC"担当者マスタエラー".
     03  DSP-ERR           PIC N(15) VALUE
                        NC"画面ファイルエラー".
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
 01  LINK-BTDATE           PIC 9(08).
 01  LINK-BTTIME           PIC 9(04).
 01  LINK-BTTORI           PIC 9(08).
 01  LINK-SOKCD            PIC X(02).
**************************************************************
 PROCEDURE             DIVISION
                          USING  LINK-BUMON
                                 LINK-TANCD
                                 LINK-BTDATE
                                 LINK-BTTIME
                                 LINK-BTTORI
                                 LINK-SOKCD.
**************************************************************
 DECLARATIVES.
 JIS-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE SYUJISL1.
     MOVE        JIS-ST    TO        E-ST.
     MOVE       "SYUJISL1" TO        E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     JIS-ERR   UPON      CONS.
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
     OPEN      INPUT     HTOKMS  ZSOKMS  HTANMS.
     OPEN      I-O       DSPFILE.
*ワークの初期化
     INITIALIZE                 FLG-AREA.
*初期画面の表示
     MOVE     SPACE               TO   DSP-PRO.
     PERFORM   INIT-DSP-SEC.
     MOVE    "1"                  TO   PSW.
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
     CLOSE   HTOKMS  ZSOKMS  HTANMS.
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
     MOVE    SPACE                TO   DSP-FKZ00201.
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
     MOVE    HEN-DATE             TO   DSP-SYSYMD.
     MOVE    HEN-TIME(1:5)        TO   DSP-SYSTIM.
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
                PERFORM   INIT-DSP-SEC
*終了
         WHEN   "F005"
                MOVE    "END"    TO   END-FLG
                MOVE    4010     TO   PROGRAM-STATUS
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
*
     MOVE      ZERO               TO   ERR-FLG.
*****担当者チェック
     IF  DSP-TANCD = SPACE
         MOVE   SPACE   TO  DSP-TANNM
         IF  ERR-FLG = ZERO
             MOVE 2     TO ERR-FLG
         END-IF
         MOVE "R"   TO  EDIT-OPTION  OF  DSP-TANCD
         MOVE "C"   TO  EDIT-CURSOR  OF  DSP-TANCD
     ELSE
*********担当者ＣＤをチェックする。
         MOVE LINK-BUMON    TO TAN-F01
         MOVE DSP-TANCD     TO TAN-F02
         PERFORM HTANMS-READ-SEC
         IF  HTANMS-INV-FLG = "INV"
             IF  ERR-FLG = ZERO
                 MOVE 3     TO ERR-FLG
             END-IF
             MOVE "R"   TO  EDIT-OPTION  OF  DSP-TANCD
             MOVE "C"   TO  EDIT-CURSOR  OF  DSP-TANCD
         ELSE
             MOVE TAN-F03 TO DSP-TANNM
             MOVE "M"   TO  EDIT-OPTION  OF  DSP-TANCD
             MOVE SPACE TO  EDIT-CURSOR  OF  DSP-TANCD
         END-IF
     END-IF
*倉庫ＣＤチェック
     IF  DSP-SOKO  =  SPACE
         MOVE SPACE     TO  DSP-SOKO
         MOVE SPACE     TO  DSP-SOKONM
         IF  ERR-FLG = ZERO
             MOVE 2     TO ERR-FLG
         END-IF
         MOVE "R"   TO  EDIT-OPTION  OF  DSP-SOKO
         MOVE "C"   TO  EDIT-CURSOR  OF  DSP-SOKO
     ELSE
         MOVE DSP-SOKO  TO  SOK-F01
         PERFORM ZSOKMS-READ-SEC
         IF  ZSOKMS-INV-FLG = "INV"
             IF  ERR-FLG = ZERO
                 MOVE   4       TO  ERR-FLG
             END-IF
             MOVE  "R"      TO  EDIT-OPTION  OF  DSP-SOKO
             MOVE  "C"      TO  EDIT-CURSOR  OF  DSP-SOKO
         ELSE
             MOVE  SOK-F02  TO  DSP-SOKONM
             IF    SOK-FIL1(18:1) NOT = "1"
                   IF  ERR-FLG = ZERO
                       MOVE   13      TO  ERR-FLG
                   END-IF
                   MOVE  "R"      TO  EDIT-OPTION  OF  DSP-SOKO
                   MOVE  "C"      TO  EDIT-CURSOR  OF  DSP-SOKO
             ELSE
                   MOVE  "M"      TO  EDIT-OPTION  OF  DSP-SOKO
                   MOVE   SPACE   TO  EDIT-CURSOR  OF  DSP-SOKO
             END-IF
         END-IF
     END-IF.
*取引先チェック
     IF  DSP-TORICD  NOT NUMERIC
     OR  DSP-TORICD  =   ZERO
         MOVE     ZERO  TO   DSP-TORICD
         IF  ERR-FLG = ZERO
             MOVE 2     TO ERR-FLG
         END-IF
         MOVE "R"   TO  EDIT-OPTION  OF  DSP-TORICD
         MOVE "C"   TO  EDIT-CURSOR  OF  DSP-TORICD
     END-IF.
*
     IF  DSP-TORICD  NOT =  ZERO
         MOVE DSP-TORICD  TO  TOK-F01
         PERFORM HTOKMS-READ-SEC
         IF  HTOKMS-INV-FLG = "INV"
             IF  ERR-FLG = ZERO
                 MOVE   6       TO  ERR-FLG
             END-IF
             MOVE  "R"      TO  EDIT-OPTION  OF  DSP-TORICD
             MOVE  "C"      TO  EDIT-CURSOR  OF  DSP-TORICD
         ELSE
             MOVE  TOK-F02  TO  DSP-TORINM
             IF    TOK-FIL1(12:1) NOT = "1"
                   IF  ERR-FLG = ZERO
                       MOVE   14      TO  ERR-FLG
                   END-IF
                   MOVE  "R"      TO  EDIT-OPTION  OF  DSP-TORICD
                   MOVE  "C"      TO  EDIT-CURSOR  OF  DSP-TORICD
             ELSE
                   MOVE  "M"      TO  EDIT-OPTION  OF  DSP-TORICD
                   MOVE   SPACE   TO  EDIT-CURSOR  OF  DSP-TORICD
             END-IF
         END-IF
     END-IF.
*明細を初期化する。
     IF   ERR-FLG  =  ZERO
          MOVE     ZERO     TO  DSP-MAS002
*システム日付・時刻の取得
          ACCEPT   WK-DATE           FROM   DATE
          MOVE     "3"                 TO   LINK-IN-KBN
          MOVE     WK-DATE             TO   LINK-IN-YMD6
          MOVE     ZERO                TO   LINK-IN-YMD8
          MOVE     ZERO                TO   LINK-OUT-RET
          MOVE     ZERO                TO   LINK-OUT-YMD
          CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                            LINK-IN-YMD6
                                            LINK-IN-YMD8
                                            LINK-OUT-RET
                                            LINK-OUT-YMD
          MOVE      LINK-OUT-YMD       TO   DATE-AREA
          MOVE      LINK-OUT-YMD       TO   DSP-BTDATE
*システム時刻取得
          ACCEPT    WK-TIME    FROM   TIME
          MOVE      WK-TIME(1:4)       TO   DSP-BTTIME
*システム取引先ＣＤ
          MOVE      DSP-TORICD         TO   DSP-BTTORI
     END-IF.
*
 HEAD-CHK-EXIT.
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
                IF  ERR-FLG  =  ZERO
                    MOVE  "3"   TO   PSW
                END-IF
*取消
         WHEN   "F004"
                MOVE    "1"      TO   PSW
                PERFORM     INIT-DSP-SEC
*終了
         WHEN   "F005"
                MOVE    "END"    TO   END-FLG
                MOVE    4010     TO   PROGRAM-STATUS
*項目戻り
         WHEN   "F006"
                MOVE    "1"      TO    PSW
                PERFORM DSP-BODYCL-SEC
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
     MOVE    ZERO       TO    SONZAI-CHK.
*
     PERFORM VARYING Y FROM 1 BY 1 UNTIL  Y  >  5
*****明細入力チェック
     IF  DSP-NOUDT(Y) = ZERO
     AND DSP-SDENNO(Y) = ZERO
     AND DSP-EDENNO(Y) = ZERO
     AND DSP-DATCNT(Y) = ZERO
     AND DSP-DENCNT(Y) = ZERO
         MOVE  "M"      TO  EDIT-OPTION  OF  DSP-NOUDT(Y)
         MOVE   SPACE   TO  EDIT-CURSOR  OF  DSP-NOUDT(Y)
         MOVE  "M"      TO  EDIT-OPTION  OF  DSP-SDENNO(Y)
         MOVE   SPACE   TO  EDIT-CURSOR  OF  DSP-SDENNO(Y)
         MOVE  "M"      TO  EDIT-OPTION  OF  DSP-EDENNO(Y)
         MOVE   SPACE   TO  EDIT-CURSOR  OF  DSP-EDENNO(Y)
         MOVE  "M"      TO  EDIT-OPTION  OF  DSP-DATCNT(Y)
         MOVE   SPACE   TO  EDIT-CURSOR  OF  DSP-DATCNT(Y)
         MOVE  "M"      TO  EDIT-OPTION  OF  DSP-DENCNT(Y)
         MOVE   SPACE   TO  EDIT-CURSOR  OF  DSP-DENCNT(Y)
     ELSE
         MOVE      1             TO   SONZAI-CHK
*********納品日チェック
         MOVE     "2"            TO   LINK-IN-KBN
         MOVE     ZERO           TO   LINK-IN-YMD6
         MOVE     DSP-NOUDT(Y)   TO   LINK-IN-YMD8
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
              MOVE  "R"      TO   EDIT-OPTION OF DSP-NOUDT(Y)
              MOVE  "C"      TO   EDIT-CURSOR OF DSP-NOUDT(Y)
         ELSE
              MOVE  "M"      TO   EDIT-OPTION OF DSP-NOUDT(Y)
              MOVE  SPACE    TO   EDIT-CURSOR OF DSP-NOUDT(Y)
         END-IF
*****伝票番号開始チェック
         IF  DSP-EDENNO(Y) = ZERO
         AND DSP-SDENNO(Y) NOT = ZERO
             MOVE DSP-SDENNO(Y)  TO  DSP-EDENNO(Y)
         END-IF
*********大小チェック
         IF  DSP-SDENNO(Y) > DSP-EDENNO(Y)
             IF  ERR-FLG = ZERO
                 MOVE 11    TO ERR-FLG
             END-IF
             MOVE "R"   TO  EDIT-OPTION  OF  DSP-SDENNO(Y)
             MOVE "R"   TO  EDIT-OPTION  OF  DSP-EDENNO(Y)
             MOVE "C"   TO  EDIT-CURSOR  OF  DSP-SDENNO(Y)
         ELSE
             MOVE "M"   TO  EDIT-OPTION  OF  DSP-SDENNO(Y)
             MOVE "M"   TO  EDIT-OPTION  OF  DSP-EDENNO(Y)
             MOVE SPACE TO  EDIT-CURSOR  OF  DSP-SDENNO(Y)
         END-IF
*********伝票枚数チェック
*********IF  DSP-DENCNT(Y) = ZERO
*            IF  ERR-FLG = ZERO
*                MOVE 12    TO ERR-FLG
*            END-IF
*            MOVE "R"   TO  EDIT-OPTION  OF  DSP-DENCNT(Y)
*            MOVE "C"   TO  EDIT-CURSOR  OF  DSP-DENCNT(Y)
*        ELSE
*            MOVE "M"   TO  EDIT-OPTION  OF  DSP-DENCNT(Y)
*            MOVE SPACE TO  EDIT-CURSOR  OF  DSP-DENCNT(Y)
*********END-IF
     END-IF
     END-PERFORM.
*
     IF  ERR-FLG NOT =  ZERO
         GO             TO  BODY-CHK-EXIT
     ELSE
         IF  SONZAI-CHK = ZERO
             IF  ERR-FLG = ZERO
                 MOVE 9     TO ERR-FLG
             END-IF
             MOVE "C"   TO  EDIT-CURSOR  OF  DSP-NOUDT(1)
         ELSE
             MOVE SPACE TO  EDIT-CURSOR  OF  DSP-NOUDT(1)
         END-IF
     END-IF.
*納品日ダブリチェック
     PERFORM VARYING Y FROM 1 BY 1 UNTIL  Y  >  5
     IF  DSP-NOUDT(Y) NOT = ZERO
         COMPUTE X = Y + 1
         PERFORM VARYING Z FROM X BY 1 UNTIL Z > 5
         IF  DSP-NOUDT(Z)  NOT =  ZERO
             IF  DSP-NOUDT(Y) =  DSP-NOUDT(Z)
                 MOVE  8    TO  ERR-FLG
                 MOVE "R"   TO  EDIT-OPTION  OF  DSP-NOUDT(Z)
                 MOVE "C"   TO  EDIT-CURSOR  OF  DSP-NOUDT(Z)
             ELSE
                 MOVE "M"   TO  EDIT-OPTION  OF  DSP-NOUDT(Z)
                 MOVE SPACE TO  EDIT-CURSOR  OF  DSP-NOUDT(Z)
             END-IF
         END-IF
         END-PERFORM
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
                PERFORM  INIT-DSP-SEC
****************MOVE    "1"      TO   PSW
                MOVE    "END"    TO   END-FLG
*取消
         WHEN   "F004"
                PERFORM  INIT-DSP-SEC
                MOVE    "1"      TO   PSW
*終了
         WHEN   "F005"
                MOVE    "END"    TO   END-FLG
                MOVE    4010     TO   PROGRAM-STATUS
*項目戻り
         WHEN   "F006"
                MOVE    "2"      TO   PSW
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
           MOVE    SPACE              TO   DSP-MSG
     ELSE
           MOVE    ERR-MSG-R(ERR-FLG) TO   DSP-MSG
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
     END-EVALUATE.
*画面の表示
     MOVE    "SCREEN"          TO   DSP-GRP.
     MOVE    "FKZ00201"        TO   DSP-FMT.
     WRITE    DSP-FKZ00201.
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
                MOVE    "GRP001"  TO   DSP-GRP
*明細入力
         WHEN   "2"
                MOVE    "GRP002"  TO   DSP-GRP
*確認
         WHEN   "3"
                MOVE    "KKNN  "  TO   DSP-GRP
     END-EVALUATE.

     MOVE    "FKZ00201"             TO   DSP-FMT.
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
***  担当者ＣＤ
     MOVE "M"   TO EDIT-OPTION OF DSP-TANCD.
     MOVE SPACE TO EDIT-CURSOR OF DSP-TANCD.
***  倉庫ＣＤ
     MOVE "M"   TO EDIT-OPTION OF DSP-SOKO.
     MOVE SPACE TO EDIT-CURSOR OF DSP-SOKO.
***  取引先ＣＤ
     MOVE "M"   TO EDIT-OPTION OF DSP-TORICD.
     MOVE SPACE TO EDIT-CURSOR OF DSP-TORICD.
***  バッチ番号初期化
     MOVE SPACE TO DSP-MAS003.
*
 DSP-SYOKI-EXIT.
     EXIT.
****************************************************************
*         明細画面制御項目初期化                               *
****************************************************************
 DSP-BODYCL-SEC         SECTION.
     MOVE     "DSP-BODYCL-SEC"    TO   S-NAME.
*
     PERFORM VARYING Y FROM 1 BY 1 UNTIL  Y  >  3
         MOVE    SPACE    TO  DSP-MAS002
*リバース，カーソルパーク解除
***  納品日
         MOVE  "M"      TO  EDIT-OPTION  OF  DSP-NOUDT(Y)
         MOVE   SPACE   TO  EDIT-CURSOR  OF  DSP-NOUDT(Y)
***  伝票番号開始
         MOVE  "M"      TO  EDIT-OPTION  OF  DSP-SDENNO(Y)
         MOVE  SPACE    TO  EDIT-CURSOR  OF  DSP-SDENNO(Y)
***  伝票番号終了
         MOVE  "M"      TO  EDIT-OPTION  OF  DSP-EDENNO(Y)
         MOVE  SPACE    TO  EDIT-CURSOR  OF  DSP-EDENNO(Y)
***  データ件数
         MOVE  "M"      TO  EDIT-OPTION  OF  DSP-DATCNT(Y)
         MOVE  SPACE    TO  EDIT-CURSOR  OF  DSP-DATCNT(Y)
***  件数枚数
         MOVE  "M"      TO  EDIT-OPTION  OF  DSP-DENCNT(Y)
         MOVE  SPACE    TO  EDIT-CURSOR  OF  DSP-DENCNT(Y)
***  行番号セット
         MOVE  Y        TO  DSP-BANGO(Y)
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
     OPEN   I-O  SYUJISL1.
*ワークテーブル更新
     PERFORM VARYING X FROM 1 BY 1 UNTIL  X  >  5
       DISPLAY "DSP-NOUDT = " DSP-NOUDT(X) UPON CONS
       IF  DSP-NOUDT(X) NOT = ZERO
           MOVE  "2"              TO  JIS-F01
           MOVE  DSP-BTDATE       TO  JIS-F02
           MOVE  DSP-BTTIME       TO  JIS-F03
           MOVE  DSP-BTTORI       TO  JIS-F04
           MOVE  DSP-SOKO         TO  JIS-F05
           MOVE  DSP-NOUDT(X)     TO  JIS-F06
           PERFORM SYUJISF-READ-SEC
           IF  SYUJISF-INV-FLG  =  SPACE
             DISPLAY NC"＃＃＃＃＃＃＃＃＃＃＃＃＃＃" UPON CONS
             DISPLAY NC"＃＃同一キーが存在します＃＃" UPON CONS
             DISPLAY NC"＃＃＃＃＃＃＃＃＃＃＃＃＃＃" UPON CONS
           ELSE
               MOVE   SPACE         TO  JIS-REC
               INITIALIZE               JIS-REC
               MOVE   "2"           TO  JIS-F01
               MOVE   DSP-BTDATE    TO  JIS-F02
               MOVE   DSP-BTTIME    TO  JIS-F03
               MOVE   DSP-BTTORI    TO  JIS-F04
               MOVE   DSP-SOKO      TO  JIS-F05
               MOVE   DSP-NOUDT(X)  TO  JIS-F06
               MOVE   DSP-SDENNO(X) TO  JIS-F07
               MOVE   DSP-EDENNO(X) TO  JIS-F08
               MOVE   DSP-DATCNT(X) TO  JIS-F09
               MOVE   DSP-DENCNT(X) TO  JIS-F10
               MOVE   DSP-TANCD     TO  JIS-F97
               MOVE   DSP-BTDATE    TO  JIS-F98
               MOVE   DSP-BTTIME    TO  JIS-F99
               WRITE  JIS-REC
               ADD    1             TO  WRT-CNT
           END-IF
        END-IF
     END-PERFORM.
*
     MOVE    DSP-BTDATE          TO   LINK-BTDATE.
     MOVE    DSP-BTTIME          TO   LINK-BTTIME.
     MOVE    DSP-BTTORI          TO   LINK-BTTORI.
     MOVE    DSP-SOKO            TO   LINK-SOKCD.
*
     CLOSE SYUJISL1.
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
*    出荷状況管理ファイル（ＫＥＹ＝１）読込　ＲＡＮＤＯＭ
****************************************************************
 SYUJISF-READ-SEC      SECTION.
*
     MOVE     "SYUJISF-READ-SEC"  TO   S-NAME.
*
     READ   SYUJISL1
            INVALID     MOVE  "INV"   TO   SYUJISF-INV-FLG
            NOT INVALID MOVE  SPACE   TO   SYUJISF-INV-FLG
     END-READ.
*
 SYUJISF-READ-EXIT.
     EXIT.
*******************< PROGRAM-END SKZ0020I >*********************
