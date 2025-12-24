****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　受注状況照会　　　　　　　　　　　*
*    業務名　　　　　　　：　受注状況照会                      *
*    モジュール名　　　　：　受注状況照会（取引先単位）　　　　*
*    作成日／作成者　　　：　2018.02.22   /  T.MIURA           *
*    更新日／更新者　　　：　　　　　　                        *
*    処理概要　　　　　　：　取引先からの受注内容を確認する。  *
*                            取引先単位                        *
****************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SJZ0010I.
 AUTHOR.               NAV.
 DATE-WRITTEN.         18/02/22.
****************************************************************
 ENVIRONMENT           DIVISION.
****************************************************************
 CONFIGURATION         SECTION.
 SPECIAL-NAMES.
     CONSOLE      IS   CONS.
*
 INPUT-OUTPUT          SECTION.
 FILE-CONTROL.
*----<< 受注残ファイル（Ｌ３） >>-*
     SELECT   JYUZANF   ASSIGN    TO        DA-01-VI-JYUZANL3
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      DYNAMIC
                        RECORD    KEY       JYU-F13   JYU-F14
                                            JYU-F15   JYU-F16
                                            JYU-F09   JYU-F03
                                            JYU-F17   JYU-F11
                        FILE      STATUS    JYU-ST.
*----<< 倉庫マスタ >>-*
     SELECT   ZSOKMS    ASSIGN    TO        DA-01-VI-ZSOKMS1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       SOK-F01
                        FILE      STATUS    SOK-ST.
*----<< 商品名称マスタ >>-*
     SELECT  HMEIMS   ASSIGN    TO         DA-01-VI-MEIMS1
                       ORGANIZATION         INDEXED
                       ACCESS    MODE       RANDOM
                       RECORD    KEY        MEI-F011 MEI-F0121
                                            MEI-F0122 MEI-F0123
                       FILE      STATUS     MEI-ST.
*----<< 取引先マスタ >>-*
     SELECT   TOKMS    ASSIGN    TO        DA-01-VI-TOKMS2
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       TOK-F01
                        FILE      STATUS    TOK-ST.
*----<< 画面定義ファイル  >>-*
     SELECT  DSPFILE   ASSIGN    TO         GS-DSPF
                       FORMAT               DSP-FMT
                       GROUP                DSP-GRP
                       PROCESSING           DSP-PRO
                       FUNCTION             DSP-FNC
                       FILE      STATUS     DSP-ST.
*
****************************************************************
 DATA                DIVISION.
****************************************************************
 FILE                SECTION.
*----<< 受注残ファイル >>-*
 FD  JYUZANF            LABEL  RECORD  IS  STANDARD.
     COPY     JYUZANF   OF     XFDLIB
              JOINING   JYU AS PREFIX.
*----<< 倉庫マスタ >>-*                                      *
 FD  ZSOKMS             LABEL  RECORD  IS  STANDARD.
     COPY     ZSOKMS    OF     XFDLIB
              JOINING   SOK AS PREFIX.
*----<< 商品名称マスタ >>-*                                    *
 FD  HMEIMS            LABEL  RECORD  IS  STANDARD.
     COPY     HMEIMS   OF     XFDLIB
              JOINING   MEI AS PREFIX.
*----<< 取引先マスタ >>-*                                    *
 FD  TOKMS             LABEL  RECORD  IS  STANDARD.
     COPY     TOKMS2   OF     XFDLIB
              JOINING   TOK AS PREFIX.
*----<< 画面定義ファイル  >>-*                                 *
 FD  DSPFILE            LABEL  RECORD  IS  OMITTED.
     COPY     FJZ00101  OF     XMDLIB
              JOINING   DSP AS PREFIX.
*
****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
*ステータス領域
 01  STATUS-AREA.
     03  JYU-ST                   PIC  X(02).
     03  SOK-ST                   PIC  X(02).
     03  MEI-ST                   PIC  X(02).
     03  TOK-ST                   PIC  X(02).
     03  DSP-ST                   PIC  X(02).
*画面制御用領域
 01  DSP-CONTROL.
     03  DSP-FMT                  PIC  X(08).
     03  DSP-GRP                  PIC  X(08).
     03  DSP-PRO                  PIC  X(02).
     03  DSP-FNC                  PIC  X(04).
 01  SET-PGID                     PIC  X(08)  VALUE "SJZ0010I".
 01  SET-FORMID                   PIC  X(08)  VALUE "FJZ00101".
 01  MAX-PGCNT                    PIC  9(03)  VALUE 50.
 01  MAX-LNCNT                    PIC  9(03)  VALUE 07.
 01  MEISAI-CNT                   PIC  9(09)  VALUE ZERO.
 01  MEIALLCNT                    PIC  9(02)  VALUE ZERO.
*フラグ領域
 01  FLG-AREA.
     03  TOKMS-INV-FLG            PIC  X(03)  VALUE  SPACE.
     03  ZSOKMS-INV-FLG           PIC  X(03)  VALUE  SPACE.
     03  MEIMS-INV-FLG            PIC  X(03)  VALUE  SPACE.
     03  JYUZANF-INV-FLG          PIC  X(03)  VALUE  SPACE.
     03  ERR-FLG                  PIC  9(02)  VALUE  ZERO.
     03  END-FLG                  PIC  X(03)  VALUE  SPACE.
     03  JYUZANF-END-FLG          PIC  X(03)  VALUE  SPACE.
     03  P-CNT                    PIC  9(02)  VALUE  ZERO.
     03  S-CNT                    PIC  9(02)  VALUE  ZERO.
     03  C-CNT                    PIC  9(02)  VALUE  ZERO.
     03  Y-CNT                    PIC  9(01)  VALUE  ZERO.
     03  X                        PIC  9(02)  VALUE  ZERO.
     03  Y                        PIC  9(02)  VALUE  ZERO.
     03  IX                       PIC  9(02)  VALUE  ZERO.
     03  IY                       PIC  9(02)  VALUE  ZERO.
     03  CHK-01                   PIC  9(03)  VALUE  ZERO.
     03  CHK-02                   PIC  9(03)  VALUE  ZERO.
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
*    03  FILLER                   PIC  X(01)  VALUE  "/".
     03  HEN-DATE-MM              PIC  9(02)  VALUE  ZERO.
*    03  FILLER                   PIC  X(01)  VALUE  "/".
     03  HEN-DATE-DD              PIC  9(02)  VALUE  ZERO.
*画面表示時刻編集
 01  HEN-TIME.
     03  HEN-TIME-HH              PIC  9(02)  VALUE  ZERO.
*    03  FILLER                   PIC  X(01)  VALUE  ":".
     03  HEN-TIME-MM              PIC  9(02)  VALUE  ZERO.
*    03  FILLER                   PIC  X(01)  VALUE  ":".
     03  HEN-TIME-SS              PIC  9(02)  VALUE  ZERO.
*
 01  WK-KEYCD.
     03  KEY-SOKCD                PIC  X(02)  VALUE  SPACE.
     03  KEY-TOKCD                PIC  9(08)  VALUE  ZERO.
     03  KEY-TANABAN              PIC  X(06)  VALUE  SPACE.
     03  KEY-NDATE                PIC  9(08)  VALUE  ZERO.
     03  KEY-SHOCD                PIC  X(08)  VALUE  SPACE.
     03  KEY-HINCD1               PIC  X(05)  VALUE  SPACE.
     03  KEY-HINCD2               PIC  X(02)  VALUE  SPACE.
     03  KEY-HINCD3               PIC  X(01)  VALUE  SPACE.
 01  WK-ENDCHK.
     03  WK-MEINO                 PIC  9(01)  VALUE  ZERO.
*ＰＦガイド
 01  PF-MSG-AREA.
     03  PF-ERRMSG.
         05  FILLER               PIC   N(30)
      VALUE NC"_取消_終了　　　　　　　　　　".
     03  PF-PFMSG.
         05  FILLER               PIC   N(30)
      VALUE NC"_取消_終了_項目戻り_前頁_次頁".
 01  PF-MSG-AREA-R       REDEFINES     PF-MSG-AREA.
     03  PF-MSG-R   OCCURS   2   PIC   N(30).
*
*メッセージの取得
 01  ERR-MSG-AREA.
     03  ERR-MSG01.
         05  FILLER              PIC   N(20)
             VALUE NC"無効ＰＦキーです。".
     03  ERR-MSG02.
         05  FILLER              PIC   N(20)
             VALUE NC"倉庫Ｍ未登録です。！！".
     03  ERR-MSG03.
         05  FILLER              PIC   N(20)
             VALUE NC"取引先Ｍ未登録です！！".
     03  ERR-MSG04.
         05  FILLER              PIC   N(20)
             VALUE NC"商品名称Ｍ未登録です！！".
     03  ERR-MSG05.
         05  FILLER              PIC   N(20)
             VALUE NC"　".
     03  ERR-MSG06.
         05  FILLER              PIC   N(20)
             VALUE NC"対象データが存在しません！！".
     03  ERR-MSG07.
         05  FILLER              PIC   N(20)
             VALUE NC"前頁がありません！！".
     03  ERR-MSG08.
         05  FILLER              PIC   N(20)
             VALUE NC"次頁がありません！！".
     03  ERR-MSG09.
         05  FILLER              PIC   N(20)
             VALUE NC"明細番号入力エラー！！".
     03  ERR-MSG10.
         05  FILLER              PIC   N(20)
             VALUE NC"　".
     03  ERR-MSG11.
         05  FILLER              PIC   N(20)
             VALUE NC"　".
     03  ERR-MSG12.
         05  FILLER              PIC   N(20)
             VALUE NC"　".
     03  ERR-MSG13.
         05  FILLER              PIC   N(20)
             VALUE NC"　".
     03  ERR-MSG14.
         05  FILLER              PIC   N(20)
             VALUE NC"　".
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
*メンテデータ退避エリア
 01  TABLE-AREA.
     03  TABLE1      OCCURS  50.
         05  PAGE-SONZAI-CHK      PIC   X(01).
         05  TABLE2  OCCURS  07.
             07  TBL-SOKCDM       PIC   X(02).
             07  TBL-SOKNMM       PIC   N(10).
             07  TBL-TANA1        PIC   X(03).
             07  TBL-TANA2        PIC   X(01).
             07  TBL-TANA3        PIC   X(02).
             07  TBL-JSURYO       PIC  S9(11).
             07  TBL-HSURYO       PIC  S9(11).
             07  TBL-TOKCDM       PIC   9(08).
             07  TBL-TOKNMM       PIC   N(10).
             07  TBL-NDATE        PIC   9(08).
             07  TBL-SSURYO       PIC  S9(11).
             07  TBL-MSURYO       PIC  S9(11).
*ファイルエラーメッセージ
 01  FILE-ERR.
     03  JYU-ERR           PIC N(15) VALUE
                        NC"受注残ファイルエラー".
     03  SOK-ERR           PIC N(15) VALUE
                        NC"倉庫マスタエラー".
     03  MEI-ERR           PIC N(15) VALUE
                        NC"商品名称マスタエラー".
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
*明細照会用パラメタ
 01  PA-MEISAI.
     03  PA-SOKCD                 PIC  X(02).
     03  PA-DSOKCD                PIC  X(02).
     03  PA-SHOCD                 PIC  X(08).
     03  PA-HINCD1                PIC  X(05).
     03  PA-HINCD2                PIC  X(02).
     03  PA-HINCD3                PIC  X(01).
     03  PA-TOKCD                 PIC  9(08).
     03  PA-TANA                  PIC  X(06).
     03  PA-NDATE                 PIC  9(08).
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
**************************************************************
 LINKAGE               SECTION.
 01  LINK-SOKCD            PIC X(02).
 01  LINK-DSOKCD           PIC X(02).
 01  LINK-SHOCD            PIC X(08).
 01  LINK-HINTAN1          PIC X(05).
 01  LINK-HINTAN2          PIC X(02).
 01  LINK-HINTAN3          PIC X(01).
 01  LINK-TANA             PIC X(06).
**************************************************************
 PROCEDURE             DIVISION
                          USING  LINK-SOKCD
                                 LINK-DSOKCD
                                 LINK-SHOCD
                                 LINK-HINTAN1 LINK-HINTAN2
                                 LINK-HINTAN3
                                 LINK-TANA.
**************************************************************F
 DECLARATIVES.
 JYU-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE JYUZANF.
     MOVE        JYU-ST    TO        E-ST.
     MOVE       "JYUZANL3" TO        E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     JYU-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
*    STOP        RUN.
     EXIT PROGRAM.
 SOK-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE ZSOKMS.
     MOVE        SOK-ST    TO        E-ST.
     MOVE       "ZSOKMS1 " TO        E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     SOK-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
*    STOP        RUN.
     EXIT PROGRAM.
 MEI-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HMEIMS.
     MOVE        MEI-ST    TO        E-ST.
     MOVE       "MEIMS1"   TO        E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     MEI-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
*    STOP        RUN.
     EXIT PROGRAM.
 TOK-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE TOKMS.
     MOVE        TOK-ST    TO        E-ST.
     MOVE       "TOKMS2"   TO        E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     TOK-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
*    STOP        RUN.
     EXIT PROGRAM.
 DSP-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE DSPFILE.
     MOVE        DSP-ST    TO        E-ST.
     MOVE       "DSPFILE"  TO        E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     DSP-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
*    STOP        RUN.
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
*
 CONTROL-EXIT.
*    STOP  RUN.
     EXIT PROGRAM.
****************************************************************
*             初期処理                               1.0
****************************************************************
 INIT-SEC              SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
*T↓
*****DISPLAY  "LINK-SOKCD  =" LINK-SOKCD   UPON CONS.
*    DISPLAY  "LINK-DSOKCD =" LINK-DSOKCD  UPON CONS.
*    DISPLAY  "LINK-SHOCD  =" LINK-SHOCD   UPON CONS.
*    DISPLAY  "LINK-HINTAN1=" LINK-HINTAN1 UPON CONS.
*    DISPLAY  "LINK-HINTAN2=" LINK-HINTAN2 UPON CONS.
*    DISPLAY  "LINK-HINTAN3=" LINK-HINTAN3 UPON CONS.
*****DISPLAY  "LINK-TANA   =" LINK-TANA    UPON CONS.
*T↑
*ファイルのＯＰＥＮ
     OPEN      INPUT     ZSOKMS  HMEIMS  TOKMS.
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
*ワークの初期化
     INITIALIZE                 FLG-AREA.
*初期画面の表示
     MOVE     SPACE               TO   DSP-PRO.
     PERFORM   INIT-DSP-SEC.
*商品コードパラメタがあれば、明細を取得する。
     IF   LINK-SHOCD   NOT =  SPACE
          PERFORM  HEAD-CHK-SEC
          IF  ERR-FLG  =  ZERO
              MOVE  "2"   TO  PSW
          ELSE
              MOVE    "1"           TO   PSW
*頁セット
              MOVE     1            TO   P-CNT
          END-IF
     ELSE
         MOVE    "1"                  TO   PSW
*頁セット
         MOVE     1                   TO   P-CNT
     END-IF.
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
*確認入力
         WHEN      "2"       PERFORM   DSP-KAKU-SEC
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
     CLOSE   ZSOKMS  HMEIMS  TOKMS.
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
     MOVE    SPACE                TO   DSP-FJZ00101.
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
     MOVE    HEN-TIME             TO   DSP-STIME.
*明細頁
     MOVE    ZERO                 TO   MEIALLCNT.
 INITD004.
*ＰＧＩＤ・ＦＭＴＩＤセット
     MOVE    SET-PGID             TO   DSP-PGID.
     MOVE    SET-FORMID           TO   DSP-FORMID.
*倉庫コードパラメタより倉庫コード入力チェック
     IF       LINK-DSOKCD  =  "01" OR "88"
              MOVE    " "         TO   EDIT-STATUS OF DSP-SOKCDH
     ELSE
              MOVE  LINK-SOKCD    TO   SOK-F01 DSP-SOKCDH
              PERFORM ZSOKMS-READ-SEC
              IF  ZSOKMS-INV-FLG = "INV"
                  MOVE      SPACE      TO   DSP-SOKNMH
              ELSE
                  MOVE      SOK-F02    TO   DSP-SOKNMH
              END-IF
**************プロテクト追加
*             MOVE    "X"    TO   EDIT-STATUS OF DSP-SOKCDH
     END-IF.
*商品コードパラメタより商品コード入力チェック
     IF       LINK-SHOCD   =  SPACE
              MOVE    " "         TO   EDIT-STATUS OF DSP-SHOCD
              MOVE    " "         TO   EDIT-STATUS OF DSP-HINCD1
              MOVE    " "         TO   EDIT-STATUS OF DSP-HINCD2
              MOVE    " "         TO   EDIT-STATUS OF DSP-HINCD3
     ELSE
              MOVE  LINK-SHOCD    TO   MEI-F011 DSP-SHOCD
              MOVE  LINK-HINTAN1  TO   MEI-F0121 DSP-HINCD1
              MOVE  LINK-HINTAN2  TO   MEI-F0122 DSP-HINCD2
              MOVE  LINK-HINTAN3  TO   MEI-F0123 DSP-HINCD3
              PERFORM MEIMS-READ-SEC
              IF  MEIMS-INV-FLG = "INV"
                  MOVE      SPACE      TO   DSP-SHONM1
                  MOVE      SPACE      TO   DSP-SHONM2
              ELSE
                  MOVE      MEI-F021    TO   DSP-SHONM1
                  MOVE      MEI-F022    TO   DSP-SHONM2
              END-IF
**************プロテクト追加
*             MOVE    "X"    TO   EDIT-STATUS OF DSP-SOKCDH
*             MOVE    "X"    TO   EDIT-STATUS OF DSP-HINCD1
*             MOVE    "X"    TO   EDIT-STATUS OF DSP-HINCD2
*             MOVE    "X"    TO   EDIT-STATUS OF DSP-HINCD3
     END-IF.
*取引先コードより取引先ＣＤ入力チェック
*    MOVE  LINK-TOKCD    TO   TOK-F01 DSP-TOKCDH.
*    PERFORM TOKMS-READ-SEC.
*    IF  TOKMS-INV-FLG = "INV"
*        MOVE      ALL NC"＊" TO   DSP-TOKNMH
*    ELSE
*                 MOVE      TOK-F02    TO   DSP-TOKNMH
*             END-IF
**************プロテクト追加
*             MOVE    "X"    TO   EDIT-STATUS OF DSP-TOKCDH
*    END-IF.
 INITD005.
*項目属性クリア
     PERFORM  DSP-SYOKI-SEC.
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
                   PERFORM DSP-SYOKI-SEC
               END-IF
*取消
         WHEN   "F004"
                MOVE    "1"      TO   PSW
                MOVE     1       TO   P-CNT
                PERFORM   INIT-DSP-SEC
*終了
         WHEN   "F005"
                MOVE    "END"    TO   END-FLG
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
     IF  DSP-SOKCDH  =  SPACE
         MOVE SPACE     TO  DSP-SOKNMH
     ELSE
         MOVE DSP-SOKCDH TO  SOK-F01
         PERFORM ZSOKMS-READ-SEC
         IF  ZSOKMS-INV-FLG = "INV"
             MOVE   2       TO  ERR-FLG
             MOVE   SPACE   TO  DSP-SOKNMH
             MOVE  "R"      TO  EDIT-OPTION  OF  DSP-SOKCDH
             MOVE  "C"      TO  EDIT-CURSOR  OF  DSP-SOKCDH
         ELSE
             MOVE  SOK-F02  TO  DSP-SOKNMH
             MOVE  "M"      TO  EDIT-OPTION  OF  DSP-SOKCDH
             MOVE   SPACE   TO  EDIT-CURSOR  OF  DSP-SOKCDH
         END-IF
**************プロテクト追加
*             MOVE    "X"    TO   EDIT-STATUS OF DSP-SOKCDH
     END-IF.
*商品ＣＤチェック
     IF  ERR-FLG   =  ZERO
         MOVE  DSP-SHOCD    TO   MEI-F011
         MOVE  DSP-HINCD1   TO   MEI-F0121
         MOVE  DSP-HINCD2   TO   MEI-F0122
         MOVE  DSP-HINCD3   TO   MEI-F0123
         PERFORM MEIMS-READ-SEC
         IF  MEIMS-INV-FLG = "INV"
             MOVE   4       TO  ERR-FLG
             MOVE  "R"      TO  EDIT-OPTION  OF  DSP-SHOCD
             MOVE  "R"      TO  EDIT-OPTION  OF  DSP-HINCD1
             MOVE  "R"      TO  EDIT-OPTION  OF  DSP-HINCD2
             MOVE  "R"      TO  EDIT-OPTION  OF  DSP-HINCD3
             MOVE  "C"      TO  EDIT-CURSOR  OF  DSP-SHOCD
         ELSE
             MOVE MEI-F021    TO   DSP-SHONM1
             MOVE MEI-F022    TO   DSP-SHONM2
             MOVE  "M"      TO  EDIT-OPTION  OF  DSP-SHOCD
             MOVE   SPACE   TO  EDIT-CURSOR  OF  DSP-SHOCD
         END-IF
     END-IF.
*取引先チェック
     IF  ERR-FLG   =  ZERO
         IF  DSP-TOKCDH  NOT NUMERIC
         OR  DSP-TOKCDH  =   ZERO
             MOVE     ZERO    TO   DSP-TOKCDH
             MOVE     SPACE   TO   DSP-TOKNMH
         END-IF
*
         IF  DSP-TOKCDH  NOT =  ZERO
             MOVE DSP-TOKCDH  TO  TOK-F01
             PERFORM TOKMS-READ-SEC
             IF  TOKMS-INV-FLG = "INV"
                 MOVE  SPACE    TO  DSP-TOKNMH
                 MOVE   3       TO  ERR-FLG
                 MOVE  "R"      TO  EDIT-OPTION  OF  DSP-TOKCDH
                 MOVE  "C"      TO  EDIT-CURSOR  OF  DSP-TOKCDH
             ELSE
                 MOVE  TOK-F02  TO  DSP-TOKNMH
                 MOVE  "M"      TO  EDIT-OPTION  OF  DSP-TOKCDH
                 MOVE   SPACE   TO  EDIT-CURSOR  OF  DSP-TOKCDH
             END-IF
         END-IF
     END-IF.
*エラーがあった場合はＥＸＩＴへ
     IF  ERR-FLG  NOT =  ZERO
         GO              TO      HEAD-CHK-EXIT
     END-IF.
*ファイルよりワークにセット
     PERFORM JYUZANL3-SET-SEC.
*明細存在チェック判定
     IF  MEISAI-CNT  =  ZERO
         MOVE   6       TO  ERR-FLG
         MOVE  "R"      TO  EDIT-OPTION  OF  DSP-SOKCDH
         MOVE  "R"      TO  EDIT-OPTION  OF  DSP-SHOCD
         MOVE  "R"      TO  EDIT-OPTION  OF  DSP-HINCD1
         MOVE  "R"      TO  EDIT-OPTION  OF  DSP-HINCD2
         MOVE  "R"      TO  EDIT-OPTION  OF  DSP-HINCD3
         MOVE  "R"      TO  EDIT-OPTION  OF  DSP-TOKCDH
         IF LINK-DSOKCD = "01" OR "88"
            MOVE "C"    TO  EDIT-CURSOR  OF  DSP-SOKCDH
         ELSE
            MOVE "C"    TO  EDIT-CURSOR  OF  DSP-SOKCDH
         END-IF
     ELSE
         MOVE    1      TO  P-CNT
         PERFORM WORK-DSP-SEC
     END-IF.
*
 HEAD-CHK-EXIT.
     EXIT.
****************************************************************
*    受注残ファイル読込（ＫＥＹ＝６）
****************************************************************
 JYUZANL3-SET-SEC       SECTION.
*
     MOVE     "JYUZANL3-SET-SEC"   TO   S-NAME.
*
     MOVE      SPACE               TO   JYUZANF-END-FLG.
*ファイルをオープンする。
     OPEN  INPUT  JYUZANF.
*ワーク領域初期化
     MOVE      SPACE               TO   TABLE-AREA.
     INITIALIZE                         TABLE-AREA.
     MOVE      SPACE               TO   WK-KEYCD.
*****INITIALIZE                         WK-KEYCD.
     MOVE      ZERO                TO   MEISAI-CNT  MEIALLCNT
                                        IX          IY.
*受注残ファイルスタート
     PERFORM JYUZANL3-START-SEC.
*　　終了判定
     IF  JYUZANF-END-FLG  =  "END"
         GO               TO  JYUZANL3-SET-020
     END-IF.
*受注残ファイル読込
 JYUZANL3-SET-010.
     PERFORM JYUZANL3-READ-SEC.
*　　終了判定
     IF  JYUZANF-END-FLG  =  "END"
         GO               TO  JYUZANL3-SET-020
     END-IF.
*
     IF  JYU-F09   =   KEY-SOKCD  AND
         JYU-F03   =   KEY-TOKCD  AND
         JYU-F11   =   KEY-NDATE  AND
         JYU-F13   =   KEY-SHOCD  AND
         JYU-F14   =   KEY-HINCD1 AND
         JYU-F15   =   KEY-HINCD2 AND
         JYU-F16   =   KEY-HINCD3 AND
         JYU-F17   =   KEY-TANABAN
*      *>受注数
         ADD  JYU-F18            TO      TBL-JSURYO(IX IY)
*      *>出荷数
         ADD  JYU-F19            TO      TBL-SSURYO(IX IY)
         IF   JYU-F25  = " "
*      *>引当済数
              ADD   JYU-F19      TO      TBL-HSURYO(IX IY)
         ELSE
*      *>未引当数　　　
              ADD   JYU-F19      TO      TBL-MSURYO(IX IY)
        END-IF
     ELSE
*********キーブレイク時、更新するテーブルＩＸ，ＩＹを算出
*********明細カウント
         ADD       1       TO     MEISAI-CNT
*********明細カウント／７算出　商　あまり算出
         DIVIDE   MEISAI-CNT   BY   7    GIVING    CHK-01
                                         REMAINDER CHK-02
*********商＋１をＩＸにセット
         COMPUTE   IX  =   CHK-01  +   1
*********余りをＩＹにセット
         MOVE  CHK-02             TO     IY
*********余りが０（割り切れた時）商をＩＸに７をＩＹにセット
         IF    CHK-02  =   ZERO
               MOVE     CHK-01    TO     IX
               MOVE     7         TO     IY
         END-IF
         MOVE  JYU-F09            TO     KEY-SOKCD
         MOVE  JYU-F03            TO     KEY-TOKCD
         MOVE  JYU-F11            TO     KEY-NDATE
         MOVE  JYU-F13            TO     KEY-SHOCD
         MOVE  JYU-F14            TO     KEY-HINCD1
         MOVE  JYU-F15            TO     KEY-HINCD2
         MOVE  JYU-F16            TO     KEY-HINCD3
         MOVE  JYU-F17            TO     KEY-TANABAN
*        *>倉庫ＣＤ／倉庫名
         MOVE  JYU-F09            TO     TBL-SOKCDM(IX IY)
         MOVE  JYU-F09            TO     SOK-F01
         PERFORM ZSOKMS-READ-SEC
         IF   ZSOKMS-INV-FLG = "INV"
              MOVE SPACE          TO     TBL-SOKNMM(IX IY)
         ELSE
              MOVE SOK-F02        TO     TBL-SOKNMM(IX IY)
         END-IF
*      *>取引先ＣＤ／仕入先名
         MOVE  JYU-F03             TO    TBL-TOKCDM(IX IY)
         MOVE  JYU-F03             TO    TOK-F01
         PERFORM TOKMS-READ-SEC
         IF   TOKMS-INV-FLG = "INV"
              MOVE SPACE           TO    TBL-TOKNMM(IX IY)
         ELSE
              MOVE TOK-F02         TO    TBL-TOKNMM(IX IY)
         END-IF
*       *>_番
          MOVE  JYU-F17(1:3)       TO    TBL-TANA1(IX IY)
          MOVE  JYU-F17(4:1)       TO    TBL-TANA2(IX IY)
          MOVE  JYU-F17(5:2)       TO    TBL-TANA3(IX IY)
*       *>納品日
          MOVE  JYU-F11            TO    TBL-NDATE(IX IY)
*       *>受注数
          ADD  JYU-F18             TO    TBL-JSURYO(IX IY)
*       *>出荷数
          ADD  JYU-F19             TO    TBL-SSURYO(IX IY)
          IF   JYU-F25  = "  "
*          *>引当済数
               ADD   JYU-F19       TO    TBL-HSURYO(IX IY)
           ELSE
*        *>未引当数　　　
               ADD   JYU-F19       TO    TBL-MSURYO(IX IY)
          END-IF
*
          MOVE    "1"              TO    PAGE-SONZAI-CHK(IX)
          MOVE    IX               TO    MEIALLCNT
*
     END-IF.
     GO           TO        JYUZANL3-SET-010.
*
 JYUZANL3-SET-020.
     CLOSE  JYUZANF.
*
 JYUZANL3-SET-EXIT.
     EXIT.
****************************************************************
*    ワークより、画面項目にセット
****************************************************************
 WORK-DSP-SEC              SECTION.
*
     MOVE "WORK-DSP-SEC"     TO   S-NAME.
*明細初期化
     PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > 7
             MOVE  SPACE     TO   DSP-BODY01(IX)
     END-PERFORM.
     MOVE  ZERO              TO   MEISAI-CNT.
*
     PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > 7
          IF TBL-TOKCDM(P-CNT IX) NOT = ZERO
             MOVE TBL-SOKCDM(P-CNT IX)  TO  DSP-SOKCDM(IX)
             MOVE TBL-SOKNMM(P-CNT IX)  TO  DSP-SOKNMM(IX)
             MOVE TBL-TANA1(P-CNT IX)  TO  DSP-TANA1(IX)
             MOVE TBL-TANA2(P-CNT IX)  TO  DSP-TANA2(IX)
             MOVE TBL-TANA3(P-CNT IX)  TO  DSP-TANA3(IX)
             MOVE TBL-JSURYO(P-CNT IX)  TO  DSP-JSURYO(IX)
             MOVE TBL-HSURYO(P-CNT IX)  TO  DSP-HSURYO(IX)
             MOVE TBL-TOKCDM(P-CNT IX)  TO  DSP-TOKCDM(IX)
             MOVE TBL-TOKNMM(P-CNT IX)  TO  DSP-TOKNMM(IX)
             MOVE TBL-NDATE(P-CNT IX)  TO  DSP-NDATE(IX)
             MOVE TBL-SSURYO(P-CNT IX)  TO  DSP-SSURYO(IX)
             MOVE TBL-MSURYO(P-CNT IX)  TO  DSP-MSURYO(IX)
*         ELSE
*            MOVE "X" TO   EDIT-STATUS OF DSP-ENDCHK
          END-IF
     END-PERFORM.
*
 WORK-DSP-EXIT.
     EXIT.
****************************************************************
*             明細部チェック                          2.4.1    *
****************************************************************
 MEISAI-CHK-SEC           SECTION.
*
     MOVE     "MEISAI-CHK-SEC"   TO   S-NAME.
*明細照会番号チェック
     IF  DSP-ENDCHK =  ZERO
     OR  DSP-ENDCHK NOT NUMERIC
         MOVE   SPACE   TO  DSP-ENDCHK
     END-IF.
*
     IF  DSP-ENDCHK =  1 OR 2 OR 3 OR 4 OR 5 OR 6 OR 7
         MOVE  "M"      TO  EDIT-OPTION  OF  DSP-ENDCHK
         MOVE   SPACE   TO  EDIT-CURSOR  OF  DSP-ENDCHK
         MOVE  DSP-ENDCHK     TO     WK-ENDCHK
     ELSE
         MOVE   9       TO  ERR-FLG
         MOVE  "R"      TO  EDIT-OPTION  OF  DSP-ENDCHK
         MOVE  "C"      TO  EDIT-CURSOR  OF  DSP-ENDCHK
         GO              TO     MEISAI-CHK-EXIT
     END-IF.
*明細データ存在チェック
*****DISPLAY "TBL-SOKCDM  = " TBL-SOKCDM(P-CNT DSP-ENDCHK)
*****                         UPON CONS.
     IF  TBL-TOKCDM(P-CNT WK-MEINO) > ZERO
         MOVE  "M"      TO  EDIT-OPTION  OF  DSP-ENDCHK
         MOVE   SPACE   TO  EDIT-CURSOR  OF  DSP-ENDCHK
     ELSE
         IF  ERR-FLG  =  ZERO
             MOVE   10      TO  ERR-FLG
         END-IF
         MOVE  "R"      TO  EDIT-OPTION  OF  DSP-ENDCHK
         MOVE  "C"      TO  EDIT-CURSOR  OF  DSP-ENDCHK
     END-IF.
*
*****DISPLAY "ERR-FLG = " ERR-FLG  UPON CONS.
     IF   ERR-FLG  NOT =  ZERO
          GO              TO     MEISAI-CHK-EXIT
     END-IF.
*
     MOVE  SPACE          TO     PA-MEISAI.
     INITIALIZE                  PA-MEISAI.
     MOVE  DSP-SOKCDM(WK-MEINO)            TO     PA-SOKCD.
     MOVE  DSP-SHOCD                       TO     PA-SHOCD.
     MOVE  DSP-HINCD1                      TO     PA-HINCD1.
     MOVE  DSP-HINCD2                      TO     PA-HINCD2.
     MOVE  DSP-HINCD3                      TO     PA-HINCD3.
     MOVE  DSP-TOKCDM(WK-MEINO)            TO     PA-TOKCD.
     MOVE  DSP-TANA1 (WK-MEINO)            TO     PA-TANA(1:3).
     MOVE  DSP-TANA2 (WK-MEINO)            TO     PA-TANA(4:1).
     MOVE  DSP-TANA3 (WK-MEINO)            TO     PA-TANA(5:2).
     MOVE  DSP-NDATE (WK-MEINO)            TO     PA-NDATE.
*
     CALL "SJZ0030I"  USING                       PA-SOKCD
                                                  PA-DSOKCD
                                                  PA-SHOCD
                                                  PA-HINCD1
                                                  PA-HINCD2
                                                  PA-HINCD3
                                                  PA-TOKCD
                                                  PA-TANA
                                                  PA-NDATE.
*
     MOVE  "M"      TO  EDIT-OPTION  OF  DSP-ENDCHK.
     MOVE   SPACE   TO  EDIT-CURSOR  OF  DSP-ENDCHK.
*全画面クリア（呼んだPGの画面表示をクリアする)
     MOVE  "CL"           TO  DSP-PRO.
     MOVE  "SCREEN"       TO  DSP-GRP.
     MOVE  "FJZ00101"     TO  DSP-FMT.
     WRITE  DSP-FJZ00101.
*通常出力指定に戻す。
     MOVE  SPACE          TO  DSP-PRO.
*
 MEISAI-CHK-EXIT.
     EXIT.
****************************************************************
*             確認処理　入力（ PSW = 2 ）             2.5
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
                PERFORM  MEISAI-CHK-SEC
                IF  ERR-FLG  =  ZERO
*                   MOVE SPACE  TO   DSP-MAS002
                    MOVE  "2"   TO   PSW
                END-IF
*取消
         WHEN   "F004"
                PERFORM  INIT-DSP-SEC
                MOVE    "1"      TO   PSW
                MOVE     1       TO   P-CNT
*終了
         WHEN   "F005"
                MOVE    "END"    TO   END-FLG
*項目戻り
         WHEN   "F006"
                MOVE    "1"      TO   PSW
                PERFORM DSP-ENDCHK-SEC
*前頁
         WHEN   "F011"
** 頁チェック
                COMPUTE C-CNT = P-CNT - 1
                IF      C-CNT = ZERO
                        MOVE  7  TO  ERR-FLG
                ELSE
                        COMPUTE P-CNT = P-CNT - 1
                        PERFORM WORK-DSP-SEC
                        MOVE    "2"      TO   PSW
                END-IF
                PERFORM DSP-ENDCHK-SEC
*次頁
         WHEN   "F012"
** 頁チェック
                MOVE    P-CNT   TO      S-CNT
                COMPUTE C-CNT = P-CNT + 1
                IF      C-CNT > MAX-PGCNT
                        MOVE   8   TO   ERR-FLG
                ELSE
                  IF    PAGE-SONZAI-CHK(C-CNT) = SPACE
                        MOVE   8     TO    ERR-FLG
                  ELSE
                        COMPUTE P-CNT = P-CNT + 1
                        PERFORM WORK-DSP-SEC
                        MOVE    "2"      TO   PSW
                  END-IF
                END-IF
                PERFORM DSP-ENDCHK-SEC
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
           MOVE    SPACE              TO   DSP-MSGSPC
     ELSE
           MOVE    ERR-MSG-R(ERR-FLG) TO   DSP-MSGSPC
           MOVE    ZERO               TO   ERR-FLG
     END-IF.
*ガイドメッセージの設定
     EVALUATE   PSW
         WHEN   "1"
           MOVE    PF-MSG-R(1)        TO   DSP-FNCSPC
         WHEN   "2"
           MOVE    PF-MSG-R(2)        TO   DSP-FNCSPC
     END-EVALUATE.
*画面の表示
     MOVE    "SCREEN"          TO   DSP-GRP.
     MOVE    "FJZ00101"        TO   DSP-FMT.
     WRITE    DSP-FJZ00101.
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
*確認
         WHEN   "2"
                MOVE    "GPBODY"  TO   DSP-GRP
     END-EVALUATE.

     MOVE    "FJZ00101"           TO   DSP-FMT.
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
     MOVE "M"   TO EDIT-OPTION OF DSP-SOKCDH.
     MOVE SPACE TO EDIT-CURSOR OF DSP-SOKCDH.
***  サカタ商品ＣＤ
     MOVE "M"   TO EDIT-OPTION OF DSP-SHOCD.
     MOVE SPACE TO EDIT-CURSOR OF DSP-SHOCD.
     MOVE "M"   TO EDIT-OPTION OF DSP-HINCD1.
     MOVE SPACE TO EDIT-CURSOR OF DSP-HINCD1.
     MOVE "M"   TO EDIT-OPTION OF DSP-HINCD2.
     MOVE SPACE TO EDIT-CURSOR OF DSP-HINCD2.
     MOVE "M"   TO EDIT-OPTION OF DSP-HINCD3.
     MOVE SPACE TO EDIT-CURSOR OF DSP-HINCD3.
***  取引先ＣＤ
     MOVE "M"   TO EDIT-OPTION OF DSP-TOKCDH.
     MOVE SPACE TO EDIT-CURSOR OF DSP-TOKCDH.
***  確認　　　
     MOVE "M"   TO EDIT-OPTION OF DSP-ENDCHK.
     MOVE SPACE TO EDIT-CURSOR OF DSP-ENDCHK.
*
 DSP-SYOKI-EXIT.
     EXIT.
****************************************************************
*         明細画面制御項目初期化                               *
****************************************************************
 DSP-BODYCL-SEC         SECTION.
     MOVE     "DSP-BODYCL-SEC"    TO   S-NAME.
     MOVE     ZERO                TO   MEIALLCNT.
     PERFORM VARYING Y FROM 1 BY 1 UNTIL  Y  >  MAX-LNCNT
         MOVE    SPACE    TO  DSP-BODY01(Y)
     END-PERFORM.
*
 DSP-BODYCL-EXIT.
     EXIT.
****************************************************************
*         確認画面制御項目初期化                               *
****************************************************************
 DSP-ENDCHK-SEC         SECTION.
     MOVE     "DSP-ENDCHK-SEC"    TO   S-NAME.
     MOVE     SPACE           TO  DSP-ENDCHK.
     MOVE "M"   TO EDIT-OPTION OF DSP-ENDCHK.
     MOVE SPACE TO EDIT-CURSOR OF DSP-ENDCHK.
*
 DSP-ENDCHK-EXIT.
     EXIT.
****************************************************************
*    受注残ファイルスタート
****************************************************************
 JYUZANL3-START-SEC    SECTION.
*
     MOVE    "JYUZANL3-START-SEC" TO   S-NAME.
*
     MOVE     SPACE               TO   JYU-REC.
     INITIALIZE                        JYU-REC.
*
     MOVE     DSP-SOKCDH          TO   JYU-F09.
     MOVE     DSP-TOKCDH          TO   JYU-F03.
     MOVE     DSP-SHOCD           TO   JYU-F13.
     MOVE     DSP-HINCD1          TO   JYU-F14.
     MOVE     DSP-HINCD2          TO   JYU-F15.
     MOVE     DSP-HINCD3          TO   JYU-F16.
*
*    DISPLAY "JYU-REC = " JYU-REC  UPON CONS.
     START  JYUZANF KEY IS >=  JYU-F13  JYU-F14  JYU-F15
                               JYU-F16  JYU-F09  JYU-F03
                               JYU-F17  JYU-F11
            INVALID
            MOVE  "END"           TO   JYUZANF-END-FLG
     END-START.
*
 JYUZANL3-START-EXIT.
     EXIT.
****************************************************************
*    受注残ファイル読込
****************************************************************
 JYUZANL3-READ-SEC     SECTION.
*
     MOVE     "JYUZANL3-READ-SEC" TO   S-NAME.
*
 JYUZANL3-010.
     READ   JYUZANF
            NEXT  AT  END    MOVE  "END"   TO   JYUZANF-END-FLG
                             GO            TO   JYUZANL3-READ-EXIT
     END-READ.
 JYUZANL3-020.
*倉庫ＣＤチェック
     IF     DSP-SOKCDH  NOT =  SPACE
            IF  DSP-SOKCDH  NOT = JYU-F09
*               MOVE  "END"   TO   JYUZANF-END-FLG
                GO            TO   JYUZANL3-READ-SEC
            END-IF
     END-IF.
 JYUZANL3-030.
*サカタ商品ＣＤ
     IF     DSP-SHOCD  NOT =  SPACE
            IF  DSP-SHOCD  =  JYU-F13
            AND DSP-HINCD1  =  JYU-F14
            AND DSP-HINCD2  =  JYU-F15
            AND DSP-HINCD3  =  JYU-F16
                CONTINUE
            ELSE
                MOVE  "END"   TO   JYUZANF-END-FLG
                GO        TO   JYUZANL3-READ-EXIT
            END-IF
     END-IF.
 JYUZANL3-040.
*取引先ＣＤチェック
     IF     DSP-TOKCDH   >  ZERO
            IF  DSP-TOKCDH  NOT = JYU-F03
*               MOVE  "END"   TO   JYUZANF-END-FLG
                GO        TO   JYUZANL3-READ-SEC
            END-IF
     END-IF.
 JYUZANL3-050.
*棚番
     IF     LINK-SHOCD  NOT =  SPACE
            IF  LINK-TANA  =  JYU-F17
                CONTINUE
            ELSE
                GO        TO   JYUZANL3-READ-SEC
            END-IF
     END-IF.
*
 JYUZANL3-READ-EXIT.
     EXIT.
****************************************************************
*    受注残ファイル読込　ＲＡＮＤＯＭ
****************************************************************
 JYUZANF-READ-SEC      SECTION.
*
     MOVE     "JYUZANF-READ-SEC"  TO   S-NAME.
*
     READ   JYUZANF
            INVALID     MOVE  "INV"   TO   JYUZANF-INV-FLG
            NOT INVALID MOVE  SPACE   TO   JYUZANF-INV-FLG
     END-READ.
*
 JYUZANF-READ-EXIT.
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
*    商品変換ﾏｽﾀ読込
****************************************************************
 MEIMS-READ-SEC      SECTION.
*
     MOVE     "MEIMS-READ-SEC"  TO   S-NAME.
*
     READ   HMEIMS
            INVALID     MOVE  "INV"   TO   MEIMS-INV-FLG
            NOT INVALID MOVE  SPACE   TO   MEIMS-INV-FLG
     END-READ.
*
 MEIMS-READ-EXIT.
     EXIT.
****************************************************************
*    取引先マスタ
****************************************************************
 TOKMS-READ-SEC       SECTION.
*
     MOVE     "TOKMS-READ-SEC"   TO   S-NAME.
*
     READ   TOKMS
            INVALID     MOVE  "INV"   TO   TOKMS-INV-FLG
            NOT INVALID MOVE  SPACE   TO   TOKMS-INV-FLG
     END-READ.
*
 TOKMS-READ-EXIT.
     EXIT.
*******************< PROGRAM-END SFU3060I >*********************
