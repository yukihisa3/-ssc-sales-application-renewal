****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　ナフコＥＤＩ　　　　　　　　　　　*
*    業務名　　　　　　　：　ナフコＥＤＩ　　　　　　　        *
*    モジュール名　　　　：　ナフコ受領状況照会　              *
*    作成日／作成者　　　：　2020.06.23   /  NAV TAKAHASHI     *
*    更新日／更新者　　　：　　　　　　                        *
*    処理概要　　　　　　：　ナフコの受領情報の照会を行う。　　*
*                                                              *
****************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SSY3980I.
 AUTHOR.               NAV.
 DATE-WRITTEN.         2020/06/23.
****************************************************************
 ENVIRONMENT           DIVISION.
****************************************************************
 CONFIGURATION         SECTION.
 SPECIAL-NAMES.
     CONSOLE      IS   CONS.
*
 INPUT-OUTPUT          SECTION.
 FILE-CONTROL.
*---<<  画面定義ファイル  >>--*
     SELECT   DSPFILE        ASSIGN        TO  GS-DSPF
                             FORMAT        IS  DSP-FMT
                             GROUP         IS  DSP-GRP
                             PROCESSING    IS  DSP-PRO
                             FUNCTION      IS  DSP-FNC
                             FILE STATUS   IS  DSP-ST.
*---<< ナフコ受領出荷受領統合ファイル >>--*
     SELECT     NFCKDTF      ASSIGN        TO  DA-01-VI-NFCKDTL3
                             ORGANIZATION  IS  INDEXED
                             ACCESS MODE   IS  SEQUENTIAL
                             RECORD KEY    IS  NFC-F00
                                               NFC-F12
                                               NFC-F300
                                               NFC-F01
                                               NFC-F03
                             FILE STATUS   IS  NFC-ST.
*---<<  ナフコ受領出荷照会ワーク  >>--*
     SELECT   NFCKJRF        ASSIGN        TO  01-VI-NFCKJRL1
                             ORGANIZATION  IS  INDEXED
                             ACCESS MODE   IS  SEQUENTIAL
                             RECORD KEY    IS  NFW-F99
                             FILE STATUS   IS  NFW-ST.
*---<<  ナフコ店舗マスタ  >>--*
     SELECT   NFTENMS        ASSIGN        TO  01-VI-NFTENMS1
                             ORGANIZATION  IS  INDEXED
                             ACCESS MODE   IS  RANDOM
                             RECORD KEY    IS  TEN-F01
                                               TEN-F02
                             FILE STATUS   IS  TEN-ST.
*
****************************************************************
 DATA                DIVISION.
****************************************************************
 FILE                SECTION.
****************************************************************
*    FILE = 画面ファイル                                       *
****************************************************************
 FD  DSPFILE
                       LABEL     RECORD    IS   OMITTED.
                       COPY      FSY39801  OF   XMDLIB
                       JOINING   DSP       AS   PREFIX.
*
****************************************************************
*    FILE = ナフコ受領出荷受領統合ファイル                     *
****************************************************************
 FD  NFCKDTF
                       LABEL     RECORD    IS   STANDARD.
                       COPY      NFCKDTF   OF   XFDLIB
                       JOINING   NFC       AS   PREFIX.
****************************************************************
*    FILE = ナフコ受領出荷照会ワーク                           *
****************************************************************
 FD  NFCKJRF
                       LABEL     RECORD    IS   STANDARD.
                       COPY      NFCKJRF   OF   XFDLIB
                       JOINING   NFW       AS   PREFIX.
****************************************************************
*    FILE = ナフコ店舗マスタ                                 *
****************************************************************
 FD  NFTENMS
                       LABEL     RECORD    IS   STANDARD.
                       COPY      NFTENMS   OF   XFDLIB
                       JOINING   TEN       AS   PREFIX.
****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
*ステータス領域
 01  STATUS-AREA.
     03  DSP-ST                   PIC  X(02).
     03  NFW-ST                   PIC  X(02).
     03  NFC-ST                   PIC  X(02).
     03  TEN-ST                   PIC  X(02).
*画面制御用領域
 01  DSP-CONTROL.
     03  DSP-FMT                  PIC  X(08).
     03  DSP-GRP                  PIC  X(08).
     03  DSP-PRO                  PIC  X(02).
     03  DSP-FNC                  PIC  X(04).
***  プログラムスイッチ（画面遷移制御）
 01  PSW                          PIC  X(01)  VALUE  SPACE.
*フラグ領域
 01  FLG-AREA.
     03  END-FLG                  PIC  X(03)  VALUE  SPACE.
     03  INV-FLG                  PIC  9(01)  VALUE  ZERO.
     03  READ-FLG                 PIC  9(01)  VALUE  ZERO.
     03  ERR-FLG                  PIC  9(02)  VALUE  ZERO.
     03  NFTENMS-INV-FLG          PIC  X(03)  VALUE  SPACE.
*カウント領域
 01  CNT-AREA.
     03  P-CNT                    PIC  9(07)  VALUE  ZERO.
     03  MAX-CNT                  PIC  9(07)  VALUE  ZERO.
     03  OUT-CNT                  PIC  9(07)  VALUE  ZERO.
     03  IX1                      PIC  9(02)  VALUE  ZERO.
     03  WRK-RENBAN               PIC  9(08)  OCCURS 4
                                              VALUE  ZERO.
*ワーク領域
 01  WRK-AREA.
     03  WK-YOTEIBI               PIC  9(08).
     03  WK-BDY-KEY.
         05  WK-BDY-F02           PIC  9(07).
     03  WK-SHOCD.
         05  WK-SHO               PIC  X(01)  OCCURS 8.
     03  WK-DENKU                 PIC  X(02)  VALUE  SPACE.
     03  WK-AKAKURO               PIC  X(02)  VALUE  SPACE.
 01  WK-TANA.
     03  WK-TANA1                 PIC  X(01).
     03  FILLER                   PIC  X(01)  VALUE  "-".
     03  WK-TANA2                 PIC  X(03).
     03  FILLER                   PIC  X(01)  VALUE  "-".
     03  WK-TANA3                 PIC  X(02).
 01  WRK-MSYON.
     03  WRK-MSYON-1              PIC  N(20).
     03  WRK-MSYON-2              PIC  N(11).
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
*
*ＰＦガイド
 01  PF-MSG-AREA.
     03  PF-MSG1                  PIC  N(20)  VALUE
         NC"_取消_終了".
     03  PF-MSG2                  PIC  N(20)  VALUE
         NC"_取消_終了_項目戻り_前頁_次頁".
 01  PF-MSG-AREA-R       REDEFINES     PF-MSG-AREA.
     03  PF-MSG-R   OCCURS   2    PIC   N(20).
*
*メッセージの取得
 01  ERR-MSG-AREA.
     03  ERR-MSG1                 PIC  N(20)  VALUE
         NC"該当データは存在しません。".
     03  ERR-MSG2                 PIC  N(20)  VALUE
         NC"無効キーです。".
     03  ERR-MSG3                 PIC  N(20)  VALUE
         NC"前頁はありません。　　　".
     03  ERR-MSG4                 PIC  N(20)  VALUE
         NC"次頁はありません。　　　".
     03  ERR-MSG5                 PIC  N(20)  VALUE
         NC"必須入力です。".
     03  ERR-MSG6                 PIC  N(20)  VALUE
         NC"伝票区分が違います。".
     03  ERR-MSG7                 PIC  N(20)  VALUE
         NC"赤黒区分が違います。".
     03  ERR-MSG8                 PIC  N(20)  VALUE
         NC"年月日に誤りがあります。".
     03  ERR-MSG9                 PIC  N(20)  VALUE
         NC"開始が終了を超えています。".
 01  ERR-MSG-AREA-R      REDEFINES     ERR-MSG-AREA.
     03  ERR-MSG-R   OCCURS  9    PIC  N(20).
*
*ファイルエラーメッセージ
 01  FILE-ERR.
     03  DSP-ERR  PIC N(15) VALUE  NC"画面ファイルエラー".
     03  NFC-ERR  PIC N(15) VALUE  NC"ナフコ統合ファイルエラー".
     03  NFW-ERR  PIC N(15) VALUE  NC"ナフコ照会ワークエラー".
     03  TEN-ERR  PIC N(15) VALUE  NC"ナフコ店舗マスタエラー".
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
**************************************************************
 LINKAGE               SECTION.
**************************************************************
 01  LINK-TOKCD            PIC X(08).
**************************************************************
 PROCEDURE             DIVISION     USING LINK-TOKCD.
**************************************************************
 DECLARATIVES.
 DSP-SEC                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE DSPFILE.
     MOVE        DSP-ST    TO        E-ST.
     MOVE        "DSPFILE" TO        E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     DSP-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 NFC-SEC                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE NFCKDTF.
     MOVE        NFC-ST    TO        E-ST.
     MOVE       "NFCKDTL3" TO        E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     NFC-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 NFW-SEC                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE NFCKJRF.
     MOVE        NFW-ST    TO        E-ST.
     MOVE        "NFCKJRL1" TO       E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     NFW-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 TEN-SEC                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE NFTENMS.
     MOVE        TEN-ST    TO        E-ST.
     MOVE        "NFTENMS1" TO       E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     TEN-ERR   UPON      CONS.
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
*ファイルのＯＰＥＮ
     OPEN      INPUT    NFCKJRF   NFCKDTF.
     OPEN      INPUT    NFTENMS.
     OPEN      I-O      DSPFILE.
*ワークの初期化
     INITIALIZE                   FLG-AREA.
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
**
     EVALUATE      PSW
*初期画面表示
         WHEN      "1"       PERFORM   DSP-INIT-SEC
*ヘッダ部入力
         WHEN      "2"       PERFORM   DSP-HEAD-SEC
*確認入力
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
     CLOSE             DSPFILE  NFCKJRF  NFCKDTF  NFTENMS.
**
 END-EXIT.
     EXIT.
****************************************************************
*             初期画面表示( PSW = 1 )                2.1       *
****************************************************************
 DSP-INIT-SEC          SECTION.
     MOVE     "DSP-INIT-SEC"      TO   S-NAME.
**
     MOVE     SPACE               TO   DSP-PRO.
*
     MOVE    SPACE                TO   DSP-FSY39801.
     MOVE    "SSY3980I"           TO   DSP-PGID.
     MOVE    "FSY39801"           TO   DSP-FORMID.
     MOVE    HEN-DATE             TO   DSP-SDATE.
     MOVE    HEN-TIME             TO   DSP-STIME.
     MOVE    LINK-TOKCD           TO   DSP-PTORCD.
*項目属性クリア
     PERFORM  DSP-SYOKI-SEC.
*ヘッダ部入力へ
     MOVE     "2"                TO   PSW.
*
 DSP-INIT-EXIT.
     EXIT.
****************************************************************
*             ヘッダ部入力( PSW = 2 )                2.2       *
****************************************************************
 DSP-HEAD-SEC          SECTION.
     MOVE     "DSP-HEAD-SEC"      TO   S-NAME.
**
     PERFORM    DSP-WRITE-SEC.
     PERFORM    DSP-READ-SEC.
*
     EVALUATE   DSP-FNC
*実行
         WHEN   "E000"       PERFORM   HEAD-CHK-SEC
*取消
         WHEN   "F004"       MOVE    "1"      TO   PSW
*終了
         WHEN   "F005"       MOVE    "END"    TO   END-FLG
*他
         WHEN    OTHER       MOVE     2       TO   ERR-FLG
     END-EVALUATE.
*
 DSP-HEAD-EXIT.
     EXIT.
****************************************************************
*             ヘッダ部チェック                 2.2.1           *
****************************************************************
 HEAD-CHK-SEC             SECTION.
     MOVE     "HEAD-CHK-SEC"     TO   S-NAME.
*発注日開始チェック
***  仕入計上日開始　未入力チェック
     IF       DSP-JYUST  NOT NUMERIC
         OR   DSP-JYUST  =  ZERO
              MOVE   5       TO   ERR-FLG
              MOVE  "R"      TO   EDIT-OPTION  OF  DSP-JYUST
              MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-JYUST
              GO             TO   HEAD-CHK-EXIT
     ELSE
***           論理チェック
              MOVE     "2"            TO   LINK-IN-KBN
              MOVE     ZERO           TO   LINK-IN-YMD6
              MOVE     DSP-JYUST      TO   LINK-IN-YMD8
              MOVE     ZERO           TO   LINK-OUT-RET
              MOVE     ZERO           TO   LINK-OUT-YMD
              CALL     "SKYDTCKB"     USING   LINK-IN-KBN
                                              LINK-IN-YMD6
                                              LINK-IN-YMD8
                                              LINK-OUT-RET
                                              LINK-OUT-YMD
              IF   LINK-OUT-RET   = 9
                   MOVE  8        TO   ERR-FLG
                   MOVE  "R"      TO   EDIT-OPTION  OF  DSP-JYUST
                   MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-JYUST
                   GO             TO   HEAD-CHK-EXIT
              END-IF
*
              MOVE  "M"      TO   EDIT-OPTION  OF  DSP-JYUST
              MOVE  SPACE    TO   EDIT-CURSOR  OF  DSP-JYUST
     END-IF.
***  仕入計上日終了　未入力チェック
     IF       DSP-JYUED  NOT NUMERIC
         OR   DSP-JYUED  =  ZERO
         OR   DSP-JYUED  =  99999999
              MOVE   5       TO   ERR-FLG
              MOVE  "R"      TO   EDIT-OPTION  OF  DSP-JYUED
              MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-JYUED
              GO             TO   HEAD-CHK-EXIT
     ELSE
***           論理チェック
              MOVE     "2"            TO   LINK-IN-KBN
              MOVE     ZERO           TO   LINK-IN-YMD6
              MOVE     DSP-JYUED      TO   LINK-IN-YMD8
              MOVE     ZERO           TO   LINK-OUT-RET
              MOVE     ZERO           TO   LINK-OUT-YMD
              CALL     "SKYDTCKB"     USING   LINK-IN-KBN
                                              LINK-IN-YMD6
                                              LINK-IN-YMD8
                                              LINK-OUT-RET
                                              LINK-OUT-YMD
              IF   LINK-OUT-RET   = 9
                   MOVE  8        TO   ERR-FLG
                   MOVE  "R"      TO   EDIT-OPTION  OF  DSP-JYUED
                   MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-JYUED
                   GO             TO   HEAD-CHK-EXIT
              END-IF
*
              MOVE  "M"      TO   EDIT-OPTION  OF  DSP-JYUED
              MOVE  SPACE    TO   EDIT-CURSOR  OF  DSP-JYUED
     END-IF.
*仕入計上日範囲チェック
     IF       DSP-JYUST   >  DSP-JYUED
              MOVE  9        TO   ERR-FLG
              MOVE  "R"      TO   EDIT-OPTION  OF  DSP-JYUST
              MOVE  "R"      TO   EDIT-OPTION  OF  DSP-JYUED
              MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-JYUST
              GO             TO   HEAD-CHK-EXIT
     ELSE
              MOVE  "M"      TO   EDIT-OPTION  OF  DSP-JYUST
              MOVE  "M"      TO   EDIT-OPTION  OF  DSP-JYUED
              MOVE  SPACE    TO   EDIT-CURSOR  OF  DSP-JYUST
     END-IF.
*店舗範囲チェック
     IF       DSP-TENCDS NOT NUMERIC
         OR   DSP-TENCDS =  ZERO
              MOVE   5       TO   ERR-FLG
              MOVE  "R"      TO   EDIT-OPTION  OF  DSP-TENCDS
              MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-TENCDS
              GO             TO   HEAD-CHK-EXIT
     END-IF.
     IF       DSP-TENCDE NOT NUMERIC
         OR   DSP-TENCDE =  ZERO
              MOVE   5       TO   ERR-FLG
              MOVE  "R"      TO   EDIT-OPTION  OF  DSP-TENCDE
              MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-TENCDE
              GO             TO   HEAD-CHK-EXIT
     END-IF.
     IF       DSP-TENCDS  >  DSP-TENCDE
              MOVE  9        TO   ERR-FLG
              MOVE  "R"      TO   EDIT-OPTION  OF  DSP-TENCDS
              MOVE  "R"      TO   EDIT-OPTION  OF  DSP-TENCDE
              MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-TENCDS
              GO             TO   HEAD-CHK-EXIT
     ELSE
              MOVE  "M"      TO   EDIT-OPTION  OF  DSP-TENCDS
              MOVE  "M"      TO   EDIT-OPTION  OF  DSP-TENCDE
              MOVE  SPACE    TO   EDIT-CURSOR  OF  DSP-TENCDS
     END-IF.
*伝票区分チェック
     IF       DSP-HDDENK  =  SPACE
     OR       DSP-HDDENK  =  "01"
     OR       DSP-HDDENK  =  "02"
     OR       DSP-HDDENK  =  "11"
     OR       DSP-HDDENK  =  "12"
     OR       DSP-HDDENK  =  "61"
              MOVE  "M"      TO   EDIT-OPTION  OF  DSP-HDDENK
              MOVE  "M"      TO   EDIT-OPTION  OF  DSP-HDDENK
              MOVE  SPACE    TO   EDIT-CURSOR  OF  DSP-HDDENK
     ELSE
              MOVE  6        TO   ERR-FLG
              MOVE  "R"      TO   EDIT-OPTION  OF  DSP-HDDENK
              MOVE  "R"      TO   EDIT-OPTION  OF  DSP-HDDENK
              MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-HDDENK
              GO             TO   HEAD-CHK-EXIT
     END-IF.
*赤黒区分チェック
     IF       DSP-HDAKKN  =  SPACE
     OR       DSP-HDAKKN  =  "00"
     OR       DSP-HDAKKN  =  "01"
              MOVE  "M"      TO   EDIT-OPTION  OF  DSP-HDAKKN
              MOVE  "M"      TO   EDIT-OPTION  OF  DSP-HDAKKN
              MOVE  SPACE    TO   EDIT-CURSOR  OF  DSP-HDAKKN
     ELSE
              MOVE  7        TO   ERR-FLG
              MOVE  "R"      TO   EDIT-OPTION  OF  DSP-HDAKKN
              MOVE  "R"      TO   EDIT-OPTION  OF  DSP-HDAKKN
              MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-HDAKKN
              GO             TO   HEAD-CHK-EXIT
     END-IF.
*#####################
*該当データ検索
     IF       ERR-FLG  =  ZERO
              PERFORM     NFCKDTF-START-SEC
     END-IF.
*ワークへデータセット
     IF       ERR-FLG  =  ZERO
              PERFORM     MST-WORK-SEC
     END-IF.
*
     IF       ERR-FLG  =  1
              MOVE    "R"      TO   EDIT-OPTION OF DSP-JYUST
              MOVE    "R"      TO   EDIT-OPTION OF DSP-JYUED
              MOVE    "R"      TO   EDIT-OPTION OF DSP-TENCDS
              MOVE    "R"      TO   EDIT-OPTION OF DSP-TENCDE
              MOVE    "R"      TO   EDIT-OPTION OF DSP-HDDENK
              MOVE    "R"      TO   EDIT-OPTION OF DSP-HDAKKN
              MOVE    "C"      TO   EDIT-CURSOR OF DSP-JYUST
     END-IF.
*
     IF       ERR-FLG  =  ZERO
              MOVE  "3"             TO   PSW
     END-IF.
 HEAD-CHK-EXIT.
     EXIT.
****************************************************************
*             該当データの検索                                 *
****************************************************************
 MST-WORK-SEC          SECTION.
     MOVE     "MST-WORK-SEC"   TO   S-NAME.
**
     CLOSE              NFCKJRF.
     OPEN     OUTPUT    NFCKJRF.
*対象データ抽出
     MOVE     ZERO      TO     OUT-CNT.
     PERFORM            UNTIL  INV-FLG NOT = ZERO
                        PERFORM   OUT-WRITE-SEC
                        PERFORM   NFCKDTF-READ-SEC
     END-PERFORM.
*
     CLOSE              NFCKJRF.
     OPEN     INPUT     NFCKJRF.
     IF       OUT-CNT    =    0
              MOVE     1       TO   ERR-FLG
              GO       TO      MST-WORK-EXIT
     END-IF.
*初ページ編集
     COMPUTE   MAX-CNT  =  ( OUT-CNT + 3 )  /  4.
     MOVE      1               TO   P-CNT.
     MOVE      0               TO   READ-FLG.
     PERFORM   MEISAI-SET-SEC
               VARYING IX1 FROM 1 BY 1   UNTIL IX1 > 4.
 MST-WORK-EXIT.
     EXIT.
****************************************************************
*             ワークファイル出力                               *
****************************************************************
 OUT-WRITE-SEC         SECTION.
     MOVE     "OUT-WRITE-SEC"    TO   S-NAME.
*
     MOVE     SPACE              TO   NFW-REC.
     INITIALIZE                       NFW-REC.
*レコードセット
     MOVE      NFC-REC           TO   NFW-REC.
*
*連番セット
     ADD  1                       TO   OUT-CNT.
     MOVE OUT-CNT                 TO   NFW-F99.
*
     WRITE  NFW-REC.
*
 OUT-WRITE-EXIT.
     EXIT.
****************************************************************
*             ワーク→画面表示                                 *
****************************************************************
 MEISAI-SET-SEC        SECTION.
     MOVE     "MEISAI-SET-SEC"   TO   S-NAME.
*
     MOVE     SPACE              TO   DSP-MAS001(IX1).
     IF       READ-FLG      =  ZERO
              PERFORM  NFCKJRF-READ-SEC
     END-IF.
     IF       READ-FLG  NOT =  ZERO
              GO     TO          MEISAI-SET-EXIT
     END-IF.
*出荷ＮＯ
     MOVE     NFW-F204           TO   DSP-MSYUNO(IX1).
*伝票ＮＯ
     MOVE     NFW-F03            TO   DSP-MDENNO(IX1).
*作場CD
     MOVE     NFW-F200           TO   DSP-MSAKCD(IX1).
*伝票区分
     MOVE     NFW-F05            TO   DSP-MDENKU(IX1).
     EVALUATE NFW-F05
        WHEN  01
              MOVE  NC"ＴＡ仕入" TO   DSP-MDENNM(IX1)
        WHEN  02
              MOVE  NC"手書仕入" TO   DSP-MDENNM(IX1)
        WHEN  11
              MOVE  NC"返品　　" TO   DSP-MDENNM(IX1)
        WHEN  12
              MOVE  NC"仕入値引" TO   DSP-MDENNM(IX1)
        WHEN  61
              MOVE  NC"相殺　　" TO   DSP-MDENNM(IX1)
        WHEN  OTHER
              MOVE  NC"＊＊　　" TO   DSP-MDENNM(IX1)
     END-EVALUATE.
*赤黒区分
     MOVE     NFW-F06            TO   DSP-MAKKBN(IX1).
     EVALUATE NFW-F06
        WHEN  00
              MOVE  NC"黒伝　　　" TO   DSP-MAKNM (IX1)
        WHEN  01
              MOVE  NC"赤伝　　　" TO   DSP-MAKNM (IX1)
        WHEN  OTHER
              MOVE  NC"＊＊＊＊＊" TO   DSP-MAKNM (IX1)
     END-EVALUATE.
*発注日
     MOVE     NFW-F201(3:6)      TO   DSP-MHATYU(IX1).
*納品日
     MOVE     NFW-F202(3:6)      TO   DSP-MNOUDT(IX1).
*発注数
     MOVE     NFW-F205           TO   DSP-MHATSU(IX1).
*出荷数
     IF       NFW-F11  =  "1"
              MOVE     NFW-F206           TO   DSP-MSYUSU(IX1)
     END-IF.
*店舗CD/店舗名
     MOVE     LINK-TOKCD         TO   TEN-F01.
     MOVE     NFW-F01            TO   DSP-MTENCD(IX1)
                                      TEN-F02.
     PERFORM  NFTENMS-READ-SEC.
     IF       NFTENMS-INV-FLG  =  SPACE
              MOVE    TEN-F05    TO   DSP-MTENNM(IX1)
     ELSE
              MOVE    ALL NC"＊" TO   DSP-MTENNM(IX1)
     END-IF.
*JANCD
     MOVE     NFW-F20B           TO   DSP-MJANCD(IX1).
*商品名
     MOVE     NFW-F08            TO   WRK-MSYON-1.
     MOVE     NFW-F09            TO   WRK-MSYON-2.
     MOVE     WRK-MSYON          TO   DSP-MSYON1(IX1).
*理由区分
     IF       NFW-F12  =  "1"
              MOVE     NFW-F307           TO   DSP-MRIYUK(IX1)
     END-IF.
*理由CD
     IF       NFW-F12  =  "1"
              MOVE     NFW-F308           TO   DSP-MRIYUC(IX1)
     END-IF.
*理由名
     IF       NFW-F12  =  "1"
              MOVE     NFW-F309           TO   DSP-MRIYNM(IX1)
     END-IF.
*仕入計上日
     IF       NFW-F12  =  "1"
              MOVE     NFW-F300           TO   DSP-MSIRDT(IX1)
     END-IF.
*受領数
     IF       NFW-F12  =  "1"
              MOVE     NFW-F304           TO   DSP-MJYUSU(IX1)
     END-IF.
*元伝票番号
     MOVE     NFW-F301           TO   DSP-MMOTOD(IX1).
*連番
     MOVE     NFW-F99            TO   WRK-RENBAN(IX1).
*
 MEISAI-SET-EXIT.
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
*実行
         WHEN   "E000"       MOVE    "3"      TO   PSW
*取消
         WHEN   "F004"       MOVE    "1"      TO   PSW
*終了
         WHEN   "F005"       MOVE    "END"    TO   END-FLG
*項目戻り
         WHEN   "F006"       MOVE    "2"      TO   PSW
*前頁
         WHEN   "F011"       PERFORM  ZEN-PAGE-SEC
*次頁
         WHEN   "F012"       PERFORM  JI-PAGE-SEC
*他
         WHEN   OTHER        MOVE     2       TO   ERR-FLG
     END-EVALUATE.
*
 DSP-KAKU-EXIT.
     EXIT.
****************************************************************
*             前ページ                                         *
****************************************************************
 ZEN-PAGE-SEC          SECTION.
     MOVE     "ZEN-PAGE-SEC"      TO   S-NAME.
**
     IF        P-CNT     =    1
               MOVE    3          TO   ERR-FLG
               GO      TO         ZEN-PAGE-EXIT
     END-IF.
*
     MOVE      WRK-RENBAN(1)      TO   NFW-F99.
     START     NFCKJRF    KEY IS   <   NFW-F99
               WITH REVERSED ORDER
        INVALID
               MOVE     3         TO    ERR-FLG
               GO       TO        ZEN-PAGE-EXIT
     END-START.
*
     SUBTRACT  1             FROM   P-CNT.
     MOVE      0               TO   READ-FLG.
     PERFORM   MEISAI-SET-SEC
               VARYING IX1 FROM 4 BY -1  UNTIL IX1 = 0.
*
 ZEN-PAGE-EXIT.
     EXIT.
****************************************************************
*             次ページ                                         *
****************************************************************
 JI-PAGE-SEC           SECTION.
     MOVE     "JI-PAGE-SEC"       TO   S-NAME.
**
     IF        P-CNT     =    MAX-CNT
               MOVE    4          TO   ERR-FLG
               GO      TO         JI-PAGE-EXIT
     END-IF.
*
     MOVE      WRK-RENBAN(4)      TO   NFW-F99.
     START     NFCKJRF    KEY IS   >   NFW-F99
        INVALID
               MOVE     4         TO    ERR-FLG
               GO       TO        JI-PAGE-EXIT
     END-START.
*
     ADD       1               TO   P-CNT.
     MOVE      0               TO   READ-FLG.
     PERFORM   MEISAI-SET-SEC
               VARYING IX1 FROM 1 BY 1   UNTIL IX1 > 4.
*
 JI-PAGE-EXIT.
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
         WHEN   "2"
                MOVE    PF-MSG-R(1)        TO   DSP-PFMSG
         WHEN   "3"
                MOVE    PF-MSG-R(2)        TO   DSP-PFMSG
     END-EVALUATE.
*画面の表示
     MOVE    "SCREEN"            TO   DSP-GRP.
     MOVE    "FSY39801"          TO   DSP-FMT.
     WRITE    DSP-FSY39801.
*
 DSP-WRITE-EXIT.
     EXIT.
****************************************************************
*             画面読込処理                                     *
****************************************************************
 DSP-READ-SEC          SECTION.
     MOVE     "DSP-READ-SEC"      TO   S-NAME.
**
     MOVE    "NE"                 TO   DSP-PRO.
     EVALUATE   PSW
         WHEN   "2"
                MOVE    "HED001"  TO   DSP-GRP
         WHEN   "3"
                MOVE    "KAKU"    TO   DSP-GRP
     END-EVALUATE.
     MOVE    "FSY39801"           TO   DSP-FMT.
     READ    DSPFILE.
*
 DSP-READ-010.
     PERFORM  DSP-SYOKI-SEC.
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
**
     MOVE   "M"        TO  EDIT-OPTION OF DSP-JYUST.
     MOVE   "M"        TO  EDIT-OPTION OF DSP-JYUED.
     MOVE   "M"        TO  EDIT-OPTION OF DSP-TENCDS.
     MOVE   "M"        TO  EDIT-OPTION OF DSP-TENCDE.
     MOVE   "M"        TO  EDIT-OPTION OF DSP-HDDENK.
     MOVE   "M"        TO  EDIT-OPTION OF DSP-HDAKKN.
     MOVE   SPACE      TO  EDIT-CURSOR OF DSP-JYUST.
     MOVE   SPACE      TO  EDIT-CURSOR OF DSP-JYUED.
     MOVE   SPACE      TO  EDIT-CURSOR OF DSP-TENCDS.
     MOVE   SPACE      TO  EDIT-CURSOR OF DSP-TENCDE.
     MOVE   SPACE      TO  EDIT-CURSOR OF DSP-HDDENK.
     MOVE   SPACE      TO  EDIT-CURSOR OF DSP-HDAKKN.
*
 DSP-SYOKI-EXIT.
     EXIT.
****************************************************************
*    ナフコ受領出荷照会ワーク　順ＲＥＡＤ                      *
****************************************************************
 NFCKJRF-READ-SEC      SECTION.
*
     MOVE     "NFCKJRF-READ-SEC"  TO   S-NAME.
**
     READ     NFCKJRF
         AT END     MOVE  1       TO   READ-FLG
     END-READ.
*
 NFCKJRF-READ-EXIT.
     EXIT.
****************************************************************
*    ナフコ受領出荷受領統合ファイル　ＳＴＡＲＴ
****************************************************************
 NFCKDTF-START-SEC     SECTION.
     MOVE     "NFCKDTF-START-SEC" TO   S-NAME.
**
     MOVE      SPACE              TO   NFC-REC.
     INITIALIZE                        NFC-REC
     MOVE      LINK-TOKCD         TO   NFC-F00.
     MOVE      "1"                TO   NFC-F12.
     MOVE      DSP-JYUST          TO   NFC-F300.
     MOVE      DSP-TENCDS         TO   NFC-F01.
     START  NFCKDTF  KEY  IS  >=  NFC-F00  NFC-F12  NFC-F300
                                  NFC-F01  NFC-F03
            INVALID
*           DISPLAY "AAAA"
            MOVE  1               TO   ERR-FLG
            GO                    TO   NFCKDTF-START-EXIT
     END-START.
*
     MOVE     ZERO                TO   INV-FLG.
     PERFORM  NFCKDTF-READ-SEC.
     IF  INV-FLG = 1
*        DISPLAY "BBBB"
         MOVE     1               TO   ERR-FLG
     END-IF.
*
 NFCKDTF-START-EXIT.
     EXIT.
****************************************************************
*    ナフコ受領出荷受領統合ファイル　順ＲＥＡＤ
****************************************************************
 NFCKDTF-READ-SEC      SECTION.
*
     MOVE     "NFCKDTF-READ-SEC"  TO   S-NAME.
**
 READ000.
     READ  NFCKDTF
           AT  END    MOVE   1      TO   INV-FLG
                      GO            TO   NFCKDTF-READ-EXIT
     END-READ.
 READ000.
*取引先チェック
     IF  NFC-F00   = LINK-TOKCD
         CONTINUE
     ELSE
         MOVE   1          TO   INV-FLG
         GO                TO   NFCKDTF-READ-EXIT
     END-IF.
 READ010.
*受領区分チェック
     IF  NFC-F12   = "1"
         CONTINUE
     ELSE
         MOVE   1          TO   INV-FLG
         GO                TO   NFCKDTF-READ-EXIT
     END-IF.
 READ020.
*仕入計上日範囲チェック
     IF  NFC-F300  >=  DSP-JYUST
     AND NFC-F300  <=  DSP-JYUED
         CONTINUE
     ELSE
         MOVE   1          TO   INV-FLG
         GO                TO   NFCKDTF-READ-EXIT
     END-IF.
 READ025.
*店舗範囲チェック
     IF  NFC-F01  >=  DSP-TENCDS
     AND NFC-F01  <=  DSP-TENCDE
         CONTINUE
     ELSE
*        MOVE   1          TO   INV-FLG
         GO                TO   NFCKDTF-READ-SEC
     END-IF.
 READ040.
*伝票区分チェック
     MOVE  NFC-F05         TO   WK-DENKU.
     IF  DSP-HDDENK  =  SPACE
         CONTINUE
     ELSE
         IF   DSP-HDDENK  =  WK-DENKU
              CONTINUE
         ELSE
              GO           TO   NFCKDTF-READ-SEC
         END-IF
     END-IF.
 READ050.
*赤黒区分チェック
     MOVE  NFC-F06         TO   WK-AKAKURO.
     IF  DSP-HDAKKN  =  SPACE
         CONTINUE
     ELSE
         IF   DSP-HDAKKN  =  WK-AKAKURO
              CONTINUE
         ELSE
              GO           TO   NFCKDTF-READ-SEC
         END-IF
     END-IF.
*
 NFCKDTF-READ-EXIT.
     EXIT.
****************************************************************
*    ナフコ店舗マスタ　検索
****************************************************************
 NFTENMS-READ-SEC       SECTION.
*
     MOVE     "NFTENMS-READ-SEC"   TO   S-NAME.
*
     READ     NFTENMS
         INVALID     MOVE  "INV"  TO   NFTENMS-INV-FLG
         NOT INVALID MOVE  SPACE  TO   NFTENMS-INV-FLG
     END-READ.
*
 NFTENMS-READ-EXIT.
     EXIT.
*******************< PROGRAM-END SSY3980I >*********************
