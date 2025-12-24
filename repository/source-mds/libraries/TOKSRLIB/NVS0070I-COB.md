# NVS0070I

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/NVS0070I.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　Ｄ３６５連携　　　　　　　　　　　*
*    モジュール名　　　　：　Ｄ３６５送受信履歴照会　　　　　　*
*    作成日／作成者　　　：　2020/02/27   ASS.II               *
*    処理内容　　　　　　：　照会条件を入力して　　　　　　　　*
*    　　　　　　　　　　　　Ｄ３６５送受信履歴の照会を行う。　*
*    変更日／変更者　　　：　2022.01.11   INOUE                *
*    変更内容　　　　　　：　スクロールバグ修正　　　　　　　　*
*    　　　　　　　　　　　　対象条件変更　　　　　　　　　　　*
*    　　　　　　　　　　　　　指示日付（≦→＝）　　　　　　　*
*    　　　　　　　　　　　　　ＤＴ種別（≦→＝）　　　　　　　*
****************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           NVS0070I.
 AUTHOR.               ASS.
 DATE-WRITTEN.         20/02/27.
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
*---<<  D365送受信履歴管理ファイル  >>--*
     SELECT   SNDRCVL2       ASSIGN        TO  01-VI-SNDRCVL2
                             ORGANIZATION  IS  INDEXED
                             ACCESS MODE   IS  SEQUENTIAL
                             RECORD KEY    IS  SN2-F016
                                               SN2-F014
                                               SN2-F019
                                               SN2-F011
                             FILE STATUS   IS  SN2-ST.
     SELECT   SNDRCVL3       ASSIGN        TO  01-VI-SNDRCVL3
                             ORGANIZATION  IS  INDEXED
                             ACCESS MODE   IS  SEQUENTIAL
                             RECORD KEY    IS  SN3-F93
                                               SN3-F011
                             FILE STATUS   IS  SN3-ST.
*---<< 担当者マスタ  >>--*
     SELECT   HTANMS    ASSIGN             TO  DA-01-VI-TANMS1
                        ORGANIZATION       IS  INDEXED
                        ACCESS  MODE       IS  RANDOM
                        RECORD  KEY        IS  TAN-F01
                                               TAN-F02
                        FILE    STATUS     IS  TAN-ST.
*---<<  条件ファイル  >>---*
     SELECT   HJYOKEN        ASSIGN        TO  DA-01-VI-JYOKEN1
                             ORGANIZATION  IS  INDEXED
                             ACCESS MODE   IS  RANDOM
                             RECORD KEY    IS  JYO-F01
                                               JYO-F02
                             FILE STATUS   IS  JYO-ST.
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
                       COPY      FVS00701  OF   XMDLIB
                       JOINING   DSP       AS   PREFIX.
*
****************************************************************
*    FILE = 担当者マスタ                                       *
****************************************************************
 FD    HTANMS
        LABEL     RECORD    IS    STANDARD.
        COPY      HTANMS    OF    XFDLIB
                  JOINING   TAN   PREFIX.
****************************************************************
*    FILE = D365送受信履歴管理ファイル                         *
****************************************************************
 FD  SNDRCVL2
                        LABEL     RECORD    IS   STANDARD.
                        COPY      SNDRCVF   OF   XFDLIB
                        JOINING   SN2       AS   PREFIX.
 FD  SNDRCVL3
                        LABEL     RECORD    IS   STANDARD.
                        COPY      SNDRCVF   OF   XFDLIB
                        JOINING   SN3       AS   PREFIX.
****************************************************************
*    FILE = 条件ファイル                                     *
****************************************************************
 FD  HJYOKEN
                        LABEL     RECORD    IS   STANDARD.
                        COPY      HJYOKEN   OF   XFDLIB
                        JOINING   JYO       AS   PREFIX.
****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
*ステータス領域
 01  STATUS-AREA.
     03  DSP-ST                   PIC  X(02).
     03  SN2-ST                   PIC  X(02).
     03  SN3-ST                   PIC  X(02).
     03  TAN-ST                   PIC  X(02).
     03  JYO-ST                   PIC  X(02).
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
     03  ZSHIMS-INV-FLG           PIC  X(03)  VALUE  SPACE.
     03  HTANMS-INV-FLG           PIC  X(03)  VALUE  SPACE.
     03  HMEIMS-INV-FLG           PIC  X(03)  VALUE  SPACE.
     03  HJYOKEN-INV-FLG          PIC  X(03)  VALUE  SPACE.
     03  HTOKMS-INV-FLG           PIC  X(03)  VALUE  SPACE.
     03  BUMONF-INV-FLG           PIC  X(03)  VALUE  SPACE.
*カウント領域
 01  CNT-AREA.
     03  P-CNT                    PIC  9(07)  VALUE  ZERO.
     03  C-CNT                    PIC  9(07)  VALUE  ZERO.
     03  S-CNT                    PIC  9(07)  VALUE  ZERO.
     03  MAX-CNT                  PIC  9(07)  VALUE  ZERO.
     03  OUT-CNT                  PIC  9(07)  VALUE  ZERO.
     03  IX1                      PIC  9(02)  VALUE  ZERO.
*ワーク領域
 01  WRK-AREA.
     03  WK-YOTEIBI               PIC  9(08).
     03  WK-BDY-KEY.
         05  WK-BDY-F02           PIC  9(07).
     03  WK-SHOCD.
         05  WK-SHO               PIC  X(01)  OCCURS 8.
 01  WK-TANA.
     03  WK-TANA1                 PIC  X(01).
     03  FILLER                   PIC  X(01)  VALUE  "-".
     03  WK-TANA2                 PIC  X(03).
     03  FILLER                   PIC  X(01)  VALUE  "-".
     03  WK-TANA3                 PIC  X(02).
 01  WK-AMARI                     PIC  9(02)  VALUE 0.
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
 01  EDT-DATE.
     03  EDT-DATE-YYYY            PIC  9(04)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  "/".
     03  EDT-DATE-MM              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  "/".
     03  EDT-DATE-DD              PIC  9(02)  VALUE  ZERO.
*画面表示時刻編集
 01  HEN-TIME.
     03  HEN-TIME-HH              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  ":".
     03  HEN-TIME-MM              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  ":".
     03  HEN-TIME-SS              PIC  9(02)  VALUE  ZERO.
 01  EDT-TIME.
     03  EDT-TIME-HH              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  ":".
     03  EDT-TIME-MM              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  ":".
     03  EDT-TIME-SS              PIC  9(02)  VALUE  ZERO.
*特販部名称編集
 01  HEN-TOKHAN-AREA.
     03  FILLER                   PIC  N(01)  VALUE  NC"（".
     03  HEN-TOKHAN               PIC  N(06)  VALUE  SPACE.
     03  FILLER                   PIC  N(01)  VALUE  NC"）".
*
 01  TABLE-AREA.
     03  TBL-YOUBI                PIC   9(01).

     03  TABLE-G.
       05  TABLE1      OCCURS  6.
           07  TABLE2  OCCURS  7.
               09  TBL-RUNNO        PIC   9(07).
               09  TBL-SIJINO       PIC   9(10).
               09  TBL-DTSYB        PIC   X(02).
               09  TBL-RSKBN        PIC   X(01).
               09  TBL-JISYKBN      PIC   X(01).
               09  TBL-KENSU        PIC   9(07).
               09  TBL-JYUSINBI     PIC   9(08).
               09  TBL-JYUSINTM     PIC   9(06).
               09  TBL-JYUSINKEK    PIC   X(04).
               09  TBL-SOUSINBI     PIC   9(08).
               09  TBL-SOUSINTM     PIC   9(06).
               09  TBL-SOUSINKEK    PIC   X(04).
               09  TBL-TANTCD       PIC   X(02).
               09  TBL-TANTNM       PIC   N(05).
     03  TABLE-G-R  REDEFINES TABLE-G.
         05  TABLE2-R  OCCURS  42.
             09  TBL-RUNNO-R        PIC   9(07).
             09  TBL-SIJINO-R       PIC   9(10).
             09  TBL-DTSYB-R        PIC   X(02).
             09  TBL-RSKBN-R        PIC   X(01).
             09  TBL-JISYKBN-R      PIC   X(01).
             09  TBL-KENSU-R        PIC   9(07).
             09  TBL-JYUSINBI-R     PIC   9(08).
             09  TBL-JYUSINTM-R     PIC   9(06).
             09  TBL-JYUSINKEK-R    PIC   X(04).
             09  TBL-SOUSINBI-R     PIC   9(08).
             09  TBL-SOUSINTM-R     PIC   9(06).
             09  TBL-SOUSINKEK-R    PIC   X(04).
             09  TBL-TANTCD-R       PIC   X(02).
             09  TBL-TANTNM-R       PIC   N(05).
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
         NC"照会区分が違います。".
     03  ERR-MSG6                 PIC  N(20)  VALUE
         NC"日付範囲が違います。".
     03  ERR-MSG7                 PIC  N(20)  VALUE
         NC"ＤＴ種別が違います。".
     03  ERR-MSG8                 PIC  N(20)  VALUE
         NC"担当者が違います。".
     03  ERR-MSG9                 PIC  N(20)  VALUE
         NC"開始が終了を超えています。".
 01  ERR-MSG-AREA-R      REDEFINES     ERR-MSG-AREA.
     03  ERR-MSG-R   OCCURS  9    PIC  N(20).
*
*ファイルエラーメッセージ
 01  FILE-ERR.
     03  DSP-ERR  PIC N(15) VALUE
         NC"画面ファイルエラー".
     03  SN2-ERR  PIC N(15) VALUE
         NC"送受信履歴管理ファイルエラー".
     03  SN3-ERR  PIC N(15) VALUE
         NC"送受信履歴管理ファイルエラー".
     03  TAN-ERR  PIC N(15) VALUE
         NC"担当者マスタエラー".
     03  JYO-ERR  PIC N(15) VALUE
         NC"条件ファイルエラー".
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
*LINKAGE               SECTION.
**************************************************************
 01  LINK-BUMNCD           PIC X(04).
 01  LINK-TANTCD           PIC X(02).
**************************************************************
 PROCEDURE             DIVISION.
*PROCEDURE             DIVISION     USING LINK-BUMNCD
*                                         LINK-TANTCD.
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
 SN2-SEC                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE SNDRCVL2.
     MOVE        SN2-ST    TO        E-ST.
     MOVE        "SNDRCVL2" TO        E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     SN2-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 SN3-SEC                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE SNDRCVL3.
     MOVE        SN3-ST    TO        E-ST.
     MOVE       "SNDRCVL3" TO        E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     SN3-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 TAN-SEC                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HTANMS.
     MOVE        TAN-ST    TO        E-ST.
     MOVE        "HTANMS"  TO        E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     TAN-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 JYO-SEC                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HJYOKEN.
     MOVE        JYO-ST    TO        E-ST.
     MOVE        "JYOKEN1" TO        E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     JYO-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 END  DECLARATIVES.
****************************************************************
*             MAIN        MODULE                     0.0       *
****************************************************************
 PROCESS-START         SECTION.
     MOVE     "PROCESS-START"     TO   S-NAME.
**********************
     MOVE  "2920"  TO  LINK-BUMNCD.
     MOVE  "30"    TO  LINK-TANTCD.
*
*****DISPLAY "部門CD  =" LINK-BUMNCD
*****DISPLAY "担当者CD=" LINK-TANTCD
**********************
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
     OPEN      INPUT    HTANMS   HJYOKEN.
     OPEN      INPUT    SNDRCVL3.
     OPEN      INPUT    SNDRCVL2.
     OPEN      I-O      DSPFILE.
*ワークの初期化
     INITIALIZE                 FLG-AREA.
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
     CLOSE             DSPFILE  HTANMS   HJYOKEN.
     CLOSE             SNDRCVL2 SNDRCVL3.
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
     MOVE    SPACE                TO   DSP-FVS00701.
     MOVE    "NVS0070I"           TO   DSP-PGID.
     MOVE    "FVS00701"           TO   DSP-FORMID.
     MOVE    HEN-DATE             TO   DSP-SDATE.
     MOVE    HEN-TIME             TO   DSP-STIME.
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
*
***  照会区分入力チェック
     IF       DSP-SKBN    =   SPACE  OR  "1"
*
              MOVE  "M"      TO   EDIT-OPTION  OF  DSP-SKBN
              MOVE  SPACE    TO   EDIT-CURSOR  OF  DSP-SKBN
     ELSE
***
              MOVE  5        TO   ERR-FLG
              MOVE  "R"      TO   EDIT-OPTION  OF  DSP-SKBN
              MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-SKBN
              GO             TO   HEAD-CHK-EXIT
     END-IF.
     IF       DSP-SKBN       =     "1"
              MOVE  ZERO     TO   DSP-HDAT
              MOVE  SPACE    TO   DSP-HDTSYB
              MOVE  SPACE    TO   DSP-HTANNM
              MOVE  SPACE    TO   DSP-HTANCD
     END-IF.
***  日付範囲入力チェック
     IF       DSP-HDAT  NOT =  ZERO
***           論理チェック
              MOVE     "2"            TO   LINK-IN-KBN
              MOVE     ZERO           TO   LINK-IN-YMD6
              MOVE     DSP-HDAT       TO   LINK-IN-YMD8
              MOVE     ZERO           TO   LINK-OUT-RET
              MOVE     ZERO           TO   LINK-OUT-YMD
              CALL     "SKYDTCKB"     USING   LINK-IN-KBN
                                              LINK-IN-YMD6
                                              LINK-IN-YMD8
                                              LINK-OUT-RET
                                              LINK-OUT-YMD
              IF   LINK-OUT-RET   = 9
                   MOVE  6        TO   ERR-FLG
                   MOVE  "R"      TO   EDIT-OPTION  OF  DSP-HDAT
                   MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-HDAT
                   GO             TO   HEAD-CHK-EXIT
              END-IF
*
              MOVE  "M"      TO   EDIT-OPTION  OF  DSP-HDAT
              MOVE  SPACE    TO   EDIT-CURSOR  OF  DSP-HDAT
     END-IF.
*ＤＴ種別チェック
     IF       DSP-HDTSYB    NOT =   SPACE
              MOVE    87           TO  JYO-F01
              MOVE    DSP-HDTSYB   TO  JYO-F02
              PERFORM HJYOKEN-READ-SEC
              IF   HJYOKEN-INV-FLG  =  SPACE
                   MOVE  "M"      TO   EDIT-OPTION OF DSP-HDTSYB
                   MOVE  SPACE    TO   EDIT-CURSOR OF DSP-HDTSYB
              ELSE
                   MOVE  "R"       TO  EDIT-OPTION OF DSP-HDTSYB
                   MOVE  "C"       TO  EDIT-CURSOR OF DSP-HDTSYB
                   IF     ERR-FLG   =  ZERO
                          MOVE 7   TO  ERR-FLG
                   END-IF
              END-IF
     ELSE
              MOVE  "M"      TO   EDIT-OPTION OF DSP-HDTSYB
              MOVE  SPACE    TO   EDIT-CURSOR OF DSP-HDTSYB
     END-IF.
*担当者チェック
     MOVE     SPACE              TO   DSP-HTANNM.
     IF       DSP-HTANCD      =     SPACE
              MOVE     SPACE     TO   DSP-HTANNM
     END-IF.
     IF       DSP-HTANCD  NOT =   SPACE
              MOVE    LINK-BUMNCD  TO  TAN-F01
              MOVE    DSP-HTANCD   TO  TAN-F02
              PERFORM HTANMS-READ-SEC
              IF   HTANMS-INV-FLG  =  SPACE
                   MOVE   TAN-F03  TO  DSP-HTANNM
              ELSE
                   MOVE  "R"       TO  EDIT-OPTION OF DSP-HTANCD
                   MOVE  "C"       TO  EDIT-CURSOR OF DSP-HTANCD
                   IF     ERR-FLG   =  ZERO
                          MOVE 8   TO  ERR-FLG
                   END-IF
              END-IF
     END-IF.
*該当データ検索
     IF       ERR-FLG  =  ZERO
              MOVE    1        TO  P-CNT
              IF  DSP-SKBN     =  "1"
                 PERFORM       SNDRCVL3-START-SEC
              ELSE
                  PERFORM      SNDRCVL2-START-SEC
              END-IF
*↓2022.01.11
*             COMPUTE  MAX-CNT   =  OUT-CNT  /  7  +  1
              DIVIDE   OUT-CNT  BY  7
                       GIVING       MAX-CNT
                       REMAINDER    WK-AMARI
              IF       WK-AMARI  NOT = 0
                       ADD  1    TO MAX-CNT
              END-IF
*↑2022.01.11
*T
*             DISPLAY  "OUT-CNT="  OUT-CNT UPON CONS
*             DISPLAY  "MAX-CNT="  MAX-CNT UPON CONS
*T
     END-IF.
*ワークへデータセット
     IF       ERR-FLG  =  ZERO
              PERFORM     MST-WORK-SEC
     END-IF.
*
     IF       ERR-FLG  =  1
         IF   DSP-SKBN     =  "1"
              MOVE    "R"      TO   EDIT-OPTION OF DSP-HRUNNO
         ELSE
              MOVE    "R"      TO   EDIT-OPTION OF DSP-SKBN
              MOVE    "R"      TO   EDIT-OPTION OF DSP-HDAT
              MOVE    "R"      TO   EDIT-OPTION OF DSP-HDTSYB
              MOVE    "R"      TO   EDIT-OPTION OF DSP-HTANCD
         END-IF
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
*対象データ抽出
     MOVE      1               TO   P-CNT.
     MOVE      0               TO   READ-FLG.
     PERFORM   WORK-DSP-SEC.
 MST-WORK-EXIT.
     EXIT.
****************************************************************
*             ワーク→画面表示                                 *
****************************************************************
 WORK-DSP-SEC        SECTION.
     MOVE     "WORK-DSP-SEC"   TO   S-NAME.
*
     MOVE     SPACE              TO   DSP-MAS002(1)
                                      DSP-MAS002(2)
                                      DSP-MAS002(3)
                                      DSP-MAS002(4)
                                      DSP-MAS002(5)
                                      DSP-MAS002(6)
                                      DSP-MAS002(7).
*
     PERFORM  VARYING  IX1  FROM 1  BY  1
                 UNTIL IX1  >    7
         IF  TBL-SIJINO    (P-CNT IX1)   NOT =  ZERO
            MOVE TBL-RUNNO    (P-CNT IX1)  TO  DSP-RUNNO    (IX1)
            MOVE TBL-SIJINO   (P-CNT IX1)  TO  DSP-SIJINO   (IX1)
            MOVE    87                     TO  JYO-F01
            MOVE    TBL-DTSYB(P-CNT IX1)   TO  JYO-F02
            PERFORM HJYOKEN-READ-SEC
            IF   HJYOKEN-INV-FLG  =  SPACE
                   MOVE   JYO-F03  TO  DSP-DTSYB    (IX1)
            ELSE
                   MOVE   NC"＊＊＊＊＊"  TO  DSP-DTSYB    (IX1)
            END-IF
            IF    TBL-RSKBN    (P-CNT IX1)   =  "1"
                   MOVE NC"送信"   TO  DSP-JYUSO    (IX1)
            ELSE
                   MOVE NC"受信"   TO  DSP-JYUSO    (IX1)
            END-IF
            IF    TBL-JISYKBN  (P-CNT IX1)   =  "0"
                   MOVE NC"自動"   TO  DSP-JIDOU    (IX1)
            ELSE
                   MOVE NC"手動"   TO  DSP-JIDOU    (IX1)
            END-IF
            MOVE TBL-KENSU    (P-CNT IX1)  TO  DSP-KENSU (IX1)
*
            MOVE TBL-JYUSINBI (P-CNT IX1)(1:4) TO  EDT-DATE-YYYY
            MOVE TBL-JYUSINBI (P-CNT IX1)(5:2) TO  EDT-DATE-MM
            MOVE TBL-JYUSINBI (P-CNT IX1)(7:2) TO  EDT-DATE-DD
            MOVE EDT-DATE                      TO  DSP-RDATE (IX1)
*
            MOVE TBL-JYUSINTM (P-CNT IX1)(1:2) TO  EDT-TIME-HH
            MOVE TBL-JYUSINTM (P-CNT IX1)(3:2) TO  EDT-TIME-MM
            MOVE TBL-JYUSINTM (P-CNT IX1)(5:2) TO  EDT-TIME-SS
            MOVE EDT-TIME                      TO  DSP-RTIME (IX1)
            MOVE TBL-JYUSINKEK(P-CNT IX1)  TO  DSP-RKEKA(IX1)
*
            MOVE TBL-SOUSINBI (P-CNT IX1)(1:4) TO  EDT-DATE-YYYY
            MOVE TBL-SOUSINBI (P-CNT IX1)(5:2) TO  EDT-DATE-MM
            MOVE TBL-SOUSINBI (P-CNT IX1)(7:2) TO  EDT-DATE-DD
            MOVE EDT-DATE                      TO  DSP-SNDATE(IX1)
*
            MOVE TBL-SOUSINTM (P-CNT IX1)(1:2) TO  EDT-TIME-HH
            MOVE TBL-SOUSINTM (P-CNT IX1)(3:2) TO  EDT-TIME-MM
            MOVE TBL-SOUSINTM (P-CNT IX1)(5:2) TO  EDT-TIME-SS
            MOVE EDT-TIME                     TO  DSP-SNTIME (IX1)
            MOVE TBL-SOUSINKEK(P-CNT IX1)  TO  DSP-SKEKA(IX1)
            MOVE TBL-TANTCD   (P-CNT IX1)  TO  DSP-TANTCD (IX1)
            MOVE TBL-TANTNM   (P-CNT IX1)  TO  DSP-TANTNM (IX1)
         END-IF

     END-PERFORM.
*
 WORK-DSP-EXIT.
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
     EVALUATE  DSP-FNC
*      実行
       WHEN  "E000"
         MOVE  "3"               TO  PSW
*      取消
       WHEN  "F004"
         MOVE  1                 TO  P-CNT
         MOVE  "1"               TO  PSW
*         PERFORM  INIT-DSP-SEC
*      終了
       WHEN  "F005"
         MOVE  "END"             TO  END-FLG
*      項目戻し
       WHEN  "F006"
         MOVE  "2"               TO  PSW
*      前頁
       WHEN  "F011"
         COMPUTE  C-CNT = P-CNT - 1
         IF  C-CNT = ZERO
             MOVE  3             TO  ERR-FLG
         ELSE
             COMPUTE  P-CNT = P-CNT - 1
****             PERFORM  INIT-DSP-SEC
             PERFORM  WORK-DSP-SEC
             MOVE "3"            TO  PSW
         END-IF
*      次頁
       WHEN  "F012"
         MOVE  P-CNT             TO  S-CNT
         COMPUTE  C-CNT = P-CNT + 1
*↓2022.01.11
*        IF  C-CNT  >=  MAX-CNT
         IF ( C-CNT  >   MAX-CNT ) OR
            ( C-CNT  >   6       )
*↑2022.01.11
             MOVE  4             TO  ERR-FLG
         ELSE
                 COMPUTE  P-CNT = P-CNT + 1
                 PERFORM  WORK-DSP-SEC
                 MOVE  "3"       TO  PSW

         END-IF

       WHEN   OTHER
         MOVE  2                 TO  ERR-FLG
     END-EVALUATE.
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
         WHEN   "2"
                MOVE    PF-MSG-R(1)        TO   DSP-PFMSG
         WHEN   "3"
                MOVE    PF-MSG-R(2)        TO   DSP-PFMSG
     END-EVALUATE.
*画面の表示
     MOVE    "SCREEN"            TO   DSP-GRP.
     MOVE    "FVS00701"          TO   DSP-FMT.
     WRITE    DSP-FVS00701.
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
     MOVE    "FVS00701"           TO   DSP-FMT.
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
     MOVE   "M"        TO  EDIT-OPTION OF DSP-SKBN.
     MOVE   "M"        TO  EDIT-OPTION OF DSP-HDAT.
     MOVE   "M"        TO  EDIT-OPTION OF DSP-HDTSYB.
     MOVE   "M"        TO  EDIT-OPTION OF DSP-HTANCD.
     MOVE   "M"        TO  EDIT-OPTION OF DSP-HRUNNO.
     MOVE   SPACE      TO  EDIT-CURSOR OF DSP-SKBN.
     MOVE   SPACE      TO  EDIT-CURSOR OF DSP-HDAT.
     MOVE   SPACE      TO  EDIT-CURSOR OF DSP-HDTSYB.
     MOVE   SPACE      TO  EDIT-CURSOR OF DSP-HTANCD.
     MOVE   SPACE      TO  EDIT-CURSOR OF DSP-HRUNNO.
*
*
 DSP-SYOKI-EXIT.
     EXIT.
****************************************************************
*    送受信履歴管理ファイル スタート
****************************************************************
 SNDRCVL2-START-SEC     SECTION.
     MOVE     "SNDRCVL2-START-SEC" TO   S-NAME.
**
*送受信履歴管理ファイルスタート
     MOVE      SPACE              TO   SN2-F01.
     INITIALIZE                        SN2-F01.
     MOVE      DSP-HDAT           TO   SN2-F016.
     MOVE      DSP-HDTSYB         TO   SN2-F014.
     MOVE      DSP-HTANCD         TO   SN2-F019.
     START  SNDRCVL2  KEY  IS  >=  SN2-F016  SN2-F014
                                   SN2-F019  SN2-F011
            INVALID
            MOVE  1               TO   ERR-FLG
            GO                    TO   SNDRCVL2-START-EXIT
     END-START.
*
     MOVE     ZERO                TO   INV-FLG.
     INITIALIZE                        TABLE-G.
     PERFORM  SNDRCVL2-READ-SEC.
     IF     INV-FLG            =  1
            MOVE  1               TO   ERR-FLG
            GO                    TO   SNDRCVL2-START-EXIT
     END-IF.
     PERFORM  VARYING  IX1  FROM  1  BY  1
              UNTIL    IX1  >     42
                 OR    INV-FLG  = 1
*
            MOVE  SN2-F93              TO TBL-RUNNO-R    (IX1)
            MOVE  SN2-F011             TO TBL-SIJINO-R   (IX1)
            MOVE  SN2-F014             TO TBL-DTSYB-R    (IX1)
            MOVE  SN2-F012             TO TBL-RSKBN-R    (IX1)
            MOVE  SN2-F013             TO TBL-JISYKBN-R  (IX1)
            MOVE  SN2-F015             TO TBL-KENSU-R    (IX1)
            MOVE  SN2-F01A             TO TBL-JYUSINBI-R (IX1)
            MOVE  SN2-F01B             TO TBL-JYUSINTM-R (IX1)
            MOVE  SN2-F01C             TO TBL-JYUSINKEK-R(IX1)
            MOVE  SN2-F016             TO TBL-SOUSINBI-R (IX1)
            MOVE  SN2-F017             TO TBL-SOUSINTM-R (IX1)
            MOVE  SN2-F018             TO TBL-SOUSINKEK-R(IX1)
            MOVE  SN2-F94              TO TBL-TANTCD-R   (IX1)

            MOVE  LINK-BUMNCD          TO TAN-F01
            MOVE  SN2-F94              TO TAN-F02
            PERFORM HTANMS-READ-SEC
            IF  HTANMS-INV-FLG   =  SPACE
                MOVE  TAN-F03          TO TBL-TANTNM-R   (IX1)
            END-IF
*
            PERFORM  SNDRCVL2-READ-SEC
     END-PERFORM.
*↓2021.01.11
*    MOVE   IX1                        TO  OUT-CNT.
     COMPUTE   OUT-CNT  = IX1  -  1.
*↑2021.01.11
*T
*    DISPLAY "OUT-CNT =" OUT-CNT  UPON CONS.
*T
*
 SNDRCVL2-START-EXIT.
     EXIT.
****************************************************************
*    送受信履歴管理ファイル読込
****************************************************************
 SNDRCVL2-READ-SEC      SECTION.
*
     MOVE     "SNDRCVL2-READ-SEC"  TO   S-NAME.
**
     READ  SNDRCVL2
           AT  END    MOVE   1      TO   INV-FLG
                      GO            TO   SNDRCVL2-READ-EXIT
     END-READ.
*
*↓2022.01.11
*    IF     DSP-HDAT     <=  SN2-F016
*    AND    DSP-HDTSYB   <=  SN2-F014
*    AND    DSP-HTANCD   <=  SN2-F019
*        CONTINUE
*    ELSE
*        MOVE   1          TO   INV-FLG
*        GO                TO   SNDRCVL2-READ-EXIT
*    END-IF.
*
     IF   ( DSP-HDAT      =  ZERO     ) OR
          ( DSP-HDAT    NOT  NUMERIC  )
            CONTINUE
     ELSE
            IF     DSP-HDAT     =  SN2-F016
                   CONTINUE
            ELSE
                   MOVE   1          TO   INV-FLG
                   GO    TO  SNDRCVL2-READ-EXIT
            END-IF
     END-IF.
     IF     DSP-HDTSYB    =  SPACE
            CONTINUE
     ELSE
            IF     DSP-HDTSYB   =  SN2-F014
                   CONTINUE
            ELSE
*                  MOVE   1          TO   INV-FLG
*                  GO    TO  SNDRCVL2-READ-EXIT
                   GO    TO  SNDRCVL2-READ-SEC
            END-IF
     END-IF.
     IF     DSP-HTANCD   <=  SN2-F019
            CONTINUE
     ELSE
*           MOVE   1          TO   INV-FLG
*           GO    TO  SNDRCVL2-READ-EXIT
            GO    TO  SNDRCVL2-READ-SEC
     END-IF.
*↑2022.01.11
*T
*    DISPLAY "IX1     =" IX1      UPON CONS.
*    DISPLAY "SN2-F016=" SN2-F016 UPON CONS.
*    DISPLAY "SN2-F014=" SN2-F014 UPON CONS.
*    DISPLAY "SN2-F019=" SN2-F019 UPON CONS.
*T
*
 SNDRCVL2-READ-EXIT.
     EXIT.
****************************************************************
*    送受信履歴管理ファイル スタート
****************************************************************
 SNDRCVL3-START-SEC     SECTION.
     MOVE     "SNDRCVL3-START-SEC" TO   S-NAME.
**
*送受信履歴管理ファイルスタート
     MOVE      SPACE              TO   SN3-F01.
     INITIALIZE                        SN3-F01.
     MOVE      DSP-HRUNNO         TO   SN3-F93.
*    MOVE      DSP-HDTSYB         TO   SN2-F011.
     START  SNDRCVL3  KEY  IS  >=  SN3-F93  SN3-F011
            INVALID
            MOVE  1               TO   ERR-FLG
            GO                    TO   SNDRCVL3-START-EXIT
     END-START.
*
     MOVE     ZERO                TO   INV-FLG.
     INITIALIZE                        TABLE-G.
     PERFORM  SNDRCVL3-READ-SEC.
     IF     INV-FLG            =  1
            MOVE  1               TO   ERR-FLG
            GO                    TO   SNDRCVL3-START-EXIT
     END-IF.
     PERFORM  VARYING  IX1  FROM  1  BY  1
              UNTIL    IX1  >     42
                 OR    INV-FLG  = 1
*
            MOVE  SN3-F93              TO TBL-RUNNO-R    (IX1)
            MOVE  SN3-F011             TO TBL-SIJINO-R   (IX1)
            MOVE  SN3-F014             TO TBL-DTSYB-R    (IX1)
            MOVE  SN3-F012             TO TBL-RSKBN-R    (IX1)
            MOVE  SN3-F013             TO TBL-JISYKBN-R  (IX1)
            MOVE  SN3-F015             TO TBL-KENSU-R    (IX1)
            MOVE  SN3-F01A             TO TBL-JYUSINBI-R (IX1)
            MOVE  SN3-F01B             TO TBL-JYUSINTM-R (IX1)
            MOVE  SN3-F01C             TO TBL-JYUSINKEK-R(IX1)
            MOVE  SN3-F016             TO TBL-SOUSINBI-R (IX1)
            MOVE  SN3-F017             TO TBL-SOUSINTM-R (IX1)
            MOVE  SN3-F018             TO TBL-SOUSINKEK-R(IX1)
            MOVE  SN3-F94              TO TBL-TANTCD-R   (IX1)
            MOVE  LINK-BUMNCD          TO TAN-F01
            MOVE  SN3-F94              TO TAN-F02
            PERFORM HTANMS-READ-SEC
            IF  HTANMS-INV-FLG   =  SPACE
                MOVE  TAN-F03          TO TBL-TANTNM-R   (IX1)
            END-IF
*
            PERFORM  SNDRCVL3-READ-SEC
     END-PERFORM.
     MOVE   IX1                        TO  OUT-CNT.

*
 SNDRCVL3-START-EXIT.
     EXIT.
****************************************************************
*    送受信履歴管理ファイル読込
****************************************************************
 SNDRCVL3-READ-SEC      SECTION.
*
     MOVE     "SNDRCVL3-READ-SEC"  TO   S-NAME.
**
     READ  SNDRCVL3
           AT  END    MOVE   1      TO   INV-FLG
                      GO            TO   SNDRCVL3-READ-EXIT
     END-READ.
*
     IF     DSP-HRUNNO     <=  SN3-F93
         CONTINUE
     ELSE
         MOVE   1          TO   INV-FLG
         GO                TO   SNDRCVL3-READ-EXIT
     END-IF.
*
 SNDRCVL3-READ-EXIT.
     EXIT.
****************************************************************
*    担当者マスタ読込
****************************************************************
 HTANMS-READ-SEC       SECTION.
*
     MOVE     "HTANMS-READ-SEC"   TO   S-NAME.
*
     READ     HTANMS
         INVALID     MOVE  "INV"  TO   HTANMS-INV-FLG
         NOT INVALID MOVE  SPACE  TO   HTANMS-INV-FLG
     END-READ.
*
 HTANMS-READ-EXIT.
     EXIT.
****************************************************************
*    条件ファイル読込
****************************************************************
 HJYOKEN-READ-SEC       SECTION.
*
     MOVE     "HJYOKEN-READ-SEC"  TO   S-NAME.
*
     READ     HJYOKEN
         INVALID     MOVE  "INV"  TO   HJYOKEN-INV-FLG
         NOT INVALID MOVE  SPACE  TO   HJYOKEN-INV-FLG
     END-READ.
*
 HJYOKEN-READ-EXIT.
     EXIT.
*******************< PROGRAM-END NVS0070I >*********************

```
