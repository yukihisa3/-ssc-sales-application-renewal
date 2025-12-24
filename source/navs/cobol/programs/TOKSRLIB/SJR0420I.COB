****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　受領返品取込機能構築　　　　　　　*
*    業務名　　　　　　　：　受信ＤＴ　伝票区分変換　　        *
*    モジュール名　　　　：　伝票区分変換マスタ保守　　　      *
*    作成日／更新日　　　：　2017/08/07                        *
*    作成者／更新者　　　：　ＮＡＶ高橋                        *
*    処理概要　　　　　　：　伝票区分変換マスタの登録修正削除を*
*                            行なう。　　                      *
*    更新日／更新者　　　：　                                  *
*    修正概要　　　　　　：                                    *
****************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SJR0420I.
 AUTHOR.               T.T.
 DATE-WRITTEN.         2017/08/07.
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
*伝票区分変換マスタ
     SELECT  DENHENF   ASSIGN    TO        DA-01-VI-DENHENL1
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      DYNAMIC
                       RECORD    KEY       DEH-F01
                                           DEH-F02
                       FILE      STATUS    DEH-ST.
*条件ファイル
     SELECT  HJYOKEN   ASSIGN    TO        DA-01-VI-JYOKEN1
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      RANDOM
                       RECORD    KEY       JYO-F01
                                           JYO-F02
                       FILE      STATUS    JYO-ST.
*取引先マスタ
     SELECT  HTOKMS    ASSIGN    TO        DA-01-VI-TOKMS2
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      RANDOM
                       RECORD    KEY       TOK-F01
                       FILE      STATUS    TOK-ST.
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
*    FILE = 伝票区分変換マスタ　　　                           *
****************************************************************
 FD  DENHENF
                       LABEL     RECORD    IS   STANDARD.
                       COPY      DENHENF   OF   XFDLIB
                       JOINING   DEH       AS   PREFIX.
****************************************************************
*    FILE = 条件ファイル　　　                                 *
****************************************************************
 FD  HJYOKEN
                       LABEL     RECORD    IS   STANDARD.
                       COPY      HJYOKEN   OF   XFDLIB
                       JOINING   JYO       AS   PREFIX.
****************************************************************
*    FILE = 取引先マスタ                                       *
****************************************************************
 FD  HTOKMS
                       BLOCK     CONTAINS  8    RECORDS
                       LABEL     RECORD    IS   STANDARD.
                       COPY      HTOKMS    OF   XFDLIB
                       JOINING   TOK       AS   PREFIX.
****************************************************************
*    FILE = 画面ファイル                                       *
****************************************************************
 FD  DSPFILE
                       LABEL     RECORD    IS   OMITTED.
                       COPY      FJR04201  OF   XMDLIB
                       JOINING   DSP       AS   PREFIX.
*
****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
*ステータス領域
 01  STATUS-AREA.
     03  DEH-ST                   PIC  X(02).
     03  JYO-ST                   PIC  X(02).
     03  TOK-ST                   PIC  X(02).
     03  DSP-ST                   PIC  X(02).
*画面制御用領域
 01  DSP-CONTROL.
     03  DSP-FMT                  PIC  X(08).
     03  DSP-GRP                  PIC  X(08).
     03  DSP-PRO                  PIC  X(02).
     03  DSP-FNC                  PIC  X(04).
*フラグ領域
 01  FLG-AREA.
     03  DENHENF-INV-FLG          PIC  X(03)  VALUE  SPACE.
     03  HTOKMS-INV-FLG           PIC  X(03)  VALUE  SPACE.
     03  HJYOKEN-INV-FLG          PIC  X(03)  VALUE  SPACE.
     03  CHK-FLG                  PIC  X(03)  VALUE  SPACE.
     03  ERR-FLG                  PIC  9(02)  VALUE  ZERO.
     03  END-FLG                  PIC  X(03)  VALUE  SPACE.
     03  RD-FLG                   PIC  X(03)  VALUE  SPACE.
     03  GYO1-CNT                 PIC  9(02)  VALUE  ZERO.
     03  CHK-CNT                  PIC  9(01)  VALUE  ZERO.
     03  CHK-MEI                  PIC  9(02)  VALUE  ZERO.
     03  CHK-MEI2                 PIC  9(02)  VALUE  ZERO.
     03  P-CNT                    PIC  9(01)  VALUE  ZERO.
     03  S-CNT                    PIC  9(01)  VALUE  ZERO.
     03  C-CNT                    PIC  9(01)  VALUE  ZERO.
     03  X                        PIC  9(02)  VALUE  ZERO.
     03  Y                        PIC  9(02)  VALUE  ZERO.
     03  FG-HANUKE                PIC  9(01)  VALUE  ZERO.
     03  FG-DUPLICATE             PIC  9(01)  VALUE  ZERO.
     03  CT-TBLMAX                PIC  9(03)  VALUE  ZERO.
     03  WK-IX                    PIC  9(03)  VALUE  ZERO.
     03  WK-IX2                   PIC  9(03)  VALUE  ZERO.
     03  WK-IX3                   PIC  9(03)  VALUE  ZERO.
     03  IX-JIGMN-PG              PIC  9(03)  VALUE  ZERO.
     03  IX-JIGMN-GYO1            PIC  9(03)  VALUE  ZERO.
     03  CT-TBLPG-MAX             PIC  9(02)  VALUE  ZERO.
     03  WK-AMARI                 PIC  9(02)  VALUE  ZERO.
*ワーク領域
 01  WRK-AREA.
***  プログラムスイッチ（画面遷移制御）
     03  PSW                      PIC  X(01)  VALUE  SPACE.
*
 01  WRK-YOBI                     PIC  9(01)  VALUE  ZERO.
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
         05  FILLER               PIC  N(20)  VALUE
             NC"_取消　_終了　　　　　　　　　　　　　".
         05  FILLER               PIC  N(10)  VALUE
             NC"　　　　　　　　　　".
     03  PF-MSG2.
         05  FILLER               PIC  N(20)  VALUE
             NC"_取消　_終了　_項目戻し　　　　　　　".
         05  FILLER               PIC  N(10)  VALUE
             NC"　　　　　　　　　　".
     03  PF-MSG3.
         05  FILLER               PIC  N(20)  VALUE
             NC"_取消　_終了　_項目戻し　_前頁　_次".
         05  FILLER               PIC  N(10)  VALUE
             NC"頁　　　　　　　　　".
 01  PF-MSG-AREA-R  REDEFINES PF-MSG-AREA.
     03  PF-MSG-R  OCCURS 3       PIC  N(30).
*
*メッセージの取得
 01  ERR-MSG-AREA.
     03  ERR-MSG1.
         05  FILLER              PIC   N(20)
             VALUE NC"取引先ＣＤ未入力です。".
     03  ERR-MSG2.
         05  FILLER              PIC   N(20)
             VALUE NC"取引先マスタ未登録です。".
     03  ERR-MSG3.
         05  FILLER              PIC   N(20)
             VALUE NC"前頁がありません。".
     03  ERR-MSG4.
         05  FILLER              PIC   N(20)
             VALUE NC"次頁がありません。".
     03  ERR-MSG5.
         05  FILLER              PIC   N(20)
             VALUE NC"無効キーです。".
     03  ERR-MSG6.
         05  FILLER              PIC   N(20)
             VALUE NC"必須入力項目です。".
     03  ERR-MSG7.
         05  FILLER              PIC   N(20)
             VALUE NC"入力伝区が条件Ｆ未登録です。".
     03  ERR-MSG8.
         05  FILLER              PIC   N(20)
             VALUE NC"受領取込区分は１又は２を指定して下さい。".
     03  ERR-MSG9.
         05  FILLER              PIC   N(20)
             VALUE NC"返品取込区分は１又は２を指定して下さい。".
     03  ERR-MSG10.
         05  FILLER              PIC   N(20)
             VALUE NC"相手伝票区分が重複しております。".
     03  ERR-MSG11.
         05  FILLER              PIC   N(20)
             VALUE NC"他頁の相手伝区が重複しております。".
     03  ERR-MSG12.
         05  FILLER              PIC   N(20)
             VALUE NC"明細行を入力して下さい。".
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
             VALUE NC"　　　　　　　　　　　　　　　　　　　　".
     03  ERR-MSG17.
         05  FILLER              PIC   N(20)
             VALUE NC"　　　　　　　　　　　　　　　　　　　　".
     03  ERR-MSG18.
         05  FILLER              PIC   N(20)
             VALUE NC"　　　　　　　　　　　　　　　　　　　　".
     03  ERR-MSG19.
         05  FILLER              PIC   N(20)
             VALUE NC"　　　　　　　　　　　　　　　　　　　　".
     03  ERR-MSG20.
         05  FILLER              PIC   N(20)
             VALUE NC"　　　　　　　　　　　　　　　　　　　　".
 01  ERR-MSG-AREA-R      REDEFINES     ERR-MSG-AREA.
     03  ERR-MSG-R   OCCURS  20   PIC   N(20).
*伝票区分変換マスタ退避エリア
 01  TABLE-AREA.
     03  TBL-TOKCD                PIC   9(08).

     03  TABLE-G.
       05  TABLE1      OCCURS  2.
           07  TABLE2  OCCURS  12.
               09  TBL-GYO1         PIC   9(02).
               09  TBL-TRDENK       PIC   X(10).
               09  TBL-TRDENN       PIC   N(10).
               09  TBL-SKDENK       PIC   X(02).
               09  TBL-JYRKBN       PIC   X(01).
               09  TBL-HNPKBN       PIC   X(01).
               09  TBL-TORBUMON     PIC   X(04).
               09  TBL-TORTANCD     PIC   X(02).
               09  TBL-TORDATE      PIC   9(08).
               09  TBL-TORTIME      PIC   9(06).
               09  TBL-KOSBUMON     PIC   X(04).
               09  TBL-KOSTANCD     PIC   X(02).
               09  TBL-KOSDATE      PIC   9(08).
               09  TBL-KOSTIME      PIC   9(06).
     03  TABLE-G-R  REDEFINES TABLE-G.
         05  TABLE2-R  OCCURS  24.
           07  TBL-GYO1-R           PIC   9(02).
           07  TBL-TRDENK-R         PIC   X(10).
           07  TBL-TRDENN-R         PIC   N(10).
           07  TBL-SKDENK-R         PIC   X(02).
           07  TBL-JYRKBN-R         PIC   X(01).
           07  TBL-HNPKBN-R         PIC   X(01).
           07  TBL-TORBUMON-R       PIC   X(04).
           07  TBL-TORTANCD-R       PIC   X(02).
           07  TBL-TORDATE-R        PIC   9(08).
           07  TBL-TORTIME-R        PIC   9(06).
           07  TBL-KOSBUMON-R       PIC   X(04).
           07  TBL-KOSTANCD-R       PIC   X(02).
           07  TBL-KOSDATE-R        PIC   9(08).
           07  TBL-KOSTIME-R        PIC   9(06).

     03  TABLE-G-INI.
       05  TABLE1-INI      OCCURS  2.
           07  TABLE2-INI  OCCURS  12.
               09  TBL-GYO1-INI          PIC   9(02).
               09  TBL-TRDENK-INI       PIC   X(10).
               09  TBL-TRDENN-INI       PIC   N(10).
               09  TBL-SKDENK-INI       PIC   X(02).
               09  TBL-JYRKBN-INI       PIC   X(01).
               09  TBL-HNPKBN-INI       PIC   X(01).
               09  TBL-TORBUMON-INI     PIC   X(04).
               09  TBL-TORTANCD-INI     PIC   X(02).
               09  TBL-TORDATE-INI      PIC   9(08).
               09  TBL-TORTIME-INI      PIC   9(06).
               09  TBL-KOSBUMON-INI     PIC   X(04).
               09  TBL-KOSTANCD-INI     PIC   X(02).
               09  TBL-KOSDATE-INI      PIC   9(08).
               09  TBL-KOSTIME-INI      PIC   9(06).
     03  TABLE-G-INI-R  REDEFINES TABLE-G-INI.
         05  TABLE2-INI-R  OCCURS  24.
           07  TBL-GYO1-INI-R        PIC   9(02).
           07  TBL-TRDENK-INI-R      PIC   X(10).
           07  TBL-TRDENN-INI-R      PIC   N(10).
           07  TBL-SKDENK-INI-R      PIC   X(02).
           07  TBL-JYRKBN-INI-R      PIC   X(01).
           07  TBL-HNPKBN-INI-R      PIC   X(01).
           07  TBL-TORBUMON-INI-R    PIC   X(04).
           07  TBL-TORTANCD-INI-R    PIC   X(02).
           07  TBL-TORDATE-INI-R     PIC   9(08).
           07  TBL-TORTIME-INI-R     PIC   9(06).
           07  TBL-KOSBUMON-INI-R    PIC   X(04).
           07  TBL-KOSTANCD-INI-R    PIC   X(02).
           07  TBL-KOSDATE-INI-R     PIC   9(08).
           07  TBL-KOSTIME-INI-R     PIC   9(06).

     03  TABLE-G-BK.
       05  TABLE1-BK      OCCURS  2.
           07  TABLE2-BK  OCCURS  12.
               09  TBL-GYO1-BK          PIC   9(02).
               09  TBL-TRDENK-BK        PIC   X(10).
               09  TBL-TRDENN-BK        PIC   N(10).
               09  TBL-SKDENK-BK        PIC   X(02).
               09  TBL-JYRKBN-BK        PIC   X(01).
               09  TBL-HNPKBN-BK        PIC   X(01).
               09  TBL-TORBUMON-BK      PIC   X(04).
               09  TBL-TORTANCD-BK      PIC   X(02).
               09  TBL-TORDATE-BK       PIC   9(08).
               09  TBL-TORTIME-BK       PIC   9(06).
               09  TBL-KOSBUMON-BK      PIC   X(04).
               09  TBL-KOSTANCD-BK      PIC   X(02).
               09  TBL-KOSDATE-BK       PIC   9(08).
               09  TBL-KOSTIME-BK       PIC   9(06).
     03  TABLE-G-BK-R  REDEFINES TABLE-G-BK.
         05  TABLE2-BK-R  OCCURS  24.
           07  TBL-GYO1-BK-R        PIC   9(02).
           07  TBL-TRDENK-BK-R      PIC   X(10).
           07  TBL-TRDENN-BK-R      PIC   N(10).
           07  TBL-SKDENK-BK-R      PIC   X(02).
           07  TBL-JYRKBN-BK-R      PIC   X(01).
           07  TBL-HNPKBN-BK-R      PIC   X(01).
           07  TBL-TORBUMON-BK-R    PIC   X(04).
           07  TBL-TORTANCD-BK-R    PIC   X(02).
           07  TBL-TORDATE-BK-R     PIC   9(08).
           07  TBL-TORTIME-BK-R     PIC   9(06).
           07  TBL-KOSBUMON-BK-R    PIC   X(04).
           07  TBL-KOSTANCD-BK-R    PIC   X(02).
           07  TBL-KOSDA-BK-R       PIC   9(08).
           07  TBL-KOSTIME-BK-R     PIC   9(06).

*
*ファイルエラーメッセージ
 01  FILE-ERR.
     03  DEH-ERR           PIC N(15) VALUE
                        NC"伝票区分変換マスタエラー".
     03  JYO-ERR           PIC N(15) VALUE
                        NC"条件ファイルエラー".
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
 01  PAR-BUMON             PIC  X(04).
 01  PAR-TANCD             PIC  X(02).
*
**************************************************************
 PROCEDURE             DIVISION   USING  PAR-BUMON PAR-TANCD.
**************************************************************
 DECLARATIVES.
 DEH-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE DENHENF.
     MOVE        DEH-ST    TO        E-ST.
     MOVE        "DENHENF" TO        E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     DEH-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 JYO-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HJYOKEN.
     MOVE        JYO-ST    TO        E-ST.
     MOVE        "JYOKEN1" TO        E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     JYO-ERR   UPON      CONS.
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
     MOVE  "PROCESS-START"       TO  S-NAME.

     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC  UNTIL  END-FLG = "END".
     PERFORM  END-SEC.

     STOP    RUN.
 CONTROL-EXIT.
     EXIT.
****************************************************************
*             初期処理                               1.0
****************************************************************
 INIT-SEC              SECTION.
     MOVE  "INIT-SEC"            TO  S-NAME.
*ファイルのＯＰＥＮ
     OPEN  I-O    DENHENF.
     OPEN  I-O    DSPFILE.
     OPEN  INPUT  HJYOKEN HTOKMS.
*ワークの初期化
     INITIALIZE  FLG-AREA.
*初期画面の表示
     MOVE  SPACE                 TO  DSP-PRO.
     PERFORM  INIT-DSP-SEC.
*ヘッド入力へ
     MOVE  "1"                   TO  PSW.
*
 INIT-EXIT.
     EXIT.

****************************************************************
*             初期画面表示                                     *
****************************************************************
 INIT-DSP-SEC          SECTION.
     MOVE  "INIT-DSP-SEC"        TO  S-NAME.
     PERFORM  SDATE-GET-SEC.
*画面の初期化
     MOVE  SPACE                 TO  DSP-FJR04201.
*システム日付転送
     MOVE  SYS-DATE              TO  DSP-SDATE.
*システム時間転送
     MOVE  WK-TIME(1:6)          TO  DSP-STIME.
*プログラムＩＤ
     MOVE  "SJR0420I"            TO  DSP-PGID.
*ＦＯＲＭＩＤ
     MOVE  "FJR04201"            TO  DSP-FORMID.
*項目属性クリア
     PERFORM  DSP-SYOKI-SEC.
*
 INIT-DSP-EXIT.
     EXIT.

****************************************************************
*    システム日付取得処理                                      *
****************************************************************
 SDATE-GET-SEC          SECTION.
*システム日付・時刻の取得
     ACCEPT  WK-DATE  FROM DATE.
     MOVE  "3"                   TO  LINK-IN-KBN.
     MOVE  WK-DATE               TO  LINK-IN-YMD6.
     MOVE  ZERO                  TO  LINK-IN-YMD8.
     MOVE  ZERO                  TO  LINK-OUT-RET.
     MOVE  ZERO                  TO  LINK-OUT-YMD.
     CALL  "SKYDTCKB"  USING LINK-IN-KBN
                             LINK-IN-YMD6
                             LINK-IN-YMD8
                             LINK-OUT-RET
                             LINK-OUT-YMD.
     MOVE  LINK-OUT-YMD          TO  DATE-AREA.
     ACCEPT  WK-TIME  FROM TIME.
*
 SDATE-GET-EXIT.
     EXIT.

****************************************************************
*             画面制御項目初期化                               *
****************************************************************
 DSP-SYOKI-SEC         SECTION.
     MOVE  "DSP-SYOKI-SEC"       TO  S-NAME.
*リバース，カーソルパーク解除
***  取引先ＣＤ
     MOVE  "M"              TO  EDIT-OPTION OF DSP-TOKCD.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-TOKCD.
     PERFORM  VARYING X  FROM 1 BY 1
              UNTIL   X > 12
***  取引先伝票区分
       MOVE  "M"            TO  EDIT-OPTION OF DSP-TRDENK(X)
       MOVE  SPACE          TO  EDIT-CURSOR OF DSP-TRDENK(X)
***  取引先伝票区分名
       MOVE  "M"            TO  EDIT-OPTION OF DSP-TRDENN(X)
       MOVE  SPACE          TO  EDIT-CURSOR OF DSP-TRDENN(X)
***  サカタ伝票区分　
       MOVE  "M"            TO  EDIT-OPTION OF DSP-SKDENK(X)
       MOVE  SPACE          TO  EDIT-CURSOR OF DSP-SKDENN(X)
***  受領取込区分
       MOVE  "M"            TO  EDIT-OPTION OF DSP-JYRKBN(X)
       MOVE  SPACE          TO  EDIT-CURSOR OF DSP-JYRKBN(X)
***  返品取込区分
       MOVE  "M"            TO  EDIT-OPTION OF DSP-HNPKBN(X)
       MOVE  SPACE          TO  EDIT-CURSOR OF DSP-HNPKBN(X)
     END-PERFORM.
*
 DSP-SYOKI-EXIT.
     EXIT.
****************************************************************
*             メイン処理                             2.0
****************************************************************
 MAIN-SEC              SECTION.
     MOVE  "MAIN-SEC"            TO  S-NAME.
*
     EVALUATE  PSW
*      処理区分入力(2.1)
       WHEN  "1"  PERFORM  DSP-HEAD-SEC
*      明細入力    (2.2)
       WHEN  "2"  PERFORM  DSP-BODY-SEC
*      確認入力    (2.3)
       WHEN  "3"  PERFORM  DSP-KAKU-SEC
*      以外
       WHEN  OTHER  CONTINUE
     END-EVALUATE.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*             処理区分入力( PSW = 1 )                          *
****************************************************************
 DSP-HEAD-SEC          SECTION.
     MOVE  "DSP-HEAD-SEC"        TO  S-NAME.

     PERFORM  DSP-WRITE-SEC.
     PERFORM  DSP-READ-SEC.

     EVALUATE  DSP-FNC
*      実行
       WHEN  "E000"
         PERFORM  HEAD-CHK-SEC
*      取消
       WHEN  "F004"
         MOVE  "1"               TO  PSW
         PERFORM   INIT-DSP-SEC
*      終了
       WHEN  "F005"
         MOVE  "END"             TO  END-FLG
       WHEN  OTHER
         MOVE  5                 TO  ERR-FLG
     END-EVALUATE.
*
 DSP-HEAD-EXIT.
     EXIT.
****************************************************************
*             画面表示処理                                     *
****************************************************************
 DSP-WRITE-SEC         SECTION.
     MOVE  "DSP-WRITE-SEC"       TO  S-NAME.

     PERFORM  SDATE-GET-SEC.
*システム日付転送
     MOVE  SYS-DATE              TO  DSP-SDATE.
*システム時間転送
     MOVE  WK-TIME(1:6)          TO  DSP-STIME.

*エラーメッセージセット
     IF  ERR-FLG = ZERO
         MOVE  SPACE               TO  DSP-MSGSPC
     ELSE
         MOVE  ERR-MSG-R(ERR-FLG)  TO  DSP-MSGSPC
         MOVE  ZERO                TO  ERR-FLG
     END-IF.
*ガイドメッセージの設定
     EVALUATE  PSW
*      曜日
       WHEN  "1"
         MOVE  PF-MSG-R(1)       TO  DSP-FNCSPC
*      明細
       WHEN  "2"
         MOVE  PF-MSG-R(2)       TO  DSP-FNCSPC
*      確認
       WHEN  "3"
         MOVE  PF-MSG-R(3)       TO  DSP-FNCSPC
     END-EVALUATE.
* 画面の表示
     MOVE  "SCREEN"              TO  DSP-GRP.
     MOVE  "FJR04201"            TO  DSP-FMT.
     WRITE  DSP-FJR04201.
     PERFORM  DSP-SYOKI-SEC.
*
 DSP-WRITE-EXIT.
     EXIT.
****************************************************************
*             画面読込処理                                     *
****************************************************************
 DSP-READ-SEC          SECTION.
     MOVE  "DSP-READ-SEC"        TO  S-NAME.
*
     MOVE  "NE"                  TO  DSP-PRO.
*
*    MOVE  "SCREEN"              TO  DSP-GRP.
     EVALUATE   PSW
*      曜日
       WHEN  "1"
         MOVE  "GPHEAD"          TO  DSP-GRP
*      明細
       WHEN  "2"
         MOVE  "GPBODY"          TO  DSP-GRP
*      確認
       WHEN  "3"
         MOVE  "GPKAKU"          TO  DSP-GRP
     END-EVALUATE.

     MOVE  "FJR04201"            TO  DSP-FMT.
     READ  DSPFILE.
*入力項目の属性を通常にする
 DSP-READ-010.
*    MOVE  ZERO                  TO  ERR-FLG.
     MOVE  SPACE                 TO  DSP-PRO.
*
 DSP-READ-EXIT.
     EXIT.
****************************************************************
*             取引先ＣＤチェック                               *
****************************************************************
 HEAD-CHK-SEC             SECTION.
     MOVE  "HEAD-CHK-SEC"        TO  S-NAME.
*取引先ＣＤチェック
     IF  DSP-TOKCD NOT NUMERIC
     OR  DSP-TOKCD = ZERO
         MOVE  1                 TO  ERR-FLG
         MOVE  "R"               TO  EDIT-OPTION  OF  DSP-TOKCD
         MOVE  "C"               TO  EDIT-CURSOR  OF  DSP-TOKCD
     ELSE
         MOVE   DSP-TOKCD        TO  TOK-F01
         PERFORM HTOKMS-READ-SEC
         IF  HTOKMS-INV-FLG  =  "INV"
             MOVE  2             TO  ERR-FLG
             MOVE  "R"           TO  EDIT-OPTION  OF  DSP-TOKCD
             MOVE  "C"           TO  EDIT-CURSOR  OF  DSP-TOKCD
             MOVE  ALL NC"＊"    TO  DSP-TOKNM
         ELSE
             MOVE  "M"           TO  EDIT-OPTION  OF  DSP-TOKCD
             MOVE  SPACE         TO  EDIT-CURSOR  OF  DSP-TOKCD
             MOVE  TOK-F02       TO  DSP-TOKNM
         END-IF
     END-IF.
*エラーフラグがゼロの場合
     IF  ERR-FLG = ZERO
         PERFORM  MST-WORK-SEC   *> MST -> TBL
         MOVE  1                 TO  P-CNT
         PERFORM  WORK-DSP-SEC   *> TBL -> DSP
         MOVE  "2"               TO  PSW
     END-IF.
*
 HEAD-CHK-EXIT.
     EXIT.
****************************************************************
*             マスタ→ワーク（退避）                           *
****************************************************************
 MST-WORK-SEC          SECTION.
     MOVE  "MST-WORK-SEC"        TO  S-NAME.
*ワークテーブルクリア
     MOVE  SPACE                 TO  TABLE-AREA.
     INITIALIZE  TABLE-AREA.

*曜日のワーク退避
     MOVE  DSP-TOKCD             TO  TBL-TOKCD.
*ＳＥＱの作成
     MOVE  ZERO                  TO  GYO1-CNT.
     PERFORM  VARYING X  FROM 1 BY 1
              UNTIL   X > 2
       PERFORM  VARYING Y FROM  1 BY 1
                UNTIL Y > 12
         ADD 1  TO  GYO1-CNT
         MOVE  GYO1-CNT           TO  TBL-GYO1   (X Y)
*********DISPLAY "TBL-GYO1 = " TBL-GYO1(X Y) UPON CONS
       END-PERFORM
     END-PERFORM.
*取引先ＣＤにより伝票区分変換マスタスタート
     PERFORM DENHENF-START-SEC.
     IF  CHK-FLG = "CHK"
         GO TO  MST-WORK-010
     END-IF.
*
*伝票区分変換マスタ読込み
     PERFORM  DENHENF-READ-SEC.
*マスタを読込みながらワークへセット
     MOVE  ZERO                  TO  CT-TBLMAX.
     PERFORM  VARYING X  FROM 1 BY 1
              UNTIL   X > 2
                   OR DSP-TOKCD NOT = DEH-F01
                   OR RD-FLG        = "END"
       PERFORM  VARYING Y  FROM 1 BY 1
                UNTIL   Y > 12
                     OR DSP-TOKCD NOT = DEH-F01
                     OR RD-FLG        = "END"
         ADD  1   TO  CT-TBLMAX
         MOVE  DEH-F02           TO  TBL-TRDENK      (X Y)
         MOVE  DEH-F03           TO  TBL-TRDENN      (X Y)
         MOVE  DEH-F04           TO  TBL-SKDENK      (X Y)
         MOVE  DEH-F05           TO  TBL-JYRKBN      (X Y)
         MOVE  DEH-F06           TO  TBL-HNPKBN      (X Y)
         MOVE  DEH-F92           TO  TBL-TORBUMON    (X Y)
         MOVE  DEH-F93           TO  TBL-TORTANCD    (X Y)
         MOVE  DEH-F94           TO  TBL-TORDATE     (X Y)
         MOVE  DEH-F95           TO  TBL-TORTIME     (X Y)
         MOVE  DEH-F96           TO  TBL-KOSBUMON    (X Y)
         MOVE  DEH-F97           TO  TBL-KOSTANCD    (X Y)
         MOVE  DEH-F98           TO  TBL-KOSDATE     (X Y)
         MOVE  DEH-F99           TO  TBL-KOSTIME     (X Y)
         PERFORM  DENHENF-READ-SEC

       END-PERFORM

     END-PERFORM.


     CLOSE  DENHENF.
     OPEN   I-O  DENHENF.
**
 MST-WORK-010.
     MOVE  TABLE-G               TO  TABLE-G-INI.

 MST-WORK-EXIT.
     EXIT.
****************************************************************
*             基本スケジュールマスタ読込み                     *
****************************************************************
 DENHENF-START-SEC     SECTION.
     MOVE  "DENHENF-START-SEC"   TO  S-NAME.
*曜日により基本スケジュールマスタスタート
     MOVE  SPACE                 TO  RD-FLG.
     MOVE  SPACE                 TO  CHK-FLG.
     MOVE  DSP-TOKCD             TO  DEH-F01.
     MOVE  SPACE                 TO  DEH-F02.
     START  DENHENF  KEY IS >= DEH-F01 DEH-F02
       INVALID
         MOVE  "CHK"             TO  CHK-FLG
     END-START.
*
 DENHENF-START-EXIT.
     EXIT.
****************************************************************
*             基本スケジュールマスタ読込み                     *
****************************************************************
 DENHENF-READ-SEC      SECTION.
     MOVE  "DENHENF-READ-SEC"    TO  S-NAME.
*マスタ読込み
     READ  DENHENF  NEXT
       AT END
         MOVE  "END"             TO  RD-FLG
     END-READ.
*
 DENHENF-READ-EXIT.
     EXIT.
****************************************************************
*             ワーク→画面表示                                 *
****************************************************************
 WORK-DSP-SEC          SECTION.
     MOVE  "WORK-DSP-SEC"        TO  S-NAME.
*項目画面セット
     MOVE  ZERO                  TO  CHK-CNT.
     MOVE  TBL-TOKCD             TO  DSP-TOKCD.
     MOVE  TBL-TOKCD             TO  TOK-F01.
     PERFORM HTOKMS-READ-SEC.
     IF  HTOKMS-INV-FLG = "INV"
         MOVE  ALL NC"＊"        TO  DSP-TOKNM
     ELSE
         MOVE  TOK-F03           TO  DSP-TOKNM
     END-IF.
*
     PERFORM  VARYING Y  FROM 1 BY 1
              UNTIL   Y > 12
       MOVE TBL-GYO1(P-CNT Y)    TO  DSP-GYO1(Y)
      IF TBL-TRDENK(P-CNT Y) NOT = SPACE
*******MOVE TBL-GYO1(P-CNT Y)    TO  DSP-GYO1(Y)
       MOVE TBL-TRDENK(P-CNT Y)  TO  DSP-TRDENK(Y)
       MOVE TBL-TRDENN(P-CNT Y)  TO  DSP-TRDENN(Y)
       MOVE TBL-SKDENK(P-CNT Y)  TO  DSP-SKDENK(Y)
       MOVE TBL-JYRKBN(P-CNT Y)  TO  DSP-JYRKBN(Y)
       MOVE TBL-HNPKBN(P-CNT Y)  TO  DSP-HNPKBN(Y)
       MOVE 01                   TO  JYO-F01
       MOVE TBL-SKDENK(P-CNT Y)  TO  JYO-F02
*******DISPLAY "SKDENK = " TBL-SKDENK(P-CNT Y) UPON CONS
       PERFORM HJYOKEN-READ-SEC
       IF  HJYOKEN-INV-FLG = SPACE
           MOVE  JYO-F03         TO  DSP-SKDENN(Y)
       ELSE
           MOVE  ALL NC"＊"      TO  DSP-SKDENN(Y)
       END-IF
       IF  TBL-JYRKBN(P-CNT Y) = "1"
           MOVE NC"○"           TO  DSP-JYRKNM(Y)
       END-IF
       IF  TBL-JYRKBN(P-CNT Y) = "2"
           MOVE NC"×"           TO  DSP-JYRKNM(Y)
       END-IF
       IF  TBL-HNPKBN(P-CNT Y) = "1"
           MOVE NC"○"           TO  DSP-HNPKNM(Y)
       END-IF
       IF  TBL-HNPKBN(P-CNT Y) = "2"
           MOVE NC"×"           TO  DSP-HNPKNM(Y)
       END-IF
       ADD  1  TO  CHK-CNT
      END-IF
     END-PERFORM.
*
 WORK-DSP-EXIT.
     EXIT.
****************************************************************
*             取引先マスタ読込み                               *
****************************************************************
 HTOKMS-READ-SEC       SECTION.
     MOVE  "HTOKMS-READ-SEC"     TO  S-NAME.
     READ  HTOKMS
       INVALID
          MOVE "INV"             TO  HTOKMS-INV-FLG
       NOT INVALID
          MOVE SPACE             TO  HTOKMS-INV-FLG
     END-READ.
*
 HTOKMS-READ-EXIT.
     EXIT.
****************************************************************
*             条件ファイル読込み                               *
****************************************************************
 HJYOKEN-READ-SEC      SECTION.
     MOVE  "HJYOKEN-READ-SEC"    TO  S-NAME.
     READ  HJYOKEN
       INVALID
          MOVE "INV"             TO  HJYOKEN-INV-FLG
       NOT INVALID
          MOVE SPACE             TO  HJYOKEN-INV-FLG
     END-READ.
*
 HJYOKEN-READ-EXIT.
     EXIT.
****************************************************************
*             明細項目　入力( PSW = 2 )                        *
****************************************************************
 DSP-BODY-SEC          SECTION.
     MOVE  "DSP-BODY-SEC"        TO  S-NAME.

     PERFORM  DSP-WRITE-SEC.
     PERFORM  DSP-READ-SEC.

     EVALUATE  DSP-FNC
*      実行
       WHEN  "E000"
         PERFORM  BODY-CHK-SEC

*      取消
       WHEN  "F004"
         MOVE  1                 TO  P-CNT
         MOVE  "1"               TO  PSW
         PERFORM  INIT-DSP-SEC

*      終了
       WHEN  "F005"
         MOVE  "END"             TO  END-FLG

*      項目戻し
       WHEN  "F006"
         MOVE  "1"               TO  PSW

****** 前頁
*      WHEN  "F011"
*        COMPUTE  C-CNT = P-CNT - 1
*        IF  C-CNT = ZERO
*            MOVE  3             TO  ERR-FLG
*        ELSE
*            PERFORM  DSP-WORK-SEC
*            COMPUTE  P-CNT = P-CNT - 1
*            PERFORM  INIT-DSP-SEC
*            PERFORM  WORK-DSP-SEC
*        END-IF
*
****** 次頁
*      WHEN  "F012"
*            MOVE  P-CNT         TO  S-CNT
*            COMPUTE C-CNT  = P-CNT + 1
*            IF  C-CNT > 2
*                MOVE  4         TO  ERR-FLG
*            ELSE
*                IF       (   DSP-TRDENK(12) = SPACE     )
********************最大ページ以下でも、そのページの最後の行
********************が未入力なら次ページなしとする。
*                     MOVE  4    TO  ERR-FLG
*                 ELSE
*                     PERFORM  DSP-WORK-SEC
*                     COMPUTE  P-CNT = P-CNT + 1
*                     PERFORM  INIT-DSP-SEC
*                     PERFORM  WORK-DSP-SEC
*                 END-IF
*            END-IF
******
       WHEN  OTHER
         MOVE  5                 TO  ERR-FLG
     END-EVALUATE.
*
 DSP-BODY-EXIT.
     EXIT.

****************************************************************
*             明細項目チェック                                 *
****************************************************************
 BODY-CHK-SEC          SECTION.
     MOVE  "BODY-CHK-SEC"        TO  S-NAME.
     MOVE  ZERO                  TO  ERR-FLG.
     MOVE  ZERO                  TO  CHK-MEI.
     MOVE  ZERO                  TO  CHK-MEI2.
     MOVE  ZERO                  TO  FG-HANUKE.

     PERFORM  VARYING Y  FROM 1 BY 1
              UNTIL   Y > 12

       PERFORM  BODY-CHK-B-SEC

     END-PERFORM.

     PERFORM BODY-CHK-C-SEC.

     IF  ERR-FLG  =  ZERO
         IF      P-CNT    = 1
             AND CHK-MEI  = ZERO
             AND CHK-MEI2 = ZERO
             MOVE  12        TO  ERR-FLG
         ELSE
**           テーブル領域に設定する。
             PERFORM  DSP-WORK-SEC
             MOVE "3"        TO  PSW
         END-IF
     END-IF.

 BODY-CHK-EXIT.
     EXIT.
****************************************************************
*             明細項目チェック                                 *
****************************************************************
 BODY-CHK-B-SEC          SECTION.
     MOVE  "BODY-CHK-B-SEC"      TO  S-NAME.

     IF  TBL-TRDENK-INI (P-CNT Y) NOT = SPACE
**       初期表示のデータ有り行
         ADD  1   TO  CHK-MEI
     END-IF.

     IF      TBL-TRDENK-INI (P-CNT Y) = SPACE
         AND DSP-TRDENK(Y)            = SPACE
**       画面のデータ無し行
         MOVE  1            TO  FG-HANUKE
         MOVE  SPACE        TO  DSP-TRDENK(Y)
         MOVE  SPACE        TO  DSP-TRDENN(Y)
         MOVE  SPACE        TO  DSP-SKDENK(Y)
         MOVE  SPACE        TO  DSP-SKDENN(Y)
         MOVE  SPACE        TO  DSP-JYRKBN(Y)
         MOVE  SPACE        TO  DSP-HNPKBN(Y)
         GO TO  BODY-CHK-B-EXIT
     ELSE
**       データ有り行
         ADD  1   TO  CHK-MEI2
     END-IF.
*
*取引先伝票区分が入力された場合は前項目必須入力
*****IF  DSP-TRDENK(Y)  NOT =  SPACE
*        IF  DSP-TRDENN(Y)  =  SPACE
*        OR  DSP-SKDENK(Y)  =  SPACE
*        OR  DSP-JYRKBN(Y)  =  SPACE
*        OR  DSP-HNPKBN(Y)  =  SPACE
*            IF  ERR-FLG  =  ZERO
*                MOVE  6         TO  ERR-FLG
*            END-IF
*            MOVE  "R"       TO  EDIT-OPTION OF DSP-TRDENN(Y)
*            MOVE  "R"       TO  EDIT-OPTION OF DSP-SKDENK(Y)
*            MOVE  "R"       TO  EDIT-OPTION OF DSP-JYRKBN(Y)
*            MOVE  "R"       TO  EDIT-OPTION OF DSP-HNPKBN(Y)
*            MOVE  "C"       TO  EDIT-CURSOR OF DSP-TRDENN(Y)
*        END-IF
*****END-IF.
*相手伝票区分名
     IF  DSP-TRDENN(Y)  =  SPACE
     AND DSP-TRDENK(Y)  NOT =  SPACE
         IF  ERR-FLG  =  ZERO
             MOVE  6         TO  ERR-FLG
         END-IF
         MOVE  "R"       TO  EDIT-OPTION OF DSP-TRDENN(Y)
         MOVE  "C"       TO  EDIT-CURSOR OF DSP-TRDENN(Y)
     END-IF.
*サカタ伝票区分
     IF  DSP-SKDENK(Y)  =  SPACE
     AND DSP-TRDENK(Y)  NOT =  SPACE
         IF  ERR-FLG  =  ZERO
             MOVE  6         TO  ERR-FLG
         END-IF
         MOVE  "R"       TO  EDIT-OPTION OF DSP-SKDENK(Y)
         MOVE  "C"       TO  EDIT-CURSOR OF DSP-SKDENK(Y)
     ELSE
       IF  DSP-SKDENK(Y)  =  SPACE
       AND DSP-TRDENK(Y)  =  SPACE
           CONTINUE
       ELSE
         MOVE 01                   TO  JYO-F01
         MOVE DSP-SKDENK(Y)        TO  JYO-F02
*********DISPLAY "Y = " Y UPON CONS
*********DISPLAY "DSP-SKDENK = " DSP-SKDENK(Y) UPON CONS
         PERFORM HJYOKEN-READ-SEC
         IF  HJYOKEN-INV-FLG = SPACE
             MOVE  JYO-F03         TO  DSP-SKDENN(Y)
         ELSE
             MOVE  ALL NC"＊"      TO  DSP-SKDENN(Y)
             IF  ERR-FLG  =  ZERO
                 MOVE  7           TO  ERR-FLG
             END-IF
             MOVE  "R"       TO  EDIT-OPTION OF DSP-SKDENK(Y)
             MOVE  "C"       TO  EDIT-CURSOR OF DSP-SKDENK(Y)
         END-IF
       END-IF
     END-IF.
*受領取込区分
     IF  DSP-JYRKBN(Y)  =  SPACE
     AND DSP-TRDENK(Y)  NOT =  SPACE
         IF  ERR-FLG  =  ZERO
             MOVE  6         TO  ERR-FLG
         END-IF
         MOVE  "R"       TO  EDIT-OPTION OF DSP-JYRKBN(Y)
         MOVE  "C"       TO  EDIT-CURSOR OF DSP-JYRKBN(Y)
     ELSE
       IF  DSP-JYRKBN(Y)  =  SPACE
       AND DSP-TRDENK(Y)  =  SPACE
           MOVE  SPACE      TO  DSP-JYRKNM(Y)
       ELSE
         IF   DSP-JYRKBN(Y)  =  "1"  OR  "2"
              IF  DSP-JYRKBN(Y) = "1"
                  MOVE  NC"○"     TO  DSP-JYRKNM(Y)
              ELSE
                  MOVE  NC"×"     TO  DSP-JYRKNM(Y)
              END-IF
         ELSE
              MOVE  ALL NC"＊"     TO  DSP-JYRKNM(Y)
              IF  ERR-FLG  =  ZERO
                  MOVE  8          TO  ERR-FLG
              END-IF
              MOVE  "R"       TO  EDIT-OPTION OF DSP-JYRKBN(Y)
              MOVE  "C"       TO  EDIT-CURSOR OF DSP-JYRKBN(Y)
         END-IF
       END-IF
     END-IF.
*返品取込区分
     IF  DSP-HNPKBN(Y)  =  SPACE
     AND DSP-TRDENK(Y)  NOT =  SPACE
         IF  ERR-FLG  =  ZERO
             MOVE  6         TO  ERR-FLG
         END-IF
         MOVE  "R"       TO  EDIT-OPTION OF DSP-HNPKBN(Y)
         MOVE  "C"       TO  EDIT-CURSOR OF DSP-HNPKBN(Y)
     ELSE
       IF  DSP-HNPKBN(Y)  =  SPACE
       AND DSP-TRDENK(Y)  =  SPACE
           MOVE  SPACE      TO  DSP-HNPKNM(Y)
       ELSE
         IF   DSP-HNPKBN(Y)  =  "1"  OR  "2"
              IF  DSP-HNPKBN(Y) = "1"
                  MOVE  NC"○"     TO  DSP-HNPKNM(Y)
              ELSE
                  MOVE  NC"×"     TO  DSP-HNPKNM(Y)
              END-IF
         ELSE
              MOVE  ALL NC"＊"     TO  DSP-HNPKNM(Y)
              IF  ERR-FLG  =  ZERO
                  MOVE  9          TO  ERR-FLG
              END-IF
              MOVE  "R"       TO  EDIT-OPTION OF DSP-HNPKBN(Y)
              MOVE  "C"       TO  EDIT-CURSOR OF DSP-HNPKBN(Y)
         END-IF
       END-IF
     END-IF.
** 重複チェック
     MOVE  ZERO                  TO  FG-DUPLICATE.
** 自頁－自頁の重複チェック
     PERFORM  VARYING WK-IX  FROM 1 BY 1
              UNTIL   WK-IX  > 12
                   OR FG-DUPLICATE = 1
       IF      WK-IX             NOT = Y
       AND     DSP-TRDENK(WK-IX)     = DSP-TRDENK(Y)
       AND     DSP-TRDENK(Y)     NOT = SPACE
           MOVE  1               TO  FG-DUPLICATE
       END-IF

     END-PERFORM.

     IF  FG-DUPLICATE = 1
         IF  ERR-FLG = ZERO
             MOVE  10            TO  ERR-FLG
         END-IF
         MOVE  "R"               TO  EDIT-OPTION OF DSP-TRDENK(Y)
         MOVE  "C"               TO  EDIT-CURSOR OF DSP-TRDENK(Y)
         MOVE  "R"               TO  EDIT-OPTION OF DSP-TRDENN(Y)
         MOVE  "C"               TO  EDIT-CURSOR OF DSP-TRDENN(Y)
         MOVE  "R"               TO  EDIT-OPTION OF DSP-SKDENK(Y)
         MOVE  "C"               TO  EDIT-CURSOR OF DSP-SKDENK(Y)
         MOVE  "R"               TO  EDIT-OPTION OF DSP-JYRKBN(Y)
         MOVE  "C"               TO  EDIT-CURSOR OF DSP-JYRKBN(Y)
         MOVE  "R"               TO  EDIT-OPTION OF DSP-HNPKBN(Y)
         MOVE  "C"               TO  EDIT-CURSOR OF DSP-HNPKBN(Y)
         GO TO  BODY-CHK-B-JIKAN-E
     END-IF.

** 自頁－他頁の重複チェック
     PERFORM  VARYING WK-IX  FROM 1 BY 1
              UNTIL   WK-IX  > 2
                   OR FG-DUPLICATE = 1
       IF  WK-IX NOT = P-CNT
           PERFORM  VARYING WK-IX2  FROM 1 BY 1
                    UNTIL   WK-IX2  > 12
             OR ((WK-IX - 1) * 12 + WK-IX2) > CT-TBLMAX
                         OR FG-DUPLICATE = 1
             IF (TBL-TRDENK(WK-IX WK-IX2) NOT = SPACE
             AND TBL-TRDENK(WK-IX WK-IX2)  = DSP-TRDENK(Y))
             AND DSP-TRDENK(Y)            NOT = SPACE
                 MOVE  1     TO  FG-DUPLICATE
             END-IF
           END-PERFORM
       END-IF

     END-PERFORM.

     IF  FG-DUPLICATE = 1
         IF  ERR-FLG = ZERO
             MOVE  10            TO  ERR-FLG
         END-IF
         MOVE  "R"               TO  EDIT-OPTION OF DSP-TRDENK(Y)
         MOVE  "C"               TO  EDIT-CURSOR OF DSP-TRDENK(Y)
         MOVE  "R"               TO  EDIT-OPTION OF DSP-TOKCD(Y)
         MOVE  "C"               TO  EDIT-CURSOR OF DSP-TOKCD(Y)
         GO TO  BODY-CHK-B-JIKAN-E
     END-IF.

 BODY-CHK-B-JIKAN-E.
*
 BODY-CHK-B-EXIT.
     EXIT.
****************************************************************
*             明細項目チェック２                               *
****************************************************************
 BODY-CHK-C-SEC          SECTION.
** 他頁－他頁の重複チェック
     IF  ERR-FLG  =  ZERO
         CONTINUE
     ELSE
         GO TO  BODY-CHK-C-EXIT
     END-IF.
**   テーブル領域を一時退避する。
     MOVE TABLE-G                TO TABLE-G-BK.
**   テーブル領域に仮設定する。
     PERFORM  DSP-WORK-SEC.
**   最終的な重複チェック
     PERFORM  VARYING IX-JIGMN-PG  FROM 1 BY 1
              UNTIL   IX-JIGMN-PG  > 2
                   OR FG-DUPLICATE = 1
       PERFORM  VARYING IX-JIGMN-GYO1  FROM 1 BY 1
                UNTIL   IX-JIGMN-GYO1 > 12
          OR ((IX-JIGMN-PG - 1) * 12 + IX-JIGMN-GYO1) > CT-TBLMAX
                     OR FG-DUPLICATE = 1

         IF  TBL-TRDENK(IX-JIGMN-PG IX-JIGMN-GYO1)
                     NOT = SPACE

             PERFORM  VARYING WK-IX2  FROM 1 BY 1
                      UNTIL   WK-IX2  > 2
                           OR FG-DUPLICATE = 1
               IF  WK-IX2 NOT = IX-JIGMN-PG
                   PERFORM  VARYING WK-IX3  FROM 1 BY 1
                            UNTIL   WK-IX3  > 12
                     OR ((WK-IX2 - 1) * 12 + WK-IX3) > CT-TBLMAX
                                 OR FG-DUPLICATE = 1
                     IF      (TBL-TRDENK(WK-IX2 WK-IX3)
                                      NOT = SPACE
                         AND TBL-TRDENK(WK-IX2 WK-IX3)
                         = TBL-TRDENK(IX-JIGMN-PG IX-JIGMN-GYO1))
                         MOVE  1     TO  FG-DUPLICATE
                     END-IF
                   END-PERFORM
               END-IF

             END-PERFORM

         END-IF

       END-PERFORM

     END-PERFORM.

     IF  FG-DUPLICATE = 1
         IF  ERR-FLG = ZERO
             MOVE  11            TO  ERR-FLG
         END-IF
     END-IF.

**   テーブル領域をを復元する。
     MOVE TABLE-G-BK             TO TABLE-G.

 BODY-CHK-C-EXIT.
     EXIT.
****************************************************************
*             画面→ワーク退避                                 *
****************************************************************
 DSP-WORK-SEC          SECTION.
     MOVE  "DSP-WORK-SEC"        TO  S-NAME.
*
*項目画面セット
     PERFORM  VARYING Y  FROM 1 BY 1
              UNTIL   Y > 12
       IF      DSP-TRDENK(Y) NOT = SPACE
           IF  (P-CNT - 1)  * 12 + Y > CT-TBLMAX
               COMPUTE CT-TBLMAX = (P-CNT - 1)  * 12 + Y
           END-IF
       END-IF

       MOVE  DSP-TRDENK   (Y)    TO  TBL-TRDENK(P-CNT Y)
       MOVE  DSP-TRDENN   (Y)    TO  TBL-TRDENN(P-CNT Y)
       MOVE  DSP-SKDENK   (Y)    TO  TBL-SKDENK(P-CNT Y)
       MOVE  DSP-JYRKBN   (Y)    TO  TBL-JYRKBN(P-CNT Y)
       MOVE  DSP-HNPKBN   (Y)    TO  TBL-HNPKBN(P-CNT Y)

     END-PERFORM.
*
 DSP-WORK-EXIT.
     EXIT.
****************************************************************
*             確認処理　入力（ PSW = 4 ）
****************************************************************
 DSP-KAKU-SEC          SECTION.
     MOVE  "DSP-KAKU-SEC"        TO  S-NAME.

     PERFORM  DSP-WRITE-SEC.
     PERFORM  DSP-READ-SEC.

     EVALUATE  DSP-FNC
*      実行
       WHEN  "E000"
         PERFORM  DSP-WORK-SEC
         PERFORM  DENHENF-DEL-SEC
         PERFORM  DENHENF-WRITE-SEC
         PERFORM  INIT-DSP-SEC
         MOVE  "1"               TO  PSW
*      取消
       WHEN  "F004"
         MOVE  1                 TO  P-CNT
         MOVE  "1"               TO  PSW
         PERFORM  INIT-DSP-SEC
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
             PERFORM  DSP-WORK-SEC
             COMPUTE  P-CNT = P-CNT - 1
             PERFORM  INIT-DSP-SEC
             PERFORM  WORK-DSP-SEC
             MOVE "2"            TO  PSW
         END-IF
*      次頁
       WHEN  "F012"
         MOVE  P-CNT             TO  S-CNT
         COMPUTE  C-CNT = P-CNT + 1
         IF  C-CNT > 2
             MOVE  4             TO  ERR-FLG
         ELSE
             IF  DSP-TRDENK(12) =  SPACE
                 MOVE 4          TO  ERR-FLG
             ELSE
                 PERFORM  DSP-WORK-SEC
                 COMPUTE  P-CNT = P-CNT + 1
                 PERFORM  INIT-DSP-SEC
                 PERFORM  WORK-DSP-SEC
                 MOVE  "2"       TO  PSW
             END-IF

         END-IF

       WHEN   OTHER
         MOVE  5                 TO  ERR-FLG
     END-EVALUATE.
*
 DSP-KAKU-EXIT.
     EXIT.
****************************************************************
*             基本スケジュールマスタ削除位置づけ               *
****************************************************************
 DENHENF-DEL-SEC       SECTION.
     MOVE  "JHKIHD-DEL-SEC"      TO  S-NAME.
*
     MOVE  SPACE                 TO  RD-FLG.
     PERFORM DENHENF-START-SEC.
     IF  CHK-FLG = "CHK"
         GO TO  DENHENF-DEL-EXIT
     END-IF.

     PERFORM DENHENF-READ-SEC.
     PERFORM DLT-SEC
             UNTIL  DSP-TOKCD NOT = DEH-F01
                 OR RD-FLG        = "END".
*
 DENHENF-DEL-EXIT.
     EXIT.
****************************************************************
*             基本スケジュールマスタ削除                       *
****************************************************************
 DLT-SEC               SECTION.
     MOVE  "DLT-SEC"             TO  S-NAME.

     DELETE  DENHENF.
     PERFORM  DENHENF-READ-SEC.

 DLT-EXIT.
     EXIT.
****************************************************************
*             基本スケジュールマスタ作成                       *
****************************************************************
 DENHENF-WRITE-SEC     SECTION.
     MOVE  "DENHENF-WRITE-SEC"   TO  S-NAME.

     PERFORM  SDATE-GET-SEC.

     MOVE  1                     TO  Y.
     PERFORM  VARYING X  FROM 1 BY 1
              UNTIL   X > 2
       PERFORM  VARYING Y  FROM 1 BY 1
                UNTIL   Y > 12
         IF  TBL-TRDENK(X Y)  NOT =  SPACE
             MOVE  SPACE           TO  DEH-REC
             INITIALIZE  DEH-REC
             MOVE  DSP-TOKCD       TO  DEH-F01
             MOVE  TBL-TRDENK(X Y) TO  DEH-F02

             EVALUATE  TRUE
**             初期データが存在しない。
**             キーが変わっていたら登録
**             X:0 X:Y
**             登録
               WHEN  (    TBL-TRDENK-INI (X Y) = SPACE
                      AND TBL-TRDENK(X Y) NOT =
                              TBL-TRDENK-INI (X Y))
*****************DISPLAY NC"登録" UPON CONS
                 MOVE  TBL-TRDENN     (X Y)  TO  DEH-F03
                 MOVE  TBL-SKDENK     (X Y)  TO  DEH-F04
                 MOVE  TBL-JYRKBN     (X Y)  TO  DEH-F05
                 MOVE  TBL-HNPKBN     (X Y)  TO  DEH-F06
                 MOVE  PAR-BUMON             TO  DEH-F92
                 MOVE  PAR-TANCD             TO  DEH-F93
                 MOVE  SYS-DATE              TO  DEH-F94
                 MOVE  WK-TIME(1:4)          TO  DEH-F95
                 MOVE  SPACE                 TO  DEH-F96
                 MOVE  SPACE                 TO  DEH-F97
                 MOVE  ZERO                  TO  DEH-F98
                 MOVE  ZERO                  TO  DEH-F99

**             変更無し
               WHEN  TBL-TRDENK (X Y) = TBL-TRDENK-INI (X Y)
               AND   TBL-TRDENN (X Y) = TBL-TRDENN-INI (X Y)
               AND   TBL-SKDENK (X Y) = TBL-SKDENK-INI (X Y)
               AND   TBL-JYRKBN (X Y) = TBL-JYRKBN-INI (X Y)
               AND   TBL-HNPKBN (X Y) = TBL-HNPKBN-INI (X Y)
*****************DISPLAY NC"変更無し" UPON CONS
                 MOVE  TBL-TRDENN     (X Y)  TO  DEH-F03
                 MOVE  TBL-SKDENK     (X Y)  TO  DEH-F04
                 MOVE  TBL-JYRKBN     (X Y)  TO  DEH-F05
                 MOVE  TBL-HNPKBN     (X Y)  TO  DEH-F06
                 MOVE  TBL-TORBUMON   (X Y)  TO  DEH-F92
                 MOVE  TBL-TORTANCD   (X Y)  TO  DEH-F93
                 MOVE  TBL-TORDATE    (X Y)  TO  DEH-F94
                 MOVE  TBL-TORTIME    (X Y)  TO  DEH-F95
                 MOVE  TBL-KOSBUMON   (X Y)  TO  DEH-F96
                 MOVE  TBL-KOSTANCD   (X Y)  TO  DEH-F97
                 MOVE  TBL-KOSDATE    (X Y)  TO  DEH-F98
                 MOVE  TBL-KOSTIME    (X Y)  TO  DEH-F99
**             修正
               WHEN  OTHER
*****************DISPLAY NC"修正" UPON CONS
                 MOVE  TBL-TRDENN     (X Y)  TO  DEH-F03
                 MOVE  TBL-SKDENK     (X Y)  TO  DEH-F04
                 MOVE  TBL-JYRKBN     (X Y)  TO  DEH-F05
                 MOVE  TBL-HNPKBN     (X Y)  TO  DEH-F06
                 MOVE  TBL-TORBUMON   (X Y)  TO  DEH-F92
                 MOVE  TBL-TORTANCD   (X Y)  TO  DEH-F93
                 MOVE  TBL-TORDATE    (X Y)  TO  DEH-F94
                 MOVE  TBL-TORTIME    (X Y)  TO  DEH-F95
                 MOVE  PAR-BUMON             TO  DEH-F96
                 MOVE  PAR-TANCD             TO  DEH-F97
                 MOVE  SYS-DATE              TO  DEH-F98
                 MOVE  WK-TIME(1:4)          TO  DEH-F99

             END-EVALUATE

             WRITE  DEH-REC
         END-IF
       END-PERFORM

       MOVE  1                   TO  Y
     END-PERFORM.
*
 DENHENF-WRITE-EXIT.
     EXIT.
****************************************************************
*             終了処理                               3.0       *
****************************************************************
 END-SEC               SECTION.
     MOVE  "END-SEC"             TO  S-NAME.
*ファイル ＣＬＯＳＥ
     CLOSE  DSPFILE DENHENF
            HJYOKEN HTOKMS.

 END-EXIT.
     EXIT.
*****************<<  SJR0420I   END PROGRAM  >>******************
