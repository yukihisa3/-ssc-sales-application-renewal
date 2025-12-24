# SBZ0080I

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/SBZ0080I.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　部門間在庫移動機能構築　　　　　　*
*    モジュール名　　　　：　部門間移動マスタ保守　　　        *
*    作成日／更新日　　　：　2018/01/22                        *
*    作成者／更新者　　　：　ＮＡＶ高橋                        *
*    処理概要　　　　　　：　部門間移動マスタの登録修正削除を  *
*                            行なう。　　                      *
*    更新日／更新者　　　：　                                  *
*    修正概要　　　　　　：                                    *
****************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SBZ0080I.
 AUTHOR.               NAV.
 DATE-WRITTEN.         2018/01/22.
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
*部門間移動マスタ
     SELECT  BUMIDOF   ASSIGN    TO        DA-01-VI-BUMIDOL1
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      DYNAMIC
                       RECORD    KEY       BUM-F01
                                           BUM-F02
                                           BUM-F03
                       FILE      STATUS    BUM-ST.
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
*倉庫マスタ
     SELECT  ZSOKMS    ASSIGN    TO        DA-01-VI-ZSOKMS1
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
*    FILE = 部門間移動マスタ　　　                           *
****************************************************************
 FD  BUMIDOF
                       LABEL     RECORD    IS   STANDARD.
                       COPY      BUMIDOF   OF   XFDLIB
                       JOINING   BUM       AS   PREFIX.
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
                       LABEL     RECORD    IS   STANDARD.
                       COPY      HTOKMS    OF   XFDLIB
                       JOINING   TOK       AS   PREFIX.
****************************************************************
*    FILE = 倉庫マスタ                                       *
****************************************************************
 FD  ZSOKMS
                       LABEL     RECORD    IS   STANDARD.
                       COPY      ZSOKMS    OF   XFDLIB
                       JOINING   SOK       AS   PREFIX.
****************************************************************
*    FILE = 画面ファイル                                       *
****************************************************************
 FD  DSPFILE
                       LABEL     RECORD    IS   OMITTED.
                       COPY      FBZ00801  OF   XMDLIB
                       JOINING   DSP       AS   PREFIX.
*
****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
*ステータス領域
 01  STATUS-AREA.
     03  BUM-ST                   PIC  X(02).
     03  JYO-ST                   PIC  X(02).
     03  TOK-ST                   PIC  X(02).
     03  SOK-ST                   PIC  X(02).
     03  DSP-ST                   PIC  X(02).
*画面制御用領域
 01  DSP-CONTROL.
     03  DSP-FMT                  PIC  X(08).
     03  DSP-GRP                  PIC  X(08).
     03  DSP-PRO                  PIC  X(02).
     03  DSP-FNC                  PIC  X(04).
*フラグ領域
 01  FLG-AREA.
     03  BUMIDOF-INV-FLG          PIC  X(03)  VALUE  SPACE.
     03  HTOKMS-INV-FLG           PIC  X(03)  VALUE  SPACE.
     03  ZSOKMS-INV-FLG           PIC  X(03)  VALUE  SPACE.
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
*            NC"_取消　_終了　_項目戻し　_前頁　_次".
             NC"_取消　_終了　_項目戻し　　　　　　　".
         05  FILLER               PIC  N(10)  VALUE
*            NC"頁　　　　　　　　　".
             NC"　　　　　　　　　　".
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
             VALUE NC"　　　　　　　　　　　　　　".
     03  ERR-MSG8.
         05  FILLER              PIC   N(20)
             VALUE NC"　　　　　　　　　　　　　　　　　　　　".
     03  ERR-MSG9.
         05  FILLER              PIC   N(20)
             VALUE NC"　　　　　　　　　　　　　　　　　　　　".
     03  ERR-MSG10.
         05  FILLER              PIC   N(20)
             VALUE NC"登録コードが重複しております。".
     03  ERR-MSG11.
         05  FILLER              PIC   N(20)
             VALUE NC"他頁の登録コードが重複しております。".
     03  ERR-MSG12.
         05  FILLER              PIC   N(20)
             VALUE NC"明細行を入力して下さい。".
     03  ERR-MSG13.
         05  FILLER              PIC   N(20)
             VALUE NC"倉庫ＣＤ未入力です。".
     03  ERR-MSG14.
         05  FILLER              PIC   N(20)
             VALUE NC"倉庫マスタ未登録です。".
     03  ERR-MSG15.
         05  FILLER              PIC   N(20)
             VALUE NC"部門ＣＤ未入力です。".
     03  ERR-MSG16.
         05  FILLER              PIC   N(20)
             VALUE NC"部門ＣＤが条件Ｆ未登録です。".
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
*
*部門間移動マスタ退避エリア
 01  TABLE-AREA.
     03  TBL-SKBUCD                 PIC   X(04).

     03  TABLE-G.
       05  TABLE1      OCCURS  2.
           07  TABLE2  OCCURS  15.
               09  TBL-GYO1         PIC   9(02).
               09  TBL-MTOKCD       PIC   9(08).
               09  TBL-MTOKNM       PIC   N(10).
               09  TBL-MSOKCD       PIC   X(02).
               09  TBL-MSOKNM       PIC   N(08).
               09  TBL-MTBUCD       PIC   X(04).
               09  TBL-MTBUNM       PIC   N(08).
               09  TBL-JTOKCD       PIC   9(08).
               09  TBL-TORBUMON     PIC   X(04).
               09  TBL-TORTANCD     PIC   X(02).
               09  TBL-TORDATE      PIC   9(08).
               09  TBL-TORTIME      PIC   9(06).
               09  TBL-KOSBUMON     PIC   X(04).
               09  TBL-KOSTANCD     PIC   X(02).
               09  TBL-KOSDATE      PIC   9(08).
               09  TBL-KOSTIME      PIC   9(06).
     03  TABLE-G-R  REDEFINES TABLE-G.
         05  TABLE2-R  OCCURS  30.
           07  TBL-GYO1-R           PIC   9(02).
           07  TBL-MTOKCD-R         PIC   9(08).
           07  TBL-MTOKNM-R         PIC   N(10).
           07  TBL-MSOKCD-R         PIC   X(02).
           07  TBL-MSOKNM-R         PIC   N(08).
           07  TBL-MTBUCD-R         PIC   X(04).
           07  TBL-MTBUNM-R         PIC   N(08).
           07  TBL-JTOKCD-R         PIC   9(08).
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
           07  TABLE2-INI  OCCURS  15.
               09  TBL-GYO1-INI       PIC   9(02).
               09  TBL-MTOKCD-INI     PIC   9(08).
               09  TBL-MTOKNM-INI     PIC   N(10).
               09  TBL-MSOKCD-INI     PIC   X(02).
               09  TBL-MSOKNM-INI     PIC   N(08).
               09  TBL-MTBUCD-INI     PIC   X(04).
               09  TBL-MTBUNM-INI     PIC   N(08).
               09  TBL-JTOKCD-INI     PIC   9(08).
               09  TBL-TORBUMON-INI   PIC   X(04).
               09  TBL-TORTANCD-INI   PIC   X(02).
               09  TBL-TORDATE-INI    PIC   9(08).
               09  TBL-TORTIME-INI    PIC   9(06).
               09  TBL-KOSBUMON-INI   PIC   X(04).
               09  TBL-KOSTANCD-INI   PIC   X(02).
               09  TBL-KOSDATE-INI    PIC   9(08).
               09  TBL-KOSTIME-INI    PIC   9(06).
     03  TABLE-G-INI-R  REDEFINES TABLE-G-INI.
         05  TABLE2-INI-R  OCCURS  30.
           07  TBL-GYO1-INI-R        PIC   9(02).
           07  TBL-MTOKCD-INI-R      PIC   9(08).
           07  TBL-MTOKNM-INI-R      PIC   N(10).
           07  TBL-MSOKCD-INI-R      PIC   X(02).
           07  TBL-MSOKNM-INI-R      PIC   N(08).
           07  TBL-MTBUCD-INI-R      PIC   X(04).
           07  TBL-MTBUNM-INI-R      PIC   N(08).
           07  TBL-JTOKCD-INI-R      PIC   9(08).
           07  TBL-TORBUMON-INI-R    PIC   X(04).
           07  TBL-TORTANCD-INI-R    PIC   X(02).
           07  TBL-TORDATE-INI-R     PIC   9(08).
           07  TBL-TORTIME-INI-R     PIC   9(06).
           07  TBL-KOSBUMON-INI-R    PIC   X(04).
           07  TBL-KOSTANCD-INI-R    PIC   X(02).
           07  TBL-KOSDATE-INI-R     PIC   9(08).
           07  TBL-KOSTIME-INI-R     PIC   9(06).
*
     03  TABLE-G-BK.
       05  TABLE1-BK      OCCURS  2.
           07  TABLE2-BK  OCCURS  15.
               09  TBL-GYO1-BK          PIC   9(02).
               09  TBL-MTOKCD-BK        PIC   9(08).
               09  TBL-MTOKNM-BK        PIC   N(10).
               09  TBL-MSOKCD-BK        PIC   X(02).
               09  TBL-MSOKNM-BK        PIC   N(08).
               09  TBL-MTBUCD-BK        PIC   X(04).
               09  TBL-MTBUNM-BK        PIC   N(08).
               09  TBL-JTOKCD-BK        PIC   9(08).
               09  TBL-TORBUMON-BK      PIC   X(04).
               09  TBL-TORTANCD-BK      PIC   X(02).
               09  TBL-TORDATE-BK       PIC   9(08).
               09  TBL-TORTIME-BK       PIC   9(06).
               09  TBL-KOSBUMON-BK      PIC   X(04).
               09  TBL-KOSTANCD-BK      PIC   X(02).
               09  TBL-KOSDATE-BK       PIC   9(08).
               09  TBL-KOSTIME-BK       PIC   9(06).
     03  TABLE-G-BK-R  REDEFINES TABLE-G-BK.
         05  TABLE2-BK-R  OCCURS  30.
           07  TBL-GYO1-BK-R        PIC   9(02).
           07  TBL-MTOKCD-BK-R      PIC   9(08).
           07  TBL-MTOKNM-BK-R      PIC   N(10).
           07  TBL-MSOKCD-BK-R      PIC   X(02).
           07  TBL-MSOKNM-BK-R      PIC   N(08).
           07  TBL-MTBUCD-BK-R      PIC   X(04).
           07  TBL-MTBUNM-BK-R      PIC   N(08).
           07  TBL-JTOKCD-BK-R      PIC   9(08).
           07  TBL-TORBUMON-BK-R    PIC   X(04).
           07  TBL-TORTANCD-BK-R    PIC   X(02).
           07  TBL-TORDATE-BK-R     PIC   9(08).
           07  TBL-TORTIME-BK-R     PIC   9(06).
           07  TBL-KOSBUMON-BK-R    PIC   X(04).
           07  TBL-KOSTANCD-BK-R    PIC   X(02).
           07  TBL-KOSDATE-BK-R     PIC   9(08).
           07  TBL-KOSTIME-BK-R     PIC   9(06).

*
*ファイルエラーメッセージ
 01  FILE-ERR.
     03  BUM-ERR           PIC N(15) VALUE
                        NC"部門間移動マスタエラー".
     03  JYO-ERR           PIC N(15) VALUE
                        NC"条件ファイルエラー".
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
 BUM-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE BUMIDOF.
     MOVE        BUM-ST    TO        E-ST.
     MOVE        "BUMIDOF" TO        E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     BUM-ERR   UPON      CONS.
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
     OPEN  I-O    BUMIDOF.
     OPEN  I-O    DSPFILE.
     OPEN  INPUT  HJYOKEN HTOKMS ZSOKMS.
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
     MOVE  SPACE                 TO  DSP-FBZ00801.
*システム日付転送
     MOVE  SYS-DATE              TO  DSP-SDATE.
*システム時間転送
     MOVE  WK-TIME(1:6)          TO  DSP-STIME.
*プログラムＩＤ
     MOVE  "SBZ0080I"            TO  DSP-PGID.
*ＦＯＲＭＩＤ
     MOVE  "FBZ00801"            TO  DSP-FORMID.
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
***  移動先部門ＣＤ
     MOVE  "M"              TO  EDIT-OPTION OF DSP-SKBUCD.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-SKBUCD.
     PERFORM  VARYING X  FROM 1 BY 1
              UNTIL   X > 15
***  移動取引先ＣＤ
       MOVE  "M"            TO  EDIT-OPTION OF DSP-MTOKCD(X)
       MOVE  SPACE          TO  EDIT-CURSOR OF DSP-MTOKCD(X)
***  移動取引先名
*      MOVE  "M"            TO  EDIT-OPTION OF DSP-MTOKNM(X)
*      MOVE  SPACE          TO  EDIT-CURSOR OF DSP-MTOKNM(X)
***  倉庫ＣＤ
       MOVE  "M"            TO  EDIT-OPTION OF DSP-MSOKCD(X)
       MOVE  SPACE          TO  EDIT-CURSOR OF DSP-MSOKCD(X)
***  倉庫名
*      MOVE  "M"            TO  EDIT-OPTION OF DSP-MSOKNM(X)
*      MOVE  SPACE          TO  EDIT-CURSOR OF DSP-MSOKNM(X)
***  移動元部門ＣＤ
       MOVE  "M"            TO  EDIT-OPTION OF DSP-MTBUCD(X)
       MOVE  SPACE          TO  EDIT-CURSOR OF DSP-MTBUCD(X)
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
*      ヘッダ入力　(2.1)
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
*             ヘッダ入力( PSW = 1 )                          *
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
     MOVE  "FBZ00801"            TO  DSP-FMT.
     WRITE  DSP-FBZ00801.
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
*      ヘッダ
       WHEN  "1"
         MOVE  "GPHEAD"          TO  DSP-GRP
*      明細
       WHEN  "2"
         MOVE  "GPBODY"          TO  DSP-GRP
*      確認
       WHEN  "3"
         MOVE  "GPKAKU"          TO  DSP-GRP
     END-EVALUATE.

     MOVE  "FBZ00801"            TO  DSP-FMT.
     READ  DSPFILE.
*
*入力項目の属性を通常にする
 DSP-READ-010.
*    MOVE  ZERO                  TO  ERR-FLG.
     MOVE  SPACE                 TO  DSP-PRO.
*
 DSP-READ-EXIT.
     EXIT.
****************************************************************
*             移動先部門ＣＤチェック                           *
****************************************************************
 HEAD-CHK-SEC             SECTION.
     MOVE  "HEAD-CHK-SEC"        TO  S-NAME.
*移動先部門ＣＤチェック
     IF  DSP-SKBUCD NOT NUMERIC
     OR  DSP-SKBUCD = ZERO
         MOVE  15                TO  ERR-FLG
         MOVE  "R"               TO  EDIT-OPTION  OF  DSP-SKBUCD
         MOVE  "C"               TO  EDIT-CURSOR  OF  DSP-SKBUCD
     ELSE
         MOVE 22                   TO  JYO-F01
         MOVE DSP-SKBUCD           TO  JYO-F02
         PERFORM HJYOKEN-READ-SEC
         IF  HJYOKEN-INV-FLG  =  "INV"
             MOVE  16            TO  ERR-FLG
             MOVE  "R"           TO  EDIT-OPTION  OF  DSP-SKBUCD
             MOVE  "C"           TO  EDIT-CURSOR  OF  DSP-SKBUCD
             MOVE  ALL NC"＊"    TO  DSP-SKBUNM
         ELSE
             MOVE  "M"           TO  EDIT-OPTION  OF  DSP-SKBUCD
             MOVE  SPACE         TO  EDIT-CURSOR  OF  DSP-SKBUCD
             MOVE  JYO-F03       TO  DSP-SKBUNM
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
*I↓
     PERFORM  VARYING X  FROM 1 BY 1
              UNTIL   X > 15
              MOVE    SPACE      TO  DSP-MAS001(X)
     END-PERFORM.
*I↑

*移動先部門のワーク退避
     MOVE  DSP-SKBUCD            TO  TBL-SKBUCD.
*ＳＥＱの作成
     MOVE  ZERO                  TO  GYO1-CNT.
     PERFORM  VARYING X  FROM 1 BY 1
              UNTIL   X > 2
       PERFORM  VARYING Y FROM  1 BY 1
                UNTIL Y > 15
         ADD 1  TO  GYO1-CNT
         MOVE  GYO1-CNT           TO  TBL-GYO1   (X Y)
*********DISPLAY "TBL-GYO1 = " TBL-GYO1(X Y) UPON CONS
       END-PERFORM
     END-PERFORM.
*移動先部門ＣＤにより部門間移動マスタスタート
     PERFORM BUMIDOF-START-SEC.
     IF  CHK-FLG = "CHK"
         GO TO  MST-WORK-010
     END-IF.
*
*部門間移動マスタ読込み
     PERFORM  BUMIDOF-READ-SEC.
*マスタを読込みながらワークへセット
     MOVE  ZERO                  TO  CT-TBLMAX.
     PERFORM  VARYING X  FROM 1 BY 1
              UNTIL   X > 2
                   OR DSP-SKBUCD NOT = BUM-F01
                   OR RD-FLG        = "END"
       PERFORM  VARYING Y  FROM 1 BY 1
                UNTIL   Y > 15
                     OR DSP-SKBUCD NOT = BUM-F01
                     OR RD-FLG        = "END"
         ADD  1   TO  CT-TBLMAX
*移動取引先ＣＤ
         MOVE  BUM-F02           TO  TBL-MTOKCD      (X Y)
*        取引先マスタ検索
         MOVE  TBL-MTOKCD(X Y)   TO  TOK-F01
         PERFORM HTOKMS-READ-SEC
         IF  HTOKMS-INV-FLG = "INV"
             MOVE  ALL NC"＊"    TO  TBL-MTOKNM      (X Y)
             MOVE  ZERO          TO  TBL-JTOKCD      (X Y)
         ELSE
             MOVE  TOK-F03       TO  TBL-MTOKNM      (X Y)
             MOVE  TOK-F52       TO  TBL-JTOKCD      (X Y)
         END-IF
*倉庫ＣＤ
         MOVE  BUM-F03           TO  TBL-MSOKCD      (X Y)
*        倉庫マスタ検索
         MOVE  TBL-MSOKCD(X Y)   TO  SOK-F01
         PERFORM ZSOKMS-READ-SEC
         IF  ZSOKMS-INV-FLG = "INV"
             MOVE  ALL NC"＊"    TO  TBL-MSOKNM      (X Y)
         ELSE
             MOVE  SOK-F02       TO  TBL-MSOKNM      (X Y)
         END-IF
*移動元部門ＣＤ
         MOVE  BUM-F04           TO  TBL-MTBUCD      (X Y)
*        条件ファイル検索
         MOVE  22                TO  JYO-F01
         MOVE  TBL-MTBUCD(X Y)   TO  JYO-F02
         PERFORM HJYOKEN-READ-SEC
         IF  HJYOKEN-INV-FLG = "INV"
             MOVE  ALL NC"＊"    TO  TBL-MTBUNM      (X Y)
         ELSE
             MOVE  JYO-F03       TO  TBL-MTBUNM      (X Y)
         END-IF
*
         MOVE  BUM-F92           TO  TBL-TORBUMON    (X Y)
         MOVE  BUM-F93           TO  TBL-TORTANCD    (X Y)
         MOVE  BUM-F94           TO  TBL-TORDATE     (X Y)
         MOVE  BUM-F95           TO  TBL-TORTIME     (X Y)
         MOVE  BUM-F96           TO  TBL-KOSBUMON    (X Y)
         MOVE  BUM-F97           TO  TBL-KOSTANCD    (X Y)
         MOVE  BUM-F98           TO  TBL-KOSDATE     (X Y)
         MOVE  BUM-F99           TO  TBL-KOSTIME     (X Y)
         PERFORM  BUMIDOF-READ-SEC

       END-PERFORM

     END-PERFORM.


     CLOSE  BUMIDOF.
     OPEN   I-O  BUMIDOF.
**
 MST-WORK-010.
     MOVE  TABLE-G               TO  TABLE-G-INI.

 MST-WORK-EXIT.
     EXIT.
****************************************************************
*             部門間移動マスタSTART                     *
****************************************************************
 BUMIDOF-START-SEC     SECTION.
     MOVE  "BUMIDOF-START-SEC"   TO  S-NAME.
*部門間移動マスタスタート
     MOVE  SPACE                 TO  RD-FLG.
     MOVE  SPACE                 TO  CHK-FLG.
     MOVE  DSP-SKBUCD            TO  BUM-F01.
     MOVE  ZERO                  TO  BUM-F02.
     MOVE  SPACE                 TO  BUM-F03.
     START  BUMIDOF  KEY IS >= BUM-F01 BUM-F02 BUM-F03
       INVALID
         MOVE  "CHK"             TO  CHK-FLG
     END-START.
*
 BUMIDOF-START-EXIT.
     EXIT.
****************************************************************
*             部門間移動マスタ読込み                     *
****************************************************************
 BUMIDOF-READ-SEC      SECTION.
     MOVE  "BUMIDOF-READ-SEC"    TO  S-NAME.
*マスタ読込み
     READ  BUMIDOF  NEXT
       AT END
         MOVE  "END"             TO  RD-FLG
     END-READ.
*
 BUMIDOF-READ-EXIT.
     EXIT.
****************************************************************
*             ワーク→画面表示                                 *
****************************************************************
 WORK-DSP-SEC          SECTION.
     MOVE  "WORK-DSP-SEC"        TO  S-NAME.
*項目画面セット
     MOVE  ZERO                  TO  CHK-CNT.
     MOVE  TBL-SKBUCD            TO  DSP-SKBUCD.
     MOVE  22                    TO  JYO-F02.
     MOVE  TBL-SKBUCD            TO  JYO-F02.
     PERFORM HJYOKEN-READ-SEC.
     IF  HJYOKEN-INV-FLG = "INV"
         MOVE  ALL NC"＊"        TO  DSP-SKBUNM
     ELSE
         MOVE  JYO-F03           TO  DSP-SKBUNM
     END-IF.
*
     PERFORM  VARYING Y  FROM 1 BY 1
              UNTIL   Y > 15
*      MOVE TBL-GYO1(P-CNT Y)    TO  DSP-GYO1(Y)
      IF TBL-MTOKCD(P-CNT Y) NOT = ZERO
*******MOVE TBL-GYO1(P-CNT Y)    TO  DSP-GYO1(Y)
       MOVE TBL-MTOKCD(P-CNT Y)  TO  DSP-MTOKCD(Y)
       MOVE TBL-MTOKNM(P-CNT Y)  TO  DSP-MTOKNM(Y)
       MOVE TBL-MSOKCD(P-CNT Y)  TO  DSP-MSOKCD(Y)
       MOVE TBL-MSOKNM(P-CNT Y)  TO  DSP-MSOKNM(Y)
       MOVE TBL-MTBUCD(P-CNT Y)  TO  DSP-MTBUCD(Y)
       MOVE TBL-MTBUNM(P-CNT Y)  TO  DSP-MTBUNM(Y)
       MOVE 01                   TO  JYO-F01
       MOVE TBL-MSOKCD(P-CNT Y)  TO  JYO-F02
*******DISPLAY "MSOKCD = " TBL-MSOKCD(P-CNT Y) UPON CONS
*      PERFORM HJYOKEN-READ-SEC
*      IF  HJYOKEN-INV-FLG = SPACE
*          MOVE  JYO-F03         TO  DSP-MSOKNM(Y)
*      ELSE
*          MOVE  ALL NC"＊"      TO  DSP-MSOKNM(Y)
*      END-IF
*      IF  TBL-MSOKNM(P-CNT Y) = "1"
*          MOVE NC"○"           TO  DSP-JYRKNM(Y)
*      END-IF
*      IF  TBL-MSOKNM(P-CNT Y) = "2"
*          MOVE NC"×"           TO  DSP-JYRKNM(Y)
*      END-IF
*      IF  TBL-MTBUCD(P-CNT Y) = "1"
*          MOVE NC"○"           TO  DSP-HNPKNM(Y)
*      END-IF
*      IF  TBL-MTBUCD(P-CNT Y) = "2"
*          MOVE NC"×"           TO  DSP-HNPKNM(Y)
*      END-IF
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
*             倉庫マスタ読込み                               *
****************************************************************
 ZSOKMS-READ-SEC       SECTION.
     MOVE  "ZSOKMS-READ-SEC"     TO  S-NAME.
     READ  ZSOKMS
       INVALID
          MOVE "INV"             TO  ZSOKMS-INV-FLG
       NOT INVALID
          MOVE SPACE             TO  ZSOKMS-INV-FLG
     END-READ.
*
 ZSOKMS-READ-EXIT.
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
*                IF       (   DSP-MTOKCD(12) = ZERO      )
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
              UNTIL   Y > 15

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

*T↓
*    PERFORM DSP-SYOKI-SEC.
*T↑
     IF  TBL-MTOKCD-INI (P-CNT Y) NOT = ZERO
**       初期表示のデータ有り行
         ADD  1   TO  CHK-MEI
     END-IF.

     IF      TBL-MTOKCD-INI (P-CNT Y) = ZERO
         AND DSP-MTOKCD(Y)            = ZERO
**       画面のデータ無し行
         MOVE  1            TO  FG-HANUKE
         MOVE  ZERO         TO  DSP-MTOKCD(Y)
         MOVE  SPACE        TO  DSP-MTOKNM(Y)
         MOVE  SPACE        TO  DSP-MSOKCD(Y)
         MOVE  SPACE        TO  DSP-MSOKNM(Y)
         MOVE  SPACE        TO  DSP-MTBUCD(Y)
         MOVE  SPACE        TO  DSP-MTBUNM(Y)
         GO TO  BODY-CHK-B-EXIT
     ELSE
**       データ有り行
         ADD  1   TO  CHK-MEI2
     END-IF.
*
*取引先伝票区分が入力された場合は前項目必須入力
*****IF  DSP-MTOKCD(Y)  NOT =  ZERO
*        IF  DSP-MTOKNM(Y)  =  SPACE
*        OR  DSP-MSOKCD(Y)  =  SPACE
*        OR  DSP-MSOKNM(Y)  =  SPACE
*        OR  DSP-MTBUCD(Y)  =  SPACE
*            IF  ERR-FLG  =  ZERO
*                MOVE  6         TO  ERR-FLG
*            END-IF
*            MOVE  "R"       TO  EDIT-OPTION OF DSP-MTOKNM(Y)
*            MOVE  "R"       TO  EDIT-OPTION OF DSP-MSOKCD(Y)
*            MOVE  "R"       TO  EDIT-OPTION OF DSP-MSOKNM(Y)
*            MOVE  "R"       TO  EDIT-OPTION OF DSP-MTBUCD(Y)
*            MOVE  "C"       TO  EDIT-CURSOR OF DSP-MTOKNM(Y)
*        END-IF
*****END-IF.
*移動取引先ＣＤ
     IF  DSP-MTOKCD(Y)  NOT =  ZERO
         MOVE DSP-MTOKCD(Y)        TO  TOK-F01
*********DISPLAY "Y = " Y UPON CONS
*********DISPLAY "DSP-MSOKCD = " DSP-MSOKCD(Y) UPON CONS
         PERFORM HTOKMS-READ-SEC
         IF  HTOKMS-INV-FLG = SPACE
             MOVE  TOK-F03         TO  DSP-MTOKNM(Y)
         ELSE
             MOVE  ALL NC"＊"      TO  DSP-MTOKNM(Y)
             IF  ERR-FLG  =  ZERO
                 MOVE  2           TO  ERR-FLG
             END-IF
             MOVE  "R"       TO  EDIT-OPTION OF DSP-MTOKCD(Y)
             MOVE  "C"       TO  EDIT-CURSOR OF DSP-MTOKCD(Y)
         END-IF
     END-IF.
*倉庫ＣＤ
     IF  DSP-MSOKCD(Y)  =  SPACE
     AND DSP-MTOKCD(Y)  NOT =  ZERO
         IF  ERR-FLG  =  ZERO
             MOVE  13        TO  ERR-FLG
         END-IF
         MOVE  "R"       TO  EDIT-OPTION OF DSP-MSOKCD(Y)
         MOVE  "C"       TO  EDIT-CURSOR OF DSP-MSOKCD(Y)
     ELSE
       IF  DSP-MSOKCD(Y)  =  SPACE
       AND DSP-MTOKCD(Y)  =  ZERO
           CONTINUE
       ELSE
         MOVE DSP-MSOKCD(Y)        TO  SOK-F01
*********DISPLAY "Y = " Y UPON CONS
*********DISPLAY "DSP-MSOKCD = " DSP-MSOKCD(Y) UPON CONS
         PERFORM ZSOKMS-READ-SEC
         IF  ZSOKMS-INV-FLG = SPACE
             MOVE  SOK-F02         TO  DSP-MSOKNM(Y)
         ELSE
             MOVE  ALL NC"＊"      TO  DSP-MSOKNM(Y)
             IF  ERR-FLG  =  ZERO
                 MOVE  14          TO  ERR-FLG
             END-IF
             MOVE  "R"       TO  EDIT-OPTION OF DSP-MSOKCD(Y)
             MOVE  "C"       TO  EDIT-CURSOR OF DSP-MSOKCD(Y)
         END-IF
       END-IF
     END-IF.
*移動先部門ＣＤ
     IF  DSP-MTBUCD(Y)  =  SPACE
     AND DSP-MTOKCD(Y)  NOT =  ZERO
         IF  ERR-FLG  =  ZERO
             MOVE  15        TO  ERR-FLG
         END-IF
         MOVE  "R"       TO  EDIT-OPTION OF DSP-MTBUCD(Y)
         MOVE  "C"       TO  EDIT-CURSOR OF DSP-MTBUCD(Y)
*T↓
*        GO              TO  BODY-CHK-B-EXIT
*T↑
     ELSE
       IF  DSP-MTBUCD(Y)  =  SPACE
       AND DSP-MTOKCD(Y)  =  ZERO
           CONTINUE
       ELSE
         MOVE 22                   TO  JYO-F01
         MOVE DSP-MTBUCD(Y)        TO  JYO-F02
*********DISPLAY "Y = " Y UPON CONS
*********DISPLAY "DSP-MTBUCD = " DSP-MTBUCD(Y) UPON CONS
         PERFORM HJYOKEN-READ-SEC
         IF  HJYOKEN-INV-FLG = SPACE
             MOVE  JYO-F03         TO  DSP-MTBUNM(Y)
         ELSE
             MOVE  ALL NC"＊"      TO  DSP-MTBUNM(Y)
             IF  ERR-FLG  =  ZERO
                 MOVE  16          TO  ERR-FLG
             END-IF
             MOVE  "R"       TO  EDIT-OPTION OF DSP-MTBUCD(Y)
             MOVE  "C"       TO  EDIT-CURSOR OF DSP-MTBUCD(Y)
*T↓
*            GO              TO  BODY-CHK-B-EXIT
*T↑
         END-IF
       END-IF
     END-IF.
** 重複チェック
     MOVE  ZERO                  TO  FG-DUPLICATE.
** 自頁－自頁の重複チェック
     PERFORM  VARYING WK-IX  FROM 1 BY 1
              UNTIL   WK-IX  > 15
                   OR FG-DUPLICATE = 1
       IF    ( WK-IX             NOT = Y             )
       AND   ( DSP-MTOKCD(Y)     NOT = ZERO          )
       AND   ( DSP-MTOKCD(WK-IX)     = DSP-MTOKCD(Y) )
       AND   ( DSP-MSOKCD(WK-IX)     = DSP-MSOKCD(Y) )
       AND   ( DSP-MTBUCD(WK-IX)     = DSP-MTBUCD(Y) )
               MOVE   1          TO    FG-DUPLICATE
       END-IF

     END-PERFORM.

     IF  FG-DUPLICATE = 1
         IF  ERR-FLG = ZERO
             MOVE  10            TO  ERR-FLG
*T↓
*    DISPLAY "ERR=10"  UPON CONS
*T↑
         END-IF
         MOVE  "R"               TO  EDIT-OPTION OF DSP-MTOKCD(Y)
         MOVE  "C"               TO  EDIT-CURSOR OF DSP-MTOKCD(Y)
         MOVE  "R"               TO  EDIT-OPTION OF DSP-MSOKCD(Y)
*???     MOVE  "C"               TO  EDIT-CURSOR OF DSP-MSOKCD(Y)
         MOVE  "R"               TO  EDIT-OPTION OF DSP-MTBUCD(Y)
*???     MOVE  "C"               TO  EDIT-CURSOR OF DSP-MTBUCD(Y)
         GO TO  BODY-CHK-B-JIKAN-E
     END-IF.

** 自頁－他頁の重複チェック
     PERFORM  VARYING WK-IX  FROM 1 BY 1
              UNTIL   WK-IX  > 2
                   OR FG-DUPLICATE = 1
       IF  WK-IX NOT = P-CNT
           PERFORM  VARYING WK-IX2  FROM 1 BY 1
                    UNTIL   WK-IX2  > 15
             OR ((WK-IX - 1) * 15 + WK-IX2) > CT-TBLMAX
                         OR FG-DUPLICATE = 1
             IF (TBL-MTOKCD(WK-IX WK-IX2) NOT = ZERO
             AND TBL-MTOKCD(WK-IX WK-IX2)  = DSP-MTOKCD(Y)
             AND TBL-MSOKCD(WK-IX WK-IX2)  = DSP-MSOKCD(Y)
             AND TBL-MTBUCD(WK-IX WK-IX2)  = DSP-MTBUCD(Y))
             AND DSP-MTOKCD(Y)            NOT = ZERO
                 MOVE  1     TO  FG-DUPLICATE
             END-IF
           END-PERFORM
       END-IF

     END-PERFORM.

     IF  FG-DUPLICATE = 1
         IF  ERR-FLG = ZERO
             MOVE  10            TO  ERR-FLG
*T↓
*    DISPLAY "ERR=10"  UPON CONS
*T↑
         END-IF
         MOVE  "R"               TO  EDIT-OPTION OF DSP-MTOKCD(Y)
         MOVE  "C"               TO  EDIT-CURSOR OF DSP-MTOKCD(Y)
         MOVE  "R"               TO  EDIT-OPTION OF DSP-SKBUCD(Y)
*???     MOVE  "C"               TO  EDIT-CURSOR OF DSP-SKBUCD(Y)
         GO TO  BODY-CHK-B-JIKAN-E
     END-IF.

 BODY-CHK-B-JIKAN-E.
*T↓
*    DISPLAY "ERR-FLG=" ERR-FLG UPON CONS.
*T↑
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
                UNTIL   IX-JIGMN-GYO1 > 15
          OR ((IX-JIGMN-PG - 1) * 15 + IX-JIGMN-GYO1) > CT-TBLMAX
                     OR FG-DUPLICATE = 1

         IF  TBL-MTOKCD(IX-JIGMN-PG IX-JIGMN-GYO1)
                     NOT = ZERO

             PERFORM  VARYING WK-IX2  FROM 1 BY 1
                      UNTIL   WK-IX2  > 2
                           OR FG-DUPLICATE = 1
               IF  WK-IX2 NOT = IX-JIGMN-PG
                   PERFORM  VARYING WK-IX3  FROM 1 BY 1
                            UNTIL   WK-IX3  > 15
                     OR ((WK-IX2 - 1) * 15 + WK-IX3) > CT-TBLMAX
                                 OR FG-DUPLICATE = 1
                     IF      (TBL-MTOKCD(WK-IX2 WK-IX3)
                                      NOT = ZERO
                         AND TBL-MTOKCD(WK-IX2 WK-IX3)
                         = TBL-MTOKCD(IX-JIGMN-PG IX-JIGMN-GYO1))
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
              UNTIL   Y > 15
       IF      DSP-MTOKCD(Y) NOT = ZERO
           IF  (P-CNT - 1)  * 15 + Y > CT-TBLMAX
               COMPUTE CT-TBLMAX = (P-CNT - 1)  * 15 + Y
           END-IF
       END-IF

       MOVE  DSP-MTOKCD   (Y)    TO  TBL-MTOKCD(P-CNT Y)
       MOVE  DSP-MTOKNM   (Y)    TO  TBL-MTOKNM(P-CNT Y)
       MOVE  DSP-MSOKCD   (Y)    TO  TBL-MSOKCD(P-CNT Y)
       MOVE  DSP-MSOKNM   (Y)    TO  TBL-MSOKNM(P-CNT Y)
       MOVE  DSP-MTBUCD   (Y)    TO  TBL-MTBUCD(P-CNT Y)
       MOVE  DSP-MTBUNM   (Y)    TO  TBL-MTBUNM(P-CNT Y)
       MOVE  DSP-MTOKCD   (Y)    TO  TOK-F01
       PERFORM HTOKMS-READ-SEC
         IF  HTOKMS-INV-FLG = "INV"
             MOVE  ZERO          TO  TBL-JTOKCD(P-CNT Y)
         ELSE
             MOVE  TOK-F52       TO  TBL-JTOKCD(P-CNT Y)
         END-IF

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
         PERFORM  BUMIDOF-DEL-SEC
         PERFORM  BUMIDOF-WRITE-SEC
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
*      WHEN  "F011"
*        COMPUTE  C-CNT = P-CNT - 1
*        IF  C-CNT = ZERO
*            MOVE  3             TO  ERR-FLG
*        ELSE
*            PERFORM  DSP-WORK-SEC
*            COMPUTE  P-CNT = P-CNT - 1
*            PERFORM  INIT-DSP-SEC
*            PERFORM  WORK-DSP-SEC
*            MOVE "2"            TO  PSW
*        END-IF
*      次頁
*      WHEN  "F012"
*        MOVE  P-CNT             TO  S-CNT
*        COMPUTE  C-CNT = P-CNT + 1
*        IF  C-CNT > 2
*            MOVE  4             TO  ERR-FLG
*        ELSE
*            IF  DSP-MTOKCD(12) =  ZERO
*                MOVE 4          TO  ERR-FLG
*            ELSE
*                PERFORM  DSP-WORK-SEC
*                COMPUTE  P-CNT = P-CNT + 1
*                PERFORM  INIT-DSP-SEC
*                PERFORM  WORK-DSP-SEC
*                MOVE  "2"       TO  PSW
*            END-IF
*
*        END-IF
*
       WHEN   OTHER
         MOVE  5                 TO  ERR-FLG
     END-EVALUATE.
*
 DSP-KAKU-EXIT.
     EXIT.
****************************************************************
*             部門間移動マスタ削除位置づけ               *
****************************************************************
 BUMIDOF-DEL-SEC       SECTION.
     MOVE  "JHKIHD-DEL-SEC"      TO  S-NAME.
*
     MOVE  SPACE                 TO  RD-FLG.
     PERFORM BUMIDOF-START-SEC.
     IF  CHK-FLG = "CHK"
         GO TO  BUMIDOF-DEL-EXIT
     END-IF.

     PERFORM BUMIDOF-READ-SEC.
     PERFORM DLT-SEC
             UNTIL  DSP-SKBUCD NOT = BUM-F01
                 OR RD-FLG        = "END".
*
 BUMIDOF-DEL-EXIT.
     EXIT.
****************************************************************
*             部門間移動マスタ削除                       *
****************************************************************
 DLT-SEC               SECTION.
     MOVE  "DLT-SEC"             TO  S-NAME.

     DELETE  BUMIDOF.
     PERFORM  BUMIDOF-READ-SEC.

 DLT-EXIT.
     EXIT.
****************************************************************
*             部門間移動マスタ作成                       *
****************************************************************
 BUMIDOF-WRITE-SEC     SECTION.
     MOVE  "BUMIDOF-WRITE-SEC"   TO  S-NAME.

     PERFORM  SDATE-GET-SEC.

     MOVE  1                     TO  Y.
     PERFORM  VARYING X  FROM 1 BY 1
              UNTIL   X > 2
       PERFORM  VARYING Y  FROM 1 BY 1
                UNTIL   Y > 15
         IF  TBL-MTOKCD(X Y)  NOT =  ZERO
             MOVE  SPACE           TO  BUM-REC
             INITIALIZE  BUM-REC
             MOVE  DSP-SKBUCD      TO  BUM-F01
             MOVE  TBL-MTOKCD(X Y) TO  BUM-F02
             MOVE  TBL-MSOKCD(X Y) TO  BUM-F03

             EVALUATE  TRUE
**             初期データが存在しない。
**             キーが変わっていたら登録
**             X:0 X:Y
**             登録
               WHEN  (    TBL-MTOKCD-INI (X Y) = ZERO
                      AND TBL-MTOKCD(X Y) NOT =
                              TBL-MTOKCD-INI (X Y))
*****************DISPLAY NC"登録" UPON CONS
*                MOVE  TBL-MTOKNM     (X Y)  TO  BUM-F03
*                MOVE  TBL-MSOKCD     (X Y)  TO  BUM-F04
*                MOVE  TBL-MSOKNM     (X Y)  TO  BUM-F05
                 MOVE  TBL-MTBUCD     (X Y)  TO  BUM-F04
                 MOVE  TBL-JTOKCD     (X Y)  TO  BUM-F05
                 MOVE  PAR-BUMON             TO  BUM-F92
                 MOVE  PAR-TANCD             TO  BUM-F93
                 MOVE  SYS-DATE              TO  BUM-F94
                 MOVE  WK-TIME(1:4)          TO  BUM-F95
                 MOVE  SPACE                 TO  BUM-F96
                 MOVE  SPACE                 TO  BUM-F97
                 MOVE  ZERO                  TO  BUM-F98
                 MOVE  ZERO                  TO  BUM-F99

**             変更無し
               WHEN  TBL-MTOKCD (X Y) = TBL-MTOKCD-INI (X Y)
*              AND   TBL-MTOKNM (X Y) = TBL-MTOKNM-INI (X Y)
               AND   TBL-MSOKCD (X Y) = TBL-MSOKCD-INI (X Y)
*              AND   TBL-MSOKNM (X Y) = TBL-MSOKNM-INI (X Y)
               AND   TBL-MTBUCD (X Y) = TBL-MTBUCD-INI (X Y)
*****************DISPLAY NC"変更無し" UPON CONS
*                MOVE  TBL-MTOKNM     (X Y)  TO  BUM-F03
*                MOVE  TBL-MSOKCD     (X Y)  TO  BUM-F04
*                MOVE  TBL-MSOKNM     (X Y)  TO  BUM-F05
                 MOVE  TBL-MTBUCD     (X Y)  TO  BUM-F04
                 MOVE  TBL-JTOKCD     (X Y)  TO  BUM-F05
                 MOVE  TBL-TORBUMON   (X Y)  TO  BUM-F92
                 MOVE  TBL-TORTANCD   (X Y)  TO  BUM-F93
                 MOVE  TBL-TORDATE    (X Y)  TO  BUM-F94
                 MOVE  TBL-TORTIME    (X Y)  TO  BUM-F95
                 MOVE  TBL-KOSBUMON   (X Y)  TO  BUM-F96
                 MOVE  TBL-KOSTANCD   (X Y)  TO  BUM-F97
                 MOVE  TBL-KOSDATE    (X Y)  TO  BUM-F98
                 MOVE  TBL-KOSTIME    (X Y)  TO  BUM-F99
**             修正
               WHEN  OTHER
*****************DISPLAY NC"修正" UPON CONS
*                MOVE  TBL-MTOKNM     (X Y)  TO  BUM-F03
*                MOVE  TBL-MSOKCD     (X Y)  TO  BUM-F04
*                MOVE  TBL-MSOKNM     (X Y)  TO  BUM-F05
                 MOVE  TBL-MTBUCD     (X Y)  TO  BUM-F04
                 MOVE  TBL-JTOKCD     (X Y)  TO  BUM-F05
                 MOVE  TBL-TORBUMON   (X Y)  TO  BUM-F92
                 MOVE  TBL-TORTANCD   (X Y)  TO  BUM-F93
                 MOVE  TBL-TORDATE    (X Y)  TO  BUM-F94
                 MOVE  TBL-TORTIME    (X Y)  TO  BUM-F95
                 MOVE  PAR-BUMON             TO  BUM-F96
                 MOVE  PAR-TANCD             TO  BUM-F97
                 MOVE  SYS-DATE              TO  BUM-F98
                 MOVE  WK-TIME(1:4)          TO  BUM-F99

             END-EVALUATE

             WRITE  BUM-REC
         END-IF
       END-PERFORM

       MOVE  1                   TO  Y
     END-PERFORM.
*
 BUMIDOF-WRITE-EXIT.
     EXIT.
****************************************************************
*             終了処理                               3.0       *
****************************************************************
 END-SEC               SECTION.
     MOVE  "END-SEC"             TO  S-NAME.
*ファイル ＣＬＯＳＥ
     CLOSE  DSPFILE BUMIDOF
            HJYOKEN HTOKMS ZSOKMS.

 END-EXIT.
     EXIT.
*****************<<  SBZ0080I   END PROGRAM  >>******************

```
