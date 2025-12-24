# SHA0031L

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SHA0031L.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    ユーザ　　　　名：　サカタのたね　　　殿                  *
*    システム　　　名：　発注管理システム                      *
*    プログラム　　名：　発注残リスト作成                      *
*    作成者　　　　　：　ＮＡＶ　　　　　                      *
*    作成日　　　　　：　2000.04.26      UPDATE: YYYY.MM.DD    *
*                                                              *
****************************************************************
 IDENTIFICATION      DIVISION.
 PROGRAM-ID.         SHA0031L.
 AUTHOR.             NAV.
 DATE-WRITTEN.       00.04.26.
*
 ENVIRONMENT         DIVISION.
 CONFIGURATION       SECTION.
 SPECIAL-NAMES.
         YA        IS   NIHONGO
         YA-21     IS   YA-21
         YB        IS   YB
     CONSOLE      IS     CONS.
*
 INPUT-OUTPUT        SECTION.
 FILE-CONTROL.
*画面Ｆ
     SELECT   DSPF           ASSIGN               TO  GS-DSPF
                             ORGANIZATION         IS  SEQUENTIAL
                             ACCESS MODE          IS  SEQUENTIAL
                             SYMBOLIC DESTINATION IS "DSP"
                             PROCESSING MODE      IS  DSP-PRO
                             GROUP                IS  DSP-GRP
                             FORMAT               IS  DSP-FMT
                             SELECTED FUNCTION    IS  DSP-FNC
                             FILE STATUS          IS  DSP-STA.

*発注Ｆ（ヘッダ）
     SELECT   HACHEDF        ASSIGN        TO  01-VI-HACHEDL3
                             ORGANIZATION  IS  INDEXED
                             ACCESS MODE   IS  SEQUENTIAL
                             RECORD KEY    IS  HED-F17
                                               HED-F06
                                               HED-F02
                             FILE STATUS   IS  HED-STA.

*発注Ｆ（明細）
     SELECT   HACMEIF        ASSIGN        TO  01-VI-HACMEIL1
                             ORGANIZATION  IS  INDEXED
                             ACCESS MODE   IS  SEQUENTIAL
                             RECORD KEY    IS  BDY-F02
                                               BDY-F03
                             FILE STATUS   IS  BDY-STA.

*条件Ｆ
     SELECT   JYOKEN1        ASSIGN        TO  01-VI-JYOKEN1
                             ORGANIZATION  IS  INDEXED
                             ACCESS MODE   IS  RANDOM
                             RECORD KEY    IS  JYO-F01
                                               JYO-F02
                             FILE STATUS   IS  JYO-STA.

*仕入先Ｍ
     SELECT   ZSHIMS1        ASSIGN        TO  01-VI-ZSHIMS1
                             ORGANIZATION  IS  INDEXED
                             ACCESS MODE   IS  RANDOM
                             RECORD KEY    IS  SHI-F01
                             FILE STATUS   IS  SHI-STA.

*倉庫Ｍ
     SELECT   ZSOKMS1        ASSIGN        TO  01-VI-ZSOKMS1
                             ORGANIZATION  IS  INDEXED
                             ACCESS MODE   IS  RANDOM
                             RECORD KEY    IS  SOK-F01
                             FILE STATUS   IS  SOK-STA.

*商品名称Ｍ
     SELECT   MEIMS1         ASSIGN        TO  01-VI-MEIMS1
                             ORGANIZATION  IS  INDEXED
                             ACCESS MODE   IS  RANDOM
                             RECORD KEY    IS  MEI-F011
                                               MEI-F012
                             FILE STATUS   IS  MEI-STA.

*プリントファイル
     SELECT   PRINTF    ASSIGN  TO   LP-04.
*
**************************************************************
 DATA                DIVISION.
**************************************************************
*=============================================================
 FILE                SECTION.
*=============================================================
*画面Ｆ
 FD  DSPF.
     COPY     FHA00301  OF  XMDLIB
     JOINING  DSP      AS  PREFIX.
*発注Ｆ（ヘッダ）
 FD  HACHEDF.
     COPY     HACHEDF  OF  XFDLIB
     JOINING  HED      AS  PREFIX.
*発注Ｆ（明細）
 FD  HACMEIF.
     COPY     HACMEIF  OF  XFDLIB
     JOINING  BDY      AS  PREFIX.
*条件Ｆ
 FD  JYOKEN1.
     COPY     HJYOKEN  OF  XFDLIB
     JOINING  JYO      AS  PREFIX.
*仕入先Ｍ
 FD  ZSHIMS1.
     COPY     ZSHIMS   OF  XFDLIB
     JOINING  SHI      AS  PREFIX.
*倉庫Ｍ
 FD  ZSOKMS1.
     COPY     ZSOKMS   OF  XFDLIB
     JOINING  SOK      AS  PREFIX.
*商品名称Ｍ
 FD  MEIMS1.
     COPY     HMEIMS   OF  XFDLIB
     JOINING  MEI      AS  PREFIX.
*プリントファイル
 FD    PRINTF    LINAGE  IS  66.
 01    P-REC                 PIC  X(200).
*
*=============================================================
 WORKING-STORAGE     SECTION.
*=============================================================
*画面制御用
 01  DSP-CONTROL.
     03  DSP-PRO             PIC  X(02).
     03  DSP-GRP             PIC  X(08).
     03  DSP-FMT             PIC  X(08).
     03  DSP-FNC             PIC  X(04).
*ステータス
 01  STA-AREA.
     03  DSP-STA             PIC  X(02).
     03  HED-STA             PIC  X(02).
     03  BDY-STA             PIC  X(02).
     03  SHI-STA             PIC  X(02).
     03  JYO-STA             PIC  X(02).
     03  SOK-STA             PIC  X(02).
     03  MEI-STA             PIC  X(02).
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
*メッセージテーブル
 01  MSG-TBL.
     03  MSG-NO01            PIC  N(20)  VALUE
            NC"誤ったＰＦキーが押されました".
     03  MSG-NO02            PIC  N(20)  VALUE
            NC"仕入先コードが違います".
     03  MSG-NO03            PIC  N(20)  VALUE
            NC"開始が終了を越えています".
     03  MSG-NO04            PIC  N(20)  VALUE
            NC"入荷予定日が違います".
     03  MSG-NO05            PIC  N(20)  VALUE
            NC"対象データはありません".
     03  MSG-NO06            PIC  N(20)  VALUE
            NC"対象データ抽出中です".
     03  MSG-NO07            PIC  N(20)  VALUE
            NC"倉庫コードが違います".
 01  TBL-MSG-R   REDEFINES    MSG-TBL.
     03  TBL-MSG             PIC  N(20)  OCCURS  7   TIMES.
*ＰＦキー
 01  PFK-TBL.
     03  PFK-NO01            PIC  N(30)  VALUE
            NC"_取消　_終了".
     03  PFK-NO01            PIC  N(30)  VALUE
            NC"_取消　_終了　_項目戻り".
 01  TBL-PFK-R       REDEFINES    PFK-TBL.
     03  TBL-PFK             PIC  N(30)  OCCURS  2   TIMES.
*
 01  WK-SDATE                PIC  9(08).
 01  WK-SDATE-R   REDEFINES   WK-SDATE.
     03  FILLER              PIC  X(02).
     03  WK-SYMD.
       05  WK-SYY            PIC  9(02).
       05  WK-SMM            PIC  9(02).
       05  WK-SDD            PIC  9(02).
 01  WK-EDATE                PIC  9(08).
 01  WK-EDATE-R   REDEFINES   WK-EDATE.
     03  FILLER              PIC  X(02).
     03  WK-EYMD.
       05  WK-EYY            PIC  9(02).
       05  WK-EMM            PIC  9(02).
       05  WK-EDD            PIC  9(02).
*
 01  OLD-KEY.
     03  OLD-F02             PIC  9(07).
 01  NEW-KEY.
     03  NEW-F02             PIC  9(07).
*
 01  FLG-AREA.
     03  MAIN-FLG            PIC  9(02)  VALUE  ZERO.
     03  ERR-FLG             PIC  9(02)  VALUE  ZERO.
     03  PFK-FLG             PIC  9(02)  VALUE  ZERO.
     03  HAC-FLG             PIC  9(01)  VALUE  ZERO.
     03  SIR-FLG             PIC  9(01)  VALUE  ZERO.
     03  DEN-FLG             PIC  9(01)  VALUE  ZERO.
*
 01  CNT-AREA.
     03  PAGE-CNT            PIC  9(07)  VALUE  ZERO.
     03  LINE-CNT            PIC  9(02)  VALUE  ZERO.
     03  MAX-LINE            PIC  9(02)  VALUE  62.
     03  IN-CNT              PIC  9(07)  VALUE  ZERO.
     03  OT-CNT              PIC  9(07)  VALUE  ZERO.
*
 01  WORK-AREA.
     03  WK-SIRCD            PIC  X(08)  VALUE  SPACE.
     03  WK-DENNO            PIC  9(07)  VALUE  ZERO.
****  見出し行０             ****
 01  MIDASI0        CHARACTER     TYPE   IS   NIHONGO.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  FILLER              PIC  X(08)  VALUE  "SHA0031L".
     02  FILLER              PIC  X(25)  VALUE  SPACE.
     02  FILLER              PIC  N(19)  VALUE
         NC"＊＊＊　発　注　残　リ　ス　ト　＊＊＊".
     02  H-TOKHAN            PIC  N(08).
     02  FILLER              PIC  X(08)  VALUE  SPACE.
     02  FILLER              PIC  N(03)  VALUE  NC"処理日".
     02  FILLER              PIC  X(01)  VALUE  ":".
     02  H-YY                PIC  9(04).
     02  FILLER              PIC  X(1)   VALUE  ".".
     02  H-MM                PIC  Z9.
     02  FILLER              PIC  X(1)   VALUE  ".".
     02  H-DD                PIC  Z9.
     02  FILLER              PIC  X(3)   VALUE  SPACE.
     02  FILLER              PIC  N(01)  VALUE  NC"頁".
     02  FILLER              PIC  X(01)  VALUE  ":".
     02  PAGE-SUU            PIC  ZZZ9.
****  見出し行１             ****
 01  MIDASI1        CHARACTER     TYPE   IS   NIHONGO.
     02  FILLER              PIC  X(03)  VALUE  SPACE.
     02  FILLER              PIC  N(03)  VALUE  NC"倉庫：".
     02  H1-SOKCD            PIC  X(02)  VALUE  SPACE.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  H1-SOKNM            PIC  N(18).
****  見出し行２             ****
 01  MIDASI2        CHARACTER     TYPE   IS   NIHONGO.
     02  FILLER              PIC  X(02)  VALUE  SPACE.
     02  FILLER              PIC  N(03)  VALUE
         NC"仕入先".
     02  FILLER              PIC  X(31)  VALUE  SPACE.
     02  FILLER              PIC  N(03)  VALUE
         NC"発注_".
     02  FILLER              PIC  X(02)  VALUE  SPACE.
     02  FILLER              PIC  N(01)  VALUE
         NC"行".
     02  FILLER              PIC  X(02)  VALUE  SPACE.
     02  FILLER              PIC  N(05)  VALUE
         NC"入荷予定日".
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  FILLER              PIC  N(05)  VALUE
         NC"商品コード".
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  FILLER              PIC  N(03)  VALUE
         NC"_　番".
     02  FILLER              PIC  X(07)  VALUE  SPACE.
     02  FILLER              PIC  N(03)  VALUE
         NC"発注数".
     02  FILLER              PIC  X(07)  VALUE  SPACE.
     02  FILLER              PIC  N(03)  VALUE
         NC"入荷数".
     02  FILLER              PIC  X(07)  VALUE  SPACE.
     02  FILLER              PIC  N(03)  VALUE
         NC"発注残".
     02  FILLER              PIC  X(04)  VALUE  SPACE.
     02  FILLER              PIC  N(02)  VALUE
         NC"備考".
****  見出し行３             ****
 01  MIDASI3        CHARACTER     TYPE   IS   NIHONGO.
     02  FILLER              PIC  X(21)  VALUE  SPACE.
     02  FILLER              PIC  N(02)  VALUE
         NC"メモ".
     02  FILLER              PIC  X(05)  VALUE  SPACE.
     02  FILLER              PIC  N(03)  VALUE
         NC"摘要名".
     02  FILLER              PIC  X(26)  VALUE  SPACE.
     02  FILLER              PIC  N(03)  VALUE
         NC"商品名".
     02  FILLER              PIC  X(41)  VALUE  SPACE.
****  見出し行４             ****
 01  MIDASI4        CHARACTER     TYPE   IS   NIHONGO.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  FILLER              PIC  N(67)  VALUE  ALL NC"─".
****  明細行１               ****
 01  MEISAI1        CHARACTER     TYPE   IS   YB.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  PRT-BSIRCD          PIC  X(08)B.
     02  PRT-BSIRNM          PIC  N(18)B.
     02  PRT-DENNO           PIC  9(07)B.
     02  PRT-GYONO           PIC  9(02)BBB.
     02  PRT-NYU-Y           PIC  9(02).
     02  PRT-NYU-1           PIC  X(01).
     02  PRT-NYU-M           PIC  9(02).
     02  PRT-NYU-2           PIC  X(01).
     02  PRT-NYU-D           PIC  9(02)BB.
     02  PRT-SHOCD           PIC  X(08)BB.
     02  PRT-TANABN          PIC  X(08)B.
     02  PRT-HACSU           PIC  ZZ,ZZZ,ZZ9.99.
     02  PRT-NYUSU           PIC  ZZ,ZZZ,ZZ9.99.
     02  PRT-ZANSU           PIC  ZZ,ZZZ,ZZ9.99BB.
     02  PRT-BIKOU           PIC  X(10).
****  明細行２               ****
 01  MEISAI2        CHARACTER     TYPE   IS   YB.
*****02  FILLER              PIC  X(62)  VALUE  SPACE.
     02  FILLER              PIC  X(20)  VALUE  SPACE.
     02  PRT-MEMO-1          PIC  X(01).
     02  PRT-MEMO            PIC  X(07).
     02  PRT-MEMO-2          PIC  X(01).
     02  PRT-TEKIYO-1        PIC  X(01).
     02  PRT-TEKIYO1         PIC  X(15).
     02  PRT-TEKIYO2         PIC  X(15).
     02  PRT-TEKIYO-2        PIC  X(01).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  PRT-SHONM1          PIC  N(15).
     02  PRT-SHONM2          PIC  N(15).
*
*メッセージ情報
 01  MSG-AREA.
     03  MSG-ABEND1.
         05  FILLER          PIC  X(12)  VALUE  "### SHA0031L".
         05  FILLER          PIC  X(11)  VALUE  "  ABEND ###".
     03  MSG-ABEND2.
         05  FILLER          PIC  X(04)  VALUE  "### ".
         05  ERR-FL-ID       PIC  X(08).
         05  FILLER          PIC  X(04)  VALUE  " ST-".
         05  ERR-STCD        PIC  X(02).
         05  FILLER          PIC  X(04)  VALUE  " ###".
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
*=============================================================
 LINKAGE             SECTION.
*=============================================================
   01  LINK-SOKCD            PIC  X(02).
   01  LINK-DSOKCD           PIC  X(02).
******************************************************************
 PROCEDURE               DIVISION      USING    LINK-SOKCD
                                                LINK-DSOKCD.
******************************************************************
 DECLARATIVES.
*画面Ｆ
 DSP-ERR-SEC        SECTION.
     USE      AFTER  EXCEPTION   PROCEDURE       DSPF.
     MOVE    "DSPF"        TO    ERR-FL-ID.
     MOVE     DSP-STA      TO    ERR-STCD.
     DISPLAY  MSG-ABEND1   UPON  CONS.
     DISPLAY  MSG-ABEND2   UPON  CONS.
     STOP     RUN.
*発注Ｆ（ヘッド）
 HED-ERR-SEC        SECTION.
     USE      AFTER  EXCEPTION   PROCEDURE       HACHEDF.
     MOVE    "HACHEDL3"    TO    ERR-FL-ID.
     MOVE     HED-STA      TO    ERR-STCD.
     DISPLAY  MSG-ABEND1   UPON  CONS.
     DISPLAY  MSG-ABEND2   UPON  CONS.
     STOP     RUN.
*発注Ｆ（明細）
 BDY-ERR-SEC        SECTION.
     USE      AFTER  EXCEPTION   PROCEDURE       HACMEIF.
     MOVE    "HACMEIL1"    TO    ERR-FL-ID.
     MOVE     BDY-STA      TO    ERR-STCD.
     DISPLAY  MSG-ABEND1   UPON  CONS.
     DISPLAY  MSG-ABEND2   UPON  CONS.
     STOP     RUN.
*条件Ｆ
 JYO-ERR-SEC        SECTION.
     USE      AFTER  EXCEPTION   PROCEDURE       JYOKEN1.
     MOVE    "JYOKEN1"     TO    ERR-FL-ID.
     MOVE     JYO-STA      TO    ERR-STCD.
     DISPLAY  MSG-ABEND1   UPON  CONS.
     DISPLAY  MSG-ABEND2   UPON  CONS.
     STOP     RUN.
*仕入先Ｍ
 SHI-ERR-SEC        SECTION.
     USE      AFTER  EXCEPTION   PROCEDURE       ZSHIMS1.
     MOVE    "ZSHIMS1"     TO    ERR-FL-ID.
     MOVE     SHI-STA      TO    ERR-STCD.
     DISPLAY  MSG-ABEND1   UPON  CONS.
     DISPLAY  MSG-ABEND2   UPON  CONS.
     STOP     RUN.
*倉庫Ｍ
 SOK-ERR-SEC        SECTION.
     USE      AFTER  EXCEPTION   PROCEDURE       ZSOKMS1.
     MOVE    "ZSOKMS1"     TO    ERR-FL-ID.
     MOVE     SOK-STA      TO    ERR-STCD.
     DISPLAY  MSG-ABEND1   UPON  CONS.
     DISPLAY  MSG-ABEND2   UPON  CONS.
     MOVE     4000         TO    PROGRAM-STATUS.
     STOP     RUN.
*商品名称Ｍ
 MEI-ERR-SEC        SECTION.
     USE      AFTER  EXCEPTION   PROCEDURE       MEIMS1.
     MOVE    "MEIMS1"      TO    ERR-FL-ID.
     MOVE     MEI-STA      TO    ERR-STCD.
     DISPLAY  MSG-ABEND1   UPON  CONS.
     DISPLAY  MSG-ABEND2   UPON  CONS.
     MOVE     4000         TO    PROGRAM-STATUS.
     STOP     RUN.
 END  DECLARATIVES.
*=============================================================
*               コントロール
*=============================================================
 CONTROL-SEC         SECTION.
     DISPLAY  "**  SHA0031L   START  **"   UPON  CONS.
*
     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC    UNTIL  MAIN-FLG  =  99.
     PERFORM  END-SEC.
*
     DISPLAY  "**  SHA0031L    END   **"   UPON  CONS.
     STOP  RUN.
 CONTROL-EXIT.
     EXIT.
*=============================================================
*               初期処理
*=============================================================
 INIT-SEC            SECTION.
*ファイル ＯＰＥＮ
     OPEN     I-O         DSPF.
     OPEN     INPUT       HACHEDF  HACMEIF.
     OPEN     INPUT       ZSHIMS1.
     OPEN     INPUT       JYOKEN1.
     OPEN     INPUT       ZSOKMS1.
     OPEN     INPUT       MEIMS1.
     OPEN     OUTPUT      PRINTF.
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
     MOVE      SYS-DATE(1:4)      TO   HEN-DATE-YYYY H-YY.
     MOVE      SYS-DATE(5:2)      TO   HEN-DATE-MM   H-MM.
     MOVE      SYS-DATE(7:2)      TO   HEN-DATE-DD   H-DD.
*システム日付取得
     ACCEPT    WK-TIME          FROM   TIME.
*画面表示時刻編集
     MOVE      WK-TIME(1:2)       TO   HEN-TIME-HH.
     MOVE      WK-TIME(3:2)       TO   HEN-TIME-MM.
     MOVE      WK-TIME(5:2)       TO   HEN-TIME-SS.
*特販部名称編集
     MOVE      SPACE              TO   JYO-REC.
     INITIALIZE                        JYO-REC.
     MOVE     "99"                TO   JYO-F01.
     MOVE     "BUMON"             TO   JYO-F02.
     READ      JYOKEN1
       INVALID KEY
             MOVE NC"＊＊＊＊＊＊"     TO   HEN-TOKHAN
       NOT INVALID KEY
             MOVE JYO-F03              TO   HEN-TOKHAN
     END-READ.
     MOVE      HEN-TOKHAN-AREA    TO   H-TOKHAN.
*初期画面表示へ
     MOVE  1              TO  MAIN-FLG.
 INIT-EXIT.
     EXIT.
*=============================================================
*                メイン処理
*=============================================================
 MAIN-SEC            SECTION.
     EVALUATE  MAIN-FLG
*初期画面表示
         WHEN   1      PERFORM  DSP-INIT-SEC
*ＢＯＤＹ部入力
         WHEN   2      PERFORM  DSP-BODY-SEC
*確認入力
         WHEN   3      PERFORM  DSP-KAKU-SEC
*更新処理
         WHEN   4      PERFORM  PRINT-SEC
     END-EVALUATE.
 MAIN-EXIT.
     EXIT.
*=============================================================
*      3.0        終了処理
*=============================================================
 END-SEC             SECTION.
*ファイル ＣＬＯＳＥ
     CLOSE      DSPF.
     CLOSE      HACHEDF   HACMEIF.
     CLOSE      ZSHIMS1.
     CLOSE      JYOKEN1.
     CLOSE      ZSOKMS1.
     CLOSE      MEIMS1.
     CLOSE      PRINTF.
*
     DISPLAY "* HACMEIF (IN)=" IN-CNT   " *" UPON CONS.
     DISPLAY "* HACMEIF (OT)=" OT-CNT   " *" UPON CONS.
     DISPLAY "* PRINTF(PAGE)=" PAGE-CNT " *" UPON CONS.
 END-EXIT.
     EXIT.
*=============================================================
*                画面初期表示処理
*=============================================================
 DSP-INIT-SEC       SECTION.
*初期画面の処理
     MOVE  SPACE        TO  DSP-CONTROL.
     MOVE "FHA00301"    TO  DSP-FMT.
     MOVE  SPACE        TO  DSP-FHA00301.
     PERFORM  OPT-CLR-SEC.
*倉庫
     IF  LINK-DSOKCD   NOT =  "01"  AND  "88"
         MOVE    LINK-SOKCD     TO   SOK-F01
                                     DSP-SSOKCD   DSP-ESOKCD
         READ    ZSOKMS1
             INVALID KEY
                 CONTINUE
             NOT INVALID KEY
                 MOVE  SOK-F02  TO   DSP-SSOKNM   DSP-ESOKNM
         END-READ
     END-IF.
*入荷予定日
     MOVE  WK-Y         TO  DSP-SNYUYY   DSP-ENYUYY.
     MOVE  WK-M         TO  DSP-SNYUMM   DSP-ENYUMM.
     MOVE  WK-D         TO  DSP-SNYUDD   DSP-ENYUDD.
*ＢＯＤＹ部入力へ
     MOVE  2            TO  MAIN-FLG.
 DSP-INIT-EXIT.
     EXIT.
*=============================================================
*                ＢＯＤＹ部入力処理
*=============================================================
 DSP-BODY-SEC       SECTION.
*画面表示
     MOVE      1        TO  PFK-FLG.
     PERFORM   DSP-WT-SEC.
*倉庫ＣＤプロテクト
     IF  LINK-DSOKCD  NOT =  "01"  AND  "88"
       MOVE  "X"          TO  EDIT-STATUS  OF  DSP-SSOKCD
                              EDIT-STATUS  OF  DSP-ESOKCD
     END-IF.
*画面入力
     MOVE      "BODY"   TO  DSP-GRP.
     PERFORM   DSP-RD-SEC.
*ＰＦ判定
     EVALUATE  DSP-FNC
          WHEN "E000"
                          PERFORM  CHK-BODY-SEC
                          IF    ERR-FLG = ZERO
                                MOVE    3    TO   MAIN-FLG
                          END-IF
          WHEN "F004"
                          MOVE  1      TO  MAIN-FLG
          WHEN "F005"
                          MOVE  99     TO  MAIN-FLG
          WHEN "F006"
                          CONTINUE
          WHEN OTHER
                          MOVE  1      TO  ERR-FLG
     END-EVALUATE.
 DSP-BODY-EXIT.
     EXIT.
*=============================================================
*                ＢＯＤＹ部入力チェック
*=============================================================
 CHK-BODY-SEC   SECTION.
     IF  LINK-DSOKCD  NOT =  "01"  AND  "88"
         GO      TO              CHK-BODY-500
     END-IF.
*開始倉庫
     MOVE    SPACE               TO   DSP-SSOKNM.
     IF  DSP-SSOKCD  NOT =  SPACE
         MOVE    DSP-SSOKCD      TO   SOK-F01
         READ    ZSOKMS1
             INVALID KEY
                 MOVE    "R"     TO   EDIT-OPTION  OF DSP-SSOKCD
                 MOVE    "C"     TO   EDIT-CURSOR  OF DSP-SSOKCD
                 IF   ERR-FLG   =    ZERO
                    MOVE    7    TO   ERR-FLG
                 END-IF
             NOT INVALID KEY
                 MOVE   SOK-F02  TO   DSP-SSOKNM
         END-READ
     END-IF.
*終了倉庫
     IF  DSP-ESOKCD      =  SPACE
         MOVE    "99"            TO   DSP-ESOKCD
     END-IF.
     MOVE    SPACE               TO   DSP-ESOKNM.
     IF  DSP-ESOKCD  NOT =  "99"
         MOVE    DSP-ESOKCD      TO   SOK-F01
         READ    ZSOKMS1
             INVALID KEY
                 MOVE    "R"     TO   EDIT-OPTION  OF DSP-ESOKCD
                 MOVE    "C"     TO   EDIT-CURSOR  OF DSP-ESOKCD
                 IF   ERR-FLG   =    ZERO
                    MOVE    7    TO   ERR-FLG
                 END-IF
             NOT INVALID KEY
                 MOVE   SOK-F02  TO   DSP-ESOKNM
         END-READ
     END-IF.
     IF  ERR-FLG     =  ZERO
     AND DSP-SSOKCD  >  DSP-ESOKCD
         IF  ERR-FLG  =  ZERO
             MOVE   3        TO  ERR-FLG
         END-IF
         MOVE  "C"           TO  EDIT-CURSOR  OF  DSP-SSOKCD
         MOVE  "R"           TO  EDIT-OPTION  OF  DSP-SSOKCD
         MOVE  "R"           TO  EDIT-OPTION  OF  DSP-ESOKCD
     END-IF.
*
 CHK-BODY-500.
*開始仕入先ＣＤ
     MOVE   SPACE                TO  DSP-SSIRNM.
     IF  DSP-SSIRCD   =   SPACE
         MOVE    SPACE           TO  DSP-SSIRCD
     END-IF.
     IF  DSP-SSIRCD  NOT  =   SPACE
         MOVE   DSP-SSIRCD       TO  SHI-F01
         READ   ZSHIMS1
           INVALID  KEY
             IF  ERR-FLG  =  ZERO
                 MOVE   2        TO  ERR-FLG
             END-IF
             MOVE  "R"           TO  EDIT-OPTION  OF  DSP-SSIRCD
             MOVE  "C"           TO  EDIT-CURSOR  OF  DSP-SSIRCD
           NOT INVALID
             MOVE  SHI-F02       TO  DSP-SSIRNM
         END-READ
     END-IF.
*終了仕入先ＣＤ
     MOVE   SPACE                TO  DSP-ESIRNM.
     IF  DSP-ESIRCD   =   SPACE
         MOVE   "99999999"       TO  DSP-ESIRCD
     END-IF.
     IF  DSP-ESIRCD   NOT =  "99999999"
         MOVE   DSP-ESIRCD       TO  SHI-F01
         READ   ZSHIMS1
           INVALID  KEY
             IF  ERR-FLG  =  ZERO
                 MOVE   2        TO  ERR-FLG
             END-IF
             MOVE  "R"           TO  EDIT-OPTION  OF  DSP-ESIRCD
             MOVE  "C"           TO  EDIT-CURSOR  OF  DSP-ESIRCD
           NOT INVALID
             MOVE  SHI-F02       TO  DSP-ESIRNM
         END-READ
     END-IF.
     IF  DSP-SSIRNM  NOT =  SPACE
     AND DSP-ESIRNM  NOT =  SPACE
     AND DSP-SSIRCD  >  DSP-ESIRCD
         IF  ERR-FLG  =  ZERO
             MOVE   3        TO  ERR-FLG
         END-IF
         MOVE  "C"           TO  EDIT-CURSOR  OF  DSP-SSIRCD
         MOVE  "R"           TO  EDIT-OPTION  OF  DSP-SSIRCD
         MOVE  "R"           TO  EDIT-OPTION  OF  DSP-ESIRCD
     END-IF.
*開始入荷予定日
     MOVE     "1"                 TO   LINK-IN-KBN.
     MOVE     DSP-SNYUYY          TO   WK-SYY.
     MOVE     DSP-SNYUMM          TO   WK-SMM.
     MOVE     DSP-SNYUDD          TO   WK-SDD.
     MOVE     WK-SYMD             TO   LINK-IN-YMD6.
     CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD.
     IF        LINK-OUT-RET  =  ZERO
         MOVE     "3"             TO   LINK-IN-KBN
         MOVE     WK-SYMD         TO   LINK-IN-YMD6
         CALL     "SKYDTCKB"   USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD
         MOVE     LINK-OUT-YMD    TO   WK-SDATE
     ELSE
         MOVE     ZERO            TO   WK-SDATE
         IF  ERR-FLG  =  ZERO
             MOVE   4        TO  ERR-FLG
         END-IF
         MOVE  "R"           TO  EDIT-OPTION  OF  DSP-SNYUYY
                                 EDIT-OPTION  OF  DSP-SNYUMM
                                 EDIT-OPTION  OF  DSP-SNYUDD
         MOVE  "C"           TO  EDIT-CURSOR  OF  DSP-SNYUYY
     END-IF.
*終了入荷予定日
     MOVE     "1"                 TO   LINK-IN-KBN.
     MOVE     DSP-ENYUYY          TO   WK-EYY.
     MOVE     DSP-ENYUMM          TO   WK-EMM.
     MOVE     DSP-ENYUDD          TO   WK-EDD.
     MOVE     WK-EYMD             TO   LINK-IN-YMD6.
     CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD.
     IF        LINK-OUT-RET  =  ZERO
         MOVE     "3"             TO   LINK-IN-KBN
         MOVE     WK-EYMD         TO   LINK-IN-YMD6
         CALL     "SKYDTCKB"   USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD
         MOVE     LINK-OUT-YMD    TO   WK-EDATE
     ELSE
         MOVE     ZERO            TO   WK-EDATE
         IF  ERR-FLG  =  ZERO
             MOVE   4        TO  ERR-FLG
         END-IF
         MOVE  "R"           TO  EDIT-OPTION  OF  DSP-ENYUYY
                                 EDIT-OPTION  OF  DSP-ENYUMM
                                 EDIT-OPTION  OF  DSP-ENYUDD
         MOVE  "C"           TO  EDIT-CURSOR  OF  DSP-ENYUYY
     END-IF.
     IF        WK-SDATE  NOT =  ZERO   AND
               WK-EDATE  NOT =  ZERO   AND
               WK-SDATE     >   WK-EDATE
         IF  ERR-FLG  =  ZERO
             MOVE   3        TO  ERR-FLG
         END-IF
         MOVE  "R"           TO  EDIT-OPTION  OF  DSP-SNYUYY
                                 EDIT-OPTION  OF  DSP-SNYUMM
                                 EDIT-OPTION  OF  DSP-SNYUDD
                                 EDIT-OPTION  OF  DSP-ENYUYY
                                 EDIT-OPTION  OF  DSP-ENYUMM
                                 EDIT-OPTION  OF  DSP-ENYUDD
         MOVE  "C"           TO  EDIT-CURSOR  OF  DSP-SNYUYY
     END-IF.
 CHK-BODY-EXIT.
     EXIT.
*=============================================================
*      2.2       確認入力処理
*=============================================================
 DSP-KAKU-SEC       SECTION.
*画面表示
     MOVE  2                     TO  PFK-FLG.
     PERFORM  DSP-WT-SEC.
*画面入力
     MOVE     "KAKU"            TO  DSP-GRP.
     PERFORM  DSP-RD-SEC.
*ＰＦ判定
     EVALUATE  DSP-FNC
         WHEN "E000"
                          PERFORM  CHK-KAKU-SEC
         WHEN "F004"
                          MOVE  1       TO   MAIN-FLG
         WHEN "F005"
                          MOVE  99     TO  MAIN-FLG
         WHEN "F006"
                          MOVE  2       TO   MAIN-FLG
         WHEN OTHER
                          MOVE  1       TO   ERR-FLG
     END-EVALUATE.
*
 DSP-KAKU-EXIT.
     EXIT.
*=============================================================
*                確認入力チェック（対象データ存在ＣＨＫ）
*=============================================================
 CHK-KAKU-SEC   SECTION.
     MOVE  ZERO      TO      ERR-FLG.
*発注Ｆスタート
     MOVE  ZERO              TO  HAC-FLG.
     MOVE  DSP-SSOKCD        TO  HED-F17.
     MOVE  DSP-SSIRCD        TO  HED-F06.
     MOVE  ZERO              TO  HED-F02.
     START HACHEDF   KEY >=      HED-F17
                                 HED-F06
                                 HED-F02
         INVALID
            MOVE   9         TO  HAC-FLG
         NOT  INVALID
*発注Ｆ読み込み（ヘッダ）
            PERFORM  HED-RD-SEC
     END-START.
*発注Ｆ読み込み（明細）
     IF  HAC-FLG  =  0
            PERFORM  DATA-GET-SEC
     END-IF.
*対象データ存在ＣＨＫ
     IF  HAC-FLG  =  9
         MOVE    5    TO    ERR-FLG
     END-IF.
*
     IF  ERR-FLG  =  ZERO
         MOVE    6    TO    ERR-FLG
         PERFORM  DSP-WT-SEC
         MOVE    4    TO    MAIN-FLG
     ELSE
*****    MOVE    2    TO    MAIN-FLG
         MOVE    99   TO    MAIN-FLG
     END-IF.
 CHK-KAKU-EXIT.
     EXIT.
*=============================================================
*                発注Ｆリード処理（ヘッダ）
*=============================================================
 HED-RD-SEC      SECTION.
*リード
     READ  HACHEDF    NEXT
         AT   END
           MOVE   9        TO  HAC-FLG
           GO              TO  HED-RD-EXIT
     END-READ.
*倉庫ＣＤ
     IF   HED-F17    >   DSP-ESOKCD
           MOVE   9        TO  HAC-FLG
           GO              TO  HED-RD-EXIT
     END-IF.
*伝票区分
     IF   HED-F01  NOT =  50
         GO                TO  HED-RD-SEC
     END-IF.
*枝番，完了区分，取消フラグ
     IF  (HED-F03  NOT =  ZERO)
     OR  (HED-F04  NOT =  ZERO)
     OR  (HED-F24  NOT =  ZERO)
         GO                TO  HED-RD-SEC
     END-IF.
*仕入先ＣＤ
     IF   HED-F06   <  DSP-SSIRCD
     OR   HED-F06   >  DSP-ESIRCD
         GO                TO  HED-RD-SEC
     END-IF.
*納入予定日
     IF   HED-F11   <  WK-SDATE    OR
          HED-F11   >  WK-EDATE
         GO                TO  HED-RD-SEC
     END-IF.
*発注ＦＬＧ
     IF   HED-F25  NOT  =  1
         GO                TO  HED-RD-SEC
     END-IF.
 HED-RD-EXIT.
     EXIT.
*=============================================================
*                発注Ｆスタート処理（明細）
*=============================================================
 BDY-START-SEC   SECTION.
     MOVE  HED-F02           TO  OLD-F02.
*
     MOVE  HED-F02           TO  BDY-F02.
     MOVE  ZERO              TO  BDY-F03.
     START HACMEIF   KEY >=      BDY-F02
                                 BDY-F03
         INVALID
            MOVE   HIGH-VALUE    TO   NEW-KEY
         NOT  INVALID
*発注Ｆ読み込み（明細）
            PERFORM  BDY-RD-SEC
     END-START.
 BDY-START-EXIT.
     EXIT.
*=============================================================
*                発注Ｆリード処理（明細）
*=============================================================
 BDY-RD-SEC      SECTION.
*リード
     READ  HACMEIF    NEXT
         AT   END
           MOVE   HIGH-VALUE   TO   NEW-KEY
           GO              TO  BDY-RD-EXIT
     END-READ.
     ADD   1               TO       IN-CNT.
     MOVE  BDY-F02         TO       NEW-F02.
*伝票番号
     IF   HED-F02  NOT =  BDY-F02
         GO                TO  BDY-RD-EXIT
     END-IF.
*完了区分
     IF   BDY-F05  =  1
         GO                TO  BDY-RD-SEC
     END-IF.
*発注残
     IF   BDY-F09  =  BDY-F10
         GO                TO  BDY-RD-SEC
     END-IF.
     ADD   1               TO         OT-CNT.
 BDY-RD-EXIT.
     EXIT.
*=============================================================
*             発注ファイルの対象データを探す
*=============================================================
 DATA-GET-SEC   SECTION.
     PERFORM    BDY-START-SEC.
*
     IF    OLD-KEY  NOT =  NEW-KEY
           PERFORM   HED-RD-SEC
           IF    HAC-FLG   =   0
                 GO       TO      DATA-GET-SEC
           END-IF
     END-IF.
 DATA-GET-EXIT.
     EXIT.
*=============================================================
*                画面表示処理
*=============================================================
 DSP-WT-SEC       SECTION.
*ＰＦキー設定
     MOVE  TBL-PFK(PFK-FLG)      TO  DSP-PFKEY.
*エラー設定
     IF    ERR-FLG   NOT = ZERO
       MOVE  TBL-MSG(ERR-FLG)    TO  DSP-ERRMSG
     END-IF.
     MOVE     HEN-DATE           TO  DSP-SDATE.
     MOVE     HEN-TIME           TO  DSP-STIME.
     MOVE     HEN-TOKHAN-AREA    TO  DSP-TOKHAN.
*画面表示
     MOVE "SCREEN"               TO  DSP-GRP.
     MOVE  SPACE                 TO  DSP-PRO.
     WRITE DSP-FHA00301.
*エラークリア
     MOVE  ZERO                  TO  ERR-FLG.
     MOVE  SPACE                 TO  DSP-ERRMSG.
 DSP-WT-EXIT.
     EXIT.
*=============================================================
*                画面読込処理
*=============================================================
 DSP-RD-SEC        SECTION.
     MOVE "NE"                   TO  DSP-PRO.
     READ  DSPF.
     PERFORM       OPT-CLR-SEC.
 DSP-RD-EXIT.
     EXIT.
*=============================================================
*                項目属性初期化
*=============================================================
 OPT-CLR-SEC        SECTION.
     MOVE  SPACE         TO  EDIT-CURSOR  OF  DSP-SSOKCD
                             EDIT-CURSOR  OF  DSP-ESOKCD
                             EDIT-CURSOR  OF  DSP-SSIRCD
                             EDIT-CURSOR  OF  DSP-ESIRCD
                             EDIT-CURSOR  OF  DSP-SNYUYY
                             EDIT-CURSOR  OF  DSP-SNYUMM
                             EDIT-CURSOR  OF  DSP-SNYUDD
                             EDIT-CURSOR  OF  DSP-SNYUYY
                             EDIT-CURSOR  OF  DSP-ENYUMM
                             EDIT-CURSOR  OF  DSP-ENYUDD.
     MOVE "M"            TO  EDIT-OPTION  OF  DSP-SSOKCD
                             EDIT-OPTION  OF  DSP-ESOKCD
                             EDIT-OPTION  OF  DSP-SSIRCD
                             EDIT-OPTION  OF  DSP-ESIRCD
                             EDIT-OPTION  OF  DSP-SNYUYY
                             EDIT-OPTION  OF  DSP-SNYUMM
                             EDIT-OPTION  OF  DSP-SNYUDD
                             EDIT-OPTION  OF  DSP-ENYUYY
                             EDIT-OPTION  OF  DSP-ENYUMM
                             EDIT-OPTION  OF  DSP-ENYUDD.
 OPT-CLR-EXIT.
     EXIT.
*=============================================================
*                帳票出力
*=============================================================
 PRINT-SEC           SECTION.
*改ページ
     IF    LINE-CNT    >=   MAX-LINE
       OR  PAGE-CNT     =   ZERO
       OR  HED-F17   NOT =  H1-SOKCD
           PERFORM  HEAD-WT-SEC
           MOVE     1           TO   DEN-FLG
     END-IF.
*仕入先ＣＤ ブレイク判定
     IF    HED-F06  NOT  =  WK-SIRCD
           MOVE     HED-F06     TO   WK-SIRCD
           MOVE     HED-F02     TO   WK-DENNO
           MOVE     1           TO   SIR-FLG
           MOVE     1           TO   DEN-FLG
     END-IF.
*伝票■ ブレイク判定
     IF   HED-F02     NOT  =  WK-DENNO
          MOVE     HED-F02     TO   WK-DENNO
          MOVE     1           TO   DEN-FLG
     END-IF.
*明細出力
     PERFORM  BODY-WT-SEC.
*抽出Ｆリード
     PERFORM  BDY-RD-SEC.
     IF  OLD-KEY  NOT =  NEW-KEY
         PERFORM   HED-RD-SEC
         IF  HAC-FLG  NOT =  9
             PERFORM   DATA-GET-SEC
         END-IF
     END-IF.
     IF  HAC-FLG   =   9    OR   HED-F06  NOT = WK-SIRCD
           MOVE    SPACE          TO      P-REC
           WRITE   P-REC          AFTER   1
           ADD     1              TO      LINE-CNT
     END-IF.
     IF  HAC-FLG   =  9
         MOVE   99          TO   MAIN-FLG
     END-IF.
 PRINT-EXIT.
     EXIT.
*=============================================================
*                ＨＥＡＤ部　印刷処理
*=============================================================
 HEAD-WT-SEC         SECTION.
     ADD   1                     TO  PAGE-CNT.
     MOVE  PAGE-CNT              TO  PAGE-SUU.
*倉庫名
     MOVE  HED-F17               TO  SOK-F01   H1-SOKCD.
     READ  ZSOKMS1
           INVALID  KEY
              MOVE  SPACE        TO  H1-SOKNM
           NOT INVALID  KEY
              MOVE  SOK-F02      TO  H1-SOKNM
     END-READ.
*
     IF      PAGE-CNT NOT =   1
             MOVE     SPACE   TO      P-REC
             WRITE    P-REC   AFTER   PAGE
     END-IF.
     WRITE   P-REC     FROM    MIDASI0     AFTER  2.
     WRITE   P-REC     FROM    MIDASI1     AFTER  1.
     WRITE   P-REC     FROM    MIDASI4     AFTER  1.
     WRITE   P-REC     FROM    MIDASI2     AFTER  1.
     WRITE   P-REC     FROM    MIDASI3     AFTER  1.
     WRITE   P-REC     FROM    MIDASI4     AFTER  1.
*
     MOVE  8                     TO  LINE-CNT.
     MOVE  1                     TO  SIR-FLG.
 HEAD-WT-EXIT.
     EXIT.
*=============================================================
*                明細印刷処理
*=============================================================
 BODY-WT-SEC                SECTION.
     MOVE  SPACE       TO  MEISAI1.
*仕入先
     IF    SIR-FLG               =   1
           MOVE  HED-F06        TO   PRT-BSIRCD
           MOVE  HED-F06        TO   SHI-F01
           READ  ZSHIMS1
                 INVALID  KEY
                    MOVE  SPACE      TO  PRT-BSIRNM
                 NOT INVALID  KEY
                    MOVE  SHI-F02    TO  PRT-BSIRNM
           END-READ
           MOVE  ZERO        TO      SIR-FLG
     END-IF.
*伝票■
     IF    DEN-FLG    =   1
           MOVE  HED-F02     TO  PRT-DENNO
           MOVE  ZERO        TO  DEN-FLG
     END-IF.
*行■
     MOVE  BDY-F03           TO  PRT-GYONO.
*入荷予定日
     MOVE  HED-F11(3:2)      TO  PRT-NYU-Y.
     MOVE  HED-F11(5:2)      TO  PRT-NYU-M.
     MOVE  HED-F11(7:2)      TO  PRT-NYU-D.
     MOVE  "."               TO  PRT-NYU-1  PRT-NYU-2.
*商品ＣＤ
     MOVE  BDY-F06           TO  PRT-SHOCD.
*商品名
     MOVE  BDY-F06           TO  MEI-F011.
     MOVE  BDY-F07           TO  MEI-F012.
     READ  MEIMS1
           INVALID  KEY
              MOVE  SPACE        TO  PRT-SHONM1
                                     PRT-SHONM2
           NOT INVALID  KEY
              MOVE  MEI-F021     TO  PRT-SHONM1
              MOVE  MEI-F022     TO  PRT-SHONM2
     END-READ.
*_番
     IF    BDY-F08  NOT = SPACE
       MOVE  BDY-F08(1:1)      TO  PRT-TANABN(1:1)
       MOVE  "-"               TO  PRT-TANABN(2:1)
       MOVE  BDY-F08(2:3)      TO  PRT-TANABN(3:3)
       MOVE  "-"               TO  PRT-TANABN(6:1)
       MOVE  BDY-F08(5:2)      TO  PRT-TANABN(7:2)
*    END-IF.
*発注数
     MOVE  0                 TO  PRT-HACSU.
     MOVE  BDY-F09           TO  PRT-HACSU.
*入荷数
     MOVE  BDY-F10           TO  PRT-NYUSU.
*発注残
     COMPUTE  PRT-ZANSU  =   BDY-F09   -  BDY-F10.
*備考
     MOVE  BDY-F16           TO  PRT-BIKOU.
*摘要
     MOVE  HED-F322          TO  PRT-TEKIYO1.
     MOVE  HED-F323          TO  PRT-TEKIYO2.
*メモ
     MOVE  HED-F15           TO  PRT-MEMO.
     MOVE  "("               TO  PRT-MEMO-1  PRT-TEKIYO-1.
     MOVE  ")"               TO  PRT-MEMO-2  PRT-TEKIYO-2.
*印刷
     WRITE    P-REC      FROM    MEISAI1    AFTER  1.
     WRITE    P-REC      FROM    MEISAI2    AFTER  1.
     ADD      2            TO    LINE-CNT.
 BODY-WT-EXIT.
     EXIT.

```
