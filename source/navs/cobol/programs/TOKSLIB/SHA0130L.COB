****************************************************************
*                                                              *
*    ユーザ　　　　名：　サカタのたね　　　殿                  *
*    システム　　　名：　自動発注管理システム                  *
*    プログラム　　名：　自動発注リスト作成                    *
*    作成者　　　　　：　ＮＡＶ　　　　　                      *
*    作成日　　　　　：　2000.07.10      UPDATE: YYYY.MM.DD    *
*                                                              *
****************************************************************
 IDENTIFICATION      DIVISION.
 PROGRAM-ID.         SHA0130L.
 AUTHOR.             NAV.
 DATE-WRITTEN.       00.07.10.
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
     SELECT   AUTHACF        ASSIGN        TO  01-VI-AUTHACL1
                             ORGANIZATION  IS  INDEXED
                             ACCESS MODE   IS  SEQUENTIAL
                             RECORD KEY    IS  AUT-F01
                                               AUT-F02
                                               AUT-F031
                                               AUT-F032
                                               AUT-F04
                             FILE STATUS   IS  AUT-STA.

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
     COPY     FHA01301  OF  XMDLIB
     JOINING  DSP      AS  PREFIX.
*発注Ｆ（ヘッダ）
 FD  AUTHACF.
     COPY     AUTHACF  OF  XFDLIB
     JOINING  AUT      AS  PREFIX.
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
     03  AUT-STA             PIC  X(02).
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
            NC"出力区分に誤りがあります。".
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
     03  AUT-FLG             PIC  9(01)  VALUE  ZERO.
     03  SIR-FLG             PIC  9(01)  VALUE  ZERO.
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
     02  FILLER              PIC  X(08)  VALUE  "SHA0130L".
     02  FILLER              PIC  X(23)  VALUE  SPACE.
     02  FILLER              PIC  N(21)  VALUE
         NC"＊＊＊　自　動　発　注　リ　ス　ト　＊＊＊".
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
*****02  FILLER              PIC  X(02)  VALUE  SPACE.
     02  FILLER              PIC  N(02)  VALUE
         NC"確定".
     02  FILLER              PIC  X(04)  VALUE  SPACE.
     02  FILLER              PIC  N(03)  VALUE
         NC"商　品".
     02  FILLER              PIC  X(04)  VALUE  SPACE.
     02  FILLER              PIC  N(03)  VALUE
         NC"品　単".
     02  FILLER              PIC  X(03)  VALUE  SPACE.
     02  FILLER              PIC  N(03)  VALUE
         NC"_　番".
     02  FILLER              PIC  X(04)  VALUE  SPACE.
     02  FILLER              PIC  N(04)  VALUE
         NC"現在庫数".
     02  FILLER              PIC  X(05)  VALUE  SPACE.
     02  FILLER              PIC  N(04)  VALUE
         NC"前月売上".
     02  FILLER              PIC  X(04)  VALUE  SPACE.
     02  FILLER              PIC  N(02)  VALUE
         NC"入数".
     02  FILLER              PIC  X(06)  VALUE  SPACE.
     02  FILLER              PIC  N(03)  VALUE
         NC"受注残".
     02  FILLER              PIC  X(07)  VALUE  SPACE.
     02  FILLER              PIC  N(03)  VALUE
         NC"発注残".
     02  FILLER              PIC  X(03)  VALUE  SPACE.
     02  FILLER              PIC  N(05)  VALUE
         NC"安全在庫数".
     02  FILLER              PIC  X(03)  VALUE  SPACE.
     02  FILLER              PIC  N(05)  VALUE
         NC"発注予定数".
     02  FILLER              PIC  X(06)  VALUE  SPACE.
     02  FILLER              PIC  N(04)  VALUE
         NC"実発注数".
****  見出し行３             ****
 01  MIDASI3        CHARACTER     TYPE   IS   NIHONGO.
*****02  FILLER              PIC  X(10)  VALUE  SPACE.
     02  FILLER              PIC  X(08)  VALUE  SPACE.
     02  FILLER              PIC  N(03)  VALUE
         NC"商品名".
     02  FILLER              PIC  X(40)  VALUE  SPACE.
     02  FILLER              PIC  N(02)  VALUE
         NC"担当".
****  見出し行４             ****
 01  MIDASI4        CHARACTER     TYPE   IS   NIHONGO.
*****02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  FILLER              PIC  N(68)  VALUE  ALL NC"─".
****  明細行１               ****
 01  MEISAI1        CHARACTER     TYPE   IS   NIHONGO.
*****02  FILLER              PIC  X(02)  VALUE  SPACE.
     02  FILLER              PIC  N(04)  VALUE
         NC"仕入先：".
     02  PRT-BSIRCD          PIC  X(08).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  PRT-BSIRNM          PIC  N(18).
****  明細行２               ****
 01  MEISAI2.
*****02  FILLER              PIC  X(04)  VALUE  SPACE.
     02  FILLER              PIC  X(02)  VALUE  SPACE.
     02  PRT-KAKUTEI         PIC  Z.
     02  FILLER              PIC  X(05)  VALUE  SPACE.
     02  PRT-SHOCD           PIC  X(08).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  PRT-HINTAN          PIC  X(08).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  PRT-TANABN          PIC  X(08).
*****02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  PRT-ZAIKO           PIC  --,---,--9.99.
     02  PRT-ZENURI          PIC  --,---,--9.99.
     02  PRT-IRISU           PIC  Z,ZZ9.99.
     02  PRT-JYUTYU-ZAN      PIC  --,---,--9.99.
     02  PRT-HATYU-ZAN       PIC  --,---,--9.99.
     02  PRT-ANZEN           PIC  --,---,--9.99.
     02  PRT-YOTEI           PIC  --,---,--9.99.
     02  FILLER              PIC  X(01)  VALUE  "(".
     02  PRT-HACSU           PIC  --,---,--9.99.
     02  FILLER              PIC  X(01)  VALUE  ")".
****  明細行３               ****
 01  MEISAI3        CHARACTER     TYPE   IS   YB.
*****02  FILLER              PIC  X(11)  VALUE  SPACE.
     02  FILLER              PIC  X(08)  VALUE  SPACE.
     02  PRT-SHONM1          PIC  N(15).
     02  PRT-SHONM2          PIC  N(15).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  PRT-TANTO           PIC  X(02).
     02  FILLER              PIC  X(54)  VALUE  SPACE.
     02  FILLER              PIC  N(05)  VALUE  NC"安全係数：".
     02  PRT-KEISU           PIC  9.99.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  FILLER              PIC  N(05)  VALUE  NC"在庫期間：".
     02  PRT-KIKAN           PIC  9.99.
****  明細行４               ****
 01  MEISAI4.
*****02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  FILLER              PIC  X(136) VALUE  ALL "-".
*
*メッセージ情報
 01  MSG-AREA.
     03  MSG-ABEND1.
         05  FILLER          PIC  X(12)  VALUE  "### SHA0130L".
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
*自動発注Ｆ
 HED-ERR-SEC        SECTION.
     USE      AFTER  EXCEPTION   PROCEDURE       AUTHACF.
     MOVE    "AUTHACL1"    TO    ERR-FL-ID.
     MOVE     AUT-STA      TO    ERR-STCD.
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
     DISPLAY  "**  SHA0130L   START  **"   UPON  CONS.
*
     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC    UNTIL  MAIN-FLG  =  99.
     PERFORM  END-SEC.
*
     DISPLAY  "**  SHA0130L    END   **"   UPON  CONS.
     STOP  RUN.
 CONTROL-EXIT.
     EXIT.
*=============================================================
*               初期処理
*=============================================================
 INIT-SEC            SECTION.
*ファイル ＯＰＥＮ
     OPEN     I-O         DSPF.
     OPEN     INPUT       AUTHACF.
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
     CLOSE      AUTHACF.
     CLOSE      ZSHIMS1.
     CLOSE      JYOKEN1.
     CLOSE      ZSOKMS1.
     CLOSE      MEIMS1.
     CLOSE      PRINTF.
*
     DISPLAY "* AUTHACF (IN)=" IN-CNT   " *" UPON CONS.
     DISPLAY "* AUTHACF (OT)=" OT-CNT   " *" UPON CONS.
     DISPLAY "* PRINTF(PAGE)=" PAGE-CNT " *" UPON CONS.
 END-EXIT.
     EXIT.
*=============================================================
*                画面初期表示処理
*=============================================================
 DSP-INIT-SEC       SECTION.
*初期画面の処理
     MOVE     SPACE          TO   DSP-CONTROL.
     MOVE    "FHA01301"      TO   DSP-FMT.
     MOVE     SPACE          TO   DSP-FHA01301.
     PERFORM  OPT-CLR-SEC.
*倉庫
     IF  LINK-DSOKCD    NOT =    "01"  AND  "88"
         MOVE      LINK-SOKCD     TO   SOK-F01   DSP-SOKCD
         READ      ZSOKMS1
              INVALID KEY
                   CONTINUE
              NOT INVALID KEY
                   MOVE      SOK-F02   TO   DSP-SOKONM
         END-READ
     END-IF.
*確定区分
     MOVE     ZERO      TO   DSP-OUTKB.
*ＢＯＤＹ部入力へ
     MOVE     2         TO   MAIN-FLG.
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
       MOVE   "X"       TO  EDIT-STATUS  OF  DSP-SOKCD
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
          WHEN OTHER
                          MOVE  1      TO  ERR-FLG
     END-EVALUATE.
 DSP-BODY-EXIT.
     EXIT.
*=============================================================
*                ＢＯＤＹ部入力チェック
*=============================================================
 CHK-BODY-SEC           SECTION.
     IF  LINK-DSOKCD    NOT =    "01"  AND  "88"
         GO   TO        CHK-BODY-500
     END-IF.
*倉庫
     MOVE     SPACE     TO   DSP-SOKONM.
     IF  DSP-SOKCD      NOT =  SPACE
         MOVE      DSP-SOKCD      TO   SOK-F01
         READ      ZSOKMS1
             INVALID KEY
                   MOVE     "R"   TO   EDIT-OPTION  OF DSP-SOKCD
                   MOVE     "C"   TO   EDIT-CURSOR  OF DSP-SOKCD
                   IF   ERR-FLG   =    ZERO
                        MOVE    7    TO   ERR-FLG
                   END-IF
             NOT INVALID KEY
                   MOVE   SOK-F02  TO   DSP-SOKONM
         END-READ
     ELSE
         MOVE      NC"全倉庫"      TO   DSP-SOKONM
     END-IF.
*
 CHK-BODY-500.
*開始仕入先ＣＤ
     MOVE   SPACE                TO  DSP-SSIRNM.
     IF  DSP-SSIIRE   =   SPACE
         MOVE    SPACE           TO  DSP-SSIIRE
     END-IF.
     IF  DSP-SSIIRE  NOT  =   SPACE
         MOVE   DSP-SSIIRE       TO  SHI-F01
         READ   ZSHIMS1
           INVALID  KEY
             IF  ERR-FLG  =  ZERO
                 MOVE   2        TO  ERR-FLG
             END-IF
             MOVE  "R"           TO  EDIT-OPTION  OF  DSP-SSIIRE
             MOVE  "C"           TO  EDIT-CURSOR  OF  DSP-SSIIRE
           NOT INVALID
             MOVE  SHI-F02       TO  DSP-SSIRNM
         END-READ
     END-IF.
*終了仕入先ＣＤ
     MOVE   SPACE                TO  DSP-ESIRNM.
     IF  DSP-ESIIRE   =   SPACE
         MOVE   "99999999"       TO  DSP-ESIIRE
     END-IF.
     IF  DSP-ESIIRE   NOT =  "99999999"
         MOVE   DSP-ESIIRE       TO  SHI-F01
         READ   ZSHIMS1
           INVALID  KEY
             IF  ERR-FLG  =  ZERO
                 MOVE   2        TO  ERR-FLG
             END-IF
             MOVE  "R"           TO  EDIT-OPTION  OF  DSP-ESIIRE
             MOVE  "C"           TO  EDIT-CURSOR  OF  DSP-ESIIRE
           NOT INVALID
             MOVE  SHI-F02       TO  DSP-ESIRNM
         END-READ
     END-IF.
     IF  DSP-SSIRNM  NOT =  SPACE
     AND DSP-ESIRNM  NOT =  SPACE
     AND DSP-SSIIRE  >  DSP-ESIIRE
         IF  ERR-FLG  =  ZERO
             MOVE   3        TO  ERR-FLG
         END-IF
         MOVE  "C"           TO  EDIT-CURSOR  OF  DSP-SSIIRE
         MOVE  "R"           TO  EDIT-OPTION  OF  DSP-SSIIRE
         MOVE  "R"           TO  EDIT-OPTION  OF  DSP-ESIIRE
     END-IF.
*    出力区分（0:全て、1:確定のみ）
     IF  DSP-OUTKB      =    ZERO  OR "1"
         CONTINUE
     ELSE
         IF   ERR-FLG   =    ZERO
              MOVE      4    TO   ERR-FLG
         END-IF
         MOVE     "R"   TO   EDIT-OPTION  OF  DSP-OUTKB
         MOVE     "C"   TO   EDIT-CURSOR  OF  DSP-OUTKB
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
                          MOVE  99      TO   MAIN-FLG
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
     MOVE     ZERO           TO   ERR-FLG.
*自動発注Ｆスタート
     MOVE     ZERO           TO   AUT-FLG.
     MOVE     SPACE          TO   AUT-REC.
     INITIALIZE                   AUT-REC.
     MOVE     DSP-SOKCD      TO   AUT-F01.
     MOVE     DSP-SSIIRE     TO   AUT-F02.
     START    AUTHACF   KEY >=    AUT-F01
                                  AUT-F02
                                  AUT-F031
                                  AUT-F032
                                  AUT-F04
         INVALID
              MOVE      9    TO   AUT-FLG
         NOT  INVALID
*自動発注Ｆ読み込み
              PERFORM   AUT-RD-SEC
     END-START.
*対象データ存在ＣＨＫ
     IF  AUT-FLG   =    9
         MOVE      5    TO   ERR-FLG
     END-IF.
*
     IF  ERR-FLG   =    ZERO
         MOVE      6    TO   ERR-FLG
         PERFORM   DSP-WT-SEC
         MOVE      4    TO   MAIN-FLG
     ELSE
         MOVE      99   TO   MAIN-FLG
     END-IF.
 CHK-KAKU-EXIT.
     EXIT.
*=============================================================
*                自動発注Ｆリード処理
*=============================================================
 AUT-RD-SEC        SECTION.
*リード
     READ     AUTHACF   NEXT
         AT END
              MOVE      9    TO   AUT-FLG
              GO   TO   AUT-RD-EXIT
     END-READ.
     ADD      1    TO   IN-CNT.
*倉庫ＣＤ
     IF       DSP-SOKCD      NOT =     SPACE
         IF   AUT-F01   NOT =     DSP-SOKCD
              MOVE      9    TO   AUT-FLG
              GO   TO   AUT-RD-EXIT
         END-IF
     END-IF.
*仕入先コード範囲
     IF  DSP-SSIIRE     <=   AUT-F02   AND
         AUT-F02        <=   DSP-ESIIRE
         CONTINUE
     ELSE
         GO   TO   AUT-RD-SEC
     END-IF.
*出力区分
     IF       DSP-OUTKB      =    1
         IF   AUT-F17   NOT =    "1"
              GO   TO   AUT-RD-SEC
         END-IF
     END-IF.
     ADD      1    TO   OT-CNT.
*
 AUT-RD-EXIT.
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
     WRITE DSP-FHA01301.
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
     MOVE  SPACE         TO  EDIT-CURSOR  OF  DSP-SOKCD
                             EDIT-CURSOR  OF  DSP-SSIIRE
                             EDIT-CURSOR  OF  DSP-ESIIRE
                             EDIT-CURSOR  OF  DSP-OUTKB.
     MOVE "M"            TO  EDIT-OPTION  OF  DSP-SOKCD
                             EDIT-OPTION  OF  DSP-SSIIRE
                             EDIT-OPTION  OF  DSP-ESIIRE
                             EDIT-OPTION  OF  DSP-OUTKB.
 OPT-CLR-EXIT.
     EXIT.
*=============================================================
*                帳票出力
*=============================================================
 PRINT-SEC           SECTION.
*改ページ
     IF    LINE-CNT    >=   MAX-LINE
       OR  PAGE-CNT     =   ZERO
       OR  AUT-F01   NOT =  H1-SOKCD
           PERFORM  HEAD-WT-SEC
     END-IF.
*仕入先ＣＤ ブレイク判定
     IF    AUT-F02  NOT  =  WK-SIRCD
           MOVE     AUT-F02     TO   WK-SIRCD
           MOVE     1           TO   SIR-FLG
     END-IF.
*明細出力
     PERFORM  BODY-WT-SEC.
*抽出Ｆリード
     PERFORM  AUT-RD-SEC.
     IF  AUT-FLG   =  9
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
     MOVE  AUT-F01               TO  SOK-F01   H1-SOKCD.
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
*
*仕入先
     IF    SIR-FLG               =   1
           MOVE  AUT-F02        TO   PRT-BSIRCD
           MOVE  AUT-F02        TO   SHI-F01    WK-SIRCD
           READ  ZSHIMS1
                 INVALID  KEY
                    MOVE  SPACE      TO  PRT-BSIRNM
                 NOT INVALID  KEY
                    MOVE  SHI-F02    TO  PRT-BSIRNM
           END-READ
           MOVE  ZERO        TO      SIR-FLG
*
           IF    LINE-CNT    NOT =      8
                 WRITE   P-REC  FROM    MEISAI4  AFTER  1
                 ADD     1      TO      LINE-CNT
           END-IF
           WRITE   P-REC  FROM    MEISAI1  AFTER  1
           ADD     1      TO      LINE-CNT
     END-IF.
*
*確定
     MOVE  AUT-F17           TO  PRT-KAKUTEI.
*商品ＣＤ
     MOVE  AUT-F031          TO  PRT-SHOCD.
*品単
     MOVE  AUT-F032          TO  PRT-HINTAN.
*商品名
     MOVE  AUT-F031          TO  MEI-F011.
     MOVE  AUT-F032          TO  MEI-F012.
     READ  MEIMS1
           INVALID  KEY
              MOVE  SPACE        TO  PRT-SHONM1
                                     PRT-SHONM2
           NOT INVALID  KEY
              MOVE  MEI-F021     TO  PRT-SHONM1
              MOVE  MEI-F022     TO  PRT-SHONM2
     END-READ.
*_番
     IF  AUT-F04   NOT =     SPACE
         MOVE      AUT-F04(1:1)   TO   PRT-TANABN(1:1)
         MOVE     "-"             TO   PRT-TANABN(2:1)
         MOVE      AUT-F04(2:3)   TO   PRT-TANABN(3:3)
         MOVE     "-"             TO   PRT-TANABN(6:1)
         MOVE      AUT-F04(5:2)   TO   PRT-TANABN(7:2)
     ELSE
         MOVE      SPACE          TO   PRT-TANABN
     END-IF.
*現在庫数
     MOVE  AUT-F05           TO  PRT-ZAIKO.
*前月売上数
     MOVE  AUT-F18           TO  PRT-ZENURI.
*入数
     MOVE  AUT-F19           TO  PRT-IRISU.
*受注残
     MOVE  AUT-F07           TO  PRT-JYUTYU-ZAN.
*発注残
     MOVE  AUT-F08           TO  PRT-HATYU-ZAN.
*安全在庫数
     MOVE  AUT-F06           TO  PRT-ANZEN.
*発注予定数
     MOVE  AUT-F09           TO  PRT-YOTEI.
*発注数
     MOVE  AUT-F10           TO  PRT-HACSU.
*担当
     MOVE  AUT-F14           TO  PRT-TANTO.
*安全係数
     MOVE  AUT-F20           TO  PRT-KEISU.
*在庫期間
     MOVE  AUT-F21           TO  PRT-KIKAN.
*印刷
     WRITE    P-REC      FROM    MEISAI2    AFTER  1.
     WRITE    P-REC      FROM    MEISAI3    AFTER  1.
     ADD      2            TO    LINE-CNT.
 BODY-WT-EXIT.
     EXIT.
