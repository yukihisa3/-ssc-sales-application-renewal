************************************************************
*    顧客名          :  （株）サカタのタネ殿               *
*    システム名      :  在庫システム　　　　               *
*    プログラム名    :  入出庫実績リスト          　　　   *
*    プログラムＩＤ  :  SZA0260L                           *
*    作成者          :  NAV      　                        *
*    作成日          :  2000.07.05                         *
************************************************************
*
 IDENTIFICATION         DIVISION.
 PROGRAM-ID.            SZA0260L.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FACOM.
 OBJECT-COMPUTER.       FACOM.
 SPECIAL-NAMES.
     CONSOLE            IS    CONS
     STATION            IS    STAT
     YA                 IS    2-0PITCH
     YB                 IS    1-5PITCH
     YB-21              IS    3-0PITCH.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*
****<<  画面ファイル        >>******************************
     SELECT   DSPF      ASSIGN  TO   GS-DSPF
                        ORGANIZATION         IS  SEQUENTIAL
                        ACCESS  MODE         IS  SEQUENTIAL
                        SYMBOLIC DESTINATION IS  "DSP"
                        PROCESSING MODE      IS   DSP-PROC
                        GROUP                IS   DSP-GROUP
                        FORMAT               IS   DSP-FORMAT
                        SELECTED FUNCTION    IS   DSP-FUNC
                        FILE STATUS          IS   DSP-STATUS.
****<<  入出庫ファイル　　  >>******************************
     SELECT   NYSFILF   ASSIGN    TO      DA-VI-NYSFILL2
                        FILE STATUS          IS  NYU-STS
                        ORGANIZATION         IS  INDEXED
                        ACCESS  MODE         IS  SEQUENTIAL
                        RECORD  KEY          IS  NYU-F97  NYU-F02
                                                 NYU-F03.
****<< 倉庫マスタ 　        >>******************************
     SELECT   ZSOKMS    ASSIGN    TO      DA-VI-ZSOKMS1
                        FILE STATUS          IS  SOK-STS
                        ORGANIZATION         IS  INDEXED
                        ACCESS  MODE         IS  RANDOM
                        RECORD  KEY          IS  SOK-F01.
****<< 商品名称マスタ       >>******************************
     SELECT   HMEIMS    ASSIGN    TO      DA-VI-MEIMS1
                        FILE STATUS          IS  MEI-STS
                        ORGANIZATION         IS  INDEXED
                        ACCESS  MODE         IS  RANDOM
                        RECORD  KEY          IS  MEI-F01.
****<< 条件ファイル　       >>******************************
     SELECT   HJYOKEN   ASSIGN    TO      DA-VI-JYOKEN1
                        FILE STATUS          IS  JYO-STS
                        ORGANIZATION         IS  INDEXED
                        ACCESS  MODE         IS  RANDOM
                        RECORD  KEY          IS  JYO-F01
                                                 JYO-F02.
*
****<<  プリントファイル    >>******************************
     SELECT   PRINTF    ASSIGN  TO   LP-04.
****
 DATA                   DIVISION.
 FILE                   SECTION.
*
****<<  画面ファイル　  　  >>******************************
 FD    DSPF.
 COPY  FZA02601          OF   XMDLIB.
****<<  入出庫ファイル  　  >>******************************
 FD    NYSFILF.
 COPY  NYSFILF          OF   XFDLIB
                             JOINING    NYU  PREFIX.
****<< 倉庫マスタ 　        >>******************************
 FD    ZSOKMS.
 COPY  ZSOKMS           OF   XFDLIB
                             JOINING    SOK  PREFIX.
****<< 商品名称マスタ       >>******************************
 FD    HMEIMS.
 COPY  MEIMS1           OF   XFDLIB
                             JOINING    MEI  PREFIX.
****<< 条件ファイル         >>******************************
 FD    HJYOKEN.
 COPY  JYOKEN1          OF   XFDLIB
                             JOINING    JYO  PREFIX.
****<<  プリントファイル  >>********************************
 FD    PRINTF.
 01    P-REC                 PIC  X(200).
*
****  作業領域  ********************************************
 WORKING-STORAGE        SECTION.
*
****  画面制御項目  ****
 01  DSP-CONTROL.
     03 DSP-PROC             PIC  X(02).
     03 DSP-GROUP            PIC  X(08).
     03 DSP-FORMAT           PIC  X(08).
     03 DSP-STATUS           PIC  X(02).
     03 DSP-FUNC             PIC  X(04).
     03 WK-GROUP.
       05  WK-BODY           PIC  X(04).
       05  WK-LINE           PIC  9(02).
       05  FILLER            PIC  X(02).
*
****  ステータス領域  ****
 01  STATUS-AREA.
     03  NYU-STS             PIC  X(02).
     03  SOK-STS             PIC  X(02).
     03  MEI-STS             PIC  X(02).
     03  JYO-STS             PIC  X(02).
     03  SQLSTATE            PIC  X(05).
*
****  フラグ  ****
 01  PSW-AREA.
     03  END-FLG             PIC  X(03)  VALUE  SPACE.
     03  INV-FLG             PIC  9(01)  VALUE  ZERO.
     03  SYR-FLG             PIC  9(01)  VALUE  ZERO.
     03  ERR-CD              PIC  9(01)  VALUE  ZERO.
*
*日付／時刻
 01  SYS-DATE                PIC  9(08).
 01  FILLER                  REDEFINES   SYS-DATE.
     03  FILLER              PIC  9(02).
     03  SYS-YMD.
       05  SYS-YY            PIC  9(02).
       05  SYS-MM            PIC  9(02).
       05  SYS-DD            PIC  9(02).
 01  SYS-TIME.
     03  SYS-HH              PIC  9(02).
     03  SYS-MN              PIC  9(02).
     03  SYS-SS              PIC  9(02).
     03  FILLER              PIC  9(02).
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
****  カウンタ  ****
 01  CNT-AREA.
     03  L-CNT               PIC  9(02)  VALUE  99.
     03  P-CNT               PIC  9(07)  VALUE  ZERO.
 01  KEN-AREA1.
     03  NYU-IN              PIC  9(07)  VALUE  ZERO.
     03  NYU-OUT             PIC  9(07)  VALUE  ZERO.
     03  NYU-SKIP            PIC  9(07)  VALUE  ZERO.
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
****  ワーク領域  ****
 01  WORK-AREA.
     03  WK-DENBAN           PIC  9(07)  VALUE  ZERO.
 01  WK-TANABAN.
     03  WK-TANA1         PIC  X(01).
     03  FILLER           PIC  X(01)  VALUE  "-".
     03  WK-TANA2         PIC  X(03).
     03  FILLER           PIC  X(01)  VALUE  "-".
     03  WK-TANA3         PIC  X(02).
****  見出し行（表題）  ****
 01  HYODAI.
     03  FILLER              PIC  X(01)  VALUE  SPACE.
     03  FILLER              PIC  X(08)  VALUE  "SZA0260L".
     03  FILLER              PIC  X(30)  VALUE  SPACE.
     03  FILLER              PIC  N(16)  VALUE
         NC"＊＊＊　入出庫実績リスト　＊＊＊"
                             CHARACTER   TYPE   IS   3-0PITCH.
     03  FILLER              PIC  X(18)  VALUE  SPACE.
     03  FILLER              PIC  N(03)  VALUE  NC"処理日"
                             CHARACTER   TYPE   IS   2-0PITCH.
     03  FILLER              PIC  X(01)  VALUE  ":".
     03  H-YY                PIC  9(04).
     03  FILLER              PIC  X(01)  VALUE  ".".
     03  H-MM                PIC  Z9.
     03  FILLER              PIC  X(01)  VALUE  ".".
     03  H-DD                PIC  Z9.
     03  FILLER              PIC  X(03)  VALUE  SPACE.
     03  FILLER              PIC  N(01)  VALUE  NC"頁"
                             CHARACTER   TYPE   IS   2-0PITCH.
     03  FILLER              PIC  X(01)  VALUE  ":".
     03  H-PAGE              PIC  ZZZ9   VALUE  SPACE.
*
****  見出し行０  ****
 01  MIDASHI-0       CHARACTER    TYPE  IS  2-0PITCH.
     03  FILLER              PIC  X(03)  VALUE  SPACE.
     03  FILLER          PIC  N(07)  VALUE NC"実績出力日付：".
     03  H0-JISEKI-YY        PIC  9(04).
     03  FILLER              PIC  N(01)  VALUE  NC"年".
     03  H0-JISEKI-MM        PIC  Z9.
     03  FILLER              PIC  N(01)  VALUE  NC"月".
     03  H0-JISEKI-DD        PIC  Z9.
     03  FILLER              PIC  N(01)  VALUE  NC"日".
*
****  見出し行１  ****
 01  MIDASHI-1       CHARACTER    TYPE  IS  2-0PITCH.
     03  FILLER              PIC  X(03)  VALUE  SPACE.
     03  FILLER          PIC  N(07)  VALUE NC"実績出力倉庫：".
     03  H1-SOKCD            PIC  X(02)  VALUE  SPACE.
     03  FILLER              PIC  X(01)  VALUE  SPACE.
     03  H1-SOKNM            PIC  N(15).
*
****  見出し行２  ****
 01  MIDASHI-2       CHARACTER    TYPE  IS  2-0PITCH.
     03  FILLER              PIC  X(07)  VALUE  SPACE.
     03  FILLER              PIC  N(03)  VALUE  NC"伝票_".
     03  FILLER              PIC  X(02)  VALUE  SPACE.
     03  FILLER              PIC  N(04)  VALUE  NC"伝票区分".
     03  FILLER              PIC  X(04)  VALUE  SPACE.
     03  FILLER              PIC  N(04)  VALUE  NC"作業区分".
     03  FILLER              PIC  X(08)  VALUE  SPACE.
     03  FILLER              PIC  N(04)  VALUE  NC"入出庫日".
     03  FILLER              PIC  X(01)  VALUE  SPACE.
     03  FILLER              PIC  N(04)  VALUE  NC"入庫場所".
     03  FILLER              PIC  X(11)  VALUE  SPACE.
     03  FILLER              PIC  N(04)  VALUE  NC"出庫場所".
     03  FILLER              PIC  X(11)  VALUE  SPACE.
     03  FILLER              PIC  N(02)  VALUE  NC"担当".
****  見出し行３  ****
 01  MIDASHI-3       CHARACTER    TYPE  IS  2-0PITCH.
     03  FILLER              PIC  X(15)  VALUE  SPACE.
     03  FILLER              PIC  N(01)  VALUE  NC"行".
     03  FILLER              PIC  X(01)  VALUE  SPACE.
     03  FILLER              PIC  N(03)  VALUE  NC"_　番".
     03  FILLER              PIC  X(03)  VALUE  SPACE.
     03  FILLER              PIC  N(02)  VALUE  NC"商品".
     03  FILLER              PIC  X(04)  VALUE  "ｺ-ﾄﾞ".
     03  FILLER              PIC  X(01)  VALUE  SPACE.
     03  FILLER              PIC  N(03)  VALUE  NC"品　単".
     03  FILLER              PIC  X(03)  VALUE  SPACE.
     03  FILLER              PIC  N(05)  VALUE  NC"商　品　名".
     03  FILLER              PIC  X(39)  VALUE  SPACE.
     03  FILLER              PIC  N(03)  VALUE  NC"数　量".
     03  FILLER              PIC  X(04)  VALUE  SPACE.
     03  FILLER              PIC  X(04)  VALUE  "ｽﾄｯｸ".
     03  FILLER              PIC  N(01)  VALUE  NC"_".
     03  FILLER              PIC  X(01)  VALUE  SPACE.
     03  FILLER              PIC  N(03)  VALUE  NC"備　考".
     03  FILLER              PIC  X(05)  VALUE  SPACE.
     03  FILLER              PIC  N(04)  VALUE  NC"相手_番".
*
****  見出し行４  ****
 01  MIDASHI-4       CHARACTER    TYPE  IS  2-0PITCH.
     03  FILLER              PIC  X(01)  VALUE  SPACE.
     03  FILLER              PIC  N(66)  VALUE  ALL NC"─".
****  明細１  ****
 01  MEISAI-1.
     03  M-SAKUJYO           PIC  N(02)
                             CHARACTER   TYPE   IS   2-0PITCH.
     03  FILLER              PIC  X(03)  VALUE  SPACE.
     03  M-GENPYO            PIC  9(07).
     03  FILLER              PIC  X(01)  VALUE  SPACE.
     03  M-DENKBN1           PIC  X(02).
     03  FILLER              PIC  X(01)  VALUE  SPACE.
     03  M-DENMEI            PIC  N(06)
                             CHARACTER   TYPE   IS   1-5PITCH.
     03  M-SAGYOKBN          PIC  X(02).
     03  FILLER              PIC  X(01)  VALUE  SPACE.
     03  M-SAMEISYO          PIC  N(08)
                             CHARACTER   TYPE   IS   1-5PITCH.
     03  FILLER              PIC  X(01)  VALUE  SPACE.
     03  M-HIZ-Y             PIC  9(02).
     03  M-HIZ-1             PIC  X(01).
     03  M-HIZ-M             PIC  9(02).
     03  M-HIZ-2             PIC  X(01).
     03  M-HIZ-D             PIC  9(02).
     03  FILLER              PIC  X(01)  VALUE  SPACE.
     03  M-NYUBCD            PIC  X(02).
     03  FILLER              PIC  X(01)  VALUE  SPACE.
     03  M-NYUBASYO          PIC  N(10)
                             CHARACTER   TYPE   IS   1-5PITCH.
     03  FILLER              PIC  X(01)  VALUE  SPACE.
     03  M-SYUBCD            PIC  X(02).
     03  FILLER              PIC  X(01)  VALUE  SPACE.
     03  M-SYUBASYO          PIC  N(10)
                             CHARACTER   TYPE   IS   1-5PITCH.
     03  FILLER              PIC  X(03)  VALUE  SPACE.
     03  M-TANTO             PIC  X(02).

*
****  明細２  ****
 01  MEISAI-2.
     03  FILLER              PIC  X(15)  VALUE  SPACE.
     03  M-GYO               PIC  Z9.
     03  FILLER              PIC  X(01)  VALUE  SPACE.
     03  M-TANABAN           PIC  X(08).
     03  FILLER              PIC  X(01)  VALUE  SPACE.
     03  M-SHOCD             PIC  X(08).
     03  FILLER              PIC  X(01)  VALUE  SPACE.
     03  M-HINTAN            PIC  X(08).
     03  FILLER              PIC  X(01)  VALUE  SPACE.
     03  M-SHOMEI            PIC  N(30)
                             CHARACTER   TYPE   IS   1-5PITCH.
     03  FILLER              PIC  X(01)  VALUE  SPACE.
     03  M-SURYO             PIC  Z,ZZZ,ZZ9.99.
     03  FILLER              PIC  X(01)  VALUE  SPACE.
     03  M-STOKU1            PIC  X(04).
     03  FILLER              PIC  X(01)  VALUE  SPACE.
     03  M-STOKU2            PIC  X(01).
     03  FILLER              PIC  X(01)  VALUE  SPACE.
     03  M-BIKOU             PIC  X(10).
     03  FILLER              PIC  X(01)  VALUE  SPACE.
     03  M-AITE              PIC  X(10).
*
 01  SEN.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  FILLER              PIC  X(132) VALUE  ALL "-".
*
****  ＰＦキーガイド  ****
 01 MSG-AREA.
    03 PFMSG01               PIC  N(30)  VALUE
       NC"_取消　_終了　_項目戻り".
    03 PFMSG02               PIC  N(30)  VALUE
       NC"_取消　_終了".
*
****  メッセージ情報  ****
 01  MSG-AREA.
     03  MSG-ABEND1.
       05  FILLER            PIC  X(04)  VALUE  "### ".
       05  ERR-PG-ID         PIC  X(08)  VALUE  "SZA0260L".
       05  FILLER            PIC  X(13)  VALUE  " ABEND    ###".

     03  MSG-ABEND2.
       05  FILLER            PIC  X(04)  VALUE  "### ".
       05  ERR-FL-ID         PIC  X(08).
       05  FILLER            PIC  X(04)  VALUE  " ST-".
       05  ERR-STCD          PIC  X(05).
       05  FILLER            PIC  X(05)  VALUE  " ###".
*
****  エラーメッセージ         ****
 01  ERR-TAB.
     03  MSG-ERR1            PIC  N(20)  VALUE
            NC"誤ったＰＦキーが押されました。".
     03  MSG-ERR2            PIC  N(20)  VALUE
            NC"倉庫コードを入力してください。".
     03  MSG-ERR3            PIC  N(20)  VALUE
            NC"倉庫コードに誤りがあります。".
     03  MSG-ERR4            PIC  N(20)  VALUE
            NC"年月日に誤りがあります。".
     03  MSG-ERR5            PIC  N(20)  VALUE
            NC"Ｙで入力して下さい。".
     03  MSG-ERR6            PIC  N(20)  VALUE
            NC"開始が終了を越えています。".
 01  ERR-MSG-ALL             REDEFINES   ERR-TAB.
     03  ERR-MSG             PIC  N(20)
                             OCCURS   6  TIMES.
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
****  パラメータ  ****
 LINKAGE                SECTION.
 01  LINK-SOKCD              PIC  X(02).
 01  LINK-DSOKCD             PIC  X(02).
************************************************************
*      ＭＡＩＮ              ＭＯＤＵＬＥ                  *
************************************************************
*
 PROCEDURE           DIVISION     USING  LINK-SOKCD
                                         LINK-DSOKCD.
*
 DECLARATIVES.
**画面ファイル
 FILEERR-SEC1        SECTION.
     USE AFTER    EXCEPTION
                  PROCEDURE   DSPF.
     MOVE   "DSPF    "            TO   ERR-FL-ID.
     MOVE    DSP-STATUS           TO   ERR-STCD.
     DISPLAY   MSG-ABEND1      UPON   CONS.
     DISPLAY   MSG-ABEND2      UPON   CONS.
     MOVE     "    "              TO   PROGRAM-STATUS.
     STOP     RUN.
**入出庫ファイル
 FILEERR-SEC2        SECTION.
     USE AFTER    EXCEPTION
                  PROCEDURE   NYSFILF.
     MOVE   "NYSFILF "            TO   ERR-FL-ID.
     MOVE    NYU-STS              TO   ERR-STCD.
     DISPLAY   MSG-ABEND1      UPON   CONS.
     DISPLAY   MSG-ABEND2      UPON   CONS.
     MOVE     "    "              TO   PROGRAM-STATUS.
     STOP     RUN.
**倉庫マスタ
 FILEERR-SEC3        SECTION.
     USE AFTER    EXCEPTION
                  PROCEDURE   ZSOKMS.
     MOVE   "ZSOKMS  "            TO   ERR-FL-ID.
     MOVE    SOK-STS              TO   ERR-STCD.
     DISPLAY   MSG-ABEND1      UPON   CONS.
     DISPLAY   MSG-ABEND2      UPON   CONS.
     MOVE     "    "              TO   PROGRAM-STATUS.
     STOP     RUN.
**商品名称マスタ
 FILEERR-SEC4        SECTION.
     USE AFTER    EXCEPTION
                  PROCEDURE   HMEIMS.
     MOVE   "HMEIMS  "            TO   ERR-FL-ID.
     MOVE    MEI-STS              TO   ERR-STCD.
     DISPLAY   MSG-ABEND1      UPON   CONS.
     DISPLAY   MSG-ABEND2      UPON   CONS.
     MOVE     "    "              TO   PROGRAM-STATUS.
     STOP     RUN.
**条件ファイル
 FILEERR-SEC5        SECTION.
     USE AFTER    EXCEPTION
                  PROCEDURE   HJYOKEN.
     MOVE   "HJYOKEN "            TO   ERR-FL-ID.
     MOVE    JYO-STS              TO   ERR-STCD.
     DISPLAY   MSG-ABEND1      UPON   CONS.
     DISPLAY   MSG-ABEND2      UPON   CONS.
     MOVE     "    "              TO   PROGRAM-STATUS.
     STOP     RUN.
**
 END     DECLARATIVES.
************************************************************
*      <0.0>    メインモジュール                           *
************************************************************
 SZA0260L-START         SECTION.
     DISPLAY  "**  SZA0260L   START  **"   UPON  CONS.
*
     PERFORM            INIT-SEC.
     PERFORM            MAIN-SEC
                        UNTIL      END-FLG  =    "END".
     PERFORM            END-SEC.
*
     DISPLAY  "**  SZA0260L    END   **"   UPON  CONS.
     STOP               RUN.
 SZA0260L-END.
     EXIT.
************************************************************
*      <1.0>    初期処理                                   *
************************************************************
 INIT-SEC               SECTION.
*----  ファイルＯＰＥＮ  ----*
     OPEN     I-O      DSPF.
     OPEN     INPUT    NYSFILF  ZSOKMS  HMEIMS  HJYOKEN.
     OPEN     OUTPUT   PRINTF.
*----  システム日付の格納  ----*
     ACCEPT      SYS-YMD     FROM  DATE.
     ACCEPT      SYS-TIME    FROM  TIME.
*システム日付・時刻の取得
     MOVE     "3"                 TO   LINK-IN-KBN.
     MOVE     SYS-YMD             TO   LINK-IN-YMD6.
     MOVE     ZERO                TO   LINK-IN-YMD8.
     MOVE     ZERO                TO   LINK-OUT-RET.
     MOVE     ZERO                TO   LINK-OUT-YMD.
     CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD.
     MOVE      LINK-OUT-YMD       TO   SYS-DATE.
*画面表示日付編集
     MOVE      SYS-DATE(1:4)      TO   HEN-DATE-YYYY H-YY.
     MOVE      SYS-DATE(5:2)      TO   HEN-DATE-MM   H-MM.
     MOVE      SYS-DATE(7:2)      TO   HEN-DATE-DD   H-DD.
*画面表示時刻編集
     MOVE      SYS-TIME(1:2)      TO   HEN-TIME-HH.
     MOVE      SYS-TIME(3:2)      TO   HEN-TIME-MM.
     MOVE      SYS-TIME(5:2)      TO   HEN-TIME-SS.
*
 INIT-EXIT.
     EXIT.
************************************************************
*      <2.0>  メイン処理                                   *
************************************************************
 MAIN-SEC               SECTION.
     EVALUATE     SYR-FLG
         WHEN     0           PERFORM   DSP-INIT-SEC
         WHEN     1           PERFORM   DSP-INPUT-SEC
         WHEN     2           PERFORM   DSP-KAKUNIN-SEC
     END-EVALUATE.
 MAIN-EXIT.
     EXIT.
************************************************************
*      3.0        終了処理                                 *
************************************************************
 END-SEC                SECTION.
*----  ファイルＣＬＯＳＥ  ----*
     CLOSE    DSPF  NYSFILF  ZSOKMS  HMEIMS  HJYOKEN  PRINTF.
*
     DISPLAY "* NYSFILF (IN)=" NYU-IN   " *" UPON CONS.
     DISPLAY "* NYSFILF (OT)=" NYU-OUT  " *" UPON CONS.
     DISPLAY "* PRINTF(PAGE)=" P-CNT    " *" UPON CONS.
 END-EXIT.
     EXIT.
************************************************************
*      <2.1>  画面初期処理                                 *
************************************************************
 DSP-INIT-SEC            SECTION.
*属性のクリア
     MOVE   SPACE                TO   FZA02601   DSP-CONTROL.
     PERFORM   ERR-CLR-SEC.
*倉庫コードのセット
     IF      LINK-DSOKCD  =  "01"  OR  "88"
             MOVE    SPACE       TO   SOKCD
     ELSE
             MOVE    LINK-SOKCD   TO    SOKCD  SOK-F01
             PERFORM    SOK-READ-SEC
             IF     INV-FLG   =  1
                    MOVE    SPACE    TO   SOKMEI
             ELSE
                    MOVE    SOK-F02  TO   SOKMEI
             END-IF
             MOVE    "X"          TO   EDIT-STATUS  OF  SOKCD
     END-IF.
     MOVE    1                    TO   SYR-FLG.
*対象年月日初期表示
     MOVE    SYS-YY               TO   SYY.
     MOVE    SYS-MM               TO   SMM.
     MOVE    SYS-DD               TO   SDD.
     MOVE    HEN-DATE             TO   SDATE.
     MOVE    HEN-TIME             TO   STIME.
*****MOVE    SPACE                TO   SOKCD.
*画面初期表示
     MOVE    SPACE                TO   DSP-CONTROL.
     MOVE    "FZA02601"           TO   DSP-FORMAT.
     MOVE    "SCREEN"             TO   DSP-GROUP.
 DSP-INIT-EXIT.
     EXIT.
************************************************************
*      <2.2>  画面入力　処理                               *
************************************************************
 DSP-INPUT-SEC           SECTION.
     MOVE    PFMSG02              TO   PFMSG.
     MOVE    "BODY"               TO   WK-GROUP.
     PERFORM     DSP-RD-SEC.
     EVALUATE    DSP-FUNC
         WHEN    "F004"
             MOVE    0            TO   SYR-FLG
         WHEN    "F005"
             MOVE    "END"        TO   END-FLG
         WHEN    "E000"
             PERFORM    ERR-CLR-SEC
             PERFORM    CHK-INPUT-SEC
         WHEN    OTHER
             MOVE    1            TO   SYR-FLG
             MOVE    1            TO   ERR-CD
     END-EVALUATE.
 DSP-INPUT-EXIT.
     EXIT.
*----------------------------------------------------------*
*                   画面ＲＥＡＤ処理                       *
*----------------------------------------------------------*
 DSP-RD-SEC             SECTION.
     IF     ERR-CD   =   0
            MOVE    SPACE         TO    ERRMSG
     ELSE
            MOVE    ERR-MSG(ERR-CD)   TO    ERRMSG
     END-IF.
     MOVE    "SCREEN"             TO    DSP-GROUP.
     PERFORM    DSP-WRITE-SEC.
*
     IF      LINK-DSOKCD  =  "01"  OR   "88"
             CONTINUE
     ELSE
             MOVE    "X"          TO    EDIT-STATUS  OF  SOKCD
     END-IF.
*
     IF     ERR-CD   =   0
            MOVE    "NE"          TO    DSP-PROC
     ELSE
            MOVE    "AL"          TO    DSP-PROC
            MOVE    0             TO    ERR-CD
     END-IF.
     MOVE   WK-GROUP              TO    DSP-GROUP.
     READ    DSPF.
     MOVE   SPACE                 TO    DSP-PROC.
 DSP-RD-EXIT.
     EXIT.
*----------------------------------------------------------*
*                  画面ＷＲＩＴＥ処理                      *
*----------------------------------------------------------*
 DSP-WRITE-SEC          SECTION.
     MOVE   SPACE                 TO   DSP-PROC.
     WRITE    FZA02601.
 DSP-WRITE-EXIT.
     EXIT.
*----------------------------------------------------------*
*                     入力チェック                         *
*----------------------------------------------------------*
 CHK-INPUT-SEC          SECTION.
*倉庫
     IF   SOKCD  =  SPACE
          MOVE   SPACE     TO   SOKMEI
          MOVE    3        TO   ERR-CD
          MOVE   "C"       TO   EDIT-CURSOR  OF  SOKCD
          MOVE   "R"       TO   EDIT-OPTION  OF  SOKCD
     ELSE
          MOVE    SOKCD          TO   SOK-F01
          PERFORM    SOK-READ-SEC
          IF      INV-FLG   =   0
                  MOVE   SOK-F02   TO   SOKMEI
          ELSE
                  MOVE   SPACE     TO   SOKMEI
                  MOVE    3        TO   ERR-CD
                  MOVE   "C"       TO   EDIT-CURSOR  OF  SOKCD
                  MOVE   "R"       TO   EDIT-OPTION  OF  SOKCD
          END-IF
     END-IF.
*出力年月日
     MOVE     "1"                 TO   LINK-IN-KBN.
     MOVE     SYY                 TO   WK-SYY.
     MOVE     SMM                 TO   WK-SMM.
     MOVE     SDD                 TO   WK-SDD.
     MOVE     WK-SYMD             TO   LINK-IN-YMD6.
     CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD.
     IF        LINK-OUT-RET   =   ZERO
            MOVE     "3"               TO   LINK-IN-KBN
            MOVE     WK-SYMD           TO   LINK-IN-YMD6
            CALL     "SKYDTCKB"     USING   LINK-IN-KBN
                                            LINK-IN-YMD6
                                            LINK-IN-YMD8
                                            LINK-OUT-RET
                                            LINK-OUT-YMD
            MOVE     LINK-OUT-YMD      TO   WK-SDATE
     ELSE
            MOVE     ZERO              TO   WK-SDATE
            IF    ERR-CD    =    0
                   MOVE    4        TO   ERR-CD
            END-IF
            MOVE   "C"              TO   EDIT-CURSOR OF SYY
            MOVE   "R"              TO   EDIT-OPTION OF SYY
                                         EDIT-OPTION OF SMM
                                         EDIT-OPTION OF SDD
     END-IF.
*エラー区分チェック
     IF      ERR-CD   =   0
             MOVE    2            TO   SYR-FLG
     END-IF.
 CHK-INPUT-EXIT.
     EXIT.
************************************************************
*      <2.3>  確認入力処理                                 *
************************************************************
*確認入力
 DSP-KAKUNIN-SEC         SECTION.
     MOVE    PFMSG01              TO   PFMSG.
     MOVE    "KAKNIN"             TO   WK-GROUP.
     IF   ERR-CD  =  ZERO
          MOVE    "Y"    TO    KAKU
     END-IF.
     PERFORM    DSP-RD-SEC.
     EVALUATE    DSP-FUNC
         WHEN    "F004"
             MOVE    0            TO   SYR-FLG
         WHEN    "F005"
             MOVE    "END"        TO   END-FLG
         WHEN    "F006"
             MOVE    1            TO   SYR-FLG
         WHEN    "E000"
*            PERFORM    KAKUNIN-CHK-SEC
             IF   KAKU    =  "Y"
                  PERFORM    KAKUNIN-CHK-SEC
             ELSE
                  MOVE    5            TO   ERR-CD
             END-IF
         WHEN    OTHER
             MOVE    1            TO   ERR-CD
     END-EVALUATE.
     IF   ERR-CD  =  ZERO
          MOVE   SPACE   TO    KAKU
     END-IF.
 DSP-KAKUNIN-EXIT.
     EXIT.
*----------------------------------------------------------*
*                        確認処理                          *
*----------------------------------------------------------*
 KAKUNIN-CHK-SEC          SECTION.
     PERFORM    NYU-READ-SEC.
     PERFORM    UNTIL   END-FLG   =   "END"
            PERFORM    PRINT-SEC
     END-PERFORM.
 KAKUNIN-CHK-EXIT.
     EXIT.
*----------------------------------------------------------*
*                    出力編集処理                          *
*----------------------------------------------------------*
 PRINT-SEC              SECTION.
*
     IF     L-CNT       >=   56
            PERFORM    MIDASHI-SEC
     END-IF.
     PERFORM   HENSYU-SEC.
     PERFORM   NYU-READ-SEC.
 PRINT-EXIT.
     EXIT.
*----------------------------------------------------------*
*                       見出し処理                         *
*----------------------------------------------------------*
 MIDASHI-SEC            SECTION.
*プリンターＦクリア
     MOVE    SPACE                TO   P-REC.
     INITIALIZE                        P-REC.
*頁カウントアップ／頁数セット
     ADD      1                   TO   P-CNT.
     MOVE     P-CNT               TO   H-PAGE.
*行カウント初期化
     MOVE     ZERO                TO   L-CNT.
*実績出力日
     MOVE     WK-SDATE(1:4)       TO   H0-JISEKI-YY.
     MOVE     WK-SDATE(5:2)       TO   H0-JISEKI-MM.
     MOVE     WK-SDATE(7:2)       TO   H0-JISEKI-DD.
*倉庫名称取得
     MOVE     SOKCD          TO      SOK-F01  H1-SOKCD.
     PERFORM  SOK-READ-SEC.
     IF       INV-FLG   =    0
              MOVE   SOK-F02         TO   H1-SOKNM
     ELSE
              MOVE   ALL NC"＊"      TO   H1-SOKNM
     END-IF.
*頁カウントが１以外の場合
     IF       P-CNT   NOT =  1
              WRITE    P-REC         AFTER   PAGE
     END-IF.
     WRITE    P-REC         FROM    HYODAI      AFTER  2.
     WRITE    P-REC         FROM    MIDASHI-0   AFTER  1.
     WRITE    P-REC         FROM    MIDASHI-1   AFTER  1.
     WRITE    P-REC         FROM    MIDASHI-4   AFTER  1.
     WRITE    P-REC         FROM    MIDASHI-2   AFTER  1.
     WRITE    P-REC         FROM    MIDASHI-3   AFTER  1.
     WRITE    P-REC         FROM    MIDASHI-4   AFTER  1.
     ADD      8                   TO      L-CNT.
 MIDASHI-EXIT.
     EXIT.
*----------------------------------------------------------*
*                      編集処理                            *
*----------------------------------------------------------*
 HENSYU-SEC             SECTION.
     MOVE    SPACE           TO    MEISAI-1  MEISAI-2.
     INITIALIZE              MEISAI-1  MEISAI-2.
*
*----  条件ファイル（作業名称取得）----*
     MOVE    "71"                 TO   JYO-F01.
     MOVE     NYU-F04             TO   JYO-F02  M-SAGYOKBN.
     READ     HJYOKEN
         INVALID
              MOVE    SPACE       TO   M-SAMEISYO
         NOT  INVALID
              MOVE    JYO-F03     TO   M-SAMEISYO
     END-READ.
*
*----  商品名称マスタＲＥＡＤ  ----*
     MOVE     NYU-F05             TO   MEI-F011.
     MOVE     NYU-F06(1:5)        TO   MEI-F0121.
     MOVE     NYU-F06(6:2)        TO   MEI-F0122.
     MOVE     NYU-F06(8:1)        TO   MEI-F0123.
     READ     HMEIMS
         INVALID
              MOVE    SPACE       TO   M-SHOMEI
         NOT  INVALID
              MOVE    MEI-F021    TO   M-SHOMEI(1:15)
              MOVE    MEI-F022    TO   M-SHOMEI(16:15)
     END-READ.
*削除区分
     MOVE     SPACE               TO   M-SAKUJYO.
*伝票番号
     MOVE     NYU-F02             TO   M-GENPYO.
*伝票区分
*****MOVE     NYU-F01             TO   M-DENKBN1.
     IF       NYU-F01  =  31
        IF    NYU-F11   NOT  =   SPACE
              MOVE    NYU-F11     TO   SOK-F01  M-NYUBCD
              PERFORM    SOK-READ-SEC
              IF    INV-FLG   =   1
                    MOVE    SPACE    TO   M-NYUBASYO
              ELSE
                    MOVE    SOK-F02  TO   M-NYUBASYO
              END-IF
        END-IF
     ELSE
        IF    NYU-F10   NOT  =   SPACE
              MOVE    NYU-F10    TO   SOK-F01  M-SYUBCD
              PERFORM    SOK-READ-SEC
              IF    INV-FLG   =   1
                    MOVE    SPACE          TO   M-SYUBASYO
              ELSE
                    MOVE    SOK-F02        TO   M-SYUBASYO
              END-IF
        END-IF
        IF    NYU-F11   NOT  =   SPACE
              MOVE    NYU-F11     TO   SOK-F01  M-NYUBCD
              PERFORM    SOK-READ-SEC
              IF    INV-FLG   =   1
                    MOVE    SPACE    TO   M-NYUBASYO
              ELSE
                    MOVE    SOK-F02  TO   M-NYUBASYO
              END-IF
        END-IF
     END-IF.
*伝票区分名称セット
*****DISPLAY "SOKCD   = " SOKCD UPON CONS.
*****DISPLAY "NYU-F10 = " NYU-F10 UPON CONS.
     IF       SOKCD  =  NYU-F11
              MOVE      NC"入　　　庫　"  TO  M-DENMEI
     ELSE
              IF   SOKCD  =  NYU-F10
                   MOVE NC"出　　　庫　"  TO  M-DENMEI
              ELSE
                   MOVE ALL NC"＊"        TO  M-DENMEI
              END-IF
     END-IF.
*担当者ＣＤ
     MOVE    NYU-F16    TO   M-TANTO.
*入出庫日付
     MOVE     NYU-F15(3:2)        TO   M-HIZ-Y.
     MOVE     NYU-F15(5:2)        TO   M-HIZ-M.
     MOVE     NYU-F15(7:2)        TO   M-HIZ-D.
     MOVE     "."                 TO   M-HIZ-1  M-HIZ-2.
*行番番号
     MOVE     NYU-F03             TO   M-GYO.
*伝票区分＝３１（入庫）の場合
     IF    NYU-F01         =   31
*      入庫側_番
       IF  NYU-F08     NOT =  SPACE
           MOVE     NYU-F08(1:1)  TO   WK-TANA1
           MOVE     NYU-F08(2:3)  TO   WK-TANA2
           MOVE     NYU-F08(5:2)  TO   WK-TANA3
           MOVE     WK-TANABAN    TO   M-TANABAN
       END-IF
*      出庫側_番
       IF  NYU-F07     NOT =  SPACE
           MOVE     NYU-F07(1:1)  TO   WK-TANA1
           MOVE     NYU-F07(2:3)  TO   WK-TANA2
           MOVE     NYU-F07(5:2)  TO   WK-TANA3
           MOVE     WK-TANABAN    TO   M-AITE
       END-IF
     ELSE
*      出庫側_番
       IF  NYU-F07     NOT =  SPACE
           MOVE     NYU-F07(1:1)  TO   WK-TANA1
           MOVE     NYU-F07(2:3)  TO   WK-TANA2
           MOVE     NYU-F07(5:2)  TO   WK-TANA3
           MOVE     WK-TANABAN    TO   M-TANABAN
       END-IF
*      入庫側_番
       IF  NYU-F08     NOT =  SPACE
           MOVE     NYU-F08(1:1)  TO   WK-TANA1
           MOVE     NYU-F08(2:3)  TO   WK-TANA2
           MOVE     NYU-F08(5:2)  TO   WK-TANA3
           MOVE     WK-TANABAN    TO   M-AITE
       END-IF
     END-IF.
*商品ＣＤ
     MOVE     NYU-F05             TO   M-SHOCD.
*品単ＣＤ
     MOVE     NYU-F06             TO   M-HINTAN.
*数量
     MOVE     NYU-F12             TO   M-SURYO.
*ストック番号
     MOVE     NYU-F09(1:4)        TO   M-STOKU1.
     MOVE     NYU-F09(5:1)        TO   M-STOKU2.
*備考
     MOVE     NYU-F13             TO   M-BIKOU.
*伝票番号がブレイクしたら
     IF    WK-DENBAN   NOT  =   NYU-F02
           PERFORM    WRT1-SEC
     END-IF.
     PERFORM    WRT2-SEC.
 HENSYU-EXIT.
     EXIT.
*----------------------------------------------------------*
*                     出力処理 1                           *
*----------------------------------------------------------*
 WRT1-SEC               SECTION.
     IF    (L-CNT   >=   56)      AND
           (END-FLG   NOT  =   "END")
            PERFORM    MIDASHI-SEC
     END-IF.
     IF    (L-CNT   >    8 )
            WRITE   P-REC         FROM   SEN
            ADD     1             TO        L-CNT
     END-IF.
     WRITE   P-REC                FROM   MEISAI-1.
     ADD      1                   TO        L-CNT.
     MOVE    NYU-F02              TO   WK-DENBAN.
 WRT1-EXIT.
     EXIT.
*----------------------------------------------------------*
*                     出力処理 2                           *
*----------------------------------------------------------*
 WRT2-SEC               SECTION.
*
     IF    (L-CNT   >=   56)     AND
           (END-FLG   NOT  =   "END")
            PERFORM    MIDASHI-SEC
     END-IF.
     WRITE    P-REC               FROM   MEISAI-2.
     ADD      1                   TO        L-CNT.
 WRT2-EXIT.
     EXIT.
*----------------------------------------------------------*
*-----  倉庫マスタ ＲＥＡＤ処理
*----------------------------------------------------------*
 SOK-READ-SEC        SECTION.
     MOVE    0                    TO  INV-FLG.
     READ    ZSOKMS
         INVALID KEY
             MOVE  1              TO  INV-FLG
     END-READ.
 SOK-READ-EXIT.
     EXIT.
*----------------------------------------------------------*
*-----  入出庫ファイルＲＥＡＤ処理
*----------------------------------------------------------*
 NYU-READ-SEC       SECTION.
     READ     NYSFILF
        AT  END
            MOVE   "END"          TO  END-FLG
            GO  TO  NYU-READ-EXIT
        NOT  AT  END
            ADD   1               TO  NYU-IN
     END-READ.
*出力判定（入荷場所／出荷場所の何れかが同一の場合）
     IF     NYU-F10      =    SOKCD
     OR     NYU-F11      =    SOKCD
            CONTINUE
     ELSE
            ADD   1               TO  NYU-SKIP
            GO                    TO  NYU-READ-SEC
     END-IF.
*取消フラグが取消の場合対象外
     IF     NYU-F96  =  1
            ADD   1               TO  NYU-SKIP
            GO                    TO  NYU-READ-SEC
     END-IF.
*登録日／修正日チェック
     IF   ( WK-SDATE    >    NYU-F98   OR
            WK-SDATE    <    NYU-F98 )    AND
          ( WK-SDATE    >    NYU-F99   OR
            WK-SDATE    <    NYU-F99 )
            ADD   1               TO  NYU-SKIP
            GO  TO  NYU-READ-SEC
     END-IF.
*
     ADD    1                     TO  NYU-OUT.
 NYU-READ-EXIT.
     EXIT.
*----------------------------------------------------------*
*-----  条件ファイルＲＥＡＤ
*----------------------------------------------------------*
 JYO-READ-SEC           SECTION.
     MOVE   ZERO                  TO   INV-FLG.
     READ   HJYOKEN
            INVALID  KEY
                MOVE    1         TO   INV-FLG
     END-READ.
 JYO-READ-EXIT.
     EXIT.
*----------------------------------------------------------*
*                  属性クリア処理                          *
*----------------------------------------------------------*
 ERR-CLR-SEC              SECTION.
     MOVE   " "                  TO   EDIT-CURSOR  OF  SOKCD
                                      EDIT-CURSOR  OF  SYY
                                      EDIT-CURSOR  OF  SMM
                                      EDIT-CURSOR  OF  SDD
                                      EDIT-CURSOR  OF  KAKU.
     MOVE   "M"                  TO   EDIT-OPTION  OF  SOKCD
                                      EDIT-OPTION  OF  SYY
                                      EDIT-OPTION  OF  SMM
                                      EDIT-OPTION  OF  SDD
                                      EDIT-OPTION  OF  KAKU.
 ERR-CLR-EXIT.
     EXIT.
************************************************************
*      4.0        アベンド処理                             *
************************************************************
 ABEND-SEC              SECTION.
     MOVE      SQLSTATE           TO         ERR-STCD.
     DISPLAY   MSG-ABEND1         UPON       CONS.
     DISPLAY   MSG-ABEND2         UPON       CONS.
     MOVE     "    "              TO         PROGRAM-STATUS.
     STOP      RUN.
 ABEND-EXIT.
     EXIT.
*****************<<  PROGRAM  END  >>***********************
