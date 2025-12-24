************************************************************
*    顧客名          :  （株）サカタのタネ殿               *
*    システム名      :  在庫システム　　　　               *
*    プログラム名    :  入出庫入力チェックリスト　　　　   *
*    プログラムＩＤ  :  ＺＮＹ００４０Ｂ                   *
*    作成者          :  ＮＡＶ　　　                       *
*    作成日          :  93.05.14                           *
************************************************************
*
 IDENTIFICATION         DIVISION.
 PROGRAM-ID.            ZNY0040B.
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
     SELECT   ZNYUSDT   ASSIGN    TO      DA-VI-ZNYUSDT1
                        FILE STATUS          IS  NYU-STS
                        ORGANIZATION         IS  INDEXED
                        ACCESS  MODE         IS  SEQUENTIAL
                        RECORD  KEY          IS  NYU-F02  NYU-F03.
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
 COPY  ZNY0040          OF   XMDLIB.
****<<  入出庫ファイル  　  >>******************************
 FD    ZNYUSDT.
 COPY  ZNYUSDT          OF   XFDLIB
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
****  日付保存　****
 01  SYSTEM-HIZUKE.
     03  SYSYMD           PIC  9(06).
     03  SYSYMD-R         REDEFINES  SYSYMD.
       05  SYS-YY            PIC  99.
       05  SYS-MM            PIC  99.
       05  SYS-DD            PIC  99.
 01  WK-HIZUKE.
     03  WK-YY            PIC  9(02).
     03  FILLER           PIC  X(01)  VALUE  ".".
     03  WK-MM            PIC  9(02).
     03  FILLER           PIC  X(01)  VALUE  ".".
     03  WK-DD            PIC  9(02).
*
****  カウンタ  ****
 01  CNT-AREA.
     03  L-CNT               PIC  9(02)  VALUE  99.
     03  P-CNT               PIC  9(04)  VALUE  ZERO.
 01  KEN-AREA1.
     03  NYU-IN              PIC  9(07)  VALUE  ZERO.
     03  NYU-OUT             PIC  9(07)  VALUE  ZERO.
     03  NYU-SKIP            PIC  9(07)  VALUE  ZERO.
 01  KEN-AREA2.
     03  NYU-IN1             PIC  ZZZZZZ9 VALUE  ZERO.
     03  NYU-OUT1            PIC  ZZZZZZ9 VALUE  ZERO.
     03  NYU-SKIP1           PIC  ZZZZZZ9 VALUE  ZERO.
     03  NYU-PAGE1           PIC  ZZZZZZ9 VALUE  ZERO.
*
****  ワーク領域  ****
 01  WORK-AREA.
     03  WK-SOKCD            PIC  9(02)  VALUE  ZERO.
     03  WK-SEIREKI          PIC  9(08)  VALUE  ZERO.
     03  WK-DENBAN           PIC  9(07)  VALUE  ZERO.
     03  WK-JYO-F04          PIC  9(02).
     03  WK-JYO-SOKCD
                  REDEFINES  WK-JYO-F04    PIC  X(02).
 01  WK-TANABAN.
     03  WK-TANA1         PIC  X(01).
     03  FILLER           PIC  X(01)  VALUE  "-".
     03  WK-TANA2         PIC  X(03).
     03  FILLER           PIC  X(01)  VALUE  "-".
     03  WK-TANA3         PIC  X(02).
 01  G-YMD.
     03  G-YY               PIC  9(02)  VALUE  ZERO.
     03  G-MM               PIC  9(02)  VALUE  ZERO.
     03  G-DD               PIC  9(02)  VALUE  ZERO.
 01  CHK-YMD.
     03  CHK-YY             PIC  9(04)  VALUE  ZERO.
     03  CHK-MM             PIC  9(02)  VALUE  ZERO.
     03  CHK-DD             PIC  9(02)  VALUE  ZERO.
 01  CHK-DATE-WORK.
     03  CHK-01             PIC  9(04)  VALUE  ZERO.
     03  CHK-02             PIC  9(02)  VALUE  ZERO.
     03  MATUBI             PIC  X(24)
                            VALUE  "312831303130313130313031".
     03  FILLER             REDEFINES  MATUBI.
         05  WK-MATUBI      PIC  9(02)
                            OCCURS  12.
****  見出し行（表題）  ****
 01  HYODAI.
     03  FILLER              PIC  X(34)  VALUE  SPACE.
     03  FILLER              PIC  N(20)  VALUE
         NC"＊＊＊　入出庫入力チェックリスト　＊＊＊"
                             CHARACTER   TYPE   IS   3-0PITCH.
     03  FILLER              PIC  X(06)  VALUE  SPACE.
     03  FILLER              PIC  N(03)  VALUE  NC"処理日"
                             CHARACTER   TYPE   IS   2-0PITCH.
     03  FILLER              PIC  X(01)  VALUE  ":".
     03  H-YY                PIC  Z9.
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
****  見出し行１  ****
 01  MIDASHI-1       CHARACTER    TYPE  IS  2-0PITCH.
     03  FILLER              PIC  X(07)  VALUE  SPACE.
     03  FILLER              PIC  N(03)  VALUE  NC"原票_".
     03  FILLER              PIC  X(02)  VALUE  SPACE.
     03  FILLER              PIC  N(04)  VALUE  NC"伝票区分".
     03  FILLER              PIC  X(04)  VALUE  SPACE.
     03  FILLER              PIC  N(04)  VALUE  NC"作業区分".
     03  FILLER              PIC  X(08)  VALUE  SPACE.
     03  FILLER              PIC  N(04)  VALUE  NC"入出庫日".
     03  FILLER              PIC  X(01)  VALUE  SPACE.
     03  FILLER              PIC  N(04)  VALUE  NC"入庫場所".
     03  FILLER              PIC  X(05)  VALUE  SPACE.
     03  FILLER              PIC  N(04)  VALUE  NC"出庫場所".
     03  FILLER              PIC  X(05)  VALUE  SPACE.
     03  FILLER              PIC  N(02)  VALUE  NC"担当".
****  見出し行２  ****
 01  MIDASHI-2       CHARACTER    TYPE  IS  2-0PITCH.
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
*
****  明細１  ****
 01  MEISAI-1.
     03  M-SAKUJYO           PIC  N(02)
                             CHARACTER   TYPE   IS   2-0PITCH.
     03  FILLER              PIC  X(03)  VALUE  SPACE.
     03  M-GENPYO            PIC  9(07).
     03  FILLER              PIC  X(01)  VALUE  SPACE.
     03  M-DENKBN1           PIC  9(02).
     03  FILLER              PIC  X(01)  VALUE  SPACE.
     03  M-DENMEI            PIC  N(06)
                             CHARACTER   TYPE   IS   1-5PITCH.
     03  M-SAGYOKBN          PIC  X(02).
     03  FILLER              PIC  X(01)  VALUE  SPACE.
     03  M-SAMEISYO          PIC  N(08)
                             CHARACTER   TYPE   IS   1-5PITCH.
     03  FILLER              PIC  X(01)  VALUE  SPACE.
     03  M-HIZUKE            PIC  X(08).
     03  FILLER              PIC  X(01)  VALUE  SPACE.
     03  M-NYUBCD            PIC  X(02).
     03  FILLER              PIC  X(01)  VALUE  SPACE.
     03  M-NYUBASYO          PIC  N(06)
                             CHARACTER   TYPE   IS   1-5PITCH.
     03  FILLER              PIC  X(01)  VALUE  SPACE.
     03  M-SYUBCD            PIC  X(02).
     03  FILLER              PIC  X(01)  VALUE  SPACE.
     03  M-SYUBASYO          PIC  N(05)
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
       05  ERR-PG-ID         PIC  X(08)  VALUE  "ZNY0040B".
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
            NC"無効ＰＦキーです。".
     03  MSG-ERR2            PIC  N(20)  VALUE
            NC"倉庫コードを入力してください。".
     03  MSG-ERR3            PIC  N(20)  VALUE
            NC"倉庫コードに誤りがあります。".
     03  MSG-ERR4            PIC  N(20)  VALUE
            NC"対象年月日に誤りがあります。".
     03  MSG-ERR4            PIC  N(20)  VALUE
            NC"Ｙで入力して下さい。".
 01  ERR-MSG-ALL             REDEFINES   ERR-TAB.
     03  ERR-MSG             PIC  N(20)
                             OCCURS   5  TIMES.
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
****  パラメータ  ****
 LINKAGE                SECTION.
 01  PARA-AREA.
     03  PARA-WKSTN          PIC  X(08).
************************************************************
*      ＭＡＩＮ              ＭＯＤＵＬＥ                  *
************************************************************
*
 PROCEDURE           DIVISION     USING  PARA-AREA.
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
                  PROCEDURE   ZNYUSDT.
     MOVE   "ZNYUSDT "            TO   ERR-FL-ID.
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
 ZNY0040B-START         SECTION.
     PERFORM            INIT-SEC.
     PERFORM            MAIN-SEC
                        UNTIL      END-FLG  =    "END".
     PERFORM            END-SEC.
     STOP               RUN.
 ZNY0040B-END.
     EXIT.
************************************************************
*      <1.0>    初期処理                                   *
************************************************************
 INIT-SEC               SECTION.
     DISPLAY    "*** ZNY0040B START ***"     UPON    CONS.
*----  ファイルＯＰＥＮ  ----*
     OPEN     I-O      DSPF.
     OPEN     INPUT    ZNYUSDT  ZSOKMS  HMEIMS  HJYOKEN.
     OPEN     OUTPUT   PRINTF.
*----  システム日付の格納  ----*
     ACCEPT   SYSYMD              FROM    DATE.
     MOVE     SYS-YY              TO   H-YY.
     MOVE     SYS-MM              TO   H-MM.
     MOVE     SYS-DD              TO   H-DD.
*----  条件ファイル索引（倉庫コード取得）  ----*
     DISPLAY "PARA-WKSTN = " PARA-WKSTN UPON CONS.
     MOVE    "65"                 TO   JYO-F01.
     MOVE     PARA-WKSTN          TO   JYO-F02.
     PERFORM  JYO-READ-SEC.
     IF       INV-FLG   =   1
         DISPLAY  "HJYOKEN  INV   KEY  =  65"   UPON  STAT
         MOVE     "END"           TO    END-FLG
         GO  TO    INIT-EXIT
     END-IF.
     MOVE     JYO-F04             TO   WK-JYO-F04  WK-SOKCD.
*----  日付処理（西暦に直す）  ----*
     MOVE    "57"                 TO   JYO-F01.
     MOVE     SPACE               TO   JYO-F02.
     PERFORM  JYO-READ-SEC.
     IF       INV-FLG   =   1
         DISPLAY  "HJYOKEN  INV   KEY  =  57"   UPON  STAT
         MOVE     "END"           TO    END-FLG
         GO  TO    INIT-EXIT
     END-IF.
     COMPUTE  WK-SEIREKI = JYO-F04 * 10000 + SYSYMD.
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
     CLOSE    DSPF  ZNYUSDT  ZSOKMS  HMEIMS  HJYOKEN  PRINTF.
     MOVE     NYU-IN             TO  NYU-IN1.
     MOVE     NYU-OUT            TO  NYU-OUT1.
     MOVE     NYU-SKIP           TO  NYU-SKIP1.
     MOVE     P-CNT              TO  NYU-PAGE1.
     DISPLAY "ﾆｭｳｼｭｯｺ ﾌｧｲﾙ   (IN) = "  NYU-IN1   UPON  CONS.
     DISPLAY "ﾆｭｳｼｭｯｺ ﾌｧｲﾙ  (OUT) = "  NYU-OUT1  UPON  CONS.
     DISPLAY "ﾆｭｳｼｭｯｺ ﾌｧｲﾙ (SKIP) = "  NYU-SKIP1 UPON  CONS.
     DISPLAY "ﾁｪｯｸﾘｽﾄ      (PAGE) = "  NYU-PAGE1 UPON  CONS.
     DISPLAY "*** ZNY0040B  END  ***"    UPON   CONS.
 END-EXIT.
     EXIT.
************************************************************
*      <2.1>  画面初期処理                                 *
************************************************************
 DSP-INIT-SEC            SECTION.
*属性のクリア
     MOVE   SPACE                TO   ZNY0040   DSP-CONTROL.
     PERFORM   ERR-CLR-SEC.
*倉庫コードのセット
     IF      WK-SOKCD    NOT  =  1
             MOVE    WK-SOKCD     TO    SOKCD  SOK-F01
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
     MOVE    SYS-YY               TO   GYY.
     MOVE    SYS-MM               TO   GMM.
     MOVE    SYS-DD               TO   GDD.
*画面初期表示
     MOVE    SPACE                TO   DSP-CONTROL.
     MOVE    "ZNY0040"            TO   DSP-FORMAT.
     MOVE    "ALLF"               TO   DSP-GROUP.
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
     MOVE    "ALLF"               TO    DSP-GROUP.
     PERFORM    DSP-WRITE-SEC.
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
     WRITE    ZNY0040.
 DSP-WRITE-EXIT.
     EXIT.
*----------------------------------------------------------*
*                     入力チェック                         *
*----------------------------------------------------------*
 CHK-INPUT-SEC          SECTION.
     MOVE    SOKCD                TO   SOK-F01.
     PERFORM    SOK-READ-SEC.
     IF      INV-FLG   =   0
             MOVE    SOK-F02      TO   SOKMEI
     ELSE
             MOVE    3            TO   ERR-CD
             MOVE   "C"           TO   EDIT-CURSOR  OF  SOKCD
             MOVE   "R"           TO   EDIT-OPTION  OF  SOKCD
     END-IF.
     MOVE    GYY                  TO   CHK-YY(3:2)  G-YY
     MOVE    GMM                  TO   CHK-MM       G-MM
     MOVE    GDD                  TO   CHK-DD       G-DD
     PERFORM    DATE-CHECK-SEC.
     IF      ERR-CD   =   0
             MOVE    2            TO   SYR-FLG
     END-IF.
 CHK-INPUT-EXIT.
     EXIT.
*----------------------------------------------------------*
*     LEVEL ALL     日付チェック                           *
*----------------------------------------------------------*
 DATE-CHECK-SEC         SECTION.
*## 1999/12/20 NAV
*****COMPUTE  CHK-YY  =  WK-SEIREKI  +  CHK-YY.
*****DISPLAY "CHK-YMD1 = " CHK-YMD UPON CONS.
     MOVE     "3"                 TO   LINK-IN-KBN.
     MOVE     G-YMD               TO   LINK-IN-YMD6.
     MOVE     ZERO                TO   LINK-IN-YMD8.
     MOVE     ZERO                TO   LINK-OUT-RET.
     MOVE     ZERO                TO   LINK-OUT-YMD.
     CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD.
     MOVE      LINK-OUT-YMD       TO   CHK-YMD.
*****DISPLAY "LINK-OUT-YMD = " LINK-OUT-YMD UPON CONS.
*****DISPLAY "CHK-YMD2 = " CHK-YMD UPON CONS.
     DIVIDE   CHK-YY  BY  4  GIVING  CHK-01  REMAINDER  CHK-01.
     IF       CHK-02  =  0
              MOVE    29          TO    WK-MATUBI (2)
     ELSE
              MOVE    28          TO    WK-MATUBI (2)
     END-IF.
     IF       CHK-MM  =  0   OR  >  12
       IF     ERR-CD  =  0
              MOVE    4           TO    ERR-CD
              MOVE    "C"         TO    EDIT-CURSOR  OF  GMM
       END-IF
              MOVE    "R"         TO    EDIT-OPTION  OF  GMM
     END-IF.
     IF       CHK-DD  =  0  OR  CHK-DD  >  WK-MATUBI (CHK-MM)
       IF     ERR-CD  =  0
              MOVE    4           TO    ERR-CD
              MOVE    "C"         TO    EDIT-CURSOR  OF  GDD
       END-IF
              MOVE    "R"         TO    EDIT-OPTION  OF  GDD
     END-IF.
 DATE-CHECK-EXIT.
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
*93.10.16 START
*****IF     L-CNT    >=    64
     IF     L-CNT    >=    56
*93.10.16 END
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
     MOVE    SPACE                TO   P-REC.
     INITIALIZE                        P-REC.
     ADD      1                   TO   P-CNT.
     MOVE     P-CNT               TO   H-PAGE.
     MOVE     ZERO                TO   L-CNT.
     IF       P-CNT  =  1
              WRITE    P-REC         FROM    HYODAI      AFTER  2
              WRITE    P-REC         FROM    MIDASHI-1   AFTER  2
              WRITE    P-REC         FROM    MIDASHI-2
     ELSE
              WRITE    P-REC         AFTER   PAGE
              WRITE    P-REC         FROM    HYODAI      AFTER  2
              WRITE    P-REC         FROM    MIDASHI-1   AFTER  2
              WRITE    P-REC         FROM    MIDASHI-2
     END-IF.
     ADD      5                   TO      L-CNT.
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
*
     IF   NYU-F96   =   1
          MOVE    NC"削除"        TO   M-SAKUJYO
     ELSE
          MOVE    SPACE           TO   M-SAKUJYO
     END-IF.
     MOVE     NYU-F02             TO   M-GENPYO.
     MOVE     NYU-F01             TO   M-DENKBN1.
     IF       NYU-F01  =  31
        MOVE    NC"入　　　庫　"  TO   M-DENMEI
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
***** < 93/06/04 START > 他倉庫からの移動入庫出力
        MOVE  NC"出　　　庫　"    TO   M-DENMEI
        IF    NYU-F10   NOT  =   SOKCD
              MOVE  NC"入　　　庫　"
                                  TO   M-DENMEI
        END-IF
*
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
*
     MOVE    NYU-F16    TO   M-TANTO.
*
***** < 93/06/04  END  > 他倉庫からの移動入庫出力
**** MOVE    NYU-F10    TO   SOK-F01  M-SYUBCD.
**** PERFORM    SOK-READ-SEC.
**** IF    INV-FLG   =   1
****       MOVE    SPACE          TO   M-SYUBASYO
**** ELSE
****       MOVE    SOK-F02        TO   M-SYUBASYO
**** END-IF.
     MOVE     NYU-F15(3:2)        TO   WK-YY.
     MOVE     NYU-F15(5:2)        TO   WK-MM.
     MOVE     NYU-F15(7:2)        TO   WK-DD.
     MOVE     WK-HIZUKE           TO   M-HIZUKE.
     MOVE     NYU-F03             TO   M-GYO.
     IF    NYU-F01         =   31
       IF  NYU-F08     NOT =  SPACE
           MOVE     NYU-F08(1:1)  TO   WK-TANA1
           MOVE     NYU-F08(2:3)  TO   WK-TANA2
           MOVE     NYU-F08(5:2)  TO   WK-TANA3
           MOVE     WK-TANABAN    TO   M-TANABAN
       END-IF
     ELSE
       IF  NYU-F07     NOT =  SPACE
           MOVE     NYU-F07(1:1)  TO   WK-TANA1
           MOVE     NYU-F07(2:3)  TO   WK-TANA2
           MOVE     NYU-F07(5:2)  TO   WK-TANA3
           MOVE     WK-TANABAN    TO   M-TANABAN
       END-IF
     END-IF.
     MOVE     NYU-F05             TO   M-SHOCD.
     MOVE     NYU-F06             TO   M-HINTAN.
     MOVE     NYU-F12             TO   M-SURYO.
     MOVE     NYU-F09(1:4)        TO   M-STOKU1.
     MOVE     NYU-F09(5:1)        TO   M-STOKU2.
     MOVE     NYU-F13             TO   M-BIKOU.
     IF   (WK-DENBAN   NOT  =   NYU-F02)   OR
          (L-CNT            =   5)
           PERFORM    WRT1-SEC
     END-IF.
     PERFORM    WRT2-SEC.
 HENSYU-EXIT.
     EXIT.
*----------------------------------------------------------*
*                     出力処理 1                           *
*----------------------------------------------------------*
 WRT1-SEC               SECTION.
*****IF    (L-CNT   >=   62)      AND
     IF    (L-CNT   >=   56)      AND
           (END-FLG   NOT  =   "END")
            PERFORM    MIDASHI-SEC
     END-IF.
     MOVE    SPACE                TO   P-REC.
     INITIALIZE                        P-REC.
     WRITE   P-REC.
     WRITE   P-REC                FROM   MEISAI-1.
     ADD      2                   TO        L-CNT.
     MOVE    NYU-F02              TO   WK-DENBAN.
 WRT1-EXIT.
     EXIT.
*----------------------------------------------------------*
*                     出力処理 2                           *
*----------------------------------------------------------*
 WRT2-SEC               SECTION.
*93.10.16 START
*****IF    (L-CNT   >=   64)     AND
     IF    (L-CNT   >=   56)     AND
*93.10.16 END
           (END-FLG   NOT  =   "END")
            PERFORM    MIDASHI-SEC
     END-IF.
     WRITE    P-REC               FROM   MEISAI-2.
     ADD      1                   TO        L-CNT.
 WRT2-EXIT.
     EXIT.
*
*-----  倉庫マスタ ＲＥＡＤ処理  -----*
 SOK-READ-SEC        SECTION.
     MOVE    0                    TO  INV-FLG.
     READ    ZSOKMS
         INVALID KEY
             MOVE  1              TO  INV-FLG
     END-READ.
 SOK-READ-EXIT.
     EXIT.
*
*-----  入出庫ファイルＲＥＡＤ処理  -----*
 NYU-READ-SEC       SECTION.
     READ     ZNYUSDT
        AT  END
             MOVE   "END"         TO  END-FLG
            GO  TO  NYU-READ-EXIT
        NOT  AT  END
            ADD   1               TO  NYU-IN
     END-READ.
*
     IF    (NYU-F01      =    31)     AND
           (NYU-F11  NOT =    SPACE)
        IF  SOKCD    NOT =    NYU-F11
            ADD   1               TO  NYU-SKIP
            GO  TO  NYU-READ-SEC
        END-IF
     ELSE
***** < 93/06/04 他倉庫からの移動入庫分出力
        IF  SOKCD    NOT =    NYU-F10
            IF     SOKCD    NOT =    NYU-F11
                   ADD   1               TO  NYU-SKIP
                   GO  TO  NYU-READ-SEC
            END-IF
        END-IF
*06/04* IF  SOKCD    NOT =    NYU-F10
*06/04*     ADD   1               TO  NYU-SKIP
*06/04*     GO  TO  NYU-READ-SEC
*06/04* END-IF
     END-IF.
*
*## 1999/12/20 NAV
     MOVE     "3"                 TO   LINK-IN-KBN.
     MOVE     G-YMD               TO   LINK-IN-YMD6.
     MOVE     ZERO                TO   LINK-IN-YMD8.
     MOVE     ZERO                TO   LINK-OUT-RET.
     MOVE     ZERO                TO   LINK-OUT-YMD.
     CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD.
*****IF    (G-YMD    NOT =  NYU-F98(3:6))     AND
***********(G-YMD    NOT =  NYU-F99(3:6))
*****IF    (LINK-OUT-YMD  NOT =  NYU-F98(3:6))     AND
*****      (LINK-OUT-YMD  NOT =  NYU-F99(3:6))
     IF    (LINK-OUT-YMD  NOT =  NYU-F98)     AND
           (LINK-OUT-YMD  NOT =  NYU-F99)
            ADD   1               TO  NYU-SKIP
            GO  TO  NYU-READ-SEC
     END-IF.
     ADD    1                     TO  NYU-OUT.
 NYU-READ-EXIT.
     EXIT.
*
*-----  条件ファイルＲＥＡＤ  -----*
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
                                      EDIT-CURSOR  OF  GYY
                                      EDIT-CURSOR  OF  GMM
                                      EDIT-CURSOR  OF  GDD
                                      EDIT-CURSOR  OF  KAKU.
     MOVE   "M"                  TO   EDIT-OPTION  OF  SOKCD
                                      EDIT-OPTION  OF  GYY
                                      EDIT-OPTION  OF  GMM
                                      EDIT-OPTION  OF  GDD
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