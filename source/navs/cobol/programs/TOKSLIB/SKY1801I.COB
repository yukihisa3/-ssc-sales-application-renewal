****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　在庫管理システム　　　　　　　　　*
*    モジュール名　　　　：　ユポラベル印刷（入力）            *
*    作成日／更新日　　　：　93/06/25                          *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　画面よりユポラベルＦの登録・修正を*
*                            行う。                            *
*                                                              *
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SKY1801I.
 AUTHOR.                TOMOKO.S.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FACOM-K670.
 OBJECT-COMPUTER.       FACOM-K670.
 SPECIAL-NAMES.
         STATION   IS   STAT
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*---<<  画面ファイル  >>---*
     SELECT   DSPF      ASSIGN    TO        GS-DSPF
                        ORGANIZATION        IS   SEQUENTIAL
                        ACCESS    MODE      IS   SEQUENTIAL
                        SYMBOLIC  DESTINATION    IS  "DSP"
                        PROCESSING MODE     IS   DSP-PROC
                        GROUP               IS   DSP-GROUP
                        FORMAT              IS   DSP-FORMAT
                        SELECTED  FUNCTION  IS   DSP-FUNC
                        FILE      STATUS    IS   DSP-STATUS.
*
*---<<  ユポラベルＦ  >>---*
     SELECT   ZYUPODT1  ASSIGN    TO        DA-01-VI-ZYUPODT1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   DYNAMIC
                        RECORD    KEY       IS   YUP-F02
                                                 YUP-F03
                                                 YUP-F011
                                                 YUP-F012
                        FILE      STATUS    IS   YUP-STATUS.
*
*---<<  商品コード変換ＴＢＬ >>---*
     SELECT   SHOTBL1   ASSIGN    TO        DA-01-VI-SHOTBL1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   SHO-F01
                                                 SHO-F02
                        FILE      STATUS    IS   SHO-STATUS.
*
*---<<  商品名称マスタ  >>---*
     SELECT   MEIMS1    ASSIGN    TO        DA-01-VI-MEIMS1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   MEI-F011
                                                 MEI-F012
                        FILE      STATUS    IS   MEI-STATUS.
*
*---<<  取引先マスタ  >>---*
     SELECT   TOKMS2    ASSIGN    TO        DA-01-VI-TOKMS2
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   TOK-F01
                        FILE      STATUS    IS   TOK-STATUS.
*
 DATA                   DIVISION.
 FILE                   SECTION.
*---<<  画面ファイル  >>---*
 FD  DSPF.
 01  DSP-AREA                PIC  X(2000).
*    COPY     FKY18011  OF        XMDLIB.
*---<<  ユポラベルＦ  >>---*
 FD  ZYUPODT1.
     COPY     ZYUPODT1  OF        XFDLIB
              JOINING   YUP       PREFIX.
*---<<  商品コード変換ＴＢＬ >>---*
 FD  SHOTBL1.
     COPY     SHOTBL1   OF       XFDLIB
              JOINING   SHO       PREFIX.
*---<<  商品名称マスタ  >>---*
 FD  MEIMS1.
     COPY     MEIMS1    OF       XFDLIB
              JOINING   MEI       PREFIX.
*---<<  取引先マスタ  >>---*
 FD  TOKMS2.
     COPY     TOKMS2    OF        XFDLIB
              JOINING   TOK       PREFIX.
****************************************************************
 WORKING-STORAGE             SECTION.
****************************************************************
****  画面ファイル  ****
 COPY   FKY18011         OF        XMDLIB.
****  画面制御項目  ****
 01  DSP-CONTROL.
     03  DSP-PROC            PIC  X(02).
     03  DSP-GROUP           PIC  X(08).
     03  DSP-FORMAT          PIC  X(08).
     03  DSP-STATUS          PIC  X(02).
     03  DSP-FUNC            PIC  X(04).
****  ステイタス情報  ***
 01  STATUS-AREA.
     02  YUP-STATUS          PIC  X(02).
     02  SHO-STATUS          PIC  X(02).
     02  MEI-STATUS          PIC  X(02).
     02  TOK-STATUS          PIC  X(02).
****  フラグ  ***
 01  PSW-AREA.
     02  END-FLG             PIC  X(03)  VALUE SPACE.
     02  SHO-INV-FLG         PIC  X(03)  VALUE SPACE.
     02  MEI-INV-FLG         PIC  X(03)  VALUE SPACE.
     02  TOR-INV-FLG         PIC  X(03)  VALUE SPACE.
     02  YUP-END-FLG         PIC  X(03)  VALUE SPACE.
     02  YUP-INV-FLG         PIC  X(03)  VALUE SPACE.
     02  CHK-MAE-FLG         PIC  9(01)  VALUE ZERO.
     02  MAIN-FLG            PIC  X(01)  VALUE SPACE.
     02  SYORI-F             PIC  X(01)  VALUE SPACE.
****  処理スイッチ            ****
 01  WK-AREA.
     03  WK-KAIPAGE        OCCURS  99.
         04  WK-SYOCD-MAE    PIC  X(13)  VALUE  SPACE.
         04  WK-SERI1-MAE    PIC  9(04)  VALUE  ZERO.
         04  WK-SERI2-MAE    PIC  9(03)  VALUE  ZERO.
     03  WK-SYOCD-ATO        PIC  X(13)  VALUE  SPACE.
     03  WK-SERI1-ATO        PIC  9(04)  VALUE  ZERO.
     03  WK-SERI2-ATO        PIC  9(03)  VALUE  ZERO.
     03  WK-TORICD           PIC  9(08)  VALUE  ZERO.
     03  PAGE-CNT            PIC  9(03)  VALUE  ZERO.
     03  PRTECT-CNT          PIC  9(01)  VALUE  ZERO.
     03  MEISAI-CNT          PIC  9(01)  VALUE  ZERO.
     03  IN-DATA             PIC  X(01)  VALUE  SPACE.
*
     03  WK-HINTAN.
         05  WK-HINTAN-1     PIC X(05)  VALUE  SPACE.
         05  WK-HINTAN-2     PIC X(02)  VALUE  SPACE.
         05  WK-HINTAN-3     PIC X(01)  VALUE  SPACE.
*
     03  WK-NM2.
         05  WK-NM21         PIC N(07)  VALUE  SPACE.
         05  WK-NM22         PIC N(07)  VALUE  SPACE.
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
*明細行登録／修正チェック
 01  WK-CHK-WORK.
     02  WK-CHKMEI     OCCURS    5.
         03  WK-CHKMEI-G          PIC  9(01)  VALUE  ZERO.
*
 01  WK-CONDEN-AREA.
     02  WK-CONDEN     OCCURS    5.
         03  WK-R10001       PIC X(01).
         03  WK-R10101       PIC X(13).
         03  WK-R10201       PIC X(05).
         03  WK-R10301       PIC X(02).
         03  WK-R10401       PIC X(01).
         03  WK-R10501       PIC N(13).
         03  WK-R10601       PIC X(13).
         03  WK-R10701       PIC N(07).
         03  WK-R11101       PIC N(07).
         03  WK-R10801       PIC X(04).
         03  WK-R10901       PIC 9(05).
         03  WK-R11001       PIC 9(04).
         03  WK-SERI1        PIC 9(04).
         03  WK-SERI2        PIC 9(03).
****  インデックス            ****
 01  IXA                     PIC  9(02).
 01  IXB                     PIC  9(02).
 01  I                       PIC  9(03)    VALUE  ZERO.
 01  A                       PIC  9(03)    VALUE  ZERO.
****  チェックデジット計算
 01  WK-CHKDG-AREA.
   02  WK-CHKDG.
     03  WK-CHKDG-13          PIC 9(01).
     03  WK-CHKDG-12          PIC 9(01).
     03  WK-CHKDG-11          PIC 9(01).
     03  WK-CHKDG-10          PIC 9(01).
     03  WK-CHKDG-9           PIC 9(01).
     03  WK-CHKDG-8           PIC 9(01).
     03  WK-CHKDG-7           PIC 9(01).
     03  WK-CHKDG-6           PIC 9(01).
     03  WK-CHKDG-5           PIC 9(01).
     03  WK-CHKDG-4           PIC 9(01).
     03  WK-CHKDG-3           PIC 9(01).
     03  WK-CHKDG-2           PIC 9(01).
     03  WK-CHKDG-1           PIC 9(01).
*
   02  WK-CHKDG-R  REDEFINES  WK-CHKDG.
     03  WK-CHKDG-13R         PIC X(01).
     03  WK-CHKDG-12R         PIC X(01).
     03  WK-CHKDG-11R         PIC X(01).
     03  WK-CHKDG-10R         PIC X(01).
     03  WK-CHKDG-9R          PIC X(01).
     03  WK-CHKDG-8R          PIC X(01).
     03  WK-CHKDG-7R          PIC X(01).
     03  WK-CHKDG-6R          PIC X(01).
     03  WK-CHKDG-5R          PIC X(01).
     03  WK-CHKDG-4R          PIC X(01).
     03  WK-CHKDG-3R          PIC X(01).
     03  WK-CHKDG-2R          PIC X(01).
     03  WK-CHKDG-1R          PIC X(01).
*
   02  WK-STEP.
     03  WK-STEP1             PIC 9(05)  VALUE  ZERO.
     03  WK-STEP2             PIC 9(05)  VALUE  ZERO.
     03  WK-STEP3             PIC 9(05)  VALUE  ZERO.
     03  WK-STEP4             PIC 9(05)  VALUE  ZERO.
     03  WK-STEP5.
         04  WK-STEP51        PIC 9(01)  VALUE  ZERO.
*
   02  WK-BAISU.
     03  WK-BAISU-1           PIC 9(04)  VALUE  ZERO.
     03  WK-BAISU-2           PIC 9(01)  VALUE  ZERO.
   02  WK-BAISU-R    REDEFINES   WK-BAISU.
     03  WK-BAISU-RR          PIC 9(05).
****  品単右詰め用エリア      ****
 01  WK-HIN-G.
     03  WK-HIN-A       OCCURS      5.
         04  WK-HIN             PIC X(01).
 01  WK-HINTAN-G.
     03  WK-HINTAN-A    OCCURS      5.
        04  WK1-HINTAN          PIC X(01).
 01  WK-BUMON-G.
     03  WK-BUM-A       OCCURS      4.
         04  WK-BUM             PIC X(01).
 01  WK-BUMON1-G.
     03  WK-BUMON1-A    OCCURS      4.
        04  WK1-BUMON           PIC X(01).
 01  WK-SYOCD-G.
     03  WK-SYOCD-SET       OCCURS      5.
         04  WK-SYO             PIC X(08).
         04  WK-RSYO            PIC X(13).
 01  WK-CON-SYOCD-G.
     03  WK-CON-SYOCD-SET   OCCURS      5.
         04  WK-CON-SYO          PIC X(08).
         04  WK-CON-RSYO         PIC X(13).
 01  WK-HTAN-G.
     03  WK-HTAN-SET        OCCURS      5.
         04  WK-HTAN             PIC X(08).
 01  WK-CON-HTAN-G.
     03  WK-CON-HTAN-SET    OCCURS      5.
         04  WK-CON-HTAN         PIC X(08).
*
****  ＰＦキーガイド  ***
 01  MSG-AREA.
     02  PMSG01            PIC N(20) VALUE
                           NC"_取消　_終了　".
     02  PMSG02            PIC N(20) VALUE
                           NC"_取消　_終了　_項目戻し　".
     02  PMSG03.
         03  PMSG031       PIC N(14) VALUE
                           NC"_取消　_終了　_項目戻し　".
         03  PMSG032       PIC N(07) VALUE
                           NC"_前頁　_次頁".
     02  PMSG04.
         03  PMSG041       PIC N(14) VALUE
                           NC"_取消　_終了　_項目戻し　".
         03  PMSG042       PIC N(07) VALUE
                           NC"_次頁".
     02  PMSG05.
         03  PMSG051       PIC N(14) VALUE
                           NC"_取消　_終了　_項目戻し　".
         03  PMSG052       PIC N(07) VALUE
                           NC"_前頁".
****  メッセージ情報  ***
 01  MSG-AREA1-1.
     02  MSG-ABEND1.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-PG-ID         PIC  X(08)  VALUE  "SKY1801I".
       03  FILLER            PIC  X(10)  VALUE  " ABEND ###".
     02  MSG-ABEND2.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-FL-ID         PIC  X(08).
       03  FILLER            PIC  X(04)  VALUE  " ST-".
       03  ERR-STCD          PIC  X(02).
       03  FILLER            PIC  X(04)  VALUE  " ###".
****  エラーメッセージコード  ***
 01  CODE-AREA.
     02  ERR-MSG-CD          PIC  9(02)  VALUE  ZERO.
****  エラーメッセージ  ***
 01  ERR-TAB.
     02  MSG-ERR1            PIC  N(28)  VALUE
            NC"無効ＰＦキーです。".
     02  MSG-ERR2            PIC  N(28)  VALUE
            NC"処理区分が違います".
     02  MSG-ERR3            PIC  N(28)  VALUE
            NC"取引先コードが未入力です。".
     02  MSG-ERR4            PIC  N(28)  VALUE
            NC"取引先マスタに未登録です。".
     02  MSG-ERR5            PIC  N(28)  VALUE
            NC"商品名称Ｍ未登録です。".
     02  MSG-ERR6            PIC  N(28)  VALUE
            NC"品単コードが未入力です。".
     02  MSG-ERR7            PIC  N(28)  VALUE
            NC"ユポラベルＦ登録済です。".
     02  MSG-ERR8            PIC  N(28) VALUE
            NC"Ｙで入力して下さい".
     02  MSG-ERR9            PIC  N(28)  VALUE
            NC"９か空白以外エラーです。".
     02  MSG-ERR10           PIC  N(28)  VALUE
            NC"金額を入力して下さい。".
     02  MSG-ERR11           PIC  N(28)  VALUE
            NC"前頁はありません。".
     02  MSG-ERR12           PIC  N(28)  VALUE
            NC"次頁はありません。".
     02  MSG-ERR13           PIC  N(28)  VALUE
            NC"商品名を入力して下さい。".
     02  MSG-ERR14           PIC  N(28)  VALUE
            NC"ユポラベルＦ対象データがありません。".
     02  MSG-ERR15           PIC  N(28)  VALUE
            NC"重複する明細行は登録できません。".
     02  MSG-ERR16           PIC  N(28)  VALUE
            NC"明細を入力して下さい。".
     02  MSG-ERR17           PIC  N(28)  VALUE
            NC"入力された商品コードは量販店です。".
     02  MSG-ERR18           PIC  N(28)  VALUE
            NC"セリＮＯ．は必須入力です。".
 01  ERR-MSG-ALL     REDEFINES    ERR-TAB.
     02  ERR-MSG             PIC  N(28)
                             OCCURS  18  TIMES.
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
*----------------------------------------------------------*
*             ＭＡＩＮ         ＭＯＤＵＬＥ                *
*----------------------------------------------------------*
 PROCEDURE              DIVISION.
**
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   DSPF.
     MOVE     "DSPF    "       TO   ERR-FL-ID.
     MOVE      DSP-STATUS      TO   ERR-STCD.
     DISPLAY   MSG-ABEND1    UPON   CONS.
     DISPLAY   MSG-ABEND2    UPON   CONS.
     MOVE      4000            TO   PROGRAM-STATUS.
     STOP      RUN.
**
  FILEERR-SEC2           SECTION.
      USE       AFTER    EXCEPTION
                         PROCEDURE   ZYUPODT1.
      MOVE     "ZYUPODT1"       TO   ERR-FL-ID.
      MOVE      YUP-STATUS      TO   ERR-STCD.
      DISPLAY   MSG-ABEND1    UPON   CONS.
      DISPLAY   MSG-ABEND2    UPON   CONS.
      MOVE      4000            TO   PROGRAM-STATUS.
      STOP      RUN.
**
 FILEERR-SEC3           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   SHOTBL1.
     MOVE     "SHOTBL1 "       TO   ERR-FL-ID.
     MOVE      SHO-STATUS      TO   ERR-STCD.
     DISPLAY   MSG-ABEND1    UPON   CONS.
     DISPLAY   MSG-ABEND2    UPON   CONS.
     MOVE      4000            TO   PROGRAM-STATUS.
     STOP      RUN.
**
 FILEERR-SEC4           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   MEIMS1.
     MOVE     "MEIMS1 "        TO   ERR-FL-ID.
     MOVE      MEI-STATUS      TO   ERR-STCD.
     DISPLAY   MSG-ABEND1    UPON   CONS.
     DISPLAY   MSG-ABEND2    UPON   CONS.
     MOVE      4000            TO   PROGRAM-STATUS.
     STOP      RUN.
**
 FILEERR-SEC5           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   TOKMS2.
     MOVE     "TOKMS2 "       TO   ERR-FL-ID.
     MOVE      TOK-STATUS      TO   ERR-STCD.
     DISPLAY   MSG-ABEND1    UPON   CONS.
     DISPLAY   MSG-ABEND2    UPON   CONS.
     MOVE      4000            TO   PROGRAM-STATUS.
     STOP      RUN.
 END     DECLARATIVES.
************************************************************
*      0.０     プログラムＳＴＡＲＴ                       *
************************************************************
 SKY1801I-START         SECTION.
     PERFORM      INIT-SEC.
     PERFORM      MAIN-SEC
                  UNTIL     END-FLG  =    "END".
     PERFORM      END-SEC.
     STOP      RUN.
 SKY1801I-END.
     EXIT.
************************************************************
*      _０     初期処理                                   *
************************************************************
 INIT-SEC               SECTION.
*
     OPEN     I-O       DSPF      ZYUPODT1
              INPUT     SHOTBL1  MEIMS1  TOKMS2.
*
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
*
     MOVE     ZERO           TO   ERR-MSG-CD.
     MOVE    "1"             TO   MAIN-FLG.
 INIT-END.
     EXIT.
************************************************************
*      _０      メイン処理                                *
************************************************************
 MAIN-SEC          SECTION.
     EVALUATE      MAIN-FLG
         WHEN      "1"  PERFORM   DSP-INIT-SUB
         WHEN      "2"  PERFORM   SYORI-SUB
         WHEN      "3"  PERFORM   TRCD-SUB
         WHEN      "4"  PERFORM   BODY-SYUSEI-SUB
         WHEN      "5"  PERFORM   BODY-IN-SUB
         WHEN      "6"  PERFORM   KAKU-SUB
     END-EVALUATE.
 MAIN-END.
     EXIT.
************************************************************
*      3.0        終了処理                                 *
************************************************************
 END-SEC                SECTION.
     CLOSE    DSPF      ZYUPODT1
              SHOTBL1  MEIMS1  TOKMS2.
 END-END.
     EXIT.
*----------------------------------------------------------*
*      2.1       初期画面表示     ( MAIN-FLG = 1 )         *
*----------------------------------------------------------*
 DSP-INIT-SUB         SECTION.
     MOVE     SPACE          TO   FKY18011.
     MOVE    "FKY18011"      TO   DSP-FORMAT.
     MOVE    "CL"            TO   DSP-PROC.
     MOVE     HEN-DATE       TO   SDATE.
     MOVE     HEN-TIME       TO   STIME.
*
     PERFORM  DSP-CLR-SUB.
     MOVE     2              TO   MAIN-FLG.
 DSP-INIT-EXIT.
     EXIT.
*----------------------------------------------------------*
*      2.1       処理区分入力     ( MAIN-FLG = 2 )         *
*----------------------------------------------------------*
 SYORI-SUB            SECTION.
*
     PERFORM  MSG-SEC.
     MOVE     PMSG01               TO   W00004.
     PERFORM  DSP-WRITE-SUB.
*
     MOVE    "SYORI" TO   DSP-GROUP.
     PERFORM         DSP-READ-SUB.
* アテンション判定
     EVALUATE    DSP-FUNC
         WHEN   "F004"
                 MOVE     1         TO   MAIN-FLG
                 GO                 TO   SYORI-EXIT
         WHEN   "F005"
                 MOVE    "9"        TO   MAIN-FLG
                 MOVE    "END"      TO   END-FLG
                 GO                 TO   SYORI-EXIT
         WHEN   "E000"
                 CONTINUE
         WHEN    OTHER
                 MOVE     01        TO   ERR-MSG-CD
                 GO                 TO   SYORI-EXIT
     END-EVALUATE.
*
*処理区分 CHK
     IF    R00001  IS NOT  NUMERIC
           MOVE   02      TO   ERR-MSG-CD
           MOVE   "R"     TO   EDIT-OPTION  OF  R00001
           MOVE   "C"     TO   EDIT-CURSOR  OF  R00001
           MOVE    SPACE  TO   W00001
           GO             TO   SYORI-EXIT
     END-IF.
     IF (  R00001    =   1    )  OR
        (  R00001    =   2    )
         MOVE   "D"     TO   EDIT-OPTION  OF  R00001
         MOVE   " "     TO   EDIT-CURSOR  OF  R00001
         IF      R00001  =  1
                 MOVE   NC"登録"      TO      W00001
                 MOVE   1             TO      SYORI-F
         ELSE
                 MOVE   NC"修正"      TO      W00001
                 MOVE   2             TO      SYORI-F
         END-IF
     ELSE
         MOVE   02      TO   ERR-MSG-CD
         MOVE   "R"     TO   EDIT-OPTION  OF  R00001
         MOVE   "C"     TO   EDIT-CURSOR  OF  R00001
         MOVE    SPACE  TO   W00001
         GO             TO   SYORI-EXIT
     END-IF.
     MOVE       "3"           TO   MAIN-FLG.
     PERFORM     CHKMEI-SUB.
 SYORI-EXIT.
     EXIT.
*----------------------------------------------------------*
*      2.2       取引先コード入力  ( MAIN-FLG = 3 )        *
*----------------------------------------------------------*
 TRCD-SUB            SECTION.
*
     PERFORM   MSG-SEC.
     MOVE      PMSG02         TO   W00004.
     PERFORM   DSP-WRITE-SUB.
*
     MOVE     "TORIHK"        TO   DSP-GROUP.
     PERFORM   DSP-READ-SUB.
* アテンション判定
     EVALUATE  DSP-FUNC
         WHEN   "F004"
                 MOVE     1         TO   MAIN-FLG
                 GO                 TO   TRCD-EXIT
         WHEN   "F005"
                 MOVE    "9"        TO   MAIN-FLG
                 MOVE    "END"      TO   END-FLG
                 GO                 TO   TRCD-EXIT
         WHEN   "F006"
                 MOVE    "2"        TO   MAIN-FLG
                 MOVE     ZERO      TO   ERR-MSG-CD
                 GO                 TO   TRCD-EXIT
         WHEN   "E000"
                 CONTINUE
         WHEN    OTHER
                 MOVE     01        TO   ERR-MSG-CD
                 GO                 TO   TRCD-EXIT
     END-EVALUATE.
*
*コード未入力チェック
     IF   ( R00002 IS NOT NUMERIC )  OR
          ( R00002     =     ZERO )
            MOVE   SPACE       TO   W00002
            MOVE   04          TO   ERR-MSG-CD
            MOVE  "R"          TO   EDIT-OPTION OF R00002
            MOVE  "C"          TO   EDIT-CURSOR OF R00002
            GO                 TO   TRCD-EXIT
     END-IF.
*
*取引先Ｍ索引
     MOVE   R00002             TO   TOK-F01.
     PERFORM  TOR-READ-SUB.
     IF     TOR-INV-FLG   =   SPACE
            MOVE   TOK-F02     TO   W00002
            MOVE  "D"          TO   EDIT-OPTION OF R00002
            MOVE   SPACE       TO   EDIT-CURSOR OF R00002
     ELSE
            MOVE   SPACE       TO   W00002
            MOVE   04          TO   ERR-MSG-CD
            MOVE  "R"          TO   EDIT-OPTION OF R00002
            MOVE  "C"          TO   EDIT-CURSOR OF R00002
            GO                 TO   TRCD-EXIT
     END-IF.
*
     IF    ERR-MSG-CD   =  ZERO
           MOVE    R00002      TO   WK-TORICD
           IF      R00001  =  1
                   MOVE    5   TO   MAIN-FLG
           ELSE
                   MOVE    4   TO   MAIN-FLG
           END-IF
     END-IF.
 TRCD-EXIT.
     EXIT.
*----------------------------------------------------------*
*      2.3   修正時ＢＯＤＹ部入力   ( MAIN-FLG = 4 )
*----------------------------------------------------------*
 BODY-SYUSEI-SUB       SECTION.
*
     PERFORM  MSG-SEC.
     MOVE     PMSG02         TO   W00004.
     PERFORM  DSP-WRITE-SUB.
*
     MOVE     1              TO   PAGE-CNT.
     MOVE    "SYOCD"         TO   DSP-GROUP.
     PERFORM       DSP-READ-SUB.
* アテンション判定
     EVALUATE    DSP-FUNC
         WHEN   "F004"
                 MOVE     "1"      TO   MAIN-FLG
                 MOVE      ZERO    TO   ERR-MSG-CD
                 GO                TO   BODY-SYUSEI-EXIT
         WHEN   "F005"
                 MOVE     "9"      TO   MAIN-FLG
                 MOVE     "END"    TO   END-FLG
                 GO                TO   BODY-SYUSEI-EXIT
         WHEN   "F006"
                 MOVE     "3"      TO   MAIN-FLG
                 MOVE      ZERO    TO   ERR-MSG-CD
                 GO                TO   BODY-SYUSEI-EXIT
         WHEN   "E000"
                 CONTINUE
         WHEN    OTHER
                 MOVE      01      TO   ERR-MSG-CD
                 GO                TO   BODY-SYUSEI-EXIT
     END-EVALUATE.
*
     MOVE   R00002          TO    YUP-F02.
     MOVE   SPACE           TO    YUP-F03.
     MOVE   R10101(1)       TO    YUP-F03.
     MOVE   SERI1(1)        TO    YUP-F011.
     MOVE   SERI2(1)        TO    YUP-F012.
     START  ZYUPODT1 KEY IS >= YUP-F02 YUP-F03 YUP-F011 YUP-F012
           INVALID
            MOVE     "R"    TO    EDIT-OPTION OF R10101(1)
            MOVE     "C"    TO    EDIT-CURSOR OF R10101(1)
            MOVE      14    TO    ERR-MSG-CD
            GO              TO    BODY-SYUSEI-EXIT
     END-START.
*ユポラベルＦ　ＲＥＡＤ
     MOVE      SPACE        TO    YUP-END-FLG.
     PERFORM   YUP-READ-SUB.
     IF      YUP-END-FLG   =  "END"
             MOVE      14   TO    ERR-MSG-CD
             GO             TO    BODY-SYUSEI-EXIT
     ELSE
            MOVE     "D"    TO    EDIT-OPTION OF R10101(1)
            MOVE      SPACE TO    EDIT-CURSOR OF R10101(1)
     END-IF.
*
*明細画面テーブルセット
     PERFORM  MEISAI-DSP-SET.
*
     MOVE       5          TO            MAIN-FLG.
 BODY-SYUSEI-EXIT.
     EXIT.
*----------------------------------------------------------*
*      2.3   ＢＯＤＹ部入力   ( MAIN-FLG = 5 )
*----------------------------------------------------------*
 BODY-IN-SUB       SECTION.
*
     PERFORM  MSG-SEC.
     IF       SYORI-F     =  1
              MOVE   PMSG02      TO   W00004
     ELSE
              EVALUATE  PAGE-CNT
                  WHEN   1
                         IF    YUP-END-FLG   =  "END"
                               MOVE  PMSG02  TO  W00004
                         ELSE  MOVE  PMSG04  TO  W00004
                         END-IF
                  WHEN   99    MOVE  PMSG05  TO  W00004
                  WHEN   OTHER
                         IF    YUP-END-FLG   =  "END"
                               MOVE  PMSG05  TO  W00004
                         ELSE  MOVE  PMSG03  TO  W00004
                         END-IF
              END-EVALUATE
     END-IF.
     PERFORM  DSP-WRITE-SUB.
     MOVE     ZERO               TO   ERR-MSG-CD.
*
     MOVE    "BODY  "            TO   DSP-GROUP.
     PERFORM  DSP-READ-SUB.
* アテンション判定
     EVALUATE    DSP-FUNC
         WHEN   "F004"
                 MOVE     "1"      TO   MAIN-FLG
                 MOVE      ZERO    TO   ERR-MSG-CD
                 GO                TO   BODY-IN-EXIT
         WHEN   "F005"
                 MOVE     "END"    TO   END-FLG
                 GO                TO   BODY-IN-EXIT
         WHEN   "F006"
                 IF      SYORI-F   =    1
                         MOVE     "3"      TO   MAIN-FLG
                 ELSE    MOVE     "4"      TO   MAIN-FLG
                 END-IF
                 PERFORM   DSP-CLR-SUB
                 MOVE      ZERO    TO   ERR-MSG-CD
                 GO                TO   BODY-IN-EXIT
*前頁
         WHEN   "F011"
                 IF    SYORI-F   =   2
                    IF    PAGE-CNT  =  1
                          MOVE      11           TO   ERR-MSG-CD
                    ELSE
                          COMPUTE   PAGE-CNT  =  PAGE-CNT  -  1
                          MOVE      R00002       TO   YUP-F02
                          MOVE      SPACE        TO   YUP-F03
                          MOVE      WK-SYOCD-MAE(PAGE-CNT)
                                                 TO   YUP-F03
                          MOVE      WK-SERI1-MAE(PAGE-CNT)
                                                 TO   YUP-F011
                          MOVE      WK-SERI2-MAE(PAGE-CNT)
                                                 TO   YUP-F012
                          MOVE      SPACE        TO   YUP-END-FLG
                          START   ZYUPODT1
                              INVALID
                                  MOVE     11  TO   ERR-MSG-CD
                              NOT  INVALID
                                  PERFORM  YUP-READ-SUB
                                  PERFORM  MEISAI-DSP-SET
                          END-START
                   END-IF
                 ELSE
                          MOVE     01          TO   ERR-MSG-CD
                 END-IF
                 GO                   TO   BODY-IN-EXIT
*次頁
         WHEN   "F012"
                 IF    SYORI-F   =   2
                    IF  ( PAGE-CNT   =  99    )   OR
                        ( S00001(1)  =  SPACE )
                          MOVE     12     TO   ERR-MSG-CD
                    ELSE
                        IF   YUP-END-FLG = "END"
                             IF  S00001(5) IS NOT NUMERIC
                                 MOVE   12  TO ERR-MSG-CD
                             ELSE
                                 PERFORM    BODY-CLR-SUB
                             END-IF
                        ELSE
                             MOVE  R00002       TO  YUP-F02
                             MOVE  SPACE        TO  YUP-F03
                             MOVE  WK-SYOCD-ATO TO  YUP-F03
                             MOVE  WK-SERI1-ATO TO  YUP-F011
                             MOVE  WK-SERI2-ATO TO  YUP-F012
                             MOVE  SPACE        TO  YUP-END-FLG
                             START  ZYUPODT1
                               INVALID
                                 MOVE  12  TO   ERR-MSG-CD
                               NOT  INVALID
*--- 前頁ポインタの格納 ---*
                                 MOVE     R10101(1) TO
                                          WK-SYOCD-MAE(PAGE-CNT)
                                 MOVE     SERI1(1)  TO
                                          WK-SERI1-MAE(PAGE-CNT)
                                 MOVE     SERI2(1)  TO
                                          WK-SERI2-MAE(PAGE-CNT)
                                 PERFORM  YUP-READ-SUB
                                 PERFORM  MEISAI-DSP-SET
                                 ADD   1   TO   PAGE-CNT
                             END-START
                        END-IF
                    END-IF
                 ELSE
                          MOVE     01          TO   ERR-MSG-CD
                 END-IF
                 GO                     TO   BODY-IN-EXIT
         WHEN   "E000"
                 CONTINUE
         WHEN    OTHER
                 MOVE      01      TO   ERR-MSG-CD
                 GO                TO   BODY-IN-EXIT
     END-EVALUATE.
*
*ボディー部チェック
     MOVE       ZERO               TO   ERR-MSG-CD
                                        MEISAI-CNT.
     INITIALIZE            WK-SYOCD-G   WK-HTAN-G.
     MOVE       1                  TO   CHK-MAE-FLG.
     PERFORM    DSP-CLR-SUB.
*
     PERFORM    VARYING  I FROM   1 BY  1
                           UNTIL         I      >     5
                PERFORM    BODY-CHK-SUB
     END-PERFORM.
*未入力チェック
     IF     MEISAI-CNT  =  ZERO
            PERFORM    BODY-CLR-SUB
            PERFORM    DSP-CLR-SUB
            MOVE       16          TO   ERR-MSG-CD
     END-IF.
*登録データダブリチェック
     IF     ERR-MSG-CD  =  ZERO
            PERFORM    DABURI-CHK-SUB
     END-IF.
*コンデンス処理
     IF     ERR-MSG-CD  =  ZERO
            PERFORM    DSP-WRITE-SUB
            PERFORM    CONDEN-CHK-SUB
            MOVE       6      TO       MAIN-FLG
      END-IF.
 BODY-IN-EXIT.
     EXIT.
*----------------------------------------------------------*
*      2.2.1     ボディー部チェック
*----------------------------------------------------------*
 BODY-CHK-SUB             SECTION.
*
     IF (R10001(I) = SPACE)  AND  (R10101(I) = SPACE)  AND
        (R10201(I) = SPACE)  AND  (R10301(I) = SPACE)  AND
        (R10401(I) = SPACE)  AND  (R10501(I) = SPACE)  AND
        (R10601(I) = SPACE)  AND  (R10701(I) = SPACE)  AND
        (R11101(I) = SPACE)  AND  (R10801(I) = SPACE)  AND
*--- 2000/02/28  NAV  START ---*
********((R10901(I) IS NOT NUMERIC) OR (R10901(I) = ZERO )) AND
*--- 2000/02/28  NAV  END   ---*
        ((R11001(I) IS NOT NUMERIC) OR (R11001(I) = ZERO ))
           GO                      TO   BODY-CHK-EXIT
     ELSE
           ADD       1             TO   MEISAI-CNT
     END-IF.
*削除フラグ
     IF  R10001(I)    =  "9"   OR   " "
         CONTINUE
     ELSE
         IF    ERR-MSG-CD  =  ZERO
               MOVE    9      TO   ERR-MSG-CD
         END-IF
         MOVE   "R"     TO   EDIT-OPTION  OF  R10001(I)
         MOVE   "C"     TO   EDIT-CURSOR  OF  R10001(I)
     END-IF.
*---    品単・部門右詰め処理
     PERFORM      HINTAN-SUB.
     PERFORM      BUMON-SUB.
*
     IF  (R10101(I) = SPACE) AND  (R10201(I) = SPACE) AND
         (R10301(I) = SPACE) AND  (R10401(I) = SPACE)
          GO                  TO  BODY-CHK-006
     END-IF.
*
*商品名称Ｍ　索引
 BODY-CHK-000.
     MOVE   R00002            TO  SHO-F01.
     MOVE   SPACE             TO  SHO-F02.
     MOVE   R10101(I)         TO  SHO-F02.
*---    商品変換ＴＢＬ　読込み
     PERFORM     SHOT-READ-SUB.
*
     IF     SHO-INV-FLG   =   SPACE
            MOVE   SHO-F02    TO     WK-RSYO(I)
            MOVE   SHO-F031   TO     MEI-F011   WK-SYO(I)
            MOVE   SHO-F032   TO     MEI-F012   WK-HTAN(I)
*画面入力が量販店の時品単が入力されていたらエラー
            IF   ( R10201(I)  NOT =  SPACE  ) OR
                 ( R10301(I)  NOT =  SPACE  ) OR
                 ( R10401(I)  NOT =  SPACE  )
                   IF    ERR-MSG-CD  =  ZERO
                         MOVE   17  TO     ERR-MSG-CD
                   END-IF
                   MOVE  "R"  TO     EDIT-OPTION OF R10201(I)
                                     EDIT-OPTION OF R10301(I)
                                     EDIT-OPTION OF R10401(I)
                   MOVE  "C"  TO     EDIT-CURSOR OF R10201(I)
                                     EDIT-CURSOR OF R10301(I)
                                     EDIT-CURSOR OF R10401(I)
            END-IF
     ELSE
            MOVE   R10101(I)  TO  MEI-F011
            MOVE   R10201(I)  TO  WK-HINTAN-1
            MOVE   R10301(I)  TO  WK-HINTAN-2
            MOVE   R10401(I)  TO  WK-HINTAN-3
            MOVE   WK-HINTAN  TO  MEI-F012
     END-IF.
 BODY-CHK-005.
*---    商品名称Ｍ　読込み
     PERFORM     SMEI-READ-SUB.
     IF     MEI-INV-FLG   =  SPACE
            IF     SHO-INV-FLG   =  "INV"
                   MOVE   MEI-F011       TO    WK-RSYO(I)
                                               WK-SYO(I)
                   MOVE   MEI-F012       TO    WK-HTAN(I)
            END-IF
            IF     R10501(I) =  SPACE
                   MOVE      MEI-F021    TO    R10501(I)
            END-IF
            IF     R10701(I) =  SPACE
                   MOVE      MEI-F022(1:7)    TO    R10701(I)
                   MOVE      MEI-F022(8:7)    TO    R11101(I)
            END-IF
            IF     R10601(I) =  SPACE
                   MOVE      MEI-F06     TO    R10601(I)
            END-IF
     ELSE
            IF     SHO-INV-FLG   =  "INV"
                   IF   ERR-MSG-CD  =  ZERO
                        MOVE   5         TO    ERR-MSG-CD
                   END-IF
                   MOVE  "R"    TO   EDIT-OPTION OF R10101(I)
                   MOVE  "C"    TO   EDIT-CURSOR OF R10101(I)
            END-IF
     END-IF.
 BODY-CHK-006.
*セリ番号チェック
     IF      SERI1(I)  NOT  NUMERIC
             MOVE   ZERO     TO    SERI1(I)
     END-IF.
     IF      SERI2(I)  NOT  NUMERIC
             MOVE   ZERO     TO    SERI2(I)
     END-IF.
     IF   (  SERI1(I)  =  ZERO )  AND  (  SERI2(I)  =  ZERO  )
             IF    ERR-MSG-CD  =  ZERO
                   MOVE   18    TO   ERR-MSG-CD
             END-IF
             MOVE  "R"    TO   EDIT-OPTION OF SERI1(I)
             MOVE  "R"    TO   EDIT-OPTION OF SERI2(I)
             MOVE  "C"    TO   EDIT-CURSOR OF SERI1(I)
     END-IF.
*
 BODY-CHK-010.
*---    登録時　ユポＦ存在チェック
*    IF     EDIT-STATUS OF R10101(I) =  SPACE
*    AND    EDIT-STATUS OF SERI1(I)  =  SPACE
*    AND    EDIT-STATUS OF SERI2(I)  =  SPACE
*    IF  (  R10101(I)  =  SPACE  )
*    AND (( SERI1(I) NOT NUMERIC ) OR ( SERI1(I) = ZERO ))
*    AND (( SERI2(I) NOT NUMERIC ) OR ( SERI2(I) = ZERO ))
*           CONTINUE
*    ELSE
*    DISPLAY "I           = " I              UPON CONS.
*    DISPLAY "WK-CHKMEI-G = " WK-CHKMEI-G(I) UPON CONS.
     IF     WK-CHKMEI-G(I)  =  1
            MOVE   R00002    TO    YUP-F02
            MOVE   R10101(I) TO    YUP-F03
            MOVE   SERI1(I)  TO    YUP-F011
            MOVE   SERI2(I)  TO    YUP-F012
            READ   ZYUPODT1
               INVALID
                 CONTINUE
               NOT  INVALID
                 IF    ERR-MSG-CD  =  ZERO
                       MOVE   7     TO   ERR-MSG-CD
                 END-IF
                 MOVE  "R"    TO   EDIT-OPTION OF R10101(I)
                 MOVE  "R"    TO   EDIT-OPTION OF R10201(I)
                 MOVE  "R"    TO   EDIT-OPTION OF R10301(I)
                 MOVE  "R"    TO   EDIT-OPTION OF R10401(I)
                 MOVE  "R"    TO   EDIT-OPTION OF SERI1(I)
                 MOVE  "R"    TO   EDIT-OPTION OF SERI2(I)
                 MOVE  "C"    TO   EDIT-CURSOR OF R10101(I)
            END-READ
     END-IF.
*<<
*商品名未入力チェック
*    IF   (( R10501(I) IS NUMERIC ) OR ( R10501(I) = SPACE  ))
*     AND (( R10701(I) IS NUMERIC ) OR ( R10701(I) = SPACE  ))
*          IF    ERR-MSG-CD  =  ZERO
*                MOVE    13     TO   ERR-MSG-CD
*          END-IF
*          MOVE   "R"     TO   EDIT-OPTION  OF  R10501(I)
*                              EDIT-OPTION  OF  R10701(I)
*          MOVE   "C"     TO   EDIT-CURSOR  OF  R10501(I)
*                              EDIT-CURSOR  OF  R10701(I)
*<<  END-IF.
*
*---    枚数
     IF     R11001(I) IS NOT NUMERIC
            MOVE   ZERO      TO    R11001(I)
     END-IF.
*---    金額   2000/02/28  チェック無し
*--  IF   ( R10901(I) IS NOT NUMERIC )  OR
*--       ( R10901(I)    =   ZERO    )
*--         IF    ERR-MSG-CD  =  ZERO
*--               MOVE   10        TO    ERR-MSG-CD
*--         END-IF
*--         MOVE  "R"        TO    EDIT-OPTION OF R10901(I)
*--         MOVE  "C"        TO    EDIT-CURSOR OF R10901(I)
*--  END-IF.
*---    チェックデジット　チェック
     PERFORM      CHK-DEJIT-SUB.
 BODY-CHK-EXIT.
     EXIT.
*----------------------------------------------------------*
*      2.2.1     画面ダブリ入力チェック
*----------------------------------------------------------*
 DABURI-CHK-SUB            SECTION.

     PERFORM  DSP-CLR-SUB.
     PERFORM  VARYING  A  FROM  1  BY  1
                          UNTIL    A      >     5
          PERFORM   VARYING  I  FROM  1  BY  1
                          UNTIL       I      >     5
              IF   (      A    =   I   )      OR
                  (( R10101(A) = SPACE )      AND
                   ( R10201(A) = SPACE )      AND
                   ( R10301(A) = SPACE )      AND
                   ( R10401(A) = SPACE )      AND
                   ( R10601(A) = SPACE )      AND
                   ( SERI1(A)  NOT NUMERIC
                 OR  SERI1(A)  =  ZERO )      AND
                   ( SERI2(A)  NOT NUMERIC
                 OR  SERI2(A)  =  ZERO ))     OR
                  (( R10101(I) = SPACE )      AND
                   ( R10201(I) = SPACE )      AND
                   ( R10301(I) = SPACE )      AND
                   ( R10401(I) = SPACE )      AND
                   ( R10601(I) = SPACE )      AND
                   ( SERI1(I)  NOT NUMERIC
                 OR  SERI1(I)  =  ZERO )      AND
                   ( SERI2(I)  NOT NUMERIC
                 OR  SERI2(I)  =  ZERO ))
                    CONTINUE
              ELSE
                    IF  ( R10101(A)  =  R10101(I)) AND
                        ( R10201(A)  =  R10201(I)) AND
                        ( R10301(A)  =  R10301(I)) AND
                        ( R10401(A)  =  R10401(I)) AND
                        ( R10601(A)  =  R10601(I)) AND
                        ( SERI1(A)   =  SERI1(I))  AND
                        ( SERI2(A)   =  SERI2(I))
                        MOVE   15  TO   ERR-MSG-CD
                        MOVE  "R"  TO   EDIT-OPTION OF R10101(A)
                                        EDIT-OPTION OF R10201(A)
                                        EDIT-OPTION OF R10301(A)
                                        EDIT-OPTION OF R10401(A)
                                        EDIT-OPTION OF R10601(A)
                                        EDIT-OPTION OF SERI1(A)
                                        EDIT-OPTION OF SERI2(A)
                        MOVE  "R"  TO   EDIT-OPTION OF R10101(I)
                                        EDIT-OPTION OF R10201(I)
                                        EDIT-OPTION OF R10301(I)
                                        EDIT-OPTION OF R10401(I)
                                        EDIT-OPTION OF R10601(I)
                                        EDIT-OPTION OF SERI1(I)
                                        EDIT-OPTION OF SERI2(I)
                        MOVE  "C"  TO   EDIT-CURSOR OF R10101(A)
                                        EDIT-CURSOR OF R10201(A)
                                        EDIT-CURSOR OF R10301(A)
                                        EDIT-CURSOR OF R10401(A)
                                        EDIT-CURSOR OF R10601(A)
                                        EDIT-CURSOR OF SERI1(A)
                                        EDIT-CURSOR OF SERI2(A)
                        MOVE  "C"  TO   EDIT-CURSOR OF R10101(I)
                                        EDIT-CURSOR OF R10201(I)
                                        EDIT-CURSOR OF R10301(I)
                                        EDIT-CURSOR OF R10401(I)
                                        EDIT-CURSOR OF R10601(I)
                                        EDIT-CURSOR OF SERI1(I)
                                        EDIT-CURSOR OF SERI2(I)
                    END-IF
              END-IF
          END-PERFORM
     END-PERFORM.
 DABURI-CHK-EXIT.
     EXIT.
*----------------------------------------------------------*
*      2.2.1     画面圧縮処理
*----------------------------------------------------------*
 CONDEN-CHK-SUB         SECTION.
*
     INITIALIZE   WK-CONDEN-AREA.
     INITIALIZE   WK-CON-SYOCD-G.
     MOVE     1                       TO       I.
     PERFORM  VARYING  A  FROM  1  BY  1
                                UNTIL  A   >   5
       IF   ((R10101(A) IS NOT NUMERIC ) OR ( R10101(A) = SPACE))
        AND ((R10201(A) IS NOT NUMERIC ) OR ( R10201(A) = SPACE))
        AND ((R10301(A) IS NOT NUMERIC ) OR ( R10301(A) = SPACE))
        AND ((R10401(A) IS NOT NUMERIC ) OR ( R10401(A) = SPACE))
        AND ((R10501(A) IS NOT NUMERIC ) OR ( R10501(A) = SPACE))
        AND ((R10601(A) IS NOT NUMERIC ) OR ( R10601(A) = SPACE))
        AND ((SERI1(A)  IS NOT NUMERIC ) OR ( SERI1(A)  = ZERO ))
        AND ((SERI2(A)  IS NOT NUMERIC ) OR ( SERI2(A)  = ZERO ))
              CONTINUE
       ELSE
            MOVE     WK-RSYO(A)       TO   WK-CON-RSYO(I)
            MOVE     WK-SYO(A)        TO   WK-CON-SYO(I)
            MOVE     WK-HTAN(A)       TO   WK-CON-HTAN(I)
            MOVE     R10001(A)        TO   WK-R10001(I)
            MOVE     R10101(A)        TO   WK-R10101(I)
            MOVE     R10201(A)        TO   WK-R10201(I)
            MOVE     R10301(A)        TO   WK-R10301(I)
            MOVE     R10401(A)        TO   WK-R10401(I)
            MOVE     R10501(A)        TO   WK-R10501(I)
            MOVE     R10601(A)        TO   WK-R10601(I)
            MOVE     R10701(A)        TO   WK-R10701(I)
            MOVE     R11101(A)        TO   WK-R11101(I)
            MOVE     R10801(A)        TO   WK-R10801(I)
            MOVE     R10901(A)        TO   WK-R10901(I)
            MOVE     R11001(A)        TO   WK-R11001(I)
            MOVE     SERI1(A)         TO   WK-SERI1(I)
            MOVE     SERI2(A)         TO   WK-SERI2(I)
            ADD      1                TO   I
       END-IF
     END-PERFORM.
     PERFORM      BODY-CLR-SUB.
     INITIALIZE   WK-SYOCD-G   WK-HTAN-G.
*
     MOVE       1                TO       A.
     PERFORM  VARYING  I  FROM  1  BY  1
                                UNTIL  I   >   5
*--- 2000/02/28 NAV START ---*
*********IF     WK-R10901(I)  =  ZERO
*********       CONTINUE
*********ELSE
*--- 2000/02/28 NAV END   ---*
         IF     WK-R10101(I)  =  SPACE
         AND    WK-R10501(I)  =  SPACE
         AND    WK-R10601(I)  =  SPACE
                CONTINUE
         ELSE
                MOVE   WK-CON-RSYO(I)     TO     WK-RSYO(A)
                MOVE   WK-CON-SYO(I)      TO     WK-SYO(A)
                MOVE   WK-CON-HTAN(I)     TO     WK-HTAN(A)
                MOVE   WK-R10001(I)       TO     R10001(A)
                MOVE   WK-R10101(I)       TO     R10101(A)
                MOVE   WK-R10201(I)       TO     R10201(A)
                MOVE   WK-R10301(I)       TO     R10301(A)
                MOVE   WK-R10401(I)       TO     R10401(A)
                MOVE   WK-R10501(I)       TO     R10501(A)
                MOVE   WK-R10601(I)       TO     R10601(A)
                MOVE   WK-R10701(I)       TO     R10701(A)
                MOVE   WK-R11101(I)       TO     R11101(A)
                MOVE   WK-R10801(I)       TO     R10801(A)
                MOVE   WK-R10901(I)       TO     R10901(A)
                MOVE   WK-R11001(I)       TO     R11001(A)
                MOVE   WK-SERI1(I)        TO     SERI1(A)
                MOVE   WK-SERI2(I)        TO     SERI2(A)
*プロテクトセット
                IF   (  SYORI-F   =   2  )  AND
                     (  A  <= PRTECT-CNT )
                       MOVE  "X" TO  EDIT-STATUS OF R10101(A)
                                     EDIT-STATUS OF R10201(A)
                                     EDIT-STATUS OF R10301(A)
                                     EDIT-STATUS OF R10401(A)
                                     EDIT-STATUS OF SERI1(A)
                                     EDIT-STATUS OF SERI2(A)
                END-IF
                ADD    1                  TO     A
         END-IF
     END-PERFORM.
 CONDEN-EXIT.
     EXIT.
*----------------------------------------------------------*
*      2.2.1     チェックデジット計算
*----------------------------------------------------------*
 CHK-DEJIT-SUB            SECTION.
*
     IF       R10601(I)   =  SPACE
              GO             TO      CHK-DEJIT-EXIT
     END-IF.
     INITIALIZE   WK-CHKDG-AREA.
     MOVE      R10601(I)     TO    WK-CHKDG-R.
     IF       WK-CHKDG-1R =  SPACE   MOVE  ZERO  TO  WK-CHKDG-1.
     IF       WK-CHKDG-2R =  SPACE   MOVE  ZERO  TO  WK-CHKDG-2.
     IF       WK-CHKDG-3R =  SPACE   MOVE  ZERO  TO  WK-CHKDG-3.
     IF       WK-CHKDG-4R =  SPACE   MOVE  ZERO  TO  WK-CHKDG-4.
     IF       WK-CHKDG-5R =  SPACE   MOVE  ZERO  TO  WK-CHKDG-5.
     IF       WK-CHKDG-6R =  SPACE   MOVE  ZERO  TO  WK-CHKDG-6.
     IF       WK-CHKDG-7R =  SPACE   MOVE  ZERO  TO  WK-CHKDG-7.
     IF       WK-CHKDG-8R =  SPACE   MOVE  ZERO  TO  WK-CHKDG-8.
     IF       WK-CHKDG-9R =  SPACE   MOVE  ZERO  TO  WK-CHKDG-9.
     IF       WK-CHKDG-10R = SPACE   MOVE  ZERO  TO  WK-CHKDG-10.
     IF       WK-CHKDG-11R = SPACE   MOVE  ZERO  TO  WK-CHKDG-11.
     IF       WK-CHKDG-12R = SPACE   MOVE  ZERO  TO  WK-CHKDG-12.
     IF       WK-CHKDG-13R = SPACE   MOVE  ZERO  TO  WK-CHKDG-13.
*
     COMPUTE  WK-STEP1  = WK-CHKDG-2 + WK-CHKDG-4  + WK-CHKDG-6
                        + WK-CHKDG-8 + WK-CHKDG-10 + WK-CHKDG-12.
     COMPUTE  WK-STEP2  = WK-STEP1   *   3.
     COMPUTE  WK-STEP3  = WK-CHKDG-3 + WK-CHKDG-5  + WK-CHKDG-7
                        + WK-CHKDG-9 + WK-CHKDG-11 + WK-CHKDG-13.
     COMPUTE  WK-STEP4  = WK-STEP2   + WK-STEP3.
     MOVE     WK-STEP4    TO           WK-BAISU-RR.
     ADD      1           TO           WK-BAISU-1.
     MOVE     ZERO        TO           WK-BAISU-2.
     COMPUTE  WK-STEP51 = WK-BAISU-RR  -  WK-STEP4.
     MOVE     WK-STEP5    TO           WK-CHKDG-1.
*
     MOVE     WK-CHKDG-R     TO        R10601(I).
 CHK-DEJIT-EXIT.
     EXIT.
*----------------------------------------------------------*
*      2.2.1     画面テーブルセット
*----------------------------------------------------------*
 MEISAI-DSP-SET           SECTION.
*
     PERFORM   BODY-CLR-SUB.
     PERFORM   DSP-CLR-SUB.
*
     PERFORM   CHKMEI-SUB.
*
     PERFORM   VARYING  I  FROM  1 BY  1
                           UNTIL   (     I       >   5   ) OR
                                   ( YUP-END-FLG = "END" )
               MOVE    YUP-F11          TO       R10001(I)
               IF      YUP-F03  =  YUP-F04
                       MOVE  YUP-F03      TO    R10101(I)
                       MOVE  YUP-F05      TO    WK-HINTAN
                       MOVE  WK-HINTAN-1  TO    R10201(I)
                       MOVE  WK-HINTAN-2  TO    R10301(I)
                       MOVE  WK-HINTAN-3  TO    R10401(I)
               ELSE
                       MOVE  YUP-F03      TO    R10101(I)
                       MOVE  SPACE        TO    R10201(I)
                       MOVE  SPACE        TO    R10301(I)
                       MOVE  SPACE        TO    R10401(I)
               END-IF
               MOVE    YUP-F05          TO       WK-HINTAN
               MOVE    YUP-F06          TO       R10601(I)
               MOVE    YUP-F071         TO       R10501(I)
               MOVE    YUP-F072(1:7)    TO       R10701(I)
               MOVE    YUP-F072(8:7)    TO       R11101(I)
               MOVE    YUP-F08          TO       R10801(I)
               MOVE    YUP-F09          TO       R10901(I)
               MOVE    YUP-F10          TO       R11001(I)
               MOVE    YUP-F011         TO       SERI1(I)
               MOVE    YUP-F012         TO       SERI2(I)
               MOVE   "X"   TO    EDIT-STATUS OF R10101(I)
                                  EDIT-STATUS OF R10201(I)
                                  EDIT-STATUS OF R10301(I)
                                  EDIT-STATUS OF R10401(I)
                                  EDIT-STATUS OF SERI1(I)
                                  EDIT-STATUS OF SERI2(I)
*
               MOVE       2                   TO WK-CHKMEI-G(I)
*              ユポラベルデータ削除
***************DELETE     ZYUPODT1
*
               PERFORM    YUP-READ-SUB
     END-PERFORM.
     COMPUTE   PRTECT-CNT  =   I        -        1.
*--- 次頁ポインタの格納 ---*
     IF     YUP-END-FLG   =   SPACE
            MOVE    YUP-F03  TO    WK-SYOCD-ATO
            MOVE    YUP-F011 TO    WK-SERI1-ATO
            MOVE    YUP-F012 TO    WK-SERI2-ATO
     END-IF.
 MEISAI-DSP-EXIT.
     EXIT.
*----------------------------------------------------------*
*      2.5       確認入力          ( MAIN-FLG = 6 )
*----------------------------------------------------------*
 KAKU-SUB           SECTION.
*
     IF  ERR-MSG-CD  =  0
         MOVE     "Y"            TO   R00003
     END-IF.
     PERFORM    MSG-SEC.
*
     IF       SYORI-F     =  1
              MOVE   PMSG02      TO   W00004
     ELSE
              EVALUATE  PAGE-CNT
                  WHEN   1       MOVE  PMSG04  TO  W00004
                  WHEN   99      MOVE  PMSG05  TO  W00004
                  WHEN   OTHER   MOVE  PMSG03  TO  W00004
              END-EVALUATE
     END-IF.
     PERFORM    DSP-WRITE-SUB.
*
     MOVE      "KAKU"            TO   DSP-GROUP.
     PERFORM    DSP-READ-SUB.
* アテンション判定
     EVALUATE  DSP-FUNC
         WHEN   "F004"
                   MOVE    "1"          TO   MAIN-FLG
                   MOVE     ZERO        TO   ERR-MSG-CD
                   MOVE     SPACE       TO   R00003
         WHEN   "F005"
                   MOVE    "9"          TO   MAIN-FLG
                   MOVE    "END"        TO   END-FLG
         WHEN   "F006"
                   MOVE    "5"          TO   MAIN-FLG
*前頁
         WHEN   "F011"
                 IF    SYORI-F   =   2
                     IF    PAGE-CNT  =  1
                           MOVE    11       TO   ERR-MSG-CD
                     ELSE
*ユポラベルＦ更新処理
                           PERFORM  SYUSEI-SUB
                           COMPUTE  PAGE-CNT = PAGE-CNT - 1
                           MOVE     R00002       TO   YUP-F02
                           MOVE     SPACE        TO   YUP-F03
                           MOVE     WK-SYOCD-MAE(PAGE-CNT)
                                                 TO   YUP-F03
                           MOVE     WK-SERI1-MAE(PAGE-CNT)
                                                 TO   YUP-F011
                           MOVE     WK-SERI2-MAE(PAGE-CNT)
                                                 TO   YUP-F012
                           MOVE     SPACE        TO   YUP-END-FLG
                           PERFORM CHKMEI-SUB
                           START  ZYUPODT1
                               INVALID
                                  MOVE     11    TO   ERR-MSG-CD
                               NOT  INVALID
                                  PERFORM  YUP-READ-SUB
                                  PERFORM  MEISAI-DSP-SET
                                  MOVE     5     TO   MAIN-FLG
                           END-START
                     END-IF
                 ELSE
                     MOVE     01        TO   ERR-MSG-CD
                 END-IF
                 MOVE    SPACE  TO   R00003
*次頁
         WHEN   "F012"
                 IF    SYORI-F   =   2
                   IF   PAGE-CNT  =  99
                        MOVE     12     TO   ERR-MSG-CD
                   ELSE
                        PERFORM  SYUSEI-SUB
*--- 前頁ポインタの格納 ---*
                        MOVE     R10101(1) TO
                                        WK-SYOCD-MAE(PAGE-CNT)
                        MOVE     SERI1(1)  TO
                                        WK-SERI1-MAE(PAGE-CNT)
                        MOVE     SERI2(1)  TO
                                        WK-SERI2-MAE(PAGE-CNT)
                        ADD      1    TO   PAGE-CNT
                        PERFORM  CHKMEI-SUB
                        IF   YUP-END-FLG  =  "END"
                             PERFORM   BODY-CLR-SUB
                        ELSE
*ユポラベルＦ更新処理
                             MOVE    R00002       TO   YUP-F02
                             MOVE    SPACE        TO   YUP-F03
                             MOVE    WK-SYOCD-ATO TO   YUP-F03
                             MOVE    WK-SERI1-ATO TO   YUP-F011
                             MOVE    WK-SERI2-ATO TO   YUP-F012
                             MOVE    SPACE        TO   YUP-END-FLG
                             START  ZYUPODT1
                                 INVALID
                                   PERFORM  BODY-CLR-SUB
                                 NOT  INVALID
                                   PERFORM  YUP-READ-SUB
                                   PERFORM  MEISAI-DSP-SET
                             END-START
                        END-IF
                        MOVE    5            TO  MAIN-FLG
                   END-IF
                 ELSE
                         MOVE     01    TO   ERR-MSG-CD
                 END-IF
                 MOVE    SPACE  TO   R00003
         WHEN   "E000"
                   IF    R00003  NOT  =  "Y"
                         MOVE      8    TO   ERR-MSG-CD
                   ELSE
*ユポラベルＦ更新処理
                         IF    SYORI-F  =  1
                               PERFORM  TOUROKU-SUB
                         ELSE
                               PERFORM  SYUSEI-SUB
                         END-IF
                         MOVE       1   TO   MAIN-FLG
                   END-IF
         WHEN    OTHER
                   MOVE  01              TO   ERR-MSG-CD
     END-EVALUATE.
 KAKU-EXIT.
     EXIT.
*----------------------------------------------------------*
*      2.7        ユポラベルＦ　登録処理
*----------------------------------------------------------*
 TOUROKU-SUB              SECTION.
*
     PERFORM  VARYING  I  FROM  1  BY  1
                                   UNTIL  I  >  5
*--- 2000/02/28  NAV  START ---*
************IF   ( R10901(I) IS NOT NUMERIC )   OR
************     ( R10901(I)    =     ZERO  )
************       CONTINUE
************ELSE
            IF     R10101(I)  =  SPACE
            AND    R10501(I)  =  SPACE
            AND    R10601(I)  =  SPACE
            AND (( SERI1(I) NOT NUMERIC ) OR ( SERI1(I) = ZERO ))
            AND (( SERI1(I) NOT NUMERIC ) OR ( SERI2(I) = ZERO ))
                   CONTINUE
            ELSE
*--- 2000/02/28  NAV  END   ---*
                   MOVE    SPACE         TO   YUP-REC
                   INITIALIZE            YUP-REC
                   MOVE    R00002        TO   YUP-F02
                   MOVE    WK-RSYO(I)    TO   YUP-F03
                   MOVE    WK-SYO(I)     TO   YUP-F04
                   MOVE    WK-HTAN(I)    TO   YUP-F05
                   MOVE    R10601(I)     TO   YUP-F06
                   MOVE    R10501(I)     TO   YUP-F071
****************** MOVE    R10701(I)     TO   YUP-F072
                   MOVE    R10701(I)     TO   WK-NM21
                   MOVE    R11101(I)     TO   WK-NM22
                   MOVE    WK-NM2        TO   YUP-F072
                   MOVE    R10801(I)     TO   YUP-F08
                   IF      R10901(I)   NOT    NUMERIC
                           MOVE ZERO     TO   YUP-F09
                   ELSE
                           MOVE R10901(I) TO  YUP-F09
                   END-IF
                   MOVE    R11001(I)     TO   YUP-F10
                   MOVE    R10001(I)     TO   YUP-F11
                   MOVE    SERI1(I)      TO   YUP-F011
                   MOVE    SERI2(I)      TO   YUP-F012
*
                   WRITE   YUP-REC
                   END-WRITE
            END-IF
     END-PERFORM.
 TOUROKU-EXIT.
     EXIT.
*----------------------------------------------------------*
*      2.7        ユポラベルＦ　修正処理
*----------------------------------------------------------*
 SYUSEI-SUB              SECTION.
*
     PERFORM  VARYING  I  FROM  1  BY  1
                                   UNTIL  I  >  5
*--- 2000/02/28  NAV  START ---*
************IF    R10901(I)  IS  NOT  NUMERIC
************      CONTINUE
************ELSE
            IF    R10101(I)  =  SPACE
            AND   R10601(I)  =  SPACE
            AND   R10501(I)  =  SPACE
            AND (( SERI1(I) NOT NUMERIC ) OR ( SERI1(I) = ZERO ))
            AND (( SERI1(I) NOT NUMERIC ) OR ( SERI2(I) = ZERO ))
                  CONTINUE
            ELSE
*--- 2000/02/28  NAV  END   ---*
                  MOVE    R00002      TO   YUP-F02
                  MOVE    SPACE       TO   YUP-F03
                  MOVE    R10101(I)   TO   YUP-F03
                  MOVE    SERI1(I)    TO   YUP-F011
                  MOVE    SERI2(I)    TO   YUP-F012
                  READ    ZYUPODT1
                      INVALID
                          MOVE    SPACE         TO   YUP-REC
                          INITIALIZE            YUP-REC
                          MOVE    R00002        TO   YUP-F02
                          MOVE    WK-RSYO(I)    TO   YUP-F03
                          MOVE    WK-SYO(I)     TO   YUP-F04
                          MOVE    WK-HTAN(I)    TO   YUP-F05
                          MOVE    R10601(I)     TO   YUP-F06
                          MOVE    R10501(I)     TO   YUP-F071
************************* MOVE    R10701(I)     TO   YUP-F072
                          MOVE    R10701(I)     TO   WK-NM21
                          MOVE    R11101(I)     TO   WK-NM22
                          MOVE    WK-NM2        TO   YUP-F072
                          MOVE    R10801(I)     TO   YUP-F08
                          IF      R10901(I)   NOT    NUMERIC
                                  MOVE ZERO     TO   YUP-F09
                          ELSE
                                  MOVE R10901(I) TO  YUP-F09
                          END-IF
                          MOVE    R11001(I)     TO   YUP-F10
                          MOVE    R10001(I)     TO   YUP-F11
                          MOVE    SERI1(I)      TO   YUP-F011
                          MOVE    SERI2(I)      TO   YUP-F012
*
                          WRITE   YUP-REC
                          END-WRITE
                     NOT  INVALID
                          MOVE    WK-SYO(I)     TO   YUP-F04
                          MOVE    WK-HTAN(I)    TO   YUP-F05
                          MOVE    R10601(I)     TO   YUP-F06
                          MOVE    R10501(I)     TO   YUP-F071
************************* MOVE    R10701(I)     TO   YUP-F072
                          MOVE    R10701(I)     TO   WK-NM21
                          MOVE    R11101(I)     TO   WK-NM22
                          MOVE    WK-NM2        TO   YUP-F072
                          MOVE    R10801(I)     TO   YUP-F08
                          IF      R10901(I)   NOT    NUMERIC
                                  MOVE ZERO     TO   YUP-F09
                          ELSE
                                  MOVE R10901(I) TO  YUP-F09
                          END-IF
                          MOVE    R11001(I)     TO   YUP-F10
                          MOVE    R10001(I)     TO   YUP-F11
                          MOVE    SERI1(I)      TO   YUP-F011
                          MOVE    SERI2(I)      TO   YUP-F012
*
                          REWRITE  YUP-REC
                          END-REWRITE
                  END-READ
            END-IF
     END-PERFORM.
 SYUSEI-EXIT.
     EXIT.
*----------------------------------------------------------*
*      2.2.1.5    品単右づめ処理                           *
*----------------------------------------------------------*
 HINTAN-SUB            SECTION.
*
     MOVE     5             TO        A.
     MOVE     SPACE         TO        WK-HIN-G  WK-HINTAN-G.
     MOVE     R10201(I)     TO        WK-HIN-G.
*
     PERFORM  VARYING  IXB   FROM   5  BY  -1
                                   UNTIL   IXB  <    1
          IF  WK-HIN(IXB)    =  SPACE
              CONTINUE
          ELSE
              MOVE      WK-HIN(IXB)    TO   WK1-HINTAN(A)
              COMPUTE   A   =    A    -    1
          END-IF
     END-PERFORM.
*
     MOVE    WK-HINTAN-G    TO        R10201(I).
 HINTAN-EXIT.
     EXIT.
*----------------------------------------------------------*
*                 部門右づめ処理                           *
*----------------------------------------------------------*
 BUMON-SUB            SECTION.
*
     MOVE     4             TO        A.
     MOVE     SPACE         TO        WK-BUMON-G  WK-BUMON1-G.
     MOVE     R10801(I)     TO        WK-BUMON-G.
*
     PERFORM  VARYING  IXB   FROM   4  BY  -1
                                   UNTIL   IXB  <    1
          IF  WK-BUM(IXB)    =  SPACE
              CONTINUE
          ELSE
              MOVE      WK-BUM(IXB)    TO   WK1-BUMON(A)
              COMPUTE   A   =    A    -    1
          END-IF
     END-PERFORM.
*
     MOVE    WK-BUMON1-G    TO        R10801(I).
 BUMON-EXIT.
     EXIT.
*----------------------------------------------------------*
*      2.1.5     エラーメッセージセット１                  *
*----------------------------------------------------------*
 MSG-SEC                SECTION.
*
     IF    ERR-MSG-CD   =   ZERO
           MOVE    SPACE     TO        W00003
     ELSE
           MOVE    ERR-MSG(ERR-MSG-CD)  TO    W00003
           MOVE    ZERO                 TO    ERR-MSG-CD
     END-IF.
 MSG-EXIT.
     EXIT.
*----------------------------------------------------------*
*      2.1.5     ＢＯＤＹクリア
*----------------------------------------------------------*
 BODY-CLR-SUB           SECTION.
*
     MOVE      SPACE        TO    S00001(1)  S00001(2)
                                  S00001(3)  S00001(4)
                                  S00001(5).
*
 BODY-CLR-EXIT.
     EXIT.
*----------------------------------------------------------*
*      2.1.5     ＢＯＤＹ部反転クリア                      *
*----------------------------------------------------------*
 DSP-CLR-SUB            SECTION.
*
     MOVE    "M"             TO   EDIT-OPTION OF R00001
                                  EDIT-OPTION OF R00002.
     MOVE    " "             TO   EDIT-CURSOR OF R00001
                                  EDIT-CURSOR OF R00002.
*
     PERFORM   VARYING  I  FROM  1  BY  1
                             UNTIL  I  >  5
              MOVE    "M"         TO   EDIT-OPTION OF R10001(I)
                                       EDIT-OPTION OF R10101(I)
                                       EDIT-OPTION OF R10201(I)
                                       EDIT-OPTION OF R10301(I)
                                       EDIT-OPTION OF R10401(I)
                                       EDIT-OPTION OF R10501(I)
                                       EDIT-OPTION OF R10601(I)
                                       EDIT-OPTION OF R10701(I)
                                       EDIT-OPTION OF R11101(I)
                                       EDIT-OPTION OF R10801(I)
                                       EDIT-OPTION OF R10901(I)
                                       EDIT-OPTION OF R11001(I)
                                       EDIT-OPTION OF SERI1(I)
                                       EDIT-OPTION OF SERI2(I)
*
              MOVE     SPACE      TO   EDIT-CURSOR OF R10001(I)
                                       EDIT-CURSOR OF R10101(I)
                                       EDIT-CURSOR OF R10201(I)
                                       EDIT-CURSOR OF R10301(I)
                                       EDIT-CURSOR OF R10401(I)
                                       EDIT-CURSOR OF R10601(I)
                                       EDIT-CURSOR OF R10801(I)
                                       EDIT-CURSOR OF R10901(I)
                                       EDIT-CURSOR OF R11001(I)
                                       EDIT-CURSOR OF SERI1(I)
                                       EDIT-CURSOR OF SERI2(I)
              IF    CHK-MAE-FLG  =   1
                    CONTINUE
              ELSE
                    MOVE   SPACE  TO   EDIT-STATUS OF R10001(I)
                                       EDIT-STATUS OF R10101(I)
                                       EDIT-STATUS OF R10201(I)
                                       EDIT-STATUS OF R10301(I)
                                       EDIT-STATUS OF R10401(I)
                                       EDIT-STATUS OF R10601(I)
                                       EDIT-STATUS OF R10801(I)
                                       EDIT-STATUS OF R10901(I)
                                       EDIT-STATUS OF R11001(I)
                                       EDIT-STATUS OF SERI1(I)
                                       EDIT-STATUS OF SERI2(I)
              END-IF
     END-PERFORM.
     MOVE     ZERO                TO   CHK-MAE-FLG.
 DSP-CLR-EXIT.
     EXIT.
*----------------------------------------------------------*
*      2.2.1.1   ユポラベルＦ　ＲＥＡＤ
*----------------------------------------------------------*
 YUP-READ-SUB           SECTION.
*
     READ    ZYUPODT1
          NEXT  AT  END
             MOVE   "END"    TO   YUP-END-FLG
             GO              TO   YUP-READ-EXIT
     END-READ.
*取引先コード
     IF      YUP-F02  =   WK-TORICD
             CONTINUE
     ELSE
             MOVE   "END"    TO   YUP-END-FLG
     END-IF.
 YUP-READ-EXIT.
     EXIT.
*----------------------------------------------------------*
*      2.2.1.1   取引先マスタ　ＲＥＡＤ
*----------------------------------------------------------*
 TOR-READ-SUB           SECTION.
*
     READ    TOKMS2
           INVALID
             MOVE   "INV"    TO   TOR-INV-FLG
           NOT INVALID
             MOVE    SPACE   TO   TOR-INV-FLG
     END-READ.
 TOR-READ-EXIT.
     EXIT.
*----------------------------------------------------------*
*      2.2.1.1   商品変換テーブル　ＲＥＡＤ
*----------------------------------------------------------*
 SHOT-READ-SUB           SECTION.
*
     READ    SHOTBL1
           INVALID
             MOVE   "INV"    TO   SHO-INV-FLG
           NOT INVALID
             MOVE    SPACE   TO   SHO-INV-FLG
     END-READ.
 SHOT-READ-EXIT.
     EXIT.
*----------------------------------------------------------*
*      2.2.1.1   商品名称Ｍ　ＲＥＡＤ
*----------------------------------------------------------*
 SMEI-READ-SUB           SECTION.
*
     READ    MEIMS1
           INVALID
             MOVE   "INV"    TO   MEI-INV-FLG
           NOT INVALID
             MOVE    SPACE   TO   MEI-INV-FLG
     END-READ.
 SMEI-READ-EXIT.
     EXIT.
*----------------------------------------------------------*
*      2.1.2.1   画面表示処理１                            *
*----------------------------------------------------------*
 DSP-WRITE-SUB         SECTION.
     MOVE     SPACE          TO   DSP-PROC.
     MOVE    "SCREEN"        TO   DSP-GROUP.
     WRITE    DSP-AREA       FROM FKY18011.
 DSP-WRITE-END.
     EXIT.
*----------------------------------------------------------*
*      2.1.3     画面データの入力処理                      *
*----------------------------------------------------------*
 DSP-READ-SUB           SECTION.
     MOVE  "NE"    TO   DSP-PROC.
     READ   DSPF.
     MOVE   DSP-AREA    TO   FKY18011.
 DSP-READ-END.
     EXIT.
*----------------------------------------------------------*
*                明細行登録／修正チェック                  *
*----------------------------------------------------------*
 CHKMEI-SUB             SECTION.
*
     PERFORM   VARYING  I  FROM  1  BY  1  UNTIL  I  >  5
               MOVE     1    TO     WK-CHKMEI-G(I)
     END-PERFORM.
*
 CHKMEI-EXIT.
     EXIT.
*****************<<  PROGRAM  END  >>***********************
