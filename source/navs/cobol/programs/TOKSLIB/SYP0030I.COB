****************************************************************
*＃＃マスタ参照なしバージョン＃＃　日次更新中実行ＯＫ          *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　在庫管理システム　　　　　　　　　*
*    モジュール名　　　　：　ユポラベル印刷（入力）            *
*    作成日／更新日　　　：　2004/01/21                        *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　画面よりユポラベルＦの登録・修正を*
*                            行う。                            *
*                            （消費税総額表示対応）            *
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SYP0030I.
 AUTHOR.                NAV.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       GP6000.
 OBJECT-COMPUTER.       GP6000.
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
     SELECT   NYUPOL1  ASSIGN    TO        DA-01-VI-NYUPOL1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   DYNAMIC
                        RECORD    KEY       IS   YUP-F02
                                                 YUP-F03
                                                 YUP-F011
                                                 YUP-F012
                        FILE      STATUS    IS   YUP-STATUS.
*
 DATA                   DIVISION.
 FILE                   SECTION.
*---<<  画面ファイル  >>---*
 FD  DSPF.
 01  DSP-AREA                PIC  X(2000).
*    COPY     FYP00101  OF        XMDLIB.
*---<<  ユポラベルＦ  >>---*
 FD  NYUPOL1.
     COPY     NYUPOL1  OF        XFDLIB
              JOINING   YUP       PREFIX.
****************************************************************
 WORKING-STORAGE             SECTION.
****************************************************************
****  画面ファイル  ****
 COPY   FYP00101         OF        XMDLIB.
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
****  フラグ  ***
 01  PSW-AREA.
     02  END-FLG             PIC  X(03)  VALUE SPACE.
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
         03  WK-SAKUJO       PIC X(01).
         03  WK-SSYOCD       PIC X(13).
         03  WK-SHINT1       PIC X(05).
         03  WK-SHINT2       PIC X(02).
         03  WK-SHINT3       PIC X(01).
         03  WK-SYONM1       PIC N(13).
         03  WK-JANCD        PIC X(13).
         03  WK-SYONM2       PIC N(07).
         03  WK-SYONM3       PIC N(07).
         03  WK-BUMON        PIC X(04).
         03  WK-ZEINUK       PIC 9(05).
         03  WK-ZEIKOM       PIC 9(05).
         03  WK-MAISU        PIC 9(04).
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
       03  ERR-PG-ID         PIC  X(08)  VALUE  "SYP0030I".
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
                         PROCEDURE   NYUPOL1.
      MOVE     "NYUPOL1"       TO   ERR-FL-ID.
      MOVE      YUP-STATUS      TO   ERR-STCD.
      DISPLAY   MSG-ABEND1    UPON   CONS.
      DISPLAY   MSG-ABEND2    UPON   CONS.
      MOVE      4000            TO   PROGRAM-STATUS.
      STOP      RUN.
 END     DECLARATIVES.
************************************************************
*      0.０     プログラムＳＴＡＲＴ                       *
************************************************************
 SYP0030I-START         SECTION.
     PERFORM      INIT-SEC.
     PERFORM      MAIN-SEC
                  UNTIL     END-FLG  =    "END".
     PERFORM      END-SEC.
     STOP      RUN.
 SYP0030I-END.
     EXIT.
************************************************************
*      _０     初期処理                                   *
************************************************************
 INIT-SEC               SECTION.
*
     OPEN     I-O       DSPF      NYUPOL1.
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
     CLOSE    DSPF      NYUPOL1.
 END-END.
     EXIT.
*----------------------------------------------------------*
*      2.1       初期画面表示     ( MAIN-FLG = 1 )         *
*----------------------------------------------------------*
 DSP-INIT-SUB         SECTION.
     MOVE     SPACE          TO   FYP00101.
     MOVE    "FYP00101"      TO   DSP-FORMAT.
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
     MOVE     PMSG01               TO   PFGAID.
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
     IF    SYORIC  IS NOT  NUMERIC
           MOVE   02      TO   ERR-MSG-CD
           MOVE   "R"     TO   EDIT-OPTION  OF  SYORIC
           MOVE   "C"     TO   EDIT-CURSOR  OF  SYORIC
           MOVE    SPACE  TO   SYORIN
           GO             TO   SYORI-EXIT
     END-IF.
     IF (  SYORIC    =   1    )  OR
        (  SYORIC    =   2    )
         MOVE   "D"     TO   EDIT-OPTION  OF  SYORIC
         MOVE   " "     TO   EDIT-CURSOR  OF  SYORIC
         IF      SYORIC  =  1
                 MOVE   NC"登録"      TO      SYORIN
                 MOVE   1             TO      SYORI-F
         ELSE
                 MOVE   NC"修正"      TO      SYORIN
                 MOVE   2             TO      SYORI-F
         END-IF
     ELSE
         MOVE   02      TO   ERR-MSG-CD
         MOVE   "R"     TO   EDIT-OPTION  OF  SYORIC
         MOVE   "C"     TO   EDIT-CURSOR  OF  SYORIC
         MOVE    SPACE  TO   SYORIN
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
     MOVE      PMSG02         TO   PFGAID.
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
     IF   ( TORICD IS NOT NUMERIC )  OR
          ( TORICD     =     ZERO )
            MOVE   SPACE       TO   TORINM
            MOVE   04          TO   ERR-MSG-CD
            MOVE  "R"          TO   EDIT-OPTION OF TORICD
            MOVE  "C"          TO   EDIT-CURSOR OF TORICD
            GO                 TO   TRCD-EXIT
     ELSE
           IF      SYORIC  =  1
                   MOVE    5   TO   MAIN-FLG
           ELSE
                   MOVE    4   TO   MAIN-FLG
           END-IF
           MOVE    TORICD      TO   WK-TORICD
     END-IF.
*
 TRCD-EXIT.
     EXIT.
*----------------------------------------------------------*
*      2.3   修正時ＢＯＤＹ部入力   ( MAIN-FLG = 4 )
*----------------------------------------------------------*
 BODY-SYUSEI-SUB       SECTION.
*
     PERFORM  MSG-SEC.
     MOVE     PMSG02         TO   PFGAID.
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
     MOVE   TORICD          TO    YUP-F02.
     MOVE   SPACE           TO    YUP-F03.
     MOVE   SSYOCD(1)       TO    YUP-F03.
     MOVE   SERI1(1)        TO    YUP-F011.
     MOVE   SERI2(1)        TO    YUP-F012.
     START  NYUPOL1 KEY IS >= YUP-F02 YUP-F03 YUP-F011 YUP-F012
           INVALID
            MOVE     "R"    TO    EDIT-OPTION OF SSYOCD(1)
            MOVE     "C"    TO    EDIT-CURSOR OF SSYOCD(1)
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
            MOVE     "D"    TO    EDIT-OPTION OF SSYOCD(1)
            MOVE      SPACE TO    EDIT-CURSOR OF SSYOCD(1)
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
              MOVE   PMSG02      TO   PFGAID
     ELSE
              EVALUATE  PAGE-CNT
                  WHEN   1
                         IF    YUP-END-FLG   =  "END"
                               MOVE  PMSG02  TO  PFGAID
                         ELSE  MOVE  PMSG04  TO  PFGAID
                         END-IF
                  WHEN   99    MOVE  PMSG05  TO  PFGAID
                  WHEN   OTHER
                         IF    YUP-END-FLG   =  "END"
                               MOVE  PMSG05  TO  PFGAID
                         ELSE  MOVE  PMSG03  TO  PFGAID
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
                          MOVE      TORICD       TO   YUP-F02
                          MOVE      SPACE        TO   YUP-F03
                          MOVE      WK-SYOCD-MAE(PAGE-CNT)
                                                 TO   YUP-F03
                          MOVE      WK-SERI1-MAE(PAGE-CNT)
                                                 TO   YUP-F011
                          MOVE      WK-SERI2-MAE(PAGE-CNT)
                                                 TO   YUP-F012
                          MOVE      SPACE        TO   YUP-END-FLG
                          START   NYUPOL1
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
                             MOVE  TORICD       TO  YUP-F02
                             MOVE  SPACE        TO  YUP-F03
                             MOVE  WK-SYOCD-ATO TO  YUP-F03
                             MOVE  WK-SERI1-ATO TO  YUP-F011
                             MOVE  WK-SERI2-ATO TO  YUP-F012
                             MOVE  SPACE        TO  YUP-END-FLG
                             START  NYUPOL1
                               INVALID
                                 MOVE  12  TO   ERR-MSG-CD
                               NOT  INVALID
*--- 前頁ポインタの格納 ---*
                                 MOVE     SSYOCD(1) TO
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
     IF (SAKUJO(I) = SPACE)  AND  (SSYOCD(I) = SPACE)  AND
        (SHINT1(I) = SPACE)  AND  (SHINT2(I) = SPACE)  AND
        (SHINT3(I) = SPACE)  AND  (SYONM1(I) = SPACE)  AND
        (JANCD(I) = SPACE)  AND  (SYONM2(I) = SPACE)  AND
        (SYONM3(I) = SPACE)  AND  (BUMON(I) = SPACE)  AND
        ((MAISU(I) IS NOT NUMERIC) OR (MAISU(I) = ZERO ))
           GO                      TO   BODY-CHK-EXIT
     ELSE
           ADD       1             TO   MEISAI-CNT
     END-IF.
*削除フラグ
     IF  SAKUJO(I)    =  "9"   OR   " "
         CONTINUE
     ELSE
         IF    ERR-MSG-CD  =  ZERO
               MOVE    9      TO   ERR-MSG-CD
         END-IF
         MOVE   "R"     TO   EDIT-OPTION  OF  SAKUJO(I)
         MOVE   "C"     TO   EDIT-CURSOR  OF  SAKUJO(I)
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
     IF     WK-CHKMEI-G(I)  =  1
            MOVE   TORICD    TO    YUP-F02
            MOVE   SSYOCD(I) TO    YUP-F03
            MOVE   SERI1(I)  TO    YUP-F011
            MOVE   SERI2(I)  TO    YUP-F012
            READ   NYUPOL1
               INVALID
                 CONTINUE
               NOT  INVALID
                 IF    ERR-MSG-CD  =  ZERO
                       MOVE   7     TO   ERR-MSG-CD
                 END-IF
                 MOVE  "R"    TO   EDIT-OPTION OF SSYOCD(I)
                 MOVE  "R"    TO   EDIT-OPTION OF SHINT1(I)
                 MOVE  "R"    TO   EDIT-OPTION OF SHINT2(I)
                 MOVE  "R"    TO   EDIT-OPTION OF SHINT3(I)
                 MOVE  "R"    TO   EDIT-OPTION OF SERI1(I)
                 MOVE  "R"    TO   EDIT-OPTION OF SERI2(I)
                 MOVE  "C"    TO   EDIT-CURSOR OF SSYOCD(I)
            END-READ
     END-IF.
*<<
*---    枚数
     IF     MAISU(I) IS NOT NUMERIC
            MOVE   ZERO      TO    MAISU(I)
     END-IF.
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
                  (( SSYOCD(A) = SPACE )      AND
                   ( SHINT1(A) = SPACE )      AND
                   ( SHINT2(A) = SPACE )      AND
                   ( SHINT3(A) = SPACE )      AND
                   ( JANCD(A) = SPACE )      AND
                   ( SERI1(A)  NOT NUMERIC
                 OR  SERI1(A)  =  ZERO )      AND
                   ( SERI2(A)  NOT NUMERIC
                 OR  SERI2(A)  =  ZERO ))     OR
                  (( SSYOCD(I) = SPACE )      AND
                   ( SHINT1(I) = SPACE )      AND
                   ( SHINT2(I) = SPACE )      AND
                   ( SHINT3(I) = SPACE )      AND
                   ( JANCD(I) = SPACE )      AND
                   ( SERI1(I)  NOT NUMERIC
                 OR  SERI1(I)  =  ZERO )      AND
                   ( SERI2(I)  NOT NUMERIC
                 OR  SERI2(I)  =  ZERO ))
                    CONTINUE
              ELSE
                    IF  ( SSYOCD(A)  =  SSYOCD(I)) AND
                        ( SHINT1(A)  =  SHINT1(I)) AND
                        ( SHINT2(A)  =  SHINT2(I)) AND
                        ( SHINT3(A)  =  SHINT3(I)) AND
                        ( JANCD(A)  =  JANCD(I)) AND
                        ( SERI1(A)   =  SERI1(I))  AND
                        ( SERI2(A)   =  SERI2(I))
                        MOVE   15  TO   ERR-MSG-CD
                        MOVE  "R"  TO   EDIT-OPTION OF SSYOCD(A)
                                        EDIT-OPTION OF SHINT1(A)
                                        EDIT-OPTION OF SHINT2(A)
                                        EDIT-OPTION OF SHINT3(A)
                                        EDIT-OPTION OF JANCD(A)
                                        EDIT-OPTION OF SERI1(A)
                                        EDIT-OPTION OF SERI2(A)
                        MOVE  "R"  TO   EDIT-OPTION OF SSYOCD(I)
                                        EDIT-OPTION OF SHINT1(I)
                                        EDIT-OPTION OF SHINT2(I)
                                        EDIT-OPTION OF SHINT3(I)
                                        EDIT-OPTION OF JANCD(I)
                                        EDIT-OPTION OF SERI1(I)
                                        EDIT-OPTION OF SERI2(I)
                        MOVE  "C"  TO   EDIT-CURSOR OF SSYOCD(A)
                                        EDIT-CURSOR OF SHINT1(A)
                                        EDIT-CURSOR OF SHINT2(A)
                                        EDIT-CURSOR OF SHINT3(A)
                                        EDIT-CURSOR OF JANCD(A)
                                        EDIT-CURSOR OF SERI1(A)
                                        EDIT-CURSOR OF SERI2(A)
                        MOVE  "C"  TO   EDIT-CURSOR OF SSYOCD(I)
                                        EDIT-CURSOR OF SHINT1(I)
                                        EDIT-CURSOR OF SHINT2(I)
                                        EDIT-CURSOR OF SHINT3(I)
                                        EDIT-CURSOR OF JANCD(I)
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
       IF   ((SSYOCD(A) IS NOT NUMERIC ) OR ( SSYOCD(A) = SPACE))
        AND ((SHINT1(A) IS NOT NUMERIC ) OR ( SHINT1(A) = SPACE))
        AND ((SHINT2(A) IS NOT NUMERIC ) OR ( SHINT2(A) = SPACE))
        AND ((SHINT3(A) IS NOT NUMERIC ) OR ( SHINT3(A) = SPACE))
        AND ((SYONM1(A) IS NOT NUMERIC ) OR ( SYONM1(A) = SPACE))
        AND ((JANCD(A) IS NOT NUMERIC ) OR ( JANCD(A) = SPACE))
        AND ((SERI1(A)  IS NOT NUMERIC ) OR ( SERI1(A)  = ZERO ))
        AND ((SERI2(A)  IS NOT NUMERIC ) OR ( SERI2(A)  = ZERO ))
              CONTINUE
       ELSE
            MOVE     WK-RSYO(A)       TO   WK-CON-RSYO(I)
            MOVE     WK-SYO(A)        TO   WK-CON-SYO(I)
            MOVE     WK-HTAN(A)       TO   WK-CON-HTAN(I)
            MOVE     SAKUJO(A)        TO   WK-SAKUJO(I)
            MOVE     SSYOCD(A)        TO   WK-SSYOCD(I)
            MOVE     SHINT1(A)        TO   WK-SHINT1(I)
            MOVE     SHINT2(A)        TO   WK-SHINT2(I)
            MOVE     SHINT3(A)        TO   WK-SHINT3(I)
            MOVE     SYONM1(A)        TO   WK-SYONM1(I)
            MOVE     JANCD(A)         TO   WK-JANCD(I)
            MOVE     SYONM2(A)        TO   WK-SYONM2(I)
            MOVE     SYONM3(A)        TO   WK-SYONM3(I)
            MOVE     BUMON(A)         TO   WK-BUMON(I)
            MOVE     ZEINUK(A)        TO   WK-ZEINUK(I)
            MOVE     ZEIKOM(A)        TO   WK-ZEIKOM(I)
            MOVE     MAISU(A)         TO   WK-MAISU(I)
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
         IF     WK-SSYOCD(I)  =  SPACE
         AND    WK-SYONM1(I)  =  SPACE
         AND    WK-JANCD(I)  =  SPACE
                CONTINUE
         ELSE
                MOVE   WK-CON-RSYO(I)     TO     WK-RSYO(A)
                MOVE   WK-CON-SYO(I)      TO     WK-SYO(A)
                MOVE   WK-CON-HTAN(I)     TO     WK-HTAN(A)
                MOVE   WK-SAKUJO(I)       TO     SAKUJO(A)
                MOVE   WK-SSYOCD(I)       TO     SSYOCD(A)
                MOVE   WK-SHINT1(I)       TO     SHINT1(A)
                MOVE   WK-SHINT2(I)       TO     SHINT2(A)
                MOVE   WK-SHINT3(I)       TO     SHINT3(A)
                MOVE   WK-SYONM1(I)       TO     SYONM1(A)
                MOVE   WK-JANCD(I)        TO     JANCD(A)
                MOVE   WK-SYONM2(I)       TO     SYONM2(A)
                MOVE   WK-SYONM3(I)       TO     SYONM3(A)
                MOVE   WK-BUMON(I)        TO     BUMON(A)
                MOVE   WK-ZEINUK(I)       TO     ZEINUK(A)
                MOVE   WK-ZEIKOM(I)       TO     ZEIKOM(A)
                MOVE   WK-MAISU(I)        TO     MAISU(A)
                MOVE   WK-SERI1(I)        TO     SERI1(A)
                MOVE   WK-SERI2(I)        TO     SERI2(A)
*プロテクトセット
                IF   (  SYORI-F   =   2  )  AND
                     (  A  <= PRTECT-CNT )
                       MOVE  "X" TO  EDIT-STATUS OF SSYOCD(A)
                                     EDIT-STATUS OF SHINT1(A)
                                     EDIT-STATUS OF SHINT2(A)
                                     EDIT-STATUS OF SHINT3(A)
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
     IF       JANCD(I)   =  SPACE
              GO             TO      CHK-DEJIT-EXIT
     END-IF.
     INITIALIZE   WK-CHKDG-AREA.
     MOVE      JANCD(I)     TO    WK-CHKDG-R.
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
     MOVE     WK-CHKDG-R     TO        JANCD(I).
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
               MOVE    YUP-F11          TO       SAKUJO(I)
               IF      YUP-F03  =  YUP-F04
                       MOVE  YUP-F03      TO    SSYOCD(I)
                       MOVE  YUP-F05      TO    WK-HINTAN
                       MOVE  WK-HINTAN-1  TO    SHINT1(I)
                       MOVE  WK-HINTAN-2  TO    SHINT2(I)
                       MOVE  WK-HINTAN-3  TO    SHINT3(I)
               ELSE
                       MOVE  YUP-F03      TO    SSYOCD(I)
                       MOVE  SPACE        TO    SHINT1(I)
                       MOVE  SPACE        TO    SHINT2(I)
                       MOVE  SPACE        TO    SHINT3(I)
               END-IF
               MOVE    YUP-F05          TO       WK-HINTAN
               MOVE    YUP-F06          TO       JANCD(I)
               MOVE    YUP-F071         TO       SYONM1(I)
               MOVE    YUP-F072(1:7)    TO       SYONM2(I)
               MOVE    YUP-F072(8:7)    TO       SYONM3(I)
               MOVE    YUP-F08          TO       BUMON(I)
               MOVE    YUP-F09          TO       ZEINUK(I)
               MOVE    YUP-F12          TO       ZEIKOM(I)
               MOVE    YUP-F10          TO       MAISU(I)
               MOVE    YUP-F011         TO       SERI1(I)
               MOVE    YUP-F012         TO       SERI2(I)
               MOVE   "X"   TO    EDIT-STATUS OF SSYOCD(I)
                                  EDIT-STATUS OF SHINT1(I)
                                  EDIT-STATUS OF SHINT2(I)
                                  EDIT-STATUS OF SHINT3(I)
                                  EDIT-STATUS OF SERI1(I)
                                  EDIT-STATUS OF SERI2(I)
*
               MOVE       2                   TO WK-CHKMEI-G(I)
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
         MOVE     "Y"            TO   KAKUNI
     END-IF.
     PERFORM    MSG-SEC.
*
     IF       SYORI-F     =  1
              MOVE   PMSG02      TO   PFGAID
     ELSE
              EVALUATE  PAGE-CNT
                  WHEN   1       MOVE  PMSG04  TO  PFGAID
                  WHEN   99      MOVE  PMSG05  TO  PFGAID
                  WHEN   OTHER   MOVE  PMSG03  TO  PFGAID
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
                   MOVE     SPACE       TO   KAKUNI
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
                           MOVE     TORICD       TO   YUP-F02
                           MOVE     SPACE        TO   YUP-F03
                           MOVE     WK-SYOCD-MAE(PAGE-CNT)
                                                 TO   YUP-F03
                           MOVE     WK-SERI1-MAE(PAGE-CNT)
                                                 TO   YUP-F011
                           MOVE     WK-SERI2-MAE(PAGE-CNT)
                                                 TO   YUP-F012
                           MOVE     SPACE        TO   YUP-END-FLG
                           PERFORM CHKMEI-SUB
                           START  NYUPOL1
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
                 MOVE    SPACE  TO   KAKUNI
*次頁
         WHEN   "F012"
                 IF    SYORI-F   =   2
                   IF   PAGE-CNT  =  99
                        MOVE     12     TO   ERR-MSG-CD
                   ELSE
                        PERFORM  SYUSEI-SUB
*--- 前頁ポインタの格納 ---*
                        MOVE     SSYOCD(1) TO
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
                             MOVE    TORICD       TO   YUP-F02
                             MOVE    SPACE        TO   YUP-F03
                             MOVE    WK-SYOCD-ATO TO   YUP-F03
                             MOVE    WK-SERI1-ATO TO   YUP-F011
                             MOVE    WK-SERI2-ATO TO   YUP-F012
                             MOVE    SPACE        TO   YUP-END-FLG
                             START  NYUPOL1
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
                 MOVE    SPACE  TO   KAKUNI
         WHEN   "E000"
                   IF    KAKUNI  NOT  =  "Y"
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
            IF     SSYOCD(I)  =  SPACE
            AND    SYONM1(I)  =  SPACE
            AND    JANCD(I)  =  SPACE
            AND (( SERI1(I) NOT NUMERIC ) OR ( SERI1(I) = ZERO ))
            AND (( SERI1(I) NOT NUMERIC ) OR ( SERI2(I) = ZERO ))
                   CONTINUE
            ELSE
                   MOVE    SPACE         TO   YUP-REC
                   INITIALIZE            YUP-REC
                   MOVE    TORICD        TO   YUP-F02
                   MOVE    WK-RSYO(I)    TO   YUP-F03
                   MOVE    WK-SYO(I)     TO   YUP-F04
                   MOVE    WK-HTAN(I)    TO   YUP-F05
                   MOVE    JANCD(I)     TO   YUP-F06
                   MOVE    SYONM1(I)     TO   YUP-F071
                   MOVE    SYONM2(I)     TO   WK-NM21
                   MOVE    SYONM3(I)     TO   WK-NM22
                   MOVE    WK-NM2        TO   YUP-F072
                   MOVE    BUMON(I)     TO   YUP-F08
                   IF      ZEINUK(I)   NOT    NUMERIC
                           MOVE ZERO     TO   YUP-F09
                   ELSE
                           MOVE ZEINUK(I) TO  YUP-F09
                   END-IF
                   IF      ZEIKOM(I)   NOT    NUMERIC
                           MOVE ZERO     TO   YUP-F12
                   ELSE
                           MOVE ZEIKOM(I) TO  YUP-F12
                   END-IF
                   MOVE    MAISU(I)      TO   YUP-F10
                   MOVE    SAKUJO(I)     TO   YUP-F11
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
            IF    SSYOCD(I)  =  SPACE
            AND   JANCD(I)  =  SPACE
            AND   SYONM1(I)  =  SPACE
            AND (( SERI1(I) NOT NUMERIC ) OR ( SERI1(I) = ZERO ))
            AND (( SERI1(I) NOT NUMERIC ) OR ( SERI2(I) = ZERO ))
                  CONTINUE
            ELSE
                  MOVE    TORICD      TO   YUP-F02
                  MOVE    SPACE       TO   YUP-F03
                  MOVE    SSYOCD(I)   TO   YUP-F03
                  MOVE    SERI1(I)    TO   YUP-F011
                  MOVE    SERI2(I)    TO   YUP-F012
                  READ    NYUPOL1
                      INVALID
                          MOVE    SPACE         TO   YUP-REC
                          INITIALIZE            YUP-REC
                          MOVE    TORICD        TO   YUP-F02
                          MOVE    WK-RSYO(I)    TO   YUP-F03
                          MOVE    WK-SYO(I)     TO   YUP-F04
                          MOVE    WK-HTAN(I)    TO   YUP-F05
                          MOVE    JANCD(I)     TO   YUP-F06
                          MOVE    SYONM1(I)     TO   YUP-F071
                          MOVE    SYONM2(I)     TO   WK-NM21
                          MOVE    SYONM3(I)     TO   WK-NM22
                          MOVE    WK-NM2        TO   YUP-F072
                          MOVE    BUMON(I)     TO   YUP-F08
                          IF      ZEINUK(I)   NOT    NUMERIC
                                  MOVE ZERO     TO   YUP-F09
                          ELSE
                                  MOVE ZEINUK(I) TO  YUP-F09
                          END-IF
                          IF      ZEIKOM(I)   NOT    NUMERIC
                                  MOVE ZERO     TO   YUP-F12
                          ELSE
                                  MOVE ZEIKOM(I) TO  YUP-F12
                          END-IF
                          MOVE    MAISU(I)      TO   YUP-F10
                          MOVE    SAKUJO(I)     TO   YUP-F11
                          MOVE    SERI1(I)      TO   YUP-F011
                          MOVE    SERI2(I)      TO   YUP-F012
*
                          WRITE   YUP-REC
                          END-WRITE
                     NOT  INVALID
                          MOVE    WK-SYO(I)     TO   YUP-F04
                          MOVE    WK-HTAN(I)    TO   YUP-F05
                          MOVE    JANCD(I)     TO   YUP-F06
                          MOVE    SYONM1(I)     TO   YUP-F071
                          MOVE    SYONM2(I)     TO   WK-NM21
                          MOVE    SYONM3(I)     TO   WK-NM22
                          MOVE    WK-NM2        TO   YUP-F072
                          MOVE    BUMON(I)     TO   YUP-F08
                          IF      ZEINUK(I)   NOT    NUMERIC
                                  MOVE ZERO     TO   YUP-F09
                          ELSE
                                  MOVE ZEINUK(I) TO  YUP-F09
                          END-IF
                          IF      ZEIKOM(I)   NOT    NUMERIC
                                  MOVE ZERO     TO   YUP-F12
                          ELSE
                                  MOVE ZEIKOM(I) TO  YUP-F12
                          END-IF
                          MOVE    MAISU(I)      TO   YUP-F10
                          MOVE    SAKUJO(I)     TO   YUP-F11
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
     MOVE     SHINT1(I)     TO        WK-HIN-G.
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
     MOVE    WK-HINTAN-G    TO        SHINT1(I).
 HINTAN-EXIT.
     EXIT.
*----------------------------------------------------------*
*                 部門右づめ処理                           *
*----------------------------------------------------------*
 BUMON-SUB            SECTION.
*
     MOVE     4             TO        A.
     MOVE     SPACE         TO        WK-BUMON-G  WK-BUMON1-G.
     MOVE     BUMON(I)     TO        WK-BUMON-G.
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
     MOVE    WK-BUMON1-G    TO        BUMON(I).
 BUMON-EXIT.
     EXIT.
*----------------------------------------------------------*
*      2.1.5     エラーメッセージセット１                  *
*----------------------------------------------------------*
 MSG-SEC                SECTION.
*
     IF    ERR-MSG-CD   =   ZERO
           MOVE    SPACE     TO        MSGNM
     ELSE
           MOVE    ERR-MSG(ERR-MSG-CD)  TO    MSGNM
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
     MOVE    "M"             TO   EDIT-OPTION OF SYORIC
                                  EDIT-OPTION OF TORICD.
     MOVE    " "             TO   EDIT-CURSOR OF SYORIC
                                  EDIT-CURSOR OF TORICD.
*
     PERFORM   VARYING  I  FROM  1  BY  1
                             UNTIL  I  >  5
              MOVE    "M"         TO   EDIT-OPTION OF SAKUJO(I)
                                       EDIT-OPTION OF SSYOCD(I)
                                       EDIT-OPTION OF SHINT1(I)
                                       EDIT-OPTION OF SHINT2(I)
                                       EDIT-OPTION OF SHINT3(I)
                                       EDIT-OPTION OF SYONM1(I)
                                       EDIT-OPTION OF JANCD(I)
                                       EDIT-OPTION OF SYONM2(I)
                                       EDIT-OPTION OF SYONM3(I)
                                       EDIT-OPTION OF BUMON(I)
                                       EDIT-OPTION OF ZEINUK(I)
                                       EDIT-OPTION OF ZEIKOM(I)
                                       EDIT-OPTION OF MAISU(I)
                                       EDIT-OPTION OF SERI1(I)
                                       EDIT-OPTION OF SERI2(I)
*
              MOVE     SPACE      TO   EDIT-CURSOR OF SAKUJO(I)
                                       EDIT-CURSOR OF SSYOCD(I)
                                       EDIT-CURSOR OF SHINT1(I)
                                       EDIT-CURSOR OF SHINT2(I)
                                       EDIT-CURSOR OF SHINT3(I)
                                       EDIT-CURSOR OF JANCD(I)
                                       EDIT-CURSOR OF BUMON(I)
                                       EDIT-CURSOR OF ZEINUK(I)
                                       EDIT-CURSOR OF ZEIKOM(I)
                                       EDIT-CURSOR OF MAISU(I)
                                       EDIT-CURSOR OF SERI1(I)
                                       EDIT-CURSOR OF SERI2(I)
              IF    CHK-MAE-FLG  =   1
                    CONTINUE
              ELSE
                    MOVE   SPACE  TO   EDIT-STATUS OF SAKUJO(I)
                                       EDIT-STATUS OF SSYOCD(I)
                                       EDIT-STATUS OF SHINT1(I)
                                       EDIT-STATUS OF SHINT2(I)
                                       EDIT-STATUS OF SHINT3(I)
                                       EDIT-STATUS OF JANCD(I)
                                       EDIT-STATUS OF BUMON(I)
                                       EDIT-STATUS OF ZEINUK(I)
                                       EDIT-STATUS OF ZEIKOM(I)
                                       EDIT-STATUS OF MAISU(I)
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
     READ    NYUPOL1
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
*      2.1.2.1   画面表示処理１                            *
*----------------------------------------------------------*
 DSP-WRITE-SUB         SECTION.
     MOVE     SPACE          TO   DSP-PROC.
     MOVE    "SCREEN"        TO   DSP-GROUP.
     WRITE    DSP-AREA       FROM FYP00101.
 DSP-WRITE-END.
     EXIT.
*----------------------------------------------------------*
*      2.1.3     画面データの入力処理                      *
*----------------------------------------------------------*
 DSP-READ-SUB           SECTION.
     MOVE  "NE"    TO   DSP-PROC.
     READ   DSPF.
     MOVE   DSP-AREA    TO   FYP00101.
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