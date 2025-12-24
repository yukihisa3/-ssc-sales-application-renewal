***********************************************************
*
*    顧客名　　　　　：（株）サカタのタネ殿
*    サブシステム　　：ＨＧ基幹システム苗業務連携
*    業務名　　　　　：
*    モジュール名　　：小売連携漏れチェックリスト出力
*    作成日／更新日　：2011/11/14
*    作成者／更新者　：飯田/NAV
*    処理概要　　　　：
*      苗業務システムへの連携漏れの有無をチェックするため、
*      売上伝票ファイルの状態を判定し、未連携の明細をリスト
*      出力する。先行プログラムにより、中間ワークには出力デ
*      ータが抽出されている。
*
***********************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SNA0240L.
 AUTHOR.               S.I.
 DATE-WRITTEN.         2011/11/01.
****************************************************************
 ENVIRONMENT           DIVISION.
****************************************************************
 CONFIGURATION         SECTION.
 SPECIAL-NAMES.
     CONSOLE    IS  CONS
     YA         IS  PIT-2
     YA-21      IS  PIT-2-2W
     YA-22      IS  PIT-2-2W2H
     YB         IS  PIT-1V5
     YB-21      IS  PIT-1V5-2W
     YB-22      IS  PIT-1V5-2W2H.
*
 INPUT-OUTPUT          SECTION.
 FILE-CONTROL.

* チェックリスト中間ワーク
     SELECT  NARERRF
       ASSIGN      TO  NARERRL1
       ORGANIZATION    INDEXED
       ACCESS    MODE  SEQUENTIAL
       RECORD    KEY
         ERR-F01 *> 担当者コード
         ERR-F03 *> 受注種別
         ERR-F04 *> 取引先コード
         ERR-F06 *> 納品日
         ERR-F07 *> 自社商品コード
         ERR-F08 *> 品単コード
       FILE STATUS     ERR-ST.

* プリントファイル
     SELECT  PRTFILE
****** ASSIGN    TO  SP-31
       ASSIGN    TO  LP-04
       FILE  STATUS  PRT-ST.
****************************************************************
 DATA                DIVISION.
****************************************************************
 FILE                SECTION.
****************************************************************
*    FILE = チェックリスト中間ワーク SF                        *
****************************************************************
 FD  NARERRF
     LABEL     RECORD    IS   STANDARD.
     COPY      NARERRF   OF   XFDLIB
     JOINING   ERR       AS   PREFIX.
****************************************************************
*    FILE = プリントファイル                                   *
****************************************************************
 FD  PRTFILE
     LABEL RECORD OMITTED.

 01  PRT-REC.
     03  FILLER             PIC  X(300).

****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
*ステータス領域
 01  STATUS-AREA.
     03  ERR-ST             PIC  X(02).
     03  PRT-ST             PIC  X(02).
*フラグ領域
 01  FLG-AREA.
     03  END-FLG            PIC  X(03)  VALUE  SPACE.
     03  FG-NARERRF-END     PIC  X(03).
     03  FG-KYOSEI-PRT      PIC  9(01).
*ワーク領域
 01  WRK-AREA.
     03  S-NAME-SV          PIC  X(30).
     03  CT-PG              PIC  9(08).
     03  CT-IN              PIC  9(08).
     03  CT-LINE            PIC  9(03).
     03  WK-CT-LINE         PIC  9(03).
     03  WK-JIKKO-LINE      PIC  9(03).
     03  WK-YMD             PIC  9(08).
     03  WK-YMDR  REDEFINES WK-YMD.
       05  WK-YMD-Y         PIC  9(04).
       05  WK-YMD-M         PIC  9(02).
       05  WK-YMD-D         PIC  9(02).

     03  WK-CMPYMD.
       05  WK-CMPYMD-Y      PIC S9(04).
       05  WK-CMPYMD-M      PIC S9(02).
       05  WK-CMPYMD-D      PIC S9(02).

     03  WK-SYO             PIC  9(06).
     03  WK-AMARI           PIC  9(06).

     03  IX                 PIC  9(04).
     03  IX2                PIC  9(04).
     03  BRK-JSYUBT         PIC  X(01).
     03  BRK-TORCD          PIC  9(08).
     03  BRK-NOHNBI         PIC  9(08).
     03  DOJ-JSYUBT         PIC  X(01).
     03  DOJ-TORCD          PIC  9(08).
     03  DOJ-NOHNBI         PIC  9(08).


*  エラーセクション名
 01  SEC-NAME.
     03  FILLER             PIC  X(05)  VALUE " *** ".
     03  S-NAME             PIC  X(30).

*　システム日付／時刻
 01  TIME-AREA.
     03  WK-TIME            PIC  9(08)  VALUE  ZERO.
 01  DATE-AREA.
     03  WK-DATE            PIC  9(06)  VALUE  ZERO.
     03  SYS-DATE           PIC  9(08)  VALUE  ZERO.

*　日付論理チェック
 01  WK-CHKDATE.
     03  WK-CHKDATE-YYYY    PIC  9(04)  VALUE  ZERO.
     03  WK-CHKDATE-MM      PIC  9(02)  VALUE  ZERO.
     03  WK-CHKDATE-DD      PIC  9(02)  VALUE  ZERO.

     03  WK-REN-NO-X        PIC  X(09).
     03  WK-REN-NO   REDEFINES WK-REN-NO-X
                            PIC  9(09).
 01  HEAD01  CHARACTER TYPE IS PIT-2.
     03  FILLER  PIC  X(08)  VALUE "SNA0240L".
     03  FILLER  PIC  X(105) VALUE SPACE.
     03  HD01-Y  PIC  9(04).
     03  FILLER  PIC  N(01)  VALUE NC"年".
     03  HD01-M  PIC  Z9.
     03  FILLER  PIC  N(01)  VALUE NC"月".
     03  HD01-D  PIC  Z9.
     03  FILLER  PIC  N(01)  VALUE NC"日".
     03  FILLER  PIC  X(02)  VALUE SPACE.
     03  HD01-PG PIC  ZZ9.
     03  FILLER  PIC  N(01)  VALUE NC"頁".

 01  HEAD02.
     03  FILLER  PIC  X(31)  VALUE SPACE.
     03  FILLER  PIC  N(15)  VALUE
     NC"＜小売連携漏れチェックリスト＞"
                      CHARACTER TYPE IS PIT-2-2W2H.

 01  HEAD03  CHARACTER TYPE IS PIT-2.
     03  FILLER  PIC  X(42)  VALUE SPACE.
     03  FILLER  PIC  N(07)  VALUE NC"自社商品コード".
     03  FILLER  PIC  X(03)  VALUE SPACE.
     03  FILLER  PIC  N(05)  VALUE NC"商　品　名".

 01  HEAD04.
     03  FILLER  PIC  X(02)  VALUE SPACE.
     03  FILLER  PIC  N(04)  VALUE NC"受注種別"
                             CHARACTER TYPE IS PIT-2.
     03  FILLER  PIC  X(03)  VALUE SPACE.
     03  FILLER  PIC  N(03)  VALUE NC"取引先"
                             CHARACTER TYPE IS PIT-2.
     03  FILLER  PIC  X(07)  VALUE SPACE.
     03  FILLER  PIC  N(03)  VALUE NC"納品日"
                             CHARACTER TYPE IS PIT-2.
     03  FILLER  PIC  X(10)  VALUE SPACE.
     03  FILLER  PIC  N(07)  VALUE NC"相手商品コード"
                             CHARACTER TYPE IS PIT-2.
     03  FILLER  PIC  X(37)  VALUE SPACE.
     03  FILLER  PIC  N(03)  VALUE NC"明細数"
                             CHARACTER TYPE IS PIT-2.
     03  FILLER  PIC  X(02)  VALUE SPACE.
     03  FILLER  PIC  N(06)  VALUE NC"依頼予定数　"
                             CHARACTER TYPE IS PIT-1V5.
     03  FILLER  PIC  X(01)  VALUE SPACE.
     03  FILLER  PIC  N(03)  VALUE NC"担当者"
                             CHARACTER TYPE IS PIT-2.

 01  YKSEN.
     03  FILLER  PIC  X(135)  VALUE ALL "-".

 01  MEISAI01.
     03  FILLER       PIC  X(02).
     03  MS01-JSYUBT  PIC  N(05)
                           CHARACTER TYPE IS PIT-1V5.
     03  FILLER       PIC  N(01)
                           CHARACTER TYPE IS PIT-1V5.
     03  FILLER       PIC  X(02).
     03  MS01-TORCD   PIC  X(08).
     03  FILLER       PIC  X(02).
     03  MS01-NOHNBI.
       05  MS01-NOHNBI-Y  PIC  Z(04).
       05  MS01-NOHNBI-YN PIC  N(01)
                          CHARACTER TYPE IS PIT-2.
       05  MS01-NOHNBI-M  PIC  Z(02).
       05  MS01-NOHNBI-MN PIC  N(01)
                          CHARACTER TYPE IS PIT-2.
       05  MS01-NOHNBI-D  PIC  Z(02).
       05  MS01-NOHNBI-DN PIC  N(01)
                          CHARACTER TYPE IS PIT-2.
     03  FILLER       PIC  X(05).
     03  MS01-SHOCD   PIC  X(08).
     03  MS01-HINTAN  PIC  X(08).
     03  FILLER       PIC  X(01).
     03  MS01-SHONM1  PIC  X(15).
     03  MS01-SHONM2  PIC  X(15).
     03  FILLER       PIC  X(04).
     03  MS01-MES-SU  PIC  Z(06).
     03  FILLER       PIC  X(03).
     03  MS01-YOT-SU  PIC  Z(06).
     03  FILLER       PIC  X(05).
     03  MS01-TANCD   PIC  X(02).

 01  MEISAI02.
     03  FILLER          PIC  X(13).
     03  MS02-TORNM      PIC  N(15)
                         CHARACTER TYPE IS PIT-1V5.
     03  FILLER          PIC  N(01)
                         CHARACTER TYPE IS PIT-1V5.
     03  FILLER          PIC  X(05).
     03  MS02-AIT-SHOCD  PIC  X(13).

 01  FILE-ERR.
     03  ERR-ERR           PIC  N(20)  VALUE
         NC"チェックリスト中間ワークエラー".
     03  PRT-ERR           PIC  N(20)  VALUE
         NC"プリントファイルエラー".
*
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
* 入力パラメータ

* 出力パラメータ
*
**************************************************************
 PROCEDURE             DIVISION.
**************************************************************
 DECLARATIVES.
 ERR-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE NARERRF.
     DISPLAY     ERR-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 PRJ-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE PRTFILE.
     DISPLAY     PRT-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     PRT-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 END  DECLARATIVES.
****************************************************************
*             MAIN        MODULE                     0.0       *
****************************************************************
 PROCESS-START         SECTION.
     MOVE  "PROCESS START"       TO  S-NAME.

     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC  UNTIL END-FLG = "END".
     PERFORM  END-SEC.

     STOP RUN.
 CONTROL-EXIT.
     EXIT.
****************************************************************
*             初期処理                               1.0
****************************************************************
 INIT-SEC              SECTION.
     MOVE  S-NAME           TO  S-NAME-SV.
     MOVE  "INIT-SEC"       TO  S-NAME.

     DISPLAY   "**  START SNA0240L  **"  UPON CONS.

     PERFORM  SDATE-GET-SEC.
*ファイルのＯＰＥＮ
     OPEN  INPUT  NARERRF.
     OPEN  OUTPUT PRTFILE.
*ワークの初期化
     INITIALIZE  WRK-AREA.
     INITIALIZE  FLG-AREA.
     MOVE  65               TO  CT-LINE.

     INITIALIZE  ERR-REC.
     MOVE  LOW-VALUE        TO  FG-NARERRF-END.
     PERFORM  RD-NARERRF-SEC.
     IF  FG-NARERRF-END = "END"
         MOVE  "END"        TO  END-FLG
     END-IF.

 INIT-090.
     MOVE  S-NAME-SV        TO  S-NAME.
 INIT-EXIT.
     EXIT.
****************************************************************
*    システム日付取得                                          *
****************************************************************
 SDATE-GET-SEC              SECTION.
     MOVE  S-NAME           TO  S-NAME-SV.
     MOVE  "SDATE-GET-SEC"  TO  S-NAME.
*システム日付・時刻の取得
     ACCEPT  WK-DATE   FROM DATE.
     MOVE  "3"              TO  LINK-IN-KBN.
     MOVE  WK-DATE          TO  LINK-IN-YMD6.
     MOVE  ZERO             TO  LINK-IN-YMD8.
     MOVE  ZERO             TO  LINK-OUT-RET.
     MOVE  ZERO             TO  LINK-OUT-YMD.
     CALL  "SKYDTCKB"  USING LINK-IN-KBN
                             LINK-IN-YMD6
                             LINK-IN-YMD8
                             LINK-OUT-RET
                             LINK-OUT-YMD.
     MOVE  LINK-OUT-YMD     TO  SYS-DATE.
     ACCEPT  WK-TIME   FROM TIME.

 SDATE-GET-090.
     MOVE  S-NAME-SV        TO  S-NAME.
 SDATE-GET-EXIT.
     EXIT.
****************************************************************
*    チェックリスト中間ワーク読込み　                          *
****************************************************************
 RD-NARERRF-SEC        SECTION.
     MOVE  S-NAME           TO  S-NAME-SV.
     MOVE  "RD-NARERRF-SEC" TO  S-NAME.

     IF  FG-NARERRF-END = LOW-VALUE
         START  NARERRF  KEY >= ERR-F01 *> 担当者コード
                                ERR-F03 *> 受注種別
                                ERR-F04 *> 取引先コード
                                ERR-F06 *> 納品日
                                ERR-F07 *> 自社商品コード
                                ERR-F08 *> 品単コード
           INVALID KEY
              MOVE  "END"   TO  FG-NARERRF-END
              GO TO  RD-NARERRF-090
         END-START

         MOVE  SPACE        TO  FG-NARERRF-END

     END-IF.

     READ  NARERRF
       AT  END
         MOVE  "END"        TO  FG-NARERRF-END
         GO TO  RD-NARERRF-090
     END-READ.

     ADD 1   TO  CT-IN.

 RD-NARERRF-090.
     MOVE  S-NAME-SV        TO  S-NAME.
 RD-NARERRF-EXIT.
     EXIT.

****************************************************************
*             メイン処理                             2.0       *
****************************************************************
 MAIN-SEC              SECTION.
     MOVE  S-NAME           TO  S-NAME-SV.
     MOVE  "MAIN-SEC"       TO  S-NAME.

     MOVE  ERR-F03          TO  BRK-JSYUBT.
     MOVE  ERR-F04          TO  BRK-TORCD.
     MOVE  ERR-F06          TO  BRK-NOHNBI.

     PERFORM  UNTIL FG-NARERRF-END = "END"
                 OR ERR-F03    NOT = BRK-JSYUBT
                 OR ERR-F04    NOT = BRK-TORCD
                 OR ERR-F06    NOT = BRK-NOHNBI
       PERFORM  MAINB-SEC

     END-PERFORM.

     PERFORM  YKSEN-PRT-SEC. *> 横線を引く

     IF  FG-NARERRF-END = "END"
         MOVE  "END"        TO  END-FLG
     END-IF.
 MAIN-090.
     MOVE  S-NAME-SV        TO  S-NAME.
 MAIN-EXIT.
     EXIT.
****************************************************************
*  メインＢ処理                                                *
****************************************************************
 MAINB-SEC                SECTION.
     MOVE  S-NAME           TO  S-NAME-SV.
     MOVE  "MAINB-SEC"      TO  S-NAME.

     PERFORM  PRT-SEC.
     PERFORM  RD-NARERRF-SEC. *> 次レコード読込み

 MAINB-090.
     MOVE  S-NAME-SV        TO  S-NAME.
 MAINB-EXIT.
     EXIT.
****************************************************************
*  印刷処理                                                   *
****************************************************************
 PRT-SEC                SECTION.
     MOVE  S-NAME           TO  S-NAME-SV.
     MOVE  "PRT-SEC"        TO  S-NAME.

     MOVE  2                TO  WK-JIKKO-LINE.
     PERFORM  HD-PRT-SEC.

     MOVE  SPACE            TO  MEISAI01.
     MOVE  SPACE            TO  MEISAI02.

     IF     FG-KYOSEI-PRT = 1
         OR ERR-F03 NOT = DOJ-JSYUBT
         OR ERR-F04 NOT = DOJ-TORCD
         OR ERR-F06 NOT = DOJ-NOHNBI *> 同上編集

         MOVE  ZERO         TO  FG-KYOSEI-PRT
         MOVE  ERR-F03      TO  DOJ-JSYUBT
         MOVE  ERR-F04      TO  DOJ-TORCD
         MOVE  ERR-F06      TO  DOJ-NOHNBI

         IF  ERR-F03 = "1"
             MOVE  NC"手　書　き"  TO  MS01-JSYUBT
         ELSE
             MOVE  NC"オンライン"  TO  MS01-JSYUBT
         END-IF

         MOVE  ERR-F04      TO  MS01-TORCD
    *> 納品日
         MOVE  ERR-F06(1:4) TO  MS01-NOHNBI-Y
         MOVE  NC"年"       TO  MS01-NOHNBI-YN
         MOVE  ERR-F06(5:2) TO  MS01-NOHNBI-M
         MOVE  NC"月"       TO  MS01-NOHNBI-MN
         MOVE  ERR-F06(7:2) TO  MS01-NOHNBI-D
         MOVE  NC"日"       TO  MS01-NOHNBI-DN

         MOVE  ERR-F05      TO  MS02-TORNM

     END-IF.

     MOVE  ERR-F07          TO  MS01-SHOCD.
     MOVE  ERR-F08          TO  MS01-HINTAN.
     MOVE  ERR-F101         TO  MS01-SHONM1.
     MOVE  ERR-F102         TO  MS01-SHONM2.
     MOVE  ERR-F11          TO  MS01-MES-SU.
     MOVE  ERR-F12          TO  MS01-YOT-SU.
     MOVE  ERR-F01          TO  MS01-TANCD.

     MOVE  ERR-F09          TO  MS02-AIT-SHOCD.

     WRITE  PRT-REC  FROM  MEISAI01  AFTER 1.
     WRITE  PRT-REC  FROM  MEISAI02  AFTER 1.
     ADD 2  TO CT-LINE.

 PRT-090.
     MOVE  S-NAME-SV        TO  S-NAME.
 PRT-EXIT.
     EXIT.

****************************************************************
*  ヘッダ印刷処理                                              *
****************************************************************
 HD-PRT-SEC               SECTION.
     MOVE  S-NAME           TO  S-NAME-SV.
     MOVE  "HC-PRT-SEC"     TO  S-NAME.

     COMPUTE  WK-CT-LINE = CT-LINE + WK-JIKKO-LINE.

     IF  WK-CT-LINE > 65
         PERFORM  HD-PRTB-SEC
     END-IF.

 HD-PRT-090.
     MOVE  S-NAME-SV        TO  S-NAME.
 HD-PRT-EXIT.
     EXIT.
****************************************************************
*  ヘッダ印刷Ｂ処理                                            *
****************************************************************
 HD-PRTB-SEC              SECTION.
     MOVE  S-NAME           TO  S-NAME-SV.
     MOVE  "HC-PRTB-SEC"    TO  S-NAME.

* 処理日
     MOVE  SYS-DATE (1:4)   TO  HD01-Y.
     MOVE  SYS-DATE (5:2)   TO  HD01-M.
     MOVE  SYS-DATE (7:2)   TO  HD01-D.

* ページ
     ADD  1   TO  CT-PG.
     MOVE  CT-PG            TO  HD01-PG.

     IF  CT-PG > 1
         MOVE  SPACE        TO  PRT-REC
         WRITE  PRT-REC  AFTER PAGE
     END-IF.

     WRITE  PRT-REC  FROM HEAD01  AFTER 0.
     WRITE  PRT-REC  FROM HEAD02  AFTER 1.
     WRITE  PRT-REC  FROM HEAD03  AFTER 3.
     WRITE  PRT-REC  FROM HEAD04  AFTER 1.
     MOVE  SPACE            TO  PRT-REC.
     WRITE  PRT-REC  AFTER 1.

     MOVE  7                TO  CT-LINE.
     MOVE  1                TO  FG-KYOSEI-PRT.

 HD-PRTB-090.
     MOVE  S-NAME-SV        TO  S-NAME.
 HD-PRTB-EXIT.
     EXIT.
****************************************************************
*  横線印刷処理                                                *
****************************************************************
 YKSEN-PRT-SEC         SECTION.
     MOVE  S-NAME           TO  S-NAME-SV.
     MOVE  "YKSEN-PRT-SEC"  TO  S-NAME.

* 横線を引く
     MOVE  2                TO  WK-JIKKO-LINE.
     PERFORM  HD-PRT-SEC.
     WRITE  PRT-REC  FROM YKSEN  AFTER 1.

     MOVE  SPACE            TO  PRT-REC.
     WRITE  PRT-REC  AFTER 1.

     ADD  2   TO  CT-LINE.
 YKSEN-PRT-090.
     MOVE  S-NAME-SV        TO  S-NAME.
 YKSEN-PRT-EXIT.
     EXIT.
****************************************************************
*             終了処理                               3.0       *
****************************************************************
 END-SEC               SECTION.
     MOVE  S-NAME           TO  S-NAME-SV.
     MOVE  "END-SEC"        TO  S-NAME.

     DISPLAY  "SNA0240L NARERRF IN =" CT-IN  UPON CONS.
     DISPLAY  "SNA0240L PRTF  PAGE =" CT-PG  UPON CONS.

*ファイル ＣＬＯＳＥ
     CLOSE  NARERRF.
     CLOSE  PRTFILE.
     DISPLAY   "**  END   SNA0240L  **"  UPON CONS.

 END-090.
     MOVE  S-NAME-SV        TO  S-NAME.
 END-EXIT.
     EXIT.
*****************<<  SNA0240L   END PROGRAM  >>******************
