# SNA0221B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/SNA0221B.COB`

## ソースコード

```cobol
***********************************************************
*
*    顧客名　　　　　：（株）サカタのタネ殿
*    サブシステム　　：ＨＧ基幹システム苗業務連携
*    業務名　　　　　：
*    モジュール名　　：取消依頼状態更新（ＨＵＬＦＴ非連携）
*    作成日／更新日　：2018/12/18
*    作成者／更新者　：高橋_/NAV
*    処理概要　　　　：
*      苗業務システムへ取消依頼を行った記録リスト。
*      連携Ｎｏ取消し依頼ジョブ内で実行される。。
*
***********************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SNA0221B.
 AUTHOR.               NAV.
 DATE-WRITTEN.         2018/12/18.
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
*連携Ｎｏ管理テーブル
     SELECT  NARKANF
       ASSIGN    TO    NARKANL1
       ORGANIZATION    INDEXED
       ACCESS    MODE  RANDOM
       RECORD    KEY
         KAN-F01 *> 連携Ｎｏ
       FILE STATUS     KAN-ST.

*担当者マスタ
     SELECT  HTANMS
       ASSIGN    TO    TANMS1
       ORGANIZATION    INDEXED
       ACCESS    MODE  RANDOM
       RECORD    KEY
         TAN-F01 *> 部門ＣＤ
         TAN-F02 *> 担当者ＣＤ
       FILE STATUS     TAN-ST.

*小売連携結果リスト
     SELECT  PRTFILE
       ASSIGN    TO  LP-04
       FILE  STATUS  PRT-ST.
****************************************************************
 DATA                DIVISION.
****************************************************************
 FILE                SECTION.
****************************************************************
*    FILE = 連携Ｎｏ管理テーブル                               *
****************************************************************
 FD  NARKANF
     LABEL     RECORD    IS   STANDARD.
     COPY      NARKANF   OF   XFDLIB
     JOINING   KAN       AS   PREFIX.
****************************************************************
*    FILE = 担当者マスタ                                       *
****************************************************************
 FD  HTANMS
     LABEL     RECORD    IS   STANDARD.
     COPY      HTANMS    OF   XFDLIB
     JOINING   TAN       AS   PREFIX.
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
     03  KAN-ST             PIC  X(02).
     03  TAN-ST             PIC  X(02).
     03  PRT-ST             PIC  X(02).
*フラグ領域
 01  FLG-AREA.
     03  END-FLG            PIC  X(03)  VALUE  SPACE.
     03  FG-NARKANF-INV     PIC  9(01).
     03  FG-HTANMS-INV      PIC  9(01).
*ワーク領域
 01  WRK-AREA.
     03  CT-IN              PIC  9(08).
     03  CT-OT              PIC  9(08).
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
     03  WK-MATUBI          PIC  9(02).

     03  IX                 PIC  9(04).
     03  IX2                PIC  9(04).

     03  WK-REN-NO-X        PIC  X(09).
     03  WK-REN-NO REDEFINES WK-REN-NO-X
                            PIC  9(09).
 01  TB-CVT-SU.
     03  TB-CVT-SU          PIC  X(11)  VALUE
         "0123456789 ".
     03  TB-CVT-SUR  REDEFINES TB-CVT-SU
                            PIC  X(01)  OCCURS 11.

     03  TB-CVT-SU-N        PIC  N(11)  VALUE
       NC"０１２３４５６７８９　".
     03  TB-CVT-SU-NR  REDEFINES TB-CVT-SU-N
                            PIC  N(01)  OCCURS 11.

 01  WK-RENNO-G.
     03  WK-RENNO           PIC  X(09).
     03  WK-RENNOR-9  REDEFINES  WK-RENNO
                            PIC  9(09).
     03  WK-RENNOR    REDEFINES  WK-RENNO
                            PIC  X(01)  OCCURS 9.

     03  WK-RENNO-N         PIC  N(09).
     03  WK-RENNO-NR  REDEFINES  WK-RENNO-N
                            PIC  N(01)  OCCURS 9.

 01  WK-DTKENSU-G.
     03  WK-DTKENSU         PIC  X(06).
     03  WK-DTKENSUR-9  REDEFINES  WK-DTKENSU
                            PIC  9(06).
     03  WK-DTKENSUR-Z  REDEFINES  WK-DTKENSU
                            PIC  ZZZZZ9.
     03  WK-DTKENSUR    REDEFINES  WK-DTKENSU
                            PIC  X(01)  OCCURS 6.

     03  WK-DTKENSU-N       PIC  N(06).
     03  WK-DTKENSU-NR  REDEFINES  WK-DTKENSU-N
                            PIC  N(01)  OCCURS 6.

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

 01  MSG-WAKU               PIC  N(25)  VALUE
     NC"＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
 01  MSG01.
     03  MSG011             PIC  N(25)  VALUE
     NC"＊　以下の連携Ｎｏは管理テーブルに存在しません　＊".
     03  MSG012.
       05  FILLER           PIC  X(22)  VALUE
       "＊　　　　　　　　　".
       05  MSG012-RENNO     PIC  X(09) VALUE SPACE.
       05  FILLER           PIC  X(01) VALUE SPACE.
       05  FILLER           PIC  X(22)  VALUE
       "　　　　　　　　　＊".

 01  HEAD01.
     03  FILLER  PIC  X(08)  VALUE "SNA0221B".
     03  FILLER  PIC  X(27)  VALUE SPACE.
     03  FILLER  PIC  N(11)  VALUE
     NC"＜小売連携結果リスト＞"
                      CHARACTER TYPE IS PIT-2-2W2H.
     03  FILLER  PIC  X(34)  VALUE SPACE.
     03  HD01-Y  PIC  9(04).
     03  FILLER  PIC  N(01)  VALUE NC"年"
                      CHARACTER TYPE IS PIT-2.
     03  HD01-M  PIC  Z9.
     03  FILLER  PIC  N(01)  VALUE NC"月"
                      CHARACTER TYPE IS PIT-2.
     03  HD01-D  PIC  Z9.
     03  FILLER  PIC  N(01)  VALUE NC"日"
                      CHARACTER TYPE IS PIT-2.
     03  FILLER  PIC  X(02)  VALUE SPACE.
     03  HD01-PG PIC  ZZ9.
     03  FILLER  PIC  N(01)  VALUE NC"頁"
                      CHARACTER TYPE IS PIT-2.

 01  HEAD02.
     03  FILLER  PIC  X(115)  VALUE SPACE.
     03  HD02-HH PIC  9(02).
     03  FILLER  PIC  N(01)  VALUE NC"："
                      CHARACTER TYPE IS PIT-2.
     03  HD02-MM PIC  Z9.
     03  FILLER  PIC  N(01)  VALUE NC"："
                      CHARACTER TYPE IS PIT-2.
     03  HD02-DD PIC  Z9.

 01  YKSEN.
     03  FILLER  PIC  X(135)  VALUE ALL "-".

 01  MEISAI01  CHARACTER TYPE IS PIT-2-2W2H.
     03  FILLER  PIC  X(04)  VALUE SPACE.
     03  FILLER  PIC  N(04)  VALUE NC"連携_　".
     03  MS01-RENNO   PIC  N(09).
     03  FILLER  PIC  N(03)  VALUE NC"　の（".
     03  FILLER  PIC  N(06)  VALUE NC"取り消し依頼".
     03  FILLER  PIC  N(09)  VALUE NC"）を　送信しました".

 01  MEISAI02.
     03  FILLER  PIC  X(40)  VALUE SPACE.
     03  FILLER  PIC  N(06)  VALUE NC"発注種別　："
                      CHARACTER TYPE IS PIT-1V5-2W.
     03  FILLER  PIC  X(01)  VALUE SPACE.
     03  MS02-HC-SYUBT  PIC  N(05)
                      CHARACTER TYPE IS PIT-1V5-2W.

 01  MEISAI03.
     03  FILLER  PIC  X(40)  VALUE SPACE.
     03  FILLER  PIC  N(06)  VALUE NC"送信件数　："
                      CHARACTER TYPE IS PIT-1V5-2W.
     03  FILLER  PIC  X(01)  VALUE SPACE.
     03  MS03-SND-KENSU PIC  N(06)
                      CHARACTER TYPE IS PIT-1V5-2W.
     03  FILLER  PIC  N(01)  VALUE NC"件"
                      CHARACTER TYPE IS PIT-1V5-2W.

 01  MEISAI04.
     03  FILLER          PIC  X(40)  VALUE SPACE.
     03  FILLER          PIC  N(06)  VALUE NC"送信担当者："
                              CHARACTER TYPE IS PIT-1V5-2W.
     03  FILLER          PIC  X(01)  VALUE SPACE.
     03  MS04-SND-BUMN   PIC  X(04).
     03  FILLER          PIC  X(01)  VALUE "-".
     03  MS04-SND-TANT   PIC  X(02).
     03  FILLER          PIC  X(01)  VALUE SPACE.
     03  MS04-SND-TANTNM PIC  N(10)
                              CHARACTER TYPE IS PIT-1V5-2W.

 01  MEISAI05.
     03  FILLER          PIC  X(19)  VALUE SPACE.
     03  FILLER          PIC  N(20)  VALUE
     NC"取消データを苗業務システムの『ＨＧ受注情"
                              CHARACTER TYPE IS PIT-1V5-2W.
     03  FILLER          PIC  N(12)  VALUE
     NC"報取込』にて取込を実施"
                              CHARACTER TYPE IS PIT-1V5-2W.

 01  MEISAI06.
     03  FILLER          PIC  X(19)  VALUE SPACE.
     03  FILLER          PIC  N(20)  VALUE
     NC"して下さい。連携照会画面は手動連携の為、"
                              CHARACTER TYPE IS PIT-1V5-2W.
     03  FILLER          PIC  N(12)  VALUE
     NC"即”取消済”になります。"
                              CHARACTER TYPE IS PIT-1V5-2W.

 01  FILE-ERR.
     03  KAN-ERR           PIC  N(20)  VALUE
         NC"連携Ｎｏ管理テーブルエラー".
     03  TAN-ERR           PIC  N(20)  VALUE
         NC"担当者マスタエラー".
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
*  パラメータＢ
 01  PAB-TANCD              PIC  X(06).
 01  PAB-ONLTEG             PIC  X(01).
 01  PAB-RENNO-FROM         PIC  X(09).
 01  PAB-RENNO-TO           PIC  X(09).
 01  PAB-IPADDR             PIC  X(15).

* 出力パラメータ
*
**************************************************************
 PROCEDURE             DIVISION   USING PAB-TANCD
                                        PAB-ONLTEG
                                        PAB-RENNO-FROM
                                        PAB-RENNO-TO
                                        PAB-IPADDR.
**************************************************************
 DECLARATIVES.
 KAN-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE NARKANF.
     DISPLAY     KAN-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     KAN-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 TAN-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HTANMS.
     DISPLAY     TAN-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     TAN-ST    UPON      CONS.
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
     MOVE  "INIT-SEC"       TO  S-NAME.

     DISPLAY   "**  START SNA0221B  **"  UPON CONS.

     PERFORM  SDATE-GET-SEC.
*ファイルのＯＰＥＮ
     OPEN  I-O    NARKANF.
     OPEN  INPUT  HTANMS.
     OPEN  OUTPUT PRTFILE.
*ワークの初期化
     INITIALIZE  WRK-AREA.
     INITIALIZE  FLG-AREA.

 INIT-EXIT.
     EXIT.
****************************************************************
*    システム日付取得                                          *
****************************************************************
 SDATE-GET-SEC              SECTION.
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
 SDATE-GET-EXIT.
     EXIT.
****************************************************************
*             メイン処理                             2.0       *
****************************************************************
 MAIN-SEC              SECTION.
     MOVE  "MAIN-SEC"       TO  S-NAME.

     PERFORM  UPD-NARKANF-SEC.

     PERFORM  PRT-SEC.

     MOVE  "END"            TO  END-FLG.

 MAIN-EXIT.
     EXIT.
****************************************************************
*  更新処理                                                    *
****************************************************************
 UPD-NARKANF-SEC            SECTION.
     MOVE  "UPD-NARKANF-SEC"  TO  S-NAME.

     MOVE   PAB-RENNO-FROM  TO  KAN-F01.

     READ  NARKANF
       INVALID
         MOVE  1            TO  FG-NARKANF-INV
       NOT INVALID
         MOVE  ZERO         TO  FG-NARKANF-INV
     END-READ.

     IF  FG-NARKANF-INV = 1
         DISPLAY  MSG-WAKU  UPON CONS
         DISPLAY  MSG011    UPON CONS
         MOVE  PAB-RENNO-FROM  TO  MSG012-RENNO
         DISPLAY  MSG012    UPON CONS
         DISPLAY  MSG-WAKU  UPON CONS
         MOVE  4001         TO  PROGRAM-STATUS
         EXIT PROGRAM
     END-IF.

     IF  KAN-F04 = "4" *> 更新前状態が取消依頼済の場合そのまま
         CONTINUE
     ELSE
         MOVE  KAN-F04      TO  KAN-F05  *> 前回状態FLG
     END-IF.

     MOVE  "5"              TO  KAN-F04. *> 状態FLG
                                         *> (取消依頼済)
     REWRITE  KAN-REC.

 UPD-NARKANF-EXIT.
     EXIT.
****************************************************************
*  作表処理                                                    *
****************************************************************
 PRT-SEC            SECTION.
     MOVE  "PRT-SEC"        TO  S-NAME.

* 担当者マスタ
*    MOVE  KAN-F02          TO  TAN-F01. *> 部門ＣＤ
     MOVE  PAB-TANCD(1:4)   TO  TAN-F01. *> 部門ＣＤ
*    MOVE  KAN-F03          TO  TAN-F02. *> 担当者ＣＤ
     MOVE  PAB-TANCD(5:2)   TO  TAN-F02. *> 担当者ＣＤ

     READ  HTANMS
       INVALID
         MOVE  1            TO  FG-HTANMS-INV
       NOT INVALID
         MOVE  ZERO         TO  FG-HTANMS-INV
     END-READ.

     MOVE  SYS-DATE (1:4)   TO  HD01-Y.
     MOVE  SYS-DATE (5:2)   TO  HD01-M.
     MOVE  SYS-DATE (7:2)   TO  HD01-D.
     MOVE  1                TO  HD01-PG.

     MOVE  WK-TIME (1:2)    TO  HD02-HH.
     MOVE  WK-TIME (3:2)    TO  HD02-MM.
     MOVE  WK-TIME (5:2)    TO  HD02-DD.

* 連携Ｎ-を日本語変換する。
     MOVE  ALL NC"？"       TO  WK-RENNO-N.
     MOVE  PAB-RENNO-FROM   TO  WK-RENNO.
     PERFORM  VARYING IX  FROM 1 BY 1
              UNTIL   IX > 9
       PERFORM  VARYING  IX2  FROM 1 BY 1
                UNTIL    IX2 > 11

         IF  TB-CVT-SUR (IX2) = WK-RENNOR (IX)
             MOVE  TB-CVT-SU-NR (IX2)  TO WK-RENNO-NR (IX)
             MOVE  11       TO  IX2
         END-IF
       END-PERFORM

     END-PERFORM.

     MOVE  WK-RENNO-N       TO  MS01-RENNO.

     IF  PAB-ONLTEG = "1"
         MOVE  NC"手書"          TO  MS02-HC-SYUBT
     ELSE
         MOVE  NC"オンライン"    TO  MS02-HC-SYUBT
     END-IF.

* データ件数を日本語変換する。
     MOVE  ALL NC"？"       TO  WK-DTKENSU-N.
     MOVE  KAN-F06          TO  WK-DTKENSUR-Z.
     PERFORM  VARYING IX  FROM 1 BY 1
              UNTIL   IX > 6
       PERFORM  VARYING  IX2  FROM 1 BY 1
                UNTIL    IX2 > 11

         IF  TB-CVT-SUR (IX2) = WK-DTKENSUR (IX)
             MOVE  TB-CVT-SU-NR (IX2)  TO WK-DTKENSU-NR (IX)
             MOVE  11       TO  IX2
         END-IF
       END-PERFORM

     END-PERFORM.

     MOVE  WK-DTKENSU-N     TO  MS03-SND-KENSU.

*    MOVE  KAN-F02          TO  MS04-SND-BUMN.
     MOVE  PAB-TANCD(1:4)   TO  MS04-SND-BUMN.
*    MOVE  KAN-F03          TO  MS04-SND-TANT.
     MOVE  PAB-TANCD(5:2)   TO  MS04-SND-TANT.

     IF  FG-HTANMS-INV = ZERO
         MOVE  TAN-F03      TO  MS04-SND-TANTNM
     ELSE
         MOVE  ALL NC"＊"   TO  MS04-SND-TANTNM
     END-IF.

     IF  FG-HTANMS-INV = ZERO
         MOVE  TAN-F03      TO  MS04-SND-TANTNM
     ELSE
         MOVE  ALL NC"＊"   TO  MS04-SND-TANTNM
     END-IF.

     WRITE  PRT-REC  FROM HEAD01    AFTER 3.
     WRITE  PRT-REC  FROM HEAD02    AFTER 1.
     WRITE  PRT-REC  FROM YKSEN     AFTER 1.

     WRITE  PRT-REC  FROM MEISAI01  AFTER 5.
     WRITE  PRT-REC  FROM MEISAI02  AFTER 7.
     WRITE  PRT-REC  FROM MEISAI03  AFTER 3.
     WRITE  PRT-REC  FROM MEISAI04  AFTER 3.
     WRITE  PRT-REC  FROM MEISAI05  AFTER 4.
     WRITE  PRT-REC  FROM MEISAI06  AFTER 4.
     WRITE  PRT-REC  FROM YKSEN     AFTER 2.

 PRT-EXIT.
     EXIT.
****************************************************************
*             終了処理                               3.0       *
****************************************************************
 END-SEC               SECTION.
     MOVE  "END-SEC"        TO  S-NAME.
*ファイル ＣＬＯＳＥ
     CLOSE  NARKANF.
     CLOSE  HTANMS.
     CLOSE  PRTFILE.
     DISPLAY   "**  END   SNA0221B  **"  UPON CONS.

 END-EXIT.
     EXIT.
*****************<<  SNA0221B   END PROGRAM  >>******************

```
