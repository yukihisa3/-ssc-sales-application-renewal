***********************************************************
*
*    顧客名　　　　　：（株）サカタのタネ殿
*    サブシステム　　：ＨＧ基幹システム
*    業務名　　　　　：
*    モジュール名　　：エンチョー物品受領書発行
*    作成日／更新日　：2012/02/14
*    作成者／更新者　：飯田/NAV
*    処理概要　　　　：
*      エンチョー受領情報データより、物品受領書を発行する。
*
***********************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SSY2711L.
 AUTHOR.               S.I.
 DATE-WRITTEN.         2012/02/14.
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

*---<<  エンチョー受領情報データ  >>---*
     SELECT  ECJYURF
         ASSIGN        TO  ECJYURL2
         ORGANIZATION  IS  INDEXED
         ACCESS MODE   IS  SEQUENTIAL
         RECORD KEY    IS  ECJ-F024 *> 受入店ＣＤ
                           ECJ-F027 *> 納品日
                           ECJ-F026 *> 仕入伝票Ｎｏ
                           ECJ-F039 *> 仕入伝票行Ｎｏ
         FILE STATUS   IS  ECJ-STS.

*---<<  店舗マスタ  >>---*
     SELECT  HTENMS
         ASSIGN        TO  TENMS1
         ORGANIZATION  IS  INDEXED
         ACCESS MODE   IS  RANDOM
         RECORD KEY    IS  TEN-F52  *> 相手取引先ＣＤ
                           TEN-F011 *> 店舗コード
         FILE STATUS   IS  TEN-STS.

*---<<  商品変換テーブル  >>---*
     SELECT  HSHOTBL
         ASSIGN        TO  SHOTBL1
         ORGANIZATION  IS  INDEXED
         ACCESS MODE   IS  RANDOM
         RECORD KEY    IS  SHT-F01  *> 相手取引先ＣＤ
                           SHT-F02  *> 相手商品ＣＤ
         FILE STATUS   IS  SHT-STS.

*---<<  商品名称マスタ  >>---*
     SELECT  HMEIMS
       ASSIGN          TO  MEIMS1
       ORGANIZATION    IS  INDEXED
       ACCESS MODE     IS  RANDOM
       RECORD KEY      IS  MEI-F011  *> 商品コード
                           MEI-F0121 *> 単１
                           MEI-F0122 *> 単２
                           MEI-F0123 *> 単３
       FILE STATUS     IS  MEI-STS.

*---<<  プリントファイル  >>---*
     SELECT  PRTFILE
         ASSIGN        TO  LP-04
         FILE  STATUS  IS  PRT-STS.
****************************************************************
 DATA                DIVISION.
****************************************************************
 FILE                SECTION.
****************************************************************
*    FILE = エンチョー受領情報データ                           *
****************************************************************
 FD  ECJYURF.
     COPY  ECJYURF OF XFDLIB   JOINING  ECJ  AS PREFIX.

****************************************************************
*    FILE = 店舗マスタ                                         *
****************************************************************
 FD  HTENMS.
     COPY  HTENMS OF XFDLIB   JOINING  TEN  AS PREFIX.

****************************************************************
*    FILE = 商品変換ＴＢＬ                                     *
****************************************************************
 FD  HSHOTBL.
     COPY  HSHOTBL OF XFDLIB   JOINING  SHT  AS PREFIX.

****************************************************************
*    FILE = 商品名称マスタ                                     *
****************************************************************
 FD  HMEIMS.
     COPY  HMEIMS OF XFDLIB   JOINING  MEI  AS PREFIX.

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
     03  ECJ-STS            PIC  X(02).
     03  TEN-STS            PIC  X(02).
     03  SHT-STS            PIC  X(02).
     03  MEI-STS            PIC  X(02).
     03  PRT-STS            PIC  X(02).
*フラグ領域
 01  FLG-AREA.
     03  END-FLG            PIC  X(03)  VALUE  SPACE.
     03  FG-ECJYURF-END     PIC  X(03).
     03  FG-HTENMS-INV      PIC  9(01).
     03  FG-HSHOTBL-INV     PIC  9(01).
     03  FG-HMEIMS-INV      PIC  9(01).
     03  FG-HD-PRT          PIC  9(01).
     03  FG-HD-PRT-SPACE-DEL PIC  9(01).
     03  FG-HDPRT-CHK       PIC  9(01).
     03  FG-JIKO-ARI        PIC  9(01).
*ワーク領域
 01  WRK-AREA.
     03  S-NAME-SV          PIC  X(30).
     03  CT-PG              PIC  9(08).
     03  CT-IN              PIC  9(08).
     03  CT-LINE            PIC  9(03).
     03  WK-JIKKO-LINE      PIC  9(03).
     03  WK-JIKKO-LINE-SV   PIC  9(03).
     03  IX                 PIC  9(04).
     03  IX2                PIC  9(04).
     03  IX-GYO             PIC  9(03).
     03  IX-GYOMAX          PIC  9(03).
     03  IX-GYO-JIKO        PIC  9(03).
     03  WK-JIKO-SURY-G     PIC S9(06).
     03  WK-GKINGK-G        PIC S9(11).
     03  WK-GKINGK-GW       PIC S9(11).

 01  BRK-KEY.
     03  BRK-TENCD          PIC  9(05).
     03  BRK-NOHNBI         PIC  9(08).
     03  BRK-DENNO          PIC  9(10).

* データ格納領域
 01  SV-DATA.
     03 SV-G  OCCURS 19. *> 行数（受領 1行、納品事故 18行）
        05  SV-ECJ-REC      PIC  X(300).

     COPY  ECJYURF OF XFDLIB  JOINING ECJW  AS PREFIX.

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

 01  HEAD01.
     03  FILLER  PIC  X(01)  VALUE SPACE.
     03  FILLER  PIC  X(08)  VALUE "SSY2711L".
     03  FILLER  PIC  X(100) VALUE SPACE.
     03  FILLER  PIC  X(05)  VALUE "DATE:".
     03  HD01-Y  PIC  9(04).
     03  FILLER  PIC  X(01)  VALUE ".".
     03  HD01-M  PIC  9(02).
     03  FILLER  PIC  X(01)  VALUE ".".
     03  HD01-D  PIC  9(02).
     03  FILLER  PIC  X(02)  VALUE SPACE.
     03  FILLER  PIC  X(05)  VALUE "PAGE:".
     03  HD01-PG PIC  ZZZ9.

 01  HEAD02.
     03  FILLER  PIC  X(36)  VALUE SPACE.
     03  FILLER  PIC  N(13)  VALUE
     NC"＜エンチョー　物品受領書＞"
                      CHARACTER TYPE IS PIT-2-2W2H.

 01  MEISAI01.
     03  FILLER  PIC  X(01)  VALUE SPACE.
     03  FILLER  PIC  N(04)  VALUE NC"店名：　"
                      CHARACTER TYPE IS PIT-1V5.
     03  MS01-TENCD   PIC  9(05).
     03  FILLER  PIC  X(01)  VALUE SPACE.
     03  MS01-TENNM   PIC  N(15)
                      CHARACTER TYPE IS PIT-1V5.
     03  FILLER  PIC  N(01)  VALUE SPACE
                      CHARACTER TYPE IS PIT-1V5.
     03  FILLER  PIC  N(04)  VALUE NC"伝票番号"
                      CHARACTER TYPE IS PIT-1V5.
     03  FILLER  PIC  N(01)  VALUE NC"："
                      CHARACTER TYPE IS PIT-2.
     03  MS01-DENNNO  PIC  9(10).
     03  FILLER  PIC  X(02)  VALUE SPACE.
     03  FILLER  PIC  N(04)  VALUE NC"納品日："
                      CHARACTER TYPE IS PIT-1V5.
     03  MS01-NOHIN-Y PIC  9(04).
     03  FILLER  PIC  N(01)  VALUE NC"年"
                      CHARACTER TYPE IS PIT-2.
     03  MS01-NOHIN-M PIC  Z9.
     03  FILLER  PIC  N(01)  VALUE NC"月"
                      CHARACTER TYPE IS PIT-2.
     03  MS01-NOHIN-D PIC  Z9.
     03  FILLER  PIC  N(01)  VALUE NC"日"
                      CHARACTER TYPE IS PIT-2.
     03  FILLER  PIC  X(02)  VALUE SPACE.
     03  FILLER  PIC  N(06)  VALUE NC"支払対象締日"
                      CHARACTER TYPE IS PIT-1V5.
     03  FILLER  PIC  N(01)  VALUE NC"："
                      CHARACTER TYPE IS PIT-2.
     03  MS01-SHHSM-Y PIC  9(04).
     03  FILLER  PIC  N(01)  VALUE NC"年"
                      CHARACTER TYPE IS PIT-2.
     03  MS01-SHHSM-M PIC  Z9.
     03  FILLER  PIC  N(01)  VALUE NC"月"
                      CHARACTER TYPE IS PIT-2.
     03  MS01-SHHSM-D PIC  Z9.
     03  FILLER  PIC  N(01)  VALUE NC"日"
                      CHARACTER TYPE IS PIT-2.
     03  FILLER  PIC  X(02)  VALUE SPACE.
     03  FILLER  PIC  N(06)  VALUE NC"原価金額合計"
                      CHARACTER TYPE IS PIT-1V5.
     03  FILLER  PIC  N(01)  VALUE NC"："
                      CHARACTER TYPE IS PIT-2.
     03  MS01-GKINGK-KEI PIC  --,---,---,---,--9.

 01  MEISAI02.
     03  FILLER  PIC  X(01)  VALUE SPACE.
     03  FILLER  PIC  N(04)  VALUE NC"取引先："
                      CHARACTER TYPE IS PIT-1V5.
     03  MS02-TORCD   PIC  9(08).
     03  FILLER  PIC  X(01)  VALUE SPACE.
     03  MS02-TORNM   PIC  X(20).
     03  FILLER  PIC  X(01)  VALUE SPACE.
     03  FILLER  PIC  N(04)  VALUE NC"課税区分"
                      CHARACTER TYPE IS PIT-1V5.
     03  FILLER  PIC  N(01)  VALUE NC"："
                      CHARACTER TYPE IS PIT-2.
     03  MS02-KAZEI-KBN  PIC  X(01).
     03  FILLER  PIC  X(01)  VALUE ":".
     03  MS02-KAZEI-KBNNM  PIC  N(04)
                      CHARACTER TYPE IS PIT-1V5.
     03  FILLER  PIC  X(04)  VALUE SPACE.
     03  FILLER  PIC  N(04)  VALUE NC"計上日："
                      CHARACTER TYPE IS PIT-1V5.
     03  MS02-KEIJY-Y PIC  9(04).
     03  FILLER  PIC  N(01)  VALUE NC"年"
                      CHARACTER TYPE IS PIT-2.
     03  MS02-KEIJY-M PIC  Z9.
     03  FILLER  PIC  N(01)  VALUE NC"月"
                      CHARACTER TYPE IS PIT-2.
     03  MS02-KEIJY-D PIC  Z9.
     03  FILLER  PIC  N(01)  VALUE NC"日"
                      CHARACTER TYPE IS PIT-2.
     03  FILLER  PIC  X(02)  VALUE SPACE.
     03  FILLER  PIC  N(04)  VALUE NC"伝票内訳"
                      CHARACTER TYPE IS PIT-1V5.
     03  FILLER  PIC  N(01)  VALUE NC"："
                      CHARACTER TYPE IS PIT-2.
     03  MS02-DEN-UCHIWK  PIC  X(02).
     03  FILLER  PIC  X(01)  VALUE SPACE.
     03  MS02-DEN-UCHIWK-NM  PIC  N(22)
                      CHARACTER TYPE IS PIT-1V5.

 01  MEISAI03. *> 納入事故情報・見出し
     03  FILLER  PIC  X(06)  VALUE SPACE.
     03  FILLER  PIC  X(01)  VALUE "<".
     03  FILLER  PIC  N(06)  VALUE NC"納品事故情報"
                      CHARACTER TYPE IS PIT-1V5.
     03  FILLER  PIC  X(01)  VALUE ">".

 01  MEISAI04. *> 納入事故情報・見出し２
     03  FILLER  PIC  X(10)  VALUE SPACE.
     03  FILLER  PIC  N(01)  VALUE NC"行"
                      CHARACTER TYPE IS PIT-2.
     03  FILLER  PIC  X(02)  VALUE SPACE.
     03  FILLER  PIC  N(10)  VALUE NC"商品名称・商品コード"
                      CHARACTER TYPE IS PIT-2.
     03  FILLER  PIC  X(13)  VALUE SPACE.
     03  FILLER  PIC  X(07)  VALUE "JAN/EOS".
     03  FILLER  PIC  N(03)  VALUE NC"コード"
                      CHARACTER TYPE IS PIT-2.
     03  FILLER  PIC  X(04)  VALUE SPACE.
     03  FILLER  PIC  N(06)  VALUE NC"納品事故数量"
                      CHARACTER TYPE IS PIT-1V5.
     03  FILLER  PIC  X(05)  VALUE SPACE.
     03  FILLER  PIC  N(04)  VALUE NC"原価単価"
                      CHARACTER TYPE IS PIT-2.
     03  FILLER  PIC  X(09)  VALUE SPACE.
     03  FILLER  PIC  N(04)  VALUE NC"原価金額"
                      CHARACTER TYPE IS PIT-2.
     03  FILLER  PIC  X(06)  VALUE SPACE.
     03  FILLER  PIC  N(04)  VALUE NC"伝票内訳"
                      CHARACTER TYPE IS PIT-2.
 01  MEISAI05. *> 納入事故情報・明細
     03  FILLER       PIC  X(10).
     03  MS05-GYO     PIC  Z9.
     03  FILLER       PIC  X(02).
     03  MS05-SYONM   PIC  X(30).
     03  FILLER       PIC  X(03).
     03  MS05-JANCD   PIC  X(13).
     03  FILLER       PIC  X(04).
     03  MS05-JIKO-SURY   PIC  ---,--9.
     03  FILLER       PIC  X(02).
     03  MS05-GTANKA  PIC  --,---,--9.99.
     03  FILLER       PIC  X(03).
     03  MS05-GKINGK  PIC  --,---,---,--9.
     03  FILLER       PIC  X(03).
     03  MS05-DEN-UCHWK  PIC  X(02).
     03  FILLER       PIC  X(01)  VALUE ":".
     03  MS05-DEN-UCHWK-NM  PIC  N(12)
                      CHARACTER TYPE IS PIT-1V5.
 01  MEISAI06. *> 納入事故情報・合計
     03  FILLER       PIC  X(62)  VALUE SPACE.
     03  FILLER       PIC  N(01)  VALUE NC"計"
                      CHARACTER TYPE IS PIT-2.
     03  MS06-JIKO-SURY-G   PIC  ---,--9.
     03  FILLER       PIC  X(16).
     03  FILLER       PIC  N(01)  VALUE NC"計"
                      CHARACTER TYPE IS PIT-2.
     03  MS06-GKINGK-G  PIC  --,---,---,--9.

 01  FILE-ERR.
     03  ECJ-ERR           PIC  N(20)  VALUE
         NC"エンチョー受領情報データ".
     03  TEN-ERR           PIC  N(20)  VALUE
         NC"店舗マスタエラー".
     03  SHT-ERR           PIC  N(20)  VALUE
         NC"商品変換ＴＢＬエラー".
     03  MEI-ERR           PIC  N(20)  VALUE
         NC"商品名称マスタエラー".
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
 ECJ-ERR                   SECTION.
     USE AFTER  EXCEPTION PROCEDURE  ECJYURF.
     DISPLAY  ECJ-ERR   UPON CONS.
     DISPLAY  SEC-NAME  UPON CONS.
     DISPLAY  ECJ-STS   UPON CONS.
     MOVE  "4000"           TO  PROGRAM-STATUS.
     STOP RUN.
 TEN-ERR                   SECTION.
     USE AFTER  EXCEPTION PROCEDURE  HTENMS.
     DISPLAY  TEN-ERR   UPON CONS.
     DISPLAY  SEC-NAME  UPON CONS.
     DISPLAY  TEN-STS   UPON CONS.
     MOVE  "4000"           TO  PROGRAM-STATUS.
     STOP RUN.
 SHT-ERR                   SECTION.
     USE AFTER  EXCEPTION PROCEDURE  HSHOTBL.
     DISPLAY  SHT-ERR   UPON CONS.
     DISPLAY  SEC-NAME  UPON CONS.
     DISPLAY  SHT-STS   UPON CONS.
     MOVE  "4000"           TO  PROGRAM-STATUS.
     STOP RUN.
 MEI-ERR                   SECTION.
     USE AFTER  EXCEPTION PROCEDURE  HMEIMS.
     DISPLAY  MEI-ERR   UPON CONS.
     DISPLAY  SEC-NAME  UPON CONS.
     DISPLAY  MEI-STS   UPON CONS.
     MOVE  "4000"           TO  PROGRAM-STATUS.
     STOP RUN.
 PRJ-ERR                   SECTION.
     USE AFTER  EXCEPTION PROCEDURE  PRTFILE.
     DISPLAY  PRT-ERR   UPON CONS.
     DISPLAY  SEC-NAME  UPON CONS.
     DISPLAY  PRT-STS   UPON CONS.
     MOVE  "4000"           TO  PROGRAM-STATUS.
     STOP RUN.
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

     DISPLAY  "**  START SSY2711L  **"  UPON CONS.

     PERFORM  SDATE-GET-SEC.
*ファイルのＯＰＥＮ
     OPEN  INPUT  ECJYURF.
     OPEN  INPUT  HTENMS.
     OPEN  INPUT  HSHOTBL.
     OPEN  INPUT  HMEIMS.
     OPEN  OUTPUT PRTFILE.
*ワークの初期化
     INITIALIZE  WRK-AREA.
     INITIALIZE  FLG-AREA.
     MOVE  66               TO  CT-LINE.

     MOVE  SPACE            TO  FG-ECJYURF-END.
     PERFORM  RD-INF-SEC.
     IF  FG-ECJYURF-END = "END"
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
*    入力ファイル読込み　                                      *
****************************************************************
 RD-INF-SEC        SECTION.
     MOVE  S-NAME           TO  S-NAME-SV.
     MOVE  "RD-INF-SEC" TO  S-NAME.

     READ  ECJYURF
       AT  END
         MOVE  "END"        TO  FG-ECJYURF-END
         GO TO  RD-INF-090
     END-READ.

     ADD 1   TO  CT-IN.

 RD-INF-090.
     MOVE  S-NAME-SV        TO  S-NAME.
 RD-INF-EXIT.
     EXIT.

****************************************************************
*             メイン処理                             2.0       *
****************************************************************
 MAIN-SEC              SECTION.
     MOVE  S-NAME           TO  S-NAME-SV.
     MOVE  "MAIN-SEC"       TO  S-NAME.

     PERFORM  DTSV-SEC.

     PERFORM  PRT-SEC.

     IF  FG-ECJYURF-END = "END"
         MOVE  "END"        TO  END-FLG
     END-IF.

 MAIN-090.
     MOVE  S-NAME-SV        TO  S-NAME.
 MAIN-EXIT.
     EXIT.
****************************************************************
*  データ退避処理                                              *
****************************************************************
 DTSV-SEC              SECTION.
     MOVE  S-NAME           TO  S-NAME-SV.
     MOVE  "DTSV-SEC"       TO  S-NAME.

     INITIALIZE  SV-DATA.
     MOVE  ZERO             TO  IX-GYO.
     MOVE  ZERO             TO  IX-GYOMAX.
     MOVE  ZERO             TO  IX-GYO-JIKO.
     MOVE  ZERO             TO  FG-JIKO-ARI.
     MOVE  ZERO             TO  WK-JIKO-SURY-G.
     MOVE  ZERO             TO  WK-GKINGK-G.
     MOVE  ECJ-F024         TO  BRK-TENCD.
     MOVE  ECJ-F027         TO  BRK-NOHNBI.
     MOVE  ECJ-F026         TO  BRK-DENNO.

     PERFORM  UNTIL FG-ECJYURF-END = "END"
                OR  ECJ-F024 NOT = BRK-TENCD
                OR  ECJ-F027 NOT = BRK-NOHNBI
                OR  ECJ-F026 NOT = BRK-DENNO
       IF  IX-GYO >= 19
           DISPLAY  "SSY2711L NAIBU TBL OVER ]]]"  UPON CONS
           MOVE  "4001"     TO  PROGRAM-STATUS
           EXIT PROGRAM
       END-IF

       ADD  1  TO  IX-GYO
       MOVE IX-GYO          TO  IX-GYOMAX
       IF  ECJ-F031 NOT = SPACE *> 納入事故・データ区分
           MOVE  1          TO  FG-JIKO-ARI
           ADD  1  TO  IX-GYO-JIKO
           COMPUTE  WK-JIKO-SURY-G =
               WK-JIKO-SURY-G + ECJ-F03B
           COMPUTE  WK-GKINGK-GW = ECJ-F03B * ECJ-F03C
           COMPUTE  WK-GKINGK-G =
               WK-GKINGK-G + WK-GKINGK-GW
       END-IF

       MOVE  ECJ-REC        TO  SV-ECJ-REC (IX-GYO)

       PERFORM  RD-INF-SEC

     END-PERFORM.

 DTSV-090.
     MOVE  S-NAME-SV        TO  S-NAME.
 DTSV-EXIT.
     EXIT.
****************************************************************
*  印刷処理                                                    *
****************************************************************
 PRT-SEC               SECTION.
     MOVE  S-NAME           TO  S-NAME-SV.
     MOVE  "PRT-SEC"        TO  S-NAME.

     PERFORM  VARYING  IX-GYO  FROM 1 BY 1
              UNTIL    IX-GYO > IX-GYOMAX

       PERFORM  PRTB-SEC

     END-PERFORM.

 PRT-090.
     MOVE  S-NAME-SV        TO  S-NAME.
 PRT-EXIT.
     EXIT.
****************************************************************
*  印刷Ｂ処理                                                  *
****************************************************************
 PRTB-SEC               SECTION.
     MOVE  S-NAME           TO  S-NAME-SV.
     MOVE  "PRTB-SEC"       TO  S-NAME.

     MOVE  SV-ECJ-REC (IX-GYO) TO  ECJW-REC.

     IF  FG-JIKO-ARI = ZERO  *> 納品事情報故無し
         PERFORM  PRTC-SEC
     ELSE                    *> 納品事情報故有り
         PERFORM  PRTC2-SEC
     END-IF.

 PRTB-090.
     MOVE  S-NAME-SV        TO  S-NAME.
 PRTB-EXIT.
     EXIT.
****************************************************************
*  印刷Ｃ処理                                                  *
****************************************************************
 PRTC-SEC                   SECTION.
     MOVE  S-NAME           TO  S-NAME-SV.
     MOVE  "PRTC-SEC"       TO  S-NAME.

     IF  IX-GYO = 1
         MOVE  ZERO         TO  WK-JIKKO-LINE
         *> 受領伝票行数（納品事情報故無し）
         COMPUTE  WK-JIKKO-LINE = WK-JIKKO-LINE + 4

         MOVE  ZERO         TO  FG-HD-PRT-SPACE-DEL
         MOVE  1            TO  FG-HDPRT-CHK
         PERFORM  HD-PRT-SEC  *> 改ページチェックのみ
         IF  FG-HD-PRT = ZERO
             CONTINUE
         ELSE
             *> 通常行でチェックし改ページなら
             *> 後ろの空白を除去してで改ページを制御
             MOVE  WK-JIKKO-LINE  TO  WK-JIKKO-LINE-SV
             COMPUTE  WK-JIKKO-LINE = WK-JIKKO-LINE - 2
             MOVE  1        TO  FG-HDPRT-CHK
             PERFORM  HD-PRT-SEC   *> 改ページチェックのみ
             IF  FG-HD-PRT = ZERO
                 MOVE  1    TO  FG-HD-PRT-SPACE-DEL
             ELSE
                 MOVE  WK-JIKKO-LINE-SV  TO  WK-JIKKO-LINE
             END-IF
         END-IF

         PERFORM  HD-PRT-SEC

     END-IF.

     PERFORM  PRT-JYRYDEN-SEC.  *> 受領伝票

     IF  FG-HD-PRT-SPACE-DEL = 1
         *> 空白行除去判定で改ページ無しの場合は
         *> 空白行を空白行出力しない。
         CONTINUE
     ELSE
         *> 通常行判定の場合は空白行出力
         MOVE  SPACE        TO  PRT-REC
         WRITE  PRT-REC  AFTER 2
     END-IF.

 PRTC-090.
     MOVE  S-NAME-SV        TO  S-NAME.
 PRTC-EXIT.
     EXIT.
****************************************************************
*  ヘッダ印刷処理                                              *
****************************************************************
 HD-PRT-SEC               SECTION.
     MOVE  S-NAME           TO  S-NAME-SV.
     MOVE  "HC-PRT-SEC"     TO  S-NAME.

     IF  CT-LINE + WK-JIKKO-LINE > 66  *> 改頁の判定
         MOVE  1            TO  FG-HD-PRT *> 改ページ有り
     ELSE
         MOVE  ZERO         TO  FG-HD-PRT *> 改ページ無し
     END-IF.

     IF  FG-HDPRT-CHK = 1 *> 改頁のチェックのみ
         MOVE  ZERO            TO  FG-HDPRT-CHK
         GO TO  HD-PRT-090
     END-IF.

     IF  FG-HD-PRT = 1 *> 改頁
         PERFORM  HD-PRTB-SEC
     END-IF.

     COMPUTE  CT-LINE = CT-LINE + WK-JIKKO-LINE.

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

     IF  CT-PG NOT = ZERO
         MOVE  SPACE        TO  PRT-REC
         WRITE  PRT-REC  AFTER PAGE
     END-IF.

* 処理日
     MOVE  SYS-DATE (1:4)   TO  HD01-Y.
     MOVE  SYS-DATE (5:2)   TO  HD01-M.
     MOVE  SYS-DATE (7:2)   TO  HD01-D.

* ページ
     ADD  1   TO  CT-PG.
     MOVE  CT-PG            TO  HD01-PG.

     WRITE  PRT-REC  FROM HEAD01  AFTER 0.
     WRITE  PRT-REC  FROM HEAD02  AFTER 1.
     MOVE  SPACE            TO  PRT-REC.
     WRITE  PRT-REC  AFTER 1.

     MOVE  3                TO  CT-LINE.

 HD-PRTB-090.
     MOVE  S-NAME-SV        TO  S-NAME.
 HD-PRTB-EXIT.
     EXIT.
****************************************************************
*  印刷受領伝票処理                                            *
****************************************************************
 PRT-JYRYDEN-SEC           SECTION.
     MOVE  S-NAME           TO  S-NAME-SV.
     MOVE  "PRT-JYRYDEN-SEC"  TO  S-NAME.

     MOVE  ECJW-F014        TO  TEN-F52.  *> 相手取引先ＣＤ
     MOVE  ECJW-F024        TO  TEN-F011. *> 店舗コード
     PERFORM  RD-HTENMS-SEC.

     MOVE  ECJW-F024        TO  MS01-TENCD.

     IF  FG-HMEIMS-INV = ZERO
         MOVE  TEN-F02      TO  MS01-TENNM
     ELSE
         MOVE  SPACE        TO  MS01-TENNM
     END-IF.

     MOVE  ECJW-F026        TO  MS01-DENNNO.
     MOVE  ECJW-F027 (1:4)  TO  MS01-NOHIN-Y.
     MOVE  ECJW-F027 (5:2)  TO  MS01-NOHIN-M.
     MOVE  ECJW-F027 (7:2)  TO  MS01-NOHIN-D.
     MOVE  ECJW-F029 (1:4)  TO  MS01-SHHSM-Y.
     MOVE  ECJW-F029 (5:2)  TO  MS01-SHHSM-M.
     MOVE  ECJW-F029 (7:2)  TO  MS01-SHHSM-D.
     MOVE  ECJW-F02A        TO  MS01-GKINGK-KEI.

     MOVE  ECJW-F014        TO  MS02-TORCD.
     MOVE  ECJW-F019        TO  MS02-TORNM.
     MOVE  ECJW-F02C        TO  MS02-KAZEI-KBN.
     IF  ECJW-F02C = 9 *> 非課税
         MOVE  NC"非課税"   TO  MS02-KAZEI-KBNNM
     ELSE             *> 課税
         MOVE  NC"課税"     TO  MS02-KAZEI-KBNNM
     END-IF.
     MOVE  ECJW-F028 (1:4)  TO  MS02-KEIJY-Y.
     MOVE  ECJW-F028 (5:2)  TO  MS02-KEIJY-M.
     MOVE  ECJW-F028 (7:2)  TO  MS02-KEIJY-D.
     MOVE  ECJW-F02B        TO  MS02-DEN-UCHIWK.

     EVALUATE  TRUE
       WHEN  ECJW-F02B = "10"
         MOVE  NC"ＥＯＳ"          TO  MS02-DEN-UCHIWK-NM

       WHEN  ECJW-F02B = "11"
         MOVE  NC"手書受領"        TO  MS02-DEN-UCHIWK-NM

       WHEN  ECJW-F02B = "12"
         MOVE  NC"ＥＯＳ受領訂正"  TO  MS02-DEN-UCHIWK-NM

       WHEN  ECJW-F02B = "13"
         MOVE  NC"手書受領訂正"    TO  MS02-DEN-UCHIWK-NM

       WHEN  OTHER
         MOVE  SPACE               TO  MS02-DEN-UCHIWK-NM

     END-EVALUATE.

     WRITE  PRT-REC  FROM  MEISAI01  AFTER 1.
     WRITE  PRT-REC  FROM  MEISAI02  AFTER 1.

 PRT-JYRYDEN-090.
     MOVE  S-NAME-SV        TO  S-NAME.
 PRT-JYRYDEN-EXIT.
     EXIT.
****************************************************************
*    店舗マスタ検索                                            *
****************************************************************
 RD-HTENMS-SEC          SECTION.
     MOVE  S-NAME           TO  S-NAME-SV.
     MOVE  "RD-HTENMS-SEC"  TO  S-NAME.

     READ  HTENMS
       INVALID
         MOVE  1            TO  FG-HTENMS-INV
       NOT INVALID
         MOVE  ZERO         TO  FG-HTENMS-INV
     END-READ.

 PRT-090.
     MOVE  S-NAME-SV        TO  S-NAME.
 PRT-EXIT.
     EXIT.

****************************************************************
*  印刷Ｃ２処理                                                *
****************************************************************
 PRTC2-SEC                   SECTION.
     MOVE  S-NAME           TO  S-NAME-SV.
     MOVE  "PRTC2-SEC"      TO  S-NAME.

     IF  IX-GYO = 1
         MOVE  ZERO         TO  WK-JIKKO-LINE

         *> 受領伝票行数（納品事情報故有り）
         COMPUTE  WK-JIKKO-LINE = WK-JIKKO-LINE + 3

         *> 納品事情報行数・見出し
         COMPUTE  WK-JIKKO-LINE = WK-JIKKO-LINE + 3

         *> 納品事情報行数・明細／計
         COMPUTE  WK-JIKKO-LINE =
             WK-JIKKO-LINE + (IX-GYO-JIKO * 3) + 3

         MOVE  ZERO         TO  FG-HD-PRT-SPACE-DEL
         MOVE  1            TO  FG-HDPRT-CHK
         PERFORM  HD-PRT-SEC  *> 改ページチェックのみ
         IF  FG-HD-PRT = ZERO
             CONTINUE
         ELSE
             *> 通常行でチェックし改ページなら
             *> 後ろの空白を除去してで改ページを制御
             MOVE  WK-JIKKO-LINE  TO  WK-JIKKO-LINE-SV
             COMPUTE  WK-JIKKO-LINE = WK-JIKKO-LINE - 2
             MOVE  1        TO  FG-HDPRT-CHK
             PERFORM  HD-PRT-SEC   *> 改ページチェックのみ
             IF  FG-HD-PRT = ZERO
                 MOVE  1    TO  FG-HD-PRT-SPACE-DEL
             ELSE
                 MOVE  WK-JIKKO-LINE-SV  TO  WK-JIKKO-LINE
             END-IF
         END-IF

         PERFORM  HD-PRT-SEC

     END-IF.

     IF ECJW-F031 = SPACE *> 納入事故・データ区分
        PERFORM  PRT-JYRYDEN-SEC  *> 受領伝票
        *> 納入事故・見出し部
        WRITE  PRT-REC  FROM  MEISAI03  AFTER 2
        WRITE  PRT-REC  FROM  MEISAI04  AFTER 1
        MOVE  SPACE            TO  PRT-REC
        WRITE  PRT-REC  AFTER 1
     ELSE
        PERFORM  PRT-NOHNJIK-SEC  *> 納品事故
     END-IF.

 PRTC2-090.
     MOVE  S-NAME-SV        TO  S-NAME.
 PRTC2-EXIT.
     EXIT.
****************************************************************
*  印刷納入事故処理                                            *
****************************************************************
 PRT-NOHNJIK-SEC           SECTION.
     MOVE  S-NAME           TO  S-NAME-SV.
     MOVE  "PRTC-NOHNJIK-SEC"  TO  S-NAME.

     MOVE  ECJW-F014        TO  SHT-F01.  *> 相手取引先ＣＤ
     MOVE  ECJW-F036        TO  SHT-F02.  *> 相手商品ＣＤ
     PERFORM  RD-HSHOTBL-SEC.
     IF  FG-HSHOTBL-INV = ZERO
         MOVE  SHT-F031     TO  MEI-F011  *> 商品コード
         MOVE  SHT-F0321    TO  MEI-F0121 *> 単１
         MOVE  SHT-F0322    TO  MEI-F0122 *> 単２
         MOVE  SHT-F0323    TO  MEI-F0123 *> 単３
         PERFORM  RD-HMEIMS-SEC
     ELSE
         MOVE  1            TO  FG-HMEIMS-INV
     END-IF.

  *> 納入事故・明細部
     MOVE  SPACE            TO  MEISAI05.
     MOVE  ECJW-F039        TO  MS05-GYO.
     IF  FG-HMEIMS-INV = ZERO
         MOVE  MEI-F03      TO  MS05-SYONM
     END-IF.

     MOVE  ECJW-F036        TO  MS05-JANCD.
     MOVE  ECJW-F03B        TO  MS05-JIKO-SURY.
     MOVE  ECJW-F03C        TO  MS05-GTANKA.

     COMPUTE  MS05-GKINGK = ECJW-F03C * ECJW-F03B.

     MOVE  ECJW-F03D        TO  MS05-DEN-UCHWK.

     EVALUATE  TRUE
       WHEN  ECJW-F03D = "12"
         MOVE  NC"ＥＯＳ受領訂正"  TO  MS05-DEN-UCHWK-NM

       WHEN  ECJW-F03D = "13"
         MOVE  NC"手書受領訂正"    TO  MS05-DEN-UCHWK-NM

       WHEN  ECJW-F03D = "30"
         MOVE  NC"納品事故"        TO  MS05-DEN-UCHWK-NM

       WHEN  OTHER
         MOVE  SPACE               TO  MS05-DEN-UCHWK-NM

     END-EVALUATE.

     WRITE  PRT-REC  FROM  MEISAI05  AFTER 1.

     MOVE  SPACE            TO  MEISAI05.
     IF  FG-HSHOTBL-INV = ZERO
         MOVE  SHT-F03      TO  MS05-SYONM *> 商品コード
     END-IF.

     MOVE  ECJW-F037        TO  MS05-JANCD. *> ＥＯＳコード
     WRITE  PRT-REC  FROM  MEISAI05  AFTER 1.

     MOVE  SPACE            TO  PRT-REC.
     WRITE  PRT-REC  AFTER 1.

     IF IX-GYO = IX-GYOMAX *> 納入事故・合計
        MOVE  WK-JIKO-SURY-G  TO  MS06-JIKO-SURY-G
        MOVE  WK-GKINGK-G     TO  MS06-GKINGK-G
        WRITE  PRT-REC  FROM  MEISAI06  AFTER 1

        IF  FG-HD-PRT-SPACE-DEL = 1
            *> 空白行除去判定で改ページ無しの場合は
            *> 空白行を出力しない。
            CONTINUE
        ELSE
            *> 通常行判定の場合は空白行出力
            MOVE  SPACE     TO  PRT-REC
            WRITE  PRT-REC  AFTER 2
        END-IF

     END-IF.

 PRT-NOHNJIK-090.
     MOVE  S-NAME-SV        TO  S-NAME.
 PRT-NOHNJIK-EXIT.
     EXIT.
****************************************************************
*    商品変換ＴＢＬ検索                                        *
****************************************************************
 RD-HSHOTBL-SEC          SECTION.
     MOVE  S-NAME           TO  S-NAME-SV.
     MOVE  "RD-HSHOTBL-SEC"  TO  S-NAME.

     READ  HSHOTBL
       INVALID
         MOVE  1            TO  FG-HSHOTBL-INV
       NOT INVALID
         MOVE  ZERO         TO  FG-HSHOTBL-INV
     END-READ.

 RD-HSHOTBL-090.
     MOVE  S-NAME-SV        TO  S-NAME.
 RD-HSHOTBL-EXIT.
     EXIT.
****************************************************************
*    商品名称マスタ検索                                        *
****************************************************************
 RD-HMEIMS-SEC          SECTION.
     MOVE  S-NAME           TO  S-NAME-SV.
     MOVE  "RD-HMEIMS-SEC"  TO  S-NAME.

     READ  HMEIMS
       INVALID
         MOVE  1            TO  FG-HMEIMS-INV
       NOT INVALID
         MOVE  ZERO         TO  FG-HMEIMS-INV
     END-READ.

 RD-HMEIMS-090.
     MOVE  S-NAME-SV        TO  S-NAME.
 RD-HMEIMS-EXIT.
     EXIT.
****************************************************************
*             終了処理                               3.0       *
****************************************************************
 END-SEC               SECTION.
     MOVE  S-NAME           TO  S-NAME-SV.
     MOVE  "END-SEC"        TO  S-NAME.

*ファイル ＣＬＯＳＥ
     CLOSE  ECJYURF.
     CLOSE  HTENMS.
     CLOSE  HSHOTBL.
     CLOSE  HMEIMS.
     CLOSE  PRTFILE.

     DISPLAY  "SSY2711L ECJYURF IN =" CT-IN  UPON CONS.
     DISPLAY  "SSY2711L PRTF  PAGE =" CT-PG  UPON CONS.
     DISPLAY  "**  END   SSY2711L  **"       UPON CONS.

 END-090.
     MOVE  S-NAME-SV        TO  S-NAME.
 END-EXIT.
     EXIT.
*****************<<  SSY2711L   END PROGRAM  >>******************
