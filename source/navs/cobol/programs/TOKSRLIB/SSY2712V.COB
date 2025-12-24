***********************************************************
*
*    顧客名　　　　　：（株）サカタのタネ殿
*    サブシステム　　：ＨＧ基幹システム
*    業務名　　　　　：
*    モジュール名　　：エンチョー物品受領書ＣＳＶ
*    作成日／更新日　：2023/09/01
*    作成者／更新者　：高橋
*    処理概要　　　　：ＣＳＶ出力対応　　　
*
***********************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SSY2712V.
 AUTHOR.               S.I.
 DATE-WRITTEN.         2012/02/14.
****************************************************************
 ENVIRONMENT           DIVISION.
****************************************************************
 CONFIGURATION         SECTION.
 SPECIAL-NAMES.
     CONSOLE    IS  CONS.
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

*---<<  出力ファイル >>---*
     SELECT  ENCJYRCV
         ASSIGN        TO  ENCJYRCV
         FILE  STATUS  IS  ENC-STS.
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
 FD  ENCJYRCV           BLOCK CONTAINS 1   RECORDS.
*
 01  ENC-REC.
     03  FILLER             PIC  X(400).

****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
*ステータス領域
 01  STATUS-AREA.
     03  ECJ-STS            PIC  X(02).
     03  TEN-STS            PIC  X(02).
     03  SHT-STS            PIC  X(02).
     03  MEI-STS            PIC  X(02).
     03  ENC-STS            PIC  X(02).
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
     03  CT-OUT             PIC  9(08).
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

     COPY  ENCJYRCV  OF XFDLIB  JOINING ECV  AS PREFIX.

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
*
 01  WK-TAITL1.
     03  FILLER  PIC  X(01)  VALUE  X"28".
     03  FILLER  PIC  N(12)
                 VALUE  NC"＜エンチョー受領データ＞".
     03  FILLER  PIC  X(01)  VALUE  X"29".
     03  FILLER  PIC  X(01)  VALUE  ",".
     03  FILLER  PIC  X(01)  VALUE  X"28".
     03  FILLER  PIC  N(05)  VALUE  NC"出力日付：".
     03  FILLER  PIC  X(01)  VALUE  X"29".
     03  FILLER  PIC  X(01)  VALUE  ",".
     03  HD-YMD  PIC  X(10).
 01  WK-TAITL2.
     03  FILLER  PIC  X(01)  VALUE  X"28".
     03  FILLER  PIC  N(06)  VALUE  NC"取引先コード".
     03  FILLER  PIC  X(01)  VALUE  X"29".
     03  FILLER  PIC  X(01)  VALUE  ",".
     03  FILLER  PIC  X(01)  VALUE  X"28".
     03  FILLER  PIC  N(05)  VALUE  NC"店舗コード".
     03  FILLER  PIC  X(01)  VALUE  X"29".
     03  FILLER  PIC  X(01)  VALUE  ",".
     03  FILLER  PIC  X(01)  VALUE  X"28".
     03  FILLER  PIC  N(03)  VALUE  NC"店舗名".
     03  FILLER  PIC  X(01)  VALUE  X"29".
     03  FILLER  PIC  X(01)  VALUE  ",".
     03  FILLER  PIC  X(01)  VALUE  X"28".
     03  FILLER  PIC  N(04)  VALUE  NC"伝票番号".
     03  FILLER  PIC  X(01)  VALUE  X"29".
     03  FILLER  PIC  X(01)  VALUE  ",".
     03  FILLER  PIC  X(01)  VALUE  X"28".
     03  FILLER  PIC  N(03)  VALUE  NC"納品日".
     03  FILLER  PIC  X(01)  VALUE  X"29".
     03  FILLER  PIC  X(01)  VALUE  ",".
     03  FILLER  PIC  X(01)  VALUE  X"28".
     03  FILLER  PIC  N(03)  VALUE  NC"計上日".
     03  FILLER  PIC  X(01)  VALUE  X"29".
     03  FILLER  PIC  X(01)  VALUE  ",".
     03  FILLER  PIC  X(01)  VALUE  X"28".
     03  FILLER  PIC  N(05)  VALUE  NC"支払対象日".
     03  FILLER  PIC  X(01)  VALUE  X"29".
     03  FILLER  PIC  X(01)  VALUE  ",".
     03  FILLER  PIC  X(01)  VALUE  X"28".
     03  FILLER  PIC  N(04)  VALUE  NC"課税区分".
     03  FILLER  PIC  X(01)  VALUE  X"29".
     03  FILLER  PIC  X(01)  VALUE  ",".
     03  FILLER  PIC  X(01)  VALUE  X"28".
     03  FILLER  PIC  N(05)  VALUE  NC"課税区分名".
     03  FILLER  PIC  X(01)  VALUE  X"29".
     03  FILLER  PIC  X(01)  VALUE  ",".
     03  FILLER  PIC  X(01)  VALUE  X"28".
     03  FILLER  PIC  N(04)  VALUE  NC"伝票内訳".
     03  FILLER  PIC  X(01)  VALUE  X"29".
     03  FILLER  PIC  X(01)  VALUE  ",".
     03  FILLER  PIC  X(01)  VALUE  X"28".
     03  FILLER  PIC  N(05)  VALUE  NC"伝票内訳名".
     03  FILLER  PIC  X(01)  VALUE  X"29".
     03  FILLER  PIC  X(01)  VALUE  ",".
     03  FILLER  PIC  X(01)  VALUE  X"28".
     03  FILLER  PIC  N(02)  VALUE  NC"税率".
     03  FILLER  PIC  X(01)  VALUE  X"29".
     03  FILLER  PIC  X(01)  VALUE  ",".
     03  FILLER  PIC  X(01)  VALUE  X"28".
     03  FILLER  PIC  N(06)  VALUE  NC"原価金額合計".
     03  FILLER  PIC  X(01)  VALUE  X"29".
     03  FILLER  PIC  X(01)  VALUE  ",".
     03  FILLER  PIC  X(01)  VALUE  X"28".
     03  FILLER  PIC  N(06)  VALUE  NC"納品事項情報".
     03  FILLER  PIC  X(01)  VALUE  X"29".
     03  FILLER  PIC  X(01)  VALUE  ",".
     03  FILLER  PIC  X(01)  VALUE  X"28".
     03  FILLER  PIC  N(04)  VALUE  NC"明細行数".
     03  FILLER  PIC  X(01)  VALUE  X"29".
     03  FILLER  PIC  X(01)  VALUE  ",".
     03  FILLER  PIC  X(01)  VALUE  X"28".
     03  FILLER  PIC  N(07)  VALUE  NC"明細ＪＡＮＣＤ".
     03  FILLER  PIC  X(01)  VALUE  X"29".
     03  FILLER  PIC  X(01)  VALUE  ",".
     03  FILLER  PIC  X(01)  VALUE  X"28".
     03  FILLER  PIC  N(09)  VALUE  NC"明細サカタ商品情報".
     03  FILLER  PIC  X(01)  VALUE  X"29".
     03  FILLER  PIC  X(01)  VALUE  ",".
     03  FILLER  PIC  X(01)  VALUE  X"28".
     03  FILLER  PIC  N(07)  VALUE  NC"明細商品名カナ".
     03  FILLER  PIC  X(01)  VALUE  X"29".
     03  FILLER  PIC  X(01)  VALUE  ",".
     03  FILLER  PIC  X(01)  VALUE  X"28".
     03  FILLER  PIC  N(06)  VALUE  NC"明細事故数量".
     03  FILLER  PIC  X(01)  VALUE  X"29".
     03  FILLER  PIC  X(01)  VALUE  ",".
     03  FILLER  PIC  X(01)  VALUE  X"28".
     03  FILLER  PIC  N(06)  VALUE  NC"明細原価単価".
     03  FILLER  PIC  X(01)  VALUE  X"29".
     03  FILLER  PIC  X(01)  VALUE  ",".
     03  FILLER  PIC  X(01)  VALUE  X"28".
     03  FILLER  PIC  N(08)  VALUE  NC"明細原価金額合計".
     03  FILLER  PIC  X(01)  VALUE  X"29".
     03  FILLER  PIC  X(01)  VALUE  ",".
     03  FILLER  PIC  X(01)  VALUE  X"28".
     03  FILLER  PIC  N(06)  VALUE  NC"明細伝票内訳".
     03  FILLER  PIC  X(01)  VALUE  X"29".
     03  FILLER  PIC  X(01)  VALUE  ",".
     03  FILLER  PIC  X(01)  VALUE  X"28".
     03  FILLER  PIC  N(07)  VALUE  NC"明細伝票内訳名".
     03  FILLER  PIC  X(01)  VALUE  X"29".
*****03  FILLER  PIC  X(01)  VALUE  ",".

 01  FILE-ERR.
     03  ECJ-ERR           PIC  N(20)  VALUE
         NC"エンチョー受領情報データ".
     03  TEN-ERR           PIC  N(20)  VALUE
         NC"店舗マスタエラー".
     03  SHT-ERR           PIC  N(20)  VALUE
         NC"商品変換ＴＢＬエラー".
     03  MEI-ERR           PIC  N(20)  VALUE
         NC"商品名称マスタエラー".
     03  ENC-ERR           PIC  N(20)  VALUE
         NC"エンチョー受領ＣＳＶエラー".
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
 ENC-ERR                   SECTION.
     USE AFTER  EXCEPTION PROCEDURE  ENCJYRCV.
     DISPLAY  ENC-ERR   UPON CONS.
     DISPLAY  SEC-NAME  UPON CONS.
     DISPLAY  ENC-STS   UPON CONS.
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
     MOVE  "INIT-SEC"       TO  S-NAME.

     DISPLAY  "**  START SSY2712V  **"  UPON CONS.

     PERFORM  SDATE-GET-SEC.
*ファイルのＯＰＥＮ
     OPEN  INPUT  ECJYURF.
     OPEN  INPUT  HTENMS.
     OPEN  INPUT  HSHOTBL.
     OPEN  INPUT  HMEIMS.
     OPEN  OUTPUT ENCJYRCV.
*ワークの初期化
     INITIALIZE  WRK-AREA.
     INITIALIZE  FLG-AREA.

     MOVE  SPACE            TO  FG-ECJYURF-END.
     PERFORM  RD-INF-SEC.
     IF  END-FLG = "END"
         MOVE  "END"        TO  END-FLG
         DISPLAY  NC"＃対象データがありません！＃" UPON CONS
     ELSE
         MOVE SYS-DATE(1:4) TO  HD-YMD(1:4)
         MOVE "/"           TO  HD-YMD(5:1)
         MOVE SYS-DATE(5:2) TO  HD-YMD(6:2)
         MOVE "/"           TO  HD-YMD(8:1)
         MOVE SYS-DATE(7:2) TO  HD-YMD(9:2)
         MOVE  WK-TAITL1    TO  ENC-REC
         WRITE ENC-REC
         ADD   1            TO   CT-OUT
         MOVE  WK-TAITL2    TO   ENC-REC
         WRITE ENC-REC
         ADD   1            TO   CT-OUT
     END-IF.
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
*    入力ファイル読込み　                                      *
****************************************************************
 RD-INF-SEC        SECTION.
     MOVE  "RD-INF-SEC" TO  S-NAME.

     READ  ECJYURF
       AT  END
         MOVE  "END"        TO  END-FLG
     END-READ.

     ADD 1   TO  CT-IN.

 RD-INF-EXIT.
     EXIT.

****************************************************************
*             メイン処理                             2.0       *
****************************************************************
 MAIN-SEC              SECTION.
     MOVE  "MAIN-SEC"       TO  S-NAME.
*明細項目セット
*カンマ、制御バイトセット
     MOVE   SPACE           TO  ENC-REC  ECV-REC.
     INITIALIZE                 ENC-REC  ECV-REC.
     MOVE  ","   TO  ECV-K01 ECV-K02 ECV-K04 ECV-K04 ECV-K05
                     ECV-K06 ECV-K07 ECV-K08 ECV-K09 ECV-K10
                     ECV-K11 ECV-K12 ECV-K13 ECV-K14 ECV-K15
                     ECV-K16 ECV-K17 ECV-K18 ECV-K19 ECV-K20
                     ECV-K21 ECV-K22 ECV-K03.
     MOVE X"28"  TO  ECV-S01 ECV-S03 ECV-S05 ECV-S07 ECV-S09.
     MOVE X"29"  TO  ECV-S02 ECV-S04 ECV-S06 ECV-S08 ECV-S10.
*取引先ＣＤ
     MOVE ECJ-F014           TO   ECV-F01.
*店舗ＣＤ
     MOVE ECJ-F024           TO   ECV-F02.
*店舗名称
     MOVE ECJ-F014           TO   TEN-F52.  *> 相手取引先ＣＤ
     MOVE ECJ-F024           TO   TEN-F011. *> 店舗コード
     PERFORM  RD-HTENMS-SEC.
     IF  FG-HTENMS-INV = ZERO
         MOVE  TEN-F02       TO   ECV-F03
     ELSE
         MOVE  ALL NC"＊"    TO   ECV-F03
     END-IF.
*伝票番号
     MOVE ECJ-F026           TO   ECV-F04.
*納品日
     MOVE ECJ-F027(1:4)      TO   ECV-F05(1:4).
     MOVE "/"                TO   ECV-F05(5:1).
     MOVE ECJ-F027(5:2)      TO   ECV-F05(6:2).
     MOVE "/"                TO   ECV-F05(8:1).
     MOVE ECJ-F027(7:2)      TO   ECV-F05(9:2).
*計上日
     MOVE ECJ-F028(1:4)      TO   ECV-F06(1:4).
     MOVE "/"                TO   ECV-F06(5:1).
     MOVE ECJ-F028(5:2)      TO   ECV-F06(6:2).
     MOVE "/"                TO   ECV-F06(8:1).
     MOVE ECJ-F028(7:2)      TO   ECV-F06(9:2).
*支払対象日
     MOVE ECJ-F029(1:4)      TO   ECV-F07(1:4).
     MOVE "/"                TO   ECV-F07(5:1).
     MOVE ECJ-F029(5:2)      TO   ECV-F07(6:2).
     MOVE "/"                TO   ECV-F07(8:1).
     MOVE ECJ-F029(7:2)      TO   ECV-F07(9:2).
*課税区分
     MOVE ECJ-F02C           TO   ECV-F08.
*課税区分名
     IF  ECJ-F02C  =  9
         MOVE NC"非課税"     TO   ECV-F09
     ELSE
         MOVE NC"課税"       TO   ECV-F09
     END-IF.
*伝票内訳
     MOVE  ECJ-F02B          TO   ECV-F10.
     EVALUATE   ECJ-F02B
          WHEN  "10"   MOVE NC"ＥＯＳ"         TO  ECV-F11
          WHEN  "11"   MOVE NC"手書受領"       TO  ECV-F11
          WHEN  "12"   MOVE NC"ＥＯＳ受領訂正" TO  ECV-F11
          WHEN  "13"   MOVE NC"手書受領訂正"   TO  ECV-F11
          WHEN  OTHER  MOVE ALL NC"＊"         TO  ECV-F11
     END-EVALUATE.
*税率
**   MOVE  ECJ-F97           TO   ECV-F12.
*原価金額合計
     IF    ECJ-F02A  <  ZERO
           MOVE  "-"         TO   ECV-F131
     ELSE
           MOVE  "0"         TO   ECV-F131
     END-IF.
     MOVE  ECJ-F02A          TO   ECV-F132.
*納品事故情報
     IF  ECJ-F039  >  ZERO
         MOVE NC"＜納品事故情報＞" TO ECV-F14
     ELSE
         MOVE SPACE                TO ECV-F14
         GO                        TO MAIN010
     END-IF.
*明細行
     MOVE ECJ-F039           TO   ECV-F15.
*明細ＪＡＮＣＤ
     MOVE ECJ-F036           TO   ECV-F16.
*明細サカタ商品情報
     MOVE ECJ-F014           TO   SHT-F01.  *> 相手取引先ＣＤ
     MOVE ECJ-F036           TO   SHT-F02.  *> 相手商品ＣＤ
     PERFORM  RD-HSHOTBL-SEC.
     IF  FG-HSHOTBL-INV = ZERO
         MOVE  SHT-F031     TO  ECV-F17(1:8)
         MOVE  SHT-F0321    TO  ECV-F17(9:5)
         MOVE  SHT-F0322    TO  ECV-F17(14:2)
         MOVE  SHT-F0323    TO  ECV-F17(16:1)
         MOVE  SHT-F031     TO  MEI-F011  *> 商品コード
         MOVE  SHT-F0321    TO  MEI-F0121 *> 単１
         MOVE  SHT-F0322    TO  MEI-F0122 *> 単２
         MOVE  SHT-F0323    TO  MEI-F0123 *> 単３
         PERFORM  RD-HMEIMS-SEC
         IF  FG-HMEIMS-INV = ZERO
              MOVE  MEI-F03       TO   ECV-F18
         ELSE
              MOVE  ALL "*"       TO   ECV-F18
         END-IF
     ELSE
         MOVE  ALL "*"            TO   ECV-F17
         MOVE  ALL "*"            TO   ECV-F18
     END-IF.
*数量
     IF    ECJ-F03B  <  ZERO
           MOVE  "-"         TO   ECV-F191 ECV-F211
     ELSE
           MOVE  "0"         TO   ECV-F191 ECV-F211
     END-IF.
     MOVE  ECJ-F03B          TO   ECV-F192.
*原価単価
     MOVE  ECJ-F03C          TO   ECV-F20.
*原価金額合計
     IF    ECJ-F03B  <  ZERO
           COMPUTE ECV-F212  =  ECV-F192  *  ECV-F20 *  -1
     ELSE
           COMPUTE ECV-F212  =  ECV-F192  *  ECV-F20 *  -1
     END-IF.
*伝票内訳
     MOVE  ECJ-F03D          TO   ECV-F22.
*伝票内訳名
     EVALUATE   ECJ-F03D
          WHEN  "12"   MOVE NC"受領訂正"       TO  ECV-F23
          WHEN  "13"   MOVE NC"手書受領訂正"   TO  ECV-F23
          WHEN  "30"   MOVE NC"納品事故"       TO  ECV-F23
          WHEN  OTHER  MOVE ALL  NC"＊"        TO  ECV-F23
     END-EVALUATE.
 MAIN010.
*明細出力
     MOVE  ECV-REC                             TO  ENC-REC.
     WRITE ENC-REC.
     ADD   1                                   TO   CT-OUT.
*読込
     PERFORM  RD-INF-SEC.

 MAIN-EXIT.
     EXIT.
****************************************************************
*    店舗マスタ検索                                            *
****************************************************************
 RD-HTENMS-SEC          SECTION.
     MOVE  "RD-HTENMS-SEC"  TO  S-NAME.

     READ  HTENMS
       INVALID
         MOVE  1            TO  FG-HTENMS-INV
       NOT INVALID
         MOVE  ZERO         TO  FG-HTENMS-INV
     END-READ.

 PRT-EXIT.
     EXIT.

****************************************************************
*    商品変換ＴＢＬ検索                                        *
****************************************************************
 RD-HSHOTBL-SEC          SECTION.
     MOVE  "RD-HSHOTBL-SEC"  TO  S-NAME.

     READ  HSHOTBL
       INVALID
         MOVE  1            TO  FG-HSHOTBL-INV
       NOT INVALID
         MOVE  ZERO         TO  FG-HSHOTBL-INV
     END-READ.

 RD-HSHOTBL-EXIT.
     EXIT.
****************************************************************
*    商品名称マスタ検索                                        *
****************************************************************
 RD-HMEIMS-SEC          SECTION.
     MOVE  "RD-HMEIMS-SEC"  TO  S-NAME.

     READ  HMEIMS
       INVALID
         MOVE  1            TO  FG-HMEIMS-INV
       NOT INVALID
         MOVE  ZERO         TO  FG-HMEIMS-INV
     END-READ.

 RD-HMEIMS-EXIT.
     EXIT.
****************************************************************
*             終了処理                               3.0       *
****************************************************************
 END-SEC               SECTION.
     MOVE  "END-SEC"        TO  S-NAME.

*ファイル ＣＬＯＳＥ
     CLOSE  ECJYURF.
     CLOSE  HTENMS.
     CLOSE  HSHOTBL.
     CLOSE  HMEIMS.
     CLOSE  ENCJYRCV.

     DISPLAY  "SSY2712V ECJYURF IN =" CT-IN  UPON CONS.
     DISPLAY  "SSY2712V CSVWK  OUT =" CT-OUT UPON CONS.
     DISPLAY  "**  END   SSY2712V  **"       UPON CONS.

 END-EXIT.
     EXIT.
*****************<<  SSY2712V   END PROGRAM  >>******************
