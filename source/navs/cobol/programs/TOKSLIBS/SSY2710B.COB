****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　
*    業務名　　　　　　　：　ＨＧ基幹システム　　　　　　　　　
*    モジュール名　　　　：　エンチョー受領情報変換
*    作成日／更新日　　　：　2012/02/13
*    作成者／更新者　　　：　NAV
*    処理概要　　　　　　：　
*      受信したエンチョー受領データ（受領情報）をサカタ
*      フォーマットに変換する。
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SSY2710B.
 AUTHOR.                NAV.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU-GP6000.
 OBJECT-COMPUTER.       FUJITSU-GP6000.
 SPECIAL-NAMES.
         STATION   IS   STAT
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*---<<  受信データ（受領情報）  >>---*
     SELECT   ONLENCJY
         ASSIGN        TO  ONLENCJY
         ORGANIZATION  IS  SEQUENTIAL
         ACCESS MODE   IS  SEQUENTIAL
         FILE  STATUS  IS  OEC-STS.

*---<<  エンチョー受領情報データ  >>---*
     SELECT  ECJYURF
         ASSIGN        TO  ECJYURL1
         ORGANIZATION  IS  INDEXED
         ACCESS MODE   IS  RANDOM
         RECORD KEY    IS  ECJ-F024 *> 受入店ＣＤ
                           ECJ-F026 *> 仕入伝票Ｎｏ
                           ECJ-F039 *> 仕入伝票行Ｎｏ
         FILE STATUS   IS  ECJ-STS.

 DATA                   DIVISION.
 FILE                   SECTION.
*---<<  受信データ（受領情報）  >>---*
 FD  ONLENCJY
     BLOCK CONTAINS  1 RECORDS
     LABEL RECORD   IS STANDARD.
     COPY  ONLENCJH OF XFDLIB   JOINING  OEC-H  PREFIX.
     COPY  ONLENCJD OF XFDLIB   JOINING  OEC-D  PREFIX.
     COPY  ONLENCJK OF XFDLIB   JOINING  OEC-K  PREFIX.

*---<<  エンチョー受領情報データ  >>---*
 FD  ECJYURF.
     COPY  ECJYURF OF XFDLIB   JOINING  ECJ  PREFIX.

****  作業領域  ***
 WORKING-STORAGE             SECTION.
****  ステイタス情報  ***
 01  STATUS-AREA.
     03  OEC-STS            PIC  X(02).
     03  ECJ-STS            PIC  X(02).
****  フラグ  ***
 01  PSW-AREA.
     03  END-FLG            PIC  X(03)  VALUE SPACE.
     03  FG-ONLENCJY-END    PIC  X(03)  VALUE SPACE.
     03  FG-ECJYURF-INV     PIC  9(01).

 01  CNT-AREA.
     03  CT-IN              PIC  9(07)  VALUE ZERO.
     03  CT-DEN-MAISU       PIC  9(06)  VALUE ZERO.
     03  CT-JIKO-GYOSU      PIC  9(06)  VALUE ZERO.

****  ＷＲＫ領域  ***
 01  WRK-AREA.
     03  WRK-DATE1          PIC  9(06).
     03  WRK-DATE1R  REDEFINES WRK-DATE1.
         05  WRK-DATE1R1    PIC  9(04).
         05  WRK-DATE1R2    PIC  9(02).
     03  WRK-DATE2          PIC  9(06).

** 受領情報（エンジョー->ベンダー)
**   取引先ヘッダーレコード
     COPY  ONLENCJH OF XFDLIB   JOINING  OEC-HW  PREFIX.

**** メッセージ情報  ***
 01  MSG-AREA1-1.
     03  MSG-ABEND1.
       05  FILLER           PIC  X(04)  VALUE  "### ".
       05  ERR-PG-ID        PIC  X(08)  VALUE  "SSY2710B".
       05  FILLER           PIC  X(10)  VALUE  " ABEND ###".
     03  MSG-ABEND2.
       05  FILLER           PIC  X(04)  VALUE  "### ".
       05  ERR-FL-ID        PIC  X(08).
       05  FILLER           PIC  X(04)  VALUE  " ST-".
       05  ERR-STCD         PIC  X(02).
       05  FILLER           PIC  X(04)  VALUE  " ###".
* 日付
 01  DATE-AREA.
     03  WK-YS              PIC  9(02)  VALUE ZERO.
     03  WK-DATE.
         05  WK-Y           PIC  9(02)  VALUE ZERO.
         05  WK-M           PIC  9(02)  VALUE ZERO.
         05  WK-D           PIC  9(02)  VALUE ZERO.
 01  DATE-AREAR1  REDEFINES DATE-AREA.
     03  SYS-YM             PIC  9(06).
     03  FILLER             PIC  9(02).
 01  DATE-AREAR2  REDEFINES DATE-AREA.
     03  SYS-DATE           PIC  9(08).

* 時刻
 01  TIME-AREA.
     03  WK-TIME            PIC  9(08)  VALUE ZERO.
 01  TIME-AREAR  REDEFINES TIME-AREA.
     03  WK-TIME-HMS        PIC  9(06).
     03  WK-TIME-MS         PIC  9(06).

*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN            PIC  X(01).
 01  LINK-IN-YMD6           PIC  9(06).
 01  LINK-IN-YMD8           PIC  9(08).
 01  LINK-OUT-RET           PIC  X(01).
 01  LINK-OUT-YMD           PIC  9(08).

 01  MSG-AREA.
     03  MSG-010.
       05  MSG-010-1.
           07  FILLER  PIC  X(34) VALUE
           "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
       05  MSG-010-2.
           07  FILLER  PIC  X(34) VALUE
           "＊　受信データは異常です！　　＊".
       05  MSG-010-3.
           07  FILLER  PIC  X(34) VALUE
           "＊　納品事故データに紐付く　　＊".
       05  MSG-010-4.
           07  FILLER  PIC  X(34) VALUE
           "＊　受領伝票が存在しません。　＊".
       05  MSG-010-5.
           07  FILLER  PIC  X(20) VALUE
           "＊　　　店コード＝".
           07  MSG-010-5-TENCD  PIC  9(05).
           07  FILLER  PIC  X(01) VALUE SPACE.
           07  FILLER  PIC  X(10) VALUE
           "　　　＊".
       05  MSG-010-6.
           07  FILLER  PIC  X(20) VALUE
           "＊　　　伝票番号＝".
           07  MSG-010-6-DENNO  PIC  9(10).
           07  FILLER  PIC  X(06) VALUE
           "　＊".
       05  MSG-010-7.
           07  FILLER  PIC  X(20) VALUE
           "＊　　　行番号　＝".
           07  MSG-010-7-GYO    PIC  9(03).
           07  FILLER  PIC  X(01) VALUE SPACE.
           07  FILLER  PIC  X(12) VALUE
           "　　　　＊".
     03  MSG-020.
       05  MSG-020-1.
           07  FILLER  PIC  X(32) VALUE
           "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
       05  MSG-020-2.
           07  FILLER  PIC  X(22) VALUE
           "＊　受領情報READ＝".
           07  MSG-020-2-IN           PIC  9(06).
           07  FILLER  PIC  X(08) VALUE
           "件　＊".
       05  MSG-020-3.
           07  FILLER  PIC  X(20) VALUE
           "＊　受領伝票枚数＝".
           07  MSG-020-3-DEN-MAISU    PIC  9(06).
           07  FILLER  PIC  X(08) VALUE
           "件　＊".
       05  MSG-020-4.
           07  FILLER  PIC  X(20) VALUE
           "＊　事故明細行数＝".
           07  MSG-020-4-JIKO-GYOSU   PIC  9(06).
           07  FILLER  PIC  X(08) VALUE
           "件　＊".


*-------------------------------------------------------------*
*             ＭＡＩＮ　　　　ＭＯＤＵＬＥ                    *
*-------------------------------------------------------------*
 PROCEDURE                   DIVISION.

 DECLARATIVES.
 FILEERR-SEC1                SECTION.
     USE AFTER  EXCEPTION
                PROCEDURE  ONLENCJY.
     MOVE  "ONLENCJY"     TO  ERR-FL-ID.
     MOVE  OEC-STS        TO  ERR-STCD.
     DISPLAY  MSG-ABEND1  UPON CONS.
     DISPLAY  MSG-ABEND2  UPON CONS.
     MOVE  4000           TO  PROGRAM-STATUS.
     STOP RUN.
 FILEERR-SEC2                SECTION.
     USE AFTER  EXCEPTION
                PROCEDURE  ECJYURF.
     MOVE  "ECJYURF"      TO  ERR-FL-ID.
     MOVE  ECJ-STS        TO  ERR-STCD.
     DISPLAY  MSG-ABEND1  UPON CONS.
     DISPLAY  MSG-ABEND2  UPON CONS.
     MOVE  4000           TO  PROGRAM-STATUS.
     STOP RUN.
 END     DECLARATIVES.
****************************************************************
 KEI0100-START               SECTION.
     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC
              UNTIL  END-FLG = "END".
     PERFORM  END-SEC.
     STOP  RUN.
 KEI0100-EXIT.
     EXIT.
****************************************************************
*    1.0  初期処理                                             *
****************************************************************
 INIT-SEC                    SECTION.
     DISPLAY   "**  START SSY2710B  **"  UPON CONS.
*ファイルのＯＰＥＮ
     OPEN  INPUT ONLENCJY.
     OPEN  I-O   ECJYURF.

*システム日付・時刻の取得
     ACCEPT  WK-DATE  FROM DATE.
     MOVE  "3"              TO  LINK-IN-KBN.
     MOVE  WK-DATE          TO  LINK-IN-YMD6.
     MOVE  ZERO             TO  LINK-IN-YMD8.
     MOVE  ZERO             TO  LINK-OUT-RET.
     MOVE  ZERO             TO  LINK-OUT-YMD.
     CALL  "SKYDTCKB"  USING  LINK-IN-KBN
                              LINK-IN-YMD6
                              LINK-IN-YMD8
                              LINK-OUT-RET
                              LINK-OUT-YMD.
     MOVE  LINK-OUT-YMD     TO  DATE-AREA.

*システム日付取得
     ACCEPT  WK-TIME   FROM  TIME.

     PERFORM  INF-READ-SEC.
     IF  FG-ONLENCJY-END = "END"
         MOVE  "END"        TO  END-FLG
         GO TO  INIT-EXIT
     END-IF.

     IF  OEC-H-F02 NOT = "A"
         DISPLAY "SSY2710B HERDER RECORD NOT FOUND ???"
                 UPON CONS
         MOVE  4001         TO  PROGRAM-STATUS
         EXIT PROGRAM
     END-IF.

 INIT-EXIT.
     EXIT.
****************************************************************
*    入力ファイル読込み                                        *
****************************************************************
 INF-READ-SEC               SECTION.

     READ  ONLENCJY
       AT END
         MOVE  "END"        TO  FG-ONLENCJY-END
         GO TO  INF-READ-EXIT
     END-READ.

     ADD 1  TO CT-IN.

 INF-READ-EXIT.
     EXIT.
****************************************************************
*    2.0  メイン処理                                           *
****************************************************************
 MAIN-SEC                    SECTION.

     MOVE  OEC-H-REC        TO  OEC-HW-REC.
     PERFORM  INF-READ-SEC.

     PERFORM  UNTIL FG-ONLENCJY-END = "END"
                 OR OEC-H-F02       = "A"
                                   *> 取引先ヘッダブレイク
       PERFORM  EDWT-SEC
       PERFORM  INF-READ-SEC

     END-PERFORM.

     IF  FG-ONLENCJY-END = "END"
         MOVE  "END"        TO  END-FLG
     END-IF.

 MAIN-EXIT.
     EXIT.
****************************************************************
*    編集／出力                                                *
****************************************************************
 EDWT-SEC                SECTION.
     IF  OEC-H-F02 = "D"
         PERFORM  EDWT-DT-SEC
     ELSE
         PERFORM  EDWT-JK-SEC
     END-IF.

 EDWT-EXIT.
     EXIT.
****************************************************************
*    データ部編集／出力                                        *
****************************************************************
 EDWT-DT-SEC                SECTION.
      MOVE  SPACE           TO  ECJ-REC.
      INITIALIZE  ECJ-REC.
      MOVE  OEC-HW-F01      TO  ECJ-F011. *> データ区分
      MOVE  OEC-HW-F02      TO  ECJ-F012. *> レコード区分
      MOVE  OEC-HW-F03      TO  ECJ-F013. *> ﾚｺｰﾄﾞｼｰｹﾝｽNO
      MOVE  OEC-HW-F04      TO  ECJ-F014. *> 取引先ＣＤ
      MOVE  OEC-HW-F05      TO  ECJ-F015. *> データ送信元
      MOVE  OEC-HW-F06      TO  ECJ-F016. *> データ作成日
      MOVE  OEC-HW-F07      TO  ECJ-F017. *> データ作成時刻
      MOVE  OEC-HW-F08      TO  ECJ-F018. *> 送信レコード件数
      MOVE  OEC-HW-F09      TO  ECJ-F019. *> 取引先名称
      MOVE  OEC-D-F01       TO  ECJ-F021. *> データ区分
      MOVE  OEC-D-F02       TO  ECJ-F022. *> レコード区分
      MOVE  OEC-D-F03       TO  ECJ-F023. *> ﾚｺｰﾄﾞｼｰｹﾝｽNO
      MOVE  OEC-D-F04       TO  ECJ-F024. *> 受入店ＣＤ
      MOVE  OEC-D-F05       TO  ECJ-F025. *> 受入課ＣＤ
      MOVE  OEC-D-F06       TO  ECJ-F026. *> 仕入伝票Ｎｏ
      MOVE  OEC-D-F07       TO  ECJ-F027. *> 納入日
      MOVE  OEC-D-F08       TO  ECJ-F028. *> 処理日
      MOVE  OEC-D-F09       TO  ECJ-F029. *> 支払対象締日
      IF  OEC-D-F10 = "-"
          COMPUTE ECJ-F02A  =  OEC-D-F11 * -1  *> 原価合計額
      ELSE
          MOVE  OEC-D-F11   TO  ECJ-F02A *> 原価合計額
      END-IF.

      MOVE  OEC-D-F12       TO  ECJ-F02B. *> 伝票内訳
      MOVE  OEC-D-F13       TO  ECJ-F02C. *> 課税区分
      MOVE  SYS-DATE        TO  ECJ-F98.  *> 作成日付
      MOVE  WK-TIME-HMS     TO  ECJ-F99.  *> 作成時刻

      WRITE  ECJ-REC.

      ADD  1   TO  CT-DEN-MAISU.

 EDWT-DT-EXIT.
     EXIT.
****************************************************************
*    納品事故部編集／出力                                      *
****************************************************************
 EDWT-JK-SEC                SECTION.

     MOVE  OEC-K-F04        TO  ECJ-F024 *> 受入店ＣＤ
     MOVE  OEC-K-F08        TO  ECJ-F026 *> 仕入伝票Ｎｏ
     MOVE  ZERO             TO  ECJ-F039 *> 仕入伝票行Ｎｏ

     READ  ECJYURF
       INVALID
         MOVE  1            TO  FG-ECJYURF-INV
       NOT INVALID
         MOVE  ZERO         TO  FG-ECJYURF-INV
     END-READ.

     IF  FG-ECJYURF-INV = 1  *> 受領レコード無し
         MOVE  OEC-K-F04    TO  MSG-010-5-TENCD
         MOVE  OEC-K-F08    TO  MSG-010-6-DENNO
         MOVE  OEC-K-F09    TO  MSG-010-7-GYO

         DISPLAY  MSG-010-1  UPON CONS
         DISPLAY  MSG-010-2  UPON CONS
         DISPLAY  MSG-010-3  UPON CONS
         DISPLAY  MSG-010-4  UPON CONS
         DISPLAY  MSG-010-5  UPON CONS
         DISPLAY  MSG-010-6  UPON CONS
         DISPLAY  MSG-010-7  UPON CONS
         DISPLAY  MSG-010-1  UPON CONS
         MOVE  4001         TO  PROGRAM-STATUS
         EXIT PROGRAM
     END-IF.

     MOVE  OEC-K-F01       TO  ECJ-F031. *> データ区分
     MOVE  OEC-K-F02       TO  ECJ-F032. *> レコード区分
     MOVE  OEC-K-F03       TO  ECJ-F033. *> ﾚｺｰﾄﾞｼｰｹﾝｽNO
     MOVE  OEC-K-F04       TO  ECJ-F034. *> 受入店ＣＤ
     MOVE  OEC-K-F05       TO  ECJ-F035. *> 受入課ＣＤ
     MOVE  OEC-K-F06       TO  ECJ-F036. *> 仕入ＪＡＮＣＤ
     MOVE  OEC-K-F07       TO  ECJ-F037. *> 仕入ＥＯＳＣＤ
     MOVE  OEC-K-F08       TO  ECJ-F038. *> 仕入伝票Ｎｏ
     MOVE  OEC-K-F09       TO  ECJ-F039. *> 仕入伝票行Ｎｏ
     MOVE  OEC-K-F10       TO  ECJ-F03A. *> 処理日

     IF  OEC-K-F11 = "-"
         COMPUTE  ECJ-F03B = OEC-K-F12 * -1 *> 納品事故数量
     ELSE
         MOVE  OEC-K-F12   TO  ECJ-F03B *> 納品事故数量
     END-IF.

     IF  OEC-K-F13 = "-"
         *> 入力定義 9(09) -> データ内容 9(07)V9(02)

         COMPUTE  ECJ-F03C =
            ( OEC-K-F14 / 100 ) * -1 *> 原単価
     ELSE
         COMPUTE  ECJ-F03C = OEC-K-F14 / 100  *> 原単価
     END-IF.

     MOVE  OEC-K-F15       TO  ECJ-F03D. *> 伝票内訳

     WRITE  ECJ-REC.

     ADD  1   TO  CT-JIKO-GYOSU.

 EDWT-JK-EXIT.
     EXIT.
****************************************************************
*    3.0  終了処理                                             *
****************************************************************
 END-SEC                SECTION.
     CLOSE  ONLENCJY.
     CLOSE  ECJYURF.

     MOVE  CT-IN            TO  MSG-020-2-IN.
     MOVE  CT-DEN-MAISU     TO  MSG-020-3-DEN-MAISU.
     MOVE  CT-JIKO-GYOSU    TO  MSG-020-4-JIKO-GYOSU.
     DISPLAY  MSG-020-1  UPON CONS.
     DISPLAY  MSG-020-2  UPON CONS.
     DISPLAY  MSG-020-3  UPON CONS.
     DISPLAY  MSG-020-4  UPON CONS.
     DISPLAY  MSG-020-1  UPON CONS.
     DISPLAY   "**  END   SSY2710B  **"  UPON CONS.

 END-EXIT.
     EXIT.
******************<<  PROGRAM  END  >>**************************
